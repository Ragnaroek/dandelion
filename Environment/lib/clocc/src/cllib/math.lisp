;;; Math utilities (Arithmetical / Statistical functions)
;;;
;;; Copyright (C) 1997-2006 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: math.lisp,v 2.84 2006/09/20 01:04:42 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/math.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `mesg', `get-int-time', `elapsed', `with-timing'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `read-from-file', `write-to-file'
  (require :cllib-fileio (translate-logical-pathname "cllib:fileio"))
  ;; `with-collect', `linear'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `call-on-split'
  (require :cllib-list (translate-logical-pathname "cllib:list")))

(in-package :cllib)

(export
 '(mulf divf sqr triangle
   ! !! stirling fibonacci ruler primes-to divisors primep
   product-from-to binomial binomial-mod2 *primes* *primes-file*
   make-primes-list number-sum-split all-num-split
   vector-shuffle permutation with-permutations-shuffle
   with-permutations-swap with-permutations-lex permutations-list subsets
   draw pick sample eval-cont-fract fract-approx
   *num-tolerance* *relative-tolerance* *absolute-tolerance*
   dot poly1 poly norm-functions norm normalize rel-dist
   erf erfc erfcx cndf cndf-tail
   log-gamma beta incomplete-gamma *max-iterations*
   mean mean-cx mean-weighted mean-geometric mean-geometric-weighted mean-some
   standard-deviation standard-deviation-cx standard-deviation-weighted
   standard-deviation-relative standard-deviation-mdl min+max
   entropy-sequence entropy-distribution kullback-leibler
   information mutual-information dependency proficiency correlation
   mdl make-mdl +bad-mdl+ mdl-mn mdl-sd mdl-le mdl-mi mdl-ma mdl-mi$ mdl-ma$
   mdl-normalize mdl-denormalize mdl-normalize-function
   normalizer-table normalize-function-list
   kurtosis-skewness kurtosis-skewness-weighted
   covariance covariance1 cov volatility
   safe-fun safe-fun1 safe-/ s/ d/
   convex-hull1 convex-hull sharpe-ratio to-percent percent-change
   rel-diff approx=-abs approx=-rel approx=
   binary-search newton integrate-simpson add-probabilities
   line make-line line-val line-rsl line-below-p line-above-p intersect
   with-line line-adjust line-adjust-dir line-adjust-list
   line-thru-points regress lincom
   plf make-plf plf-x plf-y plf-extend-left plf-extend-right plf-size
   monotonic-p plf-monotonic-p plf-val plf->function plf-simplify plf-integral
   increasify *increasify-step*))

;;;
;;;
;;;

(define-modify-macro mulf (mult) * "Multiply the arg by a number.")
(define-modify-macro divf (mult) / "Divide the arg by a number.")
(defmacro sqr (xx)
  "Compute the square of a number, taking care to eval only once."
  (if (atom xx) `(* ,xx ,xx)
      (with-gensyms ("SQR-" var) `(let ((,var ,xx)) (* ,var ,var)))))
(declaim (inline triangle))
(defun triangle (i)
  "Compute the triangular number"
  (/ (* i (1- i)) 2))

;;;
;;; Integers
;;;

(declaim (ftype (function (integer integer) (values integer))
                product-from-to binomial))
(defun product-from-to (aa bb)
  "Compute the product of integers from AA (EXclusive) to BB (INclusive)."
  (declare (integer aa bb))
  (when (> aa bb)
    (error "~s (~:d ~:d): the first argument must be smaller"
           'product-from-to aa bb))
  (when (minusp (* bb aa))
    (return-from product-from-to 0))
  ;; this algorithm insures that we multiply bignums
  ;; of approximately the same size
  ;; we use `labels' since some compilers optimize `labels' better than
  ;; plain recursion and because this avoids doing the above checks in cycle
  (labels ((pft (aa bb)
             (case (- bb aa)
               (0 1) (1 bb) (2 (* (- bb 1) bb)) (3 (* (- bb 2) (- bb 1) bb))
               (4 (* (- bb 3) (- bb 2) (- bb 1) bb))
               (t (let ((mm (ash (+ aa bb) -1)))
                    (* (pft aa mm) (pft mm bb)))))))
    (pft aa bb)))

(defun binomial (nn kk)
  "Compute the binomial coefficient for two integers."
  (declare (integer nn kk))
  ;; we do not use the double recursion a la `product-from-to'
  ;; because it would take us outside the realm of the integers
  (loop :with res = 1
    :for ii :from 1 :to (max kk (- nn kk))
    :for jj :downfrom nn
    :do (mulf res (/ jj ii))
    :finally (return res)))

(declaim (ftype (function (integer integer) (values bit)) binomial-mod2))
(defun binomial-mod2 (nn kk)
  "Compute the binomial coefficient (mod 2) for two integers.
This is equivalent to (mod (binomial nn kk) 2) but faster."
  (declare (integer nn kk))
  ;; Lucas's Theorem => even iff all non-0 digits of K are also non-0 in N
  ;; same as (= nn (logior nn kk)), but since K is smaller, this is faster:
  (if (= kk (logand nn kk)) 1 0))

(declaim (ftype (function (integer) (values integer)) ! !!))
(defun ! (nn)
  "Compute the factorial: n! = n * (n-1) * (n-2) * ..."
  (declare (integer nn))
  #+clisp (#+lisp=cl ext:! #-lisp=cl lisp:! nn) ; CLISP has built-in factorial
  #-clisp (product-from-to 1 nn))

(defun !! (nn)
  "Compute the double factorial: n!! = n * (n-2) * (n-4) * ..."
  (declare (integer nn))
  (multiple-value-bind (kk rr) (floor nn 2)
    (declare (fixnum kk) (type (integer 0 1) rr))
    (if (zerop rr) (ash (! kk) kk)
        (labels ((ff (aa bb)
                   (declare (fixnum aa bb))
                   (case (- bb aa)
                     (2 bb) (4 (* bb (- bb 2))) (6 (* bb (- bb 2) (- bb 4)))
                     (8 (* bb (- bb 2) (- bb 4) (- bb 6)))
                     (t (let ((mm (1+ (ash (ash (+ aa bb) -2) 1))))
                          (* (ff aa mm) (ff mm bb)))))))
          (if (> nn 1) (ff 1 nn) 1)))))

(defun stirling (xx)
  "Compute the approximate factorial using the Stirling formula."
  (let ((dd (dfloat xx)))
    (declare (double-float dd))
    (* (sqrt (* 2 pi dd)) (expt (/ dd (exp 1)) dd))))

(declaim (ftype (function ((integer 0)) (values (integer 0) (integer 0)))
                fibonacci))
(defun fibonacci (nn)
  "Return 2 consecutive Fibonacci numbers."
  (declare (type (integer 0) nn))
  (case nn
    (0 (values 0 0)) (1 (values 1 0)) (2 (values 1 1)) (3 (values 2 1))
    (t (multiple-value-bind (mm rr) (floor nn 2)
         (declare (integer mm) (type (integer 0 1) rr))
         (multiple-value-bind (f0 f1) (fibonacci mm)
           (declare (type (integer 0) f0 f1))
           (if (zerop rr)
               (values (* f0 (+ (* f1 2) f0))
                       (+ (* f0 f0) (* f1 f1)))
               (values (+ (* f0 f0) (sqr (+ f0 f1)))
                       (* f0 (+ (* f1 2) f0)))))))))

(defun ruler (n)
  "The exponent of the largest power of 2 which divides the given number.
See <http://mathworld.wolfram.com/RulerFunction.html>.
See also <http://www.podval.org/~sds/notes.html#ruler-function>
for the explanation of the (logand n (- n)) part.
This is ffs-1 (ffs=find first set bit):
CLISP: (ffi:def-call-out ffs (:name \"ffs\") (:arguments (i ffi:int))
         (:return-type ffi:int) (:language :stdc) (:library :default))
See http://www.opengroup.org/onlinepubs/009695399/functions/ffs.html."
  (1- (integer-length (logand n (- n)))))

(defcustom *primes* list nil "The list of primes.
The first element is the upper bound.")
(defcustom *primes-file* pathname (merge-pathnames "primes" *datadir*)
  "The file for keeping the list of primes.")

(defun primes-to (nn &optional int)
  "Return the list of primes up to N, exclusively.
The optional second argument, if non-nil, is a double float
specifying the interval for progress reports."
  (declare (fixnum nn) (type (or null double-float) int))
  (when (and (null *primes*) (probe-file *primes-file*))
    (setq *primes* (read-from-file *primes-file*)))
  (when (and *primes* (>= (car *primes*) nn))
    (return-from primes-to (cdr *primes*)))
  (do* ((ii 3 (+ 2 ii)) (res (if (> nn 2) (list 2))) (end res)
        (rt (isqrt ii) (isqrt ii)) (bt (get-int-time)))
       ((>= ii nn) (setq *primes* (cons nn res)) res)
    (declare (fixnum ii))
    (when (and int (= 1 (mod ii 1000)) (> (elapsed bt t) int))
      (format t "~:d..." ii) (force-output)
      (setq bt (get-int-time)))
    (do ((mm res (cdr mm)))
        ((or (null mm) (> (car mm) rt))
         (setq end (cdr (nconc end (list ii)))))
      (if (zerop (mod ii (car mm))) (return nil)))))

(defun divisors (nn &optional (primes-list (primes-to (1+ (isqrt nn)))))
  "Return the list of prime divisors of the given number.
The optional second argument specifies the list of primes."
  (declare (integer nn) (list primes-list))
  (labels ((ddd (n1 ds)
             (do ((pp primes-list (cdr pp)) (rt (isqrt n1)))
                 ((or (null pp) (> (car pp) rt)) (cons n1 ds))
               (declare (fixnum rt))
               (multiple-value-bind (dd rr) (floor n1 (car pp))
                 (declare (fixnum rr) (integer dd))
                 (when (zerop rr) (return (ddd dd (cons (car pp) ds))))))))
    (nreverse (ddd nn nil))))

(defsubst primep (nn &optional (primes-list (primes-to (1+ (isqrt nn)))))
  "Check whether the number is prime."
  (declare (integer nn) (list primes-list))
  (do ((pp primes-list (cdr pp)) (rt (isqrt nn)))
      ((or (null pp) (> (car pp) rt)) t)
    (declare (fixnum rt))
    (when (zerop (mod nn (car pp))) (return nil))))

(defun make-primes-list (&optional (limit most-positive-fixnum))
  "Initialize `*primes*' and write `*primes-file*'."
  (declare (fixnum limit))
  (with-timing (:done t)
    (format t "Computing primes up to ~:d..." limit) (force-output)
    (primes-to limit 10d0))
  (write-to-file *primes* *primes-file* nil
                 (format nil "~%;; Upper limit: ~:d~%;; ~:d primes~%"
                         limit (length *primes*))))

(defun number-sum-split (num fun fun-1 &optional (out *standard-output*))
  "Print all the splittings of NUM into (+ (FUN M) (FUN K))
FUN-1 is the integer inverse function.
E.g.: (number-sum-split 10 (lambda (x) (* x x)) 'isqrt) => ((1 . 3))"
  (declare (type (integer 0) num) (type (or null stream) out)
           (type (function ((integer 0)) (integer 0)) fun fun-1))
  (let ((lim (1+ (funcall fun-1 (floor num 2)))) res)
    (dotimes (ii lim (nreverse res))
      (let* ((fi (funcall fun ii)) (jj (funcall fun-1 (- num fi)))
             (fj (funcall fun jj)))
        (when (= num (+ fi fj))
          (push (cons ii jj) res)
          (when out
            (format out "~:d = ~:d + ~:d   [~:d/~:d]~%" num fi fj ii jj)))))))

(defun all-num-split (min find fun &optional int)
  "Find first FIND integers with MIN representations as a sum of 2 FUN's."
  (declare (fixnum min find) (type (function ((integer 0)) (integer 0)) fun)
           (type (or null double-float) int))
  (do* ((ht (make-hash-table :test #'eql :size 100)) (cur 0 (1+ cur)) (found 0)
        (fc (funcall fun cur) (funcall fun cur)) res (bt (get-int-time)))
       ((= found find) (nreverse res))
    (when (and int (= 1 (mod cur 1000)) (> (elapsed bt t) int))
      (format t "~:d..." cur) (force-output)
      (setq bt (get-int-time)))
    (dotimes (ii cur)
      (let* ((sum (+ fc (funcall fun ii))) (ha (gethash sum ht))
             (new (cons (cons cur ii) ha)))
        (when (>= (length new) min)
          (incf found)
          (format t "~& * ~:d --~d--> ~s~%" sum (length new) new))
        (setf (gethash sum ht) new)))))

;;;
;;; sequence permutations
;;;

(defun make-vector-indexed (len)
  "Return a simple vector #(0 1 ... (1-len))."
  (let ((vv (make-array len)))
    (dotimes (ii len vv)
      (setf (aref vv ii) ii))))

(defun vector-shuffle (vec)
  "Generate a random permutation of the vector in place.
If the argument is a number, return a new random vector of this length.
Uses the Fisher/Yates algorithm, see
 Knuth, TAOCP vol 2 Algorithm 3.4.2P, p.145
 R.A. Fisher & F. Yates, Statistical Tables, London 1938, Example 12
 R. Durstenfeld, CACM 7 (1964), 420.
This is more or less the same as
  (permutation vec (random (! (length vec))))
except that the factorial is likely to be far too large for `random'."
  (etypecase vec
    (vector (loop :for ii :downfrom (1- (length vec)) :to 1
                  :for jj = (random (1+ ii))
                  :unless (= jj ii)
                  :do (rotatef (aref vec ii) (aref vec jj)))
            vec)
    (number (vector-shuffle (make-vector-indexed vec)))))

(defun permutation (vec nth &optional (len (1- (length vec))) (fact (! len)))
  "Generate the NTH permutation of the vector VEC in place.
The algorithm is similar to the standard Fisher/Yates one, but instead
of random numbers [a_n-1,...,a_1] it represents a number in [0;n!] as
   x = a_n-1*(n-1)! + ... + a_1
The original vector is returned when NTH = (1- (! (length vec)))."
  (loop :for ff = fact :then (/ ff ii)
        :for ii :downfrom len :to 1 :with jj
        :do (setf (values jj nth) (floor nth ff))
        :unless (= jj ii)
        :do (rotatef (aref vec ii) (aref vec jj)))
  vec)

(defmacro with-permutations-shuffle ((var vec &optional ret-form) &body body)
  "Gererate the successive shufflings of vector VEC using `permutation'.
VEC is not modified, VAR storage is allocated only once,
not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The original vector is the last one returned."
  (with-gensyms ("WPU-" vv len len1 fact ii jj tot)
   `(let* ((,vv ,vec) (,len (length ,vv))  (,len1 (1- ,len)) (,fact (! ,len1))
            (,var (copy-seq ,vv)) (,tot (* ,len ,fact)))
      (dotimes (,ii ,tot ,(or ret-form tot))
        (dotimes (,jj ,len) (setf (aref ,var ,jj) (aref ,vv ,jj)))
        (permutation ,var ,ii ,len1 ,fact)
        ,@body))))

(defun check-permutations-end (name found length)
  (let ((fact (! length)))
    (unless (= found fact)
      (error "~s: generated ~:d permutation~:p, not ~d!=~:d, as expected"
             name found length fact))))

(defmacro with-permutations-swap ((var vec &optional ret-form) &body body)
  "Bind VAR to each permutation of vector VEC in turn, then execute the BODY.
Thus, BODY is evaluated (! (length vec)) times.
VEC is not modified; VAR storage is allocated only once,
not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The permutations are generated by transposing adjacent elements,
according to the CACM algorithm 115 [H.F.Trotter, Comm ACM 5 (Aug 1962) 434].
The original vector is the last one returned."
  (with-gensyms ("WPS-" nn pp dd kk qq ii done top)
    `(let* ((,var (copy-seq ,vec)) (,ii 0) (,nn (length ,var))
            (,top (- ,nn 2)) (,kk 0) (,qq 0) (,done nil)
            (,pp (make-array (1- ,nn) :element-type 'fixnum
                             :initial-element -1))
            (,dd (make-array (1- ,nn) :element-type 'fixnum
                             :initial-element 1)))
      (declare (type (array fixnum (*)) ,pp ,dd) (fixnum ,kk ,qq))
      (loop
       (tagbody
          (setq ,kk 0 ,top (- ,nn 2))
        :index
          (setf ,qq (+ (aref ,pp ,top) (aref ,dd ,top))
                (aref ,pp ,top) ,qq)
          (when (= ,qq (1+ ,top))
            (setf (aref ,dd ,top) -1)
            (go :loop))
          (when (/= -1 ,qq) (go :swap))
          (setf (aref ,dd ,top) 1)
          (incf ,kk)
        :loop
          (when (> ,top 0)
            (decf ,top)
            (go :index))
          (setq ,qq 0 ,done t)
        :swap
          (incf ,qq ,kk) (rotatef (aref ,var ,qq) (aref ,var (1+ ,qq))))
       ;;(format t "~4d * ~s [k ~d] [n ~d] [p ~s] [d ~s] [q ~d] [done ~s]~%"
       ;; ,ii ,var ,kk ,top ,pp ,dd ,qq ,done)
       (incf ,ii)
       ,@body
       (when ,done
         (check-permutations-end 'with-permutations-swap ,ii ,nn)
         (return ,(or ret-form ii)))))))

(defmacro with-permutations-lex ((var len &optional ret-form) &body body)
  "Bind VAR to each permutation of vector [0:LEN-1] in turn,
then execute the BODY - i.e, BODY is evaluated (! len) times.
VAR storage is allocated only once, not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The permutations are generated in the lexicographic order,
according to the CACM algorithm 202 [M.K.Shen, Comm ACL 6 (Sept 1963) 517]."
  (with-gensyms ("WPL-" ll nn ww ii)
    `(let* ((,ll ,len) (,var (make-vector-indexed ,ll)) (,nn 0))
      (declare (fixnum ,ll ,nn))
      (loop
       (unless (zerop ,nn)
         (let ((,ww (1- ,ll)))
           (declare (fixnum ,ww))
           (do () ((or (zerop ,ww) (< (aref ,var (1- ,ww)) (aref ,var ,ww))))
             (decf ,ww))
           (when (zerop ,ww)
             (check-permutations-end 'with-permutations-lex ,nn ,ll)
             (return ,(or ret-form nn)))
           (let ((,ii (position (aref ,var (1- ,ww)) ,var
                                :from-end t :test #'<)))
             (rotatef (aref ,var (1- ,ww)) (aref ,var ,ii)))
           (dotimes (,ii (ash (- ,ll ,ww) -1))
             (rotatef (aref ,var (- ,ll ,ii 1)) (aref ,var (+ ,ww ,ii))))))
       (incf ,nn)
       ,@body))))

(defun permutations-list (vec &key (method :lex))
  "Return the list of all the permutations of the vector VEC.
The order in which the permutations are listed is either
 lexicographic (when :METHOD is :LEX, which is the default),
  in which case `with-permutations-lex' is used;
 shuffling (when :METHOD is :SHUFFLE)
  in which case `with-permutations-shuffle' is used;
 transposing adjacent elements (when :METHOD is :SWAP),
  in which case `with-permutations-swap' is used.
:SWAP is more than twice as fast as :LEX
 and more that 10 times as fast as :SHUFFLE"
  (declare (ignorable method))
  (with-collect (coll)
    (ecase method
      (:lex (with-permutations-lex (vv (length vec))
              (let ((tv (copy-seq vec)))
                (dotimes (ii (length vec))
                  (setf (aref tv ii) (aref vec (aref vv ii))))
                (coll tv))))
      (:shuffle (with-permutations-shuffle (vv vec) (coll (copy-seq vv))))
      (:swap (with-permutations-swap (vv vec) (coll (copy-seq vv)))))))

(defun subsets (set)
  "return a list of all subsets of the given set (represented as a list)"
  (let ((first (first set)) (rest (rest set)))
    (if rest
        (let ((others (subsets rest)))
          (nconc others
                 (mapcar (lambda (subset)
                           (cons first subset))
                         others)))
        (list nil (list first)))))

(defun draw (distrib)
  "Return a random index from the distribution."
  (declare (type sequence distrib))
  (let ((len (length distrib)))
    (case len
      (0 (error "~S: no elements in the empty sequence ~S" 'draw distrib))
      (1
       (unless (= 1 (elt distrib 0))
         (error "~S: not a probability distribution: ~S" 'draw distrib))
       0)
      (t (let ((random (random 1s0)) (sum 0) (pos 0) (ret nil))
           (map nil (lambda (prob)
                      (incf sum prob)
                      (when (and (null ret) (< random sum))
                        (setq ret pos))
                      (incf pos))
                distrib)
           (when (>= (abs (- 1 sum)) short-float-epsilon)
             (error "~S: not a probability distribution (~S /= 1): ~S"
                    'draw sum distrib))
           ;; the error should never happen:
           ;; sum>1-short-float-epsilon ==> random<sum
           (or ret (error "~S: internal error: pos=~D sum=~S random=~S diff=~S"
                          'draw pos sum random
                          (- 1 short-float-epsilon sum))))))))

(defun pick (seq &optional distrib)
  "Return a random element from the sequence."
  (declare (type sequence seq))
  (let ((len (length seq)))
    (case len
      (0 (error "~S: no elements in the empty sequence ~S" 'pick seq))
      (1 (elt seq 0))
      (t (if distrib
             (progn
               (unless (= len (length distrib))
                 (error "~S: length mismatch: ~S ~S" 'pick seq distrib))
               (elt seq (draw distrib)))
             (elt seq (random len)))))))

(defun sample (seq count &key complement)
  "Return a random subset of size COUNT from sequence SEQ.
When :COMPLEMENT is non-NIL, the second value is the complement of the sample."
  (let* ((good '()) (drop '()) (len (length seq)) (got 0) (left len))
    (when (minusp count)
      (error "~S(~S ~S): cannot select a negative number of elements"
             'sample seq count))
    (map nil (lambda (elt)
               (cond ((> (* left (random 1s0)) (- count got))
                      (when complement (push elt drop)))
                     (t (push elt good) (incf got)))
               (decf left))
         seq)
    (values (nreverse good) (nreverse drop))))

;;;
;;; Ratios
;;;

(defun eval-cont-fract (fract)
  "Evaluate a continuous fraction, returning 2 values."
  (loop :for nn :of-type fixnum :in (reverse fract)
        :for v0 :of-type rational = nn :then (+ nn (/ v0))
        :for v1 :of-type rational = (1+ nn) :then (+ nn (/ v1))
        :finally (return (values v0 v1))))
  ;; (if (cdr fract)               ; recursive
  ;;     (multiple-value-bind (v0 v1) (eval-cont-fract (cdr fract))
  ;;       (values (+ (car fract) (/ v0)) (+ (car fract) (/ v1))))
  ;;     (values (car fract) (1+ (car fract)))))

(defcustom *num-tolerance* double-float #.(sqrt double-float-epsilon)
  "*The default numerical tolerance for `approx=-[abs|rel]'.")

(defsubst approx=-abs (f0 f1 &optional (tol *num-tolerance*))
  "Return T if the args are the same within TOL,
which defaults to *num-tolerance*."
  (declare (number f0 f1 tol))
  (< (abs (- f0 f1)) tol))

(defsubst approx=-rel (f0 f1 &optional (tol *num-tolerance*))
  "Return T if the args are the same relatively within TOL,
which defaults to *num-tolerance*. The first number goes to the
denominator."
  (declare (number f0 f1 tol))
  (< (abs (- f0 f1)) (abs (* f0 tol))))

(defcustom *relative-tolerance* double-float 1d-3
  "*The default relative tolerance for `approx='.")
(defcustom *absolute-tolerance* double-float 1d0
  "*The default absolute tolerance for `approx='.")
(defsubst approx= (v0 v1 &optional (rt *relative-tolerance*)
                   (at *absolute-tolerance*))
  "Check whether the two numbers are the same within the relative and
absolute tolerances (which default to *relative-tolerance* and
*absolute-tolerance*)."
  (declare (number v0 v1 rt at))
  (and (> at (abs (- v0 v1))) (> rt (rel-diff v0 v1))))

(defun fract-approx (xx &optional (eps *num-tolerance*))
  "Find an approximation via continous fractions."
  (declare (real xx eps))
  (loop :with val = xx :and num :of-type fixnum
        :and app0 :and app1 :and err0 :and err1
        :for ii :upfrom 0
        :do (setf (values num val) (floor val))
        :collect num :into fract
        :when (zerop val) :do (return (values (eval-cont-fract fract) fract 0))
        :do (setf (values app0 app1) (eval-cont-fract fract)
                  err0 (- xx app0) err1 (- xx app1) val (/ val))
        (format t "~4d [~15a ~15a] [~12,8f ~12,8f] ~12,9f ~12,9f~%"
                num app0 app1 (dfloat app0) (dfloat app1) err0 err1)
        :while (if (integerp eps) (> eps ii)
                   (or (< eps (abs err0)) (< eps (abs err1))))
        :finally
        (return
          (if (<= (abs err0) (abs err1)) (values app0 fract err0)
              (values app1 (progn (incf (car (last fract))) fract) err1)))))

;;;
;;; Floats
;;;

(defun dot (l0 l1 &key (key #'value) (key0 key) (key1 key))
  "Compute the dot-product of the two sequences,
presumed to be of the same size."
  (declare (sequence l0 l1))
  (let ((res 0))
    (map nil (lambda (r0 r1)
               (incf res (* (funcall key0 r0) (funcall key1 r1))))
         l0 l1)
    res))

(defun poly1 (var &rest coeffs)
  "Compute the polynomial with the given coefficients. Use the Horner scheme.
COEFFS are (a0 a1 a2 ...) for a0*x^n + a1*x^{n-1} + a2*x^{n-2}...
so that (poly1 10 1 2 3 4 5) ==> 12345."
  (declare (double-float var) (list coeffs))
  (let ((res 0d0))
    (declare (double-float res))
    (dolist (cc coeffs res)
      (declare (double-float cc))
      (setq res (+ cc (* var res))))))

(defun poly (var coeffs)
  "Compute the polynomial with the given coefficients. Use the Horner scheme.
COEFFS is a sequence #(a0 a1 a2 ...) for a0*x^n + a1*x^{n-1} + a2*x^{n-2}...
so that (poly 10 '(1 2 3 4 5)) ==> 12345."
  (declare (double-float var) (type sequence coeffs))
  (reduce (lambda (res coeff)
            (declare (double-float res coeff))
            (+ (* res var) coeff))
          coeffs :initial-value 0d0))

;;; This is a hand-modified version of the code generated by f2cl
;;; applied to the routine calerf from TOMS Algorithm 715.  The changes are:
;;;
;;; o added some comments
;;; o reindented some parts of the code
;;; o changed the type integer4 to (signed-byte 32)
;;; o changed AINT to floor
;;; o removed the usage of the fref macro
;;; o removed the fdo macro.
;;; o Compute the constants instead of having the approximations given
;;;   in the Fortran code
;;; o removed the arg result that was used to return the result.  The
;;;   function value is the result.

(let ((four 4.0d0)
      (one 1.0d0)
      (half 0.5d0)
      (two 2.0d0)
      (zero 0.0d0)
      (sqrpi (coerce (/ (sqrt pi)) 'double-float))
      (thresh 0.46875d0)
      (sixten 16.0d0)
      (xinf most-positive-double-float)
      ;; XNEG is the negative of the solution of 2*exp(x*x) = XINF.
      ;; Thus XNEG = -sqrt(log(XINF/2))
      (xneg (- (sqrt (log (/ most-positive-double-float 2)))))
      ;; argument below which erf(x) may be represented by
      ;; 2*x/sqrt(pi) and above which x*x will not underflow.
      ;; Conservatively, X such that 1+x=1.
      (xsmall double-float-epsilon)
      ;; largest argument acceptable to erfc; solution to the
      ;; equation: W(x) * (1-0.5/x**2) = XMIN, where W(x) =
      ;; exp(-x*x)/[x*sqrt(pi)].
      ;;
      ;; There's no analytic solution, and I'm too lazy to compute
      ;; this more accurately and erfc would underflow in this case.
      (xbig 26.543d0)
      ;; Number for which 1-1/(2*x*x) = 1.  That is, 1/(2*x*x) is
      ;; double-float-negative-epsilon.
      (xhuge (/ (sqrt (* 2 double-float-negative-epsilon))))
      ;; Largest acceptable arg to erfcx; the minimum of XINF and
      ;; 1/(sqrt(pi)*XMIN), where XMIN is the smallest positive
      ;; floating-point number (normalized)
      (xmax (min most-positive-double-float
		 (/ (* (coerce (sqrt pi) 'double-float)
		       least-positive-normalized-double-float))))
      (a
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(3.1611237438705655d0 113.86415415105016d0
                     377.485237685302d0 3209.3775891384694d0
                     0.18577770618460318d0)))
      (b
       (make-array 4
                   :element-type
                   'double-float
                   :initial-contents
                   '(23.601290952344122d0 244.02463793444417d0
                     1282.6165260773723d0 2844.2368334391704d0)))
      (c
       (make-array 9
                   :element-type
                   'double-float
                   :initial-contents
                   '(0.5641884969886701d0 8.883149794388377d0
                     66.11919063714163d0 298.6351381974001d0
                     881.9522212417692d0 1712.0476126340707d0
                     2051.078377826071d0 1230.3393547979972d0
                     2.1531153547440382d-8)))
      (d
       (make-array 8
                   :element-type
                   'double-float
                   :initial-contents
                   '(15.744926110709834d0 117.6939508913125d0
                     537.1811018620099d0 1621.3895745666903d0
                     3290.7992357334597d0 4362.619090143247d0
                     3439.3676741437216d0 1230.3393548037493d0)))
      (p
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(0.30532663496123236d0 0.36034489994980445d0
                     0.12578172611122926d0 0.016083785148742275d0
                     6.587491615298379d-4 0.016315387137302097d0)))
      (q
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(2.568520192289822d0 1.8729528499234604d0
                     0.5279051029514285d0 0.06051834131244132d0
                     0.0023352049762686918d0))))
  (declare (type (simple-array double-float (6)) p)
           (type (simple-array double-float (8)) d)
           (type (simple-array double-float (9)) c)
           (type (simple-array double-float (4)) b)
           (type (simple-array double-float (5)) q a)
           (type double-float xmax xhuge xbig xsmall xneg xinf sixten thresh
		 sqrpi zero two half one four))
  (defun calerf (arg jint)
    (declare (type (integer 0 2) jint)
	     (type double-float arg)
	     (optimize (speed 3)))
    (prog ((del 0.0d0) (x 0.0d0) (xden 0.0d0) (xnum 0.0d0) (y 0.0d0)
	   (ysq 0.0d0) (result 0d0))
       (declare (type double-float ysq y xnum xden x del result))
       (setf x arg)
       (setf y (abs x))
       (cond
	 ((<= y thresh)
	  ;; Compute erf(x) for |x| < 0.46875
	  (setf ysq zero)
	  (if (> y xsmall)
	      (setf ysq (* y y)))
	  (setf xnum (* (aref a (- 5 1)) ysq))
	  (setf xden ysq)
	  (loop for i of-type (integer 1 4) from 1 upto 3 do
		(tagbody
		   (setf xnum (* (+ xnum (aref a (- i 1))) ysq))
		   (setf xden (* (+ xden (aref b (- i 1))) ysq))
		 label20))
	  (setf result
		(/ (* x (+ xnum (aref a (- 4 1))))
		   (+ xden (aref b (- 4 1)))))
	  (if (/= jint 0) (setf result (- one result)))
	  (if (= jint 2) (setf result (* (exp ysq) result))) (go label800))
	 ((<= y four)
	  ;; Compute erfc for 0.46785 <= |x| <= 4
	  (setf xnum (* (aref c (- 9 1)) y)) (setf xden y)
	  (loop for i of-type (integer 1 8) from 1 upto 7 do
		(tagbody
		   (setf xnum (* (+ xnum (aref c (- i 1))) y))
		   (setf xden (* (+ xden (aref d (- i 1))) y))
		 label120))
	  (setf result
		(/ (+ xnum (aref c (- 8 1)))
		   (+ xden (aref d (- 8 1)))))
	  (cond
	    ((/= jint 2) (setf ysq (/ (the (signed-byte 32) (floor (* y sixten))) sixten))
	     (setf del (* (- y ysq) (+ y ysq)))
	     (setf result (* (exp (* (- ysq) ysq)) (exp (- del)) result)))))
	 (t
	  ;; Compute erfc for |x| > 4
	  (setf result zero)
	    (cond
	      ((>= y xbig) (if (or (/= jint 2) (>= y xmax)) (go label300))
	       (cond ((>= y xhuge) (setf result (/ sqrpi y)) (go label300)))))
	    (setf ysq (/ one (* y y))) (setf xnum (* (aref p (- 6 1)) ysq))
	    (setf xden ysq)
	    (loop for i of-type (integer 1 5) from 1 upto 4 do
		  (tagbody
		     (setf xnum (* (+ xnum (aref p (- i 1))) ysq))
		     (setf xden (* (+ xden (aref q (- i 1))) ysq))
		   label240))
	    (setf result
		  (/ (* ysq (+ xnum (aref p (- 5 1))))
		     (+ xden (aref q (- 5 1)))))
	    (setf result (/ (- sqrpi result) y))
	    (cond
	      ((/= jint 2) (setf ysq (/ (the (signed-byte 32) (floor (* y sixten))) sixten))
	       (setf del (* (- y ysq) (+ y ysq)))
	       (setf result (* (exp (* (- ysq) ysq)) (exp (- del)) result))))))
       label300
       (cond
	 ((= jint 0) (setf result (+ (- half result) half))
	  (if (< x zero) (setf result (- result))))
	 ((= jint 1) (if (< x zero) (setf result (- two result))))
	 (t
	  (cond
	    ((< x zero)
	     (cond ((< x xneg) (setf result xinf))
		   (t
		    (setf ysq (/ (the (signed-byte 32) (floor (* x sixten))) sixten))
		    (setf del (* (- x ysq) (+ x ysq)))
		    (setf y (* (exp (* ysq ysq)) (exp del)))
		    (setf result (- (+ y y) result))))))))
       label800
       (go end_label)
       end_label
       (return result))))

(declaim (inline erf erfc erfcx))

(defun erf (x)
  "Compute erf(x):

 		         x
 		        /       2
 	         2      [    - t
 erf(x) =    ---------  I  %E     dt
 	     SQRT(%PI)  ]
 		        /
 		         0
"
  (declare (type double-float x))
  (calerf x 0))

(defun erfc (x)
  "Compute erfc(x) = 1 - erf(x)

 		         INF
 		        /       2
 	         2      [    - t
 erfc(x) =   ---------  I  %E     dt
 	     SQRT(%PI)  ]
 		        /
 		         x
"
  (declare (type double-float x))
  (calerf x 1))

(defun erfcx (x)
  "Compute erfcx(x) = exp(x^2)*erfc(x):

 		   2     INF
 		  x     /       2
 	      2 %E      [    - t
 erfcx(x) =  ---------  I  %E     dt
 	     SQRT(%PI)  ]
 		        /
 		         x

"
  (declare (type double-float x))
  (calerf x 2))

(defun cndf (x)
  "Compute the cumulative normal distribution function.

				 2
			  x     t
			 /    - --
	          1	 [      2
 cndf(x) =  -----------  I  %E	   dt
	    SQRT(2*%PI)  ]
			 /
			  0


         = 1/2*erf(x/sqrt(2))
"
  (declare (double-float x))
  (* 0.5d0 (erf (/ x (sqrt 2d0)))))

(defun cndf-tail (x)
  "Compute the tail of the cumulative normal distribution function:

				  2
			   INF   t
			  /    - --
	          1	  [      2
 cndft(x) =  -----------  I  %E	   dt
	    SQRT(2*%PI)   ]
			  /
			   x
"
  (declare (double-float x))
  (* 0.5d0 (erfc (/ x (sqrt 2d0)))))

;;
;; Let the continued fraction be
;;
;; V = x0
;;     -------
;;     y0 + x1
;;          -------
;;          y1 + x2
;;              -------
;;              y2 + ...
;;
;; This function evaluates the above given fraction.  The numerator
;; terms are given by the function num; the denominator terms by the
;; function den.  Each function takes a single arg:  the index.
;;
;; n0 and d0 are x0 and y0 above
(defun continued-fraction (n0 d0 num den)
  (let* ((p0 0)
	 (q0 1)
	 (v0 1)
	 (p1 n0)
	 (q1 d0)
	 (v1 (/ p1 q1))
	 (k 1))
    (loop
	(let ((y (funcall den k))
	      (x (funcall num k)))
	  (multiple-value-bind (p2 q2)
	      (values (+ (* p1 y)
			 (* p0 x))
		      (+ (* q1 y)
			 (* q0 x)))
	    (let ((v2 (/ p2 q2))
		  (scale (max (abs p2) (abs q2))))
	      ;; Stop when the relative change is less than some
	      ;; multiple of epsilon
	      (when (<= (abs (/ (- v2 v1) (+ v2 v1))) (* 2 long-float-epsilon))
		;; (format t ";; Iterations required: ~A~%" k)
		(return v2))
	      ;; (format t "~4D ~A ~A ~A~%" k p2 q2 v2)
	      (when (> scale (sqrt most-positive-long-float))
		;; Rescale the numbers if we are in danger of overflowing.
		(let ((power-of-2 (- (nth-value 1 (decode-float scale)))))
		  (setf p1 (* p1 (scale-float 1L0 power-of-2)))
		  (setf p2 (* p2 (scale-float 1L0 power-of-2)))
		  (setf q1 (* q1 (scale-float 1L0 power-of-2)))
		  (setf q2 (* q2 (scale-float 1L0 power-of-2)))))

	      (psetf p0 p1
		     p1 p2)
	      (psetf q0 q1
		     q1 q2)
	      (psetf v0 v1
		     v1 v2)
	      (incf k)))))))

;; Here are versions of erf and erfc using continued fractions.  This
;; allows us to compute (somewhat slowly) these functions for
;; long-float numbers as available in Clisp.

(let ((threshold 0.5L0))
  ;; To preserve accuracy, we want threshold such that erf(x) =
  ;; erfc(x) = 1/2. erf(0.5) is 0.52, so that's close enough. However,
  ;; the continued fraction for erfc is quite slow for x < 4, so you
  ;; may want to a larger threshold, but don't make it any larger than
  ;; 1 where you lose 2-3 bits of accuracy.
  (defun erf-cf (z)
    (flet ((num (k)
	     (let ((result (* 2 k z z)))
	       (if (oddp k)
		   (- result)
		   result)))
	   (den (k)
	     (+ k k 1)))
      (if (> (abs z) threshold)
	  (- 1 (erfc-cf z))
	  (* (/ 2 (sqrt pi))
	     (exp (- (* z z)))
	     (continued-fraction z 1 #'num #'den)))))
  (defun erfc-cf (z)
    (flet ((num (k)
	     (/ k 2))
	   (den (k)
	     (declare (ignore k))
	     z))
      (if (< (abs z) threshold)
	  (- 1 (erf-cf z))
	  (* (/ (sqrt pi))
	     (exp (- (* z z)))
	     (continued-fraction 1 z #'num #'den))))))

(defun log-gamma (x)
  "log(gamma(x)), x>0
Numerical Recipes 6.1."
  (let ((tmp (+ x 5.5d0)))
    (+ (log (/ (* #.(sqrt (* 2 (float pi 1d0)))
                  (+ 1.000000000190015d0
                     (/ 76.18009172947146d0 (+ x 1))
                     (/ -86.50532032941677d0 (+ x 2))
                     (/ 24.01409824083091d0 (+ x 3))
                     (/ -1.231739572450155d0 (+ x 4))
                     (/ 0.1208650973866179d-2 (+ x 5))
                     (/ -0.5395239384953d-5 (+ x 6))))
               x))
       (- tmp) (* (+ x 0.5d0) (log tmp)))))

(defun beta (x y)
  "Beta function = G(x)G(y)/G(x+y)"
  (exp (- (+ (log-gamma x) (log-gamma y)) (log-gamma (+ x y)))))

(defun incomplete-gamma (a x &optional (log-gamma-a (log-gamma a)))
  "Incomplete gamma function int(G,0,x)/G(a).
Also accepts (and returns) ln(G(a)) for speedup.
The 3rd value returned is the number of iterations.
Numerical Recipes 6.2 (modified Lentz's method, 5.2)."
  (unless (plusp a)
    (error "~S(~S,~S): non-positive A" 'incomplete-gamma a x))
  (when (minusp x)
    (error "~S(~S,~S): negative X" 'incomplete-gamma a x))
  (if (or (< x (+ a 2)) (< x (* a 2)))
      (incomplete-gamma-series a x log-gamma-a)
      (incomplete-gamma-continued-fraction a x log-gamma-a)))

(defvar *max-iterations* 1000 "The maximum permitted number of iterations.")
(defun incomplete-gamma-error (f a x log-gamma-a n v delta)
  (error "~S(~S ~S ~S): exceeded max number of iterations (~:D); intermediate value=~S; termination expression ~S"
         f a x log-gamma-a n v delta))

(defun incomplete-gamma-series (a x &optional (log-gamma-a (log-gamma a)))
  (if (zerop x)
      (values 0 log-gamma-a 0)
      (do* ((a+ (+ a 1d0) (1+ a+)) (sum (/ 1d0 a)) (term sum) (n 1 (1+ n)))
           ((< term (* sum *num-tolerance*))
            (values (* sum (exp (- (* a (log x)) x log-gamma-a)))
                    log-gamma-a n))
        (when (= n *max-iterations*)
          (incomplete-gamma-error 'incomplete-gamma-series
                                  a x log-gamma-a n sum term))
        (mulf term (/ x a+))
        (incf sum term))))

(defun incomplete-gamma-continued-fraction
    (a x &optional (log-gamma-a (log-gamma a)))
  (if (zerop x)
      (values 0 log-gamma-a 0)
      (do* ((n 1 (1+ n)) (an (* (- a n) n) (* (- a n) n))
            (b (- x a -1d0) (+ 2 b))
            (c #1=#.(/ (* double-float-epsilon double-float-epsilon))
               (let ((tmp (+ b (/ an c))))
                 (if (< (abs tmp) #2=#.(* double-float-epsilon
                                          double-float-epsilon))
                     #2# tmp)))
            (d (if (zerop b) #1# (/ b))
               (let ((tmp (+ b (* an d))))
                 (if (< (abs tmp) #2#)
                     #1# (/ tmp))))
            (delta (* c d) (* c d))
            (h d (* h delta)))
           ((approx=-abs delta 1)
            (values (- 1 (* h (exp (- (* a (log x)) x log-gamma-a))))
                    log-gamma-a n))
        (when (= n *max-iterations*)
          (incomplete-gamma-error 'incomplete-gamma-continued-fraction
                                  a x log-gamma-a n h delta)))))

;;; the continued-fraction and series approximations are very different,
;;; especially close to X=0 where series is (correctly) 0 while
;;; continued-fraction is 1(!); thus I am reluctant to use the CF
;;; approximation for anything but very large values of X

#+(or)
(plot-functions
 (loop :for n :in '(1 2 4 8 16)
   :collect (let ((nn n))
              (cons nn (lambda (x) (incomplete-gamma nn x (log-gamma nn))))))
 0 15 100 :title "incomplete-gamma"
 :legend '(:top :left :box))

#+(or)
(plot-functions
 (loop :for n :in '(4 8 16)
   :collect (let* ((nn n) (g (log-gamma nn)))
              (cons nn (lambda (x)
                         (or (ignore-errors
                               (- (incomplete-gamma-continued-fraction nn x g)
                                  (incomplete-gamma-series nn x)))
                             0)))))
 0 15 100 :title "incomplete-gamma: cf-series"
 :legend '(:bot :right :box))

#+(or)
(let* ((a 5) (lg (log-gamma a)))
  (plot-functions
   (list (cons 'cf (lambda (x) (incomplete-gamma-continued-fraction a x lg)))
         (cons 'ser (lambda (x) (incomplete-gamma-series a x lg))))
   0 10 100 :title (format nil "incomplete-gamma(~S)" a)
   :legend '(:bot :right :box)))


(defun norm-functions (order)
  "Return a triple PRE COMBINE POST for computing NORM and ARRAY-DIST."
  (case order
    (0 (values #'abs #'max #'identity))
    (1 (values #'abs #'+ #'identity))
    (2 (values (lambda (xx) (* xx xx)) #'+ #'sqrt))
    (t (values (lambda (xx) (expt (abs xx) order)) #'+
               (lambda (xx) (expt xx (/ order)))))))

(defun norm (seq &key (key #'value) (order 1))
  "Compute the ORDERth norm of the SEQ. ORDER of 0 means infinity."
  (declare (sequence seq) (real order) (type (function (t) double-float) key))
  (multiple-value-bind (pre combine post) (norm-functions order)
    (funcall post (reduce combine seq :key (compose 'pre 'key)))))

(defun normalize (seq &optional (norm #'norm))
  "Make sure the SEQ have unit NORM.
Return the modified SEQ and the original NORM."
  (declare (sequence seq) (type (or real (function (t) double-float)) norm))
  (setq seq (delete nil seq))
  (let ((nn (etypecase norm
              (real (norm seq :order norm))
              (function (funcall norm seq)))))
    (declare (double-float nn))
    (assert (> nn 0) (seq) "Zero norm vector: ~a" seq)
    (values (map-into seq (lambda (rr) (/ rr nn)) seq)
            nn)))

(defun rel-dist (seq1 seq2 &key (key #'value) (start1 0) (start2 0) depth)
  "Return the square of the relative mismatch between the 2 sequences."
  (declare (sequence seq1 seq2) (type (or null fixnum) depth)
           (type (function (t) double-float) key) (fixnum start1 start2))
  (let ((b1 (funcall key (elt seq1 start1))) (dist 0d0)
        (b2 (funcall key (elt seq2 start2)))
        (depth (or depth (min (- (length seq1) start1)
                              (- (length seq2) start2)))))
    (declare (double-float dist b1 b2) (fixnum depth))
    (mismatch seq1 seq2 :key key :start1 (1+ start1) :start2 (1+ start2)
              :end1 (+ start1 depth) :end2 (+ start2 depth) :test
              (lambda (k1 k2) (declare (double-float k1 k2))
                      (incf dist (sqr (- (/ k1 b1) (/ k2 b2))))))
    dist))

;;;
;;; Statistics
;;;

(defun mean (seq &key (key #'value) (len (length seq)))
  "Compute the mean of the sequence of real numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (fixnum len))
  (values (/ (reduce #'+ seq :key key) len) len))

(defun mean-weighted (seq wts &key (value #'value) (weight #'value))
  "Compute the weighted mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (sequence seq wts) (type (function (t) number) weight))
  (let ((twt (reduce #'+ wts :key weight)))
    (values (/ (dot seq wts :key0 value :key1 weight) twt)
            twt)))

(defun mean-geometric (seq &key (key #'value))
  "Compute the geometric mean of the sequence of numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) double-float) key))
  (let ((len (length seq)))
    (values (expt (reduce #'* seq :key key) (/ len)) len)))

(defun mean-geometric-weighted (seq wts &key (value #'value) (weight #'value))
  "Compute the weighted geometric mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (sequence seq wts) (type (function (t) double-float) value weight))
  (let ((twt (reduce #'+ wts :key weight)) (res 1d0))
    (declare (double-float res twt))
    (map nil (lambda (rr wt)
               (setq res (* res (expt (funcall value rr)
                                      (/ (funcall weight wt) twt)))))
         seq wts)
    res))

(defun mean-some (seq &key (key #'value))
  "Compute the mean of the sequence of real numbers.
NULLs are ignored, so this is like (mean (remove nil seq :key key) :key key).
Return 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) (or null double-float)) key))
  (let ((len 0))
    (declare (type index-t len))
    (values (s/ (reduce #'+ seq :key (lambda (rr)
                                       (let ((val (funcall key rr)))
                                         (cond (val (incf len) val)
                                               (0d0)))))
                len)
            len)))

(defmacro min+max (val min min$ max max$)
  "Create a form to update MIN, MAX and their counts MIN$ and MAX$ from VAL.
All arguments are symbols, not forms!"
  `(progn
     (cond ((or (null ,min) (< ,val ,min)) (setq ,min ,val ,min$ 1))
           ((= ,val ,min) (incf ,min$)))
     (cond ((or (null ,max) (> ,val ,max)) (setq ,max ,val ,max$ 1))
           ((= ,val ,max) (incf ,max$)))))

(defun standard-deviation (seq &key (len (length seq)) (key #'value)
                           (mean (mean seq :key key :len len)))
  "Compute the standard deviation of the sequence SEQ.
The mean and the length can be pre-computed for speed."
  (declare (sequence seq) (fixnum len))
  (when (<= len 1)
    (return-from standard-deviation (values 0 mean len mean 1 mean 1)))
  (let (min max mi$ ma$)
    (values
     (sqrt (/ (reduce #'+ seq :key (lambda (yy)
                                     (let ((val (funcall key yy)))
                                       (min+max val min mi$ max ma$)
                                       (sqr (- val mean)))))
              (1- len)))
     mean len min mi$ max ma$)))

(defun standard-deviation-weighted (seq wts &key
                                    (value #'value) (weight #'value))
  "Compute the standard deviation of the sequence with weights."
  (declare (sequence seq wts) (type (function (t) number) weight))
  (multiple-value-bind (mn twt)
      (mean-weighted seq wts :value value :weight weight)
    (when (= twt 1)
      (return-from standard-deviation-weighted (values 0d0 mn twt mn 1 mn 1)))
    (let ((sum 0d0) min max mi$ ma$)
      (map nil (lambda (xx ww)
                 (let ((val (funcall value xx)))
                   (min+max val min mi$ max ma$)
                   (incf sum (* (funcall weight ww) (sqr (- val mn))))))
           seq wts)
      (values (sqrt (/ sum (1- twt))) mn twt min mi$ max ma$))))

(defsubst standard-deviation-cx (&rest args)
  "Return the `standard-deviation' of SEQ as #C(mean stdd)."
  (multiple-value-bind (stdd mean) (apply #'standard-deviation args)
    (complex mean stdd)))

(defun standard-deviation-relative (seq &key (key #'value))
  "Compute the relative standard deviation (StD(log(x[i+1]/x[i]))).
Meaningful only if all the numbers are of the same sign,
if this is not the case, the result will be a complex number."
  (declare (sequence seq))
  (let (pr (sq 0d0) (su 0d0) (nn 0))
    (declare (double-float sq su) (type (or null double-float) pr)
             (type index-t nn))
    (map nil (lambda (rr)
               (let* ((cc (funcall key rr))
                      (vv (when pr (log (/ cc pr)))))
                 (setq pr cc)
                 (when vv (incf sq (sqr vv)) (incf su vv) (incf nn))))
         seq)
    (values (sqrt (max 0d0 (/ (- sq (/ (sqr su) nn)) (1- nn)))) nn)))

(defun entropy-distribution (seq &key (count #'value))
  "Compute the entropy of the distribution with the given density.
Each element should be the count: how many time the corresponding element
occurs, i.e., the normalized sequence should be the probability distribution."
  (let* ((tot 0)
         (sum (reduce #'+ seq
                      :key (lambda (el)
                             (let ((num (funcall count el)))
                               (incf tot num)
                               (if (zerop num) 0 (* num (log num 2))))))))
    (- (log tot 2) (/ sum tot))))

(defun entropy-sequence (seq &key (key #'value) (test 'eql) (weight 1))
  "Compute the entropy of the given distribution.
The values are counted and the result is used as a probability distribution.
 (multiple-value-bind (entropy ht) (entropy-sequence seq)
   (assert (= entropy
              (entropy-distribution
               (loop :for v :being :each :hash-value :of ht :collect v)))))"
  (let* ((ht (count-all seq :key key :test test :weight weight))
         (tot 0) (sum 0))
    (loop :for num :being :each :hash-value :of ht :do
      (incf tot num)
      (incf sum (* num (log (dfloat num) 2)))) ; we know num>0
    (values (- (log (dfloat tot) 2) (/ sum tot)) ht)))

(defun kullback-leibler (seq1 seq2 &key (key1 #'value) (key2 #'value))
  "Compute the Kullback-Leibler distance (aka relative entropy) of the
two probability distributions, as well as both of their individual entropies.
NIL result stands for infinity."
  (let ((kl 0) (ent1 0) (ent2 0) (sum1 0) (sum2 0))
    (map nil (lambda (e1 e2)
               (let ((p1 (funcall key1 e1)) (p2 (funcall key2 e2)))
                 (incf sum1 p1) (incf sum2 p2)
                 (when (plusp p1)
                   (decf ent1 (* p1 (log p1 2)))
                   (if (plusp p2)
                       (when kl (incf kl (* p1 (log (/ p1 p2) 2))))
                       (setq kl nil)))
                 (when (plusp p2) (decf ent2 (* p2 (log p2 2))))))
         seq1 seq2)
    (unless (approx=-abs sum1 1)
      (error "~S: total probability is ~S /= 1" 'kullback-leibler sum1))
    (unless (approx=-abs sum2 1)
      (error "~S: total probability is ~S /= 1" 'kullback-leibler sum2))
    (values kl ent1 ent2)))

(defun kurtosis-skewness (seq &key (key #'value) std mean len)
  "Compute the skewness and kurtosis (3rd & 4th centered momenta)."
  (declare (sequence seq))
  (unless std (setf (values std mean len) (standard-deviation seq :key key)))
  (let ((skew 0d0) (kurt 0d0))
    (map nil (lambda (rr)
               (let* ((cc (/ (- (funcall key rr) mean) std))
                      (c3 (* cc cc cc)))
                 (incf skew c3)
                 (incf kurt (* cc c3))))
         seq)
    (values (/ kurt (1- len)) (/ skew (1- len)) std mean len)))

(defun kurtosis-skewness-weighted (seq wts &key std mean tot
                                   (value #'value) (weight #'value))
  "Compute the skewness and kurtosis (3rd & 4th centered momenta)."
  (declare (sequence seq))
  (unless std
    (setf (values std mean tot)
          (standard-deviation-weighted seq wts :value value :weight weight)))
  (let ((skew 0d0) (kurt 0d0))
    (map nil (lambda (rr ww)
               (let* ((cc (/ (- (funcall value rr) mean) std))
                      (wt (funcall weight ww))
                      (c3 (* wt cc cc cc)))
                 (incf skew c3)
                 (incf kurt (* c3 cc))))
         seq wts)
    (values (/ kurt (1- tot)) (/ skew (1- tot)) std mean tot)))

(defun covariance (seq0 seq1 &key (key0 #'value) (key1 #'value))
  "Compute the covariance between the data in the two sequences.
Return 6 values: covariance, mean0, mean1, variance0,
variance1, number of elements considered.
Uses the fast but numerically unstable algorithm
without pre-computing the means."
  (declare (sequence seq0 seq1) (type (function (t) double-float) key0 key1))
  (let ((xb 0d0) (yb 0d0) (x2b 0d0) (xyb 0d0) (y2b 0d0)
        (nn 0) (c0 0d0) (c1 0d0))
    (declare (double-float xb yb x2b xyb y2b c0 c1) (type index-t nn))
    (map nil (lambda (r0 r1)
               (let ((xx (funcall key0 r0)) (yy (funcall key1 r1)))
                 (declare (double-float xx yy))
                 (incf nn) (incf xb xx) (incf yb yy) (incf y2b (sqr yy))
                 (incf xyb (* xx yy)) (incf x2b (sqr xx))))
         seq0 seq1)
    (assert (> nn 1) (nn) "Too few (~d) points are given to covariance!" nn)
    (setq c0 (/ (dfloat nn)) c1 (/ (dfloat (1- nn))))
    (values (with-type double-float (* (- xyb (* xb yb c0)) c1))
            (with-type double-float (* xb c0))
            (with-type double-float (* yb c0))
            (with-type double-float (* (- x2b (* xb xb c0)) c1))
            (with-type double-float (* (- y2b (* yb yb c0)) c1))
            nn)))

(defun covariance1 (seq0 seq1 &key (key0 #'value) (key1 #'value))
  "Compute the covariance between the data in the two sequences.
Return 6 values: covariance, mean0, mean1, variance0,
variance1, number of elements considered.
Uses the numerically stable algorithm with pre-computing the means."
  (declare (sequence seq0 seq1) (type (function (t) double-float) key0 key1))
  (let ((m0 (dfloat (mean seq0 :key key0)))
        (m1 (dfloat (mean seq1 :key key1)))
        (nn 0) (d0 0d0) (d1 0d0) (rr 0d0) (co 0d0))
    (declare (fixnum nn) (double-float m0 m1 d0 d1 rr co))
    (map nil (lambda (r0 r1)
               (let ((xx (- (funcall key0 r0) m0))
                     (yy (- (funcall key1 r1) m1)))
                 (declare (double-float xx yy))
                 (incf nn) (incf d0 (sqr xx)) (incf d1 (sqr yy))
                 (incf rr (* xx yy))))
         seq0 seq1)
    (assert (> nn 1) (nn) "Too few (~d) points are given to covariance!" nn)
    (setq co (/ (dfloat (1- nn))))
    (values (* rr co) m0 m1 (* d0 co) (* d1 co) nn)))

(defsubst cov (seq &key (xkey #'car) (ykey #'cdr))
  "Interface to `covariance' with one sequence."
  (covariance seq seq :key0 xkey :key1 ykey))

(defun volatility (lst split-key &rest args
                   &key (dev-fn #'standard-deviation-relative)
                   &allow-other-keys)
  "Return volatilities for the terms corresponding to SPLIT-KEY.
The first value returned is the mean of the volatilities,
the second - the volatilities themselves.
E.g., (volatility dated-list (compose date-ye date) :key #'value)
will return the average annual volatility for the value in the dated-list
and the list of the volatilities for each year."
  (declare (type (or function fixnum) split-key) (list lst)
           (type (function (sequence) double-float) dev-fn))
  (let ((vols (apply #'call-on-split lst dev-fn :split-key split-key
                     :min-len 2 (remove-plist args :dev-fn))))
    (values (mean vols :key #'cdr) vols)))

;;; Mean / Deviation / Length

(defstruct (mdl)
  "MDL structure contains sample statistics.
It is printed as follows: [mean standard-deviation max/min length[ entropy]]
When the distribution is not discreet, entropy is not available."
  (mn 0d0 :type double-float)       ; Mean
  (sd 0d0 :type (double-float 0d0)) ; Deviation
  (ma 0 :type number)               ; Max
  (ma$ 0 :type index-t)             ; Max count
  (mi 0 :type number)               ; Min
  (mi$ 0 :type index-t)             ; Min count
  (en 0 :type (or null number)) ; entropy (only when discreet)
  (le 0 :type index-t))         ; Length

(defconst +bad-mdl+ mdl (make-mdl) "The convenient constant for init.")
(defmethod value ((mdl mdl)) (mdl-mn mdl))

(defmethod print-object ((mdl mdl) (out stream))
  (if *print-readably* (call-next-method)
      (let* ((en (mdl-en mdl))
             (fo (if en (formatter "~:D") (formatter "~6F")))
             (ma (format nil fo (mdl-ma mdl)))
             (mi (format nil fo (mdl-mi mdl)))
             (ma$ (format nil "~:D" (mdl-ma$ mdl)))
             (mi$ (format nil "~:D" (mdl-mi$ mdl))))
        (format out "[~6f ~6f ~6@A/~5A ~6@A/~5A ~5:d~@[ ~6f~]]"
                (mdl-mn mdl) (mdl-sd mdl) ma ma$ mi mi$ (mdl-le mdl) en))))

(defun standard-deviation-mdl (seq &key (key #'value) weight discreet)
  "Compute an MDL from the SEQ."
  (let ((len (length seq)))
    (if (zerop len) +bad-mdl+
        (multiple-value-bind (std mean len min mi$ max ma$)
            (if weight
                (standard-deviation-weighted seq seq :value key :weight weight)
                (standard-deviation seq :len len :key key))
          (make-mdl :sd (dfloat std) :mn (dfloat mean) :le len
                    :mi min :ma max :mi$ mi$ :ma$ ma$
                    :en (when discreet
                          (if (= min max) 0
                              (entropy-sequence seq :key key
                                                :weight weight))))))))

(declaim (inline mdl-normalize mdl-denormalize mdl-normalize-function))
(defun mdl-normalize (value mdl)   (/ (- value (mdl-mn mdl)) (mdl-sd mdl)))
(defun mdl-denormalize (value mdl) (+ (* value (mdl-sd mdl)) (mdl-mn mdl)))
(defun mdl-normalize-function (function mdl)
  "return a normalized version of FUNCTION"
  (lambda (x) (mdl-normalize (funcall function x) mdl)))

;; when the mdl argument is a constant object, access slots
;; note that this is _NOT_ safe in general and compilers must _NOT_ do this
;; automatically: the constant argument's slots might be modified elsewhere
#|
 (define-compiler-macro mdl-normalize (&whole form value mdl)
  (if (mdl-p mdl)
      `(/ (- ,value ,(mdl-mn mdl)) ,(mdl-sd mdl))
      form))
 (define-compiler-macro mdl-denormalize (&whole form value mdl)
  (if (mdl-p mdl)
      (+ (* ,value ,(mdl-sd mdl)) ,(mdl-mn mdl))
      form))
|#

(defun normalizer-table (functions list &key (out *standard-output*)
                         &aux (mdl-ht (make-hash-table)))
  "Return the HASH-TABLE mapping functions to their normalizers.
FUNCTIONS is a LIST of SYMBOLs or LISTs of SYMBOLs, each naming a FUNCTION."
  (flet ((stat-sym (f)
           (unless (gethash f mdl-ht)
             (let ((mdl (standard-deviation-mdl list :key (fdefinition f))))
               (when out (format out "~&~30@S: ~S~%" f mdl))
               (setf (gethash f mdl-ht) mdl)))))
    (dolist (fl functions mdl-ht)
      (etypecase fl
        (list (mapc #'stat-sym fl))
        (symbol (stat-sym fl))))))

(defun normalize-function-list (fl mdl-ht)
  "Return a function which is a sum of elements of FL normalized with MDL-HT.
If A and B are functions, then
 (A B) ==> (lambda (x) (+ (A/N x) (B/N x)))
where `X/N' means normalized with (GETHASH X MDL-HT)"
  `(lambda (x)
     (+ ,@(mapcar (lambda (f)
                    ;; what if the user modifies mdl-ht and expects
                    ;; the returned functions to change automatically?
                    ;;(let ((m (gethash f mdl-ht)))
                    ;;  `(/ (- (,f x) ,(mdl-mn m)) ,(mdl-sd m)))
                    `(mdl-normalize (,f x) ,(or (gethash f mdl-ht)
                                                (error "~S: ~S is not in ~S"
                                                       'normalize-function-list
                                                       f mdl-ht))))
                  fl))))

;;;
;;; information-theoretic and statistical measures of prediction performance
;;;

(defun check-probabilities (p12 p1 p2 N caller)
  "check that the arguments are valid probabilities"
  (assert (and (>= N p12 0) (>= N p1 0) (>= N p2 0)
               (>= p1 p12) (>= p2 p12) (>= p12 (- (+ p1 p2) N)))
          (p12 p1 p2)
          "~s: invalid probabilities: ~s ~s ~s" caller p12 p1 p2))

(defsubst info-component (p12 p1 p2)
  "one component of information computation"
  (if (zerop p12) 0 (* p12 (log (/ p12 p1 p2) 2))))

(defun information (p &optional (N 1) &aux (q (- N p)))
  "information of distribution P(0)=p/N, P(1)=1-p/N"
  ;; == (mutual-information p p p)
  (+ (/ (+ (info-component q q q)
           (info-component p p p))
        N)
     (log N 2)))

(defun mutual-information (p12 p1 p2 &optional (N 1)
                           &aux (q1 (- N p1)) (q2 (- N p2)))
  "Mutual information of two distributions:
p1=p(x=1), p2=p(y=1), p12=p(x=1 & y=1)"
  (check-probabilities p12 p1 p2 N 'mutual-information)
  (+ (/ (+ (info-component p12 p1 p2)                  ; x=1 y=1
           (info-component (- p2 p12) q1 p2)           ; x=0 y=1
           (info-component (- p1 p12) p1 q2)           ; x=1 y=0
           (info-component (- (+ N p12) p1 p2) q1 q2)) ; x=0 y=0
        N)
     (log N 2)))

(defun dependency (p12 p1 p2 &optional (N 1))
  "Mutual information normalized by total entropy"
  (let ((mi (mutual-information p12 p1 p2 N)))
    (/ mi (- (+ (information p1 N) (information p2 N)) mi))))

(defun proficiency (p12 p1 p2 &optional (N 1))
  "information-theoretic predictive power of the first relative to the second"
  (/ (mutual-information p12 p1 p2 N) (information p1 N)))

(defun correlation (p12 p1 p2 &optional (N 1) &aux (q1 (- N p1)) (q2 (- N p2)))
  "correlation of two binary distributions"
  (check-probabilities p12 p1 p2 N 'correlation)
  (/ (+ (* p12 q1 q2)                          ; x=1 y=1
        (* (- p1 p12) q1 (- p2))               ; x=0 y=1
        (* (- p2 p12) (- p1) q2)               ; x=1 y=0
        (* (- (+ N p12) p1 p2) (- p1) (- p2))) ; x=0 y=0
     (sqrt (* p1 q1 p2 q2)) N))

(defun 1st-moment (p12 p1 p2)
  "covariance of two binary distributions"
  (/ p12 (sqrt (* p1 p2))))

;;;
;;; Misc
;;;

(defmacro safe-fun (func pred &optional default)
  "Return a function that will run FUNC when the PRED is non-NIL.
If PRED is NIL return DEFAULT or the arguments if DEFAULT is omitted."
  (if default
      `(lambda (&rest xx) (if (apply ,pred xx) (apply ,func xx) ,default))
      `(lambda (&rest xx) (if (apply ,pred xx) (apply ,func xx)
                              (values-list xx)))))

(defmacro safe-fun1 (func pred &optional default)
  "Return a function that will run FUNC when the PRED is non-NIL.
Just like `safe-fun', but the returned function takes just 1 argument.
If PRED is NIL return DEFAULT or the arguments if DEFAULT is omitted."
  (if default
      `(lambda (xx) (if (,pred xx) (,func xx) ,default))
      `(lambda (xx) (if (,pred xx) (,func xx) xx))))

(defun safe-/ (aa &rest bb)
  "Safe division."
  (declare (number aa) (list bb))
  (if (some #'zerop bb)
      (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
            (most-negative-fixnum))
      (apply #'/ aa bb)))

(defsubst s/ (aa bb)
  "Fast safe division; only 2 arguments are allowed."
  (declare (number aa bb))
  (if (zerop bb) (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
                       (most-negative-fixnum))
      (/ aa bb)))

(defsubst d/ (aa bb)
  "Double float fast safe division; only 2 arguments are allowed."
  (declare (double-float aa bb))
  (if (zerop bb) (cond ((zerop aa) 1d0)
                       ((plusp aa) most-positive-double-float)
                       (most-negative-double-float))
      (/ aa bb)))

(defun convex-hull1 (lst up &key (xkey #'car) (ykey #'cdr))
  "Compute the `directional' convex hull of a list of 2d points.
More precisely, destructively modify LST, deleting all the points
that lie below (if UP is non-nil) or above (if UP is nil) the upper
\(or lower) part of the boundary of the convex hull.
The accessor keys XKEY and YKEY default to CAR and CDR.
Return the modified LST. 20% slower than `convex-hull'."
  (declare (list lst) (type (function (t) double-float) xkey ykey))
  (do ((mod t) dd (ts (if up #'minusp #'plusp))) ((null mod) lst)
    (setq mod nil)
    (do ((ll lst (cdr ll))) ((null (cddr ll)))
      (setq dd (- (funcall ykey (second ll))
                  (linear (funcall xkey (first ll)) (funcall ykey (first ll))
                          (funcall xkey (third ll)) (funcall ykey (third ll))
                          (funcall xkey (second ll)))))
      (when (funcall ts dd)
        (setf (cdr ll) (cddr ll) mod t)))))

(defun convex-hull (lst up &key (xkey #'car) (ykey #'cdr))
  "Compute the `directional' convex hull of a list of 2d points.
More precisely, destructively modify LST, deleting all the points
that lie below (if UP is non-nil) or above (if UP is nil) the upper
\(or lower) part of the boundary of the convex hull.
The accessor keys XKEY and YKEY default to CAR and CDR.
Return the modified LST. 20% faster than `convex-hull1'."
  (declare (list lst) (type (function (t) double-float) xkey ykey))
  (do* ((ll lst (cdr ll)) (ts (if up #'> #'<)) (x0 0d0) (y0 0d0)
        (sl (lambda (pp)        ; has to be d/!!!
              (d/ (with-type double-float (- (funcall ykey pp) y0))
                  (abs (with-type double-float (- (funcall xkey pp) x0)))))))
       ((null (cddr ll)) lst)
    (declare (double-float x0 y0) (type function ts)
             (type (function (t) double-float) sl))
    (setf x0 (funcall xkey (car ll)) y0 (funcall ykey (car ll))
          (cdr ll)
          (do ((l1 (cddr ll) (cdr l1)) (top (cdr ll)) (csl 0d0)
               (tsl (funcall sl (cadr ll))))
              ((null l1) top)
            (declare (double-float csl tsl))
            (setq csl (funcall sl (car l1)))
            (when (funcall ts csl tsl) (setq tsl csl top l1))))))

(defun sharpe-ratio (seq &key (key #'value))
  "Compute the Sharpe ratio (mean/StD) of the sequence SEQ."
  (declare (sequence seq))
  (multiple-value-bind (mm len) (mean seq :key key)
    (s/ mm (standard-deviation seq :mean mm :len len :key key))))

(defmacro to-percent (vv)
  "1.234 ==> 23.4%"
  `(* 100d0 (1- ,vv)))

(defun percent-change (v0 v1 &optional days)
  "Return the percent change in values, from V0 to V1.
If the optional DAYS is given, return the annualized change too."
  (declare (number v0 v1) (type (or null number) days))
  (if (zerop v0) (values 0d0 0d0)
      (let ((pers (dfloat (/ v1 v0))))
        (if (and days (not (zerop days)))
            (values (to-percent pers)
                    (if (zerop days) 0d0
                        (to-percent (expt pers (/ 365.25d0 days)))))
            (to-percent pers)))))

(defun rel-diff (v0 v1)
  "Return the relative difference between the two numbers.
This function is commutative, and puts the smallest number into the
denominator.  Sign is ignored."
  (declare (double-float v0 v1))
  (d/ (abs (- v1 v0)) (min (abs v0) (abs v1))))

(defun binary-search (beg end func)
  "Find the point where FUNC's value changes between BEG and END.
To look for a zero, use (COMPOSE PLUSP MY-FUNC) as FUNC."
  (loop :for vb = (funcall func beg) :for ve = (funcall func end)
    :for mid = (/ (+ beg end) 2) :for vm = (funcall func mid)
    :when (or (eql vb ve) (eql beg mid) (eql end mid))
    :return (values beg end vb ve)
    :do (if (eql vm vb) (setq beg mid) (setq end mid))))

(defun newton (ff &key (val 0) (ival val) (*num-tolerance* *num-tolerance*)
               (max-it *max-iterations*))
  "Solve the equation FF(x)=VAL using the Newton's method.
FF should return its derivative as the second value.
IVAL is the initial approximation, and it better be good!
MAX-IT is the maximum number of iterations. If -1 (default), unlimited.
Returns the solution, the last change (more or less the error),
and the number of iterations made."
  (declare (type (function (number) (values number number)) ff)
           (number val ival) (fixnum max-it))
  (do ((xx ival) f0 f1 (del 10) (it 0 (1+ it)))
      ((or (approx=-abs del 0) (= max-it it)) (values xx del it))
    (declare (type index-t it))
    (setf (values f0 f1) (funcall ff xx))
    (incf xx (setq del (/ (- val f0) (if (zerop f1) *num-tolerance* f1))))))

(defun integrate-simpson (ff x0 xm &optional (*num-tolerance* *num-tolerance*))
  "Compute an integral of a real-valued function with a given precision.
Returns the integral, the last approximation, and the number of points."
  (declare (double-float x0 xm)
           (type (function (double-float) double-float) ff))
  (do* ((f0 (funcall ff x0)) (f1 (funcall ff (* 0.5d0 (+ x0 xm))))
        (fm (funcall ff xm)) (hh (* 0.5d0 (- xm x0)) (* hh 0.5d0))
        (mm 2 (* mm 2))
        (sum-even 0d0 (+ sum-odd sum-even))
        (sum-odd
         f1 (loop :for ii :of-type index-t :from 1 :below mm :by 2
                  :and step :of-type double-float :from 1d0 :by 2d0
                  :sum (funcall ff (+ x0 (* hh step))) :of-type double-float))
        (int-last 0d0 int)
        (int (* (/ hh 3d0) (+ f0 (* 4d0 f1) fm))
             (* (/ hh 3d0) (+ f0 (* 4d0 sum-odd) (* 2d0 sum-even) fm))))
       ((approx=-abs int int-last) (values int int-last mm))
    (declare (double-float sum-odd sum-even hh f0 f1 fm int int-last)
             (type index-t mm))))

(defun add-probabilities (&rest pp)
  "Add probabilities.
Returns the probability of at least one event happening."
  (- 1 (reduce #'* pp :key (lambda (xx) (- 1 xx)))))

;;;
;;; Line
;;;

(defstruct (line)
  "A straight line."
  (sl 0d0 :type double-float) ; slope
  (co 0d0 :type double-float)) ; constant

(defconst +bad-line+ line (make-line) "*The convenient constant for init.")

(defmethod print-object ((ln line) (out stream))
  (if *print-readably* (call-next-method)
      (format out "{~6f ~6f}" (line-sl ln) (line-co ln))))

(declaim (ftype (function (line double-float) (values double-float)) line-val))
(defsubst line-val (ln par)
  "Evaluate the line at point."
  (declare (type line ln) (double-float par))
  (with-type double-float (+ (* par (line-sl ln)) (line-co ln))))

(declaim (ftype (function (line) (values double-float)) line-rsl))
(defsubst line-rsl (ln)
  "Return the relative slope of the line."
  (declare (type line ln))
  (d/ (line-sl ln) (line-co ln)))

(defsubst line-below-p (ln xx yy)
  "Return T if the point (xx yy) is below the line LN."
  (declare (double-float xx yy) (type line ln))
  (< yy (line-val ln xx)))

(defsubst line-above-p (ln xx yy)
  "Return T if the point (xx yy) is above the line LN."
  (declare (double-float xx yy) (type line ln))
  (> yy (line-val ln xx)))

(defun intersect (line x0 y0 x1 y1)
  "Return T if the points (x0 y0) and (x1 y1) are on the opposite
sides of the LINE. Call: (intersect LINE X0 Y0 X1 Y1)"
  (declare (type line line) (double-float x0 y0 x1 y1))
  (not (plusp (* (- y0 (line-val line x0)) (- y1 (line-val line x1))))))

(defmacro with-line (ln xx yy above below upon
                     &optional (tol *num-tolerance*))
  "Eval ABOVE/BELOW/UPON depending on the relative position of line LN and
point (XX YY) up to tolerance TOL.  Similar to FORTRAN's arithmetic IF."
  (with-gensyms ("WL-" di)
    `(let ((,di (- (line-val ,ln ,xx) ,yy)))
      (declare (double-float ,di))
      (cond ((> ,di ,tol) ,below) ((< ,di (- ,tol)) ,above) (t ,upon)))))

(declaim (ftype (function (line double-float double-float) (values line))
                line-adjust))
(defsubst line-adjust (ln xx yy)
  "Adjust the line LN to pass through the point, keeping the slope intact."
  (declare (double-float xx yy) (type line ln))
  (setf (line-co ln) (with-type double-float (- yy (* (line-sl ln) xx)))) ln)

(declaim (ftype (function (line double-float double-float t) (values line))
                line-adjust-dir))
(defun line-adjust-dir (ln xx yy up)
  "Adjust the line LN to be above (if UP) or below (otherwise) of (xx yy)."
  (declare (double-float xx yy) (type line ln))
  (if (funcall (if up #'line-above-p #'line-below-p) ln xx yy)
      (line-adjust ln xx yy)
      ln))

(defun line-adjust-list (ln ls up &key (xkey #'car) (ykey #'cdr))
  "Adjust the line LN to pass above (if UP) or below (otherwise) of LS,
keeping the slope intact."
  (declare (type line ln) (list ls)
           (type (function (t) double-float) xkey ykey))
  (do ((ff (if up #'line-above-p #'line-below-p))
       (ll ls (cdr ll)) (xx 0d0) (yy 0d0))
      ((endp ll) ln)
    (declare (double-float xx yy) (type function ff))
    (setq xx (funcall xkey (car ll)) yy (funcall ykey (car ll)))
    (when (funcall ff ln xx yy) (line-adjust ln xx yy))))

(declaim (ftype (function (double-float double-float double-float double-float)
                          (values line))
                line-thru-points))
(defsubst line-thru-points (x0 y0 x1 y1)
  "Make a new line, passing through these 2 points.
If (= x0 x1), an error will be signaled."
  (declare (double-float x0 y0 x1 y1))
  (make-line :co (with-type double-float (/ (- (* y0 x1) (* y1 x0)) (- x1 x0)))
             :sl (with-type double-float (/ (- y1 y0) (- x1 x0)))))

(defun regress (seq &key (xkey #'car) (ykey #'cdr))
  "Return the regression line for the sequence of 2d points.
The second value returned is the deviation from the line.
The accessor keys XKEY and YKEY default to CAR and CDR respectively."
  (declare (sequence seq) (type (function (t) double-float) xkey ykey))
  (case (length seq)
    ((0 1) (error "regress: too few points: ~s~%" seq))
    (2 (values (line-thru-points (funcall xkey (elt seq 0))
                                 (funcall ykey (elt seq 0))
                                 (funcall xkey (elt seq 1))
                                 (funcall ykey (elt seq 1)))
               0d0))
    (t (multiple-value-bind (co xm ym xd yd nn)
           (cov seq :xkey xkey :ykey ykey)
         (declare (double-float co xm ym xd yd) (fixnum nn))
         (let* ((sl (/ co xd)) (err (d/ (- yd (* sl co)) yd)))
           (declare (double-float sl err))
           (when (minusp err)
             (mesg :err t "REGRESS: error is negative: ~f (assumed 0) [len: ~d]
~s~%" err nn seq)
             (setq err 0d0))
           (values (make-line :sl sl :co
                              (with-type double-float (- ym (* xm sl))))
                   (sqrt err)))))))


(declaim (ftype (function (double-float double-float double-float double-float)
                          (values double-float))
                lincom))
(defsubst lincom (c0 x0 c1 x1)
  "Compute c0*x0+c1*x1."
  (declare (double-float c0 x0 c1 x1))
  (with-type double-float (+ (* c0 x0) (* c1 x1))))


;;;
;;; piecewise linear functions
;;;

(defstruct plf
  "piecewise linear function"
  (extend-left :error)  ; out of range behavior: :linear/:constant/:error
  (extend-right :error) ; separate for right and left
  x y)                          ; x & y vectors

(defun plf-val (plf par)
  "Evaluate the piecewise linear function at a point."
  (let* ((xv (plf-x plf)) (yv (plf-y plf)) (len (length xv))
         (box (position par xv :test #'<=)))
    (macrolet ((val (i)
                 `(let* ((next ,i) (here (1- next)))
                    (linear (aref xv here) (aref yv here)
                            (aref xv next) (aref yv next) par))))
      (if box
          (if (= par (aref xv box))
              (aref yv box)
              (if (zerop box)
                  (ecase (plf-extend-left plf)
                    (:constant (aref yv 0))
                    (:linear (val 1))
                    (:error (error "~S: ~F is out of left range for ~S"
                                   'plf-val par plf)))
                  (val box)))
          (ecase (plf-extend-right plf)
            (:constant (aref yv (1- len)))
            (:linear (val (1- len)))
            (:error (error "~S: ~F is out of right range for ~S"
                           'plf-val par plf)))))))

(defun monotonic-p (vec &key (key #'value))
  "return a symbol - <,>,<=,>= or NIL which describes argument's monotonicity"
  (loop :with <? = t :and =? = nil :and >? = t ; init strict monotonic
    :for pos :from 1 :to (1- (length vec))
    :for next = (funcall key (aref vec pos))
    :and prev = (funcall key (aref vec 0)) :then next
    :unless =? :do (setq =? (= prev next))
    :when <? :do (setq <? (<= prev next))
    :when >? :do (setq >? (>= prev next))
    :unless (or >? <?) :return nil
    :finally (return (if <? (if =? '<= '<) (if =? '>= '>)))))

(defun plf-monotonic-p (plf) (monotonic-p (plf-y plf) :key #'identity))

(defun plf->function (plf) (lambda (x) (plf-val plf x)))

(defun plf-size (plf &aux (size (length (plf-x plf))))
  (assert (= size (length (plf-y plf))) (plf)
          "~S: invalid PLF: ~:D /= ~:D: ~S"
          'plf-size size (length (plf-y plf)) plf)
  size)

(defun plf-integral (plf)
  (loop :with xv = (plf-x plf) :and yv = (plf-y plf)
    :for pos :from 1 :to (1- (plf-size plf))
    :for x-next = (aref xv pos) :and y-next = (aref yv pos)
    :and x-prev = (aref xv 0) :then x-next
    :and y-prev = (aref yv 0) :then y-next
    :sum (* (- x-next x-prev) (+ y-next y-prev) 1/2)))

(defun remove-elements (pos-list vec)
  "remove the elements at these positions from the vector"
  (let* ((vec-size (length vec))
         (ret (make-array (- vec-size (length pos-list)))))
    (loop :with ret-pos = 0 :with rem = (pop pos-list)
      :for vec-pos :upfrom 0 :while rem
      :if (= rem vec-pos) :do (setq rem (pop pos-list))
      :else :do (setf (aref ret ret-pos) (aref vec vec-pos)) (incf ret-pos)
      :end
      :finally (replace ret vec :start1 ret-pos :start2 vec-pos))
    ret))

(defun plf-simplify (plf)
  "Destructively remove unnecessary nodes."
  (let* ((size (plf-size plf)) (xv (plf-x plf)) (yv (plf-y plf))
         (to-be-removed
          (loop :for pos :from 2 :to (- size 1)
            :for y2 = (aref yv pos)
            :and y1 = (aref yv 1) :then y2
            :for y0 = (aref yv 0) :then y1
            ;; although the first case (= y0 y1 y2) is covered by the second
            ;; one mathematically, it is necessary due to roundoff errors
            :when (or (= y0 y1 y2)
                      (= y1 (linear (aref xv (- pos 2)) y0
                                    (aref xv pos) y2
                                    (aref xv (1- pos)))))
            :collect (1- pos))))
    (setf (plf-x plf) (remove-elements to-be-removed xv)
          (plf-y plf) (remove-elements to-be-removed yv))
    (values plf to-be-removed)))

(defcustom *increasify-step* (real 0 (1/2)) 3d-1
  "*The parameter which determines how aggressive INCREASIFY is.")
(defun increasify (vec &key (step *increasify-step*))
  "make the vector increasing, preserving the sum of elements"
  (loop :with keep-going = t :while keep-going :do
    (setq keep-going nil)
    (loop :for pos :from 1 :to (1- (length vec))
      :for next = (aref vec pos) :and prev = (aref vec 0) :then next
      :unless (<= prev next)
      :do (setf prev (- prev next)
                (aref vec (1- pos)) (+ next (* step prev))
                next (+ next (* (- 1 step) prev))
                (aref vec pos) next
                keep-going t)))
  (assert (case (monotonic-p vec :key #'identity) ((< <=) t)) (vec)
          "~S: result is not monotonic: ~S" 'increasify vec)
  vec)


(provide :cllib-math)
;;; math.lisp ends here
