;;; -*- mode: lisp -*-
(in-package :cl-user)

;; Your fd-stream-read-n-bytes (in 17e) crashes when reading from a
;; pipe and it didn't get the requested byte-count (it should re-read
;; because pipe-reads may be interrupted). You have done some changes
;; in from 17c to 17e (I think) but it dosen't work yet. Here is a old
;; patched version that works for us.


;;An alist with SETF and a function name causes
;;an error whenever it's used:

(check-for-bug :cmucl-bugs-legacy-14
  (defparameter foo '((setf . sqrt)))
  FOO)


(check-for-bug :cmucl-bugs-legacy-19
  foo
  ((SETF . SQRT)))


(check-for-bug :cmucl-bugs-legacy-24
  (setq foo '((zut . 4)))
  ((ZUT . 4)))


(check-for-bug :cmucl-bugs-legacy-29
  foo
  ((ZUT . 4)))


(check-for-bug :cmucl-bugs-legacy-34
  (setq foo '((setf . 3)))
  ((SETF . 3)))


(check-for-bug :cmucl-bugs-legacy-39
  '(setq . 2)
  (setq . 2))

(unintern 'foo)

;;


(check-for-bug :cmucl-bugs-legacy-48
  (* 10000000000000000000000000000000000000000
     10000000000000000000000000000000000000000)

  100000000000000000000000000000000000000000000000000000000000000000000000000000000)


(check-for-bug :cmucl-bugs-legacy-55
  (time (+ 2 2))
  4)

;; cltl2 p 727


(check-for-bug :cmucl-bugs-legacy-62
  (let ((stack (copy-list '(a b c d e f))))
    (loop for item = (length stack) then (pop stack) while stack
          collect item))
  (6 A B C D E))

;; p 737

(check-for-bug :cmucl-bugs-legacy-70
  (loop with (a b c) of-type (float integer float)
        return (list a b c))
  (0.0 0 0.0))


(check-for-bug :cmucl-bugs-legacy-76
  (loop with ( a b c) float
        return (list a b c))
  (0.0 0.0 0.0))


;; printing arrays


(check-for-bug :cmucl-bugs-legacy-85
  (make-array '(22) :element-type 'single-float :initial-element 0.0)
  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))


(check-for-bug :cmucl-bugs-legacy-90
  (make-array '(2 2))
  #-clisp
  #2A((0 0) (0 0))
  #+clisp
  #2A((NIL NIL) (NIL NIL)))


(check-for-bug :cmucl-bugs-legacy-98
  (make-array '(2 2) :element-type 'single-float :initial-element 0.0)
  #2A((0.0 0.0) (0.0 0.0)))

;; without pretty-print?

(check-for-bug :cmucl-bugs-legacy-104
  (make-array '(22) :element-type 'single-float :initial-element 0.0)
  #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))


(check-for-bug :cmucl-bugs-legacy-109
  (make-array '(2 2))
  #-clisp
  #2A((0 0) (0 0))
  #+clisp
  #2A((NIL NIL) (NIL NIL)))

(check-for-bug :cmucl-bugs-legacy-116
  (make-array '(2 2) :element-type 'single-float :initial-element 0.0)
  #2A((0.0 0.0) (0.0 0.0)))

;; bignums


(check-for-bug :cmucl-bugs-legacy-123
  (defun factorial (n &optional (i 1))
    (if (plusp n) (factorial (1- n) (* i n)) i))
  FACTORIAL)


(check-for-bug :cmucl-bugs-legacy-129
  (/ (factorial 100) (factorial 99))
  100)


(check-for-bug :cmucl-bugs-legacy-134
  (/ (factorial 1000) (factorial 999))
  1000)

(unintern 'factorial)

(check-for-bug :cmucl-bugs-legacy-140
  1e-37
  10.0e-38)


(check-for-bug :cmucl-bugs-legacy-145
  1L-38
  10.0L-39)


(check-for-bug :cmucl-bugs-legacy-150
  (flet ((print-result (x)
           (format nil "~&x is ~F (a ~S)." x (type-of x))))
    (print-result "non-number"))
  "x is non-number (a (SIMPLE-BASE-STRING 10))."
  "Notice that ~3,2F does work.")

(check-for-bug :cmucl-bugs-legacy-157
  (defun sigmoid (x)
    (/ 1 (1+ (exp (- x)))))
  SIGMOID)


(check-for-bug :cmucl-bugs-legacy-163
  (compile 'sigmoid)			; in CMU Common Lisp 17f
  SIGMOID)

#-clisp
(check-for-bug :cmucl-bugs-legacy-168
  (sigmoid 100)
  1.0)


(unintern 'sigmoid)

(check-for-bug :cmucl-bugs-legacy-175
  (setq X (copy-list '((1 2) (1 2 3) (3))))
  ((1 2) (1 2 3) (3)))


(check-for-bug :cmucl-bugs-legacy-180
  (remove-duplicates X :test #'subsetp)
  ((1 2 3) (3)))


(check-for-bug :cmucl-bugs-legacy-185
  (delete-duplicates X :test #'subsetp)
  ((1 2 3) (3)))


(unintern 'X)

(check-for-bug :cmucl-bugs-legacy-192
  (progn
    (run-program "/bin/date" '() :output t :error :stream)
    t)
  t)
;; #<process 780 :EXITED>



(check-for-bug :cmucl-bugs-legacy-201
  (- 0.0 #C( 1.0 1.0))
  #C(-1.0 -1.0))


(check-for-bug :cmucl-bugs-legacy-206
  (- #C(.5 .866) 0.0)
  #C(0.5 0.866))



(check-for-bug :cmucl-bugs-legacy-212
  (/ 2.0 #C(-1.0 -1.0))
  #C(-1.0 1.0))


(check-for-bug :cmucl-bugs-legacy-217
  (* 2.0 #C(-1.0 -1.0))
  #C(-2.0 -2.0))



(check-for-bug :cmucl-bugs-legacy-223
  (with-open-file
      (foo "/tmp/foocl"
           :direction :output
           :element-type
           (list 'signed-byte (1+ (integer-length
                                   most-positive-fixnum))))
    (write-byte 17 foo)
    (write-byte -17 foo)
    (write-byte 4517 foo)
    (write-byte -1217 foo))
  -1217)


(check-for-bug :cmucl-bugs-legacy-237
  (unwind-protect
       (with-open-file
           (foo "/tmp/foocl"
                :direction :input
                :element-type
                (list 'signed-byte (1+ (integer-length
                                        most-positive-fixnum))))
         (list (read-byte foo)
               (read-byte foo)
               (read-byte foo)
               (read-byte foo)))
    (delete-file "/tmp/foocl"))
  (17 -17 4517 -1217))


(check-for-bug :cmucl-bugs-legacy-251
  (unless (ignore-errors (error "grr"))
    (print "hi"))
  "hi")


(check-for-bug :cmucl-bugs-legacy-257
  (setf (elt '(a b c d) 2) 'x)
  x)


(check-for-bug :cmucl-bugs-legacy-262
  (acos 1.00001)
  #+(or cmu sbcl)
  #C(0.0 0.004475168)
  #+clisp
  #C(0 0.0044751023)
  #-(or clisp cmu sbcl)
  fill-this-in)


(check-for-bug :cmucl-bugs-legacy-272
  (parse-namestring (make-pathname :defaults "tst"))
  #p"tst")


(check-for-bug :cmucl-bugs-legacy-277
  (string< "abcd" "012abcz" :start2 3 :end2 6)
  NIL)


(check-for-bug :cmucl-bugs-legacy-282
  (string> "abcd" "012abcd" :start2 3 :end2 5)
  2)


(check-for-bug :cmucl-bugs-legacy-287
  (defun (setf foo) () t)
  (setf foo))


(check-for-bug :cmucl-bugs-legacy-292
  (compile '(setf foo))
  (setf foo))


(check-for-bug :cmucl-bugs-legacy-297
  (typep '(setf cons)
         'generic-function)
  NIL)


(check-for-bug :cmucl-bugs-legacy-303
  (make-sequence '(vector float) 4  :initial-element 0.0)
  #(0.0 0.0 0.0 0.0))


(check-for-bug :cmucl-bugs-legacy-308
  (typep (complex 0.0d0) '(complex double-float))
  t
  "complex returns a number whose real part is realpart
and whose imaginary part is imagpart.

If realpart is a rational and imagpart is the rational
number zero, the result of complex is realpart, a rational.
Otherwise, the result is a complex.

If either realpart or imagpart is a float, the non-float
is converted to a float before the complex is created. If
imagpart is not supplied, the imaginary part is a zero of
 the same type as realpart; i.e., (coerce 0 (type-of
realpart)) is effectively used.

the second parameter is not supplied, the first is
a double-float, so actually this is (complex 0.0d0 0.0d0)
these are not rationals, so we get a complex number back.
")


;; From: Gary Bunting <gbunting@cantor.une.edu.au>
(check-for-bug :cmucl-bugs-legacy-331
  (setf xx (expt 3 32))
  1853020188851841)

(check-for-bug :cmucl-bugs-legacy-335
  (* xx xx)
  3433683820292512484657849089281)


#|					;
(defun bugged (x)
  (labels ((f (y &optional trouble)	;  <<< or &key or &rest ..
             (if y
                 (let ((a (pop y)))
                   (f a)))))

;;;; (f x) <<<
;;;; Error in function COMMON-LISP::ASSERT-ERROR:
;;;; The assertion (EQ (C::LAMBDA-TAIL-SET C::CALLER)
;;;;               (C::LAMBDA-TAIL-SET (C::LAMBDA-HOME C::CALLEE)))
;;;; failed.

;;; However this works ok.
    (f x nil)))
|#
(check-for-bug :cmucl-bugs-legacy-356
 (defun bugged (x)
   (labels ((f (y &optional trouble)	;  <<< or &key or &rest ..
	       (if y
		   (let ((a (pop y)))
		     (f a)))))
     (f x)))
 BUGGED)

(check-for-bug :cmucl-bugs-legacy-365
 (bugged (list (list)))
 NIL)

(unintern 'bugged)

(check-for-bug :cmucl-bugs-legacy-371
 (defun tst()
   (with-open-file
    (stream "does-not-exist" :if-does-not-exist nil)
    (unless stream
      'abacab)))
 TST)

(check-for-bug :cmucl-bugs-legacy-379
 (tst)
 abacab)

(unintern 'tst)

(check-for-bug :cmucl-bugs-legacy-385
 (defun f (a b)
   (declare (type (single-float  0.0 0.5) a)
	    (type (single-float  0.0 0.2) b)
	    (optimize (debug 0) (safety 0) (speed 3)))
   (expt a b))
 F)

(check-for-bug :cmucl-bugs-legacy-393
 (progn
   (compile 'f)
   t)
 t)

;;; deltax^2 == deltat

;;; from Paul Werkowski

(check-for-bug :cmucl-bugs-legacy-403
 (progn
   (compile-file "compile-bug5.lisp")
   :ok)
 :ok)

(check-for-bug :cmucl-bugs-legacy-409
 (progn
   (compile-file "compile-bug6.lisp")
   :ok)
 :ok)

(check-for-bug :cmucl-bugs-legacy-415
 (progn
   (defclass cl1 ()())
   (defclass cl2 (cl1 missing)())
   (defclass cl4 ()())

   (defmethod foo ((c cl2))
     c)
   ;; method specializing on class with fwd reference
   ;; ok so far

   ;; then this dies

   (defmethod foo ((c cl4))
     c)   ;; add a new method to gf #'foo
   t)
 T)

(check-for-bug :cmucl-bugs-legacy-433
 (progn
   (defmethod foo ((f function))
     f)
   (defun zzz (x)
     x)
   (foo #'zzz)   ;; this is supposed to work.
   t)
 t)

(unintern 'zzz)

#+(or sbcl cmu)
(check-for-bug :cmucl-bugs-legacy-446
 (progn
   (compile-file "compile-bug1.lisp")
   :ok)
 :ok)



;;; From: William Harold Newman <william.newman@airmail.net>

(check-for-bug :cmucl-bugs-legacy-456
 (equalp #\a 'a)
 nil)

(defun my-sxhash (x)
  (declare (type double-float x))
  (sxhash x))

(check-for-bug :cmucl-bugs-legacy-464
 (eq (my-sxhash 1.2d0)
     (sxhash 1.2d0))
 T)

(check-for-bug :cmucl-bugs-legacy-469
 (progn
   (compile 'my-sxhash)
   (eq (my-sxhash 1.2d0)
       (sxhash 1.2d0)))
 T)


;;; From: Raymond Toy <toy@rtp.ericsson.se>

(defun tst2 (x n)
  (declare (type (integer -134217728 134217728) x)
           (type (integer -4 4) n)
           (optimize (speed 3) (safety 0)))
  (ash x n))

(check-for-bug :cmucl-bugs-legacy-485
 (compile 'tst2)
 tst2)

;; From pvaneynd:

(check-for-bug :cmucl-bugs-legacy-491
 (exp 1)
 2.7182817)

(check-for-bug :cmucl-bugs-legacy-495
 (macrolet ((foobar (a b)
		    `(+ ,a ,b)))
   (foobar 2 4))
 6)

(check-for-bug :cmucl-bugs-legacy-501
 (macrolet ((foobar (a b)
		    `(+ ,a ,b)))
   (foobar 2 4 5 6))
 program-error)


;;; From: Marco Antoniotti <marcoxa@parades.rm.cnr.it>

(check-for-bug :cmucl-bugs-legacy-510
 (progn
   (defclass ccc () ())
   (setf (find-class 'ccc1) (find-class 'ccc))
   :ok)
 :ok)

(check-for-bug :cmucl-bugs-legacy-517
 (progn
   (defmethod zut ((c ccc1)) 123)
   :ok)
 :ok)


;;; From: Fred Gilham <gilham@snapdragon.csl.sri.com>


(check-for-bug :cmucl-bugs-legacy-527
 (progn
   (compile-file "compile-bug2.lisp")
   :ok)
 :ok)

;;; From: lyle@cogni.iaf.cnrs-gif.fr (Lyle Borg-Graham)

(check-for-bug :cmucl-bugs-already-compile-foo
  (defun foo ()
    (loop for x from 1.0 to 10.0
          maximize x into max single-float))
  foo)

(check-for-bug :cmucl-bugs-legacy-539
 (compile 'foo)
 foo)

;;; From: Timothy Miller <tsm@cs.brown.edu>

#+(or cmu sbcl)
(check-for-bug :cmucl-bugs-legacy-546
 (> 2 single-float-positive-infinity)
 NIL)

;;; From: "Fernando D. Mato Mira" <matomira@iname.com>

(defun prolog-length (p)
  (let ((x (length (car p))))
    (reduce #'(lambda (v1 v2)
		(declare (ignore v1))
		(setq x (+ x (length v2))))
	    p)))

(check-for-bug :cmucl-bugs-legacy-559
 (compile 'prolog-length)
 prolog-length)

(check-for-bug :cmucl-bugs-legacy-563
 (prolog-length (list (list 1 2)
		      (list 3)))
 3)

(check-for-bug :cmucl-bugs-legacy-568
 (progn
   (compile-file "compile-bug3.lisp")
   :ok)
 :ok)

(check-for-bug :cmucl-bugs-legacy-574
 (progn
   (compile-file "compile-bug4.lisp")
   :ok)
 :ok)

(check-for-bug :cmucl-bugs-legacy-580
 (progn
   (compile-file "compile-bug4nt.lisp")
   :ok)
 :ok)

(check-for-bug :cmucl-bugs-legacy-586
 (prolog-length (list (list 1 2)
		      (list 3)))
 3)

;;; From: Sam Steingold <sds@gnu.org>
#+UNIX
(check-for-bug :cmucl-bugs-legacy-593
 (let ((z (make-concatenated-stream
	   (make-string-input-stream "abc")
	   (open "/etc/hosts"))))
   (read-line z)
   (concatenated-stream-streams z)
   :ok)
 :ok)


;;; From: Hannu Koivisto <azure@iki.fi>

(check-for-bug :cmucl-bugs-legacy-605
 (case t)
 nil)

;;; From: Raymond Toy <toy@rtp.ericsson.se>

(check-for-bug :cmucl-bugs-legacy-611
 (progn
  (with-open-file (file "/tmp/foobar"
			:direction :output
			:if-exists :supersede)
		  (princ #\F file))
  (unwind-protect
       (with-open-file (file "/tmp/foobar"
                             :direction :input)
         (let ((c (peek-char nil file nil 'eof t)))
           (list c (read file)
                 (peek-char nil file nil 'eof t))))
    (delete-file "/tmp/foobar")))
 (#\F F EOF))

;;; From Barry Margolin:

#+cmu
(check-for-bug :cmucl-bugs-legacy-627
 (> (length
     (pcl:generic-function-lambda-list
      (ensure-generic-function 'change-class)))
    2)
 T
 "change-class (instance t) (new-class symbol) &rest initargs")


;; From Geddis@Cadabra.Com (Don Geddis)

(check-for-bug :cmucl-bugs-equalp-hash-table
  (let ((ht (make-hash-table :test #'equalp)))
    (setf (gethash 4.0 ht) 'four)
    (gethash 4 ht))
  FOUR)


(check-for-bug :cmucl-bugs-unread-from-string
  (let* ((input (make-string-input-stream "xy"))
         (concat (make-concatenated-stream input)))
    (prog1
        (let* ((x (read-char concat))
               (unread-x (unread-char x concat))
               (x2 (read-char concat))
               (y (read-char concat nil :EOF)))
          (list x unread-x x2 y))
      (close input)
      (close concat)))
  (#\x NIL #\x #\y)
  "From Gilbert Baumann")

(check-for-bug :cmucl-bugs-unread-from-file
  (progn
    (with-open-file (sink "/tmp/tmp"
                          :direction :output
                          :if-exists :new-version)
      (write-string "xy" sink))
  (unwind-protect
       (with-open-file (input "/tmp/tmp" :direction :input)
         (let* ((concat (make-concatenated-stream input))
                (x (read-char concat))
                (unread-x (unread-char x concat))
                (x2 (read-char concat))
                (y (read-char concat nil :EOF)))
           (list x unread-x x2 y)))
    (delete-file "/tmp/tmp")))
  (#\x NIL #\x #\y)
  "From Gilbert Baumann")

(check-for-bug  :cmucl-bugs-loop-destructuring
  (loop with (a b) of-type float = (list 0.0 1.0)
        and (c d) of-type float = (list 2.0 3.0)
        return (list a b c d))
  (0.0 1.0 2.0 3.0)
  "From: Wolfhard Buss")

(check-for-bug :cmucl-bug-log-using-wrong-floats
  (progn
    (log #c(0.4 0.5))
    nil)
  nil
  "From Michael A. Koerber")


(check-for-bug :cmucl-compilation-error-with-labels
  (functionp
   (compile
    nil
    (lambda (final-indices &rest tensors+indices)
      (labels
          ((find-buggy-indices (rest-tensors+indices seen-so-far-hash)
             ;; return a list of badly-behaved indices (occurring too many times)
             ;; every index that does not appear exactly twice is badly behaved.
             ;; (either must be sum index and appear twice in sum, or appear twice
             ;; as free-left and free-right index.)
             ;; XXX NOTE: we only check for well-formed-ness, not if
             ;; index ranges are compatible.
             (if (null rest-tensors+indices)
                 (let ((bad-indices nil))
                   (maphash
                    #'(lambda (k v)
                        (if (i/= v 2)
                            (ppush bad-indices k)))
                    seen-so-far-hash)
                   bad-indices)
                 (progn
                   (dolist (ix (cdar rest-tensors+indices))
                     (if (not (and (consp ix)) (eql (car ix) :fix))
                         (hv-inc seen-so-far-hash ix)))
                   (find-buggy-indices (cdr rest-tensors+indices) seen-so-far-hash))))
           ;;
           (contract-fix-reduce-1-tensor (tensor indices final-indices) ; don't confuse final-indices with our outer defun parameter!
             (mv-bind (li-rest-indices nr-rest-indices nr-contractions li-pairs)
                      (_remove-contraction-indexpairs indices)
                      (let* ((v-given-initial-indices (coerce indices '(simple-array * (*))))
                             (nr-initial-indices (array-dimension v-given-initial-indices 0))
                             (nr-fixations
                              (reduce #'(lambda (x y)
                                          (if (and (consp x) (eq (car x) :fix))
                                              (i1+ y)
                                              y))
                                      v-given-initial-indices
                                      :initial-value 0)))
                        ;; four possible combinations of fixations and contractions.
                        (cond
                          ((and have-fixations (i/= 0 nr-contractions))
                           ;; first fix, then contract.
                           (let* ((nr-indices-after-fixations (i- nr-initial-indices nr-fixations))
                                  (v-indices-after-fixations (make-array nr-indices-after-fixations))
                                  (nonfixed-index-pos 0))
                             (dotimes (j nr-initial-indices)
                               (declare (fixnum j))
                               (let ((idx-j (aref v-given-initial-indices j)))
                                 (if (not (and (consp idx-j) (eq (car idx-j) :fix)))
                                     (progn
                                       (setf (aref v-indices-after-fixations nonfixed-index-pos) idx-j)
                                       (incf nonfixed-index-pos)))))
                             (let* ((fixated (sp-permute-fix-indices tensor (v-int-range nr-indices-after-fixations)))
                                    (contraction-permutation
                                     (map '(simple-array * (*))
                                          #'(lambda (idx)
                                              (position idx final-indices :test #'equalp))
                                          v-indices-after-fixations))
                                    (contracted (apply #'_sp-contract-tensor
                                                       `(,fixated ,contraction-permutation ,@li-pairs))))
                               contracted)))
                          ((i= 0 nr-contractions)
                           ;; (possibly) fixations, but no contractions.
                           (let ((indices-old-to-new
                                  (map '(simple-array * (*))
                                       #'(lambda (idx)
                                           (if (and (consp idx) (eq (car idx) :fix))
                                               idx
                                               (position idx final-indices :test #'equalp)))
                                       v-given-initial-indices)))
                             ;; check for special case of trivial permutation. In that case, do nothing at all.
                             (labels
                                 ((is-identity-permutation (v pos)
                                    (declare (type (simple-array fixnum (*)) v) (fixnum pos))
                                    (cond ((i< pos 0) t)
                                          ((i= pos (aref v pos))
                                           (is-identity-permutation v (i1- pos)))
                                          (t nil))))
                               (if (is-identity-permutation indices-old-to-new
                                                            (array-dimension indices-old-to-new 0))
                                   tensor
                                   (sp-permute-fix-indices tensor indices-old-to-new)))))
                          (t
                           ;; remaining case: contractions, but no fixations.
                           (let ((v-permutation
                                  (map '(simple-array fixnum (*))
                                       #'(lambda (idx) (position idx final-indices :test #'equalp))
                                       li-rest-indices)))
                             (apply #'_sp-contract-tensor `(,tensor ,v-permutation ,@li-pairs))))))))
           )
        ;; ensure index structure is correct.
        ;; (this little overhead should be worth the effort...)
        (let ((buggy-indices (find-buggy-indices (cons (cons :dummy final-indices) tensors+indices))))
          (if buggy-indices
              (error (format nil "sp-x impossible index structure, offending indices: ~S" buggy-indices))))
        (let ((nr-tensors (length tensors+indices)))
          (cond
            ;; The cases of zero and one tensors are special. Zero tensors are simple; one tensor is a bit ugly,
            ;; since it may have fixations, permutations, and contractions.
            ;;
            ((i= 0 nr-tensors);; special case of zero-factor tensor product
             (let ((result (make-sp-array nil :nonsparse t)))
               (sp-set! result 1)
               result))
            ((i= 1 nr-tensors)
             ;; oh dear. Only one tensor. Needs special treatment.
             (contract-fix-reduce-1-tensor (caar tensors+indices) (cdar tensors+indices) final-indices))
            (t
             ;; okay, we have at least two tensors.
             (let ((tensors+indices-selfcontracted-fixed
                    (map '(simple-array cons (*))
                         #'(lambda (t+i)
                             (mv-bind (li-rest-indices nr-remaining-indices nr-contractions li-pairs)
                                      (_remove-contraction-indexpairs (cdr t+i))
                                      (let* ((selfcontracted-fixed-indices
                                              (mapcan #'(lambda (idx) (if (and (consp idx) (eq (car idx) :fix))
                                                                          nil
                                                                          (cons idx nil)))
                                                      li-rest-indices))
                                             (t-selfcontracted-fixed
                                              (contract-fix-reduce-1-tensor (car t+i)
                                                                            (cdr t+i)
                                                                            selfcontracted-fixed-indices)))
                                        (cons t-selfcontracted-fixed selfcontracted-fixed-indices))))
                         tensors+indices)))
               (_sp-x-1 v-tensors+indices-selfcontracted-fixed final-indices)))))))))
  T
  "From :Thomas Fischbacher")


(check-for-bug :cmucl-bugs-format-too-short
  (format nil "~2f" 123456.123)
  "123456."
  "From Sam Steingold:
I would have to agree with Sam that it would appear that the Spec was
fairly clear that LWW is correct and the other implmentations are
buggy.  The relevant passages from the Hyperspec are:

  The full form is ~w,d,k,overflowchar,padcharF. The parameter w is the
  width of the field to be printed; d is the number of digits to print
  after the decimal point; k is a scale factor that defaults to zero.

  ...

  If it is impossible to print the value in the required format in a field
  of width w, then one of two actions is taken. If the parameter
  overflowchar is supplied, then w copies of that parameter are printed
  instead of the scaled value of arg. If the overflowchar parameter is
  omitted, then the scaled value is printed using more than w characters,
  as many more as may be needed.

It would appear that the appropriate to use additional space to get the
number output.")


(check-for-bug :cmucl-bugs-structure-with-named-part-1
  (defstruct (foo-bar-cmucl-bugs-1 (:type list) :named))
  foo-bar-cmucl-bugs-1)

(check-for-bug :cmucl-bugs-structure-with-named-part-2
  (foo-bar-cmucl-bugs-1-p nil)
  nil
  "From Fernando D. Mato Mira")

(check-for-bug :clisp-bugs-flet-part-1
  (defun test12 ()
    (flet ((test12-o ()
             (flet ((test12-i () (return-from test12 nil)))
               (test12-i))))
      (test12-o)))
  test12
  "clisp bug:
http://sf.net/tracker/index.php?func=detail&aid=550864&group_id=1355&atid=101355")

(check-for-bug :clisp-bugs-flet-part-2
  (test12)
  nil)

(check-for-bug :clisp-bugs-flet-part-3
  (compile 'test12)
  test12)

(check-for-bug :clisp-bugs-flet-part-4
  (test12)
  nil)

(makunbound 'test12)

(check-for-bug :clisp-compiled-closure-crash
  (progn
    (defun stem (&key (obj (error "missing OBJ")))
      (with-open-file (stream obj :direction :output)
        (truename stream)))
    (compile 'stem)
    (delete-file (stem :obj "foo-bar-zot"))
    t)
  t
  " a crash compiling sbcl, reported by Christophe Rhodes
 (Corrupted STACK in #<COMPILED-CLOSURE STEM> at byte 45)
 the bug was fixed by bruno in compiler.lisp 1.80")



(check-for-bug :clisp-repeated-keywords-part-1
  (defparameter x 1)
  x)
  
(check-for-bug :clisp-repeated-keywords-part-2
  (defun test-key () (find 1 #(0 1 2 3) :test #'= :test (incf x)))
  test-key)

(check-for-bug :clisp-repeated-keywords-part-3
  (test-key)
  1)

(check-for-bug :clisp-repeated-keywords-part-4
  x
  2)

(check-for-bug :clisp-repeated-keywords-part-5
  (compile 'test-key)
  test-key)

(check-for-bug :clisp-repeated-keywords-part-6
  (test-key)
  1)

(check-for-bug :clisp-repeated-keywords-part-7
  x
  3
" bug in compiled repeated keywords
 fixed by sds in compiler.lisp 1.92")

