;;; stuff for working with sorted (and not so sorted) sequences
;;;
;;; Copyright (C) 1997-2003 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: sorted.lisp,v 1.10 2005/01/27 23:02:46 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/sorted.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `print-seqs', `comma'
  (require :cllib-tilsla (translate-logical-pathname "cllib:tilsla"))
  ;; `percent-change'
  (require :cllib-math (translate-logical-pathname "cllib:math")))

(in-package :cllib)

(export '(map-sorted reduce-sorted sorted-map delete-duplicate-entries
          multi-union top-bottom top-bottom-ui top-bottom-fl
          binary-pos binary-member))

;;;
;;; {{{ top/bottom
;;;

(defmacro plunge-into (new newk lst len maxlen kend pred key)
  "Put NEW into LST. Compare with PRED.
Modify  LST, KEND (the key of the last item in LST)
and LEN - the length of LST.
If MAXLEN is nil, LST is not a list, but the record itself.
NEW and NEWK are eval's once; others - more."
  `(if ,maxlen
    (when (or (null ,kend) (funcall ,pred ,newk ,kend))
      (setq ,lst (merge 'list ,lst (list ,new) ,pred :key ,key) ,len (1+ ,len))
      (when (> ,len ,maxlen)
        (setf (cdr (last ,lst 2)) nil)
        (setq ,len (1- ,len) ,kend (funcall ,key (car (last ,lst))))))
    (when (or (null ,kend) (funcall ,pred ,newk ,kend))
      (setq ,kend ,newk ,lst ,new))))

(defmacro plunge-into-idx (new newk lst len maxlen kend pred key idx nkey)
  "Like plunge-into: put NEW into LST. Compare with PRED.
Modify  LST, KEND (the key of the last item in LST)
and LEN - the length of LST.
If MAXLEN is nil, LST is not a list, but the record itself.
NEW and NEWK are eval's once; others - more.
The resulting list will consist of conses of elements
and corresponding IDXes. NKEY must be equal to (compose 'key car)."
  `(if ,maxlen
    (when (or (null ,kend) (funcall ,pred ,newk ,kend))
      (setq ,lst (merge 'list ,lst (list (cons ,new ,idx)) ,pred :key ,nkey)
            ,len (1+ ,len))
      (when (> ,len ,maxlen)
        (setf (cdr (last ,lst 2)) nil)
        (setq ,len (1- ,len) ,kend (funcall ,key (caar (last ,lst))))))
    (when (or (null ,kend) (funcall ,pred ,newk ,kend))
      (setq ,kend ,newk ,lst (cons ,new ,idx)))))

(defun top-bottom (seq maxn minn mk-idx
                   &key (key #'value) (lessp #'<) (morep #'>))
  "Return MAXN elements from the top of SEQ and MINN from the bottom.
Get the :key and compare it using :lessp and :morep.
MAXN and MINN can be nil, in which case only one top/bottom element is
returned, *not* as a list but just itself.
If MK-IDX is non-nil, return indexes too.
2 values are returned."
  (declare (sequence seq) (type (or null fixnum) maxn minn)
           (type (function (t) t) key) (type (function (t t) t) lessp morep))
  (let (maxls minls kk ktb kbt (lmin 0) (lmax 0) (idx 0)
        (newk (compose 'key car)))
    (declare (type index-t lmin lmax idx))
    (map nil
         (if mk-idx
             (lambda (it)
               (setq kk (funcall key it))
               (plunge-into-idx it kk maxls lmax maxn kbt morep key idx newk)
               (plunge-into-idx it kk minls lmin minn ktb lessp key idx newk)
               (incf idx))
             (lambda (it)
               (setq kk (funcall key it))
               (plunge-into it kk maxls lmax maxn kbt morep key)
               (plunge-into it kk minls lmin minn ktb lessp key)
               (incf idx)))
         seq)
    (values maxls minls)))

;;;###autoload
(defun top-bottom-ui (seq maxn minn idx &key (key #'value) (lessp #'<)
                      (morep #'>) (out *standard-output*)
                      (label #'identity) (klabel #'identity))
  "Call top-bottom and pretty print the results.
Returns them for possible further processing."
  (declare (sequence seq) (type (function (t) t) key label klabel)
           (type (or null fixnum) maxn minn) (stream out))
  (multiple-value-bind (top bot)
      (top-bottom seq maxn minn idx :key key :lessp lessp :morep morep)
    (let ((lfmt (formatter "~3d: ~a~25t==> ~a~%"))
          (lifmt (formatter "~3d: [~:d]~15t~a~50t==> ~a~%"))
          (sfmt (formatter "~5t~a~50t==> ~a~%"))
          (sifmt (formatter "~5t[~:d] ~a~50t==> ~a~%")))
      (format out "Top/Bottom: ~:[vector~;list~]: ~:d records.~%Top (~a):~%"
              (listp seq) (length seq) (or maxn "the only"))
      (if maxn
          (if idx
              (print-seqs out lifmt (mapcar #'cdr top)
                          (mapcar (compose 'label car) top)
                          (mapcar (compose 'klabel 'key car) top))
              (print-seqs out lfmt (mapcar label top)
                          (mapcar (compose 'klabel 'key) top)))
          (if idx
              (format out sifmt (cdr top) (funcall label (car top))
                      (funcall klabel (funcall key (car top))))
              (format out sfmt (funcall label top)
                      (funcall klabel (funcall key top)))))
      (format out "~%Bottom (~a):~%" (or minn "the only"))
      (if minn
          (if idx
              (print-seqs out lifmt (mapcar #'cdr bot)
                          (mapcar (compose 'label car) bot)
                          (mapcar (compose 'klabel 'key car) bot))
              (print-seqs out lfmt (mapcar label bot)
                          (mapcar (compose 'klabel 'key) bot)))
          (if idx
              (format out sifmt (cdr bot) (funcall label (car bot))
                      (funcall klabel (funcall key (car bot))))
              (format out sfmt (funcall label bot)
                      (funcall klabel (funcall key bot)))))
      (values top bot))))

(defun top-bottom-fl (ls &key (val #'value) (label #'identity)
                      (out *standard-output*))
  "Print top/bottom/first/last information about the list."
  (declare (list ls) (type (function (t) double-float) val)
           (type (function (t) t) label) (stream out))
  (multiple-value-bind (to bo)
      (top-bottom ls nil nil nil :key val)
    (let* ((fi (car ls)) (ls (car (last ls))) (v0 (funcall val fi))
           (vt (funcall val to)) (vb (funcall val bo)) (vl (funcall val ls)))
      (format out
              "Start~10t~a --> ~2,8:/comma/~%Bottom~10t~a --> ~2,8:/comma/  ~
 [~5,2f%]~%Top~10t~a --> ~2,8:/comma/  [~5,2f%]~%End~10t~a --> ~2,8:/comma/  ~
 [~5,2f%]~%" (funcall label fi) v0 (funcall label bo) vb (percent-change v0 vb)
 (funcall label to) vt (percent-change v0 vt) (funcall label ls) vl
 (percent-change v0 vl)))))


;;;
;;; }}}{{{ sorted
;;;

(defmacro process-and-shift (pred akey ckey t0 e0 k0 t1 e1 k1)
  "Used in *-sorted."
  `(cond ((or (null k1) (and k0 (funcall ,pred ,k0 ,k1)))
          (multiple-value-prog1 (values (funcall ,akey ,e0) nil)
            (setq ,t0 (cdr ,t0) ,e0 (car ,t0)
                  ,k0 (and ,t0 (funcall ,ckey ,e0)))))
    ((or (null k0) (and k1 (funcall ,pred ,k1 ,k0)))
     (multiple-value-prog1 (values nil (funcall ,akey ,e1))
       (setq ,t1 (cdr ,t1) ,e1 (car ,t1) ,k1 (and ,t1 (funcall ckey ,e1)))))
    (t (multiple-value-prog1 (values (funcall ,akey ,e0) (funcall ,akey ,e1))
         (setq ,t0 (cdr ,t0) ,e0 (car ,t0) ,k0 (and ,t0 (funcall ,ckey ,e0))
               ,t1 (cdr ,t1) ,e1 (car ,t1)
               ,k1 (and ,t1 (funcall ,ckey ,e1)))))))

(defun map-sorted (type func pred l0 l1
                   &key (ckey #'identity) (akey #'identity))
  "Operate on two sorted lists. Call FUNC on the elements of the lists
that are `same' according to PRED. If TYPE is 'LIST, return the list
of whatever FUNC returns."
  (declare (function func pred ckey akey) (list l0 l1) (symbol type))
  (do ((t0 l0) (t1 l1) (e0 (car l0)) (e1 (car l1)) el res
       (k0 (and l0 (funcall ckey (car l0))))
       (k1 (and l1 (funcall ckey (car l1)))))
      ((and (null t0) (null t1)) (nreverse res))
    (setq el (multiple-value-call func
               (process-and-shift pred akey ckey t0 e0 k0 t1 e1 k1)))
    (when type (push el res))))

(defun reduce-sorted (rfunc func2 pred l0 l1
                      &key (ckey #'identity) (akey #'identity) initial-value)
  "Reduce a pair of sorted sequences."
  (declare (function rfunc func2 pred ckey akey) (list l0 l1))
  (let ((res initial-value) (t0 l0) (t1 l1) (e0 (car l0)) (e1 (car l1))
        (k0 (and l0 (funcall ckey (car l0))))
        (k1 (and l1 (funcall ckey (car l1)))))
    (unless res
      (setq res
            (if (or l0 l1)
                (multiple-value-call func2
                  (process-and-shift pred akey ckey t0 e0 k0 t1 e1 k1))
                (funcall rfunc))))
    (do () ((and (null t0) (null t1)) res)
      (setq res (funcall rfunc res
                         (multiple-value-call func2
                           (process-and-shift pred akey ckey
                                              t0 e0 k0 t1 e1 k1)))))))

(defun sorted-map (type func pred missing ckey akey &rest lists)
  "Operate on the corresponding elements of the sorted lists.  Each list
in LISTS is assumed to be sorted according to the predicate PRED applied
to keys CKEY.  Apply function FUNC to the AKEYs of the elements of the
lists with the same CKEYs.  When a list doesn't have an element with the
particular CKEY, function gets nil (if MISSING is nil) or the previous
AKEY (if MISSING is non-nil).
CKEY and AKEY values of nil are the same as #'identity.
  (sorted-map type func pred missing ckey akey &rest lists)"
  (declare (function func pred) (symbol type) (list lists)
           (type (or function null) ckey akey))
  (do ((sec (copy-list lists)) (akeys (make-list (length lists))) begck ck
       (ckey (or ckey #'identity)) (akey (or akey #'identity)) fnn res)
      ((every #'null sec) (nreverse res))
    #-cmu (declare (type (function (t) t) ckey akey))
    ;; get the current ckey
    (setq fnn (member nil sec :test-not #'eq)
          begck (funcall ckey (caar fnn)))
    (dolist (ls (rest fnn))
      (when ls (setq ck (funcall ckey (car ls)))
            (when (funcall pred ck begck) (setq begck ck))))
    ;; shift and operate
    (mapl (lambda (ls ak)
            (cond ((and (car ls)
                        (not (funcall pred begck (funcall ckey (caar ls)))))
                   (setf (car ak) (funcall akey (caar ls)))
                   (pop (car ls)))
                  (t (if missing nil (setf (car ak) nil)))))
          sec akeys)
    (cond ((eq type 'list) (push (apply func akeys) res))
          (t (apply func akeys)))))

(defun delete-duplicate-entries (lst &key (key #'identity) (test #'eql)
                                 keep-first)
  "Like `delete-duplicates', but assumes that the list LST is ordered.
Keeps the last entry, or the first if KEEP-FIRST non-nil."
  (declare (list lst) (type (function (t) t) key)
           (type (function (t t) t) test))
  (do ((ls lst) (kk (and (car lst) (funcall key (car lst))) k1) k1)
      ((endp (cdr ls)) lst)
    (if (funcall test kk (setq k1 (funcall key (second ls))))
        (setf (car ls) (if keep-first (car ls) (cadr ls)) (cdr ls) (cddr ls))
        (setq ls (cdr ls)))))

(defun multi-union (pred ckey &rest lists)
  "Return the union of multisets.
Each of LISTS is a multiset - an alist of (ELEMENT . MULTIPLE).
The lists are ordered according to PRED applied to CKEY of ELEMENT."
  (declare (function pred) (type (or function null) ckey))
  (apply #'sorted-map 'list
         (lambda (&rest args)
           (let* ((tail (member-if-not #'null args))
                  (ret (cons (caar tail) (cdar tail))))
             (dolist (co (cdr tail) ret)
               (when co (incf (cdr ret) (cdr co))))))
         pred nil (if ckey (compose 'ckey car) #'car) nil lists))

;;;
;;; }}}{{{ binary search
;;;

(defun binary-pos (el vect &key (test #'<) (key #'identity) (start 0)
                   (end (1- (length vect))))
  "Binary search of an element in a vector."
  (declare (type simple-vector vect) (type index-t start end))
  (when (or (< end start) (zerop (length vect)))
    (return-from binary-pos nil))
  (let ((ks (funcall key (svref vect start)))
        (ke (funcall key (svref vect end))))
    (when (funcall test el ks) (return-from binary-pos nil))
    (unless (funcall test ks el) (return-from binary-pos start))
    (when (funcall test ke el) (return-from binary-pos nil))
    (unless (funcall test el ke) (return-from binary-pos end))
    (when (> 2 (- end start)) (return-from binary-pos nil))
    (let* ((mid (ash (+ start end) -1)) (km (funcall key (svref vect mid))))
      (cond ((funcall test km el)
             (binary-pos el vect :test test :key key
                         :start mid :end (1- end)))
            ((funcall test el km)
             (binary-pos el vect :test test :key key
                         :start (1+ start) :end mid))
            (mid)))))

(defun binary-member (el ls &key (key #'identity) (test #'<))
  "Binary search of an element in a list.
Like member, but orders of magnitude faster when test and key
are expensive compared with `nthcdr'."
  (declare (list ls) (type (function (t) t) key)
           (type (function (t t) t) test))
  (unless ls (return-from binary-member ls))
  (when (funcall test el (funcall key (car ls)))
    (return-from binary-member ls))
  (unless (funcall test el (funcall key (car (last ls))))
    (return-from binary-member nil))
  ;; EL is between BEG and BEG+LEN
  (do ((beg ls) (len (length ls)) mid km)
      ((<= len 1) (member el beg :key key :test test))
    (declare (type index-t len))
    (setq len (ash len -1) mid (nthcdr len beg)
          km (funcall key (car mid)))
    (unless (funcall test el km) (setq beg mid))))

;;; }}}

(provide :cllib-sorted)
;;; file sorted.lisp ends here
