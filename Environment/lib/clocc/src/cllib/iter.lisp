;;; itaration: collecting and multi-dim
;;;
;;; Copyright (C) 1997-2001, 2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: iter.lisp,v 1.10 2005/06/29 18:34:10 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/iter.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `to-list'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `map-vec'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `dot', `approx=-abs', `normalize'
  (require :cllib-math (translate-logical-pathname "cllib:math")))

(in-package :cllib)

(export '(do-iter-ls do-iter optimize-pars))

;;;
;;; {{{ iterate
;;;

(defun vector< (s0 s1 &key (test #'=) (lessp #'<) (key #'identity))
  "Lexicographic comparison of vectors."
  (declare (type (simple-array * (*)) s0 s1)
           (type (function (t t) boolean) test lessp)
           (type (function (t) t) key))
  (loop :for v0 :across s0 :and v1 :across s1
    :for z0 = (funcall key v0) :and z1 = (funcall key v1)
    :unless (funcall test z0 z1) :return (funcall lessp z0 z1)))

(defsubst maj2ind (ls ix &optional (ii (make-list (length ls))))
  "Convert the row-major index IX to the list of indexes.
E.g.: (maj2ind (reverse (array-dimensions ARRAY)) INDEX LIST)"
  (declare (list ls ii) (fixnum ix))
  (do ((ir ii (cdr ir)) (lr ls (cdr lr)))
      ((endp lr) (nreverse ii))
    (multiple-value-bind (xx vv) (floor ix (car lr))
      (setq ix xx)
      (setf (car ir) vv))))

(defmacro do-iter-ls ((ii idx &optional ret) &body body)
  "Iterate over several indexes at once.
E.g.: (do-iter-ls (z (reverse '(2 3))) (princ z))
will print (0 0)(0 1)(0 2)(1 0)(1 1)(1 2)
In the body, II is the *SAME* list of indexes.
IDX is a list of fixnums."
  (declare (list idx) (symbol ii))
  (with-gensyms ("ITERATE-LS-" sz ix ls)
    `(let* ((,ls ,idx) (,sz (reduce #'* ,ls)) (,ii (make-list (length ,ls))))
      (declare (fixnum ,sz) (list ,ls))
      (dotimes (,ix ,sz ,ret)
        (declare (fixnum ,ix))
        (setq ,ii (maj2ind ,ls ,ix ,ii))
        ,@body))))

(defmacro do-iter ((ii idx &optional ret) &body body)
  "Iterate over several indexes at once.
E.g.: (do-iter (z #(2 3)) (princ z))
will print #(0 0)#(0 1)#(0 2)#(1 0)#(1 1)#(1 2)
In the body, II is the *SAME* simple vector of fixnums.
IDX is a simple vector of fixnums."
  (declare (symbol ii))
  (with-gensyms ("ITERATE-" sz ix ls len)
    `(let* ((,ls ,idx) (,sz (reduce #'* ,ls)) (,len (length ,ls))
            (,ii (make-array ,len :element-type 'fixnum)))
      (declare (fixnum ,sz ,len) (type (simple-array fixnum (*)) ,ls ,ii))
      (dotimes (,ix ,sz ,ret)
        (declare (fixnum ,ix))
        (loop :for zz :of-type fixnum :from (- ,len 1) :downto 0
          :with idx :of-type fixnum = ,ix :do
          (setf (values idx (aref ,ii zz)) (floor idx (aref ,ls zz))))
        ,@body))))

;;;
;;; }}}{{{ optimize
;;;

(defun envelope (lst)
  "Return the rectangular envelope of the cloud of points.
Returns the list of the 2^dim points, less if the set was degenerate."
  (let* ((dim (length (car lst))) res
         (freq (make-array dim :initial-element nil)))
    (declare (fixnum dim) (simple-vector freq))
    (dolist (vv lst)
      (declare (type simple-array vv))
      (dotimes (ii dim) (pushnew (aref vv ii) (aref freq ii))))
    (dotimes (ii dim)
      (setf (aref freq ii) (cons (apply #'min (aref freq ii))
                                 (apply #'max (aref freq ii)))))
    (do-iter (ii (mk-arr 'fixnum 2 dim)
                 (delete-duplicates res :test #'equalp))
      (push (map-vec 'double-float dim
                     (lambda (jj el)
                       (declare (fixnum jj) (cons el))
                       (if (zerop jj) (car el) (cdr el)))
                     ii freq) res))))

(defun convex-hull-n-dim (lst)
  "LST - list of n-vectors. Cheating!!!"
  (declare (list lst))
  (let* ((ll (if (> (length lst) (1+ (length (car lst))))
                 (convex-hull-n-dim (cdr lst)) (cdr lst)))
         (new (car lst)) (len (length ll)) (dim (length (car lst)))
         (mid (mk-arr 'double-float 0d0 dim)))
    (declare (fixnum len dim) (type (simple-array double-float (*)) new mid))
    (setq mid (apply #'map-into mid
                     (lambda (&rest nums) (/ (apply #'+ nums) len)) ll))
    (flet ((sub (v0 v1)
             (declare (type (simple-array double-float (*)) v0 v1))
             (map-vec 'double-float dim #'- v0 v1)))
      (declare (ftype (function ((simple-array double-float (*))
                                 (simple-array double-float (*)))
                                (values (simple-array double-float (*))))
                      sub))
      (if (every #'approx=-abs mid new) ll
          (let ((dir (normalize (sub mid new))))
            (declare (type (simple-array double-float (*)) dir))
            (if (every (lambda (vv) (plusp (dot dir (sub vv new)))) ll)
                (cons new ll) ll))))))

(defun optimize-pars (func pars steps nns add &optional (out t) (ni 0) (de 0))
  "Optimize a function of one variable - an array of doubles."
  ;; test case:
  ;; (defun zz (arr) (- 1 (reduce #'+ arr :key (lambda (x) (expt (1- x) 2)))))
  ;; (optimize-pars #'zz #(1.1 1.1 1.1) #(1.1 1.1 1.1) #(2 2 2) nil)
  ;; (optimize-pars #'zz #(1.1 1.1 1.1) #(.1 .1 .1) #(2 2 2) 0.75)
  ;; ==> #(1.0 1.0 1.0) ;1.0 ;152
  ;; (defun zz (arr) (- 1 (reduce #'min arr :key (compose abs 1-))))
  (declare (type (function ((simple-array double-float)) double-float) func)
           (type (simple-array double-float (*)) pars steps)
           (type (simple-array fixnum (*)) nns) (type index-t ni de)
           (type (or null double-float) add))
  (labels ((it (ar)             ; progress report
             (declare (type (simple-array fixnum (*)) ar))
             (let ((prod (reduce #'* ar :key
                                 (lambda (nn)
                                   (declare (type index-t nn))
                                   (1+ (* 2 nn))))))
               (declare (fixnum prod))
               (+ (if (every #'zerop ar) 0
                      (it (map-in (lambda (nn)
                                    (declare (type index-t nn))
                                    (max 0 (1- nn))) ar)))
                  prod))))
    (mesg :opt out
          " *** Optimize: depth: ~d; already called: ~:d; calls left: ~:d
~10tpars:  ~/pr-arr/~%~10tsteps: ~/pr-arr/~%~10tnums:  ~:/pr-arr/~%"
          de ni (1- (it (copy-seq nns))) pars steps nns))
  (let* ((dim (length pars)) rr ma op
         (dims (map-vec 'fixnum dim
                        (lambda (nn) (declare (fixnum nn)) (1+ (* 2 nn))) nns))
         (ndim (map-vec 'fixnum dim
                        (lambda (nn)
                          (declare (fixnum nn)) (max 0 (1- nn))) nns))
         (nsteps (if (every #'zerop ndim) nil
                     (map-vec
                      'double-float dim
                      (if add
                          (lambda (xx) (declare (double-float xx)) (* add xx))
                          (lambda (xx)
                            (declare (double-float xx))
                            (* 0.5d0 (1+ xx)))) steps)))
         (pps (make-array dim :element-type 'double-float))
         (pin (map-vec 'double-float dim
                       (if add
                           (lambda (pp st nn)
                             (declare (double-float pp st) (fixnum nn))
                             (- pp (* st nn)))
                           (lambda (pp st nn)
                             (declare (double-float pp st) (fixnum nn))
                             (/ pp (expt st nn))))
                       pars steps nns)))
    (declare (type (simple-array double-float (*)) pin pps)
             (type (simple-array fixnum (*)) dims ndim)
             (type (integer 1 20) dim))
    (do-iter (ii dims)
      (map-into pps (if add (lambda (pp st nn)
                              (declare (double-float pp st) (fixnum nn))
                              (+ pp (* st nn)))
                        (lambda (pp st nn)
                          (declare (double-float pp st) (fixnum nn))
                          (* pp (expt st nn))))
                pin steps ii)
      (setq rr (funcall func pps))
      (cond ((and ma (= rr ma)) (setq op (cons (copy-seq pps) (to-list op))))
            ((or (null ma) (> rr ma)) (setq ma rr op (copy-seq pps)))))
    (incf ni (reduce #'* dims)) (incf de)
    (if nsteps
        (if (consp op)
            (let (ropt rmax (*print-length* nil) (lop (length op)))
              (declare (type (or null double-float) rmax) (fixnum lop))
              (mesg :opt out " +++ Optimize: ~d optimal points.~%~s~%" lop op)
              (setq op (convex-hull-n-dim op) lop (length op))
              (mesg :opt out " +++ `Convex hull': ~d points.~%~s~%" lop op)
              (when (> lop (ash 1 dim))
                (setq op (envelope op) lop (length op))
                (mesg :opt out " +++ Envelope: ~d points.~%~s~%" lop op))
              (assert (>= (ash 1 dim) lop) (op lop)
                      "Too many points (~d):~%~a~%" lop op)
              (dolist (pp op)
                (multiple-value-bind (op1 ma1 ni1)
                    (optimize-pars func pp nsteps ndim add out ni de)
                  (setq ni ni1)
                  (when (and rmax (= ma1 rmax))
                    (setq ropt (delete-duplicates
                                (nconc (to-list ropt) (to-list op1))
                                :test #'equalp)))
                  (when (or (null rmax) (> ma1 rmax))
                    (setq rmax ma1 ropt op1))))
              (values (if (consp ropt) (sort ropt #'vector<) ropt) rmax ni))
            (optimize-pars func op nsteps ndim add out ni de))
        (values op ma ni))))

;;; }}}

(provide :cllib-iter)
;;; file iter.lisp ends here
