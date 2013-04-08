;;; simple operations
;;;
;;; Copyright (C) 2000-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: simple.lisp,v 1.15 2005/01/28 16:55:26 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/simple.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(export '(ppprint-list nsublist fix-list to-list from-list zero-len-p paste
          lexicographic-comparison ensure below-p linear
          count-all find-duplicates
          skip-to-new flatten with-collect filter list-length-dotted set=))

;;;
;;; {{{ `with-collect'
;;;

(defmacro with-collect ((&rest collectors) &body forms)
  "Evaluate forms, collecting objects into lists.
Within the FORMS, you can use local macros listed among collectors,
they are returned as multiple values.
E.g., (with-collect (c1 c2) (dotimes (i 10) (if (oddp i) (c1 i) (c2 i))))
 ==> (1 3 5 7 9); (0 2 4 6 8) [2 values]
In CLISP, push/nreverse is about 1.25 times as fast as pushing into the
tail, so this macro uses push/nreverse on CLISP and push into the tail
on other lisps (which is 1.5-2 times as fast as push/nreverse there)."
  #+clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors)))
    `(let (,@ret)
      (declare (list ,@ret))
      (macrolet ,(mapcar (lambda (co re) `(,co (form) `(push ,form ,',re)))
                         collectors ret)
        ,@forms
        (values ,@(mapcar (lambda (re) `(sys::list-nreverse ,re)) ret)))))
  #-clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors))
        (tail (mapcar (lambda (cc) (gensym (format nil "~s-TAIL-" cc)))
                      collectors))
        (tmp (mapcar (lambda (cc) (gensym (format nil "~s-TMP-" cc)))
                     collectors)))
    `(let (,@ret ,@tail)
      (declare (list ,@ret ,@tail))
      (macrolet ,(mapcar (lambda (co re ta tm)
                           `(,co (form)
                             `(let ((,',tm (list ,form)))
                               (if ,',re (setf (cdr ,',ta) (setf ,',ta ,',tm))
                                   (setf ,',re (setf ,',ta ,',tm))))))
                         collectors ret tail tmp)
        ,@forms
        (values ,@ret)))))

(defun filter (lst test collect &key (key #'identity))
  "COLLECT those elements of LST which satisfy TEST.
AKA `remove-if-not':
 (filter lst test collect :key key) ==
 (let ((res (remove-if-not test lst :key key)))
   (map-into res (compose collect key) res))"
  (declare (list lst) (type (function (t) t) test collect key))
  (with-collect (coll)
    (dolist (el lst)
      (let ((kk (funcall key el)))
        (when (funcall test kk) (coll (funcall collect kk)))))))

;;;
;;; }}}{{{ lexicographic
;;;

(defmacro lexicographic-comparison (functions &key (eq '=) (gt '>) (ge '>=))
  "create a function comparing its two arguments in the lexicographical order
determined by the FUNCTIONS"
  (labels ((l (fl)
             (let ((first (first fl)) (rest (rest fl)))
               (setq first (if (symbolp first) `(,first) `(funcall ,first)))
               (if rest
                   (let ((x1 (gensym "X")) (y1 (gensym "Y")))
                     `(let ((,x1 (,@first x)) (,y1 (,@first y)))
                        (or (,gt ,x1 ,y1)
                            (and (,eq ,x1 ,y1)
                                 ,(l rest)))))
                   `(,ge (,@first x) (,@first y))))))
    `(lambda (x y) ,(l functions))))

;;;
;;; }}}{{{ misc
;;;

(defmacro ensure (form default)
  "Check whether FORM (which should be a single-value place)
evaluates to non-NIL and set it to DEFAULT otherwise.
DEFAULT is only evaluated when FORM is NIL."
  (let* ((vars (mapcar (lambda (expr)
                         (declare (ignore expr))
                         (gensym "ENSURE-"))
                       (cdr form)))
         (new-form (cons (car form) vars)))
    `(let* ,(mapcar #'list vars (cdr form))
       (or ,new-form (setf ,new-form ,default)))))

(defsubst below-p (x0 y0 x1 y1 x2 y2)
  "Check whether (x0 y0) is below the line (x1 y1) -- (x2 y2)."
  (< y0 (/ (+ (* y1 (- x2 x0)) (* y2 (- x0 x1))) (- x2 x1))))

(defsubst linear (x0 y0 x1 y1 tt)
  "Compute the linear function through (x0 y0) and (x1 y1) at tt."
  (/ (+ (* y0 (- x1 tt)) (* y1 (- tt x0))) (- x1 x0)))

(defun ppprint-list (lst &optional (stream t))
  "Print a long list nicely."
  (declare (list lst))
  (format stream "[~a <~:d> ~a]" (car lst) (length lst) (car (last lst))))

(defun nsublist (lst &optional pos0 pos1)
  "Return the part of the list between pos0 and pos1, *destructively*.
The indexing starts from 0, so (nsublist '(1 2 3 4 5) nil 2) ==> (1 2 3)."
  (declare (list lst))
  (when pos1 (let ((cut (nthcdr pos1 lst))) (when cut (setf (cdr cut) nil))))
  (if pos0 (nthcdr pos0 lst) lst))

(defun fix-list (ls)
  "Turn (aa bb . cc) into (aa bb cc)."
  (let ((ll (last ls)))
    (when (cdr ll) (setf (cdr ll) (cons (cdr ll) nil)))) ls)

(defsubst to-list (zz)
  "If ZZ is a list, return ZZ, otherwise return (list ZZ)."
  (if (listp zz) zz (list zz)))

(defsubst from-list (zz)
  "If ZZ is a list, return (car ZZ), otherwise return ZZ."
  (if (listp zz) (car zz) zz))

(defun flatten (ll)
  "atom -> (atom); (1 (2) (3 (4) (5 (6) 7) 8) 9) -> (1 2 3 4 5 6 7 8 9)"
  (labels ((fl (ll acc)
             (cond ((null ll) acc)
                   ((atom ll) (cons ll acc))
                   (t (fl (car ll) (fl (cdr ll) acc))))))
    (fl ll nil)))

(defun zero-len-p (seq)
  "Returns T iff the sequence has zero length.
Works in constant time even with lists."
  (declare (sequence seq))
  (or (null seq) (and (vectorp seq) (zerop (length seq)))))

(defsubst paste (new ls)
  "Like `push', but do not modify LS."
  (declare (cons ls))
  (setf (cdr ls) (cons (car ls) (cdr ls)) (car ls) new) ls)

(defun skip-to-new (lst &key (test #'eql) (key #'value))
  "Return the tail of the list LST with the KEY different by TEST
from the previous one."
  (declare (list lst) (type (function (t t) t) test)
           (type (function (t) t) key))
  (do ((ll lst (cdr ll)) (k0 (funcall key (first lst)) k1) k1)
      ((or (null (cdr lst))
           (not (funcall test k0 (setq k1 (funcall key (second ll))))))
       ll)))

(defun list-length-dotted (list)
  "Return the length of the list or nil if it is circular.
The second value is the last atom (i.e., `dotted-p')."
  (do ((nn 0 (+ nn 2))
       (fast list (cddr fast))
       (slow list (cdr slow)))
      (nil)
    (declare (type (integer 0) nn))
    (when (atom fast) (return (values nn fast)))
    (when (atom (cdr fast)) (return (values (1+ nn) (cdr fast))))
    (when (eq (cdr fast) slow) (return nil))))

(defun set= (set1 set2 &rest rest &key key test test-not)
  "Check whether two sets are the same."
  (declare (ignore key test test-not))
  (and (apply #'subsetp set1 set2 rest)
       (apply #'subsetp set2 set1 rest)))

(defun count-all (seq &key (test 'eql) (key #'value) append (weight 1)
                  &aux (ht (or append (make-hash-table :test test))))
  "Return the hash table with counts for values of the sequence."
  (unless weight (setq weight 1))
  (map nil (etypecase weight
             (function
              (lambda (el)
               (incf (gethash (funcall key el) ht 0) (funcall weight el))))
             (number
              (lambda (el) (incf (gethash (funcall key el) ht 0) weight))))
       seq)
  ht)

(defun find-duplicates (seq &key (test 'eql) (key #'value))
  "Find all duplicate elements in the sequence:
call `count-all' and remove elements with count 1."
  (let ((ht (count-all seq :key key :test test)))
    (maphash (lambda (key val) (when (= val 1) (remhash key ht))) ht)
    ht))

;;; }}}

(provide :cllib-simple)
;;; file simple.lisp ends here
