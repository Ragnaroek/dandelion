;;; misc checks
;;;
;;; Copyright (C) 2000-2001 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: check.lisp,v 1.6 2005/01/27 23:02:50 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/check.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `list-format', `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `rel-diff'
  (require :cllib-math (translate-logical-pathname "cllib:math")))

(in-package :cllib)

(export '(check-list-type check-list-values out-of-bounds-p))

(defun check-list-type (lst pred &key (key #'value) (out *standard-output*))
  "Check that all the elements of the list satisfy the predicate.
Like (every lst pred), but prints a message."
  (declare (list lst) (type (function (t) t) key pred) (stream out))
  (let ((err 0) kk)
    (declare (type index-t err))
    (format out "~&Checking list (length: ~d) for type `~a'.~%"
            (length lst) pred)
    (dolist (rec lst)
      (setq kk (funcall key rec))
      (unless (funcall pred kk)
        (format out " *** Record `~a' ~:[~?~;~2*~]fails predicate `~a'.~%"
                rec (eq #'identity key) "[key (~a): `~a'] " (list key kk) pred)
        (incf err)))
    (if (zerop err)
        (format out "No errors.~%")
        (format out "~d records failed the test.~%" err))
    err))

(defcustom *data-change-tolerance* double-float 0.25d0
  "*The default maximum relative change permissible in the data.")

(defun check-list-values (lst &key (tol *data-change-tolerance*)
                          (key #'value) (out *standard-output*) label)
  "Check that the successive values in the list LST are within
TOL (which defaults to *data-change-tolerance*) from each other.
KEY may be a function or a list of functions.
LABEL is the printing name of KEY (or list thereof). If omitted,
KEY is printed with `~a'.
This is more of a UI function. See also `jumps'.
Return T if not errors found."
  (declare (list lst) (double-float tol) (stream out)
           (type (or function symbol list) key))
  (format out "Checking the list for values of~?
for relative change of at least ~5,3f~%"
          (list-format "~a") (to-list (if label label key)) tol)
  (do* ((chkey (lambda (v0 v1) (> (rel-diff v0 v1) tol)))
        (mkkey (if (listp key)
                   (lambda (rec ks)
                     (map-into ks #'(lambda (kk) (declare (function kk))
                                            (funcall kk rec))
                               key))
                   (lambda (rec ks) (declare (ignore ks)) (funcall key rec))))
        (chkeys (if (listp key) (lambda (k0 k1) (some chkey k0 k1)) chkey))
        (ll lst (cdr ll)) (r0 (car lst) r1) r1
        (k0 (funcall mkkey r0 (if (listp key) (make-list (length key)) nil)))
        (k1 (if (consp key) (make-list (length key)) nil))
        (err 0) (ix 0 (1+ ix)))
       ((null (cdr ll))
        (format out "~:d record~:p checked. ~d error~:p found.~%" (1+ ix) err)
        (zerop err))
    (declare (type index-t err ix))
    (setq r1 (second ll) k1 (funcall mkkey r1 k1))
    (when (funcall chkeys k0 k1)
      (format out " *** ~3d *** Error between records ~:d and ~:d:
 --- ~a~% --- ~a~%" (incf err) ix (1+ ix) r0 r1)
      (if (listp key)
          (format out "Relative differences:~?~%"
                  (list-format "~5,3f") (mapcar #'rel-diff k0 k1))
          (format out "Relative difference:~5,3f~%" (rel-diff k0 k1))))
    (rotatef k0 k1)))

(defcustom *big-num* double-float (/ double-float-epsilon)
  "*The big number, used in `out-of-bounds-p'.")

(defun bad-num-p (num &optional (bn *big-num*))
  "Check whether the number NUM is bad."
  (declare (double-float num bn))
  (or (< num (- bn)) (< (- (/ bn)) num 0) (< 0 num (/ bn)) (< bn num)))

(defun out-of-bounds-p (ll &key (key #'value) (bn *big-num*) (out t))
  "Check for outrageous numbers in the list."
  (declare (list ll) (type (function (t) double-float)) (double-float bn))
  (let ((err nil) (bn- (- bn)) (bn1 (/ bn)) (bn1- (- (/ bn))) (val 0d0))
    (declare (double-float bn- bn1 bn1- val))
    (dolist (rec ll err)
      (setq val (funcall key rec))
      (when (or (< val bn-) (< bn1- val 0) (< 0 val bn1) (< bn val))
        (setq err t)
        (mesg :err out " +++ ~a ===> ~f~%" rec val)))))

(provide :cllib-check)
;;; file check.lisp ends here
