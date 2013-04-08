;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: util.lisp,v 2.4 2006/12/01 17:46:16 airfoyle Exp $

;;; Utilities beyond base.lisp, which this depends on.

(eval-when (:load-toplevel)
   (export '(make-Printable printable-as-string eof* funktion
	     debuggable debuggability* begins-with ends-with
             dir-strings-concat)))

;;; Return the printed representation of 'x', snipped
(defun printed-snip (x n)
   (string-snip (format nil "~a" x) n))

;;; Discard all but the first n (or last -n if n<0) characters of s
(defun string-snip (s n)
   (let ((l (length s)))
      (cond ((< n 0)
	     (cond ((> l (- n))
		    (concatenate 'string "..." (subseq s (+ l n))))
		   (t s)))
	    (t
	     (cond ((> l n)
		    (concatenate 'string (subseq s 0 n) "..."))
		   (t s))))))

(defun slot-is-empty (obj slot)
   (not (slot-truly-filled obj slot)))

(defun slot-truly-filled (ob sl)
   (and (slot-boundp ob sl)
	(slot-value ob sl)))

;;; For debugging --
(defun dutl (ut) (values->list (decode-universal-time ut)))

;;; For debugging (it's traceable)
(defun retain-if-x (pred l)
   (retain-if pred l))

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Useful for constructing "marker" objects that have nothing but their
;;; identity: none of them is EQ to anything encountered in an ordinary
;;; S-expression.
(defstruct (Printable (:print-function
		         (lambda (me str d)
			         (declare (ignore d))
			    (funcall (Printable-printer me) str)))
	              (:constructor make-Printable (printer)))
   printer
   (sym nil))

(defun printable-as-string (s)
   ;; We resort to these strange devices so that this sort of
   ;; Printable is uniquified, but only relative to the current
   ;; package --
   (let ((sym (intern s)))
      (let ((p (make-Printable (\\ (srm) (format srm "~a" s)))))
	 (setf (Printable-sym p) sym)
	 (setf (get sym 'printable) p)
	 p)))

(defmethod make-load-form ((p Printable) &optional env)
                          (declare (ignore env))
   (let ((sym (Printable-sym p)))
      (cond ((and sym (get sym 'printable))
	     `(or (get ',sym 'printable)
		  (printable-as-string ',(symbol-name sym))))
	    (t
	     `(make-Printable ',(Printable-printer p))))))
)

(defvar eof* (printable-as-string "#<End of file>"))
;;;;(make-Printable (\\ (srm) (format srm "~a" "#<End of file>")))


(eval-when (:compile-toplevel :load-toplevel :execute)

(def-excl-dispatch #\' (srm _)
   (list 'funktion (read srm true nil true)))

(defvar syms-used-as-funktions* !())

(defmacro funktion (f)
   (let  ((quoter
             (cond ((and (atom f) (> debuggability* 0))
                    'quote)
                   (t 'function))))
      (cond ((atom f)
             `(progn
                 (eval-when (:load-toplevel :execute)
                    (note-funktion ',f ',quoter))
                 (,quoter ,f))
;;;;             `(let ((exp '(,quoter ,f)))
;;;;                 (on-list-if-new exp syms-used-as-funktions*
;;;;                            :test #'equal)
;;;;                 (,quoter ,f))
             )
            (t 
             `(,quoter ,f)))))
)

(defun note-funktion (fun-sym quoter)
   (let ((e (assq fun-sym syms-used-as-funktions*)))
      (cond ((not e)
             (setq e (tuple fun-sym quoter !()))))
      (cond ((eq quoter 'function)
             (setf (second e) quoter)
             (cond (now-loading*
                    (on-list-if-new now-loading* (third e))))))
      fun-sym))

(defmacro debuggable (n)
   (multiple-value-bind (speed safety space debug)
			(cond ((> n 0)
			       (values 2 3 0 3))
			      ((= n 0)
			       (values 2 1 1 2))
			      (t
			       (values 3 1 2 0)))
      `(eval-when (:compile-toplevel :execute)
	  (declaim (optimize (speed ,speed) (safety ,safety)
			     (space ,space) (debug ,debug)))
	  (setq debuggability* ,n))))

;;; Given a series of strings representing directories, 
;;; concatenates them and makes sure that every one ends with a "/"
;;; (or whatever the directory delimiter is).  That includes
;;; the last one.  Deals with the possibility that there might
;;; already be directory delimiters ending or beginning the strings
;;; by making sure that there is exactly one of them between
;;; two adjacent strings, even if that means deleting some of 
;;; them.
(defun dir-strings-concat (&rest strings)
   (with-output-to-string (str-srm)
      (do ((sl strings (tail sl)))
          ((null sl))
        (let ((s (head sl)))
           ;; 4 tedious cases
           (cond ((ends-with s directory-delimiter*)
                  (cond ((and (not (null (tail sl)))
                              (begins-with (head (tail sl))
                                           directory-delimiter*))
                         (format str-srm
                           "~a"
                           (subseq s 0 (- (length s)
                                          (length directory-delimiter*)))))
                        (t
                         (format str-srm "~a" s))))
                 (t
                  (format str-srm "~a" s)
                  (cond ((or (null (tail sl))
                             (not
                              (begins-with (head (tail sl))
                                           directory-delimiter*)))
                         (format str-srm "~a" directory-delimiter*)))))))))

(defun begins-with (s b)
   (let ((k (mismatch s b)))
      (or (not k)
          (= k (length b)))))

(defun ends-with (s e)
   (let ((k (mismatch s e :from-end true)))
      (or (not k)
          (= (+ k (length e))
             (length s)))))


(defmacro depends-on (&rest _)
   ''"depends-on is unimplemented")

(defun gen-var (sym)
   (build-symbol (:package false) (< sym) - (++ symno*)))

