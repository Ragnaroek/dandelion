;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: datafun.lisp,v 2.2 2006/01/13 14:35:26 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(datafun
	     datafun-table datafun-alist datafun-from-plist
	     attach-datafun datafun-on-plist)))

;;; (DATAFUN master sym def) defines a new procedure and associates
;;; it with sym under the indicator master.
;;; master is used by some function (often with the name master)
;;; for data-driven hacks.  def is an ordinary function definition,
;;; with the name omitted, or replaced by the placeholder :^ .  
;;; E.g., (DATAFUN PRINTMACRO COND (DEFUN ...)).
;;; If the def is just another symbol, then that means that the
;;; two symbols behave equivalently.
;;; If it is of the form (FUNCTION name), then that function is
;;; used.
;;; Some of the complexity of this machinery is wasted, but we'll
;;; leave it as is in case we ever need it.

(defmacro datafun (master sym def)
   (let (funame)
;;;;      `(eval-when (:compile-toplevel :load-toplevel :execute
;;;;		   :slurp-toplevel)  ... )
       (cond ((atom def)
	      (setf funame (build-symbol (< def) - (< master)))
	      `(declare-datafun ',funame ',master ',sym nil))
	     ((memq (car def) '(function funktion))
	      `(declare-datafun ',(cadr def) ',master ',sym nil))
	     (t
	      (setf funame (build-symbol (< sym) - (< master)))
	      (let ((definer (car def))
		    (definiens
		       (cond ((eq (cadr def) ':^)
			      (cddr def))
			     (t (cdr def)))))
	      `(progn
		  (,definer ,funame ,@definiens)
		  (declare-datafun ',funame ',master
				   ',sym t)))))))

(defvar datafun-attachers* (make-eq-hash-table :size 100))

(setf (table-entry datafun-attachers* 'attach-datafun)
      (\\ (_ sym funame)
	 (setf (table-entry datafun-attachers* sym)
	       (symbol-function funame))))      

;;; The DATAFUN property of a function name is a list of the form
;;; (master sym1 sym2 ...)
;;; This specifies that this function is to be used for the given
;;;master and symbols.  sym1, if non-(), is the "main symbol"--
;;; funame= (build-symbol (:< sym1) - (:< master)).

(defun declare-datafun (funame master sym main)
   (let ((spec (get funame 'datafun)))
      (cond ((null spec)
	     (setf spec (list master nil))
	     (setf (get funame 'datafun) spec)))
      (cond (main (setf (cadr spec) sym))
	    (t
	     (setf (cddr spec) (adjoin sym (cddr spec)))))
      (let ((attachfn (table-entry datafun-attachers* master)))
	 (cond (attachfn
		(funcall attachfn master sym funame))
	       (t
		(cerror
		   "I will place the function name on the property list of the symbol"
		   "No attach function for symbol ~s and task ~s~%"
		   sym  master)
		(setf (get sym master) (symbol-function funame)))))))

(defmacro datafun-table (name ind &key (size 100))
  `(eval-when  (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
      (defvar ,name (make-hash-table :size ',size :test #'eq))
      (datafun attach-datafun ,ind
         (defun :^ (ind sym fname)
	    (ignore ind)
            (setf (table-entry ,name sym) (symbol-function fname))))))

(defmacro datafun-alist (name ind)
  `(eval-when  (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
      (defvar ,name !((Tup Symbol Obj)))
      (datafun attach-datafun ,ind
         (defun :^ (ind sym fname)
	    (ignore ind)
            (setf (alref ,name sym) (symbol-function fname))))))

(defmacro datafun-from-plist (ind)
   `(eval-when  (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
       (datafun attach-datafun ,ind #'datafun-on-plist)))

(defun datafun-on-plist (ind sym fname)
   (setf (get sym ind) (symbol-function fname)))

;;;;      (setf (table-entry datafun-attachers* ',ind)
;;;;	    (\\ (_ sym funame)
;;;;	       (setf (table-entry ,name sym) 


