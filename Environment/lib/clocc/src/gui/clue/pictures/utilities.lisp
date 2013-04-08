;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "PICTURES")


(defconstant *no-storage-methods* '(font))

(defconstant *symbol-values* '(:stipple :foreground :background))

; - - - - - - - - embedded forms - - - - - - -

(defun handle-embedded-forms (form)
  "Recurses upon finding a make form or special form,
 then executes them when unwinding."
  ;; look inside the form before attempting to execute form
  (when (consp form)
    (dolist (item form form)
      (when (consp item)
	(dolist (i item item) 
	  (handle-embedded-forms i)))		; recurse on consp's
      ;; execute on unwind from recursion
      (cond ((make-form-p item)
	     (nsubstitute (make-graphic  item) item form))
	    ((executable-form-p item)
	     (nsubstitute (apply (car item) (cdr item)) item form)))))
  form)

; - - - - - - - - scanning forms for symbol values - - - - - - -

(defun scan-form (form)
  "Used by restore-graphic to scan form for the presence of particular keywords 
then prompts the user for a symbol to represent the value of the keyword."
  (dolist (f form)
    (cond ((member f *symbol-values*)
	   (let ((elm (find f form)) pos s)
	     (when elm (setq pos (position f form))
	       ;; If value saved with keyword is a number,
	       ;;string or a symbol that is
	       ;; already defined, use it - otherwise prompt the user for a value.
		   (let ((value (nth (1+ pos) form)))
		     (unless (or (null value) 
				 (numberp value)
				 (stringp value)
				 (and (not (null value)) 
				      (symbolp value)
				      (not (keywordp value))
				      (boundp value)
				      (symbol-value value)))
		       (setf s (get-defined-symbol-for elm form))
		       (nsubstitute s (nth (1+ pos) form) form))))))
	  ((consp f) (scan-form f))))
  form)



#+LispM (import '(zl:beep))
#+Lucid (import '(lucid:beep))
#-(or LispM Lucid)
  (defun beep () (write-char (code-char 7) *terminal-io*))

(defun prompt-user-for (stream format-string &rest format-args)
  "Prompts the user via *query-io* for a symbol."
  (let (form)
    (beep)
    (funcall stream :fresh-line)
    (apply #'format stream format-string format-args)
    (funcall stream :fresh-line)
    (setq form (read stream))))

(defun get-defined-symbol-for (elm form)
  (let (ok s)
    (loop until ok do
	  (setf s (prompt-user-for *query-io* 
				   "Please enter a value for :~a in ~a" 
				   elm form))
	  (cond ((or (numberp s)(stringp s)) (setf ok t))
		((and (not (null s)) 
			(symbolp s)
			(not (keywordp s))
			(boundp s)
		        (symbol-value s))
		   (setf s (symbol-value s)) (setf ok t)))
	  )
    s))




