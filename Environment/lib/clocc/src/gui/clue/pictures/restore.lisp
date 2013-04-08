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


(export '(restore-graphic) 'pictures)

; - - - - - - - - - - - generic functions - - - - - - -	
; - - - - - - - - nested format - - - - - - - -
(defmethod restore-graphic ((format (eql :nested-forms)) &optional (stream *standard-input*))
  (let (graphics-list eof)
    (if (eq stream *standard-input*)
	(let ((form (read stream nil)))
	  (setf form (scan-form form))
	  (when (make-form-p form)
	    (setf graphics-list 
		  (cons (make-graphic form) graphics-list)))
	  graphics-list)
	(loop until eof do
	      (let ((form (read stream nil 'last)))
		(unless (eq form 'last)
		  (setf form (scan-form form)))
		  (when (and form (make-form-p form))
		    (setf graphics-list 
			  (cons (make-graphic form) graphics-list)))
		  (when (eq form 'last)
		    (setf eof t)))
	      finally (return graphics-list)))))

(defun make-graphic (form)
  (eval form)
;; two forms below go with a different save.lisp file
;  (handle-embedded-forms (cdr form))
;  (apply (car form) (cdr form))
  )

