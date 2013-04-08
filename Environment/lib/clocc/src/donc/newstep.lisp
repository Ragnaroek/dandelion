;-*- Mode:LISP; Package: user; Base:10; Syntax: Common-lisp -*-

;; I, Donald Cohen (donc@{isi.edu,compsvcs.com,ap5.com} hereby
;; (1) assert that I am the author of this file and
;; (2) place it in the public domain.  Feb. 1, 2000.

#| A common lisp stepper (only steps interpreted code)

Now that step is part of common lisp the only reasons I can think of
to use this are
 - if you like it better than the one that comes with your lisp
 - the fact it allows you to use the SAME stepper in different lisps

... some minimal documentation ...
In order to step a form do something like
(watch '(+ 2 (- 3 4))) - note the form is quoted
-> means the previous thing is about to be evaluated
<- means the previous thing is about to be returned
In each case you type one character to indicate what you want.
The options are listed below in TraceinTable (also printed out
when you type an illegal character).

MaxIndent is the maximum indentation (after which it starts over at the left).
Formprinter and valueprinter are the functions for printing forms and values.
Whenever you're returning from a form !values is the list of values being
 returned and !value is the first one.
Never-step-fns is a list of functions to just evaluate without asking.
*watch-stream* is the stream we interact with - needed in case the code
we want to watch binds *standard-output* ...

Also, printupto is an independently useful program that prints
the first n characters of some object.

Portability: this package requires evalhook which is not in ansi-cl.
As of 2/2000 the current source works in Allegro and in Clisp.
Search for *** below to see what you have to do (if anything)
to make it work in another lisp.

[Now that evalhook is actually gone, perhaps it would be useful to
resurect the tracein code from which this was originally adapted.]

; History:
; converted from INTERLISP on  7-MAY-84 14:21:36
; by Donc on system VAX
; from source file /usr2/donc/utils/TRACEIN 14-MAR-84 10:23:04
; considerably adapted (mostly deletion of evl-fix & co.) since then
;
;; changes to common lisp:
; declarations replaced by proclaim; no &special, &local; dbg->break
; listp ->consp ; tyi ->read-char; /->\ ; evalhook has a new argument
; got rid of explode ; inserted calls to si:binding-interpreter-environment 
; we still need global:errset (which I would now call ignore-errors)
; replace send to get cursorpos with something that computes it
; '92 - move to lucid:
; got rid of errsets, eval -> evalhook, added package
; added never-step-fns (always seem to be doing FUNCTION)
; added peek for newline before printing it (cause of need to type return)
; added clear-input on bad input (cause it doesn't hear until the return)
; got rid of displays for non conses - good or bad?  
;  - no chance to return a different value
; too bad our emacs interface doesn't send blank space - have to type
; blank then form for eval and message cannot appear until after the form
; 2/98 ansi does not actually support evalhook (or control- ?)
; for acl 5.0 import evalhook from excl
; 3/99 do interaction to *watch-stream* which is initially *terminal-io*
; but may be rebound for debugging IO stuff
; 2/2000 remove use of my own pp, move to a separate package

|#

(in-package :user)
(eval-when (eval load compile)
  (when (find-package :watch)
    (cerror "continue"
	    "Package watch already exists!  Continue will redefine it.")))
(defpackage "WATCH" (:use "LISP")
	    (:import-from ;; ***
	     ;; We need evalhook.  If it's in user package for your lisp
	     ;; you can leave this alone, otherwise you have to do something
	     ;; like what we do here for allegro (where the package is cltl1)
	     #+allegro cltl1
	     #-(or allegro) user
	     evalhook *evalhook*)
	    (:export "WATCH" "PRINTUPTO"))
(in-package :watch)
(provide :watch)

(eval-when (compile eval load)
 (proclaim '(special MaxIndent linelength TraceinTable formprinter valueprinter
             env !value !values indent# xpr# stepaction limit ignorelst
             FormToEval never-step-fns *watch-stream*)))

(defvar *watch-stream* *terminal-io*)
(defvar never-step-fns '(function))
(defvar MaxIndent 40)
(defvar linelength 80)
(defvar TraceinTable '((#\  "<space> to eval a form you type")
		       (#\b "break")
		       (#\e "eval form silently")
		       (#\f "finish this Break")
		       (#\p "prettyprint form")
		       (#\r "retry form")
		       (#\s "step On")
		       (#\t "trace Form")
		       (#\v "v to Prettyprint Value")
		       (#\x "x to set the Exit value")))

; nil not neccesarily a legal env ; no way to get a null-env
(defun evalhk (form efn afn env)
  (if env (evalhook form efn afn env) (evalhook form efn afn)))

(defun printupto (Object Limit UsePrinc IgnoreLst file)
  (PrintUpTo* Object UsePrinc nil file))

(defun printupto* (object useprinc tailp file)
  (cond ((zerop limit))
	((not (consp object))
	 (cond
	   (tailp (case limit
		    (1. (princ " " file) (setq limit 0.))
		    (2. (princ " ." file) (setq limit 0.))
		    (t (progn (princ " . " file)
			      (setq limit (- limit 3.))
			      (printupto* object useprinc nil file)))))
	   (t (prog ((string (with-output-to-string (s)
				    (cond (useprinc (princ object s))
					  (t (prin1 object s)))))
		     size)
		    (setq size (length string))
		    (cond ((<= size limit)
			   (princ string file)
			   (setq limit (- limit size)))
			  (t (princ (subseq string 0 limit) file)
			     (setq limit 0.)))))))
	((member (car object) ignorelst)
	 (printupto* (cadr object) useprinc tailp file))
	(t (cond (tailp (princ " " file)) (t (princ "(" file)))
	   (setq limit (- limit 1))
	   (printupto* (car object) useprinc nil file)
	   (and (cdr object) (printupto* (cdr object) useprinc t file))
	   (or tailp (zerop limit)
	       (and (princ ")" file) (setq limit (- limit 1)))))))

(defun traceinfp (form file pos)
  #+debug (princ "*F*" file)
  (printupto form (max 20. (- linelength (+ 20. pos)))
	     nil '(watch-eval #+lispm si:displaced) file))

(defun traceinvp (vals file pos) 
  #+debug (princ "*V*" file)
  (loop for val on vals do
    (progn (printupto (car val) (max 20. (- linelength (+ 20. pos)))
		      nil nil file)
	   (cond ((cdr val) (tab pos))))))
;just for present purposes
(defun terpri* ()
  (if (and (listen *watch-stream*)
           (eql #\newline (peek-char nil *watch-stream*)))
      (progn #+debug (princ "*+*" *watch-stream*) (read-char *watch-stream*))
    (progn #+debug (princ "*-*" *watch-stream*) (terpri *watch-stream*))))
(defun tab (pos)
  (terpri*)
  (loop for i from 2 to pos do (write-char #\  *watch-stream*)))
(defun askuser* (wait default mess keylist &rest ignore)
   (declare (ignore wait default ignore))
   (prog (char)
     lp  (princ mess *watch-stream*)
         (finish-output *watch-stream*)
	 (cond ((assoc (setq char (read-char *watch-stream*))
	               keylist)
	        (return char))
	       ((member char '(#\newline #\return #\linefeed
			       #.(code-char 10) #+ignore #\control-J))
		(go lp))
	       (t (print char *watch-stream*) ;; so you know what you typed
		  ; so you can add it to the case above
		  (loop for k in keylist do
                        (terpri*) (princ (cadr k) *watch-stream*))
                  (finish-output *watch-stream*)
		  (clear-input *watch-stream*) (go lp)))))

(defun watch-evalhook (xpr# &optional env)
  (cond
     ((boundp 'formtoeval)
	(prog (!value !values (indent# (+ indent# 2.)))
	   (makunbound '!value)
	   (cond ((or (not (consp xpr#)) (member (car xpr#) never-step-fns))
		  (setq !value
			(car (setq !values
				   (multiple-value-list
				    (evalhk xpr# nil nil env)))))
		  (return (values-list !values)))
		 ((eq stepaction 'eval)
		  (setq !value
			(car (setq !values
				   (multiple-value-list
				     (evalhk xpr# 'watch-evalhook nil env)))))
		  (setq *evalhook* nil)
		  (return (values-list !values))))
	l0 (cond ((> indent# maxindent) (setq indent# 1.)))
	   (tab indent#)
	   (funcall formprinter xpr# *watch-stream* indent#)
          ;; above was *standard-output*
	   #+ignore(or (not (consp xpr#)) (null stepaction) (terpri*))
	l1 (cond
	      ((and (null stepaction) (consp xpr#))
		 (case
		    (askuser* nil nil (cond ((boundp '!value) "<-") (t "->"))
			      traceintable t nil nil t)
		    (#\  (terpri*)
		     (princ "eval: " *watch-stream*)
                     (finish-output *watch-stream*)
		     (print (evalhk (read *watch-stream*) nil nil env)
                            *watch-stream*)
                     (finish-output *watch-stream*)
		     (go l1))
		    (#\b (break) (go l0))
		    (#\e
		     (cond
		       ((not (boundp '!value))
			(setq !value
			      (car (setq !values
					 (multiple-value-list
					  (evalhk xpr# nil nil env))))))
		       (t (princ "Value already exists - do R first"
                                 *watch-stream*))))
		    (#\f
		     (setq stepaction 'eval)
		     (and (not (boundp '!value))
			  (setq !value 
				(car (setq !values
					   (multiple-value-list
					    (evalhk xpr# nil nil env)))))))
		    (#\p (terpri*) (write xpr# :stream *watch-stream* :pretty t)
			 (go l1))
		    (#\r (makunbound '!value) (go l0))
		    (#\s
		     (cond
		       ((boundp '!value) (go l2))
		       (t (setq !value
				(car (setq !values
					   (multiple-value-list
					     (evalhk xpr# #'watch-evalhook
						       nil env)))))
			  (setq *evalhook* nil))))
		    (#\t
		     (cond
		       ((not (boundp '!value))
			((lambda (stepaction)
			   (setq !value
				 (car (setq !values
					    (multiple-value-list
					       (evalhk xpr# #'watch-evalhook
							 nil env)))))
			   (setq *evalhook* nil))
			 t))
		       (t (princ "Value already exists - do R first"
                                 *watch-stream*))))
		    (#\v
		     (terpri*)
		     (cond ((boundp '!value)
			    (loop for val in !values do
				  (progn (terpri*)
                                         (write val :stream *watch-stream*
						:pretty t))))
			   (t (princ "no value yet" *watch-stream*)))
		     (go l1))
		    (#\x
		     (terpri*)
		     (princ "set exit value(s) to value of: " *watch-stream*)
		     (setq !value
			   (car (setq !values
				      (multiple-value-list
				       (evalhk (read *watch-stream*)
                                               nil nil env))))))
		    (t (error ""))))
	      (t (setq !value
		       (car (setq !values
				  (multiple-value-list
				    (evalhk xpr# #'watch-evalhook nil env)))))
		 (setq *evalhook* nil)))
	   (or (not (consp xpr#)) (tab indent#))
	   (princ " = " *watch-stream*)
	   (funcall valueprinter !values *watch-stream* ;; *standard-output*
		    (+ indent# 3)) ; ignore the width of the atomic form
	   (or stepaction (not (consp xpr#)) (go l1))
	l2 #+ignore(and stepaction (terpri*))
	   (or (eq stepaction 'eval)
	       (setq *evalhook* #'watch-evalhook))
	   (return (values-list !values))))
     (t (eval xpr#))))

(defun watch (FormToEval &optional env StepAction)
  (prog ((indent# 0) vals)
	(setq vals (prog1 (multiple-value-list
			      (watch-evalhook FormToEval env))
			  (setq *evalhook* nil)))
	(return (values-list vals))))

;had to move these to after the functions were defined
(defvar formprinter (function traceinfp))
(defvar valueprinter (function traceinvp))

