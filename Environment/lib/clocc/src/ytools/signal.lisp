;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: signal.lisp,v 2.1 2005/12/26 00:25:17 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on %ytools/ object setter repeat)

(needed-by-macros
   (export '(signal-problem signal-condition
	     breakpoint error-break tr untr tr* local-tr)))

(self-compile-dep :macros)
;;;;(slurp-whole-file)

(defmacro breakpoint (&rest stuff)
   (multiple-value-let (simple _ who-prints-place condspec
			;;;;breakspec
			proceedspec)
		       (signal-problem-analyze stuff)
      (cond ((and simple
		  (eq who-prints-place 'error-form)
		  ;;;;(not breakspec)
		  (or (not proceedspec)
		      (eq proceedspec ':proceed)))
	     `(break ,@condspec))
	    (t
	     (error "breakpoint too complex: ~s" `(break ,@stuff))))))
	     
; Syntax: (SIGNAL-CONDITION 
;            [(:CONDITION c) | (:CLASS condition-class -args-) | -outargs- ])
(defmacro signal-condition (&rest args)
  (let-fun ((args-ignore (al)
	       (cond ((not (null al))
		      (cerror "I will ignore them"
			 "Redundant arguments in signal-condition: ~s"
			 args)))))
   (match-cond args
      ?( ((:condition ?c) ?@should-be-empty)
	(args-ignore should-be-empty)
	`(signal ,c))
      ?( ((:class ?condition-class ?@args) ?@should-be-empty)
	(args-ignore should-be-empty)
	`(signal (make-condition ',condition-class ,@args)))
      (t
       (error "Ill-formed call: " `(signal-condition ,@args))))))


; Syntax: (SIGNAL-PROBLEM [| [:PLACE] [place-label | ()] | :NOPLACE]
;            [(:CONDITION c) | (:CLASS condition-class -args-) | -outargs- ]
;            [| :FATAL | :PROCEED
;             | (:PROMPT-FOR "description of what you type" default)
;             | (:PROCEED "description of what will happen when continued")])
; For historical reasons, the place spec must come first or not at all.
; Everything else can be in any order.
(defmacro signal-problem (&rest args)
   (multiple-value-let (simple place-string who-prints-place condspec
			;;;;breakspec
			proceedspec)
		       (signal-problem-analyze args)
      (labels ((build-error-form ()
		  (cond ((or (not proceedspec) (eq proceedspec ':fatal))
			 `(error ,@condspec))
			((car-eq proceedspec ':prompt-for)
			 (let ((supplied-var (gensym))
			       (value-var (gensym))
			       (srmvar (gensym)))
			    `(restart-case (error ,@condspec)
				(prompt-for-restart-val (,supplied-var ,value-var)
				   :report (lambda (,srmvar)
					     (out (:to ,srmvar)
						   "You will be prompted for: "
						   ,@(butlast (cdr proceedspec))))
				   :interactive prompt-for-restart-val
				  (cond (,supplied-var ,value-var)
					(t ,(lastelt proceedspec)))))))
			(t
			 (let ((srmvar (gensym)))
			    `(restart-case (error ,@condspec)
				(continue ()
				 :report ,(cond ((atom proceedspec)
						 ;; must be :proceed
						 "I will attempt to continue")
						(t
						 `(lambda (,srmvar)
						     (out (:to ,srmvar)
							,@(cdr proceedspec))))))))))))
	 (maybe-wrap-debugger-hook
	    (cond ((and simple (eq proceedspec ':proceed))
		   `(cerror "I will try to proceed" ,@condspec))
		  (t
		   (build-error-form)))
	    place-string who-prints-place ;;;;breakspec
	    ))))
;;;;            [| (:BEFORE-BREAK -things-to-do-)]


(needed-by-macros

(defun maybe-wrap-debugger-hook (form place-string who-prints-place ;;;;breakspec
				 )
   (cond (                   ;;;;(and (not breakspec) ....)
	  (eq who-prints-place 'error-form)
	  form)
	 (t
	  (let ((condvar (gensym))
		(pdvar (gensym))
;;;;		(breakforms
;;;;		   (cond (breakspec
;;;;			  ;; We do this so dbg-save, defined
;;;;			  ;; in dbghck.nsp, will work.
;;;;			  (bind ((dbg-save-msg-postpone* true))
;;;;			     (<# expand-all-macros
;;;;				 (cdr breakspec))))
;;;;			 (t '())))
		)
	    `(let ((,pdvar *debugger-hook*))
	        (bind ((*debugger-hook*
			  (\\ (,condvar _)
			     (cond (,pdvar
				    (funcall ,pdvar ,condvar ,pdvar)))
			     ;;;;,@breakforms
			     ,@(cond ((eq who-prints-place
					  'debugger-hook)
				      `((out (:to *query-io*)
					     ,(break-string place-string "")
					     t)))
				     (t '())))))
		   ,form))))))

;; Values returned:
;; Boolean simple: true if it's a CERROR in disguise.
;; String place-string: non-"" if it's the name of the "place" the break
;;    occurred (usually the function that's complaining)
;; Symbol who-prints-place: Either error-form or debugger-hook.
;;    signal-problem doesn't need to print it separately
;; (Lst Sexp) condspec: A condition designator, in the form of a list of 
;;    arguments to ERROR.
;; (~ Sexp) proceedspec: If false, then error is fatal; otherwise,
;;    it's specification of how to proceed, either :PROCEED or 
;;    (:PROMPT-FOR ...)
(defun signal-problem-analyze (args)
   (let ((pl (car args))  ;;These never change; args is cdr'ed down.
	 (al (cdr args)))
      (let ((condspec false)    ;; condition description
	    ;;;;(breakspec false)   ;; stuff to do if break occurs
	    (proceedspec false) ;; how to proceed
	    (place pl) (args al)
	    (simple true)
	    ;; true if we can dispense with conditions, restarts, and such
	    (who-prints-place 'error-form))
	    ;; true if condspec printer mentions place
	 (cond ((eq place ':noplace)
		(setf place false))
	       ((eq place ':place)
		(setf place (cond ((eq (car al) '()) false)
				  (t (car al))))
		(setf args (cdr args)))
	       ((not (and place (is-Symbol place)))
		(setf args `(,place ,@args))
		(setf place false)))
	 (let ((place-string
		  (cond (place
			 (cond ((is-String place) place)
			       ((is-Symbol place) (symbol-name place))
			       (t (format nil "~a" place))))
			(t ""))))
	    (labels (
;;;;		     (set-breakspec (b)
;;;;			(cond (breakspec
;;;;			       (error "Multiple specifications of what to do before break: ~s"
;;;;				      `(signal-problem ,pl ,@al)))
;;;;			      (t (setq breakspec b))))
		     (set-proceedspec (p)
			(cond (proceedspec
			       (error "Multiple specifications of how to proceed: ~s"
				      `(signal-problem ,pl ,@al)))
			      (t
			       (cond ((memq p '(:novalue :continue))
				      (setq p ':proceed))
				     ((and (is-Pair p)
					   (memq p '(:novalue :continue)))
				      (setq p `(:proceed ,@(cdr p)))))
			       (cond ((not (eq p ':proceed))
				      (setf simple false)))
			       (setf proceedspec p))))
		     (set-condspec (c)
			(cond (condspec
			       (error "Multiple specifications of condition: ~s~%  ~s~% vs. ~s"
				      `(signal-problem ,pl ,@al)
				      condspec c))
			      (t
			       (setf condspec c)))))
	       (repeat :for ((x :in args)
			    (outargs '()))
		  (cond ((memq x '(:fatal :proceed :continue :novalue))
			 (set-proceedspec x))
			((atom x)
			 (setf outargs (cons x outargs)))
;;;;			((eq (car x) ':before-break)
;;;;			 (set-breakspec x))
			((memq (car x) '(:proceed :continue :novalue :prompt-for))
			 (set-proceedspec x))
			((memq (car x) '(:condition :class))
			 (set-condspec x)
			 (setf who-prints-place 'debugger-hook)
			 (setf simple false))
			(t
			 (setf outargs (cons x outargs))))
		:result (cond ((not (null outargs))
 			       (set-condspec (reverse outargs)))))
	       (cond ((not place)
		      (setf who-prints-place 'error-form)))
	       (setf condspec (condspec-analyze condspec place-string))
	       (values simple place-string who-prints-place condspec
		       ;;;;breakspec
		       proceedspec))))))
;;;; (~ Sexp) breakspec: If non-false, a clause of the form (:before-break ...)

;;; Given a condspec, return list of args suitable for 'error' or 'signal'.
;;; If place-string is not false, it is a string added to the front of
;;; the args telling where the error took place.
(defun condspec-analyze (condspec place-string)
	       (cond ((car-eq condspec ':condition)
		      `(,(cadr condspec)))
		     ((car-eq condspec ':class)
		      `((make-condition
				 ',(cadr condspec) ,@(cddr condspec))))
		     ((and (not (null condspec))
			   (is-String (car condspec))
			   (or (null (cdr condspec))
			       (string-member '#\~ (car condspec))))
		      `(,(concatenate 'string
			   (break-string place-string "~%")
			   (car condspec))
			,@(cdr condspec)))
		     (t
		      `(,(concatenate 'string
			    (break-string place-string "~%")
			    "~s")
			(make-Printable
			    (\\ (srm)
			       (out (:to srm) ,@condspec)))))))

;;; Produce an argument to 'error' that says where the error took place.
;;; 'after' is a string that comes between the first arg to error and the rest,
;;; typically a newline.  If place-string=false, forget 'after' and return the
;;; empty string.
(defun break-string (place-string after)
   (cond (place-string
	  (cond ((equal place-string "")
		 (concatenate 'string "BREAK" after))
		(t
		 (letrec ((tildes-hide (str)
			     (let ((r ""))
				(repeat :for ((start = 0 :then (+ p 1))
					      (p = (position #\~ str)
						 :then (position #\~ str
								 :start (+ p 1))))
				   ;;;;(out "start = " start 1 "p = " p :%)
				 :while p
				    (setq r (concatenate 'string
					       r (subseq str start p) "~~"))
				 :result (concatenate 'string
					     r (subseq str start))))))
		    (concatenate 'string (tildes-hide place-string) " broken" after)))))
	 (t "")))
)

(defmacro error-break (&rest stuff)
   `(signal-problem ,@stuff))

;;; Embarrassing; these are necessary because ':continue' is used in both
;;; 'repeat' and 'signal-problem'.
(datafun within-unwrap signal-problem quote)
(datafun within-unwrap error-break quote)

(defun prompt-for-restart-val ()
   (clear-input *query-io*)
   (format *query-io* 
      "Type :return <expression>; :ok for default value:~%")
   (let ((r (read *query-io*)))
      (cond ((eq r ':return)
	     (list true (eval (read *query-io*))))
	    ((eq r ':ok)
	     (list false false))
	    (t
	     (list true (eval r))))))

(defvar tr-indent* 3)

(needed-by-macros
; common-lispism
(defun string-member (ch str)
   (declare (type string str) (type character ch))
   (find ch str)   ) 
)



