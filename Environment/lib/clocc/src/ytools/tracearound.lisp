;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: tracearound.lisp,v 2.2 2005/12/30 19:16:24 airfoyle Exp $

(depends-on %module/ ytools %ytools/ nilscompat)

(depends-on %ytools/ multilet)

(needed-by-macros
   (export '(trace-around tr-on tr-off)))

;;; (trace-around  [ name | :noname ] [(:if -c-)] 
;;;      (:> -start-out-) 
;;;      -forms- 
;;;      (:< (-vars-) -end-out-))
;;; If not gated, or gate is open (see below), then evaluate forms in -c-, 
;;; (default: true), and if the last comes up true, the trace is "active."
;;; If gate is closed or -c- evaluates to false, it's "dormant."
;;; If the trace is active, increment trace-around-level* by 1, 
;;; indent the current 'out'-indent-level by
;;; tr-indent* (default: 3 spaces), then process the forms in 
;;; 'start-out' as if they occurred in 
;;;     (out ">" trace-around-level* "> " -start-out-).
;;; Whether the trace is active or dormant, 
;;; evaluate the -forms-, saving the values of the last as a list
;;; stored in the special variable out-vals*.
;;; Finally, if the trace is active, bind the -vars- (as if applying
;;; a function to the list out-vals*), then process the forms in 'end-out' 
;;; as if they occurred in 
;;;    (out "<" trace-around-level* "< " -end-out-)
;;; The elements of out-vals* are then returned as values.

;;; The name is a symbol which designates this trace to the user.
;;; It is also used to "gate" the trace, meaning to enable or disable
;;; it completely.
;;; The gate is initially open, but may be closed by executing (tr-off name).
;;; If is reopened by executing (tr-on name).

;;; List of names of all trace-arounds.  A name doesn't get here until
;;; the trace-around is encountered during execution.
;;; (Unfortunately, trace-arounds that are removed still stick around
;;; on these lists.  The system has no way of knowing whether a trace
;;; around with that name is still out there.)

;;; Idea list:
;;; Within start-out and end-out, the out operator (:n) refers to the
;;; name, lower-cased and printed without vertical bars or quotes.

(defvar all-trace-arounds* !(Symbol))

(defvar active-trace-arounds* !(Symbol))

(defvar trace-around-level* 0)

(defmacro trace-around (name &rest args)
   (multi-let (((condition-forms start-out forms end-vars end-out
		 _               _         _     _        _)
		(trace-around-analyze name args))
	       (gate-blocker false))
      (let ((has-name (not (eq name ':noname)))
	    (has-conds (not (null condition-forms))))
	 (cond (has-name
		(!= gate-blocker (trace-blocker-sym name))
	        (out (:to *error-output*) "Trace: " name :%))
	       (t
		(bind ((*print-level* 3))
		   (out (:to *error-output*) "Trace- " args :%))))
	 (let-fun ()
	    `(progn

		(bind ((trace-around-level* (+ trace-around-level* 1)))
		    ,(build-it)))
	  :where
       
 (build-it ()
    (cond ((or has-name has-conds)
	   (let ((active-var (gensym)))
	       `(let ((,active-var
		       ,(cond (has-name
			       `(cond ((trace-gate-check ',gate-blocker
							 ',name)
				       ,@(cond (has-conds
						condition-forms)
					       (t '(true))))
				      (t false)))
			      (t
			       `(progn ,@condition-forms)))))
		   (out (:to *trace-output*)
		      (:q (,active-var
			   ,@(start-stuff)
			   (:i> tr-indent*)))
		      (:v ,@(form-stuff))
		      (:q (,active-var
			   (:i< tr-indent*)
			   ,@(end-stuff)))))))

;;;;		   (ytools::bind-out-stream-indent *trace-output*
;;;;		      (cond (,active-var
;;;;			     ,@(start-stuff)
;;;;			     (!= (ytools::Out-stream-indent
;;;;				    (ytools::stream-outify *trace-output*))
;;;;				 (+ *-* tr-indent*))))
;;;;		      ,@(form-stuff)
;;;;		      (cond (,active-var
;;;;			     ,@(end-stuff))))
	  (t
	   `(out (:to *trace-output*)
	       ,@(start-stuff)
	       (:i> tr-indent*)
	       (:v ,@(form-stuff))
	       (:i< tr-indent*)
	       ,@(end-stuff)))))

 (start-stuff ()
    `(">" trace-around-level* "> "
	  ,@start-out :%))

 (form-stuff ()
    `((progn ,@forms)))

 (end-stuff ()
    `((:e (apply (\\ ,end-vars 
		    (:o "<" trace-around-level* "< "
			,@end-out :%))
		 out-vals*))))))))
      
(defun trace-around-analyze (name args)
   (let (conds cond-pos
	 (offset 2))
      (match-cond args
	 ?( ((:if ?@c) ?@remaining)
	   (!= conds c)
	   (!= cond-pos offset)
	   (!= offset (+ *-* 1))
	   (!= args remaining))
	 (t
	  (!= conds '())
	  (!= cond-pos -1)))
      (match-cond args
	 ?( ((:> ?@start-out) ?@forms (:< ?end-vars ?@end-out))
	   (let ((end-offset (+ offset (len forms) 1)))
	      (values conds start-out forms end-vars end-out
		      cond-pos
		      `(:compose (:tail 1) ,offset)
		      (+ offset 1)
		      `(:compose 1 ,end-offset)
		      `(:compose (:tail 2) ,end-offset))))
	 (t
	  (signal-problem trace-around :fatal
	     "Ill-formed: " `(trace-around ,name ,@args))))))

(defmacro tr-on (&rest names)
  `(trace-around-on ',names))

(defmacro tr-off (&rest names)
  `(trace-around-off ',names))

(defun trace-around-on (names)
   (let ((reallies
	    (repeat :for ((name :in names)
			  :collectors reallies)
	       (cond ((is-Symbol name)
		      (cond ((memq name all-trace-arounds*)
			     (one-collect reallies name))
			    (t
			     (out (:to *query-io*)
				  name
				  " is not the name of a known trace-around;"
				  " add to list? ")
			     (cond ((y-or-n-p)
				    (!= all-trace-arounds* (cons name *-*))
				    (one-collect reallies name))))))
		     (t
		      (out (:to *query-io*)
			   "Ignoring " name t)))
	     :result reallies)))
      (repeat :for ((name :in reallies))
	 (cond ((memq name active-trace-arounds*)
		(out (:to *query-io*) "Already on: " name t))
	       (t
		(!= active-trace-arounds* (cons name *-*))
		(!= (Symbol-value (trace-blocker-sym name)) false))))
      active-trace-arounds*))

(defun trace-around-off (names)
   (repeat :for ((name :in names))
      (cond ((is-Symbol name)
	     (cond ((memq name active-trace-arounds*)
		    (!= (Symbol-value (trace-blocker-sym name)) true)
		    (!= active-trace-arounds* (dremove1q name *-*))
		    (out (:to *query-io*)
			 name " deactivated" t))
		   ((memq name all-trace-arounds*)
		    (out (:to *query-io*)
			 name " is not on" t))
		   (t
		    (out (:to *query-io*)
			 name " is not the name of a known trace-around"
			 t))))
	    (t
	     (out (:to *query-io*)
		  "Ignoring " name t))))
   active-trace-arounds*)

(defun trace-gate-check (sym name)
   (cond ((not (boundp sym))
	  (format *error-output* "Trace-around: ~s~%" name)
	  (!= all-trace-arounds* (cons name *-*))
	  (!= active-trace-arounds* (cons name *-*))
	  (!= (Symbol-value sym) false)
	  true)
	 (t
	  (not (eval sym)))))

(defun trace-blocker-sym (g)
   (let ((str (string-concat "block " (symbol->string g))))
      (intern str (symbol-package g))))
