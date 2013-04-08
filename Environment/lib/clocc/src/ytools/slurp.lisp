;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: slurp.lisp,v 2.3 2006/05/20 01:44:24 airfoyle Exp $

;;; Copyright (C) 1976-2004
;;;     Drew McDermott and Yale University.  All rights reserved.
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(in-readtable in-regime needed-by-macros
	     with-post-file-transduction-hooks after-file-transduction
	     during-file-transduction setf-during-file-transduction 
	     fload-verbose* eval-when-slurping
	     slurp-eval slurp-ignore
	     now-loading* now-compiling* now-slurping*)))

(defvar source-suffixes* (adjoin lisp-source-extn* '("lisp" "lsy")
                                 :test #'string=))
(defvar obj-suffix* lisp-object-extn*)
(defvar object-suffixes* `(,lisp-object-extn*))

(defun pathname-is-source (pn)
   (member (Pathname-type pn)
	   source-suffixes*
	   :test #'string=))

(defun pathname-is-object (pn)
   (member (Pathname-type pn)
	   object-suffixes*))

;;; Make sure .lisp files are found before odder ducks.
(defun new-source-suffix (suff)
   (cond ((not (member suff ytools::source-suffixes* :test #'string=))
          (let ((tl (member "lisp" source-suffixes* :test #'string=)))
             (cond ((null tl)
                    (cerror "I will put it back"
                       "\".lisp\" missing from source-suffixes* list")
                    (on-list "lisp" source-suffixes*)
                    (setq tl source-suffixes*)))
             (on-list suff (rest tl)))))
  source-suffixes*)

(defstruct (Slurp-task
;;;;	      (:constructor make-Slurp-task (label default-handler))
	      (:print-object
	          (lambda (slurp-tsk srm)
		     (print-unreadable-object (slurp-tsk srm)
			(format srm "Slurp-task ~s~a"
				(Slurp-task-label slurp-tsk)
				(cond ((Slurp-task-default-handler
					  slurp-tsk)
				       " [has default handler]")
				      (t ""))
			(print-innards slurp-tsk srm))))))
   label
   (handler-table (make-hash-table :test #'eq :size 100))
;;; -- handler-table maps symbols to functions of one argument that
;;; handle forms beginning with that symbol
   default-handler
;;; -- default-handler is for all other forms.  If it's false, then
;;; there isn't a default handler.  
   file->state-fcn) 
;;; -- file->state-fcn takes a pathname and returns the state object
;;; for the slurp task (default: return nil).
;;; Actually, its argument is not necessarily a pathname, because
;;; we may want to slurp non-file entities.  (E.g., modules.)

(defvar all-slurp-tasks* !())

(defmacro def-slurp-task (name
			  &key ((:default default-handler^)
				'nil)
			       ((:file->state-fcn file->state-fcn^)
				'(\\ (_) nil)))
   (let ((task-var (build-symbol (:< name) *)))
      `(progn
	  (defparameter ,task-var
	      (make-Slurp-task :label ',name
			       :default-handler ,default-handler^
			       :file->state-fcn ,file->state-fcn^))
	  (datafun attach-datafun ,name
	     (defun :^ (_ sym fname)
	        (setf (href (Slurp-task-handler-table ,task-var)
			    sym)
		      (symbol-function fname))))
	  (setq all-slurp-tasks*
		(cons ,task-var
		      (delete-if (\\ (stask)
				    (eq (Slurp-task-label stask)
					',name))
				 all-slurp-tasks*))))))

(defun lookup-slurp-task (name)
   (dolist (stask all-slurp-tasks* false)
      (cond ((eq (Slurp-task-label stask)
		 name)
	     (return stask)))))

(defvar fload-verbose*            true)		
;;; -- true for message during FLOAD and related ops.

(defvar fload-indent*     0)

(defvar post-file-transduce-hooks* '*not-transducing
  "A list of things to do (i.e., functions of no arguments to call) 
after YTools file transducers finish.")

(defmacro with-post-file-transduction-hooks (&body b)
   `(let ((post-file-transduce-hooks* '()))
       (cleanup-after-file-transduction ,@b)))

(defmacro after-file-transduction (&body b)
   `(cond ((check-file-transduction ',b)
	   (on-list (\\ () ,@b) post-file-transduce-hooks*))))

(defun during-file-transduction ()
   (not (eq post-file-transduce-hooks* '*not-transducing)))

(defmacro setf-during-file-transduction (place val^)
   (let ((save-var (gensym)))
      `(let ((,save-var ,place))
	  (after-file-transduction (\\ () (setf ,place ,save-var)))
	  (setf ,place ,val^))))

(defun check-file-transduction (b)
   (cond ((during-file-transduction) true)
	 (t
	  (cerror "I will skip setting the hook"
	     "Attempt to set post-file-transduction hook when not transducing a file ~%   (~s)"
	     b)
	  false)))

;; Pathname of file ...
(defvar now-loading*     nil)  ; ... being loaded by 'fload'
(defvar now-compiling* false)  ; ... being compiled by 'fcompl'
;; -- we declare these here because they have to be bound to false
;; when we're slurping.


;; List (pathname -slurp-tasks-) if slurping, else false.
(defvar now-slurping* false)

(defvar slurping-stack* '())
;;;; (defvar previous-slurp-speclist* '())

(defvar show-file-ops* true)

;;; The default args for a file-op (such as 'fload' or 'fcompl') are
;;; stored in a vector #(files flags readtable).

(defun file-op-defaults-update (op specs possible-flags
				acc-defaults set-defaults)
   (let ((defaults (funcall acc-defaults)))
      (multiple-value-bind (files flags readtab)
	                   (flags-separate specs possible-flags)
	 (let ((new-vec
		  (vector (cond ((null files) (aref defaults 0))
				(t files))
			  (cond ((and (null flags) (null files))
				 (aref defaults 1))
				((memq '- flags)
				 !())
				(t
				 flags))
			  (decipher-readtable readtab
					      (aref defaults 2)
					      flags files))))
;;;;	    (out "new-vec = " new-vec :%)
	    (funcall set-defaults new-vec)
	    (cond ((and show-file-ops* (null files))
		   (format *error-output*
		       "File op ~s [flags: ~a] ~s ~%    [readtable: ~s]~%"
		       op
		       (aref new-vec 1)
		       (aref new-vec 0)
		       (aref new-vec 2))))
	    new-vec))))

(defun decipher-readtable (readtab default-readtab flags files)
   (cond ((eq readtab ':missing)
	  (cond ((and default-readtab
		      (null flags)
		      (null files)
		      (not (eq default-readtab
			       *readtable*)))
		 (format *error-output*
			 "Readtable ~s will be used for this file operation~%"
			 default-readtab)
		 default-readtab)
		(t
		 *readtable*)))
	 (t
	  (name->readtable readtab))))

;;; True if we're in the midst of an 'fload' or 'fcompl'
(defvar file-op-in-progress* false)

(defmacro cleanup-after-file-transduction (&body b)
   `(unwind-protect (progn ,@b)
       (dolist (h post-file-transduce-hooks*)
	  (funcall h))))

;;; Returns three values: filespecs, flags, and readtable.
;;; 'flags' is a list of characters.
;;; If readtable is missing, the value is :missing.
(defun flags-separate (args possible-flags)
   (let ((flags !())
	 (readtab (memq ':readtable args)))
      (cond (readtab
	     (setq args (nconc (ldiff args readtab)
			       (cddr readtab)))
	     (setq readtab (cadr readtab)))
	    (t (setq readtab ':missing)))
      (do ((al args (cond (flags-done al) (t (cdr al))))
	   (flags-done false)
	   symname)
	  ((or flags-done (null al))
	   (values al
		   (flags-check (reverse flags) possible-flags)
		   readtab))
         (cond ((is-Symbol (car al))
		(setq symname (symbol-name (car al)))
		(setq flags-done
		      (not (char= (elt symname 0)
				  #\-)))
		(cond ((not flags-done)
		       (setq flags (nreconc (rest (coerce symname 'list))
					    flags)))))
	       (t
		(setq flags-done true))))))

;;;;		(setq interned-flag
;;;;		      (intern fname ytools-package*))
;;;;		(cond ((memq interned-flag possible-flags)
;;;;		       (setq flags (cons interned-flag flags)))
;;;;		      (t
;;;;		       (cerror "I'll ignore it"
;;;;			   "Unexpected flag ~s; expected one of ~a"
;;;;			      (car al)
;;;;			      (mapcar (lambda (flag)
;;;;					 (intern (symbol-name flag)
;;;;						 *package*))
;;;;				      possible-flags)))))))))

;;; Return flags (characters) that are legal in this context.
(defun flags-check (flags expected)
   (mapcan (\\ (flag)
	         ;;;; (let ((flag (intern (Symbol-name flag) ytools-package*))) ...)
	      (cond ((member flag expected :test #'char-equal)
		     (list (char-downcase flag)))
		    (t
		     (cerror "I'll ignore it"
			"Unexpected flag '~a'; expected one of ~a"
			   flag expected)
		     !())))
	   flags))

(defvar fload-show-actual-pathnames* true)

(defun file-op-message (beg-message pn real-pn end-message)
   (if fload-verbose*
       (progn
	  (print-spaces fload-indent* *query-io*)
	  (format *query-io* "~a " beg-message)
	  (cond (pn
		 (format *query-io* "~a" (namestring pn))))
	  (cond ((and fload-show-actual-pathnames*
		     real-pn
		     (not (equal real-pn pn)))
		 (format *query-io* "~%")
		 (print-spaces
		     (+ fload-indent* (length beg-message) -2)
		     *query-io*)))
	  (format *query-io* "~a~%" end-message))))

(defmacro in-readtable (name)
  `(eval-when (:compile-toplevel :load-toplevel :slurp-toplevel :execute)
      (setq *readtable* (name->readtable ',name))))

(defun name->readtable (name)
   (cond ((not name) lisp-readtable*)
	 ((symbolp name)
	  (let ((rt (or (named-readtable name)
			(eval name))))
	     (cond ((typep rt 'readtable)
		    rt)
		   (t (error "in-readtable: ~s is not a readtable" rt)))))
	 ((readtablep name)
	  name)
	 (t
	  (error "in-readtable: ~s is not the name of a readtable"
		 name))))

(defmacro in-regime (pkg &optional rt)
   `(progn (in-package ,pkg)
	   (in-readtable ,(or rt pkg))))

(defvar hidden-slurp-tasks* !())

;;; 'stream-init', if not false, is
;;; a function to apply to the stream when it is opened.
;;; The order of the slurp-tasks is irrelevant, and may change
;;; as the process progresses.
(defun file-slurp (pn slurp-tasks stream-init)   ;;;; states
   (cond ((not (null slurp-tasks))
	  (let ((post-file-transduce-hooks* !()))
	     (with-open-file (s pn :direction ':input)
		(cleanup-after-file-transduction
		   (let (;;;;(fload-indent*  0)
			 (now-loading*  false)
			 (now-compiling* false)
			 (now-slurping*  (cons pn slurp-tasks))
			 (slurping-stack* (cons pn slurping-stack*))
			 #+:excl (excl:*source-pathname* pn)
			 #+:excl (excl:*record-source-file-info* nil)
			 (*package* *package*)
			 (*readtable* *readtable*)
			 (slurp-states
			    (mapcar
			       (\\ (slurp-task)
				  (funcall (Slurp-task-file->state-fcn
						   slurp-task)
					   pn))
			       slurp-tasks))
			 ;;; -- 'slurp-states' is a list of data
			 ;;; structures, the same length as
			 ;;; 'slurp-tasks'.  Each element of
			 ;;; 'slurp-states' serves as a blackboard for
			 ;;; the corresponding task.
			 (vis-tasks
			       (remove-if (\\ (k)
					     (memq (Slurp-task-label
						      k)
						   hidden-slurp-tasks*))
					  slurp-tasks)))
		      (cond ((not (null vis-tasks))
			     (file-op-message
				 (format nil "Slurping ~s"
					     (mapcar #'Slurp-task-label
						     vis-tasks))
				 pn false "...")))
;;;;		      (cond ((equal slurped-pn*
;;;;				    (->pathname "tezt-s.lisp"))
;;;;			     (setq slurped-pn* pn slurp-tasks* slurp-tasks)
;;;;			     (break "Slurping")))
		      (cond (stream-init
			     (funcall stream-init s)))
		      (do ((form (read s false eof*) (read s false eof*))
			   (tasks slurp-tasks)
			   (states slurp-states))
			  ((or (eq form eof*)
			       (progn
				  (multiple-value-setq
					(tasks states)
					(form-slurp form tasks states))
				  (null tasks)))
			   slurp-states)
			(setq now-slurping*
			      (cons pn tasks))
;;;;			(format t "Slurped form ~s~%" form)
			)
		      (cond ((not (null vis-tasks))
			     (file-op-message "...slurped" pn false ""))))))))
	 (t !())))

(defun forms-slurp (forms tasks states)
      (do ((l forms (cdr l)))
	  ((or (null l)
	       (null tasks))
	   (values tasks states))
	 (multiple-value-setq (tasks states)
	                      (form-slurp (car l) tasks states))))

;;; General slurpers take 3 args: the form, the task, and the slurp state
;;; They return two values: the remaining tasks, and their states.
(datafun-table general-slurp-handlers* general-slurper :size 50)

(defvar slurp-dbg* false)

;;; Each element of 'slurp-tasks' is something like :macros, or :header-info
;;; Handler returns t if the task should stop.  So 'form-slurp'
;;; returns the tasks that should continue, plus their corresponding
;;; states.
(defun form-slurp (r slurp-tasks slurp-states)
   (cond (slurp-dbg*
	  (format *error-output*
	     "Slurping ~s ~%  wrt tasks ~s~% and states ~s~%"
	     r slurp-tasks slurp-states)))
   (flet ((form-fcn-sym (e)
	     (cond ((and (consp e)
			(is-Symbol (car e)))
		   (car e))
		  (t false))))
      (let ((continuing-tasks !())
	    (continuing-states !())
	    ;; -- tasks and states for which a handler has declare
	    ;; "not done."  Once this happens no further handlers are tried
	    ;; and the task/state pair will be returned by 'form-slurp'.
	    (asym (form-fcn-sym r))
	    (form r))
	 (loop
	   ;; Macro-expand until handled generally or
	   ;; handled by all
	   ;; At this point 'slurp-tasks' and 'slurp-states' are
	   ;; those that remain unhandled after (zero or more)
	   ;; attempts at macro expansion.
	   (cond
	     ((or (null slurp-tasks)
		  (not asym))
	      (cond (slurp-dbg*
		     (format *error-output*
			"form-slurp exits~a~a~%  "
			(cond ((null slurp-tasks)
			       "/Reason: no slurp tasks remain to figure out")
			      (t ""))
			(cond ((not asym)
			       "/Reason: no asym")))))
	      (setq slurp-tasks
		    (nconc slurp-tasks continuing-tasks))
	      (setq slurp-states
		    (nconc slurp-states continuing-states))
	      (cond (slurp-dbg*
		     (format *error-output*
			"Tasks still alive: ~s~% with states ~s~%"
			slurp-tasks slurp-states)))
	      (return (values slurp-tasks slurp-states)))
	     (t
	      (let ((h (href general-slurp-handlers*
			     asym)))
		 (cond (h
			(cond (slurp-dbg*
			       (format *error-output*
				  "Got general slurp handler for ~s~%"
				  asym)))
			(multiple-value-bind
				      (tl sl)
				      (funcall h form slurp-tasks slurp-states)
			   ;; Odd situation: Some specific
			   ;; handler may have run before
			   ;; the general one was found.
			   (setq slurp-tasks
				 (nconc tl continuing-tasks))
			   (setq slurp-states
				 (nconc sl continuing-states))
			   (cond (slurp-dbg*
				  (format *error-output*
				     "form-slurp exits after form handled generally~%  Tasks still alive: ~s with states ~s~%"
				     slurp-tasks slurp-states)))
			   (return
			      (values slurp-tasks slurp-states))))
		       (t
			(do ((tasks slurp-tasks (cdr tasks))
			     (states slurp-states (cdr states))
			     (unclear-tasks !())
			     (unclear-states !()))
			    ((null tasks)
			     ;; We will try again after
			     ;; macro-expansion --
			     (setq slurp-tasks unclear-tasks)
			     (setq slurp-states unclear-states))
			   ;; Try to run handler --
			   (let ((task (first tasks))
				 (state (first states))
				 (task-done false)
				 (handled-by-default false)
				 (handled-by-task false) h)
			      (setq h (href (Slurp-task-handler-table task)
					    asym))
			      (cond ((not h)
				     (setq h (Slurp-task-default-handler
					      task))
				     (cond (h
					    (setq handled-by-default true)))))
			      (cond (h
				     (setq handled-by-task true)
				     (setq task-done (funcall h form state))))
			      (cond (handled-by-task
				     (cond (slurp-dbg*
					    (format *error-output*
					       "Task ~s handled~a Done? ~s~%"
					       task
					       (cond (handled-by-default
						      " [by default]")
						     (t ""))
					       task-done)))
				     (cond ((not task-done)
					    (on-list task
						     continuing-tasks)
					    (on-list state
						     continuing-states))))
				    (t
				     (on-list task unclear-tasks)
				     (on-list state unclear-states)))))
			(cond ((and (not (null slurp-tasks))
				    (macro-function asym))
			       (setq form (macroexpand-1 form))
			       (setq asym (form-fcn-sym form))
			       (cond (slurp-dbg*
				      (format *error-output*
					 "Macro-expanding to ~s~%"
					 form))))
			      (t
			       (setq slurp-tasks
				     (nconc slurp-tasks
					    continuing-tasks))
                               (setq slurp-states
				     (nconc slurp-states
					    continuing-states))
			       (cond (slurp-dbg*
				      (format *error-output*
					 "form-slurp exits with tasks ~s and states ~s still active~%"
					 slurp-tasks
					 slurp-states)))
			       (return
				   (values slurp-tasks
					   slurp-states)))))))))))))

;;; The idea is that almost every slurp task will treat 'progn'
;;; the same way, so we shouldn't have to create many replicas of
;;; this pattern --
(datafun general-slurper progn
   (defun :^ (form tasks states)
      (forms-slurp (cdr form) tasks states)))

(datafun general-slurper prog1 progn)
(datafun general-slurper prog2 progn)

(defvar eval-slurp-task* false)
(defvar eval-slurp-state* false)

(datafun general-slurper eval-when-slurping
   (defun :^ (forms tasks states)
      (eval-forms-for-slurp-tasks (cdr forms) tasks states)))

(datafun general-slurper eval-when
   (defun :^ (form tasks states)
      (cond ((memq ':slurp-toplevel (cadr form))
	     (eval-forms-for-slurp-tasks (cddr form) tasks states))
	    ((memq ':compile-toplevel (cadr form))
	     (forms-slurp (cddr form) tasks states))
	    (t
	     (values tasks states)))))

(defun eval-forms-for-slurp-tasks (forms tasks states)
   (do ((fl forms (cdr fl))
	(filter-forms !())
	(eval-forms !()))
       ((null fl)
	(setq eval-forms (nreverse eval-forms))
	;; If the filter-forms are missing, this is just a plain-
	;; vanilla 'eval-when', and we assume the values returned
	;; have nothing to do with whether some slurp tasks should cease.
	(cond ((null filter-forms)
	       (dolist (evf eval-forms)
		  (eval evf))
	       (values tasks states))
	      (t
	       (multiple-value-bind
			 (selected-tasks their-states)
			 (let ((selected-names
				  (mapcan (\\ (ff) (list-copy (cdr ff)))
					  filter-forms)))
			    (let ((select-zip
				     (mapcan
				        (\\ (task state)
					   (cond ((memq (Slurp-task-label task)
							selected-names)
						  (list (tuple task state)))
						 (t !())))
					tasks states)))
			      (values (mapcar #'first select-zip)
				      (mapcar #'second select-zip))))
		  ;; Now we do something a little odd, which is to
		  ;; evaluate exactly the same code for each task.
		  ;; To keep this from being meaningless, the variables
		  ;; eval-slurp-task* and eval-slurp-state* are bound to the
		  ;; current task and state.
		  (do ((stskl selected-tasks (rest stskl))
		       (stl their-states (rest stl))
		       ;; Being "selected" means a task can be eliminated.
		       ;; The unselected ones must therefore always
		       ;; continue. --
		       (continuing-tasks
			   (set-difference tasks selected-tasks))
		       (continuing-states
			   (set-difference states their-states)))
		      ((null stskl)
		       (values continuing-tasks continuing-states))
		    (let ((eval-slurp-task* (car stskl))
			  (eval-slurp-state* (car stl)))
		       (dolist (ev-form eval-forms)
			  (let (val-matters val)
			     (cond ((car-eq ev-form ':stop-slurp-if)
				    (setq val-matters true)
				    (setq val (eval (cadr ev-form))))
				   (t
				    (setq val-matters false)
				    (setq val (eval ev-form))))
;;;;			  (multiple-value-bind
;;;;			               (val-matters val)
;;;;				       (cond ((car-eq ev-form ':stop-slurp-if)
;;;;					      (values true
;;;;						      (eval (cadr ev-form))))
;;;;					     (t
;;;;					      (values false
;;;;						      (eval ev-form))))
			     (cond ((not (and val-matters val))
				    ;; If a :stop-slurp-if form
				    ;; returns true, the task is done
				    ;; Otherwise,we keep it on
				    ;; 'continuing-tasks'.
				    (on-list eval-slurp-task*
					     continuing-tasks)
				    (on-list eval-slurp-state*
					     continuing-states)))))))))))
     (cond ((car-eq (car fl) ':slurp-filter)
	    (on-list (car fl) filter-forms))
	   (t
	    (on-list (car fl) eval-forms)))))

;;; This has no effect when evaluated.
(defmacro :slurp-filter (&whole e &rest _)
   `',e)

(datafun general-slurper with-packages-unlocked
   (defun :^ (form tasks states)
      (with-packages-unlocked
	 (forms-slurp (cdr form) tasks states))))

(defun slurp-eval (e _)
   (eval e)
   false)

(datafun general-slurper in-package
   (defun :^ (form tasks states)
      (eval form)
      (values tasks states)))

;;; For use by slurp tasks 
(defun slurp-ignore (_ _) false)

(defun pathname-source-version (pn)
  (cond ((is-Pseudo-pathname pn) false)
	(t
	 (let ((rpn (cond ((is-Pathname pn) pn)
			  (t (pathname-resolve pn false)))))
	    (let ((pn-type (Pathname-type rpn)))
	       (cond (pn-type
		      (cond ((equal pn-type obj-suffix*)
			     (get-pathname-with-suffixes
				rpn source-suffixes*))
			    ((probe-file rpn)
			     rpn)
			    (t false)))
		     ((probe-file rpn) rpn)
		     (t (get-pathname-with-suffixes
			   rpn source-suffixes*))))))))

(defun pathname-object-version (pn only-if-exists)
   (let ((ob-pn
	    (pathname-find-associate pn 'obj-version obj-suffix*
				     only-if-exists)))
      (cond ((and (not only-if-exists)
		  (not ob-pn))
	     (cerror "I will treat it as :unknown"
		     "Pathname has no object version: ~s" ob-pn)
	     ':none)
	    (t ob-pn))))

;;; pn must be a resolved Pathname, not a YTools Pathname.
(defun get-pathname-with-suffixes (pn suffixes)
   (do ((sfl suffixes (cdr sfl))
	(found false)
	newpn)
       ((or found (null sfl))
	(and found newpn))
      (setq newpn (merge-pathnames
		     (make-Pathname :type (car sfl))
		     pn))
      (cond ((probe-file newpn)
	     (setq found true)))))

(defconstant can-get-write-times*
    #.(not (not (file-write-date
		    (concatenate 'string ytools-home-dir* "files.lisp")))))

(defun pathname-write-time (pname)
  (setq pname (pathname-resolve pname false))
  (and can-get-write-times*
       (probe-file pname)
       (file-write-date pname)))





