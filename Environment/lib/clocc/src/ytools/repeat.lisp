;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: repeat.lisp,v 2.4 2007/01/29 14:17:24 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on %ytools/ outin binders)

(self-compile-dep :macros)

;;;;(slurp-whole-file)

(eval-when (:compile-toplevel :load-toplevel)
   (export '(repeat forall exists keyword-args->alist
	     one-collect list-collect empty-Collector Collector-elements
	     collector-clear)))

(defmacro forall (&rest stuff)
   (multiple-value-let (vars lists body) (for-analyze stuff)
      `(every (\\ ,vars ,@body) ,@lists)   ))

(defmacro exists (&rest stuff)
   (multiple-value-let (vars lists body) (for-analyze stuff)
      `(some (\\ ,vars ,@body) ,@lists)   ))

(defmacro for (&rest stuff)
   (let (vars lists (whenclause false) mainclause declarations)
      (multiple-value-setq (vars lists mainclause)
                           (for-analyze stuff))
      (multiple-value-setq (declarations mainclause)
			   (declarations-separate mainclause))
      (cond ((and (not (null mainclause))
		  (consp (car mainclause))
		  (memq (car (car mainclause))
			'(when :when)))
	     (setq whenclause (car mainclause))
	     (setq mainclause (cdr mainclause))))
      (cond ((and (= (len mainclause) 1)
		  (consp (car mainclause))
		  (memq (car (car mainclause))
			'(save splice filter)))
	     (let ((c (car (car mainclause)))
		   (e (cadr (car mainclause))))
		`(repeat :for ,(mapcar (lambda (v l) `(,v :in ,l))
				       vars lists)
		            ,@declarations
		  ,@(cond (whenclause `(:when ,(cadr whenclause)))
			  (t !()))
		  ,(cond ((eq c 'save)
			  ':collect)
			 (t ':nconc))
		  ,(cond ((eq c 'filter)
			  (let ((ev (gensym)))
			     `(let ((,ev ,e))
				 (cond (,ev (list ,ev))
				       (t !())))))
			 (t e)))))
            (t
             (error "Bad syntax: ~s" `(for ,@stuff))))))

(needed-by-macros

(defun for-analyze (stuff)
      (do ((l stuff (cdr l))
           (vars nil (cons (caar l) vars))
           (lists nil (cons (caddar l) lists)))
          ((or (null l)
               (atom (car l))
               (not (memq (cadar l) '(in :in))))
           (cond ((null vars)
                  (error "forall or exists with no vars: ~s" stuff)))
           (values (nreverse vars) (nreverse lists) l))   ))
)

;;; Syntax:
;;; (repeat [:for (<varspec>* [:collectors <var>*])]
;;;    <repeat clause>*
;;;  [:where local-fundef*])
;;;     
;;; where
;;; <varspec> ::== <var> | (<var> <val>)
;;;                   | (<var> = <start> [:by <inc>] [:to <thresh>])
;;;                   | (<var> = <start> :then <subsequent>)
;;;                   | (<var> = <start> :then :again)
;;;                   | (<var> :in <list> [:tail <var>])
;;; <repeat clause> ::== <exp>+
;;;    | :when <test>
;;;    | [ :collect | :nconc | :append ] <collect-spec>
;;;    | [ :while | :until ] <test>
;;;    | :result <exp>
;;;
;;; <collect-spec> ::== <exp> | (:into <var> <exp>)
;;;
;;; Semantics: variables are bound.  
;;; Local fundefs are bound (so that all local variables are visible inside
;;;    their bodies).
;;; Body (the sequence of repeat clauses) 
;;; is executed.  Then variables are
;;; "bumped", body is executed again, etc.  Bumping occurs by finding the
;;; next values, then resetting all the variables at once, as 'do' does.
;;;   (This is a change from the old version of Nisp 'repeat'.)
;;; :when <test>, if <test> is false,
;;;  causes rest of this iteration to be skipped, including
;;;    :whiles, :untils, :results, collects, etc.
;;; :collect, :nconc, :append specifies how
;;;    values are to be accumulated in a <var>, which must have been declared
;;;    as a :collector .  (If no :into, <var> defaults as explained.)
;;;       :save causes value of <exp> to be added to the end of the
;;;           collector
;;;       :splice causes value to be appended to the end of the collector;
;;;       :append causes copy of value to be appended to the end of 
;;;           the collector
;;; In a result clause, the value of a collector is
;;; the list of values accumulated.
;;; Everywhere else, including inside functions called by a result clause, 
;;; the value is the actual Collector.
;;; If a <test> says "stop," then the value of the 'repeat' is the value 
;;;   of the <exp> in the next :result clause after the <test>, after
;;;   filling stuff in according to the abbreviation conventions below.
;;; Note that some variable-binding constructs (such as '=' and ':in') imply
;;; tests.  These behave as though they were present at the beginning of the 
;;; 'repeat' body, before all the explicit repeat clauses.
;;; The macro will issue a warning message if there are no tests among the
;;; repeat clauses.  Sometimes not having any tests is actually correct,
;;; because the 'repeat' is going to exit some nonstandard way
;;; (say, by throwing
;;; a value).  To make the warning go away, put in a :while or :until whose 
;;; <test> is a string.  This will be discarded and the warning suppressed.
;;; Any atom in the repeat body is ignored unless it is one of the keywords
;;; specified above (or one allowed by the abbreviation convention below).  
;;; So you can write :else :result if it makes the control flow clearer.

;;; Abbreviations: 

;;;  A collect without an :into means to collect into the first collector.
;;;  If there are collects, but no :collectors declared, then a default
;;;  collector is created.  
;;;  If there is an (explicit or implied) termination test with no
;;;    :result after it, a :result is tacked onto the end of 
;;;    the 'repeat' body; if there are any collectors, including the default
;;;    collector, the result is the contents of the first collector.  Otherwise
;;;    the result is nil.
;;; These rules have the consequence that if there is just one collector var, 
;;;    then you may omit all its occurrences.
;;;    and in particular omit the :result <v> and the :into <v>
;;;    The implied <result> appears as the last <repeat clause>.


;;; Idea list:
;;; Should allow *-* in :then expression, but we currently don't.
;;; Why is just the first collector returned, instead of all of them
;;; as values?

;; Implementation matters:
;; Each variable spec gets analyzed into the following:
;; a list of auxiliary variables and their properties, including
;;; initial values and such.  
;;; We keep track of their positions for use by syntax checkers.

(defstruct (Rep-var-prop (:constructor make-Rep-var-prop (prop val position))
	                 (:type list))
   prop val position)
;;; The position is for complex macros that want to know where the
;;; prop originally occurred.

(defstruct (Rep-var (:constructor make-Rep-var (mode name alist))
		       (:type list))
   mode name alist)
;; mode is *simple, *step, *throughlist, *reset
;; alist is list of Rep-var-prop's

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (defun lookup-rep-var-prop (p a) (assq p a)))

(defstruct (Rep-clause (:constructor make-Rep-clause (mode pos stuff))
			  (:type list))
   mode pos stuff)   
;;; 'stuff' is different for every 'mode':
;;; 'mode'                         'stuff'
;;; :collect, :append, :nconc      (collector thing-collected)
;;; :while, :until, :when          test
;;; :do                            expressions-to-evaluate
;;; :result                        result-expression
;;; :within                        (wrapping-expression
;;;                                 list-of-Within-clauses)
;;;    where a Within-clause captures a :continue --

(defstruct (Within-clauses (:constructor make-Within-clauses
				       (subclauses continue))
	                   (:type list))
   subclauses continue)

(defstruct (Collector
	     (:predicate is-Collector)
	     (:print-function
	           (lambda (c srm level)
			(declare (ignore level))
			(out (:to srm) "#<Collector ("
			     (:q ((null (Collector-elements c))
				  "empty")
				 (t "..." (car (Collector-last-tail c))))
			     ")>")))
	    (:constructor empty-Collector
			  (&aux (elements '()) (last-tail false))))
   elements
   last-tail)

(declaim (inline col_elts))

(defun col_elts (c) (Collector-elements c))

(defsetf col_elts (c) (el)
   `(error
       "Can't set elements of Collector ~s to ~s" ,c ,el))

(defun collector-clear (col)
   (setf (Collector-elements col) !())
   (setf (Collector-last-tail col) false))

(defmacro collector (&optional _)
   `(empty-Collector))

(defun list-collect (c l)
   (cond ((not (null l))
	  (cond ((null (Collector-elements c))
		 (setf (Collector-elements c) l))
		(t
		 (setf (cdr (Collector-last-tail c))
		       l)))
	  (setf (Collector-last-tail c) (last l)))))

(defun one-collect (c x)
  (list-collect c (list x)))

(defmacro repeat (&rest stuff)
   (multiple-value-let (vars decls clauses local-fundefs _)
		       (repeat-analyze stuff)
      (multiple-value-let (standard-vars collectors _)
			  (repeat-vars-analyze vars)
	 (cond ((and (null collectors)
		     (null local-fundefs)
		     (forall (v :in standard-vars)
			(and (eq (Rep-var-mode v) '*throughlist)
                             (not (lookup-rep-var-prop ':init (Rep-var-alist v)))
			     (let ((p (lookup-rep-var-prop
                                         ':tail (Rep-var-alist v))))
			        (cond (p
				       (= (Rep-var-prop-position p)
					  -1))
				      (t
				       true)))))
		     (= (len clauses) 1)
		     (memq (Rep-clause-mode (car clauses))
			   '(:collect :nconc :append))
		     ;; Make sure no ':into' was specified
		     (not (car (Rep-clause-stuff (car clauses)))))
		(mapping-repeat standard-vars decls (car clauses)))
	       (t
		(non-mapping-repeat standard-vars collectors decls clauses
				    local-fundefs stuff))))))

(needed-by-macros

(defun mapping-repeat (vars decls clause)
   `(,(case (Rep-clause-mode clause)
	 (:nconc 'mapcan)
	 (:append 'mappend)
	 (t 'mapcar))
     (\\ ,(mapcar #'Rep-var-name vars)
	,@decls
	,(cadr (Rep-clause-stuff clause)))
     ,@(mapcar (\\ (v) (Rep-var-prop-val
                          (lookup-rep-var-prop 'init (Rep-var-alist v))))
	       vars)))

;;; It's important that the :type be list because we use 'assq' to search
;;; a list of these things --
(defstruct (Test-result-match (:type list) (:conc-name t-r-match-))
   test-clause result-clause res-fun-sym context)
;;;  -- 'context' is false for top level, else the (:continue ...)
;;; expression within which this match was found.

(defun non-mapping-repeat (standard-vars collectors decls clauses
			   local-fundefs orig-call-stuff)
   (letrec ()
	 ;; standard-vars is now a list of Rep-vars
	 ;; Get tests and related stuff and put at front:
         (setq clauses
	       (nconc (mapcan #'repeat-var-implied-front standard-vars)
		      clauses))
	 (cond ((null collectors)
		(cond ((clauses-search-for-modes
			       clauses '(:collect :nconc :append))
		       (let ((colvar (gensym)))
			  (setq collectors (list colvar))))))
	       (t
		(setq collectors (cdr collectors))))
	 (multiple-value-let (test-result-map extra-clauses)
	                     (tests-results-match clauses collectors)
	    (setq clauses (nconc clauses extra-clauses))
	    (check-for-unused-results clauses test-result-map)
	    (cond ((not (clauses-search-for-modes clauses '(:while :until)))
		   (out (:to *error-output*)
			"Warning -- 'repeat' without a test: "
			`(repeat ,@orig-call-stuff) t)))
	    (setq local-fundefs
	          (nconc (res-fun-definitions test-result-map collectors false)
			 local-fundefs))
	    `(let (,@(mapcan #'repeat-var-implied-bindings standard-vars)
		   ,@(mapcar (\\ (cv)
				`(,cv (empty-Collector)))
			     collectors))
		,@decls
		(labels ,local-fundefs
		   (common-lisp::loop
		      ,@(rep-clauses->loop-body
			    clauses standard-vars collectors
			    test-result-map)))))
	 ))

(defun clauses-search-for-modes (clauses modes)
	   (exists (c :in clauses)
	      (or (memq (Rep-clause-mode c) modes)
		  (and (eq (Rep-clause-mode c) ':within)
		       (exists (wc :in (cadr (Rep-clause-stuff c)))
			  (clauses-search-for-modes
			     (Within-clauses-subclauses wc)
			     modes))))))
		      
(datafun-alist within-unwrap-handlers* within-unwrap)

;;; Return <vars, declarations, Rep-clauses, local-fundefs, fundef-positions >
(defun repeat-analyze (stuff)
;;;;   (trace-around repeat-analyze
;;;;      (:> "(repeat-analyze: " stuff ")")
   (multiple-value-let (vars decls body offset)
		       (cond ((memq (car stuff) '(:for for))
			      (multiple-value-bind (decls body)
				                   (declarations-separate
						      (cddr stuff))
				 (values (cadr stuff) decls body
					 (+ (len decls) 2))))
			     (t
			      (values !() !() stuff 0)))
      (multiple-value-let (local-fundefs body fundef-poses _)
			  (extract-where '() body :offset offset)
	 (labels ((group-clauses (body pos)
		     (cond ((null body) '())
			   ((memq (car body) '(:collect :nconc :append))
			    (cond ((null (cdr body))
				   (early-end (car body)))
				  (t
				   (cons (make-Rep-clause
					    (car body)
					    (+ pos 1)
					    (let ((e (cadr body)))
					       (cond ((car-eq e ':into)
						      (tuple (cadr e)
							     (caddr e)))
						     (t
						      (tuple false e)))))
					 (group-clauses
					    (cddr body) (+ pos 2))))))
			   ((memq (car body)
				  '(:when :until :while :result))
			    (cond ((null (cdr body))
				   (early-end (car body)))
				  (t
				   (cons (make-Rep-clause
					    (car body)
					    (+ pos 1)
					    (cadr body))
					 (group-clauses (cddr body)
							(+ pos 2))))))
			   ((eq (car body) ':within)
			    (let ((continues (within-unwrap (cadr body))))
			       (cons (make-Rep-clause
				         ':within
					 (+ pos 1)
					 (tuple (cadr body)
						continues))
				     (group-clauses (cddr body) (+ pos 2)))))
			   (t
			    (let ((groups (group-clauses
					     (cdr body) (+ pos 1))))
			       (cond ((atom (car body))
				      ;; ignore stray atoms
				      groups)
				     ((and (not (null groups))
					   (eq (Rep-clause-mode
						  (car groups))
					       ':do))
				      (cons (make-Rep-clause
					       ':do pos
					       (cons (car body)
						     (Rep-clause-stuff
							(car groups))))
					    (cdr groups)))
				     (t
				      (cons (make-Rep-clause
					       ':do
					       pos
					       (list (car body)))
					    groups)))))))
		  ;;; Returns (Lst Within-clauses)
		  (within-unwrap (exp)
;;;;		     (trace-around within-unwrap
;;;;			(:> "(within-unwrap: " exp ")")
		     (cond ((atom exp)
			    !())
			   (t
			    (let ((h (alref within-unwrap-handlers* (car exp))))
			       (cond (h
				      (funcall h exp #'within-unwrap #'group-clauses))
				     (t
				      (mapcan #'within-unwrap exp))))))
;;;;			(:< (val &rest _) "within-unwrap: " val))
		     )

		  (early-end (op)
		     (error "Repeat ends with ~s in ~%~s"
			    op `(repeat ,@stuff))))
	    (values vars decls
		    (group-clauses body offset)
		    (mapcar (\\ (fd) (cond ((car-eq fd ':def) (cdr fd))
					   (t fd)))
			    local-fundefs)
		    fundef-poses))))
;;;;      (:< (VARS declarations rep-clauses &rest _)
;;;;	 "repeat-analyze: " vars 1 declarations :% 3 rep-clauses))
   )

(datafun within-unwrap quote
   (defun :^ (_ _ _) !()))

(datafun within-unwrap repeat quote)

(datafun within-unwrap :continue
   (defun :^ (exp _ group-clauses)
      (list (make-Within-clauses
	       (funcall group-clauses (cdr exp) 1)
	       exp))))

;;; Put into standard form, but not all the way to executable
;;; Form is (mode symbol stuff)
;;; Returns < vars-in-standard-form, collectors, collector-pos >
;;; Collectors starts with ':collectors' if present, just to
;;; maintain synchronism with types.
(defun repeat-vars-analyze (vars)
   (cond ((and (is-Symbol (car vars))
	       (memq (cadr vars) '(:in =)))
	  (values (iter-vars-analyze (list vars))
		  !()
		  -1))
	 (t
	  (let ((coll (or (memq ':collectors vars)
			  (memq ':collector vars))))
	     (cond (coll
		    (let ((vars (ldiff vars coll)))
		       (cond ((exists (c in (cdr coll))
				 (not (is-Symbol c)))
			      (error "Illegal collectors in ~s"
				     vars)))
		       (values (iter-vars-analyze vars)
			       coll
			       (+ (len vars) 1))))
		   (t
		    (values (iter-vars-analyze vars)
			    !(Sexp)
			    -1)))))))
		     
(defun iter-vars-analyze (vars)
   (mapcar (\\ (v)
	      (let-fun ((check-var (v)
			  (cond ((not (is-Symbol v))
				 (error "Illegal variable in 'repeat': ~s"
					v)))))
		 (cond ((atom v)
			(check-var v))
		       (t
			(check-var (car v)))))
	      (cond ((is-Symbol v)
		     (make-Rep-var '*simple v '()))
		    ((null (cdr v))
		     (make-Rep-var '*simple (car v) '()))
		    (t
		     (case (cadr v)
			(=
			 (repeat-=-analyze v))
			((in :in)
			 (repeat-in-analyze v))
			(t
			 (repeat-no-key-analyze v))))))
	   vars))

(defun repeat-no-key-analyze (v)
   (cond ((null (cddr v))
	  (make-Rep-var
		 '*simple (car v)
		 (list (make-Rep-var-prop
			  'init (cadr v) 1))))
	 ((equal (cddr v) '(:then :again))
	  (make-Rep-var
	     '*each-iter (car v)
	     (list (make-Rep-var-prop 'init (cadr v) 1)
		   (make-Rep-var-prop 'iterfcnvar (gensym) -1))))
	 (t
	  (make-Rep-var
	       '*reset (car v)
	       (cons (make-Rep-var-prop 'init (cadr v) 1)
		     (cond ((and (not (eq (caddr v) ':then))
				 (null (cdddr v)))
			    (list (make-Rep-var-prop ':then (caddr v) 2)))
			   (t
			    (keyword-args->alist (cddr v) '(:then)
						 :offset 3))))))))

(defun repeat-in-analyze (v)
   (make-Rep-var
      '*throughlist
      (car v)
      (cons (make-Rep-var-prop 'init (caddr v) 2)
	    (let ((alist (keyword-args->alist
			    (cdddr v) '(:tail (:initbind :init))
			    :offset 4)))
	       (let ((tv (lookup-rep-var-prop ':tail alist)))
		  (cond ((not tv)
			 (setq tv (gensym))
			 (cons (make-Rep-var-prop ':tail tv -1)
			       alist))
			(t
			 alist)))))))

(defun repeat-=-analyze (v)
   (let ((var (cond ((eq (car v) '_) (gensym))
                    (t (car v))))
	 (alist
	    (cons (make-Rep-var-prop 'init (caddr v) 2)
		  (keyword-args->alist
		     (cdddr v)
		     '((:by by) (:to to) :then)
		     :offset 4))))
      (let ((step (or (lookup-rep-var-prop ':by alist)
		      (lookup-rep-var-prop ':to alist)))
	    (reset (lookup-rep-var-prop ':then alist)))
	 (let ((each-iter (and reset (eq (Rep-var-prop-val reset) ':again))))
	    (cond (each-iter
		   (setq alist (delete reset alist :test #'eq :count 1))
		   (setq reset false)))
	    (let ((modecount
		     (+ (cond (step 1) (t 0))
			(cond (reset 1) (t 0))
			(cond (each-iter 1) (t 0)))))
	       (cond ((> modecount 1)
		      (error "Overconstrained 'repeat' var: ~s" v))
		     ((= modecount 1)
		      (cond (each-iter
			     (make-Rep-var
				'*each-iter
				var
				(cons (make-Rep-var-prop
					 'iterfcnvar (gensym) -1)
				      alist)))
			    (step
			     (cond ((lookup-rep-var-prop ':to alist)
				    (setq alist 
				          (cons (make-Rep-var-prop
						   'limvar (gensym) -1)
						alist))))
			     (let ((b (lookup-rep-var-prop ':by alist)))
				(cond ((and b (not (is-Number (Rep-var-prop-val b))))
				       (setq alist
					     (cons (make-Rep-var-prop
						      'stepvar (gensym) -1)
						   alist))))
				(make-Rep-var '*step var alist)))
			    (t
			     (make-Rep-var '*reset var alist))))
		     (t
		      (make-Rep-var '*simple var alist))))))))

;;; Returns < matchtab, revised clauses >
;;; where matchtab is a list of Test-result-match
;;; and revised clauses may include an implied :result tacked on
;;; after last test.
(defun tests-results-match (clauses collectors)
   (let ((test-result-map !((Tup test result Symbol)))
	 ;; Actually, there's at most one created, for
	 ;; the implicit return of collector or nil
	 (extra-clauses !()))
      (let-fun ((process-clauses (clauses outer-layers continue-stack)
		   ;; 'continue-stack' is stack of :continue's we're in,
		   ;; with nil as the last layer.
		   ;; 'outer-layers' is list of lists of clauses to search for
		   ;; matches.  It's length is one less than continue-stack's.
;;;;		   (trace-around process-clauses
;;;;		      (:> "(process-clauses: " clauses 1 (length outer-layers)
;;;;			  :% 6 (car continue-stack) ")")
		   (do ((cl clauses) this-clause)
		       ((null cl))
		      (setq this-clause (car cl))
		      (cond ((eq (Rep-clause-mode this-clause) ':within)
			     (dolist (continue (cadr (Rep-clause-stuff this-clause)))
			        (process-clauses
				   (Within-clauses-subclauses continue)
				   (cons (cdr cl) outer-layers)
				   (cons (Within-clauses-continue continue)
					 continue-stack)))
			     (setq cl (cdr cl)))
			    ((and (memq (Rep-clause-mode this-clause) '(:while :until))
				  (not (is-String (Rep-clause-stuff this-clause))))
			     (let-fun ()
				(cond ((not (or (find-match (cdr cl)
							    (car continue-stack))
						(some (\\ (clause-list continue)
							 (find-match clause-list
								     continue))
						      outer-layers
						      (cdr continue-stack))
						(find-match extra-clauses nil)))
				       (setq extra-clauses
					     (list (make-Rep-clause
						      ':result -1
						      (cond ((= (len collectors) 1)
							     (car collectors))
							    (t 'nil)))))
				       (connect (car extra-clauses) (gensym) nil)))
			      :where
			         (:def find-match (search-clauses enclosing-continue)
;;;;				    (trace-around find-match
;;;;				       (:> "(find-match: "search-clauses
;;;;					   :% 6 enclosing-continue  ")")
				    (let ((nextres
					     (assoc ':result search-clauses)))
				       (cond (nextres
					      (let ((prevocc
						       (result-map-lookup
							  nextres test-result-map)))
						 (let ((rfun 
							  (cond (prevocc
								 (t-r-match-res-fun-sym
								    prevocc))
								(t (gensym)))))
						    (connect nextres rfun
							     enclosing-continue)
						    true)))
					     (t false)))
;;;;				       (:< (val &rest _) "find-match: " val))
				    )

			         (:def connect (matching-res-clause rfun continue)
;;;;				    (out "Connecting " this-clause
;;;;					 " and " matching-res-clause
;;;;					 :% " in context " continue :%)
				    (setq test-result-map
				          (cons (make-Test-result-match
							 :test-clause this-clause
							 :result-clause
							    matching-res-clause
							 :res-fun-sym rfun
							 :context continue)
						test-result-map))))
			     (setq cl (cdr cl)))
			    (t
			     (setq cl (cdr cl)))))
;;;;		      (:< (val &rest _) "process-clauses: " val))
		   ))
	 (process-clauses clauses !() (list nil))
	 (values test-result-map extra-clauses))))

(defun check-for-unused-results (clauses test-result-map)
   (do ((cl clauses (cdr cl))
	(rcl))
       ((null cl))
      (setq rcl (car cl))
      (cond ((eq (Rep-clause-mode rcl) ':result)
	     (let ((occ (result-map-lookup rcl test-result-map)))
	        (cond ((not occ)
		       (format *error-output*
			    "Warning: Unused :result ~s~%"
			    (Rep-clause-stuff rcl))))))
	    ((eq (Rep-clause-mode rcl) ':within)
	     (dolist (wc (cadr (Rep-clause-stuff rcl)))
		(check-for-unused-results (Within-clauses-subclauses wc)
					  test-result-map))))))

(defun res-fun-definitions (test-result-map collectors context)
   (let ((defs !((Tup Symbol &rest Sexp)))
	 (col-synonyms (mapcar (\\ (_) (gensym)) collectors)))
      (do ((tripl test-result-map (cdr tripl)))
	  ((null tripl))
	(let ((trip (t-r-match-test-clause tripl)))
	   (let ((cxt (t-r-match-context trip)))
	      (cond ((eq cxt context)
		     (let ((fun-name (t-r-match-res-fun-sym trip)))
			(cond ((not (assq fun-name defs))
			       (setq defs
				     (cons (res-fun-def
					      fun-name
					      (Rep-clause-stuff
					         (t-r-match-result-clause trip))
					      collectors
					      col-synonyms)
					   defs))))))))))
      defs))
					  
(defun res-fun-def (fun-name fun-body collectors col-synonyms)
   ;; We have to bind new vars to the collectors to avoid an
   ;; infinite 'macrolet' loop.
   `(,fun-name ()
	(let ,(mapcar #'tuple col-synonyms collectors)
	   (declare (ignorable ,@col-synonyms))
	   (symbol-macrolet 
	      ,(mapcar (\\ (coll syn)
			  `(,coll (col_elts ,syn)))
		       collectors col-synonyms)
	      ,fun-body))))

(datafun-alist within-sublis-handlers* within-sublis)

(defun rep-clauses->loop-body (clauses standard-vars collectors
			       test-result-map)
   (let ((end-tag false))
      (let-fun ((:def build (cl)
;;;;		   (trace-around build
;;;;                      (:> "(build: " cl ")")
                   (cond ((null cl)
			  '())
			 (t
			  (let ((clause (car cl))
				(rem (build (cdr cl))))
			     (let ((stuff (Rep-clause-stuff clause))
				   (mode (Rep-clause-mode clause)))
				(case mode
				   (:do
				    `(,@stuff ,@rem))
				   (:result
				    ;; These are handled in the tests
				    ;; corresponding to them
				    rem)
				   ((:while :until)
				    (cond ((is-String stuff)
					   rem)
					  (t
					   (build-test mode stuff clause rem))))
				   (:when
				    (cond ((not end-tag)
					   (setq end-tag (gensym))))
				    `((cond ((not ,stuff) (go ,end-tag)))
				      ,@rem))
				   ((:collect :nconc :append)
				    (let ((colvar (or (car stuff)
						      (car collectors))))
				       (cond ((not (memq colvar collectors))
					      (error "Undeclared collector in ~
						      ~s ~s"
						     mode 
						     `(:into ,colvar
							     ,(cadr stuff)))))
				       `((list-collect
					    ,colvar
					    ,(case mode
						(:nconc (cadr stuff))
						(:append
						 `(list-copy ,(cadr stuff)))
						(t `(list ,(cadr stuff)))))
					 ,@rem)))
				   ((:within)
				    `(,(within-continue-subst 
					  (cadr stuff) (car stuff)
					  test-result-map collectors)
				      ,@rem))
				   (t
				    (error "Bizarre 'repeat' clause ~s"
					   clause)))))))
;;;;                      (:< (val &rest _) "build: " val))
                   ))
	 (let ((prebumps (build clauses))
	       (bumps 
		  (let ((setqs (mapcan #'repeat-var-implied-bump
				       standard-vars)))
		     (cond ((null setqs) '())
			   (t
			    `((psetq ,@(apply #'append setqs))))))))
	    (cond (end-tag
		   `((tagbody
			,@prebumps
			,end-tag
			,@bumps)))
		  (t
		   `(,@prebumps ,@bumps))))

       :where

	 (:def build-test (mode stuff clause rem)
;;;;	    (trace-around build-test
;;;;               (:> "(build-test: " mode 1 stuff 1 clause
;;;;                   :% rem ")")
            `((cond (,(cond ((eq mode ':until)
			     `(not ,stuff))
			    (t stuff))
		     ,@rem)
		    (t
		     ,(let ((trip (assq clause test-result-map)))
			 (cond (trip
				`(return (,(t-r-match-res-fun-sym trip))))
			       (t
				(error "Fumbled result clause: ~s"
				       clause)))))))
;;;;               (:< (val &rest _) "build-test: " val))
            )

	 (:def within-continue-subst (sub-conts exp test-result-map collectors)
            (let ((res-val
	             (within-sublis
                        (mapcar
                           (\\ (wc)
;;;;                              (trace-around hack-sub-cont
;;;;                                 (:> "(hack-sub-cont: " wc ")")
                              (list (Within-clauses-continue wc)
                                    `(labels ,(res-fun-definitions
                                                 test-result-map collectors
                                                 (Within-clauses-continue wc))
                                        ,@(build
                                             (Within-clauses-subclauses
                                              wc))))
;;;;                                 (:< (val &rest _) "hack-sub-cont: " val))
                              )
                           sub-conts)
                        exp)))
;;;;               (dbg-save test-result-map collectors sub-conts test-result-map
;;;;                         exp res-val)
;;;;               (breakpoint within-continue-subst
;;;;                  "after within-sublist, result = " res-val)
               res-val))

	 (:def within-sublis (alist exp)
;;;;	     (trace-around within-sublis
;;;;		(:> "(within-sublis: " alist :% 2 exp ")")
	     (cond ((atom exp) exp)
		   (t
		    (let ((h (alref within-sublis-handlers* (car exp))))
		       (cond (h
			      (funcall h alist exp #'within-sublis))
			     (t
			      (mapcar (\\ (e) (within-sublis alist e))
				      exp))))))
;;;;		(:< (val &rest _) "within-sublis: " val))
	     ))))

(datafun within-sublis quote
   (defun :^ (_ exp _) exp))

(datafun within-sublis repeat quote)

(datafun within-sublis :continue
   (defun :^ (alist exp _)
      (let ((e (assq exp alist)))
	 (cond (e (cadr e))
	       (t
		(error "No entry for :continue ~%  ~s~% in alist~%  ~s"
		       exp alist))))))
					    
(defun repeat-var-implied-bindings (rep-var)
   (let ((varname (Rep-var-name rep-var))
	 (alist (Rep-var-alist rep-var)))
      (let ((init (let ((p (lookup-rep-var-prop 'init alist)))
		     (cond (p (Rep-var-prop-val p))
			   (t 'false)))))
	 (case (Rep-var-mode rep-var)
	    ((*step)
	     (let ((limvar (lookup-rep-var-prop 'limvar alist))
		   (stepvar (lookup-rep-var-prop 'stepvar alist)))
		`((,varname ,init)
		  ,@(include-if stepvar `(,(Rep-var-prop-val stepvar)
					  ,(Rep-var-prop-val
					      (lookup-rep-var-prop ':by alist))))
		  ,@(include-if limvar `(,(Rep-var-prop-val limvar)
					 ,(Rep-var-prop-val
					     (lookup-rep-var-prop ':to alist)))))))
	    ((*simple *reset)
	     `((,varname ,init)))
	    (*throughlist
	     (let* ((tailvar (Rep-var-prop-val
                                 (lookup-rep-var-prop ':tail alist)))
                    (initexp
                        (let ((rvp (lookup-rep-var-prop ':initbind alist)))
                          (cond (rvp
                                 (cond ((eq varname '_)
                                        (cerror "I'll ignore the initialization"
                                                  !":init field supplied for ~
                                                    don't-care var _: ~s"
                                                  (Rep-var-prop-val rvp))
                                        nil)
                                       (t
                                        (Rep-var-prop-val rvp))))
                                (t 'false)))))
	        `((,tailvar ,init)
		  ,@(include-if (not (eq varname '_))
		       `(,varname ,initexp)))))
	    (*each-iter
	     (let ((iterfcnvar (Rep-var-prop-val
                                  (lookup-rep-var-prop 'iterfcnvar alist))))
	        `((,iterfcnvar (\\ () ,init))
		  (,varname nil))))
	    (t
	     (error "Undecipherable 'repeat' var spec ~s" rep-var))))))

(defun repeat-var-implied-front (rep-var)
   (let ((varname (Rep-var-name rep-var))
	 (alist (Rep-var-alist rep-var)))
      (case (Rep-var-mode rep-var)
	 (*simple '())
	 (*throughlist
	  (let ((tailvar (Rep-var-prop-val (lookup-rep-var-prop ':tail alist))))
	     (cons (make-Rep-clause
		      ':until -1 `(null ,tailvar))
		   (cond ((eq varname '_)
			  '())
			 (t (list
			       (make-Rep-clause
				  ':do -1
				  (list
				     `(setq ,varname (car ,tailvar))))))))))
	 (*step
	  (let ((lim (lookup-rep-var-prop 'limvar alist))
		(step (lookup-rep-var-prop ':by alist)))
	     (cond (lim
		    (list (make-Rep-clause
			     ':while -1
			     `(,(cond ((and step
					    (is-Number (Rep-var-prop-val step))
					    (< (Rep-var-prop-val step) 0))
				       '>=)
				      (t '=<))
			       ,varname ,(Rep-var-prop-val lim)))))
		   (t '()))))
	 (*reset '()
;;;;	  (list (make-Rep-clause
;;;;		   ':do -1 (list `(setq ,varname
;;;;				        ,(Rep-var-prop-val (lookup-rep-var-prop ':then alist))))))
	  )
	 (*each-iter
	  (list (make-Rep-clause
		   ':do -1
		   (list `(setq ,varname
			        (funcall ,(Rep-var-prop-val (lookup-rep-var-prop 'iterfcnvar alist))))))))
	 (t
	  (error "Undecipherable 'repeat' var spec ~s" rep-var)))))

(defun repeat-var-implied-bump (rep-var)
   (let ((varname (Rep-var-name rep-var))
	 (alist (Rep-var-alist rep-var)))
      (case (Rep-var-mode rep-var)
	 (*simple '())
	 (*throughlist
	    (let ((tailvar (Rep-var-prop-val (lookup-rep-var-prop ':tail alist))))
	       `((,tailvar (cdr ,tailvar)))))
	 (*step
	    (let ((stepamt (or (lookup-rep-var-prop 'stepvar alist)
			       (lookup-rep-var-prop ':by alist))))
	       `((,varname (+ ,varname ,(cond (stepamt (Rep-var-prop-val stepamt))
					      (t '1)))))))
	 (*reset
	    `((,varname ,(Rep-var-prop-val (lookup-rep-var-prop ':then alist)))))
	 (*each-iter '())
	 (t
	  (error "Undecipherable 'repeat' var spec ~s" rep-var)))))
	    
;;; 'expected' is a list of keywords, although an element can also be
;;; of form (k1 k1' ...), where
;;; k1 is the "normal" form and k1'... are alternatives, if any.
;;; Keywords do not have to be in the keyword package, but do
;;; have to be symbols.
;;; Each keyword can occur only once.
;;; 'offset' is position of item *after* first keyword
(defun keyword-args->alist (args expected
			    &key (offset 1) (allow-others false))
   (let ((keyword-tab
	    (mapcan (\\ (k)
		       (cond ((atom k) (list (tuple k k)))
			     (t
			      (mapcar (\\ (k1)
					 (tuple k1 (car k)))
				      k))))
		    expected)))
      (do ((specl args (cddr specl))
	   (position offset (+ position 2))
	   (alist !(Rep-var-prop)))
	  ((or (null specl) (null (cdr specl)))
	   (cond ((not (null specl))
		  (cerror        "I'll ignore it"
		     "Keyword at end of keyword-args list: ~s"
		     args)))
	   alist)
	 (let ((sym (car specl))
	       (a (cadr specl)))
	    (cond ((is-Symbol sym)
		   (let ((s (assq sym keyword-tab)))
		      (cond (s
			     (setq s (cadr s)))
			    (allow-others
			     (setq s sym))
			    (t
			     (cerror      "I'll ignore it"
			        "Illegal keyword ~s (expected ~s) in ~s"
				sym expected args)))
		      (let ((p (lookup-rep-var-prop s alist)))
			 (cond (p
				(cerror       !"I'll ignore ~
					        all but the ~
					        first"
				  "Redundant ~
				   specification of ~
				   ~s in ~s"
				  sym args))
			       (t
				(setq alist
				      (cons (make-Rep-var-prop
					       s a position)
					    alist)))))))
		  (t
		   (error "Illegal keyword ~s in ~s"
			  sym args)))))))

;;; 'tab' is a list of Test-result-match's.
;;; Find element of tab whose 'result-clause' is res; return false
;;; if not there.
(defun result-map-lookup (res tab)
   (do ((cla tab (cdr cla)))
       ((or (null cla)
	    (eq (t-r-match-result-clause (car cla))
		res))
	(cond ((null cla) false)
	      (t (car cla))))))

)

;;; Used by macros that sneak through pieces of a 'repeat' and then
;;; must put it back together.
(defun repeat-reassemble (standard-vars collectors decls
			  local-fundefs clauses)
   (letrec ()
      `(repeat ,@(cond ((and (null standard-vars) (null collectors))
			'())
		       (t
			`(:for (,@(standards-reassemble)
				,@collectors))))
	  ,@decls
	  ,@(mapcan #'repeat-clause-reassemble clauses)
	  ,@(cond ((null local-fundefs)
		   '())
		  (t
		   `(:where ,@local-fundefs))))
    :where
       (standards-reassemble ()
	  (mapcar
	     (\\ (sv)
	        (let ((alist (Rep-var-alist sv)))
		   (let ((init (lookup-rep-var-prop 'init alist)))
		      `(,(Rep-var-name sv)
			,@(case (Rep-var-mode sv)
			     (*throughlist
			      (let ((tailvar (lookup-rep-var-prop ':tail alist))
				    (initexp (lookup-rep-var-prop ':initbind alist)))
				 `(:in ,(second init)
				       ,@(cond (tailvar
						`(:tail ,(second tailvar)))
					       (t '()))
				       ,@(cond (initexp
						`(:initbind ,(second initexp)))
					       (t '())))))
			     (*reset
			      (let ((next (cadr (lookup-rep-var-prop
						   ':then alist))))
				 `(= ,(cadr init) :then ,next)))
			     (*step
			      (let ((by (lookup-rep-var-prop ':by alist))
				    (to (lookup-rep-var-prop ':to alist)))
				 `(= ,(cadr init)
				     ,@(cond (by `(:by ,(cadr by)))
					     (t '()))
				     ,@(cond (to `(:to ,(cadr to)))
					     (t '())))))
			     (*each-iter
			      `(= ,(cadr init) :then :again))
			     (t
			      (cond (init `(,(cadr init)))
				    (t '()))))))))
	       standard-vars))))

(defun repeat-clause-reassemble (cl)
	  (let ((mode (Rep-clause-mode cl))
		(stuff (Rep-clause-stuff cl)))
	     (case mode
		(:do (list-copy stuff))
		((:collect :nconc :append)
		 `(,mode
		   ,(cond ((car stuff) `(:into ,(car stuff) ,(cadr stuff)))
			  (t (cadr stuff)))))
		((:while :until :when :result)
		 `(,mode ,stuff))
		((:within)
		 `(:within ,(car stuff)))
		(t
		 (error "Unhandlable clause ~s" cl)))))
