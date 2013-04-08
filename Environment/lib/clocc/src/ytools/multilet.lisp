;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: multilet.lisp,v 2.5 2006/12/01 17:46:16 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on %ytools/ setter signal misc)

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(multi-let with-open-files gen-var with-gen-vars
	     keyword-args-extract
	     control-nest track-extra-vals extra-vals)))

(eval-when (:compile-toplevel :load-toplevel :execute)
   (defvar multi-let-notify* false)

   (defvar macro-exp-level* 5)

   (defun macro-exp-notify (macro-announce exp gate)
      (bind ((*print-pretty* true)
             (*print-level* macro-exp-level*))
         (dbg-out gate
            (:a macro-announce)
            :% exp :%))
      exp)
)

;;; Because the naming convention for the new variables has changed,
;;; with have to check for the old convention
(defmacro with-gen-vars (var-roots &body body)
   (let ((old-syms
             (<# (\\ (var-root) (build-symbol - (:< var-root) -))
                 var-roots))
         (new-syms
             (<# (\\ (var-root) (build-symbol (:< var-root) "$"))
                 var-roots)))
      (cond ((exists (old :in old-syms)
                (occurs-in old body))
             (out (:to *error-output*)
                "Warning: Deprecated gen-var names " old-syms
                :% " are being translated to new form " new-syms
                :% " in " body :%)
             (!= body (sublis (<# cons old-syms new-syms)
                              *-*))))
      `(let ,(<# (\\ (sym-var var-root)
                    `(,sym-var
                      (gen-var ',var-root)))
                 new-syms
                 var-roots)
          ,@body)))

(defmacro redundant-args-check (arg-alist-var form^)
   (cond ((not (is-Symbol arg-alist-var))
          (out (:to *error-output*)
             "Warning -- first argument '" arg-alist-var
             "' to 'redundant-args-check' should be a variable!" :%)))
   (let ((form-var (gen-var 'form)))
      `(cond ((not (null (tail ,arg-alist-var)))
              (let ((,form-var ,form^))
                 (signal-problem :noplace
                    "The following args to '" (car ,form-var)
                    " are not supposed to occur at the same"
                    " time: " (<# first ,arg-alist-var)
                    (:proceed "I'll ignore all but "
                              (first (head ,arg-alist-var)))))))))


;;; (multi-let (((v v v ...) form)
;;;                   ((v v v ...) form)
;;;                   ...)
;;;     -body-)
(defmacro multi-let (bindspecs &body b)
   (let ((bindspecs (multi-let-bindspecs-analyze bindspecs true b)))
      (let ((bvars
	       (<# car bindspecs))
	    (explicit-ignores '())
	    b1)
	 (cond ((matchq ((ignore ?@explicit-ignores) ?@b1)
			b)
		(setq b b1)))
	 (cond ((or (>= debuggability* 0) (= (len bindspecs) 1))
                (simple-multi-let bvars bindspecs explicit-ignores b))
	       (t
                (macro-exp-notify
                   "multi-let expands to hairy, optimized version: "
                   (hairy-multi-let bvars bindspecs explicit-ignores b)
                   multi-let-notify*))))))

(needed-by-macros

;;; This needs to be augmented with positions at some point
;;; 'check-syntax' is false if we're in decl mode and it will be checked
;;; by someone else.
(defun multi-let-bindspecs-analyze (bindspecs check-syntax body)
   (let ((standardized  
	    (<# (\\ (bs)
		   (match-cond bs
		      ((atom bs)
		       `((,bs) nil))
		      ?( (() ?_)
			bs)
		      ?( ((?_ ?@_) ?_)
			bs)
		      ?( (?(:+ ?v atom) ?@(:& ?val ?(:\| () (?_))))
			`((,v) ,@val))
		      (t
		       (signal-problem multi-let
			  "Ill-formed: " bs
			  " in: "
			  `(multi-let ,bindspecs ,@body)))))
		bindspecs)))
      (cond (check-syntax
	     (repeat :for ((bs :in standardized :tail bsl))
		(repeat :for ((v :in (car bs)))
		   (cond ((not (is-Symbol v))
			  (signal-problem multi-let
			     "Illegal variable " v " in " t
			     `(multi-let ,bindspecs ,@body)))
			 ((and (not (eq v '_))
			       (exists (other :in (cdr bsl))
				  (memq v (car other))))
			  (signal-problem multi-let
			     "Variable " v " is bound twice in" t
			     `(multi-let ,bindspecs ,@body))))))))
      standardized))

(defun simple-multi-let (bvars-with-underscores bindspecs explicit-ignores b)
   (multiple-value-let
          (bvars ign)
	  (let-fun ((bvars-elim-underscores (bll)
		      (cond ((null bll)
			     (values '() '()))
			    (t
			     (multiple-value-let (bla iga)
						 (underscores-elim (car bll))
				(multiple-value-let (bld igd)
						    (bvars-elim-underscores
						       (cdr bll))
				   (values (cons bla bld)
					   (cons iga igd))))))))
	     (bvars-elim-underscores bvars-with-underscores))
      (cond ((>= debuggability* 0)
             (let ((expansion
	              `(<< (\\ ,(<< append bvars) 
                               ,@(let ((all-ign (append (<< nconc ign)
                                                        explicit-ignores)))
                                    (ignore-if-not-null all-ign))
                               ,@b)
                           (nconc ,@(<# (\\ (m vl)
                                           `(value-list-check
                                               (multiple-value-list ,m)
                                               ',vl ',m))
                                        (<# cadr bindspecs)
                                        bvars-with-underscores)))))
                (macro-exp-notify
                   "multi-let expands to simple, debuggable version: "
                   expansion multi-let-notify*)))
	    ;; Go for efficiency.  (We know there's just one bindspec.)
	    (t
             (let ((expansion
	              (let ((vl (car bvars))
                            (arg (cadar bindspecs))
                            (ign1 (ignore-if-not-null
                                     (append explicit-ignores (car ign)))))
                         (cond ((= (len vl) 1)
                                `(let ((,(car vl)
                                        ,arg))
                                      ,@ign1
                                    ,@b))
                               (t
                                `(multiple-value-let ,vl ,arg
                                       ,@ign1
                                    ,@b))))))
               (macro-exp-notify
                  "multi-let expands to optimized version"
                  expansion multi-let-notify*))))))

(defun hairy-multi-let (bvars-with-underscores bindspecs explicit-ignores b)
   (let ((auxvars (<# (\\ (vl)
			 (<# (\\ (_) (gensym))
			    vl))
		      bvars-with-underscores)))
      (let (;; Each element of auxvars is split into
	    ;; the corresponding element of used-auxvars
	    ;; and ign-auxvars
	    (used-auxvars
	       (<# (\\ (auxl bvl_)
		      (<! (\\ (av bv_)
			     (cond ((eq bv_ '_)
				    '())
				   (t (list av))))
			  auxl bvl_))
		   auxvars
		   bvars-with-underscores))
	    (ign-auxvars
	       (<# (\\ (avl bvl_)
		      (nconc (<! (\\ (av bv_)
				    (cond ((eq bv_ '_)
					   (list av))
					  (t '())))
				 avl bvl_)
			     ;; ... but we also include the auxvars
			     ;; corresponding to explicit-ignores
			     (<! (\\ (av bv_)
				    (cond ((memq bv_ explicit-ignores)
					   (list av))
					  (t '())))
				 avl bvl_)))
		   auxvars
		   bvars-with-underscores))
	    (used-bvars
	       (<# (\\ (bvl_)
		      (<? (\\ (bv) (not (eq bv '_)))
			  bvl_))
		   bvars-with-underscores)))
	 (let-fun ((nest (auxl ign-auxl bl)
		     (cond ((null bl)
			    `(let ,(<! (\\ (vl auxl)
					  (<! (\\ (v av)
						 (cond ((memq
							   v explicit-ignores)
							'())
						       (t
							(list (tuple v av)))))
					      vl auxl))
				       used-bvars used-auxvars)
					  ,@b))
			   (t
			    `(multiple-value-let ,(car auxl)
						 ,(cadar bl)
						 ,@(ignore-if-not-null
						      (car ign-auxl))
				,(nest (cdr auxl)
				       (cdr ign-auxl)
				       (cdr bl)))))))
	    (nest auxvars ign-auxvars bindspecs)))))
      
(defun ignore-if-not-null (ign)
   (include-if (not (null ign)) `(declare (ignore ,@ign))))

(defun value-list-check (vals vars form)
   (cond ((= (len vals) (len vars))
	  vals)
	 (t
	  (error-break value-list-check
	     "Wrong number of arguments.  Wanted " vars
	     :% " got " vals
	     :% " as value of " form
	     (:novalue "I will ignore some vars or some vals"))
	  (<# (\\ (v _)
		 v)
	      vals vars))))

;;;;(def-op name-lookup (env name)
;;;;   (error-break name-lookup
;;;;      "Can't find " name " in environment " env
;;;;      :fatal))

)

;;; (with-open-files ((srmvar1 ...)
;;;                   (srmvar2 ...)
;;;                   ...)
;;;     -body-)
;;; is an abbreviation for
;;; (with-open-file (srmvar1 ...)
;;;    (with-open-file (srmvar2 ...)
;;;        (...
;;;          -body-)))
(defmacro with-open-files (bdgs &body body)
   (let-fun ((build-it (bdgs)
		(cond ((null bdgs) body)
		      (t
		       (let ((inner (build-it (cdr bdgs))))
			  `((with-open-file ,(car bdgs)
			       ,@inner)))))))
      (let ((e (build-it bdgs)))
	 (cond ((= (len e) 1)
		(car e))
	       (t `(progn ,@e))))))


;;; The form (track-extra-vals :extra-vars <bdgs>
;;;			       [:principal <vars>] 
;;;			       [:values <exps>]
;;;            -body-)
;;; where <bdgs> is a list ((var1 val1) ... (varK valK))
;;; <vars> is a list of variables (default: a new variable)
;;; and <exps> is a list of expressions (default: (<vars> <vars-from-bdgs>))
;;; binds the <bdgs>, evaluates the body, binds <vars> to the results,
;;; and returns <exps> as multiple values.
;;; It is expected that the -body- will contain occurrences of
;;; 'extra-vals' (see below).
;;; The keywords can come anywhere; in particular, :values can come
;;; after the body.
(defmacro track-extra-vals (&whole form &rest stuff)
   (multiple-value-let (remainder alist)
		       (keyword-args-extract stuff '(:extra-vars :extra
                                                     :principal-values
                                                     :principal
                                                     :num-principal-values
                                                     :extra-values
                                                     :values))
;;;;      (macrolet (((

      (let ((p-entries (all-alist-entries '(:principal-values :principal
                                            :num-principal-values)
                                          alist))
            (r-entries (all-alist-entries '(:extra-vars :extra)
                                          alist))
            (l-entries (all-alist-entries '(:extra-values :values)
                                          alist)))
         (let* ((specified-vals 
                   (cond ((null l-entries) ':unspecified)
                         (t
                          (second (head l-entries)))))
                (p-vars (cond ((null p-entries)
                               (cond ((and (not (null l-entries))
                                           (null (second (head l-entries))))
                                      !())
                                     (t (list (gen-var 'p)))))
                              (t
                               (redundant-args-check p-entries form)
                               (cond ((eq (first (head p-entries))
                                          ':num-principal-values)
                                      (<# (\\ (_) (gen-var 'p))
                                          (series (second (head p-entries)))))
                                     (t
                                      (second (head p-entries)))))))
                (e-bdgs (cond ((null r-entries)
                               (signal-problem track-extra-vals
                                  "No :extra-vars specified"
                                  (:proceed "I'll proceed, but you probably don't"
                                            " need 'track-extra-vals'"))
                               '())
                              (t
                               (redundant-args-check r-entries form)
                               (second (head r-entries)))))
                (vals (cond ((eq specified-vals ':unspecified)
                             (cond ((null p-vars)
                                    ':hide)
                                   (t
                                    (append p-vars
                                            (<# (\\ (b) (cond ((consp b) (first b))
                                                              (t b)))
                                                e-bdgs)))))
                            (t
                             (redundant-args-check l-entries form)
                             (ecase (first (head l-entries))
                                (:extra-values
                                 (cond ((and (null specified-vals)
                                             (null p-vars))
                                        ':hide)
                                       (t
                                        (append p-vars specified-vals))))
                                (:values
                                 specified-vals))))))
;;;            (out "specified-vals = " specified-vals " vals = " vals :%)
            (let ((exp (cond ((= (len remainder) 1)
                              (car remainder))
                             (t 
                              `(progn ,@remainder))))
                  (val-exp-list (cond ((eq vals ':hide) !())
                                      (t `((values ,@vals))))))
	       `(let ,e-bdgs
		   ,@(case (len p-vars)
                        (0 `(,exp ,@val-exp-list))
                        (1 `((let ((,(first p-vars)
                                    ,exp))
                                (declare (ignorable ,@p-vars))
                                ,@val-exp-list)))
                        (t `((multi-let ((,p-vars ,exp))
                                (declare (ignorable ,@p-vars))
                                ,@val-exp-list))))))))))

;;;;(defmacro track-extra-vals (&whole tev-exp
;;;;			    resvar\(s\) k init-bindings expr^ &body body^)
;;;;   (cond ((and (eq k ':extra)
;;;;	       (or (is-Symbol resvar\(s\))
;;;;		   (is-list-of resvar\(s\) #'is-Symbol)))
;;;;	  `(multi-let ,init-bindings
;;;;	      (multi-let ((,resvar\(s\) ,expr^)))
;;;;		 ,@body^))
;;;;	 (t
;;;;	  (signal-problem track-extra-vals
;;;;	     "Ill-formed: " tev-exp
;;;;	     :% " Should be of form (track-extra-vals <vars> :extra <bdgs>"
;;;;	        " expression --body--)"))))

(defmacro extra-vals (&whole form exp^ &rest stuff)
;;;;   (out "exp^ = " exp^ " stuff = " stuff :%)
   (multiple-value-let (stuff alist)
                       (keyword-args-extract stuff '(:after))
;;;;      (out "stuff = " stuff  
;;;;           " alist = " alist :%)
      (multiple-value-let (stuff explicit-accums)
                          (repeat :for ((e :in stuff :tail stl))
                           :result (values stuff false)
                           :until (memq e '(:+ :&))
                           :result (values (ldiff stuff stl)
                                           (cdr stl)))
;;;;         (out "stuff = " stuff " explicit-accums = " explicit-accums
;;;;              " alist = " alist :%)
         (cond ((> (len stuff) 2)
                (signal-problem extra-vals
                   "Ill-formed (too much stuff): " form
                   (:proceed "I'll ignore the extra stuff"))))
         (multiple-value-let (vars exp^ accums)
                             (cond ((null stuff)
                                    (values !() exp^ false))
                                   (t
                                    (values exp^
                                            (first stuff)
                                            (cond ((= (len stuff) 1)
                                                   false)
                                                  (t (second stuff))))))
            (cond ((not (null explicit-accums))
                   (cond (accums
                          (signal-problem extra-vals
                             "Ill-formed (meaningless occurrence of " accums
                             :% " in " form
                             (:proceed "I'll ignore that meaningless occurrence"))))
                   (!= accums explicit-accums)))
            (let ((main-res-vars
                     (<# (\\ (_) (gen-var 'r))
                         (series (alref alist ':after 1))))
                  (extra-vars
                     (cond ((null vars)
                            (<# (\\ (_) (gen-var 'extra))
                                accums))
                           (t
                            vars)))
                  (update-vars (<# (\\ (_) (gen-var 'u))
                                   accums)))
;;;;              (out "extra-vars = " extra-vars
;;;;                   "accums = " accums " update-vars = " update-vars :%)
               ;; Get all accums in form (var [new-val])
               (!= accums (<# (\\ (a)
                                 (cond ((atom a)
                                        `(,a))
                                       ((is-Keyword (car a))
                                        (cdr a))
                                       (t a)))
                              *-*))
               ;; If new-val is missing, it defaults to the corresponding
               ;; extra-var.  Make sure this makes sense --
;;;;               (out "accums = " accums " extra-vars = " extra-vars :%)
               (cond ((and (> (len accums) (len extra-vars))
                           (exists (acc :in (drop (len extra-vars)
                                                  accums))
                              (null (cdr acc))))
                      (signal-problem extra-vals
                         "Extra vars have ill-defined updaters: "
                         (<? (\\ (acc) (null (cdr acc)))
                             (drop (len extra-vars)
                                   accums)))))
               (let ((accum-vars (<# car accums))
                     (new-vals
                        (<# (\\ (a exv)
                               (cond ((null (cdr a))
                                      exv)
                                     (t (cadr a))))
                            accums extra-vars)))
                  `(multi-let (((,@main-res-vars ,@extra-vars)
                                ,exp^))
                      (let ,(<# (\\ (upd-var new-val) `(,upd-var ,new-val))
                                update-vars
                                new-vals)
                         ,@(<# (\\ (acc-var upd-var)
                                  `(!= ,acc-var ,upd-var))
                               accum-vars
                               update-vars)
                         (values ,@main-res-vars)))))))))

;;;;                      (match-cond acc
;;;;                         (:? (!= ?@_) acc)
;;;;                         (:? (?(:+ ?v is-Symbol) ?e)
;;;;                            `(!= ,v ,e))
;;;;                         (t (signal-problem extra-vals
;;;;                               "Ill-formed 'extra-vals' clause " acc)))


;;;;   (cond ((forall (a :in accums) (= (len a) 3))
;;;;	  ;; -- Each is of form (newval accumval accumulator)
;;;;	  (let ((main-res-var (gen-var 'r)))
;;;;	     `(multi-let (((,main-res-var ,@(<# car accums))
;;;;			   ,exp^))
;;;;		 ,@(<# (\\ (acc) `(!= ,(cadr acc) ,(caddr acc)))
;;;;		       accums)
;;;;		 ,gen-var)))
;;;;	 (t
;;;;	  (signal-problem extra-vals
;;;;	     "extra-vals expects a list of (newval accumval accumulator) triples,"
;;;;	     :% " not: " accums))))

;;;;     (control-nest :okay
;;;;                   (bind-something (what-to-bind-1)
;;;;		          (check-something-1 ...
;;;;		    	 :okay
;;;;		    	 ...))
;;;;		       (bind-something (what-to-bind-2)
;;;;		          (check-something-2 ...
;;;;		    	 :okay
;;;;		    	 ...))
;;;;                   done)
;;;;
;;;;is short for
;;;;
;;;;(bind-something (what-to-bind-1)
;;;;   (check-something-1 ...
;;;;      (bind-something (what-to-bind-1)
;;;;	     (check-something-2 ..
;;;;	         done ...))))


;;; Each elements of 'labeled-binding-exps' (except the first)
;;; is preceded by a keyword.  The binding-exp immediately before
;;; that keyword must contain exactly one occurrence of it.
;;; The macro substitutes for the keyword the binding exp labeled
;;; with it.  (Purpose: avoid deeply nexted expressions, especially
;;; when it isn't necessary for variable bindings to be vividly
;;; apparent.)
(defmacro control-nest (&rest labeled-binding-exps)
   (let-fun ((fold (lbes)
		;; -- lbes is tail of 'labeled-binding-exps'
	        ;; starting with a non-label.
		(cond ((null (cdr lbes))
		       (car lbes))
		      ((or (null (cddr lbes))
			   (not (is-Symbol (cadr lbes))))
		       (signal-problem control-nest
			  "Ill-formed labeled-binding-exp in control-nest: "
			  :% 1 (cdr lbes)
			  (:proceed
			     "I'll drop remaining labeled-binding-exps"))
		       (car lbes))
		      (t
		       (cond ((not (= (count-occs (cadr lbes) (car lbes))
				      1))
			      (signal-problem control-nest
				 "Expression should have exactly one"
				 " occurrence of " (cadr lbes) ": "
				 :% (car lbes)
				 (:proceed "I'll replace all occurrences"))))
		       (subst (fold (cddr lbes))
			      (cadr lbes)
			      (car lbes))))))
      ;; Atoms at front are assumed to be comments
      (repeat :until (not (atom (car labeled-binding-exps)))
         (!= labeled-binding-exps (cdr *-*)))
      (cond ((null labeled-binding-exps)
	     (signal-problem control-next :fatal
		"control-nest has no forms to combine"))
	    (t (fold labeled-binding-exps)))
    :where
       (:def count-occs (sym e)
	  (cond ((atom e)
		 (cond ((eq e sym) 1)
		       (t 0)))
		(t
		 (</ (\\ (tot x)
			(+ tot (count-occs sym x)))
		     0 e))))))

;;; Returns alist of keywords and vals, plus what's left over
;;; In order: < leftovers, alist >
(defun keyword-args-extract (args keywords)
   (repeat :for ((al args)
		 :collector pairs remainder)
;;;;      (out "al = " al :%)
    :until (null al)
    :result (values remainder pairs)
    :within
      (let ((a (car al)))
	 (cond ((memq a keywords)
		(:continue
		 :collect (:into pairs (list a (cadr al)))
		   (!= al (cddr al))))
	       (t
		(:continue
		 :collect (:into remainder a)
		   (!= al (cdr al))))))))

(defun all-alist-entries (syms alist)
   (repeat :for ((s :in syms))
    :within (let ((e (assq s alist)))
               (:continue
                :when e
                :collect e))))


;;;;   (let ((occurs (<? (\\ (s) (assq s alist)))))
;;;;      (cond ((null occurs)
;;;;             (funcall if-absent))
;;;;                    (signal-problem check-for-duplicate-alist-entries
;;;;                       "In " context
;;;;                       :% "  one of the following must occur: " syms))
;;;;                   (t false)))
;;;;            (t
;;;;             (cond ((not (null (tail occurs)))
;;;;                    (signal-problem check-for-duplicate-alist-entries
;;;;                       "In " context
;;;;                       :% "  you can't have occurrences of all of: "occurs
;;;;                       (:proceed "I'll ignore all but the first"))))
;;;;             (funcall if-found (head occurs))))))

