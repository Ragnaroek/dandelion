;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: setter.lisp,v 2.10 2006/06/28 22:56:58 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on (:at :compile-time :run-time :slurp-time)
	    %ytools/ binders repeat
	    :at-run-time %ytools/ outin)

;;;;(eval-when (:compile-toplevel :load-toplevel :slurp-toplevel)
;;;;   (set-dispatch-macro-character
;;;;      '#\! '#\= #'treat-excl-as-char ytools-readtable*)
;;;;   (set-dispatch-macro-character
;;;;      '#\! '#\! #'treat-excl-as-char ytools-readtable*)
;;;;)

(eval-when (:compile-toplevel :load-toplevel)
   (export '(!= !=/ *-* switch matchq matches match-cond match-let *unbound
	     make-Qvaroid make-Qvar is-Qvar is-Qvaroid Qvar-sym Qvar-notes Qvar
             setter <-this-val ^-this-val retrieve-val)))

;;;;(declaim (special *-*))

(defmacro != (lft &rest rgt)
  ; Special case: (!= < -vars- > ) means multiple-value-setq.
  (cond ((eq lft '<)
	 (multiple-value-!= rgt))
        ((and (consp lft) (eq (car lft) '<))
         `(spreadem ,@(cdr lft) ,@rgt))
	((null rgt)
	 (error "!= with no right-hand-side: ~s" `(!= ,lft)))
	(t
	 (cond ((not (null (cdr rgt)))
		(cerror "I will discard the extra expressions"
			"Too many expressions on right-hand-side: ~s"
			`(!= ,lft ,@rgt))))
	 (setq rgt (car rgt))
	 (let-fun ()
	    (cond ((*-*-occurs-in rgt)
		   (multiple-value-let (vars vals newlftvar
					writer-form reader-form)
		                       (get-setf-expansion lft)
		      ;; There's just one --
		      (setq newlftvar (car newlftvar))
		      `(let* (,@(mapcar #'list vars vals)
			      (*-* ,reader-form)
			      (,newlftvar ,rgt))
			  ,writer-form)))
		  (t
		   `(setf ,lft ,rgt)))
	  :where
	     (*-*-occurs-in (exp)
		(cond ((atom exp) (eq exp '*-*))
		      ((memq (car exp) '(quote !=)) false)
		      (t
		       (some #'*-*-occurs-in exp))))))))

(defun multiple-value-!= (stuff)
   (let ((r (memq '> stuff)) vars vals)
      (cond ((null r)
	     (setf vars (butlast stuff))
	     (setf vals (lastelt stuff))
	     (out (:to *error-output*) 0
		  "Warning-- '>' missing in " `(!= < . ,stuff)
		  :% "   To be treated as " `(!= < ,@vars > ,vals)))
	    (t
	     (setf vars (ldiff stuff r))
	     (setf vals (cond ((= (len (cdr r)) 1) (cadr r))
			      (t `(values . ,(cdr r)))   )))   )
      (multiple-value-let (realvars new-ignores)
			  (underscores-elim vars)
	 (cond ((null new-ignores)
		`(multiple-value-setq ,vars ,vals))
	       (t
		(let ((used-vars (mapcan (\\ (v) (cond ((eq v '_) '())
						       (t (list v))))
					 vars)))
		   `(multiple-value-setq
			,used-vars
		        (multiple-value-let ,realvars ,vals
					    (ignore ,@new-ignores)
			   (values ,@used-vars)))))))))

(defvar was* nil)
(defvar now* nil)

(defvar print-was* (printable-as-string "was"))

(defvar print-now* (printable-as-string "now"))

(defmacro !=/ (lft rgt)
   (let ((oldvalform (cond ((is-Symbol lft)
			    `(cond ((boundp ',lft) ,lft) 
				   (t '*unbound)))
			   (t lft))))
      `(progn (setq was* ,oldvalform)
	      (setq now* ,rgt)
	      (!= ,lft now*)
	      `((,print-was* ,was*) (,print-now* ,now*)))))

;; More efficient than MATCHQ when you know it will match.
;; Saying (!= (< var1 var2 ... >) list) sets the vars to the
;; corresponding parts of the list.
(defmacro spreadem (&rest exp)
   (let ((l (remove '> exp :count 1)))
      (let ((places (butlast l)))
	 (cond ((null places)
		(error "No places to put list elements in: ~s"
		       `(!= (< ,@places >) ,(lastelt l))))
	       (t
		(let ((sym (gensym)))
   		   (setf l (lastelt l))
		   `(let ((,sym ,l))
		       (prog1 ,sym
			      ,@(mapcan (\\ (v)
					   (let ((munch `(setf ,sym (cdr ,sym))))
					      (cond ((eq v '_) (list munch))
						    (t
						     (list `(setf ,v (car ,sym))
							   munch)))))
					places)))))))))

(defmacro switch (t1 t2 &rest crud)
   (ignore crud)
   `(setf ,t1 (prog1 ,t2 (setf ,t2 ,t1)))   )

; Match macros: MATCHQ, MATCH-COND

(defstruct
   (Qvaroid (:constructor make-Qvaroid (atsign comma sym notes))
            (:predicate is-Qvaroid)
            (:print-function
	        (lambda (qv srm k)
		        (declare (ignore k))
		   (let ((sym (Qvaroid-sym qv))
			 (nl (Qvaroid-notes qv)))
		      (out (:to srm)
			   "?"
			   (:q ((Qvaroid-atsign qv) "@"))
			   (:q ((Qvaroid-comma qv) ","))
			   (:q ((null nl) sym)
			       (t
				(cons sym nl))))))))
   atsign comma   ;; booleans indicating presence of modifiers
   sym            ;; not necessarily a symbol
   notes)

(defmethod make-load-form ((qv Qvaroid) &optional env)
   (declare (ignore env))
   `(make-Qvaroid ',(Qvaroid-atsign qv)
		  ',(Qvaroid-comma qv)
		  ',(Qvaroid-sym qv)
		  ',(Qvaroid-notes qv)))		  

(eval-when (:slurp-toplevel :load-toplevel :execute)
   (set-macro-character
       '#\?
       (\\ (s _)
	  (multiple-value-let (atsign comma)
			      (repeat :for (ch (at false) (cm false))
				 (setq ch (peek-char nil s))
			       :while (member ch '(#\@ #\,)
					      :test #'char=)
				 (read-char s)
				 (cond ((char= ch '#\@)
					(setq at true))
				       (t
					(setq cm true)))
			       :result (values at cm))
	     (let ((x (read s)))
		(let-fun ()
		   (cond ((or comma (atom x))
			  (make-Qvaroid atsign comma x '()))
			 (t
			  (make-Qvaroid atsign comma (car x) (cdr x))))))))
       true
       ytools-readtable*))

;;; Qvars are a subspecies, but are distinguished only by their constructor.

(defun make-Qvar (x notes) (make-Qvaroid false false x notes))

(defun \? (x) (make-Qvar x !()))

;;; 
(deftype Qvar () 'Qvaroid)

(defun is-Qvar (x) (is-Qvaroid x))
;;; 
;;;;			(not (Qvaroid-atsign x))
;;;;			(not (Qvaroid-comma x))))

(defun Qvar-sym (x) (Qvaroid-sym x))
(defun Qvar-notes (x) (Qvaroid-notes x))

;;;;(defun is-matchpat (x) (is-Qvar x))
;;;;(defun matchpat-items (x) (cons (Qvar-sym x) (Qvar-notes x)))

; MATCHQ does unification between FORM, the first argument and FMLA, the
; second argument. A proper call to MATCHQ looks like
; (matchq (dir ?fra ?,frb ?,fra ?@frl) 
;         conj)
; The ?, signifies that the value of the following expression is to be used.
; The ?@ signifies that the variable must match all the rest of the
; form.

; You can also call (MATCHQ (?A . ?B) '(APPLE BANANA CHERRY)))
; and set A to APPLE and B to (BANANA CHERRY), but this form is deprecated.

; Any subexpression of the form ?(V -exps-) or (\?OR -exps-) will match 
; if one of of the exps does.  Similarly for ?(& ...) and (\?AND ...).

(defmacro matchq (pat &rest dat)
   (cond ((null dat) (setf dat 'match-datum))
	 (t (setf dat (car dat)))   )
   (cond ((atom pat)
	  (cond ((and (not (is-Qvaroid pat))
		      (not (eq dat 'match-datum)))
		 (out (:to *error-output*)
		      "Warning: matchq pattern is single atom: "
		      :% " (" 'matchq 1 pat 1 dat ")"
		      :% " you could have simply written "
		      :% " (" 'equal 1 dat " '" pat ")"
		      :% " [or use some other more specific equality tester]"
		      :% " If this matchq was generated by a macro, you can make the"
		      :% " warning go away by substituting ("
		      ':quote 1 pat ") for " pat :%))))
	 ((eq (car pat) ':quote)
	  (setq pat (cadr pat)))
	 ((eq (car pat) 'quote)
	  (cond ((not (eq dat 'match-datum))
		 (out (to *error-output*)
		      "Warning: matchq pattern is quoted: " pat
		      :% " To avoid seeing this warning, substitute (("
		      ':quote 1 'quote ") " (cadr pat) ")" :%)))))
   (match-code-cleanup
      `(let ((\ dat ,dat))
	  ,(match-code pat '\ dat))))

;;; Sometimes this is a lot more readable --
(defmacro matches (datum pattern)
   `(matchq ,pattern ,datum))

(needed-by-macros

(defun match-code (form dat-name)
  (cond ((atom form)
	 (atom-match-code form dat-name))
	((eq (car form) ':quote)
	 `(equalp ,dat-name ',(cadr form)))
	((is-constant-for-match form)
	 `(equalp ,dat-name ',form))
	(t
	 (cons-match-code form dat-name))))

(defun atom-match-code (form dat-name)
   (cond ((null form)
	 `(null ,dat-name))
	 ((is-Qvaroid form)
	  (qvaroid-match-code form dat-name))
	 ((is-String form)
	  `(and (is-String ,dat-name)
                (string= ,form ,dat-name)))
	 ((is-Number form)
	  `(and (is-Number ,dat-name)
		(= ,form ,dat-name)))
	 ((is-Symbol form)
	  `(eq ,dat-name ',form))
	 (t
	  `(equalp ,dat-name ',form))))

(defun qvaroid-match-code (qv dat-name)
   (cond ((Qvaroid-atsign qv)
	  (error "Segment variable ~s occurred in illegal place in matchq"
		 qv))
	 (t
	  (qvaroid-decode-match qv dat-name))))

(datafun attach-datafun match-code #'datafun-on-plist)

(defun qvaroid-decode-match (qv dat-name)
	   (let ((sym (Qvaroid-sym qv)))
	      (cond ((is-Symbol sym)
		     (let ((h (get (Qvaroid-sym qv) 'match-code)))
			(cond (h
			       (cond ((Qvaroid-comma qv)
				      (cerror "I will ignore the comma"
					      "Apparent var to be evaluated ~s~% appears also to be a special match construct"
					      qv)))
			       (funcall h (Qvar-notes qv) dat-name))
			      (t
			       (test-or-set-equal qv dat-name)))))
		    (t
		     (test-or-set-equal qv dat-name)))))

(defun cons-match-code (form dat-name)
   (let ((a (car form)) (d (cdr form)))
      (let-fun ()
	 (cond ((is-Qvaroid a)
		(cond ((Qvaroid-atsign a)
		       (let ((l (predictable-size d)))
			  (cond ((not l)
				 (error "Matching form too complex: ~s" form))
				((= l 0)
				 (qvaroid-decode-match a dat-name))
				(t
				 `(and ,@(include-if (> l 0)
					    `(>= (length ,dat-name) ,l))
				       ,(qvaroid-decode-match
						 a `(butlast ,dat-name ,l))
				       (let ((\ dat (last ,dat-name ,l)))
					  ,(match-code d '\ dat)))))))
		      (t
		       `(and (is-Pair ,dat-name)
			     ,(match-code a `(car ,dat-name))
			     (let ((\ dat (cdr ,dat-name)))
				,(cdr-match-code '\ dat))))))
	       (t
		`(and (is-Pair ,dat-name)
		      (let ((\ dat (car ,dat-name)))
			 ,(match-code a '\ dat))
		      (let ((\ dat (cdr ,dat-name)))
			 ,(cdr-match-code '\ dat)))))

       :where

         (cdr-match-code (dat-name)
	    (cond ((atom d)
		   (atom-match-code d dat-name))
		  (t
		   (cons-match-code d dat-name)))))))
	    
(defun predictable-size (l)
   (cond ((atom l) 0)
	 ((atom (car l))
	  (cond ((and (is-Qvaroid (car l))
		      (Qvaroid-atsign (car l)))
		 false)
		(t
		 (+ (predictable-size (cdr l))
		    1))))
	 (t (+ (predictable-size (cdr l))
	       1))))

(defun test-or-set-equal (qv dat-name)
   (let ((sym (Qvaroid-sym qv))
	 (comma (Qvaroid-comma qv)))
      (let ((dontcare (memq sym '(_ () nil))))
	 (cond (dontcare
		(cond (comma
		       (cerror "I will treat it as a sure match"
			       "Don't-care matchvar ~s occurs with comma"
			       qv)))
		'true)
	       (comma
		`(equalp ,sym ,dat-name))
	       (t
		`(progn
		    (!= ,(Qvaroid-sym qv) ,dat-name)
		    true))))))

;; not used
(defun allbut (l dat-name)
   (cond ((= l 0)
	  dat-name)
	 (t
	  `(take ,l ,dat-name))))

(defun is-constant-for-match (e)
   (cond ((is-Qvaroid e) false)
	 ((atom e) true)
	 (t
	  (and (is-constant-for-match (car e))
	       (is-constant-for-match (cdr e))))))

; Handle ?(:& -pats-)
(datafun match-code :&
   (defun :^ (items dat-name)
      (multi-match-codes 'and items dat-name)))

;;;;(datafun match-code \?and &)

; Handle ?(:\| -pats-) -- disjunction
(datafun match-code :\|
   (defun :^ (items dat-name)
      (let ((and-stuff (memq ':& items)))
	 (cond (and-stuff
		`(and ,(multi-match-codes
			 'or
			 `(,@(ldiff items and-stuff)
			     ,@(cddr and-stuff))
			 dat-name)
		      ,(multi-match-codes
			   'and
			   `(,(cadr and-stuff))
			   dat-name)))
	       (t
		(multi-match-codes 'or items dat-name))))))

(datafun match-code :|| :\|)

;;;;(datafun match-code \?or \|)

;;; Handle ?(:check ...), which executes ... and succeeds if the value is true.
(datafun match-code :check
   (defun (items _)
      `(progn ,@items)))

;;; Does anyone actually use :!! ?
(datafun match-code :!! :check)

;;; ?(:+ p -preds-) matches if p matches something that satisfies all
;;; the preds.
(datafun match-code :+
   (defun (items dat-name)
      `(and ,(match-code (car items) dat-name)
	    ,@(mapcar (\\ (pred)
			 `(,(cond ((car-eq pred '\\)
				   `(lambda ,@(cdr pred)))
				  (t pred))
			   ,dat-name))
		      (cdr items)))))

(datafun match-code :~
   (defun (items dat-name)
      `(not ,(match-code (car items) dat-name))))

; Produce a list of forms to match FORMS without weird side effects.
(defun multi-match-codes (op forms dat-name)
   `(let ((\ dat ,dat-name))
       (,op 
	,@(for (form in forms)
	     (save
	        `(let ((\ dat \ dat))
		    ,(match-code form '\ dat)))))))

(defun match-code-cleanup (code)
   (cond ((atom code) code)
	 ((atom (car code))
	  (case (car code)
	     ((quote) code)
	     ((and)
	      (connective-flatten
		 'and 'true (mapcar #'match-code-cleanup (cdr code))))
	     ((or)
	      (connective-flatten
		 'or 'false (mapcar #'match-code-cleanup (cdr code))))
	     ((not)
	      (let ((a (match-code-cleanup (cadr code))))
		 (cond ((atom a)
			(cond ((eq a 'true) 'false)
			      ((eq a 'false) 'true)
			      (t `(not ,a))))
		       ((eq (car a) 'not)
			(cadr a))
		       (t `(not ,a)))))
	     ((let)
	      (let ((bod (match-code-cleanup (caddr code))))
;;;;		 (out "equal? [" (cadr code) "] => "
;;;;		      (equal (cadr code) '((\ dat \dat))) :%)
		 (cond ((and (= (length (cadr code))
				1)
			     (car-eq (car (cadr code))
				     '\ dat)
			     (simple-dat-match bod))
			(dat-subst (cadr (car (cadr code)))
				   bod))
		       ((equal (cadr code) '((\ dat \dat)))
			bod)
		       (t `(let ,(cadr code) ,bod)))))
	     (t
	      `(,(car code)
		,@(mapcar #'match-code-cleanup (cdr code))))))
	 (t
	  `(,(match-code-cleanup (car code))
	    ,@(mapcar #'match-code-cleanup (cdr code))))))
	      
(defun connective-flatten (conn ident l)
   (let ((al (for (cc in l)
		(splice
		   (cond ((eq cc ident) '())
			 ((atom cc)
			  (list cc))
			 ((eq (car cc) conn)
			  (list-copy (cdr cc)))
			 (t (list cc)))))))
      (cond ((= (len al) 0) ident)
	    ((= (len al) 1) (car al))
	    (t `(,conn ,@al)))))

;;; Returns false if current binding of | dat| is set inside e,
;;; or if there are 2 or more occurrences of | dat| free in e.
;;; Otherwise it returns
;;; the number of free occurrences of | dat| (0 or 1).
(defun simple-dat-match (e)
   (cond ((atom e)
	  (cond ((eq e '\ dat) 1)
		(t 0)))
         ((eq (car e) 'quote)
          0)
	 ((memq (car e) '(setq setf !=))
	  (cond ((eq (cadr e) '\ dat)
		 false)
		(t (simple-dat-match (caddr e)))))
         ((eq (car e) 'quote)
          0)
	 ((eq (car e) 'let)
	  (repeat :for ((bv :in (cadr e))
			(numoccs 0)
			bvm
                        ;; 'true' if | dat| is bound in this 'let' --
                        (found-dat false))
	   :result (cond (found-dat numoccs)
                         (t
                          (let ((bod-r (simple-dat-match (caddr e))))
                             (cond (bod-r
                                    (setq numoccs (+ numoccs bod-r))
                                    (and (< numoccs 2) numoccs))
                                   (t false)))))
	     (cond ((eq (car bv) '\ dat)
		    (setq found-dat true)))
	     (setq bvm (simple-dat-match (cadr bv)))
	   :while bvm
	     (setq numoccs (+ numoccs bvm))
	   :until (> numoccs 1)
	   :result false))
	 (t
	  (repeat :for ((y :in e)
			(numoccs 0)
			ym)
	   :result numoccs
	     (setq ym (simple-dat-match y))
	   :while ym
	     (setq numoccs (+ numoccs ym))
	   :until (> numoccs 1)
	   :result false))))


;; Substitute x for free occurrences of \ dat in e
(defun dat-subst (x e)
   (cond ((atom e)
	  (cond ((eq e '\ dat) x)
		(t e)))
	 ((eq (car e) 'let)
	  (let ((bvars (mapcar (\\ (bv) `(,(car bv) ,(dat-subst x (cadr bv))))
			       (cadr e))))
	     (cond ((exists (bv in bvars)
		       (eq (car bv) '\ dat))
		    `(let ,bvars ,(caddr e)))
		   (t
		    `(let ,bvars ,(dat-subst x (caddr e)))))))
	 ((eq (car e) 'quote) e)
	 (t
	  (mapcar (\\ (ee) (dat-subst x ee))
		  e))))

)

(defmacro match-let (pattern datum &body body)
   `(match-cond ,datum
       ,(make-Qvar pattern body)
       (t
	(error "match-let failed to match ~s to ~s" ',pattern  ',datum))))

(defmacro match-cond (datum &rest clauses)
   (multiple-value-let (decls match-vars clauses)
		       (match-cond-analyze clauses)
      `(let ((match-datum ,datum))
	  (let (,@match-vars)
	     ,@decls
	     (cond ,@clauses)))))

(defun match-cond-analyze (stuff)
   (multiple-value-let (decls clauses)
		       (declarations-separate stuff)
      (values decls
	      (match-cond-find-match-vars clauses)
	      (mapcar (\\ (c)
			 (cond ((is-Qvar c)
				`((matchq ,(Qvar-sym c) match-datum)
				  ,@(Qvar-notes c)))
			       ((car-eq c ':\?)
				`((matchq ,(cadr c) match-datum)
				  ,@(cddr c)))
			       (t c)   ))
		      clauses))))

(defun match-cond-find-match-vars (clauses)
   (repeat :for ((c :in clauses)
		 :collector vars)
    :result (remove-duplicates vars)
    :within
      (cond ((is-Qvar c)
	     (:continue
	      :nconc (find-matchvars (Qvar-sym c))))
	    ((car-eq c ':\?)
	     (:continue
	      :nconc (find-matchvars (cadr c)))))))

(defun find-matchvars (x)
   (cond ((is-Qvaroid x)
	  (let ((v (Qvaroid-sym x)))
	     (cond ((is-Symbol v)
		    (cond ((get v 'match-code)
			   (find-matchvars (Qvar-notes x)))
			  ((or (memq v '(nil t _))
			       (Qvaroid-comma x))
			   !())
			  (t (list v))   ))
		   (t (find-matchvars (Qvar-notes x)))   )))
         ((atom x) !())
	 ((eq (car x) ':quote)
	  !())
         (t (nconc (find-matchvars (car x))
                   (find-matchvars (cdr x))))   ))

(datafun within-unwrap match-cond
   (defun :^ (exp unwrap-deeper _)
;;;;      (trace-around match-cond-wu
;;;;	 (:> "(match-cond-wu: " exp ")")
      (mapcan (\\ (clause)
		 (cond ((is-Qvaroid clause)
			(funcall unwrap-deeper
			   (Qvaroid-notes clause)))
		       ((car-eq clause ':?)
			(funcall unwrap-deeper (cddr clause)))
;;;;			(<# (\\ (e) (funcall unwrap-deeper e))
;;;;			    (cdr clause))
		       (t
			(mapcan unwrap-deeper clause))))
	      (cddr exp))
;;;;	 (:< (val &rest _) "match-cond-wu: " val))
      ))

(datafun within-sublis match-cond
   (defun :^ (alist exp within-sublis)
      `(match-cond ,(cadr exp)
	  ,@(mapcar (\\ (c)
		       (cond ((is-Qvaroid c)
			      (make-Qvaroid
				 false false (Qvar-sym c)
				 (funcall within-sublis alist (Qvar-notes c))))
			     (t
			      (funcall within-sublis alist c))))
                    (cddr exp)))))

(defmacro setter (x)
   (let ((new-val-var (gensym))
         (fcn-var (gensym)))
      `(\\ (,new-val-var ,fcn-var)
          (!= ,x (>< ,fcn-var *-* ,new-val-var)))))

;;; For use as second arg of 'setter'
(defvar ^-this-val (\\ (_ new-val) new-val))
(defvar <-this-val ^-this-val)
;;; E.g., (funcall set-whatever (first l323) <-this-val)
(defvar retrieve-val (\\ (old-val _) old-val))