;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: binders.lisp,v 2.1 2005/12/26 00:25:16 airfoyle Exp $

;;; Copyright (C) 1976-2003 Drew McDermott and Yale University. 
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:compile-toplevel :load-toplevel)
   (export '(multiple-value-let bind letrec
	     let-fun let-fun-nonrec let-var intercept pass)))

(defmacro multiple-value-let (vars e &rest l)
   (multiple-value-bind (vars l)
			(ignore-smooth vars l)
      `(multiple-value-bind ,vars ,e ,@l)   ))

;; BIND: Like LET, but binds special variables

;;;;(defparameter built-in-globals* 
;;;;    '(*print-level* *print-length* *print-circle*
;;;;      *package* *readtable* *print-case*))

;;;;(defparameter lisp-package* (find-package :common-lisp))

(defmacro bind (vars-n-vals &body body)
   ;; Some Lisps (SBCL in particular) object to declarations of
   ;; built-in global variables as special.  Sheesh.
   (let ((touchables
            (mapcan (lambda (var-n-val)
                       (let ((var (cond ((consp var-n-val)
                                         (car var-n-val))
                                        (t var-n-val))))
                          (cond ((eq (symbol-package var) lisp-package*)
                                 ;;(member var built-in-globals*)
                                 !())
                                (t (list var)))))
                    vars-n-vals)))
      `(let ,vars-n-vals
         ;; if bindings, declare variables to be special
         ,@(include-if (not (null touchables))
              `(cl:declare (special ,@touchables)))
         ,@body)))

(defmacro letrec (&body b) `(let-fun ,@b))

; Prettier than LABELS -- allows ... :WHERE (local-fun1 ...) (local-fun2 ...)
; in addition to clauses at the front.
(defmacro let-fun (clauses &rest body)
   (let-fun-expand 'labels clauses body))

(defmacro let-fun-nonrec (clauses &body body)
   (let-fun-expand 'flet clauses body))

(defun let-fun-expand (binder clauses body)
   (multiple-value-let (clauses body _ _)
		       (extract-where clauses body)
      (setq clauses
	    (mapcar (\\ (c)
		       ;;;;;(format t "c = ~s~%" c)
		       (cond ((eq (car c) ':def)
			      (setq c (cdr c))))
		       (multiple-value-bind (args body)
			                    (ignore-smooth (cadr c)
							   (cddr c))
			  `(,(car c) ,args ,@body)))
		    clauses))
       `(,binder ,clauses
	   ,@body)))

(defmacro let-var (clauses &rest body)
   (multiple-value-let (clauses body _ _)
                       (extract-where clauses body)
      `(let ,@clauses 
	  ,@body)))

;;; returns < aug-bdgs, truncated-body, positions, wheres >
;;; aug-bdgs = bdgs + wheres, truncated-body = body-minus-wheres,
;;; positions = list of pairs (p1 p2) and numbers p 
;;;    giving positions of all aug-bdgs; (p1 p2) is for bdgs, p's for
;;;    wheres.
;;; wheres = stuff starting with ':where' flag
(defun extract-where (bdgs body &key (offset 1))
    (let ((more (memq ':where body))
	  (normal-bdgs-rels
	     (mapcar (\\ (i) `(,offset ,i))
		     (series 0 (- (len bdgs) 1)))))
       (cond (more
	      (let ((blen (len body)))
		 (values (append bdgs (cdr more))
			 (ldiff body more)
			 `(,@normal-bdgs-rels
			   ,@(mapcar (\\ (i) (+ offset
						1 
						blen
						(- (len (cdr more)))
						i))
				     (series 0 (- (len (cdr more)) 1))))
			 more)))
	     (t
	      (values bdgs body normal-bdgs-rels '())))))

; Return two values: declarations, everything else
(defun declarations-separate (body)
   (do ((b body (cdr b))
	(declarations nil (cons (car b) declarations)))
       ((or (atom (car b))
	    (not (memq (caar b) '(declare ignore))))
	(values (ignore-convert (nreverse declarations)) b))   ))

(defmacro intercept (tag &body b)
   (cond ((is-Symbol tag)
	  `(catch ',tag ,@b))
	 (t
	  (error "Argument to 'intercept' must be a symbol: ~S"
		 tag))))

(defmacro pass (tag r)
   (cond ((is-Symbol tag)
	  `(throw ',tag ,r))
	 (t
	  (error "Argument to 'pass' must be a symbol: ~S"
		 tag))))
