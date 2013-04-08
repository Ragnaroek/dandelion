;;; -*- Mode: CLtL -*-

;;; ambler.lisp --
;;; Simple code ambler. Purely syntactic and without `environment'
;;; hacking.
;;; This is essentially an instantiation of the "Visitor Pattern"
;;; which, by means of CL multiple dispatching is readily implemented.
;;;
;;; This facility is inspied by the WALKER functionalities available
;;; in many implementations.  This facility does not implement all the
;;; features (mostly non-portable) available in many WALKER
;;; implementations, but at least it is (should be) very portable and
;;; usable.
;;;
;;; Please see the file COPYING for licensing information.
;;;
;;; Copyright (c) 2001, Marco Antoniotti


;;;===========================================================================
;;; Prologue.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package "CL.UTILITIES.AMBLER")
    (cerror "Make package \"CL.UTILITIES.AMBLER\" (no symbols exported)."
	    "Package \"CL.UTILITIES.AMBLER\" does not exist.")
    (make-package "CL.UTILITIES.AMBLER" :nicknames '("AMBLER"))))

  
(in-package "CL.UTILITIES.AMBLER")

;;;===========================================================================
;;; Functional interface.

(defgeneric amble-expression (expression
			      ambling-context
			      &key environment
			      &allow-other-keys)
  (:documentation
   "Traverses a Common Lisp `expression'.
For the purpose of this package a `Common Lisp expression' is either
an ATOM or a LIST.

Adding methods to this function will result in special processing of a
given expression.

The values returned by this function must be an object of type T and
the (possibly modified) AMBLING-CONTEXT."))

  
(defgeneric amble-form (form-operator
			whole-form
			ambling-context
			&key environment
			&allow-other-keys)
  (:documentation "Traverses a Common Lisp `form'.
For the purpose of this package a `Common Lisp form' (held in
WHOLE-FORM) is a LIST, whose FIRST element is FORM-OPERATOR.

Adding methods to this function will result in special processing of a
given form.

The values returned by this function must be an object of type T and
the (possibly modified) AMBLING-CONTEXT."))


;;;===========================================================================
;;; Context definitions.
;;; In order to specialize the behavior of the AMBLER, a user may
;;; specialize the `ambling context'.  (Thus specializing the
;;; "Visitor" behavior).
;;;
;;; The package provides some standardized classes that can be used as
;;; they are or as examples for extension.

(defclass standard-ambling-context ()
  ()
  (:documentation
   "The top level `ambler context' class."))

(defclass copying-ambling-context (standard-ambling-context)
  ()
  (:documentation
   "The `ambler context' class which is used for `copying' expressions."))

(defclass identity-ambling-context (standard-ambling-context)
  ()
  (:documentation
   "The `ambler context' class which is used for `traversing' expressions."))


;;; Two instances.

(defparameter *copying-ambling-context*
  (make-instance 'copying-ambling-context))

(defparameter *identity-ambling-context*
  (make-instance 'identity-ambling-context))


;;;===========================================================================
;;; Conditions.

(define-condition no-ambler-defined ()
  ((form-operator :reader form-operator
		  :initarg :form-operator)
   (whole-form :reader whole-form
	       :initarg :whole-form)
   (context :reader context
	    :initarg :context)
   )
  (:default-initargs :form-operator nil)
  (:report (lambda (cnd stream)
	     (format stream
		     "AMBLER: error: no ambler defined for form ~S ~
                      and context ~S~
                      ~@[ (operator supplied)~]."
		     (whole-form cnd)
		     (context cnd)
		     (form-operator cnd))))
  )


;;;===========================================================================
;;; Interface implementation.

(defmethod no-applicable-method ((amble-expression (eql #'amble-expression))
				 &rest args)
  (error 'no-ambler-defined
	 :whole-form (first args)
	 :context (second args)))


(defmethod no-applicable-method ((amble-form (eql #'amble-form))
				 &rest args)
  (error 'no-ambler-defined
	 :form-operator (first args)
	 :whole-form (second args)
	 :context (third args)))


;;;---------------------------------------------------------------------------
;;; Identity amblers.

(defmethod amble-expression ((expr t)
			     (ac identity-ambling-context)
			     &key environment
			     &allow-other-keys)
  (declare (ignorable environment))
  (values expr ac)
  )


(defmethod amble-expression ((expr list)
			     (ac identity-ambling-context)
			     &key environment
			     &allow-other-keys)
  (declare (ignorable environment))
  (amble-form (first expr)
	      expr
	      ac)
  (values expr ac))


(defmethod amble-expression ((expr (eql nil))
			     (ac identity-ambling-context)
			     &key environment
			     &allow-other-keys)
  (declare (ignorable environment))
  (values expr ac))


(defmethod amble-form ((form-operator t)
		       (whole-form list)
		       (ac identity-ambling-context)
		       &key environment
		       &allow-other-keys)
  (declare (ignorable environment))
  (amble-expression form-operator ac)
  (typecase (rest whole-form)
    (null (amble-expression nil ac))
    (atom (amble-expression (rest whole-form) ac))
    (cons (amble-form (first (rest whole-form)) (rest whole-form) ac))
    )
  (values whole-form ac))


;;;---------------------------------------------------------------------------
;;; Copying amblers.

(defmethod amble-expression ((expr t)
			     (ac copying-ambling-context)
			     &key environment
			     &allow-other-keys)
  (declare (ignorable environment))
  (values expr ac))

(defmethod amble-expression ((expr (eql nil))
			     (ac copying-ambling-context)
			     &key environment
			     &allow-other-keys)
  (declare (ignorable environment))
  (values expr ac))

(defmethod amble-expression ((expr list)
			     (ac copying-ambling-context)
			     &key environment
			     &allow-other-keys)
  (declare (ignorable environment))
  (values (amble-form (first expr)
		      expr
		      ac)
	  ac))

(defmethod amble-form ((form-operator t)
		       (whole-form list)
		       (ac copying-ambling-context)
		       &key environment
		       &allow-other-keys)
  (declare (ignorable environment))
  (typecase (rest whole-form)
    (null (values (cons (amble-expression form-operator ac)
			(amble-expression nil ac))
		  ac))
    ;; A true cons.
    (atom (values (cons (amble-expression form-operator ac)
			(amble-expression (rest whole-form) ac))
		  ac))
    (cons (values (cons (amble-expression form-operator ac)
			(amble-form (first (rest whole-form))
				    (rest whole-form)
				    ac))
		  ac))
    ))


;;;===========================================================================
;;; Useful macros.

(defmacro def-expression-ambler ((&optional
				  expr-type
				  (context 'identity-ambling-context))
				 &body forms)
  "A utility short-hand macro to define `expressions amblers'.
DEF-AMBLER-EXPRESSION contructs a method for AMBLE-EXPRESSION.
The full syntax of DEF-EXPRESSION-AMBLER is

   DEF-EXPRESSION-AMBLER '(' [( EXPR-TYPE | '(' EXPR-VAR EXPR-TYPE ')' )
                              [( CONTEXT | '(' CONTEXT-VAR CONTEXT ')' )]] ')'
                         FORMS

EXPR-TYPE is the type of the expression being looked at. CONTEXT is
the context class to be used in the call.

If not supplied, CONTEXT-VAR and EXPR-VAR will be replaced by the symbols
`context' and `expr', interned in *PACKAGE*.

FORMS are just regular CL expressions constituting the body of the
method being defined. EXPR-VAR and CONTEXT-VAR can be used within
FORMS, e.g. to call AMBLE-EXPRESSION and AMBLE-FORM recursively."
  (let ((expr-var (if (listp expr-type)
		      (first expr-type)
		      (intern (symbol-name '#:expr) *package*)))
	(expr-type (if (listp expr-type)
		       (second expr-type)
		       expr-type))
	(context-var (if (listp context)
			 (first context)
			 (intern (symbol-name '#:context) *package*)))
	(context (if (listp context)
		     (second context)
		     context))
	)
    `(defmethod amble-expression ((,expr-var ,expr-type)
				  (,context-var ,context)
				  &key environment
				  &allow-other-keys)
       (declare (ignorable environment))
       ,@forms)))


(defmacro def-form-ambler (form-operator
			   (&optional
			    whole-form
			    (context 'identity-ambling-context))
			   &body forms)
    "A utility short-hand macro to define `form amblers'.
DEF-FORM-AMBLER contructs a method for AMBLE-EXPRESSION.
The full syntax of DEF-FORM-AMBLER is

   DEF-FORM-AMBLER ( FORM-OPERATOR | '(' FORM-OPERATOR-VAR FORM-OPERATOR ')' )
                   '(' [ WHOLE-FORM-VAR
                         [( CONTEXT | '(' CONTEXT-VAR CONTEXT ')' )]] ')'
                   FORMS

FORM-OPERATOR is the `operator' of the form being looked at. CONTEXT is
the context class to be used in the call.

If not supplied, CONTEXT-VAR and FORM-OPRATOR-VAR will be replaced
by the symbols `context' and `operator', interned in *PACKAGE*.

FORMS are just regular CL expressions constituting the body of the
method being defined. FORM-OPERATOR-VAR and CONTEXT-VAR can be used
within FORMS, e.g. to call AMBLE-EXPRESSION and AMBLE-FORM
recursively."
  (let ((operator-var (if (listp form-operator)
			  (first form-operator)
			  (intern (symbol-name '#:operator) *package*)
			  ))
	(operator (if (listp form-operator)
		      (second form-operator)
		      form-operator))
	(whole-form-var whole-form)
	(context-var (if (listp context)
			 (first context)
			 (intern (symbol-name '#:context) *package*)))
	(context (if (listp context)
		     (second context)
		     context))
	)
    `(defmethod amble-form ((,operator-var (eql ',operator))
			    (,whole-form-var list)
			    (,context-var ,context)
			    &key environment
			    &allow-other-keys)
       (declare (ignorable environment))
       ,@forms)))

;;;===========================================================================
;;; Epilogue.

(eval-when (:load-toplevel :execute)
  (pushnew :cl-utilities-ambler *features*))


;;;===========================================================================
#| Tests

(def-form-ambler if (the-form)
  (print the-form)
  (print context))

(def-form-ambler if (the-form copying-ambler-context)
  (print the-form)
  (print context))

(def-form-ambler if (the-form (ctx copying-ambler-context))
  (print the-form)
  (print ctx))

(def-expression-ambler (symbol) (format t "Looking at ~S~%." expr))

|#

;;; end of file -- ambler.lisp --
