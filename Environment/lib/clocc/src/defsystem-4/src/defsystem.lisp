;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; defsystem.lisp --

(in-package "MK4")

(define-condition no-loader (error)
  ((component :reader unloadable-component :initarg :component))
  (:report (lambda (nlc stream)
	     (format stream "No loader is known for component ~S."
		     (unloadable-component nlc)))))

(define-condition no-compiler (error)
  ((component :reader uncompilable-component :initarg :component))
  (:report (lambda (nlc stream)
	     (format stream "No compiler is known for component ~S."
		     (uncompilable-component nlc)))))



(defgeneric describe-component (component
				&optional stream recursive-p recursion-depth)
  (:documentation
   "Prints a description of the component.
The COMPONENT argument is a standard component. STREAM is a stream
where to print the description (default*STANDARD-OUTPUT*). RECURSIVE-P
controls whether the printing should recur on the subcomponents and
RECURSION-DEPTH limits the depth of this recursion (its NIL default
means that no limit should be imposed on this recursion)."))


(defgeneric execute-action (component action &key policy &allow-other-keys))

(defgeneric execute-action-on-subcomponents (component action
						       &key policy
						       &allow-other-keys))

(defgeneric load-action (component component-pathname &key &allow-other-keys))

(defgeneric compile-action (component component-source-pathname
				      &key
				      error-log-file
				      &allow-other-keys))


(defgeneric needs (component action &key &allow-other-keys)
  (:documentation "Tests whether a COMPONENT `needs' an ACTION."))


(defgeneric action-applicable-p (component action &key &allow-other-keys)
  (:documentation "Tests whether the ACTION is applicable to a COMPONENT.")
  (:method ((c component) (action symbol) &key &allow-other-keys) t)
  (:method ((c component) (action action) &key &allow-other-keys) t)
  )


;;;---------------------------------------------------------------------------
;;; Component Pathname Protocol.

(defgeneric get-component-source-pathname (component))
(defgeneric get-component-source-directory (component))
(defgeneric get-component-binary-pathname (component))
(defgeneric get-component-binary-directory (component))
(defgeneric get-component-error-log-pathname (component))
(defgeneric get-component-error-log-directory (component))

;;; This is from ASDF.  It is a good idea.
;;; OUTPUT-PATHNAMES is a better name.

(defgeneric output-files (component operation))
(defgeneric output-pathnames (component operation))

;;; However, it can be generalized
(defgeneric output-components (component operation))


;;; end of file -- defsystem.lisp --
