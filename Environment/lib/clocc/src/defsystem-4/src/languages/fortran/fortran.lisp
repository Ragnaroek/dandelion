;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; fortran.lisp -- Language related definitions for Fortran.

;;; This is very basic. Somebody else who needs it can add in support
;;; for header files, libraries, different C compilers, etc. For example,
;;; we might add a COMPILER-OPTIONS slot to the component defstruct.

(in-package "MK4")


;;; Fortran language definition.

(defclass fortran-language-loader (language-loader object-loader)
  ()
  (:default-initargs :name "CL external Fortran object file loader."))


(defclass fortran-language-compiler (language-compiler
				     external-language-processor)
  ()
  (:default-initargs :name "Generic Fortran Compiler" :command "f77" :tag :f77))


;;; The following classes should be singleton ones.

(defclass g77-language-compiler (fortran-language-compiler
				 gnu-language-compiler)
  ()
  (:default-initargs :name "GNU Fortran Compiler" :command "g77" :tag :g77))


(defvar *g77-language-compiler*
  (make-instance 'g77-language-compiler))


;;; Notes.
;;;
;;; 20020725 Marco Antoniotti
;;;
;;; Fill in the missing classes following the scheme in
;;; "MAKE-DEFSYSTEM:languages;c;c.lisp".

(defparameter *fortran-compiler* *g77-language-compiler*)

(defparameter *fortran-loader* *cl-foreign-object-loader*)


;;; The set of functions below may or may not be useful.  It is here
;;; as an example of how to extend the code.

(defgeneric  fortran-file-os-default-source-extension (os)
  (:method ((os cl.env:operating-system)) "f"))

(defun fortran-file-default-source-extension ()
  (fortran-file-os-default-source-extension cl.env:*os*))


(defgeneric  fortran-file-os-default-binary-extension (os)
  (:method ((os cl.env:unix)) "o")
  (:method ((os cl.env:ms-windows)) "obj"))

(defun fortran-file-default-binary-extension ()
  (fortran-file-os-default-binary-extension cl.env:*os*))


(defun fortran-file-default-object-pathanme ()
  (make-pathname :type (fortran-file-default-binary-extension)))

(defun fortran-file-default-error-pathanme ()
  (make-pathname :type *compile-error-file-type*))

(defun fortran-file-default-log-pathanme ()
  (make-pathname :type "log"))


;;; THE FOLLOWING CALLS FOR A SUPERCLASS: GNU-COMPILER! The code is
;;; repeated too much between this and the code for gcc.

;;; Generic interface to the Fortran Compiler.
;;; Add other compilers as appropriate.

(defmethod invoke-compiler ((fortran-compiler fortran-language-compiler)
			    (file pathname)
			    &rest args
			    &key
			    (output-pathname
			     (output-file-pathname
			      file
			      :output-file
			      (fortran-file-default-object-pathanme)))
			    &allow-other-keys)
  (apply #'invoke-processor-external
	 fortran-compiler
	 file
	 :output-pathname output-pathname
	 args))


;;; Language definition.

(define-language :fortran
  :compiler *fortran-compiler*
  :loader *fortran-loader*
  :source-extension "f"
  :binary-extension "o")


;;; end of file -- fortran.lisp --
