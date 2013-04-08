;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; c.lisp -- Language related definitions for C/C++.

;;; This is very basic. Somebody else who needs it can add in support
;;; for header files, libraries, different C compilers, etc. For example,
;;; we might add a COMPILER-OPTIONS slot to the component defstruct.

(in-package "MK4")


;;; C language definition.

(defclass c-language-loader (language-loader object-loader)
  ()
  (:default-initargs :name "CL external C object file loader"))


(defclass c-language-compiler (language-compiler external-language-processor)
  ()
  (:default-initargs :name "Generic C Compiler" :command "cc" :tag :cc))


;;; The following classes should be singleton ones.

(defclass gcc-language-compiler (c-language-compiler
				 gnu-language-compiler)
  ()
  (:default-initargs :name "GNU C Compiler" :command "gcc" :tag :gcc))
  
(defvar *gcc-language-compiler*
  (make-instance 'gcc-language-compiler))


(defclass sunpro-cc-language-compiler (c-language-compiler)
  ()
  (:default-initargs :name "SunPRO C Compiler" :command "cc" :tag :sunpro-cc))
  
(defvar *sunpro-cc-language-compiler*
  (make-instance 'sunpro-cc-language-compiler))


(defclass microsoft-c-language-compiler (c-language-compiler)
  ()
  (:default-initargs :name "Microsoft C Compiler"
    :command "cl"
    :tag :msc))

(defvar *microsoft-c-language-compiler*
  (make-instance 'microsoft-c-language-compiler))


(defparameter *c-compiler*  *gcc-language-compiler*)

(defparameter *c-loader* *cl-foreign-object-loader*)


;;; The set of functions below may or may not be useful.  It is here
;;; as an example of how to extend the code.

(defgeneric  c-file-os-default-source-extension (os)
  (:method ((os cl.env:operating-system)) "c"))

(defun c-file-default-source-extension ()
  (c-file-os-default-source-extension cl.env:*os*))


(defgeneric  c-file-os-default-binary-extension (os)
  (:method ((os cl.env:unix)) "o")
  (:method ((os cl.env:ms-windows)) "obj"))

(defun c-file-default-binary-extension ()
  (c-file-os-default-binary-extension cl.env:*os*))


(defgeneric static-library-os-extension (os)
  (:method ((os cl.env:unix)) "a")
  (:method ((os cl.env:ms-windows)) "lib"))

(defun static-library-extension ()
  (static-library-os-extension cl.env:*os*))


(defgeneric shared-library-os-extension (os)
  (:method ((os cl.env:unix)) "so")
  (:method ((os cl.env:ms-windows)) "dll"))

(defun shared-library-extension ()
  (shared-library-os-extension cl.env:*os*))


(defun c-file-default-object-pathanme ()
  (make-pathname :type (c-file-default-binary-extension)))

(defun c-file-default-error-pathanme ()
  (make-pathname :type *compile-error-file-type*))

(defun c-file-default-log-pathanme ()
  (make-pathname :type "log"))
  

;;; invoke-compiler --

(defmethod invoke-compiler ((c-compiler c-language-compiler)
			    (file pathname)
			    &rest args
			    &key
			    (output-pathname
			     (output-file-pathname
			      file
			      :output-file
			      (c-file-default-object-pathanme)))
			    &allow-other-keys)
  (apply #'invoke-processor-external
	 c-compiler
	 file
	 :output-pathname output-pathname
	 args))


;;; Loader.
;;; No loader defined. See INVOKE-LOADER on OBJECT-LOADER in
;;; "MAKE-DEFSYSTEM:;language-support.lisp".


;;; C Language definition.

(define-language :c
    :compiler *c-compiler*
    :loader *c-loader*
    :source-extension "c"
    :binary-extension "o")

;;; end of file -- c.lisp --
