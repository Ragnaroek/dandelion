;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; scheme.lisp -- Language related definitions for Scheme.


(in-package "MK4")


;;; Scheme language definition.

(defclass scheme-language-loader (language-loader)
  ()
  (:default-initarg :name "CL external Scheme object file loader."))


(defclass scheme-language-compiler (language-compiler)
  ()
  (:default-initarg :name "Generic Scheme Compiler" :tag :scheme-compiler))


;;; The following classes should be singleton ones.

(defclass pseudo-scheme-language-compiler (scheme-language-compiler)
  ()
  (:default-initarg :name "Pseudo Scheme Compiler"
    :tag :pseudo-scheme-compiler))

(defclass pseudo-scheme-language-loader (scheme-language-loader)
  ()
  (:default-initarg :name "Pseudo Scheme Loader"
    :tag :pseudo-scheme-loader))


(defvar *pseudo-scheme-language-compiler*
  (make-instance 'pseudo-scheme-language-compiler))

(defvar *pseudo-scheme-language-loader*
  (make-instance 'pseudo-scheme-language-loader))


;;; Notes.
;;;
;;; 20020725 Marco Antoniotti
;;;
;;; Fill in the missing classes following the scheme in
;;; "MAKE-DEFSYSTEM:languages;c;c.lisp".

(defparameter *scheme-compiler* *pseudo-scheme-language-compiler*)

(defparameter *scheme-loader* *pseudo-scheme-language-loader*)


;;; The set of functions below may or may not be useful.  It is here
;;; as an example of how to extend the code.

(defgeneric  scheme-file-os-default-source-extension (os)
  (:method ((os cl.env:operating-system)) "scm"))

(defun scheme-file-default-source-extension ()
  (scheme-file-os-default-source-extension cl.env:*os*))


;;; The following may be incorrect and probably useless.

(defgeneric  scheme-file-os-default-binary-extension (os)
  (:method ((os cl.env:operating-system)) "bin"))

(defun scheme-file-default-binary-extension ()
  (scheme-file-os-default-binary-extension cl.env:*os*))


(defun scheme-file-default-object-pathanme ()
  (make-pathname :type (scheme-file-default-binary-extension)))

(defun scheme-file-default-error-pathanme ()
  (make-pathname :type *compile-error-file-type*))

(defun scheme-file-default-log-pathanme ()
  (make-pathname :type "log"))



(defmethod invoke-compiler ((scheme-compiler pseudo-scheme-language-compiler)
			    (file pathname)
			    &rest args
			    &key
			    (output-pathname
			     (output-file-pathname
			      file
			      :output-file
			      (scheme-file-default-object-pathanme)))
			    &allow-other-keys)
  (let ((scheme-package (find-package "SCHEME")))
    (if scheme-package
	(apply (symbol-function (find-symbol "COMPILE-FILE" scheme-package))
	       filename
	       (funcall (symbol-function 
			 (find-symbol "INTERACTION-ENVIRONMENT"
				      scheme-package)))
	       args)
	(error 'language-processor-error
	       :format-control
	       "Pseudo Scheme package (package SCHEME) not found.~@
                Hence no compiler for file ~S was found.")
	)))

(defmethod invoke-loader ((ps-loader pseudo-scheme-loader)
			  (file pathname)
			  &rest args
			  &key
			  (verbose *load-verbose*)
			  (print *load-print*)
			  options
			  &allow-other-keys)
  (declare (ignore args options))

  ;; According to the MK3 code the following should be fine.
  (load file :print print :verbose verbose))


;;; Language definition.

(define-language :scheme
  :compiler *scheme-compiler*
  :loader *scheme-loader*
  :source-extension "scm"
  :binary-extension "bin")


;;; end of file -- scheme.lisp --
