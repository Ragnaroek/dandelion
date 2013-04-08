;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; java.lisp -- Language related definitions for Java.

;;; This is very basic. Somebody else who needs it can add in support
;;; for header files, libraries, different C compilers, etc. For example,
;;; we might add a COMPILER-OPTIONS slot to the component defstruct.

(in-package "MK4")


;;; Java language definition.

#|| Undefined loader.
(defclass java-language-loader (language-loader object-loader)
  ()
  (:default-initarg :name "CL external Java object file loader."))
||#


(defclass java-language-compiler (language-compiler
				  external-language-processor)
  ()
  (:default-initargs :name "Generic Java Compiler"
                     :command "javac"
		     :tag :java-compiler))


;;; The following classes should be singleton ones.

(defclass sun-java-language-compiler (java-language-compiler)
  ()
  (:default-initargs :name "Sun Java Compiler" :tag :sun-java-compiler))


(defvar *java-language-compiler*
  (make-instance 'sun-java-language-compiler))


;;; Notes.
;;;
;;; 20020725 Marco Antoniotti
;;;
;;; Fill in the missing classes following the scheme in
;;; "MAKE-DEFSYSTEM:languages;c;c.lisp".

(defparameter *java-compiler* *java-language-compiler*)

;;; (defparameter *java-loader* *cl-foreign-object-loader*)


;;; The set of functions below may or may not be useful.  It is here
;;; as an example of how to extend the code.

(defgeneric  java-file-os-default-source-extension (os)
  (:method ((os cl.env:operating-system)) "java"))

(defun java-file-default-source-extension ()
  (java-file-os-default-source-extension cl.env:*os*))


(defgeneric  java-file-os-default-binary-extension (os)
  (:method ((os cl.env:operating-system)) "class"))

(defun java-file-default-binary-extension ()
  (java-file-os-default-binary-extension cl.env:*os*))


(defun java-file-default-object-pathanme ()
  (make-pathname :type (java-file-default-binary-extension)))

(defun java-file-default-error-pathanme ()
  (make-pathname :type *compile-error-file-type*))

(defun java-file-default-log-pathanme ()
  (make-pathname :type "log"))


;;; Generic interface to the Java Compiler.
;;; Add other compilers as appropriate.
;;; Generic interface to the generic GNU Compiler.

;;; process-options --
;;;
;;; process-options for the Geneirc GNU Compiler.
;;; The recognized keywords are:
;;;
;;; :output-file	type namestring
;;; :cflags		type string
;;; :cpplags		type string
;;; :debug		type boolean
;;; :optimize		type (mod 10)
;;; :link		type boolean
;;; :static		type boolean
;;; :shared		type boolean
;;; :symbolic		type boolean
;;; :definitions	type (simple-list (or string (simple-list string t)))
;;; :include-paths	type (simple-list (or string pathname))
;;; :library-paths	type (simple-list (or string pathname))
;;; :libraries		type (simple-list (or string pathname))
;;; :other-options	type (simple-list string)

(defmethod process-options ((java-compiler sun-java-language-compiler)
			    options
			    &key &allow-other-keys)

  (flet ((process-option (option-kwd option)
	   (case option-kwd
	     (:output-file ()) ; Ignore it.
	     (:debug (cond ((and option (listp option))
			    (list (format nil "-g:~A~{,~A~}"
					  (first option)
					  (rest option))))
			   (option (list "-g"))))
	     (:deprecation (when option (list "-deprecation")))
				 
	     (:classpath (list (format nil "-classpath ~A" option)))
	     (:sourcepath (list (format nil "-sourcepath ~A" option)))
	     (:directory  (list (format nil "-d ~A" option)))
	     (:verbose  (when option (list (format nil "-verbose"))))
	     (:nowarn  (when option (list (format nil "-nowarn"))))
	     (:other-options (copy-list option))
	     (otherwise ())
	     ))
	 )
    (loop for (option-kwd option) on options by #'cddr
	  nconc (process-option option-kwd option) into result
	  finally (return (values (remove-duplicates result :test #'string-equal)
				  (list :output-file
					:classpath
					:sourcepath
					:debug
					:deprecation
					:directory
					:verbose
					:nowarn
					:other-options)))
	  )))

(defmethod list-known-processor-options ((javac sun-java-language-compiler))
  (remove-duplicates (nconc (list :output-file
				  :classpath
				  :sourcepath
				  :debug
				  :deprecation
				  :directory
				  :verbose
				  :nowarn
				  :other-options)
			    (call-next-method))))

;;; invoke-compiler --

(defmethod invoke-compiler ((java-compiler java-language-compiler)
			    (file pathname)
			    &rest args
			    &key
			    (output-pathname
			     (output-file-pathname
			      file
			      :output-file
			      (java-file-default-object-pathanme)))
			    &allow-other-keys)
  (apply #'invoke-processor-external
	 java-compiler
	 file
	 :output-pathname output-pathname
	 args))


;;; Language definition.

(define-language :java
  :compiler *java-compiler*
  ;; :loader *java-loader*
  :source-extension "java"
  :binary-extension "class")


;;; end of file -- java.lisp --
