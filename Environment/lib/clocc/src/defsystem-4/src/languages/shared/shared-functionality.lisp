;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; shared-functionality.lisp -- Functionalities of language
;;; processors shared among several languages.  E.g. GNU GCC
;;; essentially defines a set of common command line flags among
;;; Fortran and GCC. It makes sense to group them here.


(in-package "MK4")

;;; unix-generic-compiler --
;;; An (abstract) superclass for the UNIX faminily of C (an other
;;; language) compilers.

(defclass unix-language-compiler (language-compiler
				  external-language-processor)
  ()
  (:default-initargs :name "The Generic UNIX Compiler Class."
		     :tag :unix-compiler))

(defgeneric unix-language-compiler-p (x)
  (:method ((x unix-language-compiler)) t)
  (:method ((x t)) nil))

(defmethod make-instance ((class (eql (find-class 'unix-language-compiler)))
			  &rest initargs
			  &key &allow-other-keys)
  (declare (ignore initargs))
  (error "MK4: cannot create an instance of class ~S." class))


;;; gnu-language-compiler --
;;; Another, more specialized class.

(defclass gnu-language-compiler (unix-language-compiler)
  ()
  (:default-initargs :name "The Generic GNU Compiler Class."
                     :tag :gnu-compiler))

(defgeneric gnu-language-compiler-p (x)
  (:method ((x gnu-language-compiler)) t)
  (:method ((x t)) nil))

(defmethod make-instance ((class (eql (find-class 'gnu-language-compiler)))
			  &rest initargs
			  &key &allow-other-keys)
  (declare (ignore initargs))
  (error "MK4: cannot create an instance of class ~S." class))


;;; Options processing.

(defmethod process-options ((compiler language-compiler)
			    options
			    &key &allow-other-keys)
  (declare (ignorable options))
  '())


(defmethod process-options :before ((compiler language-compiler)
				    options
				    &key &allow-other-keys)
  (assert (evenp (length options))))


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

(defmethod process-options ((gnu-compiler gnu-language-compiler)
			    options
			    &key &allow-other-keys)

  (flet ((process-option (option-kwd option)
	   (case option-kwd
	     (:output-file (list (format nil "-o ~A" option)))
	     (:cflags (list option))
	     (:cppflags (list option))
	     (:debug (when option (list "-g")))
	     (:static (when option (list "-static")))
	     (:shared (when option (list "-shared")))
	     (:symbolic (when option (list "-symbolic")))
	     (:link (unless option (list "-c")))
	     (:definitions (mapcar (lambda (o)
				     (if (and (atom o) (not (numberp o)))
					 (format nil "~A" o)
					 (format nil "~{~A=~A~}" o)))
				   option))
	     (:optimize (list (format nil "-O~D" option)))
	     (:include-paths (mapcar (lambda (p)
				       (format nil "-I~A" (namestring p)))
				     option))
	     (:library-paths (mapcar (lambda (p)
				       (format nil "-L~A" (namestring p)))
				     option))
	     (:libraries (mapcar (lambda (p)
				   (format nil "-~A" p))
				 option))
	     (:other-options (copy-list option)) ; A (simple-list string).
	     (otherwise ())
	     ))
	 )
    (loop for (option-kwd option) on options by #'cddr
	  nconc (process-option option-kwd option) into result
	  finally (return (values (delete-duplicates result :test #'string-equal)
				  (list :output-file
					:cflags
					:cppflags
					:debug
					:link
					:definitions
					:optimize
					:include-paths
					:library-paths
					:libraries
					:other-options)))
	  )))


(defmethod list-known-processor-options ((gnu-compiler gnu-language-compiler))
  (delete-duplicates (nconc (list :output-file
				  :cflags
				  :cppflags
				  :debug
				  :link
				  :definitions
				  :optimize
				  :include-paths
				  :library-paths
				  :libraries
				  :other-options)
			    (call-next-method))))

;;; end of file -- shared-functionality.lisp --
