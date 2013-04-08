;;; -*- Mode: Lisp -*-

;;; load-defsystem.lisp --
;;;
;;; Copyright (c) 2000-2002 Marco Antoniotti, all rights reserved.
;;; See file COPYING for details.
;;;
;;; We cannot have a DEFSYSTEM form for the DEFSYSTEM package in the
;;; first place, due to an obvious "chicken and egg" problem.  So here
;;; is a very old style 'load-' file.

(in-package "COMMON-LISP-USER")

;;;===========================================================================
;;; User definable parameters.

;;; *mk-defsystem-absolute-directory-pathname* --
;;; The location (i.e. a directory) of the MK:DEFSYSTEM distribution.
;;; Please note that the result of PARSE-NAMESTRING must yield a
;;; PATHNAME with PATHNAME-NAME equal to NIL and PATHNAME-DIRECTORY
;;; equal to a list with FIRST equal to :ABSOLUTE and without :WILD or
;;; :WILD-INFERIORS components.

;;; Thanks to Kevin Rosenberg for the following very nice idea.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mk-defsystem-absolute-directory-pathname*
    (make-pathname :host (pathname-host *load-truename*)
		   :device (pathname-device *load-truename*)
		   :directory (pathname-directory *load-truename*)
		   )))

;;; The following three parameters are tested IN SEQUENCE and
;;; exclusively.  The first one found true one will cause the
;;; associated action to be performed.

(defparameter *mk-defsystem-load-source-only-p* nil
  "If T, loads only the MK:DEFSYSTEM source files.")

(defparameter *mk-defsystem-load-newer-p* nil
  "If T, loads the MK:DEFSYSTEM source or compiled files, whichever is newer.")

(defparameter *mk-defsystem-compile-and-load-p* t
  "If T, compiles and loads the MK:DEFSYSTEM files.")


;;;  *mk-defsystem-load-verbose*, *mk-defsystem-load-print* --
;;; The following variables are for debugging the load procedure.
;;; They are used to override the values of *LOAD-PRINT* and
;;; *LOAD-VERBOSE*.
;;;
;;; Note that it they are DEFVAR's for a good reason.  Loading the
;;; file should not change their values; however, loading the file should
;;; define them if they are not defined yet.

(defvar *mk-defsystem-load-verbose* nil)

(defvar *mk-defsystem-load-print* nil)


;;;===========================================================================
;;; Support code and actual load forms.
;;; You should not be required to look at anyhting beyond this point
;;; if not for debugging purposes.


;;; *mk-defsystem-lp-filenames* --
;;; NOTE: Order is important!!!
;;; The "MAKE-DEFSYSTEM" logical pathname is defined later on.

(defparameter *mk-defsystem-lp-filenames*
  '(
    "MAKE-DEFSYSTEM:;adjoin-dirs"
    
    "MAKE-DEFSYSTEM:defsystem-pkg"
    ;; "MAKE-DEFSYSTEM:conditions"
    ;; "MAKE-DEFSYSTEM:utilities;split-sequence" ; Loaded separatedly.
    "MAKE-DEFSYSTEM:utilities;ambler;ambler-pkg"
    "MAKE-DEFSYSTEM:utilities;ambler;ambler"
    ;; "MAKE-DEFSYSTEM:;directory-processing"
    ;; "MAKE-DEFSYSTEM:utilities;user-interaction"
    "MAKE-DEFSYSTEM:utilities;y-or-n-p-wait"
    "MAKE-DEFSYSTEM:utilities;run-os-program"
    "MAKE-DEFSYSTEM:utilities;save-image"

    "MAKE-DEFSYSTEM:language-support"
    "MAKE-DEFSYSTEM:languages;shared;shared-functionality"
    "MAKE-DEFSYSTEM:languages;c;c"
    "MAKE-DEFSYSTEM:languages;fortran;fortran"
    "MAKE-DEFSYSTEM:languages;java;java"
    ;; "MAKE-DEFSYSTEM:languages;ada;ada"
    ;; "MAKE-DEFSYSTEM:languages;scheme;scheme"

    "MAKE-DEFSYSTEM:;versions"

    "MAKE-DEFSYSTEM:;actions"

    "MAKE-DEFSYSTEM:;base-components"
    "MAKE-DEFSYSTEM:;predefined-components"
    "MAKE-DEFSYSTEM:;predefined-specialized-components"
    ;; This was below.
    "MAKE-DEFSYSTEM:;defsystem"

    ;; This was above.
    "MAKE-DEFSYSTEM:utilities;user-interaction"
    
    "MAKE-DEFSYSTEM:impl-dependent;common"
    #+(or cmu sbcl scl) "MAKE-DEFSYSTEM:impl-dependent;cmucl" ; Hopefully correct.
    #+clisp "MAKE-DEFSYSTEM:impl-dependent;clisp"
    #+lispworks "MAKE-DEFSYSTEM:impl-dependent;lispworks"
    #+allegro "MAKE-DEFSYSTEM:impl-dependent;allegro"
    #+mcl "MAKE-DEFSYSTEM:impl-dependent;mcl"
    #+genera "MAKE-DEFSYSTEM:impl-dependent;genera"
    #+lcl "MAKE-DEFSYSTEM:impl-dependent;lcl"
    #+cormanlisp "MAKE-DEFSYSTEM:impl-dependent;corman"
    #+poplog "MAKE-DEFSYSTEM:impl-dependent;poplog"
    #+(or kcl ibcl akcl ecl gcl) "MAKE-DEFSYSTEM:impl-dependent;kcl-derivates"

    ;; "MAKE-DEFSYSTEM:;defsystem"
    "MAKE-DEFSYSTEM:;registry"
    "MAKE-DEFSYSTEM:;defsystem-protocol"
    ;; "MAKE-DEFSYSTEM:;provide-require"

    "MAKE-DEFSYSTEM:;syntax"
    ))


(defparameter *mk-defsystem-load-debug-only-p* nil)

;;; The main load/compile/configure code.

(eval-when (:load-toplevel :execute)

  ;; Setting up a useful logical pathnames.
  ;; To be sure, each subdirectory is treated separatedly, to avoid
  ;; problems with implementations which cannot handle '**' as
  ;; :WILD-INFERIORS on the right hand side of the definitions.
  ;; For the time being, since we only have three subdirectories it
  ;; should not be a problem.
  
  (setf (logical-pathname-translations "MAKE-DEFSYSTEM")
	`(("impl-dependent;*.*.*"
	   ,(make-pathname
	     :host (pathname-host *mk-defsystem-absolute-directory-pathname*)
	     :device (pathname-device *mk-defsystem-absolute-directory-pathname*)
	     :directory (append (pathname-directory
				 *mk-defsystem-absolute-directory-pathname*)
				'("impl-dependent"))))
	  #|("utilities;*.*.*"
	   ,(make-pathname
	     :host (pathname-host *mk-defsystem-absolute-directory-pathname*)
	     :device (pathname-device *mk-defsystem-absolute-directory-pathname*)
	     :directory (append (pathname-directory
				 *mk-defsystem-absolute-directory-pathname*)
				'("utilities"))))|#
	  ("utilities;**;*.*.*"
	   ,(make-pathname
	     :host (pathname-host *mk-defsystem-absolute-directory-pathname*)
	     :device (pathname-device *mk-defsystem-absolute-directory-pathname*)
	     :directory (append (pathname-directory
				 *mk-defsystem-absolute-directory-pathname*)
				'("utilities" :wild-inferiors))))
	  ("languages;**;*.*.*"
	   ,(make-pathname
	     :host (pathname-host *mk-defsystem-absolute-directory-pathname*)
	     :device (pathname-device *mk-defsystem-absolute-directory-pathname*)
	     :directory (append (pathname-directory
				 *mk-defsystem-absolute-directory-pathname*)
				'("languages" :wild-inferiors))))
	  ("cl-environment;*.*.*"
	   ,(make-pathname
	     :host (pathname-host *mk-defsystem-absolute-directory-pathname*)
	     :device (pathname-device *mk-defsystem-absolute-directory-pathname*)
	     :directory (append (pathname-directory
				 *mk-defsystem-absolute-directory-pathname*)
				'("cl-environment")))
	   )
	  (";*.*.*" ,*mk-defsystem-absolute-directory-pathname*)
	  (";*.*" ,*mk-defsystem-absolute-directory-pathname*)
	  ("*.*.*" ,*mk-defsystem-absolute-directory-pathname*)
	  ("*.*" ,*mk-defsystem-absolute-directory-pathname*)
	  ))

  (flet ((load-compiling-if-needed (lp-string-filename-sans-extension)
	   (declare (type string lp-string-filename-sans-extension))
	   (let* ((*load-print* *mk-defsystem-load-print*)
		  (*load-verbose* *mk-defsystem-load-verbose*)
		  (compiled-file-type
		   (pathname-type
		    (compile-file-pathname *default-pathname-defaults*)))

		  (lp-string-binary-filename
		   (concatenate 'string
				lp-string-filename-sans-extension
				"."
				compiled-file-type))

		  (lp-string-source-filename
		   (concatenate 'string
				lp-string-filename-sans-extension
				".lisp"))

		  (binary-physical-filename
		   (translate-logical-pathname lp-string-binary-filename))

		  (source-physical-filename
		   (translate-logical-pathname lp-string-source-filename))
		  )
	     (cond (*mk-defsystem-load-debug-only-p*
		    (format t "MK:DEFSYSTEM loading:~@
                               - ~S~%- ~S~%- ~S~%- ~S~%- ~S~%"
			    lp-string-filename-sans-extension
			    lp-string-source-filename
			    lp-string-binary-filename
			    source-physical-filename
			    binary-physical-filename
			    ))
		   (*mk-defsystem-load-source-only-p*
		    (load source-physical-filename))

		   (*mk-defsystem-load-newer-p*
		    (if (and (probe-file binary-physical-filename)
			     (> (file-write-date binary-physical-filename)
				(file-write-date source-physical-filename)))
			(load binary-physical-filename)
			(load source-physical-filename)))

		   (*mk-defsystem-compile-and-load-p*
		    (cond ((and (probe-file binary-physical-filename)
				(> (file-write-date binary-physical-filename)
				   (file-write-date source-physical-filename)))
			   (load binary-physical-filename))
			  (t
			   (compile-file source-physical-filename
					 :output-file binary-physical-filename
					)
			   (load binary-physical-filename))))
		   )))			; end of load-compiling-if-needed
	 )
    (format *trace-output*
	    "~&;;; MK4: Loading MK:DEFSYSTEM package version ~A.~%" "4.0")
    (unless (member :cl-environment *features*)
      (format *trace-output*
	      "~&;;; MAKE: Ensuring CL-ENVIRONMENT package is present.")
      (unless (probe-file "MAKE-DEFSYSTEM:cl-environment;load-cl-environment.lisp")
	(error "Cannot find CL-ENVIRONMENT loader file ~@
                MAKE-DEFSYSTEM:cl-environment;load-cl-environment.lisp.~@
                Please make sure that CL-ENVIRONEMT is loaded in your ~
                system. Note that you do not really need to have the~@
                `cl-environment' directory under the `MAKE-DEFSYSTEM:' one.~@
                You can find CL-ENVIRONMENT in the CLOCC at~@
                <http://sourceforge.net/projects/clocc>."))
      (load "MAKE-DEFSYSTEM:cl-environment;load-cl-environment.lisp"
	    :verbose nil
	    :print nil)
      (load-cl-environment-library
       :directory (translate-logical-pathname "MAKE-DEFSYSTEM:cl-environment;")
       :load-verbose nil))
    (let ((*load-verbose* t))

      (unless (find-package "SPLIT-SEQUENCE")
	(load-compiling-if-needed "MAKE-DEFSYSTEM:utilities;split-sequence"))

      (dolist (f *mk-defsystem-lp-filenames*)
	(load-compiling-if-needed f)))

    (pushnew :mk-defsystem-4 *features*)
    (provide :mk-defsystem-4)
    ))


;;; end of file -- load-defsystem.lisp --
