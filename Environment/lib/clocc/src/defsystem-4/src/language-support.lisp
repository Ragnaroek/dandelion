;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; language-support.lisp --

(in-package "MK4")


;;; ********************************
;;; Language-Dependent Characteristics
;;; ********************************
;;; This section is used for defining language-specific behavior of
;;; defsystem. If the user changes a language definition, it should
;;; take effect immediately -- they shouldn't have to reload the
;;; system definition file for the changes to take effect. 


;;; Notes.
;;;
;;; 2002-05-22 Marco Antoniotti
;;; Let's bite the bullet.  CLOS is easier to deal with.

(defclass language-processor ()
  ((tag :reader language-processor-tag
	:initarg :tag
	:type symbol)
   (name :reader language-processor-name
	 :initarg :name
	 :type string))
  (:default-initargs :name "" :tag nil))


(defgeneric language-processor-p (x)
  (:method ((x language-processor)) t)
  (:method ((x t)) nil))


(defclass external-language-processor (language-processor)
  ((command-line :reader external-command
		 :type string
		 :initarg :command))
  (:default-initargs :command "unknown_external_processor")
  (:documentation "The External Language Processor Class.

A mixin class used to denote language processors that must be invoked
via the underlying OS interface, e.g. `cc'.  This class essentially
provides the `command-line' slot accessible by the `external-reader'
reader.  The default content of this slot is
\"unknown_external_processor\", which should generate an error when
invoked by the OS interface.

This is opposed to language processors that can be invoked directly
via a CL function call (e.g. a C compiler implemented in CL)."))

(defgeneric external-language-processor-p (x)
  (:method ((x external-language-processor)) t)
  (:method ((x t)) nil))


(defvar *language-processors* (make-hash-table)
  "Table containing language processor instances.
It is keyed by keywords.")


(defun find-language-processor (kwd)
  (declare (type symbol kwd))		; Maybe `keyword' would do.
  (gethash kwd *language-processors*))

(defun (setf find-language-processor) (lp kwd)
  (declare (type symbol kwd)
	   (type language-processor lp))
  (setf (gethash kwd *language-processors*) lp))



;;; Generic "traditional" processors.
;;; Note the difference betweend the "loader" and the "linker" is to
;;; be understood in the following sense.
;;;
;;; We need to "load" something in a running CL image. To do so we
;;; usually just issue '(load ...)' for CL files and something
;;; implementation dependent for 'object' files.
;;;
;;; We need to produce an "object" or "executable" by linking together
;;; some items external to the running CL. To do so we have
;;; essentially to resort to something like `ld' on UNIX, but this may
;;; not be sufficient. E.g. we may have to do some peculiar things to
;;; produce - say - a `.lib' file under Windows starting from a
;;; `.dll'.
;;;
;;; Hence the distinction between `loader-processor' and
;;; `linker-processor'.



(defclass language-compiler (language-processor))

(defclass language-loader (language-processor))

(defclass language-linker (language-processor))

(defclass language-interpreter (language-processor))


;;; object-loader --
;;; The distinction between `language-loader' and `object-loader' is
;;; nominal.  They are essentially the same class, and are both
;;; provided because of the `object-file'.

(defclass object-loader (language-processor)
  ()
  (:documentation "The Object Loader Class.
This class represents the tools that are used to load an OS dependent
object file into a running CL."))


;;; Language Processors initialization methods.

(defmethod initialize-instance :after ((lp language-processor)
				       &key tag
				       &allow-other-keys)
  (when tag (setf (find-language-processor tag) lp)))



;;; define-language-processor --
;;; This is essentially a "singleton" class definition form.
;;;
;;; Notes:
;;;
;;; 20020722 Marco Antoniotti
;;; The implementation is incorrect since I am not really using a
;;; SINGLETON :METACLASS.  Will fix this later.
;;;
;;; Unused for the time being.

(defmacro define-language-processor (name (&optional
					   (superclass 'language-processor))
					  slots
					  &rest
					  options)
  (let ((language-tag (or (find :tag options :key #'first)
			  (intern (symbol-name name)
				  (find-package "KEYWORD"))))
	)
    `(progn
       (defclass ,name (,superclass) ,slots ,@options)
       (setf ,name (make-instance ',name :tag ',language-tag)))))


;;; language class.
  
(defclass language ()
  ((name :reader language-name
	 :type symbol
	 :initarg :name)		; The name of the language (a
					; keyword).
   (compiler :accessor language-compiler ; Either a function or an
	     :type (or null		 ; instance of type
		       function		 ; `language-processor',
		       language-compiler)
	     :initarg :compiler)

   (processor :accessor language-processor ; As above.
	      :type (or null
			function
			language-processor)
	      :initarg :processor)
	     
   (loader :accessor language-loader	; As above.
	   :type (or null
		     function
		     language-loader)
	   :initarg :loader)

   (linker :accessor language-linker	; As above.
	   :type (or null
		     function
		     language-linker)
	   :initarg :linker)

   (interpreter :accessor language-interpreter	; As above.
		:type (or null
			  function
			  language-interpreter)
		:initarg :interpreter)

   (source-extension
    :accessor language-source-extension
    :type (or null string)
    :initarg :source-extension)		; Filename extensions for
					; source files. 
   (binary-extension
    :accessor language-binary-extension
    :type (or null string)
    :initarg :binary-extension)		; Filename extensions for
					; binary files.
   )
  (:default-initargs :compiler nil
     :interpreter nil
     :loader nil
     :linker nil
     :processor nil
     :source-extension nil
     :binary-extension nil)
  )


(defgeneric languagep (x)
  (:method ((x language)) t)
  (:method ((x t)) nil))


(defmethod print-object ((l language) stream)
  (print-unreadable-object (l stream)
     (format stream "MK4:LANGUAGE ~:@(~A~)~@[:~*~]~@[ source ext. ~S~]~
                     ~@[ binary ext. ~S~]"
	     (language-name l)
	     (or (language-source-extension l)
		 (language-binary-extension l))
	     (language-source-extension l)
	     (language-binary-extension l))))


;;; component-language-mixin --
;;; The following three slots are used to provide for alternate compilation
;;; and loading functions for the files contained within a component. If
;;; a component has a compiler or a loader specified, those functions are
;;; used. Otherwise the functions are derived from the language. If no
;;; language is specified, it defaults to Common Lisp (:lisp). Other current
;;; possible languages include :scheme (PseudoScheme) and :c, but the user
;;; can define additional language mappings. Compilation functions should 
;;; accept a pathname argument and a :output-file keyword; loading functions
;;; just a pathname argument. The default functions are #'compile-file and
;;; #'load. Unlike fdmm's SET-LANGUAGE macro, this allows a defsystem to 
;;; mix languages.

(defclass component-language-mixin ()
  ((language :accessor component-language
	     :initarg :language
	     :type (or null symbol language))
   )
  (:default-initargs :language nil)	; We are agnostic about the
					; language. Note however that
					; this default *must* be NIL
					; to make the overriding
					; machinery to work at
					; definition time.
  
  (:documentation
   "The Language Mixin Class.
A 'mixin' class used to specify the (progamming) `language' of the content."))


(defclass loadable-component-mixin (component-language-mixin)
  ((loader :accessor component-loader
	   :writer set-component-loader	; This is needed for initializations.
	   :initarg :loader
	   :type (or null function language-loader))
   (loader-options :accessor component-compiler-options
		   :initarg :loader-options
		   :type list)
   )
  (:default-initargs :loader nil)
  (:documentation 
   "A `mixin' class used to specify that a component is `loadable'.
The `loader' slot contains a function or an instance of the specific
`loader' to be used to perform the operation.
The `loader-options' slots contains a plist of loader dependent
options that are passed to it when invoked."))


(defclass compilable-component-mixin (component-language-mixin)
  ((compiler :accessor component-compiler
	     :writer set-component-compiler ; This is needed for initializations.
	     :initarg :compiler
	     :type (or null function language-compiler))
   (compiler-options :accessor component-compiler-options
		     :initarg :compiler-options
		     :type list)
   )
  (:default-initargs :compiler nil :compiler-options ())
  (:documentation
   "A `mixin' class used to specify that a component is `compilable'.
The `compiler' slot contains a function or an instance of the specific
`compiler' to be used to perform the operation.
The `compiler-options' slots contains a plist of compiler dependent
options that are passed to it when invoked."))


(defclass linkable-component-mixin (component-language-mixin)
  ((linker :accessor component-linker
	   :writer set-component-linker	; This is needed for initializations.
	   :initarg :linker
	   :type (or null function language-linker))
   (linker-options :accessor component-linker-options
		   :initarg :linker-options
		   :type list)
   (libraries :accessor linkable-component-libraries
	      :initarg :libraries
	      :type list)
   )
  (:default-initargs :linker nil :linker-options () :libraries ())
  (:documentation
   "A `mixin' class used to specify that a component is `linkable'.
The `linker' slot contains a function or an instance of the specific
`linker' to be used to perform the operation.
The `linker-options' slots contains a plist of compiler dependent
options that are passed to it when invoked."))

(defgeneric linkable-component-p (x)
  (:method ((x linkable-component-mixin)) t)
  (:method ((x t)) nil))


(defclass interpretable-component-mixin (component-language-mixin)
  ((interpreter :accessor component-interpreter
		:writer set-component-interpreter ; This is needed for initializations.
		:initarg :interpreter
		:type (or null function language-interpreter))
   )
  (:default-initargs :interpreter nil)
  (:documentation
   "A `mixin' class used to specify that a component is `interpretable'.
The `intepreter' slot contains a function or an instance of the specific
`interpreter' to be used to perform the operation.
The `interpreter-options' slots contains a plist of compiler dependent
options that are passed to it when invoked."))


;;; Language Mixin and Processors Mixin Initialization and Access Protocols.

(defmethod initialize-instance :after ((c component-language-mixin)
				       &key language)
  (when language
    ;; Not necessarily we supply the language.
    (let ((language (find-language language)))
      (when language (setf (component-language c) language)))))

(defmethod component-language :before ((c component-language-mixin))
  (unless (languagep (slot-value c 'language))
    (let ((language (find-language (slot-value c 'language))))
      (when language (setf (component-language c) language)))))


;;; THIS NEEDS TO BE FIXED!

(defun update-language-slot (c language-slot-reader processor-slot-writer)
  (with-accessors ((l component-language))
    c
    (when (and (languagep l) (funcall language-slot-reader l))
      (funcall processor-slot-writer (funcall language-slot-reader l) c))))


(defmethod initialize-instance :after ((c loadable-component-mixin)
				       &key)
  (update-language-slot c #'language-loader #'set-component-loader))

(defmethod component-loader :before ((c loadable-component-mixin))
  (update-language-slot c #'language-loader #'set-component-loader))



(defmethod initialize-instance :after ((c compilable-component-mixin)
				       &key)
  (update-language-slot c #'language-compiler #'set-component-compiler))

(defmethod component-compiler :before ((c compilable-component-mixin))
  (update-language-slot c #'language-compiler #'set-component-compiler))



(defmethod initialize-instance :after ((c linkable-component-mixin) &key)
  (update-language-slot c #'language-linker #'set-component-linker))

(defmethod component-linker :before ((c linkable-component-mixin))
  (update-language-slot c #'language-linker #'set-component-linker))



(defmethod initialize-instance :after ((c interpretable-component-mixin)
				       &key)
  (update-language-slot c #'language-interpreter #'set-component-interpreter))

(defmethod component-interpreter :before ((c interpretable-component-mixin))
  (update-language-slot c #'language-interpreter #'set-component-interpreter))


;;; Predefined instances.

(defvar *cl-foreign-object-loader* (make-instance 'object-loader))


;;;===========================================================================
;;; Processors Protocol.

(define-condition language-processor-error (simple-error)
  ((processor :reader language-processor-error-processor
	      :type (or null language-processor)
	      :initarg :processor))
  (:default-initargs :processor nil)
  (:documentation "The Language Processor Error Condition.
A class of conditions generated by the invokation of various language
dependent processors."))


(defvar *compile-error-file-type* "err")


;;; output-file-pathname --
;;; The next function implements a particular interpretation of the
;;; COMPILE-FILE-PATHNAME function from CLHS.
;;; I happen to think (as the CLisp implementors do) that this is the
;;; correct interpretation.
;;; The name also seems more correct.

(defun output-file-pathname (input-file
			     &key
			     (output-file (compile-file-pathname input-file))
			     &allow-other-keys)
  (declare (type (or string pathname) input-file output-file))
  (merge-pathnames (pathname output-file) (merge-pathnames input-file)))


;;; process-options --

(defgeneric process-options (processor options &key &allow-other-keys)
  (:documentation "Processes a list of OPTIONS.

OPTIONS is a plist keyed by keywords.

Returns two values. A list of STRINGs or other items suitable to be
passed to the underlying interface to the Operating System or to
whatever other interface is used to invoke the actual PROCESSOR.
A list of KEYWORDs that represent all the options being processed."))


;;; list-known-processor-options --

(defgeneric list-known-processor-options (language-processor)
  (:method ((lp language-processor)) ())
  (:documentation
   "Returns the options handled by the processor.
The result is a list of keywords."))


(defgeneric invoke-processor (processor pathname
					&rest args
					&key
					verbose
					options
					&allow-other-keys))


(defgeneric invoke-processor-external (processor pathname
						 &rest args
						 &key
						 verbose
						 options
						 &allow-other-keys))


(defgeneric invoke-compiler (compiler pathname
				      &rest args
				      &key
				      output-pathname
				      verbose
				      options
				      &allow-other-keys))


;;; I.e. the "loader" CL uses to "load" something within its execution
;;; environment.

(defgeneric invoke-loader (compiler pathname
				    &rest args
				    &key
				    verbose
				    options
				    &allow-other-keys)
  )


;;; I.e. the "linker" used to produce an output "linked"
;;; file. E.g. "ld".

(defgeneric invoke-linker (compiler pathname
				    &rest args
				    &key
				    verbose
				    options
				    &allow-other-keys)
  )

(defgeneric invoke-interpreter (interpreter pathname
					    &rest args
					    &key
					    verbose
					    options
					    &allow-other-keys))


;;;---------------------------------------------------------------------------
;;; Predefined methods and functions.

(defun file-default-error-pathname ()
  (make-pathname :type *compile-error-file-type*))


;;; run-processor-command --
;;; The main driver that sets up a few output streams (regular and
;;; error) and that eventually calls the CL implementation dependent
;;; RUN-OS-PROGRAM.
;;;
;;; The INVOKE-PROCESSOR (-COMPILER, -INTERPRETER, -LOADER, -LINKER, -EXTERNAL)
;;; methods may call RUN-PROCESSOR-COMMAND.
;;;
;;; Notes.
;;;
;;; 20020725 Marco Antoniotti
;;; History. This was in the language/c/c.lisp file.  It originates in
;;; MK3 as RUN-C-COMPILER.

(defun run-processor-command (program
			      arguments
			      output-file
			      &optional
			      (error-file
			       (compile-file-pathname
				output-file
				:output-file (file-default-error-pathname)))
			      (error-output *error-output*)
			      (verbose nil))
  (declare (type string program)
	   (type list arguments)
	   (type pathname output-file)
	   (type (or null pathname) error-file)
	   (type (or null stream (member t)) error-output)
	   (type boolean verbose))
  
  (flet ((make-compound-stream (&rest streams)
	   (apply #'make-broadcast-stream (delete nil streams)))
	 )
    (let ((error-output (if (eq t error-output) *error-output* error-output))
	  (error-file-stream nil)
	  (old-timestamp (file-write-date output-file))
	  (fatal-error nil)
	  (output-file-written-p nil)
	  )
      (declare (type (or null stream)
		     error-output
		     error-file-stream)
	       (type (or null integer) old-timestamp)
	       (type boolean fatal-error output-file-written-p))
      (handler-case
       (progn
	 (when error-file
	   (setf error-file-stream
		 (open error-file :direction :output :if-exists :supersede)))

	 (with-open-stream (verbose-stream (make-compound-stream
					    error-file-stream
					    (and verbose *trace-output*)))

	   (with-open-stream (error-output
			      (make-compound-stream error-file-stream
						    error-output))
	     (user-message verbose-stream
			   "running: ~A~@[ ~{~A~^ ~}~]."
			   program
			   arguments)

	     (handler-case
	      (let ((process-status
		     (run-os-program program
				     :arguments arguments
				     :error error-output)))
		(setf fatal-error (not (zerop process-status))))
	      (error (e)
		     (format *error-output*
			     ">>> Minchia ~?~%"
			     (simple-condition-format-control e)
			     (simple-condition-format-arguments e)
			     )))
	       
	     (setf output-file-written-p
		   (and (probe-file output-file)
			(or (null old-timestamp)
			    (/= old-timestamp
				(file-write-date output-file)))))

	     (when output-file-written-p
	       (user-message verbose-stream "~A written."
			     output-file))
	     
	     (user-message verbose-stream
			   "running of ~S completed~@[ with errors~]."
			   program
			   fatal-error)

	     (values (and output-file-written-p output-file)
		     fatal-error
		     fatal-error))))
	   
       (error (e)
	      (format *error-output* "~S" e)
	      (when error-file
		(close error-file-stream)
		(unless (or fatal-error (not output-file-written-p))
		  (delete-file error-file)))
	      (values (and output-file-written-p (pathname output-file))
		      fatal-error
		      fatal-error))

       (:no-error (output-file-result warnings-p fatal-errors-p)
		  (when error-file
		    (close error-file-stream)
		    (unless (or fatal-errors-p (not output-file-written-p))
		      (delete-file error-file)))
		  (values output-file-result
			  warnings-p
			  fatal-errors-p))
       ))
    ))


;;; invoke-processor-external ---

(defmethod invoke-processor-external ((e-proc external-language-processor)
				      (file pathname)
				      &rest args
				      &key
				      (options ())
				      (output-pathname
				       (output-file-pathname file))
				      (error-log-file nil)
				      (error-output *error-output*)
				      (errorp t)
				      (verbose *compile-verbose*)
				      &allow-other-keys)
  (declare (ignore args)
	   (type list options args)
	   (type (or null pathname) output-pathname error-log-file)
	   (type (or null stream (member t)) error-output)
	   (type boolean verbose errorp)
	   )

  (flet ((add-output-option (options)
	   (if (or (member :output-file options) (null output-pathname))
	       options
	       (list* :output-file (namestring output-pathname) options)
	       ))
	 )
    (let ((arguments (process-options e-proc (add-output-option options))))
      (multiple-value-bind (output-file warnings fatal-errors)
	  (run-processor-command (external-command e-proc)
				 (list* (namestring file) arguments)
				 output-pathname
				 error-log-file
				 error-output
				 verbose)
	(if (and errorp (or (not output-file) fatal-errors))
	    (error 'language-processor-error
		   :processor e-proc
		   :format-control "MK4: Invoking ~S on ~S failed."
		   :format-arguments (list e-proc file))
	    (values output-file warnings fatal-errors))))))


;;; load-object-file --
;;; The LOAD-OBJECT-FILE is the interface to the CL implementation
;;; dependent foreign object loader.  This is a very thin layer over
;;; the CL implementation FFI facilities.
;;;
;;; LOAD-OBJECT-FILE is called by INVOKE-LOADER (which is used
;;; essentially as a hook).
;;;
;;; The actual implementation methods are in the
;;; impl-dependent/<CL implementation>.lisp files.
;;;
;;; Notes.
;;;
;;; 20020725 Marco Antoniotti
;;; History. This was in the language/c/c.lisp file.

(defgeneric load-object-file (loadable-c-pahtname
			      &key
			      (print *load-print*)
			      (verbose *load-verbose*)
			      (libraries '("c"))
			      )
  (:documentation
   "Loads a C object file (or similar) into the CL environment."))

(defmethod no-applicable-method ((lcf (eql #'load-object-file))
				 &rest arguments)
  (error "MK4: LOAD-OBJECT-FILE undefined for arguments ~S."
	 arguments))


;;; The main loader.

(defmethod invoke-loader ((ol object-loader) (object-pathname pathname)
			  &key
			  (print *load-print*)
			  (verbose *load-verbose*)
			  (libraries ()))
  (load-object-file object-pathname
		    :print print
		    :verbose verbose
		    :libraries libraries))


;;;===========================================================================
;;; Languages Data Base.

(defvar *language-table* (make-hash-table :test #'equal)
  "Hash table that maps from languages to language instances.")

(defun find-language (name)
  (gethash name *language-table* nil))

(defun (setf find-language) (language-instance name)
  (declare (type symbol name)
	   (type language language-instance))
  (setf (gethash name *language-table*) language-instance))


(defmethod initialize-instance :after ((l language) &key)
  (setf (find-language (language-name l)) l))


;;; define-language --
;;; Useful Macro.

(defmacro define-language (name &key
				compiler loader processor
				source-extension binary-extension)
  (assert (symbolp name))
  (let ((language-tag (intern (symbol-name name) (find-package "KEYWORD"))))
    `(make-instance 'language
		    :name ,language-tag
		    :compiler ,compiler
		    :loader ,loader
		    :processor ,processor
		    :source-extension ,source-extension
		    :binary-extension ,binary-extension)))


;;;---------------------------------------------------------------------------
;;; Language definitions.

;;; Common Lisp Language Definition (mostly useless, here for completenenss).

(define-language :common-lisp
  :compiler #'compile-file
  :loader #'load
  :source-extension "lisp"
  :binary-extension (cl.env:compiled-file-extension))


;;; Pseudo Scheme Language Definition.

(define-language scheme
  :source-extension "scm"
  )


#||
;;; Test System for verifying multi-language capabilities.
(defsystem foo
  :language :lisp
  :components ((:module c :language :c :components ("foo" "bar")) 
	       (:module lisp :components ("baz" "barf"))))

||#



;;;===========================================================================
;;; Old code

(defun compile-function (component)
  (or (component-compiler component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-compiler language)))
      #'compile-file))


(defun load-function (component)
  (or (component-loader component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-loader language)))
      #'load))

;;; end of file -- language-support.lisp --
