;;; -*- Mode: CLtL -*-

;;; defconf.lisp
;;; A 'configure' for Common Lisp.

;;; Copyright (c) 2000-2002 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

;;; Notes:
;;; 20000203 Marco Antoniotti
;;; I decided to make this package dependent on the CL.ENVIRONMENT
;;; package.

(unless (find-package "CL.ENVIRONMENT")
  (error "CL.EXT.CONFIGURATION requires the CL.ENVIRONMENT package."))

(in-package "CL.EXT.CONFIGURATION")


;;;===========================================================================
;;; Implementation dependencies.

;;; The package uses the CL.ENV package to factor away implementation
;;; dependent details. Some of the "DEFSYSTEM" interfaces are
;;; the only implementation and platform dependent definitions in
;;; 'defconf.lisp'.


;;; wild-inferior-directories --

(defgeneric wild-inferior-directories (os))


;;; current-directory-namestring -- Implementation dependent!

(defgeneric current-directory-namestring (cl-implementation)
  (:method ((cl-implementation cl.env:generic-common-lisp-implementation))
	   (namestring ".")))


;;; defsys-type-available-p -- Checks for availability of a particular
;;; DEFSYS in a running CL image.  The specialized methods are in the
;;; 'impl-dependent' directory.

(defgeneric defsys-type-available-p (cl defsys-type)
  (:method ((cl cl.env:generic-common-lisp-implementation)
	    (defsys-type symbol))
	   nil)
  (:documentation
   "Checks whether a defsys of type DEFSYS-TYPE is available in a given CL."))


;;; find-system, load-system -- Implementation dependent.

;;; I want to be fancy and allow different defsystem to cohabitate in a
;;; session.  This is tricky: see the various definitions in the
;;; 'impl-dependent' directory.
;;;
;;; Note the heavy use of a combination of the CL.ENV package plus
;;; extra keyword tags.

(defgeneric find-system (system-designator cl defsys-tag))

(defgeneric load-system (system-designator cl defsys-tag &rest keys))


;;;===========================================================================
;;; Useful utilities.

;;; configure-format -- A useful little macro.

(defun configure-format (outstream format-control &rest format-args)
  (format outstream "~&CL CONFIGURE: ~?" format-control format-args))


;;;===========================================================================
;;; The implementation.

;;;---------------------------------------------------------------------------
;;; Error handling.
;;; For simplicity I will simply have CONFIGURATION-ERROR and
;;; CONFIGURATION-WARNING.  Check the inheritance structure to see
;;; what they mean.

;;; CONFIGURATION-ERROR --

(define-condition configuration-error (error)
  ((message-control :reader conf-error-message-control
                    :initarg :message-control)
   (message-arguments :reader conf-error-message-arguments
                      :initarg :message-arguments)
   )
  (:documentation "The Configuration Error Condition.")
  (:default-initargs :message-control "some error happened."
                     :message-arguments ())
  (:report (lambda (ce stream)
	     (format stream
		     "CL.EXT.CONFIGURATION: ~?"
		     (conf-error-message-control ce)
		     (conf-error-message-arguments ce)))))


;;; CONFIGURATION-WARNING --

(define-condition configuration-warning (warning)
  ((message-control :reader conf-warning-message-control
                    :initarg :message-control)
   (message-arguments :reader conf-warning-message-arguments
                      :initarg :message-arguments)
   )
  (:documentation "The Configuration Warning Condition.")
  (:default-initargs :message-control "watch out."
                     :message-arguments ())
  (:report (lambda (ce stream)
	     (format stream
		     "CL.EXT.CONFIGURATION: ~?"
		     (conf-warning-message-control ce)
		     (conf-warning-message-arguments ce)))))


;;; Interface variables.

(defvar *configure-verbose* t)


;;;---------------------------------------------------------------------------
;;; Entry points.

;;; setup -- The entry point.
;;; The DEFCONFIGURATION macro defines a method for CONFIGURE of the form
;;;   (defmethod setup ((s (eql 'system-name)) &allow-other-keys))

(eval-when (:load-toplevel :compile-toplevel :execute)

  ;;; setup

  (defgeneric setup (conf-designator &rest keys &key)
    (:documentation "One of the CONFIGURE entry points.")
    (:method ((s symbol) &rest keys &key)
	     (declare (ignore keys))
	     (error 'configuration-error
		    :message-control "No configuration defined for ~S."
		    :message-arguments (list s)))
    (:method ((s string) &rest keys &key)
	     (apply #'setup (intern s *package*) keys))
    )
  )


;;; defconfiguration --
;;; A 'class-like' syntax without inheritance.  Note the 'slots'
;;; argument which contains DEFCLASS-like slots definitions (which is
;;; just a place-holder for the time being.

(defmacro defconfiguration (system-name slots &rest configuration-clauses)
  (declare (ignore slots))

  ;; Note, most of the following variables work as stacks.  The last item
  ;; being pushed in acts like the default.
  (let ((system-name (etypecase system-name
		       (symbol system-name)
		       (string (intern system-name *package*))))
	 
	(library-location-clauses ())
	(source-location-clauses ())
	(logical-pathname-host (string-upcase (string system-name)))
	(required-package-clauses ())
	(required-module-clauses ())
	(required-system-clauses ())
	(special-translations ())
	(construction-method '(:defsystem :mk))
	(final-forms ())
	(other-clauses ())
	)

    ;; "Initialize" the following variables.
    (push (list :library-location
		(current-directory-namestring
		 cl.env:*common-lisp-implementation*)
		:os-type (cl.env:feature-tag cl.env:*os*)
		:cl-implementation (cl.env:feature-tag cl.env:*cl*)
		)
	  library-location-clauses)
    (push (list :source-location
		(current-directory-namestring
		 cl.env:*common-lisp-implementation*)
		:os-type (cl.env:feature-tag cl.env:*os*)
		:cl-implementation (cl.env:feature-tag cl.env:*cl*)
		)
	  source-location-clauses)

    ;; 20000226 Marco Antoniotti
    ;; Ok. The following dolist predates the defmethod based parsing
    ;; solution. Anyway, I leave it as it is because these are the
    ;; "standard" clauses.

    (dolist (clause configuration-clauses)
      (case (first clause)
	(:logical-pathname-host
	 (setf logical-pathname-host (second clause)))
	(:library-location
	 (push clause library-location-clauses))
	(:source-location
	 (push clause source-location-clauses))
	(:required-package
	 (push clause required-package-clauses))
	(:required-system
	 (push clause required-system-clauses))
	(:required-module
	 (push clause required-module-clauses))
	(:special-translations
	 (push clause special-translations))
	(:construction-method
	 (setf construction-method (second clause)))
	(:finally
	 (setf final-forms (rest clause)))
	(t
	 (push clause other-clauses))
	))

    ;; At this point I have all the clauses where I need them.
    ;; Now I can create the SETUP method.

    (let* ((source-locations-clauses-parsed
	    (parse-location-clauses source-location-clauses))

	   (source-location-default
	    (build-location-default source-locations-clauses-parsed))
	   
	   (library-locations-clauses-parsed
	    (parse-location-clauses library-location-clauses))

	   (library-location-default
	    (build-location-default library-locations-clauses-parsed))

	   (special-translations
	    (parse-special-translations special-translations))

	   (special-translations-keywords
	    (build-special-keyword-args
	     (collect-keywords special-translations)))
	   )
      `(progn
	 
	 ;; 20001009 Marco Antoniotti
	 ;; If the argument passing convention for this method
	 ;; changes, then we must make appropriate changes to
	 ;; BUILD-MAIN-TRANSLATIONS and friends.

	 (defmethod setup ((system-name (eql ',system-name))
			   &rest keys
			   &key
			   (logical-pathname-host
			    ,logical-pathname-host
			    lph-supplied-p)
			   (library-location
			    ,library-location-default
			    ll-supplied-p)
			   (source-location
			    ,source-location-default
			    sl-supplied-p)
			   ,@special-translations-keywords)

	   (declare (ignore keys
			    logical-pathname-host
			    ;; library-location
			    ;; source-location
			    lph-supplied-p
			    ll-supplied-p
			    sl-supplied-p
			    ,@(mapcar #'cadar
				      special-translations-keywords)))

	   ,@(build-translations-forms special-translations
				       logical-pathname-host
				       (build-location
					library-locations-clauses-parsed))
	     
	   ,@(build-required-module-forms required-module-clauses)
	   ,@(build-required-system-forms required-system-clauses)
	   ,@(build-required-package-forms required-package-clauses)
	   ,@(build-other-clauses-forms other-clauses)
	   ,@final-forms
	   system-name
	   )
	 ',system-name)
      )))


;;;===========================================================================
;;; Parsing and building (code constructing) routines.
;;;

(defgeneric parse-conf-clause (key clause)
  (:method ((key symbol) clause)
	   (warn "unrecognized clause ~S." clause)))

(defgeneric build-conf-clause-code (key parsed-clause)
  (:method ((key symbol) parsed-clause)
	   (declare (ignore parsed-clause))
	   (warn "cannot build code for clause of type ~S." key)))


;;;---------------------------------------------------------------------------
;;; Clause.
;;; A 'clause' is almost like an AST node in compiler-speak.

(defstruct clause
  (kind nil :type symbol)
  (os-type (cl.env:feature-tag cl.env:*operating-system*)
	   :type symbol)
  (machine-type (cl.env:feature-tag cl.env:*machine*)
		:type symbol)
  (cl-implementation (cl.env:feature-tag cl.env:*common-lisp-implementation*)
		     :type symbol)
  )

;;;---------------------------------------------------------------------------
;;; Locations

(defstruct (location-clause (:include clause))
  (location "" :type string))

(defmethod parse-conf-clause ((key (eql :source-location)) clause)
  (destructuring-bind (loc-kwd
		       location
		       &key
		       (os-type (cl.env:feature-tag cl.env:*os*))
		       (cl-implementation (cl.env:feature-tag cl.env:*cl*))
		       &allow-other-keys)
      clause
    (make-location-clause :kind loc-kwd
			  :location location
			  :os-type os-type
			  :cl-implementation cl-implementation
			  )))

(defmethod parse-conf-clause ((key (eql :library-location)) clause)
  ;; This is a hack.  The parsing of both kinds of clauses is identical.
  (parse-conf-clause :source-location clause))


(defun parse-location-clauses (location-clauses)
  (let ((parsed-clauses ()))
    (dolist (clause location-clauses (nreverse parsed-clauses))
      (push (parse-conf-clause (first clause) clause)
	    parsed-clauses))))


(defun build-location (location-clauses
		       &optional
		       (cl-implementation cl.env:*common-lisp-implementation*)
		       (os cl.env:*operating-system*)
		       (mach cl.env:*machine*))
  (declare (ignore cl-implementation mach))
  (let ((current-os-type (cl.env:os-feature-tag os))
	(defaulting-clause nil)		; This variable may eventually
					; hold the result.
	)
    (dolist (clause location-clauses)
      (let ((os-type (location-clause-os-type clause)))
	(cond ((check-os-type-by-tag-p os-type os)
	       (return-from build-location (location-clause-location clause)))

	      ((null os-type)
	       ;; Maybe warn?
	       (unless defaulting-clause
		 (setf defaulting-clause (location-clause-location clause))))

	      (t nil)))			; Keep looping.
	      )

    ;; If not OS specific clause was found, do the following.
    (cond ((not (null defaulting-clause))
	   (warn "CL CONFIGURATION: Defaulting to non-os specific ~
                  clause ~S."
		 defaulting-clause)
	   defaulting-clause)
	  (t
	   (error 'configuration-error
		  :message-control "no location clause found for specified ~
                                    underlying system (~A)."
		  :message-arguments (list current-os-type))))
    ))


(defun build-location-default (location-clauses
			       &optional
			       (cl-implementation
				cl.env:*common-lisp-implementation*)
			       (os
				cl.env:*operating-system*)
			       (mach
				cl.env:*machine*))
  (if location-clauses
      (build-location location-clauses cl-implementation os mach)
      (current-directory-namestring cl-implementation)))


;;;---------------------------------------------------------------------------
;;; Special Translations.
;;; These structures are really a definition of a AST of the possible
;;; syntax for the :special-translations.

(defstruct (special-translation (:include clause))
  (host nil :type (or null string))
  (translations () :type list))		; A list of 'translation' structures.


(defstruct (translation (:include clause) (:conc-name tr-))
  (spec "" :type string)
  (translation "" :type string)
  (prefix "" :type (or string
		       (member :source-location
			       :library-location)))
  ;; The prefix is a string or one of :source-location, or
  ;; :library-location. The default (the empty
  ;; string) means not to prepend anything to the translation.

  (prefix-configuration-key nil :type symbol))


(defstruct hosts-table translations)

(defun get-host-translations (host host-table &optional result)
  (let ((host-entry (assoc host
			   (hosts-table-translations host-table)
			   :test #'equal)))
    (if host-entry
	(values (rest host-entry) t)
	(values result nil))))

(defsetf get-host-translations (host host-table) (translations)
  `(let ((host-entry (assoc ,host
			    (hosts-table-translations ,host-table)
			    :test #'equal)))
     (if host-entry
	 (setf (cdr host-entry) ,translations)
	 (setf (hosts-table-translations ,host-table)
	       (acons ,host ,translations
		      (hosts-table-translations ,host-table)))
	 )))


(defmethod parse-conf-clause ((kwd (eql :special-translations)) clause)
  (let* ((host-option-present (member :host clause))
	 (host (when host-option-present
		 ;; CLAUSE may not be properly formed as a plist!
		 ;; Hence the separate IF test.
		 (second host-option-present)))
	 (transls (if host-option-present
		      (nthcdr 3 clause)
		      (rest clause)))
	 )
    (when (and transls (not (equal transls '(())))) ; It's a quirk!
      ;; 20000223 Marco Antoniotti
      ;; Note that :os-type and :cl-implementation are ignored here.
      (make-special-translation :kind :special-translation
				:host host
				:translations (parse-translation transls)))))


(defun parse-special-translations (special-translations)
  (let ((translations ()))
    (dolist (clause special-translations (nreverse translations))
      (push (parse-conf-clause (first clause) clause) translations))))


(defun parse-translation (translations)
  (let ((resulting-transl ()))
    (dolist (transl translations (nreverse resulting-transl))
      (destructuring-bind (logical-pathname-spec
			   translation
			   &key
			   (prefix "")
			   prefix-configuration-key
			   (os-type (cl.env:feature-tag
				     cl.env:*operating-system*))
			   (machine-type (cl.env:feature-tag
					  cl.env:*machine*))
			   &allow-other-keys)
	  transl
	(push (make-translation
	       :spec logical-pathname-spec
	       :translation translation
	       :prefix prefix
	       :prefix-configuration-key prefix-configuration-key
	       :os-type os-type
	       :machine-type machine-type)
	      resulting-transl)))))



;;; build-main-translations --
;;; This function is *NOT* self contained.  In particular it requires
;;; knowledge of the argument passing convention of the SETUP method
;;; being generated.

;;; THIS FUNCTION MAY BE USELESS.
;;; 20001009 MA

(defun build-main-translations (default-library-location)
  (let* ((library-location-with-inferiors
	  (concatenate 'string default-library-location
		       (wild-inferior-directories cl.env:*operating-system*)))

	 #|
	 (main-translations `('("**;*.*.*" ,library-location-with-inferiors)
			       '("**;*.*" ,library-location-with-inferiors)
			       '("*.*" ,library-location)))
         |#
	 (ll-with-inferiors-form
	  `(if ll-supplied-p		; Variable from the SETUP method.
	       (concatenate 'string library-location
		 	    (wild-inferior-directories
			     cl.env:*operating-system*))
               ',library-location-with-inferiors))
	 )
    `((list "**;*.*.*" ,ll-with-inferiors-form)
      (list "**;*.*"  ,ll-with-inferiors-form)
      (list "*.*"
	     `(if ll-supplied-p		; Variable from the SETUP method.
	          library-location
	          ',default-library-location)
	     ))
    ))

;;; build-translations-forms --
;;; This function is *NOT* self contained.  In particular it requires
;;; knowledge of the argument passing convention of the SETUP method
;;; being generated.

(defun build-translations-forms (special-translations
				 logical-host-string
				 library-location)
  (declare (ignorable library-location))
  (let* (#|(library-location-with-inferiors
	  (concatenate 'string library-location
		       (wild-inferior-directories
			cl.env:*operating-system*)))|#

	 (library-location-with-inferiors-form
	  '(concatenate 'string library-location
	                (wild-inferior-directories cl.env:*operating-system*)))

	 (source-location-with-inferiors-form
	  '(if (and source-location (string/= "" source-location))
	       (concatenate 'string source-location
		 	    (wild-inferior-directories
			     cl.env:*operating-system*))
	       (concatenate 'string library-location
		            (wild-inferior-directories
			     cl.env:*operating-system*))))
	 
	 #|(main-translations `('("**;*.*.*" ,library-location-with-inferiors)
			      '("**;*.*" ,library-location-with-inferiors)
			      '("*.*" ,library-location)))|#
	 ;; (main-translations (build-main-translations library-location))

	 (source-translations
	  `((list "SOURCE;**;*.*.*" ,source-location-with-inferiors-form)
	    (list "SOURCE;**;*.*" ,source-location-with-inferiors-form)
	    (list "SOURCE;*.*" (if (and source-location
					(string/= "" source-location))
				   source-location
				   library-location))))

	 (main-translations
	  `(,@source-translations
	    (list "**;*.*.*" ,library-location-with-inferiors-form)
	    (list "**;*.*" ,library-location-with-inferiors-form)
	    (list "*.*" library-location)
	    ))

	 (hosts-table (make-hash-table :test #'equal))
	 (hosts
	  (delete nil
		  (delete-duplicates (cons logical-host-string
					   (mapcar #'special-translation-host
						   special-translations))
				     :test #'equal)))
	 )
    
    ;; Fill in the convenience table
    (dolist (translation special-translations)
      (let ((host (or (special-translation-host translation)
		      logical-host-string)))
	(setf (gethash host hosts-table)
	      (nconc (special-translation-translations translation)
		     (gethash host hosts-table)))))

    ;; The main host must be treated specially
    ;; (setf (gethash logical-host-string hosts-table)
    ;;    (nconc main-translations
    ;;		 (gethash logical-host-string hosts-table)))
		 
    (loop for host in hosts
	  ;; do (format t ">>>> building translation form for host ~S~%" host)
	  if (string-equal host logical-host-string)
	    ;; The special translations must be first, since the "regular"
   	    ;; translations function as catch-alls.

	    collect `(setf (logical-pathname-translations ,host)
			   (list ,@(append (build-translation-form
					    host
					    (gethash host hosts-table))
					   main-translations)))
	  else
	    collect `(setf (logical-pathname-translations , host)
			   (list ,@(build-translation-form
				    host
				    (gethash host hosts-table)))))))


(defun build-translation-form (host translations)
  (declare (ignore host))
  (flet ((munge-translation (transl)
	   (let* ((spec (tr-spec transl))
		  (translation (tr-translation transl))
		  (prefix (tr-prefix transl))
		  (configuration-key (tr-prefix-configuration-key transl))
		  (os-type (tr-os-type transl))
		  )
	     (cond ((or (null os-type) (check-os-type-by-tag-p os-type))
		    (if configuration-key
			`(list ,spec (concatenate 'string
						  ,(build-var-from-keyword
						    configuration-key)
						  ,translation))
			(case prefix
			  (:source-location
			   `(list ,spec (concatenate 'string
						     source-location
						     ,translation)))
			  (:library-location
			   `(list ,spec (concatenate 'string
						     library-location
						     ,translation)))

			  (t
			   ;; In this case it should be the empty string.
			   `(list ,spec ,(concatenate 'string
						      prefix
						      translation))))
			))
		   (t (list configuration-key os-type spec translation)))
	     ))
	 )
    (loop for transl in translations
	  collect (munge-translation transl))))


;;;---------------------------------------------------------------------------
;;; Required Modules Clauses

(defstruct (required-module-clause (:include clause))
  name
  (components () :type list))


(defmethod parse-conf-clause ((key (eql :required-module)) clause)
  (destructuring-bind (clause-id module-name
				 &key
				 (pathnames-components ())
				 os-type)
      clause
    (make-required-module-clause :kind clause-id
				 :os-type os-type
				 :name module-name
				 :components pathnames-components)))

(defmethod build-conf-clause-code ((key (eql :required-module))
				   (clause required-module-clause))
  (let ((module-name (required-module-clause-name clause))
	(pathnames-components (required-module-clause-components clause))
	)
    `(handler-case
      (progn
	(configure-format *standard-output*
			  "checking required module ~S.~%"
			  ',module-name)
	(require ',module-name ',pathnames-components))
      (file-error (fe) (error fe))
      (type-error (te) (error fe))
      (error (e) (error e)))))


(defun build-required-module-forms (required-module-clauses)
  (build-forms required-module-clauses))


;;;---------------------------------------------------------------------------
;;; Required systems.

(defstruct (required-system-clause (:include clause))
  name
  (file-namestring "" :type string)
  (location "" :type string)
  (system-type :mk :type symbol))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *known-defsystem-implementations*
    '(:mk :make :mk-defsystem		; MK:DEFSYSTEM
      :allegro
      :genera
      :lispworks
      :lcl
      :mcl
      :pcl				; Portable Common Loops (CLOS)
      )))

(deftype known-defsystem-implementations ()
  (cons 'member *known-defsystem-implementations*))

(defmethod parse-conf-clause ((key (eql :required-system)) clause)
  (destructuring-bind (clause-id
		       system-name
		       &key
		       os-type
		       cl-implementation
		       (system-file-namestring 
			(concatenate 'string (string system-name) ".system"))
		       (system-location
			(concatenate 'string
				     (string-upcase (string system-name))
				     ":"))
		       (system-type :mk)
		       &allow-other-keys)
      clause
    (declare (type string system-location)
	     (type known-defsystem-implementations system-type))
    (make-required-system-clause :kind clause-id
				 :name system-name
				 :file-namestring system-file-namestring
				 :location system-location
				 :system-type system-type
				 :os-type os-type
				 :cl-implementation cl-implementation)))


(defmethod build-conf-clause-code ((key (eql :required-system))
				   (clause required-system-clause))
  ;; Resignalling in the proper context.
  ;;
  ;; I know this is long winded, but there is space for
  ;; future expansion right here.
  ;;
  ;; 19990822 Marco Antoniotti
  (let ((system-name (required-system-clause-name clause))
	(system-type (required-system-clause-system-type clause))
	(system-file-namestring
	 (required-system-clause-file-namestring clause))
	(system-location (required-system-clause-location clause))
	)
    `(handler-case
      (progn
	(configure-format *standard-output*
			  "checking required system ~S.~%"
			  ',system-name)
	
	(unless (defsys-type-available-p cl.env:*common-lisp-implementation*
		                         ',system-type)
	  (error 'configuration-error
		 :message-control "CL CONFIGURATION: no defsystem of type ~
                                   ~S is available in ~S."
		 :message-arguments
		 (list
		  ',system-type
		  (cl.env:common-lisp-implementation-type
		   cl.env:*common-lisp-implementation*))))
	
	(let ((system-found-p
	       (find-system ',system-name
			    cl.env:*common-lisp-implementation*
			    ',system-type)))

	  ;; Let's try to load it.
	  ;; I need some extra pathname magic to make this
	  ;; work!
		   
	  (unless system-found-p
	    (load (format nil "~A~A"
			  ',system-location
			  ',system-file-namestring))
	    (load-system ',system-name
			 cl.env:*common-lisp-implementation*
			 ',system-type))
	  ))
      (file-error (fe) (error fe))
      (type-error (te) (error fe))
      (error (e) (error e)))))

(defun build-required-system-forms (required-system-clauses)
  (build-forms required-system-clauses))

#| Old version
   20000226 Marco Antoniotti
(defun build-required-system-forms (required-system-clauses)
  (flet ((build-required-system-clause (rmc)
	   (destructuring-bind (clause-id
				system-name
				&key
				(system-file-namestring
				 (concatenate 'string
					      (string system-name)
					      ".system"))
				(system-location
				 (concatenate
				  'string
				  (string-upcase (string system-name))
				  ":"))
				(system-type :mk))
	       rmc
	     (declare (ignore clause-id)
		      (type string system-location)
		      (type (member :mk
				    :allegro
				    :genera
				    :lispworks
				    :lcl
				    :mcl
				    :pcl)
			    system-type))
	     ;; Resignalling in the proper context.
	     ;;
	     ;; I know this is long winded, but there is space for
	     ;; future expansion right here.
	     ;;
	     ;; 19990822 Marco Antoniotti
	     `(handler-case
	       (progn
		 (configure-format *standard-output*
				   "checking required system ~S.~%"
				   ',system-name)
		 (let ((system-found-p
			(find-system ',system-name
				     cl.env:*common-lisp-implementation*
				     ',system-type)))

		   ;; Let's try to load it.

		   ;; SUN CHI!
		   ;; I need some extra pathname magic to make this
		   ;; work!
		   
		   (unless system-found-p
		     (load (format nil "~A~A"
				   ',system-location
				   ',system-file-namestring))
		     (load-system ',system-name
				  cl.env:*common-lisp-implementation*
				  ',system-type))
		   ))
	       (file-error (fe) (error fe))
	       (type-error (te) (error fe))
	       (error (e) (error e)))))
	 )
    (mapcar #'build-required-system-clause required-system-clauses)))
|#

;;;---------------------------------------------------------------------------
;;; Required package forms

(defstruct (required-package-clause (:include clause))
  name
  location
  (components () :type list)
  (do-load-p nil :type (member nil t)))

(defmethod parse-conf-clause ((key (eql :required-package)) clause)
  (destructuring-bind (clause-id
		       package-name
		       &key
		       (package-location
			(current-directory-namestring
			 cl.env:*common-lisp-implementation*))
		       (package-components ())
		       (do-load-p nil)
		       os-type
		       cl-implementation
		       &allow-other-keys
		       )
      clause
    (make-required-package-clause :kind clause-id
				  :os-type os-type
				  :cl-implementation cl-implementation
				  :name package-name
				  :location package-location
				  :components package-components
				  :do-load-p do-load-p)))


(defmethod build-conf-clause-code ((key (eql :required-package))
				   (clause required-package-clause))
  (let ((package-name (required-package-clause-name clause))
	(package-location (required-package-clause-location clause))
	(package-components (required-package-clause-components clause))
	(do-load-p (required-package-clause-do-load-p clause))
	)
    `(handler-case
      (progn
	(configure-format *standard-output*
			  "checking required package ~S.~%"
			  ',package-name)
	(unless (find-package ',package-name)
	  ;; Try loading it.
	  ;; Note:
	  ;; I need to do directory tricks.
	  ;; The 'components' are always thought to be "stuck"
	  ;; at the end of the 'package-location'.
	  ;;
	  ;; 19990823 Marco Antoniotti
		   
	  ,(if (and do-load-p package-components)
	      `(let ((pkg-location-pathname (or ',package-location "")))
		 (dolist (p ',package-components)
		   (load (merge-pathname pkg-location-pathname p))))
	      `(configure-format "package ~S not found and not loaded."
				 ',package-name)
	      )))
      (file-error (fe) (error fe))
      (type-error (te) (error fe))
      (error (e) (error e)))
    ))

(defun build-required-package-forms (required-package-clauses)
  (build-forms required-package-clauses))

#| Old Version
   20000226 Marco Antoniotti
(defun build-required-package-forms (required-package-clauses)
  (flet ((build-required-package-clause (rmc)
	   (destructuring-bind (clause-id
				package-name
				&key
				(package-location
				 (current-directory-namestring
				  cl.env:*common-lisp-implementation*))
				(package-components ())
				(do-load-p nil)
				)
	       rmc
	     (declare (ignore clause-id))
	     ;; Resignalling in the proper context.
	     ;;
	     ;; I know this is long winded, but there is space for
	     ;; future expansion right here.
	     ;;
	     ;; 19990822 Marco Antoniotti
	     `(handler-case
	       (progn
		 (configure-format *standard-output*
				   "checking required package ~S.~%"
				   ',package-name)
		 (unless (find-package ',package-name)
		   ;; Try loading it.
		   ;; Note:
		   ;; I need to do directory tricks.
		   ;; The 'components' are always thought to be "stuck"
		   ;; at the end of the 'package-location'.
		   ;;
		   ;; 19990823 Marco Antoniotti
		   
		   (when ',do-load-p
		     (let ((pkg-location-pathname
			    (translate-logical-pathname ',package-location)))
		       (dolist (p ',package-components)
			 (load (merge-pathname pkg-location-pathname p)))
		       ))))
	       (file-error (fe) (error fe))
	       (type-error (te) (error fe))
	       (error (e) (error e))))
	   ))
    (mapcar #'build-required-package-clause required-package-clauses)))
|#

;;;---------------------------------------------------------------------------
;;; Other clauses.

(defun build-other-clauses-forms (other-clauses)
  (loop for clause in other-clauses
	for c = (build-conf-clause-code (first clause)
					(parse-conf-clause (first clause)
							   clause))
	when c collect c))


;;;---------------------------------------------------------------------------
;;; Utility functions.

(defun collect-keywords (special-translations)
  (loop for special-translation in special-translations
	nconc (mapcan #'(lambda (transl)
			  (let ((kwd (tr-prefix-configuration-key transl)))
			    (when kwd
			      (list (list kwd (tr-prefix transl))))))
		      (special-translation-translations special-translation))))


(defun build-var-from-keyword (key)
  (intern (symbol-name key) (find-package "CL.EXT.CONFIGURATION")))

(defun build-special-keyword-args (keywords)
  (loop for (key default) in keywords
	collect `((,key ,(build-var-from-keyword key)) ,default)))


(defun build-forms (unparsed-clauses)
  (loop for unparsed-clause in unparsed-clauses
	for parsed-clause = (parse-conf-clause (first unparsed-clause)
					       unparsed-clause)
	for form = (when (platform-compatible-clause-p parsed-clause)
		     (build-conf-clause-code (clause-kind parsed-clause)
					     parsed-clause))
	when form collect form))
					     

(defun check-os-type-by-tag-p (os-type-tag
			       &optional (os cl.env:*operating-system*))
  (and os-type-tag
       (or (eq os-type-tag (cl.env:os-feature-tag cl.env:*operating-system*))
	   (typep os (cl.env:find-os-class os-type-tag)))))


(defun platform-compatible-clause-p (clause
				     &key
				     (operating-system
				      cl.env:*operating-system*)
				     (cl-implementation
				      cl.env:*common-lisp-implementation*)
				     (machine
				      cl.env:*machine*))
  (declare (type clause clause))
  (declare (ignore cl-implementation machine))
  ;; Just one conjunct for the time being...
  (and (or (null (clause-os-type clause))
	   (check-os-type-by-tag-p (clause-os-type clause) operating-system)))
  )


;;; end of file -- defconf.lisp --
