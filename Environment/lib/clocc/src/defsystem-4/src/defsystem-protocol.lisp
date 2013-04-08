;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; defsystem-protocol.lisp --

(in-package "MK4")


;;;---------------------------------------------------------------------------
;;; Global variables (many of which will disappear).

(defvar *default-sysdef-file-extension* "system")

(defvar *oos-test* t)

(defvar *oos-verbose* nil)

(defvar *load-source-instead-of-binary* nil)

(defvar *load-source-if-no-binary* nil)

(defvar *bother-user-if-no-binary* t)

(defvar *compile-during-load* :query)

(defvar *compile-and-load* t)
(defvar *compile-and-load-p* t)


(defvar *minimal-load* nil)

(defvar *reload-systems-from-disk* t)



;;;---------------------------------------------------------------------------
;;; Standard CL protocol.
;;; PRINT-OBJECT etc. etc.

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :identity t)
     (format stream "MK4:~A ~S"
	     (class-name (class-of c))
	     (component-name c))))

  
(defmethod print-object ((s system) stream)
  (print-unreadable-object (s stream :identity t)
     (format stream "MK4:SYSTEM ~S" (component-name s))))


(defmethod print-object ((s system-reference) stream)
  (print-unreadable-object (s stream :identity t)
     (format stream "MK4:SYSTEM& ~S" (component-name s))))


(defmethod print-object ((s standard-system) stream)
  (print-unreadable-object (s stream :identity t)
     (format stream "MK4:STANDARD-SYSTEM ~S" (component-name s))))


(defmethod print-object ((s simple-system) stream)
  (print-unreadable-object (s stream :identity t)
     (format stream "MK4:SIMPLE-SYSTEM ~S" (component-name s))))


;;;---------------------------------------------------------------------------
;;; Components classes.

(defparameter *components-classes-by-key*
  `((:system& . ,(find-class 'system-reference)) ; Pardon the C++-ism.
    (:system . ,(find-class 'standard-system))
    (:defsystem . ,(find-class 'defsystem))
    (:simple-system . ,(find-class 'simple-system))
    (:subsystem . ,(find-class 'subsystem))
    (:module . ,(find-class 'module))
    (:file . ,(find-class 'file))
    ;; (:private-file . ,(find-class 'private-file))
    (:cl-file . ,(find-class 'common-lisp-file))
    (:common-lisp-file . ,(find-class 'common-lisp-file))
    (:c-file . ,(find-class 'c-file))
    (:c-source-file . ,(find-class 'c-source-file))
    (:c-header-file . ,(find-class 'c-header-file))
    (:library . ,(find-class 'library))
    (:statically-linked-library . ,(find-class 'statically-linked-library))
    (:dynamically-linked-library . ,(find-class 'dynamically-linked-library))
    (:java-file . ,(find-class 'java-file))
    (:java-source-file . ,(find-class 'java-source-file))
    (:java-jar-file . ,(find-class 'java-jar-file))
    ))


(define-condition unknown-component-class (simple-error)
  ((unknown-component-class-designator
    :initarg :unknown-class-designator
    :reader unknown-component-class-designator)
   )
  (:report (lambda (cnd stream)
	     (user-message stream
			   "no component class designated by ~S was found."
			   (unknown-component-class-designator cnd))))
  )


(defun get-component-class-by-key (key)
  (rest (assoc key *components-classes-by-key* :test #'eq)))


(defun add-component-class-key (key component-class)
  (let ((comp-class (rest (get-component-class-by-key key))))
    (cond ((null comp-class)
	   (setf *components-classes-by-key*
		 (acons key component-class
			*components-classes-by-key*))
	   (values key component-class))
	  ((not (eq comp-class component-class))
	   (error "MK:DEFSYSTEM: trying to change the class associated to ~@
                   component key ~S.~@
                   Current class: ~S~%New class ~S."
		  key
		  comp-class
		  component-class))
	  (t
	   (values key component-class)))))


(defun get-component-class (component-class-designator)
  (etypecase component-class-designator
    (keyword (get-component-class-by-key component-class-designator))
    (symbol (find-class component-class-designator nil))
    (class component-class-designator)))


(defmethod determine-component-class ((component-key (eql :file))
				      (language (eql :common-lisp))
				      &key
				      &allow-other-keys)
  (get-component-class :common-lisp-file))


(defmethod determine-component-class ((component-key (eql :file))
				      (language (eql :java))
				      &key
				      &allow-other-keys)
  (get-component-class :java-source-file))


(defmethod determine-component-class ((component-key (eql :file))
				      (language (eql :c))
				      &key
				      (header nil)
				      &allow-other-keys)
  (if header
      (get-component-class :c-header-file)
      (get-component-class :c-source-file)))


(defmethod determine-component-class ((component-key (eql :library))
				      (language (eql :c))
				      &key
				      (static nil)
				      &allow-other-keys)
  (if static
      (get-component-class :statically-linked-library)
      (get-component-class :dynamically-linked-library)))


(defmethod determine-component-class ((component-key (eql :library))
				      (language (eql :common-lisp))
				      &key
				      &allow-other-keys)
  (get-component-class :library))


(defmethod determine-component-class ((component-key (eql :library))
				      (language (eql :java))
				      &key
				      &allow-other-keys)
  ;; This should be :jar-file.
  (get-component-class :java-jar-file))


(defmethod determine-component-class ((component-type symbol)
				      (language symbol)
				      &key
				      &allow-other-keys)
  (get-component-class component-type))



;;; Language manipulation --

(defmethod component-language ((c component))
  (let ((l (call-next-method)))
    (if l
	l
	(let ((container (component-part-of c)))
	  (when container
	    (setf (component-language c) (component-language container)))))))


;;; If you use strings for system names, be sure to use the same case
;;; as it appears on disk, if the filesystem is case sensitive. 


;;; canonicalize-system-name --
;;;
;;; Notes.
;;; 2001-03-13: Notes from version 3.2i.
;;;
;;; Originally we were storing systems using GET. This meant that the
;;; name of a system had to be a symbol, so we interned the symbols
;;; in the keyword package to avoid package dependencies. Now that we're
;;; storing the systems in a hash table, we've switched to using strings.
;;; Since the hash table is case sensitive, we use uppercase strings.
;;; (Names of modules and files may be symbols or strings.)

(defun canonicalize-system-name (name)
  "Transforms the system name into a string.
NAME can be a symbol or a string.  Systems are stored into a EQUALP
hash table, so the case of the string does not matter."
  (declare (type (or symbol string) name))
  (etypecase name
    (string name)
    (symbol (string name))))


(defvar *defined-systems* (make-hash-table :test #'equalp)
  "Hash table containing the definitions of all known systems.")


(define-condition system-not-found (simple-error)
  ((name :reader system-not-found-system-name
	 :initarg :system-name)
   )
  (:report (lambda (cnd stream)
	     (format stream
		     "MK4: system ~S was not found anywhere."
		     (system-not-found-system-name cnd))))
  )


(defun get-system (name &optional not-found-error-p not-found-value)
  "Returns the definition of the system named NAME."
  (multiple-value-bind (sys foundp)
      (gethash (canonicalize-system-name name)
	       *defined-systems*
	       not-found-value)
    (when (and not-found-error-p (not foundp))
      (error 'system-not-found
	     :system-name (canonicalize-system-name name)))
    sys))


(defsetf get-system (name) (value)
  `(setf (gethash (canonicalize-system-name ,name) *defined-systems*) ,value))


(defun undefsystem (name)
  "Removes the definition of the system named NAME."
  (setf (get-system name) nil))


(defun defined-systems (&optional (sort-function #'string<))
  "Returns a list of defined systems."
  (loop for sys being the hash-value of *defined-systems*
	collect sys into defined-sys
	finally (if sort-function
		    (return (sort defined-sys sort-function
				  :key #'component-name))
		    (return defined-sys))))


(defun find-system (system-name
		    &optional
		    (mode :error)
		    system-definition-pathname
		    &key
		    (reload-systems-definitions-from-fs
		     *reload-systems-from-disk*)
		    (current-directory-p t)
		    (user-homedir-pathname-p t)
		    )
  "Returns the system named SYSTEM-NAME.
If the system is not already loaded, it loads it.  This allows
EXECUTE-ACTION to work on non-loaded as well as loaded system
definitions. SYSTEM-DEFINITION-PATHNAME is the pathname for the system
definition, if provided.
MODE determines the behavior of the function. MODE can have the
following values.
:QUERY	asks the user for instructions about how to proceed. (This
        option is deprecated).
:ERROR	generates an error if the system has not been defined
	(i.e. the system definition has never been loaded) yet.
:LOAD-OR-NIL	tries to load the system definition from a pathname,
		possibly generated from SYSTEM-DEFINITION-PATHNAME. If
		the LOAD operation fails return NIL.
:LOAD		as above, but generates an error if the LOAD operation fails.
No other values are allowed (with the exception of :ASK - a synonim for
:QUERY - for backward compatibility)."
  (declare (ignore user-homedir-pathname-p current-directory-p))
  (ecase mode
    ((:ask :query)			; Backward compatibility.
     (user-message *defsystem-error-output*
		   "warning: argument ~S to FIND-SYSTEM is deprecated."
		   mode)
     (or (get-system system-name)
	 (when (y-or-n-p-wait
		#\y 20
		"System ~A not loaded. Shall I try loading it? "
		system-name)
	   (find-system system-name
			:load
			system-definition-pathname
			:reload-systems-definitions-from-fs
			reload-systems-definitions-from-fs
			))))
    (:error
     (or (get-system system-name)
	 (error 'system-not-found :system-name system-name)))
    (:load-or-nil
     (let ((system (get-system system-name)))
       (unless reload-systems-definitions-from-fs
	 (when system
	   (return-from find-system system)))

       ;; If SYSTEM-NAME is a symbol, it will lowercase the
       ;; symbol's string.
       ;; If SYSTEM-NAME is a string, it doesn't change the case of the
       ;; string. So if case matters in the filename, use strings, not
       ;; symbols, wherever the system is named.
       (let ((path
	      (ignore-errors (compute-system-definition-file
			      system-name
			      :definition-pathname system-definition-pathname
			      :if-does-not-exist nil)))
	     (freshly-loaded-system nil)
	     )
	 (when (and path
		    (or (null system)
			(< (component-action-timestamp system :load)
			   (file-write-date path))))
	   (user-message *standard-output*
			 "loading system ~A from file ~A."
			 system-name
			 path))
	 (load path)
	 (setf freshly-loaded-system (get-system system-name))
	 (when freshly-loaded-system
	   (setf (component-action-timestamp freshly-loaded-system :load)
		 (file-write-date path))
	   (return-from find-system freshly-loaded-system)))
       (return-from find-system system)))
    (:load
     (or (unless reload-systems-definitions-from-fs (get-system system-name))
	 (or (find-system system-name :load-or-nil system-definition-pathname)
	     (error 'system-not-found :system-name system-name))))))


;;;---------------------------------------------------------------------------
;;; DESCRIBE protocol.
;;; Some 3.x cruft is still in these functions.

(defvar *describe-recur-on-subcomponents* nil)
(defvar *describe-component-level* 0)

(defun describe-system (name &optional
			     (stream *standard-output*)
			     (recursive *describe-recur-on-subcomponents*))
  "Prints a description of the system to the STREAM.
If NAME is the name of a system, gets it and prints a description of
the system.  If NAME is a component, prints a description of the
component."
  (let ((system (if (system-p name) name (find-system name :error)))
	(*describe-recur-on-subcomponents* recursive)
	)
    (describe system stream)))


(defmethod describe-object ((c component) (stream stream))
  (let ((csp (get-component-source-pathname c))
	(cse (get-component-source-extension c))
	(cbp (get-component-binary-pathname c))
	(cbe (get-component-binary-extension c))
	)
    (format stream "~&MK4: `~A' is a system component ~A: ~
                    ~@[~&   Part of: ~A~]~
                    ~@[~&   Action Timestamp: ~D~]~
                    ~@[~&   Host: ~A~]~
                    ~@[~&   Device: ~A~]~
                    ~@[~&   Package: ~A~]~
                    ~&   Source: ~@[~A~] ~@[~A~] ~@[(~A)~]~
                    ~&   Binary: ~@[~A~] ~@[~A~] ~@[(~A)~]~
                    ~@[~{~&   Depends On: ~A ~}~]"
	    (component-name c)
	    (class-name (class-of c))	; (component-type c)
	    (component-part-of c)
	    (component-changed-timestamp c)
	    (component-host c)
	    (component-device c)
	    (component-package c)

	    nil				; (component-root-dir c :source)
	    csp
	    cse				; (component-extension c :source)

	    nil				; (component-root-dir c :binary)
	    cbp
	    cbe				; (component-extension c :binary)
	    (component-depends-on c)
	    )
    c))


(defmethod describe-object ((c library) (stream stream))
  (let ((csd (get-component-source-directory c))
	(cse (get-component-source-extension c))
	(cbp (get-component-binary-pathname c))
	(cbe (get-component-binary-extension c))
	)
    (format stream "~&MK4: `~A' is a system component ~A: ~
                    ~@[~&   Part of: ~A~]~
                    ~@[~&   Action Timestamp: ~D~]~
                    ~@[~&   Host: ~A~]~
                    ~@[~&   Device: ~A~]~
                    ~@[~&   Package: ~A~]~
                    ~&   Source Directory: ~@[~A~] ~@[~A~] ~@[(~A)~]~
                    ~&   Binary Pathname:  ~@[~A~] ~@[~A~] ~@[(~A)~]~
                    ~@[~{~&   Depends On: ~A ~}~]"
	    (component-name c)
	    (class-name (class-of c))	; (component-type c)
	    (component-part-of c)
	    (component-changed-timestamp c)
	    (component-host c)
	    (component-device c)
	    (component-package c)

	    nil				; (component-root-dir c :source)
	    csd
	    cse				; (component-extension c :source)

	    nil				; (component-root-dir c :binary)
	    cbp
	    cbe				; (component-extension c :binary)
	    (component-depends-on c)
	    )
    c))


(defmethod describe-object :after ((c standard-hierarchical-component)
				   (stream stream))
  (let ((*describe-component-level* (1+ *describe-component-level*)))
    (format stream "~&   Components:~
                  ~:[none~;~:*~{~15T~A~&~}~]"
	    (component-components c))
    (when *describe-recur-on-subcomponents*
      (terpri stream)
      (dolist (component (component-components c))
	(describe-object component stream)))
    c))


;;;---------------------------------------------------------------------------
;;; Some components' protocol.

(defmethod binary-exists-p ((f file)
			    &optional
			    (binary-pathname
			     (get-component-binary-pathname f)))
  (declare (type (or null string stream pathname) binary-pathname))
  (and binary-pathname (probe-file binary-pathname)))


(defmethod source-exists-p ((f file)
			    &optional
			    (source-pathname
			     (get-component-source-pathname f)))
  (declare (type (or null string stream pathname) source-pathname))
  (and source-pathname (probe-file source-pathname)))


(defun canonicalize-component-name (component)
  (declare (type component component))
  ;; Within the component, the name is a string.
  (if (stringp (component-name component))
      ;; Unnecessary to change it, so just return it, same case
      (component-name component)
      ;; Otherwise, make it a string following the diktat of
      ;; COMPONENT-NAME-CASE, whose default in :DOWNCASE.
      (ecase (component-name-case component)
	(:downcase
	 (setf (component-name component) 
	       (string-downcase (string (component-name component)))))
	(:uppercase
	 (setf (component-name component) 
	       (string-upcase (string (component-name component)))))
	(:preserve
	 (setf (component-name component) 
	       (string (component-name component)))))
      ))


;;;---------------------------------------------------------------------------
;;; System Definition

;;; Component creation protocol (CCP).
;;;
;;; CONSTRUCT-COMPONENT: the real work horse which calls MAKE-INSTANCE
;;;                      on the component's class
;;;
;;; ADD-SUB-COMPONENT: (for lack of a better name) this function
;;;                    completes the component insertion in the
;;;                    hierarchy.  It is called by the
;;;                    INITIALIZE-INSTANCE :AFTER method on
;;;                    HIERARCHICAL-COMPONENT.
;;;
;;; CONSTRUCT-COMPONENT is not strictly necessary, but it is nice to
;;; have it.

(defgeneric construct-component (component-class-or-keyword
				 name
				 &rest keys
				 &key
				 &allow-other-keys))


(defmethod construct-component ((component-type symbol)
				name
				&rest keys
				&key
				&allow-other-keys)
  (declare (type (or string symbol) name))
  (let ((component-class (get-component-class component-type)))
    (unless component-class
      (error 'unknown-component-class
	     :unknown-class-designator component-type))
    (apply #'construct-component component-class name keys)))


(defmethod construct-component ((component-type standard-class)
				name
				&rest keys
				&key
				&allow-other-keys)
  (declare (type (or string symbol) name))
  (apply #'make-instance component-type :name name keys))


;;; initialize-instance -- :after on component.

(defmethod initialize-instance :after ((c component) &key)
  ;; Initializations/after makes
  (canonicalize-component-name c))


(defmethod initialize-instance :after ((c file) &key)
  (with-accessors ((csp component-source-pathname)
		   (cbp component-binary-pathname)
		   )
    c
    (unless csp
      (let ((name-as-pathname (parse-namestring (string (component-name c)))))
	(setf csp name-as-pathname)))
    (unless cbp
      (setf cbp csp))
    ))


;;; initialize-instance -- :after on system.

(defmethod initialize-instance :after ((c system) &key)
  (with-accessors ((csp component-source-pathname)
		   (cbp component-binary-pathname)
		   (name component-name)
		   )
    c
    (setf (get-system name) c)
    (unless csp
      (setf csp (cl.env:current-working-directory)))
    (unless cbp
      (setf cbp csp))
    ))


(defmethod initialize-instance :after ((c system-reference) &key)
  (with-accessors ((csp component-source-pathname)
		   (cbp component-binary-pathname)
		   (name component-name)
		   )
    c
    (unless csp
      (setf csp (cl.env:current-working-directory))) ; Change this to
						     ; *CENTRAL-REGISTRY* or
                                                     ; similar.
   
    (unless cbp
      (setf cbp csp))			; As above.
    ))


(defmethod initialize-instance :after ((c subsystem) &key)
  (with-accessors ((csp component-source-pathname))
    c
    (unless csp
      (setf csp
	    (make-pathname :directory (list :relative (component-name c)))))
    ))


(defmethod initialize-instance :after ((c module) &key)
  (with-accessors ((csp component-source-pathname)
		   (cbp component-binary-pathname)
		   )
    c
    (unless csp
      (setf csp
	    (make-pathname :directory (list :relative (component-name c)))))
    (unless cbp
      (setf cbp csp))
    ))


(defmethod initialize-instance :after ((c library) &key)
  (with-accessors ((csp component-source-pathname)
		   (cbp component-binary-pathname)
		   )
    c
    (unless csp
      (let ((name-as-pathname (parse-namestring (string (component-name c)))))
	(setf csp name-as-pathname)))
    (unless cbp
      (setf cbp csp))
    ))


(defun link-component-depends-on (components)
  (dolist (c components)
    (setf (component-depends-on c)
	  (mapcar #'(lambda (dependency)
		      (let ((dep (find (string dependency) components
				       :key #'component-name
				       :test #'string-equal)))
			(if dep
			    dep
			    ;; make it more intelligent about the
			    ;; following.
			    (warn "Dependency ~S of component ~S not found."
				  dependency c))))
		  (component-depends-on c)))))


;;; initialize-instance -- :after on hierarchical-component.

(defmethod initialize-instance :after ((hc hierarchical-component)
				       &key
				       (components ())
				       &allow-other-keys)
  (dolist (sub-c components)
    (add-sub-component hc sub-c))

  ;; :depends-on massaging.
  (link-component-depends-on components))


;;; initialize-instance -- :after on stored-component.
;;; Make sure that the computed extensions are in place.

(defmethod initialize-instance :after ((sc stored-component)
				       &key
				       (binary-extension nil bext-supplied-p)
				       (source-extension nil sext-supplied-p)
				       &allow-other-keys)
  (let ((comp-lang (component-language sc)))

    (cond ((and bext-supplied-p binary-extension)
	   (setf (computed-binary-extension sc) binary-extension))
	  ((and comp-lang (languagep comp-lang))
	   (setf (computed-binary-extension sc)
		 (language-binary-extension comp-lang))))
    
    (cond ((and sext-supplied-p source-extension)
	   (setf (computed-source-extension sc) source-extension))
	  ((and comp-lang (languagep comp-lang))
	   (setf (computed-source-extension sc)
		 (language-source-extension comp-lang))))))


;;; add-sub-component --

(defmethod add-sub-component ((hc hierarchical-component)
			      (c component))
  (setf (component-part-of c) hc))


(defmethod add-sub-component ((ss simple-system)
			      (f file))
  (setf (component-part-of f) ss))


(defmethod add-sub-component ((ss simple-system)
			      (c component))
  (error "Cannot add a component of class ~S to simple system ~S.~@
          Only FILE components can be added to SIMPLE-SYSTEMs."
	 (class-name (class-of c))
	 ss))


;;;---------------------------------------------------------------------------
;;; Pathname management.
;;; The following section contains a lot of duplicated code.
;;; I prefer this solution to the set of boilerplate functions that
;;; are part of MK:DEFSYSTEM 3.2.
;;; If you are modifiying anything in this section, please be careful
;;; to propagate all the changes to all the functions.
;;; Please also be careful with the DIRTY-BIT computation.
;;;
;;; The pathname computation algorithm is radically different from the
;;; one in MK:DEFSYSTEM 3.2.
;;; Basically, each time we ask for a pathanme (source or binary) the
;;; actual result is recomputed (if needed, see the dirty bit
;;; machinery) from the public component specs (source- and binary-
;;; pathname and extensions) and from the 'container's appropriate
;;; pathname.
;;; Most of the work is delegated to the ADJOIN-DIRECTORIES generic
;;; function, with just one extra bit: the extension is tacked on the
;;; resulting pathname using MERGE-PATHNAMES (only for 'file'
;;; components).  This implies that the :*-extension specification is
;;; superseded by the :*-pathname specification.
;;;
;;; Example:
;;; We have a components with specs
;;;     :source-pathname "I/am/here.type"
;;;     :source-extension "kind"
;;
;;; The result of calling GET-COMPONENT-SOURCE-PATHNAME will be
;;;
;;;     #p"<container pathname>/I/am/here.type"
;;;
;;; I.e. the pathname type will not be "kind".

(defgeneric set-source-pathname-dirty-bit (component))
(defgeneric set-binary-pathname-dirty-bit (component))


;;; update-source-pathname-components-from-container --
;;; This function will eventually be used to coalesce functionality
;;; now spread around several INITIALIZE-INSTANCE.

(defun update-source-pathname-components-from-container (component container)
  (declare (type component component container)
	   (ignore container))
  (with-accessors ((source-pathname component-source-pathname)
		   (computed-source-pathname computed-source-pathname)
		   (source-extension component-source-extension)
		   (computed-source-extension computed-source-extension)
		   (container component-part-of)
		   )
    component
    (if container
	(let ((container-source-directory
	       (get-component-source-directory container)))
	  (setf computed-source-pathname
		(adjoin-directories container-source-directory
				    source-pathname))
	  (if source-extension
	      (setf computed-source-extension
		    source-extension)
	      (setf computed-source-extension
		    (get-component-source-extension container))))
	  )))


;;; get-component-source-pathname --

(defmethod get-component-source-pathname ((sc stored-component))
  ;; I should add some "consistency checks" here.
  ;; Namely I should check that the computed pathname has :name and
  ;; :type NIL (or :UNSPECIFIC) and that the directory components is
  ;; meaningful.
  (with-accessors ((source-pathname component-source-pathname)
		   (computed-source-pathname computed-source-pathname)
		   (source-extension component-source-extension)
		   (computed-source-extension computed-source-extension)
		   (container component-part-of)
		   )
    sc

    ;; First of all we must see whether we need to recompute the pathname.
    (if (source-pathname-computations-dirty-bit sc)

	;; 'container' will almost always be non NULL except for
	;; SYSTEM components, the check allows for on-the-fly
	;; computations on components that have not been installed
	;; in a system tree.
	
	(if container
	    (let ((container-source-directory
		   (get-component-source-directory container)))
	      (if source-extension
		  (setf computed-source-extension source-extension)
		  (setf computed-source-extension
			(get-component-source-extension container)))
	      (setf computed-source-pathname
		    (adjoin-directories container-source-directory
					source-pathname)))

	    (etypecase source-pathname
	      (string
	       (setf computed-source-pathname
		     (parse-namestring source-pathname)))
	      (pathname
	       (setf computed-source-pathname source-pathname)))
	    )
	;; Otherwise just return the previously stored
	;; 'computed-source-pathname'.
	computed-source-pathname)))


;;; Whenever I compute the source pathname, I have to make sure that
;;; the dirty bit is properly set in the component.  Hence the :after method.

(defmethod get-component-source-pathname :after ((sc storage-component-mixin))
  (setf (source-pathname-computations-dirty-bit sc) nil))


(defmethod get-component-source-pathname ((f file))
  (if (source-pathname-computations-dirty-bit f)
      (let ((source-extension-pathname
	     (make-pathname :type (get-component-source-extension f))))
	(setf (computed-source-pathname f)
	      (merge-pathnames source-extension-pathname (call-next-method))))
      (computed-source-pathname f)))


(defmethod get-component-source-directory ((sc stored-component))
  (let ((csp (get-component-source-pathname sc)))
    ;; Just return a pathname with all the name components properly stripped.
    (make-pathname :host (pathname-host csp)
		   :device (pathname-device csp)
		   :directory (pathname-directory csp)
		   :name nil
		   :type nil
		   :version (pathname-version csp))))

;;; get-component-binary-pathname --

(defmethod get-component-binary-pathname ((sc stored-component))
  ;; I should add some "consistency checks" here.
  ;; Namely I should check that the computed pathname has :name and
  ;; :type NIL (or :UNSPECIFIC) and that the directory components is
  ;; meaningful.
  (with-accessors ((binary-pathname component-binary-pathname)
		   (computed-binary-pathname computed-binary-pathname)
		   (binary-extension component-binary-extension)
		   (computed-binary-extension computed-binary-extension)
		   (container component-part-of)
		   )
    sc
    ;; First of all we must see whether we need to recompute the pathname.
    (if (binary-pathname-computations-dirty-bit sc)
	
	;; 'container' will almost always be non NULL except for
	;; SYSTEM components, the check allows for on-the-fly
	;; computations on components that have not been installed
	;; in a system tree.

	(if container
	    (let ((container-binary-directory
		   (get-component-binary-directory container)))
	      (if binary-extension
		  (setf computed-binary-extension binary-extension)
		  (setf computed-binary-extension
			(get-component-binary-extension container)))

	      (setf computed-binary-pathname
		    (adjoin-directories container-binary-directory
					binary-pathname)))

	    (etypecase binary-pathname
	      (string
	       (setf computed-binary-pathname
		     (parse-namestring binary-pathname)))
	      (pathname
	       (setf computed-binary-pathname binary-pathname)))
	    )
	;; Otherwise just return the previously stored
	;; 'computed-binary-pathname'.
	computed-binary-pathname)))


;;; Whenever I compute the binary pathname, I have to make sure that
;;; the dirty bit is properly set in the component.  Hence the :after method.

(defmethod get-component-binary-pathname :after ((sc storage-component-mixin))
  (setf (binary-pathname-computations-dirty-bit sc) nil))


(defmethod get-component-binary-pathname ((f file))
  (if (binary-pathname-computations-dirty-bit f)
      (let ((binary-extension-pathname
	     (make-pathname :type (get-component-binary-extension f))))
	(setf (computed-binary-pathname f)
	      (merge-pathnames binary-extension-pathname (call-next-method))))
      (computed-binary-pathname f)))


(defmethod get-component-binary-pathname ((l library))
  (if (binary-pathname-computations-dirty-bit l)
      (let ((binary-extension-pathname
	     (make-pathname :type (get-component-binary-extension l))))
	(setf (computed-binary-pathname l)
	      (merge-pathnames binary-extension-pathname (call-next-method))))
      (computed-binary-pathname l)))


(defmethod get-component-binary-directory ((sc stored-component))
  (let ((cbp (get-component-binary-pathname sc)))
    ;; Just return a pathname with all the name components properly stripped.
    (make-pathname :host (pathname-host cbp)
		   :device (pathname-device cbp)
		   :directory (pathname-directory cbp)
		   :name nil
		   :type nil
		   :version (pathname-version cbp))))


#+not-yet
(defmethod get-component-error-log-pathname :after ((sc storage-component-mixin))
  (setf (binary-pathname-computations-dirty-bit sc) nil))


#+not-yet
(defmethod get-component-error-log-pathname ((f file))
  (if (binary-pathname-computations-dirty-bit f)
      (let ((binary-extension-pathname
	     (make-pathname :type (component-binary-extension f))))
	(setf (computed-binary-pathname f)
	      (merge-pathnames binary-extension-pathname (call-next-method))))
      (computed-binary-pathname f)))


;;; Very simple for the time being.

(defvar *error-extension-pathname* (make-pathname :type "err"))


(defmethod get-component-error-log-pathname ((f file))
  (merge-pathnames *error-extension-pathname*
		   (get-component-source-pathname f)))


;;; Dirty bits mangement.
;;; Four main methods to be invoked on the :after methods of all the
;;; component changing assignment methods.

(defmethod set-source-pathname-dirty-bit ((c standard-simple-component))
  (setf (source-pathname-computations-dirty-bit c) t))

(defmethod set-source-pathname-dirty-bit ((c standard-hierarchical-component))
  (setf (source-pathname-computations-dirty-bit c) t)
  (dolist (subc (component-components c) t)
    (set-source-pathname-dirty-bit subc)))


(defmethod set-binary-pathname-dirty-bit ((c standard-simple-component))
  (setf (binary-pathname-computations-dirty-bit c) t))

(defmethod set-binary-pathname-dirty-bit ((c standard-hierarchical-component))
  (setf (binary-pathname-computations-dirty-bit c) t)
  (dolist (subc (component-components c) t)
    (set-binary-pathname-dirty-bit subc)))

(defmethod (setf component-source-pathname) :after (v (c stored-component))
  (declare (ignorable v))
  (set-source-pathname-dirty-bit c))

(defmethod (setf component-binary-pathname) :after (v (c stored-component))
  (declare (ignorable v))
  (set-binary-pathname-dirty-bit c))

(defmethod (setf component-source-extension) :after (v (c stored-component))
  (declare (ignorable v))
  (set-source-pathname-dirty-bit c))

(defmethod (setf component-binary-extension) :after (v (c stored-component))
  (declare (ignorable v))
  (set-binary-pathname-dirty-bit c))

(defmethod get-component-binary-extension ((sc stored-component))
  (with-accessors ((binary-extension component-binary-extension)
		   (computed-binary-extension computed-binary-extension)
		   (container component-part-of)
		   )
    sc
    ;; First of all we must see whether we need to recompute the pathname.
    (if (binary-pathname-computations-dirty-bit sc)
	
	;; 'container' will almost always be non NULL except for
	;; SYSTEM components, the check allows for on-the-fly
	;; computations on components that have not been installed
	;; in a system tree.

	(if (and (not (null container)) (null binary-extension))
	    (let ((container-binary-extension
		   (get-component-binary-extension container)))
	      (setf computed-binary-extension container-binary-extension))
	    (setf computed-binary-extension binary-extension))
	;; Otherwise just return the previously stored
	;; 'computed-binary-extension'.
	computed-binary-extension)))


(defmethod get-component-source-extension ((sc stored-component))
  (with-accessors ((source-extension component-source-extension)
		   (computed-source-extension computed-source-extension)
		   (container component-part-of)
		   )
    sc
    ;; First of all we must see whether we need to recompute the pathname.
    (if (source-pathname-computations-dirty-bit sc)
	
	;; 'container' will almost always be non NULL except for
	;; SYSTEM components, the check allows for on-the-fly
	;; computations on components that have not been installed
	;; in a system tree.

	(cond ((and (not (null container)) (null source-extension))
	       (let ((container-source-extension
		      (get-component-source-extension container)))
		 (setf computed-source-extension container-source-extension)))
	      (t (setf computed-source-extension source-extension)))
	;; Otherwise just return the previously stored
	;; 'computed-source-extension'.
	computed-source-extension)))


;;;---------------------------------------------------------------------------
;;; Action execution. (As per KMP's definition).


(defvar *trace-action-execution* nil)


(defmethod output-pathnames ((c component) (action (eql :load)))
  '())

(defmethod output-pathnames ((c component) (action (eql :compile)))
  '())

(defmethod output-pathnames ((c file) (action (eql :compile)))
  (list (get-component-binary-pathname c)))

(defmethod output-files ((c component) (action symbol))
  (output-pathnames c action))



(defmethod execute-action :around ((s system) (operation symbol)
				   &key
				   &allow-other-keys)
  (unless (action-registered-p operation)
    (error 'unknown-system-operation
	   :operation operation
	   :system s))
  (call-next-method))


;;; This is not really different from the primary method on
;;; STANDARD-HIERARCHICAL-COMPONENT and SYMBOL.
;;; I left it defined separatedly just in case.  Originally it was a
;;; much more complicated method which did some context dependent
;;; stuff. Such context dependency has now been resolved by adding the
;;; SYSTEM-REFERENCE class.

(defmethod execute-action ((sys standard-system) (operation symbol)
			   &key
			   (policy :always)
			   &allow-other-keys)
  (execute-action-on-subcomponents sys operation :policy policy))


(defmethod execute-action ((sys-ref system-reference) (operation symbol)
			   &key
			   (policy :always)
			   &allow-other-keys)
  (with-accessors ((sys-instance system-instance))
    sys-ref
    (if sys-instance
	;; We already have the system associated to this reference.
	(execute-action sys-instance operation :policy policy)
	;; The reference is "dangling".
	;; First we FIND-SYSTEM the system instance, then we update
	;; the system reference, and finally we EXECUTE-ACTION on it.
	(let* ((system-def-pathname (get-component-source-pathname sys-ref))
	       (found-system (find-system (component-name sys-ref)
					  :load	; Generate an error if
						; not found.
					  system-def-pathname))
	       )
	  (setf sys-instance found-system)
	  (execute-action found-system operation :policy policy)))
    ))


(defmethod execute-action ((sys simple-system) (operation symbol)
			   &key
			   (policy :always)
			   &allow-other-keys)
  (let ((changed-components ()))
    (dolist (subcomponent (component-components sys) changed-components)
      (multiple-value-bind (result warning-p failure-p other-components)
	  (execute-action subcomponent operation :policy policy)
	(declare (ignore warning-p))
	(when (and result (not failure-p))
	  (setf changed-components
		(nconc (list subcomponent)
		       other-components
		       changed-components)))))
    (values sys nil nil (nreverse changed-components))
    ))


;;;---------------------------------------------------------------------------
;;; Timestamps

;;; Each component maintains a set of timestamps in its
;;; `timestamps-set' slot.  Each entry in the set is of the form
;;;
;;;	<<action-tag> <universal time>>
;;;
;;; where <action-tag> is the symbolic tag used to denote the action
;;; (e.g. :load or :compile), and <universal time> is either the last
;;; time the operation was performed on the component, or the last
;;; FILE-WRITE-DATE of a `stored' component, or a value derived from
;;; it.  (I.e. a "module" does not really have a File System
;;; representation, but a .c file does: hence the distinction)
;;;
;;; The following functions are used to access the timestamps.

(defgeneric component-action-timestamp (component action))

(defgeneric dependencies-changed-p (component action))

(defgeneric dependencies-latest-change-time (component action))


(defmethod component-action-timestamp ((c component) (action symbol))
  (let* ((ts-set (component-timestamps c))
	 (action-ts (assoc action ts-set))
	 )
    (if action-ts
	(cdr action-ts)
	0)))


(defmethod (setf component-action-timestamp) (time
					      (c component)
					      (action symbol))
  (let* ((ts-set (component-timestamps c))
	 (action-ts (assoc action ts-set))
	 )
    (if action-ts
	(setf (cdr action-ts) time)
	(setf (component-timestamps c) (acons action time ts-set))))
  time)


(defmethod component-last-timestamp ((c component))
  (loop with ts-set = (component-timestamps c)
	with max-timestamp = 0
	with last-action = nil
	for (action . timestamp) in ts-set
	if (< max-timestamp timestamp)
	   do (setf last-action action
		    max-timestamp timestamp)
	else if (and (= max-timestamp timestamp) (eq action :load))
	   do (setf last-action :load)
	finally (when last-action (return (cons last-action max-timestamp)))))


;;;---------------------------------------------------------------------------
;;; Dependencies.

;;; Dependencies are maintaned in a list in the DEPENDS-ON slot. The
;;; list contains either component designators or pairs of the form
;;;
;;;   (<component designator> <action-tag>+)
;;;
;;; where <action-tag> is the symbolic tag used to denote an action
;;; (e.g. :load or :compile) or a STRUCTURED-DEPENDENCY.
;;;
;;; Note that the STRUCTURED-DEPENDENCY-MODIFIER is unused.

;;; The next function deals with the backward compatibility issues.

(defun select-component-dependency (dep action)
  (declare (type dependency dep)
	   (type symbol action))
  (let ((dep-actions (dependency-actions dep)))
    ;; DEPENDENCY-ACTIONS returns () for simple, MK3-like dependencies
    ;; (it may return the empty list for other dependencies as well).
    (or (null dep-actions)
	(not (null (member action dep-actions :test #'eq))))))


;;; Main interface method.

(defmethod select-component-dependencies ((c component) (operation symbol))
  (loop for dependency in (component-depends-on c)
	if (select-component-dependency dependency operation)
          collect (dependency-component dependency)))



;;; Preparing the list of dependencies.

(defmethod topsorst-components-by-operation ((hc hierarchical-component)
					     (operation symbol))
  (let ((sorted-list '())
	(cs (component-components hc))
	)
    (labels ((dfs-visit (the-node)
	       (setf (topsort-color the-node) :gray)
	       (unless (and t ; *system-dependencies-delayed*
			    t ; (eq (component-type the-node) :system)
			    (systemp the-node)
			    )
		 (dolist (child (select-component-dependencies the-node
							       operation))
		   (cond ((eq (topsort-color child) :white)
			  (dfs-visit child))
			 ((eq (topsort-color child) :gray)
			  (warn "detected cycle containing ~S" child)))))
	       (setf (topsort-color the-node) :black)
	       (push the-node sorted-list)))
      (dolist (the-node cs)
	(setf (topsort-color the-node) :white))
      (dolist (the-node cs)
	(when (eq (topsort-color the-node) :white)
	  (dfs-visit the-node)))
      (nreverse sorted-list))))
  

(defmethod execute-action-on-subcomponents :before ((hc hierarchical-component)
						    (operation symbol)
						    &key
						    &allow-other-keys)
  ;; Compute the topological order of the sub-components based of the
  ;; current operation.
  (setf (component-components hc)
	(topsorst-components-by-operation hc operation)))


;;; These can be simplified now.

(defmethod execute-action-on-subcomponents ((hc hierarchical-component)
					    (operation (eql :load))
					    &key
					    (policy :always)
					    &allow-other-keys
					    )
  (let ((changed-subcomponents ()))
    (dolist (c (component-components hc))
      (let ((policy-for-subcomponent
	     (compute-component-policy c
				       :load
				       policy
				       changed-subcomponents)))
	(multiple-value-bind (result warning-p failure-p other-components)
	    (execute-action c :load :policy policy-for-subcomponent)
	  (declare (ignore warning-p))
	  (when (and result (not failure-p))
	    (setf changed-subcomponents
		  (nconc (list c)
			 other-components
			 changed-subcomponents))))))
    (values hc nil nil (nreverse changed-subcomponents))))


(defmethod execute-action-on-subcomponents ((hc hierarchical-component)
					    (operation (eql :compile))
					    &key
					    (policy :dependencies-changed)
					    (load *compile-and-load-p*)
					    &allow-other-keys
					    )
  (let ((changed-subcomponents ()))
    (dolist (c (component-components hc))
      (let ((policy-for-subcomponent
	     (compute-component-policy c
				       :compile
				       policy
				       changed-subcomponents)))
	(user-message t "executing on subc ~S~%     with policy ~S."
		      c
		      policy-for-subcomponent)
	(multiple-value-bind (result warning-p failure-p other-components)
	    (execute-action c :compile
			    :policy policy-for-subcomponent
			    :load load)
	  (declare (ignore warning-p))
	  (when (and result (not failure-p))
	    (setf changed-subcomponents
		  (nconc (list c)
			 other-components
			 changed-subcomponents))))))
    (values hc nil nil (nreverse changed-subcomponents))))



;;; Modules can depend only on siblings. If a module should depend
;;; on an uncle, then the parent module should depend on that uncle
;;; instead. Likewise a module should depend on a sibling, not a niece
;;; or nephew. Modules also cannot depend on cousins. Modules cannot
;;; depend on parents, since that is circular.

(defun component-depends-on-changed (component changed-siblings)
  (dolist (dependent (component-depends-on component))
    (when (member dependent changed-siblings)
      (return-from component-depends-on-changed t))))


(defmethod compute-component-policy ((c component)
				     (operation symbol)
				     policy
				     changed-siblings)
  (declare (ignore changed-siblings))
  policy)


(defmethod compute-component-policy ((c component)
				     (operation (eql :compile))
				     policy
				     changed-siblings)
  (if (and (component-depends-on-changed c changed-siblings)
	   (or (eq policy :dependencies-changed)
	      (eq policy :new-source-and-dependents)))
      (if (or (eq policy :dependencies-changed)
	      (eq policy :new-source-and-dependents))
	  :subcomponent-dependencies-also-changed
	  :always)
      policy))


;;; *action-execution-policies* -- In DS 3.2i the FORCE parameter
;;; was passed around with values
;;; :all                        = operate on all components
;;; :new-source                 = operate on all components with new source
;;; :new-source-all             = this is unclear and undocumented.
;;; :new-source-and-dependents  = operate on all components with new
;;;                               source and on the components whose
;;;                               components in the :depends-on list
;;;                               have been operated on.
;;;
;;; I believe this needs some reworking.
;;; First of all the new naming of the list is more precise, secondly
;;; I add "policies" whose names should be more meaningful.
;;; In the following I order them according to "strictness".
;;;
;;; :always                    = always operate on the component
;;; :dependencies-changed      = operate on the component only if some
;;;                              of its dependencies have changed
;;; :subcomponent-dependencies-also-changed
;;;                            = this is used to tell the processor of
;;;                              the subcomponents that the parent
;;;                              module had some element on the
;;;                              depends-on changed.
;;; :new-source                = operate on the component only if its
;;;                              source is new.
;;;
;;; For backward compatibility
;;;
;;; :new-source-and-dependents = :dependencies-changed
;;; :new-source-all            = :subcomponent-dependencies-also-changed
;;; :all                       = :always
;;;
;;; Note that :always implies :dependecies-changed, and
;;; :dependecies-changed implies :new-source
;;; In addition, for a hierarchical component, all policies may do
;;; nothing for the component itself, yet they are propagated to the
;;; sub-components.
;;;
;;; The default policy is :dependencies-changed.

;;; The EVAL-WHEN is needed because of the following DEFTYPE
;;; (at least CMUCL complains).

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter *action-execution-policies*
    (list :always
	  :dependencies-changed
	  :subcomponent-dependencies-also-changed
	  :new-source

	  :all
	  :new-source-and-dependents
	  :new-source-all)))

(deftype action-execution-policies ()
  `(member ,@*action-execution-policies*))

(declaim (type action-execution-policies *default-action-execution-policy*))

(defmethod execute-action ((m module) (operation (eql :load))
			   &key
			   (policy :always)
			   &allow-other-keys)
  ;; A module corresponds to a directory and or a grouping of
  ;; components.

  ;; No matter what the POLICY is, we still need to propagate the
  ;; operation to the sub-components.
  (execute-action-on-subcomponents m :load :policy policy))


(defmethod execute-action ((m module) (operation (eql :compile))
			   &key
			   (policy :dependencies-changed)
			   (load *compile-and-load-p*)
			   &allow-other-keys)
  ;; A module corresponds to a directory and or a grouping of
  ;; components.

  ;; No matter what the POLICY is, we still need to propagate the
  ;; operation to the sub-components.
  (execute-action-on-subcomponents m :compile :policy policy :load load))


(defmethod execute-action ((f file) (operation (eql :load))
			   &key
			   (policy :always)
			   (source-pathname (get-component-source-pathname f))
			   (binary-pathname (get-component-binary-pathname f))
			   &allow-other-keys
			   )
  (let ((source-exists-p (source-exists-p f source-pathname))
	(binary-exists-p (and (action-applicable-p f :compile)
			      binary-pathname
			      (binary-exists-p f binary-pathname)))
	)
    (unless (or binary-exists-p source-exists-p)
      (cerror "Ignore component."
	      'component-not-available-on-storage
	      :component f))

    
    (let* (    ; (deps-changed-p (dependencies-changed-p f operation))
	   (source-needs-loading-p
	    (or (member policy '(:always :all))
		(needs f :load
		       :file-content :source
		       :source-pathname source-pathname
		       :binary-pathname binary-pathname)))
	   (binary-needs-loading-p
	    (or (member policy '(:always :all))
		(needs f :load
		       :file-content :binary
		       :source-pathname source-pathname
		       :binary-pathname binary-pathname)))
	   (needs-compilation-p (and (not (component-load-only-p f))
				     (needs f :compile
					    :file-content :source
					    :source-pathname source-pathname
					    :binary-pathname binary-pathname)))
	   (compile-and-load-p
	    (and (session-compile-during-load f)
		 needs-compilation-p
		 (compile-and-load-source-if-no-binary-p f)))

	   (load-source-p
	    (or (and source-needs-loading-p
		     (session-load-source-instead-of-binary f))
		;; (and load-binary-p (component-load-only-p f))
		;; (and check-for-new-source-p needs-compilation-p)
		))

	   (load-binary-p
	    (or binary-needs-loading-p
		compile-and-load-p))
	   )

      ;; 20020722 Marco Antoniotti
      ;; Removed *minimal-load* cruft.
      
      #+mk4-breakpoints (break "EXECUTE-ACTION :LOAD checkpoint.")

      (when (or load-source-p load-binary-p compile-and-load-p)
	(cond (compile-and-load-p
	       #+mk4-breakpoints
	       (break "EXECUTE-ACTION :LOAD compile and load chekpoint.")

	       (execute-action f :compile :load nil :policy :always)
	       (load-action f binary-pathname))
	      ((and source-exists-p
		    (or (and load-source-p ; implicit needs-comp...
			     (or (component-load-only-p f)
				 (not (session-compile-during-load f))))
			(and load-binary-p
			     (not binary-exists-p)
			     (load-source-if-no-binary f))))
	       #+mk4-breakpoints
	       (break "EXECUTE-ACTION :LOAD load source chekpoint.")

	       (load-action f source-pathname))
	      ((and binary-exists-p load-binary-p)
	       #+mk4-breakpoints
	       (break "EXECUTE-ACTION :LOAD load binary chekpoint.")

	       (load-action f binary-pathname))
	      )))))


;;; Beginning original=========================================================
#+original
(defmethod execute-action ((f file) (operation (eql :load))
			   &key
			   (policy :always)
			   (source-pathname (get-component-source-pathname f))
			   (binary-pathname (get-component-binary-pathname f))
			   &allow-other-keys
			   )
  (let ((source-exists-p (source-exists-p f source-pathname))
	(binary-exists-p (and (action-applicable-p f :compile)
			      binary-pathname
			      (binary-exists-p f binary-pathname)))
	)
    (unless (or binary-exists-p source-exists-p)
      (cerror "Ignore component."
	      'component-not-available-on-storage
	      :component f))

    
    (let* ((deps-changed-p (dependencies-changed-p f operation))
	   (source-needs-loading-p
	    (needs f :load
		   :file-content :source
		   :source-pathname source-pathname
		   :binary-pathname binary-pathname))
	   (binary-needs-loading-p
	    (needs f :load
		   :file-content :binary
		   :source-pathname source-pathname
		   :binary-pathname binary-pathname))
	   (needs-compilation-p (and (not (component-load-only-p f))
				     (needs f :compile
					    :file-content :source
					    :source-pathname source-pathname
					    :binary-pathname binary-pathname)))
	   (check-for-new-source-p
	    (or deps-changed-p
		(member policy '(:new-source
				 :dependencies-changed
				 :subcomponent-dependencies-also-changed
				 ;; Backward compatibility.
				 :new-source-and-dependents
				 :new-source-all)
			:test #'eq)))
	   (load-binary-p (or deps-changed-p
			      (member policy
				      '(:always
					:subcomponent-dependencies-also-changed
					;; Backward compatibility.
					:all
					:new-source-all)
				      :test #'eq)
			      binary-needs-loading-p))
	   (compile-and-load-p
	    (and needs-compilation-p
		 (or load-binary-p check-for-new-source-p)
		 (compile-and-load-source-if-no-binary-p f)))

	   (load-source-p
	    (or *load-source-instead-of-binary*
		(and load-binary-p (component-load-only-p f))
		(and check-for-new-source-p needs-compilation-p)))
	   )

      ;; 20020722 Marco Antoniotti
      ;; Removed *minimal-load* cruft.
      
      #+mk4-breakpoints (break "EXECUTE-ACTION :LOAD checkpoint.")

      (when (or load-source-p load-binary-p compile-and-load-p)
	(cond (compile-and-load-p
	       #+mk4-breakpoints
	       (break "EXECUTE-ACTION :LOAD compile and load chekpoint.")

	       (execute-action f :compile :load nil)
	       (load-action f binary-pathname))
	      ((and source-exists-p
		    (or (and load-source-p ; implicit needs-comp...
			     (or *load-source-instead-of-binary*
				 (component-load-only-p f)
				 (not *compile-during-load*)))
			(and load-binary-p
			     (not binary-exists-p)
			     (load-source-if-no-binary f))))
	       #+mk4-breakpoints
	       (break "EXECUTE-ACTION :LOAD load source chekpoint.")

	       (load-action f source-pathname))
	      ((and binary-exists-p load-binary-p)
	       #+mk4-breakpoints
	       (break "EXECUTE-ACTION :LOAD load binary chekpoint.")

	       (load-action f binary-pathname))
	      )))))
;;; End original ===========================================================


(defmethod execute-action ((f file) (action (eql :compile))
			   &key
			   (policy :dependencies-changed)
			   (load *compile-and-load-p*)
			   (source-pathname (get-component-source-pathname f))
			   (binary-pathname (get-component-binary-pathname f))
			   &allow-other-keys)
  (declare (type pathname source-pathname)
	   (type (or null pathname) binary-pathname))

  (let ((load-p (and (not (component-compile-only-p f)) load))
	(must-compile-p
	 (and (not (component-load-only-p f)) ; not load-only.
	      ;; Policy munging will be cleared in a while.
	      (or (member policy '(:always
				   :subcomponent-dependencies-also-changed
				   ;; Backward compatibility.
				   :all
				   :new-source-all)
			  :test #'eq)
		  (and (member policy '(:dependencies-changed
					:new-source
					:new-source-and-dependents)
			       :test #'eq)
		       (needs f action)))))
	)
    (cond ((and must-compile-p (source-exists-p f source-pathname))
	   (multiple-value-bind (output-truename
				 warnings-p
				 failure-p)
	       (compile-action f source-pathname
			       :output-file binary-pathname
			       :error-log-file (get-component-error-log-pathname f)
			       :other-options (component-compiler-options f))
	     ;; Note.
	     ;; The following may have to be changed.  After all the
	     ;; overall result of the action may be to compile and
	     ;; load.  So, if the :LOAD action *is* called, then maybe
	     ;; its values should be returned.
	     (prog1 (values output-truename warnings-p failure-p)
	       (when (and (not failure-p) load-p)
		 ;; This needs reworking.
		 ;; Probably it needs to be moved out into an :around method.
		 (execute-action f :load
				 :policy :always
				 :source-pathname source-pathname
				 :binary-pathname binary-pathname))
	       )))
	  (must-compile-p
	   (warn 'no-source-file-found :component f)
	   (values nil nil t))
	  (T (values nil nil nil)))
    ))


(defvar *trace-action-execution-p* t)

(defmethod execute-action :around ((c component) (action symbol)
				   &key
				   &allow-other-keys)
 (multiple-value-bind (result warnings-p failure-p other-components)
      (call-next-method)
   (when result
     (setf (component-action-timestamp c action) (get-universal-time)))
   (when *trace-action-execution*
     (user-message *defsystem-trace-output*
		   "action ~A on ~A `~A' was executed and yielded ~S."
		   action
		   (class-name (class-of c))
		   (component-name c)
		   result))
   (values result warnings-p failure-p other-components)))


(defmethod execute-action-on-subcomponents :around ((hc hierarchical-component)
						    (action symbol)
						    &key
						    &allow-other-keys)
  (multiple-value-bind (result warnings-p failure-p other-components)
      (call-next-method)
    (when result
      (setf (component-changed-timestamp hc) (get-universal-time)))
    (values result warnings-p failure-p other-components)))



(defmethod execute-action :before ((c component) (action symbol)
				   &key
				   (trace-action-execution 
				    *trace-action-execution*)
				   &allow-other-keys)
  (when trace-action-execution
    (user-message *defsystem-trace-output*
		  "executing action ~A on ~A `~A'."
		  action
		  (class-name (class-of c))
		  (component-name c))))



(defmethod needs ((f file) (operation (eql :compile))
		  &key
		  (source-pathname
		   (get-component-source-pathname f))
		  (binary-pathname
		   (get-component-binary-pathname f)))
  (if (dependencies-changed-p f operation)
      ;; If any dependency has changed, then a file needs compilation
      ;; if it has a source file mapping.
      (probe-file source-pathname)

      ;; Otherwise, we check a number of conditions involving the
      ;; "binary" pathname and the write dates.
      (and (probe-file source-pathname)
	   (or (null (probe-file binary-pathname))
	       (< (file-write-date binary-pathname)
		  (file-write-date source-pathname))))))


(defparameter *known-file-contents* '(:source :binary))

(defmethod needs :around ((f file) (operation (eql :load))
			  &key
			  (file-content :source))
  (unless (member file-content *known-file-contents* :test #'eq)
    (error "argument must be either one of~{ ~S,~} instead of ~S."
	   *known-file-contents*
	   file-content))
  (call-next-method))


(defmethod needs ((f file) (action (eql :load))
		  &key
		  (file-content :binary)
		  (source-pathname
		   (get-component-source-pathname f))
		  (binary-pathname
		   (get-component-binary-pathname f)))
  (let ((load-timestamp (component-action-timestamp f action)))
    (or (component-load-always f)

	;; File never loaded.
	(zerop load-timestamp)

	;; Some dependencies have changed.
	(dependencies-changed-p f action)

	;; Check for newer binary.
	(and (eq file-content :binary)
	     (binary-exists-p f binary-pathname)
	     (< load-timestamp (file-write-date binary-pathname)))

	;; Check for newer source
	(and (eq file-content :source)
	     (source-exists-p f source-pathname)
	     (< load-timestamp (file-write-date source-pathname))))))


(defmethod action-applicable-p ((f file) (action (eql :load))
				&key (file-content :source)
				&allow-other-keys
				)
  (case file-content
    (:source t)
    (:binary (binary-exists-p f))))


(defmethod action-applicable-p ((f common-lisp-file) (action (eql :compile))
				&key
				&allow-other-keys
				)
  t)


;;; Timestamp and dependencies interaction.

(defmethod dependencies-changed-p ((c component) (action action))
  (dependencies-changed-p c (action-tag action)))

(defmethod dependencies-changed-p ((c component) (action symbol))
  (< (component-action-timestamp c action)
     (dependencies-latest-change-time c action)))


(defmethod dependencies-latest-change-time ((c component) (action action))
  (dependencies-latest-change-time c (action-tag action)))

(defmethod dependencies-latest-change-time ((c component) (action symbol))
  (loop for dep in (select-component-dependencies c action)
	maximize (component-action-timestamp dep action)))


;;; This is the LOAD-ACTION for generic files.

(defmethod load-action ((f file) (p pathname) &key)
  (error 'no-loader :component f))


;;; This is the COMPILE-ACTION for generic files.

(defmethod compile-action ((f file) (p pathname)
			   &key error-log-file
			   &allow-other-keys)
  (declare (ignore error-log-file))
  (error 'no-compiler :component f))


(defmethod compile-action :before ((f file) (source pathname)
				   &key
				   (output-file (compile-file-pathname source))
				   (error-log-file nil))
  (declare (ignore error-log-file))
  (ensure-directories-exist output-file))



;;; This is the LOAD-ACTION for CL files.
;;; It is a shorthand for the generic LOAD-ACTION processing.

#+make-debugging
(defmethod load-action ((f common-lisp-file) (p pathname) &key)
  (print
   `(load ,p
	  :print ,(or (component-load-print f) *load-print*)
	  :verbose ,(or (component-load-verbose f) *load-verbose*)
	  :external-format ,(component-external-format f)
	  :allow-other-keys t))
  (values (truename p) nil nil))

#-make-debugging
(defmethod load-action ((f common-lisp-file) (p pathname) &key)
  (values (load p
		:print (or (component-load-print f) *load-print*)
		:verbose (or (component-load-verbose f) *load-verbose*)
		:external-format (component-external-format f)
		:allow-other-keys t)
	  nil
	  nil))


;;; This is the COMPILE-ACTION for CL files.
;;; Note that the INVOKE-COMPILER machinery is bypassed in this case.
;;; Instead we call CL:COMPILE-file directly.

#+make-debugging
(defmethod compile-action ((f common-lisp-file) (source pathname)
			   &key
			   (output-file (compile-file-pathname source))
			   (error-log-file nil))
  (print
   `(compile-file-internal ,source
			   :output-file ,output-file
			   :error-file ,error-log-file
			   :print ,(or (component-compile-print f)
				       *compile-print*)
			   :verbose ,(or (component-compile-verbose f)
					 *compile-verbose*)
			   :external-format ,(component-external-format f)))
  (values (truename source)
	  nil
	  nil)
  )

#-make-debugging
(defmethod compile-action ((f common-lisp-file) (source pathname)
			   &key
			   (output-file (compile-file-pathname source))
			   (error-log-file nil))
  (cl:compile-file source
		   :output-file output-file
		   :error-file error-log-file
		   :print (or (component-compile-print f)
			      *compile-print*)
		   :verbose (or (component-compile-verbose f)
				*compile-verbose*)
		   :external-format (component-external-format f)
		   :allow-other-keys t
		   ))




(defmethod execute-action ((f file) (action (eql :clean))
			   &key
			   &allow-other-keys)
  (let ((binary-pathname (get-component-binary-pathname f)))
    (when (binary-exists-p f binary-pathname)
      (clean-action f binary-pathname))))

(defmethod clean-action ((f file) (p pathname))
  (print
   `(delete-file ,p))
  (values (truename p) nil nil))


;;;---------------------------------------------------------------------------
;;; System inspection and interface.

(defmethod find-component ((s standard-hierarchical-component) &rest path)
  (if (null path)
      s
      (let ((next-component (find (first path)
				  (component-components s)
				  :test #'string-equal
				  :key #'component-name)))
	(if next-component
	    (apply #'find-component next-component (rest path))
	    (progn
	      (warn "component ~S not found in ~S."
		    (first path)
		    s)
	      'NIL)			; Should change the interface.
	    ))))



(defmethod find-component ((s standard-simple-component) &rest path)
  (cond ((null path) s)
	(t (warn "component ~S does not have any sub components ~S."
		 s
		 path)
	   s)))


(defmethod find-component ((s string) &rest path)
  (apply #'find-component (find-system s) path))


(defmethod file-components-in-component ((c component))
  '())
  
(defmethod file-components-in-component ((c file))
  (list c))

(defmethod file-components-in-component ((shc standard-hierarchical-component))
  (mapcan #'file-components-in-component (component-components shc)))

(defmethod files-in-system ((s system) &key (content :source))
  (let ((file-components (file-components-in-component s)))
    (ecase content
      (:source (mapcar #'get-component-source-pathname file-components))
      (:binary (mapcar #'get-component-binary-pathname file-components)))))


(defmethod find-component-system ((c system))
  c)

(defmethod find-component-system ((c component))
  (let ((container (component-part-of c)))
    (when container
      (find-component-system container))))






;;;---------------------------------------------------------------------------
;;; User Interface.
;;; These are the functions that are intended as the "immediate" user
;;; interface.

(defmethod compile-system ((s system)
			   &key
			   (policy :new-source-and-dependents)
			   (compile-during-load *compile-during-load*)
			   (trace *trace-action-execution-p*)
			   )
  (let ((*compile-during-load* compile-during-load)
	(*trace-action-execution-p* trace)
	)
    (multiple-value-bind (result warnings-p failure-p other-components)
	(execute-action s :compile :policy policy)
      (declare (ignore result warnings-p))
      (unless failure-p other-components))))


(defmethod compile-system ((s string)
			   &key
			   (policy :new-source-and-dependents)
			   (compile-during-load *compile-during-load*)
			   (trace *trace-action-execution-p*)
			   )
  (let ((sys (find-system s)))
    (compile-system sys :policy policy
		    :compile-during-load compile-during-load
		    :trace trace)))

(defmethod compile-system ((s symbol)
			   &key
			   (policy :new-source-and-dependents)
			   (compile-during-load *compile-during-load*)
			   (trace *trace-action-execution-p*)
			   )
  (let ((sys (find-system s)))
    (compile-system sys :policy policy
		    :compile-during-load compile-during-load
		    :trace trace)))


(defmethod load-system ((s system)
			&key
			(policy :all)
			(compile-during-load *compile-during-load*)
			(trace *trace-action-execution-p*)
			)
  (let ((*compile-during-load* compile-during-load)
	(*trace-action-execution-p* trace)
	)
    (execute-action s :load :policy policy)))


(defmethod load-system ((s string)
			&key
			(policy :all)
			(compile-during-load *compile-during-load*)
			(trace *trace-action-execution-p*)
			)
  (let ((sys (find-system s)))
    (load-system sys
		 :policy policy
		 :compile-during-load compile-during-load
		 :trace trace)))


(defmethod load-system ((s symbol)
			&key
			(policy :all)
			(compile-during-load *compile-during-load*)
			(trace *trace-action-execution-p*)
			)
  (let ((sys (find-system s)))
    (load-system sys
		 :policy policy
		 :compile-during-load compile-during-load
		 :trace trace)))

(defun operate-on-system (sys action &rest keys)
  (apply #'execute-action sys action keys))

(defun oos (sys action &rest keys)
  (apply #'operate-on-system sys action keys))


;;;---------------------------------------------------------------------------
;;; OS object file support.

(defmethod execute-action ((c object-file) (action standard-compile-action)
			   &key &allow-other-keys)
  (values (truename (get-component-source-pathname c)) nil nil))

(defmethod needs ((c object-file) (action standard-compile-action) &key)
  nil)

(defmethod execute-action ((c object-file) (action standard-load-action)
			   &key &allow-other-keys)
  (if (needs c action)
      (load-action c (get-component-source-pathname c))
      (values (truename (get-component-source-pathname c)) nil nil)))


(defmethod load-action ((c object-file) (p pathname)
			&key &allow-other-keys)
  (invoke-loader (component-loader c) p
		 :print  (or (component-load-print c) *load-print*)
		 :verbose (or (component-load-verbose c) *load-verbose*)
		 :libraries ()))

(defmethod needs ((c object-file) (action standard-load-action) &key)
  (and (dependencies-changed-p c action)
       (< (component-action-timestamp c action)
	  (file-write-date (get-component-source-pathname c)))))



;;;---------------------------------------------------------------------------
;;; C support.

(defmethod compile-action ((f c-file) (p pathname)
			   &key
			   (output-file (get-component-binary-pathname f))
			   (error-log-file nil))
  (declare (type pathname output-file)
	   (type (or null pathname) error-log-file))
  (invoke-compiler (component-compiler f) p
		   :output-pathname output-file
		   :verbose *compile-verbose*
		   :options (component-compiler-options f)
		   :error-log-file error-log-file
		   :allow-other-keys t
		   ))


(defmethod load-action ((f c-file) (p pathname) &key)
  ;; Gross hack not to muck too much with the class hierarchy.
  ;; Essentially, not all the descendants of C-FILE are linkable components.
  (let ((libs (and (linkable-component-p f)
		   (linkable-component-libraries f))))
    (invoke-loader (component-loader f) p
		   :print (or (component-load-print f) *load-print*)
		   :verbose (or (component-load-verbose f) *load-verbose*)
		   :libraries libs)))


(defmethod execute-action ((f c-header-file) (operation (eql :load))
			   &key policy
			   &allow-other-keys)
  (declare (ignore policy))
  ;; A no-op.
  (values (truename (get-component-source-pathname f))
	  nil
	  nil))


(defmethod execute-action ((f c-header-file) (operation (eql :compile))
			   &key policy
			   &allow-other-keys)
  (declare (ignore policy))
  ;; A no-op.
  (values (truename (get-component-source-pathname f))
	  nil
	  nil))

;;;---------------------------------------------------------------------------
;;; Java support.

(defmethod compile-action ((f java-source-file) (p pathname)
			   &key
			   (output-file (get-component-binary-pathname f))
			   (error-log-file nil))
  (declare (type pathname output-file)
	   (type (or null pathname) error-log-file))
  (invoke-compiler (component-compiler f) p
		   :output-pathname output-file
		   :verbose *compile-verbose*
		   :options (component-compiler-options f)
		   :error-log-file error-log-file
		   :allow-other-keys t
		   )
  )


(defmethod execute-action ((f java-source-file) (action (eql :load))
			   &key &allow-other-keys)
  ;; A no-op for the time being.
  (values (truename (get-component-binary-pathname f))
	  nil
	  nil))


;;;---------------------------------------------------------------------------
;;; Library support.
;;; Most of the code applies at the library level.
;;; The distinction between "statically" and "dynamically" linked
;;; appears only at the linker/compiler level.
;;; The following code is a good start.

(defmethod execute-action ((c library)
			   (operation (eql :load))
			   &key
			   policy
			   (compile t)
			   &allow-other-keys)
  (when (and (component-load-only-p c) (needs c :compile))
    (warn "Component ~S is marked :LOAD-ONLY, ~
           but it seems it needs compilation."))
  (when (and compile (not (component-load-only-p c)) (needs c :compile))
    (execute-action c :compile :policy policy :load nil))
  (when (and (not (component-compile-only-p c)) (needs c operation))
    (load-action c (get-component-binary-pathname c))))


(defmethod load-action ((c library) (p pathname) &key)
  (invoke-loader (component-loader c) p
		 :print (or (component-load-print c) *load-print*)
		 :verbose (or (component-load-verbose c) *load-verbose*)
		 :libraries ()))


(defmethod execute-action ((c library)
			   (operation (eql :compile))
			   &key policy
			   ;; (load nil)
			   &allow-other-keys)
  (when (needs c operation)
    (if (build-externally-p c)
	(invoke-processor-external
	 (component-compiler c)
	 (get-component-source-pathname c)
	 :output-pathname (get-component-binary-pathname c)
	 :verbose *compile-verbose*
	 :options (component-compiler-options c)
	 :allow-other-keys t)
	(execute-action-on-subcomponents c :compile :policy policy :load nil))
    ))


;;; The next method is really the build method for the library.

(defmethod execute-action-on-subcomponents :after ((c library)
						   (action (eql :compile))
						   &key
						   ;; policy
						   (load nil)
						   &allow-other-keys)
  ;; If any of the subcomponents has changed, i.e it has a "compile"
  ;; timestamp later that the C one, then C is built appropriately by
  ;; invoking the appropriate linker.
  (when (and (not (build-externally-p c))
	     (some #'(lambda (subc)
		       (dependencies-changed-p subc action))
		   (component-components c)))
    (invoke-linker (component-linker c)
		   (get-component-source-pathname c)
		   :output-pathname (get-component-binary-pathname c)
		   :options (component-linker-options c)
		   :allow-other-keys t))
  (when load
    (invoke-loader (component-loader c) (get-component-binary-pathname c)))
  )
  



(defmethod needs ((c library) (action action)
		  &key &allow-other-keys)
  (needs c (action-tag action)))

(defmethod needs ((c library) (action (eql :load))
		  &key &allow-other-keys)
  (and (not (component-compile-only-p c))
       (dependencies-changed-p c action)))



(defmethod needs ((c library) (action (eql :compile))
		  &key &allow-other-keys)
  (and (not (component-load-only-p c))

       (dependencies-changed-p c action)

       ;; Somewhat inefficient.
       (some (lambda (subc)
	       (needs subc action))
	     (component-components c))))


;;;---------------------------------------------------------------------------
;;; Interim stuff.

(defun compile-and-load-source-if-no-binary-p (component)
  #||(break ">>> *load-source-instead-of-binary* ~S~@
          >>> *load-source-if-no-binary* ~S~@
          >>> (binary-exists component) ~S."
	 *load-source-instead-of-binary*
	 *load-source-if-no-binary*
	 (binary-exists-p component))||#
  (when (not (or (session-load-source-instead-of-binary component)
		 (and (session-load-source-if-no-binary component)
		      (not (binary-exists-p component)))))
    (cond ((component-load-only-p component)
	   #||
	   (let ((prompt (prompt-string component)))
	     (format t "~A- File ~A is load-only, ~
                        ~&~A  not compiling."
		     prompt
		     (component-full-pathname component :source)
		     prompt))
	   ||#
	   nil)
	  ((eq (session-compile-during-load component) :query)
	   (let* ((prompt (prompt-string component))
		  (compile-source
		   (y-or-n-p-wait
		    #\y 30
		    "~A- Binary file ~A is old or does not exist. ~
                     ~&~A  Compile (and load) source file ~A instead? "
		    prompt
		    (get-component-binary-pathname component)
		    prompt
		    (get-component-source-pathname component))))
	     (unless (y-or-n-p-wait
		      #\y 30
		      "~A- Should I bother you if this happens again? "
		      prompt)
	       (setq *compile-during-load*
		     (y-or-n-p-wait
		      #\y 30
		      "~A- Should I compile and load or not? "
		      prompt)))		; was compile-source, then t
	     compile-source))
	  ((session-compile-during-load component))
	  (t nil))))


(defun load-source-if-no-binary (component)
  (and (not (session-load-source-instead-of-binary component))
       (or (and (load-source-if-no-binary component)
		(not (binary-exists-p component)))
	   (component-load-only-p component)
	   (when (session-bother-user-if-no-binary component)
	     (let* ((prompt (prompt-string component))
		    (load-source
		     (y-or-n-p-wait #\y 30
				    "~A- Binary file ~A does not exist. ~@
                                     ~A  Load source file ~A instead? "
				    prompt
				    (get-component-binary-pathname component)
				    prompt
				    (get-component-source-pathname component)
				    )))
	       (setf (session-bother-user-if-no-binary component)
		     (y-or-n-p-wait #\n 30
				    "~A- Should I bother you if this ~
                                     happens  again? "
				    prompt))
	       (unless (session-bother-user-if-no-binary component)
		 (setf (session-load-source-if-no-binary component)
		       load-source))
	       load-source)))))


;;; end of file -- defsystem-protocol.lisp --
