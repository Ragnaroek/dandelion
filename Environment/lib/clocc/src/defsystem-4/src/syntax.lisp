;;; -*- Mode: Lisp -*-

;;; syntax.lisp --
;;; Code to manipulate the DEFSYSTEM syntax.

(in-package "MK4")


;;; defsystem-ambler-context --
;;; The specialized AMBLER context for MK:DEFSYSTEM.
;;; For the time being is used symply as a discrimination tag.

(defclass defsystem-ambler-context (ambler:copying-ambling-context)
  ()
  (:documentation "The specialized AMBLER context for MK:DEFSYSTEM."))


(defun make-defsystem-ambler-context ()
  (make-instance 'defsystem-ambler-context))


(defparameter *defsystem-ambler-context* (make-defsystem-ambler-context))
  

(ambler:def-form-ambler :defsystem (defsystem-form defsystem-ambler-context)
  (destructuring-bind (kwd-defsys name &rest keys)
      defsystem-form
    (declare (ignore kwd-defsys))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in DEFSYSTEM ~S ~
                              specification."
	     :format-control (list name)))
    (unless (find :source-pathname keys)
      (setf keys
	    (list* :source-pathname
		   '(when *load-pathname*
		      (make-pathname :name nil
				     :type nil
				     :defaults *load-pathname*))
		   keys)))
    `(construct-component :defsystem
			   ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :system (system-form defsystem-ambler-context)
  (destructuring-bind (kwd-system name &rest keys)
      system-form
    (declare (ignore kwd-system))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in SYSTEM ~S ~
                              specification."
	     :format-control (list name)))
    `(construct-component :system
			   ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :subsystem (subsystem-form defsystem-ambler-context)
  (destructuring-bind (kwd-subsystem name &rest keys)
      subsystem-form
    (declare (ignore kwd-subsystem))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in SUBSYSTEM ~S ~
                              specification."
	     :format-control (list name)))
    `(construct-component :subsystem
			   ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :module (module-form defsystem-ambler-context)
  (destructuring-bind (kwd-module name &rest keys)
      module-form
    (declare (ignore kwd-module))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in MODULE ~S ~
                              specification."
	     :format-control (list name)))
    `(construct-component :module
			   ',name
			  ,@(ambler:amble-expression keys context))))


;;; :FILE clause.
;;; The NIL default for the LANGUAGE keyword is needed not to force
;;; prematurely the type of the component to a specific language.

(ambler:def-form-ambler :file (file-form defsystem-ambler-context)
  (destructuring-bind (kwd-file name
				&rest keys
				&key
				(language nil)
				(header nil)
				&allow-other-keys)
      file-form
    (declare (ignore kwd-file))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in FILE ~S ~
                              specification."
	     :format-control (list name)))
    (remf keys :header)
    `(construct-component (determine-component-class :file ',language
						     :header ',header)
			   ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :private-file (private-file-form
				       defsystem-ambler-context)
  (destructuring-bind (kwd-private-file name &rest keys)
      private-file-form
    (declare (ignore kwd-private-file))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in PRIVATE-FILE ~S ~
                              specification."
	     :format-control (list name)))
    `(construct-component :private-file
			   ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :depends-on (depends-on-form defsystem-ambler-context)
  (destructuring-bind (kwd-depends-on dependencies &rest other-forms)
      depends-on-form
    (declare (ignore kwd-depends-on))
    ;; The dependency list is not evaluated.
    `(:depends-on ',dependencies
		  ,@(ambler:amble-expression other-forms context))))


(ambler:def-form-ambler :components (components-form defsystem-ambler-context)
  (destructuring-bind (kwd-components components &rest other-forms)
      components-form
    (declare (ignore kwd-components))
    ;; First normalize the components list.
    (let ((components (mapcar #'(lambda (comp-form)
				  (if (listp comp-form)
				      comp-form
				      `(:file ,comp-form)))
			      components))
	  )

      ;; The DELETE operation here is crucial, since a component
      ;; construction form may evaulate to NIL for a number of reasons.
      `(:components (delete nil
			    (list ,@(ambler:amble-expression
				     components
				     context)))
		    ,@(ambler:amble-expression other-forms context)))))


(ambler:def-form-ambler :initially (initially-form defsystem-ambler-context)
  (destructuring-bind (kwd-initially initially-expr &rest other-forms)
      initially-form
    (list kwd-initially `(lambda (,(intern (symbol-name '#:this-component)
					   *package*))
			   (declare (ignorable
				     ,(intern (symbol-name '#:this-component)
					      *package*)))
			   ,initially-expr)
	  (ambler:amble-expression other-forms context))))


(ambler:def-form-ambler :finally (initially-form defsystem-ambler-context)
  (destructuring-bind (kwd-finally initially-expr &rest other-forms)
      initially-form
    (list kwd-finally `(lambda (,(intern (symbol-name '#:this-component)
					 *package*))
			 (declare (ignorable
				   ,(intern (symbol-name '#:this-component)
					    *package*)))
			 ,initially-expr)
	  (ambler:amble-expression other-forms context))))


;;; Special and Predefined Components.

(ambler:def-form-ambler :cl-file (file-form defsystem-ambler-context)
  (destructuring-bind (kwd-file name &rest keys)
      file-form
    (declare (ignore kwd-file))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control
	     "Odd number of keyword slots in COMMON-LISP-FILE ~S ~
              specification."
	     :format-control (list name)))
    `(construct-component :common-lisp-file
			   ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :common-lisp-file (file-form defsystem-ambler-context)
  (destructuring-bind (kwd-file name &rest keys)
      file-form
    (declare (ignore kwd-file))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control
	     "Odd number of keyword slots in COMMON-LISP-FILE ~S ~
              specification."
	     :format-control (list name)))
    `(construct-component :common-lisp-file
			   ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :c-file (file-form defsystem-ambler-context)
  (destructuring-bind (kwd-file name
				&rest keys
				&key
				(language :c)
				(header nil)
				&allow-other-keys)
      file-form
    (declare (ignore kwd-file))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in C-FILE ~S ~
                              specification."
	     :format-control (list name)))
    (unless (eq language :c)
      (warn "MK4: Language for :C-FILE component ~S is ~S (instead of :C)."
	    name
	    language))
    (remf keys :header)
    `(construct-component (determine-component-class :file :c
						     :header ',header)
			  ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :c-header-file (file-form defsystem-ambler-context)
  (destructuring-bind (kwd-file name
				&rest keys
				&key
				(language :c)
				(header t)
				&allow-other-keys)
      file-form
    (declare (ignore kwd-file))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in FILE ~S ~
                              specification."
	     :format-control (list name)))
    (unless header
      (warn "Header keyword ignored in ~S specification." name))
    (unless (eq language :c)
      (warn "MK4: Language for :C-HEADER-FILE component ~S ~@
             is ~S (instead of :C)."
	    name
	    language))
    (remf keys :header)
    `(construct-component (determine-component-class :c-header-file :c
						     :header t)
			  ',name
			  ,@(ambler:amble-expression keys context))))


(ambler:def-form-ambler :statically-linked-library
    (file-form defsystem-ambler-context)
  (destructuring-bind (kwd-sll name
			       &rest keys
			       &key
			       (language :c)
			       &allow-other-keys)
      file-form
    (declare (ignore kwd-sll))
    (when (oddp (length keys))
      (error 'defsystem-syntax-error
	     :format-control "Odd number of keyword slots in ~
                              STATIC LIBRARY ~S ~
                              specification."
	     :format-control (list name)))

    `(construct-component (determine-component-class :statically-linked-library
						     ',language)
			  ',name
			  ,@(ambler:amble-expression keys context))))


;;; Extending the MK:DEFSYSTEM syntax.

(defmacro def-syntax-for-named-tag (tag)
  `(ambler:def-form-ambler ,tag (tag-form defsystem-ambler-context)
     (destructuring-bind (tag-kwd tag-expr &rest keys)
	 tag-form
       (when (oddp (length keys))
	 (error 'defsystem-syntax-error
		:format-control
		(format nil "Odd number of keyword slots in ~A ~
                              specification."
			tag-kwd)))
       `(construct-component ',tag
			      ',tag-expr
			     ,@(ambler:amble-expression keys
							context)))))

(defmacro def-syntax-for-component-keyword (kwd (var) &body forms)
  `(ambler:def-form-ambler ,kwd (kwd-form defsystem-ambler-context)
     (destructuring-bind (kwd kwd-expr &rest keys)
	 kwd-form
       (let ((processor (lambda (,var) ,forms)))
       `(,kwd ,(funcall processor kwd-expr)
	      ,@(ambler:amble-expression keys context))))))


;;; DEFSYSTEM Macro.

(defmacro defsystem (&rest defsystem-form)
  (ambler:amble-form :defsystem
		     (list* :defsystem defsystem-form)
		     *defsystem-ambler-context*))

       
;;; end of file -- syntax.lisp --
