;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; predefined-components.lisp --
;;;
;;; Notes:
;;;
;;; 2001-10-05 Marco Antoniotti
;;; Major design decision.
;;; Added the SYSTEM-REFERENCE class to be used as a place-holder for
;;; system specifications within dependency lists or component lists.
;;; Also changed the class hierarchy by adding STANDARD-SYSTEM.

(in-package "MK4")

(defclass abstract-system (stored-component)
  ()
  (:default-initargs :type :abstract-system))

(defgeneric abstract-system-p (x)
  (:method ((x t)) nil)
  (:method ((x abstract-system)) t))


(defclass system (abstract-system)
  ()
  (:default-initargs :type :system))

(defgeneric system-p (x)
  (:method ((x t)) nil)
  (:method ((x system)) t))

(declaim (inline systemp))
(defun systemp (x) (system-p x))


(defclass standard-system (standard-hierarchical-component system)
  ((type :accessor component-type
	 :type (member :system))
   (sysdef-source-pathname :accessor sysdef-source-location
			   :initform nil)
   (sysdef-compiled-pathname :accessor sysdef-compiled-location
			     :initform nil)
   )
  (:default-initargs
    :type :standard-system
    :language :common-lisp) ; For the time being.
  )

(defgeneric standard-system-p (x)
  (:method ((x t)) nil)
  (:method ((x standard-system)) t))


(defclass system-reference (standard-simple-component abstract-system)
  ((sys :accessor system-instance
	:reader referenced-system
	:type (or null standard-system)
	:initform nil))
  (:documentation "The System Reference class.
This class is to be used as a place-holder for system specifications
within dependency lists or component lists.")
  (:default-initargs :type :system-reference))

(defgeneric system-reference-p (x)
  (:method ((x t)) nil)
  (:method ((x system-reference)) t))


(defclass simple-system (standard-hierarchical-component system)
  ((type :accessor component-type
	 :type (member :simple-system)))
  (:default-initargs :type :simple-system)
  (:documentation "The Simple System Class.
A hierarchical system where all the components are files which are
operated on in a `serial' way."))

(defgeneric simple-system-p (x)
  (:method ((x t)) nil)
  (:method ((x simple-system)) t))

  
;;; This class (DEFSYSTEM) is not really necessary.
    
(defclass defsystem (standard-system)
  ((type :accessor component-type
	 :type (member :defsystem)))
  (:default-initargs :type :defsystem))

(defgeneric defsystem-p (x)
  (:method ((x t)) nil)
  (:method ((x defsystem)) t))

(declaim (inline defsystemp))
(defun defsystemp (x) (defsystem-p x))



(defclass subsystem (standard-system)
  ((type :accessor component-type
	 :type (member :subsystem)))
  (:default-initargs :type :subsystem))

(defgeneric subsystem-p (x)
  (:method ((x t)) nil)
  (:method ((x subsystem)) t))

(declaim (inline subsystemp))
(defun subsystemp (x) (subsystem-p x))


(defclass module (standard-hierarchical-component)
  ((type :accessor component-type
	 :type (member :module)))
  (:default-initargs :type :module))

(defgeneric module-p (x)
  (:method ((x t)) nil)
  (:method ((x module)) t))

(declaim (inline modulep))
(defun modulep (x) (module-p x))


(defclass file (standard-simple-component)
  ((type :accessor component-type
	 :type (member :file))
   (private :accessor private-file-p
	    :initarg :private
	    :type (member nil t))
   )
  (:default-initargs :type :file :private nil))

(defgeneric file-p (x)
  (:method ((x t)) nil)
  (:method ((x file)) t))

(declaim (inline filep))
(defun filep (x) (file-p x))


(defclass object-file (file loadable-component-mixin linkable-component-mixin)
  ()
  )

(defclass executable-file (file)
  ()
  )


(defclass common-lisp-file (file
			    loadable-component-mixin
			    compilable-component-mixin
			    interpretable-component-mixin)
  ()
  (:default-initargs
    :language :common-lisp
    :source-extension (cl.env:source-file-extension)
    :binary-extension (cl.env:compiled-file-extension)))

(defgeneric common-lisp-file-p (x)
  (:method ((x common-lisp-file)) t)
  (:method ((x t)) nil))

#|| It makes more sense to add the `private' slot to `file'.
Moreover, the :private-file has been used only to supersede the
pathname inheritance machinery.

(defclass private-file (file)
  ((type :accessor component-type
	 :initform :private-file))
  (:default-initargs :type :private-file))

(defgeneric private-file-p (x)
  (:method ((x t)) nil)
  (:method ((x private-file)) t))
||#


(defclass library (standard-hierarchical-component
		   loadable-component-mixin
		   compilable-component-mixin
		   linkable-component-mixin
		   )
  ((build-externally-p :accessor build-externally-p
		       :initargs :build-externally-p)
   )
  (:default-initargs
     :language :common-lisp
     :type :library
     :build-externally-p nil))

(defgeneric library-p (x)
  (:method ((x library)) t)
  (:method ((x t)) nil))

(declaim (inline libraryp))
(defun libraryp (x)
  (library-p x))

;;;---------------------------------------------------------------------------
;;; Protocol

(defgeneric binary-exists-p (file &optional binary-pathname))

(defgeneric source-exists-p (file &optional source-pathname))


;;; end of file -- predefined-components.lisp --
