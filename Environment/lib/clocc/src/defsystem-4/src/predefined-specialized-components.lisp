;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; predefined-specialized-components.lisp --

(in-package "MK4")


;;; Derived and specialized components.

;;;---------------------------------------------------------------------------
;;; C.

(defclass c-file (file)
  ()
  (:default-initargs :language :c)
  )

(defclass c-source-file (c-file
			 compilable-component-mixin
			 loadable-component-mixin
			 linkable-component-mixin
			 #|c-language-mixin|#)
  ()
  (:default-initargs
    ;; :compile-only t
    :source-extension "c"
    :binary-extension "o")
  )

(defclass c-header-file (c-file)
  ()
  (:default-initargs :source-extension "h"))

(defclass c-executable (c-file executable-file)
  ()
  (:default-initargs :link t :compile-only t :source-extension "exe"))

(defclass c-object (c-file object-file)
  ()
  (:default-initargs :link t :source-extension "o"))


;;; Notes:
;;;
;;; 2002-05-22 Marco Antoniotti
;;; Originally these components were defined to have :language :c.
;;; This is not necessarily the case.
;;;
;;; This realization is a good thing, because it settles the dilemma of
;;; the inherited language mixin vs. the language slot.  The language
;;; slot is what I want.

(defclass statically-linked-library (library)
  ()
  (:default-initargs :binary-extension (static-library-extension))
  )

(defclass dynamically-linked-library (library)
  ()
  (:default-initargs :binary-extension (shared-library-extension))
  )


;;;---------------------------------------------------------------------------
;;; Java.

(defclass java-file (file)
  ()
  (:default-initargs :language :java)
  )

(defclass java-source-file (java-file
			    compilable-component-mixin
			    loadable-component-mixin
			    linkable-component-mixin
			    )
  ()
  (:default-initargs :source-extension "java" :binary-extension "class")
  )


(defclass java-class-file (java-file
			   loadable-component-mixin
			   linkable-component-mixin
			   )
  ()
  (:default-initargs :source-extension "class" :binary-extension "class")
  )


(defclass java-jar-file (java-file
			 loadable-component-mixin
			 linkable-component-mixin
			 )
  ()
  (:default-initargs :source-extension "jar" :binary-extension "jar")
  )

;;; end of file -- predefined-specialized-components.lisp --
