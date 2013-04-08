;;; -*- Mode: Lisp -*-

;;; software.lisp --
;;;
;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defclass software ()
  ((type :initarg :type :reader software-type)
   (version :initarg :version :reader software-version)
   )
  (:documentation "The CL.ENVIRONMENT Software Class."))


;;; Known CL implementations.
;;; The hierarchy is rather arbitrary, representing "derivation".

(defclass generic-common-lisp-implementation (software
					      feature-tagged-type-class)
  ((type :reader common-lisp-implementation-type)
   (version :reader common-lisp-implementation-version)
   (tag :reader common-lisp-implementation-feature-tag
	:reader cl-feature-tag
	:type symbol)
   )
  (:documentation "The CL.ENVIRONMENT Common Lisp Implementation Class.")
  (:default-initargs :type (lisp-implementation-type)
                     :version (lisp-implementation-version)))


;;; Tags for CL implementations
;;; CMUCL	:cmucl
;;; SBCL	:sbcl
;;; ACL		:allegro
;;; CLISP       :clisp
;;; LW		:lispworks
;;; Corman Lisp	:corman-lisp
;;; ECLS	:ecl
;;; etc etc
;;; See below!!!

(defclass cmucl (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :cmu))

(defclass sbcl (cmucl)
  ()
  (:default-initargs :feature-tag :sbcl))

(defclass allegro (generic-common-lisp-implementation)
  ((case-sensitive :type boolean :initarg :case-sensitive
		   :reader software-case-sensitive)
   (characters-16bits :type boolean :initarg :characters-16bits
		      :reader software-characters-16bits))
  (:default-initargs :feature-tag :allegro))

(defclass lispworks (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :lispworks))

(defclass clisp (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :clisp))

(defclass kcl (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :kcl))

(defclass ibcl (kcl)
  ()
  (:default-initargs :feature-tag :ibcl))

(defclass akcl (kcl)
  ()
  (:default-initargs :feature-tag :akcl))

(defclass ecolisp (kcl)
  ()
  (:default-initargs :feature-tag :ecolisp))

(defclass ecl (ecolisp)
  ()
  (:default-initargs :feature-tag :ecl))

(defclass gcl (kcl)
  ()
  (:default-initargs :feature-tag :gcl))

(defclass mcl (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :mcl))

(defclass lucid (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :lucid))

(defclass lcl (lucid)
  ()
  (:default-initargs :feature-tag :lcl))

(defclass scl (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :genera)) ; Symbolics Genera.

(defclass corman (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :corman-lisp))

(defclass eclipse (generic-common-lisp-implementation)
  ()
  (:default-initargs :feature-tag :eclipse))


;;; The default binary directory name is the lowercase of the feature tag

(defgeneric software-binary-directory-name (software))

(defmethod software-binary-directory-name ((software software))
  (string-downcase (symbol-name (cl-feature-tag software))))


;;; A generic function that allows for the specification of a "default
;;; source extension".

(defgeneric software-source-file-extension (sw)
  )

(defmethod software-source-file-extension ((sw generic-common-lisp-implementation))
  "lisp")

;;; end of file -- software.lisp --
