;;; -*- Mode: Lisp -*-

;;; machine.lisp --
;;; This is provided mostly for symmetry.
;;;
;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defclass machine (feature-tagged-type-class)
  ((type :initarg :type :reader machine-type)
   (version :initarg :version :reader machine-version)
   (instance :initarg :instance :reader machine-instance)
   )
  (:documentation "The CL.ENVIRONMENT Machine Class.")
  (:default-initargs :type (common-lisp:machine-type)
                     :version (common-lisp:machine-version)
		     :instance (common-lisp:machine-instance)))


;;; Different machine types are provided here.
;;; The following (eventually singleton) classes mostly reflect the
;;; CPU type.

(defclass intel-x86-machine (machine)
  ()
  (:default-initargs :feature-tag :x86))
	  

(defclass ppc-machine (machine)
  ()
  (:default-initargs :feature-tag :ppc))

(defclass sparc-machine (machine)
  ()
  (:default-initargs :feature-tag :sparc))

(defclass sparc-v9-machine (sparc-machine)
  ()
  (:default-initargs :feature-tag :sparc-v9))

(defclass alpha-machine (machine)
  ()
  (:default-initargs :feature-tag :alpha))

(defclass mips-machine (machine)
  ()
  (:default-initargs :feature-tag :mips))
	  

;;; end of file -- machine.lisp
