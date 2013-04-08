;;; -*- Mode: CLtL -*-

;;; configuration-template.lisp --

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

(defclass configuration-template ()
  ((name :initarg :name
	 :reader name)
   (os :initarg :operating-system
       :reader operating-system)
   (cl :initarg :common-lisp-implementation
       :reader common-lisp-implementation)
   (machine :initarg :machine
	    :reader machine)

   (logical-host :initarg :logical-host :reader logical-host)
   (library-location-forms :initarg :library-location-forms
			   :reader library-location-forms)
   (source-location-forms :initarg :source-location-forms
			  :reader source-location-forms)
   (special-translations-forms :initform ()
			       :accessor special-translations)
   )
  (:default-initargs
    :name 'generic-configuration
    :operating-system cl.env:*operating-system*
    :machine cl.env:*machine*
    :logical-host "CL-EXT-CONFIGURATIONS"
    :library-location-forms ()
    :source-location-forms ()
    )
  (:documentation "The Common Lisp Extensions CONFIGURATION Class."))


(defun configuration-template-p (x)
  (typep x 'configuration-template))

;;; end of file -- configuration-template.lisp --
