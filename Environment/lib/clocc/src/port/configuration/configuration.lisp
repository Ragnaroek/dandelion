;;; -*- Mode: CLtL -*-

;;; configuration.lisp --

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

(defclass configuration ()
  ((name :initarg :name
	 :reader conf-name)
   (os :initarg :operating-system
       :reader conf-operating-system)
   (cl :initarg :common-lisp-implementation
       :reader conf-common-lisp-implementation)
   (machine :initarg :machine
	    :reader conf-machine)

   (logical-host :initarg :logical-host :reader conf-logical-host)
   (library-location :initarg :library-location :reader conf-library-location)
   (source-location :initarg :source-location :reader conf-source-location)
   (special-translations :initform () :accessor conf-special-translations)
   )
  (:default-initargs
    :name 'generic-configuration
    :operating-system cl.env:*operating-system*
    :machine cl.env:*machine*
    :logical-host "CL-EXT-CONFIGURATIONS"
    :library-location ""
    :source-location ""
    )
  (:documentation "The Common Lisp Extensions CONFIGURATION Class."))


(defun configurationp (x)
  (typep x 'configuration))

;;; end of file -- configuration.lisp --
