;;; -*- Mode: CLtL -*-

;;; defconf-package.lisp --
;;; A 'configure' for Common Lisp.

;;; Copyright (c) 2000-2002 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).
;;; The Package definition.
;;;
;;; Notes:
;;; 20000203 Marco Antoniotti
;;; I decided to make this package dependent on the CL.ENVIRONMENT
;;; package.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package "CL.ENVIRONMENT")
    (error "CL.EXT.CONFIGURATION requires the CL.ENVIRONMENT package.")))

(defpackage "CL.EXT.CONFIGURATION" (:use "COMMON-LISP")
  (:nicknames "CONF" "conf")
  (:shadow #:configure #:setup)		; This 'shadow' clause is a
					; little paranoid. Better safe
					; than sorry.
  (:shadow #:find-system #:load-system)	; Again, a safety measure.

  (:export #:defconfiguration
	   #:configuration
	   #:setup
	   #:configure-format
	   )

  (:export #:parse-conf-clause
	   #:build-conf-clause
	   )
  )

;;; end of file -- defconf-package.lisp --
