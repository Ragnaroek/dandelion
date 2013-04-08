;;; -*- Mode: CLtL -*-

;;; mk-defsystem.lisp --
;;; MK:DEFSYSTEM dependencies.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package "MAKE")
    (error "CL.EXT.CONFIGURATION: MK:DEFSYSTEM is required.")))

;;; DEFSYSTEM utilities

(defmethod find-system ((sys symbol)
			(cl cl.env:generic-common-lisp-implementation)
			(defsys-tag (eql :mk)))
  (apply #'mk:find-system sys nil))	; Finally it has been made external.

(defmethod load-system ((sys symbol)
			(cl cl.env:generic-common-lisp-implementation)
			(defsys-tag (eql :mk))
			&rest keys)
  (apply #'mk:load-system sys keys))


;;; end of file -- mk-defsystem.lisp --
