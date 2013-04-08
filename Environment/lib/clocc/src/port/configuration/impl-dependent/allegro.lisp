;;; -*- Mode: CLtL -*-

;;; allegro.lisp --
;;; Franz Inc. implementation dependencies.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

;;; Directory utilities

(defmethod current-directory-namestring ((cl-implementation cl.env:allegro))
  (namestring (excl:current-directory)))


;;; DEFSYSTEM utilities

(defmethod find-system ((sys symbol)
			(cl cl.env:allegro)
			(defsys-tag (eql :allegro)))
  (apply #'excl:find-system sys nil))

(defmethod load-system ((sys symbol)
			(cl cl.env:allegro)
			(defsys-tag (eql :allegro))
			&rest keys)
  (apply #'excl:load-system sys keys))


;;; end of file -- allegro.lisp --
