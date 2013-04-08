;;; -*- Mode: CLtL -*-

;;; lispworks.lisp --
;;; Xanalisys/Harlequin Ltd.Lispworks implementation dependencies.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

;;; Directory utilities

(defmethod current-directory-namestring ((cl-implementation cl.env:lispworks))
  (namestring (pwd)))

;;; DEFSYSTEM utilities

;;; find-system
;;; It looks like Lispworks does not have a FIND-SYSTEM.  Therefore,
;;; we simply and consistently return nil here, which means that the
;;; system will always result 'not-found'.

(defmethod find-system ((sys symbol)
			(cl cl.env:lispworks)
			(defsys-tag (eql :lispworks)))
  nil)

(defmethod load-system ((sys symbol)
			(cl cl.env:allegro)
			(defsys-tag (eql :lispworks))
			&rest keys)
  (apply #'lispworks:load-system sys keys))

;;; end of file -- unix.lisp --
