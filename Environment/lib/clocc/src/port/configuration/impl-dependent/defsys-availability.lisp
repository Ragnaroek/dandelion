;;; -*- Mode: CLtL -*-

;;; defsys-availability.lisp --

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")


;;; MK-DEFSYSTEM.

(defmethod defsys-type-available-p
    ((cl cl.env:generic-common-lisp-implementation)
     (defsys-type (eql :make)))
  (not (null (find :MK-DEFSYSTEM *features*))))

(defmethod defsys-type-available-p
    ((cl cl.env:generic-common-lisp-implementation)
     (defsys-type (eql :mk-defsystem)))
  (not (null (find :MK-DEFSYSTEM *features*))))

(defmethod defsys-type-available-p
    ((cl cl.env:generic-common-lisp-implementation)
     (defsys-type (eql :mk)))
  (not (null (find :MK-DEFSYSTEM *features*))))


;;; PCL-DEFSYS.

(defmethod defsys-type-available-p
    ((cl cl.env:generic-common-lisp-implementation)
     (defsys-type (eql :pcl)))
  (let ((pcl-package (find-package "PCL")))
    (and pcl-package
	 (not (null (find-symbol "GET-SYSTEM" pcl-package))))))


;;; Allegro DEFSYSTEM.

(defmethod defsys-type-available-p
    ((cl cl.env:allegro)
     (defsys-type (eql :allegro)))
  t)


;;; LispWorks DEFSYSTEM.

(defmethod defsys-type-available-p
    ((cl cl.env:lispworks)
     (defsys-type (eql :lispworks)))
  t)


;;; end of file -- defsys-availability.lisp --
