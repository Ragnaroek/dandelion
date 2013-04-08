;;; -*- Mode: Lisp -*-

;;; allegro.lisp --
;;; Franz Inc. implementation dependencies.

;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities

(defmethod current-directory-pathname ((cl-implementation cl.env:allegro))
  (pathname (excl:current-directory)))

;;; Override generic software-binary-directory-name method

(defmethod software-binary-directory-name ((software allegro))
  (concatenate 'string
    (string-downcase (symbol-name (cl-feature-tag software)))
    "-"
    (if (software-case-sensitive software) "m" "a")
    (if (software-characters-16bits software) "16" "8")))


;;; DEFSYSTEM utilities
#|
(defmethod find-system ((sys symbol)
			(cl cl.env:allegro)
			(defsys-tag (eql :allegro)))
  (apply #'excl:find-system sys nil))

(defmethod load-system ((sys symbol)
			(cl cl.env:allegro)
			(defsys-tag (eql :allegro))
			&rest keys)
  (apply #'excl:load-system sys keys))
|#


;;; end of file -- allegro.lisp --
