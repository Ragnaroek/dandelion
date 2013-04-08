;;; -*- Mode: Lisp -*-

;;; lispworks.lisp --
;;; Lispworks implementation dependencies.

;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities

(defmethod current-directory-pathname ((cl-implementation cl.env:lispworks))
  (pathname (hcl:get-working-directory))) ; This is correct for
					  ; Lispworks 4.2 and 4.3.x.


(defmethod change-current-working-directory ((cl-implementation cl.env:lispworks)
					     (new-directory string))
  (hcl:change-directory new-directory))


(defmethod change-current-working-directory ((cl-implementation cl.env:lispworks)
					     (new-directory pathname))
  (hcl:change-directory (namestring new-directory)))

 

;;; DEFSYSTEM utilities

;;; find-system
;;; It looks like Lispworks does not have a FIND-SYSTEM.  Therefore,
;;; we simply and consistently return nil here, which means that the
;;; system will always result 'not-found'.

#|
(defmethod find-system ((sys symbol)
			(cl cl.env:lispworks)
			(defsys-tag (eql :lispworks)))
  nil)

(defmethod load-system ((sys symbol)
			(cl cl.env:allegro)
			(defsys-tag (eql :lispworks))
			&rest keys)
  (apply #'lispworks:load-system sys keys))
|#

;;; end of file -- unix.lisp --
