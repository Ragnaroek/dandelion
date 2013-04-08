;;; -*- Mode: Lisp -*-

;;; cmucl.lisp --
;;; CMUCL implementation dependencies.

;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities

(defmethod current-directory-pathname ((cl-implementation cl.env:cmucl))
  (pathname (ext:default-directory)))


(defmethod change-current-working-directory ((cl-implementation cl.env:cmucl)
					     (new-dir string))
  (change-current-working-directory cl-implementation
				    (parse-namestring new-dir)))


(defmethod change-current-working-directory ((cl-implementation cl.env:cmucl)
					     (new-dir pathname))
  (pathname (setf (ext:default-directory) new-dir)))

;;; end of file -- cmucl.lisp --
