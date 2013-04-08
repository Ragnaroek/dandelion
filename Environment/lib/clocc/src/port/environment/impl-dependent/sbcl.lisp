;;; -*- Mode: Lisp -*-

;;; sbcl.lisp --
;;; SBCL implementation dependencies.

;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities

(defmethod current-directory-pathname ((cl-implementation cl.env:sbcl))
  (pathname (ext:default-directory)))


(defmethod change-current-directory ((cl-implementation cl.env:sbcl)
				     (new-dir string))
  (change-current-directory cl-implementation
			    (parse-namestring new-dir)))


(defmethod change-current-directory ((cl-implementation cl.env:sbcl)
				     (new-dir pathname))
  (pathname (setf (ext:default-directory) new-dir)))


;;; end of file -- sbcl.lisp --
