;;; -*- Mode: Lisp -*-

;;; clisp.lisp --
;;; Haible's CLisp implementation dependencies.

;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

(defmethod current-directory-pathname ((cl-implementation cl.env:clisp))
  (pathname (ext:default-directory)))


(defmethod change-current-working-directory ((cl-implementation cl.env:clisp)
					     (new-dir string))
  (change-current-working-directory cl-implementation
				    (parse-namestring new-dir)))


(defmethod change-current-working-directory ((cl-implementation cl.env:clisp)
					     (new-dir pathname))
  (pathname (setf (ext:default-directory) new-dir)))


;;; DEFSYSTEM utilities

;;; Nothing define for CLisp, which relies on third parties DEFSYSTEMs.

;;; end of file -- clisp.lisp --
