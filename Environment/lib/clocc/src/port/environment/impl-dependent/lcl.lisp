;;; -*- Mode: Lisp -*-

;;; lcl.lisp --
;;; Liquid (nee Lucid) implementation dependencies.

;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities
;;; Cfr. LCL 5.0 manual at
;;; http://www.lispworks.com/reference/documentation.html

(defmethod current-directory-pathname ((cl-implementation cl.env:lucid))
  (pathname (lcl:working-directory)))


(defmethod change-current-directory ((cl-implementation cl.env:lucid)
				     (new-dir string))
  (setf (lcl:working-directory) new-dir))


(defmethod change-current-directory ((cl-implementation cl.env:lucid)
				     (new-dir pathname))
  (setf (lcl:working-directory) new-dir))


;;; end of file -- lcl.lisp --
