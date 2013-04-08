;;; -*- Mode: CLtL -*-

;;; clisp.lisp --
;;; Haible's CLisp implementation dependencies.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

(defmethod current-directory-namestring ((cl-implementation cl.env:clisp))
  (namestring (cwd)))

;;; DEFSYSTEM utilities

;;; Nothing define for CLisp, which relies on third parties DEFSYSTEMs.

;;; end of file -- unix.lisp --
