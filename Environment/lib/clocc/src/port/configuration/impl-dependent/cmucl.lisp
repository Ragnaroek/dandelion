;;; -*- Mode: CLtL -*-

;;; cmucl.lisp --
;;; CMUCL implementation dependencies.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

;;; Directory utilities

(defmethod current-directory-namestring ((cl-implementation cl.env:cmucl))
  (namestring (ext:default-directory)))


;;; DEFSYSTEM utilities

;;; Nothing define for CMUCL, which relies on third parties DEFSYSTEMs.

;;; end of file -- cmucl.lisp --
