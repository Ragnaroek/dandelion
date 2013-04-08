;;; -*- Mode: CLtL -*-

;;; corman.lisp --
;;; Corman Lisp implementation dependencies.

;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.ENV")

;;; Directory utilities

(defmethod current-directory-pathname ((cl-implementation cl.env:corman))
  (pathname (cormanlisp:get-current-directory)))

;;; end of file -- corman.lisp --
