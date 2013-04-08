;;; -*- Mode: CLtL -*-

;;; unix.lisp --
;;; Un*x dependencies.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(in-package "CL.EXT.CONFIGURATION")

(defmethod wild-inferior-directories ((os cl.env:unix)) "**/")

;;; end of file -- unix.lisp --
