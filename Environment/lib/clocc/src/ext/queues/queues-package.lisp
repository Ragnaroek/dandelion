;;; -*- Mode: Lisp -*-

;;; queues-package.lisp --
;;; Simple queues in the Abelson and Sussman SICP style.
;;; Package definition file (in CLtL2 style).
;;;
;;; Author: Marco Antoniotti
;;;
;;; Copyright (c) 1992 - 2000 Marco Antoniotti. All rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).


;;;============================================================================
;;; QUEUES Package

(defpackage "CL.UTIL.QUEUES" (:use "COMMON-LISP")
  (:nicknames "QUEUES")
  (:shadow common-lisp:first
	   common-lisp:last)
  (:export "QUEUE"
	   "EMPTY-QUEUE"
	   "MAKE-QUEUE"
	   "SIZE"
	   "EMPTY-P"
	   "ENQUEUE"
	   "DEQUEUE"
	   "FIRST"
	   "LAST"
	   "ENQUEUED-P"
	   "DEQUEUE-ITEMS-IF"
	   "CLEAR-QUEUE"
	   "DO-QUEUE"))

;;; end of file -- queues-package.lisp --
