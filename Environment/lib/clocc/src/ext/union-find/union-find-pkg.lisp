;;; -*- Mode: CLtL -*-

;;; union-find-pkg.lisp --
;;; Simple implementation of the well known UNION-FIND data structure
;;; (with weighted path compression).
;;; See file README for more info.
;;;
;;; Author: Marco Antoniotti
;;;
;;; Copyright (c) 2000 Marco Antoniotti. All rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).


(defpackage "CL.UTIL.UNION-FIND" (:use "COMMON-LISP")
  (:nicknames "UNION-FIND" "CL-UF" "union-find" "cl-uf")
  (:documentation
   "This package contains an implementation of the well known
UNION-FIND data structure (with weighted path compression).
The data structure is very useful as a building block of many complex
algorithms.")
  (:shadow common-lisp:union)
  (:export "PARTITION"
	   "MAKE-PARTITION"
	   "PARTITION-P")
  
  (:export "SET-REPRESENTATIVE"
	   "SET-REPRESENTATIVE-P"
	   "MAKE-SET"
	   "FIND-SET-REP"
	   "FIND-SET"
	   "UNION"

	   "COLLECT-SET"
	   "PRINT-SET"))

;;; end of file -- union-find-pkg.lisp --
