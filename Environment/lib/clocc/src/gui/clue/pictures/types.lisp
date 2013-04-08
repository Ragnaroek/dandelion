;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "PICTURES")


(export '(
	  wcoord
	  ocoord
	  extent-element ; pw
	  )
	'pictures)

(DEFTYPE wcoord  () #+nil 'float  'real)

(DEFTYPE ocoord () #+nil 'FLOAT 'real)

(DEFTYPE extent-element () 'single-float)

(IMPORT '(int16 stringable card8 boolean) 'xlib)

(DEFCONSTANT *large-number* 100000)

(defparameter max-damage 3
  "Number of available damage rectangles")
