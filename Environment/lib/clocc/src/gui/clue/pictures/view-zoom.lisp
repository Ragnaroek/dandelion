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
	  view-zoom-value
	  view-zoom-x
	  view-zoom-y
	  view-zoom-gravity
	  view-zoom-in
	  view-zoom-out
	  )
	'pictures)

(DEFMETHOD  view-zoom-value ((view view))
  "the amount the view by"
  (GETF (slot-value view 'plist) :zoom))


(DEFMETHOD  (SETF view-zoom-value) ( zoom (view view) )
  "set the amount to zoom the view by"
  (IF zoom
      (SETF (GETF (slot-value view 'plist) :zoom) zoom)
      (REMF (slot-value view 'plist) :zoom)))

(DEFMETHOD  view-zoom-x ((view view))
  "the fixed x value to zoom at"
  (GETF (slot-value view 'plist) :zoom-x))


(DEFMETHOD  (SETF view-zoom-x) ( zoom-x (view view) )
  "the fixed x value to zoom at"
  (IF zoom-x
      (SETF (GETF (slot-value view 'plist) :zoom-x) zoom-x)
      (REMF (slot-value view 'plist) :zoom-y)))

(DEFMETHOD  view-zoom-y ((view view))
  "the fixed x value to zoom at"
  (GETF (slot-value view 'plist) :zoom-y))


(DEFMETHOD  (SETF view-zoom-y) (zoom-y (view view) )
  "the fixed x value to zoom at"
  (IF zoom-y
      (SETF (GETF (slot-value view 'plist) :zoom-y) zoom-y)
      (REMF (slot-value view 'plist) :zoom-y)))

(DEFMETHOD  view-zoom-gravity ((view view))
  "the gravity point at which to zoom "
  (GETF (slot-value view 'plist) :zoom-gravity))


(DEFMETHOD  (SETF view-zoom-gravity) ( gravity (view view) )
  "set the gravity point at which to zoom "
  (ASSERT  (MEMBER gravity '( :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast))
	   (gravity)
	   "enter a new value of  :northwest :north :northeast :west
 :center :east :southwest :south :southeast"
	     )
  (SETF (GETF (slot-value view 'plist) :zoom-gravity) gravity))

(DEFMETHOD view-zoom-in ((view view))
  (view-zoom view
	     (OR (view-zoom-value view) 2)
	     :fixed-point
	     (OR 
	      (AND (view-zoom-x view)
		   (view-zoom-y view)
		   (LIST (view-zoom-x view)(view-zoom-y view)))
	      (view-zoom-gravity view)
	      :center))
  (refresh-view view))

(DEFMETHOD view-zoom-out ((view view))
  (view-zoom view
	     (OR (AND (view-zoom-value view)
		      (/ 1 (view-zoom-value view) ))
		 (/ 1 2))
	     :fixed-point
	     (OR (AND (view-zoom-x view)
		      (view-zoom-y view)
		      (LIST (view-zoom-x view)(view-zoom-y view)))
		 (view-zoom-gravity view)
		 :center))
  (refresh-view view))











