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


(DEFMETHOD  view-x-pan ((view view))
  "set or get the amount to pan in along the x axis"
  (GETF (slot-value view 'plist) :x-pan))


(DEFMETHOD  (SETF view-x-pan) ( x-world-pan (view view) )
  "set the relative pan motion  along the x axis"
  (SETF (GETF (slot-value view 'plist) :x-pan) x-world-pan))

(DEFMETHOD  view-y-pan ((view view))
  "the amount to pan in along the y axis"
  (GETF (slot-value view 'plist) :y-pan))


(DEFMETHOD  (SETF view-y-pan) ( y-world-pan (view view) )
  "set the relative to pan motion  along the y axis"
  (SETF (GETF (slot-value view 'plist) :y-pan) y-world-pan))

(DEFMETHOD view-pan-right ((view view))
  (with-slots (origin-x default-gcontext height width ) view
    (LET* ((scale-x (view-scale-x view))
	   (scale-y (view-scale-y view))
	   (scale (view-scale view))
	  (pan-value (OR (view-x-pan view ) (/ width scale 2)))
	  )
      (SETF origin-x (+ origin-x  pan-value ))
      (COPY-area 
       view default-gcontext
       (FLOOR (* pan-value scale-x)) 0
       (- width (FLOOR (* pan-value scale-x))) height view  0 0)
      (MULTIPLE-VALUE-BIND
	  (x y)(view-untransform-point
		view  (- width (FLOOR (* pan-value scale-x))) height)
	(view-damage view   x  y  pan-value (/ height scale-y)))
      ))
  (repair-view view))

(DEFMETHOD view-pan-left ((view view))
  (with-slots (origin-x default-gcontext height width ) view
    (LET* ((scale-x (view-scale-x view))
	   (scale-y (view-scale-y view))
	   (scale (view-scale view))
	  (pan-value (OR (view-x-pan view ) (/ width scale 2))))
      (SETF origin-x (- origin-x  pan-value ))
      (COPY-area
       view default-gcontext
       0 0 (- width (FLOOR (* pan-value scale-x))) height view
		 (FLOOR (* pan-value scale-x)) 0)
      (MULTIPLE-VALUE-BIND (x y)(view-untransform-point view  0 height)
	(view-damage view   x  y   pan-value   (/ height scale-y)))
      ))
  (repair-view view)
  )

(DEFMETHOD view-pan-up ((view view))
  (with-slots (origin-y default-gcontext height width ) view
    (LET* ((scale (view-scale view))
	   (scale-x (view-scale-x view))
	   (scale-y (view-scale-y view))
	   (pixel (view-pixel-size view))
	  (pan-value (OR (view-y-pan view )(/ height scale 2) )))
      (SETF origin-y (+ origin-y  pan-value ))
      (COPY-area
       view default-gcontext 0 0 
       width (- height (FLOOR (* pan-value scale-y))) view
       0 (FLOOR (* pan-value scale)))
      (MULTIPLE-VALUE-BIND
	  (x y)(view-untransform-point view   0  (* pan-value scale-y))
	(view-damage view   (- x pixel) (- y pixel) (+ pixel (/ width scale-x)) (+ pixel pan-value )))
      ))
  (repair-view view ))

(DEFMETHOD view-pan-down ((view view))
  (with-slots (origin-y default-gcontext height width ) view
    (LET* ((scale (view-scale view))
	   (scale-x (view-scale-x view))
	   (scale-y (view-scale-y view))
	   (pixel (view-pixel-size view))
	  (pan-value (OR (view-y-pan view )(/ height scale 2) )))
      (SETF origin-y  (- origin-y  pan-value ))
      (COPY-area
       view default-gcontext
       0 (FLOOR (* pan-value scale-y))  width
       (- height (FLOOR (* pan-value scale-y))) view
       0 0)
      (MULTIPLE-VALUE-BIND
	  (x y)(view-untransform-point view   0   height )
	(view-damage view (- x pixel)(- y pixel)
		     (+ pixel (/ width scale-x))
		     (+ pixel pan-value )))
      ))
  (repair-view view)
  )
