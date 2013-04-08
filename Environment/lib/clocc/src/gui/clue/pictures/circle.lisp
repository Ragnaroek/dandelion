;;;-*- Mode:Lisp; Package:PICTURES; Base:10 -*-
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
	  circle-center-x
	  circle-center-y
	  circle-radius
	  circle-center
	  make-circle
	  make-filled-circle
	  make-filled-circle-edge
	  circle
	  filled-circle-edge
	  filled-circle
	  )
	'pictures)

;Circle Class Definition:

(defclass circle (extent-cache graphic)
  (
   (center-x	:type		wcoord
                :initarg	:center-x
		:accessor       circle-center-x 
		:documentation "x-coordinate of the center")
   
   (center-y	:type		wcoord
                :initarg	:center-y
		:accessor       circle-center-y 
		:documentation "y-coordinate of the center")

   (radius	:type		wcoord
                :initarg	:radius
		:accessor       circle-radius 
		:documentation	"Radius of the circle")
   
   )
  (:documentation "A graphic that represents a circle in object coordinates"))


;Filled-Circle Class Definition:

(defclass filled-circle ( circle)
  ()
  (:documentation "Filled circle class in pictures"))


;Filled-Circle-Edge Class Definition:

(defclass filled-circle-edge ( circle edge)
  ()
  (:documentation "Filled circle edge class in pictures"))


;Function: make-circle
;  Return a new circle object with the given CENTER and RADIUS.

(defun make-circle (center-x center-y radius
                    &rest options
                    )
  "Make a circle with the center coordinate of (CENTER-X CENTER-Y) and RADIUS.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"
  
  (declare (type wcoord center-x center-y radius))

  (apply #'make-instance 'circle
         :center-x center-x
         :center-y center-y
         :radius   radius
         options)
  )



;Function: make-filled-circle
;  Return a new filled-circle object with the given CENTER and RADIUS.

(defun make-filled-circle (center-x center-y radius
                            &rest options
                            )
  "Make a filled-circle with the center coordinate of
   (CENTER-X CENTER-Y) and RADIUS.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"
  
  (declare (type wcoord center-x center-y radius))

  (apply #'make-instance 'filled-circle
         :center-x center-x
         :center-y center-y
         :radius   radius
         options))


;Function: make-filled-circle-edge
;  Return a new filled-circle-edge object with the given CENTER and RADIUS.

(defun make-filled-circle-edge (center-x center-y radius &rest options)
  "Make a filled-circle with the center coordinate of
   (CENTER-X CENTER-Y) and RADIUS.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (declare (type wcoord center-x center-y radius))

  (apply #'make-instance 'filled-circle-edge
         :center-x center-x
         :center-y center-y
         :radius   radius
         options))

;Method: circle-radius
;  Returns or changes the object coordinates for the radius of the CIRCLE.

(defmethod (setf circle-radius)  :after (new-radius (circle circle))
  (declare (ignore new-radius))
  (extent-changed circle))


;Method: circle-center
;  Returns or changes the object coordinates of the center of the CIRCLE.

(defmethod circle-center ((circle circle))

  (with-slots (center-x center-y) circle
    (values center-x center-y)))

(defmethod (SETF circle-center-x) :after (new-center-x (circle circle))
  (declare (ignore new-center-x))
  (extent-changed circle))

(defmethod (setf circle-center-y) :after (new-center-y (circle circle))
  (declare (ignore new-center-y))
  (extent-changed circle))


; Graphic methods for circle graphics

;Method: extent-compute
;  Compute the extent rectangle for the CIRCLE.

;  Note: A graphic's extent rectangle is defined in the object coordinate
;  system.  This means that each graphic should apply its own transform to
;  its computed extent before returning it.

(defmethod extent-compute ((circle circle))

  (with-slots (center-x center-y radius transform) circle
    (let (new-center-x new-center-y new-radius-x new-radius-y new-radius)
      (multiple-value-setq
	  (new-center-x new-center-y)		; Transform circle center
        (transform-point transform center-x center-y))
      (multiple-value-setq
	  (new-radius-x new-radius-y)		; Transform circle radius
        (scale-point transform radius radius))
      (setf new-radius
	    (min new-radius-x new-radius-y))	; Use the smaller scale factor
      (let ((xmin (- new-center-x new-radius))
	    (ymin (- new-center-y new-radius))
	    (xmax (+ new-center-x new-radius))
	    (ymax (+ new-center-y new-radius)))
	(with-coercion ((xmin ymin xmax ymax) extent-element)
          (make-extent-rect ; Build the extent square
	   :xmin xmin
	   :ymin ymin
	   :xmax xmax
	   :ymax ymax
	   :valid t))))))

;Method: draw-graphic
;  Draw the CIRCLE object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and
;  HEIGHT are given, then only parts of the object that lie within the
;  given rectangle need to be drawn.

(defmethod draw-graphic ((circle circle) (view view)
                           &optional min-x min-y width height)
  (declare (type (or null wcoord) min-x min-y width height))
    (with-slots (extent) circle
      (WHEN (visible-p circle) 
	(multiple-value-bind (xmin ymin diameter)
	    (square-bounding-circle circle)
	  (view-draw-arc
	   view	; Draw the circle
	   xmin
	   ymin
	   diameter
	   diameter
	   0
	   (* 2 pi)
	   (graphic-gstate circle))	; Pass the combined gstate
  ))))

;Method: draw-graphic Draw the FILLED-CIRCLE object in the given VIEW. If
;MIN-X, MIN-Y, WIDTH, and HEIGHT are given, then only parts of the object
;that lie within the given rectangle need to be drawn.

(defmethod draw-graphic ((circle filled-circle) (view view)
			   &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (extent) circle

    (WHEN (visible-p circle) 
      
      (multiple-value-bind (xmin ymin diameter)
	  (square-bounding-circle circle)
	(view-draw-filled-arc
	 view	; Draw the circle
	 xmin
	 ymin
	 diameter
	 diameter
	 0
	 (* 2 pi)
	 (graphic-gstate circle))	; Pass the combined gstate
	))))

;Method: draw-graphic
;  Draw the FILLED-CIRCLE-EDGE object by first drawing the interior and
;  then boundary.  If MIN-X, MIN-Y, WIDTH, and HEIGHT are given, then only
;  parts of the object that lie within the given rectangle need to be
;  drawn.

(defmethod draw-graphic ((circle filled-circle-edge) (view view)
			   &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (extent) circle 
  
    (WHEN (visible-p circle)
      (with-slots (edge-gstate) circle
						; Use global temp buffer
	(multiple-value-bind (xmin ymin diameter)
	    (square-bounding-circle circle)
	  (view-draw-filled-arc
	   view					; Draw the circle's interior
	   xmin
	   ymin
	   diameter
	   diameter
	   0
	   (* 2 pi)
	   (graphic-gstate circle))		; Pass the combined gstate
	  (view-draw-arc
	   view					; Draw the circle's boundary
	   xmin
	   ymin
	   diameter
	   diameter
	   0
	   (* 2 pi)
	   (edge-gstate circle))	; Pass the combined edge gstate
	  )))))

;Method: normalize-graphic
; Normalize the CIRCLE by applying its transform
; to its geometry, changing it accordingly, and then setting its transform
; to nil (the identity transform).  Nothing of value is returned.

(defmethod normalize-graphic :before ((circle circle))

  (with-slots (center-x center-y radius transform) circle
    (multiple-value-setq (center-x center-y)	; Transform the center
      (transform-point transform center-x center-y))
    (let (radius-x radius-y)
      (multiple-value-setq (radius-x radius-y)	; Transform the radius
        (scale-point transform radius radius))
      (setf radius (min radius-x radius-y)))))	; Use the smaller scale factor




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Private Function: square-bounding-circle
;  Return the southwest corner and the width of the square in which the
;  given CIRCLE is inscribed.

(defun square-bounding-circle (circle)

  (with-slots (center-x center-y radius) circle
    (let (new-center-x new-center-y new-radius-x new-radius-y new-radius)
      (multiple-value-setq (new-center-x new-center-y) ; Transform circle center
        (transform-point (graphic-world-transform circle) center-x center-y))
      (multiple-value-setq (new-radius-x new-radius-y) ; Transform circle radius
        (scale-point (graphic-world-transform circle) radius radius))
      (setf new-radius (min new-radius-x new-radius-y)) ; Use the smaller scale factor

      (values (- new-center-x new-radius)
              (- new-center-y new-radius)
              (* 2 new-radius)))))
