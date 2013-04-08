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
	  make-rectangle
	  make-filled-rectangle
	  make-filled-rectangle-edge
	  rectangle-origin-x
	  rectangle-origin-y
	  rectangle-width
	  rectangle-height
	  rectangle-size
	  )
	'pictures)

;Rectangle Class Definition:

(defclass pictures::rectangle (polygon)
  ()
  (:documentation "A  graphic that represents a rectangle in object coordinates."))


;Filled-Rectangle Class Definition:

(defclass filled-rectangle (  rectangle filled-polygon)
  ()
  (:documentation "Filled rectangle class in pictures"))


;Filled-Rectangle-Edge Class Definition:

(defclass filled-rectangle-edge (  rectangle filled-polygon-edge)
  ()
  (:documentation "Filled rectangle edge class in pictures"))



(defun make-rectangle (x-min y-min width height  &rest options )
  "Make a rectangle with the with the given X-MIN, Y-MIN, WIDTH and HEIGHT.
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'rectangle
	 :point-seq (complete-rectangle
		     x-min y-min (+ x-min width) (+ y-min height))
	 options))


(defun make-filled-rectangle (x-min y-min width height  &rest options )
  "Make a filled-rectangle with with the given X-MIN, Y-MIN, WIDTH and HEIGHT.
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'filled-rectangle
	 :point-seq (complete-rectangle
		     x-min y-min (+ x-min width) (+ y-min height))
	 options))


(defun make-filled-rectangle-edge (x-min y-min width height  &rest options )
  "Make a filled-rectangle-edge with the given X-MIN, Y-MIN, WIDTH and HEIGHT. 
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST EDGE-GSTATE"

  (APPLY #'MAKE-INSTANCE 'filled-rectangle-edge
	 :point-seq (complete-rectangle
		     x-min y-min (+ x-min width) (+ y-min height))
	 options))




; Graphic methods for rectangle graphics

(DEFMETHOD rectangle-origin-x ((rectangle rectangle))
  (vertex-x rectangle 0)
  )

(DEFMETHOD (SETF rectangle-origin-x) (origin-x (rectangle rectangle) )
  (LET ((difference (- origin-x (vertex-x rectangle 0))))
    (extent-changed rectangle)
    (DOTIMES (pos 4 origin-x)
      (SETF (vertex-x rectangle pos) (+ difference (vertex-x rectangle pos)) ))))

(DEFMETHOD rectangle-origin-y ((rectangle rectangle))
  (vertex-y rectangle 0))

(DEFMETHOD (SETF rectangle-origin-y) (origin-y (rectangle rectangle) )
  (LET ((difference (- origin-y (vertex-y rectangle 0))))
    (extent-changed rectangle)
    (DOTIMES (pos 4 origin-y)
      (SETF (vertex-y rectangle pos) (+ difference (vertex-y rectangle pos)) ))))

(DEFMETHOD rectangle-width ((rectangle rectangle))
  (with-slots (vertices) rectangle
    (VALUES (distance
	      (vertex-x rectangle 0) (vertex-y rectangle 0) (vertex-x rectangle 1) (vertex-y rectangle 1)))))

(DEFMETHOD (SETF rectangle-width) (width (rectangle rectangle))
  (with-slots (vertices ) rectangle
    (extent-changed rectangle)
    (MULTIPLE-VALUE-BIND
	(x y dx dy) (compute-point (VERTEX-X RECTANGLE 0)(VERTEX-Y RECTANGLE 0)
				   (vertex-x rectangle 1)(vertex-y rectangle 1)
				   width)
      (SETF (vertex-x rectangle 1) x)
      (SETF (vertex-y rectangle 1) y)
      (SETF (vertex-x rectangle 2) (+ (vertex-x rectangle 2) dx))
      (SETF (vertex-y rectangle 2) (+ (vertex-y rectangle 2) dy))))
  rectangle)

(DEFMETHOD rectangle-height ((rectangle rectangle))
  (with-slots (vertices ) rectangle
    (VALUES (distance
	      (vertex-x rectangle 0)
	      (vertex-y rectangle 0)
	      (vertex-x rectangle 3)
	      (vertex-y rectangle 3)))))


(DEFMETHOD (SETF rectangle-height) (height (rectangle rectangle))
  (with-slots (vertices ) rectangle
    (extent-changed rectangle)
    (MULTIPLE-VALUE-BIND
	(x y dx dy) (compute-point
		     (VERTEX-X RECTANGLE 0)(VERTEX-Y RECTANGLE 0)
		     (vertex-x rectangle 3)(vertex-y rectangle 3) height)
      (SETF (vertex-x rectangle 3) x)
      (SETF (vertex-y rectangle 3) y)
      (SETF (vertex-x rectangle 2) (+ (vertex-x rectangle 2) dx))
      (SETF (vertex-y rectangle 2) (+ (vertex-y rectangle 2) dy)))
    rectangle))



(DEFMETHOD rectangle-size ((rectangle rectangle))
  
  (VALUES
    (distance
     (vertex-x rectangle 0) (vertex-y rectangle 0)
     (vertex-x rectangle 1) (vertex-y rectangle 1))
    (distance
      (vertex-x rectangle 0) (vertex-y rectangle 0)
      (vertex-x rectangle 3) (vertex-y rectangle 3))
    ))


(DEFMETHOD normalize-graphic  :around ((rectangle rectangle))
  (with-slots (vertices transform) rectangle
    (extent-changed rectangle)
    (WHEN (AND transform (= (transform-t12 transform)
			    (transform-t21 transform) 0))
      (with-slots (vertices transform) rectangle
	(transform-point-seq transform vertices)
	(SETF transform nil))
      )
    transform))



(DEFUN compute-point (x1 y1 x2 y2 distance)
  (LET* (x y)
    (IF (=  x1 x2)
	(VALUES x1 (+ y1 distance) 0 (- (+ y1 distance) y2 ))
	(progn
	  (SETF y (- y1 (* distance (SIN (ATAN (/ (- y1 y2)(- x1 x2)))))))
	  (SETF x (- x1 (* distance (COS  (ATAN (/ (- y1 y2)(- x1 x2)))))))
	  (VALUES x y (- x x2) (- y y2))))))

(DEFUN distance (x1 y1 x2 y2)
  "the distance between two points"
      (SQRT (+ (* (- x1 x2)(- x1 x2))(* (- y1 y2)(- y1 y2)))))

;Method: draw-graphic



;  Draw the RECTANGLE object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and
;  HEIGHT are given, then only parts of the object that lie within the
;  given rectangle need to be drawn.

(defmethod draw-graphic ((rectangle rectangle) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices extent transform) rectangle    
    (WHEN (visible-p rectangle)
      (LET ((world-transform (graphic-world-transform rectangle)))
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (transform-point-seq world-transform temp-vertices)
	  (if (AND world-transform 
		   (= (t12 world-transform)(t21 world-transform) 0))
	      (view-draw-rectangle
	       view ; Yes, use draw-rectangle to draw it
	       (min-value-vector temp-vertices 0)
	       (min-value-vector temp-vertices 1)
	       (- (max-value-vector temp-vertices 0)
		  (min-value-vector temp-vertices 0))
	       (- (max-value-vector temp-vertices 1)
		  (min-value-vector temp-vertices 1))
	       (graphic-gstate rectangle))
	      (view-draw-polygon view		; No, use draw-polygon to draw it
				 temp-vertices
				 (graphic-gstate rectangle))
	      )
	  ))
      rectangle)))

;Method: draw-graphic
;  Draw the FILLED-RECTANGLE object in the given VIEW. If MIN-X, MIN-Y,
;  WIDTH, and HEIGHT are given, then only parts of the object that lie
;  within the given rectangle need to be drawn.

(defmethod draw-graphic ((rectangle filled-rectangle) (view view)
			 &optional min-x min-y width height)
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices transform extent) rectangle    
    (WHEN   (visible-p rectangle)
      (LET ((world-transform (graphic-world-transform rectangle)))
	(with-vector temp-vertices 
	  (copy-to-vector vertices temp-vertices)
	  (transform-point-seq (graphic-world-transform rectangle) temp-vertices)
	  (if (AND world-transform
		   (= (t12 world-transform)(t21 world-transform) 0))
	      (view-draw-filled-rectangle
	       view	; Yes, use draw-rectangle to draw it
	       (min-value-vector temp-vertices 0)
	       (min-value-vector temp-vertices 1)
	       (- (max-value-vector temp-vertices 0)
		  (min-value-vector temp-vertices 0))
	       (- (max-value-vector temp-vertices 1)
		  (min-value-vector temp-vertices 1))
	       (graphic-gstate rectangle))
	      
	      (view-draw-filled-polygon 
	       view 	; No, use draw-polygon to draw it
	       temp-vertices
	       (graphic-gstate rectangle)))
	  )))
    ))
 

;Method: draw-graphic
;  Draw the FILLED-RECTANGLE-EDGE object by first drawing the interior and
;  then boundary.  If MIN-X, MIN-Y, WIDTH, and HEIGHT are given, then only
;  parts of the object that lie within the given rectangle need to be
;  drawn.

(defmethod draw-graphic ((rectangle filled-rectangle-edge) (view view)
			 &optional min-x min-y width height)
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (edge-gstate extent) rectangle
    (WHEN (visible-p rectangle) 
      (LET ((world-transform (graphic-world-transform rectangle)))
	(with-slots (vertices transform) rectangle    
	  (with-vector temp-vertices 
	    (copy-to-vector vertices temp-vertices)
	    (transform-point-seq
	     (graphic-world-transform rectangle) temp-vertices)
	    (if (AND world-transform 
		     (= (t12 world-transform)(t21 world-transform) 0))
		(progn	    ; Yes, use draw-fillrectangle to  Draw the interior.
		  (view-draw-filled-rectangle
		    view	
		    (min-value-vector temp-vertices 0)
		    (min-value-vector temp-vertices 1)
		    (- (max-value-vector temp-vertices 0)
		       (min-value-vector temp-vertices 0))
		    (- (max-value-vector temp-vertices 1)
		       (min-value-vector temp-vertices 1))
		    (graphic-gstate rectangle))
						; Draw the boundary
		  (view-draw-rectangle
		    view
		    (min-value-vector temp-vertices 0)
		    (min-value-vector temp-vertices 1)
		    (- (max-value-vector temp-vertices 0)
		       (min-value-vector temp-vertices 0))
		    (- (max-value-vector temp-vertices 1)
		       (min-value-vector temp-vertices 1))
		    (edge-gstate rectangle)))			       
		
		(PROGN 
		  (view-draw-filled-polygon
		   view ; No, use draw-fillpolygon to draw the interior
		   temp-vertices
		   (graphic-gstate rectangle))
		  (view-draw-polygon 
		   view	; Draw the boundary
		   temp-vertices
		   (edge-gstate rectangle))))))))))
 
(defmethod scale-transform ((graphic rectangle) scale-x scale-y
                            &optional (fixed-x 0) (fixed-y 0))
  (declare (type (or (satisfies plusp) (satisfies zerop)) scale-x scale-y))
  (declare (type ocoord fixed-x fixed-y))
  (graphic-damage graphic)				; Damage from old graphic
  
	    
  (with-slots (transform) graphic
    (UNLESS (AND transform (= (t12 transform)(t21 transform) 0))
      (COND
	((= scale-x 1) (SETF scale-x scale-y))
	((= scale-y 1) (SETF scale-y scale-x))
	((< scale-x scale-y)(SETF scale-x scale-y))
	(t (SETF  scale-y scale-x))))
    (when (null transform)			; If no transform
      (setf transform (make-transform)))	; Create one
    (graphic-stack-purge
     *transform-stack* graphic) 		; Notify the transform stack
    (PROG1
	(scale-transform transform 		; Scale it
			 scale-x scale-y fixed-x fixed-y)
      (extent-changed graphic))			; Notify graphic his extent may have changed
    (graphic-damage graphic)			; Damage from new graphic
    transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Private Method: complete-rectangle
;  Compute and return the world coordinates of all four vertices of the
;  given RECTANGLE.  Also returns the X and Y lengths of the diagonal.

(defun complete-rectangle (x1 y1 x3 y3 &aux x2 y2 x4 y4)
  (PSETF  x2 x3
	  y2 y1
	  y4 y3
	  x4 x1)
  (VALUES (vector x1 y1 x2 y2 x3 y3 x4 y4))
  )			; X and Y lengths of the diagonal


;; private method to determine if a rectangle is ortagonal

(DEFMETHOD orthogonal ((rectangle rectangle) (view view))
  (with-slots (vertices) rectangle
    (LET  ((x1 (VERTEX-X RECTANGLE 0) )
	   (y1 (VERTEX-Y RECTANGLE 0))
	   (x3 (vertex-x rectangle 2))
	   (y3 (vertex-y rectangle 2))
	   (epsilon (view-pixel-size view))) ; World size of one pixel in this view
 
      (if (or (<= (abs (- x1 x3)) epsilon)	; Is rectangle orthogonal?
	      (<= (abs (- y1 y3)) epsilon))
	  t
	  nil))))





       
(DEFUN point-in-rectangle  (x y xmin ymin height width )
  "this function determines if a given point is within the
 extent bound of a graphic"
  
    (And (>= x xmin )
	       (<= x (+ xmin height))
	       (>= y ymin)
	       (<= y (+ ymin width))))

