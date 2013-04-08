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
	  make-ellipse
	  make-filled-ellipse
	  make-filled-ellipse-edge
	  ellipse-origin-x
	  ellipse-origin-y
	  ellipse-width
	  ellipse-height
	  )
	'pictures)

;Method: rectangle-size
;Method: rectangle-width and (setf rectangle-width)
;Method: rectangle-height and (setf rectangle-height)
;Method: scale-transform

(defclass ellipse (rectangle)
  ()
  (:documentation
   "A  graphic that represents a ellipse in either object coordinates."))


;Filled-Ellipse Class Definition:

(defclass filled-ellipse (  ellipse filled-rectangle)
  ()
  (:documentation "Filled ellipse class in pictures"))


;Filled-Ellipse-Edge Class Definition:

(defclass filled-ellipse-edge (  ellipse filled-rectangle-edge)
  ()
  (:documentation "Filled ellipse edge class in pictures"))



(defun make-ellipse (x-min y-min width height  &rest options )
  "   Make a ellipse with the with the given X-MIN, Y-MIN, WIDTH and HEIGHT.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'ellipse
	 :point-seq 
	 (complete-rectangle x-min y-min (+ x-min width) (+ y-min height))
	 options))

(defun make-filled-ellipse (x-min y-min width height  &rest options )
  "   Make a filled-ellipse with the given X-MIN, Y-MIN, WIDTH and HEIGHT.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'filled-ellipse
	 :point-seq
	 (complete-rectangle x-min y-min (+ x-min width) (+ y-min height))
	 options))

(defun make-filled-ellipse-edge (x-min y-min width height  &rest options )
  "Make a filled-ellipse-edge with the given X-MIN, Y-MIN, WIDTH and HEIGHT.
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'filled-ellipse-edge
	 :point-seq
	 (complete-rectangle x-min y-min (+ x-min width) (+ y-min height))
	 options))

(DEFMETHOD ellipse-origin-x ((ellipse ellipse))
  (vertex-x ellipse 0)
  )

(DEFMETHOD (SETF ellipse-origin-x) (origin-x (ellipse ellipse) )
  (LET ((difference (- origin-x (vertex-x ellipse 0))))
    (extent-changed ellipse)
    (DOTIMES (pos 4 origin-x)
      (SETF (vertex-x ellipse pos) (+ difference (vertex-x ellipse pos)) ))))

(DEFMETHOD ellipse-origin-y ((ellipse ellipse))
  (vertex-y ellipse 0))

(DEFMETHOD (SETF ellipse-origin-y) (origin-y (ellipse ellipse) )
  (LET ((difference (- origin-y (vertex-y ellipse 0))))
    (extent-changed ellipse)
    (DOTIMES (pos 4 origin-y)
      (SETF (vertex-y ellipse pos) (+ difference (vertex-y ellipse pos)) ))))

(DEFMETHOD ellipse-width ((ellipse ellipse))
  (with-slots (vertices transform) ellipse
     (VALUES (distance
	      (vertex-x ellipse 0)
	      (vertex-y ellipse 0)
	      (vertex-x ellipse 1)
	      (vertex-y ellipse 1)))))

(DEFMETHOD (SETF ellipse-width) (width (ellipse ellipse))
  (with-slots (vertices transform) ellipse
    (extent-changed ellipse)
    (MULTIPLE-VALUE-BIND
	(x y dx dy) (compute-point
		     (VERTEX-X ELLIPSE 0)
		     (VERTEX-Y ELLIPSE 0)
		     (vertex-x ellipse 1)
		     (vertex-y ellipse 1)
		     width)
      (SETF (vertex-x ellipse 1) x)
      (SETF (vertex-y ellipse 1) y)
      (SETF (vertex-x ellipse 2) (+ (vertex-x ellipse 2) dx))
      (SETF (vertex-y ellipse 2) (+ (vertex-y ellipse 2) dy))))
  ellipse)

(DEFMETHOD ellipse-height ((ellipse ellipse))
  (with-slots (vertices transform) ellipse
    (VALUES (distance
	      (vertex-x ellipse 0)
	      (vertex-y ellipse 0)
	      (vertex-x ellipse 3)
	      (vertex-y ellipse 3)))))


(DEFMETHOD (SETF ellipse-height) (height (ellipse ellipse))
  (with-slots (vertices transform) ellipse
    (extent-changed ellipse)
    (MULTIPLE-VALUE-BIND
	(x y dx dy) (compute-point
		     (VERTEX-X ELLIPSE 0)
		     (VERTEX-Y ELLIPSE 0)
		     (vertex-x ellipse 3)(vertex-y ellipse 3) height)
      (SETF (vertex-x ellipse 3) x)
      (SETF (vertex-y ellipse 3) y)
      (SETF (vertex-x ellipse 2) (+ (vertex-x ellipse 2) dx))
      (SETF (vertex-y ellipse 2) (+ (vertex-y ellipse 2) dy)))
    ellipse))

(defmethod draw-graphic ((ellipse ellipse) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (LET ((world-transform (graphic-world-transform ellipse)))
    (with-slots (vertices extent) ellipse    
      (WHEN (visible-p ellipse)
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (if (AND world-transform 
		   (= (t12 world-transform)(t21 world-transform) 0))

	      (PROGN
		(transform-point-seq world-transform temp-vertices)
		(view-draw-arc 
		 view				; Yes, use draw-ellipse to draw it
		 (min-value-vector temp-vertices 0)
		 (min-value-vector temp-vertices 1)
		 (- (max-value-vector temp-vertices 0)
		    (min-value-vector temp-vertices 0))
		 (- (max-value-vector temp-vertices 1)
		    (min-value-vector temp-vertices 1))
		 0
		 (* 2.0 pi)
		 (graphic-gstate ellipse)))

	      (with-vector 
	       scan-vector
	       (scan-ellipse
		(ellipse-origin-x ellipse)
		(ellipse-origin-y ellipse)
		(ellipse-width ellipse)
		(ellipse-height ellipse)
		scan-vector
		(view-pixel-size view))
	       (transform-point-seq world-transform scan-vector)
	       (view-draw-polygon
		view				; No, use draw-polygon to draw it
		scan-vector
		(graphic-gstate ellipse)))
	      )
	  ))
      ))
  ellipse)

(defmethod draw-graphic ((ellipse filled-ellipse) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (LET ((world-transform (graphic-world-transform ellipse)))
    (with-slots (vertices extent) ellipse    
      (WHEN (visible-p ellipse)
	(with-vector 
	 temp-vertices
	 (copy-to-vector vertices temp-vertices)

	 (if (AND world-transform
		  (= (t12 world-transform)(t21 world-transform) 0))

	     (PROGN
	       (transform-point-seq world-transform temp-vertices)
	       (view-draw-filled-arc
		view				; Yes, use draw-ellipse to draw it
		(min-value-vector temp-vertices 0)
		(min-value-vector temp-vertices 1)
		(- (max-value-vector temp-vertices 0)
		   (min-value-vector temp-vertices 0))
		(- (max-value-vector temp-vertices 1)
		   (min-value-vector temp-vertices 1))
		0
		(* 2.0 pi)
		(graphic-gstate ellipse)))
	     
	     (with-vector 
	      scan-vector
	      (scan-ellipse 
	       (ellipse-origin-x ellipse)
	       (ellipse-origin-y ellipse)
	       (ellipse-width ellipse)
	       (ellipse-height ellipse)
	       scan-vector
	       (view-pixel-size view))
	      (transform-point-seq world-transform scan-vector)
	      (view-draw-filled-polygon
	       view	; No, use draw-polygon to draw it
	       scan-vector
	       (graphic-gstate ellipse)))
	     )
	 ))
      ))
  ellipse)

(defmethod draw-graphic ((ellipse filled-ellipse-edge) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (LET ((world-transform (graphic-world-transform ellipse)))
    (with-slots (vertices edge-gstate extent) ellipse    
      (WHEN (visible-p ellipse)
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  	  
	  (if (AND world-transform
		   (= (t12 world-transform)(t21 world-transform) 0))
	      
	      (PROGN
		(transform-point-seq world-transform temp-vertices)
		(view-draw-filled-arc
		 view				; Yes, use draw-ellipse to draw it
		 (min-value-vector temp-vertices 0)
		 (min-value-vector temp-vertices 1)
		 (- (max-value-vector temp-vertices 0)
		    (min-value-vector temp-vertices 0))
		 (- (max-value-vector temp-vertices 1)
		    (min-value-vector temp-vertices 1))
		 0
		 (* 2.0 pi)
		 (graphic-gstate ellipse))
		(view-draw-arc
		 view				; Yes, use draw-ellipse to draw it
		 (min-value-vector temp-vertices 0)
		 (min-value-vector temp-vertices 1)
		 (- (max-value-vector temp-vertices 0)
		    (min-value-vector temp-vertices 0))
		 (- (max-value-vector temp-vertices 1)
		    (min-value-vector temp-vertices 1))
		 0
		 (* 2.0 pi)
		 (edge-gstate ellipse)))
	      
	      (with-vector
	       scan-vector
	       (scan-ellipse
		(ellipse-origin-x ellipse)
		(ellipse-origin-y ellipse)
		(ellipse-width ellipse)
		(ellipse-height ellipse)
		scan-vector
		(view-pixel-size view))
	       (transform-point-seq world-transform scan-vector)
		
	       (view-draw-filled-polygon
		view				; No, use draw-polygon to draw it
		scan-vector
		(graphic-gstate ellipse))
	       (view-draw-polygon view	
				  scan-vector
				  (edge-gstate ellipse)))
	      )
	  ))
      ))
  ellipse)

(DEFMETHOD graphic-contains-p ((ellipse ellipse)  x y &optional (pixel 1))
  (DECLARE (type wcoord x y) )
  (when (point-in-extents-p ellipse x y pixel)
    (LET ((world-transform (graphic-world-transform ellipse))
	  (line-width
	   (OR (AND
		(graphic-gstate ellipse)
		(gstate-line-width ellipse)
		(+ (/ (gstate-line-width ellipse) 2) pixel))
	       pixel)))
      (with-slots (vertices) ellipse    
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (with-vector scan-vector
	    (scan-ellipse
	     (ellipse-origin-x ellipse)
	     (ellipse-origin-y ellipse)
	     (ellipse-width ellipse)
	     (ellipse-height ellipse)
	     scan-vector
	     pixel)
	    (transform-point-seq world-transform scan-vector)
	    (point-near-line scan-vector line-width x y)))))))

(DEFMETHOD graphic-contains-p ((ellipse filled-ellipse)  x y &optional (pixel 1))
  (DECLARE (type wcoord x y) )
  (when (point-in-extents-p ellipse x y pixel)
    (LET ((world-transform (graphic-world-transform ellipse)))
      (with-slots (vertices) ellipse    
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (with-vector scan-vector
	    (scan-ellipse
	     (ellipse-origin-x ellipse)
	     (ellipse-origin-y ellipse)
	     (ellipse-width ellipse)
	     (ellipse-height ellipse)
	     scan-vector
	     pixel)
	    (transform-point-seq world-transform scan-vector)
	    (WHEN 
	      (inside-p scan-vector (make-point :x x :y y)) t )
	    ))))))

(DEFMETHOD graphic-contains-p ((ellipse filled-ellipse-edge)  x y
			       &optional (pixel 1))
  (DECLARE (type wcoord x y) )
  (when (point-in-extents-p ellipse x y pixel)
    (LET ((world-transform (graphic-world-transform ellipse))
	  (line-width (OR (AND
			    (graphic-gstate ellipse)
			    (gstate-line-width ellipse)
			    (+ (/ (gstate-line-width ellipse) 2) pixel))
			  pixel)))
      (with-slots (vertices) ellipse    
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (with-vector scan-vector
	    (scan-ellipse
	     (ellipse-origin-x ellipse)
	     (ellipse-origin-y ellipse)
	     (ellipse-width ellipse)
	     (ellipse-height ellipse)
	     scan-vector
	     pixel)
	    (transform-point-seq world-transform scan-vector)
	    (WHEN (OR  
		    (inside-p scan-vector (make-point :x x :y y))
		    (point-near-line scan-vector line-width x y) ) t )
	    ))))))



(DEFMACRO ellipse-y-coordinate (x  a2 b2)
  "   Determine a given Y value in an ellispe for X and the aspects A and B.
   A2 and B2 are A and B squared"
  `(SQRT (- ,b2 (/ (* ,x ,x ,b2) (* ,a2)))))

 

(DEFUN scan-ellipse ( origin-x origin-y width height vector pixel)
  "   Contruct an ellipse from the ORIGIN-X, ORIGIN-Y, WIDTH, HEIGHT
   and store the new vertices in VECTOR"
  (SETF (FILL-POINTER vector) 0)
  (LET* ((a  (- (/  width  2) ))
	(a2 (* a a))
	(b  (/  height 2))
	(b2 (* b b))
	(smoothness (* pixel 1)))

    (VECTOR-PUSH-EXTEND a vector 100)
    (VECTOR-PUSH-EXTEND (ellipse-y-coordinate  a  a2 b2) vector 100)
    
    (DO ((x (+ a (* pixel smoothness))
	    (+ x (* pixel
		    (LET ((difference
			   (ABS (/ (- (ELT vector (- (FILL-POINTER vector) 1))
				      (ELT vector (- (FILL-POINTER vector) 3)))
				   pixel))))
		      (COND
		       ((< difference 1) 8)
		       ((< difference 2) 6)
		       ((< difference 3) 4)
		       ((< difference 4) 2)
		       (t 1))))
	       )))
	((> x 0))
      (VECTOR-PUSH-EXTEND x vector  100)
      (VECTOR-PUSH-EXTEND (ellipse-y-coordinate x  a2 b2) vector 100)
      ))

  (DO ((y (- (FILL-POINTER vector) 1) (- y 2)))
      ((< y 0))
    (VECTOR-PUSH-EXTEND (- (ELT vector (- y 1)))  vector 100)
    (VECTOR-PUSH-EXTEND  (ELT vector y) vector 100))
  
  (DO ((y (- (FILL-POINTER vector) 1) (- y 2)))
      ((< y 0))
    (VECTOR-PUSH-EXTEND (ELT vector (- y 1))  vector 100)
    (VECTOR-PUSH-EXTEND  (- (ELT vector y)) vector 100))

  (DO ((y (- (FILL-POINTER vector) 1) (- y 2)))
      ((< y 0))
    (SETF  (ELT vector (- y 1)) (+ (ELT vector (- y 1)) origin-x  (/ width 2)))
    (SETF  (ELT vector y) (+ (ELT vector y)  origin-y (/ height 2))))
  )
