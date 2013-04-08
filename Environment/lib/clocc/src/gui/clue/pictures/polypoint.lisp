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
	  vertices
	  polypoint
	  polyline
	  make-polypoint
	  min-x-vertex
	  min-y-vertex
	  max-x-vertex
	  max-y-vertex
	  make-polyline
	  )
	'pictures)


(defclass polypoint (extent-cache graphic)
  (
   (vertices     :type    array
		 :initform (make-array '(8) :adjustable t :fill-pointer 0)
		 :accessor vertices 
		   :documentation  "a vector of x,y points ")
   
   )
  (:documentation "The graphic class for drawing points in object coordinates"))
  
;Function: make-polypoint
;  Return a new polypoint object.
(defun make-polypoint (point-seq  &rest options) 
  "Make a polypoint with the coordinates contained in POINT-SEQ.
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"
  (APPLY #'MAKE-INSTANCE 'polypoint
	 :point-seq point-seq
	 options))

(DEFMETHOD initialize-instance :after ((polypoint polypoint) &key point-seq)
  (SETF (vertices polypoint)  point-seq))



(DEFMETHOD (SETF vertices) (point-seq (polypoint polypoint))
  (with-slots (vertices) polypoint
    (SETF (FILL-POINTER vertices) 0)
    (DO* ((index 0 (1+ index)) )
	 ((>= index (* 2 (TRUNCATE (LENGTH point-seq) 2)))
	  (slot-value  polypoint 'vertices))
      (VECTOR-PUSH-EXTEND (ELT point-seq index) vertices 5 ))
    (extent-changed polypoint) ; pw should this be in an :after method?
    point-seq))


;Method: vertex-i
;  Return the I'th vertex of the given POLYPOINT.  If there are fewer than I
;  vertices in the POLYPOINT, then nil is returned.

(defmethod vertex-i ((polypoint polypoint) i)
  (declare (type (integer 0 *) i))
  (WHEN  (< (* i 2) (LENGTH (vertices polypoint)))
    (with-slots (vertices) polypoint
      (VALUES (ELT  vertices (* i 2))  (ELT vertices (1+ (* i 2))))))
  )


(DEFMETHOD vertex-x ((polypoint polypoint) position)
  (with-slots (vertices) polypoint
    (WHEN (< position  (length-vertices polypoint))
	(ELT vertices (* position 2)))))

(DEFMETHOD (SETF vertex-x) (x (polypoint polypoint) position)
  (with-slots (vertices) polypoint
    (SETF (vertex-x vertices position) x)
    (extent-changed polypoint)
    (vertex-x vertices position)))

(DEFMETHOD vertex-y ((polypoint polypoint) position)
    (IF (< position  (length-vertices polypoint))
	(with-slots (vertices) polypoint
	  (ELT vertices (+ (* position 2) 1)))))

(DEFMETHOD (SETF vertex-y) (y (polypoint polypoint) position)
  (with-slots (vertices) polypoint
    (SETF (vertex-y vertices position) y)
    (extent-changed polypoint)
    (vertex-y vertices position)))

;Method: extent-compute
;  Compute the extent rectangle for the polypoint.

;  Note: A graphic's extent rectangle is defined in the object coordinate
;  system.  This means that each graphic should apply its own transform to
;  its computed extent before returning it.

(defmethod extent-compute ((polypoint polypoint))

  (LET (extent-rectangle)
    (with-slots (vertices transform) polypoint
      (with-vector temp-vector
	(copy-to-vector vertices temp-vector)
	(transform-point-seq transform temp-vector)
	(let ((xmin (min-value-vector temp-vector 0))
	      (ymin (min-value-vector temp-vector 1))
	      (xmax (max-value-vector temp-vector 0))
	      (ymax (max-value-vector temp-vector 1)))
	  (with-coercion ((xmin ymin xmax ymax) extent-element)
	    (SETF extent-rectangle
		  (make-extent-rect
		   :xmin xmin
		   :ymin ymin
		   :xmax xmax
		   :ymax ymax
		   :valid t))))))
  extent-rectangle))

;Method insert-vertex
; Given a x and y value an vertex can be inserted in to the x and y vectors
; of the object If i is greater than the length of the vector, the vertex
; points are added to the end of the x-vectors

(DEFMETHOD insert-vertex ((polypoint polypoint) new-x new-y  i)
  (DECLARE (type wcoord new-x))
  (DECLARE (type integer i))
  (extent-changed polypoint)
  (with-slots (vertices  extent) polypoint
    (insert-vertex vertices new-x new-y i)
    )
  (values new-x new-y)
  )


; Delete a vertex from the point list  
(DEFMETHOD delete-vertex ( (polypoint polypoint) i)
  (DECLARE (type integer i))
  (extent-changed polypoint)
  (MULTIPLE-VALUE-BIND (x y)  (vertex-i polypoint i)
    (with-slots (vertices extent) polypoint
      (delete-vertex vertices i))
    (VALUES x y)))



(DEFMETHOD length-vertices ((polypoint polypoint))
  (with-slots (vertices) polypoint
    (VALUES (FLOOR (FILL-POINTER vertices) 2))))

(DEFMETHOD min-x-vertex ((polypoint polypoint))
  (with-slots (vertices) polypoint
    (point-seq-x-min vertices)))

(DEFMETHOD min-y-vertex ((polypoint polypoint))
  (with-slots (vertices) polypoint
    (point-seq-y-min vertices)))

(DEFMETHOD max-x-vertex ((polypoint polypoint))
  (with-slots (vertices) polypoint
    (point-seq-x-max vertices)))

(DEFMETHOD max-y-vertex ((polypoint polypoint))
  (with-slots (vertices) polypoint
    (point-seq-y-max vertices)))




;Method: normalize-graphic
;  Normalize the POLYPOINT by applying its transform to its geometry,
;  changing it accordingly, and then setting its transform to nil (the
;  identity transform).  Nothing of value is returned.

(defmethod normalize-graphic :before ((polypoint polypoint))
  (with-slots (vertices transform) polypoint
    (transform-point-seq transform vertices)
    (SETF transform nil))
  )

;Method: draw-graphic
;  Draw the POLYPOINT object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and
;  HEIGHT are given, then only parts of the object that lie within the
;  given rectangle need to be drawn.

(defmethod draw-graphic ((polypoint polypoint) (view view) 
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate extent) polypoint
    (WHEN   (visible-p polypoint)
      (WHEN (AND ( > (LENGTH vertices) 0) (viewable-p polypoint))
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (transform-point-seq (graphic-world-transform polypoint) temp-vertices)
	  (view-draw-polypoint view temp-vertices 
			       (graphic-gstate polypoint)))))))

;Polyline Elements

;A polyline is an ordered list of points connected by straight line
;segments.  The first and last points in the list are not connected by a
;line.  The polyline class inherits from the polypoint class so that the
;following methods described for polypoints are also available for
;polylines.

;Polyline  Definition:
(defclass polyline (polypoint)
  ()
  (:documentation "the graphic class for drawing a polyline on the screen"))

;Function: make-polyline
;  Return a new polyline object.

(defun make-polyline (point-seq  &rest options )
  "Make a polygon with the coordinates contained in POINT-SEQ.
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"
  
  (APPLY #'MAKE-INSTANCE 'polyline
	 :point-seq point-seq
	 options))




;draw a polyline

(defmethod draw-graphic ((polyline polyline) (view view)
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate extent) polyline
    (WHEN (visible-p polyline)
	(WHEN (AND (viewable-p polyline)( > (LENGTH vertices) 0))
	  (with-vector temp-vertices
	    (copy-to-vector vertices temp-vertices)
	    (transform-point-seq (graphic-world-transform polyline) temp-vertices)
	    (view-draw-polyline view temp-vertices 
				(graphic-gstate polyline)))))))


(DEFMETHOD graphic-contains-p ((polyline polyline) x y &optional (pixel 1))
  (DECLARE (type wcoord x y) )
  (when (point-in-extents-p polyline x y pixel)
      (LET ((line-width
	     (OR (AND
		  (graphic-gstate polyline)
		  (gstate-line-width polyline)
		  (+ (/ (* (gstate-line-width polyline) pixel)  2 ) pixel))
		 pixel)))
	(with-slots (vertices) polyline
	  (with-vector temp-vertices 
	    (copy-to-vector vertices temp-vertices)
	    (transform-point-seq (graphic-world-transform polyline) temp-vertices)
	    (point-near-line temp-vertices line-width x y))))))


(defstruct point x y)


(defun cross-product (p1 p2 q1 q2)
  (- (* (- (point-x p2) (point-x p1))
	(- (point-y q2) (point-y q1)))
     
     (* (- (point-x q2) (point-x q1))
	(- (point-y p2) (point-y p1)))))
  
(defun inside-p (polygon p)
  (declare (type sequence polygon)
	   (type point p))

  (LET ((poly-seq (get-global-vector)))
    (DO*
      ( 
       (last   (- (length polygon) 2))
       (i           0 (+ i 2))
       )
      ((> i last) (VALUES poly-seq))
      (VECTOR-PUSH-EXTEND
       (make-point :x (ELT polygon i) :y (ELT  polygon (+ i 1))) poly-seq)
		 
      )
  ;; True if p is on a polygon edge (edge cross-product is 0) or if p is on the
  ;; same side of every polygon edge (edge cross-products have the same sign.
  
    (do*
      (current-vertex
       current-sign
       (last        (1- (LENGTH poly-seq)))
       (prev-vertex (elt poly-seq 0) current-vertex)
       (prev-sign   (SIGNUM (cross-product p (elt poly-seq last) p prev-vertex))
		    current-sign)
       (i           1 (1+ i)))
      
      ((or
	 ;; Point on previous edge?
	 (zerop prev-sign)
	 
	 ;; All edge cross-products the same sign?
	 (> i last))
       (PROGN
	 (return-global-vector poly-seq);release the global vector for reuse
       t))
      (setf current-sign
	    (signum (cross-product
		     p prev-vertex p (setf current-vertex (elt poly-seq i)))))
      (unless (= current-sign  prev-sign)
	(PROGN (return-global-vector poly-seq);release the global vector for reuse
						 (return nil))))
    
    ))
