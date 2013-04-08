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
	  make-polygon make-filled-polygon make-filled-polygon-edge 
	  )
	'pictures)

;Polygon Class Definition:

(defclass polygon (polypoint)
  ()
  (:documentation "the graphic class for drawing a polygon on the screen"))

;Function: make-polygon
; Return a new polygon object.

(defun make-polygon (point-seq  &rest options )
  "Make a polygon with the coordinates contained in POINT-SEQ.
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'polygon
	 :point-seq point-seq
	 options))


(defmethod draw-graphic ((polygon polygon) (view view)
			   &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate sensitivity extent) polygon
    (WHEN (visible-p polygon)
    (WHEN (AND ( > (LENGTH vertices) 0) (viewable-p polygon))
      (with-vector temp-vertices
	(copy-to-vector vertices temp-vertices)
	(transform-point-seq (graphic-world-transform polygon) temp-vertices)
	(view-draw-polygon view temp-vertices 
			   (graphic-gstate polygon)))))
    ))



(DEFMETHOD graphic-contains-p ((polygon polygon)  x y &optional (pixel 1) )
  (DECLARE (type wcoord x y) )
  (when (point-in-extents-p polygon x y pixel)
    (LET ((line-width
	   (OR (AND (graphic-gstate polygon)
		    (gstate-line-width polygon)
		    (+ (/ (gstate-line-width polygon) 2) pixel)) pixel)))
      (with-slots (vertices  transform) polygon
	(with-vector temp-vertices 
	  (copy-to-vector vertices temp-vertices)
	  (transform-point-seq (graphic-world-transform polygon) temp-vertices)
	  (point-near-line temp-vertices line-width x y))))))
	      

;define the filled-polygon class

(defclass filled-polygon (polygon)
  ()
  (:documentation "the graphic class for drawing a filled polygon on the screen"))

;Function: make-filled-polygon
;  Return a new filled polygon object.

(defun make-filled-polygon (point-seq  &rest options )
  "Make a filled-polygon with the coordinates contained in POINT-SEQ.
The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'filled-polygon
	 :point-seq point-seq
	 options))



;(DEFMETHOD graphic-contains-p ((filled-polygon filled-polygon) x y &optional pixel)
;  (DECLARE (type wcoord x y))
;  (WHEN (point-in-extents-p filled-polygon x y pixel)
;      (with-slots (vertices) filled-polygon
;	(with-vector temp-vertices
;		     (copy-to-vector vertices temp-vertices)
;		     (transform-point-seq (graphic-world-transform filled-polygon) temp-vertices)
;		     (WHEN 
;		       (inside-p temp-vertices (make-point :x x :y y)) t )))
;       ))

(defmethod graphic-contains-p ((filled-polygon filled-polygon) x y &optional pixel)
  
  ;; A variation of the "area fill" algorithm (see Lane J.  M. et al., ACM TOGS,
  ;; July, 1983, pp.  192-196). A series of triangles are formed connecting the
  ;; first vertex to the i'th and (1+ i)'th vertices. X/Y is inside the polygon
  ;; iff it is inside an odd number of these triangles.
  (when (point-in-extents-p filled-polygon x y pixel)
    (with-slots ( vertices) filled-polygon
      (with-vector points
	(copy-to-vector vertices points)
	(transform-point-seq (graphic-world-transform filled-polygon) points)
  
	(let*
	  ( ;;(points (polygon-points polygon))
	   
	   (p1x    (elt points 0))
	   (p1y    (elt points 1))
	   
	   (p2x    (elt points 2))
	   (p2y    (elt points 3))
	   
	   p3x p3y in-p)
	  
	  (do ((i 4 (+ i 2)))
	      ((>= i (length points))
	       in-p)
	    
	    (setf p3x (elt points i)
		  p3y (elt points (1+ i)))
	    
	    ;; Point inside next triangle?
	    (when
	      
		;; Use cross product to test if point is on same
		;; side of all triangle edges or if point lies on an edge.
		(let ((sign1 (signum (- (* (- p1x x) (- p2y y))
					(* (- p2x x) (- p1y y))))))
		(or (zerop sign1)
		    (let ((sign2 (signum (- (* (- p2x x) (- p3y y))
					    (* (- p3x x) (- p2y y))))))
		      (or (zerop sign2)
			  (when (= sign1 sign2)
			    (let ((sign3 (signum (- (* (- p3x x) (- p1y y))
						    (* (- p1x x) (- p3y y))))))
			      (or (zerop sign3)
				  (= sign1 sign3))))))))
	      
	      ;; Yes, complement inside-ness
	      (setf in-p (not in-p)))
	    
	    ;; Prepare next triangle.
	    (setf p2x p3x p2y p3y)))))))

; Method for drawing a filled-polygon

(defmethod draw-graphic ((filled-polygon filled-polygon) (view view)
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate extent) filled-polygon
    (WHEN (visible-p filled-polygon)
      (WHEN (AND ( > (LENGTH vertices) 0) (viewable-p filled-polygon))
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (transform-point-seq 
	   (graphic-world-transform filled-polygon) temp-vertices)
	  (view-draw-filled-polygon view temp-vertices 
				    (graphic-gstate filled-polygon)))))))

;;define the filled-polygon-edge class

(defclass filled-polygon-edge ( filled-polygon edge) ;mixin the edge class
  ()
  (:documentation
   "the graphic class for drawing a filled polygon with edge on the screen"))

;Function: make-filled-polygon-edge
;return a filled-polygon-edge

(defun make-filled-polygon-edge (point-seq  &rest options )
  "Make a filled-polygon-edge with the coordinates contained in POINT-SEQ.
The following keyword OPTIONS are allowed: GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'filled-polygon-edge
	 :point-seq point-seq
	 options))





;  Return a new filled-polygon-edge object.

(defmethod draw-graphic ((filled-polygon-edge filled-polygon-edge) (view view)
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate edge-gstate extent) filled-polygon-edge
    (when   (visible-p filled-polygon-edge)
      (WHEN (AND ( > (LENGTH vertices) 0) (viewable-p filled-polygon-edge))
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (transform-point-seq
	   (graphic-world-transform filled-polygon-edge) temp-vertices)
	  (view-draw-filled-polygon view temp-vertices 
				    (graphic-gstate filled-polygon-edge))
	  (view-draw-polygon view temp-vertices
			     (edge-gstate filled-polygon-edge)))
	))))
