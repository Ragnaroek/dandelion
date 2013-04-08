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
	  make-bspline make-closed-bspline make-filled-bspline
	  make-filled-bspline-edge 
	  )
	'pictures)




(defclass bspline (polypoint)
  ()
  (:documentation "the graphic class for drawing a bspline on the screen"))



(defun make-bspline (point-seq  &rest options )
  "Make a bspline with the coordinates contained in POINT-SEQ.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'bspline
	 :point-seq point-seq
	 options))



(defclass closed-bspline (bspline)
  ()
  (:documentation
   "the graphic class for drawing a closed bspline on the screen"))

(defun make-closed-bspline (point-seq  &rest options )
  "Make a bspline with the coordinates contained in POINT-SEQ.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'closed-bspline
	 :point-seq point-seq
	 options))

  
(defclass filled-bspline (bspline)
  ()
  (:documentation
   "the graphic class for drawing a filled bspline on the screen"))

(defun make-filled-bspline (point-seq  &rest options )
  "Make a bspline with the coordinates contained in POINT-SEQ.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'filled-bspline
	 :point-seq point-seq
	 options))


(defclass filled-bspline-edge (filled-bspline edge)
  ()
  (:documentation
   "the graphic class for drawing a filled bspline edge on the screen"))

(defun make-filled-bspline-edge (point-seq  &rest options )
  "Make a bspline-edge with the coordinates contained in POINT-SEQ.
   The following keyword OPTIONS are allowed:
   GSTATE PARENT SENESITIVITY TRANSFORM PLIST"

  (APPLY #'MAKE-INSTANCE 'filled-bspline-edge
	 :point-seq point-seq
	 options))

(PROCLAIM '(inline bspline-fwd-diff))
(DEFUN bspline-fwd-diff (c position dt dt2 dt3)
  "Return the forward differences for a B-spline cubic polynomial.
   (SUBSEQ C 0 4) is the B-spline geometry vector.
   DT is the increment to the curve parameter for each 
   iteration of the polynomial evaluation.
   DT2 and DT3 are DT squared and cubed, respectively."
  (LET* ((c0 (ELT c position))
	 (c1 (ELT c (+ position 2)))
	 (c2 (ELT c (+ position 4)))
	 (c3 (ELT c (+ position 6)))

	 ;; Compute coefficent of curve polynomial at3 + bt2 + ct +d
	 (a (* (/ 1 6) (+ (- c0) (* 3.0 c1) (* -3.0 c2) c3)))
	 (b (* (/ 1 6) (+ (* 3.0 c0) (* -6.0 c1) (* 3.0 c2))))
	 (c (* (/ 1 6) (+ (* -3.0 c0)  (* 3.0 c2))))
	 (d (* (/ 1 6) (+ c0 (* 4.0 c1)  c2)))

	 (adt3 (* a dt3))
	 (6adt3 (* 6.0 adt3))
	 (bdt2  (* b dt2)))

    (VALUES
      d
      (+ adt3 bdt2 (* c dt))
      (+ 6adt3 (+ bdt2 bdt2))
      6adt3)))


(DEFMACRO compose-spline (b-spline vertices)
 "Triple the end points so that B-spline will intersect first/last
  control points and place them in VERTICES"
  `(PROGN
     (DOTIMES (pos 2)
       (SETF (vertex-x ,b-spline pos) first-x))
     (DOTIMES (pos 2)
       (SETF (vertex-y ,b-spline pos) first-y))
     (SETF (vertex-i ,b-spline 2) ,vertices)
     (DOTIMES (pos 2)
       (SETF (vertex-x ,b-spline (+ *large-number* pos)) last-x))
     (LET ((length-spline (length-point-seq ,b-spline)))
       (DOTIMES (pos 2)
	 (SETF (vertex-y ,b-spline (-  length-spline pos 1)) last-y)))))

(DEFMACRO calculate-spline (vector)
  `(PROGN
    (DO ((i 0 (1+ i)))
	((EQ i ncurves))
       
      ;; Draw i'th curve      
       
      (MULTIPLE-VALUE-BIND
	  (x dx dx2 dx3) (bspline-fwd-diff spline (* i 2) dt dt2 dt3)
	(MULTIPLE-VALUE-BIND
	    (y dy dy2 dy3) (bspline-fwd-diff spline (1+ (* i 2))  dt dt2 dt3)
	  (DO ((n 0)) (())
	    (LET ((next-x x)
		  (next-y y))
	      ;; Draw n'th curve segment
	      (WHEN (AND prev-x (NOT (AND (= prev-x next-x) (= prev-y next-y))))
		(MULTIPLE-VALUE-BIND
		    (nx ny) (transform-point
			     (graphic-world-transform bspline) next-x next-y)
		  (LET ((length (length-point-seq ,vector)))
		    (SETF (vertex-x ,vector length) nx)
		    (SETF (vertex-y ,vector length) ny))))
	      (SETF prev-x x
		    prev-y y)
	      
	      (WHEN (EQ number-deltas (INCF n)) (RETURN))
	      
	      (INCF x dx)
	      (INCF y dy)
	      (INCF dx dx2)
	      (INCF dx2 dx3)
	      (INCF dy dy2)
	      (INCF dy2 dy3))))))
     ;; Draw final curve segment
    (MULTIPLE-VALUE-BIND
	(nx ny) (transform-point (graphic-world-transform bspline) last-x last-y)  
      (LET ((length (length-point-seq ,vector)))
	(SETF (vertex-x ,vector length) nx)
	(SETF (vertex-y ,vector length) ny))
      )))

(defmethod draw-graphic ((bspline bspline) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate extent) bspline
    (WHEN (visible-p bspline)
    (with-vector spline
	(WHEN  (> (LENGTH vertices) 0)
	  (LET ((last-x  (vertex-x vertices *large-number* ))
		(last-y  (vertex-y vertices *large-number* ))
		(first-x (vertex-x vertices 0))
		(first-y (vertex-y vertices 0)))
	    (compose-spline spline vertices)
	    (LET*
	      (prev-x prev-y
	       (number-deltas 4)
	       (dt      (/ 1.0 number-deltas))
	       (dt2     (* dt dt))
	       (dt3     (* dt2 dt))
	       (ncurves (+ 1 (length-point-seq vertices))))
	      (with-vector spline-points
		(calculate-spline spline-points)
		(view-draw-polyline view spline-points (graphic-gstate bspline))
	      ))))))))

(defmethod draw-graphic ((bspline closed-bspline) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate extent) bspline
    (WHEN (visible-p bspline)
      (with-vector spline
	(WHEN  (> (LENGTH vertices) 0)
	  (LET ((last-x  (vertex-x vertices *large-number* ))
		(last-y  (vertex-y vertices *large-number* ))
		(first-x (vertex-x vertices 0))
		(first-y (vertex-y vertices 0)))
	    (compose-spline spline vertices)
	    (LET*
	      (prev-x prev-y
	       (number-deltas 4)
	       (dt      (/ 1.0 number-deltas))
	       (dt2     (* dt dt))
	       (dt3     (* dt2 dt))
	       (ncurves (+ 1 (length-point-seq vertices))))
	      (with-vector spline-points
		(calculate-spline spline-points)
		(view-draw-polygon view spline-points (graphic-gstate bspline))
	      )
	      
	      )))))))


(defmethod draw-graphic ((bspline filled-bspline) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (vertices gstate extent) bspline
    (WHEN (visible-p bspline)
      (with-vector spline
	(WHEN  (> (LENGTH vertices) 0)
	  (LET ((last-x  (vertex-x vertices *large-number* ))
		(last-y  (vertex-y vertices *large-number* ))
		(first-x (vertex-x vertices 0))
		(first-y (vertex-y vertices 0)))
	    (compose-spline spline vertices)
	    (LET*
	      (prev-x prev-y
	       (number-deltas 4)
	       (dt      (/ 1.0 number-deltas))
	       (dt2     (* dt dt))
	       (dt3     (* dt2 dt))
	       (ncurves (+ 1 (length-point-seq vertices))))
	      (with-vector spline-points
		(calculate-spline spline-points)
		(view-draw-filled-polygon
		 view spline-points (graphic-gstate bspline)))
	      )))))))

(defmethod draw-graphic ((bspline filled-bspline-edge) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots
      (vertices gstate edge-gstate extent) bspline
    (WHEN (visible-p bspline)
      (with-vector spline
	(WHEN  (> (LENGTH vertices) 0)
	  (LET ((last-x  (vertex-x vertices *large-number* ))
		(last-y  (vertex-y vertices *large-number* ))
		(first-x (vertex-x vertices 0))
		(first-y (vertex-y vertices 0)))
	    (compose-spline spline vertices)
	    (LET*
	      (prev-x prev-y
	       (number-deltas 4)
	       (dt      (/ 1.0 number-deltas))
	       (dt2     (* dt dt))
	       (dt3     (* dt2 dt))
	       (ncurves (+ 1 (length-point-seq vertices))))
	      (with-vector spline-points
		(calculate-spline spline-points)
		(view-draw-filled-polygon
		 view spline-points (graphic-gstate bspline))
		(view-draw-polygon view spline-points (edge-gstate bspline))
		))))))))



(DEFMETHOD graphic-contains-p ((bspline bspline)  x y &optional (pixel 1))
  (DECLARE (type wcoord x y) )
 (when (point-in-extents-p bspline x y pixel)
   (with-vector spline
     (with-slots (vertices gstate) bspline
       (WHEN  (> (LENGTH vertices) 0)
	 (LET ((last-x  (vertex-x vertices *large-number* ))
	       (last-y  (vertex-y vertices *large-number* ))
	       (first-x (vertex-x vertices 0))
	       (first-y (vertex-y vertices 0)))
	   (compose-spline spline vertices)
	    (LET*
	      (prev-x prev-y
	       (line-width
		(OR (AND
		     (graphic-gstate bspline)
		     (gstate-line-width bspline)
		     (+ (/ (gstate-line-width bspline) 2) pixel)) pixel))
	       (number-deltas 4)
	       (dt      (/ 1.0 number-deltas))
	       (dt2     (* dt dt))
	       (dt3     (* dt2 dt))
	       (ncurves (+ 1 (length-point-seq vertices))))
	      (with-vector spline-points
		(calculate-spline spline-points)
		(point-near-line spline-points line-width x y)
		))))))))

(DEFMETHOD graphic-contains-p ((bspline closed-bspline)  x y &optional (pixel 1))
  (DECLARE (type wcoord x y) )
    (when (point-in-extents-p bspline x y pixel)
     (with-vector spline
	(with-slots (vertices gstate) bspline
	  (WHEN  (> (LENGTH vertices) 0)
	    (LET ((last-x  (vertex-x vertices *large-number* ))
		  (last-y  (vertex-y vertices *large-number* ))
		  (first-x (vertex-x vertices 0))
		  (first-y (vertex-y vertices 0)))
	      (compose-spline spline vertices)
	      (LET*
		(prev-x prev-y
		 (line-width
		  (OR (AND
		       (graphic-gstate bspline)
		       (gstate-line-width bspline)
		       (+ (/ (gstate-line-width bspline) 2) pixel)) pixel))
		 (number-deltas 4)
		 (dt      (/ 1.0 number-deltas))
		 (dt2     (* dt dt))
		 (dt3     (* dt2 dt))
		 (ncurves (+ 1 (length-point-seq vertices))))
		(with-vector spline-points
		  (calculate-spline spline-points)
		  (with-vector temp-vector
		    (VECTOR-PUSH-EXTEND first-x temp-vector)
		    (VECTOR-PUSH-EXTEND first-y temp-vector)
		    (SETF (vertex-i spline-points *large-number*) temp-vector)) 
		  (point-near-line spline-points line-width x y)
		  ))))))))

(defmacro inside-spline-p (points x y)
  `(let*
       ( ;;(points (polygon-points polygon))
	   
	(p1x    (elt ,points 0))
	(p1y    (elt ,points 1))
	   
	(p2x    (elt ,points 2))
	(p2y    (elt ,points 3))
	   
	p3x p3y in-p)
	  
     (do ((i 4 (+ i 2)))
	      
	 ((>= i (length ,points))
	  in-p)
	    
       (setf p3x (elt ,points i)
	     p3y (elt ,points (1+ i)))
       
       ;; Point inside next triangle?
       (when
	      
	   ;; Use cross product to test if point is on same side of all triangle
	   ;; edges or if point lies on an edge.
	   (let ((sign1 (signum (- (* (- p1x ,x) (- p2y ,y))
				   (* (- p2x ,x) (- p1y ,y))))))
	     (or (zerop sign1)
		 (let ((sign2 (signum (- (* (- p2x ,x) (- p3y ,y))
					 (* (- p3x ,x) (- p2y ,y))))))
		   (or (zerop sign2)
		       (when (= sign1 sign2)
			 (let ((sign3 (signum (- (* (- p3x ,x) (- p1y ,y))
						 (* (- p1x ,x) (- p3y ,y))))))
			   (or (zerop sign3)
			       (= sign1 sign3))))))))
	      
	 ;; Yes, complement inside-ness
	 (setf in-p (not in-p)))
	    
       ;; Prepare next triangle.
       (setf p2x p3x p2y p3y))))

(DEFMETHOD graphic-contains-p ((bspline filled-bspline)  x y &optional (pixel 1))
  (DECLARE (type wcoord x y) )
 (when (point-in-extents-p bspline x y pixel)
   (with-vector spline
     (with-slots (vertices gstate) bspline
       (WHEN  (> (LENGTH vertices) 0)
	 (LET ((last-x  (vertex-x vertices *large-number* ))
	       (last-y  (vertex-y vertices *large-number* ))
	       (first-x (vertex-x vertices 0))
	       (first-y (vertex-y vertices 0)))
	   (compose-spline spline vertices)
	    (LET*
	      (prev-x prev-y
	       (number-deltas 4)
	       (dt      (/ 1.0 number-deltas))
	       (dt2     (* dt dt))
	       (dt3     (* dt2 dt))
	       (ncurves (+ 1 (length-point-seq vertices))))
	      (with-vector spline-points
		(calculate-spline spline-points)
	      
		  (inside-spline-p spline-points x y )
		))))))))



(DEFMETHOD graphic-contains-p ((bspline filled-bspline-edge)  x y &optional (pixel 1))
  (DECLARE (type wcoord x y) )
 (when (point-in-extents-p bspline x y pixel)
   (with-vector spline
     (with-slots (vertices gstate) bspline
       (WHEN  (> (LENGTH vertices) 0)
	 (LET ((last-x  (vertex-x vertices *large-number* ))
	       (last-y  (vertex-y vertices *large-number* ))
	       (first-x (vertex-x vertices 0))
	       (first-y (vertex-y vertices 0))
	       (line-width
		(OR (AND
		     (graphic-gstate bspline)
		     (gstate-line-width bspline)
		     (+ (/ (gstate-line-width bspline) 2) pixel)) pixel)))
	   (compose-spline spline vertices)
	    (LET*
	      (prev-x prev-y
	       (number-deltas 4)
	       (dt      (/ 1.0 number-deltas))
	       (dt2     (* dt dt))
	       (dt3     (* dt2 dt))
	       (ncurves (+ 1 (length-point-seq vertices))))
	      (with-vector spline-points
		(calculate-spline spline-points)
		(OR 
			(inside-spline-p spline-points x y)
			(point-near-line spline-points line-width x y))
			
		))))))))

(DEFMETHOD graphic-damage ((graphic bspline))
  "Records graphic as a damaged region in each view to which it is attached."
  (with-slots (extent gstate) graphic
    (WHEN (valid-extent-p extent)
      (do ((g graphic (graphic-parent g)))
	  ((null g))
	(dolist (view (graphic-views g))
	  (UNLESS (extent-valid-p graphic) (graphic-extent graphic))
	  (WHEN (AND (NOT (AND gstate (gstate-line-width graphic)))
		     (< (ABS (- (extent-rect-xmax extent)
				(extent-rect-xmin extent)))
			(view-pixel-size view)))
	    (SETF (extent-rect-xmax extent) (+ (extent-rect-xmin extent)
					       (view-pixel-size view))))
	  (view-damage view graphic))))))
















