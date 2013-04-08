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
	  line-start-point-x
	  line-start-point-y
	  line-end-point-x
	  line-end-point-y
	  make-line
	  )
	'pictures)

;Line Class Definition:

(defclass line (extent-cache graphic)
  (
   (start-x	:type		wcoord
                :initarg	:start-x
		:accessor       line-start-point-x 
		:documentation
 "object x-coordinate of start point for the line")
   
   (start-y	:type		wcoord
                :initarg	:start-y
		:accessor       line-start-point-y 
		:documentation
 "object y-coordinate of start point for the line")
   
   (end-x	:type		wcoord
                :initarg	:end-x
		:accessor       line-end-point-x 
		:documentation
 "object x-coordinate of end point for the line")
   
   (end-y	:type		wcoord
                :initarg	:end-y
		:accessor       line-end-point-y 
		:documentation
 "object y-coordinate of end point for the line")
   
   )
  (:documentation "A graphic that represents a line"))


;Function: make-line
;  Return a new line object whose end points are (START-X, START-Y) and
;  (END-X, END-Y).

(defun make-line (start-x start-y end-x end-y
                    &rest options
                    )
  (declare (type wcoord start-x start-y end-x end-y))

  (apply #'make-instance 'line
         :start-x start-x
         :start-y start-y
         :end-x   end-x
         :end-y   end-y
         options))


;Method: line-start-point
;  Return the coordinates of the START-POINT of the given LINE.

(defmethod line-start-point ((line line))

  (with-slots (start-x start-y) line
    (values start-x start-y)))



(defmethod (setf line-start-point-x) :after (start-x (line line))
  (declare (type wcoord start-x))
  (declare (ignore start-x))
  (extent-changed line))


;Method: line-start-point-y
;  Return or change the x coordinate of the START-POINT of the given LINE.


(defmethod (setf line-start-point-y)  :after (start-y (line line))
  (declare (type wcoord start-y))
  (declare (ignore start-y))

  (extent-changed line))


;Method: line-end-point
;  Return the coordinates of the END-POINT of the given LINE.

(defmethod line-end-point ((line line))

  (with-slots (end-x end-y) line
    (values end-x end-y)))


;Method: line-end-point-x
;  Return or change the x coordinate of the END-POINT of the given LINE.


(defmethod (setf line-end-point-x) :after (end-x (line line))
  (declare (type wcoord end-x))
  (declare (IGNORE  end-x))

  (extent-changed line))


;Method: line-end-point-y
;  Return or change the x coordinate of the END-POINT of the given LINE.


(defmethod (setf line-end-point-y) :after (end-y (line line))
  (declare (type wcoord end-y))
  (declare (IGNORE  end-y))

  (extent-changed line))
  

; Graphic methods for line graphics

;Method: extent-compute
;  Compute the extent rectangle for the LINE.

;  Note: A graphic's extent rectangle is defined in the object coordinate
;  system.  This means that each graphic should apply its own transform to
;  its computed extent before returning it.

(defmethod extent-compute ((line line))

  (with-slots (start-x start-y end-x end-y transform) line
    (let (new-start-x new-start-y new-end-x new-end-y)
      (multiple-value-setq (new-start-x new-start-y)
        (transform-point transform start-x start-y))
      (multiple-value-setq (new-end-x new-end-y)
        (transform-point transform end-x end-y))
      (let ((xmin (min new-start-x new-end-x))
	    (ymin (min new-start-y new-end-y))
	    (xmax (max new-start-x new-end-x))
	    (ymax (max new-start-y new-end-y)))
	(with-coercion ((xmin ymin xmax ymax) extent-element)
		       (make-extent-rect :xmin xmin
					 :ymin ymin
					 :xmax xmax
					 :ymax ymax))))))

;Method: draw-graphic
;  Draw the LINE object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and
;  HEIGHT are given, then only parts of the object that lie within the
;  given rectangle need to be drawn.

(DEFMACRO line-visible-p (graphic)
  `(AND
    (NOT (AND (and min-x min-y width height) ; Was optional rect given
	      (not (graphic-within-p ,graphic min-x min-y width height))
	      (not (graphic-intersects-p ,graphic min-x min-y width height)))) 
    (PROGN
      (UNLESS (valid-extent-p (graphic-extent ,graphic)))
      (OR (>= (/ (- (extent-rect-xmax extent) (extent-rect-xmin extent))
		 (view-scale view) )
	      (view-pixel-size view))
	  (= (/ (- (extent-rect-xmax extent) (extent-rect-xmin extent))
		(view-scale view) )
	     0))
      (OR
       (>= (/ (- (extent-rect-ymax extent) (extent-rect-ymin extent))
	      (view-scale view) )
	   (view-pixel-size view))
       (= (/ (- (extent-rect-ymax extent) (extent-rect-ymin extent))
	     (view-scale view) )
	  0))
      )
    (viewable-p ,graphic)))

(defmethod draw-graphic ((line line) (view view)
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (gstate start-x start-y end-x end-y extent) line
    (WHEN   (line-visible-p line) 
      (MULTIPLE-VALUE-BIND
	  (x y) (transform-point (graphic-world-transform line) start-x start-y)
	(MULTIPLE-VALUE-BIND
	    (x1 y1) (transform-point (graphic-world-transform line) end-x end-y)
	  (view-draw-line view  x y x1 y1
			  (graphic-gstate line)))))))

(defmethod draw-graphic-clipped ((graphic line) (view view)
				 min-x min-y width height)
  (declare (type wcoord min-x min-y width height))
  (with-slots (extent) graphic
    (WHEN (line-visible-p graphic)
      (SETF (gstate-clip-mask graphic)
	    (clip-mask graphic view min-x min-y width height))
      (draw-graphic graphic view)
      (SETF (gstate-clip-mask graphic) :none))
    ))

(DEFMETHOD graphic-damage ((graphic line))
  "Records graphic as a damaged region in each view to which it is attached."
  (with-slots ( gstate) graphic
    (LET ((extent (world-extent graphic)))
      (WHEN (valid-extent-p extent)
	(UNLESS (extent-valid-p graphic) (graphic-extent graphic))
	(LET* ((xmin (extent-rect-xmin extent))
	       (ymin (extent-rect-ymin extent)))
	  
	  (do ((g graphic (graphic-parent g)))
	      ((null g))
	    (dolist (view (graphic-views g))
	      (LET
		((width
		  (COND ((AND (NOT (AND gstate (gstate-line-width graphic)))
			      (= (- (extent-rect-xmax extent)
				    (extent-rect-xmin extent))
				 0))
			 (view-pixel-size view))
			((AND (NOT (AND gstate (gstate-line-width graphic)))
			      (< (- (extent-rect-xmax extent)
				    (extent-rect-xmin extent))
				 (view-pixel-size view)))
			 (view-pixel-size view))
			(t (- (extent-rect-xmax extent)
			      (extent-rect-xmin extent)))))
		 (height
		  (COND ((AND (NOT (AND gstate (gstate-line-width graphic)))
			      (=  (- (extent-rect-ymax extent)
				     (extent-rect-ymin extent))
				  0))
			 (view-pixel-size view))
			((AND (NOT (AND gstate (gstate-line-width graphic)))
			      (< (- (extent-rect-ymax extent)
				    (extent-rect-ymin extent)
				    )(view-pixel-size view)))
			 (view-pixel-size view))
			(t (- (extent-rect-ymax extent)
			      (extent-rect-ymin extent))))))
		(view-damage view xmin ymin width height)))))))))

(defmethod scale-transform ((graphic line) scale-x scale-y
                            &optional (fixed-x 0) (fixed-y 0))
  (DECLARE (IGNORE fixed-x fixed-y))
  (declare (type (or (satisfies plusp) (satisfies zerop)) scale-x scale-y))
  (declare (type ocoord fixed-x fixed-y))
  (graphic-damage graphic) ; Damage from old graphic
  
  (with-slots (transform start-x start-y end-x end-y) graphic
    (COND
      ((AND
	 (OR (AND (= start-x end-x) (NOT (= scale-x 1)))
	     (AND (= start-y end-y) (NOT (= scale-y 1))))
	 (NOT transform)) nil)
     ( t (call-next-method)))
    (extent-changed graphic)	; Notify graphic his extent may have changed
    (graphic-damage graphic)	; Damage from new graphic
    transform))

;Method: normalize-graphic
;  Normalize the LINE by applying its transform to its geometry, changing it
;  accordingly, and then setting its transform to nil (the identity transform).
;  Nothing of value is returned.

(defmethod normalize-graphic :before ((line line))

  (with-slots (start-x start-y end-x end-y transform) line
    (multiple-value-setq (start-x start-y)
      (transform-point transform start-x start-y))
    (multiple-value-setq (end-x end-y)
      (transform-point transform end-x end-y))))


;;This method is for determining if a pick is on a line

(DEFMETHOD graphic-contains-p ((line line)  x y &optional pixel)
  (DECLARE (type wcoord x y) )
  (SETF pixel (/ (OR (AND (graphic-gstate line) (gstate-line-width line))
		     pixel 2)
		 2))
  (with-slots (start-x start-y end-x end-y transform) line
    (MULTIPLE-VALUE-BIND
	(sx sy)(transform-point (graphic-world-transform line) start-x start-y)
      (MULTIPLE-VALUE-BIND
	  (ex ey) (transform-point (graphic-world-transform line) end-x end-y)
	(point-on-line-p pixel x y sx sy ex ey)
	)))
  )


;function for determining if a point is on a line no consideration is given
;to make sure the point is within the endpoints, this check is done by the
;graphic in the method: graphic-contains-p

(defun  point-on-line-p ( aperture x y x1 y1 x2 y2)
  (SETF aperture (* 2 aperture))
  
  (COND ((=  (FLOOR x1) (FLOOR x2))
	 (AND (<= (ABS (- x x1)) aperture)
	      (>= y (MIN y1 y2))
	      (<= y (MAX y1 y2))))	;perpendicular line with infinite slope
	((= (FLOOR y1)(FLOOR y2))
	 (AND
	  (<= (ABS (- y y1)) aperture)
	  (>= x (MIN x1 x2))
	  (<= x (MAX x1 x2))) ) ;horizontal line
	(t (<= (MIN 
		(ABS (+ (/ (* (- y2 y1)(- x x1))
			   (- x2 x1))
			y1 (- y)));pp 227-228 Computer Graphics - Harrington
		(ABS (+ (/ (* (- x2 x1)(- y y1))
			   (- y2 y1))
			x1 (- x)))) aperture) )
	)
  )
