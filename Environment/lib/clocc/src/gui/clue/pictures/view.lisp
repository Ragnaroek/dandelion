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
	  graphic-events-enabled-p 
	  view
	  view-scale
	  view-scale-x
          view-scale-y
	  make-view
	  display
	  gravity-point
	  view-gravity
	  view-pan
	  view-show-world
	  view-show-region
	  view-damage
	  view-damaged-p
	  view-graphic
	  view-pixel-size
	  view-highlight-color
	  refresh-view
	  repair-view
	  view-scale-point
	  transform-point
	  transform-x
	  transform-y
	  view-transform-vector
	  view-untransform-point
	  view-untransform-x
	  view-untransform-y
	  view-zoom
	  world-extent
	  view-draw-arc
	  view-draw-filled-arc
	  view-draw-line
	  view-draw-polypoint
	  view-draw-polyline
	  view-draw-polygon
	  view-draw-filled-polygon
	  view-draw-image
	  view-draw-rectangle
	  view-draw-filled-rectangle
	  view-draw-text
	  view-draw-image-text
	  view-draw-char
	  view-draw-image-char
	  handle-event
	  graphic-pick
	  graphic-within
	  view-selection
	  view-add-selection
	  view-remove-selection
	  view-clear-selection
	  view-select-graphic
	  view-unselect-graphic
	  view-select-region
	  view-unselect-region
	  view-x-pan
	  view-y-pan
	  view-pan-left
	  view-pan-right
	  view-pan-up
	  view-pan-down
	  untransform-point
	  view-orientation
	  origin-x
	  orinin-y
	  )
	'pictures)


(DEFPARAMETER  *an-extent-rectangle* (make-extent-rect))

;Private Macro: valid-xcoord
;  Determine whether the given VAR is a valid coordinate for the X window system



;Function: make-view
;  Return a new view object.

(defun make-view (&rest options &key &allow-other-keys)
  "Make a new view.
The following keyword OPTIONS are allowed: GRAVITY RESIZE-EXTENT-P SCALE ORIGIN-X ORIGIN-Y GRAPHIC"
  (apply #'make-contact 'view options))

;Basic contact methods:

	

(DEFUN get-contact-background (view)
  (IF view
       (LET ((background (contact-background view)))
	 (COND
	   ((NUMBERP background) background)
	   ((pixmap-p background) 0)
	   ((EQL background :none) 0)
	   ((EQL background :parent-relative) (get-contact-background (contact-parent view)))))
       0))

(DEFMETHOD initialize-instance :after ((view view) &key)
  (with-slots (grabber-rect-transform) view
    (SETF grabber-rect-transform (make-transform))))

(DEFMETHOD realize :after ((view view))

  (with-slots (default-gcontext selection highlight-color) view
    (setf default-gcontext (create-gcontext :drawable view ))
    (LET ((black (screen-black-pixel (contact-screen view)))
	    (white (screen-white-pixel (contact-screen view)))
	    (background (get-contact-background view)))
	
	(IF 
	  (EQL  background black)
	    (PROGN 
	      (SETF (gcontext-foreground default-gcontext) white )
	      (SETF (gcontext-background default-gcontext) black ))
	    (PROGN 
	      (SETF (gcontext-foreground default-gcontext) black )
	      (SETF (gcontext-background default-gcontext) white )))
	(SETF highlight-color (LOGXOR (gcontext-foreground default-gcontext)
				      (gcontext-background default-gcontext)))
      )
    
    (LET ((selection-scene (make-selection-scene)))
      (SETF (graphic-view selection-scene)  view)	;attach the view-selection to a view
      (SETF (view-selection-scene view) selection-scene)
      (DOTIMES (place 10)
	(scene-insert
	 selection-scene
	 (make-grabber-rect view :parent selection-scene
			    :highlight (view-highlight-color view)))
	)
      (with-slots (elements parent) selection-scene
	(SETF (FILL-POINTER elements) 0)
	(SETF parent nil))
      
      (with-slots (view-graphic) view
	(UNLESS view-graphic
	  (SETF view-graphic (make-scene :sensitivity :subselectable))
	  (SETF (graphic-view view-graphic) view))))))



(DEFMETHOD resize :around ((view view) width height border-width)
  (with-slots (origin-x origin-y gravity ) view
     (MULTIPLE-VALUE-BIND (gravity-x gravity-y) (gravity-point view gravity)
	(call-next-method)
	(MULTIPLE-VALUE-BIND (new-gravity-x new-gravity-y)(gravity-point view gravity)
	  (SETF origin-x (- origin-x (- new-gravity-x gravity-x)))
	  (SETF origin-y (- origin-y (- new-gravity-y gravity-y)))))))
	  
;Method: display
;  Display the view contact after an exposure, etc.

(defmethod display ((view view) &optional (x 0) (y 0) (width (contact-width view))
		     (height (contact-height view)) &key)

  (with-slots (view-graphic damage-count origin-x origin-y
	       (contact-height height)
	       default-gcontext gcontext) view
    (LET ((scale-x (view-scale-x view))
	  (scale-y (view-scale-y view))
	  (scale (view-scale view))
	  (pixel (view-pixel-size view)))
    
    (when view-graphic
      (IF (AND (= x 0)
	       (= y 0)
	       (= width (contact-width view))
	       (= height (contact-height view)))
	  (PROGN
	    (SETF damage-count 0)
	    (refresh-view view))
	  (progn
	    (view-damage 
	     view	; Notify damage control
	     (+ origin-x (- pixel) (/ x scale-x))
	     (+ origin-y (- pixel) (/ (- contact-height y height) scale-y))
	     (float (+ (/ width scale)  pixel))
	     (float (+ (/ height scale) pixel)))
	    (repair-view view)))))))	; Go repair the damage

;View Attribute Methods:
(defmethod gravity-point ((view view) gravity)
  (declare (type (member :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast)
                 gravity))
    (let* ((extent (world-extent view))			; Get the extent
           (xmin (extent-rect-xmin extent))
           (xmax (extent-rect-xmax extent))
           (ymin (extent-rect-ymin extent))
           (ymax (extent-rect-ymax extent))
           (xmid (/ (+ xmin xmax) 2.0))			; Compute mid points
           (ymid (/ (+ ymin ymax) 2.0)))
      (case gravity 				; Return the appropriate coord
        (:northwest	(values xmin ymax))
        (:north		(values xmid ymax))
        (:northeast	(values xmax ymax))
        (:west		(values xmin ymid))
        (:center	(values xmid ymid))
        (:east		(values xmax ymid))
        (:southwest	(values xmin ymin))
        (:south		(values xmid ymin))
        (:southeast	(values xmax ymin)))))


(DEFMETHOD view-orientation ((view view) &key (x 1) (y 1))
  (SETF (view-scale-x  view) (* (SIGNUM x) (view-scale-x view))
	(view-scale-y  view) (* (SIGNUM y) (view-scale-y view))
	(slot-value (grabber-rect-transform view) 't11)
	 (* (SIGNUM x) (slot-value (grabber-rect-transform view) 't11) )
	(slot-value (grabber-rect-transform view) 't22)
	 (* (SIGNUM x) (slot-value (grabber-rect-transform view) 't22) )))

;Method: view-gravity
;  Returns or changes the current VIEW-GRAVITY.  The view-gravity is the
;  alignment point of the graphic's bounding rectangle that remains fixed
;  (in world coordinates) after the view window is resized.

(defmethod view-gravity ((view view))
  (with-slots (gravity) view
    gravity))

(defmethod (setf view-gravity) (gravity (view view))
  (with-slots ((view-gravity gravity)) view
    (setf view-gravity gravity)))


(DEFMETHOD view-scale ((view view))
  (ABS (view-scale-x view)))

(DEFMETHOD (SETF view-scale) (x (view view))
  (MULTIPLE-VALUE-BIND (pan-x pan-y) (gravity-point view (view-gravity view))
	 
    (SETF (view-scale-x view) (* (SIGNUM (view-scale-x view)) x)
	  (view-scale-y view) (* (SIGNUM (view-scale-y view)) x)
	  (slot-value (grabber-rect-transform view) 't11)
	   (* (SIGNUM (view-scale-x view)) (/ 1 x) )
	  (slot-value (grabber-rect-transform view) 't22)
	   (* (SIGNUM (view-scale-y view)) (/ 1 x) ))
    (view-pan view pan-x pan-y))
  x)
	   
;Method: view-pan
;  Change the VIEW so that the given point (X, Y) (in world coordinates) is
;  located according to the given ALIGN point of the view window.

(defmethod view-pan ((view view) x y &optional (gravity (view-gravity view)))
  (declare (type (member :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast)
                 gravity))

  (with-slots (width height origin-x origin-y ) view
    (let* (
	   (scale-x (view-scale-x view))
	   (scale-y (view-scale-y view))
	   (wc-width  	   (float (/ width scale-x)))
           (wc-height 	   (float (/ height scale-y)))
           (wc-half-width  (float (/ wc-width 2)))
           (wc-half-height (float (/ wc-height 2))))
      (case gravity
        (:northwest	(setf origin-x x
                              origin-y (- y wc-height)))
        (:north		(setf origin-x (- x wc-half-width)
                              origin-y (- y wc-height)))
        (:northeast	(setf origin-x (- x wc-width)
                              origin-y (- y wc-height)))
        (:west		(setf origin-x x
                              origin-y (- y wc-half-height)))
        (:center	(setf origin-x (- x wc-half-width)
                              origin-y (- y wc-half-height)))
        (:east		(setf origin-x (- x wc-width)
                              origin-y (- y wc-half-height)))
        (:southwest	(setf origin-x x
                              origin-y y))
        (:south		(setf origin-x (- x wc-half-width)
                              origin-y y))
        (:southeast	(setf origin-x (- x wc-width)
                              origin-y y))))))
                    

(DEFMETHOD view-show-world ((view view))
 
  (LET ((extent (world-extent (view-graphic view))))
    (UNLESS (and (zerop (- (extent-rect-xmax extent)(extent-rect-xmin extent)))
		 (zerop (- (extent-rect-ymax extent)(extent-rect-ymin extent))))
      (let ((width  (contact-width  view))
	    (height (contact-height view)))
      (with-coercion ((width height) extent-element)
	(SETF (view-scale view)
	      (MIN (/ width
		      (- (extent-rect-xmax extent)(extent-rect-xmin extent)))
		   (/ height
		      (- (extent-rect-ymax extent) (extent-rect-ymin extent)))))

	(view-pan view (extent-rect-xmin extent)(extent-rect-ymin extent))
	(refresh-view view))))))



(DEFMETHOD view-show-region ((view view) extent)
  (UNLESS (= (- (extent-rect-xmax extent)(extent-rect-xmin extent))
	     (- (extent-rect-ymax extent) (extent-rect-ymin extent)) 0)

    (SETF (view-scale view)
	  (MIN (/ (contact-width view)
		  (- (extent-rect-xmax extent)(extent-rect-xmin extent)))
	       (/ (contact-height view)
		  (- (extent-rect-ymax extent) (extent-rect-ymin extent))))))
  
  (LET* 
    ((extent-xmin (extent-rect-xmin extent))
     (extent-ymin (extent-rect-ymin extent))
     (extent-width (- (extent-rect-xmax extent)(extent-rect-xmin extent)) )
     (extent-height (-  (extent-rect-ymax extent)(extent-rect-ymin extent))))
    (MULTIPLE-VALUE-BIND 
	(x y) (CASE (view-gravity view)
				 
		(:southwest  (VALUES  extent-xmin
				      extent-ymin  ))
		(:northwest  (VALUES  extent-xmin
				      (+ extent-ymin  extent-height) ))
		(:south      (VALUES  (+ extent-xmin  (/ extent-width 2.0))
				      extent-ymin))
		(:north      (VALUES  (+ extent-xmin  (/ extent-width 2.0))
				      (+ extent-ymin  extent-height)))
		(:west       (VALUES  extent-xmin
				      (+ extent-ymin  (/ extent-height 2.0))))
		(:center     (VALUES  (+ extent-xmin  (/ extent-width 2.0))
				      (+ extent-ymin  (/ extent-height 2.0))))
		(:southeast  (VALUES  (+ extent-xmin extent-width)
				      extent-ymin))
		(:northeast  (VALUES  (+ extent-xmin extent-width)
				      (+ extent-ymin  extent-height)))
		(:east       (VALUES  (+ extent-xmin extent-width)
				      (+ extent-ymin  (/ extent-height 2.0))))
		(t           (VALUES   extent-xmin
				       extent-ymin))) 
      (view-pan view x y))
    
    ))

;Method: view-damage
;  Records a damaged region of the VIEW for later repair. The
;  DAMAGED-REGION contains either a single graphic object (in which case
;  the damaged region is given by the object's extent) or a world
;  coordinate list of the form (min-x min-y width height).

(defmethod view-damage ((view view) &rest damaged-region)
  (with-slots (damage-count damage) view
    ; Compute damage extent rectangle
    (let ((new-damage (make-extent-rect :valid t))
	  (view-extent (world-extent view))
          min-union max-intersect
          min-union-area
          (max-intersect-area 0))
      (if (typep (car damaged-region) 'graphic)
	  (PROGN
	    (UNLESS (extent-valid-p (CAR damaged-region))
	      (graphic-extent (CAR damaged-region)))
	    (world-extent (car damaged-region) new-damage))
	  
          (setf (extent-rect-xmin new-damage) (first  damaged-region)
		(extent-rect-ymin new-damage) (second damaged-region)
		(extent-rect-xmax new-damage) (+ (first damaged-region)
						 (third  damaged-region))
		(extent-rect-ymax new-damage) (+ (second damaged-region)
						 (fourth damaged-region))))
      (WHEN 
	(not (or
	      (> (extent-rect-xmin new-damage) (extent-rect-xmax view-extent))
	      (> (extent-rect-ymin new-damage) (extent-rect-ymax view-extent))
	      (< (extent-rect-xmax new-damage) (extent-rect-xmin view-extent))
	      (< (extent-rect-ymax new-damage) (extent-rect-ymin view-extent))))
	
	(dotimes (i damage-count) ; Calculate min-union and max-intersect
	  (let ((union-area     (rect-union new-damage (AREF damage i)))
		(intersect-area (rect-intersect new-damage (AREF damage i))))
	    
	    (when (or (null min-union)
		      (< union-area min-union-area))
	      (setf min-union-area union-area)
	      (setf min-union i))
	    
	    (when (> intersect-area max-intersect-area)
	      (setf max-intersect-area intersect-area)
	      (setf max-intersect i))))
	
	(cond
	  ((> max-intersect-area 0)		; It intersected with something
	   (rect-merge new-damage		; Use the largest such intersection
		       (aref damage max-intersect)))
	  
	  ((< damage-count max-damage)		; No intersection, room for more?
	   (incf damage-count)			; Yes, add it to the list
	   (setf (aref damage (- damage-count 1)) new-damage))
	  
	  (t				; Otherwise, just use the smallest union
	   (rect-merge new-damage
		       (aref damage min-union))))))))


;Method: view-damaged-p
;  Returns true if there are damaged regions to repair. May be used with
;  SETF to reset the VIEW's damage. If the new value is false, then any
;  previous damage is ignored; otherwise, the new value is ignored.

(defmethod view-damaged-p ((view view))
  (with-slots (damage-count) view
    (plusp damage-count)))


(defmethod (setf view-damaged-p) (damaged (view view))
  (declare (type boolean damaged))
  (with-slots (damage-count) view
    (unless damaged (setf damage-count 0))))


;Method: view-graphic 
;  Returns or changes the SCENE associated with the given VIEW.



(defmethod  (setf view-graphic)  :after (view-graphic (view view)  )
  (declare (type (or null graphic) view-graphic))
  (with-slots ((graphic view-graphic) )  view
    (SETF graphic view-graphic)
    (with-slots (views) view-graphic
      (push view views))))


;Method: view-pixel-size
;  Return the world-coordinate size of a pixel for the given VIEW.

(defmethod view-pixel-size ((view view))
  (LET ( (scale (view-scale view)))
    (/ 1 scale)))


;Method: refresh-view
;  Redraws the entire VIEW scene and clears any damages.

(defmethod refresh-view ((view view))

  (with-slots (view-graphic origin-x origin-y width height  damage-count) view
    (LET ((scale-x (view-scale-x view))
	  (scale-y (view-scale-y view)))
      (clear-area view)
      (display-force-output (contact-display view))
      (setf damage-count 0)
      (when view-graphic
	(draw-graphic view-graphic view origin-x origin-y
		      (float (/ width scale-x))
		      (float (/ height scale-y)))
	(display-force-output (contact-display view))
	))
    ))

(DEFMETHOD refresh-view :after ((view view))
      (draw-graphic (view-selection-scene view) view)
 )
	
;Method: repair-view
;  Redraws any damaged regions in the VIEW and clears any damages.

(defmethod repair-view ((view view))

  (with-slots (damage-count damage view-graphic default-gcontext selection) view
   (dotimes (i damage-count)				; For each damage rectangle
      (let ((xmin (extent-rect-xmin (ELT damage i)))	; Store corners locally
            (ymin (extent-rect-ymin (ELT damage i)))
            (xmax (extent-rect-xmax (ELT damage i)))
            (ymax (extent-rect-ymax (ELT damage i)))
            (pixel (view-pixel-size view)))
      (multiple-value-bind (clip-xmin clip-ymin) ; Compute clipping rectangle
          (transform-point view xmin ymin)
        (multiple-value-bind (clip-xmax clip-ymax)
            (transform-point view xmax ymax)
	  (WHEN (< clip-xmax clip-xmin)
	    (rotatef clip-xmin clip-xmax))
	  (WHEN (< clip-ymax clip-ymin)
	    (rotatef clip-ymin clip-ymax)) ; View coordinates are third quadrant
	  
          (clear-area view
		      :x clip-xmin
                      :y clip-ymin
                      :width  (- clip-xmax clip-xmin -1)
                      :height (- clip-ymax clip-ymin -1))
          (draw-graphic-clipped
	   view-graphic view ; Draw view-graphic within damaged area
	   (- xmin (* 2 pixel))		
	   (- ymin (* 2 pixel))
	   (- xmax xmin (- (* 4 pixel))) 
	   (- ymax ymin (- (* 4 pixel))))
	  (display-force-output (contact-display view))
          (setf (gcontext-clip-mask default-gcontext)	; Get rid of clip-mask
                :none)))))

    (setf damage-count 0))				; Clear the damages
  (draw-graphic (view-selection-scene view) view) ; Draw the highlight objects
  ) 




;Method: view-scale-point
;  Convert the given X-DISTANCE and Y-DISTANCE to equivalent distances in
;  the VIEW coordinate system.  If GRAPHIC-WORLD-TRANSFORM is given, apply
;  it to the distances before converting to view coordinates.

(defmethod view-scale-point ((view view) x-distance y-distance
                                       &optional graphic-world-transform)
  (declare (type (or null transform) graphic-world-transform))

  (with-slots ( height) view
    (LET ((scale-x (view-scale-x view))
	  (scale-y (view-scale-y view)))
    (multiple-value-bind (world-x world-y)
        (scale-point graphic-world-transform x-distance y-distance)
      (values (floor (* world-x scale-x))
              (floor (* world-y scale-y)))))))
  
           
;Method: transform-point
;  Convert the given X and Y object coordinates to view coordinates for the
;  given VIEW.  If GRAPHIC-WORLD-TRANSFORM is given, apply it to the point
;  before converting to view coordinates.
(defmethod transform-point ((view view) x y )
  (with-slots (origin-x origin-y scale-x scale-y height) view

      (values (floor (* (- x origin-x) scale-x))
              (FLOOR (- height  (* (- y origin-y) scale-y))))))

(defmethod transform-x ((view view) x  )

  (with-slots (origin-x origin-y scale-x height) view
     (floor (* (- x origin-x) scale-x))))

(defmethod transform-y ((view view)  y )

  (with-slots (origin-x origin-y scale-y height) view
    (floor (- height (* (- y origin-y) scale-y)))))

(DEFMETHOD view-transform-vector ((view view) vertices &optional round)
  "this function destructively changes the value of vertices
by applying the view transform to them"
  (with-slots (origin-x origin-y scale-x scale-y height) view
    (IF round
	(DO ((i 0 (+ i 2)))
	    ((>= i (LENGTH vertices)))
	  (SETF (elt vertices i)
		(round (* (- (ELT vertices i) origin-x) scale-x)))
	  (SETF (ELT vertices (1+ i))
		(- height (round (* (- (ELT vertices (1+ i)) origin-y) scale-y)))))
	
	(DO ((i 0 (+ i 2)))
	    ((>= i (LENGTH vertices)))
	  (SETF (elt vertices i)
		(floor (* (- (ELT vertices i) origin-x) scale-x)))
	  (SETF (ELT vertices (1+ i))
		(FLOOR (- height
			  (* (- (ELT vertices (1+ i)) origin-y) scale-y))))))
    (VALUES vertices)))


(defmethod view-untransform-point ((view view) window-x  window-y)

  (VALUES 
    (+ (origin-x view)
       (/ window-x (view-scale-x view))) ;change to world coordinates
    (- (+ (origin-y view) (/ (contact-height view) (view-scale-y view)))
       (/ window-y (view-scale-y view))))  ;change from 4th quadrant view
  ) ;coordinates to 1st quadrant world coordinate

(defmethod untransform-point ((view view) window-x  window-y)

  (VALUES 
    (+ (origin-x view)
       (/ window-x (view-scale-x view))) ;change to world coordinates
    (- (+ (origin-y view) (/ (contact-height view) (view-scale-y view)))
       (/ window-y (view-scale-y view))))  ;change from 4th quadrant view
  ) ;coordinates to 1st quadrant world coordinate


(defmethod view-untransform-x ((view view) window-x )
         ;change to X world coordinates
    (+ (origin-x view)
       (/ window-x (view-scale-x view)))
  ;change from 4th quadrant view coordinates
  ;to 1st quadrant world coordinate
    )


(defmethod view-untransform-y ((view view) window-y )

    (- (+ (origin-y view)
	  (/ (contact-height view)
	     (view-scale-y view)))	;change to Y world coordinates
       (/ window-y (view-scale-y view)) ;change from 4th quadrant view coordinates
       ))



;Method: view-zoom
;  Change the scale of the VIEW. The horizontal and vertical scale are
;  changed uniformly. If ABSOLUTE-P is true, then SCALE is an absolute
;  scale factor; otherwise, SCALE is multiplied with the current scale to
;  form the new scale.  FIXED-POINT is a point in the view that will remain
;  fixed after the scale is performed.  Any of the nine possible alignment
;  points may be specified and the default is :southwest.

(defmethod view-zoom ((view view) scale
		      &key  (absolute-p nil)(fixed-point :southwest))
  (declare (type (AND number (satisfies plusp)) scale))
  (declare (type boolean absolute-p))
  (declare (type (OR list (member :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast))
                 fixed-point))
  (LET (fixed-point-x fixed-point-y)
    (IF (LISTP fixed-point)
	(PROGN
	  (SETF fixed-point-x (FIRST fixed-point))
	  (SETF fixed-point-y (SECOND fixed-point)))
	
	(MULTIPLE-VALUE-SETQ
	    (fixed-point-x fixed-point-y)	; Remember the fixed point
	  (gravity-point view fixed-point)))
      (if absolute-p
          (setf (view-scale view)  scale) ;   Absolutely
          (setf (view-scale view ) (* (view-scale view) scale))) ;   Relatively
    (view-pan view fixed-point-x fixed-point-y ;   If we need to
	      fixed-point)))


;Method: world-extent
;  Return the extent of the given VIEW in world coordinates.  A nil value
;  means that the extent is undefined.  If RESULT-EXTENT is provided, it is
;  used to store the result extent.  Otherwise, a new extent-rect is
;  created and returned.

(defmethod world-extent ((view view) &optional result-extent)
  (declare (type (or null extent-rect) result-extent))

  (with-slots (origin-x origin-y  width height) view
    (unless (or (zerop width) (zerop height))
      (let ((new-extent (or result-extent (make-extent-rect)))
	    (scale (view-scale view)))
	(with-coercion
	 ((origin-x origin-y width height scale) extent-element)
	 (setf (extent-rect-xmin new-extent) origin-x
	       (extent-rect-ymin new-extent) origin-y
	       (extent-rect-xmax new-extent) (+ origin-x (/ width scale))
	       (extent-rect-ymax new-extent) (+ origin-y (/ height scale))
	       (extent-rect-valid new-extent) t)
	 new-extent)))))

		 
;Private Function: rect-union
;  Return the area of the union of RECT1 and RECT2.

(defun rect-union (rect1 rect2)
  (declare (type extent-rect rect1 rect2))

  (* (- (max (extent-rect-xmax rect1)
             (extent-rect-xmax rect2))
        (min (extent-rect-xmin rect1)
             (extent-rect-xmin rect2)))
     (- (max (extent-rect-ymax rect1)
             (extent-rect-ymax rect2))
        (min (extent-rect-ymin rect1)
             (extent-rect-ymin rect2)))))

;Private Function: rect-intersect
;  Return the area of the intersection of RECT1 and RECT2.

(defun rect-intersect (rect1 rect2)
  (declare (type extent-rect rect1 rect2))

  (let ((x-length (- (min (extent-rect-xmax rect1)
                          (extent-rect-xmax rect2))
                     (max (extent-rect-xmin rect1)
                          (extent-rect-xmin rect2))))
        (y-length (- (min (extent-rect-ymax rect1)
                          (extent-rect-ymax rect2))
                     (max (extent-rect-ymin rect1)
                          (extent-rect-ymin rect2)))))
    (if (and (plusp x-length)
             (plusp y-length))
        (* x-length y-length)
        0)))

  
;Private Function: rect-merge
;  Merge (union) RECT1 and RECT2 and modify RECT2 to contain the result.

(defun rect-merge (rect1 rect2)
  (declare (type extent-rect rect1 rect2))

  (setf (extent-rect-xmax rect2)
        (max (extent-rect-xmax rect1)
             (extent-rect-xmax rect2))
        
        (extent-rect-xmin rect2)
        (min (extent-rect-xmin rect1)
             (extent-rect-xmin rect2))

        (extent-rect-ymax rect2)
        (max (extent-rect-ymax rect1)
             (extent-rect-ymax rect2))

        (extent-rect-ymin rect2)
        (min (extent-rect-ymin rect1)
             (extent-rect-ymin rect2)))
  rect2)
