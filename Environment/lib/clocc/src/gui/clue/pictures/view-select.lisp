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


(DEFMETHOD view-selection ((view view))
  (with-slots (selection) view
    (REMOVE nil (MAP 'LIST #'(lambda (graphic) (get-graphic graphic))
		     (scene-elements selection)))))

(DEFMETHOD (SETF view-selection) (selection (view view))
  (view-clear-selection view)
  (view-add-selection view  selection))

(DEFUN get-graphic (grabber-rect)
  (LET ((grabber-graphic (grabber-graphic grabber-rect)))
    (WHEN (AND grabber-graphic
     (viewable-p grabber-graphic))  grabber-graphic)))

(defmethod view-add-selection ((view view) graphic )
  (declare (type (or graphic list) graphic))

  (WHEN  graphic 			;When "graphic" is equal to nil, ignore it.
    (IF (ATOM graphic)			;view-drag-select
					;will call view-add-selection with a list
	(place-on-selection view graphic)
	(DOLIST  (graphic-item graphic)
	  (place-on-selection view graphic-item))))
  graphic)

(DEFUN place-on-selection (view graphic)
  "Attach the graphic to a highlight object in the selection scene"
  ;; a grabber-rect is the default highlight object
  (with-slots (selection) view
    (SETF (graphic-sensitivity selection ) :subselectable)
    (LET* ((selection-elements (scene-elements selection))
	   (elements-length (FILL-POINTER selection-elements))
	   (elements-dimension (ARRAY-DIMENSION selection-elements 0)))
      (OR
       (DOTIMES (pos elements-length )
	 (WHEN (EQ (grabber-graphic (ELT selection-elements pos)) graphic)
	   (RETURN t)))	; IF the graphic  is alread selected, return
       (PROGN
	 ;;there are no grabber rects in the scene and have to
	 ;; create a new one
	 (IF  (= elements-length elements-dimension)			
	      (scene-insert
	       selection 
	       (make-grabber-rect 
		view
		:highlight (view-highlight-color view)))
	      (SETF (FILL-POINTER selection-elements)
		    (1+ (FILL-POINTER selection-elements))))
	 (LET ((grabber-scene
		(ELT selection-elements
		     (1- (FILL-POINTER selection-elements)))))
	   (SETF (grabber-graphic ; set graphic slot to current graphic
		  grabber-scene) graphic)
	   ;     yes, make the grabber rect subselectable
	   (SETF (graphic-sensitivity grabber-scene) :subselectable)
	   (draw-graphic grabber-scene view))) ;     draw the highlight object
       )))
  graphic
  )


(defmethod view-remove-selection ((view view) graphic )
  (declare (type (or graphic list) graphic))

  (IF (ATOM graphic)
	(remove-from-selection view graphic)
	(DOTIMES (pos (LENGTH graphic))
	  (remove-from-selection view (ELT graphic pos))))
  (repair-view view)
   graphic)

(DEFUN remove-from-selection (view graphic)
  "Unattach the graphic from a highlight object in the selection scene"
  (with-slots (selection) view
    (DOTIMES (pos  (LENGTH (scene-elements selection)))	          
      (LET ((scene-graphic (ELT (scene-elements selection) pos)))
	;;Is the graphic attached to a highlight object in the scene
	(WHEN (EQ (grabber-graphic scene-graphic) graphic)
	  ;  yes, erase the highlight object with the xor function
	  (view-damage view (grabber-graphic scene-graphic))
	  ;     hide the highlight objec
	  (SETF (graphic-sensitivity scene-graphic) :hidden)
	  ;     unselect the graphic
	  (SETF (grabber-graphic scene-graphic) nil)
	  (RETURN)))))
  )

(DEFMETHOD view-clear-selection ((view view))
  (with-slots (selection) view
    (LET* ((elements (scene-elements selection))
	   (elements-length (FILL-POINTER elements)))
    (WHEN (> elements-length 0)
      (DOTIMES (pos elements-length  )	
	(LET* ((highlight (ELT elements  pos))
	      (graphic (grabber-graphic highlight)))
	  ;is the highlight object alread hidden
	  (WHEN (NOT (EQ (graphic-sensitivity highlight) :hidden))
	    (draw-graphic highlight view)
	    (graphic-damage graphic)
	    )
	  ;  no, hide the highlight object
	  (SETF (graphic-sensitivity highlight) :hidden)
	  ;      unselect the graphic
	  (SETF (grabber-graphic highlight) nil)))
      ;hide the selection-view
      (SETF (graphic-sensitivity (view-selection-scene view)) :hidden) )
      ;No selected graphics in the scene
      (SETF (FILL-POINTER elements) 0)))
  (repair-view view))

(DEFMETHOD view-select-graphic ((view view)  &key add   )
  (with-event (x y display)
    (PROGN
      (MULTIPLE-VALUE-BIND
	  (x1 y1) (view-untransform-point view x y)
	(LET ((graphic (graphic-pick
			(view-graphic view) x1 y1 (* 2 (view-pixel-size view))))
	      (selection-elements 
	       (AND (view-selection view)
		    (scene-elements (slot-value  view 'selection)))))
	      
	  (IF  graphic ;(AND graphic (NOT (EQL graphic (view-graphic view))))	;Has a graphic been picked?
	       (IF (AND (view-selection view)
			(DOTIMES (pos (LENGTH selection-elements))
			  (WHEN (EQ (grabber-graphic
				     (ELT selection-elements pos)) graphic)
			    (RETURN t))))
		   (unless add
		     (view-move-graphic graphic view ))
		   (PROGN 
		     (UNLESS add ; Yes, is the graphic to be added to the selection?
		       (view-clear-selection view))  ;   No, clear the selection.
		     (view-add-selection view graphic)	; Add the graphic to the view selection.
		     ;(view-move-graphic graphic view )
		     ))
	       (view-clear-selection view)	;  No,  clear the view-selection
	       ))))
    ))

(DEFMETHOD view-move-graphic ((graphic graphic) (view view) 
			      &key  (event :button-1))
  (LET* ((world-extent (world-extent graphic))
	 (fixed-x  (transform-x view (extent-rect-xmin world-extent)))
	 (fixed-y  (transform-y view (extent-rect-ymin world-extent)))
	 (px1 (transform-x view (extent-rect-xmax world-extent)))
	 (py1 (transform-y view (extent-rect-ymax world-extent)))
	 (highlight-color (view-highlight-color view))
	 (selection-elements (scene-elements (view-selection-scene view)))
	 (pixel-size (view-pixel-size view))
	 )
  (with-event (x y display key)
    (LET* ((*px* px1  )                   ( *py*  py1)
	   (*fixed-x* fixed-x)            (*fixed-y* fixed-y)
	   (*delta-fx* (- fixed-x x  ))   ( *delta-fy* (-  fixed-y y))
	   (*delta-px* (- x *px* ))       (*delta-py* (- y *py*)) )
      
      (DECLARE (SPECIAL *px* *py* *fixed-x* *fixed-y* *delta-fx*
			*delta-fy* *delta-px* *delta-py* *transform*))
      (WHEN (= *px* *fixed-x*)
	(SETF *px* (FLOOR (+ *px* pixel-size))
	      *delta-px* (FLOOR (+ *delta-px* pixel-size))))
      (WHEN (= *py* *fixed-y*)
	(SETF *py* (FLOOR (+ *py* pixel-size))
	      *delta-py* (FLOOR (+ *delta-py* pixel-size))))
 
      (with-event-mode (view `((:motion-notify ,event) (move-box))
			     `((:button-release ,event) (view-button-release t)))
	(LET ((display-after-func (display-after-function display))
	      (events-enabled-p (graphic-events-enabled-p view)))
	  (UNWIND-PROTECT
	      (progn
		(SETF (graphic-events-enabled-p view) nil)
		(SETF (display-after-function display) #'display-force-output)
		(drawbox-with-gc
		 view highlight-color *fixed-x* *fixed-y* *px* *py*)
		(grab-pointer view
			      #.(make-event-mask :button-release :pointer-motion)
			      :owner-p t)
		(CATCH :release
		  (LOOP
		    (process-next-event display )))
		(drawbox-with-gc
		 view highlight-color *fixed-x* *fixed-y* *px* *py*)
		(SETF (display-after-function display) nil))
	    (PROGN
	      (SETF (graphic-events-enabled-p view) events-enabled-p )
	      (ungrab-pointer display))
	    )
	  (SETF (display-after-function display) display-after-func))
	(erase-grabber-rects 
	 view  selection-elements (FILL-POINTER  selection-elements))
	(repair-view view)
	
	(move-selected-graphics
	 view selection-elements (FILL-POINTER  selection-elements)
	 (- (view-untransform-x view *fixed-x*)
	    (view-untransform-x view fixed-x ))
	 (-  (view-untransform-y view *fixed-y*)
	     (view-untransform-y view fixed-y)))))))
  (repair-view view))

(DEFMETHOD view-unselect-graphic ((view view) )
  (MULTIPLE-VALUE-BIND (x y) (pointer-position view)
    (MULTIPLE-VALUE-SETQ (x y) (view-untransform-point view x y))
    (LET (( graphic (graphic-pick (view-graphic view) x y  )))
      (WHEN (AND graphic (NOT (EQL graphic (view-graphic view))))
	(view-remove-selection view graphic)
	))))


(DEFMETHOD view-select-region ((view view) &key add  )
  (with-event (x y display)
    (LET* ((highlight-color (view-highlight-color view))
	   (*px* x  )              ( *py*  y)
	   ( fixed-x x)            ( fixed-y y)
	   )
      
      (DECLARE (SPECIAL *px* *py*  ))
      (with-event-mode 
       (view `((:motion-notify :button-3)  (scale-rubberband ,fixed-x ,fixed-y))
	     '((:button-release :button-3) (view-button-release t)))
       (process-motion-notify-events 
	view display fixed-x fixed-y *px* *py* highlight-color)
       (MULTIPLE-VALUE-BIND
	   ;Transform the view coordinates to world coordinates.
	   (x y)(view-untransform-point view fixed-x fixed-y)
	 (MULTIPLE-VALUE-BIND
	     (x1 y1)(view-untransform-point view *px* *py*)
	    (UNLESS add
	      (view-clear-selection view)) ;Clear the selection unless instructed
	   (IF (< x1 x) (ROTATEF x x1))
	   (IF (< y1 y) (ROTATEF y y1))
	   (view-add-selection view (graphic-within
				     (view-graphic view)
				     x   y  (- x1 x )   (- y1 y)))
	   ))))))


(DEFMETHOD view-unselect-region ((view view) )
  (with-event (x y display)
    (LET* ((highlight-color (view-highlight-color view))
	   (*px* x  )              ( *py*  y)
	   ( fixed-x x)            ( fixed-y y)
	   )
      
      (DECLARE (SPECIAL *px* *py*  ))
      (with-event-mode
       (view '((:motion-notify :button-3) (LIST 'scale-rubberband fixed-x fixed-y))
	     '((:button-release :button-1) (view-button-release t)))
       (process-motion-notify-events
	view display fixed-x fixed-y *px* *py* highlight-color)
       (MULTIPLE-VALUE-BIND 
	   (x y)
	   ;Transform the view coordinates to world coordinates.
	   (view-untransform-point view fixed-x fixed-y)
	 (MULTIPLE-VALUE-BIND
	     (x1 y1)
	     (view-untransform-point
	      view *px* *py*)
	   ;to add it to the existing selection.
	   (view-remove-selection view
				  (graphic-within
				   (view-graphic view)
				   x   y1  (- x1 x )   (- y y1)))
	   ))))))



;button release method used for user dialogs

(DEFMETHOD view-button-release ((view view) &optional throw)
 (WHEN throw (THROW :release t)))


(DEFMETHOD scale-rubberband ((view view) fixed-x fixed-y)
  (DECLARE (SPECIAL *px* *py* ))
  (with-slots (highlight-color display button-release-p) view
    (with-event (x y)
      (using-gcontext
	  (gc :drawable view
	      :function boole-xor
	      :foreground highlight-color)
	(drawbox  fixed-x fixed-y *px* *py*)
	(drawbox  fixed-x fixed-y x y))
      (SETF *px* x)
      (SETF *py* y)
      )))


(DEFMETHOD scale-rubberband-fixed-width ((view view) fixed-x fixed-y px)
  (DECLARE (SPECIAL *py* ))
  (with-slots (highlight-color display button-release-p) view
    (with-event (x y)
      (using-gcontext
	  (gc :drawable view
	      :function boole-xor
	      :foreground highlight-color)
	(drawbox  fixed-x fixed-y px *py*)
	(drawbox  fixed-x fixed-y px y))
      (SETF *py* y)
      )))

(DEFMETHOD scale-rubberband-fixed-height ((view view) fixed-x fixed-y py )
  (DECLARE (SPECIAL *px* ))
  (with-slots (highlight-color display button-release-p) view
    (with-event (x y)
      (using-gcontext
	  (gc :drawable view
	      :function boole-xor
	      :foreground highlight-color)
	(drawbox  fixed-x fixed-y *px* py)
	(drawbox  fixed-x fixed-y x py))
      (SETF *px* x)
      )))

(DEFMETHOD move-box ((view view) )
  (DECLARE (SPECIAL *px* *py* *fixed-x* *fixed-y* *delta-fy*
		    *delta-fx* *delta-py* *delta-px*))
  (with-slots (highlight-color display button-release-p selection) view
    (WHEN (> (LENGTH (scene-elements selection)) 0)
      (with-event (x y)
	(using-gcontext
	  (gc :drawable view
	      :function boole-xor
	      :foreground highlight-color)
	  (drawbox  *fixed-x* *fixed-y* *px* *py*)
	  (SETF  *fixed-x* (+ x *delta-fx*)
		 *fixed-y* (+ y *delta-fy*)
		 *px* (- x *delta-px*)
		 *py* (- y *delta-py*))
	  (drawbox  *fixed-x* *fixed-y* *px* *py*)))
      )))

(DEFMACRO change-to-identity (transform)
  `(with-slots (t11 t12 t21 t22 t31 t32) ,transform
     (SETF t11 1s0 t12 0s0 t21 0s0 t22 1s0 t31 0s0 t32 0s0)))
  

(DEFMETHOD rotate-box ((view view) )
  (DECLARE (SPECIAL *px* *py* *fixed-x* *fixed-y* *transform* *rotate-vector*))
  (with-slots (highlight-color display button-release-p) view
    (with-event (x y)
      (with-vector draw-vertices
	(copy-to-vector *rotate-vector* draw-vertices)
	(LET* ((sx (ELT draw-vertices 4))
	       (sy (ELT draw-vertices 5))
	       (angle
		     
		(- (IF (= x *fixed-x*)
		       (radians 0)
		       (+ (ATAN (/ (-   y *fixed-y*) (-  x *fixed-x* )))
			  (IF (< (* (SIGNUM (-  sx *fixed-x* ))
				    (-  x *fixed-x* ))
				 0) (radians 180) 0)))
		   (IF (= *fixed-x* sx)
		       0
		       (ATAN (/ (-  sy *fixed-y* )
				(-  sx *fixed-x* )))))	; (radians 180))
		     
		     ))
	  (using-gcontext
	    (gc :drawable view
		:function boole-xor
		:foreground highlight-color)
	    (transform-point-seq *transform* draw-vertices)
	    (round-vector draw-vertices)
	    (draw-lines view gc draw-vertices )
	    (copy-to-vector *rotate-vector* draw-vertices)
	    (change-to-identity *transform*)
	    (rotate-transform  *transform* angle
			       (ELT *rotate-vector* 0) (ELT *rotate-vector* 1))
	    (transform-point-seq *transform* draw-vertices)
	    (round-vector draw-vertices)
	    (draw-lines view gc draw-vertices )
	    (SETF *px* (ELT draw-vertices 4))
	    (SETF *py* (ELT draw-vertices 5))
	    ))))))
