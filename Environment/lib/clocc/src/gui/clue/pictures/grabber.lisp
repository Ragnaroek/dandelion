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


(DEFPARAMETER  grabber-side-size 6
  "This is the size of the width and the height of the grabber")

(export '(
	  make-selection-scene
	  view-transform-graphic
	  view-move-graphic
	  view-scale-graphic
	  view-rotate-graphic
	  )
	'pictures)

(DEFUN round-vector (VECTOR)
  "round all of the values of a vector"
  (DOTIMES (pointer (FILL-POINTER vector) nil)
    (SETF (ELT vector pointer) (FLOOR  (ELT vector pointer) ))))

(defclass selection-scene (scene)
  (
   (grabber-size    :type   integer
		    :initform grabber-side-size
		    :accessor grabber-size
		    :documentation
 "The size of the grabber for the grabber-rect")
    )
  (:documentation "The graphic class for representing the selection scene"))


(DEFMACRO scene-visible-p (graphic)
  `(AND
    (NOT (AND (and min-x min-y width height) ; Was optional rect given
	      (not (graphic-within-p ,graphic min-x min-y width height))
	      (not (graphic-intersects-p ,graphic min-x min-y width height)))) 
	
    (viewable-p ,graphic)))

(defmethod draw-graphic ((scene selection-scene) (view view)
                           &optional min-x min-y width height)
  (DECLARE (IGNORE  min-x min-y width height))
  (with-slots (elements extent) scene
    (WHEN (viewable-p scene) 
	(graphic-world-transform scene)	; Cache our transform
	(DOTIMES (position (length elements))
	  (draw-graphic (ELT elements position) view )
	  ))))

(DEFUN   make-selection-scene ( &rest options )
   (APPLY #'MAKE-INSTANCE 'selection-scene
			     :sensitivity :subselectable
			     options))

(defclass grabber (filled-rectangle)
  ((name 
    :type	(OR null symbol)
    :initarg	:grabber-name
    :accessor	grabber-name
    :documentation "the position name of the grabber in the the grabber-rect")

   (opposing       
    :initform	nil
    :accessor	opposing-grabber
    :documentation "the grabber that is opposite of this instance of grabber")
  
    )
  (:documentation
   "This is special class of filled rectangle to represent the grabbers
    of the extent rectangle"))

(defmethod scene-insert  ((scene selection-scene) graphic &optional pos )
 (DECLARE (IGNORE pos))
  (with-slots (elements parent) scene                           ;to a scene
 	 (VECTOR-PUSH-EXTEND graphic elements 1) ;insert after last graphic
	 )			; Set its new parent
 graphic)

(DEFUN  normalize-transform (transform)
  (WHEN transform
     (with-slots (t11 t12 t21 t22 t31 t32) transform
       (SETF
	 t11 1s0
	 t12 0s0
	 t21 0s0
	 t22 1s0
	 t31 0s0
	 t32 0s0))))

(defmethod draw-graphic ((rectangle grabber) (view view)
			 &optional min-x min-y width height)
  (declare (IGNORE  min-x min-y width height))
  (WHEN (viewable-p rectangle)
    (LET ((grabber-rect-transform (grabber-rect-transform view)))
      (with-slots (vertices transform) rectangle    
	(with-vector temp-vertices 
	  (copy-to-vector vertices temp-vertices)
	  (normalize-transform transform)
	  (scale-transform transform (t11 grabber-rect-transform)
			   (t22 grabber-rect-transform )
			   (rectangle-origin-x rectangle)
			   (rectangle-origin-y rectangle) )
	  (transform-point-seq transform temp-vertices)
	  (view-draw-filled-polygon view	; No, use draw-polygon to draw it
				    temp-vertices
				    (graphic-gstate rectangle)))))))

(defclass grabber-rect (scene)
  ((graphic
    :type   (OR null graphic)
    :initform nil
    :accessor grabber-graphic
    :documentation "the graphic the grabber-rect is attached to")
  (north-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor north-grabber
   :documentation "the north grabber of the grabber-rect")
  
  (south-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor south-grabber
   :documentation "the south grabber of the grabber-rect")

  (east-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor east-grabber
   :documentation "the east grabber of the grabber-rect")

  (west-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor west-grabber
   :documentation "the west grabber of the grabber-rect")

  (northwest-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor northwest-grabber
   :documentation "the northwest grabber of the grabber-rect")

  (northeast-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor northeast-grabber
   :documentation "the northeast grabber of the grabber-rect")

  (southwest-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor southwest-grabber
   :documentation "the southwest grabber of the grabber-rect")

  (southeast-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor southeast-grabber
   :documentation "the southeast grabber of the grabber-rect")

  (background-grabber
   :type   (OR null grabber)
   :initform nil
   :accessor background-grabber
   :documentation "the background grabber of the grabber-rect")
   )
  (:documentation "The graphic class for representing the grabber rectangle"))

(defmethod draw-graphic ((scene grabber-rect) (view view)
                           &optional min-x min-y width height)
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (elements extent) scene
    (WHEN (viewable-p scene) 
	(graphic-world-transform scene)	; Cache our transform
	(DOTIMES (position (length elements))
	  (draw-graphic (ELT elements position) view min-x min-y width height)
	  ))))


(defclass background-grabber ( grabber)
  ()
  (:documentation "This is special  class of rectangle to represent the background grabber"))

;Define a graphic-contains-p method for the background-grabber that will use the graphic-contains-p 
;method of the the selected graphic

(defmethod graphic-contains-p ((grabber background-grabber) x y &optional pixel)
  (declare (type wcoord x y ))
  (graphic-contains-p (grabber-graphic (graphic-parent grabber)) x y pixel))

(DEFMETHOD graphic-contains-p ((filled-polygon grabber) x y &optional pixel)
  (DECLARE (IGNORE pixel))
  (DECLARE (type wcoord x y))

;  (WHEN (point-in-extents-p filled-polygon x y)
  (with-slots (vertices) filled-polygon
    (with-vector 
     temp-vertices
     (copy-to-vector vertices temp-vertices)
     (transform-point-seq (graphic-world-transform filled-polygon) temp-vertices)

     (WHEN 
	 (inside-p temp-vertices (make-point :x x :y y)) t ))
    )
  )

(defun make-background-grabber (name x y width height transform)
  (declare (type wcoord x y ))
  (FUNCALL  #'make-instance 'background-grabber
	 :allow-other-keys t
	 :grabber-name name
	 :vertices (complete-rectangle x y (+ x width) (+ y height))
	 :transform transform
	 ))
    

(DEFMETHOD initialize-instance :after ((background-grabber background-grabber)
				       &key vertices)
    (SETF (vertices background-grabber) vertices))


(defun make-grabber (name x y width height transform
		     &rest options
		     &key &allow-other-keys)
  (declare (type wcoord x y ))
  (apply #'make-instance 'grabber
	 :allow-other-keys t
	 :grabber-name name
	 :vertices (complete-rectangle x y (+ x width) (+ y height))
	 :transform transform
	 :gstate (make-gstate :function boole-xor)
	 options))


(DEFMETHOD initialize-instance :after ((grabber grabber) &key vertices)
    (SETF (vertices grabber) vertices))

(DEFMETHOD (SETF grabber-graphic) (agraphic (grabber-rect grabber-rect))
  (with-slots (graphic transform extent sensitivity) grabber-rect
    (SETF graphic agraphic)
     (IF agraphic
	(PROGN
	  (extent-compute grabber-rect)
	  (SETF sensitivity :subselectable))
	(SETF sensitivity :hidden)
	  )))

(DEFMACRO scale-value (fixed-p p1 p2)
  `(IF (EQ ,fixed-p ,p1)
       0.000001
       (/ (- ,fixed-p ,p2)
	  (- ,fixed-p ,p1))))


(DEFMACRO grabber-corner-p (grabber-name)
  `(OR (EQ ,grabber-name 'northwest-grabber)
	      (EQ ,grabber-name 'southwest-grabber)
	      (EQ ,grabber-name 'northeast-grabber)
	      (EQ ,grabber-name 'southeast-grabber)))

(DEFMACRO grabber-north-south-p (grabber-name)
  `(OR (EQ ,grabber-name 'north-grabber)
	      (EQ ,grabber-name 'south-grabber)))

(DEFMACRO grabber-east-west-p (grabber-name)
  `(OR (EQ ,grabber-name 'east-grabber)
	      (EQ ,grabber-name 'west-grabber)))


(defun make-grabber-rect (view &key (highlight 1) parent  )
  (LET* ((size (grabber-size (view-selection-scene view))))
	 (FUNCALL
	  #'MAKE-INSTANCE 'grabber-rect
	  :sensitivity :subselectable
	  :gstate (make-gstate :foreground highlight :function boole-xor )	;the function is set for Xor
	  :size size
	  :parent parent
	  :view view
	  )))

(DEFMACRO grabber-transform ()
  `(copy-transform transform (make-transform)))

(DEFMETHOD initialize-instance
	   :after ((grabber-rect grabber-rect) &key size view)
  (LET* ((sign-x (SIGNUM (view-scale-x view)))
	(sign-y (SIGNUM (view-scale-y view)))
	(negsize (- size))
	(width (* sign-x size))
	(negwidth (* sign-x negsize))
	(heigth (* sign-y size))
	(negheigth (* sign-y negsize))
	(transform (grabber-rect-transform view)))
    
    (with-slots (north-grabber northwest-grabber northeast-grabber
		 south-grabber southwest-grabber southeast-grabber
		 east-grabber west-grabber center-grabber background-grabber)
      grabber-rect
      
      (scene-insert
       grabber-rect
       (SETF background-grabber
	     (MAKE-background-GRABBER
	      'background-grabber 0 0 0 0 (grabber-transform) )))
      (scene-insert
       grabber-rect
       (SETF  northwest-grabber
	      (MAKE-GRABBER
	       'northwest-grabber 0 0 width negheigth (grabber-transform) )))
      (scene-insert
       grabber-rect
       (SETF north-grabber
	     (MAKE-GRABBER
	      'north-grabber 0 0 width negheigth (grabber-transform) )))
      (scene-insert
       grabber-rect
       (SETF west-grabber
	     (MAKE-GRABBER 
	      'west-grabber 0 0 width heigth (grabber-transform) )))
      (scene-insert
       grabber-rect
       (SETF east-grabber
	     (MAKE-GRABBER
	      'east-grabber 0 0 negwidth heigth (grabber-transform)) ))
      (scene-insert
       grabber-rect
       (SETF south-grabber
	     (MAKE-GRABBER
	      'south-grabber 0 0 width heigth (grabber-transform) )))
      (scene-insert
       grabber-rect
       (SETF southwest-grabber
	     (MAKE-GRABBER
	      'southwest-grabber 0 0 width heigth (grabber-transform) )))
      (scene-insert
       grabber-rect
       (SETF southeast-grabber
	     (MAKE-GRABBER
	      'southeast-grabber 0 0 negwidth heigth (grabber-transform) )))
      (scene-insert
       grabber-rect
       (SETF northeast-grabber
	     (MAKE-GRABBER 
	      'northeast-grabber 0 0 negwidth negheigth (grabber-transform))))
      
      
      (SETF (opposing-grabber northwest-grabber)  southeast-grabber
	    (opposing-grabber northeast-grabber) southwest-grabber
	    (opposing-grabber north-grabber) south-grabber
	    (opposing-grabber south-grabber) north-grabber
	    (opposing-grabber east-grabber) west-grabber
	    (opposing-grabber west-grabber) east-grabber
	    (opposing-grabber southeast-grabber) northwest-grabber
	    (opposing-grabber southwest-grabber) northeast-grabber
	    (opposing-grabber background-grabber) northeast-grabber))
    
    grabber-rect
    ))

(DEFMACRO get-motion-notify-event (grabber-name fixed-x fixed-y px py)
  `(COND
      
    ((grabber-corner-p ,grabber-name)
      (LIST 'scale-rubberband ,fixed-x  ,fixed-y))
    
    ((grabber-north-south-p ,grabber-name)
      (LIST 'scale-rubberband-fixed-width  ,fixed-x  ,fixed-y  ,px))
    
    ((grabber-east-west-p ,grabber-name)
     (LIST 'scale-rubberband-fixed-height  ,fixed-x ,fixed-y ,py))

    ((EQ ,grabber-name 'background-grabber)
      '(move-box))))

(DEFMACRO grabber-x (grabber-name grabber view)
  `(COND
    ((grabber-north-south-p ,grabber-name)
     (transform-x ,view
		  (rectangle-origin-x (east-grabber (graphic-parent ,grabber)))) )
    ((EQ 'background-grabber grabber-name)
     (transform-x ,view (rectangle-origin-x ,grabber)))
    (t (transform-x ,view (rectangle-origin-x ,grabber)))))

(DEFMACRO grabber-y (grabber-name grabber view)
  `(COND
    ((grabber-east-west-p ,grabber-name)
     (transform-y ,view
		  (rectangle-origin-y (north-grabber (graphic-parent ,grabber)))))
    ((EQ 'background-grabber grabber-name)
     (transform-y ,view (rectangle-origin-y ,grabber)))
    (t (transform-y ,view (rectangle-origin-y   ,grabber)))))



	  
(DEFMETHOD view-transform-graphic ((grabber grabber) (view view)
				   &key (event :button-1))
  (LET* ((pixel-size (view-pixel-size view))
	 (grabber-name (grabber-name grabber))
	 (opposing (opposing-grabber grabber))
	 (fixed-x  (fixed-x opposing view ))
	 (fixed-y  (fixed-y opposing view))
	 (px1 (grabber-x grabber-name grabber view))
	 (py1 (grabber-y grabber-name grabber view))
	 (highlight-color (view-highlight-color view))
	 (selection-elements (scene-elements (view-selection-scene view)))
	 )
    (when (editable-p (grabber-graphic (graphic-parent grabber)))
      (with-event (x y display)
	(LET* ((*px* px1)                   (*py*  py1)
	       (*fixed-x* fixed-x)          (*fixed-y* fixed-y)
	       (*delta-fx* (- fixed-x x))   (*delta-fy* (-  fixed-y y))
	       (*delta-px* (- x *px*))      (*delta-py* (- y *py*)) )
	  (DECLARE (SPECIAL *px* *py* *fixed-x* *fixed-y*
			    *delta-fx* *delta-fy* *delta-px* *delta-py*
			    *transform*))
	  (with-event-mode
	   (view (LIST `(:motion-notify ,event)
		       (get-motion-notify-event
			grabber-name fixed-x fixed-y px1 py1))
		 `((:button-release ,event) (view-button-release t)))
	   (WHEN (EQL (FIRST (get-motion-notify-event
			      grabber-name fixed-x fixed-y px1 py1))
		      'move-box)
	      (WHEN (= *px* *fixed-x*)
		(SETF *px* (FLOOR (+ *px* pixel-size))
		      *delta-px* (FLOOR (+ *delta-px* pixel-size))))
	      (WHEN (= *py* *fixed-y*)
		(SETF *py* (FLOOR (+ *py* pixel-size))
		      *delta-py* (FLOOR (+ *delta-py* pixel-size))))
	      )
	    
	    (process-motion-notify-events
	     view display *fixed-x* *fixed-y* *px* *py* highlight-color)
	    (erase-grabber-rects
	     view  selection-elements (FILL-POINTER  selection-elements))
	    (repair-view view)
	    
	    (if (EQ grabber-name 'background-grabber)
		(move-selected-graphics
		 view selection-elements (FILL-POINTER  selection-elements)
		 (- (view-untransform-x view *fixed-x*)
		    (view-untransform-x view fixed-x ))
		 (-  (view-untransform-y view *fixed-y*)
		     (view-untransform-y view fixed-y)))
		
		(scale-selected-graphics
		  view selection-elements (FILL-POINTER  selection-elements)
		  (scale-value *fixed-x* px1 *px*)
		  (scale-value *fixed-y* py1 *py*)
		  (grabber-name opposing))
		)
	    )))
      (repair-view view))))

	  
(DEFMETHOD view-move-graphic ((grabber grabber) (view view)
			      &key (event :button-1))
  (LET* ((pixel-size (view-pixel-size view))
	 (grabber-name (grabber-name grabber))
	 (opposing (opposing-grabber grabber))
	 (fixed-x  (fixed-x opposing view ))
	 (fixed-y  (fixed-y opposing view))
	 (px1 (grabber-x grabber-name grabber view))
	 (py1 (grabber-y grabber-name grabber view))
	 (highlight-color (view-highlight-color view))
	 (selection-elements (scene-elements (view-selection-scene view)))
	 )
  
  (with-event (x y display)
    (LET* ((*px* px1)                 (*py* py1)
	   (*fixed-x* fixed-x)        (*fixed-y* fixed-y)
	   (*delta-fx* (- fixed-x x)) (*delta-fy* (- fixed-y y))
	   (*delta-px* (- x *px*))    (*delta-py* (- y *py*)))
      
      (DECLARE (SPECIAL *px* *py* *fixed-x* *fixed-y*
			*delta-fx* *delta-fy* *delta-px*
			*delta-py* *transform*))
      (WHEN (= *px* *fixed-x*)
	(SETF *px* (FLOOR (+ *px* pixel-size))
	      *delta-px* (FLOOR (+ *delta-px* pixel-size))))
      (WHEN (= *py* *fixed-y*)
	(SETF *py* (FLOOR (+ *py* pixel-size))
	      *delta-py* (FLOOR (+ *delta-py* pixel-size))))
      (with-event-mode
       (view `((:motion-notify ,event) (move-box))
	     `((:button-release ,event) (view-button-release t)))
       (process-motion-notify-events
	view display *fixed-x* *fixed-y* *px* *py* highlight-color)
       (erase-grabber-rects
	view  selection-elements (FILL-POINTER  selection-elements))
       (repair-view view)
	
       (move-selected-graphics
	view selection-elements (FILL-POINTER  selection-elements)
	(- (view-untransform-x view *fixed-x*)
	   (view-untransform-x view fixed-x ))
	(-  (view-untransform-y view *fixed-y*)
	    (view-untransform-y view fixed-y))))))
    (repair-view view)))


	  
(DEFMETHOD view-scale-graphic ((grabber grabber) (view view)
			       &key uniform (event :button-1))
  (LET* (
	 (grabber-name (grabber-name grabber))
	 (opposing (opposing-grabber grabber))
	 (fixed-x  (fixed-x opposing view ))
	 (fixed-y  (fixed-y opposing view))
	 (px1 (grabber-x grabber-name grabber view))
	 (py1 (grabber-y grabber-name grabber view))
	 (highlight-color (view-highlight-color view))
	 (selection-elements (scene-elements (view-selection-scene view)))
	 )
    (when (editable-p (grabber-graphic (graphic-parent grabber)))
      (with-event (x y display)
	(LET* ((*px* px1)                (*py* py1)
	       (*fixed-x* fixed-x)       (*fixed-y* fixed-y)
	       (*delta-fx* (- fixed-x x))(*delta-fy* (-  fixed-y y))
	       (*delta-px* (- x *px*))   (*delta-py* (- y *py*)))
	  
	  (DECLARE (SPECIAL *px* *py* *fixed-x* *fixed-y* *delta-fx*
			    *delta-fy* *delta-px* *delta-py* *transform*))
	  (with-event-mode 
	   (view
	    (LIST `(:motion-notify ,event)
		  (get-motion-notify-event
		   grabber-name fixed-x fixed-y px1 py1))
	    `((:button-release ,event) (view-button-release t)))
	    (UNLESS (EQ grabber-name 'background-grabber)
	      (process-motion-notify-events
	       view display *fixed-x* *fixed-y* *px* *py* highlight-color))
	    (erase-grabber-rects
	     view  selection-elements (FILL-POINTER  selection-elements))
	    (repair-view view)	  
	    (scale-selected-graphics
	      view selection-elements (FILL-POINTER  selection-elements)
	      (scale-value *fixed-x* px1 *px*)
	      (IF uniform
		  (scale-value *fixed-x* px1 *px*)
		  (scale-value *fixed-y* py1 *py*))
	      (grabber-name opposing))
	    )))))
  (repair-view view))



(DEFUN  fixed-x (opposing view)
  (LET ((grabber-class-name (grabber-name opposing))
	(parent (graphic-parent opposing)))
    (COND
      ((grabber-corner-p grabber-class-name)
       (transform-x view (rectangle-origin-x opposing)))
      ((EQ 'west-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (west-grabber parent))))
      ((EQ 'east-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (east-grabber  parent))))
      ((EQ 'north-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (west-grabber  parent))))
      ((EQ 'south-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (west-grabber  parent)))))))

(DEFUN fixed-y (opposing view)
  (LET ((grabber-class-name (grabber-name opposing))
	(parent (graphic-parent opposing)))
    (COND
      ((grabber-corner-p grabber-class-name)
       (transform-y view (rectangle-origin-y opposing)))
      ((EQ 'west-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (southwest-grabber  parent))))
      ((EQ 'east-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (southeast-grabber  parent))))
      ((EQ 'north-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (north-grabber  parent))))
      ((EQ 'south-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (south-grabber  parent)))))))


			    
(DEFUN erase-grabber-rects ( view selection-elements selection-length )
  (DOTIMES (pos  SELECTION-LENGTH)	
    (LET* ((highlight (ELT selection-elements pos))
	   (graphic (grabber-graphic highlight)))
      ;Is the grabber-rect being used?
      (UNLESS (EQ (graphic-sensitivity highlight) :hidden)
	(graphic-damage graphic)
	(draw-graphic highlight view))))
  (repair-view view)
  )

(DEFUN scale-selected-graphics (view selection-elements selection-length
				     x-scale y-scale fixed-point)
  (DECLARE (IGNORE view))
  (DOTIMES (pos  SELECTION-LENGTH)			
    (LET* ((highlight (ELT selection-elements pos))
	   (graphic (grabber-graphic highlight)) )
      ;Is the grabber-rect being used?
      (UNLESS (EQ (graphic-sensitivity highlight) :hidden)
	(MULTIPLE-VALUE-BIND (fx fy)
			     (graphic-fixed-point graphic fixed-point)
	  (scale-transform graphic x-scale y-scale fx fy)  
	  (graphic-extent graphic)
	  (extent-compute highlight ))))))
 
(DEFUN move-selected-graphics (view selection-elements selection-length
				    delta-x delta-y)
  (DECLARE (IGNORE view))  
  (DOTIMES (pos  SELECTION-LENGTH) ;Move the graphics in the scene
    (LET* ((highlight (ELT selection-elements pos))
	   (graphic (grabber-graphic highlight)) )
      (WHEN (EQ (graphic-sensitivity highlight) :hidden) (RETURN))
      (WHEN graphic ;Is the graphic attached to a grabber rectangle.
	(move-transform graphic ;  Yes, move the graphic.
			delta-x delta-y)
	(graphic-extent graphic)
	(extent-compute highlight )))))



(DEFMETHOD view-rotate-graphic ((grabber grabber) (view view)
				&key (event :button-3))
  (UNLESS (OR (grabber-north-south-p (grabber-name grabber))
	      (grabber-east-west-p (grabber-name grabber)))
    (LET* (
	   (selection-elements (scene-elements (view-selection-scene view)))
	   (highlight-color (view-highlight-color view))
	   (opposing (opposing-grabber grabber))
	   (fixed-x  (fixed-x opposing view ))
	   (fixed-y  (fixed-y opposing view))
	   (px1  (transform-x view (rectangle-origin-x grabber)))
	   (py1  (transform-y view (rectangle-origin-y grabber))))
      
      (when (editable-p (grabber-graphic (graphic-parent grabber)))
	(UNWIND-PROTECT
	    (progn
	      (grab-pointer view #.(make-event-mask :button-release)
			    :owner-p t)
	      (with-event (x y display)
		(LET ((*px* px1) (*py*  py1)
		      (*fixed-x* fixed-x)(*fixed-y* fixed-y)
		      (*transform* (make-transform)))
		  
		  (DECLARE (SPECIAL *px* *py* *fixed-x* *fixed-y*
				    *delta-fx* *delta-fy* *delta-px*
				    *delta-py* *transform*))
		  
		  (with-special-vector *rotate-vector*
		    (SETF (display-after-function display) #'display-force-output)
		    
		    (SETF (FILL-POINTER *rotate-vector*) 10)
		    (SETF (ELT *rotate-vector* 0) *fixed-x*
			  (ELT *rotate-vector* 1) *fixed-y*
			  (ELT *rotate-vector* 2) *px*
			  (ELT *rotate-vector* 3) *fixed-Y*
			  (ELT *rotate-vector* 4) *px*
			  (ELT *rotate-vector* 5) *py*
			  (ELT *rotate-vector* 6) *fixed-x*
			  (ELT *rotate-vector* 7) *py*
			  (ELT *rotate-vector* 8) *fixed-x*
			  (ELT *rotate-vector* 9) *fixed-y*)
		    (with-event-mode 
		     (view `((:motion-notify ,event) (rotate-box))
			   `((:button-release ,event) (view-button-release t)))
		      (drawlines-with-gc view highlight-color *rotate-vector* )   
		      (CATCH :release
			(LOOP
			  (process-next-event display )))
		      (transform-point-seq *transform* *rotate-vector*)
		      (round-vector *rotate-vector*)
		      (drawlines-with-gc view highlight-color *rotate-vector*)
		      (SETF (display-after-function display) nil)
		      (erase-grabber-rects
		       view  selection-elements (FILL-POINTER  selection-elements))
		      (repair-view view)
		      ))
		  (rotate-selected-graphics
		    view selection-elements (FILL-POINTER  selection-elements)
		    (rotation
		     (view-untransform-x view *fixed-x*)
		     (view-untransform-y view *fixed-y*)
		     (rectangle-origin-x grabber)
		     (rectangle-origin-y grabber)
		     (view-untransform-x view *px*)
		     (view-untransform-y view *py*))
		    (grabber-name opposing)))))
	  (ungrab-pointer (contact-display view))))
      
      (repair-view view))))
 
(DEFUN rotation (fixed-x fixed-y x y x2 y2)
  "Determine the difference in the angle between FIXED-X:FIXED-Y
   X1:Y1 and FIXED-X:FIXED-Y X2:Y2."
		     
  (- (IF (= x2 fixed-x)
	    (radians 0)
	    (+ (ATAN (/ (-   y2 fixed-y) (-  x2 fixed-x )))
	       (IF (< (* (SIGNUM (-  x fixed-x ))(-  x2 fixed-x )) 0)
		   (radians 180) 0)))
	(IF (= fixed-x x)
	    0
	    (ATAN (/ (-  y fixed-y ) (-  x fixed-x ))))))	
		     

(DEFUN rotate-selected-graphics (view selection-elements selection-length
				      rotation fixed-point)
  (DECLARE (IGNORE view))
  (DOTIMES (pos  SELECTION-LENGTH) ;Rotate the graphics in the scene
	    (LET* ((highlight (ELT selection-elements pos))
		   (graphic (grabber-graphic highlight)) )
	      (WHEN (EQ (graphic-sensitivity highlight) :hidden) (RETURN))
	      (WHEN graphic ;Is the graphic attached to a grabber rectangle?
		(MULTIPLE-VALUE-BIND (fx fy)
		  (graphic-fixed-point graphic fixed-point)
		  (rotate-transform graphic ;  Yes, rotate the graphic.
				    rotation fx fy)

		(extent-compute highlight )
		)))))


;Method: extent-compute
;  Compute the extent rectangle for the GRAPHIC.  The method should be
;  defined for each derived graphic class.  The primary method returns nil,
;  meaning "undefined extent."

;  Note: A graphic's extent rectangle is defined in the local coordinate
;  system.  This means that each graphic should apply its own transform to
;  its computed extent before returning it.  To obtain the extent of a
;  graphic in the world coordinate system, call the world-extent method
;  (defined below).

(defmethod extent-compute ((grabber-rect grabber-rect))
  
  (with-slots (graphic  extent) grabber-rect
    (WHEN  graphic
      (LET* ((graphic-extent (world-extent graphic))
	    (view (FIRST (graphic-views graphic)))
	    (size (* -1 (/ (grabber-size (view-selection-scene view))
			   (view-scale view) 2.0))))
	(extent-copy graphic-extent extent)
	(LET* ((xmax (extent-rect-xmax extent))
	       (xmin (extent-rect-xmin extent))
	       (ymin (extent-rect-ymin extent))
	       (ymax (extent-rect-ymax extent)))
	  (DOLIST (grab '(northwest-grabber north-grabber east-grabber
			  northeast-grabber southwest-grabber south-grabber
			  west-grabber southeast-grabber))
		  (SETF (graphic-sensitivity (FUNCALL grab grabber-rect))
			:editable))
	  (when (= xmax xmin)
	    (SETF xmax (+ xmin (view-pixel-size view)))
	    (DOLIST (grab '(north-grabber east-grabber northeast-grabber
			    southwest-grabber west-grabber south-grabber))
	      (SETF (graphic-sensitivity (FUNCALL grab grabber-rect)) :hidden)))
	  (when (= ymax ymin)
	    (SETF ymax (+ ymin (view-pixel-size view)))
	    (DOLIST (grab '(north-grabber east-grabber northeast-grabber
			    southwest-grabber west-grabber south-grabber))
	      (SETF (graphic-sensitivity (FUNCALL grab grabber-rect)) :hidden)))
	  (LET* ((width (/ (- xmax xmin) 2.0))
		 (height (/ (- ymax ymin) 2.0)))
	    (SETF (rectangle-origin-x (northwest-grabber grabber-rect)) xmin)
	    (SETF (rectangle-origin-y (northwest-grabber grabber-rect)) ymax)
	    (SETF (rectangle-origin-x (southwest-grabber grabber-rect)) xmin)
	    (SETF (rectangle-origin-y (southwest-grabber grabber-rect)) ymin)
	    (SETF (rectangle-origin-x (northeast-grabber grabber-rect)) xmax)
	    (SETF (rectangle-origin-y (northeast-grabber grabber-rect)) ymax)
	    (SETF (rectangle-origin-x (southeast-grabber grabber-rect)) xmax)
	    (SETF (rectangle-origin-y (southeast-grabber grabber-rect)) ymin)
	    
	    (SETF (rectangle-origin-x (north-grabber grabber-rect))
		  (+ xmin width size))
	    (SETF (rectangle-origin-y (north-grabber grabber-rect)) ymax)
	    (SETF (rectangle-origin-x (south-grabber grabber-rect))
		  (+ xmin width size))
	    (SETF (rectangle-origin-y (south-grabber grabber-rect)) ymin)
	    (SETF (rectangle-origin-x (east-grabber grabber-rect)) xmax)
	    (SETF (rectangle-origin-y (east-grabber grabber-rect))
		  (+ ymin height size))
	    (SETF (rectangle-origin-x (west-grabber grabber-rect)) xmin)
	    (SETF (rectangle-origin-y (west-grabber grabber-rect))
		  (+ ymin height size))
	    
	    (SETF (rectangle-origin-x (background-grabber grabber-rect)) xmin)
	    (SETF (rectangle-origin-y (background-grabber grabber-rect)) ymin)
	    
	    ))))
    extent))



(defmethod extent-compute ((grabber grabber))
  (with-slots (vertices) grabber
    (LET  ((x-min (rectangle-origin-x grabber))
	   (y-min (rectangle-origin-y grabber)))
      (multiple-value-setq (x-min y-min )
	(transform-point (graphic-transform grabber) x-min y-min ))
      (SETF (rectangle-origin-x grabber) x-min)
      (SETF (rectangle-origin-y grabber) y-min)
      (let ((xmin (point-seq-x-min vertices))
	    (ymin (point-seq-y-min vertices))
	    (xmax (point-seq-x-max vertices))
	    (ymax (point-seq-y-max vertices)))
	(with-coercion ((xmin ymin xmax ymax) extent-element)
	  (make-extent-rect
	   :xmin xmin
	   :xmax xmax
	   :ymin ymin
	   :ymax ymax
	   :valid t))))))
