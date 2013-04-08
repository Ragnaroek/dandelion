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


(EXPORT '(
	  graphic-image-content
	  make-graphic-image
	  graphic-image-base-x
	  graphic-image-base-y
	  graphic-image-gravity
	  graphic-image-tile-p
	  )
	'pictures)


(DEFMACRO compute-base-origin-if-not-set (base-x base-y)
  `(UNLESS (AND ,base-x ,base-y)
     (MULTIPLE-VALUE-BIND (x y) (compute-base-origin graphic-image view)
       (SETF ,base-x x)
       (SETF ,base-y y))))


(defclass graphic-image (extent-cache graphic)
  ((image-content
    :type	(OR null :bitmap :xy-pixmap :z-pixmap)
    :initarg	:image-content
    :initform	nil
    :accessor	graphic-image-content
    :documentation
    "the numeric values for the graphic-image")
   
   (graphic-image-base-x
    :type	(OR wcoord null)
    :initarg	:base-x
    :initform	nil
    :accessor	graphic-image-base-x
    :documentation
    "object x-coordinate of base point for the image")

   (graphic-image-base-y
    :type	(OR wcoord null)
    :initarg	:base-y
    :initform	nil
    :accessor	graphic-image-base-y
    :documentation
    "object y-coordinate of base point for the image")

   (graphic-image-extent-x
    :type	(OR wcoord null)
    :initarg   :extent-x
    :initform	nil
    :accessor	graphic-image-extent-x
    :documentation 
    "object x-coordinate of the user defined extent for the image")

   (graphic-image-extent-y
    :type	(OR wcoord null)
    :initarg   :extent-y
    :initform	nil
    :accessor	graphic-image-extent-y
    :documentation
    "object y-coordinate of the user defined extent for the image")

   (graphic-image-extent-width
    :type	(OR wcoord null)
    :initarg   :extent-width
    :initform	nil
    :accessor	graphic-image-extent-width
    :documentation
    "object width  of the user defined extent for the image")

   (graphic-image-extent-height
    :type	(OR wcoord null)
    :initarg   :extent-height
    :initform	nil
    :accessor	graphic-image-extent-height
    :documentation
    "object height  of the user defined extent for the image")

   (graphic-image-gravity
    :type	symbol
    :accessor	graphic-image-gravity
    :initarg	:gravity
    :initform	:southwest
    :documentation
    "the gravity of the graphic-image within a frame")

   (graphic-image-tile-p
    :type	boolean
    :accessor	graphic-image-tile-p
    :initarg	:tile-p
    :initform	nil
    :documentation
    "a flag to signal if the graphic image is to tile when drawn")
 
   (graphic-image-scale
    :type	number
    :accessor	graphic-image-scale
    :initform	1
    :documentation
    "the scale factor at which the image was formed")
   )
  (:documentation
 "The graphic class for drawing a graphic-image in object coordinates"))


(DEFUN make-graphic-image (image &key
				 tile-p base-x base-y extent-x extent-y
				 extent-width extent-height
				 (gravity :southwest) gstate transform
				 parent (sensitivity :editable) plist)

  
  (FUNCALL #'MAKE-INSTANCE 'graphic-image
	   :allow-other-keys t
	   :base-x base-x
	   :base-y base-y
	   :image-content image
	   :extent-x extent-x
	   :extent-y extent-y
	   :extent-width extent-width
	   :extent-height extent-height
	   :gravity gravity
	   :gstate gstate
	   :parent parent
	   :transform transform
	   :sensitivity sensitivity
	   :tile-p tile-p
	   :plist plist
	   ))

(DEFMETHOD initialize-instance :after ((graphic-image graphic-image)
				       &key extent-x extent-y )
  (with-slots (gstate graphic-image-base-x graphic-image-base-y views
		      extent extent-valid-p) graphic-image
    (SETF (gstate-fill-style graphic-image) :tiled)
    (UNLESS (OR (AND graphic-image-base-x graphic-image-base-y)
		(AND extent-x extent-y))
      (SETF graphic-image-base-x 0)
      (SETF graphic-image-base-y 0))
    
    ))


(DEFMETHOD (SETF graphic-image-base-x)
	   :before  (x (graphic-image graphic-image) )
  (with-slots
      (extent graphic-image-extent-x graphic-image-base-x) graphic-image
    (WHEN graphic-image-extent-x 
      (SETF graphic-image-extent-x (+ graphic-image-extent-x
				      (- x graphic-image-base-x)))))
  (extent-changed graphic-image))

(DEFMETHOD (SETF graphic-image-base-y)
	   :before ( y (graphic-image graphic-image))
  (with-slots
      (extent graphic-image-extent-y graphic-image-base-y) graphic-image
    (WHEN graphic-image-extent-y 
      (SETF graphic-image-extent-y (+ graphic-image-extent-y
				      (- y graphic-image-base-y)))))
  (extent-changed graphic-image))

(DEFMETHOD (SETF graphic-image-extent-x)
	   :before (x (graphic-image graphic-image))
  (DECLARE (IGNORE x))
  (extent-changed graphic-image)
  (with-slots (transform graphic-image-base-x) graphic-image
    (SETF transform nil)
    (SETF graphic-image-base-x nil)))

(DEFMETHOD (SETF graphic-image-extent-y)
	   :before (y (graphic-image graphic-image))
  (DECLARE (IGNORE y))
  (extent-changed graphic-image)
  (with-slots (transform graphic-image-base-y) graphic-image
    (SETF transform nil)
    (SETF graphic-image-base-y nil)))



(DEFMETHOD (SETF graphic-image-gravity)
	   :after (gravity (graphic-image graphic-image))
  (DECLARE (IGNORE gravity))
  (with-slots ( graphic-image-base-x graphic-image-base-y ) graphic-image
      (SETF graphic-image-base-x nil)
      (SETF graphic-image-base-y nil)))



(DEFMETHOD extent-compute ((graphic-image graphic-image))
  
  (LET ((scale-x (view-scale-x (graphic-view graphic-image)))
	(scale-y (view-scale-y (graphic-view graphic-image))))
    (with-slots (extent graphic-image-base-x graphic-image-base-y
			graphic-image-extent-x graphic-image-extent-y
			graphic-image-extent-width graphic-image-extent-height
			image-content transform) graphic-image
    
    (IF (AND graphic-image-extent-x 
	     graphic-image-extent-y
	     graphic-image-extent-width
	     graphic-image-extent-height)
	(MULTIPLE-VALUE-BIND
	    (x-min y-min)(transform-point transform
					  graphic-image-extent-x
					  graphic-image-extent-y)
	  (MULTIPLE-VALUE-BIND
	      (x-max y-max) (transform-point
			     transform
			     (+ x-min graphic-image-extent-width)
			     (+ y-min graphic-image-extent-height))
	    (with-coercion ((x-min y-min x-max y-max) extent-element)
	      (make-extent-rect
	       :xmin x-min
	       :ymin y-min
	       :xmax x-max
	       :ymax y-max
	       :valid t))))
	(MULTIPLE-VALUE-BIND
	    (x-min y-min) (transform-point transform
					   graphic-image-base-x 
					   graphic-image-base-y)
	  (let ((xmax (+ x-min (/ (image-width image-content) scale-x)))
		(ymax (+ y-min (/ (image-height image-content) scale-y))))
	    (with-coercion ((x-min y-min xmax ymax) extent-element)
	      (make-extent-rect
	       :xmin x-min
	       :ymin y-min
	       :xmax xmax
	       :ymax ymax
	       :valid t))))))))


(DEFMACRO get-tile-x (x)
  ` (transform-x view ,x))

(DEFMACRO get-tile-y (y)
  `(transform-y view ,y))

(DEFMETHOD draw-graphic ((graphic-image graphic-image) (view view)
                           &optional min-x min-y width height) 
  (DECLARE (type (OR null wcoord) min-x min-y width height))
  (with-slots (gstate extent  graphic-image-scale transform
	       graphic-image-base-x graphic-image-base-y
	       image-content (tile-p graphic-image-tile-p)) graphic-image
    (WHEN (visible-p graphic-image)
	  (compute-base-origin-if-not-set graphic-image-base-x
					  graphic-image-base-y)
	  (LET ((world-extent (extent-transform
			       (graphic-world-transform graphic-image) extent))
		(scale-x (view-scale-x view))
		(scale-y (view-scale-y view)))
	    (WHEN (NOT (= (view-scale view) graphic-image-scale))
	      (SETF graphic-image-scale (view-scale view)) 
	      (extent-changed graphic-image)
	      (graphic-extent graphic-image))
	    (IF tile-p
		(PROGN
		  (MULTIPLE-VALUE-BIND (tx ty)
				       (compute-base-origin graphic-image view)
		    (MULTIPLE-VALUE-SETQ (tx ty) 
		      (transform-point (graphic-world-transform graphic-image)
				       tx ty))
		    (SETF (gstate-value gstate :ts-x) (get-tile-x tx))
		    (SETF (gstate-value gstate :ts-y) (get-tile-y ty)))
		  (view-draw-image
		   view
		   (extent-rect-xmin world-extent)
		   (extent-rect-ymin world-extent)
		   (extent-rect-xmax world-extent)
		   (extent-rect-ymax world-extent) 
		   image-content  gstate))
		
		(MULTIPLE-VALUE-BIND
		    (x y) (transform-point
			   (graphic-world-transform  graphic-image)
			   graphic-image-base-x
			   graphic-image-base-y)
		  (MULTIPLE-VALUE-BIND 
		      (x-max y-max)
		      (transform-point 
		       (graphic-world-transform graphic-image)
		       (+ graphic-image-base-x
			  (/ (image-width image-content) scale-x))
		       (+ graphic-image-base-y
			  (/ (image-height image-content) scale-y)))
		    (SETF (gstate-value gstate :ts-x) (get-tile-x x))
		    (SETF (gstate-value gstate :ts-y) (get-tile-y y))
		    (view-draw-image view
				     x  y
				     x-max  y-max
				     image-content  gstate))))))))


(DEFMETHOD normalize-graphic
	   :before ((graphic-image graphic-image))
  (with-slots (extent graphic-image-base-x graphic-image-base-y
	       graphic-image-extent-x graphic-image-extent-y
	       graphic-image-extent-width graphic-image-extent-height
	       image-content transform) graphic-image
    
    (IF (AND graphic-image-extent-x
	     graphic-image-extent-y
	     graphic-image-extent-width
	     graphic-image-extent-height)
	(MULTIPLE-VALUE-BIND
	    (x-min y-min) (transform-point transform 
					   graphic-image-extent-x
					   graphic-image-extent-y)
	  (MULTIPLE-VALUE-BIND
	      (x-max y-max)
	      (transform-point transform
			       (+ x-min graphic-image-extent-width)
			       (+ y-min graphic-image-extent-height))
	    (SETF graphic-image-extent-x x-min
		  graphic-image-extent-y y-min
		  graphic-image-extent-width (ABS (- x-max x-min))
		  graphic-image-extent-height (ABS (- y-max y-min)))))
	(MULTIPLE-VALUE-BIND
	    (x y) (transform-point transform 
				   graphic-image-base-x
				   graphic-image-base-y)
	  (SETF (graphic-image-base-x graphic-image) x
		(graphic-image-base-y graphic-image) y)))
    ))


(DEFUN compute-base-origin (graphic-image view)
  "Return the  baseline coordinate of the graphic-image"
  (with-slots (graphic-image-gravity
	       transform image-content extent graphic) graphic-image
    (LET ((world-extent
	   (IF (valid-extent-p extent)
	       extent
	       (graphic-extent graphic-image))))
      (with-extent-values
       world-extent extent-xmin extent-ymin extent-xmax
       extent-ymax extent-width extent-height
       (LET*  (
	       (scale-x (view-scale-x view))
	       (scale-y (view-scale-y view))
	       (width (/ (image-width image-content) scale-x))
	       (height (/ (image-height image-content) scale-y)))

	 (CASE graphic-image-gravity
			      
	   (:southwest  (VALUES  extent-xmin extent-ymin  ))
	   (:northwest  (VALUES  extent-xmin (- extent-ymax  height)))
	   (:south      (VALUES  (+ (- extent-xmin (/ width 2.0))
				    (/ extent-width 2.0) ) extent-ymin))
	   (:north      (VALUES  (+ (- extent-xmin (/ width 2.0))
				    (/ extent-width 2.0) )
				 (- extent-ymax  height)))
	   (:west       (VALUES  extent-xmin
				 (+ extent-ymin (- (/ extent-height 2.0)
						   (/ height 2.0)))))
	   (:center     (VALUES  (+ (- extent-xmin (/ width 2.0))
				    (/ extent-width 2.0) )
				 (+  extent-ymin (- (/ extent-height 2.0 )
						    (/ height 2.0)))))
	   (:southeast  (VALUES  (- extent-xmax  width) extent-ymin))
	   (:northeast  (VALUES  (- extent-xmax  width)
				 (- extent-ymax  height)))
	   (:east       (VALUES  (- extent-xmax  width)
				 (+ (- extent-ymin (/ height 2.0))
				    (/ extent-height 2.0 ))))
	   (t           (VALUES   extent-xmin extent-ymin))))))))

