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
	  make-label
	  label-base-x
	  label-base-y
	  label-extent-x
	  label-extent-y
	  label-extent-height
	  label-extent-width
	  label-gravity
	  label-font-size
	  label-font-family
	  label-extent-clip-p
	  label-angle
	  label-string
	  label-reverse-p
	  label-filled-background-p
	  label-font
	  label
	  )
	'pictures)


(DEFCONSTANT  minimum-pixel-size-to-display 4.0)

(DEFMACRO change-font-if-scale-values-not-equal ( label label-view-scale view)
  `(UNLESS (= ,label-view-scale (view-scale ,view))
     (SETF (label-font ,label) nil)
     (extent-changed ,label)
     (graphic-extent ,label)
     (SETF ,label-view-scale (view-scale ,view))))

(DEFMACRO compute-base-line-origin-if-not-set (base-x base-y scale-x scale-y)
  `(UNLESS (AND ,base-x ,base-y)
     (MULTIPLE-VALUE-BIND (x y) (compute-baseline-origin label ,scale-x ,scale-y)
       (SETF ,base-x x)
       (SETF ,base-y y))))



;Label Class Functional Definition:

;Function: make-label
(defclass label (extent-cache graphic)
  (
   (label-string
    :type	string 
    :initarg	:label-string
    :initform	(MAKE-STRING 10)
    :documentation  "the string to be displayed ")
   
   (label-base-x
    :type	(OR null ocoord)
    :accessor	label-base-x 
    :initarg	:base-x
    :initform	nil
    :documentation
  "object x-coordinate of lower left corner of the base-line of the label string")
   
   (label-base-y
    :type	(OR null ocoord)
    :accessor	label-base-y
    :initarg	:base-y
    :initform	nil
    :documentation
 "object y-coordinate of lower left corner of the base-line of the label string")
   
   (label-extent-x
    :type	(OR null ocoord)
    :accessor	label-extent-x
    :initarg	:extent-x
    :initform	nil
    :documentation
 "object x-coordinate of the lower left corner of the extent of the label")
   
   (label-extent-y
    :type	(OR null ocoord)
    :accessor	label-extent-y
    :initarg	:extent-y
    :initform	nil
    :documentation
 "object y-coordinate of the lower left corner of the extent of the label")
   
   
   (extent-height
    :type	(or null ocoord)
    :accessor	label-extent-height
    :initarg	:extent-height
    :initform	nil
    :documentation
 "the of the height of the extent of the label")
   
   (extent-width
    :type	(or null ocoord)
    :accessor	label-extent-width
    :initarg	:extent-width
    :initform	nil
    :documentation
 "width of the width of the extent fo the label")
   
   (label-gravity
     :type  symbol
     :accessor label-gravity
     :initarg :gravity
     :initform :southwest
     :documentation
 "the gravity of the label within a frame")
   
   (label-view-scale
    :type real
    :accessor label-view-scale
    :initform 1.0
    :documentation
 "the view scale factor, used to check and see if the view scale has changed")
   
   (label-font-size
    :type (OR null real)
    :accessor label-font-size
    :initarg :font-size
    :initform 10
    :documentation
 "the local coordinate size of a font, one point is on local coordinate unit")
   
   
   (label-font-family
    :type (OR null string font-family)
    :accessor label-font-family
    :initform nil
    :documentation "The  font family to be displayed")
   
   (label-font
    :type (OR null font string)
    :initform nil
    :documentation "The font that is to be displayed")
   (label-extent-clip-p
    :type boolean
    :initarg :extent-clip-p
    :initform nil
    :accessor label-extent-clip-p
    :documentation
 "Set true if the label is not to be displayed outside of the extents")

   (label-angle
    :type  number
    :accessor label-angle
    :initarg :angle
    :initform 0
    :documentation "the angle of the label in radians")
   )
  (:documentation "The graphic class for drawing points in object coordinates"))


(defun make-label
       (label-string
	&key
	base-x base-y extent-x extent-y extent-width extent-height
	font-family  (font-size 10) extent-clip-p
	filled-background-p reverse-p
	(gravity :southwest) gstate plist 
	(sensitivity :editable) (angle 0)
	view
	)
  (FUNCALL  #'make-instance 'label
	 :allow-other-keys t
	 :font-family font-family
	 :font-size font-size
	 :base-x base-x
	 :base-y base-y
	 :label-string label-string
	 :extent-x extent-x
	 :extent-y extent-y
	 :extent-clip-p extent-clip-p
	 :extent-width extent-width
	 :extent-height extent-height
	 :gravity gravity
	 :gstate gstate
	 :label-view view
	 :sensitivity sensitivity
	 :plist plist
	 :angle angle
	 :reverse-p reverse-p
	 :filled-background-p filled-background-p
	 ))

(DEFMETHOD  label-filled-background-p ((label label))
  "set or get the amount to pan in along the x axis"
  (GETF (slot-value label 'plist) :filled-background-p))

(DEFMETHOD (SETF label-filled-background-p)
	   ( filled-background-p (label label) )
  "set the relative pan motion  along the x axis"
  (SETF (GETF (slot-value label 'plist) :filled-background-p) filled-background-p))

(DEFMETHOD (SETF gstate-background) :after (background (graphic label))
  (if background
      (SETF (label-filled-background-p graphic) t)
      (SETF (label-filled-background-p graphic) nil)
    ))

(defmethod (setf gstate-value) :after (attribute  (graphic label) keyword)
  (WHEN (EQL  keyword :background)
    (IF attribute
      (SETF (label-filled-background-p graphic) t)
      (SETF (label-filled-background-p graphic) nil)
    )))

(DEFMETHOD  label-reverse-p ((label label))
  "set or get the amount to pan in along the x axis"
  (GETF (slot-value label 'plist) :reverse-p))

(DEFMETHOD  (SETF label-reverse-p) (   reverse-p (label label) )
  "set the relative pan motion  along the x axis"
  (SETF (GETF (slot-value label 'plist) :reverse-p) reverse-p))

(DEFMETHOD  label-angle-extent ((label label))
  "set or get the amount to pan in along the x axis"
  (GETF (slot-value label 'plist) :angle-extent))


(DEFMETHOD  (SETF label-angle-extent) (angle-extent (label label) )
  "set the relative pan motion  along the x axis"
  (SETF (GETF (slot-value label 'plist) :angle-extent) angle-extent))



(DEFMETHOD initialize-instance
	   :after ((label label)
		   &key label-view font-family filled-background-p reverse-p)
  (with-slots (gstate label-base-x label-base-y label-extent-x label-extent-y 
	       label-font-family
	       extent-width extent-height views extent extent-valid-p) label
    (UNLESS gstate  (SETF gstate (make-gstate)))	;create an empty gstate
    (IF (AND  label-extent-x label-extent-y)
	nil
	(UNLESS (AND label-base-x label-base-y) 
	  (SETF label-base-x 0
		label-base-y 0)))
    (WHEN (OR (gstate-background gstate) filled-background-p)
      (SETF (label-filled-background-p label) t))
    (WHEN reverse-p (SETF (label-reverse-p label) t))
    (WHEN label-view (SETF views (LIST label-view)))
    (SETF label-font-family font-family)
    (COND
      ((NOT font-family) (SETF label-font-family  default-family-name))
      ((font-p font-family) (SETF label-font-family font-family)))
    ))


(DEFMETHOD label-font ((label label) &optional change-extent-p)
  (DECLARE (IGNORE change-extent-p))
  (slot-value label 'label-font))

(DEFMETHOD (SETF label-font) (font (label label) &optional change-extent-p)
  (with-slots (label-font label-extent-x label-extent-y extent-width
	       extent-height) label
    (SETF label-font font)
    (WHEN change-extent-p
      (extent-changed label)
      (SETF label-extent-x nil
	    label-extent-y nil
	    extent-width nil
	    extent-height nil))))

(DEFMETHOD label-string ((label label) &optional change-extent-p)
  (DECLARE (IGNORE change-extent-p))
  (slot-value label 'label-string))

(DEFMETHOD (SETF label-string) (string (label label) &optional change-extent-p)
  (with-slots (label-string label-extent-x label-extent-y extent-width
                extent-height) label
    (SETF label-string string)
      (WHEN change-extent-p
	(extent-changed label)
	(SETF label-extent-x nil
	      label-extent-y nil
	      extent-width nil
	      extent-height nil)))
    )


(DEFMETHOD (SETF label-angle) :after (x (label label))
  (extent-changed label)
  (WHEN (EQUAL x 0) (SETF (label-angle-extent label) nil)))

(DEFMETHOD (SETF label-base-x) :before (x (label label) )
  (with-slots (extent label-extent-x label-base-x) label
    (WHEN label-extent-x 
      
      (SETF label-extent-x (+ label-extent-x (- x label-base-x))))
    )
  (extent-changed label))


(DEFMETHOD (SETF label-base-y) :before ( y (label label))
  (with-slots (extent label-extent-y label-base-y) label
    (WHEN label-extent-y 
      
      (SETF label-extent-y (+ label-extent-y (- y label-base-y))))
    )
  (extent-changed label))

(DEFMETHOD (SETF label-extent-x) :before (x (label label))
  (DECLARE (IGNORE x))
  (extent-changed label)
  (with-slots (transform label-base-x) label
    (SETF transform nil)
    (SETF label-base-x nil)))

(DEFMETHOD (SETF label-extent-y) :before (y (label label))
  (DECLARE (IGNORE y))
  (extent-changed label)
  (with-slots (transform label-base-y) label
    (SETF transform nil)
    (SETF label-base-y nil)))



(DEFMETHOD (SETF label-extent-width) :before (width (label label))
  (DECLARE (IGNORE width))
 (with-slots (extent label-base-x  label-gravity) label
    (WHEN  (valid-extent-p extent)
      (SETF label-base-x nil)
	) ))



(DEFMETHOD (SETF label-extent-height) :before (height (label label))
  (DECLARE (IGNORE height))
  (with-slots (extent label-base-y  label-gravity) label
    (WHEN  (valid-extent-p extent)
      (SETF label-base-y nil))
    ))




(DEFMETHOD (SETF label-gravity) :after (gravity (label label))
  (DECLARE (IGNORE gravity))
  (extent-changed label)
  (with-slots (label-base-x label-base-y ) label
      (SETF label-base-x nil)
      (SETF label-base-y nil)))




(defmethod graphic-contains-p ((label label)  x y &optional pixel-size)
  (declare (type wcoord x y))
  (DECLARE (IGNORE pixel-size))
  (IF (label-angle-extent label)
      (inside-p (label-angle-extent label) (make-point :x x :y y)) 
      (let* ((extent (world-extent label)))
	(and (>= x (extent-rect-xmin extent))
	     (>= y (extent-rect-ymin extent))
	     (<= x (extent-rect-xmax extent))
	     (<= y (extent-rect-ymax extent)))))
  )



(DEFMACRO assure-or-make-valid-label-font (font size view)
  `(PROGN
    (WHEN (STRINGP label-font-family) ;does not work on the Explorer
       (SETF label-font-family
	     (OR (find-font-family label-font-family (contact-display ,view))
		 (make-font-family  label-font-family (contact-display ,view)))))
     (SETF ,font (find-font label-font-family ,size))))


(DEFMETHOD extent-compute ((label label))
  (LET* ((view (graphic-view label))
	(scale (view-scale view)))
    (with-slots (extent label-view-scale label-font-size label-font
		 label-font-family label-extent-x label-extent-y
		 extent-width extent-height transform label-extent-clip-p) label
      (PROG1 
	(COND
	  ((label-angle-extent label)
	   (LET* ((point-seq (label-angle-extent label))
		  (xmin (point-seq-x-min point-seq))
		  (ymin (point-seq-y-min point-seq))
		  (xmax (point-seq-x-max point-seq))
		  (ymax (point-seq-y-max point-seq)))
	     (with-coercion ((xmin ymin xmax ymax) extent-element)
	       (make-extent-rect
		:xmin xmin
		:ymin ymin
		:xmax xmax
		:ymax ymax
		:valid t))))
	   ((AND label-extent-clip-p extent-width extent-height
		 label-extent-x label-extent-y)
	    (MULTIPLE-VALUE-BIND
		(x y) (transform-point transform label-extent-x label-extent-y)
	      (MULTIPLE-VALUE-BIND
		  (width height)(scale-point transform extent-width extent-height)
		(let ((xmax (+ x width -1))
		      (ymax (+ y height -1)))
		  (with-coercion ((x y xmax ymax) extent-element)
		    (extent-transform 
		     transform 
		     (make-extent-rect
		      :xmin x
		      :ymin y
		      :xmax xmax
		      :ymax ymax
		      :valid t)))))))
	   (t (view-label-extents label scale view )))
	(UNLESS (= label-view-scale scale)
	  (assure-or-make-valid-label-font
	   label-font (* scale label-font-size)
	   view))))))



(DEFMETHOD  view-label-extents ((label label) scale  (view view) )
  "changes the extents of the label in a view to match the current size of the label"
  (with-slots (label-font-family label-font-size label-base-x label-base-y
	       label-extent-x label-extent-y extent-width extent-height
	       label-font-family-name label-font transform) label
    (LET* ((font (assure-or-make-valid-label-font
		  label-font(* (scale-point (graphic-world-transform label) 1 1)
			       label-font-size scale)  view)))
      
      (MULTIPLE-VALUE-BIND 
	  (width ascent descent)(text-extents font (label-string label))
	(UNLESS (AND label-base-x label-base-y)
	  (MULTIPLE-VALUE-BIND
	      (x y) (compute-baseline-origin
		     label (view-scale-x view) (view-scale-y view))
	    (SETF label-base-x x
		  label-base-y y)))
	(MULTIPLE-VALUE-BIND
	    (xmin ymin) (transform-point transform label-base-x label-base-y )
	
	  (LET* ((scale-x (view-scale-x view))
		 (scale-y (view-scale-y view))
		 (extent (make-extent-rect
			  :xmin xmin
			  :xmax  (+ xmin (/ width scale-x) 1)
			  :ymin  (- ymin (/ (+ descent 2) scale-y) )
			  :ymax  (+ ymin (/ (+ ascent 2) scale-y)))))
	  
	    extent))))))

(DEFMACRO set-rotated-extent ()
  `(LET* ((vertices (label-angle-extent label))
	  (letter (ELT label-string  (+ pos (- increment))))
	  (width (* (max-char-width label-font)  scale))
	  (ascent  (* (max-char-ascent label-font) scale))
	  (descent (*  (max-char-descent label-font ) scale))
	  (height (IF (= 1  (+ (char-width label-font letter)
			       (char-descent label-font letter) ))
		      (* (max-char-ascent label-font) scale)
		      (+ ascent descent )))
	  (s1 (+ (ABS (* width dy))(ABS (* height dx)))))
     (SETF s2  (/ (* s1 (1+ dx2)) s)
	   x (+ x (* dy s2))
	   y (+ y (* (- dx) s2)))
     (COND	       
      ((OR (= quad 0) (= quad 3))
       (IF (label-angle-extent label)
	   (SETF (ELT vertices 0) xstart
		 (ELT vertices 1)  ystart
		 (ELT vertices 2)  x
		 (ELT vertices 3)  y 
		 (ELT vertices 4) (+ x (* dy s2))
		 (ELT vertices 5) (- y (* (- dx) s2))
		 (ELT vertices 6) (+ xstart (* dy s2))
		 (ELT vertices 7) (- ystart (* (- dx) s2)))
	   (SETF (label-angle-extent label)
		 (MAKE-ARRAY 8
			     :initial-contents
			     (LIST xstart
				   ystart
				   x
				   y
				   (+ x (* dy s2))
				   (- y (* (- dx) s2))
				   (+ xstart (* dy s2))
				   (- ystart (* (- dx) s2)))))))
      (t
       (IF (label-angle-extent label)
	   (SETF (ELT vertices 0) xstart
		 (ELT vertices 1)  ystart
		 (ELT vertices 2)  x
		 (ELT vertices 3)  y 
		 (ELT vertices 4) (- x (* dy s2))
		 (ELT vertices 5) (+ y (* (- dx) s2))
		 (ELT vertices 6) (- xstart (* dy s2))
		 (ELT vertices 7) (+ ystart (* (- dx) s2)))
	   (SETF (label-angle-extent label)
		 (MAKE-ARRAY 8
			     :initial-contents
			     (LIST xstart
				   ystart
				   x
				   y
				   (- x (* dy s2))
				   (+ y (* (- dx) s2))
				   (- xstart (* dy s2))
				   (+ ystart (* (- dx) s2))))))))))

(DEFMACRO draw-rotated-text ()
  `(LET* ((xstart x)
	  (ystart y)
	  (dy (COS label-angle))
	  (dx (SIN label-angle))
	  (scale (* (scale-point world-transform 1 1) label-view-scale ))
	  (dx2 (* dx dx) )
	  (s  (* (+ dx2 (* dy dy)) scale))
	  (quad (mod (floor (/ label-angle pi .5)) 4))
	  (start (IF (AND (label-reverse-p label) (OR (= quad 1)(= quad 2)))
		     (1- (LENGTH label-string))
		     0))
	  (stop  (IF (= start 0)
		     (LENGTH label-string)
		     -1))
	  (increment (IF (= start 0) 1 -1))
	  s2)
     
     (DO ((pos start (+ increment pos)))
	 ((= pos stop)
	  (set-rotated-extent))
       (LET* ((letter (ELT label-string  pos))
	      (width (* (- (char-width label-font letter) 0) scale))
	      (ascent (*  (char-ascent label-font letter) scale))
	      (descent (*  (char-descent label-font letter) scale))
	      (height (IF (= 1  (+ (char-width label-font letter)
				   (char-descent label-font letter) ))
			  (* (max-char-ascent label-font) scale)
			  (+ ascent descent )))
	      (s1 (+ (ABS (* width dy))(ABS (* height dx)))))
	 (WHEN (OR (= quad 2) (= quad 1))
	   (SETF s2  (/ (* s1 (1+ dx2)) s)
		 x (+ x (* dy s2))
		 y (+ y (* (- dx) s2))))
	 (IF background
	     (view-draw-image-char
	      view x y (ELT label-string pos)  (graphic-gstate label))
	     (view-draw-char
	      view x y (ELT label-string pos)  (graphic-gstate label)))
	 (WHEN (OR (= quad 0) (= quad 3))
	   (SETF s2  (/ (* s1 (1+ dx2)) s)
		 x (+ x (* dy s2))
		 y (+ y (* (- dx) s2))))
	 
	 ))
          (extent-changed label)
	  (graphic-extent label)
	  ))

(DEFMACRO label-visible-p (graphic)
  `(AND (NOT (AND (and min-x min-y width height) ; Was optional rect given
		  (not (graphic-within-p ,graphic min-x min-y width height))
		  (not (graphic-intersects-p ,graphic min-x min-y width height)))) 
	(viewable-p ,graphic)))

(defmethod draw-graphic ((label label) (view view)
			 &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))

  (WHEN  (label-visible-p label)
    (with-slots
	(gstate label-extent-clip-p label-base-x label-base-y 
		label-view-scale label-font-family 
		label-angle label-extent-x label-extent-y extent-width
		extent-height label-font-family-name label-string 
		label-font transform label-font-size extent views) label
      (LET ((world-transform (graphic-world-transform label))
	    (background (label-filled-background-p label))
	    (font-family (label-font-family label)))

	(change-font-if-scale-values-not-equal label label-view-scale view)
	(WHEN (STRINGP font-family)
	  (SETF (slot-value label  'label-font-family)
		(OR (find-font-family label-font-family (contact-display view))
		    (make-font-family label-font-family (contact-display view)))))
	(SETF label-font (find-font label-font-family
				    (* (scale-point world-transform 1 1)
				       label-font-size label-view-scale )))
	
	;; pw-need a view for extent computation
	(pushnew view views)
	(unless (extent-valid-p label)
	  (world-extent label))

	(MULTIPLE-VALUE-BIND (x y)
			     (compute-baseline-origin
			      label
			      (view-scale-x view) ( view-scale-y view))
	  (UNLESS (AND label-base-x label-base-y)
	    (SETF label-base-x  x)
	    (SETF  label-base-y y)))
	
	(UNLESS (extent-valid-p label) (graphic-extent label))
	(WHEN (>= (* label-font-size (* (scale-point world-transform 1 1)
					label-view-scale ))
		  (* 0.5 (font-size (find-font label-font-family 1))))
	  (IF label-extent-clip-p 
	      (SETF (gstate-value gstate :clip-mask)
		    (label-rectangle-mask label view ))
	      (SETF (gstate-value gstate :clip-mask) :none)
	      )
	  (SETF (gstate-value gstate :font) label-font)
	  (MULTIPLE-VALUE-BIND
	      (x y) (transform-point world-transform label-base-x label-base-y)
	    (IF (= label-angle 0)
		(IF background
		    (view-draw-image-text
		     view x y label-string  (graphic-gstate label))
		    (view-draw-text
		     view x y label-string  (graphic-gstate label)))
		(draw-rotated-text))))))))
			
	    



(DEFUN compute-baseline-origin (label  scale-x scale-y)
  "Return the  baseline coordinate of the label"
  (with-slots (extent label-gravity label-extent-x label-extent-y
	      (label-extent-height extent-height)
	      (label-extent-width extent-width) label-font label-string) label
    (MULTIPLE-VALUE-BIND
	(width ascent descent) (text-extents label-font label-string)
      (LET* ((extent-width (OR label-extent-width
			       (- (extent-rect-xmax extent)
				  (extent-rect-xmin extent))))
	     (extent-height (OR label-extent-height 
				(- (extent-rect-ymax extent)
				   (extent-rect-ymin extent))))
	     (extent-x (OR label-extent-x (extent-rect-xmin extent)))
	     (extent-y (OR label-extent-y (extent-rect-ymin extent)))
	     (width (/ width scale-x))
	     (ascent (/ ascent scale-y))
	     (descent (/ (1- descent) scale-y))
	     (height  (+ ascent descent)))
	(CASE label-gravity
	  (:southwest
	   (VALUES extent-x (+ extent-y descent) ))
	  (:northwest
	   (VALUES extent-x (+ extent-y (- extent-height height ))))
	  (:south
	   (VALUES (+ (- extent-x (/ width 2.0))
		      (/ extent-width 2.0) ) (+ extent-y descent)))
	  (:north
	   (VALUES (+ (- extent-x (/ width 2.0))
		      (/ extent-width 2.0) )
		   (+ extent-y (- extent-height height ))))
	  (:west
	   (VALUES extent-x (+ extent-y (- (/ extent-height 2.0)
					   (/ height 2.0)))))
	  (:center
	   (VALUES (+ (- extent-x (/ width 2.0))
		      (/ extent-width 2.0) )
		   (+  extent-y (- (/ extent-height 2.0) (/ height 2.0)))))
	  (:southeast
	   (VALUES  (+ extent-x (- extent-width width)) (+ extent-y descent)))
	  (:northeast
	   (VALUES  (+ extent-x (- extent-width width))
		    (+ extent-y (- extent-height height 1))))
	  (:east
	   (VALUES  (+ extent-x (- extent-width width))
		    (+  extent-y (- (/ extent-height 2.0) (/ height 2.0)))))
	  (t (VALUES extent-x extent-y))
	  )
	))))
  

(DEFUN label-rectangle-mask (label view )
  (LET ((local-vector (make-array '(4) :adjustable t :fill-pointer 0))
	(x 0)
	(y 1))
    (with-slots (transform extent) label
    (with-vector clip-vector
      (VECTOR-PUSH-EXTEND (extent-rect-xmin extent) clip-vector)
      (VECTOR-PUSH-EXTEND (extent-rect-ymin extent) clip-vector)
      (VECTOR-PUSH-EXTEND (extent-rect-xmax extent) clip-vector)
      (VECTOR-PUSH-EXTEND (extent-rect-ymax extent) clip-vector)
      (transform-point-seq
       (graphic-world-transform label) clip-vector )
      (view-transform-vector view clip-vector  )
      (VECTOR-PUSH-EXTEND
       (min-value-vector clip-vector x) local-vector) ;get min x value
      (VECTOR-PUSH-EXTEND
       (min-value-vector clip-vector y) local-vector) ;get min y value
      (VECTOR-PUSH-EXTEND
       (- (max-value-vector clip-vector x)
	  (min-value-vector clip-vector x)) local-vector) ;width
      (VECTOR-PUSH-EXTEND
       (- (max-value-vector clip-vector y)
	  (min-value-vector clip-vector y)) local-vector) ;height
      )
    local-vector)))

(DEFMETHOD normalize-graphic :before ((label label))
  (with-slots (label-base-x label-base-y label-extent-x label-extent-y
	       extent-width extent-height transform) label
    
    (MULTIPLE-VALUE-BIND
	(x y) (transform-point transform label-base-x label-base-y)
      (SETF (label-base-x label) x
	    (label-base-y label) y))
    (WHEN (AND label-extent-x label-extent-y extent-width extent-height)
      (MULTIPLE-VALUE-BIND
	  (x y) (transform-point transform label-extent-x label-extent-y)
	(MULTIPLE-VALUE-BIND
	    (x1 y1) (transform-point transform 
				     (+ label-extent-x extent-width)
				     (+ label-extent-y extent-height))
	  (SETF label-extent-x x
		label-extent-y y
		extent-width (- x1 x)
		extent-height (- y1 y)))))))

