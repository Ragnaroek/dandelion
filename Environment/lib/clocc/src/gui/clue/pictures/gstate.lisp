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
	  gstate-equal
	  make-gstate
	  gstate-value
	  pixelp
	  gstate-foreground
	  gstate-background
	  gstate-dashes
	  gstate-function
	  gstate-line-width
	  gstate-line-style
	  gstate-cap-style
	  gstate-join-style
	  gstate-fill-style
	  gstate-fill-rule
	  gstate-arc-mode
	  gstate-tile
	  gstate-stipple
	  remove-gstate-value
	  gstate-combine
	  gstate-copy
	  clear-gstate 
	  )
	'pictures)


;Private Macro: put-hash
;  A slightly nicer syntax for adding keys to a hash table.

(defmacro pixelp (pixel)
  `(AND (INTEGERP ,pixel) (<=  ,pixel 4294967296)))

(defmacro put-hash (key value table)
  `(setf (gethash ,key ,table) ,value))

(DEFPARAMETER  *gstate-signifigant-value* 15)

(DEFun gstate-equal (gstate1 gstate2)
  (LET ((ga1 (gstate-array gstate1))
	(ga2 (gstate-array gstate2)))
    (declare (type simple-array ga1 ga2))
    (DOTIMES (pos *gstate-signifigant-value* t)
      (declare (type fixnum pos))
      (UNLESS (EQUAL (ELT ga1 pos) (ELT ga2 pos)) (RETURN nil)))))

(defclass gstate ()
  ((gstate-hash
    :type	hash-table
    :initarg	:gstate-hash
    :initform	(MAKE-HASH-TABLE :size 7)
    :accessor   gstate-hash
    :documentation "a data structure of visual attribute/value pairs")
  (gstate-array
   :type	array
   :initform	(MAKE-ARRAY 21 :initial-element nil)
   :accessor    gstate-array
   :documentation "a data structure of visual attribute/value pairs"))
  (:documentation
   "The object that represents the visual state of a graphic object")
  )

(DEFMACRO with-gstate-value-place (&body body)
  `(let ,*gstate-index*

     ,@body))

(defmethod  gstate-value ((gstate gstate) keyword)
  "Return the value of a gstate keyword. Return nil if not defined"
  (declare (type keyword keyword))
  (with-slots ((ga gstate-array)) gstate
    (with-gstate-value-place
     (declare (type simple-array ga))
      (CASE keyword
	(:foreground (ELT ga foreground))
	(:background (ELT ga background))
	(:line-style  (ELT ga line-style))
	(:line-width  (ELT ga line-width))
	(:function (ELT ga function))
	(:ts-x  (ELT ga ts-x))
	(:ts-y  (ELT ga ts-y))
	(:arc-mode  (ELT ga arc-mode))
	(:cap-style (ELT ga cap-style))
	(:clip-mask (ELT ga clip-mask))
	(:clip-x  (ELT ga clip-x))
	(:clip-y  (ELT ga clip-y))
	(:tile  (ELT ga tile))
	(:dash-offset (ELT ga dash-offset))
	(:dashes (ELT ga dashes))
	(:fill-rule (ELT ga fill-rule))
	(:fill-style (ELT ga fill-style))
	(:font (ELT ga font))
	(:join-style (ELT ga join-style))
	(:stipple (ELT ga stipple))
	(:stipple-pix (ELT ga stipple-pix))
	
	(otherwise 
	 (VALUES  (GETHASH keyword (slot-value gstate 'gstate-hash))))))))

(defmethod (SETF gstate-value) ((value xlib:pixmap) (gstate gstate)
				(keyword (eql :foreground)))
  (setf (gstate-value gstate :fill-style) :stippled
	(gstate-value gstate :stipple-pix) value))


;Function: make-gstate
;  Return a new gstate object with the given attribute values and return an
;  alist of value not defined for X window gcontexts

(defun make-gstate (&rest options)
  
  (LET ((gstate (MAKE-INSTANCE 'gstate)) )
    (DO*
      ((options-list options (CDDR options-list))
       ( keyword (FIRST options-list)(FIRST options-list))
       ( value (SECOND options-list)(SECOND options-list)))
      ((EQ options-list nil) gstate)
      (IF  (KEYWORDP keyword)
	   (SETF (gstate-value gstate keyword) value) 
	   (ERROR "%keyword  ~a is not a keyword symbol" keyword)))
   ))




(DEFMETHOD print-object ((gstate gstate) stream)
  "print an alist of the values of the gstate hash table"
  (LET ((ga (gstate-array gstate)))
    (FORMAT stream "[")
    (DOTIMES (pos (LENGTH ga))
      (IF (ELT ga pos)
	  (FORMAT stream "[ ~a ~a  ] "
		  ;;(first (rassoc pos *gstate-index* :key #'first))
		  (first (find pos *gstate-index* :key #'second))
		  (ELT ga pos) )
	  ))
    (FORMAT stream "]" )))


;;Method: gstate-value
;;  Return or change the value associated with the KEYWORD in GSTATE.



(defmethod (SETF gstate-value) (value (gstate gstate) keyword )
  "set a gstate value
  if the value is of an incorrect type an error message will be returned
  if the keyword is not defined for X windows gcontext the
  keyword-value-list will be returned as a second value"
  (with-gstate-value-place
   (LET  ((ga (gstate-array gstate)))
     (declare (type simple-array ga))
     (LET* ((type-value
	     ;get the type list from the master list
	     (SECOND (ASSOC keyword *gstate-type-alist*))))
       ;if the type is "okay" place the keyword pair on the alist
       (IF type-value
	   (IF (DOLIST ( gtype type-value nil)
		 ;is type type of the keyword value in the type list
		 (IF (TYPEP value gtype) (return t)))
	       (CASE keyword
		 (:foreground  (SETF (ELT ga foreground ) value))
		 (:background  (SETF (ELT ga background ) value))
		 (:line-style  (SETF (ELT ga line-style ) value))
		 (:line-width  (SETF (ELT ga line-width ) value))
		 (:function    (SETF (ELT ga function   ) value))
		 (:ts-x        (SETF (ELT ga ts-x       ) value))
		 (:ts-y        (SETF (ELT ga ts-y       ) value))
		 (:arc-mode    (SETF (ELT ga arc-mode   ) value))
		 (:cap-style   (SETF (ELT ga cap-style  ) value))
		 (:clip-mask   (SETF (ELT ga clip-mask  ) value))
		 (:clip-x      (SETF (ELT ga clip-x     ) value))
		 (:clip-y      (SETF (ELT ga clip-y     ) value))
		 (:tile        (SETF (ELT ga tile       ) value))
		 (:dash-offset (SETF (ELT ga dash-offset) value))
		 (:dashes      (SETF (ELT ga dashes     ) value))
		 (:fill-rule   (SETF (ELT ga fill-rule  ) value))
		 (:fill-style  (SETF (ELT ga fill-style ) value))
		 (:font        (SETF (ELT ga font) value))
		 (:join-style  (SETF (ELT ga join-style ) value))
		 (:stipple     (SETF (ELT ga stipple    ) value))
		 (:stipple-pix (SETF (ELT ga stipple-pix) value))
		 
		 (otherwise
		  (PUT-HASH ;place type keyword pair on the gstate-hashtable
		   keyword value (slot-value gstate 'gstate-hash)))
		 )
	       (ERROR 
		"~%keyword pair ~a ~a has an invalid type~%" keyword value)
		)
	   (PUT-HASH ;place unknown type keyword pair on the gstate-hashtable
	    keyword value (slot-value gstate 'gstate-hash))
	   ))))
  value
  )



(DEFMETHOD (SETF gstate-value) :around (value (gstate gstate) keyword)
  (DECLARE (SPECIAL *gstate-stack*))
  (CALL-next-method)
  (graphic-stack-purge *gstate-stack* )
  (VALUES 
    (WHEN value
      (LET ((ga (slot-value gstate 'gstate-array)))
	(declare (type simple-array ga))
	(with-gstate-value-place
	 (DECLARE (IGNORE line-style line-width function
			  ts-x ts-y arc-mode cap-style clip-mask
			  stipple-pix clip-x clip-y dash-offset dashes
			  fill-rule fill-style font join-style ))
	 (COND
	  ((OR (EQ keyword :tile)
	       (EQ keyword :stipple))
	   (IF (SYMBOLP value)
	       (CASE keyword
		 (:tile 
		  (SETF (ELT ga tile   ) (LIST value (SYMBOL-VALUE value))))
		 (:stipple
		  (SETF (ELT ga stipple) (LIST value (SYMBOL-VALUE value)))))
	       (CASE keyword
		   (:tile 
		    (SETF (ELT ga tile   ) (LIST (image-name value)  value)))
		   (:stipple
		    (SETF (ELT ga stipple) (LIST (image-name value)  value))))
		 ))
	    
	  ((OR (EQ keyword :foreground)
	       (EQ keyword :background))
	   (COND
	    ((STRINGP value)
	     (CASE keyword
	       (:foreground (SETF (ELT ga foreground) (LIST value value)))
	       (:background (SETF (ELT ga background) (LIST value value)))))
	    ((LISTP value) value)
	    ((pixelp value)
	     (CASE keyword
	       (:foreground (SETF (ELT ga foreground) (LIST value value)))
	       (:background (SETF (ELT ga background) (LIST value value)))))
	    ((pixmap-p value) value)
	    (t
	     (CASE keyword
	       (:foreground
		(SETF (ELT ga foreground) (LIST value (SYMBOL-VALUE value))))
	       (:background
		(SETF (ELT ga background) (LIST value (SYMBOL-VALUE value)))))
	     )))
	    (t value)))
	))))


(DEFMETHOD (SETF gstate-foreground) :before (foreground (graphic graphic))
  (DECLARE (IGNORE foreground))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD gstate-foreground ((gstate gstate))
  (CADR (gstate-value gstate :foreground)))

(DEFMETHOD gstate-foreground ((graphic graphic))
  (CADR (gstate-value (graphic-gstate graphic) :foreground)))

(DEFMETHOD (SETF gstate-foreground) (foreground (gstate gstate))
  (COND
    ((NOT foreground) (remove-gstate-value gstate :foreground))
    ((STRINGP foreground)
     (CADR (SETF (gstate-value gstate :foreground) foreground)))
    ((NUMBERP foreground)
     (CADR (SETF (gstate-value gstate :foreground) foreground)))
    ((pixelp (SYMBOL-VALUE  foreground))
     (CADR (SETF (gstate-value gstate :foreground) foreground)))
    (t (ERROR "The value ~a for foreground is not of type PIXEL"))))

(DEFMETHOD (SETF gstate-foreground) (foreground (graphic graphic))
  (COND
   ((NOT foreground)
    (remove-gstate-value (graphic-gstate graphic) :foreground))
   ((STRINGP foreground)
    (CADR
     (SETF (gstate-value (graphic-gstate graphic) :foreground) foreground)))
   ((NUMBERP foreground)
    (CADR
     (SETF (gstate-value (graphic-gstate graphic) :foreground) foreground)))
    ((pixelp (SYMBOL-VALUE  foreground))
     (CADR
      (SETF (gstate-value (graphic-gstate graphic) :foreground) foreground)))
    (t (ERROR "The value ~a for foreground is not of type PIXEL"))))

(DEFMETHOD gstate-background ((gstate gstate))
  (CADR (gstate-value gstate :background)))

(DEFMETHOD gstate-background ((graphic graphic))
  (CADR (gstate-value (graphic-gstate graphic) :background)))

(DEFMETHOD (SETF gstate-background) (background (gstate gstate))
  (COND
    ((NOT background) (remove-gstate-value gstate :background))
    ((STRINGP background)
     (CADR (SETF (gstate-value gstate :background) background)))
    ((NUMBERP background)
     (CADR (SETF (gstate-value gstate :background) background)))
    ((pixelp (SYMBOL-VALUE  background))
     (CADR (SETF (gstate-value gstate :background) background)))
   (t (ERROR "The value ~a for background is not of type PIXEL"))))

(DEFMETHOD (SETF gstate-background) :before (background (graphic graphic))
  (DECLARE (IGNORE background))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-background) (background (graphic graphic))
  (COND
   ((NOT background)
    (remove-gstate-value (graphic-gstate graphic) :background))
   ((STRINGP background)
    (CADR
     (SETF (gstate-value (graphic-gstate graphic) :background) background)))
   ((NUMBERP background)
    (CADR
     (SETF (gstate-value (graphic-gstate graphic) :background) background)))
   ((pixelp (SYMBOL-VALUE  background))
    (CADR
     (SETF (gstate-value (graphic-gstate graphic) :background) background)))
   (t (ERROR "The value ~a for background is not of type PIXEL")))
  )

(DEFMETHOD gstate-dashes ((gstate gstate))
  (gstate-value gstate :dashes))

(DEFMETHOD gstate-dashes ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :dashes))

(DEFMETHOD (SETF gstate-dashes) (dashes (gstate gstate))
  (IF (OR (listp dashes)
	  (ARRAYP dashes)
	  (AND (NUMBERP dashes) (< -1 dashes 255)))
      (SETF (gstate-value gstate :dashes) dashes)
      (UNLESS dashes (remove-gstate-value gstate :dashes))))

(DEFMETHOD (SETF gstate-dashes) :before (dashes (graphic graphic))
  (DECLARE (IGNORE dashes))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-dashes) (dashes (graphic graphic))
  (IF (OR (listp dashes)
	  (ARRAYP dashes)
	  (AND (NUMBERP dashes) (< -1 dashes 255))) 
      (SETF (gstate-value (graphic-gstate graphic) :dashes) dashes)
      (UNLESS dashes (remove-gstate-value (graphic-gstate graphic) :dashes))))


(DEFMETHOD gstate-function ((gstate gstate))
  (gstate-value gstate :function))

(DEFMETHOD gstate-function ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :function))

#+huh
(DEFMETHOD (SETF gstate-function) (function (gstate gstate))
  (IF (OR (MEMBER function '(boole-1 boole-2 boole-and boole-andc1 booleandc2
			     boole-xor boole-c1 boole-c2 boole-clr boole-eqv
			     boole-ior boole-nand boole-nor boole-orc1
			     boole-orc2 boole-set))
	  (< -1 function 17))
      (SETF (gstate-value gstate :function) function)
      (UNLESS function (remove-gstate-value gstate :function))))

(DEFMETHOD (SETF gstate-function) (function (gstate gstate))
  (IF (and function
	   (OR (MEMBER function '(boole-1 boole-2 boole-and boole-andc1
				  booleandc2 boole-xor boole-c1 boole-c2
				  boole-clr boole-eqv boole-ior boole-nand
				  boole-nor boole-orc1 boole-orc2 boole-set))
	       (< -1 function 17)))
      (SETF (gstate-value gstate :function) function)
      (remove-gstate-value gstate :function)))

(DEFMETHOD (SETF gstate-function) :before (function (graphic graphic))
  (DECLARE (IGNORE function))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-function) (function (graphic graphic))
  (IF (and function
	   (OR (MEMBER function '(boole-1 boole-2 boole-and boole-andc1
				  booleandc2 boole-xor boole-c1 boole-c2
				  boole-clr boole-eqv boole-ior boole-nand
				  boole-nor boole-orc1 boole-orc2 boole-set))
	       (< -1 function 17)))
      (SETF (gstate-value (graphic-gstate graphic) :function) function)
      (remove-gstate-value (graphic-gstate graphic) :function)))

(DEFMETHOD gstate-line-width ((gstate gstate))
  (gstate-value gstate :line-width))

(DEFMETHOD gstate-line-width ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :line-width))

(DEFMETHOD (SETF gstate-line-width) (line-width (gstate gstate))
  (IF (AND  (NUMBERP line-width)(<= 0 line-width))
      (SETF (gstate-value gstate :line-width) line-width)
      (UNLESS line-width (remove-gstate-value gstate :line-width))))

(DEFMETHOD (SETF gstate-line-width) :before (line-width (graphic graphic))
  (DECLARE (IGNORE line-width))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-line-width) (line-width (graphic graphic))
  (extent-changed graphic)
  (IF (AND  (NUMBERP line-width)(<= 0 line-width))
      (SETF (gstate-value (graphic-gstate graphic) :line-width) line-width)
      (UNLESS line-width
	(remove-gstate-value (graphic-gstate graphic) :line-width)))
  )

(DEFMETHOD gstate-font ((gstate gstate))
  (gstate-value gstate :font))

(DEFMETHOD (SETF gstate-font) (font (gstate gstate))
  (IF (font-p font)
      (SETF (gstate-value gstate :font) font)
      (UNLESS font (remove-gstate-value gstate :font))))

(DEFMETHOD (SETF gstate-font) (font (graphic graphic))
  (IF (font-p font)
      (SETF (gstate-value (graphic-gstate graphic) :font) font)))

(DEFMETHOD gstate-font ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :font))

(DEFMETHOD gstate-line-style ((gstate gstate))
  (gstate-value gstate :line-style))

(DEFMETHOD gstate-line-style ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :line-style))

(DEFMETHOD (SETF gstate-line-style) (line-style (gstate gstate))
  (IF (MEMBER line-style '(:solid :dash :double-dash))
      (SETF (gstate-value gstate :line-style) line-style)
      (UNLESS line-style (remove-gstate-value gstate :line-style))))


(DEFMETHOD (SETF gstate-line-style) :before (line-style (graphic graphic))
  (DECLARE (IGNORE line-style))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-line-style) (line-style (graphic graphic))
  (IF (MEMBER line-style '(:solid :dash :double-dash))
      (SETF (gstate-value (graphic-gstate graphic) :line-style) line-style)
      (UNLESS line-style
	(remove-gstate-value (graphic-gstate graphic) :line-style))))

(DEFMETHOD gstate-cap-style ((gstate gstate))
  (gstate-value gstate :cap-style))

(DEFMETHOD gstate-cap-style ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :cap-style))

(DEFMETHOD (SETF gstate-cap-style) (cap-style (gstate gstate))
  (IF (MEMBER cap-style '(:not-last :butt :round :projecting))
      (SETF (gstate-value gstate :cap-style) cap-style)
      (UNLESS cap-style (remove-gstate-value gstate :cap-style))))

(DEFMETHOD (SETF gstate-cap-style) :before (cap-style (graphic graphic))
  (DECLARE (IGNORE cap-style))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-cap-style) (cap-style (graphic graphic))
  (IF (MEMBER cap-style '(:not-last :butt :round :projecting))
      (SETF (gstate-value (graphic-gstate graphic) :cap-style) cap-style)
      (UNLESS cap-style
	(remove-gstate-value (graphic-gstate graphic) :cap-style))))

(DEFMETHOD gstate-join-style ((gstate gstate))
  (gstate-value gstate :join-style))

(DEFMETHOD gstate-join-style ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :join-style))

(DEFMETHOD (SETF gstate-join-style) (join-style (gstate gstate))
  (IF (MEMBER join-style '(:miter :round :bevel))
      (SETF (gstate-value gstate :join-style) join-style)
      (UNLESS join-style (remove-gstate-value gstate :join-style))))

(DEFMETHOD (SETF gstate-join-style) :before (join-style (graphic graphic))
  (DECLARE (IGNORE join-style))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-join-style) (join-style (graphic graphic))
  (IF (MEMBER join-style '(:miter :round :bevel))
      (SETF (gstate-value (graphic-gstate graphic) :join-style) join-style)
      (UNLESS join-style
	(remove-gstate-value (graphic-gstate graphic) :join-style))))

(DEFMETHOD gstate-fill-style ((gstate gstate))
  (gstate-value gstate :fill-style))

(DEFMETHOD gstate-fill-style ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :fill-style))

(DEFMETHOD (SETF gstate-fill-style) (fill-style (gstate gstate))
  (IF (MEMBER fill-style '(:solid :tiled :opaque-stippled :stippled))
      (SETF (gstate-value gstate :fill-style) fill-style)
      (UNLESS fill-style (remove-gstate-value gstate :fill-style))))

(DEFMETHOD (SETF gstate-fill-style) :before (fill-style (graphic graphic))
  (DECLARE (IGNORE fill-style))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-fill-style) (fill-style (graphic graphic))
  (IF (MEMBER fill-style '(:solid :tiled :opaque-stippled :stippled))
      (SETF (gstate-value (graphic-gstate graphic) :fill-style) fill-style)
      (UNLESS fill-style
	(remove-gstate-value (graphic-gstate graphic) :fill-style))))

(DEFMETHOD gstate-fill-rule ((gstate gstate))
  (gstate-value gstate :fill-rule))

(DEFMETHOD gstate-fill-rule ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :fill-rule))

(DEFMETHOD (SETF gstate-fill-rule) (fill-rule (gstate gstate))
  (IF (MEMBER fill-rule '(:even-odd :winding))
      (SETF (gstate-value gstate :fill-rule) fill-rule)
      (UNLESS fill-rule (remove-gstate-value gstate :fill-rule))))

(DEFMETHOD (SETF gstate-fill-rule) :before (fill-rule (graphic graphic))
  (DECLARE (IGNORE fill-rule))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-fill-rule) (fill-rule (graphic graphic))
  (IF (MEMBER fill-rule '(:even-odd :winding))
      (SETF (gstate-value (graphic-gstate graphic) :fill-rule) fill-rule)
      (UNLESS fill-rule
	(remove-gstate-value (graphic-gstate graphic) :fill-rule))))

(DEFMETHOD gstate-arc-mode ((gstate gstate))
  (gstate-value gstate :arc-mode))

(DEFMETHOD gstate-arc-mode ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :arc-mode))

(DEFMETHOD (SETF gstate-arc-mode) (arc-mode (gstate gstate))
  (IF (MEMBER arc-mode '(:chord :pie-slice))
      (SETF (gstate-value gstate :arc-mode) arc-mode)
      (UNLESS arc-mode (remove-gstate-value gstate :arc-mode))))

(DEFMETHOD (SETF gstate-arc-mode) (arc-mode (graphic graphic))
  (IF (MEMBER arc-mode '(:chord :pie-slice))
      (SETF (gstate-value (graphic-gstate graphic) :arc-mode) arc-mode)
      (UNLESS arc-mode
	(remove-gstate-value (graphic-gstate graphic) :arc-mode))))

(DEFMETHOD gstate-tile ((gstate gstate))
  (CADR  (gstate-value gstate :tile)))

(DEFMETHOD gstate-tile ((graphic graphic))
  (CAdR  (gstate-value (graphic-gstate graphic) :tile)))

(DEFMETHOD (SETF gstate-tile) (tile (gstate gstate))
  (IF  tile
       (LET ((value (IF (SYMBOLP tile)
			(SYMBOL-VALUE tile)
			tile)))
	 (IF (OR (image-x-p value)(image-xy-p value)(image-z-p value))
	     (CADR (SETF (gstate-value gstate :tile)
			 tile))
	     (ERROR "the symbol ~a does not evaluate to an IMAGE" tile)))
       (UNLESS tile (remove-gstate-value gstate :tile))))

(DEFMETHOD (SETF gstate-tile) :before (tile (graphic graphic))
  (DECLARE (IGNORE tile))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-tile) (tile (graphic graphic))
  (IF tile
      (LET ((value (IF (SYMBOLP tile)(SYMBOL-VALUE tile) tile)))
	(IF (OR (image-x-p value)(image-xy-p value)(image-z-p value))
	    (CADR (SETF (gstate-value (graphic-gstate graphic) :tile)
			tile))
	    (ERROR "the symbol ~a does not evaluate to an IMAGE" tile)))
      (UNLESS tile (remove-gstate-value (graphic-gstate graphic) :tile))))

(DEFMETHOD gstate-stipple ((gstate gstate))
  (CADR (gstate-value gstate :stipple)))

(DEFMETHOD gstate-stipple ((graphic graphic))
  (CADR (gstate-value (graphic-gstate graphic) :stipple)))

(DEFMETHOD (SETF gstate-stipple) (stipple (gstate gstate))
  (IF stipple
      (LET ((value (IF (SYMBOLP stipple)(SYMBOL-VALUE stipple) stipple)))
	(IF (OR (image-x-p value)(image-xy-p value)(image-z-p value))
	    (IF (= (image-depth value) 1)
		(CADR (SETF (gstate-value gstate :stipple) stipple))
		(ERROR "image depth of ~a is greater than 1"
		       (image-depth value)))
	     (ERROR "the symbol ~a does not evaluate to an IMAGE" stipple)))
       (UNLESS stipple (remove-gstate-value gstate :stipple))))

(DEFMETHOD (SETF gstate-stipple) :before (stipple (graphic graphic))
  (DECLARE (IGNORE stipple))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-stipple) (stipple (graphic graphic))
  (IF stipple
      (LET ((value (IF (SYMBOLP stipple) (SYMBOL-VALUE stipple) stipple)))
	(IF (OR (image-x-p value)(image-xy-p value)(image-z-p value))
	    (IF (= (image-depth value) 1)
		(CADR (SETF (gstate-value (graphic-gstate graphic) :stipple)
			    stipple))
		(ERROR "image depth of ~a is greater than 1"
		       (image-depth value)))
	    (ERROR "the symbol ~a does not evaluate to an IMAGE" stipple)))
      (UNLESS stipple
	(remove-gstate-value (graphic-gstate graphic) :stipple))))

(DEFMETHOD gstate-clip-mask ((gstate gstate))
  (gstate-value gstate :clip-mask))

(DEFMETHOD gstate-clip-mask ((graphic graphic))
  (gstate-value (graphic-gstate graphic) :clip-mask))

(DEFMACRO sequencep (value)
  `(OR (LISTP ,value) (ARRAYP ,value)))

(DEFMETHOD (SETF gstate-clip-mask) (clip-mask (gstate gstate))
  (IF (OR (MEMBER clip-mask '(:none))(sequencep clip-mask))
      (SETF (gstate-value gstate :clip-mask) clip-mask)
      (UNLESS clip-mask (remove-gstate-value gstate :clip-mask))))

(DEFMETHOD (SETF gstate-clip-mask) :before (clip-mask (graphic graphic))
  (DECLARE (IGNORE clip-mask))
  (UNLESS  (slot-value graphic 'gstate)
    (SETF (slot-value graphic 'gstate) (make-gstate))))

(DEFMETHOD (SETF gstate-clip-mask) (clip-mask (graphic graphic))
  (IF (OR (MEMBER clip-mask '(:none))(sequencep clip-mask))
      (SETF (gstate-value (graphic-gstate graphic) :clip-mask) clip-mask)
      (UNLESS clip-mask
	(remove-gstate-value (graphic-gstate graphic) :clip-mask))))


(DEFMETHOD remove-gstate-value ((gstate gstate) keyword)
  (with-slots ((ga gstate-array) gstate-hash) gstate
    (with-gstate-value-place
     (declare (type simple-array ga))
      (CASE keyword
	(:foreground  (SETF (ELT ga foreground) nil))
	(:background  (SETF (ELT ga background) nil))
	(:line-style  (SETF (ELT ga line-style) nil))
	(:line-width  (SETF (ELT ga line-width) nil))
	(:function    (SETF (ELT ga function)   nil))
	(:ts-x        (SETF (ELT ga ts-x)       nil))
	(:ts-y        (SETF (ELT ga ts-y)       nil))
	(:arc-mode    (SETF (ELT ga arc-mode)   nil))
	(:cap-style   (SETF (ELT ga cap-style)  nil))
	(:clip-mask   (SETF (ELT ga clip-mask)  nil))
	(:clip-x      (SETF (ELT ga clip-x)     nil))
	(:clip-y      (SETF (ELT ga clip-y)     nil))
	(:tile        (SETF (ELT ga tile)       nil))
	(:dash-offset (SETF (ELT ga dash-offset) nil))
	(:dashes      (SETF (ELT ga dashes)     nil))
	(:fill-rule   (SETF (ELT ga fill-rule)  nil))
	(:fill-style  (SETF (ELT ga fill-style) nil))
	(:font (SETF  (ELT ga font)             nil))
	(:join-style  (SETF (ELT ga join-style) nil))
	(:stipple     (SETF (ELT ga stipple)    nil))
	(:stipple-pix (SETF (ELT ga stipple-pix) nil))
	
	(otherwise (REMHASH keyword gstate-hash))
	))))

;Private Variable: temp-gstate
;  Used in gstate-combine to hold temporary result
(DEFPARAMETER  *temp-gstate* (make-gstate))


;Function: gstate-combine
;  Combine GSTATE-1 with GSTATE-2 and put the result in RESULT.  If
;  RESULT is not given, then the result replaces GSTATE-2.  If both
;  GSTATE-2 and RESULT are nil, then a new gstate is created to hold the
;  result.  A nil gstate represents the empty gstate.  The new value of RESULT
;  is returned.

(defun gstate-combine (gstate-1 gstate-2
				&optional (result gstate-2))
  (cond ((not gstate-1)				; G-1 is empty
         (gstate-copy gstate-2 result))		;   Just use G-2
        ((not gstate-2)				; G-2 is empty
         (gstate-copy gstate-1 result))		;   Just use G-1
        ((eq gstate-2 result)			; G-2 and RESULT are the same
         (combine-into gstate-1 result))	;   Combine G-1 into RESULT
        ((eq gstate-1 result)			; G-1 and RESULT are the same
         (gstate-copy gstate-1 *temp-gstate*)	;   Use a temporary
         (gstate-copy gstate-2 result)
         (combine-into *temp-gstate* result))	;   Combine temp into RESULT
        (t			; Otherwise, combine G-1 and G-2 into RESULT
        (gstate-copy gstate-2 result)		;   Start with G-2
         (combine-into gstate-1 result))))	;   Combine G-1 into RESULT


;Function: gstate-copy
;  Copy GSTATE-1 into GSTATE-2.  Either or both can be nil.  Return the new
;  GSTATE-2.

					; Return G-2

(DEFMACRO clear-gstate (gstate)
  `(PROGN
    (dotimes (var (LENGTH (gstate-array ,gstate)))
      (SETF (ELT (gstate-array ,gstate) var) nil))
    (clrhash (gstate-hash ,gstate))))

(defun  gstate-copy ( gstate-1 gstate-2)
  (declare (type (or null gstate) gstate-1 gstate-2))

  (cond ((eq gstate-1 gstate-2))		; They are already identical!
        (gstate-1				; G-1 is not the empty gstate
         (if gstate-2
	     (clear-gstate gstate-2)		;   Must clean out G-2 first
             (setf gstate-2 (make-gstate)))	;   Must make G-2 first
         (combine-into gstate-1 gstate-2))	;   Combine G-1 into G-2
        (t					; G-1 is the empty gstate
         (if gstate-2
	     (clear-gstate gstate-2)		;   Make G-2 empty as well
             (setf gstate-2 (make-gstate)))))	;   Or create one if not there yet

  gstate-2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Private Function: combine-into
;  Combine the hash table entries of GSTATE-1 into those of GSTATE-2.
;  Return the result GSTATE-2



(DEFMETHOD combine-into ((gstate-1 gstate) (gstate-2 gstate))

  (maphash #'(lambda (keyword value)		;   Combine G-1 into G-2
               (PUT-HASH keyword value (gstate-hash gstate-2)))
           (gstate-hash gstate-1))
  (dotimes (var (LENGTH (gstate-array gstate-2)))
     
		 (UNLESS (ELT (gstate-array gstate-2) var)
		   (SETF (ELT (gstate-array gstate-2) var)
			 (ELT (gstate-array gstate-1) var))))
  gstate-2)



(DEFMETHOD combine-into ((gstate-1 t) (gstate-2 gstate))

  gstate-2)

(DEFMETHOD combine-into ((gstate-1 t) (gstate-2 t))

  gstate-2)

(DEFMETHOD combine-into ((gstate-1 gstate) (gstate-2 t))
  (LET ((gstate (make-gstate)))
    (maphash #'(lambda (keyword value)		;   Combine G-1 into G-2
		 (PUT-HASH keyword value (gstate-hash gstate)))
	     (gstate-hash gstate-1))
    (dotimes (var (LENGTH (gstate-array gstate)))
      
      (UNLESS (ELT (gstate-array gstate) var)
	(SETF (ELT (gstate-array gstate) var)
	      (ELT (gstate-array gstate-1) var))))
    
    gstate))
