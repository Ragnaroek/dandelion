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

(defmacro valid-xcoord (var)
  `(typep ,var 'int16))

;Private Macro: valid-xdistance
; Determine whether the given VAR is a valid distance for the X window system

(defmacro valid-xdistance (var)
  `(typep ,var 'card16))


;View Class Definition:

;;class-definitions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Private Function: working-gcontext
;  If GSTATE is nil, return the default-gcontext for VIEW.  Otherwise,
;  compute the working gcontext by combining the default gcontext with
;  gstate.

(DEFun search-for-gcontext (gstate view)
  (LET ((gcontext-cache (view-gcontext-cache view)))
    (DOTIMES  (pos (LENGTH gcontext-cache) nil)
      (LET ((pair (ELT gcontext-cache pos)))
	(WHEN (gstate-equal gstate (FIRST pair)) (RETURN (SECOND pair)))))))

(DEFMACRO prepare-gstate (gstate view)
   `(LET ((ga (slot-value ,gstate 'gstate-array)))
      (LET ((color (CADR (ELT ga foreground))))
	(WHEN  color
		(IF (STRINGP color)
		    (SETF (gstate-value ,gstate :foreground)
				(LIST color
				      (alloc-color
					(CAR (installed-colormaps ,view))
					color)))
		    color)))
      (LET ((color (CADR (ELT ga background))))
	(WHEN  color
		(IF (STRINGP color)
		    (SETF (gstate-value ,gstate :background)
				(LIST color
				      (alloc-color
					(CAR (installed-colormaps ,view))
					color)))
		    color)))))

(DEFMACRO prepare-gcontext (view gstate  gcontext)
  "set up the gcontext for drawing"
  `(LET ((ga (slot-value ,gstate 'gstate-array)))
      (WHEN (ELT ga foreground)
	(SETF (gcontext-foreground ,gcontext) (CADR (ELT ga foreground))))
      (WHEN (ELT ga background)
	(SETF (gcontext-background ,gcontext) (CADR (ELT ga background))))
      (WHEN (ELT ga line-style )
	(SETF (gcontext-line-style ,gcontext) (ELT ga line-style)))
      (WHEN (ELT ga line-width)
	(SETF (gcontext-line-width ,gcontext)
	      (floor (* (view-scale ,view)(ELT ga line-width)))))
      (when (elt ga function )
	(SETF (gcontext-function ,gcontext) (elt ga  function)))
      (when (elt ga ts-x )  (SETF (gcontext-ts-x ,gcontext)  (elt ga  ts-x)))
      (when (elt ga ts-y )   (SETF (gcontext-ts-y ,gcontext) (elt ga ts-y)))
      (when (elt ga arc-mode)
	(SETF (gcontext-arc-mode ,gcontext) (elt ga  arc-mode)))
      (when (elt ga cap-style )
	(SETF (gcontext-cap-style ,gcontext) (elt ga  cap-style)))
      (IF (elt ga clip-mask )
	  (SETF (gcontext-clip-mask ,gcontext) (elt ga  clip-mask))
	  (SETF (gcontext-clip-mask ,gcontext) :none))
      (when (elt ga clip-x )  (SETF (gcontext-clip-x ,gcontext) (elt ga  clip-x)))
      (when (elt ga clip-y )  (SETF (gcontext-clip-y ,gcontext) (elt ga  clip-y)))
      (when (elt ga tile )  (SETF (gcontext-tile ,gcontext)
	    (LET ((image (CADR (elt ga  tile))))
	      (IF (= (image-depth image) 1)
		  (contact-image-mask
		   ,view image
		   :foreground (OR (gstate-foreground gstate) 1)
		   :background (OR (gstate-background gstate) 0))
		  (contact-image-pixmap ,view image)))))
      (when (elt ga dash-offset )
	(SETF (gcontext-dash-offset ,gcontext) (elt ga dash-offset)))
      (when (elt ga dashes )
	(SETF (gcontext-dashes ,gcontext) (elt ga  dashes)))
      (when (elt ga fill-rule )
	(SETF (gcontext-fill-rule ,gcontext) (elt ga  fill-rule)))
      (when (elt ga fill-style )
	(SETF (gcontext-fill-style ,gcontext) (elt ga  fill-style)))
      (when (elt ga font )
	(SETF (gcontext-font ,gcontext) (elt ga  font)))
      (when (elt ga stipple-pix )
	(setf (gcontext-stipple ,gcontext) (elt ga stipple-pix )))
      (when (elt ga join-style )
	(SETF (gcontext-join-style ,gcontext) (elt ga  join-style)))
      (when (elt ga stipple )
	(SETF (gcontext-stipple ,gcontext)
	      (contact-image-mask ,view (CADR (elt ga  stipple))
				  :foreground (OR (gstate-foreground gstate) 1)
				  :background (OR (gstate-background gstate) 0))))
     )
  )

(defun working-gcontext ( view   gstate)
  (with-gstate-value-place
   (prepare-gstate gstate view)
   (LET ((new-gcontext (search-for-gcontext gstate view)))

      (IF new-gcontext
	  (LET ((ga (slot-value gstate 'gstate-array)))

	    (WHEN (ELT ga line-width)
	      (SETF (gcontext-line-width new-gcontext)
		    (floor (* (ELT ga line-width) (view-scale view)))))
	    (IF (elt ga clip-mask )
		(SETF (gcontext-clip-mask new-gcontext) (elt ga  clip-mask))
		(SETF (gcontext-clip-mask new-gcontext) :none))
	    (WHEN (OR (elt ga stipple )(elt ga tile )(elt ga stipple-pix ))
	      (when (elt ga ts-x )
		(SETF (gcontext-ts-x new-gcontext)  (elt ga  ts-x)))
	      (when (elt ga ts-y )
		(SETF (gcontext-ts-y new-gcontext)  (elt ga  ts-y)))))
	  (with-slots ( default-gcontext) view

	    (SETF new-gcontext (create-gcontext :drawable view))
	    (copy-gcontext default-gcontext new-gcontext)
	    (prepare-gcontext view gstate new-gcontext)
	    (VECTOR-PUSH-EXTEND
	     (LIST (gstate-copy gstate (make-gstate)) new-gcontext)
	     (view-gcontext-cache view) )
	    )
	  
	  )
      new-gcontext
      )))



(defmethod view-draw-arc ((view view) x-min y-min width height
			       angle1 angle2 &optional gstate)
  (declare (type wcoord x-min y-min width height))
  (declare (type angle angle1 angle2))

  (multiple-value-setq (x-min y-min) (transform-point view x-min y-min))
  (multiple-value-setq (width height) (view-scale-point view width height))
  (when (and (valid-xcoord x-min) (valid-xcoord y-min)
	     (valid-xdistance width) (valid-xdistance height))
    (draw-arc view
              (if gstate
		  (working-gcontext view gstate) 
		  (slot-value view 'default-gcontext))
	      x-min (- y-min height) width height angle1 angle2)))


(defmethod view-draw-filled-arc ((view view) x-min y-min width height
			       angle1 angle2 &optional gstate)
  (declare (type wcoord x-min y-min width height))
  (declare (type angle angle1 angle2))

  (multiple-value-setq (x-min y-min) (transform-point view x-min y-min))
  (multiple-value-setq (width height) (view-scale-point view width height))
  (when (and (valid-xcoord x-min)
	     (valid-xcoord y-min)
	     (valid-xdistance width)
	     (valid-xdistance height))
    (draw-arc view (if gstate (working-gcontext view gstate)
		       (slot-value view 'default-gcontext))
	      x-min (- y-min height) width height angle1 angle2 t)))


(defmethod view-draw-line ((view view) start-x start-y end-x end-y
                                   &optional gstate)
  (declare (type wcoord start-x start-y end-x end-y))

  (multiple-value-setq (start-x start-y)
    (transform-point view start-x start-y))
  (multiple-value-setq (end-x end-y)
    (transform-point view end-x end-y))
  (when (and (valid-xcoord start-x)
             (valid-xcoord start-y)
             (valid-xcoord end-x)
             (valid-xcoord end-y))
    (draw-line view (if gstate 
			(working-gcontext view gstate)
			(slot-value view 'default-gcontext))
               start-x start-y end-x end-y)))


(defmethod view-draw-polypoint ((view view)  point-sequence
					&optional gstate)
  (declare (type vector point-sequence))
  (with-vector  point-list 
    (DOTIMES (i (LENGTH point-sequence))
      (VECTOR-PUSH-EXTEND  (AREF point-sequence i) point-list))
    (draw-points view (if gstate
			  (working-gcontext view gstate)
			  (slot-value view 'default-gcontext))
		 (view-transform-vector view point-list ))
 	))



(defmethod view-draw-polyline ((view view)  point-sequence
				       &optional gstate)
  (declare (type vector point-sequence))
  (with-vector  point-list 
    (DOTIMES (i (LENGTH point-sequence) )
      (VECTOR-PUSH-EXTEND  (AREF point-sequence i) point-list))
    (draw-lines view (if gstate
			 (working-gcontext view gstate)
			 (slot-value view 'default-gcontext))
		(view-transform-vector view point-list))
    ))


(defmethod view-draw-polygon ((view view) point-sequence
				       &optional  gstate)
  (declare (type vector point-sequence))
  (with-vector  point-list 
    (copy-to-vector point-sequence point-list)
    (insert-vertex
     point-list (vertex-x point-sequence 0) (vertex-y point-sequence 0)
     *large-number*)
    (draw-lines view (if gstate
			 (working-gcontext view gstate)
			 (slot-value view 'default-gcontext))
		(view-transform-vector view point-list ))
    ))


(defmethod view-draw-filled-polygon ((view view) point-sequence
				       &optional  gstate)
  (declare (type vector point-sequence))
  (with-vector  point-list 
    (copy-to-vector point-sequence point-list)
    (insert-vertex point-list
		   (vertex-x point-sequence 0)
		   (vertex-y point-sequence 0)
		   *large-number*)
  (draw-lines view (if gstate
		       (working-gcontext view gstate)
		       (slot-value view 'default-gcontext))
	      (view-transform-vector view point-list ) :fill-p t)))


(defmethod view-draw-image ((view view) x-min y-min x-max y-max image
				    &optional  gstate)
  (declare (type wcoord x-min y-min))
  (MULTIPLE-VALUE-SETQ (x-min y-min)
    (transform-point view x-min y-min ))
  (MULTIPLE-VALUE-SETQ (x-max y-max)
    (transform-point view x-max y-max ))
  (SETF (gstate-tile gstate) image)
  (WHEN (AND (valid-xcoord x-min) (valid-xcoord y-min))
    (draw-rectangle view (working-gcontext view gstate) 	; Draw it
                    x-min y-max  (ABS (- x-max x-min)) (ABS (- y-min y-max)) t)))


(defmethod view-draw-rectangle ((view view) x-min y-min width height
			        &optional  gstate)
  (declare (type wcoord x-min y-min width height))  

  (multiple-value-setq (x-min y-min)		; Convert to view coordinates
    (transform-point view x-min y-min ))
  (multiple-value-setq (width height)
    (view-scale-point view width height ))

  (WHEN (AND (valid-xcoord x-min)	; Make sure rect is within the universe
             (valid-xcoord y-min)
             (valid-xdistance width)
             (valid-xdistance height))
    (draw-rectangle view (if gstate
			     (working-gcontext view gstate)
			     (slot-value view 'default-gcontext)) ; Draw it
                    x-min (- y-min height) width height)))


(defmethod view-draw-filled-rectangle ((view view) x-min y-min width height
			        &optional gstate)
  (declare (type wcoord x-min y-min width height))  

  (multiple-value-setq (x-min y-min)		; Convert to view coordinates
    (transform-point view x-min y-min))
  (multiple-value-setq (width height)
    (view-scale-point view width height))
  
  (WHEN (AND (valid-xcoord x-min)	; Make sure rect is within the universe
             (valid-xcoord y-min)
             (valid-xdistance width)
             (valid-xdistance height))
    (draw-rectangle view (if gstate
			     (working-gcontext view gstate)
			     (slot-value view 'default-gcontext))	; Draw it
		    x-min (- y-min height) width height t)))



(defmethod view-draw-text ((view view) x-origin y-origin text
				    &optional  gstate)
  (declare (type wcoord x-origin y-origin))
  (declare (type string text))
 (MULTIPLE-VALUE-BIND (x y)
      (transform-point view x-origin y-origin)
    (WHEN (AND (valid-xcoord x) (valid-xcoord y))
      (draw-glyphs view (if gstate
			    (working-gcontext view gstate)
			    (slot-value view 'default-gcontext))
		   x y text))))

(defmethod view-draw-image-text ((view view) x-origin y-origin text
				    &optional  gstate)
  (declare (type wcoord x-origin y-origin))
  (declare (type string text))
 (MULTIPLE-VALUE-BIND (x y)
      (transform-point view x-origin y-origin)
    (WHEN (AND (valid-xcoord x) (valid-xcoord y))
      (draw-image-glyphs view (if gstate
				  (working-gcontext view gstate)
				  (slot-value view 'default-gcontext))
			 x y text))))

(defmethod view-draw-char ((view view) x-origin y-origin text
				    &optional  gstate)
  (declare (type wcoord x-origin y-origin))
  (declare (type string text))
 (MULTIPLE-VALUE-BIND (x y)
      (transform-point view x-origin y-origin)
    (WHEN (AND (valid-xcoord x) (valid-xcoord y))
      (draw-glyph view (if gstate (working-gcontext view gstate) (slot-value view 'default-gcontext))
			 x y text))))

(defmethod view-draw-image-char ((view view) x-origin y-origin text
				    &optional  gstate)
  (declare (type wcoord x-origin y-origin))
  (declare (type string text))
 (MULTIPLE-VALUE-BIND (x y)
      (transform-point view x-origin y-origin)
    (WHEN (AND (valid-xcoord x) (valid-xcoord y))
      (draw-image-glyph view (if gstate
				 (working-gcontext view gstate)
				 (slot-value view 'default-gcontext))
			 x y text))))
