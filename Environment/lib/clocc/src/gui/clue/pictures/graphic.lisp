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


;;; Global Transform Stack
;  Stack of transforms used to speed world coordinate computation
(DEFPARAMETER  *transform-stack* (make-instance 'transform-stack))

;;; Global Gstate Stack
;  Stack of gstates used to speed gstate computation

(DEFPARAMETER  *an-graphic-extent-rectangle* (make-extent-rect))

;;; Type definitions

(export '(
	  graphic
	  draw-graphic
	  draw-graphic-clipped
	  graphic-extent
	  world-extent
	  graphic-contains-p
	  graphic-intersects-p
	  graphic-within-p
	  graphic-fixed-point
	  graphic-view
	  graphic-views
	  graphic-damage
	  repair-graphic
	  graphic-parent
	  graphic-plist
	  graphic-gstate
	  graphic-reset
	  graphic-transform
	  move-transform
	  rotate-transform
	  scale-transform
	  normalize-graphic
	  graphic-combined-world-extents
	  graphic-world-transform
	  graphic-combined-gstate
	  scene-elements
	  graphic-sensitivity
	  editable-p
	  selectable-p
	  viewable-p
	  subselectable-p
	  graphic-extent-x
	  graphic-extent-y
	  graphic-extent-height
	  graphic-extent-width
	  world-extent
	  grpahic-extent-x
	  grpahic-extent-y
	  graphic-extent-height
	  graphic-extent-width

	  )
	'pictures)

;class definition is in file class-definitions

;;Method: scene-elements


(DEFMETHOD scene-elements ((graphic graphic))
  graphic
  nil)


(DEFMETHOD graphic-extent-x ((graphic graphic))
  (with-slots (extent) graphic
    (extent-rect-xmin extent)))

(DEFMETHOD graphic-extent-y ((graphic graphic))
  (with-slots (extent) graphic
    (extent-rect-ymin extent)))

(DEFMETHOD graphic-extent-height ((graphic graphic))
  (with-slots (extent) graphic
    (ABS (- (extent-rect-ymax extent) (extent-rect-ymin extent)))))

(DEFMETHOD graphic-extent-width ((graphic graphic))
  (with-slots (extent) graphic
    (ABS (- (extent-rect-xmax extent) (extent-rect-xmin extent)))))

(defmethod graphic-fixed-point ((graphic graphic) FIXED-POINT
				&optional world-coordinate)
  (declare (type (member :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast)
                 fixed-point))
  (with-slots (extent) graphic
    (UNLESS (valid-extent-p extent) (graphic-extent graphic))
    (let* (		; Get the extent
           (xmin (extent-rect-xmin extent))
           (xmax (extent-rect-xmax extent))
           (ymin (extent-rect-ymin extent))
           (ymax (extent-rect-ymax extent))
           (xmid (/ (+ xmin xmax) 2.0))			; Compute mid points
           (ymid (/ (+ ymin ymax) 2.0)))
      (MULTIPLE-VALUE-BIND (x y)
	  (case fixed-point			       ; Return the appropriate coord
	    (:northwest	(values xmin ymax))
	    (:north		(values xmid ymax))
	    (:northeast	(values xmax ymax))
	    (:west		(values xmin ymid))
	    (:center	(values xmid ymid))
	    (:east		(values xmax ymid))
	    (:southwest	(values xmin ymin))
	    (:south		(values xmid ymin))
	    (:southeast	(values xmax ymin))
	    #| pw-- these are not valid according to declaration above. Bitrot?
	    (northwest-grabber	(values xmin ymax))
	    (north-grabber		(values xmid ymax))
	    (northeast-grabber	(values xmax ymax))
	    (west-grabber		(values xmin ymid))
	    (center-grabber	(values xmid ymid))
	    (east-grabber		(values xmax ymid))
	    (southwest-grabber	(values xmin ymin))
	    (south-grabber		(values xmid ymin))
	    (southeast-grabber	(values xmax ymin))
	    |#
	    )
	
	(IF world-coordinate
	    (transform-point (graphic-world-transform graphic) x y)
	    (VALUES x y))))))


;Method: combined-gstate
;  Return the fully combined gstate for the given GRAPHIC.
;  A gstate-stack is used to ensure that,
;	1. Repeated references to the same graphic are very fast,
;	2. Depth-first picture traversals are fast,
;	3. References to ancestors and decendents are reasonably fast, and
;	4. Off-the-wall references to unrelated graphics are not too slow.

(DEFPARAMETER  *working-gstate* (make-gstate))

(defmethod graphic-combined-gstate ((graphic graphic))
  (clear-gstate  *working-gstate*)
  (IF (graphic-parent graphic) ;added the test for a standalone graphic 3-10-90 HTH
      (graphic-stack-gstate (graphic-stack-find *gstate-stack* graphic))
      (graphic-gstate graphic)))

(defmethod graphic-combined-edge-gstate ((graphic graphic))
  (IF (graphic-parent graphic) ;added the test for a standalone graphic 3-10-90 HTH
      (graphic-stack-gstate (graphic-stack-find *edge-gstate-stack* graphic))
      (edge-gstate graphic)))

;Function: editable-p
;  Return true if GRAPHIC and all of its ancestors are editable.

(DEFMETHOD  editable-p ((graphic graphic))
  (declare (type graphic graphic))

  (and (eq (graphic-sensitivity graphic) :editable)
       (or (null (graphic-parent graphic))
           (editable-p (graphic-parent graphic)))))


;Method: extent-changed
;  Something has happened that might have changed the extent of GRAPHIC.
;  The primary method simply passes the notification on to GRAPHIC's parent.

(defmethod extent-changed ((graphic graphic))

  (with-slots (parent) graphic
    (when parent
      (extent-changed parent))))


;Method: extent-compute
;  Compute the extent rectangle for the GRAPHIC.  The method should be defined
;  for each derived graphic class.  The primary method returns nil, meaning
;  "undefined extent."

;  Note: A graphic's extent rectangle is defined in the object coordinate
;  system.  This means that each graphic should apply its own transform to
;  its computed extent before returning it.  To obtain the extent of a
;  graphic in the world coordinate system, call the world-extent method
;  (defined below).

(defmethod extent-compute ((graphic graphic))

  nil)			; Instances of the base class have undefined extent

(DEFMETHOD extent-compute :around ((graphic graphic))
  (LET ((gstate  (graphic-gstate graphic))
	(extent (call-next-method )))
    #+cmu
    (when extent
      (setf (extent-rect-valid extent) t))

    (WHEN gstate
      (LET ((width (gstate-line-width gstate)))
	(WHEN width
	  (SETF (extent-rect-xmin extent) (- (extent-rect-xmin extent)
					     (/ width 2.0))) 
	  (SETF (extent-rect-ymin extent) (- (extent-rect-ymin extent)
					     (/ width 2.0)))
	  (SETF (extent-rect-xmax extent) (+ (extent-rect-xmax extent)
					     (/ width 2.0))) 
	  (SETF (extent-rect-ymax extent) (+ (extent-rect-ymax extent)
					     (/ width 2.0))))))
    extent))

(DEFMETHOD extent-compute :around ((graphic edge))
  (LET ((edge-gstate  (edge-gstate graphic))
	(extent (call-next-method )))
    
    (WHEN edge-gstate
      (LET ((width (gstate-line-width edge-gstate)))
	(WHEN width
	  (SETF (extent-rect-xmin extent) (- (extent-rect-xmin extent)
					     (/ width 2.0))) 
	  (SETF (extent-rect-ymin extent) (- (extent-rect-ymin extent)
					     (/ width 2.0)))
	  (SETF (extent-rect-xmax extent) (+ (extent-rect-xmax extent)
					     (/ width 2.0))) 
	  (SETF (extent-rect-ymax extent) (+ (extent-rect-ymax extent)
					     (/ width 2.0))))))
    extent))

;Method: graphic-extent
;  Return the extent rectangle for the GRAPHIC in the GRAPHIC's object
;  coordinate system.  If the extent is undefined, then nil is returned.
;  This method should be called (in lieu of extent-compute) whenever a
;  graphic's extent is required.  This is to account for the possibility of
;  extent-cacheing by the graphic.

;  The primary method just calls extent-compute to compute the GRAPHIC's
;  extent.

(defmethod graphic-extent ((graphic graphic))

  (extent-compute graphic)) ; Go compute the extent and return the result


(DEFUN valid-extent-p (extent)
  "check to see if all of the values of the extent are valid"
  #-cmu
  (IF (extent-rect-p extent)
      (AND (NUMBERP (extent-rect-xmin extent))
       (NUMBERP (extent-rect-ymin extent))
       (NUMBERP (extent-rect-xmax extent))
       (NUMBERP (extent-rect-ymax extent)))
      nil)
  #+cmu
  (and (extent-rect-p extent)
       (extent-rect-valid extent)))

    
;Method: graphic-contains-p
;  Determine whether the given point in world coordinates is contained by
;  (lies on) the given GRAPHIC.  The primary method uses the world-extent
;  of the graphic to determine whether it contains the point.  Derived
;  graphic classes may wish to refine this by taking into account their
;  specific geometry.

(defmethod graphic-contains-p ((graphic graphic)  x y &optional pixel-size)
  (declare (type wcoord x y))
  (DECLARE (IGNORE pixel-size))
  (let* ((extent (world-extent graphic)))
    (and (>= x (extent-rect-xmin extent))
         (>= y (extent-rect-ymin extent))
         (<= x (extent-rect-xmax extent))
         (<= y (extent-rect-ymax extent)))))


;Function: graphic-damage
;  Records GRAPHIC as a damaged region in each view to which it is attached.


(DEFMETHOD  graphic-damage ((graphic graphic))
  "Records graphic as a damaged region in each view to which it is attached."
  (with-slots (extent) graphic
    (WHEN (valid-extent-p extent)
      (do ((g graphic (graphic-parent g)))
	  ((null g))
	(dolist (v (graphic-views g))
	  (view-damage v graphic))))))


;Method: draw-graphic
;  Draw the GRAPHIC object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and
;  HEIGHT are given, then only parts of the object that lie within the
;  given rectangle need to be drawn.  The primary method does nothing.

(defmethod draw-graphic ((graphic graphic) (view view)
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))

  (declare (ignore min-x min-y width height)))





;Method: draw-graphic-clipped
;  Draw the GRAPHIC in the given VIEW with the given clipping rectangle.

(defmethod draw-graphic-clipped ((graphic graphic) (view view)
				 min-x min-y width height)
  (declare (type wcoord min-x min-y width height))
  (with-slots (extent) graphic
    (WHEN (visible-p graphic)
      (SETF (gstate-clip-mask graphic)
	    (clip-mask graphic view min-x min-y width height))
      (draw-graphic graphic view)
      (SETF (gstate-clip-mask graphic) :none))
    ))

(DEFUN clip-mask (graphic view min-x min-y width height)
  (LET ((local-vector (make-array '(4) :adjustable t :fill-pointer 0))
	(x 0)
	(y 1))
    (with-slots (transform ) graphic
      (with-vector clip-vector
	(VECTOR-PUSH-EXTEND min-x clip-vector)
	(VECTOR-PUSH-EXTEND min-y clip-vector)
	(VECTOR-PUSH-EXTEND (+ min-x width) clip-vector)
	(VECTOR-PUSH-EXTEND (+ min-y height) clip-vector)
;	(transform-point-seq (graphic-world-transform graphic) clip-vector )
	(view-transform-vector view clip-vector  )
	(VECTOR-PUSH-EXTEND
	 (min-value-vector clip-vector x) local-vector)	;get min x value
	(VECTOR-PUSH-EXTEND
	 (min-value-vector clip-vector y) local-vector)	;get min y value
	(VECTOR-PUSH-EXTEND
	  (- (max-value-vector clip-vector x) (min-value-vector clip-vector x))
	  local-vector)	;width
	(VECTOR-PUSH-EXTEND
	  (- (max-value-vector clip-vector y) (min-value-vector clip-vector y))
	  local-vector)	;height
	)
      local-vector)))


;Method: graphic-intersects-p
;  If the given GRAPHIC intersects the rectangle given in world
;  coordinates, then return T.  Otherwise, return nil.  The primary method
;  uses the world-extent of the graphic to determine whether it intersects
;  the rectangle.  Derived graphic classes may wish to refine this by
;  taking into account their specific geometry.

(defmethod graphic-intersects-p ((graphic graphic) min-x min-y width height)
  (declare (type wcoord min-x min-y width height))

  (let* ((extent (world-extent graphic))
         (max-x  (+ min-x width))
         (max-y  (+ min-y height)))
    (IF extent                     ;empty scene do not have a world extent
	(not (or
	       (> (extent-rect-xmin extent) max-x)
	       (> (extent-rect-ymin extent) max-y)
	       (< (extent-rect-xmax extent) min-x)
	       (< (extent-rect-ymax extent) min-y)))
	nil)))

;Method: graphic-transform
;  Return or change the object transform associated with the given graphic.
;  A null graphic-transform represents the common case of the identity
;  transform.

(defmethod (setf graphic-transform) (new-transform (graphic graphic))
  (declare (type (or null transform) new-transform))
  (PROG1
    (setf (slot-value graphic 'transform) new-transform)
    (graphic-stack-purge
      *transform-stack* graphic) ; Notify the transform stack
    (extent-changed graphic))) ; Notify graphic his extent may have changed


;Method: normalize-graphic
;  Normalize the GRAPHIC by applying its transform to its geometry,
;  changing it accordingly, and then setting its transform to nil (the
;  identity transform).  Nothing of value is returned.

; The primary method just sets the graphic's transform to nil.

(defmethod normalize-graphic ((graphic graphic))
  (extent-changed graphic)
    (setf (graphic-transform graphic) nil))

;Method: graphic-parent
;  Return or change the parent of a graphic

(defmethod graphic-parent ((graphic graphic))
  (slot-value graphic 'parent))

(defmethod (setf graphic-parent) (new-parent (graphic graphic))
  (declare (type (or null graphic) new-parent))
  (with-slots (parent) graphic
    (when parent
      (extent-changed parent)
      (scene-delete parent graphic)) ; Notify old parent he's lost a child
    (IF new-parent
	(scene-insert new-parent graphic) ; Record the new parent
	(SETF parent nil))
    (extent-changed graphic) ; Notify new parent he's gained one
    (graphic-stack-purge *transform-stack* graphic) ; Notify the transform stack
    (graphic-stack-purge *gstate-stack* graphic)
    (graphic-stack-purge *edge-gstate-stack* graphic))) ; Notify the gstate stack


;Method: graphic-gstate
;  Return or change the graphics state of the given graphic.

(defmethod graphic-gstate ((graphic graphic))
  (slot-value graphic 'gstate))

(defmethod (setf graphic-gstate) (new-gstate (graphic graphic))
  (setf (slot-value graphic 'gstate) new-gstate)
  (graphic-stack-purge *gstate-stack* graphic)
  new-gstate)		; Notify the gstate stack



;Method: graphic-views
;  Return a list of all the views to which the GRAPHIC is attached.  The
;  primary method returns nil.

(defun graphic-views (graphic &optional view-list)
  "Returns the  VIEW-LIST associated with the GRAPHIC."
  (with-slots (views) graphic
    (WHEN views
      (DOLIST (aview views)
	(SETF view-list (CONS aview view-list))))
    (if (graphic-parent graphic)
	(graphic-views (graphic-parent graphic) view-list)
	view-list)))

(DEFMETHOD  graphic-view ((graphic graphic))
  "Returns or (with setf) changes the first VIEW associated with the GRAPHIC."
  (FIRST (graphic-views graphic)))

(DEFMETHOD (SETF graphic-view) (view (graphic graphic))
  (with-slots (views) graphic
	(SETF views (APPEND (LIST view) views ))
	))

;Method: graphic-within-p
;  If the given GRAPHIC lies completely within the rectangle given in world
;  coordinates, then return T.  Otherwise, return nil.  The primary method
;  uses the world-extent of the graphic to determine whether it is within
;  the rectangle.  Derived graphic classes may wish to refine this by
;  taking into account their specific geometry.

(defmethod graphic-within-p ((graphic graphic) min-x min-y width height)
  (declare (type wcoord min-x min-y width height))

  (let* ((extent (world-extent graphic))
         (max-x  (+ min-x width))
         (max-y  (+ min-y height)))
    (and (>= (extent-rect-xmin extent) min-x)
         (>= (extent-rect-ymin extent) min-y)
         (<= (extent-rect-xmax extent) max-x)
         (<= (extent-rect-ymax extent) max-y))))


;Method: gstate-value
;  Return or change the value associated with the KEYWORD in the gstate of
;  GRAPHIC.  Note: (setf (gstate-value graphic) ...) should always be used
;  to modify a graphic's gstate because it purges the gstate stack.

(defmethod gstate-value ((graphic graphic) keyword)
  (with-slots (gstate) graphic
    (when gstate
      (gstate-value (slot-value graphic 'gstate) keyword))))

(defmethod (setf gstate-value) (attribute  (graphic graphic) keyword)
  (with-slots (gstate) graphic
    (unless gstate 			; Must create it first
      (setf gstate (make-gstate)))
    (prog1
      (setf (gstate-value (slot-value graphic 'gstate) keyword) attribute)
      (graphic-stack-purge *gstate-stack* graphic)))) ; Notify the gstate stack


;Method: move-transform
;  Translate the GRAPHIC by the given distances. The new object transform
;  is returned.

(defmethod move-transform ((graphic graphic) delta-x delta-y)
  (declare (type wcoord delta-x delta-y))

  (with-slots (transform) graphic
    (graphic-damage graphic) 	; Damage from old graphic
    (when (null transform)	; If no transform
      (setf transform (make-transform)))	; Create one
    (graphic-stack-purge *transform-stack* graphic) ; Notify the transform stack
    (prog1
      (move-transform transform delta-x delta-y) ; Move it
      (extent-changed graphic))	; Notify graphic his extent may have changed
    (graphic-damage graphic)	; Damage from new graphic
    transform))




;Function: repair
;  Invokes repair-view for all views to which the GRAPHIC is attached.

(defun repair-graphic (graphic)
  "Invokes repair-view for all views to which the GRAPHIC is attached."
  (declare (type graphic graphic))

  (MAPC #'(lambda (view) (repair-view view)) (graphic-views graphic)))


;Method: rotate-transform
;  Rotate the GRAPHIC by the given ANGLE (in degrees) around the given fixed
;  point. The fixed point is given in the world coordinate system. The new
;  object transform is returned.

(defmethod rotate-transform ((graphic graphic) angle
                             &optional (fixed-x 0) (fixed-y 0))
  (declare (type angle angle))
  (declare (type wcoord fixed-x fixed-y))

  (with-slots (transform) graphic
    (graphic-damage graphic)	; Damage from old graphic
    (when (null transform)	; If no transform
      (setf transform (make-transform))) ; Create one
    (graphic-stack-purge *transform-stack* graphic) ; Notify the transform stack
    (prog1
      (rotate-transform		; Rotate it
       transform angle fixed-x fixed-y)
      (extent-changed graphic))	; Notify graphic his extent may have changed
    (graphic-damage graphic)	; Damage from new graphic
    transform))

;Method: scale-transform
;  Scale the GRAPHIC by the given scale factors around the given fixed point.
;  The fixed point is given in the world coordinate system. The new object
;  transform is returned.

(defmethod scale-transform ((graphic graphic) scale-x scale-y
                            &optional (fixed-x 0) (fixed-y 0))
  (declare (type (or (satisfies plusp) (satisfies zerop)) scale-x scale-y))
  (declare (type ocoord fixed-x fixed-y))
  (graphic-damage graphic)	; Damage from old graphic
  (with-slots (transform) graphic
    (when (null transform)	; If no transform
      (setf transform (make-transform))) ; Create one
    (graphic-stack-purge *transform-stack* graphic) ; Notify the transform stack
    (PROG1
	(scale-transform	; Scale it
	 transform scale-x scale-y fixed-x fixed-y)
      (extent-changed graphic))	; Notify graphic his extent may have changed
    (graphic-damage graphic)	; Damage from new graphic
    transform))
 
;Function: selectable-p
;  Return true if GRAPHIC and all of its ancestors are selectable.

(DEFMETHOD  selectable-p ((graphic graphic))
  (declare (type graphic graphic))

  (and (member (graphic-sensitivity graphic) '(:selectable :editable))
       (or (null (graphic-parent graphic))
           (subselectable-p (graphic-parent graphic)))))

(defun subselectable-p (graphic)
  (declare (type graphic graphic))

  (and (member (graphic-sensitivity graphic) '(:subselectable ))
       (or (null (graphic-parent graphic))
           (subselectable-p (graphic-parent graphic)))))

;Method: graphic-sensitivity
;  Return or change the graphic-sensitivity of the given GRAPHIC.  A
;  graphic may be :hidden, :viewable (and therefore not :hidden),
;  :selectable (and therefore also :viewable), or, :editable (and therefore
;  also :viewable and :selectable).

(defmethod (setf graphic-sensitivity) (how-sensitive (graphic graphic))
  (declare (type (member :hidden :viewable :selectable :subselectable :editable)
		 how-sensitive))
  (ASSERT  (MEMBER how-sensitive 
		   '(:hidden :viewable :selectable  :subselectable :editable))
	   (how-sensitive)
	   "enter a new value of :hidden :viewable :selectable  :subselectable or :editable"
	     )
  (setf (slot-value graphic 'sensitivity) how-sensitive)
  )

;Function: viewable-p
;  Return true if GRAPHIC and all of its ancestors are viewable.

(DEFMETHOD  viewable-p ((graphic graphic))
  (declare (type graphic graphic))

  (and (member (graphic-sensitivity graphic)
	       '(:viewable :subselectable :selectable :editable))
       (or (null (graphic-parent graphic))
           (viewable-p (graphic-parent graphic)))))

;Method: world-extent
;  Return the extent of the given GRAPHIC in world coordinates.  A nil
;  value means that the extent is undefined.  If RESULT-EXTENT is provided,
;  it is used to store the result extent.  Otherwise, a new extent-rect is
;  created and returned.

(defmethod world-extent ((graphic graphic) &optional result-extent)
  (declare (type (or null extent-rect) result-extent))

  (let ((parent (graphic-parent graphic))) ; Get graphic's parent
    (if parent			; Does it have one?
        (extent-transform	; Yes, use parent's world xform
	 (graphic-world-transform parent) ;  to transform graphic's extent
	 (graphic-extent graphic)
	 (or result-extent (make-extent-rect)))
	(extent-transform	; No parent, just copy extent
	 (graphic-transform graphic)
	 (graphic-extent graphic)
	 (or result-extent (make-extent-rect))))))

(DEFUN graphic-combined-world-extents ( graphic &rest graphics)
  "Combine the extents of the GRAPHICS in world coordinates and
return the combined extent"
  (LET ((extent (world-extent graphic)))
    (DOLIST (graph graphics)
      (extent-combine (world-extent graph) extent))
    extent))

;Method: graphic-world-transform
;  Return the fully-composed transform to compute world coordinates for
;  the GRAPHIC.

;  A transform-stack is used to ensure that,
;	1. Repeated references to the same graphic are very fast,
;	2. Depth-first picture traversals are fast,
;	3. References to ancestors and decendents are reasonably fast, and
;	4. Off-the-wall references to unrelated graphics are not too slow.

(defmethod graphic-world-transform ((graphic graphic))

  (graphic-stack-transform (graphic-stack-find *transform-stack* graphic)))

;; this method determines if a point in within the extent of a graphic

(DEFUN point-in-extents-p (graphic x y &optional ( pixel 1))
  "this function determines if a given point is within the extent bound of a graphic"
  (LET* (
	 (graphic-extent (world-extent graphic)))
    (AND (>= x (- (extent-rect-xmin graphic-extent) pixel))
	 (<= x (+ (extent-rect-xmax graphic-extent) pixel))
	 (>= y (- (extent-rect-ymin graphic-extent) pixel))
	 (<= y (+ (extent-rect-ymax graphic-extent) pixel)))))



(DEFUN graphic-reset (graphic )
  (SETF (graphic-gstate graphic) nil)
  (SETF (graphic-transform graphic) nil)
  (SETF (graphic-view graphic) nil)
  (SETF (graphic-parent graphic) nil)
  (SETF (graphic-plist graphic) nil)
  (extent-changed graphic)
  t)
