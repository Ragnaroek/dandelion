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
	  scene
	  make-scene
	  valid-extent-p
	  scene-delete
	  scene-elements
	  scene-insert
	  scene-reparent
	  scene-ungroup
	  scene-graphic
	  scene-restack
	  )
	'pictures)

;Scene Class Definition:




;Type: scene-position



;Function: make-scene
;  Return a new scene object with no elements.

(defun make-scene (&key  elements gstate parent
			 (sensitivity :editable)
			 transform plist view edge-gstate)
  (FUNCALL  #'make-instance 'scene
	    :scene-view view
	    :gstate gstate
	    :edge-gstate edge-gstate
	    :scene-parent parent
	    :sensitivity sensitivity
	    :transform transform
	    :plist plist
	    :elements elements
	    )
  )

(DEFMETHOD initialize-instance :after ((scene scene)
				       &key elements scene-view scene-parent)
  (with-slots (views (scene-elements elements)) scene
    (WHEN scene-parent (SETF (graphic-parent scene) scene-parent))
    (WHEN views (SETF views (LIST scene-view)))
    (WHEN elements
      (DOTIMES (pos (LENGTH elements))
	(scene-insert scene (ELT elements pos))
	))))



;Method: draw-graphic
;  Draw the SCENE object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and
;  HEIGHT are given, then only parts of the object that lie within the
;  given rectangle need to be drawn.  For scenes, just draw each of the
;  child graphics.

(defmethod draw-graphic-clipped ((scene scene) (view view)
                           min-x min-y width height)
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (elements extent) scene
    (WHEN (visible-p scene) 
	(graphic-world-transform scene)	; Cache our transform
	(DOTIMES (position (length elements))
	  (draw-graphic-clipped 
	   (ELT elements position) view min-x min-y width height)
	  ))))

(defmethod draw-graphic ((scene scene) (view view)
                           &optional min-x min-y width height)
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (elements extent) scene
    (WHEN (visible-p scene) 
	(graphic-world-transform scene)	; Cache our transform
	(DOTIMES (position (length elements))
	  (draw-graphic (ELT elements position) view min-x min-y width height)
	  ))))




;Method: normalize-graphic
;  Normalize the SCENE by applying its transform and then calling
;  normalize-graphic on each of its children.
;  Nothing of value is returned.

(defmethod normalize-graphic :before ((scene scene))

  (with-slots ((parent-transform transform) elements) scene
    (WHEN (/= (FILL-POINTER elements) 0)                    ;ignore if no elements
      (DOTIMES (position (length elements))
	(LET ((graphic (aref elements position)))
	  (with-slots ((child-transform transform)) graphic
	    (setf child-transform
		  (compose-transform child-transform
				     parent-transform
				     child-transform)))
	  (normalize-graphic graphic))))))





;Method: extent-compute
;  Compute the extent rectangle for the SCENE.  This is done by combining
;  the extents of each of the elements in SCENE and then applying the
;  SCENE's transform to the resulting extent.

(DEFMETHOD extent-compute ((scene scene))

  (with-slots (elements) scene
    (IF (/= (FILL-POINTER elements) 0) ; Is this an empty scene?
	(let ((first-child-extent ; No, get the first child's extent
	       (DOTIMES (position (length elements)) ; For all the other children.
		 (WHEN (valid-extent-p (graphic-extent (aref elements position)))
		   (RETURN (graphic-extent (aref elements position)) )))
	       ))
	  (if first-child-extent 		; Is it defined?
	      (let ((temp-extent		; Yes, make a copy in a temp
		     (extent-copy first-child-extent (make-extent-rect))))
		(DOTIMES (position (length elements)) ; For all the other children...
		  (WHEN (valid-extent-p (graphic-extent (aref elements position)))
		    (extent-combine ;  Combine their extents
		     (graphic-extent (aref elements position))
		     temp-extent)))
		(extent-transform 
		 (graphic-transform scene) ; Apply our transform to the result
		 temp-extent
		 temp-extent)
		temp-extent)
	      ))
	#-cmu (make-extent-rect :xmin 0 :ymin 0 :xmax 0 :ymax 0)
	#+cmu (make-extent-rect :valid t))))

  
;Method: scene-delete
;  If POS is 0 or nil, the first or last object in the SCENE is
;  deleted.  If POS is a graphic, then that graphic is deleted.
;  The deleted graphic is returned with its parent slot set to nil.

(defmethod scene-delete ((scene scene) pos)
  (declare (type scene-position pos))

  (extent-changed scene)
  (with-slots (elements) scene			;from a scene
    (when elements
      (let ((dead-graphic			; Remember who was killed
	     (if (null pos)
		 (UNLESS (ZEROP (FILL-POINTER elements))
		   (LET ((graphic (AREF elements (1- (length elements)))))
		     (SETF (FILL-POINTER elements)	; Delete last graphic
			   (1- (FILL-POINTER elements)))
		     graphic))
		 (LET* ((realpos (OR (POSITION pos elements) pos))
			(graphic (WHEN (NUMBERP realpos)
				   (AREF elements realpos))))
		   (WHEN (NUMBERP realpos)
		     ; Delete the given graphic or position
		     (DO ((position (1+ realpos)(1+ position)))
			 ((eq position (length elements)) nil)
		       (SETF (aref elements (1- position))
			     (aref elements position))) 
		     (SETF (FILL-POINTER elements)(1- (FILL-POINTER elements)))
		     graphic)))
		))
	(when dead-graphic			; If we killed something,
	  (graphic-stack-purge
	   *transform-stack* dead-graphic)	; Notify the transform stack
          (graphic-stack-purge
	   *gstate-stack* dead-graphic)
	  (graphic-stack-purge
	   *edge-gstate-stack* dead-graphic)	; Notify the gstate stack
	  ; Clear its parent slot
          (setf (slot-value dead-graphic 'parent) nil))

        dead-graphic))))			; Return the dead guy


;Method: scene-elements
;  Return the list of elements contained by SCENE.

(defmethod scene-elements ((scene scene))

  (slot-value scene 'elements))

(DEFMETHOD (SETF scene-elements) (element-seq (scene scene))
  (with-slots (elements) scene
    (SETF (FILL-POINTER elements) 0)
    (WHEN element-seq
      (DOTIMES (pos (LENGTH element-seq))
	(scene-insert scene (ELT element-seq pos))
	)))
  element-seq)

;Method: scene-insert
;  Inserts the GRAPHIC at the given POSITION in the SCENE.  If POSITION is
;  a graphic, then GRAPHIC is inserted immediately after it.  If POSITION
;  is :FIRST or :LAST, then GRAPHIC is inserted at the beginning or the end
;  of the elements list respectively.  The parent slot of GRAPHIC is
;  changed to point to SCENE and the GRAPHIC is returned.

(defmethod scene-insert ((scene scene) (graphic graphic) &optional pos )
  (declare (type scene-position pos))
  (declare (optimize (safety 3)))

  ;;(format t "scene-insert ~a ~a ~a~%" scene graphic pos)
  (extent-changed scene)
  (WHEN (graphic-parent graphic)
    ;; if there is a parent remove from the elements list of that parent
    (scene-delete (graphic-parent graphic) graphic))
  (SETF (graphic-gstate graphic) (combine-into (graphic-combined-gstate scene)
					       (graphic-gstate graphic)))
  (with-slots (elements parent) scene		;to a scene
    (if (not pos)
	(VECTOR-PUSH-EXTEND graphic elements 5)	;insert after last graphic
	(LET* ((realpos  (OR (POSITION pos elements) pos )))
	  (WHEN  (NUMBERP realpos)
	    ;make sure the vector is long enough
	    (VECTOR-PUSH-EXTEND 0 elements 5)
	    ;; insert at position of the given graphic or position
	    (DO ((position (1- (length elements)) (1- position)))
		((eq position realpos) nil)
	      (SETF (aref elements position)(aref elements (- position 1)))) 
	    (SETF (aref elements realpos) graphic)
	    ))))

  (setf (slot-value graphic 'parent) scene)	; Set its new parent
  graphic)					; Return the inserted graphic


(defmethod scene-insert ((scene scene) (graphic scene) &optional pos )
  (declare (type scene-position pos))

  ;(format t "scene-insert ~a ~a ~a~%" scene graphic pos)
  (extent-changed scene)
  (UNLESS (EQL   scene graphic)
    (WHEN (graphic-parent graphic)
      ;if there is a parent remove from the elements list of that parent
      ;;(format t "deleting ~a from ~a~%" graphic (graphic-parent graphic))
      (scene-delete (graphic-parent graphic) graphic))

    #+nil
    (format t "combining ~a ~a~%" 
	    (graphic-combined-gstate scene)(graphic-gstate graphic))
    (SETF (graphic-gstate graphic) (combine-into (graphic-combined-gstate scene)
						 (graphic-gstate graphic)))
    (with-vector temp-elements
      (copy-to-vector (scene-elements graphic) temp-elements)
      (setf (scene-elements graphic) temp-elements))
    (with-slots (elements parent) scene		;to a scene
      ;;(format t "  ~a ~a ~a~%" elements parent pos)
      (if (not pos)
	  ;; insert after last graphic
	  (VECTOR-PUSH-EXTEND graphic elements 5)
	  (LET* ((realpos  (OR (POSITION pos elements) pos )))
	    (WHEN  (NUMBERP realpos)
	      ;; make sure the vector is long enough
	      (VECTOR-PUSH-EXTEND 0 elements 5)
	      ;; insert at position of the given graphic or position
	      (DO ((position (1- (length elements)) (1- position)))
		  ((eq position realpos) nil)
		(SETF (aref elements position)(aref elements (- position 1)))) 
	      (SETF (aref elements realpos) graphic)
	      ))))
    ;; Set its new parent
    (setf (slot-value graphic 'parent) scene))
  ;; Return the inserted graphic
  graphic)




;Method: scene-reparent
;  Move each of the ELEMENTS into the SCENE.
;  Return the new parent scene.

(defmethod scene-reparent ((scene scene) new-parent &rest elements)
  (declare (type list elements))
  (extent-changed scene)
  (with-vector children
	       (copy-to-vector (scene-elements scene) children)
	       (MAP nil #'(lambda (child)
			    (scene-insert new-parent child)) children))
  (dolist (child elements)			; Go through the list
    (scene-insert new-parent child))		; Add it to the new scene
  scene)				; Return the scene now containing graphic


;Method: scene-ungroup
;  Reparent all elements of SCENE to the parent of SCENE.
;  Delete SCENE from its parent.

(defmethod scene-ungroup ((scene scene))
  (extent-changed scene)
  (with-slots (elements) scene
    (let ((grand-parent (graphic-parent scene)))
      (if grand-parent				; Is there a grand parent?
          (prog1
            (scene-reparent scene grand-parent ) ; Yes, make it the parent
            (scene-delete grand-parent scene))	;  and delete the scene
	  (with-vector children
	       (copy-to-vector (scene-elements scene) children)
	       (MAP nil #'(lambda (child) (scene-delete scene child)) children))
	  ))))		;  i.e., they have no parent


(DEFMETHOD scene-graphic ((scene scene) position)
  (with-slots (elements) scene
    (WHEN (< position (FILL-POINTER elements))
      (AREF elements position))))


(DEFMETHOD scene-restack ((scene scene) old-position new-position)

  (LET ((graphic (scene-delete scene old-position)))
    (scene-insert scene graphic new-position)))

