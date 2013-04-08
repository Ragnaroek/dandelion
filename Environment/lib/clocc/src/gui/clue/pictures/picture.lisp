;;;-*- Mode:Lisp; Package:PICTURES; Fonts:(CPTFONT HL12B HL12BI HL12BI CPTFONTB); Base:10 -*-
;;;	*
;;; Description:	*
;;;	*	Picture objects allow graphic objects to be arranged in hierarchical structures.	*
;;;	*	A graphic is a "child" of a picture (its "parent") if the graphic is a	*
;;;	*	member of the picture's list of elements. A picture is an "ancestor" of a	*
;;;	*	graphic (its "descendant") if it is the graphic's parent or an ancestor of its	*
;;;	*	parent. A "top-level" graphic is one whose parent is nil.	*

;;;	*	Read-access to the list of picture elements is given by the picture-elements	*
;;;	*	method.  Modification of the list of picture elements is supported by the	*
;;;	*	picture-insert/delete methods. These methods are responsible for reporting the	*
;;;	*	appropriate damaged regions to any view containing the picture.	*
;;;	*
;;; History:	*
;;;	*	8/24/89	*	  Jim Dutton	*	Created	*
;;;	*	9/05/89	*	  Jim Dutton	*	Added graphic-normalize method	*
;;;	*	9/14/89	*	  Jim Dutton      Reversed order of compose in graphic-normalize	*
;;;	*	9/19/89	*	  Jim Dutton	*	Changed graphic-draw to only draw children	*
;;;	*				that intersect the optional limit rectangle.	*
;;;	*				
;;; 	*
;;; 	*			  RESTRICTED RIGHTS LEGEND 	*
;;; 	*
;;; Use, duplication, or disclosure by the Government is subject to restrictions as  set	*
;;; forth in  subdivision  (b)(3)(ii)  of  the  Rights  in  Technical  Data and Computer	*
;;; Software clause at 52.227-7013. 	*
;;; 	*
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 149149 AUSTIN, TEXAS 78714-9149	*
;;; Copyright (C) 1989, Texas Instruments Incorporated.  All rights reserved.	*
;;; 	*

(in-package "	2PICTURES	*")
(USE-PACKAGE  '(common-lisp clos))


;Picture Class Definition:	*

(defclass 	4picture 	*(extent-cache graphic)
  (
   (elements		:type		list
                        :initarg	:elements
                        :initform	nil
                        :documentation	"	3The graphic objects in the picture(mR)	*")

   (views		:type		list
			:initform	nil
                        :documentation	"	3The views to which the picture is attached(mR)	*")
   )
  (:documentation "	3Base class for composite graphical objects	*"))


;Type: picture-position	*
(deftype 	4picture-position 	*() '(or graphic (member :first :last)))


;Function: make-picture	*
;  Return a new picture object with no elements.	*

(defun 	4make-picture	* (&rest initargs
		     &key
		     &allow-other-keys)

  (apply #'make-instance 'picture initargs))


;Method: graphic-draw	*
;  Draw the PICTURE object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and HEIGHT	*
;  are given, then only parts of the object that lie within the given rectangle	*
;  need to be drawn.	*
;  For pictures, just draw each of the child graphics.	*

(defmethod 	4graphic-draw	* ((picture picture) (view view)
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))

  (with-slots (elements sensitivity) picture
    (world-transform picture)				; Cache our transform	*
    (combined-gstate picture)                           ; Cache our gstate	*
    (UNLESS (EQUAL sensitivity :hidden)
      (if (and min-x min-y width height)			; Was optional rect is given	*
	  (dolist (child elements)		; Yes, conditionally draw children	*
	    (when (graphic-intersects-p child min-x min-y width height)
	      (graphic-draw child view min-x min-y width height)))
	  (dolist (child elements)		; No, unconditionally draw children	*
	    (graphic-draw child view))))))


;Method: graphic-normalize	*
;  Normalize the PICTURE by applying its transform and then calling	*
;  graphic-normalize on each of its children.	*
;  Nothing of value is returned.	*

(defmethod 	4graphic-normalize 	*:before ((picture picture))

  (with-slots ((parent-transform transform) elements) picture
    (dolist (child elements)
      (with-slots ((child-transform transform)) child
        (setf child-transform
              (compose-transform child-transform
                                 parent-transform
                                 child-transform)))
      (graphic-normalize child))))


;Method: graphic-views	*
;  Return a list of all the views to which the GRAPHIC is attached.	*

(defmethod 	4graphic-views 	*((picture picture))

  (with-slots (views) picture
    views))


;Method: extent-compute	*
;  Compute the extent rectangle for the PICTURE.  This is done by combining the extents	*
;  of each of the elements in PICTURE and then applying the PICTURE's transform to the	*
;  resulting extent.	*

(defmethod 	4extent-compute 	*((picture picture))

  (with-slots (elements) picture
    (when elements					; Is this an empty picture?	*
      (let ((first-child-extent				; No, get the first child's extent	*
              (extent-rectangle (first elements))))
        (when first-child-extent			; Is it defined?	*
          (let ((temp-extent				; Yes, make a copy in a temp	*
                  (extent-copy first-child-extent
                               (make-extent-rect))))
            (dolist (child (cdr elements))		; For all the other children...	*
              (extent-combine (extent-rectangle child)	;  Combine their extents	*
                              temp-extent))
            (extent-transform (graphic-transform picture)	; Apply our transform to the result	*
                              temp-extent
                              temp-extent)
            temp-extent))))))				; Return the computed extent	*

  
;Method: picture-delete	*
;  Removes the graphic at the given POSition from the PICTURE.	*
;  If POS is :FIRST or :LAST, the first or last object in the PICTURE is	*
;  deleted.  If POS is a graphic, then that graphic is deleted.	*
;  The deleted graphic is returned with its parent slot set to nil.	*

(defmethod 	4picture-delete	* ((picture picture) pos)
  (declare (type picture-position pos))

  (with-slots (elements) picture
    (when elements
      (let ((dead-graphic					; Remember who was killed	*
              (case pos
                (:first	(pop elements))				; Delete first graphic on list	*
                (:last  (prog1 (car (last elements))		; Delete last graphic	*
                               (nbutlast elements)))
                (t	(progn (setf elements	    		; Delete the given graphic	*
                                     (delete pos elements :test 'eq))
                               pos)))))
        (when dead-graphic					; If we killed something,	*
          (graphic-stack-purge *transform-stack* dead-graphic)	; Notify the transform stack	*
          (graphic-stack-purge *gstate-stack* dead-graphic)	; Notify the gstate stack	*
          (setf (graphic-parent dead-graphic) nil))		; Clear its parent slot	*

        dead-graphic))))					; Return the dead guy	*


;Method: picture-elements	*
;  Return the list of elements contained by PICTURE.	*

(defmethod 	4picture-elements	* ((picture picture))

  (slot-value picture 'elements))


;Function: picture-group	*
;  Create a new picture and reparent the given elements to it.	*
;  Return the new picture.	*

(defun 	4picture-group	* (&rest elements)
  (declare (type list elements))

  (picture-reparent (make-picture) elements))		; Reparent all elements using a new picture	*


;Method: picture-insert	*
;  Inserts the GRAPHIC at the given POSITION in the PICTURE.  If POSITION is a	*
;  graphic, then GRAPHIC is inserted immediately after it.  If POSITION is :FIRST	*
;  or :LAST, then GRAPHIC is inserted at the beginning or the end of the elements	*
;  list respectively.  The parent slot of GRAPHIC is changed to point to PICTURE	*
;  and the GRAPHIC is returned.	*

(defmethod 	4picture-insert	* ((picture picture) graphic &optional (pos :last))
  (declare (type picture-position pos))

  (with-slots (elements) picture
    (case pos
      (:first (push graphic elements))				; Push graphic onto front of list	*
      (:last  (if elements					; Glue graphic onto the end	*
                  (rplacd (last elements) (list graphic))
                  (push graphic elements)))
      (t      (let ((pos-n (position pos elements)))		; Insert after pos on the list	*
                (if pos-n
		    (LET ((a (nthcdr (+ pos-n 1) elements)))
		      (push graphic a))
                    (error "	3Graphic not found in picture	*"))))))

  (setf (graphic-parent graphic) picture)			; Set its new parent	*
  graphic)							; Return the inserted graphic	*


;Method: picture-reparent	*
;  Move each of the ELEMENTS into the PICTURE.	*
;  Return the new parent picture.	*

(defmethod 	4picture-reparent	* ((picture picture) elements)
  (declare (type list elements))

  (dolist (child elements)				; Go through the list	*
    (when (graphic-parent child)			; If graphic has a parent	*
      (picture-delete (graphic-parent child) child))	;   remove it from its parent's list	*
    (picture-insert picture child))			; Add it to the new picture	*
  picture)						; Return the picture now containing graphic	*


;Method: picture-restack	*
;  For the given PICTURE, delete the graphic in OLD-POSITION and re-insert it	*
;  in NEW-POSITION.	*

(defmethod 	4picture-restack	* ((picture picture) old-position new-position)
  (declare (type picture-position old-position new-position))

  (picture-insert picture (picture-delete picture old-position) new-position))


;Method: picture-ungroup	*
;  Reparent all elements of PICTURE to the parent of PICTURE.	*
;  Delete PICTURE from its parent.	*

(defmethod 	4picture-ungroup	* ((picture picture))

  (with-slots (elements) picture
    (let ((grand-parent (graphic-parent picture)))
      (if grand-parent					; Is there a grand parent?	*
          (prog1
            (picture-reparent grand-parent elements)	; Yes, make it the parent	*
            (picture-delete grand-parent picture))	;  and delete the picture	*
          (dolist (child elements)			; No, make them all independent	*
            (picture-delete picture child))))))		;  i.e., they have no parent








