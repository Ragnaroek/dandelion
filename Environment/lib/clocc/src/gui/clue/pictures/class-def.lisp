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

;;; Graphic class definition

(defclass graphic ()
  ((gstate
    :type	(or null gstate)
    :initarg	:gstate
    :initform	nil
    :documentation	"The graphic's visual attributes (R)")
   
   (parent
    :type	(or null graphic)
    :initarg	:parent
    :initform	nil
    :documentation
    "The graphic's parent picture, if one exists (R)")

   (plist
    :type	list
    :accessor	graphic-plist
    :initarg	:plist
    :initform	nil
    :documentation
    "A list of graphic properties")
   
   (sensitivity
    :type	(member :hidden :viewable :selectable :editable :subselectable)
    :initarg	:sensitivity
    :initform	:editable
    :reader      graphic-sensitivity
    :documentation
    "Controls state transitions")
   
   (transform
    :type	(or null transform)
    :initarg	:transform
    :reader      graphic-transform
    :initform	nil
    :documentation
    "The graphic's rotation and translation in the coordinate system  (R)")

   (views
    :type	list
    :initform	nil
    :documentation
    "The views to which the graphic is attached(mR)")
   )
  (:documentation "Base class for graphical objects"))

(defmethod initialize-instance :after ((graphic graphic)
				       &rest initargs
				       &key parent)
  (declare (ignore initargs))
  (when parent
    (scene-insert parent graphic)))

(deftype scene-position () '(or graphic (integer 1 *) NULL))

(defclass scene (edge extent-cache graphic )
  ((elements
    :type	array
    :initform	(make-array '(10) :adjustable t :fill-pointer 0)
    :documentation
    "The graphic objects in the scene(mR)")
   )
  (:documentation "Base class for composite graphical objects"))

;;view class definition

(defcontact view (composite)
  ((plist
    :type	list
    :initarg	:plist
    :initform	nil
    :documentation
    "A list of view properties")

   (button-release-p
    :type    (OR nil t)
    :initform nil
    :accessor button-release-p
    :documentation
    "toggle action for release button event")
   
   (damage-count
    :type       card8
    :initform   0
    :accessor	view-damage-count
    :documentation
    "Number of active damage rectangles. Zero means no damage.")

   (damage
    :type       vector
    :initform	(make-array (list max-damage)
			    :element-type 'extent-rect)
    :documentation
    "Damage rectangles for a view")

   (default-gcontext
     :type	(OR null gcontext)
     :documentation
     "Default gcontext,  X window visual attributes,  for a view")
   
   (gcontext-cache
    :type	array
    :accessor	view-gcontext-cache
    :initform	(MAKE-ARRAY 10 :adjustable t :fill-pointer 0)
    :documentation
    "Working gcontext, X window Visual attributes,  for a view")
  
   (gravity
    :type	(member :northwest :north  :northeast :west :center
			:east :southwest :south  :southeast)
    :initarg	:view-gravity
    :initform	:southwest
    :documentation
    "The fixed point of the world coordinate system after resize")
   
   (origin-x
    :type	wcoord
    :initarg	:origin-x
    :reader	origin-x
    :initform	0s0
    :documentation
    "World x-coordinate of the southwest corner of the view")

   (origin-y
    :type	wcoord
    :initarg	:origin-y
    :reader	origin-y
    :initform	0s0
    :documentation
    "World y-coordinate of the southwest corner of the view")

   (highlight-color
    :type	pixel
    :initarg	:highlight-color
    :accessor	view-highlight-color   
    :initform	1
    :documentation
    "define the highlight color of a view of the view")

   (view-graphic
    :type	(or null graphic)
    :initarg	:graphic
    :initform	nil
    :accessor	view-graphic
    :documentation
    "The graphic object to be displayed(R)")

   (resize-extent-p
    :type	:boolean
    :accessor	view-resize-extent-p
    :initarg	:resize-extent-p
    :initform	t
    :documentation
    "If true, resizing will change the world coordinate extent;
     If false, resizing will cause a zoom effect.")

   (scale-x
    :type	wcoord
    :initarg	:scale-x
    :initform	1s0
    :accessor	view-scale-x
    :documentation
    "Number of pixels per world coordinate unit")

   (scale-y
    :type	wcoord
    :initarg	:scale-y
    :initform	1s0
    :accessor	view-scale-y
    :documentation
    "Number of pixels per world coordinate unit")

   (selection 
    :type	scene
    :accessor	view-selection-scene
    :documentation
    "Selected graphics in a view(mR)")

   (grabber-rect-transform
    :type	transform
    :accessor	grabber-rect-transform
    :documentation
    "the transform of the grabber-rect - inverse of view transform")
   ) ; end of slot defs
  (:resources		; Turn on exposure events for this composite
   (event-mask :initform #.(make-event-mask :exposure)))
  (:documentation "A CLUE contact for viewing a picture"))


(defun graphic-events-enabled-p (view)
  (not (GETF (slot-value view 'plist) :ignore-graphic-events)))

(DEFUN set-graphic-events-enabled-p (view boolean)
  (IF boolean
      (SETF (GETF (slot-value view 'plist) :ignore-graphic-events) nil)
      (SETF (GETF (slot-value view 'plist) :ignore-graphic-events) t))
   boolean)

(defsetf graphic-events-enabled-p set-graphic-events-enabled-p)














