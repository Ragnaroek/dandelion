;;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
;;;                                                                                  |
;;;             Copyright (C) 1989, 1990 Texas Instruments Incorporated.             |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

(in-package "CLIO-OPEN")

(export '(
	  gravity
	  *default-display-bottom-margin*
	  *default-display-left-margin*
	  *default-display-right-margin*
	  *default-display-top-margin*	  
	  
	  display-bottom-margin
	  display-gravity
	  display-left-margin
	  display-right-margin
	  display-top-margin
	  )
	'clio-open)

(deftype gravity ()
  '(member :north-west :north :north-east
	   :east :center :west
	   :south-east :south :south-west))

(defparameter *default-display-bottom-margin* 0
  "The default size of the bottom margin, in points.")

(defparameter *default-display-left-margin* 0
  "The default size of the left margin, in points.")

(defparameter *default-display-right-margin* 0
  "The default size of the right margin, in points.")

(defparameter *default-display-top-margin* 0
  "The default size of the top margin, in points.")

;; Special types to support conversion of resource defaults to pixel units
(deftype default-bottom-margin () 'card16)
(deftype default-left-margin   () 'card16)
(deftype default-right-margin  () 'card16)
(deftype default-top-margin    () 'card16)

(defmethod convert ((contact contact) (value (eql :default)) (type (eql 'default-bottom-margin)))
  (point-pixels (contact-screen contact) *default-display-bottom-margin*))

(defmethod convert ((contact contact) (value (eql :default)) (type (eql 'default-left-margin)))
  (point-pixels (contact-screen contact) *default-display-left-margin*))

(defmethod convert ((contact contact) (value (eql :default)) (type (eql 'default-right-margin)))
  (point-pixels (contact-screen contact) *default-display-right-margin*))

(defmethod convert ((contact contact) (value (eql :default)) (type (eql 'default-top-margin)))
  (point-pixels (contact-screen contact) *default-display-top-margin*))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 gravity-mixin                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact gravity-mixin ()
  ((bottom-margin    :type           default-bottom-margin 
		     :initarg	     :bottom-margin
		     :reader	     display-bottom-margin)	       ; setf defined below
   (right-margin     :type           default-right-margin 
		     :initarg	     :right-margin
		     :reader	     display-right-margin)	       ; setf defined below
   (gravity          :type           (or (member :tiled) gravity)      ; setf defined below
		     :initform       :center
		     :initarg        :display-gravity
		     :reader         display-gravity)
   (clip-rectangle   :type           array
		     :initform       (make-array 4 :element-type 'integer)))
  (:resources
    (display-gravity :type (or (member :tiled) gravity)
		     :initform :center)
    (bottom-margin :initform :default)
    (right-margin  :initform :default)
    (left-margin   :type     default-left-margin
		   :initform :default)
    (top-margin    :type     default-top-margin
		   :initform :default))
  
  (:documentation  "Provides margin and gravity resources for core contacts."))



(defmacro display-clip-x (contact)
  `(svref (slot-value ,contact 'clip-rectangle) 0))

(defmacro display-clip-y (contact)
  `(svref (slot-value ,contact 'clip-rectangle) 1))

(defmacro display-clip-width (contact)
  `(svref (slot-value ,contact 'clip-rectangle) 2))

(defmacro display-clip-height (contact)
  `(svref (slot-value ,contact 'clip-rectangle) 3))

(flet
  ((update-clip-rectangle
     (contact)
     (with-slots (clip-rectangle right-margin bottom-margin width height) (the gravity-mixin contact)
       (setf
	 (display-clip-width  contact) (max 0 (- width right-margin (display-clip-x contact)))
	 (display-clip-height contact) (max 0 (- height bottom-margin (display-clip-y contact))))))

   (update-bit-gravity
     (contact)
     (with-slots (gravity) (the gravity-mixin contact)
       (setf
	 (window-bit-gravity contact)
	 
	 (cond
	   ;; If display-gravity is at a corner or margins are equal, then
	   ;; bit-gravity can equal display-gravity; this minimizes exposure on resize.
	   ;; Otherwise, must use bit-gravity :forget and redisplay.
	   ((case gravity
	       ((:north :south)
		(/= (display-left-margin contact) (display-right-margin contact)))
		
		((:west :east)
		 (/= (display-top-margin contact) (display-bottom-margin contact)))
		
		(:center
		 (or (/= (display-left-margin contact) (display-right-margin contact))
		     (/= (display-top-margin contact) (display-bottom-margin contact)))))
	     :forget)

	   ;; :tiled display-gravity is a special case...
	   ((eq gravity :tiled)
	    :north-west)

	   (t
	     gravity))))))
  
  
  (defmethod initialize-instance :after ((contact gravity-mixin)
					 &key top-margin left-margin (display-gravity :center)
					 &allow-other-keys)
    (assert (or (typep display-gravity 'gravity) (eq display-gravity :tiled))
	    () "~s is not :tiled or a gravity"
	    display-gravity)
    (setf (display-clip-x contact) left-margin
	  (display-clip-y contact) top-margin)
    (update-clip-rectangle contact))
  
  
  (defmethod (setf display-bottom-margin) (new-value (contact gravity-mixin))
    (with-slots (bottom-margin) contact
      (let ((new-value (if (eq new-value :default)
			   (convert contact new-value 'default-bottom-margin)
			   new-value)))
	(check-type new-value card16)
	(setf bottom-margin new-value)
	(update-clip-rectangle contact)
	(when (realized-p contact)
	  (update-bit-gravity contact)
	  (clear-area contact :exposures-p t))
	new-value)))
  
  (defmethod (setf display-right-margin) (new-value (contact gravity-mixin))
    (with-slots (right-margin) contact
      (let ((new-value (if (eq new-value :default)
			   (convert contact new-value 'default-right-margin)
			   new-value)))
	(check-type new-value card16)
	(setf right-margin new-value)
	(update-clip-rectangle contact)
	(when (realized-p contact)
	  (update-bit-gravity contact)
	  (clear-area contact :exposures-p t))
	new-value)))
  
  
  (defmethod (setf display-left-margin) (new-value (contact gravity-mixin))
    (with-slots (clip-rectangle) contact
      (let ((new-value (if (eq new-value :default)
			   (convert contact new-value 'default-left-margin)
			   new-value)))
	(check-type new-value card16)
	(setf (display-clip-x contact) new-value)
	(update-clip-rectangle contact)
	(when (realized-p contact)
	  (update-bit-gravity contact)
	  (clear-area contact :exposures-p t))
	new-value)))
  
  (defmethod display-left-margin ((contact gravity-mixin))
    (display-clip-x contact))
  
  
  (defmethod (setf display-top-margin) (new-value (contact gravity-mixin))
    (with-slots (clip-rectangle) contact
      (let ((new-value (if (eq new-value :default)
			   (convert contact new-value 'default-top-margin)
			   new-value)))
	(check-type new-value card16)
	(setf (display-clip-y contact) new-value)
	(update-clip-rectangle contact)
	(when (realized-p contact)
	  (update-bit-gravity contact)
	  (clear-area contact :exposures-p t))
	new-value)))
  
  (defmethod display-top-margin ((contact gravity-mixin))
    (display-clip-y contact))
  
  
  (defmethod resize :after ((contact gravity-mixin) width height border-width)
    (declare (ignore width height border-width))
    (update-clip-rectangle contact))

  (defmethod (setf display-gravity) :after (new-value (contact gravity-mixin))
    (declare (ignore new-value))
    (when (realized-p contact)
      (update-bit-gravity contact)
      (clear-area contact :exposures-p t)))

  (defmethod realize :after ((contact gravity-mixin))
    (update-bit-gravity contact)))

(defmethod (setf display-gravity) (new-value (contact gravity-mixin))
  (check-type new-value gravity)
  (setf (slot-value contact 'gravity) new-value))




(defmethod preferred-size :around ((contact gravity-mixin) &key width height border-width)
  (let
    ((tm (display-top-margin contact))
     (bm (display-bottom-margin contact))
     (lm (display-left-margin contact))
     (rm (display-right-margin contact)))

    ;; Get preferred-size with margins subtracted from suggested size, then add
    ;; margins back in to get preferred size including margins.
    (multiple-value-bind (pw ph pbw)
	(call-next-method
	  contact
	  :width        (max 0 (- (or width (contact-width contact)) lm rm))
	  :height       (max 0 (- (or height (contact-height contact)) tm bm))
	  :border-width (or border-width (contact-border-width contact)))
      
      (values
	(+ pw lm rm)
	(+ ph tm bm)
	pbw))))