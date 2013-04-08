;; -*- MODE:LISP; Package:CLIO-OPEN; Base:10; Lowercase:T; Fonts:(CPTFONT); Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                                AUSTIN, TEXAS 78714                               |
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

(proclaim '(special *default-display-text-font*)) ;; defined in display-text.lisp

(EXPORT '(
	  action-button
	  button-font
	  button-label
	  button-label-alignment
	  button-switch
	  choice-item-highlight-default-p
	  choice-item-font
	  choice-item-highlight-selected-p
	  choice-item-label
	  choice-item-selected-p
	  make-action-button
	  make-action-item
	  make-toggle-button
	  action-item
	  toggle-button
	  )
	'clio-open)

;;; =================================================================================== ;;;
;;;											;;;
;;;	 C o n s t a n t s   a n d   S t r u c t u r e s   f o r   B u t t o n s	;;;
;;;											;;;
;;; =================================================================================== ;;;

(DEFUN create-filled-in-circle-image (circle-image)

  (LET ((width (image-width circle-image))
	(height (image-height circle-image))
	(circle-pixarray (image-z-pixarray circle-image))
	filled-in-pixarray next-pixel)

    (SETF filled-in-pixarray (make-array `(,height ,width) :element-type 'bit))

    (DO ((row 0 (1+ row)) first-1-pixel last-1-pixel)
	((= row height))

      ;;  Copy pixels upto the first 1-pixel found scanning from the left...
      (DO ((col 0 (1+ col)))
	  ((= col width)			; If no 1-pixels found, claim one was
	   (SETF first-1-pixel col))		;   found off right-edge of array.
	(SETF next-pixel (AREF circle-pixarray row col))
	(SETF (AREF filled-in-pixarray row col) next-pixel)
	(WHEN (= next-pixel 1)
	  (SETF first-1-pixel col)
	  (RETURN)))

      ;;  Copy pixels upto the first 1-pixel found scanning from the right...
      (DO ((col (1- width) (1- col)))
	  ((<= col first-1-pixel)		; If no 1-pixels found, use the one
	   (SETF last-1-pixel col))		;   found in left-to-right scan.
	(SETF next-pixel (AREF circle-pixarray row col))
	(SETF (AREF filled-in-pixarray row col) next-pixel)
	(WHEN (= next-pixel 1)
	  (SETF last-1-pixel col)
	  (RETURN)))

      ;;  Fill in the pixels between these two 1-pixels...
      (DO ((col (1+ first-1-pixel) (1+ col)))
	  ((>= col last-1-pixel))
	(SETF (AREF filled-in-pixarray row col) 1)))

    (create-image :width width
		  :height height
		  :data filled-in-pixarray)))


(DEFSTRUCT (button-descriptor (:conc-name "") (:type vector))
  ab-button-ends-image
  ab-clearing-stencil-image
  ab-default-ring-image
  ab-body-clearing-stencil-image
  ab-horizontal-menu-mark-image
  ab-vertical-menu-mark-image
  ab-height
  ab-default-ring-height
  ab-left-button-end-width
  ab-right-button-end-width
  ab-text-baseline				; from top of button.
  tb-min-right-margin
  ab-menu-mark-bottom-rel-to-baseline		; this includes -1 to compensate for height of
						;    menu mark.
  ab-clearing-stencil-array			; pointer to pixarray of clearing-stencil-image.
  ai-default-ring-image
  ai-body-clearing-stencil-image
  ai-height
  ai-default-ring-height
  ai-button-end-width
  ai-text-baseline
  )

;;;
;;;   A structure of this type is the value fo the :OL-button-pixmaps property of the display
;;;   plist.  It is created (if it doesn't already exist for the display) and accessed by the
;;;   function get-button-pixmaps.
;;;
(DEFSTRUCT (button-pixmaps (:conc-name ""))
  ab-button-ends-pixmap
  ab-clearing-stencil-pixmap
  ab-default-ring-pixmap
  ab-body-clearing-stencil-pixmap
  ai-default-ring-pixmap
  ai-body-clearing-stencil-pixmap
  horizontal-menu-mark-pixmap
  vertical-menu-mark-pixmap
  )

(DEFPARAMETER *button-dimensions-by-scale*
  `(:small
       ,(make-button-descriptor
	     :ab-button-ends-image		small-action-button-ends
	     :ab-clearing-stencil-image		(create-filled-in-circle-image
						   small-action-button-ends)
	     :ab-default-ring-image		small-action-button-default-ring
	     :ab-body-clearing-stencil-image	(create-filled-in-circle-image
						   small-action-button-default-ring)
	     :ab-height				18
	     :ab-default-ring-height		13
	     :ab-left-button-end-width		8
	     :ab-right-button-end-width		9
	     :ab-text-baseline			11
	     :tb-min-right-margin		7
	     :ab-horizontal-menu-mark-image	small-horizontal-menu-mark
	     :ab-vertical-menu-mark-image	small-vertical-menu-mark
	     :ab-menu-mark-bottom-rel-to-baseline -1
	     :ai-default-ring-image		small-action-item-default-ring
	     :ai-body-clearing-stencil-image	(create-filled-in-circle-image
						   small-action-item-default-ring)
	     :ai-height				17
	     :ai-default-ring-height		16
	     :ai-button-end-width		8
	     :ai-text-baseline			10
	     )
    :medium
       ,(make-button-descriptor
	     :ab-button-ends-image		medium-action-button-ends
	     :ab-clearing-stencil-image		(create-filled-in-circle-image
						   medium-action-button-ends)
	     :ab-default-ring-image		medium-action-button-default-ring
	     :ab-body-clearing-stencil-image	(create-filled-in-circle-image
						   medium-action-button-default-ring)
	     :ab-height				20
	     :ab-default-ring-height		15
	     :ab-left-button-end-width		9
	     :ab-right-button-end-width		10
	     :ab-text-baseline			12
	     :tb-min-right-margin		8
	     :ab-horizontal-menu-mark-image	medium-horizontal-menu-mark
	     :ab-vertical-menu-mark-image	medium-vertical-menu-mark
	     :ab-menu-mark-bottom-rel-to-baseline -1
	     :ai-default-ring-image		medium-action-item-default-ring
	     :ai-body-clearing-stencil-image	(create-filled-in-circle-image
						   medium-action-item-default-ring)
	     :ai-height				19
	     :ai-default-ring-height		18
	     :ai-button-end-width		9
	     :ai-text-baseline			13
	     )
    :large
       ,(make-button-descriptor
	     :ab-button-ends-image		large-action-button-ends
	     :ab-clearing-stencil-image		(create-filled-in-circle-image
						   large-action-button-ends)
	     :ab-default-ring-image		large-action-button-default-ring
	     :ab-body-clearing-stencil-image	(create-filled-in-circle-image
						   large-action-button-default-ring)
	     :ab-height				22
	     :ab-default-ring-height		17
	     :ab-left-button-end-width		11
	     :ab-right-button-end-width		12
	     :ab-text-baseline			14
	     :tb-min-right-margin		10
	     :ab-horizontal-menu-mark-image	large-horizontal-menu-mark
	     :ab-vertical-menu-mark-image	large-vertical-menu-mark
	     :ab-menu-mark-bottom-rel-to-baseline -2
	     :ai-default-ring-image		large-action-item-default-ring
	     :ai-body-clearing-stencil-image	(create-filled-in-circle-image
						   large-action-item-default-ring)
	     :ai-height				21
	     :ai-default-ring-height		20
	     :ai-button-end-width		10
	     :ai-text-baseline			13
	     )
    :extra-large
       ,(make-button-descriptor
	     :ab-button-ends-image		extra-large-action-button-ends
	     :ab-clearing-stencil-image		(create-filled-in-circle-image
						   extra-large-action-button-ends)
	     :ab-default-ring-image		extra-large-action-button-default-ring
	     :ab-body-clearing-stencil-image	(create-filled-in-circle-image
						   extra-large-action-button-default-ring)
	     :ab-height				28
	     :ab-default-ring-height		23
	     :ab-left-button-end-width		13
	     :ab-right-button-end-width		14
	     :ab-text-baseline			18
	     :tb-min-right-margin		12
	     :ab-horizontal-menu-mark-image	extra-large-horizontal-menu-mark
	     :ab-vertical-menu-mark-image	extra-large-vertical-menu-mark
	     :ab-menu-mark-bottom-rel-to-baseline -2
	     :ai-default-ring-image		extra-large-action-item-default-ring
	     :ai-body-clearing-stencil-image	(create-filled-in-circle-image
						   extra-large-action-item-default-ring)
	     :ai-height				25
	     :ai-default-ring-height		23
	     :ai-button-end-width		14
	     :ai-text-baseline			16
	     )))

;;;
;;;   Set the clear-stencil-array slot of each button-descriptor...
;;;
(EVAL-WHEN (LOAD eval)
  (DOLIST (button-dims *button-dimensions-by-scale*)
    (UNLESS (SYMBOLP button-dims)		; skip the property names...
      (SETF (ab-clearing-stencil-array button-dims)
	    (image-z-pixarray (ab-clearing-stencil-image button-dims))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Label-String                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(deftype label-string () 'string)

(defmethod convert (contact value (type (eql 'label-string)))
  (declare (ignore contact))
  (when (or (symbolp value) (stringp value))
    (stringable-label value)))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Button                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defcontact button (core contact)

  ((font 	:type 		fontable
	 	:reader         button-font			       ; setf defined below
		:initarg	:font
	 	:initform 	*default-display-text-font*)
   
   (label 	:type 		(or pixmap label-string)
	  	:reader   	button-label			       ; setf defined below
		:initarg	:label
	  	:initform 	"")

   (label-alignment
     		:type		(member :left :center :right)
		:accessor	button-label-alignment
		:initarg	:label-alignment
		:initform	:left)

   (compress-exposures
                :initform       :off
		:type           (member :off :on)		
		:reader         contact-compress-exposures
		:allocation     :class)

   (fill-color 	:type		pixel)

   (highlight-default-p
                :type	        boolean
     		:initform	nil
		:reader   	choice-item-highlight-default-p)       ; setf defined below

   ;; Selected slot values:
   ;;     1:   unselected
   ;;     2:   selected,
   ;;     -n:  select has been pressed, receipt of a release select event
   ;;          will complement the selected state
   (selected	:type		integer
		:initform	1)

   (last-displayed-as
     		:type		(member :highlighted :unhighlighted)
		:initform	:unhighlighted)

   (preferred-width
     		:type		(or null integer)
	       	:initform	nil))

  (:resources
    label
    label-alignment
    font))


(DEFGENERIC display-button-highlighted (button &optional x))
(DEFGENERIC display-button-unhighlighted (button &optional x))






;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Accessors                                      |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(DEFMETHOD (SETF button-label) (new-label (button button))
  (with-slots (label parent preferred-width width height border-width) button

    (let ((converted-label (convert button new-label '(or pixmap label-string))))
      (assert converted-label
	      () "Label ~s is not a stringable, pixmap, or image." new-label)
      (setf label converted-label))
    (SETF preferred-width NIL)	;Note - This forces recalculation of preferred values.

    (if (= 0 width)
	;; The *first* time we must initialize geometry
	(multiple-value-setq (width height)
	  (preferred-size button))
	;; Otherwise we change-geometry to reflect new size
	(when (realized-p button)
	  ;; We defer the change-geometry if button not realized since
	  ;; change-layout will be called when it is realized.
	  (multiple-value-bind (new-width new-height)
	      (preferred-size button)
	    ;; We don't invoke change-geometry unless size actually changed.
	    (unless    
	      (and (= width new-width) (= height new-height))
	      (change-geometry button :width new-width :height new-height :accept-p t)))))
    label))


(defmethod (setf button-font) (new-font (button button))
  (check-type new-font fontable) 
  (with-slots (font label) button
    (setf font (find-font button new-font))
    
    ;; Save original fontname requested. Used again when changing scale.
    (setf (getf (window-plist button) 'fontname) new-font)
    
    (when label
      (setf (button-label button) label)))
  new-font)


(defmethod (setf button-label-alignment) :before (new-alignment (button button))
  (check-type new-alignment (member :left :center :right) "(MEMBER :LEFT :CENTER :RIGHT)"))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Initialization                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+
 
(defmethod initialize-instance :after ((button button) &key &allow-other-keys)
  (with-slots (label font name fill-color border-width) button
   
    ;;  Initialize font for current scale
    (setf (button-font button) font)
    
    (UNLESS (resource button :name)
      (SETF name (stringable-keyword label)))

    ;; Initialize fill color
    (setf fill-color (contact-current-background-pixel button))

    (SETF border-width 0)

    (MULTIPLE-VALUE-BIND (p-w p-h p-b-w)
	(preferred-size button)
      (change-geometry button :height p-h :width p-w :border-width p-b-w :accept-p t))))


(defmethod rescale :before ((self button))
  ;; Find font for new scale, using original fontname requested.
  (setf (button-font self) (getf (window-plist self) 'fontname)))


;;; =================================================================================== ;;;
;;;											;;;
;;;		     C h o i c e   P r o t o c o l   M e t h o d s			;;;
;;;											;;;
;;; =================================================================================== ;;;

(defmethod (setf choice-item-highlight-default-p) (new-value (button button))
  (with-slots (highlight-default-p) button
    (let ((new-value (when new-value t)))
      (unless (eq new-value highlight-default-p)
	(setf highlight-default-p new-value)
	(redisplay-button button))))
  new-value)

(DEFMETHOD choice-item-font ((button button))
  (button-font button))


(DEFMETHOD (SETF choice-item-font) (new-value (button button))
  (SETF (button-font button) new-value))


(DEFMETHOD choice-item-label ((button button))
  (button-label button))


(defmethod choice-item-highlight-selected-p ((button button))
  (with-slots (last-displayed-as) button
    (eq last-displayed-as :highlighted)))


(defmethod (setf choice-item-highlight-selected-p) (new-value (button button))
  (let ((highlight-selected-p (choice-item-highlight-selected-p button))
	(new-value            (when new-value t)))
    (unless (eq highlight-selected-p new-value)
      (if new-value
	  (display-button-highlighted button)
	  (display-button-unhighlighted button))))
  new-value)


(defmethod choice-item-selected-p ((button button))
  (with-slots (selected) button
    (= (abs selected) 2)))


(defmethod (setf choice-item-selected-p) (new-value (button button))
  (let ((new-value (when new-value t)))
    (unless (eq new-value (choice-item-selected-p button))
      (with-slots (selected) button 
	(setf selected (if new-value 2 1))
	(setf (choice-item-highlight-selected-p button) new-value)
	(apply-callback button (if new-value :on :off)))))
  new-value)





;;; =================================================================================== ;;;
;;;											;;;
;;;	   U t i l i t y   F u n c t i o n s    F o r   A l l   B u t t o n s		;;;
;;;											;;;
;;; =================================================================================== ;;;

(DEFEVENT button
	  (:button-press :button-1)
  	  press-select)

(DEFEVENT button
	  (:button-release :button-1)
  	  release-select)

(DEFMETHOD redisplay-button ((button button) &optional completely-p)
  (with-slots (last-displayed-as) button
    (CASE last-displayed-as
      (:unhighlighted (display-button-unhighlighted button completely-p))
      (:highlighted   (display-button-highlighted   button completely-p)))))


(DEFMETHOD display ((button button) &optional at-x at-y at-width at-height &key)
  (DECLARE (IGNORE at-x at-y at-width at-height))
  (WHEN (realized-p button)
    ;;  Put self on the display afresh, completely redrawing everything...
    (redisplay-button button t)))


;;;
;;;   This function is used when a menu button must display the label of its menu's default
;;;   choice, which may not fit within the menu button.  It has more general usage than this,
;;;   should be moved to utilities.lisp.
;;;



(DEFUN get-button-pixmaps (button)
  
  ;;
  ;;  Look on the display's plist for an :OL-button-pixmaps property.  If any action
  ;;  button has created pixmaps from its images, they'll be here...
  ;;
  (LET* ((scale (contact-scale button))
	 (display (contact-display button))
	 (button-pixmaps (GETF (display-plist display) :OL-button-pixmaps))
	 (button-pixmaps-for-this-size-button (GETF button-pixmaps scale))
	 (dims (GETF *button-dimensions-by-scale* scale)))
    
    ;;
    ;;  If there are no pixmaps cached on the display's plist for this scale action button,
    ;;  create some, put them into a button-pixmaps structure, then put it on the display's plist...
    ;;
    (UNLESS button-pixmaps-for-this-size-button
      (SETF button-pixmaps-for-this-size-button
	    (SETF (GETF button-pixmaps scale)
		  (make-button-pixmaps
		    :ab-button-ends-pixmap
		    (image-pixmap button (ab-button-ends-image dims))
		    :ab-clearing-stencil-pixmap
		    (image-pixmap button (ab-clearing-stencil-image dims))
		    :ab-default-ring-pixmap
		    (image-pixmap button (ab-default-ring-image dims))
		    :ab-body-clearing-stencil-pixmap
		    (image-pixmap button (ab-body-clearing-stencil-image dims))
		    :ai-default-ring-pixmap
		    (image-pixmap button (ai-default-ring-image dims))
		    :ai-body-clearing-stencil-pixmap
		    (image-pixmap button (ai-body-clearing-stencil-image dims))
		    :horizontal-menu-mark-pixmap
		    (image-pixmap button (ab-horizontal-menu-mark-image dims))
		    :vertical-menu-mark-pixmap
		    (image-pixmap button (ab-vertical-menu-mark-image dims)))))
      (SETF (GETF (display-plist display) :OL-button-pixmaps) button-pixmaps))
    
    ;;
    ;;  Return the button-pixmaps structure containing the pixmaps for this button's scale...
    ;;
    button-pixmaps-for-this-size-button))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                           Toggle Button                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(defcontact toggle-button (button)
  ((pointer-pressed   :type      boolean
		      :initform  nil))
  (:resources
    (border-width :initform 0)
    
    (switch       :type (member :on :off)
		  :initform :off)))

(defun toggle-button-release-menu (self)
   (declare (type toggle-button self))
   (with-slots (pointer-pressed) self
     (when pointer-pressed
       (choice-item-release self)
       (setq pointer-pressed nil))))

(defun toggle-button-leave-with-menu-pressed (self)
   (declare (type toggle-button self))
   (with-slots (pointer-pressed) self
     (with-event (mode)
       (when (and pointer-pressed
		  (eq mode :normal))
	 (choice-item-leave self)
	 (setq pointer-pressed nil)))))

(DEFMETHOD toggle-button-enter-with-menu-pressed ((self toggle-button))
   (with-event (x y state)
     (when (and (inside-contact-p self x y)
		(NOT (ZEROP (LOGAND #.(make-state-mask :button-3) state))))
       ;; The pointer has been dragged over this button w/menu button
       ;; pressed. This has the same side effects as pressing the
       ;; select button so we go ahead and use the press procedure
       ;; to take care of visuals and approve the transition.
       (when (choice-item-press self)
	 ;; Transition was approved and button is now highlighted.
	 ;; We set a flag so :button-release and :leave-notify events
	 ;; will be handled.
	 (with-slots (pointer-pressed) self
	   (setq pointer-pressed t))))))


(DEFEVENT toggle-button
	  :enter-notify
   toggle-button-enter-with-menu-pressed)

(defevent toggle-button
	  :leave-notify
   toggle-button-leave-with-menu-pressed)

(defevent toggle-button
	  (:button-release :button-1)
   tb-maybe-release-select)

;;  These two translations are for Open Look menus, which allow item selection
;;  on both button-1 and button-3 presses.
(DEFEVENT toggle-button
	  (:button-press :button-3)
   press-select)

(DEFEVENT toggle-button
	  (:button-release :button-3)
   toggle-button-release-menu)

(defun tb-maybe-release-select (button)
   (with-slots (pointer-pressed) (the toggle-button button)
     (when pointer-pressed
       (release-select button))))


(defun make-toggle-button (&rest initargs)
  (apply #'make-contact 'toggle-button initargs))




(defmethod initialize-instance :after ((toggle-button toggle-button)
				       &key switch &allow-other-keys)
  (with-slots (selected) toggle-button

    (when (eq switch :on)
	(setf selected 2)
	  (display-button-highlighted toggle-button))))


;;; ========================================================================== ;;;
;;;									       ;;;
;;;        ( T o g g l e )   B u t t o n   P r o t o c o l   M e t h o d s     ;;;
;;;									       ;;;
;;; ========================================================================== ;;;

(defmethod button-switch ((toggle-button toggle-button))
  (with-slots (selected) toggle-button
    (if (= 1 (abs selected)) :off :on)))

(DEFMETHOD (SETF button-switch) (new-state (toggle-button toggle-button))
  (ASSERT (member new-state '( :on :off)) nil
	  "~a is an illegal button state.  Must be :ON or :OFF." new-state)
  (LET ((current-state (button-switch toggle-button)))
    (WHEN (NOT (EQ current-state new-state))
      ;; We simulate a button press and release to implement identical
      ;; semantics whether done via API or via gesture.
      (WHEN (choice-item-press toggle-button)
	;; When toggle press succeeded we follow it
	;; with a release.
	(choice-item-release toggle-button)))
    (button-switch toggle-button)))

(DEFMETHOD leave ((toggle-button toggle-button))
  (with-event (state mode)
    (when (eq mode :normal)
      (with-slots (selected pointer-pressed) toggle-button
	(WHEN (AND (< selected 0)
		   (NOT (ZEROP (LOGAND (make-state-mask :button-1) state))))
	  (choice-item-leave toggle-button)
	  (setq pointer-pressed nil))))))


(DEFMETHOD preferred-size ((toggle-button toggle-button) &key width height border-width)
    (declare (ignore width height border-width))

  ;;  A toggle-button must draw its border within its window so it can be dimmed if the button
  ;;  becomes insensitive.  So its border-width is zero.
  ;;  Its preferred height is that dictated by its scale slot.
  ;;  Its preferred width is the width of its label plus the right/left margins plus the border
  ;;  width.

  (with-slots (label font preferred-width) toggle-button
    (LET*
      ((scale (contact-scale toggle-button))
       (dims (GETF *button-dimensions-by-scale* scale))
       p-width)

      ;;  Since an Action Button's min-right-margin is 2 more than the interior margin a
      ;;  Toggle Button should have, and since the border width of a Toggle Button is 1, we can
      ;;  just use the Action Button's min-right-margin...
      (SETF p-width (OR preferred-width
			(SETF preferred-width
			      (+ (label-width toggle-button label)
				 (tb-min-right-margin dims) (tb-min-right-margin dims)))))

      ;;  Since an Action Button's height is exactly that of a Toggle Button...

      (VALUES p-width
	      (ab-height dims)
	      0))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Choice Item Protocol                            |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod choice-item-press ((toggle-button toggle-button))
  (with-slots (selected) toggle-button
    (let ((to-selected-p (= selected 1)))
      (when (apply-callback-else (toggle-button :change-allowed-p to-selected-p) t)
	(setf selected (- selected))
	(if to-selected-p
	    (display-button-highlighted toggle-button)
	    (display-button-unhighlighted toggle-button))
	(apply-callback toggle-button :changing to-selected-p)
	t))))

(defmethod choice-item-release ((toggle-button toggle-button))
  (with-slots (selected) toggle-button
    (apply-callback toggle-button (IF (= 2 (SETF selected (+ 3 selected))) :on :off))))

(DEFMETHOD choice-item-leave ((toggle-button toggle-button))
  (with-slots (selected) toggle-button
    (IF (= 2 (SETF selected (- selected)))
	(PROGN
	  (display-button-highlighted toggle-button)
	  (apply-callback toggle-button :canceling-change NIL))
	(PROGN
	  (display-button-unhighlighted toggle-button)
	  (apply-callback toggle-button :canceling-change T)))))

(DEFMETHOD press-select ((toggle-button toggle-button))
   (WHEN (choice-item-press toggle-button)
     (with-slots (pointer-pressed) toggle-button
       (setq pointer-pressed t))))

(DEFMETHOD release-select ((toggle-button toggle-button))
  (with-event (state)
    (with-slots (selected pointer-pressed) toggle-button
      (WHEN (> 0 selected)
	(UNWIND-PROTECT 
	    (choice-item-release toggle-button)
	  (setq pointer-pressed nil))))))


(DEFMETHOD (SETF choice-item-selected-p) (new-value (toggle-button toggle-button))
  ;; Identical to (SETF button-switch) except returns boolean on/off indicator.
  (EQ (SETF (button-switch toggle-button) (if new-value :on :off)) :on))




;;; =================================================================================== ;;;
;;;											;;;
;;;	            The Two Ways to Display a Toggle Button...				;;;
;;;											;;;
;;; =================================================================================== ;;;

(DEFUN display-toggle-button (toggle-button mode &optional completely-p)
  (declare (type toggle-button toggle-button))
  (with-slots (font fill-color foreground highlight-default-p width height)
	      toggle-button 
    (WHEN (realized-p toggle-button)
      (LET ((tb-foreground foreground) (tb-fill-color fill-color) (tb-font font)
	    (tb-width width) (tb-height height)
	    stroke-width two-stroke-widths four-stroke-widths
	    (sensitive-p (sensitive-p toggle-button)))
	
	(SETF stroke-width 1
	      two-stroke-widths (* 2 stroke-width)
	      four-stroke-widths (* 2 two-stroke-widths))
	(using-gcontext (gc
			  :drawable 	toggle-button
			  :foreground 	tb-foreground
			  :background 	tb-fill-color
			  :font 	tb-font
			  :line-width	stroke-width
			  :fill-style	(IF sensitive-p :solid :stippled)
			  :stipple	(UNLESS sensitive-p
					  (contact-image-mask toggle-button 50%gray :depth 1)))
	  
	  (WHEN completely-p
	    (clear-area toggle-button
			:x 0
			:y 0
			:width tb-width
			:height tb-height)

	    ;;  Draw our rectangular OL UI border...
	    (DOTIMES (i stroke-width)
		 (draw-rectangle toggle-button gc i i
				 (- tb-width 1 i i)
				 (- tb-height 1 i i)))

	    (display-button-label toggle-button gc))


	  ;;  Draw/erase the highlight indicator...
	  (flet
	    ((draw/erase-default-indicator ()
	       (DOTIMES (i stroke-width)
		 (draw-rectangle toggle-button gc (+ two-stroke-widths i) (+ two-stroke-widths i)
				 (- tb-width four-stroke-widths 1 i i)
				 (- tb-height four-stroke-widths 1 i i))))				   
	     (draw/erase-highlight ()
	       (DOTIMES (i stroke-width)
		 (draw-rectangle toggle-button gc (+ stroke-width i) (+ stroke-width i)
				 (- tb-width two-stroke-widths 1 i i)
				 (- tb-height two-stroke-widths 1 i i))))
	     )
#+ansi-common-lisp (declare (inline draw/erase-default-indicator draw/erase-highlight))
	    ;;  Draw the default indicator if necessary...
	    (if highlight-default-p
	        (draw/erase-default-indicator)
		(with-gcontext (gc :foreground tb-fill-color :background tb-foreground)
		  (draw/erase-default-indicator)))
	    (IF (EQ mode :unhighlighted)
		(with-gcontext (gc :foreground tb-fill-color :background tb-foreground)
		  (draw/erase-highlight))
		(draw/erase-highlight)))

	  )))))


(DEFMETHOD display-button-highlighted ((toggle-button toggle-button) &optional completely-p)
  (with-slots (last-displayed-as) toggle-button
    (display-toggle-button toggle-button :highlighted completely-p)
    (SETF last-displayed-as :highlighted)))


(DEFMETHOD display-button-unhighlighted ((toggle-button toggle-button) &optional completely-p)
  (with-slots (last-displayed-as) toggle-button
    (display-toggle-button toggle-button :unhighlighted completely-p)
    (SETF last-displayed-as :unhighlighted)))



;;; =================================================================================== ;;;
;;;											;;;
;;;	      D i s p l a y   a   T o g g l e   B u t t o n ' s   L a b e l		;;;
;;;											;;;
;;; =================================================================================== ;;;

(DEFMETHOD display-button-label ((self toggle-button) gc)
  (display-any-buttons-label self gc 1 -2))

(defgeneric label-width (button label)
  (:documentation "Return the width of the button LABEL in pixels."))

(defmethod label-width ((button button) (label string))
  (with-slots (font) button
    (text-width font label)))

(defmethod label-width ((button button) (label pixmap)) 
  (or (getf (pixmap-plist label) :width)
      (with-state (label)
	(setf (getf (pixmap-plist label) :width)  (drawable-width label)
	      (getf (pixmap-plist label) :height) (drawable-height label)))))
 
(defun display-any-buttons-label (button gc top-border-thickness left-border-adjustment)
  (with-slots (label label-alignment width height) (the button button)
    (let*
      ((dims        (getf *button-dimensions-by-scale* (contact-scale button)))
       (label-width (label-width button label))
       (margin      (- (ab-left-button-end-width dims) left-border-adjustment)) 
       (left-margin (max margin
			 (case label-alignment
			   (:left   0)
			   (:center (pixel-round (- width label-width) 2))
			   (:right  (- width margin label-width)))))) 
      
      (if (stringp label)
	  (draw-glyphs
	    button gc
	    left-margin (+ top-border-thickness (ab-text-baseline dims))
	    label)
	  
	  ;; Else display pixmap label...
	  (let ((label-height (getf (pixmap-plist label) :height)))
	    (with-gcontext (gc :fill-style :tiled :tile label)
	      (draw-rectangle
		button gc
		left-margin (max 0 (pixel-round (- height label-height) 2))
		label-width label-height t)))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                           Action Button                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defcontact action-button (button) ()
  (:resources (border-width :initform 0)))

(defun make-action-button (&rest initargs)
  (apply #'make-contact 'action-button initargs))


(DEFUN circular-list-of-one-item (item)
  "Return a circular list whose elements are ITEM (over and over again)."
  (LET ((tem (LIST item)))
    (RPLACD tem tem)
    TEM))


(defmethod choice-item-press ((action-button action-button))
  
  ;; choice-item-press does the necessary tasks to reflect
  ;; an action-button press provided that the :change-allowed-p
  ;; callback (if any) allows the state change.  The returned
  ;; value indicates whether the press was allowed or not.
  
  (when (apply-callback-else (action-button :change-allowed-p t) t)
    (display-button-highlighted action-button)
    (apply-callback action-button :press)
    (apply-callback action-button :changing t)
    t))

(DEFMETHOD choice-item-release ((action-button action-button))
  
  ;; choice-item-release does the necessary tasks to reflect
  ;; an action-button release.  It is assumed that a press has
  ;; occurred and that the press action was allowed; thus, we
  ;; don't invoke the :change-allowed-p callback again here.

  (with-slots (selected) action-button			     
    (display-action-button-busy action-button)
    (display-force-output (contact-display action-button))

    ;; Ensure highlight is cleaned up in case :release callback performs a throw.
    (unwind-protect
	(apply-callback action-button :release)
      (SETF selected 2)
      (apply-callback action-button :on)
      (SETF selected 1)
      (display-button-unhighlighted action-button)
      (apply-callback action-button :changing NIL)
      (apply-callback action-button :off))))

(DEFMETHOD choice-item-leave ((action-button action-button))
  (display-button-unhighlighted action-button)
  (apply-callback action-button :canceling-change T))

(DEFMETHOD press-select ((action-button action-button))
  (with-event (x y)
    (WHEN (inside-contact-p action-button x y)
      ;;  Choice-item-press will set last-displayed-as if the
      ;;  transition is allowed.
      (choice-item-press action-button))))

(DEFMETHOD release-select ((action-button action-button))
  (with-slots (last-displayed-as) action-button
    ;;  Do nothing unless highlighted/selected already...
    (WHEN (EQ last-displayed-as :highlighted)
      (choice-item-release action-button))))


(DEFMETHOD (SETF choice-item-selected-p) (new-value (action-button action-button))
  (with-slots (last-displayed-as) action-button
    ;; For an unselected action button and a new-value of T, this method must act like a button
    ;; press followed immediately by a button release.  If the button is already
    ;; selected, this method does nothing.  Note that to prevent strange behavior if the
    ;; application calls us with a new-value of T from within the action-button's :release
    ;; callback, we do not check the button's selected-p slot.  Instead, we check the button's
    ;; last-displayed-as slot, only doing something if the button is completely inactive.
    (WHEN (and new-value (EQ last-displayed-as :unhighlighted))
      (WHEN (choice-item-press action-button)
	;; When press action was allowed we proceed with
	;; ersatz release.
	(choice-item-release action-button)))
    
    ;; else the application is trying to unselect an action button.  This is meaningful only when
    ;; the action button is selected, which is a momentary state for an action button.  A
    ;; "selected" action button by definition is in the process of transitioning to "unselected".
    ;; As a part of this transition all callbacks will be applied.  So in this case it seems
    ;; reasonable for the method to do nothing
    
    new-value))


(DEFMETHOD leave ((action-button action-button))
  (with-event (state mode)
    (when (eq mode :normal)
      (with-slots (last-displayed-as) action-button
	(WHEN (AND (EQ last-displayed-as :highlighted)
		   (NOT (ZEROP (LOGAND (make-state-mask :button-1) state))))
	  (choice-item-leave action-button))))))

(defevent action-button
	  :leave-notify
   leave)



;;;
;;;	The three basic ways to display an action button...
;;;

(DEFMETHOD redisplay-button ((action-button action-button) &optional completely-p)
  (with-slots (last-displayed-as) action-button
    (CASE last-displayed-as
      (:unhighlighted (display-button-unhighlighted action-button completely-p))
      (:highlighted   (display-button-highlighted   action-button completely-p))
      (:busy	    (display-action-button-busy   action-button completely-p)))))
 
(DEFMETHOD display-button-unhighlighted ((action-button action-button) &optional completely-p)
  (with-slots (font fill-color foreground highlight-default-p last-displayed-as) action-button
    
    (when (realized-p action-button)
      (LET ((ab-foreground foreground) (ab-fill-color fill-color) (ab-font font)
	    (sensitive-p (sensitive-p action-button)))
	
	;;  If displaying a dimmed (insensitive) button, always redraw the entire thing...
	(UNLESS sensitive-p
	  (SETF completely-p t))
	
	(using-gcontext (gc
			  :drawable 	action-button
			  :foreground 	ab-foreground
			  :background 	ab-fill-color
			  :font		ab-font
			  :fill-style	(IF sensitive-p :solid :stippled)
			  :stipple	(UNLESS sensitive-p
					  (contact-image-mask action-button 50%gray :depth 1)))
	  
	  (with-gcontext (gc :foreground ab-fill-color :background ab-foreground)
	    (IF completely-p
		(clear-button-and-display-border action-button gc)
		(just-clear-body-of-button action-button gc)))
	  
	  (display-button-label action-button gc)
	  
	  (WHEN highlight-default-p
	    (display-default-indicator action-button gc)))))
	  
    (SETF last-displayed-as :unhighlighted)))


(DEFMETHOD display-button-highlighted ((action-button action-button) &optional completely-p)

  (with-slots (font fill-color foreground last-displayed-as) action-button
    
    (when (realized-p action-button)
      (LET ((ab-foreground foreground) (ab-fill-color fill-color) (ab-font font))
	
	;;  An insensitive action button can never be busy, so sensitive-p is not checked
	;;  or handled here...
	(using-gcontext (gc
			  :drawable 	action-button
			  :foreground 	ab-fill-color
			  :background 	ab-foreground
			  :font 	ab-font)
	  
	  (with-gcontext (gc :foreground ab-foreground :background ab-fill-color)
	    (IF completely-p
		(clear-button-and-display-border action-button gc)
		(just-clear-body-of-button action-button gc)))
	  
	  (display-button-label action-button gc))))
    (SETF last-displayed-as :highlighted)))



(defmethod display-action-button-busy ((action-button action-button) &optional completely-p)  
  (with-slots (font fill-color foreground last-displayed-as) action-button
    
    (when (realized-p action-button)
      (let ((ab-foreground foreground) (ab-fill-color fill-color) (ab-font font))
	
	;;  An insensitive action button can never be busy, so sensitive-p is not checked
	;;  or handled here...
	 
	  ;;  Clear out the non-margin, non-border part of the button with the busy-pixmap
	  ;;  stipple pattern...
	  (using-gcontext
	    (gc
	      :drawable   action-button
	      :foreground ab-fill-color
	      :background ab-foreground
	      :stipple    (contact-image-mask action-button 88%gray :depth 1)
	      :fill-style :opaque-stippled)
	    (if completely-p
		(clear-button-and-display-border action-button gc)
		(just-clear-body-of-button action-button gc)))

	  ;;  Draw the text label in the foreground color...
	  (using-gcontext
	    (gc
	      :drawable 	action-button
	      :foreground 	ab-foreground
	      :background 	ab-fill-color
	      :font 		ab-font) 
	  (display-button-label action-button gc))))
    
    (setf last-displayed-as :busy)))


(DEFMETHOD display-default-indicator ((action-button action-button) gc)
  ;;  Draws the 1-2 pixel wide default indicator in the foreground color of GC...
  (with-slots (width height) action-button
    (LET* ((scale (contact-scale action-button)) interior-width
	   (dims (GETF *button-dimensions-by-scale* scale))
	   (top-border-thickness (IF (TYPEP action-button 'action-item) 0 1))
	   (button-pixmaps (get-button-pixmaps action-button)))
      
      (SETF interior-width
	    (- width (ab-left-button-end-width dims) (ab-right-button-end-width dims)))

      ;;  Draw the left-end of the default indicator...
      (with-gcontext (gc :clip-x 0 :clip-y top-border-thickness
			 :clip-mask (ab-default-ring-pixmap button-pixmaps))
	(draw-rectangle action-button gc
			0 top-border-thickness
			(ab-left-button-end-width dims) height t))

      ;;  Draw the top horizontal line of the default indicator...
      (draw-rectangle action-button gc
		      (ab-left-button-end-width dims)
		      (+ top-border-thickness 1)
		      interior-width 0)

      ;;  Draw the bttom horizontal line of the default indicator...
      (draw-rectangle action-button gc
		      (ab-left-button-end-width dims)
		      (+ top-border-thickness (ab-default-ring-height dims))
		      interior-width 0)

      ;;  Draw the right-end of the default indicator...
      (with-gcontext (gc :clip-x interior-width :clip-y top-border-thickness
			 :clip-mask (ab-default-ring-pixmap button-pixmaps))
	(draw-rectangle action-button gc
			(+ (ab-left-button-end-width dims) interior-width)
			top-border-thickness
			(ab-right-button-end-width dims) (contact-height action-button) t)))))


(DEFMETHOD just-clear-body-of-button ((action-button action-button) gc)
  (with-slots (width height) action-button
    (LET* ((scale (contact-scale action-button))
	   interior-width body-clear-stencil
	   (dims (GETF *button-dimensions-by-scale* scale))
	   (top-border-thickness (IF (TYPEP action-button 'action-item) 0 1))
	   (button-pixmaps (get-button-pixmaps action-button))
	   (fill-style (gcontext-fill-style gc)))

      (SETF interior-width
	    (- width (ab-left-button-end-width dims) (ab-right-button-end-width dims)))
      
      (when (< interior-width 0)
	(setq interior-width 0))

      (SETF body-clear-stencil (ab-body-clearing-stencil-pixmap button-pixmaps))

      (with-gcontext (gc :fill-style (IF (EQ fill-style :stippled) :solid fill-style))
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;   Clear out the button's non-border, non-margin pixels to
	;;	the foreground color of GC...
	
	;;   Start by clearing the left end of the button...
	(with-gcontext (gc :clip-x 0 :clip-y top-border-thickness
			   :clip-mask body-clear-stencil)
	  (draw-rectangle action-button gc 0 top-border-thickness
			  (ab-left-button-end-width dims) (contact-height action-button) t))
	
	;;   Clear out the background for the label...
	(draw-rectangle action-button gc
			(ab-left-button-end-width dims)
			(+ top-border-thickness 1)
			interior-width (ab-default-ring-height dims) t)
	
	;;   Clear out the drawable pixels of the right button end...
	(with-gcontext (gc :clip-x interior-width
			   :clip-y top-border-thickness
			   :clip-mask body-clear-stencil)
	  (draw-rectangle action-button gc
			  (+ (ab-left-button-end-width dims) interior-width)
			  top-border-thickness
			  (ab-right-button-end-width dims) height t))))))


(DEFMETHOD clear-button-and-display-border ((action-button action-button) gc)
  (with-slots (foreground fill-color width height) action-button
    (LET* ((scale (contact-scale action-button))
	   (ab-fill-color fill-color) (ab-foreground foreground) interior-width
	   clear-stencil border-stencil
	   (dims (GETF *button-dimensions-by-scale* scale))
	   (button-pixmaps (get-button-pixmaps action-button))
	   (fill-style (gcontext-fill-style gc)))

      (SETF interior-width
	    (- width (ab-left-button-end-width dims) (ab-right-button-end-width dims)))
      
      (when (< interior-width 0)
	(setq interior-width 0))

      (SETF clear-stencil (ab-clearing-stencil-pixmap button-pixmaps)
	    border-stencil (ab-button-ends-pixmap button-pixmaps))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;   Clear out all the button's pixels to fill-color...
      
      (with-gcontext (gc :fill-style (IF (EQ fill-style :stippled) :solid fill-style))
	
	;;   Start by clearing the left end of the button...
	(with-gcontext (gc :clip-x 0 :clip-y 0 :clip-mask clear-stencil)
	  (draw-rectangle action-button gc 0 0
			  (ab-left-button-end-width dims) height t))
	;;   Clear out the background for the label...
	(draw-rectangle action-button gc
			(ab-left-button-end-width dims) 0
			interior-width height t)
	;;   Clear out the drawable pixels of the right button end...
	(with-gcontext (gc :clip-x interior-width :clip-y 0 :clip-mask clear-stencil)
	  (draw-rectangle action-button gc
			  (+ (ab-left-button-end-width dims) interior-width) 0
			  (ab-right-button-end-width dims) height t)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;   Draw in the button's border in foreground...
      (with-gcontext (gc :foreground ab-foreground :background ab-fill-color
			 :fill-style (IF (EQ fill-style :opaque-stippled) :solid fill-style))
	
	;;   Start by drawing the border on the left end of the button...
	(with-gcontext (gc :clip-x 0 :clip-y 0 :clip-mask border-stencil)
	  (draw-rectangle action-button gc 0 0
			  (ab-left-button-end-width dims)
			  height t))
	
	;;   Draw the top and bottom borders for the label...
	(draw-rectangle action-button gc
			(ab-left-button-end-width dims) 0 interior-width 0)
	
	(draw-rectangle action-button gc
			(ab-left-button-end-width dims) (- (ab-height dims) 2)
			interior-width 1)
	
	;;   Finish by drawing the border on the right end of the button...
	(with-gcontext (gc :clip-x interior-width :clip-y 0 :clip-mask border-stencil)
	  (draw-rectangle action-button gc
			  (+ (ab-left-button-end-width dims) interior-width) 0
			  (ab-right-button-end-width dims) height t)))
      )))


(DEFMETHOD display-button-label ((self action-button) gc)
  (display-any-buttons-label self gc 1 0))


(DEFMETHOD preferred-size ((action-button action-button) &key width height border-width)
    (declare (ignore width height border-width))

  ;;  An action button always wants a border-width of zero.
  ;;  Its preferred height is that dictated by its scale slot.
  ;;  Given a text label, its preferred width is the width of its label in the font
  ;;    corresponding to the scale plus the widths of its button ends.
  ;;  Given an image or pixmap label, use its width.

  (with-slots (label font preferred-width width height) action-button
    (LET* ((scale (contact-scale action-button))
	  (dims (GETF *button-dimensions-by-scale* scale))
	  p-width p-height)

      (SETF p-width
	    (OR preferred-width
		(SETF preferred-width (+ (ab-left-button-end-width dims)
					 (ab-right-button-end-width dims)
					 (label-width action-button label)))))
      
      (SETF p-height (ab-height dims))

      (VALUES p-width (ab-height dims) 0))))


(DEFMETHOD inside-contact-p ((action-button action-button) x y)
  "Returns T iff the point (X,Y) is within the rounded borders of ACTION-BUTTON."
  (with-slots (width height) action-button
    (LET* ((scale (contact-scale action-button))
	   (dims (GETF *button-dimensions-by-scale* scale)))
      (AND (< -1 x width) (< -1 y height)
	   (OR (<= (ab-left-button-end-width dims)
		   x
		   (- width (ab-right-button-end-width dims) 1))
	       (LET* ((clearing-stencil-array (ab-clearing-stencil-array dims)))	    
		 (WHEN (> x (ab-left-button-end-width dims))
		   (DECF x (- width (ab-left-button-end-width dims) (ab-right-button-end-width dims))))
		 (NOT (ZEROP (AREF clearing-stencil-array x y)))))))))





;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Action Item                                     |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

;;; An ACTION-ITEM is a specialization of an ACTION-BUTTON and is intended for use
;;; in OL compliant menus.  It differs from an ACTION-BUTTON in appearance as well
;;; as in its sensitivity to various mouse gestures depending on the mode of the
;;; menu which contains it.

(defcontact action-item (action-button) ()
  (:resources
    (label-alignment :initform :left)))

(defun action-item-release-menu (self)
   (declare (type action-item self))
   (with-slots (last-displayed-as) self
     (when (eq last-displayed-as :highlighted)
       (choice-item-release self))))

(defun action-item-leave-with-menu-pressed (self)
  (declare (type action-item self))
  (with-slots (last-displayed-as) self
    (with-event (mode)
      (unless (eq mode :grab)
	(when (eq last-displayed-as :highlighted)
	  (choice-item-leave self))))))

(defun action-item-enter-with-menu-pressed (self)
  (with-slots (last-displayed-as) self
    (when (eq last-displayed-as :unhighlighted)
      (with-event (x y state)
	(when (and (inside-contact-p self x y)
		   (not (zerop (logand #.(make-state-mask :button-3) state))))
	  ;; The pointer has been dragged over this button w/menu button
	  ;; pressed. This has the same side effects as pressing the
	  ;; select button so we go ahead and use the press procedure
	  ;; to take care of visuals and approve the transition.
	  ;; The last-displayed-as slot is set inside choice-item-press
	  ;; if the transition is approved.
	  (choice-item-press self))))))

(DEFEVENT action-item
	  :enter-notify
   action-item-enter-with-menu-pressed)

(defevent action-item
	  :leave-notify
   action-item-leave-with-menu-pressed)

(defevent action-item
	  (:button-release :button-3)
   action-item-release-menu)

;;  This translation is for Open Look menus, which allow item selection
;;  on both button-1 and button-3 presses.
(DEFEVENT action-item
	  (:button-press :button-3)
   press-select)

(defun make-action-item (&rest initargs)
  (apply #'make-contact 'action-item initargs))


;;;
;;;	New drawing methods for an action-item...
;;;

(DEFMETHOD inside-contact-p ((self action-item) x y)
  "Returns T iff the point (X,Y) is within an action-item."
  (with-slots (width height) self
    (AND (< -1 x width) (< -1 y height))))


(defmethod display-button-label ((self action-item) gc)
  (with-slots (label label-alignment font width height) self
    (let*
      ((label-width (label-width self label))
       (dims        (getf *button-dimensions-by-scale* (contact-scale self)))
       (left-margin (max (ai-button-end-width dims)
			 (case label-alignment
			   (:left   0)
			   (:center (pixel-round (- width label-width) 2))
			   (:right  (- width (ai-button-end-width dims) label-width)))))) 
      
      (if (stringp label)
	  (draw-glyphs self gc left-margin (ai-text-baseline dims) label)
	  
	  ;; Else draw pixmap label...
	  (let ((label-height (getf (pixmap-plist label) :height)))
	    (with-gcontext (gc :fill-style :tiled :tile label)
	      (draw-rectangle
		self gc
		left-margin (max 0 (pixel-round (- height label-height) 2))
		label-width label-height t)))))))


(DEFMETHOD display-default-indicator ((action-item action-item) gc)
  ;;  Draws the 1-2 pixel wide default indicator in the foreground color of GC...
  (with-slots (width height) action-item
    (LET* ((scale (contact-scale action-item)) interior-width
	   (dims (GETF *button-dimensions-by-scale* scale))
	   (button-pixmaps (get-button-pixmaps action-item))
	   (button-end-width (ai-button-end-width dims))
	   (default-ring-height (ai-default-ring-height dims)))
      
      (SETF interior-width
	    (- width button-end-width button-end-width))

      ;;  Draw the left-end of the default indicator...
      (with-gcontext (gc :clip-x 0 :clip-y 0
			 :clip-mask (ai-default-ring-pixmap button-pixmaps))
	(draw-rectangle action-item gc
			0  0
			button-end-width default-ring-height t))

      ;;  Draw the top horizontal line of the default indicator...
      (draw-rectangle action-item gc button-end-width 0 interior-width 0)

      ;;  Draw the bottom horizontal line of the default indicator...
      (draw-rectangle action-item gc button-end-width (1- default-ring-height) interior-width 0)

      ;;  Draw the right-end of the default indicator...
      (with-gcontext (gc :clip-x interior-width :clip-y 0
			 :clip-mask (ai-default-ring-pixmap button-pixmaps))
	(draw-rectangle action-item gc
			(+ button-end-width interior-width) 0
			button-end-width default-ring-height t)))))


(DEFMETHOD just-clear-body-of-button ((action-item action-item) gc)
  (with-slots (width height) action-item
    (LET* ((scale (contact-scale action-item)) interior-width body-clear-stencil
	   (dims (GETF *button-dimensions-by-scale* scale))
	   (button-pixmaps (get-button-pixmaps action-item))
	   (button-end-width (ai-button-end-width dims))
	   (default-ring-height (ai-default-ring-height dims))
	   (fill-style (gcontext-fill-style gc)))
      
      (SETF interior-width
	    (- width button-end-width button-end-width))
      
      (when (< interior-width 0)
	(setq interior-width 0))
      
      (SETF body-clear-stencil (ai-body-clearing-stencil-pixmap button-pixmaps))

      (with-gcontext (gc :fill-style (IF (EQ fill-style :stippled) :solid fill-style))
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;   Clear out the button's non-border, non-margin pixels to
	;;	the foreground color of GC...
	
	;;   Start by clearing the left end of the button...
	(with-gcontext (gc :clip-x 0 :clip-y 0 :clip-mask body-clear-stencil)
	  (draw-rectangle action-item gc 0 0
			  button-end-width default-ring-height t))
	
	;;   Clear out the background for the label...
	(draw-rectangle action-item gc
			button-end-width 0
			interior-width default-ring-height t)
	
	;;   Clear out the drawable pixels of the right button end...
	(with-gcontext (gc :clip-x interior-width :clip-y 0 :clip-mask body-clear-stencil)
	  (draw-rectangle action-item gc
			  (+ button-end-width interior-width) 0
			  button-end-width default-ring-height t))))))


(DEFMETHOD clear-button-and-display-border ((action-item action-item) gc)
  (with-slots (foreground fill-color width height) action-item

      (clear-area action-item)

      (just-clear-body-of-button action-item gc)))


(DEFMETHOD preferred-size ((action-item action-item) &key width height border-width)
    (declare (ignore width height border-width))

  ;;  An action button always wants a border-width of zero.
  ;;  Its preferred height is that dictated by its scale slot.
  ;;  Given a text label, its preferred width is the width of its label in the font
  ;;    corresponding to the scale plus the widths of its button ends.
  ;;  Given an image or pixmap label, use its width.

  (with-slots (label font preferred-width width height) action-item
    (LET* ((scale (contact-scale action-item))
	  (dims (GETF *button-dimensions-by-scale* scale))
	  (button-end-width (ai-button-end-width dims))
	  p-width)

      (SETF p-width
	    (OR preferred-width
		(SETF preferred-width (+ button-end-width
					 (label-width action-item label)
					 button-end-width))))

      (VALUES p-width (ai-height dims) 0))))
