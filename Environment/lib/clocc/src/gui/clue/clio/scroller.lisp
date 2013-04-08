;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Fonts:(CPTFONT); Syntax:Common-Lisp -*-


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

(export '(
	  make-scroller
	  scale-increment
	  scale-indicator-size
	  scale-maximum
	  scale-minimum
	  scale-orientation
	  scale-update
	  scale-update-delay
	  scale-value
	  scroller
	  )
	'clio-open)


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Scroller                                       |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


;; Implementation Strategy:
;;
;; Elevator and anchor controls are implemented as non-contact subwindows (i.e.
;; scroller is NOT a composite). Overall, this strategy simplifies the tasks
;; of determining which control is receiving input and of confining the pointer
;; cursor to controls during continuous scrolling, without unnecessarily
;; incurring the full cost of a sub-contact.

;; The elevator is represented as an :input-output subwindow; the imagery of all
;; elevator controls is drawn to this subwindow.  The less/more arrow and drag
;; area controls are represented as :input-only subwindows of the elevator
;; subwindow. Subwindows are used for these controls only for use as :confine-to
;; windows while the pointer is grabbed.

;; All control subwindows are recorded in the regions vector of the scroller.
;; The scroller subwindow receiving a :button-press is determined from the child
;; slot of the button event.  During event handling, the regions vector is
;; searched to look up the vector index of the event window (see FIND-REGION).
;; If the event child is the elevator, then a further search based on elevator
;; geometry is necessary to determine which elevator subwindow is the event
;; window.  The resulting vector index is used to select an element from a
;; vector of functions to handle the :button-press (see PRESS-HANDLERS).


(defcontact scroller (core contact)  
  ((increment 	:type 		number
	 	:reader         scale-increment	       ; setf defined below
		:initarg	:increment
	 	:initform 	1)
   
   (indicator-size
                :type 		(or number (member :off))
		:reader         scale-indicator-size   ; setf defined below
		:initarg	:indicator-size
		:initform 	0)
   
   (maximum 	:type 		number
	 	:reader         scale-maximum	       ; setf defined below
		:initarg	:maximum
	 	:initform 	1)
   
   (minimum 	:type 		number
	 	:reader         scale-minimum	       ; setf defined below
		:initarg	:minimum
	 	:initform 	0)
   
   (orientation :type 		(member :horizontal :vertical)
	 	:reader         scale-orientation      ; setf defined below
		:initarg	:orientation
	 	:initform 	:vertical)
   
   (update-delay :type		(or number (member :until-done))
		 :reader         scale-update-delay    ; setf defined below
		 :initarg	:update-delay
		 :initform	0)
   
   (value 	:type 		number
	 	:reader         scale-value	       ; setf defined below
		:initarg	:value
	 	:initform 	0)
   
   (compress-exposures 
                :initform       :on
		:type           (member :off :on)
		:reader         contact-compress-exposures
		:allocation     :class)

   (regions     :type           (vector window)
		:initform       (make-array 6)))
  
  (:resources
    increment indicator-size maximum minimum orientation update-delay value
    (border-width :initform 0)
    (event-mask   :initform #.(make-event-mask :pointer-motion-hint :exposure))))


;; Index values for accessing region vector and press handler vector
(defconstant *elevator-region*   0)
(defconstant *min-anchor-region* 1)
(defconstant *max-anchor-region* 2)
(defconstant *less-arrow-region* 3)
(defconstant *drag-area-region*  4)
(defconstant *more-arrow-region* 5)
(defconstant *cable-region*      6)


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Initialization                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun make-scroller (&rest initargs &key &allow-other-keys)
  (apply #'make-contact 'scroller initargs))


(defmethod initialize-instance :after ((self scroller) &key &allow-other-keys)
  (with-slots (width height) self
    ;; Initialize required geometry
    (multiple-value-setq (width height) (preferred-size self))))

(labels
  ((min-anchor-geometry
     (dimensions scroller orientation width height)
     (declare (ignore scroller))
     (let ((anchor-width  (scrollbar-anchor-width dimensions))
	   (anchor-height (scrollbar-anchor-height dimensions)))
       (if (eq orientation :vertical)
	   (values (pixel-round (- width anchor-width) 2) 0 (- anchor-width 2) (- anchor-height 2))
	   (values 0 (pixel-round (- height anchor-width) 2) (- anchor-height 2) (- anchor-width 2)))))
   
   (max-anchor-geometry
     (dimensions scroller orientation width height)
     (declare (ignore scroller))
     (let ((anchor-width  (scrollbar-anchor-width dimensions))
	   (anchor-height (scrollbar-anchor-height dimensions)))
       (if (eq orientation :vertical)
	   (values (pixel-round (- width anchor-width) 2) (- height anchor-height) (- anchor-width 2) (- anchor-height 2))
	   (values (- width anchor-height) (pixel-round (- height anchor-width) 2) (- anchor-height 2) (- anchor-width 2)))))
   
   (elevator-geometry
     (dimensions scroller orientation width height) 
     (let ((anchor-width  (scrollbar-anchor-width dimensions)))
       (if (eq orientation :vertical)
	   (values (pixel-round (- width anchor-width) 2) (scroller-value-position scroller) anchor-width (scrollbar-elevator-size scroller))
	   (values (scroller-value-position scroller) (pixel-round (- height anchor-width) 2) (scrollbar-elevator-size scroller) anchor-width))))
   
   (less-arrow-geometry
     (dimensions scroller orientation width height)
     (declare (ignore scroller orientation width height))
     (let ((anchor-width  (scrollbar-anchor-width dimensions)))
       (values 0 0 anchor-width anchor-width)))
   
   (more-arrow-geometry
     (dimensions scroller orientation width height)
     (declare (ignore width height))
     (let ((anchor-width  (scrollbar-anchor-width dimensions))
	   (abbreviated-p (scrollbar-abbreviated-p scroller)))
       (if (eq orientation :vertical)
	   (values 0 (+ anchor-width (if abbreviated-p 0 anchor-width)) anchor-width anchor-width)
	   (values (+ anchor-width (if abbreviated-p 0 anchor-width)) 0 anchor-width anchor-width))))
   
   (drag-area-geometry
     (dimensions scroller orientation width height)
     (declare (ignore scroller width height))
     (let ((anchor-width  (scrollbar-anchor-width dimensions)))
       (if (eq orientation :vertical)
	   (values 0 anchor-width anchor-width anchor-width)
	   (values anchor-width 0 anchor-width anchor-width))))
   
   (reconfigure-controls (self)
     (declare (type scroller self))
     (with-slots (regions width height orientation) (the scroller self) 
      (let ((dimensions (getf *scrollbar-dimensions* (contact-scale self)))) 
	(let ((window (svref regions *min-anchor-region*)))
	  (with-state (window)
	    (multiple-value-bind (window-x window-y window-width window-height)
		(min-anchor-geometry dimensions self orientation width height)
	      (setf (drawable-x window)      window-x
		    (drawable-y window)      window-y
		    (drawable-width window)  window-width
		    (drawable-height window) window-height))))
	(let ((window (svref regions *max-anchor-region*)))
	  (with-state (window)
	    (multiple-value-bind (window-x window-y window-width window-height)
		(max-anchor-geometry dimensions self orientation width height)
	      (setf (drawable-x window)      window-x
		    (drawable-y window)      window-y
		    (drawable-width window)  window-width
		    (drawable-height window) window-height))))
	(let ((window (svref regions *elevator-region*)))
	  (with-state (window)
	    (multiple-value-bind (window-x window-y window-width window-height)
		(elevator-geometry dimensions self orientation width height)
	      (setf (drawable-x window)      window-x
		    (drawable-y window)      window-y
		    (drawable-width window)  window-width
		    (drawable-height window) window-height))))
	(let ((window (svref regions *drag-area-region*)))
	  (with-state (window)
	    (multiple-value-bind (window-x window-y window-width window-height)
		(drag-area-geometry dimensions self orientation width height)
	      (setf (drawable-x window)      window-x
		    (drawable-y window)      window-y
		    (drawable-width window)  window-width
		    (drawable-height window) window-height))))
	(let ((window (svref regions *less-arrow-region*)))
	  (with-state (window)
	    (multiple-value-bind (window-x window-y window-width window-height)
		(less-arrow-geometry dimensions self orientation width height)
	      (setf (drawable-x window)      window-x
		    (drawable-y window)      window-y
		    (drawable-width window)  window-width
		    (drawable-height window) window-height))))
	(let ((window (svref regions *more-arrow-region*)))
	  (with-state (window)
	    (multiple-value-bind (window-x window-y window-width window-height)
		(more-arrow-geometry dimensions self orientation width height)
	      (setf (drawable-x window)      window-x
		    (drawable-y window)      window-y
		    (drawable-width window)  window-width
		    (drawable-height window) window-height))))))))

  (defmethod realize :after ((self scroller))
    ;; Create control region windows
    (with-slots (regions width height orientation foreground) self
      (let*
	((dimensions    (getf *scrollbar-dimensions* (contact-scale self))) 
	 
	 (min-anchor    (multiple-value-bind (region-x region-y region-width region-height)
			    (min-anchor-geometry dimensions self orientation width height)
			  (create-window
			    :parent self
			    :x      region-x
			    :y      region-y
			    :width  region-width
			    :height region-height
			    :background :parent-relative
			    :border-width 1
			    :border foreground
			    :gravity (if (eq orientation :vertical) :north :west))))
	 
	 (max-anchor    (multiple-value-bind (region-x region-y region-width region-height)
			    (max-anchor-geometry dimensions self orientation width height)
			  (create-window
			    :parent self
			    :x      region-x
			    :y      region-y
			    :width  region-width
			    :height region-height
			    :background :parent-relative
			    :border-width 1
			    :border foreground
			    :gravity (if (eq orientation :vertical) :north :west))))
	 
	 (elevator      (multiple-value-bind (region-x region-y region-width region-height)
			    (elevator-geometry dimensions self orientation width height)
			  (create-window
			    :parent self
			    :x      region-x
			    :y      region-y
			    :width  region-width
			    :height region-height
			    :border-width 0
			    :gravity (if (eq orientation :vertical) :north :west))))
	 
	 (drag-area     (multiple-value-bind (region-x region-y region-width region-height)
			    (drag-area-geometry dimensions self orientation width height)
			  (create-window
			    :parent elevator
			    :class  :input-only
			    :x      region-x
			    :y      region-y
			    :width  region-width
			    :height region-height
			    :border-width 0)))
	 
	 (less-arrow    (multiple-value-bind (region-x region-y region-width region-height)
			    (less-arrow-geometry dimensions self orientation width height)
			  (create-window
			    :parent elevator
			    :class  :input-only
			    :x      region-x
			    :y      region-y
			    :width  region-width
			    :height region-height
			    :border-width 0)))
	 
	 (more-arrow    (multiple-value-bind (region-x region-y region-width region-height)
			    (more-arrow-geometry dimensions self orientation width height)
			  (create-window
			    :parent elevator
			    :class  :input-only
			    :x      region-x
			    :y      region-y
			    :width  region-width
			    :height region-height
			    :border-width 0))))
	
	(setf (svref regions *min-anchor-region*) min-anchor)
	(setf (svref regions *max-anchor-region*) max-anchor)
	(setf (svref regions *elevator-region*)   elevator)
	(setf (svref regions *drag-area-region*)  drag-area)
	(setf (svref regions *less-arrow-region*) less-arrow)
	(setf (svref regions *more-arrow-region*) more-arrow)
	
	(map-subwindows self)
	(map-subwindows elevator))))

  (defmethod rescale ((self scroller))
    (with-slots (orientation) self
      ;; Request change to preferred width/height, depending on orientation.
      (multiple-value-bind (rw rh) (if (eq :vertical orientation) (values 0 nil) (values nil 0))
	(multiple-value-bind (pw ph) (preferred-size self :width rw :height rh)
	  (change-geometry self :width pw :height ph :accept-p t))))
    
    (when (realized-p self)
      (reconfigure-controls self)))

  (defmethod (setf scale-orientation) (new-orientation (scroller scroller))
    (with-slots (orientation width height) scroller
      (unless (eq orientation new-orientation)
	(check-type new-orientation (member :horizontal :vertical))
	
	(setf orientation new-orientation)
	
	(multiple-value-bind (new-width new-height)
	    (preferred-size scroller :width height :height width)
	  (change-geometry scroller :width new-width :height new-height))
	(reconfigure-controls scroller)))
    
    new-orientation))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Accessors                                     |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod (setf scale-update-delay) (new-update-delay (scroller scroller))
  (with-slots (update-delay) scroller
    (assert (or (eq new-update-delay :until-done)
		(and (numberp new-update-delay) (not (minusp new-update-delay)))) ()
	    "~a is neither :UNTIL-DONE or a non-negative number." new-update-delay)    
    (setf update-delay new-update-delay)))

(defmethod (setf scale-value) (new-value (scroller scroller))
  (scale-update scroller :value new-value)
  new-value)
    
(defmethod (setf scale-minimum) (new-minimum (scroller scroller))
  (scale-update scroller :minimum new-minimum)
  new-minimum)

(defmethod (setf scale-maximum) (new-maximum (scroller scroller))
  (scale-update scroller :maximum new-maximum)
  new-maximum)

(defmethod (setf scale-increment) (new-increment (scroller scroller))
  (scale-update scroller :increment new-increment)
  new-increment)

(defmethod (setf scale-indicator-size) (new-indicator-size (scroller scroller))
  (scale-update scroller :indicator-size new-indicator-size)
  new-indicator-size)

(defmacro true-indicator-size (size)
  `(if (eq ,size :off) 0 ,size))

(defmethod scale-update ((scroller scroller) &key value minimum maximum indicator-size increment)
  (with-slots
    ((current-val value)
     (current-min minimum)
     (current-max maximum)
     (current-ind indicator-size)
     (current-inc increment)
     regions
     orientation)
    scroller
    
    
    (setf minimum        (or minimum current-min)
	  maximum        (or maximum current-max)
	  value          (or value current-val)
	  indicator-size (or indicator-size current-ind)
	  increment      (or increment current-inc))

    (assert (numberp value) () "~s for :value is not a number" value)
    (assert (numberp minimum) () "~s for :minimum is not a number" minimum)
    (assert (numberp maximum) () "~s for :maximum is not a number" maximum)
    (assert (and (numberp increment) (not (minusp increment))) ()
	  "~s for :increment is not a number" increment)
    (assert (or (and (numberp indicator-size) (not (minusp indicator-size)))
		(eq indicator-size :off))
	  () "~s for :indicator-size is not :off or a non-negative-number)" indicator-size)


    (assert (<= minimum maximum) ()
	    "Minimum (~a) is greater than maximum (~a)."
	    minimum maximum)
    (assert (<= minimum value maximum) ()
	    "Value (~a) must be in the range [~a, ~a]."
	     value minimum maximum)

    (let*
      ((insensitive-p    (not (sensitive-p scroller)))
       (less-arrow-dim-p (or insensitive-p (= current-val current-min)))
       (more-arrow-dim-p (or insensitive-p (= current-val current-max)))
       (prev-min         current-min)
       (prev-max         current-max)
       (prev-ind         current-ind)
       (prev-val         current-val))
      
      (setf current-min minimum
	    current-max maximum
	    current-val value
	    current-ind indicator-size
	    current-inc increment)
      
      ;; Update display
      (when (realized-p scroller)
	(cond
	  ((not (and (eql current-min prev-min) (eql current-max prev-max) (eql current-ind prev-ind)))
	   (display scroller))

	  ((not (eql current-val prev-val))
	   
	   ;; Position elevator
	   (let ((position (scroller-value-position scroller))
		 (elevator (svref regions *elevator-region*)))
	     (if (eq :vertical orientation)
		 (setf (drawable-y elevator) position)
		 (setf (drawable-x elevator) position)))
	   
	   ;; Dim arrows, if necessary
	   (scrollbar-update-less-arrow scroller less-arrow-dim-p insensitive-p)
	   (scrollbar-update-more-arrow scroller more-arrow-dim-p insensitive-p)))))))

(defmethod (setf contact-foreground) :after (new-fg (self scroller))
  (declare (ignore new-fg))
  (with-slots (foreground regions) self
    (setf (window-border (svref regions *min-anchor-region*)) foreground)
    (setf (window-border (svref regions *max-anchor-region*)) foreground)))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                        Geometry Management                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod preferred-size ((self scroller) &key width height border-width)
  (declare (ignore border-width))
  (with-slots (orientation (current-height height) (current-width width)) self
    (let*
      ((dimensions       (getf *scrollbar-dimensions* (contact-scale self))) 
       (margin           (scrollbar-margin dimensions))
       (anchor-width     (scrollbar-anchor-width dimensions))
       (anchor-height    (scrollbar-anchor-height dimensions))

       ;; Calculate geometry assuming :vertical orientation
       (preferred-width  (+ margin anchor-width margin))
       (preferred-height (max
			   ;; Suggested or current height
			   (or (if (eq orientation :vertical) height width)
			       (if (eq orientation :vertical) current-height current-width))
			   
			   ;; Size of abbreviated scrollbar (no drag area)
			   (+ anchor-height margin
			      anchor-width  anchor-width
			      margin anchor-height))))

      ;; Return preferred geometry according to actual orientation
      (values
	(if (eq orientation :vertical) preferred-width  preferred-height)
	(if (eq orientation :vertical) preferred-height preferred-width)
	0))))

(defmethod resize :around ((self scroller) new-width new-height new-border-width)
  (if (realized-p self)
      ;; Reconfigure subwindows
      (let* ((abbreviated-before-p (scrollbar-abbreviated-p self))
	     (resized-p            (call-next-method)))
	(with-slots (width height orientation regions) self
	  (let*
	    ((scale             (contact-scale self))
	     (max-anchor        (svref regions *max-anchor-region*))
	     (dimensions        (getf *scrollbar-dimensions* scale))
	     (anchor-position   (- (if (eq orientation :vertical) height width)
				   (scrollbar-anchor-height dimensions)
				   1))
	     (elevator          (svref regions *elevator-region*))
	     (elevator-position (scroller-value-position self)))
	    
	    ;; Reposition max anchor
	    (if (eq orientation :vertical)
		(setf (drawable-y max-anchor) anchor-position)
		(setf (drawable-x max-anchor) anchor-position))
	    
	    ;; Reconfigure elevator
	    (multiple-value-bind (elevator-size abbreviated-after-p)
		(scrollbar-elevator-size self)
	      (with-state (elevator)
		(case orientation
		  (:vertical	      
		   (setf (drawable-y elevator)      elevator-position)
		   (setf (drawable-height elevator) elevator-size))
		  
		  (:horizontal
		   (setf (drawable-x elevator)      elevator-position)
		   (setf (drawable-width elevator)  elevator-size))))
	      
	      ;; Changing abbreviation?
	      (unless (eq abbreviated-before-p abbreviated-after-p)
		;; Reposition more-arrow region
		(let* ((anchor-width   (scrollbar-anchor-width dimensions))
		       (more-arrow-pos (+ anchor-width (if abbreviated-after-p 0 anchor-width))))
		  (if (eq orientation :vertical)
		      (setf (drawable-y (svref regions *more-arrow-region*)) more-arrow-pos)
		      (setf (drawable-x (svref regions *more-arrow-region*)) more-arrow-pos)))
		
		;; Redisplay elevator image
		(scrollbar-display-elevator self scale)))))
	resized-p)
      
      ;; If not yet realized, just do it
      (call-next-method)))
	


(defun scrollbar-abbreviated-p (scroller)
  (with-slots (width height orientation) (the scroller scroller)
    (let*
      ((dimensions       (getf *scrollbar-dimensions* (contact-scale scroller))) 
       (margin           (scrollbar-margin dimensions))
       (anchor-width     (scrollbar-anchor-width dimensions))
       (anchor-height    (scrollbar-anchor-height dimensions)))

      (<= (if (eq orientation :vertical) height width)
	  
	  (+ anchor-height margin
	     anchor-width anchor-width anchor-width
	     margin anchor-height)))))


(defun scrollbar-elevator-size (scroller)
  (let ((abbreviated-p (scrollbar-abbreviated-p scroller)))
    (values
      (+ (* (scrollbar-anchor-width
	      (getf *scrollbar-dimensions* (contact-scale scroller)))
	    (if abbreviated-p  2 3))
	 2)
      abbreviated-p)))

(defun scrollbar-less-arrow-geometry (scroller)
  (let ((arrow-size (1- (scrollbar-anchor-width (getf *scrollbar-dimensions*
						      (contact-scale scroller))))))
    (if (eq :vertical (scale-orientation scroller))
	(values 1 1 (- arrow-size 2) arrow-size)
	(values 1 1 arrow-size (- arrow-size 2)))))

(defun scrollbar-drag-area-geometry (scroller)
  (let*
    ((area-size      (scrollbar-anchor-width
			(getf *scrollbar-dimensions* (contact-scale scroller))))
     (area-position  (1+ area-size)))

    (if (eq :vertical (scale-orientation scroller))
	(values 1 area-position (- area-size 3) (1- area-size))
	(values area-position 1 (1- area-size) (- area-size 3)))))

(defun scrollbar-more-arrow-geometry (scroller)
  (let*
    ((arrow-size      (scrollbar-anchor-width
			(getf *scrollbar-dimensions* (contact-scale scroller))))
     (arrow-position  (1+ (+ arrow-size (if (scrollbar-abbreviated-p scroller) 0 arrow-size)))))

    (if (eq :vertical (scale-orientation scroller))
	(values 1 arrow-position (- arrow-size 3) (1- arrow-size))
	(values arrow-position 1 (1- arrow-size) (- arrow-size 3)))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               Display                                      |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod display ((self scroller) &optional at-x at-y at-width at-height &key)  
  (with-slots (width height foreground regions orientation) self
    ;; Default exposed rectangle, if necessary
    (setf at-x      (or at-x      0)
	  at-y      (or at-y      0)
	  at-width  (or at-width  (- width at-x))
	  at-height (or at-height (- height at-y)))
    (let*
      ((scale             (contact-scale self))
       (dimensions        (getf *scrollbar-dimensions* scale))
       (anchor-width      (scrollbar-anchor-width dimensions))
       (anchor-height     (scrollbar-anchor-height dimensions))
       (margin            (scrollbar-margin dimensions))
       (cable-margin      (scrollbar-cable-margin dimensions))
       (cable-width       (scrollbar-cable-width dimensions))
       (elevator-position (scroller-value-position self))
       (elevator-size     (scrollbar-elevator-size self))
       (elevator-end      (+ elevator-position elevator-size)))

      ;;-----------------------------------------------------------------------------+
      ;;                                                                             |
      ;; Draw cable.                                                                 |
      ;;                                                                             |
      ;; Stipple fill  is  relatively  slow,  so  redrawing  entire cable can cause  |
      ;; annoying flicker.   But  computing  the  minimal  cable  area to redraw is  |
      ;; complicated, because the display  method is expected  to update the  image  |
      ;; when the elevator  moves (thus  exposing a  small region  of the  scroller  |
      ;; previously obscured by the  elevator).  In this  case, we must  redraw the  |
      ;; cable not only in  the area exposed,  but also elsewhere  to cover up  the  |
      ;; previous gaps between elevator/proportion indicator/cable.                  |
      ;;                                                                             |
      ;; The following algorithm is a compromise.  If the exposed area is  entirely  |
      ;; on one side of the  elevator (as it is  in the case of  an elevator move),  |
      ;; then we redraw the cable only on that side.                                 |
      ;;                                                                             | 
      ;;-----------------------------------------------------------------------------+

      (flet
	 ((exposed-cable-segment
	   (exposed-position exposed-size scroller-size)
	   (let ((min (+ anchor-height margin))
		 (max (- scroller-size margin anchor-height)))
	     (cond
	       ;; Exposed area before elevator?
	       ((>= elevator-position (+ exposed-position exposed-size))
		;; Redraw only first part of cable.
		(values min (- elevator-position min)))
	       
	       ;; Exposed area behind elevator?
	       ((>= exposed-position elevator-end)
		;; Redraw only last part of cable.
		(values elevator-end (- max elevator-end)))
	       
	       (t
		;; Redraw all of cable.
		(values min (- max min)))))))
	   
      (multiple-value-bind (cable-x cable-y cable-width cable-height)
	  (if (eq orientation :vertical)
	      (multiple-value-bind (cy ch) (exposed-cable-segment at-y at-height height)
		(values (pixel-round (- width cable-width) 2) cy cable-width ch))

	      (multiple-value-bind (cx cw) (exposed-cable-segment at-x at-width width)
		  (values cx (pixel-round (- height cable-width) 2) cw cable-width)))

	;; Draw exposed cable area
	(using-gcontext (gc :drawable self
			    :fill-style :stippled
			    :foreground foreground
			    :stipple    (contact-image-mask self 50%gray :depth 1))
	  (clear-area self :x cable-x :y cable-y :width cable-width :height cable-height)
	  (draw-rectangle self gc cable-x cable-y cable-width cable-height :fill-p))

	;; Draw proportion indicator
	(let* ((pi-size (scroller-indicator-size self))
	       (pi-pos  (scroller-indicator-position self pi-size)))
	  (multiple-value-bind (pi-x pi-y pi-width pi-height margin-x margin-y margin-width margin-height)
	      (if (eq orientation :vertical)
		  (values
		    cable-x
		    pi-pos
		    cable-width
		    pi-size

		    cable-x
		    (- pi-pos margin)
		    cable-width
		    (+ pi-size margin margin))
		  (values
		    pi-pos
		    cable-y
		    pi-size
		    cable-height

		    (- pi-pos margin)
		    cable-y
		    (+ pi-size margin margin)
		    cable-height))
	    (clear-area self :x margin-x :y margin-y :width margin-width :height margin-height)
	    (using-gcontext (gc :drawable self
				:fill-style :solid
				:foreground foreground)
	      (draw-rectangle self gc pi-x pi-y pi-width pi-height :fill-p))))))
      
      ;; Clear cable margin around elevator
      (multiple-value-bind (gap-x gap-y gap-width gap-height)
	  (if (eq orientation :vertical)
	      (values
		0 (- elevator-position cable-margin)
		nil (+ cable-margin elevator-size cable-margin))
	      (values
		(- elevator-position cable-margin) 0
		(+ cable-margin elevator-size cable-margin) nil))
	(clear-area self :x gap-x :y gap-y :width gap-width :height gap-height))
      
      ;; Compute elevator geometry
      (multiple-value-bind (elevator-x elevator-y elevator-width elevator-height)
	  (if (eq orientation :vertical)
	      (values
		(scrollbar-margin dimensions)
		elevator-position
		anchor-width
		elevator-size)
	      (values
		elevator-position
		(scrollbar-margin dimensions)				
		elevator-size
		anchor-width))	
	(when
	  ;; Exposed area intersects elevator?
	  (and (< elevator-x (+ at-x at-width))
	       (< elevator-y (+ at-y at-height))
	       (> (+ elevator-x elevator-width) at-x)
	       (> (+ elevator-y elevator-height) at-y))

	  (scrollbar-display-elevator self scale))))))



(defun scrollbar-display-elevator (scroller &optional scale)
  (setf scale (or scale (contact-scale scroller)))
  
  (with-slots (orientation regions foreground) (the scroller scroller)
    ;; Draw elevator image
    (let*
      ((image    (getf (getf *scrollbar-images* orientation) scale))
       (mask     (contact-image-mask
		   scroller image
		   :foreground foreground
		   :background (contact-current-background-pixel scroller)))
       (elevator (svref regions *elevator-region*)))
      
      (using-gcontext (gc :drawable scroller :exposures :off)
	(copy-area
	  mask gc
	  0 0
	  (image-width image) (image-height image)
	  elevator
	  0 0)
	
	(when (scrollbar-abbreviated-p scroller)
	  (let ((copy-size (scrollbar-anchor-width (getf *scrollbar-dimensions* scale))))
	    
	    (multiple-value-bind (from-x from-y copy-width copy-height)
		(if (eq :vertical orientation)
		    (values 0 (+ copy-size copy-size) copy-size (+ copy-size 2))
		    (values (+ copy-size copy-size) 0 (+ copy-size 2) copy-size))
	      
	      (multiple-value-bind (to-x to-y)
		  (if (eq :vertical orientation)
		      (values 0 copy-size)
		      (values copy-size 0))
		
		(copy-area
		  mask gc
		  from-x from-y
		  copy-width copy-height		  
		  elevator
		  to-x to-y))))))))
  
  ;; Dim arrows, if necessary
  (let ((insensitive-p (not (sensitive-p scroller))))
    (scrollbar-update-less-arrow scroller nil insensitive-p)
    (scrollbar-update-more-arrow scroller nil insensitive-p)))


 
(defun scrollbar-update-less-arrow (scroller dim-p insensitive-p)
  (with-slots (value minimum foreground regions) (the scroller scroller)
    (unless (eq dim-p (or insensitive-p (= value minimum)))

      (multiple-value-bind (arrow-x arrow-y arrow-width arrow-height)
	  (scrollbar-less-arrow-geometry scroller)
	
	(using-gcontext
	  (gc :drawable   scroller
	      :function   boole-xor
	      :fill-style :stippled
	      :foreground (logxor foreground (contact-current-background-pixel scroller))
	      :stipple    (contact-image-mask scroller 25%gray :depth 1))
	  
	  (draw-rectangle
	    (svref regions *elevator-region*) gc
	    arrow-x arrow-y
	    arrow-width arrow-height
	    :fill-p))))))


(defun scrollbar-update-more-arrow (scroller dim-p insensitive-p)
  (with-slots (value maximum foreground regions) (the scroller scroller)
    (unless (eq dim-p (or insensitive-p (= value maximum)))

      (multiple-value-bind (arrow-x arrow-y arrow-width arrow-height)
	  (scrollbar-more-arrow-geometry scroller)
	
	(using-gcontext
	  (gc :drawable   scroller
	      :function   boole-xor
	      :fill-style :stippled
	      :foreground (logxor foreground (contact-current-background-pixel scroller))
	      :stipple    (contact-image-mask scroller 25%gray :depth 1))
	      
	  (draw-rectangle
	    (svref regions *elevator-region*) gc
	    arrow-x arrow-y
	    arrow-width arrow-height
	    :fill-p))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                          Event Translations                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defevent scroller (:motion-notify :button-1)  scroller-handle-motion)
(defevent scroller (:timer :update-delay)      scroller-report-new-value)
(defevent scroller (:timer :click)             (throw-action :click-timeout))
(defevent scroller (:button-release :button-1) scroller-handle-release)
(defevent scroller (:button-press :button-1)   scroller-handle-press)

(defparameter *scroller-click-timeout* 0.2
  "Number of seconds to wait before starting continuous scrolling")

(defparameter *scroller-hold-timeout* 0.05
  "Number of seconds to wait during continuous scrolling before updating value")

(let ((press-handlers (make-array 7)))
  (flet
    ((find-region (scroller)
      ;; Return index of scroller region containing the current event
      (with-slots (regions orientation) scroller
	(with-event (child x y)
	  (if child
	      
	      ;; Look up event child window among scroller regions
	      (let ((region (position child regions :test #'eq)))
		
		(if (= region *elevator-region*)
		    ;; Which part of elevator got the press: less-arrow, drag-area, or more-arrow?
		    (let
		      ((region (+ *less-arrow-region*
				  (floor
				    (- (if (eq :vertical orientation) y x)
				       (scroller-value-position scroller))
				    (scrollbar-anchor-width
				      (getf *scrollbar-dimensions*
						     (contact-scale scroller)))))))
		      (if (and (= region *drag-area-region*)
			       (scrollbar-abbreviated-p scroller))
			  *more-arrow-region*
			  region))
		    
		    ;; Min/max anchor press
		    region))
	      
	      ;; Event occurred on non-child area of scroller
	      *cable-region*))))
    
    (press-cable (scroller)
      (with-slots (orientation increment width height indicator-size update-delay display value) scroller
	(with-event (x y)
	  (multiple-value-bind (event-position max-position)
	      (if (eq :vertical orientation) (values y height) (values x width))
	    (let*
	      ((anchor-height  (scrollbar-anchor-height
				 (getf *scrollbar-dimensions* (contact-scale scroller))))
	       (min-position   anchor-height)
	       (max-position   (- max-position anchor-height))
	       (pane-size      (let ((size (true-indicator-size indicator-size)))
				 (if (plusp size) size increment)))
	       (pane-increment (if (< event-position (scroller-value-position scroller))
				   ;; Decrement by pane?
				   (when (>= event-position min-position)
				     (- pane-size))
				   
				   ;; Increment by pane?
				   (when (<= event-position max-position)
				     pane-size))))
	      (unless pane-increment
		;; Just wait for release and do nothing
		(catch :release (loop (process-next-event display)))
		(return-from press-cable))

	    
	      (if (catch :release
		     ;; If user is clicking fast on cable, then we can arrive here
		     ;; before all :exposure's from previous clicks have been processed.
		     ;; Therefore, must use a timer, so we can continue processing :exposure's
		     ;; while waiting for click release.
		     (add-timer scroller :click *scroller-click-timeout*)
		     (unwind-protect
			 (catch :click-timeout
			   (loop (process-next-event display)))
		       (delete-timer scroller :click))
		     t)

		    ;; Perform continuous pane scrolling...
		    (let ((current-x x) (current-y y))
		      ;; Set timer for update
		      (when (and (numberp update-delay) (plusp update-delay))
			(add-timer scroller :update-delay update-delay))
		      
		      ;; Increment and warp pointer as needed, until release event
		      (apply-callback scroller :begin-continuous)
		      (catch :release		  
			(loop
			  (scroller-increment-value scroller pane-increment)
			  (multiple-value-setq (current-x current-y)
			    (scrollbar-cable-warp
			      scroller pane-increment
			      current-x current-y
			      min-position max-position))
			  
			  ;; Wait for timeout to elapse
			  (do () ((not (process-next-event display *scroller-hold-timeout*))))))
		      (apply-callback scroller :end-continuous))
		
		    ;; Single click -- increment value
		    (progn
		      (scroller-increment-value scroller pane-increment)
		      
		      ;; Warp pointer to keep it between elevator and anchor
		      (scrollbar-cable-warp
			scroller pane-increment
			x y
			min-position max-position)))
		  
	    ;; Report final value, if necessary		   		    
	    (unless (eql 0 update-delay)
	      (delete-timer scroller :update-delay)
	      (apply-callback scroller :new-value value)))))))

    (press-drag-area (scroller)
      (with-slots (display regions value update-delay foreground orientation) scroller
	(let
	  ((highlight-pixel (logxor foreground (contact-current-background-pixel scroller)))
	   (elevator        (svref regions *elevator-region*)))
	  
	  (multiple-value-bind (drag-x drag-y drag-width drag-height)
	      (scrollbar-drag-area-geometry scroller)
	    
	    (using-gcontext
	      (gc :drawable       scroller
		  :function       boole-xor
		  :foreground     highlight-pixel)
	      
	      ;; Highlight drag area
	      (draw-rectangle
		elevator gc
		drag-x drag-y drag-width drag-height :fill-p)

	      (with-event (x y)
		(let ((*previous-position* (if (eq :vertical orientation) y x))
		      (*drag-motion*       t))
		  (declare (special *previous-position* *drag-motion*))
		  
		  ;; Set timer for update
		  (when (and (numberp update-delay) (plusp update-delay))
		    (add-timer scroller :update-delay update-delay))

		  ;; Handle motion events until release.
		  (catch :release
		    (loop (process-next-event display))))) 
	      
	      ;; Report final value.
	      (when (and (numberp update-delay) (plusp update-delay))
		(delete-timer scroller :update-delay))
	      (apply-callback scroller :new-value value)

	      ;; Unhighlight drag area
	      (draw-rectangle
		elevator gc
		drag-x drag-y drag-width drag-height :fill-p))))))
    
    (press-less-arrow (scroller)
      (with-slots (display regions value maximum increment update-delay foreground orientation) scroller
	(let
	  ((highlight-pixel (logxor foreground (contact-current-background-pixel scroller))))

	  (multiple-value-bind (arrow-x arrow-y arrow-width arrow-height)
	      (scrollbar-less-arrow-geometry scroller)
	    
	    (using-gcontext
	      (gc :drawable       scroller
		  :function       boole-xor
		  :foreground     highlight-pixel)
	      
	      ;; Highlight arrow
	      (draw-rectangle
		(svref regions *elevator-region*) gc
		arrow-x arrow-y arrow-width arrow-height :fill-p)      

	      ;; Force pointer to stay within arrow window
	      (grab-pointer scroller #.(make-event-mask :button-press :button-release)
			    :confine-to (svref regions *less-arrow-region*))
		    
	      (if (catch :release (not (process-next-event display *scroller-click-timeout*)))
		  
		  ;; Perform continuous scrolling...
		  (progn
		    ;; Set timer for update
		    (when (and (numberp update-delay) (plusp update-delay))
		      (add-timer scroller :update-delay update-delay))
		    
		    ;; Increment until release event
		    (apply-callback scroller :begin-continuous)
		    (catch :release
		      (loop
			(scroller-increment-value scroller (- increment))
			(do () ((not (process-next-event display *scroller-hold-timeout*))))))
		    (apply-callback scroller :end-continuous))
		  
		  ;; Single click -- increment value.
		  (scroller-increment-value scroller (- increment)))	
		
	      ;; Report final value, if necessary		   		    
	      (unless (eql 0 update-delay)
		(delete-timer scroller :update-delay)
		(apply-callback scroller :new-value value))

	      ;; Release grab
	      (ungrab-pointer display)
	      
	      ;; Unhighlight arrow
	      (draw-rectangle
		(svref regions *elevator-region*) gc
		arrow-x arrow-y arrow-width arrow-height :fill-p))))))

    (press-more-arrow (scroller)
      (with-slots (display regions value maximum increment update-delay foreground orientation) scroller
	(let
	  ((highlight-pixel (logxor foreground (contact-current-background-pixel scroller))))

	  (multiple-value-bind (arrow-x arrow-y arrow-width arrow-height)
	      (scrollbar-more-arrow-geometry scroller)
	    
	    (using-gcontext
	      (gc :drawable       scroller
		  :function       boole-xor
		  :foreground     highlight-pixel)
	      
	      ;; Highlight arrow
	      (draw-rectangle
		(svref regions *elevator-region*) gc
		arrow-x arrow-y arrow-width arrow-height :fill-p)      

	      ;; Force pointer to stay within arrow window
	      (grab-pointer scroller #.(make-event-mask :button-press :button-release)
			    :confine-to (svref regions *more-arrow-region*))
		    
	      (if (catch :release (not (process-next-event display *scroller-click-timeout*)))
		  
		  ;; Perform continuous scrolling...
		  (progn
		    ;; Set timer for update
		    (when (and (numberp update-delay) (plusp update-delay))
		      (add-timer scroller :update-delay update-delay))
		    
		    ;; Increment until release event
		    (apply-callback scroller :begin-continuous)
		    (catch :release
		      (loop
			(scroller-increment-value scroller increment)
			(do () ((not (process-next-event display *scroller-hold-timeout*))))))
		    (apply-callback scroller :end-continuous))
		  
		  ;; Single click -- increment value.
		  (scroller-increment-value scroller increment))
	      
	      ;; Report final value, if necessary		   		    
	      (unless (eql 0 update-delay)
		(delete-timer scroller :update-delay)
		(apply-callback scroller :new-value value))

	      ;; Release grab
	      (ungrab-pointer display)
	      
	      ;; Unhighlight arrow
	      (draw-rectangle
		(svref regions *elevator-region*) gc
		arrow-x arrow-y arrow-width arrow-height :fill-p))))))
    
    (press-max-anchor (scroller)
      (with-slots (display regions foreground maximum value) scroller
	;; Highlight max anchor
	(let
	  ((max-anchor     (svref regions *max-anchor-region*))
	   (highlight-size (scrollbar-anchor-width
			     (getf *scrollbar-dimensions* (contact-scale scroller)))))
	  
	  ;; This rectangle size is "too big", but we let the server clip it
	  (using-gcontext (gc :drawable scroller :foreground foreground)
	    (draw-rectangle
	      max-anchor gc
	      0 0 highlight-size highlight-size
	      :fill-p))
	  
	  ;; Wait for release event
	  (catch :release
	    (loop (process-next-event display)))
	  
	  ;; Unhighlight max anchor
	  (clear-area max-anchor)
	  
	  ;; Go to maximum position
	  (unless (= value maximum)
	    (setf (scale-value scroller) maximum)
	    (apply-callback scroller :new-value maximum)))))
    
    (press-min-anchor (scroller)
      (with-slots (display regions foreground minimum value) scroller
	;; Highlight min anchor
	(let
	  ((min-anchor     (svref regions *min-anchor-region*))
	   (highlight-size (scrollbar-anchor-width
			     (getf *scrollbar-dimensions* (contact-scale scroller)))))
	  
	  ;; This rectangle size is "too big", but we let the server clip it
	  (using-gcontext (gc :drawable scroller :foreground foreground)
	    (draw-rectangle
	      min-anchor gc
	      0 0 highlight-size highlight-size
	      :fill-p))
	  
	  ;; Wait for release event
	  (catch :release
	    (loop (process-next-event display)))
	  
	  ;; Unhighlight min anchor
	  (clear-area min-anchor)
	  
	  ;; Go to minimum position
	  (unless (= value minimum)
	    (setf (scale-value scroller) minimum)
	    (apply-callback scroller :new-value minimum))))))

    ;; Initialize press-handlers dispatch vector
    (setf (svref press-handlers *elevator-region*)   nil)	; should never be used!!
    (setf (svref press-handlers *min-anchor-region*) #'press-min-anchor)
    (setf (svref press-handlers *max-anchor-region*) #'press-max-anchor)
    (setf (svref press-handlers *less-arrow-region*) #'press-less-arrow)
    (setf (svref press-handlers *drag-area-region*)  #'press-drag-area)
    (setf (svref press-handlers *more-arrow-region*) #'press-more-arrow)
    (setf (svref press-handlers *cable-region*)      #'press-cable)

    ;; Define press action function
    (defun scroller-handle-press (scroller)
      (let ((*scroller-pressed-p* t))
	(declare (special *scroller-pressed-p*))
	(funcall (svref press-handlers (find-region scroller)) scroller)))))

(defun scroller-handle-release (scroller)
  (declare (ignore scroller))
  (declare (special *scroller-pressed-p*))
  (when (boundp '*scroller-pressed-p*)
    (throw :release nil)))

(defun scroller-handle-motion (scroller)
  (declare (special *previous-position* *drag-motion*))
  (when (boundp '*drag-motion*)
    (with-slots (orientation) (the scroller scroller)   
      (with-event (state x y)
	(multiple-value-bind (ptr-x ptr-y)
	    ;; Is :button-1 still down?
	    (if (plusp (logand state #.(make-state-mask :button-1)))
		
		;; Yes, query current pointer position
		(query-pointer scroller)
		
		;; No, use final x,y returned for button transition
		(values x y))
	  
	  (let* ((new-position (if (eq :vertical orientation) ptr-y ptr-x))
		 (increment    (scroller-pixel-value scroller (- new-position *previous-position*))))
	    (unless (zerop increment)
	      (scroller-increment-value scroller increment)
	      (setf *previous-position* new-position))))))))


(defun scroller-report-new-value (scroller)
  (with-slots (value) (the scroller scroller)
    (apply-callback scroller :new-value value)))

(defun scroller-increment-value (scroller increment)
  (with-slots (value minimum maximum update-delay) (the scroller scroller)    
    (let*
      ((new-value (+ value increment))
       (adjusted  (min (max  (or (apply-callback scroller :adjust-value new-value)
				 new-value)
			     minimum)
		       maximum)))
      
      (unless (= adjusted value)
	(setf (scale-value scroller) adjusted)
	(when (eql 0 update-delay)
	  (apply-callback scroller :new-value adjusted))))))


(defun scrollbar-cable-warp (scroller pane-increment current-x current-y min-position max-position)
  (with-slots (orientation) (the scroller scroller)
    (let*
      ((current-position
	 (if (eq :vertical orientation) current-y current-x))
       
       (new-pointer-position
	 (if (plusp pane-increment)
	     (when (< current-position (setf min-position
					     (+ (scroller-value-position scroller)
						(scrollbar-elevator-size  scroller))))
	       (1+ min-position))
	     
	     (when (> current-position (setf max-position
					     (scroller-value-position scroller)))
	       (1- max-position)))))
      
      (when new-pointer-position	
	(if (eq :vertical orientation)
	    (setf current-y new-pointer-position)
	    (setf current-x new-pointer-position))	      
	(warp-pointer scroller current-x current-y))
      
      (values current-x current-y))))


(defun scroller-value-position (scroller)
  (with-slots (width height orientation value minimum maximum) (the scroller scroller)
    (let*
      ((dimensions       (getf *scrollbar-dimensions* (contact-scale scroller))) 
       (margin           (scrollbar-margin dimensions)) 
       (anchor-height    (scrollbar-anchor-height dimensions))
       (range            (- maximum minimum)))
      
      (+ anchor-height
	 margin
	 (if (zerop range) 0	     
	     (pixel-round
	       (* (- value minimum)
		  
		  ;; Pixels per value unit 
		  (/ (- (if (eq orientation :vertical) height width)
			anchor-height margin
			(* (scrollbar-anchor-width dimensions)
			   (if (scrollbar-abbreviated-p scroller) 2 3))
			2
			margin anchor-height)
		     range))))))))


(defun scroller-pixel-value (scroller pixels)
  (with-slots (width height orientation minimum maximum increment) (the scroller scroller)
    (let*
      ((dimensions       (getf *scrollbar-dimensions* (contact-scale scroller))) 
       (margin           (scrollbar-margin dimensions)) 
       (anchor-height    (scrollbar-anchor-height dimensions)))

      ;; pixels times value-units-per-pixel, rounded to nearest multiple of increment
      (* (pixel-round
	   (/ (* pixels (- maximum minimum))
	      (- (if (eq orientation :vertical) height width)
		 anchor-height margin
		 (* (scrollbar-anchor-width dimensions)
		    (if (scrollbar-abbreviated-p scroller) 2 3))
		 2
		 margin anchor-height))
	   increment)
	 increment))))


(defun scroller-indicator-position (scroller &optional size)
  (setf size (or size (scroller-indicator-size scroller)))
  
  (with-slots (width height orientation value minimum maximum) (the scroller scroller)
    (let*
      ((dimensions       (getf *scrollbar-dimensions* (contact-scale scroller))) 
       (margin           (scrollbar-margin dimensions)) 
       (anchor-height    (scrollbar-anchor-height dimensions))
       (range            (- maximum minimum)))
      
      (+ anchor-height
	 margin
	 (if (zerop range) 0	     
	     (pixel-round
	       (* (- value minimum)
		  
		  ;; Pixels per value unit for indicator position
		  (/ (- (if (eq orientation :vertical) height width)
			anchor-height margin
			size		    
			margin anchor-height)
		     range))))))))

(defun scroller-indicator-size (scroller)
  (with-slots (width height orientation minimum maximum indicator-size) (the scroller scroller)
    (let*
      ((dimensions       (getf *scrollbar-dimensions* (contact-scale scroller))) 
       (margin           (scrollbar-margin dimensions)) 
       (anchor-height    (scrollbar-anchor-height dimensions))
       (range            (- maximum minimum)))
      
      (pixel-round
	(*
	  (min (true-indicator-size indicator-size) range)     ; "clip" displayed size to cable range 
	  (if (zerop range) 0	     
	      ;; Pixels per value unit for indicator size
	      (/ (- (if (eq orientation :vertical) height width)
		    anchor-height margin		    
		    margin anchor-height)
		 range)))))))