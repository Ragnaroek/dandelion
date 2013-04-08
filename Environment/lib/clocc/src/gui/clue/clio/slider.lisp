;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Fonts:(CPTFONT); Syntax:Common-Lisp;  -*-


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
	  make-slider
	  slider
	  )
	'clio-open)

(defmacro translate-x-to-y (x x-width slider)
  "Translate x coord for horizontal slider into y of a vertical slider.
X-WIDTH is the width in x-direction that must be changed into a y-offset."
  `(with-slots (height) ,slider
     (- height 1 ,x (max 0 (1- ,x-width)))))


(defmacro confine-to (value minimum maximum)
  `(max ,minimum (min ,value ,maximum)))


(defmacro align (value increment)
  ;; Since we are talking SCALE VALUE here
  ;; we pixel-round since it may be a REAL number.
  `(if (= 1 ,increment)
       ,value
       (* (pixel-round ,value ,increment) ,increment)))


(defmacro value-length (value minimum)
  `(- ,value ,minimum))

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Slider                                         |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


;; Implementation Strategy:
;;
;;  Since CLIO should implement a "look and
;; feel" independent implemementation of open-look, then only those
;; parts of the slider that are going to exist in most implemementations
;; will be supported.  Since make-slider accepts :
;; (increment indicator-size maximum minimum orientation update-delay value compress-exposures)
;; then all "features" must be derived from these inputs.
;; 
;;  To provide numeric visual feedback of the current value is desirable,
;; but providing this as a typein field or read-only field really requires a label
;; or else the displayed result is somewhat confusing.
;;  The current value will be implemented as AUTOMATIC tick-marks and tick-mark
;; labels based on the min-max values and the space available to print them. The
;; actual current value will not be printed but will be discernable by "reading the scale".
;; 
;; Thus the slider parts implemented are :
;; 	(bar, drag-box, (automatic) tick-marks, (automatic) tick-text)
;; and the following will NOT be provided :
;;  	(End boxes, labels, typein fields, non-numeric text of any kind)
;; This means that the read-only min-max current-value fields will be provided only by way
;; of the min-/max tick-mark tick-text labels.
;; 
;;  When horizontal sliders require max (or min) values of more than 2 digits
;; then the tick-mark & tick-mark-number-labels are difficult to display. In this
;; case a :vertical orientation is more appropriate. If more than 2-digits are used
;; for a :horizontal slider then the tick-mark granularity will be reduced in order
;; to accommodate the width of the digits.
;; 

(defcontact slider (core contact)
  ((increment 	:type 		number
	 	:reader		scale-increment		;; SETF method defined below
		:initarg	:increment
	 	:initform 	1)
   
   (indicator-size		;; The size of the distance between tick-marks in value units.
                                ;; :off means "no tickmarks or tick labels", 1 will cause tick-mark
			        ;; overlap if there is not enough space to display. [2..N] will
				;; cause a tick-spacing of [1..(1- N)]. 
				;; 
                :type 		(or number (member :off)) ;; 0 means "automatic" tick mark spacing.
		:reader		scale-indicator-size	;; SETF method defined below
		:initarg	:indicator-size
		:initform 	0)
   
   (maximum 	:type 		number
	 	:reader		scale-maximum		;; SETF method defined below
		:initarg	:maximum
	 	:initform 	1)
   
   (minimum 	:type 		number
	 	:reader		scale-minimum		;; SETF method defined below
		:initarg	:minimum
	 	:initform 	0)
   
   (orientation :type 		(member :horizontal :vertical)
	 	:reader		scale-orientation	;; SETF method defined below
		:initarg	:orientation
	 	:initform 	:horizontal)
   
   (update-delay :type		(or number (member :until-done))
		 :reader	scale-update-delay	;; SETF method defined below
		 :initarg	:update-delay
		 :initform	0)
   
   (value 	:type 		number
	 	:reader		scale-value	;; SETF method defined below
		:initarg	:value
	 	:initform 	0)
   
   (compress-exposures 
                :initform       :on
		:type           (member :off :on)
		:reader         contact-compress-exposures
		:allocation     :class)

   ;; Internal storage slots
   (font 	   :type	fontable);; font for current scale

   (min-text-width :type	number)	 ;; pixel width of minimum value print string

   (max-text-width :type	number)	 ;; pixel width of maximum value print string

   (dimensions	   :type	list)    ;; (getf *slider-dimensions* scale)

   (middle-length  :type        number)	 ;; pixel length between first & last tick marks
   )
  
  (:resources
    increment indicator-size maximum minimum orientation update-delay value 
    (border-width :initform 0)
    (event-mask   :initform #.(make-event-mask :exposure :pointer-motion-hint))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; Setf Accessors                                                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf scale-orientation) (new-orientation (slider slider))
  (with-slots (orientation width height) slider
    (unless (eq orientation new-orientation)
      (check-type new-orientation (member :horizontal :vertical))
      
      (setf orientation new-orientation)
      
      (multiple-value-bind (new-width new-height)
	  (preferred-size slider :width height :height width)
	(change-geometry slider :width new-width :height new-height :accept-p t))))
  
  new-orientation)

(defmethod (setf scale-update-delay) (new-update-delay (slider slider))
  (with-slots (update-delay) slider
    (assert (or (eq new-update-delay :until-done)
		(and (numberp new-update-delay) (not (minusp new-update-delay)))) (new-update-delay)
	    "~a is neither :UNTIL-DONE or a non-negative number." new-update-delay)    
    (setf update-delay new-update-delay)))

(defmethod (setf scale-value) (new-value (slider slider))
  (scale-update slider :value new-value)
  new-value) 
    
(defmethod (setf scale-minimum) (new-minimum (slider slider))
  (scale-update slider :minimum new-minimum)
  new-minimum)

(defmethod (setf scale-maximum) (new-maximum (slider slider))
  (scale-update slider :maximum new-maximum)
  new-maximum)

(defmethod (setf scale-increment) (new-increment (slider slider))
  (scale-update slider :increment new-increment)
  new-increment)

(defmethod (setf scale-indicator-size) (new-indicator-size (slider slider))
  (scale-update slider :indicator-size new-indicator-size)
  new-indicator-size)


;;;------------------------------------------------------------------------------------+
;;;                                                                                    |
;;;	  Helper Functions                                                             |
;;;                                                                                    |
;;;------------------------------------------------------------------------------------+

(defun slider-tick-mark-thickness (slider)
  (if (eq :extra-large (contact-scale slider))
      3
      2))

(defun slider-bar-tick-gap (slider)
  ;; Distance top of tick-mark and nearest point on bar
  (1+ (ecase (contact-scale slider)
	(:small 1) (:medium 2) (:large 3) (:extra-large 4))))

(defun slider-margin (slider margin)
  "Returns the MARGIN of SLIDER, one of :min :top :text :max"
  ;; This is initially *slider-default-margin* until
  ;; after the PREFERRED-SIZE method is called. Then margins include
  ;; any additional increase due to a width or height larger than the
  ;; preferred size. :LEFT means the left margin for this particular orientation.
  (assert (member margin '(:min :top :text :max)) (margin)
	  "~a is an illegal margin" margin)
    (let ((margins (getf (getf (window-plist slider) :slider-info) :margins)))
      (or (getf margins margin)
	  ;;  Calling before margins are setup is never
	  ;;  done but code is here for completeness
	  *slider-default-margin*)))

(defun first-tick-offset (slider)
  ;; Offset, not including (slider-margin slider :min), from
  ;; min edge of contact to CENTERLINE of first tick-mark.
  (with-slots (min-text-width orientation font dimensions indicator-size) slider
    (let ((tick-mark-offset (slidebar-tick-mark-offset dimensions))
	   (gap (slidebar-gap dimensions)))
      (+ *slider-default-margin*
	 ;;  Add GAP below since drag-box clear-gap-around extends past bar edge
	 (if (eq :off indicator-size)
	     (+ gap tick-mark-offset)
	     (if (eq orientation :horizontal)
		 (max (floor min-text-width 2) (+ gap tick-mark-offset))
		 (+ gap tick-mark-offset) ;; the text baseline is ALWAYS above end of bar MIN
		 ))))))

(defun last-tick-offset (slider)
  ;; Offset, not including (slider-margin slider :max), from
  ;; max edge of contact to CENTERLINE of last tick-mark.
  (with-slots (max-text-width orientation font dimensions indicator-size) slider
    (let ((tick-mark-offset (slidebar-tick-mark-offset dimensions))
	  (gap (slidebar-gap dimensions)))
      (+ *slider-default-margin*
	 ;;  Add GAP below since drag-box clear-gap-around extends past bar edge
	 (if (eq :off indicator-size)
	     (+ gap tick-mark-offset)
	     (if (eq orientation :horizontal)
		 (max (ceiling max-text-width 2) (+ gap tick-mark-offset))
		 (+ tick-mark-offset
		    (max gap 
			 ;; font-ascent may go beyond end of bar MAX if font is bigger than "point" requested
			 (abs (- (cadr (getf (slidebar-bar-text-offset dimensions) orientation))
				 (max-char-ascent font)
				 ))))))))))


;;
;;
;;  Pixels :                          Scale Units :
;;
;; middle-length                   (- maximum minimum)
;;   
;;  [MAX]                               
;;    |                                 
;;    |                                 
;;    |              proportional       [MAX]
;;    |    -                              |
;;    |                                   |
;;    |   Pixel-delta                     |    -
;;    |                                   |    Scale-delta
;;    |                                   |  
;;  [MIN]  -                            [MIN]  -
;;  
;;    0                                 minimum
;;
;; Since :
;;
;; pixel-delta / middle-length = scale-delta / (- maximum minimum)
;;
;; Then :
;;
;; Pixel-delta = (* scale-delta middle-length) / (- maximum minimum)
;;
;; And :
;; 
;; Scale-delta = (* pixel-delta (- maximum minimum)) / middle-length
;;
(defun units-to-pixels (slider scale-delta)
  ;; Convert a scale delta to a pixel delta
  (with-slots (minimum maximum middle-length) slider
    (pixel-round (/ (* scale-delta middle-length)
	      (- maximum minimum)))))

(defun pixels-to-units (slider pixel-delta)
  (with-slots (orientation minimum maximum increment middle-length) slider
    ;; Convert a pixel delta to a scale units delta
    ;; *DON'T* round this - units may be fractional !
    (/ (* pixel-delta (- maximum minimum))
       middle-length)))

;; NOTE: The functions named ????-x and below return values strictly for a :horizontal 
;; slider and the return values must be translated for a :vertical slider.

(defun first-tick-x (slider)
  (+ (slider-margin slider :min)
     (first-tick-offset slider)))


(defun drag-box-center-x (slider &optional (scale-value (scale-value slider)))
  (with-slots (minimum) slider
    ;; Returns dead center of drag-box
    (+ (first-tick-x slider)
       ;; Must subtract minimum since minimum can be negative and is NOT always zero!
       (units-to-pixels slider (value-length scale-value minimum)))))


(defun drag-box-min-x (slider &optional (scale-value (scale-value slider)))
  (with-slots (dimensions minimum maximum) slider
    (let* ((drag-box-width (slidebar-drag-box-width dimensions))
	   (gap (slidebar-gap dimensions)))
      (- (drag-box-center-x slider scale-value)
	 (floor drag-box-width 2)
	 gap ;; subtract whitespace gap around drag-box
	 ))))

(defun drag-box-position (slider &optional (scale-value (scale-value slider)))
  (with-slots (orientation ) slider
    ;; Return values describing area of drag-box for SCALE-VALUE
    (let* ((drag-min-edge (drag-box-min-x slider scale-value))
	   (drag-image (getf (getf *slider-drag-box-images* orientation)
			     (contact-scale slider))))

      (if (eq orientation :horizontal)
	  (values drag-min-edge
		  (+ (slider-margin slider :top) *slider-default-margin*)
		  (image-width drag-image)
		  (image-height drag-image))
	  (values (+ (slider-margin slider :top) *slider-default-margin*)
		  (translate-x-to-y drag-min-edge (image-height drag-image) slider)
		  (image-width drag-image)
		  (image-height drag-image)))
      )))

  
(defmethod scale-update ((slider slider) &key value minimum maximum indicator-size increment)
  ;; Called by (method initialize-instance :after (slider)) to do error checking, and by
  ;; SETF methods for slots in arglist above, and by (setf scale-value) called to move slider.
  (with-slots
    ((current-val value)
     (current-min minimum)
     (current-max maximum)
     (current-ind indicator-size)
     (current-inc increment)
     orientation min-text-width max-text-width font)
    slider
    (let ((old-val (and value current-val)) ;; old-value & flag that value was passed in.
	  (old-min (and minimum current-min))
	  (old-max (and maximum current-max))
	  (old-inc (and increment current-inc))
	  (old-ind (and indicator-size current-ind)))

      (setf minimum        (or minimum current-min)
	    maximum        (or maximum current-max)
	    value          (or value (confine-to current-val minimum maximum))
	    indicator-size (or indicator-size current-ind)
	    increment      (or increment current-inc))

      (assert (and (numberp minimum) (numberp maximum)
		   (< minimum maximum))
	      (minimum maximum)
	      "Minimum (~a) is not less than maximum (~a)."
	      minimum maximum)

      (assert (and (numberp value)
		   (<= minimum value maximum))
	      (value)
	      "Value (~a) must be in the range [~a, ~a]."
	      value minimum maximum)

      (assert (or (eq :off indicator-size)
		  (and (numberp indicator-size)
		       (not (minusp indicator-size))))
	      (indicator-size)
	      "Indicator-size (~a) must be :OFF, 0, or a positive number."
	      indicator-size)

      (assert (and (numberp increment)
		   (< 0 increment (1+ (- maximum minimum))) ;; allow fractional increments, allow increment = maximum
		   (zerop (mod (- maximum minimum) increment)))
	      (increment)
	      "Increment (~a) must be in the range [0 ~a] and a factor of ~:*~d."
	      increment (- maximum minimum))

      ;;  Once VALUE & INCREMENT are valid we can align VALUE, if necessary,
      ;;  to be a multiple of INCREMENT.
      (setq value (+ minimum (align (value-length value minimum) increment)))
      
      (setf current-min minimum
	    current-max maximum
	    current-val value
	    current-ind indicator-size
	    current-inc increment
	    min-text-width (text-extents font (format nil "~a" minimum))
	    max-text-width (text-extents font (format nil "~a" maximum)))

      ;; Redisplay drag-box and any changes
      (when (realized-p slider)
	(cond ((or (and old-min
			(not (= old-min current-min)))
		   (and old-max
			(not (= old-max current-max)))
		   (and old-ind
			(not (eq old-ind current-ind)))
		   (and old-inc
			(not (= old-inc current-inc))))
	       (clear-area slider :exposures-p t))
	      
	      ((and old-val ;; when called with NEW increment value 
		    (not (= old-val current-val))) ;; when something has changed
	       
	       ;; Compute area of old drag-box ( if any )
	       (multiple-value-bind (old-x old-y old-width old-height)
		   (drag-box-position slider old-val)
		 
		 ;; Compute area of new drag-box 
		 (multiple-value-bind (x y width height)
		     (drag-box-position slider current-val)
		   
		   ;; Merge areas to redisplay : new drag-box, bar between old & new,
		   ;; old drag-box ( if any ), & tick marks obscured by drag-box
		   (when old-val
		     (if (eq orientation :horizontal)		  
			 (setf width (+ (abs (- x old-x)) (max old-width width))
			       x (min x old-x))
			 (setf height (+ (abs (- y old-y)) (max old-height height))
			       y (min y old-y))))
		   (clear-area slider :x x :y y :width width :height height)
		   (display slider x y width height))))
	      (t)))
      )))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Initialization                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun make-slider (&rest initargs &key &allow-other-keys)
  (apply #'make-contact 'slider initargs))

(defun bar-bottom-offset (slider)
  ;; Offset from the top (left) of horizontal (vertical) slider
  ;; ... does NOT include (slider-margin slider :top) ...
  ;; and 1-pixel past the bottom (right) edge of the slidebar
  (with-slots (dimensions) slider
    (+ *slider-default-margin*
       (slidebar-gap dimensions)
       (slidebar-bar-drag-offset dimensions)
       (slidebar-bar-thickness dimensions))))

(defun fixed-thickness (slider &key include-text-p)
  ;; The minimum thickness of slider required for the
  ;; scale, orientation, and string characteristics of the minimum & maximum
  (with-slots (orientation min-text-width max-text-width
		     indicator-size dimensions font) slider
    (let ((x (first  (getf (slidebar-bar-text-offset dimensions) orientation)))
	  (y (second (getf (slidebar-bar-text-offset dimensions) orientation)))
	  (scale (contact-scale slider)))
      (if (eq :off indicator-size)
	  (+ *slider-default-margin*
	     (if (eq orientation :horizontal)
		 (image-height (getf (getf *slider-drag-box-images* orientation) scale))
		 (image-width  (getf (getf *slider-drag-box-images* orientation) scale)))
	     *slider-default-margin*) ;; no space allocated for tick marks & text
	  ;; else
	  (if (eq orientation :horizontal)
	      (+ (bar-bottom-offset slider) y 
		 (if include-text-p
		     (+ (max-char-descent font) *slider-default-margin*)
		     0))
	      (+ (bar-bottom-offset slider) x 
		 (if include-text-p
		     (+ (max min-text-width max-text-width) *slider-default-margin*)
		     0)))))))

(defun slider-compute-margins (slider)
  ;;  Now margins can be computed from the delta between the size needed and the
  ;;  size we were given. The length of the slider basically stretches to fit but
  ;;  any extra height results in the slider being centered in space provided.
  ;;  PREFERRED-SIZE (via initialize-instance :after) MUST have been called to
  ;;  set WIDTH & HEIGHT by this time.
  (with-slots (orientation width height middle-length) slider
    (let* ((total-min-thickness (fixed-thickness slider :include-text-p t))
	   (size          (if (eq orientation :horizontal) height width))
	   (top-margin    (floor (- size total-min-thickness) 2))
	   (bottom-margin (- size total-min-thickness top-margin)))
      
      (setf (getf (getf (window-plist slider) :slider-info) :margins)
	    ;; left top bottom right (horizontal)
	    (list :min 0 :top top-margin :text bottom-margin :max 0))

      ;; With margins set we can now compute and save middle-length for efficiency
      (setf middle-length 
	    (- (if (eq :horizontal orientation)
		   width
		   height)
	       (slider-margin slider :min)
	       (first-tick-offset slider)
	       (last-tick-offset slider)
	       (slider-margin slider :max)
	       )))))


(defmethod initialize-instance :after ((slider slider) &key &allow-other-keys)
  (with-slots (font width height minimum maximum
		    dimensions min-text-width max-text-width) slider

    (setq font (find-font slider *default-display-text-font*)
	  dimensions (getf *slider-dimensions* (contact-scale slider)))
    
    (scale-update slider) ;; do some error checking, set min-text-width, etc.

    ;;  Initialize required geometry				 
    (multiple-value-setq (width height) (preferred-size slider))
    
    ;; Compute margins now that WIDTH & HEIGHT are known
    (slider-compute-margins slider)
    ))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                        Geometry Management                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(DEFMETHOD rescale :before ((slider slider))
  (with-slots (font dimensions) slider
    (setf font (find-font slider  *default-display-text-font*)
	  dimensions (getf *slider-dimensions* (contact-scale slider)))
    (slider-compute-margins slider)
    ))


(defmethod resize :after ((slider slider) new-width new-height new-border-width)
  ;; This method duplicates calculations started in (method initialize-instance :after (slider))
  ;; but are done here since they also must be performed when change-geometry is invoked.
  ;; Called when window-manager or someone else calls change-geometry.
  (declare (ignore new-width new-height new-border-width))
  (slider-compute-margins slider))


(defmethod preferred-size ((slider slider) &key width height border-width)
  (declare (ignore border-width)) ;; preferred-border-width is 0
  (with-slots (orientation min-text-width max-text-width font dimensions indicator-size
			   (current-height height) (current-width width)) slider
    (let* ((drag-box-width   (slidebar-drag-box-width dimensions))
	   (tick-mark-offset (slidebar-tick-mark-offset dimensions))

	   ;;  Min width  of slider with 2 positions = double size of drag box 
	   (minimum-double-width
	     (+ (- (first-tick-offset slider) tick-mark-offset)
		(max (* 2 drag-box-width)
		     (if (eq :off indicator-size)
			 0
			 (if (eq orientation :horizontal)
			     ;; room needed to display text between first/last-tick
			     (+ (ceiling min-text-width 2)
				(max-char-descent font) 
				(floor max-text-width 2))
			     ;; room needed to display 2 text lines (min & max) vertically,
			     ;; plus a small gap between
			     (+ (max-char-ascent font)
				(max-char-descent font) ;; gap between
				(max-char-ascent font)))))
		(- (last-tick-offset slider) tick-mark-offset)))
	   
	   ;; Calculate geometry assuming :horizontal orientation
	   (preferred-height
	     (max
	       ;; Suggested or current height
	       (if (eq orientation :horizontal)
		   (or height current-height)
		   (or width current-width))

	       ;; Total thickness of horizontal bar
	       (fixed-thickness slider :include-text-p t)))
	     
	   (preferred-width
	     (max
	       ;; Suggested or current width
	       (if (eq orientation :horizontal)
		   (or width current-width)
		   (or height current-height))

	       minimum-double-width))
	   )

      ;; Return preferred geometry according to actual orientation
      (if (eq orientation :horizontal) ;; preferred-border-width is always 0
	  (values preferred-width preferred-height 0)
	  (values preferred-height preferred-width 0))
	)))

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                          Event Translations                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defevent slider (:button-press :button-1)   slider-press)
(defevent slider (:button-release :button-1) slider-release)
(defevent slider (:motion-notify :button-1)  slider-handle-motion)

(defun slider-release (slider)
  (declare (special *slider-pressed-p*))
  (when (boundp '*slider-pressed-p*)
    (throw-action slider :release t)))

(defun highlite-drag-box (slider gc x y width height gap)
  (draw-rectangle slider gc (+ gap 1 gap x) (+ gap 1 gap y)
		  (- width (* 4 gap) 3) (- height (* 4 gap) 3) :fill-p))

(defun slider-press (slider)
  (with-event (x y)
    
    (with-slots
      (foreground orientation update-delay minimum maximum
       increment width height display value dimensions)
      slider

      (let (*slider-pressed-p* )
	(declare (special *slider-pressed-p*))
		   
	(multiple-value-bind (drag-x drag-y drag-width drag-height)
	  (drag-box-position slider)
	
	(when
	  (cond
	    ((and (>= x drag-x) (< x (+ drag-x drag-width))
		  (>= y drag-y) (< y (+ drag-y drag-height)))
	     
	     ;; SELECT on drag box
	     (let ((*highlight-pixel* (logxor foreground (contact-current-background-pixel slider)))
		   (gap (slidebar-gap dimensions)))
	       (declare (special *highlight-pixel*)) ;; use this in display method while moving ..
	       (using-gcontext
		 (gc :drawable slider
		     :function boole-xor
		     :foreground *highlight-pixel*)
		 
		 ;; Highlight drag area
		 (highlite-drag-box slider gc drag-x drag-y drag-width drag-height gap)  
		 
		 ;; Set timer for update
		 (when (and (numberp update-delay) (plusp update-delay))
		   (add-timer slider :update-delay update-delay))
		 
		 (apply-callback slider :begin-continuous)
		 (catch :release
		   (let ((*previous-position* (if (eq :vertical orientation) y x)))
		     (declare (special *previous-position*))
		     (loop (process-next-event display))))
		 (apply-callback slider :end-continuous)
		 
		 ;; Unhighlight drag area.
		 (multiple-value-bind (new-drag-x new-drag-y)
		     (drag-box-position slider)
		   (highlite-drag-box slider gc new-drag-x new-drag-y drag-width drag-height gap))))
	     t)

	  ;;  SELECT on bar
	  ;;  Since it is NOT in drag-box, just check if it is in bar
	  ;;  or the area of the bar if it had the thickness of the drag-box.
	  ;;  This makes clicking somewhat easier.
	  ((multiple-value-bind (bar-x bar-y bar-width bar-height)
	       (if (eq orientation :horizontal)
		   (values (slider-margin slider :min) (slider-margin slider :top)
			   (- width (slider-margin slider :min) (slider-margin slider :max))
			   drag-height)
		   (values (slider-margin slider :top) (slider-margin slider :min)
			   drag-width
			   (- height (slider-margin slider :min) (slider-margin slider :max))))
	     (and (>= x bar-x) (>= y bar-y)
		  (< x (+ bar-x bar-width))
		  (< y (+ bar-y bar-height))))

	   ;; Advance drag-box one increment in direction indicated.
	   ;; User may click so fast that the drag box passes the click
	   ;; position, thus inadvertently reversing the increment direction.
	   ;; Synchronize by using current pointer position, not click position.
	   (multiple-value-bind (ptr-x ptr-y) (pointer-position slider)
	     
	     (let ((delta (if (if (eq orientation :horizontal)
				  (< ptr-x drag-x)
				  (>= ptr-y (+ drag-y drag-height))) 
			      (- increment)
			      increment))		
		   (gap   (slidebar-gap dimensions)))
	       
	       (slider-increment-value slider delta)
	       
	       ;; Must warp pointer to stay in MIN (or MAX) bar, if necessary
	       (multiple-value-bind (new-drag-x new-drag-y drag-width drag-height)
		   (drag-box-position slider)
		 
		 (multiple-value-bind (warp-x warp-y)
		     (if (eq orientation :horizontal)
			 (if (plusp delta)
			     (let ((min-x (min (1- width) (+ new-drag-x drag-width gap))))
			       (when (< ptr-x min-x)
				 (values min-x ptr-y)))
			     (let ((max-x (max 0 (- new-drag-x gap))))
			       (when (< max-x ptr-x)
				 (values max-x ptr-y))))
			 
			 (if (minusp delta)
			     (let ((min-y (min (1- height) (+ new-drag-y drag-height gap))))
			       (when (< ptr-y min-y)
				 (values ptr-x min-y)))
			     (let ((max-y (max 0 (- new-drag-y gap))))
			       (when (< max-y ptr-y)
				 (values ptr-x max-y)))))
		   (when warp-x
		     (warp-pointer slider warp-x warp-y))))))
	   t))

	  ;; Report final value, if necessary		   		    
	  (unless (eql 0 update-delay)
	    (delete-timer slider :update-delay)
	    (apply-callback slider :new-value value))))))))



(defun slider-increment-value (slider scale-increment)
  "Convert the scale-increment to a (possibly) new scale position
and (possibly) cause the slider to be updated."

  (with-slots (value orientation increment minimum maximum update-delay) slider
    ;;  Must use truncate for negative scale-increment's - rounds to zero.
    (let* ((new-value (+ value scale-increment))
	   (adjusted  (confine-to (or (apply-callback slider :adjust-value new-value)
				      new-value)
				  minimum maximum)))

      (unless (= value adjusted)   ;; unless no change in slider scale occurs

	(setf (scale-value slider) adjusted) ;; <- this calls scale-update & redisplays slider

	(when (eql 0 update-delay)
	  (apply-callback slider :new-value adjusted))))))


(defun slider-handle-motion (slider)
  (declare (special *previous-position*))
  (when (boundp '*previous-position*)
    (with-slots (orientation increment) slider   
      (with-event (state x y)       
	(multiple-value-bind (ptr-x ptr-y)
	    ;;  Is :button-1 still down?
	    (if (plusp (logand state #.(make-state-mask :button-1)))
		
		;; Yes, query current pointer position
		(pointer-position slider)
		
		;; No, use final x,y returned for button transition
		(values x y))
	  
	  (let
	    ((modulo-increment
	       (* (truncate
		    (pixels-to-units
		      slider
		      (if (eq :horizontal orientation)
			  (- ptr-x *previous-position*)
			  
			  ;; Must swap order of subtraction since positive y direction
			  ;; is negative scale direction for :vertical slider
			  (- *previous-position* ptr-y)))
		    increment)
		  increment)))
	    
	    ;;  Convert the pixel motion to a suitable slider scale motion
	    (unless (zerop modulo-increment)
	      (slider-increment-value slider modulo-increment)
	      ;;  Use drag-box position. Ptr position is only correct if the drag-box can
	      ;;  move to the ptr posiiton without bumping up against the min/max limits.
	      (setf *previous-position*
		    (if (eq orientation :horizontal)
			(drag-box-center-x slider)
			(translate-x-to-y (drag-box-center-x slider) 1 slider))
		    ))))))))

(defun choose-indicator-size (slider)
  "Returns TICK-LIMIT = the number of ticks to draw."
  ;; Called when indicator-size eq :off to automatic tick-marks

  (with-slots (maximum minimum increment) slider
    (let* ((tick-mark-thickness (slider-tick-mark-thickness slider))
	   (increments-in-tick	1)
	   (min-visible-width   (* 2 tick-mark-thickness))
	   (ticks               (floor (- maximum minimum) increment))
	   )
      ;;  Return appropriate tick-limit
      (values
	(1+ 	;; 1+ since we draw the first-tick plus any calculated ticks
	  (do* ((ticks-visible nil))
	       ((cond
		  ((<= ticks 1) (setq ticks-visible 1)) ;; reached minimum ticks, 1 at each end
		  ((>= (units-to-pixels slider (* increments-in-tick increment))
		       min-visible-width)
		   (setq ticks-visible ticks)))
		;; Exit form
		(return ticks-visible))
	    	  
	    (setq ticks (floor ticks 2)
		  increments-in-tick (* 2 increments-in-tick))))

	increments-in-tick ;; 2nd return value
	)))) 

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               Display                                      |
;;;                                                                            |
;;;----------------------------------------------------------------------------+
;;; d1 = first-tick-x 
;;; d2 = slidebar-tick-mark-offset 
;;; 
;;;     min  fill  drag   empty   max
;;; +-----------------------------------+ -
;;; |                                   |  gap
;;; |             +----+                | -
;;; |             |   ||                |  drag-bar-offset 
;;; |   .-- ----- |   || -------- --.   | -
;;; |   |** ***** |   ||            |   |  bar-thickness
;;; |   `-- ----- |   || -------- --'   | -
;;; |             |---+|                |
;;; |<----->      +----+                |
;;; | d1  ||        ||           ||     |  bar-text-offset	
;;; |   <-->                            |
;;; |     d2                            |
;;; |    MIN                     MAX    |
;;; +-----------------------------------+ -
;;;     ^^ ^     ^       ^       ^^ ^
;;;     || |     |       |       || +-- bar-max-x
;;;     || |     |       |       |+----   (<= last-tick-x MAX-UPPER-EDGE bar-max-x)
;;;     || |     |       |       +----- last-tick-x
;;;     || |     |       +------------- drag-max-edge
;;;     || |     +--------------------- drag-min-edge
;;;     || +--------------------------- first-tick-x
;;;     |+-----------------------------   (<= bar-min-x MIN-LOWER-EDGE first-tick-x )
;;;     +------------------------------ bar-min-x
;;; 
;;; Note: No margins are shown except *slider-default-margin* 
;;;
;;;
;;;    .--.
;;;    |  |   max
;;;
;;;    |  |
;;;    |  |
;;;    |  |   empty
;;;    |  |
;;;
;;;   +----+
;;;   |    |
;;;   |    |  drag
;;;   |    |
;;;   +----+
;;;
;;;    |**|
;;;    |**|   fill
;;;    |**|
;;;    |**|
;;;
;;;    |**|   min
;;;    `--'
;;;
  
(defmethod display ((slider slider) &optional at-x at-y at-width at-height &key) 
  (with-slots (dimensions width height foreground orientation
			  minimum maximum increment middle-length sensitive
			  min-text-width max-text-width indicator-size font) slider
    ;; Default exposed rectangle, if necessary
    (setf at-x      (or at-x      0)
	  at-y      (or at-y      0)
	  at-width  (or at-width  (- width at-x))
	  at-height (or at-height (- height at-y)))
    (let* ((drag-box-width  (slidebar-drag-box-width dimensions))
	   (drag-bar-offset (slidebar-bar-drag-offset dimensions))
	   (gap 	    (slidebar-gap dimensions))
	   (bar-thickness   (slidebar-bar-thickness dimensions))
	   (bar-image       (getf (getf *slider-bar-images* :masks) (contact-scale slider)))
	   (image-half-size (floor (image-width bar-image) 2))  ;; image is BOTH ends, use min half
	   (first-tick-x    (first-tick-x slider))
	   (last-tick-x     (+ first-tick-x middle-length))
	   (bar-y           (+ (slider-margin slider :top) *slider-default-margin*
			       gap drag-bar-offset))
	   (bar-min-x       (- first-tick-x ;; ZRP
			       (slidebar-tick-mark-offset dimensions)))
	   (bar-max-x       (+ last-tick-x (slidebar-tick-mark-offset dimensions)))
	   (drag-min-edge   (drag-box-min-x slider))
	   (drag-max-edge   (+ drag-min-edge gap drag-box-width gap))
	   (end-portion     (min image-half-size (max 0 (- drag-min-edge bar-min-x)))) ;; for min end ONLY
	   (min-lower-edge  (+ bar-min-x end-portion))
	   (max-upper-edge  (max (- bar-max-x image-half-size) drag-max-edge))
	   (mask            (contact-image-mask slider bar-image
						:foreground foreground
						:background (contact-current-background-pixel slider)))
	   (inactive-p      (not (sensitive-p slider)))
	   (scale 	    (contact-scale slider))
	   )

      ;; First draw the bar outline, then
      ;; draw the tick-marks, draw the tick-text, and
      ;; finally, fill the bar then blt the drag-box to the correct position.
      (using-gcontext (gc :drawable slider
			  :font font
			  :exposures :off
			  :foreground (if inactive-p
					  (logxor foreground (contact-current-background-pixel slider))
					  foreground)
			  :fill-style (when inactive-p :stippled)
			  :function   (when inactive-p boole-xor)
			  ;; Use 50%gray, since 25%gray looks bad (bar disappears) for args :
			  ;; (make-slider :width 200 :height 200 :maximum 4 :orientation :vertical :scale :medium)
			  :stipple    (when inactive-p (contact-image-mask slider 50%gray :depth 1))
			  :clip-mask  (list at-x at-y at-width at-height)
			  )
	;; Draw MIN end - if it will be visible after drag-box is drawn later.
	;; The zero reference point (ZRP) is the center of the first tick-mark.
	;; If at this position we just draw the drag-box at ZRP after subtracting
	;; the half-width of the drag-box to get the coordinate of the left edge.
	;; Actually the image blt to the slider also contains a gap, but the ZRP is
	;; situated such that it centers the drag-box at the extreme min position.
	
	;;  Draw (at least part of) MIN
	(when (> drag-min-edge bar-min-x) ;; drag-box is NOT less than gap away from MIN edge
	  (multiple-value-bind (src-x src-y -width -height dst-x dst-y)
	      (if (eq orientation :horizontal)
		  (values 0 0 end-portion (image-width bar-image) bar-min-x bar-y)
		  (values 0 (- (image-width bar-image) image-half-size) ;; image may have odd # of pixels
			  (image-width bar-image) end-portion
			  bar-y (translate-x-to-y bar-min-x end-portion slider)))
	    (when (area-overlaps-p at-x at-y at-width at-height dst-x dst-y -width -height)
	      (if inactive-p
		  (draw-rectangle slider gc dst-x dst-y -width -height :fill-p)
		  (copy-area mask gc src-x src-y -width -height slider dst-x dst-y))))
	  	  
	  ;; Draw FILL, if any
	  (when (> drag-min-edge min-lower-edge)
	    (multiple-value-bind (x y -width -height)
		(if (eq orientation :horizontal)
		    (values min-lower-edge bar-y (- drag-min-edge min-lower-edge) bar-thickness)
		    (values bar-y (translate-x-to-y min-lower-edge (- drag-min-edge min-lower-edge) slider)
			    bar-thickness (- drag-min-edge min-lower-edge)))
	    (when (area-overlaps-p at-x at-y at-width at-height x y -width -height)
	      (draw-rectangle slider gc x y -width -height :fill-p))))
	    )
	
	;; Draw EMPTY portion, if any
	(when (> max-upper-edge drag-max-edge)
	  (multiple-value-bind (x y x2 y2 x3 y3 x4 y4)
	      (if (eq orientation :horizontal)
		  (values drag-max-edge bar-y 					;; x y
			  max-upper-edge bar-y					;; x2 y2
			  drag-max-edge (+ bar-y bar-thickness -1)		;; x3 y3
			  max-upper-edge (+ bar-y bar-thickness -1))		;; x4 y4
		  (values bar-y (translate-x-to-y drag-max-edge 1 slider)	;; x y
			  bar-y (translate-x-to-y max-upper-edge 1 slider)	;; x2 y2
			  (+ bar-y bar-thickness -1) (translate-x-to-y drag-max-edge 1 slider) 		;; x3 y3
			  (+ bar-y bar-thickness -1) (translate-x-to-y max-upper-edge 1 slider)))	;; x4 y4
	    (when (area-overlaps-p at-x at-y at-width at-height x y (- x2 x) bar-thickness)
	      (draw-segments slider gc (list x y x2 y2 x3 y3 x4 y4))))
	  )

	  ;; Draw MAX, or portion not obscured by drag-box
	(when (plusp (setq end-portion (min image-half-size (- bar-max-x max-upper-edge))))
	  (setq mask (contact-image-mask slider (getf (GETF *slider-bar-images* :borders) scale)
					 :foreground foreground
					 :background (contact-current-background-pixel slider)))
	  (multiple-value-bind (src-x src-y -width -height dst-x dst-y)
	      (if (eq orientation :horizontal)
		  (values (- (image-width bar-image) end-portion) 0
			  end-portion (image-width bar-image)
			  max-upper-edge bar-y)
		  (values 0 0
			  (image-width bar-image) end-portion
			  bar-y (translate-x-to-y max-upper-edge end-portion slider)))
	    (when (area-overlaps-p at-x at-y at-width at-height dst-x dst-y -width -height)
	      (if inactive-p
		  (draw-rectangle slider gc dst-x dst-y -width -height :fill-p)
		  (copy-area mask gc src-x src-y -width -height slider dst-x dst-y))))
	  )

	;; Draw TICK-TEXT, the labels for MIN and MAX
	(unless (eq :off indicator-size) ;; don't draw tick marks or tick text

	  (let ((min-thickness (+ (slider-margin slider :top)
				  (fixed-thickness slider :include-text-p nil)))
		(text-x-offset (first  (getf (slidebar-bar-text-offset dimensions) orientation)))
		(text-y-offset (second (getf (slidebar-bar-text-offset dimensions) orientation))))
	    
	    (multiple-value-bind (x-min y-min x-max y-max)
		(if (eq orientation :horizontal)
		    (values (+ (slider-margin slider :min)
			       text-x-offset
			       ;; center text at :first-tick
			       (+ (first-tick-offset slider)
				  (- (floor min-text-width 2))))
			    min-thickness
			    (- width
			       (slider-margin slider :max)
			       (last-tick-offset slider)
			       (ceiling max-text-width 2))  ;; scoot left to fit on odd widths!
			    min-thickness)
		    (values min-thickness
			    (+ (slider-margin slider :max)
			       (last-tick-offset slider)
			       middle-length
			       (slidebar-tick-mark-offset dimensions)
			       (- gap))
			    min-thickness
			    (+ (slider-margin slider :max)
			       (last-tick-offset slider)
			       (- (slidebar-tick-mark-offset dimensions))
			       text-y-offset)))
	      (let* ((font-ascent (max-char-ascent font))
		     (font-height (+ font-ascent (max-char-descent font))))
		(when (if (eq orientation :horizontal)
			  (area-overlaps-p at-x at-y at-width at-height
					   x-min (- y-min font-ascent)
					   (+ (- x-max x-min) max-text-width)
					   font-height)
			  (area-overlaps-p at-x at-y at-width at-height
					   x-max (- y-max font-ascent)
					   (max min-text-width max-text-width)
					   (+ (- y-min y-max) font-height)))
		  ;; Draw TICK-TEXT for min and max	    
		  (draw-glyphs slider gc x-min y-min (format nil "~a" minimum))
		  (draw-glyphs slider gc x-max y-max (format nil "~a" maximum))))))
	  
	  ;; Draw TICK-MARKS
	  (multiple-value-bind (tick-limit increments-in-tick)
	      (if (plusp indicator-size)
		  (values (1+ (floor (- maximum minimum) (* increment indicator-size)))
			  indicator-size)
		  (choose-indicator-size slider)) ;; automatic tick marks
	    (do* ((tick 0 (incf tick))
		  (tick-thickness (slider-tick-mark-thickness slider))
		  (tick-x (+ (first-tick-x slider) (- (floor tick-thickness 2))) ;; adjust from center to edge of tick
			  (+ (first-tick-x slider) (- (floor tick-thickness 2))
			     (units-to-pixels slider (* tick increments-in-tick increment))))
		  (tick-y (+ (slider-margin slider :top)
			     (bar-bottom-offset slider)
			     (slider-bar-tick-gap slider)))
		  (tick-height (slidebar-tick-mark-length dimensions)))
		 
		 ((= tick tick-limit)) ;; draw tick @min plus TICK-LIMIT more

	      (multiple-value-bind (x y -width -height)
		  (if (eq orientation :horizontal)
		      (values tick-x tick-y
			      tick-thickness tick-height)
		      (values tick-y (translate-x-to-y tick-x tick-thickness slider)
			      tick-height tick-thickness))
		(when (area-overlaps-p at-x at-y at-width at-height x y -width -height)
		  (draw-rectangle slider gc x y -width -height :fill-p)
		  ))))
	  )

	;; Draw DRAG BOX (possibly over a tick mark)
	(let ((drag-image (getf (getf *slider-drag-box-images* orientation) scale)))
	  (setq mask (contact-image-mask slider drag-image 
					 :foreground foreground
					 :background (contact-current-background-pixel slider)))
	  (multiple-value-bind (src-x src-y -width -height dst-x dst-y)
	      (if (eq orientation :horizontal)
		  (values 0 0 (image-width drag-image) (image-height drag-image)
			  drag-min-edge (- bar-y drag-bar-offset gap))
		  (values 0 0 (image-width drag-image) (image-height drag-image)
			  (- bar-y drag-bar-offset gap)
			  (translate-x-to-y drag-min-edge (image-height drag-image) slider)))
	    (when (area-overlaps-p at-x at-y at-width at-height
				   dst-x dst-y -width -height)

	      (if inactive-p
		  (draw-rectangle slider gc dst-x dst-y -width -height :fill-p)
		  (copy-area mask gc src-x src-y -width -height slider dst-x dst-y))
	      (when (boundp '*highlight-pixel*)
		(special-highlite-drag-box slider gc dst-x dst-y -width -height gap)))))))))

;;; Crock! This function could be inlined, except that causes the Explorer compiler
;;; to barf on (method display (slider)) when using R4 CLX. 
(defun special-highlite-drag-box (slider gc dst-x dst-y -width -height gap)
  (declare (special *highlight-pixel*))
  (with-gcontext (gc :function boole-xor :foreground *highlight-pixel*)
    ;; Highlight drag area while button is still down )
    (highlite-drag-box slider gc dst-x dst-y -width -height gap)))
