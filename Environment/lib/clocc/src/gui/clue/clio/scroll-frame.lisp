;;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


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

(export '(scroll-frame
	  make-scroll-frame
	  scroll-frame-area
	  scroll-frame-content
	  scroll-frame-horizontal
	  scroll-frame-left
	  scroll-frame-position
	  scroll-frame-reposition
	  scroll-frame-top
	  scroll-frame-vertical
	  ))



(defcontact scroll-frame (core composite)
  ((horizontal               :type      switch
			     :initform  :on
			     :initarg   :horizontal
			     :reader    scroll-frame-horizontal) ; setf defined below
   (left		     :type      integer
			     :initform  0
			     :initarg   :left
			     :accessor  scroll-frame-left)
   (top                      :type      integer
			     :initform  0
			     :initarg   :top
			     :accessor  scroll-frame-top)
   (vertical                 :type      switch
			     :initform  :on
			     :initarg   :vertical
			     :reader    scroll-frame-vertical))	 ; setf defined below
  (:resources
    (border-width :initform 0)
    (content      :type (or function list) :initform nil)
    horizontal
    left
    top
    vertical)
  (:documentation
    "Provide horizontal and/or vertical scrolling controls for an arbitrary content contact"))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Accessors                                     |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod scroll-frame-content ((scroll-frame scroll-frame))
  (first (composite-children (scroll-frame-area scroll-frame))))

(proclaim '(inline scroll-frame-hscroller))
(defun scroll-frame-hscroller (scroll-frame)
  (with-slots (children) scroll-frame
    (find :hscroller children
	  :key #'contact-name
	  :test #'eq)))

(proclaim '(inline scroll-frame-vscroller))
(defun scroll-frame-vscroller (scroll-frame)
  (with-slots (children) scroll-frame
    (find :vscroller children
	  :key #'contact-name
	  :test #'eq)))


(defmethod scroll-frame-area ((scroll-frame scroll-frame))
  (with-slots (children) scroll-frame
    (find :scroll-area children
	  :key #'contact-name
	  :test #'eq)))

(defmethod (setf contact-foreground) :after (value (self scroll-frame))
  (declare (ignore value))
  (with-slots (foreground) self
    (let ((hscroller (scroll-frame-hscroller self))
	  (vscroller (scroll-frame-vscroller self)))
      (when hscroller
	(setf (contact-foreground hscroller) foreground))
      (when vscroller
	(setf (contact-foreground vscroller) foreground)))
    
    (setf (window-border (scroll-frame-area self)) foreground)))


(defmethod (setf scroll-frame-vertical) (value (self scroll-frame))
  (with-slots (foreground top vertical) self
    (setf vertical value)
    
    (let ((vscroller (scroll-frame-vscroller self))
	  (content   (scroll-frame-content self)))
      
      (ecase value
	(:on
	 (if vscroller
	     ;; Map existing scroller
	     (setf (contact-state vscroller) :mapped)

	     (progn
	       ;; Create a new scroller
	       (setf vscroller (make-scroller :parent self
					      :name :vscroller
					      :foreground foreground
					      :border-width 0
					      :orientation :vertical))
	       
	       ;; Program scroller to scroll content
	       (add-callback vscroller :new-value
			     #'(lambda (new-top scroll-frame) 
				 (with-slots (left top) scroll-frame
				   (unless (= new-top top)
				     (sf-scroll-to
				       scroll-frame
				       left
				       (setf top new-top)))))
			     self)))

	 ;; Calibrate scroller with current content
	 (when content	   	   
	   (sf-vertical-calibrate
	     content vscroller top (contact-height (scroll-frame-area self)))))
	
	(:off
	 (when vscroller
	   (setf (contact-state vscroller) :withdrawn)))))

    value))


(defmethod (setf scroll-frame-horizontal) (value (self scroll-frame))
  (with-slots (foreground left horizontal) self
    (setf horizontal value)

    (let ((hscroller (scroll-frame-hscroller self))
	  (content   (scroll-frame-content self)))
      
      (ecase value
	(:on
	 (if hscroller
	     ;; Map existing scroller
	     (setf (contact-state hscroller) :mapped)

	     (progn
	       ;; Create a new scroller
	       (setf hscroller (make-scroller :parent self
					      :name :hscroller
					      :foreground foreground
					      :border-width 0
					      :orientation :horizontal))
	       
	       ;; Program scroller to scroll content
	       (add-callback hscroller :new-value
			     #'(lambda (new-left scroll-frame) 
				 (with-slots (left top) scroll-frame
				   (unless (= new-left left)
				     (sf-scroll-to
				       scroll-frame
				       (setf left new-left)
				       top))))
			     self)))
	 
	 ;; Calibrate scroller with current content
	 (when content	   
	   (sf-horizontal-calibrate
	     content hscroller left (contact-width (scroll-frame-area self)))))
	
	(:off
	 (when hscroller
	   (setf (contact-state hscroller) :withdrawn)))))

    value))
 

(defmethod scroll-frame-position ((self scroll-frame))
  (with-slots (left top) self
    (values left top)))

(defmethod scroll-frame-reposition ((self scroll-frame) &key left top)
  "Changes the horizontal/vertical position of the content (in content
units) which appears at the left/top edge of the scroll-frame.  The
final content position (possibly adjusted via :horizontal-adjust and
:vertical-adjust callbacks) is returned."
  (with-slots ((current-left left) (current-top top) vertical horizontal) self
    (let*
      ((content        (scroll-frame-content   self))
       (left-changed-p (and
			 left
			 (/= (setf left (apply-callback-else (content :horizontal-adjust left) left))
			     current-left)))
       (top-changed-p  (and
			 top
			 (/= (setf top (apply-callback-else (content :vertical-adjust top) top))
			     current-top))))
      (when left-changed-p
	(setf current-left left)
	(when (eq :on horizontal)
	  (setf (scale-value (scroll-frame-hscroller self)) current-left)))
      
      (when top-changed-p
	(setf current-top top)
	(when (eq :on vertical)
	  (setf (scale-value (scroll-frame-vscroller self)) current-top)))
      
      (when (or left-changed-p top-changed-p)
	;; Redisplay content at new position
	(sf-scroll-to self current-left current-top))
    
    (values current-left current-top))))



(defun sf-scroll-to (scroll-frame left top)
  (let ((content (scroll-frame-content scroll-frame)))
    (when content
      (apply-callback-else (content :scroll-to left top)
	
	;; Default scrolling by moving content window w.r.t area.
	;; Content units are n pixels, where n is determined from
	;; pixels-per-unit used to calibrate scroller indicator size.
	(let ((hscroller (scroll-frame-hscroller scroll-frame))
	      (vscroller (scroll-frame-vscroller scroll-frame))
	      (area      (scroll-frame-area scroll-frame)))	    
	  (with-state (content)
	    (move content
		  (- (pixel-round (if hscroller
				      (/ (* left (contact-width area))
					 (scale-indicator-size hscroller))
				      left)))
		  (- (pixel-round (if vscroller
				      (/ (* top (contact-height area))
					 (scale-indicator-size vscroller))
				      top))))))))))



(defun sf-horizontal-calibrate (content hscroller left width)        
  ;; Program scroller to adjust value
  (add-callback hscroller :adjust-value
		#'(lambda (value content)
		    (or (when content
			  (apply-callback content :horizontal-adjust value))
			value))
		content)
  
  ;; Update scroller values
  (multiple-value-bind (min max ppu)
      (apply-callback-else (content :horizontal-calibrate)
	(values 0 (max 0 (- (contact-width content) width)) 1))
    
    ;; Clamp current left to new range
    (let ((value (min max left)))
      (scale-update hscroller
		    :value          value
		    :minimum        min
		    :maximum        max
		    :indicator-size (/ width ppu)
		    :increment      1)

      ;; Returned clamped value
      value)))

(defun sf-vertical-calibrate (content vscroller top height)
  ;; Program scroller to adjust value
  (add-callback vscroller :adjust-value
		#'(lambda (value content)
		    (or (when content
			  (apply-callback content :vertical-adjust value))
			value))
		content)
  
  ;; Update scroller values
  (multiple-value-bind (min max ppu)
      (apply-callback-else (content :vertical-calibrate)
	(values 0 (max 0 (- (contact-height content) height)) 1))
    (let ((value (min max top)))
      (scale-update vscroller
		    :value          value
		    :minimum        min
		    :maximum        max
		    :indicator-size (/ height ppu)
		    :increment      1)
      
      ;; Return clamped value
      value))) 



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                         Geometry Management                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod change-layout ((self scroll-frame) &optional newly-managed)
  (declare (ignore newly-managed))
  (with-slots (width height horizontal vertical) self
    
    ;; Is initial scroll-frame size still undefined?
    (if (unless (realized-p self) (or (zerop width) (zerop height)))
	
	;; Yes, change to valid initial size (this invokes change-layout again)
	(multiple-value-bind (preferred-width preferred-height)
	    (preferred-size self)
	  (change-geometry
	    self :width preferred-width :height preferred-height :accept-p t))
	
	;; No, update layout for valid size.      
	(let*
	  ((hscroller (when (eq :on horizontal) (scroll-frame-hscroller self)))
	   (vscroller (when (eq :on vertical)   (scroll-frame-vscroller self)))
	   (area      (scroll-frame-area   self))
	   (hheight   (if hscroller (contact-height hscroller) 0))	    
	   (vwidth    (if vscroller (contact-width  vscroller) 0))
	   (hwidth    (max 0 (- width vwidth)))
	   (vheight   (max 0 (- height hheight)))	    
	   (abw       (* 2 (contact-border-width area))))
	  
	  ;; Lay out scrollers
	  (when hscroller
	    (with-state (hscroller)
	      (resize hscroller hwidth hheight 0)
	      (move hscroller 0 (- height hheight))))
	  (when vscroller
	    (with-state (vscroller)
	      (resize vscroller vwidth vheight 0)
	      (move vscroller (- width vwidth) 0)))
	  
	  ;; Layout scroll area
	  (with-state (area)
	    (resize area
		    (max 0 (- width vwidth abw))
		    (max 0 (- height hheight abw))
		    (contact-border-width area))
	    (move area 0 0))
	  ))))

(defmethod manage-geometry ((self scroll-frame) child x y width height border-width &key)  
  
  (case (contact-name child)
    (:scroll-area     
     ;; Approve if total outside size/position remains unchanged.
     (let* ((approved-bw     (or border-width (contact-border-width child)))
	    (delta-bw        (* 2 (- (contact-border-width child) approved-bw)))     
	    (approved-x      0)
	    (approved-y      0)
	    (approved-width  (+ (contact-width child) delta-bw))
	    (approved-height (+ (contact-height child) delta-bw)))
       
       
       (values
	 (when
	   ;; Change approved?
	   (and
	     (or (null x)      (= x approved-x))
	     (or (null y)      (= y approved-y))
	     (or (null width)  (= width approved-width))
	     (or (null height) (= height approved-height))
	     (= border-width approved-bw))
	   
	   ;; Yes, update layout if change is performed	  
	   'change-layout)
	 approved-x
	 approved-y
	 approved-width
	 approved-height
	 approved-bw)))

    (otherwise
     ;; Approve any scroller size change. This should happen only during rescale.
     (values
       (when
	 (and 
	   (or (null border-width) (= border-width (contact-border-width child)))
	   (or (null x)            (= x (contact-x child)))
	   (or (null y)            (= y (contact-y child))))
	 'change-layout)
       (contact-x child)
       (contact-y child)
       (or width (contact-width child))
       (or height (contact-height child))
       (contact-border-width child)))))

(defmethod preferred-size ((self scroll-frame) &key width height border-width)  
  (with-slots ((self-width width) (self-height height) (self-border-width border-width)) self

    (let ((suggested-width        (or width self-width))
	  (suggested-height       (or height self-height))
	  (suggested-border-width (or border-width self-border-width)))
      (values
	(if (plusp suggested-width)
	    suggested-width
	    (let ((content   (scroll-frame-content self))
		  (hscroller (scroll-frame-hscroller self))
		  (vscroller (scroll-frame-vscroller self)))
	      (+ (max (if content   (contact-width content)   0)
		      (if hscroller (contact-width hscroller) 0))
		 (if vscroller (contact-width vscroller) 0))))

	(if (plusp suggested-height)
	    suggested-height
	    (let ((content   (scroll-frame-content self))
		  (hscroller (scroll-frame-hscroller self))
		  (vscroller (scroll-frame-vscroller self)))
	      (+ (max (if content   (contact-height content)   0)
		      (if vscroller (contact-height vscroller) 0))
		   (if hscroller (contact-height hscroller) 0))))
	suggested-border-width ))))

(defmethod resize :after ((self scroll-frame) new-width new-height new-border-width)
  (declare (ignore new-width new-height new-border-width))
  (change-layout self)) 

(defmethod add-child :before ((self scroll-frame) child &key)
  (assert (member (contact-name child) '(:hscroller :vscroller :scroll-area) :test #'eq) ()
	  "A scroll-frame does not allow you to define new children."))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Initialization                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defun make-scroll-frame (&rest initargs)
  (apply #'make-contact 'scroll-frame initargs))
    

(defmethod initialize-instance :after ((self scroll-frame) &key content &allow-other-keys)
  (with-slots (foreground vertical horizontal) self
    (let (;; Create scroll area
	  (area (make-contact 'scroll-area
			      :parent self
			      :name :scroll-area
			      :border-width 1
			      :border foreground)))
      ;; Create content, if given.
      (when content
	(multiple-value-bind (content-constructor content-initargs)
	    (etypecase content
	      (function content) 
	      (list (values (first content) (rest content))))
		
	  (apply content-constructor
		 :name (or (getf content-initargs :name) :content)
		 :parent area
		 content-initargs))))
    
    ;; Initialize scroll bars
    (setf (scroll-frame-horizontal self) horizontal)
    (setf (scroll-frame-vertical self) vertical)))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Scroll Area                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(defcontact scroll-area (composite) ()
  (:documentation "Geometry manager for the scroll area of a scroll frame."))


;;; Geometry management policy:
;;;     1. Content border width forced to 0. This prevents the bottom/right edges
;;;        of a small content from intruding.
;;;     2. Content size and position is unrestricted.
;;;     3. Only one content child allowed.

(defmethod add-child :before ((self scroll-area) child &key)
  (declare (ignore child))
  (assert (not (composite-children self)) ()
	  "A scroll area can have only one child."))
    
(defmethod change-layout ((self scroll-area) &optional newly-managed)
  (declare (ignore newly-managed))
  (with-slots (children (scroll-frame parent) width height) self
    (let ((content (first children)))
      (when content
	;; If realized, then recalibrate scrollers for new content
	;; (otherwise, not necessary since initial calibration will be done
	;; after initial scroll-area size is set).
	(when (realized-p self)
	  (sf-recalibrate scroll-frame))

	;; Define content callbacks used by application to report new calibration data
	(flet
	  ((horizontal-update
	     (&key position minimum maximum pixels-per-unit scroll-frame)
	     
	     ;; Recalibrate scroller, if necessary
	     (when (eq :on (scroll-frame-horizontal scroll-frame))
	       (scale-update
		 (scroll-frame-hscroller scroll-frame)
		 :value          position
		 :minimum        minimum
		 :maximum        maximum
		 :indicator-size (when pixels-per-unit
				   (/ (contact-width (scroll-frame-area scroll-frame))
				      pixels-per-unit))))
	     
	     ;; Update current scroll-frame position
	     (when position
	       (with-slots (left top) scroll-frame
	       (sf-scroll-to
		 scroll-frame
		 (setf left position)
		 top))))
	   
	   (vertical-update
	     (&key position minimum maximum pixels-per-unit scroll-frame)
	     
	     ;; Recalibrate scroller, if necessary
	     (when (eq :on (scroll-frame-vertical scroll-frame))
	       (scale-update
		 (scroll-frame-vscroller scroll-frame)
		 :value          position
		 :minimum        minimum
		 :maximum        maximum
		 :indicator-size (when pixels-per-unit
				   (/ (contact-height (scroll-frame-area scroll-frame))
				      pixels-per-unit))))
	     
	     ;; Update current scroll-frame position
	     (when position
	       (with-slots (left top) scroll-frame
		 (sf-scroll-to
		   scroll-frame
		   left
		   (setf top position))))))
	    
	  (add-callback content :horizontal-update
			#'horizontal-update
			:scroll-frame scroll-frame)
	  (add-callback content :vertical-update
			#'vertical-update
			:scroll-frame scroll-frame))

	;; Initialize content
	(with-state (content)
	  ;; Initialize content position (this may be changed later if
	  ;; default pixel scrolling is used)
	  (move content 0 0)
	  
	  ;; Force content border width to 0
	  (with-slots
	    ((content-width width) (content-height height)) content
	    (resize content content-width content-height 0)))))))



(defmethod manage-geometry ((self scroll-area) content x y width height border-width &key)
  (flet
    ((update-scroller-maximum
      (scroll-area)
      ;; Called when an approved content geometry change is performed. When default
      ;; scrolling is used, then scrollers must be updated to reflect new
      ;; pixel size of content w.r.t scroll-area. 
      (let ((content (first (composite-children scroll-area))))
	;; Default scrolling?
	(unless (callback-p content :scroll-to)
	  (let
	    ((frame (contact-parent scroll-area))
	     (max-h (max 0 (- (contact-width content) (contact-width scroll-area))))
	     (max-v (max 0 (- (contact-height content) (contact-height scroll-area)))))	   
	    (apply-callback
	      content :horizontal-update
	      :maximum  max-h
	      :position (min (scroll-frame-left frame) max-h))
	    (apply-callback
	      content :vertical-update
	      :maximum  max-v
	      :position (min (scroll-frame-top frame) max-v)))))))
		      
    (values
      (when (or (null border-width) (= border-width 0))
	#'update-scroller-maximum)
      (or x (contact-x content))
      (or y (contact-y content))
      (or width (contact-width content))
      (or height (contact-height content))
      0)))	


(defmethod resize :after ((self scroll-area) new-width new-height new-bw)
  (declare (ignore  new-width new-height new-bw))
  (with-slots (parent) self
    (let ((scroll-frame parent))
      
      (sf-recalibrate scroll-frame)
      
      (unless (realized-p self)	  
	;; Move content into initial position, now that content units have been
	;; defined.
	(sf-scroll-to
	  scroll-frame
	  (scroll-frame-left scroll-frame)
	  (scroll-frame-top scroll-frame))))))
  
(defun sf-recalibrate (scroll-frame)
  (let ((content (scroll-frame-content scroll-frame)))
    
    (when content      
      (with-slots (left top horizontal vertical) scroll-frame
	(with-slots (width height) (scroll-frame-area scroll-frame)

	  (let ((new-left left) (new-top top))
	    (let ((hscroller (when (eq :on horizontal) (scroll-frame-hscroller scroll-frame))))
	      (when hscroller
		(setf new-left (sf-horizontal-calibrate content hscroller left width))))
	    
	    (let ((vscroller (when (eq :on vertical) (scroll-frame-vscroller scroll-frame))))
	      (when vscroller
		(setf new-top (sf-vertical-calibrate content vscroller top height))))

	    (scroll-frame-reposition scroll-frame :left new-left :top new-top)))))))
