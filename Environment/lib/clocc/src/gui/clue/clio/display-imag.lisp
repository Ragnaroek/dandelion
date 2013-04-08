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
	  make-display-image
	  display-image
	  display-image-source
	  )
	'clio-open)


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               DISPLAY IMAGE                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact display-image (gravity-mixin core contact)
  ;; Source is what is given and/or returned by/to outside callers.
  ;; Internally, source is converted to source-pixmap for all other operations.
  ((source		 :type		(or null pixmap image)
			 :initform	nil
			 :initarg	:source
			 :reader	display-image-source 	;; SETF method defined below
			 )
   (source-pixmap	 :type		(or null pixmap)	;; internal storage
			 :initform	nil)
   (source-pixmap-width  :type		card16			;; internal storage
			 :initform	0)
   (source-pixmap-height :type		card16			;; internal storage
			 :initform	0) 	

   (compress-exposures   :initform       :off
		 	 :type           (member :off :on)
			 :reader         contact-compress-exposures
			 :allocation     :class))
  
  (:resources source))


(defmethod (setf display-gravity) (new-gravity (display-image display-image))
  (check-type new-gravity (or gravity (member :tiled)))
  (setf (slot-value display-image 'gravity) new-gravity))

  
(defmethod (setf display-image-source) ((new-source pixmap) (display-image display-image))
  (with-slots (source source-pixmap source-pixmap-width source-pixmap-height depth) display-image
    (with-state (new-source)
      (let ((source-depth (drawable-depth new-source)))
	(assert (or (= source-depth depth) (= source-depth 1)) ()
		"~a depth is ~a, which is neither 1 nor ~a." new-source source-depth depth)
	
	(setf source-pixmap-width  (drawable-width new-source)
	      source-pixmap-height (drawable-height new-source)
	      source-pixmap        (when (realized-p display-image)
				     (realize-display-image display-image new-source)))
	
	(setf source new-source)))))

(defmethod (setf display-image-source) ((new-source image) (display-image display-image))
  (with-slots (source source-pixmap source-pixmap-width source-pixmap-height foreground depth) display-image
    (let ((source-depth (image-depth new-source)))
      (assert (or (= source-depth depth) (= source-depth 1)) ()
	      "~a depth is ~a, which is neither 1 nor ~a." new-source source-depth depth)
      
      (setf source-pixmap-width  (image-width new-source)
	    source-pixmap-height (image-height new-source)
	    source-pixmap        (when (realized-p display-image)
				     (realize-display-image display-image new-source)))
      
      (setf source new-source))))

(defmethod (setf display-image-source) (new-source (display-image display-image))
  (assert (not new-source) () "New source is ~a, which is not NIL, a PIXMAP, or an IMAGE.")
  (with-slots (source source-pixmap source-pixmap-width source-pixmap-height) display-image
    (setf source-pixmap-width  0
	  source-pixmap-height 0
	  source-pixmap        nil) 
    
    (setf source new-source)))

(defmethod (setf display-image-source) :after (new-source (display-image display-image))
  (declare (ignore new-source))
  (when (realized-p display-image)
    (clear-area display-image)
    (display display-image)))

(defmethod realize-display-image ((display-image display-image) (new-source pixmap))
  (with-slots (source-pixmap-width source-pixmap-height foreground depth) display-image
    (if (= (drawable-depth new-source) depth)
	new-source
	
	;; Else expand bitmap to full depth pixmap.
	(let ((pixmap (create-pixmap
			:drawable display-image
			:width    source-pixmap-width
			:height   source-pixmap-height
			:depth    depth)))
	  (using-gcontext
	    (gc :drawable   display-image
		:foreground foreground
		:background (contact-current-background-pixel display-image))
	    (copy-plane new-source gc 1		; Note, this is a mask, not an index.
			0 0 source-pixmap-width source-pixmap-height
			pixmap 0 0))
	  pixmap))))

(defmethod realize-display-image ((display-image display-image) (new-source image))
  (with-slots (source-pixmap-width source-pixmap-height foreground depth) display-image
    (if (= (image-depth new-source) depth)
	(contact-image-pixmap display-image new-source)
	
	;; Else expand bitmap to full depth pixmap.
	(contact-image-mask display-image new-source
			    :foreground foreground
			    :background (contact-current-background-pixel display-image)))))

(defmethod realize :after ((display-image display-image))
  (with-slots (source source-pixmap) display-image
    (when source
      (setf source-pixmap (realize-display-image display-image source)))))

(defmethod (setf contact-foreground) :after (new-value (display-image display-image))
  (declare (ignore new-value))
  (with-slots (source source-pixmap) display-image
    (when (and source (realized-p display-image))
      (setf source-pixmap (realize-display-image display-image source)))))

(defmethod (setf contact-background) :after (new-value (display-image display-image))
  (declare (ignore new-value))
  (with-slots (source source-pixmap) display-image
    (when (realized-p display-image)
      (when source
	(setf source-pixmap (realize-display-image display-image source)))
	
      (clear-area display-image)
      (display display-image))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Initialization                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun make-display-image (&rest initargs &key &allow-other-keys)
  (apply #'make-contact 'display-image initargs))


(defmethod initialize-instance :after ((display-image display-image) &key source &allow-other-keys)
  (with-slots (width height) display-image
    ;; Insure that source-pixmap & source-pixmap-width & source-pixmap-height
    ;; get set up if the source arg is specified. Also check for valid source argument.
    (setf (display-image-source display-image) source)
    ;;  Initialize required geometry
    (when (or (zerop width) (zerop height))
      (multiple-value-bind (pwidth pheight)
	  (preferred-size display-image :width width :height height)
	(change-geometry display-image :width pwidth :height pheight)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                        Geometry Management                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod preferred-size ((display-image display-image) &key width height border-width)

  (with-slots
    ((current-border-width border-width) (current-height height) (current-width width) gravity
     source-pixmap-height source-pixmap-width)
    display-image

    (values
      ;;  Preferred-width
      (max (or width current-width) source-pixmap-width)

      ;;  Preferred-height
      (max (or height current-height) source-pixmap-height)

      ;;  Preferred-border-width
      (max 0 (or border-width current-border-width)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  DISPLAY                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

      

(defmethod display ((display-image display-image) &optional (exposed-x 0) (exposed-y 0) exposed-width exposed-height &key)
  (with-slots
    (source-pixmap source-pixmap-height source-pixmap-width gravity width height clip-rectangle)
    display-image
    
    (when source-pixmap
      (let ((exposed-width  (or exposed-width (- width exposed-x)))
	    (exposed-height (or exposed-height (- height exposed-y)))
	    (tiled-p        (eq gravity :tiled)))
	
	(using-gcontext
	  (gc :drawable   display-image
	      :exposures  :off
	      :clip-mask  clip-rectangle
	      :fill-style (when tiled-p :tiled) 
	      :tile       (when tiled-p source-pixmap)
	      :ts-x       (when tiled-p (display-clip-x display-image))	    
	      :ts-y       (when tiled-p (display-clip-y display-image)))
	  
	  (if tiled-p
	      (draw-rectangle
		display-image gc exposed-x exposed-y exposed-width exposed-height :fill-p)

	      (multiple-value-bind (extent-x extent-y)
		  (case gravity
		    (:north-west
		     (values
		       (display-clip-x display-image)
		       (display-clip-y display-image)))
		    
		    (:north
		     (values
		       (+ (display-clip-x display-image)
			  (pixel-round (- (display-clip-width display-image) source-pixmap-width) 2))
		       (display-clip-y display-image)))
		    
		    (:north-east
		     (values
		       (+ (display-clip-x display-image)
			  (- (display-clip-width display-image) source-pixmap-width))
		       (display-clip-y display-image)))
		    
		    (:west
		     (values
		       (display-clip-x display-image)
		       (+ (display-clip-y display-image)
			  (pixel-round (- (display-clip-height display-image) source-pixmap-height) 2))))
		    
		    (:center
		     (values
		       (+ (display-clip-x display-image)
			  (pixel-round (- (display-clip-width display-image) source-pixmap-width) 2))
		       (+ (display-clip-y display-image)
			  (pixel-round (- (display-clip-height display-image) source-pixmap-height) 2))))
		    
		    (:east
		     (values
		       (+ (display-clip-x display-image)
			  (- (display-clip-width display-image) source-pixmap-width))
		       (+ (display-clip-y display-image)
			  (pixel-round (- (display-clip-height display-image) source-pixmap-height) 2))))
		    
		    (:south-west
		     (values
		       (display-clip-x display-image)
		       (+ (display-clip-y display-image)
			  (- (display-clip-height display-image) source-pixmap-height))))
		    
		    (:south
		     (values
		       (+ (display-clip-x display-image)
			  (pixel-round (- (display-clip-width display-image) source-pixmap-width) 2))
		       (+ (display-clip-y display-image)
			  (- (display-clip-height display-image) source-pixmap-height))))
		    
		    (:south-east
		     (values
		       (+ (display-clip-x display-image)
			  (- (display-clip-width display-image) source-pixmap-width))
		       (+ (display-clip-y display-image)
			  (- (display-clip-height display-image) source-pixmap-height)))))
		(multiple-value-setq (exposed-x exposed-y exposed-width exposed-height)
		  (area-overlaps-p
		    exposed-x exposed-y exposed-width       exposed-height
		    extent-x  extent-y  source-pixmap-width source-pixmap-height))
		(when exposed-x
		  (copy-area
		    source-pixmap gc
		    (- exposed-x extent-x) (- exposed-y extent-y) exposed-width exposed-height
		    display-image exposed-x exposed-y)))))))))


(defmethod resize :around ((display-image display-image) new-width new-height new-border-width)
  (with-slots (width height  border-width gravity) display-image
    (let* ((delta-width  (- new-width width))
	   (delta-height (- new-height height))
	  
	   ;; Establish new size.
	   (resized-p    (call-next-method)))
      
      (unless
	(or (not resized-p)
	    
	    ;; If bit-gravity is :forget, then usual exposure handling is sufficient.
	    (case gravity
	      ((:north :south)
	       (/= (display-left-margin display-image) (display-right-margin display-image)))
	      
	      ((:west :east)
	       (/= (display-top-margin display-image) (display-bottom-margin display-image)))
	      
	      (:center
	       (or (/= (display-left-margin display-image) (display-right-margin display-image))
		   (/= (display-top-margin display-image) (display-bottom-margin display-image))))))
	
	;; Otherwise, must redisplay part of image previously obscured by margins.
	(cond
	  ((plusp delta-width)
	   ;; Redisplay exposed part of left margin.
	   (multiple-value-bind (left-x left-y left-width left-height)
	       (case gravity
		 ((:north :center :south)
		  (values
		    (display-clip-x display-image) (display-clip-y display-image)
		    (pixel-round delta-width 2) (display-clip-height display-image)))
		 
		 ((:north-east :east :south-east)
		  (values
		    (display-clip-x display-image) (display-clip-y display-image)
		    delta-width (display-clip-height display-image))))
	     (when left-x
	       (display display-image left-x left-y left-width left-height)))
	   
	   ;; Redisplay exposed part of right margin.
	   (multiple-value-bind (right-x right-y right-width right-height)
	       (case gravity
		 ((:north :center :south)
		  (let ((delta (pixel-round delta-width 2)))
		    (values
		      (- width (display-right-margin display-image) delta) (display-clip-y display-image)
		      delta (display-clip-height display-image))))
		 
		 ((:north-west :west :south-west :tiled)
		  (values
		    (- width (display-right-margin display-image) delta-width) (display-clip-y display-image)
		    delta-width (display-clip-height display-image))))
	     (when right-x
	       (display display-image right-x right-y right-width right-height))))
	  
	  (:else
	   ;; Clear out left margin for smaller window.
	   (unless (case gravity ((:north-west :west :south-west) t))
	     (clear-area display-image
			 :x 0 :y 0
			 :width (display-left-margin display-image) :height height))
	   
	   ;; Clear out right margin for smaller window.
	   (unless (case gravity ((:north-east :east :south-east) t))
	     (clear-area display-image
			 :x (- width (display-right-margin display-image)) :y 0
			 :width (display-right-margin display-image) :height height))))
	
	(cond
	  ((plusp delta-height)
	   ;; Redisplay exposed part of top margin.
	   (multiple-value-bind (top-x top-y top-width top-height)
	       (case gravity
		 ((:west :center :east)
		  (values
		    (display-clip-x display-image) (display-clip-y display-image)
		    (display-clip-width display-image) (pixel-round delta-height 2)))
		 
		 ((:south-west :south :south-east)
		  (values
		    (display-clip-x display-image) (display-clip-y display-image)
		    (display-clip-width display-image) delta-height)))
	     (when top-x
	       (display display-image top-x top-y top-width top-height)))
	   
	   ;; Redisplay exposed part of bottom margin.
	   (multiple-value-bind (bottom-x bottom-y bottom-width bottom-height)
	       (case gravity
		 ((:west :center :east)
		  (let ((delta (pixel-round delta-height 2)))
		    (values
		      (display-clip-x display-image) (- height (display-bottom-margin display-image) delta)
		      (display-clip-width display-image) delta)))
		 
		 ((:north-west :north :north-east :tiled)
		  (values
		    (display-clip-x display-image) (- height (display-bottom-margin display-image) delta-height)
		    (display-clip-width display-image) delta-height)))
	     (when bottom-x
	       (display display-image bottom-x bottom-y bottom-width bottom-height))))
	  
	  (:else
	   ;; Clear out top margin for smaller window.
	   (unless (case gravity ((:north-west :north :north-east) t))
	     (clear-area display-image
			 :x 0 :y 0
			 :width width :height (display-top-margin display-image)))
	   
	   ;; Clear out bottom margin for smaller window.
	   (unless (case gravity ((:south-west :south :south-east) t))
	     (clear-area display-image
			 :x 0 :y (- height (display-bottom-margin display-image))
			 :width width :height (display-bottom-margin display-image))))))

      resized-p)))
