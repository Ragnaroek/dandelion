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

(export '(
	  make-menu
	  menu
	  menu-choice
	  menu-title
	  )
	'clio-open)


;================================================================;
;		           THE PUSHPIN CONTACT  		 ;
;================================================================;





(defcontact pushpin-button (button)
  ((pointer-pressed   :type      boolean
		      :initform  nil))
  (:resources
    (border-width :initform 0)
    
    (switch       :type (member :in :out)
		  :initform :out)))

(defun make-pushpin-button (&rest initargs)
  (apply #'make-contact 'pushpin-button initargs))


(let
  ((last-scale '())      ;These state variables are used to cache the menu spec for the
   (last-spec '()))      ;most recent scale requested.
  (defun get-OL-menu-spec (self)
    (declare (type (or NULL contact) self))
    (if (null self)
	(setf last-scale NIL)     ;This is just in case anyone ever needs to reset state
	(let((this-scale (contact-scale self)))
	  (if (eq this-scale last-scale)
	      last-spec
	      (let
		((spec (cdr (assoc this-scale *OL-menu-spec-alist*))))
		(setf last-scale this-scale)
		(setf last-spec spec)))))))

(defun get-pushpin-spec (self)
  (declare (type pushpin-button self))
  (OL-menu-spec-pushpin (get-OL-menu-spec self)))
  

(defmethod initialize-instance :after ((self pushpin-button)
				       &key switch &allow-other-keys)

  (with-slots (border-width selected) self
    (setf border-width 0)

    (when (eq switch :in)
      (setf selected 2))))





(DEFMETHOD preferred-size ((self pushpin-button) &key width height border-width)
  (declare (ignore width height border-width))
  
  (let*
    ((menu-spec (get-OL-menu-spec self))
     (pushpin-spec (OL-menu-spec-pushpin menu-spec)))
    
    (with-slots (preferred-width) self
      (VALUES				      
	(OR preferred-width
	    (SETF preferred-width
		  (+ (pushpin-spec-box-width pushpin-spec)
		     (- (OL-menu-spec-pushpin-dx menu-spec)	;Spec left margin
			(pushpin-spec-left-margin pushpin-spec)))))	;Account for (possible) padding in image
	(+ (OL-menu-spec-pushpin-dy menu-spec)	;menu top to pushpin baseline distance
	   (OL-menu-spec-title-bar-dy menu-spec))	;pushpin (title) baseline to title bar
	0))))



;;; =================================================================================== ;;;
;;;											;;;
;;;    	                      Display a Pushpin Button...				;;;
;;;											;;;
;;; =================================================================================== ;;;

(flet
  ((display-pushpin-button (self menu-spec spec &optional completely-p)
      (with-slots (font background foreground width height label) self
	(WHEN (realized-p self)			
	  (using-gcontext (gc
			    :drawable 	self
			    :exposures  :off
;;			    :foreground foreground
;;			    :background background
			    :font 	font
			    :line-width	1)
	    
	    (WHEN completely-p
	      (clear-area self
			  :x 0
			  :y 0
			  :width  width
			  :height height))
	      

	    (let*
	      ((lab-width (drawable-width label))
	       (lab-height (drawable-height label))
	       (x (- (OL-menu-spec-pushpin-dx menu-spec)  ;desired dx to left of pin
		     (pushpin-spec-left-margin spec)))    ;left margin padding in pixmap
	       (y (- (OL-menu-spec-pushpin-dy menu-spec)  ;desired baseline relative menu
		     (pushpin-spec-baseline spec)         ;pin baseline relative box
		     (pushpin-spec-top-margin spec))))    ;top margin padding in pixmap
	      (copy-area label gc 0 0 lab-width lab-height self x y)
	      ))))))


  (DEFMETHOD display-button-highlighted ((self pushpin-button) &optional completely-p)
    (with-slots (last-displayed-as label) self
      (let*
	((menu-spec (get-OL-menu-spec self))
	 (spec (OL-menu-spec-pushpin menu-spec))
	 (screen (contact-screen self)))
	(setf label
	      (contact-image-mask
		self
		(pushpin-spec-image-in spec)
		:foreground (screen-black-pixel screen)
		:background (screen-white-pixel screen)))
	  (display-pushpin-button self menu-spec spec completely-p))
      (SETF last-displayed-as :highlighted)))


  (DEFMETHOD display-button-unhighlighted ((self pushpin-button) &optional completely-p)
    (with-slots (last-displayed-as highlight-default-p label) self
      (let*
	((menu-spec (get-OL-menu-spec self))
	 (spec (OL-menu-spec-pushpin menu-spec))
	 (screen (contact-screen self)))
	(setf label
	      (contact-image-mask
		self
		(pushpin-spec-image-out spec)
		:foreground (screen-black-pixel screen)
		:background (screen-white-pixel screen)))
	  (display-pushpin-button self menu-spec spec completely-p))
      (SETF last-displayed-as :unhighlighted))) )

;;; NOTE: The following choice item methods for pushpins should invoke the choice 
;;; item callbacks, but I haven't done this yet.

(defmethod choice-item-press ((pushpin-button pushpin-button))
  (with-slots (selected) pushpin-button
    (LET((to-selected-p (= selected 1)))
      (SETF selected (- selected))
      (IF to-selected-p
	  (display-button-highlighted pushpin-button)
	  (display-button-unhighlighted pushpin-button))
      T)))

(defmethod choice-item-release ((pushpin-button pushpin-button))
  (with-slots (selected) pushpin-button
    (apply-callback pushpin-button (IF (= 2 (SETF selected (+ 3 selected))) :on :off))))

(defmethod choice-item-leave ((pushpin-button pushpin-button))
  (display-button-unhighlighted pushpin-button))

(defun menu-leave-pushpin-button (self)
   (declare (type pushpin-button self))
   (with-slots (pointer-pressed) self
     (when pointer-pressed
       (choice-item-leave self)
       (setq pointer-pressed nil))))

(defun menu-release-pushpin-button (self)
   (declare (type pushpin-button self))

   ;; Eventually this is where the logic associated with pinning a menu will exist.
;     (format t "~%Sorry - Pinning of menus not yet implemented.")
;     ;; For now we treat a release just like a leave 'cause pinning
;     ;; isn't implemented yet.
;     (menu-leave-pushpin-button self)

   ;;  +++ For now we hack pushpins to just change the pushpin state, and let
   ;;      menu-dismissal fake pinning by not dismissing if the pushpin is in.
   ;;      So, do press-and-release and delete the events.
   (with-slots (pointer-pressed) self
     (when pointer-pressed
       (choice-item-release self)
       (setq pointer-pressed nil))))

(DEFMETHOD press-select ((self pushpin-button))
   (WHEN (choice-item-press self)
     (with-slots (pointer-pressed) self
       (setq pointer-pressed t))))

(DEFMETHOD release-select ((self pushpin-button))
   (with-event (state)
     (with-slots (selected pointer-pressed) self
       (WHEN (AND (> 0 selected)
		  (NOT (ZEROP (LOGAND #.(make-state-mask :button-1) state))))
	 (UNWIND-PROTECT 
	     (choice-item-release self)
	   (setq pointer-pressed nil))))))

(DEFMETHOD menu-enter-pushpin-button ((self pushpin-button))
   (with-event (state)
     (unless (zerop (logand #.(make-state-mask :button-3) state))
       (when (choice-item-press self)
	 ;(display-button-highlighted self)
	 (with-slots (pointer-pressed) self
	   (setq pointer-pressed t))))))

(DEFMETHOD (SETF choice-item-selected-p) (new-value (self pushpin-button))
   ;; Identical to (SETF button-switch) except returns boolean in/out indicator.
   (EQ (SETF (button-switch self) (if new-value :in :out)) :in))

;;; ========================================================================== ;;;
;;;									       ;;;
;;;        ( P u s h p i n )   B u t t o n   P r o t o c o l   M e t h o d s   ;;;
;;;									       ;;;
;;; ========================================================================== ;;;

(DEFMETHOD button-switch ((self pushpin-button))
   (with-slots (selected) self
     (NTH (1- (ABS selected)) '(:out :in))))

(DEFMETHOD (SETF button-switch) (new-state (self pushpin-button))
   (check-type new-state (member :in :out))
   (LET ((current-state (button-switch self)))
     (WHEN (NOT (EQ current-state new-state))
       ;; We simulate a button press and release to implement identical
       ;; semantics whether done via API or via gesture.
       (WHEN (choice-item-press self)
	 ;; When toggle press succeeded we follow it
	 ;; with a release.
	 (choice-item-release self)))
     (button-switch self)))


(DEFEVENT pushpin-button
	  :enter-notify
  	  menu-enter-pushpin-button)

(defevent pushpin-button
	  :leave-notify
   menu-leave-pushpin-button)

(defevent pushpin-button
	  (:button-release :button-1)
   pp-maybe-release-select)

(defun pp-maybe-release-select (button)
   (with-slots (pointer-pressed) (the pushpin-button button)
     (when pointer-pressed
       (release-select button))))

;;  These two translations are for Open Look menus, which allow item selection
;;  on both button-1 and button-3 presses.
(DEFEVENT pushpin-button
	  (:button-press :button-3)
   press-select)

(DEFEVENT pushpin-button
	  (:button-release :button-3)
   menu-release-pushpin-button)


;================================================================;
;			   MENU  CONTACT			 ;
;================================================================;
(defcontact menu (core-shell core override-shell)
  ()
  (:resources
   (title 	  :type     (or null string)
	  	  :initform nil)
   (pushpin      :type     switch
		  :initform :off)
   (save-under   :initform :on)
   (border-width :initform 0))
  (:documentation "A shell which presents a set of choice items."))

(defun make-menu (&rest initargs)
  "Creates and returns a menu instance."
  (apply #'make-contact 'menu initargs))

(defmethod initialize-instance :after ((menu menu) &rest args
				       &key (choice 'make-choices) &allow-other-keys)
  (with-slots (background border-width width height) menu    

;;  Can't do this now that the choice arg is a constructor rather than a type.
;    (let ((choice-class (if (consp choice) (first choice) choice))) 
;      (assert (subtypep choice-class 'composite) nil
;	      "~s is not a composite subclass name." choice-class)) 
    
    (apply #'make-contact
	   'drop-shadow
	   :parent menu
	   :x 0
	   :y 0
	   :width width
	   :height height
	   :content choice
	   args)

    ;; Now that content is created with initial attributes,
    ;; reset background, border-width to accommodate drop-shadow.
    (setf background   :none
	  border-width 0)))

;;  When asked for background, give the background of the container.
(defmethod contact-background ((menu menu))
   (with-slots (children) menu
     (let ((container (and children (menu-container menu))))
       (if container
	   (contact-background container)
	   :none))))

(defmethod (setf contact-background) (new-value (menu menu))
  (setf (contact-background (menu-container menu)) new-value))

(defmethod (setf contact-foreground) (new-value (menu menu))
  (setf (contact-foreground (menu-container menu)) new-value))

(defmethod menu-choice ((menu menu))
  (find :content (composite-children (menu-container menu)) :key #'contact-name))

(defun menu-container (menu)
  (first (composite-children (first (composite-children menu)))))

(defmethod (setf menu-title) (new-title (menu menu))
  (let
    ((title-field (find :menu-title
			(composite-children (menu-container menu))
			:key 'contact-name))
     (title       (convert menu new-title 'string)))
    
    (assert title nil "~a cannot be converted to a title string." new-title)

    (cond
      (title-field
       (change-geometry
	 title-field
	 :width    (text-width (display-text-font title-field) title)
	 :accept-p t)       
       (setf (display-text-source title-field) title))

      (t
       (make-display-text-field
	  :name            :menu-title
	  :parent          (menu-container menu)
	  :display-gravity :center
	  :source          title)))
    title))

(defmethod menu-title ((menu menu))
  (let ((title-field (find :menu-title
			   (composite-children (menu-container menu))
			   :key 'contact-name)))
    (when title-field
      (display-text-source title-field))))


(defmethod preferred-size ((self menu) &key width height border-width)
  (declare (ignore width height border-width))

  (preferred-size (first (composite-children self))))


(defmethod shell-mapped ((self menu))
  "Invokes :initialize callback function."
  (apply-callback self :map)
  (apply-callback-else (self :initialize)
    (with-slots ((members children)) (menu-choice self)
      (dolist (member members)
	(apply-callback member :initialize)))))


;================================================================;
;			DROP SHADOW CONTACT			 ;
;================================================================;

(defcontact drop-shadow (core composite)
  ((compress-exposures :initform :on))
  (:documentation "A composite containing a content and a drop-shadow.")
  (:resources (event-mask :initform #.(make-event-mask :exposure))))

(defmethod initialize-instance :after ((drop-shadow drop-shadow) &rest args)
  (with-slots (background border-width) drop-shadow
    ;; Ignore background, border-width
    (setf background :none)
    (setf border-width 0))

  ;; Make the menu container to hold the content, title, & pushpin components.
  (apply #'make-contact 'menu-container
    :name :menu-container
    :parent drop-shadow
    args))

(defmethod preferred-size ((self drop-shadow) &key width height border-width)
  (declare (ignore width height border-width))
  (let*
    ((spec (get-OL-menu-spec self))
     (dsw (OL-menu-spec-drop-shadow-width spec)))
    (multiple-value-bind (pw ph pbw)
	(preferred-size (first (composite-children self)))
      (values
	(+ dsw pbw pbw pw)
	(+ dsw pbw pbw ph)
	0))))

(defmethod display ((drop-shadow drop-shadow) &optional x y width height &key)
  (declare (ignore x y width height))
  (with-slots (children) drop-shadow
    (when children
      (with-slots (width height border-width) (first children)
	(let*
	  ((spec (get-OL-menu-spec drop-shadow))
	   (dsw (OL-menu-spec-drop-shadow-width spec))
	   (dso (OL-menu-spec-drop-shadow-offset spec))
	   (menu-container-width (+ width border-width border-width))
	   (menu-container-height (+ height border-width border-width)))
	  (using-gcontext (gc :drawable drop-shadow
			      :foreground (contact-foreground drop-shadow)
			      :fill-style :stippled
			      :stipple (contact-image-mask drop-shadow 50%gray :depth 1))
            ;; We draw a full rectangle, depending on the server to clip the
	    ;; portion covered by the menu container.
	    (draw-rectangle drop-shadow gc
	                    dso dso
			    (- (+ menu-container-width dsw) dso)
			    (- (+ menu-container-height dsw) dso)
			    :fill-p)))))))

(defmethod manage-geometry ((self drop-shadow) child x y width height border-width &key)
  (let*
    ((spec (get-OL-menu-spec self))
     (dsw (OL-menu-spec-drop-shadow-width spec))
     (child-bw (or border-width (contact-border-width child)))
     (width (or width (contact-width child)))
     (height (or height (contact-height child)))
     (drop-shadow-contact-width (+ width child-bw child-bw dsw))
     (drop-shadow-contact-height (+ height child-bw child-bw dsw))

     (self-change-not-required-p
       (and (= (contact-width self) drop-shadow-contact-width)
	    (= (contact-height self) drop-shadow-contact-height)))
     (approved-p
       (and
	 (or (null x) (= x 0))
	 (or (null y) (= y 0))
	 (or self-change-not-required-p
	     (change-geometry self
			      :width drop-shadow-contact-width
			      :height drop-shadow-contact-height
			      :accept-p t)))))

    (values
      approved-p
      0 0
      (- (contact-width self) child-bw child-bw dsw)
      (- (contact-height self) child-bw child-bw dsw)
      child-bw)))

(defmethod change-layout ((self drop-shadow) &optional newly-managed)
  (declare (ignore newly-managed))
  (let((children (composite-children self)))
    (when children
      (let*
	((spec (get-OL-menu-spec self))
	 (dsw (OL-menu-spec-drop-shadow-width spec))
	 (menu-container (first children))
	 (border-width (contact-border-width menu-container))
	 (width (contact-width menu-container))
	 (height (contact-height menu-container)))
	(change-geometry
	  self
	  :width (+ width border-width border-width dsw)
	  :height (+ height border-width border-width dsw)
	  :accept-p t)))))

(defmethod add-child :before ((self drop-shadow) child &key)
  (let((children (composite-children self)))
    (when children
      (error "~s already has child ~s; cannot add child ~s."
	     self
	     (first children)
	     child))))

(defmethod resize :after ((self drop-shadow) width height border-width)
  (declare (ignore border-width))
  (let*
    ((children (composite-children self))
     (menu-container (first children))
     (spec (get-OL-menu-spec self))
     (dsw (OL-menu-spec-drop-shadow-width spec))
     (bw (contact-border-width menu-container)))
    (resize menu-container
	    (max 1 (- width bw bw dsw))
	    (max 1 (- height bw bw dsw))
	    bw)))
    

;================================================================;
;			MENU-CONTAINER CONTACT			 ;
;================================================================;

(defcontact menu-container (core composite)
  ((compress-exposures :initform :on))
  (:resources
    (event-mask :initform #.(make-event-mask :exposure)))
  (:documentation 
    "A composite containing a content and (optionally) a title and pushpin"))

(defmethod initialize-instance :after ((self menu-container)
				       &rest args
				       &key content (pushpin :off) title
				       &allow-other-keys) 

  (let ((menu      (contact-parent (contact-parent self)))
	(menu-spec (get-OL-menu-spec self)))

    ;; Initialize container window attributes.
    (with-slots (background border-width) self
      ;; Inherit background from menu, not drop-shadow.
      (when (eq :parent-relative background)
	(setf background (contact-current-background menu)))

      ;; Inherit border-width from menu
      (setf border-width (max (contact-border-width menu)
			      (point-pixels (contact-screen self)))))
      
      
      ;; Initialize content
      (multiple-value-bind (content-constructor content-args)
	  (if (consp content) (values (first content) (rest content)) content)
	
	(add-callback
	  (if (null content-args)
	      
	      ;; Default choice initialization 
	      (funcall content-constructor
		:name          :content
		:parent        self
		:border-width  0 
		:left-margin   (OL-menu-spec-pushpin-dx menu-spec)
		:right-margin  (OL-menu-spec-pushpin-dx menu-spec)
		:bottom-margin (OL-menu-spec-drop-shadow-offset menu-spec) 
		:top-margin    (OL-menu-spec-drop-shadow-offset menu-spec)
		:columns       1
		:same-width-in-column :on)
	      
	      ;; Else use given content initargs
	      (apply content-constructor
		     :name          :content
		     :parent        self
		     :border-width  0 
		     content-args))
	  
	  :new-choice-item
	  #'add-menu-item-callbacks menu))

      ;; Initialize title field
      (when title
	(setf (menu-title menu) title))

      ;; Initialize pushpin
      (when (eq pushpin :on)
	(add-callback
	  (make-pushpin-button
	    :name :pushpin
	    :parent self
	    :label (pushpin-spec-image-out (get-pushpin-spec self)))
	  :off
	  #'dismiss-menu menu))))

;;  A method so dialog-button can add daemons.
(defmethod add-menu-item-callbacks (item menu)
   (when (typep item 'toggle-button)
     (add-callback item :on #'dismiss-menu menu))
   (add-callback item :off #'dismiss-menu menu))

(defun dismiss-menu (menu)
   (unless (eq (contact-state menu) :withdrawn)	;i.e., already dismissed
     ;;  +++ Pushpin hack:  If the menu has a pushpin and it's :in, don't
     ;;      withdraw the menu.  It doesn't clone, but it does stay up.
     (let ((pushpin (find :pushpin (composite-children
				     (contact-parent (menu-choice menu)))
			  :key #'contact-name)))
       (unless (and pushpin
		    (eq :in (button-switch pushpin)))
	 (setf (contact-state menu) :withdrawn)))))
     

(defmethod display ((self menu-container)
		    &optional exposed-x exposed-y exposed-width exposed-height &key)
  (declare (ignore exposed-x exposed-y exposed-width exposed-height))
  (with-slots (children width foreground) self
    (when (find :menu-title children :key #'contact-name)
      (let ((tbar-x 4) ; Kludge! this is actually a function of scale.
	    (tbar-y (- (contact-y (find :content children :key #'contact-name)) 1)))
	(using-gcontext (gc :drawable self :foreground foreground)
	  (draw-line self gc tbar-x tbar-y (- width tbar-x) tbar-y))))))	


;================================================================;
;		  MENU-CONTAINER GEOMETRY MANAGEMENT		 ;
;================================================================;
(labels
  (
   (disapprove () NIL)
   
   (fail () (error "Unable to layout menu-container." ))

   (shrink/expand-title (title pw ph tw th cw ch failure-thunk)
     (multiple-value-bind (tw1 th1)
	 (preferred-size title :width 0 :height 0)
       (if (<= tw1 tw)
	   ;; We assume it's OK to make title *wider* than preferred width,
	   ;; but avoid making it narrower.
	   (values
	     (+ 2 (max (or (and pw tw (+ pw
					 (ol-menu-spec-title-dx (get-ol-menu-spec title))
					 (ol-menu-spec-title-dx (get-ol-menu-spec title))
					 tw))
			   (and tw
				(+ tw
				   (ol-menu-spec-title-dx (get-ol-menu-spec title))
				   (ol-menu-spec-title-dx (get-ol-menu-spec title))))
			   0)
		       (+ cw (ol-menu-spec-title-dx (get-ol-menu-spec title))
			  (ol-menu-spec-title-dx (get-ol-menu-spec title)))))	;Allow 2 pixels for left & right border
	     (+ 2				                                ;Allow 2 pixels for top & bottom border
		(or (and ph (max ph th1)) th1)
		1				;Allow 1 pixel for title bar
		ch)
	     pw
	     ph
	     tw
	     th
	     cw
	     ch)
	   (funcall failure-thunk))))

   (shrink/expand-content (content pw ph tw th cw ch failure-thunk)
     (multiple-value-bind (cw1 ch1)
	 (preferred-size content :width 0 :height 0) 
       (if (<= cw1 cw)
	   ;; We assume it's OK to make content *wider* than preferred width,
	   ;; but avoid making it narrower.
	   (values
	     (+ 2 (max (or (and pw tw (+ pw
					 (ol-menu-spec-title-dx (get-ol-menu-spec content))
					 (ol-menu-spec-title-dx (get-ol-menu-spec content))
					 tw))
			   (and tw
				(+ tw
				   (ol-menu-spec-title-dx (get-ol-menu-spec content))
				   (ol-menu-spec-title-dx (get-ol-menu-spec content))))
			   0)
		       (+ cw (ol-menu-spec-title-dx (get-ol-menu-spec content))
			     (ol-menu-spec-title-dx (get-ol-menu-spec content)))
		       ))			;Allow 2 pixels for left & right border ;; jba 
	     (+ 2				;Allow 2 pixels for top & bottom border
		(or (and ph th (max ph th)) ph th 0)
		(if th 1 0)			;Title bar only if title
		ch1)
	     pw
	     ph
	     tw
	     th
	     cw1
	     ch)
	   (funcall failure-thunk))))

   (reposition&resize (component cx cy cw ch)
      (let
	((x (contact-x component))
	 (y (contact-y component))
	 (w (contact-width component))
	 (h (contact-height component)))
	(unless (and (= x cx) (= y cy)) (move component cx cy))
	(unless (and (= w cw) (= h ch)) (resize component cw ch 0))))
	
   (execute-layout (self width height ppin pw ph title tw th content cw ch) 
     (assert
       (change-geometry self :width width :height height)
       ()
       "Unable to layout menu ~a" self)
	
     (multiple-value-bind (px py tx ty cx cy)
	 (locate-menu-components pw ph tw th width self)
       (when ppin (reposition&resize ppin px py pw ph))
       (when title 
	 (reposition&resize title tx ty tw th))
       (when content (reposition&resize content cx cy cw ch))) )

   (locate-menu-components (pw ph tw th width self) 
     (cond
       ((and pw tw)
	(values
	  0 0     ; x & y for pushpin
	  (+ pw (ol-menu-spec-title-dx (get-ol-menu-spec self)) ) 0    ; x & y for title
	  (max (ol-menu-spec-title-dx (get-ol-menu-spec self))
	       (pixel-round (- width
			 (contact-width (find :content (composite-children self) :key #'contact-name)))
		   2))
	  (+ (max ph th) 1)))  ;x & y for content
       (pw
	(values
	  0 0
	  NIL NIL
	  (max (ol-menu-spec-title-dx (get-ol-menu-spec self))
	       (pixel-round (- width
			 (contact-width (find :content (composite-children self) :key #'contact-name)))
		   2))
	  ph))
       (tw
	(values
	  NIL NIL 
	  (max (ol-menu-spec-title-dx (get-ol-menu-spec self))
	       (pixel-round (- width
			 (contact-width (find :menu-title (composite-children self) :key #'contact-name)))
		      2))
	  0
	  (max (ol-menu-spec-title-dx (get-ol-menu-spec self))
	       (pixel-round (- width
			 (contact-width (find :content (composite-children self) :key #'contact-name)))
		      2))
	  (+ 1 th)))
       (t
	(values
	  NIL NIL
	  NIL NIL
	  0 0))))
      
   (layout-menu-container (ppin pw ph title tw th content cw ch) 
     (cond
       ((and title ppin)
	; Menu has title and pushpin in addition to content
	(let ((title-area-height (max ph th)))
	  (if (= (+ tw pw) cw)
	      (values
		(+ 2 cw (ol-menu-spec-title-dx (get-ol-menu-spec content))
		   (ol-menu-spec-title-dx (get-ol-menu-spec content)))
		(+ 2 title-area-height 1 ch)
		pw title-area-height
		tw title-area-height
		cw ch)
	      ;; We must expand/shrink title or shrink/expand content.
	      (let((newtw (- cw pw)))
		(if (< newtw 0)
		    (shrink/expand-content content pw ph tw th (+ tw pw) ch #'fail)
		    (shrink/expand-title
		      title
		      pw
		      title-area-height
		      newtw
		      title-area-height
		      cw
		      ch
		      #'(lambda()
			  (shrink/expand-content
			    content
			    pw
			    title-area-height
			    tw
			    title-area-height
			    (+ tw pw)		; Will fail when title is bigger, so width is title width.
			    ch
			    #'fail))))))))
       
       (title
	;Menu has title, but no pushpin
	(if (= tw cw)
	    (values
	      (+ 2 cw (ol-menu-spec-title-dx (get-ol-menu-spec content))
		      (ol-menu-spec-title-dx (get-ol-menu-spec content)))
	      (+ 2 th 1 ch)
	      pw ph
	      tw th
	      cw ch)
	    (shrink/expand-title
	      title
	      pw
	      ph
	      tw
	      th
	      cw
	      ch
	      #'(lambda()
		  (shrink/expand-content content pw ph tw th tw ch #'fail)))
	    ))
       
       (ppin
	;Menu has pushpin, but no title
	(if (> cw pw)
	    (values (+ 2 cw (ol-menu-spec-title-dx (get-ol-menu-spec content))
		       (ol-menu-spec-title-dx (get-ol-menu-spec content)))
		    (+ 2 ph ch) pw ph tw th cw ch)
	    (shrink/expand-content content pw ph tw th pw ch #'fail)))
       
       (t
	;Menu has neither pushpin nor title
	(values (+ 2 cw) (+ 2 ch) pw ph tw th cw ch))))   )


  (defmethod change-layout ((self menu-container) &optional newly-managed)
    (declare (ignore newly-managed))
    (let*
      ((children (composite-children self))
       
       (content (find :content children :key #'contact-name))
       (cw (contact-width content))
       (ch (contact-height content))
       
       (title (find :menu-title children :key #'contact-name))
       (tw (and title (contact-width title)))
       (th (and title (contact-height title)))
       
       (ppin (find :pushpin children :key #'contact-name))
       (pw (and ppin (contact-width ppin)))
       (ph (and ppin (contact-height ppin))))
       (multiple-value-bind (width height pw1 ph1 tw1 th1 cw1 ch1)
	   (layout-menu-container ppin pw ph title tw th content cw ch)
	 (execute-layout self width height ppin pw1 ph1 title tw1 th1 content cw1 ch1))))

  (defmethod preferred-size ((self menu-container) &key width height border-width)
    (declare (ignore width height border-width))
    (let*
      ((children (composite-children self))
       (content (find :content children :key #'contact-name))
       (title (find :menu-title children :key #'contact-name))
       (ppin (find :pushpin children :key #'contact-name)))

      (MULTIPLE-VALUE-BIND (cw ch)
	  (preferred-size content :width 0 :height 0)
	(MULTIPLE-VALUE-BIND (tw th)
	    (AND title (preferred-size title :width 0 :height 0))
	  (MULTIPLE-VALUE-BIND (pw ph)
	      (AND ppin (preferred-size ppin))
	    (MULTIPLE-VALUE-BIND (preferred-width preferred-height)
		(layout-menu-container ppin pw ph title tw th content cw ch)
	      (VALUES preferred-width preferred-height 0)))))))

  (defmethod manage-geometry ((self menu-container) child x y width height bw &key)
    (let*
      ((children (composite-children self))
       
       (content (find :content children :key #'contact-name))
       (cw (contact-width content))
       (ch (contact-height content))
       
       (title (find :menu-title children :key #'contact-name))
       (tw (and title (contact-width title)))
       (th (and title (contact-height title)))
       
       (ppin (find :pushpin children :key #'contact-name))
       (pw (and ppin (contact-width ppin)))
       (ph (and ppin (contact-height ppin)))

       (x (or x (contact-x child)))
       (y (or y (contact-y child)))
       (width (or width (contact-width child)))
       (height (or height (contact-height child))))


      (multiple-value-bind (self-width self-height pw1 ph1 tw1 th1 cw1 ch1)
	  (cond
	    ((eq child content)
	     (cond
	       (title
		;; If there is a title then try to adjust it
		(shrink/expand-title
		  title
		  pw
		  ph
		  (if pw (- width pw) width)
		  th
		  width
		  height
		  ;; If title adjust fails then try to adjust content
		  ;; so can at least offer compromise.
		  #'(lambda()
		      (shrink/expand-content
			content
			pw
			ph
			tw
			th
			(or (and pw (+ pw tw)) tw)
			height
			#'disapprove))))
	       (ppin
		;; If ppin exists must make sure content is at least as wide
		(if (> width pw)
		    (values (+ 2 width) (+ 2 height ph 1) pw ph tw th width height)
		    (shrink/expand-content
		      content
		      pw
		      ph
		      tw
		      th
		      pw
		      height
		      #'disapprove)))
	       (t
		;; Menu has neither pushpin nor title
		(values
		  (+ 2 width) (+ 2 height) pw ph tw th width height))))
	    ((eq child title)
	     (shrink/expand-content
	       content
	       pw
	       ph
	       width
	       height
	       (if ppin (+ pw tw) tw)
	       ch
	       #'(lambda()
		   (shrink/expand-title
		     title
		     pw
		     ph
		     (if ppin (- cw pw) width)
		     height
		     cw
		     ch
		     #'disapprove))))
	    ;; It must be the pushpin which has changed
	    (title    
	     (shrink/expand-title
	       title
	       width
	       height
	       (- cw width)
	       th
	       cw
	       ch
	       #'(lambda()
		   (shrink/expand-content
		     content
		     width
		     height
		     tw
		     th
		     (+ width tw)
		     ch
		     #'disapprove))))
	    ;; Pushpin is being managed, but no title to adjust, so must adjust content.
	    ((< cw width)
	     ;; If the content width is less than the requested pushpin width we try to
	     ;; shrink the content accordingly. (Pretty unlikely case, eh?)
	     (shrink/expand-content
	       content
	       width
	       height
	       tw
	       th
	       width
	       ch
	       #'disapprove))
	    (t
	     ;; Else the content is at least as wide as the pushpin and we can simply
	     ;; accept the pushpin change without any ripple effects.
	     (values
	       (+ 2 cw) (+ 2 ch ph)
	       width height
	       tw th
	       cw ch)))

	(and
	  self-width       ;Width = NIL implies failure without suggesting compromise.
	  (multiple-value-bind (px1 py1 tx1 ty1 cx1 cy1)
	      (locate-menu-components pw1 ph1 tw1 th1 self-width self)
	    (let
	      ((self-change-approved
		 (or
		   (and
		     (= self-width (contact-width self))
		     (= self-height (contact-height self)))
		   (change-geometry self
				  :width self-width
				  :height self-height
				  :accept-p nil)))
	       (approve-p
		 (and
		   (or (null bw) (= 0 bw))
		   (cond
		     ((eq child ppin) (and (= pw1 width) (= ph1 height)
					   (= px1 x)     (= py1 y)))
		     ((eq child title) (and (= tw1 width) (= th1 height)
					    (= tx1 x)     (= ty1 y)
					    )
				       )
		     (t (and (= cw1 width) (= ch1 height)
			     (= cx1 x)     (= cy1 y)))))))
	      (and
		self-change-approved
		(progn
		  (when approve-p
		    (execute-layout
		      self self-width self-height
		      ppin pw1 ph1
		      title tw1 th1
		      content cw1 ch1))
		  (cond
		    ((eq child ppin) (values approve-p px1 py1 pw1 ph1 0))
		    ((eq child title) (values approve-p tx1 ty1 tw1 th1 0))
		    (t (values approve-p cx1 cy1 cw1 ch1 0))))))))))))
