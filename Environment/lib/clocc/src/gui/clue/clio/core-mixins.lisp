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
	  switch
	  
	  *default-contact-border*
	  *default-contact-foreground*
	  
	  core
	  core-shell
	  contact-foreground
	  contact-border
	  contact-scale
	  rescale

	  *default-display-horizontal-space*
	  *default-display-vertical-space* 
	  display-horizontal-space
	  display-vertical-space
	  )
	'clio-open)



(deftype switch ()
  '(member :on :off))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                core-shell                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact core-shell ()
  ((scale      :type	       (member :small :medium :large :extra-large)
	       :initarg	       :scale
	       :initform       :medium
	       :accessor       contact-scale
	       :documentation  "The OPEN LOOK scale for the contact."))
   (:resources
     scale)
   (:documentation "A base class for OPEN LOOK shells."))

(defmethod initialize-instance :after ((contact core-shell) &rest initargs)
  (declare (ignore initargs))
  (with-slots (background) contact
    (when (eq :parent-relative background)
      ;; Neither shell nor its owner specified a background...default to :white
      (setf background (screen-white-pixel (contact-screen contact))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                          Scale Implementation	                       |
;;;  Strategy:								       |
;;;	1. Remove scale slot from core class.				       |
;;;	2. Define new core-shell mixin to carry scale slot.		       |
;;;	3. Add core-shell as superclass for all CLIO-OPEN shell classes.       |
;;;	4. Define contact-scale reader methods for all contacts.	       |
;;;	5. Define (setf contact-scale) methods only for root and core-shell.   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+
;;;
;;;   Nearly all contacts inherit scale from their parents...
;;;
(defmethod contact-scale ((self contact))
  (with-slots (parent) self
    (contact-scale parent)))

(defparameter *default-root-scale* :medium)

;;;
;;;   A root, lacking a parent, returns a default...
;;;
(defmethod contact-scale ((self root))
  ;; Contacts descended from a non-core-shell end up here...
  *default-root-scale*)

;;;
;;;   Changing the root's scale changes it for everyone...
;;;
(defmethod (setf contact-scale) (new-value (self root))  
  (setf *default-root-scale* new-value)

  ;; Propagate scale change to descendants
  (while-changing-layout (self)
    (rescale self))

  new-value)

;;;
;;;   Trying to change a non-root non-top-level contact's scale changes is an error.
;;;
(defmethod (setf contact-scale) (new-value (self contact))
  (declare (ignore new-value))
  (error "~s inherits scale from its ~:[top-level~;root~] ancestor." self (top-level-p self)))


;;;
;;;   Once a top-level shell's scale has been changed, propagate the effect of the change to all
;;;   its descendents...
;;;
(defmethod (setf contact-scale) :after (new-value (self core-shell))
  ;; Propagate scale change to descendants
  (declare (ignore new-value))
  (while-changing-layout (self)
    (rescale self)))


(defmethod rescale ((self composite))
  (with-slots (children shells) self
    ;; Rescale children.
    (while-changing-layout (self)
      (dolist (child children)
	(rescale child))
      (dolist (shell shells)
	(setf (contact-scale shell) (contact-scale self))))))

(defmethod rescale ((self contact))
  ;; Default is to resize to preferred size for new scale.
  ;; Any changes to font, pixmaps, etc. should be done in a specialized :before
  ;; method
  (multiple-value-bind (width height bw) (preferred-size self :width 0 :height 0)
    (change-geometry self
		     :width        width
		     :height       height
		     :border-width bw
		     :accept-p     t)))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              core-wm-shell                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact core-wm-shell (core-shell)
  ((pinned-p   :type	       boolean
	       :initarg	       :pinned-p
	       :initform       nil
	       :accessor       contact-pinned-p))
   
   (:documentation "A base class for OPEN LOOK pop-up windows"))


(defevent core-wm-shell (:property-notify :_ol_pin_state) update-pin-state)

(defun update-pin-state (shell)
  (declare (type core-wm-shell shell))
  (with-slots (pinned-p) (the core-wm-shell shell)
    (let ((previous (when pinned-p t)))
      (unless
	(or (setf pinned-p (= 1 (first (get-property shell  :_ol_pin_state))))
	    (eq pinned-p previous))
	(setf (contact-state shell) :withdrawn)))))

(defmethod any-accept-focus-p ((contact contact))
  (plusp (logand (cluei::contact-event-translations-mask contact)
		 #.(make-event-mask :key-press :key-release))))

(defmethod any-accept-focus-p ((composite composite))
  (or (call-next-method) (with-slots (children) composite
			   (when (find-if #'any-accept-focus-p children) t))))

(defmethod realize :before ((self core-wm-shell))
  ;; Initialize standard properties as needed by Open Look.
  ;; Window group...
  (unless (wm-group self)
    ;; Group leader is base window owner (i.e. root shell)
    (setf (wm-group self) (contact-root-shell self)))
  
  ;; Input focus... 
  (unless (wm-keyboard-input self)
    ;; By default, don't ask for window to perform set-input-focus.
    (setf (wm-keyboard-input self) :off)
    
    ;; Use Globally Active model if keyboard input to descendants is possible.
    (when (any-accept-focus-p self)
      (setf (wm-protocols-used self)
	    (adjoin :WM_TAKE_FOCUS (wm-protocols-used self)))))
  
  ;; ICCCM protocols...
  (setf (wm-protocols-used self)
	(adjoin :WM_DELETE_WINDOW (wm-protocols-used self))))

(defmethod realize :after ((self core-wm-shell)) 
  (with-slots ((contact-display display) pinned-p) self
    (let ((display contact-display)) 

      ;; Set OLWM window type and initial push-pin.
      (intern-atom display :_OL_WIN_ATTR)
      (change-property
	self :_OL_WIN_ATTR			  
	`(,(intern-atom display :_OL_WT_CMD)
	  ,(intern-atom display :_OL_MENU_LIMITED) 
	  , (intern-atom display (if pinned-p :_OL_PIN_IN :_OL_PIN_OUT)))
	:_OL_WIN_ATTR
	32)

      ;; Set OLWM protocols.
      (intern-atom display :_OL_PROTOCOLS) 
      (change-property
	self :_OL_PROTOCOLS
	`(,(intern-atom display :_OL_SCALE))
	:ATOM
	32))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                   core                                     |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defparameter *default-contact-border*
	      :black)

(defparameter *default-contact-foreground*
	      :black)

(defcontact core ()
  ((border     :type           (or (member :copy) pixel pixmap)
	       :initform        *default-contact-border*
	       :initarg        :border
	       :reader         contact-border	        ; setf defined below
	       :documentation  "Contents of the contact border.")
   (foreground :type	       pixel
	       :initarg	       :foreground
	       :reader         contact-foreground	; setf defined below
	       :documentation  "The foreground color for the contact."))
  
  (:resources border foreground)
  (:documentation "Base class for all core contacts."))

(defmethod initialize-instance :after ((contact core) &rest initargs)
  (declare (ignore initargs))
  (with-slots (foreground) contact
    (unless foreground
      (assert
	(setf foreground (or (inherited-foreground contact)
			     (convert contact *default-contact-foreground* 'pixel)))
	nil
	"Default foreground color is ~a, which cannot be converted to a pixel."
	*default-contact-foreground*))))

(defmethod inherited-foreground ((contact contact))
  (with-slots (parent) contact
    (contact-foreground parent)))

(defmethod inherited-foreground ((contact shell))
  (contact-foreground (shell-owner contact)))
  
(defmethod (setf contact-border) (new-border (contact core))
  (with-slots (border) contact
    (let ((converted-border (convert contact new-border '(or pixel pixmap (member :copy)))))
      (assert converted-border nil "~a cannot be converted to PIXEL, PIXMAP, or :COPY." new-border)
      (unless (eql border converted-border)
	(setf border converted-border)
	(setf (window-border contact) converted-border)))
    border))

(defmethod (setf contact-foreground) (new-foreground (contact core))
  (with-slots (foreground) contact
    (let ((converted-foreground (convert contact new-foreground 'pixel)))
      (assert converted-foreground nil "~a cannot be converted to a PIXEL." new-foreground)
      (unless (eql foreground converted-foreground)
	(setf foreground converted-foreground)
	(clear-area contact :exposures-p t)))
    foreground))

(defmethod contact-foreground (object)
  (declare (ignore object))
  ;; Default method for non-core objects.
  nil)



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                Font Handling                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defparameter *open-look-scale-fontnames*
	      '(
		:small       "-*-lucida-*-r-normal-sans-10-*-*-*-p-*-*-*"
		:medium      "-*-lucida-*-r-normal-sans-12-*-*-*-p-*-*-*"
		:large       "-*-lucida-*-r-normal-sans-14-*-*-*-p-*-*-*"
		:extra-large "-*-lucida-*-r-normal-sans-19-*-*-*-p-*-*-*"
		)
  "These are the fontnames used to implement Open Look, if available.
Modify these if you want to override CLIO's use of Open Look fonts.")

(defparameter *default-scale-fontnames*
	      '(
		:small       "-*-helvetica-*-r-*-*-10-*-*-*-p-*-*-*"
		:medium      "-*-helvetica-*-r-*-*-12-*-*-*-p-*-*-*"
		:large       "-*-helvetica-*-r-*-*-14-*-*-*-p-*-*-*"
		:extra-large "-*-charter-*-r-*-*-19-*-*-*-p-*-*-*"
		)
  "If standard Open Look are not available, use these fontname
attributes for each scale.") 


(defmethod find-font ((contact core) fontname)
  (declare (type fontable fontname))
  (flet
    ((find-font-attributes (attributes fontnames)
        ;; Assert:
        ;;   ATTRIBUTES is a fully-qualified fontname.
        ;;   FONTNAMES is a list of fully-qualified fontnames.
        
        ;; Return a member of FONTNAMES that matches ATTRIBUTES for every
	;; non-* component of ATTRIBUTES.
			   
        (let ((lengtha (length attributes)))
	  (dolist (fontname fontnames)
	    (when
	      ;; Does this fontname match?
	      (do
		(
		 ;; Start/end of next component of attributes.
		 (starta 0 (min lengtha (1+ enda)))
		 enda
		 
		 ;; Start/end of next component of fontname.
		 (startf 0 (min lengthf (1+ endf)))
		 endf
		 (lengthf (length fontname)))
		
		;; If finished scanning, return match.
		((and (>= starta lengtha) (>= startf lengthf)) t)
		
		;; Find end of next component.
		(setf enda (or (position #\- attributes :start starta) lengtha))
		(setf endf (or (position #\- fontname   :start startf) lengthf))
 
		(unless
		  (or
		    ;; Is next attributes component is *?
		    (string-equal attributes "*" :start1 starta :end1 enda)
		    
		    ;; Does corresponding fontname component match?
		    (string-equal
		      fontname attributes
		      :start1 startf :end1 endf :start2 starta :end2 enda))
		  
		  ;; No, match failed...try next fontname
		  (return nil)))
	      
	      (return fontname))))))
    
    (let* ((display  (contact-display contact))
	   (scale    (contact-scale contact))
	   
	   ;; Get requested fontname string.
	   (fontname (etypecase fontname
		       (font    (font-name fontname))
		       (string  fontname)
		       (null    "*")
		       (symbol  (symbol-name fontname))))
	   
	   (cache    (getf (display-plist display) 'fontnames)))
      
      (or
	;; Already found in fontname cache?
	(third (find-if
		 #'(lambda (entry) (and (eq (first entry) scale)
					(string-equal (second entry) fontname)))
		 cache))
	
	;; No, create new fontname cache entry.
	(let*
	  ;; Get (fully-qualified) fontnames matching request. 
	  ((requested
	     (delete-if #'(lambda (name) (not (find #\- name)))
			(list-font-names display fontname)))
	   
	   ;; Find fontname that best matches given fontname+Open Look requirements.
	   (best-match
	     (open-font
	       display
	       (or
		 (let ((ol-required (getf *open-look-scale-fontnames* scale))) 
		   (or
		     ;; Find one matching Open Look requirements?
		     (find-font-attributes ol-required requested)
		     
		     ;; Find appropriate Open Look font for scale?
		     (when (list-font-names display ol-required)
		       ol-required)))
		 
		 (let ((ol-requested (getf *default-scale-fontnames* scale))) 
		   (or
		     ;; Find one matching Open Look suggestions?
		     (find-font-attributes ol-requested requested)
		     
		     ;; Find appropriate suggested font for scale?
		     (when (list-font-names display ol-requested)
		       ol-requested)))
		 
		 ;; Given fontname exists?
		 (when (list-font-names display fontname)
		   fontname)
		 
		 ;; Last resort is to use "fixed" font.
		 "fixed"))))
	  
	  ;; Add match to cache.
	  (setf (getf (display-plist display) 'fontnames)
		(nconc `((,scale ,fontname ,best-match)) cache))
	  best-match)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                spacing-mixin                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defparameter *default-display-horizontal-space* 0
  "The default size of the horizontal spacing, in points.")

(defparameter *default-display-vertical-space* 0
  "The default size of the vertical spacing, in points.")



;;;  Special types to support conversion of resource defaults to pixel units
(deftype default-horizontal-space () 'integer)
(deftype default-vertical-space   () 'integer)



(defmethod convert ((contact contact) (value (eql :default)) (type (eql 'default-horizontal-space)))
  (point-pixels (contact-screen contact) *default-display-horizontal-space*))

(defmethod convert ((contact contact) (value (eql :default)) (type (eql 'default-vertical-space)))
  (point-pixels (contact-screen contact) *default-display-vertical-space*))



(defcontact spacing-mixin ()
  ((horizontal-space :type	     integer
		     :initarg	     :horizontal-space
		     :reader 	     display-horizontal-space	; setf defined below
		     :documentation  "The size of the horizontal spacing in pixels")
   (vertical-space   :type	     integer
		     :initarg	     :vertical-space
		     :reader 	     display-vertical-space	; setf defined below
		     :documentation  "The size of the vertical spacing in pixels"))
  (:resources
    (horizontal-space :type     default-horizontal-space
		      :initform :default)
    (vertical-space   :type     default-vertical-space
		      :initform :default))
  
  (:documentation  "Provides horizontal and vertical spacing resources for core contacts"))




(defmethod (setf display-horizontal-space) (new-value (contact spacing-mixin))
  (with-slots (horizontal-space) contact
    (check-type new-value (or (member :default) integer) ":DEFAULT or INTEGER")
    (setf horizontal-space (if (eq :default new-value)
			       (convert contact new-value 'default-horizontal-space)
			       new-value))))



(defmethod (setf display-vertical-space) (new-value (contact spacing-mixin))
  (with-slots (vertical-space) contact
    (check-type new-value (or (member :default) integer) ":DEFAULT or INTEGER")
    (setf vertical-space (if (eq :default new-value)
			       (convert contact new-value 'default-vertical-space)
			       new-value))))
