;;; -*- Mode:Lisp; Package:CLUEI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package "CLUEI")

(export
  '(
    contact-root-shell 
    override-shell
    shell
    shell-mapped
    shell-owner
    shell-unmapped
    sm-client-host
    sm-command
    top-level-session
    top-level-shell
    transient-shell
    with-wm-properties
    with-wm-properties 
    wm-base-height
    wm-base-width
    wm-colormap-owners
    wm-delta-height
    wm-delta-width
    wm-gravity
    wm-group
    wm-icon
    wm-icon-mask
    wm-icon-title
    wm-icon-x
    wm-icon-y
    wm-initial-state
    wm-keyboard-input
    wm-max-aspect
    wm-max-height
    wm-max-width
    wm-message 
    wm-message-protocol
    wm-message-timestamp
    wm-min-aspect
    wm-min-height
    wm-min-width
    wm-protocols-used
    wm-shell    
    wm-title
    wm-user-specified-position-p
    wm-user-specified-size-p
    )
  'cluei)



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 				      Shell                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defcontact shell (composite)
  ((state        :type     (member :withdrawn :iconic :mapped)
		 :reader   contact-state)	; setf defined below
   
   (owner        :type     composite
		 :reader   shell-owner))
  (:resources
    (state       :type     (member :withdrawn :iconic :mapped)
		 :initform (shell-default-state)))
  
  (:documentation
    "Base class for all shell contacts."))



(defmethod initialize-instance :around ((shell shell) &rest initargs)
  (declare (ignore initargs))
  ;; Must bind new shell in order to evaluate initform for state slot.
  (let ((*new-shell* shell))
    (declare (special *new-shell*))
    (call-next-method)))
    
(defmethod initialize-instance :after ((shell shell) &rest initargs)
  (declare (ignore initargs))
  (with-slots (background owner) shell
    ;; Use initial background, if not :parent-relative.
    ;; Else look for background inherited from owner.
    (setf background
	  (do*
	    ((shell     shell ancestor)
	     (ancestor  owner (if (typep ancestor 'shell)
				  (shell-owner ancestor)
				  (contact-parent ancestor)))
	     bg)
	    ((not (and ancestor (eq (setf bg (contact-background shell))
				    :parent-relative)))
	     bg)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 				 Override Shell                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact override-shell (shell)
  ()
  (:resources
    (override-redirect :initform :on))
  (:documentation
    "Base class for shells which override the window manager."))

(defmethod (setf contact-state) :around (new-state (shell override-shell))
  ;; :iconic is equivalent to :withdrawn for override-shell
  (call-next-method
    (if (eq :iconic new-state) :withdrawn new-state)
    shell))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;		    Batching window manager property changes                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmacro wm-properties-changed (shell &optional default)
  "Return list of changed window manager properties for the SHELL."
  `(getf (window-plist ,shell) 'wm-properties-changed ,default))

(defmacro wm-changing-properties-p (shell)
  "Return true if currently batching changes to window manager properties of the SHELL."
  `(not (eq :undefined (wm-properties-changed ,shell :undefined))))

(defsetf wm-changing-properties-p (shell) (value)
  "Turn off/on batching of changes to window manager properties of the SHELL."
  `(if ,value
       (setf (wm-properties-changed ,shell) nil)
       (remf (window-plist ,shell) 'wm-properties-changed)))

(defmacro with-wm-properties ((shell) &body body)
  "Batch all changes to window manager properties of the SHELL into one request
after the BODY." 
  `(progn
     (setf (wm-changing-properties-p ,shell) t)
     ,@body
     (when (wm-properties-changed ,shell)
       (apply #'wm-batch-change-properties ,shell (wm-properties-changed ,shell)))
     (setf (wm-changing-properties-p ,shell) nil)))


(defmacro define-wm-batch-change-properties ()
  "Generate WM-BATCH-CHANGE-PROPERTIES function definition."
  `(defun wm-batch-change-properties (shell &rest properties)
     "Change the properties which control window manager interaction."
     ,@(let (code)
	 (dolist (p
		   '(
		     ;; class, transient-for properties not included because
		     ;; they should only be changed during initialization
		     
		     client-machine
		     colormap-windows
		     command
		     hints
		     icon-name
		     name
		     normal-hints
		     protocols
		     )
		   code)
	   (let ((accessor (intern (format nil "WM-CHANGE-~a" (symbol-name p)) 'cluei)))
	     (push
	       `(when (member ',p properties :test #'eq)
		  (,accessor shell))
	       code))))))

(define-wm-batch-change-properties)


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;			      Window Manager Shell                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact wm-shell (shell)
  ((hints                :type     (or null wm-hints)
			 :initform nil 
			 :initarg  :wm-hints
			 :accessor shell-hints)
   
   (normal-hints         :type     (or null wm-size-hints)
			 :initform nil 
			 :initarg  :wm-normal-hints
			 :accessor shell-normal-hints)
   
   (protocols-used       :type     (or null list) 
			 :initform nil 
			 :initarg  :wm-protocols-used
			 :accessor wm-protocols-used)
   
   (title                :type     (or null string)
			 :initform nil 
			 :initarg  :wm-title
			 :accessor wm-title)

   (reparented-p         :type     boolean
			 :initform nil))

  (:resources
    (event-mask       :initform #.(make-event-mask :structure-notify))
    
    (wm-base-height   :type (or null card16))
    (wm-base-width    :type (or null card16))             
    (wm-delta-height  :type (or null card16))                 
    (wm-delta-width   :type (or null card16))              
    (wm-gravity       :type (or null (member :north-west :north  :north-east
					      :west       :center :east
					      :south-west :south  :south-east)))          
    (wm-initial-state :type (or null (member :normal :iconic)))
    (wm-max-aspect    :type (or null number))             
    (wm-max-height    :type (or null card16))             
    (wm-max-width     :type (or null card16))            
    (wm-min-aspect    :type (or null number))             
    (wm-min-height    :type (or null card16))             
    (wm-min-width     :type (or null card16))            
     wm-title
    (wm-user-specified-position-p :type boolean)
    (wm-user-specified-size-p     :type boolean))
    
  (:documentation
    "Base class for shells which interact with the window manager."))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			     WM_PROTOCOLS accessors                            |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod wm-change-protocols ((shell wm-shell))
  "Send a request to change the WM_PROTOCOLS property for the SHELL."
  (when (realized-p shell)
    (with-slots (protocols-used) shell
      (let ((display (contact-display shell)))
	;; Crock! Work around bug in CLX R4.2 --- make sure all intern-atom's occur 
	;; outside other request functions.
	(intern-atom display :wm_protocols)
	(if protocols-used 
	    (change-property
	      shell :wm_protocols
	      (mapcar #'(lambda (atom) (intern-atom display atom)) protocols-used)
	      :atom
	      32)
	    (delete-property shell :wm_protocols))))))

(defmethod (setf wm-protocols-used) :after (new-protocols (shell wm-shell))
  (declare (ignore new-protocols))
  (if (wm-changing-properties-p shell)
      (pushnew 'protocols (wm-properties-changed shell))
      (wm-change-protocols shell)))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			        WM_NAME accessors                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod wm-change-name ((shell wm-shell))
  "Send a request to change the WM_NAME property for the SHELL."
  (when (realized-p shell)
    (with-slots (title name) shell
      (setf (wm-name shell) (or title name)))))

(defmethod (setf wm-title) :after (new-title (shell wm-shell))
  (declare (ignore new-title))
  (if (wm-changing-properties-p shell)
      (pushnew 'name (wm-properties-changed shell))
      (wm-change-name shell)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			       WM_CLASS accessors                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

;; This should only be called during initialization
(defmethod wm-change-class ((shell wm-shell))
  "Send a request to change the WM_CLASS property for the SHELL."
  (when (realized-p shell)
    (let ((d (contact-display shell)))
      (set-wm-class shell
		    (display-name d)
		    (display-class d)))))
			    

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			    WM_NORMAL_HINTS accessors                          |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf shell-normal-hints) :after (new-normal-hints (shell wm-shell))
  (declare (ignore new-normal-hints))
  (wm-change-normal-hints shell))

(defmethod wm-change-normal-hints ((shell wm-shell))
  "Send a request to change the WM_NORMAL_HINTS property for the SHELL."
  (when (realized-p shell)
    (with-slots (normal-hints) shell
      (if normal-hints
	  (setf (wm-normal-hints shell) normal-hints)
	  (delete-property shell :wm_normal_hints)))))

(defun wm-update-normal-hints (shell)
  "Record an update to the WM_NORMAL_HINTS property for the SHELL."
  (if (wm-changing-properties-p shell)
      (pushnew 'normal-hints (wm-properties-changed shell))
      (wm-change-normal-hints shell)))

(defmethod wm-user-specified-size-p ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-user-specified-size-p normal-hints))))

(defmethod (setf wm-user-specified-size-p) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-user-specified-size-p normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-user-specified-position-p ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-user-specified-position-p normal-hints))))

(defmethod (setf wm-user-specified-position-p) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-user-specified-position-p normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-min-width ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-min-width normal-hints))))

(defmethod (setf wm-min-width) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-min-width normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-min-height ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-min-height normal-hints))))

(defmethod (setf wm-min-height) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-min-height normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-min-aspect ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-min-aspect normal-hints))))

(defmethod (setf wm-min-aspect) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-min-aspect normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-max-width ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-max-width normal-hints))))

(defmethod (setf wm-max-width) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-max-width normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-max-height ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-max-height normal-hints))))

(defmethod (setf wm-max-height) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-max-height normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-max-aspect ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-max-aspect normal-hints))))

(defmethod (setf wm-max-aspect) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-max-aspect normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-gravity ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-win-gravity normal-hints))))

(defmethod (setf wm-gravity) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-win-gravity normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-delta-width ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-width-inc normal-hints))))

(defmethod (setf wm-delta-width) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-width-inc normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-delta-height ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-height-inc normal-hints))))

(defmethod (setf wm-delta-height) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-height-inc normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-base-width ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-base-width normal-hints))))

(defmethod (setf wm-base-width) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-base-width normal-hints) value))
  (wm-update-normal-hints shell)
  value)

(defmethod wm-base-height ((shell wm-shell))
  (with-slots (normal-hints) shell
    (when normal-hints
      (wm-size-hints-base-height normal-hints))))

(defmethod (setf wm-base-height) (value (shell wm-shell))
  (with-slots (normal-hints) shell
    (unless normal-hints
      (setf normal-hints (make-wm-size-hints)))
    (setf (wm-size-hints-base-height normal-hints) value))
  (wm-update-normal-hints shell)
  value)




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			 WM_HINTS accessors for wm-shell                       |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf shell-hints) :after (new-hints (shell wm-shell))
  (declare (ignore new-hints))
  (wm-change-hints shell))

(defmethod wm-change-hints ((shell wm-shell))
  "Send a request to change the WM_HINTS property for the SHELL."
  (when (realized-p shell)
    (with-slots (hints) shell
      (if hints
	  (setf (wm-hints shell) hints)
	  (delete-property shell :wm_hints)))))

(defun wm-update-hints (shell)
  "Record an update to the WM_HINTS property for the SHELL."
  (if (wm-changing-properties-p shell)
      (pushnew 'hints (wm-properties-changed shell))
      (wm-change-hints shell)))

(defmethod wm-group ((shell wm-shell))
  (with-slots (hints) shell
    (let ((id (when hints (wm-hints-window-group hints))))
      (when id
	(xlib::lookup-window (contact-display shell) id)))))

(defmethod (setf wm-group) (value (shell wm-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-window-group hints) (window-id value)))
  (wm-update-hints shell)
  value)

(defmethod wm-keyboard-input ((shell wm-shell))
  (with-slots (hints) shell
    (when hints
      (wm-hints-input hints))))

(defmethod (setf wm-keyboard-input) (value (shell wm-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-input hints) value))
  (wm-update-hints shell)
  value)

(defmethod wm-initial-state ((shell wm-shell))
  (with-slots (hints) shell
    (when hints
      (wm-hints-initial-state hints))))

(defmethod (setf wm-initial-state) (value (shell wm-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-initial-state hints) value))
  (wm-update-hints shell)
  value)



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Initialization                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod initialize-instance :after ((shell wm-shell) &key
				       wm-base-height wm-base-width wm-delta-height wm-delta-width              
				       wm-initial-state wm-gravity
				       wm-max-aspect wm-max-height wm-max-width            
				       wm-min-aspect wm-min-height wm-min-width
				       wm-user-specified-position-p wm-user-specified-size-p
				       wm-group wm-keyboard-input
				       &allow-other-keys)

  ;; Initialize hints from initargs
  (when wm-base-height (setf (wm-base-height shell) wm-base-height))
  (when wm-base-width (setf (wm-base-width shell) wm-base-width))
  (when wm-delta-height (setf (wm-delta-height shell) wm-delta-height))
  (when wm-delta-width (setf (wm-delta-width shell) wm-delta-width))  
  (when wm-initial-state (setf (wm-initial-state shell) wm-initial-state))
  (when wm-gravity (setf (wm-gravity shell) wm-gravity))
  (when wm-max-aspect (setf (wm-max-aspect shell) wm-max-aspect))
  (when wm-max-height (setf (wm-max-height shell) wm-max-height))
  (when wm-max-width (setf (wm-max-width shell) wm-max-width))  
  (when wm-min-aspect (setf (wm-min-aspect shell) wm-min-aspect))
  (when wm-min-height (setf (wm-min-height shell) wm-min-height))
  (when wm-min-width (setf (wm-min-width shell) wm-min-width))
  (when wm-user-specified-position-p (setf (wm-user-specified-position-p shell) wm-user-specified-position-p))
  (when wm-user-specified-size-p (setf (wm-user-specified-size-p shell) wm-user-specified-size-p))
  (when wm-group (setf (wm-group shell) wm-group))
  (when wm-keyboard-input (setf (wm-keyboard-input shell) wm-keyboard-input)))

(defmethod realize :after ((shell wm-shell))
  ;; Set initial property values
  (wm-change-class        shell)
  (wm-change-hints        shell)
  (wm-change-name         shell)
  (wm-change-normal-hints shell)
  (wm-change-protocols    shell))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 		      Window Manager Shell: State management                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf contact-state) (new-state (shell wm-shell))
  (check-type new-state (member :withdrawn :iconic :mapped))    
  
  (with-slots (parent display state) shell
    (unless (eq state new-state)
      (let ((old-state state))
	
	(setf state new-state)
	
	(if
	  (realized-p shell)
	  
	  ;; Change state now -- but don't send side-effect requests if inside
	  ;; without-requests wrapper (i.e. eq *contact-notified*) ---
	  ;; that is, if responding to notification of state change from window mgr.
	  (when
	    ;; Was a (un)map request actually sent?
	    (case new-state
	      (:mapped    (shell-mapped shell)
			  (unless (eq *contact-notified* shell)
			    (map-window shell)
			    t))				 ; Request sent
	      
	      (:iconic    (if (eq old-state :withdrawn)
			      
			      (unless (eq *contact-notified* shell)
				(unless (eq :iconic (wm-initial-state shell))
				  (setf (wm-initial-state shell) :iconic))
				(map-window shell)
				nil)			 ; No :map-notify coming, so don't wait!
			      
			      (progn
				(unless (eq *contact-notified* shell)
				  (send-event parent
					      :client-message
					      #.(make-event-mask :substructure-redirect :substructure-notify)
					      :window shell
					      :type :wm_change_state
					      :format 32
					      :data '(3) ; Crock: this should be an xlib defconstant
					      ))	
				(shell-unmapped shell)
				t)))			 ; Request sent
	      
	      (:withdrawn (prog1
			    (unless (eq *contact-notified* shell)
			      (unmap-window shell)
			      (send-event parent
					  :unmap-notify
					  #.(make-event-mask :substructure-redirect :substructure-notify)
					  :event-window parent
					  :window shell
					  :configure-p nil)
			      t)			 ; Request sent
			    (shell-unmapped shell))))
	    
	    ;; Wait until resulting :(un)map-notify event has been received.
	    (let ((*ignore-map-notify* t))
	      (declare (special *ignore-map-notify*))
	      (with-event-mode (shell '(:map-notify   (throw-action :map-notify))
				      '(:unmap-notify (throw-action :map-notify)))
		(catch :map-notify
		  ;; Don't update-state to avoid infinite recursion during realization.
		  (loop (process-next-event display nil nil))))))
	  
	  ;; Not realized, let UPDATE-STATE do the work
	  (setf (display-update-flag display) t)))))
  new-state)


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 		       Window Manager Shell: Event Handling                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod handle-event :before ((shell wm-shell) (event event))
  (with-slots (key parent) event
    (case key
      (:reparent-notify
       (setf (slot-value shell 'reparented-p)
	     (not (eq parent (contact-parent shell)))))
      
      (:configure-notify
       ;; Update geometry slots to reflect change made interactively by user.
       (with-slots (x y width height border-width send-event-p) event
	 (without-requests shell   ; No configure request side-effect
	   ;; Use move/resize protocol so that any auxiliary methods will fire.
	   (resize shell width height border-width)

	   ;; Ensure shell coordinates are w.r.t root.
	   (let ((new-x x) (new-y y))
	     (when (and (not send-event-p) (slot-value shell 'reparented-p))
	       (multiple-value-setq (new-x new-y)
		 (translate-coordinates shell (- border-width) (- border-width) (contact-root shell))))
	     (move shell new-x new-y)))))
      
      ((:map-notify :unmap-notify)
       ;; Ignore if in response to (setf contact-state)
       (unless (boundp '*ignore-map-notify*)
	 
	 ;; Update state to reflect change made interactively by user.
	 (without-requests shell
	   (setf (contact-state shell) (if (eq key :map-notify) :mapped :iconic))))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			   :client-message translations                        |
;;;                                                                            |
;;;----------------------------------------------------------------------------+
(defstruct (wm-message (:type #-kcl (vector (unsigned-byte 32))
			      #+kcl vector))
  "Common data fields of all :client-message events from a window/session mgr." 
  (protocol  0)
  (timestamp 0))

(defun wm-message-protocol-atom (wm-message)
  (declare (special *event-display*))
  (atom-name *event-display* (wm-message-protocol wm-message)))

(defevent wm-shell (:wm_take_focus) wm-take-focus)
(defevent wm-shell :focus-in        wm-take-focus)

(defmethod wm-take-focus ((shell wm-shell))
  (with-event (kind key)
    (when (or (not (eq key :focus-in)) (member kind '(:ancestor :nonlinear)))
      (with-slots (children) shell
	(when children
	  (wm-take-focus (first children)))))))

(defmethod wm-take-focus ((composite composite))
  (with-slots (display children) composite
    (when (viewable-p composite)
      (or 
	;; Set focus to composite itself?
	(when (accept-focus-p composite)
	  (set-input-focus display composite :parent)
	  composite)
	
	;; Set focus to one of children?
	(move-focus composite :set)
	(move-focus composite :next)
	
	;; Search for descendant to take focus. 
	(dolist (child children)
	  (let ((focus (wm-take-focus child)))
	    (when focus (return focus))))))))

(defmethod wm-take-focus ((contact contact))
  (with-slots (display) contact
    (when (accept-focus-p contact)
      (set-input-focus display contact :parent)
      contact)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 				Transient Shells                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(defcontact transient-shell (wm-shell)
  ()  
  (:documentation
    "Base class for shells which are never iconified."))

(defmethod realize :after ((shell transient-shell)) 
  ;; Send a request to change the TRANSIENT-FOR property for the SHELL.
  (let ((transient-for (contact-root-shell shell)))
    (if (eq transient-for shell)
	(warn "Can't define TRANSIENT-FOR for root shell ~a." shell)		
	(setf (transient-for shell) transient-for))))

(defun contact-root-shell (contact)
  (do ((rso (contact-top-level contact) (contact-top-level (shell-owner rso))))
      ((eq (shell-owner rso) (contact-root rso)) rso)))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 				Top-Level Shells                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defcontact top-level-shell (wm-shell)
  ((colormap-owners      :type     list
			 :initform nil 
			 :accessor wm-colormap-owners)
   
   (icon-title           :type     (or null string)
			 :initform nil 
			 :initarg  :wm-icon-title
			 :accessor wm-icon-title))
  
  (:resources
    (wm-icon        :type (or null drawable))           
    (wm-icon-mask   :type (or null pixmap))                
     wm-icon-title
    (wm-icon-x      :type (or null int16))             
    (wm-icon-y      :type (or null int16)))

  (:documentation
    "Base class for normal top-level shells."))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			  WM_COLORMAP_WINDOWS accessors                        |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf wm-colormap-owners) :after (new-colormap-owners (shell top-level-shell))
  (declare (ignore new-colormap-owners))
  (if (wm-changing-properties-p shell)
      (pushnew 'colormap-windows (wm-properties-changed shell))
      (wm-change-colormap-windows shell)))

(defmethod wm-change-colormap-windows ((shell top-level-shell))
  "Send a request to change the COLORMAP-WINDOWS property for the SHELL."
  (when (realized-p shell)
    (with-slots (colormap-owners) shell
      (if colormap-owners
	  (change-property shell :wm_colormap_windows
			   colormap-owners
			   :window
			   32
			   :transform #'window-id)
	  (delete-property shell :wm_colormap_windows)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			     WM_ICON_NAME accessors                            |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod (setf wm-icon-title) :after (new-icon-title (shell top-level-shell))
  (declare (ignore new-icon-title))
  (if (wm-changing-properties-p shell)
      (pushnew 'icon-name (wm-properties-changed shell))
      (wm-change-icon-name shell)))

(defmethod wm-change-icon-name ((shell top-level-shell))
  "Send a request to change the WM_ICON_NAME property for the SHELL."
  (when (realized-p shell)
    (with-slots (icon-title) shell
      (if icon-title
	  (setf (wm-icon-name shell) icon-title)
	  (delete-property shell :wm_icon_name)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			 WM_HINTS accessors for top-level-shell                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod wm-icon ((shell top-level-shell))
  (with-slots (hints) shell
    (when hints
      (or (wm-hints-icon-pixmap hints)
	  (wm-hints-icon-window hints)))))

(defmethod (setf wm-icon) ((value pixmap) (shell top-level-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-icon-pixmap hints) value
	  (wm-hints-icon-window hints) nil))
  (wm-update-hints shell)
  value)

(defmethod (setf wm-icon) ((value window) (shell top-level-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-icon-window hints) value
	  (wm-hints-icon-pixmap hints) nil))
  (wm-update-hints shell)
  value)

(defmethod wm-icon-mask ((shell top-level-shell))
  (with-slots (hints) shell
    (when hints
      (wm-hints-icon-mask hints))))

(defmethod (setf wm-icon-mask) (value (shell top-level-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-icon-mask hints) value))
  (wm-update-hints shell)
  value)
 

(defmethod wm-icon-x ((shell top-level-shell))
  (with-slots (hints) shell
    (when hints
      (wm-hints-icon-x hints))))

(defmethod (setf wm-icon-x) (value (shell top-level-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-icon-x hints) value))
  (wm-update-hints shell)
  value)


(defmethod wm-icon-y ((shell top-level-shell))
  (with-slots (hints) shell
    (when hints
      (wm-hints-icon-y hints))))

(defmethod (setf wm-icon-y) (value (shell top-level-shell))
  (with-slots (hints) shell
    (unless hints
      (setf hints (make-wm-hints)))
    (setf (wm-hints-icon-y hints) value))
  (wm-update-hints shell)
  value)


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Initialization                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod initialize-instance :after ((shell top-level-shell) &key
				       wm-icon wm-icon-mask wm-icon-x wm-icon-y
				       &allow-other-keys)
  ;; Initialize hints from initargs
  (when wm-icon (setf (wm-icon shell) wm-icon))
  (when wm-icon-mask (setf (wm-icon-mask shell) wm-icon-mask))
  (when wm-icon-x (setf (wm-icon-x shell) wm-icon-x))
  (when wm-icon-y (setf (wm-icon-y shell) wm-icon-y)))

(defmethod realize :after ((shell top-level-shell))
  ;; Set initial property values
  (wm-change-colormap-windows shell) 
  (wm-change-icon-name        shell))





;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			    Top-Level Session Shells                           |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defcontact top-level-session (top-level-shell)
  ((client-host          :type     (or null string)
			 :initform nil 
			 :initarg  :sm-client-host
			 :accessor sm-client-host)

   (command              :type     (or null string)
			 :initform nil 
			 :initarg  :sm-command
			 :accessor sm-command))

  (:resources
     sm-command)
  
  (:documentation
    "Base class for top-level shells that communicate with a session manager."))

(defmethod initialize-instance :after ((shell top-level-session) &rest initargs)
  (declare (ignore initargs))
  ;; Define default client host.
  (with-slots (client-host) shell
    (unless client-host
      (setf client-host
	    (string-capitalize (host-namestring (user-homedir-pathname)))))))

(defmethod realize :after ((shell top-level-session))
  ;; Set initial property values
  (wm-change-client-machine shell)
  (wm-change-command        shell))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			     WM_CLIENT_MACHINE accessors                       |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod (setf sm-client-host) :after (new-client-host (shell top-level-session))
  (declare (ignore new-client-host))
  (if (wm-changing-properties-p shell)
      (pushnew 'client-machine (wm-properties-changed shell))
      (wm-change-client-machine  shell)))

(defmethod wm-change-client-machine  ((shell top-level-session))
  "Send a request to change the WM_CLIENT_MACHINE property for the SHELL."
  (when (realized-p shell)
    (with-slots (client-host) shell
      (if client-host
	  (setf (wm-client-machine shell) client-host)
	  (delete-property shell :wm_client_machine)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			     WM_COMMAND accessors                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod (setf sm-command) :after (new-command (shell top-level-session))
  (declare (ignore new-command))
  (if (wm-changing-properties-p shell)
      (pushnew 'command (wm-properties-changed shell))
      (wm-change-command shell)))

(defmethod wm-change-command ((shell top-level-session))
  "Send a request to change the WM_COMMAND property for the SHELL."
  (when (realized-p shell)
    (with-slots (command) shell
      (if command
	  (setf (wm-command shell) command)
	  (delete-property shell :wm_command)))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			     Shell: State management                           |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun shell-default-state ()
  (declare (special *new-shell*))		       ; Bound to shell by initialize-instance :around
  
  ;; WARNING: We assume this function is called during (initialize-instance :after (basic-contact)).
  ;; Therefore, the parent slot still contains the owner and has not yet been reset to the root!
  
  ;; Is this a root shell?
  (if (eq (contact-root *new-shell*) (contact-parent *new-shell*))
      ;; Yes...
      :mapped
      
      ;; No...
      :withdrawn))

(defmethod initial-state-transition ((shell shell))
  "Return the old-state/new-state for the initial (setf contact-state) after CONTACT
   is realized. Return nil if (setf contact-state) need not be called, i.e. no
   initial state transition is necessary."
  (with-slots (state) shell
    (unless (eq :withdrawn state)
      (values :withdrawn state))))

(defmethod shell-mapped ((shell shell))
  (apply-callback shell :map))

(defmethod shell-unmapped ((shell shell))
  (apply-callback shell :unmap))

(defmethod shell-mapped :before ((shell shell))
  ;; Place up front when mapped, since the  window manager cannot
  ;; intervene to inform stacking priority.
  (setf (window-priority shell) :above))

(defmethod (setf contact-state) (new-state (shell shell))
  (check-type new-state (member :withdrawn :mapped))    
  
  (with-slots (parent display state) shell
    (unless (eq state new-state)
      (setf state new-state)
      
      (if
	(realized-p shell)
	
	(case new-state
	  (:mapped    (shell-mapped shell)
		      (map-window shell))	    	    
	  
	  (:withdrawn (unmap-window shell)
		      (shell-unmapped shell)))
	
	;; Not realized, let UPDATE-STATE do the work
	(setf (display-update-flag display) t))))
  new-state)



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 			   Shell: Geometry Management                          |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod add-to-parent ((self shell))
  (with-slots (parent owner) self
    (let ((root (contact-root self)))
      
      ;; Initialize shell owner
      (setf owner parent)
      (with-slots (shells) owner
	;; Add to owner's shells list unless...
	(unless (or
		  ;; ... Owner is root (not necessary in this case because
		  ;; root cannot be destroyed, so its shells list is never cleaned up)...
		  (eq owner root)

		  ;; ... Shell already belongs to list (an error we just ignore)
		  (member self shells :test #'eq))
	    (setf shells (nconc shells (cons self nil)))))

      ;; A shell is always a child of its root
      (setf parent root)
      (add-child root self))))

(defmethod contact-resource-parent ((shell shell))
  (slot-value shell 'owner))

(defmethod add-child :before ((shell shell) child &key)  
  (with-slots (children) shell
    (when children
      (error "~s already has child ~s; cannot add child ~s."
	     shell
	     (first children)
	     child))))

(defmethod manage-geometry ((parent shell) (child contact) x y width height border-width &key) 
  (declare (type (or null int16) x y)
	   (type (or null card16) width height border-width))  
  
  (let* ((child-bw     (or border-width (contact-border-width child)))
	 (required-pos (- child-bw)))
    
    (with-slots ((parent-width width) (parent-height height)) parent 
      (multiple-value-bind (size-approved-p approved-width approved-height)
	  
	  (if (and (realized-p parent)
		   (or (setf width  (unless (eq width  (contact-width child))  width))
		       (setf height (unless (eq height (contact-height child)) height))))
	      
	      ;; Request corresponding change in top-level shell size
	      ;; Since shell is top-level, changed size is effected immediately
	      (values
		(change-geometry parent :width width :height height)
		parent-width
		parent-height)
	      
	      ;; Unrealized shell approves and adopts change immediately
	      (values
		t
		(setf parent-width  (or width  (contact-width child)))
		(setf parent-height (or height (contact-height child)))))

	;; Shell child always positioned so that its borders are invisible.
	(values
	  (and size-approved-p
	       (or (null x) (= x required-pos))
	       (or (null y) (= y required-pos)))    
	  required-pos
	  required-pos
	  approved-width   
	  approved-height
	  child-bw)))))


(defmethod manage-priority ((parent shell) child priority sibling &key)  
  (declare (type (member :above :below :top-if :bottom-if :opposite) priority)
	   (type (or null contact) sibling))
  (declare (ignore child priority sibling))
  ;; Never approved since shell has only one child
  nil)

(defmethod change-layout ((shell shell) &optional newly-managed)
  (declare (ignore newly-managed))
  (with-slots (children width height) shell
    (when children
      ;; Shell assumes size of its content
      (let* ((content        (first children))
	     (content-width  (contact-width content))
	     (content-height (contact-height content))
	     (content-bw     (contact-border-width content)))
	
	(if (realized-p shell)
	    ;; Negotiate with window mgr
	    (change-geometry shell
			     :width  content-width
			     :height content-height)
	    
	    ;; Else change size of unrealized shell immediately
	    (setf width  content-width
		  height content-height))
	
	;; Position content to hide content border
	(with-state (content)
	  (move content (- content-bw) (- content-bw)))))))


(defmethod resize :after ((shell shell) width height border-width)
  (declare (ignore border-width)) 
  (with-slots (children) shell
    (let ((content (first children)))
      (when content
	;; Force content to have same size
	(resize content width height
		(contact-border-width content))))))


(defmethod preferred-size ((shell shell) &key width height border-width)
  (declare (ignore border-width))

  (with-slots (children (current-width width) (current-height height)) shell
    (let ((content (first children)))
      (multiple-value-bind (preferred-width preferred-height)	
	  (if content
	      ;; Return preferred size of content
	      (preferred-size content :width width :height height)

	      ;; Else accept suggested or current size
	      (values (or width current-width) (or height current-height)))
	
	(values preferred-width preferred-height 0)))))



