;;;  -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

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

;;;
;;;  Dialog-button and dialog-item, buttons and items that bring up general
;;;  dialogs when messed with.  Dialogs include menus, property-sheets, commands,
;;;  and confirms.  Also defined here is the menu protocol event translations,
;;;  for press-drag-release and click-move-click.


(in-package "CLIO-OPEN")

(export '(
	  dialog-button
	  dialog-item
	  make-dialog-item
	  make-dialog-button
	  button-dialog
	  present-dialog			; As good a place as any.
	  )
	'clio-open)


;;;
;;;  Contact definitions and interface functions.

(defcontact dialog-button (action-button)
  ((dialog     :type         (or null list function contact)
	       :reader	     button-dialog	; Note (setf button-dialog) below.
	       :initarg      :dialog
	       :initform     nil))
   (:resources
     (dialog :initform nil
	     :type (or null list function contact))))

(defun make-dialog-button (&rest initargs)
  (apply #'make-contact 'dialog-button initargs))

(defmethod (setf button-dialog) (new-dialog (button dialog-button))
   (check-type new-dialog (or null contact))
   (with-slots (preferred-width dialog) button
     ;;  (LG) Force preferred-size to recalculate width.
     (setq preferred-width nil)
     (when (and dialog
		(not (eq dialog new-dialog)))
       (disassociate-dialog-from-button dialog button)))
   (associate-dialog-with-button new-dialog button))


;;  A DIALOG-ITEM is a specialization of an ACTION-ITEM and is intended for use
;;  in OL compliant menus.  It differs from a DIALOG-BUTTON in appearance as well
;;  as in its sensitivity to various mouse gestures depending on the mode of the
;;  menu which contains it.
(defcontact dialog-item (action-item)
   ((dialog     :type         (or null list function contact)
		:reader	      button-dialog	; Note (setf button-dialog) below.
		:initarg      :dialog
		:initform     nil)
    (last-x	:type	      integer		; For drag-right checking.
		:initform     0)
    (active-x   :type	      (or null integer)	; Ditto.
		:initform     nil))
   (:resources
     (dialog :initform nil
	     :type (or null list function contact))))

(defun make-dialog-item (&rest initargs)
  (apply #'make-contact 'dialog-item initargs))

(defmethod (setf button-dialog) (new-dialog (item dialog-item))
   (check-type new-dialog (or null contact))
   (with-slots (preferred-width dialog) item
     ;;  (LG) Force preferred-size to recalculate width.
     (setq preferred-width nil)
     (when (and dialog
		(not (eq dialog new-dialog)))
       (disassociate-dialog-from-button dialog item)))
   (associate-dialog-with-button new-dialog item))

(defmethod resize :after ((item dialog-item) width height border-width)
   (declare (ignore width height border-width))
   (with-slots (active-x) item
     (setq active-x nil)))

;;;
;;;  Other definitions.

;;  A way to get from the dialog back to the button.
(defmacro button-owning-dialog (contact)
   `(getf (window-plist ,contact) 'button-owning-dialog))

(defun pointer-inside-menu-p (button menu)
   (declare (ignore button))
   (multiple-value-bind (pointer-x pointer-y same-screen-p)
       (pointer-position menu)
     (and same-screen-p (inside-contact-p menu pointer-x pointer-y))))

;;  A handy place to put the state (nil, press-drag-release, or click-move-click).
(defmacro menu-state (menu)
   `(getf (window-plist ,menu) 'menu-state))

;;  Flag used to handle off-menu presses and releases.
(defmacro menu-button-press-p (menu)
   `(getf (window-plist ,menu) 'menu-button-press-p))

(defparameter *menu-item-drag-right-distance* 5
   "Distance in pixels to drag the pointer rightward over a menu item
to bring up a submenu.")

(defparameter *menu-cursor-index* top-left-arrow-cursor	; (That's 132.)
   "Index of glyph used for pointer cursor when grabbed by menu.")

;;  Flag used to prevent multiple drag-mode submenus from appearing.
(defmacro menu-present-in-progress (container)
   `(getf (window-plist ,container) 'present-dialog-in-progress))

;;;
;;;  Initialisation code.

;;  Allow for a class-name-symbol or list of class-name and initargs by
;;  parsing the :dialog initarg and making it a contact before passing it
;;  on to the rest of the init method.
(defmethod initialize-instance :around ((self dialog-button) &rest initargs &key dialog parent)
   (let ((new-dialog (parse-dialog-spec dialog parent)))
     (apply #'call-next-method self :dialog new-dialog initargs)))

(defmethod initialize-instance :around ((self dialog-item) &rest initargs &key dialog parent)
   (let ((new-dialog (parse-dialog-spec dialog parent)))
     (apply #'call-next-method self :dialog new-dialog initargs)))

(defun parse-dialog-spec (spec parent)
   (etypecase spec
     ((or contact null)    spec)
     ((or symbol function) (funcall spec :parent parent))
     (list		   (apply (car spec) :parent parent (cdr spec)))))

(defmethod initialize-instance :after ((self dialog-button) &key &allow-other-keys)
   (associate-dialog-with-button (button-dialog self) self))

(defmethod initialize-instance :after ((item dialog-item) &key &allow-other-keys)
   (associate-dialog-with-button (button-dialog item) item))

(defmethod associate-dialog-with-button ((new-dialog t) button)
   (with-slots (dialog) (the dialog-button button)
     (setq dialog new-dialog)))

(defmethod associate-dialog-with-button :after ((new-dialog menu) button)
   (associate-menu-with-dialog-button new-dialog button))

;;  These dialogs use callback instead of event because the action is supposed to happen on release,
;;  while pressing just highlights.  The callback works, because that's what action-button
;;  is doing.
(defmethod associate-dialog-with-button :after ((new-dialog command) button)
   (add-callback button :release #'(lambda ()
				     (present-dialog (button-dialog button)))))

(defmethod associate-dialog-with-button :after ((new-dialog confirm) button)
   ;;  A bit of a hack for confirm:  We don't want the menu to dismiss until the
   ;;  the confirm does, so if there's a dismiss-menu callback (indicating that
   ;;  our owning button is within a menu), we remove it, extracting its menu
   ;;  argument, and put it on the confirm's :accept and :cancel callbacks instead.
   (let* ((off-callbacks (callback-p button :off))
	  (dismiss-callback (assoc #'dismiss-menu off-callbacks)))
     (when dismiss-callback
       (delete-callback button :off #'dismiss-menu)	; Move from here ...
       (when (typep button 'toggle-button)
	 (delete-callback button :off #'dismiss-menu))
       (add-callback new-dialog :cancel #'dismiss-menu (second dismiss-callback))	; ... to here.
       (add-callback new-dialog :accept #'dismiss-menu (second dismiss-callback))))
   (setf (button-owning-dialog new-dialog) button)
   (add-callback button :release #'(lambda ()
				     (setf (confirm-near (button-dialog button))
					   (viewable-ancestor button))
				     (present-dialog (button-dialog button)))))

(defmethod associate-dialog-with-button :after ((new-dialog property-sheet) button)
   (add-callback button :release #'(lambda ()
				     (present-dialog (button-dialog button)))))

(defun associate-menu-with-dialog-button (menu button)
   (declare (type menu menu)
	    (type (or NULL dialog-button dialog-item) button))
   ;;  Make-menu handles associating dismiss-menu with :on and :off callbacks
   ;;  on each item.  This :unmap callback handles taking down submenus and
   ;;  doing choice-item-release when the menu is withdrawn by dismiss-menu.
   (add-callback menu :unmap #'dismiss-menu-group menu button)
   ;;  Remember owning button for later use in event-handlers.
   (setf (button-owning-dialog menu) button))


(defmethod disassociate-dialog-from-button ((dialog menu) button)
   (disassociate-menu-from-dialog-button dialog button))

(defmethod disassociate-dialog-from-button ((dialog command) button)
   (delete-callback button :release))

(defmethod disassociate-dialog-from-button ((dialog confirm) button)
   (add-callback dialog :cancel #'dismiss-menu)
   (add-callback dialog :accept #'dismiss-menu)
   (setf (button-owning-dialog dialog) nil)
   (delete-callback button :release))

(defmethod disassociate-dialog-from-button ((dialog property-sheet) button)
   (delete-callback button :release))

(defmethod disassociate-dialog-from-button ((dialog null) button)
   (declare (ignore button))
   nil)

(defun disassociate-menu-from-dialog-button (menu button)
   (declare (type menu menu)
	    (ignore button))
   (setf (button-owning-dialog menu) nil)
   (delete-callback menu :unmap #'dismiss-menu-group))

;;  Hook for an off-menu-press problem:  when leaving an item, turn off the
;;  off-menu-press flag so an off-menu-release won't dismiss the menu, because
;;  the press was within an item, not off the menu.  Also a hook for a confirm-
;;  related grab problem:  when firing an action-item, ungrab the pointer and
;;  set the menu-state to a special state, finishing, that just ignores enter
;;  and leave events on the menu.  We need to do this for items whose callbacks
;;  call confirm-p or some similar dialog-presenting function, so the dialog
;;  gets a chance to get button presses and releases.
(defmethod add-menu-item-callbacks :after (item menu)
   (add-callback item :canceling-change
		 #'(lambda (to-selected-p)
		     (declare (ignore to-selected-p))
		     (setf (menu-button-press-p menu) nil)))
   (add-callback item :release #'(lambda ()
				   (setf (menu-state menu) 'finishing)
				   (ungrab-pointer (contact-display menu))
				   )))

(defun viewable-ancestor (contact)
   (let ((parent (typecase contact
		   (menu
		    (button-owning-dialog contact))
		   (shell
		    (shell-owner contact))
		   (otherwise
		    (contact-parent contact)))))
     (if (typep parent 'root)
	 contact
	 (let ((ancestor (viewable-ancestor parent)))
	   (if (and (mapped-p contact)
		    (eq ancestor parent))
	       contact
	       ancestor)))))


;;;
;;;  Action functions for dialog-button and dialog-item.

;;  Present-dialog methods for other dialogs are in their respective files.
;;  This method starts the menu protocol defined below in the event handlers,
;;  and sets position according to the complicated Open Look rules.
(defmethod present-dialog ((menu menu) &key x y button state)
   (declare (type (or card16 null) x y))
   (declare (ignore x y))			; Stick to Open Look positioning rules.
   (check-type button (or (member :button-1 :button-2 :button-3 :button-4 :button-5) null))
   (check-type state (or mask16 null))
   (let ((owning-button (button-owning-dialog menu)))
     (cond (owning-button
	    (set-menu-position owning-button menu
			       (and button state
				    (not (logtest (make-state-mask button) state)))))
	   (:else
	    ;;  No button, this is a pop-up menu.
	    (set-menu-position nil menu
			       (and button state
				    (not (logtest (make-state-mask button) state))))
	    (associate-menu-with-dialog-button menu nil)
	    ;;  Need this to do the initial grab-handoff to the menu so we can
	    ;;  start popups in press-drag-release -- a quick enough button-release
	    ;;  will switch to click-move-click, but I'm not sure of the mechanism.
	    ;;  Need to do it as a callback because we can't grab until we're mapped,
	    ;;  and that doesn't happen immediately.
	    (add-callback menu :map
			  #'(lambda ()
			      (ungrab-pointer (contact-display menu))
			      (grab-pointer menu #.(make-event-mask :button-release :enter-window :leave-window)
					    :owner-p t
					    :cursor (contact-glyph-cursor menu *menu-cursor-index*))))))
     (setf (contact-state menu) :mapped)
     (setf (menu-state menu) nil)))

;;  Default case, just position it and map it (this method handles commands and
;;  property-sheets, but not confirms or menus).
(defmethod present-dialog ((contact contact) &key x y button state)
   (declare (type (or (member :button-1 :button-2 :button-3 :button-4 :button-5) null) button)
	    (type (or mask16 null) state))
   (declare (ignore button state))
   (check-type x (or card16 null))
   (check-type y (or card16 null))
   (unless (or x y)
     (multiple-value-setq (x y)
       (pointer-position (contact-parent contact))))
   (change-geometry contact :x x :y y :accept-p t)
   (setf (contact-state contact) :mapped))


;;  This function is called in the :unmap callback of a menu, which dismiss-menu
;;  causes to happen by withdrawing the menu.  Other cleanup, like taking down any
;;  submenus and releasing the button or item, happens here.
(defun dismiss-menu-group (menu button)
   ;;  If there are any submenus up, take them down, too.
   (mapc #'dismiss-submenu-item
	 (composite-children (menu-choice menu)))

   (when button
     (setf (menu-present-in-progress (contact-parent button)) nil))

   (when (and button				; Button will be NIL for pop-up.
	      ;;  Special state, only during leave-notify of menu when exiting
	      ;;  to left, which means take down the menu but not any superiors.
	      (not (eq (menu-state menu) 'exiting-to-left)))
     ;; NOTE we defer the "release" of the button until the associated
     ;; menu is dismissed.  We do this because the menu button will
     ;; normally never see the actual release event.  Note also that,
     ;; as an action button, the :ON callback is not invoked until the
     ;; release method is invoked.
     (choice-item-release button)
     (release-select button)))

(defmethod dismiss-submenu-item (item)
   (declare (ignore item))
   nil)

(defmethod dismiss-submenu-item ((item dialog-item))
   (with-slots (dialog) item
     (when (and (typep dialog 'menu)
		(mapped-p dialog))
       (dismiss-menu dialog))))

;;  Used to dismiss any dialogs active under a given menu, when bringing
;;  up a different dialog from that menu.
(defmethod dismiss-active-dialogs (item)
   (declare (ignore item))
   nil)

(defmethod dismiss-active-dialogs ((item dialog-item))
   (with-slots (dialog) item
     (when (mapped-p dialog)
       ;;  This flag, originally used when taking down drag-mode menus by
       ;;  exiting to the left, here is used to prevent superior menus of this
       ;;  one from being taken down.
       (when (typep dialog 'menu)
	 (setf (menu-state dialog) 'exiting-to-left))
       (setf (contact-state dialog) :withdrawn)
       (display-button-unhighlighted item))))


;;  Special methods for dialog-button because we need to display the default
;;  on press and select it on release.  The special stuff will only be called
;;  when the dialog is a menu, the others will just call the next method and
;;  get the action-button normal stuff.  I'd like to do this in an :after
;;  method or some other cleaner way, but I need to do this all inside the
;;  conditional, and I'm not sure how to tell whether it was true.
(DEFMETHOD press-select ((dialog-button dialog-button))
   (with-slots (dialog) dialog-button
     (if (typep dialog 'menu)
	 (press-select-show-default dialog-button dialog)
	 (call-next-method))))

(DEFMETHOD press-select ((dialog-item dialog-item))
   (with-slots (dialog) dialog-item
     (if (typep dialog 'menu)
	 (press-select-show-default dialog-item dialog)
	 (call-next-method))))

(defun press-select-show-default (dialog-button dialog)
  (declare (type action-button dialog-button))	       ; Both dialog-item and dialog-button are.
  (with-event (x y)
    (WHEN (and (inside-contact-p dialog-button x y)
	       (choice-item-press dialog-button))
      ;;  Show the default value in the button.
      (with-slots (font label fill-color foreground last-displayed-as width height) dialog-button
	(LET* ((scale (contact-scale dialog-button))
	       (choice (menu-choice dialog))
	       (default (or (choice-default choice)    ; Could be NIL, but Open Look insists.
			    (first (composite-children choice))))
	       (ab-foreground foreground)
	       (ab-fill-color fill-color)
	       (ab-font font)
	       (dims (getf *button-dimensions-by-scale* scale))
	       (text-x (ab-left-button-end-width dims))
	       (text-y (1+ (ab-text-baseline dims))))  ; 0+ for dialog-item.
	  
;;  Experiment:  try changing the label and redisplaying.  Problems:  doesn't
;;  suppress the menu mark, doesn't show the more-text-to-right gray arrow.
;	   (with-slots (label) dialog-button
;	     (let ((old-label label))
;	       (unwind-protect
;		   (progn (setq label (button-label default))
;			  (redisplay-button dialog-button))
;		 (setq label old-label))))
	  
	  ;;  Avoid error on abbreviated buttons -- interior width ends up negative.
	  ;;  This way, we just highlight and don't even try to show the default.
	  (unless (< width (+ (ab-left-button-end-width dims)
			      (ab-right-button-end-width dims)))
	    
	    (using-gcontext
	      (gc
		:drawable   dialog-button
		:foreground ab-foreground
		:background ab-fill-color
		:font	    ab-font) 
	      (just-clear-body-of-button dialog-button gc))
	    
	    (using-gcontext
	      (gc
		:drawable    dialog-button
		:foreground  ab-fill-color
		:background  ab-foreground
		:font	     ab-font)
	      
	      (let ((default-label (button-label default)))
		(if (stringp default-label)
		    (display-constrained-text
		      dialog-button gc default-label ab-font
		      (label-width dialog-button label)
		      :x text-x :y text-y)
		    
		    (let*
		      ((label-width  (label-width dialog-button default-label))
		       (label-height (getf (pixmap-plist default-label) :height)))
		      (with-gcontext (gc :fill-style :tiled :tile default-label)
			(draw-rectangle
			  dialog-button gc
			  text-x (max 0 (pixel-round (- height label-height) 2))
			  label-width label-height t))))))))))))

(DEFUN display-constrained-text (contact gc text font available-width &key (x 0) (y 0))
  (LET* ((more-arrow (GETF *more-text-arrows-by-scale* (contact-scale contact)))
	 (more-arrow-image (more-text-arrow-image more-arrow))
	 (more-arrow-width (image-width more-arrow-image))
	)
    (FLET
      ((get-displayable-width-of-text (text font available-width)
	 ;;  Returns (<#-of-chars-in-text>) if entire text fits.
	 ;;  Returns (<#-of-displayable-chars> <npixels-displayable>) if not.
        (IF (<= (text-width font text) available-width)
	    (LENGTH text)
	  ;;  else we gotta figure out how many chars will fit.
	  ;;  Since text-width is a very expensive function we're going to try to get an estimate
	  ;;  for where in the text we get too wide to fit before we start calling it.
	  
	  (DO* ((reduced-space-for-text (- available-width more-arrow-width))
		(est-displayable-length (FLOOR reduced-space-for-text (max-char-width font)))
		(i (1+ est-displayable-length) (1+ i))
		(test-width)
		(last-test-width (text-width font text :end est-displayable-length) test-width)
		)
	       ((>= i (LENGTH text)))
	    (SETF test-width (text-width font text :end i))
	    (WHEN (> test-width reduced-space-for-text)
	      (RETURN  (VALUES (1- i) last-test-width))))))
       )

    ;;  Get the # of characters that fit (and their width if truncating)...
    (MULTIPLE-VALUE-BIND (displayable-length-of-text displayable-width-of-text)
	(get-displayable-width-of-text text font available-width)
      
      ;;  Draw the characters that we can...
      (draw-glyphs contact gc x y text :end displayable-length-of-text)
      
      ;;  If the entire label would not fit, place a More Text Arrow to the right of it...
      ;;  We assume here that the pixmap for this scale's More Text Arrow has already been
      ;;     cached so contact-mask can pick it up...
      (WHEN displayable-width-of-text
	(LET* ((more-arrow-x (+ x displayable-width-of-text
				(more-text-arrow-offset-from-text more-arrow)))
	       (more-arrow-y (+ y (more-text-arrow-offset-from-baseline more-arrow)))
	       (more-arrow-pixmap (contact-image-mask contact more-arrow-image :depth 1)))
	  (with-gcontext (gc :clip-x more-arrow-x
			     :clip-y more-arrow-y
			     :clip-mask more-arrow-pixmap)
	    (draw-rectangle contact gc more-arrow-x more-arrow-y
			    more-arrow-width (image-height more-arrow-image) t))))))))

(DEFMETHOD release-select ((dialog-button dialog-button))
   (with-slots (dialog) dialog-button
     (if (typep dialog 'menu)
	 (release-select-choose-default dialog-button dialog)
	 (call-next-method))))

(DEFMETHOD release-select ((dialog-item dialog-item))
   (with-slots (dialog) dialog-item
     (if (typep dialog 'menu)
	 (release-select-choose-default dialog-item dialog)
	 (call-next-method))))

(defun release-select-choose-default (dialog-button dialog)
   (with-slots (last-displayed-as) (the dialog-button dialog-button)
     ;;  Do nothing unless highlighted/selected already...
     (WHEN (EQ last-displayed-as :highlighted)
       (let ((ultimate-default (find-ultimate-default (menu-choice dialog))))
	 (choice-item-press   ultimate-default)
	 (choice-item-release ultimate-default)
	 (choice-item-release dialog-button)))))

(defun find-ultimate-default (choice)
   (let ((default (or (choice-default choice)	; Could be NIL, but Open Look insists on a default.
		      (first (composite-children choice)))))
     (typecase default
       ((or dialog-button dialog-item)
	(let ((dialog (button-dialog default)))
	  (if (typep dialog 'menu)
	      (find-ultimate-default (menu-choice dialog))
	      default)))
       (otherwise
	default))))

;;;
;;;  Event translations for dialog-button/item and menus.
;;;
;;;  These implement a sort of state machine.  The components of the current state
;;;  are the state of dialog (:mapped or not), the type of the dialog (menus behave
;;;  differently than other dialogs), and the menu-state of the menu (nil,
;;;  press-drag-release, or click-move-click).  Mostly they use the type to decide
;;;  their sensitivity to the event, the state of the dialog to determine whether
;;;  this is the first time for this event (for example, startup should only happen
;;;  once), and the menu-state to differentiate between modes for grabbing purposes.
;;;
;;;  Dialog button translations.

(defevent dialog-button
	  (:button-press :button-3)
   dialog-button-do-startup)

(defun dialog-button-do-startup (button)
   (let ((dialog (button-dialog button)))
     (when (and (typep dialog 'menu)
		(not (mapped-p dialog))
		(choice-item-press button))
       ;;  Present-dialog on menu sets menu-state to nil.
       (present-dialog dialog :button :button-3 :state (with-event (state) state)))))


(defevent dialog-button
	  (:button-release :button-3)
   dialog-button-button-3-release)

(defun dialog-button-button-3-release (button)
  (let ((dialog (button-dialog button)))
    (when (and (typep dialog 'menu)
	       (mapped-p dialog)
	       (null (menu-state dialog)))
      ;;  Menu just brought up by preceding press, go into click-move-click mode.
      (display-action-button-busy button)
      (grab-pointer dialog #.(make-event-mask :button-press :button-release :enter-window)
		    :owner-p t
		    :cursor (contact-glyph-cursor dialog *menu-cursor-index*))
      (setf (menu-state dialog) 'click-move-click))))


(defevent dialog-button
	  :leave-notify
   dialog-button-leave-notify)

(defun dialog-button-leave-notify (button)
   (declare (type dialog-button button))
   (let ((dialog (button-dialog button)))
     (if (and (typep dialog 'menu)
	      (mapped-p dialog)
	      (null (menu-state dialog)))
	 (with-event (time mode kind root-x root-y)
	   (when (eq mode :normal)
	     ;; We ungrab the pointer independent of its current location since
	     ;; the menu will be responsible for fielding any release event.
	     (ungrab-pointer (contact-display button) :time time)
	     
	     (multiple-value-bind (dialog-x dialog-y)
		 (contact-translate (contact-root button) root-x root-y dialog)
	       (if (inside-contact-p dialog dialog-x dialog-y) ; Avoid server round-trip.
		   (grab-pointer dialog #.(make-event-mask :button-release :enter-window :leave-window)
				 :owner-p t
				 :cursor (contact-glyph-cursor dialog *menu-cursor-index*))
		   (grab-pointer dialog #.(make-event-mask :button-release :enter-window)
				 :cursor (contact-glyph-cursor dialog *menu-cursor-index*))))
	     (setf (menu-state dialog) 'press-drag-release)))
	 
	 (with-slots (last-displayed-as) button
	   ;;  Do nothing unless highlighted/selected already...
	   (WHEN (EQ last-displayed-as :highlighted)
	     (leave button))))))


;;;
;;;  Menu translations.

(defevent menu
	  :button-press
   dialog-button-button-press)

(defun dialog-button-button-press (menu)
   (setf (menu-button-press-p menu) t))


(defevent menu
	  :button-release
   dialog-button-dismiss-menu-group)

(defun dialog-button-dismiss-menu-group (menu)
   (cond ((null (menu-state menu))
	  (setf (menu-state menu) 'click-move-click))
	 ((or (menu-button-press-p menu)
	      (eq (menu-state menu) 'press-drag-release))
	  (dismiss-menu menu)))
   (setf (menu-button-press-p menu) nil))


(defevent menu
	  :enter-notify
   dialog-button-menu-enter-notify)

(defun dialog-button-menu-enter-notify (menu)
   (with-event (time mode state)
     (flet ((pdr-enter ()
	      ;; First we ungrab the pointer so choice items will get proper
	      ;; event notifications
	      (ungrab-pointer (contact-display menu) :time time)
	      (grab-pointer menu #.(make-event-mask :button-release :enter-window :leave-window)
			    :owner-p t
			    :cursor (contact-glyph-cursor menu *menu-cursor-index*)))
	    (cmc-enter ()
	      (ungrab-pointer (contact-display menu) :time time)
	      (grab-pointer menu #.(make-event-mask :button-press :button-release)
			    :owner-p t
			    :cursor (contact-glyph-cursor menu *menu-cursor-index*))))
       (ecase (menu-state menu)
	 ((nil)
	  ;;  Pop-up menu, a la SCIFI.  Choose mode based on button state.
	  ;;  The test below will be T if button-3 is down, meaning we've entered
	  ;;  the menu with the button pressed, hence press-drag-release mode.  If
	  ;;  the button is up, we go to click-move-click.
	  (cond ((logtest #.(make-state-mask :button-3) state)
		 (setf (menu-state menu) 'press-drag-release)
		 (pdr-enter))
		(:else
		 (setf (menu-state menu) 'click-move-click)
		 (cmc-enter))))
	 (press-drag-release
	  (when (eq mode :normal)
	    (pdr-enter)))
	 (click-move-click
	  (when (eq mode :normal)
	    (cmc-enter)))
	 (finishing
;	  (when (eq mode :normal)
;	    (setf (menu-state menu) 'click-move-click)
;	    (cmc-enter))
	  )
	 (exiting-to-left
	  ;;  May happen if we leave a dialog-item before the menu's up
	  ;;  and have to take it down again.
	  nil)))))


(defevent menu
	  :leave-notify
   dialog-button-menu-leave-notify)

(defun dialog-button-menu-leave-notify (menu)
   (with-event (time mode x y)
     (when (eq mode :normal)
       (ecase (menu-state menu)
	 (press-drag-release
	  (ungrab-pointer (contact-display menu) :time time)
	  (let ((button (button-owning-dialog menu)))
	    (cond ((and (typep button 'dialog-item)
			(< x 0))		; A crude leave-left-edge test for items.
		   (setf (menu-state menu) 'exiting-to-left)	; Flag for dismiss-menu-group.
		   (setf (contact-state menu) :withdrawn)
;;  +++ I want to do choice-item-leave if the new position isn't within the button.
;;      The event coordinates are relative to the menu, though, so how exactly do
;;      I translate them?  In the meantime, it seems to be better to leave always.
		   (choice-item-leave button)
		   )
		  (:else
		   (grab-pointer menu #.(make-event-mask :button-release :enter-window)
				 :cursor (contact-glyph-cursor menu *menu-cursor-index*))))))
	 (click-move-click
	  (ungrab-pointer (contact-display menu) :time time)
	  (grab-pointer menu #.(make-event-mask :button-press :button-release :enter-window)
			:cursor (contact-glyph-cursor menu *menu-cursor-index*)))
	 (exiting-to-left
	  ;;  Need this because there'll be another leave-notify during the unmapping.
	  nil)
	 (finishing
	  nil)))))


;;;
;;;  Dialog item translations.

(defevent dialog-item
	  (:button-press :button-3)
   choice-item-press)


(defevent dialog-item
	  (:button-release :button-3)
   dialog-item-start-cmc-mode)

(defun dialog-item-start-cmc-mode (item)
   (let ((dialog (button-dialog item)))
     (when (not (mapped-p dialog))
       (cond ((typep dialog 'menu)
	      ;;  If there are any dialogs up at this level, take them down.
	      (mapc #'dismiss-active-dialogs
		    (composite-children (contact-parent item)))
	      ;;  Dialog-item, superior menu in stay-up mode, we fire on the release
	      ;;  and bring up the submenu in stay-up mode.
	      (present-dialog dialog :button :button-3 :state 0)
	      ;;  This is dialog-button-button-3-release without the grab-pointer.
	      (display-action-button-busy item)
	      (setf (menu-state dialog) 'click-move-click))
	     (:else
	      (choice-item-release item))))))


(defevent dialog-item
	  :leave-notify
   leave-dialog-item)

(defun leave-dialog-item (item)
  (declare (type dialog-item item))
  (with-event (state mode)
    (cond ((and (logtest #.(make-state-mask :button-3) state)
		(not (mapped-p (button-dialog item))))
	   ;;  We set last-x to the right-hand end of the item to force recalculation
	   ;;  when we re-enter.
	   (with-slots (last-x width) (the dialog-item item)
	     (setq last-x width))
	   ;; We ungrab the pointer independent of its current location since
	   ;; the menu will be responsible for fielding any release event.
	   (with-event (time mode)
	     (with-slots (last-displayed-as) item
	       (when (and (eq mode :normal)
			  (eq last-displayed-as :highlighted))
		 (ungrab-pointer (contact-display item) :time time)
		 (choice-item-leave item)))))
	  (:else
	   (with-slots (last-displayed-as) item
	     ;;  Do nothing unless highlighted/selected already...
	     (when (eq last-displayed-as :highlighted)
	       (leave item)))))))


(defevent dialog-item
	  :enter-notify
   dialog-item-enter-notify)

(defmethod dialog-item-enter-notify ((item dialog-item))
   (with-slots (dialog last-x active-x width last-displayed-as) item
     (when (and (not (mapped-p dialog)) (eq last-displayed-as :unhighlighted))
       (with-event (x y state)
	 (if (and (inside-contact-p item x y)	; +++ Inactive items don't get enter-notify, remove this?
		  (logtest #.(make-state-mask :button-3) state)
		  (or (not (typep dialog 'menu))
		      (not (menu-present-in-progress (contact-parent item))))	; Don't allow multiple PDR menus.
		  ;;  The pointer has been dragged over this button w/menu button
		  ;;  pressed. This has the same side effects as pressing the
		  ;;  select button so we go ahead and use the press procedure
		  ;;  to take care of visuals and approve the transition.
		  (choice-item-press item))
	     ;;  Transition was approved and button is now highlighted.
	     ;;  The choice-item-press is enough for non-menus, but menus have more:
	     (when (typep dialog 'menu)
	       (when (null active-x)
		 (let ((dims (getf *button-dimensions-by-scale* (contact-scale item))))
		   (setq active-x (- width
				     (ab-right-button-end-width dims)
				     (image-width (ab-horizontal-menu-mark-image dims))))))
	       (setq last-x x)
	       (when (>= x active-x)
		 ;;  Entered in the "submenu region," which is that area from the
		 ;;  left edge of the menu mark to the right edge of the item.
		 ;;  If there are any dialogs up at this level, take them down.
		 (mapc #'dismiss-active-dialogs
		       (composite-children (contact-parent item)))
		 ;;  Bring up the menu and go into the submenu protocol.
		 (present-dialog dialog :button :button-3 :state state)
		 (setf (menu-present-in-progress (contact-parent item)) t)
;		 (setf (menu-state dialog) 'press-drag-release)
		 ))
	     ;;  Transition not approved, so inhibit the drag-right check on :motion-notify.
	     (when (typep dialog 'menu)
	       (setq last-x width)))))))


(defevent dialog-item
	  :motion-notify
   dialog-item-drag-right)

(defmethod dialog-item-drag-right ((item dialog-item))
   (with-slots (dialog last-x active-x width) item
     (when (and (typep dialog 'menu)
		(not (mapped-p dialog))
		(not (menu-present-in-progress (contact-parent item)))
		active-x)			; Paranoia check.
       (with-event (x y state)
	 (when (and (inside-contact-p item x y)
		    (logtest #.(make-state-mask :button-3) state))
	   (cond ((or (>= x active-x)
		      (> (- x last-x) *menu-item-drag-right-distance*))
		  ;;  If there are any dialogs up at this level, take them down.
		  (mapc #'dismiss-active-dialogs
			(composite-children (contact-parent item)))
		  ;;  Dragged right far enough, or into active area, bring up menu.
		  (present-dialog dialog :button :button-3 :state state)
		  (setq last-x width)		; Force recalculation on later entries.
		  (setf (menu-state dialog) 'press-drag-release))
		 ((< x last-x)			; Moving left, save leftmost.
		  (setq last-x x))
		 (:else				; Moving right, keep old left.
		  nil)))))))


;;;
;;;  Display code.  Dialog-buttons and dialog-items show a menu mark or
;;;  window mark to the right of the item.  These functions and methods
;;;  allow space for it and do the drawing.

(defvar *inside-display-window-mark* nil)	; Don't do it inside internal routine.

;; Daemons on the Dialog Button's label manipulation methods to adjust the width
;; of the label for the menu mark and the display the menu mark.
(defmethod label-width :around ((button dialog-button) label)
  (if *inside-display-window-mark*
      (call-next-method)
      (with-slots (dialog) button
	(let ((dims (getf *button-dimensions-by-scale* (contact-scale button))))
	  (+ (call-next-method)
	     (additional-label-width dialog button dims)
	     (- (ab-right-button-end-width dims)
		2))))))				; Right border thickness

(defmethod label-width :around ((button dialog-item) label)
  (if *inside-display-window-mark*
      (call-next-method)
      (with-slots (dialog) button
	(let ((dims (getf *button-dimensions-by-scale* (contact-scale button))))
	  (+ (call-next-method)
	     (additional-label-width dialog button dims)
	     (- (ab-right-button-end-width dims)
		2))))))				; Right border thickness


(defmethod additional-label-width ((dialog null) button dims)
   (declare (ignore button dims))
   0)

(defmethod additional-label-width ((dialog menu) (button dialog-button) dims)
   (image-width (ab-vertical-menu-mark-image dims)))

(defmethod additional-label-width ((dialog menu) (button dialog-item) dims)
   (image-width (ab-horizontal-menu-mark-image dims)))

(defmethod additional-label-width ((dialog command) button dims)
   (declare (ignore dims))
   (text-extents (button-font button) "..."))

(defmethod additional-label-width ((dialog confirm) button dims)
   (declare (ignore dims))
   (text-extents (button-font button) "..."))

(defmethod additional-label-width ((dialog property-sheet) button dims)
   (declare (ignore dims))
   (text-extents (button-font button) "..."))


(DEFMETHOD display-button-label :after ((button dialog-button) gc)
  ;;  Now draw in the menu-mark at the right end of the button, just to the left of the
  ;;  right-button-end (which leaves right-margin pixels to the right of the mark)
  (with-slots (dialog) button
    (after-display-button-label dialog button gc)))

(DEFMETHOD display-button-label :after ((item dialog-item) gc)
  ;;  Now draw in the menu-mark at the right end of the button, just to the left of the
  ;;  right-button-end (which leaves right-margin pixels to the right of the mark)
  (with-slots (dialog) item
    (after-display-button-label dialog item gc)))

(defmethod after-display-button-label ((dialog null) button gc)
   (declare (ignore button gc))
   nil)

(defmethod after-display-button-label ((dialog menu) (button dialog-button) gc)
   (display-menu-mark button gc :below))

(defmethod after-display-button-label ((dialog menu) (item dialog-item) gc)
   (display-menu-mark item gc :to-the-right))

(defun display-menu-mark (button gc direction)
   (let ((width (contact-width button)))
     (LET* ((scale	      (contact-scale button))
	    (dims	      (getf *button-dimensions-by-scale* scale))
	    (button-pixmaps   (get-button-pixmaps button))
	    (menu-mark-image  (ecase direction
				(:to-the-right
				 (ab-horizontal-menu-mark-image dims))
				(:below
				 (ab-vertical-menu-mark-image dims))))
	    (menu-mark-pixmap (ecase direction
				(:to-the-right
				 (horizontal-menu-mark-pixmap button-pixmaps))
				(:below
				 (vertical-menu-mark-pixmap button-pixmaps))))
	    (menu-mark-x      (- width
				 (ecase direction
				   (:below
				    (ab-right-button-end-width dims))
				   (:to-the-right
				    (ai-button-end-width dims)))
				 (image-width menu-mark-image)))
	    (menu-mark-y      (- (ecase direction
				   (:below	  (ab-text-baseline dims))
				   (:to-the-right (1- (ai-text-baseline dims))))
				 (image-height menu-mark-image)
				 ;;  The 1- for :to-the-right is correction to this.
				 (ab-menu-mark-bottom-rel-to-baseline dims))))
       (with-gcontext (gc :clip-x menu-mark-x
			  :clip-y menu-mark-y
			  :clip-mask menu-mark-pixmap)
	 (draw-rectangle button gc
			 menu-mark-x menu-mark-y
			 (image-width menu-mark-image) (image-height menu-mark-image)
			 t)))))


(defmethod after-display-button-label ((dialog command) button gc)
   (display-window-mark button gc))

(defmethod after-display-button-label ((dialog property-sheet) button gc)
   (display-window-mark button gc))

(defmethod after-display-button-label ((dialog confirm) button gc)
   (display-window-mark button gc))

;;  Draw the window mark flush against the right end of the label, using
;;  similar computations to those from display-button-label.
(defmethod display-window-mark ((button dialog-button) gc)
   (with-slots (font label-alignment label width) button
     (let* ((scale (contact-scale button))
	    (dims (GETF *button-dimensions-by-scale* scale))
	    (label-width (let ((*inside-display-window-mark* t))
			   (label-width button label)))
	    (margin (ab-left-button-end-width dims))
	    (left-margin (max margin
			      (case label-alignment
				(:left   0)
				(:center (pixel-round (- width label-width) 2))
				(:right  (- width margin label-width)))))
	    (window-mark-x (+ left-margin label-width 1))	; Extra pixel looks better.
	    (window-mark-y (1+ (ab-text-baseline dims))))
       (with-gcontext (gc :font font)
	 (draw-glyphs button gc window-mark-x window-mark-y "...")))))

(defmethod display-window-mark ((item dialog-item) gc)
   (with-slots (font label-alignment label width) item
     (let* ((scale (contact-scale item))
	    (dims (GETF *button-dimensions-by-scale* scale))
	    (label-width (let ((*inside-display-window-mark* t))
			   (label-width item label)))
	    (margin (ai-button-end-width dims))
	    (left-margin (max margin
			      (case label-alignment
				(:left   0)
				(:center (pixel-round (- width label-width) 2))
				(:right  (- width margin label-width)))))
	    (window-mark-x (+ left-margin label-width 1))	; Extra pixel looks better.
	    (window-mark-y (ai-text-baseline dims)))
       (with-gcontext (gc :font font)
	 (draw-glyphs item gc window-mark-x window-mark-y "...")))))

;;;
;;;  Position the menu according to the Open Look rules:
;;;  For a button, centered horizontally with the top edge against the bottom
;;;  edge of the button.  For an item, with the default item centered vertically
;;;  relative to the item itself.  In press-drag-release mode (release-p NIL),
;;;  positioned horizontally so the left end of the default item is over the
;;;  mouse;  in click-move-click mode (release-p T), positioned horizontally so
;;;  the left edge of the menu is a pixel away from right edge of the item.
;;;
;;;  For pop-ups (not yet implemented), the button will be NIL.  In that case,
;;;  we align the default item vertically with the mouse, and place the menu so
;;;  that the mouse is a pixel or two to the left of the left edge of the default.

(DEFMETHOD set-menu-position ((self dialog-button) menu &optional release-p)
  (declare (ignore release-p))
  (with-slots (width height x y border-width parent) self
    (unless (realized-p menu)
      (initialize-geometry menu))
    
    (let ((menu-width (contact-width (contact-parent (menu-choice menu)))))
	;; We use the width of the *container* so menu will be
	;; centered without considering the drop shadow.
      (multiple-value-bind (menu-x menu-y)
	  (contact-translate      
	    (contact-parent self)
	    (- (+ x (round width 2)) (round menu-width 2))
	    (+ y border-width border-width height 1)
	    (contact-parent menu))
	(SETF menu-x (MIN (MAX 0 menu-x)
			  (- (contact-width (contact-parent menu)) menu-width)))
	(change-geometry menu
			 :x menu-x
			 :y menu-y
			 :accept-p t)))))

;;  For a dialog-item, the menu comes up to the right, with the default item aligned with
;;  the item, center to center.  In pdr mode, the X coordinate is such that the left end of the
;;  default item is under the pointer;  in cmc mode, the left edge of the menu is one pixel
;;  to the right of the item.
(DEFMETHOD set-menu-position ((self dialog-item) menu &optional release-p)
  (initialize-geometry menu)			; Needed to get correct sizes for Y coord.
  (with-slots (width height x y border-width parent) self
    (let* ((choice (menu-choice menu))
	   (default (or (choice-default choice)	; Could be NIL, but Open Look insists on a default.
			(first (composite-children choice))))
	   (default-scale (contact-scale default))
	   (dims (GETF *button-dimensions-by-scale* default-scale))
	   (container (contact-parent choice))
	   (menu-width (contact-width container))
	   (menu-height (contact-height container)))
      ;; We use the width of the *container* so menu will be
      ;; centered without considering the drop shadow.
      (multiple-value-bind (default-x default-y)
	  ;;  Translate default-item position into offset from menu 0,0.
	  (contact-translate (contact-parent default)
			     (contact-x default)
			     (contact-y default)
			     menu)
	(multiple-value-bind (menu-x menu-y)
	    (contact-translate (contact-parent self)
			       (if release-p
				   (+ x width border-width border-width 2)	; I think that's 1 + 1 for default-ring.
				   (- (+ (pointer-position self)	; Should be the pointer X.
					 x)
				      (ab-left-button-end-width dims)
				      default-x))
			       (- (+ y (round height 2))	; Align the centers in Y.
				  (+ default-y (round (contact-height default) 2)))
			       (contact-parent menu))
	  (setq menu-x (MIN (MAX 0 menu-x)
			    (- (contact-width (contact-parent menu)) menu-width))
		menu-y (MIN (MAX 0 menu-y)
			    (- (contact-height (contact-parent menu)) menu-height)))
	  (change-geometry menu
			   :x menu-x
			   :y menu-y
			   :accept-p t))))))

;;  For a pop-up menu, there is no item.  Bring it up under the mouse, with the default
;;  item centered vertically and its left edge a couple of pixels to the right of the mouse.
(defmethod set-menu-position ((self null) menu &optional release-p)
   (declare (ignore release-p))
   (initialize-geometry menu)			; Needed to get correct sizes for Y coord.
   (let* ((choice (menu-choice menu))
	  (default (or (choice-default choice)	; Could be NIL, but Open Look insists on a default.
		       (first (composite-children choice))))
	  (container (contact-parent choice))
	  (menu-width  (contact-width container))
	  (menu-height (contact-height container)))
     ;; We use the width of the *container* so menu will be
     ;; centered without considering the drop shadow.
     (multiple-value-bind (pointer-x pointer-y)
	 (pointer-position (contact-parent menu))
       (multiple-value-bind (default-x default-y)
	   ;;  Translate default-item position into offset from menu 0,0.
	   (contact-translate (contact-parent default)
			      (contact-x default)
			      (contact-y default)
			      menu)
	 (let ((menu-x (- pointer-x (- default-x 2)))
	       (menu-y (- pointer-y default-y (round (contact-height default) 2))))
	   (setq menu-x (MIN (MAX 0 menu-x)
			     (- (contact-width (contact-parent menu)) menu-width))
		 menu-y (MIN (MAX 0 menu-y)
			     (- (contact-height (contact-parent menu)) menu-height)))
	   (change-geometry menu
			    :x menu-x
			    :y menu-y
			    :accept-p t))))))
