;;; -*- mode:lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;;
;;; Change history:
;;;
;;;  Date	Author	Description
;;; -------------------------------------------------------------------------------------
;;; 10/14/87	LGO	Created

(in-package "CLUEI")

(export '( virtual
	  virtual-composite
	  ))

;; These are methods that duplicate the xlib draw-rectangle, draw-rectangle and clear-area
;; functions.  I'm not happy with the names, please mail suggestions to clue-review@dsg.csc.ti.com
(export '(rectangle glyphs clear))

(defcontact virtual (basic-contact)
  ()
  (:documentation "A contact without a window")
  )

(defmethod (setf contact-state) (state (contact virtual))
  (check-type state (member :withdrawn :managed :mapped))
  (let ((old-state (slot-value (the contact contact) 'state)))
    (unless (eq old-state state)
      (setf (slot-value (the contact contact) 'state) state))))

(defmethod realize ((contact virtual))
  ;; Create the Window for CONTACT.
  ;; This is a method to allow contacts to specialize the options.
  ;; Applications should call PRESENT
  ;;
  ;; Ensure the parent is realized
  (with-slots (parent) contact
    (unless (realized-p parent)
      (realize parent)))
  ;; Use the PARENT's window
  (setf (window-id contact) (window-id (contact-parent contact))))

(defmethod destroy ((contact virtual))
  ;; Destroy a contact and all its resources
  (when (and (realized-p contact) 	 ;; only destroy realized windows once
	     (contact-parent contact))   ;; Don't destroy screen
    (setf (contact-state contact) :withdrawn)
    (setf (window-id contact) -1)))

(defmethod accept-focus-p ((contact virtual))
  "Returns non-nil when CONTACT is willing to become the keyboard input focus"
  nil)

(defmethod move ((contact virtual) x y)
  (with-slots ((contact-x x) (contact-y y)) contact
    (setf contact-x x)
    (setf contact-y y)))
  
(defmethod resize ((contact virtual) width height border-width)
  (with-slots ((contact-width width)
	       (contact-height height)
	       (contact-border-width border-width)) contact
    (setf contact-width width)
    (setf contact-height height)
    (setf contact-border-width border-width)))

(defmethod inside-contact-p ((contact virtual) x y)
  ;; Returns T when x/y (relative to parent) is inside or on contact"
  (with-slots ((contact-x x)
	       (contact-y y)
	       (contact-width width)
	       (contact-height height)) contact
    (and (< 0 (- x contact-x) contact-width)
	 (< 0 (- y contact-y) contact-height))))

(defmethod rectangle ((contact virtual) gcontext x y width height &optional fill-p)
  (with-slots ((contact-x x)
	       (contact-y y)) contact
    (draw-rectangle contact gcontext (+ x contact-x) (+ y contact-y) width height fill-p)))

(defmethod glyphs ((contact virtual) gcontext x y sequence &rest options)
  (with-slots ((contact-x x) (contact-y y)) contact
    (apply #'draw-glyphs contact gcontext (+ x contact-x) (+ y contact-y) sequence options)))

(defmethod clear ((contact virtual) &key (x 0) (y 0) width height exposures-p)
  (with-slots ((contact-x x)
	       (contact-y y)
	       (contact-width width)
	       (contact-height height)) contact
    (clear-area contact :x (+ x contact-x) :y (+ y contact-y)
		:width (or width contact-width) :height (or height contact-height)
		:exposures-p exposures-p)))

;;-----------------------------------------------------------------------------

(defcontact virtual-composite (composite)
  ((mouse-contact :type (or null virtual) :accessor mouse-contact) ;; Set to the virtual window the mouse is in
   )
  (:documentation "A composite contact that may have virtual children")
  )

(defmethod realize :before ((contact virtual-composite))
  (with-slots ((composite-event-mask event-mask)) contact
    (let ((event-mask 0))
      ;; Combine the event masks for the virtual children
      (dolist (child (composite-children contact))
	(when (typep child 'virtual)
	  (setq event-mask (logior event-mask (contact-event-mask child)))))
      ;; Select pointer-motion when enter/leave window is needed
      (when (plusp (logand event-mask #.(make-event-mask :enter-window :leave-window)))
	(setq event-mask (logior event-mask #.(make-event-mask :pointer-motion))))
      ;; Combine virtual event mask with the composite's
      (setf composite-event-mask (logior event-mask composite-event-mask)))))

(defmethod handle-event ((contact virtual-composite) (event event))
  ;; Do event/callback translation based on the event-translations slot  
  (labels ((event-child (event)
	     (let ((x (slot-value (the event event) 'x))
		   (y (slot-value (the event event) 'y)))
	       (dolist (child (composite-children contact))
		 (when (and (typep child 'virtual)
			    (inside-contact-p child x y))
		   (return child))))))
    
    (block exit
      (let ((event-key (slot-value (the event event) 'key))
	    (event-sequence (slot-value (the event event) 'sequence)))
	;; Handle universal events
	(case event-key
	  ;; Forward events to virtual children
	  ((:key-press :key-release :button-press :button-release)
	   (let ((child (event-child event)))
	     (with-slots ((child-x x) (child-y y)
			  (child-event-mask event-mask)) (the virtual child)
	       (with-slots ((event-x x) (event-y y)
			   #+nil (event-key key)) (the event event)
		 (when (and child
			    (plusp
			      (logand child-event-mask
				      (ecase event-key
					(:key-press #.(make-event-mask :key-press))
					(:key-release #.(make-event-mask :key-release))
					(:button-press #.(make-event-mask :button-press))
					(:button-release #.(make-event-mask :button-release))))))
		   ;; Make event relative to child
		   (setf event-x (- event-x child-x)
			 event-y (- event-y child-y))
		   (cluei::dispatch-event event event-key t event-sequence child)
		   (return-from exit nil))))))
	  
	  ;; fabricate mouse enter/leave for virtual children
	  (:motion-notify
	   (let ((child (event-child event)))
	     (when child
	       (let ((mouse-contact (mouse-contact contact))
		     (handled-p nil)
		     (x (slot-value (the event event) 'x))
		     (y (slot-value (the event event) 'x)))
		 (with-slots ((child-x x) (child-y y)
			      (child-event-mask event-mask)) (the virtual child)
		   (with-slots ((mouse-x x) (mouse-y y)
				(mouse-event-mask event-mask)) (the virtual mouse-contact)
		     (with-slots ((event-x x) (event-y y)
				  (event-key key)) (the event event)
		       (when (and mouse-contact (not (eq mouse-contact child))
				  (plusp (logand #.(make-event-mask :leave-window)
						 mouse-event-mask)))
			 ;; Make event relative to child
			 (setf event-x (- x mouse-x)
			       event-y (- y mouse-y))
			 (cluei::dispatch-event event :leave-notify t event-sequence mouse-contact)
			 (setq handled-p t))
		       (setf (mouse-contact contact) child)
		       (when (and (not (eq mouse-contact child))
				  (plusp (logand #.(make-event-mask :enter-window)
						 child-event-mask)))
			 (setf event-x (- x child-x)
			       event-y (- y child-y))
			 (cluei::dispatch-event event :enter-notify t event-sequence child)
			 (setq handled-p t))
		       (when (plusp (logand #.(make-event-mask
						:pointer-motion :pointer-motion-hint
 						:button-1-motion :button-2-motion :button-3-motion
						:button-4-motion :button-5-motion :button-motion)
					    child-event-mask))
			 (setf event-x (- x child-x)
			       event-y (- y child-y))
			 (cluei::dispatch-event event :motion-notify t event-sequence child)
			 (setq handled-p t))
		       (when handled-p (return-from exit nil))
		       (setf event-x x
			     event-y y))))))))

	  ;; When mouse leaves composite, fabricate leave-notify for virtual children
	  (:leave-notify
	   (let ((mouse-contact (mouse-contact contact)))
	     (with-slots ((mouse-x x) (mouse-y y)
			  (mouse-event-mask event-mask)) (the virtual mouse-contact)
	       (with-slots ((event-x x) (event-y y)
			    (event-key key)) (the event event)
		 (when (and mouse-contact
			    (plusp (logand #.(make-event-mask :leave-window)
					   mouse-event-mask)))
		   ;; Make event relative to child
		   (setf event-x (- event-x mouse-x)
			 event-y (- event-y mouse-y))
		   (cluei::dispatch-event event :leave-notify t event-sequence mouse-contact)
		   (setf (mouse-contact contact) nil)
		   (return-from exit nil))))))

	  (:exposure
	   (with-slots ((event-x x) (event-y y)
			(event-height height)
			(event-width width)) (the event event)
	     (let ((x event-x)
		   (y event-y))
	       (display contact x y event-width event-height)
	       (dolist (child (composite-children contact))
		 (when (typep child 'virtual)
		   (with-slots ((child-x x) (child-y y))
			       (the virtual child)
		     (setf event-x (- x child-x)
			   event-y (- y child-y)))
		   (cluei::dispatch-event event :exposure t event-sequence child)))
	       (setf event-x x
		     event-y y)))))

	(call-next-method)
	))))
