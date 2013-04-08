;;; -*- Mode:Lisp; Package:CLUEI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                   P.O. BOX 149149                                |
;;;                                AUSTIN, TEXAS 78714-9149                          |
;;;                                                                                  |
;;;                Copyright (C)1989,1990 Texas Instruments Incorporated.            |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

(in-package "CLUEI")


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                  Geometry management methods for ROOT contacts             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod add-child ((parent root) (child shell) &key)
  ;; No problem...
  (with-slots (children) parent
    (setf children (nconc children (cons child nil)))))

(defmethod add-child ((parent root) non-shell &key)
  ;; Big problem...
  (error "~a cannot be top-level because it is not a shell." non-shell))

(defmethod manage-geometry ((parent root) (shell wm-shell) x y width height border-width &key) 
  (declare (type contact          shell)
	   (type (or null int16)  x y)
	   (type (or null card16) width height border-width))
  
  (with-slots ((contact-x x)
	       (contact-y y)
	       (contact-width width)
	       (contact-height height)
	       (contact-border-width border-width)) shell
    
    
    ;; Reconfigure top-level window, if necessary
    (let (changed-p (display (contact-display parent)))
      
      ;; Wait for any previous :configure-notify events to be handled.
      ;; Don't update-state to avoid infinite recursion during realization.
      (process-all-events display nil)
      
      (with-state (shell)
	(when (and x (/= x contact-x)
		   (setf (drawable-x shell) x))
	  (setf changed-p t))
	(when (and y (/= y contact-y)
		   (setf (drawable-y shell) y))
	  (setf changed-p t))
	(when (and width (/= width contact-width)
		   (setf (drawable-width shell) width))
	  (setf changed-p t))
	(when (and height (/= height contact-height)
		   (setf (drawable-height shell) height))
	  (setf changed-p t))
	(when (and border-width (/= border-width contact-border-width)
		   (setf (drawable-border-width shell) border-width))
	  (setf changed-p t)))
      
      ;; Return approved geometry
      (values
	(or
	  ;; Null changed approved immediately
	  (not changed-p)
	  
	  ;; Actual change approved if it is not modified by window mgr.
	  (progn	    
	    ;; Wait for :configure-notify to report actual new window geometry.
	    ;; wm-shell's handle-event will update geometry slots in
	    ;; response to :configure-notify.	    
	    (with-event-mode (shell '(:configure-notify (throw-action :configure-notify)))
	      (catch :configure-notify
		;; Don't update-state to avoid infinite recursion during realization.
		(loop (process-next-event display nil nil))))
	    
	    ;; Assert: shell slots now contain actual (wm-approved) geometry
	    ;; Return approval of original geometry request
	    (and
	      (not (and x (/= x contact-x)))
	      (not (and y (/= y contact-y)))
	      (not (and width (/= width contact-width)))
	      (not (and height (/= height contact-height)))
	      (not (and border-width (/= border-width contact-border-width))))))
	
	contact-x contact-y contact-width contact-height contact-border-width))))


(defmethod manage-geometry :around ((parent root) (shell override-shell) x y width height border-width &key) 
  (declare (type contact shell)
	   (type (or null int16) x y)
	   (type (or null card16) width height border-width))

  ;; Approve and perform change immediately. We could let change-geometry do the
  ;; move/resize normally, but doing them here allows change-geometry to assume
  ;; consistently that the change to a realized shell has already been done. See
  ;; (manage-geometry root wm-shell) method.

  ;; This must be an :around method in order to override less specific :around methods.
  
  (with-slots
    ((contact-x            x)
     (contact-y            y)
     (contact-width        width)
     (contact-height       height)
     (contact-border-width border-width))
    (the contact shell)
    
    (let
      ((approved-x            (or x contact-x))
       (approved-y            (or y contact-y))
       (approved-width        (or width contact-width))
       (approved-height       (or height contact-height))
       (approved-border-width (or border-width contact-border-width)))
      
      (with-state (shell)
	(move shell approved-x approved-y)
	(resize shell approved-width approved-height approved-border-width))

      (values t approved-x approved-y approved-width approved-height approved-border-width))))


(defmethod manage-priority ((parent root) (shell wm-shell) priority sibling &key)  
  (declare (type (member :above :below :top-if :bottom-if :opposite) priority)
	   (type (or null contact) sibling))
  
  ;; Shrug..just return approval, because ICCCM doesn't require the window manager
  ;; to report the result of an attempt to change priority. Besides, even if we did get
  ;; a :configure-notify saying what really happened, we can't always tell
  ;; if the request was successful or not. That's because only an above-sibling comes back in the
  ;; :configure-notify, and we may not own this window or know its place in the hierarchy.

  (values t priority sibling))




(defmethod change-layout ((parent root) &optional newly-managed)
  (declare (ignore newly-managed)) 
  ;; Adding/deleting root children has no effect on sibling layout
  )
