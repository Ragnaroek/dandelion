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

(export '(make-command
	  command
	  command-area
	  command-control-area
	  command-default-accept
	  command-default-cancel
	  dialog-default-control 
	  ))


(defconstant *default-accept-label*  "OK")
(defconstant *default-cancel-label*  "Cancel")

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  command                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(eval-when (compile load eval)

(defcontact command (core core-wm-shell transient-shell)
  ((previous-pointer-x
                    :type (or null int16)
		    :initform nil)
   (previous-pointer-y
                    :type (or null int16)
		    :initform nil)
   (control-default :type (or null contact)
		    :initform nil))
  (:resources
    (border-width    :initform 1)
    (command-area    :type (or function list) :initform nil)
    (control-area    :type (or function list) :initform nil)
    (default-accept  :type (or (member :on :off) string)
                     :initform :on)
    (default-cancel  :type (or (member :on :off) string)
                     :initform :on)
    (default-control :type symbol :initform nil))
  
  (:documentation "A dialog which presents a set of related values and a set of commands."))
)

(defmethod (setf contact-foreground) :after (new-value (self command))
  (setf (contact-foreground (car (composite-children self))) new-value))

(defun make-command (&rest initargs &key &allow-other-keys)
  "Creates and returns a command instance."
  (apply #'make-contact 'command initargs))


(defmethod command-area ((self  command))
  "Returns the command area of the COMMAND."
  (with-slots (children) (first (slot-value self 'children))
    (find :area children :key #'contact-name)))


(defmethod command-control-area ((self  command))
  "Returns the command control of the COMMAND."
  (with-slots (children) (first (slot-value self 'children))
    (find :control children :key #'contact-name)))
  

(defmethod dialog-accept ((self command))
  "Invokes :accept callback function and pops down the dialogue"
  (if (callback-p self :accept)
      (apply-callback self :accept)
      (with-slots ((members children)) (command-area self)
	(dolist (member members)
	  (apply-callback member :accept))))
  (with-slots (pinned-p) self
    (unless pinned-p (setf (contact-state self) :withdrawn))))


(defmethod dialog-cancel ((self command))
  "Invokes :cancel callback function and pops down the dialogue."
  (with-slots (pinned-p) self
    (unless pinned-p (setf (contact-state self) :withdrawn)))
  (if (callback-p self :cancel)
      (apply-callback self :cancel)
      (with-slots ((members children)) (command-area self)
	(dolist (member members)
	  (apply-callback member :cancel)))))


(defmethod shell-mapped ((self command))
  "Invokes :initialize callback function."
  (with-slots (children) self
    (setf (display-text-source (find :footer (composite-children (first children)) :key 'contact-name))
	  "")
    (apply-callback self :map)
    (apply-callback-else (self :initialize)
      (with-slots ((members children)) (command-area self)
	(dolist (member members)
	  (apply-callback member :initialize))))))

(defmethod (setf contact-state) :after ((new-state (eql :mapped)) (self command))
  ;; Pointer warping must occur after :map-notify received, in case root-relative
  ;; positions have been changed by window manager redirection.
  

  (with-slots (previous-pointer-x previous-pointer-y display control-default) self
    (let ((control-def (or control-default
			       (first (composite-children (command-control-area self))))))
      ;; Ensure realized.
      (cond ((realized-p self)
	     ;; Store position for pointer unwarping later....
	     (multiple-value-setq
	       (previous-pointer-x previous-pointer-y) (pointer-position self))
	     
	     (warp-pointer
	       control-def
	       (pixel-round (contact-width control-def)  2)
	       (- (contact-height control-def) 2)))
	    ;; Ensure realized.
	    (t (update-state display))) 
      )))
 
(defmethod shell-unmapped :before ((self command))
  (with-slots (previous-pointer-x previous-pointer-y) self
    ;; Unwarp pointer to original position, if necessary.
    (when previous-pointer-x
      (warp-pointer self previous-pointer-x previous-pointer-y))))


(defmethod dialog-warn ((self command) message field)
  "Display a warning for verification error."
  (assert (or (null field) (typep field 'contact)) nil "~s is not a contact." field)
  (let* ((footer (find :footer (composite-children (car (composite-children self)))
				  :key #'contact-name))
	 (actual-message (or message "These values cannot be accepted."))
	 (tw  (text-width (display-text-font footer) actual-message)))
    (if (>= tw (contact-width footer))
	(confirm-p
	  :message     actual-message
	  :near        (or field (slot-value self 'control-default))
	  :parent      self
	  :accept-only :on)
	(setf (display-text-source footer) actual-message))))

(flet
  ((find-default-accept (command)		       
			(find :accept (composite-children (command-control-area command))
			      :key #'contact-name
			      :test #'eq))
   (find-default-cancel (command)
			(find :cancel (composite-children (command-control-area command))
			      :key #'contact-name
			      :test #'eq))
   (command-verify      (command)  
			(multiple-value-bind (verified-p message field)
			    (or (not (callback-p command :verify))
				(apply-callback command :verify))
			  (if verified-p
			      (dialog-accept command)	
			      (dialog-warn command message field)))))
  
  (defmethod command-default-accept ((self command))
    (let ((control (find-default-accept self)))
      (if (and control (eq (contact-state control) :mapped))
	:on
	:off)))
  
  (defmethod command-default-cancel ((self command))
    (let ((control (find-default-cancel self)))
      (if (and control (eq (contact-state control) :mapped))
	:on
	:off)))
  
  (defmethod (setf command-default-accept) (new-value (self command))
    (check-type new-value (or (member :on :off) string) "one of :ON, :OFF, or a string.")
    (let ((control (find-default-accept self)))
      (case new-value
	(:on
	 (if control
	     (setf (contact-state control) :mapped)
	     (add-callback (make-action-button
			     :parent (command-control-area self)
			     :name   :accept
			     :label  *default-accept-label*)
			   :release #'command-verify self)))
	(:off
	 (when control
	   (setf (contact-state control) :withdrawn)))
	
 	(otherwise
	 (cond
	   (control
	    (setf (button-label control) new-value)
	    (setf (contact-state control) :mapped))

	   (t
	    (add-callback (make-action-button
			    :parent (command-control-area self)
			    :name   :accept
			    :label  new-value)
			  :release #'command-verify self))))))
      new-value)

  (defmethod (setf command-default-cancel) (new-value (self command))
    (check-type new-value (or (member :on :off) string) "one of :ON, :OFF, or a string.")
    (let ((control (find-default-cancel self)))
      (case new-value
	(:on
	 (if control
	     (setf (contact-state control) :mapped)
	     (add-callback (make-action-button
			     :parent (command-control-area self)
			     :name   :cancel
			     :label  *default-cancel-label*)
			   :release #'dialog-cancel self)))
	(:off
	 (when control
	   (setf (contact-state control) :withdrawn)))
	
 	(otherwise
	 (cond
	   (control
	    (setf (button-label control) new-value)
	    (setf (contact-state control) :mapped))

	   (t
	    (add-callback (make-action-button
			    :parent (command-control-area self)
			    :name   :cancel
			    :label  new-value)
			  :release #'dialog-cancel self))))))
      new-value)

  (defmethod dialog-default-control ((self command))
    (with-slots (control-default) self
      (let ((default (or control-default
			 (first (composite-children (command-control-area self))))))
	(when default (contact-name default)))))


  (defmethod (setf dialog-default-control) (new-value (command command))
    (check-type new-value symbol)
    (with-slots (control-default) command
      (when control-default
	(setf (choice-item-highlight-default-p control-default) nil))
      (or (and (setf control-default
		 (find-if #'(lambda (c) (and (mapped-p c) (eq new-value (contact-name c))))
			  (composite-children (command-control-area command))))
	       (setf (choice-item-highlight-default-p control-default) t))
	  (setf control-default new-value))
      new-value)))




(defmethod initialize-instance :after ((self command)
				       &key
				       command-area control-area
				       default-accept default-cancel
				       default-control
				       &allow-other-keys)
  (multiple-value-bind (command-constructor command-area-initargs)
      (etypecase command-area
	(null
	 (let ((space (ab-height (getf *button-dimensions-by-scale* (contact-scale self)))))
	   (values 'make-table 
		   `(
		     :columns              2
		     :column-alignment     :right
		     :same-width-in-column :on
		     :same-height-in-row   :on
		     :horizontal-space     ,space
		     :vertical-space       ,space))))

	(function command-area)

	(list (values (first command-area) (rest command-area))))
      
    (multiple-value-bind (control-constructor control-area-initargs)
	(etypecase control-area
	  (null
	   (let ((space (point-pixels
			  (contact-screen self)
			  (getf *dialog-point-spacing* (contact-scale self)))))
	     (values 'make-table 
		     `(
		       :columns              :maximum
		       :column-alignment     :center
		       :same-height-in-row   :on
		       :horizontal-space     ,space
		       :vertical-space       ,space))))
	  
	  (function control-area)
	  
	  (list (values (first control-area) (rest control-area)))) 
      
      (with-slots (width height) self
	
	;; Create the sheet
	(let ((sheet (make-contact 'command-sheet :name :sheet
				   :parent self
				   :x 0 :y 0
				   :width width :height height
				   :border-width 0)))
	  
	  ;; Create the command area
	  (assert (typep (apply command-constructor :name :area
				:parent sheet
				:x 0 :y 0
				:width width :height height
				:border-width 0 command-area-initargs)
			 'composite) nil
		  "Command area is not a composite.")
	  
	  ;; Create the control area
	  (assert (typep (apply control-constructor :name :control
				:parent sheet
				:x 0 :y 0
				:width width :height height
				:border-width 0 control-area-initargs)
			 'composite) nil
		  "Control area is not a composite.")
		  
	  (add-event
	    (command-control-area self)
	    :enter-notify
	    #'(lambda (controls)
		(with-slots (parent) (the contact (contact-parent controls))
		  (with-slots (previous-pointer-x) (the command parent)
		    (with-event (kind)
		      ;; Entering from a child? The first time this happens the child must be
		      ;; the default control. Open Look GUI thus dictates that pointer will not
		      ;; warp to original position after exiting the command
		      (when (eq kind :inferior)
			(setf previous-pointer-x nil)))))))
	  
	  ;; Create footer area - display-text-field
	  (make-display-text-field :parent sheet :name :footer
				   :source " " :alignment :left
				   :display-gravity :west)
	  
	  ;; Create default controls
	  (setf (command-default-accept  self)  default-accept)
	  (setf (command-default-cancel  self)  default-cancel)
	  (when default-control
	    (setf (dialog-default-control self)  default-control)
	  ))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;;                               command-sheet                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defcontact command-sheet (core composite)
  ((compress-exposures :initform :on))
  (:resources (event-mask :initform #.(make-event-mask :exposure)))
  (:documentation "The geometry manager for command and control areas."))

 
(defmethod change-layout ((self command-sheet) &optional newly-managed)
  (declare (ignore newly-managed))
  (with-slots (width height parent) self

    ;; Initialize default control instance, if necessary.
    (with-slots (control-default) parent
      (let*
	((controls (composite-children (command-control-area parent)))
	 (instance (cond
		     ((null control-default)
		      (first controls))
		     
		     ((symbolp control-default)
		      (find control-default controls :key #'contact-name)))))
	(when instance
	  (setf (choice-item-highlight-default-p (setf control-default instance)) t))))
    
    ;; Ensure big enough for command/control areas if possible.
    (multiple-value-bind (pw ph) (preferred-size self)
      
      ;; Let window mgr know new preferred minimum height.
      (with-wm-properties (parent)
	(setf (wm-min-width  parent) pw
	      (wm-min-height parent) ph))
      
      (let ((rw (when (< width pw) pw))
	    (rh (when (< height ph) ph)))
	
	(when
	  (or
	    ;; Don't need to request larger size?
	    (not (or rw rh))
	    
	    ;; Request for larger size rejected?
	    (multiple-value-bind (approved-p nx ny nw nh)
		(change-geometry self :width rw :height rh :accept-p t)
	      (declare (ignore nx ny))
	      (and (not approved-p) (eql nw width) (eql nh height))))
	  
	  ;; Yes, adjust child layout for current size.
	  (adjust-layout self))))))


(defmethod adjust-layout ((self command-sheet))
  (with-slots (width height children) self
    (let*
      ((space         (point-pixels
			(contact-screen self)
			(getf *dialog-point-spacing*
			      (contact-scale (contact-parent self)))))
       
       (control-area (find :control children :key #'contact-name))
       (command-area (find :area children :key #'contact-name)) 
       
       (footer        (find :footer children :key #'contact-name))
       (footer-height (contact-height footer)))
      
      ;; Adjust footer geometry.
      (resize footer width footer-height (contact-border-width footer))
      (move footer 0 (- height footer-height))

      ;; Adjust control area geometry: preferred size if possible, but
      ;; no more than available width and no more than half available height.
      (multiple-value-bind (pw ph) (preferred-size control-area :width 0 :height 0)
	(let* ((caw (min (- width space space) pw))
	       (avh (- height footer-height 1))
	       (cah (min (pixel-round avh 2) ph))
	       (cay (- avh cah space)))
	  (resize control-area caw cah 0)
	  (move control-area (max space (pixel-round (- width caw) 2)) cay)
      
	  
	  ;; Adjust command area geometry: preferred size if possible, but
	  ;; no more than available space.
	  (multiple-value-bind (pw ph) (preferred-size command-area :width 0 :height 0)
	    (let ((caw (min (- width space space) pw))
		  (cah (min (- cay space space) ph)))
	      (resize command-area caw cah 0)
	  
	      ;;Center command-area within available space.
	      (move command-area
		    (max space (pixel-round (- width caw) 2))
		    (max space (pixel-round (- cay cah) 2))))))))))


(defmethod display ((manager command-sheet) &optional x y width height &key)
  (declare (ignore x y height width))
  (with-slots (width height children foreground) manager
    (let ((footer (find :footer children :key 'contact-name)))
      (using-gcontext (gcontext :drawable manager :background (contact-current-background-pixel manager)
				:foreground foreground :subwindow-mode :include-inferiors)
	(draw-rectangle manager gcontext 0 0
			(max 1 (1- width))
			(max 1 (- height (contact-height footer) 1))
			)
	))))


(defmethod manage-geometry ((command-sheet command-sheet) child x y width height border-width &key)
  (let (success-p)
    (if (or 
	    
	    (and width  (> width  (contact-width child)))
	    (and height (> height (contact-height child)))
	    )
	(setf success-p #'(lambda (command-sheet)
			    (multiple-value-bind (p-w p-h p-b-w)
				(preferred-size command-sheet)
			      (change-geometry command-sheet
					       :width p-w
					       :height p-h
					       :border-width p-b-w
					       :accept-p t))))
      ;; else...
      (setf success-p t))
    
    (values success-p
	    (or x (contact-x child))
	    (or y (contact-y child))
	    (or width (contact-width child))
	    (or height (contact-height child))
	    (or border-width (contact-border-width child)))))



(defmethod preferred-size ((self command-sheet) &key width height border-width)
  (declare (ignore width height border-width))
  (with-slots (children) self
    (let
      ((space   (point-pixels
		  (contact-screen self)
		  (getf *dialog-point-spacing*
			(contact-scale (contact-parent self)))))
       (area    (find :area children :key 'contact-name))
       (control (find :control children :key 'contact-name))
       (footer  (find :footer children :key 'contact-name)))
      
      (multiple-value-bind (area-width area-height) (preferred-size area)
	(multiple-value-bind (control-width control-height) (preferred-size control)
	  (multiple-value-bind (footer-width footer-height) (preferred-size footer)
	    (declare (ignore footer-width))
	    
	    (values
	      (+ space (max area-width control-width) space)
	      (+ 1 space area-height space control-height space footer-height 1)
	      0)))))))


(defmethod resize :after ((self command-sheet) width height border-width)
  (declare (ignore width height border-width))
  (adjust-layout self))
