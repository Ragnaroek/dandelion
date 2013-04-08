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

(export '(make-property-sheet
	  property-sheet
	  property-sheet-area
 
	  dialog-accept
	  dialog-cancel
	  ))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              property-sheet                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact property-sheet (core core-wm-shell transient-shell)
  ((previous-pointer-x
                    :type (or null int16)
		    :initform nil)
   (previous-pointer-y
                    :type (or null int16)
		    :initform nil)
   (control-default :type (or null contact)
		    :initform nil))
  
  (:resources                                                            
    (border-width  :initform 1) 
    (property-area :type (or function list) :initform nil)
    (default-control :type (member :accept :cancel) :initform :accept)
    )
  (:documentation "A dialog which presents a set of related values for user editing."))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Accessors                                      |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf contact-foreground) :after (new-value (self property-sheet))
  (setf (contact-foreground (car (composite-children self))) new-value))
(defmethod property-sheet-area ((self  property-sheet))
  "Returns the property area of the PROPERTY-SHEET."
  (with-slots (children) (first (slot-value self 'children))
    (find :area children :key #'contact-name)))


(defmethod dialog-default-control ((property-sheet property-sheet))
  (with-slots (control-default) property-sheet
    (contact-name control-default)))


(defmethod (setf dialog-default-control) (new-value (property-sheet property-sheet))
  (check-type new-value (member :accept :cancel) "one of :ACCEPT or :CANCEL")
  (with-slots (control-default) property-sheet
    (when control-default
      (setf (choice-item-highlight-default-p control-default) nil))
    (setf control-default
	  (find new-value (composite-children (first (composite-children property-sheet)))
		:key 'contact-name))
    (setf (choice-item-highlight-default-p control-default) t)
    new-value))

(defmethod dialog-accept ((self property-sheet))
  "Invokes :accept callback function and pops down the dialog" 
  (if (callback-p self :accept)
      (apply-callback self :accept)
      (with-slots ((members children)) (property-sheet-area self)
	(dolist (member members)
	  (apply-callback member :accept))))
  (with-slots (pinned-p) self
    (unless pinned-p (setf (contact-state self) :withdrawn)))) 


(defmethod dialog-cancel ((self property-sheet))
  "Invokes :cancel callback function and pops down the dialog."
  (with-slots (pinned-p) self
    (unless pinned-p (setf (contact-state self) :withdrawn))) 
  (if (callback-p self :cancel)
      (apply-callback self :cancel)
      (with-slots ((members children)) (property-sheet-area self)
	(dolist (member members)
	  (apply-callback member :cancel))))) 

(defmethod shell-mapped ((self property-sheet))
  "Invokes :initialize callback function."
  (let ((footer (find :footer (composite-children
				(car (composite-children self))) :key 'contact-name)))
    (setf (display-text-source footer) " "))
  (apply-callback self :map)
  (apply-callback-else (self :initialize)
    (with-slots ((members children)) (property-sheet-area self)
      (dolist (member members)
	(apply-callback member :initialize)))))

(defmethod (setf contact-state) :after ((new-state (eql :mapped)) (self property-sheet))
  ;; Pointer warping must occur after :map-notify received, in case root-relative
  ;; positions have been changed by window manager redirection.
    (with-slots (previous-pointer-x previous-pointer-y control-default display) self
      (cond ((realized-p self)
      ;; Store position for pointer unwarping later....
	     (multiple-value-setq		
	       (previous-pointer-x previous-pointer-y) (pointer-position self))
	     (warp-pointer
	       control-default
	       (pixel-round (contact-width control-default)  2)
	       (- (contact-height control-default) 2)))
	    (t  ;; Ensure realized.
	     (update-state display))
	    )))


(defmethod shell-unmapped :before ((self property-sheet))
  (with-slots (previous-pointer-x previous-pointer-y) self
    ;; Unwarp pointer to original position, if necessary.
    (when previous-pointer-x
      (warp-pointer self previous-pointer-x previous-pointer-y))))

(defmethod dialog-warn ((self property-sheet) message field)
  "Display a warning for verification error."
  (assert (or (null field) (typep field 'contact)) nil "~s is not a contact." field)
  (let* ((footer (find :footer (composite-children
				 (car (composite-children self))) :key #'contact-name))
	 (actual-message (or message "These values cannot be accepted."))
	 (tw (text-width (display-text-font footer) actual-message)))
    (if (>= tw (contact-width footer))
	(confirm-p
	  :message     actual-message
	  :near        (or field (slot-value self 'control-default))
	  :parent      self
	  :accept-only :on
	  )
	(setf (display-text-source footer) actual-message))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                          Initialization                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun make-property-sheet (&rest initargs &key default-control &allow-other-keys)
  "Creates and returns a property-sheet instance."
  (when default-control
    (assert (symbolp default-control) nil "~s is not a symbol name."))
  (apply #'make-contact 'property-sheet initargs))


(defmethod initialize-instance :after ((self property-sheet)
				       &key property-area (default-control :accept) &allow-other-keys)
  (multiple-value-bind (area-constructor area-initargs)
      (etypecase property-area
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

	(function property-area)

	(list (values (first property-area) (rest property-area))))
    
    (with-slots (width height) self

      ;; Create the manager
      (let ((manager (make-contact 'property-sheet-manager
				   :name :manager
				   :parent self
				   :x 0 :y 0
				   :width width :height height
				   :border-width 0)))
	
	;; Create the property area
	(assert (typep (apply area-constructor
			      :name :area
			      :parent manager
			      :x 0 :y 0
			      :width width :height height
			      :border-width 0
			      area-initargs)
		       'composite) nil
		"Property area is not a composite." )
	
	(labels
	  ((verify      (property-sheet)
			(multiple-value-bind (verified-p message field)
			    (or (not (callback-p property-sheet :verify))
				(apply-callback property-sheet :verify))
			  (if verified-p
			      (dialog-accept property-sheet)	
			      (dialog-warn property-sheet message field))))
	   (menu-accept (property-sheet)
			(verify property-sheet)
			(throw :menu nil))
	   (menu-cancel (property-sheet)
			(dialog-cancel property-sheet)
			(throw :menu nil)))
	  
	  ;; Create buttons for command area
	  (add-callback (make-action-button :parent manager :name :accept :label "Apply")
			  :release #'verify self)
	  (add-callback (make-action-button :parent manager :name :cancel :label "Reset")
			:release #'dialog-cancel self)


	  ;; Create footer area - display-text-field
	  (make-display-text-field :parent manager :name :footer :alignment :left
				   :display-gravity :west)
	  
	  ;; Create settings menu
	  (let ((choice (menu-choice (make-menu :parent self :title "Settings"))))
	    (add-callback (make-action-item :parent choice :name :accept :label "Apply")
			  :release #'menu-accept self)
	    (add-callback (make-action-item :parent choice :name :cancel :label "Reset")
			  :release #'menu-cancel self))
	  
	  ;; Set default control
	  (setf (dialog-default-control self) default-control))))))

  



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                          property-sheet-manager                            |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defcontact property-sheet-manager (core composite)
  ((compress-exposures :initform :on))
  (:resources
    (event-mask :initform #.(make-event-mask :exposure)))
  (:documentation "The geometry manager for property sheet component areas."))


 
(defmethod change-layout ((self property-sheet-manager) &optional newly-managed)
  (declare (ignore newly-managed))
  (with-slots (width height parent) self
    
    ;; Ensure big enough for property area if possible.
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


(defmethod adjust-layout ((psm property-sheet-manager))
  (with-slots (width height children) psm
    (let*
      ((space         (point-pixels
			(contact-screen psm)
			(getf *dialog-point-spacing*
			      (contact-scale (contact-parent psm)))))
       
       (accept-button (find :accept children :key #'contact-name))
       (abw           (contact-border-width accept-button))
       (awidth        (+ abw abw (contact-width accept-button)))
       (aheight       (+ abw abw (contact-height accept-button)))
       
       (cancel-button (find :cancel children :key #'contact-name))
       (cbw           (contact-border-width cancel-button))
       (cwidth        (+ cbw cbw (contact-width cancel-button)))
       (cheight       (+ cbw cbw (contact-height cancel-button)))
       
       (property-area (find :area children :key #'contact-name))
       
       (footer        (find :footer children :key #'contact-name))
       (footer-height (contact-height footer))
       (button-y      (- height (+ (max aheight cheight) space  footer-height 1)))
       (button-x      (pixel-round (- width (+ awidth cwidth space 1)) 2)))
      
      ;; Adjust footer geometry.
      (resize footer width footer-height (contact-border-width footer))
      (move footer 0 (- height footer-height))
      
      ;; Adjust button geometry.  Make their top edges align.
      (move accept-button button-x button-y)
      (move cancel-button (+ button-x (+ awidth space)) button-y)
      
      ;; Adjust property-area geometry: preferred size if possible, but
      ;; no more than available space.
      (multiple-value-bind (pw ph) (preferred-size property-area :width 0 :height 0)
	(let ((paw (min (max 1 (- width space space)) pw))
	      (pah (min (max 1 (- height space space)) ph)))
	  (resize property-area paw pah 0)
	  
	  ;;Center property-area within available space.
	  (move property-area
		(max space (pixel-round (- width paw) 2))
		(max space (pixel-round (- button-y pah) 2))))))))



(defmethod display ((manager property-sheet-manager) &optional x y width height &key)
  (declare (ignore x y height width))
  (with-slots (width height children foreground) manager
    (let ((footer (find :footer children :key 'contact-name)))
      (using-gcontext (gcontext :drawable manager :background (contact-current-background-pixel manager)
				:foreground foreground :subwindow-mode :include-inferiors)
	(draw-rectangle manager gcontext 0 0
			(max 1 (- width 1))
			(max 1 (- height (contact-height footer) 1))
			)
	))))


(defmethod rescale :after ((contact property-sheet))
  (when (realized-p contact)
    (refresh contact)))

;;;
;;;  When the Property Area or one of the Buttons wants to change its geometry we must let it.
;;;  A change in scale will change the sizes of our children.
;;;

(defmethod manage-geometry ((self property-sheet-manager) child x y width height border-width &key)
  (let (success-p)
    (if (or 
	  
	  (and width  (> width  (contact-width child)))
	  (and height (> height (contact-height child)))
	  )
	(setf success-p #'(lambda (self)
			    (multiple-value-bind (p-w p-h p-b-w)
				(preferred-size self)
			      (cond ((and width   (< (contact-width self) p-w))
				     (change-geometry self
						   :width  p-w
						   :border-width p-b-w
						   :accept-p t))
				    ((and height (< (contact-height self) p-h))
				     (change-geometry self
						   :height p-h
						   :border-width p-b-w
						   :accept-p t))
				    (t (change-layout self))))))
	;; else...
	(setf success-p t))
    
    (values success-p
	    (or x (contact-x child))
	    (or y (contact-y child))
	    (or width (contact-width child))
	    (or height (contact-height child))
	    (or border-width (contact-border-width child)))))

(defmethod preferred-size ((self property-sheet-manager) &key width height border-width)
  (declare (ignore width height border-width))
  (with-slots (children) self
    (let* ((accumulated-width 0)
	   (highest 0)
	   (area (find :area children :key #'contact-name))
	   (FOOTER (FIND :FOOTER CHILDREN :KEY #'CONTACT-NAME))
	   (buttons (REMOVE FOOTER (remove area children)))
	   (screen (contact-screen self))
	   (scale  (contact-scale (contact-parent self)))
	   (pixel (getf *dialog-point-spacing* scale))
	   (hspace (point-pixels screen pixel :horizontal))
	   (vspace (point-pixels screen pixel :vertical)))

      ;;Find out how much space the buttons will need.
      ;;Remember: buttons are in a row, so we're interested in combined width
      ;;          and the maximum height
      (multiple-value-bind (pwidth1 pheight1 pbw1)
	  (preferred-size (first buttons))
	(multiple-value-bind (pwidth2 pheight2 pbw2)
	    (preferred-size (second buttons))
	  (setf accumulated-width (+ pwidth1 pbw1 pbw1 hspace pwidth2 pbw2 pbw2)
		highest (max (+ pheight1 pbw1 pbw1) (+ pheight2 pbw2 pbw2)))))
      
      ;;We can ignore the preferred border-width because property-sheet-manager
      ;;geometry management forces a zero-width border.
      (multiple-value-bind (pwidth pheight)
	  (preferred-size area :width 0 :height 0)
	(MULTIPLE-VALUE-BIND (f-pwidth F-PHEIGHT)
	    (PREFERRED-SIZE FOOTER)
	  (declare (ignore f-pwidth))
	  (values (+ (max pwidth accumulated-width) hspace hspace 2)
		  (+ pheight highest F-PHEIGHT vspace vspace vspace 2) ;; add two for rectangle
		  0))))))	                                       ;; drawn around property-area


(defmethod resize :after ((self property-sheet-manager) width height border-width)
  (declare (ignore width height border-width))
  (adjust-layout self))





;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Actions                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defevent property-sheet-manager :enter-notify property-sheet-forget-warp)

(defevent property-sheet (:button-press :button-3) property-sheet-display-menu)

(defun property-sheet-forget-warp (property-sheet-manager)
  (with-slots (parent) (the property-sheet-manager property-sheet-manager)
    (with-slots (previous-pointer-x) (the property-sheet parent)
      (with-event (kind)
	;; Entering from a child? The first time this happens the child must be
	;; the default control. Open Look GUI thus dictates that pointer will not
	;; warp to original position after exiting the property-sheet
	(when (eq kind :inferior)
	  (setf previous-pointer-x nil))))))

(defun property-sheet-display-menu (property-sheet)
  (let ((menu    (first (composite-shells property-sheet)))
	(display (contact-display property-sheet)))
    
    ;; Pop up settings menu
    (present-dialog menu :button :button-3 :state (with-event (state) state))
    
    (catch :menu
      (loop (process-next-event display)))
    
    ;; Pop down settings menu
    (setf (contact-state menu) :withdrawn)))



