;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


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
;;;  Implementation Strategy:
;;;
;;;  
;;;  A confirm is invoked by a originating contact (near). A triangular shadow originating 
;;;  from the "near" contact is drawn into the root with a given quadrant gravity, which
;;;  is dependent on the position of the originating contact. After a response is given
;;;  to confirm the area overshadowed by the confirm's shadow is refreshed over two rectangular
;;;  areas covering the overshadowed area. The sensitivity of the originating contact is turned
;;;  off when a confirm is invoked and turned back on when confirm receives a response.
;;;  


(in-package "CLIO-OPEN")

(export '(
	  confirm
	  confirm-accept-label
	  confirm-accept-only
	  confirm-cancel-label
	  confirm-message
	  confirm-near
	  confirm-p
	  make-confirm
	  ))

;; OL GUI spec for the apex of the confirm, scale-dependent distance from the originating contact)
(defconstant  *confirm-apex-dimensions* (list :small 36 :medium 42 :large 50 :extra-large 64))

(defconstant  *confirm-shadow-images*
	      (list 
		:north-west (list :upper 12%gray :lower 25%gray)
		:north-east (list :upper 12%gray :lower 25%gray)
		:south-west (list :upper 25%gray :lower 50%gray)
		:south-east (list :upper 25%gray :lower 50%gray)
		))


;; Confirm scale is one scale larger than near's scale
(defconstant *scales* '(:small :medium :large :extra-large :extra-large))

;;;----------------------------------------------------------------------------+
;;;    Utility  Functions                                                      +
;;;                                                                            +
;;;----------------------------------------------------------------------------+


(defun quadrant-gravity (x y root)
  (let* ((xc (pixel-round (contact-width root) 2))
	 (yc (pixel-round (contact-height root) 2))
	 (north (< y yc))
	 (west  (< x xc))
	 )
    (if north
	(if west
	    :north-west
	    :north-east)
	(if west
	    :south-west
	    :south-east))))

(defun find-confirm-sheet (confirm)
  (car (composite-children confirm)))

;;;----------------------------------------------------------------------------+
;;;                                                                            +
;;;  Confirm-SHEET contact                                                     +
;;;                                                                            +
;;;----------------------------------------------------------------------------+

(defcontact confirm-sheet  (core composite)
  ()
  (:resources
    (background :initform :parent-relative)
    (event-mask :initform #.(make-event-mask :exposure)))
  (:documentation "The actual container for confirm component areas."))

;;;----------------------------------------------------------------------------+
;;;                                                                            +
;;;  CONFIRM contact                                                           +
;;;                                                                            +
;;;----------------------------------------------------------------------------+

(defcontact confirm (core core-shell override-shell)
  ((near         :initform nil
		 :type (or null contact)
		 :initarg :near
		 :accessor confirm-near
		 :documentation "Indicating point or contact of origination")

   (cancel-label :initform "Cancel"
		 :type     string
		 :accessor confirm-cancel-label
		 :initarg  :cancel-label)
   
   ;; Internal slots
   (points       :type           (vector window) ;; storage x-near y-near & shadow regions
	 	 :initform       (make-array 6))
   (previous-pointer-x
                    :type (or null int16)
		    :initform nil)
   (previous-pointer-y
                    :type (or null int16)
		    :initform nil)
   (control-default :type (or null contact)
		    :initform nil))
  (:resources
    (save-under :initform :on)
    (default-control :initform :accept :type (member :accept :cancel))
    (accept-label    :type string :initform "OK")
    cancel-label
    (border-width    :initform 1) 
    (accept-only     :type (member :on :off) :initform :off)
    (message         :initform "Press a button to continue."))
  (:documentation "A dialog which presents a simple message."))

(defmethod (setf contact-foreground) :after (new-value (self confirm))
  (setf (contact-foreground (car (composite-children self))) new-value))


;; Index values for accessing x-near y-near
(defconstant *x-near*       0)
(defconstant *y-near*       1)


(defun make-confirm (&rest initargs)
  "Creates and returns a confirm instance."
  (apply #'make-contact 'confirm initargs))


;;;----------------------------------------------------------------------------+
;;;                                                                            +
;;; Accessors                                                                  +
;;;                                                                            +
;;;----------------------------------------------------------------------------+

(defun find-accept-button (confirm)
  (find :accept (composite-children (find-confirm-sheet confirm)) :key 'contact-name))

(defun find-cancel-button (confirm)
  (find :cancel (composite-children (find-confirm-sheet confirm)) :key 'contact-name))

(defun find-message-area (confirm)
  (find :message (composite-children (find-confirm-sheet confirm)) :key 'contact-name))

(defmethod dialog-default-control ((self confirm))
  (with-slots (control-default) self
   (contact-name control-default)))

(defmethod (setf dialog-default-control) (new-value (confirm confirm))
  (check-type new-value (member :accept :cancel) "one of :ACCEPT or :CANCEL")
  (assert (or (eq new-value :accept) (eq (confirm-accept-only confirm) :off)) nil
	  "No cancel control exists for ~a." confirm)
  (with-slots (control-default) confirm
    (when control-default
      (setf (choice-item-highlight-default-p control-default) nil))
    (setf control-default
	  (find new-value (composite-children (find-confirm-sheet confirm)) :key #'contact-name))
    (setf (choice-item-highlight-default-p control-default) t)
    new-value))


(defmethod confirm-accept-only ((self confirm))
  (let ((cancel-button (find-cancel-button self)))
    (if (and cancel-button (eq :mapped (contact-state cancel-button)))
	:off
	:on)))


(defmethod (setf confirm-accept-only) (value (self confirm))
  "Set confirm's cancel button to the appropriate setting depending on VALUE.
   create the buttons if necessary."
  (check-type value switch "one of :ON or :OFF")
  (let* ((sheet (find-confirm-sheet self))
	 (cancel-button (find-cancel-button self)))
    (if cancel-button
	(setf (contact-state cancel-button)
	      (if (eq value :on) :withdrawn :mapped))

	(when (eq value :off)
	  (with-slots (cancel-label) self
	    (add-callback (make-action-button :parent sheet :name :cancel :label cancel-label)
			  :release 'dialog-cancel self)))))
  value)


(defmethod confirm-message ((self confirm))
  (display-text-source (find-message-area self)))

 (defmethod (setf confirm-message) (string (self confirm))
  (setf (display-text-source (find-message-area self)) string))

(defmethod confirm-accept-label ((self confirm))
  (button-label (find-accept-button self)))

(defmethod (setf confirm-accept-label) (string (self confirm))
  (setf (button-label (find-accept-button self)) string))

(defmethod confirm-cancel-label ((self confirm))
  (button-label (find-cancel-button self)))

(defmethod (setf confirm-cancel-label) :after (string (self confirm))
  (let ((label  (find-cancel-button self)))
    (when label (setf (button-label label) string))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Initialization                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod initialize-instance :after ((self confirm) &key message accept-only accept-label 
					(default-control :accept) &allow-other-keys)
  (with-slots (x y width height near foreground scale)  self
    (unless near (setq near self))

    ;; Create the sheet
    (let* ((sheet (make-contact 'confirm-sheet :name :sheet
				:parent self
				:x 0 :y 0
				:width width :height height
				:border-width 0))
	   (near-scale (contact-scale near)))

      (setf scale (nth (1+ (or (position near-scale *scales*) 0)) *scales*))
      ;; Create the message area

      (make-display-text :name :message
			 :parent sheet
			 :source message
			 :alignment :center
			 :x 0 :y 0
			 :border-width 0)

      ;; Create buttons for command area
      (add-callback (make-action-button :parent sheet :name :accept :label accept-label)
		    :release 'dialog-accept self)
      
      ;; Initialize cancel control if necessary
      (setf (confirm-accept-only self) accept-only)
            
      
      (setf (dialog-default-control self) default-control))))


;;;----------------------------------------------------------------------------+
;;;                                                                            +
;;; Dialog                                                                     +
;;;                                                                            +
;;;----------------------------------------------------------------------------+
	

(defmethod dialog-accept ((self confirm))
  "Invokes :accept callback function and pops down the dialogue"
  (setf (contact-state self) :withdrawn)
  (apply-callback self :accept)
  )

(defmethod dialog-cancel ((self confirm))
  "Invokes :cancel callback function and pops down the dialogue."
  (setf (contact-state self) :withdrawn)
  (apply-callback self :cancel)
  )



;;;----------------------------------------------------------------------------+
;;;                                                                            +
;;; Confirm Map : where real work happens                                      +
;;;                                                                            +
;;;----------------------------------------------------------------------------+


;; If the pointer moves off the Confirm don't warp pointer to Near just leave
;; where the Confirm action button was selected otherwise warp pointer to Near after
;; selecting a Confirm action button.

;; Track the state of pointer position w.r.t Confirm by storing state in internal slot 
;; of Confirm (ie. Did it stay on the Confirm the whole time or did it move off the Confirm?).


(labels
  ((calculate-upper-shadow-vertices (points x y gravity right-edge bottom-edge)
    "Determine the two sets of points for drawing the upper triangular shadow"
    (case gravity
      (:north-east
       (setf (svref points 2) x (svref points 3) y (svref points 4) (+ 3 right-edge) (svref points 5)  y))
      (:north-west
       (setf (svref points 2) x (svref points 3) y (svref points 4) right-edge (svref points 5)  (1- y)))
      (:south-west
       (setf (svref points 2) x (svref points 3) y (svref points 4) x (svref points 5) (+ bottom-edge 1)))
      (:south-east
       (setf (svref points 2) (+ right-edge 2)
	     (svref points 3) (+ 2 bottom-edge) (svref points 4) (+ right-edge 2) (svref points 5) y))))
   
   (calculate-lower-shadow-vertices (points x y gravity right-edge bottom-edge)
    "Determine the two sets of points for drawing the lower triangular shadow"
	 (case gravity
	   (:north-east
	    (setf (svref points 2) (+ right-edge 2)
		  (svref points 3) y (svref points 4) (+ right-edge 2) (svref points 5) bottom-edge))
	   (:north-west
	    (setf (svref points 2) x (svref points 3) (1- y) (svref points 4) x (svref points 5) bottom-edge))
	   (:south-west
	    (setf (svref points 2) (1- x)
		  (svref points 3) (+ bottom-edge 1) (svref points 4) right-edge (svref points 5) (+ bottom-edge 2)))
	   (:south-east
	    (setf (svref points 2) x
		  (svref points 3) (+ 2 bottom-edge) (svref points 4) (+ 2 right-edge) (svref points 5) (+ 2  bottom-edge)))))
   
   (draw-confirm-triangular-shadows (confirm root x y width height points gravity)
    "Draw two triangular shadows originating from NEAR given the calculated vertices"
      (proclaim '(inline calculate-shadows-vertices ))
      (let*
	((images (getf *confirm-shadow-images* gravity))
	 (upper-image (getf images :upper))
	 (lower-image (getf images :lower))
	 (bottom-edge  (+ y height))
	 (right-edge   (+ x width)))
	(calculate-upper-shadow-vertices points x y gravity right-edge bottom-edge)
	(using-gcontext
	  (gcontext :drawable root
		    :background (contact-current-background-pixel confirm)
		    :foreground (screen-black-pixel (contact-screen root))
		    :fill-style :opaque-stippled
		    :stipple    (contact-image-mask root upper-image :depth 1)
		    :subwindow-mode :include-inferiors
		    )	  
	  (draw-lines root gcontext points :fill-p t :shape :complex)
	  (calculate-lower-shadow-vertices points x y gravity right-edge bottom-edge)
	  (with-gcontext (gcontext :stipple (contact-image-mask root lower-image :depth 1))
	    (draw-lines root gcontext points :fill-p t :shape :complex))))))
  
  (defmethod shell-mapped ((self confirm))
    "Recomputes x and y given NEAR and invokes :initialize callback function."    
    (with-slots (near height width points previous-pointer-x previous-pointer-y control-default) self
      (unless (eq self near)
	(multiple-value-bind (x-near y-near)
	  (contact-translate near
			     (pixel-round (contact-width near) 2)   ;; Use center point of near
			     (pixel-round (contact-height near) 2))
	  (setf (svref points *x-near*) x-near)
	  (setf (svref points *y-near*) y-near)
	  (let* ((root (contact-root self))
		 (gravity (quadrant-gravity x-near y-near root))
		 (apex    (getf *confirm-apex-dimensions* (contact-scale self)))
		 (root-width  (contact-width root))
		 (root-height (contact-height root)))

	    ;; Set Confirm's X and Y w.r.t originating contact
	    (multiple-value-bind (x y)
		(case gravity
		  (:north-east
		   (values (- x-near apex width)
			   (+ y-near apex)))
		  (:north-west
		   (values (+ x-near apex)
			   (+ y-near apex)))
		  (:south-west
		   (values (+ x-near apex)
			   (- y-near apex height)))
		  (:south-east
		   (values (- x-near apex width)
			   (- y-near apex height))))
	      
	      ;; If CONFIRM will be clipped, compensate
	      ;; and adjust x and y of CONFIRM
	      (let ((adjusted-x (min (max x 0) (- root-width width)))
		    (adjusted-y (min (max y 0) (- root-height height))))
		(change-geometry self
				 :x adjusted-x 
				 :y adjusted-y 
				 )
		;; Turn near's sensitivity off
		(setf (contact-sensitive near) :off))))))
      
      (apply-callback self :map)
      (apply-callback self :initialize)

      ;; Store position for pointer unwarping later....
      (multiple-value-setq
	(previous-pointer-x previous-pointer-y) (pointer-position self))
      
      (warp-pointer
	control-default
	(pixel-round (contact-width control-default)  2)
	(- (contact-height control-default) 2))))
  

  (defmethod display ((manager confirm-sheet)
		      &optional exposed-x exposed-y exposed-width exposed-height &key)
    (declare (ignore exposed-x exposed-y exposed-height exposed-width))
    (proclaim '(inline draw-confirm-triangular-shadows))
    
    (with-slots (width height x y points) (contact-parent manager)
      (let ((root (contact-root manager)))
	(draw-confirm-triangular-shadows
	  manager root
	  x  y width height points
	  (quadrant-gravity (svref points *x-near*) (svref points *y-near*) root))))

      (with-slots (width height foreground) manager
	(using-gcontext (gcontext :drawable manager :foreground foreground :Subwindow-mode :include-inferiors)
	  (draw-rectangle manager gcontext 3 3 (- width 7) (- height 7))))))



(defevent confirm :leave-notify pointer-off-confirm)

(defmethod pointer-off-confirm ((self confirm))
  (with-slots (previous-pointer-x) self
    (setf previous-pointer-x nil)))



(labels
  ((calculate-reexposed-areas (confirm root)
    "Determine two rectangular areas encompassing the triangular shadows drawn by confirm"
      (with-slots (x y width height near points) confirm
	(let* (
	       (apex (getf *confirm-apex-dimensions* (contact-scale confirm)))
	       (x-near      (svref points *x-near*))
	       (y-near      (svref points *y-near*))
	       (right-edge  (+ x width))
	       (bottom-edge (+ y height))
	       (gravity (quadrant-gravity x-near y-near root))
	       )
	  (case gravity
	    (:north-east
	     (values
	       x           (- y apex) width apex
	       right-edge (- y apex) apex  (+ height apex))
	     )
	    (:north-west
	     (values
	       x-near y-near     apex   (+ height apex)
	       x     (- y apex)  width  apex)
	     )
	    (:south-west
	     (values
	       (- x apex) y           apex  (+ height apex)
	       x         bottom-edge width apex)
	     )
	    (:south-east
	     (values 
	       x           bottom-edge width apex
	       right-edge	 y           apex  (+ height apex)))))))
   
   (reexpose-overshadowed-area (confirm root near)
    "Refresh the root area that confirm overshadowed"
      (proclaim '(inline calculate-reexposed-areas))
      (multiple-value-bind (area1-x area1-y area1-width area1-height
			    area2-x area2-y area2-width area2-height)
	  (calculate-reexposed-areas confirm root)
	(refresh root :x area1-x :y area1-y :width area1-width :height area1-height)
	(with-slots (sensitive) near
	    (setq sensitive :on))
	(refresh root :x area2-x :y area2-y :width area2-width :height area2-height))))

  (defmethod shell-unmapped :before ((self confirm))
    (proclaim '(inline reexpose-overshadowed-area))
    (with-slots (points near previous-pointer-x previous-pointer-y) self
      (unless (eq self near)
	;; Erase shadow.
	(reexpose-overshadowed-area self (contact-root self) near)
	
	;; Unwarp pointer to original position, if necessary.
	(when previous-pointer-x
	  (warp-pointer self previous-pointer-x previous-pointer-y))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            +
;;;  Geometry Management                                                       +
;;;                                                                            +
;;;----------------------------------------------------------------------------+
 
(defmethod change-layout ((self confirm-sheet) &optional newly-managed)
  ;;The idea here is to make the accept and cancel buttons be separated by the
  ;;standard horizontal spacing, and then centered within the sheet.  The standard 
  ;;vertical spacing will be enforced between the bottom edge of the taller button
  ;;and the edge of the message.
  ;;Force the message area to be the smaller of its preferred size or the space remaining
  ;;(allowing for horizontal/vertical margins).  Center it within the remaining space.
  (declare (ignore newly-managed))
  (with-slots (width height children parent) self
    (let* ((accept-button (find-accept-button parent))
	   (cancel-button (find-cancel-button parent))
	   (message-area  (find-message-area  parent))
	   (abw (contact-border-width accept-button))
	   (awidth (+ abw abw (contact-width accept-button)))
	   (aheight (+ abw abw (contact-height accept-button)))
	   (screen (contact-screen self))
	   (pixel (getf *dialog-point-spacing* (contact-scale self)))
	   (hspace (point-pixels screen pixel :horizontal))	
	   (vspace (point-pixels screen pixel :vertical))
	   rbw rwidth rheight button-x button-y) 

      ;;Figure out where buttons should go.  Make their top edges align.
      (if (eq (confirm-accept-only (contact-parent self)) :on)
	  (progn
	    (setf button-y (- height aheight vspace)
		  button-x (floor (- width awidth) 2))
	    (move accept-button  button-x button-y)
	    )
	  (progn 
	    (setf rbw (contact-border-width cancel-button)
		  rwidth (+ rbw rbw (contact-width cancel-button))
		  rheight (+ rbw rbw (contact-height cancel-button))
		  button-y (- height (+ (max aheight rheight) vspace 3))       
		  button-x (floor (- width (+ awidth rwidth hspace 3)) 2))
	    (with-state (accept-button)
	      (move accept-button  button-x  button-y)
	      )
	    (incf button-x (+ awidth hspace))
	    (with-state (cancel-button)
	      (move cancel-button button-x button-y)
	      )
	    )
	  )

      (IF (or (zerop width) (zerop height) )			; not initialized...
	  (multiple-value-bind (p-width p-height)
	      (preferred-size self)
	    (change-geometry self :width p-width :height p-height :accept-p t))
	  ;; else...
	  
	  ;;Make message-area fit within space remaining
	  (with-state (message-area)
	    (let ((new-width  (max 1
				   (- width hspace hspace)
				   ))
		  (new-height (max 1 (- button-y vspace vspace)))
		  )
	      (resize message-area
		      new-width 	;;use 1 as a lower bound to prevent
		      new-height	;;width/height sizing errors
		      0)
	      ;;Center message-area within space remaining.
	      ;;Don't have to worry about it's border-width since it's guaranteed
	      ;;to be zero by the previous call to RESIZE.
	      (move message-area
		    (max hspace (floor (- width (contact-width message-area)) 2))
		    (max vspace (floor (- (contact-y accept-button) (contact-height message-area)) 2)))))
	  ))))



(defmethod resize :after ((self confirm-sheet) width height border-width)
  (declare (ignore width height border-width))
  (change-layout self))

(defmethod manage-geometry ((self confirm-sheet) child x y width height border-width &key) 
  (let (success-p)
    (multiple-value-bind (p-w p-h p-b-w)
	(preferred-size self)
      (if  (or 
	     (/= p-w  (contact-width self))
	     (/= p-h  (contact-height self))
	     (and width  (/= width  (contact-width child)))
	     (and height (/= height (contact-height child)))
	     )
	   (setf success-p  #'(lambda (self)
			       (progn (change-geometry self
						       :width p-w
						       :height p-h
						       :border-width p-b-w
						       :accept-p t)
				      (change-layout self))))
	   (setf success-p t)))
    (values success-p
	    (or x (contact-x child))
	    (or y (contact-y child))
	    (or width (contact-width child))
	    (or height (contact-height child))
	    (or border-width (contact-border-width child)))))
      


(defmethod preferred-size ((self confirm-sheet) &key width height border-width)
  (declare (ignore width height border-width))
  (with-slots (children parent) self
    (let* ((accumulated-width 0)
	   (highest 0)
	   (apply-button  (find-accept-button parent))
	   (cancel-button (find-cancel-button parent))
	   (message-area  (find-message-area  parent))
	   (screen (contact-screen self))
	   (pixel (getf *dialog-point-spacing* (contact-scale self)))
	   (hspace (point-pixels screen pixel :horizontal))
	   (vspace (point-pixels screen pixel :vertical)))

      ;;Find out how much space the buttons will need.
      ;;Remember: buttons are in a row, so we're interested in combined width
      ;;          and the maximum height
      (multiple-value-bind (pwidth1 pheight1 pbw1)
	  (preferred-size apply-button)
	(setf accumulated-width (+ pwidth1 pbw1 pbw1)
	      highest (+ pheight1 pbw1 pbw1))
	(when (eq (confirm-accept-only (contact-parent self)) :off)
	  (multiple-value-bind (pwidth2 pheight2 pbw2)
	      (preferred-size cancel-button)
	    (setf accumulated-width (+ accumulated-width hspace pwidth2 pbw2 pbw2)
		  highest (max highest (+ pheight2 pbw2 pbw2))))))
      
      ;;We can ignore the preferred border-width because confirm-sheet
      ;;geometry management forces a zero-width border.
      (multiple-value-bind (pwidth pheight)
	  ;; Use width/height 0 to request minimum text extent size.
	  (preferred-size message-area :width 0 :height 0) 
	(values (+ (max pwidth accumulated-width) hspace hspace 6)
		(+ pheight highest vspace vspace vspace 6)
		0))))) 


;;;----------------------------------------------------------------------------+
;;;                                                                            +
;;;   WITH-CONFIRM     Using cached confirms                                   +
;;;                                                                            +                        
;;;----------------------------------------------------------------------------+


(defmacro top-level-confirms (top)
  "A list of confirm contacts associated with TOP."
  `(getf (window-plist ,top) :confirm-cache))


(defun confirm-p (&rest initargs &key near &allow-other-keys)
  "Bind a confirm to the given initargs either by allocating one from
   the confirm cache if one exists or instantiate one"
  (assert near () "A :near initarg was not provided for CONFIRM-P")
  (let* ((near-scale (contact-scale near))	 
	 (top-level  (contact-top-level near))
	 (background (getf initargs :background))
	 (confirm    (pop (top-level-confirms top-level)))
	 (display    (contact-display near)))

    (setf background
	  (if background
	      (convert near background '(or (member :none :parent-relative) pixel pixmap))
	      (contact-current-background-pixel top-level)))

    (if confirm
	(let ((foreground      (getf initargs :foreground))	    
	      (accept-label    (getf initargs :accept-label))
	      (cancel-label    (getf initargs :cancel-label))
	      (accept-only     (getf initargs :accept-only))
	      (message         (getf initargs :message))
	      (near            (getf initargs :near))
	      (default-control (getf initargs :default-control)))
	  
	  (setf (contact-background confirm) background)
	  
	  (setf (contact-foreground confirm)
		(convert near
			 (or foreground :black)
			 '(or (member :none :parent-relative) pixel pixmap)))	  
	  (setf (confirm-accept-label confirm)
		(if accept-label
		    (convert near accept-label 'string)
		    "OK"))
	  (setf (confirm-accept-only confirm)
		(if accept-only
		    (convert near accept-only '(member :on :off))
		    :off))
	  (setf (confirm-cancel-label confirm)
		(if cancel-label
		    (convert near cancel-label 'string)
		    "Cancel"))
	  (setf (confirm-message confirm)
		(if message
		    (convert near message 'string)
		    "Press a button to continue."))
	  (setf (confirm-near confirm) near)
	  (setf (dialog-default-control confirm)
		(if default-control
		    (convert near default-control '(member :accept :cancel))
		    :accept))
	  (setf (contact-scale confirm)
		(nth (1+ (or (position near-scale *scales*) 0)) *scales*)))

	(setf confirm
	      (apply
		#'make-confirm
		:parent top-level
		:background background
		:scale near-scale
		:callbacks `((:accept (,#'(lambda () (throw :exit-confirm t))))
			     (:cancel (,#'(lambda () (throw :exit-confirm nil)))))
		initargs))) 

    
    (setf (contact-state confirm) :mapped)
    (unwind-protect
	(catch :exit-confirm
	  (loop (process-next-event display)))
      (push confirm (top-level-confirms top-level)))))


(defmethod present-dialog ((confirm confirm) &key x y button state)
   (declare (type (or card16 null) x y)
	    (type (or (member :button-1 :button-2 :button-3 :button-4 :button-5) null) button)
	    (type (or mask16 null) state))
   (declare (ignore button state x y))
   (setf (contact-state confirm) :mapped))
