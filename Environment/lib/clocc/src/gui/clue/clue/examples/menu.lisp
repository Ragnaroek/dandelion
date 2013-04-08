;;; -*- Mode:Lisp; Package:USER; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                   P.O. BOX 149149                                |
;;;                                AUSTIN, TEXAS 78714-9149                          |
;;;                                                                                  |
;;;                Copyright (C) 1989,1990 Texas Instruments Incorporated            |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; Source code for CLUE examples described in Explorer X Window System        |
;;; Programmer's Reference Manual.                                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(in-package "CLUE-EXAMPLES" :use '(common-lisp xlib clue))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                   Menu                                     |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact menu (override-shell)
  ()
  (:resources
    (font       :type     font)
    (foreground :type     pixel)
    (title      :type     string)
	   
    (state      :initform :withdrawn))
  
  (:documentation
    "Presents a column of menu items."))

;;                               Initialization

(defmethod initialize-instance :after ((menu menu) &key title font foreground background &allow-other-keys)
  ;; Create title-frame containing choices child to manage menu items
  (let* ((title   (make-contact
                    'title-frame
                    :parent     menu
                    :name       :title
                    :text       title
                    :font       font
                    :foreground foreground
                    :background (or background :white)))
         
         (manager (make-contact
                    'choices
                    :parent       title
                    :name         :manager
                    :border-width 0)))

    ;; Define callback to handle effect of selection
    (add-callback manager :select 'popup-menu-select menu)

    ;; Moving pointer off menu causes nil selection
    (add-event manager
               '(:leave-notify :ancestor :nonlinear)
               '(choice-select nil))))



;;                      :Leave-Notify Event Specifications

(defun leave-check (event-key &rest kinds)
  (dolist (kind kinds)
    (unless (member kind '(:ancestor :virtual :inferior :nonlinear :nonlinear-virtual))
      (error "~s isn't a valid kind of ~s event" kind event-key)))
  (list 'leave-match kinds))

(defun leave-match (event kinds)
  (member (slot-value event 'kind) kinds :test #'eq))

(setf (check-function :leave-notify) 'leave-check)



;;                               Menu operations

(defun menu-manager (menu)
  (title-content (first (composite-children menu))))

(defmethod popup-menu-select ((menu menu))
  ;; Pop down immediately
  (setf (contact-state menu) :withdrawn)
  (display-force-output (contact-display menu))

  ;; Invoke menu callback
  (apply-callback menu :select))

(defun menu-present (menu x y)
  "Present the MENU with the first item centered on the given position."
  ;; Complete initial geometry management before positioning menu
  (unless (realized-p menu)
    (initialize-geometry menu))

  (let ((parent  (contact-parent menu))
        (item    (first (composite-children (menu-manager menu)))))

    ;; Compute the y position of the center of the first item
    ;; with respect to the menu
    (multiple-value-bind (item-x item-y)
        (contact-translate item 0 (round (contact-height item) 2) menu)
      (declare (ignore item-x))

      ;; Try to center first item at the given location, but
      ;; make sure menu is completely visible in its parent  
      (change-geometry
        menu
        :x (max 0 (min (- (contact-width parent) (contact-width menu))
                       (- x (round (contact-width menu) 2))))
        :y (max 0 (min (- (contact-height parent) (contact-height menu))
                       (- y item-y)))
        :accept-p t)))
  
  ;; Make menu visible
  (setf (contact-state menu) :mapped))


(defun menu-choose (menu x y)
  "Present the MENU at the given location and return the label of the
item chosen. If no item is chosen, then nil is returned."

  ;; Set menu callback to return chosen item label
  (add-callback menu :select 'throw-menu-selection menu)

  ;; Display the menu so that first item is at x,y.
  (menu-present menu x y)

  ;; Event processing loop
  (catch :menu-selection
    (loop (process-next-event (contact-display menu)))))

(defun throw-menu-selection (menu)
  "Throw to :menu-selection tag, returning the label of the selected menu button (if any)."
  (let ((selection (choice-selection (menu-manager menu))))
    (throw :menu-selection
           (when selection (button-label selection)))))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                Title Frame                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact title-frame (composite)

  ((font
     :accessor title-font
     :initarg  :font
     :initform "fixed"
     :type     font)

   (foreground
     :accessor title-foreground
     :initarg  :foreground
     :initform :black
     :type     pixel)   
   
   (text
     :accessor title-text
     :initarg  :text
     :type     string)

   (compress-exposures
     :allocation :class
     :initform   :on
     :reader     contact-compress-exposures
     :type       (member :off :on)))
  
  (:resources
    font
    foreground
    text
    (event-mask :initform #.(make-event-mask :exposure)))
  
  (:documentation
    "A composite consisting of a text title and another contact."))



;;                                  Accessors

(defmethod (setf title-font) (new-value (title-frame title-frame))
  (title-update title-frame :font (convert title-frame new-value 'font)))

(defmethod (setf title-text) (new-value (title-frame title-frame))
  (title-update title-frame :text new-value))

(defmethod title-update ((title-frame title-frame) &key text font)
  (with-slots ((current-text text) (current-font font)) title-frame
            
    ;; Update slots
    (setf current-text (or text current-text)
          current-font (or font current-font))
    
    ;; Update geometry
    (when (realized-p title-frame)
      (change-layout title-frame))))

(defmethod title-content ((title-frame title-frame))
  (with-slots (children) title-frame
    (first children)))



;;                             Geometry management

(defmethod add-child :before ((title-frame title-frame) child &key)
  (declare (ignore child))
  ;; A title-frame can only have a single content child
  (assert (not (title-content title-frame))
          nil "~s already has a content." title-frame))


(defmethod manage-geometry ((title-frame title-frame) child x y width height border-width &key)
  (with-slots ((frame-width width) (frame-height height)) title-frame
    
    (let* ((x            (or x            (contact-x child)))
           (y            (or y            (contact-y child)))
           (width        (or width        (contact-width child)))
           (height       (or height       (contact-height child)))
           (border-width (or border-width (contact-border-width child)))
           (total-width  (+ width border-width border-width))
           (total-height (+ height border-width border-width)))
      
      ;; Get preferred frame size for requested content geometry
      (multiple-value-bind (min-width min-height)
          (title-preferred-size-if
            title-frame total-width total-height frame-width frame-height)
        
        ;; Try to ensure at least preferred frame size
        (when
          (or (setf min-width  (when (< frame-width min-width)   min-width))
              (setf min-height (when (< frame-height min-height) min-height)))
          (change-geometry title-frame
                           :width min-width
                           :height min-height
                           :accept-p t)))
      
      ;; Approve request based on current frame size and title size
      (multiple-value-bind (title-width title-height) (title-size title-frame)
        (declare (ignore title-width))
        
        (let ((approved-x      0)
              (approved-y      title-height)
              (approved-width  (- frame-width border-width border-width))
              (approved-height (- frame-height title-height border-width border-width)))
          
          (values
            (and (= x approved-x) (= y approved-y)
                 (= width approved-width) (= height approved-height))
            approved-x
            approved-y
            approved-width
            approved-height
            border-width))))))

(defmethod change-layout ((title-frame title-frame) &optional newly-managed)
  (declare (ignore  newly-managed))
  (with-slots (width height) title-frame

    ;; Try to ensure at least preferred size
    (multiple-value-bind (min-width min-height) (preferred-size title-frame)
      (when
        (or (setf min-width  (when (< width min-width)   min-width))
            (setf min-height (when (< height min-height) min-height)))
        (change-geometry title-frame
                         :width min-width
                         :height min-height
                         :accept-p t)))

    ;; Adjust title, content geometry to current size
    (title-adjust title-frame)))


(defmethod preferred-size ((title-frame title-frame) &key width height border-width)
  (let ((content (title-content title-frame))
        (width   (or width (contact-width title-frame)))
        (height  (or height (contact-height title-frame))))

    ;; Determine total size of content, including border width
    (multiple-value-bind (current-content-width current-content-height)
        (if content
            
            (with-slots ((content-width width)
                         (content-height height)
                         (content-border-width border-width)) content
              (values (+ content-width content-border-width content-border-width)
                      (+ content-height content-border-width content-border-width)))
            
            (values 0 0))

      ;; Determine preferred frame size for this content
      (multiple-value-bind (preferred-width preferred-height)
          (title-preferred-size-if
            title-frame current-content-width current-content-height width height)
        
        (values
          preferred-width
          preferred-height        
          (or border-width (contact-border-width title-frame)))))))


(defun title-preferred-size-if (title-frame content-width content-height width height)
  "Return preferred TITLE-FRAME width and height, assuming given content size and the
   suggested WIDTH and HEIGHT for the TITLE-FRAME."
  
  (multiple-value-bind (title-width title-height)
      (title-size title-frame)
    
    (values
      ;; width
      (max title-width content-width width)
      
      ;; height
      (max (+ title-height content-height) height))))


(defun title-adjust (title-frame)
  "Rearrange title and content according to current size of TITLE-FRAME."
  (with-slots (width height) title-frame
    (let* ((content      (title-content title-frame))
           (border-width (contact-border-width content)))

      ;; Determine dimensions of title string
      (multiple-value-bind (title-width title-height) (title-size title-frame)
        (declare (ignore title-width))
        
        (let ((approved-x      0)
              (approved-y      title-height)
              (approved-width  (- width border-width border-width))
              (approved-height (- height title-height border-width border-width)))

          ;; Reposition content
          (with-state (content)
            (when (not (and (= (contact-x content) approved-x)
                            (= (contact-y content) approved-y)))
              (move content approved-x approved-y))
            
            (when (not (and (= (contact-width content) approved-width)
                            (= (contact-height content) approved-height)))
              (resize content approved-width approved-height border-width)))

          ;; Redisplay title
          (when (realized-p title-frame)
            (clear-area title-frame :exposures-p t)))))))


(defun title-size (title-frame)
  "Return the width and height of the title string of the TITLE-FRAME."
  (with-slots (font text) title-frame
    (values
      (text-width font text)
      (+ (font-ascent font) (font-descent font)))))

(defun title-position (title-frame)
  "Return the position of the title string of the TITLE-FRAME."
  (with-slots (font text width) title-frame
    (values
      (round (- width (text-width font text)) 2)
      (font-ascent font))))

(defmethod resize :after ((title-frame title-frame) width height border-width)
  (declare (ignore width height border-width))
  (title-adjust title-frame))



;;                                   Display

(defmethod display ((title-frame title-frame) &optional x y width height &key)
  (declare (ignore x y width height))
  
  (with-slots (font text foreground background) title-frame
    (multiple-value-bind (title-x title-y) (title-position title-frame)
      
      ;; Draw title string in "reverse-video"
      (using-gcontext (gc :drawable   title-frame
                          :font       font
                          :foreground background
                          :background foreground)       
        (draw-image-glyphs title-frame gc title-x title-y text)))))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Column                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact column (composite) ()
  (:documentation
    "Arranges its children in a vertical column."))

(defmethod manage-geometry ((column column) child x y width height border-width &key)
  (with-slots
    ((child-width width)
     (child-height height)
     (child-border-width border-width)
     (child-x x)
     (child-y y))
    child

    (let*
      ;; No position change can be approved.
      ((position-approved-p     (not (or (unless (null x) (/= x child-x))
                                         (unless (null y) (/= y child-y)))))
       
       ;; Check if requested size change can be approved.
       (total-width            (+ child-width child-border-width child-border-width))
       (total-height           (+ child-height child-border-width child-border-width))
       (requested-width        (or width child-width))
       (requested-height       (or height child-height))
       (requested-border-width (or border-width child-border-width))
       (new-total-width        (+ requested-width requested-border-width requested-border-width))
       (new-total-height       (+ requested-height requested-border-width requested-border-width)))

      ;; Refuse size change immediately if it reduces item size
      (when (or (< new-total-width total-width) (< new-total-height total-height))
        (return-from manage-geometry
          (values
            nil
            child-x
            child-y
            (- child-width requested-border-width requested-border-width)
            (- child-height requested-border-width requested-border-width)                 
            requested-border-width)))

      ;; Approve size change immediately if it does not affect item size
      (when (and (= new-total-width total-width) (= new-total-height total-height))     
        (return-from manage-geometry
          (values
            position-approved-p 
            child-x
            child-y
            requested-width
            requested-height
            requested-border-width)))

      ;; Otherwise, a larger item size has been requested.
      ;; Check if column size can be enlarged sufficiently.
      (multiple-value-bind (column-width column-height)
          (column-preferred-size column new-total-width new-total-height)

        ;; Request change to preferred column size
        (multiple-value-bind
          (approved-p approved-x approved-y approved-width approved-height)
            (change-geometry column :width column-width :height column-height)
          (declare (ignore approved-x approved-y))
         
          (if approved-p
              
              ;; Larger column size approved.
              (return-from manage-geometry
                (values
                  ;; When requested child geometry approved (both size and position),
                  ;; then return a function to implement the after-effect of approved
                  ;; child geometry changes. Column layout will then reflect the new
                  ;; item size.           
                  (when position-approved-p 'change-layout)
                  child-x
                  child-y
                  requested-width
                  requested-height
                  requested-border-width))
              
              ;; Larger column size NOT approved. Return best item size that could fit
              ;; approved column size
              (return-from manage-geometry
                (values
                  nil
                  child-x
                  child-y
                  (- approved-width requested-border-width requested-border-width)
                  (- (floor approved-height (length (composite-children column)))
                     requested-border-width requested-border-width)
                  requested-border-width))))))))


(defmethod change-layout ((column column) &optional newly-managed)
  (declare (ignore newly-managed))
  (with-slots (width height) column

    ;; Compute the maximum preferred size of all children.
    (multiple-value-bind (item-width item-height)
        (column-item-size column)

      ;; Compute preferred column size, assuming this item size
      (multiple-value-bind (preferred-width preferred-height)
          (column-preferred-size column item-width item-height)
        
        ;; Try to ensure at least preferred size
        (if
          (or (setf preferred-width  (when (< width preferred-width)   preferred-width))
              (setf preferred-height (when (< height preferred-height) preferred-height)))
          
          ;; Ask parent for larger size
          (change-geometry column
                           :width    preferred-width
                           :height   preferred-height
                           :accept-p t)
          
          ;; Else current size is big enough
          (column-adjust column item-width item-height))))))


(defmethod preferred-size ((column column) &key width height border-width)
  (multiple-value-bind (item-width item-height)
        (column-item-size column)       
      (multiple-value-bind (preferred-width preferred-height)
          (column-preferred-size column item-width item-height)
        (values
          (if width  (max width preferred-width)   preferred-width)
          (if height (max height preferred-height) preferred-height)
          (or border-width (slot-value column 'border-width))))))


(defun column-preferred-size (column item-width item-height)
  "Return the preferred width and height for COLUMN, assuming the given
ITEM-WIDTH and ITEM-HEIGHT."
  (with-slots (children) column
    (let ((preferred-margin 8))
      (values
        (+ item-width preferred-margin preferred-margin)
        (+ (* (length children) (+ item-height preferred-margin))
           preferred-margin)))))


(defun column-item-size (column)
  "Return the maximum preferred width and height of all COLUMN children."
  (with-slots (children) column
    (let ((item-width 0) (item-height 0))
      (dolist (child children)
        (multiple-value-bind (child-width child-height child-bw)
            (preferred-size child)
          (setf item-width  (max item-width  (+ child-width child-bw child-bw))
                item-height (max item-height (+ child-height child-bw child-bw)))))
      (values item-width item-height))))


(defun column-adjust (column &optional item-width item-height)
  "Rearrange COLUMN items according to current COLUMN size. If given, ITEM-WIDTH
   and ITEM-HEIGHT define the new size for all items."
  (with-slots (children width height) column
    (when children
      ;; Compute preferred item size, if necessary
      (unless item-height
        (multiple-value-setq (item-width item-height)
          (column-item-size column)))
      
      ;; Compute item spacing
      (let* ((number-items (length children))
             (margin       (max (round (- width item-width)
                                       2)
                                0))
             (space        (max (round (- height (* number-items item-height))
                                       (1+ number-items))
                                0)))
        
        ;; Set size and position of each child
        (let ((y 0))
          (dolist (child children)
            (let ((bw (contact-border-width child)))
              (with-state (child)
                (resize child (- item-width bw bw) (- item-height bw bw) bw) 
                (move child margin (incf y space))))
            (incf y item-height)))))))


(defmethod resize :after ((column column) width height border-width)
  (declare (ignore width height border-width))
  (column-adjust column))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Choices                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact choices (column)

  ((selection
     :reader   choice-selection
     :initform nil
     :type     (or null contact)))
  
  (:documentation
    "A column of items to choose from."))


(defmethod add-child :after ((choices choices) child &key)
  ;; Initialize child's :select callback
  (add-callback child :select 'choice-select choices child))


(defmethod choice-select ((choices choices) child)
  ;; Record current selection
  (with-slots (selection) choices
    (setf selection child))

  ;; Invoke selection callback
  (apply-callback choices :select))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Button                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact button (contact)
  
  ((label
     :accessor   button-label
     :initarg    :label
     :initform   ""
     :type       string)

   (font
     :accessor   button-font
     :initarg    :font
     :initform   "fixed"
     :type       font)

   (foreground
     :accessor   button-foreground
     :initarg    :foreground
     :initform   :black
     :type       pixel)

   (compress-exposures
     :allocation :class
     :initform   :on
     :reader     contact-compress-exposures
     :type       (member :off :on)))
  
  (:resources
    (background :initform :white)
    (border     :initform :white)
    font
    foreground
    label)
  
  (:documentation
    "Triggers an action."))




;;                                   Display

(defmethod display ((button button) &optional x y width height &key)
  (declare (ignore x y width height))
  
  (with-slots
    (font label foreground (button-width width) (button-height height))
    button
    
    ;; Get metrics for label string
    (multiple-value-bind (label-width ascent descent left right font-ascent font-descent)
        (text-extents font label)
      (declare (ignore ascent descent left right))

      ;; Center label in button
      (let ((label-x (round (- button-width label-width) 2))
            (label-y (+ (round (- button-height font-ascent font-descent) 2)
                        font-ascent)))

        ;; Use an appropriate graphics context from the cache
        (using-gcontext (gc :drawable   button
                            :font       font
                            :foreground foreground)
          (draw-glyphs button gc label-x label-y label))))))


(defmethod preferred-size ((button button) &key width height border-width)
  (with-slots (font label (bw border-width)) button
    
    ;; Get metrics for label string
    (multiple-value-bind (label-width ascent descent left right font-ascent font-descent)
        (text-extents font label)
      (declare (ignore ascent descent left right))

      (let* ((margin      2)
             (best-width  (+ label-width margin margin))
             (best-height (+  font-ascent font-descent margin margin)))

        ;; Return best geometry for this label
        (values
          (if width  (max width best-width)   best-width)
          (if height (max height best-height) best-height)
          (or border-width bw))))))



;;                                   Actions

(defmethod button-select ((button button))
  (apply-callback button :select))

(defmethod button-set-highlight ((button button) on-p)
  (with-slots (foreground background) button
    (setf (window-border button) (if on-p foreground background))))


;;                             Event translations

(defevent button :button-press button-select)
(defevent button :enter-notify (button-set-highlight t))
(defevent button :leave-notify (button-set-highlight nil))






;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Demonstrations                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+
 

(defun just-say-lisp (host &optional (font-name "fixed"))
  (let* ((display   (open-contact-display 'just-say-lisp :host host))
         (screen    (contact-screen (display-root display)))
         (fg-color  (screen-black-pixel screen))
         (bg-color  (screen-white-pixel screen))

         ;; Create menu
         (menu      (make-contact
                      'menu
                      :parent     display
                      :font       font-name
                      :title      "Please pick your favorite language:"
                      :foreground fg-color
                      :background bg-color))
         (menu-mgr  (menu-manager menu)))    
    
    ;; Create menu items
    (dolist (label '("Fortran" "APL" "Forth" "Lisp"))
      (make-contact
        'button
        :parent     menu-mgr
        :label      label
	:font       font-name
        :foreground fg-color))
       
    ;; Bedevil the user until he picks a nice programming language
    (unwind-protect
        (loop
          ;; Pop up menu at current pointer position
          (multiple-value-bind (x y) (query-pointer (contact-parent menu))
            (let ((choice (menu-choose menu x y)))
              (when (string-equal "Lisp" choice)
                (return)))))      

      (close-display display))))


(defun pick-one (host &rest strings)
  (let* ((display  (open-contact-display 'pick-one :host host))         
         (menu     (make-contact 'menu :parent display :title "Pick one:")))    
    
    ;; Create menu items
    (dolist (string strings)
      (make-contact 'button :parent (menu-manager menu) :label string))    
    
    ;; Set menu callback to return chosen item label
    (add-callback menu :select 'throw-menu-selection menu)
    
    ;; Display the menu so that first item is at x,y
    (initialize-geometry menu)
    (multiple-value-bind (x y) (query-pointer (contact-parent menu))
      (menu-present menu x y))
    
    ;; Event processing loop
    (let ((selected (catch :menu-selection
                     (loop (process-next-event display)))))

      ;; Close server connection
      (close-display display)
    
      ;; Return selected string
      selected)))


(defun resource-menu (host menu-name item-defaults &rest buttons)
  (let*
    ((display (open-contact-display 'resource-menu :host host))         
     (menu    (make-contact 'menu :parent display :name menu-name)))    

    (unwind-protect
	(progn
	  ;; Create menu items
	  (dolist (label buttons)
	    (make-contact 'button
			  :parent   (menu-manager menu)
			  :name     (intern (string label))
			  :label    (format nil "~:(~a~)" label)
			  :defaults item-defaults))    
	  
	  ;; Set menu callback to return chosen item label
	  (add-callback menu :select 'throw-menu-selection menu)
	  
	  ;; Display the menu so that first item is at x,y
	  (initialize-geometry menu)
	  (multiple-value-bind (x y) (query-pointer (contact-parent menu))
	    (menu-present menu x y))
	  
	  ;; Event processing loop --- return selected string.
	  (catch :menu-selection
	    (loop (process-next-event display))))
      
      (close-display display))))


(defun beatlemenuia (host &optional defaults)
  (loop

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Example 1                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

    (define-resources
      (* beatles title) "Who is your favorite Beatle?")
    

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Example 2                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

    (format t "~%Buttons are white-on-black ...")
    
    (define-resources (* button foreground) :white
                      (* button background) :black
                      (* button border)     :white)
    
    (format t " Choice is ~a"
            (resource-menu host 'Beatles defaults 'John 'Paul 'George 'Ringo))
    
    (undefine-resources (* button foreground) :white
                        (* button background) :black
                        (* button border) :white)
    (unless (y-or-n-p "~%Continue?") (return))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Example 3                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

    (format t "~%Use large Courier font everywhere ...")
    
    (define-resources (resource-menu * font) "*courier-bold-r-normal--24*")
    
    (format t " Choice is ~a"
            (resource-menu host 'Beatles defaults 'John 'Paul 'George 'Ringo))
    
    (undefine-resources (resource-menu * font) "*courier-bold-r-normal--24*")
    (unless (y-or-n-p "~%Continue?") (return))    


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Example 4                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

    (format t "~%Use gray background in menu ...")
    
    (define-resources (* beatles * background) .8)
    
    (format t " Choice is ~a"
            (resource-menu host 'Beatles defaults 'John 'Paul 'George 'Ringo))
    
    (undefine-resources (* beatles * background) .8)
    (unless (y-or-n-p "~%Continue?") (return))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Example 5                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

    (format t "~%Only John uses large Courier, Ringo uses gray background ...")
    
    (define-resources (* John font)        "*courier-bold-r-normal--24*"
                      (* Ringo background) "50%gray")

    (format t " Choice is ~a"
            (resource-menu host 'Beatles defaults 'John 'Paul 'George 'Ringo))

    (undefine-resources (* John font)        "*courier-bold-r-normal--24*"
                        (* Ringo background) "50%gray")    
    (unless (y-or-n-p "~%Continue?") (return))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Example 6                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

    (format t "~%Select only with :button-3 ...")
    
    (define-resources (* button event-translations)
                      '(((:button-press :button-3) button-select)
                        ((:button-press :button-1) ignore-action)
                        ((:button-press :button-2) ignore-action)))
    
    (format t " Choice is ~a"
            (resource-menu host 'Beatles defaults 'John 'Paul 'George 'Ringo))
    
    (undefine-resources (* button event-translations)
                      '(((:button-press :button-3) button-select)
                        ((:button-press :button-1) ignore-action)
                        ((:button-press :button-2) ignore-action)))
    (unless (y-or-n-p "~%Continue?") (return))))


