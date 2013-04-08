;;; -*- Mode:Common-Lisp; Package:CLIO-EXAMPLES; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
;;;                                                                                  |
;;;                Copyright (C) 1990 Texas Instruments Incorporated.                |
;;;                              All Rights Reserved                                 |
;;;                                                                                  |
;;; Use, duplication, or disclosure by the Government is subject to  restrictions as |
;;; set forth in subdivision (b)(3)(ii) of the Rights in Technical Data and Computer |
;;; Software clause at 52.227-7013.                                                  |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+


(in-package "CLIO-EXAMPLES")

(defconstant
  *sketch-help*
  "SKETCH  PAD  is  a  simple  drawing  editor.   You  can  draw  graphical
primitives by  clicking  the  pointer  button  to  enter  vertex points.
Complete a primitive by double-clicking on the final vertex.

Use the Graphics menu to select the type of primitive drawn.

Use the Attributes dialog to set attributes such as line width  and fill
pattern.

Select Save from the File menu to save the drawing in a file. Select Open
from the File menu to read another drawing for editing.

To quit, select Quit from the File menu.")

(defun sketch (&key (host *default-host*) foreground background width height)
  "A simple picture editor."
  (let*
    (;; Open connection to the host server.
     (display        (OPEN-CONTACT-DISPLAY 'sketch-pad :host host))     

     ;; Determine pixels for foreground and background.
     (screen         (CONTACT-SCREEN (DISPLAY-ROOT display)))
     (foreground     (or foreground (screen-black-pixel screen)))
     (background     (or background (screen-white-pixel screen)))

     ;; Determine initial size of top-level-window.
     (initial-width  (or width  400))
     (initial-height (or height 400))

     ;; Create top-level window
     (top            (MAKE-COMMAND-FRAME
                       :parent    display

                       ;; Initialize standard top-level properties.
                       :wm-title     "Sketch Pad"
                       :wm-user-specified-size-p (and width height)

                       ;; Specify initial geometry.
                       :x          0
                       :y          0
                       :width      initial-width
                       :height     initial-height

                       ;; Set colors.
                       :background background
                       :foreground foreground

                       ;; Content is a scroll-frame containing a sketchpad.
                       :content   `(MAKE-SCROLL-FRAME
                                     :content (make-sketchpad
                                                :width  ,initial-width
                                                :height ,initial-height)))))

    (declare (special display top))

    ;; Initialize control buttons.
    (let*
      ((controls (COMMAND-FRAME-CONTROLS top))
       (sketch   (SCROLL-FRAME-CONTENT (COMMAND-FRAME-CONTENT top))))
      
      ;; Build File menu
      (let*
        ((file-menu   (MAKE-MENU
                        :parent     controls
                        :name       :file 
                        :title      "File"))
         (choice      (MENU-CHOICE file-menu)))

        ;; Add control to display File menu.        
        (MAKE-DIALOG-BUTTON
          :parent     controls
          :name       :file 
          :dialog     file-menu
          :label      "File")

        ;; Add File menu items.
        (let ((help-item (MAKE-ACTION-ITEM
                           :parent choice
                           :name   :help
                           :label  "Help")))
          (ADD-CALLBACK
            help-item :release                  ; Present help message when released.
            'CONFIRM-P
            :near        sketch
            :message     *sketch-help*
            :accept-only :on))
        
        ;; Build Open, Save dialogs...
        (let*
          ((open-dialog (MAKE-PROPERTY-SHEET
                          :parent        controls
                          :name          :open 
                          :wm-title      "Sketch Pad Open")) 
           (open-area   (PROPERTY-SHEET-AREA open-dialog))
           
           (save-dialog (MAKE-PROPERTY-SHEET
                          :parent        controls
                          :name          :save 
                          :wm-title      "Sketch Pad Save")) 
           (save-area   (PROPERTY-SHEET-AREA save-dialog)))
          
          ;; Add menu item to display Open dialog.
          (MAKE-DIALOG-ITEM
            :parent     choice
            :name       :open 
            :label      "Open"
            :dialog     open-dialog)
          
          ;; Add menu item to display Save dialog.
          (MAKE-DIALOG-ITEM
            :parent     choice
            :name       :save 
            :label      "Save"
            :dialog     save-dialog) 
          
          ;; Add members to Open dialog...
          (MAKE-DISPLAY-TEXT-FIELD
            :parent open-area :source "Open File:" :display-gravity :east) 

          (let*
            ((initial-path  (nstring-downcase
                              (namestring (make-pathname
                                            :defaults (user-homedir-pathname)
                                            :type "SKETCH"
                                            :name "?")))) 
             (open-field    (MAKE-EDIT-TEXT-FIELD
                              :parent      open-area 
                              :source      initial-path
                              :length      (+ (length initial-path) 12)))
             initial-value) 
            
            ;; Define callback functions for edit-text-field member.
            (flet
              ((open-file
                 ()
                 (with-open-file
                   (in (DISPLAY-TEXT-SOURCE open-field) :direction :input)

                   (setf (sketchpad-picture sketch) (read in))
                   (clear-area sketch :exposures-p t)))
               
               (open-initial-value
                 ()
                 (let*
                   ((source (DISPLAY-TEXT-SOURCE open-field))
                    (name   (pathname-name source))
                    (point  (search name source :test #'char-equal)))
                   (setf initial-value source)
                   (setf (edit-text-point open-field) point)
                   (setf (edit-text-mark open-field) (+ (length name) point))))
               
               (restore-initial-value
                 ()
                 (setf (DISPLAY-TEXT-SOURCE open-field) initial-value)))
              
              (ADD-CALLBACK open-field :initialize #'open-initial-value)
              (ADD-CALLBACK open-field :accept     #'open-file)
              (ADD-CALLBACK open-field :complete   #'dialog-accept open-dialog)
              (ADD-CALLBACK open-field :cancel     #'restore-initial-value))
          
          
            ;; Add members to Save dialog...
            (MAKE-DISPLAY-TEXT-FIELD
              :parent save-area :source "Save File:" :display-gravity :east) 

            (let
              ((save-field (MAKE-EDIT-TEXT-FIELD
                             :parent      save-area))) 
              
              ;; Define callback functions for edit-text-field member.
              (flet
                ((save-file
                   ()
                   (with-open-file
                     (out (DISPLAY-TEXT-SOURCE save-field) :direction :output)

                     (write (sketchpad-picture sketch) :stream out)))
                 
                 (initialize-file
                   ()
                   (let*
                     ((source (DISPLAY-TEXT-SOURCE open-field))
                      (name   (pathname-name source))
                      (point  (search name source :test #'char-equal)))
                     
                     (setf (DISPLAY-TEXT-SOURCE save-field) source)
                     (setf (EDIT-TEXT-FIELD-LENGTH save-field) (+ (length source) 12))
                     (setf (EDIT-TEXT-POINT save-field) point)
                     (setf (EDIT-TEXT-MARK save-field) (+ (length name) point)))))
                
                (ADD-CALLBACK save-field :initialize #'initialize-file)
                (ADD-CALLBACK save-field :accept     #'save-file)
                (ADD-CALLBACK save-field :complete   #'dialog-accept save-dialog)))))
        
        (MAKE-ACTION-ITEM
          :parent    choice
          :name      :quit
          :label     "Quit" 
          :callbacks (list
                       (list :release           ; Exit event loop when released.
                             (list #'(lambda () (throw :quit nil)))))))
      
      ;; Build Graphics menu
      (let*
        ((graphics-menu   (MAKE-MENU
                            :parent     controls
                            :name       :graphics 
                            :title "Graphics"))
         (choice          (MENU-CHOICE graphics-menu)))

        ;; Add control to display Graphics menu.        
        (MAKE-DIALOG-BUTTON
          :parent     controls
          :name       :graphics 
          :dialog     graphics-menu
          :label      "Graphics") 

	(flet
	  ((setf-sketchpad-mode (mode sp) (setf (sketchpad-mode sp) mode)))
	  ;; Add Graphics menu items
	  (MAKE-ACTION-ITEM
	    :parent    choice
	    :name      :line
	    :label     "Line"
	    :callbacks `((:release		; Change to :line mode when released.
			   (,#'setf-sketchpad-mode
			    :line
			    ,sketch))))
	  (MAKE-ACTION-ITEM
	    :parent    choice
	    :name      :polygon
	    :label     "Polygon"
	    :callbacks `((:release		; Change to :polygon mode when released.
			   (,#'setf-sketchpad-mode
			    :polygon
			    ,sketch))))))
      
      ;; Build Attributes dialog
      (let*
        ((attributes-dialog (MAKE-PROPERTY-SHEET
                              :parent        controls
                              :name          :attributes 
                              :wm-title      "Sketch Pad Attributes")) 
         (area              (PROPERTY-SHEET-AREA attributes-dialog)))

        ;; Add control to display Attributes dialog.
        (MAKE-DIALOG-BUTTON
          :parent     controls
          :name       :attributes 
          :label      "Attributes"
          :dialog     attributes-dialog)

        ;; Add members to Attributes dialog...
        ;; ... a slider to change line width...
        (MAKE-DISPLAY-TEXT-FIELD
          :parent area :source "Line Width:" :display-gravity :east)
        (let
          ((slider (MAKE-SLIDER
                     :parent      area
                     :name        :line-width 
                     :minimum     1
                     :maximum     8
                     :increment   1
                     :orientation :horizontal))
           initial-value)
          
          ;; Define callback functions for slider member.
          (flet
            ((set-line-width
               () (setf (sketchpad-line-width sketch) (SCALE-VALUE slider)))
             (save-initial-value
               () (setf initial-value (SCALE-VALUE slider)))
             (restore-initial-value
               () (setf (SCALE-VALUE slider) initial-value)))
            
            (ADD-CALLBACK slider :initialize #'save-initial-value)
            (ADD-CALLBACK slider :accept     #'set-line-width)
            (ADD-CALLBACK slider :cancel     #'restore-initial-value)))
        
        
        ;; ... and "radio-buttons"-style choices to change fill pattern.
        (MAKE-DISPLAY-TEXT-FIELD
          :parent area :source "Fill Pattern:" :display-gravity :east)
        (let
          ((choice (MAKE-CHOICES :parent area :name :fill :choice-policy :always-one))
           initial-selection)

          ;; Define callback functions for choices member.
          (flet
            ((set-fill-pattern
               () (setf (sketchpad-fill sketch)
                        (APPLY-CALLBACK (CHOICE-SELECTION choice) :pattern)))
             (save-initial-selection
               () (setf initial-selection (CHOICE-SELECTION choice)))
             (restore-initial-selection
               () (setf (CHOICE-SELECTION choice) initial-selection)))

            (ADD-CALLBACK choice :initialize #'save-initial-selection)
            (ADD-CALLBACK choice :accept     #'set-fill-pattern)
            (ADD-CALLBACK choice :cancel     #'restore-initial-selection)


            ;; Add choice items for each fill pattern. Define initial selection.
            (MAKE-TOGGLE-BUTTON
              :parent choice :label "White"
              :callbacks `((:pattern (identity 0%gray))))
            (MAKE-TOGGLE-BUTTON
              :parent choice :label "Gray"
              :callbacks `((:pattern (identity 50%gray))))
            (setf (CHOICE-SELECTION choice)
                  (MAKE-TOGGLE-BUTTON
                    :parent choice :label "Black"
                    :callbacks `((:pattern (identity 100%gray))))))))) 
           
    (unwind-protect
        
        ;; Main event loop.
        (catch :quit
          (loop
            (PROCESS-NEXT-EVENT display)))

      ;; Destroy window and close connection to server.
      (close-display display))))


