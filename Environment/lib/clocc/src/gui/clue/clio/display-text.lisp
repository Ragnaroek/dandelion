;;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
;;;                                                                                  |
;;;             Copyright (C) 1990, 1990 Texas Instruments Incorporated.             |
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
	  *default-display-text-font*
	  display-text
	  display-text-alignment
	  display-text-copy
	  display-text-field
	  display-text-font
	  display-text-selection 
	  display-text-source
	  edit-text-mark
	  edit-text-point
	  make-display-text
	  make-display-text-field
	  )
	'clio-open)

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                select-text                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact select-text ()
  ((point :type     text-mark
	  :initform nil
	  :initarg  :point
	  :reader   edit-text-point)		       ; setf defined below
   
   (mark  :type     text-mark
	  :initarg  :mark
	  :initform -1				       ; a value representing "undefined"
	  :reader   edit-text-mark))		       ; setf defined below 
  
  (:resources point mark)

  (:documentation "Text that may be selected interactively."))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Selection Handling                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defgeneric (setf text-selection-displayed-p) (boolean text &optional exposed-x exposed-y exposed-width exposed-height)
  (:documentation "Turn on/off the display of the current TEXT selection."))

(defmethod (setf text-selection-displayed-p) (boolean (text select-text) &optional
					      exposed-x exposed-y exposed-width exposed-height)
  (declare (ignore boolean))
  (multiple-value-bind (from to) (text-selection-range text)
    (when from
      (text-change-highlight text from to exposed-x exposed-y exposed-width exposed-height))))

(defun text-selection-range (text)
  (with-slots (point mark buffer) (the select-text text)
    (multiple-value-bind (start end equal-p) (mark-range buffer point mark)
      (unless equal-p
	(values start end)))))

(defgeneric display-text-copy (text)
  (:documentation "Causes the current TEXT selection to become the :CLIPBOARD selection.
Returns the selected string."))

(defmethod display-text-selection ((text select-text))
  (multiple-value-bind (start end) (text-selection-range text)
    (when start
      (display-text-source text :start start :end end))))

(defgeneric text-change-highlight (text from to &optional exposed-x exposed-y exposed-width exposed-height)
  (:documentation "Turn on/off highlighting of TEXT between the FROM and TO positions."))
  
(defgeneric (setf text-caret-displayed-p)
	    (boolean text &optional exposed-x exposed-y exposed-width exposed-height)
  (:documentation "Turn on/off the display of the TEXT caret."))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Accessors                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmacro while-changing-marks ((text &optional time) &body body)
  `(progn 
     (setf (text-caret-displayed-p ,text) nil)

     ,@body

     ;; Update :primary ownership.
     (text-own-selection ,text :primary ,time)
 
     (setf (text-caret-displayed-p ,text) t)))


(defgeneric (setf edit-text-mark) (new-mark text)
  (:documentation "Change the selection mark for the TEXT."))

(defmethod (setf edit-text-mark) (new-mark (text select-text))
  (with-slots (buffer mark display) text
    (unless (mark-equal new-mark mark)
      (while-changing-marks (text (when (processing-event-p) (with-event (time) time)))
	(text-change-highlight text new-mark mark)
	(setf mark (move-mark mark new-mark)))))
  new-mark)

(defgeneric (setf edit-text-point) (new-point text &key clear-p)
  (:documentation "Change the insert point for the TEXT. If CLEAR-P is true,
then the mark is also changed to clear the current selection."))

(defmethod (setf edit-text-point) (new-point (text select-text) &key clear-p)
  (with-slots (display buffer point mark) text
    (unless (mark-equal new-point point)
      (while-changing-marks (text (when (processing-event-p) (with-event (time) time)))
	(when clear-p (text-change-highlight text new-point mark))
	(text-change-highlight text new-point point)

	;; Update marks
	(setf point (move-mark point new-point))
	(when clear-p (setf mark (move-mark mark new-point))))))
  new-point)


(defmethod (setf display-text-source) :after (new-value (text select-text)
				       &key (start 0) end (from-start 0) from-end)
  (declare (ignore new-value start end from-start from-end)) 
  (with-slots (buffer point mark) text
    ;; Ensure valid insertion point and clear selection.
    
    ;; Note: primary method causes exposure and redisplay, so no need to update
    ;; selection highlighting here.
    (setf mark (move-mark mark (setf point (move-mark point (mark-range buffer point nil)))))))


(defmethod display-text-copy ((text select-text))
  (with-slots (point) text
    (prog1
      (clipboard-copy text)
      (setf (edit-text-mark text) point))))

(defgeneric display-text-selection (text)
  (:documentation "Returns the current TEXT selection."))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                          Initialization                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod initialize-instance :after ((text select-text) &rest initargs)
  (declare (ignore initargs))
  (with-slots (point mark) text
    (setf (edit-text-point text) point)
    (setf (edit-text-mark text) (if (eql -1 mark) point mark))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Event Handling                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defevent select-text :selection-clear                       (text-selection-clear nil))
(defevent select-text :selection-request                     text-selection-request)
(defevent select-text (:button-press   :button-2)            text-adjust-selection)
(defevent select-text (:button-press   :button-1 :meta)      display-text-copy)
(defevent select-text (:button-release :button-1)            text-handle-release)
(defevent select-text (:button-press   :button-1 :none :all) text-start-selection)
(defevent select-text (:motion-notify  :button-1)            text-drag-selection)



(let ((new-mark (make-mark)))

  (defun text-start-selection (text)
    (declare (type select-text text))
    (with-slots (display mark point buffer) (the select-text text)
      (with-event (x y time)
	
	;; Undisplay either caret or current text selection.
	(setf (text-selection-displayed-p text) nil)
	(while-changing-marks (text time)
	  
	  (multiple-value-bind
	    (*current-left* *current-top* *current-width* *current-height* *current-ascent* *current-descent*)
	      (text-geometry text)
	    
	    (declare (special *current-left* *current-top*
			      *current-width* *current-height*
			      *current-ascent* *current-descent*))
	    
	    (let
	      ((new-mark            (text-point-mark text x y new-mark))
	       (*pointer-selection* t)
	       (display             (contact-display text)))
	      (declare (special *pointer-selection*))
	      
	      
	      ;; Initialize selection.
	      (setf
		mark  (move-mark mark new-mark)
		point (move-mark point new-mark))
	      
	      ;; Complete text selection interaction.
	      (catch :pointer-selection
		(loop (process-next-event display)))
	      (apply-callback
		text :point text (buffer-mark-position buffer point))))))))



  (defun text-drag-selection (text)
    (declare (special *pointer-selection*))
    (declare (type select-text text))
    
    (when (boundp '*pointer-selection*)
      (with-event (x y)
	(let ((new-mark (text-point-mark text x y new-mark)))
	  (with-slots (point) (the select-text text)
	    (text-change-highlight text point new-mark)
	    (setf point (move-mark point new-mark)))))))

  (defun text-adjust-selection (text)
    (declare (type select-text text))
    (when (text-selection-range text)
      (with-event (x y)
	(setf (edit-text-point text)
	      (text-point-mark text x y new-mark))
	(with-slots (point buffer) (the select-text text)
	  (apply-callback text :point text (buffer-mark-position buffer point)))))))


(defun text-handle-release (text)
  (declare (ignore text))
  (declare (special *pointer-selection*))
  (when (boundp '*pointer-selection*) 
    (throw :pointer-selection nil)))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Selection Ownership                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun display-selection-owner (display selection)
  "Return the owner window and time of ownership for the SELECTION."
  (let ((entry (getf (getf (display-plist display) 'selections) selection)))
    (values
      (first entry)		  ; Owner window
      (second entry))))		  ; Timestamp

(defsetf display-selection-owner (display selection &optional timestamp) (owner) 
  `(setf-display-selection-owner ,display ,selection ,timestamp ,owner))

(defun setf-display-selection-owner (display selection timestamp owner)
  (assert (or (not owner) timestamp) nil
	  "Must give non-NIL timestamp for ~a to own ~s selection."
	  owner selection)
  (let ((entry (or
		 (getf (getf (display-plist display) 'selections) selection)
		 (setf
		   (getf (getf (display-plist display) 'selections) selection)
		   (list nil nil)))))
    (setf
      (first entry)  owner
      (second entry) timestamp)
    (values owner timestamp)))



(defgeneric text-selection-clear (text selection)
  (:documentation "Perform result of TEXT losing ownership of the given SELECTION."))

(defmethod text-selection-clear ((text contact) selection)
  (with-slots (display) text
    (setf (display-selection-owner display selection) nil)))

(defmethod text-selection-clear ((text contact) (selection null))
  (text-selection-clear
    text
    (with-event ((event-selection selection)) event-selection)))

(defmethod text-selection-clear :after ((text select-text) (selection (eql :primary)))
  (with-slots (point) text
    (setf (edit-text-mark text) point)))



(defun text-selection-request (text)
  (with-event (requestor selection target time property)
    (selection-notify text requestor selection target time property)))

(defun selection-notify (text requestor selection target time property)
  (declare (type select-text text)) 
  (multiple-value-bind (property-name property-value property-format transform)
      (text-convert-selection text selection property target time)
    
    (when property-name 
      ;; Store converted reply property. 
      (change-property
	requestor property-name property-value target property-format :transform transform))
    
    ;; Send :selection-notify to requestor
    (send-event requestor :selection-notify nil
		:window requestor
		:selection selection
		:target target
		:property property-name
		:time time)

    ;; Return nil if failed to convert.
    property-name))

(defgeneric text-convert-selection (text selection property target time)
  (:documentation "Convert SELECTION (if owned by TEXT), to the given
TARGET type. TIME is the time of the conversion request. PROPERTY 
is the name of the property where the converted value should be stored.

Return values are the PROPERTY to be used (nil if conversion refused),
the converted VALUE, the FORMAT of the value, and the TRANSFORM function
to be used with change-property."))

(defmethod text-convert-selection (text selection property target time)
  ;; Default method for converting a SELECTION. Since this is called when
  ;; no other method exists, it always returns NIL to indicate that the SELECTION
  ;; is not supported.
  (declare (ignore text selection target time property))
  nil)

(defmethod text-convert-selection :around (text selection (property null) target time)
  ;; This method ensures conformance with the ICCCM convention that if no
  ;; PROPERTY atom is given by the requestor, the TARGET atom is used as the
  ;; name of the property where the converted value is stored.
  (when (not (eq :multiple target))
    (call-next-method text selection target target time)))

(defmethod text-convert-selection :around ((text contact) selection property (target (eql :multiple)) time)
  (with-event (requestor)
    (with-slots (display) text
      (values
	property

	;; Value returned is list of targets, updated for failed conversions.
	(do*
	  ((conversions
	     (get-property requestor property :transform #'(lambda (atom) (atom-name display atom))))
	   (targets
	     conversions (cddr targets)))
	  
	  ((endp targets)
	   ;; Crock! Have to apply the transform here to avoid a CLX R4.2 bug in change-property.
	   (mapcar
	     ;; Protocol encodes None atoms as 0. Future version of intern-atom should do this.
	     #'(lambda (a) (if a (intern-atom display a) 0))
	     conversions))
	  
	  (let ((target (first targets)) (property (second targets)))
	    (unless (selection-notify text requestor selection target time property)
	      ;; Mark failed target
	      (setf (getf conversions target) nil))))
	
	32))))

(flet
  ((convert-text (text selection property target time)
     (declare (type select-text text))
     (with-slots (display buffer) (the select-text text)
       (multiple-value-bind (owner owner-time) (display-selection-owner display selection)
	 (when
	   (and
	     ;; Selection owned?...
	     (eq text owner)
	     
	     ;; ...and Conversion time is...
	     (or
	       ;; ... CurrentTime?
	       (not time)
	       
	       ;; Crock! Remove this test when CLX maps CurrentTime correctly to nil!!
	       (zerop time)
	       
	       ;; ... later than owner time?
	       (> time owner-time)))
	   
	   ;; Convert successfully?
	   (case target
	     
	     ((:string :text)
	      (values property
		      (text-selection-string text selection)
		      8
		      #'xlib::char->card8))
	     
	     (:timestamp
	      (values property (list owner-time) 32 nil))
	     
	     (:targets
	      (values property
		      '(:string :text :targets :timestamp) 
		      32
		      #'(lambda (a) (intern-atom display a))))))))))

  (defmethod text-convert-selection ((text select-text) (selection (eql :primary)) property target time)
    (convert-text text selection property target time))

  (defmethod text-convert-selection ((text select-text) (selection (eql :clipboard)) property target time)
    (convert-text text selection property target time)))



(defgeneric text-selection-string (text selection)
  (:documentation "Return the TEXT string for the given SELECTION."))

(defmethod text-selection-string ((text select-text) (selection (eql :primary)))
  (with-slots (buffer) text
    (multiple-value-bind (start end) (text-selection-range text)
      (if start (buffer-subseq buffer start end) ""))))




(defgeneric text-own-selection (text selection time)
  (:documentation "Assert TEXT (non)ownership of the SELECTION at the given TIME.
This function should call (setf selection-owner) with owner as TEXT or nil, 
as needed."))

(defmethod text-own-selection ((text contact) selection time)
  (with-slots (display) text
    (or
      ;; Selection already owned?
      (let ((owner (display-selection-owner display selection))) 
	(unless (or (not owner) (eq owner text))
	  ;; Yes, change owner window.
	  (text-selection-clear owner selection)
	  
	  ;; Record new owner window/time.
	  (setf (selection-owner display selection time)
		(setf (display-selection-owner display selection time) text)))
	owner)
      
      ;; Selection ownership acquired?
      (progn
	(unless time
	  ;; Ensure actual timestamp is used, as per ICCCM.
	  (with-event-mode
	    (text
	      `((:property-notify :current-time)
		,#'(lambda (c) (declare (ignore c))
			   (with-event ((event-time time)) (setf time event-time)))))
	    
	    ;; Null property change just to generate a :property-notify timestamp
	    (change-property text :current-time nil :string 8 :mode :append)
	    
	    ;; Wait for :property-notify to be processed.
	    (do () (time) (process-next-event display))))
	
	;; Ownership request successful?
	(when (eq (setf (selection-owner display selection time) text)
		  (selection-owner display selection))
	  ;; Record owner window/time.
	  (setf (display-selection-owner display selection time) text))))))


(defmethod text-own-selection ((text select-text) (selection (eql :primary)) time)
  (with-slots (display point) text
    ;; Need to own selection?
    (if (text-selection-range text)
	
	;; Yes, ownership request successful?
	(or (call-next-method)
	    ;; No, abandon selection.
	    (when (setf (edit-text-mark text) point) nil)) 
	
	;; No, abandon ownership, if necessary
	(multiple-value-bind (owner owner-time) (display-selection-owner display selection)
	  (when (eq text owner)
	    (text-selection-clear text selection)
	    (setf (selection-owner display selection owner-time) nil))))))






;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            display-text-field                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+
 
(defparameter *default-display-text-font*
	      "-*-*-medium-r-*-*-*-*-*-*-*-*-iso8859-1")

(defparameter *default-display-text-field-font*
	      "-*-*-bold-r-*-*-*-*-*-*-*-*-iso8859-1")

(defcontact display-text-field (gravity-mixin core contact)
  ((font      :type     fontable
	      :initform *default-display-text-field-font*
	      :reader   display-text-font	       ; setf defined below
	      :initarg  :font)

   (buffer    :type     (or buffer buffer-line)
	      :initform (make-buffer-line)) 

   (compress-exposures
              :initform :on))
  
  (:resources
    (border-width :initform 0)
    font
    (source :type string :initform ""))

  (:documentation
    "Presents a single line of text for viewing."))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Accessors                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf display-text-font) (new-value (text display-text-field))
  (with-slots (font) text
    (setf font (find-font text new-value)))

  ;; Save original fontname requested. Used again when changing scale.
  (setf (getf (window-plist text) 'fontname) new-value)

  (multiple-value-bind (width height) (preferred-size text)
    (change-geometry text :width width :height height :accept-p t))
  
  (when (realized-p text)
    ;; Delay redisplay until :exposure handling to allow other class methods
    ;; a chance to prepare for redisplay.
    (clear-area text :exposures-p t))
  
  new-value)


(defgeneric display-text-source (text &key start end)
  (:documentation "Return the source substring of TEXT given by START/END."))

(defmethod display-text-source ((text display-text-field) &key (start 0) end)
  (with-slots (buffer) text
    (buffer-subseq buffer start end)))

(defgeneric (setf display-text-source) (new-string text &key start end from-start from-end)
  (:documentation 
     "Replace the source substring of TEXT given by START/END with the
      substring of NEW-STRING given by FROM-START/FROM-END."))

(defmethod (setf display-text-source) (new-value (text display-text-field)
				       &key (start 0) end (from-start 0) from-end)
  (let ((new (string new-value)))
    (with-slots (buffer) text
      (buffer-delete buffer start end)
      (buffer-insert buffer new start :start from-start :end from-end)

      (multiple-value-bind (width height) (preferred-size text)
	(change-geometry text :width width :height height :accept-p t))
      
      (when (realized-p text)
	;; Delay redisplay until :exposure handling to allow other class methods
	;; a chance to prepare for redisplay.
	(clear-area text :exposures-p t))
      
      new-value)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                Initialization                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun make-display-text-field (&rest initargs)
  (apply #'make-contact 'display-text-field initargs))

(defmethod initialize-instance :after ((text display-text-field) &key source &allow-other-keys)
  (setf (display-text-font text) (slot-value text 'font))
  (setf (display-text-source text) source))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                   Display                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod display ((text display-text-field) &optional x y width height &key)
  (declare (ignore x y width height))
  (with-slots (buffer) text
    (multiple-value-bind (x y) (text-base-point text)	    
      (text-display-chars text (buffer-line-chars buffer) x y)
      
      ;; Return base position for other methods to use
      (values x y))))


(defun text-display-chars (text chars x y &key (start 0) end)
  (declare (type display-text-field text))
  (with-slots (font foreground clip-rectangle) (the display-text-field text)
    (let ((sensitive-p (sensitive-p text)))
      (using-gcontext
	(gcontext
	  :drawable   text
	  :font       font
	  :fill-style (unless sensitive-p :stippled)
	  :stipple    (unless sensitive-p (contact-image-mask text 50%gray :depth 1))
	  :foreground foreground
	  :clip-mask  clip-rectangle)
	(draw-glyphs
	  text gcontext x y chars
	  :start start :end end)))))

(defgeneric text-refresh-line (text position &key clear-p base-x base-y)
  (:documentation "Clear and redisplay one line of TEXT, beginning at the given POSITION."))

(defmethod text-refresh-line ((text display-text-field) (position integer) &key (clear-p t) base-x base-y)
  (with-slots (buffer) text
    (unless (and base-x base-y)
      (multiple-value-setq (base-x base-y) (text-base-position text position)))
    
    ;; Clear line
    (when clear-p
      (text-clear-line text base-x base-y))
      
    ;; Redisplay chars
    (text-display-chars
      text (buffer-line-chars buffer)
      base-x base-y :start position)))

(defmethod text-refresh-line ((text display-text-field) (position null) &key (clear-p t) base-x base-y)
  ;; Nothing to do!
  (declare (ignore clear-p base-x base-y)))

(defgeneric text-clear-line (text base-x base-y)
  (:documentation "Clear one line of TEXT, beginning at the given base position."))

(defmethod text-clear-line ((text display-text-field) base-x base-y)
  (with-slots (font) text
    (clear-area
      text
      :x      base-x
      :y      (- base-y (font-ascent font))
      :height (+ (font-ascent font) (font-descent font)))))

(defmethod (setf text-caret-displayed-p) (boolean (text display-text-field)
					  &optional exposed-x exposed-y exposed-width exposed-height)
  ;; No caret displayed for non-editable text.
  (declare (ignore exposed-x exposed-y exposed-width exposed-height))
  boolean)


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                   Geometry                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric display-text-extent (text)
  (:documentation "Return the width, height, ascent, and descent of the TEXT extent rectangle."))

(defmethod display-text-extent ((text display-text-field))
  (with-slots (buffer font) text
    (multiple-value-bind (text-width a d l r font-ascent font-descent)
	(text-extents font (buffer-line-chars buffer))
      (declare (ignore a d l r))      
      (values text-width (+ font-ascent font-descent) font-ascent font-descent))))

(defmethod rescale :before ((text display-text-field))
  (with-slots (font) text
    ;; Find font for new scale, using original fontname requested.
    (setf font (find-font text (getf (window-plist text) 'fontname))))
  (when (realized-p text)
    (clear-area text :exposures-p t)))

(defmethod preferred-size ((text display-text-field) &key width height border-width)
  (with-slots
    ((contact-width width) (contact-height height) (contact-border-width border-width))
    text
    (multiple-value-bind (text-width text-height) (display-text-extent text)
            
      (values
	(max text-width (or width contact-width))
	(max text-height (or height contact-height))
	(or border-width contact-border-width)))))


(defgeneric text-geometry (text)
  (:documentation
    "Return the left, top, width, height, ascent, and descent of the TEXT extent rectangle."))

(defmethod text-geometry ((text display-text-field))
  (declare (special *current-left* *current-top*
		    *current-width* *current-height*
		    *current-ascent* *current-descent*))
  (if (boundp '*current-left*)
      (values  *current-left* *current-top*
	       *current-width* *current-height*
	       *current-ascent* *current-descent*)
      (compute-text-geometry text)))

(defgeneric compute-text-geometry (text))

(defmethod compute-text-geometry ((text display-text-field)) 
  (with-slots (gravity) (the display-text-field text)
    (multiple-value-bind (width height ascent descent) (display-text-extent text)
      (multiple-value-bind (left top)
	  
	  ;; Note: use FLOOR to compute position to be consistent with repositioning
	  ;; according to bit-gravity. Needed for small exposed regions to match
	  ;; up properly when redisplayed.
	  
	  (case gravity
	    (:north-west
	     (values
	       (display-clip-x text)
	       (display-clip-y text)))
	    
	    (:north
	     (values
	       (+ (display-clip-x text) (floor (- (display-clip-width text) width) 2))
	       (display-clip-y text)))
	    
	    (:north-east
	     (values
	       (+ (display-clip-x text) (- (display-clip-width text) width))
	       (display-clip-y text)))
	    
	    (:east
	     (values
	       (+ (display-clip-x text) (- (display-clip-width text) width))
	       (+ (display-clip-y text) (floor (- (display-clip-height text) height) 2))))
	    
	    (:center
	     (values
	       (+ (display-clip-x text) (floor (- (display-clip-width text) width) 2))
	       (+ (display-clip-y text) (floor (- (display-clip-height text) height) 2))))
	    
	    (:west
	     (values
	       (display-clip-x text)
	       (+ (display-clip-y text) (floor (- (display-clip-height text) height) 2))))
	    
	    (:south-east
	     (values
	       (+ (display-clip-x text) (- (display-clip-width text) width))
	       (+ (display-clip-y text) (- (display-clip-height text) height))))
	    
	    (:south
	     (values
	       (+ (display-clip-x text) (floor (- (display-clip-width text) width) 2))
	       (+ (display-clip-y text) (- (display-clip-height text) height))))
	    
	    (:south-west
	     (values
	       (display-clip-x text)
	       (+ (display-clip-y text) (- (display-clip-height text) height)))))
	
	(values left top width height ascent descent)))))


(defun text-base-point (text)
  "Return the left baseline endpoint for the TEXT."
  (multiple-value-bind (left top w h ascent) (text-geometry text)
    (declare (ignore w h))
    (values left (+ top ascent))))

(defgeneric text-base-position (text position)
  (:documentation "Return the left baseline endpoint for the TEXT substring
beginning at the given POSITION."))

(defmethod text-base-position ((text display-text-field) (position integer))
  (with-slots (font buffer) text
    (multiple-value-bind (start-x start-y) (text-base-point text)
      (unless (zerop position)
	(incf start-x (text-width font (buffer-line-chars buffer) :end position)))
      (values start-x start-y))))

(defmethod text-base-position ((text display-text-field) (position null))
  (with-slots (buffer) text
    (text-base-position text (length (buffer-line-chars buffer)))))



(defgeneric text-point-mark (text x y &optional mark)
  (:documentation "Return the text-mark corresponding to the given X/Y point."))


(defmethod text-point-mark ((text display-text-field) x y &optional mark)
  (declare (ignore mark))
  (with-slots (buffer) text
  
    ;; Compute extent.
    (multiple-value-bind (left top width height)
	(text-geometry text) 
 
      (cond
	(;; Return first position if above or left of extent
	 (or (< x left) (< y top))
	 0)
	
	(;; Return end position if below or righ of extent
	 (or (>= x (+ left width)) (>= y (+ top height)))
	 (buffer-length buffer))
    
	(t
	 (text-point-index text nil left x))))))


(defgeneric text-point-index (text line left x)
  (:documentation "Return the index of the character in the given LINE of the TEXT
which is at the given X position. LEFT gives the left edge position of the TEXT extent 
rectangle."))

(let ((char-string (make-string 1))
      (char-index  (make-array 1 :element-type 'card8)))
  

  (defmethod text-point-index ((text display-text-field) line left x)
    (declare (ignore line))
    
    (with-slots (font buffer) text
      (do* ((index-x left)
	    (index   0)
	    (chars  (buffer-line-chars buffer))
	    (max    (length chars)))
	   
	   ((= index max) index)
	
	(setf (elt char-string 0) (elt chars index))
	(translate-default char-string 0 1 font char-index 0)
	(let ((char-width (char-width font (elt char-index 0))))
	  (when (> (+ index-x (pixel-round char-width 2)) x)
	    (return index))
	  
	  (incf index-x char-width)
	  (incf index))))))

(defgeneric text-point-line (text top ascent descent y)
  (:documentation "Return the line of the given Y position for the TEXT, given
the TOP, ASCENT, DESCENT of the TEXT extent rectangle."))


(defmethod text-point-line ((text display-text-field) top ascent descent y)
  (declare (ignore top ascent descent y))
  0)


(defgeneric text-mark-point (text mark)
  (:documentation "Return the X/Y point corresponding to the given TEXT mark."))

(defmethod text-mark-point ((text display-text-field) mark)
  ;; Compute extent.
  (multiple-value-bind (left top w h ascent) 
      (text-geometry text)
    (declare (ignore w h))
    
    (with-slots (buffer font) text
      (values
	(+ left (buffer-text-extents buffer font 0 mark))
	(+ top ascent)))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               display-text                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact display-text (select-text display-text-field)
  ((buffer    :type     buffer
	      :initform (make-buffer))

   (alignment :type     (member :left :center :right)
	      :initform :left
	      :initarg  :alignment
	      :accessor display-text-alignment)

   (extent-top       
              :type     integer)
   (extent-left      
              :type     integer)
   (extent-width
              :type     (integer 0 *))
   (extent-height
              :type     (integer 0 *))

   (compress-exposures
              :initform :off))
  
  (:resources
    alignment
    (font :initform  *default-display-text-font*))

  (:documentation
    "Presents multiple lines of text for viewing."))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Initialization                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun make-display-text (&rest initargs)
  (apply #'make-contact 'display-text initargs))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Accessors                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defun text-extent-defined-p (text)
  (slot-boundp text 'extent-top))

(defsetf text-extent-defined-p (text) (boolean)
  (declare (ignore boolean))
  `(slot-makunbound ,text 'extent-top))

(defmethod (setf display-text-source) :after (new-value (text display-text)
							&key (start 0) end (from-start 0) from-end)
  (declare (ignore new-value start end from-start from-end))
  (setf (text-extent-defined-p text) nil))

(defmethod (setf display-gravity) :after (new-value (text display-text))
  (declare (ignore new-value))
  (setf (text-extent-defined-p text) nil))

(defmethod (setf display-text-font) :after (new-value (text display-text))
  (declare (ignore new-value))
  (setf (text-extent-defined-p text) nil))

(defmethod (setf display-text-alignment) :before (new-value (text display-text))
  (check-type new-value (member :left :center :right)
	      "one of :LEFT, :CENTER, or :RIGHT"))

(defmethod (setf display-text-alignment) :after (new-value (text display-text))
  (declare (ignore new-value))
  (setf (text-extent-defined-p text) nil)
  (when (realized-p text)
    ;; Delay redisplay until :exposure handling to allow other class methods
    ;; a chance to prepare for redisplay.
    (clear-area text :exposures-p t)))

(defmethod (setf display-bottom-margin) :after (new-value (text display-text))
  (declare (ignore new-value))
  (setf (text-extent-defined-p text) nil))

(defmethod (setf display-left-margin) :after (new-value (text display-text))
  (declare (ignore new-value))
  (setf (text-extent-defined-p text) nil))

(defmethod (setf display-right-margin) :after (new-value (text display-text))
  (declare (ignore new-value))
  (setf (text-extent-defined-p text) nil))

(defmethod (setf display-top-margin) :after (new-value (text display-text))
  (declare (ignore new-value))
  (setf (text-extent-defined-p text) nil))

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Display                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod display ((text display-text) &optional exposed-x exposed-y exposed-width exposed-height &key)
  (with-slots (width height) text
    (text-refresh
      text 0 nil nil
      (or exposed-x 0) (or exposed-y 0)
      exposed-width exposed-height))
    
  ;; Display caret or current selection
  (setf (text-selection-displayed-p text exposed-x exposed-y exposed-width exposed-height) t)
  (setf (text-caret-displayed-p text exposed-x exposed-y exposed-width exposed-height) t))


(let ((char-string (make-string 1))
      (char-index  (make-array 1 :element-type 'card8)))
  
  (defun text-clipped-line (text line start end clip-left clip-right)
    "Clip the substring of LINE in TEXT given by START/END to the horizontal region
between CLIP-LEFT and CLIP-RIGHT. Returns the start/end indexes and the left x position
of the clipped substring."
    (declare (type display-text text))
    
    (flet
      ((actual-width (font char)
	(cond
	  ((graphic-char-p char)
	   (setf (elt char-string 0) char)
	   (translate-default char-string 0 1 font char-index 0)
	   (char-width font (elt char-index 0)))
	  (t 0))))
				   
    (with-slots (font buffer) (the display-text text)
      (let*
	((index-x (text-base-x text line))
	 (index   0)
	 
	 (chars   (buffer-line-chars (elt (buffer-lines buffer) line)))
	 (max     (length chars))
	 (end     (if end (min max end) max))

	 (start   (do (char-right)
		      ((= index end) index)
		    
		    ;; Find right edge of next char 
		    (setf char-right (+ index-x (actual-width font (elt chars index))))
		    
		    ;; Is this the first char in start/end subseq that intersects
		    ;; the clip region?
		    (when (and (>= index start) (> char-right clip-left))
		      (return index))
		    
		    ;; Setf index-x to left edge of next char
		    (incf index)
		    (setf index-x char-right)))
	 
	 (start-x index-x))

	(values
	  start
	  
	  ;; Find end of clipped substring.
	  (do ()	      
	      ;; Is this the last char in start/end subseq past that intersects
	      ;; the clip region?
	      ((or (>= index-x clip-right) (= index end)) index)
	    
	    ;; Find left edge of next char.
	    (incf index-x (actual-width font (elt chars index)))
	    (incf index))

	  start-x))))))

(defgeneric text-refresh (text start end &optional clear-p exposed-x exposed-y exposed-width exposed-height)
  (:documentation "Draw TEXT characters from START to END. By default CLEAR-P is true, in which case the displayed 
area is cleared first. If EXPOSED-X, EXPOSED-Y, EXPOSED-WIDTH, and EXPOSED-HEIGHT are given, then 
characters are refreshed only if they lie in the exposed area."))


(defmethod text-refresh ((text display-text) (start mark) (end mark)
			   &optional (clear-p t) (exposed-x 0) (exposed-y 0) exposed-width exposed-height) 
  ;; Update extent cache
  (text-geometry text)
  
  (with-slots (buffer font extent-left extent-top extent-width extent-height width height) text
    
    ;; Redisplay only text inside the exposed region.
    (multiple-value-bind (exposed-x exposed-y exposed-width exposed-height)
	(area-overlaps-p
	  exposed-x exposed-y (or exposed-width (- width exposed-x)) (or exposed-height (- height exposed-y))
	  extent-left extent-top extent-width extent-height)
      
      (when exposed-x
	(let*
	  ((ascent         (font-ascent font))
	   (descent        (font-descent font))
	   (exposed-right  (+ exposed-x exposed-width)) 
	   
	   (sli            (mark-line-index start))
	   (next-line      (max sli
				(text-point-line
				  text extent-top ascent descent
				  exposed-y)))
	   
	   (eli            (mark-line-index end))
	   (end-line       (min eli
				(text-point-line
				  text extent-top ascent descent
				  (+ exposed-y exposed-height))))
	   
	   (nlines         (1+ (- end-line next-line)))
	   (lines          (buffer-lines buffer))
	   (line-height    (+ ascent descent))
	   (base-y         (+ extent-top ascent (* next-line line-height)))) 
	  
	  
	  (dotimes (i nlines)
	    (multiple-value-bind (start end start-x)
		(text-clipped-line text next-line
				   (if (unless clear-p (= next-line sli))
				       (mark-index start)
				       0)
				   (if (unless clear-p (= next-line eli))
				       (mark-index end)
				       nil)
				   exposed-x exposed-right)
	      
	      (text-display-chars
		text (buffer-line-chars (elt lines next-line))
		start-x base-y
		:start start
		:end end)
	      (incf next-line)
	      (incf base-y line-height))))))))


(let ((start-mark (make-mark))
      (end-mark (make-mark)))

  (defmethod text-refresh ((text display-text) start end
			   &optional (clear-p t) (exposed-x 0) (exposed-y 0) exposed-width exposed-height)
    (with-slots (buffer) text
      (text-refresh
	text
	(if (or (null start) (integerp start))
	    (buffer-position-mark buffer start start-mark)
	    start)
	(if (or (null end) (integerp end))
	    (buffer-position-mark buffer end end-mark)
	    end)
	clear-p exposed-x exposed-y exposed-width exposed-height))))



(defmethod text-change-highlight ((text display-text) (from mark) (to mark)
				  &optional exposed-x exposed-y exposed-width exposed-height)
  (when (realized-p text)    
  (with-slots (font buffer foreground  clip-rectangle) text
    
    (multiple-value-bind (from to equal-p) (mark-range buffer from to)
      (unless equal-p
	(let*
	  ((lines       (buffer-lines buffer))
	   (ascent      (font-ascent font))
	   (descent     (font-descent font))
	   (line-height (+ ascent descent))
	   (fli         (mark-line-index from))
	   (tli         (mark-line-index to)))

	  (flet
	    ((draw-highlight
	      (gc)
	      ;; Draw highlight for all chars between from and to marks.
	      (do ((line fli (1+ line))
		   (y (- (text-base-y text fli ascent descent) ascent) (+ y line-height)))
		  ((> line tli))
		
		(let ((start-index (if (= line fli) (mark-index from) 0)))
		  (draw-rectangle
		    text gc
		    (text-base-x text line start-index) y
		    (text-width
		      font (buffer-line-chars (elt lines line))
		      :start start-index
		      :end (when (= line tli) (mark-index to)))
		    line-height
		    t)))))
	    
	    (using-gcontext
	      (gc :drawable   text
		  :function   boole-xor
		  :clip-mask  clip-rectangle
		  :foreground (logxor
				foreground
				(contact-current-background-pixel text)))
	      
	      (if exposed-x
		  
		  ;; Clip highlight to intersection of clip rectangle and exposed region.
		  (let
		    ((old-clip-x      (display-clip-x text))
		     (old-clip-y      (display-clip-y text))
		     (old-clip-width  (display-clip-width text))
		     (old-clip-height (display-clip-height text)))

		    (multiple-value-bind (new-clip-x new-clip-y new-clip-width new-clip-height)
			(area-overlaps-p
			  old-clip-x old-clip-y old-clip-width old-clip-height
			  exposed-x exposed-y exposed-width exposed-height)

		      (when new-clip-x
			(setf
			  (display-clip-x text)      new-clip-x
			  (display-clip-y text)      new-clip-y
			  (display-clip-width text)  new-clip-width
			  (display-clip-height text) new-clip-height)
		    
			;; Draw highlight when intersection exists.
			(with-gcontext (gc :clip-mask clip-rectangle)
			  (draw-highlight gc))
		    
			;; Restore clip rectangle
			(setf (display-clip-x text)      old-clip-x
			      (display-clip-y text)      old-clip-y
			      (display-clip-width text)  old-clip-width
			      (display-clip-height text) old-clip-height))))
		  
		  ;; Else draw highlight without additional clipping
		  (draw-highlight gc))))))))))
	  
	  
;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Geometry                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod resize :after ((text display-text) width height border-width)
  (declare (ignore width height border-width))
  (setf (text-extent-defined-p text) nil))

(defmethod display-text-extent ((text display-text))
  (with-slots (buffer font) text
    (let ((ascent  (font-ascent font))
	  (descent (font-descent font))
	  (lines   (buffer-lines buffer)))
      (values
	(reduce #'(lambda (max-width line)
		    (max max-width (text-width font (buffer-line-chars line))))
		lines :initial-value 0)
	(* (+ ascent descent) (length lines))
	ascent
	descent))))

(defmethod text-geometry ((text display-text))
  (with-slots (font extent-top extent-left extent-width extent-height) text
    (multiple-value-bind (ascent descent)
	(if (text-extent-defined-p text)
	    (values (font-ascent font) (font-descent font))

	    ;; Update extent slots with current geometry.
	    (multiple-value-bind (left top w h a d) (compute-text-geometry text)
	      (setf extent-left   left
		    extent-top    top
		    extent-width  w
		    extent-height h)
	      (values a d)))

      (values extent-left extent-top extent-width extent-height ascent descent))))

(defmethod text-point-mark ((text display-text) x y &optional mark)
  (with-slots (font buffer extent-top extent-left extent-width extent-height) text  
    (let
      ((mark (or mark (make-mark)))
       (line (text-point-line
	       text extent-top
	       (font-ascent font) (font-descent font)
	       (max extent-top (min (1- (+ extent-top extent-height)) y)))))
      (setf (mark-buffer mark) buffer)
      (move-mark mark line (text-point-index text line (text-base-x text line) x)))))

(defun text-base-x (text line &optional (start 0))
  "Return the left edge of the START position of the LINE in the TEXT."
  (declare (type display-text text)
	   (type integer     line))

  ;; Ensure extent is defined.
  (text-geometry text)
  
  (with-slots (font buffer alignment extent-left extent-width) (the display-text text)
    (let ((chars (buffer-line-chars (elt (buffer-lines buffer) line))))
      (+ (ecase alignment
	   (:left   extent-left)
	   (:center (+ extent-left (floor (- extent-width (text-width font chars)) 2)))
	   (:right  (+ extent-left (- extent-width (text-width font chars)))))
	 
	 (if (plusp start)
	     (text-width font chars :end start)
	     0)))))

(defun text-base-y (text line &optional ascent descent)
  "Return the baseline position of the LINE in the TEXT."
  (declare (type display-text text)
	   (type integer     line))

  ;; Ensure extent is defined.
  (text-geometry text) 
  
  (with-slots (font extent-top) (the display-text text)
    (+ extent-top
       (* line (+ (or ascent (setf ascent (font-ascent font)))
		  (or descent (font-descent font))))
       ascent)))

(defmethod text-base-position ((text display-text) (position mark))
  (let ((line (mark-line-index position)))
    (values
      (text-base-x text line (mark-index position))
      (text-base-y text line))))

(let ((mark (make-mark)))
  (defmethod text-base-position ((text display-text) position)
    (with-slots (buffer) text
      (setf (mark-buffer mark) buffer)
      (text-base-position text (buffer-position-mark buffer position mark)))))



(let ((char-string (make-string 1))
      (char-index  (make-array 1 :element-type 'card8)))
  
  (defmethod text-point-index ((text display-text) line left x)
    (with-slots (font buffer) text
      (do* ((index-x left)
	    (index   0 (1+ index))
	    (chars  (buffer-line-chars (elt (buffer-lines buffer) line)))
	    (max    (let* ((max (length chars)) (last (1- max)))
		      ;; Can't point or insert after #\newline!
		      (if (and (plusp max) (eql #\newline (elt chars last))) last max))))
	   
	   ((= index max) index)
	
	(let ((char (elt chars index)))
	  (when (graphic-char-p char)
	    (setf (elt char-string 0) char)
	    (translate-default char-string 0 1 font char-index 0)
	    (let ((char-width (char-width font (elt char-index 0))))
	      (when (> (+ index-x (pixel-round char-width 2)) x)
		(return index))	  
	      (incf index-x char-width))))))))
	   
	   
	   
(defmethod text-point-line ((text display-text) top ascent descent y)
  (floor (- y top) (+ ascent descent)))

(defmethod text-mark-point ((text display-text) (mark mark))
  (with-slots (font buffer extent-top) text
    (let ((ascent (font-ascent font))
	  (line   (mark-line-index mark)))
      (values
	(text-base-x text line (mark-index mark))
	(+ extent-top (* line (+ ascent (font-descent font))) ascent)))))







;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Selection                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod (setf edit-text-point) :before (new-value (text display-text) &key clear-p)
  (declare (ignore new-value clear-p))
  (with-slots (point buffer) text
    ;; Allocate point if necessary.
    (unless (mark-p point) (setf point (make-mark :buffer buffer)))))


(defmethod edit-text-point :around ((text display-text))
  (with-slots (buffer) text
    (buffer-mark-position buffer (call-next-method))))

(let ((mark (make-mark)))
  (defmethod (setf edit-text-point) :around ((new-value integer) (text display-text) &key clear-p)
    (with-slots (buffer) text
      (call-next-method
	(buffer-position-mark buffer new-value mark)
	text :clear-p clear-p))
    new-value))


(defmethod (setf edit-text-mark) :before (new-value (text display-text))
  (declare (ignore new-value))
  (with-slots (mark buffer) text
    ;; Allocate mark if necessary.
    (unless (mark-p mark) (setf mark (make-mark :buffer buffer)))))

(defmethod edit-text-mark :around ((text display-text))
  (with-slots (buffer) text
    (buffer-mark-position buffer (call-next-method))))

(let ((mark (make-mark)))
  (defmethod (setf edit-text-mark) :around ((new-value integer) (text display-text))
    (with-slots (buffer) text
      (call-next-method
	(buffer-position-mark buffer new-value mark)
	text))
    new-value))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Clipboard Handling                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


;;; When text is copied or cut to the Clipboard, then the client becomes the owner 
;;; of the :CLIPBOARD selection.  During this time, the current :CLIPBOARD value is
;;; a string stored on the display plist.  (The current :CLIPBOARD value is unique to
;;; server; in particular, individual text contacts do not need to store this value
;;; independently.) To minimize garbage, the display-clipboard-text is an adjustable
;;; string vector which is reused.

(defmacro display-clipboard-text (display)
  `(getf (display-plist ,display) 'clipboard-text))

(defsetf display-clipboard-text setf-display-clipboard-text)
(defun setf-display-clipboard-text (display string)
  (let
    ;; Create string vector, if necessary.
    ((clipboard (or (display-clipboard-text display)
		    (make-array *minimum-buffer-line-length*
				:adjustable   t
				:fill-pointer 0
				:element-type 'buffer-character)))
     dont-care)

    ;; Delete previous contents.
    (vector-delete clipboard)

    ;; Store new contents and adjust vector, if necessary.
    (when string
      (multiple-value-setq (dont-care clipboard)
	(vector-insert clipboard 0 string 0 (length string)))
      (setf (getf (display-plist display) 'clipboard-text) clipboard)))
  string)


(defun clipboard-copy (text)
  (declare (type display-text-field text))
  
  (with-slots (buffer display) text
    (let ((time (when (processing-event-p) (with-event (time) time)))) 

      (multiple-value-bind (start end) (text-selection-range text)
	(if (and start (text-own-selection text :clipboard time))

	    (setf (display-clipboard-text display) (buffer-subseq buffer start end))
	  
	    (when start
	      (warn "~a ~a.~%~a"
		    "Cannot acquire :CLIPBOARD selection for"
		    text
		    "Text not copied to Clipboard.")))))))

(defmethod text-selection-string ((text select-text) (selection (eql :clipboard))) 
  (with-slots (display) text
    (display-clipboard-text display)))


