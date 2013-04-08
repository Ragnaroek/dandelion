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
	  edit-text
	  edit-text-clear
	  edit-text-cut
	  edit-text-field
	  edit-text-grow
	  edit-text-field-length
	  edit-text-paste

	  make-edit-text
	  make-edit-text-field
	  )
	'clio-open)


(defmacro char-or-keysym (keysym)
  ;; Expands to the character corresponding to the KEYSYM in the
  ;; default global (display-independent) keysym mapping, if any.
  ;; Otherwise, expands to the KEYSYM.
  (let ((mapping (find-if #'(lambda (mapping)
			      ;; Better to use keysym-mapping accessors directly, but in R3 CLX these
			      ;; macros are defined only at compile time.
			      (and (characterp (first mapping))		       ; xlib::keysym-mapping-object
				   (not (second mapping))		       ; xlib::keysym-mapping-mask
				   (not (third mapping))		       ; xlib::keysym-mapping-modifiers
				   (not (fourth mapping))		       ; xlib::keysym-mapping-lowercase
				   (not (fifth mapping))		       ; xlib::keysym-mapping-translate
				   ))

			  (gethash keysym xlib::*keysym->character-map*))))
    `,(if mapping
	(first mapping)							       ; xlib::keysym-mapping-object
	keysym)))



(defconstant
  *default-edit-text-field-command-table*
  (make-text-command-table
    :default                                 'text-insert
    #\rubout                                 'text-rubout
    #\newline                                'text-complete
    #\linefeed                               'text-complete
    (char-or-keysym #.(xlib::keysym 255 83)) '(text-move-point :chars 1)       ; Right Arrow
    (char-or-keysym #.(xlib::keysym 255 81)) '(text-move-point :chars -1)      ; Left Arrow
    (char-or-keysym #.(xlib::keysym 255 82)) 'ignore			       ; Up Arrow
    (char-or-keysym #.(xlib::keysym 255 84)) 'ignore			       ; Down Arrow

    ;; CLISP, KCL or CMUCL do not support char-bits!
    #-(or kcl cmu clisp) #\Control-\y #-(or kcl cmu clisp) 'edit-text-paste
    #-(or kcl cmu clisp) #\Control-\w #-(or kcl cmu clisp) 'edit-text-cut
    #-(or kcl cmu clisp) #\Meta-\w    #-(or kcl cmu clisp) 'display-text-copy
    #-(or kcl cmu clisp) #\Control-\a #-(or kcl cmu clisp) '(text-move-sol)
    #-(or kcl cmu clisp) #\Control-\e #-(or kcl cmu clisp) '(text-move-eol)
    #-(or kcl cmu clisp) #\Control-\k #-(or kcl cmu clisp) '(text-delete-eol)
    ))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                text-editor                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defconstant *i-bar-cursor-index* 152)

(defcontact text-editor ()
  ((commands         :type	     list
		     :initform       (list *default-edit-text-field-command-table*)
		     :initarg	     :commands
		     :accessor	     edit-text-commands)

   (focus-p          :type           boolean
		     :initform       nil
		     :accessor       edit-text-focus-p))

  (:resources
    (cursor :initform *i-bar-cursor-index* :type cursor))

  (:documentation  "Basic behaviors for editing text."))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Event Handling                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defevent text-editor (:button-press :button-1 :control) edit-text-cut)
(defevent text-editor :enter-notify  (change-focus t))
(defevent text-editor :leave-notify  (change-focus nil))
(defevent text-editor :focus-out     (change-focus nil t))
(defevent text-editor :focus-in      (change-focus t t))
(defevent text-editor :key-press     perform-command)

(defun change-focus (text new-value &optional explicit-p)
  (with-event (focus-p kind)
    (when
      (and
	;; Text window actually the one gaining/losing focus?
	(if explicit-p
	    (member kind '(:ancestor :inferior :nonlinear))
	    focus-p)

	;; Actually losing when leaving?
	(or new-value explicit-p (not (eq (input-focus (contact-display text)) text))))

      (setf (edit-text-focus-p text) new-value))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Display                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(defmethod (setf text-caret-displayed-p) (boolean (text text-editor)
					  &optional exposed-x exposed-y exposed-width exposed-height)
  (unless (or (not (realized-p text)) (text-selection-range text))
    (with-slots (focus-p point foreground) text
      (let*
	((scale  (contact-scale text))
	 (caret  (getf *text-caret-dimensions* scale))
	 (offset (text-caret-baseline-offset caret)))

	;; Get image and dimensions for active/inactive caret.
	(multiple-value-bind (width height image)
	    (if focus-p
		(values
		  (text-caret-width caret)
		  (text-caret-height caret)
		  (getf (getf *text-caret-images* :active) scale))

		(values
		  nil
		  (or (text-caret-inactive-height caret) (text-caret-height caret))
		  (getf (getf *text-caret-images* :inactive) scale)))

	  ;; Adjust amount of image to copy.
	  (setf width  (or width height)
		height (min height (+ (text-caret-descent text scale) offset)))

	  ;; Copy image pixmap.
	  (multiple-value-bind (x y) (text-base-position text point)
	    (using-gcontext (gc :drawable text :function boole-xor :exposures :off)
	      (with-gcontext (gc :clip-mask (when exposed-x (list exposed-x exposed-y exposed-width exposed-height)))
		(copy-area
		  (contact-image-mask
		    text image
		    :foreground (logxor foreground (contact-current-background-pixel text)))
		  gc
		  0 0 width height
		  text
		  (1+ (- x (pixel-round width 2))) (- y offset)))))))))
  boolean)


(defgeneric text-caret-descent (text scale)
  (:documentation "Return the descent of the displayed caret for TEXT."))


(defmethod text-caret-descent ((text text-editor) scale)
  (let ((dimensions (getf *text-caret-dimensions* scale)))
    (- (or (text-caret-inactive-height dimensions)
	   (text-caret-height dimensions))
       (text-caret-baseline-offset dimensions))))


(defmethod compute-text-geometry :around ((text text-editor))
  (with-slots (gravity) text
    (multiple-value-bind (left top width height ascent descent)
	(call-next-method)
      (values
	;; Leave room for caret at end.
	(case gravity
	  ((:north-west :west :south-west)
	   (+ left (pixel-round (text-caret-width (getf *text-caret-dimensions* (contact-scale text))) 2)))
	  ((:north-east :east :south-east)
	   (- left (pixel-round (text-caret-width (getf *text-caret-dimensions* (contact-scale text))) 2)))
	  (otherwise
	   left))
	top width height ascent descent))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Selection                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric edit-text-clear (text)
  (:documentation "Sets the source of the TEXT to the empty string."))

(defmethod edit-text-clear ((text text-editor))
  (setf (display-text-source text) ""))

(defgeneric edit-text-cut (text)
  (:documentation "Causes the TEXT selection to be deleted into the :CLIPBOARD.
Returns the deleted text."))

(defmethod edit-text-cut ((text text-editor))
  (let ((clip (clipboard-copy text)))
    (when clip (text-rubout text))
    clip))

(defgeneric edit-text-paste (text)
  (:documentation "Inserts the :CLIPBOARD into the TEXT and returns the inserted string."))

(defmethod edit-text-paste ((text text-editor))
  (let*
    ((display     (contact-display text))
     (client-clip (display-clipboard-text display))
     (paste
       ;; Does this client own the :CLIPBOARD selection?
       (if (plusp (length client-clip))

	   ;; Yes, get it the easy way.
	   client-clip

	   ;; No, use interclient communication.
	   (flet
	     ((throw-convert (text)
			     (declare (ignore text))
			     (with-event (property) (throw :convert property))))

	     (let ((time (when (processing-event-p) (with-event (time) time))))
	       (with-event-mode (text `(:selection-notify ,#'throw-convert))
		 (convert-selection :clipboard :string text :paste time)

		 ;; Wait for :selection-notify to report result of conversion.
		 (when (catch :convert (loop (process-next-event display)))

		   ;; Conversion successful --- get stored value.
		   (get-property
		     text :paste :result-type 'string

		     ;; The :string target specifies Latin-1 encoding. This happens to correspond
		     ;; to the keysym encoding, hence the following transform function.
		     ;; Note that #'code-char might work on many systems, but this is not guaranteed
		     ;; since Common Lisp does not specify a standard character encoding.

		     :transform #'(lambda (code) (keysym->character display code))))))))))

    (if paste
	(text-insert text paste)
	(bell display))
    paste))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Accessors                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod (setf edit-text-focus-p) :around (new-value (text text-editor))
  (with-slots (focus-p) text
    (let* ((changed-p (if new-value (not focus-p) focus-p))
	   (caret-p   (and changed-p (not (text-selection-range text)))))
      (when caret-p
	(setf (text-caret-displayed-p text) nil))
      (call-next-method)
      (when changed-p
	(when caret-p
	  (setf (text-caret-displayed-p text) t))
	(apply-callback text (if new-value :resume :suspend)))))
  new-value)




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              edit-text-field                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defcontact edit-text-field (text-editor select-text display-text-field)
  ((length           :type	     (or null (integer 0 *))
		     :initform       nil
		     :initarg	     :length
		     :accessor	     edit-text-field-length))

  (:resources
    (font :initform *default-display-text-font*)
    (display-gravity :initform :west)
    length)

  (:documentation  "A single line of editable text."))

(defun make-edit-text-field (&rest initargs)
  (apply #'make-contact 'edit-text-field initargs))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Accessors                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defmethod (setf edit-text-point) :before (new-point (text edit-text-field) &key clear-p)
  (declare (ignore clear-p))
  (check-type new-point (or null (integer 0 *))))

(defmethod (setf edit-text-mark) :before (new-mark (text edit-text-field))
  (check-type new-mark (or null (integer 0 *))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Display                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod display :around ((text edit-text-field) &optional x y width height &key)

  ;; Display underline
  (multiple-value-bind (base-x base-y) (call-next-method)
    (let ((scale (contact-scale text)))
      (with-slots (foreground font length width clip-rectangle) text
	(let*
	  ((underline-y (+ base-y (text-caret-descent text scale)))
	   ;; Length of line reflects max string length
	   (start-x     (if length
			    base-x
			    0))
	   (end-x       (if length
			    (+ base-x
			       (* length
				  ;; Use average char width and hope for the best.
				  (pixel-round (+ (min-char-width font) (max-char-width font)) 2)))
			    width)))

	  (using-gcontext (gc :drawable   text
			      :foreground foreground
			      :clip-mask  clip-rectangle)
	    (draw-line text gc start-x underline-y end-x underline-y))))))

  ;; Display caret, current selection
  (setf (text-selection-displayed-p text x y width height) t)
  (setf (text-caret-displayed-p text) t))

(defmethod text-clear-line ((text edit-text-field) base-x base-y)
  (with-slots (font) text
    (clear-area
      text
      :x      base-x
      :y      (- base-y (font-ascent font))
      :height (+ (font-ascent font) (text-caret-descent text (contact-scale text))))))



(defmethod text-change-highlight ((text edit-text-field) from to
				  &optional exposed-x exposed-y exposed-width exposed-height)
  (when (realized-p text)
    (with-slots (font foreground clip-rectangle) text
    (let ((ascent  (font-ascent font))
	  (descent (font-descent font)))

      (multiple-value-bind (from-x from-y)
	  (text-mark-point text from)
	(let ((to-x (text-mark-point text to)))

	  (using-gcontext
	    (gc :drawable   text
		:function   boole-xor
		:clip-mask  clip-rectangle
		:foreground (logxor
			      foreground
			      (contact-current-background-pixel text)))

	    (if (every #'numberp (list exposed-x exposed-y
				       exposed-width exposed-height))

		;; Clip highlight to intersection of clip rectangle and exposed region.
		(let
		  ((old-clip-x      (display-clip-x text))
		   (old-clip-y      (display-clip-y text))
		   (old-clip-width  (display-clip-width text))
		   (old-clip-height (display-clip-height text)))

		  (setf
		    (display-clip-x text)     (max old-clip-x exposed-x)
		    (display-clip-y text)     (max old-clip-y exposed-y)
		    (display-clip-width text) (- (min (+ exposed-x exposed-width)
						      (+ old-clip-x old-clip-width))
						 (display-clip-x text))
		    (display-clip-height text) (- (min (+ exposed-y exposed-height)
						       (+ old-clip-y old-clip-height))
						  (display-clip-y text)))
		  ;; Does intersection exist?
		  (when (and (plusp (display-clip-width text)) (plusp (display-clip-height text)))
		    (with-gcontext (gc :clip-mask clip-rectangle)
		      (draw-rectangle
			text gc
			(min from-x to-x) (- from-y ascent)
			(abs (- from-x to-x))
			(+ ascent descent)
			t)))

		  ;; Restore clip rectangle
		  (setf (display-clip-x text)      old-clip-x
			(display-clip-y text)      old-clip-y
			(display-clip-width text)  old-clip-width
			(display-clip-height text) old-clip-height))

		;; Else draw highlight without additional clipping
		(draw-rectangle
		  text gc
		  (min from-x to-x) (- from-y ascent)
		  (abs (- from-x to-x))
		  (+ ascent descent)
		  t)))))))))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                   Geometry                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod preferred-size ((text edit-text-field) &key width height border-width)
  (with-slots
    (length font (contact-width width) (contact-height height) (contact-border-width border-width))
    text
    (multiple-value-bind (text-width text-height)
	(if length
	    ;; Prefer to be big enough for length chars (use average char width and hope for the best).
	    (values (* length (pixel-round (+ (min-char-width font) (max-char-width font)) 2))
		    (+ (font-ascent font) (font-descent font)))

	    ;; Else use current source extent.
	    (display-text-extent text))

      (let ((scale (contact-scale text)))
	(values
	  ;; Ensure wide enough to display caret at end.
	  (max (+ text-width (text-caret-width (getf *text-caret-dimensions* scale))) (or width contact-width))

	  ;; Ensure tall enough to display caret and underline.
	  (max (+ text-height (text-caret-descent text scale) 1) (or height contact-height))
	  (or border-width contact-border-width))))))


(defmethod text-caret-descent :around ((text edit-text-field) scale)
  ;; Decrement normal caret height to avoid underline.
  (1- (call-next-method)))


(defmethod display-text-extent :around ((text edit-text-field))
  (multiple-value-bind (width height ascent) (call-next-method)
    (declare (ignore height))
    (let ((descent (1+ (text-caret-descent text (contact-scale text)))))
      (values width (+ ascent descent) ascent descent))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Command Functions                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defun perform-command (edit-text)
  (with-slots (commands) edit-text
    (with-event (character keysym)
      (let ((input (or character keysym)))

	;; Look up command in command table list.
	(multiple-value-bind (command default)
	    (dolist (table commands)
	      (let* ((command (text-command table input))
		     (default (unless command (text-command table :default))))
		(when (or command default)
		  (return (values command default)))))

	  (cond
	    ;; Command found --- call with edit-text and other args.
	    (command
	     (if (listp command)
		 (apply (first command) edit-text (rest command))
		 (funcall command edit-text)))

	    ;; Default command found --- call with edit-text, input, and other args.
	    (default
	     (if (listp default)
		 (apply (first default) edit-text input (rest default))
		 (funcall default edit-text input)))))))))



(defgeneric text-insert (edit-text chars)
  (:documentation "Insert the CHARS into the EDIT-TEXT at the current point
and increment the point."))


(defmethod text-insert ((text text-editor) input)
  ;; If input not character or string, then ignore.
  ;; This case may occur for non-character keysyms like arrow keys.
  (declare (ignore input)))

(defmethod text-insert :around ((text text-editor) (input character))
  ;; Ignore non-graphic characters (e.g. #\Hyper-Q).
  (if (graphic-char-p input)
      (call-next-method)
      (text-insert-nongraphic text input)))


(defgeneric text-insert-nongraphic (text input)
  (:documentation "Insert non-graphic INPUT into the EDIT-TEXT at the current point."))

(defmethod text-insert-nongraphic ((text text-editor) input)
  (declare (ignore input))
  (bell (contact-display text)))



(flet
  ((edit-text-field-insert (text char)
     (declare (type edit-text-field text)
	      (type (or character string) char))
     (with-slots (buffer mark point gravity length) text
       (multiple-value-bind (select-start select-end) (text-selection-range text)

	 ;; Invoke :insert callback
	 (let ((initial-insert-point (or select-start point)))
	   (multiple-value-bind (insert-point char)
	       (apply-callback-else (text :insert text initial-insert-point char)
		 (values initial-insert-point char))

	     (when
	       (or
		 ;; Insertion refused?
		 (not insert-point)

		 ;; Too many chars?
		 (and length (>= (buffer-length buffer) length)
		      (bell (contact-display text)) t))

	       ;; Insertion not allowed.
	       (return-from edit-text-field-insert))

	     ;; If insert point altered, then clear selection and do not delete it.
	     (unless (or (not select-start) (= insert-point initial-insert-point))
	       (setf (edit-text-mark text) point
		     select-start          nil))

	     (while-changing-marks (text)
	       (let*
		 ((clear-all-p
		    (case gravity
		      ((:north-west :west :south-west) select-start)
		      (otherwise                       t)))
		  (clear-position
		    (if clear-all-p 0 insert-point)))

		 ;; Clear before changing source.
		 (multiple-value-bind (base-x base-y) (text-base-position text clear-position)
		   (text-clear-line text base-x base-y)

		   ;; Delete current selection.
		   (when select-start
		     (buffer-delete buffer select-start select-end))


		   ;; Insert new character and move point
		   (let ((new-point (buffer-line-insert buffer char insert-point)))

		     ;; Refresh new line
		     (text-refresh-line
		       text clear-position
		       :clear-p nil
		       :base-x  (unless clear-all-p base-x)
		       :base-y  base-y)

		     ;; Update point, mark.
		     (setf mark (setf point new-point))))))))))))

  (defmethod text-insert ((text edit-text-field) (char character))
    (edit-text-field-insert text char))

  (defmethod text-insert ((text edit-text-field) (char string))
    (edit-text-field-insert text char)))


(defgeneric text-move-point (edit-text &key lines chars)
  (:documentation "Increment the point of the EDIT-TEXT by the
given number of LINES and CHARS."))

(defmethod text-move-point ((text text-editor) &key (lines 0) (chars 0))
  (with-slots (point mark  buffer) text
    (while-changing-marks (text)
      (let ((new-point (buffer-move-mark buffer point :chars chars :lines lines)))
	(if (text-selection-range text)
	    (text-change-highlight text point new-point)
	    (setf mark (move-mark mark new-point)))
	(setf point (move-mark point new-point))))

    (apply-callback text :point text (buffer-mark-position buffer point))))



(defgeneric text-move-sol (edit-text)
  (:documentation "Move to the start of the current line of EDIT-TEXT."))

(defmethod text-move-sol ((text text-editor))
  (with-slots (point buffer) text
    (setf (edit-text-point text :clear-p (not (text-selection-range text)))
	  (buffer-sol buffer point))))

(defgeneric text-move-eol (edit-text)
  (:documentation "Move to the end of the current line of EDIT-TEXT."))

(defmethod text-move-eol ((text text-editor))
  (with-slots (point buffer) text
    (setf (edit-text-point text :clear-p (not (text-selection-range text)))
	  (buffer-eol buffer point))))


(defgeneric text-delete-eol (edit-text)
  (:documentation "Delete to the end of the current line of EDIT-TEXT."))

(defmethod text-delete-eol ((text text-editor))
  (with-slots (point buffer) text
    ;; Select to end of line...
    (setf (edit-text-mark text) (buffer-eol buffer point))

    ;; ...and delete it.
    (text-rubout text)))

(defgeneric text-rubout (edit-text)
  (:documentation "Decrement the current point and delete the character in the EDIT-TEXT
at the new point."))


(defmethod text-rubout ((text edit-text-field))
  (with-slots (point mark gravity buffer) text
    (multiple-value-bind (select-start select-end) (text-selection-range text)

      ;; Attempt to delete non-existent character?
      (if (and (not select-start) point (zerop point))

	  ;; Yes, beep a warning.
	  (bell (contact-display text))

	  ;; No, perform delete.
	  (let ((initial-start (or select-start (buffer-move-mark buffer point :chars -1)))
		(initial-end  (or select-end point)))

	    ;; Invoke :delete callback.
	    (multiple-value-bind (start end)
		(apply-callback-else (text :delete text initial-start initial-end)
		  (values initial-start initial-end))

	      ;; Deletion allowed?
	      (unless start (return-from text-rubout))

	      ;; If delete range altered, then clear selection and do not delete it.
	      (unless (and (= start initial-start) (= end initial-end))
		(setf (edit-text-mark text) point
		      select-start          nil))

	      (let*
		((clear-all-p
		   (case gravity
		     ((:north-west :west :south-west) select-start)
		     (otherwise                       t)))
		 (clear-position
		   (if clear-all-p 0 start)))

		(while-changing-marks (text)
		  ;; Clear before changing source.
		  (multiple-value-bind (base-x base-y) (text-base-position text clear-position)
		    (text-clear-line text base-x base-y)

		    ;; Delete chars and reset point, mark.
		    (buffer-line-delete buffer (setf point (setf mark start)) end)

		    ;; Redisplay chars
		    (text-refresh-line
		      text clear-position
		      :clear-p nil
		      :base-x  (unless clear-all-p base-x)
		      :base-y  base-y))))))))))


(defgeneric text-complete (edit-text)
  (:documentation "Invoke the :complete callback."))

(defmethod text-complete ((text text-editor))
  (multiple-value-bind (verified-p message)
      (apply-callback-else (text :verify text)
	t)

    (if verified-p
	(apply-callback text :complete)

	(confirm-p
	  :near          text
	  :message       (or message "Text changes not accepted.")
	  :accept-only   :on))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 edit-text                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defconstant
  *default-edit-text-command-table*
  (make-text-command-table
    :default                                 'text-insert
    #\rubout                                 'text-rubout
    (char-or-keysym #.(xlib::keysym 255 83)) '(text-move-point :chars 1)   ; Right Arrow
    (char-or-keysym #.(xlib::keysym 255 81)) '(text-move-point :chars -1)  ; Left Arrow
    (char-or-keysym #.(xlib::keysym 255 82)) '(text-move-point :lines -1)  ; Up Arrow
    (char-or-keysym #.(xlib::keysym 255 84)) '(text-move-point :lines 1)  ; Down Arrow

    ;; CLISP, KCL and CMUCL do not support char-bits!
    #-(or kcl cmu clisp) #\Control-\y #-(or kcl cmu clisp) 'edit-text-paste
    #-(or kcl cmu clisp) #\Control-\w #-(or kcl cmu clisp) 'edit-text-cut
    #-(or kcl cmu clisp) #\Meta-\w    #-(or kcl cmu clisp) 'display-text-copy
    #-(or kcl cmu clisp) #\Control-\a #-(or kcl cmu clisp) '(text-move-sol)
    #-(or kcl cmu clisp) #\Control-\e #-(or kcl cmu clisp) '(text-move-eol)
    #-(or kcl cmu clisp) #\Control-\k #-(or kcl cmu clisp) '(text-delete-eol)
    ))

(defcontact edit-text (text-editor display-text)
  ((commands :initform (list *default-edit-text-command-table*)))

  (:resources
    (display-gravity :initform :north-west))


  (:documentation  "Multiple lines of editable text."))

(defun make-edit-text (&rest initargs)
  (apply #'make-contact 'edit-text initargs))





;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Command Functions                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(let ((insert-start (make-mark))
      (insert-mark  (make-mark)))
  (flet
    ((edit-text-insert (text string)
       (declare (type edit-text text)
		(type (or character string) string))

       (with-slots (buffer mark point font gravity alignment extent-left extent-width) text
	 (multiple-value-bind (select-start select-end) (text-selection-range text)

	   ;; Initialize insert mark.
	   (move-mark insert-start (or select-start point))

	   ;; Invoke :insert callback, if necessary
	   (multiple-value-bind (insert-pos string)
	       (apply-callback-else
		 (text :insert text (buffer-mark-position buffer insert-start) string)
		 (values t string))

	     ;; Insert allowed?
	     (unless insert-pos (return-from edit-text-insert))

	     ;; New insert position returned?
	     (unless (eq insert-pos t)
	       ;; Yes, convert to insert mark.
	       (buffer-position-mark buffer insert-pos insert-start))

	     ;; If insert point altered, then clear selection and do not delete it.
	     (unless (or (not select-start) (mark-equal insert-start select-start))
	       (setf (edit-text-mark text) point
		     select-start          nil))

	     (while-changing-marks (text)
	       (let ((small-delete-p
		       (cond
			 (select-start
			  ;; Delete current selection, if any.
			  (buffer-delete buffer select-start select-end)

			  ;; Return true if delete limited to one line.
			  (= (mark-line-index select-end) (mark-line-index select-start)))

			 (:else
			  t))))

		 ;; Insert new string and move insert mark
		 (move-mark insert-mark insert-start)
		 (buffer-insert buffer string insert-mark)
		 (move-mark mark (move-mark point insert-mark))

		 ;; Redisplay is simple and efficient for most common case ---
		 ;; :north-west gravity, :left alignment, and insert/delete affecting only one line.
		 ;; Otherwise, redisplay is simple and inefficient! Replace with more
		 ;; sophisticated algorithm when possible.

		 (multiple-value-bind (refresh-start refresh-end clear-p)
		     (if
		       (or (and (eq gravity :north-west) (eq alignment :left))
			   (and (eq gravity :north-east) (eq alignment :right)))

		       ;; Optimize this case...
		       (let*
			 ((one-line-p  (and small-delete-p
					    (= (mark-line-index insert-mark)
					       (mark-line-index insert-start))))
			  (ascent      (font-ascent font))
			  (descent     (font-descent font))
			  (clear-start (mark-line-index insert-start))
			  (line-height (+ ascent descent)))

			 ;; Clear damaged areas. If multiple lines damaged, just clear to bottom of window.
			 (when (eq alignment :left)
			   ;; This case can be optimized: clear first line only from insert point.
			   (text-clear-line
			     text
			     (text-base-x text clear-start (mark-index insert-start))
			     (text-base-y text clear-start)))

			 (unless (and (eq alignment :left) one-line-p)
			   (when (eq alignment :left)
			     ;; First line already cleared above.
			     (incf clear-start))

			   ;; Clear one or more lines.
			   (clear-area
			     text
			     :x extent-left
			     :y (- (text-base-y text clear-start) ascent)
			     :width extent-width
			     :height (when one-line-p line-height)))

			 ;; If multiple lines damaged, just redisplay to end of buffer.
			 (values insert-start (if one-line-p insert-mark nil) t))

		       ;; Else punt and redisplay everything!  Replace with more efficient
		       ;; algorithm when possible.
		       (progn
			 (clear-area text)
			 (values 0 nil nil)))

		   (setf (text-extent-defined-p text) nil)
		   (text-refresh text refresh-start refresh-end clear-p)))))))))

    (defmethod text-insert ((text edit-text) (input character))
      (edit-text-insert text input))

    (defmethod text-insert ((text edit-text) (input string))
      (edit-text-insert text input))

    (defmethod text-insert-nongraphic ((text edit-text) (char (eql #\newline)))
      (edit-text-insert text char))

    (defmethod text-insert-nongraphic ((text edit-text) (char (eql #\linefeed)))
      (edit-text-insert text #\newline))))


(let ((prev-point (make-mark)))

  (defmethod text-rubout ((text edit-text))
    (with-slots (point mark gravity alignment buffer font extent-left extent-width) text

      (multiple-value-bind (initial-start initial-end) (text-selection-range text)
	;; Attempt to delete non-existent character?
	(if
	  (and (not initial-start) (mark-equal point 0))

	  ;; Yes, beep a warning.
	  (bell (contact-display text))

	  ;; No, perform delete.
	  (while-changing-marks (text)
	    (move-mark prev-point point)

	    ;; Determine initial delete range.
	    (setf initial-start (or initial-start (buffer-move-mark buffer point :chars -1))
		  initial-end   (or initial-end prev-point))

	    ;; Invoke :delete callback to determine actual delete range.
	    (multiple-value-bind (start end)
		(apply-callback-else (text :delete text initial-start initial-end)
		  (values initial-start initial-end))

	      ;; Deletion allowed?
	      (unless start (return-from text-rubout))

	      ;; If delete range altered, then clear selection and do not delete it.
	      (unless (and (mark-equal start initial-start) (mark-equal end initial-end))
		(setf (edit-text-mark text) point))

	      ;; Clear damaged area, delete chars, then redisplay.
	      ;;
	      ;; Redisplay is simple and efficient for most common case ---
	      ;; :north-west gravity, :left alignment, and delete affecting only one line.
	      ;; Otherwise, redisplay is simple and inefficient! Replace with more
	      ;; sophisticated algorithm when possible.

	      (let ((start (move-mark initial-start start))
		    (end   (move-mark initial-end end)))

		;; Clear efficiently, if possible.
		(multiple-value-bind (refresh-start refresh-end clear-p)
		    (if
		      (or (and (eq gravity :north-west) (eq alignment :left))
			  (and (eq gravity :north-east) (eq alignment :right)))

		      ;; Optimize this case...
		      (let*
			((one-line-p  (= (mark-line-index start) (mark-line-index end)))
			 (ascent      (font-ascent font))
			 (descent     (font-descent font))
			 (clear-start (mark-line-index start))
			 (line-height (+ ascent descent)))

			;; Clear damaged areas. If multiple lines damaged, just clear to bottom of window.
			(when (eq alignment :left)
			  ;; This case can be optimized: clear first line only from delete point.
			  (text-clear-line
			    text
			    (text-base-x text clear-start (mark-index start))
			    (text-base-y text clear-start)))

			(unless (and (eq alignment :left) one-line-p)
			  (when (eq alignment :left)
			    ;; First line already cleared above.
			    (incf clear-start))

			  ;; Clear one or more lines.
			  (clear-area
			    text
			    :x extent-left
			    :y (- (text-base-y text clear-start) ascent)
			    :width extent-width
			    :height (when one-line-p line-height)))

			(values start (when one-line-p end) t))

		      ;; Else punt and redisplay everything!  Replace with more efficient
		      ;; algorithm when possible.
		      (progn
			(clear-area text)
			(values 0 nil nil)))

		  ;; Delete chars.
		  (buffer-delete buffer start end)

		  ;; Redisplay buffer.
		  (setf (text-extent-defined-p text) nil)
		  (text-refresh text refresh-start refresh-end clear-p))

		;; Update point and mark.
		(move-mark point (move-mark mark start))))))))))







