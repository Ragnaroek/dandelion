;;; -*- Mode:Lisp; Package:CLUEI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C) 1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;; To do:
;; 1. Need margins

;;; Change History:
;;; ----------------------------------------------------------------------------
;;;  11/09/87   LGO	Created.
;;;  08/17/88	LGO	Added Common-Windows rubout-handler support
;;;  08/22/88   SLM     Display the text cursor position.
;;;  08/23/88   SLM     Toggle solid and hollow text cursor when input focus 
;;;  02/24/89	DNG	When using Explorer CLOS, enable instances of 
;;;			interactive-stream to accept flavor messages from the 
;;;			system I/O functions.
;;;  02/28/89   KK      Updated for CLUE 1.16
;;;  05/06/89	DNG	For the Explorer, update to use the stream generic functions 
;;;			in the TICLOS package, and add support for the :READ-CURSORPOS and 
;;;			:INCREMENT-CURSORPOS messages.
;;;  12/14/89   MAY	Added numerous fixes from myself and others.
;;;  02/12/90   MAY	More fixes for rubout-handler code.


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;  WARNING: Non-portable code! A portable implementation of an interactive-  |
;;;  stream will not be possible until a standard generic function protocol    |
;;;  for Common Lisp streams has been defined. This implementation works       |
;;;  for Explorers and other Lisp machines. It may serve as an example for     |
;;;  other implementations as well.                                            |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(in-package "CLUEI")

#+(and Explorer CLOS)
(import '(ticlos:stream-clear-input
	  ticlos:stream-unread-char
	  ticlos:stream-listen
	  ticlos:stream-read-char
	  ticlos:stream-clear-output
	  ticlos:stream-write-char
	  ticlos:stream-write-string
	  ticlos:stream-fresh-line
	  ))
(export '(interactive-stream
	  stream-clear-input
	  stream-unread-char
	  stream-listen
	  stream-peek-char
	  stream-read-char
	  stream-read-line
	  set-cursorpos
	  stream-clear-output
	  stream-move-cursor
	  stream-write-char
	  clear-line
	  clear-eol
	  stream-write-string
	  text-within-width
	  stream-fresh-line
	  draw-lozenged-string
	  simple-rubout-handler
	  with-input-editing
	  rubout-handler
	  get-rubout-handler-buffer
	  force-input
	  make-interactive-stream ;; may 12/14/89 
	  ))

(defcontact interactive-stream
	    (contact #+(and Explorer CLOS) TICLOS:FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
		     #+(and Explorer CLOS) TICLOS:FUNDAMENTAL-CHARACTER-INPUT-STREAM)
  ((gcontext
     :type     (or null gcontext)
     :initform nil
     :reader   stream-gcontext)
   
   (font
     :type     font
     :reader   stream-font
     :initform 'fixed)
   
   (cursor-x
     :initform 0
     :type     integer
     :reader   stream-cursor-x)
   
   (cursor-y
     :initform 0
     :type     integer
     :reader   stream-cursor-y)
   
   (line-height
     :initform 0
     :type     integer
     :accessor stream-line-height) ; Pixels per character line.

   (tab-width
     :initform 0
     :type     integer
     :accessor stream-tab-width)   ; Number of pixels in a tab

   (lozenge-font
     :type     font
     :accessor stream-lozenge-font
     :initform 'micro)

   (unreadp
     :type     boolean
     :initform nil)		   ; True if a character was unread (already echoed)
   
   ;; more-height:
   ;; When non-nil, then every time a new line is output, this is decremented by
   ;; LINE-HEIGHT.  When this is less than zero, MORE-PROCESSING is called.
   ;; MORE-HEIGHT gets set to the value of (RESET-MORE-HEIGHT stream) in
   ;; STREAM-READ-CHAR, STREAM-CLEAR-OUTPUT and :after REALIZE.
   (more-height
     :initform t
     :type (or boolean card16)
     :accessor stream-more-height)
   
   (rubout-handler-function
     :initform 'simple-rubout-handler	
     :accessor stream-rubout-handler-function)
   
   (output-history-top
     :initform nil)		   ; Output history line at top of window

   (output-history
     :initform nil)		   ; Circular list of strings

   (output-history-size
     :initform 100))
  
  (:resources
    (background :initform :black)
    cursor-x
    cursor-y
    font
    gcontext
    line-height
    lozenge-font
    more-height
    output-history-size
    rubout-handler-function
    tab-width))

(define-resources
  (* interactive-stream width)  400
  (* interactive-stream height) 400)

(defmethod initialize-instance :after ((self interactive-stream) &rest options &aux (between-line-spacing 1))
  (declare (ignore options))
  (with-slots ( output-history output-history-top output-history-size
	       gcontext cursor-y font line-height tab-width) (the interactive-stream self)
    (when (zerop line-height)
      (setf line-height (+ (max-char-ascent font) (max-char-descent font) between-line-spacing)))
    (when (zerop cursor-y) (setf cursor-y (- line-height (max-char-descent font))))
    (when (zerop tab-width)
      (setf tab-width (* 8 (max-char-width font))))
    (setf output-history (make-list output-history-size))
    (setf (cdr (last output-history)) output-history) ;; Make circular
    (setf (car output-history) (make-array 256 :fill-pointer 0 :element-type 'string-char))
    (setf output-history-top output-history)
    ))

(defmethod realize :after ((self interactive-stream))
  ;; Ensure the gcontext is initialized
  (with-slots (gcontext font background) self
    (unless gcontext
      (setf gcontext (create-gcontext :drawable self :font font
				      :background background
				      :foreground (logxor background 1)))))
  (reset-more-height self))

(defevent interactive-stream :key-press stream-save-key)
(defmethod stream-save-key ((stream interactive-stream))
  (with-event (character display)
    (let ((char character))
      (when (characterp char)
	;; translate return to newline.
	#+(and unix allegro)	;; may 12/14/89  added (for al)
	(if (char= char #\return)
	    (setq char #\newline))
	(append-characters display char)))
    t))

(defevent interactive-stream :focus-in (stream-display-cursor t))
(defevent interactive-stream :focus-out (stream-display-cursor nil))
(defmethod stream-display-cursor ((stream interactive-stream) fill-p)
  (with-slots (gcontext cursor-x cursor-y) stream
    (draw-cursor stream cursor-x cursor-y gcontext :erase-p t :fill-p (not fill-p))
    (draw-cursor stream cursor-x cursor-y gcontext :fill-p fill-p)))

(defun draw-cursor (window cursor-x cursor-y gcontext &optional &key (fill-p t) (erase-p nil))
  (let* ((font (gcontext-font gcontext))
	 (width (xlib:max-char-width font))
	 (height (+ (xlib:max-char-ascent font) (xlib:max-char-descent font)))
	 (rectangle-y (- cursor-y (xlib:max-char-ascent font))))
    (when erase-p
      (using-gcontext (gc :drawable window
			  :background (gcontext-foreground gcontext)
			  :foreground (gcontext-background gcontext))
	(draw-rectangle window gc cursor-x rectangle-y width height fill-p))
      (return-from draw-cursor))
    (draw-rectangle window gcontext cursor-x rectangle-y width height fill-p)))

(defmethod stream-clear-input ((self interactive-stream))
  "Clear all input that hasn't been read yet from the current io-buffer."
  (setf (slot-value (the interactive-stream self) 'unreadp) nil)
  (clear-characters (contact-display self))
  (loop  ;; Eat any characters sitting in the event buffer
    (unless (read-character (contact-display self) 0)
      (return nil))))

(defmethod stream-unread-char ((self interactive-stream) character)
  "Put CHARACTER back in the io-buffer so that it will be the next
character returned by ANY-TYI.  Note that CHARACTER must be exactly the
last character that was read, and that it is illegal to do two
unread-char's in a row."
  (setf (slot-value (the interactive-stream self) 'unreadp) t)
  (unread-character (contact-display self) character))

;; for common-windows compatibility
(defun force-input (contact string &key (begin 0) end)
  "Forces the characters from BEGIN to END in STRING into the input buffer for CONTACT."
  (append-characters (contact-display contact) string begin end))

(defmethod stream-listen ((self interactive-stream))
  "If a character is waiting in the current io-buffer, return it
leaving the character in the buffer.  If no character is available, return NIL."
  (listen-character (contact-display self)))

(defmethod stream-peek-char ((self interactive-stream) peek-type)
  (let (char)
    (loop
      (setq char (stream-read-char self))
      (cond ((null peek-type) (return nil))
	    ((eq peek-type t)
	     (unless (member char '(#\space #\tab #\newline)) (return nil)))
	    ((char= char peek-type) (return nil)))
      (stream-unread-char self char))
    char))

#+ti
(defun handle-asynchronous-characters (char contact)
  ;; This handles things like abort, break, system and terminal for TI Explorers
  (let ((entry (assoc char tv:kbd-intercepted-characters)))
    (cond (entry (funcall (second entry) char))
	  ((setq entry (assoc char tv:kbd-global-asynchronous-characters))
	   (funcall (second entry) char contact))
	  ((setq entry (assoc char tv:kbd-standard-asynchronous-characters))
	   (funcall (second entry) char contact)))))

;; may 12/14/89 Added to prevent asynchronous chars, like BREAK from
;; being handled twice.
#+ti
(defun handle-asynchronous-characters-p (char)
  ;; This handles things like abort, break, system and terminal for TI Explorers
  (or (assoc char tv:kbd-intercepted-characters)
      (assoc char tv:kbd-global-asynchronous-characters)
      (assoc char tv:kbd-standard-asynchronous-characters)))

(defmethod stream-read-char ((self interactive-stream))
  "Read a character from the keyboard.
 Echoing is handled by the rubout handler (see rh-read-char)"
  (declare (special *rubout-handler*))
  (reset-more-height self)
  (let (char)
    (cond ((slot-value (the interactive-stream self) 'unreadp)
	   (setf (slot-value (the interactive-stream self) 'unreadp) nil)
	   (setq char (read-character (contact-display self))))
	  ((eq *rubout-handler* 'tyi)
	   (setq char (read-character (contact-display self))))
	  (*rubout-handler*
	   (setq char (rh-read-char self)))
	  (t (setq char (read-character (contact-display self)))
	     ;; Note: characters are not echoed without a rubout handler
	     ))
    #+ti
    ;; may 12/18/89 Ignore asynchronous chars unless we are inside
    ;; the rubout-handler *AND* in rh-read-char which binds *rubout-handler*
    ;; to 'tyi. This fixes the abort key inside the debugger which should return
    ;; to whence it came just like super-a. Similiar to use of eh:*reading-command*
    ;; in (:method ucl-debugger :fetch-input) .
    (when (eq *rubout-handler* 'tyi)
      (handle-asynchronous-characters char self))
    char))

(defmethod reset-more-height ((self interactive-stream))
  (with-slots (height more-height line-height) self
    (when more-height		   ;; Reset MORE height
      (setf more-height (- height (* 2 line-height))))))

(defmethod stream-read-line ((self interactive-stream) &rest make-array-options)
  (do ((line nil (cons (stream-read-char self) line)))
      ((eql (car line) #\newline)
       (let* ((i (1- (length line)))
	      (result (apply #'make-array i :element-type 'string-char
			     make-array-options)))
	 (dolist (c (cdr line))
	   (setf (aref result (decf i)) c))
	 result))))


(defun set-cursorpos (interactive-stream &key x y)
  (with-slots (cursor-x cursor-y gcontext)
	      (the interactive-stream interactive-stream)
    (draw-cursor interactive-stream cursor-x cursor-y gcontext :erase-p t)
    (when x
      (setf cursor-x x))
    (when y
      (setf cursor-y y))
    (draw-cursor interactive-stream cursor-x cursor-y gcontext :fill-p t))
  ;; needs to move a cursor character around
  )


;;;-----------------------------------------------------------------------------
;;; Output

(defvar *no-stream-history-p* nil) ;; Bound to T during display

;; may 12/14/89 from (Aaron Larson) & (sayuri nishimura) & myself
;; 1. Fixes chopped off initial cursor display bug.
;; 2. together with change in simple-rubout-handler-edit fixes missing
;;    initial rubout-handler prompt.
(defmethod display ((self interactive-stream) &optional x y width height &key)
  (declare (ignore x y width height))
  (let ((win self)
	(*no-stream-history-p* t))	;; may 12/14/89 moved here from inside DO
    (clear-area win)
    (with-slots (cursor-x cursor-y line-height font gcontext
			  output-history output-history-top) self
      (setf cursor-x 0
	    cursor-y (- line-height (max-char-descent font)))
      ;;display the cursor
      (draw-cursor self cursor-x cursor-y gcontext :fill-p t)	;; may 12/14/89 had cursor-x twice
      (do ((history output-history-top (cdr history)))
	  ((eq history output-history))
	  (stream-write-string self (car history))
          (stream-move-cursor self 0 (+ cursor-y line-height)))
      (stream-write-string self (car output-history) 0
			   (fill-pointer (car output-history)))
      ;; may 12/14/89 *rhb* may not be bound so below is NOT equivalent to above.
      ;;(prompt-if-any self)
      ;;(stream-write-string self (rhb-buffer *rhb*) 0 (rhb-fill-pointer *rhb*))
      )))

(defmethod stream-clear-output ((self interactive-stream))
  (with-slots ( output-history output-history-size output-history-top
	       font height cursor-x cursor-y more-height line-height gcontext) self
    (do ((i output-history-size (1- i))
	 (history output-history (cdr history)))
	((zerop i))
      (when (car history)
	(setf (fill-pointer (car history)) 0)))
    (setf cursor-x 0
	  cursor-y (- line-height (max-char-descent font)))
    (reset-more-height self)
    (setf output-history-top output-history)
    (clear-area self)
    (draw-cursor self cursor-x cursor-y gcontext))
  nil)

(defmethod stream-move-cursor ((self interactive-stream) &optional x y)
  (with-slots (cursor-x cursor-y gcontext font) self
    (let ((oldx cursor-x)
	  (oldy cursor-y))
      (when x (setf cursor-x x))
      (when y (setf cursor-y y))	;; may 12/14/89
      (let ((newx cursor-x)
	    (newy cursor-y))
	(draw-cursor self oldx oldy gcontext :erase-p t)
	(draw-cursor self newx newy gcontext :fill-p t)
	;; ********************* NEED TO ADD CURSOR OBJECT *****************
	newx newy oldx oldy nil
	))))

;; may 12/14/89 Created (for sn) Keeps output-history manageable and fixes
;; some more-processing bugs.
(defmethod stream-new-line ((self interactive-stream))
  (with-slots (cursor-x cursor-y line-height output-history 
			(contact-height height) more-height font) self
    ;; may 12/14/89 This method replaced form below in some places.
    (SETF CURSOR-X 0
	  CURSOR-Y (+ CURSOR-Y LINE-HEIGHT))
    ;; Free up some space
    (when (and (< 512 (array-total-size (car output-history)))
	       (< (fill-pointer (car output-history)) 256))
      (adjust-array (car output-history) 256))
    (unless *no-stream-history-p*
	    (pop output-history)
	    (unless (car output-history)
		    (setf (car output-history)
			  (make-array 256 :fill-pointer 0 :element-type 'string-char :adjustable t)))
	    (setf (fill-pointer (car output-history)) 0))
    (when (> (+ cursor-y (max-char-descent font)) contact-height)
	  ;; [10/16/89 sn] it is possible to get here because display is called when
	  ;;; the window gets smaller
	  #+ignore
	  (when *no-stream-history-p*
	     (error "EOP during refresh")) ;; should never get here...
	  (end-of-page self))
    (when (and more-height (not *no-stream-history-p*)
	       (minusp (decf more-height line-height)))
	  (more-processing self))))

;; may 12/14/89 (for sn)
;; This fixes the extra cursor in col 0 since now using stream-new-line
;; and bug with control-l when screen had line-wrap.
(defmethod stream-write-char ((self interactive-stream) character)
  (when (integerp character)		   ;; Kludge for old zetalisp code
    (setq character (lisp:int-char character)))
  (with-slots ( cursor-x cursor-y (contact-width width) gcontext font
	       output-history  tab-width lozenge-font) self
    (draw-cursor self cursor-x cursor-y gcontext :erase-p t) 
    (if (graphic-char-p character)
	(let ((width (char-width font (char-int character))))
	  (when (> (+ cursor-x width) contact-width) ;; Wrap on wide lines
	    (let ((*no-stream-history-p* t))	;; may 12/14/89 
	      (stream-new-line self)))		;; may 12/14/89 
	  (draw-glyph self gcontext cursor-x cursor-y (char-int character)
		      :width width :size 8 :translate #'xlib::translate-default)
	    (incf cursor-x width)
	    (unless *no-stream-history-p*
	      (vector-push-extend character (car output-history))))
      (progn ;; Undrawable character
	(case character
	  (#\newline (stream-new-line self))
	  (#\backspace (let ((width (char-width font (font-default-char font))))
			 
		  (setf cursor-x (max 0 (- cursor-x width))))
		(unless *no-stream-history-p*
		  (vector-push-extend character (car output-history))))
	  (#\tab (setf cursor-x (+ cursor-x tab-width))
		 (unless *no-stream-history-p*
		   (vector-push-extend character (car output-history))))
	  (otherwise
	   (unless *no-stream-history-p*
	     (vector-push-extend character (car output-history)))
	   (incf cursor-x
		 (draw-lozenged-string self gcontext cursor-x cursor-y
				       (string (or (char-name character)
						   (format nil "~:@C" (char-int character))))
				       lozenge-font))))))
    (draw-cursor self cursor-x cursor-y gcontext))
  character)

(defmethod end-of-page ((interactive-stream interactive-stream))
  ;; Scroll up one line
  (with-slots (cursor-x cursor-y height width line-height output-history-top (gc gcontext))
	      (the interactive-stream interactive-stream)
    (let* ((bottom-line (min cursor-y height))
	   (clear-height (- height bottom-line)))
      (copy-area interactive-stream gc 0 line-height width bottom-line
		 interactive-stream 0 0)
      (when (plusp clear-height)
	(clear-area interactive-stream :x 0 :y bottom-line
		    :width width :height clear-height))
      (pop output-history-top))
    (decf cursor-y line-height)
    #+IGNORE ;; may 12/14/89 (for sn) This draw-cursor is an extra.
    (draw-cursor interactive-stream cursor-x cursor-y gc)))

(defun more-processing (interactive-stream)
  (reset-more-height interactive-stream)
  (let ((*no-stream-history-p* t))
    (stream-write-string interactive-stream "*** MORE ***")
    (display-force-output (contact-display interactive-stream))
    (stream-read-char interactive-stream)
    (clear-line interactive-stream)))
  
(defun clear-line (interactive-stream)
  ;; Clear the current line
  (setf (slot-value (the interactive-stream interactive-stream) 'cursor-x) 0)
  (clear-eol interactive-stream))

(defun clear-eol (interactive-stream)
  ;; Clear the current line starting at the current cursor-x
  (with-slots (font cursor-x cursor-y line-height width line-height gcontext)
	      (the interactive-stream interactive-stream)
    (clear-area interactive-stream :x cursor-x :y (+ (font-descent font)
						     (- cursor-y line-height))
		:width width :height  line-height)
    (draw-cursor interactive-stream cursor-x cursor-y gcontext)))

;; may 12/14/89 (for sn), Modified to hide output-history for stream-write-char.
(defmethod stream-write-string ((self interactive-stream) string &optional (start 0) end)
  (unless end (setq end (length string)))
  (do ((i start new-end)
       (new-end end end)
       (index 0))
      ((>= i end) string)
    (declare (type integer i)
	     (type (or null integer) index))
    (with-slots ((contact-width width) cursor-x cursor-y font gcontext) self
      (draw-cursor self cursor-x cursor-y gcontext :erase-p t)
      ;; may 12/14/89 Change to wrap on max instead of average and hide newline from history.
      (when (> (+ cursor-x (xlib:max-char-width font)) contact-width) ;; Wrap on wide lines
	(let ((*no-stream-history-p* t))
	  (stream-new-line self)))
      (let ((line-width (- contact-width cursor-x))
	    (string-width 0))
	(multiple-value-setq (string-width index)
	  (text-width font string :start i :end end))
	(when index (setq new-end index))
	(when (> string-width line-width) ;; Clip strings longer than remaing line width
	  (setq new-end (text-within-width line-width font string :start i :end new-end)
		string-width line-width))
	(setq index
	      (draw-glyphs self gcontext cursor-x cursor-y
			   string :start i :end new-end :width string-width))
	(incf cursor-x string-width))
      (draw-cursor self cursor-x cursor-y gcontext)
      ;; do special characters not printed	
      (when index
	(LET ((*no-stream-history-p* t))	;; may 12/14/89 
	  (stream-write-char self (aref string index))
	  (setf new-end (+ index 1))))))
  ;; Save history
  (unless *no-stream-history-p*
    (with-slots (output-history) self
      (let* ((history (car output-history))
	     (j (fill-pointer history))
	     (h (+ j (- end start))))
	;; Grow history if necessary
	(when (> h (array-total-size history))
	  (setq history (adjust-array history (+ h 80)))
	  (setf (car output-history) history))
	(setf (fill-pointer history) h)
	(replace history string :start1 j :end1 h :start2 start :end2 end))))
  )

(defun text-within-width (width font string &key (start 0) end translate)
  "Return an index within STRING such that the string width is less than WIDTH"
  ;; Estimate size
  (do* ((index (+ start (ceiling width (min-char-width font))) (1+ index))
	(stop (or end (length string))))
       ((>= index stop) index)
    (multiple-value-bind (w i)
	(text-width font string :start start :end index :translate translate)
      (when i (return i))
      (when (> w width) (return (1- index))))))

(defmethod stream-fresh-line ((self interactive-stream))
  (with-slots (output-history) self
    (unless (or (null (car output-history))
		(zerop (length (car output-history))))
      (stream-write-char self #\newline))))

;;;--------------------------------------------------------------------------------------------
;;; LOZENGED-STRINGS

;; may 12/14/89 Added for #\rubout calculation.
(defun lozenge-width (char font); gcontext)
  ;; Tied closely to DRAW-LOZENGED-STRING calulations : (let (( .. (wid (+ lozenge-height width)) ..)))
  (let ((ch-name (char-name char)))
    (multiple-value-bind (width ascent descent)
	(text-extents font ch-name)
      (+ ascent descent width))))

;; may 12/14/89 Fix cosmetics.
(defun draw-lozenged-string (window gcontext x0 y0 string font)
  "Display string inside a lozenge at X0 Y0."
  (multiple-value-bind (width ascent descent)	;; may 12/14/89 
      (text-extents font string)
    (let* (;; Put some pixels to the top and bottom of the string and still stay inside lineheight.
	   (lozenge-height (+ ascent descent))	;; may 12/14/89  
	   (wid (+ lozenge-height width))
	   (xpos (+ x0 (floor lozenge-height 2)))	;; may 12/14/89 was ceiling
	   ;;(ypos (+ y0 descent))		;; may 12/14/89 
	   )
      ;; Put the string then the box around it.
      (using-gcontext (gc :drawable (contact-root window) :default gcontext :font font)
	(draw-glyphs window gc xpos
		     y0 ;ypos ;; may 12/14/89 
		     string))
      (draw-lozenge window gcontext wid lozenge-height x0 (- y0 ascent 1))	;; may 12/14/89 was 2
      (values wid lozenge-height))))

(defun draw-lozenge (window gcontext width height x y)
  "Draw a hollow lozenge on WINDOW.
 (a LOZENGE is a rectangle whose left and right ends are <pointed>.
A lozenge whose width and height are equal is a diamond shape.)"
  (let* ((hh (floor (1- height) 2))
	 (cy (+ y hh    hh))
	 (cx (+ x width -1)))
    (draw-lines window gcontext (list
                ;;                        ; _
                ;;                        ;/ \  This looks like
                ;;                        ;\_/  what we are drawing
		x         (+ y hh)
		(+  x hh) y		  ;/
		(- cx hh) y		  ; _
		cx        (+ y hh)	  ;  \
		(- cx hh) cy		  ;  /
		(+  x hh) cy		  ; _
		x         (+ y hh)))	  ;\
    ))


;;;-----------------------------------------------------------------------------
;;; Alas, Common-lisp doesn't specify a portable way to make your own stream object.
;;; Here is a zetalisp implementation for lisp machines using clos-kludge.
;;; PLEASE mail an implementation for YOUR lisp to clue-review@dsg.csc.ti.com
#+(and lispm (not clos))
(defun (:property interactive-stream si:named-structure-invoke)
       (method self &rest args
	&aux (operations '( :which-operations :operation-handled-p :send-if-handles
			   :print-self :listen :clear-input :untyi :tyi :line-in
			   :clear-screen :tyo :string-out :fresh-line :rubout-handler :clear-eol)))
  (ecase method
    (:which-operations operations)
    (:operation-handled-p (member (first args) operations))
    (:send-if-handles (when (member (first args) operations)
			(apply self args)))
    (:print-self (format (first args) "#<interactive-stream ~o>" (si:%pointer self)))
    (:listen (apply #'stream-listen self args))
    (:clear-input (apply #'stream-clear-input self args))
    (:untyi (apply #'stream-unread-char self args))
    ((:tyi :any-tyi) (stream-read-char self))
    (:line-in (let ((leader (car args)))
		(stream-read-line self :leader-length (and (numberp leader) leader))))
    (:clear-screen (apply #'stream-clear-output self args))
    (:tyo (apply #'stream-write-char self args))
    (:string-out (apply #'stream-write-string self args))
    (:fresh-line (apply #'stream-fresh-line self args))
    (:clear-eol (apply #'clear-eol self args))
    (:force-output (display-force-output (contact-display self)))
    (:finish (display-finish-output (contact-display self)))
    (:rubout-handler (apply #'stream-rubout-handler self args))

;;    #+ti (:preemptable-read (apply #'stream-rubout-handler self args))
    #+ti (:read-cursorpos (values 0 0))
    #+ti (:process si:current-process)
    ))

#+(and Explorer CLOS)
(progn

;; may 12/14/89 Hook READ function into interactive-stream
(defmethod (interactive-stream :rubout-handler) (options function &rest args)
  ;Args are args to FUNCTION, the first arg being the stream.
  (apply #'stream-rubout-handler zl:self options function args))

;; may 12/14/89 needed for ucl's handle-read-function in break loop
(defmethod (interactive-stream :preemptable-read) (options function &rest args)
  "This just does the same thing as :rubout-handler for now ..."
  ;Args are args to FUNCTION, the first arg being the stream.
  (apply #'stream-rubout-handler zl:self options function args))

(defmethod (interactive-stream :clear-eol) ()
  (clear-eol zl:self))

(defmethod (interactive-stream :line-in) (&optional leader)
  (stream-read-line zl:self :leader-length (and (numberp leader) leader)))

(defmethod ticlos:stream-read-line ((stream interactive-stream))
  (stream-read-line stream))

(defmethod ticlos:stream-read-char-no-hang ((self interactive-stream))
  (and (listen-character (contact-display self))
       (stream-read-char self)))

(defmethod ticlos:stream-force-output ((stream interactive-stream))
  (display-force-output (contact-display stream)))

(defmethod ticlos:stream-finish-output ((stream interactive-stream))
  (display-finish-output (contact-display stream)))

(defmethod ticlos:stream-line-column ((stream interactive-stream))
  (values (zl:send stream :read-cursorpos ':character)))

(defmethod ticlos:stream-start-line-p ((stream interactive-stream))
  (zerop (slot-value stream 'cursor-x)))

(defmethod (interactive-stream :read-cursorpos) (&optional units)
  (declare (notinline char-width))
  (if (eq units ':character)
      (values (round cursor-x (char-width font (char-int #\n)))
	      (round cursor-y line-height))
    ;; else assume pixels
    (values cursor-x cursor-y)))

;; may 12/14/89 Added recording for output history.  Although we
;; (original author, actually) are using the size of #\n to calculate
;; pixels and the actual char #\space to put in history - it is only
;; intended to work for fixed-width fonts.  Variable fonts require a
;; good guess - but anything is better than NO guess at all.
(defmethod (interactive-stream :increment-cursorpos) (dx dy &optional units)
  "UNITS can be :pixel (default) or :character.
This is called by format:format-ctl-tab and anyone else that
won't mind the output history being recorded as the number of space chars
needed to redisplay the same cursor movement, if possible."
  ;; A good test of this is (si:print-herald), then control-l
  (declare (notinline char-width))
  (LET (newx newy
	(ch-width (char-width font (char-int #\n))))
    (if (eq units ':character)
	(SETQ newx (+ cursor-x (* dx ch-width))
	      newy (+ cursor-y (* dy line-height)))
	;; else assume pixels (default)
	(SETQ newx (+ cursor-x dx)
	      newy (+ cursor-y dy)))
    (UNLESS *no-stream-history-p*
      (DOTIMES (i (CEILING (MAX 0 (- newx cursor-x)) ch-width))
	(VECTOR-PUSH-EXTEND #\space (car output-history))))
    (stream-move-cursor zl:self newx newy)))

(defmethod (interactive-stream :process) () si:current-process)

  ) ; end Explorer and CLOS


;;-----------------------------------------------------------------------------
;; A SIMPLE rubbout handler
;;		(what an understatement, but it's something to build on...)


(defmacro with-input-editing ((stream &optional rubout-options) &body body)
  "Execute BODY inside of STREAM's stream-rubout-handler method.
If BODY does input from STREAM, it will be done with rubout processing
if STREAM is an interactive-stream.
RUBOUT-OPTIONS should be the options for the stream-rubout-handler method"
  (unless stream (setq stream '*standard-input*))
  `(stream-rubout-handler ,stream ,rubout-options
			  #'(lambda () ,@body)))

(defun stream-rubout-handler (contact options function &rest args)
  ;; Rubout handling in the zetalisp tradition
  ;; may 12/18/89 Remove any options that are not value pairs such as
  ;; are passed in by eh:command-loop-read i.e., (:activation member (#\end #\return))
  ;; This fixes missing " Eval: " prompt when in debugger.
  (setq options (remove-if-not #'(lambda (pair) (= 2 (length pair))) options))

  (if (typep contact 'interactive-stream)
      (let ((option-plist nil))
	(dolist (option options)
	  (setq option-plist (append option-plist option)))
	(if args
	    (funcall (stream-rubout-handler-function contact) contact option-plist
		     #'(lambda () (apply function args)))
	  (funcall (stream-rubout-handler-function contact) contact option-plist function)))
    (apply function args)))

(defmacro rubout-handler (&rest options &key (stream *terminal-io*) body
			  pass-through do-not-echo help initial-input)
  "Common-windows rubout-hander"
  (declare (ignore pass-through do-not-echo help initial-input))
  (let ((option-plist (copy-list options)))
    (remf option-plist :stream)
    (remf option-plist :body)
    `(rubout-handler-internal ,stream #'(lambda () ,body) ,@option-plist)))

(defun rubout-handler-internal (contact function &rest options)
  ;; Rubout handling in the common-windows tradition
  (if (typep contact 'interactive-stream)
      ;; Note: OPTIONS doesn't have to be copied, even though it's an &rest arg,
      ;; because it's never referenced outside this dynamic scope.
      (funcall (stream-rubout-handler-function contact) contact options function)
    (funcall function)))
;;
;; Rubout-Handler-Buffer
;;
(defstruct (rubout-handler-buffer (:conc-name rhb-))
  (fill-pointer   0)
  (scan-pointer   0)
  (buffer (make-array 128 :element-type 'string-char))
  (options nil))

;; Since it doesn't make much sense for a process to get input from more
;; than one stream at a time, This rubout handler implementation doesn't
;; allocate a rubout-handler-buffer for each stream.  Instead, a cache
;; of buffers is kept, and the rubout-handler-buffer is bound to *rhb*
;; within the scope of the rubout handler, instead of in a slot of the
;; stream.

(defvar *rhb* nil) ;; rubout-handler-buffer
(defvar *rhb-cache* nil) ;; rubout handler buffer cache

(defun allocate-rhb ()
  (or (pop *rhb-cache*)
      (make-rubout-handler-buffer)))

(defun deallocate-rhb (rhb)
  (setf (rhb-options rhb) nil) ;; Zap options for garbage collection
  (push rhb *rhb-cache*))

(defun get-rubout-handler-buffer (stream)
  "Return a string that represents the current state of
 the rubout handler associated with STREAM.
 This must be called from within the BODY passed to rubout handler."
  (declare (ignore stream))
  ;; We keep the rubout-handler-buffer in *RHB*, NOT in a slot of the stream.
  (subseq (rhb-buffer *rhb*) 0 (rhb-fill-pointer *rhb*)))

;; *RUBOUT-HANDLER* keeps track of the state of the rubout handler.  It can have
;; one of the following settings:
;;
;;    NIL	Outside the rubout handler
;;    READ	Inside the rubout-handler but not inside rubout-handler-edit
;;    TYI	Inside rubout-handler-edit
;;
;; This variable is bound back to NIL whenever entering a new listener loop
;; which establishes its own editing context.
(defvar *rubout-handler* nil "Rubout handler state. NIL when not INSIDE the rubout handler")

;; may 12/14/89 From code in simple-rubout-handler.
;; Need to be able to get the width of the prompt string.
(DEFUN prompt-if-any (STREAM &optional prompt)
  "Print prompt on STREAM. *rhb* must be bound.
PROMPT defaults to the :reprompt or :prompt option
in *rhb* options - whichever is found first, if any."
  (SETQ prompt (or prompt
		   (getf (rhb-options *rhb*) :reprompt)
		   (getf (rhb-options *rhb*) :prompt)))
  (cond ((null prompt) nil)	;; may 12/18/89 Return nil not T
	((stringp prompt)
	 (IF stream
	     (stream-write-string stream prompt)
	     prompt)) ;; just return string if stream is nil, as format would do
	#+explorer ;; explorer error-handler hack
	;; old zetalisp required 2 arguments to prompt and read.
	((eq prompt 'sys:prompt-and-read-prompt-function)
	 (funcall prompt stream nil))
	(t
	 (funcall prompt stream))))

;; may 12/18/89 Fix, on read errors, to make rubout code in
;; simple-rubout-handler-edit (that expects the prompt to
;; be displayed on the line ...) to work.
(defun simple-rubout-handler (contact options function)
  ;; A rubout handler in the common-windows tradition
  ;; Options include:
  ;; :full-rubout flag    If the user erases all of the characters then presses
  ;;                      the rubout character once more, control is returned
  ;;			  from the input editor immediately.  Two values are
  ;;			  returned: NIL and FLAG.  In the absence of this option,
  ;;			  the input editor simply waits for more characters.
  ;;
  ;; :prompt string	  string to display or function of one argument (the contact)
  ;; :reprompt string
  ;; :initial-input string
  ;; :initial-input-pointer card16
  (let ((*rhb* (allocate-rhb)))
    (unwind-protect
	(progn
	  (setf (rhb-options *rhb*) options)
	  (setf (rhb-fill-pointer *rhb*) 0)	   ; number of characters in the buffer
	  (setf (rhb-scan-pointer *rhb*) 0)	   ; number of characters sent to application

	  ;; PROMPT option
	  (prompt-if-any contact (getf options :prompt))	;; may 12/14/89 

	  ;; INITIAL-INPUT option
	  (let ((initial-input (getf options :initial-input)))
	    (when initial-input
	      (let* ((initial-input-pointer (getf options :initial-input-pointer 0))
		     (length (- (length initial-input) initial-input-pointer))
		     (size (array-total-size (rhb-buffer *rhb*))))
		(when (> length size)
		  (setf (rhb-buffer *rhb*) (adjust-array (rhb-buffer *rhb*) (+ length size))))
		(replace (rhb-buffer *rhb*) initial-input :start1 initial-input-pointer)
		(setf (rhb-fill-pointer *rhb*) length)
		(stream-write-string contact initial-input initial-input-pointer))))

	  (when (slot-value (the interactive-stream contact) 'unreadp) ; Make sure type ahead is processed
	    (simple-rubout-handler-edit contact)		    ; by rubout handler, not just by TYI
	    (setf (rhb-scan-pointer *rhb*) 0))
	  (do ((*rubout-handler* 'read)	   ;Establish rubout handler
	       #+ti (si:rubout-handler t)  ;Needed for explorer compatability
	       )
	      (nil)
	    (catch 'rubout-handler		; Throw here when rubbing out
	      (progn
		#+cleh				; Hopefully, someday everyone will use this
		(conditions:handler-case
		    (return (funcall function))
		  (error (condition) (princ condition)))
		#+(and lispm (not cleh))
		(si:catch-error			; If a read-error occurs, print a message and loop back
		  (return			; Exit rubbout handler when read function returns
		    (funcall function)))	; Call read function
		#+(and kcl (not cleh))
		(multiple-value-bind (tag value)
		    (si:error-set `(funcall ',function))
		  (unless tag (return value)))
		#+(and excl (not cleh))
		(multiple-value-bind (tag value)
		    (excl:error-set (funcall function) :announce)
		  (unless tag (return value)))
		#-(or cleh lispm kcl excl)
		(return (funcall function))
		;; We come here after read errors (catch-error caught)
		(STREAM-fresh-line contact)		; Echo the rubout handler buffer	;; may 12/14/89 (for sn)
		;; may 12/18/89 Reprint the prompt as spaces since #\rubout expect the line to have printed the prompt
		(let ((prompt (prompt-if-any nil)))
		  (dotimes (i (length prompt)) (princ #\space)))
		(stream-write-string contact (rhb-buffer *rhb*) 0 (rhb-fill-pointer *rhb*))
		(loop (stream-read-char contact))))	; and force user to edit it
	    ;; Come here on throw to 'rubout-handler
	    ;; Maybe return when user rubs all the way back
	    (and (zerop (rhb-fill-pointer *rhb*))
		 (let ((full-rubout-option (getf options :full-rubout)))
		   (and full-rubout-option (return (values nil full-rubout-option)))))))
      (deallocate-rhb *rhb*))))

(defun rh-read-char (contact &aux idx)
  ;; Get the next character from the rubout-handler buffer, or the user
  ;; Called from stream-read-char when *rubout-handler* is 'read
  (cond ((> (rhb-fill-pointer *rhb*)	   ;Return characters from rhb until end of buffer
	    (setq idx (rhb-scan-pointer *rhb*)))
	 (setf (rhb-scan-pointer *rhb*) (1+ idx))
	 (aref (rhb-buffer *rhb*) idx))
	(t (simple-rubout-handler-edit contact)))	;Else, editing the buffer
  )

;; may 12/14/89
;; 1. added lozenge string rubout ability
;; 2. able to rubout newline onto previous line
;; 3. added tab rubout ability
;; 4. added (Aaron Larson) fixes to handle uppercase c-l an c-u chars
;; 5. prevented stream-history for code after display for control-l key
;;    by moving redisplay of prompt and current *rhb* into display function.
;; 6. Allowed BREAK/RESUME to work (mostly) - si:*use-old-break* t or nil
;; 7. added #\clear-screen
;; may 01/03/90 Added ctrl-y code. Fixed rubout on continuation line.
;; 
(defun simple-rubout-handler-edit (contact)
  ;; This is the "guts" of the rubout handler, where the editing occurs
  ;; This needs LOTS more editing commands!
  (do ((rubbed-out-some nil)
       (*rubout-handler* 'tyi)
       (ch))
      (nil) ;; forever
    (setq ch (stream-read-char contact))
    (case ch
      ((#\control-u #\control-U)				;CLEAR-INPUT flushes all buffered input
       (setf (rhb-fill-pointer *rhb*) 0)
       (setq rubbed-out-some t)			;Will need to throw out
       (stream-write-char contact ch)		;Echo and advance to new line
       (stream-write-char contact #\Newline))
      ((#\control-l #\control-L)				;Retype ALL visible area
	 (display contact))	;; may 12/14/89
      ;; may 12/14/89 Added
      #+ti
      ((#\clear-screen)						;Retype buffered input
       (stream-clear-output contact)
       (prompt-if-any contact)
       (stream-write-string contact (rhb-buffer *rhb*) 0 (rhb-fill-pointer *rhb*)))
      #+ti	;; may 01/03/90 Added ctrl-y code from tv:RH-COM-YANK-INPUT-RING & tv:RH-YANK-FROM-HISTORY
      ((#\control-y #\control-Y)		;yank from kill history
       (LET* ((COPY (zwei:string-interval (ZWEI:HISTORY-ELEMENT-SET-YANK-POINTER ZWEI:*KILL-HISTORY* 1)))
	      (STRING (tv:STRING-REMOVE-FONTS COPY))
	      (LEN (LENGTH STRING))
	      (LAST-CHAR (AREF STRING (1- LEN)))
	      (fill-pointer (rhb-fill-pointer *rhb*))
	      (END (IF ;;(AND CHOP
		     (OR (EQL LAST-CHAR #\RETURN) 
			 (EQL LAST-CHAR #\SPACE)
			 (EQL LAST-CHAR #\TAB)
			 (EQL LAST-CHAR #\)))
		     (1- LEN)
		     len)))
	 ;;(RH-INSERT-STRING STRING 0 END T NIL)
	 (stream-write-string contact string 0 end)	;; update display
	 (si:copy-array-portion string 0 end  ;; cram in rhb buffer
				(rhb-buffer *rhb*) fill-pointer (+ len fill-pointer))
	 (setf (rhb-fill-pointer *rhb*) (+ len fill-pointer))	;; update fill-pointer
	 (setf (rhb-scan-pointer *rhb*) 0)  	;; update # of chars sent to application
	 (throw 'rubout-handler t)
	 ))
      (#\Rubout
       (let ((len (rhb-fill-pointer *rhb*)))
	 (unless (zerop len)
	   (setf (rhb-fill-pointer *rhb*) (setq len (1- len)))
	   ;; may 12/14/89 Change output-history, too, for control-l
	   (LET* ((THIS-LINE (CAR (SLOT-VALUE CONTACT 'OUTPUT-HISTORY)))
		  (THIS-LINE-LEN (AND THIS-LINE (LENGTH THIS-LINE))))
	     (WHEN (PLUSP THIS-LINE-LEN)
	       (SETF (FILL-POINTER THIS-LINE) (1- THIS-LINE-LEN))))
	   (LET* ((rub-ch (aref (rhb-buffer *rhb*) len))
		  (new-y (stream-cursor-y contact))
		  (new-x (cond ((OR (char= rub-ch #\newline)
				    (AND (PLUSP (RHB-FILL-POINTER *RHB*)) 	;; Not at start of typein ..
					 (ZEROP (STREAM-CURSOR-X CONTACT))))	;; ... but at start of a wrapped line.       
				(SETQ new-y (MAX 0 (- new-y (stream-line-height contact))))
				;; return new-x at end of previous line
				(+ (text-width (stream-font contact)
					       (rhb-buffer *rhb*) 
					       :start 0 :end (rhb-fill-pointer *rhb*))
				   (text-width (stream-font contact) (prompt-if-any nil))))
			       ((graphic-char-p rub-ch)	;; may 12/14/89 
				(- (stream-cursor-x contact)
				   (char-width (stream-font contact)
					       (char-int rub-ch))))
			       ((char= rub-ch #\tab)
				(- (stream-cursor-x contact)
				   (stream-tab-width contact)))
			       (t (- (stream-cursor-x contact)
				     (lozenge-width rub-ch (stream-lozenge-font contact)))))))	;; may 12/14/89 
	     (set-cursorpos contact :x new-x :y new-y)
	     (clear-eol contact)
	     (setq rubbed-out-some t))
	   (when (zerop len) ;; when all rubbed out
	     (setf (rhb-scan-pointer *rhb*) 0)
	     (throw 'rubout-handler t)))))
      (otherwise
       (if (plusp (lisp:char-bits ch))
	   (bell (contact-display contact)) ;; unknown command
	 ;; Echo character
	 #+ti
	 (when (handle-asynchronous-characters-p ch)
	   ;; Make the reader closure re-read all input from the beginning
	   (setf (rhb-scan-pointer *rhb*) 0)
	   (stream-fresh-line contact)
	   (prompt-if-any contact (GET (rhb-options *rhb*) :reprompt))
	   (THROW 'rubout-handler t))
	 (let ((fill-pointer (rhb-fill-pointer *rhb*)))
	   (stream-write-char contact ch)
	   ;; Put character in buffer, after first ensuring its big enough
	   (when (> (setf (rhb-fill-pointer *rhb*) (1+ fill-pointer))
		    (array-total-size (rhb-buffer *rhb*)))
	     (setf (rhb-buffer *rhb*)
		   (adjust-array (rhb-buffer *rhb*) (* 2 fill-pointer))))
	   (setf (aref (rhb-buffer *rhb*) fill-pointer) ch)
	   (cond (rubbed-out-some
		  ;; Make the reader closure re-read all input from the beginning
		  (setf (rhb-scan-pointer *rhb*) 0)
		  (throw 'rubout-handler t))
		 (t
		  ;; New character at the end of the buffer, just return it.
		  (setf (rhb-scan-pointer *rhb*) (rhb-fill-pointer *rhb*))
		  (return ch)))))))))

;; may 12/14/89 Added function documented in manual but never coded?
(DEFUN cluei:make-interactive-stream (&rest keywords
				&key (type 'interactive-stream)
				&allow-other-keys)
  "Creates and returns an interactive-stream contact STREAM. The contact-display
associated with the returned stream is automatically created. Keywords can include
any that are understood by OPEN-CONTACT-DISPLAY, MAKE-CONTACT for the TYPE of
stream to be created, and for XLIB:CREATE-WINDOW."
  ;; must have a :parent for make-contact
  (SETF (GETF keywords :parent)
	(OR (GETF keywords :parent)
	    ;; may 12/14/89 
	    ;; This does not seem right ? How can we open a display - there may 12/14/89 a display
	    ;; already opened. The doc in the manual says the display is created automatically.
	    ;; I think the :parent had better be a display in the normal case!
	    (open-contact-display
	      'cluei:lisp-listener ;; this is (not?) used by resource database
	      ;; for the width & height of the listener - this is the PARENT of the
	      ;; listener ... 
	      :authorization-data 	(GETF keywords :authorization-data)
	      :authorization-name 	(GETF keywords :authorization-name)
	      :class 			(GETF keywords :class)
	      :before-actions 		(GETF keywords :before-actions)
	      :default-screen 		(OR (GETF keywords :default-screen) 0)
	      :display 			(GETF keywords :display)
	      :host 			(OR (GETF keywords :host)
					    *default-host*
					    "lm")	;; may 12/14/89 This is not portable, but what to use ???
	      :protocol 		(GETF keywords :protocol)
	      :root-class 		(OR (GETF keywords :root-class) 'cluei:root)
	      )))
  (SETF (GETF keywords :name) (OR (GETF keywords :name) 'cluei:listener)) ;; used by resource database
  (PROG1 (APPLY #'make-contact type keywords)
	 ;; Realize it or else the :prompt rubout handler option will cause access
	 ;; gcontext before :exposure when it is created by REALIZE .
	 (update-state (GETF keywords :parent))
	 ))
