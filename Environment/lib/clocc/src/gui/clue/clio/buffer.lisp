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
	  buffer
	  buffer-insert
	  buffer-delete
	  buffer-subseq
	  buffer-length
	  buffer-number-lines

	  mark
	  move-mark
	  )
	'clio-open)


;;; Define base character type for either CLtL or ANSI Common Lisp variants.
(deftype buffer-character ()
  #+(or explorer ansi-common-lisp) 'base-character
  #+cmu 'base-char
  #-(or explorer ansi-common-lisp cmu) 'string-char)

;;; PCL can't specialize methods on structure classes. Use defstruct*
;;; to define such structures.
(defmacro defstruct* (name &rest slots)
  #+(or (not pcl) cmu17)
  `(defstruct ,name ,@slots)
  
  #+(and pcl (not cmu17))
  (flet ((translate-slot (slot &optional initform &key (type t))
	   `(,slot
	     :initform ,initform
	     :type ,type
	     :initarg ,(intern (string slot) (find-package :keyword))
	     :accessor ,(intern (format nil "~a-~a" name slot)))))
    (let ((pred (intern (format nil "~a-P" name))))
      `(progn
	(defclass ,name ()
	  ,(mapcar #'(lambda (x) (apply #'translate-slot x)) slots))
	(defmethod ,pred ((z t))  nil)
	(defmethod ,pred ((z ,name)) t)
	(defun ,(intern (format nil "MAKE-~a" name)) (&rest args)
	  (apply #'make-instance ',name args))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               Vector Functions                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defconstant *vector-adjust-factor* 1.20
  "Factor used to increase the size  of a vector when calling adjust-array.")

(defun vector-insert (vector start &optional from (from-start 0) (count 1))
  "Insert COUNT new elements into VECTOR beginning at the START index.
New elements, if any, are taken from FROM vector, beginning at FROM-START.
The VECTOR index of the end of the inserted elements is returned. The
second return value is the (possibly adjusted) vector."
  (declare (type vector           vector)
	   (type (or null vector) from)
	   (type (integer 0 *)    start from-start count))
  (let* ((start      (min (max start 0) (fill-pointer vector)))
	 (new-length (+ (fill-pointer vector) count))
	 (end        (+ start count)))
    
    ;; Extend vector, if necessary
    (when (> new-length (array-dimension vector 0))
      (setf vector (adjust-array vector (ceiling (* *vector-adjust-factor* new-length))
				 :fill-pointer (fill-pointer vector))))	; Keep fill-pointer.
    (setf (fill-pointer vector) new-length)
    
    ;; Make room for new elements
    (replace vector vector :start1 end :end1 new-length :start2 start)
    
    ;; Insert new elements
    (when from
      (replace vector from :start1 start :start2 from-start :end2 (+ from-start count)))

    (values end vector)))

(defun vector-delete (vector &optional (start 0) (end nil))
  "Deletes the substring from START to END from the VECTOR."
  (let ((start (min (max start 0) (fill-pointer vector))))
    (if end
	;; Delete from middle
	(let* ((end (min (max end start) (fill-pointer vector))))
	  (replace vector vector :start1 start :start2 end)
	  (decf (fill-pointer vector) (- end start)))

	;; Delete from end
	(setf (fill-pointer vector) start))))


(defun vector-append (vector from &optional (from-start 0) from-end)
  (let* ((old-length (fill-pointer vector))
	 (new-length (+ old-length (- (or from-end (length from)) from-start))))
    (when (> new-length (array-dimension vector 0))
      (adjust-array vector new-length :fill-pointer old-length))	; Keep fill-pointer.
    (setf (fill-pointer vector) new-length)
    (replace vector from :start1 old-length :start2 from-start :end2 from-end)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 buffer-line                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defconstant *minimum-buffer-line-length* 10
  "Initial string dimension for a new buffer-line.")

(defstruct* buffer-line
  (chars (make-array *minimum-buffer-line-length*
		     :adjustable   t
		     :fill-pointer 0
		     :element-type 'buffer-character)
	 :type (array buffer-character)))

(defmethod print-object ((buffer-line buffer-line) stream)
  (format stream "#<BUFFER-LINE ~a>" (substitute (code-char 0) #\newline (buffer-line-chars buffer-line))))

(defgeneric buffer-line-insert (buffer-line chars position &key start end)
  (:documentation
    "Inserts the substring of CHARS given by START/END into the BUFFER-LINE at the given POSITION.
Returns the position at the end of the inserted CHARS."))

(defmethod buffer-line-insert ((buffer-line buffer-line) chars (position null) &key (start 0) end)
  (buffer-line-insert
    buffer-line chars (length (buffer-line-chars buffer-line))
    :start start
    :end end))

(defmethod buffer-line-insert ((buffer-line buffer-line) (chars string) (position integer) &key (start 0) end)
  (multiple-value-bind (position chars)
      (vector-insert
	(buffer-line-chars buffer-line) position chars
	start
	(- (or end (length chars)) start))
    (setf (buffer-line-chars buffer-line) chars)
    position))

(defmethod buffer-line-insert ((buffer-line buffer-line) (char character) (position integer) &key (start 0) end)
  (declare (ignore start end))
  (let ((chars (buffer-line-chars buffer-line)))
    (multiple-value-bind (new-position chars) (vector-insert chars position)
      (setf (elt chars position) char)

      (setf (buffer-line-chars buffer-line) chars)
      new-position)))

(defmethod buffer-line-insert ((buffer-line buffer-line) (chars buffer-line) position &key (start 0) end) 
  (buffer-line-insert buffer-line (buffer-line-chars chars) position :start start :end end))

(defun buffer-line-delete (buffer-line &optional (start 0) (end nil))
  "Deletes the substring from START to END from the BUFFER-LINE."
  (vector-delete (buffer-line-chars buffer-line) start end))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                    buffer                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defconstant *minimum-buffer-length* 2
  "Initial dimension of lines array for a new buffer.")

(defstruct* buffer
  (lines (make-array *minimum-buffer-length*
		     :adjustable   t
		     :fill-pointer 0
		     :element-type 'buffer-line)
	 :type array))

(defmethod print-object ((buffer buffer) stream)
  (format stream "#<BUFFER :LENGTH ~d>" (fill-pointer (buffer-lines buffer))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                     mark                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+


(defstruct* mark
  (buffer     nil)
  (line-index  0   :type (integer 0 *))
  (index       0   :type (integer 0 *)))

;; A composite type including all forms of positioning within text
(deftype text-mark () '(or null			; end-of-buffer
			   integer	        ; string array index
			   mark))		; multiline-text mark

(defun mark-line (mark)
  "Return the BUFFER-LINE indicated by the MARK."
  (elt (buffer-lines (mark-buffer mark)) (mark-line-index mark)))

(defgeneric move-mark (mark line &optional index)
  (:documentation "Updates MARK to point to the given LINE/INDEX.
The new MARK is returned."))

(defmethod move-mark ((mark mark) (line mark) &optional index)  
  (setf (mark-buffer mark)     (or (mark-buffer line) (mark-buffer mark)) 
	(mark-line-index mark) (mark-line-index line)
	(mark-index mark)      (or index (mark-index line)))
  mark)

(defmethod move-mark ((mark mark) (position integer) &optional (index nil index-p))
  (if (and index-p (numberp index))
      ;; Then POSITION is a line index and INDEX is a char index. 
      (setf (mark-line-index mark) position
	    (mark-index mark)      index)

      ;; Else POSITION is a buffer index.
      (buffer-position-mark (mark-buffer mark) position mark))
  mark)

(defmethod move-mark (mark new-mark &optional index)
  ;; This method allows move-mark to be used generically for all text-mark's
  (declare (ignore mark))
  (assert (not index) nil
	  "Text mark is not a mark object; only a single new text-mark value may be specified.")
  new-mark)

(defmethod print-object ((mark mark) stream)
  (format stream "#<MARK :LINE ~A :INDEX ~D>"
	  (if (and (mark-buffer mark)
		   (< (mark-line-index mark) (buffer-number-lines (mark-buffer mark))))
	      (mark-line mark)
	      (mark-line-index mark))
	  (mark-index mark)))

(defgeneric mark-equal (mark1 mark2)
  (:documentation "Returns true if the marks point to the same buffer position."))

(defmethod mark-equal ((mark1 mark) (mark2 mark))
  (and (eq (mark-buffer mark1)     (mark-buffer mark2))
       (=  (mark-line-index mark1) (mark-line-index mark2))
       (=  (mark-index mark1)      (mark-index mark2))))

(defmethod mark-equal (mark1 mark2) 
  (eql mark1 mark2))

(let ((mark (make-mark)))
  (defmethod mark-equal ((mark1 mark) mark2) 
    (mark-equal mark1 (buffer-position-mark (mark-buffer mark1) mark2 mark)))
  
  (defmethod mark-equal (mark1 (mark2 mark)) 
    (mark-equal (buffer-position-mark (mark-buffer mark2) mark1 mark) mark2)))

(defgeneric mark-range (buffer mark1 mark2)
  (:documentation "Compares the marks and returns three values: the smaller mark, the larger mark,
and the result of MARK-EQUAL."))

(defmethod mark-range (buffer (mark1 mark) (mark2 mark))
  (declare (ignore buffer))
  (assert (eq (mark-buffer mark1)     (mark-buffer mark2)) nil
	  "~s and ~s point to different buffers.")
  (cond
    ((< (mark-line-index mark1) (mark-line-index mark2))
     (values mark1 mark2 nil))

    ((< (mark-line-index mark2) (mark-line-index mark1))
     (values mark2 mark1 nil))

    ((< (mark-index mark1) (mark-index mark2))
     (values mark1 mark2 nil))

    ((< (mark-index mark2) (mark-index mark1))
     (values mark2 mark1 nil))

    (t
     (values mark1 mark2 t))))

(let ((mark (make-mark)))
  (defmethod mark-range (buffer (mark1 mark) mark2)
    (mark-range buffer mark1 (buffer-position-mark buffer mark2 mark))))

(defmethod mark-range (buffer (mark1 integer) (mark2 integer))
  (declare (ignore buffer))
  (values (min mark1 mark2) (max mark1 mark2) (= mark1 mark2)))

(defmethod mark-range (buffer (mark1 integer) mark2)
  (let ((mark2 (buffer-mark-position buffer mark2)))
    (values (min mark1 mark2) (max mark1 mark2) (= mark1 mark2))))
     
(defmethod mark-range (buffer (mark1 null) mark2)
    (mark-range buffer (buffer-length buffer) mark2))

(defgeneric buffer-mark-position (buffer mark)
  (:documentation "Return a buffer index corresponding to the MARK."))

(defmethod buffer-mark-position (buffer (mark mark))
  "Return a buffer index corresponding to the MARK."
  (declare (ignore buffer))
  (let ((index  0)
	(buffer (mark-buffer mark))
	(line   (mark-line-index mark)))
    (assert buffer nil "Buffer not defined for ~a." mark)
    (dotimes (i line) (incf index (buffer-length (buffer-line buffer i))))
    (incf index (mark-index mark))))

(defmethod buffer-mark-position (buffer (mark integer))
  (declare (ignore buffer))
  mark)

(defmethod buffer-mark-position (buffer (mark null))
  (buffer-length buffer))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Utilities                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun parse-source (source &key (start 0) end)
  "Parse the substring of SOURCE given by START/END, returning:
    1. The index behind the first #\newline, or END (whichever is smaller)
    2. An array of buffer-line's containing all characters 
       between the first and last #\newline's.
    3. The index behind the last #\newline."
  (declare (type string source))

  (let ((first-end (or (position #\newline source :start start :end end) end)))
    (cond
      ((eql first-end end) end)
            
      (:else
       ;; Compute end of first line.
       (incf first-end)
       
       ;; Build internal buffer-line's, if any.
       (multiple-value-bind (buffer-lines tail-start)
	   (do ((next-end first-end) lines) (())
	     (setf start    next-end 
		   next-end (position #\newline source :start start :end end) 
		   next-end (when next-end (1+ next-end)))
	     
	     (unless next-end
	       (return (values lines start)))
	     
	     (unless lines
	       (setf lines (make-array *minimum-buffer-length*
				       :adjustable   t
				       :fill-pointer 0
				       :element-type 'buffer-line)))
	     
	     (let ((buffer-line (make-buffer-line)))
	       (buffer-line-insert buffer-line source 0 :start start :end next-end) 
	       (vector-push-extend buffer-line lines)))
	 (values first-end buffer-lines tail-start))))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                buffer-insert                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-insert (buffer chars position &key start end)
  (:documentation
    "Inserts the substring of CHARS given by START/END into the BUFFER 
at the given POSITION and returns the updated POSITION."))


;; buffer-line methods ----------------------------------------

(defmethod buffer-insert ((buffer buffer-line) chars (position mark) &key (start 0) end)
  (assert (eq buffer (mark-buffer position)) nil "~s is not a mark for ~s." position buffer)
  (move-mark position 0 (buffer-insert buffer chars (mark-index position) :start start :end end)))

(defmethod buffer-insert ((buffer buffer-line) chars (position null) &key (start 0) end)
  (buffer-insert buffer chars (length (buffer-line-chars buffer)) :start start :end end))

(defmethod buffer-insert ((buffer buffer-line) (chars string) (position integer) &key (start 0) end)
  (assert (not (find #\newline chars :start start :end end)) ()
	  "Can't insert #\NEWLINE into a one-line buffer.")
  (buffer-line-insert buffer chars position :start start :end end))

(defmethod buffer-insert ((buffer buffer-line) (char character) (position integer) &key (start 0) end)
  (declare (ignore start end))
  (assert (not (eql char #\newline)) ()
	  "Can't insert #\NEWLINE into a one-line buffer.")
  (buffer-line-insert buffer char position))


;; buffer methods ----------------------------------------------

(defmethod buffer-insert ((buffer buffer) (chars string) (position mark) &key (start 0) end)
  (assert (eq buffer (mark-buffer position)) nil "~s is not a mark for ~s." position buffer)
  
  (when (plusp (length chars))
    (multiple-value-bind (head lines tail) (parse-source chars :start start :end end)
      (let*
	((line        (mark-line-index position))
	 (insert-line (buffer-line buffer line))
	 	 
	 ;; Insert head chars at mark position	 
	 (end-head    (buffer-line-insert
			insert-line chars (mark-index position)
			:start start :end head))
	 
	 ;; Initialize final line/index.
	 (newline-p   (not (eql head end)))
	 (index       (if
			;; Does insert end on another line?
			(cond			  
			  (lines
			   ;; Insert following lines into buffer line array.
			   (multiple-value-bind (position vector)
			       (vector-insert (buffer-lines buffer) (1+ line) lines 0 (length lines))
			     (setf (buffer-lines buffer) vector)
			     (setf line position)))
			  
			  (newline-p
			   (incf line)))
			
			;; Yes, restart index at beginning of line.
			0
			
			;; No, final index is end of head chars.
			end-head)))

	;; Handle source chars after inserted newline.
	(when newline-p	  
	  (let* ((buffer-lines       (buffer-lines buffer))
		 (insert-line-chars  (buffer-line-chars insert-line))
		 (insert-line-length (length insert-line-chars))
		 (prev-tail-p        (< end-head insert-line-length)))
	    
	    ;; Add a new line when...
	    (when
	      (or
		;; ... tail of insert line ends in #\newline, or...
		(and prev-tail-p
		     (eql #\newline (elt insert-line-chars (1- insert-line-length))))  
		
		;; ... there's something to add at the end of the buffer.
		(and (>= line (length buffer-lines)) (or tail prev-tail-p)))
	      
	      (multiple-value-bind (position buffer-lines) (vector-insert buffer-lines line)
		(declare (ignore position))
		(setf (buffer-lines buffer) buffer-lines)
		(setf (elt buffer-lines line) (make-buffer-line))))
	    
	    (let ((next-line (elt buffer-lines line)))	    
	      ;; Insert source tail chars at beginning of next line.	
	      (when tail	  
		(setf index (buffer-line-insert next-line chars index :start tail :end end)))
	      
	      ;; Move previous tail of insert line, if necesssary.
	      (when prev-tail-p
		(buffer-line-insert next-line insert-line index :start end-head)
		(buffer-line-delete insert-line end-head)))))

	;; Return position at end of inserted chars
	(move-mark position line index))))
	  
  position)

(defmethod buffer-insert ((buffer buffer) (char character) (position mark) &key (start 0) end)
  (declare (ignore start end))
  (assert (eq buffer (mark-buffer position)) nil "~s is not a mark for ~s." position buffer)
  
  (let*
    ((line               (mark-line-index position))
     (insert-line        (buffer-line buffer line))
     (end                (buffer-line-insert insert-line char (mark-index position)))
     (index              (if
			   ;; Does insert end on another line?
			   (when (eql char #\newline) (incf line))
			   
			   ;; Yes, restart index at beginning of line.
			   0
			   
			   ;; No 
			   end))
     (insert-line-length (length (buffer-line-chars insert-line))))
    
    ;; ;; Is there something behind an inserted newline?
    (when (and (eql char #\newline) (< end insert-line-length))
      
      (let ((buffer-lines (buffer-lines buffer)))	
	;; Add a new line when...
	(when
	  (or
	    ;; ... tail of insert line ends in #\newline, or...
	    (eql #\newline (elt (buffer-line-chars insert-line) (1- insert-line-length)))
	    
	    ;; ... we're at the end of the buffer.
	    (>= line (length buffer-lines)))
	  
	  (multiple-value-bind (position buffer-lines) (vector-insert buffer-lines line)
	    (declare (ignore position))
	    (setf (buffer-lines buffer) buffer-lines) 
	    (setf (elt buffer-lines line) (make-buffer-line))))
	
	;; Move previous tail of insert line
	(buffer-line-insert (elt buffer-lines line) insert-line index :start end)
	(buffer-line-delete insert-line end)))
    
    ;; Return position at end of inserted chars
    (move-mark position line index)))

(let ((mark (make-mark)))
  (defmethod buffer-insert ((buffer buffer) chars (position null) &key (start 0) end)
    (buffer-insert buffer chars (buffer-position-mark buffer position mark) :start start :end end)
    nil)

  (defmethod buffer-insert ((buffer buffer) chars (position integer) &key (start 0) end)
    (buffer-insert buffer chars (buffer-position-mark buffer position mark) :start start :end end)
    (+ position (if (characterp chars) 1 (length chars)))))


(defun buffer-position-mark (buffer position &optional mark)
  "Return a MARK pointing at the given POSITION in the BUFFER. If a MARK
   is given, then it is updated and returned; otherwise a new mark is returned."
  (declare (type buffer                  buffer)
	   (type (or null (integer 0 *)) position))
  (check-type position (or null (integer 0 *)))
  (check-type buffer buffer)
  
  (let ((mark (or mark (make-mark))))
    (setf (mark-buffer mark) buffer)            
    
    (multiple-value-bind (line-index index)
	(when position
	  ;; Search for line/index corresponding to position.
	  (do* ((lines  (buffer-lines buffer))
		(nlines (length lines))
		(line   0 (1+ line)))
	      
	      ;; Return nil if position is past end of buffer.
	      ((>= line nlines))
	    
	    (let* ((chars  (buffer-line-chars (elt lines line)))
		   (nchars (length chars)))	      
	      (when
		(or
		  ;; Position within current line?
		  (< position nchars)
		  
		  ;; Position at end of line not ending in #\newline?
		  (and (= position nchars)
		       (or (zerop nchars)
			   (not (eql #\newline (elt chars (1- nchars)))))))
		
		;; Return valid line/index.
		(return (values line position)))
	      
	      (decf position nchars))))
      
      ;; Valid line/index found?
      (unless line-index
	(multiple-value-setq (line-index index)
	  
	  ;; No, return line/index for end of buffer.
	  (let* ((lines    (buffer-lines buffer))
		 (max-line (1- (length lines)))
		 (line     (unless (minusp max-line) (buffer-line-chars (elt lines max-line))))
		 (max-char (when line (length line))))
	    (cond
	      ((or
		 ;; No lines?
		 (minusp max-line)
		 
		 ;; Last line ends in #\newline?
		 (when (plusp max-char)
		   (eql #\newline (elt line (1- max-char)))))
	       
	       ;; Add empty line to empty buffer.
	       (vector-push-extend (make-buffer-line) lines)
	       
	       ;; End of buffer is begining of new line.
	       (values (1+ max-line) 0))
	      
	      (:else
	       (values max-line max-char))))))
      (move-mark mark line-index index))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                buffer-delete                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-delete (buffer start end)
  (:documentation
    "Deletes the chars from START to END from the BUFFER."))


;; buffer-line methods ----------------------------------------

(defmethod buffer-delete ((buffer buffer-line) (start mark) end)
  (buffer-delete buffer (mark-index start) end))

(defmethod buffer-delete ((buffer buffer-line) start (end mark))
  (buffer-delete buffer start (mark-index end)))

(defmethod buffer-delete ((buffer buffer-line) (start mark) (end mark))
  (buffer-delete buffer (mark-index start) (mark-index end)))

(defmethod buffer-delete ((buffer buffer-line) (start integer) end)
  (vector-delete (buffer-line-chars buffer) start end))



;; buffer methods ----------------------------------------------

(let ((mark (make-mark)))
  (defmethod buffer-delete ((buffer buffer) (start integer) end)
    (buffer-delete buffer (buffer-position-mark buffer start mark) end)))

(let ((mark (make-mark)))
  (defmethod buffer-delete ((buffer buffer) start (end integer))
    (buffer-delete buffer start (buffer-position-mark buffer end mark))))

(defmethod buffer-delete ((buffer buffer) (start mark) (end null))
  (let ((line-index (mark-line-index start))
	(lines      (buffer-lines buffer)))
    (buffer-line-delete (elt lines line-index) (mark-index start))
    (vector-delete lines (1+ line-index))))

(defmethod buffer-delete ((buffer buffer) (start mark) (end mark))
  (assert (eq buffer (mark-buffer start)) nil
	  "Start mark does not point to ~s." buffer)
  (assert (eq buffer (mark-buffer end)) nil
	  "End mark does not point to ~s." buffer)
  (let*
    ((lines       (buffer-lines buffer))
     
     (sli         (mark-line-index start))
     (start-line  (elt lines sli))
     (eli         (mark-line-index end)) 
     
     (start-start (mark-index start))
     (start-end   (when (= sli eli) (mark-index end))))

    ;;
    ;; Assert: start-end is non-nil iff start/end are on same line.
    ;;
    (assert (or (> eli sli) (and start-end (>= start-end start-start)))
	    nil "Start mark is past end mark.")

    ;; Delete chars from start line.
    (buffer-line-delete start-line start-start start-end)

    (unless start-end
      ;; Move chars up from end line and discard end line.
      (buffer-line-insert
	start-line (buffer-line-chars (elt lines eli)) nil
	:start (mark-index end))
      (vector-delete lines eli (1+ eli))
      
      ;; Delete any lines between start and end marks.
      (vector-delete lines (1+ sli) eli))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                buffer-length                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-length (buffer)
  (:documentation
    "Returns the number of characters in the BUFFER."))

(defmethod buffer-length ((buffer buffer-line))
  (length (buffer-line-chars buffer)))

(defmethod buffer-length ((buffer buffer))
  (let ((length 0)
	(lines  (buffer-lines buffer)))
    (dotimes (i (length lines) length)
      (incf length (length (buffer-line-chars (elt lines i))))))) 
    


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                buffer-subseq                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-subseq (buffer start end)
  (:documentation
    "Returns the BUFFER substring given by START and END."))


;; buffer-line methods ----------------------------------------

(defmethod buffer-subseq ((buffer buffer-line) (start mark) end)
  (buffer-subseq buffer (mark-index start) end))

(defmethod buffer-subseq ((buffer buffer-line) start (end mark))
  (buffer-subseq buffer start (mark-index end)))

(defmethod buffer-subseq ((buffer buffer-line) (start integer) (end integer))
  (subseq (buffer-line-chars buffer) start end))

(defmethod buffer-subseq ((buffer buffer-line) (start integer) (end null))
  (subseq (buffer-line-chars buffer) start end))


;; buffer methods ----------------------------------------------

(let ((mark (make-mark)))
  (defmethod buffer-subseq ((buffer buffer) (start integer) end)
    (buffer-subseq buffer (buffer-position-mark buffer start mark) end)))

(let ((mark (make-mark)))
  (defmethod buffer-subseq ((buffer buffer) start (end integer))
    (buffer-subseq buffer start (buffer-position-mark buffer end mark))))

(defmethod buffer-subseq ((buffer buffer) start (end null))
  (buffer-subseq buffer start (buffer-length buffer)))

(defmethod buffer-subseq ((buffer buffer) (start mark) (end mark))
  (assert (eq buffer (mark-buffer start)) nil
	  "Start mark does not point to ~s." buffer)
  (assert (eq buffer (mark-buffer end)) nil
	  "End mark does not point to ~s." buffer)

  (let ((start-line  (mark-line-index start))
	(start-index (mark-index start))
	(end-line    (mark-line-index end))
	(end-index   (mark-index end)))
    
    (assert (or (> end-line start-line)
		(and (= end-line start-line) (>= end-index start-index)))
	    nil "Start mark is past end mark.")
    
    (let ((subseq (make-array *minimum-buffer-line-length*
			      :adjustable   t
			      :fill-pointer 0
			      :element-type 'buffer-character)))
      (do
	((lines (buffer-lines buffer))
	 (max   (length (buffer-lines buffer)))
	 (line  start-line  (1+ line))
	 (start start-index 0))
	
	((or (> line end-line) (>= line max)))
	
	(vector-append subseq (buffer-line-chars (elt lines line))
		       start
		       (when (= line end-line) end-index)))
      
      subseq)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            buffer-number-lines                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-number-lines (buffer)
  (:documentation
    "Returns the number of lines in the BUFFER."))


(defmethod buffer-number-lines ((buffer buffer-line))
  (if (zerop (length (buffer-line-chars buffer))) 0 1))

(defmethod buffer-number-lines ((buffer buffer))
  (length (buffer-lines buffer)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                buffer-line                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-line (buffer i)
  (:documentation
    "Returns the i'th buffer-line in the BUFFER."))


(defmethod buffer-line ((buffer buffer-line) i)
  (when (and (plusp (length (buffer-line-chars buffer))) (zerop i))
    buffer))

(defmethod buffer-line ((buffer buffer) i)
  (elt (buffer-lines buffer) i))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             buffer-move-mark                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-move-mark (buffer mark &key lines chars)
  (:documentation "Move the MARK by the given number of LINES and CHARS and return the updated MARK."))

(defmethod buffer-move-mark ((buffer buffer-line) (mark integer) &key (lines 0) (chars 0))
  (assert (zerop lines) nil "Cannot change line within ~a." buffer)
  (max 0 (min (+ mark chars) (length (buffer-line-chars buffer)))))

(defmethod buffer-move-mark ((buffer buffer-line) (mark null) &key (lines 0) (chars 0))
  (buffer-move-mark buffer (length (buffer-line-chars buffer)) :lines lines :chars chars))

(let ((new-mark (make-mark)))
  (defmethod buffer-move-mark ((buffer buffer) (mark mark) &key (lines 0) (chars 0))
    (let* ((blines   (buffer-lines buffer))
	   (max-line (1- (length blines))))

      (move-mark new-mark mark)
      
      (unless (zerop lines)
	(setf (mark-line-index new-mark)
	      (max 0 (min max-line (+ (mark-line-index new-mark) lines))))

	;; Trying to move past end of line?
	(let* ((line (elt blines (mark-line-index new-mark)))
	       (max  (1- (buffer-length line))))
	  (unless (or (minusp max) (eql #\newline (elt (buffer-line-chars line) max)))
	    (incf max))
	  (when (> (mark-index new-mark) max)
	   (setf (mark-index new-mark) (max max 0)))))
	  
      (unless (zerop chars)
	(setf (mark-index new-mark)
	      (do ((position (+ (mark-index new-mark) chars))
		   max)
		  (())
		(cond
		  ;; Trying to move before start of line?
		  ((< position 0)
		   (cond
		     ;; Trying to move before first character in buffer?
		     ((zerop (mark-line-index new-mark))
		      ;; Yes, stop at first character.
		      (setf position 0))
		     
		     ;; No, move to previous line.
		     (t
		      (decf (mark-line-index new-mark))
		      (incf position (buffer-length (elt blines (mark-line-index new-mark)))))))

		  ;; Trying to move past end of (not the last) line?
		  ((and
		     (>= position (setf max (buffer-length (elt blines (mark-line-index new-mark)))))
		     (< (mark-line-index new-mark) max-line))		   
		   ;; Yes, move to next line.
		   (decf position max)
		   (incf (mark-line-index new-mark)))

		  ;; Trying to move past end of buffer?
		  ((> position max)
		   ;; Yes, stop at end of buffer.
		   (setf position max))
		  
		  (t
		   (return position))))))
      new-mark)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            buffer-text-extents                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-text-extents (buffer font start end &key translate)
  (:documentation "Return the width, height, ascent, and descent of the given substring of the BUFFER."))

(defmethod buffer-text-extents ((buffer buffer-line) font start end &key translate)
  (multiple-value-bind (width a d l r ascent descent)
      (text-extents font (buffer-line-chars buffer)
		    :start start :end end :translate translate)
    (declare (ignore a d l r))
    (values width (+ ascent descent) ascent descent)))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                          buffer-sol/eol                                    |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defgeneric buffer-sol (buffer position)
  (:documentation "Return the position in BUFFER at the start of the line containing POSITION."))

(defmethod buffer-sol ((buffer buffer-line) position)
  (declare (ignore position))
  0)

(let ((mark (make-mark)))
  (defmethod buffer-sol ((buffer buffer) (position mark))
    (setf (mark-buffer mark) buffer)
    (move-mark mark (mark-line-index position) 0))

  (defmethod buffer-sol ((buffer buffer) position)
    (buffer-position-mark buffer position mark)
    (setf (mark-index mark) 0)
    mark))


(defgeneric buffer-eol (buffer position)
  (:documentation "Return the position in BUFFER at the end of the line containing POSITION."))

(defmethod buffer-eol ((buffer buffer-line) position)
  (declare (ignore position))
  (buffer-length buffer))

(let ((mark (make-mark)))
  (defmethod buffer-eol ((buffer buffer) (position mark))
    (let*
      ((line  (mark-line-index position))
       (bline (elt (buffer-lines buffer) line))
       (max   (buffer-length bline))
       (end   (if (and (plusp max) (eql #\newline (elt (buffer-line-chars bline) (1- max)))) (1- max) max)))

      (setf (mark-buffer mark) buffer)
      (move-mark mark line end)))

  (defmethod buffer-eol ((buffer buffer) position)
    (buffer-eol buffer (buffer-position-mark buffer position mark))))
