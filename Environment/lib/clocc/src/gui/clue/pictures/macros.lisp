;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "PICTURES")


(DEFMACRO with-vector (avector &body body)
  `(let ((,avector (get-global-vector)))
     (LET ((value (PROGN ,@body)))
       (return-global-vector ,avector)
     value)))

(DEFMACRO with-extent-corners (extent xmin ymin xmax ymax &body body)
  `(LET ((,xmin (extent-rect-xmin ,extent))
	 (,ymin (extent-rect-ymin ,extent))
	 (,xmax (extent-rect-xmax ,extent))
	 (,ymax (extent-rect-ymax ,extent)))
     ,@body))

(DEFMACRO with-extent-values (extent xmin ymin xmax ymax width height
				     &body body)
  `(LET ((,xmin (extent-rect-xmin ,extent))
	 (,ymin (extent-rect-ymin ,extent))
	 (,xmax (extent-rect-xmax ,extent))
	 (,ymax (extent-rect-ymax ,extent))
	 (,width (- (extent-rect-xmax ,extent)(extent-rect-xmin ,extent)))
	 (,height (-  (extent-rect-ymax ,extent)(extent-rect-ymin ,extent))))
     ,@body))

(DEFMACRO with-special-vector (avector &body body)
  `(let ((,avector (get-global-vector)))
     (DECLARE (SPECIAL ,avector))
     (LET ((value (PROGN ,@body)))
       (return-global-vector ,avector)
     value)))

(DEFMACRO drawbox-with-gc (view highlight-color fixed-x fixed-y px py)
  	`(using-gcontext
	  (gc :drawable ,view
	      :function boole-xor
	      :foreground ,highlight-color)
	  (draw-rectangle view gc
			  (MIN ,fixed-x ,px)
			  (MIN ,fixed-y ,py)
			  (ABS (- ,fixed-x  ,px))(ABS (- ,fixed-y  ,py)))))


(defmacro drawbox (fixed-x fixed-y px py)
  `(draw-rectangle view gc
			  (MIN ,fixed-x ,px)
			  (MIN ,fixed-y ,py)
			  (ABS (- ,fixed-x  ,px))(ABS (- ,fixed-y  ,py))))


(DEFMACRO drawlines-with-gc (view highlight-color vertices)
  	`(using-gcontext
	  (gc :drawable ,view
	      :function boole-xor
	      :foreground ,highlight-color)
	  (draw-lines view gc ,vertices)))


;;#+nil ;; pw--This seems pretty messed up. See new attempt next.
(DEFMACRO visible-p (graphic)
  `(AND (NOT (AND (and min-x min-y width height) ; Was optional rect given
		  (not (graphic-within-p
			,graphic min-x min-y width height))
		  (not (graphic-intersects-p
			,graphic min-x min-y width height)))) 
	(PROGN
	  (UNLESS (valid-extent-p (graphic-extent ,graphic)))
	  (>= (* (view-scale view) (- (extent-rect-xmax extent)
				      (extent-rect-xmin extent))) 1)
	  (>= (* (view-scale view) (- (extent-rect-ymax extent)
				      (extent-rect-ymin extent))) 1))
	(viewable-p ,graphic)))

(DEFMACRO visible-p (graphic)
  `(AND (NOT (AND (and min-x min-y width height) ; Was optional rect given
		  (not (graphic-within-p
			,graphic min-x min-y width height))
		  (not (graphic-intersects-p
			,graphic min-x min-y width height)))) 

	(valid-extent-p (graphic-extent ,graphic))
	#+nil
	(plusp (* (view-scale view) (- (extent-rect-xmax extent)
				       (extent-rect-xmin extent))))
	#+nil
	(plusp (* (view-scale view) (- (extent-rect-ymax extent)
				       (extent-rect-ymin extent))))
	(viewable-p ,graphic)))


(DEFMACRO change-base-x (x extent width gravity)
  `(COND 
    ((OR (EQ :southwest ,gravity)
	 (EQ :west ,gravity)
	 (EQ :northwest ,gravity)) ,x)
    ((OR (EQ :north ,gravity )
	 (EQ :south ,gravity)
	 (EQ :center ,gravity))
     (+ ,x (/ (- (- (extent-rect-xmax ,extent)
		    (extent-rect-xmin ,extent))
		 ,width) 2.0)))
    (t (+ ,x (- (- (extent-rect-xmax ,extent)
		   (extent-rect-xmin ,extent))
		,width)))))


(DEFMACRO change-base-y (y extent height gravity)
  `(COND 
    ((OR (EQ :southwest ,gravity)
	 (EQ :south ,gravity)
	 (EQ :southeast ,gravity)) ,y)
    ((OR (EQ :east ,gravity )
	 (EQ :west ,gravity)
	 (EQ :center ,gravity))
     (+ ,y (/ (- (- (extent-rect-ymax ,extent)
		    (extent-rect-ymin ,extent)
		    ) ,height) 2.0)))
    (t (+ ,y (- (- (extent-rect-ymax ,extent)
		   (extent-rect-ymin ,extent))
		,height)))))


(DEFMACRO point-near-line (vertices pixel x y)
  `(DO ((sx 0 (+ sx 2))
	(sy 1 (+ sy 2)))
       ((> (+ sy 2) (FILL-POINTER ,vertices))
	(point-on-line-p ,pixel ,x ,y
			 (ELT ,vertices 0)
			 (ELT ,vertices 1)
			 (ELT ,vertices sx )
			 (ELT ,vertices sy )))
     (WHEN (point-on-line-p 
	    ,pixel ,x ,y (ELT ,vertices sx)(ELT ,vertices sy)
	    (ELT ,vertices (+ sx 2))(ELT ,vertices (+ sy 2))) (RETURN t))))

(defmacro copy-to-vector (source result)
  "copy the source SOURCE sequence into the RESULT vector"
  `(WHEN (NOT  (EQ ,source ,result) )
     (SETF (FILL-POINTER ,result) 0)
     (DOTIMES (i (LENGTH ,source))
       (VECTOR-PUSH-EXTEND (elt ,source i) ,result))))

(DEFMACRO copy-to-point-seq (source result)
  `(PROGN  
     (WHEN (NOT  (EQ ,source ,result))
       (IF (LISTP ,result)
	   (LET ((source-length (1- (LENGTH ,source))))
	     (SETq ,result (LIST (ELT ,source 0)))
	     (DO  ((i 1 (1+ i)))
		  ((> i source-length))
	       (NCONC ,result (LIST (elt ,source i)) )
	       )
	     )
	   (PROGN 
	     (SETF (FILL-POINTER ,result) 0)
	     (DOTIMES (i (LENGTH ,source))
	       (VECTOR-PUSH-EXTEND (elt ,source i) ,result)))))
     ,result))


(DEFMACRO process-motion-notify-events (view display  fixed-x
					     fixed-y px py highlight-color)
  `(LET ((display-after-func (display-after-function ,display))
	 (events-enabled-p (graphic-events-enabled-p view)))
     (UNWIND-PROTECT
	 (progn
	   (SETF (display-after-function ,display) #'display-force-output)
	   (SETF (graphic-events-enabled-p view) nil)
	   (drawbox-with-gc ,view ,highlight-color ,fixed-x ,fixed-y ,px ,py)
	   (grab-pointer ,view #.(make-event-mask 
				  :button-release  :pointer-motion) :owner-p t)
	   (CATCH :release
	     (LOOP
	       (process-next-event ,display )))
	   (drawbox-with-gc ,view ,highlight-color ,fixed-x ,fixed-y ,px ,py)
	   (SETF (display-after-function ,display) nil))
       (PROGN
	 (SETF (graphic-events-enabled-p view) events-enabled-p )
	 (ungrab-pointer ,display)))
     (SETF (display-after-function ,display) display-after-func)))

; - - - - - - - -  identifier macros for save/restore  - - - - - - -
(defmacro executable-form-p (form)
  "Recognizes forms like make-array, make-hash-table ..."
  `(and (consp ,form)
	(symbolp (car ,form))
	(not (keywordp (car ,form)))
	(symbol-function (car ,form))))

(defmacro make-form-p (form)
  "Recognizes forms like make-circle, make-line, make-image-x..."
  `(and (executable-form-p ,form)
	(or (eq (symbol-package (car ,form)) (find-package 'pictures))
	    (eq (symbol-package (car ,form)) (find-package 'xlib)))
	;; really should do the next test, but its too expensive 
	;;(string-equal "make-" (format nil "~a" (car ,form)) :end1 5 :end2 5)
	))

(DEFVAR *gstate-index*
  '((foreground 0)
    (pictures::background 1)  ;;to avoid conflict with clue::background
    (line-style  2)
    (stipple  3)
    (line-width  4)
    (function 5)
    (dash-offset 6)
    (dashes 7)
    (tile  8)
    (fill-rule 9)
    (fill-style 10)
    (cap-style 11)
    (join-style 12)
    (pictures::font 13)
    (arc-mode  20)
    (ts-x  15)
    (ts-y  16)
    (clip-mask 17)
    (clip-x  18)
    (clip-y 19)
    (stipple-pix 14)
    ))

(DEFVAR *gstate-type-alist*
  '(
    (:function  ( null boole-constant))
    (:foreground ( string pixel list))
    (:background ( string pixel list))
    (:plane-mask  ( null pixel))
    (:dash-offset ( null card16))
    (:ts-x ( null int16) )
    (:ts-y  ( null int16) )
    (:clip-x  ( null int16) )
    (:clip-y  ( null int16))
    (:line-style ( null (member :solid :dash :double-dash)))
    (:line-width (null number))
    (:cap-style ( null (member :not-last :butt :round :projecting)))
    (:join-style ( null (member :miter :round :bevel)))
    (:fill-style ( null (member :solid :tiled :opaque-stippled :stippled)))
    (:fill-rule ( null (member :even-odd :winding)))
    (:arc-mode ( null (member :chord :pie-slice)))
    (:stipple  ( null symbol list image))
    (:stipple-pix  ( null pixmap))
    (:tile ( null symbol list image) )
    (:font ( null font))
    (:fontname (NULL stringable))
    (:subwindow-mode ( null (member :clip-by-children :include-inferiors)))
    (:exposures ( null (member :on :off)))
    (:clip-mask ( null (member :none) symbol rect-seq))	
    (:clip-ordering ( null (member :unsorted :y-sorted :yx-sorted :yx-banded)))
    (:dashes ( null card8 sequence))))	;Gstate Class Definition:

;;; CMUCL optimization note. Little chance here as both NIL and float
;;; values with be stored as descriptor objects.

#-cmu
(defstruct extent-rect
  xmin ymin xmax ymax)

#+cmu
(defstruct extent-rect
  (xmin 0s0 :type extent-element)
  (ymin 0s0 :type extent-element)
  (xmax 0s0 :type extent-element)
  (ymax 0s0 :type extent-element)
  (valid nil :type (member t nil)))

(defmacro with-coercion((vars type) &body body)
  (flet ((varname (v)
	   (if (symbolp v) v (cadr v)))
	 (varform (v)
	   (etypecase v
	     (symbol
	      `(,v (coerce ,v ',type)))
	     (list
	      `(,(car v) (coerce ,(cadr v) ',type))))))
    `(let ,(mapcar #'varform vars)
       (declare (type ,type ,@(mapcar #'varname vars)))
       ,@body)))
