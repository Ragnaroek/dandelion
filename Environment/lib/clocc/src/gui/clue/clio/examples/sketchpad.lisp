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

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 sketchpad                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(DEFCONTACT sketchpad (core contact)
  ((mode          :type     (member :line :polygon)
		  :accessor sketchpad-mode
		  :initform :line)
   (in-progress-p :type list
                  :accessor sketchpad-in-progress-p
                  :initform nil)   
   (picture       :type list
                  :accessor sketchpad-picture
                  :initform nil)
   (line-width    :type card16
                  :accessor sketchpad-line-width
                  :initform 0)
   (fill          :type symbol
                  :accessor sketchpad-fill
                  :initform '100%gray)
   (next-x        :type (or null int16)           
                  :initform nil)
   (next-y        :type (or null int16)           
                  :initform nil)
   (compress-exposures
                  :allocation :class
                  :initform   :on))
  (:documentation "A basic picture editor.")
  (:resources
    (cursor      :initform 'crosshair-cursor)
    (event-mask  :initform #.(make-event-mask :exposure :button-press))))


(defun make-sketchpad (&rest initargs)
  (apply #'make-contact 'sketchpad initargs))

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                  Display                                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod DISPLAY ((sketchpad sketchpad) &optional x y width height &key)
  (with-slots
    (picture (total-width width) (total-height height))
    sketchpad
    
    (let*
      ;; Compute default exposed area, if necessary.
      ((x      (or x      0))
       (y      (or y      0))
       (width  (or width  (- total-width x)))
       (height (or height (- total-height y))))

      ;; Draw all picture elements that intersect exposed area.
      (dolist (element picture)
        (when (intersect-p element x y width height)
          (draw-element sketchpad element))))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Point-Seq Utilities                             |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmacro last-x (points)
  `(first ,points))

(defmacro last-y (points)
  `(second ,points))

(defmacro point-seq-length (points)
  `(/ (length ,points) 2))

(defmacro point-seq-x (points i)
   `(elt ,points (* ,i 2)))

(defmacro point-seq-y (points i)
   `(elt ,points (1+ (* ,i 2))))

(defun nreverse-point-seq (point-seq)
  (let ((rest (cddr point-seq)))
    (cond
      (rest
	(setf (cddr point-seq) nil)
	(nconc (nreverse-point-seq rest) point-seq))
      (:else       
       point-seq))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            Event Translations                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(DEFEVENT sketchpad (:button-release :button-1) enter-point)
(DEFEVENT sketchpad :motion-notify              move-point)
(DEFEVENT sketchpad :leave-notify               finish-points)


(defun enter-point (sketchpad)
  (WITH-EVENT (x y)
    (with-slots (in-progress-p next-x next-y) sketchpad

      ;; Is this point the same as the last one entered?
      (if (and in-progress-p
	       (= x (last-x in-progress-p))
	       (= y (last-y in-progress-p)))

	  ;; Yes, complete element.
	  (end-points sketchpad)
	  
	  ;; No, update point list with new point.
	  (setf in-progress-p (nconc (list x y) in-progress-p)
		next-x        nil
	        next-y        nil)))))


(defun end-points (sketchpad)
  (with-slots (mode) sketchpad
    ;; Complete element in current mode.
    (finish-element sketchpad mode)))

(defun finish-points (sketchpad)
  (with-slots (in-progress-p mode) sketchpad
    (when in-progress-p
      ;; Undisplay last rubberband line.
      (display-next-point sketchpad mode)

      ;; Complete element.
      (end-points sketchpad))))

(defun move-point (sketchpad)
  (WITH-EVENT (x y)
    (with-slots (next-x next-y mode in-progress-p) sketchpad

      ;; Ignore if first point not yet entered.
      (when in-progress-p
        ;; Undisplay last rubberband line.
        (when next-x
          (display-next-point sketchpad mode))
        
        ;; Update next point.
        (setf next-x x next-y y)
        
        ;; Display next rubberband line.
        (display-next-point sketchpad mode)))))


(defmethod finish-element ((sketchpad sketchpad) mode)
  (with-slots (in-progress-p picture) sketchpad
    ;; Restore point list to order entered.
    (setf in-progress-p (nreverse-point-seq in-progress-p))
    
    ;; Erase all old rubberband lines.
    (clear-in-progress sketchpad mode)

    ;; Add new element to display list.
    (let ((element (add-element sketchpad mode)))
      (when element
	(setf picture (nconc picture (list element)))))
    
    ;; Get ready to begin next element.
    (setf in-progress-p nil)))


    

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                                 Line Mode                                  |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defstruct line
  points
  width)

      
(defmethod add-element ((sketchpad sketchpad) (mode (eql :line)))
  (with-slots (in-progress-p line-width) sketchpad
    (unless (< (point-seq-length in-progress-p) 2)
      (let ((new-line (make-line
			:width  line-width
			:points in-progress-p)))
	(draw-element sketchpad new-line)
	new-line))))


(defmethod clear-in-progress ((sketchpad sketchpad) mode)
  (declare (ignore mode))
  (with-slots (in-progress-p line-width) sketchpad
    (USING-GCONTEXT (gcontext
		      :drawable   sketchpad
		      :line-width line-width
		      :foreground (logxor (CONTACT-FOREGROUND sketchpad)
					  (CONTACT-CURRENT-BACKGROUND-PIXEL sketchpad))
		      :function   boole-xor)
      (do* ((from-x (first in-progress-p)  to-x)
	    (from-y (second in-progress-p) to-y)
	    (points (cddr in-progress-p)   (cddr points))
	    (to-x   (first points)         (first points))
	    (to-y   (second points)        (second points)))
	   ((endp points))
	(draw-line sketchpad gcontext from-x from-y to-x to-y)))))

(defmethod display-next-point ((sketchpad sketchpad) mode)
  (declare (ignore mode))
  (with-slots (line-width next-x next-y in-progress-p) sketchpad
    (USING-GCONTEXT (gcontext
		      :drawable   sketchpad
		      :line-width line-width
		      :foreground (logxor (CONTACT-FOREGROUND sketchpad)
					  (CONTACT-CURRENT-BACKGROUND-PIXEL sketchpad))
		      :function   boole-xor)
      (draw-line sketchpad gcontext
		 (last-x in-progress-p) (last-y in-progress-p)
		 next-x next-y))))

(defmethod draw-element ((sketchpad sketchpad) (element line))
  (USING-GCONTEXT (gcontext
		    :drawable   sketchpad
		    :line-width (line-width element)
		    :foreground (CONTACT-FOREGROUND sketchpad))
    (draw-lines sketchpad gcontext (line-points element))))

(defmethod intersect-p ((element line) x y width height)
  (let*
    ((points (line-points element))
     (min-x  (point-seq-x points 0))
     (max-x  min-x)
     (min-y  (point-seq-y points 0))
     (max-y  min-y))
    (dotimes (i (point-seq-length points))
      (setf
	min-x (min min-x (point-seq-x points i))
	max-x (max max-x (point-seq-x points i))
	min-y (min min-y (point-seq-y points i))
	max-y (max max-y (point-seq-y points i))))    
    (and
      (>= max-x x)
      (>= max-y y)
      (<  min-x (+ x width))
      (<  min-y (+ y height)))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               Polygon Mode                                 |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defstruct (polygon (:include line))
  fill)

      
(defmethod add-element ((sketchpad sketchpad) (mode (eql :polygon)))
  (with-slots (in-progress-p line-width fill) sketchpad
    (unless (< (point-seq-length in-progress-p) 3)
      (let ((new-polygon (make-polygon
			   :width  line-width
			   :fill   fill
			   :points in-progress-p)))
	(draw-element sketchpad new-polygon)
	new-polygon))))



(defmethod draw-element ((sketchpad sketchpad) (element polygon))
  (let ((foreground (CONTACT-FOREGROUND sketchpad)))
    (USING-GCONTEXT (gcontext
		      :drawable   sketchpad
		      :fill-style :tiled
		      :tile       (CONTACT-IMAGE-MASK
				    sketchpad (symbol-value (polygon-fill element))
				    :foreground foreground
				    :background (CONTACT-CURRENT-BACKGROUND-PIXEL sketchpad)))
      
      ;; Fill interior
      (draw-lines sketchpad gcontext (line-points element) :fill-p t)
      
      ;; Draw boundary
      (with-gcontext (gcontext
		       :fill-style :solid
		       :line-width (polygon-width element)
		       :foreground foreground)
	(draw-lines sketchpad gcontext (line-points element))
	(let ((last  (1- (point-seq-length (line-points element)))))
	  (draw-line  sketchpad gcontext
		      (point-seq-x (line-points element) last) (point-seq-y (line-points element) last)
		      (point-seq-x (line-points element) 0) (point-seq-y (line-points element) 0)))))))


