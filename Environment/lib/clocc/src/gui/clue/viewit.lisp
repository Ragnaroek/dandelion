;;; viewit.lisp -*- Package: User -*-
;;;
;;; Example of use of pictures to draw line charts.
;;; Very much work in progress. Not really ready for
;;; prime time.
;;;
;;; try (testme (make-pt-seq '(0 1 2 3 2 3 5 6 10)))
;;;
;;; To remove graphic from display
;;; (killit)
;;;
;;; Paul Werkowski
;;;
(in-package :user)
(use-package :clue)
(use-package :pictures)

(defvar *default-host* "localhost")

;;; first some CLUE setup
(defparameter *database* (xlib:make-resource-database))
(defvar *display* (open-contact-display 'test :host "localhost"))

(eval-when (load eval)
  (define-resources
      (* background) :wheat
      (* foreground) :black
      (* highlight) :yellow
      (* font) "fixed")

  (when (and (boundp '*display*)(xlib:display-p *display*))
    ;; Grab resource values from server.
    (xlib:wm-resources 
     *database*
     (xlib:screen-root (xlib:display-default-screen *display*))))
  
  (define-resources
    (* top y) 80
    (* top x) 10
    (* plot background) :blue
    (* tactical foreground) :red
    (* tactical plane color) :red
    (* tactical dish color) :green
    (* gunner bg-color) :skyblue
    (* gunner fg-color) :black
    (* gunner ax-color) :blue))


;;; Some Pictures stuff
(defvar *top*)
(defvar *all* nil)
(defvar *view*)
(defvar *rectangle*)
(defvar *line*)
(defvar *polygon*)
(defvar *label*)
(defvar *scene*)



;;; This  leaves the view active after exiting. Need to
;;; run disable-clx-event-handling when done.
;;; (showit2 (make-scene)) produces a blank display suitable
;;; for experimenting with objects,
;;;  eg (scene-insert *scene* (make-line 10 10 50 50))
;;;     (refresh-view *view*)
;;;
(defun showit2(&optional (scene (make-scene))
			 &key
			 (width 100)(height 100))
  (let* ((top (make-contact 'top-level-shell
			    :parent *display*
			    :state :mapped
			    :wm-title "Demo"
			    :x 30
			    :y 30))
	 (scene-parent (make-scene :elements (list scene)))
	 ;; A vew connects the graphic to the xwindow system via
	 ;; a CLUE contact object. This lets event handlers work.
	 (view (make-view
		:parent top
		:width width
		:height height
		;; Seems a graphic parent is needed for proper transforming
		;; so wrap the input scene in one of its own
		:graphic scene-parent
		)))
    (setq *scene* scene
	  *view* view
	  *top* top)
    (push top *all*)
    ;; This is needed to let labels find scale factor.
    (setf (graphic-view scene-parent) view)
    (loop while (process-next-event *display* 1))
    (ext:enable-clx-event-handling
     *display* #'(lambda(display)
		   (process-next-event display 0)))))

(defun killit()
  (ext:disable-clx-event-handling *display*)
  (do ((x (pop *all*)(pop *all*)))
      ((not x))
    (destroy x)))

(defun make-pt-seq(seq &optional (dx 1.0))
  (etypecase seq
    (list
     (loop
       for y in seq
       and x upfrom 0.0 by dx
       append (list x y)))
    (vector
     (loop
       for y across seq
       and x upfrom 0.0 by dx
       append (list x y)))))
      
(defun ensure-polypoint (seq)
  (if (typep seq 'polypoint)
      seq
      (make-polyline seq)))

(defun get-combined-extent (list-of-graphics)
  (when list-of-graphics
    (let ((tmp-extent (make-extent-rect)))
      (dolist (g list-of-graphics tmp-extent)
	(let ((e (graphic-extent g)))
	  (cond ((and (valid-extent-p e)(valid-extent-p tmp-extent))
		 (extent-combine e tmp-extent))
		((and (valid-extent-p e)(not (valid-extent-p tmp-extent)))
		 (extent-copy e tmp-extent))))))))

(defun make-chart (list-of-curves
		   &key
		   (width 100)(height 100)
		   xlow xhgh ylow yhgh)
  "Lets say each curve is a polypoint in the same coordinate
   world (forgetting for now any annotations)
   The idea is to build up a scene tree that can be scaled
   to the \"world-coordinate\" (really window coordinate)
   system of the view.
   The x/y keys are used to establish the boundaries of
   our coordinate system. Else these are derived from the
   max extent of the components."
  (let* ((curves (mapcar 'ensure-polypoint list-of-curves))
	 (extent (get-combined-extent curves))
	 (xlow (or xlow (extent-rect-xmin extent)))
	 (xhgh (or xhgh (extent-rect-xmax extent)))
	 (ylow (or ylow (extent-rect-ymin extent)))
	 (yhgh (or yhgh (extent-rect-ymax extent)))
	 (xscale (/ width (- xhgh xlow)))
	 (yscale (/ height (- yhgh ylow)))
	 (x0 0)
	 (y0 0))
    (declare (ignore x0 y0))
    (let ((scene (make-scene :elements curves)))
      (move-transform scene (- xlow) (- ylow))
      (scale-transform scene xscale yscale)
      scene)))



(defun testme(seq &key (width 400)(height 300) yhgh ylow)
  (let* ((seq (if (and (listp seq)(numberp (first seq)))
		  (list seq) seq))
	 (ymax (reduce 'max (mapcar #'point-seq-y-max seq)))
	 (ymin (reduce 'min (mapcar #'point-seq-y-min seq)))
	 (xmin (reduce 'min (mapcar #'point-seq-x-min seq)))
	 (xmax (reduce 'max (mapcar #'point-seq-x-max seq)))
	 (xorg (and (not (zerop (- (signum ymax)(signum ymin))))
		    (list (list xmin 0 xmax 0))))
	 (scene (make-chart (append seq xorg)
			    :yhgh yhgh :ylow ylow
			    :width width :height height)))
    (showit2 scene :width width :height height )))


(defun good-extents (low hgh)
  (let* ((range (- hgh low))
	 (logr (ceiling (log range 10))))
    (cond ((= logr 2)
	   ;; 10+ -> 100
	   (values (* (floor   low 10) 10)
		   (* (ceiling hgh 10) 10)))
	  (t (values low hgh)))))

(defun make-xgrid(xlow ylow xhgh yhgh by)
  (format t "xgrid ~a ~a ~a ~a~%" xlow ylow xhgh yhgh)
  (let ((res nil))
    (do ((x xlow (+ x by)))
	((> x xhgh))
      (push (list x ylow x yhgh) res))
    (nreverse res)))

(defun make-ygrid(xlow ylow xhgh yhgh by)
  (format t "ygrid ~a ~a ~a ~a~%" xlow ylow xhgh yhgh)
  (let ((res nil))
    (do ((y ylow (+ y by)))
	((> y yhgh))
      (push (list xlow y xhgh y) res))
    (nreverse res)))

(defun make-xy-grid (xmin ymin xmax ymax)
  (multiple-value-bind (xgl xgh)(good-extents xmin xmax)
    (multiple-value-bind (ygl ygh) (good-extents ymin ymax)
      (let ((ygrid (make-ygrid xgl ygl xgh ygh 10))
	    (xgrid (make-xgrid xgl ygl xgh ygh 20)))
	(nconc xgrid ygrid)))))

(defun show/grid(seq &key (width 400)(height 300) yhgh ylow)
  (let* ((seq (if (and (listp seq)(numberp (first seq)))
		  (list seq) seq))
	 (ymax (reduce 'max (mapcar #'point-seq-y-max seq)))
	 (ymin (reduce 'min (mapcar #'point-seq-y-min seq)))
	 (xmin (reduce 'min (mapcar #'point-seq-x-min seq)))
	 (xmax (reduce 'max (mapcar #'point-seq-x-max seq)))
	 (xorg (and (not (zerop (- (signum ymax)(signum ymin))))
		    (list (list xmin 0 xmax 0))))
	 (grid (make-xy-grid xmin (or ylow ymin) xmax (or yhgh ymax)))
	 (scene (make-chart (append seq xorg grid)
			    :yhgh yhgh :ylow ylow
			    :width width :height height)))
    (showit2 scene :width width :height height )))
