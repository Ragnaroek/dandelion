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


(export '(save-graphic) 'pictures)

; - - - - - - - - - - - - generic function - - - - - - - - - - - - - - 

; - - - - - - - - - - - - base class graphic - - - - - - - - - - -
(defmethod save-graphic :before ((graphic graphic) (format (eql :nested-forms)) 
			 &optional (stream *standard-output*))
  (normalize-graphic graphic)
  (format stream "(MAKE-~a " (class-name (class-of graphic))))

(defmethod save-graphic ((graphic graphic) (format (eql :nested-forms)) 
			 &optional (stream *standard-output*))
  (declare (ignore stream))
  ;; subclasses will save their own data in a specialized primary method
  ;; edge-gstate (when applicable), sensitivity, gstate are saved in an :after method
  )

(defmethod save-graphic :after ((graphic graphic) (format (eql :nested-forms)) 
				&optional (stream *standard-output*))
  ;; do edge-gstate first if applicable
  (when (typep graphic 'edge)
    (with-slots (edge-gstate) graphic
      (when edge-gstate
	(format stream ":edge-gstate ")
	(save-graphic edge-gstate format stream))))
  ;; do sensitivity and normal gstate next
  (with-slots (sensitivity gstate) graphic
    (format stream ":sensitivity :~a " sensitivity)
    (when gstate
      (format stream ":gstate ")
      (save-graphic gstate format stream)))
  ;; sometimes transforms will still remain, even if graphic was normalized
  (let ((gt (graphic-transform graphic)))
    (when gt
      (format stream ":transform ")
      (save-graphic gt format stream)))
  (format stream ") "))

; - - - - - - - - -  transform - - - - - - - - - -
(defmethod save-graphic ((graphic transform) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (with-slots (t11 t12 t21 t22 t31 t32) graphic
    (format stream "(MAKE-TRANSFORM :t11 ~a :t12 ~a :t21 ~a :t22 ~a :t31 ~a :t32 ~a) "
	    t11 t12 t21 t22 t31 t32)))

; - - - - - - - - -  gstate - - - - - - - - - - 
(defmethod save-graphic ((graphic gstate) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (format stream "(MAKE-GSTATE ")
  (with-slots (gstate-array gstate-hash) graphic
    ;; save the gstate-array
    (dotimes (pos (length gstate-array))
      (let ((element (elt gstate-array pos))) 
	(when element
	  (let ((keyword ;;(first (rassoc pos *gstate-index* :key #'first))))
		           (first (find pos *gstate-index* :key #'second)))) 
	    (unless (member keyword *no-storage-methods*)
	      (format stream  ":~a "  keyword)	  
	      (save-graphic (if (consp element) (car element) element) format stream))))))
    ;; save the gstate-hash
    (maphash #'(lambda (keyword value)
		 (unless (member keyword *no-storage-methods*)
		   (FORMAT stream ":~a " keyword)
		   (save-graphic value format stream)))
	     gstate-hash)
    )
  (format stream ") "))



;------------- specialized primary methods for subclasses -----------------

; - - - - - - - - - - - - circle - - - - - - - - - - -
(defmethod save-graphic ((graphic circle) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (with-slots (center-x center-y radius) graphic
    (format stream "~a ~a ~a " center-x center-y radius)))

; - - - - - - - - - - - - ellipse - - - - - - - - - - -
(defmethod save-graphic ((graphic ellipse) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
    (format stream " ~a ~a ~a ~a " 
	    (ellipse-origin-x graphic)
	    (ellipse-origin-y graphic)
	    (ellipse-width graphic)
	    (ellipse-height graphic)))

; - - - - - - - - - - - - graphic image - - - - - - - - - - -
(defmethod save-graphic ((graphic graphic-image) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
    (save-graphic (graphic-image-content graphic) format stream)
    (format stream ":base-x ~a :base-y ~a :gravity :~a  :tile-p ~a " 
	    (graphic-image-base-x graphic)
	    (graphic-image-base-y graphic)
	    (graphic-image-gravity graphic)
	    (graphic-image-tile-p graphic))
    (when (graphic-image-tile-p graphic)
      ;; extent has to be specified for tiled objects
      (format stream ":extent-x ~a :extent-y ~a :extent-width ~a :extent-height ~a "
	      (graphic-extent-x graphic) (graphic-extent-y graphic)
	      (graphic-extent-width graphic) (graphic-extent-height graphic))))

(defmethod save-graphic ((graphic image-x) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (format stream "(XLIB::MAKE-IMAGE-X  :width ~a :height ~a  :depth ~a  :plist "
	  (image-width graphic) 
	  (image-height graphic)
	  (image-depth  graphic))
  (save-graphic (image-plist graphic) format stream)
  (format stream " :format :~a :bytes-per-line ~a :bits-per-pixel ~a ~
                   :bit-lsb-first-p ~a  :byte-lsb-first-p ~a :data " 
	  (xlib::image-x-format graphic) 
	  (xlib::image-x-bytes-per-line graphic)
	  (xlib::image-x-bits-per-pixel graphic)
	  (xlib::image-x-bit-lsb-first-p graphic)
	  (xlib::image-x-byte-lsb-first-p graphic))
  (save-graphic (xlib::image-x-data graphic) format stream)
  (format stream ") "))

(defmethod save-graphic ((graphic image-xy) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (format stream "(XLIB::MAKE-IMAGE-XY :width ~a :height ~a :depth ~a :plist " 
	  (image-width graphic) 
	  (image-height graphic)
	  (image-depth  graphic))
  (save-graphic (image-plist graphic) format stream)
  (format stream ":bitmap-list ")
  (save-graphic (image-xy-bitmap-list graphic) format stream)
  (format stream ") "))


(defmethod save-graphic ((graphic image-z) (format (eql :nested-forms))
			&optional (stream *standard-output*))
    (format stream "(XLIB::MAKE-IMAGE-Z :width ~a :height ~a :depth ~a :plist " 
	  (image-width graphic) 
	  (image-height graphic)
	  (image-depth  graphic))
  (save-graphic (image-plist graphic) format stream)
  (format stream ":bits-per-pixel ~a :pixarray " (image-z-bits-per-pixel graphic))
  (save-graphic (image-z-pixarray graphic) format stream)
  (format stream ") "))

; - - - - - - - - - - - - label - - - - - - - - - - -
(defmethod save-graphic ((graphic label) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (with-slots (label-string 
	       label-base-x 
	       label-base-y 
	       label-extent-x 
	       label-extent-y 
	       extent-height
	       extent-width 
	       label-gravity
	       label-angle) graphic
    ;; label-string is required argument to make-label - rest are keywords
    (format stream "\"~a\" :base-x ~a :base-y ~a :extent-x ~a :extent-y ~a ~
                       :extent-width ~a :extent-height ~a :gravity :~a :angle ~a "
	    label-string label-base-x label-base-y label-extent-x label-extent-y 
	    extent-width extent-height label-gravity label-angle))
  (let ((family (label-font-family graphic)))
    (when family
      (format stream ":font-family \"~a\" " (if (stringp family) family (family-name family))))))

; - - - - - - - - - - - - line - - - - - - - - - - -
(defmethod save-graphic ((graphic line) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (with-slots (start-x start-y end-x end-y) graphic
    (format stream "~a ~a ~a ~a " start-x start-y end-x end-y)))

; - - - - - - - - - - - - polypoint - - - - - - - - - - -
(defmethod save-graphic ((graphic polypoint) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (with-slots (vertices) graphic
    (save-graphic vertices format stream)))

; - - - - - - - - - - - - polyline - - - - - - - - - - -
; uses polypoint

; - - - - - - - - - - - - bspline - - - - - - - - - - -
; uses polypoint

; - - - - - - - - - - - - polygon - - - - - - - - - - -
; uses polypoint

; - - - - - - - - - - - - rectangle - - - - - - - - - - -
(defmethod save-graphic ((graphic rectangle) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (format stream " ~a ~a ~a ~a " 
	  (rectangle-origin-x graphic)
	  (rectangle-origin-y graphic)
	  (rectangle-width graphic)
	  (rectangle-height graphic))
  ;; check for graphic transforms on rectangles
  
  )

; - - - - - - - - - scene - - - - - - - - -
(defmethod save-graphic ((graphic scene) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (with-slots (elements parent) graphic
    (when elements
      (format stream ":elements (LIST ")
      ;(format stream ":elements '( ")
      (dotimes (position (fill-pointer elements))
	(save-graphic (elt elements position) :nested-forms stream))
      (format stream ") "))
    ))

;-------------------- Special cases -----------------

; - - - - - - - - - - - - view - - - - - - - - - - - -
(defmethod save-graphic ((graphic view) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (save-graphic (view-graphic graphic) format stream))

;-------------------- Built in type classes ---------------------

; - - - - - - - - - - - - array - - - - - - - - - - -
;; warning - for portability's sake I have avoided array handling functions 
;; that are implementation dependent and have only implemented saving 
;; 1 & 2 dimensional arrays (the common variety) because I could not 
;; figure out how to do this in a general manner for n dimensions 
;; (i.e. like listarray in TICL).
;; It could be done in a portable manner with a macro to construct the 
;; right number of dimensions, but I don't have the time at this writing. 
;; (TAC - 3-28-90)
(defmethod save-graphic ((graphic array) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (let ((ad (array-dimensions graphic))
	(ar (array-rank graphic))
	list-of-elements temp-list)
    (cond ((eql ar 1)
	   (setq ad (length graphic))
	   (dotimes (n ad)
	     (setf  list-of-elements (cons (elt graphic n) list-of-elements))))
	  ((eql ar 2)
	   (dotimes (n (car ad))
	     (setf temp-list nil)
	     (dotimes (m (cadr ad))
	       (setf  temp-list (cons (aref graphic n m) temp-list)))
	     (setf list-of-elements (cons (nreverse temp-list) list-of-elements))))
	  (t (error "cannot yet save an array of rank ~a" ar)))

    (format stream "(MAKE-ARRAY ")
    (save-graphic ad format stream)
    (format stream " :initial-contents ")
    (save-graphic (nreverse list-of-elements) format stream)
    (format stream ") ")

    ;(format stream "(MAKE-ARRAY ~a :initial-contents ~a) " ad (nreverse list-of-elements))
    ))

; - - - - - - - - - - - - character - - - - - - - - - - -
(defmethod save-graphic ((graphic character) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (format stream "~a " graphic))

; - - - - - - - - - - - - number - - - - - - - - - - -
(defmethod save-graphic ((graphic number) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (format stream "~a " graphic))

; - - - - - - - - - - - - symbol - - - - - - - - - - -
(defmethod save-graphic ((graphic symbol) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (if (keywordp graphic)
      (format stream ":~a " graphic)
      (format stream "~a " graphic)))

; - - - - - - - - - - - - list - - - - - - - - - - -
(defmethod save-graphic ((graphic list) (format (eql :nested-forms))
			 &optional (stream *standard-output*))
  (format stream "(LIST ")
  ;(format stream "'( ")
  (dolist (item graphic)
    (save-graphic item format stream))
  (format stream ") "))

; - - - - - - - - - - - - vector - - - - - - - - - - -
;; done as one dimensional array

; - - - - - - - - - - - - string - - - - - - - - - - -
(defmethod save-graphic ((graphic string) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (format stream "\"~a\" " graphic))

; - - - - - - - - - - - - integer - - - - - - - - - - -
(defmethod save-graphic ((graphic integer) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (format stream "~a " graphic))

; - - - - - - - - - - - - float - - - - - - - - - - -
(defmethod save-graphic ((graphic float) (format (eql :nested-forms))
			&optional (stream *standard-output*))
  (format stream "~a " graphic))

