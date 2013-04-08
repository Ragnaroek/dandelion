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


;Vectors for coordinate storage
;  These are used to reduce cons'ing during extent computations.
;; CMUCL porting note. The use of adjustable array is not very efficient,
;; but is pervasive in this code. For now, just leave it alone.
(defvar *extent-vector*
  (make-array '(8) :adjustable t :fill-pointer t))

;;;  Extent-cache Mixin Definition:
(EXPORT '(
	  make-extent-rect
	  extent-rect-xmin
	  extent-rect-ymin
	  extent-rect-xmax
	  extent-rect-ymax
	  extent
	  extent-valid-p
	  valid-extent-p
	  extent-changed
	  extent-copy
	  extent-transform
	  extent-move
	  extent-combine
	  ))

(defclass extent-cache ()
  ((extent
    :type	extent-rect
    :initform	(make-extent-rect)
    :accessor	extent
    :documentation
    "Defines the minimum containing rectangle in object coordinates of a graphic")
   
   (extent-valid-p
    :type	boolean
    :initform	nil
    :reader	extent-valid-p
    :documentation
    "Because of caching, is the extent valid?")
   )
  
  (:documentation
   "Mixin for efficient handling of extents on a first in,
    first out presistent stack"))


;Method: extent-changed
;  The given EXTENT-CACHE may have changed.  Invalidate it and notify its parent.
;  No useful value is returned.

(defmethod extent-changed ((extent-cache extent-cache))

  (with-slots (extent-valid-p) extent-cache
    (setf extent-valid-p nil)
    (when (graphic-parent extent-cache)
      (extent-changed (graphic-parent extent-cache)))))


;Function: extent-combine
;  Combine EXTENT-1 with EXTENT-2 by computing their union (the smallest
;  rectangle that encloses both).  If either extent-rect is undefined (nil)
;  then the result is also nil.  Put the result in EXTENT-2 and return it.

(defun extent-combine (extent-1 extent-2)
  (declare (type (or null extent-rect) extent-1 extent-2))

    (if (and extent-1 extent-2)	; Are they both there?
	(if (and (extent-rect-valid extent-1)(extent-rect-valid extent-2))
	    (psetf				; Yes, combine them
	     (extent-rect-xmin extent-2) (min (extent-rect-xmin extent-2)
					      (extent-rect-xmin extent-1))
	     (extent-rect-xmax extent-2) (max (extent-rect-xmax extent-2)
					      (extent-rect-xmax extent-1))
	     (extent-rect-ymin extent-2) (min (extent-rect-ymin extent-2)
					      (extent-rect-ymin extent-1))
	     (extent-rect-ymax extent-2) (max (extent-rect-ymax extent-2)))
	    (progn
	      (warn "Attempt to combine invalid EXTENT-RECT ~a"
		    (if (extent-rect-valid extent-1) extent-2 extent-1))
	      (if (extent-rect-valid extent-1)
		  (setf extent-2 extent-1))))
	(when extent-2			; Make result undefined if necessary
	  (setf extent-2 nil)))

    extent-2)				; Return the combined extent


;Function: extent-copy
;  Copy SRC into DEST.  Both extents-rects should exist.  DEST is returned.

(defun extent-copy (src dest)
  (declare (type extent-rect src dest))
  (psetf (extent-rect-xmin dest) (extent-rect-xmin src)
         (extent-rect-xmax dest) (extent-rect-xmax src)
         (extent-rect-ymin dest) (extent-rect-ymin src)
         (extent-rect-ymax dest) (extent-rect-ymax src)
	 (extent-rect-valid dest)(extent-rect-valid src))

  dest)


;Method: graphic-extent
;  If the given EXTENT-CACHE is currently valid, return its extent-rect.
;  Otherwise, call extent-compute to actually compute the extent for the
;  EXTENT-CACHE, copy it into the cache, and then set extent-valid-p to t.
;  If the extent computation returns an undefined extent (nil) then nil is
;  returned and the cache remains invalid.

(defmethod graphic-extent ((extent-cache extent-cache))

  (with-slots ((cache extent) extent-valid-p) extent-cache
    (if extent-valid-p				; Is the cache valid?
        cache					; Yes, just return it
        (let ((new-extent			; No, compute it
                (extent-compute extent-cache)))
          (when new-extent			; Was it defined?
            (extent-copy new-extent cache)	; Yes, copy it into cache
            (setf extent-valid-p t)		; Cache is valid now
            cache)))))				; Return new extent


;Function: extent-transform
;  Apply the TRANSFORM to the EXTENT-RECT and return a transformed
;  extent-rect.  The extent is transformed as a full rectangle and then a
;  new extent is computed using the minimums and maximums from the
;  transformed rectangle.  If RESULT-EXTENT is specified, then the result
;  is placed there and returned as the function value.  If RESULT-EXTENT is
;  nil, a new extent-rect is created and used to store the result.
;  EXTENT-RECT is not modified.  Either EXTENT-RECT or TRANSFORM may be
;  nil.  A nil EXTENT-RECT means "undefined extent" and nil is returned.  A
;  nil TRANSFORM means the identity transform and a copy of EXTENT-RECT is
;  returned.

(defun extent-transform (transform extent-rect
                            &optional (result (make-extent-rect)))
  (declare (type (or null transform) transform))
  (declare (type (or null extent-rect) extent-rect))
  (SETF (FILL-POINTER *extent-vector*) 8)
  (when extent-rect		; "Undefined" transformed is still "undefined"
    (if transform		; Is it the identity transform?
        (progn			; No, apply it to extent-rect
	  ; Build vector of all four corners
          (setf (aref *extent-vector* 0) (extent-rect-xmin extent-rect)
		(aref *extent-vector* 2) (extent-rect-xmin extent-rect)
		(aref *extent-vector* 4) (extent-rect-xmax extent-rect)
		(aref *extent-vector* 6) (extent-rect-xmax extent-rect)
		
		(aref *extent-vector* 1) (extent-rect-ymin extent-rect)
		(aref *extent-vector* 3) (extent-rect-ymax extent-rect)
		(aref *extent-vector* 5) (extent-rect-ymax extent-rect)
		(aref *extent-vector* 7) (extent-rect-ymin extent-rect))
	  (transform-point-seq transform *extent-vector*)  ; Transform it
	  (setf (extent-rect-xmin result)
		(min-value-vector *extent-vector* 0)	; Find min/max
		(extent-rect-xmax result) (max-value-vector *extent-vector* 0)
		(extent-rect-ymin result) (min-value-vector *extent-vector* 1)
		(extent-rect-ymax result) (max-value-vector *extent-vector* 1)))

        (extent-copy extent-rect result)) ; Yes, just copy extent-rect

    result))						; Return new extent

        

(DEFUN extent-move (extent delta-x delta-y)

  (SETF (extent-rect-xmin extent) (+ (extent-rect-xmin extent) delta-x)) 
  (SETF (extent-rect-ymin extent) (+ (extent-rect-ymin extent) delta-y))
  (SETF (extent-rect-xmax extent) (+ (extent-rect-xmax extent) delta-x)) 
  (SETF (extent-rect-ymax extent) (+ (extent-rect-ymax extent) delta-y)))
