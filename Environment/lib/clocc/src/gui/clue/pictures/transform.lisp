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
;;;
;;; Severely hacked by Paul Werkowski for efficient use on stock hardware.
;;; Change TRANSFORM class from standard-class to structure class.
;;; Change some defuns to defmethods for efficient access using PCL.
;;;
(in-package "PICTURES")



(DEFPARAMETER  *vector-cache* nil)

(export '(
	  t11 t12 t21 t22 t31 t32
	  make-transform
	  compose-transform
	  copy-transform
	  move-transform
	  radians
	  rotate-transform
	  scale-transform
	  scale-point
	  transform-point
	  transform-point-seq
	  )
	'pictures)

;;; Transform Class Definition:
;; pw--lets replace this with defstruct which is more efficient on stock HW
;; The change seems easy -- too easy?
#+nil
(eval-when (compile load eval)
(defclass transform ()
  (
   (t11		:type     single-float
                :initarg  :t11
		:reader   t11
                :initform 1s0
		:documentation "Position (1,1) in transform matrix")
   
   (t12		:type     single-float
                :initarg  :t12
 		:reader   t12
		:initform 0s0
		:documentation "Position (1,2) in transform matrix")
   
   (t21		:type     single-float
                :initarg  :t21
		:reader   t21
                :initform 0s0
		:documentation "Position (2,1) in transform matrix")
   
   (t22		:type     single-float
                :initarg  :t22
		:reader   t22
                :initform 1s0
		:documentation "Position (2,2) in transform matrix")
   
   (t31		:type     single-float
                :initarg  :t31
		:reader   t31
                :initform 0s0
		:documentation "Position (3,1) in transform matrix")
   
   (t32		:type     single-float
                :initarg  :t32
		:reader   t32
                :initform 0s0
		:documentation "Position (3,2) in transform matrix")
   )
  (:documentation
 "Represents a 3x3 homogeneous coordinate system transform matrix"))
)
(eval-when (compile load eval)
(defstruct (transform (:print-function %print-transform)
		      (:copier nil))
  (t11 1s0 :type single-float)
  (t12 0s0 :type single-float)
  (t21 0s0 :type single-float)
  (t22 1s0 :type single-float)
  (t31 0s0 :type single-float)
  (t32 0s0 :type single-float))
)
(defun %print-transform (transform stream depth)
  (declare (ignore depth))
  (print-object transform stream)
  transform)

(defmethod print-object ((transform transform) stream)
  (print-unreadable-object (transform stream :type t :identity t)))

;; some porting help - change names in files later
(macrolet ((help (name)
	     `(defmethod ,name ((transform transform))
		(with-slots (,name) transform
		  ,name))))
  (help t11)
  (help t12)
  (help t21)
  (help t22)
  (help t31)
  (help t32))
	
;Function: make-transform
;  Create a new transform object.  With no initargs, this creates an
;  identity transform.
#+nil ;; pw--handled by defstruct
(defun make-transform (&rest initargs
                         &key &allow-other-keys)

  (apply #'make-instance 'transform initargs))


;;; @@@move to macros
(defmacro with-defstruct-slots (vars (obj class) &body body)
  "Like WITH-SLOTS but for things of STRUCTURE-CLASS only. Allows
   efficent uses of optimized DEFSTRUCT slot accessors outside of
   method bodies."
    (labels ((slot-ref (obj slot class)
	       `(,(intern
		   (concatenate 'string (string class) "-" (string slot)))
		 ,obj))
	     (varform (v)
	       (etypecase v
		 (symbol
		  `(,v ,(slot-ref obj v class)))
	       (list
		`(,(car v) ,(slot-ref obj (cadr v) class))))))
      `(symbol-macrolet ,(mapcar #'varform vars)
	 ,@body)))

;Private Function: post-mult
;  Change the RESULT transform to be the product of (X x Y), where Y is a
;  homogeneous matrix defined by Y11, Y12, ...  If RESULT is nil, create a
;  new transform for the result.  The new value of RESULT is returned.
;;
;; pw-- This was previously a defun at end of file.
;; Made to use special WITH-SLOTS like macro for efficient use
;; of defstruct slot accessors outside of defmethod thus allowing
;; inlining.
;;
;; Private to this file.

(declaim (#+cmu ext:maybe-inline #-cmu inline post-mult))
(defun post-mult (x y11 y12 y21 y22 y31 y32 &optional (result x))
  (declare (type transform x)
	   (type (or null transform) result))
  (declare (type single-float y11 y12 y21 y22 y31 y32))
  (let ((result (or result (make-transform))))
    (declare (type transform result))
    (with-defstruct-slots
     ((x11 t11) (x12 t12) (x21 t21) (x22 t22) (x31 t31) (x32 t32))
     (X transform)
     (with-defstruct-slots 
      ((z11 t11) (z12 t12) (z21 t21)
       (z22 t22) (z31 t31) (z32 t32))
      (result transform)
      
      (let ((temp11 x11) ; Use temporaries in case RESULT and X are the same.
	    (temp21 x21)
	    (temp31 x31))
	
	(psetq z11 (+ (* temp11 y11) (* x12 y21))	; Compute first row
	       z12 (+ (* temp11 y12) (* x12 y22))
	       
	       z21 (+ (* temp21 y11) (* x22 y21))	; Compute second row
	       z22 (+ (* temp21 y12) (* x22 y22))
	       
	       z31 (+ (* temp31 y11) (* x32 y21) y31)	; Compute third row
	       z32 (+ (* temp31 y12) (* x32 y22) y32)))))
    result))
  
;Private Variable: temp-transform
;  Used in compose-transform to hold temporary result
(defvar *temp-transform* (make-transform))
(proclaim '(type transform *temp-transform*))

;Function: compose-transform
;  Change the RESULT transform to be the product of (TRANSFORM-1 x
;  TRANSFORM-2).  If RESULT is not given, then the result replaces
;  TRANSFORM-2.  If both TRANSFORM-2 and RESULT are nil, then a new
;  transform is created to hold the result.  A nil transform represents the
;  identity transform.  The new value of RESULT is returned.
#+nil
(defun compose-transform (transform-1 transform-2
                          &optional (result transform-2))
  (declare (type (or null transform) transform-1 transform-2 result))
  (cond ((null transform-1)			; T-1 is the identity
         (copy-transform transform-2 result))	;   Just use T-2
        ((null transform-2)			; T-2 is the identity
         (copy-transform transform-1 result))	;   Just use T-1
        ((eq transform-2 result)		; T2 and RESULT are the same
         (with-slots (t11 t12 t21 t22 t31 t32) transform-2
           (post-mult transform-1 t11 t12 t21 t22 t31 t32 *temp-transform*)
           (copy-transform *temp-transform* result))) ;   Use temporary result
        (t					; Otherwise, compose them
         (with-slots (t11 t12 t21 t22 t31 t32) transform-2
           (post-mult transform-1 t11 t12 t21 t22 t31 t32 result)))))

;;; pw-- Ensure PCL can optimize structure-class access by removing
;;; ambiguity from argument types. This, plus inlined post-mult
;;; effectively removes consing.

(defgeneric compose-transform (transform-1 transform-2 &optional result))

(defmethod compose-transform (transform-1 transform-2
					  &optional (result transform-2))
  (declare (ignore transform-1))
  ;; Both transforms are the NIL Identity transform. Result might not be.
  (if result
      (copy-transform transform-2 result) ; return Identity transform
      nil))

(defmethod compose-transform (transform-1 (transform-2 transform)
					  &optional (result transform-2))
  (declare (ignore transform-1))
  (copy-transform transform-2 result))

(defmethod compose-transform ((transform-1 transform) transform-2
			      &optional (result transform-2))
  (copy-transform transform-1 result))


(defmethod compose-transform ((transform-1 transform)
			      (transform-2 transform)
			      &optional (result transform-2))
  (declare (type transform transform-1 transform-2))
  (locally (declare (inline post-mult))
    (cond ((eq transform-2 result) ; T2 and RESULT are the same
	   (with-slots (t11 t12 t21 t22 t31 t32) transform-2
	     (post-mult transform-1 t11 t12 t21 t22 t31 t32 *temp-transform*)
	     (copy-transform *temp-transform* result))) ;   Use temporary result
	  (t ; Otherwise, compose them
	   (with-slots (t11 t12 t21 t22 t31 t32) transform-2
	     (post-mult transform-1 t11 t12 t21 t22 t31 t32 result))))))

;Function: copy-transform
;  Copy transform-1 into transform-2.  Either or both can be nil.  Return
;  the new transform-2.
#+nil
(defun copy-transform (transform-1 transform-2)
  (declare (type (or null transform) transform-1 transform-2))
  (cond ((eq transform-1 transform-2))		; They are already identical!
        (transform-1				; T-1 is not identity
         (unless transform-2
           (setf transform-2 (make-transform)))	;   Must make T-2 first
         (with-slots (t11 t12 t21 t22 t31 t32) transform-1
           (with-slots ((y11 t11) (y12 t12) (y21 t21) (y22 t22)
			(y31 t31) (y32 t32)) transform-2
             (psetf y11 t11 y12 t12 y21 t21 y22 t22 y31 t31 y32 t32))))
        (t					; T-1 is the identity
         (if transform-2
             (with-slots (t11 t12 t21
			  t22 t31 t32) transform-2	; Make T-2 the identity
               (psetf t11 1s0 t12 0s0 t21 0s0 t22 1s0 t31 0s0 t32 0s0))
             (setf transform-2 (make-transform))))) ; Or create one if not there yet

  transform-2)					; Return T-2

(defgeneric copy-transform (transform-1 transform-2))
(defmethod  copy-transform (transform-1 transform-2)
  ;; both args NIL
  (declare (ignore transform-1))
  transform-2)

(defmethod copy-transform ((transform-1 transform) transform-2)
  (declare (ignore transform-2))
  ;; T2 is NIL identity so make a real one.
  (let ((transform-2 (make-transform)))
    (declare (type transform transform-1 transform-2))
    (with-slots (t11 t12 t21 t22 t31 t32) transform-1
      (with-slots ((y11 t11)(y12 t12)(y21 t21)(y22 t22)(y31 t31)(y32 t32))
	transform-2
	(psetf y11 t11 y12 t12 y21 t21 y22 t22 y31 t31 y32 t32)))
    transform-2))

(defmethod copy-transform (transform-1 (transform-2 transform))
  (declare (ignore transform-1))
  (with-slots (t11 t12 t21 t22 t31 t32) transform-2	; Make T-2 the identity
    (psetf t11 1s0 t12 0s0 t21 0s0 t22 1s0 t31 0s0 t32 0s0))
  transform-2)

(defmethod copy-transform ((transform-1 transform)(transform-2 transform))
  (declare (type transform transform-1 transform-2))
  (unless (eq transform-1 transform-2)
    (with-slots (t11 t12 t21 t22 t31 t32) transform-1
      (with-slots ((y11 t11)(y12 t12)(y21 t21)(y22 t22)(y31 t31)(y32 t32))
	transform-2
	(psetf y11 t11 y12 t12 y21 t21 y22 t22 y31 t31 y32 t32))))
  transform-2)

(defmethod move-transform ((transform transform) delta-x delta-y)
  (declare (type transform transform))

  (with-slots (t31 t32) transform		; Just translate the transform
    (psetq t31 (+ t31 delta-x)
           t32 (+ t32 delta-y)))
  transform)					; Return the modified transform


;Method: print-object
;   Print a transform object

(defmethod print-object :after ((transform transform) stream)
  (with-slots (t11 t12 t21 t22 t31 t32) transform
    (format 
     stream
     "~&[|~6,2f  ~6,2f  ~6,2f|~% |~6,2f  ~6,2f  ~6,2f|~% |~6,2f  ~6,2f  ~6,2f|]~%"
     t11 t12 0s0 t21 t22 0s0 t31 t32 1s0)))


;Macro: radians
;  Convert degrees to radians using the same floating point precision

(defmacro radians (degrees)
 
 `(* ,degrees (/ pi  180)))


;Method: rotate-transform
;  Modify the TRANSFORM, rotating the previous transformation by the given
;  ANGLE (in radians) around the given fixed point. The new value of the
;  TRANSFORM is returned.

(defmethod rotate-transform ((transform transform) angle
                               &optional (fixed-x 0) (fixed-y 0))
  (declare (type transform transform)
	   (inline post-mult))

  (let* ((cos-angle (cos angle))	; Implementation note:
         (sin-angle (sin angle))	; (cis angle) is NOT faster on Explorer!
         (origin-fixed (and (zerop fixed-x) (zerop fixed-y)))
         (trans-x (if origin-fixed ; Translate only if fixed-point is not origin
                      0s0
                      (+ (* fixed-x (- 1 cos-angle)) (* fixed-y sin-angle))))
         (trans-y (if origin-fixed
                      0s0
                      (- (* fixed-y (- 1 cos-angle)) (* fixed-x sin-angle)))))
    (with-coercion ((cos-angle sin-angle trans-x trans-y) single-float)
      (post-mult transform	; Translate to origin, rotate, translate back
		 cos-angle	sin-angle
		 (- sin-angle) 	cos-angle
		 trans-x	trans-y))))


;Method: scale-transform
;  Modify the TRANSFORM, scaling the previous transformation by the given
;  scale factors around the given fixed point. The new value of the
;  TRANSFORM is returned.

(defmethod scale-transform ((transform transform) scale-x scale-y
                  &optional (fixed-x 0s0) (fixed-y 0s0))
  (declare (inline post-mult))
  (with-coercion ((scale-x scale-y fixed-x fixed-y) single-float)
    (let* ((origin-fixed
	    ; Translate only if fixed point is not origin
	    (and (zerop fixed-x)
		 (zerop fixed-y)))
	   (trans-x (if origin-fixed
			0s0
			(* fixed-x (1- scale-x))))
	   (trans-y (if origin-fixed
			0s0
			(* fixed-y (1- scale-y)))))
      (post-mult transform	; Translate to origin, scale, translate back
		 scale-x	0s0
		 0s0		scale-y
		 trans-x	trans-y))))

;Method: scale-point
;  Return the result of applying TRANSFORM to the given X-DISTANCE and
;  Y-DISTANCE.
;;
(defmethod scale-point ((transform NULL) x-distance y-distance)
  (declare (ignore transform))
  ;; Identity transform. Do nothing.
  (with-coercion ((x-distance y-distance) single-float)
    (values x-distance y-distance)))

(defmethod scale-point ((transform transform) x-distance y-distance)
  (declare (type transform transform))
  (declare (type wcoord x-distance y-distance))

  (with-coercion ((x-distance y-distance) single-float)
    (with-slots (t11 t12 t21 t22) transform
      (let ((x-scale (sqrt (+ (* t11 t11) (* t12 t12))))
	    (y-scale (sqrt (+ (* t21 t21) (* t22 t22)))))
      (values (* x-distance x-scale)	; new-x-distance
	      (* y-distance y-scale))))	; new-y-distance
    (values x-distance y-distance)))		;  Yes, old-x, old-y


;Method: transform-point
;  Return the result of applying TRANSFORM to the given point.

(DEFMETHOD  transform-point ((transform transform) x y)
  (declare (type transform transform))
  (declare (type wcoord x y))

  (with-coercion ((x y) single-float)
    (with-slots (t11 t12 t21 t22 t31 t32) transform
      (values (+ (* x t11) (* y t21) t31)
	      (+ (* x t12) (* y t22) t32)))))

(DEFMETHOD  transform-point ((transform t) x y)
  (declare (type wcoord x y))

  ;; Identity transform
  (with-coercion ((x y) single-float)
    (values x y)))

;Method: transform-point-seq
;  Destructively changes the point-seq by applying TRANSFORM to the
;  given points.  

(defmethod transform-point-seq ((transform t) point-vector
				&optional (result point-vector))
  (copy-to-point-seq point-vector result))

(defmethod transform-point-seq ((transform transform)  point-vector
				&optional (result point-vector))
  (declare (type transform transform))
  (declare (type (or null vector) point-vector )) 
  (with-vector transformed-vector
    (LET* ((vector-length (LENGTH point-vector))) ; How many pairs are there?
      (with-slots (t11 t12 t21 t22 t31 t32) transform
	(let ((x11 t11)	; Store transform in local vars for efficiency
	      (x12 t12)
	      (x21 t21)
	      (x22 t22)
	      (x31 t31)
	      (x32 t32))
	  (IF (AND (= x11 x22 1s0) (= x12 x21 0s0))
	      (do ((i 0 (+ i 2)))
		  ((>= i  vector-length) nil)
		(let ((x-i (coerce (ELT point-vector       i) 'single-float))
		      (Y-i (coerce (ELT point-vector (+ 1 i)) 'single-float)))
		  (vector-push-extend (+  x-i x31) transformed-vector)
		  (vector-push-extend (+  y-i x32) transformed-vector)))
	      (do ((i 0 (+ i 2)))
		  ((>= i  vector-length) nil)
		(let ((x-i (coerce (ELT point-vector       i) 'single-float))
		      (y-i (coerce (ELT point-vector (+ 1 i)) 'single-float)))
		  (vector-push-extend 
		   (+ (* x-i x11) (* y-i x21) x31) transformed-vector)
		  (vector-push-extend
		   (+ (* x-i x12) (* y-i x22) x32) transformed-vector)))))
	(copy-to-point-seq transformed-vector result)))))

(DEFUN get-global-vector ()
  "return a reusable vector from the a global *vector-cache*. If the
   fillpointer for a vector is 0, it is available"
  (DOLIST
      (VECTOR *vector-cache*
	      (PROGN
		(PUSH (cons (make-array '(10) :adjustable t :fill-pointer 0 ) 1)
		      *vector-cache*)
		(CAAR *vector-cache*)))
    (WHEN (= (cdr vector) 0)
      (SETF (CDR vector) 1)(RETURN (car vector)))))

(DEFUN return-global-vector (avector)
  (SETF (FILL-POINTER avector) 0)
  (SETF (CDR (ASSOC avector *vector-cache*)) 0))

