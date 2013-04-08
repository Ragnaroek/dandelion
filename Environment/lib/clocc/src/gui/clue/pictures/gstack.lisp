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


;Parameter: *graphic-stack-initial-size*
(defparameter *graphic-stack-initial-size* 10
  "Initial number of elements in the graphic stack")

;Parameter: *graphic-stack-increment*
(defparameter *graphic-stack-increment* 5
  "Number of elements to add when expanding the graphic stack")


;;; Graphic-stack Class Definition:
(defclass graphic-stack ()
  ((stack
    :type       vector
    :initform	(make-array (list *graphic-stack-initial-size*)
			    :adjustable	  t
			    :fill-pointer 0)
    :documentation
    "Dynamically expandable stack of (graphic . object) pairs")))


;Constants
;  These should not be modified!!
(DEFPARAMETER  *identity-transform* (make-transform))
(DEFPARAMETER  *empty-gstate* (make-gstate))


;Function: graphic-stack-empty-p
;  Determine if the given GRAPHIC-STACK is empty.

(proclaim '(inline graphic-stack-empty-p))
(defun graphic-stack-empty-p (graphic-stack)

  (zerop (fill-pointer (slot-value graphic-stack 'stack))))


;Method: graphic-stack-fill
;  Empty the given GRAPHIC-STACK and then re-fill it by pushing each of
;  GRAPHIC's ancestors onto the stack in order beginning with the root
;  ancestor and ending with GRAPHIC itself.
  
(defmethod graphic-stack-fill ((graphic-stack graphic-stack) graphic)
  ;;(format t "gs-fill ~a~%" graphic)
  (graphic-stack-purge graphic-stack)	; Empty the stack
  (when graphic				; Nil graphic means end of recursion
    (graphic-stack-fill graphic-stack
			(graphic-parent graphic)) ; Recurse on graphic's parent
    ;;(format t "gs-fill pushing ~a~%" graphic)
    (graphic-stack-push graphic-stack graphic))) ; Push the graphic onto stack


;Method: graphic-stack-find
;  Try to find the given GRAPHIC or its parent on the given GRAPHIC-STACK.
;  If the GRAPHIC is found, return the pair containing it and make that the
;  top of the stack.  If the parent is found, make that the top of the
;  stack, push the GRAPHIC onto the stack and return the resulting pair.
;  If neither is found, purge the stack and rebuild it by pushing each of
;  the ancestors of GRAPHIC starting with its root ancestor and ending with
;  the GRAPHIC itself.  Return the pair containing the GRAPHIC.
  
(defmethod graphic-stack-find ((graphic-stack graphic-stack) graphic)
  ;;(format t "gsf <~a> ~a~%" graphic-stack graphic)(finish-output)
  (do ((parent (graphic-parent graphic))	; Find the parent (for speed)
       (top-graphic				; Loop variable
         (graphic-stack-top-graphic graphic-stack) ; Initialize to top of stack
         (graphic-stack-top-graphic		; Step by popping stack
           (graphic-stack-pop graphic-stack))))
      ((cond					; Termination conditions
         ((eq top-graphic graphic))		; Graphic found, do nothing
         ((eq top-graphic parent)		; Parent found,
          (graphic-stack-push graphic-stack graphic)) ; Push graphic onto stack
         ((graphic-stack-empty-p graphic-stack)	; Stack is empty,
          (graphic-stack-fill graphic-stack graphic)))	;   Fill it up
       
       (graphic-stack-top graphic-stack))))	; Return the top entry


;Method: graphic-stack-pop
;  Pop the given GRAPHIC-STACK and return the resultant graphic-stack.  If
;  the "object" is non-nil, set save-object to point to it so that we can
;  reuse it next time.  If GRAPHIC-STACK is empty, this function has no
;  effect.

(defmethod graphic-stack-pop ((graphic-stack graphic-stack))

  (with-slots (stack) graphic-stack
    (unless (graphic-stack-empty-p graphic-stack)
      (vector-pop stack))
    graphic-stack))


;Method: graphic-stack-purge
;  Pop the GRAPHIC-STACK until the given GRAPHIC is found and then pop that
;  entry as well.  If the GRAPHIC is not on the stack, then pop everything.
;  Return the resultant graphic-stack.  Note that passing nil for GRAPHIC
;  results in clearing everything off the stack.

(defmethod graphic-stack-purge ((graphic-stack graphic-stack) &optional graphic)

  (if (graphic-stack-empty-p graphic-stack)	; For an empty stack,
      graphic-stack				; just return it.
      (if (eq graphic (graphic-stack-top-graphic graphic-stack)) ; If graphic is on top
          (graphic-stack-pop graphic-stack)	; Just pop and return stack
          (graphic-stack-purge (graphic-stack-pop graphic-stack) ; Otherwise, continue looking
                               graphic))))


;Method: graphic-stack-push
;  Push the given GRAPHIC onto the given GRAPHIC-STACK.  The second element
;  of the pair is initially unmodified.  Expand the stack if needed (in
;  which case the second element is initially nil).  Return the newly
;  pushed pair.

(defmethod graphic-stack-push ((graphic-stack graphic-stack) graphic)
  (declare (type graphic graphic))

  (with-slots (stack) graphic-stack
    (let* ((stack-pointer (fill-pointer stack))	; Locals for stack pointer
	   (current-size  (array-total-size stack)) ; and current array size
           (top-entry     (and (plusp stack-pointer)
			       (AREF stack (1- stack-pointer)))))

      ;;(format t "(gsp <~a ~d> ~a) ~a~%" stack stack-pointer graphic top-entry)

      ;; Apparently this code was written on a machine
      ;; where default value of array elements was NIL. I keep
      ;; finding 0 where a cons is expected!
      
      (when (eql stack-pointer current-size)	; If stack will be full,
        (adjust-array
          stack
          (list (+ current-size *graphic-stack-increment*)))) ; Expand it first.
      (if (null top-entry)			; Is entry already there?
	  (progn
	    (vector-push (cons (cons graphic nil) nil) stack) ; No, just push one
	    ;;(format t "gsp returning ~a~%" (car (aref stack 0)))
	    (car (aref stack 0)))
	  #+nil
          (progn
	    (setf (caar top-entry) graphic) ; Yes, change the graphic part
	    (if (cdar top-entry)		; Is the "object" there?
		(setf (cdr top-entry) (cdar top-entry)) ; Yes, save it away
		(setf (cdar top-entry) (cdr top-entry))) ; No, use saved object
	    (incf (fill-pointer stack)))	; Increment stack pointer.

          (progn
	    ;;(format t "gsp changing ~a to " top-entry)
	    (setf (caar top-entry) graphic) ; Yes, change the graphic part
	    ;;(format t "~a~%" top-entry)
	    (if (cdar top-entry)		; Is the "object" there?
		(setf (cdr top-entry) (cdar top-entry)) ; Yes, save it away
		(setf (cdar top-entry) (cdr top-entry))) ; No, use saved object
	    #+nil (incf (fill-pointer stack))
	    #+nil(vector-push nil stack)
	    #+nil(format t "gsp now  <~a ~d>~%" stack stack-pointer)
	    #+nil(format t "gsp returning ~a~%"
		    (car (aref stack (1- (fill-pointer stack)))))
	    ; Return the pair we just pushed
	    ;;(break)
	    (car (AREF stack (1- (fill-pointer stack)))))

      ))))


;Function: graphic-stack-top
; Return the top entry on the given GRAPHIC-STACK

(defun graphic-stack-top (graphic-stack)
  
  (with-slots (stack) graphic-stack
    (let ((stack-pointer (fill-pointer stack)))		; Local for stack pointer

      (if (zerop stack-pointer)				; Is stack empty?
          nil						; Yes, return nil
          (car (AREF stack (- stack-pointer 1)))))))	; No, return top entry


;Function: graphic-stack-top-graphic
; Return the graphic part of the top entry on the given GRAPHIC-STACK

(defun graphic-stack-top-graphic (graphic-stack)
  
  (with-slots (stack) graphic-stack
    (let ((stack-pointer (fill-pointer stack)))		; Local for stack pointer

      (if (zerop stack-pointer)				; Is stack empty?
          nil						; Yes, return nil
          (caar (AREF stack (- stack-pointer 1)))))))	; No, return top graphic


;Method: print-object
;  Print the given GRAPHIC-STACK bottom to top.

(defmethod print-object ((graphic-stack graphic-stack) stream)
  (with-slots (stack) graphic-stack
    (dotimes (i (length stack))
      (PRINT (CAAR (ELT stack i)) stream)
      (print (cdar (elt stack i)) stream)
      (print '--------------------------- stream))))




;;;Transform-stack Class Definition:

(defclass transform-stack (graphic-stack) ()
  (:documentation "A graphic stack for transform objects"))




;Method: graphic-stack-push
;  Push the given GRAPHIC and its fully composed transform onto the given
;  TRANSFORM-STACK.  If a transform already exists in the pushed pair, it
;  is reused.  Otherwise a new transform is created.  Return the newly
;  pushed pair.

(defmethod graphic-stack-push :around ((transform-stack transform-stack) graphic)

  (let* ((current-transform			; Get top transform from stack
           (cdr (graphic-stack-top transform-stack)))
         (graphics-transform			; Get the graphic's transform
           (graphic-transform graphic))
         (new-pair				; Push a new pair and remember it
           (call-next-method)))
    (setf (cdr new-pair)			; Change the stack transform	
          (if (or current-transform graphics-transform) ; Is anything there?
              (compose-transform graphics-transform ; Compose the graphic's transform
                                 current-transform ; With the current transform
                                 (cdr new-pair)) ; Put result on pair
              nil))				; Nope, just make it nil

    new-pair))					; Return the new pair


;Macro: graphic-stack-transform
;  Return the transform part of a transform-stack entry.

(defmacro graphic-stack-transform (stack-entry)
  `(cdr ,stack-entry))





;;;Gstate-stack Class Definition:

(defclass gstate-stack (graphic-stack) ()
  (:documentation "A graphic stack for gstate objects"))

(defclass edge-gstate-stack (gstate-stack) ()
  (:documentation "A graphic stack for gstate objects"))

(DEFPARAMETER *gstate-stack* (make-instance 'gstate-stack))
(DEFPARAMETER *edge-gstate-stack* (make-instance 'edge-gstate-stack))

;Method: graphic-stack-push
;  Push the given GRAPHICS and its fully combined gstate onto the given
;  GSTATE-STACK.  If a gstate already exists in the pushed pair, it is
;  reused.  Otherwise a new gstate is created.  Return the newly pushed
;  pair.

(defmethod graphic-stack-push :around ((gstate-stack gstate-stack) graphic)
  ;;(format t "gsp :around <~a> ~a entered.~%" gstate-stack graphic)
  (let* ((current-gstate			; Get top gstate from stack
           (cdr (graphic-stack-top gstate-stack)))
         (graphics-gstate			; Get the graphic's gstate
           (graphic-gstate graphic))
         (new-pair				; Push a new pair and remember it
	  ;;; call-next-method with no args is supposed to pass orig args
	  ;;; is this a bug in cmucl?
           (call-next-method)))
    ;(format t "gsp :around ~a ~a ~a~%" current-gstate graphics-gstate new-pair)
    (setf (cdr new-pair)			; Change the stack gstate
          (if (or current-gstate graphics-gstate) ; Is anything there?
              (gstate-combine graphics-gstate	; Compose current gstate
                              current-gstate	; With the graphic's gstate
                              (cdr new-pair))	; Put result on pair
              nil))				; Nope, just make it nil
    ;(format t "gsp :around => ~a~%" new-pair)
    new-pair))					; Return the new pair

(defmethod graphic-stack-push
	   :around ((gstate-stack edge-gstate-stack) graphic)

  (let* ((current-gstate			; Get top gstate from stack
           (cdr (graphic-stack-top gstate-stack)))
         (graphics-gstate			; Get the graphic's gstate
           (edge-gstate graphic))
         (new-pair				; Push a new pair and remember it
           (call-next-method)))
    
    (setf (cdr new-pair)			; Change the stack gstate
          (if (or current-gstate graphics-gstate) ; Is anything there?
              (gstate-combine graphics-gstate  current-gstate ; Compose current gstate
						; With the graphic's gstate
                              (cdr new-pair))	; Put result on pair
              nil))				; Nope, just make it nil

    new-pair))

;Macro: graphic-stack-gstate
;  Return the gstate part of a gstate-stack entry.

(defmacro graphic-stack-gstate (stack-entry)
  `(cdr ,stack-entry))





