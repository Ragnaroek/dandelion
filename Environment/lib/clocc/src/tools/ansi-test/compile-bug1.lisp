(in-package  :cl-user)

;; from Douglas Thomas Crosher <dtc@seagull.cons.org>

;;; Compiling this file caused a type error in
;;; maybe-convert-to-assignment due to a continuation-dest being
;;; null. One of the refs to the inlined definition of nthcdr had been
;;; marked for deletion, but this was seen. The path is ir1-optimize,
;;; flush-dead-code; delete-ref; then maybe-convert-to-assignment.
;;;
;;; After patching maybe-convert-to-assignment to abort in this case,
;;; the code compiles but compiles to a type error. Seems to be a
;;; problem propagating the argument type to the inlined nthcdr
;;; function?
;;;

(declaim (optimize (space 0) (debug 2)
                   (c::compilation-speed 0) (speed 0)
                   (c::brevity 0) (safety 0)))

(proclaim '(inline wrappers (setf wrappers)))
(defun wrappers (structure)
  (declare (type list structure))
  (elt structure 2))
(defun (setf wrappers) (new-value structure)
  (declare (type list structure))
  (setf (elt structure 2) new-value))

(defun tst (x)
  (let ((wrappers (prog1 (wrappers x) (setf (wrappers x) nil)))
        (fns (nthcdr 0 x)))
    (car (nthcdr 0 x))))
