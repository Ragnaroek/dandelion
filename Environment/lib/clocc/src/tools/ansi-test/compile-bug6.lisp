(in-package :cl-user)

(defclass super1 () ())

(defclass sub1 (super1)())

(defun fooey ()
  (make-instance 'sub1))

