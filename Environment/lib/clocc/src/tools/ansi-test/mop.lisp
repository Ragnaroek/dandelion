;;; based on v1.1.1.1 -*- mode: lisp -*-
(in-package :cl-user)
;; Test some MOP-like CLOS features

#+clisp
(check-for-bug :mop-legacy-6
  (progn
    (defstruct rectangle1 (x 0.0) (y 0.0))
    (defclass counted1-class (structure-class)
              ((counter :initform 0)) #+CLISP (:metaclass structure-class))
    (defclass counted1-rectangle (rectangle1) () (:metaclass counted1-class))
    (defmethod make-instance :after ((c counted1-class) &rest args)
      (incf (slot-value c 'counter)))
    (slot-value (find-class 'counted1-rectangle) 'counter)
    (make-instance 'counted1-rectangle)
    (slot-value (find-class 'counted1-rectangle) 'counter)
    )
  1)

#+clisp
(check-for-bug :mop-legacy-21
  (progn
    (defclass rectangle2 ()
              ((x :initform 0.0 :initarg x) (y :initform 0.0 :initarg y)))
    (defclass counted2-class (standard-class)
              ((counter :initform 0)) #+CLISP (:metaclass structure-class))
    (defclass counted2-rectangle (rectangle2) () (:metaclass counted2-class))
    (defmethod make-instance :after ((c counted2-class) &rest args)
      (incf (slot-value c 'counter)))
    (slot-value (find-class 'counted2-rectangle) 'counter)
    (make-instance 'counted2-rectangle)
    (slot-value (find-class 'counted2-rectangle) 'counter)
    )
  1)
