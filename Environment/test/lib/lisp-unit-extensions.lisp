;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Erweiterung zu Lisp-Unit
;++
;+++++++++++++++++++++++++++++++++++++++++++++

(in-package :cl-user)

(defpackage :de.fh-trier.test.extensions
  (:use #:common-lisp 
        #:lisp-unit)
  (:export #:define-guarded-test
           #:define-teardown
           #:define-setup))

(in-package #:de.fh-trier.test.extensions)

(defparameter *setup* (make-hash-table))
(defparameter *teardown* (make-hash-table))

(defmacro define-setup (&body body)
  `(set-setup (lambda () ,@body)))

(defmacro define-teardown (&body body)
  `(set-teardown (lambda () ,@body)))

(defmacro define-guarded-test (name &body body)
  `(progn
     (check-guards-set)
     (define-test ,name
       (funcall (get-setup))
       ,@body
       (funcall (get-teardown)))))

(defun check-guards-set ()
  (when (or (not (get-setup)) (not (get-teardown)))
    (error "setup and/or teardown not defined")))

(defun set-setup (function)
  ;jedes paket darf eigene setup und teardown funktion haben
  (setf (gethash *package* *setup*) function))

(defun get-setup ()
  (gethash *package* *setup*))

(defun set-teardown (function)
  (setf (gethash *package* *teardown*) function))

(defun get-teardown () 
  (gethash *package* *teardown*))


  