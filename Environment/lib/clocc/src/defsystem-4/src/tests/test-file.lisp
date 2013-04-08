;;; -*- Mode: Lisp -*-

;; MK4 test file.

(eval-when (:load-toplevel)
  (print ";;; Loading test file."))

(eval-when (:load-toplevel :execute)
  (print ";;; Loading/executing test file."))

(eval-when (:compile-toplevel)
  (print ";;; Compiling test file."))
