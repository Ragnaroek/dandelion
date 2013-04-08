;;; -*- Mode: CLtL -*-

(in-package "MK4")

(require :run-unix-program)

(defun run-program (program &rest arguments)
  (poplog:run-program program :args arguments))

;;; end of file -- poplog.lisp --
