(in-package :cl-user)

;;; From: "Fernando D. Mato Mira" <matomira@iname.com>

(defun prolog-length (p)
  (let ((x (length (car p))))
    (reduce #'(lambda (v1 v2)
		(declare (ignore v1))
		(setq x (+ x (length v2))))
	    p)))

