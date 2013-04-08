;;;  -*- mode: lisp -*-
(proclaim '(special log))
(in-package :cl-user)


;; From: Gary Bunting <gbunting@cantor.une.edu.au>

(check-for-bug :new-bugs-legacy-8
  (setf xx (expt 3 32))
  1853020188851841)

(check-for-bug :new-bugs-legacy-12
  (* xx xx)
  3433683820292512484657849089281)

;; paul

(check-for-bug :new-bugs-legacy-18
  (defun bugged (x)
    (labels ((f (y &optional trouble)	;  <<< or &key or &rest ..
	       (if y
		   (let ((a (pop y)))
		     (f a)))))))
  BUGGED)

(check-for-bug :new-bugs-legacy-26
  (defun tst ()
    (labels
        ((eff (&key trouble)
           (eff)
           ))
      ;;(eff :trouble nil)  ;<< this works
      (eff);; << this causes assert failure
      ))
  tst)


