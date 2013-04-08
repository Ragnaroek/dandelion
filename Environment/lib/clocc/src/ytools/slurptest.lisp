;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(eval-when (:compile-toplevel :load-toplevel)
   (def-slurp-task :slurptest-def
       :default (\\ (_ s)
		   (incf (aref s 0))
		   false)
       :file->state-fcn
	   (\\ (_) (vector 0 !())))

   (def-slurp-task :slurptest-nodef

       :file->state-fcn
	   (\\ (_) (vector 0 !())))
)

(datafun :slurptest-def fribble
   (defun :^ (e v)
      (push e (aref v 1))
      false))

(datafun :slurptest-def garble
   (defun :^ (e v)
      (setf (aref v 1)
	    (cons (- (cadr e))
		  (aref v 1)))
      false))

(datafun :slurptest-nodef fribble
   (defun :^ (e v)
      (push e (aref v 1))
      false))

(datafun :slurptest-nodef garble
   (defun :^ (e v)
      (setf (aref v 1)
	    (cons (- (cadr e))
		  (aref v 1)))
      false))

(defun slurp-test (&optional (file "slurptest.dat")
			     (tasks (list slurptest-def*
					  slurptest-nodef*))
			     (target
			        '(#(1 ((fribble two)
				       5 -3
				       (fribble one)))
				  #(0 ((fribble two)
				       5 -3
				       (fribble one))))))
   (setq file (->pathname file))
   (let ((result (file-slurp file tasks false)))
      (cond ((equalp result target)
	     (format t "Slurp-test okay~%")
	     true)
	    (t
	     (format t !"Slurp-test failure!  Wanted: ~s~
		       ~%                        Got: ~s~%"
		     target result)
	     false))))