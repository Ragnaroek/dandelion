;;; -*- Mode: CLtL -*-

(in-package "MK4")

(defun run-program (program
		    &key
		    arguments
		    (error-output *error-output*)
		    )
  (apply #'lisp:execute program arguments))


(defmethod run-os-program ((program string)
			   &key
			   (arguments ())
			   (input nil)
			   (output :terminal)
			   (error-output t)
			   &allow-other-keys)
  (declare (ignore error-output))  
  (lisp:run-program program
		    :arguments arguments
		    :output output
		    :input input))


;;; end of file -- clisp.lisp --
