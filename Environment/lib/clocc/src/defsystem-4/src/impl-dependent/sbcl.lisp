;;; -*- Mode: CLtL -*-

;;; sbcl.lisp --
;;; Implementation dependent DEFSYSTEM support for SBCL.

(in-package "MK4")


;;; run-program --

(defun run-external-program (program
			     &key
			     arguments
			     (error-output *error-output*))
  (let ((process
	 (extensions:run-program program arguments :error error-output)))
    (if process
	(extensions:process-exit-code process)
	-1)))

(defmethod run-os-program ((program string)
			   &key
			   (arguments ())
			   (input nil)
			   (output t)
			   (error-output t)
			   &allow-other-keys)
  (let ((process (extensions:run-program program arguments
					 :output output
					 :input input
					 :error error-output)))
    (if process
	(extensions:process-exit-code process)
	-1)))


;;; Loading C and C-like files.

(defmethod load-object-file ((loadable-object-pathname pathname)
			     &key
			     (print *load-print*)
			     (verbose *load-verbose*)
			     (libraries '("c"))
			     )
  (declare (ignore print))
  (when verbose
    (format *trace-output* ";;; MK4: Loading Foreign File ~A."
	    loadable-c-pathname))
  (sb-alien:load-foreign (list loadable-object-pathname)
			 :libraries (mapcar #'(lambda (l)
						(format nil "-l~A"))
					    libraries))
  )


;;; save-working-image --

(defun save-working-image (image-name &rest arguments
				      &key
				      (purify t)
				      (root-structures nil)
				      (environment-name "auxiliary")
				      (init-function #'%top-level)
				      (load-init-file t)
				      (site-init "library:site-init")
				      (print-herald t)
				      (process-command-line t))
  (format *standard-output*
	  "~%;;; MK:DEFSYSTEM: Saving image in file '~A'.~%"
	  image-name)
  (let ((mk-defsystem-herald (second (member :MK-DEFSYSTEM ext:*herald-items*
					     :test #'eq))))
    (when (or (not mk-defsystem-herald)
	      (not (string= (second mk-defsystem-herald) "4.0")))
      (setf ext:*herald-items*
	    (append ext:*herald-items*
		    (list :MK-DEFSYSTEM '("    MK:DEFSYSTEM " "4.0")))))
    (ext:save-lisp image-name)
    ))


;;; end of file -- cmucl.lisp --
