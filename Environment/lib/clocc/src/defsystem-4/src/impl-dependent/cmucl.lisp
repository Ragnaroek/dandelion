;;; -*- Mode: CLtL -*-

;;; cmucl.lisp --
;;; Implementation dependent DEFSYSTEM support for CMUCL

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
			   (error-output *error-output*)
			   &allow-other-keys)
  (let ((process (extensions:run-program program arguments
					 :output output
					 :input input
					 :error error-output)))
    (if process
	(prog1 (extensions:process-exit-code process)
	  (extensions:process-close process))
	-1)))


;;; Loading C and C-like files.

(defmethod load-object-file ((loadable-object-pathname pathname)
			     &key
			     (print *load-print*)
			     (verbose *load-verbose*)
			     (libraries '("c"))
			     &allow-other-keys
			     )
  (declare (ignore print))
  (when verbose
    (user-message *trace-output* "Loading Foreign File ~A."
		  loadable-object-pathname))
  (alien:load-foreign (list loadable-object-pathname)
		      :libraries (mapcar #'(lambda (l)
					     (format nil "-l~A" l))
					 libraries))
  )


;;; save-working-image --

(defun save-working-image (image-name &rest arguments
				      &key
				      (purify t)
				      (root-structures nil)
				      (environment-name "auxiliary")
				      (init-function #'cl::%top-level)
				      (load-init-file t)
				      (site-init "library:site-init")
				      (print-herald t)
				      (process-command-line t))
  (declare (ignore arguments))
  (user-message *standard-output* "Saving image in file '~A'.~%" image-name)
  (let ((mk-defsystem-herald (second (member :MK-DEFSYSTEM ext:*herald-items*
					     :test #'eq))))
    (when (or (not mk-defsystem-herald)
	      (not (string= (second mk-defsystem-herald) "4.0")))
      (setf ext:*herald-items*
	    (append ext:*herald-items*
		    (list :MK-DEFSYSTEM '("    MK:DEFSYSTEM " "4.0")))))
    (ext:save-lisp image-name
		   :purify purify
		   :root-structures root-structures
		   :environment-name environment-name
		   :init-function init-function
		   :load-init-file load-init-file
		   :site-init site-init
		   :print-herald print-herald
		   :process-command-line process-command-line
		   )
    ))


;;; end of file -- cmucl.lisp --
