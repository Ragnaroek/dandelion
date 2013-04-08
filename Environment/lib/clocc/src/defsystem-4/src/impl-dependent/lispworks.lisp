;;; -*- Mode: CLtL -*-

(in-package "MK4")

(defmethod run-os-program ((program string)
			   &key
			   (arguments ())
			   (input nil)
			   (output nil)
			   (error-output nil)
			   &allow-other-keys)
  (declare (ignore input output error-output))
  (system:call-system-showing-output (format nil "~A~@[~{ ~A~}~]"
					     program arguments)))

(defun run-program (program &rest arguments)
  (system:call-system-showing-output (format nil "~A~@[~{ ~A~}~]"
					      program arguments)))


(defmethod load-object-file ((loadable-object-pathname pathname)
			     &key
			     (print *load-print*)
			     (verbose *load-verbose*)
			     (libraries '("c"))
			     )
  (declare (ignore print libraries))
  (when verbose
    (format *trace-output* ";;; MK4: Loading Foreign File ~A."
	    loadable-c-pathname))
  #+(and :unix (not :linux))
  (link-load:read-foreign-module loadable-object-pathname)

  #+(and :unix (not :linux))
  (fli:register-module loadable-object-pathname)
  )

;;; end of file -- lispworks.lisp --
