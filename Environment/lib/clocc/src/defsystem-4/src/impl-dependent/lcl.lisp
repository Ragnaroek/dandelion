;;; -*- Mode: CLtL -*-

(in-package "MK4")

(shadow '(lcl:run-program))

(defun run-program (program &rest arguments)
  (lcl:run-program program :arguments arguments))


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
  (load-foreign-files loadable-object-pathname)
  )

;;; end of file -- lcl.lisp --
