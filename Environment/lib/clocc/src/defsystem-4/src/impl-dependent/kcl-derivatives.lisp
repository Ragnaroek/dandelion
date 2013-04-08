;;; -*- Mode: CLtL -*-

(in-package "MK4")

(defun run-program (program &rest arguments)
  (system (format nil "~A~@[~{ ~A~}~]" program arguments)))



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
  (load loadable-object-pathname :print print :verbose nil) ; Should be enough.
  )

;;; end of file -- kcl-derivatives.lisp --
