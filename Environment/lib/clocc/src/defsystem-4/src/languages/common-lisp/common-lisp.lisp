;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; common-lisp.lisp -- Language related definitions for CL.

(in-package "MK4")

(define-language :common-lisp
  :compiler #'compile-file
  :loader #'load
  :source-extension (cl.env:source-file-extension)
  :binary-extension (cl.env:compiled-file-extension))


(defmethod invoke-compiler ((cl-compiler (eql #'cl:compile-file))
			    (file pathname)
			    &rest args
			    &key
			    &allow-other-keys)
  (apply #'cl:compile-file file args))


(defmethod invoke-loader ((cl-loader (eql #'cl:load))
			  (file pathname)
			  &rest args
			  &key
			  (verbose *load-verbose*)
			  (print *load-print*)
			  options
			  &allow-other-keys)
  (declare (ignore args options))

  ;; According to the MK3 code the following should be fine.
  (cl:load file :print print :verbose verbose))


;;; end of file -- common-lisp.lisp --
