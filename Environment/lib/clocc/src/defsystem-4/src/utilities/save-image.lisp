;;; -*- Mode: CLtL -*-

;;; save-image --

(in-package "MK4")			; Maybe this functionality
					; should be in its own
					; package.

;;; ********************************
;;; SAVE-IMAGE *********************
;;; ********************************

;;; Portable wrapper for saving a Common Lisp image.
;;; Note that this is only the main definition.  The actual
;;; implementation dependent definitions are in the 'impl-dependent'
;;; files.

(defun save-working-image (image-name &rest arguments)
  (declare (ignore image-name arguments))
  (warn "MK:DEFSYSTEM: sorry: no SAVE-IMAGE function defined for ~A ~A.~@
         Make sure that the implementation dependent files are loaded after~@
         the common one."
	(lisp-implementation-type)
	(lisp-implementation-version)))

;;; end of file -- save-image --