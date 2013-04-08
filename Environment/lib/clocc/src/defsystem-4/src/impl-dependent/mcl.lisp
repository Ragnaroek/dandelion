;;; -*- Mode: CLtL -*-

(in-package "MK4")

(defun run-program (program &rest arguments)
  (declare (ignore program arguments))
  (error "MK:DEFSYSTEM: no RUN-PROGRAM defined for MCL."))

(defun edit-operation (component force)
  "Always returns nil, i.e. component not changed."
  (declare (ignore force))
  ;;
  (let* ((full-pathname (component-full-pathname component :source))
         (already-editing\?
	  #+:mcl
	   (dolist (w (CCL:windows :class 'ccl:fred-window))
	     (when (equal (CCL:window-filename w) full-pathname)
	       (return w)))

	   #-:mcl
	   nil))
    
    (if already-editing\?
	#+:mcl (CCL:window-select already-editing\?) #-:mcl nil
	(ed full-pathname)))
  nil)

(eval-when (:load-toplevel :execute)
  (component-operation :edit 'edit-operation)
  (component-operation 'edit 'edit-operation))

;;; end of file -- mcl.lisp --
