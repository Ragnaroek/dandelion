;; -*- Mode: lisp; Package: F2CL -*-

(in-package :f2cl)

(defun make-cmucl-ffi (name info)
  (let ((arg-info (mapcar #'(lambda (arg ret)
			      (if ret
				  `(,arg :in-out)
				  `(,arg)))
			  (f2cl-finfo-arg-types info)
			  (f2cl-finfo-return-values info))))
    (format t "~A -> ~A~%" name arg-info)))

(defun make-ffi (&key (style :cmucl))
  (let ((maker-fun (ecase style
		     (:cmucl
		      #'make-cmucl-ffi))))
    (maphash maker-fun *f2cl-function-info*)))

