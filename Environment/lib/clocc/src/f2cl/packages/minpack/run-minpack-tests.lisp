(in-package :minpack)

(in-package :minpack)

(defun run-minpack-tests ()
  (dolist (f '(tlmdif tlmder))
    (with-open-file (input "minpack:lmdif-input.dat"
			   :direction :input)
      (let ((old-lun (gethash 5 f2cl-lib::*lun-hash*)))
      (unwind-protect
	   (progn
	     (setf (gethash 5 f2cl-lib::*lun-hash*) input)
	     (funcall f))
	(setf (gethash 5 f2cl-lib::*lun-hash*) old-lun))))))
  

  
