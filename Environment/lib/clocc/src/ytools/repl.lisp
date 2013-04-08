;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: repl.lisp,v 2.1 2005/12/26 00:25:17 airfoyle Exp $

(defun simp-repl ()
   (let (r*
	 (* nil)
	 (** nil))
      (declare (special * **))
      (format *query-io* "Simple read-eval-print loop; *package* = ~s~%"
			 *package*)
      (loop
	 (format *query-io* "?*> ")
	 (setq r* (read))
	 (cond ((eq r* ':quit)
		(return 'repl-done)))
	 (restart-case
	        (let ((newval (eval r*)))
		  (setq ** *)
		  (setq * newval)
		  (format *query-io* "~s~%" newval))
	    (resume-simp-repl ()
		:report "Resume simple read-eval-print loop")))))
