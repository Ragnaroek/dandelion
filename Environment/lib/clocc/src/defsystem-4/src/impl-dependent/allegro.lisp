;;; -*- Mode: CLtL -*-

;;; allegro.lisp --

(in-package "MK4")

(defmethod run-os-program ((program string)
			   &key
			   (arguments ())
			   (input nil)
			   (output nil)
			   (error-output nil)
			   &allow-other-keys)
  (excl:run-shell-command (format nil
				  "~A~@[~{ ~A~}~]"
				  program arguments)
			  :wait t
			  :output output
			  :input input
			  :error error-output))


(defun run-program (program &rest arguments)
  (excl:run-shell-command (format nil
				  "~A~@[~{ ~A~}~]"
				  program arguments)))


(defmethod load-object-file ((loadable-object-pathname pathname)
			     &key
			     (print *load-print*)
			     (verbose *load-verbose*)
			     (libraries '("c"))
			     )
  (declare (ignore libraries))
  (when verbose
    (format *trace-output* ";;; MK4: Loading Foreign File ~A."
	    loadable-c-pathname))
  (load loadable-object-pathname :print print :verbose verbose))



;;; ********************************
;;; Allegro Toplevel Commands ******
;;; ********************************
;;; Creates toplevel command aliases for Allegro CL.

(top-level:alias ("compile-system" 8) 
  (system &key force (minimal-load mk:*minimal-load*)
	  test verbose version)
  "Compile the specified system"

  (mk:compile-system system :force force 
		     :minimal-load minimal-load
		     :test test :verbose verbose
		     :version version))

(top-level:alias ("load-system" 5) 
  (system &key force (minimal-load mk:*minimal-load*)
	  (compile-during-load mk:*compile-during-load*)
	  test verbose version)
  "Compile the specified system"

  (mk:load-system system :force force 
		  :minimal-load minimal-load
		  :compile-during-load compile-during-load
		  :test test :verbose verbose
		  :version version))

(top-level:alias ("show-system" 5) (system)
  "Show information about the specified system."

  (mk:describe-system system))


(top-level:alias ("describe-system" 9) (system)
  "Show information about the specified system."

  (mk:describe-system system))


(top-level:alias ("system-source-size" 9) (system)
  "Show size information about source files in the specified system."

  (mk:system-source-size system))


(top-level:alias ("clean-system" 6)
  (system &key force test verbose version)
  "Delete binaries in the specified system."

  (mk:clean-system system :force force 
		   :test test :verbose verbose
		   :version version))


(top-level:alias ("edit-system" 7) 
  (system &key force test verbose version)
  "Load system source files into Emacs."

  (mk:edit-system system :force force 
		  :test test :verbose verbose
		  :version version))


(top-level:alias ("hardcopy-system" 9) 
  (system &key force test verbose version)
  "Hardcopy files in the specified system."

  (mk:hardcopy-system system :force force 
		      :test test :verbose verbose
		      :version version))


(top-level:alias ("make-system-tag-table" 13) (system)
  "Make an Emacs TAGS file for source files in specified system."

  (mk:make-system-tag-table system))


;;; ********************************
;;; Allegro Make System Fasl *******
;;; ********************************

(defun allegro-make-system-fasl (system destination 
					&optional (include-dependents t))
  (excl:shell
   (format nil "rm -f ~A; cat~{ ~A~} > ~A" 
	   destination
	   (if include-dependents
	       (files-in-system-and-dependents system :all :binary)
	       (files-in-system system :all :binary))
	   destination)))


(defun edit-operation (component force)
  "Edit a component - always returns nil, i.e. component not changed."
  (declare (ignore force))
  (let ((full-pathname (component-full-pathname component :source)))
    (ed full-pathname))
  nil)

;;; *** Hardcopy System ***
(defparameter *print-command* "enscript -2Gr" ; "lpr"
  "Command to use for printing files on UNIX systems.")

(defun hardcopy-operation (component force)
  "Hardcopy a component - always returns nil, i.e. component not changed."
  (declare (ignore force))
  (let ((full-pathname (component-full-pathname component :source)))
    (excl:run-shell-command (format nil "~A ~A"
				    *print-command* full-pathname)))
  nil)


;;; *** System Tag Table ***

(defun make-system-tag-table (system-name)
  "Makes an Emacs tag table using the GNU etags program."
  (let ((files-in-system (files-in-system system-name :all :source)))

    (format t "~&Making tag table...")
    (excl:run-shell-command (format nil "etags ~{~a ~}" files-in-system))
    (format t "done.~%")))


#+not-yet-in-mk4
(eval-when (:load-toplevel :execute)
  (component-operation :edit 'edit-operation)
  (component-operation 'edit 'edit-operation)
  (component-operation :hardcopy 'hardcopy-operation)
  (component-operation 'hardcopy 'hardcopy-operation)
  )

  

;;; end of file -- allegro.lisp --