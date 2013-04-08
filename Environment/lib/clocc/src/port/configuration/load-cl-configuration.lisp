;;; -*- Mode: CLtL -*-

;;; load-cl-configuration.lisp --
;;; A 'configure' for Common Lisp.  The MK:DEFSYSTEM setup.

;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYING for details).

(unless (find-package "MAKE")
  (error "CL.EXT.CONFIGURATION requires the MK:DEFSYSTEM package."))

(unless (find-package "CL.ENVIRONMENT")
  (error "CL.EXT.CONFIGURATION requires the CL.ENVIRONMENT package."))

(defun load-cl-configuration-library (&key
				      (directory
				       (make-pathname
					:directory
					(pathname-directory
					 *default-pathname-defaults*)))
				      (compile-first-p nil)
				      (load-verbose *load-verbose*)
				      (print-herald t)
				      )
  (when print-herald
    (format *standard-output*
	    "~&;;; CL.CONF: Loading CL.EXT.CONFIGURATION package from ~
                            directory~@
               ;;;          \"~A\"~2%"
	    directory))
  (let ((directory (pathname directory))
	(directory-separator
	 (cl.env:os-file-system-directory-separator cl.env:*operating-system*))
	)
    (flet ((load-and-or-compile (file)
	     (if compile-first-p
		 (multiple-value-bind (output-truename warnings-p failure-p)
		     (compile-file file)
		   ;; (declare (ignore warnings-p))
		   (when failure-p
		     (format *standard-output*
			     ";;; File ~S compiled~@
                              ;;; Warnings ~S, Failure ~S.~%"
			     output-truename
			     warnings-p
			     failure-p)
		     (return-from load-cl-configuration-library nil)
		     )
		   (load output-truename :verbose load-verbose))
		 (load file :verbose load-verbose)))
	   )
      
      (setf (logical-pathname-translations "CL-CONF-LIBRARY")
	    `(("**;*.*.*" ,(concatenate 'string
					(namestring (truename directory))
					directory-separator
					"**"
					directory-separator))
	      ("**;*.*" ,(concatenate 'string
				      (namestring (truename directory))
				      directory-separator
				      "**"
				      directory-separator))
	      ("*.*.*" ,(namestring (truename directory)))
	      ("*.*"   ,(namestring (truename directory)))))

      (load-and-or-compile "CL-CONF-LIBRARY:defconf-package.lisp")
      (load-and-or-compile "CL-CONF-LIBRARY:configuration-template.lisp")
      (load-and-or-compile "CL-CONF-LIBRARY:configuration.lisp")
      (load-and-or-compile "CL-CONF-LIBRARY:defconf.lisp")

      ;; System dependent part.
      (load-and-or-compile
       "CL-CONF-LIBRARY:impl-dependent;defsys-availability.lisp")
      #+cmu
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;cmucl.lisp")
      #+allegro
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;allegro.lisp")
      #+lispworks
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;lispworks.lisp")
      #+clisp
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;clisp.lisp")
      #+unix
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;unix.lisp")
      #+(or :mswindows :windows-32)
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;windows.lisp")
      #+genera
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;genera.lisp")
      #+pcl-defsys
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;pcl-defsys.lisp")
      #+mk-defsystem
      (load-and-or-compile "CL-CONF-LIBRARY:impl-dependent;mk-defsystem.lisp")
      ))
  (pushnew :cl-configuration *features*))
    

;;; end of file -- load-cl-configuration.lisp --
