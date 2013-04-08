;;; -*- Mode: Lisp -*-

;;; init-environment.lisp --
;;; This file is really hairy.  I decide to leave it so.  The
;;; alternative would be to split it up in several implementation
;;; dependent files, but then things may really get hairier :)
;;; This is the only file with #+/#- (well... almost).
;;; Anyway, I am open for suggestions.
;;;
;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defun featurep (feature)
  (and (symbolp feature)
       (not (null (member (symbol-name feature) *features*
			  :key #'symbol-name
			  :test #'string=)))))

;;; These are parameters - as opposed to constants - to allow (in
;;; principle) some fancy cross development tricks.

(defparameter *common-lisp-implementation* ())

(defparameter *operating-system* ())

(defparameter *machine* ())

(eval-when (:load-toplevel :execute)

  ;;--------------------
  ;; Setup Machine Info.

  (cond ((or (featurep :x86))		; Works for CMUCL and ACL
	 (setf *machine* (make-instance 'intel-x86-machine)))
	((or (featurep :pentium4))	; Works for ECL
	 (setf *machine* (make-instance 'intel-x86-machine)))
	((or (featurep :sparc))
	 (setf *machine* (make-instance 'sparc-machine)))
	((or (featurep :sparcv9) (featurep :sparc-v9) (featurep :sparc9))
	 (setf *machine* (make-instance 'sparc-v9-machine)))
	((or (featurep :ppc))
	 (setf *machine* (make-instance 'sparc-machine)))
	((or (featurep :alpha))
	 (setf *machine* (make-instance 'sparc-machine)))
	((or (featurep :mips))
	 (setf *machine* (make-instance 'sparc-machine)))
	((or (and (featurep :apple) (featurep :macosx) (featurep :lispworks)))
	 (setf *machine* (make-instance 'ppc-machine)))
	(t (setf *machine* (make-instance 'machine))))

  ;;-----------------------------
  ;; Setup Operating System Info.
  #+cmu
  (cond ((featurep :solaris)
	 (setf *operating-system*
	       (make-instance 'Solaris
			      :version (common-lisp:software-version))))

	((and (featurep :sunos) (not (featurep :solaris)))
	 (setf *operating-system*
	       (make-instance 'SunOS
			      :version (common-lisp:software-version))))

	((featurep :linux)
	 (setf *operating-system* (make-instance 'linux)))

	(t
	 (setf *operating-system* (make-instance 'unix))))

  #+allegro
  (cond ((featurep :linux)
	 (setf *operating-system* (make-instance 'linux)))

	((featurep :unix)
	 (setf *operating-system* (make-instance 'unix)))

	((featurep :mswindows)
	 (if (featurep :windows-32)
	     (setf *operating-system* (make-instance 'ms-windows-32))
	     (setf *operating-system* (make-instance 'ms-windows))))

	(t)				; do nothing?
	)

  #+lispworks
  (cond ((featurep :linux)
	 (setf *operating-system* (make-instance 'linux)))

	((featurep :macosx)
	 (setf *operating-system* (make-instance 'mac-os-x)))

	((featurep :unix)
	 (setf *operating-system* (make-instance 'unix)))

	((featurep :win32)
	 (setf *operating-system* (make-instance 'ms-windows)))

	(t)				; do nothing?
	)

  #+clisp
  (cond ((featurep :unix)
	 (setf *operating-system* (make-instance 'unix)))

	((featurep :os/2)
	 (setf *operating-system* (make-instance 'os/2)))

	((featurep :ms-dos)
	 (setf *operating-system* (make-instance 'ms-dos)))

	((featurep :win32)
	 (setf *operating-system* (make-instance 'ms-windows)))

	((featurep :amiga)
	 (setf *operating-system* (make-instance 'amiga)))

	(t)				; do nothing?
	)

  #+ecl
  (cond ((and (featurep :unix) (featurep :linux))
	 (setf *operating-system* (make-instance 'linux)))

	((featurep :unix)
	 (setf *operating-system* (make-instance 'unix)))

	((featurep :win32)
	 (setf *operating-system* (make-instance 'ms-windows)))

	(t)				; do nothing?
	)

  (define-symbol-macro *os* *operating-system*)
	
  ;;---------------------------------------
  ;; Setup Common Lisp Implementation Info.

  #+cmu
  (setf *common-lisp-implementation*
	(make-instance 'cmucl :feature-tag :cmu))

  #+sbcl
  (setf *common-lisp-implementation*
	(make-instance 'sbcl :feature-tag :sbcl))

  #+clisp
  (setf *common-lisp-implementation*
	(make-instance 'clisp :feature-tag :clisp))

  #+allegro
  (setf *common-lisp-implementation*
    (make-instance 'allegro :feature-tag :allegro
		   :case-sensitive 
		   (case excl:*current-case-mode*
		     ((:case-sensitive-lower :case-sensitive-upper)
		      t)
		     (t 
		      nil))
		   :characters-16bits (when (find :ics cl:*features*) t)))

  #+lispworks
  (setf *common-lisp-implementation*
	(make-instance 'lispworks :feature-tag :lispworks))

  #+kcl
  (setf *common-lisp-implementation*
	(make-instance 'kcl :feature-tag :kcl))

  #+ibcl
  (setf *common-lisp-implementation*
	(make-instance 'ibcl :feauture-tag :ibcl))

  #+akcl
  (setf *common-lisp-implementation*
	(make-instance 'akcl :feature-tag :akcl))

  #+ecolisp
  (setf *common-lisp-implementation*
	(make-instance 'ecolisp :feature-tag :ecolisp))

  #+ecl
  (setf *common-lisp-implementation*
	(make-instance 'ecl :feature-tag :ecl))

  #+gcl
  (setf *common-lisp-implementation*
	(make-instance 'gcl :feature-tag :gcl))

  #+lucid
  (setf *common-lisp-implementation*
	(make-instance 'lucid :feature-tag :lucid))

  #+mcl
  (setf *common-lisp-implementation*
	(make-instance 'mcl :feature-tag :mcl))

  #+genera
  (setf *common-lisp-implementation*
	(make-instance 'scl :feature-tag :genera))

  (define-symbol-macro *cl* *common-lisp-implementation*)

  )

;;; end of file -- init-environment.lisp --
