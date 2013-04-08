;;; -*- Mode: Lisp -*-

;;; operating-system.lisp --
;;;
;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defclass operating-system (software feature-tagged-type-class)
  ((type :reader operating-system-type :reader os-type)
   (version :reader operating-system-version :reader os-version)
   (tag :reader operating-system-feature-tag :reader os-feature-tag
	:type symbol)
   )
  (:documentation "The CL.ENVIRONMENT Operating System Class."))
	   

;;;===========================================================================
;;; Known Operating Systems.

;;; NOTE: the tag must match the class name. See FIND-OS-CLASS below
;;; for an explanation.

(defclass unix (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT Unix Operating System Class.")
  (:default-initargs :type "UNIX" :version "" :feature-tag :unix))


(defclass sun-os (unix)
  ()
  (:documentation "The CL.ENVIRONMENT SunOS Operating System Class.")
  (:default-initargs :type "SunOS" :version "4.1.1" :feature-tag :sun-os))


(defclass solaris (unix)
  ()
  (:documentation "The CL.ENVIRONMENT Solaris Operating System Class.")
  (:default-initargs :type "Solaris" :version "7.x" :feature-tag :solaris))

(defclass hp-ux (unix)
  ()
  (:documentation "The CL.ENVIRONMENT HP-UX Operating System Class.")
  (:default-initargs :type "HP-UX" :version "10.x" :feature-tag :hp-ux))

(defclass irix (unix)
  ()
  (:documentation "The CL.ENVIRONMENT Irix Operating System Class.")
  (:default-initargs :type "Irix" :version "" :feature-tag :irix))

(defclass linux (unix)
  ()
  (:documentation "The CL.ENVIRONMENT Linux Operating System Class.")
  (:default-initargs :type "Linux" :version "" :feature-tag :linux))


(defclass mac-os (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT MacOS Operating System Class.")
  (:default-initargs :type "MacOS" :version "9.x" :feature-tag :mac-os))

(defclass mac-os-x (mac-os unix)
  ()
  (:documentation "The CL.ENVIRONMENT MacOS X Operating System Class.")
  (:default-initargs :type "MacOS-X" :version "10.x" :feature-tag :mac-os-x))


(defclass genera (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT Genera Operating System Class.")
  (:default-initargs :type "Genera" :version "8.x" :feature-tag :genera))

(defclass amiga (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT Amiga Operating System Class.")
  (:default-initargs :type "Amiga" :version "" :feature-tag :amiga))

(defclass os/2 (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT OS/2 Operating System Class.")
  (:default-initargs :type "OS/2" :version "" :feature-tag :os/2))


(defclass ms-dos (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT MS-DOS Operating System Class.")
  (:default-initargs :type "MS-DOS" :version "" :feature-tag :ms-dos))

(defclass ms-windows (operating-system)	; Maybe (MS-DOS) would be better!
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows Operating System Class.")
  (:default-initargs :type "Windows (generic)"
                     :version ""
		     :feature-tag :ms-windows))


(defclass ms-windows-32 (ms-windows)	; Maybe (MS-DOS) would be better!
  ()
  (:documentation
   "The CL.ENVIRONMENT Generic MS Windows (32 bits) Operating System Class.")
  (:default-initargs :type "Windows (generic, 32 bits)"
                     :version ""
		     :feature-tag :ms-windows-32))


(defclass ms-windows-95 (ms-windows-32)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows 95 Operating System Class.")
  (:default-initargs :type "Windows 95"
                     :version ""
		     :feature-tag :ms-windows-95))

(defclass ms-windows-98 (ms-windows-95)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows 95 Operating System Class.")
  (:default-initargs :type "Windows 98"
                     :version ""
		     :feature-tag :ms-windows-98))

(defclass ms-windows-nt (ms-windows-32)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows NT Operating System Class.")
  (:default-initargs :type "Windows NT"
                     :version "4.1"
		     :feature-tag :ms-windows-nt))

(defclass ms-windows-nt-tse (ms-windows-nt)
  ()
  (:documentation
   "The CL.ENVIRONMENT MS Windows NT Terminal Server Operating System Class.")
  (:default-initargs :type "Windows NT TSE"
                     :version "4.1"
		     :feature-tag :ms-windows-nt-tse))

(defclass ms-windows-2000 (ms-windows-nt ms-windows-98)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows 2000 Operating System Class.")
  (:default-initargs :type "Windows 2000"
                     :version ""
		     :feature-tag :ms-windows-2000))

(defclass ms-windows-me (ms-windows-nt ms-windows-98)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows ME Operating System Class.")
  (:default-initargs :type "Windows ME"
                     :version ""
		     :feature-tag :ms-windows-me))

(defclass ms-windows-xp (ms-windows-2000)
  ()
  (:documentation "The CL.ENVIRONMENT MS Windows XP Operating System Class.")
  (:default-initargs :type "Windows XP"
                     :version ""
		     :feature-tag :ms-windows-xp))


(defclass vms (operating-system)
  ()
  (:documentation "The CL.ENVIRONMENT VMS Operating System Class.")
  (:default-initargs :type "VMS"
                     :version ""
		     :feature-tag :vms))


(defclass openvms (vms)
  ()
  (:documentation "The CL.ENVIRONMENT OPENVMS Operating System Class.")
  (:default-initargs :type "OPENVMS"
                     :version ""
		     :feature-tag :openvms))


;;; Special operating system related functionalities.

(declaim (inline find-os-class find-operating-system-class))

(defun find-os-class (tag)
  (declare (type symbol tag))
  (find-class (intern (symbol-name tag) "CL.ENVIRONMENT") nil))

(defun find-operating-system-class (tag) (find-os-class tag))


;;; os-tag-compatible-p

(defgeneric os-tag-compatible-p (os tag))

(declaim (inline operating-system-tag-compatible-p))

(defun operating-system-tag-compatible-p (os tag)
  (declare (type operating-system os)
	   (type symbol tag))
  (os-tag-compatible-p os tag))

(defmethod os-tag-compatible-p ((os cl.env:operating-system) (tag symbol))
  (eq tag (cl.env:os-feature-tag os)))

;;; UNIX
(defmethod os-tag-compatible-p ((os cl.env:unix) (tag (eql :unix))) t)

(defmethod os-tag-compatible-p ((os cl.env:unix) (tag (eql :solaris))) t)

(defmethod os-tag-compatible-p ((os cl.env:unix) (tag (eql :sun-os))) t)

(defmethod os-tag-compatible-p ((os cl.env:unix) (tag (eql :linux))) t)

(defmethod os-tag-compatible-p ((os cl.env:unix) (tag (eql :irix))) t)


;;; MS-WINDOWS
(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-32)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-nt)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-nt-tse)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-95)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-98)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-2000)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-me)))
  t)

(defmethod os-tag-compatible-p ((os cl.env:ms-windows)
				(tag (eql :ms-windows-xp)))
  t)


;;; One piece of information always useful and needed.

(defgeneric os-file-system-directory-separator (os)
  (:documentation
   "Returns a string that contains the 'directory separator' for the OS.
The string is usually one character long."))

(defmethod os-file-system-directory-separator ((os unix)) "/")

(defmethod os-file-system-directory-separator ((os mac-os)) ":")

(defmethod os-file-system-directory-separator ((os mac-os-x))
  (warn "The directory separator returned is \"/\".~@
         This may not be right for some CL implementations on Mac OS X."
	os)
  "/")

(defmethod os-file-system-directory-separator ((os genera)) ">")

(defmethod os-file-system-directory-separator ((os amiga)) "\\") ; Check this!!

(defmethod os-file-system-directory-separator ((os os/2)) "\\")

(defmethod os-file-system-directory-separator ((os ms-dos)) "\\")

(defmethod os-file-system-directory-separator ((os ms-windows)) "\\")

(defmethod os-file-system-directory-separator ((os vms)) ".")


;;; end of file -- operating-system.lisp
