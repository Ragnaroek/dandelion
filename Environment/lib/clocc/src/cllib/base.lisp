;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997-2004 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: base.lisp,v 2.19 2005/01/27 23:03:05 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base.lisp,v $

(eval-when (compile load eval)
  (require :port-ext (translate-logical-pathname "clocc:src;port;ext"))
  (require :port-sys (translate-logical-pathname "port:sys")))

(defpackage #:cllib
  (:use #:common-lisp #:port)
  (:nicknames #:org.cons.clocc/sds/cllib)
  #+cmu (:shadowing-import-from #:port port:defstruct)
  (:export #:value #:code #:*datadir* #:*mail-host-address*
           #:*user-mail-address*))

(in-package :cllib)

;;;
;;; }}}{{{paths
;;;

(setf (logical-pathname-translations "cllib")
      `(("**;*" ,(logical-pathname "clocc:src;cllib;**;*"))
        ("**;*.*" ,(logical-pathname "clocc:src;cllib;**;*.*"))))

(defun mk-path (default &rest make-pathname-args)
  "This is a helper function for portable creation of pathnames.
If you need to create a pathname under a specific directory, you need
to pass it first to `make-pathname' and then to `merge-pathnames' since
otherwise `*default-pathname-defaults*' will get in the way.
Beware: `default' should not be a relative pathname!"
  (merge-pathnames (apply #'make-pathname :defaults default
                          make-pathname-args)
                   default))

(defcustom *datadir* pathname
  (mk-path (or (user-homedir-pathname) "") :directory '(:relative "data"))
  "The directory where the data file are created by default.")
(defcustom *mail-host-address* simple-string
  (let ((st (machine-instance)))
    (if st (subseq st 0 (position #\Space st)) "localhost"))
  "*Name of this machine, for purposes of naming users.")
(defcustom *user-mail-address* simple-string
  (concatenate 'string (or (getenv "USER") (getenv "USERNAME") "nobody")
               "@" *mail-host-address*)
  "*Full mailing address of this user.
This is initialized based on `mail-host-address'.")

;;;
;;; }}}{{{ generic
;;;

(declaim (ftype (function (t) number) value))
(defgeneric value (xx)
  (:documentation "Get the value of the object.")
  (:method ((xx number)) xx)
  (:method ((xx cons)) (value (cdr xx))))

(declaim (ftype (function (t) symbol) code))
(defgeneric code (xx)
  (:documentation "Get the code of the object.")
  (:method ((xx symbol)) xx))

(provide :cllib-base)
;;; }}} base.lisp ends here
