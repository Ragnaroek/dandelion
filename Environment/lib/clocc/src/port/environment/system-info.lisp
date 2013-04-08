;;; -*- Mode: Lisp -*-

;;; system-info.lisp --
;;; Cannibalized from Sam Steingold's SYSINFO.
;;; Added some new interface functions which may turn out to be
;;; useful.
;;;
;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defun system-info (&optional (out *standard-output*))
  "Print a detailed report of the current system configuration to a stream."
  (declare (stream out))
  (format out "~&~%~75~~%~75,,,'-:@<<{[ The current environment ]}>~>~@
               Implementation:~20t~a~@
                   ~7tversion:~20t~a~@
               Machine:  type:~20t~a~@
                   ~7tversion:~20t~a~@
                  ~6tinstance:~20t~a~@
                       System:~20t~A~%"
          (common-lisp-implementation-type *common-lisp-implementation*)
	  (common-lisp-implementation-version *common-lisp-implementation*)
          (machine-type *machine*)
	  (machine-version *machine*)
	  (machine-instance *machine*)
	  (operating-system-type *operating-system*))

  (format out "~%Software: type:~20t~a~@
                     ~7tversion:~20t~a~@
                           Site:~20t~a (~a)~@
                      User home:~20t~S~@
              Current directory:~20t~S~@
               Default pathname:~20t~S~@
                       Features:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.~@
                        Modules:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.~@
                Current package:~20t~S~%"
          (software-type *operating-system*)
	  (software-version *operating-system*)
	  (long-site-name)
          (short-site-name)
	  (user-homedir-pathname)
	  (current-working-directory)
          *default-pathname-defaults*
	  *features*
	  *modules*
	  *package*)
  #+clisp (format out "[CLISP] Current language:~30t~a~%"
                  (system::current-language))
  (flet ((exdi (fl) (integer-length (nth-value 1 (decode-float fl)))))
    (format out "Fixnum length:~25t~3d bits~@
Short Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~@
Single Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~@
Double Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~@
Long Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~%"
            (integer-length most-positive-fixnum)
            (exdi most-positive-short-float)
            (float-digits most-positive-short-float)
            (exdi most-positive-single-float)
            (float-digits most-positive-single-float)
            (exdi most-positive-double-float)
            (float-digits most-positive-double-float)
            (exdi most-positive-long-float)
            (float-digits most-positive-long-float)))
  #+clisp (format out "[CLISP] long-float-digits:~44t~3d~%"
                  (ext:long-float-digits))
  (dolist (sy '(array-total-size-limit
		array-rank-limit
		array-dimension-limit
                lambda-parameters-limit
		call-arguments-limit
                multiple-values-limit
		char-code-limit))
    (format out " ~a:~30t~15:d~%" sy (symbol-value sy)))
  (format out "lambda-list-keywords:~30t~{~<~%~30t~1,74:; ~a~>~}~%"
          lambda-list-keywords)
  (format out "Internal time unit:~25t~f sec~%*gensym-counter*:~25t~:d~%"
	  (/ internal-time-units-per-second)
	  *gensym-counter*)
  ;; (format out "Current time:~25t ~A~%" (current-time))
  (format out "~&~75~~%")
  (room)
  (values))

;;; New Interface Functions.

(declaim (inline exdi))

(defun exdi (fl)
  (integer-length (nth-value 1 (decode-float fl))))


;;; end of file -- system-info.lisp --
