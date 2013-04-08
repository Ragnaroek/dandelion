;;; ADJOIN-DIRECTORIES

(defpackage "CL.EXT.FILESYSTEM" (:use "COMMON-LISP")
  (:nicknames "cl.ext.filesystem")
  (:export #:adjoin-directories))

(in-package "CL.EXT.FILESYSTEM")

;;; Debugging aids.

(defparameter *debug-adjoin-directories* nil)

(defun debug-message (format-control &rest format-args)
  (when *debug-adjoin-directories*
    (apply #'format *trace-output* format-control format-args)))


;;; Interface and implementation.

(defgeneric adjoin-directories (dp1 p2 &optional host)
  (:documentation
   "Adjoins the directory components of two pathname designators.
The two required arguments may be strings (logical pathname strings
too), pathnames, or logical pathnames.  They are first parsed and/or
translated to pathnames and then merged accordingly.
The result is always a PATHNAME object, which means that it can also be a
LOGICAL-PATHNAME object.
It is an error if DP1 resolves to a pathname with a valid NAME
component (i.e. a NAME component different from NIL or :UNSPECIFIC)."))


;;; Case 1.
;;; DP1 and P2 are PATHNAMEs.  This is the base case.

(defmethod adjoin-directories ((dp1 pathname) (p2 pathname) &optional host)
  (declare (ignore host))
  (debug-message "pathname pathname (~S ~S)~%" dp1 p2)
  (when (and (pathname-name dp1) (not (eq (pathname-name dp1) :unspecific)))
    (error "Directory pathname has valid NAME component ~S." dp1))
  (merge-pathnames p2 dp1))


;;; Case 2.
;;; dp1 and p2 are strings.
;;; First PARSE-NAMESTRING them.

#+old-version
(defmethod adjoin-directories ((dp1 string) (p2 string) &optional host)
  (debug-message "string string (~S ~S)~%" dp1 p2)
  (adjoin-directories (parse-namestring dp1 host) (parse-namestring p2 host)))

(defmethod adjoin-directories ((dp1 string) (p2 string) &optional host)
  (debug-message "string string (~S ~S)~%" dp1 p2)
  (adjoin-directories (parse-namestring dp1 host) p2))


;;; Case 3 and 4.
;;; One of DP1 or P2 is a PATHNAME and the other is a STRING.

(defmethod adjoin-directories ((dp1 pathname) (p2 string) &optional host)
  (debug-message "pathname string (~S ~S)~%" dp1 p2)
  (adjoin-directories dp1 (parse-namestring p2 host)))


(defmethod adjoin-directories ((dp1 string) (p2 pathname) &optional host)
  (debug-message "string pathname (~S ~S)~%" dp1 p2)
  (adjoin-directories (parse-namestring dp1 host) p2))


;;; Case 5.
;;; DP1 is a LOGICAL-PATHNAME and P2 is a LOGICAL-PATHNAME as
;;; well.  This is fun. MERGE-PATHNAMES returns a LOGICAL-PATHNAME
;;; which must be correctly translated.

#+adjdirs-lp-translation
(defmethod adjoin-directories ((dp1 logical-pathname) (p2 logical-pathname)
			       &optional host)
  (declare (ignore host))
  (debug-message "logical-pathname logical-pathname (~S ~S)~%" dp1 p2)
  (translate-logical-pathname (merge-pathnames p2 dp1 nil)))


(defmethod adjoin-directories ((dp1 logical-pathname) (p2 logical-pathname)
			       &optional host)
  (declare (ignore host))
  (debug-message "logical-pathname logical-pathname (~S ~S)~%" dp1 p2)
  (merge-pathnames p2 dp1 nil))


;;; Case 6.
;;; DP1 is a LOGICAL-PATHNAME and P2 is a PATHNAME.
;;; We need to translate the LOGICAL-PATHNAME before MERGEing the result.

(defmethod adjoin-directories ((dp1 logical-pathname) (p2 pathname)
			       &optional host)
  (declare (ignore host))
  (debug-message "logical-pathname pathname (~S ~S)~%" dp1 p2)
  (merge-pathnames p2 (translate-logical-pathname dp1)))


;;; Case 6a:
;;; DP1 is a LOGICAL-PATHNAME and P2 is a STRING.  Note that this
;;; would required a change in the (string string) method. That method
;;; now does PARSE-NAMESTRING DP1 only and then recurs.

(defmethod adjoin-directories ((dp1 logical-pathname) (p2 string)
			       &optional host)
  (declare (ignore host))
  (debug-message "logical-pathname string (~S ~S)~%" dp1 p2)
  (merge-pathnames p2 dp1 nil))


;;; Case 7.
;;; DP1 is a PATHNAME and P2 is a LOGICAL-PATHNAME.
;;; We need to translate the LOGICAL-PATHNAME resulting from MERGEing
;;; the result.

(defmethod adjoin-directories ((dp1 pathname) (p2 logical-pathname)
			       &optional host)
  (declare (ignore host))
  (debug-message "pathname logical-pathname (~S ~S)~%" dp1 p2)
  (translate-logical-pathname (merge-pathnames p2 dp1 nil)))


;;; Case 8 and 9.
;;; This would be the specialization of Cases 3 and 4. However a
;;; LOGICAL-PATHNAME is a PATHNAME, so Cases 3 and 4 do apply before,
;;; and the recursion works its magic.


;;; Testing.

(setf (logical-pathname-translations "AD-TEST-SIMPLE")
      `(("*.*.*" "/usr/local/tmp/*.*.*")
	("*.*" "/usr/local/tmp/*.*")))


(setf (logical-pathname-translations "AD-TEST-SUBDIR")
      `(("**;*.*.*" "/usr/local/share/**/*.*.*")
	("**;*.*" "/usr/local/share/**/*.*")

	(";**;*.*" "/usr/local/share/r/**/*.*")
	(";**;*.*" "/usr/local/share/r/**/*.*")))


;;; end of file -- adjoin-dirs.lisp --
