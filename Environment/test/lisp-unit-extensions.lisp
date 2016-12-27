;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Erweiterung zu Lisp-Unit
;++
;+++++++++++++++++++++++++++++++++++++++++++++

(in-package :cl-user)

(defpackage #:defmacro-test-extensions
  (:use #:common-lisp 
        #:lisp-unit)
  (:export #:report-test-result
           #:assert-equal-str-set))

(in-package #:defmacro-test-extensions)

(defun passed (result)
  (zerop (+ (lisp-unit::fail result) (lisp-unit::exerr result))))

(defun do-report (package result &aux pass)
  (setf pass (passed result))
  (format T "~a    ~:[FAILED~;PASSED~]~%" package pass)
  (unless pass 
    (print-failures result)
    (print-errors result))
  pass)

(defun condensed-report (package)
  (do-report package
    (with-open-file (*standard-output* "/dev/null" :direction :output :if-exists :supersede)
      (lisp-unit:run-tests :all package))))

(defun report-test-result (&rest packages)
  (if (every #'identity (mapcar #'condensed-report packages))
    (format T "~%SUCCESS :)")
    (format T "~%FAILED :(")))

(defun assert-equal-str-set (l1 l2) 
  (assert-equalp (sort (copy-list l1) #'string<=) (sort (copy-list l2) #'string<=)))
