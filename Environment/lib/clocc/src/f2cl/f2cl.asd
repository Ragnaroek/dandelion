;;; -*- Mode: lisp -*-
;; f2cl asd file

(defpackage #:f2cl-asd
  (:use :cl :asdf))

(in-package #:f2cl-asd)

(defsystem f2cl
  :components
  ((:module src
	    :components
	    ((:file "f2cl0")
	     (:file "f2cl1")
	     (:file "f2cl2")
	     (:file "f2cl3")
	     (:file "f2cl4")
	     (:file "f2cl5")
	     (:file "f2cl6")
	     (:file "f2cl7")
	     #+cmu(:file "f2cl8")
	     (:file "macros")))))

(defmethod asdf:source-file-type
    ((f cl-source-file) (s (eql
			    (asdf:find-system 'f2cl)))) "l")
