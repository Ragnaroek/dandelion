;;; -*- Mode: lisp -*-
;;;
;;; This defsystem converts the FISHPACK Fortran routines
;;; to Lisp and compiling the result.
;;;

(defpackage #:fishpack-asd
  (:use :cl :asdf))

(in-package :fishpack-asd)

(defclass fortran-source-file (source-file) ())
(defmethod source-file-type ((c fortran-source-file) (s module)) "f")

(defmethod output-files ((o operation) (c fortran-source-file))
  (list (make-pathname :name (component-name c)
		       :type #+cmu "x86f" #-cmu "fasl"
		       :defaults (component-pathname c))))

(defmethod perform ((o compile-op) (c fortran-source-file))
  (f2cl:f2cl-compile (make-pathname :name (component-name c)
				    :defaults (component-pathname c)) ))

(defmethod perform ((o load-op) (c fortran-source-file))
  (load (car (output-files o c))))

(defsystem fishpack
  :components
  
  ((:fortran-source-file "pimach")
   (:fortran-source-file "merge")
   (:fortran-source-file "cosgen"
			 :depends-on ("pimach"))
   (:fortran-source-file "genbun"
			 :depends-on ("pimach" "poisd2" "poisp2" "poisn2"))
   (:fortran-source-file "tri3")
   (:fortran-source-file "trix")
   (:fortran-source-file "hwscrt"
			 :depends-on ("trix" "tri3" "pimach"))
   (:fortran-source-file "poisd2"
			 :depends-on ("cosgen" "trix" "merge"))
   (:fortran-source-file "poisn2"
			 :depends-on ("cosgen" "trix" "tri3" "merge"))
   (:fortran-source-file "poisp2"
			 :depends-on ("cosgen" "trix" "tri3" "merge"
					       "poisd2"))))
