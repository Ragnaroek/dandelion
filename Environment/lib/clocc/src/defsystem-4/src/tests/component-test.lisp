;;; -*- Mode: CLtL -*-

(defparameter f1 (mk4::construct-component :file "actions"))

(defparameter f2 (mk4::construct-component :file "versions"))

(defparameter f3 (mk4::construct-component
		  :file "test-file"
		  :source-pathname "tests/test-file.lisp"))

(defparameter m1
  (mk4::construct-component :module "tests"
			    :components (list f1 f2 f3)))

(defparameter m2
  (mk4::construct-component :module "tests"
			    :components (list f3)))

(defparameter s1
  (mk4::construct-component :system "TEST-1"
			    :components (list m1)))

(defparameter ds1
  (mk4::construct-component :defsystem "DEFSYSTEM-TEST-1"
			    :components (list m1)))

(defparameter s2
  (mk4::construct-component :system "TEST-2"
			    :components (list m2)))


(defparameter f4 (mk4::construct-component
		  :file "versions"
		  :source-pathname (make-pathname :directory
						  '(:relative :up)
						  :name "versions"
						  :type "lisp")))

(defparameter m3 (mk4::construct-component :module "tests"
					   :components (list f4)))

(defparameter s3
  (mk4::construct-component :system "TEST-3"
			    :components (list m3)))

;;; end of file -- component-test.lisp --
