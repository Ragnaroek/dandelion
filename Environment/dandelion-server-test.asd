(defpackage :dandelion-server-test.system
   (:use #:cl #:asdf))
   
(in-package :dandelion-server-test.system)

(defsystem :dandelion-server-test
     :version "1.1.0"
     :depends-on (:dandelion-server :lisp-unit)
     :components
     ((module "test" :serial T
     	             :components ((:file "lisp-unit-extensions")
     	                          (:file "utils")
     	              			  (:file "test-meta")
     	                          (:file "test-protocol")
     	                          (:file "test-utils")
     	                          (:file "test-server")
     	                          (:file "test-main")
     	                          (:file "test-runall")))))