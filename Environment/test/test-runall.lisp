(in-package :cl-user)

(defpackage #:dandelion-test-runall
  (:use #:common-lisp)
  (:export #:run-all))

(in-package #:dandelion-test-runall)

(defun run-all ()
  (lisp-unit:remove-tests :all)
  (defmacro-test-extensions:report-test-result 
     :dandelion-test-meta 
     :dandelion-test-protocol 
     :dandelion-test-dandelion-utils 
     :dandelion-test-server 
     :dandelion-test-main))