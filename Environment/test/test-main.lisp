;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Testmodul de.fh-trier.test.evalserver.main
;++ Testet alle Funktionen aus dem main-Modul
;++
;+++++++++++++++++++++++++++++++++++++++++++++

(in-package #:cl-user)

(defpackage #:dandelion-test-main
  (:use #:common-lisp 
        #:lisp-unit
        #:defmacro-test-extensions
        #:dandelion-test-utils
        #:dandelion-main))

(in-package #:dandelion-test-main)

(defmacro with-exit-disabled (&body body)
  `(let ((dandelion-main::*exit-lisp* NIL))
      ,@body
      ))

(define-test test-check-port
  (with-exit-disabled
    (assert-prints "Illegal parameter: port number nil is invalid" (dandelion-main::check-port "nil"))
    (assert-prints "Illegal parameter: port number bla is invalid" (dandelion-main::check-port "bla"))
    (assert-prints "Illegal parameter: port number -1 is invalid" (dandelion-main::check-port "-1"))
    (assert-prints "Illegal parameter: port number 65536 is invalid" (dandelion-main::check-port "65536"))
    (assert-eql 0 (dandelion-main::check-port "0"))
    (assert-eql 65535 (dandelion-main::check-port "65535"))))

(define-test test-check-boolean
  (with-exit-disabled
    (assert-prints "Illegal parameter: illegal bool, must be one of: T NIL" (dandelion-main::check-boolean "a" "bool" nil))
    (assert-prints "Illegal parameter: illegal bool, must be one of: T NIL" (dandelion-main::check-boolean "blubb" "bool" nil))
    (assert-true (dandelion-main::check-boolean "T" "x" nil))
    (assert-false (dandelion-main::check-boolean "nil" "x" nil))
    (assert-eql 50 (dandelion-main::check-boolean nil "x" 50))))

(define-test test-log-level
  (with-exit-disabled
    (assert-prints "Illegal parameter: illegal log level, must be one of: OFF FATAL ERROR WARN INFO DEBUG"
      (dandelion-main::check-log-level "unknown" nil))
    (assert-eql :debug (dandelion-main::check-log-level "debug" nil))
    (assert-eql :error (dandelion-main::check-log-level "error" nil))
    (assert-eql :info (dandelion-main::check-log-level nil :info))))

(define-test test-check-log-file
  (with-exit-disabled
    #+lispworks (assert-equalp (list #p"\"/tmp\"") (dandelion-main::check-log-file "\"/tmp\"" nil))
    (assert-equalp (list #p"bla") (dandelion-main::check-log-file "bla" nil))
    (assert-equalp (list #p"") (dandelion-main::check-log-file "" nil))
    #+lispworks (assert-equalp (list #p"_/") (dandelion-main::check-log-file "_/" nil))
    (assert-equalp (list #p"J:\\mbEigeneDateien\\Studium\\7. Semester\\abschlussarbeit\\workspace\\ClispPlugin\\lib\\CLISP via XML_0.0.1.html")
      (dandelion-main::check-log-file "J:\\mbEigeneDateien\\Studium\\7. Semester\\abschlussarbeit\\workspace\\ClispPlugin\\lib\\CLISP via XML_0.0.1.html" nil))
    (assert-equal 'bla (dandelion-main::check-log-file nil 'bla))))
