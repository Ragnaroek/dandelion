;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Testmodul de.fh-trier.test.evalserver.main
;++ Testet alle Funktionen aus dem main-Modul
;++
;+++++++++++++++++++++++++++++++++++++++++++++

(in-package #:cl-user)

(defpackage :de.fh-trier.test.evalserver.main
  (:use #:common-lisp 
        #:lisp-unit
        #:de.fh-trier.test.extensions
        #:de.fh-trier.test.utils
        #:de.fh-trier.evalserver.main))

(in-package #:de.fh-trier.test.evalserver.main)

(define-setup 
  (setf de.fh-trier.evalserver.main::*exit-lisp* NIL))

(define-teardown 
  (setf de.fh-trier.evalserver.main::*exit-lisp* T))

;(remove-all-tests)
;(run-tests)

(define-guarded-test test-check-port
  (assert-prints "Illegal parameter: port number nil is invalid" (de.fh-trier.evalserver.main::check-port "nil"))
  (assert-prints "Illegal parameter: port number bla is invalid" (de.fh-trier.evalserver.main::check-port "bla"))
  (assert-prints "Illegal parameter: port number -1 is invalid" (de.fh-trier.evalserver.main::check-port "-1"))
  (assert-prints "Illegal parameter: port number 65536 is invalid" (de.fh-trier.evalserver.main::check-port "65536"))
  (assert-eql 0 (de.fh-trier.evalserver.main::check-port "0"))
  (assert-eql 65535 (de.fh-trier.evalserver.main::check-port "65535")))

(define-guarded-test test-check-boolean
  (assert-prints "Illegal parameter: illegal bool, must be one of: T NIL" (de.fh-trier.evalserver.main::check-boolean "a" "bool" nil))
  (assert-prints "Illegal parameter: illegal bool, must be one of: T NIL" (de.fh-trier.evalserver.main::check-boolean "blubb" "bool" nil))
  (assert-true (de.fh-trier.evalserver.main::check-boolean "T" "x" nil))
  (assert-false (de.fh-trier.evalserver.main::check-boolean "nil" "x" nil))
  (assert-eql 50 (de.fh-trier.evalserver.main::check-boolean nil "x" 50)))

(define-guarded-test test-log-level
  (assert-prints "Illegal parameter: illegal log level, must be one of: DEBUG INFO WARNING ERROR FATAL" 
                 (de.fh-trier.evalserver.main::check-log-level "unknown" nil))
  (assert-eql :debug (de.fh-trier.evalserver.main::check-log-level "debug" nil))
  (assert-eql :error (de.fh-trier.evalserver.main::check-log-level "error" nil))
  (assert-eql :info (de.fh-trier.evalserver.main::check-log-level nil :info)))

(define-guarded-test test-check-log-file
  #+lispworks (assert-equalp (list #p"\"/tmp\"") (de.fh-trier.evalserver.main::check-log-file "\"/tmp\"" nil))
  (assert-equalp (list #p"bla") (de.fh-trier.evalserver.main::check-log-file "bla" nil))
  (assert-equalp (list #p"") (de.fh-trier.evalserver.main::check-log-file "" nil))
  #+lispworks (assert-equalp (list #p"_/") (de.fh-trier.evalserver.main::check-log-file "_/" nil))
  (assert-equalp (list #p"J:\\mbEigeneDateien\\Studium\\7. Semester\\abschlussarbeit\\workspace\\ClispPlugin\\lib\\CLISP via XML_0.0.1.html") 
     (de.fh-trier.evalserver.main::check-log-file "J:\\mbEigeneDateien\\Studium\\7. Semester\\abschlussarbeit\\workspace\\ClispPlugin\\lib\\CLISP via XML_0.0.1.html" nil))

  (assert-equal 'bla (de.fh-trier.evalserver.main::check-log-file nil 'bla)))


  






  


  
