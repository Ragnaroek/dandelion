;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Testmodul de.fh-trier.test.evalserver
;++ Testet alle Funktionen aus dem Evalserver-Modul
;++
;+++++++++++++++++++++++++++++++++++++++++++++

;################### Testpacket um Symbole aus Paket lesen zu testen #######################

(in-package #:cl-user)

(defpackage :de.fh-trier.test.evalserver.testpackage
  (:use #:common-lisp)
  (:export #:test-function0
           #:test-function1
           #:test-function2
           #:test-macro0
           #:test-macro1
           #:test-macro2))

(in-package #:de.fh-trier.test.evalserver.testpackage)

(defun test-function0 nil)
(defun test-function1 (a b c) "Dokumentation" 'x)
(defun test-function2 (a &optional (x (atom 'y)) &key z) "Dok2" 'x)
(defmacro test-macro0 ())
(defmacro test-macro1 (a b c) "Dokumentation" 'x)
(defmacro test-macro2 (a &optional (x (symbolp 1)) &key z) "Dok2" 'x)

;################### ENDE Testpaket #######################

(in-package #:cl-user)

(defpackage :de.fh-trier.test.evalserver
  (:use #:common-lisp 
        #:de.fh-trier.evalserver
        #:port
        #:lisp-unit
        #:de.fh-trier.test.extensions
        #:de.fh-trier.test.utils
        #:de.fh-trier.evalserver.protocol))

(in-package #:de.fh-trier.test.evalserver)

;###########################################
;#
;# Hilfsfunktionen fuer Tests
;#
;###########################################

(define-setup
  (init-protocol))

(define-teardown
  (setf *reader* nil)
  (setf *writer* nil))

;###########################################
;#
;# Tests (run-tests)
;#
;###########################################

(remove-all-tests)

(define-test test-setup-logging
  (assert-error 'error (de.fh-trier.evalserver::setup-logging 'illegal nil nil nil))
  (assert-error 'error (de.fh-trier.evalserver::setup-logging 'file-appender 'illegal nil nil))
  (assert-error 'error (de.fh-trier.evalserver::setup-logging 'file-appender :error 'illegal nil))

  
  (de.fh-trier.evalserver::setup-logging 'log4cl:file-appender :error 'log4cl:simple-layout nil)
  (assert-true de.fh-trier.evalserver::*logger*)
  
  (setf de.fh-trier.evalserver::*logger* nil)
  (de.fh-trier.evalserver::setup-logging 'log4cl:file-appender :debug 'log4cl:html-layout nil)
  (assert-true de.fh-trier.evalserver::*logger*))


(define-test test-not-member->error
  (assert-error 'type-error (de.fh-trier.evalserver::not-member->error 'x nil))
  (assert-error 'type-error (de.fh-trier.evalserver::not-member->error 'x '(a b c)))
  (assert-error 'type-error (de.fh-trier.evalserver::not-member->error nil nil))
  (assert-error 'type-error (de.fh-trier.evalserver::not-member->error nil '(a b c)))
  (de.fh-trier.evalserver::not-member->error 'x '(x y)))

(define-test test-find-package-from-string
  (assert-true (de.fh-trier.evalserver::find-package-from-string ":cl-user")) ;keyword
  (assert-true (de.fh-trier.evalserver::find-package-from-string "#:cl-user"))
  (assert-true (de.fh-trier.evalserver::find-package-from-string "cl-user")) ;symbol
  (assert-true (de.fh-trier.evalserver::find-package-from-string "\"CL-USER\"")) ;string in string

  (assert-false (de.fh-trier.evalserver::find-package-from-string "(")) ;fehlerhaft, read-from-string error
  (assert-false (de.fh-trier.evalserver::find-package-from-string "\"cl-user\"")) ;klein geschrieben in string
  (assert-false (de.fh-trier.evalserver::find-package-from-string "PACKAGE-DOES-NOT-EXIST")))

(define-guarded-test test-handle-package-request
  (with-test-writer2
    (de.fh-trier.evalserver::handle-package-request *writer*)

    (let ((packages (read-file)))
      (assert-true (member "common-lisp-user" packages :test #'equalp))
      (assert-true (member "common-lisp" packages :test #'equalp)))))

(define-guarded-test test-handle-symbol-request
  (with-test-writer2
   (assert-error 'malformed-protocol-error (de.fh-trier.evalserver::handle-symbol-request *writer* "FUNCTIONS" :function))
   (assert-error 'malformed-protocol-error (de.fh-trier.evalserver::handle-symbol-request *writer* "FUNCTIONS :cl-user" :macros))
   (assert-error 'malformed-protocol-error (de.fh-trier.evalserver::handle-symbol-request *writer* "MACROS" :macros))
   (assert-error 'malformed-protocol-error (de.fh-trier.evalserver::handle-symbol-request *writer* "MACROS :cl-user" :function)))

  (with-test-writer2
   (de.fh-trier.evalserver::handle-symbol-request *writer* "FUNCTIONS NO-SUCH-PACKAGE" :function)
   (assert-equalp '("ERROR no such package: NO-SUCH-PACKAGE") (read-file)))

  (with-test-writer2
   (de.fh-trier.evalserver::handle-symbol-request *writer* "FUNCTIONS de.fh-trier.test.evalserver.testpackage" :function)
   (assert-equalp '("FUNCTION-LIST 3" "TEST-FUNCTION0 TklM" "TEST-FUNCTION1 RG9rdW1lbnRhdGlvbg== A B C"
                                      "TEST-FUNCTION2 RG9rMg== A &OPTIONAL X &KEY Z") (read-file)))

  (with-test-writer2
   (de.fh-trier.evalserver::handle-symbol-request *writer* "MACROS NO-SUCH-PACKAGE" :macros)
   (assert-equalp '("ERROR no such package: NO-SUCH-PACKAGE") (read-file)))

  (with-test-writer2
   (de.fh-trier.evalserver::handle-symbol-request *writer* "MACROS de.fh-trier.test.evalserver.testpackage" :macros)
   (assert-equalp '("FUNCTION-LIST 3" "TEST-MACRO0 TklM" "TEST-MACRO1 RG9rdW1lbnRhdGlvbg== A B C"
                                      "TEST-MACRO2 RG9rMg== A &OPTIONAL X &KEY Z") (read-file))))

(define-test test-get-shortest-package-name
  (assert-equalp "CL-USER" (de.fh-trier.evalserver::get-shortest-package-name (find-package 'cl-user)))
  (assert-equalp "CL" (de.fh-trier.evalserver::get-shortest-package-name (find-package 'cl)))
  (assert-error 'error (de.fh-trier.evalserver::get-shortest-package-name nil)))


(define-test test-unique-name-restart
  (assert-equal nil (de.fh-trier.evalserver::unique-name-restart nil))
  (let (test-restart abort-restart)
  ;test-restart erzeugen
    (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
      (handler-bind ((error #'(lambda (c)
                                (setf abort-restart (find-restart 'abort c))
                                (setf test-restart (find-restart 'use-value c)))))
        (restart-case (error "a error")
          (use-value (value) value))))
  (assert-equal (list (list "USE-VALUE" test-restart)) (de.fh-trier.evalserver::unique-name-restart (list test-restart)))
  (assert-equal (list (list "USE-VALUE" test-restart) (list "USE-VALUE-1" test-restart))
                (de.fh-trier.evalserver::unique-name-restart (list test-restart test-restart))) ;zweimal selber name
  (assert-equal (list (list "USE-VALUE" test-restart) (list "USE-VALUE-1" test-restart) (list "USE-VALUE-2" test-restart)
                      (list "USE-VALUE-3" test-restart) (list "USE-VALUE-4" test-restart))
                (de.fh-trier.evalserver::unique-name-restart (list test-restart test-restart test-restart test-restart test-restart)))
  ;test gemischt mit anderen Restarts
  (assert-equal (list (list "USE-VALUE" test-restart) (list "ABORT" abort-restart)) 
                (de.fh-trier.evalserver::unique-name-restart (list test-restart abort-restart)))
  (assert-equal (list (list "USE-VALUE" test-restart) (list "ABORT" abort-restart) (list "ABORT-1" abort-restart))
                (de.fh-trier.evalserver::unique-name-restart (list test-restart abort-restart abort-restart)))
  (assert-equal (list (list "USE-VALUE" test-restart) (list "ABORT" abort-restart) (list "ABORT-1" abort-restart)
                      (list "USE-VALUE-2" test-restart))
                (de.fh-trier.evalserver::unique-name-restart (list test-restart abort-restart abort-restart test-restart)))))

(define-test test-find-selected-restart
   (let (test-restart abort-restart assoc)
  ;test-restart erzeugen
    (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
      (handler-bind ((error #'(lambda (c)
                                (setf abort-restart (find-restart 'abort c))
                                (setf test-restart (find-restart 'use-value c)))))
        (restart-case (error "a error")
          (use-value (value) value))))
    (setf assoc (de.fh-trier.evalserver::unique-name-restart (list test-restart abort-restart abort-restart test-restart)))
    
    (assert-equal nil (de.fh-trier.evalserver::find-selected-restart nil nil))
    (assert-equal test-restart (de.fh-trier.evalserver::find-selected-restart assoc "USE-VALUE"))
    (assert-equal test-restart (de.fh-trier.evalserver::find-selected-restart assoc "USE-VALUE-2"))
    (assert-equal abort-restart (de.fh-trier.evalserver::find-selected-restart assoc "ABORT"))
    (assert-equal abort-restart (de.fh-trier.evalserver::find-selected-restart assoc "ABORT-1"))
    (assert-equal nil (de.fh-trier.evalserver::find-selected-restart assoc "<unknown>"))))

(define-test test-result->string
  (assert-false (de.fh-trier.evalserver::result->string nil))
  (assert-equalp '("a") (de.fh-trier.evalserver::result->string '(a)))
  (assert-equalp '("1") (de.fh-trier.evalserver::result->string '(1)))
  (assert-equalp '("\"string \"") (de.fh-trier.evalserver::result->string '("string ")))
  (assert-equalp '("1/3") (de.fh-trier.evalserver::result->string (list (/ 1 3))))
  
  (assert-equalp '("a" "b" "1") (de.fh-trier.evalserver::result->string '(a b 1)))
  (assert-equalp '("\"s\"" "1/3" "(QUOTE A)") (de.fh-trier.evalserver::result->string (list "s" (/ 1 3) ''a)))

  (assert-equalp '("#'(LAMBDA NIL NIL)") (de.fh-trier.evalserver::result->string (list #'(lambda () nil))))
  (assert-equalp '("#P\"/test/bla\"") (de.fh-trier.evalserver::result->string (list #P"/test/bla"))))

(define-test test-replace-return
  (let (string result)
    (setf string "")
    (setf string (de.fh-trier.evalserver::replace-return string))
    (assert-equal "" string)

    (setf string (make-string 1 :initial-element #\return))
    ;(setf (char string 0) #\return)
    (setf string (de.fh-trier.evalserver::replace-return string))
    (setf result (make-string 1 :initial-element #\return))
    (setf (char result 0) #\newline)
    (assert-equal result string)

    (setf string "abcdefgha")
    (setf string (de.fh-trier.evalserver::replace-return string))
    (assert-equal "abcdefgha" string)

    (setf string (make-string 10 :initial-element #\1))
    (setf (char string 0) #\return)
    (setf (char string 9) #\return)
    (setf string (de.fh-trier.evalserver::replace-return string))
    (setf result (make-string 10 :initial-element #\1))
    (setf (char result 0) #\newline)
    (setf (char result 9) #\newline)
    (assert-equal result string)))
;(remove-all-tests)
;(run-tests)






