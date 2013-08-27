;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Testmodul de.fh-trier.test.evalserver.protocol
;++ Testet alle Funktionen aus dem Protocol-Modul
;++
;+++++++++++++++++++++++++++++++++++++++++++++

(in-package :cl-user)

(defpackage #:dandelion-test-protocol
  (:use #:common-lisp 
        #:dandelion-protocol
        #:dandelion-test-utils
        #:lisp-unit
        #:defmacro-test-extensions
        #:dandelion-test-meta))

(in-package #:dandelion-test-protocol)

;(lisp-unit:run-tests (list 'TEST-WRITE-FUNCTION-LIST) :dandelion-test-protocol)
;(lisp-unit:run-tests :all :dandelion-test-protocol)
;###########################################
;#
;# Hilfsfunktionen fuer Tests
;#
;###########################################

(remove-tests :all)

;###########################################
;#
;# Tests (run-tests)
;#
;###########################################

;; Test Objektinitialisierung + Objektmethoden

(define-test test-make-instance-protocol-reader ()
  (assert-error 'error (make-instance 'protocol-reader))
  (assert-error 'type-error (make-instance 'protocol-reader :input-stream nil))
  (assert-error 'type-error (make-instance 'protocol-reader :input-stream (make-string-output-stream))) ;kein input-stream
  (assert-eql *standard-input* (input-stream (make-instance 'protocol-reader :input-stream *standard-input*))))

(define-test test-make-instance-protocol-writer ()
  (assert-error 'error (make-instance 'protocol-writer))
  (assert-error 'type-error (make-instance 'protocol-writer :output-stream nil))
  (assert-error 'type-error (make-instance 'protocol-writer :output-stream (make-string-input-stream "jr")))
  (assert-eql *standard-output* (output-stream (make-instance 'protocol-writer :output-stream *standard-output*))))

(define-test test-read-line-from-reader
  (with-test-reader "line" 
    (assert-equal "line" (read-line-from-reader *reader*))))

(define-test test-write-line-to-writer
  (with-test-writer
    (write-line-to-writer *writer* "text written")
    (assert-equalp (list "text written") (read-test-file))))


;; Test Read-Funktionen + Makros

(define-test testmacro-with-line-read
  (with-test-reader "the-line"
    (with-line-read *reader* linevar
      (assert-equal "the-line" linevar))
    (assert-error 'unbound-variable (symbol-value 'linevar))))

(define-test test-read-identifier
  (assert-error 'malformed-protocol-error (read-identifier "###"))
  (assert-error 'malformed-protocol-error (read-identifier nil))
  (assert-error 'malformed-protocol-error (read-identifier ""))
  (assert-error 'malformed-protocol-error (read-identifier "43%$&"))
  (assert-equal "valid-id" (read-identifier "valid-id"))
  (assert-equal "valid-id" (read-identifier "valid-id rest args")))

(define-test test-read-connect
  (assert-error 'malformed-protocol-error (read-connect nil))
  (assert-error 'malformed-protocol-error (read-connect ""))
  (assert-error 'malformed-protocol-error (read-connect "CONNEC 192.168.0.1 80"))
  (assert-error 'malformed-protocol-error (read-connect "CONNECT"))
  (assert-error 'malformed-protocol-error (read-connect "CONNECT host"))
  (assert-error 'malformed-protocol-error (read-connect "CONNECT %&illegal-host 31337"))
  (assert-error 'malformed-protocol-error (read-connect "CONNECT host 78777788")) ;Port ungueltig
  (assert-error 'malformed-protocol-error (read-connect "host 789"))
  (assert-equal '("CONNECT" "host" 80) (read-connect "CONNECT host 80"))
  (assert-equal '("CONNECT" "192.168.0.100" 31337) (read-connect "CONNECT 192.168.0.100 31337")))


(define-test test-read-eval
  (assert-error 'malformed-protocol-error (read-eval nil))
  (assert-error 'malformed-protocol-error (read-eval ""))
  (assert-error 'malformed-protocol-error (read-eval "EVA package Zm9ybQ=="))
  (assert-error 'malformed-protocol-error (read-eval "EVAL"))
  (assert-error 'malformed-protocol-error (read-eval "EVAL package"))
  (assert-error 'malformed-protocol-error (read-eval "EVAL pack age Zm9ybQ=="))
  (assert-error 'malformed-protocol-error (read-eval "EVAL :package keinbase64%%Zm9ybQ=="))
  (assert-equal '("EVAL" ":package" "form") (read-eval "EVAL :package Zm9ybQ=="))
  (assert-equal '("EVAL" "the-package-symbol" "(setf x 'y)") (read-eval "EVAL the-package-symbol KHNldGYgeCAneSk=")))

(define-test test-read-invoke-restart
  (assert-error 'malformed-protocol-error (read-invoke-restart nil))
  (assert-error 'malformed-protocol-error (read-invoke-restart ""))
  (assert-error 'malformed-protocol-error (read-invoke-restart "INVOKE symbol YXJncw=="))
  (assert-error 'malformed-protocol-error (read-invoke-restart "INVOKE-RESTART"))
  (assert-error 'malformed-protocol-error (read-invoke-restart "INVOKE-RESTART illegal()symbol YXJncw=="))
  (assert-error 'malformed-protocol-error (read-invoke-restart "INVOKE-RESTART sym $%%%%"))
  (assert-equal '("INVOKE-RESTART" "sym") (read-invoke-restart "INVOKE-RESTART sym"))
  (assert-equal '("INVOKE-RESTART" "sym" "args") (read-invoke-restart "INVOKE-RESTART sym YXJncw==")))

(define-test test-read-function-request
  (assert-error 'malformed-protocol-error (read-function-request nil))
  (assert-error 'malformed-protocol-error (read-function-request ""))
  (assert-error 'malformed-protocol-error (read-function-request "FUNCTIONS"))
  (assert-error 'malformed-protocol-error (read-function-request "FUNCTIONS "))
  (assert-equalp '("FUNCTIONS" "package") (read-function-request "FUNCTIONS package"))
  (assert-equalp '("FUNCTIONS" ":the-package") (read-function-request "FUNCTIONS :the-package")))

(define-test test-read-macro-request
  (assert-error 'malformed-protocol-error (read-macro-request nil))
  (assert-error 'malformed-protocol-error (read-macro-request ""))
  (assert-error 'malformed-protocol-error (read-macro-request "MACROS"))
  (assert-error 'malformed-protocol-error (read-macro-request "MACROS "))
  (assert-equalp '("MACROS" "package") (read-macro-request "MACROS package"))
  (assert-equalp '("MACROS" ":the-package") (read-macro-request "MACROS :the-package")))

(define-test test-write-ok
  (with-test-writer
    (write-ok *writer*)
    (assert-equalp (list +token-ok+) (read-test-file))))

(define-test test-write-error
  (with-test-writer
   (write-error *writer* "unknown error ~a" 'blubb)
    (assert-equalp (list (format nil "~a ~a" +token-error+ "dW5rbm93biBlcnJvciBCTFVCQg==")) (read-test-file)))

  (with-test-writer
    (write-error *writer* "unknown error")
    (assert-equalp (list (format nil "~a ~a" +token-error+ "dW5rbm93biBlcnJvcg==")) (read-test-file)))

  (with-test-writer
    (write-error *writer* "error ~a ~a" "package not found:" 'the-package)
    (assert-equal (list (format nil "~a ~a" +token-error+ "ZXJyb3IgcGFja2FnZSBub3QgZm91bmQ6IFRIRS1QQUNLQUdF")) (read-test-file))))


(define-test test-write-eval-success
  (with-test-writer
    (write-eval-success *writer* ":the-package" (list "(result form)"))
    (assert-equalp (list (format nil "~a ~a ~a" +token-ok+ ":the-package" "KHJlc3VsdCBmb3JtKQ==")) (read-test-file))
    (assert-error 'error (write-eval-success *writer* 'sym 'sym)))
  
  (with-test-writer
    (write-eval-success *writer* ":the-package" (list "(result form)" "(multi value 1)" "mv-2"))
    (assert-equalp (list (format nil "~a ~a ~a" +token-ok+ ":the-package" "KHJlc3VsdCBmb3JtKQ== KG11bHRpIHZhbHVlIDEp bXYtMg==")) (read-test-file)))

  (with-test-writer
    (write-eval-success *writer* ":the-package" nil)
    (assert-equalp (list (format nil "~a ~a ~a" +token-ok+ ":the-package" "TklM")) (read-test-file))
    (assert-error 'error (write-eval-success *writer* 'sym 'sym))))


(define-test test-write-eval-error
  ;ein Restart
  (with-test-writer
    (let (test-restart)
      ;test-restart erzeugen
      (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
        (handler-bind ((error #'(lambda (c)
                                  (setf test-restart (find-restart 'use-value c)))))
          (restart-case (error "a error")
            (use-value (value) value))))
      
      (write-eval-error *writer* (dandelion-server::unique-name-restart (list test-restart)) "eval failed: ~a" "the reason")
      (assert-equalp (list (format nil "~a ~a" +token-eval-error+ "ZXZhbCBmYWlsZWQ6IHRoZSByZWFzb24= USE-VALUE VVNFLVZBTFVF")) (read-test-file))))

  ;zwei Restarts
  (with-test-writer
    (let (test-restart)
      ;test-restart erzeugen
      (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
        (handler-bind ((error #'(lambda (c)
                                  (setf test-restart (find-restart 'use-value c)))))
          (restart-case (error "a error")
            (use-value (value) value))))
      
      (write-eval-error *writer* (dandelion-server::unique-name-restart (list test-restart test-restart))
                        "Errormessage benutzt kein Format")
      (assert-equalp (list (format nil "~a ~a ~a" +token-eval-error+ "RXJyb3JtZXNzYWdlIGJlbnV0enQga2VpbiBGb3JtYXQ= USE-VALUE VVNFLVZBTFVF" 
                                    "USE-VALUE-1 VVNFLVZBTFVF")) (read-test-file))))

  ;mehrere Restarts
  (with-test-writer
    (let (test-restart)
      ;test-restart erzeugen
      (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
        (handler-bind ((error #'(lambda (c)
                                  (setf test-restart (find-restart 'use-value c)))))
          (restart-case (error "a error")
            (use-value (value) value))))
      
      (write-eval-error *writer* (dandelion-server::unique-name-restart (list test-restart test-restart test-restart))
                        "Errormessage benutzt kein Format")
      (assert-equalp (list (format nil "~a ~a ~a ~a" +token-eval-error+ "RXJyb3JtZXNzYWdlIGJlbnV0enQga2VpbiBGb3JtYXQ= USE-VALUE VVNFLVZBTFVF" 
                                    "USE-VALUE-1 VVNFLVZBTFVF" "USE-VALUE-2 VVNFLVZBTFVF")) (read-test-file))))
  
  (with-test-writer
    (let (test-restart)
      ;test-restart erzeugen
      (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
        (handler-bind ((error #'(lambda (c)
                                  (setf test-restart (find-restart 'use-value c)))))
          (restart-case (error "a error")
            (use-value (value) value))))
      
      (write-eval-error *writer* (dandelion-server::unique-name-restart (list test-restart test-restart test-restart test-restart))
                        "Errormessage benutzt kein Format")
      (assert-equalp (list (format nil "~a ~a ~a ~a ~a" +token-eval-error+ 
                                   "RXJyb3JtZXNzYWdlIGJlbnV0enQga2VpbiBGb3JtYXQ= USE-VALUE VVNFLVZBTFVF" 
                                   "USE-VALUE-1 VVNFLVZBTFVF" "USE-VALUE-2 VVNFLVZBTFVF" "USE-VALUE-3 VVNFLVZBTFVF")) (read-test-file))))

  ;kein Restart verfuegbar, nur Meldung
  (with-test-writer
    (write-eval-error *writer* nil "Panic: Kein Restart!")
    (assert-equalp (list (format nil "~a ~a" +token-eval-error+ "UGFuaWM6IEtlaW4gUmVzdGFydCE=")) (read-test-file)))

  (with-test-writer
    (assert-error 'error (write-eval-error *writer* "string" "not used format"))))

(define-test test-write-read-error
  (with-test-writer
    (write-read-error *writer* "Error while reading ~a" "(form)")
    (assert-equalp (list (format nil "~a ~a" +token-read-error+ "RXJyb3Igd2hpbGUgcmVhZGluZyAoZm9ybSk=")) (read-test-file)))
  
  (with-test-writer
    (write-read-error *writer* "Error ohne beschreibung")
    (assert-equalp (list (format nil "~a ~a" +token-read-error+ "RXJyb3Igb2huZSBiZXNjaHJlaWJ1bmc=")) (read-test-file))))

(define-test test-write-package-list
  (with-test-writer
    (write-package-list *writer* (list (find-package 'common-lisp) (find-package 'cl-user) (find-package :dandelion-protocol)))
    (assert-equalp '("PACKAGE-LIST 3" "COMMON-LISP" "COMMON-LISP-USER" "DANDELION-PROTOCOL") (read-test-file)))
  
  (with-test-writer
    (write-package-list *writer* nil)
    (assert-equalp '("PACKAGE-LIST 0") (read-test-file)))

  (with-test-writer
    (write-package-list *writer* (list (find-package 'cl-user)))
    (assert-equalp '("PACKAGE-LIST 1" "COMMON-LISP-USER") (read-test-file))))


(define-test test-write-function-list
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (defun test-function0 nil nil)
  (defun test-function1 (a b c)
    "Dokumentation" 'x)
  (defun test-function2 (arg1 &optional (bla 'x) &rest rest &key key1 key2)
    "Dokumentation mit Leerzeichen" 'y)
  #+sbcl (locally (declare (sb-ext:unmuffle-conditions style-warning)))  
  
  (assert-true (fboundp 'test-function0))
  (assert-true (fboundp 'test-function1))
  (assert-true (fboundp 'test-function2))
  
  (with-test-writer
    (pprint "###########writer is: ")
    (pprint *writer*)
    (write-function-list *writer* nil)
    (assert-equalp '("FUNCTION-LIST 0") (read-test-file)))
  
  (with-test-writer
    (write-function-list *writer* (list 'test-function0))
    (assert-equalp '("FUNCTION-LIST 1" "TEST-FUNCTION0 TklM") (read-test-file)))

  (with-test-writer
    (write-function-list *writer* (list 'test-function1))
    (assert-equalp '("FUNCTION-LIST 1" "TEST-FUNCTION1 RG9rdW1lbnRhdGlvbg== A B C") (read-test-file)))
  
  (with-test-writer
    (write-function-list *writer* (list 'test-function2))
    (assert-equalp 
     '("FUNCTION-LIST 1" 
       "TEST-FUNCTION2 RG9rdW1lbnRhdGlvbiBtaXQgTGVlcnplaWNoZW4= ARG1 &OPTIONAL BLA &REST REST &KEY KEY1 KEY2") 
     (read-test-file)))

  (with-test-writer
    (write-function-list *writer* (list 'test-function1 'test-function2))
    (assert-equalp 
     '("FUNCTION-LIST 2"
       "TEST-FUNCTION1 RG9rdW1lbnRhdGlvbg== A B C"
       "TEST-FUNCTION2 RG9rdW1lbnRhdGlvbiBtaXQgTGVlcnplaWNoZW4= ARG1 &OPTIONAL BLA &REST REST &KEY KEY1 KEY2")
     (read-test-file)))

  ;destructuring-test

  (with-test-writer
    (write-function-list *writer* (list 'dummy-macro-1))
    (assert-equalp 
     '("FUNCTION-LIST 1"
       "DUMMY-MACRO-1 TklM ( VAR EXPR ) &BODY BODY")
     (read-test-file)))

  (with-test-writer
    (write-function-list *writer* (list 'dummy-macro-1 'dummy-macro-2))
    (assert-equalp 
     '("FUNCTION-LIST 2"
       "DUMMY-MACRO-1 TklM ( VAR EXPR ) &BODY BODY"
       "DUMMY-MACRO-2 TklM ( VAR EXPR &KEY KEY ) &BODY BODY &KEY KEY-1 KEY-2 KEY-3")
     (read-test-file)))

  (with-test-writer
    (write-function-list *writer* (list 'dummy-macro-1 'dummy-macro-2 'dummy-macro-3))
    (assert-equalp 
     '("FUNCTION-LIST 3"
       "DUMMY-MACRO-1 TklM ( VAR EXPR ) &BODY BODY"
       "DUMMY-MACRO-2 TklM ( VAR EXPR &KEY KEY ) &BODY BODY &KEY KEY-1 KEY-2 KEY-3"
       "DUMMY-MACRO-3 TklM ( VAR EXPR ( VAR2 EXPR2 ( VAR3 EXPR3 ) ) ) ( VAR4 EXPR4 ) &OPTIONAL OPT-1 &REST REST")
     (read-test-file))))

(define-test test-write-io-termination ()
  (let (string)
    (with-test-writer
      (write-io-termination (output-stream *writer*))
      (setf string (second (read-test-file)))
      (assert-equal (code-char 0) (char string 0)))))










