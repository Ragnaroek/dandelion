;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Testmodul de.fh-trier.test.evalserver
;++ Testet alle Funktionen aus dem Evalserver-Modul
;++
;+++++++++++++++++++++++++++++++++++++++++++++

;################### Testpacket um Symbole aus Paket lesen zu testen #######################

(in-package #:cl-user)

(defpackage #:dandelion-test-server-pkg
  (:use #:common-lisp)
  (:export #:test-function0
           #:test-function1
           #:test-function2
           #:test-macro0
           #:test-macro1
           #:test-macro2))

(in-package #:dandelion-test-server-pkg)

(defun test-function0 nil)
(defun test-function1 (a b c) "Dokumentation" (list a b c))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(defun test-function2 (a &optional (x (atom 'y)) &key z) "Dok2" 'x)
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

(defmacro test-macro0 ())
(defmacro test-macro1 (a b c) "Dokumentation" 
  (declare (ignore a b c))
  'x)

(defmacro test-macro2 (a &optional (x (symbolp 1)) &key z) "Dok2" (list a x z))

;################### ENDE Testpaket #######################

(in-package #:cl-user)

(defpackage #:dandelion-test-server
  (:use #:common-lisp 
        #:dandelion-server
        #:lisp-unit
        #:defmacro-test-extensions
        #:dandelion-test-utils
        #:dandelion-test-protocol))

(in-package #:dandelion-test-server)

;###########################################
;#
;# Tests (run-tests)
;#
;###########################################

(remove-tests :all)

(define-test test-setup-logging
  (assert-error 'error (dandelion-server::setup-logging 'illegal))
  (dandelion-server::setup-logging :debug))

(define-test test-not-member->error
  (assert-error 'type-error (dandelion-server::not-member->error 'x nil))
  (assert-error 'type-error (dandelion-server::not-member->error 'x '(a b c)))
  (assert-error 'type-error (dandelion-server::not-member->error nil nil))
  (assert-error 'type-error (dandelion-server::not-member->error nil '(a b c)))
  (dandelion-server::not-member->error 'x '(x y)))

(define-test test-find-package-from-string
  (assert-true (dandelion-server::find-package-from-string ":cl-user")) ;keyword
  (assert-true (dandelion-server::find-package-from-string "#:cl-user"))
  (assert-true (dandelion-server::find-package-from-string "cl-user")) ;symbol
  (assert-true (dandelion-server::find-package-from-string "\"CL-USER\"")) ;string in string

  (assert-false (dandelion-server::find-package-from-string "(")) ;fehlerhaft, read-from-string error
  (assert-false (dandelion-server::find-package-from-string "\"cl-user\"")) ;klein geschrieben in string
  (assert-false (dandelion-server::find-package-from-string "PACKAGE-DOES-NOT-EXIST")))

(define-test test-handle-package-request
  (with-test-writer
    (dandelion-server::handle-package-request *writer*)

    (let ((packages (read-test-file)))
      (assert-true (member "common-lisp-user" packages :test #'equalp))
      (assert-true (member "common-lisp" packages :test #'equalp)))))

(define-test test-handle-symbol-request
  (with-test-writer
   (assert-error 'malformed-protocol-error (dandelion-server::handle-symbol-request *writer* "FUNCTIONS" :function))
   (assert-error 'malformed-protocol-error (dandelion-server::handle-symbol-request *writer* "FUNCTIONS :cl-user" :macros))
   (assert-error 'malformed-protocol-error (dandelion-server::handle-symbol-request *writer* "MACROS" :macros))
   (assert-error 'malformed-protocol-error (dandelion-server::handle-symbol-request *writer* "MACROS :cl-user" :function)))

  (with-test-writer
   (dandelion-server::handle-symbol-request *writer* "FUNCTIONS NO-SUCH-PACKAGE" :function)
   (assert-equalp '("ERROR no such package: NO-SUCH-PACKAGE") (read-test-file)))

  (with-test-writer
    (dandelion-server::handle-symbol-request *writer* "FUNCTIONS dandelion-test-server-pkg" :function)
    (let ((f (read-test-file)))
      (assert-equalp "FUNCTION-LIST 3" (car f))
      (assert-equal-str-set '("TEST-FUNCTION0 TklM" "TEST-FUNCTION1 RG9rdW1lbnRhdGlvbg== A B C"
                                      "TEST-FUNCTION2 RG9rMg== A &OPTIONAL X &KEY Z") (cdr f))))

  (with-test-writer
   (dandelion-server::handle-symbol-request *writer* "MACROS NO-SUCH-PACKAGE" :macros)
   (assert-equalp '("ERROR no such package: NO-SUCH-PACKAGE") (read-test-file)))

  (with-test-writer
   (dandelion-server::handle-symbol-request *writer* "MACROS dandelion-test-server-pkg" :macros)
   (let ((f (read-test-file)))
     (assert-equalp "FUNCTION-LIST 3" (car f)
     (assert-equalp '("FUNCTION-LIST 3" "TEST-MACRO0 TklM" "TEST-MACRO1 RG9rdW1lbnRhdGlvbg== A B C"
                      "TEST-MACRO2 RG9rMg== A &OPTIONAL X &KEY Z") (cdr f))))))

(define-test test-get-shortest-package-name
  (assert-equalp "CL-USER" (dandelion-server::get-shortest-package-name (find-package 'cl-user)))
  (assert-equalp "CL" (dandelion-server::get-shortest-package-name (find-package 'cl)))
  (assert-error 'error (dandelion-server::get-shortest-package-name nil)))


(define-test test-unique-name-restart
  (assert-equal nil (dandelion-server::unique-name-restart nil))
  (let (test-restart abort-restart)
  ;test-restart erzeugen
    (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
      (handler-bind ((error #'(lambda (c)
                                (setf abort-restart (find-restart 'abort c))
                                (setf test-restart (find-restart 'use-value c)))))
        (restart-case (error "a error")
          (use-value (value) value))))
  (assert-equal (list (list "USE-VALUE" test-restart)) (dandelion-server::unique-name-restart (list test-restart)))
  (assert-equal (list (list "USE-VALUE" test-restart) (list "USE-VALUE-1" test-restart))
                (dandelion-server::unique-name-restart (list test-restart test-restart))) ;zweimal selber name
  (assert-equal (list (list "USE-VALUE" test-restart) (list "USE-VALUE-1" test-restart) (list "USE-VALUE-2" test-restart)
                      (list "USE-VALUE-3" test-restart) (list "USE-VALUE-4" test-restart))
                (dandelion-server::unique-name-restart (list test-restart test-restart test-restart test-restart test-restart)))
  ;test gemischt mit anderen Restarts
  (assert-equal (list (list "USE-VALUE" test-restart) (list "ABORT" abort-restart)) 
                (dandelion-server::unique-name-restart (list test-restart abort-restart)))
  (assert-equal (list (list "USE-VALUE" test-restart) (list "ABORT" abort-restart) (list "ABORT-1" abort-restart))
                (dandelion-server::unique-name-restart (list test-restart abort-restart abort-restart)))
  (assert-equal (list (list "USE-VALUE" test-restart) (list "ABORT" abort-restart) (list "ABORT-1" abort-restart)
                      (list "USE-VALUE-2" test-restart))
                (dandelion-server::unique-name-restart (list test-restart abort-restart abort-restart test-restart)))))

(define-test test-find-selected-restart
   (let (test-restart abort-restart assoc)
  ;test-restart erzeugen
    (ignore-errors ;verhindert aufruf des debuggers, da handler-bind die error-condition nicht behandelt
      (handler-bind ((error #'(lambda (c)
                                (setf abort-restart (find-restart 'abort c))
                                (setf test-restart (find-restart 'use-value c)))))
        (restart-case (error "a error")
          (use-value (value) value))))
    (setf assoc (dandelion-server::unique-name-restart (list test-restart abort-restart abort-restart test-restart)))
    
    (assert-equal nil (dandelion-server::find-selected-restart nil nil))
    (assert-equal test-restart (dandelion-server::find-selected-restart assoc "USE-VALUE"))
    (assert-equal test-restart (dandelion-server::find-selected-restart assoc "USE-VALUE-2"))
    (assert-equal abort-restart (dandelion-server::find-selected-restart assoc "ABORT"))
    (assert-equal abort-restart (dandelion-server::find-selected-restart assoc "ABORT-1"))
    (assert-equal nil (dandelion-server::find-selected-restart assoc "<unknown>"))))

(define-test test-result->string
  (assert-false (dandelion-server::result->string nil))
  (assert-equalp '("DANDELION-TEST-SERVER::A") (dandelion-server::result->string '(a)))
  (assert-equalp '("1") (dandelion-server::result->string '(1)))
  (assert-equalp '("\"string \"") (dandelion-server::result->string '("string ")))
  (assert-equalp '("1/3") (dandelion-server::result->string (list (/ 1 3))))
  
  (assert-equalp '("DANDELION-TEST-SERVER::A" "DANDELION-TEST-SERVER::B" "1") (dandelion-server::result->string '(a b 1)))
  (assert-equalp '("\"s\"" "1/3" "'DANDELION-TEST-SERVER::A") (dandelion-server::result->string (list "s" (/ 1 3) ''a)))

  (assert-equalp '("#P\"/test/bla\"") (dandelion-server::result->string (list #P"/test/bla"))))

(define-test test-replace-return
  (let (string result)
    (setf string "")
    (setf string (dandelion-server::replace-return string))
    (assert-equal "" string)

    (setf string (make-string 1 :initial-element #\return))
    ;(setf (char string 0) #\return)
    (setf string (dandelion-server::replace-return string))
    (setf result (make-string 1 :initial-element #\return))
    (setf (char result 0) #\newline)
    (assert-equal result string)

    (setf string "abcdefgha")
    (setf string (dandelion-server::replace-return string))
    (assert-equal "abcdefgha" string)

    (setf string (make-string 10 :initial-element #\1))
    (setf (char string 0) #\return)
    (setf (char string 9) #\return)
    (setf string (dandelion-server::replace-return string))
    (setf result (make-string 10 :initial-element #\1))
    (setf (char result 0) #\newline)
    (setf (char result 9) #\newline)
    (assert-equal result string)))







