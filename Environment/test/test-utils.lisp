;+++++++++++++++++++++++++++++++++++++++++++++
;++
;++ Testmodul de.fh-trier.test.evalserver
;++ Testet alle Funktionen aus dem Evalserver-Modul
;++
;+++++++++++++++++++++++++++++++++++++++++++++

(in-package :cl-user)

(defpackage #:dandelion-test-dandelion-utils
  (:use #:common-lisp 
        #:dandelion-utils
        #:lisp-unit))

(in-package #:dandelion-test-dandelion-utils)

(remove-tests :all)

(define-test test-find-shortest-string
  (assert-error nil (find-shortest-string nil))
  (assert-equal "1" (find-shortest-string (list "1")))
  (assert-equal "1" (find-shortest-string (list "22" "1")))
  (assert-equal "2" (find-shortest-string (list "22" "333" "2" "1")))
  (assert-equal "333" (find-shortest-string (list "4444" "4444" "333" "55555" "666666"))))

(define-test test-read-all-from-string
  (assert-equal '(cl-user::a cl-user::b cl-user::c) (read-all-from-string "a b c  "))
  (assert-equal '(nil ((quote cl-user::a) 1 cl-user::x)) (read-all-from-string "() ('a 1 x)")))

(define-test test-make-formatted-string
  (assert-equal "A B C" (make-formatted-string "~{~a~^ ~}" (list '(a b c))))
  (assert-equal "TEST" (make-formatted-string "~a" (list 'test)))
  (assert-equal "Text" (make-formatted-string "Text" nil))
  (assert-equal "A B C" (make-formatted-string "~a ~a ~a" (list 'a 'b 'c))))