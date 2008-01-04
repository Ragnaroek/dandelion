(in-package :cl-user)

(defpackage :de.fh-trier.test.evalserver.meta
  (:use #:common-lisp 
        #:de.fh-trier.evalserver.meta
        #:de.fh-trier.test.utils
        #:lisp-unit
        #:de.fh-trier.test.extensions)
  (:export #:dummy-macro-1
           #:dummy-macro-2
           #:dummy-macro-3
           #:dummy-function-1
           #:dummy-function-2
           #:dummy-function-3))

(in-package #:de.fh-trier.test.evalserver.meta)

;###########################################
;#
;# Testfunktionen
;#
;###########################################

(defmacro dummy-macro-1 ((var expr) &body body)
 nil)

(defmacro dummy-macro-2 ((var expr &key key) &body body &key key-1 (key-2 0) (key-3 (+ 1 2 3) supplied-p))
 nil)

(defmacro dummy-macro-3 ((var expr (var2 expr2 (var3 expr3))) (var4 expr4) &optional (opt-1 0) &rest rest)
 nil)

(defun dummy-function-1 (a b c &rest rest)
  nil)

(defun dummy-function-2 (a &rest rest &key key (key-2 0) (key-3 (+ 1 2 3) supplied-p))
  nil)

(defun dummy-function-3 (a &optional (opt-1 0) &rest rest)
  nil)

;###########################################
;#
;# Tests (run-tests)
;#
;###########################################
(remove-all-tests)

(define-test test-package-symbols 
  (assert-true (member "COMMON-LISP-USER" (package-symbols) :key #'package-name :test #'equalp))
  (assert-true (member "COMMON-LISP" (package-symbols) :key #'package-name :test #'equalp)))


(define-test test-function-symbols
  (let (f)
    (setf f (function-symbols (find-package :de.fh-trier.test.evalserver.meta)))
    (assert-eql 3 (length f))
    (assert-true (member 'dummy-function-1 f))
    (assert-true (member 'dummy-function-2 f))
    (assert-true (member 'dummy-function-3 f))

    (setf f (function-symbols (find-package :cl)))
    (assert-true (member 'quote f))
    (assert-true (member 'mapcar f))
    (assert-true (member 'length f))))

(define-test test-macro-symbols
  (let (f)
    (setf f (macro-symbols (find-package :de.fh-trier.test.evalserver.meta)))
    (assert-eql 3 (length f))
    (assert-true (member 'dummy-macro-1 f))
    (assert-true (member 'dummy-macro-2 f))
    (assert-true (member 'dummy-macro-3 f))

    (setf f (macro-symbols (find-package :cl)))
    (assert-true (member 'defun f))
    (assert-true (member 'defmacro f))
    (assert-true (member 'dotimes f))))


(define-test test-function-arglist->string 
  (assert-equal '("(" "VAR" "EXPR" ")" "&BODY" "BODY") 
                (function-arglist->string 'dummy-macro-1))

  (assert-equal '("(" "VAR" "EXPR" "&KEY" "KEY" ")" "&BODY" "BODY" "&KEY" "KEY-1" "KEY-2" "KEY-3") 
                (function-arglist->string 'dummy-macro-2))

  (assert-equal '("(" "VAR" "EXPR" "(" "VAR2" "EXPR2" "(" "VAR3" "EXPR3" ")" ")" ")" "(" "VAR4" "EXPR4" ")" "&OPTIONAL" "OPT-1" "&REST" "REST")
                (function-arglist->string 'dummy-macro-3))

  (assert-equal '("A" "B" "C" "&REST" "REST")
                (function-arglist->string 'dummy-function-1))
  
  (assert-equal '("A" "&REST" "REST" "&KEY" "KEY" "KEY-2" "KEY-3")
                (function-arglist->string 'dummy-function-2))
  
  (assert-equal '("A" "&OPTIONAL" "OPT-1" "&REST" "REST")
                (function-arglist->string 'dummy-function-3)))

(define-test test-function-name
  (assert-equal "MAPCAR" (function-name 'mapcar))
  (assert-equal "DOTIMES" (function-name 'dotimes))
  (assert-equal "DUMMY-FUNCTION-1" (function-name 'dummy-function-1))
  (assert-equal "NIL" (function-name nil)))

(define-test test-macro-symbol-p
  (assert-true (macro-symbol-p 'defun))
  (assert-true (macro-symbol-p 'defmacro))
  (assert-true (macro-symbol-p 'dummy-macro-1))
  (assert-true (macro-symbol-p 'dummy-macro-2))
  (assert-true (macro-symbol-p 'dummy-macro-3)))

(define-test test-function-symbol-p
  (assert-true (function-symbol-p 'dummy-function-1))
  (assert-true (function-symbol-p 'dummy-function-2))
  (assert-true (function-symbol-p 'dummy-function-2))
  (assert-true (function-symbol-p 'quote))
  (assert-true (function-symbol-p 'mapcar))
  (assert-true (function-symbol-p 'length)))

(define-test test-function-arglist 
  (let (args)
    (setf args (multiple-value-list (function-arglist 'dummy-macro-1)))
    (assert-eql 'macro (second args))
    (setf args (first args))
    (assert-equalp (first args) '(var expr))
    (assert-eql '&body (second args))
    (assert-eql 'body (third args))

    (setf args (multiple-value-list (function-arglist 'dummy-macro-2)))
    (assert-eql 'macro (second args))
    (setf args (first args))
    (assert-equalp '(var expr &key key) (first args))
    (assert-eql '&body (second args))
    (assert-eql 'body (third args))
    (assert-eql '&key (fourth args))
    (assert-eql 'key-1 (fifth args))
    (assert-equalp '(key-2 0) (sixth args))
    (assert-equalp '(key-3 (+ 1 2 3) supplied-p) (seventh args))

    (setf args (multiple-value-list (function-arglist 'dummy-macro-3)))
    (assert-eql 'macro (second args))
    (setf args (first args))
    (assert-equalp '(var expr (var2 expr2 (var3 expr3))) (first args))
    (assert-equalp '(var4 expr4) (second args))
    (assert-eql '&optional (third args))
    (assert-equalp '(opt-1 0) (fourth args))
    (assert-eql '&rest (fifth args))
    (assert-eql 'rest (sixth args))

    (setf args (multiple-value-list (function-arglist 'dummy-function-1)))
    (assert-eql 'function (second args))
    (setf args (first args))
    (assert-equalp '(a b c &rest rest) args)

    (setf args (multiple-value-list (function-arglist 'dummy-function-2)))
    (assert-eql 'function (second args))
    (setf args (first args))
    (assert-equalp '(a &rest rest &key key (key-2 0) (key-3 (+ 1 2 3) supplied-p)) args)

    (setf args (multiple-value-list (function-arglist 'dummy-function-3)))
    (assert-eql 'function (second args))
    (setf args (first args))
    (assert-equalp '(a &optional (opt-1 0) &rest rest) args)))

;(run-tests)

(define-test test-map-function-name
  (let ((f nil))
    (map-function-name #'(lambda (name) 
                         (setf f (cons name f)))
                     (function-symbols (find-package :de.fh-trier.test.evalserver.meta)))
    (assert-true (member "DUMMY-FUNCTION-1" f :test #'equal))
    (assert-true (member "DUMMY-FUNCTION-2" f :test #'equal))
    (assert-true (member "DUMMY-FUNCTION-3" f :test #'equal))

    (setf f nil)
    (map-function-name #'(lambda (name) 
                         (setf f (cons name f)))
                     (macro-symbols (find-package :de.fh-trier.test.evalserver.meta)))
    (assert-true (member "DUMMY-MACRO-1" f :test #'equal))
    (assert-true (member "DUMMY-MACRO-2" f :test #'equal))
    (assert-true (member "DUMMY-MACRO-3" f :test #'equal))))