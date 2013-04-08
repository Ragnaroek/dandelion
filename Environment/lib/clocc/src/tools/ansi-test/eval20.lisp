;; based on v1.5 -*- mode: lisp -*-
(in-package :cl-user)

;; CLtL2 20; ANSI CL 3.1

;;  eval

(check-for-bug :eval20-legacy-9
  (eval (list 'cdr
              '(car (list (cons 'a 'b) 'c))))
  b)

(check-for-bug :eval20-legacy-14
  (makunbound 'x)
  x)

(check-for-bug :eval20-legacy-18
  (eval 'x)
  UNBOUND-VARIABLE)

(check-for-bug :eval20-legacy-22
  (setf x 3)
  3)

(check-for-bug :eval20-legacy-26
  (eval 'x)
  3)

;; eval-when
(check-for-bug :eval20-eval-when
  (let ((ff "eval-when-test.lisp"))
    (with-open-file (foo ff :direction :output)
      (format foo "~%(eval-when (compile eval)
  ;; note that LAMBDA is not externalizable
  (defvar *junk* #.(lambda (x) (+ 15 x))))~%"))
    (delete-file (compile-file ff))
    (delete-file ff)
    nil)
  nil)

;; constantp

(check-for-bug :eval20-legacy-32
  (constantp 2)
  T)

(check-for-bug :eval20-legacy-36
  (constantp #\r)
  T)

(check-for-bug :eval20-legacy-40
  (constantp "max")
  T)

(check-for-bug :eval20-legacy-44
  (constantp '#(110))
  T)

(check-for-bug :eval20-legacy-48
  (constantp :max)
  T)

(check-for-bug :eval20-legacy-52
  (constantp T)
  T)

(check-for-bug :eval20-legacy-56
  (constantp NIL)
  T)

(check-for-bug :eval20-legacy-60
  (constantp 'PI)
  #-CLISP T
  #+CLISP NIL)

(check-for-bug :eval20-legacy-65
  (constantp '(quote foo))
  T)

