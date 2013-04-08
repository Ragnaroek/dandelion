;;; Based on 1.2 -*- mode: lisp -*-
;;; tests for backquote
(in-package :cl-user)

(check-for-bug :backquot-legacy-5
  (setf x (list 'a 'b 'c))
  (a b c))

(check-for-bug :backquot-legacy-9
  `(x ,x ,@x foo ,(cadr  x) bar ,(cdr x) baz ,@(cdr x) ,. x)
  (X (A B C) A B C FOO B BAR (B C) BAZ B C A B C))

(check-for-bug :backquot-legacy-13
  (read-from-string "`,@x")
  ERROR)

(check-for-bug :backquot-legacy-17
  `(,x . ,x)				; = (append (list x) x)
  ((a b c) a b c))


(check-for-bug :backquot-legacy-22
  (read-from-string "`(,x . ,@x)")
  ERROR)


(check-for-bug :backquot-legacy-27
  (read-from-string ",x")
  ERROR)

(check-for-bug :backquot-legacy-31
  `#(1 2 3 4)
  #(1 2 3 4))

(check-for-bug :backquot-expansion-into-vector
  (let ((x (list 'a 'b 'c)))
    `#(,@x))
  #(a b c))
