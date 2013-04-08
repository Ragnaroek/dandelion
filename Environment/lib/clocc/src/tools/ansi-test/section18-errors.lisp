;;; section 18 hash tables -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(check-for-bug :section18-errors-legacy-6
  (defvar *counters* (make-hash-table))
  *COUNTERS*)

(check-for-bug :section18-errors-legacy-10
  (multiple-value-bind (a b)
      (gethash 'foo *counters*)
    (list a b))
  (NIL nil))

(check-for-bug :section18-errors-legacy-16
  (multiple-value-bind (a b)
      (gethash 'foo *counters* 0)
    (list a b))
  (0 nil))

(check-for-bug :section18-errors-legacy-22 ; XXX
  (defmacro how-many (obj) `(values (gethash ,obj *counters* 0)))
  HOW-MANY)

(check-for-bug :section18-errors-legacy-26 ; XXX
  (defun count-it (obj) (incf (how-many obj)))
  COUNT-IT)

(dolist (x '(bar foo foo bar bar baz)) (count-it x))

(check-for-bug :section18-errors-legacy-32
  (how-many 'foo)
  2)

(check-for-bug :section18-errors-legacy-36
  (how-many 'bar)
  3)

(check-for-bug :section18-errors-legacy-40
  (how-many 'quux)
  0)

