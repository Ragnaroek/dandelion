;;; section 9: conditions -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))


(check-for-bug :section9-legacy-7
  (subtypep 'arithmetic-error 'condition)
  t)

(check-for-bug :section9-legacy-11
  (subtypep 'floating-point-overflow 'condition)
  t)

(check-for-bug :section9-legacy-15
  (subtypep 'simple-type-error 'condition)
  t)

(check-for-bug :section9-legacy-19
  (subtypep 'cell-error 'condition)
  t)

(check-for-bug :section9-legacy-23
  (subtypep 'floating-point-underflow 'condition)
  t)

(check-for-bug :section9-legacy-27
  (subtypep 'simple-warning 'condition)
  t)

(check-for-bug :section9-legacy-31
  (subtypep 'condition 'condition)
  t)

(check-for-bug :section9-legacy-35
  (subtypep 'package-error 'condition)
  t)

(check-for-bug :section9-legacy-39
  (subtypep 'storage-condition 'condition)
  t)

(check-for-bug :section9-legacy-43
  (subtypep 'control-error 'condition)
  t)

(check-for-bug :section9-legacy-47
  (subtypep 'parse-error 'condition)
  t)

(check-for-bug :section9-legacy-51
  (subtypep 'stream-error 'condition)
  t)

(check-for-bug :section9-legacy-55
  (subtypep 'division-by-zero 'condition)
  t)

(check-for-bug :section9-legacy-59
  (subtypep 'print-not-readable 'condition)
  t)

(check-for-bug :section9-legacy-63
  (subtypep 'style-warning 'condition)
  t)

(check-for-bug :section9-legacy-67
  (subtypep 'end-of-file 'condition)
  t)

(check-for-bug :section9-legacy-71
  (subtypep 'program-error 'condition)
  t)

(check-for-bug :section9-legacy-75
  (subtypep 'type-error 'condition)
  t)

(check-for-bug :section9-legacy-79
  (subtypep 'error 'condition)
  t)

(check-for-bug :section9-legacy-83
  (subtypep 'reader-error 'condition)
  t)

(check-for-bug :section9-legacy-87
  (subtypep 'unbound-slot 'condition)
  t)

(check-for-bug :section9-legacy-91
  (subtypep 'file-error 'condition)
  t)

(check-for-bug :section9-legacy-95
  (subtypep 'serious-condition 'condition)
  t)

(check-for-bug :section9-legacy-99
  (subtypep 'unbound-variable 'condition)
  t)

(check-for-bug :section9-legacy-103
  (subtypep 'floating-point-inexact 'condition)
  t)

(check-for-bug :section9-legacy-107
  (subtypep 'simple-condition 'condition)
  t)

(check-for-bug :section9-legacy-111
  (subtypep 'undefined-function 'condition)
  t)

(check-for-bug :section9-legacy-115
  (subtypep 'floating-point-invalid-operation 'condition)
  t)

(check-for-bug :section9-legacy-119
  (subtypep 'simple-error 'condition)
  t)

(check-for-bug :section9-legacy-123
  (subtypep 'warning   'condition)
  t)


