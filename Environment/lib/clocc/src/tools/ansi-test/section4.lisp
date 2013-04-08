;;; types -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;coerce
(check-for-bug :section4-legacy-7
  (coerce '(a b c) 'vector)
  #(A B C))

(check-for-bug :section4-legacy-11
  (coerce '(a b c) 'list)
  (A B C))

(check-for-bug :section4-legacy-15
  (coerce '(#\A #\B #\C) 'string)
  "ABC")

(check-for-bug :section4-legacy-19
  (coerce #(a b c) 'vector)
  #(A B C))

(check-for-bug :section4-legacy-23
  (coerce #(a b c) 'list)
  (A B C))

(check-for-bug :section4-legacy-27
  (coerce #(#\A #\B #\C) 'string)
  "ABC")

(check-for-bug :section4-legacy-31
  (coerce "ABC" 'vector)
  #(#\A #\B #\C))

(check-for-bug :section4-legacy-35
  (coerce "ABC" 'list)
  (#\A #\B #\C))

(check-for-bug :section4-legacy-39
  (coerce "ABC" 'string)
  "ABC")

(check-for-bug :section4-legacy-43
  (coerce '(a b c) '(vector * 3))
  #(A B C))

(check-for-bug :section4-legacy-47
  (coerce '(a b c) 'list)
  (A B C))

(check-for-bug :section4-legacy-51
  (coerce '(#\A #\B #\C) '(string 3))
  "ABC")

(check-for-bug :section4-legacy-55
  (coerce #(a b c) '(vector * 3))
  #(A B C))

(check-for-bug :section4-legacy-59
  (coerce #(a b c) 'list)
  (A B C))

(check-for-bug :section4-legacy-63
  (coerce #(#\A #\B #\C) '(string 3))
  "ABC")

(check-for-bug :section4-legacy-67
  (coerce "ABC" '(vector * 3))
  #(#\A #\B #\C))

(check-for-bug :section4-legacy-71
  (coerce "ABC" 'list)
  (#\A #\B #\C))

(check-for-bug :section4-legacy-75
  (coerce "ABC" '(string 3))
  "ABC")

(check-for-bug :section4-legacy-79
  (coerce 'a 'character)
  #\A)

(check-for-bug :section4-legacy-83
  (coerce 4.56 'complex)
  #C(4.56 0.0))

(check-for-bug :section4-legacy-87
  (coerce 4.5s0 'complex)
  #C(4.5s0 0.0s0))

(check-for-bug :section4-legacy-91
  (coerce 7/2 'complex)
  7/2)

(check-for-bug :section4-legacy-95
  (coerce 0 'short-float)
  0.0s0)

(check-for-bug :section4-legacy-99
  (coerce 3.5L0 'float)
  3.5L0)

(check-for-bug :section4-legacy-103
  (coerce 7/2 'float)
  3.5)

(check-for-bug :section4-legacy-107
  (coerce (cons 1 2) t)
  (1 . 2))

(check-for-bug :section4-legacy-111
  (coerce '(a b c) '(vector * 4))
  type-error)

(check-for-bug :section4-legacy-115
  (coerce #(a b c) '(vector * 4))
  type-error)

(check-for-bug :section4-legacy-119
  (coerce '(a b c) '(vector * 2))
  type-error)

(check-for-bug :section4-legacy-123
  (coerce #(a b c) '(vector * 2))
  type-error)

(check-for-bug :section4-legacy-127
  (coerce "foo" '(string 2))
  type-error)

(check-for-bug :section4-legacy-131
  (coerce #(#\a #\b #\c) '(string 2))
  type-error)

(check-for-bug :section4-legacy-135
  (coerce '(0 1) '(simple-bit-vector 3))
  type-error)

;; subtypep
(check-for-bug :section4-legacy-140
  (multiple-value-bind (a b)
      (subtypep 'compiled-function 'function)
    (list a b))
  (T T)
  "Type COMPILED-FUNCTION

Supertypes:

compiled-function, function, t
...
")

(check-for-bug :section4-legacy-153
  (multiple-value-bind (a b)
      (subtypep 'null 'list)
    (list a b))
  (T T))

(check-for-bug :section4-legacy-159
  (multiple-value-bind (a b)
      (subtypep 'null 'symbol)
    (list a b))
  (T T))

(check-for-bug :section4-legacy-165
  (multiple-value-bind (a b)
      (subtypep 'integer 'string)
    (list a b))
  (nil #-clisp T
       #+clisp nil))

(check-for-bug :section4-legacy-172
  (multiple-value-bind (a b)
      (subtypep '(satisfies dummy) nil)
    (list a b))
  (nil t))

(check-for-bug :section4-legacy-179
  (multiple-value-bind (a b)
      (subtypep '(integer 1 3) '(integer 1 4))
    (list a b))
  (T T))

(check-for-bug :section4-legacy-185
  (multiple-value-bind (a b)
      (subtypep '(member) 'nil)
    (list a b))
  (T T))                                ;   true, true ;or false, false

(check-for-bug :section4-legacy-191
  (multiple-value-bind (a b)
      (subtypep 'nil '(member))
    (list a b))
  (T T))                                ; true, true ;or false, false

;;; type-of

(check-for-bug :section4-legacy-199
  (type-of 'a)
  SYMBOL          )

(check-for-bug :section4-legacy-203
  (type-of '(1 . 2))
  CONS)
					; OR=>  (CONS FIXNUM FIXNUM)

(check-for-bug :section4-legacy-208
  (type-of #c(0 1))
  #-cmu COMPLEX
  #+cmu (COMPLEX BIT))
					; OR=>  (COMPLEX INTEGER)

(check-for-bug :section4-legacy-214
  (defstruct temp-struct x y z)
  TEMP-STRUCT)

(check-for-bug :section4-legacy-218
  (type-of (make-temp-struct))
  TEMP-STRUCT)

(check-for-bug :section4-legacy-222
  (type-of "abc")
  #+(or cmu sbcl clisp)
  (SIMPLE-BASE-STRING 3)
  #-(or cmu sbcl clisp)
  STRING)
					; OR=>  (STRING 3)

(check-for-bug :section4-legacy-230
  (multiple-value-bind (a b)
      (subtypep (type-of "abc") 'string)
    (list a b))
  (T T))

(check-for-bug :section4-legacy-236
  (type-of (expt 2 40))
  BIGNUM)
					; OR=>  INTEGER
					; OR=>  (INTEGER 1099511627776 1099511627776)
					; OR=>  SYSTEM::TWO-WORD-BIGNUM
					; OR=>  FIXNUM

(check-for-bug :section4-legacy-244
  (multiple-value-bind (a b)
      (subtypep (type-of 112312) 'integer)
    (list a b))
  (T T))

(check-for-bug :section4-legacy-250
  (defvar *foo* (make-array 5 :element-type t))
  *FOO*)

(check-for-bug :section4-legacy-254
  (class-name (class-of *foo*))
  #+(or cmu sbcl) SIMPLE-VECTOR
  #-(or cmu sbcl) VECTOR)

(check-for-bug :section4-legacy-259
  (type-of *foo*)
  #+(or cmu sbcl clisp)
  (SIMPLE-VECTOR 5)
  #-(or cmu sbcl clisp)
  VECTOR)
					; OR=>  (VECTOR T 5)

;;; typep

(check-for-bug :section4-legacy-269
  (typep 12 'integer)
  T)

(check-for-bug :section4-legacy-273
  (typep (1+ most-positive-fixnum) 'fixnum)
  nil)

(check-for-bug :section4-legacy-277
  (typep nil t)
  t)

(check-for-bug :section4-legacy-281
  (typep nil nil)
  nil)

(check-for-bug :section4-legacy-285
  (typep 1 '(mod 2))
  t )

(check-for-bug :section4-legacy-289
  (typep #c(1 1) '(complex (eql 1)))
  t )

;; To understand this next example, you might need to refer to
;; Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals).
(check-for-bug :section4-legacy-295
  (typep #c(0 0) '(complex (eql 0)))
  nil
  "(upgraded-complex-part-type '(eql 0)) -> RATIONAL
a subtype of REAL. So it should work.

12.1.5.3:
also #C(5 0) is eql to 5
     #C(5.0 0.0) is not eql to 5.0
CMUCL bombs here because of the eql. We give two
replacement tests below:
")

(check-for-bug :section4-legacy-308
  (typep #c(1 1) 'complex)
  T
  "Because #C(1 1) remains an complex")

(check-for-bug :section4-legacy-313
  (typep #c(3/2 0) 'complex)
  NIL
  "Because #C(3/2 0) is eql to 3/2")

(check-for-bug :section4-legacy-318
  (typep #c(1 0) 'complex)
  NIL
  "Because #c(0 0) is eql to 0")

(check-for-bug :section4-legacy-323
  (typep #c(0.0 0.0) 'complex)
  T
  "Because #c(0.0 0.0) remains a complex")

;;; type-error-datum
;;(check-for-bug :section4-legacy-329
;;(progn
;;  (defun fix-digits (condition)
;;    (check-type condition type-error)
;;    (let* ((digits '(zero one two three four
;;			  five six seven eight nine))
;;	   (val (position (type-error-datum condition) digits)))
;;      (if (and val (subtypep 'number (type-error-expected-type condition)))
;;	  (store-value 7))))

;;  (defun foo (x)
;;    (handler-bind ((type-error #'fix-digits))
;;		  (check-type x number)
;;		  (+ x 3)))
;;  t)
;;t)

;;(check-for-bug :section4-legacy-346
;;(foo 'seven)
;;10)







