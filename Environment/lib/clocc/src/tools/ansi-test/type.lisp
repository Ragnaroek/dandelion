;;; based on v1.13 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :type-legacy-4
  (typep (quote a) (quote symbol))
  t)

(check-for-bug :type-legacy-8
  (typep (quote nil) (quote symbol))
  t)

(check-for-bug :type-legacy-12
  (typep (quote (nil)) (quote symbol))
  nil)

(check-for-bug :type-legacy-16
  (typep 3 (quote integer))
  t)

(check-for-bug :type-legacy-20
  (typep 3 (quote (integer 0 4)))
  t)

(check-for-bug :type-legacy-24
  (typep 3 (quote (integer 0 3)))
  t)

(check-for-bug :type-legacy-28
  (typep 3 (quote (integer 0 2)))
  nil)

(check-for-bug :type-legacy-32
  (typep 3 (quote (float 0.0 2.0)))
  nil)

(check-for-bug :type-legacy-36
  (typep 3 (quote (float 0.0 2.0)))
  nil)

(check-for-bug :type-legacy-40
  (typep 3 (quote (float 0.0 4.0)))
  nil)

(check-for-bug :type-legacy-44
  (typep 3.2 (quote (float 0.0 4.0)))
  t)

(check-for-bug :type-legacy-48
  (typep 3.2 (quote (float 0.0 3.2)))
  t)

(check-for-bug :type-legacy-52
  (typep 3.2 (quote (float 0.0 (3.2))))
  nil)

(check-for-bug :type-legacy-56
  (typep 3.2 (quote (short-float 0.0s0 3.2s0)))
  #+(or allegro cmu sbcl ecl) t
  #-(or allegro cmu sbcl ecl) nil)

(check-for-bug :type-legacy-61
  (typep 3.2 (quote (single-float 0.0f0 3.2f0)))
  t)

(check-for-bug :type-legacy-65
  (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
  nil)

(check-for-bug :type-legacy-69
  (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
  nil)

(check-for-bug :type-legacy-73
  (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
  nil)

(check-for-bug :type-legacy-77
  (typep 3.2s0 (quote (double-float 0.0d0 3.2d0)))
  nil)

(check-for-bug :type-legacy-81
  (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
  nil)

(check-for-bug :type-legacy-85
  (typep 3.2 (quote (float 0.0 3.2)))
  t)

(check-for-bug :type-legacy-89
  (typep 3.2s0 (quote (float 0.0s0 3.2s0)))
  t)

(check-for-bug :type-legacy-93
  (typep 2.0s0 (quote (short-float 0.0s0 3.0s0)))
  t)

(check-for-bug :type-legacy-97
  (typep 2.0s0 (quote (single-float 0.0f0 3.0f0)))
  #+(or allegro cmu sbcl) t
  #-(or allegro cmu sbcl) nil)

(check-for-bug :type-legacy-102
  (typep 2.0 (quote (single-float 0.0f0 3.0f0)))
  t)

(check-for-bug :type-legacy-106
  (typep 2.0d0 (quote (double-float 0.0d0 3.0d0)))
  t)

(check-for-bug :type-legacy-110
  (typep 3.0d0 (quote (double-float 0.0d0 3.0d0)))
  t)

(check-for-bug :type-legacy-114
  (typep 3.0d0 (quote (double-float 0.0d0 (3.0d0))))
  nil)

(check-for-bug :type-legacy-118
  (typep 4 (quote (mod 4)))
  nil)

(check-for-bug :type-legacy-122
  (typep 4 (quote (mod 5)))
  t)

(check-for-bug :type-legacy-126
  (typep 4 (quote (rational 2 5)))
  t)

(check-for-bug :type-legacy-130
  (typep 4 (quote (rational 2 7/2)))
  nil)

(check-for-bug :type-legacy-134
  (typep 4 (quote (rational 2 9/2)))
  t)

(check-for-bug :type-legacy-138
  (typep 4 (quote (rational 2 4)))
  t)

(check-for-bug :type-legacy-142
  (typep 4/3 (quote (rational 2 4)))
  nil)

(check-for-bug :type-legacy-146
  (typep 2 (quote (rational 2 4)))
  t)

(check-for-bug :type-legacy-150
  (typep "abcd" (quote string))
  t)

(check-for-bug :type-legacy-154
  (typep "abcd" (quote (string 4)))
  t)

(check-for-bug :type-legacy-158
  (typep "abcd" (quote (string 43)))
  nil)

(check-for-bug :type-legacy-162
  (typep '#(2 3) (quote (complex integer)))
  nil)

(check-for-bug :type-legacy-166
  (typep '#(2 3) (quote complex))
  nil)

(check-for-bug :type-legacy-170
  (typep #c(2 3) (quote complex))
  t)

(check-for-bug :type-legacy-174
  (typep #c(2 3) (quote (complex integer)))
  t)

(check-for-bug :type-legacy-178
  (typep #c(2 3) (quote (complex float)))
  #.(subtypep (type-of 2) (upgraded-complex-part-type 'float))
  "(complex foo) == (complex (upgraded-complex-part-type foo))")

(check-for-bug :type-legacy-182
  (typep #c(2 3) (quote (complex symbol)))
  error)

(check-for-bug :type-legacy-187
  (typep '#(a b c d) (quote vector))
  t)

(check-for-bug :type-legacy-191
  (typep '#(a b c d) (quote (vector * 4)))
  t)

(check-for-bug :type-legacy-191.1
  (typep #C(0 1) '(complex (eql 0)))
  #.(subtypep (type-of 0)
              (upgraded-complex-part-type '(eql 0))))

#|                                      ;
;;
;; depends on (upgraded-array-element-type 'symbol) !
 (typep '#(a b c d) (quote (vector symbol 4)))
 nil
|#

(check-for-bug :type-legacy-202
 (typep (quote a) (quote (symbol cons)))
 error)

(check-for-bug :type-legacy-206
 (typep (quote a) (quote (or cons symbol)))
 t)

(check-for-bug :type-legacy-210
 (typep (quote a) (quote (or cons number)))
 nil)

(check-for-bug :type-legacy-214
 (typep (quote a) (quote (or atom number)))
 t)

(check-for-bug :type-legacy-218
 (typep (quote a) (quote (and atom number)))
 nil)

(check-for-bug :type-legacy-222
 (typep (quote 2) (quote (and atom number)))
 t)

(check-for-bug :type-legacy-226
 (typep (quote 2) (quote (member 1 2 3)))
 t)

(check-for-bug :type-legacy-230
 (typep (quote 2) (quote (member 1 3)))
 nil)

(check-for-bug :type-legacy-234
 (typep (quote 2) (quote (not (member 1 3))))
 t)

(check-for-bug :type-legacy-238
 (typep (quote 2) (quote (not (member 1 2 3))))
 nil)

(check-for-bug :type-legacy-242
 (typep 2 (quote (and number (not symbol))))
 t)

(check-for-bug :type-legacy-246
 (typep 2 (quote (and string (not symbol))))
 nil)

(check-for-bug :type-legacy-250
 (typep 2 (quote (or string (not symbol))))
 t)

(check-for-bug :type-legacy-254
 (typep (quote cons) (quote function))
 nil)

(check-for-bug :type-legacy-258
 (typep (quote cons) (quote (satisfies functionp)))
 nil)

(check-for-bug :type-legacy-262
 (typep (quote cons) (quote (satisfies not)))
 nil)

(check-for-bug :type-legacy-266
 (typep (quote nil) (quote (satisfies not)))
 t)

(check-for-bug :type-legacy-270
 (typep (quote nil) nil)
 nil)

(check-for-bug :type-legacy-274
 (typep (quote t) nil)
 nil)

(check-for-bug :type-legacy-278
 (subtypep (quote cons) t)
 t)

(check-for-bug :type-legacy-282
 (subtypep nil (quote cons))
 t)

(check-for-bug :type-legacy-286
 (subtypep (quote cons) (quote list))
 t)

(check-for-bug :type-legacy-290
 (subtypep (quote cons) (quote (or atom cons)))
 t)

(check-for-bug :type-legacy-294
 (subtypep (quote cons) (quote (and atom cons)))
 nil)

(check-for-bug :type-legacy-298
 (subtypep (quote cons) (quote (not atom)))
 t
 "Type atom is equivalent to (not cons)")

(check-for-bug :type-legacy-304
 (subtypep (quote list) (quote (not atom)))
 nil)

(check-for-bug :type-legacy-308
 (subtypep (quote (integer 1 5)) (quote (integer 0 7)))
 t)

(check-for-bug :type-legacy-312
 (subtypep (quote (integer 1 5)) (quote (integer 0 (5))))
 nil)

(check-for-bug :type-legacy-316
 (subtypep (quote (integer 1 5)) (quote (integer 0 5)))
 t)

(check-for-bug :type-legacy-320
 (subtypep (quote (integer 1 5)) (quote (mod 5)))
 nil)

(check-for-bug :type-legacy-324
 (subtypep (quote (integer 1 (5))) (quote (mod 5)))
 t)

(check-for-bug :type-legacy-328
 (subtypep '(or (integer 1 (5) float))
	   '(or float (mod 5)))
 #+(or xcl clisp ecls) t
 #+(or allegro cmu sbcl) error
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :type-legacy-335
 (subtypep '(or (integer 1 (5)) float)
	   '(or float (mod 5)))
 t)

(check-for-bug :type-legacy-340
 (subtypep '(and number (float 1.0 (5.0)))
	   '(or float (mod 5)))
 t)

(check-for-bug :type-legacy-345
 (subtypep '(and number (not (float 1.0 (5.0))))
	   '(or float (mod 5)))
 nil)


(check-for-bug :type-legacy-351
 (subtypep '(and float (not (float 1.0 (5.0))))
	   '(or float (mod 5)))
 t
 "a float that is not in [1-5[ is a subtype of float")

(check-for-bug :type-legacy-357
 (subtypep '(and float (not (float 1.0 (5.0))))
	   '(or (float * 1.0) (float * 5.0)))
 nil)

(check-for-bug :type-legacy-362
 (subtypep '(satisfies consp)
	   'list)
 nil)

(check-for-bug :type-legacy-367
 (subtypep (quote simple-string) (quote array))
 t)

(check-for-bug :type-legacy-371
 (deftype mod1 (n) `(and number (float 0.0 (,n))))
 mod1)

(check-for-bug :type-legacy-375
 (typep 4.1 (quote (mod1 5.0)))
 t)

(check-for-bug :type-legacy-379
 (typep 4.1 (quote (mod1 4.1)))
 nil)

(check-for-bug :type-legacy-383
 (subtypep (quote (float 2.3 6.7)) (quote (mod1 6.8)))
 t)

(check-for-bug :type-legacy-387
 (subtypep (quote (float 2.3 6.7)) (quote (mod1 6.7)))
 nil)

(check-for-bug :type-legacy-391
 (defun beliebiger-test (a) (member a (quote (u i v x))))
 beliebiger-test)

(check-for-bug :type-legacy-395
 (not (null (typep (quote u) (quote (satisfies beliebiger-test)))))
 t)

(check-for-bug :type-legacy-399
 (typep (quote a) (quote (satisfies beliebiger-test)))
 nil)

;; This looks like asking a bit _too_ much
;; of the type system [pve]
(check-for-bug :type-legacy-405
 (subtypep (quote (member u i)) (quote (satisfies beliebiger-test)))
 #-(or cmu sbcl) t
 #+(or cmu sbcl) nil)

(check-for-bug :type-legacy-410
 (subtypep (quote (or (member u i))) (quote (satisfies beliebiger-test)))
 #-(or cmu sbcl) t
 #+(or cmu sbcl) nil)


(check-for-bug :type-legacy-416
 (subtypep (quote (or (member u i a))) (quote (satisfies beliebiger-test)))
 nil)

(check-for-bug :type-legacy-420
 (subtypep (quote (satisfies beliebiger-test))
	   (quote (member u i v x y)))
 nil)

(check-for-bug :type-legacy-425
 (deftype beliebiger-typ nil (quote (satisfies beliebiger-test)))
 beliebiger-typ)

(check-for-bug :type-legacy-429
 (not (null (typep (quote u) (quote beliebiger-typ))))
 t)

(check-for-bug :type-legacy-433
 (typep (quote a) (quote beliebiger-typ))
 nil)

(check-for-bug :type-legacy-437
 (subtypep (quote (member u i)) (quote beliebiger-typ))
 #-(or cmu sbcl) t
 #+(or cmu sbcl) nil)


(check-for-bug :type-legacy-443
 (subtypep (quote beliebiger-typ) (quote (member u i v x y)))
 nil)

(check-for-bug :type-legacy-447
 (subtypep nil 'fixnum) t)

(check-for-bug :type-legacy-450
 (subtypep 'short-float 'float ) t)

(check-for-bug :type-legacy-453
 (subtypep 'single-float 'float ) t)

(check-for-bug :type-legacy-456
 (subtypep 'double-float 'float ) t)

(check-for-bug :type-legacy-459
 (subtypep 'long-float 'float ) t)

(check-for-bug :type-legacy-462
 (subtypep 'null 'symbol) t)

(check-for-bug :type-legacy-465
 (subtypep 'null 'list) t)

(check-for-bug :type-legacy-468
 (subtypep 'cons 'list) t)

(check-for-bug :type-legacy-471
 (subtypep 'string 'vector) t)

(check-for-bug :type-legacy-474
 (subtypep 'bit-vector 'vector) t)
(check-for-bug :type-legacy-476
 (subtypep 'vector 'array) t)

(check-for-bug :type-legacy-479
 (subtypep 'simple-array 'array) t)

(check-for-bug :type-legacy-482
 (subtypep 'simple-vector 'simple-array) t)

(check-for-bug :type-legacy-485
 (subtypep 'simple-vector 'vector) t)

(check-for-bug :type-legacy-488
 (subtypep 'simple-string 'simple-array) t)

(check-for-bug :type-legacy-491
 (subtypep 'simple-bit-vector 'simple-array) t)

(check-for-bug :type-legacy-494
 (subtypep 'simple-string 'string) t)

(check-for-bug :type-legacy-497
 (subtypep 'simple-string 'vector) t)

(check-for-bug :type-legacy-500
 (subtypep 'simple-string 'simple-vector) nil)

(check-for-bug :type-legacy-503
 (subtypep 'simple-bit-vector 'bit-vector) t)

(check-for-bug :type-legacy-506
 (subtypep 'bit-vector 'vector) t)

(check-for-bug :type-legacy-509
 (subtypep 'simple-bit-vector 'simple-vector) nil)

(check-for-bug :type-legacy-512
 (subtypep 'unsigned-byte 'integer) t)

(check-for-bug :type-legacy-515
 (subtypep 'signed-byte 'integer) t)

(check-for-bug :type-added-1
  (type-of (coerce (list 1 2 3 4) '(simple-array (unsigned-byte 8) (*))))
  (simple-array (unsigned-byte 8) (4)))

(check-for-bug :type-added-2
  (type-of (coerce (list 1 2 3 4) '(simple-array * (*))))
  (simple-vector 4))

(check-for-bug :type-added-3
  (type-of (coerce (list 1 2 3 4) '(simple-array * (4))))
  (simple-vector 4))

;; these must be recognized correctly (see clhs subtype and figure 4-2)
(check-for-bug :type-added-4
  (multiple-value-list (subtypep 'atom 'cons))
  (nil t))

(check-for-bug :type-added-5
  (multiple-value-list (subtypep 'atom 'list))
  (nil t))

(check-for-bug :type-added-5.1
 (multiple-value-list (subtypep 'cons 'atom))
  (nil t))

(check-for-bug :type-added-5.2
 (multiple-value-list (subtypep 'list 'atom))
  (nil t))

(check-for-bug :type-added-5.3
 (multiple-value-list (subtypep 'stream 'atom))
  (t t))

(check-for-bug :type-added-5.4
 (multiple-value-list (subtypep 'string 'atom))
  (t t))

(check-for-bug :type-added-5.5
 (multiple-value-list (subtypep 'vector 'atom))
  (t t))

(check-for-bug :type-added-6
  (multiple-value-list (subtypep nil nil))
  (t t))

(check-for-bug :type-added-7
  (multiple-value-list (subtypep 'extended-char 'character))
  (t t))
