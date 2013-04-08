;;; 12: numbers -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; 12.1.4.1.1

;;;; Combining rationals with floats.
;;; This example assumes an implementation in which
;;; (float-radix 0.5) is 2 (as in IEEE) or 16 (as in IBM/360),
;;; or else some other implementation in which 1/2 has an exact
;;;  representation in floating point.
(check-for-bug :section12-legacy-13
  (+ 1/2 0.5)
  1.0)


(check-for-bug :section12-legacy-18
  (- 1/2 0.5d0)
  0.0d0)


(check-for-bug :section12-legacy-23
  (+ 0.5 -0.5 1/2)
  0.5)

;;;; Comparing rationals with floats.
;;; This example assumes an implementation in which the default float
;;; format is IEEE single-float, IEEE double-float, or some other format
;;; in which 5/7 is rounded upwards by FLOAT.

(check-for-bug :section12-legacy-32
  (< 5/7 (float 5/7))
  t)


(check-for-bug :section12-legacy-37
  (< 5/7 (rational (float 5/7)))
  t)


(check-for-bug :section12-legacy-42
  (< (float 5/7) (float 5/7))
  nil)

;;; 12.1.5.3.1


(check-for-bug :section12-legacy-49
  #c(1.0 1.0)
  #C(1.0 1.0))

(check-for-bug :section12-legacy-53
  #c(0.0 0.0)
  #C(0.0 0.0))

(check-for-bug :section12-legacy-57
  #c(1.0 1)
  #C(1.0 1.0))

(check-for-bug :section12-legacy-61
  #c(0.0 0)
  #C(0.0 0.0))

(check-for-bug :section12-legacy-65
  #c(1 1)
  #C(1 1))

(check-for-bug :section12-legacy-69
  #c(0 0)
  0)

(check-for-bug :section12-legacy-73
  (typep #c(1 1) '(complex (eql 1)))
  t)


(check-for-bug :section12-legacy-78
  (typep #c(0 0) '(complex (eql 0)))
  nil)

;;; number


(check-for-bug :section12-legacy-85
  (subtypep 'real  'number)
  t)


(check-for-bug :section12-legacy-90
  (subtypep 'complex 'number)
  t)


(check-for-bug :section12-legacy-95
  (subtypep 'rational 'real)
  t)


(check-for-bug :section12-legacy-100
  (subtypep 'float 'real)
  t)


;;; float


(check-for-bug :section12-legacy-108
  (subtypep 'short-float 'float)
  t)


(check-for-bug :section12-legacy-113
  (subtypep 'single-float 'float)
  t)


(check-for-bug :section12-legacy-118
  (subtypep 'double-float 'float)
  t)


(check-for-bug :section12-legacy-123
  (subtypep 'long-float  'float)
  t)

;;; rational


(check-for-bug :section12-legacy-130
  (subtypep 'integer 'rational)
  t)


(check-for-bug :section12-legacy-135
  (subtypep 'ratio 'rational)
  t)

;;; integer


(check-for-bug :section12-legacy-142
  (subtypep 'fixnum 'integer)
  t)


(check-for-bug :section12-legacy-147
  (subtypep 'bignum 'integer)
  t)
;;; fixnum

(check-for-bug :section12-legacy-152
  (subtypep '(signed-byte 16) 'fixnum)
  t)
;;; = /= < > <= >=

(check-for-bug :section12-legacy-157
  (= 3 3)
  t)

(check-for-bug :section12-legacy-161
  (/= 3 3)
  nil)

(check-for-bug :section12-legacy-165
  (= 3 5)
  nil)

(check-for-bug :section12-legacy-169
  (/= 3 5)
  t)

(check-for-bug :section12-legacy-173
  (= 3 3 3 3)
  t)

(check-for-bug :section12-legacy-177
  (/= 3 3 3 3)
  nil)

(check-for-bug :section12-legacy-181
  (= 3 3 5 3)
  nil)

(check-for-bug :section12-legacy-185
  (/= 3 3 5 3)
  nil)

(check-for-bug :section12-legacy-189
  (= 3 6 5 2)
  nil)

(check-for-bug :section12-legacy-193
  (/= 3 6 5 2)
  t)

(check-for-bug :section12-legacy-197
  (= 3 2 3)
  nil)

(check-for-bug :section12-legacy-201
  (/= 3 2 3)
  nil)

(check-for-bug :section12-legacy-205
  (< 3 5)
  t)

(check-for-bug :section12-legacy-209
  (<= 3 5)
  t)

(check-for-bug :section12-legacy-213
  (< 3 -5)
  nil)

(check-for-bug :section12-legacy-217
  (<= 3 -5)
  nil)

(check-for-bug :section12-legacy-221
  (< 3 3)
  nil)

(check-for-bug :section12-legacy-225
  (<= 3 3)
  t)

(check-for-bug :section12-legacy-229
  (< 0 3 4 6 7)
  t)

(check-for-bug :section12-legacy-233
  (<= 0 3 4 6 7)
  t)

(check-for-bug :section12-legacy-237
  (< 0 3 4 4 6)
  nil)

(check-for-bug :section12-legacy-241
  (<= 0 3 4 4 6)
  t)

(check-for-bug :section12-legacy-245
  (> 4 3)
  t)

(check-for-bug :section12-legacy-249
  (>= 4 3)
  t)

(check-for-bug :section12-legacy-253
  (> 4 3 2 1 0)
  t)

(check-for-bug :section12-legacy-257
  (>= 4 3 2 1 0)
  t)

(check-for-bug :section12-legacy-261
  (> 4 3 3 2 0)
  nil)

(check-for-bug :section12-legacy-265
  (>= 4 3 3 2 0)
  t)

(check-for-bug :section12-legacy-269
  (> 4 3 1 2 0)
  nil)

(check-for-bug :section12-legacy-273
  (>= 4 3 1 2 0)
  nil)

(check-for-bug :section12-legacy-277
  (= 3)
  t)

(check-for-bug :section12-legacy-281
  (/= 3)
  t)

(check-for-bug :section12-legacy-285
  (< 3)
  t)

(check-for-bug :section12-legacy-289
  (<= 3)
  t)

(check-for-bug :section12-legacy-293
  (= 3.0 #c(3.0 0.0))
  t)

(check-for-bug :section12-legacy-297
  (/= 3.0 #c(3.0 1.0))
  t)

(check-for-bug :section12-legacy-301
  (= 3 3.0)
  t)

(check-for-bug :section12-legacy-305
  (= 3.0s0 3.0d0)
  t)

(check-for-bug :section12-legacy-309
  (= 0.0 -0.0)
  t)

(check-for-bug :section12-legacy-313
  (= 5/2 2.5)
  t)
(check-for-bug :section12-legacy-316
  (> 0.0 -0.0)
  nil)
(check-for-bug :section12-legacy-319
  (= 0 -0.0)
  t)


;;; min max


(check-for-bug :section12-legacy-327
  (max 3)
  3 )


(check-for-bug :section12-legacy-332
  (min 3)
  3)


(check-for-bug :section12-legacy-337
  (max 6 12)
  12 )


(check-for-bug :section12-legacy-342
  (min 6 12)
  6)


(check-for-bug :section12-legacy-347
  (max -6 -12)
  -6 )


(check-for-bug :section12-legacy-352
  (min -6 -12)
  -12)


(check-for-bug :section12-legacy-357
  (max 1 3 2 -7)
  3 )


(check-for-bug :section12-legacy-362
  (min 1 3 2 -7)
  -7)


(check-for-bug :section12-legacy-367
  (max -2 3 0 7)
  7 )


(check-for-bug :section12-legacy-372
  (min -2 3 0 7)
  -2)


(check-for-bug :section12-legacy-377
  (max 5.0 2)
  5.0 )


(check-for-bug :section12-legacy-382
  (min 5.0 2)
  #+(or cmu sbcl clisp ecls) 2
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  2 OR  2.0


(check-for-bug :section12-legacy-389
  (max 3.0 7 1)
  #+(or cmu sbcl clisp ecls) 7
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  7 OR  7.0


(check-for-bug :section12-legacy-396
  (min 3.0 7 1)
  #+(or cmu sbcl clisp ecls) 1
  #-(or cmu sbcl clisp ecls) fill-this-in)
					; 1 OR  1.0


(check-for-bug :section12-legacy-403
  (max 1.0s0 7.0d0)
  7.0d0)


(check-for-bug :section12-legacy-408
  (min 1.0s0 7.0d0)
  #+(or cmu sbcl ecls) 1.0		;hmm in fact an error?
  #+clisp 1.0s0
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  1.0s0 OR  1.0d0


(check-for-bug :section12-legacy-416
  (max 3 1 1.0s0 1.0d0)
  #+(or cmu sbcl clisp ecls) 3
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  3 OR 3.0d0


(check-for-bug :section12-legacy-423
  (min 3 1 1.0s0 1.0d0)
  #+(or cmu sbcl clisp ecls) 1
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  1 OR  1.0s0  OR  1.0d0

;;; plusp minusp


(check-for-bug :section12-legacy-432
  (minusp -1)
  t)


(check-for-bug :section12-legacy-437
  (plusp 0)
  nil)


(check-for-bug :section12-legacy-442
  (plusp least-positive-single-float)
  t)


(check-for-bug :section12-legacy-447
  (plusp least-positive-double-float)
  t)


(check-for-bug :section12-legacy-452
  (minusp least-positive-single-float)
  nil)


(check-for-bug :section12-legacy-457
  (minusp least-positive-double-float)
  nil)


(check-for-bug :section12-legacy-462
  (plusp least-negative-single-float)
  nil)


(check-for-bug :section12-legacy-467
  (plusp least-negative-double-float)
  nil)


(check-for-bug :section12-legacy-472
  (minusp least-negative-single-float)
  t)


(check-for-bug :section12-legacy-477
  (minusp least-negative-double-float)
  t)


(check-for-bug :section12-legacy-482
  (minusp 0)
  nil)


(check-for-bug :section12-legacy-487
  (minusp -0.0)
  nil)


(check-for-bug :section12-legacy-492
  (minusp +0.0)
  nil)


(check-for-bug :section12-legacy-497
  (plusp 0)
  nil)


(check-for-bug :section12-legacy-502
  (plusp -0.0)
  nil)


(check-for-bug :section12-legacy-507
  (plusp +0.0)
  nil)


;;; zerop


(check-for-bug :section12-legacy-515
  (zerop 0)
  t)


(check-for-bug :section12-legacy-520
  (zerop 0.0)
  t)


(check-for-bug :section12-legacy-525
  (zerop +0.0)
  t)


(check-for-bug :section12-legacy-530
  (zerop -0.0)
  t)


(check-for-bug :section12-legacy-535
  (zerop -1)
  nil)


(check-for-bug :section12-legacy-540
  (zerop 1)
  nil)


(check-for-bug :section12-legacy-545
  (zerop 0/100)
  t)


(check-for-bug :section12-legacy-550
  (zerop #c(0 0.0))
  t)

;;; random-state-p


(check-for-bug :section12-legacy-557
  (random-state-p *random-state*)
  t)


(check-for-bug :section12-legacy-562
  (random-state-p (make-random-state))
  t)


(check-for-bug :section12-legacy-567
  (random-state-p 'test-function)
  nil)

;;; number-p


(check-for-bug :section12-legacy-574
  (numberp 12)
  t)


(check-for-bug :section12-legacy-579
  (numberp (expt 2 130))
  t)


(check-for-bug :section12-legacy-584
  (numberp #c(5/3 7.2))
  t)


(check-for-bug :section12-legacy-589
  (numberp nil)
  nil)


(check-for-bug :section12-legacy-594
  (numberp (cons 1 2))
  nil)

;;; most-positive-fixnum



(check-for-bug :section12-legacy-602
  (>= most-positive-fixnum (- (expt 2 15) 1))
  t)


(check-for-bug :section12-legacy-607
  (>= most-positive-fixnum array-dimension-limit)
  t
  "
most-positive-fixnum is that fixnum closest in value
to positive infinity provided by the implementation, and
greater than or equal to both 2^15 - 1 and array-dimension-limit.
")


(check-for-bug :section12-legacy-617
  (<= most-negative-fixnum (- (expt 2 15)))
  t)

;;; most-positive bla bla


(check-for-bug :section12-legacy-624
  (plusp MOST-POSITIVE-SHORT-FLOAT)
  t)


(check-for-bug :section12-legacy-629
  (plusp  LEAST-POSITIVE-SHORT-FLOAT)
  t)


(check-for-bug :section12-legacy-634
  (plusp  LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT)
  t)


(check-for-bug :section12-legacy-639
  (plusp  MOST-POSITIVE-DOUBLE-FLOAT)
  t)


(check-for-bug :section12-legacy-644
  (plusp LEAST-POSITIVE-DOUBLE-FLOAT)
  t)


(check-for-bug :section12-legacy-649
  (plusp  LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT)
  t)


(check-for-bug :section12-legacy-654
  (plusp MOST-POSITIVE-LONG-FLOAT)
  t)


(check-for-bug :section12-legacy-659
  (plusp LEAST-POSITIVE-LONG-FLOAT)
  t)


(check-for-bug :section12-legacy-664
  (plusp LEAST-POSITIVE-NORMALIZED-LONG-FLOAT)
  t)


(check-for-bug :section12-legacy-669
  (plusp  MOST-POSITIVE-SINGLE-FLOAT)
  t)


(check-for-bug :section12-legacy-674
  (plusp LEAST-POSITIVE-SINGLE-FLOAT)
  t)


(check-for-bug :section12-legacy-679
  (plusp  LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT)
  t)


(check-for-bug :section12-legacy-684
  (minusp MOST-NEGATIVE-SHORT-FLOAT)
  t)


(check-for-bug :section12-legacy-689
  (minusp  LEAST-NEGATIVE-SHORT-FLOAT)
  t)


(check-for-bug :section12-legacy-694
  (minusp LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT)
  t)


(check-for-bug :section12-legacy-699
  (minusp  MOST-NEGATIVE-SINGLE-FLOAT)
  t)


(check-for-bug :section12-legacy-704
  (minusp LEAST-NEGATIVE-SINGLE-FLOAT)
  t)


(check-for-bug :section12-legacy-709
  (minusp  LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT)
  t)


(check-for-bug :section12-legacy-714
  (minusp MOST-NEGATIVE-DOUBLE-FLOAT)
  t)


(check-for-bug :section12-legacy-719
  (minusp  LEAST-NEGATIVE-DOUBLE-FLOAT)
  t)


(check-for-bug :section12-legacy-724
  (minusp LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT)
  t)


(check-for-bug :section12-legacy-729
  (minusp  MOST-NEGATIVE-LONG-FLOAT)
  t)


(check-for-bug :section12-legacy-734
  (minusp LEAST-NEGATIVE-LONG-FLOAT)
  t)


(check-for-bug :section12-legacy-739
  (minusp  LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT)
  t)

;;; epsilons



(check-for-bug :section12-legacy-747
  (not (= (float 1 short-float-epsilon)
          (+ (float 1 short-float-epsilon) short-float-epsilon)))
  t
  "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated:

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(check-for-bug :section12-legacy-761
  (not (= (float 1 single-float-epsilon)
          (+ (float 1 single-float-epsilon) single-float-epsilon)))
  t
  "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated:

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(check-for-bug :section12-legacy-775
  (not (= (float 1 double-float-epsilon)
          (+ (float 1 double-float-epsilon) double-float-epsilon)))
  t
  "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated:

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(check-for-bug :section12-legacy-789
  (not (= (float 1 long-float-epsilon )
          (+ (float 1 long-float-epsilon ) long-float-epsilon )))
  t
  "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated:

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(check-for-bug :section12-legacy-803
  (not (= (float 1 short-float-negative-epsilon)
          (- (float 1 short-float-negative-epsilon)
             short-float-negative-epsilon)))
  t
  "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated:

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")


(check-for-bug :section12-legacy-818
  (not (= (float 1 single-float-negative-epsilon)
          (- (float 1 single-float-negative-epsilon)
             single-float-negative-epsilon)))
  t
  "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated:

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")


(check-for-bug :section12-legacy-833
  (not (= (float 1 double-float-negative-epsilon)
          (- (float 1 double-float-negative-epsilon)
             double-float-negative-epsilon)))
  t
  "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated:

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")



(check-for-bug :section12-legacy-849
  (not (= (float 1 long-float-negative-epsilon)
          (- (float 1 long-float-negative-epsilon)
             long-float-negative-epsilon)))
  t
  "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated:

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")
