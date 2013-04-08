;;; section 5 -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; 5.1.1.1.1

(check-for-bug :section5-legacy-8
  (let ((ref2 (list '())))
    (push (progn (princ "1") 'ref1)
          (car (progn (princ "2") ref2))))
  (REF1))

#+nil
(check-for-bug :section5-legacy-15
  (let (x)
    (push (setq x (list 'a))
          (car (setq x (list 'b))))
    x)
  (((A) . B)))				;possible bug in hyperspec


;;; apply
(check-for-bug :section5-legacy-24
  (setq f '+)
  +)

(check-for-bug :section5-legacy-28
  (apply f '(1 2))
  3)

(check-for-bug :section5-legacy-32
  (progn
    (setq f #'-)
    t)
  t)

(check-for-bug :section5-legacy-38
  (apply f '(1 2))
  -1)

(check-for-bug :section5-legacy-42
  (apply #'max 3 5 '(2 7 3))
  7)

(check-for-bug :section5-legacy-46
  (apply 'cons '((+ 2 3) 4))
  ((+ 2 3) . 4))

(check-for-bug :section5-legacy-50
  (apply #'+ '())
  0)

(check-for-bug :section5-legacy-54
  (defparameter *some-list* '(a b c))
  *SOME-LIST*)

(check-for-bug :section5-legacy-58
  (defun strange-test (&rest x) (eq x *some-list*))
  STRANGE-TEST)

(check-for-bug :section5-legacy-62
  (apply #'strange-test *some-list*)
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in )

;;(check-for-bug :section5-legacy-67
;;(defun foo (size &rest keys &key double &allow-other-keys)
;;   (let ((v (apply #'make-array size :allow-other-keys t keys)))
;;     (if double (concatenate (type-of v) v v) v)))
;;FOO)

;;(check-for-bug :section5-legacy-73
;;(foo 4 :initial-contents '(a b c d) :double t)
;;#(A B C D A B C D))

;;; defun

(check-for-bug :section5-legacy-79
  (defun recur (x)
    (when (> x 0)
      (recur (1- x))))
  RECUR )

(check-for-bug :section5-legacy-85
  (defun ex (a b &optional c (d 66) &rest keys &key test (start 0))
    (list a b c d keys test start))
  EX )

(check-for-bug :section5-legacy-90
  (ex 1 2)
  (1 2 NIL 66 NIL NIL 0))

(check-for-bug :section5-legacy-94
  (ex 1 2 3 4 :test 'equal :start 50)
  (1 2 3 4 (:TEST EQUAL :START 50) EQUAL 50))

(check-for-bug :section5-legacy-98
  (ex :test 1 :start 2)
  (:TEST 1 :START 2 NIL NIL 0))

;; This function assumes its callers have checked the types of the
;; arguments, and authorizes the compiler to build in that assumption.
(check-for-bug :section5-legacy-104
  (defun discriminant (a b c)
    (declare (number a b c))
    "Compute the discriminant for a quadratic equation."
    (- (* b b) (* 4 a c)))
  DISCRIMINANT)

(check-for-bug :section5-legacy-111
  (discriminant 1 2/3 -2)
  76/9)

;; This function assumes its callers have not checked the types of the
;; arguments, and performs explicit type checks before making any assumptions.

(check-for-bug :section5-legacy-118
  (defun careful-discriminant (a b c)
    "Compute the discriminant for a quadratic equation."
    (check-type a number)
    (check-type b number)
    (check-type c number)
    (locally (declare (number a b c))
      (- (* b b) (* 4 a c))))
  CAREFUL-DISCRIMINANT)

(check-for-bug :section5-legacy-128
  (careful-discriminant 1 2/3 -2)
  76/9)

;;; fboundp

(check-for-bug :section5-legacy-134
  (fboundp 'car)
  t)

(check-for-bug :section5-legacy-138
  (fboundp 'nth-value)
  #+(or cmu sbcl clisp) t
  #-(or cmu sbcl clisp) nil)

(check-for-bug :section5-legacy-143
  (fboundp 'with-open-file)
  t)

(check-for-bug :section5-legacy-147
  (fboundp 'unwind-protect)
  t)

(check-for-bug :section5-legacy-151
  (defun my-function (x) x)
  MY-FUNCTION)

(check-for-bug :section5-legacy-155
  (fboundp 'my-function)
  t)

(check-for-bug :section5-legacy-159
  (let ((saved-definition (symbol-function 'my-function)))
    (unwind-protect (progn (fmakunbound 'my-function)
                           (fboundp 'my-function))
      (setf (symbol-function 'my-function) saved-definition)))
  nil)

(check-for-bug :section5-legacy-166
  (fboundp 'my-function)
  t)

(check-for-bug :section5-legacy-170
  (defmacro my-macro (x) `',x)
  MY-MACRO)

(check-for-bug :section5-legacy-174
  (fboundp 'my-macro)
  t)

(check-for-bug :section5-legacy-178
  (fmakunbound 'my-function)
  MY-FUNCTION)

(check-for-bug :section5-legacy-182
  (fboundp 'my-function)
  nil)

(check-for-bug :section5-legacy-186
  (flet ((my-function (x) x))
    (fboundp 'my-function))
  nil)

;;; fmakunbound

(check-for-bug :section5-legacy-193
  (defun add-some (x) (+ x 19))
  ADD-SOME)

(check-for-bug :section5-legacy-197
  (fboundp 'add-some)
  t)

(check-for-bug :section5-legacy-201
  (flet ((add-some (x) (+ x 37)))
    (fmakunbound 'add-some)
    (add-some 1))
  38)

(check-for-bug :section5-legacy-207
  (fboundp 'add-some)
  nil)

;;; macroletjes

(check-for-bug :section5-legacy-213
  (flet ((flet1 (n) (+ n n)))
    (flet ((flet1 (n) (+ 2 (flet1 n))))
      (flet1 2)))
  6)

(check-for-bug :section5-legacy-219
  (defun dummy-function () 'top-level)
  DUMMY-FUNCTION )

(check-for-bug :section5-legacy-223
  (funcall #'dummy-function)
  TOP-LEVEL )

(check-for-bug :section5-legacy-227
  (flet ((dummy-function () 'shadow))
    (funcall #'dummy-function))
  SHADOW )

(check-for-bug :section5-legacy-232
  (eq (funcall #'dummy-function) (funcall 'dummy-function))
  t )

(check-for-bug :section5-legacy-236
  (flet ((dummy-function () 'shadow))
    (eq (funcall #'dummy-function)
        (funcall 'dummy-function)))
  nil)

(check-for-bug :section5-legacy-242
  (defun recursive-times (k n)
    (labels ((temp (n)
               (if (zerop n) 0 (+ k (temp (1- n))))))
      (temp n)))
  RECURSIVE-TIMES)

(check-for-bug :section5-legacy-249
  (recursive-times 2 3)
  6)

(check-for-bug :section5-legacy-253
  (defmacro mlets (x &environment env)
    (let ((form `(babbit ,x)))
      (macroexpand form env)))
  MLETS)

(check-for-bug :section5-legacy-259
  (macrolet ((babbit (z) `(+ ,z ,z))) (mlets 5))
  10)

(check-for-bug :section5-legacy-263
  (flet ((safesqrt (x) (sqrt (abs x))))
    ;; The safesqrt function is used in two places.
    (safesqrt (apply #'+ (map 'list #'safesqrt '(1 2 3 4 5 6)))))
  3.2911735)

(check-for-bug :section5-legacy-269
  (defun integer-power (n k)
    (declare (integer n))
    (declare (type (integer 0 *) k))
    (labels ((expt0 (x k a)
               (declare (integer x a) (type (integer 0 *) k))
               (cond ((zerop k) a)
                     ((evenp k) (expt1 (* x x) (floor k 2) a))
                     (t (expt0 (* x x) (floor k 2) (* x a)))))
             (expt1 (x k a)
               (declare (integer x a) (type (integer 0 *) k))
               (cond ((evenp k) (expt1 (* x x) (floor k 2) a))
                     (t (expt0 (* x x) (floor k 2) (* x a))))))
      (expt0 n k 1)))
  INTEGER-POWER)

(check-for-bug :section5-legacy-285
  (defun example (y l)
    (flet ((attach (x)
             (setq l (append l (list x)))))
      (declare (inline attach))
      (dolist (x y)
        (unless (null (cdr x))
          (attach x)))
      l))
  EXAMPLE)

(check-for-bug :section5-legacy-296
  (example '((a apple apricot) (b banana) (c cherry) (d) (e))
           '((1) (2) (3) (4 2) (5) (6 3 2)))
  ((1) (2) (3) (4 2) (5) (6 3 2) (A APPLE APRICOT) (B BANANA) (C CHERRY)))


;;; funcall

(check-for-bug :section5-legacy-304
  (funcall #'+ 1 2 3)
  6)

(check-for-bug :section5-legacy-308
  (funcall 'car '(1 2 3))
  1)

(check-for-bug :section5-legacy-312
  (funcall 'position 1 '(1 2 3 2 1) :start 1)
  4)

(check-for-bug :section5-legacy-316
  (cons 1 2)
  (1 . 2))

(check-for-bug :section5-legacy-320
  (flet ((cons (x y) `(kons ,x ,y)))
    (let ((cons (symbol-function '+)))
      (funcall #'cons
               (funcall 'cons 1 2)
               (funcall cons 1 2))))
  (KONS (1 . 2) 3))

;;; functionp

(check-for-bug :section5-legacy-330
  (functionp 'append)
  nil)

(check-for-bug :section5-legacy-334
  (functionp #'append)
  t)

(check-for-bug :section5-legacy-338
  (functionp (symbol-function 'append))
  t)

(check-for-bug :section5-legacy-342
  (flet ((f () 1)) (functionp #'f))
  t)

(check-for-bug :section5-legacy-346
  (functionp (compile nil '(lambda () 259)))
  t)

(check-for-bug :section5-legacy-350
  (functionp nil)
  nil)

(check-for-bug :section5-legacy-354
  (functionp 12)
  nil)

(check-for-bug :section5-legacy-358
  (functionp '(lambda (x) (* x x)))
  nil)

(check-for-bug :section5-legacy-362
  (functionp #'(lambda (x) (* x x)))
  t)

;;; compiled-function-p


(check-for-bug :section5-legacy-369
  (defun f (x) x)
  F)

(check-for-bug :section5-legacy-373
  (compiled-function-p #'f)
  #+(or cmu sbcl ecls) t
  #+clisp nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
;;  false OR true

(check-for-bug :section5-legacy-380
  (compiled-function-p 'f)
  nil)

(check-for-bug :section5-legacy-384
  (compile 'f)
  F)

(check-for-bug :section5-legacy-388
  (compiled-function-p #'f)
  t)

(check-for-bug :section5-legacy-392
  (compiled-function-p 'f)
  nil)

(check-for-bug :section5-legacy-396
  (compiled-function-p (compile nil '(lambda (x) x)))
  t)

(check-for-bug :section5-legacy-400
  (compiled-function-p #'(lambda (x) x))
  #+(or cmu sbcl ecls) t
  #+clisp nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  false OR true

(check-for-bug :section5-legacy-407
  (compiled-function-p '(lambda (x) x))
  nil)

;;; CALL-ARGUMENTS-LIMIT

(check-for-bug :section5-legacy-413
  (>= CALL-ARGUMENTS-LIMIT  50)
  t)

;;; LAMBDA-LIST-KEYWORDS

(check-for-bug :section5-legacy-419
  (not (member '&ALLOW-OTHER-KEYS LAMBDA-LIST-KEYWORDS))
  nil)

(check-for-bug :section5-legacy-423
  (not (member '&AUX LAMBDA-LIST-KEYWORDS))
  nil)

(check-for-bug :section5-legacy-427
  (not (member '&ENVIRONMENT LAMBDA-LIST-KEYWORDS))
  nil)

(check-for-bug :section5-legacy-431
  (not (member '&OPTIONAL LAMBDA-LIST-KEYWORDS))
  nil)

(check-for-bug :section5-legacy-435
  (not (member '&REST LAMBDA-LIST-KEYWORDS))
  nil)

(check-for-bug :section5-legacy-439
  (not (member '&WHOLE LAMBDA-LIST-KEYWORDS))
  nil)

;;; LAMBDA-PARAMETERS-LIMIT

(check-for-bug :section5-legacy-445
  (>= LAMBDA-PARAMETERS-LIMIT 50)
  t)

;;; defconstant

(check-for-bug :section5-legacy-451
  (defconstant this-is-a-constant 'never-changing "for a test")
  THIS-IS-A-CONSTANT)

(check-for-bug :section5-legacy-455
  this-is-a-constant
  NEVER-CHANGING)

(check-for-bug :section5-legacy-459
  (documentation 'this-is-a-constant 'variable)
  "for a test")

(check-for-bug :section5-legacy-463
  (constantp 'this-is-a-constant)
  t)

;;; defparameter

(check-for-bug :section5-legacy-469
  (defparameter *p* 1)
  *P*)

(check-for-bug :section5-legacy-473
  *p*
  1)

(check-for-bug :section5-legacy-477
  (constantp '*p*)
  nil)

(check-for-bug :section5-legacy-481
  (setq *p* 2)
  2)

(check-for-bug :section5-legacy-485
  (defparameter *p* 3)
  *P*)

(check-for-bug :section5-legacy-489
  *p*
  3)

(unintern '*V*)

(check-for-bug :section5-legacy-495
  (defvar *v* 1)
  *V*)

(check-for-bug :section5-legacy-499
  *v*
  1)

(check-for-bug :section5-legacy-503
  (constantp '*v*)
  nil)

(check-for-bug :section5-legacy-507
  (setq *v* 2)
  2)

(check-for-bug :section5-legacy-511
  (defvar *v* 3)
  *V*)

(check-for-bug :section5-legacy-515
  *v*
  2)

(check-for-bug :section5-legacy-519
  (defun foo ()
    (let ((*p* 'p) (*v* 'v))
      (bar)))
  FOO)

(check-for-bug :section5-legacy-525
  (defun bar () (list *p* *v*))
  BAR)

(check-for-bug :section5-legacy-529
  (foo)
  (P V))

;;; destructuring-bind

(check-for-bug :section5-legacy-535
  (defun iota (n) (loop for i from 1 to n collect i)) ;helper
  IOTA)

(check-for-bug :section5-legacy-539
  (destructuring-bind ((a &optional (b 'bee)) one two three)
      `((alpha) ,@(iota 3))
    (list a b three two one))
  (ALPHA BEE 3 2 1))

;;; let & let*

					;(let ((a 'top))

					;  (check-for-bug :section5-legacy-549
					;   (defun dummy-function () a)
					;   DUMMY-FUNCTION)

					;  (check-for-bug :section5-legacy-553
					;   (let ((a 'inside) (b a))
					;     (format nil "~S ~S ~S" a b (dummy-function)))
					;   "INSIDE TOP TOP" )

					;  (check-for-bug :section5-legacy-558
					;   (let* ((a 'inside) (b a))
					;     (format nil "~S ~S ~S" a b (dummy-function)))
					;   "INSIDE INSIDE TOP" ))

					;(setf a 'top)

					;(check-for-bug :section5-legacy-565
					; (let ((a 'inside) (b a))
					;   (declare (special a))
					;   (format nil "~S ~S ~S" a b (dummy-function)))
					; "INSIDE TOP INSIDE")

;;; progv

(check-for-bug :section5-legacy-573
  (let ((*x* 3))
    (progv '(*x*) '(4)
      (list *x* (symbol-value '*x*))))
  (3 4))

(check-for-bug :section5-legacy-579
  (setq *x* 1)
  1)

(check-for-bug :section5-legacy-583
  (progv '(*x*) '(2) *x*)
  2)

(check-for-bug :section5-legacy-587
  *x*
  1)


;;; setq
(check-for-bug :section5-legacy-593
  (setq a 1 b 2 c 3)
  3)

(check-for-bug :section5-legacy-597
  a
  1)

(check-for-bug :section5-legacy-601
  b
  2)

(check-for-bug :section5-legacy-605
  c
  3)

;; Use of SETQ to update values by sequential assignment.
(check-for-bug :section5-legacy-610
  (setq a (1+ b) b (1+ a) c (+ a b))
  7)

(check-for-bug :section5-legacy-614
  a
  3)

(check-for-bug :section5-legacy-618
  b
  4)

(check-for-bug :section5-legacy-622
  c
  7)

;; This illustrates the use of SETQ on a symbol macro.
(check-for-bug :section5-legacy-627
  (let ((x (list 10 20 30)))
    (symbol-macrolet ((y (car x)) (z (cadr x)))
        (setq y (1+ z) z (1+ y))
      (list x y z)))
  ((21 22 30) 21 22))

;;; psetq
(check-for-bug :section5-legacy-635
  (psetq a 1 b 2 c 3)
  NIL)

(check-for-bug :section5-legacy-639
  a
  1)

(check-for-bug :section5-legacy-643
  b
  2)

(check-for-bug :section5-legacy-647
  c
  3)

;; Use of PSETQ to update values by parallel assignment.
;; The effect here is very different than if SETQ had been used.
(check-for-bug :section5-legacy-653
  (psetq a (1+ b) b (1+ a) c (+ a b))
  NIL)

(check-for-bug :section5-legacy-657
  a
  3)

(check-for-bug :section5-legacy-661
  b
  2)

(check-for-bug :section5-legacy-665
  c
  3)

;; Use of PSETQ on a symbol macro.
(check-for-bug :section5-legacy-670
  (let ((x (list 10 20 30)))
    (symbol-macrolet ((y (car x)) (z (cadr x)))
        (psetq y (1+ z) z (1+ y))
      (list x y z)))
  ((21 11 30) 21 11))

;; Use of parallel assignment to swap values of A and B.
(check-for-bug :section5-legacy-678
  (multiple-value-bind (n h)
      (let ((a 1) (b 2))
        (psetq a b  b a)
        (values a b))
    (list n h))
  (2 1))


;;; block

(check-for-bug :section5-legacy-689
  (block empty)
  NIL)

(check-for-bug :section5-legacy-693
  (multiple-value-bind (n h)
      (block whocares (values 1 2) (values 3 4))
    (list n h))
  (3 4))

(check-for-bug :section5-legacy-699
  (let ((x 1))
    (block stop (setq x 2) (return-from stop) (setq x 3))
    x)
  2)

(check-for-bug :section5-legacy-705
  (multiple-value-bind (n h)
      (block early
        (return-from early (values 1 2))
        (values 3 4))
    (list n h))
  (1  2))

(check-for-bug :section5-legacy-713
  (block outer (block inner (return-from outer 1)) 2)
  1)

(check-for-bug :section5-legacy-717
  (block twin (block twin (return-from twin 1)) 2)
  2)

;; Contrast behavior of this example with corresponding example of CATCH.
(check-for-bug :section5-legacy-722
  (block b
    (flet ((b1 () (return-from b 1)))
      (block b (b1) (print 'unreachable))
      2))
  1)

;; catch

(check-for-bug :section5-legacy-731
  (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4)
  3)

(check-for-bug :section5-legacy-735
  (catch 'dummy-tag 1 2 3 4)
  4)

(check-for-bug :section5-legacy-739
  (defun throw-back (tag) (throw tag t))
  THROW-BACK)

(check-for-bug :section5-legacy-743
  (catch 'dummy-tag (throw-back 'dummy-tag) 2)
  T)

;; Contrast behavior of this example with corresponding example of BLOCK.
(check-for-bug :section5-legacy-748
  (catch 'c
    (flet ((c1 () (throw 'c 1)))
      (catch 'c (c1) (print 'unreachable))
      2))
  2)

;;; go
(check-for-bug :section5-legacy-756
  (tagbody
     (setq val 2)
     (go lp)
     (incf val 3)
   lp (incf val 4))
  NIL)

(check-for-bug :section5-legacy-764
  val
  6 )

;;; return-from
(check-for-bug :section5-legacy-769
  (block alpha (return-from alpha) 1)
  NIL)

(check-for-bug :section5-legacy-773
  (block alpha (return-from alpha 1) 2)
  1)

(check-for-bug :section5-legacy-777
  (multiple-value-bind (n h)
      (block alpha (return-from alpha (values 1 2)) 3)
    (list n h))
  (1  2))

(check-for-bug :section5-legacy-783
  (let ((a 0))
    (dotimes (i 10) (incf a) (when (oddp i) (return)))
    a)
  2)

(check-for-bug :section5-legacy-789
  (defun temp (x)
    (if x (return-from temp 'dummy))
    44)
  TEMP)

(check-for-bug :section5-legacy-795
  (temp nil)
  44)

(check-for-bug :section5-legacy-799
  (temp t)
  DUMMY)

(check-for-bug :section5-legacy-803
  (block out
    (flet ((exit (n) (return-from out n)))
      (block out (exit 1)))
    2)
  1)

(check-for-bug :section5-legacy-810
  (block nil
    (unwind-protect (return-from nil 1)
      (return-from nil 2)))
  2)

;;; return

(check-for-bug :section5-legacy-818
  (block nil (return) 1)
  NIL)

(check-for-bug :section5-legacy-822
  (block nil (return 1) 2)
  1)

(check-for-bug :section5-legacy-826
  (multiple-value-bind (n h)
      (block nil (return (values 1 2)) 3)
    (list n h))
  (1 2))

(check-for-bug :section5-legacy-832
  (block nil (block alpha (return 1) 2))
  1)

(check-for-bug :section5-legacy-836
  (block alpha (block nil (return 1)) 2)
  2)

(check-for-bug :section5-legacy-840
  (block nil (block nil (return 1) 2))
  1)

;;; tagbody

(check-for-bug :section5-legacy-846
  (let (val)
    (tagbody
       (setq val 1)
       (go point-a)
       (incf val 16)
     point-c
       (incf val 04)
       (go point-b)
       (incf val 32)
     point-a
       (incf val 02)
       (go point-c)
       (incf val 64)
     point-b
       (incf val 08))
    val)
  15)

(check-for-bug :section5-legacy-865
  (defun f1 (flag)
    (let ((n 1))
      (tagbody
         (setq n (f2 flag #'(lambda () (go out))))
       out
         (prin1 n))))
  F1)

(check-for-bug :section5-legacy-874
  (defun f2 (flag escape)
    (if flag (funcall escape) 2))
  F2)

(check-for-bug :section5-legacy-879
  (f1 nil)
  NIL)

(check-for-bug :section5-legacy-883
  (f1 t)
  NIL)

;;; trow

(check-for-bug :section5-legacy-889
  (multiple-value-bind (n h)
      (catch 'result
        (setq i 0 j 0)
        (loop (incf j 3) (incf i)
          (if (= i 3) (throw 'result (values i j)))))
    (list n h))
  (3 9))


(check-for-bug :section5-legacy-899
  (catch nil
    (unwind-protect (throw nil 1)
      (throw nil 2)))
  2)

;;; unwind-protect

(check-for-bug :section5-legacy-907
  (defun dummy-function (x)
    (setq state 'running)
    (unless (numberp x) (throw 'abort 'not-a-number))
    (setq state (1+ x)))
  DUMMY-FUNCTION)

(check-for-bug :section5-legacy-914
  (catch 'abort (dummy-function 1))
  2)

(check-for-bug :section5-legacy-918
  state
  2)

(check-for-bug :section5-legacy-922
  (catch 'abort (dummy-function 'trash))
  NOT-A-NUMBER)

(check-for-bug :section5-legacy-926
  state
  RUNNING)

(check-for-bug :section5-legacy-930
  (catch 'abort (unwind-protect (dummy-function 'trash)
                  (setq state 'aborted)))
  NOT-A-NUMBER)

(check-for-bug :section5-legacy-935
  state
  ABORTED)

;;; not

(check-for-bug :section5-legacy-941
  (not nil)
  T)

(check-for-bug :section5-legacy-945
  (not '())
  T)

(check-for-bug :section5-legacy-949
  (not (integerp 'sss))
  T)

(check-for-bug :section5-legacy-953
  (not (integerp 1))
  NIL)

(check-for-bug :section5-legacy-957
  (not 3.7)
  NIL)

(check-for-bug :section5-legacy-961
  (not 'apple)
  NIL)

;;; eq

(check-for-bug :section5-legacy-967
  (eq 'a 'b)
  nil)

(check-for-bug :section5-legacy-971
  (eq 'a 'a)
  t)

(check-for-bug :section5-legacy-975
  (eq 3 3)
  #+(or cmu sbcl clisp ecls) t
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR false

(check-for-bug :section5-legacy-981
  (eq 3 3.0)
  nil)

(check-for-bug :section5-legacy-985
  (eq 3.0 3.0)
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(check-for-bug :section5-legacy-991
  (eq #c(3 -4) #c(3 -4))
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR  false

(check-for-bug :section5-legacy-997
  (eq #c(3 -4.0) #c(3 -4))
  nil)

(check-for-bug :section5-legacy-1001
  (eq (cons 'a 'b) (cons 'a 'c))
  nil)

(check-for-bug :section5-legacy-1005
  (eq (cons 'a 'b) (cons 'a 'b))
  nil)

(check-for-bug :section5-legacy-1009
  (eq '(a . b) '(a . b))
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(check-for-bug :section5-legacy-1015
  (progn (setq x (cons 'a 'b)) (eq x x))
  T)

(check-for-bug :section5-legacy-1019
  (progn (setq x '(a . b)) (eq x x))
  T)

(check-for-bug :section5-legacy-1023
  (eq #\A #\A)
  #+(or cmu sbcl clisp ecls) t
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(check-for-bug :section5-legacy-1029
  (let ((x "Foo")) (eq x x))
  T)

(check-for-bug :section5-legacy-1033
  (eq "Foo" "Foo")
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(check-for-bug :section5-legacy-1039
  (eq "Foo" (copy-seq "Foo"))
  nil)

(check-for-bug :section5-legacy-1043
  (eq "FOO" "foo")
  nil)

(check-for-bug :section5-legacy-1047
  (eq "string-seq" (copy-seq "string-seq"))
  nil)

(check-for-bug :section5-legacy-1051
  (let ((x 5)) (eq x x))
  #+(or cmu sbcl clisp ecls) t
  #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR false

;;; eql

(check-for-bug :section5-legacy-1059
  (eql 'a 'b)
  nil)

(check-for-bug :section5-legacy-1063
  (eql 'a 'a)
  t)

(check-for-bug :section5-legacy-1067
  (eql 3 3)
  t)

(check-for-bug :section5-legacy-1071
  (eql 3 3.0)
  nil)

(check-for-bug :section5-legacy-1075
  (eql 3.0 3.0)
  t)

(check-for-bug :section5-legacy-1079
  (eql #c(3 -4) #c(3 -4))
  t)

(check-for-bug :section5-legacy-1083
  (eql #c(3 -4.0) #c(3 -4))
  nil)

(check-for-bug :section5-legacy-1087
  (eql (cons 'a 'b) (cons 'a 'c))
  nil)

(check-for-bug :section5-legacy-1091
  (eql (cons 'a 'b) (cons 'a 'b))
  nil)

(check-for-bug :section5-legacy-1095
  (eql '(a . b) '(a . b))
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR  false

(check-for-bug :section5-legacy-1101
  (progn (setq x (cons 'a 'b)) (eql x x))
  t)

(check-for-bug :section5-legacy-1105
  (progn (setq x '(a . b)) (eql x x))
  t)

(check-for-bug :section5-legacy-1109
  (eql #\A #\A)
  t)

(check-for-bug :section5-legacy-1113
  (eql "Foo" "Foo")
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR  false

(check-for-bug :section5-legacy-1119
  (eql "Foo" (copy-seq "Foo"))
  nil)

(check-for-bug :section5-legacy-1123
  (eql "FOO" "foo")
  nil)

;;; equal

(check-for-bug :section5-legacy-1129
  (equal 'a 'b)
  nil)

(check-for-bug :section5-legacy-1133
  (equal 'a 'a)
  T)

(check-for-bug :section5-legacy-1137
  (equal 3 3)
  t)

(check-for-bug :section5-legacy-1141
  (equal 3 3.0)
  nil)

(check-for-bug :section5-legacy-1145
  (equal 3.0 3.0)
  t)

(check-for-bug :section5-legacy-1149
  (equal #c(3 -4) #c(3 -4))
  t)

(check-for-bug :section5-legacy-1153
  (equal #c(3 -4.0) #c(3 -4))
  nil)

(check-for-bug :section5-legacy-1157
  (equal (cons 'a 'b) (cons 'a 'c))
  nil)

(check-for-bug :section5-legacy-1161
  (equal (cons 'a 'b) (cons 'a 'b))
  t)

(check-for-bug :section5-legacy-1165
  (equal #\A #\A)
  t)

(check-for-bug :section5-legacy-1169
  (equal #\A #\a)
  nil)

(check-for-bug :section5-legacy-1173
  (equal "Foo" "Foo")
  t)

(check-for-bug :section5-legacy-1177
  (equal "Foo" (copy-seq "Foo"))
  t)

(check-for-bug :section5-legacy-1181
  (equal "FOO" "foo")
  nil)

(check-for-bug :section5-legacy-1185
  (equal "This-string" "This-string")
  t)

(check-for-bug :section5-legacy-1189
  (equal "This-string" "this-string")
  nil)

;;; equalp

(check-for-bug :section5-legacy-1195
  (equalp 'a 'b)
  nil)

(check-for-bug :section5-legacy-1199
  (equalp 'a 'a)
  t)

(check-for-bug :section5-legacy-1203
  (equalp 3 3)
  t)

(check-for-bug :section5-legacy-1207
  (equalp 3 3.0)
  t)

(check-for-bug :section5-legacy-1211
  (equalp 3.0 3.0)
  t)

(check-for-bug :section5-legacy-1215
  (equalp #c(3 -4) #c(3 -4))
  t)

(check-for-bug :section5-legacy-1219
  (equalp #c(3 -4.0) #c(3 -4))
  t)

(check-for-bug :section5-legacy-1223
  (equalp (cons 'a 'b) (cons 'a 'c))
  nil)

(check-for-bug :section5-legacy-1227
  (equalp (cons 'a 'b) (cons 'a 'b))
  t)

(check-for-bug :section5-legacy-1231
  (equalp #\A #\A)
  t)

(check-for-bug :section5-legacy-1235
  (equalp #\A #\a)
  t)

(check-for-bug :section5-legacy-1239
  (equalp "Foo" "Foo")
  t)

(check-for-bug :section5-legacy-1243
  (equalp "Foo" (copy-seq "Foo"))
  t)

(check-for-bug :section5-legacy-1247
  (equalp "FOO" "foo")
  t)

(check-for-bug :section5-legacy-1251
  (setq array1 (make-array 6 :element-type 'integer
                           :initial-contents '(1 1 1 3 5 7)))
  #(1 1 1 3 5 7))

(check-for-bug :section5-legacy-1256
  (setq array2 (make-array 8 :element-type 'integer
                           :initial-contents '(1 1 1 3 5 7 2 6)
                           :fill-pointer 6))
  #(1 1 1 3 5 7))

(check-for-bug :section5-legacy-1262
  (equalp array1 array2)
  t)

(check-for-bug :section5-legacy-1266
  (setq vector1 (vector 1 1 1 3 5 7))
  #(1 1 1 3 5 7))

(check-for-bug :section5-legacy-1270
  (equalp array1 vector1)
  t )

;; hashtables etc?

;;; identity

(check-for-bug :section5-legacy-1278
  (identity 101)
  101)

(check-for-bug :section5-legacy-1282
  (mapcan #'identity (list (list 1 2 3) '(4 5 6)))
  (1 2 3 4 5 6))

;;; complement

(check-for-bug :section5-legacy-1288
  (funcall (complement #'zerop) 1)
  t)

(check-for-bug :section5-legacy-1292
  (funcall (complement #'characterp) #\A)
  nil)

(check-for-bug :section5-legacy-1296
  (funcall (complement #'member) 'a '(a b c))
  nil)

(check-for-bug :section5-legacy-1300
  (funcall (complement #'member) 'd '(a b c))
  t)


;;; constantly

(check-for-bug :section5-legacy-1307
  (mapcar (constantly 3) '(a b c d))
  (3 3 3 3))

(check-for-bug :section5-legacy-1311
  (defmacro with-vars (vars &body forms)
    `((lambda ,vars ,@forms) ,@(mapcar (constantly nil) vars)))
  WITH-VARS)

(check-for-bug :section5-legacy-1316
  (multiple-value-bind (n h)
      (macroexpand '(with-vars (a b)
                     (setq a 3 b (* a a))
                     (list a b)))
    (list n h))
  (((LAMBDA (A B) (SETQ A 3 B (* A A)) (LIST A B)) NIL NIL) t))

;;; every en co

(check-for-bug :section5-legacy-1326
  (every #'characterp "abc")
  t)

(check-for-bug :section5-legacy-1330
  (some #'= '(1 2 3 4 5) '(5 4 3 2 1))
  t)

(check-for-bug :section5-legacy-1334
  (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
  nil)

(check-for-bug :section5-legacy-1338
  (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
  t)

;;; and

(check-for-bug :section5-legacy-1344
  (setq temp1 1 temp2 1 temp3 1)
  1 )

(check-for-bug :section5-legacy-1348
  (and (incf temp1) (incf temp2) (incf temp3))
  2 )

(check-for-bug :section5-legacy-1352
  (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3))
  t)

(check-for-bug :section5-legacy-1356
  (decf temp3)
  1 )

(check-for-bug :section5-legacy-1360
  (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3))
  NIL )

(check-for-bug :section5-legacy-1364
  (and (eql temp1 temp2) (eql temp2 temp3))
  t)

(check-for-bug :section5-legacy-1368
  (and)
  T )

;;; cond

(check-for-bug :section5-legacy-1374
  (defun select-options ()
    (cond ((= a 1) (setq a 2))
          ((= a 2) (setq a 3))
          ((and (= a 3) (floor a 2)))
          (t (floor a 3))))
  SELECT-OPTIONS)

(check-for-bug :section5-legacy-1382
  (setq a 1)
  1)

(check-for-bug :section5-legacy-1386
  (select-options)
  2)

(check-for-bug :section5-legacy-1390
  a
  2)

(check-for-bug :section5-legacy-1394
  (select-options)
  3)

(check-for-bug :section5-legacy-1398
  a
  3)

(check-for-bug :section5-legacy-1402
  (select-options)
  1)

(check-for-bug :section5-legacy-1406
  (setq a 5)
  5)

(check-for-bug :section5-legacy-1410
  (multiple-value-bind (n h)
      (select-options)
    (list n h))
  (1 2))

;;; or

(check-for-bug :section5-legacy-1418
  (or)
  NIL )

(check-for-bug :section5-legacy-1422
  (setq temp0 nil temp1 10 temp2 20 temp3 30)
  30)

(check-for-bug :section5-legacy-1426
  (or temp0 temp1 (setq temp2 37))
  10)

(check-for-bug :section5-legacy-1430
  temp2
  20)

(check-for-bug :section5-legacy-1434
  (or (incf temp1) (incf temp2) (incf temp3))
  11)

(check-for-bug :section5-legacy-1438
  temp1
  11)

(check-for-bug :section5-legacy-1442
  temp2
  20)

(check-for-bug :section5-legacy-1446
  temp3
  30)

(check-for-bug :section5-legacy-1450
  (or (values) temp1)
  11)

(check-for-bug :section5-legacy-1454
  (or (values temp1 temp2) temp3)
  11)

(check-for-bug :section5-legacy-1458
  (multiple-value-bind (n h)
      (or temp0 (values temp1 temp2))
    (list n h))
  (11 20))

(check-for-bug :section5-legacy-1464
  (multiple-value-bind (n h)
      (or (values temp0 temp1) (values temp2 temp3))
    (list n h))
  (20 30))

;;; when

(check-for-bug :section5-legacy-1472
  (when t 'hello)
  HELLO)

(check-for-bug :section5-legacy-1476
  (unless t 'hello)
  NIL)

(check-for-bug :section5-legacy-1480
  (when nil 'hello)
  NIL)

(check-for-bug :section5-legacy-1484
  (unless nil 'hello)
  HELLO)

(check-for-bug :section5-legacy-1488
  (when t)
  NIL)

(check-for-bug :section5-legacy-1492
  (unless nil)
  NIL)

(check-for-bug :section5-legacy-1496
  (when t (prin1 1) (prin1 2) (prin1 3))
  3)

(check-for-bug :section5-legacy-1500
  (unless t (prin1 1) (prin1 2) (prin1 3))
  NIL)

(check-for-bug :section5-legacy-1504
  (when nil (prin1 1) (prin1 2) (prin1 3))
  NIL)

(check-for-bug :section5-legacy-1508
  (unless nil (prin1 1) (prin1 2) (prin1 3))
  3)

(check-for-bug :section5-legacy-1512
  (let ((x 3))
    (list (when (oddp x) (incf x) (list x))
          (when (oddp x) (incf x) (list x))
          (unless (oddp x) (incf x) (list x))
          (unless (oddp x) (incf x) (list x))
          (if (oddp x) (incf x) (list x))
          (if (oddp x) (incf x) (list x))
          (if (not (oddp x)) (incf x) (list x))
          (if (not (oddp x)) (incf x) (list x))))
  ((4) NIL (5) NIL 6 (6) 7 (7)))

;;; multiple-value-bind

(check-for-bug :section5-legacy-1526
  (multiple-value-bind (f r)
      (floor 130 11)
    (list f r))
  (11 9))

;;; multiple-value-call

(check-for-bug :section5-legacy-1534
  (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
  (1 / 2 3 / / 2 0.5))

(check-for-bug :section5-legacy-1538
  (+ (floor 5 3) (floor 19 4))
  5)

(check-for-bug :section5-legacy-1542
  (multiple-value-call #'+ (floor 5 3) (floor 19 4))
  10)

;;; multiple-value-list

(check-for-bug :section5-legacy-1548
  (multiple-value-list (floor -3 4))
  (-1 1))

;;; multiple-value-prog1

(check-for-bug :section5-legacy-1554
  (setq temp '(1 2 3))
  (1 2 3))

(check-for-bug :section5-legacy-1558
  (multiple-value-bind (n h j)
      (multiple-value-prog1
          (values-list temp)
        (setq temp nil)
        (values-list temp))
    (list n h j))
  (1 2 3))

;;; multiple-value-setq

(check-for-bug :section5-legacy-1569
  (multiple-value-setq (quotient remainder) (truncate 3.2 2))
  1)

(check-for-bug :section5-legacy-1573
  quotient
  1)

(check-for-bug :section5-legacy-1577
  remainder
  1.2)

(check-for-bug :section5-legacy-1581
  (multiple-value-setq (a b c) (values 1 2))
  1)

(check-for-bug :section5-legacy-1585
  a
  1)

(check-for-bug :section5-legacy-1589
  b
  2)

(check-for-bug :section5-legacy-1593
  c
  NIL)

(check-for-bug :section5-legacy-1597
  (multiple-value-setq (a b) (values 4 5 6))
  4)

(check-for-bug :section5-legacy-1601
  a
  4)

(check-for-bug :section5-legacy-1605
  b
  5)

;;; values

(check-for-bug :section5-legacy-1611
  (values 1)
  1)

(check-for-bug :section5-legacy-1615
  (multiple-value-bind (n h)
      (values 1 2)
    (list n h))
  (1 2))

(check-for-bug :section5-legacy-1621
  (multiple-value-bind (n h j)
      (values 1 2 3)
    (list n h j))
  (1 2 3))

(check-for-bug :section5-legacy-1627
  (multiple-value-bind (n h j)
      (values (values 1 2 3) 4 5)
    (list n h j))
  (1 4 5))

(check-for-bug :section5-legacy-1633
  (defun polar (x y)
    (values (sqrt (+ (* x x) (* y y))) (atan y x)))
  POLAR)

(check-for-bug :section5-legacy-1638
  (multiple-value-bind (r theta) (polar 3.0 4.0)
    (vector r theta))
  #(5.0 0.9272952))

;;; values-list

(check-for-bug :section5-legacy-1645
  (values-list '(1))
  1)

(check-for-bug :section5-legacy-1649
  (multiple-value-bind (n h)
      (values-list '(1 2))
    (list n h))
  (1 2))

(check-for-bug :section5-legacy-1655
  (multiple-value-bind (n h j)
      (values-list '(1 2 3))
    (list n h j))
  (1 2 3))

;;; multiple-values-limit

(check-for-bug :section5-legacy-1663
  (>= MULTIPLE-VALUES-LIMIT 20)
  T)


;;; nth-value

(check-for-bug :section5-legacy-1670
  (nth-value 0 (values 'a 'b))
  A)

(check-for-bug :section5-legacy-1674
  (nth-value 1 (values 'a 'b))
  B)

(check-for-bug :section5-legacy-1678
  (nth-value 2 (values 'a 'b))
  NIL)

(check-for-bug :section5-legacy-1682
  (multiple-value-bind (n h j)
      (let* ((x 83927472397238947423879243432432432)
             (y 32423489732)
             (a (nth-value 1 (floor x y)))
             (b (mod x y)))
        (values a b (= a b)))
    (list n h j))
  (3332987528 3332987528 t))

;;; prog

(check-for-bug :section5-legacy-1694
  (setq a 1)
  1)

(check-for-bug :section5-legacy-1698
  (prog ((a 2) (b a)) (return (if (= a b) '= '/=)))
  /=)

(check-for-bug :section5-legacy-1702
  (prog* ((a 2) (b a)) (return (if (= a b) '= '/=)))
  =)

(check-for-bug :section5-legacy-1706
  (prog () 'no-return-value)
  NIL)

;;; prog1

(check-for-bug :section5-legacy-1712
  (setq temp 1)
  1)

(check-for-bug :section5-legacy-1716
  (prog1 temp (print temp) (incf temp) (print temp))
  1)

(check-for-bug :section5-legacy-1720
  (prog1 temp (setq temp nil))
  2)

(check-for-bug :section5-legacy-1724
  temp
  NIL)

(check-for-bug :section5-legacy-1728
  (prog1 (values 1 2 3) 4)
  1 )

(check-for-bug :section5-legacy-1732
  (setq temp (list 'a 'b 'c))
  (A B C))

(check-for-bug :section5-legacy-1736
  (prog1 (car temp) (setf (car temp) 'alpha))
  A)

(check-for-bug :section5-legacy-1740
  temp
  (ALPHA B C))

(check-for-bug :section5-legacy-1744
  (multiple-value-bind (n h)
      (flet ((swap-symbol-values (x y)
               (setf (symbol-value x)
                     (prog1 (symbol-value y)
                       (setf (symbol-value y) (symbol-value x))))))
        (let ((*foo* 1) (*bar* 2))
          (declare (special *foo* *bar*))
          (swap-symbol-values '*foo* '*bar*)
          (values *foo* *bar*)))
    (list n h))
  (2 1))

(check-for-bug :section5-legacy-1757
  (setq temp 1)
  1)

(check-for-bug :section5-legacy-1761
  (prog2 (incf temp) (incf temp) (incf temp))
  3)

(check-for-bug :section5-legacy-1765
  temp
  4)

(check-for-bug :section5-legacy-1769
  (prog2 1 (values 2 3 4) 5)
  2)

;;; progn

(check-for-bug :section5-legacy-1775
  (progn)
  NIL)

(check-for-bug :section5-legacy-1779
  (progn 1 2 3)
  3)

(check-for-bug :section5-legacy-1783
  (multiple-value-bind (n h j)
      (progn (values 1 2 3))
    (list n h j))
  (1 2 3))

(check-for-bug :section5-legacy-1789
  (setq a 1)
  1)

(check-for-bug :section5-legacy-1793
  (if a
      (progn (setq a nil) 'here)
      (progn (setq a t) 'there))
  HERE)

(check-for-bug :section5-legacy-1799
  a
  NIL)

;;; define-modify-macro

(check-for-bug :section5-legacy-1805
  (define-modify-macro appendf (&rest args)
                       append "Append onto list")
  APPENDF)

(check-for-bug :section5-legacy-1810
  (setq x '(a b c) y x)
  (A B C))

(check-for-bug :section5-legacy-1814
  (appendf x '(d e f) '(1 2 3))
  (A B C D E F 1 2 3))

(check-for-bug :section5-legacy-1818
  x
  (A B C D E F 1 2 3))

(check-for-bug :section5-legacy-1822
  y
  (A B C))

;;; defsetf

(check-for-bug :section5-legacy-1828
  (defun middleguy (x) (nth (truncate (1- (list-length x)) 2) x))
  MIDDLEGUY)

(check-for-bug :section5-legacy-1832
  (defun set-middleguy (x v)
    (unless (null x)
      (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
    v)
  SET-MIDDLEGUY)

(check-for-bug :section5-legacy-1839
  (defsetf middleguy set-middleguy)
  MIDDLEGUY)

(check-for-bug :section5-legacy-1843
  (setq a (list 'a 'b 'c 'd)
        b (list 'x)
        c (list 1 2 3 (list 4 5 6) 7 8 9))
  (1 2 3 (4 5 6) 7 8 9))

(check-for-bug :section5-legacy-1849
  (setf (middleguy a) 3)
  3)

(check-for-bug :section5-legacy-1853
  (setf (middleguy b) 7)
  7)

(check-for-bug :section5-legacy-1857
  (setf (middleguy (middleguy c)) 'middleguy-symbol)
  MIDDLEGUY-SYMBOL)

(check-for-bug :section5-legacy-1861
  a
  (A 3 C D))

(check-for-bug :section5-legacy-1865
  b
  (7))

(check-for-bug :section5-legacy-1869
  c
  (1 2 3 (4 MIDDLEGUY-SYMBOL 6) 7 8 9))

(check-for-bug :section5-legacy-1873
  (defsetf my-subseq (sequence start &optional end) (new-sequence)
    `(progn (replace ,sequence ,new-sequence
                     :start1 ,start :end1 ,end)
            ,new-sequence))
  MY-SUBSEQ)

(unintern '*XY*)

(check-for-bug :section5-legacy-1882
  (defvar *xy* (make-array '(10 10) :initial-element NIL))
  *XY*)

(defun xy (&key ((:x x) 0) ((:y y) 0))
  (aref *xy* x y))

(defun set-xy (new-value &key ((:x x) 0) ((:y y) 0))
  (setf (aref *xy* x y) new-value))

(defsetf xy (&key ((:x x) 0) ((:y y) 0)) (store)
  `(set-xy ,store :x ,x :y ,y))

(check-for-bug :section5-legacy-1895
  (progn
    (get-setf-expansion '(xy :x a :y b))
    t)
  t)

(check-for-bug :section5-legacy-1901
  (xy :x 1)
  NIL)

(check-for-bug :section5-legacy-1905
  (setf (xy :x 1) 1)
  1)

(check-for-bug :section5-legacy-1909
  (xy :x 1)
  1)

(check-for-bug :section5-legacy-1913
  (setf (xy :x 1 :y 2) 3)
  3)

(check-for-bug :section5-legacy-1917
  (setf (xy :y 5 :x 9) 14)
  14)

(check-for-bug :section5-legacy-1921
  (xy :y 0 :x 1)
  1)

(check-for-bug :section5-legacy-1925
  (xy :x 1 :y 2)
  3)

;;; define-setf-expander

(check-for-bug :section5-legacy-1931
  (defun lastguy (x) (car (last x)))
  LASTGUY)

(check-for-bug :section5-legacy-1935
  (define-setf-expander lastguy (x &environment env)
    "Set the last element in a list to the given value."
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion x env)
      (let ((store (gensym)))
        (values dummies
                vals
                `(,store)
                `(progn (rplaca (last ,getter) ,store) ,store)
                `(lastguy ,getter)))))
  LASTGUY)

(check-for-bug :section5-legacy-1948
  (setq a (list 'a 'b 'c 'd)
        b (list 'x)
        c (list 1 2 3 (list 4 5 6)))
  (1 2 3 (4 5 6)))

(check-for-bug :section5-legacy-1954
  (setf (lastguy a) 3)
  3)

(check-for-bug :section5-legacy-1958
  (setf (lastguy b) 7)
  7)

(check-for-bug :section5-legacy-1962
  (setf (lastguy (lastguy c)) 'lastguy-symbol)
  LASTGUY-SYMBOL)

(check-for-bug :section5-legacy-1966
  a
  (A B C 3))

(check-for-bug :section5-legacy-1970
  b
  (7))

(check-for-bug :section5-legacy-1974
  c
  (1 2 3 (4 5 LASTGUY-SYMBOL)))

;;; setf

(check-for-bug :section5-legacy-1980
  (setq x (cons 'a 'b) y (list 1 2 3))
  (1 2 3) )

(check-for-bug :section5-legacy-1984
  (setf (car x) 'x (cadr y) (car x) (cdr x) y)
  (1 X 3) )

(check-for-bug :section5-legacy-1988
  x
  (X 1 X 3) )

(check-for-bug :section5-legacy-1992
  y
  (1 X 3) )

(check-for-bug :section5-legacy-1996
  (setq x (cons 'a 'b) y (list 1 2 3))
  (1 2 3) )

(check-for-bug :section5-legacy-2000
  (psetf (car x) 'x (cadr y) (car x) (cdr x) y)
  NIL )

(check-for-bug :section5-legacy-2004
  x
  (X 1 A 3) )

(check-for-bug :section5-legacy-2008
  y
  (1 A 3) )

;;; shiftf

(check-for-bug :section5-legacy-2014
  (setq x (list 1 2 3) y 'trash)
  TRASH)

(check-for-bug :section5-legacy-2018
  (shiftf y x (cdr x) '(hi there))
  TRASH)

(check-for-bug :section5-legacy-2022
  x
  (2 3))

(check-for-bug :section5-legacy-2026
  y
  (1 HI THERE))

(check-for-bug :section5-legacy-2030
  (setq x (list 'a 'b 'c))
  (A B C))

(check-for-bug :section5-legacy-2034
  (shiftf (cadr x) 'z)
  B)

(check-for-bug :section5-legacy-2038
  x
  (A Z C))

(check-for-bug :section5-legacy-2042
  (shiftf (cadr x) (cddr x) 'q)
  Z)

(check-for-bug :section5-legacy-2046
  x
  (A (C) . Q))

(check-for-bug :section5-legacy-2050
  (setq n 0)
  0)

(check-for-bug :section5-legacy-2054
  (setq x (list 'a 'b 'c 'd))
  (A B C D))

(check-for-bug :section5-legacy-2058
  (shiftf (nth (setq n (+ n 1)) x) 'z)
  B)

(check-for-bug :section5-legacy-2062
  x
  (A Z C D))

;;; rotatef

(check-for-bug :section5-legacy-2068
  (let ((n 0)
        (x (list 'a 'b 'c 'd 'e 'f 'g)))
    (rotatef (nth (incf n) x)
             (nth (incf n) x)
             (nth (incf n) x))
    x)
  (A C D B E F G))



























