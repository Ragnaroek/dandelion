;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)
;;;
;;; testfile nach steele-beispielen
;;;

;; 7.3


(check-for-bug :steele7-legacy-10
  (let ((f '+))
    (apply f '(1 2)))
  3)

(check-for-bug :steele7-legacy-15
  (let    ((f #'-))
    (apply f '(1 2)))
  -1)

(check-for-bug :steele7-legacy-20
  (apply #'max 3 5 '(2 7 3))
  7)

(check-for-bug :steele7-legacy-24
  (apply 'cons '((+ 2 3) 4))
  ((+ 2 3) . 4))


(check-for-bug :steele7-legacy-29
  (apply #'+ '())
  0)

(check-for-bug :steele7-legacy-33
  (apply #'(lambda (&key a b)(list a b)) '(:b 3))
  (nil 3))


(check-for-bug :steele7-legacy-38
  (funcall '+ 2 3)
  5)

(check-for-bug :steele7-legacy-42
  (let    ((c (symbol-function '+)))
    (funcall c 1 2 3 4))
  10)


;;abschnitt 7.4

;; progn
(check-for-bug :steele7-legacy-51
  (progn 1 2 3)
  3)

(check-for-bug :steele7-legacy-55
  (progn (+ 2 1) 2)
  2)

(check-for-bug :steele7-legacy-59
  (progn 1 2 (values  2 3))
  2)

(check-for-bug :steele7-legacy-63
  (progn)
  nil)


;; prog1
(check-for-bug :steele7-legacy-69
  (prog1 1 2 3)
  1)

(check-for-bug :steele7-legacy-73
  (prog1 3 (+ 1 2) 2)
  3)

(check-for-bug :steele7-legacy-77
  (prog1 (values  2 3) 1 2 )
  2)

(check-for-bug :steele7-legacy-81
  (let ((x '(a b c)))
    (prog1 (car x)(rplaca x 'foo)))
  a)

;; prog2

(check-for-bug :steele7-legacy-88
  (prog2 1 2 3)
  2)

(check-for-bug :steele7-legacy-92
  (prog2  (+ 1 2) 2 3)
  2)

(check-for-bug :steele7-legacy-96
  (prog2 1 (values  2 3) 4)
  2)

;; 7.5

;; let
(setf a 0)

(check-for-bug :steele7-legacy-105
  (let ((a 1)(b 2) c)
    (declare (integer a b))
    (list a b c))
  (1 2 nil))


(check-for-bug :steele7-legacy-112
  (let ((a 1)
        (b a))
    (declare (integer a b))
    (list a b))
  (1 0))

(check-for-bug :steele7-legacy-119
  (let (x239)
    (declare (special x239))
    (symbol-value 'x239))
  nil)

;; let*
(check-for-bug :steele7-legacy-126
  (let* ((a 1)(b 2) c )
    (declare (integer a b))
    (list a b c))
  (1 2 nil))


(check-for-bug :steele7-legacy-133
  (let* ((a 1)(b a))
    (declare (integer a b))
    (list a b))
  (1 1))

;; compiler-let (?)


;; progv

(check-for-bug :steele7-legacy-144
  (progv
      '(a b c)
      '(1 2 3)

    (+ a b c))
  6)

(unintern 'a)
(unintern 'b)
(unintern 'c)

(check-for-bug :steele7-legacy-156
  (progv
      '(a b c)
      '(1 2)

    (list a b c))
  error)

(check-for-bug :steele7-legacy-164
  (let ((v '(a b c))
        (val '(3 2 1)))
    (progv v val (mapcar #'eval v)))
  (3 2 1))


;; flet

(check-for-bug :steele7-legacy-173
  (flet ((plus (a b)(+ a b))
         (minus (a b)(- a b)))
    (list (plus 1 2)(minus 1 2)))
  (3 -1))


(check-for-bug :steele7-legacy-180
  (list (flet ( (+ (a b)(- a b)))(+ 3 2))(+ 3 2))
  (1 5))

(check-for-bug :steele7-legacy-184
  (flet ((+ (a b)(+ (+ a b a) b)))(+ 3 2))
  10)

;; labels
(check-for-bug :steele7-legacy-189
  (labels ((queue (l)(if (car l)(queue (cdr l))'ende)))(queue '(1 2 3)))
  ende)

(check-for-bug :steele7-legacy-193
  (labels ((+ (a b)(* a (+ a a b))))(+ 1 2 3))
  error)

;; macrolet ?


;; 7.6

;; if

(check-for-bug :steele7-legacy-204
  (let ((a t)(b nil))(list (if a 1 2)(if b 1 2)(if a 1)(if b 1)))
  (1 2 1 nil))


;; when
(check-for-bug :steele7-legacy-210
  (let ((a t)(b nil))(list (when a 1 2)(when b 1 2)(when a 1)))
  (2 nil 1))


;; unless
(check-for-bug :steele7-legacy-216
  (let ((a t)(b nil))(list (unless a 1 2)(unless b 1 2)(unless a 1)))
  (nil 2 nil))


;; cond
(check-for-bug :steele7-legacy-222
  (let ((a t)(b 10)(c nil))
    (list (cond (a 1)(t 'end))(cond (b)(t 'end))(cond (c 1)(t 'end))))
  (1 10 end))


;; case
(check-for-bug :steele7-legacy-229
  (case (+  1 2)
    (1 -1)
    (2 -2)
    (3 -3))
  -3)

(check-for-bug :steele7-legacy-236
  (case (+  1 2)
    (1 -1)
    (2 -2))
  nil)


;; (case (+  1 2)
;;       (1 -1)
;;       (2 -2)
;;       (1 -1)
;;       (3 -3))
;; error


(check-for-bug :steele7-legacy-251
  (case (+  1 2)
    ((1 3) -1)
    (2 -2)
    (otherwise 100))
  -1)


;;
;; (case (+  1 2)
;;       ((1 3) -1)
;;       ((2 1) -2)
;;       (t 100))
;; error          ;weil ein key nur einmal erscheinen darf!
;;



;; typecase

(check-for-bug :steele7-legacy-271
  (typecase (+  1 2)
    (list -2)
    (null -3)
    (integer -1))
  -1)

;; 7.7

;; block

(check-for-bug :steele7-legacy-282
  (block blocktest (if t (return 0) ) 1)
  error)

(check-for-bug :steele7-legacy-286
  (block blocktest (if t (return-from blocktest 0) ) 1)
  0)


(check-for-bug :steele7-legacy-291
  (block blocktest (if nil (return-from blocktest 0) ) 1)
  1)


(check-for-bug :steele7-legacy-296
  (block blocktest (catch 'catcher
                     (if t (throw 'catcher 0) ) 1))
  0)


;; 7.8

;; 7.8.1

;; loop

(check-for-bug :steele7-legacy-308
  (let ((i 10))
    (loop (if (< (decf i) 1)(return i))))
  0)


(check-for-bug :steele7-legacy-314
  (let ((i 10))
    (catch 'catcher
      (loop (if (< (decf i) 1)(return i)))))
  0)

;; 7.8.2
;; do,do*

(setf a 0)

(check-for-bug :steele7-legacy-325
  (do ((a 1 (+ a 1))(b a))
      ((> a 9) (list b c))
    (setf c (+ a b)))
  (0 9))

(check-for-bug :steele7-legacy-331
  (do* ((a 1 (+ a 1))(b a))
       ((> a 9) b)
    )
  1)

(check-for-bug :steele7-legacy-337
  (let ((a 0))
    (do* ((a 1 (+ a 1))(b a))
         ((> a 9) a)
      (declare (integer a b)))
    a)
  0)



;; 7.8.3


;; dolist
(check-for-bug :steele7-legacy-351
  (let    ((l '(1 2 3))
           (r 0))
    (dolist (x l r)
      (setf r (+ r  x)) ))
  6)


;; dolist
(check-for-bug :steele7-legacy-360
  (let ((l '(1 2 3)))
    (dolist (x l)(if (> 0 x)(incf x)(return 10))))
  10)

(check-for-bug :steele7-legacy-365
  (let ((l '(1 2 3)))
    (dolist (x l )(incf x)))
  nil)

;; dotimes

(check-for-bug :steele7-legacy-372
  (let ((s 0))
    (dotimes (i (+ 1 9)s)
      (setf s (+ s i))))
  45)


;; 7.8.4


;; mapcar

(check-for-bug :steele7-legacy-384
  (mapcar #'abs '(3 -4 2 -5 -6))
  (3 4 2 5 6))

(check-for-bug :steele7-legacy-388
  (mapcar #'cons '(a b c) '(1 2 3))
  ((a . 1) (b . 2) (c . 3)))


;; maplist

(check-for-bug :steele7-legacy-395
  (maplist #'(lambda (x)(cons 'foo x))'(a b c d))
  ((foo a b c d)(foo b c d)(foo c d)(foo d)))


(check-for-bug :steele7-legacy-400
  (maplist #'(lambda (x) (if (member (car x)(cdr x)) 0 1))
           '(a b a c d b c))
  (0 0 1 0 1 1 1))


;; mapc
(check-for-bug :steele7-legacy-407
  (mapc #'abs '(3 -4 2 -5 -6))
  (3 -4 2 -5 -6))

;; mapc

(check-for-bug :steele7-legacy-413
  (mapl #'(lambda (x y)(cons x y))'(a b c d)'(1 2 3 4))
  (a b c d))

;; mapcan

(check-for-bug :steele7-legacy-419
  (mapcan #'(lambda (x)(and (numberp x)(list x)))'(a 1 b c 3 4 d 5))
  (1 3 4 5))

;; mapcon

(check-for-bug :steele7-legacy-425
  (mapcon #'(lambda (x)(and (oddp (car x))(list (car x))))'(5 4 3 2 1))
  (5 3 1))

;; 7.8.5

;; tagbody
(check-for-bug :steele7-legacy-432
  (let ((a 0))
    (tagbody (if nil (go tag0) (go tag1))
       (this will never be reached)
     tag0
       (setf a 1)
     tag1
       (setf a 2))
    a)
  2)

(check-for-bug :steele7-legacy-443
  (let ((a 0))
    (tagbody (if t (go tag0) (go tag1))
       (this will never be reached)
     tag0
       (setf a 1)
       )
    a)
  ;;  cmucl compiles on the fly and therefore signals an error
  #-(or cmu sbcl) 1
  #+(or cmu sbcl) error)



;; prog*

(check-for-bug :steele7-legacy-459
  (let ((z '(1 0)))
    (prog* ((y z)(x (car y)))
       (return x)))
  1)

(check-for-bug :steele7-legacy-465
  (prog  (a (b 1))
     (if a (go tag0) (go tag1))
     (this will never be reached)
     tag0
     (setf a 1)
     (this will never be reached)
     tag1
     (setf a 2))
  nil)



(check-for-bug :steele7-legacy-478
  (prog  (a (b 1))
     (if a (return nil) (go tag1))
     (this will never be reached)
     tag0
     (return (list a 1))
     tag1
     (setf a 2)
     (go tag0))
  (2 1))


;; 7.9

;; multiple-value-bind
(check-for-bug :steele7-legacy-493
  (defun adder (x y)(values (+ 1 x)(+ 1 y) ) )
  adder)


(check-for-bug :steele7-legacy-498
  (multiple-value-bind (a b)(adder 1 2)(+ a b))
  5)

(check-for-bug :steele7-legacy-502
  (defun adder (x y)(values-list (list  (+ 1 x)(+ 1 y))))
  adder)


(check-for-bug :steele7-legacy-507
  (multiple-value-bind (a b)(adder 1 2)(+ a b))
  5)


(check-for-bug :steele7-legacy-512
  (multiple-value-list (floor -3 4))
  (-1 1))


(check-for-bug :steele7-legacy-517
  (multiple-value-call #'+ (floor 5 3)(floor 19 4))
  10)

(check-for-bug :steele7-legacy-521
  (multiple-value-bind (c d)
      (multiple-value-prog1 (floor -3 4) (+ 1 2))
    (list c d))
  (-1 1))


(check-for-bug :steele7-legacy-528
  (multiple-value-bind (x)(floor 5 3)(list x))
  (1))


(check-for-bug :steele7-legacy-533
  (multiple-value-bind (x y)(floor 5 3)(list x y))
  (1 2))


(check-for-bug :steele7-legacy-538
  (multiple-value-bind (x y z)(floor 5 3)(list x y z))
  (1 2 nil))




(check-for-bug :steele7-legacy-545
  (multiple-value-setq
      (a b)
    (values 10 20))
  10)

(check-for-bug :steele7-legacy-551
  b
  20)

(unintern 'a)
(unintern 'b)
;; 7.10

;; catch/throw/unwind-protect

