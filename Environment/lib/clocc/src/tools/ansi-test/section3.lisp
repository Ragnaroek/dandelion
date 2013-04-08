;;; 3.1.2.1.1.4 -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(if (boundp 'x2q) (makunbound 'x2q) 'ok)

(check-for-bug :section3-legacy-8
  (let ((x2q 1))                        ;Binds a special variable X
    (declare (special x2q))
    (let ((x2q 2))			;Binds a lexical variable X
      (+ x2q				;Reads a lexical variable X
         (locally (declare (special x2q))
           x2q))))                      ;Reads a special variable X
  3)

(if (boundp 'x3q) (makunbound 'x3q) 'ok)

(check-for-bug :section3-legacy-19
  (progn
    (defun two-funs (x3q)
      (list (function (lambda () x3q))
            (function (lambda (y) (setq x3q y)))))
    (setq funs (two-funs 6))
    T)
  T)

(check-for-bug :section3-legacy-28
  (funcall (car funs))
  6)

(check-for-bug :section3-legacy-32
  (funcall (cadr funs) 43)
  43)

(check-for-bug :section3-legacy-36
  (funcall (car funs))
  43)

;;; 3.1.5
(check-for-bug :section3-legacy-41
  (progn
    (defun contorted-example (f g x)
      (if (= x 0)
          (funcall f)
          (block here
            (+ 5 (contorted-example g
                                    #'(lambda () (return-from here 4))
                                    (- x 1))))))
    t)
  T)

(check-for-bug :section3-legacy-53
  (contorted-example nil nil 2)
  4)


(check-for-bug :section3-legacy-58
  (progn
    (defun contorted-example (f g x)
      (if (= x 0)
          (funcall g)
          (block here
            (+ 5 (contorted-example g
                                    #'(lambda () (return-from here 4))
                                    (- x 1))))))
    t)
  T)

(check-for-bug :section3-legacy-70
  (contorted-example nil nil 2)
  9)

;;; 3.1.6

(check-for-bug :section3-legacy-76
  (progn
    (defun invalid-example ()
      (let ((y (block here #'(lambda (z) (return-from here z)))))
        (if (numberp y) y (funcall y 5))))
    T)
  T)

(check-for-bug :section3-legacy-84
  (invalid-example)
  CONTROL-ERROR)

(check-for-bug :section3-legacy-88
  (progn
    (defun fun1 (x)
      (catch 'trap (+ 3 (fun2 x))))
    (defun fun2 (y)
      (catch 'trap (* 5 (fun3 y))))
    (defun fun3 (z)
      (throw 'trap z))
    T)
  T)

(check-for-bug :section3-legacy-99
  (fun1 7)
  10)

;;; 3.3.4.1

(unintern 'x)

(check-for-bug :section3-legacy-107
  (let ((x 1))
    (declare (special x))
    (let ((x 2))
      (let ((old-x x)
            (x 3))
        (declare (special x))
        (list old-x x))))
  (2 3)
  "The first declare is only valid in it's
block. The (let ((x 2)) is a new block,
where x is not special anymore.")

(if (boundp 'x) (makunbound 'x) 'ok)

(check-for-bug :section3-legacy-122
  (let ((x4q  1))			;[1]
    (declare (special x4q))		;[2]
    (let ((x4q 2))			;[3]
      (dotimes (i x4q x4q)		;[4]
        (declare (special x4q)))))	;[5]
  1)


(if (boundp 'x) (makunbound 'x) 'ok)

;;; 3.4.1.4.1.1


(check-for-bug :section3-legacy-136
  ((lambda (&key x) x) :x 1 :y 2 :allow-other-keys t)
  1)

(check-for-bug :section3-legacy-140
  ((lambda (&key x &allow-other-keys) x) :x 1 :y 2)
  1)

(check-for-bug :section3-legacy-144
  ((lambda (&key) t) :allow-other-keys nil)
  T)

(check-for-bug :section3-legacy-148
  ((lambda (&key x) x)
   :x 1 :y 2 :allow-other-keys t :allow-other-keys nil)
  1)

(check-for-bug :section3-legacy-153
  ((lambda (&key x) x)                  ;This call is not valid
   :x 1 :y 2 :allow-other-keys nil :allow-other-keys t)
  PROGRAM-ERROR
  "See 3.5.1.4:
If this situation occurs in a safe call, an error of type
program-error must be signaled; and in an unsafe call the
situation has undefined consequences. ");; from 3.5.1.4

;;; 3.4.1.6


(check-for-bug :section3-legacy-165
  ((lambda (a b) (+ a (* b 3))) 4 5)
  19)

(check-for-bug :section3-legacy-169
  ((lambda (a &optional (b 2)) (+ a (* b 3))) 4 5)
  19)

(check-for-bug :section3-legacy-173
  ((lambda (a &optional (b 2)) (+ a (* b 3))) 4)
  10)

(check-for-bug :section3-legacy-177
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)))
  (2 NIL 3 NIL NIL))

(check-for-bug :section3-legacy-181
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6)
  (6 T 3 NIL NIL))

(check-for-bug :section3-legacy-185
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3)
  (6 T 3 T NIL))

(check-for-bug :section3-legacy-189
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3 8)
  (6 T 3 T (8)))

(check-for-bug :section3-legacy-193
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x))
   6 3 8 9 10 11)
  (6 t 3 t (8 9 10 11)))

(check-for-bug :section3-legacy-198
  ((lambda (a b &key c d) (list a b c d)) 1 2)
  (1 2 NIL NIL))

(check-for-bug :section3-legacy-202
  ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6)
  (1 2 6 NIL))

(check-for-bug :section3-legacy-206
  ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8)
  (1 2 NIL 8))

(check-for-bug :section3-legacy-210
  ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6 :d 8)
  (1 2 6 8))

(check-for-bug :section3-legacy-214
  ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8 :c 6)
  (1 2 6 8))

(check-for-bug :section3-legacy-218
  ((lambda (a b &key c d) (list a b c d)) :a 1 :d 8 :c 6)
  (:a 1 6 8))

(check-for-bug :section3-legacy-222
  ((lambda (a b &key c d) (list a b c d)) :a :b :c :d)
  (:a :b :d NIL))

(check-for-bug :section3-legacy-226
  ((lambda (a b &key ((:sea c)) d) (list a b c d)) 1 2 :sea 6)
  (1 2 6 NIL))

(check-for-bug :section3-legacy-230
  ((lambda (a b &key ((c c)) d) (list a b c d)) 1 2 'c 6)
  (1 2 6 NIL)
  "3.4.1.4: ...
If the notation ((keyword-name var) init-form) is used,
then the keyword name used to match arguments to
parameters is keyword-name, which may
be a symbol in any package. ...
")

(check-for-bug :section3-legacy-240
  ((lambda (a &optional (b 3) &rest x &key c (d a))
     (list a b c d x)) 1)
  (1 3 NIL 1 ()) )

(check-for-bug :section3-legacy-245
  ((lambda (a &optional (b 3) &rest x &key c (d a))
     (list a b c d x)) 1 2)
  (1 2 NIL 1 ()))

(check-for-bug :section3-legacy-250
  ((lambda (a &optional (b 3) &rest x &key c (d a))
     (list a b c d x)) :c 7)
  (:c 7 NIL :c ()))

(check-for-bug :section3-legacy-255
  ((lambda (a &optional (b 3) &rest x &key c (d a))
     (list a b c d x)) 1 6 :c 7)
  (1 6 7 1 (:c 7)))

(check-for-bug :section3-legacy-260
  ((lambda (a &optional (b 3) &rest x &key c (d a))
     (list a b c d x)) 1 6 :d 8)
  (1 6 NIL 8 (:d 8)))

(check-for-bug :section3-legacy-265
  ((lambda (a &optional (b 3) &rest x &key c (d a))
     (list a b c d x)) 1 6 :d 8 :c 9 :d 10)
  (1 6 9 8 (:d 8 :c 9 :d 10)))

;;;; eval function
					;(let ((form2p5 nil)
					;      (a2p5 nil))

					;  (check-for-bug :section3-legacy-274
					;   (setq form2p5 '(1+ a2p5) a2p5 999)
					;   999)

					;  (check-for-bug :section3-legacy-278
					;   (eval form2p5)
					;   1000)

					;  (check-for-bug :section3-legacy-282
					;   (eval 'form2p5)
					;   (1+ A2p5))

					;  (check-for-bug :section3-legacy-286
					;   (let ((a2p5 '(this would break if eval used local value)))
					;     (eval form2p5))
					;   1000))

;;; quote
(check-for-bug :section3-legacy-292
  (let ((a 1))
    a)
  1)

(check-for-bug :section3-legacy-297
  (let ((a 1))
    (quote (setq a 3)))
  (SETQ A 3))

(check-for-bug :section3-legacy-302
  (let ((a 1))
    (quote (setq a 3))
    a)
  1)

(check-for-bug :section3-legacy-308
  (let ((a 1))
    (quote (setq a 3))
    'a)
  A)

(check-for-bug :section3-legacy-314
  (let ((a 1))
    (quote (setq a 3))
    ''a)
  (QUOTE A) )

(check-for-bug :section3-legacy-320
  (let ((a 1))
    (quote (setq a 3))
    '''a)
  (QUOTE (QUOTE A)))

(check-for-bug :section3-legacy-326
  (let ((a 43))
    a)
  43)

(check-for-bug :section3-legacy-331
  (let ((a 43))
    (list a (cons a 3)))
  (43 (43 . 3)))

(check-for-bug :section3-legacy-336
  (let ((a 43))
    (list a (cons a 3))
    (list (quote a) (quote (cons a 3))))
  (A (CONS A 3)) )


(check-for-bug :section3-legacy-343
  1
  1)

(check-for-bug :section3-legacy-347
  '1
  1)

(check-for-bug :section3-legacy-351
  '"foo"
  "foo")

(check-for-bug :section3-legacy-355
  (car '(a b))
  A)

(check-for-bug :section3-legacy-359
  '(car '(a b))
  (CAR (QUOTE (A B))))

(check-for-bug :section3-legacy-363
  #(car '(a b))
  #(CAR (QUOTE (A B))))

(check-for-bug :section3-legacy-367
  '#(car '(a b))
  #(CAR (QUOTE (A B))))

;;; define-compiler-macro
(check-for-bug :section3-legacy-372
  (defun square (x) (expt x 2))
  SQUARE)

(check-for-bug :section3-legacy-376
  (define-compiler-macro square (&whole form arg)
    (if (atom arg)
        `(expt ,arg 2)
        (case (car arg)
          (square (if (= (length arg) 2)
                      `(expt ,(nth 1 arg) 4)
                      form))
          (expt   (if (= (length arg) 3)
                      (if (numberp (nth 2 arg))
                          `(expt ,(nth 1 arg) ,(* 2 (nth 2 arg)))
                          `(expt ,(nth 1 arg) (* 2 ,(nth 2 arg))))
                      form))
          (otherwise `(expt ,arg 2)))))
  SQUARE)

(check-for-bug :section3-legacy-392
  (square (square 3))
  81)

(check-for-bug :section3-legacy-396
  (macroexpand '(square x))
  (SQUARE X))				;  f

(if (boundp 'x) (makunbound 'x) 'ok)

(check-for-bug :section3-legacy-402
  (funcall (compiler-macro-function 'square) '(square x) nil)
  (EXPT X 2))

(check-for-bug :section3-legacy-406
  (funcall (compiler-macro-function 'square) '(square (square x)) nil)
  (EXPT X 4))

(check-for-bug :section3-legacy-410
  (funcall (compiler-macro-function 'square) '(funcall #'square x) nil)
  (EXPT X 2)
  "define-compiler-macro:
... but if the car of the actual form is the symbol funcall,
then the destructuring of the arguments
is actually performed using its cddr instead")

;;; defmacro
(check-for-bug :section3-legacy-419
  (defmacro mac1 (a b) "Mac1 multiplies and adds"
            `(+ ,a (* ,b 3)))
  MAC1 )

(check-for-bug :section3-legacy-424
  (mac1 4 5)
  19 )

(check-for-bug :section3-legacy-428
  (documentation 'mac1 'function)
  "Mac1 multiplies and adds" )

(check-for-bug :section3-legacy-432
  (defmacro mac2 (&optional (a 2 b) (c 3 d) &rest x)
    `'(,a ,b ,c ,d ,x))
  MAC2 )

(check-for-bug :section3-legacy-437
  (mac2 6)
  (6 T 3 NIL NIL) )

(check-for-bug :section3-legacy-441
  (mac2 6 3 8)
  (6 T 3 T (8)) )

(check-for-bug :section3-legacy-445
  (defmacro mac3 (&whole r a &optional (b 3) &rest x &key c (d a))
    `'(,r ,a ,b ,c ,d ,x))
  MAC3 )

(check-for-bug :section3-legacy-450
  (mac3 1 6 :d 8 :c 9 )
  ((MAC3 1 6 :D 8 :C 9 ) 1 6 9 8 (:D 8 :C 9)) )

;;; part II
(check-for-bug :section3-legacy-455
  (progn
    (defmacro dm1a (&whole x) `',x)
    t)
  t)

(check-for-bug :section3-legacy-461
  (macroexpand '(dm1a))
  (QUOTE (DM1A)))

(check-for-bug :section3-legacy-465
  (macroexpand '(dm1a a))
  ERROR)

(check-for-bug :section3-legacy-469
  (progn
    (defmacro dm1b (&whole x a &optional b) `'(,x ,a ,b))
    t)
  t)

(check-for-bug :section3-legacy-475
  (macroexpand '(dm1b))
  ERROR)

(check-for-bug :section3-legacy-479
  (macroexpand '(dm1b q))
  (QUOTE ((DM1B Q) Q NIL)))

(check-for-bug :section3-legacy-483
  (macroexpand '(dm1b q r))
  (QUOTE ((DM1B Q R) Q R)))

(check-for-bug :section3-legacy-487
  (macroexpand '(dm1b q r s))
  ERROR)

(check-for-bug :section3-legacy-491
  (progn
    (defmacro dm2a (&whole form a b) `'(form ,form a ,a b ,b))
    t)
  t)

(check-for-bug :section3-legacy-497
  (macroexpand '(dm2a x y))
  (QUOTE (FORM (DM2A X Y) A X B Y)))

(check-for-bug :section3-legacy-501
  (dm2a x y)
  (FORM (DM2A X Y) A X B Y))

(check-for-bug :section3-legacy-505
  (progn
    (defmacro dm2b (&whole form a (&whole b (c . d) &optional (e 5))
                           &body f &environment env)
      ``(,',form ,,a ,',b ,',(macroexpand c env) ,',d ,',e ,',f))
    t)
  t)

					;Note that because backquote is involved, implementations may differ
					;slightly in the nature (though not the functionality) of the expansion.

					;(check-for-bug :section3-legacy-516
					;(macroexpand '(dm2b x1 (((incf x2) x3 x4)) x5 x6))
					;#+(or cmu sbcl sbcl) `((DM2B X1 (((INCF X2) X3 X4)) X5 X6) ,X1 (((INCF X2) X3 X4))
					;	(LET* ((#:G411 (+ X2 1)))
					;	      (SETQ X2 #:G411))
					;	(X3 X4) 5 (X5 X6))
					;#-(or cmu sbcl sbcl) (LIST* '(DM2B X1 (((INCF X2) X3 X4))
					;                   X5 X6)
					;            X1
					;            '((((INCF X2) X3 X4)) (SETQ X2 (+ X2 1)) (X3 X4) 5 (X5 X6))))

(check-for-bug :section3-legacy-527
  (let ((x1 5))
    (macrolet ((segundo (x) `(cadr ,x)))
      (dm2b x1 (((segundo x2) x3 x4)) x5 x6)))
  ((DM2B X1 (((SEGUNDO X2) X3 X4)) X5 X6)
   5 (((SEGUNDO X2) X3 X4)) (CADR X2) (X3 X4) 5 (X5 X6)))

;;; macrofunction

(check-for-bug :section3-legacy-536
  (defmacro macfun (x) '(macro-function 'macfun))
  MACFUN )

(check-for-bug :section3-legacy-540
  (not (macro-function 'macfun))
  nil)

(check-for-bug :section3-legacy-544
  (macrolet ((foo (&environment env)
               (if (macro-function 'bar env)
                   ''yes
                   ''no)))
    (list (foo)
          (macrolet ((bar () :beep))
            (foo))))
  (NO YES))

;;; macroexpand

(check-for-bug :section3-legacy-556
  (defmacro alpha (x y) `(beta ,x ,y))
  ALPHA)

(check-for-bug :section3-legacy-560
  (defmacro beta (x y) `(gamma ,x ,y))
  BETA)

(check-for-bug :section3-legacy-564
  (defmacro delta (x y) `(gamma ,x ,y))
  DELTA)

(check-for-bug :section3-legacy-568
  (defmacro expand (form &environment env)
    (multiple-value-bind (expansion expanded-p)
        (macroexpand form env)
      `(values ',expansion ',expanded-p)))
  EXPAND)

(check-for-bug :section3-legacy-575
  (defmacro expand-1 (form &environment env)
    (multiple-value-bind (expansion expanded-p)
        (macroexpand-1 form env)
      `(values ',expansion ',expanded-p)))
  EXPAND-1)

;; Simple examples involving just the global environment
(check-for-bug :section3-legacy-583
  (multiple-value-bind (a b)
      (macroexpand-1 '(alpha a b))
    (list a b))
  ((BETA A B) T))

(check-for-bug :section3-legacy-589
  (multiple-value-bind (a b)
      (expand-1 (alpha a b))
    (list a b))
  ((BETA A B) T))

(check-for-bug :section3-legacy-595
  (multiple-value-bind (a b)
      (macroexpand '(alpha a b))
    (list a b))
  ((GAMMA A B) T))

(check-for-bug :section3-legacy-601
  (multiple-value-bind (a b)
      (expand (alpha a b))
    (list a b))
  ((GAMMA A B) T))

(check-for-bug :section3-legacy-607
  (multiple-value-bind (a b)
      (macroexpand-1 'not-a-macro)
    (list a b))
  (NOT-A-MACRO nil))

(check-for-bug :section3-legacy-613
  (multiple-value-bind (a b)
      (expand-1 not-a-macro)
    (list a b))
  (NOT-A-MACRO nil) )

(check-for-bug :section3-legacy-619
  (multiple-value-bind (a b)
      (macroexpand '(not-a-macro a b))
    (list a b))
  ((NOT-A-MACRO A B) nil))

(check-for-bug :section3-legacy-625
  (multiple-value-bind (a b)
      (expand (not-a-macro a b))
    (list a b))
  ((NOT-A-MACRO A B) nil))

;; Examples involving lexical environments

(check-for-bug :section3-legacy-633
  (multiple-value-bind (n h)
      (macrolet ((alpha (x y) `(delta ,x ,y)))
        (macroexpand-1 '(alpha a b)))
    (list n h))
  ((BETA A B) T))

(check-for-bug :section3-legacy-640
  (multiple-value-bind (n h)
      (macrolet ((alpha (x y) `(delta ,x ,y)))
        (expand-1 (alpha a b)))
    (list n h))
  ((DELTA A B) T))

(check-for-bug :section3-legacy-647
  (multiple-value-bind (n h)
      (macrolet ((alpha (x y) `(delta ,x ,y)))
        (macroexpand '(alpha a b)))
    (list n h))
  ((GAMMA A B) T))

(check-for-bug :section3-legacy-654
  (multiple-value-bind (n h)
      (macrolet ((alpha (x y) `(delta ,x ,y)))
        (expand (alpha a b)))
    (list n h))
  ((GAMMA A B) T))


(check-for-bug :section3-legacy-662
  (multiple-value-bind (n h)
      (macrolet ((beta (x y) `(epsilon ,x ,y)))
        (expand (alpha a b)))
    (list n h))
  ((EPSILON A B) T))

(check-for-bug :section3-legacy-669
  (multiple-value-bind (n h)
      (let ((x (list 1 2 3)))
        (symbol-macrolet ((a (first x)))
            (expand a)))
    (list n h))
  error
  "A has been declared special, thus SYMBOL-MACROLET may not bind it")

(check-for-bug :section3-legacy-678
  (multiple-value-bind (n h)
      (let ((x (list 1 2 3)))
        (symbol-macrolet ((a-new (first x)))
            (expand a-new)))
    (list n h))
  ((FIRST X) T))

(check-for-bug :section3-legacy-686
  (multiple-value-bind (n h)
      (let ((x (list 1 2 3)))
        (symbol-macrolet ((a (first x)))
            (macroexpand 'a)))
    (list n h))
  error
  "A has been declared special, thus SYMBOL-MACROLET may not bind it")

(check-for-bug :section3-legacy-695
  (multiple-value-bind (n h)
      (let ((x (list 1 2 3)))
        (symbol-macrolet ((a-new (first x)))
            (macroexpand 'a-new)))
    (list n h))
  (a-new nil))

(check-for-bug :section3-legacy-703
  (multiple-value-bind (n h)
      (symbol-macrolet ((b (alpha x y)))
          (expand-1 b))
    (list n h))
  error
  "B has been declared special, thus SYMBOL-MACROLET may not bind it")

(check-for-bug :section3-legacy-711
  (multiple-value-bind (n h)
      (symbol-macrolet ((b-new (alpha x y)))
          (expand-1 b-new))
    (list n h))
  ((ALPHA X Y)  T))

(check-for-bug :section3-legacy-718
  (multiple-value-bind (n h)
      (symbol-macrolet ((b (alpha x y)))
          (expand b))
    (list n h))
  error
  "B has been declared special, thus SYMBOL-MACROLET may not bind it")

(check-for-bug :section3-legacy-726
  (multiple-value-bind (n h)
      (symbol-macrolet ((b-new (alpha x y)))
          (expand b-new))
    (list n h))
  ((GAMMA X Y) T))

(check-for-bug :section3-legacy-733
  (multiple-value-bind (n h)
      (symbol-macrolet ((b (alpha x y))
                        (a b))
          (expand-1 a))
    (list n h))
  error
  "A and B have been declared special, thus SYMBOL-MACROLET may not bind them")

(check-for-bug :section3-legacy-742
  (multiple-value-bind (n h)
      (symbol-macrolet ((b-new (alpha x y))
                        (a-new b-new))
          (expand-1 a-new))
    (list n h))
  (B-NEW T))

(check-for-bug :section3-legacy-750
  (multiple-value-bind (n h)
      (symbol-macrolet ((b (alpha x y))
                        (a b))
          (expand a))
    (list n h))
  error
  "A and B have been declared special, thus SYMBOL-MACROLET may not bind them")

(check-for-bug :section3-legacy-759
  (multiple-value-bind (n h)
      (symbol-macrolet ((b-new (alpha x y))
                        (a-new b-new))
          (expand a-new))
    (list n h))
  ((GAMMA X Y) T))

;; Examples of shadowing behavior
(check-for-bug :section3-legacy-768
  (multiple-value-bind (n h)
      (flet ((beta (x y) (+ x y)))
        (expand (alpha a b)))
    (list n h))
  ((BETA A B) T))

(check-for-bug :section3-legacy-775
  (multiple-value-bind (n h)
      (macrolet ((alpha (x y) `(delta ,x ,y)))
        (flet ((alpha (x y) (+ x y)))
          (expand (alpha a b))))
    (list n h))
  ((ALPHA A B) nil))

(check-for-bug :section3-legacy-783
  (multiple-value-bind (n h)
      (let ((x (list 1 2 3)))
        (symbol-macrolet ((a (first x)))
            (let ((a x))
              (expand a))))
    (list n h))
  error
  "A has been declared special, thus SYMBOL-MACROLET may not bind it")

(check-for-bug :section3-legacy-793
  (multiple-value-bind (n h)
      (let ((x (list 1 2 3)))
        (symbol-macrolet ((a-new (first x)))
            (let ((a-new x))
              (expand a-new))))
    (list n h))
  (a-new nil))

;;; define-symbol-macro
(check-for-bug :section3-legacy-803
  (defvar *things* (list 'alpha 'beta 'gamma))
  *THINGS*)

(check-for-bug :section3-legacy-807
  (fboundp 'define-symbol-macro)
  T
  "The macro DEFINE-SYMBOL-MACRO should exist")

(check-for-bug :section3-legacy-812
  (define-symbol-macro thing1 (first *things*))
  THING1)

(check-for-bug :section3-legacy-816
  (define-symbol-macro thing2 (second *things*))
  THING2)

(check-for-bug :section3-legacy-820
  (define-symbol-macro thing3 (third *things*))
  THING3)

(check-for-bug :section3-legacy-824
  thing1
  ALPHA)

(check-for-bug :section3-legacy-828
  (setq thing1 'ONE)
  ONE)

(check-for-bug :section3-legacy-832
  *things*
  (ONE BETA GAMMA))

(check-for-bug :section3-legacy-836
  (multiple-value-setq (thing2 thing3) (values 'two 'three))
  TWO)

(check-for-bug :section3-legacy-840
  thing3
  THREE)

(check-for-bug :section3-legacy-844
  *things*
  (ONE TWO THREE))

(check-for-bug :section3-legacy-848
  (list thing2 (let ((thing2 2)) thing2))
  (TWO 2))

;;; *macrexpand-hook*

(check-for-bug :section3-legacy-854
  (defun hook (expander form env)
    (format t "Now expanding: ~S~%" form)
    (funcall expander form env))
  HOOK )

(check-for-bug :section3-legacy-860
  (defmacro machook (x y) `(/ (+ ,x ,y) 2))
  MACHOOK )

(check-for-bug :section3-legacy-864
  (macroexpand '(machook 1 2))
  (/ (+ 1 2) 2))                        ; true

(check-for-bug :section3-legacy-868
  (let ((*macroexpand-hook* #'hook)) (macroexpand '(machook 1 2)))
  (/ (+ 1 2) 2))                        ; true

;;; special opperator

(check-for-bug :section3-legacy-874
  (special-operator-p 'if)
  T)

(check-for-bug :section3-legacy-878
  (special-operator-p 'car)
  nil)

(check-for-bug :section3-legacy-882
  (special-operator-p 'one)
  nil)


(check-for-bug :section3-legacy-887
  (special-operator-p 'block)
  T)

(check-for-bug :section3-legacy-891
  (special-operator-p 'let*)
  T)

(check-for-bug :section3-legacy-895
  (special-operator-p 'return-from)
  T)

(check-for-bug :section3-legacy-899
  (special-operator-p 'catch)
  T)

(check-for-bug :section3-legacy-903
  (special-operator-p 'load-time-value)
  T)

(check-for-bug :section3-legacy-907
  (special-operator-p 'setq)
  T)

(check-for-bug :section3-legacy-911
  (special-operator-p 'eval-when)
  T)

(check-for-bug :section3-legacy-915
  (special-operator-p 'locally)
  T
  "locally is a special operator")

(check-for-bug :section3-legacy-920
  (special-operator-p 'symbol-macrolet)
  T)

(check-for-bug :section3-legacy-924
  (special-operator-p 'flet)
  T)

(check-for-bug :section3-legacy-928
  (special-operator-p 'macrolet)
  T)

(check-for-bug :section3-legacy-932
  (special-operator-p 'tagbody)
  T)

(check-for-bug :section3-legacy-936
  (special-operator-p 'function)
  T)

(check-for-bug :section3-legacy-940
  (special-operator-p 'multiple-value-call)
  T)

(check-for-bug :section3-legacy-944
  (special-operator-p 'the)
  T)

(check-for-bug :section3-legacy-948
  (special-operator-p 'go)
  T)

(check-for-bug :section3-legacy-952
  (special-operator-p 'multiple-value-prog1)
  T)

(check-for-bug :section3-legacy-956
  (special-operator-p 'throw)
  T)

(check-for-bug :section3-legacy-960
  (special-operator-p 'progn)
  T)

(check-for-bug :section3-legacy-964
  (special-operator-p 'unwind-protect)
  T)

(check-for-bug :section3-legacy-968
  (special-operator-p 'labels)
  T)

(check-for-bug :section3-legacy-972
  (special-operator-p 'progv)
  T)

(check-for-bug :section3-legacy-976
  (special-operator-p 'let)
  T)

(check-for-bug :section3-legacy-980
  (special-operator-p 'quote)
  T)

;;; constantp

(check-for-bug :section3-legacy-986
  (constantp 1)
  T)

(check-for-bug :section3-legacy-990
  (constantp 'temp)
  nil)

(check-for-bug :section3-legacy-994
  (constantp ''temp)
  t)

(check-for-bug :section3-legacy-998
  (defconstant this-is-a-constant 'never-changing)
  THIS-IS-A-CONSTANT )

(check-for-bug :section3-legacy-1002
  (constantp 'this-is-a-constant)
  t)

(check-for-bug :section3-legacy-1006
  (constantp "temp")
  t)

(check-for-bug :section3-legacy-1010
  (let ((a 6))
    a)
  6 )

(check-for-bug :section3-legacy-1015
  (let ((a 6))
    (constantp a))
  t)

(check-for-bug :section3-legacy-1020
  (constantp (values 37 Pi 'foo))
  #+(or cmu sbcl sbcl clisp ecls) t
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)


(check-for-bug :section3-legacy-1026
  (constantp '(sin pi))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(check-for-bug :section3-legacy-1031
  (constantp '(car '(x)))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(check-for-bug :section3-legacy-1036
  (constantp '(eql x x))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(check-for-bug :section3-legacy-1041
  (constantp '(typep x 'nil))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(check-for-bug :section3-legacy-1046
  (constantp '(typep x 't))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(check-for-bug :section3-legacy-1051
  (constantp '(values this-is-a-constant))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(check-for-bug :section3-legacy-1056
  (constantp '(values 'x 'y))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(check-for-bug :section3-legacy-1061
  (constantp '(let ((a '(a b c))) (+ (length a) 6)))
  #+(or cmu sbcl sbcl clisp ecls) nil
  #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)
