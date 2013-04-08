;;; section 10: symbols -*- mode: lisp -*-
(in-package :cl-user)

;;; symbolp

(check-for-bug :section10-legacy-6
  (symbolp 'elephant)
  t)


(check-for-bug :section10-legacy-11
  (symbolp 12)
  nil)


(check-for-bug :section10-legacy-16
  (symbolp nil)
  t)


(check-for-bug :section10-legacy-21
  (symbolp '())
  t)


(check-for-bug :section10-legacy-26
  (symbolp :test)
  t)


(check-for-bug :section10-legacy-31
  (symbolp "hello")
  nil)

;;; keywordp

(check-for-bug :section10-legacy-37
  (keywordp 'elephant)
  nil)


(check-for-bug :section10-legacy-42
  (keywordp 12)
  nil)


(check-for-bug :section10-legacy-47
  (keywordp :test)
  t)


(check-for-bug :section10-legacy-52
  (keywordp ':test)
  t)


(check-for-bug :section10-legacy-57
  (keywordp nil)
  nil)


(check-for-bug :section10-legacy-62
  (keywordp :nil)
  t)


(check-for-bug :section10-legacy-67
  (keywordp '(:test))
  nil)


(check-for-bug :section10-legacy-72
  (keywordp "hello")
  nil)


(check-for-bug :section10-legacy-77
  (keywordp ":hello")
  nil)


(check-for-bug :section10-legacy-82
  (keywordp '&optional)
  nil)

;;; make-symbol


(check-for-bug :section10-legacy-89
  (setq temp-string "temp")
  "temp")


(check-for-bug :section10-legacy-94
  (progn
    (setq temp-symbol (make-symbol temp-string))
    t)
  t)


(check-for-bug :section10-legacy-101
  (symbol-name temp-symbol)
  "temp")


(check-for-bug :section10-legacy-106
  (eq (symbol-name temp-symbol) temp-string)
  #+(or cmu sbcl clisp ecls) t
  #-(or cmu sbcl clisp ecls) fill-this-in)


(check-for-bug :section10-legacy-112
  (multiple-value-bind (a b)
      (find-symbol "temp")
    (list a b))
  ( NIL NIL))


(check-for-bug :section10-legacy-119
  (eq (make-symbol temp-string) (make-symbol temp-string))
  nil)

;;; copy-symbol


(check-for-bug :section10-legacy-126
  (setq fred 'fred-smith)
  FRED-SMITH)


(check-for-bug :section10-legacy-131
  (setf (symbol-value fred) 3)
  3)


(check-for-bug :section10-legacy-136
  (progn
    (setq fred-clone-1a (copy-symbol fred nil))
    t)
  t)


(check-for-bug :section10-legacy-143
  (progn
    (setq fred-clone-1b (copy-symbol fred nil))
    t)
  t)


(check-for-bug :section10-legacy-150
  (progn
    (setq fred-clone-2a (copy-symbol fred t))
    t)
  t)


(check-for-bug :section10-legacy-157
  (progn
    (setq fred-clone-2b (copy-symbol fred t))
    t)
  t)


(check-for-bug :section10-legacy-164
  (eq fred fred-clone-1a)
  nil)


(check-for-bug :section10-legacy-169
  (eq fred-clone-1a fred-clone-1b)
  nil)


(check-for-bug :section10-legacy-174
  (eq fred-clone-2a fred-clone-2b)
  nil)


(check-for-bug :section10-legacy-179
  (eq fred-clone-1a fred-clone-2a)
  nil)


(check-for-bug :section10-legacy-184
  (symbol-value fred)
  3)


(check-for-bug :section10-legacy-189
  (boundp fred-clone-1a)
  nil)


(check-for-bug :section10-legacy-194
  (symbol-value fred-clone-2a)
  3)


(check-for-bug :section10-legacy-199
  (setf (symbol-value fred-clone-2a) 4)
  4)


(check-for-bug :section10-legacy-204
  (symbol-value fred)
  3)


(check-for-bug :section10-legacy-209
  (symbol-value fred-clone-2a)
  4)


(check-for-bug :section10-legacy-214
  (symbol-value fred-clone-2b)
  3)


(check-for-bug :section10-legacy-219
  (boundp fred-clone-1a)
  nil)


(check-for-bug :section10-legacy-224
  (progn
    (setf (symbol-function fred) #'(lambda (x) x))
    t)
  t)


(check-for-bug :section10-legacy-231
  (fboundp fred)
  t)


(check-for-bug :section10-legacy-236
  (fboundp fred-clone-1a)
  nil)


(check-for-bug :section10-legacy-241
  (fboundp fred-clone-2a)
  nil)

;;; symbol-function


(check-for-bug :section10-legacy-248
  (progn
    (symbol-function 'car)
    t)
  t)

(check-for-bug :section10-legacy-254
  (symbol-function 'twice)
  UNDEFINED-FUNCTION)


(check-for-bug :section10-legacy-259
  (defun twice (n) (* n 2))
  TWICE)


(check-for-bug :section10-legacy-264
  (progn
    (symbol-function 'twice)
    t)
  t)


(check-for-bug :section10-legacy-271
  (list (twice 3)
        (funcall (function twice) 3)
        (funcall (symbol-function 'twice) 3))
  (6 6 6))


(check-for-bug :section10-legacy-278
  (flet ((twice (x) (list x x)))
    (list (twice 3)
          (funcall (function twice) 3)
          (funcall (symbol-function 'twice) 3)))
  ((3 3) (3 3) 6)   )


(check-for-bug :section10-legacy-286
  (progn
    (setf (symbol-function 'twice) #'(lambda (x) (list x x)))
    t)
  t)


(check-for-bug :section10-legacy-293
  (list (twice 3)
        (funcall (function twice) 3)
        (funcall (symbol-function 'twice) 3))
  ((3 3) (3 3) (3 3)))


(check-for-bug :section10-legacy-300
  (fboundp 'defun)
  t)


(check-for-bug :section10-legacy-305
  (progn
    (symbol-function 'defun)
    t)
  t)


(check-for-bug :section10-legacy-312
  (progn
    (functionp (symbol-function 'defun))
    t)
  t)


(check-for-bug :section10-legacy-319
  (defun symbol-function-or-nil (symbol)
    (if (and (fboundp symbol)
             (not (macro-function symbol))
             (not (special-operator-p symbol)))
        (symbol-function symbol)
        nil))
  SYMBOL-FUNCTION-OR-NIL)


(check-for-bug :section10-legacy-329
  (progn
    (symbol-function-or-nil 'car)
    t)
  t)


(check-for-bug :section10-legacy-336
  (symbol-function-or-nil 'defun)
  NIL)

;;; symbol-name


(check-for-bug :section10-legacy-343
  (symbol-name 'temp)
  "TEMP" )


(check-for-bug :section10-legacy-348
  (symbol-name :start)
  "START")

;;; symbol-package


(check-for-bug :section10-legacy-355
  (progn
    (in-package "CL-USER")
    t)
  t)



(check-for-bug :section10-legacy-363
  (progn
    (symbol-package 'car)
    t)
  t)



(check-for-bug :section10-legacy-371
  (progn
    (symbol-package 'bus)
    t)
  t)


(check-for-bug :section10-legacy-378
  (progn
    (symbol-package :optional)
    t)
  t)


;; Gensyms are uninterned, so have no home package.

(check-for-bug :section10-legacy-387
  (symbol-package (gensym))
  NIL)


(if (find-package "PK2")
    (delete-package
     (find-package "PK2")))

(if (find-package "PK1")
    (delete-package
     (find-package "PK1")))

(check-for-bug :section10-legacy-400
  (find-package "PK1")
  nil)

(check-for-bug :section10-legacy-404
  (progn
    (make-package 'pk1)
    t)
  t)


(check-for-bug :section10-legacy-411
  (multiple-value-bind (a b)
      (intern "SAMPLE1" "PK1")
    (list a b))
  (PK1::SAMPLE1 :internal))


(check-for-bug :section10-legacy-418
  (export (find-symbol "SAMPLE1" "PK1") "PK1")
  T)


(check-for-bug :section10-legacy-423
  (progn
    (make-package 'pk2 :use '(pk1))
    t)
  t)


(check-for-bug :section10-legacy-430
  (multiple-value-bind (a b)
      (find-symbol "SAMPLE1" "PK2")
    (list a b))
  (PK1:SAMPLE1 :INHERITED))


(check-for-bug :section10-legacy-437
  (progn
    (symbol-package 'pk1::sample1)
    t)
  t)


(check-for-bug :section10-legacy-444
  (progn
    (symbol-package 'pk2::sample1)
    t)
  t)


(check-for-bug :section10-legacy-451
  (progn
    (symbol-package 'pk1::sample2)
    t)
  t)


(check-for-bug :section10-legacy-458
  (progn
    (symbol-package 'pk2::sample2)
    t)
  t)

;; The next several forms create a scenario in which a symbol
;; is not really uninterned, but is "apparently uninterned",
;; and so SYMBOL-PACKAGE still returns NIL.

(check-for-bug :section10-legacy-468
  (setq s3 'pk1::sample3)
  PK1::SAMPLE3)


(check-for-bug :section10-legacy-473
  (import s3 'pk2)
  T)


(check-for-bug :section10-legacy-478
  (unintern s3 'pk1)
  T)


(check-for-bug :section10-legacy-483
  (symbol-package s3)
  NIL)


(check-for-bug :section10-legacy-488
  (eq s3 'pk2::sample3)
  T)

;;; symbol-plist


(setq sym (gensym))

(check-for-bug :section10-legacy-497
  (symbol-plist sym)
  ())


(check-for-bug :section10-legacy-502
  (setf (get sym 'prop1) 'val1)
  VAL1)


(check-for-bug :section10-legacy-507
  (symbol-plist sym)
  (PROP1 VAL1))


(check-for-bug :section10-legacy-512
  (setf (get sym 'prop2) 'val2)
  VAL2)


(check-for-bug :section10-legacy-517
  (symbol-plist sym)
  (PROP2 VAL2 PROP1 VAL1))


(check-for-bug :section10-legacy-522
  (setf (symbol-plist sym) (list 'prop3 'val3))
  (PROP3 VAL3))


(check-for-bug :section10-legacy-527
  (symbol-plist sym)
  (PROP3 VAL3))

;;; setf


(check-for-bug :section10-legacy-534
  (setf (symbol-value 'a) 1)
  1)


(check-for-bug :section10-legacy-539
  (symbol-value 'a)
  1)

;; SYMBOL-VALUE can see dynamic variables.

(check-for-bug :section10-legacy-545
  (let ((a 2))
    (declare (special a))
    (symbol-value 'a))
  2)


(check-for-bug :section10-legacy-552
  (let ((a 2))
    (declare (special a))
    (setq a 3)
    (symbol-value 'a))
  3)


(check-for-bug :section10-legacy-560
  (let ((a 2))
    (setf (symbol-value 'a) 3)
    t)
  t)


					;(check-for-bug :section10-legacy-567
					;a
					;3)


					;(check-for-bug :section10-legacy-572
					;(symbol-value 'a)
					;3)


(check-for-bug :section10-legacy-577
  (multiple-value-bind (h j)
      (let ((a 4))
        (declare (special a))
        (let ((b (symbol-value 'a)))
          (setf (symbol-value 'a) 5)
          (values a b)))
    (list h j))
  (5 4))


					;(check-for-bug :section10-legacy-588
					;a
					;3)


(check-for-bug :section10-legacy-593
  (symbol-value :any-keyword)
  :ANY-KEYWORD)


(check-for-bug :section10-legacy-598
  (symbol-value 'nil)
  NIL)


(check-for-bug :section10-legacy-603
  (symbol-value '())
  NIL)

;; The precision of this next one is implementation-dependent.

(check-for-bug :section10-legacy-609
  (symbol-value 'pi)
  #-clisp
  3.141592653589793d0
  #+clisp
  3.1415926535897932385L0)

;;; get


(check-for-bug :section10-legacy-619
  (defun make-person (first-name last-name)
    (let ((person (gensym "PERSON")))
      (setf (get person 'first-name) first-name)
      (setf (get person 'last-name) last-name)
      person))
  MAKE-PERSON)


(check-for-bug :section10-legacy-628
  (defvar *john* (make-person "John" "Dow"))
  *JOHN*)


(check-for-bug :section10-legacy-633
  (progn
    *john*
    t)
  t)


(check-for-bug :section10-legacy-640
  (defvar *sally* (make-person "Sally" "Jones"))
  *SALLY*)


(check-for-bug :section10-legacy-645
  (get *john* 'first-name)
  "John")


(check-for-bug :section10-legacy-650
  (get *sally* 'last-name)
  "Jones")


(check-for-bug :section10-legacy-655
  (defun marry (man woman married-name)
    (setf (get man 'wife) woman)
    (setf (get woman 'husband) man)
    (setf (get man 'last-name) married-name)
    (setf (get woman 'last-name) married-name)
    married-name)
  MARRY)


(check-for-bug :section10-legacy-665
  (marry *john* *sally* "Dow-Jones")
  "Dow-Jones")


(check-for-bug :section10-legacy-670
  (get *john* 'last-name)
  "Dow-Jones")


(check-for-bug :section10-legacy-675
  (get (get *john* 'wife) 'first-name)
  "Sally")


(check-for-bug :section10-legacy-680
  (progn
    (symbol-plist *john*)
    t)
  t)


(check-for-bug :section10-legacy-687
  (defmacro age (person &optional (default ''thirty-something))
    `(get ,person 'age ,default))
  AGE)


(check-for-bug :section10-legacy-693
  (age *john*)
  THIRTY-SOMETHING)


(check-for-bug :section10-legacy-698
  (age *john* 20)
  20)


(check-for-bug :section10-legacy-703
  (setf (age *john*) 25)
  25)


(check-for-bug :section10-legacy-708
  (age *john*)
  25)


(check-for-bug :section10-legacy-713
  (age *john* 20)
  25)

;;; remprop


(check-for-bug :section10-legacy-720
  (progn
    (setq test (make-symbol "PSEUDO-PI"))
    t)
  t)


(check-for-bug :section10-legacy-727
  (symbol-plist test)
  ())


(check-for-bug :section10-legacy-732
  (setf (get test 'constant) t)
  T)


(check-for-bug :section10-legacy-737
  (setf (get test 'approximation) 3.14)
  3.14)


(check-for-bug :section10-legacy-742
  (setf (get test 'error-range) 'noticeable)
  NOTICEABLE)


(check-for-bug :section10-legacy-747
  (symbol-plist test)
  (ERROR-RANGE NOTICEABLE APPROXIMATION 3.14 CONSTANT T))


(check-for-bug :section10-legacy-752
  (setf (get test 'approximation) nil)
  NIL)


(check-for-bug :section10-legacy-757
  (symbol-plist test)
  (ERROR-RANGE NOTICEABLE APPROXIMATION NIL CONSTANT T))


(check-for-bug :section10-legacy-762
  (get test 'approximation)
  NIL)


(check-for-bug :section10-legacy-767
  (not (remprop test 'approximation))
  nil)


(check-for-bug :section10-legacy-772
  (get test 'approximation)
  NIL)


(check-for-bug :section10-legacy-777
  (symbol-plist test)
  (ERROR-RANGE NOTICEABLE CONSTANT T))


(check-for-bug :section10-legacy-782
  (remprop test 'approximation)
  NIL)


(check-for-bug :section10-legacy-787
  (symbol-plist test)
  (ERROR-RANGE NOTICEABLE CONSTANT T))


(check-for-bug :section10-legacy-792
  (not (remprop test 'error-range))
  nil)


(check-for-bug :section10-legacy-797
  (setf (get test 'approximation) 3)
  3)


(check-for-bug :section10-legacy-802
  (symbol-plist test)
  (APPROXIMATION 3 CONSTANT T))


;;; boundp


(check-for-bug :section10-legacy-810
  (setq x 1)
  1)


(check-for-bug :section10-legacy-815
  (boundp 'x)
  t)


(check-for-bug :section10-legacy-820
  (makunbound 'x)
  X)


(check-for-bug :section10-legacy-825
  (boundp 'x)
  nil)



(check-for-bug :section10-legacy-831
  (let ((x 2)) (declare (special x)) (boundp 'x))
  t)

;;; mkunbound


(check-for-bug :section10-legacy-838
  (setf (symbol-value 'a) 1)
  1)


(check-for-bug :section10-legacy-843
  (boundp 'a)
  t)


(check-for-bug :section10-legacy-848
  a
  1)


(check-for-bug :section10-legacy-853
  (makunbound 'a)
  A)


(check-for-bug :section10-legacy-858
  (boundp 'a)
  nil)

;;; set


(check-for-bug :section10-legacy-865
  (setf (symbol-value 'n) 1)
  1)


(check-for-bug :section10-legacy-870
  (set 'n 2)
  2)


(check-for-bug :section10-legacy-875
  (symbol-value 'n)
  2)


					;(check-for-bug :section10-legacy-880
					;(let ((n 3))
					;  (declare (special n))
					;  (setq n (+ n 1))
					;  (setf (symbol-value 'n) (* n 10))
					;  (set 'n (+ (symbol-value 'n) n))
					;   n)
					;80)


					;(check-for-bug :section10-legacy-890
					;n
					;2)


					;(check-for-bug :section10-legacy-895
					;(let ((n 3))
					;  (setq n (+ n 1))
					;  (setf (symbol-value 'n) (* n 10))
					;  (set 'n (+ (symbol-value 'n) n))
					;  n)
					;4)


					;(check-for-bug :section10-legacy-904
					;n
					;44)


(check-for-bug :section10-legacy-909
  (defvar *n* 2)
  *N*)


(check-for-bug :section10-legacy-914
  (let ((*n* 3))
    (setq *n* (+ *n* 1))
    (setf (symbol-value '*n*) (* *n* 10))
    (set '*n* (+ (symbol-value '*n*) *n*))
    *n*)
  80)


(check-for-bug :section10-legacy-923
  *n*
  2)


(check-for-bug :section10-legacy-928
  (defvar *even-count* 0)
  *EVEN-COUNT*)


(check-for-bug :section10-legacy-933
  (defvar *odd-count* 0)
  *ODD-COUNT*)


(check-for-bug :section10-legacy-938
  (defun tally-list (list)
    (dolist (element list)
      (set (if (evenp element) '*even-count* '*odd-count*)
           (+ element (if (evenp element) *even-count* *odd-count*)))))
  tally-list)


(check-for-bug :section10-legacy-946
  (tally-list '(1 9 4 3 2 7))
  NIL)


(check-for-bug :section10-legacy-951
  *even-count*
  6)



(check-for-bug :section10-legacy-957
  *odd-count*
  20)

