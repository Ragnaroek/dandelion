;;; based on v1.11 -*- mode: lisp -*-
(in-package :cl-user)


#-(OR CMU SBCL)
(check-for-bug :clos-legacy-6
  (use-package "CLOS")
  T)

#+SBCL
(check-for-bug :clos-legacy-11
  (use-package "SB-PCL")
  T)

(check-for-bug :clos-legacy-15
  (unintern '<C1>)
  T)

(check-for-bug :clos-legacy-19
  (progn
    (defclass <C1> ()
              ((x :initform 0 :accessor x-val :reader get-x :writer set-x :initarg :x)
               (y :initform 1 :accessor y-val :reader get-y :writer set-y :initarg :y)))
    ())
  NIL)

(check-for-bug :clos-legacy-27
  (progn
    (defclass <C2> (<C1>)
              ((z :initform 0 :accessor z-val :reader get-z :writer set-z :initarg :z)))
    ())
  NIL)

(check-for-bug :clos-legacy-34
  (defparameter a (make-instance (find-class '<C1>) :x 10))
  A)


(check-for-bug :clos-legacy-38
  (x-val a)
  10)

(check-for-bug :clos-legacy-42
  (y-val a)
  1)

(check-for-bug :clos-legacy-46
  (setf (x-val a) 20)
  20)

(check-for-bug :clos-legacy-50
  (x-val a)
  20)

(check-for-bug :clos-legacy-54
  (get-x a)
  20)

(check-for-bug :clos-legacy-58
  (set-x 10 a)
  10)

(check-for-bug :clos-legacy-62
  (x-val a)
  10)

(check-for-bug :clos-added-1
  (with-slots (x y) a (+ x y))
  11)

(check-for-bug :clos-added-2
  (defun foo (z)
    (with-slots (x y)
        z
      (+ x y)))
  foo)

(check-for-bug :clos-added-3
  (foo a)
  11)

(check-for-bug :clos-added-4
  (compile 'foo)
 foo)

(check-for-bug :clos-added-5
  (foo a)
  11)

(check-for-bug :clos-added-6
  (fmakunbound 'foo)
  foo)

(check-for-bug
    :clos-reinit-instance-x-20
  (x-val (reinitialize-instance a :x 20))
  20)

(check-for-bug
    :clos-reinit-instance-x-30
  (x-val (reinitialize-instance a :x 30))
  30)

(check-for-bug
    :clos-reinit-instance-x-50
  (x-val (reinitialize-instance a :x 50))
  50)

(check-for-bug
    :clos-reinit-instance-x-80
  (x-val (reinitialize-instance a :x 80))
  80)

(check-for-bug
    :clos-reinit-instance--y-20
  (x-val (reinitialize-instance a :y 20))
  80)

(check-for-bug
    :clos-reinit-instance-y-30
  (y-val (reinitialize-instance a :x 30))
  20)

(check-for-bug
    :clos-reinit-instance-y-50
  (x-val (reinitialize-instance a :y 50))
  30)

(check-for-bug
    :clos-reinit-instance-y-80
  (y-val (reinitialize-instance a :x 80))
  50)

(check-for-bug :clos-legacy-66
  (defparameter b (make-instance (find-class '<C2>) :x 10 :y 20 :z 30))
  B)

(check-for-bug :clos-legacy-70
  (x-val b)
  10)

(check-for-bug :clos-legacy-74
  (y-val b)
  20)

(check-for-bug :clos-legacy-78
  (z-val b)
  30)

(check-for-bug :clos-legacy-82
  (progn
    (defgeneric f (x y)
      (:method ((x t) (y t))
               (list x y)))
    (defmethod f ((i integer) (j number))
        (+ i j))
    (defmethod f ((s1 string) (s2 string))
        (concatenate 'string s1 s2))
    (lambda ()
      (defmethod f ((x list) (y list))
          (append x y)))
    ())
  NIL)

(check-for-bug :clos-legacy-94
  (f t t)
  (T T))

(check-for-bug :clos-legacy-98
  (f 2 3)
  5)

(check-for-bug :clos-legacy-102
  (f 2 3.0)
  5.0)

(check-for-bug :clos-legacy-106
  (f 2.0 3)
  (2.0 3))

(check-for-bug :clos-legacy-110
  (f "ab" "cd")
  "abcd")

(check-for-bug :clos-legacy-114
  (f 1 "abc")
  (1 "abc"))

(check-for-bug :clos-legacy-118
  (progn
    (defgeneric f (x y)
      (:method ((x t) (y t))
               (list x y))
      (:method ((i number) (j integer))
               (list (call-next-method) (- i j)))
      (:method ((i integer) (j number))
               (list (call-next-method) (+ i j))))
    ())
  NIL)

(check-for-bug :clos-legacy-130
  (f 'x 'y)
  (X Y))

(check-for-bug :clos-legacy-134
  (f 1 2)
  (((1 2) -1) 3))

(check-for-bug :clos-legacy-138
  (f 1 2.0)
  ((1 2.0) 3.0))

(check-for-bug :clos-legacy-142
  (f 1.0 2)
  ((1.0 2) -1.0))

(check-for-bug :clos-legacy-146
  (progn
    (defgeneric g (x)
      (:method ((x null))
               (cons 'null (call-next-method)))
      (:method ((x list))
               (if (next-method-p)
                   (cons 'list (call-next-method))
                   '(list$)))
      (:method ((x symbol))
               (if (next-method-p)
                   (cons 'symbol (call-next-method))
                   '(symbol$))))
    ())
  NIL)

(check-for-bug :clos-legacy-162
  (g 'x)
  (SYMBOL$))

(check-for-bug :clos-legacy-166
  (g '(x))
  (LIST$))

(check-for-bug :clos-legacy-170
  (g '())
  (NULL SYMBOL LIST$)
  "Class precedence list for NULL:

null, symbol, list, sequence, t")

(check-for-bug :clos-legacy-177
  (defvar hl)
  HL)

(check-for-bug :clos-legacy-181
  (progn
    (defgeneric hgen (x)
      (:method ((x integer))
               (setf hl (cons 'i-primary-1 hl))
               (call-next-method)
               (setf hl (cons 'i-primary-2 hl)))
      (:method :before ((x integer))
               (setf hl (cons 'i-before hl)))
      (:method :after ((x integer))
               (setf hl (cons 'i-after hl)))
      (:method :around ((x integer))
               (setf hl (cons 'i-around-1 hl))
               (call-next-method)
               (setf hl (cons 'i-around-2 hl)))
      (:method ((x number))
               (setf hl (cons 'n-primary-1 hl))
               (call-next-method)
               (setf hl (cons 'n-primary-2 hl)))
      (:method :before ((x number))
               (setf hl (cons 'n-before hl)))
      (:method :after ((x number))
               (setf hl (cons 'n-after hl)))
      (:method :around ((x number))
               (setf hl (cons 'n-around-1 hl))
               (call-next-method)
               (setf hl (cons 'n-around-2 hl)))
      (:method ((x t))
               (setf hl (cons 'innermost hl))))
    (defun h (x)
      (setf hl '()) (hgen x) (reverse hl))
    )
  H)

(check-for-bug :clos-legacy-215
  (h 'abc)
  (INNERMOST))

(check-for-bug :clos-legacy-219
  (h 3.14)
  (N-AROUND-1 N-BEFORE N-PRIMARY-1 INNERMOST N-PRIMARY-2 N-AFTER N-AROUND-2))

(check-for-bug :clos-legacy-223
  (h 3)
  (I-AROUND-1 N-AROUND-1 I-BEFORE N-BEFORE I-PRIMARY-1 N-PRIMARY-1 INNERMOST
              N-PRIMARY-2 I-PRIMARY-2 N-AFTER I-AFTER N-AROUND-2 I-AROUND-2
              ))

(check-for-bug :clos-legacy-229
  (unintern '<C1>)
  T)

(check-for-bug :clos-legacy-233
  (progn
    (defclass <C1> ()
              ((x :initform 0 :accessor x-val :initarg :x)
               (y :initform 1 :accessor y-val :initarg :y)))
    ())
  NIL)

(check-for-bug :clos-legacy-241
  (defparameter a (make-instance (find-class '<C1>) :x 10))
  A)

(check-for-bug :clos-legacy-245
  (defparameter b (make-instance (find-class '<C1>) :y 20 :x 10))
  B)

(check-for-bug :clos-legacy-249
  (defparameter c (make-instance (find-class '<C1>)))
  C)

(check-for-bug :clos-legacy-253
  (x-val a)
  10)

(check-for-bug :clos-legacy-257
  (y-val a)
  1)

(check-for-bug :clos-legacy-261
  (x-val b)
  10)

(check-for-bug :clos-legacy-265
  (y-val b)
  20)

(check-for-bug :clos-legacy-269
  (x-val c)
  0)

(check-for-bug :clos-legacy-273
  (y-val c)
  1)

(check-for-bug :clos-legacy-277
  (unintern '<C1>)
  T)

(check-for-bug :clos-legacy-281
  (progn
    (defclass <C1> ()
              ((x :initform 0 :accessor x-val :initarg :x)
               (y :initform 1 :accessor y-val :initarg :y)))
    (defmethod initialize-instance :after ((instance <C1>) &rest initvalues)
      (if (= (x-val instance) 0)
          (setf (x-val instance) (y-val instance))))
    ())
  NIL)

(check-for-bug :clos-legacy-292
  (x-val (make-instance (find-class '<C1>)))
  1)

(check-for-bug :clos-legacy-296
  (x-val (make-instance (find-class '<C1>) :x 10))
  10)

(check-for-bug :clos-legacy-300
  (x-val (make-instance (find-class '<C1>) :y 20))
  20)

(check-for-bug :clos-legacy-304
  (x-val (make-instance (find-class '<C1>) :x 10 :y 20))
  10)


(check-for-bug :clos-added-7
 (progn
   (defmethod initialize-instance ((inst <C1>) &rest ignore)
       (call-next-method)
     123)
   nil)
 nil)

(check-for-bug :clos-added-8
  (x-val (make-instance (find-class '<C1>) :x 101 :y 120))
  101)

(check-for-bug :clos-legacy-308
  (unintern '<C1>)
  T)

(check-for-bug :clos-legacy-312
  (subtypep (class-of ())
            (find-class 'null))
  T)

(check-for-bug :clos-legacy-316
  (subtypep (class-of t)
            (find-class 'symbol))
  T)

(check-for-bug :clos-legacy-320
  (subtypep (class-of 10)
            (find-class 'integer))
  T)

(check-for-bug :clos-legacy-326
  (subtypep (class-of 10.0)
            (find-class 'float))
  T)

(check-for-bug :clos-legacy-332
  (subtypep (class-of '(a b))
            (find-class 'cons))
  T)

(check-for-bug :clos-legacy-337
  (subtypep (class-of "abc")
            (find-class 'string))
  T)

(check-for-bug :clos-legacy-343
  (subtypep (class-of '#(1 2))
            (find-class 'vector))
  T)

(check-for-bug :clos-legacy-349
  (subtypep (class-of #'car)
            (find-class 'function))
  T)

(check-for-bug :clos-legacy-354
  (subtypep (class-of #'make-instance)
            (find-class 'standard-generic-function))
  T)

(check-for-bug :clos-legacy-359
  (subtypep (class-of '#2a((a) (b)))
            (find-class 'array))
  T)

(check-for-bug :clos-legacy-365
  (subtypep (class-of *standard-input*)
            (find-class 'stream))
  T)

(check-for-bug :clos-legacy-370
  (subtypep (class-of (lambda (x) x))
            (find-class 'function))
  T
  "lambda should return a function.

a function is:
function n. 1. an object representing code, which can
be called with zero or more arguments, and which produces
zero or more values. 2. an object of type function.

So class-of should return a function. Not?")

(check-for-bug :clos-legacy-383
  (subtypep (class-of (find-class 't))
            (find-class 'built-in-class))
  T)

(check-for-bug :clos-legacy-388
  (typep "abc" (find-class 't))
  T)

(check-for-bug :clos-legacy-392
  (typep "abc" (find-class 'array))
  T)

(check-for-bug :clos-legacy-396
  (typep "abc" (find-class 'vector))
  T)

(check-for-bug :clos-legacy-400
  (typep "abc" (find-class 'string))
  T)

(check-for-bug :clos-legacy-404
  (typep "abc" (find-class 'integer))
  NIL)

(check-for-bug :clos-legacy-408
  (typep 3 (find-class 't))
  T)

(check-for-bug :clos-legacy-412
  (typep 3 (find-class 'number))
  T)

(check-for-bug :clos-legacy-416
  (typep 3 (find-class 'float))
  NIL)

(check-for-bug :clos-legacy-420
  (typep 3 (find-class 'integer))
  T)

(check-for-bug :clos-legacy-424
  (typep 3 (find-class 'string))
  NIL)

(check-for-bug :clos-legacy-428
  (typep *standard-input* (find-class 'stream))
  T)

#+(or clisp allegro cmu sbcl)
(check-for-bug :clos-legacy-433
  #+CLISP
  (defun subclassp (class1 class2)
    (clos::subclassp class1 class2)
    )
  #+ALLEGRO
  (defun subclassp (class1 class2)
    (finalize-inheritance class1)
    (not (null (member class2 (class-precedence-list class1))))
    )
  #+CMU
  (defun subclassp (class1 class2)
    (not (null (member (car (pcl:class-precedence-list class2))
                       (pcl:class-precedence-list class1)
                       ) )    )     )
  #+sbcl
  (defun subclassp (class1 class2)
    (not (null (member (car (sb-pcl:class-precedence-list class2))
                       (sb-pcl:class-precedence-list class1)
                       ) )    )     )
  #+(or CLISP ALLEGRO cmu sbcl) SUBCLASSP)

(check-for-bug :clos-legacy-455
  (subclassp (find-class 'number)
             (find-class 't))
  T)

(check-for-bug :clos-legacy-460
  (subclassp (find-class 'integer)
             (find-class 'number))
  T)

(check-for-bug :clos-legacy-465
  (subclassp (find-class 'float)
             (find-class 'number))
  T)


;; make-load-form
;; from kmp

(check-for-bug :clos-make-load-form-test
  (progn
    (defclass test-class1 ()
              ((foo :initarg :foo :accessor foo :initform 0)))
    (defclass test-class2 ()
              ((foo :initarg :foo :accessor foo :initform 0)))
    (defmethod make-load-form ((obj test-class1) &optional environment)
        `(make-instance 'test-class1 :foo ',(foo obj)))
    (defparameter *t-list*
      (list (make-instance 'test-class1 :foo 100)
            (make-instance 'test-class2 :foo 200)))
    (let*
        ((lisp-file "make-load-form-demo.lisp")
         (compiled-file
          (compile-file
           (with-open-file (stream lisp-file
                                   :direction :output
                                   :if-exists :supersede)
             (format stream "(in-package \"CL-USER\")~
                                ~%(defparameter *t-list* '#.*t-list*)~%")
             (truename stream)))))
      (setq *t-list* '())
      (load compiled-file)
      (delete-file compiled-file)
      (delete-file lisp-file)
      #+clisp
      (delete-file (make-pathname :type "lib" :defaults lisp-file))
      (mapcar #'foo *t-list*)))
  (100 200))
