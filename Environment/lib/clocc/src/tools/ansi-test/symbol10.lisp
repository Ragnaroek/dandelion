;;; based on v1.5 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :symbol10-legacy-4
  (progn (in-package :cl-user) nil)
  nil
  "in-package expects a 'string designator'
this is or a character, a symbol or a string.")
;; test der neuen valuezelle

;;; 1. ungebundenes symbol


(check-for-bug :symbol10-legacy-14
  (defun testvar (var)
    (list (boundp var)			; gebunden
          (if (boundp var)
              (symbol-value var)
              nil)			; wert/nil
          (constantp var)		; konstante
          #+xcl
          (eq (sys::%p-get-cdr var 0)
              sys::%cdr-specsym)        ; specvar
          #+clisp
          (and (sys::special-variable-p var)
               (not (constantp var)))	; specvar
          #+allegro
          (and (not (constantp var))
               (eval `(let ((,var (list nil)))
                        (and (boundp ',var)
                             (eq (symbol-value ',var)
                                 ,var)))))
          #+cmu
          (eq (ext:info variable kind var)
              ':special);; like clisp
          #+ECL (and (sys::specialp var) (not (constantp var))) ; specvar
          #+ecls
          (si::specialp var)
          #+sbcl
          (eq (sb-int::info variable kind var)
              ':special);; like clisp
          (and (fboundp var) t)		; funktion. eigenschaft
          (and (fboundp var) (macro-function var) t) ; macro?
          (and (fboundp var)
               (special-operator-p var)
               t)			; spezialform?
          #-(or clisp ecl)
          (and (symbol-plist var) t)	; p-liste?
          #+(or clisp ecl)
          (and (or (get var 'i1)
                   (get var 'i2)
                   (get var 'i3))
               t)			; p-liste?
          (get var 'i1)			; i1
          (get var 'i2)			; i2
          (get var 'i3)			; i3
          )  )
  testvar)

(check-for-bug :symbol10-legacy-59
  (defun clrvar (var)
    #+xcl
    (subr 84				;sys::%p-set-cdr-content
          var 0 (sys::%p-get-content 'sys::%void-value 0) 0)
    #-xcl
    (progn (makunbound var) (fmakunbound var)
           (setf (symbol-plist var) '()))
    #+allegro
    (setf (excl::symbol-bit var 'excl::.globally-special.) nil)
    #+cmu
    (setf (ext:info variable kind var) ':global)
    #+sbcl
    (setf (sb-int::info variable kind var) ':global)
    var)
  clrvar)

#+(or xcl clisp allegro cmu sbcl)
(check-for-bug :symbol10-legacy-77
  (progn (setf (symbol-function 'setf-get)
               (symbol-function #+xcl 'sys::setf-get
                                #+clisp 'sys::%put
                                #+allegro 'excl::.inv-get
                                #+(or cmu sbcl) 'cl::%put)) t)
  t)

;;; begin breitentest

(check-for-bug :symbol10-legacy-87
  (clrvar 'v1)
  v1)

;;;; value - umbinden - macro - umbinden - props - umbinden

;;; value

(check-for-bug :symbol10-legacy-95
  (testvar 'v1)
  ;; geb val konst svar func mac spec plist i1  i2  i3
  (nil nil nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-100
  (setq v1 'val)
  val)

(check-for-bug :symbol10-legacy-104
  (testvar 'v1)
  ;; geb val konst svar func mac spec plist i1  i2  i3
  (t   val nil   nil  nil  nil nil  nil   nil nil nil))

;;; umbinden

(check-for-bug :symbol10-legacy-111
  (makunbound 'v1)
  v1)

(check-for-bug :symbol10-legacy-115
  (testvar 'v1)
  ;; geb val konst svar func mac spec plist i1  i2  i3
  (nil nil nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-120
  (setq v1 'val2)
  val2)

(check-for-bug :symbol10-legacy-124
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   val2 nil   nil  nil  nil nil  nil   nil nil nil))

;;; macro

(check-for-bug :symbol10-legacy-131
  (defmacro v1 (x) (list 'quote x))
  v1)

(check-for-bug :symbol10-legacy-135
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   val2 nil   nil  t    t   nil  nil   nil nil nil))

;;; umbinden

(check-for-bug :symbol10-legacy-142
  (fmakunbound 'v1)
  v1)

(check-for-bug :symbol10-legacy-146
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   val2 nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-151
  (defmacro v1 (x) (list 'quote (list x x)))
  v1)

(check-for-bug :symbol10-legacy-155
  (v1 33)
  (33 33))

(check-for-bug :symbol10-legacy-159
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   val2 nil   nil  t    t   nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-164
  (makunbound 'v1)
  v1)

(check-for-bug :symbol10-legacy-168
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  t    t   nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-173
  (setq v1 'val3)
  val3)

(check-for-bug :symbol10-legacy-177
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   val3 nil   nil  t    t   nil  nil   nil nil nil))

;;; props

(check-for-bug :symbol10-legacy-184
  (setf-get 'v1 'i1 11)
  11)

(check-for-bug :symbol10-legacy-188
  (setf-get 'v1 'i2 22)
  22)

(check-for-bug :symbol10-legacy-192
  (setf-get 'v1 'i3 33)
  33)

(check-for-bug :symbol10-legacy-196
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   val3 nil   nil  t    t   nil  t     11  22  33))

;;; umbinden

(check-for-bug :symbol10-legacy-203
  (not (null (remprop 'v1 'i2)))
  t)
(check-for-bug :symbol10-legacy-206
  (not (null (remprop 'v1 'i1)))
  t)
(check-for-bug :symbol10-legacy-209
  (not (null (remprop 'v1 'i3)))
  t)
(check-for-bug :symbol10-legacy-212
  (fmakunbound 'v1)
  v1)
(check-for-bug :symbol10-legacy-215
  (makunbound 'v1)
  v1)

(check-for-bug :symbol10-legacy-219
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-224
  (setf-get 'v1 'i1 99)
  99)
(check-for-bug :symbol10-legacy-227
  (defmacro v1 (x) (list 'quote (list x x x)))
  v1)
(check-for-bug :symbol10-legacy-230
  (v1 a)
  (a a a))
(check-for-bug :symbol10-legacy-233
  (setq v1 'val4)
  val4)

(check-for-bug :symbol10-legacy-237
  (testvar 'v1)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   val4 nil   nil  t    t   nil  t     99  nil nil))

;;; --- ende test1 -----

(check-for-bug :symbol10-legacy-244
  (clrvar 'v2)
  v2)

;;; specvar - props - rebind - function

(check-for-bug :symbol10-legacy-250
  (defvar v2 'v2a)
  v2)

(check-for-bug :symbol10-legacy-254
  (testvar 'v2)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   v2a  nil   t    nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-259
  (setf-get 'v2 'i3 33)
  33)
(check-for-bug :symbol10-legacy-262
  (setf-get 'v2 'i2 22)
  22)
(check-for-bug :symbol10-legacy-265
  (setf-get 'v2 'i1 11)
  11)

(check-for-bug :symbol10-legacy-269
  (testvar 'v2)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   v2a  nil   t    nil  nil nil  t     11  22  33))

;;; rebind

(check-for-bug :symbol10-legacy-276
  (makunbound 'v2)
  v2)
(check-for-bug :symbol10-legacy-279
  (not (null (remprop 'v2 'i1)))
  t)
(check-for-bug :symbol10-legacy-282
  (not (null (remprop 'v2 'i2)))
  t)
(check-for-bug :symbol10-legacy-285
  (not (null (remprop 'v2 'i3)))
  t)

(check-for-bug :symbol10-legacy-289
  (testvar 'v2)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  #+xcl
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil)
  #-xcl
  (nil nil  nil   t    nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-297
  (defvar v2 'v2b)
  v2)
(check-for-bug :symbol10-legacy-300
  (setf-get 'v2 'i1 111)
  111)
(check-for-bug :symbol10-legacy-303
  (setf-get 'v2 'i2 222)
  222)
(check-for-bug :symbol10-legacy-306
  (setf-get 'v2 'i3 333)
  333)

(check-for-bug :symbol10-legacy-310
  (testvar 'v2)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   v2b  nil   t    nil  nil nil  t     111 222 333))

;;; function

(check-for-bug :symbol10-legacy-317
  (defun v2 (x) (list x x))
  v2)
(check-for-bug :symbol10-legacy-320
  (v2 44)
  (44 44))

(check-for-bug :symbol10-legacy-324
  (testvar 'v2)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   v2b  nil   t    t    nil nil  t     111 222 333 ))


(check-for-bug :symbol10-legacy-330
  (clrvar 'v3)
  v3)

;;;;; function - con - rebind - prop

;;; function

(check-for-bug :symbol10-legacy-338
  (defun v3 (x y) (list x y))
  v3)

(check-for-bug :symbol10-legacy-342
  (testvar 'v3)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  t    nil nil  nil   nil nil nil))

;;; constant

(check-for-bug :symbol10-legacy-349
  (defconstant v3 99)
  v3)

(check-for-bug :symbol10-legacy-353
  v3
  99)
(check-for-bug :symbol10-legacy-356
  (v3 'a 'b)
  (a b))

(check-for-bug :symbol10-legacy-360
  (testvar 'v3)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t    99  t     nil  t    nil nil  nil   nil nil nil))

;;; rebind

(check-for-bug :symbol10-legacy-367
  (makunbound 'v3)
  #+(or xcl allegro cmu sbcl) v3
  #+(or clisp ecls) error
  #-(or xcl allegro cmu sbcl clisp ecls) unknown)

(check-for-bug :symbol10-legacy-372
  (fmakunbound 'v3)
  v3)

#+xcl
(check-for-bug :symbol10-legacy-377
  (testvar 'v3)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-382
  (defconstant v3 999)
  v3)

(check-for-bug :symbol10-legacy-386
  (defun v3 (x) (list x x))
  v3)

(check-for-bug :symbol10-legacy-390
  (v3 'c)
  (c c))

(check-for-bug :symbol10-legacy-394
  v3
  999)

(check-for-bug :symbol10-legacy-398
  (testvar 'v3)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   999  t     nil  t    nil nil  nil   nil nil nil))

;;;defparameter

(check-for-bug :symbol10-legacy-405
  (defparameter var33)
  error)

(check-for-bug :symbol10-legacy-409
  (defparameter var3 99)
  var3)

(check-for-bug :symbol10-legacy-413
  var3
  99)

(check-for-bug :symbol10-legacy-417
  (testvar 'var3)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t    99  nil   t    nil  nil nil  nil   nil nil nil))

;;; rebind

(check-for-bug :symbol10-legacy-424
  (makunbound 'var3)
  var3)

(check-for-bug :symbol10-legacy-428
  (testvar 'var3)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  #+xcl
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil)
  #-xcl
  (nil nil  nil   t    nil  nil nil  nil   nil nil nil))

;;; props

(check-for-bug :symbol10-legacy-438
  (setf-get 'v3 'i2 222)
  222)

(check-for-bug :symbol10-legacy-442
  (setf-get 'v3 'i1 111)
  111)

(check-for-bug :symbol10-legacy-446
  (testvar 'v3)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   999  t     nil  t    nil nil  t     111 222 nil))


(check-for-bug :symbol10-legacy-452
  (clrvar 'v4)
  v4)

;;;;  function - rebind - prop - rebind - specvar

(check-for-bug :symbol10-legacy-458
  (defun v4 (x) x)
  v4)

(check-for-bug :symbol10-legacy-462
  (v4 55)
  55)

(check-for-bug :symbol10-legacy-466
  (testvar 'v4)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  t    nil nil  nil   nil nil nil))

;;; rebind

(check-for-bug :symbol10-legacy-473
  (fmakunbound 'v4)
  v4)

(check-for-bug :symbol10-legacy-477
  (testvar 'v4)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-482
  (defun v4 (x) (list x))
  v4)

(check-for-bug :symbol10-legacy-486
  (v4 88)
  (88))

(check-for-bug :symbol10-legacy-490
  (testvar 'v4)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  t    nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-495
  (setf-get 'v4 'i1 11)
  11)

(check-for-bug :symbol10-legacy-499
  (setf-get 'v4 'i2 22)
  22)

(check-for-bug :symbol10-legacy-503
  (testvar 'v4)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  t    nil nil  t     11  22  nil))

;;; rebind

(check-for-bug :symbol10-legacy-510
  (fmakunbound 'v4)
  v4)
(check-for-bug :symbol10-legacy-513
  (not (null (remprop 'v4 'i1)))
  t)
(check-for-bug :symbol10-legacy-516
  (not (null (remprop 'v4 'i2)))
  t)
(check-for-bug :symbol10-legacy-519
  (testvar 'v4)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-524
  (defun v4 (x) (list x x x))
  v4)

(check-for-bug :symbol10-legacy-528
  (v4 44)
  (44 44 44))

(check-for-bug :symbol10-legacy-532
  (setf-get 'v4 'i2 222)
  222)

(check-for-bug :symbol10-legacy-536
  (setf-get 'v4 'i3 333)
  333)

(check-for-bug :symbol10-legacy-540
  (testvar 'v4)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  t    nil nil  t     nil 222 333))

(check-for-bug :symbol10-legacy-545
  (defvar v4 'v4-value)
  v4)

(check-for-bug :symbol10-legacy-549
  (testvar 'v4)
  ;; geb val     konst svar func mac spec plist i1  i2  i3
  (t  v4-value nil   t    t    nil nil  t     nil 222 333))

(check-for-bug :symbol10-legacy-554
  (clrvar 'v5)
  v5)

;;;;; prop - rebind - con - rebind - fun

(check-for-bug :symbol10-legacy-560
  (setf-get 'v5 'i1 1)
  1)
(check-for-bug :symbol10-legacy-563
  (setf-get 'v5 'i2 2)
  2)

(check-for-bug :symbol10-legacy-567
  (testvar 'v5)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  t     1   2  nil))

;;; rebind

(check-for-bug :symbol10-legacy-574
  (not (null (remprop 'v5 'i1)))
  t)
(check-for-bug :symbol10-legacy-577
  (not (null (remprop 'v5 'i2)))
  t)

(check-for-bug :symbol10-legacy-581
  (testvar 'v5)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(check-for-bug :symbol10-legacy-586
  (setf-get 'v5 'i1 11)
  11)
(check-for-bug :symbol10-legacy-589
  (setf-get 'v5 'i2 22)
  22)

(check-for-bug :symbol10-legacy-593
  (testvar 'v5)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  t     11  22  nil))

;;; con

(check-for-bug :symbol10-legacy-600
  (defconstant v5 '123)
  v5)

(check-for-bug :symbol10-legacy-604
  (testvar 'v5)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   123  t     nil  nil  nil nil  t     11  22  nil))

;;; rebind

(check-for-bug :symbol10-legacy-611
  (makunbound 'v5)
  #+(or xcl allegro cmu sbcl) v5
  #+(or clisp ecls) error  
  #-(or xcl allegro cmu sbcl clisp ecls) unknown)

(check-for-bug :symbol10-legacy-616
  (not (null (remprop 'v5 'i2)))
  t)

(check-for-bug :symbol10-legacy-620
  (not (null (remprop 'v5 'i1)))
  t)

#+xcl
(check-for-bug :symbol10-legacy-625
  (testvar 'v5)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

;;; das ging schief !!

(check-for-bug :symbol10-legacy-632
  (defconstant v5 321)
  v5)

(check-for-bug :symbol10-legacy-636
  (setf-get 'v5 'i3 333)
  333)

(check-for-bug :symbol10-legacy-640
  (setf-get 'v5 'i2 222)
  222)

(check-for-bug :symbol10-legacy-644
  (testvar 'v5)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   321  t     nil  nil  nil nil  t     nil 222 333))

(check-for-bug :symbol10-legacy-649
  (defun v5 (x) x)
  v5)

(check-for-bug :symbol10-legacy-653
  (v5 666)
  666)

(check-for-bug :symbol10-legacy-657
  (testvar 'v5)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   321  t     nil  t    nil nil  t     nil 222 333))

(check-for-bug :symbol10-legacy-662
  (clrvar 'v6)
  v6)

;;;;; prop mac con

(check-for-bug :symbol10-legacy-668
  (setf-get 'v6 'i1 1)
  1)

(check-for-bug :symbol10-legacy-672
  (setf-get 'v6 'i3 3)
  3)

(check-for-bug :symbol10-legacy-676
  (testvar 'v6)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  nil  nil nil  t     1   nil 3))

(check-for-bug :symbol10-legacy-681
  (defmacro v6 (x) (list 'quote x))
  v6)

(check-for-bug :symbol10-legacy-685
  (v6 a)
  a)

(check-for-bug :symbol10-legacy-689
  (testvar 'v6)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (nil nil  nil   nil  t    t   nil  t     1   nil 3))

(check-for-bug :symbol10-legacy-694
  (defconstant v6 234)
  v6)

(check-for-bug :symbol10-legacy-698
  (testvar 'v6)
  ;; geb val  konst svar func mac spec plist i1  i2  i3
  (t   234  t     nil  t    t   nil  t     1   nil 3))


;;  aufraeumen
(mapc #'unintern '(v1 v2 v3 v4 v5 v6))

