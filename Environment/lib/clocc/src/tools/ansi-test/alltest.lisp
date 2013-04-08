;;; based on v1.19 -*- mode: lisp -*-
;;*****************************************************************************
;;*                    short test      XCL                                    *
;;*****************************************************************************

;; Chapter 1  Introduction
;; -----------------------

;; Chapter 2  Data Types
;; ---------------------

;; Chapter 3  Valid Values
;; -----------------------

;; Chapter 4  Type specifiers
;; --------------------------
;; deftype, COERCE, TYPE-OF

;; Chapter 5  Program Structure
;; ----------------------------


;; lambda lists

(in-package :cl-user)

(check-for-bug :alltest-legacy-22
  ((lambda (a b)
     (+ a (* b 3)))
   4 5)
  19)

(check-for-bug :alltest-legacy-28
  ((lambda (a &optional (b 2))
     (+ a (* b 3)))
   4 5)
  19)

(check-for-bug :alltest-legacy-34
  ((lambda (&optional (a 2 b) (c 3 d) &rest x)
     (list a b c d x)))
  (2 nil 3 nil nil))

(check-for-bug :alltest-legacy-39
  ((lambda (a b &key c d)
     (list a b c d))
   1 2)
  (1 2 nil nil))

(check-for-bug :alltest-legacy-45
  ((lambda (a &optional (b 3) &rest x &key c (d a))
     (list a b c d x))
   1)
  (1 3 nil 1 nil))

(check-for-bug :alltest-legacy-51
  ((lambda (x &aux (a 3) (b 4))
     (+ x (* a b)))
   2)
  14)

(check-for-bug :alltest-legacy-57
  ((lambda (x y &optional a b &rest z &key c (d y) &aux (u 3) (v 4))
     (+ x y a (* b (car z)) c (* d u) v))
   3 4 5 2 7 :c 6 :d 8)
  error)

(check-for-bug :alltest-legacy-63
  ((lambda (x y)
     ((lambda (a b)
        (list a b))
      'u 'v))
   5 6)
  (u v))

(check-for-bug :alltest-legacy-71
  ((lambda (x &allow-other-keys)
     (list x y))
   2 :y 3)
  error)

(check-for-bug :alltest-legacy-77
  lambda-list-keywords
  #+xcl (&optional &rest &key &allow-other-keys &aux &body &whole system::&environment)
  #+clisp (&optional &rest &key &allow-other-keys &aux &body &whole &environment)
  #+(or akcl ecls)
  (&optional &rest &key &allow-other-keys &aux &whole &environment &body)
  #+(or allegro cmu sbcl)
  (&optional &rest &key &aux &body &whole &allow-other-keys &environment)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-87
  (let ((s (prin1-to-string lambda-parameters-limit )))
    (or #+xcl (equal s "128")
        #+clisp (equal s "65536")
        #+clisp (equal s "4294967296")
        #+clisp (equal s "4096")
        #+akcl (equal s "64")
        #+allegro (equal s "16384")
        #+(or cmu sbcl sbcl) (equal s "536870911")
        ) )
  t)

;; defvar, defconstant, defparameter, eval-when

;; kap 6 praedikate
;; ----------------------------------------------------------------------------

(check-for-bug :alltest-legacy-103
  (typep 'nil 'null)
  t)

(check-for-bug :alltest-legacy-107
  (typep (list 'a 'b 'c) 'null)
  nil)

(check-for-bug :alltest-legacy-111
  (typep 'abc 'symbol)
  t)

(check-for-bug :alltest-legacy-115
  (typep 4 'atom)
  t)

(check-for-bug :alltest-legacy-119
  (typep 55 'cons)
  nil)

(check-for-bug :alltest-legacy-123
  (typep (list 'a (list 'b 'c)) 'list)
  t)

(check-for-bug :alltest-legacy-127
  (typep 5/8 'number)
  t)

(check-for-bug :alltest-legacy-131
  (typep -800 'integer)
  t)

(check-for-bug :alltest-legacy-135
  (typep 5/7 'rational)
  t)

(check-for-bug :alltest-legacy-139
  (typep 2.718 'float)
  t)

(check-for-bug :alltest-legacy-143
  (typep #c(1.23 3.56) 'float)
  nil)

(check-for-bug :alltest-legacy-147
  (typep #\a 'character)
  t)

(check-for-bug :alltest-legacy-151
  (typep "abc" 'string)
  t)

(check-for-bug :alltest-legacy-155
  (typep '#(1 2 3) 'string)
  nil)

(check-for-bug :alltest-legacy-159
  (typep '#(a b c) 'bit-vector)
  nil)

(check-for-bug :alltest-legacy-163
  (typep '#(a b c) 'vector)
  t)

(check-for-bug :alltest-legacy-167
  (typep "abc" 'vector)
  t)

(check-for-bug :alltest-legacy-171
  (typep '#(1 2 3 4) 'simple-vector)
  t)

(check-for-bug :alltest-legacy-175
  (typep 3 'simple-vector)
  nil)

(check-for-bug :alltest-legacy-179
  (typep "a b cd" 'simple-string)
  t)

(check-for-bug :alltest-legacy-183
  (typep 'abc 'simple-string)
  nil)

(check-for-bug :alltest-legacy-187
  (typep #*1101 'simple-bit-vector)
  t)

(check-for-bug :alltest-legacy-191
  (typep '#(1 0 0 1) 'simple-bit-vector)
  nil)

(check-for-bug :alltest-legacy-195
  (typep '#2a((a b)(c d)) 'array)
  t)

(check-for-bug :alltest-legacy-199
  (setq x 7)
  7)

(check-for-bug :alltest-legacy-203
  (typep x 'compiled-function)
  nil)

(check-for-bug :alltest-legacy-207
  (typep x 'common)
  error)

(unintern 'x)

(check-for-bug :alltest-legacy-213
  (subtypep 'character 'number)
  nil)

(check-for-bug :alltest-legacy-217
  (subtypep 'number 'character)
  nil)

(check-for-bug :alltest-legacy-221
  (subtypep 'string 'number)
  nil)

(check-for-bug :alltest-legacy-225
  (subtypep 'complex 'number)
  t)

(check-for-bug :alltest-legacy-229
  (subtypep 'float 'number)
  t)

(check-for-bug :alltest-legacy-233
  (subtypep 'fixnum 'number)
  t)

(check-for-bug :alltest-legacy-237
  (subtypep 'rational 'number)
  t)

(check-for-bug :alltest-legacy-241
  (subtypep 'float 'complex)
  nil)

(check-for-bug :alltest-legacy-245
  (subtypep 'integer 'rational)
  t)

(check-for-bug :alltest-legacy-249
  (subtypep 'number 'vector)
  nil)

(check-for-bug :alltest-legacy-253
  (subtypep 'vector 'array)
  t)

(check-for-bug :alltest-legacy-257
  (subtypep 'number 'array)
  nil)

(check-for-bug :alltest-legacy-261
  (null 'nil)
  t)

(check-for-bug :alltest-legacy-265
  (symbolp *standard-input*)
  nil)

(check-for-bug :alltest-legacy-269
  (symbolp 'car)
  t)

(check-for-bug :alltest-legacy-273
  (atom 'abc)
  t)

(check-for-bug :alltest-legacy-277
  (consp (acons 'x 'y 'a))
  #+xcl error
  #+(or clisp akcl allegro cmu sbcl sbcl ecls) t
  #-(or xcl clisp akcl allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-283
  (listp (list (append (cons 'a 'b) 'c)))
  t)

(check-for-bug :alltest-legacy-287
  (listp 'a)
  nil)

(check-for-bug :alltest-legacy-291
  (listp nil)
  t)

(check-for-bug :alltest-legacy-295
  (listp '(a b c))
  t)

(check-for-bug :alltest-legacy-299
  (numberp #*101)
  nil)

(check-for-bug :alltest-legacy-303
  (numberp -5)
  t)

(check-for-bug :alltest-legacy-307
  (integerp 5)
  t)

(check-for-bug :alltest-legacy-311
  (integerp #\+)
  nil)

(check-for-bug :alltest-legacy-315
  (rationalp 0)
  t)

(check-for-bug :alltest-legacy-319
  (floatp -5)
  nil)

(check-for-bug :alltest-legacy-323
  (floatp (read-from-string "1.0e30"))
  t)

(check-for-bug :alltest-legacy-327
  (floatp 123.4)
  t)

(check-for-bug :alltest-legacy-331
  (complexp 1/2)
  nil)

(check-for-bug :alltest-legacy-335
  (complexp #c(2 3))
  t)

(check-for-bug :alltest-legacy-339
  (characterp #\1)
  t)

(check-for-bug :alltest-legacy-343
  (stringp "abc")
  t)

(check-for-bug :alltest-legacy-347
  (stringp :+*/-)
  nil)

(check-for-bug :alltest-legacy-351
  (bit-vector-p (read-from-string "#5*01110"))
  t)

(check-for-bug :alltest-legacy-355
  (vectorp "abc")
  t)

(check-for-bug :alltest-legacy-359
  (simple-vector-p #*101)
  nil)

(check-for-bug :alltest-legacy-363
  (simple-string-p "abc")
  t)

(check-for-bug :alltest-legacy-367
  (simple-string-p :+*/-)
  nil)

(check-for-bug :alltest-legacy-371
  (simple-bit-vector-p #*101)
  t)

(check-for-bug :alltest-legacy-375
  (arrayp (read-from-string "#7(2 4 3)"))
  t)

(check-for-bug :alltest-legacy-379
  (arrayp '(read-from-string "#1a 5.77"))
  nil)

(check-for-bug :alltest-legacy-383
  (packagep (read-from-string "#5*01110"))
  nil)

(check-for-bug :alltest-legacy-387
  (packagep *package*)
  t)

(check-for-bug :alltest-legacy-391
  (functionp 'atom)
  #-(or cltl2 clisp) t
  #+(or cltl2 clisp) nil)

(check-for-bug :alltest-legacy-396
  (compiled-function-p 'do)
  nil)

;; commonp

(check-for-bug :alltest-legacy-402
  (eq (list 1 2 3 4 5)
      (copy-list (list 1 2 3 4 5)))
  nil)

(check-for-bug :alltest-legacy-407
  (setq x (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) )
  ((1 . a) (2 . b) (3 . c)))

(check-for-bug :alltest-legacy-411
  (eq (cadr x) (cadr (copy-alist x)))
  nil)

(unintern 'x)

(check-for-bug :alltest-legacy-417
  (eq #\a #\a)
  t)

(check-for-bug :alltest-legacy-421
  (booleanp (eq "Foo" "Foo"))
  t)

(check-for-bug :alltest-legacy-425
  (eq "Foo" (copy-seq "Foo"))
  nil)

(check-for-bug :alltest-legacy-429
  (eql #c(3.0 -4.0) #c(3 -4))
  nil)

(check-for-bug :alltest-legacy-433
  (eql (cons 'a 'b) (cons 'a 'c))
  nil)

(check-for-bug :alltest-legacy-437
  (equal (list 1 2 3 4 5) (copy-list (list 1 2 3 4 5)))
  t)

(check-for-bug :alltest-legacy-441
  (equal x (copy-alist x))
  t)

(check-for-bug :alltest-legacy-445
  (equal 3 3)
  t)

(check-for-bug :alltest-legacy-449
  (equal 3 3.0)
  nil)

(check-for-bug :alltest-legacy-453
  (equal 3.0 3.0)
  t)

(check-for-bug :alltest-legacy-457
  (equal #c(3 -4) #c(3 -4))
  t)

(check-for-bug :alltest-legacy-461
  (equalp (list 1 2 3 4 5) (copy-list (list 1 2 3 4 5)))
  t)

(check-for-bug :alltest-legacy-465
  (equalp "            foo" "            FOO")
  t)

(check-for-bug :alltest-legacy-469
  (equalp "            fou" "            FOO")
  nil)

(check-for-bug :alltest-legacy-473
  (not 1)
  nil)

(check-for-bug :alltest-legacy-477
  (not nil)
  t)

(check-for-bug :alltest-legacy-481
  (and (eq 1 2) (eq 2 3) (eq 3 4) (eq 4 4))
  nil)

(check-for-bug :alltest-legacy-485
  (and (eq 1 2) (eq 3 3) (eq 3 4) (eq 4 4))
  nil)

(check-for-bug :alltest-legacy-489
  (or (eq 2 2) (eq 3 3) (eq 3 4) (eq 4 4))
  t)

(check-for-bug :alltest-legacy-493
  (or (eq 1 2) (eq 2 3) (eq 3 4) (eq 4 5))
  nil)

;; kap 7 kontrollstructuren
;; ----------------------------------------------------------------------------

;;  quote, function, symbol-value, symbol-function, boundp, fboundp,
;;  special-form-p, setq, psetq, set, makunbound, fmakunbound,

(check-for-bug :alltest-legacy-503
  (setq li1 (list 'a (list 'b)
                  (list (list 'c)
                        (list 'd))))
  (a (b) ((c) (d))))

(check-for-bug :alltest-legacy-509
  (setq vec1 (vector 0 1 2 3))
  #(0 1 2 3))

(check-for-bug :alltest-legacy-513
  (setf (nth 1 li1) 'uu)
  uu)

(check-for-bug :alltest-legacy-517
  (eval 'li1)
  (a uu ((c) (d))))

(check-for-bug :alltest-legacy-521
  (setf (elt li1 1) 'oo)
  oo)

(check-for-bug :alltest-legacy-525
  (setf (elt vec1 1) 'oo)
  oo)

(check-for-bug :alltest-legacy-529
  (eval 'li1)
  (a oo ((c) (d))))

(check-for-bug :alltest-legacy-533
  (eval 'vec1)
  #(0 oo 2 3))

(check-for-bug :alltest-legacy-537
  (setf (rest li1) '((ww)))
  ((ww)))

(check-for-bug :alltest-legacy-541
  (eval 'li1)
  (a (ww)))

(check-for-bug :alltest-legacy-545
  (setf (first li1) 'aa)
  aa)

(check-for-bug :alltest-legacy-549
  (first li1)
  aa)

(check-for-bug :alltest-legacy-553
  (setf (second li1) 'bb)
  bb)

(check-for-bug :alltest-legacy-557
  (eval 'li1)
  (aa bb))

(check-for-bug :alltest-legacy-561
  (setf (rest li1) (list 2 3 4 5 6 7 8 9 10))
  (2 3 4 5 6 7 8 9 10))

(check-for-bug :alltest-legacy-565
  (setf (second li1) 22)
  22)

(check-for-bug :alltest-legacy-569
  (eval 'li1)
  (aa 22 3 4 5 6 7 8 9 10))

(check-for-bug :alltest-legacy-573
  (setf (third li1) '33)
  33)

(check-for-bug :alltest-legacy-577
  (setf (fourth li1) '44)
  44)

(check-for-bug :alltest-legacy-581
  (setf (fifth li1) '55)
  55)

(check-for-bug :alltest-legacy-585
  (setf (sixth li1) '66)
  66)

(check-for-bug :alltest-legacy-589
  (setf (seventh li1) '77)
  77)

(check-for-bug :alltest-legacy-593
  (setf (eighth li1) '88)
  88)

(check-for-bug :alltest-legacy-597
  (setf (ninth li1) '99)
  99)

(check-for-bug :alltest-legacy-601
  (setf (tenth li1) '1010)
  1010)

(check-for-bug :alltest-legacy-605
  (eval 'li1)
  (aa 22 33 44 55 66 77 88 99 1010))

(check-for-bug :alltest-legacy-609
  (setf (first li1) '(((a))))
  (((a))))

(check-for-bug :alltest-legacy-613
  (setf (caaar li1) 'uu)
  uu)

(check-for-bug :alltest-legacy-617
  (caaar li1)
  uu)

(check-for-bug :alltest-legacy-621
  (car li1)
  ((uu)))

(check-for-bug :alltest-legacy-625
  (setf (caar li1) 'oo)
  oo)

(check-for-bug :alltest-legacy-629
  (eval 'li1)
  ((oo) 22 33 44 55 66 77 88 99 1010))

(check-for-bug :alltest-legacy-633
  (setf (car li1) 'ii)
  ii)

(check-for-bug :alltest-legacy-637
  (eval 'li1)
  (ii 22 33 44 55 66 77 88 99 1010))

(check-for-bug :alltest-legacy-641
  (setf (cdddr li1) 'pp)
  pp)

(check-for-bug :alltest-legacy-645
  (eval 'li1)
  (ii 22 33 . pp))

(check-for-bug :alltest-legacy-649
  (setf (caddr li1) '333)
  333)

(check-for-bug :alltest-legacy-653
  (eval 'li1)
  (ii 22 333 . pp))

(check-for-bug :alltest-legacy-657
  (setf (svref vec1 2) 'kk)
  kk)

(check-for-bug :alltest-legacy-661
  (eval 'vec1)
  #(0 oo kk 3))

(unintern 'vec1)
(unintern 'li1)

(check-for-bug :alltest-legacy-668
  (setf (get 'a 'b) 'uu)
  uu)

(check-for-bug :alltest-legacy-672
  (get 'a 'b)
  uu)

(check-for-bug :alltest-legacy-676
  (setf (getf
         (cadr
          (setq xx
                (list 'aaa
                      (list 'i1 'v1 'i2 'v2))))
         'i2)
        'v222)
  v222)

(check-for-bug :alltest-legacy-686
  (eval 'xx)
  (aaa (i1 v1 i2 v222)))

(check-for-bug :alltest-legacy-690
  (getf (cadr xx) 'i2)
  v222)

(check-for-bug :alltest-legacy-694
  (getf (cadr xx) 'i1)
  v1)

(unintern 'xx)

(check-for-bug :alltest-legacy-700
  (setf (documentation 'beispiel 'typ1) "doc 1")
  "doc 1")

(check-for-bug :alltest-legacy-704
  (setf (documentation 'beispiel 'typ2) "doc 2")
  "doc 2")

(check-for-bug :alltest-legacy-708
  (documentation 'beispiel 'typ2)
  #+xcl (typ2 . "doc 2")
  #-xcl "doc 2")

(check-for-bug :alltest-legacy-713
  (setf (documentation 'beispiel 'typ2) "doc 3")
  "doc 3")

(check-for-bug :alltest-legacy-717
  (documentation 'beispiel 'typ2)
  #+xcl (typ2 . "doc 3")
  #-xcl "doc 3")

(check-for-bug :alltest-legacy-722
  (symbol-plist 'beispiel)
  #+xcl (documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
  #+clisp (system::documentation-strings (typ2 "doc 3" typ1 "doc 1"))
  #+allegro (excl::%documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
  #+(or cmu sbcl ecls) nil
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-730
  (setf (symbol-value 'xx) 'voelligneu)
  voelligneu)

(check-for-bug :alltest-legacy-734
  (eval 'xx)
  voelligneu)

(unintern 'xx)

;; psetf, shiftf, rotatef, define-modify-macro, defsetf, define-setf-method,
;; get-setf-method, get-setf-method-multiple-value, apply, funcall, progn,
;; prog1, prog2,

(check-for-bug :alltest-legacy-744
  (let ((x (list 'a 'b 'c)))
    (rplacd (last x) x)
    (list-length x))
  nil)

;; let*, compiler-let, progv, flet, labels, macrolet, if, when, unless, cond,
;; case, typecase, block, loop, do, do*, dolist, dotimes,

(check-for-bug :alltest-legacy-753
  (mapcar (function (lambda (x) (list x))) (list 'a 'b 'c))
  ((a) (b) (c)))

(check-for-bug :alltest-legacy-757
  (mapc (function
         (lambda (x y z)
	  (list x y z)))
        (list 'a 'b 'c)
        (list 1 2 3)
        (list 'u 'i 'v))
  (a b c))

(check-for-bug :alltest-legacy-766
  (mapl (function (lambda (x y z) (list x y z))) (list 'a 'b 'c) (list 1 2 3)
        (list 'u 'i 'v))
  (a b c))

(check-for-bug :alltest-legacy-771
  (maplist (lambda (x y z) (list x y z))
           (list 'a 'b 'c)
           (list 1 2 3)
           (list 'u 'i 'v))
  (((a b c) (1 2 3) (u i v)) ((b c) (2 3) (i v)) ((c) (3) (v))))

(check-for-bug :alltest-legacy-778
  (mapcon (lambda (x y z) (list x y z))
          (list 'a 'b)
          (list 1 2 3)
          (list 'u 'i 'v))
  ((a b) (1 2 3) (u i v) (b) (2 3) (i v)))

(check-for-bug :alltest-legacy-785
  (mapcan (lambda (x y z) (list x y z))
          (list 'a 'b 'c)
          (list 1 2 3)
          (list 'u 'i 'v))
  (a 1 u b 2 i c 3 v))

(check-for-bug :alltest-direct-funcall-of-compiled-lambda
  (funcall
   (compile nil (lambda (x) (flet ((foo (y) (+ y 1))) (foo (* 2 x)))))
   3)
  7)

;; tagbody, go, multiple-value-list, multiple-value-call, multiple-value-prog1,
;; multiple-value-bind, multiple-value-setq, values, values-list, catch,

;; unwind-protect, throw,

;; kap 8 macros
;; ----------------------------------------------------------------------------

;; macro-function, defmacro, macroexpand, macroexpand-1,

;; kap 9 declarationen
;; ----------------------------------------------------------------------------

;; declare, locally, proclaim, the,

;; kap 10 symbole
;; ----------------------------------------------------------------------------

;; get, remprop, symbol-plist, getf, remf, get-properties, symbol-name,

;; make-symbol, copy-symbol, gensym, gentemp, symbol-package,

(check-for-bug :alltest-legacy-814
  (keywordp 36)
  nil)

(check-for-bug :alltest-legacy-818
  (keywordp :rename)
  t)

;; kap 11 pakete
;; ----------------------------------------------------------------------------

;; find-package, in-package, list-all-packages, make-package, package-name,
;; package-nicknames, package-shadowing-symbols, package-use-list,
;; package-used-by-list, rename-package, unuse-package, use-package, intern,
;; unintern, find-symbol, export, unexport, import, shadowing-import, shadow,
;; find-all-symbols, do-symbols, do-external-symbols, do-all-symbols,
;; provide, require,

;; kap 12 zahlen
;; ----------------------------------------------------------------------------

(check-for-bug :alltest-legacy-835
  (zerop -456)
  nil)

(check-for-bug :alltest-legacy-839
  (zerop 0)
  t)

(check-for-bug :alltest-legacy-843
  (plusp 3)
  t)

(check-for-bug :alltest-legacy-847
  (plusp 3453786543987565)
  t)

(check-for-bug :alltest-legacy-851
  (minusp -456)
  t)

(check-for-bug :alltest-legacy-855
  (oddp -1)
  t)

(check-for-bug :alltest-legacy-859
  (oddp 0)
  nil)

(check-for-bug :alltest-legacy-863
  (evenp -456)
  t)

(check-for-bug :alltest-legacy-867
  (evenp -345)
  nil)

(check-for-bug :alltest-legacy-871
  (= 5/2 2.5)
  t)

(check-for-bug :alltest-legacy-875
  (/= 3.0 3)
  nil)

(check-for-bug :alltest-legacy-879
  (/= 3.0 #c(3.0 1.0))
  t)

(check-for-bug :alltest-legacy-883
  (< 3.0 3)
  nil)

(check-for-bug :alltest-legacy-887
  (< 3 3.0 3 #c(3.0 0.0))
  #+(or allegro cmu sbcl sbcl) nil
  #-(or allegro cmu sbcl sbcl) error)

(check-for-bug :alltest-legacy-892
  (< -5 -4 -2 0 4 5)
  t)

(check-for-bug :alltest-legacy-896
  (> 8 7 6 5 4)
  t)

(check-for-bug :alltest-legacy-900
  (> 3 3.0 3 #c(3.0 0.0))
  #+(or allegro cmu sbcl sbcl) nil
  #-(or allegro cmu sbcl sbcl) error)

(check-for-bug :alltest-legacy-905
  (<= 3.0 3)
  t)

(check-for-bug :alltest-legacy-909
  (<= 3 3)
  t)

(check-for-bug :alltest-legacy-913
  (<= 1 3 3 2 5)
  nil)

(check-for-bug :alltest-legacy-917
  (<= 5/2 2.5)
  t)

(check-for-bug :alltest-legacy-921
  (>= -5 -4 -2 0 4 5)
  nil)

(check-for-bug :alltest-legacy-925
  (max 1 3 2 -7)
  3)

;; min,

(check-for-bug :alltest-legacy-931
  (+ 1 1/2 0.5 #c(3.0 5.5))
  #c(5.0 5.5))

(check-for-bug :alltest-legacy-935
  (- 3 0 3 5 -6)
  1)

(check-for-bug :alltest-legacy-939
  (- #c(0 6) 1/4 0.5 7)
  #c(-7.75 6.0))

(check-for-bug :alltest-legacy-943
  (* 7 6 5 4 3 2 1)
  5040)

(check-for-bug :alltest-legacy-947
  (* 2 2 2.0 2)
  16.0)

(check-for-bug :alltest-legacy-951
  (/ -8)
  -1/8)

(check-for-bug :alltest-legacy-955
  (/ 4 2)
  2)

(check-for-bug :alltest-legacy-959
  (1+ 0)
  1)

(check-for-bug :alltest-legacy-963
  (1+ #c(0 1))
  #c(1 1))

(check-for-bug :alltest-legacy-967
  (1- 5.0)
  4.0)

;; incf, decf,

(check-for-bug :alltest-legacy-973
  (conjugate #c(3/5 4/5))
  #c(3/5 -4/5))

(check-for-bug :alltest-legacy-977
  (gcd 91 -49)
  7)

(check-for-bug :alltest-legacy-981
  (lcm 14 35)
  70)

(check-for-bug :alltest-legacy-985
  (prin1-to-string (exp 1) )
  "2.7182817")				; "2.718282"

(check-for-bug :alltest-legacy-989
  (expt #c(0 1) 2)
  -1)

(check-for-bug :alltest-legacy-993
  (prin1-to-string (expt 2 #c(0 1)) )
  "#C(0.7692389 0.63896126)")		; "#C(0.7692389 0.6389612)"

(check-for-bug :alltest-legacy-997
  (prin1-to-string (log -3 10) )
  "#C(0.47712126 1.3643764)")		; "#C(0.4771213 1.364376)"

(check-for-bug :alltest-legacy-1001
  (log 3 0)
  #+(or xcl cmu sbcl sbcl) 0
  #+allegro 0.0
  #-(or xcl allegro cmu sbcl sbcl) error)

(check-for-bug :alltest-legacy-1007
  (sqrt 9)
  3.0)

(check-for-bug :alltest-legacy-1011
  (sqrt -9.0)
  #c(0.0 3.0))

(check-for-bug :alltest-legacy-1015
  (isqrt 9)
  3)

(check-for-bug :alltest-legacy-1019
  (isqrt 26)
  5)

(check-for-bug :alltest-legacy-1023
  (abs 6)
  6)

(check-for-bug :alltest-legacy-1027
  (abs -6)
  6)

;; phase,

(check-for-bug :alltest-legacy-1033
  (signum 0)
  0)

(check-for-bug :alltest-legacy-1037
  (signum -4)
  -1)

(check-for-bug :alltest-legacy-1041
  (signum 4)
  1)

;; sin(8*pi/2) = sin(4*pi) = 0.  Assume that our pi is a bit off.  Say
;; pi*(1+eps).  Then sin(8*pi*(1+eps)/2) = sin(4*pi + 4*pi*eps) =
;; sin(4*pi*eps) ~= 4*pi*eps.  Test for this, but allow a fudge factor
;; of 2.
(check-for-bug :alltest-legacy-1045
  (let ((y (sin (* 8 (/ pi 2))))
	(fudge 2))
    (< (abs y) (* fudge (* 4 pi long-float-epsilon))))
  t)

(check-for-bug :alltest-legacy-1053
  (prin1-to-string (sin (expt 10 3)) )
  "0.82687956")				; "0.8268796"

(check-for-bug :alltest-legacy-1057
  (cos 0)
  1.0)

;; As in 1045, cos(pi*(1+eps)/2) = cos(pi/2+pi*eps/2) = -sin(pi*eps/2)
;; ~= -pi*eps/2.  Check that we are close enough.
(check-for-bug :alltest-legacy-1061
  (let ((y (cos (/ pi 2)))
	(fudge 2))
    (< (abs y) (* fudge (* pi long-float-epsilon 1/2))))
  t)


;; http://www.lisp.org/HyperSpec/Body/convar_short-_tive-epsilon.html
(check-for-bug :alltest-test-pos-epsilon
  (defun test-pos-epsilon (<EPSILON>)
    (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>)))
  test-pos-epsilon)

(check-for-bug :alltest-test-pos-epsilon-1
  (test-pos-epsilon short-float-epsilon)
  nil)

(check-for-bug :alltest-test-pos-epsilon-2
  (test-pos-epsilon single-float-epsilon)
  nil)

(check-for-bug :alltest-test-pos-epsilon-3
  (test-pos-epsilon double-float-epsilon)
  nil)

(check-for-bug :alltest-test-pos-epsilon-4
  (test-pos-epsilon long-float-epsilon)
  nil)

(check-for-bug :alltest-test-neg-epsilon
  (defun test-neg-epsilon (<EPSILON>)
    (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>)))
  test-neg-epsilon)

(check-for-bug :alltest-test-neg-epsilon-1
  (test-neg-epsilon short-float-negative-epsilon)
  nil)

(check-for-bug :alltest-test-neg-epsilon-2
  (test-neg-epsilon single-float-negative-epsilon)
  nil)

(check-for-bug :alltest-test-neg-epsilon-3
  (test-neg-epsilon double-float-negative-epsilon)
  nil)

(check-for-bug :alltest-test-neg-epsilon-4
  (test-neg-epsilon long-float-negative-epsilon)
  nil)

(check-for-bug :alltest-legacy-1069
  (prin1-to-string (tan 1) )
  "1.5574077")				; "1.557408"

;; As in 1045, tan(x) = sin(x)/cos(x).  When x = pi/2+pi*eps/2, sin(x)
;; is essentially 1, but cos(x) is about pi/2*eps.  So tan(x) =
;; 1/cos(x).  So we have 1/tan(x) = cos(x).  But from 1061, we know
;; that cos(x) ~= -pi*eps/2.  So our check is |1/tan(x) - cos(x)| is
;; close enough to zero.
(check-for-bug :alltest-legacy-1073
  (let* ((fudge 2)
	 (val (tan (/ pi 2)))
	 (ref (cos (/ pi 2)))
	 (rel-err (abs (/ (- (/ val) ref) ref))))
    (< rel-err (* fudge long-float-epsilon)))
  t)

(check-for-bug :alltest-legacy-1082
  (prin1-to-string (cis -1) )
  "#C(0.5403023 -0.84147096)")		; "#C(0.5403023 -0.8414709)"

(check-for-bug :alltest-legacy-1086
  (cis 2.5)
  #c(-0.8011436 0.5984721))

(check-for-bug :alltest-legacy-1090
  (prin1-to-string (asin -1) )
  "-1.5707964")				; "-1.570796"

(check-for-bug :alltest-legacy-1094
  (asin 0)
  0.0)

(check-for-bug :alltest-legacy-1098
  (asin 2)
  #+(or cmu sbcl)
  #c(1.5707964 -1.3169578)
  #-(or cmu sbcl)
  #c(1.5707964 -1.316958))

(check-for-bug :alltest-legacy-1105
  (prin1-to-string (acos 0) )
  "1.5707964")				; "1.570796"

(check-for-bug :alltest-legacy-1109
  (prin1-to-string (acos -1) )
  "3.1415927")				; "3.141593"

(check-for-bug :alltest-legacy-1113
  (prin1-to-string (acos 2) )
  #+xcl
  "#C(0.0 1.316958)"
  #+clisp
  "#C(0 1.316958)"
  #+allegro
  "#c(0.0 1.316958)"
  #+(or cmu sbcl sbcl ecls)
  "#C(0.0 1.3169578)"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1125
  (acos 1.00001)
  #+ganz-korrekt
  #c(0.0 0.0044721322)
  #+xcl
  #c(0.0 0.004475157)
  #+clisp-korrekt
  #c(0.0 0.0044751678)                  ; da schon 1.00001 gerundet wurde
  #+clisp
  #c(0.0 0.0044751023)                  ; i * ln(x+sqrt(x^2-1))
  #+clisp-anders
  #c(0.0 0.0044752206)                  ; i * ln(x+sqrt((x-1)*(x+1)))
  #+allegro
  #c(0.0 0.004475168)
  #+(or cmu sbcl)
  #c(0.0 0.0044751678)
  #-(or xcl clisp allegro cmu sbcl sbcl)
  #c(0.0 0.0044721322))

(check-for-bug :alltest-legacy-1144
  (atan 1)
  #+(or xcl allegro cmu sbcl sbcl ecls) 0.7853982
  #+clisp 0.7853981
  #-(or xcl allegro clisp cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1150
  (prin1-to-string pi )
  #+xcl "3.141592653589793D0"
  #+clisp "3.1415926535897932385L0"
  #+(or allegro cmu sbcl sbcl ecls) "3.141592653589793d0"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1157
  (sinh 0)
  0.0)

(check-for-bug :alltest-legacy-1161
  (prin1-to-string (sinh #c(5.0 -9.6)) )
  #+(or cmu sbcl)
  "#C(-73.06699 12.936809)"
  #-(or cmu sbcl)
  "#C(-73.06699 12.93681)")

(check-for-bug :alltest-legacy-1168
  (cosh 0)
  1.0)

(check-for-bug :alltest-legacy-1172
  (prin1-to-string (cosh 1) )
  #+(or cmu sbcl clisp) "1.5430807"	; round-off error
  #-(or cmu sbcl clisp) "1.5430806")	; "1.543081"

(check-for-bug :alltest-legacy-1177
  (tanh 50)
  1.0)

(check-for-bug :alltest-legacy-1181
  (prin1-to-string (tanh 0.00753) )
  #-allegro "0.0075298576"
  #+allegro "0.0075298795")		; "0.007529857"

(check-for-bug :alltest-legacy-1186
  (prin1-to-string (asinh 0.5) )
  #-(or allegro cmu sbcl sbcl) "0.48121184"
  #+(or allegro cmu sbcl sbcl) "0.4812118") ; "0.4812118"

(check-for-bug :alltest-legacy-1191
  (prin1-to-string (asinh 3/7) )
  #-(or clisp allegro cmu sbcl sbcl) "0.4164308"
  #+clisp "0.4164307"			; rundungsfehler
  #+(or allegro cmu sbcl sbcl) "0.41643077")

(check-for-bug :alltest-legacy-1197
  (acosh 0)
  #c(0 1.5707964))

(check-for-bug :alltest-legacy-1201
  (acosh 1)
  0)

(check-for-bug :alltest-legacy-1205
  (acosh -1)
  #c(0 3.1415927))

(check-for-bug :alltest-legacy-1209
  (prin1-to-string (atanh 0.5) )
  "0.54930615")				; "0.5493061"

(check-for-bug :alltest-legacy-1213
  (prin1-to-string (atanh 3/7) )
  #-(or clisp allegro cmu sbcl sbcl) "0.4581454"
  #+clisp "0.4581453"			; rundungsfehler
  #+(or allegro cmu sbcl sbcl) "0.45814538")

(check-for-bug :alltest-legacy-1219
  (= (sin (* #c(0 1) 5)) (* #c(0 1) (sinh 5)))
  t)

(check-for-bug :alltest-legacy-1223
  (= (cos (* #c(0 1) 5)) (cosh 5))
  t)

(check-for-bug :alltest-legacy-1227
  (= (tan (* #c(0 1) 5)) (* #c(0 1) (tanh 5)))
  t)

(check-for-bug :alltest-legacy-1231
  (= (sinh (* #c(0 1) 5)) (* #c(0 1) (sin 5)))
  t)

(check-for-bug :alltest-legacy-1235
  (= (cosh (* #c(0 1) 5)) (cos 5))
  t)

(check-for-bug :alltest-legacy-1235-bis
  (= (tanh (* #c(0 1) 5)) (* #c(0 1) (tan 5)))
  t)

(check-for-bug :alltest-legacy-1239
  (float 1)
  1.0)

(check-for-bug :alltest-legacy-1243
  (float 0.5)
  0.5)

(check-for-bug :alltest-legacy-1247
  (rational 2)
  2)

(check-for-bug :alltest-legacy-1251
  (rational 2.0)
  2)

(check-for-bug :alltest-legacy-1255
  (rational 2.5)
  5/2)

(check-for-bug :alltest-legacy-1259
  (rationalize 2.5)
  5/2)

(check-for-bug :alltest-legacy-1263
  (rationalize 7/3)
  7/3)

(check-for-bug :alltest-legacy-1267
  (rationalize pi)
  #+xcl 28296953155597409/9007199254740992
  #+clisp 8717442233/2774848045
  #+ecls 884279719003555/281474976710656
  #+(or allegro cmu sbcl sbcl) 245850922/78256779
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1275
  (numerator 5/2)
  5)

(check-for-bug :alltest-legacy-1279
  (numerator (/ 8 -6))
  -4)

(check-for-bug :alltest-legacy-1283
  (denominator 5/2)
  2)

(check-for-bug :alltest-legacy-1287
  (denominator (/ 8 -6))
  3)

(check-for-bug :alltest-legacy-1291
  (gcd (numerator 7/9) (denominator 7/9))
  1)

(check-for-bug :alltest-legacy-1295
  (floor 2.6)
  2)

(check-for-bug :alltest-legacy-1299
  (floor 2.5)
  2)

(check-for-bug :alltest-legacy-1303
  (ceiling 2.6)
  3)

(check-for-bug :alltest-legacy-1307
  (ceiling 2.5)
  3)

(check-for-bug :alltest-legacy-1311
  (ceiling 2.4)
  3)

(check-for-bug :alltest-legacy-1315
  (truncate 2.6)
  2)

(check-for-bug :alltest-legacy-1319
  (truncate 2.5)
  2)

(check-for-bug :alltest-legacy-1323
  (truncate 2.4)
  2)

(check-for-bug :alltest-legacy-1327
  (round 2.6)
  3)

(check-for-bug :alltest-legacy-1331
  (round 2.5)
  2)

(check-for-bug :alltest-legacy-1335
  (round 2.4)
  2)

(check-for-bug :alltest-legacy-1339
  (mod 13 4)
  1)

(check-for-bug :alltest-legacy-1343
  (mod -13 4)
  3)

(check-for-bug :alltest-legacy-1347
  (prin1-to-string (rem 13.4 1) )
  #-(or clisp allegro cmu sbcl sbcl) "0.4" ;
  #+xcl "0.3999996"
  #+(or clisp allegro cmu sbcl sbcl) "0.39999962") ; rundungsfehler

(check-for-bug :alltest-legacy-1353
  (ffloor 2.6)
  2)

(check-for-bug :alltest-legacy-1357
  (ffloor 2.5)
  2)

(check-for-bug :alltest-legacy-1361
  (ffloor 2.4)
  2)

(check-for-bug :alltest-legacy-1365
  (fceiling -0.3)
  0)

(check-for-bug :alltest-legacy-1369
  (fceiling -0.7)
  0)

(check-for-bug :alltest-legacy-1373
  (fceiling -2.4)
  -2)

(check-for-bug :alltest-legacy-1377
  (ftruncate 2.5)
  2.0)

(check-for-bug :alltest-legacy-1381
  (ftruncate 2.4)
  2.0)

(check-for-bug :alltest-legacy-1385
  (fround -0.7)
  -1.0)

(check-for-bug :alltest-legacy-1389
  (fround -2.4)
  -2.0)

(check-for-bug :alltest-legacy-1393
  (decode-float 35.0)
  0.546875)

(check-for-bug :alltest-legacy-1397
  (decode-float 3.5s0)
  0.875s0)

(check-for-bug :alltest-legacy-1401
  (scale-float 2.5 5)
  80.0)

(check-for-bug :alltest-legacy-1405
  (scale-float 0.7541 2)
  3.0164)

(check-for-bug :alltest-legacy-1409
  (float-radix 2.5)
  2)

(check-for-bug :alltest-legacy-1413
  (float-radix 3.5d0)
  2)

;; float-digits, float-precision, float-sign, integer-decode-float,

(check-for-bug :alltest-legacy-1419
  (complex 1/4 7.3)
  #c(0.25 7.3))

(check-for-bug :alltest-legacy-1423
  (complex 1 0)
  1)

(check-for-bug :alltest-legacy-1427
  (realpart 5)
  5)

(check-for-bug :alltest-legacy-1431
  (realpart #c(1.4 0.0))
  1.4)

(check-for-bug :alltest-legacy-1435
  (imagpart 5)
  0)

(check-for-bug :alltest-legacy-1439
  (imagpart #c(1.4 0.0))
  0.0)

;; logand, logandc1, logandc2, logeqv, logior, lognand, lognor, lognot,
;; logorc1, logorc2, logtest, logxor, logbitp, ash,

(check-for-bug :alltest-legacy-1446
  (logcount 13)
  3)

(check-for-bug :alltest-legacy-1450
  (logcount -13)
  2)

(check-for-bug :alltest-legacy-1454
  (integer-length 0)
  0)

(check-for-bug :alltest-legacy-1458
  (integer-length 1)
  1)

;; byte, byte-position, byte-size, ldb, ldb-test, mask-field, dpb, deposit-field,

;; random,

#+xcl
(check-for-bug :alltest-legacy-1467
  (random-state-p
   (eval (read-from-string "(sys::%set-type-pointer sys::%type-random-state 1)")))
  t)

;; make-random-state,

(check-for-bug :alltest-legacy-1474
  boole-clr
  0)

(check-for-bug :alltest-legacy-1478
  boole-set
  #+(or xcl allegro cmu sbcl sbcl) 1
  #+(or clisp ecls) 15
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1484
  boole-1
  #+(or xcl allegro cmu sbcl sbcl) 2
  #+clisp 10
  #+ecls 3
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1491
  boole-2
  #+(or xcl allegro cmu sbcl sbcl) 3
  #+clisp 12
  #+ecls 5
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1498
  boole-c1
  #+(or xcl allegro cmu sbcl sbcl) 4
  #+clisp 5
  #+ecls 12
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1505
  boole-c2
  #+(or xcl allegro cmu sbcl sbcl) 5
  #+clisp 3
  #+ecls 10
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1512
  boole-and
  #+(or xcl allegro cmu sbcl sbcl) 6
  #+clisp 8
  #+ecls 1
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1519
  boole-ior
  #+(or xcl allegro cmu sbcl sbcl) 7
  #+clisp 14
  #+ecls 7
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1526
  boole-xor
  #+(or xcl allegro cmu sbcl sbcl) 8
  #+(or clisp ecls) 6
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1532
  boole-eqv
  #+(or xcl allegro cmu sbcl sbcl) 9
  #+(or clisp ecls) 9
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1538
  boole-nand
  #+(or xcl allegro cmu sbcl sbcl) 10
  #+clisp 7
  #+ecls 14
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1545
  boole-nor
  #+(or xcl allegro cmu sbcl sbcl) 11
  #+clisp 1
  #+ecls 8
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1552
  boole-andc1
  #+(or xcl allegro cmu sbcl sbcl) 12
  #+(or clisp ecls) 4
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1558
  boole-andc2
  #+(or xcl allegro cmu sbcl sbcl) 13
  #+(or clisp ecls) 2
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1564
  boole-orc1
  #+(or xcl allegro cmu sbcl sbcl) 14
  #+(or clisp ecls) 13
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1570
  boole-orc2
  #+(or xcl allegro cmu sbcl sbcl) 15
  #+(or clisp ecls) 11
  #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1576
  (let ((s (prin1-to-string most-positive-fixnum )))
    (or #+(or xcl clisp) (equal s "16777215")
        #+clisp (equal s "33554431")
        #+clisp (equal s "67108863")
        #+clisp (equal s "4294967295")
        #+(or allegro cmu sbcl sbcl) (equal s "536870911")
        ) )
  t)

(check-for-bug :alltest-legacy-1586
  (let ((s (prin1-to-string most-negative-fixnum )))
    (or #+(or xcl clisp) (equal s "-16777216")
        #+clisp (equal s "-33554432")
        #+clisp (equal s "-67108864")
        #+clisp (equal s "-4294967296")
        #+(or allegro cmu sbcl) (equal s "-536870912")
        ) )
  t)

(check-for-bug :alltest-legacy-1596
  (prin1-to-string most-positive-short-float )
  #+xcl "1.701S38"
  #+clisp "1.7014s38"
  #+allegro "3.4028232e+38"
  #+(or cmu sbcl) "3.4028235e+38"
  #+ecls "3.4028235e38"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1605
  (prin1-to-string least-positive-short-float )
  #+xcl "2.939S-39"
  #+clisp "2.93874s-39"
  #+(or allegro cmu sbcl sbcl) "1.4012985e-45"
  #+ecls "1.401298E-45"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1613
  (prin1-to-string least-negative-short-float )
  #+xcl "-2.939S-39"
  #+clisp "-2.93874s-39"
  #+(or allegro cmu sbcl sbcl) "-1.4012985e-45"
  #+ecls "-1.401298E-45"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1621
  (prin1-to-string most-negative-short-float )
  #+xcl "-1.701S38"
  #+clisp "-1.7014s38"
  #+allegro "-3.4028232e+38"
  #+(or cmu sbcl) "-3.4028235e+38"
  #+ecls "-3.402823E38"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1630
  (let ((s (prin1-to-string most-positive-single-float )))
    (or #+xcl (equal s "1.701411E38")
        #+clisp (equal s "1.7014117E38")
        #+clisp (equal s "3.4028235E38")
        #+allegro (equal s "3.4028232e+38")
        #+(or cmu sbcl) (equal s "3.4028235e+38")
        ) )
  t)

(check-for-bug :alltest-legacy-1640
  (let ((s (prin1-to-string least-positive-single-float )))
    (or #+(or xcl clisp) (equal s "2.938736E-39")
        #+clisp (equal s "1.1754944E-38")
        #+(or allegro cmu sbcl sbcl) (equal s "1.4012985e-45")
        ) )
  t)

(check-for-bug :alltest-legacy-1648
  (let ((s (prin1-to-string least-negative-single-float )))
    (or #+(or xcl clisp) (equal s "-2.938736E-39")
        #+clisp (equal s "-1.1754944E-38")
        #+(or allegro cmu sbcl sbcl) (equal s "-1.4012985e-45")
        ) )
  t)

(check-for-bug :alltest-legacy-1656
  (let ((s (prin1-to-string most-negative-single-float )))
    (or #+xcl (equal s "-1.701411E38")
        #+clisp (equal s "-1.7014117E38")
        #+clisp (equal s "-3.4028235E38")
        #+allegro (equal s "-3.4028232e+38")
        #+(or cmu sbcl) (equal s "-3.4028235e+38")
        ) )
  t)

(check-for-bug :alltest-legacy-1666
  (let ((s (prin1-to-string most-positive-double-float )))
    (or #+xcl (equal s "1.701411834604692D38")
        #+clisp (equal s "8.988465674311579d307")
        #+clisp (equal s "1.7976931348623157d308")
        #+allegro (equal s "4.494232837155787d+307")
        #+(or cmu sbcl) (equal s "1.7976931348623157d+308")
        ) )
  t)

(check-for-bug :alltest-legacy-1676
  (let ((s (prin1-to-string least-positive-double-float )))
    (or #+xcl (equal s "2.938735877055719D-39")
        #+clisp (equal s "5.562684646268004d-309")
        #+clisp (equal s "2.2250738585072014d-308")
        #+allegro (equal s "4.9406564584124657d-324")
        #+(or cmu sbcl) (equal s "4.940656458412465d-324")
        ) )
  t)

(check-for-bug :alltest-legacy-1686
  (let ((s (prin1-to-string least-negative-double-float )))
    (or #+xcl (equal s "-2.938735877055719D-39")
        #+clisp (equal s "-5.562684646268004d-309")
        #+clisp (equal s "-2.2250738585072014d-308")
        #+allegro (equal s "-4.9406564584124657d-324")
        #+(or cmu sbcl) (equal s "-4.940656458412465d-324")
        ) )
  t)

(check-for-bug :alltest-legacy-1696
  (let ((s (prin1-to-string most-negative-double-float )))
    (or #+xcl (equal s "-1.701411834604692D38")
        #+clisp (equal s "-8.988465674311579d307")
        #+clisp (equal s "-1.7976931348623157d308")
        #+allegro (equal s "-4.494232837155787d+307")
        #+(or cmu sbcl) (equal s "-1.7976931348623157d+308")
        ) )
  t)

(check-for-bug :alltest-legacy-1706
  (prin1-to-string most-positive-long-float )
  #+xcl "1.701411834604692D38"
  #+clisp "8.8080652584198167656L646456992"
  #+allegro "4.494232837155787d+307"
  #+(or cmu sbcl) "1.7976931348623157d+308"
  #+ecls "1.797693134862316d308"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1715
  (prin1-to-string least-positive-long-float )
  #+xcl "2.938735877055719D-39"
  #+clisp "5.676615526003731344L-646456994"
  #+allegro "4.9406564584124657d-324"
  #+(or cmu sbcl ecls) "4.940656458412465d-324"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1723
  (prin1-to-string least-negative-long-float )
  #+xcl "-2.938735877055719D-39"
  #+clisp "-5.676615526003731344L-646456994"
  #+allegro "-4.9406564584124657d-324"
  #+(or cmu sbcl ecls) "-4.940656458412465d-324"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1731
  (prin1-to-string most-negative-long-float )
  #+xcl "-1.701411834604692D38"
  #+clisp "-8.8080652584198167656L646456992"
  #+allegro "-4.494232837155787d+307"
  #+(or cmu sbcl) "-1.7976931348623157d+308"
  #+ecls "-1.797693134862316d308"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1740
  (prin1-to-string short-float-epsilon )
  #+xcl "1.526S-5"
  #+clisp "7.6295s-6"
  #+allegro "1.1920929e-7"
  #+(or cmu sbcl) "5.960465e-8"
  #+ecls "6.258487E-8"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1749
  (prin1-to-string single-float-epsilon )
  #+xcl "5.960464E-8"
  #+clisp "5.960465E-8"
  #+allegro "1.1920929e-7"
  #+(or cmu sbcl) "5.960465e-8"
  #+ecls "6.258487E-8"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1758
  (let ((s (prin1-to-string double-float-epsilon)))
    (or #+xcl (equal s "1.387778780781446D-17")
        #+clisp (equal s "1.1107651257113995d-16") ; linux/i386
        #+(or clisp cmu sbcl sbcl) (equal s "1.1102230246251568d-16")
        #+allegro (or equal "2.220446049250313d-16")
        #+ecls (or equal  "1.165734175856414d-16")))
  t)

(check-for-bug :alltest-legacy-1766
  (prin1-to-string long-float-epsilon )
  #+xcl "1.387778780781446D-17"
  #+clisp "5.4210108624275221706L-20"
  #+allegro "2.220446049250313d-16"
  #+(or cmu sbcl) "1.1102230246251568d-16"
  #+ecls "1.165734175856414d-16"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1775
  (prin1-to-string short-float-negative-epsilon )
  #+xcl "1.526S-5"
  #+clisp "3.81476s-6"
  #+allegro "1.1920929e-7"
  #+(or cmu sbcl) "2.9802325e-8"
  #+ecls "3.129244E-8"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1784
  (prin1-to-string single-float-negative-epsilon )
  #+xcl "5.960464E-8"
  #+clisp "2.9802326E-8"
  #+allegro "1.1920929e-7"
  #+(or cmu sbcl) "2.9802325e-8"
  #+ecls "3.129244E-8"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1793
  (let ((s (prin1-to-string double-float-negative-epsilon)))
    (or #+xcl (equal s "1.387778780781446D-17")
        #+clisp (equal s "5.553825628556998d-17") ; linux/i386
        #+(or clisp cmu sbcl sbcl) (equal s "5.551115123125784d-17")
        #+allegro (equal s "2.220446049250313d-16")
        #+ecls (equal s "5.828670879282072d-17")))
  t)

(check-for-bug :alltest-legacy-1801
  (prin1-to-string long-float-negative-epsilon )
  #+xcl "1.387778780781446D-17"
  #+clisp "2.7105054312137610853L-20"
  #+allegro "2.220446049250313d-16"
  #+(or cmu sbcl) "5.551115123125784d-17"
  #+ecls "5.828670879282072d-17"
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-1810
  (/ 1 0)
  error)

(check-for-bug :alltest-legacy-1814
  (/ 1 0.0s0)
  error)

(check-for-bug :alltest-legacy-1818
  (/ 1 0.0f0)
  error)

(check-for-bug :alltest-legacy-1822
  (/ 1 0.0d0)
  error)

(check-for-bug :alltest-legacy-1826
  (/ 1 0.0l0)
  error)

(check-for-bug :alltest-legacy-1830
  (expt 10.0s0 1000)
  error)

(check-for-bug :alltest-legacy-1834
  (expt 10.0f0 1000)
  error)

(check-for-bug :alltest-legacy-1838
  (expt 10.0d0 1000)
  error)

(check-for-bug :alltest-legacy-1842
  (expt 10.0l0 1000000000)
  error)

;; kap 13 zeichen
;; ----------------------------------------------------------------------------

(check-for-bug :alltest-legacy-1849
  (standard-char-p #\a)
  t)

(check-for-bug :alltest-legacy-1853
  (standard-char-p 1)
  error)

(check-for-bug :alltest-legacy-1857
  (graphic-char-p #\a)
  t)

(check-for-bug :alltest-legacy-1861
  (graphic-char-p 1)
  error)

(check-for-bug :alltest-legacy-1865
  (characterp
   #\a)
  t)

(check-for-bug :alltest-legacy-1870
  (characterp
   #\1)
  t)

(check-for-bug :alltest-legacy-1875
  (alpha-char-p #\a)
  t)

(check-for-bug :alltest-legacy-1879
  (alpha-char-p #\$)
  nil)

(check-for-bug :alltest-legacy-1883
  (upper-case-p #\a)
  nil)

(check-for-bug :alltest-legacy-1887
  (lower-case-p #\A)
  nil)

(check-for-bug :alltest-legacy-1891
  (both-case-p #\a)
  t)

(check-for-bug :alltest-legacy-1895
  (both-case-p #\$)
  nil)

(check-for-bug :alltest-legacy-1899
  (digit-char-p #\a)
  nil)

(check-for-bug :alltest-legacy-1903
  (digit-char-p #\5)
  5)

(check-for-bug :alltest-legacy-1907
  (alphanumericp #\a)
  t)

(check-for-bug :alltest-legacy-1911
  (alphanumericp #\$)
  nil)

(check-for-bug :alltest-legacy-1915
  (char= #\d #\d)
  t)

(check-for-bug :alltest-legacy-1919
  (char/= #\d #\d)
  nil)

(check-for-bug :alltest-legacy-1923
  (char< #\z #\0)
  nil)

;; char>, char>=, char<=,

(check-for-bug :alltest-legacy-1929
  (char-equal #\d #\d)
  t)

(check-for-bug :alltest-legacy-1933
  (char-not-equal #\d #\d)
  nil)

(check-for-bug :alltest-legacy-1937
  (char-lessp #\d #\x)
  t)

(check-for-bug :alltest-legacy-1941
  (char-lessp #\d #\d)
  nil)

(check-for-bug :alltest-legacy-1945
  (char-not-greaterp #\d #\d)
  t)

(check-for-bug :alltest-legacy-1949
  (char-greaterp #\e #\d)
  t)

(check-for-bug :alltest-legacy-1953
  (char-not-lessp #\e #\d)
  t)

;; char-code, code-char, character,

(check-for-bug :alltest-legacy-1959
  (char-upcase #\a)
  #\a)

(check-for-bug :alltest-legacy-1963
  (char-upcase #\=)
  #\=)

(check-for-bug :alltest-legacy-1967
  (char= (char-downcase (char-upcase #\x)) #\x)
  t)

(check-for-bug :alltest-legacy-1971
  (char-downcase #\a)
  #\a)

(check-for-bug :alltest-legacy-1975
  (char= (char-upcase (char-downcase #\X)) #\X)
  t)

(check-for-bug :alltest-legacy-1979
  (digit-char 7)
  #\7)

(check-for-bug :alltest-legacy-1983
  (digit-char 12)
  nil)

;; char-int, int-char, char-name, name-char,

(check-for-bug :alltest-legacy-1989
  char-code-limit
  #+xcl 128
  #+(or (and clisp (not unicode)) akcl sbcl cmu ecls) 256
  #+allegro 65536
  #+(and clisp unicode) 1114112
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

;; kap 14 sequenzen
;; ----------------------------------------------------------------------------

(check-for-bug :alltest-legacy-1999
  (elt (symbol-name 'abc) 0)
  #\a)

(check-for-bug :alltest-legacy-2003
  (subseq (list 'a 'b 'c 'd 'e) 2)
  (c d e))

(check-for-bug :alltest-legacy-2007
  (copy-seq '#(a b c))
  #(a b c))

(check-for-bug :alltest-legacy-2011
  (copy-seq (list (list 'a 'b) 'c (list 'd 'e)))
  ((a b) c (d e)))

(check-for-bug :alltest-legacy-2015
  (length #(a b c d e f))
  6)

(check-for-bug :alltest-legacy-2019
  (length (list 'a 'b 'c 'd 'e 'f))
  6)

(check-for-bug :alltest-legacy-2023
  (nreverse (list 'a
                  (list 'b
                        (list 'c)
                        'd)))
  ((b (c) d) a))

(check-for-bug :alltest-legacy-2030
  (reverse (list 1 2 3 4))
  (4 3 2 1))

(check-for-bug :alltest-legacy-2034
  (make-sequence 'vector 4 :initial-element 'o)
  #(o o o o))

(check-for-bug :alltest-legacy-2038
  (make-sequence 'list 4 :initial-element 'o)
  (o o o o))

(check-for-bug :alltest-legacy-2042
  (concatenate 'list (list 'a 'b 'c) (list 1 2))
  (a b c 1 2))

(check-for-bug :alltest-legacy-2046
  (map 'list 'list
       (list #\a #\b #\c)
       (list #\1 #\2 #\3))
  ((#\a #\1) (#\b #\2) (#\c #\3)))

(check-for-bug :alltest-legacy-2052
  (map 'list 'list (list 'a 'b 'c) (list 1 2 3))
  ((a 1) (b 2) (c 3)))

(check-for-bug :alltest-legacy-2056
  (some 'null (list 'a 'b nil 't 'e))
  t)

(check-for-bug :alltest-legacy-2060
  (every 'atom (list 'a 8 #(a b)))
  t)

(check-for-bug :alltest-legacy-2064
  (notany 'eq
          (list 'a 'b 'c 'd 'e 4)
          (list 'i 'j 'k 'l 'm 4))
  nil)					;? t

(check-for-bug :alltest-legacy-2070
  (notevery 'eq '#(u)
            (list 'a 'x 'u))
  t)

(check-for-bug :alltest-legacy-2075
  (reduce 'list '(a) :from-end nil :initial-value nil)

  (nil a))

(check-for-bug :alltest-legacy-2080
  (reduce 'list
          (list 'a 'b 'c 'd)
          :from-end nil
          :initial-value 'iii)
  ((((iii a) b) c) d))

(check-for-bug :alltest-legacy-2087
  (reduce 'list (list 'a 'b 'c 'd) :from-end t)
  (a (b (c d))))

(check-for-bug :alltest-legacy-2091
  (fill '#(a b c d) 'i :start 1 :end 3)
  #(a i i d))

(check-for-bug :alltest-legacy-2095
  (replace '#(a b c d) '#(i j) :start1 1)
  #(a i j d))

(check-for-bug :alltest-legacy-2099
  (remove 'numberp '#(y a 4 a c 9 a d 2 3)
          :count 1 :from-end t)
  #(y a 4 a c 9 a d 2 3))

(check-for-bug :alltest-legacy-2104
  (remove 'a
          (list 'a 1 'b 'a '2 'a)
          :start 1)
  (a 1 b 2))

(check-for-bug :alltest-legacy-2110
  (remove-duplicates (list 'a 'b 'c 'a 'd 'a)
                     :start 1)
  (a b c d a))

(check-for-bug :alltest-legacy-2115
  (remove-if 'numberp '#(y a 4 a c 9 a d 2 3))
  #(y a a c a d))

(check-for-bug :alltest-legacy-2119
  (remove-if-not 'numberp #(y a 4 a c 9 a d 2 3))
  #(4 9 2 3))

(check-for-bug :alltest-legacy-2123
  (remove-if-not 'numberp #(y a 4 a c 9 a d 2 3)
                 :count 2 :from-end nil)
  #(4 a c 9 a d 2 3))

(check-for-bug :alltest-legacy-2128
  (delete '(a) (list (list 'a 'b) (list 'c 'd) (list 'a))
          :test 'equal)
  ((a b) (c d)))

(check-for-bug :alltest-legacy-2133
  (delete-if (lambda (x) (eq (car x) 'a))
             (list (list 'a 'b)
                   (list 'c 'd)
                   (list 'a)))
  ((c d)))

(check-for-bug :alltest-legacy-2140
  (delete-if-not 'numberp (list 'a 3 'b 4))
  (3 4))

;; delete-duplicates,

(check-for-bug :alltest-legacy-2146
  (nsubstitute 'new (list 1 'old)
               (list (list 0 'old) (list 1 'old) (list 2 'old))
               :test-not 'equal
               :from-end t)
  (new (1 old) new))

(check-for-bug :alltest-legacy-2153
  (nsubstitute 'new 'old (list 0 'old 1 'old 2 'old) :end 2)
  (0 new 1 old 2 old))

(check-for-bug :alltest-legacy-2157
  (nsubstitute-if 'new 'numberp (list 0 'a 1 'b 2 'c 3 'd)
                  :count 2
                  :end 5)
  (new a new b 2 c 3 d))

(check-for-bug :alltest-legacy-2163
  (nsubstitute-if-not 'new 'numberp
                      (list 0 'a 1 'b 2 'c 3 'd)
                      :count 2
                      :from-end t)
  (0 a 1 b 2 new 3 new))

(check-for-bug :alltest-legacy-2170
  (substitute 'new (list 2 'old)
              (list (list 1 'old) (list 2 'old) (list 3 'old) (list 4 'old))
              :test 'equal
              :start 3)
  ((1 old) (2 old) (3 old) (4 old)))

(check-for-bug :alltest-legacy-2177
  (substitute-if 'new 'numberp
                 (list 'a 1 'b 2 'd 3))
  (a new b new d new))

(check-for-bug :alltest-legacy-2182
  (substitute-if-not 'new 'numberp (list 'a 1 'b 2 'd 3)
                     :count 2
                     :from-end t)
  (a 1 new 2 new 3))

(check-for-bug :alltest-legacy-2188
  (find '0 (list (list 0 'a) (list 1 'a) (list 2 'a) (list 0 'b))
        :test '=
        :from-end t
        :key 'car
        :start 1)
  (0 b))

(check-for-bug :alltest-legacy-2196
  (find-if 'numberp (list (list 'a 0) (list 'b 1) (list 'c 2))
           :key 'cadr
           :start 3)
  nil)

;; find-if-not,

(check-for-bug :alltest-legacy-2204
  (position 'a (list (list 0 'a) (list 1 'b) (list 2 'a) (list 3 'b))
            :test #'(lambda (x y) (eq x (cadr y)))
            :start 1)
  2)

(check-for-bug :alltest-legacy-2210
  (position 'a
            (list (list 0 'a) (list 1 'b) (list 2 'a) (list 3 'b))
            :key 'cadr)

  0)

(check-for-bug :alltest-legacy-2217
  (position-if 'numberp
               (list (list 0 'x) (list 1 7.0) (list 2 8))
               :from-end t
               :start 1
               :key 'cadr)
  2)

;; position-if-not,

(check-for-bug :alltest-legacy-2227
  (count '(a)
         (list 'a (list 'a) 'a (list 'a) 'a 'b)
         :test-not 'equal
         :key (lambda (x)
                (when (atom x)
                  (list x))))
  3)

(check-for-bug :alltest-legacy-2236
  (count-if-not 'numberp '#(a 3 b 5 7 c d) :start 2 :end 5)
  1)

;; count-if-not,

(check-for-bug :alltest-legacy-2242
  (mismatch (list 'a 'b 'c 3 4 5)
            (list 'a 'b 'x 3 4 'b)
            :start1 1
            :start2 5
            :end1 2
            :test-not 'eq)
  1)

(check-for-bug :alltest-legacy-2251
  (mismatch (list 'a 'b 'c 3 4 5)
            (list 'u 'b 'x 3 4 5)
            :from-end t)
  #+xcl 2
  #-xcl 3)

(check-for-bug :alltest-legacy-2258
  (search "ABCD" "0ABIABJBCBC"
          :end1 3
          :start1 1
          :start2 0
          :from-end t)
  9)

(check-for-bug :alltest-legacy-2266
  (search (list #\A #\B #\C #\D)
          "0ABIABJBCBC"
          :end1 2
          :start2 0
          :from-end t)
  4)

(check-for-bug :alltest-legacy-2274
  (search (list 'a 'b 'c 'd)
          (list 0 'a 'b 'i 'a 'b 'j 'b 'c 'b 'c)
          :end1 2
          :start2 2)
  4)

(check-for-bug :alltest-legacy-2281
  (sort (list (list 'u 3) (list 'i 1)
              (list 'a 7) (list 'k 3)
              (list 'c 4) (list 'b 6))
        '<
        :key 'cadr)
  ((i 1) (u 3) (k 3) (c 4) (b 6) (a 7)))

(check-for-bug :alltest-legacy-2289
  (stable-sort (list (list 'b 4) (list 'a 3)
                     (list 'a 2) (list 'b 1)
                     (list 'c 9) (list 'b 2))
               'string<
               :key  'car)
  ((a 3) (a 2) (b 4) (b 1) (b 2) (c 9)))

(check-for-bug :alltest-legacy-2297
  (merge 'list
         (list 5 1 4 4 7)
         (list 2 3 5 6 8 9)
         '<)
  (2 3 5 1 4 4 5 6 7 8 9))		;? error

(check-for-bug :alltest-legacy-2304
  (merge 'list
         (list 1 4 4 7)
         (list 2 3 5 6 8 9)
         '<)
  (1 2 3 4 4 5 6 7 8 9))                ;? error

;; kap 15 listen
;; ----------------------------------------------------------------------------

(check-for-bug :alltest-legacy-2314
  (car (list 'a 'b 'c 'd 'e 'f 'g))
  a)

(check-for-bug :alltest-legacy-2318
  (cdr (list 'a 'b 'c 'd 'e 'f 'g))
  (b c d e f g))

(check-for-bug :alltest-legacy-2322
  (cadr (list 'a 'b 'c 'd 'e 'f 'g))
  b)

(check-for-bug :alltest-legacy-2326
  (cddr (list 'a 'b 'c 'd 'e 'f 'g))
  (c d e f g))

(check-for-bug :alltest-legacy-2330
  (caddr (list 'a 'b 'c 'd 'e 'f 'g))
  c)

(check-for-bug :alltest-legacy-2334
  (cdddr (list 'a 'b 'c 'd 'e 'f 'g))
  (d e f g))

(check-for-bug :alltest-legacy-2338
  (cadddr (list 'a 'b 'c 'd 'e 'f 'g))
  d)

(check-for-bug :alltest-legacy-2342
  (cddddr (list 'a 'b 'c 'd 'e 'f 'g))
  (e f g))

(check-for-bug :alltest-legacy-2346
  (caadr
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  ((u v w) x))

(check-for-bug :alltest-legacy-2361
  (cadar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (6 7))

(check-for-bug :alltest-legacy-2376
  (cdaar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (5))

(check-for-bug :alltest-legacy-2391
  (cdadr
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (y))

(check-for-bug :alltest-legacy-2406
  (cddar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  nil)

(check-for-bug :alltest-legacy-2421
  (caaaar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (1 2 3))

(check-for-bug :alltest-legacy-2436
  (caadar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  6)

(check-for-bug :alltest-legacy-2451
  (caaddr
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (q w e))

(check-for-bug :alltest-legacy-2466
  (cadaar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  5)

(check-for-bug :alltest-legacy-2481
  (cadadr
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  y)

(check-for-bug :alltest-legacy-2496
  (caddar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  nil)

(check-for-bug :alltest-legacy-2511
  (cadddr
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (a b c))

(check-for-bug :alltest-legacy-2526
  (cdaaar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (4))

(check-for-bug :alltest-legacy-2541
  (cdaadr
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (x))

(check-for-bug :alltest-legacy-2556
  (cdadar
   (list (list (list (list (list 1 2 3)
                           4)
                     5)
               (list 6 7))
         (list (list (list 'u 'v 'w)
                     'x)
               'y)
         (list (list 'q 'w 'e)
               'r)
         (list 'a 'b 'c)
         'e 'f 'g))
  (7))

(check-for-bug :alltest-legacy-2571
  (cons 1 2)
  (1 . 2))

(check-for-bug :alltest-legacy-2575
  (cons 'a (cons 'b (cons 'c 'nil)))
  (a b c))

(check-for-bug :alltest-legacy-2579
  (cons 'a (list 'b 'c 'd))
  (a b c d))

(check-for-bug :alltest-legacy-2583
  (tree-equal 5 (+ 2 3) :test (function eql))
  t)

(check-for-bug :alltest-legacy-2587
  (endp 'nil)
  t)

(check-for-bug :alltest-legacy-2591
  (endp (cons 'a 'b))
  nil)

(check-for-bug :alltest-legacy-2595
  (list-length (list 'a 'b 'c 'd))
  4)

(check-for-bug :alltest-legacy-2599
  (let ((x (list 'a 'b 'c))) (rplacd (last x) x)

       (list-length x))
  nil)

(check-for-bug :alltest-legacy-2605
  (nth 0 (list 'a 'b 'c 'd))
  a)

(check-for-bug :alltest-legacy-2609
  (first (list 1 2 3 4 5 6 7 8 9 10 11))
  1)

(check-for-bug :alltest-legacy-2613
  (second (list 1 2 3 4 5 6 7 8 9 10 11))
  2)

(check-for-bug :alltest-legacy-2617
  (third (list 1 2 3 4 5 6 7 8 9 10 11))
  3)

(check-for-bug :alltest-legacy-2621
  (fourth (list 1 2 3 4 5 6 7 8 9 10 11))
  4)

(check-for-bug :alltest-legacy-2625
  (fifth (list 1 2 3 4 5 6 7 8 9 10 11))
  5)

(check-for-bug :alltest-legacy-2629
  (sixth (list 1 2 3 4 5 6 7 8 9 10 11))
  6)

(check-for-bug :alltest-legacy-2633
  (seventh (list 1 2 3 4 5 6 7 8 9 10 11))
  7)

(check-for-bug :alltest-legacy-2637
  (eighth (list 1 2 3 4 5 6 7 8 9 10 11))
  8)

(check-for-bug :alltest-legacy-2641
  (ninth (list 1 2 3 4 5 6 7 8 9 10 11))
  9)

(check-for-bug :alltest-legacy-2645
  (tenth (list 1 2 3 4 5 6 7 8 9 10 11))
  10)

(check-for-bug :alltest-legacy-2649
  (rest (cons 'a 'b))
  b)

(check-for-bug :alltest-legacy-2653
  (nthcdr 1 (list 'a 'b 'c 'd))
  (b c d))

(check-for-bug :alltest-legacy-2657
  (last (list 1 2 3 4 5))
  (5))

(check-for-bug :alltest-legacy-2661
  (last (append (list 1 2 3) 4))
  (3 . 4))

(check-for-bug :alltest-legacy-2665
  (list 'a 'b 'c 'd)
  (a b c d))

(check-for-bug :alltest-legacy-2669
  (list* 'a 'b 'c 'd)
  (a b c . d))

(check-for-bug :alltest-legacy-2673
  (make-list 4 :initial-element 'o)
  (o o o o))

(check-for-bug :alltest-legacy-2677
  (make-list 3 :initial-element 'rah)
  (rah rah rah))

(check-for-bug :alltest-legacy-2681
  (append (list 'a 'b 'c)
          (list 'd 'e 'f) 'nil '(g))
  (a b c d e f g))

(check-for-bug :alltest-legacy-2686
  (copy-list (list 1 2 3 4 5))
  (1 2 3 4 5))

(check-for-bug :alltest-legacy-2690
  (copy-list (append (list 1 2 3) 4))
  (1 2 3 . 4))

(check-for-bug :alltest-legacy-2694
  (copy-alist (list 'a 'b))
  (a b))

(check-for-bug :alltest-legacy-2698
  (copy-alist (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))
  ((1 . a) (2 . b) (3 . c)))

(check-for-bug :alltest-legacy-2702
  (copy-alist (list (list 'a 'b) 'c (list 'd 'e)))
  ((a b) c (d e)))

(check-for-bug :alltest-legacy-2706
  (copy-tree (list 'a 'b
                   (list 'c
                         (list 'd)
                         (list 'e 'f))
                   'g))
  (a b (c (d) (e f)) g))

(check-for-bug :alltest-legacy-2714
  (revappend (list 'a 'b 'c) (list 'd 'e 'f))
  (c b a d e f))

(check-for-bug :alltest-legacy-2718
  (revappend (list 'a 'b 'c) 'i)
  (c b a . i))				;? error

(check-for-bug :alltest-legacy-2722
  (nreconc (list 'a 'b 'c) (list 'i 'j))
  (c b a i j))

;; nreconc

(check-for-bug :alltest-legacy-2728
  (setq aa nil)
  nil)

(check-for-bug :alltest-legacy-2732
  (push '1 aa)
  (1))

(check-for-bug :alltest-legacy-2736
  (push '2 aa)
  (2 1))

(check-for-bug :alltest-legacy-2740
  (pop aa)
  2)

(check-for-bug :alltest-legacy-2744
  (pop aa)
  1)

(check-for-bug :alltest-legacy-2748
  (pop aa)
  nil)

(check-for-bug :alltest-legacy-2752
  (setq aa (list 'b 'a))
  (b a))

(check-for-bug :alltest-legacy-2756
  (pushnew 'a aa)
  (b a))

(check-for-bug :alltest-legacy-2760
  (pushnew 'c aa)
  (c b a))

(check-for-bug :alltest-legacy-2764
  (pushnew 'u (car (setq xx (list nil 'kkk))))
  (u))

(check-for-bug :alltest-legacy-2768
  (pushnew 'u (car xx))
  (u))

(check-for-bug :alltest-legacy-2772
  (pushnew 'v (car xx))
  (v u))

(check-for-bug :alltest-legacy-2776
  (eval 'xx)
  ((v u) kkk))

(check-for-bug :alltest-legacy-2780
  (butlast (list 'a 'b 'c) 2)
  (a))

(check-for-bug :alltest-legacy-2784
  (nbutlast (list 'a 'b 'c 'd) 6)
  nil)

(check-for-bug :alltest-legacy-2788
  (nbutlast (list 'a 'b 'c 'd) 1)
  (a b c))

(check-for-bug :alltest-legacy-2792
  (ldiff (setq xx (list 'a 'b 'c 'd 'e))
         (cddr xx))
  (a b))

(check-for-bug :alltest-legacy-2797
  (ldiff (setq xx (append (list 'a 'b 'c 'd)
                          'e))
         (cddr xx))
  (a b))

(unintern 'xx)

(check-for-bug :alltest-legacy-2805
  (ldiff (append (list 'a 'b 'c 'd)
                 'e)
         'e)
  (a b c d))

(check-for-bug :alltest-ldiff-1
  (ldiff (cons 1 2)
         3)
  (1 . 2))

(check-for-bug :alltest-legacy-2811
  (let ((lists '#((a b c) (a b c . d)))
        (ld-res #(#(nil (a b) (a b c) (a b c) (a b c) (a b c) (a b c))
                  #(nil (a b) (a b c . d) (a b c . d) (a b c . d) (a b c)
                    (a b c . d))))
        (tp-res #(#(t t nil nil t nil nil) #(t t nil nil nil t nil))))
    (dotimes (i (length lists))
      (let* ((list (aref lists i)) (l-r (aref ld-res i)) (t-r (aref tp-res i))
             (objects (vector list (cddr list) (copy-list (cddr list))
                              '(f g h) '() 'd 'x)))
        (dotimes (j (length objects))
          (let ((object (aref objects j)))
            (unless (equal (tailp object list) (aref t-r j))
              (error "(tailp ~s ~s): ~s; should be: ~s"
                     object list (tailp object list) (aref t-r j)))
            (unless (equal (ldiff list object) (aref l-r j))
              (error "(ldiff ~s ~s): ~s; should be: ~s"
                     list object (ldiff list object) (aref l-r j))))))))
  nil)

;; rplaca, rplacd

(check-for-bug :alltest-legacy-2833
  (nsubst 'a 'b
          (list 'u 'b (list 'b) 'c)
          :test-not (lambda (x y)
                      (not (eql x y))))
  (u a (a) c))

(check-for-bug :alltest-legacy-2840
  (nsubst-if 'oo
             'numberp
             (list 'a 'b 'c (list 3 (list 4) 0)))
  (a b c (oo (oo) oo)))

(check-for-bug :alltest-legacy-2846
  (nsubst-if-not 'oo
                 #'(lambda (x)
                     (or (list x)
                         (symbolp x)))
                 (list 'a 'b 'c (list 3 (list 4) 0)))
  (a b c (3 (4) 0)))

(check-for-bug :alltest-legacy-2854
  (subst 'a 'b (list 'u 'b (list 'b) 'c)
         :test-not (lambda (x y)
                     (not (eql x y)))
         :key (lambda (u)
                (when (listp u)
                  (car u))))
  (u . a))

(check-for-bug :alltest-legacy-2863
  (subst-if 'nummmer
            'numberp
            (list (list 'a (list 7 (list 'v 6)))))

  ((a (nummmer (v nummmer)))))

(check-for-bug :alltest-legacy-2870
  (subst-if-not 'nummmer
                #'(lambda (x)
                    (or (listp x)
                        (numberp x)))
                (list (list 'a (list 7 (list 'v 6)))))
  ((nummmer (7 (nummmer 6)))))

(check-for-bug :alltest-legacy-2878
  (nsublis (list (cons (list 'a) 'uu)
                 (cons 'a 'ii))
           (list 'i (list 'a) 'a)
           :test
           (lambda (x y)
             (when (listp y)
               (eql x (car y)))))
  #+(or xcl allegro ecls) (i ii . ii)	; x aus der aliste, y ein blatt des baumes
  #+(or clisp cmu sbcl lucid) (i (uu) uu) ; x ein blatt, y aus der aliste
  #-(or xcl clisp cmu sbcl lucid allegro ecls) unknown)

(check-for-bug :alltest-legacy-2890
  (SUBLIS (QUOTE (((A) . UU) (A . II)))
          (QUOTE (I (A) A))
          :TEST (LAMBDA (X Y)
                  (IF (LISTP Y) (EQL X (CAR Y)))))
  #+nil

  (sublis (list (list (cons (list 'a) 'uu) (cons 'a 'ii)))
          (list 'i (list 'a) 'a)
          :test (lambda (x y)
                  (when (listp y)
                    (eql x (car y)))))
  #+(or xcl allegro lucid ecls) (i ii . ii) ; x aus der aliste, y ein blatt des baumes
  #+(or clisp cmu sbcl sbcl) (i (uu) uu) ; x ein blatt, y aus der aliste
  #-(or xcl clisp cmu sbcl lucid allegro ecls) unknown)

(check-for-bug :alltest-legacy-2906
  (member 'A
          (list (list 'A)
                (list 'B)
                (list 'A)
                (list 'C))
          :key
          'car)
  ((a) (b) (a) (c)))

(check-for-bug :alltest-legacy-2916
  (member-if 'numberp
             (list (list 'a)
                   (list 'b)
                   (list 3)
                   (list 'c))
             :key 'car)

  ((3) (c)))

(check-for-bug :alltest-legacy-2926
  (member-if-not 'numberp
                 (list (list 8)
                       (list 'a)
                       (list 'b)
                       (list 3)
                       (list 'c))
                 :key 'car)
  ((a) (b) (3) (c)))

(check-for-bug :alltest-legacy-2936
  (tailp (cddr (setq xx (list 'u 'i 'a 'b))) xx)
  t)

(unintern 'xx)

(check-for-bug :alltest-legacy-2942
  (tailp 'd (append (list 'a 'b 'c) 'd))
  t)

(check-for-bug :alltest-legacy-2946
  (adjoin 'a
          (list (list 'a)
                'b
                'c)
          :test 'equal)
  (a (a) b c))

(check-for-bug :alltest-legacy-2954
  (nunion (list 'a 'b 'c 'd)
          (list 'u 'i 'b 'a))
  #+xcl (a b c d u i)
  #+(or ecls clisp) (c d u i b a)
  #+(or allegro cmu sbcl) (d c u i b a)
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-2962
  (union (list 'a 'b 'c 'd)
         (list 'a 'd 'i 'v))
  #+xcl (v i a b c d)
  #+(or ecls clisp) (b c a d i v)
  #+(or allegro cmu sbcl) (c b a d i v)
  #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(check-for-bug :alltest-legacy-2970
  (intersection (list (list 'a 1)
                      (list 'a 2)
                      (list 'a 3))
                (list (list 'a 4)
                      (list 'a 2)
                      (list 'b 6)
                      (list 'c 7))
                :test 'equal)
  ((a 2)))

(check-for-bug :alltest-legacy-2981
  (sort
   (nintersection (list 'a 'b 'c 'd)
                  (list 'c 'd 'e 'f 'g)
                  :test-not (quote eql))
   #'string<
   :key #'symbol-name)
  (a b c d))

(check-for-bug :alltest-legacy-2988
  (sort
   (nset-difference (list 'a 'b 'c 'd)
                    (list 'i 'j 'c))
   #'string<
   :key #'symbol-name)
  (a b d))

(check-for-bug :alltest-legacy-2994
  (sort
   (nset-exclusive-or (list 'a 'b 'c)
                      (list 'i 'a 'd 'c))
   #'string<
   :key #'symbol-name)
  (b d i))

(check-for-bug :alltest-legacy-2999
  (sort
   (set-difference (list 'anton 'berta 'auto 'berlin)
                   (list 'amerilla)
                   :test (lambda (x y)
                           (eql (elt (symbol-name x) 0)
                                (elt (symbol-name y) 0))))
   #'string<
   :key #'symbol-name)
  (berlin berta))

(check-for-bug :alltest-legacy-3009
  (sort
   (set-exclusive-or (list 'anton 'anna 'emil)
                     (list 'berta 'auto 'august)
                     :test (lambda (x y)
                             (eql (elt (symbol-name x) 0)
                                  (elt (symbol-name y) 0))))
   #'string<
   :key #'symbol-name)
  (berta emil))

(check-for-bug :alltest-legacy-3018
  (subsetp (list 'a 'b) (list 'b 'a 'u 'i 'c 'd))
  t)

(check-for-bug :alltest-legacy-3022
  (acons 'a 'b (list (cons 'c 'd)))
  ((a . b) (c . d)))

(check-for-bug :alltest-legacy-3026
  (acons 'a 'b nil)
  ((a . b)))

(check-for-bug :alltest-legacy-3030
  (assoc 'a (list (list 'b 'c)
                  'a
                  (list (list 'a) 'u)
                  (list 'a 'i))
         :test-not (lambda (x y)
                     (when (atom y)
                       (eql y x))))
  #+allegro error
  #-allegro (b c))

(check-for-bug :alltest-legacy-3041
  (assoc-if 'symbolp
            (list (cons 'a 3)
                  (cons 3 'a)))
  (a . 3))

(check-for-bug :alltest-legacy-3047
  (assoc-if-not 'numberp (list (cons 'a 3)
                               (cons 3 'a)))
  (a . 3))

(check-for-bug :alltest-legacy-3052
  (pairlis (list 'a 'b 'c) (list 1 2 3))
  ((c . 3) (b . 2) (a . 1)))

(check-for-bug :alltest-legacy-3056
  (rassoc 'a (list (cons 1 'b) (cons 2 'a)))
  (2 . a))

(check-for-bug :alltest-legacy-3060
  (rassoc-if 'symbolp
             (list (cons 1 3) (cons 2 'a)))
  (2 . a))

(check-for-bug :alltest-legacy-3065
  (rassoc-if-not 'symbolp
                 (list (cons 1 3) (cons 2 'a)))
  (1 . 3))

;; kap 16 hash-tabellen
;; ----------------------------------------------------------------------------

(check-for-bug :alltest-legacy-3073
  (hash-table-p (make-hash-table :test #'eql
                                 :rehash-size 2
                                 :size 20))
  t)

(check-for-bug :alltest-legacy-3079
  (hash-table-p (make-hash-table :test #'eql
                                 :rehash-size 1.1
                                 :size 20))
  t)
;; clrhash, gethash, hash-table-count, maphash, remhash, sxhash,

;; <hs>/body/mac_with-hash_ble-iterator.html
(check-for-bug :alltest-legacy-3087
  (defun test-hash-table-iterator (hash-table)
    (let ((all-entries '())
          (generated-entries '())
          (unique (list nil)))
      (maphash #'(lambda (key value)
                   (push (list key value)
                         all-entries))
               hash-table)
      (with-hash-table-iterator
          (generator-fn hash-table)
        (loop
            (multiple-value-bind (more? key value)
                (generator-fn)
              (unless more? (return))
              (unless (eql value (gethash key hash-table unique))
                (error "Key ~S not found for value ~S" key value))
              (push (list key value) generated-entries))))
      (unless (= (length all-entries)
                 (length generated-entries)
                 (length (union all-entries
                                generated-entries
                                :key #'car
                                :test (hash-table-test hash-table))))
        (error "Generated entries and Maphash entries don't correspond"))
      t))
  test-hash-table-iterator)

(check-for-bug :alltest-legacy-3115
  (let ((tab (make-hash-table :test #'equal)))
    (setf (gethash "Richard" tab) "Gabriel")
    (setf (gethash "Bruno" tab) "Haible")
    (setf (gethash "Michael" tab) "Stoll")
    (setf (gethash "Linus" tab) "Torvalds")
    (setf (gethash "Richard" tab) "Stallman")
    (test-hash-table-iterator tab)
    )
  t)


;; This uses the Clisp specific, non ANSI, :initial-contents keyword
;; to make-hash-table.  Run this test only for Clisp.
#+clisp
(check-for-bug :alltest-equalp-string-hash
  (gethash "foo" (read-from-string
                  (prin1-to-string
                   (make-hash-table :test 'equalp :initial-contents
                                    '(("FOO" . "bar"))))))
  "bar")

;; kap 17 felder
;; ----------------------------------------------------------------------------

;; make-array, vector, aref, svref, array-element-type, array-rank,
;; array-dimension, array-dimensions, array-total-size, array-in-bounds-p,
;; array-row-major-index, adjustable-array-p,

;; array-rank-limit, array-dimension-limit, array-total-size-limit,


;; bit, sbit, bit-and, bit-andc1, bit-andc2, bit-eqv, bit-ior, bit-nand,
;;; bit-nor, bit-not, bit-orc1, bit-orc2, bit-xor,

;; array-has-fill-pointer-p, fill-pointer, vector-pop, vector-push,
;; vector-push-extend, adjust-array,

;; kap 18 strings
;; ----------------------------------------------------------------------------

;; char, schar, string=, string-equal, string/=, string<, string<=, string>,

;; string>=, string-greaterp, string-lessp, string-not-equal,
;; string-not-greaterp, string-not-lessp, make-string, string-left-trim,

;; string-right-trim, string-trim, string-upcase, string-capitalize,

;; string-downcase, nstring-capitalize, nstring-downcase, nstring-upcase,
;; string,

;; kap 19 strukturen
;; ----------------------------------------------------------------------------

;; defstruct,

(check-for-bug :alltest-legacy-3160
  (defstruct (ice-cream-factory
               (:constructor make-factory)
               (:constructor fabricate-factory
                             (&key (capacity 5)
                                   location
                                   (local-flavors
                                    (case location
                                      ((hawaii) '(pineapple macadamia guava))
                                      ((massachusetts) '(lobster baked-bean))
                                      ((california) '(ginger lotus avocado bean-sprout garlic))
                                      ((texas) '(jalapeno barbecue))))
                                   (flavors
                                    (subseq (append local-flavors
                                                    '(vanilla chocolate strawberry pistachio
                                                      maple-walnut peppermint))
                                            0 capacity))
                                   ((:own owner)))))
    (capacity 3)
    (flavors '(vanilla chocolate strawberry mango))
    (owner 'me))
  ice-cream-factory)

(check-for-bug :alltest-legacy-3181
  (let ((houston (fabricate-factory :capacity 4 :location 'texas)))
    (ice-cream-factory-flavors houston))
  (jalapeno barbecue vanilla chocolate))

(check-for-bug :alltest-legacy-3186
  (let ((cambridge (fabricate-factory :location 'massachusetts)))
    (ice-cream-factory-flavors cambridge))
  (lobster baked-bean vanilla chocolate strawberry))

(check-for-bug :alltest-legacy-3191
  (let ((seattle (fabricate-factory :local-flavors '(salmon))))
    (ice-cream-factory-flavors seattle))
  (salmon vanilla chocolate strawberry pistachio))

(check-for-bug :alltest-legacy-3196
  (let ((wheaton (fabricate-factory :capacity 4 :location 'illinois)))
    (ice-cream-factory-flavors wheaton))
  (vanilla chocolate strawberry pistachio))

(check-for-bug :alltest-legacy-3201
  (let ((pittsburgh (fabricate-factory :capacity 4)))
    (ice-cream-factory-flavors pittsburgh))
  (vanilla chocolate strawberry pistachio))

(check-for-bug :alltest-legacy-3206
  (let ((cleveland (make-factory :capacity 4)))
    (ice-cream-factory-flavors cleveland))
  (vanilla chocolate strawberry mango))

;; kap 20 eval
;; ----------------------------------------------------------------------------

;; eval, evalhook, *evalhook*, applyhook, *applyhook*,

(check-for-bug :alltest-legacy-3216
  (constantp -5)
  t)

(check-for-bug :alltest-legacy-3220
  (constantp (read-from-string "1.0e30"))
  t)

;; kap 21 streams
;; ----------------------------------------------------------------------------

;; make-synonym-stream, make-broadcast-stream, make-concatenated-stream,
;; make-two-way-stream, make-echo-stream, make-string-input-stream,
;; make-string-output-stream, get-output-stream-string, with-input-from-string,
;; with-open-stream, with-output-to-string,

(check-for-bug :alltest-legacy-3232
  (streamp *standard-input*)
  t)

(check-for-bug :alltest-legacy-3236
  (input-stream-p *terminal-io*)
  t)

;; output-stream-p, stream-element-type, close,

;; kap 22 ein- und ausgabe
;; ----------------------------------------------------------------------------

(check-for-bug :alltest-legacy-3245
  (readtablep *readtable*)
  t)

(check-for-bug :alltest-legacy-3249
  (readtablep 'progn)
  nil)

;; copy-readtable, read, *read-base*, read-byte, read-char, read-char-no-hang,

;; *read-default-float-format*, read-delimited-list, read-from-string, read-line,
;; read-preserving-whitespace, *read-suppress*, *readtable*, unread-char,

;; get-dispatch-macro-character, get-macro-character,
;; set-dispatch-macro-character, set-macro-character, set-syntax-from-char,
;; make-dispatch-macro-character,

(check-for-bug :alltest-legacy-3262
  (get-dispatch-macro-character #\# #\0)
  nil)

;; pprint, prin1, prin1-to-string, princ, princ-to-string, print, *print-array*,
;; *print-base*, *print-case*, *print-circle*, *print-escape*, *print-gensym*,

;; *print-length*, *print-level*, *print-pretty*, *print-radix*,

;; peek-char, listen, clear-input, clear-output, parse-integer,

;; write, write-byte, write-char, write-line, write-string, write-to-string,
;; y-or-n-p, yes-or-no-p,

;; terpri, finish-output, force-output, format, fresh-line,

;; kap 23 file-interface
;; ----------------------------------------------------------------------------

;; pathname, truename, parse-namestring, merge-pathnames,
;; *default-pathname-defaults*, make-pathname, pathnamep, pathname-device,
;; pathname-directory, pathname-host, pathname-name, pathname-type,
;; pathname-version, namestring, file-namestring, directory-namestring,

;; host-namestring, enough-namestring, user-homedir-pathname, open,
;; with-open-file, rename-file, delete-file, probe-file, file-write-date,

;; file-author, file-length, file-position, load, *load-verbose*, directory

;; kap 24 fehler
;; ----------------------------------------------------------------------------

;; cerror, error, *break-on-warnings*, warn, break, check-type, assert, etypecase,
;; ecase, ctypecase, ccase

;; kap 25 erweiterungen
;; ----------------------------------------------------------------------------

;; compile, disassemble, compile-file, documentation, trace, untrace, step, time,
;; describe, inspect, room, ed, dribble, apropos, apropos-list,
;; get-decoded-time, get-internal-real-time, get-internal-run-time,
;; get-universal-time, decode-universal-time, encode-universal-time,

;; internal-time-units-per-second, sleep, lisp-implementation-type,
;; lisp-implementation-version, machine-instance, machine-type, machine-version,

;; software-type, software-version, short-site-name, long-site-name, *features*,
;; identity

;; kap i systeminterne praedikate
;; ----------------------------------------------------------------------------
;; ? (sequencep (type-specifier-p (bit-array-p
;; ? (adjustable-vector-with-fill-pointer-p (alistp (declaration-specifier-p


#-sbcl
(check-for-bug :alltest-legacy-3318
  (fixnump 10)                          ;?
  t)					;?

;; kap ii systeminterne atome
;; ----------------------------------------------------------------------------

;; case-every, comment, cond-every, displace, return, return-from, access, boole,
;; call-arguments-limit, defun, errset, *errset*, *macroexpand-hook*, *package*,
;; *random-state*, *save-old-definition-when-redefined*,
