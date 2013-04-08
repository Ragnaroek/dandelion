;;; based on v1.5 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :setf-legacy-4
  (setf li1 '(a (b) ((c) (d)) )  vec1 '#(0 1 2 3))
  #(0 1 2 3))

(check-for-bug :setf-legacy-8
  (setf pa 'old)
  old)

(check-for-bug :setf-legacy-12
  (psetf pa 'new pao pa)
  nil)

(check-for-bug :setf-legacy-16
  pa
  new)

(check-for-bug :setf-legacy-20
  pao
  old)

(check-for-bug :setf-legacy-24
  (setf (nth 1 li1) (quote uu))
  uu)

(check-for-bug :setf-legacy-28
  (eval (quote li1))
  (a uu ((c) (d))))

(check-for-bug :setf-legacy-32
  (setf (elt li1 1) (quote oo))
  oo)

(check-for-bug :setf-legacy-36
  (setf (elt vec1 1) (quote oo))
  oo)

(check-for-bug :setf-legacy-40
  (eval (quote li1))
  (a oo ((c) (d))))

(check-for-bug :setf-legacy-44
  (eval (quote vec1))
  #(0 oo 2 3))

(check-for-bug :setf-legacy-48
  (setf (rest li1) (quote ((ww))))
  ((ww)))

(check-for-bug :setf-legacy-52
  (eval (quote li1))
  (a (ww)))

(check-for-bug :setf-legacy-56
  (setf (first li1) (quote aa))
  aa)

(check-for-bug :setf-legacy-60
  (first li1)
  aa)

(check-for-bug :setf-legacy-64
  (setf (second li1) (quote bb))
  bb)

(check-for-bug :setf-legacy-68
  (eval (quote li1))
  (aa bb))

(check-for-bug :setf-legacy-72
  (setf (third li1) (quote bb))
  type-error)

(check-for-bug :setf-legacy-76
  (eval (quote li1))
  (aa bb))


(check-for-bug :setf-legacy-81
  (setf (rest li1) (quote (2 3 4 5 6 7 8 9 10)))
  (2 3 4 5 6 7 8 9 10))

(check-for-bug :setf-legacy-85
  (setf (second li1) 22)
  22)

(check-for-bug :setf-legacy-89
  (eval (quote li1))
  (aa 22 3 4 5 6 7 8 9 10))

(check-for-bug :setf-legacy-93
  (setf (third li1) (quote 33))
  33)

(check-for-bug :setf-legacy-97
  (setf (fourth li1) (quote 44))
  44)

(check-for-bug :setf-legacy-101
  (setf (fifth li1) (quote 55))
  55)

(check-for-bug :setf-legacy-105
  (setf (sixth li1) (quote 66))
  66)

(check-for-bug :setf-legacy-109
  (setf (seventh li1) (quote 77))
  77)

(check-for-bug :setf-legacy-113
  (setf (eighth li1) (quote 88))
  88)

(check-for-bug :setf-legacy-117
  (setf (ninth li1) (quote 99))
  99)

(check-for-bug :setf-legacy-121
  (setf (tenth li1) (quote 1010))
  1010)

(check-for-bug :setf-legacy-125
  (eval (quote li1))
  (aa 22 33 44 55 66 77 88 99 1010))

(check-for-bug :setf-legacy-129
  (setf (first li1) (quote (((a)))))
  (((a))))

(check-for-bug :setf-legacy-133
  (setf (caaar li1) (quote uu))
  uu)

(check-for-bug :setf-legacy-137
  (caaar li1)
  uu)

(check-for-bug :setf-legacy-141
  (car li1)
  ((uu)))

(check-for-bug :setf-legacy-145
  (setf (caar li1) (quote oo))
  oo)

(check-for-bug :setf-legacy-149
  (eval (quote li1))
  ((oo) 22 33 44 55 66 77 88 99 1010))

(check-for-bug :setf-legacy-153
  (setf (car li1) (quote ii))
  ii)

(check-for-bug :setf-legacy-157
  (eval (quote li1))
  (ii 22 33 44 55 66 77 88 99 1010))

(check-for-bug :setf-legacy-161
  (setf (cdddr li1) (quote pp))
  pp)

(check-for-bug :setf-legacy-165
  (eval (quote li1))
  (ii 22 33 . pp))

(check-for-bug :setf-legacy-169
  (setf (caddr li1) (quote 333))
  333)

(check-for-bug :setf-legacy-173
  (eval (quote li1))
  (ii 22 333 . pp))

(check-for-bug :setf-legacy-177
  (setf (svref vec1 2) (quote kk))
  kk)

(check-for-bug :setf-legacy-181
  (eval (quote vec1))
  #(0 oo kk 3))

(check-for-bug :setf-legacy-185
  (setf (get (quote a) (quote b)) (quote uu))
  uu)

(check-for-bug :setf-legacy-189
  (get (quote a) (quote b))
  uu)

(check-for-bug :setf-legacy-193
  (setf (getf (cadr (setq xx (quote (aaa (i1 v1 i2 v2))))) (quote i2))

        (quote v222))
  v222)

(check-for-bug :setf-legacy-199
  (eval (quote xx))
  (aaa (i1 v1 i2 v222)))

(check-for-bug :setf-legacy-203
  (getf (cadr xx) (quote i2))
  v222)

(check-for-bug :setf-legacy-207
  (getf (cadr xx) (quote i1))
  v1)

(check-for-bug :setf-legacy-211
  (setf (documentation (quote beispiel) (quote typ1)) "doc 1")
  "doc 1")

(check-for-bug :setf-legacy-215
  (setf (documentation (quote beispiel) (quote typ2)) "doc 2")
  "doc 2")

(check-for-bug :setf-legacy-219
  (documentation (quote beispiel) (quote typ2))
  #+xcl (typ2 . "doc 2")
  #-xcl "doc 2")

(check-for-bug :setf-legacy-224
  (setf (documentation (quote beispiel) (quote typ2)) "doc 3")
  "doc 3")

(check-for-bug :setf-legacy-228
  (documentation (quote beispiel) (quote typ2))
  #+xcl (typ2 . "doc 3")
  #-xcl "doc 3")

(check-for-bug :setf-legacy-233
  (symbol-plist 'beispiel)
  #+xcl (documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
  #+clisp (system::documentation-strings (typ2 "doc 3" typ1 "doc 1"))
  #+allegro (excl::%documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
  #+(or cmu ecls) nil
  #-(or xcl clisp allegro cmu ecls) unknown)

(check-for-bug :setf-legacy-241
  (setf (symbol-value (quote xx)) (quote voelligneu))
  voelligneu)

(check-for-bug :setf-legacy-245
  (eval (quote xx))
  voelligneu)

(check-for-bug :setf-legacy-249
  (progn
    (setf (symbol-function (quote ff))
          (coerce (quote (lambda (x) (print x) (quote hello))) (quote function)))
    nil)
  nil)

(check-for-bug :setf-legacy-256
  (ff 5)
  hello)

(check-for-bug :setf-legacy-260
  (defun xx nil 'a)
  xx)

(check-for-bug :setf-legacy-264
  (progn (setf (symbol-function 'xx1) (symbol-function 'xx)) nil)
  nil)

(check-for-bug :setf-legacy-268
  (xx1)
  a)

(check-for-bug :setf-legacy-272
  (setq l '(a 1 c d))
  (a 1 c d))

(check-for-bug :setf-legacy-276
  (setf (the integer (cadr l)) 100)
  100)

(check-for-bug :setf-legacy-280
  l
  (a 100 c d))

(check-for-bug :setf-legacy-284
  (progn (setf a (make-hash-table)) t)
  t)

(check-for-bug :setf-legacy-288
  (setf (gethash 'color a) 'brown)
  brown)

(check-for-bug :setf-legacy-292
  (gethash 'color a)
  brown)

(check-for-bug :setf-legacy-296
  (defstruct schiff masse)
  schiff)

(check-for-bug :setf-legacy-300
  (progn (setf s1 (make-schiff)) nil)
  nil)

(check-for-bug :setf-legacy-304
  (setf (schiff-masse s1) 500)
  500)

(check-for-bug :setf-legacy-308
  (schiff-masse s1)
  500)

(check-for-bug :setf-legacy-312
  (defmacro setf-test (v) `(svref ,v 3))
  setf-test)

(check-for-bug :setf-legacy-316
  (progn (setf (macro-function 'setf-test1) (macro-function 'setf-test)) nil)
  nil)

(check-for-bug :setf-legacy-320
  (setf (setf-test vec1) 'oho)
  oho)

(check-for-bug :setf-legacy-324
  (eval 'vec1)
  #(0 oo kk oho))

(check-for-bug :setf-legacy-328
  (setf (setf-test1 vec1) 'hihi)
  hihi)

(check-for-bug :setf-legacy-332
  (eval 'vec1)
  #(0 oo kk hihi))

;;  (setf (displace ?? (svref vec1 3)) "aha")
;;  aha

;;  (eval 'vec1)
;;  #(0 oo kk aha)

(check-for-bug :setf-legacy-342
  (progn (setf a (make-array '(4 3))) nil)
  nil)

(check-for-bug :setf-legacy-346
  (aref a 2 2)
  #+(or xcl cmu) 0
  #+(or clisp akcl allegro ecls) nil
  #-(or xcl clisp akcl allegro cmu ecls) unknown)

(check-for-bug :setf-legacy-352
  (setf (apply #'aref a '(2 2)) 'xxxx)
  xxxx)

(check-for-bug :setf-legacy-356
  (aref a 2 2)
  xxxx)

(check-for-bug :setf-legacy-360
  (setf (aref '#(a b c) 1) (quote ii))
  ii)

(check-for-bug :setf-legacy-364
  (setf b #*101010)
  #*101010)

(check-for-bug :setf-legacy-368
  (bit b 2)
  1)

(check-for-bug :setf-legacy-372
  (setf (bit b 2) 0)
  0)

(check-for-bug :setf-legacy-376
  (bit b 2)
  0)

(check-for-bug :setf-legacy-380
  (setf (sbit b 2) 1)
  1)

(check-for-bug :setf-legacy-384
  (sbit b 2)
  1)

(check-for-bug :setf-legacy-388
  (progn (setf a (make-array 5 :fill-pointer t)) t)
  t)

(check-for-bug :setf-legacy-392
  (fill-pointer a)
  5)

(check-for-bug :setf-legacy-396
  (setf (fill-pointer a) 3)
  3)

(check-for-bug :setf-legacy-400
  (fill-pointer a)
  3)

(check-for-bug :setf-legacy-404
  (let ((str (copy-seq "hose")))
    str)
  "hose")

(check-for-bug :setf-legacy-409
  (let ((str (copy-seq "hose")))
    (setf (char str 0) #\d))
  #\d)

(check-for-bug :setf-legacy-414
  (let ((str (copy-seq "hose")))
    (setf (char str 0) #\d)
    str)
  "dose")

(check-for-bug :setf-legacy-420
  (let ((str (copy-seq "hose")))
    (setf (char str 0) #\d)
    (setf str "aaaxxxccc"))
  "aaaxxxccc")

(check-for-bug :setf-legacy-426
  (let ((str (copy-seq "hose")))
    (setf (char str 0) #\d)
    (setf str (copy-seq "aaaxxxccc"))
    (setf (subseq str 3 6) "bbb"))
  "bbb")

(check-for-bug :setf-legacy-433
  (let ((str (copy-seq "hose")))
    (setf (char str 0) #\d)
    (setf str (copy-seq "aaaxxxccc"))
    (setf (subseq str 3 6) "bbb")
    str)
  "aaabbbccc")

(check-for-bug :setf-legacy-441
  (setq x (list 'a 'b 'c))
  (a b c))

(check-for-bug :setf-legacy-445
  (shiftf (cadr x) 'z)
  b)

(check-for-bug :setf-legacy-449
  x
  (a z c))

(check-for-bug :setf-legacy-453
  (shiftf (cadr x) (cddr x) 'q)
  z)

(check-for-bug :setf-legacy-457
  x
  (a (c) . q))

(check-for-bug :setf-legacy-461
  (progn
    (defun ad (x) (values (car x) (cdr x)))
    (defsetf ad (x) (a b) `(setf (values (car ,x) (cdr ,x)) (values ,a ,b)))
    (setq x (cons 1 2) y 3 z 4 w 5 v 6 u 7))
  7)

(check-for-bug :setf-legacy-468
  (rotatef (ad x) (values y z) (values w v u))
  nil)

(check-for-bug :setf-legacy-472
  x
  (3 . 4))

(check-for-bug :setf-legacy-476
  (list y z w v u)
  (5 6 1 2 nil))

(check-for-bug :setf-legacy-480
  (multiple-value-list
      (shiftf (ad x)
              (values y z w)
              (values v u)
              (floor 89 10)))
  (3 4)
  "(ad x) ->  3 and  4)
 (y -> 5 z -> 6 w -> 1)
 (v -> 2 u-> nil)
 (floor 89 10) -> 8 and 9

so after shifting we expect:
x -> (5 . 6)
(y -> 2 z-> nil w -> nil)
(v -> 8 u -> 9)

and we return 3 and 4")

(check-for-bug :setf-legacy-499
  x
  (5 . 6)
  "check the shiftf result")

(check-for-bug :setf-legacy-504
  (list y z w v u)
  (2 nil nil 8 9)
  "check the shiftf result")

(check-for-bug :setf-legacy-509
  (progn (defsetf my-subseq (sequence start &optional end) (new-sequence)
           `(progn (replace ,sequence ,new-sequence
                            :start1 ,start :end1 ,end)
                   ,new-sequence)) t)
  t)

(check-for-bug :setf-legacy-516
  (let (s)
    (setf s (copy-seq "asdfg")
          (my-subseq s 1 3) "xy"))
  "xy")

(check-for-bug :setf-legacy-522
  (let (s)
    (setf s (copy-seq "asdfg")
          (my-subseq s 1 3) "xy")
    s)
  "axyfg")

