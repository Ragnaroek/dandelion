;;; based on v1.4 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :lists152-legacy-4
  (endp 'nil)
  t)

(check-for-bug :lists152-legacy-8
  (endp (cons 'a 'b))
  nil)

(check-for-bug :lists152-legacy-12
  (endp (append (list 'a 'b) 'c))
  nil)

(check-for-bug :lists152-legacy-16
  (endp (list 'a 'b 'c))
  nil)

(check-for-bug :lists152-legacy-20
  (endp (list 'a 'b 'c 'd))
  nil)

(check-for-bug :lists152-legacy-24
  (endp (append (list 'a 'b 'c)  'd))
  nil)

(check-for-bug :lists152-legacy-28
  (endp (list ''nil ''nil))
  nil)

(check-for-bug :lists152-legacy-32
  (list-length 'nil)
  0)

(check-for-bug :lists152-legacy-36
  (list-length (cons 'a 'b))
  #+xcl 1
  #-xcl type-error)

(check-for-bug :lists152-legacy-41
  (list-length (list 'a 'b 'c 'd))
  4)

(check-for-bug :lists152-legacy-45
  (list-length
   (list 'a (list 'b 'c) 'd))
  3)

(check-for-bug :lists152-legacy-50
  (let ((x (list 'a 'b 'c)))
    (rplacd (last x)
            x)
    (list-length x))
  nil)

(check-for-bug :lists152-legacy-57
  (nth 0
       (list 'a 'b 'c 'd))
  a)

(check-for-bug :lists152-legacy-62
  (nth 1
       (list 'a 'b 'c 'd))
  b)

(check-for-bug :lists152-legacy-67
  (nth 3
       (list 'a 'b 'c 'd))
  d)

(check-for-bug :lists152-legacy-72
  (nth 5
       (list 'a 'b 'c 'd))
  nil)

(check-for-bug :lists152-legacy-77
  (nth -2
       (list 'a 'b 'c 'd))
  type-error)

(check-for-bug :lists152-legacy-82
  (nth 0 'nil)
  nil)

(check-for-bug :lists152-legacy-86
  (nth 2 'nil)
  nil)

(check-for-bug :lists152-legacy-90
  (first (list 1 2 3 4 5 6 7 8 9 10 11))
  1)

(check-for-bug :lists152-legacy-94
  (second (list 1 2 3 4 5 6 7 8 9 10 11))
  2)

(check-for-bug :lists152-legacy-98
  (third (list 1 2 3 4 5 6 7 8 9 10 11))
  3)

(check-for-bug :lists152-legacy-102
  (fourth (list 1 2 3 4 5 6 7 8 9 10 11))
  4)

(check-for-bug :lists152-legacy-106
  (fifth (list 1 2 3 4 5 6 7 8 9 10 11))
  5)

(check-for-bug :lists152-legacy-110
  (sixth (list 1 2 3 4 5 6 7 8 9 10 11))
  6)

(check-for-bug :lists152-legacy-114
  (seventh (list 1 2 3 4 5 6 7 8 9 10 11))
  7)

(check-for-bug :lists152-legacy-118
  (eighth (list 1 2 3 4 5 6 7 8 9 10 11))
  8)

(check-for-bug :lists152-legacy-122
  (ninth (list 1 2 3 4 5 6 7 8 9 10 11))
  9)

(check-for-bug :lists152-legacy-126
  (tenth (list 1 2 3 4 5 6 7 8 9 10 11))
  10)

(check-for-bug :lists152-legacy-130
  (first (list 1 2 3))
  1)

(check-for-bug :lists152-legacy-134
  (second (list 1 2 3))
  2)

(check-for-bug :lists152-legacy-138
  (third (list 1 2 3))
  3)

(check-for-bug :lists152-legacy-142
  (fourth (list 1 2 3))
  nil)

(check-for-bug :lists152-legacy-146
  (fifth (list 1 2 3))
  nil)

(check-for-bug :lists152-legacy-150
  (sixth (list 1 2 3))
  nil)

(check-for-bug :lists152-legacy-154
  (seventh (list 1 2 3))
  nil)

(check-for-bug :lists152-legacy-158
  (eighth (list 1 2 3))
  nil)

(check-for-bug :lists152-legacy-162
  (ninth (list 1 2 3))
  nil)

(check-for-bug :lists152-legacy-166
  (tenth (list 1 2 3))
  nil)

(check-for-bug :lists152-legacy-170
  (first 'nil)
  nil)

(check-for-bug :lists152-legacy-174
  (second 'nil)
  nil)

(check-for-bug :lists152-legacy-178
  (third 'nil)
  nil)

(check-for-bug :lists152-legacy-182
  (fourth 'nil)
  nil)

(check-for-bug :lists152-legacy-186
  (fifth 'nil)
  nil)

(check-for-bug :lists152-legacy-190
  (sixth 'nil)
  nil)

(check-for-bug :lists152-legacy-194
  (seventh 'nil)
  nil)

(check-for-bug :lists152-legacy-198
  (eighth 'nil)
  nil)

(check-for-bug :lists152-legacy-202
  (ninth 'nil)
  nil)

(check-for-bug :lists152-legacy-206
  (tenth 'nil)
  nil)

(check-for-bug :lists152-legacy-210
  (rest (list 1 2 3 4 5))
  (2 3 4 5))

(check-for-bug :lists152-legacy-214
  (rest 'nil)
  nil)

(check-for-bug :lists152-legacy-218
  (rest (cons 'a 'b))
  b)

(check-for-bug :lists152-legacy-222
  (rest (append (list 1 2 3) 4))
  (2 3 . 4))

(check-for-bug :lists152-legacy-226
  (nthcdr 0
          (list 'a 'b 'c 'd))
  (a b c d))

(check-for-bug :lists152-legacy-231
  (nthcdr 1
          (list 'a 'b 'c 'd))
  (b c d))

(check-for-bug :lists152-legacy-236
  (nthcdr 3
          (list 'a 'b 'c 'd))
  (d))

(check-for-bug :lists152-legacy-241
  (nthcdr 5
          (list 'a 'b 'c 'd))
  nil)

(check-for-bug :lists152-legacy-246
  (nthcdr -2
          (list 'a 'b 'c 'd))
  type-error)

(check-for-bug :lists152-legacy-251
  (nthcdr 0 'nil)
  nil)

(check-for-bug :lists152-legacy-255
  (nthcdr 2 'nil)
  nil)

(check-for-bug :lists152-legacy-259
  (last (list 1 2 3 4 5))
  (5))

(check-for-bug :lists152-legacy-263
  (last 'nil)
  nil)

(check-for-bug :lists152-legacy-267
  (last (cons 'a 'b))
  (a . b))

(check-for-bug :lists152-legacy-271
  (last (append (list 1 2 3) 4))
  (3 . 4))

(check-for-bug :lists152-legacy-275
  (list 'a 'b 'c 'd)
  (a b c d))

(check-for-bug :lists152-legacy-279
  (list 'a)
  (a))

(check-for-bug :lists152-legacy-283
  (list (list 'a 'b)
        (list 'c 'd))
  ((a b)
   (c d)))

(check-for-bug :lists152-legacy-289
  (list 'a 'nil)
  (a nil))

(check-for-bug :lists152-legacy-293
  (list 'nil 'a)
  (nil a))

(check-for-bug :lists152-legacy-297
  (list 'nil 'nil)
  (nil nil))

(check-for-bug :lists152-legacy-301
  (list)
  nil)

(check-for-bug :lists152-legacy-305
  (list 3 4 'a
        (car (cons 'b 'c))
        (+ 6 -2))
  (3 4 a b 4))

(check-for-bug :lists152-legacy-311
  (list* 'a 'b 'c 'd)
  (a b c . d))

(check-for-bug :lists152-legacy-315
  (list* 'a)
  a)

(check-for-bug :lists152-legacy-319
  (list* (list 'a 'b)
         (list 'c 'd))
  ((a b)
   c d))

(check-for-bug :lists152-legacy-325
  (list* 'a 'nil)
  (a))

(check-for-bug :lists152-legacy-329
  (list* 'nil 'a)
  (nil . a))

(check-for-bug :lists152-legacy-333
  (list* 'nil 'nil)
  (nil))

(check-for-bug :lists152-legacy-337
  (list*)
  program-error)

(check-for-bug :lists152-legacy-341
  (list* 3 4 'a
         (car (cons 'b 'c))
         (+ 6 -2))
  (3 4 a b . 4))

(check-for-bug :lists152-legacy-347
  (list* 'a 'b 'c
         (list 'd 'e 'f))
  (a b c d e f))

(check-for-bug :lists152-legacy-352
  (list* x)
  unbound-variable)

(check-for-bug :lists152-legacy-356
  (list* 'nil)
  nil)

(check-for-bug :lists152-legacy-360
  (make-list 5)
  (nil nil nil nil nil))

(check-for-bug :lists152-legacy-364
  (make-list 5 :initial-element)
  program-error)

(check-for-bug :lists152-legacy-368
  (make-list 3 :initial-element 'rah)
  (rah rah rah))

(check-for-bug :lists152-legacy-372
  (make-list 0)
  nil)

(check-for-bug :lists152-legacy-376
  (make-list 0 :initial-element 'aaa)
  nil)

(check-for-bug :lists152-legacy-380
  (make-list 5 :initial-element 'nil)
  (nil nil nil nil nil))

(check-for-bug :lists152-legacy-384
  (make-list)
  program-error)

(check-for-bug :lists152-legacy-388
  (append (list 'a 'b 'c)
          (list 'd 'e 'f)
          'nil
          (list 'g))
  (a b c d e f g))

(check-for-bug :lists152-legacy-395
  (append (list 'a 'b 'c)
          'd)
  (a b c . d))

(check-for-bug :lists152-legacy-400
  (append 'a 'b)
  error)

(check-for-bug :lists152-legacy-404
  (append 'a 'nil)
  error)

(check-for-bug :lists152-legacy-408
  (append 'nil 'nil)
  nil)

(check-for-bug :lists152-legacy-412
  (append 'nil 'a)
  #+xcl error
  #-xcl a)

(check-for-bug :lists152-legacy-417
  (append 'nil
          (list 'a 'b 'c))
  (a b c))

(check-for-bug :lists152-legacy-422
  (setq x
        (list 'a 'b 'c))
  (a b c))

(check-for-bug :lists152-legacy-427
  (setq y
        (list 'd 'e 'f))
  (d e f))

(check-for-bug :lists152-legacy-432
  (setq r
        (append x y))
  (a b c d e f))

(check-for-bug :lists152-legacy-437
  x
  (a b c))

(check-for-bug :lists152-legacy-441
  y
  (d e f))

(check-for-bug :lists152-legacy-445
  (eq (cdddr r)
      y)
  t)

(check-for-bug :lists152-legacy-450
  (copy-list (list 1 2 3 4 5))
  (1 2 3 4 5))

(check-for-bug :lists152-legacy-454
  (copy-list 'nil)
  nil)

(check-for-bug :lists152-legacy-458
  (copy-list (cons 'a 'b))
  (a . b))

(check-for-bug :lists152-legacy-462
  (copy-list (append (list 1 2 3) 4))
  (1 2 3 . 4))

(check-for-bug :lists152-legacy-466
  (setq l
        (list 1 2 3 4 5))
  (1 2 3 4 5))

(check-for-bug :lists152-legacy-471
  (eq l
      (copy-list l))
  nil)

(check-for-bug :lists152-legacy-476
  (eql l
       (copy-list l))
  nil)

(check-for-bug :lists152-legacy-481
  (equal l
         (copy-list l))
  t)

(check-for-bug :lists152-legacy-486
  (equalp l
          (copy-list l))
  t)

(check-for-bug :lists152-legacy-491
  (copy-alist 'a)
  #-clisp error
  #+clisp a)

(check-for-bug :lists152-legacy-496
  (copy-alist 'nil)
  nil)

(check-for-bug :lists152-legacy-500
  (copy-alist 5)
  #-clisp error
  #+clisp 5)

(check-for-bug :lists152-legacy-505
  (copy-alist (list 'a 'b))
  #+(or xcl clisp allegro cmu sbcl) (a b)
  #+(or ecls gcl) error
  #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists152-legacy-511
  (copy-alist (list (cons 1 'a)
                    (cons 2 'b)
                    (cons 3 'c)))
  ((1 . a)
   (2 . b)
   (3 . c)))

(check-for-bug :lists152-legacy-519
  (setq x
        (list (cons 1 'a)
              (cons 2 'b)
              (cons 3 'c)))
  ((1 . a)
   (2 . b)
   (3 . c)))

(check-for-bug :lists152-legacy-528
  (eq x
      (copy-alist x))
  nil)

(check-for-bug :lists152-legacy-533
  (eql x
       (copy-alist x))
  nil)

(check-for-bug :lists152-legacy-538
  (equal x
         (copy-alist x))
  t)

(check-for-bug :lists152-legacy-543
  (eq (cadr x)
      (cadr (copy-alist x)))
  nil)

(check-for-bug :lists152-legacy-548
  (eql (cadr x)
       (cadr (copy-alist x)))
  nil)

(check-for-bug :lists152-legacy-553
  (equal (cadr x)
         (cadr (copy-alist x)))
  t)

(check-for-bug :lists152-legacy-558
  (copy-alist (list (cons 1 2))
              (list (cons a b)))
  error)

(check-for-bug :lists152-legacy-563
  (copy-alist (list (list 'a 'b)
                    'c
                    (list 'd 'e)))
  #+(or xcl clisp allegro cmu sbcl) ((a b) c (d e))
  #+(or gcl ecls) error
  #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists152-legacy-571
  (copy-tree 'x)
  x)

(check-for-bug :lists152-legacy-575
  (copy-tree 5)
  5)

(check-for-bug :lists152-legacy-579
  (copy-tree (list 'a 'b))
  (a b))

(check-for-bug :lists152-legacy-583
  (copy-tree (list 'a 'b
                   (list 'c
                         (list 'd)
                         (list 'e 'f))
                   'g))
  (a b
     (c (d)
        (e f))
     g))

(check-for-bug :lists152-legacy-594
  (copy-tree (list (cons 1 'e)
                   (cons 2 'f)))
  ((1 . e)
   (2 . f)))

(check-for-bug :lists152-legacy-600
  (copy-tree #*001)
  #*001)

(check-for-bug :lists152-legacy-604
  (setq x
        (list 'a 'b
              (list 'c 'd)
              'e))
  (a b
     (c d)
     e))

(check-for-bug :lists152-legacy-613
  (eq x
      (copy-tree x))
  nil)

(check-for-bug :lists152-legacy-618
  (eql x
       (copy-tree x))
  nil)

(check-for-bug :lists152-legacy-623
  (equal x
         (copy-tree x))
  t)

(check-for-bug :lists152-legacy-628
  (eq (cdaddr x)
      (cdaddr (copy-tree x)))
  nil)

(check-for-bug :lists152-legacy-633
  (eql (cdaddr x)
       (cdaddr (copy-tree x)))
  nil)

(check-for-bug :lists152-legacy-638
  (equal (cdaddr x)
         (cdaddr (copy-tree x)))
  t)

(check-for-bug :lists152-legacy-643
  (revappend (list 'a 'b 'c)
             (list 'd 'e 'f)
             'nil
             (list 'g))
  program-error)

(check-for-bug :lists152-legacy-650
  (revappend (list 'a 'b 'c)
             'd)
  (c b a . d))

(check-for-bug :lists152-legacy-655
  (revappend 'a 'b)
  #-clisp type-error
  #+clisp b)

(check-for-bug :lists152-legacy-660
  (revappend 'a 'nil)
  #-clisp type-error
  #+clisp nil)

(check-for-bug :lists152-legacy-665
  (revappend 'nil 'nil)
  nil)

(check-for-bug :lists152-legacy-669
  (revappend 'nil 'a)
  a)

(check-for-bug :lists152-legacy-673
  (revappend 'nil
             (list 'a 'b 'c))
  (a b c))

(check-for-bug :lists152-legacy-678
  (revappend (list 'a 'b 'c)
             (list 'd 'e 'f))
  (c b a d e f))

(check-for-bug :lists152-legacy-683
  (revappend (list 'd 'e 'f)
             (list 'a 'b 'c))
  (f e d a b c))

(check-for-bug :lists152-legacy-688
  (eql (revappend (list 'a 'b 'c)
                  (list 'd 'e 'f))
       (append (reverse (list 'a 'b 'c))
               (list 'd 'e 'f)))
  nil)

(check-for-bug :lists152-legacy-695
  (equal (revappend (list 'a 'b 'c)
                    (list 'd 'e 'f))
         (append (reverse (list 'a 'b 'c))
                 (list 'd 'e 'f)))
  t)

(check-for-bug :lists152-legacy-702
  (setq x
        (list 'a 'b 'c))
  (a b c))

(check-for-bug :lists152-legacy-707
  (setq y
        (list 'd 'e 'f))
  (d e f))

(check-for-bug :lists152-legacy-712
  (setq r
        (revappend x y))
  (c b a d e f))

(check-for-bug :lists152-legacy-717
  x
  (a b c))

(check-for-bug :lists152-legacy-721
  y
  (d e f))

(check-for-bug :lists152-legacy-725
  (eq (cdddr r)
      y)
  t)

(check-for-bug :lists152-legacy-730
  (setq x
        (list 'a 'b 'c)
        y
        (list 'd 'e 'f))
  (d e f))

(check-for-bug :lists152-legacy-737
  (nconc x y)
  (a b c d e f))

(check-for-bug :lists152-legacy-741
  x
  (a b c d e f))

(check-for-bug :lists152-legacy-745
  (eq (cdddr x)
      y)
  t)

(check-for-bug :lists152-legacy-750
  (setq x
        (list 'a 'b 'c)
        y
        (list 'd 'e 'f)
        z
        (list 'g 'h 'i))
  (g h i))

(check-for-bug :lists152-legacy-759
  (nconc)
  nil)

(check-for-bug :lists152-legacy-763
  (nconc x)
  (a b c))

(check-for-bug :lists152-legacy-767
  (nconc nil)
  nil)

(check-for-bug :lists152-legacy-771
  (nconc nil nil)
  nil)

(check-for-bug :lists152-legacy-775
  (nconc x nil)
  (a b c))

(check-for-bug :lists152-legacy-779
  (nconc nil nil nil nil)
  nil)

(check-for-bug :lists152-legacy-783
  (nconc nil nil x nil)
  (a b c))

(check-for-bug :lists152-legacy-787
  (nconc x nil y nil z nil)
  (a b c d e f g h i))

(check-for-bug :lists152-legacy-791
  x
  (a b c d e f g h i))

(check-for-bug :lists152-legacy-795
  y
  (d e f g h i))

(check-for-bug :lists152-legacy-799
  z
  (g h i))

(check-for-bug :lists152-legacy-803
  (eq (cdddr x)
      y)
  t)

(check-for-bug :lists152-legacy-808
  (eq (cdddr y)
      z)
  t)

(check-for-bug :lists152-legacy-813
  (nconc (list 1 2)
         'a)
  #+xcl error
  #+(or clisp akcl allegro cmu sbcl ecls) (1 2 . a)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists152-legacy-820
  (nconc 'a)
  #+xcl error
  #+(or clisp akcl allegro cmu sbcl ecls) a
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists152-legacy-826
  (setq x
        (list 'a 'b 'c)
        y
        (list 'd 'e 'f))
  (d e f))

(check-for-bug :lists152-legacy-833
  (nreconc x y)
  (c b a d e f))

(check-for-bug :lists152-legacy-837
  x
  #+xcl was-destroyed			; wo kommt denn so was her?
  #+clisp (c b a d e f)
  #+(or akcl allegro cmu sbcl ecls) (a d e f)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists152-legacy-844
  (tailp y x)
  t)

(check-for-bug :lists152-legacy-848
  (setq x
        (list 'a 'b 'c)
        y
        (list 'd 'e 'f)
        z
        (list 'g 'h 'i))
  (g h i))

(check-for-bug :lists152-legacy-857
  (nreconc)
  program-error)

(check-for-bug :lists152-legacy-861
  (nreconc x)
  program-error)

(check-for-bug :lists152-legacy-865
  (nreconc nil)
  program-error)

(check-for-bug :lists152-legacy-869
  (nreconc nil nil)
  nil)

(check-for-bug :lists152-legacy-873
  (nreconc x nil)
  (c b a))

(check-for-bug :lists152-legacy-877
  x
  #+xcl was-destroyed
  #+clisp (c b a)
  #+(or akcl allegro cmu sbcl ecls) (a)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists152-legacy-884
  (nreconc nil nil nil nil)
  program-error)

(check-for-bug :lists152-legacy-888
  (nconc nil 'x)
  #+xcl error
  #+(or clisp akcl allegro cmu sbcl ecls) x
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists152-legacy-894
  (setq aa nil)
  nil)

(check-for-bug :lists152-legacy-898
  (push '1 aa)
  (1))

(check-for-bug :lists152-legacy-902
  (push '2 aa)
  (2 1))

(check-for-bug :lists152-legacy-906
  (push '2 aa)
  (2 2 1))

(check-for-bug :lists152-legacy-910
  (setq aa
        (list 'b 'a))
  (b a))

(check-for-bug :lists152-legacy-915
  (pushnew 'a aa)
  (b a))

(check-for-bug :lists152-legacy-919
  (pushnew 'c aa)
  (c b a))

(check-for-bug :lists152-legacy-923
  (setq xxx nil)
  nil)

(check-for-bug :lists152-legacy-927
  (pushnew 'c xxx :test 'equal)
  (c))

(check-for-bug :lists152-legacy-931
  (pushnew 'c xxx :test 'equal)
  (c))

(check-for-bug :lists152-legacy-935
  (pushnew (list 'c) xxx :test 'equal)
  ((c) c))

(check-for-bug :lists152-legacy-939
  xxx
  ((c) c))

(check-for-bug :lists152-legacy-943
  (setq xx (list nil
                 'kkk))
  (nil kkk))

(check-for-bug :lists152-legacy-948
  (pushnew 'u (car xx))
  (u))

(check-for-bug :lists152-legacy-952
  (pushnew 'u
           (car xx))
  (u))

(check-for-bug :lists152-legacy-957
  (pushnew 'v
           (car xx))
  (v u))

(check-for-bug :lists152-legacy-962
  xx
  ((v u) kkk))

(check-for-bug :lists152-legacy-966
  (pushnew (list 'w)
           (car xx))
  ((w)
   v u))

(check-for-bug :lists152-legacy-972
  (pushnew (list 'w)
           (car xx))
  ((w)
   (w)
   v u))

(check-for-bug :lists152-legacy-979
  (pushnew (list 'w)
           (car xx)
           :test 'equal)
  ((w)
   (w)
   v u))

(check-for-bug :lists152-legacy-987
  (pushnew (list 'w)
           (car xx)
           :test-not 'equal)
  ((w)
   (w)
   v u))

(check-for-bug :lists152-legacy-995
  (setq aa (list 1 2 3))
  (1 2 3))

(check-for-bug :lists152-legacy-999
  (pop aa)
  1)

(check-for-bug :lists152-legacy-1003
  aa
  (2 3))

(check-for-bug :lists152-legacy-1007
  (pop aa)
  2)

(check-for-bug :lists152-legacy-1011
  (pop aa)
  3)

(check-for-bug :lists152-legacy-1015
  (pop aa)
  nil)

(check-for-bug :lists152-legacy-1019
  (pop aa)
  nil)

(check-for-bug :lists152-legacy-1023
  (butlast (list 'a 'b 'c))
  (a b))

(check-for-bug :lists152-legacy-1027
  (butlast (list 'a 'b 'c)
           2)
  (a))

(check-for-bug :lists152-legacy-1032
  (nbutlast (list 'a 'b 'c 'd)
            3)
  (a))

(check-for-bug :lists152-legacy-1037
  (nbutlast (list 'a 'b 'c 'd)
            1)
  (a b c))

(check-for-bug :lists152-legacy-1042
  (nbutlast (list 'a 'b 'c 'd)
            0)
  (a b c d))

(check-for-bug :lists152-legacy-1047
  (nbutlast (list 'a 'b 'c 'd)
            4)
  nil)

(check-for-bug :lists152-legacy-1052
  (nbutlast (list 'a 'b 'c 'd)
            6)
  nil)

