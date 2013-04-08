;;; based on v1.1.1.1 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :lists151-legacy-4
  (makunbound 'a)
  a)

(check-for-bug :lists151-legacy-8
  (makunbound 'x)
  x)

(check-for-bug :lists151-legacy-12
  (car '(a b c d e f g))
  a)

(check-for-bug :lists151-legacy-16
  (cdr '(a b c d e f g))
  (b c d e f g))

(check-for-bug :lists151-legacy-20
  (caar '((a) b c d e f g))
  a)

(check-for-bug :lists151-legacy-24
  (cadr '(a b c d e f g))
  b)

(check-for-bug :lists151-legacy-28
  (cdar '((a b) c d e f g))
  (b))

(check-for-bug :lists151-legacy-32
  (cddr '(a b c d e f g))
  (c d e f g))

(check-for-bug :lists151-legacy-36
  (caaar '(((a)) b c d e f g))
  a)

(check-for-bug :lists151-legacy-40
  (caadr '(a (b) c d e f g))
  b)

(check-for-bug :lists151-legacy-44
  (cadar '((a b) c d e f g))
  b)

(check-for-bug :lists151-legacy-48
  (caddr '(a b c d e f g))
  c)

(check-for-bug :lists151-legacy-52
  (cdaar '(((a b)) c d e f g))
  (b))

(check-for-bug :lists151-legacy-56
  (cdadr '(a (b c) d e f g))
  (c))

(check-for-bug :lists151-legacy-60
  (cddar '((a b c) d e f g))
  (c))

(check-for-bug :lists151-legacy-64
  (cdddr '(a b c d e f g))
  (d e f g))

(check-for-bug :lists151-legacy-68
  (caaaar '((((a))) b c d e f g))
  a)

(check-for-bug :lists151-legacy-72
  (caaadr '(a ((b)) c d e f g))
  b)

(check-for-bug :lists151-legacy-76
  (caadar '((a (b)) c d e f g))
  b)

(check-for-bug :lists151-legacy-80
  (caaddr '(a b (c) d e f g))
  c)

(check-for-bug :lists151-legacy-84
  (cadaar '(((a b)) c d e f g))
  b)

(check-for-bug :lists151-legacy-88
  (cadadr '(a (b c) d e f g))
  c)

(check-for-bug :lists151-legacy-92
  (caddar '((a b c) d e f g))
  c)

(check-for-bug :lists151-legacy-96
  (cadddr '(a b c d e f g))
  d)

(check-for-bug :lists151-legacy-100
  (cdaaar '((((a b))) c d e f g))
  (b))

(check-for-bug :lists151-legacy-104
  (cdaadr '(a ((b c)) d e f g))
  (c))

(check-for-bug :lists151-legacy-108
  (cdadar '((a (b c)) d e f g))
  (c))

(check-for-bug :lists151-legacy-112
  (cdaddr '(a b (c d) e f g))
  (d))

(check-for-bug :lists151-legacy-116
  (cddaar '(((a b c)) d e f g))
  (c))

(check-for-bug :lists151-legacy-120
  (cddadr '(a (b c d) e f g))
  (d))

(check-for-bug :lists151-legacy-124
  (cdddar '((a b c d) e f g))
  (d))

(check-for-bug :lists151-legacy-128
  (cddddr '(a b c d e f g))
  (e f g))

(check-for-bug :lists151-legacy-132
  (car '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
         e f g))
  ((((1 2 3) 4) 5) (6 7)))

(check-for-bug :lists151-legacy-137
  (cdr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
         e f g))
  ((((u v w) x) y) ((q w e) r) (a b c) e f g))

(check-for-bug :lists151-legacy-142
  (caar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
          e f g))
  (((1 2 3) 4) 5))

(check-for-bug :lists151-legacy-147
  (cadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
          e f g))
  (((u v w) x) y))

(check-for-bug :lists151-legacy-152
  (cdar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
          e f g))
  ((6 7)))

(check-for-bug :lists151-legacy-157
  (cddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
          e f g))
  (((q w e) r) (a b c) e f g))

(check-for-bug :lists151-legacy-162
  (caaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  ((1 2 3) 4))

(check-for-bug :lists151-legacy-167
  (caadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  ((u v w) x))

(check-for-bug :lists151-legacy-172
  (cadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  (6 7))

(check-for-bug :lists151-legacy-177
  (caddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  ((q w e) r))

(check-for-bug :lists151-legacy-182
  (cdaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  (5))

(check-for-bug :lists151-legacy-187
  (cdadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  (y))

(check-for-bug :lists151-legacy-192
  (cddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  nil)

(check-for-bug :lists151-legacy-197
  (cdddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
           e f g))
  ((a b c) e f g))

(check-for-bug :lists151-legacy-202
  (caaaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (1 2 3))

(check-for-bug :lists151-legacy-207
  (caaadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (u v w))

(check-for-bug :lists151-legacy-212
  (caadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  6)

(check-for-bug :lists151-legacy-217
  (caaddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (q w e))

(check-for-bug :lists151-legacy-222
  (cadaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  5)

(check-for-bug :lists151-legacy-227
  (cadadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  y)

(check-for-bug :lists151-legacy-232
  (caddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  nil)

(check-for-bug :lists151-legacy-237
  (cadddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (a b c))

(check-for-bug :lists151-legacy-242
  (cdaaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (4))

(check-for-bug :lists151-legacy-247
  (cdaadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (x))

(check-for-bug :lists151-legacy-252
  (cdadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (7))

(check-for-bug :lists151-legacy-257
  (cdaddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (r))

(check-for-bug :lists151-legacy-262
  (cddaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  nil)

(check-for-bug :lists151-legacy-267
  (cddadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  nil)

(check-for-bug :lists151-legacy-272
  (cdddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  nil)

(check-for-bug :lists151-legacy-277
  (cddddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
                                                                 c) e f g))
  (e f g))

(check-for-bug :lists151-legacy-282
  (car 'nil)
  nil)

(check-for-bug :lists151-legacy-286
  (cdr 'nil)
  nil)

(check-for-bug :lists151-legacy-290
  (caar 'nil)
  nil)

(check-for-bug :lists151-legacy-294
  (cadr 'nil)
  nil)

(check-for-bug :lists151-legacy-298
  (cdar 'nil)
  nil)

(check-for-bug :lists151-legacy-302
  (cddr 'nil)
  nil)

(check-for-bug :lists151-legacy-306
  (caaar 'nil)
  nil)

(check-for-bug :lists151-legacy-310
  (caadr 'nil)
  nil)

(check-for-bug :lists151-legacy-314
  (cadar 'nil)
  nil)

(check-for-bug :lists151-legacy-318
  (caddr 'nil)
  nil)

(check-for-bug :lists151-legacy-322
  (cdaar 'nil)
  nil)

(check-for-bug :lists151-legacy-326
  (cdadr 'nil)
  nil)

(check-for-bug :lists151-legacy-330
  (cddar 'nil)
  nil)

(check-for-bug :lists151-legacy-334
  (cdddr 'nil)
  nil)

(check-for-bug :lists151-legacy-338
  (caaaar 'nil)
  nil)

(check-for-bug :lists151-legacy-342
  (caaadr 'nil)
  nil)

(check-for-bug :lists151-legacy-346
  (caadar 'nil)
  nil)

(check-for-bug :lists151-legacy-350
  (caaddr 'nil)
  nil)

(check-for-bug :lists151-legacy-354
  (cadaar 'nil)
  nil)

(check-for-bug :lists151-legacy-358
  (cadadr 'nil)
  nil)

(check-for-bug :lists151-legacy-362
  (caddar 'nil)
  nil)

(check-for-bug :lists151-legacy-366
  (cadddr 'nil)
  nil)

(check-for-bug :lists151-legacy-370
  (cdaaar 'nil)
  nil)

(check-for-bug :lists151-legacy-374
  (cdaadr 'nil)
  nil)

(check-for-bug :lists151-legacy-378
  (cdadar 'nil)
  nil)

(check-for-bug :lists151-legacy-382
  (cdaddr 'nil)
  nil)

(check-for-bug :lists151-legacy-386
  (cddaar 'nil)
  nil)

(check-for-bug :lists151-legacy-390
  (cddadr 'nil)
  nil)

(check-for-bug :lists151-legacy-394
  (cdddar 'nil)
  nil)

(check-for-bug :lists151-legacy-398
  (cddddr 'nil)
  nil)

(check-for-bug :lists151-legacy-402
  (car '(a b c d e f g))
  a)

(check-for-bug :lists151-legacy-406
  (cdr '(a b c d e f g))
  (b c d e f g))

(check-for-bug :lists151-legacy-410
  (caar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-414
  (cadr '(a b c d e f g))
  b)

(check-for-bug :lists151-legacy-418
  (cdar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-422
  (cddr '(a b c d e f g))
  (c d e f g))

(check-for-bug :lists151-legacy-426
  (caaar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-430
  (caadr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-434
  (cadar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-438
  (caddr '(a b c d e f g))
  c)

(check-for-bug :lists151-legacy-442
  (cdaar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-446
  (cdadr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-450
  (cddar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-454
  (cdddr '(a b c d e f g))
  (d e f g))

(check-for-bug :lists151-legacy-458
  (caaaar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-462
  (caaadr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-466
  (caadar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-470
  (caaddr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-474
  (cadaar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-478
  (cadadr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-482
  (caddar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-486
  (cadddr '(a b c d e f g))
  d)

(check-for-bug :lists151-legacy-490
  (cdaaar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-494
  (cdaadr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-498
  (cdadar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-502
  (cdaddr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-506
  (cddaar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-510
  (cddadr '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-514
  (cdddar '(a b c d e f g))
  type-error)

(check-for-bug :lists151-legacy-518
  (cddddr '(a b c d e f g))
  (e f g))

(check-for-bug :lists151-legacy-522
  (car '(a))
  a)

(check-for-bug :lists151-legacy-526
  (cdr '(a))
  nil)

(check-for-bug :lists151-legacy-530
  (caar '(a))
  type-error)

(check-for-bug :lists151-legacy-534
  (cadr '(a))
  nil)

(check-for-bug :lists151-legacy-538
  (cdar '(a))
  type-error)

(check-for-bug :lists151-legacy-542
  (cddr '(a))
  nil)

(check-for-bug :lists151-legacy-546
  (caaar '(a))
  type-error)

(check-for-bug :lists151-legacy-550
  (caadr '(a))
  nil)

(check-for-bug :lists151-legacy-554
  (cadar '(a))
  type-error)

(check-for-bug :lists151-legacy-558
  (caddr '(a))
  nil)

(check-for-bug :lists151-legacy-562
  (cdaar '(a))
  type-error)

(check-for-bug :lists151-legacy-566
  (cdadr '(a))
  nil)

(check-for-bug :lists151-legacy-570
  (cddar '(a))
  type-error)

(check-for-bug :lists151-legacy-574
  (cdddr '(a))
  nil)

(check-for-bug :lists151-legacy-578
  (caaaar '(a))
  type-error)

(check-for-bug :lists151-legacy-582
  (caaadr '(a))
  nil)

(check-for-bug :lists151-legacy-586
  (caadar '(a))
  type-error)

(check-for-bug :lists151-legacy-590
  (caaddr '(a))
  nil)

(check-for-bug :lists151-legacy-594
  (cadaar '(a))
  type-error)

(check-for-bug :lists151-legacy-598
  (cadadr '(a))
  nil)

(check-for-bug :lists151-legacy-602
  (caddar '(a))
  type-error)

(check-for-bug :lists151-legacy-606
  (cadddr '(a))
  nil)

(check-for-bug :lists151-legacy-610
  (cdaaar '(a))
  type-error)

(check-for-bug :lists151-legacy-614
  (cdaadr '(a))
  nil)

(check-for-bug :lists151-legacy-618
  (cdadar '(a))
  type-error)

(check-for-bug :lists151-legacy-622
  (cdaddr '(a))
  nil)

(check-for-bug :lists151-legacy-626
  (cddaar '(a))
  type-error)

(check-for-bug :lists151-legacy-630
  (cddadr '(a))
  nil)

(check-for-bug :lists151-legacy-634
  (cdddar '(a))
  type-error)

(check-for-bug :lists151-legacy-638
  (cddddr '(a))
  nil)

(check-for-bug :lists151-legacy-642
  (cons 1 2)
  (1 . 2))

(check-for-bug :lists151-legacy-646
  (cons 'a 'b)
  (a . b))

(check-for-bug :lists151-legacy-650
  (cons 'a 'b 'c)
  program-error)

(check-for-bug :lists151-legacy-654
  (cons 'a)
  program-error)

(check-for-bug :lists151-legacy-658
  (cons)
  program-error)

(check-for-bug :lists151-legacy-662
  (cons 'a 'nil)
  (a))

(check-for-bug :lists151-legacy-666
  (cons 'nil 'a)
  (nil . a))

(check-for-bug :lists151-legacy-670
  (cons 'a (cons 'b (cons 'c 'nil)))
  (a b c))

(check-for-bug :lists151-legacy-674
  (cons 'a '(b c d))
  (a b c d))

(check-for-bug :lists151-legacy-678
  (tree-equal 1 1)
  t)

(check-for-bug :lists151-legacy-682
  (tree-equal 'word 'word)
  t)

(check-for-bug :lists151-legacy-686
  (tree-equal 'word1 'word2)
  nil)

(check-for-bug :lists151-legacy-690
  (tree-equal '(a b) '(a b))
  t)

(check-for-bug :lists151-legacy-694
  (tree-equal '(a (b c)) '((a b) c))
  nil)

(check-for-bug :lists151-legacy-698
  (tree-equal 5 (+ 2 3))
  t)

(check-for-bug :lists151-legacy-702
  (tree-equal '(a (b quote nil)) '(a (b)))
  nil)

(check-for-bug :lists151-legacy-706
  (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))))
  nil)

(check-for-bug :lists151-legacy-710
  (tree-equal 1 1 :test #'eq)
  t)

(check-for-bug :lists151-legacy-714
  (tree-equal 'word 'word :test #'eq)
  t)

(check-for-bug :lists151-legacy-718
  (tree-equal 'word1 'word2 :test #'eq)
  nil)

(check-for-bug :lists151-legacy-722
  (tree-equal '(a b) '(a b) :test #'eq)
  t)

(check-for-bug :lists151-legacy-726
  (tree-equal '(a (b c)) '((a b) c) :test #'eq)
  nil)

(check-for-bug :lists151-legacy-730
  (tree-equal 5 (+ 2 3) :test #'eq)
  t)

(check-for-bug :lists151-legacy-734
  (tree-equal '(a (b)) '(a (b)) :test #'eq)
  t)

(check-for-bug :lists151-legacy-738
  (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test #'eq)
  nil)

(check-for-bug :lists151-legacy-742
  (tree-equal 1 1 :test #'eql)
  t)

(check-for-bug :lists151-legacy-746
  (tree-equal 'word 'word :test #'eql)
  t)

(check-for-bug :lists151-legacy-750
  (tree-equal 'word1 'word2 :test #'eql)
  nil)

(check-for-bug :lists151-legacy-754
  (tree-equal '(a b) '(a b) :test #'eql)
  t)

(check-for-bug :lists151-legacy-758
  (tree-equal '(a (b c)) '((a b) c) :test #'eql)
  nil)

(check-for-bug :lists151-legacy-762
  (tree-equal 5 (+ 2 3) :test #'eql)
  t)

(check-for-bug :lists151-legacy-766
  (tree-equal '(a (b)) '(a (b)) :test #'eql)
  t)

(check-for-bug :lists151-legacy-770
  (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test #'eql)
  nil)

(check-for-bug :lists151-legacy-774
  (tree-equal 1 1 :test #'equal)
  t)

(check-for-bug :lists151-legacy-778
  (tree-equal 'word 'word :test #'equal)
  t)

(check-for-bug :lists151-legacy-782
  (tree-equal 'word1 'word2 :test #'equal)
  nil)

(check-for-bug :lists151-legacy-786
  (tree-equal '(a b) '(a b) :test #'equal)
  t)

(check-for-bug :lists151-legacy-790
  (tree-equal '(a (b c)) '((a b) c) :test #'equal)
  nil)

(check-for-bug :lists151-legacy-794
  (tree-equal 5 (+ 2 3) :test #'equal)
  t)

(check-for-bug :lists151-legacy-798
  (tree-equal '(a (b)) '(a (b)) :test #'equal)
  t)

(check-for-bug :lists151-legacy-802
  (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test #'equal)
  nil)

(check-for-bug :lists151-legacy-806
  (tree-equal 1 1 :test-not #'eq)
  nil)

(check-for-bug :lists151-legacy-810
  (tree-equal 'word 'word :test-not #'eq)
  nil)

(check-for-bug :lists151-legacy-814
  (tree-equal 'word1 'word2 :test-not #'eq)
  t)

(check-for-bug :lists151-legacy-818
  (tree-equal '(a b) '(a b) :test-not #'eq)
  nil)

(check-for-bug :lists151-legacy-822
  (tree-equal '(a (b c)) '((a b) c) :test-not #'eq)
  nil)

(check-for-bug :lists151-legacy-826
  (tree-equal 5 (+ 2 3) :test-not #'eq)
  nil)

(check-for-bug :lists151-legacy-830
  (tree-equal '(a (b)) '(a (b)) :test-not #'eq)
  nil)

(check-for-bug :lists151-legacy-834
  (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test-not #'eq)
  nil)

