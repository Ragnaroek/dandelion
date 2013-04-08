;;; based on v1.5 -*- mode: lisp -*-
(in-package :cl-user)

;;erzeuge ein feld mit doppeltgenauen zahlen

(check-for-bug :array-legacy-6
  (setq da1
        (make-array
         (list 4 2 3)
         :initial-contents
         (list
          (list (list 1.0d0 2.0d0 3.0d0)
                (list 4.0d0 5.0d0 6.0d0))
          (list (list 7.0d0 8.0d0 9.0d0)
                (list 10.0d0 11.0d0 12.0d0))
          (list (list 13.0d0 14.0d0 15.0d0)
                (list 16.0d0 17.0d0 18.0d0))
          (list (list 19.0d0 20.0d0 21.0d0)
                (list 22.0d0 23.0d0 24.0d0)))
         :element-type
         (quote double-float)))
  #3a(((1.0d0 2.0d0 3.0d0)   (4.0d0 5.0d0 6.0d0))
      ((7.0d0 8.0d0 9.0d0)   (10.0d0 11.0d0 12.0d0))
      ((13.0d0 14.0d0 15.0d0)(16.0d0 17.0d0 18.0d0))
      ((19.0d0 20.0d0 21.0d0)(22.0d0 23.0d0 24.0d0))))

(check-for-bug :array-legacy-27
  (aref da1 0 0 0)
  1.0d0)

(check-for-bug :array-legacy-31
  (aref da1 0 0 1)
  2.0d0)

(check-for-bug :array-legacy-35
  (aref da1 0 0 2)
  3.0d0)

(check-for-bug :array-legacy-39
  (aref da1 0 1 0)
  4.0d0)

(check-for-bug :array-legacy-43
  (aref da1 0 1 1)
  5.0d0)

(check-for-bug :array-legacy-47
  (aref da1 0 1 2)
  6.0d0)

(check-for-bug :array-legacy-51
  (aref da1 1 0 0)
  7.0d0)

(check-for-bug :array-legacy-55
  (aref da1 1 0 1)
  8.0d0)

(check-for-bug :array-legacy-59
  (aref da1 1 0 2)
  9.0d0)

(check-for-bug :array-legacy-63
  (aref da1 1 1 0)
  10.0d0)

(check-for-bug :array-legacy-67
  (aref da1 1 1 1)
  11.0d0)

(check-for-bug :array-legacy-71
  (aref da1 1 1 2)
  12.0d0)

(check-for-bug :array-legacy-75
  (aref da1 2 0 0)
  13.0d0)

(check-for-bug :array-legacy-79
  (aref da1 2 0 1)
  14.0d0)

(check-for-bug :array-legacy-83
  (aref da1 2 0 2)
  15.0d0)

(check-for-bug :array-legacy-87
  (aref da1 2 1 0)
  16.0d0)

(check-for-bug :array-legacy-91
  (aref da1 2 1 1)
  17.0d0)

(check-for-bug :array-legacy-95
  (aref da1 2 1 2)
  18.0d0)

(check-for-bug :array-legacy-99
  (aref da1 3 0 0)
  19.0d0)

(check-for-bug :array-legacy-103
  (aref da1 3 0 1)
  20.0d0)

(check-for-bug :array-legacy-107
  (aref da1 3 0 2)
  21.0d0)

(check-for-bug :array-legacy-111
  (aref da1 3 1 0)
  22.0d0)

(check-for-bug :array-legacy-115
  (aref da1 3 1 1)
  23.0d0)

(check-for-bug :array-legacy-119
  (aref da1 3 1 1)
  23.0d0)

;;erzeuge ein feld mit einfachgenauen zahlen

(check-for-bug :array-legacy-125
  (setq fa1
        (make-array
         (list 4 2 3)
         :initial-contents
         (list
          (list (list 1.0d0 2.0d0 3.0d0)
                (list 4.0d0 5.0d0 6.0d0))
          (list (list 7.0d0 8.0d0 9.0d0)
                (list 10.0d0 11.0d0 12.0d0))
          (list (list 13.0d0 14.0d0 15.0d0)
                (list 16.0d0 17.0d0 18.0d0))
          (list (list 19.0d0 20.0d0 21.0d0)
                (list 22.0d0 23.0d0 24.0d0)))
         :element-type 'double-float))
  #3a(((1.0 2.0 3.0)(4.0 5.0 6.0))
      ((7.0 8.0 9.0)(10.0 11.0 12.0))
      ((13.0 14.0 15.0)(16.0 17.0 18.0))
      ((19.0 20.0 21.0)(22.0 23.0 24.0))))

(check-for-bug :array-legacy-145
  (aref fa1 0 0 0)
  1.0)

(check-for-bug :array-legacy-149
  (aref fa1 0 0 1)
  2.0)

(check-for-bug :array-legacy-153
  (aref fa1 0 0 2)
  3.0)

(check-for-bug :array-legacy-157
  (aref fa1 0 1 0)
  4.0)

(check-for-bug :array-legacy-161
  (aref fa1 0 1 1)
  5.0)

(check-for-bug :array-legacy-165
  (aref fa1 0 1 2)
  6.0)

(check-for-bug :array-legacy-169
  (aref fa1 1 0 0)
  7.0)

(check-for-bug :array-legacy-173
  (aref fa1 1 0 1)
  8.0)

(check-for-bug :array-legacy-177
  (aref fa1 1 0 2)
  9.0)

(check-for-bug :array-legacy-181
  (aref fa1 1 1 0)
  10.0)

(check-for-bug :array-legacy-185
  (aref fa1 1 1 1)
  11.0)

(check-for-bug :array-legacy-189
  (aref fa1 1 1 2)
  12.0)

(check-for-bug :array-legacy-193
  (aref fa1 2 0 0)   13.0)

(check-for-bug :array-legacy-196
  (aref fa1 2 0 1)
  14.0)

(check-for-bug :array-legacy-200
  (aref fa1 2 0 2)
  15.0)

(check-for-bug :array-legacy-204
  (aref fa1 2 1 0)   16.0)

(check-for-bug :array-legacy-207
  (aref fa1 2 1 1)
  17.0)

(check-for-bug :array-legacy-211
  (aref fa1 2 1 2)
  18.0)

(check-for-bug :array-legacy-215
  (aref fa1 3 0 0)
  19.0)

(check-for-bug :array-legacy-219
  (aref fa1 3 0 1)
  20.0)

(check-for-bug :array-legacy-223
  (aref fa1 3 0 2)
  21.0)

(check-for-bug :array-legacy-227
  (aref fa1 3 1 0)
  22.0)

(check-for-bug :array-legacy-231
  (aref fa1 3 1 1)
  23.0)

(check-for-bug :array-legacy-235
  (aref fa1 3 1 1)
  23.0)


;; limits fuer felder

(check-for-bug :array-legacy-242
  (let ((s (prin1-to-string array-rank-limit )))
    (or #+xcl (equal s "256")
        #+clisp (equal s "4294967296")
        #+clisp (equal s "65536")
        #+akcl (equal s "64")
        #+gcl (equal s "63")
        #+allegro (equal s "65536")
        #+(or cmu sbcl) (equal s "65529")
        #+ecls (equal s "64")
        #-(or xcl clisp akcl allegro cmu sbcl ecls) "unknown"
        ) )
  t)

(check-for-bug :array-legacy-256
  (let ((s (prin1-to-string array-dimension-limit )))
    (or #+xcl (equal s "17920")
        #+akcl (equal s "16777216")
        #+gcl (equal s "2147483647")
        #+clisp (equal s (prin1-to-string (1+ most-positive-fixnum)))
        #+allegro (equal s "16777216")
        #+(or cmu sbcl) (equal s "536870911")
        #+ecls (equal s "16777216")
        #-(or xcl clisp akcl allegro cmu sbcl ecls) "unknown"
        ) )
  t)

(check-for-bug :array-legacy-269
  (let ((s (prin1-to-string array-total-size-limit )))
    (or #+xcl (equal s "17920")
        #+akcl (equal s "16777216")
        #+clisp (equal s (prin1-to-string (1+ most-positive-fixnum)))
        #+allegro (equal s "16777216")
        #+(or cmu sbcl) (equal s "536870911")
        #+ecls (equal s "16777216")
        #-(or xcl clisp akcl allegro cmu sbcl ecls) "unknown"
        ) )
  t)

;;erzeuge einen einfachen (simple) vector

(check-for-bug :array-legacy-283
  (equalp (setq sv (vector (quote a) (quote b) (quote c) 1.0s0 3.7d0
                           4.1))
          #(a b c 1.0s0 3.7d0 4.1))   t)

(check-for-bug :array-legacy-288
  (svref sv 0)   a)

(check-for-bug :array-legacy-291
  (svref sv 1)   b)

(check-for-bug :array-legacy-294
  (svref sv 2)   c)

(check-for-bug :array-legacy-297
  (svref sv 3)   1.0s0)

(check-for-bug :array-legacy-300
  (svref sv 4)   3.7d0)

;;pruefe setzen eines elements

(check-for-bug :array-legacy-305
  (setf (svref sv 0) (quote test))   test)

(check-for-bug :array-legacy-308
  (equalp sv #(test b c 1.0s0 3.7d0 4.1))   t)

;;test array-element-typ ... da2 nicht def.

(check-for-bug :array-legacy-313
  (array-element-type sv)   t)

(unintern 'sv)

(check-for-bug :array-legacy-318
  (array-element-type da1)
  #+(or xcl allegro cmu sbcl) double-float
  #+clisp t
  #+(or akcl ecls) long-float
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

;;test rang

(check-for-bug :array-legacy-327
  (array-rank da1)   3)

(check-for-bug :array-legacy-330
  (array-rank fa1)   3)

(unintern 'fa1)

;;test der einzelnen dimensionen

(check-for-bug :array-legacy-337
  (array-dimension da1 0)   4)

(check-for-bug :array-legacy-340
  (array-dimension da1 1)   2)

(check-for-bug :array-legacy-343
  (array-dimension da1 2)   3)

(check-for-bug :array-legacy-346
  (array-dimension da1 3)   error)

(unintern 'da1)
;;erzeuge ein 0-dim. feld (pseudoscalar) mit inhalt mod 5

(check-for-bug :array-legacy-352
  (progn
    (setq zero
          (make-array (quote nil)
                      :element-type '(mod 5)))
    t)
  t)

(check-for-bug :array-legacy-360
  (array-rank zero)   0)

(check-for-bug :array-legacy-363
  (setf (aref zero) 4)   4)

(check-for-bug :array-legacy-366
  (setf (aref zero) 1.0)
  #+(or xcl clisp akcl allegro cmu sbcl ecls) type-error
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(unintern 'zero)

;;erzeuge ein 3-dim gen. feld

(check-for-bug :array-legacy-375
  (setq a1
        (make-array (list 4 2 3)
                    :initial-contents
                    (list
                     (list (list 'a 'b 'c)
                           (list 1 2 3))
                     (list (list 'd 'e 'f)
                           (list 3 1 2))
                     (list (list 'g 'h 'i)
                           (list 2 3 1))
                     (list (list 'j 'k 'l)
                           (list 0 0 0)))))
  #3a(((a b c)(1 2 3))
      ((d e f)(3 1 2))
      ((g h i)(2 3 1))
      ((j k l)(0 0 0))))

(check-for-bug :array-legacy-393
  (aref a1 0 0 0)   a)

(check-for-bug :array-legacy-396
  (aref a1 0 0 1)   b)

(check-for-bug :array-legacy-399
  (aref a1 0 0 2)   c)

(check-for-bug :array-legacy-402
  (aref a1 0 1 0)   1)

(check-for-bug :array-legacy-405
  (aref a1 0 1 1)   2)

(check-for-bug :array-legacy-408
  (aref a1 0 1 2)   3)

(check-for-bug :array-legacy-411
  (aref a1 1 0 0)   d)

(check-for-bug :array-legacy-414
  (aref a1 1 0 1)   e)

(check-for-bug :array-legacy-417
  (aref a1 1 0 2)   f)

(check-for-bug :array-legacy-420
  (aref a1 1 1 0)   3)

(check-for-bug :array-legacy-423
  (aref a1 1 1 1)   1)

(check-for-bug :array-legacy-426
  (aref a1 1 1 2)   2)

(check-for-bug :array-legacy-429
  (aref a1 2 0 0)   g)

(check-for-bug :array-legacy-432
  (aref a1 2 0 1)   h)

(check-for-bug :array-legacy-435
  (aref a1 2 0 2)   i)

(check-for-bug :array-legacy-438
  (aref a1 2 1 0)   2)

(check-for-bug :array-legacy-441
  (aref a1 2 1 1)   3)

(check-for-bug :array-legacy-444
  (aref a1 2 1 2)   1)

(check-for-bug :array-legacy-447
  (aref a1 3 0 0)   j)

(check-for-bug :array-legacy-450
  (aref a1 3 0 1)   k)

(check-for-bug :array-legacy-453
  (aref a1 3 0 2)   l)

(check-for-bug :array-legacy-456
  (aref a1 3 1 0)   0)

(check-for-bug :array-legacy-459
  (aref a1 3 1 1)   0)

(check-for-bug :array-legacy-462
  (aref a1 3 1 1)   0)

(unintern 'a1)

;;erzeuge ein 2-dim adj.feld, das ueberlagert wird

(check-for-bug :array-legacy-469
  (progn (setq m (make-array (list 4 4)
                             :adjustable t
                             :initial-contents
                             (list
                              (list 'alpha 'beta 'gamma 'delta)
                              (list 'epsilon 'zeta 'eta 'theta)
                              (list 'iota 'kappa 'lambda 'mu)
                              (list 'nu 'xi 'omicron 'pi))))
         t)
  t)

(check-for-bug :array-legacy-481
  (aref m 0 0)   alpha)

(check-for-bug :array-legacy-484
  (aref m 0 1)   beta)

(check-for-bug :array-legacy-487
  (aref m 0 2)   gamma)

(check-for-bug :array-legacy-490
  (aref m 0 3)   delta)

(check-for-bug :array-legacy-493
  (aref m 1 0)   epsilon)

(check-for-bug :array-legacy-496
  (aref m 1 1)   zeta)

(check-for-bug :array-legacy-499
  (aref m 1 2)   eta)

(check-for-bug :array-legacy-502
  (aref m 1 3)   theta)

(check-for-bug :array-legacy-505
  (aref m 2 0)   iota)

(check-for-bug :array-legacy-508
  (aref m 2 1)   kappa)

(check-for-bug :array-legacy-511
  (aref m 2 2)   lambda)

(check-for-bug :array-legacy-514
  (aref m 2 3)   mu)

(check-for-bug :array-legacy-517
  (aref m 3 0)   nu)

(check-for-bug :array-legacy-520
  (aref m 3 1)   xi)

(check-for-bug :array-legacy-523
  (aref m 3 2)   omicron)

(check-for-bug :array-legacy-526
  (aref m 3 3)   pi)

;;erzeuge ueberl. der zeilen

(check-for-bug :array-legacy-531
  (equalp (setq md0 (make-array 4 :displaced-to m))   #(alpha beta gamma
                                                        delta)) t)

(check-for-bug :array-legacy-535
  (equalp (setq md1 (make-array 4 :displaced-to m :displaced-index-offset4))
          #(epsilon zeta eta theta)) t)


(check-for-bug :array-legacy-540
  (equalp (setq md2 (make-array 4 :displaced-to m :displaced-index-offset8))
          #(iota kappa lambda mu)) t)


(unintern 'md0)
(unintern 'md1)
(unintern 'md2)


;;adjustiere feld m

(check-for-bug :array-legacy-552
  (progn (adjust-array m (quote (3 5)) :initial-element (quote baz))
         t)   t)

(check-for-bug :array-legacy-556
  (aref m 0 0)   alpha)

(check-for-bug :array-legacy-559
  (aref m 0 1)   beta)

(check-for-bug :array-legacy-562
  (aref m 0 2)   gamma)

(check-for-bug :array-legacy-565
  (aref m 0 3)   delta)

(check-for-bug :array-legacy-568
  (aref m 0 4)   baz)

(check-for-bug :array-legacy-571
  (aref m 1 0)   epsilon)

(check-for-bug :array-legacy-574
  (aref m 1 1)   zeta)

(check-for-bug :array-legacy-577
  (aref m 1 2)   eta)

(check-for-bug :array-legacy-580
  (aref m 1 3)   theta)

(check-for-bug :array-legacy-583
  (aref m 1 4)   baz)

(check-for-bug :array-legacy-586
  (aref m 2 0)   iota)

(check-for-bug :array-legacy-589
  (aref m 2 1)   kappa)

(check-for-bug :array-legacy-592
  (aref m 2 2)   lambda)

(unintern 'm)

;;teste zusammenspiel der schluesselworte

(check-for-bug :array-legacy-599
  (progn
    (setq dv (make-array 10 :element-type (quote double-float)
                         :initial-contents(quote (0.0d0 1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0 7.0d0 8.0d0 9.0d0))))
    t)
  t)

#| *************************************************************************** ;


(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :initial-contents (quote ((1.0d0 2.0d0) (3.0d0 4.0d0 5.0d0)))))   error

(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :initial-contents (quote
					 ((1.0d0 2.0d0) (3.0d0 4.0d0) :displaced-to dv :displaced-index-offset
					  8))))   error

(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :initial-contents (quote ((1.0d0 2.0d0) (3.0d0 4.0d0))) :displaced-to
		      dv
		      :displaced-index-offset 8))   error

(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :displaced-to dv :displaced-index-offset 8))   error

***************************************************************************|#

(check-for-bug :array-legacy-631
 (aref dv 0)   0.0d0)

(check-for-bug :array-legacy-634
 (aref dv 1)   1.0d0)

(check-for-bug :array-legacy-637
 (aref dv 2)   2.0d0)

(check-for-bug :array-legacy-640
 (aref dv 3)   3.0d0)

(check-for-bug :array-legacy-643
 (aref dv 4)   4.0d0)

(check-for-bug :array-legacy-646
 (aref dv 5)   5.0d0)

(check-for-bug :array-legacy-649
 (aref dv 6)   6.0d0)

(check-for-bug :array-legacy-652
 (aref dv 7)   7.0d0)

(check-for-bug :array-legacy-655
 (aref dv 8)   8.0d0)

(check-for-bug :array-legacy-658
 (aref dv 9)   9.0d0)

(check-for-bug :array-legacy-661
 (setf (aref dv 5) -5.0d0)   -5.0d0)

(unintern 'dv)

;;definiere testfkt fuer indices

(check-for-bug :array-legacy-668
 (defun array-index-test (a &rest subs) (unless
					    (apply (function array-in-bounds-p) a subs)
					  (return-from array-index-test (quote error))) (=
					  (apply (function array-row-major-index) a subs) (apply (function +)
												 (maplist
												  (function (lambda (x y) (* (car x) (apply (function *) (cdr y)))))
												  subs
												  (array-dimensions a)))))   array-index-test)

(check-for-bug :array-legacy-678
 (array-index-test (make-array (quote (5 4 3 2 1))) 4 2 2 1 0)   t)

(check-for-bug :array-legacy-681
 (array-index-test (make-array (quote (5 4 3 2 1))) 3 4 2 1 2)   error)

;;test bitfelder

(check-for-bug :array-legacy-686
 (setq bvzero (make-array 100 :element-type (quote bit) :initial-element
			  0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(check-for-bug :array-legacy-691
 (setq bvone (make-array 100 :element-type (quote bit) :initial-element
			 1))
 #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)

(check-for-bug :array-legacy-696
 (setq bv3 (make-array 100 :element-type (quote bit) :initial-element
		       0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(check-for-bug :array-legacy-701
 (setq bv2 (make-array 100 :element-type (quote bit) :initial-element
		       0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(check-for-bug :array-legacy-706
 (setq bv1 (make-array 100 :element-type (quote bit) :initial-element
		       0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

;;setze bitfelder

(check-for-bug :array-legacy-713
 (dotimes (i 50 bv1) (setf (sbit bv1 (* i 2)) 1))
 #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)

(check-for-bug :array-legacy-717
 (dotimes (i 50 bv2) (setf (bit bv2 (* i 2)) 1))
 #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)

(check-for-bug :array-legacy-721
 (equalp bv1 bv2)   t)

(check-for-bug :array-legacy-724
 (dotimes (i 25 bv3)
   (setf (sbit bv3 (* i 4))
	 1))
 #*1000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000)

(check-for-bug :array-legacy-730
 (bit-and bv1 bv3)
 #*1000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000)

(check-for-bug :array-legacy-734
 (bit-ior bv1 bv3)
 #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)

(check-for-bug :array-legacy-738
 (bit-xor bv1 bv3)
 #*0010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010)

(check-for-bug :array-legacy-742
 (bit-eqv bv1 bv3)
 #*1101110111011101110111011101110111011101110111011101110111011101110111011101110111011101110111011101)

(check-for-bug :array-legacy-746
 (bit-nand bv1 bv3)
 #*0111011101110111011101110111011101110111011101110111011101110111011101110111011101110111011101110111)

(check-for-bug :array-legacy-750
 (bit-andc1 bv1 bv3)
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(check-for-bug :array-legacy-754
 (bit-andc2 bv1 bv3)
 #*0010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010)

(check-for-bug :array-legacy-758
 (bit-orc1 bv1 bv3)
 #*1101110111011101110111011101110111011101110111011101110111011101110111011101110111011101110111011101)

(check-for-bug :array-legacy-762
 (bit-orc2 bv1 bv3)
 #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)

(check-for-bug :array-legacy-766
 (bit-not bv1)
 #*0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101)

(check-for-bug :array-legacy-770
 (bit-not bvzero)
 #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)

(check-for-bug :array-legacy-774
 (bit-not bvone)
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(unintern 'bv1)
(unintern 'bv2)
(unintern 'bv3)
(unintern 'bvzero)
(unintern 'bvone)

;;teste operationen mit fillpointern

(check-for-bug :array-legacy-786
 (make-array (quote (3 4 5)) :fill-pointer t)   error)

(check-for-bug :array-legacy-789
 (equalp (make-array 5 :fill-pointer 5)
	 #+(or xcl cmu sbcl)
	 #(0 0 0 0 0)
	 #-(or xcl cmu sbcl)
	 #(nil nil nil nil nil))
 t)

(check-for-bug :array-legacy-797
 (make-array 5 :fill-pointer -5)   error)

;;allgem. vector mit fillpointer

(check-for-bug :array-legacy-802
 (progn (setq vmf (make-array 5 :fill-pointer 0)) t)   t)

(check-for-bug :array-legacy-805
 (fill-pointer vmf)   0)

(check-for-bug :array-legacy-808
 (vector-push (quote a) vmf)   0)

(check-for-bug :array-legacy-811
 (fill-pointer vmf)   1)

(check-for-bug :array-legacy-814
 (vector-push (quote b) vmf)   1)

(check-for-bug :array-legacy-817
 (vector-push (quote c) vmf)   2)

(check-for-bug :array-legacy-820
 (vector-push (quote d) vmf)   3)

(check-for-bug :array-legacy-823
 (vector-push (quote e) vmf)   4)

(check-for-bug :array-legacy-826
 (vector-push (quote voll) vmf)   nil)

(check-for-bug :array-legacy-829
 (vector-pop vmf)   e)

(check-for-bug :array-legacy-832
 (vector-pop vmf)   d)

(check-for-bug :array-legacy-835
 (vector-pop vmf)   c)

(check-for-bug :array-legacy-838
 (vector-pop vmf)   b)

(check-for-bug :array-legacy-841
 (vector-pop vmf)   a)

(check-for-bug :array-legacy-844
 (vector-pop vmf)   error)

;;adjustabler allgem. vector mit fillpointer

(unintern 'vmf)

(check-for-bug :array-legacy-851
 (progn (setq vmfa (make-array 5 :fill-pointer 0 :adjustable t)) t)
 t)

(check-for-bug :array-legacy-855
 (fill-pointer vmfa)   0)

(check-for-bug :array-legacy-858
 (vector-push-extend (quote a) vmfa)   0)

(check-for-bug :array-legacy-861
 (fill-pointer vmfa)   1)

(check-for-bug :array-legacy-864
 (vector-push-extend (quote b) vmfa)   1)

(check-for-bug :array-legacy-867
 (vector-push-extend (quote c) vmfa)   2)

(check-for-bug :array-legacy-870
 (vector-push-extend (quote d) vmfa)   3)

(check-for-bug :array-legacy-873
 (vector-push-extend (quote e) vmfa)   4)

(check-for-bug :array-legacy-876
 (vector-push-extend (quote voll) vmfa)   5)

(check-for-bug :array-legacy-879
 (vector-pop vmfa)   voll)

(check-for-bug :array-legacy-882
 (vector-pop vmfa)   e)

(check-for-bug :array-legacy-885
 (vector-pop vmfa)   d)

(check-for-bug :array-legacy-888
 (vector-pop vmfa)   c)

(check-for-bug :array-legacy-891
 (vector-pop vmfa)   b)

(check-for-bug :array-legacy-894
 (vector-pop vmfa)   a)

;;doppeltgen. vector mit fillpointer

(unintern 'vmfa)

(check-for-bug :array-legacy-901
 (progn
   (setq vmfd (make-array 5 :fill-pointer 0 :element-type (quote double-float)))
   t)   t)

(check-for-bug :array-legacy-906
 (fill-pointer vmfd)   0)

(check-for-bug :array-legacy-909
 (vector-push 0.0d0 vmfd)   0)

(check-for-bug :array-legacy-912
 (fill-pointer vmfd)   1)

(check-for-bug :array-legacy-915
 (vector-push 1.0d0 vmfd)   1)

(check-for-bug :array-legacy-918
 (vector-push 2.0d0 vmfd)   2)

(check-for-bug :array-legacy-921
 (vector-push 3.0d0 vmfd)   3)

(check-for-bug :array-legacy-924
 (vector-push 4.0d0 vmfd)   4)

(check-for-bug :array-legacy-927
 (vector-push 5.0d0 vmfd)   nil)

(check-for-bug :array-legacy-930
 (vector-pop vmfd)   4.0d0)

(check-for-bug :array-legacy-933
 (vector-pop vmfd)   3.0d0)

(check-for-bug :array-legacy-936
 (vector-pop vmfd)   2.0d0)

(check-for-bug :array-legacy-939
 (vector-pop vmfd)   1.0d0)

(check-for-bug :array-legacy-942
 (vector-pop vmfd)   0.0d0)

(check-for-bug :array-legacy-945
 (vector-pop vmfd)   error)

;;doppeltgen. adjust. vector mit fillpointer

(unintern 'vmfd)

(check-for-bug :array-legacy-952
 (progn (setq vmfad
	      (make-array 5 :fill-pointer 0 :element-type (quote double-float) :adjustable
			  t))
	t)   t)

(check-for-bug :array-legacy-958
 (fill-pointer vmfad)   0)

(check-for-bug :array-legacy-961
 (vector-push-extend 0.0d0 vmfad)   0)

(check-for-bug :array-legacy-964
 (fill-pointer vmfad)   1)

(check-for-bug :array-legacy-967
 (vector-push-extend 1.0d0 vmfad)   1)

(check-for-bug :array-legacy-970
 (vector-push-extend 2.0d0 vmfad)   2)

(check-for-bug :array-legacy-973
 (vector-push-extend 3.0d0 vmfad)   3)

(check-for-bug :array-legacy-976
 (vector-push-extend 4.0d0 vmfad)   4)

(check-for-bug :array-legacy-979
 (vector-push-extend 5.0d0 vmfad)   5)

(check-for-bug :array-added-1
  (setf (fill-pointer vmfad) 3)
  3)

(check-for-bug :array-added-2
  (aref vmfad 5)
  5.0D0)

(check-for-bug :array-added-3
  (elt  vmfad 5)
  error)
  
(check-for-bug :array-added-4
  (setf (fill-pointer vmfad) 6)
  6)

(check-for-bug :array-added-5
  VMFAD
  #(0d0 1d0 2d0 3d0 4d0 5d0))

(check-for-bug :array-added-6
  (REVERSE VMFAD)
  #(5d0 4d0 3d0 2d0 1d0 0d0))

(check-for-bug :array-added-7
  (NREVERSE VMFAD)
  #(5d0 4d0 3d0 2d0 1d0 0d0))

(check-for-bug :array-added-8
  VMFAD
  #(5d0 4d0 3d0 2d0 1d0 0d0))

(check-for-bug :array-legacy-997
 (vector-pop vmfad)   0.0d0)

(check-for-bug :array-legacy-994
 (vector-pop vmfad)   1.0d0)

(check-for-bug :array-legacy-991
 (vector-pop vmfad)   2.0d0)

(check-for-bug :array-legacy-988
 (vector-pop vmfad)   3.0d0)

(check-for-bug :array-legacy-985
 (vector-pop vmfad)   4.0d0)

(check-for-bug :array-legacy-982
 (vector-pop vmfad)   5.0d0)

(check-for-bug :array-legacy-1000
 (vector-push-extend 5.0s0 vmfad)
 #+(or xcl gcl allegro cmu sbcl) error
 #+(or clisp ecls (and akcl (not gcl))) 0
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(unintern 'vmfad)
