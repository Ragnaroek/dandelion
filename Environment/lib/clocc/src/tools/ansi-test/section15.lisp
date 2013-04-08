;;; section 15: arrays -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(check-for-bug :section15-legacy-6
  (subtypep 'simple-array 'array)
  t)

;;; make-array

(check-for-bug :section15-legacy-12
  (make-array 4 :initial-element nil)
  #(NIL NIL NIL NIL))

(check-for-bug :section15-legacy-16
  (make-array '(2 4)
              :element-type '(unsigned-byte 2)
              :initial-contents '((0 1 2 3) (3 2 1 0)))
  #2A((0 1 2 3) (3 2 1 0)))

(check-for-bug :section15-legacy-22
  (make-array 6
              :element-type 'character
              :initial-element #\a
              :fill-pointer 3)
  "aaa")

(check-for-bug :section15-legacy-29
  (progn
    (setq a (make-array '(4 3)))
    t)
  t)

(check-for-bug :section15-legacy-35
  (dotimes (i 4)
    (dotimes (j 3)
      (setf (aref a i j) (list i 'x j '= (* i j)))))
  NIL)

(check-for-bug :section15-legacy-41
  (progn
    (setq b (make-array 8 :displaced-to a
                        :displaced-index-offset 2))
    t)
  t)

(check-for-bug :section15-legacy-48
  (let ((a '()))
    (dotimes (i 8)
      (setq a (append a (list i (aref b i)))))
    a)
  (0 (0 X 2 = 0)
     1 (1 X 0 = 0)
     2 (1 X 1 = 1)
     3 (1 X 2 = 2)
     4 (2 X 0 = 0)
     5 (2 X 1 = 2)
     6 (2 X 2 = 4)
     7 (3 X 0 = 0)))

(check-for-bug :section15-legacy-62
  (progn
    (setq a1 (make-array 50))
    t)
  t)

(check-for-bug :section15-legacy-68
  (progn
    (setq b1 (make-array 20 :displaced-to a1 :displaced-index-offset 10))
    t)
  t)

(check-for-bug :section15-legacy-74
  (length b1)
  20)

(check-for-bug :section15-legacy-78
  (progn
    (setq a2 (make-array 50 :fill-pointer 10))
    t)
  t)

(check-for-bug :section15-legacy-84
  (progn
    (setq b2 (make-array 20 :displaced-to a2 :displaced-index-offset 10))
    t)
  t)

(check-for-bug :section15-legacy-90
  (length a2)
  10)

(check-for-bug :section15-legacy-94
  (length b2)
  20)

(check-for-bug :section15-legacy-98
  (progn
    (setq a3 (make-array 50 :fill-pointer 10))
    t)
  t)

(check-for-bug :section15-legacy-104
  (progn
    (setq b3 (make-array 20 :displaced-to a3 :displaced-index-offset 10
                         :fill-pointer 5))
    t)
  t)

(check-for-bug :section15-legacy-111
  (length a3)
  10)

(check-for-bug :section15-legacy-115
  (length b3)
  5)


;;; adjust-array

(check-for-bug :section15-legacy-122
  (adjustable-array-p
   (setq ada (adjust-array
              (make-array '(2 3)
                          :adjustable t
                          :initial-contents '((a b c) (1 2 3)))
              '(4 6))))
  T )

(check-for-bug :section15-legacy-131
  (array-dimensions ada)
  (4 6) )

(check-for-bug :section15-legacy-135
  (aref ada 1 1)
  2 )

(check-for-bug :section15-legacy-139
  (setq beta (make-array '(2 3) :adjustable t))
  #+(or cmu sbcl) #2A((0 0 0) (0 0 0))
  #-(or cmu sbcl) #2A((NIL NIL NIL) (NIL NIL NIL)))

(check-for-bug :section15-legacy-144
  (adjust-array beta '(4 6) :displaced-to ada)
  #+(or cmu sbcl) #2A((A B C 0 0 0)
                      (1 2 3 0 0 0)
                      (0 0 0 0 0 0)
                      (0 0 0 0 0 0))
  #-(or cmu sbcl) #2A((A B C NIL NIL NIL)
                      (1 2 3 NIL NIL NIL)
                      (NIL NIL NIL NIL NIL NIL)
                      (NIL NIL NIL NIL NIL NIL)))

(check-for-bug :section15-legacy-155
  (array-dimensions beta)
  (4 6))

(check-for-bug :section15-legacy-159
  (aref beta 1 1)
  2 )

(check-for-bug :section15-legacy-163
  (let ((m
         (make-array '(4 4)
                     :adjustable t
                     :initial-contents
                     '(( alpha     beta      gamma     delta )
                       ( epsilon   zeta      eta       theta )
                       ( iota      kappa     lambda    mu    )
                       ( nu        xi        omicron   pi    )))))
    m)
  #2A(( alpha     beta      gamma     delta )
      ( epsilon   zeta      eta       theta )
      ( iota      kappa     lambda    mu    )
      ( nu        xi        omicron   pi    )))

(check-for-bug :section15-legacy-178
  (let ((m
         (make-array '(4 4)
                     :adjustable t
                     :initial-contents
                     '(( alpha     beta      gamma     delta )
                       ( epsilon   zeta      eta       theta )
                       ( iota      kappa     lambda    mu    )
                       ( nu        xi        omicron   pi    )))))
    (adjust-array m '(3 5) :initial-element 'baz))
  #2A(( alpha     beta      gamma     delta     baz )
      ( epsilon   zeta      eta       theta     baz )
      ( iota      kappa     lambda    mu        baz )))

;;; adjustable-array-p

(check-for-bug :section15-legacy-194
  (adjustable-array-p
   (make-array 5
               :element-type 'character
               :adjustable t
               :fill-pointer 3))
  t)

;;; aref

(check-for-bug :section15-legacy-204
  (aref (setq alpha (make-array 4)) 3)
  #+(or cmu sbcl) 0
  #+(or clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)

(check-for-bug :section15-legacy-210
  (setf (aref alpha 3) 'sirens)
  SIRENS)

(check-for-bug :section15-legacy-214
  (aref alpha 3)
  SIRENS)

(check-for-bug :section15-legacy-218
  (aref (setq beta (make-array '(2 4)
                               :element-type '(unsigned-byte 2)
                               :initial-contents '((0 1 2 3) (3 2 1 0))))
        1 2)
  1)

(check-for-bug :section15-legacy-225
  (setq gamma '(0 2))
  (0 2))

(check-for-bug :section15-legacy-229
  (apply #'aref beta gamma)
  2)

(check-for-bug :section15-legacy-233
  (setf (apply #'aref beta gamma) 3)
  3)

(check-for-bug :section15-legacy-237
  (apply #'aref beta gamma)
  3)

(check-for-bug :section15-legacy-241
  (aref beta 0 2)
  3)

;;; array-dimension

(check-for-bug :section15-legacy-247
  (array-dimension (make-array 4) 0)
  4)

(check-for-bug :section15-legacy-251
  (array-dimension (make-array '(2 3)) 1)
  3)

;;; array-dimensions

(check-for-bug :section15-legacy-257
  (array-dimensions (make-array 4))
  (4))

(check-for-bug :section15-legacy-261
  (array-dimensions (make-array '(2 3)))
  (2 3))

(check-for-bug :section15-legacy-265
  (array-dimensions (make-array 4 :fill-pointer 2))
  (4))

;;; array-element-type

(check-for-bug :section15-legacy-271
  (array-element-type (make-array 4))
  T)

(check-for-bug :section15-legacy-275
  (array-element-type (make-array 12 :element-type '(unsigned-byte 8)))
  #+(or cmu sbcl clisp) (unsigned-byte 8)
  #+ecls fixnum
  #-(or cmu sbcl clisp ecls) fill-this-in)

(check-for-bug :section15-legacy-281
  (array-element-type (make-array 12 :element-type '(unsigned-byte 5)))
  #+(or cmu sbcl clisp) (unsigned-byte 8)
  #+ecls fixnum
  #-(or cmu sbcl clisp ecls) fill-this-in)

(check-for-bug :section15-legacy-287
  (array-element-type (make-array 5 :element-type '(mod 5)))
  #+(or cmu sbcl clisp) (UNSIGNED-BYTE 4)
  #+ecls fixnum
  #-(or cmu sbcl clisp ecls) fill-this-in)
					; (mod 5), (mod 8), fixnum, t, or any other type of which (mod 5) is a subtype.

;;; array-has-fill-pointer

(check-for-bug :section15-legacy-296
  (array-has-fill-pointer-p (make-array 4))
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)

(check-for-bug :section15-legacy-301
  (array-has-fill-pointer-p (make-array '(2 3)))
  nil)

(check-for-bug :section15-legacy-305
  (array-has-fill-pointer-p
   (make-array 8
               :fill-pointer 2
               :initial-element 'filler))
  t)

;;; array-displacement

(check-for-bug :section15-legacy-314
  (progn
    (setq a1 (make-array 5))
    t)
  t)

(check-for-bug :section15-legacy-320
  (progn
    (setq a2 (make-array 4 :displaced-to a1
                         :displaced-index-offset 1))
    t)
  t)

(check-for-bug :section15-legacy-327
  (progn
    (multiple-value-bind (a b)
        (array-displacement a2)
      (list a b))
    t)
  t)

(check-for-bug :section15-legacy-335
  (progn
    (setq a3 (make-array 2 :displaced-to a2
                         :displaced-index-offset 2))
    t)
  t)

(check-for-bug :section15-legacy-342
  (progn
    (array-displacement a3)
    t)
  t)

;;; array-in-bounds

(check-for-bug :section15-legacy-350
  (progn
    (setq a (make-array '(7 11) :element-type 'string-char))
    t)
  t)

(check-for-bug :section15-legacy-356
  (array-in-bounds-p a 0  0)
  t)

(check-for-bug :section15-legacy-360
  (array-in-bounds-p a 6 10)
  t)

(check-for-bug :section15-legacy-364
  (array-in-bounds-p a 0 -1)
  nil)

(check-for-bug :section15-legacy-368
  (array-in-bounds-p a 0 11)
  nil)

(check-for-bug :section15-legacy-372
  (array-in-bounds-p a 7  0)
  nil)

;;; array-rank

(check-for-bug :section15-legacy-378
  (array-rank (make-array '()))
  0)

(check-for-bug :section15-legacy-382
  (array-rank (make-array 4))
  1)

(check-for-bug :section15-legacy-386
  (array-rank (make-array '(4)))
  1)

(check-for-bug :section15-legacy-390
  (array-rank (make-array '(2 3)))
  2)

;;; array-row-major-index

(check-for-bug :section15-legacy-396
  (progn
    (setq a (make-array '(4 7) :element-type '(unsigned-byte 8)))
    t)
  t)

(check-for-bug :section15-legacy-402
  (array-row-major-index a 1 2)
  9)

(check-for-bug :section15-legacy-406
  (array-row-major-index
   (make-array '(2 3 4)
               :element-type '(unsigned-byte 8)
               :displaced-to a
               :displaced-index-offset 4)
   0 2 1)
  9)

;;; array-total-size

(check-for-bug :section15-legacy-417
  (array-total-size (make-array 4))
  4)

(check-for-bug :section15-legacy-421
  (array-total-size (make-array 4 :fill-pointer 2))
  4)

(check-for-bug :section15-legacy-425
  (array-total-size (make-array 0))
  0)

(check-for-bug :section15-legacy-429
  (array-total-size (make-array '(4 2)))
  8)

(check-for-bug :section15-legacy-433
  (array-total-size (make-array '(4 0)))
  0)

(check-for-bug :section15-legacy-437
  (array-total-size (make-array '()))
  1)

;;; arrayp

(check-for-bug :section15-legacy-443
  (arrayp (make-array '(2 3 4) :adjustable t))
  t)

(check-for-bug :section15-legacy-447
  (arrayp (make-array 6))
  t)

(check-for-bug :section15-legacy-451
  (arrayp #*1011)
  t)

(check-for-bug :section15-legacy-455
  (arrayp "hi")
  t)

(check-for-bug :section15-legacy-459
  (arrayp 'hi)
  nil)

(check-for-bug :section15-legacy-463
  (arrayp 12)
  nil)

;;; fill-pointer

(check-for-bug :section15-legacy-469
  (setq a (make-array 8 :fill-pointer 4))
  #+(or cmu sbcl) #(0 0 0 0)
  #-(or cmu sbcl) #(NIL NIL NIL NIL))

(check-for-bug :section15-legacy-474
  (fill-pointer a)
  4)

(check-for-bug :section15-legacy-478
  (dotimes (i (length a)) (setf (aref a i) (* i i)))
  NIL)

(check-for-bug :section15-legacy-482
  a
  #(0 1 4 9))

(check-for-bug :section15-legacy-486
  (setf (fill-pointer a) 3)
  3)

(check-for-bug :section15-legacy-490
  (fill-pointer a)
  3)

(check-for-bug :section15-legacy-494
  a
  #(0 1 4))

(check-for-bug :section15-legacy-498
  (setf (fill-pointer a) 8)
  8)

(check-for-bug :section15-legacy-502
  a
  #+(or cmu sbcl) #(0 1 4 9 0 0 0 0)
  #-(or cmu sbcl) #(0 1 4 9 NIL NIL NIL NIL))

(check-for-bug :section15-legacy-507
  (>= ARRAY-DIMENSION-LIMIT 1024)
  t)

(check-for-bug :section15-legacy-511
  (>= ARRAY-RANK-LIMIT  8)
  t)

(check-for-bug :section15-legacy-515
  (>= ARRAY-TOTAL-SIZE-LIMIT 1024)
  t)

;;; simple-vector-p

(check-for-bug :section15-legacy-521
  (simple-vector-p (make-array 6))
  t)

(check-for-bug :section15-legacy-525
  (simple-vector-p "aaaaaa")
  nil)

(check-for-bug :section15-legacy-529
  (simple-vector-p (make-array 6 :fill-pointer t))
  nil)

;;; svref

(check-for-bug :section15-legacy-535
  (simple-vector-p (setq v (vector 1 2 'sirens)))
  t)

(check-for-bug :section15-legacy-539
  (svref v 0)
  1)

(check-for-bug :section15-legacy-543
  (svref v 2)
  SIRENS)

(check-for-bug :section15-legacy-547
  (setf (svref v 1) 'newcomer)
  NEWCOMER               )

(check-for-bug :section15-legacy-551
  v
  #(1 NEWCOMER SIRENS))

;;; vector

(check-for-bug :section15-legacy-557
  (arrayp (setq v (vector 1 2 'sirens)))
  t)

(check-for-bug :section15-legacy-561
  (vectorp v)
  t)

(check-for-bug :section15-legacy-565
  (simple-vector-p v)
  t         )

(check-for-bug :section15-legacy-569
  (length v)
  3)

;;; vector-pop

(check-for-bug :section15-legacy-575
  (vector-push (setq fable (list 'fable))
               (setq fa (make-array 8
                                    :fill-pointer 2
                                    :initial-element 'sisyphus)))
  2 )

(check-for-bug :section15-legacy-582
  (fill-pointer fa)
  3 )

(check-for-bug :section15-legacy-586
  (eq (vector-pop fa) fable)
  t)

(check-for-bug :section15-legacy-590
  (vector-pop fa)
  SISYPHUS )

(check-for-bug :section15-legacy-594
  (fill-pointer fa)
  1 )

;;; vector-push

(check-for-bug :section15-legacy-600
  (vector-push (setq fable (list 'fable))
               (setq fa (make-array 8
                                    :fill-pointer 2
                                    :initial-element 'first-one)))
  2)

(check-for-bug :section15-legacy-607
  (fill-pointer fa)
  3 )

(check-for-bug :section15-legacy-611
  (eq (aref fa 2) fable)
  t)

(check-for-bug :section15-legacy-615
  (vector-push-extend #\X
                      (setq aa
                            (make-array 5
                                        :element-type 'character
                                        :adjustable t
                                        :fill-pointer 3)))
  3)

(check-for-bug :section15-legacy-624
  (fill-pointer aa)
  4 )

(check-for-bug :section15-legacy-628
  (vector-push-extend #\Y aa 4)
  4 )

(check-for-bug :section15-legacy-632
  (>= (array-total-size aa) 5)
  t)

(check-for-bug :section15-legacy-636
  (vector-push-extend #\Z aa 4)
  5 )

(check-for-bug :section15-legacy-640
  (>= (array-total-size aa) 9)
  t)

;;; vectorp

(check-for-bug :section15-legacy-646
  (vectorp "aaaaaa")
  t)

(check-for-bug :section15-legacy-650
  (vectorp (make-array 6 :fill-pointer t))
  t)

(check-for-bug :section15-legacy-654
  (vectorp (make-array '(2 3 4)))
  nil)

(check-for-bug :section15-legacy-658
  (vectorp #*11)
  t)

(check-for-bug :section15-legacy-662
  (vectorp #b11)
  nil)

;;; bit

(check-for-bug :section15-legacy-668
  (bit (setq ba (make-array 8
                            :element-type 'bit
                            :initial-element 1))
       3)
  1)

(check-for-bug :section15-legacy-675
  (setf (bit ba 3) 0)
  0)

(check-for-bug :section15-legacy-679
  (bit ba 3)
  0)

(check-for-bug :section15-legacy-683
  (sbit ba 5)
  1)

(check-for-bug :section15-legacy-687
  (setf (sbit ba 5) 1)
  1)

(check-for-bug :section15-legacy-691
  (sbit ba 5)
  1)

;;; bit-and etc

(check-for-bug :section15-legacy-697
  (bit-and (setq ba #*11101010) #*01101011)
  #*01101010)

(check-for-bug :section15-legacy-701
  (bit-and #*1100 #*1010)
  #*1000      )

(check-for-bug :section15-legacy-705
  (bit-andc1 #*1100 #*1010)
  #*0010)

(check-for-bug :section15-legacy-709
  (setq rba (bit-andc2 ba #*00110011 t))
  #*11001000)

(check-for-bug :section15-legacy-713
  (eq rba ba)
  t)

(check-for-bug :section15-legacy-717
  (bit-not (setq ba #*11101010))
  #*00010101)

(check-for-bug :section15-legacy-721
  (setq rba (bit-not ba
                     (setq tba (make-array 8
                                           :element-type 'bit))))
  #*00010101)

(check-for-bug :section15-legacy-727
  (equal rba tba)
  t)

(check-for-bug :section15-legacy-731
  (bit-xor #*1100 #*1010)
  #*0110)

;;; bit-vector-p

(check-for-bug :section15-legacy-737
  (bit-vector-p (make-array 6
                            :element-type 'bit
                            :fill-pointer t))
  t)

(check-for-bug :section15-legacy-743
  (bit-vector-p #*)
  t)

(check-for-bug :section15-legacy-747
  (bit-vector-p (make-array 6))
  nil)

;;; simple-bit-vector

(check-for-bug :section15-legacy-753
  (simple-bit-vector-p (make-array 6))
  nil)

(check-for-bug :section15-legacy-757
  (simple-bit-vector-p #*)
  t)







