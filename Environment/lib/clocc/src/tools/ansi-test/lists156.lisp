;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :lists156-legacy-4
  (ACONS 'A 'B NIL)
  ((A . B)))

(check-for-bug :lists156-legacy-8
  (ACONS 'A 'B
         '((C . D)))
  ((A . B)
   (C . D)))

(check-for-bug :lists156-legacy-14
  (PAIRLIS '(A B C)
           '(1 2))
  #+XCL
  ((B . 2)
   (A . 1))
  #-XCL
  ERROR)

(check-for-bug :lists156-legacy-23
  (PAIRLIS '(A B C)
           '(1 2 3))
  #+(or XCL CLISP ALLEGRO cmu sbcl ecls)
  ((C . 3)
   (B . 2)
   (A . 1))
  #+AKCL ((A . 1) (B . 2) (C . 3))
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl ecls) UNKNOWN)

(check-for-bug :lists156-legacy-33
  (ASSOC 'A
         '((B C)
           (A U)
           (A I)))
  (A U))

(check-for-bug :lists156-legacy-40
  (ASSOC 'A
         '((B C)
           ((A)
            U)
           (A I)))
  (A I))

(check-for-bug :lists156-legacy-48
  (ASSOC 'A
         '((B C)
           ((A)
            U)
           (A I))
         :KEY
         #'(LAMBDA (X)
             (IF (LISTP X)
                 (CAR X))))
  ((A)
   U))

(check-for-bug :lists156-legacy-61
  (ASSOC 'A
         '((B C)
           A
           ((A)
            U)
           (A I))
         :KEY
         #'(LAMBDA (X)
             (IF (LISTP X)
                 (CAR X))))
  #-(or GCL ALLEGRO cmu sbcl)
  ((A) U)
  #+(or GCL ALLEGRO cmu sbcl)
  TYPE-ERROR)

(check-for-bug :lists156-legacy-77
  (ASSOC 'A
         '((B C)
           A
           ((A)
            U)
           (A I))
         :KEY
         #'(LAMBDA (X)
             (IF (ATOM X)
                 X)))
  #-(or GCL ALLEGRO cmu sbcl) (A I)
  #+(or GCL ALLEGRO cmu sbcl)
  TYPE-ERROR)

(check-for-bug :lists156-legacy-92
  (ASSOC 'A
         '((B C)
           A
           ((A)
            U)
           (A I))
         :TEST
         #'(LAMBDA (X Y)
             (IF (LISTP Y)
                 (EQL (CAR Y)
                      X))))
  #-(or GCL ALLEGRO cmu sbcl) ((A) U)
  #+(or GCL ALLEGRO cmu sbcl)
  TYPE-ERROR)

(check-for-bug :lists156-legacy-108
  (ASSOC 'A
         '((B C)
           A
           ((A)
            U)
           (A I))
         :TEST
         #'(LAMBDA (X Y)
             (IF (ATOM Y)
                 (EQL Y X))))
  #-(or GCL ALLEGRO cmu sbcl) (A I)
  #+(or GCL ALLEGRO cmu sbcl) ERROR)

(check-for-bug :lists156-legacy-122
  (ASSOC 'A
         '((B C)
           A
           ((A)
            U)
           (A I))
         :TEST-NOT
         #'(LAMBDA (X Y)
             (IF (ATOM Y)
                 (EQL Y X))))
  #-ALLEGRO (B C)
  #+ALLEGRO ERROR)

(check-for-bug :lists156-legacy-136
  (ASSOC-IF 'NUMBERP
            '((A . 3)
              (3 . A)))
  (3 . A))

(check-for-bug :lists156-legacy-142
  (ASSOC-IF 'SYMBOLP
            '((A . 3)
              (3 . A)))
  (A . 3))

(check-for-bug :lists156-legacy-148
  (ASSOC-IF-NOT 'SYMBOLP
                '((A . 3)
                  (3 . A)))
  (3 . A))

(check-for-bug :lists156-legacy-154
  (ASSOC-IF-NOT 'NUMBERP
                '((A . 3)
                  (3 . A)))
  (A . 3))

(check-for-bug :lists156-legacy-160
  (RASSOC 'A
          '((1 . B)
            (2 . A)))
  (2 . A))

(check-for-bug :lists156-legacy-166
  (RASSOC-IF 'SYMBOLP
             '((1 . B)
               (2 . A)))
  (1 . B))

(check-for-bug :lists156-legacy-172
  (RASSOC-IF 'SYMBOLP
             '((1 . 3)
               (2 . A)))
  (2 . A))

(check-for-bug :lists156-legacy-178
  (RASSOC-IF-NOT 'SYMBOLP
                 '((1 . 3)
                   (2 . A)))
  (1 . 3))

