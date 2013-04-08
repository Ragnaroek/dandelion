;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :map-legacy-4
  (setf a-vector (make-array 10))
  #+(or XCL cmu sbcl) #(0 0 0 0 0 0 0 0 0 0)
  #+(or CLISP AKCL ALLEGRO ecls) #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl ecls) UNKNOWN)

(check-for-bug :map-legacy-10
  (do ((i 0 (1+ i))
       (n (length a-vector)))
      ((= i n))
    (when (null (aref a-vector i))
      (setf (aref a-vector i) 0)))
  nil)

(check-for-bug :map-legacy-18
  (setq liste '(a b c d))
  (a b c d))

(check-for-bug :map-legacy-22
  (setq x 'anfangswert-von-x)
  anfangswert-von-x)

(check-for-bug :map-legacy-26
  (do ((x liste (cdr x))
       (oldx x x))
      ((null x))
    (print oldx) (print x))
  nil)

(check-for-bug :map-legacy-33
  (defun list-reverse(list)
    (do ((x list (cdr x))
         (y '() (cons (car x) y)))
        ((endp x) y)))
  list-reverse)

(check-for-bug :map-legacy-40
  (list-reverse '(a b c d))
  (d c b a))

(check-for-bug :map-legacy-44
  (setq foo '(a b c d))
  (a b c d))

(check-for-bug :map-legacy-48
  (setq bar '(1 2 3 4))
  (1 2 3 4))

(check-for-bug :map-legacy-52
  (defun fkt(a b) (cons a b))
  fkt)

;; mapcar

(check-for-bug :map-legacy-58
  (mapcar #'abs '(3 -4 2 -5 -6))
  (3 4 2 5 6))

(check-for-bug :map-legacy-62
  (mapcar #'cons '(a b c) '(1 2 3))
  ((a . 1) (b . 2) (c . 3)))


(check-for-bug :map-legacy-67
  (mapcar #'fkt foo bar)
  ((a . 1)(b . 2)(c . 3)(d . 4)))

(check-for-bug :map-legacy-71
  (do ((x foo (cdr x))
       (y bar (cdr y))
       (z '() (cons (fkt (car x) (car y)) z)))
      ((or (null x) (null y))
       (nreverse z)))
  ((a . 1)(b . 2)(c . 3)(d . 4)))

;; dolist
(check-for-bug :map-legacy-80
  (let    ((l '(1 2 3))
           (r 0))
    (dolist (x l r)
      (setf r (+ r  x)) ))
  6)


;; dolist
(check-for-bug :map-legacy-89
  (let ((l '(1 2 3)))
    (dolist (x l)(if (> 0 x)(incf x)(return 10))))
  10)

(check-for-bug :map-legacy-94
  (let ((l '(1 2 3)))
    (dolist (x l )(incf x)))
  nil)

;; dotimes

(check-for-bug :map-legacy-101
  (let ((s 0))
    (dotimes (i (+ 1 9)s)(setf s (+ s i))))
  45)


(check-for-bug :map-legacy-107
  (dolist (x '(a b c d)) (prin1 x) (princ " "))
  nil)

(check-for-bug :map-legacy-111
  (defun palindromep (string &optional
                             (start 0)
                             (end (length string)))
    (dotimes (k (floor (- end start) 2) t)
      (unless (char-equal (char string (+ start k))
                          (char string (- end k 1)))
        (return nil))))
  palindromep)

(check-for-bug :map-legacy-121
  (palindromep "Able was I ere I saw Elba")
  t)

(check-for-bug :map-legacy-125
  (palindromep "einnegermitgazellezagtimregennie")
  t)

(check-for-bug :map-legacy-129
  (palindromep "eisgekuehlter bommerlunder")
  nil)

(check-for-bug :map-legacy-133
  (palindromep (remove-if-not #'alpha-char-p
                              "A man, a plan, a canal -- Panama"))
  t)

(check-for-bug :map-legacy-138
  (MAPCAR (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
  ((A) (B) (C)))

(check-for-bug :map-legacy-142
  (MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
                                                                (1 2 3)))
  ((A 1) (B 2) (C 3)))

(check-for-bug :map-legacy-147
  (MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
                                                                (1 2)))
  ((A 1) (B 2)))

(check-for-bug :map-legacy-152
  (MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (C)) (QUOTE (1
                                                                   2)))
  ((C 1)))

(check-for-bug :map-legacy-157
  (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y))) (QUOTE (C)) (QUOTE (1
                                                                     2)) (U V W))
  ERROR)

(check-for-bug :map-legacy-162
  (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y))) (QUOTE (C)) (QUOTE (1
                                                                     2))
          (QUOTE (U V W)))
  ((C 1)))

(check-for-bug :map-legacy-168
  (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y))) (QUOTE (A B C)) (QUOTE
                                                                  (1 2 3))
          (QUOTE (U V W)))
  ((A 1) (B 2) (C 3)))

(check-for-bug :map-legacy-174
  (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
                                                                    (1 2 3))
          (QUOTE (U V W)))
  ((A 1 U) (B 2 V) (C 3 W)))

;; mapc
(check-for-bug :map-legacy-181
  (mapc #'abs '(3 -4 2 -5 -6))
  (3 -4 2 -5 -6))

(check-for-bug :map-legacy-185
  (MAPC (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
                                                                  (1 2 3))
        (QUOTE (U I V)))
  (A B C))

(check-for-bug :map-legacy-191
  (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
                                                                    (1 2 3))
          (QUOTE (U I V)))
  ((A 1 U) (B 2 I) (C 3 V)))

(check-for-bug :map-legacy-197
  (mapl #'(lambda (x y)(cons x y))'(a b c d)'(1 2 3 4))
  (a b c d))

(check-for-bug :map-legacy-201
  (MAPL (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
                                                                  (1 2 3))
        (QUOTE (U I V)))
  (A B C))

;; maplist

(check-for-bug :map-legacy-209
  (maplist #'(lambda (x)(cons 'foo x))'(a b c d))
  ((foo a b c d)(foo b c d)(foo c d)(foo d)))


(check-for-bug :map-legacy-214
  (maplist #'(lambda (x) (if (member (car x)(cdr x)) 0 1))
           '(a b a c d b c))
  (0 0 1 0 1 1 1))


(check-for-bug :map-legacy-220
  (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C))
           (QUOTE (1 2 3)) (QUOTE (U I V)))
  (((A B C) (1 2 3) (U I V)) ((B C) (2 3) (I V)) ((C) (3) (V))))

(check-for-bug :map-legacy-225
  (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C))
           (QUOTE (1 2 3)) (QUOTE (U I)))
  (((A B C) (1 2 3) (U I)) ((B C) (2 3) (I))))

(check-for-bug :map-legacy-230
  (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
                                                                     (1 2))
           (QUOTE (U I V)))
  (((A B C) (1 2) (U I V)) ((B C) (2) (I V))))

(check-for-bug :map-legacy-236
  (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B)) (QUOTE
                                                                   (1 2 3))
           (QUOTE (U I V)))
  (((A B) (1 2 3) (U I V)) ((B) (2 3) (I V))))

;; mapcon

(check-for-bug :map-legacy-244
  (mapcon #'(lambda (x)(and (oddp (car x))(list (car x))))'(5 4 3 2 1))
  (5 3 1))


(check-for-bug :map-legacy-249
  (MAPCON (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B)) (QUOTE
                                                                  (1 2 3))
          (QUOTE (U I V)))
  ((A B) (1 2 3) (U I V) (B) (2 3) (I V)))

(check-for-bug :map-legacy-255
  (MAPCON (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
                                                                    (1 2 3))
          (QUOTE (U I V)))
  ((A B C) (1 2 3) (U I V) (B C) (2 3) (I V) (C) (3) (V)))

;; mapcan

(check-for-bug :map-legacy-263
  (mapcan #'(lambda (x)(and (numberp x)(list x)))'(a 1 b c 3 4 d 5))
  (1 3 4 5))

(check-for-bug :map-legacy-267
  (MAPCAN (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
                                                                    (1 2 3))
          (QUOTE (U I V)))
  (A 1 U B 2 I C 3 V))

(check-for-bug :map-legacy-273
  (MAPCAN (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
                                                                (1 2 3)))
  (A 1 B 2 C 3))

(check-for-bug :map-legacy-278
  (MAPCAN (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
  (A B C))

(check-for-bug :map-legacy-282
  (MAPCON (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
  ((A B C) (B C) (C)))

(check-for-bug :map-legacy-286
  (MAPCON (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
                                                                (1 2)))
  ((A B C) (1 2) (B C) (2)))

(check-for-bug :map-legacy-291
  (MAPCON (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
  ((A B C) (B C) (C)))

