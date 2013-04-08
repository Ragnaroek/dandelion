;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :lists154-legacy-4
  (subst 'a 'b
         '(u b
           (b)
           c))
  (u a
     (a)
     c))

(check-for-bug :lists154-legacy-13
  (subst 'a 'b
         '(u b
           (b)
           c)
         :test-not
         #'(lambda (x y)
             (if (atom y)
                 (eql x y)
                 t)))
  (a b
     (b . a)
     a . a))

(check-for-bug :lists154-legacy-27
  (subst 'a 'b
         '(u b
           (b)
           c)
         :test
         #'(lambda (x y)
             (not (eql x y))))
  a)

(check-for-bug :lists154-legacy-37
  (subst 'a 'b
         '(u b
           (b)
           c)
         :test-not
         #'(lambda (x y)
             (not (eql x y))))
  (u a
     (a)
     c))

(check-for-bug :lists154-legacy-49
  (subst 'a 'b
         '(u b
           (b)
           c)
         :test-not
         #'(lambda (x y)
             (not (eql x y)))
         :key
         #'(lambda (u)
             (if (listp u)
                 (car u))))
  (u . a))

(check-for-bug :lists154-legacy-63
  (subst-if 'nummmer 'numberp
            '((a (7 (v 6)))))
  ((a (nummmer (v nummmer)))))

(check-for-bug :lists154-legacy-68
  (subst-if-not 'nummmer 'numberp
                '((a (7 (v 6)))))
  nummmer)

(check-for-bug :lists154-legacy-73
  (subst-if-not 'nummmer
                #'(lambda (x)
                    (and (listp x)
                         (numberp x)))
                '((a (7 (v 6)))))
  nummmer)

(check-for-bug :lists154-legacy-81
  (subst-if-not 'nummmer
                #'(lambda (x)
                    (or (listp x)
                        (numberp x)))
                '((a (7 (v 6)))))
  ((nummmer (7 (nummmer 6)))))

(check-for-bug :lists154-legacy-89
  (nsubst 'a 'b
          '(u b
            (b)
            c)
          :test-not
          #'(lambda (x y)
              (if (atom y)
                  (eql x y)
                  t)))
  (a b
     (b . a)
     a . a))

(check-for-bug :lists154-legacy-103
  (nsubst 'a 'b
          '(u b
            (b)
            c)
          :test-not
          #'(lambda (x y)
              (not (eql x y))))
  (u a
     (a)
     c))

(check-for-bug :lists154-legacy-115
  (nsubst 'a 'b
          '(u b
            (b)
            c)
          :test
          #'(lambda (x y)
              (not (eql x y))))
  a)

(check-for-bug :lists154-legacy-125
  (nsubst-if 'oo 'numberp
             '(a b c
               (3 (4)
                0)))
  (a b c
     (oo (oo)
         oo)))

(check-for-bug :lists154-legacy-134
  (nsubst-if-not 'oo 'numberp
                 '(a b c
                   (3 (4)
                    0)))
  oo)

(check-for-bug :lists154-legacy-141
  (nsubst-if-not 'oo
                 #'(lambda (x)
                     (or (atom x)
                         (numberp x)))
                 '(a b c
                   (3 (4)
                    0)))
  oo)

(check-for-bug :lists154-legacy-151
  (nsubst-if-not 'oo
                 #'(lambda (x)
                     (and (atom x)
                          (numberp x)))
                 '(a b c
                   (3 (4)
                    0)))
  oo)

(check-for-bug :lists154-legacy-161
  (nsubst-if-not 'oo
                 #'(lambda (x)
                     (or (list x)
                         (numberp x)))
                 '(a b c
                   (3 (4)
                    0)))
  (a b c
     (3 (4)
        0)))

(check-for-bug :lists154-legacy-173
  (nsubst-if-not 'oo
                 #'(lambda (x)
                     (or (list x)
                         (symbolp x)))
                 '(a b c
                   (3 (4)
                    0)))
  (a b c
     (3 (4)
        0)))

(check-for-bug :lists154-legacy-185
  (sublis '((a . a1)
            (b . b1))
          '(a b))
  (a1 b1))

(check-for-bug :lists154-legacy-191
  (sublis '((a . a1)
            (b . b1))
          '(a b
            (b . c)))
  (a1 b1
      (b1 . c)))

(check-for-bug :lists154-legacy-199
  (sublis '((a . a1)
            (b . b1)
            (nil . nil1))
          '(a b
            (b . c)))
  (a1 b1
      (b1 . c) .
      nil1))

(check-for-bug :lists154-legacy-209
  (sublis '((a . a1)
            (b . b1)
            (nil . nil1))
          '(a b
            (b c)))
  (a1 b1
      (b1 c . nil1) .
      nil1))

(check-for-bug :lists154-legacy-219
  (sublis '((a . a1)
            (b . b1)
            (nil . nil1))
          '(a b
            (b c))
          :test-not 'eql)
  a1)

(check-for-bug :lists154-legacy-228
  (sublis '((a . a1)
            (b . b1)
            (nil . nil1))
          '(a b
            (b c))
          :test-not
          #'(lambda (x y)
              (if (atom y)
                  (eql x y))))
  a1)

(check-for-bug :lists154-legacy-240
  (sublis '(((a) .
             uu)
            (a . ii))
          '(i (a)
            a))
  (i (ii)
     ii))

(check-for-bug :lists154-legacy-249
  (sublis '(((a) . uu) (a . ii))
          '(i (a) a)
          :key #'(lambda (x) (if (listp x) (car x))))
  (i ii . ii))				; key wird angewandt auf: x ein blatt des baumes

(check-for-bug :lists154-legacy-249-bis-1
  (sublis '((1 . 2) (2 . 4) (3 . 6) (a . aa) (b . bb) (c . cc) (d . dd))
          '((a b (c (d 1) 2 (3)))))
  ((aa bb (cc (dd 2) 4 (6)))))

(check-for-bug :lists154-legacy-249-bis-2
  (sublis '((1 . 2) (2 . 4) (3 . 6) (a . aa) (b . bb) (c . cc) (d . dd))
          '((a b (c (d 1) 2 (3))))
          :test #'(lambda (x y) (and (numberp x) (numberp y) (= x y))))
  ((a b (c (d 2) 4 (6)))))

(check-for-bug :lists154-legacy-249-bis-3
  (sublis '((1 . 2) (2 . 4) (3 . 6) (a . aa) (b . bb) (c . cc) (d . dd))
         '((a b (c (d 1) 2 (3))))
         :test #'equalp :key #'(lambda (x) (and (symbolp x) x)))
  ((aa bb (cc (dd 1) 2 (3)))))

(check-for-bug :lists154-legacy-255
  (sublis '(((a) . uu) (a . ii))
          '(i (a) a)
          :test #'(lambda (x y) (if (listp y) (eql x (car y)))))
  #+(or xcl akcl lucid allegro ecls) (i ii . ii) ; x aus der aliste, y ein blatt des baumes
  #+(or clisp cmu sbcl)              (i (uu) uu) ; x ein blatt, y aus der aliste
  #-(or xcl clisp akcl cmu sbcl lucid allegro ecls) unknown)

(check-for-bug :lists154-legacy-263
  (nsublis '(((a) . uu) (a . ii))
           '(i (a) a)
           :key #'(lambda (x) (if (listp x) (car x))))
  (i ii . ii))				; key wird angewandt auf: x ein blatt des baumes

(check-for-bug :lists154-legacy-269
  (nsublis '(((a) . uu) (a . ii))
           '(i (a) a)
           :test #'(lambda (x y) (if (listp x) (equal x y))))
  (i uu . uu))

(check-for-bug :lists154-legacy-275
  (nsublis '(((a) . uu) (a . ii))
           '(i (a) a)
           :test #'(lambda (x y) (if (listp y) (equal x y))))
  (i uu . uu))

(check-for-bug :lists154-legacy-281
  (nsublis '(((a) . uu) (a . ii))
           '(i (a) a)
           :test #'(lambda (x y) (if (listp y) (eql x (car y)))))
  #+(or xcl akcl allegro ecls) (i ii . ii) ; x aus der aliste, y ein blatt des baumes
  #+(or clisp cmu sbcl lucid)  (i (uu) uu) ; x ein blatt, y aus der aliste
  #-(or xcl clisp akcl cmu sbcl lucid allegro ecls) unknown)

