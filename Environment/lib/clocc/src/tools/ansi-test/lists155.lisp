;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :lists155-legacy-4
  (member 'a
          '((a)
            (b)
            (a)
            (c)))
  nil)

(check-for-bug :lists155-legacy-12
  (member 'a
          '((a)
            (b)
            (a)
            (c))
          :key 'car)
  ((a)
   (b)
   (a)
   (c)))

(check-for-bug :lists155-legacy-24
  (member-if 'numberp
             '((a)
               (b)
               (3)
               (c))
             :key 'car)
  ((3)
   (c)))

(check-for-bug :lists155-legacy-34
  (member-if-not 'numberp
                 '((8)
                   (a)
                   (b)
                   (3)
                   (c))
                 :key 'car)
  ((a)
   (b)
   (3)
   (c)))

(check-for-bug :lists155-legacy-47
  (tailp '(a b)
         '(u a b))
  nil)

(check-for-bug :lists155-legacy-52
  (tailp (cddr (setq xx
                     '(u i a b)))
         xx)
  t)

(check-for-bug :lists155-legacy-58
  (tailp (cddr (setq xx
                     '(u i a b)))
         xx)
  t)

(check-for-bug :lists155-legacy-64
  (adjoin 'a
          '(a b c))
  (a b c))

(check-for-bug :lists155-legacy-69
  (adjoin 'a
          '((a)
            b c)
          :test 'equal)
  (a (a)
     b c))

(check-for-bug :lists155-legacy-77
  (adjoin 'a
          '((a)
            b c)
          :test 'equal)
  (a (a)
     b c))

(check-for-bug :lists155-legacy-85
  (union '(a b c d)
         '(a d i v))
  #+xcl (v i a b c d)
  #+(or clisp akcl ecls) (b c a d i v)
  #+(or allegro cmu sbcl) (c b a d i v)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-93
  (nunion '(a b c d)
          '(u i b a))
  #+xcl (a b c d u i)
  #+(or clisp akcl ecls) (c d u i b a)
  #+(or allegro cmu sbcl) (d c u i b a)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-101
  (nintersection '(a b c d)
                 '(c d e f g))
  #+(or xcl clisp gcl ecls) (c d)
  #+(or allegro cmu sbcl) (d c)
  #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-108
  (nintersection '(a b c d)
                 '(c d e f g)
                 :test-not 'eql)
  #+(or xcl clisp gcl ecls) (a b c d)
  #+(or allegro cmu sbcl) (d c b a)
  #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-116
  (set-difference '(a b c d e)
                  '(d b e))
  #+(or xcl allegro gcl cmu sbcl ecls) (c a)
  #+(or clisp (and akcl (not gcl))) (a c)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-123
  (set-difference
   '(auto anton berta berlin)
   '(a)
   :test
   #'(lambda (x y)
       (eql (elt (symbol-name x)
                 1)
            (elt (symbol-name y)
                 1))))
  #+(or xcl allegro)
  (berlin berta anton auto)
  #-(or xcl allegro)
  type-error)

(check-for-bug :lists155-legacy-138
  (set-difference '(anton berta auto berlin)
                  '(amerilla)
                  :test
                  #'(lambda (x y)
                      (eql (elt (symbol-name x)
                                0)
                           (elt (symbol-name y)
                                0))))
  #+(or xcl gcl allegro cmu sbcl) (berlin berta)
  #+(or clisp (and akcl (not gcl)) ecls) (berta berlin)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-151
  (nset-difference '(a b c d)
                   '(i j c))
  #+(or xcl clisp gcl ecls) (a b d)
  #+(or allegro cmu sbcl) (d b a)
  #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-158
  (set-exclusive-or '(a b c d)
                    '(c a i l))
  #+(or xcl gcl) (d b l i)
  #+(or clisp (and akcl (not gcl)) ecls) (b d i l)
  #+(or allegro cmu sbcl) (l i d b)
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-166
  (set-exclusive-or '(anton anna emil)
                    '(berta auto august)
                    :test
                    #'(lambda (x y)
                        (eql (elt (symbol-name x)
                                  0)
                             (elt (symbol-name y)
                                  0))))
  #+(or xcl clisp gcl ecls) (emil berta)
  #+(or allegro cmu sbcl) (berta emil)
  #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(check-for-bug :lists155-legacy-179
  (nset-exclusive-or '(a b c)
                     '(i a d c))
  (b i d))

(check-for-bug :lists155-legacy-184
  (subsetp '(a b)
           '(b u i a c d))
  t)

(check-for-bug :lists155-legacy-189
  (subsetp '(a b)
           '(b u i c d))
  nil)

(check-for-bug :lists155-legacy-194
  (subsetp '(a b)
           '(b a u i c d))
  t)

(check-for-bug :lists155-legacy-199
  (subsetp '(a b)
           '(a u i c d))
  nil)

