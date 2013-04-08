;;; based on 1.2 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :lambda-legacy-4
  (makunbound 'b) b)

(check-for-bug :lambda-legacy-7
  (makunbound 'e) e)

(check-for-bug :lambda-legacy-10
  (setq z 2) 2)

(check-for-bug :lambda-legacy-13
  ((lambda (z) (declare (special z)) (list z (symbol-value 'z))) 3)
  (3 3))

(check-for-bug :lambda-legacy-17
  (makunbound 'z) z)

(check-for-bug :lambda-legacy-20
  ((lambda (a b) (+ a (* b 3))) 4 5)
  19)

(check-for-bug :lambda-legacy-24
  ((lambda (a &optional (b 2)) (+ a (* b 3))) 4 5)
  19)

(check-for-bug :lambda-legacy-28
  ((lambda (a &optional (b 2)) (+ a (* b 3))) 4)
  10)

(check-for-bug :lambda-legacy-32
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)))
  (2 nil 3 nil nil))

(check-for-bug :lambda-legacy-36
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6)
  (6 t 3 nil nil))

(check-for-bug :lambda-legacy-40
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3)
  (6 t 3 t nil))

(check-for-bug :lambda-legacy-44
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3
   8)
  (6 t 3 t (8)))

(check-for-bug :lambda-legacy-49
  ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3
   8 9 10 11)
  (6 t 3 t (8 9 10 11)))

(check-for-bug :lambda-legacy-54
  ((lambda (a b &key c d) (list a b c d)) 1 2)
  (1 2 nil nil))

(check-for-bug :lambda-legacy-58
  ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6)
  (1 2 6 nil))

(check-for-bug :lambda-legacy-62
  ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8)
  (1 2 nil 8))

(check-for-bug :lambda-legacy-66
  ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6 :d 8)
  (1 2 6 8))

(check-for-bug :lambda-legacy-70
  ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8 :c 6)
  (1 2 6 8))

(check-for-bug :lambda-legacy-74
  ((lambda (a b &key c d) (list a b c d)) :a 1 :d 8 :c 6)
  (:a 1 6 8))

(check-for-bug :lambda-legacy-78
  ((lambda (a b &key c d) (list a b c d)) :a :b :c :d)
  (:a :b :d nil))

(check-for-bug :lambda-legacy-82
  ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
   1)
  (1 3 nil 1 nil))

(check-for-bug :lambda-legacy-87
  ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
   1 2)
  (1 2 nil 1 nil))

(check-for-bug :lambda-legacy-92
  ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
   :c 7)
  (:c 7 nil :c nil))

(check-for-bug :lambda-legacy-97
  ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
   1 6 :c 7)
  (1 6 7 1 (:c 7)))

(check-for-bug :lambda-legacy-102
  ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
   1 6 :d 8)
  (1 6 nil 8 (:d 8)))

(check-for-bug :lambda-legacy-107
  ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
   1 6 :d 8 :c
   9 :d 10)
  (1 6 9 8 (:d 8 :c 9 :d 10)))

(check-for-bug :lambda-legacy-113
  ((lambda (x &aux (a 3) (b 4)) (+ x (* a b))) 2)
  14)

(check-for-bug :lambda-legacy-117
  ((lambda (x y &optional a b &rest z &key c (d y) &aux (u 3) (v 4))
     (+ x y a (* b (car z)) c (* d u) v))
   3 4 5 2 7 :c 6 :d 8)
  program-error)

(check-for-bug :lambda-legacy-123
  ((lambda (x y &optional a b &rest z &key c (d y) &aux (u 3) (v 4))
     (+ x y a (* b (car z)) c (* d u) v))
   3 4 5 2 7 :c 6)
  program-error)

(check-for-bug :lambda-legacy-129
  ((lambda (x &aux c) (cons x c)) (quote a))
  (a))

(check-for-bug :lambda-legacy-133
  ((lambda (x &rest y z) (list x y z)) 1 2 3)
  error)

(check-for-bug :lambda-legacy-137
  ((lambda (5 a b) (list a b)) 1 2)
  error)

(check-for-bug :lambda-legacy-141
  ((lambda ((length (quote (a b))) c) (list c)) 1)
  error)

(check-for-bug :lambda-legacy-145
  ((lamda (x &key :y :z) (list x y z)) 1 :y 2 :z 3)
  error)

(check-for-bug :lambda-legacy-149
  ((lambda (x y) (list x y z)) 1 2)
  unbound-variable)

(check-for-bug :lambda-legacy-153
  ((lambda (x y) (list x y z)) 1 2 3)
  error)

(check-for-bug :lambda-legacy-157
  ((lambda (&optional) (list a b c)) 1)
  error)

(check-for-bug :lambda-legacy-161
  ((lambda (&optional (a)) (list a)) 1)
  (1))

(check-for-bug :lambda-legacy-165
  ((lambda (&optional (a b)) (list a b)) 1)
  unbound-variable)

(check-for-bug :lambda-legacy-169
  ((lambda (&optional (a 3 b)) (list a b)) 1)
  (1 t))

(check-for-bug :lambda-legacy-173
  ((lambda (&optional (a 3)) (list a)) 1)
  (1))

(check-for-bug :lambda-legacy-177
  ((lambda (&optional (a 3 b 4)) (list a b)) 1)
  #+xcl (1 t)
  #-xcl error)

(check-for-bug :lambda-legacy-182
  ((lambda (x) (list x y)) 1 2)
  error)

(check-for-bug :lambda-legacy-186
  ((lambda (x) (list x)) 1 2)
  error)

(check-for-bug :lambda-legacy-190
  ((lambda (#\a) (list a)) 1)
  error)

(check-for-bug :lambda-legacy-194
  ((lambda (#*10) (list 1 2 3)))
  error)

(check-for-bug :lambda-legacy-198
  ((lambda (x y) ((lambda (a b) (list a b)) (quote u) (quote v))) 5 6)
  (u v))

(check-for-bug :lambda-legacy-202
  ((lambda (x y) (list x y)) 1)
  error)

(check-for-bug :lambda-legacy-206
  ((lambda (x &rest y &optional (z 5)) (list x y z)) 1 3)
  error)

(check-for-bug :lambda-legacy-210
  ((lambda (x &x) (list x)) 7)
  error)

(check-for-bug :lambda-legacy-214
  ((lambda (x &aux) (list x)) 6)
  (6))

(check-for-bug :lambda-legacy-218
  ((lambda (x &aux y) (list x y)) 6)
  (6 nil))

(check-for-bug :lambda-legacy-222
  ((lambda (x &aux (y)) (list x y)) 6)
  (6 nil))

(check-for-bug :lambda-legacy-226
  ((lambda (x &rest) (list x)) 2)
  error)

(check-for-bug :lambda-legacy-230
  ((lambda (x &key) (list x)) 3)
  (3))

(check-for-bug :lambda-legacy-234
  ((lambda (x &key y) (list x)) 3)
  (3))

(check-for-bug :lambda-legacy-238
  ((lambda (x &key y) (list x)) 3 :y)
  error)

(check-for-bug :lambda-legacy-242
  ((lambda (x &key y) (list x)) :\3)
  (:\3))

(check-for-bug :lambda-legacy-246
  ((lambda nil (list 1 2 3)))
  (1 2 3))

(check-for-bug :lambda-legacy-250
  ((lambda nil (list 1 2 3)) 4 5)
  error)

(check-for-bug :lambda-legacy-254
  ((lambda (list 1 2 3)))
  error)

(check-for-bug :lambda-legacy-258
  ((lambda (x)))
  error)

(check-for-bug :lambda-legacy-262
  ((lambda (&aux &key &rest &optional)))
  error)

(check-for-bug :lambda-legacy-266
  ((lambda (a b &key c d &allow-other-keys) (list a b c d e f)) 1 2 :c
   6 :d 8 :e 5
   :f 7)
  error)

(check-for-bug :lambda-legacy-272
  ((lambda (x &allow-other-keys) (list x y)) 2 :y 3)
  error)

(check-for-bug :lambda-legacy-276
  ((lambda))
  error)

;; CLHS 3.4.1.4.1.1

(check-for-bug :lambda-added-1
  ((lambda (&key x) x) :x 1 :y 2 :allow-other-keys t)
  1)

(check-for-bug :lambda-added-2
  ((lambda (&key x) x) :x 1 :y 2 :allow-other-keys t :allow-other-keys nil)
  1)

(check-for-bug :lambda-added-3
  ((lambda (&key x) x) :x 1 :y 2 :allow-other-keys nil :allow-other-keys t)
  error)

(check-for-bug :lambda-added-4
  ((lambda (&key x &allow-other-keys) x) :x 1 :y 2)
  1)

(check-for-bug :lambda-added-5
  ((lambda (&key x &allow-other-keys) x) :x 1 :y 2 :allow-other-keys t :allow-other-keys nil)
  1)

(check-for-bug :lambda-added-6
  ((lambda (&key x &allow-other-keys) x) :x 1 :y 2 :allow-other-keys nil :allow-other-keys t)
  1)

(check-for-bug :lambda-added-7
  ((lambda (&key x) x) :x 1 :allow-other-keys nil)
  1)

(check-for-bug :lambda-added-8
  ((lambda (&key x) x) :x 1 :allow-other-keys nil :allow-other-keys nil)
  1)


