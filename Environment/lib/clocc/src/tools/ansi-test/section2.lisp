;;; 2.1.4.5.1 examples of mutiple escape characters -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(check-for-bug :section2-legacy-6
  (eq 'abc 'ABC)
  T)

(check-for-bug :section2-legacy-10
  (eq 'abc '|ABC|)
  T)

(check-for-bug :section2-legacy-14
  (eq 'abc 'a|B|c)
  T)

(check-for-bug :section2-legacy-18
  (eq 'abc '|abc|)
  nil)

;;; 2.1.4.6.1
(check-for-bug :section2-legacy-23
  (eq 'abc '\A\B\C)
  T)

(check-for-bug :section2-legacy-27
  (eq 'abc 'a\Bc)
  T)

(check-for-bug :section2-legacy-31
  (eq 'abc '\ABC)
  T)

(check-for-bug :section2-legacy-35
  (eq 'abc '\abc)
  nil)

;;; 2.1.4.7.1
(check-for-bug :section2-legacy-40
  (length '(this-that))
  1)

(check-for-bug :section2-legacy-44
  (length '(this - that))
  3)

(check-for-bug :section2-legacy-48
  (length '(a
            b))
  2)

(check-for-bug :section2-legacy-53
  (+ 34)
  34)

(check-for-bug :section2-legacy-57
  (+ 3 4)
  7)

;;; 2.4.1

(check-for-bug :section2-legacy-63
  (cons 'this-one 'that-one)
  (this-one . that-one))


;;; 2.4.3.1

(check-for-bug :section2-legacy-70
  'foo
  FOO)

(check-for-bug :section2-legacy-74
  ''foo
  (QUOTE FOO))

(check-for-bug :section2-legacy-78
  (car ''foo)
  QUOTE)

;;; 2.4.4.1

(check-for-bug :section2-legacy-84
  (+ 3					; three
     4)
  7)

;;; 2.4.8.7

(check-for-bug :section2-legacy-91
  #B1101
  13 )

(check-for-bug :section2-legacy-95
  #b101/11
  5/3)

;;; 2.4.8.8
(check-for-bug :section2-legacy-100
  #o37/15
  31/13)

(check-for-bug :section2-legacy-104
  #o777
  511)

(check-for-bug :section2-legacy-108
  #o105
  69)

;;; 2.4.8.9
(check-for-bug :section2-legacy-113
  #xF00
  3840             )

(check-for-bug :section2-legacy-117
  #x105
  261 )

;;; 2.4.8.10
(check-for-bug :section2-legacy-122
  #2r11010101
  213)

(check-for-bug :section2-legacy-126
  #b11010101
  213)

(check-for-bug :section2-legacy-130
  #b+11010101
  213)

(check-for-bug :section2-legacy-134
  #o325
  213)

(check-for-bug :section2-legacy-138
  #xD5
  213)

(check-for-bug :section2-legacy-142
  #16r+D5
  213)

(check-for-bug :section2-legacy-146
  #o-300
  -192)

(check-for-bug :section2-legacy-150
  #3r-21010
  -192)

(check-for-bug :section2-legacy-154
  #25R-7H
  -192)

(check-for-bug :section2-legacy-158
  #xACCEDED
  181202413)


