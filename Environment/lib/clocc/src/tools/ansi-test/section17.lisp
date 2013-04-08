;;; section 17: sequences -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; 17.2.1.1

(check-for-bug :section17-legacy-8
  (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'equal)
  (foo bar "BAR" "foo" "bar"))

(check-for-bug :section17-legacy-12
  (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'equalp)
  (foo bar "BAR" "bar"))

(check-for-bug :section17-legacy-16
  (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'string-equal)
  (bar "BAR" "bar"))

(check-for-bug :section17-legacy-20
  (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'string=)
  (BAR "BAR" "foo" "bar"))

(check-for-bug :section17-legacy-24
  (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test-not #'eql)
  (1))

(check-for-bug :section17-legacy-28
  (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test-not #'=)
  (1 1.0 #C(1.0 0.0)))

(check-for-bug :section17-legacy-32
  (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test (complement #'=))
  (1 1.0 #C(1.0 0.0)))

(check-for-bug :section17-legacy-36
  (count 1 '((one 1) (uno 1) (two 2) (dos 2)) :key #'cadr)
  2)

(check-for-bug :section17-legacy-40
  (count 2.0 '(1 2 3) :test #'eql :key #'float)
  1)

(check-for-bug :section17-legacy-44
  (count "FOO" (list (make-pathname :name "FOO" :type "X")
                     (make-pathname :name "FOO" :type "Y"))
         :key #'pathname-name
         :test #'equal)
  2)

;;; 17.2.2.1

(check-for-bug :section17-legacy-53
  (count-if #'zerop '(1 #C(0.0 0.0) 0 0.0d0 0.0s0 3))
  4)

(check-for-bug :section17-legacy-57
  (remove-if-not #'symbolp '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
  (A B C D E F))

(check-for-bug :section17-legacy-61
  (remove-if (complement #'symbolp) '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
  (A B C D E F))

(check-for-bug :section17-legacy-65
  (count-if #'zerop '("foo" "" "bar" "" "" "baz" "quux") :key #'length)
  3)

;;; copy-seq

(check-for-bug :section17-legacy-71
  (let ((str "a string"))
    str)
  "a string")

(check-for-bug :section17-legacy-76
  (let ((str "a string"))
    (equalp str (copy-seq str)))
  t)

(check-for-bug :section17-legacy-81
  (let ((str "a string"))
    (eql str (copy-seq str)))
  nil)

;;; elt

(check-for-bug :section17-legacy-88
  (let ((str "a string"))
    (setq str (copy-seq "0123456789")))
  "0123456789")

(check-for-bug :section17-legacy-93
  (let ((str "a string"))
    (setq str (copy-seq "0123456789"))
    (elt str 6))
  #\6)

(check-for-bug :section17-legacy-99
  (let ((str "a string"))
    (setq str (copy-seq "0123456789"))
    (setf (elt str 0) #\#))
  #\#)

(check-for-bug :section17-legacy-105
  (let ((str "a string"))
    (setq str (copy-seq "0123456789"))
    (setf (elt str 0) #\#)
    str)
  "#123456789")

;;; fill

(check-for-bug :section17-legacy-114
  (fill (list 0 1 2 3 4 5) '(444))
  ((444) (444) (444) (444) (444) (444)))

(check-for-bug :section17-legacy-118
  (fill (copy-seq "01234") #\e :start 3)
  "012ee")

(check-for-bug :section17-legacy-122
  (setq x (vector 'a 'b 'c 'd 'e))
  #(A B C D E))

(check-for-bug :section17-legacy-126
  (fill x 'z :start 1 :end 3)
  #(A Z Z D E))

(check-for-bug :section17-legacy-130
  x
  #(A Z Z D E))

(check-for-bug :section17-legacy-134
  (fill x 'p)
  #(P P P P P))

(check-for-bug :section17-legacy-138
  x
  #(P P P P P))

;;; make-sequence

(check-for-bug :section17-legacy-144
  (make-sequence 'list 0)
  ())

(check-for-bug :section17-legacy-148
  (make-sequence 'string 26 :initial-element #\.)
  "..........................")

(check-for-bug :section17-legacy-152
  (make-sequence '(vector double-float) 2
                 :initial-element 1d0)
  #(1.0d0 1.0d0))

(check-for-bug :section17-legacy-157
  (make-sequence '(vector * 2) 3)
  TYPE-ERROR)

(check-for-bug :section17-legacy-161
  (make-sequence '(vector * 4) 3)
  TYPE-ERROR)

;;; subseq

(check-for-bug :section17-legacy-167
  (let ((str (copy-seq "012345")))
    str)
  "012345")

(check-for-bug :section17-legacy-172
  (let ((str (copy-seq "012345")))
    (subseq str 2))
  "2345")

(check-for-bug :section17-legacy-177
  (let ((str (copy-seq "012345")))
    (subseq str 3 5))
  "34")

(check-for-bug :section17-legacy-182
  (let ((str (copy-seq "012345")))
    (setf (subseq str 4) "abc"))
  "abc")

(check-for-bug :section17-legacy-187
  (let ((str (copy-seq "012345")))
    (setf (subseq str 4) "abc")
    str)
  "0123ab")

(check-for-bug :section17-legacy-193
  (let ((str (copy-seq "012345")))
    (setf (subseq str 4) "abc")
    (setf (subseq str 0 2) "A"))
  "A")

(check-for-bug :section17-legacy-199
  (let ((str (copy-seq "012345")))
    (setf (subseq str 4) "abc")
    (setf (subseq str 0 2) "A")
    str)
  "A123ab")

;;; map

(check-for-bug :section17-legacy-208
  (map 'string #'(lambda (x y)
                   (char "01234567890ABCDEF" (mod (+ x y) 16)))
       '(1 2 3 4)
       '(10 9 8 7))
  "AAAA")

(check-for-bug :section17-legacy-215
  (let ((seq (map 'list #'copy-seq
                  '("lower" "UPPER" "" "123"))))
    seq)
  ("lower" "UPPER" "" "123"))

(check-for-bug :section17-legacy-221
  (let ((seq (map 'list #'copy-seq
                  '("lower" "UPPER" "" "123"))))
    (map nil #'nstring-upcase seq))
  NIL)

(check-for-bug :section17-legacy-227
  (let ((seq (map 'list #'copy-seq
                  '("lower" "UPPER" "" "123"))))
    (map nil #'nstring-upcase seq)
    seq)
  ("LOWER" "UPPER" "" "123"))

(check-for-bug :section17-legacy-234
  (map 'list #'- '(1 2 3 4))
  (-1 -2 -3 -4))

(check-for-bug :section17-legacy-238
  (map 'string
       #'(lambda (x) (if (oddp x) #\1 #\0))
       '(1 2 3 4))
  "1010")

(check-for-bug :section17-legacy-244
  (map '(vector * 4) #'cons "abc" "de")
  TYPE-ERROR)

;;; map-into

(check-for-bug :section17-legacy-250
  (setq a (list 1 2 3 4) b (list 10 10 10 10))
  (10 10 10 10))

(check-for-bug :section17-legacy-254
  (map-into a #'+ a b)
  (11 12 13 14))

(check-for-bug :section17-legacy-258
  a
  (11 12 13 14))

(check-for-bug :section17-legacy-262
  b
  (10 10 10 10))

(check-for-bug :section17-legacy-266
  (setq k '(one two three))
  (ONE TWO THREE))

(check-for-bug :section17-legacy-270
  (map-into a #'cons k a)
  ((ONE . 11) (TWO . 12) (THREE . 13) 14))

;;; reduce

(check-for-bug :section17-legacy-276
  (reduce #'* '(1 2 3 4 5))
  120)

(check-for-bug :section17-legacy-280
  (reduce #'append '((1) (2)) :initial-value '(i n i t))
  (I N I T 1 2))

(check-for-bug :section17-legacy-284
  (reduce #'append '((1) (2)) :from-end t
          :initial-value '(i n i t))
  (1 2 I N I T))

(check-for-bug :section17-legacy-289
  (reduce #'- '(1 2 3 4))
  -8)

(check-for-bug :section17-legacy-293
  (reduce #'- '(1 2 3 4) :from-end t)
  -2)

(check-for-bug :section17-legacy-297
  (reduce #'+ '())
  0)

(check-for-bug :section17-legacy-301
  (reduce #'+ '(3))
  3)

(check-for-bug :section17-legacy-305
  (reduce #'+ '(foo))
  FOO)

(check-for-bug :section17-legacy-309
  (reduce #'list '(1 2 3 4))
  (((1 2) 3) 4))

(check-for-bug :section17-legacy-313
  (reduce #'list '(1 2 3 4) :from-end t)
  (1 (2 (3 4))))

(check-for-bug :section17-legacy-317
  (reduce #'list '(1 2 3 4) :initial-value 'foo)
  ((((foo 1) 2) 3) 4))

(check-for-bug :section17-legacy-321
  (reduce #'list '(1 2 3 4)
          :from-end t :initial-value 'foo)
  (1 (2 (3 (4 foo)))))

;;; count

(check-for-bug :section17-legacy-328
  (count #\a "how many A's are there in here?")
  2)

(check-for-bug :section17-legacy-332
  (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car)
  2)

(check-for-bug :section17-legacy-336
  (count-if #'upper-case-p "The Crying of Lot 49" :start 4)
  2)

;; length

(check-for-bug :section17-legacy-342
  (length "abc")
  3)

(check-for-bug :section17-legacy-346
  (setq str (make-array '(3) :element-type 'character
                        :initial-contents "abc"
                        :fill-pointer t))
  "abc")

(check-for-bug :section17-legacy-352
  (length str)
  3)

(check-for-bug :section17-legacy-356
  (setf (fill-pointer str) 2)
  2)

(check-for-bug :section17-legacy-360
  (length str)
  2)

;;; reverse

(check-for-bug :section17-legacy-366
  (setq str "abc")
  "abc")

(check-for-bug :section17-legacy-370
  (reverse str)
  "cba")

(check-for-bug :section17-legacy-374
  str
  "abc")

(check-for-bug :section17-legacy-378
  (setq str (copy-seq str))
  "abc")

(check-for-bug :section17-legacy-382
  (nreverse str)
  "cba")

(check-for-bug :section17-legacy-386
  str
  #+(or cmu sbcl clisp ecls) "cba"
  #-(or cmu sbcl clisp ecls) fill-this-in)

(check-for-bug :section17-legacy-391
  (let ((l (list 1 2 3)))
    l)
  (1 2 3))

(check-for-bug :section17-legacy-396
  (let ((l (list 1 2 3)))
    (nreverse l))
  (3 2 1))

(check-for-bug :section17-legacy-401
  (let ((l (list 1 2 3)))
    (nreverse l)
    l)
  #+(or cmu sbcl ecls) (1)
  #+clisp (3 2 1)
  #-(or cmu sbcl clisp ecls) fill-this-in)

;;; sort

(check-for-bug :section17-legacy-411
  (setq tester (copy-seq "lkjashd"))
  "lkjashd")

(check-for-bug :section17-legacy-415
  (sort tester #'char-lessp)
  "adhjkls")

(check-for-bug :section17-legacy-419
  (setq tester (list '(1 2 3) '(4 5 6) '(7 8 9)))
  ((1 2 3) (4 5 6) (7 8 9)))

(check-for-bug :section17-legacy-423
  (sort tester #'> :key #'car)
  ((7 8 9) (4 5 6) (1 2 3)))

(check-for-bug :section17-legacy-427
  (setq tester (list 1 2 3 4 5 6 7 8 9 0))
  (1 2 3 4 5 6 7 8 9 0))

(check-for-bug :section17-legacy-431
  (stable-sort tester #'(lambda (x y) (and (oddp x) (evenp y))))
  (1 3 5 7 9 2 4 6 8 0))

(check-for-bug :section17-legacy-435
  (sort (setq committee-data
              (vector (list (list "JonL" "White") "Iteration")
                      (list (list "Dick" "Waters") "Iteration")
                      (list (list "Dick" "Gabriel") "Objects")
                      (list (list "Kent" "Pitman") "Conditions")
                      (list (list "Gregor" "Kiczales") "Objects")
                      (list (list "David" "Moon") "Objects")
                      (list (list "Kathy" "Chapman") "Editorial")
                      (list (list "Larry" "Masinter") "Cleanup")
                      (list (list "Sandra" "Loosemore") "Compiler")))
        #'string-lessp :key #'cadar)
  #((("Kathy" "Chapman") "Editorial")
    (("Dick" "Gabriel") "Objects")
    (("Gregor" "Kiczales") "Objects")
    (("Sandra" "Loosemore") "Compiler")
    (("Larry" "Masinter") "Cleanup")
    (("David" "Moon") "Objects")
    (("Kent" "Pitman") "Conditions")
    (("Dick" "Waters") "Iteration")
    (("JonL" "White") "Iteration")))

;; Note that individual alphabetical order within `committees'
;; is preserved.

(check-for-bug :section17-legacy-460
  (setq committee-data
        (stable-sort committee-data #'string-lessp :key #'cadr))
  #((("Larry" "Masinter") "Cleanup")
    (("Sandra" "Loosemore") "Compiler")
    (("Kent" "Pitman") "Conditions")
    (("Kathy" "Chapman") "Editorial")
    (("Dick" "Waters") "Iteration")
    (("JonL" "White") "Iteration")
    (("Dick" "Gabriel") "Objects")
    (("Gregor" "Kiczales") "Objects")
    (("David" "Moon") "Objects")))

;;; find

(check-for-bug :section17-legacy-475
  (find #\d "here are some letters that can be looked at" :test #'char>)
  #\Space)

(check-for-bug :section17-legacy-479
  (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t)
  3)

(check-for-bug :section17-legacy-483
  (find-if-not #'complexp
               '#(3.5 2 #C(1.0 0.0) #C(0.0 1.0))
               :start 2)
  NIL)


;;; position

(check-for-bug :section17-legacy-492
  (position #\a "baobab" :from-end t)
  4)

(check-for-bug :section17-legacy-496
  (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car)
  2)

(check-for-bug :section17-legacy-500
  (position 595 '())
  NIL)

(check-for-bug :section17-legacy-504
  (position-if-not #'integerp '(1 2 3 4 5.0))
  4)

;;; search

(check-for-bug :section17-legacy-510
  (search "dog" "it's a dog's life")
  7)

(check-for-bug :section17-legacy-514
  (search '(0 1) '(2 4 6 1 3 5) :key #'oddp)
  2)

;;; mismatch

(check-for-bug :section17-legacy-520
  (mismatch "abcd" "ABCDE" :test #'char-equal)
  4)

(check-for-bug :section17-legacy-524
  (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t)
  3)

(check-for-bug :section17-legacy-528
  (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp)
  NIL)

(check-for-bug :section17-legacy-532
  (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4)
  NIL)

;;; replace

(check-for-bug :section17-legacy-538
  (replace (copy-seq "abcdefghij")
           "0123456789" :start1 4 :end1 7 :start2 4)
  "abcd456hij")

(check-for-bug :section17-legacy-543
  (let ((lst (copy-seq "012345678")))
    lst)
  "012345678")

(check-for-bug :section17-legacy-548
  (let ((lst (copy-seq "012345678")))
    (replace lst lst :start1 2 :start2 0))
  "010123456")

(check-for-bug :section17-legacy-553
  (let ((lst (copy-seq "012345678")))
    (replace lst lst :start1 2 :start2 0)
    lst)
  "010123456")

;;; substitute

(check-for-bug :section17-legacy-561
  (substitute #\. #\SPACE "0 2 4 6")
  "0.2.4.6")

(check-for-bug :section17-legacy-565
  (substitute 9 4 '(1 2 4 1 3 4 5))
  (1 2 9 1 3 9 5))

(check-for-bug :section17-legacy-569
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5))

(check-for-bug :section17-legacy-573
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(check-for-bug :section17-legacy-577
  (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5))

(check-for-bug :section17-legacy-581
  (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  ((1) (2) (3) 0))

(check-for-bug :section17-legacy-585
  (substitute-if 9 #'oddp '(1 2 4 1 3 4 5))
  (9 2 4 9 9 4 9))

(check-for-bug :section17-legacy-589
  (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(check-for-bug :section17-legacy-593
  (setq some-things (list 'a 'car 'b 'cdr 'c))
  (A CAR B CDR C))

(check-for-bug :section17-legacy-597
  (nsubstitute-if "function was here" #'fboundp some-things
                  :count 1 :from-end t)
  (A CAR B "function was here" C))

(check-for-bug :section17-legacy-602
  some-things
  (A CAR B "function was here" C))

(check-for-bug :section17-legacy-606
  (setq alpha-tester (copy-seq "ab "))
  "ab ")

(check-for-bug :section17-legacy-610
  (nsubstitute-if-not #\z #'alpha-char-p alpha-tester)
  "abz")

(check-for-bug :section17-legacy-614
  alpha-tester
  "abz")

;;; concatenate

(check-for-bug :section17-legacy-620
  (concatenate 'string "all" " " "together" " " "now")
  "all together now")

(check-for-bug :section17-legacy-624
  (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
  (#\A #\B #\C D E F 1 2 3 1 0 1 1))

(check-for-bug :section17-legacy-628
  (concatenate 'list)
  NIL)

(check-for-bug :section17-legacy-632
  (concatenate '(vector * 2) "a" "bc")
  TYPE-ERROR)

;;; merge

(check-for-bug :section17-legacy-638
  (setq test1 (list 1 3 4 6 7))
  (1 3 4 6 7))

(check-for-bug :section17-legacy-642
  (setq test2 (list 2 5 8))
  (2 5 8))

(check-for-bug :section17-legacy-646
  (merge 'list test1 test2 #'<)
  (1 2 3 4 5 6 7 8))

(check-for-bug :section17-legacy-650
  (setq test1 (copy-seq "BOY"))
  "BOY")

(check-for-bug :section17-legacy-654
  (setq test2 (copy-seq "nosy"))
  "nosy")

(check-for-bug :section17-legacy-658
  (merge 'string test1 test2 #'char-lessp)
  "BnOosYy")

(check-for-bug :section17-legacy-662
  (setq test1 (vector '(red . 1) '(blue . 4)))
  #((RED . 1) (BLUE . 4)))

(check-for-bug :section17-legacy-666
  (setq test2 (vector '(yellow . 2) '(green . 7)))
  #((YELLOW . 2) (GREEN . 7)))

(check-for-bug :section17-legacy-670
  (merge 'vector test1 test2 #'< :key #'cdr)
  #((RED . 1) (YELLOW . 2) (BLUE . 4) (GREEN . 7)))

(check-for-bug :section17-legacy-674
  (merge '(vector * 4) '(1 5) '(2 4 6) #'<)
  TYPE-ERROR)


;;; remove

(check-for-bug :section17-legacy-681
  (remove 4 '(1 3 4 5 9))
  (1 3 5 9))

(check-for-bug :section17-legacy-685
  (remove 4 '(1 2 4 1 3 4 5))
  (1 2 1 3 5))

(check-for-bug :section17-legacy-689
  (remove 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 1 3 4 5))

(check-for-bug :section17-legacy-693
  (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))

(check-for-bug :section17-legacy-697
  (remove 3 '(1 2 4 1 3 4 5) :test #'>)
  (4 3 4 5))

(check-for-bug :section17-legacy-701
  (setq lst '(list of four elements))
  (LIST OF FOUR ELEMENTS))

(check-for-bug :section17-legacy-705
  (setq lst2 (copy-seq lst))
  (LIST OF FOUR ELEMENTS))

(check-for-bug :section17-legacy-709
  (setq lst3 (delete 'four lst))
  (LIST OF ELEMENTS))

(check-for-bug :section17-legacy-713
  (equal lst lst2)
  nil)

(check-for-bug :section17-legacy-717
  (remove-if #'oddp '(1 2 4 1 3 4 5))
  (2 4 4))

(check-for-bug :section17-legacy-721
  (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))

(check-for-bug :section17-legacy-725
  (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
  (1 2 3 4 5 6 8))

(check-for-bug :section17-legacy-729
  (setq tester (list 1 2 4 1 3 4 5))
  (1 2 4 1 3 4 5))

(check-for-bug :section17-legacy-733
  (delete 4 tester)
  (1 2 1 3 5))

(check-for-bug :section17-legacy-737
  (setq tester (list 1 2 4 1 3 4 5))
  (1 2 4 1 3 4 5))

(check-for-bug :section17-legacy-741
  (delete 4 tester :count 1)
  (1 2 1 3 4 5))

(check-for-bug :section17-legacy-745
  (setq tester (list 1 2 4 1 3 4 5))
  (1 2 4 1 3 4 5))

(check-for-bug :section17-legacy-749
  (delete 4 tester :count 1 :from-end t)
  (1 2 4 1 3 5))

(check-for-bug :section17-legacy-753
  (setq tester (list 1 2 4 1 3 4 5))
  (1 2 4 1 3 4 5))

(check-for-bug :section17-legacy-757
  (delete 3 tester :test #'>)
  (4 3 4 5))

(check-for-bug :section17-legacy-761
  (setq tester (list 1 2 4 1 3 4 5))
  (1 2 4 1 3 4 5))

(check-for-bug :section17-legacy-765
  (delete-if #'oddp tester)
  (2 4 4))

(check-for-bug :section17-legacy-769
  (setq tester (list 1 2 4 1 3 4 5))
  (1 2 4 1 3 4 5))

(check-for-bug :section17-legacy-773
  (delete-if #'evenp tester :count 1 :from-end t)
  (1 2 4 1 3 5))

(check-for-bug :section17-legacy-777
  (setq tester (list 1 2 3 4 5 6))
  (1 2 3 4 5 6))

(check-for-bug :section17-legacy-781
  (delete-if #'evenp tester)
  (1 3 5))

(check-for-bug :section17-legacy-785
  tester
  #+(or cmu sbcl clisp ecls) (1 3 5)
  #-(or cmu sbcl clisp ecls)  fill-this-in)

(check-for-bug :section17-legacy-790
  (setq foo (list 'a 'b 'c))
  (A B C))

(check-for-bug :section17-legacy-794
  (setq bar (cdr foo))
  (B C))

(check-for-bug :section17-legacy-798
  (setq foo (delete 'b foo))
  (A C))

(check-for-bug :section17-legacy-802
  bar
  #+(or cmu sbcl clisp ecls) (B C)
  #-(or cmu sbcl clisp ecls)  fill-this-in)
					; ((C))) or ...

(check-for-bug :section17-legacy-808
  (eq (cdr foo) (car bar))
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)
					; T or ...


;;; remove-duplicates

(check-for-bug :section17-legacy-817
  (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t)
  "aBcD")

(check-for-bug :section17-legacy-821
  (remove-duplicates '(a b c b d d e))
  (A C B D E))

(check-for-bug :section17-legacy-825
  (remove-duplicates '(a b c b d d e) :from-end t)
  (A B C D E))

(check-for-bug :section17-legacy-829
  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
                     :test #'char-equal :key #'cadr)
  ((BAR #\%) (BAZ #\A)))

(check-for-bug :section17-legacy-834
  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
                     :test #'char-equal :key #'cadr :from-end t)
  ((FOO #\a) (BAR #\%)))

(check-for-bug :section17-legacy-839
  (setq tester (list 0 1 2 3 4 5 6))
  (0 1 2 3 4 5 6))

(check-for-bug :section17-legacy-843
  (delete-duplicates tester :key #'oddp :start 1 :end 6)
  (0 4 5 6))

