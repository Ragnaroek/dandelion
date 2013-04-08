;;; based on v1.7 -*- mode: lisp -*-
(in-package :cl-user)


(check-for-bug :characters-legacy-5
  char-code-limit
  #+xcl 128
  #+(or (and clisp (not unicode)) akcl sbcl cmu ecls) 256
  #+allegro 65536
  #+(and clisp unicode) 1114112
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :characters-legacy-12
  (standard-char-p #\a)
  t)

(check-for-bug :characters-legacy-16
  (standard-char-p #\$)
  t)

(check-for-bug :characters-legacy-20
  (standard-char-p #\.)
  t)

(check-for-bug :characters-legacy-24
  (standard-char-p #\A)
  t)

(check-for-bug :characters-legacy-28
  (standard-char-p 1)
  type-error)

(check-for-bug :characters-legacy-32
  (standard-char-p #\\)
  t)

(check-for-bug :characters-legacy-36
  (standard-char-p #\5)
  t)

(check-for-bug :characters-legacy-40
  (standard-char-p #\))
  t)

(check-for-bug :characters-legacy-44
  (standard-char-p #\%)
  t)

(check-for-bug :characters-legacy-48
  (standard-char-p #\Backspace)
  #+xcl t
  #-xcl nil)

(check-for-bug :characters-legacy-53
  (standard-char-p #\Page)
  #+xcl t
  #-xcl nil)

(check-for-bug :characters-legacy-58
  (standard-char-p #\Return)
  #+xcl t
  #-xcl nil)

(check-for-bug :characters-legacy-63
  (graphic-char-p #\a)
  t)

(check-for-bug :characters-legacy-67
  (graphic-char-p #\$)
  t)

(check-for-bug :characters-legacy-71
  (graphic-char-p #\.)
  t)

(check-for-bug :characters-legacy-75
  (graphic-char-p #\A)
  t)

(check-for-bug :characters-legacy-79
  (graphic-char-p 1)
  type-error)

(check-for-bug :characters-legacy-83
  (graphic-char-p #\\)
  t)

(check-for-bug :characters-legacy-87
  (graphic-char-p #\5)
  t)

(check-for-bug :characters-legacy-91
  (graphic-char-p #\))
  t)

(check-for-bug :characters-legacy-95
  (graphic-char-p #\%)
  t)

(check-for-bug :characters-legacy-99
  (graphic-char-p #\Backspace)
  nil)

(check-for-bug :characters-legacy-103
  (graphic-char-p #\Page)
  nil)

(check-for-bug :characters-legacy-107
  (graphic-char-p #\Return)
  nil)

(check-for-bug :characters-legacy-111
  (characterp
   #\a)
  t)

(check-for-bug :characters-legacy-116
  (characterp
   #\$)
  t)

(check-for-bug :characters-legacy-121
  (characterp
   #\.)
  t)

(check-for-bug :characters-legacy-126
  (characterp
   #\A)
  t)

(check-for-bug :characters-legacy-131
  (characterp
   #\\)
  t)

(check-for-bug :characters-legacy-136
  (characterp
   #\5)
  t)

(check-for-bug :characters-legacy-141
  (characterp
   #\))
  t)

(check-for-bug :characters-legacy-146
  (characterp
   #\%)
  t)

(check-for-bug :characters-legacy-151
  (characterp
   #\Backspace)
  t)

(check-for-bug :characters-legacy-156
  (characterp
   #\Page)
  t)

(check-for-bug :characters-legacy-161
  (characterp
   #\Return)
  t)

(check-for-bug :characters-legacy-166
  (alpha-char-p #\a)
  t)

(check-for-bug :characters-legacy-170
  (alpha-char-p #\$)
  nil)

(check-for-bug :characters-legacy-174
  (alpha-char-p #\.)
  nil)

(check-for-bug :characters-legacy-178
  (alpha-char-p #\A)
  t)

(check-for-bug :characters-legacy-182
  (alpha-char-p 1)
  type-error)

(check-for-bug :characters-legacy-186
  (alpha-char-p #\\)
  nil)

(check-for-bug :characters-legacy-190
  (alpha-char-p #\5)
  nil)

(check-for-bug :characters-legacy-194
  (alpha-char-p #\))
  nil)

(check-for-bug :characters-legacy-198
  (alpha-char-p #\%)
  nil)

(check-for-bug :characters-legacy-202
  (alpha-char-p #\Backspace)
  nil)

(check-for-bug :characters-legacy-206
  (alpha-char-p #\Page)
  nil)

(check-for-bug :characters-legacy-210
  (alpha-char-p #\Return)
  nil)

(check-for-bug :characters-legacy-214
  (upper-case-p #\a)
  nil)

(check-for-bug :characters-legacy-218
  (upper-case-p #\$)
  nil)

(check-for-bug :characters-legacy-222
  (upper-case-p #\.)
  nil)

(check-for-bug :characters-legacy-226
  (upper-case-p #\A)
  t)

(check-for-bug :characters-legacy-230
  (upper-case-p 1)
  type-error)

(check-for-bug :characters-legacy-234
  (upper-case-p #\\)
  nil)

(check-for-bug :characters-legacy-238
  (upper-case-p #\5)
  nil)

(check-for-bug :characters-legacy-242
  (upper-case-p #\))
  nil)

(check-for-bug :characters-legacy-246
  (upper-case-p #\%)
  nil)

(check-for-bug :characters-legacy-250
  (upper-case-p #\Backspace)
  nil)

(check-for-bug :characters-legacy-254
  (upper-case-p #\Page)
  nil)

(check-for-bug :characters-legacy-258
  (upper-case-p #\Return)
  nil)

(check-for-bug :characters-legacy-262
  (lower-case-p #\a)
  t)

(check-for-bug :characters-legacy-266
  (lower-case-p #\$)
  nil)

(check-for-bug :characters-legacy-270
  (lower-case-p #\.)
  nil)

(check-for-bug :characters-legacy-274
  (lower-case-p #\A)
  nil)

(check-for-bug :characters-legacy-278
  (lower-case-p 1)
  type-error)

(check-for-bug :characters-legacy-282
  (lower-case-p #\\)
  nil)

(check-for-bug :characters-legacy-286
  (lower-case-p #\5)
  nil)

(check-for-bug :characters-legacy-290
  (lower-case-p #\))
  nil)

(check-for-bug :characters-legacy-294
  (lower-case-p #\%)
  nil)

(check-for-bug :characters-legacy-298
  (lower-case-p #\Backspace)
  nil)

(check-for-bug :characters-legacy-302
  (lower-case-p #\Page)
  nil)

(check-for-bug :characters-legacy-306
  (lower-case-p #\Return)
  nil)

(check-for-bug :characters-legacy-310
  (both-case-p #\a)
  t)

(check-for-bug :characters-legacy-314
  (both-case-p #\$)
  nil)

(check-for-bug :characters-legacy-318
  (both-case-p #\.)
  nil)

(check-for-bug :characters-legacy-322
  (both-case-p #\A)
  t)

(check-for-bug :characters-legacy-326
  (both-case-p 1)
  type-error)

(check-for-bug :characters-legacy-330
  (both-case-p #\\)
  nil)

(check-for-bug :characters-legacy-334
  (both-case-p #\5)
  nil)

(check-for-bug :characters-legacy-338
  (both-case-p #\))
  nil)

(check-for-bug :characters-legacy-342
  (both-case-p #\%)
  nil)

(check-for-bug :characters-legacy-346
  (both-case-p #\Backspace)
  nil)

(check-for-bug :characters-legacy-350
  (both-case-p #\Page)
  nil)

(check-for-bug :characters-legacy-354
  (both-case-p #\Return)
  nil)

(check-for-bug :characters-legacy-358
  (digit-char-p #\a)
  nil)

(check-for-bug :characters-legacy-362
  (digit-char-p #\$)
  nil)

(check-for-bug :characters-legacy-366
  (digit-char-p #\.)
  nil)

(check-for-bug :characters-legacy-370
  (digit-char-p #\A)
  nil)

(check-for-bug :characters-legacy-374
  (digit-char-p 1)
  type-error)

(check-for-bug :characters-legacy-378
  (digit-char-p #\\)
  nil)

(check-for-bug :characters-legacy-382
  (digit-char-p #\5)
  5)

(check-for-bug :characters-legacy-386
  (digit-char-p #\))
  nil)

(check-for-bug :characters-legacy-390
  (digit-char-p #\%)
  nil)

(check-for-bug :characters-legacy-394
  (digit-char-p #\Backspace)
  nil)

(check-for-bug :characters-legacy-398
  (digit-char-p #\Page)
  nil)

(check-for-bug :characters-legacy-402
  (digit-char-p #\Return)
  nil)

(check-for-bug :characters-legacy-406
  (digit-char-p #\5 4)
  nil)

(check-for-bug :characters-legacy-410
  (digit-char-p #\5 8)
  5)

(check-for-bug :characters-legacy-414
  (digit-char-p #\E 16)
  14)

(check-for-bug :characters-legacy-418
  (digit-char-p #\R 35)
  27)

(check-for-bug :characters-legacy-422
  (digit-char-p #\5 4)
  nil)

(check-for-bug :characters-legacy-426
  (digit-char-p #\5 5)
  nil)

(check-for-bug :characters-legacy-430
  (digit-char-p #\5 6)
  5)

(check-for-bug :characters-legacy-434
  (digit-char-p #\1 2)
  1)

(check-for-bug :characters-legacy-438
  (alphanumericp #\a)
  t)

(check-for-bug :characters-legacy-442
  (alphanumericp #\$)
  nil)

(check-for-bug :characters-legacy-446
  (alphanumericp #\.)
  nil)

(check-for-bug :characters-legacy-450
  (alphanumericp #\A)
  t)

(check-for-bug :characters-legacy-454
  (alphanumericp 1)
  type-error)

(check-for-bug :characters-legacy-458
  (alphanumericp #\\)
  nil)

(check-for-bug :characters-legacy-462
  (alphanumericp #\5)
  t)

(check-for-bug :characters-legacy-466
  (alphanumericp #\))
  nil)

(check-for-bug :characters-legacy-470
  (alphanumericp #\%)
  nil)

(check-for-bug :characters-legacy-474
  (alphanumericp #\Backspace)
  nil)

(check-for-bug :characters-legacy-478
  (alphanumericp #\Page)
  nil)

(check-for-bug :characters-legacy-482
  (alphanumericp #\Return)
  nil)

(check-for-bug :characters-legacy-486
  (alphanumericp #\5 4)
  error)

(check-for-bug :characters-legacy-490
  (alphanumericp #\5 8)
  error)

(check-for-bug :characters-legacy-494
  (alphanumericp #\E 16)
  error)

(check-for-bug :characters-legacy-498
  (alphanumericp #\R 35)
  error)

(check-for-bug :characters-legacy-502
  (char= #\d #\d)
  t)

(check-for-bug :characters-legacy-506
  (char/= #\d #\d)
  nil)

(check-for-bug :characters-legacy-510
  (char= #\d #\x)
  nil)

(check-for-bug :characters-legacy-514
  (char/= #\d #\x)
  t)

(check-for-bug :characters-legacy-518
  (char= #\d #\D)
  nil)

(check-for-bug :characters-legacy-522
  (char/= #\d #\D)
  t)

(check-for-bug :characters-legacy-526
  (char= #\d #\d #\d #\d)
  t)

(check-for-bug :characters-legacy-530
  (char/= #\d #\d #\d #\d)
  nil)

(check-for-bug :characters-legacy-534
  (char= #\d #\d #\x #\d)
  nil)

(check-for-bug :characters-legacy-538
  (char/= #\d #\d #\x #\d)
  nil)

(check-for-bug :characters-legacy-542
  (char= #\d #\y #\x #\c)
  nil)

(check-for-bug :characters-legacy-546
  (char/= #\d #\y #\x #\c)
  t)

(check-for-bug :characters-legacy-550
  (char= #\d #\c #\d)
  nil)

(check-for-bug :characters-legacy-554
  (char/= #\d #\c #\d)
  nil)

(check-for-bug :characters-legacy-558
  (char< #\d #\x)
  t)

(check-for-bug :characters-legacy-562
  (char<= #\d #\x)
  t)

(check-for-bug :characters-legacy-566
  (char< #\d #\d)
  nil)

(check-for-bug :characters-legacy-570
  (char<= #\d #\d)
  t)

(check-for-bug :characters-legacy-574
  (char< #\a #\e #\y #\z)
  t)

(check-for-bug :characters-legacy-578
  (char<= #\a #\e #\y #\z)
  t)

(check-for-bug :characters-legacy-582
  (char< #\a #\e #\e #\y)
  nil)

(check-for-bug :characters-legacy-586
  (char<= #\a #\e #\e #\y)
  t)

(check-for-bug :characters-legacy-590
  (char> #\e #\d)
  t)

(check-for-bug :characters-legacy-594
  (char>= #\e #\d)
  t)

(check-for-bug :characters-legacy-598
  (char> #\d #\c #\b #\a)
  t)

(check-for-bug :characters-legacy-602
  (char>= #\d #\c #\b #\a)
  t)

(check-for-bug :characters-legacy-606
  (char> #\d #\d #\b #\a)
  nil)

(check-for-bug :characters-legacy-610
  (char>= #\d #\d #\b #\a)
  t)

(check-for-bug :characters-legacy-614
  (char> #\e #\d #\b #\c #\a)
  nil)

(check-for-bug :characters-legacy-618
  (char>= #\e #\d #\b #\c #\a)
  nil)

(check-for-bug :characters-legacy-622
  (char> #\z #\A)
  t)

(check-for-bug :characters-legacy-626
  (char> #\Z #\a)
  nil)

(check-for-bug :characters-legacy-630
  (char< #\9 #\a)
  t)

(check-for-bug :characters-legacy-634
  (char> #\9 #\a)
  nil)

(check-for-bug :characters-legacy-638
  (char> #\z #\0)
  t)

(check-for-bug :characters-legacy-642
  (char< #\z #\0)
  nil)

(check-for-bug :characters-legacy-646
  (char-equal #\d #\d)
  t)

(check-for-bug :characters-legacy-650
  (char-not-equal #\d #\d)
  nil)

(check-for-bug :characters-legacy-654
  (char-equal #\d #\x)
  nil)

(check-for-bug :characters-legacy-658
  (char-not-equal #\d #\x)
  t)

(check-for-bug :characters-legacy-662
  (char-equal #\d #\D)
  t)

(check-for-bug :characters-legacy-666
  (char-not-equal #\d #\D)
  nil)

(check-for-bug :characters-legacy-670
  (char-equal #\d #\d #\d #\d)
  t)

(check-for-bug :characters-legacy-674
  (char-not-equal #\d #\d #\d #\d)
  nil)

(check-for-bug :characters-legacy-678
  (char-equal #\d #\d #\x #\d)
  nil)

(check-for-bug :characters-legacy-682
  (char-not-equal #\d #\d #\x #\d)
  nil)

(check-for-bug :characters-legacy-686
  (char-equal #\d #\y #\x #\c)
  nil)

(check-for-bug :characters-legacy-690
  (char-not-equal #\d #\y #\x #\c)
  t)

(check-for-bug :characters-legacy-694
  (char-equal #\d #\c #\d)
  nil)

(check-for-bug :characters-legacy-698
  (char-not-equal #\d #\c #\d)
  nil)

(check-for-bug :characters-legacy-702
  (char-lessp #\d #\x)
  t)

(check-for-bug :characters-legacy-706
  (char-not-greaterp #\d #\x)
  t)

(check-for-bug :characters-legacy-710
  (char-lessp #\d #\d)
  nil)

(check-for-bug :characters-legacy-714
  (char-not-greaterp #\d #\d)
  t)

(check-for-bug :characters-legacy-718
  (char-lessp #\a #\e #\y #\z)
  t)

(check-for-bug :characters-legacy-722
  (char-not-greaterp #\a #\e #\y #\z)
  t)

(check-for-bug :characters-legacy-726
  (char-lessp #\a #\e #\e #\y)
  nil)

(check-for-bug :characters-legacy-730
  (char-not-greaterp #\a #\e #\e #\y)
  t)

(check-for-bug :characters-legacy-734
  (char-greaterp #\e #\d)
  t)

(check-for-bug :characters-legacy-738
  (char-not-lessp #\e #\d)
  t)

(check-for-bug :characters-legacy-742
  (char-greaterp #\d #\c #\b #\a)
  t)

(check-for-bug :characters-legacy-746
  (char-not-lessp #\d #\c #\b #\a)
  t)

(check-for-bug :characters-legacy-750
  (char-greaterp #\d #\d #\b #\a)
  nil)

(check-for-bug :characters-legacy-754
  (char-not-lessp #\d #\d #\b #\a)
  t)

(check-for-bug :characters-legacy-758
  (char-greaterp #\e #\d #\b #\c #\a)
  nil)

(check-for-bug :characters-legacy-762
  (char-not-lessp #\e #\d #\b #\c #\a)
  nil)

(check-for-bug :characters-legacy-766
  (char-greaterp #\z #\A)
  t)

(check-for-bug :characters-legacy-770
  (char-greaterp #\Z #\a)
  t)

(check-for-bug :characters-legacy-774
  (char-lessp #\9 #\a)
  t)

(check-for-bug :characters-legacy-778
  (char-greaterp #\9 #\a)
  nil)

(check-for-bug :characters-legacy-782
  (char-greaterp #\z #\0)
  t)

(check-for-bug :characters-legacy-786
  (char-lessp #\z #\0)
  nil)

(check-for-bug :characters-legacy-790
  (char-equal #\A #\a)
  t)

(check-for-bug :characters-legacy-794
  (char-upcase #\a)
  #\A)

(check-for-bug :characters-legacy-798
  (char-upcase #\A)
  #\A)

(check-for-bug :characters-legacy-802
  (char-upcase #\5)
  #\5)

(check-for-bug :characters-legacy-806
  (char-upcase #\;)
  #\;)

(check-for-bug :characters-legacy-810
  (char-upcase #\=)
  #\=)

(check-for-bug :characters-legacy-814
  (char= (char-downcase (char-upcase #\x)) #\x)
  t)

(check-for-bug :characters-legacy-818
  (char-downcase #\A)
  #\a)

(check-for-bug :characters-legacy-822
  (char-downcase #\a)
  #\a)

(check-for-bug :characters-legacy-826
  (char-downcase #\%)
  #\%)

(check-for-bug :characters-legacy-830
  (char-downcase #\+)
  #\+)

(check-for-bug :characters-legacy-834
  (char-downcase #\-)
  #\-)

(check-for-bug :characters-legacy-838
  (char= (char-upcase (char-downcase #\X)) #\X)
  t)

(check-for-bug :characters-legacy-842
  (digit-char 7)
  #\7)

(check-for-bug :characters-legacy-846
  (digit-char 12)
  nil)

(check-for-bug :characters-legacy-850
  (digit-char 'a)
  error)

(check-for-bug :characters-legacy-854
  (digit-char 12 16)
  #\C)

(check-for-bug :characters-legacy-858
  (digit-char 6 2)
  nil)

(check-for-bug :characters-legacy-862
  (digit-char 1 2)
  #\1)

;; evan though char-*-bit are not in the ANSI CL standard,
;; they may be present as an extension

;; (check-for-bug :characters-legacy-869
;;  char-control-bit
;;  error)

;; (check-for-bug :characters-legacy-873
;;  char-meta-bit
;;  error)

;; (check-for-bug :characters-legacy-877
;;  char-super-bit
;;  error)

;; (check-for-bug :characters-legacy-881
;;  char-hyper-bit
;;  error)

(check-for-bug :characters-legacy-885
  (char-name #\Space)
  "Space")

(check-for-bug :characters-legacy-889
  (char-name #\Newline)
  #-cmu
  "Newline"
  #+cmu
  "Linefeed")

(check-for-bug :characters-without-good-names
  (let ((wrong-codes nil))
    (dotimes
        (code char-code-limit)
      (let ((c (code-char code)))
        (unless (eql c (name-char (char-name c)))
          (push code wrong-codes))))
    wrong-codes)
  NIL)
