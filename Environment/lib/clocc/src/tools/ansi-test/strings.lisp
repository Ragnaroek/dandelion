;;; based on v1.6 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :strings-legacy-4
  (char  "abcdef-dg1ndh" 0)
  #\a)

(check-for-bug :strings-legacy-8
  (char  "abcdef-dg1ndh" 1)
  #\b)

(check-for-bug :strings-legacy-12
  (char  "abcdef-dg1ndh" 6)
  #\-)

(check-for-bug :strings-legacy-16
  (char  "abcdef-dg1ndh" 20)
  error)

(check-for-bug :strings-legacy-20
  (char  "abcdef-dg1ndh")
  program-error)

(check-for-bug :strings-legacy-24
  (char  "abcdef-dg1ndh" -3)
  error)

(check-for-bug :strings-legacy-28
  (char)
  program-error)

(check-for-bug :strings-legacy-32
  (char 2)
  program-error)

(check-for-bug :strings-legacy-36
  (char  "abcde" 2 4)
  error)

(check-for-bug :strings-legacy-40
  (char 'a 0)
  #+xcl #\a
  #-xcl error)

(check-for-bug :strings-legacy-45
  (char 'anna 0)
  #+xcl #\a
  #-xcl error)

(check-for-bug :strings-legacy-50
  (schar 'a 0)
  #+xcl #\a
  #-xcl error)

(check-for-bug :strings-legacy-55
  (schar 'anna 0)
  #+xcl #\a
  #-xcl error)

(check-for-bug :strings-legacy-60
  (schar  "abcdef-dg1ndh" 0)
  #\a)

(check-for-bug :strings-legacy-64
  (schar  "abcdef-dg1ndh" 1)
  #\b)

(check-for-bug :strings-legacy-68
  (schar  "abcdef-dg1ndh" 6)
  #\-)

(check-for-bug :strings-legacy-72
  (schar  "abcdef-dg1ndh" 20)
  error)

(check-for-bug :strings-legacy-76
  (schar  "abcdef-dg1ndh")
  program-error)

(check-for-bug :strings-legacy-80
  (schar  "abcdef-dg1ndh" -3)
  error)

(check-for-bug :strings-legacy-84
  (schar 2)
  program-error)

(check-for-bug :strings-legacy-88
  (schar 2 2)
  error)

(check-for-bug :strings-legacy-92
  (schar  "abcde" 2 4)
  program-error)

(check-for-bug :strings-legacy-96
  (string=  "foo" "foo")
  t)

(check-for-bug :strings-legacy-100
  (string=  "foo" "Foo")
  nil)

(check-for-bug :strings-legacy-104
  (string=  "foo" "FOO")
  nil)

(check-for-bug :strings-legacy-108
  (string=  "foo" "bar")
  nil)

(check-for-bug :strings-legacy-112
  (string=  "together" "frog" :start1 1 :end1 3 :start2 2)
  t)

(check-for-bug :strings-legacy-116
  (string=  "abcdef" "defghi" :start1 3 :end2 3)
  t)

(check-for-bug :strings-legacy-120
  (string=  "abcdefghi" "uvdefmgnj" :start1 3 :end1 6 :start2 2 :end2
            5)
  t)

(check-for-bug :strings-legacy-125
  (string=  "abcdefg" "abcdefg" :end2 4)
  nil)

(check-for-bug :strings-legacy-129
  (string=  "abcdef" "abcdef" :start1 1 :end1 4 :start2 4 :end2 1)
  error)

(check-for-bug :strings-legacy-133
  (string-equal  "foo" "foo")
  t)

(check-for-bug :strings-legacy-137
  (string-equal  "foo" "Foo")
  t)

(check-for-bug :strings-legacy-141
  (string-equal  "foo" "FOO")
  t)

(check-for-bug :strings-legacy-145
  (string-equal  "foo" "bar")
  nil)

(check-for-bug :strings-legacy-149
  (string-equal  "absDEfg-HijM1#r" "udEFG-hIfvd" :start1 3 :end1 10 :start2
                 1 :end2
                 8)
  t)

(check-for-bug :strings-legacy-155
  (string-equal  "ABCdefg" "abcDEFG")
  t)

(check-for-bug :strings-legacy-159
  (string-equal  "ABCdefg" "abcDEFG" :start1 3)
  nil)

(check-for-bug :strings-legacy-163
  (string-equal  "AbCdEf" "aBcDeF" :start1 5 :end1 3)
  error)

(check-for-bug :strings-legacy-167
  (string<  "" "abcdefgh")
  0)

(check-for-bug :strings-legacy-171
  (string<  "a" "abcdefgh")
  1)

(check-for-bug :strings-legacy-175
  (string<  "abc" "abcdefgh")
  3)

(check-for-bug :strings-legacy-179
  (string<  "cabc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-183
  (string<  "abcdefgh" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-187
  (string<  "xyzabc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-191
  (string<  "abc" "xyzabcdefgh")
  0)

(check-for-bug :strings-legacy-195
  (string<  "abcdefgh" "abcdefgh" :end1 4)
  4)

(check-for-bug :strings-legacy-199
  (string<  "xyzabc" "abcdefgh" :start1 3)
  6)

(check-for-bug :strings-legacy-203
  (string<  "abc" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-207
  (string<  "abc" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-211
  (string<  "abc" "xyzabcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-215
  (string<  "abcdefgh" "")
  nil)

(check-for-bug :strings-legacy-219
  (string<  "abcdefgh" "a")
  nil)

(check-for-bug :strings-legacy-223
  (string<  "abcdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-227
  (string<  "abcdefgh" "cabc")
  0)

(check-for-bug :strings-legacy-231
  (string<  "abcdefgh" "xyzabc")
  0)

(check-for-bug :strings-legacy-235
  (string<  "xyzabcdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-239
  (string<  "abcdefgh" "abcdefgh" :end2 4)
  nil)

(check-for-bug :strings-legacy-243
  (string<  "xyzabc" "abcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-247
  (string<  "abc" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-251
  (string<  "abc" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-255
  (string<  "abc" "xyzabcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-259
  (string<  "abcdef" "bcdefgh")
  0)

(check-for-bug :strings-legacy-263
  (string<  "abcdef" "abcdefgh" :start2 2)
  0)

(check-for-bug :strings-legacy-267
  (string<  "abcdef" "bngdabcdef" :start2 9 :end2 5)
  error)

(check-for-bug :strings-legacy-271
  (string>  "" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-275
  (string>  "a" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-279
  (string>  "abc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-283
  (string>  "cabc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-287
  (string>  "abcdefgh" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-291
  (string>  "xyzabc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-295
  (string>  "abc" "xyzabcdefgh")
  nil)

(check-for-bug :strings-legacy-299
  (string>  "abcdefgh" "abcdefgh" :end1 4)
  nil)

(check-for-bug :strings-legacy-303
  (string>  "xyzabc" "abcdefgh" :start1 3)
  nil)

(check-for-bug :strings-legacy-307
  (string>  "abc" "xyzabcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-311
  (string>  "abc" "xyzabcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-315
  (string>  "abc" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-319
  (string>  "abcdefgh" "")
  0)

(check-for-bug :strings-legacy-323
  (string>  "abcdefgh" "a")
  1)

(check-for-bug :strings-legacy-327
  (string>  "abcdefgh" "abc")
  3)

(check-for-bug :strings-legacy-331
  (string>  "abcdefgh" "cabc")
  nil)

(check-for-bug :strings-legacy-335
  (string>  "abcdefgh" "xyzabc")
  nil)

(check-for-bug :strings-legacy-339
  (string>  "xyzabcdefgh" "abc")
  0)

(check-for-bug :strings-legacy-343
  (string>  "abcdefgh" "abcdefgh" :end2 4)
  4)

(check-for-bug :strings-legacy-347
  (string>  "xyzabc" "abcdefgh" :start2 3)
  0)

(check-for-bug :strings-legacy-351
  (string>  "abc" "xyzabcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-355
  (string>  "abc" "xyzabcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-359
  (string>  "abc" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-363
  (string>  "abcde" "bc")
  nil)

(check-for-bug :strings-legacy-367
  (string>  "bcdef" "abcde")
  0)

(check-for-bug :strings-legacy-371
  (string>  "bcdef" "abcdef")
  0)

(check-for-bug :strings-legacy-375
  (string>  "abcdefghij" "abcdefgh" :start1 1)
  1)

(check-for-bug :strings-legacy-379
  (string>  "ghijkl" "xyzabcd" :start2 6 :end2 4)
  error)

(check-for-bug :strings-legacy-383
  (string<  "" "abcdefgh")
  0)

(check-for-bug :strings-legacy-387
  (string<=  "a" "abcdefgh")
  1)

(check-for-bug :strings-legacy-391
  (string<=  "abc" "abcdefgh")
  3)

(check-for-bug :strings-legacy-395
  (string<=  "aaabce" "aaabcdefgh")
  nil)

(check-for-bug :strings-legacy-399
  (string<=  "cabc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-403
  (string<=  "abcdefgh" "abcdefgh")
  8)

(check-for-bug :strings-legacy-407
  (string<=  "xyzabc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-411
  (string<=  "abc" "xyzabcdefgh")
  0)

(check-for-bug :strings-legacy-415
  (string<=  "abcdefgh" "abcdefgh" :end1 4)
  4)

(check-for-bug :strings-legacy-419
  (string<=  "xyzabc" "abcdefgh" :start1 3)
  6)

(check-for-bug :strings-legacy-423
  (string<=  "abc" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-427
  (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-431
  (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-435
  (string<=  "abcdefgh" "")
  nil)

(check-for-bug :strings-legacy-439
  (string<=  "abcdefgh" "a")
  nil)

(check-for-bug :strings-legacy-443
  (string<=  "abcdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-447
  (string<=  "abcdefgh" "cabc")
  0)

(check-for-bug :strings-legacy-451
  (string<=  "abcdefgh" "xyzabc")
  0)

(check-for-bug :strings-legacy-455
  (string<=  "xyzabcdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-459
  (string<=  "abcdefgh" "abcdefgh" :end2 4)
  nil)

(check-for-bug :strings-legacy-463
  (string<=  "xyzabc" "abcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-467
  (string<=  "abc" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-471
  (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-475
  (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-479
  (string<=  "abcdef" "bcdefgh")
  0)

(check-for-bug :strings-legacy-483
  (string<=  "abcdef" "abcdefgh" :start2 2)
  0)

(check-for-bug :strings-legacy-487
  (string<=  "abcdef" "bngdabcdef" :start2 9 :end2 5)
  error)


(check-for-bug :strings-legacy-492
  (string>= "" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-496
  (string>= "a" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-500
  (string>= "abc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-504
  (string>= "cabc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-508
  (string>= "abcdefgh" "abcdefgh")
  8)

(check-for-bug :strings-legacy-512
  (string>= "xyzabc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-516
  (string>= "abc" "xyzabcdefgh")
  nil)

(check-for-bug :strings-legacy-520
  (string>= "abcdefgh" "abcdefgh" :end1 4)
  nil)

(check-for-bug :strings-legacy-524
  (string>= "xyzabc" "abcdefgh" :start1 3)
  nil)

(check-for-bug :strings-legacy-528
  (string>= "abc" "xyzabcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-532
  (string>= "abc" "xyzabcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-536
  (string>= "abc" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-540
  (string>= "abcdefgh" "")
  0)

(check-for-bug :strings-legacy-544
  (string>= "abcdefgh" "a")
  1)

(check-for-bug :strings-legacy-548
  (string>= "abcdefgh" "abc")
  3)

(check-for-bug :strings-legacy-552
  (string>= "abcdefgh" "cabc")
  nil)

(check-for-bug :strings-legacy-556
  (string>= "abcdefgh" "xyzabc")
  nil)

(check-for-bug :strings-legacy-560
  (string>= "xyzabcdefgh" "abc")
  0)

(check-for-bug :strings-legacy-564
  (string>= "abcdefgh" "abcdefgh" :end2 4)
  4)

(check-for-bug :strings-legacy-568
  (string>= "xyzabc" "abcdefgh" :start2 3)
  0)

(check-for-bug :strings-legacy-572
  (string>= "xyzabc" "abcdefgh" :start1 3)
  nil)

(check-for-bug :strings-legacy-576
  (string>= "abc" "xyzabcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-580
  (string>= "abc" "xyzabcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-584
  (string>= "abc" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-588
  (string>= "bcdef" "abcdef")
  0)

(check-for-bug :strings-legacy-592
  (string>= "abcdefghij" "abcdefgh" :start1 1)
  1)

(check-for-bug :strings-legacy-596
  (string>= "ghijkl" "xyzabcd" :start2 6 :end2 4)
  error)

(check-for-bug :strings-legacy-600
  (string/= "" "abcdefgh")
  0)

(check-for-bug :strings-legacy-604
  (string/= "a" "abcdefgh")
  1)

(check-for-bug :strings-legacy-608
  (string/= "abc" "abcdefgh")
  3)

(check-for-bug :strings-legacy-612
  (string/= "cabc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-616
  (string/= "abcdefgh" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-620
  (string/= "xyzabc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-624
  (string/= "abc" "xyzabcdefgh")
  0)

(check-for-bug :strings-legacy-628
  (string/= "abcdefgh" "abcdefgh" :end1 4)
  4)

(check-for-bug :strings-legacy-632
  (string/= "xyzabc" "abcdefgh" :start1 3)
  6)

(check-for-bug :strings-legacy-636
  (string/= "abc" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-640
  (string/= "abc" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-644
  (string/= "abc" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-648
  (string/= "abcdefgh" "")
  0)

(check-for-bug :strings-legacy-652
  (string/= "abcdefgh" "a")
  1)

(check-for-bug :strings-legacy-656
  (string/= "abcdefgh" "abc")
  3)

(check-for-bug :strings-legacy-660
  (string/= "abcdefgh" "cabc")
  0)

(check-for-bug :strings-legacy-664
  (string/= "abcdefgh" "xyzabc")
  0)

(check-for-bug :strings-legacy-668
  (string/= "xyzabcdefgh" "abc")
  0)

(check-for-bug :strings-legacy-672
  (string/= "abcdefgh" "abcdefgh" :end2 4)
  4)

(check-for-bug :strings-legacy-676
  (string/= "xyzabc" "abcdefgh" :start2 3)
  0)

(check-for-bug :strings-legacy-680
  (string/= "abc" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-684
  (string/= "abc" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-688
  (string/= "abc" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-692
  (string/= "abcdefghi" "uvdefmgnj" :start1 3 :end1 6 :start2 2 :end2 5)
  nil)

(check-for-bug :strings-legacy-696
  (string/= "abcdefg" "abcdefg" :end2 4)
  4)

(check-for-bug :strings-legacy-700
  (string/= "abcdef" "abcdef" :start1 1 :end1 4 :start2 4 :end2 1)
  error)

(check-for-bug :strings-legacy-704
  (string-lessp "" "abcDEFgh")
  0)

(check-for-bug :strings-legacy-708
  (string-lessp "a" "Abcdefgh")
  1)

(check-for-bug :strings-legacy-712
  (string-lessp "abc" "aBcDEfgh")
  3)

(check-for-bug :strings-legacy-716
  (string-lessp "cABc" "aBCDefgh")
  nil)

(check-for-bug :strings-legacy-720
  (string-lessp "abCDeFgh" "abCDEfgh")
  nil)

(check-for-bug :strings-legacy-724
  (string-lessp "xyzAbc" "ABcCDfgh")
  nil)

(check-for-bug :strings-legacy-728
  (string-lessp "aBC" "xYZAbcdEfgh")
  0)

(check-for-bug :strings-legacy-732
  (string-lessp "abcDEfgh" "abcDEfgh" :end1 4)
  4)

(check-for-bug :strings-legacy-736
  (string-lessp "XYZabc" "ABcdefgh" :start1 3)
  6)

(check-for-bug :strings-legacy-740
  (string-lessp "aBc" "xyZABcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-744
  (string-lessp "abc" "xyzabCDEcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-748
  (string-lessp "abc" "xyzABcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-752
  (string-lessp "abcdefgh" "")
  nil)

(check-for-bug :strings-legacy-756
  (string-lessp "Abcdefgh" "a")
  nil)

(check-for-bug :strings-legacy-760
  (string-lessp "ABCdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-764
  (string-lessp "ABCdefgh" "cabc")
  0)

(check-for-bug :strings-legacy-768
  (string-lessp "abcdefgh" "xyzABC")
  0)

(check-for-bug :strings-legacy-772
  (string-lessp "xyzABCdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-776
  (string-lessp "abcdEFgh" "abcdeFGh" :end2 4)
  nil)

(check-for-bug :strings-legacy-780
  (string-lessp "xyzaBC" "abCDefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-784
  (string-lessp "ABC" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-788
  (string-lessp "ABC" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-792
  (string-lessp "ABC" "xyzabcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-796
  (string-lessp "aBCDef" "bcdefgh")
  0)

(check-for-bug :strings-legacy-800
  (string-lessp "aBCDef" "abcdefgh" :start2 2)
  0)

(check-for-bug :strings-legacy-804
  (string-lessp "aBCDef" "bngdabcdef" :start2 9 :end2 5)
  error)

(check-for-bug :strings-legacy-808
  (string-greaterp "" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-812
  (string-greaterp "A" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-816
  (string-greaterp "ABc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-820
  (string-greaterp "CAbc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-824
  (string-greaterp "abcdefgh" "abcDEFgh")
  nil)

(check-for-bug :strings-legacy-828
  (string-greaterp "xyzabc" "abCDEfgh")
  0)

(check-for-bug :strings-legacy-832
  (string-greaterp "ABC" "xyzabcdefgh")
  nil)

(check-for-bug :strings-legacy-836
  (string-greaterp "ABCdefgh" "abcdefgh" :end1 4)
  nil)

(check-for-bug :strings-legacy-840
  (string-greaterp "xyzaBc" "ABCdefgh" :start1 3)
  nil)

(check-for-bug :strings-legacy-844
  (string-greaterp "abc" "xyzABcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-848
  (string-greaterp "abc" "xyzABcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-852
  (string-greaterp "abc" "xyZAbcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-856
  (string-greaterp "abcdefgh" "")
  0)

(check-for-bug :strings-legacy-860
  (string-greaterp "Abcdefgh" "a")
  1)

(check-for-bug :strings-legacy-864
  (string-greaterp "ABCdefgh" "abc")
  3)

(check-for-bug :strings-legacy-868
  (string-greaterp "ABCdefgh" "cabc")
  nil)

(check-for-bug :strings-legacy-872
  (string-greaterp "ABCdefgh" "xyzabc")
  nil)

(check-for-bug :strings-legacy-876
  (string-greaterp "xyzabcdefgh" "Abc")
  0)

(check-for-bug :strings-legacy-880
  (string-greaterp "abcdefgh" "aBCDefgh" :end2 4)
  4)

(check-for-bug :strings-legacy-884
  (string-greaterp "xyzabc" "abcdEFgh" :start2 3)
  0)

(check-for-bug :strings-legacy-888
  (string-greaterp "ABC" "xyzabcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-892
  (string-greaterp "ABC" "xyzabcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-896
  (string-greaterp "ABC" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-900
  (string-greaterp "bCDEf" "abcde")
  0)

(check-for-bug :strings-legacy-904
  (string-greaterp "bcDEF" "abcdef")
  0)

(check-for-bug :strings-legacy-908
  (string-greaterp "abCDEfghij" "abcdefgh" :start1 1)
  1)

(check-for-bug :strings-legacy-912
  (string-greaterp "ghijKl" "xyzabcd" :start2 6 :end2 4)
  error)

(check-for-bug :strings-legacy-916
  (string-not-greaterp  "" "abcdefgh")
  0)

(check-for-bug :strings-legacy-920
  (string-not-greaterp  "A" "abcdefgh")
  1)

(check-for-bug :strings-legacy-924
  (string-not-greaterp  "aBC" "abcdefgh")
  3)

(check-for-bug :strings-legacy-928
  (string-not-greaterp  "CABc" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-932
  (string-not-greaterp  "abcDEFgh" "abcdefgh")
  8)

(check-for-bug :strings-legacy-936
  (string-not-greaterp  "xyzabc" "ABcdefgh")
  nil)

(check-for-bug :strings-legacy-940
  (string-not-greaterp  "abc" "xyzABcdefgh")
  0)

(check-for-bug :strings-legacy-944
  (string-not-greaterp  "ABCDEFgh" "abcdefgh" :end1 4)
  4)

(check-for-bug :strings-legacy-948
  (string-not-greaterp  "xyzabc" "aBCDefgh" :start1 3)
  6)

(check-for-bug :strings-legacy-952
  (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-956
  (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-960
  (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-964
  (string-not-greaterp  "abcdefgh" "")
  nil)

(check-for-bug :strings-legacy-968
  (string-not-greaterp  "Abcdefgh" "a")
  nil)

(check-for-bug :strings-legacy-972
  (string-not-greaterp  "ABCdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-976
  (string-not-greaterp  "ABCdefgh" "cabc")
  0)

(check-for-bug :strings-legacy-980
  (string-not-greaterp  "ABCdefgh" "xyzabc")
  0)

(check-for-bug :strings-legacy-984
  (string-not-greaterp  "xyzABCdefgh" "abc")
  nil)

(check-for-bug :strings-legacy-988
  (string-not-greaterp  "abcdeFgh" "abcdefgh" :end2 4)
  nil)

(check-for-bug :strings-legacy-992
  (string-not-greaterp  "xyzABC" "abcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-996
  (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-1000
  (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-1004
  (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
  nil)

(check-for-bug :strings-legacy-1008
  (string-not-greaterp  "abcDEF" "bcdefgh")
  0)

(check-for-bug :strings-legacy-1012
  (string-not-greaterp  "abcDEF" "abcdefgh" :start2 2)
  0)

(check-for-bug :strings-legacy-1016
  (string-not-greaterp  "abcdef" "bngDAbcdef" :start2 9 :end2 5)
  error)

(check-for-bug :strings-legacy-1020
  (string-not-lessp  "" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-1024
  (string-not-lessp  "a" "Abcdefgh")
  nil)

(check-for-bug :strings-legacy-1028
  (string-not-lessp  "ABC" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-1032
  (string-not-lessp  "CABc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-1036
  (string-not-lessp  "ABCdefgh" "abcdefgh")
  8)

(check-for-bug :strings-legacy-1040
  (string-not-lessp  "xyzABC" "abcdefgh")
  0)

(check-for-bug :strings-legacy-1044
  (string-not-lessp  "ABC" "xyzabcdefgh")
  nil)

(check-for-bug :strings-legacy-1048
  (string-not-lessp  "ABCdefgh" "abcdefgh" :end1 4)
  nil)

(check-for-bug :strings-legacy-1052
  (string-not-lessp  "xyzABC" "abcdefgh" :start1 3)
  nil)

(check-for-bug :strings-legacy-1056
  (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-1060
  (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-1064
  (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-1068
  (string-not-lessp  "abcdefgh" "")
  0)

(check-for-bug :strings-legacy-1072
  (string-not-lessp  "Abcdefgh" "a")
  1)

(check-for-bug :strings-legacy-1076
  (string-not-lessp  "ABCdefgh" "abc")
  3)

(check-for-bug :strings-legacy-1080
  (string-not-lessp  "abCDEfgh" "cabc")
  nil)

(check-for-bug :strings-legacy-1084
  (string-not-lessp  "aBCdefgh" "xyzabc")
  nil)

(check-for-bug :strings-legacy-1088
  (string-not-lessp  "xyzABcdefgh" "abc")
  0)

(check-for-bug :strings-legacy-1092
  (string-not-lessp  "abCDEfgh" "abcdefgh" :end2 4)
  4)

(check-for-bug :strings-legacy-1096
  (string-not-lessp  "xyzABc" "abcdefgh" :start2 3)
  0)

(check-for-bug :strings-legacy-1100
  (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3)
  nil)

(check-for-bug :strings-legacy-1104
  (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
  nil)

(check-for-bug :strings-legacy-1108
  (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-1112
  (string-not-lessp  "bCDef" "abcdef")
  0)

(check-for-bug :strings-legacy-1116
  (string-not-lessp  "ABCdefghij" "abcdefgh" :start1 1)
  1)

(check-for-bug :strings-legacy-1120
  (string-not-lessp  "ghIjkl" "xyzabcd" :start2 6 :end2 4)
  error)

(check-for-bug :strings-legacy-1124
  (string-not-equal  "" "abcdefgh")
  0)

(check-for-bug :strings-legacy-1128
  (string-not-equal  "A" "abcdefgh")
  1)

(check-for-bug :strings-legacy-1132
  (string-not-equal  "ABc" "abcdefgh")
  3)

(check-for-bug :strings-legacy-1136
  (string-not-equal  "cABc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-1140
  (string-not-equal  "ABCdefgh" "abcdefgh")
  nil)

(check-for-bug :strings-legacy-1144
  (string-not-equal  "xyzABc" "abcdefgh")
  0)

(check-for-bug :strings-legacy-1148
  (string-not-equal  "ABC" "xyzabcdefgh")
  0)

(check-for-bug :strings-legacy-1152
  (string-not-equal  "ABCdefgh" "abcdefgh" :end1 4)
  4)

(check-for-bug :strings-legacy-1156
  (string-not-equal  "xyzaBC" "abcdefgh" :start1 3)
  6)

(check-for-bug :strings-legacy-1160
  (string-not-equal  "ABC" "xyzabcdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-1164
  (string-not-equal  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-1168
  (string-not-equal  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-1172
  (string-not-equal  "abcdefgh" "")
  0)

(check-for-bug :strings-legacy-1176
  (string-not-equal  "Abcdefgh" "a")
  1)

(check-for-bug :strings-legacy-1180
  (string-not-equal  "aBCdefgh" "abc")
  3)

(check-for-bug :strings-legacy-1184
  (string-not-equal  "abcdefgh" "cABc")
  0)

(check-for-bug :strings-legacy-1188
  (string-not-equal  "abcdefgh" "xyzAbc")
  0)

(check-for-bug :strings-legacy-1192
  (string-not-equal  "xyzabcdefgh" "ABC")
  0)

(check-for-bug :strings-legacy-1196
  (string-not-equal  "abcdefgh" "abcDEFgh" :end2 4)
  4)

(check-for-bug :strings-legacy-1200
  (string-not-equal  "xyzabc" "aBCDefgh" :start2 3)
  0)

(check-for-bug :strings-legacy-1204
  (string-not-equal  "abc" "xyzABCdefgh" :start2 3)
  3)

(check-for-bug :strings-legacy-1208
  (string-not-equal  "abc" "xyzABCdefgh" :start2 3 :end2 8)
  3)

(check-for-bug :strings-legacy-1212
  (string-not-equal  "abc" "xyzABCdefgh" :start2 3 :end2 5)
  2)

(check-for-bug :strings-legacy-1216
  (string/=  "abcdefghi" "uvdEFmgnj" :start1 3 :end1 6 :start2 2 :end2 5)
  4)

(check-for-bug :strings-legacy-1220
  (string/=  "abcdefg" "abcDEfg" :end2 4)
  3)

(check-for-bug :strings-legacy-1224
  (string/=  "abcdef" "abCDef" :start1 1 :end1 4 :start2 4 :end2 1)
  error)

(check-for-bug :strings-legacy-1228
  (string-trim   (quote (#\space #\tab #\newline)) " garbanzo beans
   ")
  "garbanzo beans")

(check-for-bug :strings-legacy-1233
  (string-trim   " (*)" " ( *three(siily) words* ) ")
  "three(siily) words")

(check-for-bug :strings-legacy-1237
  (string-trim   (quote a) "ababa")
  error)

(check-for-bug :strings-legacy-1241
  (string-trim   (quote (a)) "ababa")
  #+xcl error
  #+(or clisp gcl allegro cmu ecls) "ababa"
  #-(or xcl clisp gcl allegro cmu ecls) unknown)

(check-for-bug :strings-legacy-1247
  (string-trim   "a" "ababa")
  "bab")

(check-for-bug :strings-legacy-1251
  (string-trim   "c e" "    ceabceabce    c")
  "abceab")

(check-for-bug :strings-legacy-1255
  (string-trim   (quote (#\a)) "abcd")
  "bcd")

(check-for-bug :strings-legacy-1259
  (string-trim   (quote (#\a)) "xyzabcd")
  "xyzabcd")

(check-for-bug :strings-legacy-1263
  (string-trim   (quote (#\a)) "abcda")
  "bcd")

(check-for-bug :strings-legacy-1267
  (string-left-trim   (quote (#\space #\tab #\newline)) " garbanzo beans
   ")
  "garbanzo beans
   ")

(check-for-bug :strings-legacy-1273
  (string-left-trim   " (*)" " ( *three(siily) words* ) ")
  "three(siily) words* ) ")

(check-for-bug :strings-legacy-1277
  (string-left-trim   (quote a) "ababa")
  error)

(check-for-bug :strings-legacy-1281
  (string-left-trim   (quote (a)) "ababa")
  #+xcl error
  #+(or clisp gcl allegro cmu ecls) "ababa"
  #-(or xcl clisp gcl allegro cmu ecls) unknown)

(check-for-bug :strings-legacy-1287
  (string-left-trim   "a" "ababa")
  "baba")

(check-for-bug :strings-legacy-1291
  (string-left-trim   "c e" "    ceabceabce    c")
  "abceabce    c")

(check-for-bug :strings-legacy-1295
  (string-left-trim   (quote (#\a)) "abcd")
  "bcd")

(check-for-bug :strings-legacy-1299
  (string-left-trim   (quote (#\a)) "xyzabcd")
  "xyzabcd")

(check-for-bug :strings-legacy-1303
  (string-left-trim   (quote (#\a)) "abcda")
  "bcda")

(check-for-bug :strings-legacy-1307
  (string-right-trim   (quote (#\space #\tab #\newline)) " garbanzo beans
   ")
  " garbanzo beans")

(check-for-bug :strings-legacy-1312
  (string-right-trim   " (*)" " ( *three(siily) words* ) ")
  " ( *three(siily) words")

(check-for-bug :strings-legacy-1316
  (string-right-trim   (quote a) "ababa")
  error)

(check-for-bug :strings-legacy-1320
  (string-right-trim   (quote (a)) "ababa")
  #+xcl error
  #+(or clisp gcl allegro cmu ecls) "ababa"
  #-(or xcl clisp gcl allegro cmu ecls) unknown)

(check-for-bug :strings-legacy-1326
  (string-right-trim   "a" "ababa")
  "abab")

(check-for-bug :strings-legacy-1330
  (string-right-trim   "c e" "    ceabceabce    c")
  "    ceabceab")

(check-for-bug :strings-legacy-1334
  (string-right-trim   (quote (#\a)) "abcd")
  "abcd")

(check-for-bug :strings-legacy-1338
  (string-right-trim   (quote (#\a)) "xyzabcd")
  "xyzabcd")

(check-for-bug :strings-legacy-1342
  (string-right-trim   (quote (#\a)) "abcda")
  "abcd")

(check-for-bug :strings-legacy-1346
  (string-upcase  "abCD efGh-ij")
  "ABCD EFGH-IJ")

(check-for-bug :strings-legacy-1350
  (string-upcase  "abCD efGh-ij" :start 5)
  "abCD EFGH-IJ")

(check-for-bug :strings-legacy-1354
  (string-upcase  "abCD efGh-ij" :end 5)
  "ABCD efGh-ij")

(check-for-bug :strings-legacy-1358
  (string-upcase  "abCD efGh-ij" :start 1 :end 6)
  "aBCD EfGh-ij")

(check-for-bug :strings-legacy-1362
  (string-upcase  "abCD efGh-ij" :start 6 :end 1)
  error)

(check-for-bug :strings-legacy-1366
  (string-upcase  "abCD efGh-ij" :start 3 :end 3)
  "abCD efGh-ij")

(check-for-bug :strings-legacy-1370
  (string-downcase  "abCD efGh-ij")
  "abcd efgh-ij")

(check-for-bug :strings-legacy-1374
  (string-downcase  "abCD efGh-ij" :start 3)
  "abCd efgh-ij")

(check-for-bug :strings-legacy-1378
  (string-downcase  "abCD efGh-ij" :end 3)
  "abcD efGh-ij")

(check-for-bug :strings-legacy-1382
  (string-downcase  "abCD efGh-ij" :start 3 :end 3)
  "abCD efGh-ij")

(check-for-bug :strings-legacy-1386
  (string-downcase  "abCD efGh-ij" :start 1 :end 6)
  "abcd efGh-ij")

(check-for-bug :strings-legacy-1390
  (string-downcase  "abCD efGh-ij" :start 6 :end 1)
  error)

(check-for-bug :strings-legacy-1394
  (string-capitalize  "abcd def g hi")
  "Abcd Def G Hi")

(check-for-bug :strings-legacy-1398
  (string-capitalize  "abCd dEf G hi")
  "Abcd Def G Hi")

(check-for-bug :strings-legacy-1402
  (string-capitalize  "Abcd Def G Hi")
  "Abcd Def G Hi")

(check-for-bug :strings-legacy-1406
  (string-capitalize  "abcd def g hi" :start 6)
  "abcd dEf G Hi")

(check-for-bug :strings-legacy-1410
  (string-capitalize  "abcd def g hi" :end 6)
  "Abcd Def g hi")

(check-for-bug :strings-legacy-1414
  (string-capitalize  "abcd def g hi" :start 2 :end 10)
  "abCd Def G hi")

(check-for-bug :strings-legacy-1418
  (string-capitalize  "abcd def g hi" :start 10 :end 2)
  error)

(check-for-bug :strings-legacy-1422
  (string-capitalize  "don't")
  "Don'T")

(check-for-bug :strings-legacy-1426
  (string-capitalize  "DON'T")
  "Don'T")

(check-for-bug :strings-legacy-1430
  (string-capitalize  "34a 5BC")
  "34a 5bc")

(check-for-bug :strings-legacy-1434
  (string  65)
  #+ecl "A"
  #-ecl error)

(check-for-bug :strings-legacy-1438
  (string  (quote a))
  "A")

(check-for-bug :strings-legacy-1442
  (string  #\a)
  "a")

(check-for-bug :strings-legacy-1446
  (string  "abc")
  "abc")

(check-for-bug :strings-legacy-1450
  (nstring-upcase
   (copy-seq "abCD efGh-ij"))
  "ABCD EFGH-IJ")

(check-for-bug :strings-legacy-1455
  (nstring-upcase
   (copy-seq "abCD efGh-ij")
   :start 5)
  "abCD EFGH-IJ")

(check-for-bug :strings-legacy-1461
  (nstring-upcase  (copy-seq "abCD efGh-ij")
                   :end 5)
  "ABCD efGh-ij")

(check-for-bug :strings-legacy-1466
  (nstring-upcase  (copy-seq "abCD efGh-ij")
                   :start6 :end 1)
  error)

(check-for-bug :strings-legacy-1471
  (nstring-upcase  (copy-seq "abCD efGh-ij")
                   :start 3 :end 3)
  "abCD efGh-ij")

(check-for-bug :strings-legacy-1476
  (nstring-downcase  (copy-seq "abCD efGh-ij"))
  "abcd efgh-ij")

(check-for-bug :strings-legacy-1480
  (nstring-downcase  (copy-seq "abCD efGh-ij")
                     :start 3)
  "abCd efgh-ij")

(check-for-bug :strings-legacy-1485
  (nstring-upcase  (copy-seq "abCD efGh-ij")
                   :start 1 :end 6)
  "aBCD EfGh-ij")

(check-for-bug :strings-legacy-1490
  (nstring-downcase  (copy-seq "abCD efGh-ij")
                     :end 3)
  "abcD efGh-ij")

(check-for-bug :strings-legacy-1495
  (nstring-downcase  (copy-seq "abCd efGh-ij")
                     :start 3 :end 3)
  "abCd efGh-ij")

(check-for-bug :strings-legacy-1500
  (nstring-downcase  (copy-seq "abCd efGh-ij")
                     :start 1 :end 6)
  "abcd efGh-ij")

(check-for-bug :strings-legacy-1505
  (nstring-downcase  (copy-seq "abCD efGh-ij")
                     :start 6 :end 1)
  error)

(check-for-bug :strings-legacy-1510
  (nstring-downcase  (copy-seq "abCD efGh-ij")
                     :start nil :end nil)
  #+(or xcl akcl) "abcd efgh-ij"
  #-(or xcl akcl) error)

(check-for-bug :strings-legacy-1516
  (nstring-upcase  (copy-seq "abDC efGh-oj"))
  "ABDC EFGH-OJ")

(check-for-bug :strings-legacy-1520
  (nstring-upcase (copy-seq "abCD efGh-ij")
                  :start 1 :end 6)
  "aBCD EfGh-ij")

(check-for-bug :strings-legacy-1525
  (nstring-upcase  (copy-seq "abCD efGh-fg")
                   :start 1 :end 6)
  "aBCD EfGh-fg")

(check-for-bug :strings-legacy-1530
  (nstring-upcase (copy-seq "abCD efGh-ef")
                  :start 3 :end 3)
  "abCD efGh-ef")

(check-for-bug :strings-legacy-1535
  (nstring-upcase  (copy-seq "abCD efGh-ef")
                   :start 3 :end 3)
  "abCD efGh-ef")

(check-for-bug :strings-legacy-1540
  (nstring-upcase  (copy-seq "abCD efGh-ef")
                   :start 3 :end 3)
  "abCD efGh-ef")

(check-for-bug :strings-legacy-1545
  (nstring-upcase  (copy-seq "abCD efGh-ef")
                   :start 3 :end 1)
  error)

(check-for-bug :strings-legacy-1550
  (nstring-upcase  (copy-seq "abCD efGh-ef")
                   :start nil :end nil)
  #+(or xcl akcl) "ABCD EFGH-EF"
  #-(or xcl akcl) error)

(check-for-bug :strings-legacy-1556
  (nstring-downcase  (copy-seq "saBG efGh-ef"))
  "sabg efgh-ef")

(check-for-bug :strings-legacy-1560
  (nstring-downcase  (copy-seq "dfGV efGh-ef")
                     :start 1 :end 6)
  "dfgv efGh-ef")

(check-for-bug :strings-legacy-1565
  (nstring-downcase  (copy-seq "fgCD efGf-ef")
                     :start 1 :end 3)
  "fgcD efGf-ef")

(check-for-bug :strings-legacy-1570
  (nstring-downcase  (copy-seq "dfCF edFg-fg")
                     :start nil :end nil)
  #+(or xcl akcl) "dfcf edfg-fg"
  #-(or xcl akcl) error)

(check-for-bug :strings-legacy-1576
  (nstring-downcase  (copy-seq "fgHG edgf-fg")
                     :start 5 :end 1)
  error)

(check-for-bug :strings-legacy-1581
  (nstring-downcase  (copy-seq "scDF edFG-ef")
                     :start 1)
  "scdf edfg-ef")

(check-for-bug :strings-legacy-1586
  (nstring-downcase  (copy-seq "fgHG edFG-ef")
                     :end 4)
  "fghg edFG-ef")

(check-for-bug :strings-legacy-1591
  (nstring-capitalize  (copy-seq "fg hgf fgh"))
  "Fg Hgf Fgh")

(check-for-bug :strings-legacy-1595
  (let ((x (copy-seq "ABCDEF")))
    (nstring-downcase x)
    x)
  "abcdef")


(check-for-bug :strings-added-1
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    x)
  "abcde")


(check-for-bug :strings-added-2
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    (char x 7))
  #\h)

(check-for-bug :strings-added-3
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    (elt x 7))
  error)

(check-for-bug :strings-added-3.1
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    (setf (char x 7) #\H))
  #\H)

(check-for-bug :strings-added-3.2
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    (setf (char x 7) #\H)
    (char x 7))
  #\H)

(check-for-bug :strings-added-4
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    (reverse x))
  "edcba")

(check-for-bug :strings-added-5
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    (reverse x)
    x)
  "abcde")

(check-for-bug :strings-added-6
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    
    (nreverse x))
  "edcba")

(check-for-bug :strings-added-7
  (let ((x (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    
    (nreverse x)
    x)
  "edcba")

(check-for-bug :strings-added-8
  (let* ((x (make-array 10 :fill-pointer 4 :element-type 'character
                        :initial-element #\space :adjustable t))
         (y (make-array 10 :fill-pointer 4 :element-type 'character
                        :displaced-to x)))
    (adjust-array x '(5))
    (char y 5))
  error)
