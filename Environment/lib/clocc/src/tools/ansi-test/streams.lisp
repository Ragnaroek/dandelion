;;; based on v1.6 -*- mode: lisp -*-
(in-package :cl-user)

#+xcl
(check-for-bug :streams-legacy-5
  (progn (in-package :sys) t)
  t)

#-(or akcl allegro)
(check-for-bug :streams-legacy-10
  (prin1-to-string (make-broadcast-stream))
  #+xcl "#<%TYPE-STRUCTURE-STREAM NIL>"
  #+clisp "#<OUTPUT BROADCAST-STREAM>"
  #+(or cmu sbcl) "#<Broadcast Stream>"
  #+ecls "#<broadcast stream 8>"
  #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(check-for-bug :streams-legacy-18
  (progn (setq s1 (open "d1.plc" :direction :output))
         (setq s2 (open "d2.plc" :direction :output))
         (setq s3 (open "d3.plc" :direction :output))
         (setq b1 (make-broadcast-stream s1 s2 s3 *standard-output*)) t)
  t)

(check-for-bug :streams-legacy-25
  (print "test broadcast satz 1" b1)
  "test broadcast satz 1")

(check-for-bug :streams-legacy-29
  (print "test broadcast satz 2" b1)
  "test broadcast satz 2")

(check-for-bug :streams-legacy-33
  (print "test broadcast satz 3" b1)
  "test broadcast satz 3")

;; CLOSE should not delete information about
;; element type, direction, and external format
(defun close-1 (s)
  (let* ((i (input-stream-p s))
         (o (output-stream-p s))
         (e (stream-element-type s))
         (f (stream-external-format s))
         (c (close s)))
    (and (eq i (input-stream-p s))
         (eq o (output-stream-p s))
         (equal e (stream-element-type s))
         (equal f (stream-external-format s))
         c)))

(check-for-bug :streams-legacy-37
  (close-1 s1)
  t)

(check-for-bug :streams-legacy-41
  (close-1 s2)
  t)

(check-for-bug :streams-legacy-45
  (close-1 s3)
  t)

(check-for-bug :streams-legacy-49
  (progn (setq s (open "d1.plc")) t)
  t)

(check-for-bug :streams-legacy-53
  (read s)
  "test broadcast satz 1")

(check-for-bug :streams-legacy-57
  (read s)
  "test broadcast satz 2")

(check-for-bug :streams-legacy-61
  (read s)
  "test broadcast satz 3")

(check-for-bug :streams-legacy-65
  (close-1 s)
  t)

(check-for-bug :streams-legacy-69
  (progn (setq s (open "d2.plc")) t)
  t)

(check-for-bug :streams-legacy-73
  (read s)
  "test broadcast satz 1")

(check-for-bug :streams-legacy-77
  (read s)
  "test broadcast satz 2")

(check-for-bug :streams-legacy-81
  (read s)
  "test broadcast satz 3")

(check-for-bug :streams-legacy-85
  (close-1 s)
  t)

(check-for-bug :streams-legacy-89
  (progn (setq s (open "d3.plc")) t)
  t)

(check-for-bug :streams-legacy-93
  (read s)
  "test broadcast satz 1")

(check-for-bug :streams-legacy-97
  (read s)
  "test broadcast satz 2")

(check-for-bug :streams-legacy-101
  (read s)
  "test broadcast satz 3")

(check-for-bug :streams-legacy-105
  (close-1 s)
  t)

(check-for-bug :streams-legacy-109
  (progn (setq s (open "t0.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-113
  (print (quote read1) s)
  read1)

(check-for-bug :streams-legacy-117
  (print (quote read2) s)
  read2)

(check-for-bug :streams-legacy-121
  (close-1 s)
  t)

(check-for-bug :streams-legacy-125
  (progn (setq inptw (open "t0.plc"))
         (setq s1 (open "d1.plc" :direction :output))
         (setq s2 (open "d2.plc" :direction :output))
         (setq sy (make-synonym-stream (quote s2)))
         (setq s3 (open "d3.plc" :direction :output))
         (setq tw (make-two-way-stream inptw s3))
         (setq s4 (open "d4.plc" :direction :output))
         (setq ec (make-echo-stream inptw s4))
         (setq s5 (open "d5.plc" :direction :output))
         (setq s6 (open "d6.plc" :direction :output))
         (setq b1 (make-broadcast-stream s5 s6))
         (setq s7 (open "d7.plc" :direction :output))
         (setq b2 (make-broadcast-stream s1 sy tw ec b1 s7)) t)
  t)

(check-for-bug :streams-legacy-141
  (print "w to b2 1.satz" b2)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-145
  (print "w to sy" sy)
  "w to sy")

(check-for-bug :streams-legacy-149
  (print "w to b2 2.satz" b2)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-153
  (print "w to tw" tw)
  "w to tw")

(check-for-bug :streams-legacy-157
  (print "w to b2 3.satz" b2)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-161
  (print "w to ec" ec)
  "w to ec")

(check-for-bug :streams-legacy-165
  (print "w to b2 4.satz" b2)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-169
  (print "w to b1" b1)
  "w to b1")

(check-for-bug :streams-legacy-173
  (print "w to b2 5.satz" b2)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-177
  (print "w to s7" s7)
  "w to s7")

(check-for-bug :streams-legacy-181
  (print "w to b2 6.satz" b2)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-185
  (read tw)
  read1)

(check-for-bug :streams-legacy-189
  (read ec)
  read2)

(check-for-bug :streams-legacy-193
  (print "w to b2 7.satz" b2)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-197
  (print "w to b2 8.satz" b2)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-201
  (close-1 inptw)
  t)

(check-for-bug :streams-legacy-205
  (close-1 s1)
  t)

(check-for-bug :streams-legacy-209
  (close-1 s2)
  t)

(check-for-bug :streams-legacy-213
  (close-1 s3)
  t)

(check-for-bug :streams-legacy-217
  (close-1 s4)
  t)

(check-for-bug :streams-legacy-221
  (close-1 s5)
  t)

(check-for-bug :streams-legacy-225
  (close-1 s6)
  t)

(check-for-bug :streams-legacy-229
  (close-1 s7)
  t)

(check-for-bug :streams-legacy-233
  (progn (setq s (open "d1.plc")) t)
  t)

(check-for-bug :streams-legacy-237
  (read s)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-241
  (read s)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-245
  (read s)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-249
  (read s)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-253
  (read s)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-257
  (read s)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-261
  (read s)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-265
  (read s)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-269
  (close-1 s)
  t)

(check-for-bug :streams-legacy-273
  (progn (setq s (open "d2.plc")) t)
  t)

(check-for-bug :streams-legacy-277
  (read s)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-281
  (read s)
  "w to sy")

(check-for-bug :streams-legacy-285
  (read s)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-289
  (read s)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-293
  (read s)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-297
  (read s)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-301
  (read s)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-305
  (read s)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-309
  (read s)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-313
  (close-1 s)
  t)

(check-for-bug :streams-legacy-317
  (progn (setq s (open "d3.plc")) t)
  t)

(check-for-bug :streams-legacy-321
  (read s)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-325
  (read s)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-329
  (read s)
  "w to tw")

(check-for-bug :streams-legacy-333
  (read s)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-337
  (read s)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-341
  (read s)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-345
  (read s)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-349
  (read s)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-353
  (read s)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-357
  (close-1 s)
  t)

(check-for-bug :streams-legacy-361
  (progn (setq s (open "d4.plc")) t)
  t)

(check-for-bug :streams-legacy-365
  (read s)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-369
  (read s)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-373
  (read s)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-377
  (read s)
  "w to ec")

(check-for-bug :streams-legacy-381
  (read s)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-385
  (read s)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-389
  (read s)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-393
  (read s)
  read2)

(check-for-bug :streams-legacy-397
  (read s)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-401
  (read s)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-405
  (close-1 s)
  t)

(check-for-bug :streams-legacy-409
  (progn (setq s (open "d5.plc")) t)
  t)

(check-for-bug :streams-legacy-413
  (read s)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-417
  (read s)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-421
  (read s)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-425
  (read s)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-429
  (read s)
  "w to b1")

(check-for-bug :streams-legacy-433
  (read s)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-437
  (read s)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-441
  (read s)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-445
  (read s)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-449
  (close-1 s)
  t)

(check-for-bug :streams-legacy-453
  (progn (setq s (open "d6.plc")) t)
  t)

(check-for-bug :streams-legacy-457
  (read s)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-461
  (read s)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-465
  (read s)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-469
  (read s)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-473
  (read s)
  "w to b1")

(check-for-bug :streams-legacy-477
  (read s)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-481
  (read s)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-485
  (read s)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-489
  (read s)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-493
  (close-1 s)
  t)

(check-for-bug :streams-legacy-497
  (progn (setq s (open "d7.plc")) t)
  t)

(check-for-bug :streams-legacy-501
  (read s)
  "w to b2 1.satz")

(check-for-bug :streams-legacy-505
  (read s)
  "w to b2 2.satz")

(check-for-bug :streams-legacy-509
  (read s)
  "w to b2 3.satz")

(check-for-bug :streams-legacy-513
  (read s)
  "w to b2 4.satz")

(check-for-bug :streams-legacy-517
  (read s)
  "w to b2 5.satz")

(check-for-bug :streams-legacy-521
  (read s)
  "w to s7")

(check-for-bug :streams-legacy-525
  (read s)
  "w to b2 6.satz")

(check-for-bug :streams-legacy-529
  (read s)
  "w to b2 7.satz")

(check-for-bug :streams-legacy-533
  (read s)
  "w to b2 8.satz")

(check-for-bug :streams-legacy-537
  (close-1 s)
  t)

(check-for-bug :streams-legacy-541
  (progn (setq s (open "t1.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-545
  (print "1.satz t1" s)
  "1.satz t1")

(check-for-bug :streams-legacy-549
  (print "2.satz t1" s)
  "2.satz t1")

(check-for-bug :streams-legacy-553
  (close-1 s)
  t)

(check-for-bug :streams-legacy-557
  (progn (setq s (open "t2.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-561
  (print "1.satz t2" s)
  "1.satz t2")

(check-for-bug :streams-legacy-565
  (print "2.satz t2" s)
  "2.satz t2")

(check-for-bug :streams-legacy-569
  (close-1 s)
  t)

(check-for-bug :streams-legacy-573
  (progn (setq s (open "t3.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-577
  (print "1.satz t3" s)
  "1.satz t3")

(check-for-bug :streams-legacy-581
  (print "2.satz t3" s)
  "2.satz t3")

(check-for-bug :streams-legacy-585
  (close-1 s)
  t)

(check-for-bug :streams-legacy-589
  (progn (setq s (open "t4.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-593
  (print "1.satz t4" s)
  "1.satz t4")

(check-for-bug :streams-legacy-597
  (print "2.satz t4" s)
  "2.satz t4")

(check-for-bug :streams-legacy-601
  (close-1 s)
  t)

(check-for-bug :streams-legacy-605
  (progn (setq s (open "t5.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-609
  (print "1.satz t5" s)
  "1.satz t5")

(check-for-bug :streams-legacy-613
  (print "2.satz t5" s)
  "2.satz t5")

(check-for-bug :streams-legacy-617
  (close-1 s)
  t)

(check-for-bug :streams-legacy-621
  (progn (setq s (open "t6.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-625
  (print "1.satz t6" s)
  "1.satz t6")

(check-for-bug :streams-legacy-629
  (print "2.satz t6" s)
  "2.satz t6")

(check-for-bug :streams-legacy-633
  (close-1 s)
  t)

(check-for-bug :streams-legacy-637
  (progn (setq s (open "t7.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-641
  (print "1.satz t7" s)
  "1.satz t7")

(check-for-bug :streams-legacy-645
  (print "2.satz t7" s)
  "2.satz t7")

(check-for-bug :streams-legacy-649
  (close-1 s)
  t)

(check-for-bug :streams-legacy-653
  (progn (setq s (open "t8.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-657
  (print "1.satz t8" s)
  "1.satz t8")

(check-for-bug :streams-legacy-661
  (print "2.satz t8" s)
  "2.satz t8")

(check-for-bug :streams-legacy-665
  (close-1 s)
  t)

(check-for-bug :streams-legacy-669
  (progn (setq s (open "t9.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-673
  (print "1.satz t9" s)
  "1.satz t9")

(check-for-bug :streams-legacy-677
  (print "2.satz t9" s)
  "2.satz t9")

(check-for-bug :streams-legacy-681
  (close-1 s)
  t)

(check-for-bug :streams-legacy-685
  (progn (setq s (open "t10.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-689
  (print "1.satz t10" s)
  "1.satz t10")

(check-for-bug :streams-legacy-693
  (print "2.satz t10" s)
  "2.satz t10")

(check-for-bug :streams-legacy-697
  (close-1 s)
  t)

(check-for-bug :streams-legacy-701
  (progn (setq s1 (open "t1.plc")) (setq s2 (open "t2.plc"))
         (setq s3 (open "t3.plc")) (setq s4 (open "t4.plc")) (setq s5 (open
                                                                       "t5.plc"))
         (setq c1 (make-concatenated-stream s1 s2 s3))
         (setq c2 (make-concatenated-stream s4 s5)) t)
  t)

(check-for-bug :streams-legacy-709
  (read c1)
  "1.satz t1")

(check-for-bug :streams-legacy-713
  (read c2)
  "1.satz t4")

(check-for-bug :streams-legacy-717
  (read c1)
  "2.satz t1")

(check-for-bug :streams-legacy-721
  (read c1)
  "1.satz t2")

(check-for-bug :streams-legacy-725
  (read c2)
  "2.satz t4")

(check-for-bug :streams-legacy-729
  (read c2)
  "1.satz t5")

(check-for-bug :streams-legacy-733
  (read c1)
  "2.satz t2")

(check-for-bug :streams-legacy-737
  (read c1)
  "1.satz t3")

(check-for-bug :streams-legacy-741
  (read c1)
  "2.satz t3")

(check-for-bug :streams-legacy-745
  (read c2)
  "2.satz t5")

(check-for-bug :streams-legacy-749
  (close-1 s1)
  t)

(check-for-bug :streams-legacy-753
  (close-1 s2)
  t)

(check-for-bug :streams-legacy-757
  (close-1 s3)
  t)

(check-for-bug :streams-legacy-761
  (close-1 s4)
  t)

(check-for-bug :streams-legacy-765
  (close-1 s5)
  t)

(check-for-bug :streams-legacy-769
  (progn (setq s1 (open "t1.plc")) (setq s2 (open "t2.plc"))
         (setq s3 (open "t3.plc")) (setq s4 (open "t4.plc")) (setq s5 (open
                                                                       "t5.plc"))
         (setq s6 (open "t6.plc")) (setq s7 (open "t7.plc")) (setq s8 (open
                                                                       "t8.plc"))
         (setq s9 (open "t9.plc")) (setq s10 (open "t10.plc"))
         (setq c1 (make-concatenated-stream s1 s2))
         (setq c2 (make-concatenated-stream s3))
         (setq c3 (make-concatenated-stream c1 c2 s4))
         (setq c4 (make-concatenated-stream s5 s6 s7 s8 s9 s10)) t)
  t)

(check-for-bug :streams-legacy-782
  (read c4)
  "1.satz t5")

(check-for-bug :streams-legacy-786
  (read c3)
  "1.satz t1")

(check-for-bug :streams-legacy-790
  (read c4)
  "2.satz t5")

(check-for-bug :streams-legacy-794
  (read c4)
  "1.satz t6")

(check-for-bug :streams-legacy-798
  (read c3)
  "2.satz t1")

(check-for-bug :streams-legacy-802
  (read c3)
  "1.satz t2")

(check-for-bug :streams-legacy-806
  (read c4)
  "2.satz t6")

(check-for-bug :streams-legacy-810
  (read c4)
  "1.satz t7")

(check-for-bug :streams-legacy-814
  (read c4)
  "2.satz t7")

(check-for-bug :streams-legacy-818
  (read c3)
  "2.satz t2")

(check-for-bug :streams-legacy-822
  (read c3)
  "1.satz t3")

(check-for-bug :streams-legacy-826
  (read c3)
  "2.satz t3")

(check-for-bug :streams-legacy-830
  (read c4)
  "1.satz t8")

(check-for-bug :streams-legacy-834
  (read c4)
  "2.satz t8")

(check-for-bug :streams-legacy-838
  (read c4)
  "1.satz t9")

(check-for-bug :streams-legacy-842
  (read c4)
  "2.satz t9")

(check-for-bug :streams-legacy-846
  (read c3)
  "1.satz t4")

(check-for-bug :streams-legacy-850
  (read c3)
  "2.satz t4")

(check-for-bug :streams-legacy-854
  (read c4)
  "1.satz t10")

(check-for-bug :streams-legacy-858
  (read c4)
  "2.satz t10")

(check-for-bug :streams-legacy-862
  (close-1 s1)
  t)

(check-for-bug :streams-legacy-866
  (close-1 s2)
  t)

(check-for-bug :streams-legacy-870
  (close-1 s3)
  t)

(check-for-bug :streams-legacy-874
  (close-1 s4)
  t)

(check-for-bug :streams-legacy-878
  (close-1 s5)
  t)

(check-for-bug :streams-legacy-882
  (close-1 s6)
  t)

(check-for-bug :streams-legacy-886
  (close-1 s7)
  t)

(check-for-bug :streams-legacy-890
  (close-1 s8)
  t)

(check-for-bug :streams-legacy-894
  (close-1 s9)
  t)

(check-for-bug :streams-legacy-898
  (close-1 s10)
  t)

(check-for-bug :streams-legacy-902
  (setq str1 "test 123456")
  "test 123456")

(check-for-bug :streams-legacy-906
  (progn (setq s1 (make-string-input-stream str1)) t)
  t)

(check-for-bug :streams-legacy-910
  (read s1)
  test)

(check-for-bug :streams-legacy-914
  (read-char s1)
  #\1)

(check-for-bug :streams-legacy-918
  (read-char s1)
  #\2)

(check-for-bug :streams-legacy-922
  (unread-char #\2 s1)
  nil)

(check-for-bug :streams-legacy-926
  (read-char s1)
  #\2)

(check-for-bug :streams-legacy-930
  (read-char s1)
  #\3)

(check-for-bug :streams-legacy-934
  (read-char s1)
  #\4)

(check-for-bug :streams-legacy-938
  (unread-char #\a s1)
  error
  "We previously read #\4 from S1, we are not allowed to
put #\a back in!")

(check-for-bug :streams-legacy-944
  (read-char s1)
  #\5
  "The previous unread-char should have failed, so
we expect to see #\5 here. If the unread-char worked
we will (wrongly!) see #\4 or #\a")

(check-for-bug :streams-legacy-951
  (read-char s1)
  #\6
  "Likewise the unread-char should have failed")

(check-for-bug :streams-legacy-956
  (close-1 s1)
  t)

(check-for-bug :streams-legacy-960
  str1
  "test 123456")

(check-for-bug :streams-legacy-964
  (multiple-value-list (read-from-string "012345 789"))
  (12345 7))

(check-for-bug :streams-legacy-968
  (multiple-value-list (read-from-string "012345 789" t nil
                                         :preserve-whitespace t))
  (12345 6))

(check-for-bug :streams-legacy-973
  (multiple-value-list (read-from-string "012345 789" t nil :end 4))
  (123 4))

(check-for-bug :streams-legacy-977
  (multiple-value-list (read-from-string "012345 789" t nil :start 2))
  (2345 7))

(check-for-bug :streams-legacy-981
  (progn (setq strgstream (make-string-input-stream "0123456789" 5 8))
         t)
  t)

(check-for-bug :streams-legacy-986
  (read strgstream)
  567)

(check-for-bug :streams-legacy-990
  (progn (setq strgstream
               (make-string-input-stream "wenn alles gut geht ist das ein stream 012"))
         t)
  t)

(check-for-bug :streams-legacy-996
  (read strgstream)
  wenn)

(check-for-bug :streams-legacy-1000
  (read strgstream)
  alles)

(check-for-bug :streams-legacy-1004
  (read strgstream)
  gut)

(check-for-bug :streams-legacy-1008
  (read strgstream)
  geht)

(check-for-bug :streams-legacy-1012
  (read strgstream)
  ist)

(check-for-bug :streams-legacy-1016
  (read strgstream)
  das)

(check-for-bug :streams-legacy-1020
  (read strgstream)
  ein)

(check-for-bug :streams-legacy-1024
  (read strgstream)
  stream)

(check-for-bug :streams-legacy-1028
  (read strgstream)
  12)

(check-for-bug :streams-legacy-1032
  (progn (setq strgstream (make-string-output-stream)) t)
  t)

(check-for-bug :streams-legacy-1036
  (princ "das " strgstream)
  "das ")

(check-for-bug :streams-legacy-1040
  (princ "ist " strgstream)
  "ist ")

(check-for-bug :streams-legacy-1044
  (princ "ein " strgstream)
  "ein ")

(check-for-bug :streams-legacy-1048
  (princ "string " strgstream)
  "string ")

(check-for-bug :streams-legacy-1052
  (princ "output " strgstream)
  "output ")

(check-for-bug :streams-legacy-1056
  (princ "stream " strgstream)
  "stream ")

(check-for-bug :streams-legacy-1060
  (get-output-stream-string strgstream)
  "das ist ein string output stream ")

(check-for-bug :streams-legacy-1064
  (get-output-stream-string strgstream)
  "")

(check-for-bug :streams-legacy-1068
  (princ "das ist ein neuer string output stream" strgstream)
  "das ist ein neuer string output stream")

(check-for-bug :streams-legacy-1072
  (get-output-stream-string strgstream)
  "das ist ein neuer string output stream")

(check-for-bug :streams-legacy-1076
  (setq *print-length* 50)
  50)

(check-for-bug :streams-legacy-1080
  (write-to-string 123456789)
  "123456789")

(check-for-bug :streams-legacy-1084
  "(write-to-string '#1=(123456789 . #1#))"
  "(write-to-string '#1=(123456789 . #1#))")

(check-for-bug :streams-legacy-1088
  (prin1-to-string "abc")
  "\"abc\"")

(check-for-bug :streams-legacy-1092
  (princ-to-string "abc")
  "abc")

(check-for-bug :streams-legacy-1096
  (progn (setq os (make-string-output-stream)) t)
  t)

(check-for-bug :streams-legacy-1100
  (setq s50 "123456789A123456789B123456789C123456789D12345678
E")
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1106
  (setq s49 "123456789A123456789B123456789C123456789D1234567
*")
  "123456789A123456789B123456789C123456789D1234567
*")

(check-for-bug :streams-legacy-1112
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1117
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1122
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1127
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1132
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1137
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1142
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1147
  (princ s49 os)
  "123456789A123456789B123456789C123456789D1234567
*")

(check-for-bug :streams-legacy-1152
  (princ "A" os)
  "A")

(check-for-bug :streams-legacy-1156
  (princ "B" os)
  "B")

(check-for-bug :streams-legacy-1160
  (princ "C" os)
  "C")

(check-for-bug :streams-legacy-1164
  (length (princ (get-output-stream-string os)))
  402)

(check-for-bug :streams-legacy-1168
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1173
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1178
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1183
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1188
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1193
  (princ s50 os)
  "123456789A123456789B123456789C123456789D12345678
E")

(check-for-bug :streams-legacy-1198
  (princ s49 os)
  "123456789A123456789B123456789C123456789D1234567
*")

(check-for-bug :streams-legacy-1203
  (princ s49 os)
  "123456789A123456789B123456789C123456789D1234567
*")

(check-for-bug :streams-legacy-1208
  (princ s49 os)
  "123456789A123456789B123456789C123456789D1234567
*")

(check-for-bug :streams-legacy-1213
  (princ s49 os)
  "123456789A123456789B123456789C123456789D1234567
*")

(check-for-bug :streams-legacy-1218
  (length (princ (get-output-stream-string os)))
  496)

(check-for-bug :streams-legacy-1222
  (progn (setq os (open "d0.plc" :direction :output))
         (setq os1 (open "d1.plc" :direction :output))
         (setq is (open "t0.plc" :direction :output)) t)
  t)

(check-for-bug :streams-legacy-1228
  (princ "'(a b #.(print \"1.zwischenwert\" os1) c d)" is)
  "'(a b #.(print \"1.zwischenwert\" os1) c d)")

(check-for-bug :streams-legacy-1232
  (princ "'(a b #.(prin1-to-string \"2.zwischenwert\") c d)" is)
  "'(a b #.(prin1-to-string \"2.zwischenwert\") c d)")

(check-for-bug :streams-legacy-1236
  (princ "'(a b #.(format nil  \"3.zwischenwert\") c d)" is)
  "'(a b #.(format nil  \"3.zwischenwert\") c d)")

(check-for-bug :streams-legacy-1240
  (close-1 is)
  t)

(check-for-bug :streams-legacy-1244
  (progn (setq is (open "t0.plc")) (setq es (make-echo-stream is os))
         t)
  t)

(check-for-bug :streams-legacy-1249
  (print "ausgabe os1" os1)
  "ausgabe os1")

(check-for-bug :streams-legacy-1253
  (read es)
  (quote (a b "1.zwischenwert" c d)))

(check-for-bug :streams-legacy-1257
  (print "ausgabe os1" os1)
  "ausgabe os1")

(check-for-bug :streams-legacy-1261
  (read es)
  (quote (a b "\"2.zwischenwert\"" c d)))

(check-for-bug :streams-legacy-1265
  (print "ausgabe os1" os1)
  "ausgabe os1")

(check-for-bug :streams-legacy-1269
  (read es)
  (quote (a b "3.zwischenwert" c d)))

(check-for-bug :streams-legacy-1273
  (print "ausgabe os1" os1)
  "ausgabe os1")

(check-for-bug :streams-legacy-1277
  (close-1 is)
  t)

(check-for-bug :streams-legacy-1281
  (close-1 os)
  t)

(check-for-bug :streams-legacy-1285
  (progn (setq is (open "d0.plc")) t)
  t)

(check-for-bug :streams-legacy-1289
  (read is)
  (quote (a b "1.zwischenwert" c d)))

(check-for-bug :streams-legacy-1293
  (read is)
  (quote (a b "\"2.zwischenwert\"" c d)))

(check-for-bug :streams-legacy-1297
  (read is)
  (quote (a b "3.zwischenwert" c d)))

(check-for-bug :streams-legacy-1301
  (close-1 is)
  t)

(check-for-bug :streams-legacy-1305
  (close-1 os1)
  t)

(check-for-bug :streams-legacy-1309
  (progn (setq is (open "d1.plc")) t)
  t)

(check-for-bug :streams-legacy-1313
  (read is)
  "ausgabe os1")

(check-for-bug :streams-legacy-1317
  (read is)
  "1.zwischenwert")

(check-for-bug :streams-legacy-1321
  (read is)
  "ausgabe os1")

(check-for-bug :streams-legacy-1325
  (read is)
  "ausgabe os1")

(check-for-bug :streams-legacy-1329
  (read is)
  "ausgabe os1")

(check-for-bug :streams-legacy-1333
  (read is)
  "1.zwischenwert")

(check-for-bug :streams-legacy-1337
  (close-1 is)
  t)

(check-for-bug :streams-legacy-1341
  (progn (mapc #'delete-file (directory "*.plc")) t)
  t)

(check-for-bug :streams-legacy-1345
  (progn
    (makunbound 's)
    (makunbound 's1)
    (makunbound 's2)
    (makunbound 's3)
    (makunbound 's4)
    (makunbound 's5)
    (makunbound 's6)
    (makunbound 's7)
    (makunbound 's8)
    (makunbound 's9)
    (makunbound 's10)
    (makunbound 'b1)
    (makunbound 'b2)
    (makunbound 'c1)
    (makunbound 'c2)
    (makunbound 'c3)
    (makunbound 'c4)
    (makunbound 'inptw)
    (makunbound 'sy)
    (makunbound 'tw)
    (makunbound 'ec)
    (makunbound 'str1)
    (makunbound 'strgstream)
    (makunbound 'os)
    (makunbound 'os1)
    (makunbound 'is)
    (makunbound 'es)
    (makunbound 's50)
    (makunbound 's49)
    (setq *print-length* nil)
    t)
  t)

