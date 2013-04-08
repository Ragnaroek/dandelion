;;; based on v1.10 -*- mode: lisp -*-
(in-package :cl-user)

;; ****************************************************************************
;; *      test der i/o-funktionen                                             *
;; ****************************************************************************

#+xcl
(check-for-bug :iofkts-legacy-9
  (progn (in-package "SYS") t)
  t)

;; --- let test ---------------------------------------------------------------
;;  ewiger compiler-fehler
;;

(check-for-bug :iofkts-legacy-17
  (progn (setq bs (make-broadcast-stream)) t)
  t)

#+xcl
(check-for-bug :iofkts-legacy-22
  *cur-broadcast-stream*
  nil)

(check-for-bug :iofkts-legacy-26
  (print 123. bs)
  123.)

#+xcl
(check-for-bug :iofkts-legacy-31
  *cur-broadcast-stream*
  nil)

;; -------------------------------------------------------------------------------
;;  unread test mit structure-stream
;;

(check-for-bug :iofkts-legacy-39
  (setq str1 "test 123456")   "test 123456")

(check-for-bug :iofkts-legacy-42
  (progn (setq s1 (make-two-way-stream (make-string-input-stream str1)
                                       *standard-output*)) t)
  t)

(check-for-bug :iofkts-legacy-47
  (read s1)   test)

(check-for-bug :iofkts-legacy-50
  (read-char s1)   #\1)

(check-for-bug :iofkts-legacy-53
  (read-char s1)   #\2)

(check-for-bug :iofkts-legacy-56
  (unread-char #\2 s1)   nil)

(check-for-bug :iofkts-legacy-59
  (read-char s1)   #\2)

(check-for-bug :iofkts-legacy-62
  (read-char s1)   #\3)

(check-for-bug :iofkts-legacy-65
  (read-char s1)   #\4)

(check-for-bug :iofkts-legacy-68
  (unread-char #\a s1)   error
  "I just read #\4 I cannot put #\a back")

(check-for-bug :iofkts-legacy-72
  (read-char s1)   #\5 "The last unread should have failed, we're
out of sync")

(check-for-bug :iofkts-legacy-76
  (read-char s1)   #\6 "still out of sync?")

(check-for-bug :iofkts-legacy-79
  (close s1)   t)

(check-for-bug :iofkts-legacy-82
  str1   "test 123456")


;; -------------------------------------------------------------------------------

(check-for-bug :iofkts-legacy-88
  (multiple-value-list (parse-integer "abc"))
  error)

(check-for-bug :iofkts-legacy-92
  (multiple-value-list (parse-integer "  abc  "))
  error)

(check-for-bug :iofkts-legacy-96
  (multiple-value-list (parse-integer "123"))
  (123 3))

(check-for-bug :iofkts-legacy-100
  (multiple-value-list (parse-integer "  123  "))
  #-(or cmu sbcl)
  (123 7)
  #+(or cmu sbcl)
  (123 5))

(check-for-bug :iofkts-legacy-107
  (multiple-value-list (parse-integer "123 t"))
  error)

(check-for-bug :iofkts-legacy-111
  (multiple-value-list (parse-integer "  123   t  "))
  error)

(check-for-bug :iofkts-legacy-115
  (multiple-value-list (parse-integer " ( 12 ) 43   t  "))
  error)

(check-for-bug :iofkts-legacy-119
  (multiple-value-list (parse-integer "  abc  " :junk-allowed t))
  (nil 2))

(check-for-bug :iofkts-legacy-123
  (multiple-value-list (parse-integer "123" :junk-allowed t))
  (123 3))

(check-for-bug :iofkts-legacy-127
  (multiple-value-list (parse-integer "  123  " :junk-allowed t))
  (123 #+xcl 7
       #+(or clisp akcl allegro cmu sbcl ecls) 5
       #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown))

(check-for-bug :iofkts-legacy-133
  (multiple-value-list (parse-integer "123 t" :junk-allowed t))
  (123 #+xcl 4
       #+(or clisp akcl allegro cmu sbcl ecls) 3
       #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown))

(check-for-bug :iofkts-legacy-139
  (multiple-value-list (parse-integer "  123   t  " :junk-allowed t))
  (123 #+xcl 8
       #+(or clisp akcl allegro cmu sbcl ecls) 5
       #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown))

(check-for-bug :iofkts-legacy-145
  (multiple-value-list (parse-integer " ( 12 ) 43   t  " :junk-allowed
                                      t))
  (nil 1))

(check-for-bug :iofkts-legacy-150
  (setq a "q w e 1 2 r 4 d : :;;;")
  "q w e 1 2 r 4 d : :;;;")

(check-for-bug :iofkts-legacy-154
  (setq b "1 2 3 4 5 6 7")
  "1 2 3 4 5 6 7")

(check-for-bug :iofkts-legacy-158
  (setq c "1.3 4.223")
  "1.3 4.223")

(check-for-bug :iofkts-legacy-162
  (setq d "q w e r t z")
  "q w e r t z")

(check-for-bug :iofkts-legacy-166
  (multiple-value-list (parse-integer a))
  error)

(check-for-bug :iofkts-legacy-170
  (multiple-value-list (parse-integer b))
  error)

(check-for-bug :iofkts-legacy-174
  (multiple-value-list (parse-integer c))
  error)

(check-for-bug :iofkts-legacy-178
  (multiple-value-list (parse-integer d))
  error)

(check-for-bug :iofkts-legacy-182
  (multiple-value-list (parse-integer a :start 4 :end 6))
  error)

(check-for-bug :iofkts-legacy-186
  (multiple-value-list (parse-integer b :start 2 :end 3))
  (2 3))

(check-for-bug :iofkts-legacy-190
  (multiple-value-list (parse-integer c :start 1))
  error)

(check-for-bug :iofkts-legacy-194
  (multiple-value-list (parse-integer d :start 6))
  error)

(check-for-bug :iofkts-legacy-198
  (multiple-value-list (parse-integer a :end 4))
  error)

(check-for-bug :iofkts-legacy-202
  (multiple-value-list (parse-integer b :end 3))
  error)

(check-for-bug :iofkts-legacy-206
  (multiple-value-list (parse-integer c :end 3))
  error)

(check-for-bug :iofkts-legacy-210
  (multiple-value-list (parse-integer d :end 1))
  error)

(check-for-bug :iofkts-legacy-214
  (multiple-value-list (parse-integer a :radix 1))
  error)

(check-for-bug :iofkts-legacy-218
  (multiple-value-list (parse-integer b :radix 10))
  error)

(check-for-bug :iofkts-legacy-222
  (multiple-value-list (parse-integer c :radix 20))
  error)

(check-for-bug :iofkts-legacy-226
  (multiple-value-list (parse-integer d :radix 40))
  error)

(check-for-bug :iofkts-legacy-230
  (multiple-value-list (parse-integer a :junk-allowed t))
  (nil 0))

(check-for-bug :iofkts-legacy-234
  (multiple-value-list (parse-integer b :junk-allowed t))
  (1 #+xcl 2
     #+(or clisp akcl allegro cmu sbcl ecls) 1
     #-(or xcl clisp akcl allegro cmu sbcl) unknown))

(check-for-bug :iofkts-legacy-240
  (multiple-value-list (parse-integer c :junk-allowed t))
  (1 1))

(check-for-bug :iofkts-legacy-244
  (multiple-value-list (parse-integer d :junk-allowed t))
  (nil 0))

(check-for-bug :iofkts-legacy-248
  (stream-element-type #+xcl stdin
                       #-xcl *terminal-io*)
  character)

(check-for-bug :iofkts-legacy-253
  (progn (setq a (make-string-input-stream "aaa bbb")) t)
  t)

(check-for-bug :iofkts-legacy-257
  (read a)
  aaa)

#+xcl
(check-for-bug :iofkts-legacy-262
  (b-clear-input a)
  nil)

(check-for-bug :iofkts-legacy-266
  (read a)
  #+xcl error
  #-xcl bbb)

(check-for-bug :iofkts-legacy-271
  (progn (setq a (make-string-output-stream))
         (setq b (make-string-output-stream))
         (setq c (make-broadcast-stream a b)) t)
  t)

(check-for-bug :iofkts-legacy-277
  (print "xxx" c)
  "xxx")

(check-for-bug :iofkts-legacy-281
  (clear-output c)
  nil)

(check-for-bug :iofkts-legacy-285
  (finish-output c)
  #+xcl t
  #-xcl nil)

(check-for-bug :iofkts-legacy-290
  (get-output-stream-string a)
  "
\"xxx\" ")

(check-for-bug :iofkts-legacy-295
  (get-output-stream-string b)
  "
\"xxx\" ")

(check-for-bug :iofkts-legacy-300
  (print "yyy" c)
  "yyy")

(check-for-bug :iofkts-legacy-304
  (clear-output c)
  nil)

(check-for-bug :iofkts-legacy-308
  (finish-output c)
  #+xcl t
  #-xcl nil)

(check-for-bug :iofkts-legacy-313
  (print "zzz" a)
  "zzz")

(check-for-bug :iofkts-legacy-317
  (clear-output a)
  nil)

(check-for-bug :iofkts-legacy-321
  (finish-output a)
  #+xcl t
  #-xcl nil)

(check-for-bug :iofkts-legacy-326
  (get-output-stream-string a)
  #+xcl ""
  #-xcl "
\"yyy\" \
\"zzz\" ")

(check-for-bug :iofkts-legacy-333
  (get-output-stream-string b)
  "
\"yyy\" ")

(check-for-bug :iofkts-legacy-338
  (progn (setq a (make-string-input-stream "123")) t)
  t)

(check-for-bug :iofkts-legacy-342
  (listen a)
  t)

(check-for-bug :iofkts-legacy-346
  (read a)
  123)

(check-for-bug :iofkts-legacy-350
  (listen a)
  nil)

(check-for-bug :iofkts-legacy-354
  *print-case*
  :upcase)

(check-for-bug :iofkts-legacy-358
  *print-gensym*
  t)

(check-for-bug :iofkts-legacy-362
  *print-level*
  nil)

(check-for-bug :iofkts-legacy-366
  *print-length*
  nil)

(check-for-bug :iofkts-legacy-370
  *print-array*
  t)

(check-for-bug :iofkts-legacy-374
  *print-escape*
  t)

(check-for-bug :iofkts-legacy-378
  *print-pretty*
  nil)

(check-for-bug :iofkts-legacy-382
  *print-circle*
  nil)

(check-for-bug :iofkts-legacy-386
  *print-base*
  10)

(check-for-bug :iofkts-legacy-390
  *print-radix*
  nil)

(check-for-bug :iofkts-legacy-394
  (setq string1 "Das ist ein Test mit Print ")
  "Das ist ein Test mit Print ")

(check-for-bug :iofkts-legacy-398
  (prin1-to-string string1)
  "\"das ist ein test mit print \"")

(check-for-bug :iofkts-legacy-402
  (princ-to-string string1)
  "Das ist ein Test mit Print ")

(check-for-bug :iofkts-legacy-406
  (progn (setq a (make-string-input-stream "123")) t)
  t)

(check-for-bug :iofkts-legacy-410
  (read-char-no-hang a)
  #\1)

(check-for-bug :iofkts-legacy-414
  (read a)
  23)

(check-for-bug :iofkts-legacy-418
  (read-char-no-hang a)
  error)

(check-for-bug :iofkts-legacy-422
  (read-char-no-hang a nil "EOF")
  "EOF")

(check-for-bug :iofkts-legacy-426
  (progn (setq a (make-string-input-stream "1   2   ;32  abA"))
         (setq b (make-string-input-stream " 1 2 3 A x y z
a b c ")) t)
  t)

(check-for-bug :iofkts-legacy-432
  (read-delimited-list #\A b)
  (1 2 3))

(check-for-bug :iofkts-legacy-436
  (setq c (multiple-value-list (read-line b)))
  (" x y z" nil))

(check-for-bug :iofkts-legacy-440
  (length c)
  2)

(check-for-bug :iofkts-legacy-444
  (multiple-value-list (read-line b))
  ("a b c " t))

(check-for-bug :iofkts-legacy-448
  (multiple-value-list (read-line b))
  error)

(check-for-bug :iofkts-legacy-452
  (multiple-value-list (read-line b nil "EOF"))
  ("EOF" t)
  "read-line &optional input-stream eof-error-p eof-value recursive-p

=> line, missing-newline-p
")

(check-for-bug :iofkts-legacy-460
  (peek-char nil a)
  #\1)

(check-for-bug :iofkts-legacy-464
  (read-char a)
  #\1)

(check-for-bug :iofkts-legacy-468
  (peek-char t a)
  #\2)

(check-for-bug :iofkts-legacy-472
  (read-char a)
  #\2)

(check-for-bug :iofkts-legacy-476
  (peek-char t a)
  #\;)

(check-for-bug :iofkts-legacy-480
  (read-char a)
  #\;)

(check-for-bug :iofkts-legacy-484
  (peek-char #\A a)
  #\A)

(check-for-bug :iofkts-legacy-488
  (read-char a)
  #\A)

(check-for-bug :iofkts-legacy-492
  (peek-char nil a)
  error)

(check-for-bug :iofkts-legacy-496
  (peek-char nil a nil "EOF")
  "EOF")

(check-for-bug :iofkts-legacy-500
  (setq a (quote
           ((berlin (dresden frankfurt bonn muenchen)) (mueller (karl luise dieter
                                                                      aldo)))))
  ((berlin (dresden frankfurt bonn muenchen)) (mueller (karl luise dieter
                                                             aldo))))

(check-for-bug :iofkts-legacy-507
  (progn (setq aa (make-string-input-stream "berlin d mueller :r")) t)
  t)

(check-for-bug :iofkts-legacy-511
  (defun ask (&optional (res nil))
    "  (terpri)(terpri)(terpri)
  (print '(*** Eingabe des  Keywortes ***))
  (print '(- mit :r reset))
  (terpri)" (setq x (read aa)) "  (print x)" (cond
                                               ((equal x (quote :r)) (cons "--- reset ---" res))
                                               (t (cons (cadr (assoc x a)) (ask res)))))
  ask)

(check-for-bug :iofkts-legacy-521
  (ask)
  ((dresden frankfurt bonn muenchen) nil (karl luise dieter aldo) "--- reset ---"))

(check-for-bug :iofkts-legacy-525
  (setq string1 "Das ist ein Teststring")
  "Das ist ein Teststring")

(check-for-bug :iofkts-legacy-529
  (setq string2 "Auch das 1 2 3 ist ein Teststring")
  "Auch das 1 2 3 ist ein Teststring")

(check-for-bug :iofkts-legacy-533
  (multiple-value-list (read-from-string string1))
  (das 4))

(check-for-bug :iofkts-legacy-537
  (multiple-value-list (read-from-string string2))
  (auch 5))

(check-for-bug :iofkts-legacy-541
  (multiple-value-list (read-from-string string1 t nil :start 2))
  (s 4))

(check-for-bug :iofkts-legacy-545
  (multiple-value-list
      (read-from-string string1 t nil :start 2 :preserve-whitespace t))
  (s 3))

(check-for-bug :iofkts-legacy-550
  (multiple-value-list (read-from-string string2 t nil :start 5))
  (das 9))

(check-for-bug :iofkts-legacy-554
  (multiple-value-list (read-from-string string2 t nil :start 5 :end
                                         6))
  (d 6))

(check-for-bug :iofkts-legacy-559
  (multiple-value-list (read-from-string string1 t nil :start 4 :end
                                         3))
  error)

(check-for-bug :iofkts-legacy-564
  (multiple-value-list (read-from-string string1 t nil :end 0))
  error)

(check-for-bug :iofkts-legacy-568
  (multiple-value-list (read-from-string string1 t nil :start -2 :end
                                         0))
  error)

(check-for-bug :iofkts-legacy-573
  (multiple-value-list (read-from-string string1 t nil :end 2))
  (da 2))

(check-for-bug :iofkts-legacy-577
  *read-suppress*
  nil)

(check-for-bug :iofkts-legacy-581
  (standard-char-p (quote a))
  error)

(check-for-bug :iofkts-legacy-585
  (standard-char-p (quote #\backspace))
  #+xcl t
  #-xcl nil)

(check-for-bug :iofkts-legacy-590
  (standard-char-p (quote #\tab))
  #+xcl t
  #-xcl nil)

(check-for-bug :iofkts-legacy-595
  (standard-char-p (quote #\newline))
  t)

(check-for-bug :iofkts-legacy-599
  (standard-char-p (quote #\page))
  #+xcl t
  #-xcl nil)

(check-for-bug :iofkts-legacy-604
  (standard-char-p (quote #\return))
  #+xcl t
  #-xcl nil)

#-(or cmu sbcl sbcl)
(check-for-bug :iofkts-legacy-610
  (string-char-p (quote a))
  error)

(check-for-bug :iofkts-legacy-614
  (characterp (quote
               #\space))
  t)

(check-for-bug :iofkts-legacy-619
  (characterp (quote
               #\newline))
  t)

(check-for-bug :iofkts-legacy-624
  (characterp (quote
               #\backspace))
  t)

(check-for-bug :iofkts-legacy-629
  (characterp (quote
               #\a))
  t)

(check-for-bug :iofkts-legacy-634
  (characterp (quote
               #\8))
  t)

(check-for-bug :iofkts-legacy-639
  (characterp (quote
               #\-))
  t)

(check-for-bug :iofkts-legacy-644
  (characterp (quote
               #\n))
  t)

(check-for-bug :iofkts-legacy-649
  (characterp (quote
               #\())
  t)

(check-for-bug :iofkts-legacy-654
  (stringp "das ist einer der Teststrings")
  t)

(check-for-bug :iofkts-legacy-658
  (stringp (quote (das ist natuerlich falsch)))
  nil)

(check-for-bug :iofkts-legacy-662
  (stringp "das ist die eine Haelfte" "und das die andere")
  error)

(check-for-bug :iofkts-legacy-666
  (setq j 0)
  0)

(check-for-bug :iofkts-legacy-670
  (with-input-from-string (s "animal crackers" :start 6) (read s))
  crackers)

(check-for-bug :iofkts-legacy-674
  (with-input-from-string (s "animal crackers" :index j :start 6) (read s))
  crackers)

(check-for-bug :iofkts-legacy-678
  j
  15)

(check-for-bug :iofkts-legacy-682
  (with-input-from-string (s "animal crackers" :index j :start 7) (read s))
  crackers)

(check-for-bug :iofkts-legacy-686
  j
  15)

(check-for-bug :iofkts-legacy-690
  (with-input-from-string (s "animal crackers" :index j :start 2) (read s))
  imal)

(check-for-bug :iofkts-legacy-694
  j
  7)

(check-for-bug :iofkts-legacy-698
  (with-input-from-string (s "animal crackers" :index j :start 0 :end 6) (read s))
  animal)

(check-for-bug :iofkts-legacy-702
  j
  6)

(check-for-bug :iofkts-legacy-706
  (with-input-from-string (s "animal crackers"
                             :index j
                             :start 0 :end 12)
    (read s))
  animal)

(check-for-bug :iofkts-legacy-713
  j
  7)

(check-for-bug :iofkts-legacy-717
  (with-input-from-string (s "animal crackers" :index j :start -1) (read s))
  error)

(check-for-bug :iofkts-legacy-721
  j
  7)

(check-for-bug :iofkts-legacy-725
  (with-input-from-string (s "animal crackers"
                             :index j
                             :start 6 :end 20)
    (read s))
  #+xcl
  crackers
  #+(or clisp akcl allegro sbcl cmu ecls)
  error
  #-(or xcl clisp akcl allegro sbcl cmu ecls)
  unknown)

(check-for-bug :iofkts-legacy-737
  j
  #+xcl
  20
  #+(or clisp akcl allegro sbcl cmu ecls)
  7
  #-(or xcl clisp akcl allegro sbcl cmu ecls)
  unknown)

(check-for-bug :iofkts-legacy-746
  (setq a "Das ist wieder einmal einer der SUUPERTESTstrings.")
  "Das ist wieder einmal einer der SUUPERTESTstrings.")

(check-for-bug :iofkts-legacy-750
  (progn (setq b (make-string-output-stream)) t)
  t)

(check-for-bug :iofkts-legacy-754
  (write-string a b)
  "Das ist wieder einmal einer der SUUPERTESTstrings.")

(check-for-bug :iofkts-legacy-758
  (write-string a b :start 10)
  "Das ist wieder einmal einer der SUUPERTESTstrings.")

(check-for-bug :iofkts-legacy-762
  (write-string a b :start 80)
  #+xcl "Das ist wieder einmal einer der SUUPERTESTstrings."
  #-xcl error)

(check-for-bug :iofkts-legacy-767
  (write-string a b :end 5)
  "Das ist wieder einmal einer der SUUPERTESTstrings.")

(check-for-bug :iofkts-legacy-771
  (write-string a b :end -2)
  error)

(check-for-bug :iofkts-legacy-775
  (write-string a b :end 100)
  #+(or sbcl cmu xcl)
  "Das ist wieder einmal einer der SUUPERTESTstrings."
  #-(or sbcl cmu xcl)
  error)

(check-for-bug :iofkts-legacy-782
  (write-string a b :start 5 :end 20)
  "Das ist wieder einmal einer der SUUPERTESTstrings.")

(check-for-bug :iofkts-legacy-786
  (write-string a b :start 10 :end 5)
  #+xcl "Das ist wieder einmal einer der SUUPERTESTstrings."
  #-xcl error)

(check-for-bug :iofkts-legacy-791
  (get-output-stream-string b)
  #+(or sbcl cmu xcl)
  "Das ist wieder einmal einer der SUUPERTESTstrings.eder einmal einer der SUUPERTESTstrings.Das iDas ist wieder einmal einer der SUUPERTESTstrings.st wieder einma"
  #+(or clisp akcl ecls)
  "Das ist wieder einmal einer der SUUPERTESTstrings.eder einmal einer der SUUPERTESTstrings.Das ist wieder einma"
  #-(or xcl clisp akcl sbcl cmu ecls)
  unknown)

(check-for-bug :iofkts-legacy-800
  (write-string a b)
  "Das ist wieder einmal einer der SUUPERTESTstrings.")

(check-for-bug :iofkts-legacy-804
  (length (get-output-stream-string b))
  50)

(check-for-bug :iofkts-legacy-808
  (write-line a b)
  "Das ist wieder einmal einer der SUUPERTESTstrings.")

(check-for-bug :iofkts-legacy-812
  (length (get-output-stream-string b))
  51)

(check-for-bug :iofkts-legacy-816
  (with-output-to-string (s) (print (quote xxx) s))
  "
XXX ")

(check-for-bug :iofkts-legacy-821
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    a)
  "")

(check-for-bug :iofkts-legacy-828
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s)))
  123)

(check-for-bug :iofkts-legacy-835
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s))
    a)
  "123")

(check-for-bug :iofkts-legacy-843
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s))
    (with-output-to-string (s a) (princ 4567 s)))
  4567)

(check-for-bug :iofkts-legacy-851
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s))
    (with-output-to-string (s a) (princ 4567 s))
    a)
  "1234567")

(check-for-bug :iofkts-legacy-860
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s))
    (with-output-to-string (s a) (princ 4567 s))
    (with-output-to-string (s a)
      (princ 890 s)))
  890)

(check-for-bug :iofkts-legacy-870
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s))
    (with-output-to-string (s a) (princ 4567 s))
    (with-output-to-string (s a)
      (princ 890 s))
    a)
  "1234567890")

(check-for-bug :iofkts-legacy-881
  (let ((a (make-array 10
                       :element-type 'character
                       :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s))
    (with-output-to-string (s a) (princ 4567 s))
    (with-output-to-string (s a)
      (princ 890 s))
    (if (adjustable-array-p a)
        (progn
          (with-output-to-string (s a)
            (princ (quote a)
                   s))
          (string= a
                   "1234567890A"))
        T))
  T
  "All 10 characters are up. But:
Body/m_w_out_.htm#with-output-to-string
...
If string is supplied, element-type is ignored, and the output is
incrementally appended to string as if by use of vector-push-extend.
...

See also Issues/iss365_w.htm

On the other hand: (Sam Steingold)

the example in the issue 365 clearly indicates that the error should be
signalled when the string is not adjustable.

so it could work, provided the string is adjustable...")

(check-for-bug :iofkts-legacy-908
  (setq a
        (make-array 10 :element-type 'character
                    :fill-pointer 0
                    :adjustable t))
  "")

(check-for-bug :iofkts-legacy-915
  (with-output-to-string (s a) (princ 123 s))
  123)

(check-for-bug :iofkts-legacy-919
  a
  "123")

(check-for-bug :iofkts-legacy-923
  (with-output-to-string (s a) (princ 4567 s))
  4567)

(check-for-bug :iofkts-legacy-927
  a
  "1234567")

(check-for-bug :iofkts-legacy-931
  (with-output-to-string (s a) (princ 890 s))
  890)

(check-for-bug :iofkts-legacy-935
  a
  "1234567890")

(check-for-bug :iofkts-legacy-939
  (with-output-to-string (s a) (princ (quote abcde) s))
  abcde)

(check-for-bug :iofkts-legacy-943
  a
  "1234567890ABCDE")

(check-for-bug :iofkts-legacy-947
  (with-output-to-string (s a) (princ (quote fghi) s))
  fghi)

(check-for-bug :iofkts-legacy-951
  a
  "1234567890ABCDEFGHI")

(makunbound 'bs)
(makunbound 'a)
(makunbound 'b)
(makunbound 'c)
(makunbound 'd)
(makunbound 'aa)
(makunbound 'string1)
(makunbound 'string2)
(makunbound 'x)
(makunbound 'j)
(makunbound 's1)
(makunbound 'str1)

(check-for-bug :iofkts-strange-symbol-names
  (let ((st (string (code-char 27))))
    (string= st (symbol-name (read-from-string
                              (prin1-to-string (make-symbol st))))))
  t)

