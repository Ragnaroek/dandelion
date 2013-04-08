;;; based on v1.6 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :loop-legacy-4
  (loop for x from 1 to 9
        for y = nil then x
        collect (list x y)
        )
  ((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9)))

(check-for-bug :loop-legacy-11
  (loop for x from 1 to 9
        and y = nil then x
        collect (list x y)
        )
  ((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8)))

(check-for-bug :loop-legacy-18
  (with-output-to-string (*standard-output*)
    (loop as i from 1 to 5
          do (print i)
          ) )
  "
1 
2 
3 
4 
5 ")

(check-for-bug :loop-legacy-30
  (with-output-to-string (*standard-output*)
    (loop for i from 10 downto 1 by 3
          do (print i)
          ) )
  "
10 
7 
4 
1 ")

(check-for-bug :loop-legacy-41
  (with-output-to-string (*standard-output*)
    (loop as i below 5
          do (print i)
          ) )
  "
0 
1 
2 
3 
4 ")

(check-for-bug :loop-legacy-53
  (with-output-to-string (*standard-output*)
    (loop for item in '(1 2 3 4 5)
          do (print item)
          ) )
  "
1 
2 
3 
4 
5 ")

(check-for-bug :loop-legacy-65
  (with-output-to-string (*standard-output*)
    (loop for item in '(1 2 3 4 5) by #'cddr
          do (print item)
          ) )
  "
1 
3 
5 ")

(check-for-bug :loop-legacy-75
  (loop for (item . x) (t . fixnum) in '((A . 1) (B . 2) (C . 3))
        unless (eq item 'B) sum x
        )
  4)

(check-for-bug :loop-legacy-81
  (loop for sublist on '(a b c d)
        collect sublist
        )
  ((A B C D) (B C D) (C D) (D)))

(check-for-bug :loop-legacy-87
  (with-output-to-string (*standard-output*)
    (loop for (item) on '(1 2 3)
          do (print item)
          ) )
  "
1 
2 
3 ")

(check-for-bug :loop-legacy-97
  (with-output-to-string (*standard-output*)
    (loop for item in '(1 2 3)
          do (print item)
          ) )
  "
1 
2 
3 ")

(check-for-bug :loop-legacy-107
  (loop for i below 5
        for j = 10 then i
        collect j)
  (10 1 2 3 4))

(check-for-bug :loop-legacy-114
  (loop for i below 5
        for j = i
        collect j
        )
  (0 1 2 3 4))

(check-for-bug :loop-legacy-121
  (loop for item = 1 then (+ item 10)
        repeat 5
        collect item
        )
  (1 11 21 31 41))

(check-for-bug :loop-legacy-128
  (loop for char across (the simple-string "Hello")
        collect char
        )
  (#\H #\e #\l #\l #\o))

(check-for-bug :loop-legacy-134
  (with-output-to-string (*standard-output*)
    (loop repeat 3
      do (write-line "What I say three times is true")
      ) )
  "What I say three times is true
What I say three times is true
What I say three times is true
")

(check-for-bug :loop-legacy-144
  (with-output-to-string (*standard-output*)
    (loop repeat -15
      do (write-line "What you see is what you expect")
      ) )
  "")

#|;; FOR clauses should come before WHILE clauses
(let ((stack '(a b c d e f)))
  (loop while stack
    for item = (length stack) then (pop stack)
    collect item
    ) )
(6 A B C D E F)
|#

(check-for-bug :loop-legacy-160
 (loop for i fixnum from 3
   when (oddp i) collect i
   while (< i 5)
   )
 (3 5))

(check-for-bug :loop-legacy-167
 (loop for i from 0 to 10
   always (< i 11)
   )
 T)

(check-for-bug :loop-legacy-173
 (loop for i from 0 to 10
   never (> i 11)
   )
 T)

(check-for-bug :loop-legacy-179
 (loop for i from 0
   thereis (when (> i 10) i)
   )
 11)

(check-for-bug :loop-legacy-185
 (with-output-to-string (*standard-output*)
			(loop for i from 0 to 10
			  always (< i 9)
			  finally (print "You won't see this")
			  ) )
 "")

(check-for-bug :loop-legacy-193
 (with-output-to-string (*standard-output*)
			(loop never t
			  finally (print "You won't see this")
			  ) )
 "")

(check-for-bug :loop-legacy-200
 (with-output-to-string (*standard-output*)
			(loop thereis "Here is my value"
			  finally (print "You won't see this")
			  ) )
 "")

(check-for-bug :loop-legacy-207
 (loop thereis "Here is my value"
   finally (print "You won't see this")
   )
 "Here is my value")

(check-for-bug :loop-legacy-213
 (with-output-to-string (*standard-output*)
			(loop for i from 1 to 10
			  thereis (> i 11)
			  finally (print i)
			  ) )
 "
11 ")

(check-for-bug :loop-legacy-222
 (let (everest chocorua sahara)
   (defstruct mountain  height difficulty (why "because it is there"))
   (setq everest (make-mountain :height '(2.86e-13 parsecs)))
   (setq chocorua (make-mountain :height '(1059180001 microns)))
   (defstruct desert  area (humidity 0))
   (setq sahara (make-desert :area '(212480000 square furlongs)))
   (loop for x in (list everest sahara chocorua)
     thereis (and (mountain-p x) (mountain-height x))
     ) )
 (2.86e-13 parsecs))

(check-for-bug :loop-legacy-234
 (with-output-to-string (*standard-output*)
			(loop for (month date-list) in '((january (24 28)) (february (17 29 12)))
			  do (loop for date in date-list
			       do (case date
				    (29 (when (eq month 'february) (loop-finish)))
				    )
			       do (format t "~:(~A~) ~A~%" month date)
			       ) )        )
 "January 24
January 28
February 17
")

(check-for-bug :loop-legacy-248
 (loop for i in '(1 2 3 stop-here 4 5 6)
   when (symbolp i) do (loop-finish)
   count i
   )
 3)

(check-for-bug :loop-legacy-255
 (loop for i in '(1 2 3 stop-here 4 5 6)
   until (symbolp i)
   count i
   )
 3)

(check-for-bug :loop-legacy-262
 (loop for name in '(fred sue alice joe june)
   for kids in '((bob ken) () () (kris sunshine) ())
   collect name
   append kids
   )
 (FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE))

(check-for-bug :loop-legacy-270
 (multiple-value-list
  (loop for name in '(fred sue alice joe june)
    as age in '(22 26 19 20 10)
    append (list name age) into name-and-age-list
    count name into name-count
    sum age into total-age
    finally
    (return (values (round total-age name-count) name-and-age-list))
    ) )
 (19 (FRED 22 SUE 26 ALICE 19 JOE 20 JUNE 10)))

(check-for-bug :loop-legacy-282
 (loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
   when (symbolp i) collect i
   )
 (BIRD TURTLE HORSE CAT))

(check-for-bug :loop-legacy-288
 (loop for i from 1 to 10
   if (oddp i) collect i
   )
 (1 3 5 7 9))

(check-for-bug :loop-legacy-294
 (with-output-to-string (*standard-output*)
			(loop for i in '(a b c d) by #'cddr
			  collect i into my-list
			  finally (print my-list)
			  ) )
 "
(A C) ")

(check-for-bug :loop-legacy-303
 (loop for x in '((a) (b) ((c)))
   append x
   )
 (A B (C)))

(check-for-bug :loop-legacy-309
 (loop for i upfrom 0
   as x in '(a b (c))
   nconc (if (evenp i) (list x) '())
   )
 (A (C)))

(check-for-bug :loop-legacy-316
 (loop for i in '(a b nil c nil d e)
   count i
   )
 5)

(check-for-bug :loop-legacy-322
 (loop for i fixnum in '(1 2 3 4 5)
   sum i
   )
 15)

(check-for-bug :loop-legacy-328
 (let ((series '(1.2 4.3 5.7)))
   (loop for v in series
     sum (* 2.0 v)
     ) )
 22.4)

(check-for-bug :loop-legacy-335
 (loop for i in '(2 1 5 3 4)
   maximize i
   )
 5)

(check-for-bug :loop-legacy-341
 (loop for i in '(2 1 5 3 4)
   minimize i
   )
 1)

(check-for-bug :loop-legacy-347
 (let ((series '(1.2 4.3 5.7)))
   (loop for v in series
     maximize (round v) fixnum
     ) )
 6)

(check-for-bug :loop-legacy-354
 (let ((series '(1.2 4.3 5.7)))
   (loop for v in series
     minimize (round v) into result fixnum
     finally (return result)
     ) )
 1)

(check-for-bug :loop-legacy-362
 (loop with a = 1
   with b = (+ a 2)
   with c = (+ b 3)
   with d = (+ c 4)
   return (list a b c d)
   )
 (1 3 6 10))

(check-for-bug :loop-legacy-371
 (loop with a = 1
   and b = 2
   and c = 3
   and d = 4
   return (list a b c d)
   )
 (1 2 3 4))

(check-for-bug :loop-legacy-380
 (let ((a 5) (b 10) (c 1729))
   (loop with a = 1
     and b = (+ a 2)
     and c = (+ b 3)
     and d = (+ c 4)
     return (list a b c d)
     ) )
 (1 7 13 1733))

(check-for-bug :loop-legacy-390
 (loop with (a b c) of-type (float integer float)
   return (format nil "~A ~A ~A" a b c)
   )
 "0.0 0 0.0")

(check-for-bug :loop-legacy-396
 (loop with (a b c) of-type float
   return (format nil "~A ~A ~A" a b c)
   )
 "0.0 0.0 0.0")

(check-for-bug :loop-legacy-402
 (let ((numbers-list '(3 2 4 6 1 7 8)) (results nil))
   (cons
    (with-output-to-string (*standard-output*)
			   (loop for i in numbers-list
			     when (oddp i)
			     do (print i)
			     and collect i into odd-numbers
			     and do (terpri)
			     else
			     collect i into even-numbers
			     finally (setq results (list odd-numbers even-numbers))
			     ) )
    results
    ) )
 ("
3 

1 

7 
"
  (3 1 7) (2 4 6 8)))

(check-for-bug :loop-legacy-426
 (loop for i in '(1 2 3 4 5 6)
   when (and (> i 3) i)
   collect it
   )
 (4 5 6))

(check-for-bug :loop-legacy-433
 (loop for i in '(1 2 3 4 5 6)
   when (and (> i 3) i)
   return it
   )
 4)

(check-for-bug :loop-legacy-440
 (loop for i in '(1 2 3 4 5 6)
   thereis (and (> i 3) i)
   )
 4)

(check-for-bug :loop-legacy-446
 (with-output-to-string (*standard-output*)
			(loop for x from 0 to 3
			  do (print x)
			  if (zerop (mod x 2))
			  do (write-string " a")
			  and
			  if (zerop (floor x 2))
			  do (write-string " b")
			  and
			  do (write-string " c")
			  ) )
 "
0  a b c
1 
2  a
3 ")

(check-for-bug :loop-legacy-464
 (with-output-to-string (*standard-output*)
			(loop for x from 0 to 3
			  do (print x)
			  if (zerop (mod x 2))
			  do (write-string " a")
			  and
			  if (zerop (floor x 2))
			  do (write-string " b")
			  end
			  and
			  do (write-string " c")
			  ) )
 "
0  a b c
1 
2  a c
3 ")

(check-for-bug :loop-legacy-483
 (with-output-to-string (*standard-output*)
			(loop for i from 1 to 5
			  do (print i)
			  ) )
 "
1 
2 
3 
4 
5 ")

(check-for-bug :loop-legacy-495
 (with-output-to-string (*standard-output*)
			(loop for i from 1 to 4
			  do (print i)
			  (print (* i i))
			  ) )
 "
1 
1 
2 
4 
3 
9 
4 
16 ")

(check-for-bug :loop-legacy-511
 (loop for item in '(1 2 3 a 4 5)
       when (not (numberp item))
       return (format nil "non-numeric value: ~S" item))
 "non-numeric value: A")

(check-for-bug :loop-legacy-517
 (loop for item in '(1 2 3 a 4 5)
       when (not (numberp item))
       do (return (format nil "non-numeric value: ~S" item)))
 "non-numeric value: A")

(check-for-bug :loop-legacy-523
 (loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
       for a of-type integer = (first numlist)
       for b of-type integer = (second numlist)
       for c of-type float = (third numlist)
       collect (list c b a))
 ((4.0 2 1) (8.3 6 5) (10.4 9 8)))

;; According to the BNF syntax, "and" must not be followed by "for". But
;; ANSI CL section 6.1.1.5.1 contains ambiguous wording, and this example
;; appears in CLtL2 p. 743, we keep it.
(check-for-bug :loop-legacy-534
 (loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
       for a of-type integer = (first numlist)
       and for b of-type integer = (second numlist)
       and for c of-type float = (third numlist)
       collect (list c b a))
 #-(OR CMU SBCL) ((4.0 2 1) (8.3 6 5) (10.4 9 8))
 #+(OR CMU SBCL) ERROR)

(check-for-bug :loop-legacy-543
 (loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
       for a of-type integer = (first numlist)
       and b of-type integer = (second numlist)
       and c of-type float = (third numlist)
       collect (list c b a))
 ((4.0 2 1) (8.3 6 5) (10.4 9 8)))

(check-for-bug :loop-legacy-551
 (loop for (a b c) of-type (integer integer float)
       in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
       collect (list c b a))
 ((4.0 2 1) (8.3 6 5) (10.4 9 8)))

(check-for-bug :loop-legacy-557
 (loop for (a b c) of-type float
       in '((1.0 2.0 4.0) (5.0 6.0 8.3) (8.0 9.0 10.4))
       collect (list c b a))
 ((4.0 2.0 1.0) (8.3 6.0 5.0) (10.4 9.0 8.0)))

(check-for-bug :loop-legacy-563
 (loop with (a b) of-type float = '(1.0 2.0)
       and (c d) of-type integer = '(3 4)
       and (e f)
       return (list a b c d e f))
 (1.0 2.0 3 4 NIL NIL))

(check-for-bug :loop-legacy-570
 (loop for (a nil b) = '(1 2 3)
       do (return (list a b)))
 (1 3))

(check-for-bug :loop-legacy-575
 (loop for (x . y) = '(1 . 2)
       do (return y))
 2)

(check-for-bug :loop-legacy-580
 (loop for ((a . b) (c . d)) of-type ((float . float) (integer . integer))
       in '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
       collect (list a b c d))
 ((1.2 2.4 3 4) (3.4 4.6 5 6)))

(check-for-bug :loop-legacy-586
 (loop for buffer in '("\"Hello\"" "\"unterminated" "nothing")
   collect
   (loop initially (unless (char= (char buffer 0) #\") (loop-finish))
     for i fixnum from 1 below (length buffer)
     when (char= (char buffer i) #\")
     return i
     )       )
 (6 NIL NIL))

(check-for-bug :loop-legacy-596
 (let (result)
   (list
    (with-output-to-string (*standard-output*)
			   (setq result
				 (loop for i from 1 to 10
				   when (> i 5)
				   collect i
				   finally (print i)
				   ) ) )
    result
    ) )
 ("
11 " (6 7 8 9 10)))

(check-for-bug :loop-legacy-611
 (multiple-value-list
  (loop for i from 1 to 10
    when (> i 5)
    collect i into number-list
    and count i into number-count
    finally (return (values number-count number-list))
    ) )
 (5 (6 7 8 9 10)))

(check-for-bug :loop-legacy-621
 (let (result)
   (list
    (with-output-to-string (*standard-output*)
			   (setq result
				 (loop named max
				   for i from 1 to 10
				   do (print i)
				   do (return-from max 'done)
				   ) ) )
    result
    ) )
 ("
1 " DONE))

;;; The following tests are not mandatory according to dpANS or ANSI CL,
;;; but that's how users expect the LOOP macro to work, so we check them.

(check-for-bug :loop-legacy-639
 (loop for i = 0
   for j to 2
   collect j
   )
 (0 1 2))

(check-for-bug :loop-legacy-646
 (loop for i in '(1 2)
   for j = i
   for k = j
   collect (list i j k)
   )
 ((1 1 1) (2 2 2)))

(check-for-bug :loop-legacy-654
 (loop for idx upfrom 0 below 5
   for char = (aref "Error" idx)
   collect char
   )
 (#\E #\r #\r #\o #\r))

(check-for-bug :loop-legacy-661
 (let ((hash-table (make-hash-table)))
   (setf (gethash 1 hash-table) 100)
   (setf (gethash 2 hash-table) 200)
   (sort
    (loop for key being each hash-key in hash-table using (hash-value val)
      for key+1 = (1+ key)
      collect (list key key+1 val))
    #'<
    :key #'car
    ) )
 ((1 2 100) (2 3 200)))

(check-for-bug :loop-legacy-674
 (loop for i across '#(1 2 3 4)
   for j = (1+ i)
   collect (list i j)
   )
 ((1 2) (2 3) (3 4) (4 5)))

(check-for-bug :loop-legacy-681
 (loop for i in '()
   for j = (1+ i)
   collect j
   )
 nil)

(check-for-bug :loop-legacy-688
 (loop for i across '#()
   for j = (1+ i)
   collect j
   )
 nil)

(check-for-bug :loop-legacy-695
 (loop for x = t
   for y in '(A B C)
   for z = t
   collect y
   )
 (A B C))

(check-for-bug :loop-legacy-703
 (loop for x = t
   for y across '#(A B C)
   for z = t
   collect y
   )
 (A B C))

(check-for-bug :loop-legacy-711
 (loop for x = t
   for y in ()
   for z = t
   collect y
   )
 nil)

(check-for-bug :loop-legacy-719
 (loop for x = t
   for y across '#()
   for z = t
   collect y
   )
 nil)

(check-for-bug :loop-legacy-727
 (let ((hash-table (make-hash-table)))
   (setf (gethash 1 hash-table) 100)
   (setf (gethash 2 hash-table) 200)
   (sort
    (loop for x = t
      for key being each hash-key in hash-table using (hash-value val)
      for key+1 = (1+ key)
      for z = t
      collect (list key key+1 val))
    #'<
    :key #'car
    ) )
 ((1 2 100) (2 3 200)))

(check-for-bug :loop-legacy-742
 (loop for i from 1 to 0
   collect i
   )
 nil)

(check-for-bug :loop-legacy-748
 (let ((hash-table (make-hash-table)))
   (setf (gethash 1 hash-table) 100)
   (setf (gethash 2 hash-table) 200)
   (sort
    (loop for val being each hash-value in hash-table
      collect val)
    #'<
    ) )
 (100 200))

(check-for-bug :loop-legacy-759
 (let ((hash-table (make-hash-table)))
   (setf (gethash 1 hash-table) 100)
   (setf (gethash 2 hash-table) 200)
   (sort
    (loop for val being each hash-value in hash-table
      for deriv-val = (/ 1 val)
      collect deriv-val)
    #'<
    ) )
 (1/200 1/100))

(check-for-bug :loop-legacy-771
 (let ((hash-table (make-hash-table)))
   (setq i 123456789)
   (setf (gethash 1 hash-table) 100)
   (setf (gethash 2 hash-table) 200)
   (loop for i across '#(1 2 3 4 5 6)
     collect i)
   (loop for i in '(1 2 3 4 5 6)
     collect i)
   (loop for i being each hash-key of hash-table
     collect i)
   (loop for i being each present-symbol of *package*
     collect i)
   i
   )
 123456789)

(check-for-bug :loop-legacy-788
 (loop for x on '(3 4 5)
   for y = (car x)
   for z in '(a b c)
   collect z
   )
 (a b c))

(check-for-bug :loop-legacy-796
 (loop for x across '#(3 4 5)
   for y = (1+ x)
   for z across '#(a b c)
   collect (list x y z)
   )
 ((3 4 a) (4 5 b) (5 6 c)))

(check-for-bug :loop-legacy-804
 (loop for x across '#()
   for y = x
   for z across '#(a b c)
   collect (list x y z)
   )
 nil)

(check-for-bug :loop-legacy-812
 (loop for x across '#(1 2 3)
   for y = x
   for z across '#()
   collect (list x y z)
   )
 nil)

(check-for-bug :loop-legacy-820
 (loop for x across '#(1 2 3)
   for y = (1+ x)
   for z across '#(a b)
   collect (list x y z)
   )
 ((1 2 a) (2 3 b)))

(check-for-bug :loop-legacy-828
 (loop for x across '#(1 2)
   for y = (1+ x)
   for z across '#(a b c)
   collect (list x y z)
   )
 ((1 2 a) (2 3 b)))

(check-for-bug :loop-legacy-836
 (let ((package (make-package "LOOP-TEST")))
   (intern "blah" package)
   (let ((blah2 (intern "blah2" package)))
     (export blah2 package)
     )
   (list
    (sort
     (loop for sym being each present-symbol of package
       for sym-name = (symbol-name sym)
       collect sym-name
       )
     #'string<
     )
    (sort
     (loop for sym being each external-symbol of package
       for sym-name = (symbol-name sym)
       collect sym-name
       )
     #'string<
     ) ) )
 (("blah" "blah2") ("blah2")))

(check-for-bug :loop-legacy-859
 (let ((ht (make-hash-table)))
   (loop for key being each hash-key of ht
     for value = (gethash key ht)
     collect (list key value)
     ) )
 nil)

(check-for-bug :loop-legacy-867
 (let ((ht (make-hash-table)))
   (loop for dummy = (+ 1 2)
     for key being each hash-key of ht
     collect (list key)
     ) )
 nil)

;;; Three more tests, found by Russell Senior.
;;; They are justified by ANSI CL 6.1.1.4 and 6.1.2.1.5.

(check-for-bug :loop-legacy-878
 (let ((list '(1 2 3)))
   (loop for x in list
     and y = nil then x
     collect (list x y)))
 ((1 NIL) (2 1) (3 2)))

(check-for-bug :loop-legacy-885
 (let ((list '(1 2 3)))
   (loop for x in list
     for y = nil then x
     collect (list x y)))
 ((1 NIL) (2 2) (3 3)))

(check-for-bug :loop-legacy-892
 (let ((list '(1 2 3)))
   (loop for x in list
     for y = nil then x
     and z = nil then y
     collect (list x y z)))
 ((1 NIL NIL) (2 2 NIL) (3 3 2)))

;;; One more test, found by Lennart Staflin.

(check-for-bug :loop-legacy-902
 (loop repeat 4 for x = (+ 1 1) collect x)
 (2 2 2 2))

;;; Tests from ANSI CL section 6.1.2.1.1.

(check-for-bug :loop-legacy-908
 (let ((x 1)) (loop for i from x by (incf x) to 10 collect i))
 (1 3 5 7 9))

(check-for-bug :loop-legacy-912
  (let ((x 1))
    (loop for i by (incf x) from x to 10
          collect i))
  (2 4 6 8 10)
 "This should be the same as:
(let* ((x 1)
       (y (incf x)))
  (loop for i from x to 10 by y
        collect i))

Because left-to-right evalution is preserved.
it is legal to have by first:
arithmetic-up::= [[{from | upfrom} form1 |   {to | upto | below} form2 |   by form3]]+ ")

(check-for-bug :loop-legacy-920
 (loop for i from 1 to 5
       collect i
       into c collect (copy-list c))
 ((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)))

(check-for-bug :loop-added-1
 (let ((rem 55))
   (loop for i below 3
         with num = (* 10 rem)
           and rem
           collect rem))
 (nil nil nil))

;; Clean up.
(check-for-bug :loop-legacy-925
 (progn (delete-package "LOOP-TEST") t)
 T)

