;;; section 14 conses -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))
;;; cons

(check-for-bug :section14-legacy-7
  (cons 1 2)
  (1 . 2))

(check-for-bug :section14-legacy-11
  (cons 1 nil)
  (1))

(check-for-bug :section14-legacy-15
  (cons nil 2)
  (NIL . 2))

(check-for-bug :section14-legacy-19
  (cons nil nil)
  (NIL))

(check-for-bug :section14-legacy-23
  (cons 1 (cons 2 (cons 3 (cons 4 nil))))
  (1 2 3 4))

(check-for-bug :section14-legacy-27
  (cons 'a 'b)
  (A . B))

(check-for-bug :section14-legacy-31
  (cons 'a (cons 'b (cons 'c '())))
  (A B C))

(check-for-bug :section14-legacy-35
  (cons 'a (list 'b 'c 'd))
  (A B C D))


;;; consp

(check-for-bug :section14-legacy-42
  (consp nil)
  nil)

(check-for-bug :section14-legacy-46
  (consp (cons 1 2))
  t)

(check-for-bug :section14-legacy-50
  (consp '())
  nil)

(check-for-bug :section14-legacy-54
  (consp 'nil)
  nil)

;;; atom

(check-for-bug :section14-legacy-60
  (atom 'sss)
  t)

(check-for-bug :section14-legacy-64
  (atom (cons 1 2))
  nil)

(check-for-bug :section14-legacy-68
  (atom nil)
  t)

(check-for-bug :section14-legacy-72
  (atom '())
  t)

(check-for-bug :section14-legacy-76
  (atom 3)
  t)

;;; rplaca

(check-for-bug :section14-legacy-82
  (defparameter *some-list* (list* 'one 'two 'three 'four))
  *some-list*)

(check-for-bug :section14-legacy-86
  *some-list*
  (ONE TWO THREE . FOUR))

(check-for-bug :section14-legacy-90
  (rplaca *some-list* 'uno)
  (UNO TWO THREE . FOUR))

(check-for-bug :section14-legacy-94
  *some-list*
  (UNO TWO THREE . FOUR))

(check-for-bug :section14-legacy-98
  (rplacd (last *some-list*) (list 'IV))
  (THREE IV))

(check-for-bug :section14-legacy-102
  *some-list*
  (UNO TWO THREE IV))

;;; copy-tree

(check-for-bug :section14-legacy-108
  (let* ((object (list (cons 1 "one")
                       (cons 2 (list 'a 'b 'c))))
         (object-too object)
         (copy-as-list (copy-list object))
         (copy-as-alist (copy-alist object))
         (copy-as-tree (copy-tree object)))
    (list
     (eq object object-too);; T
     (eq copy-as-tree object);; NIL
     (eql copy-as-tree object);; NIL
     (equal copy-as-tree object);; T
     )
    )
  (t nil nil t))

(check-for-bug :section14-legacy-124
  (let* ((object (list (cons 1 "one")
		       (cons 2 (list 'a 'b 'c)))))

    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1)))
  (ONE . 1))

(check-for-bug :section14-legacy-133
  (let* ((object (list (cons 1 "one")
		       (cons 2 (list 'a 'b 'c)))))

    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    object)
  ((ONE . 1) ("two" "a" B C)))

(check-for-bug :section14-legacy-143
  (let* ((object (list (cons 1 "one")
                       (cons 2 (list 'a 'b 'c))))
	 (object-too object))
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    object-too)
  ((ONE . 1) ("two" "a" B C)))

(check-for-bug :section14-legacy-153
  (let* ((object (list (cons 1 "one")
                       (cons 2 (list 'a 'b 'c))))
         (copy-as-list (copy-list object)))
    (setf (first (cdr (second object))) "a"
          (car (second object)) "two"
          (car object) (cons 'one  1))
    copy-as-list)
  ((1 . "one") ("two" "a" B C)))

(check-for-bug :section14-legacy-163
  (let* ((object (list (cons 1 "one")
                       (cons 2 (list 'a 'b 'c))))
         (copy-as-alist (copy-alist object)))
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    copy-as-alist)
  ((1 . "one") (2 "a" B C)))

(check-for-bug :section14-legacy-173
  (let* ((object (list (cons 1 "one")
                       (cons 2 (list 'a 'b 'c))))
         (copy-as-tree (copy-tree object)))
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    copy-as-tree)
  ((1 . "one") (2 A B C)) )

;;; sublis

(check-for-bug :section14-legacy-185
  (sublis (list (cons 'x  100)
                (cons 'z 'zprime))
          (append (list 'plus 'x
                        (list 'minus 'g 'z 'x 'p)
                        4)
                  'x))
  (PLUS 100 (MINUS G ZPRIME 100 P) 4 . 100))

(check-for-bug :section14-legacy-194
  (sublis (list (cons (list '+ 'x 'y)
                      (list '- 'x 'y))
                (cons (list '- 'x 'y)
                      (list '+ 'x 'y)))
          (list '*
                (list '/
                      (list '+ 'x 'y)
                      (list '+ 'x 'p))
                (list '- 'x 'y))
          :test #'equal)
  (* (/ (- X Y) (+ X P)) (+ X Y)))

(check-for-bug :section14-legacy-207
  (let ((tree1 (list 1
                     (list 1 2)
                     (list (list 1 2 3))
                     (list (list (list 1 2 3 4))))))
    tree1)
  (1 (1 2) ((1 2 3)) (((1 2 3 4)))))

(check-for-bug :section14-legacy-215
  (let ((tree1 (list 1
                     (list 1 2)
                     (list (list 1 2 3))
                     (list (list (list 1 2 3 4))))))
    (sublis (list (cons 3 "three"))
	    tree1))
  (1 (1 2) ((1 2 "three")) (((1 2 "three" 4)))))

(check-for-bug :section14-legacy-224
  (let ((tree1 (list 1
                     (list 1 2)
                     (list (list 1 2 3))
                     (list (list (list 1 2 3 4))))))
    (sublis (list (cons t  "string"))
            (sublis (list (cons 1 "") (cons 4 44))
                    tree1)
            :key #'stringp))
  ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44)))))

(check-for-bug :section14-legacy-235
  (let ((tree1 (list 1
                     (list 1 2)
                     (list (list 1 2 3))
                     (list (list (list 1 2 3 4))))))
    (sublis (list (cons 3 "three"))
	    tree1)
    (sublis (list (cons t  "string"))
	    (sublis (list (cons 1 "") (cons 4 44))
		    tree1)
	    :key #'stringp)
    tree1)
  (1 (1 2) ((1 2 3)) (((1 2 3 4)))))

(check-for-bug :section14-legacy-249
  (let ((tree2 (list "one" (list "one" "two")
                     (list (list "one" "Two" "three")))))
    tree2)
  ("one" ("one" "two") (("one" "Two" "three"))) )

(check-for-bug :section14-legacy-255
  (let ((tree2 (list (string "one")
                     (list (string "one")
                           (string "two"))
                     (list (list (string "one")
                                 (string "Two")
                                 (string "three")
                                 )))))
    (sublis (list (cons (copy-seq "two")
                        2))
            tree2))
  ("one" ("one" "two") (("one" "Two" "three"))))

(check-for-bug :section14-legacy-268
  (let ((tree2 (list (string "one")
		     (list (string "one")
			   (string "two")
			   )
                     (list (list (string "one")
                                 (string "Two")
                                 (string "three")
                                 )))))
    (sublis (list (cons (string "two")
                        2))
            tree2)
    tree2)
  ("one" ("one" "two") (("one" "Two" "three"))) )

(check-for-bug :section14-legacy-283
  (let ((tree2 (list "one" (list "one" "two")
                     (list (list "one" "Two" "three")))))
    (sublis (list (cons "two" 2)) tree2 :test 'equal))
  ("one" ("one" 2) (("one" "Two" "three"))) )

(check-for-bug :section14-legacy-289
  (let ((tree1 (list 1
                     (list 1 2)
                     (list (list 1 2 3))
                     (list (list (list 1 2 3 4))))))
    (nsublis (list (cons t '(quote temp)))
             tree1
             :key #'(lambda (x) (or (atom x) (< (list-length x) 3)))))
  ((QUOTE TEMP) (QUOTE TEMP) QUOTE TEMP) )

;;; subst

(check-for-bug :section14-legacy-301
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    tree1)
  (1 (1 2) (1 2 3) (1 2 3 4)))

(check-for-bug :section14-legacy-306
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst "two" 2 tree1))
  (1 (1 "two") (1 "two" 3) (1 "two" 3 4)))

(check-for-bug :section14-legacy-311
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst "five" 5 tree1))
  (1 (1 2) (1 2 3) (1 2 3 4)))

(check-for-bug :section14-legacy-316
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (eq tree1 (subst "five" 5 tree1)))
  #+(or sbcl cmu sbcl clisp) T
  #+ecls nil
  #-(or sbcl cmu sbcl clisp ecls) fill-this-in)

(check-for-bug :section14-legacy-323
  (subst 'tempest 'hurricane
         (list 'shakespeare 'wrote (list 'the 'hurricane)))
  (SHAKESPEARE WROTE (THE TEMPEST)))

(check-for-bug :section14-legacy-328
  (subst 'foo 'nil (list 'shakespeare 'wrote (list 'twelfth 'night)))
  (SHAKESPEARE WROTE (TWELFTH NIGHT . FOO) . FOO))

(check-for-bug :section14-legacy-332
  (subst (cons 'a 'cons)
         (cons 'old 'pair)
         (list (cons 'old 'spice)
               (append (list (cons 'old 'shoes) 'old)
                       'pair)
               (cons 'old 'pair))
         :test #'equal)
  ((OLD . SPICE) ((OLD . SHOES) A . CONS) (A . CONS)))

(check-for-bug :section14-legacy-342
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst-if 5 #'listp tree1))
  5)

(subst-if-not (list 'x)
	      #'consp
	      (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4)))

(check-for-bug :section14-legacy-351
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst-if-not (list 'x) #'consp tree1))
					;(1 (X))
  ((X) ((X) (X) X) ((X) (X) (X) X) ((X) (X) (X) (X) X) X))

(check-for-bug :section14-legacy-357
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst-if-not (list 'x) #'consp tree1)
    tree1)
  (1 (1 2) (1 2 3) (1 2 3 4)))

(check-for-bug :section14-legacy-363
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (nsubst 'x 3 tree1 :key #'(lambda (y) (and (listp y) (third y)))))
  (1 (1 2) X X))

(check-for-bug :section14-legacy-368
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (nsubst 'x 3 tree1 :key #'(lambda (y) (and (listp y) (third y))))
    tree1)
  (1 (1 2) X X))

;;; tree-equal

(check-for-bug :section14-legacy-376
  (let ((tree1 (list 1 (list 1 2)))
        (tree2 (list 1 (list 1 2))))
    tree2)
  (1 (1 2)))

(check-for-bug :section14-legacy-382
  (let ((tree1 (list 1 (list 1 2)))
        (tree2 (list 1 (list 1 2))))
    (tree-equal tree1 tree2))
  t)

(check-for-bug :section14-legacy-388
  (let ((tree1 (list 1 (list 1 2)))
        (tree2 (list 1 (list 1 2))))
    (eql tree1 tree2))
  nil)

(check-for-bug :section14-legacy-394
  (let ((tree1 (list ''a (list ''b ''c)))
        (tree2 (list ''a (list ''b ''c))))
    tree2)
  ('a ('b 'c)) )

(check-for-bug :section14-legacy-400
  (let ((tree1 (list ''a (list ''b ''c)))
        (tree2 (list ''a (list ''b ''c))))
    (tree-equal tree1 tree2 :test 'eq))
  t)

;;; copy-list

(check-for-bug :section14-legacy-408
  (let ((lst (list 1 (list 2 3))))
    lst)
  (1 (2 3)))

(check-for-bug :section14-legacy-413
  (let* ((lst (list 1 (list 2 3)))
         (slst lst))
    slst)
  (1 (2 3)))

(check-for-bug :section14-legacy-419
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    clst)
  (1 (2 3)))

(check-for-bug :section14-legacy-426
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (eq slst lst))
  t)

(check-for-bug :section14-legacy-433
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (eq clst lst))
  nil)

(check-for-bug :section14-legacy-440
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (equal clst lst))
  t)

(check-for-bug :section14-legacy-447
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (rplaca lst "one"))
  ("one" (2 3)))

(check-for-bug :section14-legacy-454
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (rplaca lst "one")
    slst)
  ("one" (2 3)))

(check-for-bug :section14-legacy-462
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (rplaca lst "one")
    clst)
  (1 (2 3)))

(check-for-bug :section14-legacy-470
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (rplaca lst "one")
    (setf (caadr lst) "two"))
  "two")

(check-for-bug :section14-legacy-478
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (rplaca lst "one")
    (setf (caadr lst) "two")
    lst)
  ("one" ("two" 3)))

(check-for-bug :section14-legacy-487
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (rplaca lst "one")
    (setf (caadr lst) "two")
    slst)
  ("one" ("two" 3)))

(check-for-bug :section14-legacy-496
  (let* ((lst (list 1 (list 2 3)))
         (slst lst)
         (clst (copy-list lst)))
    (rplaca lst "one")
    (setf (caadr lst) "two")
    clst)
  (1 ("two" 3)))

;;; list list*

(check-for-bug :section14-legacy-507
  (list 1)
  (1))

(check-for-bug :section14-legacy-511
  (list* 1)
  1)

(check-for-bug :section14-legacy-515
  (let (( a 1))
    a)
  1)

(check-for-bug :section14-legacy-520
  (let (( a 1))
    (list a 2))
  (1 2))

(check-for-bug :section14-legacy-525
  (let (( a 1))
    (list 'a '2))
  (A 2))

(check-for-bug :section14-legacy-530
  (let (( a 1))
    (list 'a 2))
  (A 2))

(check-for-bug :section14-legacy-535
  (let (( a 1))
    (list* a 2))
  (1 . 2))

(check-for-bug :section14-legacy-540
  (list)
  NIL)

(check-for-bug :section14-legacy-544
  (let ((a (list 1 2)))
    a)
  (1 2))

(check-for-bug :section14-legacy-549
  (let ((a (list 1 2)))
    (eq a (list* a)))
  t)

(check-for-bug :section14-legacy-554
  (let ((a (list 1 2)))
    (list 3 4 'a (car (cons 'b 'c)) (+ 6 -2)))
  (3 4 A B 4))

(check-for-bug :section14-legacy-559
  (let ((a (list 1 2)))
    (list* 'a 'b 'c 'd))
  (A B C . D))

(check-for-bug :section14-legacy-564
  (cons 'a (cons 'b (cons 'c 'd)))
  (A B C . D))

(check-for-bug :section14-legacy-568
  (list* 'a 'b 'c (list 'd 'e 'f))
  (A B C D E F))

;;; list-length

(check-for-bug :section14-legacy-574
  (list-length (list 'a 'b 'c 'd))
  4)

(check-for-bug :section14-legacy-578
  (list-length (list 'a (list 'b 'c) 'd))
  3)

(check-for-bug :section14-legacy-582
  (list-length '())
  0)

(check-for-bug :section14-legacy-586
  (list-length nil)
  0)

(check-for-bug :section14-legacy-590
  (defun circular-list (&rest elements)
    (let ((cycle (copy-list elements)))
      (nconc cycle cycle)))
  CIRCULAR-LIST)

(check-for-bug :section14-legacy-596
  (list-length (circular-list 'a 'b))
  NIL)

(check-for-bug :section14-legacy-600
  (list-length (circular-list 'a))
  NIL)

(check-for-bug :section14-legacy-604
  (list-length (circular-list))
  0)

;;; listp

(check-for-bug :section14-legacy-610
  (listp nil)
  t)

(check-for-bug :section14-legacy-614
  (listp (cons 1 2))
  t)

(check-for-bug :section14-legacy-618
  (listp (make-array 6))
  nil)

(check-for-bug :section14-legacy-622
  (listp t)
  nil)

;;; make-list

(check-for-bug :section14-legacy-628
  (make-list 5)
  (NIL NIL NIL NIL NIL))

(check-for-bug :section14-legacy-632
  (make-list 3 :initial-element 'rah)
  (RAH RAH RAH))

(check-for-bug :section14-legacy-636
  (make-list 2 :initial-element (list 1 2 3))
  ((1 2 3) (1 2 3)))

(check-for-bug :section14-legacy-640
  (make-list 0)
  NIL)					;i.e.,  ())

(check-for-bug :section14-legacy-644
  (make-list 0 :initial-element 'new-element)
  NIL )

;;; push

(check-for-bug :section14-legacy-650
  (let ((llst (list nil)))
    llst)
  (NIL))

(check-for-bug :section14-legacy-655
  (let ((llst (list nil)))
    (push 1 (car llst)))
  (1))

(check-for-bug :section14-legacy-660
  (let ((llst (list nil)))
    (push 1 (car llst))
    llst)
  ((1)))

(check-for-bug :section14-legacy-666
  (let ((llst (list nil)))
    (push 1 (car llst))
    (push 1 (car llst)))
  (1 1))

(check-for-bug :section14-legacy-672
  (let ((llst (list nil)))
    (push 1 (car llst))
    (push 1 (car llst))
    llst)
  ((1 1)))

(check-for-bug :section14-legacy-679
  (let ((x (list 'a
		 (list 'b 'c)
		 'd)))
    x)
  (A (B C) D))

(check-for-bug :section14-legacy-686
  (let ((x (list 'a
                 (list 'b 'c)
                 'd)))
    (push 5 (cadr x)))
  (5 B C)  )

(check-for-bug :section14-legacy-693
  (let ((x (list 'a
                 (list 'b 'c)
                 'd)))
    (push 5 (cadr x))
    x)
  (A (5 B C) D))

;;; pop

(check-for-bug :section14-legacy-703
  (let ((stack (list 'a 'b 'c)))
    stack)
  (A B C))

(check-for-bug :section14-legacy-708
  (let ((stack (list 'a 'b 'c)))
    (pop stack))
  A)

(check-for-bug :section14-legacy-713
  (let ((stack (list 'a 'b 'c)))
    (pop stack)
    stack)
  (B C))

(check-for-bug :section14-legacy-719
  (let ((llst (list (list 1 2 3 4))))
    llst)
  ((1 2 3 4)))

(check-for-bug :section14-legacy-724
  (let ((llst (list (list 1 2 3 4))))
    (pop (car llst)))
  1)

(check-for-bug :section14-legacy-729
  (let ((llst (list (list 1 2 3 4))))
    (pop (car llst))
    llst)
  ((2 3 4)))

;;; nth

(check-for-bug :section14-legacy-737
  (nth 0 (list 'foo 'bar 'baz))
  FOO)

(check-for-bug :section14-legacy-741
  (nth 1 (list 'foo 'bar 'baz))
  BAR)

(check-for-bug :section14-legacy-745
  (nth 3 (list 'foo 'bar 'baz))
  NIL)

(check-for-bug :section14-legacy-749
  (let ((0-to-3 (list 0 1 2 3)))
    0-to-3)
  (0 1 2 3))

(check-for-bug :section14-legacy-754
  (let ((0-to-3 (list 0 1 2 3)))
    (setf (nth 2 0-to-3) "two"))
  "two")

(check-for-bug :section14-legacy-759
  (let ((0-to-3 (list 0 1 2 3)))
    (setf (nth 2 0-to-3) "two")
    0-to-3)
  (0 1 "two" 3))

;;; endp

(check-for-bug :section14-legacy-767
  (endp nil)
  t)

(check-for-bug :section14-legacy-771
  (endp (list 1 2))
  nil)

(check-for-bug :section14-legacy-775
  (endp (cddr (list 1 2)))
  t)

;;; null

(check-for-bug :section14-legacy-781
  (null '())
  T)

(check-for-bug :section14-legacy-785
  (null nil)
  T)

(check-for-bug :section14-legacy-789
  (null t)
  NIL)

(check-for-bug :section14-legacy-793
  (null 1)
  NIL)

;;; nconc

(check-for-bug :section14-legacy-799
  (nconc)
  NIL)

(check-for-bug :section14-legacy-803
  (setq x (list 'a 'b 'c))
  (A B C))

(check-for-bug :section14-legacy-807
  (setq y (list 'd 'e 'f))
  (D E F))

(check-for-bug :section14-legacy-811
  (nconc x y)
  (A B C D E F))

(check-for-bug :section14-legacy-815
  x
  (A B C D E F))

(check-for-bug :section14-legacy-819
  (setq foo (list 'a 'b 'c 'd 'e)
        bar (list 'f 'g 'h 'i 'j)
        baz (list 'k 'l 'm))
  (K L M))

(check-for-bug :section14-legacy-825
  (setq foo (nconc foo bar baz))
  (A B C D E F G H I J K L M))

(check-for-bug :section14-legacy-829
  foo
  (A B C D E F G H I J K L M))

(check-for-bug :section14-legacy-833
  bar
  (F G H I J K L M))

(check-for-bug :section14-legacy-837
  baz
  (K L M))

(check-for-bug :section14-legacy-841
  (setq foo (list 'a 'b 'c 'd 'e)
        bar (list 'f 'g 'h 'i 'j)
        baz (list 'k 'l 'm))
  (K L M))

(check-for-bug :section14-legacy-847
  (setq foo (nconc nil foo bar nil baz))
  (A B C D E F G H I J K L M) )

(check-for-bug :section14-legacy-851
  foo
  (A B C D E F G H I J K L M))

(check-for-bug :section14-legacy-855
  bar
  (F G H I J K L M))

(check-for-bug :section14-legacy-859
  baz
  (K L M))

;;; append

(check-for-bug :section14-legacy-865
  (append (list 'a 'b 'c)
          (list 'd 'e 'f)
          '()
          (list 'g))
  (A B C D E F G))

(check-for-bug :section14-legacy-872
  (append (list 'a 'b 'c)
          'd)
  (A B C . D))

(check-for-bug :section14-legacy-877
  (setq lst (list 'a 'b 'c))
  (A B C))

(check-for-bug :section14-legacy-881
  (append lst (list 'd))
  (A B C D))

(check-for-bug :section14-legacy-885
  lst
  (A B C))

(check-for-bug :section14-legacy-889
  (append)
  NIL)

(check-for-bug :section14-legacy-893
  (append 'a)
  A)

;;; revappend

(check-for-bug :section14-legacy-899
  (let ((list-1 (list 1 2 3))
        (list-2 (list 'a 'b 'c)))
    (list (revappend list-1 list-2)
          (equal list-1 (list 1 2 3))
          (equal list-2 (list 'a 'b 'c))))
  ((3 2 1 A B C)  T  T))


(check-for-bug :section14-legacy-908
  (revappend (list 1 2 3) '())
  (3 2 1))

(check-for-bug :section14-legacy-912
  (revappend (list 1 2 3)
             (cons 'a 'b))
  (3 2 1 A . B))

(check-for-bug :section14-legacy-917
  (revappend '()
             (list 'a 'b 'c))
  (A B C))

(check-for-bug :section14-legacy-922
  (revappend (list 1 2 3)
             'a)
  (3 2 1 . A))

(check-for-bug :section14-legacy-927
  (revappend '() 'a)
  A )					;degenerate case)

(check-for-bug :section14-legacy-931
  (let ((list-1 (copy-list (list 1 2 3)))
        (list-2 (list 'a 'b 'c)))
    (list (nreconc list-1 list-2)
          (equal list-1 (list 1 2 3))
          (equal list-2 (list 'a 'b 'c))))
  ((3 2 1 A B C) NIL T))


;;; butlast

(check-for-bug :section14-legacy-942
  (setq lst (list 1 2 3 4 5 6 7 8 9))
  (1 2 3 4 5 6 7 8 9))

(check-for-bug :section14-legacy-946
  (butlast lst)
  (1 2 3 4 5 6 7 8))

(check-for-bug :section14-legacy-950
  (butlast lst 5)
  (1 2 3 4))

(check-for-bug :section14-legacy-954
  (butlast lst (+ 5 5))
  NIL)

(check-for-bug :section14-legacy-958
  lst
  (1 2 3 4 5 6 7 8 9))

(check-for-bug :section14-legacy-962
  (nbutlast lst 3)
  (1 2 3 4 5 6))

(check-for-bug :section14-legacy-966
  lst
  (1 2 3 4 5 6))

(check-for-bug :section14-legacy-970
  (nbutlast lst 99)
  NIL)

(check-for-bug :section14-legacy-974
  lst
  (1 2 3 4 5 6))

(check-for-bug :section14-legacy-978
  (butlast (list 'a 'b 'c 'd))
  (A B C))

(check-for-bug :section14-legacy-982
  (butlast (list (list 'a 'b)
                 (list 'c 'd)))
  ((A B)))

(check-for-bug :section14-legacy-987
  (butlast (list 'a))
  NIL)

(check-for-bug :section14-legacy-991
  (butlast nil)
  NIL)

(check-for-bug :section14-legacy-995
  (setq foo (list 'a 'b 'c 'd))
  (A B C D))

(check-for-bug :section14-legacy-999
  (nbutlast foo)
  (A B C))

(check-for-bug :section14-legacy-1003
  foo
  (A B C))

(check-for-bug :section14-legacy-1007
  (nbutlast (list 'a))
  NIL)

(check-for-bug :section14-legacy-1011
  (nbutlast '())
  NIL)

;;; last

(check-for-bug :section14-legacy-1017
  (last nil)
  NIL)

(check-for-bug :section14-legacy-1021
  (last (list 1 2 3))
  (3))

(check-for-bug :section14-legacy-1025
  (last (append (list 1 2)
                3))
  (2 . 3))

(check-for-bug :section14-legacy-1030
  (setq x (list 'a 'b 'c 'd))
  (A B C D))

(check-for-bug :section14-legacy-1034
  (last x)
  (D))

(check-for-bug :section14-legacy-1038
  (progn
    (rplacd (last x) (list 'e 'f))
    t)
  t)

(check-for-bug :section14-legacy-1044
  x
  (A B C D E F))

(check-for-bug :section14-legacy-1048
  (last x)
  (F))

(check-for-bug :section14-legacy-1052
  (last (list 'a 'b 'c))
  (C))

(check-for-bug :section14-legacy-1056
  (last (list 'a 'b 'c) 0)
  ())

(check-for-bug :section14-legacy-1060
  (last (list 'a 'b 'c) 1)
  (C))

(check-for-bug :section14-legacy-1064
  (last (list 'a 'b 'c) 2)
  (B C))

(check-for-bug :section14-legacy-1068
  (last (list 'a 'b 'c) 3)
  (A B C))

(check-for-bug :section14-legacy-1072
  (last (list 'a 'b 'c) 4)
  (A B C))

(check-for-bug :section14-legacy-1076
  (last (cons 'a 'b) 0)
  B)

(check-for-bug :section14-legacy-1080
  (last (cons 'a 'b) 1)
  (A . B))

(check-for-bug :section14-legacy-1084
  (last (cons 'a 'b) 2)
  (A . B))

;;; nthcdr

(check-for-bug :section14-legacy-1090
  (nthcdr 0 '())
  NIL)

(check-for-bug :section14-legacy-1094
  (nthcdr 3 '())
  NIL)

(check-for-bug :section14-legacy-1098
  (nthcdr 0 (list 'a 'b 'c))
  (A B C))

(check-for-bug :section14-legacy-1102
  (nthcdr 2 (list 'a 'b 'c))
  (C))

(check-for-bug :section14-legacy-1106
  (nthcdr 4 (list 'a 'b 'c))
  ())

(check-for-bug :section14-legacy-1110
  (nthcdr 1 (cons 0 1))
  1)

(check-for-bug :section14-legacy-1114
  (locally (declare (optimize (safety 3)))
    (nthcdr 3 (cons 0 1)))
  TYPE-ERROR)

;;; rest

(check-for-bug :section14-legacy-1121
  (rest (list 1 2))
  (2))

(check-for-bug :section14-legacy-1125
  (rest (cons 1 2))
  2)

(check-for-bug :section14-legacy-1129
  (rest (list 1))
  NIL)

(check-for-bug :section14-legacy-1133
  (setq *cons* (cons 1 2))
  (1 . 2))

(check-for-bug :section14-legacy-1137
  (setf (rest *cons*) "two")
  "two")

(check-for-bug :section14-legacy-1141
  *cons*
  (1 . "two"))

;;; member

(check-for-bug :section14-legacy-1147
  (member 2 (list 1 2 3))
  (2 3))

(check-for-bug :section14-legacy-1151
  (member 2 (list (cons 1 2)
                  (cons 3 4))
          :test-not #'=
          :key #'cdr)
  ((3 . 4)))

(check-for-bug :section14-legacy-1158
  (member 'e (list 'a 'b 'c 'd))
  NIL)

(check-for-bug :section14-legacy-1162
  (member-if #'listp (list 'a 'b nil 'c 'd))
  (NIL C D))

(check-for-bug :section14-legacy-1166
  (member-if #'numberp (list 'a #\Space 5/3 'foo))
  (5/3 FOO))

(check-for-bug :section14-legacy-1170
  (member-if-not #'zerop
                 (append (list 3 6 9 11)
                         12)
                 :key #'(lambda (x) (mod x 3)))
  (11 . 12))

;;; mapc and co

(check-for-bug :section14-legacy-1179
  (mapcar #'car (list (list 1 'a)
                      (list 2 'b)
                      (list 3 'c)))
  (1 2 3) )

(check-for-bug :section14-legacy-1185
  (mapcar #'abs (list 3 -4 2 -5 -6))
  (3 4 2 5 6))

(check-for-bug :section14-legacy-1189
  (mapcar #'cons (list 'a 'b 'c) (list 1 2 3))
  ((A . 1) (B . 2) (C . 3)))

(check-for-bug :section14-legacy-1193
  (maplist #'append (list 1 2 3 4) (list 1 2) (list 1 2 3))
  ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

(check-for-bug :section14-legacy-1197
  (maplist #'(lambda (x) (cons 'foo x)) (list 'a 'b 'c 'd))
  ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D)))

(check-for-bug :section14-legacy-1201
  (maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1))
           (list 'a 'b 'a 'c 'd 'b 'c))
  (0 0 1 0 1 1 1))

(check-for-bug :section14-legacy-1206
  (setq dummy nil)
  NIL )

(check-for-bug :section14-legacy-1210
  (mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
        (list 1 2 3 4)
        (list 'a 'b 'c 'd 'e)
        (list 'x 'y 'z))
  (1 2 3 4) )

(check-for-bug :section14-legacy-1217
  dummy
  (1 A X 2 B Y 3 C Z)                   )

(check-for-bug :section14-legacy-1221
  (setq dummy nil)
  NIL )

(check-for-bug :section14-legacy-1225
  (mapl #'(lambda (x) (push x dummy))
        (list 1 2 3 4))
  (1 2 3 4) )

(check-for-bug :section14-legacy-1230
  dummy
  ((4) (3 4) (2 3 4) (1 2 3 4)) )

(check-for-bug :section14-legacy-1234
  (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
          (list nil nil nil 'd 'e)
          (list 1 2 3 4 5 6))
  (D 4 E 5) )

(check-for-bug :section14-legacy-1240
  (mapcan #'(lambda (x) (and (numberp x) (list x)))
          (list 'a 1 'b 'c 3 4 'd 5))
  (1 3 4 5))

(check-for-bug :section14-legacy-1245
  (mapcon #'list (list 1 2 3 4))
  ((1 2 3 4) (2 3 4) (3 4) (4)) )


;;; acons

(check-for-bug :section14-legacy-1252
  (setq alist '())
  NIL)

(check-for-bug :section14-legacy-1256
  (acons 1 "one" alist)
  ((1 . "one")))

(check-for-bug :section14-legacy-1260
  alist
  NIL)

(check-for-bug :section14-legacy-1264
  (setq alist (acons 1 "one" (acons 2 "two" alist)))
  ((1 . "one") (2 . "two")))

(check-for-bug :section14-legacy-1268
  (assoc 1 alist)
  (1 . "one"))

(check-for-bug :section14-legacy-1272
  (setq alist (acons 1 "uno" alist))
  ((1 . "uno") (1 . "one") (2 . "two")))

(check-for-bug :section14-legacy-1276
  (assoc 1 alist)
  (1 . "uno"))

;;; assoc

(check-for-bug :section14-legacy-1282
  (setq values
        (list (cons 'x 100)
              (cons 'y 200)
              (cons 'z 50)))
  ((X . 100) (Y . 200) (Z . 50)))

(check-for-bug :section14-legacy-1289
  (assoc 'y values)
  (Y . 200))

(check-for-bug :section14-legacy-1293
  (rplacd (assoc 'y values) 201)
  (Y . 201))

(check-for-bug :section14-legacy-1297
  (assoc 'y values)
  (Y . 201))

(check-for-bug :section14-legacy-1301
  (setq alist
        (list (cons 1 "one")
              (cons 2 "two")
              (cons 3 "three")))
  ((1 . "one") (2 . "two") (3 . "three")))

(check-for-bug :section14-legacy-1308
  (assoc 2 alist)
  (2 . "two"))

(check-for-bug :section14-legacy-1312
  (assoc-if #'evenp alist)
  (2 . "two"))

(check-for-bug :section14-legacy-1316
  (assoc-if-not #'(lambda(x) (< x 3)) alist)
  (3 . "three"))

(check-for-bug :section14-legacy-1320
  (setq alist (list (cons "one" 1)
                    (cons "two" 2)))
  (("one" . 1) ("two" . 2)))

(check-for-bug :section14-legacy-1325
  (assoc "one" alist)
  NIL)

(check-for-bug :section14-legacy-1329
  (assoc "one" alist :test #'equalp)
  ("one" . 1))

(check-for-bug :section14-legacy-1333
  (assoc "two" alist :key #'(lambda(x) (char x 2)))
  NIL )

(check-for-bug :section14-legacy-1337
  (assoc #\o alist :key #'(lambda(x) (char x 2)))
  ("two" . 2))

(check-for-bug :section14-legacy-1341
  (assoc 'r (list (cons 'a 'b)
                  (cons 'c 'd)
                  (cons 'r 'x)
                  (cons 's 'y)
                  (cons 'r 'z)))
  (R . X))

(check-for-bug :section14-legacy-1349
  (assoc 'goo (list (cons 'foo 'bar)
                    (cons 'zoo 'goo)))
  NIL)

(check-for-bug :section14-legacy-1354
  (assoc '2 (list (list 1 'a 'b 'c)
                  (list 2 'b 'c 'd)
                  (list -7 'x 'y 'z)))
  (2 B C D))

(check-for-bug :section14-legacy-1360
  (setq alist (list (cons "one" 1)
                    (cons "2" 2)
                    (cons "three" 3)))
  (("one" . 1) ("2" . 2) ("three" . 3)))

(check-for-bug :section14-legacy-1366
  (assoc-if-not #'alpha-char-p alist
                :key #'(lambda (x) (char x 0)))
  ("2" . 2))

;;; copy-alist

(check-for-bug :section14-legacy-1373
  (progn
    (defparameter *alist* (acons 1 "one" (acons 2 "two" '())))
    t)
  t)

(check-for-bug :section14-legacy-1379
  *alist*
  ((1 . "one") (2 . "two")))

(check-for-bug :section14-legacy-1383
  (progn
    (defparameter *list-copy* (copy-list *alist*))
    t)
  t)

(check-for-bug :section14-legacy-1389
  *list-copy*
  ((1 . "one") (2 . "two")))

(check-for-bug :section14-legacy-1393
  (progn
    (defparameter *alist-copy* (copy-alist *alist*))
    t)
  t)

(check-for-bug :section14-legacy-1399
  *alist-copy*
  ((1 . "one") (2 . "two")))

(check-for-bug :section14-legacy-1403
  (setf (cdr (assoc 2 *alist-copy*)) "deux")
  "deux")

(check-for-bug :section14-legacy-1407
  *alist-copy*
  ((1 . "one") (2 . "deux")))

(check-for-bug :section14-legacy-1411
  *alist*
  ((1 . "one") (2 . "two")))

(check-for-bug :section14-legacy-1415
  (setf (cdr (assoc 1 *list-copy*)) "uno")
  "uno")

(check-for-bug :section14-legacy-1419
  *list-copy*
  ((1 . "uno") (2 . "two")))

(check-for-bug :section14-legacy-1423
  *alist*
  ((1 . "uno") (2 . "two")))

;;; pairlis

(check-for-bug :section14-legacy-1429
  (setq keys (list 1 2 3)
        data (list "one" "two" "three")
        alist (list (cons 4 "four")))
  ((4 . "four")))

(check-for-bug :section14-legacy-1435
  (pairlis keys data)
  ((3 . "three") (2 . "two") (1 . "one")))

(check-for-bug :section14-legacy-1439
  (pairlis keys data alist)
  ((3 . "three") (2 . "two") (1 . "one") (4 . "four")))

(check-for-bug :section14-legacy-1443
  alist
  ((4 . "four")))

;;; rassoc

(check-for-bug :section14-legacy-1449
  (setq alist (list (cons 1 "one")
                    (cons 2 "two")
                    (cons 3 3)))
  ((1 . "one") (2 . "two") (3 . 3)))

(check-for-bug :section14-legacy-1455
  (rassoc 3 alist)
  (3 . 3))

(check-for-bug :section14-legacy-1459
  (rassoc "two" alist)
  NIL)

(check-for-bug :section14-legacy-1463
  (rassoc "two" alist :test 'equal)
  (2 . "two"))

(check-for-bug :section14-legacy-1467
  (rassoc 1 alist :key #'(lambda (x) (if (numberp x) (/ x 3))))
  (3 . 3))

(check-for-bug :section14-legacy-1471
  (rassoc 'a
          (list (cons 'a 'b)
                (cons 'b 'c)
                (cons 'c 'a)
                (cons 'z 'a)))
  (C . A))

(check-for-bug :section14-legacy-1479
  (rassoc-if #'stringp alist)
  (1 . "one"))

(check-for-bug :section14-legacy-1483
  (rassoc-if-not #'vectorp alist)
  (3 . 3))

;;; get-properties

(check-for-bug :section14-legacy-1489
  (setq x '())
  NIL)

(check-for-bug :section14-legacy-1493
  (setq *indicator-list* (list 'prop1 'prop2))
  (PROP1 PROP2))

(check-for-bug :section14-legacy-1497
  (getf x 'prop1)
  NIL)

(check-for-bug :section14-legacy-1501
  (setf (getf x 'prop1) 'val1)
  VAL1)

(check-for-bug :section14-legacy-1505
  (eq (getf x 'prop1) 'val1)
  t)

(check-for-bug :section14-legacy-1509
  (multiple-value-bind (a b c)
      (get-properties x *indicator-list*)
    (list a b c))
  (PROP1  VAL1 (PROP1 VAL1)))

(check-for-bug :section14-legacy-1515
  x
  (PROP1 VAL1))

;;; getf

(check-for-bug :section14-legacy-1521
  (setq x '())
  NIL)

(check-for-bug :section14-legacy-1525
  (getf x 'prop1)
  NIL)

(check-for-bug :section14-legacy-1529
  (getf x 'prop1 7)
  7)

(check-for-bug :section14-legacy-1533
  (getf x 'prop1)
  NIL)

(check-for-bug :section14-legacy-1537
  (setf (getf x 'prop1) 'val1)
  VAL1)

(check-for-bug :section14-legacy-1541
  (eq (getf x 'prop1) 'val1)
  t)

(check-for-bug :section14-legacy-1545
  (getf x 'prop1)
  VAL1)

(check-for-bug :section14-legacy-1549
  (getf x 'prop1 7)
  VAL1)

(check-for-bug :section14-legacy-1553
  x
  (PROP1 VAL1))

;;; remf

(check-for-bug :section14-legacy-1559
  (setq x (cons () ()))
  (NIL))

(check-for-bug :section14-legacy-1563
  (setf (getf (car x) 'prop1) 'val1)
  VAL1)

(check-for-bug :section14-legacy-1567
  (remf (car x) 'prop1)
  t)

(check-for-bug :section14-legacy-1571
  (remf (car x) 'prop1)
  nil)

;;; intersection

(check-for-bug :section14-legacy-1577
  (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" (string #\B) "C" "d"))
        (list2 (list 1 4 5 'b 'c 'd "a" (string #\B) "c" "D")))
    (intersection list1 list2))
  #-clisp
  (C B 4 1 1)
  #+clisp
  (1 1 4 B C))

(check-for-bug :section14-legacy-1586
  (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" (string #\B) "C" "d"))
        (list2 (list 1 4 5 'b 'c 'd "a" (string #\B) "c" "D")))
    (intersection list1 list2 :test 'equal))
  #-clisp
  ("B" C B 4 1 1)
  #+clisp
  (1 1 4 B C "B"))

(check-for-bug :section14-legacy-1595
  (let ((list1 (list 1 1 2 3 4 'a 'b 'c (string "A") (string "B") (string "C") (string "d")))
        (list2 (list 1 4 5 'b 'c 'd (string "a") (string "B") (string "c") (string "D"))))
    (intersection list1 list2 :test #'equalp))
  #-clisp
  ("d" "C" "B" "A" C B 4 1 1)
  #+clisp
  (1 1 4 B C "A" "B" "C" "d"))

(check-for-bug :section14-legacy-1604
  (let ((list1 (list 1 1 2 3 4 'a 'b 'c (string "A") (string "B") (string "C") (string "d")))
        (list2 (list 1 4 5 'b 'c 'd (string "a") (string "B") (string "c") (string "D"))))
    (nintersection list1 list2))
  #-clisp
  ("B" C B 4 1 1)
  #+clisp
  (1 1 4 B C))
					;(1 1 4 B C))

(check-for-bug :section14-legacy-1614
  (let ((list1 (copy-list (list (cons 1 2)
                                (cons 2 3)
                                (cons 3 4)
                                (cons 4 5))))
	(list2 (copy-list (list (cons 1 3)
                                (cons 2 4)
                                (cons 3 6)
                                (cons 4 8)))))
    (nintersection list1 list2 :key #'cdr))
  #+(or sbcl cmu) ((3 . 4) (2 . 3))
  #-(or sbcl cmu) ((2 . 3) (3 . 4)) )

;;; adjoin

(check-for-bug :section14-legacy-1629
  (setq slist '())
  NIL )

(check-for-bug :section14-legacy-1633
  (adjoin 'a slist)
  (A) )

(check-for-bug :section14-legacy-1637
  slist
  NIL )

(check-for-bug :section14-legacy-1641
  (setq slist (adjoin (list 'test-item
                            '1)
                      slist))
  ((TEST-ITEM 1)) )

(check-for-bug :section14-legacy-1647
  (adjoin (list 'test-item 1)
          slist)
  ((TEST-ITEM 1) (TEST-ITEM 1)) )

(check-for-bug :section14-legacy-1652
  (adjoin (list 'test-item 1)
          slist
          :test 'equal)
  ((TEST-ITEM 1)) )

(check-for-bug :section14-legacy-1658
  (adjoin (list 'new-test-item 1)
          slist
          :key #'cadr)
  ((TEST-ITEM 1)) )

(check-for-bug :section14-legacy-1664
  (adjoin (list 'new-test-item 1)
          slist)
  ((NEW-TEST-ITEM 1) (TEST-ITEM 1)) )

;;; pushnew

(check-for-bug :section14-legacy-1671
  (setq x (list 'a (list 'b 'c) 'd))
  (A (B C) D))

(check-for-bug :section14-legacy-1675
  (pushnew 5 (cadr x))
  (5 B C)   )

(check-for-bug :section14-legacy-1679
  x
  (A (5 B C) D))

(check-for-bug :section14-legacy-1683
  (pushnew 'b (cadr x))
  (5 B C)  )

(check-for-bug :section14-legacy-1687
  x
  (A (5 B C) D))

(check-for-bug :section14-legacy-1691
  (setq lst (list (list 1)
                  (list 1 2)
                  (list 1 2 3)))
  ((1) (1 2) (1 2 3)))

(check-for-bug :section14-legacy-1697
  (pushnew (list 2) lst)
  ((2) (1) (1 2) (1 2 3)))

(check-for-bug :section14-legacy-1701
  (pushnew (list 1) lst)
  ((1) (2) (1) (1 2) (1 2 3)))

(check-for-bug :section14-legacy-1705
  (pushnew (list 1) lst :test 'equal)
  ((1) (2) (1) (1 2) (1 2 3)))

(check-for-bug :section14-legacy-1709
  (pushnew (list 1) lst :key #'car)
  ((1) (2) (1) (1 2) (1 2 3)) )

;;; set-difference

(check-for-bug :section14-legacy-1715
  (let ((lst1 (mapcar #'string (list #\A #\b #\C #\d)))
        (lst2 (mapcar #'string (list #\a #\B #\C #\d))))
    (set-difference lst1 lst2))
  #-clisp
  ("d" "C" "b" "A")
  #+clisp
  ("A" "b" "C" "d"))

(check-for-bug :section14-legacy-1724
  (let ((lst1 (list "A" "b" "C" "d"))
        (lst2 (list "a" "B" "C" "d")))
    (set-difference lst1 lst2 :test 'equal))
  #-clisp
  ("b" "A")
  #+clisp
  ("A" "b"))

(check-for-bug :section14-legacy-1733
  (let ((lst1 (list "A" "b" "C" "d"))
        (lst2 (list "a" "B" "C" "d")))
    (set-difference lst1 lst2 :test #'equalp))
  NIL )

(check-for-bug :section14-legacy-1739
  (let ((lst1 (list "A" "b" "C" "d"))
        (lst2 (list "a" "B" "C" "d")))
    (nset-difference lst1 lst2 :test #'string=))
  #+(or sbcl cmu)
  ("b" "A")
  #-(or sbcl cmu)
  ("A" "b"))

(check-for-bug :section14-legacy-1748
  (let ((lst1 (list (cons "a" "b")
                    (cons "c" "d")
                    (cons "e" "f")))
        (lst2 (list (cons "c" "a")
                    (cons "e" "b")
                    (cons "d" "a"))))
    (nset-difference lst1 lst2 :test #'string= :key #'cdr))
  #+(or sbcl cmu)
  (("e" . "f") ("c" . "d"))
  #-(or sbcl cmu)
  (("c" . "d") ("e" . "f")))

(check-for-bug :section14-legacy-1761
  (let ((lst1 (list (cons "a" "b")
                    (cons "c" "d")
                    (cons "e" "f")))
        (lst2 (list (cons "c" "a")
                    (cons "e" "b")
                    (cons "d" "a"))))
    (nset-difference lst1 lst2 :test #'string= :key #'cdr)
    lst1)
  #+(or sbcl cmu) (("a" . "b") ("c" . "d"))
  #-(or sbcl cmu) (("a" . "b") ("c" . "d") ("e" . "f")) )


(check-for-bug :section14-legacy-1774
  (let ((lst1 (list (cons "a" "b")
                    (cons "c" "d")
                    (cons "e" "f")))
        (lst2 (list (cons "c" "a")
                    (cons "e" "b")
                    (cons "d" "a"))))
    (nset-difference lst1 lst2 :test #'string= :key #'cdr)
    lst2)
  (("c" . "a") ("e" . "b") ("d" . "a")) )

;; Remove all flavor names that contain "c" or "w".
(check-for-bug :section14-legacy-1786
  (set-difference (list
                   "strawberry" "chocolate" "banana"
                   "lemon" "pistachio" "rhubarb")
                  '(#\c #\w)
                  :test #'(lambda (s c) (find c s)))
  #+(or sbcl cmu) ("rhubarb" "lemon" "banana")
  #+clisp ("banana" "lemon" "rhubarb")
  #-(or sbcl cmu sbcl clisp) ("banana" "rhubarb" "lemon"))
;;One possible ordering.)

;;; set-exclusive-or

(check-for-bug :section14-legacy-1799
  (let ((lst1 (list 1 (string #\a) (string #\b)))
        (lst2 (list 1 (string #\A) (string #\b))))
    (set-exclusive-or lst1 lst2))
  #-clisp
  ("b" "A" "b" "a")
  #+clisp
  ("a" "b" "A" "b"))

(check-for-bug :section14-legacy-1808
  (let ((lst1 (list 1 (string #\a) (string #\b)))
        (lst2 (list 1 (string #\A) (string #\b))))
    (set-exclusive-or lst1 lst2 :test #'equal))
  ("A" "a"))

(check-for-bug :section14-legacy-1814
  (let ((lst1 (list 1 (string #\a) (string #\b)))
        (lst2 (list 1 (string #\A) (string #\b))))
    (set-exclusive-or lst1 lst2 :test 'equalp))
  NIL )

(check-for-bug :section14-legacy-1820
  (let ((lst1 (list 1 (string #\a) (string #\b)))
	(lst2 (list 1 (string #\A) (string #\b))))
    (nset-exclusive-or lst1 lst2))
  ("a" "b" "A" "b") )


(check-for-bug :section14-legacy-1827
  (let ((lst1 (list (cons "a" "b")
                    (cons "c" "d")
                    (cons "e" "f")))
        (lst2 (list (cons "c" "a")
                    (cons "e" "b")
                    (cons "d" "a"))))
    (nset-exclusive-or lst1 lst2 :test #'string= :key #'cdr))
  (("c" . "d") ("e" . "f") ("c" . "a") ("d" . "a")))

(check-for-bug :section14-legacy-1837
  (let ((lst1 (list (cons "a" "b")
                    (cons "c" "d")
                    (cons "e" "f")))
        (lst2 (list (cons "c" "a")
                    (cons "e" "b")
                    (cons "d" "a"))))
    (nset-exclusive-or lst1 lst2 :test #'string= :key #'cdr)
    lst1)
  #-(or sbcl cmu) (("a" . "b") ("c" . "d") ("e" . "f"))
  #+(or sbcl cmu) (("a" . "b") ("c" . "d") ("e" . "f") ("c" . "a") ("d" . "a")))

(check-for-bug :section14-legacy-1849
  (let ((lst1 (list (cons "a" "b")
                    (cons "c" "d")
                    (cons "e" "f")))
        (lst2 (list (cons "c" "a")
                    (cons "e" "b")
                    (cons "d" "a"))))
    (nset-exclusive-or lst1 lst2 :test #'string= :key #'cdr)
    lst2)
  (("c" . "a") ("d" . "a")))

;;; subsetp

(check-for-bug :section14-legacy-1862
  (setq cosmos (list 1 "a" (list 1 2)))
  (1 "a" (1 2)))

(check-for-bug :section14-legacy-1866
  (subsetp (list 1) cosmos)
  t)

(check-for-bug :section14-legacy-1870
  (subsetp (list (list 1 2)) cosmos)
  nil)

(check-for-bug :section14-legacy-1874
  (subsetp (list (list 1 2)) cosmos :test 'equal)
  t)

(check-for-bug :section14-legacy-1878
  (subsetp (list 1 "A") cosmos :test #'equalp)
  t)

(check-for-bug :section14-legacy-1882
  (subsetp (list (list 1) (list 2))
           (list (list 1) (list 2)))
  nil)

(check-for-bug :section14-legacy-1887
  (subsetp (list (list 1) (list 2))
           (list (list 1) (list 2)) :key #'car)
  t)

;;; union

(check-for-bug :section14-legacy-1894
  (union (list 'a 'b 'c) (list 'f 'a 'd))
  #+(or sbcl cmu) (C B F A D)
  #+(or clisp ecls) (B C F A D)
  #-(or sbcl cmu sbcl clisp ecls) fill-this-in)

;; (A B C F D) OR  (B C F A D) OR  (D F A B C)

(check-for-bug :section14-legacy-1902
  (union (list (list 'x 5)
               (list 'y 6))
         (list (list 'z 2)
               (list 'x 4))
         :key #'car)
  #+(or sbcl cmu sbcl clisp ecls) ((Y 6) (Z 2) (X 4))
  #-(or sbcl cmu sbcl clisp ecls) fill-this-in)
;;  ((X 5) (Y 6) (Z 2)) OR  ((X 4) (Y 6) (Z 2))

(check-for-bug :section14-legacy-1912
  (let ((lst1 (list 1 2 (list 1 2) "a" "b"))
        (lst2 (list 2 3 (list 2 3) "B" "C")))
    (nunion lst1 lst2))
  #+(or sbcl cmu)
  ("b" "a" (1 2) 1 2 3 (2 3) "B" "C")
  #+(or clisp ecls)
  (1 (1 2) "a" "b" 2 3 (2 3) "B" "C")
  #-(or sbcl cmu sbcl clisp ecls)
  fill-this-in)

;;  (1 (1 2) "a" "b" 2 3 (2 3) "B" "C")  OR  (1 2 (1 2) "a" "b" "C" "B" (2 3) 3)











