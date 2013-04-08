;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :hash-legacy-4
  (progn (in-package "SYS") t)
  t)

(check-for-bug :hash-legacy-7
  (make-hash-table :test (quote eq) :size 20)
  #s(hash-table test eq size 20 %%size 64 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-25
  (make-hash-table :test (quote eql) :size 2)
  #s(hash-table test eql size 2 %%size 4 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-31
  (make-hash-table :test (quote equal) :size 2)
  #s(hash-table test equal size 2 %%size 4 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-37
  (progn (make-hash-table :test (function eq) :size 2) t) t)

(check-for-bug :hash-legacy-40
  (progn (make-hash-table :test (function eql) :size 2)t) t)

(check-for-bug :hash-legacy-43
  (make-hash-table :size nil)
  error)

(check-for-bug :hash-legacy-47
  (make-hash-table :size -3)
  error)

(check-for-bug :hash-legacy-51
  (make-hash-table :size 2.0)
  error)

(check-for-bug :hash-legacy-55
  (make-hash-table :size 2 :rehash-size 1.5)
  #s(hash-table test eql size 2 %%size 4 rehash-size 1.5 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-61
  (make-hash-table :size 2 :rehash-size -1.5)
  error)

(check-for-bug :hash-legacy-65
  (make-hash-table :size 2 :rehash-size 0.5)
  error)

(check-for-bug :hash-legacy-69
  (make-hash-table :size 2 :rehash-size 1.0)
  #s(hash-table test eql size 2 %%size 4 rehash-size 1.0 %%rehash-size 4
                rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-75
  (make-hash-table :size 2 :rehash-size 5)
  #s(hash-table test eql size 2 %%size 4 rehash-size 5 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-81
  (make-hash-table :size 4 :rehash-size 5.0)
  #s(hash-table test eql size 4 %%size 8 rehash-size 5.0 %%rehash-size 8
                rehash-threshold 13 %%rehash-threshold 3 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-88
  (make-hash-table :size 2 :rehash-size nil)
  error)

(check-for-bug :hash-legacy-92
  (make-hash-table :size 2 :rehash-threshold nil)
  error)

(check-for-bug :hash-legacy-96
  (make-hash-table :%%size 3)
  #s(hash-table test eql size 16 %%size 3 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-102
  (setq tab (make-hash-table))
  #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-114
  (setf-gethash (quote hallo) tab (quote wiegwhts))
  wiegwhts)

(check-for-bug :hash-legacy-118
  (setf-gethash (quote uhu) tab (quote kauz))
  kauz)

(check-for-bug :hash-legacy-122
  (gethash (quote uhu) tab)
  kauz)

(check-for-bug :hash-legacy-126
  (gethash uhu tab)
  error)

(check-for-bug :hash-legacy-130
  (make-hash-table)
  #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-142
  (setq tab nil)
  nil)

(check-for-bug :hash-legacy-146
  (setf-gethash (quote uhu) tab (quote kaus))
  error)

(check-for-bug :hash-legacy-150
  (gethash (quote uhu) tab)
  error)

(check-for-bug :hash-legacy-154
  (gethash (quote otto) tab)
  error)

(check-for-bug :hash-legacy-158
  (setq tab (make-hash-table))
  #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-170
  (setf-gethash (quote uhu) tab (quote kaus))
  kaus)

(check-for-bug :hash-legacy-174
  (gethash (quote uhu) tab)
  kaus)

(check-for-bug :hash-legacy-178
  (gethash (quote otto) tab)
  nil)

(check-for-bug :hash-legacy-182
  (setf-gethash (quote uhu) tab (quote kauz))
  kauz)

(check-for-bug :hash-legacy-186
  (setf-gethash tab)
  error)

(check-for-bug :hash-legacy-190
  (remhash (quote uhu) tab)
  t)

(check-for-bug :hash-legacy-194
  tab
  #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-206
  (clrhash tab9)
  error)

(check-for-bug :hash-legacy-210
  (clrhash tab)
  #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element)))

(check-for-bug :hash-legacy-222
  (hash-table-count tab)
  0)

(check-for-bug :hash-legacy-226
  (setf-gethash (quote klak) tab (quote klase))
  klase)

(check-for-bug :hash-legacy-230
  (setf-gethash (quote kunze) tab (quote riese))
  riese)

(check-for-bug :hash-legacy-234
  (hash-table-p tab)
  t)

(check-for-bug :hash-legacy-238
  (hash-table-count tab)
  2)

(check-for-bug :hash-legacy-242
  (remhash (quote kunze) tab)
  t)

(check-for-bug :hash-legacy-246
  (setf-gethash (quote wald) tab (quote khjgsfgjhdf))
  khjgsfgjhdf)

(check-for-bug :hash-legacy-250
  (gethash)
  error)

(check-for-bug :hash-legacy-254
  (remhash)
  error)

(check-for-bug :hash-legacy-258
  (clrhash tab)
  #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
                rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
                #(%%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
                  %%empty-element %%empty-element %%empty-element)))

