;;; section 18 hash tables -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(check-for-bug :section18-legacy-6
  (progn
    (setq a (make-hash-table))
    t)
  t)
;; #<HASH-TABLE EQL 0/120 32536573>

(check-for-bug :section18-legacy-13
  (setf (gethash 'color a) 'brown)
  BROWN)

(check-for-bug :section18-legacy-17
  (setf (gethash 'name a) 'fred)
  FRED)

(check-for-bug :section18-legacy-21
  (multiple-value-bind (a b)
      (gethash 'color a)
    (list a b))
  (BROWN t))

(check-for-bug :section18-legacy-27
  (multiple-value-bind (a b)
      (gethash 'name a)
    (list a b))
  ( FRED t))

(check-for-bug :section18-legacy-33
  (multiple-value-bind (a b)
      (gethash 'pointy a)
    (list a b))
  ( NIL nil))

;;;make-hash-table

(check-for-bug :section18-legacy-41
  (progn
    (setq table (make-hash-table))
    t)
  t)
;;  #<HASH-TABLE EQL 0/120 46142754>

(check-for-bug :section18-legacy-48
  (setf (gethash "one" table) 1)
  1)

(check-for-bug :section18-legacy-52
  (multiple-value-bind (a b)
      (gethash "one" table)
    (list a b))
  (  NIL nil))

(check-for-bug :section18-legacy-58
  (progn
    (setq table (make-hash-table :test 'equal))
    t)
  t)
					;  #<HASH-TABLE EQUAL 0/139 46145547>

(check-for-bug :section18-legacy-65
  (setf (gethash "one" table) 1)
  1)

(check-for-bug :section18-legacy-69
  (multiple-value-bind (a b)
      (gethash "one" table)
    (list a b))
  (  1 T))

(check-for-bug :section18-legacy-75
  (progn
    (make-hash-table :rehash-size 1.5 :rehash-threshold 0.7)
    t)
  t)
					;  #<HASH-TABLE EQL 0/120 46156620>

;;; hash-table-p

(check-for-bug :section18-legacy-84
  (progn
    (setq table (make-hash-table))
    t)
  t)
					; #<HASH-TABLE EQL 0/120 32511220>

(check-for-bug :section18-legacy-91
  (hash-table-p table)
  t)

(check-for-bug :section18-legacy-95
  (hash-table-p 37)
  nil)

(check-for-bug :section18-legacy-99
  (hash-table-p '((a . 1) (b . 2)))
  nil)

;; hash-table-count

(check-for-bug :section18-legacy-105
  (progn
    (setq table (make-hash-table))
    t)
  t)
					;  #<HASH-TABLE EQL 0/120 32115135>

(check-for-bug :section18-legacy-112
  (hash-table-count table)
  0)

(check-for-bug :section18-legacy-116
  (setf (gethash 57 table) "fifty-seven")
  "fifty-seven")

(check-for-bug :section18-legacy-120
  (hash-table-count table)
  1)

(check-for-bug :section18-legacy-124
  (dotimes (i 100) (setf (gethash i table) i))
  NIL)

(check-for-bug :section18-legacy-128
  (hash-table-count table)
  100)

;;; hash-table-rehash-size

(check-for-bug :section18-legacy-134
  (progn (setq table (make-hash-table :size 100 :rehash-size 1.4))
         t)
  t)
					;  #<HASH-TABLE EQL 0/100 2556371>

(check-for-bug :section18-legacy-140
  (hash-table-rehash-size table)
  #-clisp 1.4
  #+clisp 1.4s0)

;;; HASH-TABLE-REHASH-THRESHOLD

(check-for-bug :section18-legacy-147
  (progn
    (setq table (make-hash-table :size 100 :rehash-threshold 0.5))
    t)
  t)
					;  #<HASH-TABLE EQL 0/100 2562446>

(check-for-bug :section18-legacy-154
  (hash-table-rehash-threshold table)
  #-clisp 0.5
  #+clisp 0.75s0)


;;; get-hash

(check-for-bug :section18-legacy-162
  (progn
    (setq table (make-hash-table))
    t)
  t)


(check-for-bug :section18-legacy-169
  (multiple-value-bind (a b)
      (gethash 1 table)
    (list a b))
  (NIL nil))

(check-for-bug :section18-legacy-175
  (multiple-value-bind (a b)
      (gethash 1 table 2)
    (list a b))
  (2 nil))

(check-for-bug :section18-legacy-181
  (setf (gethash 1 table) "one")
  "one")

(check-for-bug :section18-legacy-185
  (setf (gethash 2 table "two") "two")
  "two")

(check-for-bug :section18-legacy-189
  (multiple-value-bind (a b)
      (gethash 1 table)
    (list a b))
  ("one" t))

(check-for-bug :section18-legacy-195
  (multiple-value-bind (a b)
      (gethash 2 table)
    (list a b))
  ("two" t))

(check-for-bug :section18-legacy-201
  (multiple-value-bind (a b)
      (gethash nil table)
    (list a b))
  (NIL nil))

(check-for-bug :section18-legacy-207
  (setf (gethash nil table) nil)
  NIL)

(check-for-bug :section18-legacy-211
  (multiple-value-bind (a b)
      (gethash nil table)
    (list a b))
  (NIL t))

(unintern '*counters*)

(check-for-bug :section18-legacy-219
  (defvar *counters* (make-hash-table))
  *COUNTERS*)

(check-for-bug :section18-legacy-223
  (multiple-value-bind (a b)
      (gethash 'foo *counters*)
    (list a b))
  (NIL nil))

(check-for-bug :section18-legacy-229
  (multiple-value-bind (a b)
      (gethash 'foo *counters* 0)
    (list a b))
  (0 nil))

;;; remhash

(setq table (make-hash-table))

(check-for-bug :section18-legacy-239
  (setf (gethash 100 table) "C")
  "C")
(check-for-bug :section18-legacy-242
  (multiple-value-bind (a b)
      (gethash 100 table)
    (list a b))
  ("C" t))

(check-for-bug :section18-legacy-248
  (remhash 100 table)
  t)

(check-for-bug :section18-legacy-252
  (multiple-value-bind (a b)
      (gethash 100 table)
    (list a b))
  (NIL nil))

(check-for-bug :section18-legacy-258
  (remhash 100 table)
  nil)

;;; maphash

(setq table (make-hash-table))

(check-for-bug :section18-legacy-266
  (dotimes (i 10) (setf (gethash i table) i))
  NIL)

(check-for-bug :section18-legacy-270
  (let ((sum-of-squares 0))
    (maphash #'(lambda (key val)
                 (let ((square (* val val)))
                   (incf sum-of-squares square)
                   (setf (gethash key table) square)))
             table)
    sum-of-squares)
  285)

(check-for-bug :section18-legacy-280
  (hash-table-count table)
  10)

(check-for-bug :section18-legacy-284
  (maphash #'(lambda (key val)
               (when (oddp val) (remhash key table)))
           table)
  NIL)

(check-for-bug :section18-legacy-290
  (hash-table-count table)
  5)

(check-for-bug :section18-legacy-294
  (let ((a nil))
    (maphash #'(lambda (k v) (setq a (cons  (list k v) a ))) table)
    a)
  #-clisp
  ((8 64) (6 36) (4 16) (2 4) (0 0))
  #+clisp
  ((0 0) (2 4) (4 16) (6 36) (8 64)))

;;; with-hash-table-iterator

(check-for-bug :section18-legacy-305
  (defun test-hash-table-iterator (hash-table)
    (let ((all-entries '())
          (generated-entries '())
          (unique (list nil)))
      (maphash #'(lambda (key value) (push (list key value) all-entries))
               hash-table)
      (with-hash-table-iterator (generator-fn hash-table)
        (loop
            (multiple-value-bind (more? key value) (generator-fn)
              (unless more? (return))
              (unless (eql value (gethash key hash-table unique))
                (error "Key ~S not found for value ~S" key value))
              (push (list key value) generated-entries))))
      (unless (= (length all-entries)
                 (length generated-entries)
                 (length (union all-entries generated-entries
                                :key #'car :test (hash-table-test hash-table))))
        (error "Generated entries and Maphash entries don't correspond"))
      t))
  test-hash-table-iterator)

(check-for-bug :section18-legacy-327
  (test-hash-table-iterator table)
  t)

;;; clrhash

(setq table (make-hash-table))

(check-for-bug :section18-legacy-335
  (dotimes (i 100) (setf (gethash i table) (format nil "~R" i)))
  NIL)

(check-for-bug :section18-legacy-339
  (hash-table-count table)
  100)

(check-for-bug :section18-legacy-343
  (multiple-value-bind (a b)
      (gethash 57 table)
    (list a b))
  ("fifty-seven" t))

(clrhash table)

(check-for-bug :section18-legacy-351
  (hash-table-count table)
  0)

(check-for-bug :section18-legacy-355
  (multiple-value-bind (a b)
      (gethash 57 table)
    (list a b))
  ( NIL nil))

;;; sxhash

(check-for-bug :section18-legacy-363
  (= (sxhash (list 'list "ab")) (sxhash (list 'list "ab")))
  t)


(check-for-bug :section18-legacy-368
  (= (sxhash "a") (sxhash (make-string 1 :initial-element #\a)))
  t)


(check-for-bug :section18-legacy-373
  (let ((r (make-random-state)))
    (= (sxhash r) (sxhash (make-random-state r))))
  t)









