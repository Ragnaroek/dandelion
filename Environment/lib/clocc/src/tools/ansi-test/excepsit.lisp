;;; based clisp version 1.12 -*- mode: lisp -*-
(in-package :cl-user)

;;; Test "Exceptional situations" as specified by CLHS

;; NB: CLHS section 1.4.2 implies that we have to verify only those
;; specifications which have the wording "an error is signalled" or
;; "an error should be signalled".

#-(or cmu sbcl lispworks);; XXXX
(check-for-bug :excepsit-legacy-11
  (abort)
  control-error)

(check-for-bug :excepsit-legacy-15
  (acos 'x)
  type-error)

(check-for-bug :excepsit-legacy-19
  (acosh 'x)
  type-error)

(check-for-bug :excepsit-legacy-23
  (progn
    (defgeneric foo01 (x))
    (defmethod foo01 ((x number)) t)
    (let ((m (find-method #'foo01 nil (list (find-class 'number)))))
      (remove-method #'foo01 m)
      (defgeneric foo01 (x y))
      (add-method #'foo01 m)
      ) )
  error)

#-CLISP
(check-for-bug :excepsit-legacy-35
  ;; documented behaviour of ADD-METHOD
  (progn
    (defgeneric foo02 (x))
    (defmethod foo02 ((x number)) t)
    (let ((m (find-method #'foo02 nil (list (find-class 'number)))))
                                        ; wrong, not? (remove-method #'foo02 m)
      (defgeneric foo03 (x))
      (add-method #'foo03 m)
      ) )
  error
  "add-method:...

If method is a method object of another generic function,
an error of type error is signaled. ")

(check-for-bug :excepsit-legacy-51
  (let ((a (make-array 5 :adjustable t)))
    (adjust-array a 4 :fill-pointer 1)
    )
  error)

(check-for-bug :excepsit-legacy-57
  (adjustable-array-p '(x))
  type-error)

(check-for-bug :excepsit-legacy-61
  (alpha-char-p 33)
  type-error)

(check-for-bug :excepsit-legacy-65
  (alphanumericp 33)
  type-error)

(check-for-bug :excepsit-legacy-69
  (array-dimensions '(x))
  type-error)

(check-for-bug :excepsit-legacy-73
  (array-displacement '(x))
  type-error)

(check-for-bug :excepsit-legacy-77
  (array-element-type '(x))
  type-error)

(check-for-bug :excepsit-legacy-81
  (array-has-fill-pointer-p '(x))
  type-error)

(check-for-bug :excepsit-legacy-85
  (array-rank '(x))
  type-error)

(check-for-bug :excepsit-legacy-89
  (array-total-size '(x))
  type-error)

(check-for-bug :excepsit-legacy-93
  (ash 3/4 2)
  type-error)

(check-for-bug :excepsit-legacy-97
  (ash 3 4.0)
  type-error)

(check-for-bug :excepsit-legacy-101
  (asin 'x)
  type-error)

(check-for-bug :excepsit-legacy-105
  (asinh 'x)
  type-error)

(check-for-bug :excepsit-legacy-109
  (atan 'x)
  type-error)

(check-for-bug :excepsit-legacy-113
  (atan #c(0 0.4) 3.4)
  type-error)

(check-for-bug :excepsit-legacy-117
  (atan -4 #c(3 4))
  type-error)

(check-for-bug :excepsit-legacy-121
  (atanh 'x)
  type-error)

(check-for-bug :excepsit-legacy-125
  (boole 'x 3 4)
  type-error)

(check-for-bug :excepsit-legacy-129
  (boole boole-and 3/4 -7)
  type-error)

(check-for-bug :excepsit-legacy-133
  (boole boole-set 5 #c(-3 4))
  type-error)

(check-for-bug :excepsit-legacy-137
  (both-case-p 33)
  type-error)

(check-for-bug :excepsit-legacy-141
  (boundp 47)
  type-error)

(check-for-bug :excepsit-legacy-145
  (butlast '(a b c) -1)
  type-error)

(check-for-bug :excepsit-legacy-149
  (butlast '#(a b c))
  type-error)

(check-for-bug :excepsit-legacy-153
  (car 'x)
  type-error)

(check-for-bug :excepsit-legacy-157
  (cdr '#(a b c))
  type-error)

(check-for-bug :excepsit-legacy-161
  (cdadar '((x y)))
  type-error)

(check-for-bug :excepsit-legacy-165
  (progn
    (defgeneric foo04 (x))
    (defmethod foo04 ((x real)) 'ok)
    (defmethod foo04 ((x integer)) (call-next-method (sqrt x)))
    (foo04 -1))
  error
  "(sqrt -1) is not a real...")

(check-for-bug :excepsit-legacy-174
  (progn
    (defgeneric foo041 (x))
    (defmethod foo041 ((x real)) 'ok)
    (defmethod foo041 ((x integer)) (call-next-method (sqrt x)))
    (foo041 2))
  error
  "CLHS:  When providing arguments to CALL-NEXT-METHOD, the following
 rule must be satisfied or an error of type ERROR should be signaled:
 the ordered set of applicable methods for a changed set of arguments
 for CALL-NEXT-METHOD must be the same as the ordered set of applicable
 methods for the original arguments to the generic function.")

(check-for-bug :excepsit-legacy-187
  (ccase 'x)
  type-error)

(check-for-bug :excepsit-legacy-191
  (char-code 33)
  type-error)

(check-for-bug :excepsit-legacy-195
  (char-downcase 33)
  type-error)

(check-for-bug :excepsit-legacy-199
  (char-equal)
  program-error)

(check-for-bug :excepsit-legacy-203
  (char-greaterp)
  program-error)

(check-for-bug :excepsit-legacy-207
  (char-lessp)
  program-error)

(check-for-bug :excepsit-legacy-211
  (char-name 33)
  type-error)

(check-for-bug :excepsit-legacy-215
  (char-not-equal)
  program-error)

(check-for-bug :excepsit-legacy-219
  (char-not-greaterp)
  program-error)

(check-for-bug :excepsit-legacy-223
  (char-not-lessp)
  program-error)

(check-for-bug :excepsit-legacy-227
  (char-upcase 33)
  type-error)

(check-for-bug :excepsit-legacy-231
  (char/=)
  program-error)

(check-for-bug :excepsit-legacy-235
  (char<)
  program-error)

(check-for-bug :excepsit-legacy-239
  (char<=)
  program-error)

(check-for-bug :excepsit-legacy-243
  (char=)
  program-error)

(check-for-bug :excepsit-legacy-247
  (char>)
  program-error)

(check-for-bug :excepsit-legacy-251
  (char>=)
  program-error)

(check-for-bug :excepsit-legacy-255
  (character "abc")
  type-error)

(check-for-bug :excepsit-legacy-259
  (character "")
  type-error)

(check-for-bug :excepsit-legacy-263
  (character 33)
  type-error)

(check-for-bug :excepsit-legacy-267
  (clear-input '*terminal-io*)
  type-error)

(check-for-bug :excepsit-legacy-271
  (clear-output '*terminal-io*)
  type-error)

(check-for-bug :excepsit-legacy-275
  (coerce '(a b c) '(vector * 4))
  type-error)

(check-for-bug :excepsit-legacy-279
  (coerce '#(a b c) '(vector * 4))
  type-error)

(check-for-bug :excepsit-legacy-283
  (coerce '(a b c) '(vector * 2))
  type-error)

(check-for-bug :excepsit-legacy-287
  (coerce '#(a b c) '(vector * 2))
  type-error)

(check-for-bug :excepsit-legacy-291
  (coerce "foo" '(string 2))
  type-error)

(check-for-bug :excepsit-legacy-295
  (coerce '#(#\a #\b #\c) '(string 2))
  type-error)

(check-for-bug :excepsit-legacy-299
  (coerce '(0 1) '(simple-bit-vector 3))
  type-error)

(check-for-bug :excepsit-legacy-303
  (coerce nil 'nil)
  type-error)

(check-for-bug :excepsit-legacy-307
  (coerce '#:nonexistent 'function)
  error)

(check-for-bug :excepsit-legacy-311
  (coerce 'and 'function)
  error)

(check-for-bug :excepsit-legacy-315
  (compile-file "./12836123.lsp")
  file-error)

(check-for-bug :excepsit-legacy-319
  (concatenate 'symbol)
  error)

(check-for-bug :excepsit-legacy-323
  (concatenate '(string 3) "ab" "cd")
  type-error)

(check-for-bug :excepsit-legacy-328
  (copy-pprint-dispatch 'x)
  type-error)

(check-for-bug :excepsit-legacy-332
  (copy-seq 'x)
  type-error)

(check-for-bug :excepsit-legacy-336
  (copy-symbol #\x)
  type-error)

(check-for-bug :excepsit-legacy-340
  (cos 'x)
  type-error)

(check-for-bug :excepsit-legacy-344
  (cosh 'x)
  type-error)

(check-for-bug :excepsit-legacy-348
  (count #\x 'x)
  type-error)

(check-for-bug :excepsit-legacy-352
  (let ((x nil)) (ctypecase x))
  type-error)

(check-for-bug :excepsit-legacy-356
  (decode-float 2/3)
  type-error)

(check-for-bug :excepsit-legacy-360
  (defclass foo05 () (a b a))
  program-error
  "defclass:
...
If there are any duplicate slot names,
an error of type program-error is signaled. ")

(check-for-bug :excepsit-legacy-368
  (defclass foo06 () (a b) (:default-initargs x a x b))
  program-error
  "defclass:
...
If an initialization argument name appears more
than once in :default-initargs class option, an
error of typeprogram-error is signaled. ")

(check-for-bug :excepsit-legacy-377
  (defclass foo07 () ((a :allocation :class :allocation :class)))
  program-error
  "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(check-for-bug :excepsit-legacy-386
  (defclass foo08 () ((a :initform 42 :initform 42)))
  program-error
  "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(check-for-bug :excepsit-legacy-395
  (defclass foo09 () ((a :type real :type real)))
  program-error
  "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(check-for-bug :excepsit-legacy-404
  (defclass foo10 () ((a :documentation "bla" :documentation "blabla")))
  program-error
  "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(check-for-bug :excepsit-legacy-413
  (defgeneric if (x))
  program-error
  "defgeneric:
...
If function-name names an ordinary function,
a macro, or a special operator, an error of type
program-error is signaled.")

(check-for-bug :excepsit-legacy-422
  (progn
    (defmacro foo11 (x) x)
    (defgeneric foo11 (x)))
  program-error
  "defgeneric:
...
If function-name names an ordinary function,
a macro, or a special operator, an error of type
program-error is signaled.")

(check-for-bug :excepsit-legacy-433
  (progn
    (defun foo12 (x) x)
    (defgeneric foo12 (x)))
  program-error
  "defgeneric:
...
If function-name names an ordinary function,
a macro, or a special operator, an error of type
program-error is signaled.")

(check-for-bug :excepsit-legacy-444
  (defgeneric foo13 (x y &rest l)
    (:method (x y))
    )
  error
  "")

(check-for-bug :excepsit-legacy-451
  (defgeneric foo14 (x)
    (:documentation "bla")
    (:documentation "blabla")
    )
  program-error)

(check-for-bug :excepsit-legacy-458
  (defgeneric foo15 (x)
    (:my-option t))
  program-error)

;;  define-method-combination is too complicated

(check-for-bug :excepsit-legacy-465
  (progn
    (defvar foo16)
    (define-symbol-macro foo16 t))
  program-error)

(check-for-bug :excepsit-legacy-471
  (defmethod if (x) nil)
  error)

(check-for-bug :excepsit-legacy-475
  (progn
    (defmacro foo17 (x) x)
    (defmethod foo17 (x) nil))
  error)

(check-for-bug :excepsit-legacy-481
  (progn
    (defun foo18 (x) x)
    (defmethod foo18 (x) nil))
  error)

(check-for-bug :excepsit-legacy-487
  (progn
    (defgeneric foo19 (x))
    (defmethod foo19 (x y) nil))
  error)

(check-for-bug :excepsit-legacy-493
  (progn
    (defpackage "FOO20")
    (defpackage "FOO21" (:nicknames "FOO20")))
  package-error)

(check-for-bug :excepsit-legacy-499
  (defpackage "FOO22" (:size 20) (:size 20))
  program-error)

(check-for-bug :excepsit-legacy-503
  (defpackage "FOO23" (:documentation "bla") (:documentation "blabla"))
  program-error)

(check-for-bug :excepsit-legacy-507
  (defpackage "FOO24" (:my-option t))
  program-error)

(check-for-bug :excepsit-legacy-511
  (defpackage "FOO25" (:shadow "IF") (:intern "IF"))
  program-error)

(check-for-bug :excepsit-legacy-515
  (defpackage "FOO26" (:shadow "IF") (:import-from "USER" "IF"))
  program-error)

(check-for-bug :excepsit-legacy-519
  (defpackage "FOO27" (:shadow "IF") (:shadowing-import-from "USER" "IF"))
  program-error)

(check-for-bug :excepsit-legacy-523
  (defpackage "FOO28" (:intern "IF") (:import-from "USER" "IF"))
  program-error)

(check-for-bug :excepsit-legacy-527
  (defpackage "FOO29" (:intern "IF") (:shadowing-import-from "USER" "IF"))
  program-error)

(check-for-bug :excepsit-legacy-531
  (defpackage "FOO30" (:import-from "USER" "IF") (:shadowing-import-from "USER" "IF"))
  program-error)

(check-for-bug :excepsit-legacy-535
  (defpackage "FOO31" (:export "IF") (:intern "IF"))
  program-error)

#-sbcl
(check-for-bug :excepsit-legacy-540
  (defstruct foo32 a foo20::a)
  program-error)

#-sbcl
(check-for-bug :excepsit-legacy-545
  (progn
    (defstruct foo33 a)
    (defstruct (foo34 (:include foo33)) foo20::a))
  program-error)

(check-for-bug :excepsit-legacy-551
  (delete #\x 'x)
  type-error)

(check-for-bug :excepsit-legacy-555
  (delete-duplicates 'abba)
  type-error)

;; deleting a non-existing file can be successful!
;; the results are not easily predictable across implementations
;;(check-for-bug :excepsit-legacy-561
;; (progn
;;   (with-open-file (s "/tmp/foo35.tmp" :direction :output))
;;   (delete-file "/tmp/foo35.tmp/bar"))
;; nil or file-error??)

(check-for-bug :excepsit-legacy-567
  (destructuring-bind (a) '(1 2) a)
  error)

;;  directory - no way to make a directory search fail

#-CLISP
(check-for-bug :excepsit-legacy-574
  ;; documented behaviour of DISASSEMBLE
  (disassemble #x123456)
  type-error)

;;  dribble - no way to force a file-error

(check-for-bug :excepsit-legacy-581
  (ecase 'x)
  type-error)

(check-for-bug :excepsit-legacy-585
  (elt 'x 0)
  type-error)

(check-for-bug :excepsit-legacy-589
  (elt "abc" 4)
  type-error)

(check-for-bug :excepsit-legacy-593
  (elt '(a b c) 4)
  type-error)

(check-for-bug :excepsit-legacy-597
  (elt '#(a b c) 4)
  type-error)

(check-for-bug :excepsit-legacy-601
  (elt (make-array 3 :fill-pointer 3 :adjustable t) 4)
  type-error)

(check-for-bug :excepsit-legacy-605
  (endp 'x)
  type-error)

(check-for-bug :excepsit-legacy-609
  (ensure-directories-exist "/*/")
  file-error)

(check-for-bug :excepsit-legacy-613
  (error 42)
  type-error)

(check-for-bug :excepsit-legacy-617
  (let ((x nil)) (etypecase x))
  type-error)

(check-for-bug :excepsit-legacy-621
  (every '(lambda (x) x) nil)
  type-error)

(check-for-bug :excepsit-legacy-625
  (every #'identity 'x)
  type-error)

(check-for-bug :excepsit-legacy-629
  (fboundp '(psetf aref))
  type-error)

(check-for-bug :excepsit-legacy-633
  (fdefinition '(psetf aref))
  type-error)

(check-for-bug :excepsit-legacy-637
  (fdefinition '#:nonexistent)
  undefined-function)

(check-for-bug :excepsit-legacy-641
  (file-author "*")
  file-error)

(check-for-bug :excepsit-legacy-645
  (file-length *terminal-io*)
  type-error)

(check-for-bug :excepsit-legacy-649
  (with-open-file (s "./foo35.tmp" :direction :output)
    (file-position s 0.0))
  error)

(check-for-bug :excepsit-legacy-654
  (with-open-file (s "./foo35.tmp" :direction :output)
    (file-position s -1))
  error)

(check-for-bug :excepsit-legacy-659
  (with-open-file (s "./foo35.tmp" :direction :input)
    (file-position s (+ (file-length s) 1000)))
  error)

(check-for-bug :excepsit-legacy-664
  (not (delete-file "./foo35.tmp"))
  nil)

(check-for-bug :excepsit-legacy-668
  (file-write-date "*")
  file-error)

(check-for-bug :excepsit-legacy-672
  (fill 'x #\x)
  type-error)

(check-for-bug :excepsit-legacy-676
  (fill (make-list 3) 'x :start nil)
  type-error)

(check-for-bug :excepsit-legacy-680
  (fill (make-list 3) 'x :start -1)
  type-error)

(check-for-bug :excepsit-legacy-684
  (fill (make-list 3) 'x :start 1 :end -1)
  type-error)

(check-for-bug :excepsit-legacy-688
  (fill-pointer "abc")
  type-error)

(check-for-bug :excepsit-legacy-692
  (find #\x 'x)
  type-error)

(check-for-bug :excepsit-legacy-696
  (find-class '#:nonexistent t)
  error)

(check-for-bug :excepsit-legacy-700
  (progn
    (defgeneric foo36 (x y))
    (find-method #'foo36 nil (list (find-class 'number))))
  error)

(check-for-bug :excepsit-legacy-706
  (progn
    (defgeneric foo37 (x))
    (find-method #'foo37 nil (list (find-class 'number))))
  error)

(check-for-bug :excepsit-legacy-712
  (finish-output '*terminal-io*)
  type-error)

(check-for-bug :excepsit-legacy-716
  (float-digits 2/3)
  type-error)

(check-for-bug :excepsit-legacy-720
  (float-precision 2/3)
  type-error)

(check-for-bug :excepsit-legacy-724
  (float-radix 2/3)
  type-error)

(check-for-bug :excepsit-legacy-728
  (float-sign 2/3)
  type-error)

(check-for-bug :excepsit-legacy-732
  (float-sign -4.5 2/3)
  type-error)

(check-for-bug :excepsit-legacy-736
  (fmakunbound '(psetf aref))
  type-error)

(check-for-bug :excepsit-legacy-740
  (force-output '*terminal-io*)
  type-error)

(check-for-bug :excepsit-legacy-744
  (funcall 'foo38)
  undefined-function)

(check-for-bug :excepsit-legacy-748
  (funcall 'and)
  undefined-function)

(check-for-bug :excepsit-legacy-752
  (gcd 4 3/4)
  type-error)

(check-for-bug :excepsit-legacy-756
  (gensym #\x)
  type-error)

(check-for-bug :excepsit-legacy-760
  (gentemp 't)
  type-error)

(check-for-bug :excepsit-legacy-764
  (gentemp "X" 24)
  type-error)

(check-for-bug :excepsit-legacy-768
  (get "a" 'x)
  type-error)

(check-for-bug :excepsit-legacy-772
  (get-dispatch-macro-character #\0 #\#)
  error)

(check-for-bug :excepsit-legacy-776
  (graphic-char-p 33)
  type-error)

(check-for-bug :excepsit-legacy-780
  (hash-table-rehash-size *readtable*)
  type-error)

(check-for-bug :excepsit-legacy-784
  (hash-table-rehash-threshold *package*)
  type-error)

(check-for-bug :excepsit-legacy-788
  (hash-table-size *random-state*)
  type-error)

(check-for-bug :excepsit-legacy-792
  (hash-table-test '#(a b c))
  type-error)

(check-for-bug :excepsit-legacy-796
  (imagpart #\c)
  type-error)

(check-for-bug :excepsit-legacy-800
  (in-package "FOO39")
  package-error)

(check-for-bug :excepsit-legacy-807
  (input-stream-p (pathname "abc"))
  type-error)

(check-for-bug :excepsit-legacy-811
  (integer-decode-float 2/3)
  type-error)

(check-for-bug :excepsit-legacy-815
  (integer-length 0.0)
  type-error)

(check-for-bug :excepsit-legacy-819
  (interactive-stream-p (pathname "abc"))
  type-error)

(check-for-bug :excepsit-legacy-823
  (invoke-restart 'foo40)
  control-error)

(check-for-bug :excepsit-legacy-827
  (invoke-restart-interactively 'foo41)
  control-error)

(check-for-bug :excepsit-legacy-831
  (isqrt -1)
  type-error)

(check-for-bug :excepsit-legacy-835
  (isqrt #c(3 4))
  type-error)

(check-for-bug :excepsit-legacy-839
  (last '(a b c) -1)
  type-error)

(check-for-bug :excepsit-legacy-843
  (lcm 4/7 8)
  type-error)

(check-for-bug :excepsit-legacy-847
  (length 'x)
  type-error)

(check-for-bug :excepsit-legacy-851
  (list-length 'x)
  type-error)

(check-for-bug :excepsit-legacy-855
  (list-length '(x . y))
  type-error)

(check-for-bug :excepsit-legacy-859
  (load "./128347234.lsp")
  file-error)

(check-for-bug :excepsit-legacy-863
  (load "*.lsp")
  file-error)

(check-for-bug :excepsit-legacy-867
  (load-logical-pathname-translations "FOO41")
  error)

(check-for-bug :excepsit-legacy-871
  (logand -3 2.3)
  type-error)

(check-for-bug :excepsit-legacy-875
  (logbitp -1 5)
  type-error)

(check-for-bug :excepsit-legacy-879
  (logbitp 2 3/7)
  type-error)

(check-for-bug :excepsit-legacy-883
  (logcount #*01010011)
  type-error)

(check-for-bug :excepsit-legacy-887
  (logical-pathname '#(#\A #\B))
  type-error)

(check-for-bug :excepsit-legacy-891
  (logical-pathname-translations '#(#\A #\B))
  type-error)

(check-for-bug :excepsit-legacy-895
  (lower-case-p 33)
  type-error)

(check-for-bug :excepsit-legacy-899
  (make-broadcast-stream (make-string-input-stream "abc"))
  type-error)

(check-for-bug :excepsit-legacy-903
  (make-concatenated-stream (make-string-output-stream))
  type-error)

(check-for-bug :excepsit-legacy-907
  (progn
    (defclass foo42 () ())
    (make-instance 'foo42 :x 1))
  error)

(check-for-bug :excepsit-legacy-913
  (make-list -1)
  type-error)

(check-for-bug :excepsit-legacy-917
  (progn
    (defstruct foo43)
    (make-load-form (make-foo43)))
  error)

(check-for-bug :excepsit-legacy-923
  (make-random-state 'x)
  type-error)

(check-for-bug :excepsit-legacy-927
  (make-sequence 'x 5)
  type-error)

(check-for-bug :excepsit-legacy-931
  (make-sequence 'sequence 5)
  type-error)

(check-for-bug :excepsit-legacy-935
  (make-sequence '(string 3) 4)
  type-error)

(check-for-bug :excepsit-legacy-939
  (make-symbol 'x)
  type-error)

(check-for-bug :excepsit-legacy-943
  (make-synonym-stream *terminal-io*)
  type-error)

(check-for-bug :excepsit-legacy-947
  (make-two-way-stream (make-string-input-stream "abc") (make-string-input-stream "def"))
  type-error)

(check-for-bug :excepsit-legacy-951
  (make-two-way-stream (make-string-output-stream) (make-string-output-stream))
  type-error)

(check-for-bug :excepsit-legacy-955
  (makunbound "xx")
  type-error)

(check-for-bug :excepsit-legacy-959
  (map 'x #'identity "abc")
  type-error)

(check-for-bug :excepsit-legacy-963
  (map '(string 3) #'identity "ab")
  type-error)

(check-for-bug :excepsit-legacy-967
  (max 3 #c(4 0.0))
  type-error)

(check-for-bug :excepsit-legacy-971
  (merge '(vector * 5) '(3 1) '(2 4) #'<)
  type-error)

(check-for-bug :excepsit-legacy-975
  (min 3 #c(4 0.0))
  type-error)

(check-for-bug :excepsit-legacy-979
  (minusp #c(4 -3/4))
  type-error)

(check-for-bug :excepsit-legacy-983
  (muffle-warning)
  control-error)

(check-for-bug :excepsit-legacy-987
  (name-char '#(#\N #\u #\l))
  type-error)

(check-for-bug :excepsit-legacy-991
  (nbutlast '(a b c) -1)
  type-error)

(check-for-bug :excepsit-legacy-995
  (nbutlast '#(a b c))
  type-error)

(check-for-bug :excepsit-legacy-999
  (no-applicable-method #'cons)
  error)

(check-for-bug :excepsit-legacy-1003
  (no-next-method #'print-object (find-method #'print-object nil (list (find-class 'standard-object) (find-class 't))))
  error)

(check-for-bug :excepsit-legacy-1007
  (notany '(lambda (x) x) nil)
  type-error)

(check-for-bug :excepsit-legacy-1011
  (notany #'identity 'x)
  type-error)

(check-for-bug :excepsit-legacy-1015
  (notevery '(lambda (x) x) nil)
  type-error)

(check-for-bug :excepsit-legacy-1019
  (notevery #'identity 'x)
  type-error)

(check-for-bug :excepsit-legacy-1023
  (nthcdr 2 '(a . b))
  type-error)

(check-for-bug :excepsit-legacy-1027
  (oddp 3.5)
  type-error)

#+UNIX
(check-for-bug :excepsit-legacy-1032
  (progn (open "/etc/passwd" :direction :input :if-exists :error) (/ 0))
  division-by-zero)

#+UNIX
(check-for-bug :excepsit-legacy-1037
  (progn (open "/etc/nonexistent" :direction :input :if-exists :error) (/ 0))
  file-error)

(check-for-bug :excepsit-legacy-1041
  (open "./foo44nonexistent" :direction :input :if-does-not-exist :error)
  file-error)

(check-for-bug :excepsit-legacy-1045
  (open "./*" :direction :input)
  file-error)

#+UNIX
(check-for-bug :excepsit-legacy-1050
  (open "/etc/mtab" :direction :input :external-format 'mtab-entries)
  error)

(check-for-bug :excepsit-legacy-1054
  (open-stream-p (pathname "foo45"))
  type-error)

(check-for-bug :excepsit-legacy-1058
  (output-stream-p (pathname "foo46"))
  type-error)

(check-for-bug :excepsit-legacy-1062
  (package-name 47)
  type-error)

(check-for-bug :excepsit-legacy-1066
  (package-nicknames (pathname "foo47"))
  type-error)

(check-for-bug :excepsit-legacy-1070
  (package-shadowing-symbols (vector 'a 'b 'c))
  type-error)

(check-for-bug :excepsit-legacy-1074
  (package-use-list (list 'a 'b 'c))
  type-error)

(check-for-bug :excepsit-legacy-1078
  (package-used-by-list (list 'a 'b 'c))
  type-error)

(check-for-bug :excepsit-legacy-1082
  (parse-integer "x-y")
  error)

(check-for-bug :excepsit-legacy-1086
  (parse-namestring (coerce (list #\f #\o #\o (code-char 0) #\4 #\8) 'string))
  parse-error)

(check-for-bug :excepsit-legacy-1090
  (parse-namestring "foo48:a" (logical-pathname "foo49:"))
  error)

(check-for-bug :excepsit-legacy-1094
  (pathname-match-p 34 "*")
  type-error)

(check-for-bug :excepsit-legacy-1098
  (pathname-match-p "x" 34)
  type-error)

(check-for-bug :excepsit-legacy-1102
  (peek-char nil (make-string-input-stream "") t)
  end-of-file)

(check-for-bug :excepsit-legacy-1106
  (peek-char #\space (make-string-input-stream "") t)
  end-of-file)

#|					; It's not clear why peek-char should signal an error, where read-char and
;;  read-line don't. Kent Pitman says: "Sounds like a mess."
(peek-char nil (make-string-input-stream "") nil nil t)
end-of-file
|#

(check-for-bug :excepsit-legacy-1116
 (phase 'x)
 type-error)

(check-for-bug :excepsit-legacy-1120
 (plusp #c(0 4.2))
 type-error)

(check-for-bug :excepsit-legacy-1125
 (pprint-dispatch nil t)
 type-error)

(check-for-bug :excepsit-legacy-1130
 (pprint-exit-if-list-exhausted)
 error)

(check-for-bug :excepsit-legacy-1135
 (pprint-indent nil 2)
 error)

(check-for-bug :excepsit-legacy-1140
 (let ((x (make-string-output-stream)))
   (pprint-logical-block (x nil :prefix 24)))
 type-error)

(check-for-bug :excepsit-legacy-1146
 (let ((x (make-string-output-stream)))
   (pprint-logical-block (x nil :prefix "a" :per-line-prefix "b")))
 error)

(check-for-bug :excepsit-legacy-1152
 (pprint-newline :fresh)
 type-error)

(check-for-bug :excepsit-legacy-1157
 (pprint-pop)
 error)

(check-for-bug :excepsit-legacy-1161
 (pprint-tab :paragraph 0 1)
 error)

(check-for-bug :excepsit-legacy-1165
 (let ((*print-readably* t)) (print-unreadable-object (nil *standard-output*)))
 print-not-readable)

(check-for-bug :excepsit-legacy-1169
 (probe-file "*")
 file-error)

(check-for-bug :excepsit-legacy-1173
 (provide 25)
 type-error)

(check-for-bug :excepsit-legacy-1177
 (random -2.3)
 type-error)

(check-for-bug :excepsit-legacy-1181
 (rational #c(2.4 -0.3))
 type-error)

(check-for-bug :excepsit-legacy-1185
 (rationalize #c(2.4 -0.3))
 type-error)

(check-for-bug :excepsit-legacy-1189
 (read (make-string-input-stream "((a b)") nil)
 end-of-file)

(check-for-bug :excepsit-legacy-1193
 (read (make-string-input-stream " ") t)
 end-of-file)

(check-for-bug :excepsit-legacy-1197
 (read-byte (pathname "foo50"))
 type-error)

(check-for-bug :excepsit-legacy-1201
 (read-byte (make-string-input-stream "abc"))
 error)

(check-for-bug :excepsit-legacy-1205
 (let ((filename "./foo51.bin"))
   (with-open-file (s filename :direction :output
		      :if-exists :overwrite
		      :if-does-not-exist :create))
   (with-open-file (s filename :direction :input
		      :element-type '(unsigned-byte 8))
		   (read-byte s t)))
 end-of-file)

(check-for-bug :excepsit-legacy-1215
 (not (delete-file "./foo51.bin"))
 nil)

(check-for-bug :excepsit-legacy-1219
 (let ((filename "./foo52.txt"))
   (with-open-file (s filename :direction :output
		      :if-exists :overwrite
		      :if-does-not-exist :create))
   (with-open-file (s filename :direction :input)
		   (read-char s t)))
 end-of-file)

(check-for-bug :excepsit-legacy-1228
 (not (delete-file "./foo52.txt"))
 nil)

(check-for-bug :excepsit-legacy-1232
 (let ((filename "./foo53.txt"))
   (with-open-file (s filename :direction :output
		      :if-exists :overwrite
		      :if-does-not-exist :create))
   (with-open-file (s filename :direction :input)
		   (read-char-no-hang s t)))
 end-of-file)

(check-for-bug :excepsit-legacy-1241
 (not (delete-file "./foo53.txt"))
 nil)

(check-for-bug :excepsit-legacy-1245
 (read-from-string "((a b))" nil nil :end 6)
 end-of-file)

(check-for-bug :excepsit-legacy-1249
 (read-from-string " () () " t nil :start 3 :end 4)
 end-of-file)

(check-for-bug :excepsit-legacy-1253
 (read-line (make-string-input-stream "") t)
 end-of-file)

(check-for-bug :excepsit-legacy-1257
 (read-sequence (list 1 2 3) (make-string-input-stream "") :start nil)
 type-error)

(check-for-bug :excepsit-legacy-1261
 (read-sequence (list 1 2 3) (make-string-input-stream "") :end -1)
 type-error)

(check-for-bug :excepsit-legacy-1265
 (readtable-case nil)
 type-error)

(check-for-bug :excepsit-legacy-1269
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) ':unknown))
 type-error)

(check-for-bug :excepsit-legacy-1273
 (realpart #\c)
 type-error)

(check-for-bug :excepsit-legacy-1277
 (progn
   (defclass foo54 () ())
   (reinitialize-instance (make-instance 'foo54) :dummy 0))
 error)

(check-for-bug :excepsit-legacy-1283
 (remove #\x 'x)
 type-error)

(check-for-bug :excepsit-legacy-1287
 (remove-duplicates 'abba)
 type-error)

(check-for-bug :excepsit-legacy-1291
 (remprop 55 'abc)
 type-error)

(check-for-bug :excepsit-legacy-1295
 (rplaca nil 5)
 type-error)

(check-for-bug :excepsit-legacy-1299
 (rplacd nil 5)
 type-error)

(check-for-bug :excepsit-legacy-1303
 (scale-float 2/3 -1)
 type-error)

(check-for-bug :excepsit-legacy-1307
 (scale-float 3.4 1.0)
 type-error)

(check-for-bug :excepsit-legacy-1311
 (set-dispatch-macro-character #\0 #\# #'(lambda (s c n) (loop)))
 error)

(check-for-bug :excepsit-legacy-1315
 (set-pprint-dispatch '(vector * 2) nil #c(3 4))
 error)

(check-for-bug :excepsit-legacy-1319
 (sin 'x)
 type-error)

(check-for-bug :excepsit-legacy-1323
 (sinh 'x)
 type-error)

(check-for-bug :excepsit-legacy-1327
 (sleep -1)
 type-error)

(check-for-bug :excepsit-legacy-1331
 (progn
   (defclass foo55 () (a))
   (slot-boundp (make-instance 'foo55) ':a))
 error)

(check-for-bug :excepsit-legacy-1337
 (progn
   (defclass foo56 () (a))
   (slot-makunbound (make-instance 'foo56) ':a))
 error)

(check-for-bug :excepsit-legacy-1343
 (slot-missing (find-class 't) nil ':a 'setf)
 error)

(check-for-bug :excepsit-legacy-1347
 (slot-unbound (find-class 't) nil ':a)
 unbound-slot)

(check-for-bug :excepsit-legacy-1351
 (progn
   (defclass foo57 () (a))
   (slot-value (make-instance 'foo57) ':a))
 error)

(check-for-bug :excepsit-legacy-1357
 (some '(lambda (x) x) nil)
 type-error)

(check-for-bug :excepsit-legacy-1361
 (some #'identity 'x)
 type-error)

(check-for-bug :excepsit-legacy-1365
 (special-operator-p '(and x y))
 type-error)

(check-for-bug :excepsit-legacy-1369
 (special-operator-p '(setf aref))
 type-error)

(check-for-bug :excepsit-legacy-1373
 (sqrt 'x)
 type-error)

(check-for-bug :excepsit-legacy-1377
 (standard-char-p 33)
 type-error)

(check-for-bug :excepsit-legacy-1381
 (stream-element-type '*terminal-io)
 type-error)

(check-for-bug :excepsit-legacy-1385
 (string 33)
 type-error)

(check-for-bug :excepsit-legacy-1389
 (symbol-function 33)
 type-error)

(check-for-bug :excepsit-legacy-1393
 (symbol-function ':compile)
 undefined-function)

(check-for-bug :excepsit-legacy-1397
 (symbol-macrolet ((t true)))
 program-error)

(check-for-bug :excepsit-legacy-1401
 (symbol-macrolet ((*print-pretty* (stream-print-pretty *standard-output*))))
 program-error)

(check-for-bug :excepsit-legacy-1405
 (symbol-macrolet ((foo58 t)) (declare (special foo58)))
 program-error)

(check-for-bug :excepsit-legacy-1409
 (symbol-name '(setf foo59))
 type-error)

(check-for-bug :excepsit-legacy-1413
 (symbol-package '(setf foo59))
 type-error)

(check-for-bug :excepsit-legacy-1417
 (symbol-plist '(setf foo59))
 type-error)

(check-for-bug :excepsit-legacy-1421
 (symbol-value '(setf foo59))
 type-error)

(check-for-bug :excepsit-legacy-1425
 (symbol-value '#:nonexistent)
 unbound-variable)

(check-for-bug :excepsit-legacy-1429
 (tan 'x)
 type-error)

(check-for-bug :excepsit-legacy-1433
 (tanh 'x)
 type-error)

(check-for-bug :excepsit-legacy-1437
 (throw '#:nonexistent nil)
 control-error)

(check-for-bug :excepsit-legacy-1441
 (translate-logical-pathname (make-broadcast-stream))
 type-error)

(check-for-bug :excepsit-legacy-1445
 (translate-logical-pathname (logical-pathname "foo61:"))
 file-error)

#-CLISP
(check-for-bug :excepsit-legacy-1450
 ;; clisp explicitly allows symbols as pathnames
 (translate-pathname 'x "x" "y")
 type-error)

#-CLISP
(check-for-bug :excepsit-legacy-1456
 ;; clisp explicitly allows symbols as pathnames
 (translate-pathname "a" '* '*)
 type-error)

(check-for-bug :excepsit-legacy-1461
 (translate-pathname "x" "y" "z")
 error)

(check-for-bug :excepsit-legacy-1465
 (truename "./foo62nonexistent")
 file-error)

(check-for-bug :excepsit-legacy-1469
 (truename "./*/x")
 file-error)

(check-for-bug :excepsit-legacy-1473
 (typep nil 'values)
 error)

(check-for-bug :excepsit-legacy-1477
 (typep #'cons '(values t))
 error)

(check-for-bug :excepsit-legacy-1481
 (typep #'cons '(function (t t) list))
 error)

(check-for-bug :excepsit-legacy-1485
 (unexport ':foo63)
 package-error)

(check-for-bug :excepsit-legacy-1489
 (progn
   (defpackage "FOO64" (:export "XYZ"))
   (defpackage "FOO65" (:export "XYZ"))
   (defpackage "FOO66" (:use "FOO64" "FOO65") (:shadow "XYZ"))
   (unintern (find-symbol "XYZ" (find-package "FOO66")) (find-package "FOO66")))
 error)

;;  update-instance-for-different-class too complicated

;;  update-instance-for-redefined-class too complicated

(check-for-bug :excepsit-legacy-1501
 (upper-case-p 33)
 type-error)

(check-for-bug :excepsit-legacy-1505
 (values-list '(a b . c))
 type-error)

(check-for-bug :excepsit-legacy-1509
 (vector-pop "foo67")
 type-error)

(check-for-bug :excepsit-legacy-1513
 (vector-pop (make-array 10 :fill-pointer 0))
 error)

(check-for-bug :excepsit-legacy-1517
 (vector-push 'x (make-array 10))
 error)

(check-for-bug :excepsit-legacy-1521
 (let ((a (make-array 5
		      :fill-pointer 0
		      :adjustable nil)))
   (if (adjustable-array-p a)
       'error
       (dotimes (i 100) (vector-push-extend 'x a))))
 error)

(check-for-bug :excepsit-legacy-1530
 (warn (make-condition 'error))
 type-error)

(check-for-bug :excepsit-legacy-1534
 (warn (make-condition 'warning) "x")
 type-error)

(check-for-bug :excepsit-legacy-1538
 (warn 'error)
 type-error)

(check-for-bug :excepsit-legacy-1542
 (wild-pathname-p #\x)
 type-error)

(check-for-bug :excepsit-legacy-1546
 (write-byte 1 (pathname "foo67"))
 type-error)

(check-for-bug :excepsit-legacy-1550
 (write-byte 1 (make-string-output-stream))
 error)

(check-for-bug :excepsit-legacy-1554
 (write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :start nil)
 type-error)

(check-for-bug :excepsit-legacy-1558
 (write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :end -1)
 type-error)

(check-for-bug :excepsit-legacy-1562
 (zerop 'x)
 type-error)

;;  section 2.3.1.1
(check-for-bug :excepsit-legacy-1567
 (read-from-string "-35/000")
 reader-error)				; not division-by-zero!

(check-for-bug :excepsit-legacy-1571
 (read-from-string "31e300")
 reader-error)				; not floating-point-overflow!



