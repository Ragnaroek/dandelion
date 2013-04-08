;;; 13 characters -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(check-for-bug :section13-legacy-6
  (subtypep 'base-char 'character)
  T)


(check-for-bug :section13-legacy-11
  (subtypep 'standard-char 'base-char)
  t)


(check-for-bug :section13-legacy-16
  (subtypep 'extended-char 'character)
  t
  "Type EXTENDED-CHAR

Supertypes:

extended-char, character, t")

;;; char= etc


(check-for-bug :section13-legacy-28
  (char= #\d #\d)
  t)


(check-for-bug :section13-legacy-33
  (char= #\A #\a)
  nil)


(check-for-bug :section13-legacy-38
  (char= #\d #\x)
  nil)


(check-for-bug :section13-legacy-43
  (char= #\d #\D)
  nil)


(check-for-bug :section13-legacy-48
  (char/= #\d #\d)
  nil)


(check-for-bug :section13-legacy-53
  (char/= #\d #\x)
  t)


(check-for-bug :section13-legacy-58
  (char/= #\d #\D)
  t)


(check-for-bug :section13-legacy-63
  (char= #\d #\d #\d #\d)
  t)


(check-for-bug :section13-legacy-68
  (char/= #\d #\d #\d #\d)
  nil)


(check-for-bug :section13-legacy-73
  (char= #\d #\d #\x #\d)
  nil)


(check-for-bug :section13-legacy-78
  (char/= #\d #\d #\x #\d)
  nil)


(check-for-bug :section13-legacy-83
  (char= #\d #\y #\x #\c)
  nil)


(check-for-bug :section13-legacy-88
  (char/= #\d #\y #\x #\c)
  t)


(check-for-bug :section13-legacy-93
  (char= #\d #\c #\d)
  nil)


(check-for-bug :section13-legacy-98
  (char/= #\d #\c #\d)
  nil)


(check-for-bug :section13-legacy-103
  (char< #\d #\x)
  t)


(check-for-bug :section13-legacy-108
  (char<= #\d #\x)
  t)


(check-for-bug :section13-legacy-113
  (char< #\d #\d)
  nil)


(check-for-bug :section13-legacy-118
  (char<= #\d #\d)
  t)


(check-for-bug :section13-legacy-123
  (char< #\a #\e #\y #\z)
  t)


(check-for-bug :section13-legacy-128
  (char<= #\a #\e #\y #\z)
  t)


(check-for-bug :section13-legacy-133
  (char< #\a #\e #\e #\y)
  nil)


(check-for-bug :section13-legacy-138
  (char<= #\a #\e #\e #\y)
  t)


(check-for-bug :section13-legacy-143
  (char> #\e #\d)
  t)


(check-for-bug :section13-legacy-148
  (char>= #\e #\d)
  t)


(check-for-bug :section13-legacy-153
  (char> #\d #\c #\b #\a)
  t)


(check-for-bug :section13-legacy-158
  (char>= #\d #\c #\b #\a)
  t)


(check-for-bug :section13-legacy-163
  (char> #\d #\d #\c #\a)
  nil)


(check-for-bug :section13-legacy-168
  (char>= #\d #\d #\c #\a)
  t)


(check-for-bug :section13-legacy-173
  (char> #\e #\d #\b #\c #\a)
  nil)


(check-for-bug :section13-legacy-178
  (char>= #\e #\d #\b #\c #\a)
  nil)


(check-for-bug :section13-legacy-183
  (char> #\z #\A)
  #+(or cmu sbcl clisp ecls) T
  #-(or cmu sbcl clisp ecls) fill-this-in)


(check-for-bug :section13-legacy-189
  (char> #\Z #\a)
  #+(or cmu sbcl clisp ecls) nil
  #-(or cmu sbcl clisp ecls) fill-this-in)


(check-for-bug :section13-legacy-195
  (char-equal #\A #\a)
  t)


(check-for-bug :section13-legacy-200
  (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp)
  (#\A #\a #\b #\B #\c #\C))


(check-for-bug :section13-legacy-205
  (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char<)
  #+(or cmu sbcl clisp ecls) (#\A #\B #\C #\a #\b #\c)
  #-(or cmu sbcl clisp ecls) fill-this-in)
					;  (#\A #\B #\C #\a #\b #\c) ;Implementation A
					;  (#\a #\b #\c #\A #\B #\C) ;Implementation B
					;  (#\a #\A #\b #\B #\c #\C) ;Implementation C
					;  (#\A #\a #\B #\b #\C #\c) ;Implementation D
					;  (#\A #\B #\a #\b #\C #\c) ;Implementation E

;;; character


(check-for-bug :section13-legacy-218
  (character #\a)
  #\a)


(check-for-bug :section13-legacy-223
  (character "a")
  #\a)

(check-for-bug :section13-legacy-227
  (character 'a)
  #\A)


(check-for-bug :section13-legacy-232
  (character '\a)
  #\a)


(check-for-bug :section13-legacy-237
  (character 65.0)
  TYPE-ERROR)


(check-for-bug :section13-legacy-242
  (character 'apple)
  TYPE-ERROR)


;;; alpha-char-p


(check-for-bug :section13-legacy-250
  (alpha-char-p #\a)
  t)


(check-for-bug :section13-legacy-255
  (alpha-char-p #\5)
  nil)


(check-for-bug :section13-legacy-260
  (alpha-char-p #\Newline)
  nil)

;;; alphanumericp


(check-for-bug :section13-legacy-267
  (alphanumericp #\Z)
  t)


(check-for-bug :section13-legacy-272
  (alphanumericp #\9)
  t)


(check-for-bug :section13-legacy-277
  (alphanumericp #\Newline)
  nil)


(check-for-bug :section13-legacy-282
  (alphanumericp #\#)
  nil)

;;; digit-char


(check-for-bug :section13-legacy-289
  (digit-char 0)
  #\0)


(check-for-bug :section13-legacy-294
  (digit-char 10 11)
  #\A)


(check-for-bug :section13-legacy-299
  (digit-char 10 10)
  nil)


(check-for-bug :section13-legacy-304
  (digit-char 7)
  #\7)


(check-for-bug :section13-legacy-309
  (digit-char 12)
  nil)


(check-for-bug :section13-legacy-314
  (digit-char 12 16)
  #\C)


(check-for-bug :section13-legacy-319
  (digit-char 6 2)
  nil)


(check-for-bug :section13-legacy-324
  (digit-char 1 2)
  #\1)

;;; digit-char-p


(check-for-bug :section13-legacy-331
  (digit-char-p #\5)
  5)


(check-for-bug :section13-legacy-336
  (digit-char-p #\5 2)
  nil)


(check-for-bug :section13-legacy-341
  (digit-char-p #\A)
  nil)


(check-for-bug :section13-legacy-346
  (digit-char-p #\a)
  nil)


(check-for-bug :section13-legacy-351
  (digit-char-p #\A 11)
  10)


(check-for-bug :section13-legacy-356
  (digit-char-p #\a 11)
  10)


(check-for-bug :section13-legacy-361
  (mapcar #'(lambda (radix)
              (map 'list #'(lambda (x) (digit-char-p x radix))
                   "059AaFGZ"))
          '(2 8 10 16 36))
  ((0 NIL NIL NIL NIL NIL NIL NIL)
   (0 5 NIL NIL NIL NIL NIL NIL)
   (0 5 9 NIL NIL NIL NIL NIL)
   (0 5 9 10 10 15 NIL NIL)
   (0 5 9 10 10 15 16 35)))

;;; graphic-char


(check-for-bug :section13-legacy-375
  (graphic-char-p #\G)
  t)


(check-for-bug :section13-legacy-380
  (graphic-char-p #\#)
  t)


(check-for-bug :section13-legacy-385
  (graphic-char-p #\Space)
  t)


(check-for-bug :section13-legacy-390
  (graphic-char-p #\Newline)
  nil)

;;; standard-char-p


(check-for-bug :section13-legacy-397
  (standard-char-p #\Space)
  t)


(check-for-bug :section13-legacy-402
  (standard-char-p #\~)
  t)

;;; char-upcase


(check-for-bug :section13-legacy-409
  (char-upcase #\a)
  #\A)


(check-for-bug :section13-legacy-414
  (char-upcase #\A)
  #\A)


(check-for-bug :section13-legacy-419
  (char-downcase #\a)
  #\a)


(check-for-bug :section13-legacy-424
  (char-downcase #\A)
  #\a)


(check-for-bug :section13-legacy-429
  (char-upcase #\9)
  #\9)


(check-for-bug :section13-legacy-434
  (char-downcase #\9)
  #\9)


(check-for-bug :section13-legacy-439
  (char-upcase #\@)
  #\@)


(check-for-bug :section13-legacy-444
  (char-downcase #\@)
  #\@)

;; Note that this next example might run for a very long time in
;; some implementations if CHAR-CODE-LIMIT happens to be very large
;; for that implementation.

(check-for-bug :section13-legacy-452
  (dotimes (code char-code-limit)
    (let ((char (code-char code)))
      (when char
        (unless (cond ((upper-case-p char)
                       (char= (char-upcase
                               (char-downcase char)) char))
                      ((lower-case-p char)
                       (char= (char-downcase
                               (char-upcase char)) char))
                      (t (and (char= (char-upcase
                                      (char-downcase char)) char)
                              (char= (char-downcase
                                      (char-upcase char)) char))))
          (return char)))))
  NIL)

;;; upper-case-p

(check-for-bug :section13-legacy-471
  (upper-case-p #\A)
  t)


(check-for-bug :section13-legacy-476
  (upper-case-p #\a)
  nil)


(check-for-bug :section13-legacy-481
  (both-case-p #\a)
  t)


(check-for-bug :section13-legacy-486
  (both-case-p #\5)
  nil)


(check-for-bug :section13-legacy-491
  (lower-case-p #\5)
  nil)


(check-for-bug :section13-legacy-496
  (upper-case-p #\5)
  nil)

;;; char-code-limit


(check-for-bug :section13-legacy-503
  (>= char-code-limit 96)
  t)

;;; char-name


(check-for-bug :section13-legacy-510
  (char-name #\ )
  "Space")


(check-for-bug :section13-legacy-515
  (char-name #\Space)
  "Space")


(check-for-bug :section13-legacy-520
  (char-name #\Page)
  "Page")


(check-for-bug :section13-legacy-525
  (char-name #\a)
  #+(or cmu sbcl ecls) nil
  #+clisp "LATIN_SMALL_LETTER_A"
  #-(or cmu sbcl clisp ecls) fill-this-in)
;; NIL OR "LOWERCASE-a" OR  "Small-A" OR  "LA01"


(check-for-bug :section13-legacy-533
  (char-name #\A)
  #+(or cmu sbcl ecls) nil
  #+clisp "LATIN_CAPITAL_LETTER_A"
  #-(or cmu sbcl clisp ecls) fill-this-in)
;;  NIL OR "UPPERCASE-A" OR  "Capital-A" OR  "LA02"

;; Even though its CHAR-NAME can vary, #\A prints as #\A

(check-for-bug :section13-legacy-542
  (prin1-to-string (read-from-string (format nil "#\\~A" (or (char-name #\A) "A"))))
  "#\\A")

;;; name-char


(check-for-bug :section13-legacy-549
  (name-char 'space)
  #\Space)


(check-for-bug :section13-legacy-554
  (name-char "space")
  #\Space)


(check-for-bug :section13-legacy-559
  (name-char "Space")
  #\Space)



(check-for-bug :section13-legacy-565
  (let ((x (char-name #\a)))
    (or (not x) (eql (name-char x) #\a)))
  t)















