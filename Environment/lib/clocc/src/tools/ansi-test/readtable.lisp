;;; based on v1.1.1.1 -*- mode: lisp -*-
(in-package :cl-user)

;; ****************************************************************************
;; *      Rosenmueller  tel.340 Testquelle READTABLE.que    23.03.1988        *
;; ****************************************************************************

(check-for-bug :readtable-legacy-8
  (prin1-to-string (setq *readtable* (copy-readtable nil)))
  "#<SYSTEM::%TYPE-READTABLE #<SYSTEM::%TYPE-SIMPLE-VECTOR SYSTEM::%TYPE-UNSIGNED-WORD-POINTER
 00000000 00000000 00000000 00000000 00040001 00000004 00040004 00000000
 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
 00010004 011E0075 00010001 02250001 00A50395 0C010401 14010535 00010B41
 06010601 06010601 06010601 06010601 06010601 00850701 08010001 00010001
 0D010061 12010E01 00011501 00010001 00010001 00010001 00010001 0F010001
 00010001 13011101 00010001 00010001 00011001 00010001 00010902 00010001
 0D010055 12010E01 00011501 00010001 00010001 00010001 00010001 0F010001
 00010001 13011101 00010001 00010001 00011001 00C50001 00B50A03 00010001>
NIL>")

(check-for-bug :readtable-legacy-21
  (setq $ 23)
  23)

(check-for-bug :readtable-legacy-25
  (defun single-dollar-reader (stream char)
    (declare (ignore stream))
    (intern (string char)))
  SINGLE-DOLLAR-READER)

(check-for-bug :readtable-legacy-31
  (set-macro-character #\$ #'single-dollar-reader)
  T)

(check-for-bug :readtable-legacy-35
  $
  23)

(check-for-bug :readtable-legacy-39
  45
  45
  ;;  => 23 => 45
  (prin1-to-string (get-macro-character #\$))
  "#<SYSTEM::%TYPE-CLOSURE SINGLE-DOLLAR-READER
NIL
NIL
(LAMBDA (STREAM CHAR) (DECLARE (IGNORE STREAM)) (INTERN (STRING CHAR)))>")


(check-for-bug :readtable-legacy-50
  (progn (setq *readtable* (copy-readtable nil)) t)
  t)

(check-for-bug :readtable-legacy-54
  (sys::rt-bitmask-char #\" )
  117)

(check-for-bug :readtable-legacy-58
  (sys::rt-bitmask-char #\( )
  917)

(check-for-bug :readtable-legacy-62
  (sys::rt-bitmask-char #\) )
  165)

(check-for-bug :readtable-legacy-66
  (sys::rt-bitmask-char #\\ )
  2306)

(check-for-bug :readtable-legacy-70
  (sys::rt-bitmask-char #\x )
  4097)

(check-for-bug :readtable-legacy-74
  (sys::rt-bitmask-char #\y )
  1)

(check-for-bug :readtable-legacy-78
  (set-syntax-from-char #\" #\( )
  T)

(check-for-bug :readtable-legacy-82
  (sys::rt-bitmask-char #\" )
  917)

(check-for-bug :readtable-legacy-86
  (sys::rt-bitmask-char #\( )
  917)

(check-for-bug :readtable-legacy-90
  (sys::rt-bitmask-char #\) )
  165)

(check-for-bug :readtable-legacy-94
  (sys::rt-bitmask-char #\\ )
  2306
  ;;  *readtable* nil
  ;;  *readtable* cl-standard
  (progn (setq doppelquote-liston-readtable (copy-readtable)) t)
  t)

(check-for-bug :readtable-legacy-102
  (sys::rt-bitmask-char #\" doppelquote-liston-readtable )
  917)

(check-for-bug :readtable-legacy-106
  (sys::rt-bitmask-char #\( doppelquote-liston-readtable )
  917)

(check-for-bug :readtable-legacy-110
  (sys::rt-bitmask-char #\) doppelquote-liston-readtable )
  165)

(check-for-bug :readtable-legacy-114
  (sys::rt-bitmask-char #\\ doppelquote-liston-readtable )
  2306)

(check-for-bug :readtable-legacy-118
  '"1 2 3)
(1 2 3))

(check-for-bug :readtable-legacy-122
 (set-syntax-from-char #\" #\\ )
T)

(check-for-bug :readtable-legacy-126
 (sys::rt-bitmask-char #\" )
2306)

(check-for-bug :readtable-legacy-130
 (sys::rt-bitmask-char #\( )
917)

(check-for-bug :readtable-legacy-134
 (sys::rt-bitmask-char #\) )
165)

(check-for-bug :readtable-legacy-138
 (sys::rt-bitmask-char #\\ )
2306)

(check-for-bug :readtable-legacy-142
 (progn (setq doppelquote-backslash-readtable (copy-readtable)) t)
T)

(check-for-bug :readtable-legacy-146
 (sys::rt-bitmask-char #\" doppelquote-backslash-readtable )
2306)

(check-for-bug :readtable-legacy-150
 (sys::rt-bitmask-char #\\ doppelquote-backslash-readtable )
2306)

(check-for-bug :readtable-legacy-154
 (sys::rt-bitmask-char #\( doppelquote-backslash-readtable )
917)

(check-for-bug :readtable-legacy-158
 (sys::rt-bitmask-char #\) doppelquote-backslash-readtable )
165)

(check-for-bug :readtable-legacy-162
 #"<
  #\<)

(check-for-bug :readtable-legacy-166
  (progn (setq 2.-doppelquote-backslash-readtable
               (copy-readtable doppelquote-backslash-readtable)) t)
  t)

(check-for-bug :readtable-legacy-171
  (sys::rt-bitmask-char #\" 2.-doppelquote-backslash-readtable )
  2306)

(check-for-bug :readtable-legacy-175
  (sys::rt-bitmask-char #\\ 2.-doppelquote-backslash-readtable )
  2306)

(check-for-bug :readtable-legacy-179
  (sys::rt-bitmask-char #\( 2.-doppelquote-backslash-readtable )
  917)

(check-for-bug :readtable-legacy-183
  (sys::rt-bitmask-char #\) 2.-doppelquote-backslash-readtable )
  165)

(check-for-bug :readtable-legacy-187
  (progn (setq 2.-doppelquote-liston-readtable
               (copy-readtable doppelquote-liston-readtable)) t)
  t)

(check-for-bug :readtable-legacy-192
  (sys::rt-bitmask-char #\" 2.-doppelquote-liston-readtable )
  917)

(check-for-bug :readtable-legacy-196
  (sys::rt-bitmask-char #\( 2.-doppelquote-liston-readtable )
  917)

(check-for-bug :readtable-legacy-200
  (sys::rt-bitmask-char #\) 2.-doppelquote-liston-readtable )
  165)

(check-for-bug :readtable-legacy-204
  (sys::rt-bitmask-char #\\ 2.-doppelquote-liston-readtable )
  2306)

(check-for-bug :readtable-legacy-208
  (progn (setq cl-standard-readtable
               (copy-readtable nil))
         (setq *readtable* cl-standard-readtable) t)
  t)

(check-for-bug :readtable-legacy-214
  (sys::rt-bitmask-char #\" cl-standard-readtable )
  117)

(check-for-bug :readtable-legacy-218
  (sys::rt-bitmask-char #\( cl-standard-readtable )
  917)

(check-for-bug :readtable-legacy-222
  (sys::rt-bitmask-char #\) cl-standard-readtable )
  165)

(check-for-bug :readtable-legacy-226
  (sys::rt-bitmask-char #\\ cl-standard-readtable )
  2306)

(check-for-bug :readtable-legacy-230
  (sys::rt-bitmask-char #\" )
  117)

(check-for-bug :readtable-legacy-234
  (sys::rt-bitmask-char #\( )
  917)

(check-for-bug :readtable-legacy-238
  (sys::rt-bitmask-char #\) )
  165)

(check-for-bug :readtable-legacy-242
  (sys::rt-bitmask-char #\\ )
  2306)

(check-for-bug :readtable-legacy-246
  "1234"
  "1234")

(check-for-bug :readtable-legacy-250
  (progn (setq *readtable* 2.-doppelquote-liston-readtable) t)
  t)

(check-for-bug :readtable-legacy-254
  (sys::rt-bitmask-char #\" )
  917)

(check-for-bug :readtable-legacy-258
  (sys::rt-bitmask-char #\( )
  917)

(check-for-bug :readtable-legacy-262
  (sys::rt-bitmask-char #\) )
  165)

(check-for-bug :readtable-legacy-266
  (sys::rt-bitmask-char #\\ )
  2306)

(check-for-bug :readtable-legacy-270
  '"1 2 3)
(1 2 3)

(progn (setq *readtable* doppelquote-backslash-readtable) t)
T

(sys::rt-bitmask-char #\" )
2306

(sys::rt-bitmask-char #\( )
917

(sys::rt-bitmask-char #\) )
165

(sys::rt-bitmask-char #\\ )
2306

#"<
  #\<)

(check-for-bug :readtable-legacy-292
  (readtablep 2.-doppelquote-backslash-readtable )
  T)

(readtablep 1)
NIL


(set-syntax-from-char #\" #\" 2.-doppelquote-backslash-readtable )
T

(sys::rt-bitmask-char #\" 2.-doppelquote-backslash-readtable )
117

(sys::rt-bitmask-char #\\ 2.-doppelquote-backslash-readtable )
2306

(sys::rt-bitmask-char #\( 2.-doppelquote-backslash-readtable )
917

(sys::rt-bitmask-char #\) 2.-doppelquote-backslash-readtable )
165


(set-syntax-from-char #\) #\( 2.-doppelquote-backslash-readtable )
T

(sys::rt-bitmask-char #\" 2.-doppelquote-backslash-readtable )
117

(sys::rt-bitmask-char #\\ 2.-doppelquote-backslash-readtable )
2306

(sys::rt-bitmask-char #\( 2.-doppelquote-backslash-readtable )
917

(sys::rt-bitmask-char #\) 2.-doppelquote-backslash-readtable )
917


(set-syntax-from-char #\( #\) 2.-doppelquote-backslash-readtable )
T

(sys::rt-bitmask-char #\" 2.-doppelquote-backslash-readtable )
117

(sys::rt-bitmask-char #\\ 2.-doppelquote-backslash-readtable )
2306

(sys::rt-bitmask-char #\( 2.-doppelquote-backslash-readtable )
165

(sys::rt-bitmask-char #\) 2.-doppelquote-backslash-readtable )
917


(set-syntax-from-char #\( #\( 2.-doppelquote-liston-readtable
		      2.-doppelquote-backslash-readtable )
T

(sys::rt-bitmask-char #\" 2.-doppelquote-liston-readtable )
917

(sys::rt-bitmask-char #\( 2.-doppelquote-liston-readtable )
165

(sys::rt-bitmask-char #\) 2.-doppelquote-liston-readtable )
165

(sys::rt-bitmask-char #\\ 2.-doppelquote-liston-readtable )
2306


(set-syntax-from-char #\) #\) 2.-doppelquote-liston-readtable
		      2.-doppelquote-backslash-readtable )
T

(sys::rt-bitmask-char #\" 2.-doppelquote-liston-readtable )
917

(sys::rt-bitmask-char #\( 2.-doppelquote-liston-readtable )
165

(sys::rt-bitmask-char #\) 2.-doppelquote-liston-readtable )
917

(sys::rt-bitmask-char #\\ 2.-doppelquote-liston-readtable )
2306


(progn (setq *readtable* 2.-doppelquote-backslash-readtable ) t)
t

)sys::rt-bitmask-char #\( (
165

)sys::rt-bitmask-char #\) (
917

)sys::rt-bitmask-char #\\ (
2306

"1234"
"1234"

')1 2 3(
(1 2 3)

)progn )setq *readtable* 2.-doppelquote-liston-readtable ( t(
t


)sys::rt-bitmask-char #\( (
165

)sys::rt-bitmask-char #\) (
917

)sys::rt-bitmask-char #\\ (
2306

'"1234(
(1234)

')1 2 3(
(1 2 3)
;;  ) muesste listen-anfang-sein
)progn )setq *readtable* )copy-readtable nil(( t(
t

(sys::rt-bitmask-char #\" )
117

(sys::rt-bitmask-char #\( )
917

(sys::rt-bitmask-char #\) )
165

(sys::rt-bitmask-char #\\ )
2306

(sys::rt-bitmask-char #\x )
4097

(sys::rt-bitmask-char #\y )
1


(make-dispatch-macro-character #\x)
T

(sys::rt-bitmask-char #\x )
4109

(sys::rt-bitmask-char #\y )
1

(defun d1 (a b c) (princ "1.dmacro"))
D1

(d1 1 2 3)
"1.dmacro"

(set-dispatch-macro-character #\x #\. #'d1)
T

(sys::rt-bitmask-char #\x )
4109

(prin1-to-string (get-dispatch-macro-character #\x #\.))
"#<SYSTEM::%TYPE-CLOSURE D1
NIL
NIL
(LAMBDA (A B C) (PRINC \"1.dmacro\"))>"

(multiple-value-list (read-from-string "123x.45"))
(   123 3)

(multiple-value-list (read-from-string "123x.45" t nil :start 3))
(   "1.dmacro" 5)

(multiple-value-list (read-from-string "123x.45" t nil :start 5))
(45 7)


(make-dispatch-macro-character #\y)
T

(s\Ys::rt-bitmask-char #\x )
4109

(s\Ys::rt-bitmask-char #\y )
13

(defun d2 (a b c) (princ "2.dmacro"))
D2

(d2 1 2 3)
"2.dmacro"

(set-dispatch-macro-character #\y #\, #'d2)
T

(s\Ys::rt-bitmask-char #\x )
4109

(s\Ys::rt-bitmask-char #\y )
13

(prin1-to-string (get-dispatch-macro-character #\x #\.))
"#<SYSTEM::%TYPE-CLOSURE D1
NIL
NIL
(LAMBDA (A B C) (PRINC \"1.dmacro\"))>"

(prin1-to-string (get-dispatch-macro-character #\y #\,))
"#<SYSTEM::%TYPE-CLOSURE D2
NIL
NIL
(LAMBDA (A B C) (PRINC \"2.dmacro\"))>"

(multiple-value-list (read-from-string "123y,45"))
(   123 3)

(multiple-value-list (read-from-string "123y,45" t nil :start 3))
(   "2.dmacro" 5)

(multiple-value-list (read-from-string "123y,45" t nil :start 5))
(45 7)

(set-dispatch-macro-character #\x #\. #'d2)
T

(s\Ys::rt-bitmask-char #\x )
4109

(s\Ys::rt-bitmask-char #\y )
13

(prin1-to-string (get-dispatch-macro-character #\y #\,))
"#<SYSTEM::%TYPE-CLOSURE D2
NIL
NIL
(LAMBDA (A B C) (PRINC \"2.dmacro\"))>"

(prin1-to-string (get-dispatch-macro-character #\x #\.))
"#<SYSTEM::%TYPE-CLOSURE D2
NIL
NIL
(LAMBDA (A B C) (PRINC \"2.dmacro\"))>"

(multiple-value-list (read-from-string "123x.45"))
(   123 3)

(multiple-value-list (read-from-string "123x.45" t nil :start 3))
(   "2.dmacro" 5)

(multiple-value-list (read-from-string "123x.45" t nil :start 5))
(45 7)

(set-dispatch-macro-character #\y #\. #'d1)
T

(s\Ys::rt-bitmask-char #\x )
4109

(s\Ys::rt-bitmask-char #\y )
13

(prin1-to-string (get-dispatch-macro-character #\x #\.))
"#<SYSTEM::%TYPE-CLOSURE D2
NIL
NIL
(LAMBDA (A B C) (PRINC \"2.dmacro\"))>"

(prin1-to-string (get-dispatch-macro-character #\y #\,))
"#<SYSTEM::%TYPE-CLOSURE D2
NIL
NIL
(LAMBDA (A B C) (PRINC \"2.dmacro\"))>"

(prin1-to-string (get-dispatch-macro-character #\y #\.))
"#<SYSTEM::%TYPE-CLOSURE D1
NIL
NIL
(LAMBDA (A B C) (PRINC \"1.dmacro\"))>"

(multiple-value-list (read-from-string "123y.45"))
(   123 3)

(multiple-value-list (read-from-string "123y.45" t nil :start 3))
(   "1.dmacro" 5)

(multiple-value-list (read-from-string "123y.45" t nil :start 5))
(45 7)

(multiple-value-list (read-from-string "123y,45"))
(   123 3)

(multiple-value-list (read-from-string "123y,45" t nil :start 3))
(   "2.dmacro" 5)

(multiple-value-list (read-from-string "123y,45" t nil :start 5))
(45 7)

(progn (setq *readtable* (cop\Y-readtable nil nil)) t)
t

(sys::rt-bitmask-char #\x )
4097

(sys::rt-bitmask-char #\y )
1

(get-dispatch-macro-character #\x #\.)
ERROR

(get-dispatch-macro-character #\y #\,)
ERROR

(get-dispatch-macro-character #\y #\.)
ERROR

(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  (mapcon #'(lambda (x)
              (mapcar #'(lambda (y)(list (car x) y))(cdr x)))
          (read-delimited-list #\} stream)))
|#{|-|reader|

(set-dispatch-macro-character #\# #\{ #'|#{-reader|)
T

;;      (set-macro-character #\} (get-macro-character #\)) nil))
;;      geht bei uns nicht !
;;      dafuer :
(set-syntax-from-char #\} #\) )
;;      nicht notwendig, da superklammer
(progn
(setq read-st (make-string-input-stream "#{p q z a} #{a b c d}")) t)
T

(read read-st)
((P Q) (P Z) (P A) (Q Z) (Q A) (Z A))

(read read-st)
((A B) (A C) (A D) (B C) (B D) (C D))

(progn (setq *readtable* (copy-readtable nil nil))
       (makunbound 'doppelquote-liston-readtable)
       (makunbound 'doppelquote-backslash-readtable)
       (makunbound '2.-doppelquote-liston-readtable)
       (makunbound '2.-doppelquote-backslash-readtable)
       (makunbound 'cl-standard-readtable)
       (makunbound 'read-st)
       (makunbound '$)
t)
T

