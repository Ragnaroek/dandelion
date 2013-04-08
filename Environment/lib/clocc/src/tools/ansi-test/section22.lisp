;;; section 22: printer -*- mode: lisp -*-
(in-package :cl-user)


;;; from : Raymond Toy <toy@rtp.ericsson.se>
(check-for-bug :section22-legacy-6
  (format nil "~V,,,'-A" 10 "abc")
  "abc-------")
					; 0123456789

(check-for-bug :section22-legacy-11
  (format nil "foo")
  "foo")

(check-for-bug :section22-legacy-15
  (setq x 5)
  5)

(check-for-bug :section22-legacy-19
  (format nil "The answer is ~D." x)
  "The answer is 5.")

(check-for-bug :section22-legacy-23
  (format nil "The answer is ~3D." x)
  "The answer is   5.")

(check-for-bug :section22-legacy-27
  (format nil "The answer is ~3,'0D." x)
  "The answer is 005.")

(check-for-bug :section22-legacy-31
  (format nil "The answer is ~:D." (expt 47 x))
  "The answer is 229,345,007.")

(check-for-bug :section22-legacy-35
  (setq y "elephant")
  "elephant")

(check-for-bug :section22-legacy-39
  (format nil "Look at the ~A!" y)
  "Look at the elephant!")

(check-for-bug :section22-legacy-43
  (setq n 3)
  3)

(check-for-bug :section22-legacy-47
  (format nil "~D item~:P found." n)
  "3 items found.")

(check-for-bug :section22-legacy-51
  (format nil "~R dog~:[s are~; is~] here." n (= n 1))
  "three dogs are here.")

(check-for-bug :section22-legacy-55
  (format nil "~R dog~:*~[s are~; is~:;s are~] here." n)
  "three dogs are here.")

(check-for-bug :section22-legacy-59
  (format nil "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n)
  "Here are three puppies.")

(check-for-bug :section22-legacy-63
  (defun foo (x)
    (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
            x x x x x x))
  FOO)

(check-for-bug :section22-legacy-69
  (foo 3.14159)
  "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")

(check-for-bug :section22-legacy-73
  (foo -3.14159)
  " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")

(check-for-bug :section22-legacy-77
  (foo 100.0)
  "100.00|******|100.00| 100.0|100.00|100.0")

(check-for-bug :section22-legacy-81
  (foo 1234.0)
  "1234.00|******|??????|1234.0|1234.00|1234.0")

(check-for-bug :section22-legacy-85
  (foo 0.006)
  "  0.01|  0.06|  0.01| 0.006|0.01|0.006")

(check-for-bug :section22-legacy-89
  (defun foo (x)
    (format nil
            "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E"
            x x x x))
  FOO)

(check-for-bug :section22-legacy-97
  (foo 3.14159)
  "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0")

(check-for-bug :section22-legacy-101
  (foo -3.14159)
  " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0")

(check-for-bug :section22-legacy-105
  (foo 1100.0)
  "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3")

(check-for-bug :section22-legacy-109
  (foo 1100.0L0)
  #-(or cmu sbcl) "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"
  #+(or cmu sbcl) "  1.10d+3| 11.00$+02|+.001d+06|  1.10d+3")

(check-for-bug :section22-legacy-114
  (foo 1.1E13)
  "*********| 11.00$+12|+.001E+16| 1.10E+13")

(check-for-bug :section22-legacy-118
  (foo 1.1L120)
  #-(or cmu sbcl) "*********|??????????|%%%%%%%%%|1.10L+120"
  #+(or cmu sbcl) "*********|??????????|%%%%%%%%%|1.10d+120")

(check-for-bug :section22-legacy-123
  (defun foo (x)
    (format nil "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
            x x x x))
  foo)

(check-for-bug :section22-legacy-129
  (foo 0.0314159)
  "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")

(check-for-bug :section22-legacy-133
  (foo 0.314159)
  "  0.31   |0.314    |0.314    | 0.31    ")

(check-for-bug :section22-legacy-137
  (foo 3.14159)
  "   3.1   | 3.14    | 3.14    |  3.1    ")

(check-for-bug :section22-legacy-141
  (foo 31.4159)
  "   31.   | 31.4    | 31.4    |  31.    ")

(check-for-bug :section22-legacy-145
  (foo 314.159)
  "  3.14E+2| 314.    | 314.    |  3.14E+2")

(check-for-bug :section22-legacy-149
  (foo 3141.59)
  "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")

(check-for-bug :section22-legacy-153
  (foo 3141.59L0)
  #-(or cmu sbcl) "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"
  #+(or cmu sbcl) "  3.14d+3|314.2$+01|0.314d+04|  3.14d+3")

(check-for-bug :section22-legacy-158
  (foo 3.14E12)
  "*********|314.0$+10|0.314E+13| 3.14E+12")

(check-for-bug :section22-legacy-162
  (foo 3.14L120)
  #-(or cmu sbcl) "*********|?????????|%%%%%%%%%|3.14L+120"
  #+(or cmu sbcl) "*********|?????????|%%%%%%%%%|3.14d+120")

(check-for-bug :section22-legacy-167
  (format nil "~10<foo~;bar~>")
  "foo    bar")

(check-for-bug :section22-legacy-171
  (format nil "~10:<foo~;bar~>")
  "  foo  bar")

(check-for-bug :section22-legacy-175
  (format nil "~10<foobar~>")
  "    foobar")

(check-for-bug :section22-legacy-179
  (format nil "~10:<foobar~>")
  "    foobar")

(check-for-bug :section22-legacy-183
  (format nil "~10:@<foo~;bar~>")
  #+(or sbcl cmu ecls)
  " foo bar  "
  #+clisp
  "  foo bar "
  #-(or sbcl cmu clisp ecls)
  fill-this-in)

(check-for-bug :section22-legacy-192
  (format nil "~10@<foobar~>")
  "foobar    ")

(check-for-bug :section22-legacy-196
  (format nil "~10:@<foobar~>")
  "  foobar  ")

(check-for-bug :section22-legacy-200
  (FORMAT NIL "Written to ~A." #P"foo.bin")
  "Written to foo.bin.")

