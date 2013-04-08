;;; based on v1.4 -*- mode: lisp -*-
(in-package :cl-user)

;; ****************************************************************************
;; *      Rosenmueller            format.tst                                  *
;; ****************************************************************************

;;  ~< ------------------------------------------------------------------------
(check-for-bug :format-legacy-9
  (format nil "~10<foo~;bar~>")
  "foo    bar")

(check-for-bug :format-legacy-13
  (format nil "~10:<foo~;bar~>")
  "  foo  bar")

(check-for-bug :format-legacy-17
  (format nil "~10@<foo~;bar~>")
  "foo  bar  ")

(check-for-bug :format-legacy-21
  (format nil "~10:@<foo~;bar~>")
  #+(or XCL CLISP ALLEGRO) "  foo bar "
  #+(or AKCL cmu sbcl ecls) " foo bar  "
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl ecls) UNKNOWN)

(check-for-bug :format-legacy-27
  (format nil "~10<foobar~>")
  "    foobar")

(check-for-bug :format-legacy-31
  (format nil "~10:<foobar~>")
  "    foobar")

(check-for-bug :format-legacy-35
  (format nil "~10@<foobar~>")
  "foobar    ")

(check-for-bug :format-legacy-39
  (format nil "~10:@<foobar~>")
  "  foobar  ")

;;  ~< ~s ~^ ------------------------------------------------------------------
(check-for-bug :format-legacy-44
  (format nil "~15<~S~>" 'foo)
  "            foo")

(check-for-bug :format-legacy-48
  (format nil "~15<~S~;~^~S~>" 'foo)
  "            foo")

(check-for-bug :format-legacy-52
  (format nil "~15<~S~;~^~S~;~^~S~>" 'foo)
  "            foo")

(check-for-bug :format-legacy-56
  (format nil "~15<~S~;~^~S~>" 'foo 'bar)
  "foo         bar")

(check-for-bug :format-legacy-60
  (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
  "foo         bar")

(check-for-bug :format-legacy-64
  (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
  "foo   bar   baz")

(check-for-bug :format-legacy-68
  (format nil "~12<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
  #+(or CLISP ALLEGRO) "foo  bar baz"
  #+(OR CMU SBCL) "foo bar  baz"
  #-(or CLISP ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-74
  (progn
    (setq liste '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
                  gggggggg
                  hhhhh iiii j kk lll mmmm nnnnnn oooooooooo ppppppppppppppp qqqqqqq
                  rrrrrrrrrrrr
                  s ttt uuuuuuuuu vvvvvvv wwwwwwwwww xxxxx yyyyyy zzzzzzzz)) ;26
    T)
  T)

(check-for-bug :format-legacy-84
  (format nil "~%;; ~<~%;; ~1:; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~;~>~%"
          ;; 2!
          'aaaaaaa 'bbbbbb 'cccccccccccc 'dddddddddddddd 'eeee 'fffffffff 'gggggggg
          'hhhhh 'iiii 'j 'kk 'lll 'mmmm 'nnnnnn 'oooooooooo 'ppppppppppppppp
          'qqqqqqq
          'rrrrrrrrrrrr 's 'ttt 'uuuuuuuuu 'vvvvvvv 'wwwwwwwwww 'xxxxx 'yyyyyy
          'zzzzzzzz)
  #+XCL
  "
;;  AAAAAAA  BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG
;;  HHHHH  IIII  JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR    S    TTT   UUUUUUUUU   VVVVVVV   WWWWWWWWWW   XXXXX
"
  ;; 23456789;123456789;123456789;123456789;123456789;123456789;123456789;12
  #-XCL
  "
;; 
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG HHHHH IIII JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ RRRRRRRRRRRR S TTTUUUUUUUUU VVVVVVV WWWWWWWWWW XXXXX
")

(check-for-bug :format-legacy-107
  (format nil
	  "~%;; ~<~%;; ~1,50:; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~;~>~%"
					; 2!
          'aaaaaaa 'bbbbbb 'cccccccccccc 'dddddddddddddd 'eeee 'fffffffff 'gggggggg
          'hhhhh 'iiii 'j 'kk 'lll 'mmmm 'nnnnnn 'oooooooooo 'ppppppppppppppp
          'qqqqqqq
          'rrrrrrrrrrrr 's 'ttt 'uuuuuuuuu 'vvvvvvv 'wwwwwwwwww 'xxxxx 'yyyyyy
          'zzzzzzzz)
  #+XCL
  "
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC  DDDDDDDDDDDDDD
;;  EEEE  FFFFFFFFF  GGGGGGGG  HHHHH IIII JKK LLL
;;  MMMM    NNNNNN   OOOOOOOOOO   PPPPPPPPPPPPPPP
;;  QQQQQQQ  RRRRRRRRRRRR  S TTTUUUUUUUUU VVVVVVV
;;  WWWWWWWWWW                              XXXXX
"
  ;; 23456789;123456789;123456789;123456789;123456789;
  #-XCL
  "
;; 
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG HHHHH IIII JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ RRRRRRRRRRRR S TTTUUUUUUUUU VVVVVVV WWWWWWWWWW XXXXX
")

#-sbcl
(check-for-bug :format-legacy-132
  (defun format-blocksatz (stream parts prefix &optional line-length start-p end-p)
    (if (null stream)
        (let ((stream (make-string-output-stream)))
          (format-blocksatz stream parts prefix line-length start-p end-p)
          (get-output-stream-string stream)
          )
        (unless (endp parts)
          (setq line-length (or line-length #|(sys::line-length stream)|# 72))
          (when start-p (format stream prefix))
          (loop
              ;;  Hier ist parts /= NIL
              (let ((pos (#+CLISP sys::line-position
                                  #+ALLEGRO excl::charpos
                                  #+(OR CMU SBCL) cl::charpos stream))
                    (parts-now '()))
                (let ((pos-now pos))
                  (loop
                      (when (endp parts) (return))
                      (let* ((part (first parts))
                             (part-length (length part)))
                        (unless (null parts-now)
                          (when (> (+ pos-now part-length) line-length)
                            (return)
                            ) )
                        (pop parts)
                        (push part parts-now)
                        (incf pos-now part-length)
                        ) ) )
                ;;  Hier ist parts-now /= NIL
                (apply #'format
                       stream
                       (if (and (endp parts) (not end-p))
                           (apply #'concatenate 'string
                                  (make-list (length parts-now) :initial-element "~A")
                                  )
                           (concatenate 'string
                                        "~"
                                        (write-to-string (max 0 (- line-length pos))
                                                         :radix nil :base 10
                                                         )
                                        (if (= (length parts-now) 1) "@" "")
                                        "<"
                                        (apply #'concatenate 'string
                                               "~A"
                                               (make-list (1- (length parts-now)) :initial-element "~;~A")
                                               )
                                        "~>"
                                        ) )
                       (nreverse parts-now)
                       ) )
              (when (endp parts) (return))
            (format stream prefix)
            ) ) ) )
  FORMAT-BLOCKSATZ)

#-sbcl
(check-for-bug :format-legacy-189
  (format-blocksatz nil
                    (mapcar #'(lambda (x) (format nil " ~A" x))
                            '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
                              gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
                              ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
                              wwwwwwwwww xxxxx yyyyyy zzzzzzzz)
                            )
                    "~%;; "
                    nil t nil
                    )
  #+(or CLISP ALLEGRO)
  "
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG
;;  HHHHH  IIII  J KK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR   S  TTT  UUUUUUUUU  VVVVVVV  WWWWWWWWWW  XXXXX  YYYYYY
;;  ZZZZZZZZ"
  #+(OR CMU SBCL)
  "
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE  FFFFFFFFF  GGGGGGGG
;;  HHHHH IIII J KK LLL MMMM NNNNNN OOOOOOOOOO  PPPPPPPPPPPPPPP  QQQQQQQ
;;  RRRRRRRRRRRR  S  TTT  UUUUUUUUU  VVVVVVV  WWWWWWWWWW  XXXXX   YYYYYY
;;  ZZZZZZZZ"
  #-(or CLISP ALLEGRO cmu sbcl) UNKNOWN)
;; 123456789;123456789;123456789;123456789;123456789;123456789;123456789;12

#-sbcl
(check-for-bug :format-legacy-216
  (format-blocksatz nil
                    (mapcar #'(lambda (x) (format nil " ~A" x))
                            '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
                              gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
                              ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
                              wwwwwwwwww xxxxx yyyyyy zzzzzzzz)
                            )
                    "~%;; "
                    50 t t
                    )
  #+(or CLISP ALLEGRO)
  "
;;  AAAAAAA   BBBBBB  CCCCCCCCCCCC  DDDDDDDDDDDDDD
;;  EEEE  FFFFFFFFF  GGGGGGGG  HHHHH IIII J KK LLL
;;  MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR    S    TTT   UUUUUUUUU   VVVVVVV
;;  WWWWWWWWWW      XXXXX      YYYYYY     ZZZZZZZZ"
  #+(OR CMU SBCL)
  "
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC   DDDDDDDDDDDDDD
;;  EEEE FFFFFFFFF GGGGGGGG HHHHH IIII  J  KK  LLL
;;  MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR   S   TTT    UUUUUUUUU    VVVVVVV
;;  WWWWWWWWWW     XXXXX      YYYYYY      ZZZZZZZZ"
  #-(or CLISP ALLEGRO cmu sbcl) UNKNOWN)
;; 123456789;123456789;123456789;123456789;123456789;

;;; unklare Bedeutung (Fehler in Sprachbeschreibung?)
;;; (format nil "~%;; ~{~<~%;; ~1:; ~s~>~^,~}.~%" liste) ""
;;; (format nil "~%;; ~{~<~%;; ~1,50:; ~s~>~^,~}.~%" liste) ""

(check-for-bug :format-legacy-248
  (format nil "~%;; ~{~<~%;; ~1,50:; ~s~>~^,~}.~%"
          '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
            gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
            ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
            wwwwwwwwww xxxxx yyyyyy zzzzzzzz))
  "
;;  AAAAAAA, BBBBBB, CCCCCCCCCCCC, DDDDDDDDDDDDDD,
;;  EEEE, FFFFFFFFF, GGGGGGGG, HHHHH, IIII, J, KK,
;;  LLL, MMMM, NNNNNN, OOOOOOOOOO,
;;  PPPPPPPPPPPPPPP, QQQQQQQ, RRRRRRRRRRRR, S,
;;  TTT, UUUUUUUUU, VVVVVVV, WWWWWWWWWW, XXXXX,
;;  YYYYYY, ZZZZZZZZ.
")

;;  ~f ------------------------------------------------------------------------
;;  Format F

(check-for-bug :format-legacy-266
  (DEFUN FOO (X)
    (FORMAT NIL "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F" X X X X
            X X))
  FOO)

(check-for-bug :format-legacy-272
  (FOO 3.14159)
  ;;        "  3.14| 31.42|  3.14|3.1416|3.14|3.141590116672995328"
  "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")

(check-for-bug :format-legacy-277
  (FOO -3.14159)
  ;;        " -3.14|-31.42| -3.14|-3.142|-3.14|-3.141590116672995328"
  " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")

(check-for-bug :format-legacy-282
  (FOO 100.0)
  "100.00|******|100.00| 100.0|100.00|100.0")

(check-for-bug :format-legacy-286
  (FOO 1234.0)
  "1234.00|******|??????|1234.0|1234.00|1234.0")

(check-for-bug :format-legacy-290
  (FOO 0.006)
  "  0.01|  0.06|  0.01| 0.006|0.01|0.006")

(check-for-bug :format-legacy-294
  (format nil "~5,2,-13f" 1.1e13)
  " 1.10")

(check-for-bug :format-legacy-298
  (format nil "~9,0,6f" 3.14159)
  " 3141590.")

(check-for-bug :format-legacy-302
  (FORMAT NIL "~5D" (QUOTE A))
  "    A"
  "ANSI CL is not clear here whether the width is ignored or not,
but it makes more sense to print non-numeric arguments properly alighned")

(check-for-bug :format-legacy-308
  (FORMAT NIL "~5,3F" (QUOTE A))
  "    A"
  "ANSI CL is not clear here whether the width is ignored or not,
but it makes more sense to print non-numeric arguments properly alighned")

(check-for-bug :format-legacy-314
  (FORMAT NIL "~5,3F" #C(1.2 0.3))
  "#C(1.2 0.3)")

(check-for-bug :format-legacy-318
  (FORMAT NIL "~5,3F" 2/3)
  "0.667")

(check-for-bug :format-legacy-322
  (format nil "~1f" 10)
  "10."
  "22.3.3.1 Tilde F: Fixed-Format Floating-Point

If it is impossible to print the value in the required format in a
field of width w, then one of two actions is taken. If the parameter
overflowchar is supplied, then w copies of that parameter are printed
instead of the scaled value of arg. If the overflowchar parameter is
omitted, then the scaled value is printed using more than w characters,
as many more as may be needed.")

(check-for-bug :format-legacy-334
  (format nil "~0f" 10)
  "10."
  "22.3.3.1 Tilde F: Fixed-Format Floating-Point

If it is impossible to print the value in the required format in a
field of width w, then one of two actions is taken. If the parameter
overflowchar is supplied, then w copies of that parameter are printed
instead of the scaled value of arg. If the overflowchar parameter is
omitted, then the scaled value is printed using more than w characters,
as many more as may be needed.")

(check-for-bug :format-legacy-346
  (format nil "~1,,,'xf" -10)
  "x"
  "22.3.3.1 Tilde F: Fixed-Format Floating-Point

If it is impossible to print the value in the required format in a
field of width w, then one of two actions is taken. If the parameter
overflowchar is supplied, then w copies of that parameter are printed
instead of the scaled value of arg. If the overflowchar parameter is
omitted, then the scaled value is printed using more than w characters,
as many more as may be needed.")

(check-for-bug :format-too-long-fp
  (format nil "~2f" 1234567.1234)
  "1234567.")

;;  ~e ----------------------------- ------------------------------------------
;;  Format E

(check-for-bug :format-legacy-361
  (defun foo (x)
    (format nil
            "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@e|~9,2E"
            x x x x))
  FOO)

(check-for-bug :format-legacy-368
  (foo 3.14159)
  "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0")

(check-for-bug :format-legacy-372
  (foo -3.14159)
  " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0")

(check-for-bug :format-legacy-376
  (foo 1100.0)
  "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3")

(check-for-bug :format-legacy-380
  (foo 1100.0L0)
  #+XCL "  1.10D+3| 11.00$+02|+.001D+06|  1.10D+3"
  #+(or CLISP AKCL) "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"
  #+(or ALLEGRO cmu sbcl) "  1.10d+3| 11.00$+02|+.001d+06|  1.10d+3"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-387
  (foo 1.1E13)
  "*********| 11.00$+12|+.001E+16| 1.10E+13")

;;  ERROR beim read der zahl (foo 1.1L120)

(check-for-bug :format-legacy-393
  (FORMAT NIL "_~10,4E_" 1.2)
  "_ 1.2000E+0_")

(check-for-bug :format-legacy-397
  (format nil "~9,2,1E" 0.0314159)
  "  3.14E-2")

;;  ~% ~d ~e (v) --------------------------------------------------------------
(check-for-bug :format-legacy-402
  (let (x)
    (dotimes (k 13 x)
      (setq x (cons (format nil "~%Scale factor ~2D: |~13,6,2,VE|"
                            (- k 5) (- k 5) 3.14159) x))))
  (
   "
Scale factor  7: | 3141590.E-06|" "
Scale factor  6: | 314159.0E-05|" "
Scale factor  5: | 31415.90E-04|" "
Scale factor  4: | 3141.590E-03|" "
Scale factor  3: | 314.1590E-02|" "
Scale factor  2: | 31.41590E-01|" "
Scale factor  1: | 3.141590E+00|" "
Scale factor  0: | 0.314159E+01|" "
Scale factor -1: | 0.031416E+02|" "
Scale factor -2: | 0.003142E+03|" "
Scale factor -3: | 0.000314E+04|" "
Scale factor -4: | 0.000031E+05|" "
Scale factor -5: | 0.000003E+06|"))


;;  ~g ------------------------------------------------------------------------
(check-for-bug :format-legacy-425
  (defun foo (x)
    (format nil "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
            x x x x))
  foo)

(check-for-bug :format-legacy-431
  (foo 0.0314159)
  "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")

(check-for-bug :format-legacy-435
  (foo 0.314159)
  "  0.31   |0.314    |0.314    | 0.31    ")

(check-for-bug :format-legacy-439
  (foo 3.14159)
  "   3.1   | 3.14    | 3.14    |  3.1    ")

(check-for-bug :format-legacy-443
  (foo 31.4159)
  "   31.   | 31.4    | 31.4    |  31.    ")

(check-for-bug :format-legacy-447
  (foo 314.159)
  "  3.14E+2| 314.    | 314.    |  3.14E+2")

(check-for-bug :format-legacy-451
  (foo 3141.59)
  "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")

(check-for-bug :format-legacy-455
  (foo 3141.59L0)
  #+XCL "  3.14D+3|314.2$+01|0.314D+04|  3.14D+3"
  #+(or CLISP AKCL) "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"
  #+(or ALLEGRO cmu sbcl) "  3.14d+3|314.2$+01|0.314d+04|  3.14d+3"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-462
  (foo 3.14E12)
  "*********|314.0$+10|0.314E+13| 3.14E+12")

;; (foo 3.14L120 und L1200) fehler in numerik

;;  ~a ------------------------------------------------------------------------

(check-for-bug :format-legacy-470
  (FORMAT NIL "foo")
  "foo")

(check-for-bug :format-legacy-474
  (FORMAT NIL "format-a:--~a--ende" (QUOTE AB\c))
  "format-a:--ABc--ende")

(check-for-bug :format-legacy-478
  (SETQ Y "elephant")
  "elephant")

(check-for-bug :format-legacy-482
  (FORMAT NIL "Look at the ~A!" Y)
  "Look at the elephant!")

(check-for-bug :format-legacy-486
  (FORMAT NIL "format-%:--~%--1-newline-*")
  "format-%:--
--1-newline-*")

(check-for-bug :format-legacy-491
  (FORMAT NIL "format-%:--~3%--3-newline-*")
  "format-%:--


--3-newline-*")

(check-for-bug :format-legacy-498
  (FORMAT NIL "format-a:--~5a--ende-*" (QUOTE AB\c))
  "format-a:--ABc  --ende-*")

(check-for-bug :format-legacy-502
  (FORMAT NIL "format-a:--~5,2a--ende-*" (QUOTE AB\c))
  "format-a:--ABc  --ende-*")

(check-for-bug :format-legacy-506
  (FORMAT NIL "format-a:--~5,2,3a--ende-*" (QUOTE AB\c))
  "format-a:--ABc   --ende-*")

(check-for-bug :format-legacy-510
  (FORMAT NIL "format-a:--~5,2,3,'*a--ende-*" (QUOTE AB\c))
  "format-a:--ABc***--ende-*")

(check-for-bug :format-legacy-514
  (FORMAT NIL "format-a:--~@a--ende-*" (QUOTE AB\c))
  "format-a:--ABc--ende-*")

(check-for-bug :format-legacy-518
  (FORMAT NIL "format-a:--~5@a--ende-*" (QUOTE AB\c))
  "format-a:--  ABc--ende-*")

(check-for-bug :format-legacy-522
  (FORMAT NIL "format-a:--~5,2@a--ende-*" (QUOTE AB\c))
  "format-a:--  ABc--ende-*")

(check-for-bug :format-legacy-526
  (FORMAT NIL "format-a:--~5,2,3@a--ende-*" (QUOTE AB\c))
  "format-a:--   ABc--ende-*")

(check-for-bug :format-legacy-530
  (FORMAT NIL "format-a:--~5,2,3,'*@a--ende-*" (QUOTE AB\c))
  "format-a:--***ABc--ende-*")

(check-for-bug :format-legacy-534
  (FORMAT NIL "format-a:--~:a--ende-*" (QUOTE (AB\c NIL XYZ)))
  "format-a:--(ABc NIL XYZ)--ende-*")

(check-for-bug :format-legacy-538
  (FORMAT NIL "format-s:--~s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--AB\\c--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-544
  (FORMAT NIL "format-s:--~5s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--AB\\c --ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-550
  (FORMAT NIL "format-s:--~5,2s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--AB\\c  --ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-556
  (FORMAT NIL "format-s:--~5,2,3s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--AB\\c   --ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|   --ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-562
  (FORMAT NIL "format-s:--~5,2,3,'*s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--AB\\c***--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|***--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-568
  (FORMAT NIL "format-s:--~@s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--AB\\c--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-574
  (FORMAT NIL "format-s:--~5@s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:-- AB\\c--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-580
  (FORMAT NIL "format-s:--~5,2@s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--  AB\\c--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-586
  (FORMAT NIL "format-s:--~5,2,3@s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--   AB\\c--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--   |ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-592
  (FORMAT NIL "format-s:--~5,2,3,'*@s--ende-*" (QUOTE AB\c))
  #+XCL "format-s:--***AB\\c--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--***|ABc|--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-598
  (FORMAT NIL "format-s:--~:s--ende-*" (QUOTE (AB\c NIL XYZ)))
  #+XCL "format-s:--(AB\\c NIL XYZ)--ende-*"
  #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--(|ABc| NIL XYZ)--ende-*"
  #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(check-for-bug :format-legacy-604
  (SETQ X 5)
  5)

(check-for-bug :format-legacy-608
  (FORMAT NIL "The answer is ~D." X)
  "The answer is 5.")

(check-for-bug :format-legacy-612
  (FORMAT NIL "The answer is ~3D." X)
  "The answer is   5.")

(check-for-bug :format-legacy-616
  (FORMAT NIL "The answer is ~3,'0D." X)
  "The answer is 005.")

(check-for-bug :format-legacy-620
  (FORMAT NIL "The answer is ~:D." (EXPT 47 X))
  "The answer is 229,345,007.")

(check-for-bug :format-legacy-624
  (FORMAT NIL "decimal:~d, width=5:~5d-*" 10 10)
  "decimal:10, width=5:   10-*")

(check-for-bug :format-legacy-628
  (FORMAT NIL "format-d:--~d--ende-*" 123)
  "format-d:--123--ende-*")

(check-for-bug :format-legacy-632
  (FORMAT NIL "format-d:--~10d--ende-*" 123)
  "format-d:--       123--ende-*")

(check-for-bug :format-legacy-636
  (FORMAT NIL "format-d:--~10,'?d--ende-*" 123)
  "format-d:--???????123--ende-*")

(check-for-bug :format-legacy-640
  (FORMAT NIL "format-d:--~@d--ende-*" 123)
  "format-d:--+123--ende-*")

(check-for-bug :format-legacy-644
  (FORMAT NIL "format-d:--~10@d--ende-*" 123)
  "format-d:--      +123--ende-*")

(check-for-bug :format-legacy-648
  (FORMAT NIL "format-d:--~10,'?@d--ende-*" 123)
  "format-d:--??????+123--ende-*")

(check-for-bug :format-legacy-652
  (FORMAT NIL "format-b:--~b--ende-*" 123)
  "format-b:--1111011--ende-*")

(check-for-bug :format-legacy-656
  (FORMAT NIL "format-b:--~10b--ende-*" 123)
  "format-b:--   1111011--ende-*")

(check-for-bug :format-legacy-660
  (FORMAT NIL "format-b:--~10,'?b--ende-*" 123)
  "format-b:--???1111011--ende-*")

(check-for-bug :format-legacy-664
  (FORMAT NIL "format-b:--~:b--ende-*" 123)
  "format-b:--1,111,011--ende-*")

(check-for-bug :format-legacy-668
  (FORMAT NIL "format-b:--~10:b--ende-*" 123)
  "format-b:-- 1,111,011--ende-*")

(check-for-bug :format-legacy-672
  (FORMAT NIL "format-b:--~10,'?:b--ende-*" 123)
  "format-b:--?1,111,011--ende-*")

(check-for-bug :format-legacy-676
  (FORMAT NIL "format-b:--~10,'?,'.:b--ende-*" 123)
  "format-b:--?1.111.011--ende-*")

(check-for-bug :format-legacy-680
  (FORMAT NIL "format-b:--~@b--ende-*" 123)
  "format-b:--+1111011--ende-*")

(check-for-bug :format-legacy-684
  (FORMAT NIL "format-b:--~10@b--ende-*" 123)
  "format-b:--  +1111011--ende-*")

(check-for-bug :format-legacy-688
  (FORMAT NIL "format-b:--~10,'?@b--ende-*" 123)
  "format-b:--??+1111011--ende-*")

(check-for-bug :format-legacy-692
  (FORMAT NIL "format-b:--~:@b--ende-*" 123)
  "format-b:--+1,111,011--ende-*")

(check-for-bug :format-legacy-696
  (FORMAT NIL "format-o:--~o--ende-*" 123)
  "format-o:--173--ende-*")

(check-for-bug :format-legacy-700
  (FORMAT NIL "format-o:--~10o--ende-*" 123)
  "format-o:--       173--ende-*")

(check-for-bug :format-legacy-704
  (FORMAT NIL "format-o:--~10,'?o--ende-*" 123)
  "format-o:--???????173--ende-*")

(check-for-bug :format-legacy-708
  (FORMAT NIL "format-o:--~@o--ende-*" 123)
  "format-o:--+173--ende-*")

(check-for-bug :format-legacy-712
  (FORMAT NIL "format-o:--~10@o--ende-*" 123)
  "format-o:--      +173--ende-*")

(check-for-bug :format-legacy-716
  (FORMAT NIL "format-x:--~x--ende-*" 123)
  "format-x:--7B--ende-*")

(check-for-bug :format-legacy-720
  (FORMAT NIL "format-x:--~10x--ende-*" 123)
  "format-x:--        7B--ende-*")

(check-for-bug :format-legacy-724
  (FORMAT NIL "format-x:--~10,'?x--ende-*" 123)
  "format-x:--????????7B--ende-*")

(check-for-bug :format-legacy-728
  (FORMAT NIL "format-x:--~10:x--ende-*" 123)
  "format-x:--        7B--ende-*")

(check-for-bug :format-legacy-732
  (FORMAT NIL "format-x:--~@x--ende-*" 123)
  "format-x:--+7B--ende-*")

(check-for-bug :format-legacy-736
  (FORMAT NIL "format-x:--~10@x--ende-*" 123)
  "format-x:--       +7B--ende-*")

(check-for-bug :format-legacy-740
  (FORMAT NIL "format-r:--~20r--ende-*" 123)
  "format-r:--63--ende-*")

(check-for-bug :format-legacy-744
  (FORMAT NIL "format-r:--~20,10r--ende-*" 123)
  "format-r:--        63--ende-*")

(check-for-bug :format-legacy-748
  (FORMAT NIL "format-r:--~20@r--ende-*" 123)
  "format-r:--+63--ende-*")

(check-for-bug :format-legacy-752
  (FORMAT NIL "format-r:--~r--ende-*" 9)
  "format-r:--nine--ende-*")

(check-for-bug :format-legacy-756
  (FORMAT NIL "format-r:--~:r--ende-*" 9)
  "format-r:--ninth--ende-*")

(check-for-bug :format-legacy-760
  (FORMAT NIL "format-r:--~@r--ende-*" 9)
  "format-r:--IX--ende-*")

(check-for-bug :format-legacy-764
  (FORMAT NIL "format-r:--~:@r--ende-*" 9)
  "format-r:--VIIII--ende-*")

(check-for-bug :format-legacy-768
  (FORMAT NIL "format-p:--~d  object~p-*" 1 1)
  "format-p:--1  object-*")

(check-for-bug :format-legacy-772
  (FORMAT NIL "format-p:--~d  object~p-*" 2 2)
  "format-p:--2  objects-*")

(check-for-bug :format-legacy-776
  (FORMAT NIL "format-p:--~d  bab~@p-*" 1 1)
  "format-p:--1  baby-*")

(check-for-bug :format-legacy-780
  (FORMAT NIL "format-p:--~d  bab~@p-*" 2 2)
  "format-p:--2  babies-*")

(check-for-bug :format-legacy-784
  (FORMAT NIL "format-p:--~d  object~:p-*" 1)
  "format-p:--1  object-*")

(check-for-bug :format-legacy-788
  (FORMAT NIL "format-p:--~d  object~:p-*" 2)
  "format-p:--2  objects-*")

(check-for-bug :format-legacy-792
  (FORMAT NIL "format-p:--~d  bab~:@p-*" 1)
  "format-p:--1  baby-*")

(check-for-bug :format-legacy-796
  (FORMAT NIL "format-&:--~%~&--1-newline-*")
  "format-&:--
--1-newline-*")

(check-for-bug :format-legacy-801
  (FORMAT NIL "format-&:--~%~3&--3-newline-*")
  "format-&:--


--3-newline-*")

(check-for-bug :format-legacy-808
  (FORMAT NIL "format-tilde:--~~--1-tilde-*")
  "format-tilde:--~--1-tilde-*")

(check-for-bug :format-legacy-812
  (FORMAT NIL "format-tilde:--~3~--3-tilden-*")
  "format-tilde:--~~~--3-tilden-*")

(check-for-bug :format-legacy-816
  (FORMAT NIL "format-|:--~|--1-ff-*")
  "format-|:----1-ff-*")

(check-for-bug :format-legacy-820
  (FORMAT NIL "format-|:--~2|--2-ff-*")
  "format-|:----2-ff-*")

(check-for-bug :format-legacy-824
  (FORMAT NIL
          "format-<nl>:~
                         gl. zeile gl. angeschlossen trotz 2*<tab> und sp-*")
  "format-<nl>:gl. zeile gl. angeschlossen trotz 2*<tab> und sp-*")

(check-for-bug :format-legacy-830
  (FORMAT NIL "format-<nl>:~@
                         neue Zeile Anfang trotz <tab> + sp-*")
  "format-<nl>:
neue Zeile Anfang trotz <tab> + sp-*")

(check-for-bug :format-legacy-836
  (FORMAT NIL "format-<nl>:~:
	gleiche Zeile aber ein tab vor Anfang-*")
  "format-<nl>:	gleiche Zeile aber ein tab vor Anfang-*")

(check-for-bug :format-legacy-841
  (FORMAT NIL "format-?:***~a***~?***~a***-*" 1 "+++~s+++~s+++" (QUOTE
                                                                 (A B)) 2)
  "format-?:***1***+++A+++B+++***2***-*")

(check-for-bug :format-legacy-846
  (FORMAT NIL "format-?:***~a***~?***~a***-*" 1 "+++++++++++++" NIL 2)
  "format-?:***1***+++++++++++++***2***-*")

(check-for-bug :format-legacy-850
  (FORMAT NIL "~(AAAAAAAA BBBBBB ccccccc dddddddd~)")
  "aaaaaaaa bbbbbb ccccccc dddddddd")

(check-for-bug :format-legacy-854
  (FORMAT NIL "~:(AAAAAAAA BBBBBB ccccccc dddddddd~)")
  "Aaaaaaaa Bbbbbb Ccccccc Dddddddd")

(check-for-bug :format-legacy-858
  (FORMAT NIL "~@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
  "Aaaaaaaa bbbbbb ccccccc dddddddd")

(check-for-bug :format-legacy-862
  (FORMAT NIL "~:@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
  "AAAAAAAA BBBBBB CCCCCCC DDDDDDDD")

(check-for-bug :format-legacy-866
  (FORMAT NIL "++~{-=~s=-~}++" (QUOTE (1 2 3)))
  "++-=1=--=2=--=3=-++")

(check-for-bug :format-legacy-870
  (FORMAT NIL "++~2{-=~s=-~}++" (QUOTE (1 2 3)))
  "++-=1=--=2=-++")

(check-for-bug :format-legacy-874
  (FORMAT NIL "++~@{-=~s=-~}++" 1 2 3)
  "++-=1=--=2=--=3=-++")

(check-for-bug :format-legacy-878
  (FORMAT NIL "++~:{-=~s=~s=-~}++" (QUOTE ((1 2) (3 4 5) (6 7))))
  "++-=1=2=--=3=4=--=6=7=-++")

(check-for-bug :format-legacy-882
  (FORMAT NIL "++~:@{-=~s=~s=-~}++" (QUOTE (1 2)) (QUOTE (3 4 5)) (QUOTE
                                                                   (6 7)))
  "++-=1=2=--=3=4=--=6=7=-++")

(check-for-bug :format-legacy-887
  (FORMAT NIL "~{abc~:}")
  #+XCL "abc"
  #-XCL ERROR)

(check-for-bug :format-legacy-892
  (FORMAT NIL "~{~:}" "xyz")
  #+XCL "xyz"
  #-XCL ERROR)

(check-for-bug :format-legacy-897
  (FORMAT NIL "~1{~:}" "-~s-" (QUOTE (1 2)) 3)
  "-1-")

(check-for-bug :format-legacy-901
  (FORMAT NIL "123456789012345678901234567890
~10,4txx~10,4ty~10,4tzzz~10,4tende")
  #+XCL
  "123456789012345678901234567890
         xx  y   zzz ende"
  #-XCL
  "123456789012345678901234567890
          xx  y   zzz ende")

(check-for-bug :format-legacy-911
  (FORMAT NIL "123456789012345678901234567890
~3,4@txx~3,4@ty~3,4@tzzz~3,4@tende")
  #+XCL
  "123456789012345678901234567890
   xx      y   zzz     ende"
  #-XCL
  "123456789012345678901234567890
    xx      y   zzz     ende")

(check-for-bug :format-legacy-921
  (FORMAT NIL "-~a-~a-~a-~a-" 1 2 3 4 5 6 7 8 9)
  "-1-2-3-4-")

(check-for-bug :format-legacy-925
  (FORMAT NIL "-~a-~a-~*~a-~a-" 1 2 3 4 5 6 7 8 9)
  "-1-2-4-5-")

(check-for-bug :format-legacy-929
  (FORMAT NIL "-~a-~a-~3*~a-~a-" 1 2 3 4 5 6 7 8 9)
  "-1-2-6-7-")

(check-for-bug :format-legacy-933
  (FORMAT NIL "-~a-~a-~:*~a-~a-" 1 2 3 4 5 6 7 8 9)
  "-1-2-2-3-")

(check-for-bug :format-legacy-937
  (FORMAT NIL "-~a-~a-~2:*~a-~a-" 1 2 3 4 5 6 7 8 9)
  "-1-2-1-2-")

(check-for-bug :format-legacy-941
  (FORMAT NIL "-~a-~a-~@*~a-~a-" 1 2 3 4 5 6 7 8 9)
  "-1-2-1-2-")

(check-for-bug :format-legacy-945
  (FORMAT NIL "-~a-~a-~6@*~a-~a-" 1 2 3 4 5 6 7 8 9)
  "-1-2-7-8-")

(check-for-bug :format-legacy-949
  (FORMAT NIL "~[aa~;bb~;cc~]" 1)
  "bb")

(check-for-bug :format-legacy-953
  (FORMAT NIL "~[aa~;bb~;cc~]" 10)
  "")

(check-for-bug :format-legacy-957
  (FORMAT NIL "~2[aa~;bb~;cc~]" 10)
  "cc")

(check-for-bug :format-legacy-961
  (FORMAT NIL "~@[aaa~]" NIL 10)
  "")

(check-for-bug :format-legacy-965
  (FORMAT NIL "~@[aaa~]" 20 10)
  "aaa")

(check-for-bug :format-legacy-969
  (FORMAT NIL "~@[aaa~d~]" NIL 10)
  "")

(check-for-bug :format-legacy-973
  (FORMAT NIL "~@[aaa~d~]" 20 10)
  "aaa20")

(check-for-bug :format-legacy-977
  (FORMAT NIL "~@[aaa~d~]bbb~d" NIL 10 30)
  "bbb10")

(check-for-bug :format-legacy-981
  (FORMAT NIL "~@[aaa~d~]bbb~d" 20 10 30)
  "aaa20bbb10")

(check-for-bug :format-legacy-985
  (FORMAT NIL "~:[-nil-~;-true-~d~]-ende~d" NIL 10 20)
  "-nil--ende10")

(check-for-bug :format-legacy-989
  (FORMAT NIL "~:[-nil-~;-true-~d~]-ende~d" T 10 20)
  "-true-10-ende20")

(check-for-bug :format-legacy-993
  (FORMAT NIL "Start test, newline:~%freshline:~&")
  "Start test, newline:
freshline:
")

(check-for-bug :format-legacy-999
  (FORMAT NIL "decimal pad with period:~10,vd-*" #\. 12)
  "decimal pad with period:........12-*")

(check-for-bug :format-legacy-1003
  (FORMAT NIL "char normal:~c, as ~%# would read:~%~@c, human read:~:c-*"
          #\SPACE
          #\SPACE #\SPACE)
  #+(or XCL cmu sbcl CLISP) "char normal: , as 
# would read:
#\\Space, human read:Space-*"
  #+(or AKCL LUCID)    "char normal:Space, as 
# would read:
#\\Space, human read:Space-*"
  #+ALLEGRO            "char normal: , as 
# would read:
#\\space, human read:space-*"
  #-(or XCL cmu sbcl CLISP AKCL LUCID ALLEGRO) UNKNOWN)

(check-for-bug :format-legacy-1018
  (FORMAT NIL
          "cardinal:~r, roman new:~@r, roman-old:~:@r~
                <same line I hope>~@
                new line but at beginning~:
   same line, but spaced out~@
        new line and over two tabs-*" 4 4 4)
  "cardinal:four, roman new:IV, roman-old:IIII<same line I hope>
new line but at beginning   same line, but spaced out
new line and over two tabs-*")

(check-for-bug :format-legacy-1029
  (SETQ N 3)
  3)

(check-for-bug :format-legacy-1033
  (FORMAT NIL "~D item~:P found." N)
  "3 items found.")

(check-for-bug :format-legacy-1037
  (FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
  "three dogs are here.")

(check-for-bug :format-legacy-1041
  (FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
  "three dogs are here.")

(check-for-bug :format-legacy-1045
  (FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
  "Here are three puppies.")

(check-for-bug :format-legacy-1049
  (SETQ N 1)
  1)

(check-for-bug :format-legacy-1053
  (FORMAT NIL "~D item~:P found." N)
  "1 item found.")

(check-for-bug :format-legacy-1057
  (FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
  "one dog is here.")

(check-for-bug :format-legacy-1061
  (FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
  "one dog is here.")

(check-for-bug :format-legacy-1065
  (FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
  "Here is one puppy.")

(check-for-bug :format-legacy-1069
  (SETQ N 0)
  0)

(check-for-bug :format-legacy-1073
  (FORMAT NIL "~D item~:P found." N)
  "0 items found.")

(check-for-bug :format-legacy-1077
  (FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
  "zero dogs are here.")

(check-for-bug :format-legacy-1081
  (FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
  "zero dogs are here.")

(check-for-bug :format-legacy-1085
  (FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
  "Here are zero puppies.")

(check-for-bug :format-legacy-1089
  (FORMAT NIL "~D tr~:@p/~D win~:P" 7 1)
  "7 tries/1 win")

(check-for-bug :format-legacy-1093
  (FORMAT NIL "~D tr~:@p/~D win~:P" 1 0)
  "1 try/0 wins")

(check-for-bug :format-legacy-1097
  (FORMAT NIL "~D tr~:@p/~D win~:P" 1 3)
  "1 try/3 wins")

(check-for-bug :format-legacy-1101
  (DEFUN TYPE-CLASH-ERROR (FN NARGS ARGNUM RIGHT-TYPE WRONG-TYPE) (FORMAT
                                                                   NIL
                                                                   "~&~S requires itts ~:[~:R~;~*~] ~
           argument to be of type ~S,~%but it was called ~
           with an argument of type ~S.-*" FN (EQL NARGS 1) ARGNUM
                                                                   RIGHT-TYPE
                                                                   WRONG-TYPE))
  TYPE-CLASH-ERROR)

(check-for-bug :format-legacy-1111
  (TYPE-CLASH-ERROR (QUOTE AREF) NIL 2 (QUOTE INTEGER) (QUOTE VECTOR))
  "AREF requires itts second argument to be of type INTEGER,
but it was called with an argument of type VECTOR.-*")

(check-for-bug :format-legacy-1116
  (TYPE-CLASH-ERROR (QUOTE CAR) 1 1 (QUOTE LIST) (QUOTE SHORT-FLOAT))
  "CAR requires itts  argument to be of type LIST,
but it was called with an argument of type SHORT-FLOAT.-*")

(check-for-bug :format-legacy-1121
  (FORMAT NIL "~? ~D" "<~A ~D>" (QUOTE ("Foo" 5)) 7)
  "<Foo 5> 7")

(check-for-bug :format-legacy-1125
  (FORMAT NIL "~? ~D" "<~A ~D>" (QUOTE (" Foo" 5 14)) 7)
  "< Foo 5> 7")

(check-for-bug :format-legacy-1129
  (FORMAT NIL "~@? ~d" "<~A ~D>" "Foo" 5 7)
  "<Foo 5> 7")

(check-for-bug :format-legacy-1133
  (FORMAT NIL "~@? ~D" "<~A ~D>" "Foo" 5 14 7)
  "<Foo 5> 14")

(check-for-bug :format-legacy-1137
  (FORMAT NIL "~@R ~(~@R~)" 14 14)
  "XIV xiv")

(check-for-bug :format-legacy-1141
  (DEFUN F (N) (FORMAT NIL "~@(~R~) error~:P detected." N))
  F)

(check-for-bug :format-legacy-1145
  (F 0)
  "Zero errors detected.")

(check-for-bug :format-legacy-1149
  (F 1)
  "One error detected.")

(check-for-bug :format-legacy-1153
  (F 23)
  "Twenty-three errors detected.")

(check-for-bug :format-legacy-1157
  (SETQ *PRINT-LEVEL* NIL *PRINT-LENGTH* 5)
  5)

(check-for-bug :format-legacy-1161
  (FORMAT NIL "~@[ print level = ~D~]~@[ print length = ~D~]" *PRINT-LEVEL*

          *PRINT-LENGTH*)
  " print length = 5")

(check-for-bug :format-legacy-1167
  (SETQ *PRINT-LENGTH* NIL)
  NIL)

(check-for-bug :format-legacy-1171
  (SETQ FOO
        "Items:~#[none~; ~s~; ~S and ~S~
          ~:;~@{~#[~; and~] ~S~^,~}~].")
  "Items:~#[none~; ~s~; ~S and ~S~
          ~:;~@{~#[~; and~] ~S~^,~}~].")

(check-for-bug :format-legacy-1178
  (FORMAT NIL FOO)
  "Items:none.")

(check-for-bug :format-legacy-1182
  (FORMAT NIL FOO (QUOTE FOO))
  "Items: FOO.")

(check-for-bug :format-legacy-1186
  (FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR))
  "Items: FOO and BAR.")

(check-for-bug :format-legacy-1190
  (FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR) (QUOTE BAZ))
  "Items: FOO, BAR, and BAZ.")

(check-for-bug :format-legacy-1194
  (FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR) (QUOTE BAZ) (QUOTE QUUX))
  "Items: FOO, BAR, BAZ, and QUUX.")

(check-for-bug :format-legacy-1198
  (FORMAT NIL "The winners are:~{ ~S~}." (QUOTE (FRED HARRY JILL)))
  "The winners are: FRED HARRY JILL.")

(check-for-bug :format-legacy-1202
  (FORMAT NIL "Pairs:~{ <~S,~S>~}." (QUOTE (A 1 B 2 C 3)))
  "Pairs: <A,1> <B,2> <C,3>.")

(check-for-bug :format-legacy-1206
  (FORMAT NIL "Pairs:~:{ <~S,~S>~}." (QUOTE ((A 1) (B 2) (C 3))))
  "Pairs: <A,1> <B,2> <C,3>.")

(check-for-bug :format-legacy-1210
  (FORMAT NIL "Pairs:~@{ <~S,~S>~}." (QUOTE A) 1 (QUOTE B) 2 (QUOTE C)
          3)
  "Pairs: <A,1> <B,2> <C,3>.")

(check-for-bug :format-legacy-1215
  (FORMAT NIL "Pairs:~:@{ <~S,~S>~}." (QUOTE (A 1)) (QUOTE (B 2)) (QUOTE
                                                                   (C 3)))
  "Pairs: <A,1> <B,2> <C,3>.")

(check-for-bug :format-legacy-1220
  (SETQ DONESTR "done.~^ ~D warning~:P.~^ ~D error~:P.")
  "done.~^ ~D warning~:P.~^ ~D error~:P.")

(check-for-bug :format-legacy-1224
  (FORMAT NIL DONESTR)
  "done.")

(check-for-bug :format-legacy-1228
  (FORMAT NIL DONESTR 3)
  "done. 3 warnings.")

(check-for-bug :format-legacy-1232
  (FORMAT NIL DONESTR 1 5)
  "done. 1 warning. 5 errors.")

(check-for-bug :format-legacy-1236
  (SETQ TELLSTR "~@(~@[~R~]~^ ~A.~)")
  "~@(~@[~R~]~^ ~A.~)")

(check-for-bug :format-legacy-1240
  (FORMAT NIL TELLSTR 23)
  "Twenty-three")

(check-for-bug :format-legacy-1244
  (FORMAT NIL TELLSTR NIL "losers")
  " Losers.")

(check-for-bug :format-legacy-1248
  (FORMAT NIL TELLSTR 23 "losers")
  "Twenty-three losers.")

(check-for-bug :format-legacy-1252
  (FORMAT NIL "**~c**" #\SPACE)
  #+(or XCL cmu sbcl CLISP ALLEGRO) "** **"
  #+(or AKCL LUCID)            "**Space**"
  #-(or XCL cmu sbcl CLISP AKCL LUCID ALLEGRO) UNKNOWN)

(check-for-bug :format-legacy-1258
  (FORMAT NIL "**~:c**" #\SPACE)
  "**Space**")

(check-for-bug :format-legacy-1262
  (FORMAT NIL "**~:@c**" #\SPACE)
  "**Space**")

(check-for-bug :format-legacy-1266
  (FORMAT NIL "**~@c**" #\SPACE)
  "**#\\Space**")

(check-for-bug :format-legacy-1270
  (FORMAT NIL "**~c**" #\A)
  "**A**")

(check-for-bug :format-legacy-1274
  (FORMAT NIL "**~:c**" #\A)
  "**A**")

(check-for-bug :format-legacy-1278
  (FORMAT NIL "**~:@c**" #\A)
  "**A**")

(check-for-bug :format-legacy-1282
  (FORMAT NIL "**~@c**" #\A)
  "**#\\A**")

#+XCL
(check-for-bug :format-legacy-1287
  (FORMAT NIL "**~c**" (CODE-CHAR 26))
  "****")

#+clisp
(check-for-bug :format-legacy-1292
  (FORMAT NIL "**~c**" (CODE-CHAR 27))
  "****")

#+XCL
(check-for-bug :format-legacy-1297
  (FORMAT NIL "**~:c**" (CODE-CHAR 26))
  "**Z**")

#+clisp
(check-for-bug :format-legacy-1302
  (FORMAT NIL "**~:c**" (CODE-CHAR 27))
  "**Escape**")

#+XCL
(check-for-bug :format-legacy-1307
  (FORMAT NIL "**~:@c**" (CODE-CHAR 26))
  "**^Z**")

#+clisp
(check-for-bug :format-legacy-1312
  (FORMAT NIL "**~:@c**" (CODE-CHAR 27))
  "**Escape**")

#+XCL
(check-for-bug :format-legacy-1317
  (FORMAT NIL "**~@c**" (CODE-CHAR 26))
  "**#\\**")

#+clisp
(check-for-bug :format-legacy-1322
  (FORMAT NIL "**~@c**" (CODE-CHAR 27))
  "**#\\Escape**")

(check-for-bug :format-legacy-1326
  (progn (fmakunbound 'foo)
         (makunbound 'liste)
         t)
  T)

