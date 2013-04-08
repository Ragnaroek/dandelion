;;; section 16: strings -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; simple-string-p

(check-for-bug :section16-legacy-8
  (simple-string-p "aaaaaa")
  t)

(check-for-bug :section16-legacy-12
  (simple-string-p (make-array 6
                               :element-type 'character
                               :fill-pointer t))
  nil)

;;; char

(check-for-bug :section16-legacy-20
  (setq my-simple-string (make-string 6 :initial-element #\A))
  "AAAAAA")

(check-for-bug :section16-legacy-24
  (schar my-simple-string 4)
  #\A)

(check-for-bug :section16-legacy-28
  (setf (schar my-simple-string 4) #\B)
  #\B)

(check-for-bug :section16-legacy-32
  my-simple-string
  "AAAABA")

(check-for-bug :section16-legacy-36
  (setq my-filled-string
        (make-array 6 :element-type 'character
                    :fill-pointer 5
                    :initial-contents my-simple-string))
  "AAAAB")

(check-for-bug :section16-legacy-43
  (char my-filled-string 4)
  #\B)

(check-for-bug :section16-legacy-47
  (char my-filled-string 5)
  #\A
  "char: ...

char ignores fill pointers when accessing elements. ")

(check-for-bug :section16-legacy-54
  (setf (char my-filled-string 3) #\C)
  #\C)

(check-for-bug :section16-legacy-58
  (setf (char my-filled-string 5) #\D)
  #\D
  "char: ...

char ignores fill pointers when accessing elements. ")

(check-for-bug :section16-legacy-65
  (setf (fill-pointer my-filled-string) 6)
  6)

(check-for-bug :section16-legacy-69
  my-filled-string
  "AAACBD")

;;; string

(check-for-bug :section16-legacy-75
  (string "already a string")
  "already a string")

(check-for-bug :section16-legacy-79
  (string 'elm)
  "ELM")

(check-for-bug :section16-legacy-83
  (string #\c)
  "c")

;;; string-upcase

(check-for-bug :section16-legacy-89
  (string-upcase "abcde")
  "ABCDE")

(check-for-bug :section16-legacy-93
  (string-upcase "Dr. Livingston, I presume?")
  "DR. LIVINGSTON, I PRESUME?")

(check-for-bug :section16-legacy-97
  (string-upcase "Dr. Livingston, I presume?" :start 6 :end 10)
  "Dr. LiVINGston, I presume?")

(check-for-bug :section16-legacy-101
  (string-downcase "Dr. Livingston, I presume?")
  "dr. livingston, i presume?")

(check-for-bug :section16-legacy-105
  (string-capitalize "elm 13c arthur;fig don't")
  "Elm 13c Arthur;Fig Don'T")

(check-for-bug :section16-legacy-109
  (string-capitalize " hello ")
  " Hello ")

(check-for-bug :section16-legacy-113
  (string-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
  "Occluded Casements Forestall Inadvertent Defenestration")

(check-for-bug :section16-legacy-117
  (string-capitalize 'kludgy-hash-search)
  "Kludgy-Hash-Search")

(check-for-bug :section16-legacy-121
  (string-capitalize "DON'T!")
  "Don'T!")				;not "Don't!"

(check-for-bug :section16-legacy-125
  (string-capitalize "pipe 13a, foo16c")
  "Pipe 13a, Foo16c")

(check-for-bug :section16-legacy-129
  (setq str (copy-seq "0123ABCD890a"))
  "0123ABCD890a")

(check-for-bug :section16-legacy-133
  (nstring-downcase str :start 5 :end 7)
  "0123AbcD890a")

(check-for-bug :section16-legacy-137
  str
  "0123AbcD890a")

;;; string-trim

(check-for-bug :section16-legacy-143
  (string-trim "abc" "abcaakaaakabcaaa")
  "kaaak")

(check-for-bug :section16-legacy-147
  (string-trim '(#\Space #\Tab #\Newline) " garbanzo beans
        ")
  "garbanzo beans")

(check-for-bug :section16-legacy-152
  (string-trim " (*)" " ( *three (silly) words* ) ")
  "three (silly) words")

(check-for-bug :section16-legacy-156
  (string-left-trim "abc" "labcabcabc")
  "labcabcabc")

(check-for-bug :section16-legacy-160
  (string-left-trim " (*)" " ( *three (silly) words* ) ")
  "three (silly) words* ) ")

(check-for-bug :section16-legacy-164
  (string-right-trim " (*)" " ( *three (silly) words* ) ")
  " ( *three (silly) words")

;;; string=

(check-for-bug :section16-legacy-170
  (string= "foo" "foo")
  t)

(check-for-bug :section16-legacy-174
  (string= "foo" "Foo")
  nil)

(check-for-bug :section16-legacy-178
  (string= "foo" "bar")
  nil)

(check-for-bug :section16-legacy-182
  (string= "together" "frog" :start1 1 :end1 3 :start2 2)
  t)

(check-for-bug :section16-legacy-186
  (string-equal "foo" "Foo")
  t)

(check-for-bug :section16-legacy-190
  (string= "abcd" "01234abcd9012" :start2 5 :end2 9)
  t)

(check-for-bug :section16-legacy-194
  (string< "aaaa" "aaab")
  3)

(check-for-bug :section16-legacy-198
  (string>= "aaaaa" "aaaa")
  4)

(check-for-bug :section16-legacy-202
  (string-not-greaterp "Abcde" "abcdE")
  5)

(check-for-bug :section16-legacy-206
  (string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7
                :start2 2 :end2 6)
  6)

(check-for-bug :section16-legacy-211
  (string-not-equal "AAAA" "aaaA")
  nil)

;;; stringp

(check-for-bug :section16-legacy-217
  (stringp "aaaaaa")
  t)

(check-for-bug :section16-legacy-221
  (stringp #\a)
  nil)

;;; make-string

(check-for-bug :section16-legacy-227
  (make-string 10 :initial-element #\5)
  "5555555555")

(check-for-bug :section16-legacy-231
  (length (make-string 10))
  10)







