;;; based on v1.25 -*- mode: lisp -*-
;;; the trick is that the tests that come with CLISP are run in the
;;; traditional mode, when symbols are recognized as pathname
;;; designators, while these tests are run in the ansi mode, where the
;;; only pathname designators are pathnames, strings and file streams.

(in-package :cl-user)

(check-for-bug :path-legacy-4
  (setf string "test-pathname.abc" symbol 'test-pathname.abc)
  test-pathname.abc)

;;pathname argument type: pathname,string,stream [ symbol - non-ANSI]
;;         result: pathname

(check-for-bug :path-legacy-11
  (SETF PATHSTRING (PATHNAME STRING))
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
              TYPE "ABC" SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(check-for-bug :path-legacy-21
  (SETF PATHSYMBOL (PATHNAME #+XCL symbol #+CLISP string))
  #+XCL
  #S(PATHNAME SYSTEM::HOST
              NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
              "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(check-for-bug :path-legacy-31
  (SETF PATHPATH (PATHNAME PATHSYMBOL))
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
              TYPE "ABC" SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(check-for-bug :path-legacy-41
  (SETF STREAM (OPEN STRING :DIRECTION :OUTPUT)
        a nil)
  nil)

;; (SETF PATHSTREAM (PATHNAME STREAM))
;; "test-pathname.lsp"

(check-for-bug :path-legacy-49
  (MAPCAR (FUNCTION PATHNAMEP)
          (LIST PATHSTRING PATHSYMBOL PATHPATH ;PATHSTREAM
                ))
  (T T T                                ;T
     ))


;; funktion truename liefert filename fuer pathname oder stream
;;                   einen Pfadnamen
;;
;; (MAPCAR (FUNCTION TRUENAME) (LIST PATHSTRING PATHSYMBOL PATHPATH STREAM
;;                                                                ;PATHSTREAM
;;                                                                  ))
;;   ERROR



(check-for-bug :path-legacy-67
  (PARSE-NAMESTRING STRING)
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
              TYPE "ABC" SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(check-for-bug :path-legacy-77
  (PARSE-NAMESTRING #+XCL SYMBOL #+CLISP STRING)
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
              TYPE "ABC" SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

#+XCL

(check-for-bug :path-legacy-89
  (PARSE-NAMESTRING "bab:test-pathname.abc")
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
              "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL))

#+XCL
(check-for-bug :path-legacy-96
  (PARSE-NAMESTRING "bab:test-pathname.abc;3")
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
              "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION 3))

(check-for-bug :path-legacy-102
  (PARSE-NAMESTRING PATHSTRING)
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
              "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC"
              SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(check-for-bug :path-legacy-112
  (PARSE-NAMESTRING "test-pathname.abc" NIL)
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
              TYPE "ABC" SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

#+XCL
(check-for-bug :path-legacy-123
  (PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc")
  #S(PATHNAME
     SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
     SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL))

#+XCL
(check-for-bug :path-legacy-130
  (PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "sirius")
  #S(PATHNAME
     SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
     SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL))

#+XCL
(check-for-bug :path-legacy-137
  (PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "orion")
  ERROR)

(check-for-bug :path-legacy-141
  (PARSE-NAMESTRING "abc.123" NIL NIL :START 0 :END 5)
  #+XCL
  #S(PATHNAME SYSTEM::HOST
              NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "ABC" TYPE
              "1" SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "abc" :TYPE "1" :VERSION NIL))

(check-for-bug :path-legacy-151
  (PARSE-NAMESTRING "abc.123" NIL NIL :START 2 :END 5)
  #+XCL
  #S(PATHNAME SYSTEM::HOST
              NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "C" TYPE "1"
              SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME "c" :TYPE "1" :VERSION NIL))

#+XCL
(check-for-bug :path-legacy-162
  (PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 3)
  #S(PATHNAME SYSTEM::HOST
              NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME NIL TYPE
              NIL SYSTEM::VERSION NIL))

#+XCL
(check-for-bug :path-legacy-169
  (PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 7)
  #S(PATHNAME SYSTEM::HOST
              NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "BABYLON"
              TYPE NIL SYSTEM::VERSION NIL))

#+XCL
(check-for-bug :path-legacy-176
  (PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 7)
  #S(PATHNAME
     SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
     "BABYLON" TYPE NIL SYSTEM::VERSION NIL))

(check-for-bug :path-legacy-182
  (make-pathname :device nil :defaults *DEFAULT-PATHNAME-DEFAULTS*)
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE NIL
              DIRECTORY NIL SYSTEM::NAME NIL TYPE "lsp"
              SYSTEM::VERSION :NEWEST)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME NIL :TYPE NIL :VERSION NIL))

#+XCL
(check-for-bug :path-legacy-195
  (PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 3)
  #S(PATHNAME
     SYSTEM::HOST NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2"
     SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION NIL))

;; (PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED T)
;; #S(PATHNAME
;; SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;; "BABYLON" TYPE "C" SYSTEM::VERSION NIL)

;; (PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED T)
;; #S(PATHNAME
;; SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;; "BABYLON" TYPE NIL SYSTEM::VERSION NIL)

#+XCL
(check-for-bug :path-legacy-212
  (PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED NIL)
  ERROR)

#+XCL
(check-for-bug :path-legacy-217
  (PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED NIL)
  ERROR)

#+XCL
(check-for-bug :path-legacy-222
  (PARSE-NAMESTRING "babylon.c;c" NIL NIL :JUNK-ALLOWED NIL)
  ERROR)

#+XCL
(check-for-bug :path-legacy-227
  (PARSE-NAMESTRING "babylon.c;" NIL NIL :JUNK-ALLOWED NIL)
  #S(PATHNAME
     SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
     "BABYLON" TYPE "C" SYSTEM::VERSION NIL))

#+XCL
(check-for-bug :path-legacy-234
  (PARSE-NAMESTRING "babylon.c;5" NIL NIL :JUNK-ALLOWED NIL)
  #S(PATHNAME
     SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
     "BABYLON" TYPE "C" SYSTEM::VERSION 5))

;; (MERGE-PATHNAME "test$$" SYMBOL 10)   ERROR
;;
;; (MERGE-PATHNAME "test$$" SYMBOL)   ERROR
;;
;; (MERGE-PATHNAME "test$$" PATH)   ERROR
;;
;; (MERGE-PATHNAME "test$$")   ERROR

#+XCL
(check-for-bug :path-legacy-249
  (MERGE-PATHNAMES "test$$")
  #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
              "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "lsp"
              SYSTEM::VERSION :NEWEST))

#+XCL
(check-for-bug :path-legacy-256
  (MERGE-PATHNAMES "test$$" SYMBOL)
  #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
              "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "ABC"
              SYSTEM::VERSION :NEWEST))

#+XCL
(check-for-bug :path-legacy-263
  (MERGE-PATHNAMES "test$$" SYMBOL 2)
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
              "ABC" SYSTEM::VERSION 2))

#+XCL
(check-for-bug :path-legacy-270
  (MERGE-PATHNAMES "test$$" (PATHNAME SYMBOL) 2)
  #S(PATHNAME SYSTEM::HOST NIL
              SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
              "ABC" SYSTEM::VERSION 2))

#+XCL
(check-for-bug :path-legacy-277
  (MERGE-PATHNAMES "test$$" STREAM 2)
  #S(PATHNAME SYSTEM::HOST 16 SYSTEM::DEVICE
              "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE :ESCAPE
              SYSTEM::VERSION 2))


;; (MERGE-PATHNAME STRING SYMBOL)   ERROR

#+XCL
(check-for-bug :path-legacy-287
  (MAKE-PATHNAME :NAME "a" :HOST (QUOTE ORION))
  #S(PATHNAME SYSTEM::HOST ORION
              SYSTEM::DEVICE NIL DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
              :NEWEST))

#+XCL
(check-for-bug :path-legacy-294
  (DEFMACRO TEST (&REST BODY) (\` (APPLY (FUNCTION MAKE-PATHNAME) (\,@ BODY))))
  TEST)

#+XCL
(check-for-bug :path-legacy-299
  (setf a '(:host "sirius" :name "a"))
  (:host "sirius" :name "a"))

#+XCL
(check-for-bug :path-legacy-304
  (TEST A)
  #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE NIL DIRECTORY NIL
              SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST))

#+XCL
(check-for-bug :path-legacy-310
  (SETF A (LIST* :DEVICE "disk00$abt43" A))
  (:DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(check-for-bug :path-legacy-315
  (TEST A)
  #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
              DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST))

#+XCL
(check-for-bug :path-legacy-321
  (SETF A (LIST* :DIRECTORY "[heicking.comlisp]" A))
  (:DIRECTORY
   "[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(check-for-bug :path-legacy-327
  (TEST A)
  #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
              DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
              :NEWEST))

#+XCL
(check-for-bug :path-legacy-334
  (SETF A (LIST* :TYPE "raf" A))
  (:TYPE "raf" :DIRECTORY "[heicking.comlisp]"
         :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(check-for-bug :path-legacy-340
  (TEST A)
  #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
              DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION
              :NEWEST))

#+XCL
(check-for-bug :path-legacy-347
  (SETF A (LIST* :VERSION 3 A))
  (:VERSION 3 :TYPE "raf" :DIRECTORY
            "[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(check-for-bug :path-legacy-353
  (TEST A)
  #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
              DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION 3))

(check-for-bug :path-legacy-358
  (MAPCAR (FUNCTION PATHNAMEP) (LIST PATHSYMBOL PATHPATH PATHSTRING))
  (T T T))

#+XCL
(check-for-bug :path-legacy-363
  (SETF PATH (TEST A))
  #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE
              "disk00$abt43" DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf"
              SYSTEM::VERSION 3))

#+XCL
(check-for-bug :path-legacy-370
  (MAPCAR (FUNCTION PATHNAME-HOST) (LIST SYMBOL STRING STREAM PATH))
  (NIL NIL NIL NIL))

#+XCL
(check-for-bug :path-legacy-375
  (MAPCAR (FUNCTION PATHNAME-DEVICE) (LIST SYMBOL STRING STREAM PATH))
  ("DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43"))

#+XCL
(check-for-bug :path-legacy-380
  (MAPCAR (FUNCTION PATHNAME-DIRECTORY) (LIST SYMBOL STRING STREAM PATH))
  ("XCL.MAIN" "XCL.MAIN" "XCL.MAIN" "XCL.MAIN"))

(check-for-bug :path-legacy-384
  (PROGN (CLOSE STREAM) T)
  T)

#+XCL
(check-for-bug :path-legacy-389
  (USER-HOMEDIR-PATHNAME)
  #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
              "DISK00$ABT43" DIRECTORY "HEICKING" SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION
              NIL))

(check-for-bug :path-legacy-395
  (PATHNAME "*.*")
  #+XCL
  #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43"
              DIRECTORY "HEICKING" SYSTEM::NAME "*" TYPE :WILD SYSTEM::VERSION NIL)
  #+CLISP
  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
              :NAME :WILD :TYPE :WILD :VERSION NIL)
  #-(or XCL CLISP)
  #P"*.*")

(check-for-bug :path-truename-with-borken-path
  (truename "~/no/ such / path /  nicht-vorhandenes-file.new")
  error)

(check-for-bug :path-legacy-406
  (progn (setf file (open "nicht-vorhandenes-file.non"
                          :direction :input
                          :element-type 'character
                          :if-does-not-exist :create)) t)
  t
  "")

(check-for-bug :path-legacy-414
  (null (probe-file "nicht-vorhandenes-file.non"))
  NIL)

(check-for-bug :path-legacy-418
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-422
  (setf file (open "nicht-vorhandenes-file.non"
                   :direction :io
                   :element-type 'string-char
                   :if-exists :error))
  error)

(check-for-bug :path-legacy-429
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-433
  (null (setf file (open "nicht-vorhandenes-file.non"
                         :direction :io
                         :element-type 'character
                         :if-exists :new-version)))
  nil
  "")

(check-for-bug :path-legacy-441
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-445
  (null (setf file (open "nicht-vorhandenes-file.non"
                         :direction :io
                         :element-type 'character
                         :if-exists :rename)))
  nil)

(check-for-bug :path-legacy-452
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-456
  (null (setf file (open "nicht-vorhandenes-file.non"
                         :direction :io
                         :element-type 'character
                         :if-exists :rename-and-delete)))
  nil)

(check-for-bug :path-legacy-463
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-467
  (null (setf file (open "nicht-vorhandenes-file.non"
                         :direction :io
                         :element-type 'character
                         :if-exists :overwrite)))
  nil)

(check-for-bug :path-legacy-474
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-478
  (null (setf file (open "nicht-vorhandenes-file.non"
                         :direction :io
                         :element-type 'character
                         :if-exists :append)))
  nil)

(check-for-bug :path-legacy-485
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-489
  (null (setf file (open "nicht-vorhandenes-file.non"
                         :direction :io
                         :element-type 'character
                         :if-exists :supersede)))
  nil)

(check-for-bug :path-legacy-496
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-500
  (setf file (open "nicht-vorhandenes-file.non"
                   :direction :io
                   :element-type 'character
                   :if-exists nil))
  nil)

(check-for-bug :path-legacy-507
  (progn (close file) t)
  error)

(check-for-bug :path-legacy-511
  (setf file (open "nicht-vorhandenes-file.new"
                   :direction :io
                   :element-type 'character
                   :if-does-not-exist :error))
  error)

(check-for-bug :path-legacy-518
  (progn (close file) t)
  error)

(check-for-bug :path-legacy-522
  (null (setf file (open "nicht-vorhandenes-file.new"
                         :direction :io
                         :element-type 'character
                         :if-does-not-exist :create)))
  nil)

(check-for-bug :path-legacy-529
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-533
  (null (setf file (open "nicht-vorhandenes-file.non"
                         :direction :io
                         :element-type 'character
                         :if-does-not-exist nil)))
  nil)

(check-for-bug :path-legacy-540
  (progn (close file) t)
  t)

(check-for-bug :path-legacy-544
  (namestring
   (multiple-value-setq (new-name pathname truename)
     (rename-file "nicht-vorhandenes-file.non" "file.da")))
  "file.da")

(check-for-bug :path-legacy-550
  (namestring new-name)
  "file.da")

(check-for-bug :path-legacy-554
  (null pathname)
  nil)

(check-for-bug :path-legacy-558
  (null truename)
  nil)

(check-for-bug :path-legacy-562
  (progn (delete-file "test-pathname.abc") t)
  t)

(check-for-bug :path-legacy-566
  (progn (mapc #'delete-file (directory "nicht-vorhandenes-file.*")) t)
  t)

(check-for-bug :path-legacy-570
  (progn (delete-file "file.da") t)
  t)


(check-for-bug :path-legacy-574
  (progn
    (setf (logical-pathname-translations "clocc")
          '(("**;*" "/usr/local/src/clocc/**/*")))
    nil)
  nil)

(check-for-bug :path-legacy-581
  (translate-logical-pathname "clocc:src;port;")
  #P"/usr/local/src/clocc/src/port/")

(check-for-bug :path-added-1
  (progn
    (setf (logical-pathname-translations "clocc")
          '(("**;*" "/usr/local/src/clocc/**/*"))
          (logical-pathname-translations "CL-LIBRARY")
          '((";**;*.*.*" "/tmp/clisp/"))
          (logical-pathname-translations "cl-systems")
          '((";**;*.*.*"  "/usr/share/common-lisp/systems/**/*.*")
            ("**;*.*.*"  "/usr/share/common-lisp/systems/**/*.*")
            (";*.*.*"  "/usr/share/common-lisp/systems/*.*")
            ("*.*.*"  "/usr/share/common-lisp/systems/*.*"))
          (logical-pathname-translations "TEST-SIMPLE")
          '(("*.*.*" "/usr/local/tmp/*.*.*")
            ("*.*" "/usr/local/tmp/*.*"))
          (logical-pathname-translations "TEST-SUBDIR")
          '(("**;*.*" "/usr/local/share/**/*.*")
            ("**;*.*.*" "/usr/local/share/**/*.*.*")
            (";**;*.*" "/usr/local/share/r/**/*.*")
            (";**;*.*.*" "/usr/local/share/r/**/*.*.*")))
    nil)
  nil)

(check-for-bug :path-added-2
  (translate-logical-pathname "clocc:src;port;")
  #P "/usr/local/src/clocc/src/port/")

(check-for-bug :path-added-3
  (translate-pathname "foobar" "foo*" "*baz")
  #P"barbaz")

(check-for-bug :path-added-4
  (translate-pathname "foobarbazquux" "foo*baz*" "*baq*zot")
  #P"barbaqquuxzot")

(check-for-bug :path-added-5
  (translate-pathname "foobarbazquuxfff" "foo*baz*f?" "*baq*zot*")
  #P"barbaqquuxfzotf")

(check-for-bug :path-added-6
  (translate-pathname "uufoobarbazquuxfff" "u?foo*baz*f?" "**baq*zot*")
  #P"ubarbaqquuxfzotf")

(check-for-bug :path-added-6-1
 (translate-pathname "test.txt" "*.txt" "*.text")
 #P"test.text")

(check-for-bug :path-added-6-2
 (translate-pathname "foo/bar" "*/bar" "*/baz")
 #P"foo/baz")

(check-for-bug :path-added-6-3
 (translate-pathname "bar/foo" "bar/*" "baz/*")
 #P"baz/foo")

(check-for-bug :path-added-7
  (make-pathname :defaults "**/*.FASL" :host "CL-LIBRARY")
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL
                      :DIRECTORY (:RELATIVE :WILD-INFERIORS)
                      :NAME :WILD :TYPE "FASL" :VERSION NIL)
  #-CLISP
  FIXME)

(check-for-bug :path-added-8
  (make-pathname :defaults "/**/*.FASL" :host "CL-LIBRARY")
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL
                      :DIRECTORY (:ABSOLUTE :WILD-INFERIORS)
                      :NAME :WILD :TYPE "FASL" :VERSION NIL)
  #-CLISP
  FIXME)

(check-for-bug :path-added-9
  (merge-pathnames (logical-pathname "cl-systems:")
                   "metering.system")
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CL-SYSTEMS" :DEVICE NIL :DIRECTORY (:ABSOLUTE)
                      :NAME "METERING" :TYPE "SYSTEM" :VERSION :NEWEST)
  #-CLISP
  FIXME)

(check-for-bug :path-added-10
  (merge-pathnames (logical-pathname "cl-systems:") #P"metering.system")
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CL-SYSTEMS" :DEVICE NIL :DIRECTORY (:ABSOLUTE)
                      :NAME "METERING" :TYPE "SYSTEM" :VERSION :NEWEST)
  #-CLISP
  FIXME)

(check-for-bug :path-added-11
  (merge-pathnames (logical-pathname "clocc:clocc.lisp"))
  #+CLISP
  #S(logical-pathname :host "CLOCC" :device nil :directory (:absolute)
                      :name "CLOCC" :type "LISP" :version :newest)
  #-CLISP
  FIXME)

(check-for-bug :path-added-12
  (merge-pathnames ".fas" (logical-pathname "clocc:src;cllib;xml.lisp"))
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL :DIRECTORY
                      (:ABSOLUTE "SRC" "CLLIB") :NAME "XML" :TYPE "FAS" :VERSION :NEWEST)
  #-CLISP
  FIXME)

(check-for-bug :path-added-13
  (merge-pathnames (logical-pathname "clocc:;foo;bar;")
                   (logical-pathname "clocc:baz;quux.lisp.3"))
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL :DIRECTORY
                      (:ABSOLUTE "BAZ" "FOO" "BAR") :NAME "QUUX" :TYPE "LISP" :VERSION 3)
  #-CLISP
  FIXME)

(check-for-bug :path-added-14
  (compile-file-pathname (logical-pathname "clocc:clocc.lisp"))
  #+CLISP
  #S(logical-pathname :host "CLOCC" :device nil :directory (:absolute)
                      :name "CLOCC" :type "FAS" :version :newest)
  #-CLISP
  FIXME)

(check-for-bug :path-added-15
  (compile-file-pathname (logical-pathname "clocc:src;cllib;xml.lisp"))
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL :DIRECTORY
                      (:ABSOLUTE "SRC" "CLLIB") :NAME "XML" :TYPE "FAS" :VERSION :NEWEST)
  #-CLISP
  FIXME)

(check-for-bug :path-added-16
  (parse-namestring "foo;bar;baz.fas.3" "clocc")
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL
                      :DIRECTORY (:ABSOLUTE "FOO" "BAR") :NAME "BAZ" :TYPE "FAS" :VERSION 3)
  #-CLISP
  FIXME)

(check-for-bug :path-added-17
  (parse-namestring "foo;bar;baz.fas.3" nil (logical-pathname "clocc:"))
  #+CLISP
  #S(LOGICAL-PATHNAME :HOST "CLOCC" :DEVICE NIL
                      :DIRECTORY (:ABSOLUTE "FOO" "BAR") :NAME "BAZ" :TYPE "FAS" :VERSION 3)
  #-CLISP
  FIXME)

;; Relative
(check-for-bug :path-added-18
  (translate-logical-pathname
   (merge-pathnames (logical-pathname "TEST-SUBDIR:;FOO;BAR;")
                    (logical-pathname "TEST-SIMPLE:ZOT.LISP")))
  #p"/usr/local/share/r/foo/bar/zot.lisp")

;; Absolute
(check-for-bug :path-added-19
  (translate-logical-pathname
 (merge-pathnames (logical-pathname "TEST-SUBDIR:FOO;BAR;")
                  (logical-pathname "TEST-SIMPLE:ZOT.LISP")))
  #p"/usr/local/share/foo/bar/zot.lisp")

(check-for-bug :path-added-20
  (make-pathname :defaults "a.b" :name "c" :type nil)
  #p"c")

#+CLISP
(check-for-bug :path-added-21
  (make-pathname :defaults #S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL
                                               :DIRECTORY (:ABSOLUTE "FOO")
                                               :NAME "BAR" :TYPE "BAZ" :VERSION 3))
  #S(LOGICAL-PATHNAME :HOST "CL-LIBRARY" :DEVICE NIL :DIRECTORY (:ABSOLUTE "FOO")
                      :NAME "BAR" :TYPE "BAZ" :VERSION 3))

(check-for-bug :path-added-22
  (defun foo (x host)
    (let ((dflt (make-pathname :directory '(:relative :wild-inferiors)
                               :type x :case :common)))
      (if host
          (make-pathname :defaults dflt :host host :case :common)
          (make-pathname :defaults dflt :case :common))))
  foo)

;; :defaults arg is not subject to :case conversion
(check-for-bug :path-added-23
  (string= "c" (pathname-type (foo "c" nil) :case :common))
  t)

(check-for-bug :path-added-24
  (string= "C" (pathname-type (foo "C" nil) :case :common))
  t)

;; :case is ignored for logical pathnames
(check-for-bug :path-added-25
  (string= "C" (pathname-type (foo "c" "CLOCC") :case :common))
  t)

(check-for-bug :path-added-26
  (string= "c" (pathname-type (foo "C" "CLOCC") :case :common))
  t)

(check-for-bug :path-added-27
  (namestring (logical-pathname "foo:bar;baz"))
  "FOO:BAR;BAZ")

(check-for-bug :path-added-28
  (let* ((foo (copy-seq "abcdefghijkl"))
       (bar (make-array 5 :displaced-to foo :displaced-index-offset 2
                        :element-type 'character))
       (path (make-pathname :directory bar)))
  (setf (aref foo 3) #\/)
  (equalp path (make-pathname :directory (pathname-directory path))))
  t)

(check-for-bug :path-added-29
  (string= (namestring (make-pathname :name "FOO" :case :common
                                      :defaults #P"/home/kent/"))
           (namestring #P"/home/kent/foo"))
  t)
