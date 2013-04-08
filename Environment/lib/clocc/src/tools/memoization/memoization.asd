;;; -*- Mode: lisp -*-
(in-package :asdf)

(defsystem :memoization
    :components ((:file "package")
 		  (:file "memoization" :depends-on ("package"))
 		  (:file "save-memo-table" :depends-on ("memoization"))
 		  (:file "memoization-examples" :depends-on ("save-memo-table"))))

