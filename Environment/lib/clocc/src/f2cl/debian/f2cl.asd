;;; -*- Mode: lisp -*-

(in-package :asdf)

(defsystem :f2cl
    :components
  ((:file "f2cl0")
   (:file "f2cl1" :depends-on ("f2cl0"))
   (:file "f2cl2" :depends-on ("f2cl1"))
   (:file "f2cl3" :depends-on ("f2cl2"))
   (:file "f2cl4" :depends-on ("f2cl3"))
   (:file "f2cl5" :depends-on ("f2cl4"))
   (:file "f2cl6" :depends-on ("f2cl5"))
   (:file "f2cl7" :depends-on ("f2cl6"))
   (:file "f2cl8" :depends-on ("f2cl7"))
   (:file "macros" :depends-on ("f2cl8"))))


(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :f2cl))))
  "l")
