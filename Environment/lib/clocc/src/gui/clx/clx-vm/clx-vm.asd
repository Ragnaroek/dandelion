;;; -*- Mode: Lisp; Package: ASDF; Base: 10; Syntax: Common-Lisp -*-

(in-package :asdf)

(defsystem :clx-vm
    :components
    ((:file "package")
     (:file "threading" :depends-on ("package"))))
