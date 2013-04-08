;;; -*- Mode: Lisp; Package: ASDF; Base: 10; Syntax: Common-Lisp -*-

(in-package :asdf)

(pushnew :clx-debugging *features*)

(defsystem :clx
    :depends-on (:port :split-sequence)
    :components
    ((:file "package")
     (:file "depdefs" :depends-on ("package"))
     (:file "clx" :depends-on ("depdefs"))
     (:file "dependent" :depends-on ("clx"))
     (:file "macros" :depends-on ("dependent"))				; these are just macros
     (:file "bufmac" :depends-on ("macros"))				; these are just macros
     (:file "buffer" :depends-on ("bufmac"))
     (:file "display" :depends-on ("buffer"))
     (:file "gcontext" :depends-on ("display"))
     (:file "input" :depends-on ("gcontext"))
     (:file "requests" :depends-on ("input"))
     (:file "fonts" :depends-on ("requests"))
     (:file "graphics" :depends-on ("fonts"))
     (:file "text" :depends-on ("graphics"))
     (:file "attributes" :depends-on ("text"))
     (:file "translate" :depends-on ("attributes"))
     (:file "keysyms" :depends-on ("translate"))
     (:file "manager" :depends-on ("keysyms"))
     (:file "image" :depends-on ("manager"))
     (:file "resource" :depends-on ("image"))))
