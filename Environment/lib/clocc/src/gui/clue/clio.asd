;;; -*- Mode: lisp -*-

(in-package :asdf)

#+cmucl
(defsystem :clio
    :depends-on ( :clue )
    :components
    ((:file "clio")
     (:file "defgeneric" :depends-on ("clio"))
     (:file "ol-defs" :depends-on ("defgeneric"))
     (:file "utility" :depends-on ("ol-defs"))
     (:file "core-mixins" :depends-on ("utility"))
     (:file "gravity" :depends-on ("core-mixins"))
     (:file "ol-images" :depends-on ("gravity"))
     (:file "buttons" :depends-on ("ol-images"))
     (:file "form" :depends-on ("buttons"))
     (:file "table" :depends-on ("form"))
     (:file "choices" :depends-on ("table"))
     (:file "scroller" :depends-on ("choices"))
     (:file "slider" :depends-on ("scroller"))
     (:file "scroll-frame" :depends-on ("slider"))
     (:file "mchoices" :depends-on ("scroll-frame"))
     (:file "menu" :depends-on ("mchoices"))
     (:file "psheet" :depends-on ("menu"))
     (:file "command" :depends-on ("psheet"))
     (:file "confirm" :depends-on ("command"))
     (:file "buffer" :depends-on ("confirm"))
     (:file "text-command" :depends-on ("buffer"))
     (:file "display-text" :depends-on ("text-command"))
     (:file "edit-text" :depends-on ("display-text"))
     (:file "display-imag" :depends-on ("edit-text"))
     (:file "dlog-button" :depends-on ("display-imag"))))
