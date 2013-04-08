;;; -*- Mode: lisp -*-

(in-package :asdf)

#+cmucl
(defsystem :pictures
    :depends-on ( :clue )
    :components
    ((:file "package")
     (:file "defgeneric" :depends-on ("package"))
     (:file "types" :depends-on ("defgeneric"))
     (:file "macros" :depends-on ("types"))
     (:file "sequence" :depends-on ("macros"))
     (:file "transform" :depends-on ("sequence"))
     (:file "extent" :depends-on ("transform"))
     (:file "edge"  :depends-on ("extent"))
     (:file "class-def" :depends-on ("edge" "extent"))
     ;; these have circular dependencies
     (:file "gstate" :depends-on ("class-def"))
     (:file "gstack" :depends-on ("gstate"))
     (:file "graphic" :depends-on ("gstack"))
     (:file "font-family" :depends-on ("graphic"))
     (:file "view-draw" :depends-on ("font-family"))
     (:file "scene" :depends-on ("view-draw"))
     (:file "line" :depends-on ("scene"))
     (:file "circle" :depends-on ("line"))
     (:file "polypoint" :depends-on ("circle"))
     (:file "polygon" :depends-on ("polypoint"))
     (:file "rectangle" :depends-on ("polygon"))
     (:file "bspline" :depends-on ("rectangle"))
     (:file "ellipse" :depends-on ("bspline"))
     (:file "label" :depends-on ("ellipse"))
     (:file "gimage" :depends-on ("label"))
     (:file "gevents" :depends-on ("gimage"))
     (:file "grabber" :depends-on ("gevents"))
     (:file "view" :depends-on ("grabber"))
     (:file "view-events" :depends-on ("view"))
     (:file "view-select" :depends-on ("view-events"))
     (:file "view-zoom" :depends-on ("view-select"))
     (:file "view-pan" :depends-on ("view-zoom"))
     (:file "utilities" :depends-on ("view-pan"))
     (:file "save" :depends-on ("utilities"))
     (:file "restore" :depends-on ("save"))))

