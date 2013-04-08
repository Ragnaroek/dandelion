(in-package :user)
(eval-when (load eval)
(unless (find-package :mk)
  (load "library:defsystem"))
)

(defparameter *here* (make-pathname
		      :directory (pathname-directory *load-truename*)))

#+pcl
(progn
  (pushnew 'compile pcl::*defclass-times*)
  (pushnew 'compile pcl::*defgeneric-times*))

(defvar *clx-directory*  "target:clx/*")
(defvar *clue-directory* (merge-pathnames "clue/" *here*))
#+cmu
(setf (search-list "clue:") (list *here*))

;; Ensure VALUES is a legal declaration
#-cmu17
(proclaim '(declaration values))

#+cmu17 ;; Don't warn about botched values decls
(setf c:*suppress-values-declaration* t)

#+pcl ;; This is now in current sources
(pushnew 'values pcl::*non-variable-declarations*)

;; Ensure *features* knows about CLOS and PCL
(when (find-package 'pcl)
  (pushnew :pcl  *features*)
  (pushnew :clos *features*)
  (unless (find-package :clos)
    (rename-package :pcl :pcl '(:clos))))

(when (find-package 'clos)
  (pushnew :clos *features*))


;; Ensure *features* knows about the Common Lisp Error Handler
(when (find-package 'conditions)
  (pushnew :cleh *features*))

;;; handle PCL's precompile "feature"
(defun hack-precom (path op &optional (pathname "precom"))
  (declare (type (member :compile :load :ignore) op))
  (let* ((src (merge-pathnames (make-pathname :name pathname :type "lisp")
			       path))
	 (obj (compile-file-pathname src)))
    ;;(format t "~& ~a ~a ~a ~a~%" path op pathname src)
    (case op
      (:compile
       (when (probe-file src)
	 (unless (probe-file obj)
	   (compile-file src))))
      (:load
       (when (probe-file obj)
	 (load obj))))))

(mk:defsystem 
 clx
 :source-pathname "target:clx/"
 :components ((:file "macros")(:file "bufmac")))

(defvar *clue-precom* "clue:clue/precom")
(defvar *pict-precom* "clue:pictures/precom")

(when (probe-file "clue:patch.lisp")
  (load "clue:patch" :if-source-newer :compile))

(mk:defsystem
 clue
 :source-pathname "clue:clue/"
 :depends-on (clx)
 :components
 (
  #+nil(:file "common-lisp")
  (:file "clue")		; Define packages
  (:module clue-precom-load
	   :load-form (hack-precom "clue:clue/" :load)
	   :compile-form t)
  #+nil(:file "clx-patch")	; Modify xlib:create-window
  #+nil(:file "window-doc")	; pointer documentation window support
  (:file "defgeneric")		; pw adds 
  (:file "event-parse")		; Utilities for event translation
  (:file "defcontact")		; CLOS for resources and type conversion
  (:file "intrinsics")		; The "guts"
  (:file "caches")		; Support for gcontext, pixmap, cursor cacheing
  (:file "resource")		; Resource and type conversion
  (:file "gray")		; Gray stipple patterns
  (:file "cursor")		; Standard cursor names
  (:file "events")		; Event handling
  (:file "virtual")		; Support for windowless contacts
  (:file "shells")		; Support for top-level window/session mgr.
  (:file "root-gmgmt")		; Geometry management methods for root contacts
  #+nil(:file "stream")		; interactive-stream (non-portable!!)
  (:file "package")		; External cluei symbols exported from clue
  (:module clue-precom-com
	   :load-form t
	   :compile-only t
	   :compile-form (hack-precom "clue:clue/" :compile))
  ))

(mk:defsystem
 clio
 :depends-on ( clue )
 :source-pathname "clue:clio/"
 :components
 ((:file "clio")
  (:file "defgeneric")
  (:module clio-precom-load
	   :load-form (hack-precom "clue:clio/" :load)
	   :compile-form t)
  (:file "ol-defs")
  (:file "utility")
  (:file "core-mixins")
  (:file "gravity")
  (:file "ol-images")
  (:file "buttons")
  (:file "form")
  (:file "table")
  (:file "choices")
  (:file "scroller")
  (:file "slider")
  (:file "scroll-frame")
  (:file "mchoices")
  (:file "menu")
  
  (:file "psheet")
  (:file "command")
  (:file "confirm")
  (:file "buffer")
  (:file "text-command")
  (:file "display-text")
  (:file "edit-text")
  (:file "display-imag")
  (:file "dlog-button")
  (:module clio-precom-com
	   :load-form t
	   :compile-only t
	   :compile-form (hack-precom "clue:clio/" :compile))  
  ))

(mk:defsystem
 clio-examples
 :source-pathname "clue:clio/examples/"
 :components ("package" "cmd-frame" "sketchpad" "sketch"))

(mk:defsystem
 pictures
 :source-pathname "clue:pictures/"
 :depends-on (clue )
 :components
 ((:file "package")
  (:module pict-precom-load
	   :load-form (hack-precom "clue:pictures/" :load)
	   :compile-form t)
  (:file "defgeneric")
  (:file "types")
  (:file "macros")
  (:file "sequence")
  (:file "transform")
  (:file "extent")


  (:file "edge" )
  (:file "class-def" :depends-on ("edge" "extent"))

  ;; these have circular dependencies
  (:file "gstate" :depends-on ("class-def"))
  (:file "gstack")
  (:file "graphic")

  (:file "font-family")
  (:file "view-draw")
  (:file "scene")
  (:file "line")
  (:file "circle")
  (:file "polypoint")
  (:file "polygon")
  (:file "rectangle")
  (:file "bspline")
  (:file "ellipse")
  (:file "label")
  (:file "gimage")
  (:file "gevents")
  (:file "grabber")
  (:file "view")
  (:file "view-events")
  (:file "view-select")
  (:file "view-zoom")
  (:file "view-pan")
  (:file "utilities")
  (:file "save")
  (:file "restore")
  (:module pict-precom-com
	   :load-form t
	   :compile-only t
	   :compile-form (hack-precom "clue:pictures/" :compile))

  ))



(defun load-clue()
  (mk:oos 'clue :load)
  (purify))

(defun compile-it (thing)
  (with-compilation-unit
   (:optimize
    '(optimize
      (ext:inhibit-warnings 3)
      (debug #-small 2 #+small .5) 
     ; (speed 2) (inhibit-warnings 3)
      (safety #-small 1 #+small 0)
      )
    :optimize-interface
    '(optimize-interface (debug .5))
    :context-declarations
    '(((:and :external :global)
       (declare (optimize-interface (safety 2) (debug 1))))
      ((:and :external :macro)
       (declare (optimize (safety 2))))
      (:macro (declare (optimize (speed 0))))))
   (mk:oos thing :compile)))


(defun compile-all()
  
  (with-compilation-unit
   (:optimize
    '(optimize
      ;;(ext:inhibit-warnings 3)
      (debug #-small 2 #+small .5) 
      (speed 2) (inhibit-warnings 3)
      (safety #-small 1 #+small 1)
      )
    :optimize-interface
    '(optimize-interface (debug .5))
    :context-declarations
    '(((:and :external :global)
       (declare (optimize-interface (safety 2) (debug 1))))
      ((:and :external :macro)
       (declare (optimize (safety 2))))
      (:macro (declare (optimize (speed 0))))))
   (mk:oos 'graphics :compile)))

(unintern 'name)
