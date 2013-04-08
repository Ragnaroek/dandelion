(in-package :user)

(defparameter *here* (make-pathname
		      :directory (pathname-directory *load-truename*)))

(pushnew 'compile pcl::*defclass-times*)
(pushnew 'compile pcl::*defgeneric-times*)

(defvar *clx-directory*  "target:clx/*")
(defvar *clue-directory* (merge-pathnames "clue/" *here*))
(setf (search-list "clue:") (list *here*))

;; Don't warn about botched values decls
(setf c:*suppress-values-declaration* t)

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

(defsystem 
 clx
 (:default-pathname "target:clx/")
 "macros"
 "bufmac")

(defvar *clue-precom* "clue:clue/precom")
(defvar *pict-precom* "clue:pictures/precom")

(when (probe-file "clue:patch.lisp")
  (load "clue:patch" :if-source-newer :compile))

(defsystem
 clue
    (:default-pathname "clue:clue/"
	:needed-systems (clx) )
  
  #+nil"common-lisp"
  "clue"				; Define packages
  (:module clue-precom-load
	   :load-form (hack-precom "clue:clue/" :load)
	   :compile-form t)
  #+nil"clx-patch"			; Modify xlib:create-window
  #+nil"window-doc"			; pointer documentation window support
  "defgeneric"				; pw adds 
  "event-parse"				; Utilities for event translation
  "defcontact"				; CLOS for resources and type conversion
  "intrinsics"				; The "guts"
  "caches"				; Support for gcontext, pixmap, cursor cacheing
  "resource"				; Resource and type conversion
  "gray"				; Gray stipple patterns
  "cursor"				; Standard cursor names
  "events"				; Event handling
  "virtual"				; Support for windowless contacts
  "shells"				; Support for top-level window/session mgr.
  "root-gmgmt"				; Geometry management methods for root contacts
  #+nil"stream"				; interactive-stream (non-portable!!)
  "package"				; External cluei symbols exported from clue
  (:module clue-precom-com
	   :load-form t
	   :compile-only t
	   :compile-form (hack-precom "clue:clue/" :compile))
     )

(defsystem
    clio
    (:needed-systems ( clue )
     :default-pathname "clue:clio/"
     :components
     "clio"
     "defgeneric"
     (:module clio-precom-load
	      :load-form (hack-precom "clue:clio/" :load)
	      :compile-form t)
     "ol-defs"
     "utility"
     "core-mixins"
     "gravity"
     "ol-images"
     "buttons"
     "form"
     "table"
     "choices"
     "scroller"
     "slider"
     "scroll-frame"
     "mchoices"
     "menu"
     
     "psheet"
     "command"
     "confirm"
     "buffer"
     "text-command"
     "display-text"
     "edit-text"
     "display-imag"
     "dlog-button"
     (:module clio-precom-com
	      :load-form t
	      :compile-only t
	      :compile-form (hack-precom "clue:clio/" :compile))  
     ))

(defsystem
    clio-examples
    (:default-pathname "clue:clio/examples/")
  "package" "cmd-frame" "sketchpad" "sketch")

(defsystem
    pictures
    ( :default-pathname "clue:pictures/"
	:needed-systems (clue ))
  "package"
  (:module pict-precom-load
	   :load-form (hack-precom "clue:pictures/" :load)
	   :compile-form t)
  "defgeneric"
  "types"
  "macros"
  "sequence"
  "transform"
  "extent"
  

  "edge" 
  "class-def" :needed-systems ("edge" "extent")
  
  ;; these have circular dependencies
  "gstate" :needed-systems ("class-def")
  "gstack"
  "graphic"

  "font-family"
  "view-draw"
  "scene"
  "line"
  "circle"
  "polypoint"
  "polygon"
  "rectangle"
  "bspline"
  "ellipse"
  "label"
  "gimage"
  "gevents"
  "grabber"
  "view"
  "view-events"
  "view-select"
  "view-zoom"
  "view-pan"
  "utilities"
  "save"
  "restore"
  (:module pict-precom-com
	   :load-form t
	   :compile-only t
	   :compile-form (hack-precom "clue:pictures/" :compile))

  )



(defun load-clue()
  (load-system 'clue)
  (purify))

(defun compile-it (thing)
  (with-compilation-unit
   (:optimize
    '(optimize
      (ext:inhibit-warnings 3)
      (debug #-(or high-security small) 2 
       #+(and small (not high-security)) .5
       #+high-security 3) 
     ; (speed 2) (inhibit-warnings 3)
      (safety #-(or high-security small) 1 
       #+(and small (not high-security)) 0
       #+high-security 3)
      )
    :optimize-interface
    '(optimize-interface (debug .5))
    :context-declarations
    '(((:and :external :global)
       (declare (optimize-interface (safety 2) (debug 1))))
      ((:and :external :macro)
       (declare (optimize (safety 2))))
      (:macro (declare (optimize (speed 0))))))
   (compile-system thing)))


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
   (compile-system 'graphics)))

(unintern 'name)
