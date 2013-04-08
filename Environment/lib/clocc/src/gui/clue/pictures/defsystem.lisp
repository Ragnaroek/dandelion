;;;-*- Mode:Common-Lisp; Package:user; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "USER")

#-lispm
(progn						; SITE DEPENDENT
  ;; NOTE: All pathname strings must end in /*
  (defvar *pictures-directory*  "/arc/lw/pictures/")
  (defvar *pictures-source*   "/arc/lw/pictures/source/")
  )

#+lispm
(progn						; Lispm's have logical pathnames
  (defvar *pictures-directory*  "pictures:pictures;")
  )

#+comment
;; Here's a recommended set of LISPM logical pathname translations:
(net::set-logical-pathname-host "PICTURES" :physical-host si:local-host
				:translations 
				'(("SOURCE"   "PICTURES.SOURCE;")
				  ("PATCH"    "PICTURES.PATCH;")
				  ("DOC"      "PICTURES.DOC;")
				  ("EXAMPLES" "PICTURES.EXAMPLES;"))
				:if-exists t)

;; Ensure VALUES is a legal declaration
(proclaim '(declaration values))

#+Lispworks
(defsystem::defsystem user::pictures
    (:default-pathname *pictures-source*
     :default-type :lisp-file)
    :members (
	      "package"
              "defgeneric"
	      "types"
	      "macros"
	      "class-def" 
	      "edge" 
	      "font-family"
	      "sequence"
	      "transform"
	      "extent"   
	      "gstate"
              "gstack" 
	      "view-draw"
              "graphic"
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
	      )
    :rules ((:in-order-to :compile :all
			  (:requires (:load :serial)))))




#+Explorer
(defsystem pictures
  (:name "Common Lisp 2-D Graphics Library")
  (:short-name "Pictures")
  (:nicknames "pic")
  (:pathname-default "pictures:source;")
  (:patchable "pictures:patch;")
  (:initial-status :released)

  ;;  The real source files...
  (:module package             ("package"))
  (:module types               ("types"))
  (:module caches              ("defgeneric" "edge"  "macros"))
  (:module class-definitions   ("class-def" "font-family"))
  (:module base                ("sequence"  "gstate" "transform"))
  (:module stack               ("gstack"))
  (:module graphic             ("graphic"))
  (:module extent              ("extent"))
  (:module scene               ("scene"))
  (:module view	               ("view"))
  (:module view-methods        ("view-select" "view-pan" "view-draw" "view-events"))
  (:module line                ("line"))
  (:module circle              ("circle"))
  (:module polypoint           ("polypoint"))
  (:module polygon             ("polygon"))
  (:module rectangle           ("rectangle" "bspline" "ellipse"))
  (:module label               ("label"))
  (:module graphical-image     ("gimage"))
  (:module graphic-events      ("gevents"))
  (:module grabber-rect        ("grabber"))
  (:module view-zoom           ("view-zoom"))
  (:module save                ("utilities" "save" "restore"))
  
  ;;  The transformations...
  (:compile-load package)
  (:compile-load types             (:fasload package))
  (:compile-load class-definitions (:fasload types))
  (:compile-load caches            (:fasload class-definitions))
  (:compile-load base              (:fasload caches))
  (:compile-load stack             (:fasload base))
  (:compile-load extent            (:fasload stack))
  (:compile-load graphic           (:fasload extent))
  (:compile-load scene             (:fasload graphic))
  (:compile-load line              (:fasload graphic))
  (:compile-load circle            (:fasload graphic))
  (:compile-load polypoint         (:fasload graphic))
  (:compile-load polygon           (:fasload polypoint))
  (:compile-load rectangle         (:fasload polygon))
  (:compile-load label             (:fasload graphic))
  (:compile-load graphical-image   (:fasload graphic))
  (:compile-load graphic-events    (:fasload graphic))
  (:compile-load grabber-rect      (:fasload rectangle))
  (:compile-load view              (:fasload extent grabber-rect))
  (:compile-load view-methods      (:fasload view))
  (:compile-load view-zoom         (:fasload view-methods))
  (:compile-load save              (:fasload caches))
  )
	    

;;;-----------------------------------------------------------------------------
;;; Compile/Load pictures

(defun load-pictures (&key (host #+lispm "PICTURES" #-lispm "") 
		           (directory #+lispm "SOURCE" #-lispm *pictures-source*) 
			   (compile-p t) (verbose-p t))
  (unless (find-package "CLUE") (error "CLUE is not loaded"))
  (unless (find-package "COMMON-LISP") (error "COMMON-LISP is not available"))
  (dolist (file (mapcar
		  #'(lambda (name)
		      (make-pathname
			:host      host
			:directory directory
			:name      name
			:version   :newest))
		  '("package"          ;; define the pictures package 
		    "defgeneric"       ;; define the defgenric for clos methods
		    "types"            ;; create special type definitions
		    "macros"
		    "class-def"        ;; class definitions for view and graphic class
		    "edge"             ;; mixin for fill edges
		    "font-family"      ;; class for creating special font families
		    "sequence"         ;; routines for handling x,y pair in a list or one array
		    "transform"
		    "extent"           ;; extent caching routines
		    "gstate"           ;; graphic state class
		    "gstack"           ;; graphic and gstate caching routines
		    "view-draw"        ;; primitive draw routines for the view class
		    "graphic"
		    "scene"            ;; display list class for graphics
		    "line"
		    "circle"
		    "polypoint"
		    "polygon"
		    "rectangle"
		    "bspline"
		    "ellipse"
		    "label"
		    "gimage"           ;; grpahic image class
		    "gevents"          ;; definitions for graphic events
		    "grabber"          ;; routines for creating the selection rectangle
		    "view"             ;; generic methods for the view class
		    "view-events"      ;; event handling for the view
		    "view-select"      ;; routines for selecting graphics in a view
		    "view-zoom"        ;; routines for zooming 
		    "view-pan"         ;; routines for panning
		    "utilities"
		    "save"
		    "restore"
		    )))
    (when compile-p
      (when verbose-p
	(format t "~% Compiling ~12t~a..." file))
      (compile-file file))
    
    (when verbose-p
      (format t "~% Loading ~12t~a..." file))
    (load file :verbose nil)
    
    (when (and compile-p verbose-p)
      (format t "~%"))))










