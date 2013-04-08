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



(unless (find-package "PICTURES")
  (make-package "PICTURES"
		:use '(lisp xlib clos clue)
		:nicknames '("PIC"))
  )


#+Lispworks
(unless (find-package "PICTURES")
  (make-package "PICTURES"
		:use '(lisp xlib clos clue)
		:nicknames '("PIC"))
  )


#+Lispworks
(defsystem::defsystem user::pictures
    (:default-pathname "/home/pictures/source/"
     :default-type :lisp-file
     :package pictures)
    :members (
	      "package"
              "defgeneric"
	      ;; "caches"
	      "types" "pictures-macros" "class-definitions" 
	      "edge" 
	      "font-family"
	      "sequence"  "transform"
	      "extent"   
	      "graphics-state"
              "graphic-stack" 
	      "view-draw"
              "graphic"
	      "scene"
	      "line"
	      "circle"
	      "polypoint"
	      "polygon"
              "rectangle" "bspline" "ellipse"
              "label"
              ;; "graphical-image"
              "graphic-events"
              "grabber-rect"
	       "view"  "view-events" "view-selection"
              "view-zoom"  "view-pan"
	      )
    :rules ((:in-order-to :compile :all
			  (:requires (:load :serial)))))

#+Explorer
(in-package "USER")

#+Explorer
(defsystem pictures
  (:name "Common Lisp 2-D Graphics Library")
  (:short-name "Pictures")
  (:nicknames "pic")
  (:pathname-default "pictures:source;")
  (:patchable "pictures:patch;")
  (:initial-status :experimental)

  ;;  The real source files...
  (:module package             ("package" ))
  (:module caches              ( "edge" "types" "pictures-macros"))
  (:module class-definitions   ( "class-definitions" "font-family" ))
  (:module base                ("sequence"  "graphics-state" "transform"))
  (:module stack               ("graphic-stack"))
  (:module graphic             ("graphic"))
  (:module extent              ("extent"))
  (:module scene               ("scene"))
  (:module view	               ("view" ))
  (:module view-methods        ("view-selection" "view-pan" "view-draw" "view-events"))
  (:module line                ("line"))
  (:module circle              ("circle"))
  (:module polypoint           ("polypoint"))
  (:module polygon             ("polygon"))
  (:module rectangle           ("rectangle" "bspline" "ellipse"))
  (:module label               ("label"))
  (:module graphical-image     ("graphical-image"))
  (:module graphic-events      ("graphic-events"))
  (:module grabber-rect        ("grabber-rect"))
  (:module view-zoom           ("view-zoom"))
  (:module save                ("utilities" "save" "restore"))
  
  ;;  The transformations...
  (:compile-load package)
  (:compile-load caches            (:fasload class-definitions))
  (:compile-load class-definitions (:fasload package  ))
  (:compile-load base              (:fasload caches))
  (:compile-load stack             (:fasload   base))
  (:compile-load extent            (:fasload   stack ))
  (:compile-load graphic           (:fasload   extent))
  (:compile-load scene             (:fasload   graphic))
  (:compile-load line              (:fasload   graphic))
  (:compile-load circle            (:fasload   graphic ))
  (:compile-load polypoint         (:fasload   graphic ))
  (:compile-load polygon           (:fasload   polypoint))
  (:compile-load rectangle         (:fasload   polygon))
  (:compile-load label             (:fasload   graphic ))
  (:compile-load graphical-image   (:fasload   graphic ))
  (:compile-load graphic-events    (:fasload   graphic ))
  (:compile-load grabber-rect      (:fasload   rectangle))
  (:compile-load view              (:fasload   extent grabber-rect))
  (:compile-load view-methods      (:fasload   view))
  (:compile-load view-zoom         (:fasload   view-methods))
  (:compile-load save              (:fasload   caches))
  )
	    














