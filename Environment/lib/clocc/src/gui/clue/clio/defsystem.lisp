;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                                AUSTIN, TEXAS 78714                               |
;;;                                                                                  |
;;;             Copyright (C) 1989, 1990 Texas Instruments Incorporated.             |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+


(in-package "USER")

#+explorer
(defsystem clio
  (:name "Common Lisp Interactive Objects")
  (:short-name "CLIO")
  (:pathname-default "clio:source;")
  (:patchable "clio:patch;" "CLIO")
  (:initial-status :experimental)

  ;;  The real source files...
  (:module clio             ("clio"))
  (:module defs             ("ol-defs" "utility"))
  (:module core             ("core-mixins" "gravity"))
  (:module images           "ol-images")
  (:module buttons          "buttons")
  (:module form             "form")
  (:module table            "table")
  (:module choices          "choices")
  (:module scroller         "scroller")
  (:module slider           "slider")
  (:module scroll-frame     "scroll-frame")
  (:module multiple-choices "mchoices")
  (:module menu             "menu")
  (:module property-sheet   "psheet")
  (:module command          "command")
  (:module confirm          "confirm")
  (:module text-defs         ("buffer" "text-command")) 
  (:module display-text     "display-text")
  (:module edit-text        "edit-text")
  (:module display-image    "display-imag")
  (:module dialog-button    "dlog-button")

  ;;  The auxiliary files...
  ;;(:module doc ("readme" "doc;clio.ps" "doc;release.1-0"))
  ;;(:auxiliary doc)

  ;;  The transformations...
  (:compile-load clio)
  
  (:compile-load defs
		 (:fasload clio)
		 (:fasload clio))
  (:compile-load core
		 (:fasload clio defs)
		 (:fasload clio defs))
  (:compile-load images
		 (:fasload clio defs)
		 (:fasload clio defs))
  (:compile-load text-defs
		 (:fasload clio)
		 (:fasload clio))
  (:compile-load display-text
		 (:fasload clio core text-defs)
		 (:fasload clio core text-defs))
  (:compile-load confirm
		 (:fasload clio core display-text)
		 (:fasload clio core display-text))
  (:compile-load edit-text
		 (:fasload clio core text-defs display-text confirm images)
		 (:fasload clio core text-defs display-text confirm images))
  (:compile-load buttons
		 (:fasload clio core display-text images)
		 (:fasload clio core display-text images)) 
  (:compile-load scroller
		 (:fasload clio core defs images)
		 (:fasload clio core defs images)) 
  (:compile-load scroll-frame
		 (:fasload clio core scroller)
		 (:fasload clio core scroller))
  (:compile-load slider
		 (:fasload clio core defs images)
		 (:fasload clio core defs images))
  (:compile-load form
		 (:fasload clio core)
		 (:fasload clio core)) 
  (:compile-load table
		 (:fasload clio core)
		 (:fasload clio core)) 
  (:compile-load choices
		 (:fasload clio core table)
		 (:fasload clio core table)) 
  (:compile-load multiple-choices
		 (:fasload clio core table)
		 (:fasload clio core table)) 
  (:compile-load menu
		 (:fasload clio core display-text choices buttons defs images)
		 (:fasload clio core display-text choices buttons defs images)) 
  (:compile-load property-sheet
		 (:fasload clio core form menu confirm display-text)
		 (:fasload clio core form menu confirm display-text))
  (:compile-load command
		 (:fasload clio core form table confirm display-text)
		 (:fasload clio core form table confirm display-text))
  (:compile-load dialog-button
		 (:fasload clio core confirm menu property-sheet command)
		 (:fasload clio core confirm menu property-sheet command))
  (:compile-load display-image
		 (:fasload clio core)
		 (:fasload clio core))

  )



(defun load-clio (&key (host "CLIO") (directory "SOURCE") (compile-p t) (verbose-p t))
  (dolist (file (mapcar
		  #'(lambda (name)
		      (make-pathname
			:host      host
			:directory directory
			:name      name
			:version   :newest))
		  '("CLIO"
		    "OL-DEFS"
		    "UTILITY"
		    "OL-IMAGES"
		    "CORE-MIXINS"
		    "GRAVITY"
		    "BUFFER"
		    "TEXT-COMMAND"
		    "DISPLAY-TEXT"
		    "BUTTONS"
		    "CONFIRM"
		    "SCROLLER"
		    "TABLE"
		    "CHOICES"
		    "FORM"
		    "MENU"
		    "PSHEET"
		    "COMMAND"
		    "EDIT-TEXT"
		    "SCROLL-FRAME"
		    "SLIDER"
		    "MCHOICES"
		    "DLOG-BUTTON"
		    "DISPLAY-IMAG"
		    )))
    (when compile-p
      (when verbose-p
	(format t "~% Compiling ~12t~a..." file))
      (compile-file file))
    
    (when verbose-p
      (format t "~% Loading ~12t~a..." file))
    (load file)
    
    (when (and compile-p verbose-p)
      (format t "~%"))))