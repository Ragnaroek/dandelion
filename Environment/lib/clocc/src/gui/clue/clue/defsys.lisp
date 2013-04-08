;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
;;;                                                                                  |
;;;                Copyright (C)1989,1990 Texas Instruments Incorporated.            |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

;;; PCL style defsystem definitions.  Use pcl::operate-on-system
;;; This code contributed by Martin Dragomirecky <steinmetz!dragomir@uunet.UU.NET>

(in-package "USER")

#-lispm
(progn						; SITE DEPENDENT
  (defvar *clx-directory*  "/usr/X11/lib/CLX/*")
  (defvar *clos-kludge-directory*)
  (defvar *clue-directory* nil)
  (defvar *clue-examples-directory* nil)
  (defvar *clue-demo-directory* nil)
  )
#+lispm
(progn						; Lispm's have logical pathnames
  (defvar *clx-directory*  "clx:clx;")
  (defvar *clue-directory* "clue:clue;")
  (defvar *clos-kludge-directory*  "clue:clos-kludge;")
  (defvar *clue-examples-directory* "clue:examples;")
  (defvar *clue-demo-directory* "clue:demo;")
  )

#+comment
;; Here's a recommended set of LISPM logical pathname translations:
(fs:set-logical-pathname-host "CLUE" :physical-host si:local-host
			      :translations 
			      '(("CLUE" "CLUE;")
				("EXAMPLES" "CLUE.EXAMPLES;")
				("DEMO" "CLUE.DEMO;")
				("DOC" "CLUE.DOC;")
				("PATCH" "CLUE.PATCH;")
				("CLOS-KLUDGE" "clue.clos-kludge;")
				))

;; Ensure VALUES is a legal declaration
(proclaim '(declaration values))

;; Ensure *features* knows about CLOS and PCL
(when (find-package 'pcl)
  (pushnew :pcl  *features*)
  (pushnew :clos *features*))

(when (find-package 'clos)
  (pushnew :clos *features*))

;; Ensure *features* knows about the Common Lisp Error Handler
(when (find-package 'conditions)
  (pushnew :cleh *features*))

(pcl::defsystem CLX
  *clx-directory*
  (
   (depdefs () () ())
   (clx t t ())
   (dependent t t ())
   (clos-patch t t ())
   (macros t t ())
   (bufmac t t ())
   (buffer t t ())
   (display t t ())
   (gcontext t t ())
   (requests t t ())
   (input t t ())
   (fonts t t ())
   (graphics t t ())
   (text t t ())
   (attributes t t ())
   (translate t t ())
   (keysyms t t ())
   (manager t t ())
   (image t t ())
   (resource t t ())
   )
  )

(pcl::defsystem CLUE
  *clue-directory*
  (
   (clue () () ())
   (clx-patch t t ())
   (window-doc t t ())
   (gc-cache t t ())
   (defcontact t t ())
   (intrinsics t t ())
   (resource t t ())
   (gray t t ())
   (cursor t t ())
   (events t t ())
   (virtual t t ())
   (package t t ())
   )
  )

(pcl::defsystem CLUE-EXAMPLES
  *clue-examples-directory*
  (
    (button () () ())		;; label and button contacts
    (menu () () ())		;; menu contacts
    (stream () () ())		;; Interactive stream contact
    (valuators () () ())	;; sliders
    (alt-menu () () ())		;; Menus whose items aren't windows
    (rmenu () () ())		;; Menu whose item list is a resource
    (scroll () () ())     ;; wraps scroll bars onto a window
   )
  )

(pcl::defsystem CLUE-DEMO
  *clue-demo-directory*
  (
   (mouse-doc () () ())			;; pointer documentation window
   (menu-demo () () ())			;; Simple menu demos
   (grapher () () ())			;; tree displayer
   (graph-data (grapher) (grapher) ())	;; Data for grapher
   (listener () () ())			;; Lisp Listener
   (scroller () () ())    		;; scrolling bitmap displayer
   (demo-all t t ())			;; Menu-driven interface for above
   )
  )


