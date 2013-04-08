;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                   P.O. BOX 149149                                |
;;;                              AUSTIN, TEXAS 78714-9149                            |
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

(unless (find-package "CLIO-OPEN")
  (error "You must load CLIO before building the CLIO examples."))

#+explorer
(defsystem clio-examples
  (:name "CLIO Example Programs")
  (:short-name "CLIO Examples")
  (:pathname-default "CLIO:EXAMPLES;")
  
  (:initial-status :experimental)

  ;;  The real source files...
  (:module package          ("package"))
  (:module clio-extras      ("cmd-frame"))
  (:module example-contacts ("sketchpad"))
  (:module sketch           ("sketch"))

  ;;  The transformations...
  (:compile-load package)
  (:compile-load clio-extras)
  (:compile-load example-contacts
		 (:fasload  package)
		 (:fasload  package))
  
  (:compile-load sketch
		 (:fasload  package clio-extras example-contacts)
		 (:fasload  package clio-extras example-contacts)))






(defun load-clio-examples (&key (host "CLIO") (directory "EXAMPLES") (compile-p t) (verbose-p t))
  (dolist (file (mapcar
		  #'(lambda (name)
		      (make-pathname
			:host      host
			:directory directory
			:name      name
			:type	   :wild
			:version   :newest))
		  '("PACKAGE"
		    "CMD-FRAME"
		    "SKETCHPAD"
		    "SKETCH")))
    (when compile-p
      (when verbose-p
	(format t "~% Compiling ~12t~a..." file))
      (compile-file file))
    
    (when verbose-p
      (format t "~% Loading ~12t~a..." file))
    (load file)
    
    (when (and compile-p verbose-p)
      (format t "~%"))))
