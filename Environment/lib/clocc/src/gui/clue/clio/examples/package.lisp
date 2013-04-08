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

(unless (find-package "CLIO-EXAMPLES")
  (make-package "CLIO-EXAMPLES" :use '(common-lisp xlib
						   #-ANSI-CL clos
						   clue clio-open)))
