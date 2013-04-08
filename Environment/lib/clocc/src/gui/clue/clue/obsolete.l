;;; -*- Mode:Lisp; Package:CLUEI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                   P.O. BOX 149149                                |
;;;                                AUSTIN, TEXAS 78714-9149                          |
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

(in-package "CLUEI")


#+explorer
(compiler:make-obsolete defaction "use DEFMETHOD or DEFUN to define an action function")

#+explorer
(compiler:make-obsolete call-action "replace with a call to the action function")

#+explorer
(compiler:make-obsolete present "set contact-state to :MAPPED instead")

#+explorer
(compiler:make-obsolete dismiss "set contact-state to :WITHDRAWN instead")

#+explorer
(unless (find-symbol "WM-PROTOCOLS" 'xlib)
  (compiler:make-obsolete wm-protocols "use the WM-PROTOCOLS-USED accessor instead.")
  (export 'wm-protocols 'cluei))

