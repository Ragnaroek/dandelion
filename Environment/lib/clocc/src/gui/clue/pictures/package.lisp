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

(assert
  (find-package "COMMON-LISP") ()
  "COMMON-LISP package does not exist. 

 Please create a package named COMMON-LISP, nicknamed CL, which exports
     LISP, CLOS and CONDITIONS external symbols.
")

(assert
  (find-package "CLUE") ()
  "CLUE must be loaded before making PICTURES")

(unless (find-package "PICTURES")
  (make-package "PICTURES" :use '(common-lisp clue) :nicknames '("PIC"))

  #-(and clx-mit-r4 ansi-common-lisp)
  ;; Crock! XLIB and COMMON-LISP both want to export define-condition!
  ;; Shadow all XLIB externals already exported by COMMON-LISP.
  (shadowing-import
    (let (shadows)
      (do-external-symbols (s :xlib shadows)
	(multiple-value-bind 
	    (symbol status)(find-symbol (symbol-name s) :common-lisp)
	  (when (and symbol (eq :external status))
	    (push symbol shadows)))))
    :pictures)

  (use-package :xlib :pictures))

