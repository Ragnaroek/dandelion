;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
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


(in-package "COMMON-LISP-USER")

(assert
  (find-package "COMMON-LISP") ()
  "COMMON-LISP package does not exist. 

 Please create a package named COMMON-LISP which exports CLOS and CLCS.
 (You may need to make this a nickname of the LISP package.)
")

(assert
  (find-package "CLUE") ()
  "CLUE must be loaded before making CLIO-OPEN.")


;; The Open Look implementation of CLIO goes in CLIO-OPEN...
(unless (find-package "CLIO-OPEN")
  (make-package "CLIO-OPEN" :use '(:common-lisp :clue))
  
  
  #-(and clx-mit-r4 ansi-common-lisp)
  ;; Crock! XLIB and COMMON-LISP both want to export define-condition!
  ;; Shadow all XLIB externals already exported by COMMON-LISP.
  (shadowing-import
    (let (shadows)
      (do-external-symbols (s :xlib shadows)
	(multiple-value-bind (symbol status) (find-symbol (symbol-name s) :common-lisp)
	  (when (and symbol (eq :external status))
	    (push symbol shadows)))))
    :clio-open)

  (use-package :xlib :clio-open))



