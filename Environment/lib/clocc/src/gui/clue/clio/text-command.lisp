;;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
;;;                                                                                  |
;;;             Copyright (C) 1990, 1990 Texas Instruments Incorporated.             |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

(in-package "CLIO-OPEN")

(export '(
	  text-command-table
	  text-command
	  make-text-command-table
	  )
	'clio-open)

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                            text-command-table                              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(deftype text-command-table () 'hash-table)

(defmacro text-command (text-command-table char)
  `(gethash ,char ,text-command-table))


(defun make-text-command-table (&rest commands)
  "Return a new text-command-table containing the given COMMANDS.
COMMANDS is a plist of the form ([char command]*), where command is
either a functionp object or a list of the form (function . args)."

  (let* ((initial-size (floor (length commands) 2))
	 (table        (make-hash-table :size initial-size)))
    (do ()
	((endp commands))
      (let ((char    (first commands))
	    (command (second commands)))
	(assert command nil "No command given for ~a." char)
	
	(setf (text-command table char) command)

	(setf commands (cddr commands))))
    table)) 


