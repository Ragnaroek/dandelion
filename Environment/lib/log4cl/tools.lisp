;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          
;;;; Purpose:       
;;;; Developer:    Nicolas Lamirault <lam@tuxfamily.org>
;;;;
;;;; This file, part of , is Copyright (c) 2003 by Nicolas Lamirault
;;;;
;;;; log4cl users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id
;;;;
;;;; *************************************************************************



(in-package #:log4cl)



(defun replace-string (pattern-string pattern-replaces)
  "Create a log message, depends on format pattern"
  (let ((result "")
	(token ""))
    (loop for i from 0 to (- (length pattern-string) 1) do 
 	  (if (char= #\% (elt pattern-string i))
	      (let ((next-elem (string (elt pattern-string (+ i 1)))))
		(setf token (cdr (find next-elem pattern-replaces
				       :test #'string-equal :key #'car)))
		(setf i (incf i)))
	      (setf token (string (elt pattern-string i))))
	  (setf result (concatenate 'string result token))
	  (setf token ""))
    result))

