;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          layout.lisp
;;;; Purpose:       Format the log message
;;;; Developers :   Nicolas Lamirault <lam@tuxfamily.org>>
;;;;
;;;; This file, part of log4cl , is Copyright (c) 2003 by Nicolas Lamirault
;;;;
;;;; log4cl users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id
;;;;
;;;; *************************************************************************


(in-package #:log4cl)


(defparameter *layouts*
  '(;; Category of the logging event.
    ("c" . nil)
    ;; Fully qualified package (or class) name of the caller
    ;;"C"                    
    ;; Current date in yyyy/MM/dd hh:mm:ss format
    ("d" . get-time)
    ;; File where the logging event occurred
    ;;"F"
    ;; Hostname
    ("H" . machine-instance)
    ;; Fully qualified name of the calling method followed by the
    ;; callers source the file name and line number between 
    ;; parentheses.
    ;;"l"
    ;; Line number within the file where the log statement was issued
    ;;"L"
    ;; The message to be logged
    ("m" . nil)
    ;; Method or function where the logging request was issued
    ;;"M"
    ;; Newline (OS-independent)
    ("n" . nil)
    ;; Priority of the logging event
    ("p" . nil)
    ;; pid of the current process
    ("P" . get-pid)
    ;;    "%r" ;; Number of milliseconds elapsed from program start to logging event
    ;;    "%x" ;; The elements of the NDC stack (see below)
    ;;    "%X{key}" ;; The entry 'key' of the MDC (see below)
    ;;    "%%" ;; A literal percent (%) sign
    ))
    


(defclass layout ()
  ())
  ;;((format :initform "%c : [%p] %m" :initarg :format :reader get-format :writer set-format)))


(defgeneric format-log-message (layout name level message)
   (:documentation "Print log message with the layout configuration"))



;; --------------------------------------------
;; Simple Layout : print level and log message
;; --------------------------------------------


(defclass simple-layout (layout)
  ())


(defmethod format-log-message ((l simple-layout) name level message)
  (concatenate 'string name " : [" level "] " message))


;; -------------------------------------
;; Pattern layout : customizable layout
;; -------------------------------------


(defclass pattern-layout (layout)
  ((format :initarg :format :reader layout-format :initform "%c : [%p] %m")))



(defmethod format-log-message ((layout pattern-layout) name level message)
  (let ((pattern (list (cons "c" name)
		       (cons "p" level)
		       (cons "m" message)
		       (cons "n" "\n"))))
    (replace-string (layout-format layout) pattern)))


;; ------------
;; HTML Layout
;; ------------


(defclass html-layout (layout)
  ((init :initform 0 :reader get-init :writer set-init)
   (title :initarg :title :reader get-title :initform "Log4cl Log Messages")
   (header :initform "<html>
                      <head>
                      <title> ~A </title>
                      <style type=\"text/css\">
                      <!--
                      body, table {font-family: arial,sans-serif; font-size: x-small;}
                      th {background: #336699; color: #FFFFFF; text-align: left;}
                      -->
                     </style>
                     </head>
                     <body bgcolor=\"#FFFFFF\" topmargin=\"6\" leftmargin=\"6\">
                     <hr size=\"1\" noshade>
                     Log session start at ~A <br>
                     <br>
                     <table cellspacing=\"0\" cellpadding=\"4\" border=\"1\" bordercolor=\"#224466\" width=\"100%\">
                     <tr>
                     <th>Level</th>
                     <th>Category</th>
                     <th>Message</th>~%"
	   :reader get-header)
   (footer :initform "</table>
                      </body>
                      </html>" 
	   :writer get-footer)
   (line-number :initarg :line :initform nil)))


(defconstant +colors+ 
  '(("debug" . "#339933")
    ("info" . "#000000")
    ("warning" . "#993300")
    ("error" . "#993300")
    ("fatal" . "#993300")))


(defmethod format-log-message ((layout html-layout) name level message)
  "Create a log message, with HTML tag"
  (let* ((level-tag "<tr><td title=\"Level\"><font color=\"~A\">~A</td>")
	 (categ-tag "<td title=\"~A category\">~A</td>")
	 (msg-tag "<td title=\"Message\"> ~A </td></tr>~%")
	 (msg (concatenate 'string 
			   (when (= (get-init layout) 0)
			     (format nil (get-header layout) (get-title layout) (get-time)))
			   (format nil level-tag (cdr (find level +colors+ :test #'string-equal :key #'car)) level)
			   (format nil categ-tag name name)
			   (format nil msg-tag message))))
    (when (= (get-init layout) 0)
      (set-init 1 layout))
    msg))



;; -----------
;; XML Layout ????
;; -----------










;; ---------------------
;; Differents functions
;; ---------------------


(defun get-time ()
  "Get current time"
  (multiple-value-bind
      (second minute hour date month year)
      (get-decoded-time)
    (let ((date
           (format nil "~d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d"
                   month date year hour minute second)))
      date)))

  
