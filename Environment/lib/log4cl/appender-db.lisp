;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          appender.lisp
;;;; Purpose:       Different appender to log message
;;;; Developers :   Nicolas Lamirault <lam@tuxfamily.org>
;;;;
;;;; This file, part of log4cl, is Copyright (c) 2003 by Nicolas Lamirault
;;;;
;;;; log4cl users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id
;;;;
;;;; *************************************************************************

(in-package :log4cl)

;; ------------------------------
;; Appender to log into database
;; ------------------------------

(defclass db-appender (appender)
  ((hostname :initarg :hostname 
	     :accessor db-appender-hostname)
   (username :initarg :username 
	     :accessor db-appender-username)
   (password :initarg :password 
	     :accessor db-appender-password)
   (database :initarg :database
	     :accessor db-appender-database)
   (type :initarg :type
	 :accessor db-appender-type)
   (table :initarg :table
	  :accessor db-appender-table))
  (:documentation "Database appender : Mysql, PostgreSQL"))

(defparameter *db-types*
  '(("mysql" . :mysql)
    ("postgresql" . :postgresql)))

(defmethod log-msg ((appender db-appender) name level message)
  "Log a message with into a Mysql database
   Table must have this structure :
    id   	int(16)  	   	auto_increment   Primary   
    level  	varchar(10) 	  	o  	  	 Index 
    message  	varchar(255)"
  (progn
    (clsql:connect (list (db-appender-hostname appender) 
			 (db-appender-database appender)
			 (db-appender-username appender)
			 (db-appender-password appender))
		   :database-type (cdr (assoc (db-appender-type appender) *db-types* test #'string-equal))
		   :if-exists :old)
    (let ((sql (format nil "INSERT INTO ~A (level,message) VALUES ('~A','~A')"
		       (db-appender-table appender)
		       level
		       (format-log-message (appender-layout appender) name level message))))
      (clsql:execute-command sql))))
