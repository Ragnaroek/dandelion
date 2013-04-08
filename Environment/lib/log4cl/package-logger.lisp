;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package-logger.lisp
;;;; Purpose:       Package logger
;;;; Developers :   Rafal Strzalinski <rstrzalinski@common-lisp.net>
;;;;
;;;; This file, part of log4cl , is Copyright (c) 2003 by Nicolas Lamirault
;;;;
;;;; log4cl users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id: package-logger.lisp,v 1.1 2007/07/17 19:25:39 ragnaroek Exp $
;;;;
;;;; *************************************************************************


(in-package #:log4cl)

(defvar package/logger (make-hash-table :test #'equal))

(defun set-logger-for-package (package-name logger)
  (setf (gethash package-name package/logger) logger))

(defvar *default-package-logger* nil)

(defun find-package-logger (package)
  (or (gethash (package-name package) package/logger)
      *default-package-logger*))  

(defmethod log-debug ((logger (eql t)) message &key appender-type)
  "Log message with debug level"
  (log-message logger :error message :appender-type appender-type))

(defmethod log-info ((logger (eql t)) message &key appender-type)
  "Log message with info level"
  (log-message logger :info message :appender-type appender-type))

(defmethod log-warning ((logger (eql t)) message &key appender-type)
  "Log message with warning level"
  (log-message logger :warning message :appender-type appender-type))

(defmethod log-error ((logger (eql t)) message &key appender-type)
    "Log message with error level"
  (log-message logger :error message :appender-type appender-type))

(defmethod log-fatal ((logger (eql t)) message &key appender-type)
  "Log message with fatal level"
  (log-message logger :fatal message :appender-type appender-type))

(defmethod log-message ((logger (eql t)) level message &key appender-type)
  (let ((logger (find-package-logger *package*)))
    (when logger
      (log-message logger level message :appender-type appender-type))))



