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

;; ---------------------
;; Appender with syslog
;; ---------------------

(defclass syslog-appender (appender)
  ())

(defmethod log-msg ((appender syslog-appender) name level message)
  "Log a message with Syslog"
  (progn
    (openlog name LOG_CONS LOG_LOCAL7)
    (syslog LOG_INFO (format-log-message (appender-layout appender) "" level message))))
