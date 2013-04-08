;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file for log4cl
;;;; Developer:     Nicolas Lamirault <lam@tuxfamily.org>
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


(in-package #:cl-user)



(defpackage #:log4cl
  (:use #:cl)
  (:export

   ;; logger
   #:logger
   #:with-logger

   #:set-level
   #:add-level
   #:remove-level
   
   #:get-appender
   #:add-appender
   #:remove-appender
   
   #:log-debug
   #:log-info
   #:log-warning
   #:log-error
   #:log-fatal

   #:debugp
   #:infop
   #:warningp
   #:errorp
   #:fatalp

   ;; appenders
   #:console-appender
   #:file-appender
   #:rolling-file-appender
   #:daily-rolling-file-appender
   #:syslog-appender
   #:db-appender
   #:mail-appender

   #:log-msg
   
   ;; layout
   #:simple-layout
   #:pattern-layout
   #:html-layout
   #:format-log-message
   
   ;; config
   #:load-config-file
   #:parse-config
   )
  (:documentation "log4cl is a Common Lisp utility to logging"))

