;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          log4cl.asd
;;;; Purpose:       ASDF definition file for log4cl package
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


(defpackage #:log4cl-system 
  (:use #:cl 
	#:asdf))

(in-package #:log4cl-system)

(defsystem :log4cl
  :name "log4cl"
  :author "Nicolas Lamirault <lam@tuxfamily.org>"
  :version "0.3"
  :licence "Lisp Lesser GNU General Public License"
  :description "Log tool for Common Lisp"
  :properties (((#:author #:email) . "lam@tuxfamily.org")
	       (#:date . "05/11/2003")
	       ((#:albert #:output-dir) . "doc/")
	       ((#:albert #:formats) . ("docbook"))
	       ((#:albert #:docbook #:template) . "book")
	       ((#:albert #:docbook #:baseurl) . "")
	       ((#:albert #:docbook #:bgcolor) . "white")
	       ((#:albert #:docbook #:textcolor) . "black"))
  :components ((:file "package")
	       (:file "logger" :depends-on ("package"))
	       (:file "appender" :depends-on ("package" "tools"))
	       (:file "layout" :depends-on ("package" "tools"))
	       (:file "package-logger" :depends-on ("logger"))
               (:file "tools" :depends-on ("package"))))

(defsystem :log4cl.syslog
  :components ((:file "appender-syslog" :depends-on ("cl-syslog"))
               (:file "cl-syslog"))
  :depends-on (:log4cl :uffi))

(defsystem :log4cl.db
  :components ((:file "appender-db"))
  :depends-on (:log4cl))

(defsystem :log4cl.mail
  :components ((:file "appender-mail"))
  :depends-on (:log4cl :unetwork))
