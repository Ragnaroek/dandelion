;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-syslog
;;;; Purpose:       Syslog-ng interface using UFFI
;;;; Developer:     Nicolas Lamirault <lam@perave.org>
;;;;
;;;; This file, part of cl-syslog, is Copyright (c) 2003 by Nicolas Lamirault
;;;;
;;;; log4cl users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id
;;;;
;;;; *************************************************************************


(in-package #:log4cl)

;; Log options

(uffi:def-constant LOG_CONS #X02)
(uffi:def-constant LOG_ODELAY #X04)
(uffi:def-constant LOG_NDELAY #X18)
(uffi:def-constant LOG_PERROR #X20)
(uffi:def-constant LOG_PID #X01)

;; priority level

(uffi:def-constant LOG_EMERG 0)
(uffi:def-constant LOG_ALERT 1)
(uffi:def-constant LOG_CRIT 2)
(uffi:def-constant LOG_ERR 3)
(uffi:def-constant LOG_WARNING 4)
(uffi:def-constant LOG_NOTICE 5)
(uffi:def-constant LOG_INFO 6)
(uffi:def-constant LOG_DEBUG 7)

;; facilities

(uffi:def-constant LOG_KERN (ash 0 3))
(uffi:def-constant LOG_USER (ash 1 3))
(uffi:def-constant LOG_MAIL (ash 2 3))
(uffi:def-constant LOG_DAEMON (ash 3 3))
(uffi:def-constant LOG_AUTH (ash 4 3))
(uffi:def-constant LOG_SYSLOG (ash 5 3))
(uffi:def-constant LOG_LPR (ash 6 3))
(uffi:def-constant LOG_NEWS (ash 7 3))
(uffi:def-constant LOG_UUCP (ash 8 3))
(uffi:def-constant LOG_CRON (ash 9 3))
(uffi:def-constant LOG_AUTHPRIV (ash 10 3))
(uffi:def-constant LOG_FTP (ash 11 3))
(uffi:def-constant LOG_LOCAL0 (ash 16 3))
(uffi:def-constant LOG_LOCAL1 (ash 17 3))
(uffi:def-constant LOG_LOCAL2 (ash 18 3))
(uffi:def-constant LOG_LOCAL3 (ash 19 3))
(uffi:def-constant LOG_LOCAL4 (ash 20 3))
(uffi:def-constant LOG_LOCAL5 (ash 21 3))
(uffi:def-constant LOG_LOCAL6 (ash 22 3))
(uffi:def-constant LOG_LOCAL7 (ash 23 3))

;; functions

(uffi:def-function ("openlog" openlog)
		   ((ident :cstring)
		    (option :int)
		    (facility :int))
		   :returning :void)


(uffi:def-function ("syslog" syslog)
		   ((priority :int)
		    (format :cstring))
		   :returning :void)


(uffi:def-function ("closelog" closelog)
		   ()
		   :returning :void)





