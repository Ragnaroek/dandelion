;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          appender-mail.lisp
;;;; Purpose:       Appender which send logs messages by mail
;;;; Programmer:    Nicolas Lamirault <lam@tuxfamily.org>
;;;;
;;;; This file, part of log4cl, is Copyright (c) 2004 by Nicolas Lamirault
;;;;
;;;; log4cl users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id
;;;;
;;;; *************************************************************************

;; mail-appender use unetwork library to send mail


(in-package :log4cl)

;; --------------
;; Mail appender
;; --------------



(defclass mail-appender (network-appender)
  ((from :initarg :from
         :accessor mail-appender-from)
   (to :initarg :to
       :accessor mail-appender-to)
   (subject :initarg :subject
            :accessor mail-appender-subject))
  (:documentation "Stores loging events and send them by mail"))


(defmethod log-msg :before ((appender mail-appender) name level message)
  (when (= (length (network-appender-data appender)) (network-appender-items appender))
    (let ((buffer (create-buffer (reverse (network-appender-data appender)))))
      (unetwork:smtp-send-mail (mail-appender-from appender)
			       (list (cons :to (mail-appender-to appender)))
			       (mail-appender-subject appender)
			       buffer
			       (network-appender-server appender))
      (setf (slot-value appender 'data) '()))))



