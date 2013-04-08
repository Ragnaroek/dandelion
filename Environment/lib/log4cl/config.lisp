;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Config.lisp     
;;;; Purpose:       Load configuration file of Log4cl
;;;; Developers:    Nicolas Lamirault <lam@tuxfamily.org>
;;;;
;;;; This file, part of log4cl , is Copyright (c) 2004 by Nicolas Lamirault 
;;;;
;;;; log4cl users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id
;;;;
;;;; *************************************************************************



(in-package #:log4cl)



(defparameter *appenders-type*
  '("console-appender"
    "file-appender"
    "rolling-file-appender"
    "daily-rolling-file-appender"
    "syslog-appender"
    "db-appender"
    ))





(defun extract-root-values (root-config)
  (let ((index (position #\, root-config)))
    (when (not (null index))
      (values (subseq root-config 0 index)
	      (remove #\Space (subseq root-config
				      (+ index 1) 
				      (length root-config)))))))


(defun extract-name (appender-config)
  (subseq appender-config
	  (+ (position #\. appender-config :from-end t) 1)
	  (length appender-config)))
	  

(defmacro with-config-params (params config appender tokens &body body)
  "Macro to get some config parameters"
  `(let ,(mapcar #'(lambda (param-name token)
		      `(,param-name (cl-ini:get-value ,config
						    :section "general"
						    :parameter (concatenate 'string
									    "log4cl.appender."
									    ,appender
									    "."
									    ;;(symbol-name ',param-name)))))
									    ,token))))
		 params tokens)
     ,@body))
	   


(defun set-layout-type (config appender-name layout)
  "Create a layout from configuration"
  ;;(format t "{{{ ~A }} ~%" layout)
  (cond ((string-equal layout "pattern-layout")
	 (with-config-params (pattern) config appender-name ("layout.pattern")
	     (make-instance 'pattern-layout :format pattern)))
	((string-equal layout "simple-layout")
	 (make-instance 'simple-layout))
	((string-equal layout "html-layout")
	 (make-instance 'html-layout))))


(defun set-appender-type (config appender-name appender-type layout-type)
  "Create an appender from configuration"
  (cond ((string-equal appender-type "console-appender")
	 (make-instance 'console-appender
			:name appender-name 
			:layout layout-type))
	((or (string-equal appender-type "file-appender")
	     (string-equal appender-type "rolling-file-appender")
	     (string-equal appender-type "daily-rolling-file-appender"))
	 (with-config-params (file) config appender-name ("file")
	     (cond ((string-equal appender-type "file-appender")
		    (make-instance 'file-appender
				   :name appender-name
				   :layout layout-type
				   :file file))
		   ((string-equal appender-type "rolling-file-appender")
		    (with-config-params (size) config appender-name ("max-size")
			(make-instance 'rolling-file-appender
				       :name appender-name
				       :layout layout-type
				       :file file
				       :max-size (read-from-string size)))))))
	((string-equal appender-type "db-appender")
	 (with-config-params (host user passwd base table type)
			     config
			     appender-name
			     ("host" "user" "passwd" "base" "table" "type")
	     (make-instance 'db-appender
			    :name appender-name
			    :layout layout-type
			    :hostname host
			    :username user
			    :password passwd
			    :database base
			    :type type
			    :table table)))
	((string-equal appender-type "mail-appender")
	 (with-config-params (server from to subject items)
			     config
			     appender-name
			     ("server" "from" "to" "subject" "items")
	     (make-instance 'mail-appender
			    :name appender-name
			    :layout layout-type
			    :server server
			    :from from
			    :to to
			    :subject subject
			    :items (read-from-string items))))))




(defun parse-config (config)
  "Log4cl configuration"
  (let ((data (cl-ini:get-section-data config :section "general"))
	(appenders '()))
    (multiple-value-bind (level appender)
    	(extract-root-values (cl-ini:get-value config
    					       :section "general"
    					       :parameter "log4cl.rootCategory"))
      (mapc #'(lambda (token)
		(when (member (cdr token) *appenders-type* :test #'string-equal)
		  (push (cons (extract-name (car token))
			      (cdr token))
			appenders)))
	    data)
      (let ((logger (make-instance 'logger 
				   :level level
				   :current-appender appender)))
	(mapc #'(lambda (appender-data)
		  ;;(format t "<~A> : <~A> ~%" (car appender-data) (cdr appender-data))
		  (when (member (cdr appender-data) *appenders-type* :test #'string-equal)
		    ;;(format t "### ~A ## ~%" (cdr appender-data))
		    (with-config-params (layout) config (car appender-data) ("layout")
			;;(format t "---> ~A ## ~%" layout)
			(let* ((appender-name (car appender-data))
			       (appender-type (cdr appender-data))
			       (layout-type (set-layout-type config appender-name layout))
			       (appender (set-appender-type config appender-name appender-type layout-type)))
 			  ;;(format t "~A -> ~% ~A> ~% ~A> ~%"
			  ;;appender-type (type-of layout) (type-of appender))
			  (add-appender logger appender)))))
	      appenders)
	logger))))
      
      
      
(defun load-config-file  (file)
  "Create configuration based on log4cl configuration file"
  (cl-ini:parse-conf-file file))

	 
	 
	 
	 