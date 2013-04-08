;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          logger.lisp
;;;; Purpose:       Main logger
;;;; Developer:    Nicolas Lamirault <lam@tuxfamily.org>
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


(in-package #:log4cl)


(defparameter *levels* '(:debug :info :warning :error :fatal))


(defclass logger ()
  ((name :initform "logger" 
	 :initarg :name 
	 :accessor logger-name)
   (appenders :initform '()
	      :reader logger-appenders)
   (level :initarg :level
	  :initform :info
	  :reader logger-level)
   (possible-levels :initform *levels* 
		    :reader logger-possible-levels)
   (current-appender :initarg :current-appender 
                     :initform nil
		     :accessor logger-current-appender)
   (parent :reader logger-parent))
  (:documentation "Logger"))



;; (defmethod initialize-instance :after ((logger logger) &rest initargs)
;;   (declare (ignore initargs))
;;   (with-slots (appenders) logger
;;     (push (make-instance 'console-appender :name "default")
;; 	  (slot-value logger 'appenders))))



(defmacro with-logger ((logger &key name appenders level) &body body)
  `(let ((,logger (make-instance 'logger
				 :name ,name
				 :appenders ,appenders
				 :level ,level)))
     ,@body))



;; -------
;; Levels
;; -------


(defmacro with-level (logger level &body body)
  `(when (not (null (level-rank ,logger ,level)))
     ,@body))


(defmacro with-not-level (logger level &body body)
  `(when (null (level-rank ,logger ,level))
     ,@body))


(defmethod level-rank ((logger logger) level)
  "Get the rank level"
  (position level (logger-possible-levels logger) :test #'string-equal))


(defmethod set-level ((logger logger) new-level)
  "Change the logger's level"
  (with-level logger new-level
      (setf (slot-value logger 'level) new-level)))


(defmethod is-enabled-for ((logger logger) level)
  "Verify is priority can be treate"
  (with-level logger level
      (<= (level-rank logger (logger-level logger)) (level-rank logger level))))


(defmethod add-level ((logger logger) new-level &key place level)
  "Add a new level. We can set the new-level with the least importance if place
   keyword is 'least', or the most importance with 'most', or after a level with
   'relative' keyword"
  (with-not-level logger new-level
      (let* ((levels (logger-possible-levels logger))
	     (new-levels (if (not (null place))
			     (cond ((string-equal place "least")
				    (append (list new-level) levels))
				   ((string-equal place "most")
				    (append levels (list new-level)))
				   ((string-equal place "relative")
				    (with-level logger level
					(let ((rank (level-rank logger level)))
					  (append (subseq levels 0 rank)
						  (list new-level)
						  (subseq levels rank (length levels)))))))
			     levels)))
	(setf (slot-value logger 'possible-levels) new-levels))))


(defmethod remove-level ((logger logger) level)
  "Remove a level from list's levels"
  (setf (slot-value logger 'possible-levels) 
	(remove level (logger-possible-levels logger) :test #'string=)))
  


;; ---------------------
;; Appenders operations
;; ---------------------


(defmethod get-appender ((logger logger) type)
  "Look for the appender type"
  (find type (logger-appenders logger) :test #'string-equal :key #'type-of))


(defmethod is-appender ((logger logger) appender)
  "Is the appender is attached to this category"
  (member appender (logger-appenders logger) :test #'string-equal :key #'appender-name))


(defmethod add-appender ((logger logger) appender)
  "Add an appender to the logger"
  (setf (slot-value logger 'appenders) (cons appender (logger-appenders logger)))
  (when (null (logger-current-appender logger))
    (setf (slot-value logger 'current-appender) (appender-name appender))))


(defmethod remove-appender ((logger logger) appender)
  "Remove an appender"
  (with-slots (appenders current-appender) logger
    (setf appenders (remove (appender-name appender) (logger-appenders logger) 
			    :test #'string-equal :key #'appender-name))
    (when (string-equal (logger-current-appender logger) (appender-name appender))
      (setf current-appender (car (logger-appenders logger))))))


(defmacro with-appender ((appender) logger appender-type &body body)
  `(let ((,appender (get-appender ,logger ,appender-type)))
     (when (not (null appender))
       ,@body)))



;; --------------
;; Log functions
;; --------------


(defmethod log-debug ((logger logger) message &key appender-type)
  "Log message with debug level"
  (log-message logger :error message :appender-type appender-type))


(defmethod log-info ((logger logger) message &key appender-type)
  "Log message with info level"
  (log-message logger :info message :appender-type appender-type))


(defmethod log-warning ((logger logger) message &key appender-type)
  "Log message with warning level"
  (log-message logger :warning message :appender-type appender-type))


(defmethod log-error ((logger logger) message &key appender-type)
    "Log message with error level"
  (log-message logger :error message :appender-type appender-type))


(defmethod log-fatal ((logger logger) message &key appender-type)
  "Log message with fatal level"
  (log-message logger :fatal message :appender-type appender-type))


(defmethod log-message ((logger logger) level message &key appender-type)
  "Log message with the appropriate level if the default level of the logger is less important
   If appender type is specified, we log only with it, with all appenders"
  (let ((name (logger-name logger))
	(level-name (symbol-name level)))
    (when (is-enabled-for logger level)
      (if (not (null appender-type))
	  (with-appender (appender) logger (symbol-name appender-type)
	      (log-msg appender name level-name message))
	  (mapc #'(lambda (app)
		    (log-msg app name level-name message))
		(logger-appenders logger))))))



;; -----------------------------------
;; Predicat to know the current level
;; -----------------------------------


(defmethod levelp ((logger logger) level)
  "Predicat for level"
  (not (null (member level (logger-possible-levels logger)))))


(defmethod debugp ((logger logger))
  "Return TRUE if logger's level is debug"
  (equal (logger-level logger) :debug))


(defmethod infop ((logger logger))
  "Return TRUE if logger's level is info"
  (equal (logger-level logger) :info))


(defmethod warningp ((logger logger))
  "Return TRUE if logger's level is warning"
  (equal (logger-level logger) :warning))


(defmethod errorp ((logger logger))
  "Return TRUE if logger's level is error"
  (equal (logger-level logger) :error))


(defmethod fatalp ((logger logger))
  "Return TRUE if logger's level is fatal"
  (equal (logger-level logger) :fatal))







