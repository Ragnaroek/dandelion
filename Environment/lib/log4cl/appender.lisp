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

(in-package #:log4cl)

(defclass appender ()
  ((name :initarg :name
	 :initform nil
	 :accessor appender-name)
   (layout :initarg :layout
	   :accessor appender-layout))
  (:documentation "Appender main class"))

(defmethod initialize-instance :after ((appender appender) &rest initargs)
  (declare (ignore initargs))
  (with-slots (layout name) appender
    (setf layout (make-instance 'simple-layout))
    (when (null name)
      (setf name (type-of appender)))))

;; ----------
;; Protocole
;; ----------

(defgeneric log-msg (appender name level message)
  (:documentation "Log a message with the appropriate level"))

;; ------------------------------
;; Appender to a stream
;; ------------------------------

(defclass stream-appender (appender)
  ((log-stream :initarg :log-stream :accessor stream-appender-log-stream)))

(defmethod log-msg ((appender stream-appender) name level message)
  (format (stream-appender-log-stream appender)
          " ~A ~%"
          (format-log-message (appender-layout appender) name level message)))

(defclass console-appender (stream-appender)
  ((log-stream :initform *standard-output*))
  (:documentation "Console appender, is an appender which log message to the default exit"))

;; ----------------------------
;; Appender to log into a file
;; ----------------------------

(defclass file-appender (appender)
  ((file :initarg :file
	 :accessor file-appender-file))
  (:documentation "Appender which log message in a file"))

(defmethod log-msg ((appender file-appender) name level message)
  "Log message into a file. If file exist, the message is append to it,
   or the appender create the file"
  (with-open-file (stream (file-appender-file appender)
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
      (format stream "~A ~%" (format-log-message (appender-layout appender) name level message))))

;; ---------------------------
;; File Appender with rolling
;; ---------------------------

(defclass rolling-file-appender (file-appender)
  ((max-size :initarg :max-size :initform 1000000
	     :accessor rolling-file-appender-max-size)
   (current :initform 0
	    :accessor rolling-file-appender-current))
  (:documentation "Appender which log message in a file. There is a rolling
    with this file when the size of it is grater than a specify size"))

(defun copy-file (source target)
  "Copy a file"
  (with-open-file (in source :direction :input)
      (with-open-file (out target :direction :output :if-does-not-exist :create)
	  (loop with buffer = (make-string (* 64 512))
		for n = (read-sequence buffer in)
		until (= n 0)
		do (write-sequence buffer out :end n)))))

(defun make-archive-name (name number)
  "Create name of this archive file"
  (concatenate 'string name "." (format nil "~A" number)))
  
(defun make-archive (rolling-file-appender)
  "Make a copy of current log file, and incremente current number"
  (let* ((name (file-appender-file rolling-file-appender))
	 (current-number (rolling-file-appender-current rolling-file-appender))
	 (next-number (+ current-number 1)))
    (when (> current-number 0)
      (loop for i from current-number downto 1
	    do (copy-file (make-archive-name name (- i 1)) 
			  (make-archive-name name i))))
    (copy-file name (make-archive-name name 0))
    (delete-file name)
    (setf (slot-value rolling-file-appender 'current) next-number)))

(defmethod log-msg :before ((appender rolling-file-appender) name level message)
  "Log message into a file. If size of the file is greater than the max size,
   we create an archive of the current file, and we create a new current file
   for logging message"
  (declare (ignore name level message))
  (when (> (with-open-file (s (file-appender-file appender) :if-does-not-exist :create) 
	       (file-length s))
	   (rolling-file-appender-max-size appender))
    (make-archive appender)))

;; -------------------
;; Daily Rolling File 
;; -------------------

(defclass daily-rolling-file-appender (file-appender)
  ((date-pattern :initform "%Y-%M-%D"
		 :initarg :date-pattern
		 :accessor daily-rolling-file-appender-pattern)))

(defmethod initialize-instance :after ((appender daily-rolling-file-appender) &rest initargs)
  (declare (ignore initargs))
  (with-slots (file) appender
    (setf file (concatenate 'string
			    (directory-namestring file)
			    (make-date-pattern (daily-rolling-file-appender-pattern appender))
			    "_"
			    (file-namestring file)))))

(defmethod log-msg :before ((appender daily-rolling-file-appender) name level message)
  "Log message into a file named by the current date. If log file is
   a previous date, a new file is created"
  (declare (ignore name level message))
  (let ((pattern (make-date-pattern (daily-rolling-file-appender-pattern appender))))
    (when (not (string-equal (extract-date-pattern (file-appender-file appender))
			     pattern))
      (with-slots (file) appender
	(setf file (concatenate 'string
				(directory-namestring file)
				pattern
				"_"
				(file-namestring file)))))))

(defun extract-date-pattern (file)
  (let* ((name (file-namestring file))
	 (index (position #\_ name)))
    (when (not (null index))
      (subseq name 0 index))))

(defun make-date-pattern (date-pattern)
  (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore second minute hour day-of-week dst-p tz))
    (let ((pattern (list (cons "Y" (write-to-string year))
			 (cons "M" (write-to-string month))
			 (cons "D" (write-to-string date)))))
      (replace-string date-pattern pattern))))



;; -----------------
;; Network Appender
;; -----------------


(defclass network-appender (appender)
  ((server :initarg :server 
           :accessor network-appender-server)
   (data :initarg :data
         :initform '()
         :accessor network-appender-data)
   (items :initarg :size
          :initform 10
          :accessor network-appender-items))
  (:documentation "Network appender bufferize logs message, and send them when size is over"))


(defmethod log-msg ((appender network-appender) name level message)
  (setf (slot-value appender 'data)
        (cons (format-log-message (appender-layout appender) name level message)
              (network-appender-data appender))))


(defun create-buffer (tokens)
  "Create a paragrah from a list of string"
  (apply #'concatenate 'string
         (mapcar #'(lambda (token)
                     (let ((new-token (format nil "~A~%" token)))
                       (concatenate 'string new-token)))
                 tokens)))
