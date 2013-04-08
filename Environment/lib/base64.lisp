;;;; This file implements the Base64 transfer encoding algorithm as
;;;; defined in RFC 1521 by Borensten & Freed, September 1993.
;;;;
;;;; Written by Juri Pakaste <juri@iki.fi>. It is in the public
;;;; domain. Input is welcome.
;;;;
;;;; $Id: base64.lisp,v 1.1 2007/07/17 19:25:52 ragnaroek Exp $

(defpackage "BASE64"
  (:use "CL")
  (:export #:base64-encode #:base64-decode))

(in-package :base64)

(defparameter *encode-table*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

#+sbcl (defconstant table-init-elem 0)
#-sbcl (defconstant table-init-elem nil)

(defparameter *decode-table*
  (let ((da (make-array (list 256)
			:element-type 'integer
			:initial-element table-init-elem)))
    (loop for character across *encode-table*
	  for index from 0 below 64
	  do (setf (elt da (char-code character)) index))
    da))

(defun base64-encode (string)
  (let ((result (make-array
		 (list (* 4 (truncate (/ (+ 2 (length string)) 3))))
		 :element-type 'base-char)))
    (do ((sidx 0 (+ sidx 3))
	 (didx 0 (+ didx 4))
	 (chars 2 2)
	 (value nil nil))
	((>= sidx (length string)) t)
      (setf value (ash (logand #xFF (char-code (char string sidx))) 8))
      (dotimes (n 2)
	(when (< (+ sidx n 1) (length string))
	  (setf value
		(logior value
			(logand #xFF (char-code (char string (+ sidx n 1))))))
	  (incf chars))
	(when (= n 0)
	  (setf value (ash value 8))))
      (setf (elt result (+ didx 3))
	    (elt *encode-table* (if (> chars 3) (logand value #x3F) 64)))
      (setf value (ash value -6))
      (setf (elt result (+ didx 2))
	    (elt *encode-table* (if (> chars 2) (logand value #x3F) 64)))
      (setf value (ash value -6))
      (setf (elt result (+ didx 1))
	    (elt *encode-table* (logand value #x3F)))
      (setf value (ash value -6))
      (setf (elt result didx)
	    (elt *encode-table* (logand value #x3F))))
    result))

(defun base64-decode (string)
  (let ((result (make-array (* 3 (truncate (/ (length string) 4)))
			    :element-type 'base-char))
	(ridx 0))
    (loop for schar across string
	  for svalue = (elt *decode-table* (char-code schar))
	  with bitstore = 0
	  with bitcount = 0
	  do (unless (null svalue)
	       (setf bitstore (logior (ash bitstore 6) svalue))
	       (incf bitcount 6)
	       (when (>= bitcount 8)
		 (decf bitcount 8)
		 (setf (elt result ridx)
		       (code-char (logand (ash bitstore (- bitcount)) #xFF)))
		 (incf ridx)
		 (setf bitstore (logand bitstore #xFF)))))
    (subseq result 0 ridx)))
