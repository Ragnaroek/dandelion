#|
 Dandelion, a Lisp plugin for Eclipse.
 Copyright (C) 2007 Michael Bohn

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
|#


;;;
;; File		     - utils.lisp
;; Description	     - Definiert verschieden allgemeine Hilfsfunktionen
;; Author	     - Michael Bohn
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-user)

(defpackage #:de.fh-trier.evalserver.utils
  (:use #:common-lisp)
  (:export #:find-shortest-string
           #:make-formatted-string
           #:now-string
           #:read-all-from-string
           #:OutputRedirector
           #:get-output
           #:redirect-output?))

(in-package #:de.fh-trier.evalserver.utils)

; input:  list - Liste von Strings, die Liste muss mindestens einen String enthalten
; effect: Sucht den kuerzesten String in einer Liste und gibt diesen zurueck.
; value:  Kuerzester String aus Liste.
;@testcase
(defun find-shortest-string (list)
  (let ((result (first list)))
    (mapc #'(lambda (name)
              (when (< (length name) (length result))
                (setf result name))) list)
  (identity result)))

; input:  formatstring - FORMAT Formatierungsstring
;         arglist - Liste der Argument passend zu formatstring
; effect: Wendet FORMAT auf formatstring und die Liste der Argumente an
; value:  Der mit FORMAT gebildetet String
;@testcase
;@deprecated - weil zu haesslich
(defun make-formatted-string (formatstring arglist)
  (apply 'format (cons nil (cons formatstring arglist))))

; input:  -
; effect: -
; value:  Aktuelles Datum und Zeit im Format DD.MM.YYYY HH:MM:SS als String
; signal: eof-error wenn ungueltiges Objekt in String
(defun now-string ()
  (let ((time-parts (multiple-value-list (get-decoded-time))))
    (format nil "~2,'0d.~2,'0d.~a ~2,'0d:~2,'0d:~2,'0d" (nth 3 time-parts) (nth 4 time-parts) (nth 5 time-parts) 
                                    (nth 2 time-parts) (nth 1 time-parts) (nth 0 time-parts))))

; input:  string - String aus dem alle Objekte gelesen werden sollen
; effect: -
; value:  Liste aller gelesenen Objekte aus dem String
; signal: eof-error wenn ungueltiges Objekt in String
;
; src: Uebungsskript
;@testcase
(defun read-all-from-string (string &optional (pos 0))
  (let (value-list)
    (setq value-list (multiple-value-list (read-from-string string nil 'ende :start pos)))
    (cond ((equal (first value-list) 'ende) nil)
          (t (cons (first value-list) (read-all-from-string string (second value-list)))))))

;;;
;;; Class OutputRedirector
;;;

(defclass OutputRedirector ()
  ((output :reader get-output
     :documentation "the redirected output, as string")
   (out-backup
     :documentation "original stream before redirection")
   (redirect-stream
     :documentation "temporary stream for redirection, a string stream")
   (redirect? :initarg :redirect
             :initform NIL
             :documentation "redirection enabled/disabled?")))

(defmethod redirect-output? ((redirector OutputRedirector) bool)
  (with-slots ((redirect? redirect?)) redirector
    (unless (eql bool redirect?)
      (setf redirect? bool)
      (if bool
        (do-redirect redirector)
        (restore redirector)))))

(defun do-redirect (redirector)
  (with-slots ((stream redirect-stream) (out out-backup)) redirector
    (setf out *standard-output*)
    (setf stream (make-string-output-stream))
    (setf *standard-output* stream)))

(defun restore (redirector)
  (with-slots ((output output) (stream redirect-stream) (out out-backup)) redirector
    (setf *standard-output* out)
    (setf output (get-output-stream-string stream))
    (setf stream nil)))

#|
(setf *test* (make-instance 'OutputRedirector))
(redirect-output? *test* NIL)

(format *standard-output* "test")
(get-output *test*)
|#


;;; END Class OutputRedirector






