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
;; File		     - main.lisp
;; Description	 - Definiert main-Funktion die als Einsprungspunkt fuer Images dient
;; Author	     - Michael Bohn
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage #:dandelion-main
  (:use #:common-lisp 
        #:dandelion-server
        #:dandelion-utils)
  (:export #:main))

(in-package #:dandelion-main)

(defparameter *exit-lisp* T) ;nur fuer testzwecke

; input:  -
; effect: Beendet die Lisp-Umgebung. Verhindert Aufruf der Konsole.
; value:  -
(defun exit-lisp ()
  (when *exit-lisp*
    #+clisp (ext:exit)
    #+sbcl (sb-ext:exit)
    #-(or clisp sbcl) (error "No exit method implemented")))

; input:  &rest
;         message - Kein oder mehrere Strings
; effect: Schreibt message an den Standard-Output und beendet die Lisp-Umgebung
; value:  - (Lisp-Umgebung wird beendet)
(defun arg-error (&rest message)
  (format t "Illegal parameter: ~{~a~^ ~}" message)
  (exit-lisp))

; input:  bool - String der Boolean enthaelt (T oder NIL)
;         param - String fuer Beschreibung des Parameters
;         default - Der Wert der zurueckgeliefert wird wenn bool = NIL
; effect: Ueberprueft die bool-Variable ob sie gueltige Boolean Bezeichnung enthaelt.
; signal: arg-error - wenn bool ungueltig
; value:  T oder NIL (nicht String), default wenn bool = NIL
;@testcase
(defun check-boolean (bool param default)
  (cond ((not bool) default)
        ((unless (or (equalp bool "T") (equalp bool "NIL"))
           (arg-error (format nil "illegal ~a, must be one of: T NIL" param))))
          ;string kann hier nur T oder NIL sein
        (T (read-from-string bool))))


; input:  port-string - String der Portnummer enthaelt
; effect: Ueberprueft den uebergebenen String auf gueltige Port-Angabe
; signal: arg-error - wenn port-string ungueltig
; value:  portnummer wenn port-string korrekte port-angabe enthaelt
;@testcase
(defun check-port (port-string)
  (let ((port (and port-string (parse-integer port-string :junk-allowed T))))
    (when (or (not port) (< port 0) (> port 65535))
        (arg-error "port number" port-string "is invalid"))
    port))

; input:  level-string - String der Log-Level Angabe enthaelt
;         default - Der Wert der zurueckgeliefert wird wenn level-string = NIL
; effect: Ueberprueft die level-string Variable ob sie gueltige Log-Level Angabe enthaelt
; signal: arg-error - wenn level-string ungueltig
; value:  Log-Level wenn level-string gueltige Angabe enthaelt, default wenn level-string = NIL
;@testcase
(defun check-log-level (level-string default)
  (cond ((not level-string) default)
        ((unless (some #'(lambda (valid-level) (equalp level-string (format nil "~a" valid-level))) +log-levels+)
           (arg-error (format nil "illegal log level, must be one of: ~{~a~^ ~}" +log-levels+))))
        (T (read-from-string (format nil ":~a" level-string)))))

;normale Reihenfolge der Parameter in args
(defun start-with-default-order (args)
  (start-eval-server (check-port (first args))
                       :serve-forever (check-boolean (second args) "serve mode" nil)
                       :log-level (check-log-level (third args) :error)))

#+clisp
(defun main-clisp ()
  (let ((args ext:*args*)) ;uebergebene Parameter aus der Konsole
    (when (equalp (first args) "lispfile") (setf args (rest args)))
    (start-with-default-order args)))

#+sbcl
(defun main-sbcl ()
  (start-with-default-order (rest sb-ext:*posix-argv*)))

; input:  -
; effect: Hauptfunktion die fuer Image-Builds und Exec-Builds angegeben wird.
; value:  -
(defun main ()
  ;Verteilung auf verschiende read-time-conditionals
  #+clisp (main-clisp)
  #+sbcl (main-sbcl)
  #-(or clisp sbcl) (error "No main method implemented")
  (exit-lisp))
