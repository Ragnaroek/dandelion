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
;; File		     - protocol.lisp
;; Description	     - Protokollimplementierung fuer Eval-Server
;; Author	     - Michael Bohn
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage #:dandelion-protocol
  (:use #:common-lisp 
        #:cl-ppcre
        #:base64
        #:dandelion-utils
        #:dandelion-meta)
  (:export #:malformed-protocol-error
           #:protocol-string
           #:protocol-reader
           #:input-stream
           #:protocol-writer
           #:output-stream
           #:read-connect
           #:read-eval
           #:read-identifier
           #:read-invoke-restart
           #:read-line-from-reader
           #:read-function-request
           #:read-macro-request
           #:write-ok
           #:write-error
           #:write-eval-success
           #:write-eval-error
           #:write-read-error
           #:with-line-read
           #:write-line-to-writer
           #:write-package-list
           #:write-function-list
           #:write-io-termination
           #:init-protocol
           #:+token-ok+
           #:+token-error+
           #:+token-eval-error+
           #:+token-read-error+
           #:+token-connect+
           #:+token-disconnect+
           #:+token-eval+
           #:+token-invoke-restart+
           #:+token-abort+
           #:+token-packages+
           #:+token-macros+
           #:+token-functions+
           #:+token-function-list+))

(in-package #:dandelion-protocol)

;###########################################
;#
;# Conditions
;#
;###########################################

(define-condition malformed-protocol-error (error)
  ((text :initarg :protocol-string :reader protocol-string))
  (:report (lambda (condition stream)
             (format stream "unexpected response: ~a" (protocol-string condition)))))

;###########################################
;#
;# Klassendefinitionen
;#
;###########################################

(defclass protocol-reader ()
  ((input-stream :initarg :input-stream
                 :initform (error "must supply a input-stream")
                 :reader input-stream)))

; Abstraktes Lesen von einem Reader
(defgeneric read-line-from-reader (reader)
  (:documentation "Zeile aus dem input-stream lesen"))

(defclass protocol-writer ()
  ((output-stream :initarg :output-stream
                  :initform (error "must supply a output-stream")
                  :reader output-stream)))

(defgeneric write-line-to-writer (writer line)
  (:documentation "Zeile in den output-stream schreiben, terminiert mit LF"))

(defclass compiled-regex ()
  ((regex :initarg :regex
          :initform (error "must supply a regular expression for the command")
          :reader regex)
   (scanner :reader scanner)))

;###########################################
;#
;# Konstruktoren
;#
;###########################################

;@testcase
(defmethod initialize-instance :after ((reader protocol-reader) &key)
  (when (not (input-stream-p (input-stream reader)))
    (error 'type-error :datum (input-stream reader) :expected-type 'input-stream-p)))

;@testcase
(defmethod initialize-instance :after ((writer protocol-writer) &key)
  (when (not (output-stream-p (output-stream writer)))
    (error 'type-error :datum (output-stream writer) :expected-type 'output-stream-p)))

;@testcase
(defmethod initialize-instance :after ((compiled-regex compiled-regex) &key)
  (setf (slot-value compiled-regex 'scanner) (create-scanner (regex compiled-regex) :case-insensitive-mode T)))

;###########################################
;#
;# Konstanten + Globale Variablen
;#
;###########################################
;; String-UI Konstanten
(defparameter +no-restart-description+ "none")

;; Tokens
(defparameter +token-ok+         "OK")
(defparameter +token-error+      "ERROR")
(defparameter +token-eval-error+ "EVAL-ERROR")
(defparameter +token-read-error+ "READ-ERROR")
(defparameter +token-connect+    "CONNECT")
(defparameter +token-disconnect+ "DISCONNECT")
(defparameter +token-eval+       "EVAL")
(defparameter +token-invoke-restart+ "INVOKE-RESTART")
(defparameter +token-abort+      "ABORT")
(defparameter +token-packages+   "PACKAGES")
(defparameter +token-macros+     "MACROS")
(defparameter +token-functions+  "FUNCTIONS")
(defparameter +token-package-list+ "PACKAGE-LIST")
(defparameter +token-function-list+ "FUNCTION-LIST")

(defparameter +regex-blank+ "\\s+") ;blank auch in build-regex aendern
(defparameter +regex-identifier+ "^(([\\w\\-]+)\\s+)|([\\w\\-]+$)")
(defparameter +regex-host+ "[\\w\\.]+")
(defparameter +regex-port+ "(\\d{1,5}$)|(\\d{1,5}\\s+)")
(defparameter +regex-number+ "\\d+")
(defparameter +regex-symbol+ "[^\\s()]+")
(defparameter +regex-base64+ "[a-zA-Z0-9+/=]+")

; input:  parts - Kein oder mehrere Regex-Teile
; effect: Fuegt die Regex-Teile zu einem Regex zusammen. Die einzelnen Teile werden durch das Whitespace-Regex getrennt (\\s+)
; value:  -
(defun build-regex (&rest parts)
  (format NIL "^~{(~a)~^\\s+~}$" parts))

; input:  &rest
;         strings - Kein oder mehrere Strings
; effect: Konkateniert die String aus Parameterliste und gibt die Konkatenation zurueck.
; value:  String, Konkatenierte String aus Rest-Parametern.
(defun conc-strings (&rest strings)
  (apply #'concatenate 'string strings)
)


;TODO convert to constants

(defparameter *invoke-restart-compiled* (make-instance 'compiled-regex :regex (conc-strings 
                                                                         (build-regex +token-invoke-restart+ +regex-symbol+ +regex-base64+)
                                                                         "|" (build-regex +token-invoke-restart+ +regex-symbol+))))
(defparameter *connect-compiled* (make-instance 'compiled-regex :regex (build-regex +token-connect+ +regex-host+ +regex-port+)))
(defparameter *eval-compiled* (make-instance 'compiled-regex :regex (build-regex +token-eval+ +regex-symbol+ +regex-base64+)))
(defparameter *function-request-compiled* (make-instance 'compiled-regex :regex (build-regex +token-functions+ +regex-symbol+)))
(defparameter *macro-request-compiled* (make-instance 'compiled-regex :regex (build-regex +token-macros+ +regex-symbol+)))
(defparameter *identifier-compiled* (make-instance 'compiled-regex :regex +regex-identifier+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Systemabhaengige Funktionen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-10-termination (stream)
  #+clisp (write-byte-clisp 10 stream) ;newline problem in clisp
  #-clisp (write-char (code-char 10) stream))

#+clisp
(defun write-byte-clisp (byte stream)
   (setf (stream-element-type stream) '(UNSIGNED-BYTE 8))
   (write-byte byte stream)
   (setf (stream-element-type stream) 'character))

;###########################################
;#
;# Methoden
;#
;###########################################

;****************************
;; INTERNAL helper functions
;****************************

; input:  string - Ein String
;         scanner - Scanner-Objekt
; effect: Ueberprueft den uebergebenen String mit scanner auf syntaktische Korrektheit. Loest
;         ein malformed-protocol-error Signal aus wenn String inkorrekt. Wenn String korrekt
;         wird dier Zerlegung des Strings als Liste zureckgegeben.
; value:  - 
(defun read-list (string scanner)
  (cond ((scan scanner string)
         (split +regex-blank+ string))
        (T (error 'malformed-protocol-error :protocol-string string))))

; input:  check - beliebig
;         received - Empfanger String
; effect: Wenn check = NIL ist wird ein malformed-protocol-error Signalisiert. Ansonsten wird
;         check zurueckgegeben.
; value:  check, wenn check != NIL
(defun check-read (check received)
  (cond (check (identity check))
        (T (error 'malformed-protocol-error :protocol-string received))))

;****************************
;; EXPORTED functions
;****************************

;;;;;;;;;;;;;;
;;;; Reader
;;;;;;;;;;;;;;

; input:  reader - Ein protocol-reader Objekt
; effect: Liest aus dem uebergebenen protocol-reader die naechste Zeile ein.
;         Implementierung der generischen Methode fuer protocol-reader.
; value:  Stringzeile gelesen aus dem Input-Stream des Readers.
;@testcase
(defmethod read-line-from-reader ((reader protocol-reader))
  (read-line (input-stream reader)))

; input:  reader - nicht ausgewertet, Reader aus dem die Zeile gelesen werden kann
;         var    - nicht ausgewertet, Variable an die die gelesene Variable gebunden wird
;         body   - Funktionsrump, var ist nur innerhalb diese Rumpfes gueltig
; effect: Bindet die naechste Zeile aus dem Input-Stream des Readers an die in var
;         gelieferte Variable. Die Variable ist nur gueltig innerhalb des Funktionsrumpfes
; value:  -
;@testcase
(defmacro with-line-read (reader var &body body)
  `(let ((,var (read-line-from-reader ,reader)))
     ,@body))

; input:  string - String aus dem der Identifier gelesen werden soll
; effect: Liefert das Identifier Token des Kommandos zurueck. Liefert keine Liste (wie die anderen Read-Funktionen) sondern
;         direkt den Identifier-String.
; value:  Identifier-String
; signal: malformed-protocol-error, wenn Identifier ungueltiges Format hat (siehe Protokollbeschreibung)
;@testcase
(defun read-identifier (string)
  (check-read
   (register-groups-bind (nil id1 id2) ((scanner *identifier-compiled*) string)
     (or id1 id2)) string))

; input:  string - String aus dem das CONNECT-Kommando gelesen werden soll
; effect: Liefert die Bestandteile des CONNECT-Kommandos als Liste zurueck.
; value:  Liste Bestandteile CONNECT-Kommando. Portnummer als Zahl.
; signal: malformed-protocol-error, wenn ungueltiges Format (siehe Protokollbeschreibung)
;@testcase
(defun read-connect (string)
  (let ((list (read-list string (scanner *connect-compiled*))))
    (setf (third list) (parse-integer (third list)))
    (identity list)))

; input:  string - String aus dem das EVAL-Kommando gelesen werden soll
; effect: Liefert die Bestandteile des EVAL-Kommandos als Liste zurueck.
; value:  Liste Bestandteile EVAL-Kommando. Base64 Form wird dekodiert. 
; signal: malformed-protocol-error, wenn ungueltiges Format (siehe Protokollbeschreibung)
;@testcase
(defun read-eval (string)
  (check-read 
   (register-groups-bind (id package form) ((scanner *eval-compiled*) string)
     (list id package (base64-string-to-string form))) string))

; input:  string - String aus dem das INVOKE-RESTART-Kommando gelesen werden soll
; effect: Liefert die Bestandteile des INVOKE-RESTART-Kommandos als Liste zurueck.
; value:  Liste Bestandteile INVOKE-RESTART-Kommando. restart-args werden Base64-dekodiert.
; signal: malformed-protocol-error, wenn ungueltiges Format (siehe Protokollbeschreibung)
;@testcase
(defun read-invoke-restart (string)
  (let (list)
    (setf list (read-list string (scanner *invoke-restart-compiled*)))
    (when (third list) (setf (third list) (base64-string-to-string (third list))))
    (identity list)))

; input:  string - String aus dem das Function-Request Kommando gelesen werden soll
; effect: Liefert die Bestandteile des Function-Request als Liste.
; value:  Liste Bestandteile Function-Request.
; signal: malformed-protocol-error, wenn ungueltiges Format (siehe Protokollbeschreibung)
;@testcase
(defun read-function-request (string)
  (read-list string (scanner *function-request-compiled*)))

; input:  string - String aus dem das Macro-Request Kommando gelesen werden soll
; effect: Liefert die Bestandteile des Macro-Request als Liste.
; value:  Liste Bestandteile Macro-Request.
; signal: malformed-protocol-error, wenn ungueltiges Format (siehe Protokollbeschreibung)
;@testcase
(defun read-macro-request (string)
  (read-list string (scanner *macro-request-compiled*)))

;;;;;;;;;;;;;;
;;;; Writer
;;;;;;;;;;;;;;

; input:  writer - Ein protocol-writer Objekt
; effect: Schreibt in den uebergebenen protocol-writer den uebergebenen String.
;         Implementierung der generischen Methode fuer protocol-writer.
; value:  -
;@testcase
(defmethod write-line-to-writer ((writer protocol-writer) line)
  (write-string line (output-stream writer))
  (write-10-termination (output-stream writer))
  (force-output (output-stream writer)))

; input:  writer - Ein protocol-writer Objekt
; effect: Schreibt das OK-Kommando in den Output-Stream des Writers.
; value:  -
;@testcase
(defun write-ok (writer)
  (write-line-to-writer writer (format nil "~a" +token-ok+)))

; input:  writer - Ein protocol-writer Objekt
;         description - Ein String welcher den Fehler genauer beschreibt
; effect: Schreibt das ERROR-Kommando in den Output-Stream des Writers.
; value:  -
;@testcase
(defun write-error (writer formatstring &rest args)
  (write-line-to-writer writer (format nil "~a ~a" +token-error+ (string-to-base64-string (make-formatted-string formatstring args)))))

; input:  writer - Ein protocol-writer Objekt
;         package-string - Paketname des aktuellen Pakets
;         result-string  - Ergebnis der Evaluierung
; effect: Schreibt das EVAL-SUCCESS-Kommando in den Output-Stream des Writers.
; value:  -
;@testcase
(defun write-eval-success (writer package-string multiple-value-list) ;(setf multiple-value-list '("1" "2") package-string ":my-package")
  (let (encoded-mv)
    (when (not multiple-value-list) (setf multiple-value-list (list "NIL"))) 
    (setf encoded-mv (mapcar #'string-to-base64-string multiple-value-list))
    (write-line-to-writer writer (format nil "~a ~a ~{~a~^ ~}" +token-ok+ package-string encoded-mv))))

; input:  writer - Ein protocol-writer Objekt
;         restart-list - Eine Liste mit Restart-Objekten
; effect: Schreibt das EVAL-ERROR-Kommando in den Output-Stream des Writers.
; value:  -
;@testcase
(defun write-eval-error (writer restart-assoc formatstring &rest args) ;(setf restart-list (list (find-restart 'abort)))
  (let (restarts-stringed error-stringed)
    (setf restarts-stringed (mapcar #'(lambda (restart-list)
                                        (list (first restart-list) (string-to-base64-string (format nil "~a" (second restart-list)))))
                                    restart-assoc))
    (setf error-stringed (string-to-base64-string (make-formatted-string formatstring args)))
    (write-line-to-writer writer 
                          (format nil "~a ~a~:[~; ~{~{~a ~a~}~^ ~}~]" +token-eval-error+ error-stringed restarts-stringed restarts-stringed))))

; input:  writer - Ein protocol-writer Objekt
;         formatstring - FORMAT-Funktion String
;         args - passende Argument zu formatstring
; effect: Schreibt das READ-ERROR-Kommando in den Output-Stream des Writers.
; value:  -
;@testcase
(defun write-read-error (writer formatstring &rest args)
  (write-line-to-writer writer (format nil "~a ~a" +token-read-error+ (string-to-base64-string (make-formatted-string formatstring args)))))

; input:  writer - Ein protocol-writer Objekt
;         package-list - Ein Liste von Package-Objekten
; effect: Teilt die Pakete aus package-list dem Client kodiert mit.
; value:  -
;@testcase
(defun write-package-list (writer package-list)
  (write-line-to-writer writer (format nil "~a ~a" +token-package-list+ (length package-list)))
  (mapc #'(lambda (pack)
            (write-line-to-writer writer (package-name pack))) package-list))

; input:  writer - Ein protocol-writer Objekt
;         function-symbols - Eine Liste von Funktions- oder Makrosymbolen
; effect: Die Funktions- / Makrosymbole werden an den Client gesendet.
; value:  -
;@testcase
(defun write-function-list (writer function-symbols)
  (write-line-to-writer writer (format nil "~a ~a" +token-function-list+ (length function-symbols)))
  (mapc #'(lambda (fs)
            (let ((documentation (function-documentation fs))
                  (arglist (function-arglist->string fs)))              
              (if (or (not documentation) (equalp (string-trim '(#\Space #\Tab #\Newline) documentation) ""))
                  (setf documentation (string-to-base64-string "NIL"))
                  (setf documentation (string-to-base64-string documentation)))
              (let ((*print-pretty* NIL))
                (write-line-to-writer writer (format nil "~a ~a~:[~; ~]~{~a~^ ~}" fs documentation arglist arglist)))))	
        function-symbols))

; input:  stream - Ein Stream Objekt
; effect: Schreibt das Terminierungssignal fuer IO-Ende in den uebergebenen Stream.
; value:  -
;@testcase
(defun write-io-termination (stream)
  (write-10-termination stream)
  (write-char (code-char 0) stream)
  (write-10-termination stream)
  (force-output stream))








