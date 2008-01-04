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
;; File		     - eval-server.lisp
;; Description	     - Modul mit den Hauptfunktionen des Eval-Servers
;; Author	     - Michael Bohn
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO Anzahl Parameter Restart herausfinden
;TODO evalserver -> environment (nickname)

(in-package :cl-user)

(defpackage :de.fh-trier.evalserver
  (:use #:common-lisp 
        #:port
        #:log4cl
        #:de.fh-trier.evalserver.meta
        #:de.fh-trier.evalserver.protocol
        #:de.fh-trier.evalserver.utils)
  (:export #:start-eval-server
           #:shutdown-eval-server
           #:is-running
           #:*log-levels*))

(in-package #:de.fh-trier.evalserver)

;###########################################
;#
;# Konstanten + Globale Variablen
;#
;###########################################

(defconstant +up-message+ "SERVER_UP")

(defparameter *reader* nil) ;werden nur gebunden an Werte durch Makro in-read-write-environment
(defparameter *writer* nil)
(defparameter *io*     nil)

(defparameter *logger* nil) ; der Logger
(defparameter *log-levels* (list :debug :info :warning :error :fatal))
(defparameter *is-running* nil) ;Flag das angibt ob Server laueft
(defparameter *client-socket* nil) ;Der Socket zum Client - wenn verbunden
(defparameter *server-socket* nil) ;Der Server-Socket, global fuer Shutdown des Servers

;###########################################
;#
;# Conditions
;#
;###########################################

(define-condition abort-condition (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "user aborted"))))

;###########################################
;#
;# Makros
;#
;###########################################

; input:  stream - Stream-Objekt an das das Terminierungssignal gesendet werden soll (ausgewertet)
; effect: Stellt sicher das im nach verlassen des Funktionskoerpers das Terminierungssignal gesendet wird.
;         Insbesondere wird das Signal auch bei Fehlern und return-from Funktionsabbruch gesendet.
; value:  -
(defmacro ensure-termination-is-send (stream &body body)
  `(unwind-protect 
      (progn ,@body)
      (progn (write-io-termination ,stream) (eLog :info "Termination signal send"))))

; input:  reader - Protocol-Reader (ausgewertet)
;         writer - Protocol-Writer (ausgewertet)
;         io     - IO-Stream (ausgewertet)
; effect: Die globale Variablen *reader*, *writer* und *io* werden im Funktionskoerper an die jeweiligen Objekte gebunden.
;         Notwendig fuer Uebergabe der Objekte an die restart-redirect Funktion.
; value:  -
(defmacro in-read-write-environment (reader writer io &body body)
  `(let ((*reader* ,reader) (*writer* ,writer) (*io* ,io))
    ,@body))

; input:  packagevar - Package als String (ausgewertet)
;         writer - Protocol-Writer (ausgewerte)
;         success-form (ausgewertet)
;         error-form (ausgewertet)
; effect: Bindet die *package* Variable an das uebergebenen Paket. Wird das Paket nicht gefunden wird die Error-Form
;         ausgefuehrt. Bei erfolgreicher Bindung der *package* Variablen wird die Success-Form ausgefuehrt.
; value:  Wert der Success- oder Error-Form
(defmacro in-user-environment (packagevar writer success-form error-form)
  `(let ((*package* (find-package-from-string ,packagevar)))
     (cond ((not *package*) ,error-form)
           (T ,success-form))))

; input:  input - Stream (ausgewertet)
;         output - Stream (ausgewertet)
; effect: *standard-input* und *standard-output* werden an die jeweiligen Variablen gebunden.
;         Nach Verlassen des Funktionskoerpers wird die Bindung wieder aufgehoben.
; value:  -
(defmacro in-redirected-io-environment (input output &body body)
  `(let ((*standard-input* ,input) (*standard-output* ,output))
     ,@body))


;###########################################
;#
;# Funktionen
;#
;###########################################

;  Logging
;; debug:   detailierte Meldungen ueber einzelne Vorgaenge
;; info:    Jede evaluierte Form wird geloggt
;; warning: Fehler die vom Client ausgeloest wurden
;; error :  unerwartete Server-Fehler
;; fatal : -

;****************************
;; EXPORTIERTE Funktionen
;****************************

; input:  port - Portnummer auf der der Server lauschen soll
;         &key
;         server-forever - Nimmt neue Clientverbindungen entgegen nachdem der zuerst verbundene
;                          Client seine Session beendet (default: nil)
; effect: Startet den Eval-Server.
; value:  -
(defun start-eval-server (port &key (serve-forever nil) (logging-enabled nil) 
                               (log-type 'file-appender) (log-level :error) (layout 'html-layout) log-type-args)
  (setf *is-running* T)
  (init-protocol)
   
  (when logging-enabled ;(setf log-type 'file-appender log-level :debug layout 'html-layout log-type-args (list #p"/tmp/log.html"))
    (setup-logging log-type log-level layout log-type-args))
  
  (eLog :debug "Eval-Server started at ~a" (now-string))

  (setf *server-socket* (open-socket-server port))
  (unwind-protect
      (progn
        (send-startup)
        (loop do
              (eLog :debug "Waiting for client connect...")
              (start-handle-loop (setf *client-socket* (socket-accept *server-socket*)))
              while serve-forever))
    (shutdown-eval-server)))

; input:  -
; effect: Beendet den Eval-Server.
; value:  -
(defun shutdown-eval-server ()
  (when *client-socket*
    (close *client-socket*)
    (setf *client-socket* nil))
  (when *server-socket*
    (socket-server-close *server-socket*)
    (setf *server-socket* nil))
  (setf *is-running* NIL)
  (eLog :debug "Eval-Server shutdown at ~a" (now-string)))

; input:  -
; effect: Fragt den Zustand des Servers ab. Gibt T zurueck wenn der Server laueft.
; value:  T wenn Server laueft, sonst NIL
(defun is-running ()
  *is-running*)

; input:  -
; effect: Schreibt an den Standard-Output das Startup-Token
; value:  -
(defun send-startup ()
  (format T "~a~%" +up-message+))

;****************************
;; INTERNE Funktionen
;****************************

; input:  type - Typ des Logging (member '(file-appender))
;         level - Loggin-Level (member '(:debug :info :warning :error :fatal))
;         layout - Layout der Ausgaben (member '(simple-layout html-layout))
;         args - Parameter fuer den jeweiligen Logger-Typ
; effect: Initalisiert das Logging-Tool.
; value:  -
;@testcase
(defun setup-logging (type level layout args) ;(setf type :bla layout 'blubb level 'x)
  ;Argumente ueberpruefen
  (not-member->error type '(file-appender))
  (not-member->error level *log-levels*)
  (not-member->error layout '(simple-layout html-layout))
 
  (let (appender)
    (setf *logger* (make-instance 'logger :level level))
    (setf appender (make-instance type :file (first args)))
    (when (eql layout 'html-layout) ;workaround: zugriff nicht oeffentlich :(
      (setf (log4cl::appender-layout appender) (make-instance layout))) 
    (add-appender *logger* appender))
  nil)

; input:  item - Ein Objekt das in der Liste vorkommen soll
;         list - Liste die item enthalten soll
; effect: Enthaelt die uergebene Liste nicht das uebergebene item wird ein Error vom Typ 'type-error ausgeloest.
; value:  -
;@testcase
(defun not-member->error (item list) ;(setf item 'file-appender list '(file-appenender))
  (unless (member item list)
    (error 'type-error :datum item :expected-type list)))

; input:  level - Level unter dem die Meldung gelockt werden soll
;         formatstring - String, beliebiges Formatierung aus FORMAT-Funktion
;         args - Argumente passend zu formatstring
; effect: Loggt eine Meldung in dem konfigurierten Logger.
;         Wurde keine Logger konfiguriert (oder kein Logger ist gewuenscht) macht diese
;         Methode nichts.
;         Funktion log schon in cl-package deshalb Name eLog (EvalServer-Log)
; value:  -
(defun eLog (level formatstring &rest args)
  (when *logger*
    (not-member->error level *log-levels*)
    (let ((msg (make-formatted-string formatstring args)))
      (log4cl::log-message *logger* :debug msg))))

; input:  socket - Entgegengenommener Client-Socket.
; effect: Liest das Connect-Kommando vom Client und startet die Event-Schleife die die Anfragen eines einzelnen Clients behandelt.
;         Die Funktion kehrt erst zurueck wenn die Handle-Loop auslaueft (siehe Funktion handle-loop) 
;         oder das Connect-Kommando ungueltig ist.
; value:  -
(defun start-handle-loop (socket)
  (let ((reader (make-instance 'protocol-reader :input-stream socket))
        (writer (make-instance 'protocol-writer :output-stream socket))
        io-socket)
    (unwind-protect ;stellt sicher das die Stream in jedem Fall geschlossen werden
        (progn ;protected-form
          (eLog :debug "New Client connected at ~a : ~a" (now-string) (socket-string socket))

          ;Client Verbindungsversuch
          (handler-case (setf io-socket (connect-client reader)) ;Connect-Kommando vom Client lesen
            (malformed-protocol-error (se) (eLog :error "illegal connect: ~a" se) (return-from start-handle-loop)) ;Bei Fehler Schleife beenden
            (error (se) (eLog :error "unknown error: ~a" se) (return-from start-handle-loop)))
          
          ;Event-Loop
          (handler-case (handle-loop reader writer io-socket)
            (end-of-file (se) (eLog :error "handle loop unexpected exited: ~a" se))
            (error (se) (eLog :error "unknown error in handle loop: ~a" se)))
          
          (eLog :debug "Client disconnected at ~a" (now-string)))
        (progn ;clean-up form
          (when (input-stream reader) (close (input-stream reader))) ;;selber Socket wie Writer, aber trotzdem beides schliessen
          (when (output-stream writer) (close (output-stream writer)))
          (when io-socket (close io-socket))
          (when socket (close socket))))))


; input:  reader - Protocol-Reader von dem das Connect-Kommando gelesen werden soll
; effect: Das Connect-Kommando wird gelesen und die IO-Socket Verbindung zum Client hergestellt.
; signal: malformed-protocol-error - Wenn das Connect-Kommand ungueltig, der Client wird _nicht_ benachrichtigt.
; value:  Der verbundene IO-Socket
(defun connect-client (reader)
  (with-line-read reader line
    (let (connect-args)
      (setf connect-args (read-connect line))
      (open-socket (second connect-args) (third connect-args)))))

; input:  reader - ProtocolReader
;         writer - ProtocolWriter
;         socket-io - Verbundener IO-Socket
; effect: Die Event-Schleife wird ausgefuehrt. Die Schleife nur 'ordentlich' verlassen wenn das DISCONNECT Signal
;         gesendet wurde. Schliesst der Client die Verbindung ohne senden von DISCONNECT wird ein end-of-file signal
;         ausgeloest. Wird ein unbekanntes oder ungueltiges Kommando empfangen wird eine Fehlermeldung an den Client
;         gemeldet (mittels write-error) und der Request wird ignoriert.
; signal: end-of-file - Wenn der Client die Socketverbindung unerwartet schliesst.
; value:  -
(defun handle-loop (reader writer socket-io)
  (eLog :debug "handle loop entered")
  (loop
   (with-line-read reader line
     (let ((id (read-identifier line)))
       (eLog :info "handling request: ~a" line)
       (handler-case 
           (cond ((equalp id +token-eval+) 
                  (handle-eval-request reader writer socket-io line))
                 ((equalp id +token-packages+)
                  (handle-package-request writer))
                 ((equalp id +token-functions+)
                  (handle-symbol-request writer line :function))
                 ((equalp id +token-macros+)
                  (handle-symbol-request writer line :macro))
                 ((equalp id +token-disconnect+) 
                  (write-ok writer) (eLog :info "Client disconnects, stopping handle loop") (return T))) ;bei disconnect laeuft die handle-loop aus
         (malformed-protocol-error (se) (write-error writer "unknown-request ~a" (protocol-string se)) 
                                        (eLog :info "Illegal/Unknown command: ~a, ignoring request" line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Request Handler Funktionen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; input:  writer - ProtocolWriter    
; effect: Teilt dem Client alle zur Zeit definierten Pakete mit.
; value:  -
;@testcase
(defun handle-package-request (writer)
  (write-package-list writer (package-symbols)))

; input:  writer - ProtocolWriter
;         line - das gelesene FUNCTION-Kommando. Muss syntaktisch ok sein
; effect: Teilt dem Client alle zur Zeit definierten Pakete mit.
; value:  -
;@testcase
(defun handle-symbol-request (writer line type)
  (let* ((fr (if (eql type :function) (read-function-request line) (read-macro-request line)))
         (package (find-package (string-upcase (second fr)))))
    
    (if package 
        (write-function-list writer (get-symbols type package))
        (write-line-to-writer writer (format nil "~a no such package: ~a" +token-error+ (second fr))))))

; input:  type - Symboltyp, :function oder :macro
;         package - Package-Objekt aus dem die Symbole geholt werden sollen
; effect: Erstellt einer Liste von Symbole des angegebenen Typs aus package
; value:  List von Symbolen
;@testcase=test-handle-symbol-request
(defun get-symbols (type package)
  (cond ((eql type :function) (function-symbols package))
        (T (macro-symbols package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EVAL-Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; input:  reader - ProtocolReader
;         writer - ProtocolWriter
;         socket-io - Verbundener IO-Socket
;         string - Das gelesene EVAL-Kommando.
; effect: Verarbeitet einen EVAL-Request. Die Funktion kommuniziert solange mit dem Client bis er ein gueltige Form
;         liefert, entweder durch aufrufen eines oder mehrerer Restarts oder durch direktes senden einer Form die
;         bei der Evaluierung keine Restarts ausloest. Ein Abort der Evaluierung wird immer angeboten und ausloesen
;         dieses besonderen Restarts beendet die Evaluierung sofort.
; value:  -
(defun handle-eval-request (reader writer socket-io string)
  (let* ((eval (read-eval string))
         (package (second eval))
         (form (third eval))
         post-eval-package-string
         result-list)
    
   (ensure-termination-is-send socket-io ;stellt sicher das in jedem Fall, auch bei return-from termination-signal an io-stream gesendet wird
    
     ;;korrekte Form geliefert??
    (handler-case
        (in-user-environment package writer
                             (progn
                               (setf form (read-from-string (replace-return form) nil nil)))
                                   ;nur fehler bei ungueltigen Form, wird keine Form angegeben nil evaluieren
                                   ;wichtig fuer read-time-conditionals wie z.B.: #+clisp 'x. In clisp soll
                                   ;(quote x) gelesen werden, in lispworks z.B. nichts.
                             (progn (report-package-not-found writer package) (return-from handle-eval-request)))
      (error (se) ;wird ausgeloest bei illegalem Form String
        (eLog :info "malformed form: ~a" form)
        (write-read-error writer "error reading form: ~a" form)
        (return-from handle-eval-request)))

    (eLog :info "form read: ~a" form)

    ;;invariante: Form syntaktisch ok
    
    (handler-case (progn ;alle Fehler hier auffangen, bei Auswahl von Abort wird der Fehler nicht behandelt und
                         ;landet in diesem handler-case, Eval-Request wird dann verlassen
                    (in-read-write-environment reader writer socket-io
                      (handler-bind ((error #'restart-redirect))
                        (in-user-environment package writer
                                             (progn (in-redirected-io-environment socket-io socket-io
                                                      (setf result-list (result->string (multiple-value-list (eval form))))
                                                      (setf post-eval-package-string (get-shortest-package-name *package*))))
                                             (progn (report-package-not-found writer package) (return-from handle-eval-request))))))
      (abort-condition (condition) (write-ok writer) (eLog :info "User aborted restart") (return-from handle-eval-request))
      (error (condition) (eLog :error "Error condition-handle request ignored: ~a" condition)
             (return-from handle-eval-request)))
    
    ;;invariante: Form wurde evaluiert, evaluieren direkt erfolgreich oder durch aufruf eines oder mehrerer Restarts

    (eLog :info "Eval succesfully completed, result is: ~a" result-list))
   
   ;invariante: Terminierungssignal wurde gesendet
    (write-eval-success writer post-eval-package-string result-list)
    (eLog :debug "eval-handle done, result written")))


; input:  string - Ein String
; effect: Ersetzt alle #\return Zeichen in string destruktiv durch #\newline
; value:  uebergebener String
;@testcase
(defun replace-return (string)
  (dotimes (i (length string)) 
    (when (eql (char string i) #\return) 
      (setf (char string i) #\newline)))
  string)

; input:  package - Package-Objekt
; effect: Sucht den kuerzesten Paketnamen in der Liste der moeglichen Paketbezeichnungen.
;         Fuer das Standard-Paket COMMON-LISP-USER wird zuerst versucht das Paket CL-USER zu finden.
; value:  Der kuerzeste Paketname dieses Paketnamens als String
;@testcase
(defun get-shortest-package-name (package)
  (let ((namelist (cons (package-name package) (package-nicknames package))))
    (cond ((equalp (package-name package) "COMMON-LISP-USER") ;fuer CL-USER, wenn moegelich diese Abkuerzung nehmen
           (cond ((find "CL-USER" namelist :test #'equalp))
                 (T (find-shortest-string namelist))))
          (T (find-shortest-string namelist)))))

; input:  writer - Protocol-Writer
;         package - Paketname als String
; effect: Meldet dem Client das das Paket nicht gefunden wurde.
; value:  -
;@testcase=test-handle-symbol-request
(defun report-package-not-found (writer package)
  (eLog :error "package ~a not found" package)
  (write-read-error writer "package ~a not found" package))

; input:  form - Liste von Lisp-Forms
; effect: Wandelt jede Form in der List der Forms in einen String um.
; value:  Die Form als String.
;@testcase
(defun result->string (formlist)
  (mapcar #'(lambda (form)
              (cond ((stringp form)
                     (format nil "\"~a\"" form))
                    (T (prin1-to-string form)))) formlist))   ;(write-to-string form :escape nil :readably nil))))

; input:  condition - Vom System uebergeben.
; effect: Behandelt einen Fehler bei der Evaluierung. Alle verfuegbaren Restarts (Abort wird herausgefiltert) werden an den Client
;         gemeldet und es wird auf die INVOKE-RESTART Meldung des Clients gewartet. Der gewaehlte Restart wird ausgeloest.
; value:  -
(defun restart-redirect (condition)
  (let ((restarts (compute-restarts condition)) invoke-restart restart-selected identifier)
    (write-io-termination *io*) ;Client muss erst Output lesen, bevor Result gelesen werden kann. Deshalb hier terminierung senden
    (setf restarts (filter-restarts restarts)) ;aborts rausfiltern
    (setf restarts (unique-name-restart restarts))
    (write-eval-error *writer* restarts "~a" condition) ;restarts client mitteilen und auf antwort warten
    (with-line-read *reader* line
      (setf identifier (read-identifier line))
      (cond ((equalp identifier +token-abort+) ;abort request
             (error 'abort-condition)) ;Error-Signal wird weiter hoeher im Aufruf-Stack abgefangen
            ((equalp identifier +token-invoke-restart+) ;invoke-restart antwort
             (setf invoke-restart (read-invoke-restart line))
             (setf restart-selected (find-selected-restart restarts (second invoke-restart)))
             (call-invoke-restart restart-selected (third invoke-restart)))
            (T (error 'malformed-protocol-error :protocol-string line)))))) ;ungueltige antwort

; input:  restarts - Assoc-Liste wie geliefert von unique-name-restart
;         selected - Der vom Client gewaehlte Restart.
; effect: Sucht den gewaehlten Restart anhand von restart-name in der Liste der Restarts und gibt das
;         Restart-Objekt zurueck. Liefert NIL wenn kein passender Restart gefunden wird.
; value:  Der Restart mit restart-name selected, oder NIL wenn kein Restart mit uebergebenen Name gefunden
;@testcase
(defun find-selected-restart (restart-assoc selected)
  (second (assoc selected restart-assoc :test #'equalp)))

; input:  restarts - Liste von Restarts
; effect: Vergibt fuer jeden Restart einen eindeutigen Namen. Ist ein Restart-Name zweimal vergeben
;         wird eine laufende Nummer an den Namen gehangen
; value:  Assoc Liste Eintraege haben Format: (unique-restart-name restart-object)
;@testcase
(defun unique-name-restart (restarts)
  (let ((result nil) (unique-id 0))
    (mapc #'(lambda (restart)
                (let ((restart-name (symbol-name (restart-name restart))))
                   (cond ((assoc restart-name result :test #'equalp) ;Name bereits vergeben
                          (setf result (cons (list (format nil "~a-~a" restart-name (incf unique-id)) restart) result)))
                         (T (setf result (cons (list restart-name restart) result))))
                  )
                ) restarts)
    (nreverse result)))

; input:  restarts - Liste von Restarts
; effect: Filtert die Abort-Restarts aus einer Listen von Restarts.
; value:  Liste von Restarts, ohne Abort-Restarts.
(defun filter-restarts (restarts)
  (mapcan #'(lambda (restart) 
              (cond ((eql (restart-name restart) 'abort) nil)
                    (T (list restart)))) restarts))

; input:  restart - Ein Restart-Objekt
;         args - Argument fuer Aufruf des Restarts als String
; effect: Ruft invoke-restart mit dem uebergebenen Restart-Objekt auf. Die Argumentliste wird mittels read-from-string eingelesen.
; value:  -
(defun call-invoke-restart (restart args)
  (cond (args (apply #'invoke-restart restart (read-all-from-string args)))
        (T (invoke-restart restart)))) ;restart ohne args

; input: string
; effect: Sucht das Packet anhand dem uebergeben String.
; value:  NIL, wenn Paket nicht gefunden/vorhanden. package-object wenn Paket gefunden
;@testcase
(defun find-package-from-string (string)
    (find-package 
     (ignore-errors (read-from-string string)))) ;ignore-errors gibt nil zurueck wenn fehler auftritt, ansonsten das symbol
