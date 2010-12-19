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
;; File		     - load.lisp
;; Description	 - Load-Skript fuer Eval-Server
;; Author	     - Michael Bohn
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defun this-files-pathname ()
  "Returns the directory pathname of the file currently being loaded."
  (car
   (list
    #+allegro excl:*source-pathname*
    #+lucid  lucid::*source-pathname*
    #+(or mcl openmcl)  (parse-namestring ccl::*LOADING-FILE-SOURCE-FILE*)
    #+clisp common-lisp::*load-pathname*
    #+sbcl *load-pathname*
    #+(and :coral (not mcl)) (car ccl::*loading-files*)
    #+genera (concatenate 'string
	       (host-namestring sys:fdefine-file-pathname)
           ":"
           sys:fdefine-file-pathname
           )
    #+next  *source-pathname*
    #+lispworks (lw:current-pathname)
    #-(or openmcl lucid :coral genera next mcl clisp allegro lispworks sbcl)
    (error "this-files-pathname not implemented"))))


; input:  file - Name der Datei
;         &key 
;         relpath - Relative Pfadangabe, als Liste von Strings
;         type - Dateityp (Endung)
; effect: Erstellt eine absolute Pfadangabe fuer die uebergebene Datei, relativ zur aktuellen Datei.
; value:  Absolute Pfadangabe zur Zeit, relativ zur aktuellen Datei
(defun make-load-pathname (file &key (relpath nil) (type "lisp"))
  (unless (listp relpath) (setf relpath (list relpath))) ;pfad normalisieren
  (unless file (setf type nil))
  (merge-pathnames (make-pathname :device nil :directory (cons :relative relpath) :name file :type type :defaults (this-files-pathname)) 
                   (make-pathname :directory (pathname-directory (this-files-pathname)))))

;###########################################
;#
;# Einstellungsvariablen
;#
;###########################################

;Pfad fuer die Erstellung der executable und mem-Dateien
(defparameter *binary-path* "/Users/mb/tmp/env")

;Test laden ja/nein
(defparameter *load-tests* NIL)

;Workaround: Fehler Maximum error nesting depth exceeded
#+sbcl (setf *terminal-io* (make-two-way-stream *standard-input* *standard-output*))
;SBCL Sockets laden
#+sbcl (require :sb-bsd-sockets)

;###########################################
;#
;# Load Evalserver
;#
;###########################################

;Laden externe libs
(load (make-load-pathname "base64" :relpath "lib"))
(load (make-load-pathname "load" :relpath '("lib" "cl-ppcre")))

;Laden ASDF-System
(load (make-load-pathname "asdf" :relpath "lib"))
(setf asdf:*central-registry* (list *default-pathname-defaults* (make-load-pathname nil :relpath '("lib" "log4cl"))))

;;; Laden CLOCC
(setf *clocc-root* (namestring (make-load-pathname nil :relpath '("lib" "clocc"))))
;(compile-file (make-load-pathname "clocc" :relpath '("lib" "clocc")))
;(compile-file (translate-logical-pathname "clocc:src;defsystem;defsystem"))
(load (make-load-pathname "clocc" :relpath '("lib" "clocc")))
(load (translate-logical-pathname "clocc:src;defsystem;defsystem"))
(dolist (l '("clocc:src;port;"
             "clocc:src;port;configuration;" "clocc:src;port;environment;"))
  (mk:add-registry-location (translate-logical-pathname l)))
(load (translate-logical-pathname "clocc:src;port;port.system"))
(mk:oos "port" :compile)

;;; Laden Log4CL (mittels asdf)
(asdf:operate 'asdf:load-op 'log4cl)

;Laden src
(load (make-load-pathname "utils" :relpath "src"))
(load (make-load-pathname "meta" :relpath "src"))
(load (make-load-pathname "protocol" :relpath "src"))
(load (make-load-pathname "eval-server" :relpath "src"))
(load (make-load-pathname "main" :relpath "src"))

;Starten des Servers
;(de.fh-trier.evalserver:start-eval-server 31337 :serve-forever T :logging-enabled T :log-level :debug :layout 'log4cl:html-layout :log-type-args (list #p"/tmp/log.html"))
;(de.fh-trier.evalserver:start-eval-server 31338 :serve-forever T :logging-enabled T :log-level :debug :layout 'log4cl:html-layout :log-type-args (list #p"/tmp/log-clisp.html"))

;###########################################
;#
;# Load Tests
;#
;###########################################

;Laden Lisp-Unit + Hilfsmodule
(load (make-load-pathname "lisp-unit" :relpath '("test" "lib")))
(load (make-load-pathname "lisp-unit-extensions" :relpath '("test" "lib")))
(load (make-load-pathname "utils" :relpath '("test" "lib")))

(when *load-tests*
  (lisp-unit:remove-all-tests)
  (load (make-load-pathname "test-meta" :relpath "test"))
  (load (make-load-pathname "test-protocol" :relpath "test"))
  (load (make-load-pathname "test-utils" :relpath "test"))
  (load (make-load-pathname "test-eval-server" :relpath "test"))
  (load (make-load-pathname "test-main" :relpath "test")))

;Funktion die alle definierten Tests ausfuehrt
;(run-suite)
(defun run-suite ()
  (when (not *load-tests*) (error "Tests not loaded"))
  (in-package #:de.fh-trier.test.evalserver.meta)
  (lisp-unit:run-tests)
  (in-package #:de.fh-trier.test.evalserver.protocol)
  (lisp-unit:run-tests)
  (in-package #:de.fh-trier.test.evalserver.utils)
  (lisp-unit:run-tests)
  (in-package #:de.fh-trier.test.evalserver)
  (lisp-unit:run-tests)
  (in-package #:de.fh-trier.test.evalserver.main)
  (lisp-unit:run-tests)
  (in-package #:cl-user))

;Tests Laden wenn Variable gesetzt


;###########################################
;#
;# Build
;# Bisher verfuegbar fuer:
;#  - CLISP
;#  - SBCL
;#
;###########################################

; input: -
; effect: Fuehrt ein Executable-Build des Servers durch.
; signal: error - wenn build fuer die Plattform nicht verfuegbar
; value:  -
(defun build ()
 #+clisp (ext:saveinitmem *binary-path* :init-function #'de.fh-trier.evalserver.main:main :executable T)
 #+sbcl (sb-ext:save-lisp-and-die *binary-path* :executable T :toplevel #'de.fh-trier.evalserver.main:main)
 #-(or clisp sbcl) (error "build for this platform not available"))

; input: -
; effect: Fuehrt ein Memoryimage-Build des Servers durch.
; signal: error - wenn mem-build fuer die Plattform nicht verfuegbar
; value:  -
(defun build-mem ()
 #+clisp (ext:saveinitmem *binary-path* :init-function #'de.fh-trier.evalserver.main:main)
 #+sbcl (sb-ext:save-lisp-and-die *binary-path* :toplevel #'de.fh-trier.evalserver.main:main) ;ungetestet
 #-(or clisp) (error "build for this platform not available"))


