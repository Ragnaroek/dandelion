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

;###########################################
;#
;# Build Script for Dandelion Server
;# Currently available for:
;#  - CLISP
;#  - SBCL
;# 
;###########################################

(in-package :cl-user)

(ql:quickload :dandelion-server)

;Where to put the binaries in
(defparameter *binary-path* "/Users/mb/tmp/env")

;Starten des Servers
;(dandelion-server:start-eval-server 31337 :serve-forever T)
;(dandelion-server:start-eval-server 31338 :serve-forever T :logging-enabled T :log-level :debug :layout 'log4cl:html-layout :log-type-args (list #p"/tmp/log-clisp.html"))

; input: -
; effect: Fuehrt ein Executable-Build des Servers durch.
; signal: error - wenn build fuer die Plattform nicht verfuegbar
; value:  -
(defun build ()
 #+clisp (ext:saveinitmem *binary-path* :init-function #'dandelion-main:main :executable T)
 #+sbcl (sb-ext:save-lisp-and-die *binary-path* :executable T :toplevel #'dandelion-main:main)
 #-(or clisp sbcl) (error "build for this platform not available"))

; input: -
; effect: Fuehrt ein Memoryimage-Build des Servers durch.
; signal: error - wenn mem-build fuer die Plattform nicht verfuegbar
; value:  -
(defun build-mem ()
 #+clisp (ext:saveinitmem *binary-path* :init-function #'dandelion-main:main)
 #+sbcl (sb-ext:save-lisp-and-die *binary-path* :toplevel #'dandelion-main:main) ;ungetestet
 #-(or clisp) (error "build for this platform not available"))


