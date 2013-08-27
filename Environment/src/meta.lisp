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
;; File		     - meta.lisp
;; Description	     - Definiert Funktionen fuer Zugriff auf Information ueber Makros, Funktionen, Packages
;; Author	     - Michael Bohn
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(in-package :cl-user)

(defpackage #:dandelion-meta
  (:use #:common-lisp 
        #:dandelion-utils)
  (:export #:package-symbols
           #:function-symbols
           #:macro-symbols
           #:function-name
           #:function-documentation
           #:function-arglist
           #:function-symbol-p
           #:macro-symbol-p
           #:macro
           #:function
           #:map-function-name
           #:function-arglist->string))

(in-package #:dandelion-meta)

; input:  -
; effect: Erstellt eine Liste aller bekannten Paket-Symbole und liefert diese als Liste zurueck.
; value:  Liste von Paket-Symbolen
;@testcase
(defun package-symbols ()
  (list-all-packages))

; input:  var - beliebiges Symbol
; effect: Praedikatsfunktion fuer Test auf Funktion. Gibt NIL zurueck wenn das uebergebene Symbol nicht
;         den Namen einer Funktion darstellt.
; value:  T, wenn var ein Funktionssymbol
;@testcase
(defun function-symbol-p (var)
  (and (fboundp var) (not (macro-function var))))

; input:  package - Package-Objekt
; effect: Erstellt eine Liste aller bekannte Funktionssymbole des uebergeben Paketes.
; value:  Liste von Funktionssymbolen aus Paket
;@testcase
(defun function-symbols (package)
  (let ((fsymbols nil))
    (do-external-symbols (var package)
      (when (function-symbol-p var) (setf fsymbols (cons var fsymbols))))
    fsymbols))

; input:  package - Package-Objekt
; effect: Erstellt eine Liste aller bekannte Makrosymbole des uebergeben Paketes.
; value:  Liste von Makrosymbolen aus Paket
;@testcase
(defun macro-symbols (package)
  (let ((msymbols nil))
    (do-external-symbols (var package)
      (when (macro-function var) (setf msymbols (cons var msymbols))))
    msymbols))

#+sbcl
(defun function-arglist-sbcl (func)
  (sb-introspect:function-lambda-list func))

#+clisp
(defun function-arglist-clisp (func)
  (cond ((and (function-symbol-p func) (not (special-operator-p func)))
         (sys::arglist func))
        ((macro-function func)
         (let ((macro-def (get func 'system::definition)))
               (if macro-def
                   (third (assoc 'defmacro macro-def))
                   (sys::arglist (macro-function func)))))
        (T nil)))

#+lispworks
(defun function-arglist-lispworks (func)
  (cond ((macro-function func) ;Bei Makro ohne Argument liefert Lispworks nicht NIL
         (let ((arglist (lw:function-lambda-list func))) 
           (if (or (not (listp arglist)) ;ab und zu ist arglist :dont-know!!!
                   (find 'DSPEC::%%MACROARG%% arglist)) ;wenn dieses Symbol vorkommt annehmen das es die rueckgabe fuer die NIL-Lambda list ist
               nil
               arglist)))
        (T (lw:function-lambda-list func))))

; input:  func - Ein Funktions- oder Makrosymbol
; effect: Gibt die zu diesen Symbol passenden Argumentliste zurueck.
; value-primary: Die Argumentliste
; value-mv1: Das symbol MACRO wenn Funktionssymbol ein Makro war, sonst FUNCTION
;@testcase
(defun function-arglist (func &aux arglist)
  (handler-case 
      (setf arglist
            #+clisp (function-arglist-clisp func)
            #+lispworks (function-arglist-lispworks func)
            #+sbcl (function-arglist-sbcl func)
            #-(or clisp lispworks sbcl) (error "function-arglist not implemented"))
    (error (se) (format T "~a" se) (setf arglist nil)))

    (unless (listp arglist) (setf arglist (list 'not-available)))
    (values arglist (if (function-symbol-p func) 'function 'macro)))

; input:  list - Liste von Symbolen, evtl. geschachtelt
; effect: Wandelt die Liste von Symbole rekursiv in einen String um.
; value:  String
;@testcase=test-function-arglist->string
(defun destructuring-argument->string (list &aux result)
  (setf result (mapcan #'(lambda (elem)
                           (cond ((consp elem) ;Liste in Liste
                                  (destructuring-argument->string elem)) ;rekursiv aufloesen
                                 ((symbolp elem)
                                  (list (symbol-name elem)))
                                 (T (list "ERROR")))) list))
  (append (cons "(" result) (list ")")))

; input:  list - Liste dessen erstes Argument ein Symbol ist
; effect: Wandelt eine Parameter init-form in einen String um, ist erstes Argument der Liste kein Symbole wird ("ERROR") zurueckgegeben
; value:  Argument als String-Liste, ("ERROR") wenn ungueltig
;@testcase=test-function-arglist->string
(defun init-form->string (list)
    (if (symbolp (first list))
        (list (symbol-name (first list)))
        (list "ERROR")))

; input:  fsymbol - Ein Funktions- oder Makrosymbol
; effect: Gibt die zu diesem Symbol passende Argumentliste (nicht geschachtelt) als Stringliste zurueck.
;         Destructuring-Parameter in Makroparameter werden passend umgewandelt.
; value:  Die umgewandelte Argumentliste.
;@testcase
(defun function-arglist->string (fsymbol &aux keyword-seen (fargs (multiple-value-list (function-arglist fsymbol))))
  (mapcan #'(lambda (farg)
              (when (member farg LAMBDA-LIST-KEYWORDS)
                (setf keyword-seen T))
              (cond ((consp farg) ;parameter symbol ist eine Liste
                     (if (and (not keyword-seen) (eql (second fargs) 'macro)) ;destructuring parameter
                           (destructuring-argument->string farg)
                           (init-form->string farg)))
                    ((symbolp farg)
                     (list (symbol-name farg)))
                    (T (list "ERROR")))) (first fargs)))

; input:  func - Funktionssymbol
; effect: Gibt den Namen der Funktion als String zurueck
; value:  String, Name der Funktion
;@testcase
(defun function-name (func)
  (symbol-name func))

; input:  var - beliebiges Symbol
; effect: Praedikatsfunktion fuer Test auf Makro. Gibt NIL zurueck wenn das uebergebene Symbol nicht
;         den Namen eines Makros darstellt.
; value:  T, wenn var ein Makrosymbol
;@testcase
(defun macro-symbol-p (var)
  (macro-function var))

; input:  func - Ein Funktionssymbol
; effect: Gibt den Dokumentationsstring des Funktionssymbols zurueck.
; value:  String, Dokumentation der Funktion
(defun function-documentation (func)
  (documentation func 'function))

; input:  func - Eine Funktion die einen Parameter erwartet
;         list - Eine Liste von Funktionssymbolen
; effect: Ruft fuer jedes Element der Liste func auf und uebergibt dieser Funktion den Funktionsnamen des Elements.
; value:  -
;@testcase
(defun map-function-name (func list)
  (mapc #'(lambda (fn)
              (funcall func (function-name fn))) list))