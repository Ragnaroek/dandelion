;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: nilscompat.lisp,v 2.1 2005/12/26 00:25:17 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on %module/ ytools)

;;; Compatibility with old NILs stuff

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(dreverse symbol->string symbol->fun
	     remove1q dremove1 dremove1q adjoinq complementq
	     lrecord selq nodupq prop is-fun-name one-value
	     string-elt
;;;;	     fx+ fx- fx* fx/ fx= fx< fx> fx=< fx>=
;;;;	     fl+ fl- fl* fl/ fl= fl< fl> fl=< fl>=
	     atan2 fxrandom flrandom cr list-elt
	     )))

(subr-synonym dreverse nreverse)
(subr-synonym symbol->string symbol-name)

(defun symbol->fun (s)
   (if (fboundp s)
       (symbol-function s)
       s))

(defun remove1q (x l) (remove x l :test #'eq :count 1))

(defun dremove1 (x l) (delete x l :count 1))
(defun dremove1q (x l) (delete x l :test #'eq :count 1))

(defun adjoinq (x l) (adjoin x l :test #'eq))
(defun complementq (l1 l2) (set-difference l1 l2 :test #'eq))
(defun nodupq (l) (remove-duplicates (the list l) :test #'eq)   )

(subr-synonym lrecord list)

(defmacro selq (&rest stuff) `(case ,@stuff))

(defun prop (ind sym) (get sym ind))

(defun (setf prop) (v ind sym) (setf (get sym ind) v))

(defun is-fun-name (x)
   (and (symbolp x)
        (fboundp x)))

(defmacro one-value (e) `(nth-value 0 ,e))

(defun string-elt (s i) (elt (the string s) (the fixnum i)))

#|
These probably make it harder, not easier, for a modern Lisp compiler
to optimize arithmetic
(declaim (inline fx+ fx- fx* fx/ fx= fx< fx> fx=< fx>=
                   fl+ fl- fl* fl/ fl= fl< fl> fl=< fl>=))

(defun fx+ (i j) (declare (fixnum i j)) (the fixnum (+ i j)))
(defun fx- (i j) (declare (fixnum i j)) (the fixnum (- i j)))
(defun fx* (i j) (declare (fixnum i j)) (the fixnum (* i j)))
(defun fx/ (i j) (declare (fixnum i j)) (the fixnum (truncate i j)))

(defun fl+ (i j) (declare #+:lcl4.0-bug-1 (optimize (speed 0))
			  (single-float i j))
   (the single-float (+ i j)))
(defun fl- (i j) (declare #+:lcl4.0-bug-1 (optimize (speed 0))
			  (single-float i j))
   (the single-float (- i j)))
(defun fl* (i j) (declare (single-float i j)) (the single-float (* i j)))
(defun fl/ (i j) (declare (single-float i j)) (the single-float (/ i j)))

(defun fx= (i j) (declare (fixnum i j)) (= i j))
(defun fx< (i j) (declare (fixnum i j)) (< i j))
(defun fx> (i j) (declare (fixnum i j)) (> i j))
(defun fx=< (i j)(declare (fixnum i j)) (<= i j))
(defun fx>= (i j)(declare (fixnum i j)) (>= i j))

(defun fl= (i j) (declare (single-float i j)) (= i j))
(defun fl< (i j) (declare (single-float i j)) (< i j))
(defun fl> (i j) (declare (single-float i j)) (> i j))
(defun fl=< (i j)(declare (single-float i j)) (<= i j))
(defun fl>= (i j)(declare (single-float i j)) (>= i j))
|#

(subr-synonym atan2 atan)

(subr-synonym fxrandom random)
(subr-synonym flrandom random)

(subr-synonym cr identity)

(defun list-elt (l i) (elt (the list l) i)   )

(defsetf list-elt (l i) (x) `(setf (elt (the list ,l) ,i) ,x)   )