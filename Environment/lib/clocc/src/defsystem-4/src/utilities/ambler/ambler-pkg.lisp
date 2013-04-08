;;; -*- Mode: CLtL -*-

;;; ambler-pkg.lisp --
;;; Simple code ambler. Purely syntactic and without `environment'
;;; hacking.
;;; This is essentially an instantiation of the "Visitor Pattern"
;;; which, by means of CL multiple dispatching is readily implemented.
;;;
;;; This facility is inspied by the WALKER functionalities available
;;; in many implementations.  This facility does not implement all the
;;; features (mostly non-portable) available in many WALKER
;;; implementations, but at least it is (should be) very portable and
;;; usable.
;;;
;;; Please see the file COPYING for licensing information.
;;;
;;; Copyright (c) 2001-2002 Marco Antoniotti

(defpackage "CL.UTILITIES.AMBLER" 
  (:use "COMMON-LISP")
  (:nicknames "AMBLER" "ambler")

  (:export #:standard-ambling-context
	   #:copying-ambling-context
	   #:identity-ambling-context

	   #:*identity-ambling-context*
	   #:*copying-ambling-context*

	   #:amble-form
	   #:amble-expression

	   #:no-ambler-defined

	   #:def-expression-ambler
	   #:def-form-ambler
	   ))

;;; end of file -- ambler-pkg.lisp --
