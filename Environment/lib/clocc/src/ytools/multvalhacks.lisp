;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(depends-on %module/ ytools)

;;;;(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
;;;;   (export ))

(defvar multvalhacks-break* false)

(out (:to *error-output*)
   "Loading file 'multvalhacks', which is no longer necessary."
   (:q (multvalhacks-break*
        (:e (breakpoint multvalhacks
	        "Care to find and eliminate the reference to it?"))))
   :% :%)
