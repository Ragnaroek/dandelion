;;; -*- Mode: Lisp -*-

;;; environment.lisp --
;;;
;;; Copyright (c) 2000-2005 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defgeneric version (environment-item)
  (:documentation "Wrapper for the various 'version' accessors."))

(defmethod version ((environment-item t))
  (error "Undefined VERSION operation for ~S." environment-item))

(defmethod version ((os operating-system))
  (operating-system-version os))

(defmethod version ((m machine))
  (machine-version m))

(defmethod version ((cl generic-common-lisp-implementation))
  (common-lisp-implementation-version cl))


(defmacro version-case ((item) &rest clauses)
  "The VERSION-CASE macro executes forms depending on the ITEM version.
The macro works as the *CASE Common Lisp forms (e.g. TYPECASE). The
macro is used as

     (CL.ENV:VERSION-CASE (<item>)
        (<version discriminant> <form>*)*)

where each <version discriminant> is either a string or a list of
strings.  The forms in first clause that 'matches' the version (currently the
test is done via STRING-EQUAL) are executed and the value(s) of the
last form executed is (are) returned.

This macro is provided because there is not easy and/or clear way to
provide an 'objectified' version denotation. Hence CLOS dispatching
cannot be used on version strings."

  (let ((version-var (gensym)))
    `(let ((,version-var (version ,item))) ; VERSION must be made a
					   ; READER in every class.
       (cond ,@(process-version-clauses version-var clauses)))))

(defun process-version-clauses (version-var version-clauses)
  ;; CASE-like clauses with FIRST a string or a list of strings.
  (flet ((process-version-clause (clause)
	   (destructuring-bind (discriminant &body forms)
	       clause
	     (etypecase discriminant
	       (string `((match-version ,version-var ,discriminant) ,@forms))
	       (list (if (every #'stringp discriminant)
			 `((or ,@(mapcar #'(lambda (discr)
					     `(match-version ,version-var
							     ,discr))
					     discriminant))
			   ,@forms)
			 (error "Malformed discriminant in version clause ~S."
				discriminant))))
	     )))
    (loop for clause in version-clauses
	  collect (process-version-clause clause))))


(defun match-version (item-version version-pattern)
  "Match a string version against a 'version-pattern.
For the time being this function is simply STRING-EQUAL.  It may
(will) evolve into something more complex."
  (string-equal item-version version-pattern))

;;; end of file -- environment.lisp --
