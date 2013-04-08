;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: User; Base: 10 -*-

(in-package :common-lisp-user)

;; Ripped from Load-Memoization.lisp by Peter Van Eynde, originally:

;;;===========================================================================
;;; (C) 1992 Marty Hall. Permission is granted for any use or modification
;;      of this code provided this notice is retained. Version of 8/93.
;;;===========================================================================

;;; Defines the package Memoization is done in. Only the things that are 
;;; exported are needed by users of memoization. CLtL/2 users will want
;;; to use the more convenient "defpackage".

(defpackage :Memoization
	(:nicknames :Memo)
	(:use :Common-Lisp)
	(:export "DEFINE-MEMO-FUNCTION" "DEFINE-PRECALCULATED-MEMO-FUNCTION" "MEMOIZE"
	  "MEMOIZE-FUNCTIONS" "UNMEMOIZE" "UNMEMOIZE-FUNCTIONS"
	  "UNMEMOIZE-ALL-FUNCTIONS" "REMEMOIZE" "REMEMOIZE-FUNCTIONS"
	  "CLEAR-MEMO-TABLE" "CLEAR-MEMO-TABLES" "SAVE-MEMO-TABLE"
	  "MEMOIZED-FUNCTION-CALL-COUNT" "MEMOIZED-TIME"
	  "WITH-MEMOIZATION" "WITHOUT-MEMOIZATION"
	  "*MEMOIZED-FUNCTION-NAMES*"
	  "COMPILE-MEMOIZATION-SYSTEM" "LOAD-MEMOIZATION-SYSTEM"))

(in-package :Memoization)

;;; The directory that the saved memo-table files will sit in. This variable
;;; is only used by the routines in Save-Memo-Table.lisp that save/reload
;;; memo tables from disk. Thus, this only needs to be changed if those 
;;; capabilities are being used.
;;;
;;; The actual file name is assumed to be <Function Name>.<type> 
;;; where <type> is defined by *Source-File-Extension*, and <Function Name> 
;;; is the lowercase equivalent of the name of the LISP function. This may 
;;; need to be changed if you are using an OS that disallows filenames that 
;;; are legal LISP function names.
;;;   See the functions Memo-Table-Source-File and Memo-Table-Object-File.

(defvar *Memo-Table-Base-Pathname* "home:Memo-Tables;"
  "The directory where hash table values will get saved and looked up from.
   The filename will be the name of the associated memoized function name."
)
