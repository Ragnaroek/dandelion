;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: User; Base: 10 -*-

(in-package :User)

;;;===========================================================================
;;; (C) 1992 Marty Hall. Permission is granted for any use or modification
;;      of this code provided this notice is retained. Version of 8/93.
;;;===========================================================================

;;;===========================================================================
;;; This file is one of four files that define the Memoization facility:
;;;    - Load-Memoization [THIS FILE]: Defines Memoization package and loads 
;;;                                    other 3 files
;;;    - Memoization: Defines core memoization routines.
;;;    - Save-Memo-Table: Defines routines to save and load hash tables
;;;                       associated with a memoized function.
;;;    - Memoization-Examples: Simplistic version of memoization to illustrate
;;;                            the principle, and 2 example functions to which
;;;                            memoization can be applied.
;;;
;;; This file [Load-Memoization] defines the Memoization package, then loads
;;; the three other necessary files. The very first time memoization is added
;;; to your system, you should examine and possibly alter the five variables
;;; listed here and defined in the following code:
;;;   o *Memoization-Base-Directory* -- Directory this file is in
;;;   o *Source-File-Extension* -- ending for lisp source files
;;;   o *Compiled-File-Extension* -- ending for binary LISP files
;;;   o *Memo-Table-Base-Pathname* -- pathname to saved hash tables
;;;   o *Memoization-File-Names* -- Names of files that define this system
;;;
;;; Once they are defined, call (Compile-Memoization-System), which will
;;; create binary versions of the files in *Memoization-File-Names*. From 
;;; that time on, you can simply load this file to get the binary version
;;; of the automatic memoization facility. Do NOT compile this file.
;;;
;;; Marty Hall
;;; The Johns Hopkins University Applied Physics Lab
;;; Room 7-38
;;; Johns Hopkins Rd.
;;; Laurel MD 20723
;;; hall@aplcenmp.apl.jhu.edu
;;; (410) 792-6000 x3440
;;;===========================================================================

;;;===========================================================================
;;; Defines the package Memoization is done in. Only the things that are 
;;; exported are needed by users of memoization. CLtL/2 users will want
;;; to use the more convenient "defpackage".

(unless (find-package :Memoization)
  (make-package :Memoization
		:nicknames '(:Memo)
		:use '(:Common-Lisp)))

(in-package :Memoization)

;;;===========================================================================
;;; FIVE VARIABLE DEFINITIONS THAT MAY NEED TO BE ALTERED.

;;;--------------------------------------------------------------------------
;;; Home directory of the 4 memoization source files. THIS WILL DEFINITELY
;;; NEED TO BE CHANGED.

(defvar *Memoization-Base-Directory*
  (if (boundp 'common-lisp-user::*SMS-File-Prefix*)
      (concatenate 'string common-lisp-user::*SMS-File-Prefix* "Memoization/")
      #+cmu                    "/usr/src/cmucl/memoization-1.0/"
      #+:symbolics             "bugs:/shared/SMS/Code/Memoization/"
      #+:lucid                 "/shared/SMS/Code/Memoization/"
      #+:harlequin-common-lisp "/u2/instructors/hall/lisp/Memoization-1.0/"
      #+dec                    "/usr/users/instruct/hall/lisp/Memoization/"
      #+:akcl                  "/home/grad/hall/lisp/Memoization/")
  "The directory that the memoization files are stored in. This MUST
   be changed by the user when memoization is installed."
)

;;;--------------------------------------------------------------------------
;;; The source file extension, which is usually just .lisp. IF THIS IS NOT
;;; THE DEFAULT NAME FOR LISP SOURCE FILES, THEN CHANGE THIS TO .LSP (OR
;;; THE APPROPRIATE ENDING) **AND** RENAME THE FOUR MEMOIZATION SOURCE FILES
;;; FROM XXX.LISP TO XXX.LSP (OR WHATEVER ENDING).

(defvar *Source-File-Extension* ".lisp"
  "The file suffix for Lisp source files")

;;;--------------------------------------------------------------------------
;;; Extension for binary files. ADD AN ENTRY IF THE APPROPRIATE ONE DOES NOT
;;; ALREADY APPEAR.

(defvar *Compiled-File-Extension*
	#+(and :lucid :sun)             ".sbin"
        #+cmu                           ".x86f"
	#+:imach                        ".ibin"
	#+(and :symbolics (not :imach)) ".bin"
	#+:harlequin-common-lisp        ".wfasl"
	#+dec                           ".fas"
	#+:akcl                         ".o"
	
	"The file suffix for compiled Lisp files"
)

;;;--------------------------------------------------------------------------
;;; Names of other three files besides Load-Memoization. This doesn't 
;;; normally require changing.

(defvar *Memoization-File-Names* '("Save-Memo-Table"
				   "Memoization"
				   "Memoization-Examples")
  "The names of the files that need to be loaded in the memoization system")

;;;--------------------------------------------------------------------------
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

(defvar *Memo-Table-Base-Pathname*
 (concatenate 'string *Memoization-Base-Directory* "Memo-Tables/")
  "The directory where hash table values will get saved and looked up from.
   The filename will be the name of the associated memoized function name."
)

;;;===========================================================================
;;; Loads all of the memoization system. Binary if it exists, otherwise source.
;;; If you are using a package that does (use-package :Memoization), then the
;;; printout at the bottom can be removed.

(defun Load-Memoization-System ()
  "Loads the files in the memoization system. Loads compiled ones if possible."
  (let (Base-Filename Source Binary)
    (dolist (File *Memoization-File-Names*)
      (setq Base-Filename 
	      (concatenate 'string *Memoization-Base-Directory* File)
	    Source (concatenate 'string Base-Filename *Source-File-Extension*)
	    Binary 
	      (concatenate 'string Base-Filename *Compiled-File-Extension*))
      (if
	(probe-file Binary)
	(load Binary)
	(load Source)) )
    (format t "~2%Memoization routines are in the MEMOIZATION package.~%~
                 Use (in-package :Memo) or~%~
                     (use-package :Memo <Package>) to use it.~2%")
))

;;;===========================================================================
;;; Compiles each of the files listed in *Memoization-File-Names*

(defun Compile-Memoization-System ()
  (dolist (File *Memoization-File-Names*)
    (locally
      (proclaim '(optimize (speed 3) (safety 1) (compilation-speed 0)))
      (compile-file (concatenate 'string
				 *Memoization-Base-Directory*
				 File
				 *Source-File-Extension*)) )))

;;;===========================================================================
;;; Once your own package is established, you may want to have an entry here 
;;; that has that package use-package :Memoization, or uncomment the line below
;;; that makes Memoization available in the :common-lisp-user package.

(let ((Symbols
	'(Define-Memo-Function Define-Precalculated-Memo-Function Memoize
	  Memoize-Functions Unmemoize Unmemoize-Functions
	  Unmemoize-All-Functions Rememoize Rememoize-Functions
	  Clear-Memo-Table Clear-Memo-Tables Save-Memo-Table
	  Memoized-Function-Call-Count Memoized-Time
	  With-Memoization Without-Memoization
	  *Memoized-Function-Names*
	  Compile-Memoization-System Load-Memoization-System)))
  (export Symbols :Memoization) )

(if (and (fboundp 'Memoize)
	 (fboundp 'Save-Memo-Table))
    (format t "~%Memoization routines already loaded, not reloading.")
    (Load-Memoization-System))

;;;===========================================================================
