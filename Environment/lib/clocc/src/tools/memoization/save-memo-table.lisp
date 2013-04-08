;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Memoization; Base: 10 -*-

(in-package "MEMOIZATION")

;;;===========================================================================
;;; (C) 1992 Marty Hall. Permission is granted for any use or modification
;;      of this code provided this notice is retained. Version of 8/93.
;;;===========================================================================

;;;===========================================================================
;;; This file is one of four files that define the Memoization facility:
;;;    - Load-Memoization: Defines Memoization package and loads other 3 files
;;;    - Memoization: Defines core memoization routines.
;;;    - Save-Memo-Table [THIS FILE]: Defines routines to save and load hash 
;;;                                   tables associated with memoized
;;;                                   function. 
;;;    - Memoization-Examples: Simplistic version of memoization to illustrate 
;;;                            the principle, and 2 example functions to which 
;;;                            memoization can be applied.
;;;
;;; This file does not need to be loaded if the capability to save memo tables
;;; to disk and reload them in a later session is not desired.
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
;;; Looks up the Memo-Table currently on the function, and generates a list of 
;;; all the keys and associated values in this current table. Then it uses
;;; that information to create a file that can be used to recreate the
;;; Memo-Table in a later session. Use 
;;;     (Define-Memo-Function Name (<Args>)
;;;       (:Hash-Table-Source :Disk)           <--- Add this
;;;       <Body>)
;;; to define a function that reloads this file, or call
;;; "Load-Saved-Memo-Table" directly.
;;;
;;; Writes the following into a file named <...Function-Name.lisp>, then 
;;; compiles that file:
;;;
;;;    ;;; - Mode: LISP ... [LISP Machine header]
;;; 
;;;    (in-package <Current Package Name>)    
;;;
;;;    (defparameter user::*Temporary-Hash-Table*
;;;                  (make-hash-table :size <Size> :test <Hash-Table-Test>))
;;;
;;;    (flet ((F (Key Value)
;;;             (setf (gethash Key user::*Temporary-Hash-Table*) Value)))
;;;      (F '<Key1> '<Value1>)
;;;      (F '<Key2> '<Value2>)
;;;      ...
;;;      (F '<KeyN> '<ValueN>) )     
;;;
;;; Note that when reloading the table, minimizing garbage may be an issue, so
;;; all the (setf (gethash <Key> Table) <Value>) are listed individually,
;;; rather than being collected into a list that some function is mapped over.
;;; This is just abbreviated via the flet to keep the file size (and thus
;;; saving and loading time) smaller.

(defun Save-Memo-Table (Function-Name &key (Size-Factor 1.5))
  "Saves (to disk) the hash table associated with a memoized function.
   The keyword SIZE-FACTOR determines how big to make the table relative to the
   number of entries saved."
  (let* ((Table (get Function-Name :Memo-Table))
	 (Size (hash-table-count Table))
	 (Filename (Memo-Table-Source-File Function-Name))
	 (Hash-Table-Test
	   (Hash-Table-Test-Name (get Function-Name :Memo-Table-Test))) )
    (with-open-file (File Filename :direction :output)
      (Print-File-Header File)
      (format File "(proclaim '(special common-lisp-user::*Temporary-Hash-Table*))~2%")
      (format File "(setq common-lisp-user::*Temporary-Hash-Table*~%")
      (format File "      (make-hash-table :size ~S :test #'~S))~2%"
	      (round (* Size-Factor Size)) Hash-Table-Test)
      (format File "(flet ((F (Key Value)~%")
      (format
	File
	"         (setf (gethash Key common-lisp-user::*Temporary-Hash-Table*) Value)))~%")
      (maphash #'(lambda (Key Value) (format File "  (F '~S '~S)~%" Key Value))
	       Table)
      (format File ")") )      ; Ends the flet
    (format t "~%Wrote ~S entries to ~A. Now compiling:"
	    Size (namestring Filename))
    (compile-file Filename)
    (values) ))

;;;===========================================================================
;;; Prints the header appropriate to the lisp version. An (in-package ...) 
;;; form works on any machine, but the LISP machine header is preferable on 
;;; Symbolics. This file cannot be in the memoization package since the
;;; argument list may contain symbols in other packages and you don't want
;;; to inflate the size with these package specifiers.

(defun Print-File-Header (File)
  "Puts a header in the file that sets the package"
  (let ((Pkg (package-name *package*)))
    (format
      File
      ";;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ~A; Base: 10 -*-~2%"
      Pkg)
  (format File "(in-package ~S)~2%" Pkg)
))
  
;;;===========================================================================
;;; Given a function corresponding to a hash table test, such as #'equal, this 
;;; returns its name, such as EQUAL. This would not be needed if the CLtL/2 
;;; construct "hash-table-test" were available in all implementations. This 
;;; also depends on the test being limited to a specified set of predicates, 
;;; and will fail on predicates accepted by a given implementation that are 
;;; not in this set.

(defun Hash-Table-Test-Name (Hash-Table-Test)
  "Given a function corresponding to a hash table test, returns its name"
  (cond
    ((eq Hash-Table-Test #'eq)     'eq)
    ((eq Hash-Table-Test #'eql)    'eql)
    ((eq Hash-Table-Test #'equal)  'equal)
    ((eq Hash-Table-Test #'equalp) 'equalp)
    (t (error "~S was not one of the legal hash table tests."
	      Hash-Table-Test)) ))

;;;===========================================================================
;;; See Load-Saved-Memo-Table below and Save-Memo-Table.

(defparameter common-lisp-user::*Temporary-Hash-Table* NIL
  "Temporary table to store hash table loaded from a saved file")

;;;===========================================================================
;;; This function is called from "Memo" if the :Restore-Old-Table? flag is
;;; set, returning a hash table. Normally accessed by using
;;; "Define-Precalculated-Memo-Function".

(defun Load-Saved-Memo-Table (Function-Name)
  "Loads file defining hash table that was saved in an earlier session"
  (Load-and-Compile-if-Needed (Memo-Table-Base-File Function-Name))
  common-lisp-user::*Temporary-Hash-Table*)

;;;===========================================================================
;;; Returns the pathname of the source file (presumably ...Foo.lisp)
;;; associated with the Function-Name Foo. It appears that simple string
;;; concatenation yields results as portable as using make-pathname.

(defun Memo-Table-Source-File (Function-Name)
  "Gives pathname to source file for table associated with function"
  (concatenate 'string
	       *Memo-Table-Base-Pathname*
	       (string-downcase (symbol-name Function-Name))
	       *Source-File-Extension*))

;;;===========================================================================
;;; Returns the pathname of the binary file (e.g. ...Foo.bin or ...Foo.fas)
;;; associated with the Function-Name Foo. If you are using
;;; "Load-and-Compile-if-Needed", then this routine is not used. However, on a
;;; single machine it might be preferable to simply compile the file when
;;; saving it then just load the binary version later without checking for
;;; recompilation. 

(defun Memo-Table-Object-File (Function-Name)
  "Gives pathname to binary file for table associated with function"
  (concatenate 'string
	       *Memo-Table-Base-Pathname*
	       (string-downcase (symbol-name Function-Name))
	       *Compiled-File-Extension*))

;;;===========================================================================
;;; Returns the pathname as a string without any file extension, as expected
;;; by load-and-compile-if-needed. Eg "FT:>Hall>Memo-Tables>foo"

(defun Memo-Table-Base-File (Function-Name)
  "Gives partial pathname, minus file type, for file storing table"
  (concatenate 'string
	       *Memo-Table-Base-Pathname*
	       (string-downcase (symbol-name Function-Name))))

;;;===========================================================================
;;; Returns a list of lists, where each sublist is a key and associated value.

(defun Key-Value-Pairs (Hash-Table)
  "Returns a list of (Key Value) lists from a hash table"
  (let ((KV-Pairs '()))
    (maphash #'(lambda (Key Value) (push (list Key Value) KV-Pairs))
	     Hash-Table)
    KV-Pairs ))

;;;===========================================================================
;;; Deletes the files associated with a Function-Name. If you are concerned
;;; about disk space from multiple versions of the files accumulating, you
;;; could add a call to this in the Save-Memo-Table routine.

(defun Delete-Memo-Table-Files (Function-Name)
  "Deletes source and binary files storing table associated with function"
  (let ((Source-File (Memo-Table-Source-File Function-Name))
	(Object-File (Memo-Table-Object-File Function-Name)))
    (if
      (probe-file Source-File)
      (delete-file Source-File))
    (if
      (probe-file Object-File)
      (delete-file Object-File)) ))

;;;===========================================================================
;;; Returns the time in universal time format (a big integer corresponding to
;;; the number of seconds since midnight, January 1, 1900, Greenwich Mean
;;; Time) at which the file was saved to disk.  The actual number is
;;; unimportant, but the bigger the number, the more recently created. 0 is
;;; returned if file does not exist. Function name changed from 
;;; File-Creation-Date since that conflicted with a system function in
;;; some LISP implementations.

(defun File-Creation-Time (Pathname)
  "Universal time when file was last modified"
  (if
    (null (probe-file Pathname))
    0
    (file-write-date Pathname)) )

;;;===========================================================================
;;; Given a pathname wo/ a file type (eg "FT:>Hall>Utility>Menu-Functions"),
;;; it returns t iff there is a source version of the file that is more recent
;;; than the binary version.  

(defun Compiled-Version-Outdated-p (Pathname-without-Filetype)
  "Predicate testing if source is more recent than binary"
  (> (File-Creation-Time (concatenate 'string
				      Pathname-without-Filetype
				      *Source-File-Extension*))
     (File-Creation-Time (concatenate 'string
				      Pathname-without-Filetype
				      *Compiled-File-Extension*))) )

;;;===========================================================================
;;; Loads the binary version of the pathname, compiling the lisp version first
;;; if the current binary version is outdated. Note that since the only type
;;; of files this is applied to are the data files that hold hash table 
;;; contents, size is the only real consideration, thus the somewhat unusual
;;; optimization parameters.

(defun Load-and-Compile-if-Needed (Pathname-without-Filetype)
  "Loads binary, recompiling if source is more recent than existing binary"
  (when
    (Compiled-Version-Outdated-p Pathname-without-Filetype)
    (format t "~%Compiling ~A" Pathname-without-Filetype)
    (locally (proclaim '(optimize (size 3) (compilation-speed 0)))
	     (compile-file (concatenate 'string
					Pathname-without-Filetype
					*Source-File-Extension*))))
  (load (concatenate 'string
		     Pathname-without-Filetype
		     *Compiled-File-Extension*)) )

;;;===========================================================================
