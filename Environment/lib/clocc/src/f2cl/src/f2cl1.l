;; -*- Mode: lisp; Package: F2CL -*-
 ; f2cl1.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check
;functions:
;errorset - commented out
;	f2cl
;	concaten
;	f-to-l
;---------------------------------------------------------------------------
(in-package :f2cl)
(defvar *verbose* nil)
(defvar *comments* nil)
(defparameter *f2cl-version*
  (remove #\$ "2.0 beta $Date: 2006/12/21 03:42:11 $"))
 
(defvar *intrinsic-function-names*
  '(int ifix idint real float sngl dble cmplx dcmplx ichar char aint dint
    anint dnint nint idnint iabs abs dabs cabs mod amod dmod isign sign dsign
    idim dim ddim dprod max max0 max1 amax1 dmax1 amax0 amax1 min min0 amin1 dmin1
    min1 len index lge lgt lle llt aimag dimag conjg sqrt dsqrt csqrt 
    exp dexp cexp log alog dlog clog log10 alog10 dlog10 sin dsin csin
    cos dcos ccos tan dtan asin dasin acos dacos atan datan atan2 datan2
    sinh dsinh cosh dcosh tanh dtanh
    zsqrt
    cdabs dconjg
    )
  "A list of all the intrinsic functions in Fortran 77")

;;------------------------------------------------------------------------------
;; Define the Fortran types that we need.  This MUST match the types
;; given in macros.l, so be sure to keep this in sync!
(deftype logical ()
  `(member t nil))

;; Decide what you want integer*4 to be.  Good choices are fixnum or
;; (signed-byte 32).  The latter is good only if your compiler does a
;; good job with this type.  If you aren't sure, use fixnum.  CMUCL
;; does a good job with (signed-byte 32).
;;
;; If you change this, you may need to change some of the macros
;; below, such as INT and AINT!

#+(or cmu scl)
(deftype integer4 ()
  `(signed-byte 32))
#-(or cmu scl)
(deftype integer4 ()
  'fixnum)

(deftype integer2 ()
  `(signed-byte 16))
(deftype integer1 ()
  `(signed-byte 8))
(deftype real8 ()
  'double-float)
(deftype real4 ()
  'single-float)
(deftype complex8 ()
  `(complex single-float))
(deftype complex16 ()
  `(complex double-float))

(deftype array-double-float ()
    `(array double-float (*)))
(deftype array-integer4 ()
    `(array integer4 (*)))
(deftype array-single-float ()
    `(array single-float (*)))
(deftype array-strings ()
  `(array string (*)))
;;------------------------------------------------------------------------------

(eval-when (compile load eval)
  (proclaim '(special *sentable*))
  (proclaim '(special *external-function-names*
	      *undeclared_vbles* *declared_vbles* *implicit_vble_decls*
	      *subprog-arglist* *data-init*
	      *explicit_vble_decls* *function-flag* *key_params*
	      *save_vbles* *program-flag* *subprog_name*
	      *subprog_common_vars*  *common_array_dims*
	      *format_stmts* *current_label*
	      *subprog-stmt-fns* *subprog_stmt_fns_bodies* *prune_labels*
	      *auto-save-data*
	      *functions-used*
	      *vble-declaration-done*
	      ;; Specifies how Fortran arrays should be declared (array or simple-array)
	      *array-type*
	      ;; If non-NIL, treat all array references in calls to
	      ;; routines as a slice of the array.  Thus, we create a
	      ;; displaced array.  (Implies *array-type* is array.)
	      *array-slicing*
	      ;; If non-NIL, apply array slicing (in ID-FACTOR)
	      *apply-array-slice*
	      ;; If non-NIL, we are parsing the LHS of an assignment.
	      ;; (Used mostly so we don't incorrectly convert the
	      ;; definition of a statement function into a call of the
	      ;; function with mutliple-value-bind.)
	      *parsing-lhs*
	      ;; List of statement labels in a subprogram
	      *statement-labels*
	      )))

(defvar *common-blocks* (make-hash-table)
  "Hash table of all common blocks.  The key is the name of the common
block; the value is a list of all variables in the common block.")

(defvar *common-block-initialized* '())
(defvar *relaxed-array-decls* t
  "If T, array declarations with sizes are relaxed to be any size.")
(defvar *coerce-assignments* :never
  "This controls how assignment statements are coerced.  If T or
:always, assignments are always ccoerced.  If NIL or :never,
assignments are never coerced.  For any other value, coercion is done
only when needed. ")
(defvar *default-lisp-extension* "lisp"
  "The default extension (file type) for the output file name")

(defvar *fortran-extensions* '("f" "for")
  "A list of possible extensions for Fortran files.")

(defvar *declare-common-blocks* nil
  "When non-NIL, the structures for any common blocks are defined in this file")

(defvar *assigned-variables* nil
  "List of variables that are assigned a value")

(defvar *entry-points* nil
  "List of ENTRY points")

(defvar *f2cl-trace* nil)

(defvar *float-format* 'single-float)

(defvar *common-blocks-as-arrays* nil)

(defvar *use-function-info*
  t
  "If non-NIL, the database of function return values is used in
generating function calls and setting the argument values
correctly. If the database is incorrect, the function call will be
incorrect, so use caution.")

(defvar *equivalenced-vars* nil
  "List of equivalenced variables.  Each entry of the list is a list
  consisting of the equivalenced variables.")

;; For some reason Allegro needs this eval-when because it complains
;; about undefined slot accessors without this.
(eval-when (compile load eval)
(defstruct f2cl-finfo
  arg-types return-values calls)
)

;; Hash table of all known converted functions.  
(defvar *f2cl-function-info*
  (let ((table (make-hash-table)))
    ;; Insert d1mach and i1mach into the table
    (setf (gethash 'd1mach table)
	  (make-f2cl-finfo :arg-types '(integer4) :return-values '(nil)))
    (setf (gethash 'i1mach table)
	  (make-f2cl-finfo :arg-types '(integer4) :return-values '(nil)))
    table))

;; Hash table of all statement functions
(defvar *f2cl-statement-finfo* (make-hash-table))

(defun clear-f2cl-finfo ()
  (clrhash *f2cl-function-info*)
  (setf (gethash 'd1mach *f2cl-function-info*)
	(make-f2cl-finfo :arg-types '(integer4) :return-values '(nil)))
  (setf (gethash 'i1mach *f2cl-function-info*)
	(make-f2cl-finfo :arg-types '(integer4) :return-values '(nil)))
  *f2cl-function-info*)

(defun save-f2cl-finfo (path)
  "Save *F2CL-FUNCTION-INFO* to PATH.  This file can be reloaded
later to provide the function information for f2cl to compile functions
correctly"
  (with-open-file (s path :direction :output :if-exists :supersede)
    (maphash #'(lambda (key val)
		 (write `(setf (gethash ',key f2cl::*f2cl-function-info*)
			  ,val)
			:stream s
			:pretty t)
		 (terpri s))
	     *f2cl-function-info*)))
;----------------------------------------------------------------------------

(defun f2cl (input-file &key
	                output-file verbose prune-labels include-comments
			(auto-save t) (relaxed-array-decls t)
			(coerce-assigns :as-needed)
			(extension (or *default-lisp-extension* "lisp"))
			keep-temp-file
			(array-type :array array-type-p)
			(array-slicing t)
			(package :common-lisp-user)
			declaim
			declare-common
			(float-format *read-default-float-format*)
	                common-as-array
			&allow-other-keys)
  "Fortran to Common Lisp converter

INPUT-FILE               File containing Fortran code

:OUTPUT-FILE             File to contain Lisp code

:VERBOSE                 verbose output. Default = NIL.
:PRUNE-LABELS            Prune unused labels. Default = NIL.
:INCLUDE-COMMENTS        Include Fortran comments in the Lisp output (May be
                          buggy.)
                         Default = NIL
:AUTO-SAVE               Variables in DATA statements are automatically SAVE'd.
                         Default = T.
:RELAXED-ARRAY-DECLS     Declarations of array sizes are relaxed in formal
                          parameters
                         to functions. That is, any array length declarations
                          are ignored
                         if possible, like old Fortran used to. Default = T.
:COERCE-ASSIGNS          If T or :ALWAYS, all assignment statements
                          automatically coerce the RHS to the appropriate
                          type for the assignment.  If NIL or :NEVER,
                          coercion never happens.  Otherwise, coercion happens
                          as needed.
                          The Default = :AS-NEEDED
:EXTENSION               The extension to use for the output file, if needed.
                          Defaults to *DEFAULT-LISP-EXTENSION* or \"lisp\"
:KEEP-TEMP-FILE          If T, the temporary file is not deleted.
                          Default = NIL.
:ARRAY-TYPE              The type of array f2cl should use.  Should be
                          :simple-array or :array.
:ARRAY-SLICING           When non-NIL, f2cl assumes that, whenever we do an
                          array reference in a call to a subroutine or
                          function, we are really passing a subarray to the
                          routine instead of just the single value, unless
                          f2cl knows the function takes a scalar arg that
                          is not modified.
:PACKAGE                 A string or symbol specifying what package the
                          resulting code should be in. (Basically puts a
                          (in-package <p>) at the top.)
:DECLAIM                 Declaim compilation options  (Basically puts a
                          (declaim <declaim>) at the top.)
:DECLARE-COMMON          When non-NIL, any structures defintions for common
                          blocks are defined here. Otherwise, the structures
                          for the common blocks are expected to be defined
                          elsewhere.
:FLOAT-FORMAT            Float format to use when printing the result.
                          Default is *READ-DEFAULT-FLOAT-FORMAT*
:COMMON-AS-ARRAY         Common blocks are created as a set of arrays, and the
                          common block variables are offsets into the arrays.
                          This mimics Fortran common block layouts.
                          Default = NIL.
"


  ;;(format t "Copyright(c) 92-95 University of Waikato - all rights reserved~%")
  ;;(format t "1997, 1999 Many changes and fixes by Raymond Toy (toy@rtp.ericsson.se)~%")

  ;; Check (some) parameters for validity
  (assert (or (null coerce-assigns)
	      (member coerce-assigns '(:always :never :as-needed t))))
  (assert (member array-type '(:simple-array :array)))
  #+nil
  (when (and array-slicing array-type-p
	     (eq array-type :simple-array))
    (warn ":array-slicing is T, so specified :array-type of :simple-array is overridden"))
  (let ((*verbose* verbose)
	(*prune_labels* prune-labels)
	(*comments* include-comments)
	(*auto-save-data* auto-save)
	(*common-block-initialized* nil)
	(*relaxed-array-decls* relaxed-array-decls)
	(*coerce-assignments* coerce-assigns)
	(*array-type* (cdr (assoc array-type '((:simple-array . common-lisp:simple-array)
					       (:array . common-lisp:array)))))
	(*array-slicing* array-slicing)
	(*apply-array-slice* nil)
	(*declare-common-blocks* declare-common)
	(*assigned-variables* nil)
	(*entry-points* nil)
	(*equivalenced-vars* nil)
	(*float-format* float-format)
	(*common-blocks-as-arrays* common-as-array))

    (unless (probe-file input-file)
      ;; Can't find it, so look for some other possibilities
      (do* ((ext *fortran-extensions* (rest ext))
	    (path (merge-pathnames input-file (make-pathname :type (first ext)))
		  (merge-pathnames input-file (make-pathname :type (first ext)))))
	  ((or (null ext)
	       (probe-file path))
	   (setf input-file path))))
      
    (unless output-file
      (let ((input-path (pathname input-file)))
	(setf output-file (merge-pathnames
			   (make-pathname :host (pathname-host input-path)
					  :device (pathname-device input-path)
					  :directory (pathname-directory input-path)
					  :name (pathname-name input-path)
					  )
			   (make-pathname :type extension)))))
    (format t "~&;; ~S -> ~S~%" input-file output-file)
    (let ((processed-file (preprocess input-file)))
      (fortran-to-lisp processed-file output-file
		       :declaim declaim
		       :package package
		       :options `((:prune-labels ,prune-labels)
				  (:auto-save ,auto-save)
				  (:relaxed-array-decls ,relaxed-array-decls)
				  (:coerce-assigns ,coerce-assigns)
				  (:array-type ',array-type)
				  (:array-slicing ,array-slicing)
				  (:declare-common ,declare-common)
				  (:float-format ,float-format)))
      (unless keep-temp-file
	(delete-file processed-file))
      (values output-file))))

(defun f2cl-compile (filename &key
			      (output-file (compile-file-pathname filename))
			      error-file prune-labels include-comments
			      (auto-save t) (relaxed-array-decls t)
			      (coerce-assigns :as-needed)
			      (keep-lisp-file t)
			      (array-type :array)
			      (array-slicing t)
			      (package :common-lisp-user)
			      declaim
			      declare-common
			      (float-format *read-default-float-format*)
                              common-as-array
			      &allow-other-keys)
  "Convert the Fortran to Common Lisp and compile the resulting Lisp file

FILENAME                 File containing Fortran code

:OUTPUT-FILE             File to contain Lisp code

:VERBOSE                 verbose output. Default = NIL.
:PRUNE-LABELS            Prune unused labels. Default = NIL.
:INCLUDE-COMMENTS        Include Fortran comments in the Lisp output (May be buggy.)
                         Default = NIL
:AUTO-SAVE               Variables in DATA statements are automatically SAVE'd.
                         Default = T.
:RELAXED-ARRAY-DECLS     Declarations of array sizes are relaxed in formal parameters
                         to functions.  Default = T.
:COERCE-ASSIGNS          If T or :ALWAYS, all assignment statements automatically coerce the
                         RHS to the appropriate type for the assignment.  If NIL or :NEVER,
                         coercion never happens.  Otherwise, coercion happens as needed.
                         The Default = :AS-NEEDED
:KEEP-LISP-FILE          If T, the converted Lisp file is not deleted.  Default = NIL.
:ARRAY-TYPE              The type of array f2cl should use.  Should be 'simple-array or
                         'array.
:ARRAY-SLICING           When non-NIL, f2cl assumes that, whenever we do an array reference
                         in a call to a subroutine or function, we are really passing a
                         subarray to the routine instead of just the single value.
:PACKAGE                 A string or symbol specifying what package the result code
                         should be in. (Basically puts a (in-package <p>) at the top.)
                         Default:  :common-lisp-user.
:DECLAIM                 Declaim compilation options  (Basically puts a
                         (declaim <declaim>) at the top.)
:DECLARE-COMMON          When non-NIL, any structures for common blocks are declared here.
                         Otherwise, the structures for the common blocks are not declared.
:FLOAT-FORMAT            Float format to use when printing the result.  Default is
                         *READ-DEFAULT-FLOAT-FORMAT*
:COMMON-AS-ARRAY         Common blocks are created as a set of arrays, and the
                         common block variables are offsets into the arrays.
                         This mimics Fortran common block layouts.
                         Default = NIL.
"
  (let ((lisp-file
	 (f2cl filename :prune-labels prune-labels :include-comments include-comments
	       :auto-save auto-save :relaxed-array-decls relaxed-array-decls
	       :coerce-assigns coerce-assigns
	       :array-type array-type
	       :array-slicing array-slicing
	       :package package :declaim declaim
	       :declare-common declare-common
	       :float-format float-format
	       :common-as-array common-as-array))
	(*read-default-float-format* float-format))
    (multiple-value-prog1
	#+(or cmu scl) (compile-file lisp-file :output-file output-file :error-file error-file)
	#-(or cmu scl) (compile-file lisp-file :output-file output-file)
      (unless keep-lisp-file
	(delete-file lisp-file)))))
;---------------------------------------------------------------------------
(defun process-data (x) 
   (print x)
   (fortran-to-lisp 
     (concatenate 'string 
        "[.xnr]" (string-downcase (princ-to-string x)) ".for")
     (concatenate 'string 
        "[.lnr]" (string-downcase (princ-to-string x)) ".l")))
;-----------------------------------------------------------------------------  
; not the same as Senac's concat:
 (defun concaten (x &rest more-args)
  (intern
    (apply #'concatenate 'string
      (princ-to-string x) (mapcar #'princ-to-string more-args)) 
    (find-package :user)))
;----------------------------------------------------------------------------
  
; utilities
;	fortran-to-lisp
;       readsubprog-extract-format-stmts
;       translate-and-write-subprog
;	translate-line
;	setsyntax
;	single-macro-character
;	set-fortran-read
;	setlispread
;	lineread
;	read-six-chars
;	introduce-continue
;	find-do
;----------------------------------
; identifiers
;       id-definition-prog
;	id-definition-sub
;	id-definition-fun-typed
;	id-definition-fun
;	id-declaration
;	id-parameter
;       id-implicit-decl
;	id-assignment
;	id-subroutine-call
;	id-do-loop
;       id-pause
;       id-return
;       id-predicate
;	id-if
;	id-endif
;       id-if-goto
;	id-if-then
;	id-goto
;       id-continue
;-----------------------------------
; parsers
;       parse-prog-definition
;	parse-subr-definition
;       parse-typed-fun-definition
;       parse-fun-definition
;       parse-declaration
;       parse-implicit-decl
;	parse-parameter
;	parse-assignment
;	parse-expression
;       parse-pause
;	parse-subroutine-call
;	parse-do-loop
;	parse-if
;       parse-if-goto
;	parse-if-then
;	parse-return
;	parse-goto
;       parse-save
;       parse-common
;       parse-char-decl
;       parse-data
;       parse-data1
;	princ-reset
;-------------------------------------
; parsing utilities
;       extract-atoms
;       tail-chop
;       head-chop
;       list-split
;       gen-list-split
;       list-split-multi
;       list-split-bin
;       concat-operators
;       brackets-check
;	subsequence
;-------------------------------------
; matching
;	binding-value
;	variablep
;	variable-value
;	unify - returns fail or a bindings list/nil
;	match - returns multiple values or nil
;	maybe-extend-bindings
;------------------------------------------------------------------------------

(defun fortran-to-lisp (file ofile &key declaim package options)
  (let ((*package* (find-package :f2cl))
	*common_array_dims* *format_stmts* *statement-labels*)
    (when *verbose*
      (format t "beginning the main translation ...~%"))
    (with-open-file (inport file :direction :input)
      (with-open-file (outport ofile :direction :output 
			       :if-exists :rename-and-delete)
        (setq *common_array_dims* nil)
        (do ((char (peek-char nil inport nil 'eof) 
                   (peek-char nil inport nil 'eof)))
            ((eq char 'eof) )
          (setq *format_stmts* nil)
	  (setq *statement-labels* nil)
	  (let ((*print-level* nil)
		(*print-length* nil))
	    ;; Print a header to the file indicating when this was
	    ;; compiled and the version of f2cl used to compile it.
	    ;; Include the options used to compile the file.
	    (format outport ";;; Compiled by f2cl version ~A~%"
		    *f2cl-version*)
	    (format outport ";;; Using Lisp ~A ~A~%"
		    (lisp-implementation-type) (lisp-implementation-version))
	    (let ((*print-case* :downcase))
	      ;; Insert in-package and declaim, if needed
	      (when options
		(pprint-logical-block (outport options :per-line-prefix ";;; ")
		  (format outport "~&Options: ")
		  (write options :stream outport :case :downcase))
		(format outport "~2%"))
	      (when package
		(format outport "~&(in-package ~S)~%" (cond ((keywordp package)
							     package)
							    ((symbolp package)
							     (string package))
							    ((packagep package)
							     (package-name package))
							    (t
							     package))))
	      (when declaim
		(write (list 'declaim declaim) :stream outport :case :downcase))
	      (when (or package declaim)
		(format outport "~2%"))
	      ;;(format outport "~2&(use-package :f2cl)~2%")
	      )
	    (translate-and-write-subprog 
	     (introduce-continue
	      (readsubprog-extract-format-stmts inport))
	     outport))))))
  t)

;---------------------------------------------------------------------------

(defun single-macro-character (stream char)
  (declare (ignore stream))
  (let ((x (intern (string char)))) x))

(defvar *fortran-readtable*
  (let ((*readtable* (copy-readtable nil)))
    #+scl
    (ecase ext:*case-mode*
      (:upper (setf (readtable-case *readtable*) :upcase))
      (:lower (setf (readtable-case *readtable*) :downcase)))
    (flet ((setsyntax (x)
	     (set-macro-character x #'single-macro-character)))
      (setsyntax #\,)
      (setsyntax #\:)
      (setsyntax #\*)
      (setsyntax #\=)
      (setsyntax #\/)
      (setsyntax #\+)
      (setsyntax #\-)
      (setsyntax #\^)
      (setsyntax #\<)
      (setsyntax #\>)
      (setsyntax #\[)
      (setsyntax #\])
      (setsyntax #\()
      (setsyntax #\))
      (setsyntax #\{)
      (setsyntax #\})
      (setsyntax #\!)
      ;; reserved characters:
      (setsyntax #\$)
      (setsyntax #\@) 
      (setsyntax #\&) 
      (setsyntax #\~) 
      (setsyntax #\')
      (setsyntax #\|)
      (setsyntax #\`)
      ;; Why do we have to handle \ specially here?
      #+nil (setsyntax #\\)
      )
    *readtable*))

(defmacro with-fortran-syntax (&body body)
  `(let ((*readtable* *fortran-readtable*))
     ,@body))

;-----------------------------------------------------------------------------
#+nil
(defun readsubprog-extract-format-stmts (inport)
  (let (input-list output-list margin *current_label*)
    (when *verbose*
      (format t "~&extracting format statements ...~%"))

    (loop
	;; Get the left margin (contains line number)
	(setq margin (unless (eq (peek-char nil inport nil 'eof) 'eof)
		       (read-six-chars inport)))
	;; Set the label (line number), if any
	(setq *current_label*
	      (let ((label (read-from-string (coerce margin 'string) nil)))
		(if (integerp label)
		    label)))
        ;; Make sure we aren't hosed if we break out of this!
	(with-fortran-syntax
	  ;; read body of a line
	  (setq input-list (lineread inport))
	  ;; read newline character
	  (read-char inport nil 'eof t)
	  ;; (format t "~% input-list: ~S" input-list)
	  )
        ;; extract format-stmts
        ;;(format t "extract format-stmts~%")
	(if (eq (car input-list) 'format)
	    (parse-format (brackets-check (concat-operators input-list)))
	    (push (list *current_label* input-list) output-list))
	;; Check for end of subprogram
	(if (and (eq (car input-list) 'end) (null (cdr input-list)))
	    (return (nreverse output-list))))))

;; An extended DO statement is a DO statement without the following
;; line number.
(defun id-extended-do (line)
  (and (eq (first line) 'do)
       (not (numberp (second line)))
       (not (eq (second line) 'while))))

;; The DO-WHILE extension.  This looks something like
;;
;; DO <label> WHILE (<expr>)
;;
;; So we just make sure we have DO and WHILE and an optional label.
(defun id-do-while (line)
  (and (eq (first line) 'do)
       (or (eq (second line) 'while)
	   (and (numberp (second line))
		(eq (third line) 'while)))))

(defun rewrite-extended-do (label line)
  `(,(first line) ,label ,@(rest line)))

(defun rewrite-do-while (label1 label2 line)
  ;; "do while (cond)" becomes
  ;; "label1 if (.not. (cond)) goto <label2>"
  ;;
  ;; But
  ;;
  ;; "do <l1> while (cond) becomes
  ;; "label1 if (.not. (cond)) goto <l1>"
  (if (integerp (second line))
      `(if |(| not ,@(cdddr line) |)| goto ,label2)
      `(if |(| not ,@(cddr line) |)| goto ,label2)))

;; Find an END DO or ENDDO statement
(defun id-end-do (x)
  (or (eq (car x) 'enddo)
      (and (eq (car x) 'end)
	   (eq (cadr x) 'do))))

;; Find a WRITE statement where the format is not a statement number
;; but a string giving the format itself.
(defun id-write-format (input-list)
  (and (eq (car input-list) 'write)
       (stringp (fifth input-list))))

(defun readsubprog-extract-format-stmts (inport)
  (let ((extended-label 100000)		; Must be bigger than any possible valid Fortran label.
	(extended-do-label-stack '())
	input-list output-list margin *current_label*)
    (when *verbose*
      (format t "~&extracting format statements ...~%"))
    (loop
	;; Get the left margin (contains line number)
	(setq margin (unless (eq (peek-char nil inport nil 'eof) 'eof)
		       (read-six-chars inport)))
	;; Set the label (line number), if any
	(setq *current_label*
	      (let ((label (read-from-string (coerce margin 'string) nil)))
		(if (integerp label)
		    label)))
        ;; Add to list
        (when *current_label*
	  (push *current_label* *statement-labels*))
	;; Make sure we aren't hosed if we break out of this!
	(with-fortran-syntax
	  ;; read body of a line
	  (setq input-list (lineread inport))
	  ;; read newline character
	  (read-char inport nil 'eof t)
	  ;;(format t "~% input-list: ~S" input-list)
	  )
      ;; extract format-stmts
      ;;(format t "extract format-stmts~%")
	       
      (cond ((id-extended-do input-list)
	     ;; Handle extended DO statements.  These are DO
	     ;; statements that do not have a line number.  The DO
	     ;; statement is ended with an END-DO statement.
	     ;;
	     ;; We handle these by faking it. We create a standard
	     ;; DO statement with a new label.  The matching ENDDO
	     ;; statement is converted into a corresponding CONTINUE
	     ;; statement with the correct label.  The label is just
	     ;; an integer bigger than any allowable Fortran label
	     ;; (99999), since Fortran labels are limited to 5
	     ;; digits.

	     (push extended-label extended-do-label-stack)
	     (push `(,margin ,(rewrite-extended-do extended-label input-list))
		   output-list)
	     (incf extended-label))
	    ((id-do-while input-list)
	     ;; Handle do-while statements.
	     ;;
	     ;; We handle this by faking it.
	     ;;
	     ;; do while (cond)
	     ;;   <stuff>
	     ;; end do
	     ;;
	     ;; <label1> if (.not. (cond)) goto <label2>
	     ;;            <stuff>
	     ;;          goto <label1>
	     ;; <label2> continue
	     ;;
	     ;; So we need to push label1 and label2 onto our label
	     ;; stack.  Then rewrite the line with what we want.
	     (let ((label1 extended-label)
		   (label2 (if (integerp (second input-list))
			       (second input-list)
			       (incf extended-label))))
	       (incf extended-label)
	       (push (list label1 label2) extended-do-label-stack)
	       (push `(,label1 ,(rewrite-do-while label1 label2 input-list))
		     output-list)))
	    ((id-end-do input-list)
	     ;; The end of the extended DO statement.  This can either
	     ;; be the end of an extended do loop or a do-while loop.
	     ;; Figure out what do do.  For a do-loopk, 
	     (let ((label (pop extended-do-label-stack)))
	       (cond ((numberp label)
		      ;; We have a do-loop.  Convert to a standard
		      ;; continue statement with the correct label.
		      (push `(,label (continue))
			    output-list))
		     ((consp label)
		      ;; We have a do-while loop.  We need to replace
		      ;; this with "goto <label1>" and "<label2>
		      ;; continue".
		      (destructuring-bind (l1 l2)
			  label
			(push `(nil (goto ,l1)) output-list)
			(push `(,l2 (continue)) output-list))))))
	    ((id-write-format input-list)
	     ;; We have a write statement where the format isn't a
	     ;; label but a string.  Convert this to a label and add
	     ;; a format statement.
	     (let ((new `(write |(| ,(third input-list) |,| ,extended-label |)|
				,@(nthcdr 6 input-list))))
	       ;;(format t "new = ~S~%" new)
	       (push `(,margin ,new) output-list))
	     (setf *current_label* extended-label)
	     (incf extended-label)
	     ;; Convert the string format into the appropriate format
	     (let ((fmt (with-fortran-syntax
			    ;; read body of a line
			    (lineread (make-string-input-stream
				       (fifth input-list))))))
	       (setf input-list `(format ,@fmt))
	       (parse-format (brackets-check (concat-operators input-list)))))
	    ((eq (car input-list) 'format)
	     (parse-format (brackets-check (concat-operators input-list))))
	    (t
	     (push (list *current_label* input-list) output-list)))
	
      ;; Check for end of subprogram
      (when (and (eq (car input-list) 'end) (null (cdr input-list)))
	(when extended-do-label-stack
	  (warn "An extended DO statement is missing its matching ENDDO statement"))
	(return (nreverse output-list))))))

;------------------------------------------------------------------------------
(defun introduce-continue (prog-list) ; ((margin line) (margin line) ...)
 (prog (ret labels next rest)
  (setq labels (remove nil (mapcar #'find-do prog-list)))
  (setq next (first prog-list) rest (rest prog-list))
  loop
  (if (and (member (car next) labels)
           (multiple-do-labelp 
              (concatenate 'string (symbol-name :label)
                           (princ-to-string (car next))) labels))
      (setq ret (append  (list (list (car next) '(continue)))
                         (list (list nil (second next)))  ret))
      (setq ret (cons next ret)))
 (if (null rest) (return (reverse ret)))
 (setq next (car rest) rest (cdr rest))
 (go loop)))

(defun find-do (margin-line)
  (if (eq (caadr margin-line) 'do)
      (cadadr margin-line)
      nil))

;; Now look through the tree of code and replace every symbol in the
;; f2cl library with the symbol actually from the library.  (This is
;; currently quite expensive because we look through the entire tree
;; for every symbol.  We should traverse the tree just once and look
;; at each symbol to see if it needs replacing.  Should profile this
;; to see if this really matters.)

(defun fixup-f2cl-lib (tree)
  (do-external-symbols (lib-sym :f2cl-lib)
    (setf tree (subst lib-sym lib-sym tree
		      :test #'(lambda (a b)
				(and (symbolp a)
				     (symbolp b)
				     (string-equal a b))))))
  tree)

;------------------------------------------------------------------------------
; prog-list of form ((margin line) (margin line) ...)

#+nil
(defun maybe-nullify-returns (ret-values)
  ;; If the return values are not members of *assigned-variables*,
  ;; that means the var was never assigned, so we can return NIL
  ;; instead of the variable.
  (mapcar #'(lambda (v)
	      (if (member v *assigned-variables*)
		  v
		  nil))
	  ret-values))

(defun maybe-nullify-returns (fcn-name arg-names)
  ;; If the return values are not members of *assigned-variables*,
  ;; that means the var was never assigned, so we can return NIL
  ;; instead of the variable.
  (let ((arg-types nil)
	(ret-vals nil)
	(entry (gethash fcn-name *f2cl-function-info*)))
    (dolist (v arg-names)
      (push (if (member v *assigned-variables*)
		v
		nil)
	    ret-vals)
      )
    (setf ret-vals (nreverse ret-vals))
    (if entry
	(setf (f2cl-finfo-return-values entry) ret-vals)
	(setf (gethash fcn-name *f2cl-function-info*)
	      (make-f2cl-finfo :return-values ret-vals
			       :arg-types arg-types)))
    ret-vals))


(defun translate-and-write-subprog (prog-list outport)
  (clrhash *common-blocks*)
  (clrhash *f2cl-statement-finfo*)
  (let ((labels (remove nil (mapcar #'find-do prog-list))) ; labels is the do integers
	fort-fun *external-function-names*
	*undeclared_vbles* *declared_vbles* *implicit_vble_decls* *explicit_vble_decls*
	*save_vbles* *key_params* *subprog_common_vars* 
	*subprog-stmt-fns* *subprog_stmt_fns_bodies* *subprog_name*
	*function-flag* *program-flag*
	*subprog-arglist* *data-init* *functions-used* *vble-declaration-done*
	*parsing-lhs*
	*equivalenced-vars*
	)

     (setq fort-fun
	   (do ((lines prog-list (cdr lines)) 
		(fort-fun nil (append fort-fun
				      (append (translate-label (caar lines))
					      (translate-line 
					       (brackets-check (concat-operators
								(cadar lines))))))))
	       ((null (cdr lines))
		(append fort-fun
			(cond (*function-flag*
			       ;; A function.  Return the value of the
			       ;; function, the value of the variable
			       ;; with the same name as the function.
			       `(end_label
				 (return (values ,(cadr fort-fun)
						 ,@(mapcar #'first *entry-points*)
						 ,@(maybe-nullify-returns (second fort-fun) (caddr fort-fun))))))
			      ((or *program-flag*
				   (not (eq (car fort-fun) 'defun)))
			       ;; Return nil if this is the main
			       ;; program, indicated by either having
			       ;; a PROGRAM statement (*program-flag*
			       ;; set) or no DEFUN in FORT-FUN.
			       `(end_label (return nil)))
			      (t
			       ;; A subroutine definition.  Return the
			       ;; arguments of the subroutine.
			       `(end_label (return (values 
						    ,@(maybe-nullify-returns (second fort-fun) (caddr fort-fun)))))))))))
     ;; check for missing PROGRAM stmt
     (if (not (eq (car fort-fun) 'defun))
         (setq fort-fun (append '(defun *main* nil) fort-fun)))
     ;;     (format t "~%after translation : ~A" fort-fun)

     (multiple-value-bind (spec-proc spec-decl fun)
	 (insert-declarations (prune-labels (fix-structure fort-fun labels)))
       ;;(format t "spec-proc = ~S~%" spec-proc)
       ;;(format t "spec-decl = ~S~%" spec-decl)
       (when spec-proc
	 (mapc #'(lambda (s-proc s-decl)
		   (unless (find (caadr s-proc) *common-block-initialized*)
		     (setf s-proc (fixup-f2cl-lib s-proc))
		     (special-print s-proc outport)
		     (terpri outport)
		     (format outport "~%~%")
		     (setf s-decl (fixup-f2cl-lib s-decl))
		     (special-print s-decl outport)
		     (terpri outport)
		     (format outport "~%~%")
		     (pushnew (caadr s-proc) *common-block-initialized*)))
	       spec-proc
	       spec-decl)
	 (terpri outport))

       (setf fun (fixup-f2cl-lib fun))

       (special-print fun outport)
       (format outport "~2&(in-package #-gcl #:cl-user #+gcl \"CL-USER\")~%#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))~%")
       (let* ((*package* (find-package '#:cl-user))
	      (fname (second fort-fun))
	      (info (gethash fname *f2cl-function-info*)))
	 (write `(eval-when (:load-toplevel :compile-toplevel :execute)
		   (setf (gethash ',fname *f2cl-function-info*)
			 (make-f2cl-finfo :arg-types ',(f2cl-finfo-arg-types info)
					  :return-values ',(f2cl-finfo-return-values info)
					  :calls ',(f2cl-finfo-calls info))))
		:stream outport
		:pretty t
		:case :downcase
		:circle nil
		:readably t)))
     (print (cadr fort-fun))		;indicate which subprogram is being translated
     (write-char '#\newline outport)
     (write-char '#\newline outport)
     ))

;=============================================================================
(defun special-print (x o)
  (with-standard-io-syntax
    (let ((*package* (find-package :f2cl))
	  (*print-pretty* t)
	  (*print-case* :downcase)
	  (*print-circle* nil)
	  ;; Do we need this?  clisp basically ignores all other
	  ;; settings and tries very, very hard to make sure what's
	  ;; written is read back in exactly the same way.
	  #+nil (*print-readably* t)
	  (*read-default-float-format* *float-format*))
      (write x :stream o))))
;------------------------------------------------------------------------------
(defun princ-reset (x &optional (y ""))
  (print x)
  (princ y))

(defun translate-label (label)
   (if label 
       (list (read-from-string
                  (concatenate 'string (symbol-name :label) (princ-to-string label))
		  nil))
       nil))

;------------------------------------------------------------------------------

(defun translate-line (x)
  ;; x is a list being a line body-> list of lisp
  (prog (bindings)
     ;; reduce any DOUBLE PRECSION, REAL*8 etc to one word data types
     (when (member (car x) '(double real integer complex)) 
       (setq x (reduce-data-type x)))
     (when *verbose*
       (write x :case :downcase)
       (terpri))

     ;;return
     (when (id-return x)
       (return '((go end_label))))
     ;;continue
     (setq bindings (id-continue x))
     (when (not (eq bindings 'fail))
       (return `(continue_place_holder)))
     ;;definition
     (setq bindings (id-definition-prog-name x))
     (when (not (eq bindings 'fail))
       (return (parse-prog-definition bindings)))
     (setq bindings (id-definition-prog x))
     (when (not (eq bindings 'fail))
       (return (parse-prog-definition bindings)))
     (setq bindings (id-definition-sub x))
     (when (not (eq bindings 'fail))
       (return (parse-subr-definition bindings)))
     (setq bindings (id-definition-fun x))
     (when (not (eq bindings 'fail))
       (return (parse-fun-definition bindings)))
     (setq bindings (id-definition-fun-typed x))
     (when (not (eq bindings 'fail))
	
       (return (parse-typed-fun-definition bindings)))
     ;;goto/go to
     (when (id-comp-goto x)
       (return (parse-comp-goto x)))
     (when (id-comp-go-to x)
       (return (parse-comp-go-to x)))
     (when (id-assign x)
       (return (parse-assign x)))
     (when (id-assgn-goto x)
       (return (parse-assgn-goto x)))
     (setq bindings (id-goto x))
     (when (not (eq bindings 'fail))
       (return (parse-goto bindings)))
     (when (id-assgn-go-to x)
       (return (parse-assgn-go-to x)))
     (setq bindings (id-go-to x))
     (when (not (eq bindings 'fail))
       (return (parse-goto bindings)))
     ;;declarations
     (when (id-declaration x)
       (return (parse-declaration x))) 
     ;;parameters
     (when (id-parameter x)
       (return (parse-parameter x)))
     ;;implicit declarations
     (when (id-implicit-decl x)
       (return (parse-implicit-decl x)))
     ;;do loop
     (when (id-do-loop x)
       (return (parse-do-loop (check_new_vbles x))))

     ;; if-then
     (when (id-if-then x)
       (return (parse-if-then (check_new_vbles x))))
     ;; elseif (or else-if)
     (when (id-elseif x)
       (return `((elseif_place_holder 
		  ,(id-logical (cadr (check_new_vbles x)))))))
     (when (id-else-if x)
       (return `((elseif_place_holder
		  ,(id-logical (caddr (check_new_vbles x)))))))
     (when (id-else x)
       (return '((elseif_place_holder t))))
     (when (id-endif x)
       (return '(endif_place_holder)))

     ;;if
     (setq bindings (id-if-goto x))
     (when (not (eq bindings 'fail))
       (return (parse-if-goto bindings)))
     (when (id-if x)
       (return (parse-if (check_new_vbles x))))

     ;;assignments or statement functions
     (when (id-assignment x)
       (return (parse-assignment x)))
     ;;subroutine call
     (when (id-subroutine-call x) 
       (return (parse-subroutine-call x)))
     ;;pause
     (when (id-pause x)
       (return (parse-pause x)))
     ;;write
     (when (eq (car x) 'write)
       (return (parse-write x)))
     ;;print
     (when (eq (car x) 'print)
       (return (parse-print x)))
     ;; read
     (when (eq (car x) 'read)
       (return (parse-read x)))
       
     ;;format
     (when (eq (car x) 'format)
       (return (parse-format x)))
     ;;data
     (when (eq (car x) 'data)
       (return (parse-data x)))
     ;;save
     (when (eq (car x) 'save)
       (return (parse-save x)))
     ;;intrinsic
     (when (eq (car x) 'intrinsic)
       (return nil))
     ;;external
     (when (eq (car x) 'external)
       (return (parse-external x)))
     ;;common
     (when (eq (car x) 'common)
       (return (parse-common x)))
     ;;stop
     (when (eq (car x) 'stop)
       ;; Translate STOP as the stop function.
       (return `((f2cl-lib::stop ,(rest x)))))

     ;; Equivalence
     (when (eq (car x) 'equivalence)
       (return (parse-equivalence x)))
     
     ;; Handle Fortran comments that were converted by the
     ;; preprocessor to be (fortran_comment "comment string").
     ;;
     ;; Convert it to just a quoted string if possible.  Otherwise
     ;; just leave it as is.
     (when (eq (car x) 'fortran_comment)
       (return (if (cddr x)
		   (list x)
		   (list (list 'quote (second x))))))
     
     ;; (return (list '----> (check_new_vbles x)))

     (when (or (eq (car x) 'blockdata)
	       (and (eq (car x) 'block)
		    (eq (second x) 'data)))
       ;; A block data subprogram.  We cheat and pretend it's a
       ;; subroutine with a special name. If a name is given, use that
       ;; as part of the special name.  Be careful.  If we change the
       ;; name used here, we need to modify f2cl5.l to recognize the
       ;; new name.
       (let* ((name (cond ((eq (car x) 'blockdata)
			   (if (second x)
			       (string (second x))
			       nil))
			  ((eq (car x) 'block)
			   (if (third x)
			       (string (third x))
			       nil))
			  (t
			   nil)))
	      (bdname (if name
			  (intern (concatenate 'string (symbol-name '#:/blockdata-) name "/"))
			  '/blockdata/)))
	 (setq bindings (id-definition-sub `(subroutine ,bdname)))
	 (return (parse-subr-definition bindings))))

     #+nil
     (when (eq (car x) 'entry)
       (format t "ENTRY point: ~A~%" (car x))
       (format t " REST = ~A~%" (rest x))
       (push (rest x) *entry-points*)
       (return (list (second x))))
     (setf bindings (id-definition-entry x))
     (when (not (eq bindings 'fail))
       (return (parse-entry-definition bindings)))

     (when (eq (car x) 'open)
       (return (parse-open x)))

     (when (eq (car x) 'rewind)
       (return (parse-rewind x)))

     (when (eq (car x) 'close)
       (return (parse-close x)))
     
     ;;fall out the bottom:
     (warn "F2CL did not translate: ~S" (write-to-string (check_new_vbles x)))
     (return (list (list 'quote (concatenate ' string  "****NOT TRANSLATED: " 
					       (write-to-string (check_new_vbles x))))))
     ))
;------------------------------------------------------------------------------
;program 
(defun id-definition-prog-name (x) 
  (unify x '(program %name) nil))

(defun id-definition-prog (x) 
  (unify x '(program) nil))

;subroutine s(x,...)
(defun id-definition-sub (x) 
  (prog (bindings)
     (setq bindings (unify x '(subroutine %name %arg-list) nil))
     (if (eq bindings 'fail)
         (setq bindings (unify x '(subroutine %name) nil)))
     (return bindings)))

;type function f(x,...)
(defun id-definition-fun-typed (x) 
  (unify x '(%type function %name %arg-list) nil))

;untyped function f(x,...)
(defun id-definition-fun (x) 
  (unify x '(function %name %arg-list) nil))

(defun id-definition-entry (x)
  (unify x '(entry %name %arg-list) nil))

; continue
(defun id-continue (x) (unify x '(continue) nil))

;goto label
(defun id-goto (x)
  (unify x '(goto %label) nil))

(defun id-comp-goto-core (x)
  ;; (l1, l2, ...) [,] expr
  (let ((ex (remove '|,| x)))
    (and (listp (first ex))
	 (second x))))

(defun id-comp-goto (x)
  ;; goto (l1, l2, l3, ...) [,] expr
  (and (eq (first x) 'goto)
       (id-comp-goto-core (cdr x))))

(defun id-comp-go-to (x)
  ;; go to (l1, l2, l3, ...) [,] expr
  (and (eq (car x) 'go)
       (eq (cadr x) 'to)
       (id-comp-goto-core (cddr x))))

;go to label
(defun id-go-to (x)
 (unify x '(go to %label) nil))

(defun id-assgn-go-to-core (x)
  ;; var [,] [(l1, l2, ...)]
  (let ((expr (remove '|,| x)))
    (and (symbolp (first expr))
	 (listp (second expr)))))

(defun id-assgn-go-to (x)
  ;; go to var [,] [(l1, l2, l3, ...)]
  (and (eq (first x) 'go)
       (eq (second x) 'to)
       (id-assgn-go-to-core (cddr x))))

(defun id-assgn-goto (x)
  ;; goto var [,] [(l1, l2, l3, ...)]
  (and (eq (car x) 'goto)
       (id-assgn-go-to-core (cdr x))))

(defun id-assign (x)
  ;; assign num to var
  (and (eq (car x) 'assign)
       (eq (third x) 'to)))

;integer x,y,z,...
(defun id-declaration (x) 
  (member (first x) '(integer integer1 integer2 integer4
		      double real real8 character
		      complex complex8 complex16
		      logical dimension) :test #'eq))
;------------------------------------------------------------------------------
(defun id-if-then (x) (and (eq (car x) 'if) (member 'then x :test #'eq))) 
(defun id-if-goto (x) (unify x '(if %pred goto %label) nil)) 
;(defun id-if-assignment (x) (and (eq (car x) 'if) (member '= x :test #'eq))) 
;(defun id-if-pause (x) (and (eq (car x) 'if) (member 'pause x :test #'eq))) 

(defun id-if (x)
  ;; Is this an IF statement?
  (cond ((eq (car x) 'if)
	 ;; But "IF =" is definitely not.  Assume everything else is.
	 ;; This needs more work.
	 (not (eq (second x) '=)))
	(t
	 nil)))

(defun id-else (x) (eq (car x) 'else)) 
;; elseif can be written either as "ELSE IF" or "ELSEIF"
(defun id-else-if (x)
  (and (eq (car x) 'else)
       (cadr x)
       (eq (cadr x) 'if)))

(defun id-elseif (x)
  (eq (car x) 'elseif))

(defun id-endif (x) 
       (or (eq (car x) 'endif) 
           (and (eq (car x) 'end) (eq (cadr x) 'if)))) 

(defun id-assignment (x) (member '= x :test #'eq))
(defun id-subroutine-call (x) (eq (car x) 'call))
(defun id-do-loop (x) (eq (car x) 'do))

(defun id-pause (x) (eq (car x) 'pause))

(defun id-return (x) (and (eq (car x) 'return) (null (cdr x))))
(defun id-predicate (x) (id-logical x))

(defun id-implicit-decl (x) (eq (car x) 'implicit))
(defun id-parameter (x) (eq (car x) 'parameter))

;=============================================================================
; parsers

; program definition
(defun parse-prog-definition (bindings) 
   (setq *program-flag* t)
   (setq *subprog_name* (check-reserved-lisp-names (variable-value '%name bindings)))
   (list 'defun (if *subprog_name* *subprog_name* '*MAIN*)
;         (read-from-string 
;             (concatenate 'string "*MAIN*"
;                    (princ-to-string *subprog_name*)))
         nil
         ))

; subroutine definition
(defun parse-subr-definition (bindings) 
  (prog (arg-list)
   (setq *subprog_name* (check-reserved-lisp-names (variable-value '%name bindings)))
   (setq arg-list (mapcar #'check-reserved-lisp-names
			  (remove '|,| (variable-value '%arg-list bindings))))
   (setq *subprog-arglist* arg-list)
   (return (list 'defun *subprog_name* 
                    (if arg-list arg-list nil)))))

; typed function definition
(defun parse-typed-fun-definition (bindings)
  (setq *function-flag* t)
  (prog (fun-name)
   (setq fun-name (check-reserved-lisp-names (variable-value '%name bindings))
         *declared_vbles* (list fun-name)
         *explicit_vble_decls* 
              (list (list (convert-data-type (variable-value '%type bindings))
                          (list fun-name))))
   (setq *subprog-arglist* (remove '|,| (variable-value '%arg-list bindings)))
   (setf *subprog_name* fun-name)
   (return
   (list 'defun 
         fun-name 
         (remove '|,| (variable-value '%arg-list bindings))))))

; untyped function definition
(defun parse-fun-definition (bindings)
   (setq *function-flag* t)
   (setq *subprog-arglist* (remove '|,| (variable-value '%arg-list bindings)))
   (list 'defun 
         (setf *subprog_name*
	       (check-reserved-lisp-names (variable-value '%name bindings)))
         (remove '|,| (variable-value '%arg-list bindings))))


;; Entry point
(defun parse-entry-definition (bindings)
  (let* ((entry-name (variable-value '%name bindings))
	 (arglist (remove '|,| (variable-value '%arg-list bindings)))
	 (args (mapcar #'check-reserved-lisp-names arglist)))
    ;;(format t "parent = ~A~%" *subprog_name*)
    (push (list entry-name args *subprog_name*) *entry-points*)
    (list entry-name)))

;goto
(defun parse-goto (bindings) 
   `((go ,(read-from-string 
            (concatenate 'string (symbol-name :label)
                 (princ-to-string (variable-value '%label bindings))) 
            nil))) )
;------------------------------------------------------------------------------
(defun make-label (n) 
  (read-from-string (concatenate 'string (symbol-name :label) (princ-to-string n))))

(defun listn (a b)
  "Compute a list of integers from A to B, inclusive"
  (if (<= a b)
      (loop for x from a to b collect x)
      (loop for x from b downto a collect x)))

;------------------------------------------------------------------------------
(defun parse-comp-goto (x)
  ;; goto (l1, l2, l3, ...) [,] expr
  ;; Remove optional comma before the expression
  (let ((x (remove '|,| x)))
    (if (cddr x)
	`((computed-goto 
	   ,(mapcar #'(lambda (l) 
			(read-from-string 
			 (concatenate 'string (symbol-name :label) (princ-to-string l))))
		    (remove '|,| (second x)))
	   ,(third x)))
	`((computed-goto ,(second x))))))

(defun parse-comp-go-to (x)
  ;; go to (l1, l2, l3, ...) [,] expr
  ;; Remove the optional comma before the expression
  (let ((x (remove '|,| x)))
    (if (fourth x)
	`((computed-goto 
	   ,(mapcar #'(lambda (l) 
			(read-from-string 
			 (concatenate 'string (symbol-name :label) (princ-to-string l))))
		    (remove '|,| (third x)))
	   ,(fourth x)))
	`((computed-goto ,(caddr x))))))

;------------------------------------------------------------------------------
#+nil
(defun parse-assgn-go-to (x)
 (prog (labels len)
  (setq labels (remove '|,| (caddr x)))
  (setq len (length labels))
  (return
    `((case 
       ,(id-expression (cddddr x)) 
	,@(mapcar #'(lambda (x n) (list n (make-label x))) labels (listn 1 len)))))))

(defun parse-assgn-go-to (x)
  ;; go to var [,] [(l1, l2, ...)]
  (let* ((expr (remove '|,| x))
	 (labels (remove '|,| (fourth expr))))
    `((assigned-goto ,(third expr) ,(or labels (reverse *statement-labels*))))))
    
#+nil
(defun parse-assgn-goto (x)
 (prog (labels len)
  (setq labels (remove '|,| (cadr x)))
  (setq len (length labels))
  (return
    `((case 
       ,(id-expression (cdddr x)) 
       ,@(mapcar #'(lambda (x n)
		     (list n (make-label x)))
		 labels (listn 1 len)))))))

(defun parse-assgn-goto (x)
  ;; goto var [,] [(l1, l2, ...)]
  (let* ((expr (remove '|,| x))
	 (labels (remove '|,| (third expr))))
    `((assigned-goto ,(second expr) ,(or labels (reverse *statement-labels*))))))

(defun parse-assign (x)
  ;; assign num to var
  (let ((label (second x))
	(var (fourth x)))
    `((setf ,var ,label))))

;------------------------------------------------------------------------------

;declaration
(defun parse-declaration (x)		; x is the line
  (setq *declared_vbles*
	(append (mapcar #'(lambda (v)
			    (when (member (first v) *intrinsic-function-names*)
			      (warn "~A is being declared but is also the name of an intrinsic function"
				    (first v)))
			    (check-reserved-lisp-names (car v)))
			(list-split '|,|
			 (if (eq (cadr x) '*)
			     (cdddr x)
			     (cdr x))))
		*declared_vbles*))
  ;; If we declared an intrinsic function name, remove that from the
  ;; list of declared variables.

  ;; I don't think this is right.  Let the user declare the variable
  ;; and let us hope that the declaration wasn't for the intrinsic
  ;; function.  I'll need to check the Fortran 77 standard to see what
  ;; is supposed to happen here.
  #+nil
  (setf *declared_vbles*
	(set-difference *declared_vbles* *intrinsic-function-names*))

  (let ((type (find (first x) '((integer integer4)
				(integer4 integer4)
				(integer2 integer2)
				(integer1 integer1)
				(double double-float)
				(real single-float)
				(real8 double-float)
				(character #'parse-char-decl)
				(complex complex8)
				(complex8 complex8)
				(complex16 complex16)
				(logical logical)
				(dimension array))
		    :key #'car)))
    (when type
      (cond ((symbolp (second type))
	     (setq *explicit_vble_decls* 
		   (build_decl_list *explicit_vble_decls* (second type) (cdr x))))
	    ((consp (second type))
	     (funcall (second (second type)) x))
	    (t
	     (error "Failed to parse a declaration!")))))
  nil)

;; Parse an implicit declaration.  X is the whole statement.
(defun parse-implicit-decl (x)
  (cond ((eq (second x) 'none)
	 ;; implicit none
	 ;;
	 ;; I don't think we need to do anything here.  Besides, this
	 ;; causes problems later because there's no such Lisp type as
	 ;; :none.  By leaving this out, I think it means when we
	 ;; derive types we'll use the standard Fortran types.
	 ;; However, variables and such should already have been
	 ;; declared so we should be ok.  (We don't have to check
	 ;; because the Fortran code is supposed to be valid.)
	 #+nil
	 (setq *implicit_vble_decls* '((:none (a z))))
	 )
	(t
	 (let ((decls 
		(if (eq (third x) '*)
		    ;; Handle IMPLICIT <type> * <length> by rewriting x with
		    ;; the appropriate type.
		    `(,(first x) 
		      (,(second x) ,(third x) ,(fourth x))
		      ,@(cddddr x))
		    x)))
	   (mapc #'(lambda (y)
		     (push `(,(car (convert-data-type y))
			     ,@(list-split '|,|
					   (remove '- (car (last y)))))
			   *implicit_vble_decls*))
		 (list-split '|,| (cdr decls))))))
  nil)


(defun build_decl_list (decl_list type decl)
  (cons `(,type 
	  ,@(mapcar #'(lambda (y)
			`(,(check-reserved-lisp-names (car y))
			  ,@(parse_dimension_specs (cadr y))))
		    (list-split '|,| decl)))
	decl_list))

(defun parse_dimension_specs (specs)
  (if (null specs) nil
      (mapcar #'parse_upper_and_lower_bounds (list-split '|,| specs))))

;; Array bounds can be either (l:u) or (u).  However, U can also be
;; "*" to mean unbounded or unknown.
(defun parse_upper_and_lower_bounds (bds)
  (flet ((fixup-negative (expr)
	   ;; If a bound is negative, f2cl separates the negative sign
	   ;; from the expression.  This function replaces that
	   ;; expression with the actual negative number if the
	   ;; expression is, in fact, a number.
	   (cond ((and (eq '- (first expr))
		       (numberp (second expr)))
		  (list (- (second expr))))
		 (t
		  expr))))
    (let ((fixed-up-bds (mapcar #'fixup-negative (list-split '|:| bds))))
      ;; If a bound is "*", just return "*".
      (if (null (cdr fixed-up-bds))
	  (list 1 (if (eq (caar fixed-up-bds) '*)
		      '*
		      (id-expression (car fixed-up-bds))))
	  (list (if (eq (caar fixed-up-bds) '*)
		    '*
		    (id-expression (car fixed-up-bds)))
		(if (eq (caadr fixed-up-bds) '*)
		    '*
		    (id-expression (cdr fixed-up-bds))))))))


;; Try to coerce the RHS type to the LHS type as appropriate.  May
;; also include taking the realpart of the RHS.
(defun coerce-rhs-to-lhs (lhs-type rhs-type rhs)
  (cond ((or (eq t rhs-type)
	     (eq t lhs-type)
	     (subtypep rhs-type lhs-type))
	 ;; No coercion is needed if the types are compatible, or if
	 ;; we can't determine the type of the LHS or RHS.
	 rhs)
	((and (subtypep lhs-type 'integer)
	      (not (subtypep rhs-type 'integer)))
	 ;; We're trying to set a integer variable to non-integer
	 ;; value.  Use truncate.
	 `(int ,rhs))
	((and (subtypep rhs-type 'integer)
	      (subtypep lhs-type 'float))
	 ;; We're trying to set a float to an integer value.  Convert
	 ;; to a float.
	 `(coerce (the integer4 ,rhs) ',lhs-type))
	((and (subtypep lhs-type 'real)
	      (subtypep rhs-type 'complex))
	 ;; Assigning a complex to a real.  We need to take the real
	 ;; part.
	 `(coerce (realpart ,rhs) ',lhs-type))
	(t
	 ;; Haven't a clue, so coerce
	 `(coerce ,rhs ',lhs-type))))

;-----------------------------------------------------------------------------
; expression is algebraic with function calls:
(defun parse-assignment (x)
  ;; Make sure we don't mangle statement functions into
  ;; multiple-value-bind forms by saying we're parsing the LHS
  ;; now. When parsing the RHS, it's OK if we convert function calls
  ;; to a multiple-value-bind form.
  (let ((lhs (parse-expression (tail-chop '= x) t))
	(rhs (parse-expression (head-chop '= x) nil)))
    ;;(format t "lhs = ~A~%" lhs)
    ;;(format t "rhs = ~A~%" rhs)
    (cond ((listp lhs)
	   ;; Look for undeclared variables in the rhs
	   (check_new_vbles rhs)
	   (parse-arrayref-or-stmtfn lhs rhs))
	  (t
	   ;; Look for undeclared variables in the lhs or rhs.
	   (check_new_vbles (list lhs))
	   (check_new_vbles (list rhs))
	   (when (member lhs *subprog-arglist*)
	     ;;(format t "subprog-arglist = ~A~%" *subprog-arglist*)
	     ;;(format t "lhs = ~A~%" lhs)
	     (pushnew lhs *assigned-variables*))
	   (let* ((lhs-type (first (get-upgraded-fun-arg-type (list (list lhs)))))
		  (rhs-type (first (get-upgraded-fun-arg-type (list (list rhs))))))
	     #+nil
	     (progn
	       (format t "~&")
	       (format t "lhs = ~A, type ~A~%" lhs lhs-type)
	       (format t "rhs = ~A, type ~A~%" rhs rhs-type))
	     (cond ((subtypep lhs-type 'string)
		    ;; Strings need to be handled carefully
		    `((f2cl-set-string ,lhs ,rhs ,lhs-type)))
		   (t
		    (let ((new-rhs
			   (cond ((find *coerce-assignments* '(t :always))
				  `(coerce ,rhs (type-of ,lhs)))
				 ((find *coerce-assignments* '(nil :never))
				  rhs)
				 (t
				  (coerce-rhs-to-lhs lhs-type rhs-type rhs)))))
		      #+nil
		      (when (and (listp new-rhs)
				 (eq 'coerce (first new-rhs)))
			(format t "lhs, rhs types = ~A (~A) = ~A (~A) ~%      -> ~A~%"
				lhs lhs-type rhs rhs-type new-rhs))
		      `((setf ,lhs ,new-rhs))))))))))

(defun parse-arrayref-or-stmtfn (lhs rhs)
  ;;lhs is either an array ref or a statement function name
  (cond ((eq (car lhs) 'fref)
	 ;;array_ref 
	 (let* ((lhs-type (first (get-upgraded-fun-arg-type (list (list lhs)))))
		(rhs-type (first (get-upgraded-fun-arg-type (list (list rhs))))))
	   ;;(format t "~&")
	   ;;(format t "lhs = ~A, type ~A~%" lhs lhs-type)
	   ;;(format t "rhs = ~A, type ~A~%" rhs rhs-type)
	   (cond ((subtypep lhs-type 'string)
		  ;; Strings need to be handled specially
		  `((f2cl-set-string ,lhs ,rhs ,lhs-type)))
		 (t
		  (let ((new-rhs
			 (cond ((find *coerce-assignments* '(t :always))
				`(coerce ,rhs (type-of ,lhs)))
			       ((find *coerce-assignments* '(nil :never))
				rhs)
			       (t
				(coerce-rhs-to-lhs lhs-type rhs-type rhs)))))
		    `((setf ,lhs ,new-rhs)))))))
	((eq (car lhs) 'fref-string)
	 ;; Fortran string ref
	 `((fset-string ,lhs ,rhs)))
	(t
	 ;;statement_function
	 (let* ((lhs-type (first (get-upgraded-fun-arg-type (list (list (car lhs))))))
		(rhs-type (first (get-upgraded-fun-arg-type (list (list rhs)))))
		(rhs-expr (if (subtypep rhs-type lhs-type)
			      rhs
			      `(coerce ,rhs ',lhs-type))))
	   ;; Look up type of statement function and coerce the RHS to the desired type.
	   ;;(format t "statement func:  ~A~%" `((,(car lhs) ,(cdr lhs) ,rhs)))
	   ;;(format t "type = ~A, ~A~%" lhs-type rhs-type)
	   
	   ;; Add this function to the function database
	   (let ((arg-types (mapcar #'(lambda (a)
					(first (get-upgraded-fun-arg-type (list (list a)))))
				    (cdr lhs))))
	     ;;(format t "arg-types = ~A~%" arg-types)
	     (setf (gethash (car lhs) *f2cl-statement-finfo*)
		   (make-f2cl-finfo :arg-types arg-types :return-values (make-list (length arg-types)))))
	   
	   (setq *subprog-stmt-fns* (append *subprog-stmt-fns* (list (car lhs)))
		 *subprog_stmt_fns_bodies*
		 (append *subprog_stmt_fns_bodies* 
			 `((,(car lhs) ,(cdr lhs) ,rhs-expr))))
	   nil))))
;       (and (setq *subprog-stmt-fns* (append *subprog-stmt-fns* (list (car lhs))))
;            `((defun ,(read-from-string 
;                           (concatenate 'string 
;                                   (princ-to-string *subprog_name*)
;                                   (princ-to-string (car lhs))))
;                     ,(cdr lhs) ,rhs)))))

(defun parse-expression (x &optional (parse-lhs-p nil))
  (let ((*parsing-lhs* parse-lhs-p))
    (id-expression x)))

(defun parse-pause (x) 
   `((error ,(cadr x))))

(defun find-duplicates (list)
  (let ((dups '()))
    (do ((head list (rest head))
	 (tail (rest list) (rest tail)))
	((null tail))
      (when (and (atom (first head))
		 (member (first head) tail :test 'eq))
	(pushnew (first head) dups)))
    dups))


;; This routine takes the actual arglist and the saved function
;; information about the arg types and tries to convert array-slicing
;; to array reference if necessary.
;;
;; That is if the actual arg is an array slice, but the function wants
;; a simple variable, we convert the array slice to an array
;; reference.  I think this is really nice.
(defun massage-arglist (arglist info)
  ;;(format t "info = ~A~%" info)
  (let ((arg-types (f2cl-finfo-arg-types info))
	new-arglist)
    (do ((arg-type arg-types (cdr arg-type))
	 (arg arglist (cdr arg)))
	((null arg-type))
      ;;(format t "~A -> ~A~%" (car arg-type) (car arg))
      (cond ((and (symbolp (car arg-type))
		  (listp (car arg))
		  (eq (caar arg) 'array-slice))
	     ;; This means the expected arg type is a variable.  If
	     ;; the actual arg is is an array slice, we can replace
	     ;; the slice with the actual value.
	     (destructuring-bind (slice var type indices limits)
		 (car arg)
	       (push `(fref ,var ,indices ,limits)
		     new-arglist))
	     )
	    (t
	     (push (car arg) new-arglist))))
    (setf new-arglist (nreverse new-arglist))
    ;;(format t "new arglist = ~A~%" new-arglist)
    new-arglist))

;; Subroutines and functions can modify the values of input
;; parameters.  We handle this in Lisp by returning the input
;; parameters to the caller as multiple values.  This function
;; generates a multiple-value-bind to get the returned values and
;; assigns them to the input parameters if we can.
;;
;; If RETURN is T, we are generating a call to a function which
;; returns the value of the function in addition to the parameters.

(defun generate-call-to-routine (routine arglist &optional return)
  (multiple-value-bind (routine-name finfo)
      (if (eq (first routine) 'funcall)
	  (values routine nil)
	  (let ((checked (check-reserved-lisp-names (first routine))))
	    (values (list checked)
		    (gethash checked *f2cl-function-info*))))
    (if (and finfo (f2cl-finfo-return-values finfo))
	(let ((this (gethash *subprog_name* *f2cl-function-info*)))
	  (unless this
	    (setf (gethash *subprog_name* *f2cl-function-info*)
		  (make-f2cl-finfo)))
	  ;; Add this function to the list of called functions
	  (pushnew (first routine-name)
		   (f2cl-finfo-calls (gethash *subprog_name*
					      *f2cl-function-info*))))
	(warn "Generating call to unknown function ~A.  Check generated call!" routine-name))

    (let ((ret-finfo (if (and *use-function-info* finfo)
			 (f2cl-finfo-return-values finfo)
			 (make-list (length arglist) :initial-element :unknown))))

      ;; (format t "ret-info = ~A~%" ret-finfo)
      (multiple-value-bind (all-setters vnames)
	  ;; Figure out if and how to set the parameters to the returned values.
	  ;;
	  ;; Note that if the same arg appears more than once, we assume
	  ;; that only one use actually sets the variable.  If this is
	  ;; not true, I think the original Fortran was broken anyway.
	  ;; We don't check for this here.  Should we?  We can't in
	  ;; general unless *f2cl-function-info* tells us the return
	  ;; values for the function.
	  (do ((v nil)
	       (count 0 (+ count 1))
	       (vnames nil)
	       (args arglist (rest args))
	       (ret-info ret-finfo (rest ret-info)))
	      ((null args)
	       (values (nreverse v) (nreverse vnames)))
	    (let* ((arg (first args))
		   (var (intern (concatenate 'string (symbol-name '#:var-)
					     (princ-to-string count)))))
	      (push var vnames)
	      ;; (format t "arg = ~A~%" arg)
	      ;;(format t "ret-info = ~A~%" (first ret-info))
	      (push (cond ((or (vble-is-array-p arg)
			       (member arg *external-function-names*)
			       (member arg *intrinsic-function-names*)
			       (member arg '(%true% %false%)))
			   ;; Fortran can't return whole arrays or
			   ;; functions, so don't try to assign the value
			   ;; to arrays or functions.  Also, we don't want
			   ;; to assign to constants like %true% and
			   ;; %false%.
			   ;;
			   nil)
			  ((symbolp arg)
			   ;; A simple variable
			   (cond ((eq (first ret-info) :unknown)
				  ;; Don't know anything about this, so
				  ;; we need to check.  Also assume
				  ;; worst case that the variable is
				  ;; assigned to.
				  (pushnew arg *assigned-variables*)
				  `(when ,var (setf ,arg ,var)))
				 ((eq (first ret-info) nil)
				  ;; Definitely isn't returned
				  nil)
				 (t
				  ;; Definitely returned.  Update
				  ;; *assigned-variables* with this new
				  ;; arg.
				  (pushnew arg *assigned-variables*)
				  `(setf ,arg ,var))))
			  ((and (listp arg)
				(eq (first arg) 'fref))
			   ;; A reference to a single element of an array.
			   (cond ((eq (first ret-info) :unknown)
				  ;; Don't know anything about this
				  `(when ,var (fset ,arg ,var)))
				 ((eq (first ret-info) nil)
				  ;; Definitely isn't returned
				  nil)
				 (t
				  ;; Definitely is returned
				  `(setf ,arg ,var))))
			  (t
			   ;; This means it's either a number or some
			   ;; expression, so obviously we can't modify
			   ;; that.  (The original Fortran code was broken
			   ;; if the routine is trying to modify this.)
			   nil))
		    v)))
	;; (format t "all-setters = ~A~%" all-setters)
	(let ((ignored-vars
	       ;; A variable is ignored if no setter for it is given.
	       (remove nil (mapcar #'(lambda (varname setter)
				       (if (not setter)
					   varname))
				   vnames all-setters)))
	      (setters (remove nil all-setters)))
	  ;;(format t "ignored-vars = ~A~%" ignored-vars)
	  ;;(format t "setters = ~A~%" setters)
	  ;;(format t "arglist = ~A~%" arglist)
	  (when (and *use-function-info* finfo)
	    (setf arglist (massage-arglist arglist finfo)))
	  (if setters
	      `((multiple-value-bind ,(if return (append '(ret-val) vnames) vnames)
		    (,@routine-name ,@arglist)
		  (declare (ignore ,@ignored-vars))
		  ,@setters
		  ,@(if return (list 'ret-val))))
	      `((,@routine-name ,@arglist))))))))

;; Convert array references to an array slice, as appropriate. That
;; is, the array reference must be the actual arg. No arithmetic
;; allowed.  We assume that's true if FREF is the first element of the
;; arg expression.
#+nil
(defun maybe-convert-array-ref-to-slice (arg-list)
  (if (not *array-slicing*)
      arg-list
      (mapcar #'(lambda (expr)
		  ;;(format t "expr = ~A~%" expr)
		  (if (and (listp expr)
			   (eq (first expr) 'fref))
		      `(array-slice ,(second expr) ,(lookup-vble-type (second expr)) ,@(cddr expr))
		      expr))
	      arg-list)))

(defun maybe-convert-array-ref-to-slice (arg-list fun-name)
  (cond (*array-slicing*
	 ;; Look up the arg types for the function.  If we know the
	 ;; function, we can be smarter about generating the call.
	 ;;
	 ;; That is, if the actual arg is an array reference, we need
	 ;; to decide if we want just the selected element or a slice
	 ;; of the array.  If the declared type is a scalar, and we
	 ;; don't modify it, just use the array element.  Otherwise,
	 ;; we want a slice.
	 (let ((n-args (length arg-list))
	       (finfo (if (member fun-name *subprog-arglist*)
			  nil
			  (or (gethash fun-name *f2cl-statement-finfo*)
			      (gethash fun-name *f2cl-function-info*))))
	       arg-info ret-info)
	   #+nil
	   (when *subprog-arglist*
	     (format t "external = ~A~%" *subprog-arglist*)
	     (format t "finfo = ~A~%" finfo)
	     (format t "fun-name = ~A~%" fun-name))
	   ;;(format t "~A finfo = ~A~%" fun-name finfo)
	   (cond (finfo
		  (setf arg-info (f2cl-finfo-arg-types finfo))
		  (setf ret-info (f2cl-finfo-return-values finfo)))
		 (t
		  (setf arg-info (make-list n-args))
		  (setf ret-info (make-list n-args))))
	   ;;(format t "calling ~A~%" fun-name)
	   ;;(format t " finfo = ~A~%" finfo)
	   (mapcar #'(lambda (expr a-info r-info)
		       ;;(format t "expr = ~A~%" expr)
		       (cond ((and (listp expr)
				   (eq (first expr) 'fref))
			      #+nil
			      (format t " expr = ~A~%" expr)
			      #+nil
			      (format t "  a-info, r-info, SLICE-P = ~A ~A ~A~%"
				      a-info r-info (or (subtypep a-info 'array) r-info))
			      ;; If the declared type is an array,
			      ;; slice it.  Otherwise, grab just the
			      ;; element.
			      (if (subtypep a-info 'array)
				  `(array-slice ,(second expr) ,(lookup-vble-type (second expr)) ,@(cddr expr))
				  expr))
			     (t
			      expr)))
		   arg-list arg-info ret-info)))
	(t
	 arg-list)))

      
;------------------------------------------------------------------------------
(defun parse-subroutine-call (x)
  ;; X looks like (CALL SUBNAME (comma-separated list of args, if any))
  (let ((arglist (if (third x)
		     (mapcar #'id-expression
			     (list-split '|,| (check_new_vbles (third x))))
		     nil)))
    ;; Note that this is not a variable and is, in fact, a subroutine.
    (update-called-functions-list (list (second x) :subroutine) arglist)

    ;; Now convert array references to an array slice, as appropriate.
    (setf arglist (maybe-convert-array-ref-to-slice arglist (second x)))

    (cond ((null arglist)
	   ;; No args to the subroutine
	   `((,(second x))))
	  (t
	   ;; This is the more complicated case where we need to be
	   ;; more careful.  M-v-bind is used to get all of the return
	   ;; values.  Then we go and set the parameters according to
	   ;; the returned values.  If the parameter is a simple
	   ;; variable or an array reference, set the value.
	   ;; Otherwise, we do nothing.
	   ;;
	   ;; The user will have to check to make sure this is right.
	   ;; Array references can also be a slice of an array that is
	   ;; passed to the subroutine, and that isn't handled here!
	   ;;
	   (let ((fname (second x)))
	     (generate-call-to-routine
	      (cond ((member fname *subprog-arglist*)
		     ;; We want to use funcall only if the external function
		     ;; was passed in as a parameter.  If not, then we don't
		     ;; need to funcall it.  The user was just telling us that
		     ;; it was external function instead an intrinsic.
		     `(funcall ,fname)) 
		    (t 
		     `(,fname)))
	      arglist))))))

;------------------------------------------------------------------------------
(defun parse-do-loop (x)
  ;; Sometimes we get stuff like "DO 10, X = 1...".  Strictly speaking
  ;; the comma after the line number is illegal, but Fortran compilers
  ;; seem to accept this.  So if we find it, we need to remove that
  ;; extra comma.
  (when (eq (third x) '|,|)
    ;; Copy the list and remove the extra comma
    (setf x (list* (first x)
		   (second x)
		   (cdddr x))))
  (let* ((limits (list-split '|,| (cddddr x)))
	 (step (third limits))
	 (loop-var (check-reserved-lisp-names (third x))))
    #+nil
    (progn
      (format t "x = ~A~%" x)
      (format t "limits = ~A~%" limits)
      (format t "step   = ~A~%" step)
      (format t "var    = ~A~%" loop-var))
    `((fdo (,loop-var
	    ,(id-expression (first limits))
	    (+ ,loop-var ,(if (null step) 1 (id-expression step))))
       ((> ,loop-var ,(id-expression (second limits))) nil)
       ,(read-from-string 
	 (concatenate 'string (symbol-name :fdo_body_label)
		      (princ-to-string (second x))) nil)
       ))))

; (let* ((limits (list-split '|,| (cddddr x)))
;        (init-val (id-expression (first limits)))
;        (final-val (id-expression (second limits)))
;        (step (if (null (third limits)) 1 (id-expression (third limits))))
;       )
;`((fdo ((iteration-count 
;             (max (truncate (+ (- ,final-val ,init-val) ,step) ,step) 0)
;             (1- iteration-count))
;        (,(third x) ,init-val (+ ,(third x) ,step))
;       )
;       ((zerop iteration-count) nil)
;,(read-from-string 
;            (concatenate 'string "fdo_body_label" 
;                 (princ-to-string (second x))) nil)
;))))

(defun parse-if-then (x) 
   `((if-then ,(id-predicate (butlast (rest x)))  )))

(defun parse-if-goto (bindings) 
   `((if ,(id-predicate (variable-value '%pred bindings) )
         (go ,(read-from-string 
            (concatenate 'string (symbol-name :label)
                 (princ-to-string (variable-value '%label bindings))) nil)))))

(defun parse-if (x)
   (cond ;arithmetic if
         (;(member '|,| (cddr x))
          (and (eq (length (remove '|,| (cddr x))) 3)
               (not (member-if-not #'numberp (remove '|,| (cddr x)))))
          `((arithmetic-if ,(id-predicate (second x)) 
                           (go ,(read-from-string
                                 (concatenate 'string 
                                              (symbol-name :label)
                                              (princ-to-string (first (cddr x))))))
                           (go ,(read-from-string
                                 (concatenate 'string 
                                              (symbol-name :label)
                                              (princ-to-string (third (cddr x))))))
                           (go ,(read-from-string
                                 (concatenate 'string 
                                              (symbol-name :label)
                                              (princ-to-string (fifth (cddr x)))))))))
         ;logical if
         (t `((if ,(id-predicate (second x)) ,@(translate-line (cddr x)))))))

(defun parse-return (x) x)

(defun parse-parameter (x) 
   (setq x (mapcar #'(lambda (l) (remove '= l))
                   (list-split '|,| (cadr x))))

   ;;(format t "~&split x = ~A~%" x)
   (setq *key_params*
	 (append *key_params* 
		 (mapcar #'(lambda (l)
			     (list (first l) (id-expression (rest l))))
			 x)))
   nil)

;; restrict SAVE stmts to
;;     SAVE a,b,...
;; or  SAVE /label/a,b,..
;;
;; But a plain SAVE means save all local variables.
(defun parse-save (x)
  ;; Use the magic token '%save-all-locals% to mean we want to save
  ;; all local variables.  This can't be a valid Fortran variable, so
  ;; we're safe.
  (if (rest x)
      (setq *save_vbles*
	    (append *save_vbles*
		    (remove '|,| (if (eq (first (cdr x)) '/) (cddddr x) (cdr x)))))
      (setq *save_vbles* '%save-all-locals%))
   nil) 
  
(defun parse-common (common-statement)
  (let ((x (if (member 'f2cl-// common-statement)
	       ;; In preprocessing '//' got converted to f2cl-//.  We undo that
	       ;; here by converting f2cl-// to '(/ /).
	       (let ((result '()))
		 (dolist (item common-statement)
		   (cond ((eq item 'f2cl-//)
			  (push '/ result)
			  (push '/ result))
			 (t
			  (push item result))))
		 (nreverse result))
	       common-statement)))
	  
    (setq x (mapcar #'(lambda (l)
			(remove '|,| l))
		    (list-split '|/| (cdr x))))
    (setq x (if (null (car x))
		(cdr x)
		(cons nil x)))
    ;; x now in form (cb nlist cb nlist ... )

    ;; Pick out the common blocks and variables and put them in a hash
    ;; table for later use.  We want to associate the variables with
    ;; the common block.
    (do ((list x (cddr list)))
	((endp list))
      (let ((varlist (second list))
	    (block-name (or (caar list) '%blank%)))
	;; Check to see if this is another instance.  If so, Fortran
	;; says these elements are a member of the common block.
	(multiple-value-bind (val found)
	    (gethash block-name *common-blocks*)
	  (declare (ignore found))
	  ;;(format t "block = ~S~%" block-name)
	  ;;(format t "  val = ~S~%" val)
	  ;;(format t "  vars = ~S~%" varlist)
	  ;;(format t "  varlst = ~S~%" (append val (remove-if-not #'symbolp varlist)))
	  (setf (gethash block-name *common-blocks*)
		(append val (mapcar #'check-reserved-lisp-names
				    (remove-if-not #'symbolp varlist)))))))

   
    ;; pick out lists of vblenames and add to *subprog_common_vars*
    (setq *subprog_common_vars* 
	  (append *subprog_common_vars*
		  (do ((list x (cddr list))
		       (ret nil (append (extract-atoms (cadr list))
					ret)))
		      ((endp list)
		       (mapcar #'check-reserved-lisp-names ret))
		    ;; look for common arrays and store dimensions of
		    ;; new common vars
		    (do ((nlist (cadr list) (cdr nlist)))
			((endp nlist))
		      (cond ((and (cdr nlist)
				  (listp (cadr nlist)))
			     ;; array dimensioned in COMMON stmt
			     (update_cm_array_dims (car nlist)
						   (cadr nlist))
			     (setq nlist (cdr nlist)))
			    ;; check if array dimensioned elsewhere
			    ((member (car nlist) *declared_vbles*)
			     (do ((decls *explicit_vble_decls* (cdr decls)))
				 ((null decls) nil)
			       (do ((vbles (cdar decls) (cdr vbles)))
				   ((null vbles) nil)
				 (if (and (eq (car nlist) (caar vbles))
					  (cdar vbles))
				     ;; Array has been dimensioned
				     ;; elsewhere.  Update the
				     ;; dimensions.  The dimensions
				     ;; have already been parsed, so
				     ;; "unparse" them by putting a :
				     ;; between the dimensions.
				     ;; FIXME: This is really gross!
				     (update_cm_array_dims
				      (car nlist)
				      (car (mapcar #'(lambda (v)
						       (list (first v)
							     '|:|
							     (second v)))
						   (cdar vbles))))))))
			    )))))
    ;;(format t "*common_array_dims* = ~A~%" *common_array_dims*)
    nil))

; append list of vble and dims to *common_array_dims* if vble not already in list
; when vble in list check dims match with that stored
(defun update_cm_array_dims (vble dims)
  (let ((stored-dims (member vble *common_array_dims*))
	(parsed-dims (parse_dimension_specs dims)))
    (if stored-dims
	(when (not (equal (cadr stored-dims) parsed-dims))
	  (error "common array ~A dimensions not equivalent between subprograms" vble))
	(setq *common_array_dims*
	      (append (list vble parsed-dims)
		      *common_array_dims*)))))

(defun extract-atoms (x)
   (do ((l x (cdr l))
        (ret nil (if (atom (car l))
		     (cons (car l) ret)
		     ret)))
       ((endp l) ret)))

;; Parse Fortran character declarations. These are rather complicated
;; and we need to be able to handle things like:
;;
;; character*10 a, b, c
;; character a*10, b*20, c*30
;; character*10 x(10), y(20), z(3,4)
;; character x(10)*10, y(20)*7, z(3,4)*8
;;
;; By the time we get these, the line looks something like the
;; following, respectively:
;;
;; character * 10 a |,| b |,| c
;; character a * 10 |,| b * 20 |,| c * 30
;; character * 10 x(10) |,| y(20) |,| z(3 |,| 4)
;; character x(10) * 10 |,| y(20) * 7 |,| z(3 |,| 4) * 8

(defun parse-char-decl (x)
  ;; x is the line.
  (cond ((eq (second x) '*)
	 ;; The length was given explicitly as part of the
	 ;; character declaration.
	 (push `((character ,(third x))
		 ,@(mapcar #'(lambda (decl)
			       (let ((dcl (remove '* decl)))
				 ;; (format t "  decl = ~A~%" dcl)
				 (destructuring-bind (name &optional dim-or-len)
				     dcl
				   (if dim-or-len
				       ;; An array of characters
				       `(,name ,@(parse_dimension_specs dim-or-len))
				       ;; A simple character string
				       dcl))))
			   (list-split '|,| (cdddr x))))
	       *explicit_vble_decls*))
	(t
	 ;; The length may have been given as part of the variable.
	 ;; Put the length with the variable type.
	 (mapc #'(lambda (decl)
		   (destructuring-bind (name &optional dim-or-len &rest len)
		       (remove '* decl)
		     (push
		      (if (and dim-or-len (listp dim-or-len))
			  ;; An array
			  `((character ,@len) (,name ,@(parse_dimension_specs dim-or-len)))
			  ;; A simple character string
			  `((character ,(or dim-or-len 1)) (,name)))
		      *explicit_vble_decls*)))
	       (list-split '|,| (cdr x)))))
     
  ;;(format t "explicit_vble_decls* = ~A~%" *explicit_vble_decls*)
  )


; DATA stmts 
;           DATA nlist/clist[,nlist/clist/]...
; restricted to 
;   one vble name per nlist
;   no implied do's
;   clist either a alist or a repetition (not a combination of both)

;; rlt:  also handles
;;	data var1, var2, var3/val1, val2, val3/

(defun parse-data (x)
  ;;(format t "parse-data:  ~S~%" x)
  ;;(setq x (list-split '|/| (cdr x)))
  ;;(format t "parse-data post-split:  ~S~%" x)
  (do ((list (list-split '|/| (cdr x))
	     (cddr list))
       (ret nil
	    (let ((var (remove nil (list-split '|,| (car list))))
		  (vals (list-split '|,| (cadr list))))
	      ;;(format t "list = ~a~%" list)
	      ;;(format t "var = ~a~%" var)
	      ;;(format t "vals = ~a~%" vals)
	      
	      ;; Need to be careful here.  We might be doing either
	      ;;
	      ;; data x / 1 , 2 , 3 /
	      ;;
	      ;; or
	      ;;
	      ;; data x(1) / 1 /.
	      ;;
	      ;; So we check the var is an array without dimensions.
	      ;; If so, the VALS list is really all the values for the
	      ;; array.
	      ;;
	      ;; We also might have an implied do loop, in which case,
	      ;; we want VALS to be all the values.
	      ;;
	      ;; (FIXME:  This area probably needs a lot of rework!)
	      (let* ((is-array (or (and (vble-is-array-p (check-reserved-lisp-names (caar var)))
					(null (cdar var)))
				   (and (listp var) (listp vals)
					(listp (caar var))
					(member '= (caar var)))))
		     (result (mapcar #'parse-data1 var (if is-array (list vals) vals))))
		;;(format t "result = ~a~%" result)
		(setf *data-init* (append *data-init* (if (consp (first result))
							result
							(list result)))))
	      ;;(format t "*data-init*:  ~A~%" *data-init*)
	      )))
      ((null (cdr list)))))

; parse a (vble_name data1 data2 ...) list
;    or a (vble_name n*x) list
(defun parse-data1 (v l)
  ;;(format t "parse-data1:  v = ~S~%" v)
  ;;(format t "parse-data1:  l = ~S~%" l)
  (labels ((fix-up-negative-number (x)
	     (cond ((numberp x)
		    x)
		   (t
		    (let ((ex (id-expression (if (atom x) (list x) x))))
		      (if (listp ex)
			  (- (second ex))
			  ex)))))
	   (fix-up-data-reps (data)
	     (let ((result '()))
	       (dolist (item data (nreverse result))
		 ;;(format t "data-reps item = ~S~%" item)
		 (cond ((and (>= (length item) 3)
			     (eq (second item) '*))
			;;(format t "~a reps of ~S~%" (first item) (rest (rest item)))
			(let ((num (fix-up-negative-number (rest (rest item)))))
			  (dotimes (k (first item))
			    (push num result))))
		       (t
			(push (fix-up-negative-number item) result))))))
	     (data-var (v)
	       (cond ((atom v)
		      v)
		     ((listp v)
		      (mapcar #'car v))
		     (t
		      nil))))
    (cond ((and (listp (car l))
		(eq (second (car l)) '*))
	   ;;(format t "parse-data1 1 l = ~A~%" l)
	   `(fill ,(check-reserved-lisp-names (first v))
	          ,(fix-up-negative-number (cddar l))
	          :end ,(first (car l))))
	  ((and (listp v) (listp l)
		(listp (second v))
		(numberp (first (second v))))
	   ;; Initializing one element of an array.

	   ;;(format t "parse-data1 array~%")
	   (let* ((vname (check-reserved-lisp-names (first v)))
		  (dims (lookup-array-bounds vname)))
	     ;; FIXME: DO NOT change this to SETF.  Other code uses
	     ;; this FSET to figure out how to initialize the DATA
	     ;; array nicely.  When this is fixed, we can use plain
	     ;; SETF.
	     `(fset (fref ,vname
		          ,(remove '|,| (second v))
		          ,dims)
	       ,(fix-up-negative-number l))))
	  ((and (listp v) (listp l)
		(listp (first v))
		(member '= (first v)))
	   ;; Implied do loop.  Extract out the important parts of the
	   ;; implied do and construct what we need from it.
	   (let* ((posn (position '= (first v)))
		  (split (list-split '|,| (subseq (first v) 0 (- posn 2))))
		  (loop-info
		   `(,split ,(remove '= (remove '|,| (subseq (first v) (1- posn)))))))
	     `(data-implied-do ,loop-info ,(data-var split) ,(fix-up-data-reps l))
	     ))
	  ((and (listp v) (listp l)
		(= (length v) (length l))
		(> (length v) 1))
	   ;;(format t "parse-data1 op~%")
	   (mapcar #'(lambda (var val)
		       `(setq ,(check-reserved-lisp-names var)
			      ,(fix-up-negative-number val)))
		   v l))
	  (t
	   ;;(format t "parse-data1 else: v, l = ~S ~S~%" v l)
	   ;;(format t "parse-data1 else:  v is array:  ~A~%" (vble-is-array-p (check-reserved-lisp-names (first v))))
	   (cond ((subtypep (lookup-vble-type (check-reserved-lisp-names (first v)))
			    'string)
		  ;; Need to initialize a string carefully.
		  ;;
		  ;; FIXME: We should probably do this when we declare
		  ;; the array instead.
		  `(replace ,(check-reserved-lisp-names (first v)) ,l)
		  )
		 ((and (vble-is-array-p (check-reserved-lisp-names (first v)))
			(null (rest v)))
		  `(replace ,(check-reserved-lisp-names (first v))
		         ',(mapcar #'fix-up-negative-number l)))
		 (t
		  `(setq ,(check-reserved-lisp-names (first v))
		      ,(fix-up-negative-number l))))))))

; parse EXTERNAL f1, f2, ...
; by adding the function names to *external-function-names*
(defun parse-external (x)
   (setq *external-function-names*
         (append *external-function-names* (remove '|,| (cdr x))))
   nil)

;; parse EQUIVALENCE (x1,y1), (x2, y2), ...
#+nil
(defun parse-equivalence (x)
  (format t "equivalence ~A~%" x)
  (let ((pairs (mapcar #'car (list-split '|,| (cdr x)))))
    (setf *equivalenced-vars* (mapcar #'(lambda (l)
					  (list-split '|,| l))
				      pairs))
    (format t "~A~%" *equivalenced-vars*)
    nil))

(defun parse-equivalence (x)
  ;;(format t "equivalence ~A~%" x)
  (let ((vars (mapcar #'(lambda (l)
			  (list-split '|,| l))
		      (mapcar #'car (list-split '|,| (cdr x))))))
    ;;(format t "~A~%" *equivalenced-vars*)
    ;;(setf *equivalenced-vars* vars)
    (setf *equivalenced-vars*
	  (append *equivalenced-vars*
		  (mapcar #'(lambda (l)
			      `(,(parse-expression (first l))
				 ,(parse-expression (second l))))
			  vars)))
    ;;(format t "~A~%" *equivalenced-vars*)
    nil))

;=============================================================================
;parsing utilities 

(defun tail-chop (beta lis) 
   (prog (retlist)
      (setq retlist nil)
    loop
      (cond ((or (equal lis nil) (equal (car lis) beta))
             (return retlist))
            (t (setq retlist (append1 retlist (car lis))
                  lis (cdr lis))
             (go loop)))))

(defun head-chop (beta lis) (cdr (member beta lis)))

(defun list-split (beta lis)
     (cond ((equal (member beta lis)  nil) (list lis))
           (t `(,(tail-chop beta lis)
                            ,@(list-split beta (head-chop beta lis))))))

(defun gen-list-split (beta lis)
  (prog (sym ops)
     (cond ((null (remove nil (mapcar 'member beta 
                                           (const lis (length beta)))))
                                            (return (list (list lis) nil)))
           (t (setq sym (gensym))
              (setq ops (extract beta lis))
              (setq lis (tpl-subpair (const sym (length beta)) beta lis))
              (return (list `(,(tail-chop sym lis)
                 ,@(list-split sym (head-chop sym lis))) ops))))))


(defun concat (&rest syms)
  (cond ((null syms)
	 "")
	(t
	 (concatenate 'string (symbol-name (first syms))
		      (apply #'concat (rest syms))))))


(defun list-split-multi (op lis)
  (prog (ret)
    (setq ret (match-separated (list op) lis))
    (cond ((null ret) (princ-reset (concat
'|Senac syntax error: failure to parse an expression 
                  using the operator "| op '|"
Senac syntax: ... | op '| ... | op '| ... OR
              ... | op '| ...|)))
          (t (return (car ret))))))

(defun list-split-bin (op lis)
  (prog (ret)
    (setq ret (match-separated (list op) lis))
    (cond ((null ret) (princ-reset (concat
'|Senac syntax error: failure to parse an expression
                   using the operator "| op '|"
Senac syntax:... | op '| ...|)))
          (t (return  (car ret))))))

(defun list-split-multi-string (op lis)
  (prog (ret)
    (setq ret (match-separated op lis))
    (cond ((null ret) (princ-reset (concat
'|Senac syntax error: failure to parse an expression
                   using the operator "| op '|"
Senac syntax:... | op '| ...|)))
          (t (return  (car ret))))))

(defun concat-operators (x)
  ;; Look through the elements of the list X for Fortran operators
  ;; that were split up but really should be one.
  (let (done)
    (do* ((this (car x) (first rest))
	  (rest (cdr x) (cdr rest)))
	 ((null rest)
	  (return (nreverse (push this done))))
      (let ((next (car rest)))
	(cond ((and (eq this '*)
		    (eq next '*))
	       ;; Exponentiation operator
	       (pop rest)
	       (push '^ done))
	      ((and (eq this '>)
		    (eq next '=))
	       ;; >=
	       (pop rest)
	       (push '>= done))
	      ((and (eq this '>)
		    (eq next '<))
	       ;; >< (not equal)
	       (pop rest)
	       (push '>< done))
	      ((and (eq this '<)
		    (eq next '=))
	       ;; <=
	       (pop rest)
	       (push '<= done))
	      ((and (eq this '/)
		    (eq next '/))
	       ;; Fortran string concat operator
	       (pop rest)
	       (push 'f2cl-// done))
	      (t
	       (push this done)))))))

(defun convert-data-type (x)
  (cond ((not (listp x)) 
	 (car (convert-data-type (list x))))
	((equalp (subsequence x 0 2) '(double precision))
	 (append '(double-float) (cdr x)))
	((equalp (subsequence x 0 3) '(real * 8))
	 (append '(double-float) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(integer * 1))
	 (append '(integer1) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(integer * 2))
	 (append '(integer2) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(integer * 4))
	 (append '(integer4) (subsequence x 3)))
	((eq (car x) 'real)   
	 (append '(single-float) (cdr x)))
	((eq (car x) 'integer)
	 (append '(integer4) (cdr x)))
	((equalp (subsequence x 0 3) '(complex * 8))
	 (append '(complex8) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(complex * 16))
	 (append '(complex16) (subsequence x 3)))
	((and (listp (car x)) (eq (caar x) 'complex))
	 (append '(complex8) (cdr x)))
	((and (listp (car x)) (eq (caar x) 'character))
	 (append `(character ,(third (car x))) (cdr x)))
	((and (listp (car x)) (eq (caar x) 'real))
	 (append '(single-float) (cdr x)))
	((and (listp (car x)) (eq (caar x) 'integer))
	 (append '(fixnum) (cdr x)))
	((eq (car x) 'double) 
	 (append '(double-float) (cdr x)))
	((eq (car x) 'logical) 
	 (append '(logical) (cdr x)))
	(t x)))

(defun reduce-data-type (x)
  (let ((subseq3 (subsequence x 0 3)))
    (cond ((equalp (subsequence x 0 2) '(double precision))
	   (remove 'precision x))
	  ((equalp (subsequence x 0 2) '(double complex))
	   (append '(complex16) (subsequence x 2)))
	  ((equalp subseq3 '(real * 4))
	   (append '(real) (subsequence x 3)))
	  ((equalp subseq3 '(real * 8))
	   (append '(double) (subsequence x 3)))
	  ((equalp subseq3 '(complex * 16))
	   (append '(complex16) (subsequence x 3)))
	  ((equalp subseq3 '(integer * 4))
	   (append '(integer) (subsequence x 3)))
	  ((equalp subseq3 '(integer * 2))
	   (append '(integer2) (subsequence x 3)))
	  ((equalp subseq3 '(integer * 1))
	   (append '(integer1) (subsequence x 3)))
	  (t x))))
  

;=============================================================================

; unification pattern matcher

(defun binding-value (binding)
 (cdr binding))

(defun variablep (v)
 (and (symbolp v)
      (char= (schar (symbol-name v) 0) #\%)))

(defun variable-value (variable bindings)
  (let ((binding (assoc variable bindings)))
    (values (binding-value binding)
            (not (null binding)))))

(defun unify (term-1 term-2 bindings)
  (cond 
    ((eq bindings 'fail) bindings)
    ((variablep term-1) 
     (maybe-extend-bindings term-1 term-2 bindings))
    ((variablep term-2)
     (maybe-extend-bindings term-2 term-1 bindings))
    ((or (atom term-1)
         (atom term-2))
     (if (equal term-1 term-2)
         bindings
         'fail))
    (t (unify (cdr term-1)
              (cdr term-2)
              (unify (car term-1)
                     (car term-2)
                     bindings)))))

(defun match (term-1 term-2 bindings)
 (let ((new-bindings (unify  term-1 term-2 bindings)))
  (if (eq 'fail new-bindings)
      nil
      (values t  new-bindings))))

(defun maybe-extend-bindings (variable value bindings)
  (multiple-value-bind (present-value found?)
    (variable-value variable bindings)
      (if found?
       (unify present-value
              value
              bindings) 
; no occurs check !
       (acons variable value bindings)))) 

;-----------------------------------------------------------------------------
(defun lineread (stream)
  (prog (ans rans next-char)
loop1 
    (setq ans (cons 
	       (read-preserving-whitespace stream nil 'eof nil) ans))
loop2  
    (setq next-char (peek-char nil stream nil 'eof nil))
    (cond ((eql next-char #\Space)
	   (read-char stream nil 'eof t) (go loop2)))
    (cond ((member next-char '(#\Newline eof) :test #'eql)
	   (setq rans (nreverse ans))
	   (return rans)))
    (go loop1)))

;------------------------------------------------------------------------------
(defun read-six-chars (stream)
; (make-array '(6) :element-type 'string-char :initial-contents
    (list
       (read-char stream nil 'eof t)
       (read-char stream nil 'eof t)
       (read-char stream nil 'eof t)
       (read-char stream nil 'eof t)
       (read-char stream nil 'eof t)
       (read-char stream nil 'eof t)))

;------------------------------------------------------------------------------	

(defun const (x n)
  (make-list n :initial-element x))
;------------------------------------------------------------------------------	
(defun brackets-check (x)
  (prog (path-stack ce check-list ret-list)
 
     (cond ((and 
             (not (member '|(| x)) (not (member '|)| x))
             (not (member '|[| x)) (not (member '|[| x))
             (not (member '|{| x)) (not (member '|}| x))) (return x)))
     (setq path-stack '((0 0 0)) ;;;stack-top '(0 0 0)
           check-list x ce (car x) ret-list nil)
     loop
     (cond ((null check-list)
	    (cond 
	      ((greaterp (caar path-stack) 0)
	       (princ-reset 
		'|Syntax error: missing right parenthesis ")"|) )
	      ((greaterp (cadar path-stack) 0)
	       (princ-reset 
		'|Syntax error: missing right bracket "]"|) )
	      ((greaterp (caddar path-stack) 0)
	       (princ-reset 
		'|Syntax error: missing right brace "}"|))
	      (t (return ret-list)))))

     (cond 
       ((equal ce '|(|) 
	(push (mapcar 'plus '(1 0 0) (car path-stack)) path-stack)
	(setq ret-list (gen-append ret-list nil (caadr path-stack))))
       ((equal ce '|[|) 
	(push (mapcar 'plus '(0 1 0) (car path-stack)) path-stack)
	(setq ret-list (gen-append ret-list '|[| (caar path-stack))))
       ((equal ce '|{|) 
	(push (mapcar 'plus '(0 0 1) (car path-stack)) path-stack)
	(setq ret-list (gen-append ret-list '|{| (caar path-stack))))
       ((and (equal ce '|)|) (or (nequal (mapcar 'diff (pop path-stack) '(1 0 0))
					 (car path-stack))
				 (lessp (caar path-stack) 0)))
	(princ-reset 
	 '|Syntax error: right parenthesis ")" in an invalid position or unmatched|))
       ((equal ce '|)|))
       ((and (equal ce '|]|) (or (nequal (mapcar 'diff (pop path-stack) '(0 1 0))
					 (car path-stack))
				 (lessp (caar path-stack) 0)))
	(princ-reset 
	 '|Syntax error: right bracket "]" in an invalid position or unmatched|))
       ((equal ce '|]|)
	(setq ret-list (gen-append ret-list ce (caar path-stack))))
       ((and (equal ce '|}|) (or (nequal (mapcar 'diff (pop path-stack) '(0 0 1))
					 (car path-stack))
				 (lessp (caar path-stack) 0)))
	(princ-reset 
	 '|Syntax error: right brace "}" in an invalid position or unmatched|))
       ((equal ce '|]|)
	(setq ret-list (gen-append ret-list ce (caar path-stack))))
       (t (setq ret-list (gen-append ret-list ce (caar path-stack))))

       )
      
     (setq check-list (cdr check-list) ce (car check-list))
     (go loop)))

;-----------------------------------------------------------------------------

(defun subsequence (seq start &optional (end (length seq)))
  (if (null seq) seq
                 (subseq seq start (min end (length seq)))))
;-----------------------------------------------------------------------------

(defun gen-append (lis x n) 
(cond ((equal n 0) (append1 lis x))
      (t (append1 (end-cdr lis) (gen-append (car (last lis)) x
                        (sub1 n))))))

(defun append1 (l x) (append l (list x)))
(defun plus (&rest args) (apply #'+ args))
(defun end-cdr (x) (butlast x))
(defun sub1 (x) (1- x))
(defun nequal (x y) (not (equal x y)))
(defun diff (x y) (- x y))
(defun lessp (x y) (< x y))
(defun greaterp (x y) (> x y))
;==============================================================================

(defun f-to-l (file)
   (fortran-to-lisp (preprocess file) "temp"))


;; Some pretty printers for f2cl code.
(defun pprint-fdo (stream fdo-sexp)
  ;; Print fdo's like so:
  ;;
  ;; (f2cl-lib:fdo (iter 1 (+ iter 1))
  ;;               ((> iter itmax) nil)
  ;;   (tagbody
  ;;     ...))
  (pprint-logical-block (stream fdo-sexp :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (write-char #\space stream)
    ; (pprint-newline :miser)
    (pprint-indent :current 0 stream)
    (write (pprint-pop) :stream stream)
    (pprint-newline :mandatory stream)
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 1 stream)
    (pprint-newline :mandatory stream)
    (loop
     (write (pprint-pop) :stream stream)
     (pprint-exit-if-list-exhausted)
     (write-char #\space stream)
     (pprint-newline :linear stream))))

(defun pprint-with-array-data (stream list)
  ;; Print with-array-data like so:
  ;;
  ;; (f2cl-lib:with-array-data ((data-var offset-var array))
  ;;   body)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (write-char #\space stream)
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 1 stream)
    (pprint-newline :mandatory stream)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
       (pprint-newline :linear stream))))

(defun pprint-with-multi-array-data (stream list)
  ;; Print with-array-data like so:
  ;;
  ;; (f2cl-lib:with-array-data ((data-var offset-var array))
  ;;   body)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    ;;(write-char #\space stream)
    (pprint-indent :block 3 stream)
    (pprint-newline :mandatory stream)
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 1 stream)
    (pprint-newline :mandatory stream)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
       (pprint-newline :linear stream))))

(set-pprint-dispatch '(cons (member f2cl-lib:fdo)) #'pprint-fdo)
(set-pprint-dispatch '(cons (member f2cl-lib:with-array-data)) #'pprint-with-array-data)
(set-pprint-dispatch '(cons (member f2cl-lib:with-multi-array-data))
		     #'pprint-with-multi-array-data)

;;;-----------------------------------------------------------------------------
;;; end of f2cl1.l
;;;
;;; $Id: f2cl1.l,v 1.169 2006/12/21 03:42:11 rtoy Exp $
;;; $Log: f2cl1.l,v $
;;; Revision 1.169  2006/12/21 03:42:11  rtoy
;;; GCL doesn't like some of the circular objects printed out in the f2cl
;;; info.  Turn off :circle t.
;;;
;;; Revision 1.168  2006/12/21 03:18:39  rtoy
;;; GCL doesn't like our IN-PACKAGE forms that we produce.  It also
;;; doesn't like (:and) or (:or), so replace them with (and) and (or),
;;; respectively.
;;;
;;; Revision 1.167  2006/11/28 21:41:12  rtoy
;;; After each function is translated, save the function info too so that
;;; we can reload the correct info.  This is useful when we want to
;;; recompile just a single file that calls other functions.  Then f2cl
;;; can do a good job compiling the new function, without warnings about
;;; calls to unknown functions.
;;;
;;; Revision 1.166  2006/11/21 18:20:17  rtoy
;;; o Add CDABS and DCONJG to the list of intrinsic functions.
;;; o Modify PARSE-DO-LOOP to be a little more forgiving to handle some
;;;   issues in BLAS routines.  We recognize things like "DO 10, X=1,10"
;;;   and treat it as if it were "DO 10 X=1,10".  Strictly speaking, the
;;;   former is illegal, but it seems many compilers accept it.
;;; o Make REDUCE-DATA-TYPE recognize "double complex" as if it were
;;;   "complex*16".  Some BLAS routines use "double complex".
;;;
;;; Revision 1.165  2006/05/05 20:28:21  rtoy
;;; README:
;;; o Document the options.
;;; o Add some info about what the :common-as-array parameter really
;;;   means.  (Because I couldn't remember either!)
;;;
;;; src/f2cl1.l:
;;; o Add :common-as-array to the docstring for f2cl and f2cl-compile.
;;;
;;; Revision 1.164  2006/05/04 04:02:09  rtoy
;;; Add a function, SAVE-F2CL-FINFO to save the compiled function info to
;;; a file which can be reloaded later.
;;;
;;; Revision 1.163  2006/05/03 02:31:09  rtoy
;;; src/f2cl1.l:
;;; o When parsing an entry point, keep track of the actual parent
;;;   function so we can generate the correct calling info.  (We only
;;;   support entry points with exactly the same number and type of
;;;   arguments so the calling info has to be the same.)
;;;
;;;   Do this by adding the parent to the list pushed on *entry-points*.
;;;
;;; o Set *subprog_name* to the function name.  (Is this right?)
;;;
;;; src/f2cl5.l:
;;; o If possible, use the parent name to figure out the calling info for
;;;   the entry point.
;;;
;;;
;;; With these changes hompack can be compiled twice, successfully.
;;; Previously polyp.f would call polynf correctly the first time, but
;;; when everything is recompiled, polyp.f would incorrectly call polynf
;;; with no args!
;;;
;;; Revision 1.162  2006/05/01 17:38:27  rtoy
;;; Replace some uses of FSET with plain ol' SETF because SETF does
;;; everything we want it to do.  But leave some FSET's around because we
;;; need them later to generate initializers for DATA statements, and
;;; such.
;;;
;;; Revision 1.161  2006/04/28 15:42:01  rtoy
;;; o Forgot to apply the same change to PARSE-ARRAYREF-OR-STMTFN that was
;;;   needed for PARSE-ASSIGNMENT for assigning a complex to a real.
;;; o Add new common function COERCE-RHS-TO-LHS to be used by of these
;;;   routines so we don't forget again.
;;;
;;; Revision 1.160  2006/04/28 13:27:42  rtoy
;;; o When assigning a complex to a real variable, we need to take the
;;;   realpart.  We weren't doing that.
;;; o Cleanup a compiler warning
;;; o Simplify a few debugging prints.
;;;
;;; Revision 1.159  2006/04/27 17:44:01  rtoy
;;; src/f2cl0.l:
;;; o Export dimag, dcmplx, zsqrt
;;;
;;; src/f2cl1.l:
;;; o Add dcmplx, dimag, and zsqrt to the list of intrinsic function
;;;   names.
;;; o When parsing "implicit none" statements, we don't modify
;;;   *IMPLICIT_VBLE_DECLS*. I don't think it's needed and it can cause
;;;   errors later on because :none is not a Lisp type.
;;;
;;; src/f2cl5.l:
;;; o Tell GET-FUN-ARG-TYPE about the result type of dcmplx, dsqrt, the
;;;   complex*8 and complex*16 special functions.
;;; o ABS is an allowed lisp name.  This gets rid of the spurious ABS$
;;;   local variable whenever we use the ABS function.
;;;
;;; src/macros.l:
;;; o Add implementations of dcmplx, dimag, and zsqrt.  (We need to add
;;;   more, I think.)
;;;
;;; Revision 1.158  2006/04/26 19:59:51  rtoy
;;; Oops.  Need to check for extended DO before DO WHILE.  (This should be
;;; fixed so it doesn't have to be that way.)
;;;
;;; Revision 1.157  2006/04/26 18:38:11  rtoy
;;; src/Notes:
;;; o Update
;;;
;;; src/f2cl1.l:
;;; o Add support for DO-WHILE statements.
;;;
;;; Revision 1.156  2006/04/07 20:38:41  rtoy
;;; Wrap eval-when around the defstruct for f2cl-finfo for Allegro.  Bug
;;; and fix reported by Richard Fateman.
;;;
;;; Revision 1.155  2006/01/31 15:11:05  rtoy
;;; Checkin to update version id.
;;;
;;; Revision 1.154  2006/01/30 22:59:51  rtoy
;;; o Check in to get new f2cl-version number.
;;; o Reindented brackets-check to something more typical.
;;;
;;; Revision 1.153  2006/01/27 14:12:55  rtoy
;;; o Adjust SPECIAL-PRINT so we don't try to print readably because Clisp
;;;   tries very hard to make every truly readable, including escaping all
;;;   symbols and including package qualifiers to make sure it's read
;;;   exactly the same.  That's not what f2cl really wants.
;;;
;;; o In PARSE-EQUIVALENCES, we want to append to *equivalenced-vars*, not
;;;   set it each time we have an equivalence statement!  Otherwise, we
;;;   end up with just the last equivalence statement to process,
;;;   forgetting all others.
;;;
;;; Revision 1.152  2006/01/11 22:57:58  rtoy
;;; Add rudimentary support for opening files and reading from files.
;;;
;;; src/f2cl1.l:
;;; o Recognize and handle open, rewind, and close statements.
;;;
;;; src/f2cl5.l:
;;; o Update parser for read to handle unit numbers.  Rudimentary support
;;;   for implied-do lists too.
;;; o Add parser for open, rewind, and close statements.
;;;
;;; src/macros.l:
;;; o Add functions and macros to handle opening, rewinding,
;;;   and closing files.  Needs more work still.
;;;
;;; Revision 1.151  2006/01/11 16:32:15  rtoy
;;; Print a warning (as well) if we failed to translate something.  (We
;;; were just leaving a comment in the translated code.)
;;;
;;; Revision 1.150  2006/01/10 21:20:59  rtoy
;;; Oops.  Need to clear out *equivalenced-vars* for every subprogram.
;;;
;;; Revision 1.149  2006/01/09 19:19:17  rtoy
;;; Don't add \ to the Fortran readtable.
;;;
;;; I don't know why we do that, but it breaks parsing of strings that
;;; contain embedded double-quote marks.  I hope removing this doesn't
;;; break something else, but more testing definitely needed.
;;;
;;; Revision 1.148  2006/01/09 03:08:13  rtoy
;;; src/f2cl1.l:
;;; o Translate a Fortran STOP to be the stop function.  Was just
;;;   returning NIL, and this doesn't work so well.
;;;
;;; src/macros.l:
;;; o Add STOP function.  It prints out the any arg, and then signals an
;;;   error.
;;;
;;; Revision 1.147  2006/01/04 17:53:40  rtoy
;;; We were not correctly processing intialization of string arrays in
;;; data statements.
;;;
;;; src/f2cl1.l:
;;; o In PARSE-DATA1, return the entire list of initializers instead of
;;;   just the first, in case we have an array of initializers.
;;;
;;; src/f2cl5.l:
;;; o In MERGE-DATA-AND-SAVE-INITS, we need to recognize the
;;;   initialization of strings and such.  We don't do anything special
;;;   right now, like we do for arrays of numbers.
;;; o In INSERT-DECLARATIONS, we need to handle the case of REPLACE in the
;;;   *data-init*'s.  We assume it's been handled somewhere else, so
;;;   there's nothing to do here.
;;;
;;; Revision 1.146  2005/07/26 12:37:15  rtoy
;;; Don't proclaim *readtable* as special, because it's already in the CL
;;; package and special.  (For sbcl.)
;;;
;;; Revision 1.145  2005/07/16 15:11:55  rtoy
;;; In GENERATE-CALL-TO-ROUTINE, if the routine is a parameter, we don't
;;; really know anything about it even if we know the info about a routine
;;; of the same name.  Thus, there's nothing we can do except just call it
;;; and hope it's right.  (We warn the user, though!)
;;;
;;; Revision 1.144  2005/07/15 15:28:07  rtoy
;;; Make the parser for IF statements a little smarter.  "IF =" is not an
;;; IF statement.
;;;
;;; Revision 1.143  2005/07/14 21:44:09  rtoy
;;; Oops.  Need to clear out *equivalenced-vars* on every call to f2cl, so
;;; we don't get old info!
;;;
;;; Revision 1.142  2005/07/14 21:38:58  rtoy
;;; o Change default array-type in F2CL to be the same as F2CL-COMPILE.
;;;
;;; o Add support for some EQUIVALENCE statements.  We can handle
;;;   equivalence statements that equivalence an array (element) to a
;;;   simple variable of the same type.  Everything else will cause an
;;;   error.  This is much better than putting a silly "not-translated"
;;;   string into the generated lisp file.
;;;
;;; Revision 1.141  2005/07/14 17:29:28  rtoy
;;; Make f2cl a little smarter when calling functions if f2cl knows the
;;; expected arg types of the function.  If the arg type is a simple
;;; variable and the actual arg is an array slice, convert the slice to an
;;; array reference.
;;;
;;; Revision 1.140  2005/06/20 01:53:39  rtoy
;;; Add code to try to merge the data statement initializers into the
;;; declaration of the saved variable itself instead of generating a bunch
;;; of fset forms.
;;;
;;; See NOTES for more detail.
;;;
;;; src/NOTES:
;;; o Describe change
;;;
;;; src/f2cl5.l:
;;; o (Gross) Implementation
;;;
;;; src/f2cl1.l:
;;; o Update version.
;;;
;;; Revision 1.139  2005/06/01 15:29:41  rtoy
;;; o Shorten the length of some lines in docstrings.
;;; o As part of the header, print out the lisp implementation type and
;;;   version.
;;;
;;; Revision 1.138  2005/05/19 15:09:32  rtoy
;;; Gratuitous change to update the rev date.
;;;
;;; Revision 1.137  2004/11/09 18:37:12  rtoy
;;; o Use compile-file-pathname for the output-file instead of T, because
;;;   that's a CMUCL extension.
;;; o Recognize real*4 as a declaration of type REAL.  We had forgotten
;;;   that.
;;; o When parsing common blocks with arrays in them that were already
;;;   dimensioned elsewhere, we were calling update_cm_array_dims
;;;   incorrectly with the parsed dimensions.  "Unparse" them first.  This
;;;   is really gross.
;;;
;;; Revision 1.136  2004/08/17 16:09:01  rtoy
;;; Reinstate make-label here.  This really needs to be cleaned up.
;;;
;;; Revision 1.135  2004/08/14 13:44:37  rtoy
;;; Add support for ASSIGN statement.
;;;
;;; Revision 1.134  2004/08/13 21:16:27  rtoy
;;; First pass at creating common blocks as arrays.  Intent is to allow
;;; odepack to be converted via f2cl.
;;;
;;; So a common block structure is created that creates as large an array
;;; as possible for consecutive elements of the same type in the common
;;; block.  A new array is created for each such section.  Then the
;;; elements of the common block are accessed either as either an
;;; individual element of the array or as a displaced array.
;;;
;;; This might have speed impacts, so the default is not to do this.  Use
;;; the keyword :common-as-array to control this feature.  Default is off,
;;; preserving old behavior.
;;;
;;; Revision 1.133  2004/08/11 19:00:33  rtoy
;;; Set the f2cl version to include the RCS Date.
;;;
;;; Revision 1.132  2003/11/18 19:33:47  rtoy
;;; Push the function name onto the calls list, not a list of the function
;;; name.
;;;
;;; Revision 1.131  2003/11/14 06:45:41  rtoy
;;; Actually, if the declared arg type is an array, we always want to
;;; slice.  Otherwise, we just want the single element.
;;;
;;; Revision 1.130  2003/11/14 04:29:31  rtoy
;;; Handle function calls to statement functions too so we can generate
;;; the correct args for them.  Do this by adding a hash table to hold
;;; info about statement functions.
;;;
;;; 	(*f2cl-statement-finfo*): New variable holding the hash-table for
;;; 	of function info for statement functions.
;;; 	(f2cl): Give better descriptions of some options in the docstring.
;;; 	(translate-and-write-subprog): Clear out the hash-table for the
;;; 	statement function info.
;;; 	(parse-arrayref-or-stmtfn): Save away function info when we find a
;;; 	statement function.
;;; 	(maybe-convert-array-ref-to-slice): If the function is in the
;;; 	arglist, we can't do anything special about it.  Otherwise, try to
;;; 	find the function in the global database or the statement-function
;;; 	database so we can generate the correct array references.
;;;
;;; Revision 1.129  2003/11/14 02:55:54  rtoy
;;; 	* src/f2cl1.l (maybe-convert-array-ref-to-slice): When looking up
;;; 	the function name to get the argument types, we need to be
;;; 	careful.  If the function is an EXTERNAL function (i.e., a
;;; 	parameter to the function we're compiling), it doesn't necessarily
;;; 	have the same types as a global function with the same name.
;;;
;;; Revision 1.128  2003/11/13 22:37:08  rtoy
;;; Oops.  We want subtypep, not typep!
;;;
;;; Revision 1.127  2003/11/13 22:16:59  rtoy
;;; Try to be smarter about generating args to functions, which is an
;;; issue if the arg is an element of an array.  If we know the declared
;;; types of the function, try to generate the appropriate arg, meaning
;;; either a single element of the array or a slice of the array.
;;;
;;; Revision 1.126  2003/11/13 21:06:22  rtoy
;;; Was not correctly handling a plain SAVE statement, which means save
;;; all locals.  Put a special token in this case to indicate that.
;;;
;;; Revision 1.125  2003/11/13 05:38:15  rtoy
;;; Define a pretty-printer for WITH-MULTI-ARRAY-DATA.
;;;
;;; Revision 1.124  2003/11/12 05:32:03  rtoy
;;; Many changes to make assigned gotos work
;;;
;;; o Add *statement-labels* to hold a list of statement labels found in s
;;;   subprogram.  (Needed so we can branch to the correct label in
;;;   assigned goto statements.)
;;; o Use *statement-labels* in various places.
;;; o Unify the parsing of computed GOTOs.
;;; o Make assigned gotos work.
;;; o Add support for the ASSIGN statement (which was missing).
;;;
;;; Fixups for ENTRY points:
;;; o Fix up parsing of ENTRY points.  We weren't setting up
;;;   *entry-points* correctly, and did not handle the arglist correctly.
;;; o Parse entry points similarly to subroutine calls, using two new
;;;   functions:  ID-DEFINITION-ENTRY, PARSE-ENTRY-DEFINITION
;;;
;;; Revision 1.123  2003/07/13 18:58:30  rtoy
;;; Be more careful in generate-call-to-routine when the routine is
;;; actually funcalling a routine.
;;;
;;; Revision 1.122  2003/07/12 04:24:38  rtoy
;;; o Add new keyword parameter to specify the package to be used for
;;;   compiling the code.  Defaults to COMMON-LISP-USER
;;; o When generating a call to a routine, we need to check for reserved
;;;   Lisp names and mangle it appropriately.  Use the new name as needed.
;;; o When parsing a do loop, we need to check reserved lisp names
;;;   for the loop variable.  (Because other places will have mangled the
;;;   name).
;;;
;;; Revision 1.121  2003/01/08 18:41:46  rtoy
;;; Reference symbols in the common-lisp package with "common-lisp:",
;;; instead of "lisp:".
;;;
;;; Revision 1.120  2003/01/08 18:19:00  rtoy
;;; Was incorrectly converting things like
;;;
;;; 	character*8 s
;;; 	data s/'z'/
;;;
;;; to
;;;
;;;   (let ((s (make-array 8 :element-type 'base-char :initial-element #\space)))
;;;     (setf s "z"))
;;;
;;; It should really be
;;;
;;;   (let ((s (make-array 8 :element-type 'base-char :initial-element #\space)))
;;;     (replace s "z"))
;;;
;;; We really should do this when making the array, not afterwords.
;;;
;;; Bug noted by Christophe Rhodes.
;;;
;;; Revision 1.119  2002/09/13 17:50:18  rtoy
;;; From Douglas Crosher:
;;;
;;; o Make this work with lower-case Lisps
;;; o Fix a few typos
;;; o Make a safer fortran reader.
;;;
;;; Revision 1.118  2002/07/02 21:33:24  rtoy
;;; Always start output on a newline when printing the name of the source
;;; and output files.
;;;
;;; Revision 1.117  2002/06/30 13:09:58  rtoy
;;; Let f2cl also keep track of a list of functions that a function
;;; calls.  (Useful for generating dependencies.)
;;;
;;; Revision 1.116  2002/05/07 03:56:07  rtoy
;;; o In TRANSLATE_LINT, change how output when *verbose* is set so we can
;;;   see better what f2cl is really reading.  (Was hard to differentiate
;;;   between strings and symbols, before, for example.)
;;; o In PARSE_UPPER_AND_LOWER_BOUNDS, we were returning T for unknown
;;;   array bounds.  This caused an extraneous Fortran variable T to be
;;;   introduced.  Return '* instead so we don't get the extraneous
;;;   variable anymore.
;;;
;;; Revision 1.115  2002/05/07 03:26:10  rtoy
;;; o With the function info changes, we were incorrectly saying a
;;;   variable was set when it might not have been.  Fix it.
;;; o Clean up/add a few comments
;;;
;;; Revision 1.114  2002/05/07 03:06:17  rtoy
;;; o Include a date on the version string.
;;; o Don't print out the date in the generated file.
;;;
;;; Revision 1.113  2002/05/06 18:05:15  rtoy
;;; o We need to have d1mach and i1mach as known functions, so initialize
;;;   and clear the hash table appropriately.
;;; o When generating a call to a routine, print a warning if we don't
;;;   know the function.  (User should check to see if the call is
;;;   correct.)
;;; o Remove an extraneous debugging print statement.
;;;
;;; Revision 1.112  2002/05/05 23:37:50  rtoy
;;; Was not generating calls to routines correctly when a parameter is
;;; given multiple times in the arg list.  Don't check for duplicates.
;;;
;;; (I'm a little fuzzy on the rules of Fortran on aliasing of parameters
;;; of routines.  I think you're not allowed, so having an actual
;;; parameter be an input and output is not allowed.  This almost always
;;; works, however.)
;;;
;;; Revision 1.111  2002/05/05 21:09:49  rtoy
;;; f2cl-compile needs to bind *READ-DEFAULT-FLOAT-FORMAT* before
;;; compiling the Lisp file so that numbers are read in the specified
;;; format.
;;;
;;; Revision 1.110  2002/05/04 20:32:36  rtoy
;;; If the entry for the function already exists, we don't want to smash
;;; it.  Just update the return-values for the entry.
;;;
;;; Revision 1.109  2002/05/04 17:00:06  rtoy
;;; We now keep a hash table of all functions and their return values and
;;; use that, if available, for generating calls to that function.
;;;
;;; This is experimental, but seems to work so far, and generates better code.
;;;
;;; Revision 1.108  2002/05/03 17:42:13  rtoy
;;; Allow other keys for f2cl and f2cl-compile.
;;;
;;; Revision 1.107  2002/04/18 13:05:19  rtoy
;;; Added :FLOAT-FORMAT option to F2CL and F2CL-COMPILE so that the user
;;; can specify how to print out numbers in case the user is going to
;;; read/compile the file using some other setting for
;;; *READ-DEFAULT-FLOAT-FORMAT*.  (This is a simple hack to get around the
;;; problem of not having a portable way to specify all numbers should be
;;; printed with an exponent marker.)
;;;
;;; Revision 1.106  2002/03/22 23:00:12  rtoy
;;; When generating the call to a routine, we don't have to have a setter
;;; if the same arg is used multiple times in the arglist.  This is
;;; undefined Fortran behavior.
;;;
;;; Revision 1.105  2002/03/19 23:11:12  rtoy
;;; Be conservative: For F2CL-COMPILE, change the default :array-type to
;;; be :array instead of :simple-array.
;;;
;;; Revision 1.104  2002/03/19 06:03:14  rtoy
;;; First pass at adding support for ENTRY statements (multiple entry
;;; points into a routine).  See NOTES for description of technique.
;;;
;;; Revision 1.103  2002/03/19 04:10:05  rtoy
;;; Comment out some debugging print statements.
;;;
;;; Revision 1.102  2002/03/19 01:45:20  rtoy
;;; Oops.  Remove the debugging print statements.
;;;
;;; Revision 1.101  2002/03/18 23:34:15  rtoy
;;; Was not correctly handling some implied do loops containing multiple
;;; variables in the loop in data statements.  Fix that and clean up some
;;; of the processing.  (Should probably do this kind of work in the f2cl
;;; compiler instead of at runtime, but it's only done once at runtime, so
;;; it's not a big deal.)
;;;
;;; Revision 1.100  2002/03/16 15:21:28  rtoy
;;; If an argument to a subprogram is not assigned to, return NIL as the
;;; value instead of the argument.  (See NOTES file.)
;;;
;;; Revision 1.99  2002/03/13 03:58:48  rtoy
;;; Use INT instead of TRUNCATE.
;;;
;;; Revision 1.98  2002/03/10 15:45:06  rtoy
;;; Oops.  A call to HANDLE-EXTENDED-DO was inadvertently left in.
;;;
;;; Revision 1.97  2002/03/07 19:00:39  rtoy
;;; o Merge the extended DO handling with the
;;;   write-statement-with-format-string handling into one place.
;;;   Reorderd the code a bit too.
;;; o Instead of using truncate to convert a float to an int for
;;;   assignement, use the Fortran INT.
;;; o When coercing a integer type to a float for assignment, declare the
;;;   int to be an INTEGER4 to help the coercion use a single
;;;   instruction.
;;;
;;; Revision 1.96  2002/03/07 05:19:59  rtoy
;;; o Comment out some debugging print statements.
;;; o Wasn't rewriting the write statement correctly.  This works much
;;;   better.
;;;
;;; Revision 1.95  2002/03/07 04:57:22  rtoy
;;; First cut at handling write(*, <fmt-string>).  Do this be converting
;;; to write(*,<number>) with a new format statement containing the format
;;; string.
;;;
;;; Revision 1.94  2002/03/06 03:16:37  rtoy
;;; Oops.  A block data subprogram looks like "blockdata <name>" or "block
;;; data <name>" where <name> is optional.
;;;
;;; Revision 1.93  2002/03/06 02:49:07  rtoy
;;; o Correct some comments.
;;; o BLOCKDATA subprograms can have names, so make that part of the
;;;   translated blockdata subprogram name.
;;;
;;; Revision 1.92  2002/03/01 02:41:54  rtoy
;;; Add some pretty-printers so the resulting code looks a bit neater.
;;;
;;; Revision 1.91  2002/02/17 15:51:19  rtoy
;;; With the new array-slicing method, the default array type can be
;;; simple-array again.
;;;
;;; Revision 1.90  2002/02/10 03:42:45  rtoy
;;; Since :array-slicing defaults to T, make :array-type default to :array
;;; instead of :simple-array.
;;;
;;; Revision 1.89  2002/02/09 16:10:45  rtoy
;;; o Add new var *DECLARE-COMMON-BLOCKS*
;;; o F2CL and F2CL-COMPILE take a new arg :declare-common, defaulting to
;;;   NIL, which allows the user to specify if the structures for the
;;;   common blocks in this file should be declared in this file.
;;;
;;; Revision 1.88  2002/02/08 23:28:26  rtoy
;;; Off-by-one error in initializing an array with FILL.  We didn't fill
;;; the last element!
;;;
;;; Revision 1.87  2002/02/08 04:24:38  rtoy
;;; Add support for BLOCK DATA subprograms.
;;;
;;; Revision 1.86  2002/02/08 03:35:30  rtoy
;;; We need to also fix up the names for any initialization stuff, so add
;;; new function FIXUP-F2CL-LIB to do it.
;;;
;;; Revision 1.85  2002/01/13 16:27:56  rtoy
;;; o Move the intrinsic function names from macros.l to here.  Include a
;;;   copy of the deftypes in macros.l here as well.  (Be sure to keep
;;;   them in sync!  I wish I knew a better way....)
;;; o Do not print out (use-package :f2cl) in the output file anymore.
;;; o In the generated code, we know look through the code and any symbol
;;;   that is string-= to a exported symbol in f2cl-lib is replaced by the
;;;   corresponding symbol from f2cl-lib.  That we the generated code can
;;;   reference the f2cl-lib without clashing with whatever other packages
;;;   the code might be used in.
;;;
;;; Revision 1.84  2002/01/08 20:53:48  rtoy
;;; PARSE-PARAMETER was incorrectly parsing parameter statements like
;;; parameter (k2prim = K2 - K1*MW/MD) because it never expected the rhs
;;; to be an expression.
;;;
;;; Revision 1.83  2002/01/07 18:16:15  rtoy
;;; o Change the :array-type parameter to take a keyword instead of a
;;;   symbol.
;;; o Print a warning if :array-type is specified and inconsistent with
;;;   :array-slicing option.  :array-slicing takes precedence.
;;; o If possible convert a fortran_comment into a quoted string.  (Makes
;;;   it easier to read the embedded comment.)
;;; o Print the options in lower case.
;;;
;;; Revision 1.82  2002/01/07 03:09:05  rtoy
;;; Print out a warning if a variable is declared that has the same name
;;; as a Fortran intrinsic.  Not sure what the spec says, but as long as
;;; it's a variable and not a redeclaration of the function, then I think
;;; it's ok.  If not, the warning tells you something might not be right.
;;;
;;; Revision 1.81  2002/01/06 23:28:16  rtoy
;;; Missed a few renamings of *intrinsic_function_names* and
;;; *external_function_names*.
;;;
;;; Revision 1.80  2002/01/06 23:10:11  rtoy
;;; Rename *intrinsic_function_names*, *external_function_names* and
;;; *subprog_stmt_fns* to use dashes.
;;;
;;; Revision 1.79  2002/01/05 19:01:21  rtoy
;;; Don't print out the copyright messages when running f2cl.
;;;
;;; Revision 1.78  2002/01/05 18:30:35  rtoy
;;; o Clisp's pretty-printer seems to work well enough now, so use it.
;;; o Use with-standard-io-syntax in SPECIAL-PRINT when printing out the
;;;   code (with a few minor changes).
;;;
;;; Revision 1.77  2001/09/11 14:29:14  rtoy
;;; Try to do a better job of figuring out the extension for the output
;;; file.
;;;
;;; Revision 1.76  2001/06/04 17:20:10  rtoy
;;; CONVERT-DATA-TYPE was incorrectly returning T instead of '(LOGICAL)
;;; for LOGICAL Fortran types.
;;;
;;; Revision 1.75  2001/06/04 17:14:38  rtoy
;;; Handle IMPLICIT NONE by putting ":NONE (A-Z)" as the type for
;;; *implicit_vble_decls*.
;;;
;;; Revision 1.74  2001/06/04 14:31:20  rtoy
;;; Recognize IMPLICIT NONE, but the semantics are NOT currently
;;; implemented:  undeclared variables will still be declared with default
;;; implicit rules even when implicit none is given.  This is probably ok,
;;; because this would be invalid Fortran anyway, and f2cl is only
;;; expected to process valid Fortran.
;;;
;;; Revision 1.73  2001/06/03 20:49:00  rtoy
;;; o Removed an old unused version of TRANSLATE-AND-WRITE-SUBPROG.
;;; o Gratuitously re-indented PARSE-DO-LOOP.
;;; o Key change is adding code to handle extended DO loops, i.e., DO
;;;   loops that don't have statement numbers and are ended with an ENDDO
;;;   statement.
;;;
;;; Revision 1.72  2001/06/01 20:08:21  liam
;;; Remove conditionalization #+cmu on concat.  Removed extra parenthesis.
;;;
;;; Revision 1.71  2001/04/30 15:37:34  rtoy
;;; Add in-package statement, just like the comments say instead of trying
;;; to compile everything in the given package.
;;;
;;; Revision 1.70  2001/02/26 15:38:23  rtoy
;;; Move *check-array-bounds* from f2cl1.l to macros.l since the generated
;;; code refers to it.  Export this variable too.
;;;
;;; Revision 1.69  2000/09/01 16:33:25  rtoy
;;; MAYBE-CONVERT-ARRAY-REF-TO-SLICE: the expression isn't always a list!
;;; Check for that.  Fix a typo too.
;;;
;;; Revision 1.68  2000/09/01 13:54:26  rtoy
;;; o F2CL-COMPILE:  not all COMPILE-FILE's have the :ERROR-FILE option.
;;; o Added MAYBE-CONVERT-ARRAY-REF-TO-SLICE to convert array refs to
;;;   slices if appropriate.
;;; o PARSE-SUBROUTINE-CALL:  we were not careful enough about array
;;;   slicing.  We should only apply array slicing if the argument is an
;;;   array ref.  Any arithmetic should disable array-slicing.
;;;
;;; Revision 1.67  2000/08/30 16:54:04  rtoy
;;; In F2CL-COMPILE, make :output-file default to T.  CMUCL won't produce
;;; an output if it's NIL.
;;;
;;; Revision 1.66  2000/08/29 15:52:35  rtoy
;;; Need to coerce the RHS of statement functions to the type of the
;;; statement function itself.
;;;
;;; Revision 1.65  2000/08/18 15:08:49  rtoy
;;; Gratuitous change from prog to let in TRANSLATE-AND-WRITE-SUBPROG.
;;;
;;; Revision 1.64  2000/08/13 04:16:53  rtoy
;;; Oops!  CONCAT is still being used!  Reinstate it.
;;;
;;; Revision 1.63  2000/08/10 13:50:42  rtoy
;;; o UPDATE_CM_ARRAY_DIMS was not handling the new method of storing
;;;   dimensions.  Fix it.
;;; o Removed unused CONCAT function.
;;; o Gratuitous mods to PARSE-COMMON.
;;;
;;; Revision 1.62  2000/08/09 22:33:27  rtoy
;;; The preprocessor converted // to f2cl-//.  We need to undo that
;;; because // in a COMMON statement really means the blank common block.
;;; For example:
;;;
;;; 	COMMON /c1/a, b//d, e
;;;
;;; The variables d and e are in the blank common block.  The file
;;; val/commontest.for should now be converted correctly.
;;;
;;; Revision 1.61  2000/08/09 18:50:30  rtoy
;;; Fortran says I can build up the elements of a common block in pieces
;;; by specifying the pieces in several separate common statements.
;;; Support that.
;;;
;;; Revision 1.60  2000/08/07 13:02:21  rtoy
;;; Make :keep-lisp-file default to T.
;;;
;;; Revision 1.59  2000/08/05 19:16:46  rtoy
;;; o Add special var *PARSING-LHS*
;;; o Add function F2CL-COMPILE to compile a Fortran file to object code
;;;   so the user doesn't have to call compile-file himself.
;;; o In PARSE-ASSIGNMENT, call PARSE-EXPRESSION appropriately if we're
;;;   parsing the LHS or RHS of an assignment.  (Prevents mangling of
;;;   statement function definitions into multiple-value-bind's of the
;;;   function.)
;;; o Changes to GENERATE-CALL-TO-ROUTINE:
;;;     o Revert back to just naming the variables sequentially.  The
;;;       problem is if the same variable is used more than once in the
;;;       parameter list.  (We may want to add this back in eventually.)
;;;     o Don't try to assign to constants like %TRUE% and %FALSE%.
;;;     o Only try to assign a new value to the parameter if the function
;;;       actually returned a new (non-NIL) value.  This allows for
;;;       functions that don't return extra parameters (like intrinsics)
;;;       to still be used without f2cl knowing exactly the calling sequence.
;;;
;;; Revision 1.58  2000/08/04 14:20:31  rtoy
;;; Add very rudimentary support for Fortran READ statements.  This means
;;; we just basically call read and assign the result to the (simple)
;;; variable.  We don't even bother to look at the format number or check
;;; the variable type.
;;;
;;; Revision 1.57  2000/08/01 22:45:46  rtoy
;;; o Add a few comments.
;;; o GENERATE-CALL-TO-ROUTINE was crashing when passed an array-slice.
;;;   We also hosed up function calls where no setters were needed.
;;;
;;; Revision 1.56  2000/07/31 04:08:35  rtoy
;;; Remove unused function PARSE-FUNCTION-CALL.
;;;
;;; Revision 1.55  2000/07/31 03:00:20  rtoy
;;; o Remove the support for continuation lines in
;;;   READSUBPROG-EXTRACT-FORMAT-STMTS since the preprocessing handles
;;;   that now.  Make this routine easier to read too(?).
;;; o Make CONCAT-OPERATORS more lispy.
;;; o Remove unused code that was replaced with new versions.
;;;
;;; Revision 1.54  2000/07/30 06:12:23  rtoy
;;; o In GENERATE-CALL-TO-ROUTINE, don't use VAR-n for the variable names;
;;;   prepend NEW- to the actual variable names.  Some gratuitous
;;;   re-indenting.
;;;
;;; o In PARSE-SUBROUTINE-CALL, subroutines can be passed in so we need to
;;;   funcall them, just like we do for functions.
;;;
;;; Revision 1.53  2000/07/30 05:54:06  rtoy
;;; Create new function GENERATE-CALL-TO-ROUTINE that takes the heart of
;;; the multiple-value-bind stuff needed for getting the return values of
;;; subroutines.  Extend to handle functions.  Use this new routine in
;;; PARSE-SUBROUTINE-CALL.
;;;
;;; Revision 1.52  2000/07/30 04:33:19  rtoy
;;; READSUBPROG-EXTRACT-FORMAT-STMTS:
;;;
;;;   If the very first line had a line number, f2cl wouldn't understand the
;;;   line.  Fix this be reading the margin at the top of the main loop.
;;;   Also, don't do anything with MULTIPLE-LINE-FLAG since PREPROCESS now
;;;   handles line continuations.  (Need to remove MULTIPLE-LINE-FLAG).
;;;
;;; TRANSLATE-AND-WRITE-SUBPROG:
;;;
;;;   By Fortran calling rules, functions can actually modify the input
;;;   parameters.  Thus, functions need to return the function value and
;;;   all of the parameters.  (Still need to modify the code that
;;;   generates the caller so we can update the values appropriately.)
;;;
;;; Revision 1.51  2000/07/28 22:08:31  rtoy
;;; Take out the pprint-logical-block if we're using Clisp since it
;;; doesn't have it.
;;;
;;; Revision 1.50  2000/07/28 16:59:27  rtoy
;;; o We are in the f2cl package now.
;;; o Read the preprocessed file in the f2cl package instead of the user
;;;   package.
;;; o Convert the Fortran string concatenation operator (//) to f2cl-//.
;;;
;;; Revision 1.49  2000/07/27 16:40:27  rtoy
;;; o We want to be in the CL-USER package, not the USER package.
;;; o Clisp doesn't have pprint-logical-block.
;;;
;;; Revision 1.48  2000/07/21 21:56:37  rtoy
;;; Squash another parsing bug in initializing a single element of an
;;; array.
;;;
;;; Revision 1.47  2000/07/21 21:14:21  rtoy
;;; The last change to PARSE-DATA broke the case of
;;;
;;; 	dimension x(3)
;;; 	data x/1, 2, 3/
;;;
;;; Make sure we are really initializing an array and not just one element
;;; of the array.
;;;
;;; Revision 1.46  2000/07/21 17:39:27  rtoy
;;; PARSE-DATA and friends were mishandling the case
;;;
;;; 	DATA array/1,2,3,4/
;;;
;;; where array was an array.
;;;
;;; Revision 1.45  2000/07/20 13:40:52  rtoy
;;; o PARSE-DATA was not correctly handling data statements of the form:
;;;
;;; 	data x(1),x(2),x(3)/n1, n2, n3/
;;;
;;; o FIX-UP-NEGATIVE-NUMBER in PARSE-DATA1 didn't handle the case when
;;;   passed a value like 1.0d%1 which f2cl had converted from 1.0d-1.
;;;
;;; Revision 1.44  2000/07/19 22:16:34  rtoy
;;; o TRANSLATE-AND-WRITE-SUBPROG doesn't have the :declaim, :package, and
;;;   :options arguments anymore.
;;; o Clean out unused code.
;;;
;;; Revision 1.43  2000/07/19 13:47:06  rtoy
;;; o Only print out one banner and version number, not one for each
;;;   subprog!
;;; o In PARSE-DATA1, FREF now requires the dimensions of the array as the
;;;   third argument. (Needed to support 1-d arrays and slicing.)
;;;
;;; Revision 1.42  2000/07/18 14:07:05  rtoy
;;; Make the appropriate code changes due to the change in usage in
;;; UPDATE-CALLED-FUNCTIONS-LIST since the name arg is now either a list
;;; of the name for functions or a list of the name and :subroutine for
;;; subroutines.
;;;
;;; Revision 1.41  2000/07/14 16:45:54  rtoy
;;; o Allow the user to specify what package the resulting file should be
;;;   in and any declaims he wants.
;;; o Print out some additional information in the result like the f2cl
;;;   version and the compilation options.
;;; o Added *f2cl-version*
;;;
;;; Revision 1.40  2000/07/14 15:44:46  rtoy
;;; o Added keyword :array-slicing to f2cl to support array slicing.
;;; o Preliminary support for array slicing.  That is,
;;;
;;; 	real x(100)
;;; 	call sub(x(4))
;;;
;;;   means the subroutine sub actually gets an array of size 96 starting
;;;   from x(4).
;;;
;;;   There are some problems with this.  If sub actually wanted a simple
;;;   real variable, we'll be passing the wrong thing to sub.  f2cl in
;;;   general doesn't know what type of parameters sub wants.
;;;
;;;   To work around this problem, either run f2cl with :array-slicing set
;;;   to NIL, or change the call to something like:
;;;
;;; 	real x(100)
;;; 	real tmp
;;;
;;; 	tmp = x(4)
;;; 	call sub(tmp)
;;; 	x(4) = tmp
;;;
;;;   (That last assignment needed only if sub is modifies the parameter.)
;;;
;;; o In parsing a subroutine call, don't use the multiple-value-setq
;;;   version at all anymore.  Use the multiple-value-bind version
;;;   instead.
;;;
;;; Revision 1.39  2000/07/14 14:05:45  rtoy
;;; Allow the user to specify whether he wants f2cl to declare arrays as
;;; type array or simple-array.
;;;
;;; Revision 1.38  2000/07/14 13:30:08  rtoy
;;; o Computed goto apparently can have a comma before the expression, and
;;;   we weren't handling this correctly.  (At least g77 allows it, even
;;;   though I can't find it mentioned in the Fortran 77 standard.)
;;;
;;; o When doing a subroutine call, don't try to assign the return value
;;;   to external functions.  Fortran can't return functions.
;;;
;;; Revision 1.37  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
