;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Memoization; Base: 10 -*-

(in-package "MEMOIZATION")

;;;===========================================================================
;;; (C) 1992 Marty Hall. Permission is granted for any use or modification
;;      of this code provided this notice is retained. Version of 8/93.
;;;===========================================================================

;;;===========================================================================
;;; This file is one of four files that define the Memoization facility:
;;;    - Load-Memoization: Defines Memoization package and loads other 3 files
;;;    - Memoization [THIS FILE]: Defines core memoization routines.
;;;    - Save-Memo-Table: Defines routines to save and load hash tables
;;;                       associated with a memoized function.
;;;    - Memoization-Examples: Simplistic version of memoization to illustrate
;;;                            the principle, and 2 example functions to which
;;;                            memoization can be applied.
;;; 
;;; This file [Memoization] is divided into the following major sections:
;;;    - Section 1: A high level description of the main user routines
;;;    - Section 2: A quick overview of the applications of memoization
;;;    - Section 3: The source code for the main user routines
;;;    - Section 4: The source code for lower-level internal routines
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
;;; SECTION 1 - High Level Description
;;; ----------------------------------
;;; Memoization routines and utilities. The idea of memoization is that it
;;; allows a function to "remember" previous invocations, returning the
;;; previously calculated values (rather than recalculating) if it is called
;;; with exactly the same arguments as in a previous execution. This can
;;; result in tremendous speedups if calculations are repeated at various
;;; points in a program's execution, yet while remaining somewhat transparent
;;; to the users of the code. 
;;;
;;; The main user routines:
;;; =======================
;;;
;;; DEFINE-MEMO-FUNCTION: a macro that can be used just like "defun", but
;;;   which has the result of defining a memoized function. Also updates
;;;   the doc string and the results of calling "Arglist" (if available in
;;;   current LISP implementation) on that function name. Any of the keywords
;;;   acceptable to Memoize can optionally be passed on, resulting in 
;;;   specialized versions of memoization that seed their initial hash 
;;;   tables from the disk, use particular hash table tests, etc.
;;; WITH-MEMOIZATION: a macro that takes a list of function names and any
;;;   number of LISP forms and executes them in a context where the
;;;   functions are temporarily memoized.
;;;     (With-Memoization (Foo Bar Baz)
;;;       (Form-1)
;;;       (Form-2))    results in executing the two forms while functions
;;;   named Foo, Bar, and Baz are memoized. Useful for getting a quick feel
;;;   for the potential speed benefits of memoization.
;;; WITHOUT-MEMOIZATION: a macro that executes LISP forms in a context
;;;   where all memoization is temporarily turned off.
;;;   (Without-Memoization
;;;     (Form-1)
;;;     (Form-2))  executes the two forms with no functions memoized.
;;; MEMOIZE: Takes a function name and changes its function definition to
;;;   be a memoized function. 
;;;     (defun Foo (Args) <Body of Foo>)  followed by 
;;;     (Memoize 'Foo) has the same effect as doing 
;;;     (Define-Memo-Function Foo (Args) <Body of Foo>), but calling
;;;   "Memoize directly is sometimes more convenient when testing things
;;;   out, as it requires no changes in the preexisting code.
;;; SAVE-MEMO-TABLE: Saves to disk a definition of the hash table
;;;   associated with a given memoized function name. By defining a
;;;   memoized function via 
;;;      (Define-Memo-Function Foo (<Args>)
;;;        <Body>)
;;;    running the time-consuming cases off-line, calling
;;;      (Save-Memo-Table '<Function-Name>)
;;;    then using
;;;      (Define-Memo-Function Foo (<Args>)
;;;        (:Hash-Table-Source :Disk)
;;;        <Body>)
;;;   or by calling Memoize with the :Hash-Table-Source set to :Disk,
;;;   you can have a function "remember" the values it calculated, not only
;;;   in the current run but also in the previous off-line run.
;;; CLEAR-MEMO-TABLE: Takes a function name and clears out the memo table
;;;   associated with the function. Useful when some internal change makes
;;;   the previously stored values incorrect.
;;; UNMEMOIZE: Takes a function name and returns it to the unmemoized form.
;;;   Useful for timing and for debugging, especially tracing recursive
;;;   routines. In combination with "Memoize", this lets you switch back
;;;   and forth between memoized and normal versions without changing or
;;;   reloading the code. Similarly, Unmemoize-Functions takes a list
;;;   instead of a single one, and Unmemoize-All-Functions unmemoizes
;;;   everything.
;;; REMEMOIZE: Takes the name of a function that is currently unmemoized,
;;;   but which had previously been memoized. Memoizes it again, but uses
;;;   the previous hash table instead of creating a new one. Similarly,
;;;   Rememoize-Functions applies to a list.
;;; MEMOIZED-FUNCTION-CALL-COUNT: Given the name of a memoized function,
;;;   tells how many times that function has been called, and which of
;;;   those were simple table lookups that had been stored from a previous
;;;   invocation, vs how many were newly calculated using the original
;;;   function. For a normal memoized function, lets the user see if
;;;   memoization is paying off after a long period of time. For a function
;;;   whose memo table was stored on disk, lets the user see if the stored
;;;   values covered all or most of the cases.
;;; MEMOIZED-TIME: Takes a list of functions and a single form and evaluates
;;;   and times the form 3 times, once without memoization, once with
;;;   memoization and an empty cache, and once with memoization but the
;;;   full cache from the previous run. 
;;; *MEMOIZED-FUNCTION-NAMES*: a list of the currently memoized functions.
;;;
;;; "Memoize", "Memo", and "Clear-Memo-Table" were based on code in Peter
;;; Norvig's outstanding book _Paradigms of AI Programming: Case Studies in
;;; Common LISP_, Morgan Kaufmann, 1992, which in turn was inspired by code in
;;; ex. 3.27 of Abelson, Sussman, and Sussman's _Structure and Interpretation
;;; of Computer Languages_, MIT Press, 1985. Comments and suggestions on the
;;; code were given by Jim Mayfield (University of Maryland Baltimore County),
;;; J. Paul McNamee (AAI Corporation), Peter Norvig (Sun Microsystems), and
;;; David Scheerer (The Johns Hopkins Applied Physics Lab).
;;;===========================================================================

;;;===========================================================================
;;; SECTION 2 - Applications
;;; ------------------------
;;; There are four basic applications of memoization. The first, which is
;;; illustrated below, is when a single routine calls some subroutine (or
;;; itself recursively) more than is needed, resulting in extra calculations.
;;; By memoizing, these results are returned immediately for subsequent calls,
;;; with the effect of dynamic programming. In fact, this first case can be
;;; thought of as a tool for automatic dynamic programming, but without the
;;; need to build the subpieces in the correct order. This can frequently
;;; reduce the time of exponential algorithms to polynomial or even linear
;;; time. Given enough thought, this can be solved without an automatic
;;; memoization facility by either building up the subpieces in the proper
;;; order or maintaining a special purpose local data structure to retain the
;;; results (ie "manual" memoization). The advantage of doing it automatically
;;; is that less debugging and testing is required if the simple algorithm has
;;; been already tested, the versions can be changed back and forth
;;; interactively at run time, it is more transparent, and most importantly it
;;; is simple and easy to use.
;;;
;;; The second case is for invocations of a function that are repeated over
;;; time, but from scattered places in the program, or even when revoked
;;; repeatedly by a user in an interactive program. This generally yields a
;;; speedup by a constant factor, but that factor may be large. Without an
;;; automatic memoization facility, the only alternative is maintaining a
;;; special purpose global data structure, requiring testing and debugging,
;;; and much extra effort for something that at best is equally efficient as
;;; memoization.
;;;
;;; The third case is when a function is so expensive that you want to
;;; perform the calculations off-line and save the results for a later
;;; session. The automatic memoization facility provides a simple and
;;; transparent method to save the results and have them associated with the
;;; function automatically in a later session.
;;;
;;; The final case is when using memoization as a tool in conventional
;;; performance profiling and optimization. Many implementations provide some
;;; sort of a metering system, and this should be used for major test cases.
;;; However, there is often tremendous overhead involved, with 20-30x slower
;;; performance when a routine is fully metered. For quick test cases, it is
;;; often useful to know if speeding up a particular routine will have much
;;; effect on the top-level timing. By using Memoized-Time or
;;; With-Memoization, a user can memoize the routines in question then run the
;;; same test case multiple times. If the identical test case runs only, say
;;; 5% faster even during the second memoized run, then this suggests that no
;;; amount of memoization in the routines in question will make more than a 5%
;;; difference in the performance of the test case, and that this is likely
;;; not the place to begin the optimization efforts.
;;;===========================================================================


;;;===========================================================================
;;; SECTION 3 - The main user routines
;;; ----------------------------------
;;; A quick summary of these is provided at the top in section 1.
;;;===========================================================================

;;;===========================================================================
;;; This records the name of every function that is memoized. 

(defvar *Memoized-Function-Names* '() )

;;;===========================================================================
;;; Defines a macro that you use like "defun" but that automatically memoizes 
;;; the resultant function. REMEMBER! This is a macro, and thus changes here 
;;; do not get propagated to files that use this until they are recompiled. So, 
;;; if you want to change this in an incompatible manner, check
;;; *Memoized-Function-Names* first, then be sure to recompile all the files 
;;; that use those functions afterwards.
;;;
;;; After Function-Name and Arguments but before the function body, a list
;;; of keywords and values for Memoize may be specified, as shown in the example
;;; in the doc string below. See "Memoize" for a list of legal keywords and an
;;; explanation of their values.

(defmacro Define-Memo-Function (Function-Name Arguments &body Keys-and-Fn-Body)
  "Same syntax as `defun', but defines a memoized function. After the function
   args, but before the function body, you can optionally supply a list of 
   keywords to Memoize. Eg 
       (Define-Memo-Function Square (X) (* X X))  OR
       (Define-Memo-Function Square (X)
         (:Hash-Table-Source :Create :Test #'eq) <-- Any keys `Memoize' accepts
         (* X X))"
  (let ((Keys '())
	(First-Entry (first Keys-and-Fn-Body)))
    (when (and (listp First-Entry)
	       (keywordp (first First-Entry)))
      (setq Keys First-Entry)
      (setq Keys-and-Fn-Body (rest Keys-and-Fn-Body)))
    `(progn
       (Unmemoize ',Function-Name)
       (apply #'Memoize
	      (defun ,Function-Name ,Arguments ,@Keys-and-Fn-Body)
	      ',Keys))  ))

;;;===========================================================================
;;; This and Define-Memo-Function (which expands into Memoize) are the main two
;;; functions user's would use to create their own memoized functions. Any
;;; keywords acceptable to Memoize can be passed to Define-Memo-Function as a
;;; list specified after the function-name and function-lambda-list, but before
;;; the function-body. See Define-Memo-Function for an example.
;;;
;;; Takes a function name, looks up the associated function object, and replaces
;;; it with a function object that "remembers" previous values. See "Memo" for
;;; more on how this is done. In addition, it updates the documentation string
;;; of the function to reflect that it is memoized, and sets up some
;;; bookkeeping that will be used to keep track of the number of times the
;;; new, memoized function gets called, broken down by whether or not it
;;; returned a cached value.
;;;
;;; "Test" must be a predicate acceptable to make-hash-table. Generally #'eq or
;;;   #'equal.
;;;
;;; "Key" is used if memoization is to be performed based on some transformation
;;;   (eg a subset) of the argument list.
;;;
;;; "Hash-Table-Source" is one of 
;;;   :Create       -- make a new hash table
;;;   :Disk         -- load a table from disk that was previously saved via
;;;                    Save-Memo-Table.
;;;   :Old-Function -- Use the previous hash table. This assumes this function 
;;;                    used to be memoized, was unmemoized, and now is being 
;;;                    memoized again (with the old table still on the function
;;;                    names :Memo-Table property).
;;;
;;; "One-Arg-p" is a flag indicating whether or not to use the restricted but
;;;   slightly faster version that assumes a single argument to the function
;;;   that can be compared via "eq".

(defun Memoize (Function-Name &key (Test #'equal)
		                   (Key #'identity)
				   (Hash-Table-Source :Create)
				   One-Arg-p)
  "Alters function to save previously calculated values in a hash table"
  (cond
    ((Memoized-Function-p Function-Name)
     (format t "~%~S is already memoized." Function-Name))
    (t
     (let ((Doc-String (documentation Function-Name 'function)))
       (push Function-Name *Memoized-Function-Names*)
       (if
	 One-Arg-p
	 (setf (symbol-function Function-Name)
	     (One-Arg-Memo (symbol-function Function-Name)
			   :Function-Name Function-Name))
	 (setf (symbol-function Function-Name)
	       (Memo (symbol-function Function-Name)
		     :Function-Name Function-Name
		     :Test Test
		     :Key Key
		     :Hash-Table-Source Hash-Table-Source)))
       (setf (get Function-Name :Unmemoized-Doc-String) Doc-String)
       (setf (documentation Function-Name 'function)
	     (Memoized-Doc-String Function-Name Doc-String))
       (setf (get Function-Name :Hash-Table-Lookups) 0)
       (setf (get Function-Name :Original-Function-Calls) 0))))
  Function-Name
)

;;;===========================================================================
;;; Assumes default values for test, key, Hash-Table-Source, and One-Arg-p.
;;; "dolist" instead of "loop" to maintain CLtL/1 compatibility.

(defun Memoize-Functions (Function-Names  &key (Test #'equal)
			                       (Key #'identity)
					       (Hash-Table-Source :Create)
					       One-Arg-p)
  (dolist (Function-Name Function-Names Function-Names)
    (Memoize Function-Name
	     :Test Test
	     :Key Key
	     :Hash-Table-Source Hash-Table-Source
	     :One-Arg-p One-Arg-p)) )

;;;===========================================================================
;;; The unwind-protect is in case the user aborts during the body, so that 
;;; things don't get left unmemoized. It is common for the user to abort
;;; during  the body, since the unmemoized version might be much slower than
;;; they  expected.

(defmacro Without-Memoization (&body Forms)
  "Executes forms with all memoization temporarily turned off"
  (let ((Arg (gensym "MEMOIZED-FUNCTIONS-")))
    `(let ((,Arg *Memoized-Function-Names*))
       (unwind-protect
	   (progn
	     (Unmemoize-All-Functions)
	     ,@Forms)
	 (Rememoize-Functions ,Arg))) ))

;;;===========================================================================
;;; Executes Forms in a context where the listed function names are
;;; temporarily memoized.

(defmacro With-Memoization ((&rest Function-Names) &body Forms)
  "Executes forms with the listed functions temporarily memoized"
  (let ((Arg (gensym "TEMPORARILY-MEMOIZED-FUNCTIONS-")))
    `(let ((,Arg ',Function-Names))
       (Memoize-Functions ,Arg)
       (unwind-protect
	   (progn ,@Forms)
	 (Unmemoize-Functions ,Arg))) ))
  
;;;===========================================================================
;;; Times the Form 3 times: without any new memoization, with memoization and
;;; an initially empty hash table, and with memoization but with the memo
;;; tables left in their full state from the previous run.

(defmacro Memoized-Time ((&rest Function-Names) Form)
  "Times form 3 times: without new memoization, with memoization and an
   initially empty table, and then again without clearing the table"
  `(progn
     (format t "~%---------------------------------------------")
     (format t "~%Without additional memoization:")
     (time ,Form)
     (With-Memoization ,Function-Names
       (format t "~%---------------------------------------------")
       (format t "~%First memoized invocation:")
       (time ,Form)
       (format t "~%---------------------------------------------")
       (format t "~%Second memoized invocation:")
       (time ,Form))
     (format t "~%---------------------------------------------"))
)

;;;===========================================================================
;;; Given the name of a memoized function, tells how many times that function
;;; has been called, and which of those were simple table lookups that had
;;; been stored from a previous invocation, vs how many were newly calculated
;;; using the original function. For a normal memoized function, lets the user
;;; see if memoization is paying off after a long period of time. For a
;;; function whose memo table was stored on disk, lets the user see if the
;;; stored values covered all or most of the cases.

(defun Memoized-Function-Call-Count (Function-Name &key (Print-p t))
  "List of table lookups and calls to original function for a memoized function"
  (cond
    ((not (Memoized-Function-p Function-Name))
     (format t "~%~S is not memoized!" Function-Name))
    ((One-Arg-Memoized-Function-p Function-Name)
     (format t "~%~S was memoized with the restricted case that assumes ~%~
                only 1 argument and does not keep track of function calls."
	     Function-Name))
    (t
     (let* ((Table-Lookups (get Function-Name :Hash-Table-Lookups))
	    (Function-Calls (get Function-Name :Original-Function-Calls))
	    (Total (+ Table-Lookups Function-Calls))
	    (Table-Lookup-Percentage (if (> Total 0)
					 (round (* 100 (/ Table-Lookups Total)))
					 NIL)))
       (when Print-p
	 (if Table-Lookup-Percentage
	     (format t "~%Function ~S was invoked ~S times. ~%~
                        ~S% were memo table lookups, and ~S% ~
                         were calls to the original function."
		     Function-Name
		     Total
		     Table-Lookup-Percentage
		     (- 100 Table-Lookup-Percentage))
	     (format t "~%Function ~S was never invoked." Function-Name)))
       (list Table-Lookups Function-Calls))) ))


;;;===========================================================================
;;; This is needed if you want to clear out the old values, but leave the
;;; function memoized for the future. This is often done if one of the
;;; subfunctions that Function-Name calls changes, so that the previously
;;; stored values are no longer accurate.

(defun Clear-Memo-Table (Function-Name)
  "Removes all entries from hash table associated with memoized function"
  (let ((Hash-Table (get Function-Name :Memo-Table)))
    (when Hash-Table
      (clrhash Hash-Table))))

;;;===========================================================================
;;; Takes a list of names instead of just one.

(defun Clear-Memo-Tables (Function-Name-List)
  "Clears the cache for a list of functions via calls to Clear-Memo-Table"
  (mapc #'Clear-Memo-Table Function-Name-List))

;;;===========================================================================
;;; Unmemoizes - changes the function definition back to the original one, and
;;; resets the doc string. Useful for timing and for debugging, especially
;;; with recursive code. The function returns t if the function was already
;;; memoized (and thus unmemoized), NIL if it was not already memoized (in
;;; which case no action was taken).

(defun Unmemoize (Function-Name)
  "Returns function to original, unmemoized form. Returns NIL if not memoized"
  (let ((Original-Function (get Function-Name :Unmemoized-Function)))
    (when
      Original-Function
      (setq *Memoized-Function-Names*
	    (remove Function-Name *Memoized-Function-Names*))
      (setf (symbol-function Function-Name) Original-Function)
      (setf (documentation Function-Name 'function)
	    (get Function-Name :Unmemoized-Doc-String))
      (setf (get Function-Name :Unmemoized-Function) NIL)
      t) ))

;;;===========================================================================
;;; Takes a list of names instead of just one.

(defun Unmemoize-Functions (Function-Name-List)
  "Unmemoizes a list of functions via successive calls to Unmemoize"
  (mapc #'Unmemoize Function-Name-List) )

;;;===========================================================================
;;; Unmemoizes all functions that are currently memoized, returning a list of
;;; their names. This is useful when doing time comparisons and optimizations
;;; and you want all memoization turned off. The list of names can be saved
;;; for later re-memoization.

(defun Unmemoize-All-Functions ()
  "Unmemoizes all currently-memoized functions"
  (let ((Previously-Memoized-Functions *Memoized-Function-Names*))
    (Unmemoize-Functions *Memoized-Function-Names*)
    Previously-Memoized-Functions))

;;;===========================================================================
;;; Takes a function that had previously been memoized, but is now unmemoized.
;;; Memoizes it again, keeping the original hash table instead of creating a
;;; new one. Note that there is no :Hash-Table-Test or :Key options as in
;;; Memoize, since they only make sense if a new hash table were being created.

(defun Rememoize (Function-Name)
  (Memoize Function-Name :Hash-Table-Source :Old-Function))

;;;===========================================================================
;;; Takes a list of names instead of just one.

(defun Rememoize-Functions (Function-Name-List)
  (mapc #'Rememoize Function-Name-List))

;;;===========================================================================


;;;===========================================================================
;;; SECTION 3 - Internal Routines
;;; -----------------------------
;;;===========================================================================

;;;===========================================================================
;;; This function is not normally called directly but is accessed via "Memoize".
;;;
;;; Memo: takes a function object as an argument, and returns a new function
;;; object which can "remember" previously calculated values. Basically, if
;;; the arguments have been seen before, they are used as key into the hash
;;; table, and that stored value is returned. If not, the original function is
;;; called, the results are stored in the hash table with the argument list as
;;; the key, and then those results are returned. This yields function
;;; behavior that, in many cases, is virtually transparent to the user, except
;;; for the potentially large performance gains. 
;;;
;;; In addition, it takes four keywords args: Function-Name, Test, Key, and
;;; Hash-Table-Source The function name is needed so that its property list
;;; can be used to record the associated hash table (to clear or save if
;;; requested) and the original function definition (to allow reversing the
;;; memoization later). Specifying a Test of "eql" and a Key of "first" is
;;; useful if the function takes only a single argument which is amenable to
;;; eql testing, or if you want to only treat parts of the argument list as
;;; significant with respect to determining which values to return. The
;;; Restore-Old-Table? flag, if non-NIL, starts the memo-table "primed" with
;;; values calculated in a previous run and saved via "Save-Memo-Table". 
;;;
;;; The copy-list business is needed on LISP machines because cdr-coding
;;; prevents direct comparisons of &rest arguments. In my experience,
;;; hashing on Symbolics is enough faster that the whole process about the
;;; same relative speed vs Unix Lisps.
;;;
;;; Hash-Table-Source is one of 
;;;   :Create       -- make a new hash table
;;;   :Disk         -- load a table from disk that was previously saved via
;;;                    Save-Memo-Table.
;;;   :Old-Function -- Use previous one, which assumes this function used to be
;;;                    memoized, then unmemoized, + now is being memoized again
;;;

(defun Memo (Function &key Function-Name
	                   (Test #'equal)
			   (Key #'identity)
			   (Hash-Table-Source :Create))
  "Takes a normal function object and returns an `equivalent' memoized one"
  (let ((Hash-Table (ecase Hash-Table-Source
		      (:Create       (make-hash-table :test Test))
		      (:Disk         (Load-Saved-Memo-Table Function-Name))
		      (:Old-Function (get Function-Name :Memo-Table)))))
    (setf (get Function-Name :Memo-Table) Hash-Table)
    (setf (get Function-Name :Unmemoized-Function) Function)
    (setf (get Function-Name :Memo-Table-Test) Test)
    #'(lambda (&rest Args)
	(declare (optimize (speed 3) (safety 1)))
	(let ((Hash-Key (funcall Key #+:LispM(copy-list Args)
				     #-:LispM Args )))
	  (multiple-value-bind (Value Found?)
	      (gethash Hash-Key Hash-Table)
	    (cond
	      (Found?
	       (incf (the fixnum (get Function-Name :Hash-Table-Lookups)))
	       Value)
	      (t
	       (incf (the fixnum (get Function-Name :Original-Function-Calls)))
	       (setf (gethash Hash-Key Hash-Table)
		     (apply Function Args))))))) ))

;;;===========================================================================
;;; Similar to the above, but has only a single argument testable by eq, and 
;;; does not keep track of function call counts. For a function of one
;;; eq-testable-argument, this is about 2.5 times faster than the more general
;;; Memo, even if :test #'eq and :key #'first are specified. On most machines 
;;; this difference is down in the thousandths of a second range, but is still
;;; useful in the cases of low-overhead functions that are called many times.

(defun One-Arg-Memo (Function &key Function-Name)
  "Takes a normal function object and returns an `equivalent' memoized one"
  (let ((Hash-Table (make-hash-table)))
    (setf (get Function-Name :Memo-Table) Hash-Table)
    (setf (get Function-Name :Unmemoized-Function) Function)
    #'(lambda (Arg)
	(multiple-value-bind (Value Found?)
	    (gethash Arg Hash-Table)
	  (if
	    Found?
	    Value
	    (setf (gethash Arg Hash-Table) (funcall Function Arg))))) ))

;;;===========================================================================
;;; Tests if the symbol refers to a function that is memoized. This property 
;;; is removed when the function is unmemoized.

(defun Memoized-Function-p (Function-Name)
  "Predicate to test if function is memoized"
  (get Function-Name :Unmemoized-Function) )

;;;===========================================================================
;;; Tests if the symbol refers to a function that was memoized with the
;;; :one-arg-p flag set, which results in restricted memoization for arguments
;;; of one parameter that are testable by eq.

(defun One-Arg-Memoized-Function-p (Function-Name)
  "Predicate to test if function is memoized by restricted 1-argument version"
  (and (Memoized-Function-p Function-Name)
       (not (get Function-Name :Memo-Table-Test))) )

;;;===========================================================================
;;; Converts the original, unmemoized doc string into a new one appropriate 
;;; for the memoized function.

(defun Memoized-Doc-String (Function-Name Original-Doc-String)
  "Returns a new doc string to be used with the memoized function"
  (format nil
	  "~S is a memoized function, returning previously calculated~%~
           values from a hash table instead of recalculating them.~%~A"
		  Function-Name
		  (Original-Doc-String-Description Original-Doc-String))
)

;;;===========================================================================
;;; Returns a short string describing previous documentation string.

(defun Original-Doc-String-Description (String-or-NIL)
  "Returns a string based on a function's original documentation"
  (if
    String-or-NIL
    (format nil "The original documentation was~%`~A'." String-or-NIL)
    (format nil "Original function had no documentation.")))

;;;===========================================================================
;;; Returns a new definition of "arglist". The "arglist" function is not part
;;; of Common LISP, but is contained in many of the implementations, including
;;; at least Symbolics, Lucid, Franz (Allegro), and Apple (MCL). This function
;;; returns a list giving the argument list to the function, as written by the
;;; user. Since memoization causes a function's arglist to become "&rest
;;; Args", this simply gives a more useful description, mostly by showing what
;;; the arglist of the original (i.e. unmemoized) function was. Change the
;;; definition of Arglist-Function-Name if it is different in your
;;; implementation. 

(defun Redefine-Arglist ()
  "Alters `arglist' to show original arguments for memoized functions"
  (let ((Arglist-Function-Name 'common-lisp-user::arglist))
    (when
      (and (fboundp Arglist-Function-Name)
	   (not (get Arglist-Function-Name :Original-Definition)))
      (setf (symbol-function Arglist-Function-Name)
	    (let ((Original-Definition
		    (symbol-function Arglist-Function-Name)))
	      (setf (get Arglist-Function-Name :Original-Definition)
		    Original-Definition)
	      #'(lambda (Function-Name &rest Unused-Args)
		  (declare (ignore Unused-Args))
		  (let ((Unmemoized-Function
			  (if (symbolp Function-Name)
			      (get Function-Name :Unmemoized-Function))))
		    (if
		      Unmemoized-Function
		      `(&rest Memoized-Args
			      (originally ,(funcall Original-Definition
						    Unmemoized-Function)))
		      (funcall Original-Definition Function-Name)))) )))))

(eval-when (eval load) (Redefine-Arglist))

;;;===========================================================================
