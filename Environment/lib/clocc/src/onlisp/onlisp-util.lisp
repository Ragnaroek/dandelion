;;; This code is copyright 1993 by Paul Graham.
;;; Distribution under clocc.sourceforge.net
;;; Liam M. Healy (LMH), liam@users.sourceforge.net
;;; By permission of Paul Graham, this is distributed under the
;;; GNU General Public License (GPL), version 2.
;;; The license accompanies this file under the name "COPYING", or
;;; see http://www.gnu.org/copyleft/gpl.html for a copy.

;;; LMH changes are:
;;; Added
;;;  - in-package and export defintions from this package
;;;  - form seperators (by book section)
;;;  - comments
;;;  - some type declarations (originally to help CMUCL.
;;;  - macro #'defmemoize.
;;;  - (declare (ignorable it)) for the anaphoric things.
;;;  - added eval-when to ddfn to supress compile-warning
;;;  - CLtL1 conditionalization for alrec/atrec.
;;; Removed
;;;  - multiple definitions (see below)
;;;  - #\{ delimiter - messes up emacs
;;; Changed
;;;  - get-setf-method to ANSI's get-setf-expansion (conditionally)

;;; Book Info:
;;; Paul Graham, On Lisp, Prentice-Hall 1994
;;; ISBN:  0-13-030552-9
;;; LC: QA 76.73.C28G73 1994

;;; This file is utilities (Chapters 1-18).  An effort has been made to insure
;;; only the best definition (if multiple definintions were given) actually
;;; will be read.

(in-package :pg)


;;;; ********************************************************************************
;;;; Section 4.3, Utility Functions: Operations on Lists
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(last1 single append1 conc1 mklist longer filter group prune)))   ; LMH

(declaim (inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  "The last item in a list."  ; LMH
  (declare (list lst))
  (car (last lst)))

(defun single (lst)
  "Test list for one element."   ; LMH
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "Add the obj onto the end of the lst."   ; LMH
  (append lst (list obj)))

(defun conc1 (lst obj)
  "Destructively add the obj onto the end of the lst."   ; LMH
  (nconc lst (list obj)))

(defun mklist (obj)
  "Make a list out of the object if it isn't already."  ; LMH
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  "Test if list x is longer than list y.  More efficient
   than calling length twice."   ; LMH
  (declare (sequence x y))   ; LMH
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "Calls fn on each element of the lst, returning a
   list of non-NIL results."   ; LMH
  (declare (function fn))   ; LMH
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  "Group source list into sublists of length n."   ; LMH
  (declare (fixnum n))   ; LMH
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

#|
(defun flatten (x)
  "Flatten the list."    ; LMH
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
|#

(defun prune (test tree)
  "Descend tree, removing items that return nil from test."	; LMH
  (declare (function test))   ; LMH
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

;;;; ********************************************************************************
;;;;  Section 4.4, Utility Functions: Search
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(find2 before after duplicate split-if most best mostn)))   ; LMH

(defun find2 (fn lst)
  "Find the first element of lst that satisfies fn, returning
   both the element and result of fn.  Like find, but also returns
   the result of calling fn."   ; LMH
  (declare (function fn))   ; LMH
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Test if x occurs before y in lst.
   Returns true if second element doesn't occur at all."   ; LMH
  (declare (function test))   ; LMH
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Test if x occurs after y in lst."   ; LMH
  (declare (function test))   ; LMH
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "Test if obj is duplicated in lst."   ; LMH
  (declare (function test))   ; LMH
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  "Split the list into two at the first element that satisfies fn."   ; LMH
  (declare (function fn))   ; LMH
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (scoring-fn lst)
  "Return the element and the score that returns the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))			; LMH
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall scoring-fn wins)))
	(declare (fixnum max))			; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
	    (declare (fixnum score))		; LMH scoring-fn must return fixnum
            (when (> score max)
              (setq wins obj
                    max  score))))
        (values wins max))))

(defun best (predicate lst)
  "The element of lst for which the predicate always returns t when called
   with other elements in lst, like (car (sort lst predicate)) but
   potentially more efficient."   ; LMH
  (declare (function predicate))   ; LMH
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall predicate obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (scoring-fn lst)
  "Return a list of all elements and the score that return the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))			; LMH
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall scoring-fn (car lst))))
	(declare (fixnum max))			; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
	    (declare (fixnum score))		; LMH scoring-fn must return fixnum
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;;; ********************************************************************************
;;;; Section 4.5, Utility Functions: Mapping
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(map0-n map1-n mapa-b map-> mappend mapcars rmapcar)))   ; LMH

(defun map0-n (fn n)
  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 0...n."  ; LMH
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 1...n."  ; LMH
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (declare (function fn) (fixnum a b step))   ; LMH
  "Apply the fn to the list of numbers a...b, stepping with step."  ; LMH
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (declare (fixnum i))   ; LMH
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (declare (function fn test-fn succ-fn))	; LMH
  "Apply fn to each member of the generated sequence whose
  first element is start, succesive elements are generated
  with succ-fn, and whose last element is the last element
  that does not satisify test-fn."   ; LMH
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (declare (fixnum i))			; LMH
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (declare (function fn))   ; LMH
  "Nondestructive form of mapcan."   ; LMH
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (declare (function fn))			; LMH
  "Mapcar a function over several lists, like (apply #'mapcar fn lsts) but without
   unnecessary consing."   ; LMH
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (declare (function fn))			; LMH
  "Mapcar for trees."				; LMH
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

;;;; ********************************************************************************
;;;; Section 4.6, Utility Functions: I/O
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(readlist prompt break-loop)))   ; LMH

(defun readlist (&rest args)
  "Read separate items and make into a list."   ; LMH
  (values (read-from-string
            (concatenate 'string "("
                                 (the simple-base-string (apply #'read-line args))   ; LMH
                                 ")"))))

(declaim (stream *query-io*))  ; LMH

(defun prompt (&rest args)
  "Prompt by outputting text and reading something typed."   ; LMH
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (declare (function fn quit))			; LMH
  "Call the fn repeatedly on everything typed to the prompt in args,
   until quit function is true.  Usefule for imitating the LISP
   top level loop."			; LMH
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

;;;; ********************************************************************************
;;;; Section 4.7, Utility Functions: Symbols and Strings
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mkstr symb reread explode)))   ; LMH

(defun mkstr (&rest args)
  "Make a string out of the printed representations of the arguments."   ; LMH
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Make a symbol out of the printed representations of the arguments."   ; LMH
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  "Interpret as LISP the printed represenatations of the arguments."   ; LMH
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  "Take apart symbol, returning a list of symbols whose print form
   is each character."   ; LMH
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
             (symbol-name sym)))

;;;; ********************************************************************************
;;;; Section 5.2, Returning Functions: Orthogonality
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(! def!)))   ; LMH

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  "The destructive equivalent of a function."   ; LMH
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  "Define the destructive equivalent of a function."   ; LMH
  (setf (gethash fn *!equivs*) fn!))

(defsetf ! (fn) (fn!) `(def! ,fn ,fn!))   ; LMH -- for convenience

;;;; ********************************************************************************
;;;; Section 5.3, Returning Functions: Memoizing
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(memoize defmemoize)))   ; LMH

(defun memoize (fn)
  (declare (function fn))   ; LMH
  "Create and return a memoized version of the function fn."   ; LMH
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

;;; LMH make into a macro
(defmacro defmemoize (name args &body body)
  "Create a function which memoizes its arguments."
  (let ((nm (gensym)))
   `(flet ((,nm ,args ,@body))
      (setf (symbol-function ',name) (memoize (function ,nm))))))

;;;; ********************************************************************************
;;;; Section 5.4, Returning Functions: Composing Functions
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(compose fif fint fun)))   ; LMH

(defun compose (&rest fns)
  "Compose the functions."   ; LMH
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
	(declare (function fn1))			; LMH
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else)
  (declare (function if then) (type (or function null) else))   ; LMH
  "Function-if: create a function that, if if is true on its
   argument, then return then called on the argument, otherwise
   else called on the argument (or nil)."   ; LMH
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (declare (function fn))			; LMH
  "Function intersection: a function that is the
   AND of each function called on the argument."   ; LMH
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	(declare (function chain))		; LMH correct?
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (declare (function fn))			; LMH
  "Function union: a function that is the
   OR of each function called on the argument."   ; LMH
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	(declare (function chain))		; LMH correct?
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

;;;; ********************************************************************************
;;;; Section 5.5, Returning Functions: Recursion on Cdrs
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(lrec)))   ; LMH

(defun lrec (rec &optional base)
  (declare (function rec))			; LMH
  "Function to define flat list recurser.
   Rec is a function that takes two arguments, the first
   will be the car of the list, the second is a function
   that will produce the cdr.  Base is a function to be called
   or value when the list is empty.
   For example, a length function could be defined as
   (lrec #'(lambda (x f) (1+ (funcall f))) 0)."   ; LMH
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                              #'(lambda ()
                                  (self (cdr lst)))))))
    #'self))

;;; Some examples of lrec:
;;; copy-list: (lrec #'(lambda (x f) (cons x (funcall f))))
;;; remove-duplicates: (lrec #'(lambda (x f) (adjoin x (funcall f))))
;;; find-if, for some function fn: (lrec #'(lambda (x f) (if (fn x) x (funcall f))))
;;; some, for some function fn: (lrec #'(lambda (x f) (or (fn x) (funcall f))))

;;;; ********************************************************************************
;;;; Section 5.6, Returning Functions: Recursion on Subtrees
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(ttrav trec)))   ; LMH

#|
(defun find-if (fn tree)
  (declare (function fn))   ; LMH
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))
|#

;;; ttrav can be used to define rfind-if, copy-tree, count-leaves, and flatten

(defun ttrav (rec &optional (base #'identity))
  (declare (function rec))   ; LMH
  "Tree traverser: Recurse down a tree.  Rec is a function
   that takes two arguments, the first being the result of the left branch,
   the second the result of the right branch.  Base is a function called
   or value returned if at a leaf."   ; LMH
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                              (if (cdr tree)
                                  (self (cdr tree)))))))
    #'self))

;;; example count-leaves
;;;      (ttrav #'(lambda (l r) (+ l (or r 1))) 1))

;;; example flatten
;;;      (ttrav #'nconc #'mklist))

(defun trec (rec &optional (base #'identity))
  (declare (function rec))   ; LMH
  "Tree traverser: Recurse down a tree.  Rec is a function
   that takes three arguments, the first is the tree,
   the second is the result of the left branch,
   the third is the result of the right branch.  Base is a function called
   or value returned if at a leaf.  Differs from ttrav in that
   it need not traverse the whole tree. "   ; LMH
  (labels
    ((self (tree)
       (if (atom tree)
           (if (functionp base)
               (funcall base tree)
               base)
           (funcall rec tree
                        #'(lambda ()
                            (self (car tree)))
                        #'(lambda ()
                            (if (cdr tree)
                                (self (cdr tree))))))))
    #'self))

;;; Example for rfind-if:
;;;   (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
;;;	    #'(lambda (tree) (and (oddp tree) tree))))

;;;; ********************************************************************************
;;;; Section 7.4, Macros: Testing Macroexpansion
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mac)))   ; LMH

(defmacro mac (expr)
  "Print macroexpansion."   ; LMH
  `(pprint (macroexpand-1 ',expr)))

;;;; ********************************************************************************
;;;; Section 11.1, Classic Macros: Creating Context
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(when-bind when-bind* with-gensyms condlet)))   ; LMH

(defmacro when-bind ((var expr) &body body)
  "Like a single let binding, but body is not executed if var is bound to nil"   ; LMH
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  "Like let*, but body is not executed if var is bound to nil"   ; LMH
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  "Create gensyms, useful for creating macros."   ; LMH
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro condlet (clauses &body body)
  "Conditional let.  Example:
   (condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
             ((= 1 1) (x (princ 'c)) (y (princ 'd)))
             (t       (x (princ 'e)) (y (princ 'f))))
     (list x y z))
  CD
  (D C NIL)."   ; LMH
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                        (mapcar #'car
                                (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                 ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))


(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (cons (cdr (assoc (the symbol (car bindform)) vars))   ; LMH the symbol
                        (cdr bindform))))
          (cdr cl)))

;;;; ********************************************************************************
;;;; Section 11.3, Classic Macros: Conditional Evaluation
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(if3 nif in inq in-if >case >casex)))   ; LMH

(defmacro if3 (test t-case nil-case ?-case)
  "Three valued if."   ; LMH
  `(case ,test
     ((nil) ,nil-case)
     (?     ,?-case)
     (t     ,t-case)))

(defmacro nif (expr pos zero neg)
  "Numeric if."   ; LMH
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(defmacro in (obj &rest choices)
  "Test efficiently for set membership.  More efficient than member."   ; LMH
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  "Quoted version of in -- all arguments are implicitly quoted."   ; LMH
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  "More efficient version of some."   ; LMH
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defmacro >case (expr &rest clauses)
  "Case for which keys are evaluated.  Keys must always be given as
   a list to avoid ambiguity."   ; LMH
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

;;;; ********************************************************************************
;;;; Section 11.4, Classic Macros: Iteration
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(while till for do-tuples/o do-tuples/c)))   ; LMH

(defmacro while (test &body body)
  "Repeatedly do body as long as test is true."   ; LMH
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  "Repeatedly do body until test is true."   ; LMH
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  "Iterate, stepping var from start to stop."   ; LMH
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro do-tuples/o (parms source &body body)
  (declare (list parms body))   ; LMH correct?
  "Iterate, destructuring bind of parms to tuples in source.
   Example:
   (do-tuples/o (x y) '(a b c d) (princ (list x y)))
   (A B)(B C)(C D)
   NIL."   ; LMH
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
           (mapc #'(lambda ,parms ,@body)
                 ,@(map0-n #'(lambda (n)
                               `(nthcdr ,n ,src))
                           (1- (length parms))))))))

(defmacro do-tuples/c (parms source &body body)
  (declare (list parms body))			; LMH correct?
  "Iterate, destructuring bind of parms to tuples in source considered to be
  a circlular list.
   Example:
   (do-tuples/c (x y) '(a b c d) (princ (list x y)))
   (A B)(B C)(C D)(D A)
   NIL."   ; LMH
  (if parms
      (with-gensyms (src rest bodfn)
		    (let ((len (length parms)))
		      (declare (fixnum len))	; LMH
		      `(let ((,src ,source))
			 (when (nthcdr ,(1- len) ,src)
			   (labels ((,bodfn ,parms ,@body))
			     (do ((,rest ,src (cdr ,rest)))
				 ((not (nthcdr ,(1- len) ,rest))
				  ,@(mapcar #'(lambda (args)
						`(,bodfn ,@args))
					    (dt-args len rest src))
				  nil)
			       (,bodfn ,@(map1-n #'(lambda (n)
						     (declare (fixnum n))	; LMH
						     `(nth ,(1- n)
							   ,rest))
						 len))))))))))

(defun dt-args (len rest src)
  (declare (fixnum len))			; LMH
  (map0-n #'(lambda (m)
	      (declare (fixnum m))		; LMH
              (map1-n #'(lambda (n)
			  (declare (fixnum n))	; LMH
                          (let ((x (+ m n)))
			    (declare (fixnum x))	; LMH
                            (if (>= x len)
                                `(nth ,(- x len) ,src)
                                `(nth ,(1- x) ,rest))))
                      len))
          (- len 2)))

;;;; ********************************************************************************
;;;; Section 11.5, Classic Macros: Iteration with Multiple Values
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mvdo* mvpsetq shuffle mvdo)))   ; LMH

(defmacro mvdo* (parm-cl test-cl &body body)
  "Like do, but each clause can bind multiple values."   ; LMH
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
           ,label
           (if ,(car test)
               (return (progn ,@(cdr test))))
           ,@body
           ,@(mvdo-rebind-gen rebinds)
           (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (declare (list rebinds))   ; LMH correct?
  (cond ((null rebinds) nil)
        ((< (length (the list (car rebinds))) 3)   ; LMH correct?
         (mvdo-rebind-gen (cdr rebinds)))
        (t
         (cons (list (if (atom (caar rebinds))
                         'setq
                         'multiple-value-setq)
                     (caar rebinds)
                     (third (car rebinds)))
               (mvdo-rebind-gen (cdr rebinds))))))

(defmacro mvpsetq (&rest args)
  "Multiple value version of psetq."		; LMH
  (let* ((pairs (group args 2))
         (syms  (mapcar #'(lambda (p)
                            (mapcar #'(lambda (x)
					(declare (ignore x))	; LMH
					(gensym))
                                    (mklist (car p))))
                        pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq
		      ,@(mapcan #'(lambda (p s)
				    (shuffle (mklist (car p))
					     s))
				pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss)
				,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  "Interleave two lists."   ; LMH
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

(defmacro mvdo (binds (test &rest result) &body body)
  "Like do, but each clause can bind multiple values, with
   values bound in parallel."   ; LMH
  (let ((label (gensym))
        (temps (mapcar #'(lambda (b)
                           (if (listp (car b))
                               (mapcar #'(lambda (x)
					   (declare (ignore x))	; LMH
                                           (gensym))
                                       (car b))
                               (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
                              (list var (cadr b)))
                          binds
                          temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
		      (mappend #'mklist (mapcar #'car binds))
		      (mappend #'mklist temps))
	     ,label
	     (if ,test
		 (return (progn ,@result)))
	     ,@body
	     (mvpsetq ,@(mapcan #'(lambda (b)
				    (if (third b)
					(list (car b)
					      (third b))))
				binds))
	     (go ,label)))))

;;;; ********************************************************************************
;;;; Section 12.3, Generalized Variables: New Utilities
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(allf nilf tf toggle toggle2 concf conc1f concnew)))   ; LMH

(defmacro allf (val &rest args)
  "Set a number of generalized variables to the same value."   ; LMH
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))

(defmacro nilf (&rest args)
  "Set all args to nil."			; LMH
  `(allf nil ,@args))

(defmacro tf (&rest args)
  "Set all args to t."			; LMH
  `(allf t ,@args))

(defmacro toggle (&rest args)
  "Give all args the opposite truth value."   ; LMH
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
               args)))

(define-modify-macro toggle2 () not)

(define-modify-macro concf (obj) nconc
   "Destructively append, insuring the first place becomes
   the appended object.")   ; LMH

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj)))
  "Destructively add one element to the end of the list.")   ; LMH

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj))))
  "Destructively add a new element to the end of the list.")   ; LMH

;;;; ********************************************************************************
;;;; Section 12.4, Generalized Variables: More Complex Utilities
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(_f pull pull-if popn sortf)))   ; LMH

(defmacro _f (op place &rest args)
  "Setf to a new value based on the old by applying op;
   e.g., incf could be considered _f with op = +."   ; LMH
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)	; LMH ANSI CL changed name
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

;;; To memoize foo,
;;; (_f memoize (symbol-function 'foo))

(defmacro pull (obj place &rest args)
  "Pull specified obj out of place; complement to pushnew."   ; LMH
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)	; LMH ANSI CL changed name
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args)
  "Pull what passes test out of place."   ; LMH
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)	; LMH ANSI CL changed name
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro popn (n place)
  "Pop n elements off place."   ; LMH
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)	; LMH ANSI CL changed name
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
                ,set)))))

(defmacro sortf (op &rest places)
  "Sort places according to op."	; LMH
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list
			     (get-setf-expansion p))) ; LMH ANSI CL changed name
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
	     (mapcan #'(lambda (m)
			 (append (first m)
				 (third m)))
		     meths)
	     (mapcan #'(lambda (m)
			 (append (second m)
				 (list (fifth m))))
		     meths))
       ,@(mapcon #'(lambda (rest)
                     (mapcar
		      #'(lambda (arg)
			  `(unless (,op ,(car rest) ,arg)
			     (rotatef ,(car rest) ,arg)))
		      (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))

;;;; ********************************************************************************
;;;; Section 14.1, Anaphoric Macros: Anaphoric Variants
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(it aif awhen awhile aand acond alambda ablock
	       aif2 awhen2 acond2 read2 do-file)))   ; LMH

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."   ; LMH
  `(let ((it ,test-form))
     (declare (ignorable it))		; LMH
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "Anaphoric when: use `it' in body to
   refer to result of the test-form."   ; LMH
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  "Anaphoric while: use `it' in body to
   refer to result of the expr"		; LMH
  `(do ((it ,expr ,expr))
       ((not it))
     (declare (ignorable it))		; LMH
     ,@body))

(defmacro aand (&rest args)
  "Anaphoric and: use `it' to refer to result of
  previous form evaluation."   ; LMH
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  "Anaphoric cond: use `it' in consequent to refer to result of test."    ; LMH
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym))
		 (declare (ignorable it)) ; LMH
		 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  "Anaphoric lambda: use `self' in body to refer to this lambda."   ; LMH
  `(labels ((self ,parms ,@body))
     #'self))

#|
(defmacro ablock (tag &rest args)
  (declare (list args))				; LMH correct?
  "Anaphoric block: use `it' in to refer to previous expression."	; LMH
  `(block ,tag
     ,(funcall (alambda (args)
		(case (length args)	; LMH the list; better to do null, cdr tests?
		  (0 nil)
		  (1 (car args))
		  (t `(let ((it ,(car args)))
			,(self (cdr args))))))
               args)))
|#

(defmacro ablock (tag &rest args)
  "Anaphoric block: use `it' in to refer to previous expression." ; LMH
  `(block ,tag
     ,(funcall (alambda (args)
			(cond
			 ((null args) nil)
			 ((single args) (car args))
			 (t `(let ((it ,(car args)))
			       (declare (ignorable it)) ; LMH
			       ,(self (cdr args))))))
	       args)))

;;; LMH the 2 versions are useful for e.g. gethash returning two values:

(defmacro aif2 (test &optional then else)
  "Anaphoric if for two returned values: use `it' in `then', `else' clauses
   to refer to result of the test.  Test can return two values,
   if is satisfied if either is true, but `it' refers to first." ; LMH
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win)
	 ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  "Anaphoric when for two returned values: use `it' in body to
   refer to result of the test.  Test can return two values,
   when is satisfied if either is true, but `it' refers to first."   ; LMH
  `(aif2 ,test
         (progn ,@body)))

(defmacro acond2 (&rest clauses)
  "Anaphoric cond for two returned values: use `it' in body to
   refer to result of the test.  Test can return two values,
   when is satisfied if either is true, but `it' refers to first."   ; LMH
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val))
		 (declare (ignorable it)); LMH
		 ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
  "Read, returning a second value showing something read,
   otherwise nil."   ; LMH
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body)
  "Iterate over file, reading forms."   ; LMH
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
         ,@body))))

;;;; ********************************************************************************
;;;; Section 15.1, Macros Returning Functions: Building Functions
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(fn rbuild)))   ; LMH

;;; fn subsumes fif, fint, fun

(defmacro fn (expr)
  "Make the function according to the expression,
   e.g., (fn (and ingegerp oddp)) makes the function
   #'(lambda (x) (and (integerp x) (oddp x)))."   ; LMH
 `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

;;;; ********************************************************************************
;;;; Section 15.1, Macros Returning Functions: Recursion on CDRS
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(alrec on-cdrs rec unions intersections differences maxmin)))   ; LMH

#+(or cltl2 symbolics)    ; LMH
(defmacro alrec (rec &optional base)
  "Anaphoric list recurser (lrec): use `it' to refer to the current
   car of the list, and `rec' to the function rec itself.
   every on #'oddp,
   (alrec (and (oddp it) rec) t) is the equivalent of
   (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)."   ; LMH
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (declare (ignorable it) (function ,gfn))   ; LMH
               (#+cltl2 symbol-macrolet #+symbolics clos:symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

#-(or cltl2 symbolics)    ; LMH   -- I don't think this works
(defmacro alrec (rec &optional base)
  "Anaphoric list recurser (lrec): use `it' to refer to the current
   car of the list, and `rec' to the function rec itself.
   every on #'oddp,
   (alrec (and (oddp it) rec) t) is the equivalent of
   (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)."   ; LMH
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (declare (ignorable it) (function ,gfn))   ; LMH
               (labels ((rec () (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  "Anaphoric list recursion, for defining named functions,
   e.g., (defun our-every (fn lst) (on-cdrs (and (funcall fn it) rec) t lst))."   ; LMH
  `(funcall (the function (alrec ,rec #'(lambda () ,base))) ,@lsts))   ; LMH the function

(defun unions (&rest sets)
  "The union of all the sets (like union, but takes an arbitrary number of arguments)."   ; LMH
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  "The intersection of all the sets (like intersection,
   but takes an arbitrary number of arguments)."   ; LMH
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs)
  "The set difference of all the sets (like set-difference,
   but takes an arbitrary number of arguments).  Handles multiple
   arguments the same way #'- does."   ; LMH
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  "Finds both the maximum and minimum in a single traversal of the list."   ; LMH
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
	       (declare (number mx mn))   ; LMH
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))

;;;; ********************************************************************************
;;;; Section 15.3, Macros Returning Functions: Recursion on Subtrees
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(atrec on-trees count-leaves flatten rfind-if)))   ; LMH

#+(or cltl2 symbolics)    ; LMH
(defmacro atrec (rec &optional (base 'it))
  "Anaphoric tree recursion: current tree is 'it, left subtree is 'left
   right subtree is 'right."   ; LMH
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
	       (declare (ignorable it) (function ,lfn ,rfn))   ; LMH
               (#+cltl2 symbol-macrolet #+symbolics clos:symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) (declare (ignorable it)) ,base)))); LMH

#-(or cltl2 symbolics)    ; LMH
(defmacro atrec (rec &optional (base 'it))
  "Anaphoric tree recursion: current tree is 'it, left subtree is 'left
   right subtree is 'right."   ; LMH
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
	       (declare (ignorable it) (function ,lfn ,rfn))   ; LMH
               (labels ((left () (funcall ,lfn))
                        (right () (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) (declare (ignorable it)) ,base))))

(defmacro on-trees (rec base &rest trees)
  "Anaphoric tree recursion, for defining named functions"   ; LMH
  `(funcall (atrec ,rec ,base) ,@trees))

(defun count-leaves (tree)
  (on-trees (+ (the fixnum left)   ; LMH add the fixnum
	       (or (the (or fixnum null) right) 1))   ; LMH add the fixnum
	    1 tree))

(defun flatten (tree)
  (on-trees (nconc left right) (mklist it) tree))

(defun rfind-if (fn tree)
  (declare (function fn))   ; LMH
  "Find-if in a tree."   ; LMH
  (on-trees (or left right)
	    (and (funcall fn it) it)
	    tree))

;;;; ********************************************************************************
;;;; Section 15.4, Macros Returning Functions: Lazy evaluation
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(force delay)))   ; LMH

(defconstant unforced (gensym))

(defstruct delay  forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             #'(lambda ()
                 (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (the function (delay-closure x)))   ; LMH the function
          (delay-forced x))
      x))

;;;; ********************************************************************************
;;;; Section 16.1, Macro-Defining Macros: Abbreviations
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(abbrev abbrevs)))   ; LMH

(defmacro abbrev (short long)
  "Define another name for the macro."   ; LMH
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  "Define other names for the macros."   ; LMH
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

;;;; ********************************************************************************
;;;; Section 16.2, Macro-Defining Macros: Properties
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(propmacro propmacros)))   ; LMH

(defmacro propmacro (propname)
  "Macro to define a property."   ; LMH
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  "Macro to define properties."   ; LMH
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
               props)))

;;;; ********************************************************************************
;;;; Section 16.3, Macro-Defining Macros: Anaphoric Macros
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defanaph)))   ; LMH

#|
(defmacro defanaph (name &optional calls)
  "Define an anaphoric macro; 'it refers to result of previous computation"   ; LMH
   (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))

(defun anaphex (args expr)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex (cdr args)
                     (append expr (list sym)))))
      expr))
|#

;;; examples
;;; (defanaph alist)
;;; (defanaph aif :rule :first)
;;; (defanaph asetf :rule :place)
;;; (defanaph a+)
;;; usage: (a+ menu-price (* it 0.05) (* it 3))
;;; menu-price + 5% tax + 3x tax for tip

(defmacro defanaph (name &optional &key calls (rule :all))
  "Define an anaphoric macro; 'it refers to result of previous computation.
   Rule is:
     :all -- 'it will always be bound to value of previous argument,
     :first -- 'it bound to first argument,
  or :place -- first argument treated as generalized variable, 'it bound to
               its initial value."   ; LMH
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all   `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defun anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
      call))

(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

;;;; ********************************************************************************
;;;; Section 17.3, Read Macros: Delimiters
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defdelim)))   ; LMH

(defmacro defdelim (left right parms &body body)
  "Define deliminters for dispatching read macros."   ; LMH
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute); LMH to supress compile warning
 (let ((rpar (get-macro-character #\) )))
 (defun ddfn (left right fn)
  (declare (function fn))		; LMH
  (set-macro-character right rpar)
  (set-dispatch-macro-character #\# left
      #'(lambda (stream char1 char2)
	  (declare (ignore char1 char2))    ; LMH
          (apply fn
                 (read-delimited-list right stream t)))))))

;;; This defines #[] to give a sequence, e.g. #[3 7 ] expands to (3 4 5 6 7)
;;; Note a space before ] is needed on Symbolics but not CMUCL;
;;; See CLtR p. 687.
(defdelim #\[ #\] (x y)
	  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

;;; This defines #{} as functional composition, e.g. (funcall #{list 1+} 7) gives (8).
;;; This is commented out because of subtle problems balancing parenthesis in emacs.
#|
(defdelim #\{ #\} (&rest args)
	  `(fn (compose ,@args)))
|#

;;;; ********************************************************************************
;;;; Section 18.2, Destructuring: Other Structures
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(dbind with-matrix with-array with-struct)))   ; LMH

(defmacro dbind (pat seq &body body)
  "Destructuring bind for any kind of sequence."   ; LMH
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(eval-when (compile load)    ; LMH destruc, dbind-ex needs to be loaded when match1 compiled

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (declare (function atom?) (fixnum n))   ; LMH
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
       (if rest
           `((,rest (subseq ,seq ,n)))
           (let ((p (car pat))
                 (rec (destruc (cdr pat) seq atom? (1+ n))))
             (if (funcall atom? p)
                 (cons `(,p (elt ,seq ,n))
                       rec)
                 (let ((var (gensym)))
                   (cons (cons `(,var (elt ,seq ,n))
                               (destruc p var atom?))
                         rec))))))))

(defun dbind-ex (binds body)
    (if (null binds)
	`(progn ,@body)
	`(let ,(mapcar #'(lambda (b)
			   (if (consp (car b))
			       (car b)
			       b))
		       binds)
	   ,(dbind-ex (mapcan #'(lambda (b)
				  (if (consp (car b))
				      (cdr b)))
			      binds)
		      body))))

)

(defmacro with-matrix (pats ar &body body)
  "Destructuring a two dimensional array."   ; LMH
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1) (col -1))        ; LMH add decl for col
	       (declare (fixnum row))		; LMH
               (mapcan
                 #'(lambda (pat)
                     (incf row)
                     (setq col -1)
                     (mapcar #'(lambda (p)
                                 `(,p (aref ,gar
                                            ,row
                                            ,(incf (the fixnum col)))))	; LMH
			     pat))
                 pats))
         ,@body))))

(defmacro with-array (pat ar &body body)
  "Destructuring on an arbitrary dimensional array."   ; LMH
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))

(defmacro with-struct ((name . fields) struct &body body)
  "Destructuring on a structure; name is the conc name of the structure."   ; LMH
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f) ,gs)))
                     fields)
         ,@body))))

;;;; ********************************************************************************
;;;; Section 18.3, Destructuring: Reference
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-places)))   ; LMH

(defmacro with-places (pat seq &body body)
  "Destructuring bind on generalized variables."   ; LMH
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
                                     (if (consp (car b))
                                         (car b)
                                         b))
                                 binds)
        ,(wplac-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                   (cdr b)))
                           binds)
                   body))))

;;;; ********************************************************************************
;;;; Section 18.4, Destructuring: Matching
;;;; ********************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(match)))

(declaim (ftype (function (t t &optional t) (values list symbol)) match))
(defun match (x y &optional binds)
  "Destructuring match on symbols that begin with ?,
   e.g. (match '(p a b c a) '(p ?x ?y c ?x))
   returns ((?Y . B) (?X . A)) and T."   ; LMH
  (acond2
;    ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
    ((or (eq x y) (eq x '_) (eq y '_)) (values binds t))   ; LMH change to eq
    ((binding x binds) (match it y binds))
    ((binding y binds) (match x it binds))
    ((varsym? x) (values (cons (cons x y) binds) t))
    ((varsym? y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (match (car x) (car y) binds))
     (match (cdr x) (cdr y) it))
    (t (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (declare (symbol x))   ; LMH
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

#|
;;; LMH these are the slow versions

(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))

(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
      (values ?x ?y)
      nil))
|#

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun vars-in (expr &optional (atom? #'atom))
  (declare (function atom?))   ; LMH
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defmacro if-match (pat seq then &optional else)
  "Destructuring match on symbols that begin with ?,
   like match but with pat known at compile time, more efficient."   ; LMH
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq)
                             (destruc pat gseq #'simple?))
                       then
                       `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
    (cond ((gensym? pat)
           `(let ((,pat ,expr))
              (if (and (typep ,pat 'sequence)
                       ,(length-test pat rest))
                  ,then
                  ,else)))
          ((eq pat '_) then)
          ((var? pat)
           (let ((ge (gensym)))
             `(let ((,ge ,expr))
                (if (or (gensym? ,pat) (equal ,pat ,ge))
                    (let ((,pat ,ge)) ,then)
                    ,else))))
          (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))


