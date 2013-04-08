;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Memoization; Base: 10 -*-

(in-package "MEMOIZATION")

;;;===========================================================================
;;; (C) 1992 Marty Hall. Permission is granted for any use or modification
;;      of this code provided this notice is retained. Version of 8/93.
;;;===========================================================================

;;;===========================================================================
;;; This file is one of four files that define the Memoization facility:
;;;    - Load-Memoization: Defines Memoization package and loads other 3 files
;;;    - Memoization: Defines core memoization routines.
;;;    - Save-Memo-Table: Defines routines to save and load hash tables
;;;                       associated with a memoized function.
;;;    - Memoization-Examples [THIS FILE]: Simplistic version of memoization to
;;;                                        illustrate the principle, and 2
;;;                                        example functions to which 
;;;                                        memoization can be applied.
;;; This file contains the following:
;;;    - Basic-Memo: A simplistic version of "Memo" (which appears in the file
;;;      ==========  Memoization.lisp). It takes a function object and returns
;;;                  an "equivalent" one that remembers previous args. It is
;;;                  restricted to functions that take exactly one parameter,
;;;                  where that parameter is testable by "eq".
;;;    - Basic-Memoize: A simplistic version of "Memoize" (also appears in the 
;;;      =============  file Memoization.lisp). Takes a function NAME as
;;;                     argument, looks up the associated function object, 
;;;                     makes a new one via Basic-Memo, and then associates 
;;;                     that new one with the the function name it was given.
;;;    - Fib: A deliberately inefficient recursive implementation of a routine
;;;      ===  to calculate the Nth fibonacci number. Although it is easy to
;;;           fix the inefficiency without resorting to memoization, memoizing
;;;           it is illustrative of how memoization performs.
;;;    - Divided-Difference: Slightly more complex example in the vein of Fib
;;;      ==================
;;;
;;; Marty Hall
;;; The Johns Hopkins University Applied Physics Lab
;;; Room 7-38
;;; Johns Hopkins Rd.
;;; Laurel MD 20723
;;; hall@aplcenmp.apl.jhu.edu
;;; (410) 792-6000 x3440
;;;
;;;===========================================================================


;;;===========================================================================
;;; Simplest version to give the idea. Takes a function object as an argument
;;; and returns another one that does lookup from a hash table. The idea is
;;; that this new function, if called with an argument it has seen before,
;;; simply returns the corresponding value in the hash table. For a new
;;; argument, this new function will call the original function, stick the
;;; resultant value in the hash table, then return that value. For this simple
;;; case, function can only have a single argument that can be compared via
;;; eql. Normally used via "Basic-Memoize", but could be invoked
;;; directly by  (setq Bar (Basic-Memo #'Foo)), then (funcall Bar <Arg>).
;;;
;;; Derived from code in Peter Norvig's _Paradigms of AI Programming: Case
;;; Studies in Common LISP_, Morgan Kaufmann, 1992.

(defun Basic-Memo (Function)
  "Takes a normal function object and returns an `equivalent' memoized one"
  (let ((Hash-Table (make-hash-table)))
    #'(lambda (Arg)
	(multiple-value-bind (Value Foundp)
	    (gethash Arg Hash-Table)
	  (if
	    Foundp
	    Value
	    (setf (gethash Arg Hash-Table) (funcall Function Arg))))) ))

;;;===========================================================================
;;; Changes the symbol function, so that you can memoize recursive functions,
;;; and so that you can avoid changing any of the other code that already
;;; calls the existing function.
;;;
;;; Derived from code in Peter Norvig's _Paradigms of AI Programming: Case
;;; Studies in Common LISP_, Morgan Kaufmann, 1992.

(defun Basic-Memoize (Function-Name)
  "Memoize function associated with Function-Name. Simplified version"
  (setf (symbol-function Function-Name)
	(Basic-Memo (symbol-function Function-Name))))

;;;===========================================================================
;;; As described in section 2 of the main memoization file, there are four
;;; basic applications of memoization. The first, which is illustrated below,
;;; is when a single routine calls some subroutine (or itself recursively)
;;; more than is needed, resulting in extra calculations. By memoizing, these
;;; results are returned immediately for subsequent calls, with the effect of
;;; dynamic programming. In fact, this first case can be thought of as a tool
;;; for automatic dynamic programming, but without the need to build the
;;; subpieces in the correct order. This can frequently reduce the time of
;;; exponential algorithms to polynomial or even linear time.
;;;===========================================================================
 
;;;===========================================================================
;;; Simple example for testing. A silly recursive algorithm for finding the
;;; Nth Fibonacci number, assuming the 0th is 0, the 1st is 1, and all the
;;; rest are the sum of the previous two. Doing (Memoize 'Fib) or changing the
;;; "defun" to "Define-Memo-Function" results in a reduction from exponential
;;; complexity (Golden-Ratio to the N) to linear on the first run, plus it
;;; gives constant time results on anything previously calculated. Of course,
;;; this is a deliberately poor way to write a recursive definition, but
;;; illustrates the point well (try timing it both ways for moderate sized
;;; N's). Besides, even the obvious iterative or tail-recursive algorithms get
;;; no better than linear complexity. Since this function only takes a single
;;; argument that is testable by eq, (Basic-Memoize 'Fib) is sufficient. Try
;;; timing a case before and after this.
;;;
;;; On most machines, the unmemoized version should start slowing down around
;;; N = 28 or so.
;;;
;;; WARNING! On Lucid, if you compile with speed 3, you get "optimization of
;;; tail calls", which is actually removal of all directly recursive calls.
;;; This can be changed back by either a SPEED optimization of 2, or an
;;; explicity setting of (common-lisp-user::compiler-options :tail-merge NIL). Giving a
;;; NOTINLINE proclamation is not sufficient. This same problem may occur on
;;; other systems as well, so be on the lookout.

(defun Fib (N)
  "Returns Nth Fibonacci number, where 0th is 0, 1st is 1,
   and the rest are the sum of the previous two."
  (if
    (<= N 1)
    N
    (+ (Fib (- N 1)) (Fib (- N 2)))))

;;;===========================================================================
;;; An example for testing. This computes the divided differences used in
;;; determining coefficients when interpolating polynomials. Algorithm is
;;; taken from Cheney and Kincaid, _Numerical Mathematics and Computing_,
;;; Brooks/Cole, 1980. The point is that the algorithm, as written, performs
;;; wasted calculations, resulting in O(2^N) complexity. The memoized version
;;; has O(N^2) complexity in the first invocation, since it calculates only
;;; once for each of the subsequences of Points, and there are 1+2+3+...+N =
;;; N(N+1)/2 subsequences. Later invocations with the same parameters have
;;; effectively constant-time results. Both this and the following example are
;;; deliberately simple ones to illustrate the idea, and in neither case is it
;;; very difficult to discover a better order to get the same performance as
;;; the first invocation of the memoized version. But the transparent syntax
;;; and lack of additional debugging required often makes memoization an
;;; attractive alternative. Also, in many cases the alternative algorithm is
;;; less obvious. For instance, memoizing the simple, obvious algorithms for
;;; the matrix chain multiplication problem or the 0/1 knapsack problem give
;;; results with the same efficiency as the best dynamic programming
;;; algorithms. Similarly, Peter Norvig showed that memoizing a simple
;;; recursive descent parser can yield the same performance on context free
;;; grammars as Chart Parsing or Earley's Algorithm.
;;;
;;; Unlike Fib, this problem requires the real memoization routine, not just
;;; Basic-Memoize. Using this as an example for memoization was suggested by
;;; John Aspinall of Symbolics.
;;;
;;; As a test case, try (Divided-Difference #'Test-Function (First-N <X>))
;;; On most machines, the unmemoized version should start slowing down around
;;; <X> = 14 or so.

(defun Divided-Difference (Function Points)
  "Determines kth coefficient, where `Points' contains k entries"
  (if
    (null (rest Points))
    (apply Function Points)
    (/ (- (Divided-Difference Function (rest Points))
	  (Divided-Difference Function (butlast Points)))
       (- (first (last Points)) (first Points)))) )

;;;-----------------------------------------------------------
;;; Relatively arbitrary test function.

(defun Test-Function (N)
  (* N (cos N)))

;;;-----------------------------------------------------------
;;; This is just (loop for I from 1 to N collecting I), but CLtL/1
;;; compatibility was desired.
 
(defun First-N (N)
  (let ((List '()))
    (dotimes (I N (reverse List))
      (push (1+ I) List))) )

;;;===========================================================================
