;-*-syntax:COMMON-LISP-*-

(in-package "USER")

;This is the January 13, 1992 version of tests of Richard C. Waters'
;example showing the use of his XP pretty printer.

#|----------------------------------------------------------------------------|
 | Copyright 1992 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

;The first few pages are a copy of Richard Waters RT testing program
;see R.C. Waters, "Supporting the Regression Testing of Lisp Programs",
;       ACM Lisp Pointers, 4(2):47--53, June 1991.

(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")

(defstruct (entry (:conc-name nil)
		  (:type list))
  pend name form)

(defmacro vals (entry) `(cdddr ,entry))
(defmacro defn (entry) `(cdr ,entry))

(defun pending-tests ()
  (do ((l (cdr *entries*) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (pend (car l))
      (push (name (car l)) r))))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  nil)

(defun rem-test (&optional (name *test*))
  (do ((l *entries* (cdr l)))
      ((null (cdr l)) nil)
    (when (equal (name (cadr l)) name)
      (setf (cdr l) (cddr l))
      (return name))))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry (find name (cdr *entries*)
		     :key #'name
		     :test #'equal)))
    (when (null entry)
      (report-error t
        "~%No test with name ~:@(~S~)."
	name))
    entry))

(defmacro deftest (name form &rest values)
  `(add-entry '(t ,name ,form .,values)))

(defun add-entry (entry)
  (setq entry (copy-list entry))
  (do ((l *entries* (cdr l))) (nil)
    (when (null (cdr l))
      (setf (cdr l) (list entry))
      (return nil))
    (when (equal (name (cadr l)) 
		 (name entry))
      (setf (cadr l) entry)
      (report-error nil
        "Redefining test ~@:(~S~)"
        (name entry))
      (return nil)))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug* 
	 (apply #'format t args)
	 (if error? (throw '*debug* nil)))
	(error? (apply #'error args))
	(t (apply #'warn args))))

(defun do-test (&optional (name *test*))
  (do-entry (get-entry name)))

(defun do-entry (entry &optional
		 (s *standard-output*))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
	   (*break-on-warnings* t)
	   (r (multiple-value-list
		(eval (form entry)))))
      (setf (pend entry)
	    (not (equal r (vals entry))))
      (when (pend entry)
	(format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value~P: ~
                      ~{~S~^~%~17t~}~
                   ~%Actual value~P: ~
                      ~{~S~^~%~15t~}.~%"
		*test* (form entry)
		(length (vals entry))
		(vals entry)
		(length r) r))))
      (when (not (pend entry)) *test*))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional
		 (out *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
      (do-entries out)
      (with-open-file 
	  (stream out :direction :output)
	(do-entries stream))))

(defun do-entries (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (cdr *entries*)
		 :key #'pend)
	  (length (cdr *entries*)))
  (dolist (entry (cdr *entries*))
    (when (pend entry)
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
	      (do-entry entry s))))
  (let ((pending (pending-tests)))
    (if (null pending)
	(format s "~&No tests failed.")
	(format s "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		(length pending)
		(length (cdr *entries*))
		pending))
    (null pending)))

;convenient shorthand
(defun more () (continue-testing))

(rem-all-tests)

(defmacro test-print (x &rest bindings)
  `(let* ((*print-level* nil) (*print-length* nil) (*print-lines* nil)
	  (*print-right-margin* 70) (*print-miser-width* 0)
	  (*print-pretty* nil)
	  .,(copy-list bindings))
     (with-output-to-string (s) (pascal-write ',x :stream s))))

(deftest exp1 (test-print (+ 1 big-number)) "1+BigNumber")
(deftest exp2 (test-print (+ (- 1 2) (+ 2 3))) "1-2+(2+3)")
(deftest exp3 (test-print (* (- 1 2) (+ 2 3))) "(1-2)*(2+3)")
(deftest exp3.1 (test-print (* (- 1 2) (+ 2 3)) (*print-right-margin* 8))
  "(1-2)
*(2+3)")
(deftest exp3.2 (test-print (* (- 1 2) (+ 2 3)) (*print-right-margin* 5))
  "(1-2)
*(2
  +3)")
(deftest exp4 (test-print (* (/ 1 2) (* (- 2) 3))) "1/2*((-2)*3)")
(deftest exp5 (test-print (and (or (< 2 3) (>= 3 4)) (\= (+ 2 2) (* 3 1.0))))
  "((2 < 3) or (3 >= 4)) and (2+2 = 3*1.0)")
(deftest exp6 (test-print (or (and (< 2 3) (>= (fob-limit 3 3.1) 4))
			      (\= (+ 2 2) (* 3 1.0))))
  "(2 < 3) and (FobLimit(3, 3.1) >= 4) or (2+2 = 3*1.0)")
(deftest exp7 (test-print (not (and (equal #\b b) (equal a "don't"))))
  "not (('b' = B) and (A = 'don''t'))")
(deftest exp7.1 (test-print (and (not (equal a b)) (< a b))
			    (*print-right-margin* 14))
  "not (A = B)
 and (A < B)")

(deftest exp9 (test-print (* (abs 2) (- (abs(/ 3 4))))) "Abs(2)*(-Abs(3/4))")
(deftest exp10 (test-print (- (my-func (* baz (+ 2 1)) bar (/ tuz 3.0))))
  "-MyFunc(Baz*(2+1), Bar, Tuz/3.0)")
(deftest exp10.1 (test-print (- (my-func (* baz (+ 2 1)) bar (/ tuz 3.0)))
			     (*print-right-margin* 20))
  "-MyFunc(Baz*(2+1),
        Bar,
        Tuz/3.0)")
(deftest exp11 (test-print (setq big-var (+ (* last-value 236.2) another-var)))
  "BigVar := LastValue*236.2+AnotherVar")
(deftest exp11.1 (test-print (setq big-var (+ (* last-value 236.2) another-var))
			     (*print-right-margin* 20))
  "BigVar :=
 LastValue*236.2
 +AnotherVar")
(deftest exp11.2 (test-print (setq big-var (+ (* last-value 236.2) another-var))
			     (*print-right-margin* 15))
  "BigVar :=
 LastValue
 *236.2
 +AnotherVar")


(deftest state1 (test-print (setq my-var (+ your-var (/ x y))))
  "MyVar := YourVar+X/Y")
(deftest state1.1 (test-print (setq my-var (+ your-var (/ x y)))
			      (*print-right-margin* 15))
  "MyVar :=
 YourVar+X/Y")
(deftest state2 (test-print (if (<= your-var my-var)
				(foo-bar your-var my-var)
				(your-func)))
  "if YourVar <= MyVar then FooBar(YourVar, MyVar) else YourFunc")
(deftest state2.1 (test-print (if (<= your-var my-var)
				  (foo-bar your-var my-var)
				  (+ the-var (your-func)))
			      (*print-right-margin* 35))
  "if YourVar <= MyVar then
   FooBar(YourVar, MyVar)
else
   TheVar+YourFunc")
(deftest state2.2 (test-print (if (<= your-var my-var)
				  (foo-bar your-var my-var)
				  (+ the-var (your-func)))
			      (*print-right-margin* 16))
  "if YourVar
    <= MyVar
then
   FooBar(YourVar,
          MyVar)
else
   TheVar
   +YourFunc")
(deftest state2.3 (test-print (if a (when b c) (unless e f g)))
  "if A then begin if B then C end else if not E then begin F; G end")
(deftest state2.4 (test-print (if a (unless b c)))
  "if A then if not B then C")
(deftest state2.5 (test-print (if a (if b c d)))
  "if A then if B then C else D")
(deftest state2.6 (test-print (if a (if b c d) (when e f g h)))
  "if A then if B then C else D else if E then begin F; G; H end")
(deftest state2.7 (test-print (if a (if b c d) (when e f g h))
			     (*print-right-margin* 30))
  "if A then
   if B then C else D
else
   if E then begin F; G; H end")
(deftest state2.8 (test-print (if a (if b c d) (when e f g h))
			     (*print-right-margin* 20))
  "if A then
   if B then
      C
   else
      D
else
   if E then
      begin
        F;
        G;
        H
      end")

(deftest state3 (test-print (progn (if x y) (setq a b) (setq c d)))
  "begin if X then Y; A := B; C := D end")
(deftest state3.1 (test-print (progn (if x y) (setq a b) (setq c d))
			      (*print-right-margin* 20))
  "begin
  if X then Y;
  A := B;
  C := D
end")
(deftest state3.2 (test-print (progn (if x y)))
  "begin if X then Y end")
(deftest state3.3 (test-print (progn))
  "")
(deftest state4 (test-print (loop (if (> x 0) (return nil))
				  (setq x (+ x 1))
				  (setq y (* 2 y))))
  "while not (X > 0) do begin X := X+1; Y := 2*Y end")
(deftest state4.1 (test-print (loop (when (> x 0) (return nil))
				    (setq x (+ x 1))
				    (setq y (* 2 y)))
			      (*print-right-margin* 30))
  "while not (X > 0) do
  begin X := X+1; Y := 2*Y end")
(deftest state4.2 (test-print (loop (when (> x 0) (return nil))
				    (setq x (+ x 1))
				    (setq y (* 2 y)))
			      (*print-right-margin* 22))
  "while not (X > 0) do
  begin
    X := X+1;
    Y := 2*Y
  end")
(deftest state4.3 (test-print (loop (setq x (+ x 1))
				    (if (> x 0) (return nil))))
  "repeat X := X+1 until X > 0")
(deftest state4.4 (test-print (loop (setq x (+ x 1))
				    (setq y (* 2 y))
				    (when (> x 0) (return nil)))
			      (*print-right-margin* 25))
  "repeat
  X := X+1;
  Y := 2*Y
until X > 0")
(deftest state4.5 (test-print (loop (setq x (+ x 1))
				    (if (not (> x 0)) (return nil)))
			      (*print-right-margin* 30))
  "repeat X := X+1
until not (X > 0)")

(deftest big (test-print
	       (defun my-sqrt (num &aux my-sqrt)
		 (declare (float num) (float my-sqrt))
		 (setq my-sqrt 1.0)
		 (loop (when (< (abs (- (* my-sqrt my-sqrt) num)) 1.0E-4)
			 (return nil))
		       (setq my-sqrt (/ (+ my-sqrt (/ num my-sqrt)) 2.0)))
		 my-sqrt))
  "function MySqrt (Num: Real): Real;
begin
  MySqrt := 1.0;
  while not (Abs(MySqrt*MySqrt-Num) < 1.0E-4) do
    MySqrt := (MySqrt+Num/MySqrt)/2.0
end")
(deftest big2 (test-print
		 (defun foo (num n m &aux u v)
		   (declare (float num n u) (integer m v))
		   (setq u (+ n m))
		   (prin1 u)
		   (terpri)
		   (if (> m 0) (setq v (* num m)) (prin1 (* n m))))
		 (*print-right-margin* 30))
  "procedure Foo (Num: Real;
               N: Real;
               M: Integer);
  var
    U: Real;
    V: Integer;
begin
  U := N+M;
  Write(U);
  Writeln;
  if M > 0 then
     V := Num*M
  else
     Write(N*M)
end")
(deftest rational (test-print 1/2) "0.5")
 
(deftest paper1 (test-print
		  (defun sqt (n &aux sqt)
		    (declare (float n) (float sqt))
		    (setq sqt 1.0)
		    (loop (when (< (abs (- (* sqt sqt) n))
				   1.0E-4)
			    (return nil))
			  (setq sqt 
				(/ (+ sqt (/ n sqt)) 2.0)))
		    sqt)
		  (*print-right-margin* 43))
  "function Sqt (N: Real): Real;
begin
  Sqt := 1.0;
  while not (Abs(Sqt*Sqt-N) < 1.0E-4) do
    Sqt := (Sqt+N/Sqt)/2.0
end")
(deftest paper1.1 (progn
		    (defun sqt (n &aux sqt)
		      (declare (float n) (float sqt))
		      (setq sqt 1.0)
		      (loop (when (< (abs (- (* sqt sqt) n))
				     1.0E-4)
			      (return nil))
			    (setq sqt 
				  (/ (+ sqt (/ n sqt)) 2.0)))
		      sqt)
		    (values (round (* 1000 (sqt 144)))))
  12000)

(deftest paper2 (test-print "Bob's house") "'Bob''s house'")
(deftest paper3 (test-print "say \"Hi\"")  "'say \"Hi\"'")
(deftest paper4 (test-print #\s)  "'s'")
(deftest paper5 (test-print first-num) "FirstNum")
(deftest paper6 (test-print break-level-2) "BreakLevel2")
(deftest paper7 (test-print (* (+ 1 2) 3)) "(1+2)*3")
(deftest paper8 (test-print (* (* 1 2) 3)) "1*2*3")
(deftest paper9 (test-print (* 1 (* 2 3))) "1*(2*3)")

(deftest paper10 (test-print (> Threshold (+ new-val delta))
		   (*print-right-margin* 43))
  "Threshold > NewVal+Delta")
(deftest paper10.1 (test-print (> Threshold (+ new-val delta))
		     (*print-right-margin* 20))
  "Threshold
 > NewVal+Delta")
(deftest paper10.2  (test-print (> Threshold (+ new-val delta))
		     (*print-right-margin* 10))
  "Threshold
 > NewVal
   +Delta")

(deftest paper11 (test-print (terpri)) "Writeln")
(deftest paper12 (test-print (log x)) "Ln(X)")
(deftest paper13 (test-print (my-fn a b c)) "MyFn(A, B, C)")



(deftest paper14 (test-print (my-fn epsilon (+ end delta) total)
			     (*print-right-margin* 43))
  "MyFn(Epsilon, End+Delta, Total)")
(deftest paper14.1 (test-print (my-fn epsilon (+ end delta) total)
			       (*print-right-margin* 20))
  "MyFn(Epsilon,
     End+Delta,
     Total)")

(deftest paper15 (test-print (if a (if b c) d))
  "if A then begin if B then C end else D")
(deftest paper16 (test-print (if a (if b c d)))
  "if A then if B then C else D")

(deftest paper17 (test-print
		  (loop (setq result (* result x))
			(setq count (- count 1))
			(when (= count 0) (return nil)))
		  (*print-right-margin* 43))
  "repeat
  Result := Result*X;
  Count := Count-1
until Count = 0")

(deftest paper18 (test-print
		   (defun print-exp (x i &aux count result)
		     (declare (integer x count result))
		     (setq count i)
		     (setq result 1)
		     (loop (setq result (* result x))
			   (setq count (- count 1))
			   (when (= count 0) (return nil)))
		     (print result))
		   (*print-right-margin* 43))
  "procedure PrintExp (X: Integer;
                    I: Integer);
  var
    Count: Integer;
    Result: Integer;
begin
  Count := I;
  Result := 1;
  repeat
    Result := Result*X;
    Count := Count-1
  until Count = 0;
  Print(Result)
end")
(deftest paper18.1 (progn
		     (defun print-exp (x i &aux count result)
		       (declare (integer x count result))
		       (setq count i)
		       (setq result 1)
		       (loop (setq result (* result x))
			     (setq count (- count 1))
			     (when (= count 0) (return nil)))
		       (print result))
		     (with-output-to-string (*standard-output*)
		       (print-exp 3 3)))  "
27 ")
  

#|----------------------------------------------------------------------------|
 | Copyright 1992 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#
