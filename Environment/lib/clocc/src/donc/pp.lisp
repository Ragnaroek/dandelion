;;; -*- Mode:LISP; Package: user; Base:10; Syntax: Common-lisp -*-

;; I, Donald Cohen (donc@{isi.edu,compsvcs.com,ap5.com} hereby
;; (1) assert that I am the author of this file and
;; (2) place it in the public domain.  Feb. 1, 2000.

#| A very small (but perhaps very good) pretty printer by Don Cohen.
 This was motivated by the following properties of available prettyprinters:
 - no ability to declare printmacros
 - a tendency to print a narrow column at the right margin
 - a tendency to take a long time

 These are largely solved by the new Waters prettyprinter (the solutions 
 are quite similar), but I still like my solution to the 2nd problem.

 This is not intended to deal with things like *print-circle*, 
 *print-length*, *print-level*, etc.  It's for formatting data and code 
 made of lists.

 See the start of the code below for specials that you can set to influence
 global behavior, and also the arguments to the main entry point, pp.

 Printmacros work as follows:
 To declare a printmacro for lists starting with FOO,
   do (setf (get 'foo :printmacro) <macro>).
 <macro> should be a function of the same four arguments as pp (see below).
 It may either decide not to handle the form, by returning nil (and not 
 printing) or it may print the form and return the resulting position.
 The functions here will probably be useful in doing that printing.
 See the printmacros at the end for examples to follow when writing your own.
 Incidentally, horizontal position is the column that the next character, if
 printed, would be in - after a terpri, the horizontal position is 1.

 We now allow printsizemacros to tell pp how much space something is likely
 to take.  In the same way normal printmacros are analogous to pp, a
 printsizemacro is analogous to prinsize.  They are declared via
   (setf (get 'foo :printsizemacro) <macro>).
 The arguments are a form (whose car is foo) and a maximum size which may
 be nil (meaning no maximum).  The meaning of the maximum is that anything
 greater is equivalent - it won't fit.
 Again, the macro may either return a number or nil, which means that it
 does not apply and the normal mechanism should be used.

 We also allow "CONSPmacros" which tell whether a list (whose car is a
 symbol, probably a printmacro) should be treated as a cons.  Since 
 non-conses are treated specially (keep printing after them on the same line)
 this gives you the chance to declare that a cons should be treated that way.
 Again, (setf (get 'foo :conspmacro) <macro>),
 where the macro will be given arguments like (foo x) and should return T
 if they should be treated like conses, nil if not.

 ---------------- History of Changes: ----------------
 1/28/2000
 put it all back into a pp package, minor formatting
 tried to generalize backquote (Do I have it right?) and unify it
 with ', #' (what others?)
 also made prinsize work right for symbols (deal with packages)
  -- there must be a cheaper way to do this
 10/06/88 12:08:25
 changed pp-rest behavior - if next element (regardless of consp*) fits,
 print it, and then start on next line only if it's consp*
 4/19/87 19:16:40
 changed last calls to write into calls to PP
 * commitment: (?) *
 In general, printmacros ought to use PP to print things that correspond
 to lisp objects.  This allows advice to PP to catch all such objects that
 are printed.  [This is used by Kai's editor correspondence regions.]
 4/19/87 17:39:10
 Added conspmacros and printlengthmacros
 4/19/87 16:59:39
 Added printmacros for backquote at Kai's request
 5/02/86 11:04:26
 Following Eve's preference, lists with lists as car are now printed
 without miser indentation.
 Also, after looking at performance in IQ lisp, Prinsize is now
 optimized to do less consing.
 12/18/85 18:23:35
 removed prin - just use write, which is sensitive to all printer flags
 10/31/85 18:06:26
 Since you get to load files into the pkg of your choice, you can now get
 the whole thing defined in whatever package you like.  Note, however, that
 the printmacros defined below will be for the package you choose as well!
 - Introduced tabsize and made moveto use it.  This is just to save space by
 printing tabs rather than lots of leading spaces.
|#

(in-package :user)
(eval-when (eval load compile)
  (when (find-package :pp)
    (cerror "continue"
	    "Package pp already exists!  Continue will redefine it.")))
(defpackage "PP" (:use "LISP")
	    (:export "PP" "MOVETO" "PRINSIZE" "PP-WIDTH" "PP-REST"
		     "PP-REST-ACROSS" "MAXSIZE" "MIN-MISER-CAR"
		     "MAX-NORMAL-CAR" "MISER-SIZE" "TABSIZE"))
(in-package :pp)
(provide :pp)

;; ---------------- declarations ----------------

(declaim (special maxsize tabsize min-miser-car max-normal-car miser-size))

;; -------- Parameters that can be set by the user: --------

(setq maxsize 50.)  
;; try not to print more than maxsize chars on one line (it's hard to read)

(setq tabsize 8)
;; one tab is worth 8 spaces - to turn off tabs, set this to nil

(setq miser-size 2)
;; how many spaces to indent in miser mode

(setq min-miser-car 4)
;; don't be a miser if you're already getting off cheap,
;; e.g., (car ...) is not much more horizontal space than
;; (car
;;   ...)
;; 4 actually means that CONS may miser but CAR cannot

(setq max-normal-car 9)
;; be a miser if this car is long,
;; e.g., (verylongfunctionname ...) takes much more space than
;; (verylongfunctionname
;;   ...)
;; 9 actually means that a 9 character function name will always miser
;; while an 8 character name may not

; ---------------- the code ----------------

;; pp prettyprints x to stream and returns its ending horizontal position.
;; It has to be told its current position and the right margin.
;; It tries not to go over right margin, but no guarantees.

(defun pp (x &optional (stream *standard-output*) (curpos 1) (rmargin 80)
	     &aux size position width)
  (cond ((not (consp x)) (write x :stream stream) (+ curpos (prinsize x)))
	((printmacrop x stream curpos rmargin))
	;; should return nil if decides not to handle it, else result position
	((and (> curpos (* 7 (/ rmargin 8))) ;; near right margin
	      (> (pp-width x (/ rmargin 4)) (/ rmargin 4))) ;; long way to go
	 (format stream "~& ;; pp fold~& ")
	 (pp x stream 2 rmargin)
	 (format stream "~& ;; pp unfold~&")
	 (moveto stream 1 curpos)
	 curpos)
	(t (write-char #\( stream)
	   (setq position (pp (car x) stream (1+ curpos) rmargin))
	   (cond ((and (>= (setq width (- rmargin position))
			   (setq size (prinsize (cdr x) width)))
		       (<= size maxsize))  ;; just print it
		  (pp-rest-across (cdr x) stream position rmargin))
		 ((consp (car x)) 
		  (moveto stream position curpos) ;; eve's suggestion
		  (pp-rest (cdr x) stream curpos rmargin))
		 ((> (- position curpos) max-normal-car)
		  (moveto stream position (+ curpos miser-size)) ;; miser mode
		  (pp-rest (cdr x) stream (+ curpos miser-size) rmargin))
		 ((and (> (- position curpos) min-miser-car)
		       ;; the real reason for miser mode
		       (> (pp-width x width) width)
		       #+ignore ;; this might be better if folding enabled
		       ;; - if too far to go then don't miser - just fold
		       ;; but it's also more expensive
		       (> (* 2 width) (pp-width x (* 2 width)) width))
		  (moveto stream position (+ curpos miser-size))
		  (pp-rest (cdr x) stream (+ curpos miser-size) rmargin))
		 (t (pp-rest (cdr x) stream position rmargin))))))

#|
 some data on an example (switchpeople - depth 19 - rather deep)
 817 conses, 575 symbols, 170 lines 80 wide (fold or no fold)
 # calls on
  pp  pp-width  prinsize  #columns
  797   268     3125      50 no fold
  794   677     4574      80 no fold

  807   308     3241      50 fold
  794   677     4574      80 fold

  831   637     3711      50 fold with (* 2 width) test
  794  1785     5626      80 fold with (* 2 width) test
|#

#+ignore
(defun prin (x stream)
  (cond (cl::*print-escape* (prin1 x stream))
	(t (princ x stream))))

;; useful for tab + terpri; suppresses multiple cr's
;; move from curpos to goalpos
(defun moveto (stream curpos goalpos)
  (cond ((> curpos goalpos)
         (terpri stream)
	 (setq curpos 1)))
  (and tabsize (loop until (< (- goalpos curpos) tabsize) do
		     (write-char #\tab stream)
		     (setq curpos (+ curpos tabsize))))
  (loop for i from curpos to (1- goalpos) do (write-char #\  stream))
  goalpos)

(defun printmacrop (x stream curpos rmargin &aux macro)
  (and (symbolp (car x))  ;; only called on lists
       (setq macro (get (car x) :printmacro))
       (funcall macro x stream curpos rmargin)))

(defun printmacrosizep (x max &aux macro)
  (and (symbolp (car x))  ;; only called on lists
       (setq macro (get (car x) :printsizemacro))
       (funcall macro x max)))

;; similar to flatsize, but if size > max just return anything over max
;; Here we could make things faster by caching the sizes of non consp
;; objects for the duration of a top level call to pp.
;; That would involve adding a new special for the table and a new argument 
;; to all the mutually recursive functions.
(defun prinsize (x &optional max &aux (ans 0))
  (cond #+ignore ;; no, even the string case is more complex
        ;; have to worry about quoted characters
        ((stringp x) (+ 2 (length x))) ;; quotes
	;; this was ok until we had to deal with packages!
	;; ((symbolp x) (length (symbol-name x)))
	((not (consp x)) ;; I hate to do it this way - any better ideas?
	 (length (write-to-string x)))
	((printmacrosizep x max))
	;; again, the sizemacro should return nil or a number
	(t (setq ans (1+ ans))  ;; 2 parens minus one space
	   (prog ((tail x) tmp)
	      lp (cond ((and max (> ans max)) (return))
		       ((null tail)(return))
		       ((not (consp tail)) (setq tmp tail) (setq tail nil))
		       (t (setq tmp (car tail)) (setq tail (cdr tail))))
		 (setq ans (+ 1 ans (prinsize tmp (and max (- max ans)))))
		 (go lp))
	   #+ignore(loop for y in x until (and max (> ans max)) do
		   (setq ans (+ ans 1)) ;; the space
		   (setq ans (+ ans (prinsize y (- max ans)))))
	   ans)))


;; the width that "normal" pp would take - used to decide when to be a miser
 
;; Actually, the (frequent) use of this function makes the algorithm n^2 in 
;; the worst case, expected n log n.  With a little more trouble (which I 
;; don't think is worth it) one could scan the structure once and save the 
;; work, thereby reducing the whole algorithm to worst case linear (?)

(defun pp-width (x &optional max &aux car)
  ;; computes width with normal pp indentation
  ;; except that it now considers max-normal-car
  ;; again anything over max is equivalent
  (cond ((not (consp x)) (prinsize x))
	(t (setq car (cond ((consp (car x)) miser-size)
			   (t (+ 1 (prinsize (car x))))))  ;; paren
	   (cond ((> car max-normal-car) (setq car miser-size)))
	   (cond ((null (cdr x)) (1+ car))  ;; right paren
		 ((and max (>= car max)) (1+ max))
		 (t (setq car (+ 2 car))  ;; paren and space
		    (+ car
		       (prog ((cdr 0) (tail (cdr x))
			      (newmax (and max (- max car))) tmp)
			  lp (cond
			       ((null tail)(return cdr))
			       ((and newmax (> cdr newmax)) (return cdr))
			       ((not (consp tail))
				(setq tmp tail) (setq tail nil))
			       (t (setq tmp (car tail)) (setq tail (cdr tail))))
			     (cond
			       ((> (setq tmp (pp-width tmp newmax)) cdr)
				(setq cdr tmp)))
			     (go lp))))))))


;; the tacit assumption is that printmacros will shorten rather than lengthen
;; the output - this is used when you want to put the rest on the same line
;; just like printing a tail, but watch out for printmacros
(defun pp-rest-across (x stream curpos rmargin &aux (position curpos))
  (prog nil
	lp
	(cond ((null x)
	       (write-char #\) stream)
	       (return (1+ position)))
	      ((not (consp* x))
	       (princ " . " stream)
	       (pp x stream position rmargin)
	       (write-char #\) stream)
	       (return (+ 4 position (prinsize x))))
	      (t (write-char #\  stream)
		 (setq position
		       (pp (car x) stream (1+ position) rmargin))
		 (setq x (cdr x))
		 (go lp)))))

;; normal way to print a tail
(defun pp-rest (x stream curpos rmargin &aux (position curpos));; pos2 size max
  (prog nil
	lp
	(cond ((null x)  ;; don't worry about rmargin here
	       (write-char #\) stream)
	       (return (1+ position)))
	      ((not (consp x))  ;; same here
	       (and (> (prinsize x) (- rmargin position 3))
		    (setq position (moveto stream position curpos)))
	       (princ " . " stream)
	       (pp x stream position rmargin)
	       (write-char #\) stream)
	       (return (+ position 4 (prinsize x))))
	      ((let ((max (- (min rmargin (+ curpos maxsize)) position)))
		 (< (prinsize (car x) max) max))
	       (write-char #\  stream)
	       (setq position (pp (car x) stream (1+ position) rmargin)))
	      (t (moveto stream position (1+ curpos))
		 (setq position (pp (car x) stream (1+ curpos) rmargin))))
	(cond ((and (consp* (car x)) (cdr x))
	       (setq position (moveto stream position curpos))))
        (setq x (cdr x))
        (go lp)))

#+ignore  ;; old
(defun pp-rest (x stream curpos rmargin &aux (position curpos) pos2)
  (prog nil
	lp
	(cond ((null x)  ;; don't worry about rmargin here
	       (write-char #\) stream)
	       (return (1+ position)))
	      ((not (consp x))  ;; same here
	       (and (> (prinsize x) (- rmargin position 3))
		    (setq position (moveto stream position curpos)))
	       (princ " . " stream)
	       (pp x stream position rmargin)
	       (write-char #\) stream)
	       (return (+ position 4 (prinsize x))))
	       ((and (not (consp* (car x)))
		    ;; if an "atom" (see below) fits, what the hell...
		    (<= (setq pos2
			      (+ position 1 (prinsize (car x))))
			rmargin)
		    (<= pos2 (+ curpos maxsize)))
	       (write-char #\  stream)
	       (pp (car x) stream position rmargin)
	       (setq position pos2))
	      (t (moveto stream position (1+ curpos))
		 (setq position (pp (car x) stream (1+ curpos) rmargin))))
	(cond ((and (consp (car x)) (cdr x))
	       (setq position (moveto stream position curpos))))
        (setq x (cdr x))
        (go lp)))

(defun consp* (x &aux macro)
  (cond ((not (consp x)) nil)
	((not (symbolp (car x))) t)
	((not (setq macro (get (car x) :conspmacro))) t)
	(t (funcall macro x))))

;; ---------------- the obvious printmacros ----------------

(defun prefix-macro (prefix)
  (lambda (x stream pos rmargin)
    (cond ((and (cdr x) (null (cddr x)))
	   (format stream prefix)
	   (pp (cadr x) stream (+ pos (length prefix)) rmargin)))))

(defun prefix-size (prefix)
  (lambda (x max)
    (cond ((and (cdr x) (null (cddr x)))
	   (+ (length prefix) (prinsize (cadr x) max))))))

(defun consp-cadr (x) (consp (cadr x))) ;; the common case

(setf (get 'quote :printmacro) (prefix-macro "'")
      (get 'quote :printsizemacro) (prefix-size "'")
      (get 'quote :conspmacro) 'consp-cadr)
(setf (get 'function :printmacro) (prefix-macro "#'")
      (get 'function :printsizemacro) (prefix-size "#'")
      (get 'function :conspmacro) 'consp-cadr)

#-symbolics ;; we could use something cross-platform for backquote.
(eval-when (load) ;; are these the only cases?
  (let ((example '`(list ,car (cadr ,.cdr) ,@cdr)) ex)
    (setf ex example)
    (if (or (member (car ex) '(list car cadr cdr))
	    (not (= 2 (length ex))))
	(warn "confusion about backquote")
      (setf (get (car ex) :printmacro) (prefix-macro "`")
	(get (car ex) :printsizemacro) (prefix-size "`")
	(get (car ex) :conspmacro) (get 'quote :conspmacro)))
    (setf ex (cadadr example))
    (if (or (member (car ex) '(list car cadr cdr))
	    (not (= 2 (length ex))))
	(warn "confusion about backquote comma")
      (setf (get (car ex) :printmacro) (prefix-macro ",")
	(get (car ex) :printsizemacro) (prefix-size ",")
	(get (car ex) :conspmacro) (get 'quote :conspmacro)))
    (setf ex (second (third (second example))))
    (if (or (member (car ex) '(list car cadr cdr))
	    (not (= 2 (length ex))))
	(warn "confusion about backquote comma-dot")
      (setf (get (car ex) :printmacro) (prefix-macro ",.")
	(get (car ex) :printsizemacro) (prefix-size ",.")
	(get (car ex) :conspmacro) (get 'quote :conspmacro)))
    (setf ex (fourth (second example)))
    (if (or (member (car ex) '(list car cadr cdr))
	    (not (= 2 (length ex))))
	(warn "confusion about backquote comma-att")
      (setf (get (car ex) :printmacro) (prefix-macro ",@")
	(get (car ex) :printsizemacro) (prefix-size ",@")
	(get (car ex) :conspmacro) (get 'quote :conspmacro)))))  

;; it seems that symbolics has already done the work for backquote ...

#+symbolics
(defun pp-bq (x stream pos rmargin)
  (princ "`" stream)
  (pp (si::GRIND-UNBACKQUOTIFY x) stream (1+ pos) rmargin))
#+symbolics
(defun bq-prinsize (x max)
  ;; admittedly this is inefficient if x is large and max is small ...
  (prinsize (si::grind-unbackquotify x) max))

#+symbolics
(progn
  (setf (get 'si::XR-BQ-CONS :printmacro) 'pp-BQ)
  (setf (get 'si::XR-BQ-list :printmacro) 'pp-BQ)
  (setf (get 'si::XR-BQ-list* :printmacro) 'pp-BQ)
  (setf (get 'si::XR-BQ-append :printmacro) 'pp-BQ)
  (setf (get 'si::XR-BQ-nconc :printmacro) 'pp-BQ)
  (setf (get 'si::GRIND-DOT-COMMA :printmacro)
	'(lambda (x stream pos rmargin)
	   (princ ". ," stream)
	   (pp (cadr x) stream (+ pos 3) rmargin)))
  (setf (get 'si::GRIND-COMMA :printmacro)
	'(lambda (x stream pos rmargin)
	   (princ "," stream)
	   (pp (cadr x) stream (1+ pos) rmargin)))
  (setf (get 'si::GRIND-COMMA-dot :printmacro)
	'(lambda (x stream pos rmargin)
	   (princ ",." stream)
	   (pp (cadr x) stream (+ pos 2) rmargin)))
  (setf (get 'si::GRIND-COMMA-atsign :printmacro)
	'(lambda (x stream pos rmargin)
	   (princ ",@" stream)
	   (pp (cadr x) stream (+ pos 2) rmargin)))
  (setf (get 'si::GRIND-DOT-COMMA :printsizemacro)
	'(lambda (x max) (+ 3 (prinsize (cadr x) max))))
  (setf (get 'si::GRIND-COMMA :printsizemacro)
	'(lambda (x max) (1+ (prinsize (cadr x) max))))
  (setf (get 'si::GRIND-COMMA-dot :printsizemacro)
	'(lambda (x max) (+ 2 (prinsize (cadr x) max))))
  (setf (get 'si::GRIND-COMMA-atsign :printsizemacro)
	'(lambda (x max) (+ 2 (prinsize (cadr x) max))))
  (loop for x in '(si::GRIND-COMMA-atsign si::GRIND-COMMA-dot
		   si::GRIND-COMMA si::GRIND-DOT-COMMA)
	do (setf (get x :conspmacro) 'consp-cadr)))

(setf (get 'defun :printmacro)
      (lambda (x stream pos rmargin &aux (cur pos) (origpos pos))
	(cond ((> (length x) 3)
	       (write-char #\( stream)
	       (pp (car x) stream (1+ pos) rmargin)
	       (write-char #\  stream)
	       (pp (cadr x) stream (+ 7 pos) rmargin)
	       (write-char #\  stream)
	       (setq cur
		 (pp (caddr x)
		     stream
		     (setq pos (+ 3 pos
				  (prinsize (cadr x))
				  (prinsize (car x))))
		     rmargin))
	       (moveto stream cur (+ origpos 1))
	       (pp-rest (cdddr x) stream (+ origpos 1) rmargin)))))

(defun pp-binding-form (x stream pos rmar &aux (cur pos))
  ;; at least start the bound vars on the same line
  (cond ((> (length x) 2)
	 (write-char #\( stream)
	 (pp (car x) stream (1+ pos) rmar)
	 (write-char #\  stream)
	 (setq cur (pp (cadr x) stream (+ 2 pos (prinsize (car x))) rmar))
	 (moveto stream cur (+ pos 2))
	 (pp-rest (cddr x) stream (+ pos 2) rmar))))

(setf (get 'let :printmacro) 'pp-binding-form)
(setf (get 'prog :printmacro) 'pp-binding-form)
(setf (get 'lambda :printmacro) 'pp-binding-form)
(setf (get 'macrolet :printmacro) 'pp-binding-form)
(setf (get 'flet :printmacro) 'pp-binding-form)
(setf (get 'labels :printmacro) 'pp-binding-form)

(defun pp-tagbody (x stream pos rmar &aux (cur pos))
  (write-char #\( stream)
  (setf cur (pp (car x) stream (1+ pos) rmar))
  (moveto stream cur (+ pos 2))
  (pp-rest (cdr x) stream (+ pos 2) rmar))

(setf (get 'tagbody :printmacro) 'pp-tagbody)

(defun fits (x pos rmar &aux width size)
  (and (>= (setq width (- rmar pos))
	   (setq size (prinsize x width)))
       (<= size maxsize)
       (+ pos size)))

;; this is what Dennis wants if's to look like ...
(defun pp-if (x stream pos rmar &aux (cur (+ pos 1)) temp)
  (cond ((setq temp (fits x pos rmar))
	 (write x :stream stream)  ;; just print it
	 temp)
	(t (write-char #\( stream)
	   (setq pos (+ pos 1))
	   (loop while x do
		 ;; assume we're on if/elseif and print to next such
		 (moveto stream cur pos)
		 (write (car x) :stream stream)  ;;print the if/elseif/else
		 (and (cdr x) (write-char #\  stream))
		 (setq cur
		       (+ pos (cond ((cdr x) 1)  ;; worry about end of list
				    (t 0))
			      (prinsize (car x))))   ;;if/elseif/else<space>
		 (cond ((null (cdr x)) (setq x nil))  ;;premature end?? - dga
		       ((string-equal (car x) "ELSE")	;;has no <cond> - dga
			(setq x (cdr x)))
		       ((setq temp (fits (cadr x) cur rmar))
			(write (cadr x) :stream stream)
			(setq cur temp) ;; now try to put in the THEN
			(cond ((null (cddr x)) (setq x nil))
			      ((and (symbolp (caddr x))
				    (or (string-equal (caddr x) "THEN"))
				    (setq temp
					  (fits (caddr x) cur (- rmar 1))))
			       (write-char #\  stream)
			       (write (caddr x) :stream stream)
			       (setq cur (+ 1 temp))
			       (setq x (cdddr x))) ;; start after the THEN
			      (t (setq x (cddr x))))) ;; start with the THEN
		       (t (setq cur (pp (cadr x) stream cur rmar))
			  (moveto stream cur pos)  ;;was missing - dga
			  (write (caddr x) :stream stream) ;;write THEN
			  (setq x (cdddr x)))) ;;continue with rest
		 (loop until (or (null x)
				 (and (symbolp (car x))
				      (member (car x) '(if elseif else)
					      :test #'string-equal)))
		       do
		       (moveto stream cur (+ pos 2))
		       (setq cur (pp (pop x) stream (+ 2 pos) rmar))))
	   (write-char #\) stream)
	   (+ cur 1))))

;; in order to use this, do
;;(setf (get '<your-package>::if :printmacro) '<this-package>::pp-if)

(defun cadr-size (x max)
  (prinsize (cadr x) max))
#+symbolics
(loop for x in si:*digested-special-forms* do
      (setf (get x :printmacro)
	    '(lambda (x stream pos rmargin)
		(pp (cadr x) stream pos rmargin)))
      (setf (get x :printsizemacro) 'cadr-size)
      (setf (get x :conspmacro) 'consp-cadr))
#+ignore(SI:DIGESTED-MULTIPLE-VALUE-BIND
   SI:DIGESTED-MACROLET SI:DIGESTED-FLET
   SI:DIGESTED-LABELS SI:DIGESTED-LAMBDA
   SI:DIGESTED-DO* SI:DIGESTED-DO SI:DIGESTED-PROG*
   SI:DIGESTED-PROG SI:DIGESTED-LET* SI:DIGESTED-LET)

#+ (or TI symbolics)
(progn
  (setf (get 'si:displaced :printmacro)
	'(lambda (x stream pos rmargin)
	   (pp (cadr x) stream pos rmargin)))
  (setf (get 'si:displaced :printsizemacro) 'cadr-size)
  (setf (get 'si:displaced :conspmacro) 'consp-cadr))


#|
 ---------------- changes under consideration: ----------------

 - Probably ought to have more printmacros:
   - cond could stand a macro, though the default is reasonable
   - prog could stand a macro - put labels further to the left
 
 - perhaps maxsize should be not a matter of how many chars but
   rather how many parens

 - improve speed by only doing pp-size estimate once
   (see explanation above)

 - return something to indicate that a newline (or how many) was printed
   during a call to pp - that way you can, for example, assure that 
   after a multi-line lambda you start the args on a new line
   [Here's an interesting application for complex numbers!]

 - we now have printsize connected to printmacros, but not pp-width ...

 - current pp (due to order of first 2 cond clauses) sometimes prints
   a multiline car (lambda expression), then puts the cdr on the same line 
   where the car ends.  Maybe this is all right.  Or maybe we should first
   check for nonatomic car that takes more than one line...

 - when you get near the right margin and have a long way to go,
   start over at the left.  In this case perhaps we should detect
   that the width is so large that misering won't help.
|#
