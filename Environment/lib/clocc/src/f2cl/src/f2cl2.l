; f2cl2.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 92-95 - all rights reserved;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; functions
;	id-expression 
;	parse
;	symbol-listp
;	id-term
;	id-factor 
;       id-factor1
;------------------------------------------------------------------------------
(in-package :f2cl)
(eval-when (compile load eval)
  (proclaim '(optimize (speed 3) (safety 1)))

  (proclaim '(special *intrinsic-function-names* *external-function-names*
	      *subprog_name* *subprog-stmt-fns*
	      *functions-used*)))
;-----------------------------------------------------------------------------
; subst-splice substitutes the list b for the atom a in the list c via splicing
(defun subst-splice (b a c) 
  (cond ((atom c) c)  
        ((equal (car c) a) (append b (subst-splice b a (cdr c))))
        (t (cons (subst-splice b a (car c)) (subst-splice b a (cdr c))))))
;-----------------------------------------------------------------------------

(defun extract (alpha beta)
  (prog (retlist)
    (setq retlist nil)
    loop
    (cond ((null beta) (return retlist))
          ((member (car beta) alpha)
           (setq retlist (append1 retlist (car beta))
                 beta (cdr beta))
           (go loop))
          (t (setq beta (cdr beta))
           (go loop)))))

(defun list-itp (lis signs)
 (cond 
      ((member nil lis)
       (princ-reset '|f2cl error: failure to parse an expression|)))
  (prog (ret)
    (cond ((equal signs nil) (return (car lis))))
    (setq signs (subst 'f2cl/ '/ signs))
    (setq ret (list (car signs) (car lis) (cadr lis)))
    (setq lis (cdr lis))
    loop
    (setq lis (cdr lis))
    (setq signs (cdr signs))
    (cond ((null (car signs)) (return ret))
          (t (setq ret (list (car signs) ret (car lis)))
             (go loop)))))

(defun tpl-subst (x y lis)
   (cond 
         ((null lis) nil)
         ((equal y (car lis))
           `(,x ,@(tpl-subst x y (cdr lis))))
         (t `(,(car lis) ,@(tpl-subst x y (cdr lis))))))

(defun tpl-subpair (lx ly lis)
   (prog (elx ely restx resty ret)
    (setq elx (car lx) ely (car ly) restx (cdr lx) resty (cdr ly) ret lis)
    loop
    (setq ret (tpl-subst elx ely ret))
    (cond ((null restx) (return ret)))
    (setq elx (car restx) ely (car resty) restx (cdr restx) resty (cdr resty))
    (go loop)))


;----------------------------------------------------------------------------- 
(defun symbol-listp (x)
  (and (listp x)
       (symbolp (car x))
       (null (cdr x))))
;------------------------------------------------------------------------------
(defun check-tags-go (x)
  (prog (tags gos)
   (setq tags (mapcar #'car (delete nil (mapcar #'(lambda (y) (cond ((symbol-listp y) y)
                                                     (t nil))) x))))
   (setq gos (mapcar #'car (find-gos x)))

   (return
    (cond ((eq (length tags) (length (unique-elements (append gos tags)))) t)
          (t nil))))) 
;------------------------------------------------------------------------------
(defun find-gos (x)
 (cond ((atom x) nil)
       ((eq (car x) 'go) 
        (cond ((null (symbol-listp (cadr x))) 
               (princ-reset
'|f2cl error: the argument to a program "go" must be a valid tag.
Tag being parsed:| (cadr x))))
        (list (cadr x)))
       (t (append (find-gos (car x)) (find-gos (cdr x))))))
    
;----------------------------------------------------------------------------- 

(defun id-expression (larg)
  (prog (x y l)
     (setq l (subst-splice '(+ -1 *) '- larg))
     (cond ((eq (car l) '+) (setq l (cdr l)))
           ((eq (car l) '-) (setq l (cons 0 l))))

     ;; FIXME: This probably needs a rethink.  We need to parse
     ;; logical operations better and I don't think this will work in
     ;; all cases.
     
     ;; This is a hack.  Let's try to identify some logical
     ;; operations.  If we find them we rewrite the expression with
     ;; parentheses and parse it normally.  Thus, x .le. y*z is
     ;; written as (x) .le. (y*z).
     (dolist (log-op '(or and <= < >= > equal ><))
       (let ((split (list-split-multi log-op l)))
	 ;;(format t "op = ~A, split = ~A~%" log-op split)
	 (when (cdr split)
	   ;; The expression was split into parts for the given
	   ;; logical op.  Rewrite the expression with appropropriate
	   ;; parens.
	   (let ((rewrite nil))
	     (dolist (s split)
	       (push s rewrite)
	       (push log-op rewrite))
	     ;; The first element of rewrite is the operator.  Need to
	     ;; remove it.
	     (setf l (nreverse (cdr rewrite)))))))
	       
     (setq x (list-split-multi '+ l))
     (setq x (list x (extract '(+ -) l)))
     (setq y (list-itp (mapcar #'id-term (car x)) 
		       (cadr x)))
     (return y)))

;-----------------------------------------------------------------------------

(defun id-term (l)
  (prog (x)
     (setq x (apply #'append (mapcar #'(lambda (x)
					 (list-split-bin '/ x))
				     (list-split-multi '* l))))
     (setq x (list x (extract '(* /) l)))
     (if (and (equal (caar x) '(-1)) (equal (cadr x) '(*)))
         (return `(- ,(id-factor1 (cadar x)))))
     (return (list-itp (mapcar #'id-factor1 (car x)) 
		       (cadr x)))))

;-----------------------------------------------------------------------------
(defun id-factor1 (l)
  (prog (x)
     (setq x (list-split-multi-string '(^ f2cl-//) l))
     (setq x (list x (extract '(^ f2cl-//) l)))
     (return (list-itp (mapcar #'id-factor (car x)) 
		       (subst 'expt  '^ (cadr x))))))

;------------------------------------------------------------------------------
(defun lookup-array-bounds (v)
  ;; Lookup the variable in the explicitly declared variables list.
  (map nil #'(lambda (e)
	       (let ((res (find v (rest e) :key #'car)))
		 (when (and res
			    (not (null (rest res))))
		   ;; If this entry has dimensions, we're done!
		   (return-from lookup-array-bounds
		     ;; Arrays, including character strings, look
		     ;; something like (var (lo-1 hi-1) (lo-2 hi-2)
		     ;; ...)
		     (rest res)
		     #+nil
		     (if (listp (first (second res)))
			 (second res)
			 (rest res))))))
       *explicit_vble_decls*)
  ;; Check to see if the array was declared in a common statement.
  (let ((res (member v *common_array_dims*)))
    (when res
      (return-from lookup-array-bounds
	(second res))))
  (error "Cannot find array bounds for ~S!" v))


;; NAME should be a list.  For functions, it's just a list of the name
;; itself.  For subroutines, it should be a list of the name and the
;; keyword :subroutine.
;;
;; The format of *functions-used* is this:
;;
;; (f1 f2 f3 f4 ...)
;;
;; where f1 is (function-type list-of-args)
;;
;; where function-type is (name [:subroutine])
;;
;; and list-of-args is (arg1 arg2 arg3 ...)
;;
;; where arg1 is (a1 a2 a3 ...), the list of arguments used in the
;; call to the subprogram.
;;
;; For example,
;;
;; (((DLOG) (((DABS (F2CL/ (+ 1.0d0 (- CC)) (+ 1.0d0 CC))))))
;;  ((DQCHEB :SUBROUTINE) ((X) (FVAL) (CHEB12) (CHEB24)))
;;  ((F)
;;   (((+ CENTR (- U))) ((+ U CENTR)) ((+ CENTR (- HLGTH))) (CENTR)
;;    ((+ HLGTH CENTR))))
;;  ((DQK15W :SUBROUTINE)
;;   ((F) (DQWGTC) (C) (P2) (P3) (P4) (KP) (A) (B) (RESULT) (ABSERR)
;;    (RESABS) (RESASC)))
;;  ((DABS)
;;   (((+ RES24 (- RES12))) ((F2CL/ (+ 1.0d0 (- CC)) (+ 1.0d0 CC)))
;;    ((F2CL/ (+ 1.0d0 (- CC)) (+ 1.0d0 CC))) (CC))))
;;
;; This means DLOG is a function and was called once with the
;; arguments (DABS (F2CL/ (+ 1.0d0 (- CC)) (+ 1.0d0 CC))).
;;
;; DQCHEB is a subroutine called once with the arguments ((X) (FVAL) (CHEB12) (CHEB24)).
;;
;; F is a function and was called 5 different times with arguments:
;; 1. (+ CENTR (- U))
;; 2. (+ U CENTR)
;; 3. (+ CENTR (- HLGTH))
;; 4. CENTR
;; 5. (+ HLGTH CENTR)
(defun update-called-functions-list (name args)
  (let ((fcn (find (first name) *functions-used* :key #'caar)))
    (cond (fcn
	   ;; Found it.  Let's add the arguments to the arg list
	   (push args (second fcn)))
	  (t
	   ;; A new function.  Let's add it and the args to the list.
	   (push (list name (list args)) *functions-used*)))
    *functions-used*))

(defun id-factor (l)
  (cond 
    ;;         ((null l) nil)

    ;; number:
    ((numberp l) l)
    ;; string:
    ((stringp l) l)
    ;; x.xE%x (scientific notation) number - % is used to represent a minus sign:
    ((and (symbolp l)
	  (let ((suffix (string-trim '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.) 
				     (string l))))
	    (or (string-equal suffix "E%") (string-equal suffix "e%"))))
     (setq l
	   (read-from-string
	    (concatenate 'string
			 (string-right-trim 
			  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\%) l)
			 "-"
			 (string-left-trim '(#\%)
					   (string-left-trim 
					    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\E #\e) l)))))
     l)
    ;; x.xD%x, like above
    ((and (symbolp l)
	  (let ((suffix (string-trim '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.) 
				     (string l))))
	    (or (string-equal suffix "D%") (string-equal suffix "d%"))))
     (setq l
	   (read-from-string
	    (concatenate 'string
			 (string-right-trim 
			  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\%) l)
			 "-"
			 (string-left-trim '(#\%)
					   (string-left-trim 
					    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\D #\d) l)))))
     l)
    
					; symbol:
    ((symbolp l)
     (check-reserved-lisp-names l))
     
					;number: 
    ((and (null (cdr l)) (numberp (car l)))
     (car l))


					; bracketed symbol or string:
    ((and (null (cdr l)) (or (symbolp (car l)) (stringp (car l))))
     (id-factor (car l)))

					; function of no variables:
    ((and (symbolp (car l)) (eq nil (cadr l)))
     (update-called-functions-list (list (car l)) nil)
     (list (car l)))

					; boolean expression
    ((intersection l '(not equal and or < > <= >= ><))
     (id-logical l))

					; intrinsic function call:
    ((and (symbolp (car l)) 
	  (listp (cadr l))
	  (member (car l) *intrinsic-function-names*))
     (cond ((not (null (cddr l)))
	    (princ-reset
	     '|f2cl error: missing +, *, /, or ^ operator following a function call.| l)))
     (update-called-functions-list (list (car l))
				   (mapcar #'id-expression (list-split '|,| (cadr l))))
     (cons (case (car l)
	     ;; Handle some special cases for intrinsic functions
	     ;; because they conflict with the Lisp functions of the
	     ;; same name.
	     (char 'fchar)
	     (sqrt 'fsqrt)
	     (log 'flog)
	     (float 'ffloat)
	     (real 'freal)
	     (t (car l)))
	   (mapcar #'id-expression
		   (list-split '|,| (cadr l)))))
    ;; array reference:
    ((and (symbolp (car l))
	  (listp (cadr l))
	  (vble-is-array-p (check-reserved-lisp-names (car l))))
     (when (cddr l)
       (princ-reset
	'|f2cl error: missing +, *, /, or ^ operator following a function call.| l))


     ;; Given the array name and indices, look up the the
     ;; bounds for this array and construct the appropriate
     ;; expression to access the array correctly.
     (let* ((vname (check-reserved-lisp-names (car l)))
	    (indices (mapcar #'id-expression
			     (list-split '|,| (cadr l))))
	    (bounds (lookup-array-bounds vname)))

       ;; Decide if we want to access the specified element or return a
       ;; slice of the array.  (Used only in calls to functions and
       ;; subroutines.
       (cond (*apply-array-slice*
	      ;;(format t "array   = ~A~%" vname)
	      ;;(format t "indices = ~A~%" indices)
	      ;;(format t "bounds  = ~A~%" bounds)
	      `(array-slice ,vname ,(lookup-vble-type vname) ,indices ,bounds))
	     (t
	      `(fref ,vname ,indices ,bounds)
	      ))))
    ;; string (character array) reference 
    ((and (symbolp (car l))
	  (listp (cadr l))
	  (subtypep (lookup-vble-type (car l)) 'string))
     (let* ((vname (check-reserved-lisp-names (car l)))
	    (indices (mapcar #'id-expression
			     (list-split '|:| (cadr l)))))

     `(fref-string ,vname ,indices)))
    
    ;; function call:
    ((and (symbolp (car l))
	  (listp (cadr l)))
     ;; Should we check for T and PI here?
     (let ((fname (car l))
	   (args (mapcar #'id-expression (list-split '|,| (cadr l)))))
       
       ;; Save the function name and number of arguments so we can
       ;; declare the function appropriately.  We also save the
       ;; argument list so we can determine later the types of each
       ;; parameter, if we really want to.
       (update-called-functions-list (list fname) args)

       ;; Now convert array references to an array slice, as
       ;; appropriate.
       (setf args (maybe-convert-array-ref-to-slice args fname))

       ;; Functions can modify the arguments in addition to returning
       ;; a value.  We need to handle this like a subroutine call, as
       ;; done in PARSE-SUBROUTINE-CALL.  If this function call is on
       ;; the LHS of an assignment, it's not really a function call
       ;; but the definition of a statement function.  Return it as
       ;; is.  If it's on the RHS, it's a real function call so call
       ;; it correctly.
       (if (or *parsing-lhs*
	       (member fname *subprog-stmt-fns*))
	   `(,fname ,@args)
	   (first (generate-call-to-routine
		   (cond ((member fname *subprog-arglist*)
			  ;; We want to use funcall only if the external function
			  ;; was passed in as a parameter.  If not, then we don't
			  ;; need to funcall it.  The user was just telling us that
			  ;; it was external function instead an intrinsic.
			  `(funcall ,fname)) 
			 (t 
			  `(,fname)))
		   args t)))
       ))

					; expression:
    ((and (listp (car l))
	  (null (cdr l))
	  (not (member '|,| (car l))))
     (id-expression (car l)))

					; complex number:
    #+nil
    ((and (eq (length l) 3) (eq (second l) '|,|))
     (list 'complex (id-factor (first l)) (id-factor (third l))))
    ((and (listp (car l))
	  (null (cdr l))
	  (member '|,| (car l)))
     ;; Not sure this is the test for a complex number, but a Fortran
     ;; complex number looks like (<re>, <im>) where <re> and <im> are
     ;; numbers.  We don't check for that here.
     (let ((split (list-split '|,| (car l))))
       (list 'cmplx
	     (id-expression (list (first split)))
	     (id-expression (list (second split))))))
					; error dropping out the bottom:
    (t 
     (princ-reset '|f2cl syntax error| l))
    ))

#||
;-------------------------------------------------------------------------------

; (a0 |,| a1 |,| a2 ..... |,| an) is checked:
(defun comma-sep-symb-listp (l)
 (prog (rest)
  (cond
   ((and 
         (listp l) 
         (neq '|,| (car l))
         (every #'symbolp l) 
         (evenp (length (cdr l)))))
   (t (return nil)))
  (setq rest (cdr l))
  loop
  (cond ((null rest) (return t)))
  (cond ((or (neq (car rest) '|,|)
             (eq (cadr rest) '|,|))
          (princ-reset
'|f2cl syntax error: failure to parse the parameter list of a function 
definition.| l) ))
  (setq rest (cddr rest))
  (go loop)))

;------------------------------------------------------------------------------

; (a0 |,| a1 |,| a2 ..... |,| an) is checked where a0 are anything :
(defun comma-sep-listp (l)
 (prog (rest)
  (cond
   ((and 
         (listp l) 
         (neq (car l) '|,|)
         (evenp (length (cdr l)))))
   (t (return nil)))
  (setq rest (cdr l))
  loop
  (cond ((null rest) (return t)))
  (cond ((or (neq (car rest) '|,|)
             (eq (cadr rest) '|,|))
          (princ-reset 
'|f2cl syntax error: failure to parse a list.| l) ))
  (setq rest (cddr rest))
  (go loop)))
||#
;;;-----------------------------------------------------------------------------
;;; end of f2cl2.l
;;;
;;; $Id: f2cl2.l,v 1.35 2006/04/28 00:35:29 rtoy Exp $
;;; $Log: f2cl2.l,v $
;;; Revision 1.35  2006/04/28 00:35:29  rtoy
;;; Modify how expressions and Fortran complex numbers are recognized.  An
;;; expression should not contain a comma, and a Fortran complex number
;;; must have a comma.
;;;
;;; Not sure this is correct for an expression, but a complex number must
;;; have a comma.
;;;
;;; Revision 1.34  2004/08/14 13:45:34  rtoy
;;; Handle OR and AND expressions.  (Maybe.  This probably needs a lot
;;; more work.)
;;;
;;; Revision 1.33  2004/08/11 16:51:32  rtoy
;;; A possible fix for the parsing bug wherein x .le. y*z was parsed as (x
;;; .le. y) * z.
;;;
;;; We look for logical operations and rewrite the expression with
;;; parentheses around the left and right parts of the operation.  Thus x
;;; .le. y*z becomes (x) .le. (y*z), which will be parsed correctly by the
;;; rest of the routines.
;;;
;;; Revision 1.32  2003/11/13 22:16:59  rtoy
;;; Try to be smarter about generating args to functions, which is an
;;; issue if the arg is an element of an array.  If we know the declared
;;; types of the function, try to generate the appropriate arg, meaning
;;; either a single element of the array or a slice of the array.
;;;
;;; Revision 1.31  2002/09/13 17:50:19  rtoy
;;; From Douglas Crosher:
;;;
;;; o Make this work with lower-case Lisps
;;; o Fix a few typos
;;; o Make a safer fortran reader.
;;;
;;; Revision 1.30  2002/01/06 23:28:16  rtoy
;;; Missed a few renamings of *intrinsic_function_names* and
;;; *external_function_names*.
;;;
;;; Revision 1.29  2002/01/06 23:10:12  rtoy
;;; Rename *intrinsic_function_names*, *external_function_names* and
;;; *subprog_stmt_fns* to use dashes.
;;;
;;; Revision 1.28  2000/09/01 14:05:25  rtoy
;;; o ID-FACTOR: we were not careful enough about array slicing in
;;;   function calls.  We should only apply array slicing if the argument
;;;   is an array ref.  Any arithmetic on the arg should disable
;;;   array-slicing.
;;;
;;; Revision 1.27  2000/08/29 03:20:25  rtoy
;;; Array slicing needs to handled in not only subroutine calls but
;;; function calls too.  Make it so.
;;;
;;; Revision 1.26  2000/08/05 19:20:01  rtoy
;;; o If we're parsing the LHS of an assignment, and it looks like a
;;;   function call, we don't want to mangle it because it's not a
;;;   function call but the definition of a statement function.
;;; o If the function is a call to statement function, we don't need to
;;;   mangle it because a statement function can't modify its parameters.
;;;
;;; Revision 1.25  2000/07/31 04:21:28  rtoy
;;; Remove some unused functions.
;;;
;;; Revision 1.24  2000/07/30 05:56:26  rtoy
;;; Function calls need to be handled like subroutine calls because
;;; functions can modify their parameters.
;;;
;;; Revision 1.23  2000/07/28 17:01:28  rtoy
;;; o We are in the f2cl package now.
;;; o The Fortran string concatenation operator, //, is now f2cl-//.
;;;   (Prevents conflicts with the lisp variable //.)  Handle
;;;   appropriately.
;;; o Change the name of the Fortran routine from REAL to FREAL.  Prevents
;;;   conflicts with the lisp type REAL.
;;;
;;; Revision 1.22  2000/07/27 16:39:17  rtoy
;;; We want to be in the CL-USER package, not the USER package.
;;;
;;; Revision 1.21  2000/07/19 13:50:11  rtoy
;;; o Use the macro ARRAY-SLICE to slice the array instead of inline as we
;;;   were doing it.
;;; o All arrays are 1-D now to support proper Fortran array slicing, so
;;;   FREF needs to know the array dimensions.
;;;
;;; Revision 1.20  2000/07/18 14:04:33  rtoy
;;; Changes for UPDATE-CALLED-FUNCTIONS-LIST:
;;;
;;; o Document the layout of *functions-list*
;;; o Make the appropriate code changes in other parts since the name arg
;;;   is now either a list of the name for functions or a list of the name
;;;   and :subroutine for subroutines.  (Used for declaring functions and
;;;   subroutines correctly.)
;;;
;;; Revision 1.19  2000/07/14 15:48:34  rtoy
;;; Preliminary support for array slicing.  Any array reference in a
;;; function or subroutine call is assumed to mean a slice of the array.
;;; This is done by creating a displaced array to the original array.
;;;
;;; Revision 1.18  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
