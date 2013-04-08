; -*- Mode: lisp; package: F2CL -*-
; f2cl5.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;functions:
; declaration insertion
;      check_new_vbles
;      insert-declarations
;      make-initialisation
;      make-declaration
;      make-char-decl
;      make-char-init
;      get-implicit-type
;      get_array_type
;      default-int-p
;      vble-is-array-p
;      sym-is-fun-name
;      make-special-proclamation
;      make-special-var-decl
;      check-reserved-lisp-names
; structured stmt fix
;      fix-structure
;      fix-ifthen
;      end-do-p
;      label-matches-dolabel-p
;      fix-do
;      fix-tagbodies
;      remove-prefix 
;      multiple-do-labelp
; FORMAT parsing
;      parse-format
;      parse-format1
;      parse-write
;      parse-format-descriptor-list
;      fixnum-string
;      parse-format-descriptor-I
;      parse-format-descriptor-F
;      parse-format-descriptor-E
;      parse-format-descriptor-G
;      parse-format-descriptor-/
;      parse-format-descriptor-TR
;      parse-format-descriptor-X
;      parse-format-descriptor-S
;      parse-default-format-descriptor
;      fix-slashes

;;-----------------------------------------------------------------------------
(in-package :f2cl)

;; functions for setting up varaible declarations and initialisations
(eval-when (compile load eval)
  (proclaim '(special *intrinsic-function-names* *external-function-names*
	      *declared_vbles* *undeclared_vbles* *key_params* *save_vbles*
	      *explicit_vble_decls* *implicit_vble_decls* *common_array_dims*
	      *subprog_common_vars* *program-flag* 
	      *subprog-stmt-fns* *subprog_stmt_fns_bodies*
	      *auto-save-data*
	      *functions-used*
	      *vble-declaration-done*))
)

(defvar *fortran-keywords*
  '(assign if then go to goto call continue do fdo else elseif return
    stop print read write backspace close endfile inquire
    open rewind format and or not))



(defun check_new_vbles (exprs)
  (cond ((or (null exprs)
	     (numberp exprs)
	     (typep exprs 'string))
	 nil)
	((symbolp exprs)
	 (cond ((or (member exprs '(\, \' + - * / ** // ^ = equal > < >= <= >< |:|
				    f2cl-// cmplx fchar
				    fref fref-string f2cl/
				    flog fsqrt ffloat freal
				    expt
				    %false% %true%
				    funcall))
		    (member exprs *fortran-keywords*)
		    (member exprs *key_params* :key #'car)
		    (member exprs *external-function-names*)
		    (member exprs *declared_vbles*)
		    (member exprs *undeclared_vbles*)
		    (sym-is-fun-name (list exprs))
		    (sym-is-number-p exprs))
		nil)
	       ((eq exprs 'pause)
		)
	       (t
		(pushnew (check-reserved-lisp-names exprs) *undeclared_vbles*))))
	((listp exprs)
	 (cond ((eq (first exprs) 'multiple-value-bind)
		;; Function calls get turned into a MULTIPLE-VALUE-BIND, so
		;; the only possible place for variables is the arg list for
		;; the function.
		(mapc #'check_new_vbles (cddr (third exprs))))
	       ((eq (first exprs) 'array-slice)
		;; The only place for variables is the array name or
		;; the indices
		(check_new_vbles (second exprs))
		(mapc #'check_new_vbles (cdddr exprs)))
	       (t
		(mapc #'check_new_vbles exprs))))
	(t
	 (error "What happened?")))
  exprs)

;; This takes a list of individual Lisp declarations and combines them
;; into one based on the declared type.  Thus
;;
;;  (declare (type fixnum x))
;;  (declare (type fixnum y))
;;  (declare (type single-float z))
;;
;; becomes
;;
;;  (declare (type fixnum x y)
;;           (type single-float z))
(defun pretty-decls (decls)
  (let ((type-hash (make-hash-table :test 'equal)))
    (dolist (decl decls)
      (destructuring-bind (dcl (type v-type var))
	  decl
	(declare (ignore dcl type))
	(let ((val (gethash v-type type-hash)))
	  (setf (gethash v-type type-hash)
		(push var val)))))
    (let ((dec '()))
      (maphash #'(lambda (key val)
		   (push `(type ,key ,@val) dec))
	       type-hash)
      `((declare ,@dec)))))

(defun lookup-vble-type (vble &optional (decls *explicit_vble_decls*))
  ;;(format t "lookup-vble: ~a~%" vble)
  (cond ((or (member vble *declared_vbles*)
	     (member vble *subprog-arglist*))
	 ;;(format t "  is declared var~%")
	 ;;(format t "  explicit_vbld_decls = ~A~%" *explicit_vble_decls*)
	 
	 ;; First for declared variables
	 (do* ((type-clauses decls
			     (rest type-clauses))
	       (type (member vble (cdar type-clauses) :key #'car)
		     (member vble (cdar type-clauses) :key #'car)))
	      ((or type
		   (null type-clauses))
	       (let ((v-type (caar type-clauses)))
		 (cond ((eq v-type 'array)
			;; Hmm, this is probably an array declared
			;; with a dimension statement, and either the
			;; type is declared later or implicit typing
			;; is used.
			;;
			;; Try to lookup the type again but skip over
			;; array types.  If this works, the array type
			;; was explicitly given so use it. Otherwise,
			;; use Fortran typing rules.
			(cond ((lookup-vble-type vble (remove 'array decls :key #'first)))
			      ((get-implicit-type vble))
			      ((default-int-p vble)
			       'integer4)
			      (t 'single-float)))
		       ((and (listp v-type)
			     (eq (first v-type) 'character))
			;; Hmm, a Fortran character string.  Make it a Lisp string
			(if (numberp (second v-type))
			    `(string ,(second v-type))
			    'string))
		       (t
			;; If type is NIL, use implicit type
			(if (null v-type)
			    (cond ((get-implicit-type vble))
				  ((default-int-p vble)
				   'integer4)
				  (t
				   'single-float))
			    v-type))))
	       ;;(format t  "type-clause = ~S~%" type-clauses)
	       )))
	(t
	 ;;(format t "  is not declared. Implicit type = ~a~%" (get-implicit-type vble))
	 ;;(format t "implicit ~a~%" *implicit_vble_decls*)
	 (cond ((get-implicit-type vble))
	       ((default-int-p vble)
		'integer4)
	       (t
		'single-float)))))

;; A simple implementation of Fortran contagion.
;;
;; Coerce 0 to the types T1 and T2 and add the result.  The type of
;; the sum is the desired type.  However, we convert some of the types
;; to ones that f2cl understands.
(defun fortran-contagion (t1 t2)
  (let ((sum (+ (coerce 0 t1) (coerce 0 t2))))
    (typecase sum
      ((complex double-float)
       'complex16)
      ((complex single-float)
       'complex8)
      (integer
       'integer4)
      (t
       (type-of sum)))))

(defun get-fun-arg-type (arg)
  (cond ((symbolp arg)
	 (case arg
	   ((%false% %true%)
	    'logical)
	   (t
	    ;; Lookup the variable type and return it.
	    (let ((var-type (lookup-vble-type arg)))
	      (cond ((eq var-type 'array)
		     ;; Look up the type of the array
		     (destructuring-bind (&optional decl1 decl2)
			 (vble-declared-twice-p arg *explicit_vble_decls*)
		       (declare (ignorable decl2))
		       (values (first decl1) t)))
		    ((vble-is-array-p arg)
		     (values var-type t))
		    (t
		     var-type)))
	    )))
	((listp arg)
	 ;; We have an expression of some type
	 (let ((fun (first arg)))
	   (cond ((eq 'make-array fun)
		  ;; Some array slicing here.
		  (values (second (second (member :element-type arg))) t))
		 ((eq 'array-slice fun)
		  (values (get-fun-arg-type (second arg)) t))
		 ((eq 'funcall fun)
		  ;; Look up function type
		  (get-fun-arg-type (second arg)))
		 ((eq 'fref fun)
		  ;; Look up array references.
		  (let ((result (get-fun-arg-type (second arg))))
		    (values result nil)))
		 ((member fun '(+ -))
		  ;; Basic arithmetic operations that might be unary
		  ;; operators.
		  (if (third arg)
		      (fortran-contagion (get-fun-arg-type (second arg))
					 (get-fun-arg-type (third arg)))
		      (get-fun-arg-type (second arg))))
		 ((member fun '(* f2cl/ expt))
		  ;; Basic arithmetic operations.
		  (fortran-contagion (get-fun-arg-type (second arg))
				     (get-fun-arg-type (third arg))))
		 ((member fun '(conjg))
		  ;; Complex conjugate
		  (fortran-contagion (get-fun-arg-type (second arg))
				     'complex8))
		 ((eq fun 'abs)
		  ;; Absolute value
		  (let ((arg-type (get-fun-arg-type (second arg))))
		    (case arg-type
		      (complex8
		       'single-float)
		      (complex16
		       'double-float)
		      (otherwise
		       arg-type))))
		 ((member fun '(cmplx))
		  ;; Complex.  Figure out the type of complex we
		  ;; should return.
		  (fortran-contagion
		   (fortran-contagion (get-fun-arg-type (second arg))
				      (get-fun-arg-type (third arg)))
		   'complex8))
		 ((member fun '(dcmplx))
		  ;; double complex.  Always returns a complex16
		  'complex16)
		 ((member fun '(sin cos tan
				asin acos atan atan2
				sinh cosh tanh
				exp flog alog10 log10
				fsqrt aint sign dim max min))
		  ;; Generic functions.  These generics always return
		  ;; a number of the same type as its args.  Some
		  ;; functions take more than one arg, but Fortran
		  ;; says they are supposed to be the same type, so we
		  ;; only need to look at the first.  Also, some of
		  ;; them only take real-type arguments.  We don't
		  ;; check for that because Fortran says you're
		  ;; supposed to make sure they are anyway.
		  (get-fun-arg-type (second arg)))
		 ((member fun '(dsin dcos dtan
				dasin dacos datan datan2
				dexp dlog dlog10
				dabs dmax1 dmin1
				dble dimag
				dsqrt))
		  ;; Double-precision functions
		  'double-float)
		 ((member fun '(csin ccos ctan
				casin cacos catan catan2
				cexp clog clog10
				cabs 
				csqrt))
		  ;; Complex functions
		  'complex8)
		 ((member fun '(zsin zcos ztan
				zasin zacos zatan zatan2
				zexp zlog zlog10
				zabs 
				zsqrt))
		  ;; Complex functions
		  'complex16)
		 ((member fun '(and or not > >= equal <= < logeqv logxor))
		  ;; Logical operations
		  'logical)
		 ((eq fun 'multiple-value-bind)
		  (get-fun-arg-type (third arg)))
		 (t
		  (get-fun-arg-type fun)))))
	(t
	 ;; If we have a fixnum, return integer4 instead.
	 (if (typep arg 'integer)
	     'integer4
	     (type-of arg)))))

(defun get-upgraded-fun-arg-type (args)
  (let ((res '()))
    ;;(format t "args = ~a~%" args)
    (dolist (arg args
	     (nreverse res))
      ;; Handle the simple cases, and then Run down the list and
      ;; lookup the argument types of each.
      (cond ((numberp arg)
	     (push (type-of arg) res))
	    ((symbolp arg)
	     (push (get-fun-arg-type arg) res))
	    ((and (listp arg)
		  (eq 'fref (first arg)))
	     (push (get-fun-arg-type (second arg)) res))
	    ((and (listp arg)
		  (listp (car arg))
		  (eq 'multiple-value-bind (caar arg)))
	     ;; A call to a function.  Look up the type of the
	     ;; function, but be careful.  Sometimes it's (fun args)
	     ;; and sometimes it's (funcall fun args)
	     (let ((f (third (car arg))))
	       (push (get-fun-arg-type (if (eq (first f) 'funcall)
					   (second (third (car arg)))
					   (first f)))
				       res)))
	    (t
	     (let ((arg-types (mapcar #'get-fun-arg-type arg)))
	       (push (cond ((every #'(lambda (x)
				       (eq x (first arg-types)))
				   arg-types)
			    ;; All items are the same, so that's our type.
			    (first arg-types))
			   (t
			    ;; Some items are different, so we can't
			    ;; really say. (Although I think Fortran
			    ;; says they should all have been the
			    ;; same.)
			    `(or ,@arg-types)))
		     res)))))))

;; This needs to be reworked!
(defun get-arg-decl (list-of-arglists)
  (let (res)
    ;; Given a list of arglists, we derive the type for each of the arglists.
    (dolist (arglist list-of-arglists)
      (push (mapcar #'(lambda (arg)
			;; Literal strings (which are of type (string
			;; <len>)) confuse the mapcar below, so change
			;; them into just type 'string.
			(multiple-value-bind (type arrayp)
			    (get-fun-arg-type arg)
			  (cond (arrayp
				 (if (subtypep type 'string)
				     ;; An array of strings
				     (intern (concatenate 'string (symbol-name '#:array-strings)))
				     ;; Some other type of array
				     (intern (concatenate 'string (symbol-name '#:array-) (string type)))))
				((subtypep type 'string)
				 'string)
				((subtypep type 'integer4)
				 'integer4)
				(t
				 type))))
		    arglist)
	    res))
    ;; Now make a list of each of the types of the arguments
    (let ((types
	   (mapcar #'(lambda (z)
		       (if (atom z)
			   z
			   (let ((nodup (remove-duplicates z :test #'equalp)))
			     (if (rest nodup)
				 `(or ,@nodup)
				 (first nodup)))))
		   (reduce #'(lambda (x accum)
			       (mapcar #'(lambda (a b)
					   (if (listp a)
					       (cons b a)
					       (list b a)))
				       x accum))
			   res))))
      ;; Remove an extra set of parens if needed.
      (if (and (= (length types) 1) (listp (first types)))
	  (first types)
	  types))))

;; Create declarations for all the functions used in the subprogram.
;; We return three items: statement functions, other functions, and
;; functions in the parameter list of the subprogram.
(defun make-fcn-decl (fcn-list arglist)
  (let (stmt-fcns other-fcns arg-fcns)
    (dolist (fcn fcn-list)
      (destructuring-bind ((fname &optional ftype) args)
	  fcn
	;;(format t "declaring fcn ~S~%" fcn)
	;;(format t "fname = ~A~%" fname)
	;;(format t "args = ~A~%" args)
	(let* ((type (get-fun-arg-type fcn))
	       ;; If the function is an intrinsic, there is exactly
	       ;; one return value.  If it's not an intrinsic but it
	       ;; is a function, the return values are the function
	       ;; value and the list of arguments.  (This is how f2cl
	       ;; works).  If it's not an intrinsic and is a
	       ;; subroutine (ftype non-NIL), the return type is the
	       ;; list of arguments.  (Should we derive the type of
	       ;; these and put them in the values list?)
	       (decl `(function ,(get-arg-decl args)
		       (values
			,@(if ftype nil (list type))
			,@(if (member fname *intrinsic-function-names*)
			      nil
			      '(&rest t))))))
	  ;;(format t "type = ~A~%" type)
	  ;;(format t "decl = ~A~%" decl)
	  ;;(format t "get-arg-decl = ~A~%" (get-arg-decl args))
	  (cond ((member fname *subprog-stmt-fns*)
		 (push `(declare (ftype ,decl ,fname)) stmt-fcns))
		((member fname arglist)
		 ;; What can we really do for a function that appears
		 ;; in the arglist?  We don't really know anything
		 ;; about it, so don't try to declare the function.
		 
		 ;;(format t "arglist fcn = ~A~%" `(declare (type ,decl ,fname)))
		 #+nil
		 (push `(declare (type ,decl ,fname)) arg-fcns))
		((member fname *intrinsic-function-names*)
		 ;; We don't need declarations for intrinsic functions
		 nil)
		(t
		 (push `(declare (ftype ,decl ,fname)) other-fcns))))))
    (values stmt-fcns other-fcns arg-fcns)))

;; Given the program list P, look through it and try to convert all
;; occurrences of f2cl/ with either / or TRUNCATE.  We use TRUNCATE if
;; both parameters are integers; / if both parameters are float or
;; complex.  If we can't determine the types of the parameters, we
;; leave it alone.
;;
;; A new tree with the (possibly) modified code is returned.
;;
;; This is an attempt to optimize out the calls to the f2cl/ macro.
;; This is for the benefit of Lisp compilers that aren't smart enough
;; to optimize unused parts of the macros out. (We should probably do
;; a similar thing for sqrt, log, log10.)
;;
(defun optimize-f2cl/ (p)
  (cond ((or (atom p) (null p))
	 p)
	((eq (first p) 'f2cl/)
	 ;; Let's try to optimize f2cl/ to either / or truncate
	 ;; appropriately.  If both parameters to f2cl/ are integers,
	 ;; we use truncate; if either parameter is a float or
	 ;; complex, use /.  If we can't determine the type, leave the
	 ;; macro in.

	 (let* ((left (get-fun-arg-type (list (second p))))
		(right (get-fun-arg-type (list (third p))))
		(fun (cond ((and (subtypep left 'integer)
				 (subtypep right 'integer))
			    'truncate)
			   ((or (subtypep left 'number)
				(subtypep right 'number))
			    '/)
			   (t
			    'f2cl/))))
	   `(,fun ,@(mapcar #'optimize-f2cl/ (rest p)))))
	(t
	 `(,(first p) ,@(mapcar #'optimize-f2cl/ (rest p))))))

;; Given the program list P, look through it and try to convert all
;; occurrences of integer arithmetic with the integer arithmetic
;; macros.  This is done so that integer arithmetic doesn't have to
;; check for possible overflow because that behavior is undefined in
;; Fortran anyway.
;;
;; A new tree is returned.
(defun optimize-integer-arithmetic (p)
  (cond ((or (atom p) (null p))
	 p)
	((eq (first p) 'declare)
	 ;; Skip over declarations
	 p)
        ((eq (first p) 'fdo)
         ;; FDO handled specially
         (destructuring-bind (fdo (var1 init step-form)
                                  ((cmp-fun var2 end-form) ret-form)
                                  &rest body)
             p
         `(,fdo (,var1 ,(optimize-integer-arithmetic init)
                      ,(optimize-integer-arithmetic step-form))
               ((,cmp-fun ,var2 ,(optimize-integer-arithmetic end-form))
                ,ret-form)
           ,@(mapcar #'optimize-integer-arithmetic body))))
        ((eq (first p) 'fref)
         ;; FREF handled specially
         (destructuring-bind (fref data-var indices bounds &rest offset)
             p
           `(,fref ,data-var ,(mapcar #'optimize-integer-arithmetic indices)
                             ,(mapcar #'optimize-integer-arithmetic bounds)
                             ,@offset)))
	((member (first p) '(min max))
	 ;; We might need to do something here because CMUCL doesn't
	 ;; always know how to optimize this right.
	 (let* ((int-args-p
		 (every #'(lambda (e)
			    (eq e 'integer4))
			(mapcar #'(lambda (arg)
				    (let ((res (get-fun-arg-type (list arg))))
				      ;;(format t "arg = type ~A: ~A~%" res arg)
				      res))
				(rest p))))
		(opt-args
		 (mapcar #'optimize-integer-arithmetic (rest p))))
	   ;;(format t "min/max opt. int-args-p = ~A~%" int-args-p)
	   ;;(format t "args = ~A~%" (rest p))
	   ;;(format t "opt-args = ~A~%" opt-args)
	   (if int-args-p
	       `(,(first p) ,@(mapcar #'(lambda (x)
					  `(the integer4 ,x))
				      opt-args))
	       `(,(first p) ,@opt-args))))
	((eq (first p) 'truncate)
	 ;; Hmm, make sure truncate returns a integer4
	 `(the integer4 ,p))
	((member (first p) '(+ - *))
	 ;; Basic arithmetic operations.  Division has already been
	 ;; handled by converting / to f2cl/ which is optimized
	 ;; somewhere else.
	 
	 ;;(format t "expr = ~A~%" p)
	 (let ((fun (if (every #'(lambda (e)
				   (eq e 'integer4))
			       (mapcar #'(lambda (arg)
					   (let ((res (get-fun-arg-type arg)))
					     ;;(format t "arg = type ~A: ~A~%" res arg)
					     res))
				       (rest p)))
			(cdr (assoc (first p) '((+ . int-add)
						(- . int-sub)
						(* . int-mul))))
			(first p))))
	   ;;(format t "fun = ~A~%" fun)
	   `(,fun ,@(mapcar #'optimize-integer-arithmetic (rest p)))))
	((listp (first p))
	 `(,(mapcar #'optimize-integer-arithmetic
		     (first p))
	   ,@(mapcar #'optimize-integer-arithmetic (rest p))))
	(t
	 `(,(first p) ,@(mapcar #'optimize-integer-arithmetic (rest p))))))
  
;; Given the program list P, look through it and try to convert all
;; occurrences of external functions with #'<func>.  Do this only if
;; the function is not first element of a list.
;;
;; A new tree with the (possibly) modified code is returned.
;;
(defun fixup-external-function-refs (p externs)
  (cond ((null p)
	 p)
	((atom p)
	 (if (member p externs)
	     `(function ,p)
	     p))
	((eq (first p) 'declare)
	 ;; Skip over declarations
	 p)
	((eq (first p) 'function)
	 ;; Skip over functions that we have already done
	 p)
	((eq (first p) 'funcall)
	 ;; Skip over funcalls!
	 `(,(first p) ,(second p) ,@(mapcar #'(lambda (pp)
						(fixup-external-function-refs pp externs))
					    (cddr p))))
	(t
	 `(,(first p) ,@(mapcar #'(lambda (pp)
				    (fixup-external-function-refs pp externs))
				(rest p))))))

;; In the following functions, we are trying to simplify some of the
;; expressions produced by f2cl.  We should probably use some real
;; pattern matching algorithm instead of these hand-written matchers.
;;
;; Fix up the expression by destructively modifying it. This tries to
;; convert some expressions produced by f2cl into something more
;; readable:

;; f2cl always converts (- X) to (* -1 X).  This function tries to
;; undo that.  A new tree is returned with the result.
(defun fixup-expr-mul (expr)
  (cond ((or (atom expr) (null expr))
	 expr)
	((and (eq (first expr) '*)
	      (numberp (second expr))
	      (= (second expr) -1))
	 ;; Convert (* -1 X) to (- X)
	 `(- ,@(fixup-expr-mul (rest (rest expr)))))
	(t
	 `(,(first expr) ,@(mapcar #'fixup-expr-mul (rest expr))))))

;; We try to handle some conversions that make the resulting
;; expressions a bit easier to read and more natural.  Also, some of
;; the transformations reduce the number of operations needed.  (These
;; may not be needed if the compiler is smart enough.)
;;
;; F2CL                            RESULT
;; (- N)                           -N        (for numeric N)
;; (+ (- Z) X)                     (- X Z)
;; (op (- Z) X)                    (op -Z X) (for number N, and any function F, except +
;; (+ X (- Z))                     (- X Z)
;; (F X (- Z))                     (F X -Z)  (for F /= + and Z a number)
;; (+ X (op (- Z) Y))              (- X (op Z Y))  (for op = * or /)
(defun fixup-expression (expr) 
  (flet ((is-neg (e)
	   ;; Return T if the e looks something like '(- Z)
	   (and (listp e)
		(eq (first e) '-)
		(= (length e) 2))))
    (cond ((or (atom expr) (null expr))
	   expr)
	  ((eq (first expr) 'fref)
	   ;; Fixup the expressions for the index.
	   ;;(format t "expr = ~S: ~S~%" expr (mapcar #'fixup-expression (third expr)))
	   `(fref ,(second expr)
	     ,(mapcar #'fixup-expression (third expr))
	     ,@(if (fourth expr)
		   (list (fourth expr)))))
	  ((eq (first expr) 'fdo)
	   ;; Handle fdo: the fdo macro depends on a certain format
	   ;; for the loop (yuck!  Need to change that!)
	   `(fdo ,(second expr) ,(third expr) ,@(mapcar #'fixup-expression (nthcdr 3 expr))))
	  ((and (eq (first expr) '-)
		(numberp (second expr)))
	   ;; Convert (- N) to just -N
	   (- (second expr)))
	  ((is-neg (second expr))
	   ;; We have (F (- Z) X ...).  If F is '+,
	   ;; make it (- X Z).  If Z is a number, make it (F -Z X ...)
	   (cond ((eq (first expr) '+)
		  ;; (+ (- Z) X ...) => (- X Z)
		  `(- ,@(mapcar #'fixup-expression (rest (rest expr)))
		      ,@(mapcar #'fixup-expression (rest (second expr)))))
		 ((numberp (second (second expr)))
		  `(,(first expr) ,(- (second (second expr)))
		    ,@(mapcar #'fixup-expression (rest (rest expr)))))
		 (t
		  `(,@(mapcar #'fixup-expression expr)))))
	  ((is-neg (third expr))
	   ;; We have (F X (- Z) ...).  If F is '+, convert to (- X Z).
	   ;; If not, but Z is a number, convert to (F X -Z ...).
	   (cond ((eq '+ (first expr))
		  ;; We have '(+ X (- Z)).  Make that '(- X Z)
		  `(- ,@(mapcar #'fixup-expression (list (second expr)))
		      ,@(mapcar #'fixup-expression (list (second (third expr))))))
		 ((numberp (second (third expr)))
		  ;; We have (F X (- N) ...).  Make that (F X -N ...)
		  `(,(first expr) ,@(mapcar #'fixup-expression (list (second expr)))
		                  ,(- (second (third expr)))
		                  ,@(mapcar #'fixup-expression (nthcdr 3 expr))))
		 (t
		  ;; Process the X and (- Z) parts now
		  ;;(format t "T case (F X (- Z) ...): ~S~%" expr)
		  `(,(first expr) ,(fixup-expression (second expr))
		                  ,@(mapcar #'fixup-expression (rest (rest expr)))))))
	  ((and (eq (first expr) '+)
		(listp (third expr))
		(member (first (third expr)) '(* /))
		(listp (second (third expr)))
		(eq (first (second (third expr))) '-))
	   ;; We have (+ X (op (- Z) Y)).  Convert to (- X (op Z Y)), for op = * or /.

	   ;;(format t "got ~S~%" expr)
	   `(- ,(fixup-expression (second expr))
	     (,(first (third expr))
	      ,@(mapcar #'fixup-expression (rest (second (third expr))))
	      ,@(mapcar #'fixup-expression (rest (rest (third expr)))))))
	  (t
	   ;; Didn't match, so descend into the remaining args
	   `(,(first expr) ,@(mapcar #'fixup-expression (rest expr)))))))

(defun fixup-relop (expr)
  (cond ((or (null expr)
	     (atom expr)
	     (typep expr 'string))
	 expr)
	((member (first expr) '(|>=| |<=| equal |/=| < > =))
	 ;; If one arguments is a string, replace operation with the
	 ;; appropriate string operation.
	 (let ((lhs (get-fun-arg-type (list (second expr))))
	       (rhs (get-fun-arg-type (list (third expr)))))
	   (cond ((or (subtypep lhs 'string)
		      (subtypep rhs 'string))
		  (let ((op (intern (concatenate 'string
						 (symbol-name '#:fstring-)
						 (symbol-name (first expr))))))
		    `(,op ,(second expr) ,(third expr))))
		 (t
		  expr))))
	(t
	 `(,@(mapcar #'fixup-relop expr)))))

(defun merge-ops (expr)
  (cond ((or (null expr)
	     (atom expr)
	     (typep expr 'string))
	 expr)
	((and (third expr)
	      (member (first expr) '(+ * - /)))
	 ;; Try to merge a bunch of +, *, -, / operations into one.
	 (destructuring-bind (op next &rest args)
	     expr
	   ;;(setf args (list args))
	   (when (and (listp next) (third next))
	     (loop while (and (listp next) (eq op (first next)))
	       do
	       (push (third next) args)
	       (setf next (second next))))
	   `(,op ,(merge-ops next) ,@(merge-ops args))))
	(t
	 `(,@(mapcar #'merge-ops expr)))))
	 
 
(defun create-sym-macros ()
  (let ((sym-mlets '())
	(sym-lets ()))
    (if *common-blocks-as-arrays*
	(let ((common-var-decls (mapcar #'make-special-var-decl *subprog_common_vars*)))
	  (setf common-var-decls (append '(declaim)
					 (mapcar #'(lambda (decl)
						     (second decl))
						 common-var-decls)))
	  (multiple-value-setq (sym-mlets sym-lets)
	    (create-sym-macros-array common-var-decls)))
	(maphash
	 #'(lambda (key varlist)
	     (mapc #'(lambda (var)
		       (let ((accessor-name
			      (intern (concatenate 'string
						   (symbol-name key)
						   "-"
						   (symbol-name var))))
			     (name
			      (intern (concatenate 'string
						   "*"
						   (symbol-name key)
						   (symbol-name '#:-common-block*)))))
			 (if (vble-is-array-p var)
			     (progn
			       (push `(,accessor-name (,accessor-name ,name)) sym-lets)
			       (push `(,var ,accessor-name) sym-mlets))
			     (push `(,var (,accessor-name ,name)) sym-mlets))
			 ))
		   varlist))
	 *common-blocks*))
    (values sym-mlets sym-lets)))

(defun create-sym-macros-array (common_var_decls)
  (let ((sym-mlets '())
	(sym-lets ()))
    (labels
	((create (key v var-type part posn len arrayp)
	   #+nil
	   (format t "var = ~A :type ~A Part ~A posn ~A~%"
		   v var-type part posn)
	   (let ((accessor-name
		  (intern (format nil "~A-~A" key v)))
		 (name
		  (intern (format nil "*~A-COMMON-BLOCK*" key)))
		 (part-name (intern (format nil "~A-PART-~D" key part))))
	     (if arrayp
		 (progn
		   (push `(,accessor-name
			   (make-array ,len :element-type ',(second var-type)
				       :displaced-to (,part-name ,name)
				       :displaced-index-offset ,posn))
			 sym-lets)
		   (push `(,v ,accessor-name) sym-mlets))
		 (push `(,v (aref (,part-name ,name) ,posn))
		       sym-mlets))))
	 (process-block (key varlist)
	   (let ((part 0)
		 (prev-type nil)
		 (total-len 0)
		 (posn 0))
	     (dolist (v varlist)
	       (let* ((decl (find v (rest common_var_decls) :key #'third))
		      (var-type (if decl (second decl) nil))
		      (el-type (if (subtypep var-type 'array)
				   (second var-type)
				   var-type)))
		 (unless prev-type
		   (setf prev-type el-type))
		 (multiple-value-bind (len arrayp)
		     (if (subtypep var-type 'array)
			 (values (first (third var-type)) t)
			 (values 1 nil))
		   (unless (and (subtypep el-type prev-type)
				(subtypep prev-type el-type))
		     (setf prev-type el-type)
		     (incf part)
		     (setf total-len 0)
		     (setf posn 0))
		       
		   (create key v var-type part posn len arrayp)
		   (incf total-len len)
		   (incf posn len)))))))
      (maphash #'process-block *common-blocks*)
      (setf sym-mlets (nreverse sym-mlets))
      (setf sym-lets (nreverse sym-lets))
      ;;(format t "sym-mlets = ~S~%" sym-mlets)
      ;;(format t "sym-lets  = ~S~%" sym-lets)
      (values sym-mlets sym-lets))))

(defun coerce-parameter-assign (lhs rhs)
  (let* ((lhs-type (first (get-upgraded-fun-arg-type (list (list lhs)))))
	 (rhs-type (first (get-upgraded-fun-arg-type (list (list rhs))))))
    ;;(format t "~&")
    ;;(format t "lhs = ~A, type ~A~%" lhs lhs-type)
    ;;(format t "rhs = ~A, type ~A~%" rhs rhs-type)
    (cond ((subtypep lhs-type 'string)
	   (warn "Assignment of string in parameter statements may not be right.")
	   rhs)
	  (t
	   (let ((new-rhs
		  (cond ((find *coerce-assignments* '(t :always))
			 `(coerce ,rhs (type-of ,lhs)))
			((find *coerce-assignments* '(nil :never))
			 rhs)
			(t
			 ;;(format t "rhs-type, rhs = ~S ~S~%" rhs-type rhs)
			 ;; RHS.  Otherwise, coerce the RHS to the
			 ;; type of the LHS.  However, we can't coerce
			 ;; something to an integer.  Use truncate for
			 ;; that.
			 (cond ((or (eq t rhs-type)
				    (eq t lhs-type)
				    (subtypep rhs-type lhs-type))
				;; No coercion is needed if the types
				;; match, or if we can't determine the
				;; type of the LHS or RHS.
				rhs)
			       ((and (subtypep lhs-type 'integer)
				     (not (subtypep rhs-type 'integer)))
				;; We're trying to set a integer
				;; variable to non-integer value.  Use
				;; truncate.
				`(int ,rhs))
			       (t
				;; Haven't a clue, so coerce
				`(coerce ,rhs ',lhs-type)))))))
	     new-rhs)))))

(defun flatten-list (x)
  (labels ((flatten-helper (x r);; 'r' is the stuff to the 'right'.
	     (cond ((null x) r)
		   ((atom x)
		    (cons x r))
		   (t (flatten-helper (car x)
				      (flatten-helper (cdr x) r))))))
    (flatten-helper x nil)))

#+nil
(defun entry-functions (entry main args)
  (flet ((make-vars (n)
	   (let ((v '()))
	     (dotimes (k n)
	       (push (intern (format nil "V~D" k)) v))
	     (nreverse v)))
	 (select-vars (k n vlist)
	   (append (list (elt vlist k))
		   (subseq vlist n)))
	 (make-ignore (k n vlist)
	   (append (subseq vlist 0 k)
		   (subseq vlist (1+ k) n)))
	   )
    (let* ((count 0)
	   (n-entries (1+ (length *entry-points*)))
	   (n-returns (+ n-entries (length args)))
	   (vlist (make-vars n-returns)))
      (cons `(defun ,main ,args
	      (multiple-value-bind ,vlist
		  (,entry ',main ,@args)
		(declare (ignore ,@(make-ignore 0 n-entries vlist)))
		(values ,@(select-vars 0 n-entries vlist))))
	    (mapcar #'(lambda (x)
			(let ((vlist (make-vars n-returns)))
			  (incf count)
			  `(defun ,(first x) ,@(rest x)
			    (multiple-value-bind ,vlist
				(,entry ',(first x) ,@(second x))
			      (declare (ignore ,@(make-ignore count n-entries vlist)))
			      (values ,@(select-vars count n-entries vlist))))
			  ))
		    *entry-points*)))))

(defun entry-functions (entry main args)
  (flet ((make-vars (n)
	   (let ((v '()))
	     (dotimes (k n)
	       (push (intern (format nil "V~D" k)) v))
	     (nreverse v)))
	 #+nil
	 (make-ignore (k n vlist)
	   (append (subseq vlist 0 k)
		   (subseq vlist (1+ k) n)))
	   )
    (let* ((count 0)
	   (n-returns (length args))
	   (vlist (make-vars n-returns)))
      (cons `(defun ,main ,args
	      (multiple-value-bind ,vlist
		  (,entry ',main ,@args)
		(values ,@vlist)))
	    (mapcar #'(lambda (x)
			(let ((vlist (make-vars n-returns)))
			  (incf count)
			  `(defun ,(first x) ,@(rest x)
			    (multiple-value-bind ,vlist
				(,entry ',(first x) ,@(second x))
			      (values ,@vlist)))))
		    *entry-points*)))))


(defvar *enable-merging-data-and-save-init* t)

;; This is kind of experimental (and a very, very gross
;; implementation!).  What we're trying to do is move all of the data
;; initialization statements for arrays (from DATA statements) into
;; the initialization of the variable itself.  This makes the code
;; look like more idiomatic Lisp and it seems to help out compilers
;; quite a bit.
(defun merge-data-and-save-inits (saves data)
  (labels ((find-inits (name)
	     ;; Look through the data init statements to find an array
	     ;; initializer for our specified variable NAME.  The
	     ;; initializer will look something like (FSET (FREF VAR
	     ;; index limits) value).  Return a list of all matches as
	     ;; the first value.  The scond value indicates if this is a
	     ;; 1D array.
	     (let ((inits '())
		   (1d-array-p t))
	       (dolist (item data)
		 ;; FIXME: This critically depends on FSET being used
		 ;; to initialize the values for DATA statements.  If
		 ;; this is changed, we need to change this!  (Gross!)
		 (when (eq 'fset (first item))
		   (destructuring-bind (fset (fref var-name indices &rest dims) value)
		       item
		     (declare (ignore fset fref dims value))
		     (when (and (eq var-name name))
		       (push item inits)
		       (unless (= 1 (length indices))
			 (setf 1d-array-p nil))))))
	       (values (nreverse inits) 1d-array-p)))
	   (col-major-index (indices limits)
	     ;; Compute the column major index given the set of indices and bounds.
	     ;; Given indicies (i1 i2 ... in) and limits ((l1 h1) (l2 h2) ... (ln hn)), the colum-major index as used by Fortran would be
	     ;; (i1 - l1) +
	     ;;   (h1-l1+1)*((i2 - l2) +
	     ;;                (h2-l2+1)*((i3 - l3) +
	     ;;                             (h3 - l3 + 1) * ...)))
	     ;;
	     ;; See col-major-index in macros.l too.
	     (if (null indices)
		 0
		 (destructuring-bind (lo hi)
		     (car limits)
		   (+ (- (car indices)
			 lo)
		      (* (1+ (- hi lo))
			 (col-major-index (rest indices) (rest limits))))))))
	 
    (let ((new-saves '())
	  (new-data '()))
      ;; First, save all non-array data initializers.
      (dolist (item data)
	(unless (eq 'fset (first item))
	  (push item new-data)))
      ;; Now look through all the saved vars
      (dolist (item saves)
	(cond ((and (listp (second item))
		    (eq 'make-array (first (second item))))
	       ;; Got an array.  Look for initializers in DATA.
	       (let ((type (second (fourth (second item))))
		     (inits (find-inits (first item))))
		 (cond ((and inits (= (length inits) (second (second item))))
			;; Got enough initializers.
			(let ((init (make-array (length inits)
						:element-type type
						:initial-element
						(coerce 0 type))))
			  ;; Gather up the values into an array to be
			  ;; used for initializing the variable.  The
			  ;; array is initialized to zero (of the
			  ;; appropriate type).  If the original code
			  ;; didn't initialize it, then it didn't
			  ;; care.
			  (dolist (i inits)
			    (destructuring-bind (fset (fref var indices limits) val)
				i
			      (declare (ignore fset fref var))
			      (setf (aref init (col-major-index indices limits))
				    val)))
			  (push (list (car item)
				      (append (copy-list (second item))
					      `(:initial-contents ',(coerce init 'list))))
				new-saves)))
		       (t
			;; Didn't match.  Save the item, and whatever
			;; initializers we found that matched the name
			;; but not the dimension.
			(push item new-saves)
			(dolist (i inits)
			  (push i new-data))))))
	      ((and (listp (second item))
		    (eq 'f2cl-init-string (first (second item))))
	       ;; Initializing an array of strings.  We don't do
	       ;; anything special right now.
	       (push item new-saves)
	       (dolist (i (find-inits (first item)))
		 (push i new-data)))
	      (t
	       ;; Not an array
	       (push item new-saves))))
	(values (nreverse new-saves)
		(nreverse new-data)))))

(defun verify-and-generate-equivalences ()
  ;; Look over equivalences and see if we can handle them.  Right now,
  ;; we can only handle equivalences of the form (array, simple) or
  ;; (simple, array), and they must have the same type.
  ;;
  ;; So, if we have something like (x, y(4)), we can use a
  ;; symbol-macrolet to make x equivalent to y(4).
  (flet ((verify-types (array b)
	   #+nil
	   (progn
	     (format t "Type of array: ~A = ~A (~A)~%" array (lookup-vble-type array)
		     (vble-is-array-p array))
	     (format t "Type of a: ~A = ~A~%" b (lookup-vble-type b))
	     (format t "explicit-vars = ~A~%" *explicit_vble_decls*)
	     (format t "*declared_vbles* = ~A~%" *declared_vbles*))
	   (assert (vble-is-array-p array))
	   (let ((a-type (lookup-vble-type array))
		 (b-type (lookup-vble-type b)))
	     (unless (eq a-type b-type)
	       (error "f2cl cannot equivalence variables of different types: ~A (~A) and ~A (~A)"
		      array a-type b b-type))))
	 (gen-fref (a)
	   a))
    (let (res simple)
      (dolist (equiv *equivalenced-vars*)
	(cond ((and (symbolp (first equiv))
		    (symbolp (second equiv)))
	       ;; Equivalence of two simple vars.  Just make one a
	       ;; symbol-macrolet of the other, if the types match.
	       (let ((a-type (lookup-vble-type (first equiv)))
		     (b-type (lookup-vble-type (second equiv))))
		 (unless (eq a-type b-type)
		   (error "f2cl cannot equivalence variables of different types: ~A (~A) and ~A (~A)"
			  (first equiv) a-type (second equiv) b-type))
		 (push (first equiv) simple)
		 (push `(,(first equiv) ,(second equiv)) res)))
	      ((and (symbolp (first equiv))
		    (listp (second equiv))
		    (eq (car (second equiv)) 'fref))
	       ;; (simple, array)
	       ;; We want (symbol-macrolet (simple expansion))
	       (verify-types (second (second equiv)) (first equiv))
	       (push `(,(first equiv) ,(gen-fref (second equiv))) res)
	       (push (first equiv) simple))
	      ((and (symbolp (second equiv))
		    (listp (first equiv))
		    (eq (car (first equiv)) 'fref))
	       ;; (array, simple)
	       (verify-types (second (first equiv)) (second equiv))
	       (push `(,(second equiv) ,(gen-fref (first equiv))) res)
	       (push (second equiv) simple))
	      (t
	       (format t "~S~%" (first equiv))
	       (format t "~S~%" (second equiv))
	       (error "f2cl cannot handle EQUIVALENCE of ~A and ~A~%"
		      (first equiv) (second equiv)))))
      (values (nreverse res) (nreverse simple)))))

(defun get-var-types (arglist &key declare-vars)
  "Compute the types of each variable in ARGLIST and also an
  appropriate declaration for each variable, if DECLARE-VARS is
  non-NIL."
  (let ((var-decls nil)
	(var-type-list nil))
    (dolist (vble arglist)
      (if (or (member vble *external-function-names*)
	      (member vble *functions-used*
		      :key #'caar))
	  (push t var-type-list)
	(let ((decl (make-declaration vble :vble-is-formal-arg t)))
	  (destructuring-bind (declare-sym (type-sym type var))
	      decl
	    (declare (ignore declare-sym type-sym var))
	    (when declare-vars
	      (push decl var-decls))
	    (setf *declared_vbles*
		  (remove vble *declared_vbles*))
	    (setf *undeclared_vbles*
		  (remove vble *undeclared_vbles*))
	    (push type var-type-list)))))
    (values (nreverse var-type-list)
	    (nreverse var-decls))))

(defun insert-declarations (fort-fun) 
  (prog (defun-bit arglist prog-bit formal-arg-decls common_var_decls
		   local-vbles vble-decls body common-blocks
		   saved-decls save-inits
		   other-fcn-decls
		   stmt-fcn-decls
		   arg-fcn-decls
		   common-block-structs
		   key-params
		   key-params-decls
		   all-decls
		   #+nil additional-args
		   entry-points equivalences)

     (setq defun-bit (list (car fort-fun) (cadr fort-fun))
	   arglist (caddr fort-fun)
	   body (cdddr fort-fun))
     (setq *undeclared_vbles* 
	   (set-difference *undeclared_vbles* *subprog_common_vars*))
     (when (member :insert-declaration *f2cl-trace*)
       (format t "~&")
       (format t "declared_vbles   = ~S~%" *declared_vbles*)
       (format t "undeclared_vbles = ~S~%" *undeclared_vbles*)
       (format t "implicit_vbles   = ~S~%" *implicit_vble_decls*)
       (format t "*functions used* = ~S~%" *functions-used*)
       (format t "external func    = ~S~%" *external-function-names*)
       (format t "*subprog_common_vars* = ~S~%" *subprog_common_vars*)
       (format t "*common_array_dims*   = ~S~%" *common_array_dims*)
       (format t "*explicit_vble_decls* = ~A~%" *explicit_vble_decls*)
       (maphash #'(lambda (key val)
		    (format t "~A => ~A~%" key val))
		*common-blocks*))
     ;;(setq special-proclamation (make-special-proclamation *subprog_common_vars*))
     #+nil
     (when (member :insert-declaration *f2cl-trace*)
       (format t "special-proclamation = ~a~%" special-proclamation))
     (setq common_var_decls 
	   (mapcar #'make-special-var-decl *subprog_common_vars*))
     ;; Clean up the declarations by merging them into one
     (setq common_var_decls
	   (append '(declaim)
		   (mapcar #'(lambda (decl)
			       (second decl))
			   common_var_decls)))

     ;;(format t "*subprog_common_vars* = ~S~%" *subprog_common_vars*)
     ;;(format t "*common_array_dims* = ~S~%" *common_array_dims*)
     ;;(format t "declared = ~S~%" common_var_decls)
     
     (setf common-block-structs (make-common-block-structure common_var_decls))
     (when (member :insert-declaration *f2cl-trace*)
       (format t "struct = ~S~%" common-block-structs))

     (setq common-blocks
	   (if *declare-common-blocks*
	       common-block-structs
	       nil))

     (when (member :insert-declaration *f2cl-trace*)
       (format t "*declare-common-blocks* = ~S~%" *declare-common-blocks*)
       (format t "spec-proc = ~S~%" common-blocks))
     
     ;;(format t "common-blocks = ~a~%" common-blocks)

     ;; Get the declarations for all functions
     (multiple-value-setq (stmt-fcn-decls other-fcn-decls arg-fcn-decls)
       (make-fcn-decl *functions-used* arglist))

     #+nil
     (setq formal-arg-decls
	   (pretty-decls
	    (append
	     ;; Declare any function arguments
	     (if (equalp arg-fcn-decls '((declare)))
		 nil
		 arg-fcn-decls)
	     ;; Declare variables
	     (mapcar #'(lambda (vble)
			 (let ((decl
				(make-declaration vble :vble-is-formal-arg t)))
			   (format t "~S: ~S is ~S~%" defun-bit vble decl)
			   (setf *declared_vbles*
				 (remove vble *declared_vbles*))
			   (setf *undeclared_vbles*
				 (remove vble *undeclared_vbles*))
			   decl))
		     (set-difference arglist *external-function-names*)))))
     ;;(format t "*functions used* = ~S~%" *functions-used*)
     (let* ((var-decls nil)
	    (var-type-list
	      (mapcar #'(lambda (vble)
			  (if (or (member vble *external-function-names*)
				  (member vble *functions-used*
					  :key #'caar))
			      t
			      (let ((decl
				     (make-declaration vble :vble-is-formal-arg t)))
				(destructuring-bind (declare-sym (type-sym type var))
				    decl
				  (declare (ignore declare-sym type-sym var))
				  (push decl var-decls)
				  (setf *declared_vbles*
					(remove vble *declared_vbles*))
				  (setf *undeclared_vbles*
					(remove vble *undeclared_vbles*))
				  type))))
		      arglist)
	      ))
       (setf var-decls (nreverse var-decls))

       ;; (format t "declare ~S: ~S~%" (second defun-bit) var-type-list)
       (let ((entry (gethash (second defun-bit) *f2cl-function-info*)))
	 (if entry
	     (setf (f2cl-finfo-arg-types entry) var-type-list)
	     (setf (gethash (second defun-bit) *f2cl-function-info*)
		   (make-f2cl-finfo :arg-types var-type-list))))
       (setq formal-arg-decls
	   (pretty-decls
	    (append
	     ;; Declare any function arguments
	     (if (equalp arg-fcn-decls '((declare)))
		 nil
		 arg-fcn-decls)
	     ;; Declare variables
	     var-decls))))
       

     ;;(format t "formal-arg-decls = ~A~%" formal-arg-decls)
     (when *relaxed-array-decls*
       ;; Old Fortran practice often declared arrays to functions to
       ;; have length 1.  Since Fortran didn't check array bounds,
       ;; this basically meant such arrays could have any length.
       ;; This bit of code converts any such explicit declarations of
       ;; sizes to '*, meaning anything goes.
       (setf formal-arg-decls
	     (list 
	      (mapcar #'(lambda (decl)
			  (cond ((and (listp decl)
				      (eq 'type (first decl))
				      (subtypep (second decl) 'array))
				 (destructuring-bind (a n l)
				     (second decl)
				   `(type (,a ,n ,(mapcar #'(lambda (x)
							      (declare (ignore x))
							      '*)
							  l))
				     ,@(rest (rest decl)))))
				(t
				 decl)))
		      (first formal-arg-decls)))))

     ;;(format t "maybe relaxed formal-arg-decls = ~A~%" formal-arg-decls)

     ;; Clean up other-fcn-decls.  If there weren't any, make it so,
     ;; instead of leaving it as an empty declare.
     (if (equal other-fcn-decls '((declare)))
	 (setf other-fcn-decls nil)
	 (setf other-fcn-decls (pretty-decls other-fcn-decls)))

     (setf other-fcn-decls nil)
     
     ;; If we are auto-SAVE'ing variables initialized in DATA
     ;; statements, we add all of the variables in the *data-init* list
     ;; to the *save_vbles* list, removing duplicates.

     (flet ((extract-var-name (setter)
	      ;; From the setting form, we extract the variable name.
	      ;; Currently setters look something like this:
	      ;;
	      ;; (setq var val)
	      ;;
	      ;; (replace array '(a b c ...))
	      ;;
	      ;; (fset (fref array n bounds) v)
	      ;;
	      ;; (data-implied-do do-loop var val)

	      ;;(format t "e-v-n:  ~S~%" setter)
	      (when (listp setter)
		(cond ((eq 'setq (first setter))
		       (second setter))
		      ((eq 'replace (first setter))
		       (second setter))
		      ((eq 'fset (first setter))
		       (second (second setter)))
		      ((eq 'data-implied-do (first setter))
		       (third setter))))))
       (when (and *auto-save-data* *data-init* (not (eq *save_vbles* '%save-all-locals%)))
	 (setf *save_vbles*
	       (remove-duplicates
		(append *save_vbles*
			(remove nil 
				(flatten-list
				 (mapcar #'extract-var-name
					 *data-init*))))))))

     ;; If a variable names a function used or an external function,
     ;; delete the variable.
     (setf *declared_vbles*
	   (remove-if #'(lambda (v)
			  (or (member v *functions-used* :key #'caar)
			      (member v *external-function-names*)))
		      *declared_vbles*))
     ;; If a variable names a function used or an external function or
     ;; delete the variable.
     (setf *undeclared_vbles*
	   (remove-if #'(lambda (v)
			  (or (member v *functions-used* :key #'caar)
			      (member v *external-function-names*)))
		      *undeclared_vbles*))

     (when (eq *save_vbles* '%save-all-locals%)
       ;; If *save_vbles* is the magic '%save-all-locals%, we want to
       ;; save all local variables.
       (setf *save_vbles* (concatenate 'list *declared_vbles* *undeclared_vbles*)))
       
     ;;(format t "*key_params* = ~S~%" *key_params*)
     ;;(format t "key-params = ~S~%" key-params)

     ;; Convert reserved names in parameter statements.  Coerce the
     ;; bindings to the right type as well.
     (setq key-params
	   (mapcar #'(lambda (x)
		       (let ((maybe-new-name (check-reserved-lisp-names (car x))))
				    
			 (list maybe-new-name
			       (coerce-parameter-assign maybe-new-name (cadr x)))))
		   *key_params*))
     ;;(format t "key-params = ~S~%" key-params)
     
     (setq key-params-decls
	   (mapcar #'(lambda (param)
		       (destructuring-bind (v val)
			   param
			 (make-declaration v :parameterp (if (numberp val) val nil))))
		   key-params))
     ;;(format t "key-params-decl = ~S~%" key-params-decls)
     ;; Clean up the declarations
     (setf key-params-decls
	   `((declare ,@(mapcan #'cdr key-params-decls))))
     ;;(format t "key-params-decl = ~S~%" key-params-decls)
     

     ;; Initialize local variables
     (setq local-vbles
	   (remove-duplicates
	    (remove nil
		    (mapcar #'make-initialisation 
			    (remove-if 
			     #'(lambda (x)
				 (or (member x *save_vbles*) 
				     (member x key-params :key #'car)))
			     (set-difference (append *declared_vbles*
						     *undeclared_vbles*)
					     *subprog_common_vars*))))
	    :test #'(lambda (a b)
		      (eq (first a) (first b)))))

     ;; Remove %false% and %true% from the initialization lists.
     (setf local-vbles (remove-if #'(lambda (x)
				      (member (first x) '(%false% %true%)))
				  local-vbles))

     ;; Declare local variables, but remove any variables explicitly
     ;; declared as SAVE'd.
     (setq vble-decls
	   (pretty-decls
	    (remove-if
	     #'(lambda (x)
		 (member (third (second x)) '(%false% %true%)))
	     (remove-duplicates
	      (remove nil
		      (mapcar #'(lambda (vble)
				  (make-declaration vble
						    :vble-is-formal-arg nil))
			      (remove-if
			       #'(lambda (x)
				   (or (member x *save_vbles*)))
			       (set-difference
				(append
				 (set-difference *declared_vbles* 
						 (mapcar #'car key-params))
				 *undeclared_vbles*)
				*subprog_common_vars*))))
	      :test #'(lambda (a b)
			(eq (third (second a))
			    (third (second b))))))))

     ;; If we have saved variables, setup their declarations too.
     ;;(format t "*save_vbles* = ~S~%" *save_vbles*)
     (setq saved-decls
	   (pretty-decls
	    (remove-if
	     #'(lambda (x)
		 (member (third (second x)) '(%false% %true%)))
	     (remove-duplicates
	      (remove nil
		      (mapcar #'(lambda (vble)
				  (make-declaration vble
						    :vble-is-formal-arg nil))
			      *save_vbles*))
	      :test #'(lambda (a b)
			(eq (third (second a))
			    (third (second b))))))))

     ;; Initialize SAVE'd variables appropriately
     (setq save-inits
	   (remove-duplicates
	    (remove nil
		    (mapcar #'make-initialisation 
			    (remove-if 
			     #'(lambda (x)
				 (member x key-params :key #'car))
			     *save_vbles*)))
	    :test #'(lambda (a b)
		      (eq (first a) (first b)))))

     ;; Clean up data inits: handle the fset inits and the
     ;; data-implied-do inits.

     ;;(format t "*data-init* before: ~S~%" *data-init*)
     (setq *data-init*
	   (mapcar #'(lambda (init)
		       (flet ((get-dims (var)
				(mapcar #'caar
					    (mapcar #'lookup-array-bounds
						    (if (listp var) var (list var)))))
			      (get-types (vars)
				;; I'm lazy.  Use make-declaration
				;; to figure out the type of the array
				;; element.
				(mapcar #'(lambda (v)
					    (let ((init (make-declaration v)))
					      (second (second (second init)))))
					vars)))

		       (cond #+nil
			     ((eq 'fset (first init))
			      ;; We need to get the dimensions for this array
			      (destructuring-bind (fset (fref var idx) val)
				  init
				(declare (ignore fset fref))
				`(fset (fref ,var ,idx ,(get-dims var)) ,val)))
			     ((eq 'data-implied-do (first init))
			      ;; We need to get the dimensions for
			      ;; this array as well as the types.
			      (destructuring-bind (ido loop var vals)
				  init
				(declare (ignore ido))
				`(data-implied-do ,loop ,(get-dims var) ,(get-types var)
						  ,vals)))
			     ((eq 'replace (first init))
			      ;; These are initialized by f2cl-init-string (I hope).
			      nil)
			     (t
			      init))))
		   *data-init*))
     ;;(format t "*data-init* after : ~S~%" *data-init*)
     (setf *data-init* (delete nil *data-init*))
     ;;(format t "*data-init* after : ~S~%" *data-init*)
     
     (setq arglist (mapcar #'check-reserved-lisp-names arglist))

     ;;(format t "arglist = ~a~%" arglist)
     #+nil
     
     (format t "arglist arrays = ~A~%"
	     (remove nil
		     (mapcar #'(lambda (x)
				 (if (subtypep (second x) 'array)
				     (cddr x)
				     nil))
			     (rest (first formal-arg-decls)))))
     ;;(format t "local-vbles     = ~S~%" local-vbles)
     ;;(format t "vbles-decls     = ~S~%" vble-decls)
     ;;(format t "other-fcn-decls = ~S~%" other-fcn-decls)
     ;;(format t "body            = ~S~%" body)
     (setf all-decls (append (rest (first vble-decls))
			     (rest (first other-fcn-decls))))
     (setf all-decls `((declare ,@all-decls)))
     ;;(format t "all-decls = ~A~%" all-decls)

     (when *entry-points*
       ;; First make sure the entry points are consistent in name and
       ;; number of arguments.  We don't support anything else (yet?)
       (unless (every #'(lambda (f)
			  (let ((result (equal (second f) arglist)))
			    (unless result
			      (warn "ENTRY ~A doesn't match the expected arg list: ~A~%" (second f) arglist))
			    result))
		      *entry-points*)
	 (warn "Some ENTRY points don't match the expected signature ~A~%"
	       arglist))

       ;; Add entry points to the function database

       (dolist (f *entry-points*)
	 ;;(format t "Adding entry point ~A to database~%" f)
	 (destructuring-bind (name args &optional parent)
	     f
	   ;;(format t "name, args, parent = ~A ~A ~A~%" name args parent)
	   (let ((entry (gethash name *f2cl-function-info*))
		 (var-type-list (get-var-types args)))
	     #+nil
	     (progn
	       (format t "entry = ~A~%" entry)
	       (format t "var-type-list = ~A~%" var-type-list)
	       (format t "parent info = ~A~%" (gethash parent *f2cl-function-info*)))
	     (cond
	       (parent
		;; If we know parent of the entry point function, we
		;; copy the information from the parent to this entry.
		;; (Because we only support entry points with the same
		;; number and type of args.)
		(let ((pe (gethash parent *f2cl-function-info*)))
		  (cond
		    (entry
		     (setf (f2cl-finfo-arg-types entry)
			   (f2cl-finfo-arg-types pe))
		     (setf (f2cl-finfo-return-values entry)
			   (f2cl-finfo-return-values pe)))
		    (t
		     (setf (gethash name *f2cl-function-info*)
			   (make-f2cl-finfo :arg-types (f2cl-finfo-arg-types pe)
					    :return-values (f2cl-finfo-return-values pe)))))))
	       (t
		(warn "Got entry point for which we have no parent!")
		(if entry
		    (setf (f2cl-finfo-arg-types entry) var-type-list)
		    (setf (gethash name *f2cl-function-info*)
			  (make-f2cl-finfo :arg-types var-type-list))))))))
       
       (setf entry-points
	     (mapcar #'(lambda (x)
			 (let ((name (first x)))
			   `(if (eq %name% ',name) (go ,name))))
		     *entry-points*)))
     
     ;;(format t "entry-points = ~A~%" entry-points)
       
     (setq prog-bit
	   (if (or *save_vbles* *auto-save-data*)
	       ;; If we have SAVE'd variables, don't put their inits
	       ;; into the function.  Put them in the let outside the function
	       ;; where they belong.
	       (if *subprog-stmt-fns*
		   `(labels ,*subprog_stmt_fns_bodies* ,@stmt-fcn-decls
		     (prog ,local-vbles ,@all-decls ,@entry-points ,@body))
		   `(prog ,local-vbles ,@all-decls ,@entry-points ,@body))
	       (if *subprog-stmt-fns*
		   `(labels ,*subprog_stmt_fns_bodies* ,@stmt-fcn-decls
		     (prog ,local-vbles ,@all-decls ,@*data-init* ,@entry-points ,@body))
		   `(prog ,local-vbles ,@all-decls ,@*data-init* ,@entry-points ,@body)))
	       )
     ;;(format t "prog-bit = ~%~S~%" prog-bit)

     ;; Do some common fixups to make the code faster (for compilers
     ;; not sufficiently smart) and prettier

     (setf prog-bit (fixup-expression (fixup-expr-mul prog-bit)))

     (setf prog-bit (fixup-relop prog-bit))

     (setf prog-bit (merge-ops prog-bit))

     ;; Try to optimize out calls to f2cl/.
     ;;(format t "fixed-up prog-bit = ~%~S~%" prog-bit)
     (setf prog-bit (optimize-f2cl/ prog-bit))

     ;;(format t "before opt int:~%~A~%" prog-bit)
     (setf prog-bit (optimize-integer-arithmetic prog-bit))
     ;;(format t "after opt int:~%~A~%" prog-bit)
     ;;(format t "opt prog = ~%~S~%" prog-bit)
     ;;(format t "*save_vbles* = ~a~%" *save_vbles*)
     ;;(format t "*data-init* = ~S~%" *data-init*)
     ;;(format t "save-inits  = ~S~%" save-inits)

     (when *enable-merging-data-and-save-init*
       (multiple-value-setq (save-inits *data-init*)
	 (merge-data-and-save-inits save-inits *data-init*)))
     
     ;;(format t "new *data-init* = ~S~%" *data-init*)
     ;;(format t "new save-inits  = ~S~%" save-inits)
     
     ;;(format t "prog-bit = ~a~%" prog-bit)

     ;; Replace all references to external functions with #'.
     ;; However, if the external function was on the parameter list,
     ;; we don't need to do that.
     (setf prog-bit (fixup-external-function-refs
		     prog-bit
		     (set-difference *external-function-names* arglist)))
     ;; Do the same for intrinsic function names, but be sure to
     ;; remove any variables whose name might match an intrinsic
     ;; function name.

     (when (member :insert-declaration *f2cl-trace*)
       (format t "key-params            = ~A~%" key-params)
       (format t "*declared_vbles*      = ~A~%" *declared_vbles*)
       (format t "*undeclared_vbles*    = ~A~%" *undeclared_vbles*)
       (format t "*subprog_common_vars* = ~A~%" *subprog_common_vars*)
       (format t "arglist               = ~A~%" arglist))
     (setf prog-bit
	   (fixup-external-function-refs
	    prog-bit
	    (set-difference *intrinsic-function-names*
			    (append *declared_vbles*
				    *undeclared_vbles*
				    arglist
				    (mapcar #'first key-params)))))

     ;; Add additional parameters for slicing
     #+nil
     (let ((array-args
	    (let ((a '()))
	      (mapc #'(lambda (x)
			(when (subtypep (second x) 'array)
			  (mapc #'(lambda (y)
				    (push y a))
				(cddr x))))
		    (rest (first formal-arg-decls)))
	      (nreverse a))))
       ;;(format t "array-args = ~A~%" array-args)
       (setf additional-args
	     (remove nil
		     (mapcar #'(lambda (x)
				 (when (member x array-args)
				   (list (intern (concatenate 'string
							      (symbol-name x)
							      (symbol-name '#:-offset)))
					 0)))
			     arglist)))
       ;;(format t "additional args = ~A~%" additional-args)

       ;;(format t "arglist = ~a~%" arglist)
       (when additional-args
	 (setf arglist (append arglist `(&optional ,@additional-args))))
       ;;(format t "new arglist = ~a~%" arglist)

       ;; Grovel over the code looking for frefs.  Modify them to handle
       ;; array-slicing.

       ;;(format t "prog-bit = ~A~%" prog-bit)

       (labels
	   ((array-offset-name (name)
	      (intern (concatenate 'string
				   (symbol-name name)
				   (symbol-name '#:-offset))))
	    (grovel-call (p)
	      ;;(format t "grovel-call = ~A~%" p)
	      (let* ((offsets '())
		     (new-call
		      (mapcar
		       #'(lambda (x)
			   ;;(format t "x = ~A~%" x)
			   (cond
			     ((and (listp x)
				   (eq (first x) 'array-slice))
			      ;; (array-slice var type (indices) bounds)
			      (push `(+ ,(if (member (second x) array-args)
					     (array-offset-name (second x))
					     0)
				      ,(f2cl-lib::col-major-index (fourth x)
								  (fifth x)))
				    offsets)
			      (second x))
			     ((member x array-args)
			      (push (array-offset-name x)
				    offsets)
			      x)
			     ((vble-is-array-p x)
			      (push 0 offsets)
			      x)
			     (t
			      x)))
		       p)))
		(append new-call (reverse offsets))))
	    (grovel-frefs (p)
	      (cond ((or (atom p) (null p))
		     p)
		    ((eq (first p) 'fref)
		     (destructuring-bind (fref-name var &rest stuff)
			 p
		       (if (member var array-args)
			   ;; We have an fref.  If the array is an argument
			   ;; to the routine, we need to add in the offset.
			   `(,fref-name ,var ,@stuff ,(array-offset-name var))
			   p)))
		    ((eq (first p) 'multiple-value-bind)
		     ;; A Fortran function call.
		     (destructuring-bind (m-v-b vars call &rest stuff)
			 p
		       `(,m-v-b ,vars ,(grovel-call call) ,@stuff)))
		    ((and (symbolp (first p))
			  (member 'array-slice (rest p)
				  :key #'(lambda (x)
					   (if (listp x)
					       (car x)
					       x))))
		     ;; array-slice in a function call.
		     ;;(format t "array-slice in fcall: ~a~%" p)
		     `(,(first p) ,@(grovel-call (rest p))))
		    ((and (symbolp (first p))
			  (some #'(lambda (x)
				    (member x array-args))
				(rest p)))
		     `(,(first p) ,@(grovel-call (rest p))))
		    (t
		     `(,(first p) ,@(mapcar #'grovel-frefs (rest p)))))))
	 (setf prog-bit (grovel-frefs prog-bit))
	 ;;(format t "new-prog-bit = ~A~%" prog-bit)
	 ))

     (let ((array-args
	    (let ((a '()))
	      (mapc #'(lambda (x)
			(when (subtypep (second x) 'array)
			  (mapc #'(lambda (y)
				    (push y a))
				(cddr x))))
		    (rest (first formal-arg-decls)))
	      (nreverse a))))
       ;;(format t "array-args = ~A~%" array-args)
       
       
       ;;(format t "arglist = ~a~%" arglist)
       ;;(format t "new arglist = ~a~%" arglist)

       ;; Grovel over the code looking for frefs.  Modify them to handle
       ;; array-slicing.

       ;;(format t "prog-bit = ~A~%" prog-bit)

       (labels
	   ((array-offset-name (name)
	      (intern (concatenate 'string
				   (symbol-name name)
				   (symbol-name '#:-%offset%))))
	    (array-data-name (name)
	      (intern (concatenate 'string
				   (symbol-name name)
				   (symbol-name '#:-%data%))))
	    (grovel-frefs (p)
	      (cond ((or (atom p) (null p))
		     p)
		    ((eq (first p) 'fref)
		     (destructuring-bind (fref-name var &rest stuff)
			 p
		       (if (member var array-args)
			   ;; We have an fref.  If the array is an argument
			   ;; to the routine, we need to add in the offset.
			   `(,fref-name ,(array-data-name var)
			                ,@stuff ,(array-offset-name var))
			   p)))
		    (t
		     `(,(first p) ,@(mapcar #'grovel-frefs (rest p))))))
	    (generate-with-array (arrays body)
	      (let (array-data-forms)
		(flet ((find-type (a)
			 (dolist (x (rest (first formal-arg-decls)))
			   (when (member a x)
			     (return-from find-type (second (second x))))
			   t)))
		  (dolist (a arrays)
		    (let ((d-name (array-data-name a))
			  (o-name (array-offset-name a))
			  (d-type (find-type a)))
		      (push `(,a ,d-type ,d-name ,o-name) array-data-forms)))
		  (if array-data-forms
		      `(with-multi-array-data ,array-data-forms
			,body)
		      body)))))
	 ;;(format t "formal-arg-decls = ~A~%" formal-arg-decls)

	 (when *equivalenced-vars*
	   (multiple-value-bind (equiv simple-vars)
	       (verify-and-generate-equivalences)
	     ;;(setf equivalences equiv)
	     ;;(format t "equivalences = ~A~%" equivalences)

	     ;; We need to go through prog-bit and remove any
	     ;; initializations and declarations of the simple-vars
	     ;; that were equivalenced.  Otherwise the initialization
	     ;; will very likely mess up the equivalence.

	     ;;(format t "prog = ~A~%" (second prog-bit))
	     (let ((fixed (remove-if #'(lambda (x)
					 (member x simple-vars))
				     (second prog-bit)
				     :key #'first)))
	       ;;(format t "inits = ~A~%" (second prog-bit))
	       ;;(format t "fixed = ~A~%" fixed)
	       (setf prog-bit `(prog ,fixed ,@(cddr prog-bit))))
	     
	     (setf prog-bit `(symbol-macrolet ,equiv
			       ,prog-bit))))

	 ;; If array-slicing is not used and the array-type is
	 ;; :simple-array, we don't need the with-array-data stuff
	 ;; because we couldn't have sliced the array.
	 (when (eq *array-type* 'common-lisp:array)
	   (setf prog-bit (grovel-frefs prog-bit))
	   ;;(format t "new-prog-bit = ~A~%" prog-bit)

	   (setf prog-bit (generate-with-array array-args prog-bit)))
	 ;;(format t "new-prog-bit = ~A~%" prog-bit)
		 
	 ))

     ;; We need to handle BLOCK DATA subprograms differently from
     ;; normal subprograms.  (BLOCK DATA subprograms always start with
     ;; "/BLOCKDATA".  See f2cl1.l that sets this name.)
     (cond
       ((and (let* ((sub-name (string (second defun-bit)))
		    (name-len (min 10 (length sub-name))))
	       (string-equal sub-name "/blockdata" :end1 name-len :end2 name-len))
	     *subprog_common_vars*)
	;; Block data subprograms need to be handled specially.  The
	;; data-init part needs to be moved inside the body and
	;; massaged to initialize the data.  
	(multiple-value-bind (sym-macs sym-lets)
	    (create-sym-macros)
	  #+nil
	  (format t "save-inits = ~A~%" (mapcar #'(lambda (x)
						    `(setf ,@x))
						save-inits))
	  #+nil
	  (format t "data-inits = ~A~%"
		  (mapcar #'(lambda (x)
			      (if (eq (first x) 'setq)
				  `(setf ,@(rest x))
				  x))
			  *data-init*))
	  ;; May want to remove multiple initializations since
	  ;; save-inits and *data-init* might both initiliaze the
	  ;; variable.  (Why is that?)
	  (setf prog-bit
		`(let ,sym-lets
		  (symbol-macrolet ,sym-macs
		  ,@(append (mapcar #'(lambda (x)
					`(setf ,@x))
				    save-inits)
			    (mapcar #'(lambda (x)
					(if (eq (first x) 'setq)
					    `(setf ,@(rest x))
					    x))
				    *data-init*))))))
	(let* ((defun-stuff `(defun ,(cadr defun-bit) ,arglist
			      ,prog-bit))
	       (param-stuff (if key-params
				`(let* ,key-params
				  ,@key-params-decls
				  ,defun-stuff)
				defun-stuff)))
	  (return
	    (values common-blocks
		    (when common-blocks
		      (make-common-block-init *common-blocks* common_var_decls key-params key-params-decls))
		    param-stuff))
	  )
	)
       (t

	;; Return 3 pieces: Any proclamations for special variables, the
	;; declarations for the special variables, and, finally, the
	;; function itself.
	(when *subprog_common_vars*
	  (multiple-value-bind (sym-macs sym-lets)
	      (create-sym-macros)
	    (setf prog-bit `(let ,sym-lets
			      (symbol-macrolet ,sym-macs ,prog-bit)))))

	;; Handle entry points.
	(let* ((defun (if *entry-points* 'labels 'defun))
	       (defun-name (if *entry-points*
			       (intern (concatenate 'string (symbol-name '#:multi-entry-)
						    (string (cadr defun-bit))))
			       (cadr defun-bit)))
	       (defun-stuff (if *entry-points*
				`(,defun ((,defun-name (%name% ,@arglist)
					  ,@(unless (equal formal-arg-decls '((declare)))
					    formal-arg-decls)
					  ,prog-bit))
				  ,@(entry-functions defun-name (cadr defun-bit) arglist))
				`(,defun ,defun-name ,arglist
				  ;; Remove empty declaration
				  ,@(unless (equal formal-arg-decls '((declare)))
					    formal-arg-decls)
				  ,prog-bit)))
	       (save-stuff (if (and (or *save_vbles* *auto-save-data*)
				    save-inits)
			       `(let ,save-inits
				 ,@saved-decls
				 ,@*data-init*
				 ,defun-stuff)
			       defun-stuff))
	       (param-stuff (if key-params
				`(let* ,key-params
				  ,@key-params-decls
				  ,save-stuff)
				save-stuff)))
	  ;;(format t "param-stuff = ~A~%" param-stuff)
	  (return 
	    (values common-blocks
		    (when common-blocks
		      (make-common-block-init *common-blocks* common_var_decls key-params key-params-decls))
		    param-stuff)))))
     ))



;; given vble return (vble init-value)
;; for prog arglist

(defun vble-declared-twice-p (vble vble_decls)
  (let ((ndecls 0)
	v-type v-name)
    (dolist (type-list vble_decls)
      (let ((found (member vble (rest type-list) :key #'car)))
	(when found
	  (incf ndecls)
	  (push (first type-list) v-type)
	  (push (car found) v-name))))
    (when (> ndecls 1)
      (mapcar #'list v-type v-name))))


(defun make-initialisation (vble)
  ;; Look up variable in *data-init* to see if it has an
  ;; initialized value from a data statement
  (flet ((lookup-data-init (v)
	   (let ((val (find-if #'(lambda (name)
				   (eq v (second name)))
			       *data-init*)))
	     ;;(format t "lookup-data-init for ~S = ~S~%" v val)
	     (when val
	       (cond ((eq 'fill (first val))
		      (list 'fill (third val)))
		     ((eq 'setq (first val))
		      (third val))
		     ((eq 'replace (first val))
		      (third val)))))))
    (let* ((vble_name (check-reserved-lisp-names vble))
	   (init-val (lookup-data-init vble))
	   type decl1)
      #+nil
      (progn
	(format t "*data-init* = ~A~%" *data-init*)
	(format t "*explicit_vble_decls* = ~a~%" *explicit_vble_decls*)
	(format t "vble = ~a~%" vble)
	(format t "init-val = ~S~%" init-val))
      (cond
	;;check for vble with two declarations i.e. an array
	((setf decl1 (vble-declared-twice-p vble *explicit_vble_decls*))
	 ;; If we've already processed this variable, we don't need to
	 ;; do it again.
	 (unless (member vble *vble-declaration-done*)
	   (destructuring-bind (decl1 decl2)
	       decl1
	     ;; Remember that we have done this already
	     (pushnew vble *vble-declaration-done*)
	     (if (eq (car decl1) 'array)
		 `(,vble_name ,(make_make-array_stmt (cdadr decl1)
						     (car decl2)
						     init-val vble_name))
		 `(,vble_name ,(make_make-array_stmt (cdadr decl2)
						     (car decl1)
						     init-val vble_name))))))
	   
	;; Don't need any initialization for statement functions or
	;; external functions.  (But perhaps we should put one in to
	;; say these are actually functions?)
	((or (member vble *external-function-names*)
	     (member vble *subprog-stmt-fns*))
	 nil
	 )
	;; check for declared variable
	((member vble *declared_vbles*)
	 ;;(format t "declared var:  ~A = ~A~%" vble init-val)
	 (do ((type-clauses *explicit_vble_decls* (cdr type-clauses))
	      (decl nil))
	     ((null type-clauses) )
	   (setq type (caar type-clauses))
	   ;;(format t "type-clauses = ~S~%" type-clauses)
	   ;;(format t "looping: type = ~A~%" type)
	   (cond ((and (listp type)
		       (eq (car type) 'character))
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return `(,vble_name ,
				(make-char-init (car decl) type init-val)))))
		 ((eq type 'array)
		  ;;(format t "array type-clauses = ~S~%" (cdar type-clauses))
		  ;;(format t "array decl = ~S~%" (member vble (cdar type-clauses) :key #'car))
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return 
			`(,vble_name 
			  ,(make_make-array_stmt
			    (cdar decl)
			    (get_array_type (caar decl) nil)
			    init-val
			    vble_name)))))
		 ((eq type 'logical)
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return
			(if (cdar decl)
			    (flet ((fixup-logical (init)
				     ;; Replace %false% with NIL and
				     ;; %true% with T so we can
				     ;; initialize the logical array
				     ;; correctly.
				     (subst t '%true% (subst nil '%false% init))))
			      (let ((init (make_make-array_stmt (cdar decl) t
								(fixup-logical init-val)
								vble_name)))
				`(,vble_name ,init)))
			    `(,vble_name nil)))))
		 (t
 		  ;;(format t "default decl = ~S~%" (member vble (cdar type-clauses) :key #'car))
 		  ;;(format t "default init-val = ~S~%" init-val)
		  (if (setq decl (member vble (cdar type-clauses) :key #'car))
		      (return 
			(if (cdar decl) 
			    `(,vble_name
			      ,(make_make-array_stmt (cdar decl) type init-val vble_name))
			    `(,vble_name
			      ,(cond (init-val
				      (remove-*data-init*-var vble_name)
				      init-val)
				     (t
				      (ecase type
					((fixnum integer4 integer2 integer1)
					 0)
					(single-float 0f0)
					(double-float 0d0)
					((complex complex8) #c(0f0 0f0))
					(complex16 #c(0d0 0d0)))))))))))))
	(t
	 ;;(format t "implicit type = ~A~%" (get-implicit-type vble))
	 (cond ((setq type (get-implicit-type vble))
		`(,vble_name ,(cond (init-val
				     (remove-*data-init*-var vble_name)
				     init-val)
				    ((eq type 'fixnum) 0)
				    ((eq type 'single-float) 0f0)
				    ((eq type 'double-float) 0d0)
				    ((eq type 'complex) '(complex 0f0 0f0))
				    ((and (listp type) 
					  (eq (car type) 'character))
				     (make-char-init (list vble) type))
				    (t nil))))
	       (t
		(when init-val
		  (remove-*data-init*-var vble_name))
		(if (default-int-p vble) 
		    `(,vble_name ,(or init-val 0))
		    `(,vble_name ,(or init-val 0f0))))))))))


(defun remove-*data-init*-var (vble)
  (setf *data-init* (remove-if #'(lambda (x)
				 (equal vble (second x)))
			     *data-init*)))

;; DIMS is a list of the dimensions of an array.  DATA is a list of
;; data to use for initializing an array.
(defun fortran-data-init (type dims data)
  (cond ((every #'numberp dims)
	 ;; We can compute everything we need at compile time so let's
	 ;; do it.
	 (let ((data-len (length data))
	       (total-length (reduce #'* dims)))
	   (cond ((< data-len total-length)
		  ;; Need to append some data.
		  `(',(append data (make-list (- total-length data-len)
					      :initial-element (coerce 0 type)))))
		 ((> data-len total-length)
		  ;; Need to truncate some data
		  `(',(subseq data 0 total-length)))
		 (t
		  `(',data)))))
	(t
	 ;; Dispatch the creation to a macro to be compiled later by
	 ;; Lisp.
	 `((array-initialize ,type ,dims ,data))
	 )))

(defun make_make-array_stmt (dimens type &optional init vble)
  (let ((dims (mapcar #'(lambda (x) 
                           (cond ((member '* x) '*)
                                 ((eq (car x) 1) (cadr x))
                                 ((not (member nil (mapcar #'numberp x)))
                                  (1+ (- (cadr x) (car x))))
                                 (t `(1+ (- ,(cadr x) ,(car x))))))
                       dimens)))
    ;; Don't need to initialize this variable separately, so kill it.
    (setf *data-init* (remove-*data-init*-var vble))
    
    (let ((init-val
	   (when init
	     (if (eq 'fill (first init))
		 `(:initial-element ,(second init))
		 `(:initial-contents
		   ,@(fortran-data-init type dims
					(first (rest init))))))))
      (cond ((every #'numberp dims)
	     ;; If all of the dimensions are numbers, we don't have to
	     ;; make a list out of them.  This can speed up array
	     ;; creation for smart compilers.
	     (if (= (length dims) 1)
		 `(make-array ,(first dims) :element-type ',type ,@init-val)
		 `(make-array ,(reduce #'* dims) :element-type ',type ,@init-val)))
	    (t
	     (if (= (length dims) 1)
		 `(make-array ,@dims :element-type ',type ,@init-val)
		 `(make-array (the fixnum (reduce #'* (list ,@dims))) :element-type ',type ,@init-val)))))))

;; create a labels form for when statement functions are present

(defun make-labels-form (local-vbles vble-decls *data-init* body)
    `(labels ,*subprog_stmt_fns_bodies*
      (prog ,local-vbles ,@vble-decls ,@*data-init* ,@body)))


;; Take the bounds of an array and compute the total size of the array
;; if possible.  If not, return '*.  Useful for getting the
;; appropriate type declaration for an array.
(defun f2cl-array-total-size (bounds)
  (list (reduce #'(lambda (accum y)
		    (if (and (numberp accum) (numberp y))
			(* accum y)
			'*)
		    )
		(mapcar #'(lambda (x)
			    (if (every #'realp x)
				(1+ (reduce #'- (reverse x)))
				'*))
			bounds))))

;; make a declaration for vble which may or may not be a formal arg
;; vble_name is used to replace vbles called T with T_
;;
;; If vble is defined in a PARAMETER statement, :PARAMETERP should be
;; set to the value of the parameter.  This will generate appropriate
;; declarations for the parameter.
(defun make-declaration (vble &key vble-is-formal-arg parameterp)
 (prog (vble_name type decl1)
   (setq vble_name (check-reserved-lisp-names vble))
  (return
   (cond
     ;;check for vble with two declarations i.e. an array
     ((setf decl1 (vble-declared-twice-p vble *explicit_vble_decls*))
      (destructuring-bind (decl1 decl2)
	  decl1
	(when (eq (car decl2) 'array)
	  (rotatef decl1 decl2))
	(return `(declare (type (,(if (and vble-is-formal-arg *array-slicing*)
				      'array
				      *array-type*)
				 ,(car decl2)
                                 ,(f2cl-array-total-size (cdadr decl1))
				 )
				 ,vble_name)))))
	  
     ;; Don't need any initialization for statement functions or
     ;; external functions
     ((or (member vble *external-function-names*)
	  (member vble *subprog-stmt-fns*))
      ;;(format t "no declaration needed for ~A~%" vble)
      ;;(format t "stmt fns = ~A~%" *subprog-stmt-fns*)
      ;;(format t "ext fns  = ~A~%" *external-function-names*)
      ;;(format t "arglist  = ~A~%" *subprog-arglist*)
      nil
      )
     ;; check for declared variable
     ((member vble *declared_vbles*)
      ;;(format t "~a is declared~%" vble)
      (do ((type-clauses *explicit_vble_decls* (cdr type-clauses))
	   (decl nil))
	  ((null type-clauses) )
	(setq type (caar type-clauses))
	(cond ((and (listp type)
		    (eq (car type) 'character))
	       (if (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return (make-char-decl (car decl) type))))
	      ((eq type 'array)
	       (when (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return `(declare (type (,*array-type*
					    ,(get_array_type (caar decl) 
							     vble-is-formal-arg)
					    ,(f2cl-array-total-size (cdar decl))
					    )
				      ,vble_name)))
		   ))
	      ((eq type 'logical)
	       (if (setq decl (member vble (cdar type-clauses) :key #'car))
		   (return
		     (if (cdar decl) 
			 `(declare (type (,*array-type* logical ,(f2cl-array-total-size (cdar decl)))
				    ,vble_name))
			 `(declare (type logical ,vble))))))
	      (t
	       (when (setq decl (member vble (cdar type-clauses) :key #'car))
		 ;;(format t "declared vble ~A = ~A~%" vble parameterp)
		 (return 
		   (cond ((cdar decl) 
			  `(declare (type (,*array-type*
					   ,type 
					   ,(f2cl-array-total-size (cdar decl)))
					  ,vble_name)))
			 (t
			  (let ((limit (when parameterp
					 `(,parameterp ,parameterp))))
			  `(declare (type (,type ,@limit) ,vble_name)))))))))))
     ;; check implicitly declared variable.  (Why do we check to see
     ;; if the var is a formal arg?  What difference does that make
     ;; here?  I'm taking it out.)
     ((and #+nil (not vble-is-formal-arg)
	   (setq type (get-implicit-type vble)))
      ;;(format t "implicit decl~%")
      (if (and (listp type) (eq (car type) 'character))
	  (make-char-decl (list vble_name) type)
	  (progn
	    (when (eq type :none)
	      (warn "Undeclared variable `~A' with no implicit type!" vble))
	    `(declare (type ,type ,vble_name)))))
     ;; otherwise use default declaration
     (t
      ;;(format t "default decl~%")
      (let ((limit (when parameterp
		     `(,parameterp ,parameterp))))
	(if (default-int-p vble)
	    `(declare (type (integer ,@limit) ,vble_name))
	    `(declare (type (single-float ,@limit) ,vble_name)))))))))

(defun make-char-decl (decl type)
  (flet ((decl-bounds (bounds)
	   (let ((dims 
		  (mapcar #'(lambda (b)
			      (destructuring-bind (lo hi)
				  b
				(if (and (numberp lo) (numberp hi))
				    (+ 1 (- hi lo))
				    '*)))
			  bounds)))
	     (cond ((every #'numberp dims)
		    (list (reduce #'* dims)))
		   ((= 1 (length bounds))
		    '(*))
		   (t
		    '*)))))
    (cond ((null (cdr decl))
	   ;; scalar, no length spec.
	   ;;(format t "scalar, no length spec = ~A~%" decl)
	   (if (equal (cadr type) '(*))
	       `(declare (type (simple-array character (*)) ,(car decl)))    
	       `(declare (type (simple-array character (,(cadr type))) ,(car decl)))))
	  ((atom (cadr decl))
	   ;; scalar, length spec.
	   ;;(format t "scalar, length spec = ~A~%" decl)
	   (error "shouldn't happen!")
	   ;;`(declare (type (simple-array base-char (,(cadr decl))) ,(car decl)))
	   )
	  ((equal (cadr decl) '(*))
	   ;; unspecified length spec
	   ;;(format t "unspecified length spec = ~A~%" decl)
           `(declare (type (simple-array character (*)) ,(car decl))))
	  (t
	   ;; array, no length spec.
	   ;;(format t "array, no length spec = ~A~%" decl)
	   ;;(format t "type = ~S~%" type)
	   ;;(format t "decl-bounds = ~S~%" (decl-bounds (rest decl)))
           `(declare (type (,*array-type*
			    (simple-array character ,(if (second type)
							 `(,(second type))
							 '(*)))
			    ,(decl-bounds (rest decl))) ,(car decl)))))))

(defun make-char-init (decl type &optional init)
  (cond ((equal (cadr type) '(*))	; unspecified length spec
	 ;;(format t "make-char-init unspecified length spec (*)~%")
	 "")
	((null (cdr decl))		; scalar, no length spec.      
	 `(make-array '(,(cadr type)) :element-type 'character :initial-element #\Space))
	((atom (cadr decl))		; scalar, length spec.
	 (error "shouldn't happen!")
	 ;;`(make-array '(,(cadr decl)) :element-type 'base-char :initial-element #\Space)
	 )
	(t				; array, no length spec.
	 ;;(format t "make-char-init array, no length spec~%")
	 (let ((dims (mapcar #'(lambda (bounds)
				 (destructuring-bind (lo hi)
				     bounds
				   (if (and (numberp lo) (numberp hi))
				       (+ 1 (- hi lo))
				       `(+ 1 (- ,hi ,lo)))))
			     (remove '|,| (rest decl)))))
	   ;;(format t "dims = ~A~%" dims)
	   `(f2cl-init-string ,dims ,(cdr type) ,init)))))


(defun get-implicit-type (vble)
  (do ((decls *implicit_vble_decls* (cdr decls))
       (vble-str (aref (string vble) 0)))
      ((null decls) nil)
    (if
     (do ((vble-ranges (cdar decls) (cdr vble-ranges)))
	 ((null vble-ranges) nil)
       (if (or (and (cdar vble-ranges)
		    (string>= vble-str (string (caar vble-ranges)))
		    (string<= vble-str (string (cadar vble-ranges))))
	       (string= vble-str (string (caar vble-ranges))))
	   (return t)))
     (return (caar decls)))))


#+nil
(defun get_array_type (decl vble-is-formal-arg) 
  (prog (type)
      (return
       (cond ((and (member decl *common_array_dims*)
                   (setq type (car (member decl *explicit_vble_decls* :key #'caadr)))
                   (not (eq (car type) 'array)))
              (car type))
             ((setq type (get-implicit-type decl))
              type)
             ((default-int-p decl)
	      'integer4)
             (t
	      'single-float)))))

(defun get_array_type (decl vble-is-formal-arg) 
  (prog (type)
      (return
       (cond ((member decl *common_array_dims*)
	      (lookup-vble-type decl))
             ((setq type (get-implicit-type decl))
              type)
             ((default-int-p decl)
	      'integer4)
             (t
	      'single-float)))))

(defun default-int-p (vble)
   (and (string>= (string vble) (symbol-name :i))
        (string< (string vble) (symbol-name :o))))

;; Test whether or not a given symbol has been defined as an array.
(defun vble-is-array-p (v)
  ;;(format t "common_array_dims = ~A~%" *common_array_dims*)
  ;;(format t "declard_vbles     = ~A~%" *declared_vbles*)

  ;; If it's an explicitly declared name, look through the
  ;; declarations to see if there are dimensions associated.
  ;;
  ;; If it's not, try to see if it's in the list of comman arrays that
  ;; have been dimensioned.
  ;;
  ;; ### This is pretty gross!  Why can't we keep this info all in one
  ;; place?
  (when (member v *declared_vbles*)
    (do ((decls *explicit_vble_decls* (cdr decls)))
	((null decls) nil)
      (if (do ((vbles (cdar decls) (cdr vbles)))
	      ((null vbles) nil)
	    (if (and (eq v (caar vbles)) (cdar vbles))
		(return-from vble-is-array-p t)))
	  (return-from vble-is-array-p t))))
  ;; else check if v is a common variable and an array or has 2 declarations
  (or (member v *common_array_dims*)
      (vble-declared-twice-p v *explicit_vble_decls*)))



; test if a symbol is a function call rather than an undeclared vble
; used in check_new_vbles
(defun sym-is-fun-name (expr)
   (and (listp (cadr expr)) (not (null (cdr expr)))))

;; Test if a symbol is really an f2cl hacked number.  This only
;; happens when the number is in exponential form with a negative
;; exponent.
(defun sym-is-number-p (expr)
  (let* ((s (string expr))
	 (neg (position #\% s)))
    (when neg
      ;; Ok, it might be a f2cl hacked number which looks something like
      ;;
      ;; m.mmmD%xx Make sure the character before "%" is D or E, and
      ;; then make sure that the stuff before the exponent and after
      ;; the "%" is a valid number.
      (and (member (aref s (1- neg)) '(#\d #\D #\e #\E) :test #'char-equal)
	   (numberp (read-from-string (subseq s 0 (1- neg))))
	   (numberp (read-from-string (subseq s (1+ neg))))))))
      
; given a list of variables names proclaim them special
(defun make-special-proclamation (vars)
   (if vars `(proclaim '(special ,@vars)) nil))

; declare a special (common) variable
(defun make-special-var-decl (v)
  (let ((dim (member v *common_array_dims*)))
    (cond				; check if v is an array 
      (dim
       `(declare (type (,*array-type* ,(get_array_type v nil)
			,(f2cl-array-total-size (cadr dim))) 
		  ,(check-reserved-lisp-names v))))
					; else make ordinary declaration
      (t (make-declaration v)))))

(defparameter +reserved-lisp-names+
  '(t pi nil))

(defparameter +allowed-lisp-names+
  '(abs sin cos tan
    asin acos atan
    sinh cosh tanh
    exp sign max min 
    mod))

;; Check if the Fortran name would collide with Lisp names like T, PI,
;; NIL, FUNCALL, PROG, etc.  If it does, replace it a new name with a
;; trailing underscore.

#+nil
(defun check-reserved-lisp-names (x)
  (if (member x +reserved-lisp-names+)
      (intern (concatenate 'string (string x) "_"))
      x))

(defun check-reserved-lisp-names (x)
  (multiple-value-bind (found-it access)
      (find-symbol (string x) :common-lisp)
    (cond ((member x +allowed-lisp-names+)
	   x)
	  ((or (and found-it
		    (not (eq access :internal))
		    (fboundp found-it))
	       (member x +reserved-lisp-names+))
	   (intern (concatenate 'string (string x) "$")))
	  (t
	   x))))

;-----------------------------------------------------------------------------     
; functions for fixing DO and IF structures

; sort out do loops and if-then-else structures
(defun fix-structure (checklist labels) ; labels is the do labels
   (do ((retlist (list (third checklist) (second checklist) (first checklist))
                 (if clause (cons clause retlist) retlist))
        (exprs (cdddr checklist))
        (clause))
       ((null exprs) (reverse retlist))
       (cond ((listp (car exprs))
              (cond ((eq (caar exprs) 'if-then)
                     (multiple-value-setq (clause exprs) 
                                          (fix-ifthen exprs labels)))
                    ((eq (caar exprs) 'fdo)
                     (multiple-value-setq (clause exprs) (fix-do exprs labels)))
                    ((eq (caar exprs) 'continue_place_holder)
                     (setq clause nil
                           exprs (cdr exprs)))
                    (t (setq clause (car exprs)
                             exprs (cdr exprs)))))
            ((eq (car exprs) 'continue_place_holder)
              (setq clause nil
                    exprs (cdr exprs)))
            (t (setq clause (car exprs)
                     exprs (cdr exprs))))))

(defun fix-ifthen (checklist labels)
    (do ((clause (cdar checklist))
         (exprs (cdr checklist))
         (ret nil) (subclause))
        ((eq (car exprs) 'endif_place_holder)
         (setq ret (cons (reverse clause) ret))
         (values (append '(cond) (fix-tagbodies (reverse ret))) (cdr exprs)))
        (cond ((listp (car exprs))
               (cond ((eq (caar exprs) 'fdo)
                      (multiple-value-setq (subclause exprs) 
                                           (fix-do exprs labels))
                      (setq clause (cons subclause clause)))
                     ((eq (caar exprs) 'if-then)
                      (multiple-value-setq (subclause exprs) 
                                           (fix-ifthen exprs labels))
                      (setq clause (cons subclause clause)))
                     ((eq (caar exprs) 'elseif_place_holder)
                      (setq ret (cons (reverse clause) ret)
                            clause (list (cadar exprs))
                            exprs (cdr exprs)))
                     (t (setq clause (cons (car exprs) clause)
                              exprs (cdr exprs)))))
              ((eq (car exprs) 'continue_place_holder)
               (setq exprs (cdr exprs)))
              (t (setq clause (cons (car exprs) clause)
                       exprs (cdr exprs))))))


; compare FDO_BODY_LABELx with LABELy
; return t if x = y
(defun end-do-p (checklist do-label)
  (and (not (listp (car checklist)))
       (label-matches-dolabel-p (car checklist) do-label)))

; given (go labelx) does it match with current fdo_body_labely
(defun label-matches-dolabel-p (label do_label)
   (and (eq 5 (string-lessp (symbol-name :label) (string label)))
        (equal (string-left-trim (symbol-name :label) label)
               (string-left-trim (symbol-name :fdo_body_label) do_label))))

;------------------------------------------------------------------------------

(defun fix-do (checklist labels)
  (do ((do-expr (ldiff (car checklist) (last (car checklist))) )
       (loop-body '(tagbody) 
		  (if subclause (cons subclause loop-body) loop-body))
       (exprs (cdr checklist))
       (subclause nil)
       (do-label (car (last (car checklist)))))

      ((or (null exprs)
	   (end-do-p exprs do-label))
       (cond
	 ((null exprs)
	  (error "A DO statement without a matching label?!"))
	 ((multiple-do-labelp (car exprs) labels)
	  (values (append do-expr 
			  (list (reverse loop-body))) exprs))

	 ((not (eq (second exprs) 'continue_place_holder)) 
	  (values (append do-expr 
			  (list 
			   (reverse (cons (second exprs) 
					  (cons (intern (remove-prefix do-label)) loop-body))))) 
		  (cddr exprs))) 
	 (t 
	  (values (append do-expr 
			  (list 
			   (reverse (cons (intern (remove-prefix do-label)) loop-body)))) 
		  (cddr exprs))) ))
    ;; body
    (cond ((listp (car exprs))
	   (cond ((eq (caar exprs) 'if-then)
		  (multiple-value-setq (subclause exprs) 
		    (fix-ifthen exprs labels)))
		 ((eq (caar exprs) 'fdo)
		  (multiple-value-setq (subclause exprs) 
		    (fix-do exprs labels)))
		 ((and (eq (caar exprs) 'go)
		       (label-matches-dolabel-p (cadar exprs) do-label))
		  ;; Why do we treat a go to to the continue
		  ;; statement of the do loop differently?  Why
		  ;; "(return)" instead of "(go label)"
		  #+nil
		  (setq subclause '(return)
			exprs (cdr exprs))
		  (setq subclause (car exprs)
			exprs (cdr exprs)))
		 (t (setq subclause (car exprs)
			  exprs (cdr exprs)))))
	  ((eq (car exprs) 'continue_place_holder)
	   (setq subclause nil
		 exprs (cdr exprs)))
	  (t (setq subclause (car exprs)
		   exprs (cdr exprs))))))

(defun remove-prefix (x) (string-left-trim (symbol-name '#:fdo_body_) x))

(defun multiple-do-labelp (labelnnn labels)
  (< 1 (length (remove-if-not #'(lambda (x)
				  (label-matches-dolabel-p labelnnn 
							   (princ-to-string x)))
			      labels))))

;------------------------------------------------------------------------------

; if a cond clause contains a jump (from within to within) make a tagbody
(defun fix-tagbodies (cond-expr)
   (do ((clauses cond-expr (cdr clauses))
        (new-clauses nil (cons clause new-clauses))
        (clause nil))
       ((null clauses) (reverse new-clauses))
       (setq clause (car clauses)) ;(<pred> expr expr ....)
       (if (member-if #'symbolp (cdr clause))
           (setq clause `(,(car clause) (tagbody ,@(cdr clause)))))))

;----------------------------------------------------------------------------- 

(eval-when (compile load eval)  
  (proclaim '(special *format_stmts* *current_label* *SP* *dlist-flag*)))

(defun parse-format (x)
  (prog (*SP*)
   (declare (special *SP*))
   (setq *SP* nil)
   (setq *format_stmts* (cons (list *current_label*
                                 (parse-format1 (cadr x)))
                            *format_stmts*)))
)

;; x is of form: '(WRITE (* |,| 8000) |,| J |,| K)
;;           or  '(WRITE (*) |,| J |,| K)
;;           or  '(WRITE (* |,| *) |,| J |,| K)
;;
;; Note that the unit can be any arbitrary expression, so be careful.

(defun parse-write (x)
  ;; check for comma before arguments
  (if (and (third x) (not (eq (third x) '|,|)))
      (setq x (append (list (first x) (second x) '|,|) (cddr x))))
  ;; As shown above, (second x) will contain the unit number and the
  ;; format number, if any.  Split this at the comma to get the unit
  ;; part and the format part.
  (destructuring-bind (lun-part fmt-part)
      (list-split '|,| (second x))
    ;;(format t "lun-part = ~A~%" lun-part)
    ;;(format t "fmt-part = ~A~%" fmt-part)
    
    `((fformat ,(parse_format_dest lun-part)
       ,(if (null fmt-part) 
	    '(("~A~%"))
	    (get_format_stmt fmt-part))
       ,@(mapcar #'parse-output-argument 
		 (list-split '|,| (cdddr x)))))))

;; x is of the form: '(PRINT * |,| X |,| Y)
;;               or: '(PRINT 9000 |,| X |,| Y)

(defun parse-print (x)
  `((fformat t
     ,(get_format_stmt (list (second x)))
     ,@(mapcar #'parse-output-argument 
		       (list-split '|,| (cdddr x))))))

;; x is of the form '(read (lun |,| format) var |,| var)
;;
;; We're not trying to implement full Fortran read functionality.  We
;; just want to read data into the variables.  Implied do loops are
;; not supported.
#+nil
(defun parse-read (x)
  (append (list '(fortran_comment "***WARNING:  READ statement may not be translated correctly!"))
	  (mapcar #'(lambda (var)
		      `(setf ,(if (rest var)
				  var
				  (first var))
			(read)))
		  (remove nil (list-split '|,| (cddr x))))
	  (list '(fortran_comment "***WARNING: Preceding READ statements may not be correct!"))))

;; x is of the form (read (lun , fmt) <var-or-implied-do>)
#+nil
(defun parse-read (x)
  (labels ((handle-simple-var (expr)
	     (cond ((and (listp expr)
			 (eq (first expr) 'fref))
		    `(fset ,expr (read)))
		   ((and (symbolp expr)
			 (subtypep (lookup-vble-type expr) 'string))
		    `(f2cl-set-string ,expr (read) ,(lookup-vble-type expr)))
		   (t
		    `(setf ,expr (read)))))
	   (handle-implied-do (do-list)
	     ;; Like parse-implied-do
	     (let* ((ctrl-vars (member-if #'(lambda (x) (eq (second x) '=)) do-list))
		    (dlist (parse-dlist (ldiff do-list ctrl-vars)))
		    (ivar (first (first ctrl-vars)))
		    (e1 (id-expression (cdr (member '= (first ctrl-vars)))))
		    (e2 (id-expression (second ctrl-vars)))
		    (e3 (if (third ctrl-vars) (third ctrl-vars) 1)))
	       ;;(format t "do-list = ~A~%" do-list)
	       ;;(format t "dlist = ~S~%" dlist)
	       ;;(format t "*dlist-flag* = ~A~%" *dlist-flag*)
	       `(do ((,ivar ,e1 (+ ,ivar ,e3)))
			((> ,ivar ,e2))
		      (declare (type integer4 ,ivar))
		      ,@(mapcar #'(lambda (v)
				    `(fset ,v (read)))
				(cdr dlist)))))
	   (handle-var (arg)
	     (cond ((null arg) nil)
		   ((and (listp arg)
			 (listp (car arg))
			 (member '= (car arg)))
		    ;; Implied do
		    (handle-implied-do (list-split '|,| (car arg))))
		   (t
		    (handle-simple-var (id-expression arg))))))
    (append (list '(fortran_comment "***WARNING:  READ statement may not be translated correctly!"))
	    (mapcar #'handle-var
		    (remove nil (list-split '|,| (cddr x))))
	    (list '(fortran_comment "***WARNING: Preceding READ statements may not be correct!")))))

(defun parse-read (x)
  (let* ((read-opts (list-split '|,| (second x)))
	 (lun (caar read-opts)))
    ;;(format t "read-opts = ~S~%" read-opts)
    (labels ((handle-simple-var (expr)
	       (cond ((and (listp expr)
			   (eq (first expr) 'fref))
		      `(fset ,expr (read (f2cl-lib::lun->stream ,lun))))
		     ((and (symbolp expr)
			   (subtypep (lookup-vble-type expr) 'string))
		      `(f2cl-set-string ,expr (read (f2cl-lib::lun->stream ,lun))
					,(lookup-vble-type expr)))
		     (t
		      `(setf ,expr (read (f2cl-lib::lun->stream ,lun))))))
	     (handle-implied-do (do-list)
	       ;; Like parse-implied-do
	       (let* ((ctrl-vars (member-if #'(lambda (x) (eq (second x) '=)) do-list))
		      (dlist (parse-dlist (ldiff do-list ctrl-vars)))
		      (ivar (first (first ctrl-vars)))
		      (e1 (id-expression (cdr (member '= (first ctrl-vars)))))
		      (e2 (id-expression (second ctrl-vars)))
		      (e3 (if (third ctrl-vars) (third ctrl-vars) 1)))
		 ;;(format t "do-list = ~A~%" do-list)
		 ;;(format t "dlist = ~S~%" dlist)
		 ;;(format t "*dlist-flag* = ~A~%" *dlist-flag*)
		 `(do ((,ivar ,e1 (+ ,ivar ,e3)))
		      ((> ,ivar ,e2))
		    (declare (type integer4 ,ivar))
		    ,@(mapcar #'(lambda (v)
				  `(fset ,v (read (f2cl-lib::lun->stream ,lun))))
			      (cdr dlist)))))
	     (handle-var (arg)
	       (cond ((null arg) nil)
		     ((and (listp arg)
			   (listp (car arg))
			   (member '= (car arg)))
		      ;; Implied do
		      (handle-implied-do (list-split '|,| (car arg))))
		     (t
		      (handle-simple-var (id-expression arg))))))
      ;; Only handle the simple case of read(<lun>,...)
      (append (list '(fortran_comment "***WARNING:  READ statement may not be translated correctly!"))
	      (mapcar #'handle-var
		      (remove nil (list-split '|,| (cddr x))))
	      (list '(fortran_comment "***WARNING: Preceding READ statements may not be correct!"))))))
  

;; x is (OPEN (lun |,| <open-keywords)
;;   or (OPEN (unit = <x> |,| <open-keywords>))
(defun parse-open (x)
  (let ((options (list-split '|,| (second x))))
    (when (= (length (car options)) 1)
      ;; We have just a unit number, instead of "unit = lun".  Convert
      ;; it to the latter form.
      (setf (car options) `(unit = ,(caar options))))
    ;; Convert the list of options into Lisp-style keyword options.
    `((f2cl-lib::open-file
       ,@(flatten-list
	  (mapcar #'(lambda (opt)
		      (destructuring-bind (key = val)
			  opt
			(declare (ignore =))
			`(,(intern (string-upcase (string key)) :keyword)
			   ,val)))
		  options))))))

(defun parse-rewind (x)
  (let ((options (if (listp (second x))
		     (flatten-list
		      (mapcar #'(lambda (opt)
				  (destructuring-bind (key = val)
				      opt
				    (declare (ignore =))
				    `(,(intern (string-upcase (string key)) :keyword)
				       ,val)))
			      (list-split '|,| (second x))))
		     `(:unit ,(second x)))))
    `((f2cl-lib::rewind ,@options))))

(defun parse-close (x)
  (let ((options (if (= (length (second x)) 1)
		     `(:unit ,(first (second x)))
		     (flatten-list
		      (mapcar #'(lambda (opt)
				  (destructuring-bind (key = val)
				      opt
				    (declare (ignore =))
				    `(,(intern (string-upcase (string key)) :keyword)
				       ,val)))
			      (list-split '|,| (second x)))))))
    `((f2cl-lib::close$ ,@options))))

;; Handle things like FMT = number
(defun get_format_stmt (label)
  (let ((fmt-num (first label)))
    (cond ((eq fmt-num 'fmt)
	   ;; We have something like "FMT = number"
	   (get_format_stmt (list (third label))))
	  ((or (eq fmt-num '*)
	       (not (numberp fmt-num)))
	   '(("~A~%")))
	  (t
	   (do ((lis *format_stmts* (cdr lis)))
	       ((null lis)
		(error "Format statement ~A not found" fmt-num))
	     (if (equal fmt-num (caar lis))
		 (return (cadar lis))))))))

;; Figure out where we're trying to WRITE to.
;;
;; DEST can be any arbitrary expression, so we need to parse it. 
(defun parse_format_dest (dest)
  (cond ((null (rest dest))
	 (if (eq (first dest) '*) t (first dest)))
	(t
	 (id-expression dest))))

(defun parse-output-argument (arg) 
   (cond ((null arg) nil)
         ((and (listp arg) (listp (car arg)) (member '= (car arg)))  ;implied-do
          (parse-implied-DO (list-split '|,| (car arg))))
         ((and (listp arg) (every #'listp arg)) ; dlist
          (parse-dlist arg))
         (t (id-expression arg))))

(defun parse-implied-do (do-list)
   (prog (ctrl-vars dlist ivar e1 e2 e3 (*dlist-flag* nil))
      (setq ctrl-vars (member-if #'(lambda (x) (eq (second x) '=)) do-list)
            dlist (parse-dlist (ldiff do-list ctrl-vars))
            ivar (first (first ctrl-vars))
            e1 (id-expression (cdr (member '= (first ctrl-vars))))
            e2 (id-expression (second ctrl-vars))
            e3 (if (third ctrl-vars) (third ctrl-vars) 1))
      (return
      (if *dlist-flag*
      `(do ((,ivar ,e1 (+ ,ivar ,e3))
            (ret nil (append ret ,dlist)))
           ((> ,ivar ,e2) ret)
	(declare (type integer4 ,ivar)))

      `(do ((,ivar ,e1 (+ ,ivar ,e3))
            (ret nil (cons ,dlist ret)))
           ((> ,ivar ,e2) (reverse ret))
	(declare (type integer4 ,ivar)))))))

(defun parse-dlist (x)
   (setq *dlist-flag* t)
   `(list ,@(mapcar #'parse-output-argument x)))

;--------------------------------------------------------------------------------

; convert an array to a list
; this is possibly implementation dependent
; but is not actually used by the translator
(defun array-list (array)
  (prog (str i dim old-print-length old-print-level old-print-array)
   (setq old-print-array *print-array*
         old-print-length *print-length*
         old-print-level *print-level*
         *print-array* t
         *print-length* nil
         *print-level* nil)
   (setq str (princ-to-string array)
         dim (length str))
   (setq i (do ((i 0 (1+ i)))
               ((eq (schar str i) #\() i)))
   (setq str
         (read-from-string (make-array (- dim i) :displaced-to str 
                                                 :displaced-index-offset i
                                                 :element-type 'character)))
   (setq *print-array* old-print-array
         *print-length* old-print-length
         *print-level* old-print-level)
   (return str)))

;--------------------------------------------------------------------------------

(defun parse-format1 (format &optional (newlinep t))
  (let ((x (list-split '|,| (fix-slashes format)))
	*scale-factor*)
    (declare (special *scale-factor*))
    ;; It's valid to have a format like "1p,e15.8" instead of
    ;; "1pe15.8".  So the variable *scale-factor* is used to handle
    ;; this case.  The parser for P sets *scale-factor* appropriately
    ;; for use by other edit descriptors.
    (do ((desc-lists x (cdr desc-lists))
	 (directive) (dl)
	 (directive-list nil (append directive-list directive)))
	((null desc-lists)
	 (if newlinep
	     (append directive-list '("~%"))
	     directive-list))
      (setq dl (car desc-lists))
      (setq directive 
	    (cond ((null dl) nil)	;(list nil))
		  ((stringp (car dl))
		   (list (car dl)))
		  ((listp (car dl))
		   ;; Repeat group forever, as signaled by the rep factor of T.
		   (append  '(t) (list (parse-format1 (car dl) nil))))
		  ((and (numberp (car dl))
			(listp (cadr dl)))
		   ;; Repetition of a group
		   (append (list (car dl))
			   (list (parse-format1 (cadr dl) nil))))
		  ((numberp (car dl))
		   ;; Simple repeated format descriptor (Handles the
		   ;; case where the repetition factor is separated
		   ;; from the format descriptor.  The case when the
		   ;; repetition factor is NOT separated is handled
		   ;; below.
		   (parse-format-descriptor-list (car dl) 
						 (destruct-descriptor-list (cdr dl))))
		  (t (parse-format-descriptor-list 1 
						   (destruct-descriptor-list dl))))))))

;--------------------------------------------------------------------------------

;;; Given a symbol e.g I4 or P2G10.4E2 this function seperates it into
;;; numbers and symbols eg (#\I 4) or (#\P 2 #\G 10 4 #\E 2).  The characters
;;; are converted to upper case.

(defun destruct-descriptor-list (x)
  (let (descriptor-list char atm)
    (cond ((listp x)
	   (setq x (make-string-input-stream
		    (reduce #'(lambda (r s)
				(concatenate 'string r " " s))
			    (mapcar #'symbol-name x)))))
	  ((symbolp x)
	   (setq x (make-string-input-stream (symbol-name x))))
	  (t
	   (return-from destruct-descriptor-list x)))
    (loop
	(setq char (peek-char nil x nil :eof nil))
	(when (equal char :eof)
	  (return-from destruct-descriptor-list (reverse descriptor-list)))
      (cond ((or (digit-char-p char)
		 (member char '(#\+ #\-)))
	     (multiple-value-setq (atm x)
	       (read-number-from-stream x)))
	    ((eq char #\.)
	     (read-char x)
	     (setq atm nil))
	    (t
	     (setq atm (char-upcase (read-char x)))))
      (when atm
	(setq descriptor-list (cons atm descriptor-list))))))

(defun read-number-from-stream (x)
   (prog ((number-str "") char)
     loop
   (setq char (peek-char nil x nil 'eof nil))
   (if (equal char 'eof) (return (values (read-from-string number-str) x)))
   (if (or (digit-char-p char) (member char '(#\+ #\-)))
       (setq number-str (concatenate `string number-str (string (read-char x))))
       (return (values (read-from-string number-str) x)))
   (go loop)))

;--------------------------------------------------------------------------------
; pattern matching functions adapted from those in f3.l

; bind matched atoms to A etc if the pattern has (> A)
; bind matched groups of atoms to B if the pattern has (+ B)
; the eval obtains the list of atoms seen so far and the cons adds the atom
; matched at the current level
; e.g. > (setq x  '(#\P 2 #\G 10 4 #\E 2))
;      > (pattern-match-and-bind '((+ left) #\G (+ right)) x)
;      binds (#\P 2) to left and (10 4 #\E 2) to right
;      > (pattern-match-and-bind '(#\P (> A) #\G (> B) (> C) (+ D)) x)
;      binds 2 to A, 10 to B, 4 to C and '(#\E 2) to D

(defun pattern-match-and-bind (p d)
      (cond ((and (null p) (null d)) t)
            ((or (null p) (null d)) nil)   
            ((or (equal (car p) '>)       
                 (equal (car p) (car d)))
             (pattern-match-and-bind (cdr p) (cdr d)))
            
            ((and (listp (car p))
                  (equal (caar p) '>)  
                  (pattern-match-and-bind (cdr p) (cdr d)))
             (set (cadar p) (car d)) t) 
            ((equal (car p) '+)                 
             (cond ((pattern-match-and-bind (cdr p) (cdr d)))   
                   ((pattern-match-and-bind p (cdr d)))))
            ((and (listp (car p))
                  (equal (caar p) '+))
             (cond ((pattern-match-and-bind (cdr p) (cdr d))
                    (set (cadar p) (list (car d))) t) ; building begins
                   ((pattern-match-and-bind p (cdr d))
                    (set (cadar p)
                         (cons (car d) (eval (cadar p)))) t)))))

;;--------------------------------------------------------------------------------
;; functions for parsing format stmts which are stored with their labels
;; on a global list (referenced by calls to WRITE)

(defun parse-format-descriptor-list (a x)
  (let ((matched-p nil)
	left right)
    (declare (special left right *scale-factor*))
    (if (numberp (car x))
	nil
	(setq x (cons a x)))
    ;; find most significant descriptor and parse
    (let ((result
	   (cond
	     ;;((typep (cdr x) 'string)
	     ;; (cdr x))
	     ((pattern-match-and-bind '((+ left) #\H (+ right)) x)
	      (parse-format-descriptor-H left right))
	     ((pattern-match-and-bind '((> left) #\I (+ right)) x)
	      (parse-format-descriptor-I left right))
	     ((pattern-match-and-bind '((+ left) #\F (+ right)) x)
	      (parse-format-descriptor-F left right))
	     ((pattern-match-and-bind '((+ left) #\G (+ right)) x)
	      (parse-format-descriptor-G left right))
	     ((pattern-match-and-bind '((+ left) #\E (+ right)) x)
	      (parse-format-descriptor-E left right))
	     ((pattern-match-and-bind '((+ left) #\D (+ right)) x)
	      (parse-format-descriptor-D left right))
	     ((pattern-match-and-bind '((> left) #\T #\R (+ right)) x)
	      (parse-format-descriptor-TR right))
	     ((pattern-match-and-bind '((> left) #\X) x)
	      (parse-format-descriptor-X left))
	     ((pattern-match-and-bind '((> left) #\P) x)
	      (setf matched-p t)
	      (parse-format-descriptor-P left))
	     ((pattern-match-and-bind '((> left) #\S (> right)) x)
	      (parse-format-descriptor-S right))
	     ((pattern-match-and-bind '((> left) #\A (+ right)) x)
	      (parse-format-descriptor-A left right))
	     ((equal (cadr x) '#\/)
	      (parse-format-descriptor-/))
	     (t (parse-default-format-descriptor x)))))
      (unless matched-p
	;; Need to reset *scale-factor* if this wasn't a P descriptor.
	(setf *scale-factor* nil))
      result)))

(defun fixnum-string (x)
   (princ-to-string x))

(defun parse-format-descriptor-A (a w)
  (let* ((width (if (listp w)
		   (car w)
		   w))
	(directive (list (format nil "~~~DA" width))))
    (list a (list directive))))

;; Handle Fortran Iw.m format
(defun print-i-format (ostream val colonp at-sign-p width pad-width)
  (declare (ignore colonp at-sign-p))
  (let ((output (format nil "~V,'0D" width val)))
    ;; Converting leading zeros to blanks as needed
    (dotimes (k (- width pad-width))
      (setf (aref output k) #\space))
    (princ output ostream)
    (values)))

(defun parse-format-descriptor-I (a w)
  ;; aIw.m
  ;;
  ;; According to the Fortran standard, if m is given, the output is
  ;; zero-padded on the left to occupy m positions.  In addition if m
  ;; = 0, and the number is 0, we aren't supposed to output
  ;; anything. We don't handle that here.  It's also unclear to me
  ;; what happens if m < w, but I think that means the number is
  ;; zero-padded to a width of m and printed right-justified in a
  ;; field of width w.
  (let* ((width (if (listp w)
		   (car w)
		   w))
	(directive (if (and (listp w) (second w))
		       (format nil "~~~D,~D/f2cl:print-i-format/" width (second w))
		       (format nil "~~~DD" width)
		       )))
    (list a (list (list directive)))))

;<kP><a>Fw.d -> ~a{~w,d,k,,,[@]F}
(defun parse-format-descriptor-F (left right)
  (let ((k 0)
	(a 1))
    (declare (special k a *scale-factor*))
    (or (pattern-match-and-bind '((> k) #\P (> a)) left)
	(pattern-match-and-bind '((> k) #\P) left)
	(pattern-match-and-bind '((> a)) left))
    (when *scale-factor*
      (setf k *scale-factor*))
    (let ((directive
	   (list (concatenate 'string 
			      "~" (fixnum-string (car right)) "," ;w
			      (fixnum-string (cadr right)) "," ;d
			      (fixnum-string k) "," ;k
			      "'*,"
			      (if *SP* "@F" "F")))))
      (list a (list directive)))))

;; Note: The Fortran standard says that, for a format like Ew.d, if
;; the the exponent is three digits long, the exponent indicator (E or
;; is not printed.  Thus instead of xxxE+ddd, we get xxx+ddd.  We
;; don't handle that here!
(defun parse-format-descriptor-E (left right)
  (let ((k 0) (a 1) w d (e 2))
    (declare (special k a w d e *scale-factor*))
    (or (pattern-match-and-bind '((> k) #\P (> a)) left)
	(pattern-match-and-bind '((> k) #\P) left)
	(pattern-match-and-bind '((> a)) left))
    (or (pattern-match-and-bind '((> w) (> d)) right)
	(pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
    (when *scale-factor*
      (setf k *scale-factor*))
    (let ((directive 
	   (list (concatenate 'string
			      "~" (fixnum-string w) ","
			      (fixnum-string d) ","
			      (fixnum-string e) ","
			      (fixnum-string k) ",'*,,'E"
			      (if *SP* "@E" "E")))))
      (list a (list directive)))))

(defun parse-format-descriptor-D (left right)
  (let ((k 0) (a 1) w d (e 2))
    (declare (special k a w d e *scale-factor*))
    (or (pattern-match-and-bind '((> k) #\P (> a)) left)
	(pattern-match-and-bind '((> k) #\P) left)
	(pattern-match-and-bind '((> a)) left))
    (or (pattern-match-and-bind '((> w) (> d)) right)
	(pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
    (when *scale-factor*
      (setf k *scale-factor*))
    (let ((directive 
	   (list (concatenate 'string
			      "~" (fixnum-string w) ","
			      (fixnum-string d) ","
			      (fixnum-string e) ","
			      (fixnum-string k) ",'*,,'D"
			      (if *SP* "@E" "E")))))
      (list a (list directive)))))

(defun parse-format-descriptor-G (left right)
  (let ((k 0) (a 1) w d (e 2))
    (declare (special k a w d e *scale-factor*))
    (or (pattern-match-and-bind '((> k) #\P (> a)) left)
	(pattern-match-and-bind '((> k) #\P) left)
	(pattern-match-and-bind '((> a)) left))
    (or (pattern-match-and-bind '((> w) (> d)) right)
	(pattern-match-and-bind '((> w) (> d) #\E (> e)) right))
    (when *scale-factor*
      (setf k *scale-factor*))
    (let ((directive 
	   (list (concatenate 'string
			      "~" (fixnum-string w) ","
			      (fixnum-string d) ","
			      (fixnum-string e) ","
			      (fixnum-string k) ",'*"
			      (if *SP* "@G" "G")))))
      (list a (list directive)))))

(defun parse-format-descriptor-/ ()
   '("~%"))

(defun parse-format-descriptor-TR (s)
   (list (concatenate 'string "~" (fixnum-string s) "@T")))


(defun parse-format-descriptor-X (w)
   (list (concatenate 'string "~" (fixnum-string w) "@T")))

(defun parse-format-descriptor-P (w)
  (declare (special *scale-factor*))
  ;; Save the scale-factor away so we can use it later.
  (setf *scale-factor* (fixnum-string w))
  nil)

(defun parse-format-descriptor-S (right)
   (setq *SP* (if (eq right #\P) t nil))
   "")

(defun parse-format-descriptor-H (width string)
  (declare (ignore width))
  (list (coerce (mapcar #'(lambda (char-or-digit)
			    (cond ((numberp char-or-digit)
				   (aref (princ-to-string char-or-digit) 0))
				  ((symbolp char-or-digit)
				   (aref (symbol-name char-or-digit) 0))
				  (t
				   char-or-digit)))
			string)
		'string)))

(defun parse-default-format-descriptor (x)
   (list (do ((i 1 (1+ i))
              (ret nil (append ret '("~A"))))
             ((> i (car x)) ret))))

;; given a list containing /'s put commas on either side
;; e.g. (x / y) -> ( |,| / |,| y) 
;;      (/ x) -> (/ |,| x)
;;
;; Note that f2cl will convert // to f2cl-//, so if we find that, we
;; need to insert the appropriate number of commas and slashes.
(defun fix-slashes (x)
  (do ((lis x (cdr lis))
       (ret nil 
	    (append ret (cond ((eq (car lis) 'f2cl-//)
			       (cond ((and ret (cdr lis))
				      '(|,| / |,| / |,|))
				     ((cdr lis)
				      '(/ |,| / |,|))
				     (t
				      '(|,| / |,| /))))
			      ((eq (car lis) '/) 
			       (cond ((and ret (cdr lis))
				      '(|,| / |,|))
				     ((cdr lis)
				      '(/ |,|))
				     (t '(|,| /))))
			      (t
			       (list (car lis)))))))
      ((null lis) ret)))


;; Creates the slots for the common block.  Each variable in the
;; common block is a slot.  We initialize the slot with the
;; appropriate value and type.  Even if the array dimensions are
;; known, we don't use them here, just in case the dimensions contain
;; Fortran PARAMETER values.
(defun make-common-block-vars (varlist common_var_decls)
  ;; VARLIST is the list of variables in the common block.
  ;; COMMON_VAR_DECLS is a set of declaims computed in
  ;; INSERT-DECLARATIONS.  We use that to determine the appropriate
  ;; initalizations for the variables.
  (if *common-blocks-as-arrays*
      (make-common-block-vars-as-array varlist common_var_decls)
      (mapcar #'(lambda (var)
		  (let* ((decl (find var (rest common_var_decls)
				     :key #'third))
			 (var-type (if decl (second decl) nil))
			 (dims (cond ((and (listp var-type)
					   (subtypep var-type 'array)
					   (not (subtypep var-type 'string)))
				      ;;(format t "array var-type = ~A ~A~%" var var-type)
				      ;;(format t "lookup-array-bounds ~A = ~A~%" var (lookup-array-bounds var))

				      ;; If the dimension of the array is
				      ;; a number, use the actual
				      ;; dimension.  Otherwise use 0.
				      (if (every #'numberp (third var-type))
					  (third var-type)
					  0))
				     (t nil)))
			 (var-init (cond ((and (listp var-type)
					       (subtypep (first var-type) 'array))
					  `(make-array ',dims :element-type ',(second var-type)
						       :initial-element ,(if (subtypep (second var-type) 'logical)
									     nil
									     (coerce 0 (second var-type)))))
					 ((subtypep var-type 'logical)
					  nil)
					 (t
					  (coerce 0 var-type)))))
		    ;; Initialize the slot with a 0 of the appropriate
		    ;; type (for scalars) or a zero element array of the
		    ;; appropriate dimensions for array slots.
		    #+nil
		    (when dims
		      (format t "dims = ~A~%" dims)
		      (format t "new dims = ~A~%" (subst '(*) 0 dims))
		      (format t "relaxed  = ~A~%" (make-list (length dims) :initial-element '*)))
		    (if dims
			`(,var ,var-init
			       :type (,*array-type* ,(second var-type)
						    ,(if *relaxed-array-decls*
							 (make-list (if (listp dims)
									(length dims)
									1)
								    :initial-element '*)
							 (subst '(*) 0 dims))))
			`(,var ,var-init :type ,var-type))))
	      varlist)))

(defun make-common-block-vars-as-array (varlist common_var_decls)
  (let ((part 0)
	(prev-type nil)
	(total-len 0)
	(slots nil))
    (dolist (v varlist)
      (let* ((decl (find v (rest common_var_decls) :key #'third))
	     (var-type (if decl (second decl) nil))
	     (el-type (if (subtypep var-type 'array)
			  (second var-type)
			  var-type))
	     #+nil
	     (dims (cond ((and (listp var-type)
			       (subtypep var-type 'array)
			       (not (subtypep var-type 'string)))
			  ;;(format t "array var-type = ~A ~A~%" v var-type)
			  ;;(format t "lookup-array-bounds ~A = ~A~%" v (lookup-array-bounds v))

			  ;; If the dimension of the array is
			  ;; a number, use the actual
			  ;; dimension.  Otherwise use 0.
			  (if (every #'numberp (third var-type))
			      (third var-type)
			      0))
			 (t nil))))
	(unless prev-type
	  (setf prev-type el-type))
	(let ((len (if (subtypep var-type 'array)
			      (first (third var-type))
			      1)))
	  (cond ((and (subtypep prev-type el-type)
		      (subtypep el-type prev-type))
		 ;; Keep accumulating
		 (incf total-len len))
		(t
		 ;; Different type.  Stop accumulating
		 ;;(format t "Part ~A:  Len ~A, type ~A ~%" part total-len prev-type)
		 (push `(,(intern (format nil "PART-~D" part))
			  (make-array ,total-len :element-type ',prev-type)
			  :type (simple-array ,prev-type (,total-len)))
		       slots)
		 (setf prev-type el-type)
		 (incf part)
		 (setf total-len len))))
	;;(format t "var = ~A :type ~A :el-type ~A :dims ~A~%" v var-type el-type dims)
	))
    (when prev-type
      ;;(format t "Part ~A:  Len ~A, type ~A~%" part total-len prev-type)
      (push `(,(intern (format nil "PART-~D" part))
	      (make-array ,total-len :element-type ',prev-type)
	      :type (simple-array ,prev-type (,total-len)))
	    slots))
    (nreverse slots)))
			     
;; Create a structure for the given common blocks.
(defun make-common-block-structure (common_var_decls)
  (let ((res '()))
    (maphash #'(lambda (key varlist)
		 ;;(format t "key varlist = ~S ~S~%" key varlist)
		 ;;(format t "res = ~S~%" res)
		 ;; The varlist looks something like (v1 v2 v3
		 ;; (v3-dims) v4 (v4-dims)).  That is if the variable
		 ;; is an array, the following tiem in varlist gives
		 ;; the dimensions of the array.
		 (push `(defstruct (,key (:predicate ,(intern (concatenate 'string
									   (symbol-name '#:is-)
									   (symbol-name key)
									   (symbol-name '#:-p)))))
			 ,@(make-common-block-vars varlist common_var_decls))
		       res))
	     *common-blocks*)
    (nreverse res)))

(defun make-common-block-var-init (varlist common_var_decls)
  (flet ((fixup-bounds (bounds)
	   (mapcar #'(lambda (bound)
		       (if (every #'numberp bound)
			   (1+ (- (second bound) (first bound)))
			   `(1+ (- ,(second bound) ,(first bound)))))
		   bounds)))
    (let ((initializer '()))
      (dolist (var varlist)
	(let* ((decl (find var (rest common_var_decls)
			   :key #'third))
	       (var-type (if decl (second decl) nil))
	       (dims (cond ((and (listp var-type)
				 (subtypep (first var-type) 'array)
				 (not (subtypep var-type 'string)))
			    (fixup-bounds (lookup-array-bounds var)))
			   (t nil)))
	       (var-init (cond ((and (listp var-type)
				     (subtypep (first var-type) 'array))
				;; If the dimensions are numbers,
				;; we've already initialized the array
				;; in the structure definition.
				(unless (every #'numberp dims)
				  `(make-array (list ,@dims) :element-type ',(second var-type)
				  :initial-element ,(cond ((subtypep (second var-type) 'logical)
							   nil)
							  (t
							   (coerce 0 (second var-type)))))))
			       ((subtypep var-type 'logical)
				nil)
			       (t
				(coerce 0 var-type)))))
	  (when (and dims var-init)
	    (setf initializer (append initializer
				      `(,(intern (symbol-name var) :keyword) ,var-init))))))
      initializer)))

;; This assigns the common block structure to a global variable.  The
;; common block is initialized with reasonable defaults.  The
;; KEY-PARAMS is needed, just in case arrays in the common block are
;; dimensioned from PARAMETER values.
(defun make-common-block-init (comm-blocks comm-decls key-params key-param-decls)
  ;; COMM-BLOCKS is the hash table of all common blocks.  COMM-DECLS
  ;; is the list of declaims for the variables in the common block.
  ;; KEY-PARAMS is the list of Fortran PARAMETER values.
  ;; KEY-PARAM-DECLS is the appropriate set of declarations for the
  ;; PARAMTERs.
  (let ((var-inits '()))
    (maphash
     #'(lambda (key val)
	 (push `(let* ,key-params
		  ,@key-param-decls
		  (defparameter ,(intern (concatenate 'string
						      "*"
						      (symbol-name key)
						      (symbol-name '#:-common-block*)))
		    (,(intern (concatenate 'string
					   (symbol-name '#:make-)
					   (symbol-name key)))
		      ,@(make-common-block-var-init val comm-decls))))
	       var-inits))
     comm-blocks)
    (nreverse var-inits)))
;;;-----------------------------------------------------------------------------
;;; end of f2cl5.l
;;;
;;; $Id: f2cl5.l,v 1.152 2006/11/28 19:04:07 rtoy Exp $
;;; $Log: f2cl5.l,v $
;;; Revision 1.152  2006/11/28 19:04:07  rtoy
;;; o fchar and cmplx shouldn't be Fortran variables.
;;; o Clean up some compiler warnings about unused vars and functions.
;;;
;;; Revision 1.151  2006/11/28 17:43:53  rtoy
;;; f2cl-// can't be a Fortran variable.
;;;
;;; Revision 1.150  2006/11/26 04:43:55  rtoy
;;; Replace %false% and %true% in a different way and fix a typo.
;;;
;;; Revision 1.149  2006/11/24 05:06:58  rtoy
;;; Logical arrays were not getting initialized from data statements.
;;;
;;; Revision 1.148  2006/05/04 19:10:11  rtoy
;;; We were not correctly testing for the variable types in the common
;;; block when we are using common-as-array feature.  This was caused by a
;;; previous change where our type declarations sometimes come out as
;;; (type (double-float) foo) instead of (type double-float foo).
;;;
;;; This fix allows odepack to compile and run again.
;;;
;;; Revision 1.147  2006/05/03 20:06:42  rtoy
;;; Was not correctly handling handling the dimensions for a logical
;;; array when computing the declaration for the array.
;;;
;;; Revision 1.146  2006/05/03 17:37:23  rtoy
;;; o A better implementation, I think, of the separate nP edit
;;;   descriptor.
;;; o Forgot to declare *scale-factor* as special in the F, D, and G
;;;   descriptor parser functions.
;;;
;;; Revision 1.145  2006/05/03 17:22:46  rtoy
;;; src/f2cl5.l
;;; o We weren't handling formats like "1P,E15.8", which is the same as
;;;   "1PE15.8".  Make this work.  We make a note of the fact that we have
;;;   a nP descriptor, and the following descriptor uses it.  Afterwords,
;;;   the scale-factor is reset.
;;;
;;; packages/homepack/mains.f:
;;; o Revert the 1P change because f2cl handles this now.
;;;
;;; Revision 1.144  2006/05/03 02:31:10  rtoy
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
;;; Revision 1.143  2006/05/02 22:17:17  rtoy
;;; Clean declarations for parameters by combining all declarations int
;;; one.
;;;
;;; Revision 1.142  2006/05/02 22:12:02  rtoy
;;; src/f2cl5.l:
;;; o Try to make better declarations for variables defined in parameter
;;;   statements.  We'll declare them as (double-float 42d0 42d0) if the
;;;   parameter was initialized to 42d0.
;;; o MAKE-DECLARATION updated to take an extra keyword argument to
;;;   indicate if this is a parameter variable and to give the initial
;;;   value of the parameter so we can make the appropriate declaration.
;;; o When initializing simple variables in data statements, try to bind
;;;   the variable with the initial value instead binding a default 0 zero
;;;   and setq'ing it later.
;;;
;;; src/macros.l:
;;; o Change DEFTYPE for INTEGER4 to allow parameters so we can specify
;;;   tight bounds if desired.
;;;
;;; Revision 1.141  2006/05/01 17:38:27  rtoy
;;; Replace some uses of FSET with plain ol' SETF because SETF does
;;; everything we want it to do.  But leave some FSET's around because we
;;; need them later to generate initializers for DATA statements, and
;;; such.
;;;
;;; Revision 1.140  2006/04/28 13:28:39  rtoy
;;; Add type-derivation for ABS.  Otherwise, we end up with COERCE calls
;;; wherever ABS is used.
;;;
;;; Revision 1.139  2006/04/28 01:36:13  rtoy
;;; If there are no array data forms, don't emit a WITH-MULTI-ARRAY-DATA
;;; form either.
;;;
;;; Revision 1.138  2006/04/27 17:44:01  rtoy
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
;;; Revision 1.137  2006/01/31 15:09:25  rtoy
;;; Try to return a 1-D array declaration when possible.
;;;
;;; Revision 1.136  2006/01/30 21:21:25  rtoy
;;; o Fix bug in MERGE-DATA-AND-SAVE-INITS.  For multidimensional arrays
;;;   that were fully initialized with data statements, f2cl was
;;;   forgetting to leave the fsets around to initialize them, and thus,
;;;   the arrays were never actually initialized.
;;; o Extend MERGE-DATA-AND-SAVE-INITS to support multidimensional arrays
;;;   that are fully initialized.
;;;
;;; Revision 1.135  2006/01/12 17:19:03  rtoy
;;; F2CL can handle equivalences of 2 simple variables of the same type.
;;;
;;; Revision 1.134  2006/01/11 22:57:58  rtoy
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
;;; Revision 1.133  2006/01/11 16:30:00  rtoy
;;; Allow implied-do loops in read statements.  Still has the f2cl
;;; limitations with read, but at least they're parsed and converted into
;;; a loop that reads into the variables.
;;;
;;; Revision 1.132  2006/01/09 00:37:43  rtoy
;;; src/f2cl5.l:
;;; o When looking for initializers, don't just remove initializers when
;;;   the array is not a 1-D array.  Keep them, and return a second value
;;;   indicating if the array is 1-D or not.
;;; o MAKE-CHAR-DECL was not properly declaring and initializing 2-D
;;;   arrays as 1-D arrays like we're supposed to.  Compute the total size
;;;   of the array if we can.
;;;
;;; src/macros.l:
;;; o F2CL-INIT-STRING needs to make a 1-D array, even if the string array
;;;   is multi-dimensional.
;;;
;;; Revision 1.131  2006/01/04 17:53:40  rtoy
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
;;; Revision 1.130  2005/07/26 12:45:53  rtoy
;;; Oops.  Need to make the declaration anyway because we need to figure
;;; out the type of the variable.
;;;
;;; Revision 1.129  2005/07/17 02:17:42  rtoy
;;; Don't try to declare a function if the function was a parameter.  We don't really know anything about the function.
;;;
;;; Revision 1.128  2005/07/16 22:43:48  rtoy
;;; Make sure entry points are also entered into the function database.
;;;
;;; Revision 1.127  2005/07/16 21:00:19  rtoy
;;; In CHECK_NEW_VBLES, we need to add a special case for an expression
;;; containing ARRAY-SLICE because we don't want to add ARRAY-SLICE and
;;; the array type as new variables.  Are there other special cases?
;;;
;;; Revision 1.126  2005/07/14 21:38:58  rtoy
;;; o Change default array-type in F2CL to be the same as F2CL-COMPILE.
;;;
;;; o Add support for some EQUIVALENCE statements.  We can handle
;;;   equivalence statements that equivalence an array (element) to a
;;;   simple variable of the same type.  Everything else will cause an
;;;   error.  This is much better than putting a silly "not-translated"
;;;   string into the generated lisp file.
;;;
;;; Revision 1.125  2005/06/20 01:53:39  rtoy
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
;;; Revision 1.124  2005/05/26 19:18:00  rtoy
;;; Oops.  Remove some extraneous debugging outputs.
;;;
;;; Revision 1.123  2005/05/26 16:00:40  rtoy
;;; Don't create a declaration for a subprogram parameter if that
;;; parameter is also used as a function.  This happens if the function
;;; isn't declared external.
;;;
;;; Revision 1.122  2005/03/28 20:38:02  rtoy
;;; Make strings with an element-type of character instead of base-char,
;;; in case the Lisp implementation has unicode support.
;;;
;;; Revision 1.121  2004/08/15 11:16:14  rtoy
;;; Don't want that eval-when there.
;;;
;;; Revision 1.120  2004/08/14 19:27:41  rtoy
;;; Try to clean up code a little.
;;;
;;; Revision 1.119  2004/08/14 16:10:10  rtoy
;;; Forgot to create the symbol-macrolet for arrays in the common block,
;;; when using common-blocks-as-arrays.
;;;
;;; Revision 1.118  2004/08/14 04:15:56  rtoy
;;; o GET_ARRAY_TYPE was not computing the array type in some situations.
;;;   (I hope this is the right fix.)
;;; o Was not counting the array lengths correctly.
;;;
;;; Revision 1.117  2004/08/13 21:16:28  rtoy
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
;;; Revision 1.116  2003/11/15 14:16:45  rtoy
;;; When parsing READ, if the variable has type STRING, we need to use
;;; f2cl-set-string to make sure strings get the right length.
;;;
;;; Revision 1.115  2003/11/15 05:02:07  rtoy
;;; Some simple fixups for READ when reading into arrays.  We need to
;;; identify the expression to generate the correct form for setting the
;;; variable to what was read.
;;;
;;; READ still needs lots of work.
;;;
;;; Revision 1.114  2003/11/14 06:32:43  rtoy
;;; In INSERT-DECLARATIONS, we were computing var-type-list and var-decls
;;; incorrectly.  The were both too short when the arglist had external
;;; functions.
;;;
;;; Revision 1.113  2003/11/13 21:07:38  rtoy
;;; o Was not correctly handling a plain SAVE statement, which means save
;;;   all locals.  Build up the list from the declared and undeclared
;;;   variables.
;;; o Subprograms with multiple entry points weren't returning the right
;;;   number of values.  Fix that.
;;;
;;; Revision 1.112  2003/11/13 05:39:09  rtoy
;;; Generate code to use the new macro WITH-MULTI-ARRAY-DATA.
;;;
;;; Revision 1.111  2003/11/12 05:32:49  rtoy
;;; The test for matching arglist for ENTRY points was wrong.  Fix it.
;;;
;;; Revision 1.110  2003/07/13 18:54:51  rtoy
;;; Add mod to +allowed-lisp-names+.
;;;
;;; Revision 1.109  2003/07/12 04:27:02  rtoy
;;; o Make +reserved-lisp-names+ be T, PI, and NIL.
;;; o Add +allowed-lisp-names+ to be a list of names which can be used as
;;;   is because the Fortran usage matches the Lisp usage.  This prevents
;;;   spurious variables with names like ABS$ from being created.
;;; o Use +allowed-lisp-names+ when checking for reserved lisp names.
;;;
;;; Revision 1.108  2003/01/08 18:41:47  rtoy
;;; Reference symbols in the common-lisp package with "common-lisp:",
;;; instead of "lisp:".
;;;
;;; Revision 1.107  2003/01/08 18:37:58  rtoy
;;; Checking of reserved lisp names was rather weak.  Now check to see if
;;; the symbol is an external symbol in the common-lisp package.  If so,
;;; mangle the name.  (Append with $ instead of _.  Should I really do
;;; that?)
;;;
;;; Revision 1.106  2002/09/13 17:50:19  rtoy
;;; From Douglas Crosher:
;;;
;;; o Make this work with lower-case Lisps
;;; o Fix a few typos
;;; o Make a safer fortran reader.
;;;
;;; Revision 1.105  2002/05/05 21:10:10  rtoy
;;; Comment out extraneous print.
;;;
;;; Revision 1.104  2002/05/04 20:33:32  rtoy
;;; When we construct the declarations for the formal args of the
;;; function, we save the arg types in away as well for later use.
;;;
;;; Revision 1.103  2002/05/03 17:43:46  rtoy
;;; If the array type is simple-array, don't do the with-array-data stuff
;;; because we don't need it.
;;;
;;; Revision 1.102  2002/04/19 18:40:26  rtoy
;;; o Forgot to add FREAL as one of the intrinsics functions we skip over
;;;   when checking for new variables.
;;; o GET-FUN-ARG-TYPE:  was incorrectly handling MULTIPLE-VALUE-BIND
;;;   forms and erroneously returning INTEGER as the type.  We really need
;;;   to look at the function that is being called to get the type instead
;;;   of just looking at MULTIPLE-VALUE-BIND as function name!
;;;
;;; Revision 1.101  2002/03/20 15:50:41  rtoy
;;; Fix typo in getting the argument type in optimize-integer-arithmetic.
;;;
;;; Revision 1.100  2002/03/19 23:45:47  rtoy
;;; When calling a function, it's sometimes (fun args) or (funcall fun
;;; args), so we need to check before we look up the type of the function
;;; call.
;;;
;;; Revision 1.99  2002/03/19 23:08:24  rtoy
;;; Oops.  The array type should default to whatever *array-type* is, not
;;; simple-array!
;;;
;;; Revision 1.98  2002/03/19 17:28:07  rtoy
;;; o Declare some vars as ignored when creating the entry point
;;;   functions.
;;; o Add a check to make sure the entry point functions have exactly the
;;;   same names for the parameters and number of parameters.  That's all
;;;   we support right now.
;;; o Remove a debugging print statement.
;;;
;;; Revision 1.97  2002/03/19 06:03:14  rtoy
;;; First pass at adding support for ENTRY statements (multiple entry
;;; points into a routine).  See NOTES for description of technique.
;;;
;;; Revision 1.96  2002/03/19 04:11:46  rtoy
;;; GET-UPGRADED-FUN-ARG-TYPE wasn't correctly returning the type of a
;;; function call.  Didn't matter before, but with the recent coercion
;;; changes, it does.
;;;
;;; Revision 1.95  2002/03/19 02:23:09  rtoy
;;; According to the rules of Fortran, the initializers in a DATA
;;; statement are supposed to be converted to match the type of the
;;; variable that is being initialized.  Make it so by passing the
;;; variable type to the macro DATA-IMPLIED-DO so that the conversion can
;;; be done.
;;;
;;; Revision 1.94  2002/03/18 23:46:48  rtoy
;;; Was not correctly handling implicit variable declarations.  We just
;;; want to compare the first character of the variable agains the given
;;; ranges, not the whole variable name!
;;;
;;; Revision 1.93  2002/03/18 23:34:16  rtoy
;;; Was not correctly handling some implied do loops containing multiple
;;; variables in the loop in data statements.  Fix that and clean up some
;;; of the processing.  (Should probably do this kind of work in the f2cl
;;; compiler instead of at runtime, but it's only done once at runtime, so
;;; it's not a big deal.)
;;;
;;; Revision 1.92  2002/03/16 14:27:34  rtoy
;;; Remove some print statements that were left in.
;;;
;;; Revision 1.91  2002/03/15 04:04:01  rtoy
;;; When creating the symbol macros for accessing common block vars, we
;;; can get some speed gain by treating arrays specially so that we don't
;;; have to access the array through the structure accessor for every
;;; array access.  For an array, bind a new var to the array, and have the
;;; symbol macro reference the new var.
;;;
;;; Revision 1.90  2002/03/13 04:01:37  rtoy
;;; o Use INT instead of TRUNCATE when coercing parameter assignments.
;;; o Update OPTIMIZE-INTEGER-ARITHMETIC to handle min/max because CMUCL
;;;   isn't always smart enough to figure it out by itself.
;;; o Similarly, if we find a bare TRUNCATE, add an assertion about the
;;;   return value of TRUNCATE.
;;;
;;; Revision 1.89  2002/03/11 16:45:28  rtoy
;;; Try to optimize integer arithmetic. Fortran says integer overflow is
;;; undefined, so we try to wrap all integer arithmetic with (THE INTEGER4
;;; (op operands...)).  This can help the compiler generate better code.
;;;
;;; Revision 1.88  2002/03/10 16:19:03  rtoy
;;; Assignments in PARAMETER statements need to have them coerced to the
;;; right type.
;;;
;;; Revision 1.87  2002/03/06 23:04:10  rtoy
;;; Actually handle Iw.m as Fortran would.
;;;
;;; Revision 1.86  2002/03/06 03:17:52  rtoy
;;; With the block data name changes in f2cl1.l, we need to recognize here
;;; the names that are generated there in f2cl1.
;;;
;;; Revision 1.85  2002/02/17 15:58:16  rtoy
;;; o Implement the new array-slicing method.  (Still needs work.)
;;; o Try to declare all arrays as simple-array, except for arrays that
;;;   are parameters to a function
;;; o Declare some loop variables apropriately for implied do loops in
;;;   write statements.
;;; o We don't try to declare the functions used by a routine anymore.
;;;   (Should this be optional?)
;;;
;;; Revision 1.84  2002/02/14 14:36:16  rtoy
;;; Add sinh, cosh, tanh to the list of generics we need to handle.
;;; Expand on the comments.
;;;
;;; Revision 1.83  2002/02/09 16:08:49  rtoy
;;; o GET-FUN-ARG-TYPE:  We forgot to handle the AINT, SIGN, DIM, MAX, and
;;;   MIN intrinsics.
;;; o INSERT-DECLARATIONS:
;;;   o Rename special-proclamation to common-blocks because we don't have
;;;     special proclamations anymore.
;;;   o The declarations for common blocks are inserted only if
;;;     *DECLARE-COMMON-BLOCKS* is non-NIL.
;;; o GET-IMPLICIT-TYPE:  We don't care if the arg is a formal arg or
;;;   not.
;;; o MAKE-COMMON-BLOCK-VARS:  If *RELAXED-ARRAY-DECLS* is non-NIL, the
;;;   dimensions of the array in the common block is unspecified, even if
;;;   we already know it.  (Useful for changing the sizes of the arrays in
;;;   common blocks at run time.  Some Fortran code uses this feature.)
;;;
;;; Revision 1.82  2002/02/08 23:32:51  rtoy
;;; The last change to support block data was majorly broken.  Right idea,
;;; wrong implementation.  Initializers and stuff need to be inside the
;;; function otherwise nothing is really initialized as expected.  This
;;; should work better.
;;;
;;; Revision 1.81  2002/02/08 06:04:48  rtoy
;;; o We were generating incorrect code for symbol-macrolets for
;;;   initializers.  Fix it and clean it up.  Now only generate it either
;;;   for initialization or the body but not both since you can only
;;;   initialize common blocks in a block data subprogram.
;;; o Fix a bug wherein a variable in a parameter statement that was also
;;;   declared was getting the wrong type and also getting declared as a
;;;   local var.  Happened because we were checking the other var against
;;;   the wrong list (was *key-params* but should have been key-params).
;;;
;;; Revision 1.80  2002/02/08 04:27:31  rtoy
;;; To support BLOCK DATA subprograms, we need to have the data
;;; initialization part of the code wrapped by symbol-macrolets.  Make it
;;; so.  Move the common code to its own routine.
;;;
;;; Revision 1.79  2002/02/07 23:21:58  rtoy
;;; MAKE-DECLARATION
;;; o For some reason when we were checking for implicitly declared
;;;   variables, we checked to see if the variable was a formal arg or
;;;   not.  Formal args can be implicitly declared too, so the check is
;;;   removed.  (Why where we checking before?)
;;; o Clean up some comments.
;;;
;;; SYM-IS-NUMBER-P
;;; o Add some comments
;;; o We were incorrectly saying things like 1D%3 were not numbers, but
;;;   this is wrong.  It is a number.  Solves the occasional problem where
;;;   we got weird things like unused variables named |1D%3|.  (Off-by-one
;;;   bug.)
;;;
;;; Revision 1.78  2002/02/07 22:16:19  rtoy
;;; If an array was declared but actually dimensioned in a common block
;;; like
;;;
;;; 	double precison c
;;; 	common /foo/ c(42)
;;;
;;; VBLE-IS-ARRAY-P didn't think it was an array.  Fix it.
;;;
;;; Revision 1.77  2002/02/07 03:58:45  rtoy
;;; o The previous change messed up the parsing of the format number of a
;;;   write statement.  Fix it.
;;; o The previous change also messed up the unit number for
;;;   PARSE_FORMAT_DEST. Fix it.
;;; o Add a parser for Fortran's A format descriptor.
;;;
;;; Revision 1.76  2002/02/04 03:22:31  rtoy
;;; Handle the case where the unit number for a write statement can be an
;;; arbitrary expression.
;;;
;;; Revision 1.75  2002/01/13 16:57:52  rtoy
;;; When looking up variables in the declared variables list or the
;;; subprog arglist, we still need to handle them via Fortran's implicit
;;; typing rules.
;;;
;;; Revision 1.74  2002/01/09 15:31:08  rtoy
;;; o In GET-FUN-ARG-TYPE, we weren't correctly handling the case of unary
;;;   + and -.
;;; o When looking up the type of a variable, we need to check for the
;;;   variable in *subprog-arglist* as well as *declared_vbles*!
;;;
;;; Thanks to Mike Koerber for sending sample code where this fails.
;;;
;;; Revision 1.73  2002/01/08 03:24:44  rtoy
;;; o Correct the previous change about looking up the type of array
;;;   references.  If we have an fref, it can't be an array slice because
;;;   we would have already sliced it before we get here.
;;; o Try to pretty up the declarations for other functions by merging
;;;   them into just one declaration.
;;; o Group the declaration statements for variables and other functions
;;;   into just a single declaration.
;;;
;;; Revision 1.72  2002/01/07 20:55:40  rtoy
;;; In GET-FUN-ARG-TYPE, we were always returning array types for FREF.  This
;;; is true if array-slicing is enabled.
;;;
;;; Revision 1.71  2002/01/06 23:10:12  rtoy
;;; Rename *intrinsic_function_names*, *external_function_names* and
;;; *subprog_stmt_fns* to use dashes.
;;;
;;; Revision 1.70  2001/06/04 17:16:24  rtoy
;;; Print a warning if there is no implicit type given and the variable
;;; was not declared.  This is invalid Fortran anyway.
;;;
;;; Revision 1.69  2001/06/03 20:46:08  rtoy
;;; Changes to FIX-DO:
;;; o Add a test and code to keep FIX-DO from looping forever searching
;;;   for a non-existent DO label.  We cause an error now if this happens.
;;; o Gratuitously re-indented code.
;;;
;;; Revision 1.68  2000/09/03 02:33:39  rtoy
;;; FORTRAN-CONTAGION returned (complex double-float) and (complex
;;; single-float) which confuses f2cl.  Return complex16 and complex8
;;; instead, respectively.
;;;
;;; Revision 1.67  2000/08/30 16:56:24  rtoy
;;; In PARSE-FORMAT1, if there's no repetition factor for a group, then
;;; the group is supposed to be repeated forever until all the data has
;;; been printed.  (I think)
;;;
;;; Revision 1.66  2000/08/29 15:55:04  rtoy
;;; o In GET-ARG-DECL, return type INTEGER4 if the arg is a subtype of
;;;   INTEGER4. (So fixnum args are treated as INTEGER4 args for
;;;   declaration purposes instead of a union of fixnum and integer4
;;;   types.)
;;; o In PARSE-READ, don't use literal strings; make them FORTRAN_COMMENT's.
;;;
;;; Revision 1.65  2000/08/29 14:41:41  rtoy
;;; o Remove lots of unused code
;;; o Fix a typo in the Fortra D format parser: the equivalent Lisp format
;;;   is E not D!  Also, explicitly specify the exponent character of "D".
;;; o For the E format, specify an explicit exponent character of "E".
;;;
;;; Revision 1.64  2000/08/27 16:36:07  rtoy
;;; Clean up handling of format statements.  Should handle many more
;;; formats correctly now.
;;;
;;; Revision 1.63  2000/08/10 18:00:37  rtoy
;;; Declarations of the array type for arrays in common blocks were
;;; wrong.  (Oops!)
;;;
;;; Revision 1.62  2000/08/09 22:45:17  rtoy
;;; o In MAKE-DECLARATION, use the LOGICAL type instead of (MEMBER T NIL)
;;;   in declaring arrays.
;;; o MAKE-SPECIAL-VAR-DECL was not returning the right dimensions.
;;;   (After the conversion to 1-D arrays.)
;;; o In MAKE-COMMON-BLOCK-VARS, try to initialize arrays to the right
;;;   size and values.  (Particularly for logical arrays.)
;;; o In MAKE-COMMON-BLOCK-VAR-INIT, we didn't compute the array bounds
;;;   correctly.  Also, if we know the initializer for the common block
;;;   structure element initialized the array (because the dimension was a
;;;   number), don't do it when creating the structure.
;;;
;;; Revision 1.61  2000/08/07 18:55:03  rtoy
;;; GET-ARG-DECL was confused by arrays of strings.  We return the type
;;; ARRAY-STRINGS now.  (Need better names for these types or need to fix
;;; the code so it handles these better!)
;;;
;;; Revision 1.60  2000/08/05 19:23:16  rtoy
;;; Comment out some unreachable code.
;;;
;;; Revision 1.59  2000/08/04 14:20:31  rtoy
;;; Add very rudimentary support for Fortran READ statements.  This means
;;; we just basically call read and assign the result to the (simple)
;;; variable.  We don't even bother to look at the format number or check
;;; the variable type.
;;;
;;; Revision 1.58  2000/08/03 03:39:49  rtoy
;;; The string passed to PARSE-FORMAT-DESCRIPTOR-H can be a character,
;;; digit, or symbol.  These all need to be converted to character.  Make
;;; it so.
;;;
;;; Revision 1.57  2000/08/02 16:26:20  rtoy
;;; The D and E format descriptors should print out a D and E,
;;; respectively.  Thus add PARSE-FORMAT-DESCRIPTOR-D.
;;;
;;; Revision 1.56  2000/08/02 14:42:20  rtoy
;;; o Add support for parsing Hollerith strings in format statements.  Not
;;;   perfect because the preprocessor mangles spaces within the string,
;;;   but we can at least print out the non-space parts of the string.
;;;   Changed PARSE-FORMAT1, DESTRUCT-DESCRIPTOR-LIST, and
;;;   PARES-FORMAT-DESCRIPTOR-H for this.
;;;
;;; o PARSE-FORMAT-DESCRIPTOR-LIST treated D format descriptor as F
;;;   instead of as E.  (Should we distinguish between D and E?)
;;;
;;; o PARSE-FORMAT-DESCRIPTOR-F didn't add the fill character when the
;;;   number is too large to fit in the desired field.
;;;
;;; Revision 1.55  2000/07/30 05:58:48  rtoy
;;; Don't check for new variables inside of multiple-value-bind's that
;;; were created for function calls.  We only need to check in the
;;; arguments of the function call.
;;;
;;; Revision 1.54  2000/07/28 17:05:01  rtoy
;;; o We are in the f2cl package now.
;;; o We convert // to f2cl-//, even in format statements so fix
;;;   FIX-SLASHES to handle this case by replacing f2cl-// with 2 slashes,
;;;   as appropriate.
;;;
;;; Revision 1.53  2000/07/27 16:42:01  rtoy
;;; o We want to be in the CL-USER package, not the USER package.
;;; o Use (typep x 'integer) instead of (fixnump x) in GET-FUN-ARG-TYPE.
;;;
;;; Revision 1.52  2000/07/21 17:47:20  rtoy
;;; o FIXUP-EXPRESSION:  add a case to convert (- N) to just -N, when N is
;;;   a number.
;;;
;;; o MERGE-OPS: incorrectly merged (- (- 3) IT) to (- 3 IT).  I think
;;;   this is fixed now.
;;;
;;; o INSERT-DECLARATIONS: Don't fixup external function refs if an
;;;   intrinsic was actually a variable in the arglist.  (Missed this case
;;;   from before.)
;;;
;;; Revision 1.51  2000/07/20 13:43:00  rtoy
;;; Since all arrays are now actually stored in column-major order in a
;;; 1-dimensional vector, we don't need to transpose the data initializers
;;; anymore.  Replace fortran-transpose with fortran-data-init to
;;; correctly initialize the array.
;;;
;;; Revision 1.50  2000/07/19 22:17:13  rtoy
;;; Remove a print statement inadvertently left in.
;;;
;;; Revision 1.49  2000/07/19 14:04:28  rtoy
;;; o GET-FUN-ARG-TYPE returns a second value to indicate if the arg is an
;;;   array or not.
;;; o More hacking on GET-ARG-DECL.  Should now correctly identify if an
;;;   array is used as a parameter.  Still needs work.
;;; o MAKE_MAKE-ARRAY_STMT creates 1-D arrays for all arrays to support
;;;   Fortran array slicing.
;;; o In MAKE-DECLARATION, declare all arrays as 1-D even if
;;;   multi-dimensional, for supporting Fortran array slicing.
;;;
;;; Revision 1.48  2000/07/18 13:59:23  rtoy
;;; o Left out some double precision intrinsics for getting function
;;;   types.
;;; o Declarations for functions were not quite right.  Make it better, but
;;;   still needs some work.
;;; o The format of *functions-used* has changed.  Do the right thing in
;;;   insert-declarations.
;;;
;;; Revision 1.47  2000/07/14 21:23:37  rtoy
;;; o In GET-FUN-ARG-TYPE, when looking up the type of an expression,
;;;   handle (funcall f ...) by looking up the type of "f" instead of
;;;   looking up the type of "funcall"!
;;;
;;; o In FIX-DO, a goto to the end of the loop would get translated into a
;;;   (return).  This seems wrong, and I don't know why it wants to do
;;;   this.
;;;
;;; Revision 1.46  2000/07/14 15:50:25  rtoy
;;; o When getting the type of an arg, handle the case when the arg is
;;;   actually a call to make-array for array slicing.  Get the type form
;;;   the :element-type.
;;; o If the arg has type fixnum, return integer4 instead.
;;;
;;; Revision 1.45  2000/07/14 14:08:26  rtoy
;;; Honor the user's choice of declaring arrays as array or simple-array.
;;; Except we leave Fortran character strings still declared as
;;; simple-array.
;;;
;;; Revision 1.44  2000/07/14 13:33:26  rtoy
;;; Don't apply external ref fixups if the external function was also a
;;; parameter to the routine.  If we do, then we referring to the wrong
;;; thing!
;;;
;;; Revision 1.43  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
