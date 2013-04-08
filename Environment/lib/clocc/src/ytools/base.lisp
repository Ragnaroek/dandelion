;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools -*-
(in-package :ytools)
;;;$Id: base.lisp,v 2.4 2006/05/20 01:44:24 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.



#+allegro (eval-when (:compile-toplevel :load-toplevel :execute)
	     (shadowing-import 'excl:*current-case-mode*))
#-allegro (defvar *current-case-mode* ':case-insensitive-upper)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
   (shadow '(#:defun #:defmacro #:eval-when #:defmethod)))

;;; These have to be separate because if the following is
;;; *read* before the 'shadow' is *evaluated*, then there will
;;; be no symbol with name "defun" internal to, or importable into,
;;; the :ytools package.
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
   (export '(ytools-readtable* _ defun defmacro \\
	     make-eq-hash-table table-entry href walk-table clear-table
	     make-Array make-Symbol
	     Array-dimension Array-dimensions
	     is-Vector is-Array is-Symbol
             Symbol-name Symbol-plist Symbol-function Symbol-value
	     is-Keyword is-String memq assq nodup =< retain-if
	     is-Pair is-cons list-copy is-list
	     tuple pair head tail nthrest nthtail left right
	     ;;;; one two three four five six seven eight nine ten
	     is-Char is-Integer is-Number
	     is-Float is-Single-float is-Double-float
	     is-Fixnum is-Ratio is-sublist is-whitespace
	     is-Stream list->values values->list lastelt len
             string-length string-concat
	     build-symbol symno* true false keyword-package*
	     eval-when condense
	     assoc= alist-entry alist-entry-set alref. alref
	     include-if series car-eq take drop occurs-in empty-list
	     on-list on-list-if-new off-list -- loading-bogus
	     *current-case-mode*)))

;;;;(eval-when (:compile-toplevel)
;;;;   (format t "shadow-export done"))

#-:excl
(cl:defmacro with-packages-unlocked (&body body)
  `(progn ,@body))
#+:excl
(cl:defmacro with-packages-unlocked (&body body)
  `(excl:without-package-locks ,@body))

(cl:defmacro eval-when (situations &body b)
   (cond ((member ':slurp-toplevel situations)
	  `(progn (eval-when-slurping ,@b)
		  (cl:eval-when ,(remove ':slurp-toplevel situations)
		     ,@b)))
	 (t
	  `(cl:eval-when ,situations ,@b))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)

(cl:defun ignore-smooth (args body)
   (multiple-value-bind (newargs to-be-ignored)
			(underscores-elim args)
      (let ((realbod (ignore-convert body)))
	 (cond ((null to-be-ignored)
		(values args realbod))
	       (t
		(values newargs `((declare (cl:ignore ,@to-be-ignored))
				  ,@realbod)))))))

(cl:defun underscores-elim (args)
   (let ((realargs '())
	 (new-ignores '())
	 (keyargs nil))
      (dolist (a args)
	 (labels ((got-underscore ()
		   (let ((new (gensym)))
		      (push new new-ignores)
		      (push (subst new '_ a)
			    realargs))))
	    (cond ((eq a '_)
		   (got-underscore))
		  ((consp a)
		   (cond (keyargs
			  (cond ((or (eq (car a) '_)
				     (and (consp (car a))
					  (eq (cadr (car a))
					      '_)))
				 (got-underscore))
				(t
				 (push a realargs))))
			 ((eq (car a) '_)
			  (got-underscore))
			 (t
			  (push a realargs))))
		  (t
		   (cond ((eq a '&key)
			  (setq keyargs t)))
		   (push a realargs)))))
      (values (reverse realargs)
	      new-ignores)))

(cl:defun ignore-convert (body)
   (cond ((and (not (null body))
	       (consp (car body))
	       (eq (caar body) 'ignore))
	  `((declare (cl:ignore ,@(cdar body))) ,@(cdr body)))
	 (t body)   ))

)

(cl:defmacro defun (name args &rest body)
   (multiple-value-bind (args body)
			(ignore-smooth args body)
      `(cl:defun ,name ,args ,@body)))

(cl:defmacro defmacro (name args &rest body)
   (multiple-value-bind (args body)
			(ignore-smooth args body)
      `(cl:defmacro ,name ,args ,@body)   ))

(cl:defmacro defmethod (&whole dm-exp name &rest stuff)
   (let ((qualifiers '())
	 args body)
      (do ((sl stuff (cdr sl)))
	  ((or (null sl)
	       (listp (car sl)))
	   (cond ((null sl)
		  (error "Unintelligible: ~s" dm-exp))
		 (t
		  (setq args (car sl))
		  (setq body (cdr sl)))))
	(push (car sl) qualifiers))
      (multiple-value-bind (args body)
	                   (ignore-smooth args body)
	 `(cl:defmethod ,name ,@(reverse qualifiers) ,args ,@body))))

(cl:defmacro \\ (args &rest body)
   (multiple-value-bind (args body)
			(ignore-smooth args body)
      `(function (lambda ,args ,@body))   ))

(defmacro eval-when-slurping (&rest _) ''no-op)

(defconstant true t)
(defconstant false nil)
(defconstant nil* 'nil)

(defparameter keyword-package* (find-package 'keyword))

;;; Each entry is of form (char global-handler -local-handlers-)
;;; The current readtable is looked up in the local-handlers (itself an
;;; alist).  If no entry, use global (the value of ytools-readtable*).  
;;; If no global, '!' should be taken
;;; as an ordinary character.
(defvar excl-handlers* '())

;;; The following two procedures return an access function and a set function
;;; for the place in
;;; an entry where the excl-reader procedure is stored.
;;; Note that if no special handler is associated with the readtable,
;;; we return the handler, if any, associated with ytools-readtable*.
;;; Specifying false as the value for rt is the same as specifying
;;; ytools-readtable*.
(defun readtable-excl-acc (rt)
   (cond ((or (not rt) (eq rt ytools-readtable*))
	  (\\ (entry) (cadr entry)))
	 (t
	  (\\ (entry)
	     (let ((rte (assoc rt (cddr entry) :test #'eq)))
		(cond (rte (cadr rte))
		      (t (cadr entry))))))))

(defun readtable-excl-set (rt)
   (cond ((or (not rt) (eq rt ytools-readtable*))
	  (\\ (entry fcn) (setf (cadr entry) fcn)))
	 (t
	  (\\ (entry fcn)
	     (let ((rte (assoc rt (cddr entry) :test #'eq)))
		(cond ((not rte)
		       (setq rte (tuple rt nil))
		       (setf (cddr entry) (cons rte (cddr entry)))))
		(setf (cadr rte) fcn))))))

(defun excl-reader (srm ch)
   (setq ch (peek-char nil srm nil nil))
   (cond ((not ch)
	  ;; end of file
	  (intern "!"))
	 (t
	  (labels ((nonmacro ()
		      (cond ((member ch '(#\space #\tab #\newline
					  #\return #\linefeed
					  #\page #\( #\))
				     :test #'char=)
			     (intern "!"))
			    (t
			     ;; if any problems here, could try
			     ;; UNREAD-CHAR + (VALUES)
			     (values
			      (intern (concatenate 'string
					    "!"
					    (string (read srm t nil t)))))))))
	     (let ((e (assoc ch excl-handlers* :test #'eq)))
		(cond (e
		       (let ((handler
			        (funcall (readtable-excl-acc *readtable*) e)))
			  (cond (handler
				 (read-char srm)
				 (funcall handler srm ch))
				(t
				 (nonmacro)))))
		      (t (nonmacro))))))))

(set-macro-character #\! #'excl-reader ytools-readtable*)

;;;;(cl:eval-when (:compile-toplevel :load-toplevel)
;;;;   #+cmu (ignore-errors
;;;;		(make-dispatch-macro-character #\! t ytools-readtable*))
;;;;   #-cmu (make-dispatch-macro-character #\! t ytools-readtable*))

;;;;(defun treat-excl-as-char (srm ch param)
;;;;   (let ((pc (peek-char nil srm)))
;;;;      (cond ((or (is-whitespace pc)
;;;;		 (eq pc '#\()
;;;;		 (eq pc '#\)))
;;;;	     (intern (format nil "!~a~a" (or param "") ch)))
;;;;	    ((eq pc '#\?)
;;;;	     (read-char srm)
;;;;	     '|!=?|)
;;;;	    (t
;;;;	     ;;;(unread-char ch)
;;;;	     (let ((sym (read srm)))
;;;;		(cond ((symbolp sym)
;;;;		       (intern (format nil "!~a~a~a"
;;;;				       (or param "") ch (symbol-name sym))))
;;;;		      (t
;;;;		       (error "Illegal '!' construct !~a~a~s"
;;;;			      (or param sym) ch sym))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)

; Useful macro in backquote. ,@(INCLUDE-IF test -stuff-) includes stuff if
; test is non-false.  If stuff is omitted, it defaults to test itself.
(defmacro include-if (tst &rest stuff)
   (cond ((null stuff)
	  `(list-if-not-false ,tst))
	 (t
	  `(cond (,tst (list ,@stuff)) (t '())   ))))

(cl:defmacro subr-synonym (syn subr &optional setf-able (numargs 1))
   (cond ((eq syn subr)
	  ;; If they're eq, the only reason to declare them synonyms
	  ;; is that they looked different when input, as:
	  ;; (subr-synonym 'make-Pathname 'make-pathname)
	  `'(subr-synonym ,syn ,subr))
	 (t
	  `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
	      (define-subr-synonym ',syn ',subr)
	      (define-compiler-macro ,syn (&rest args)
		 `(,',subr ,@args))
	      ;; If setf-able, we assume only one arg, or
	      ;; things will get too messy.
	      ,@(include-if setf-able
		   (let ((setf-name (intern (concatenate 'string
					        (symbol-name syn)
						"-" (symbol-name :setf))))
			 (setf-args '()))
		      (dotimes (i numargs)
			 (setq setf-args
			       (cons (intern
				      (format nil "x~s" (- numargs i)))
				     setf-args)))
		      `(progn
			  (defun ,setf-name (,@setf-args v)
			     (setf (,subr ,@setf-args) v))
			  (defsetf ,syn ,setf-name))))))))
)

;;;; This doesn't work due to a bug in Allegro
;;;;	      ,@(include-if setf-able
;;;;		   `(progn
;;;;		       (defun (setf ,syn) (v x)
;;;;			  (setf (,subr x) v))
;;;;		       (define-compiler-macro (setf ,syn) (v^ x^)
;;;;			   `(setf (,',subr ,x^) ,v^))))))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)

(defvar subr-synonyms* '())
		  
(cl:defun define-subr-synonym (syn subr)
   ;; Work around bug in OpenMCL --
   #+openmcl (cond ((macro-function subr)
		    (setf (macro-function syn) (macro-function subr)))
		   (t
		    (setf (symbol-function syn)
		          (symbol-function subr))))
   #-openmcl (setf (symbol-function syn) (symbol-function subr))
   (let ((p (assoc syn subr-synonyms* :test #'eq)))
      (cond ((not p)
	     (setq subr-synonyms*
		   (cons (list syn subr) subr-synonyms*)))
	    (t
	     (setf (cadr p) subr)))))
)

(cl:defun make-eq-hash-table (&rest args)
   (apply #'make-hash-table :test #'eq args))

(cl:defun table-entry (htab key) (gethash key htab))

(defsetf table-entry (htab key) (x) `(setf (gethash ,key ,htab) ,x))

(defmacro href (htab^ key^ &optional (default^ 'false))
   (let ((valvar (gensym)) (presvar (gensym)))
      `(multiple-value-bind (,valvar ,presvar)
			    (gethash ,key^ ,htab^)
	  (cond (,presvar ,valvar)
		(t ,default^)))))

(defsetf href (htab^ key^ &optional default^) (newval^)
   (declare (ignore default^))
   `(setf (gethash ,key^ ,htab^) ,newval^))

(subr-synonym is-Hash-table hash-table-p)
(subr-synonym walk-table    maphash)
(subr-synonym clear-table  clrhash)

(subr-synonym is-Symbol symbolp)
(subr-synonym Symbol-name symbol-name)
(subr-synonym Symbol-plist symbol-plist)
(subr-synonym Symbol-function symbol-function)
(subr-synonym Symbol-value symbol-value)

(subr-synonym is-Keyword keywordp)

(subr-synonym is-String stringp)

(subr-synonym list-copy copy-list)

(cl:defun memq (x l) (member x l :test #'eq))
(cl:defun assq (k al) (assoc k al :test #'eq))
(cl:defun assoc= (test k al) (assoc k al :test test))
(define-compiler-macro assoc= (eqt^ x^ al^)
   `(assoc ,x^ ,al^ :test ,eqt^))

(defun nodup (l &key (test #'eql))
   (declare (type list l))
   (remove-duplicates l :test test))

(subr-synonym retain-if remove-if-not)

;;;;   (do ((tl (reverse l) (cdr tl))
;;;;	(res '()))
;;;;       ((null tl) res)
;;;;     (cond ((not (member (car tl) (cdr tl) :test test))
;;;;	    (setq res (cons (car tl) res))))))

(subr-synonym =< <=)

(subr-synonym make-Array make-array)
(subr-synonym make-Symbol make-symbol)

(subr-synonym is-Array arrayp)
(subr-synonym is-Pair consp)
(subr-synonym is-Vector vectorp)
(subr-synonym is-cons consp)
(subr-synonym is-sublist subsetp)
(subr-synonym is-list listp)

(subr-synonym tuple list)
(subr-synonym pair cons)

;;; For decomposing lists
(subr-synonym head car t)
(subr-synonym tail cdr t)

;;; For decomposing dot-pairs used as two-entry records (e.g., binary trees)
(subr-synonym left car t)
(subr-synonym right cdr t)

(subr-synonym nthrest nthcdr nil)
(subr-synonym nthtail nthcdr nil)

;;;;(subr-synonym one first)
;;;;(subr-synonym two second t)
;;;;(subr-synonym three third t)
;;;;(subr-synonym four fourth t)
;;;;(subr-synonym five fifth t)
;;;;(subr-synonym six sixth t)
;;;;(subr-synonym seven seventh t)
;;;;(subr-synonym eight eighth t)
;;;;(subr-synonym nine ninth t)
;;;;(subr-synonym ten tenth t)

(subr-synonym is-Char characterp)
(subr-synonym is-Integer integerp)
(subr-synonym is-Number numberp)
(subr-synonym is-Float floatp)

(defun is-Single-float (x) (typep x 'single-float))
(defun is-Double-float (x) (typep x 'double-float))

(defun is-Fixnum (n)
   (and (integerp n)
	(<= n most-positive-fixnum)
	(>= n most-negative-fixnum)))	

(defun is-Ratio (x) (typep x 'ratio))

(subr-synonym is-Stream streamp)
(subr-synonym list->values values-list)
(subr-synonym values->list multiple-value-list)

(subr-synonym Array-dimension array-dimension)
(subr-synonym Array-dimensions array-dimensions)

(defun lastelt (l) (car (last l)))

(defun len (l) (length (the list l))   )
(defun string-length (s) (length (the string s)))

(defun string-concat (&rest vl)
  (apply #'concatenate 'string vl))

(define-compiler-macro string-concat (&rest vl)
   `(concatenate 'string ,@(mapcar (\\ (v) `(the string ,v))
                                   vl)))

;; Macro for building new symbols.
;; (BUILD-SYMBOL [(:package <p>)] -specs-) creates a symbol
;; whose print name is built out of specs.  An atomic or string spec is
;; concatenated in.  A spec of the form (:< e1 e2 ...) has each eI evaluated
;; and its characters concatenated in.  A spec of the form (:++ e) increments e
;; and concatenates in its characters.  Anything else is supposed to
;; evaluate to a list of characters, which are concatenated in.
;; Example: If A = FOO, B = (1 2 3), and C = (#\B \#A \#R), then
;; (build-symbol (:< A) (:++ (CAR B)) (CDR C) "< >") is FOO2AR</ >, and
;; B becomes (2 2 3).
(defmacro build-symbol (&rest l)
   (let ((p (find-if #'(lambda (x) (and (consp x) (eq (car x) ':package)))
		     l)))
      (cond (p
	     (setq l (remove p l))))
      (let ((pkg (cond ((memq (cadr p) '(false nil))
			false)
		       (t `(find-package ',(cadr p))))))
	 (cond (p
		(cond (pkg
		       `(values (intern ,(symstuff l) ,pkg)))
		      (t
		       `(make-symbol ,(symstuff l)))))
	       (t
		`(values (intern ,(symstuff l))))))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)

(defun symstuff (l)
   `(concatenate 'string
     ,@(mapcan
           (\\ (x)  (cond ((stringp x)
			   (cond ((and (find-if #'alpha-char-p x)
				       (not (find-if #'is-whitespace
						     x)))
				  (format *error-output*
				     "Warning: alphabetic string ~s in ~
                                      build-symbol;~
                                    ~%  more portable to use symbols.~
                                    ~%  to make this warning go away ~
                                      replace with :~a)"
				     x x)))
			   (list `',x))
                          ((symbolp x)
                           (list `',(symbol-name x)))
                          ((atom x)
                           (list `(coerce-to-string ,x)))
                          ((memq (car x) '(:< <))
			   (mapcar (\\ (y) `(coerce-to-string ,y)   )
				   (cdr x)))
                          ((memq (car x) '(:++ ++))
                           (list `(princ-to-string (incf ,(cadr x)))))
                          (t (list `(coerce-to-string ,x)))   ))
	   l)   ))

(defvar symno* 0) ;; General-purpose counter for built symbols

(defun is-whitespace (ch)
     (or (char= ch #\space)
	 (not (graphic-char-p ch)))   )

)

(defun coerce-to-string (x)
   (cond ((stringp x) x)
	 ((characterp x) (string x))
	 ((numberp x)
	  (princ-to-string x))
	 ((symbolp x)
	  (symbol-name x))
	 (t (format nil "~a" x))   ))

(defmacro alist-entry (x^ l^ &optional (initial^ 'false))
   `(alist-entry-def ,x^ ,l^
		     ,(cond ((eq initial^ 'false) 'false)
			    (t `(\\ () ,initial^)))))

(defun alist-entry-def (x a initializer)
   (let ((p (assoc x a :test #'eq)))
      (cond (p (second p))
	    (t (and initializer (funcall initializer))))))

(define-setf-expander alist-entry-def (x l initializer)
   (multiple-value-bind (ltemps lvals lstores lset lacc)
                        (get-setf-expansion l)
      (let ((entry-var (gensym))
	    (xvar (gensym)) (newvar (gensym)) (lvar (gensym))
            (storevar (car lstores)))
         (values `(,xvar ,@ltemps ,lvar ,entry-var)
                 `(,x ,@lvals ,lacc (assq ,xvar ,lvar))
                 `(,newvar)
                 `(progn
                     (cond ((not ,entry-var)
			    (setq ,entry-var (tuple ,xvar ,newvar))
			    (let ((,storevar (cons ,entry-var ,lvar)))
			      ,lset)))
		     (setf (cadr ,entry-var) ,newvar)
		     ,newvar)
                 `(cond (,entry-var (cadr ,entry-var))
			(t ,(cond ((memq initializer '(false nil)) 'false)
				  (t `(funcall (\\ () ,initializer))))))))))

;;;;(alist-entry-def ,xvar ,lvar ,initializer)

(defmacro alref. (alist^ key^ &optional (default^ 'false)
			      &key ((:test test^) '#'eq)
				   ((:acc acc^) '#'right)
				   ((:new-entry new-entry^)))
                 (declare (ignore new-entry^))
   (let ((entry-var (gensym)))
      (let ((acc-form (cond ((and (consp acc^)
				  (memq (car acc^) '(function quote funktion)))
			     `(,(cadr acc^) ,entry-var))
			    (t
			     `(funcall ,acc^ ,entry-var)))))
	 `(let ((,entry-var (assoc= ,test^ ,key^ ,alist^)))
	     (cond (,entry-var ,acc-form)
		   (t ,default^))))))

(define-setf-expander alref. (alist^ key^ &optional (default^ 'false)
					  &key ((:test test^) '#'eq)
					       ((:acc acc^) '#'right)
					       ((:new-entry new-entry^) 'nil))
   (multiple-value-bind (altemps alvals alstores alist-set alist-acc)
                        (get-setf-expansion alist^)
      (let ((key-var (gensym)) (alist-var (gensym)) (entry-var (gensym))
	    (new-var (gensym))
            (store-var (car alstores)))
	 (let ((acc-form (cond ((and (consp acc^)
				     (memq (car acc^) '(function quote funktion)))
				`(,(cadr acc^) ,entry-var))
			       (t
				(error "Can't set ~s of alref. entry in ~s"
				       acc^ alist^)))))
	    (values `(,@altemps ,key-var ,alist-var
			       ,entry-var)
		    `(,@alvals ,key^ ,alist-acc
			    (assoc= ,test^ ,key-var ,alist-var))
		    `(,new-var)
		    `(progn
		         (cond ((not ,entry-var)
				(setq ,entry-var (cons ,key-var ,new-entry^))
				(let ((,store-var (cons ,entry-var ,alist-var)))
				   ,alist-set)))
			 (setf ,acc-form ,new-var))
		    `(cond (,entry-var ,acc-form)
			   (t ,default^)))))))

;;; Most common special case.
(defmacro alref (alist^ key^ &optional (default^ 'false)
			     &key ((:test test^) '#'eq))
   `(alref. ,alist^ ,key^ ,default^ :test ,test^ :acc #'second :new-entry (list nil)))

;;;;   (let ((entryvar (gensym)))
;;;;      `(let ((,entryvar (assoc ,key^ ,alist^ :test ,test^)))
;;;;	  (cond (,entryvar (cadr ,entryvar))
;;;;		(t ,default^)))))

;;;;   (let ((entryvar (gensym)))
;;;;      `(let ((,entryvar (assoc= #'eq ,key^ ,alist^)))
;;;;	  (cond (,entryvar (second ,entryvar))
;;;;		(t ,default^)))))

;;;;(define-setf-expander alref (alist^ key^ &optional default^)
;;;;   (multiple-value-bind (altemps alvals alstores alist-set alist-acc)
;;;;                        (get-setf-expansion alist^)
;;;;      (let ((keyvar (gensym)) (newvar (gensym)) (alist-var (gensym))
;;;;	    (entry-var (gensym))
;;;;            (storevar (car alstores)))
;;;;         (values `(,keyvar ,@altemps ,alist-var ,entry-var)
;;;;                 `(,key^ ,@alvals ,alist-acc (assoc= #'eq ,keyvar ,alist-var))
;;;;                 `(,newvar)
;;;;                 `(cond (,entry-var
;;;;			 (setf (cadr ,entry-var) ,newvar))
;;;;			(t
;;;;			 (let ((,storevar (cons (tuple ,keyvar ,newvar)
;;;;						,alist-var)))
;;;;			   ,alist-set
;;;;			   ,newvar)))
;;;;                 `(cond (,entry-var (cadr ,entry-var))
;;;;			(t ,default^))))))

;;;;(set-dispatch-macro-character #\! #\( #'lpar-exclmac ytools-readtable*)

;;; Function will be called after char has been read.  'args'
;;; are stream + the character.
(defmacro def-excl-dispatch (char args &body b)
   (multiple-value-bind (b rt)
                        (let ((tl (member ':readtable b)))
			   (cond (tl
				  (values `(,@(ldiff b tl) ,@(cddr tl))
					  (cadr tl)))
				 (t
				  (values b '*readtable*))))
      (let ((fun-name (intern (format nil "excl-handler-~a" char))))
	 `(progn
	     (defun ,fun-name ,args
		,@b)
	     (let ((e (assoc ',char excl-handlers* :test #'eq)))
		(cond ((not e)
		       (setq e (tuple ',char nil))
		       (setq excl-handlers* (cons e excl-handlers*))))
		(funcall (readtable-excl-set ,rt) e #',fun-name))))))

;;;;                  (cond (rt
;;;;			`(let ((rte (assoc ,rt (cddr e) :test #'eq)))
;;;;			    (cond ((not rte)
;;;;				   (setq rte (tuple ,rt nil))
;;;;				   (setf (cddr e) (cons rte (cddr e)))))
;;;;			    (setf (cadr rte) #',fun-name)))
;;;;		       (t
;;;;			`(setf (cadr e) #',fun-name)))

(def-excl-dispatch #\( (srm ch)
   (setq ch (peek-char t srm))
   (cond ((char= ch '#\) )
	  (read-char srm)
	  (list 'empty-list false))
	 (t
	  (let ((thing (read srm)))
	     (setq ch (peek-char t srm))
	     (cond ((eq ch '#\) )
		    (read-char srm)
		    (list 'empty-list thing))
		   (t
		    (cerror "!(~s...) has too much stuff before close paren"
			    thing)))))))


(set-pprint-dispatch
    '(cons (eql empty-list))
    (\\ (srm el)
       (cond ((= (length el) 2)
	      (cond ((cadr el) (format srm "!(~s)" (cadr el)))
		    (t (format srm "!()"))))
	     (t
	      (pprint-fill srm el true))))
    2)

(defmacro empty-list (&rest _)
  ''())

(define-symbol-macro _ 'nil)

(def-excl-dispatch #\" (srm _)
   (unread-char #\" srm)
   (let* ((fstr (read srm))
	  (slen (length fstr)))
      (do ((squig (position #\~ fstr)
		  (position #\~ fstr :start postskip))
	   (prev 0 postskip)
	   postskip
	   (seg "" "")
	   (res ""
		(concatenate 'string
		             res (subseq fstr prev squig) seg)))
	  ((or (null squig)
	       (>= squig (- slen 1)))
	   (concatenate 'string res
		    (subseq fstr prev
			    (or squig slen))))
	(let ((ch (elt fstr (+ squig 1))))
	   (cond ((char= ch #\~)
		  (setq seg "~")
		  (setq postskip (+ squig 2)))
		 ((char= ch #\%)
		  (setq seg (string #\Newline))
		  (setq postskip (+ squig 2)))
		 ((is-whitespace ch)
		  (setq postskip (position-if-not #'is-whitespace fstr
					      :start (+ squig 2)))
		  (cond ((not postskip)
			 ;; This shouldn't happen, so do something
			 ;; inelegant to avoid trouble
			 (setq postskip slen))))
		 (t
		  (setq seg "~")
		  (setq postskip (+ squig 1))))))))
	 
;;;;(format nil fstr)))

;;;;(set-dispatch-macro-character #\! #\" #'quote-funmac ytools-readtable*)


(defun print-spaces (number stream)
    (dotimes (n number) (write-char #\Space stream))   )

(defun list-if-not-false (x)
   (cond (x (list x))
	 (t '())))

(declaim (inline car-eq))
(defun car-eq (x y) (and (consp x) (eq (car x) y)))

(defun series (l &optional h i)
   (cond ((null i)
          (setf i 1)
          (cond ((null h) (setf h l) (setf l 1))   ))   )
   (cond ((> l h) nil)
         (t (make-series l h i))   ))

(defun make-series (l h i)
   (declare (fixnum l h i))
   (let ((ans (list l)))
      (do ((tail ans)
           (l (+ l i) (+ l i)))
          ((> l h) ans)
         (setf (cdr tail) (list l))
         (setf tail (cdr tail))   )))

(defun symbol-is-non-function (x)
  (or (macro-function x)
      (special-operator-p x)))

(defgeneric condense (x))

(defmethod condense ((x t))
   (cond ((or (symbolp x) (simple-string-p x) (numberp x))
	  x)
	 ((consp x)
	  (cons (condense (car x)) (and (cdr x) '(--))))
	 ((simple-vector-p x)
	  (cond ((= (array-dimension x 0) 0) '#())
		((= (array-dimension x 0) 1)
		 (vector (condense (svref x 0))))
		(t
		 (vector (condense (svref x 0)) '--))))
	 (t (format nil "--~s--" (type-of x)))))

(defun take (n l)
   (declare (type fixnum n)
            (type list l))
   (cond ((< n 0)
          (let ((g (length l)))  (subseq l (+ g n) g)   ))
         (t (subseq l 0 n))   ))

(defun drop (n l)
   (declare (type fixnum n)
            (type list l))
   (cond ((< n 0) (subseq l 0 (+ (length l) n)))
         (t (subseq l n (length l)))   ))

(defun occurs-in (x tr)
   (cond ((eql x tr) true)
	 ((atom tr) false)
	 (t
	  (or (occurs-in x (car tr))
	      (occurs-in x (cdr tr))))))

#|
(def-excl-dispatch #\? (srm _)
  (labels ((read-like-sym (chars)
	      (let ((*package* keyword-package*))
		 (symbol-name
		    (read-from-string (coerce chars 'string))))))
     (do ((ch (read-char srm) (read-char srm))
	  (chars-to-colon '() (cons ch chars-to-colon)))
	 ((char= ch #\:)
	  (progn
	     (cond ((char= (peek-char false srm) #\:)
		    (read-char srm)))
	     (let ((pkg-name (read-like-sym (reverse chars-to-colon))))
		(let ((pkg (find-package pkg-name)))
		   (cond (pkg
			  (do ((ch (peek-char false srm) (peek-char false srm))
			       (chars-to-delim '() (cons ch chars-to-delim)))
			      ((or (is-whitespace ch)
				   (char= ch #\()
				   (char= ch #\)))
			       (let ((symname (read-like-sym (reverse chars-to-delim))))
				  (let ((sym (find-symbol symname pkg)))
				     (or sym
					 (progn
					    (cerror "I will create it"
						"Symbol ~s not found in package ~s"
						 symname pkg)
					    (intern symname pkg))))))
			     (read-char srm)))
			 (t
			  (error "Package not found: ~s" pkg-name))))))))))
|#

(defmacro on-list (&rest whatever)
  `(push ,@whatever))

(defmacro on-list-if-new (&rest whatever)
  `(pushnew ,@whatever))

(defmacro off-list (l^) `(pop ,l^))

(declaim (special constant-condtests-silent*))

;;; Used to turn off complaints about unreachable code.
(cond (constant-condtests-silent*
       (defmacro -- (x) x))
      (t
       (defmacro -- (x) `(rv ,x))))

(defun rv (v) v)

(defmacro loading-bogus (&rest whatever)
   `(eval-when (:compile-toplevel :load-toplevel)
       (error "Loading bogus file: ~s"
	      ',whatever)))
