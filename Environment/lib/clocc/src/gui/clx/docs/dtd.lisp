;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-
;;;; --------------------------------------------------------------------------
;;;;     Title:	A very first approach to parse DTD's directly
;;;;   Created:	Tue Oct 14 13:52:09 1997
;;;;    Author: Gilbert Baumann <gilbert@ma2s2.mathematik.uni-karlsruhe.de>
;;;; --------------------------------------------------------------------------
;;;;  (c) copyright 1997 by Gilbert Baumann

(provide 'dtd)
;;(require 'lalr)
;;(require 'clex)

(defvar *entities*)

(defstruct element
  name
  include
  exclude
  obegin?
  oend?
  attlist)

(defstruct (dtd (:print-function print-dtd))
  name
  elements)

(defun print-dtd (self sink depth)
  (declare (ignore depth))
  (format sink "#<~S ~S>" (type-of self) (dtd-name self)))

(clex:deflexer dtd
    ((alpha (or (range "a" "z") (range "A" "Z")))
     (digit (range "0" "9"))
     (white (or #\space #\newline #\tab #\page))
     (name-start alpha)
     (name-char (or alpha digit "." "-"))
     
     (string-char (or white (range #\space #\!) (range #\# #\&) (range #\( #\~)))
     
     (name (and name-start (* name-char)))
     (any (or white (range #\space #\~))) )
  
  ;;slurp away white spaces
  ((white))		
  ;; slurp away comments
  ("--" (clex:begin 'comment))
  ((clex::in comment "--") (clex:begin 'clex:initial))
  ((clex::in comment any))
  
  (("%" name ";")
   ;;this is a defined entity
   (let ((looked (cdr (assoc (subseq clex:bag 1 (- (length clex:bag) 1)) *entities* :test #'string=))))
     (cond (looked
	    (do ((i (- (length looked) 1) (- i 1)))
		((< i 0))
	      (clex:backup (char looked i))))
	   ((error "Entity ~A is not defined." clex:bag)) )))
  (("%" name)
   ;;this is a defined entity
   (let ((looked (cdr (assoc (subseq clex:bag 1) *entities* :test #'string=))))
     (cond (looked
	    (do ((i (- (length looked) 1) (- i 1)))
		((< i 0))
	      (clex:backup (char looked i))))
	   ((error "Entity ~A is not defined." clex:bag)) )))
  
  ("<!" 
   (return :open))
  (">"
   (return :close))
  
  ("ENTITY"  (return :entity))
  ("ATTLIST" (return :attlist))
  ("ELEMENT" (return :element))
  ("#REQUIRED" (return :required))
  ("#IMPLIED" (return :implied))
  ("#PCDATA"  (return :pcdata))
  ("#FIXED"  (return :fixed))
  (name (return (list :name clex:bag)))

  ("+(" (clex:backup #\() (return :plus-prefix))
  ("-(" (clex:backup #\() (return :minus-prefix))
  
  ((+ digit)
   (return (list :number (parse-integer clex:bag))))
  
  ;;singetons
  ((or "%()|+-*?,&")
   (return (char clex:bag 0)))
  ;;
  ((and #\" (* (or string-char #\')) #\")
   (return (list :string (subseq clex:bag 1 (- (length clex:bag) 1)))))
  ((and #\' (* (or string-char #\")) #\')
   (return (list :string (subseq clex:bag 1 (- (length clex:bag) 1))))) )

(locally
  (declare (optimize (speed 3) (debug 0) (safety 2)))

(lalr:define-grammar dtd-parser (:open :close :entity :attlist :element :name 
				 #\% #\( #\) #\| #\+ #\- #\* #\? #\, #\&
				 :string
				 :plus-prefix :minus-prefix
				 :fixed
				 :required :implied :number :pcdata)
  (start --> definition start #'cons)
  (start --> #'(lambda () nil))
  
  (definition --> :open :close #'(lambda (x y) x y nil))
  (definition --> :open :entity #\% :name :string :close
	      #'(lambda (i0 i1 i2 name string i3)
		  i0 i1 i2 i3
		  (push (cons name string) *entities*)
		  (list 'defentity name string)
		  nil))
  (definition --> :open :entity :name :name :string :close
	      #'(lambda (i0 i1 name i2 string i3)
		  i0 i1 i2 i3
		  (print (list `(,name as ,i2 = ,string)))
		  nil))
  (definition --> :open :attlist production attliste :close
	      #'(lambda (i0 i1 name attlist i2)
		  i0 i1 i2
		  (list 'defattlist name attlist)))
  (definition --> :open :element production odef odef production maybe-pm :close 
	      #'(lambda (i0 i1 name open close prod pm i2)
		  i0 i1 i2
		  (princ ".")
		  (list 'defelement name open close prod pm)))
  
  (attliste --> #'(lambda () nil))
  (attliste --> ident production value attliste
	    #'(lambda (n p v m)
		(cons (list n p v) m)))
  (attliste --> ident production :fixed value attliste
	    #'(lambda (n p v i m)
		i
		(cons (list n p v) m)))
  
  (maybe-pm --> #'(lambda () nil))
  (maybe-pm --> :plus-prefix production		#'(lambda (i p) i (list '+ p)))
  (maybe-pm --> :minus-prefix production	#'(lambda (i p) i (list '- p)))
  
  (odef --> #\-   #'(lambda (i) i '-))
  (odef --> ident #'identity)
  (production --> p1 #'identity)
  
  (p1 --> p2 #\| p1				#'(lambda (a b c) b (as-cons 'or a c)))
  (p1 --> p2					#'identity)
  (p2 --> p3 #\, p2				#'(lambda (a b c) b (as-cons 'and a c)))
  (p2 --> p3 #\& p2				#'(lambda (a b c) b (as-cons 'amp a c)))
  (p2 --> p3					#'identity)
  (p3 --> p4 #\*				#'(lambda (a b) b (list '* a)))
  (p3 --> p4 #\+				#'(lambda (a b) b (list '+ a)))
  (p3 --> p4 #\?				#'(lambda (a b) b (list '? a)))
  (p3 --> p4					#'identity)
  (p4 --> #\( production #\)			#'(lambda (a b c) a c b))
  (p4 --> ident					#'identity)
  (p4 --> :pcdata				#'identity)
  (p4 --> :number				#'identity)

  (value --> :implied				#'(lambda (a) a :implied))
  (value --> :required				#'(lambda (a) a :required))
  (value --> ident				#'identity)
  (value --> :string				#'identity)
  (value --> :number				#'identity)
  
  (ident --> :name
	 #'(lambda (x) (intern (string-upcase x) :keyword))) ))

(defun as-cons (op x y)
  (cond ((and (consp y) (eq (car y) op))
	 (list* op x (cdr y)))
	((list op x y))))

(defun parse-dtd/low (input)
  (let ((lexer (make-dtd-lexer input))
	(*entities* nil))
    (labels ((next-input ()
	       (let ((x (funcall lexer)))
		 ;(print x)
		 (cond ((eq x :eof) (values :eof :eof))
		       ((atom x) (values x x))
		       (t (values (first x) (second x))))))
	     (parse-error ()
	       (format T "Parse-Error! at pos = ~D"
		       (file-position input))))
      (dtd-parser #'next-input #'parse-error))))

(defun find-element (dtd name &optional (intern? nil) (error? t))
  (or (gethash name (dtd-elements dtd))
      (and intern?
	   (let ((new (make-element :name name)))
	     (setf (gethash name (dtd-elements dtd)) new)
	     new))
      (and error? (error "Element ~S is not defined." name))))

(defun find-element-attlist (dtd name)
  (let ((x (find-element dtd name nil nil)))
    (and x (element-attlist x))))

(defun canon-optional-tag-definition (x)
  (cond ((eq x '-) nil)
	((eq x :O) t)
	(t
	 (error "Optionalilty definition must be either '-' or 'O' - ~S. " x))))

(defun production->name-list (prod)
  (cond ((atom prod) (list prod))
	((eq (car prod) 'or)
	 (mapcan #'production->name-list (cdr prod)))
	(t
	 (error "Bogus production - ~S" prod))))

(defun production->name-list/2 (prod)
  (cond ((atom prod) (list prod))
	((member (car prod) '(or and amp + * ?))
	 (mapcan #'production->name-list/2 (cdr prod)))
	(t
	 (error "Bogus production - ~S" prod))))

(defun process-def-element (dtd name odef cdef production additional)
  (cond ((consp name)
	 (dolist (name (production->name-list name))
	   (process-def-element dtd name odef cdef production additional)))
	((let ((obegin? (canon-optional-tag-definition odef))
	       (oend?   (canon-optional-tag-definition cdef))
	       (incl    (subst :pcdata :cdata		;xxx hack here
			       (production->name-list/2 production)))
	       (excl    nil))
	   (cond ((and (consp additional) (eq (car additional) '+))
		  (setf incl (union incl (production->name-list/2 (cadr additional)))))
		 ((and (consp additional) (eq (car additional) '-))
		  (setf excl (production->name-list/2 (cadr additional))))
		 ((null additional))
		 (t
		  (error "Bogus extra inclusion/exclusion - ~S" additional)))
	   (let ((elm (find-element dtd name t)))
	     (setf (element-include elm) (if (equal incl '(:empty)) nil incl)
		   (element-exclude elm) excl
		   (element-obegin? elm) obegin?
		   (element-oend? elm)   oend?))))))

(defun process-attribute (name type value)
  (declare (ignore value))
  (setq type (production->name-list type))
  (cond ((and (= (length type) 1)
	      (member (car type) '(:cdata)))
	 (list (intern (symbol-name name) :keyword) 't))
	((and (= (length type) 1)
	      (member (car type) '(:number :name)))
	 (list name (car type)))
	((list (intern (symbol-name name) :keyword) type)) ))

(defun process-def-attlist (dtd name attlist)
  (cond ((consp name)
	 (dolist (name (production->name-list name))
	   (process-def-attlist dtd name attlist)))
	(t
	 (setf (element-attlist (find-element dtd name t))
	   (mapcar #'(lambda (x) 
		       (process-attribute (first x) (second x) (third x)))
		   attlist)))))

(defun process-dtd-def (dtd def)
  (cond ((eq (car def) 'defelement)
	 (process-def-element dtd (second def) (third def) (fourth def) (fifth def) (sixth def)))
	((eq (car def) 'defattlist)
	 (process-def-attlist dtd (second def) (third def)))
	(t
	 (error "Bogus dtd-def-form ~S" def))))

(defun parse-dtd (filename)
  (let ((dtd (make-dtd :name filename :elements (make-hash-table :test #'eq))))
    (with-open-file (in filename)
      (let ((defs (parse-dtd/low in)))
	(dolist (def defs)
	  (and def
	       (process-dtd-def dtd def)))))
    (setf (gethash :pcdata (dtd-elements dtd))
	  (make-element :name :pcdata
			:include nil
			:exclude nil))
    '(setf (gethash :cdata (dtd-elements dtd))
	  (make-element :name :cdata
			:include nil
			:exclude nil))
    dtd))

;;; Uhu! CDATA seems also to been a defined content element. E.g. upon
;;; STYLE in the HTML-4.0 DTD.
