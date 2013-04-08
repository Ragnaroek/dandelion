;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-
;;;; ------------------------------------------------------------------------------------------
;;;;     Title:	Parsing of SGML input
;;;;   Created:	1996/10/21 11:25:17
;;;;    Author: Gilbert Baumann <gilbert@ma2s2.mathematik.uni-karlsruhe.de>
;;;; ------------------------------------------------------------------------------------------
;;;;  (c) copyright 1996,97 by Gilbert Baumann

(provide 'sgml-parse)
;;(require 'sgml-lex)
;;(require 'dtd)

(defvar *html-dtd*)
(defvar *%top-element*)

;;; In principle each element needs additional information on how to
;;; process withe space as there a three options:
;;;
;;;  - collapse newlines and multiple occurences of white space
;;;  - preserve the exact formating
;;;  - even include comments  (thanks to the STYLE element)
;;; XMP and PLAIN whould even require:
;;;  - turn off processing of markup until end tag seen.
;;;

;;; Further we have to more carefully parsing white space at beginning
;;; and ending of tags. Initial white space has to be retained after
;;; end tags and before start tags.
;;;
;;; Example: 
;;;   <P> <B>foo</B> <I>bar</I> baz
;;; has to be tokenized into:
;;;   <P> <B> "foo" </B> " " <I> "bar" </I> " baz"
;;;
;;; However this processing belongs to the typesetter, since it is not
;;; specified at all how an SGML parser should handle white space.
;;; 

;;; Resolving conflicts:
;;;
;;; When we encounter an illegal situation, when we see a start tag not
;;; allowed, we could proceed in two ways:
;;;
;;;  a. close anything until start tags becomes legal again
;;;  b. ignore the tag
;;;
;;;   Both alternatives could be equally harmful. For instance, when
;;; we see: "<B> foo <TD> ..." it is probably more appropriate to
;;; close the open <B> element. On the other hand, when we encounter
;;; "<UL> <FONT face=times> <LI> baz" it is a much better idea to
;;; ignore the FONT tag alltogether to retain the structure.
;;;
;;;   So the only real alternative I see here is to specify a
;;; hierarchy of importance of tags. However best results are probably
;;; gained by indeterministically try both alternatives and choose the
;;; one, which produces fewer parsing error while continuing
;;; processing.

;;; Currently the situation is as follows:
;;; when we encounter a new tag, we have the following information:

;;;   1. a set of possible sub-elements (the inclusion)
;;;   2. a stack of currently open elements

;;; We could now insert additional tags into the input stream to
;;; achieve legalness of the tag at hand by:

;;;  a. close any number open element with optional end tags.
;;;  b. open any number of elements from the inclusion with optional
;;;     start tags.

;;; However this process could face us to multiple choices, what
;;; should been called an ambiguous situation. When your DTD is sane
;;; such an situation should not been able to araise.

;;; If there is no such path found, we would normally just quit
;;; service announcing a parsing error. Within an interactive web
;;; browser and the vast amount of malformed HTML floating around the
;;; Web, this is no feasible way to go. I'll then choose to retry the
;;; aproach above with opening and closing also elements, which do not
;;; have an optional start or end tag. Expirience showed, that just
;;; ignoreing the questable tag at hand is not reasonable way to go.
;;; However the question still remains, when to ignore and when to
;;; force. 

(defun find-element* (dtd name &rest more)
  (cond ((eq name '%top) *%top-element*)
	((apply #'find-element dtd name more)) ))

(defun find-element-exclusion (dtd name)
  (element-exclude (find-element* dtd name)))

(defun find-element-inclusion (dtd name)
  (element-include (find-element* dtd name)))

(defstruct (pt (:constructor make-pt/low))
  name attrs children parent)

(defun make-pt (&key name attrs children)
  (let ((res (make-pt/low :name name :attrs attrs :children children)))
    (dolist (k children)
      (setf (pt-parent k) res))
    res))

(defvar *look-ahead*)

(defvar *pt-id* 0)

(defvar *debug-parser-p* nil)
(defvar *parse-warn-level* 0)
(setq *parse-warn-level* 3)

(defun parse-warn (level fmt &rest args)
  (let ((*print-pretty* nil))		;disable ugly^H^H^H^Hpretty printing 
    (when (>= level *parse-warn-level*)
      (unless *options/parser-silent-p*
	(format *trace-output* "~&Warning:~4D: ~A~%" 
		*line-number* ;;(ignore-errors (file-position *input*))
		(apply #'format nil fmt args)) ))))

;;; Warning Levels
;;; --------------
;;;  1 - Absolutely normal mumbleing
;;;  2 - Laziness introduced by the HTML standard
;;;  3 - Semantic error upon parsing attributes
;;;  4 - Accidents happening while parsing the structure
;;;  5 - More serve errors
;;;

(defun push-back (a b c)
  (when *debug-parser-p* 
    (print (list '---> 'push-back a b c)))
  (push (list a b c) *look-ahead*))

(defun skip-whitespaces-from-stream (stream)
  "Slurp up all white spaces from the input stream `stream'."
  (do ((ch (sgml/read-char stream nil :eof) (sgml/read-char stream nil :eof)))
      ((or (eq ch :eof) (not (whitespacep ch)))
       (unless (eq ch :eof)
	 (sgml/unread-char ch stream)))))

(defun skip-newlines-from-stream (stream)
  "Slurp up all newlines from the input stream `stream'."
  (do ((ch (sgml/read-char stream nil :eof) (sgml/read-char stream nil :eof)))
      ((or (eq ch :eof) (not (char= ch #\newline)))
       (unless (eq ch :eof)
	 (sgml/unread-char ch stream)))))

(defun read-token/2 (dtd input pcdata-expected? preserve?)
  (declare (ignore first-p pcdata-expected-p))
  (multiple-value-bind (b/e tag xtra) (read-token dtd input t)
    (cond ((eq tag :pcdata)
	   (multiple-value-bind (next-b/e next-tag next-xtra) (read-token/2 dtd input pcdata-expected? preserve?)
	     (unless preserve?
	       (setq xtra (sanify-string/2 xtra nil nil)))
	     (cond ((and (not pcdata-expected?) (every #'(lambda (x) (whitespacep x)) xtra))
		    ;; The whitespace conventions alone are not suffiecient,
		    ;; we need to ignore 'empty' PCDATA during passages, where
		    ;; no PCDATA is expected at all.
		    (values next-b/e next-tag next-xtra))
		   (t
		    (cond
		     ;; Any sequence of white space before a end tag must be ignored,
		     ;; so look if the next token is an end tag.
		     ((eq next-b/e 'e)
		      ;; It is an end tag, so kill trailing white space
		      (if preserve?
			  (setf xtra (string-right-trim '(#\newline) xtra))
			(setf xtra (string-right-trim '(#\newline #\tab #\space) xtra)))))
		    (cond ((= (length xtra) 0) ;string empty?
			   ;; return the look ahead instead
			   (values next-b/e next-tag next-xtra))
			  (t
			   ;; Put back the token read into look ahead queue
			   (push-back next-b/e next-tag next-xtra)
			   ;; return what we found
			   (values b/e tag xtra)))))))
	  ((and (eq b/e 'b) (not preserve?))
	   ;; Any sequence of white space must be ignored after a
	   ;; start tag.
	   (skip-whitespaces-from-stream input)
	   (values b/e tag xtra))
	  ((and (eq b/e 'b) preserve?)
	   ;; In preserve mode (e.g. within <PRE> elements open, the white
	   ;; space convention only affects newline's
	   (skip-newlines-from-stream input)
	   (values b/e tag xtra))
	  (t
	   ;; Nothing happens, just return what we found.
	   (values b/e tag xtra)) )))
		
(defun consume-token (dtd input pcdata-expected? preserve?)
  (let ((x (pop *look-ahead*)))
    (cond (x
	   (values (first x) (second x) (third x)))
	  (t
	   (read-token/2 dtd input pcdata-expected? preserve?)))))

(defun pretty-print-tag-list (lst)
  (cond ((null lst) "")
	((concatenate 'string (format nil "<~A~A> " (if (eq (caar lst) 'e) "/" "") (cadar lst))
		      (pretty-print-tag-list (cdr lst))))))

(defun parse (input dtd stack exclusion)
  (let* ((elm (find-element* dtd (car stack)))
	 (expect-set (set-difference (element-include elm) exclusion))
	 (oend?      (element-oend? elm)))
    (parse2 input dtd stack exclusion expect-set oend?)))

(defun parse3 (input dtd ielm)
  (parse2 input dtd nil nil (list ielm) t))

(defun parse2 (input dtd stack exclusion expect-set oend?
	       &aux (res nil))
  (if (null expect-set)
      nil
    (loop
      (multiple-value-bind (b/e name extra) 
	  (consume-token dtd input (member :pcdata expect-set) (member :pre stack))
	(cond 
	 ((null (find-element* dtd name nil nil))
	  (parse-warn 2 "??! There is such thing as <~A> defined -- ignored." 
		      name))
	 ((eq b/e 'b)
	  (cond 
	   ((member name expect-set)
	    (push (make-pt :name name
			   :attrs (if (eq name :pcdata) extra (list* :%id (incf *pt-id*) extra))
			   :children (parse input dtd (cons name stack) (union (find-element-exclusion dtd name) exclusion)))
		  res))
	   (t
	    (let ((pathen (progn
			    ;;(print (list 'find-pathen stack name ))
			    (find-pathen dtd stack name exclusion))))
	      (when (null pathen)
		(parse-warn 4 "Trying insane pathen also. Because of ~A seen in ~A context." name stack)
		(setq pathen (find-pathen dtd stack name exclusion t)))
	      (and *debug-parser-p*
		   (print (list '--path-- (car stack) name pathen)))
	      (cond ((and t (null pathen))
		     (parse-warn 4 "There is no path from ~S to ~S -- ignored. [stack = ~A]"
				 (car stack) name (let ((*print-length* 3)) (write-to-string stack))) )
				       
		    ((null pathen)
		     (parse-warn 4 "There is no path from ~S to ~S. [stack = ~A]"
				 (car stack) name (let ((*print-length* 3)) (write-to-string stack)))
		     (parse-warn 4 "           therefor I'll close ~S." (car stack))
		     (push-back 'b name extra)
		     (push-back 'e (car stack) nil) )
					
		    ((not (null (cdr pathen)))
		     (error "A path from ~S to ~S is ambiguous.~%Namely ~S."
			    (car stack) name pathen))
		    (t
		     (parse-warn 1 "I'll insert ~A because of ~A --> ~A. [stack = ~A]"
				 (pretty-print-tag-list (car pathen))
				 (car stack) name
				 (let ((*print-length* 3)) (write-to-string stack)))
		     (assert (not (null (car pathen))))
		     (push-back b/e name extra)
		     (dolist (k (car pathen))
		       (push-back (first k) (second k) (third k))) ))))))
	 ((eq b/e 'e)
	  (cond 
	   ((eq name (car stack))
	    (return-from parse2 (reverse res)))
	   ((not (member name stack))
	    (parse-warn 3 "`</~A>' seen without any opening tag -- ignored. -- [stack = ~A]"
			name name 
			(let ((*print-length* 3)) (write-to-string stack))) )
	   ((eq oend? t)
	    (parse-warn 1 "Optional close: </~A> because of </~A> -- [stack = ~A]" 
			(car stack) name
			(let ((*print-length* 3)) (write-to-string stack)))
	    (push-back b/e name extra)
	    (push-back 'e (car stack) nil) )
	   ((member name stack)
	    (parse-warn 3 "I'll close ~A unauthorized because of </~A>. -- [stack = ~A]"
			(car stack) name
			(let ((*print-length* 3)) (write-to-string stack)))
	    (push-back b/e name extra)
	    (push-back 'e (car stack) nil) )
	   (t
	    (parse-warn 3 "</~A> will be ignore because it is illegal within ~A. -- [stack = ~A]"
			name (car stack)
			(let ((*print-length* 3)) (write-to-string stack))) )) )) )) ))

(defun find-pathen/aux (dtd cur wish exclusion path receiver)
  (cond ((null cur))
	(t
	 (let* ((elm (find-element* dtd cur))
		(expect (set-difference (element-include elm) exclusion)))
	   (cond ((member wish expect)
		  (funcall receiver path))
		 (t
		  (setq expect (remove-if-not #'(lambda (x) (element-obegin? (find-element* dtd x))) expect))
		  (dolist (e expect)
		    (find-pathen/aux dtd e wish (union (find-element-exclusion dtd e) exclusion) 
				     (cons (list 'b e nil) path)
				     receiver))))))))

(defun find-pathen (dtd stack wish exclusion &optional insane?)
  (let ((res nil))
    (cond ((null stack) nil)
	  (t
	   (find-pathen/aux dtd (car stack) wish exclusion nil #'(lambda (x) (push x res)))
	   (cond (res)
		 ((cond ((or insane? (element-oend? (find-element* dtd (car stack))))
			 (mapcar #'(lambda (x)
				     ;;(cons (list 'e (car stack) nil) x)
				     (append x (list (list 'e (car stack) nil))) )
				 (find-pathen dtd (cdr stack) wish exclusion)))
			(t nil))))))))

(defun ppt (pt &optional (prefix "") (barp nil))
  (cond ((eq (pt-name pt) :pcdata)
	 (let ((s (progn #+NIL sanify-string (pt-attrs pt))) flag)
	   (if (> (length s) (- 120 (length prefix)))
	       (setq s (concatenate 'string (subseq s 0 (- 120 (length prefix))))
		     flag t))
	   (write-string (format nil "~%~A| \"~A\" ~A" prefix s
				 (if flag "..." "")))))
	(t
	 (write-string (format nil "~%~A| ~A" prefix (pt-name pt)))
	 (when (pt-children pt)
	   (write-string (format nil "~%~A~A-~A." 
				 prefix 
				 (if barp "+" "`")
				 (make-array (length (symbol-name (pt-name pt))) 
					     :initial-element #\-
					     :element-type *base-char*)))
	   (let ((prefix1 (concatenate 'string 
			    prefix (if barp "|" " ")
			    (make-array (length (symbol-name (pt-name pt))) 
					:initial-element #\space
					:element-type *base-char*)
			    " ")))
	     (do ((q (pt-children pt) (cdr q)))
		 ((null q))
	       (ppt (car q) prefix1 (if (cdr q) 't 'nil))))))))

(defun sanify-string (x)
  (sanify-string/2 x t t))

(defun sanify-string/2 (string &optional (begin? t) (end? t))
  (let ((i (position-if #'(lambda (x) (whitespacep x)) string)))	;whitespacep defined in parse.lisp
    (cond (i
	   (let ((j (position-if-not #'(lambda (x) (whitespacep x)) string :start i)))
	     (if j
		 (concatenate 'string (subseq string 0 i)
			      (if (and (= i 0) begin?) "" " ")
			      (sanify-string/2 (subseq string j) nil end?))
	       (concatenate 'string (subseq string 0 i)
			    (if (not end?) " " "")))))
	  (t string))))

(defun sgml-parse (input dtd)
  (let ((*look-ahead* nil))
    (car (parse input dtd (list '%top) nil))))

(defun parse4 (input dtd ielm)
  ;; This is still a big bad hack
  (let ((*line-number* 1)
	(*look-ahead* nil)
	(*%top-element* (make-element :name '%top
				      :oend? t
				      :include (list ielm)
				      :exclude nil)))
    (skip-whitespaces-from-stream input)
    (parse input dtd (list '%top) nil)))
  
(defun html-parse (input)
  (car (parse4 input *html-dtd* :HTML)))


