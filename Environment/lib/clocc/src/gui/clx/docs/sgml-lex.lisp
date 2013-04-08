;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-
;;;; ------------------------------------------------------------------------------------------
;;;;     Title:	Lexical analysis of SGML input
;;;;   Created:	1996/10/21 11:25:17
;;;;    Author: Gilbert Baumann <gilbert@ma2s2.mathematik.uni-karlsruhe.de>
;;;; ------------------------------------------------------------------------------------------
;;;;  (c) copyright 1996,97 by Gilbert Baumann

(provide 'sgml-lex)
;;(require 'dtd)

(defvar *options/parser-silent-p* nil)

(defmacro whitespacep (ch)
  `(member ,ch '(#\Return #\Newline #\Space #\Tab #\Page)) )

(defvar *line-number*)

(defun sgml/read-char (stream &optional (eof-error-p t) (eof-value nil))
  (let ((res (read-char stream eof-error-p eof-value)))
    (cond ((and (characterp res) (char= res #\newline))
	   (incf *line-number*)))
    res))

(defun sgml/unread-char (ch stream)
  (when (char= ch #\newline)
    (decf *line-number*))
  (unread-char ch stream))

(defun sgml/peek-char (&optional (peek-type nil) (source *standard-input*)
				 (eof-error-p T) eof-value)
  (cond ((eq peek-type T)
	 (do ((ch (sgml/read-char source eof-error-p '%the-eof-object%)
		  (sgml/read-char source eof-error-p '%the-eof-object%)))
	     ((or (eq ch '%the-eof-object%)
		  (not (whitespacep ch)))
	      (cond ((eq ch '%the-eof-object%) eof-value)
		    (t (sgml/unread-char ch source) ch)) )))
	((eq peek-type NIL)
	 (let ((ch (sgml/read-char source eof-error-p '%the-eof-object%)))
	   (cond ((eq ch '%the-eof-object%) eof-value)
		 (t (sgml/unread-char ch source)
		    ch))))
	((characterp peek-type)
	 (do ((ch (sgml/read-char source eof-error-p '%the-eof-object%)
		  (sgml/read-char source eof-error-p '%the-eof-object%)))
	     ((or (eq ch '%the-eof-object%) (char= ch peek-type))
	      (cond ((eq ch '%the-eof-object%) eof-value)
		    (t (sgml/unread-char ch source) ch)) )) ) ))

;;;; ----------------------------------------------------------------------------------------------------
;;;;  Lexical analysis
;;;;

;;;; We want probably carefully read the
;;;; 'http://www.w3.org/pub/WWW/MarkUp/SGML/sgml-lex' document.  But not too
;;;; carefully since. e.g. '<A HREF=//foo/bar>' is illegal syntax due to that
;;;; document but is too common praxis on too many web documents to die on
;;;; that. 

;;;; What we should read carefully is indeed the section about wether
;;;; something is markup or not. (e.g. '<<!>' is way to escape '<' and &<!>
;;;; for '&')

;;;; Q: Should we split here between words allready.

;;;; Q: We could easily have a bag of preallocated strings and 'free' them
;;;;    after parsing.  (If this is the right way to go will be decided by the
;;;;    quality of your garbage collection.)

;;;;  N: For proper parsing of <PRE> tags the lexer should know when in pre
;;;;    mode. Since most text in a page is not non-PRE, we could esily just
;;;;    consume whitespaces and returning a standard whitespace tag.

;;;; N: also when reading a <STYLE> tag <!-- --> is no longer a comment!

;;;; N: some html files (I have an example somewhere) do not care that
;;;;    <!-----> is illegal.

(defun read-token (dtd source &optional no-skip?)
  "Reads one token from 'source'."
  (let ((ch (sgml/peek-char (not no-skip?) source nil :eof)))
    (cond ((eq ch :eof)       (values 'e '%top nil))
	  ((char= ch #\<)     (read-tag dtd source))
	  ((char= ch #\&)     (read-& source))
	  (t                  (read-pcdata source) ) )) )

(defmacro define-&-entities (&rest x)
  `(progn (defvar *&entities*)
	  (setq *&entities*
		',(mapcar #'(lambda (x)
			      (cons (car x)
				    (if (< (cadr x) 256)
					(string (code-char (cadr x)))
				      (cadr x)))) x))
	  '*&entities*))

(defun lookup-& (s)
  (let ((len (length s)))
    (dolist (k *&entities*
	       (progn
		 ;; nothing found
		 (unless *options/parser-silent-p*
		   (print `(unknown & ,s)))
		 (concatenate 'string "&" s)))
      (when (and (>= (length (car k)) len)
		 (string= (car k) s :end1 len))
	(return (cdr k)) ))))

(defun read-&-prim (source)
  (let ((res (make-array 6 :element-type (array-element-type "") :fill-pointer 0 :adjustable t))
	(looked nil)
	(ch (sgml/peek-char nil source)) )
    (cond ((alpha-char-p ch)
	   ;; named entity
	   (do ((ch (sgml/read-char source) (sgml/read-char source)))
	       ((not (alpha-char-p ch))
		(unless (char= ch #\;) (sgml/unread-char ch source))
		(lookup-& res))
	     (vector-push-extend ch res 10)
	     (cond ((setq looked (cdr (assoc res *&entities* :test #'string=)))
		    (setq ch (sgml/read-char source))
		    (unless (char= ch #\;) (sgml/unread-char ch source))
		    (return looked)) ) ))
	  
	  ((char= ch #\#)
	   (sgml/read-char source)
	   (do ((ch (sgml/read-char source) (sgml/read-char source)))
	       ((not (digit-char-p ch))
		(cond ((= (length res) 0)
		       ;; no digits read
		       (concatenate 'string "&#" (string ch)))
		      
		      ((< (setq res (parse-integer res :radix 10)) 256)
		       (unless (char= ch #\;) (sgml/unread-char ch source))
		       (string (code-char res)))

		      (t
		       (parse-warn 3 "Warning: '&#~D;' seen -- This system has only octet characters." res)) ))
	     (vector-push-extend ch res 10)) )
	  
	  (t "&") )))

(defun read-& (source)
  (sgml/read-char source)					;consume '&'
  (values 'b :pcdata (read-&-prim source)) )

(defun read-pcdata (source)
  (labels ((aux (source n)
	     (declare (optimize (speed 3) (safety 0)))
	     (let ((ch (sgml/read-char source nil :eof)))
	       (cond ((eq ch :eof)
		      (make-array n :element-type *base-char*))
		     ((or (char= ch #\<) (char= ch #\&))
		      (sgml/unread-char ch source)
		      (make-array n :element-type *base-char*))
		     (t
		      (let ((res (aux source (+ n 1))))
			(setf (schar res n) ch)
			res))))))
    (values 'b :pcdata (aux source 0))))

;; These are according to http://www.w3.org/pub/WWW/TR/WD-sgml-lex-060615
;;
(defun name-char-p (ch)
  (or (alphanumericp ch) (char= ch #\.) (char= ch #\-)) )

(defun name-start-char-p (ch)
  (alpha-char-p ch))

(defun read-tag (dtd source)
  (sgml/read-char source)  ;slurp away the #\<
  ;; Now look at the next character to decide what to do
  (let ((ch (sgml/peek-char nil source)))
    (cond ((name-start-char-p ch)
	   (read-ordinary-tag source dtd 'b))
	  ((or (char= ch #\/) (char= ch #\?))
	   ;;start of an another tag??
	   (sgml/read-char source)		;consume that '/', '?' (ch holds it thru')
	   (cond ((name-start-char-p (sgml/peek-char nil source))
		  ;;o.k valid name start char here proceed
		  (case ch
		    (#\/ (read-ordinary-tag source dtd 'e))		;normal end tag
		    (#\? (read-local-tag source)			;local or experimental tag
			 (read-token dtd source))))
		 (t
		  ;; no markup -- return pcdata
		  (values 'b :pcdata (concatenate 'string "<" (string ch)))) ))
	  #+NIL
	  ((char= ch #\!)
	   (values 'b :pcdata "<"))
	  ((char= ch #\!)
	   (sgml/read-char source)					;consume
	   (read-define-tag dtd source))				;SGML define tags
	  (t
	   ;; no markup -- return pcdata
	   (values 'b :pcdata "<") ) )) )

(defun read-name (source &optional (intern? :keyword))
  (let ((scratch (make-array 0 :element-type *base-char* :fill-pointer 0 :adjustable t)))
    (let ()
      (do ((ch (sgml/read-char source) (sgml/read-char source)))
	  ((not (name-char-p ch))
	   (sgml/unread-char ch source)	;push char back
	   (if intern?
	       (intern (nstring-upcase scratch) intern?) ;xxx
	     (copy-seq scratch)))	;upcase or no upcase?
	(vector-push-extend ch scratch 10) ))))

(defun read-local-tag (source)
  ;; Since it not specified at all how a local tag should look like, I
  ;; simply skip anything until a ">"
  (loop while (not (char= (sgml/read-char source) #\>))))

;;; http://www.w3.org/pub/WWW/TR/WD-sgml-lex-060615 says:
;;;
;;;  If the ATTLIST declaration specifies an enumerated list of names,
;;;  and the value specifiaction is one of those names, the attribute
;;;  name and "=" may be omitted.
;;;
;;; Hence in '<A foo>' foo is not a attribute but a *value*.
;;;
;;; It further says (somewhat hidden):
;;;
;;;    Section 7.9.3 of SGML says that an attribute value literal is interpreted
;;;    as an attribute value by:
;;;
;;;      o Removing quotes
;;;      o Replacing character and entity references
;;;      o Deleting character 10 (ASCII LF)
;;;      o Replacing character 9 and 13 (ASCII HT and CR) with character 32 (SPACE)
;;;
;;; Questions:
;;;  - should a name token been converted to upcase?
;;;  - could end tags consume attributes
;;;  - It would be a good idea to make the too common '<A href=//foo/bar/baz>'
;;;    a legal construct. Even pages from www.w3.org use the illegal <FOO bar=50%>
;;;    #\% is no literal character.
;;; 

(defun read-literal (source)
  (let ((scratch (make-array 0 :element-type *base-char* :fill-pointer 0 :adjustable t)))
    (let ((delim (sgml/read-char source)) )
      (do ((ch (sgml/read-char source) (sgml/read-char source)))
	  ((char= ch delim)
	   (copy-seq scratch)) 
	(cond ((char= ch #\&)
	       (let ((a (read-&-prim source)))
		 (do ((i 0 (+ i 1)))
		     ((>= i (length a)))
		   (let ((c (aref a i)))
		     (vector-push-extend c scratch 10))))) ;xxx convert [tab,return]->space delete newline
	      ;; was: (doseq (c (read-&-prim source)) ...)
	      ((char= ch #\Tab)
	       (vector-push-extend #\Space scratch 10))
	      ((char= ch #\Return)
	       (vector-push-extend #\Space scratch 10))
	      ((char= ch #\Newline)
	       ;; discard newlines
	       nil)
	      (t
	       (vector-push-extend ch scratch 10)) ))) ))

(defun read-value (source)
  (let ((ch (sgml/peek-char t source)))
    (cond ((or (char= ch #\") (char= ch #\'))
	   (read-literal source))
	  ((name-char-p ch)
	   (read-attlist-nam source))
	  (t
	   ;; lazy
	   (read-attlist-nam source)
	   ;; strict:
	   #| (where? source)
	      (error "Illegal start of an attlist value.")
           |#
	   ) )))

(defun read-attlist-nam (source)
  ;;Lazy way read anything til white space
  ;;Even W3C documents have <xx foo=50%>
  (let ((res (make-array 20 :element-type (array-element-type "") :fill-pointer 0 :adjustable t)))
    (do ((ch (sgml/read-char source) (sgml/read-char source)))
	((or (whitespacep ch) (char= ch #\>))
	 (sgml/unread-char ch source)
	 res)
      (vector-push-extend ch res 10)) ))

(defun valid-name-string-p (string)
  "Is the string `string' a valid name string according to the SGML
   conventions?"
  (and (> (length string) 0)
       (name-start-char-p (char string 0))
       (every #'name-char-p string)) )

(defun canon-value (dtd tag slot value)
  (let* ((attlist (find-element-attlist dtd tag))
	 (looked  (assoc slot attlist)))
    (cond ((and looked (listp (cadr looked)))
	   (or (find value (cadr looked) :test #'(lambda (x y) (string-equal (string x) (string y))))
	       (progn
		 ;; Oh yeah! monster format strings are fun!
		 (parse-warn 3 
			     "~S is a bad value for the '~A' slot of '<~A>', which could ~
                              ~{~#[not take any value~;only take '~A'~:;take one of ~@{'~A'~#[~; or ~:;, ~]~}~]~:}."
			     value slot tag (cadr looked)))))
	  ((member (cadr looked) '(:number :pixels)) ;?? pixels right here?
	   (cond ((search "%" value)	
		  ;;xxx
		  (* (or (parse-integer value :junk-allowed t)
			 100)
		     500))
		 (t
		  (parse-integer value :junk-allowed t))))
	  ((eq (cadr looked) :name)
	   (cond ((valid-name-string-p value)
		  (intern (string-upcase value) :keyword))
		 (t
		  (parse-warn 3 
			      "The `~A' slot of a <~A> element was declared ~
                               as NAME."
			      slot tag)
		  nil)))
	  (looked
	   value)
	  (t
	   (parse-warn 3 "The '<~A>' element has no '~A' slot." tag slot)
	   nil) )))

(defun find-slot-value-pair (dtd tag value)
  (let* ((attlist (find-element-attlist dtd tag))
	 (looked  nil))
    (dolist (att attlist)
      (cond ((and (listp (cadr att))
		  (setq looked (find value (cadr att) :test #'(lambda (x y) (string-equal (string x) (string y))))))
	     (return-from find-slot-value-pair (values (car att) looked)))))
    ;;fall thru'
    (parse-warn 3
		"The '<~A>' tag has no slot which could take the '~A' keyword.~%~
                 ~1{~#[There are no possible slots at all.~;~
                       Only possible slot is:~:;~
                       Possible slots are:~]~
                    ~@{~&  ~1{Slot '~A'~20T could ~{~#[not take any value~;~
                                                       only take '~A'~:;~
                                                       take one of ~@{'~A'~#[~; or ~:;, ~]~}~].~:}~}~}~:}"
		tag value (remove-if-not #'(lambda (x) (consp (cadr x))) attlist)) ))

(defun read-tag-attlist (source dtd tag)
  (let ((res nil))
    (do ((ch (sgml/peek-char t source) (sgml/peek-char t source)))
	((char= ch #\>)
	 (sgml/read-char source)				;consume
	 (nreverse res))
      (cond ((name-start-char-p ch)
	     (let ((nam (read-name source :keyword))
		   (val nil))
	       (setq ch (sgml/peek-char t source))
	       ;; Look if '=' is there?
	       (cond ((char= ch #\=)
		      (sgml/read-char source)		;consume
		      (setq val (read-value source))
		      (setq val (canon-value dtd tag nam val)) )
		     (t
		      ;; now nam is actually a value!
		      (setf (values nam val) (find-slot-value-pair dtd tag nam)) ))
	       (when val
		 (push nam res)
		 (push val res)) ))
	    (t
	     (parse-warn 3 "Bad attlist syntax")
	     (do ((ch (sgml/read-char source) (sgml/read-char source)))
		 ((char= ch #\>)
		  (return-from read-tag-attlist nil))) )))))

(defun read-ordinary-tag (source dtd b/e)
  (let* ((tag-name (read-name source))
	 (attlist  (read-tag-attlist source dtd tag-name)))
    (values b/e tag-name attlist)))

(defun slurp-away-define (source)
  (let ((c0 #\$) (c1 #\$))
    (loop
      '(unless *options/parser-silent-p*
	(princ c0))
      (psetf c0 c1 c1 (sgml/read-char source))
      (cond ((and (char= c0 #\-) (char= c1 #\-))
	     (slurp-away-define-comment source)
	     (setf c0 #\$ c1 #\$))
	    ((char= c1 #\>)
	     '(unless *options/parser-silent-p*
	       (princ c0))
	     (return nil)) ))))

(defun slurp-away-define-comment (source)
  (let ((c00 #\x)
	(c0 (sgml/read-char source))
	(c1 (sgml/read-char source)))
    (loop
      #+NIL(princ c1)
      ;;FIXME we want a better method to read these comments
      ;;and deal with rouge ones!
      #+NIL
      (when (and (char= c0 #\-) (char= c1 #\-))
	(return nil))
      (when (and (char= c00 #\-) (char= c0 #\-) (char= c1 #\>))
	;; some people go rouge with the "--" comment syntax
	;; TODO: emit warning
	;;(parse-warn "Rouge comment.")
	(sgml/unread-char #\> source)
	(return nil))
      (psetf c00 c0 c0 c1 c1 (sgml/read-char source)) )))

(defun read-define-tag (dtd source)
  (let ((ch (sgml/read-char source)))
    (cond ((name-start-char-p ch)
	   (slurp-away-define source)
	   (read-token dtd source))					;we ignore <!xx> tags entirely
	  ((char= ch #\-)
	   (setq ch (sgml/read-char source))
	   (cond ((char= ch #\-)
		  (slurp-away-define-comment source)
		  (slurp-away-define source)
		  (read-token dtd source))				;ignored
		 (t
		  ;; no markup -- return pcdata
		  (values 'b :pcdata (concatenate 'string "<!-" (string ch)))) ))
	  ((char= ch #\>)
	   ;; '<!>' seen -- empty comment
	   (read-token dtd source))
	  (t
	   ;; no markup -- return pcdata
	   (values 'b :pcdata (concatenate 'string "<!" (string ch))) ) )))


;;;; ----------------------------------------------------------------------------------------------------
;;;;  ISO-Latin-1 Entities
;;;;

(define-&-entities
  ("copy"   169)  ;copyright sign
  ("reg"    174)  ;registered sign
  ("amp"     38)  ;ampersand
  ("gt"      62)  ;greater than
  ("lt"      60)  ;less than
  ("quot"    34)  ;double quote
  ("nbsp"   160)  ;non breaking space
  ("AElig"  198)  ;capital AE diphthong (ligature)
  ("Aacute" 193)  ;capital A, acute accent
  ("Acirc"  194)  ;capital A, circumflex accent
  ("Agrave" 192)  ;capital A, grave accent
  ("Aring"  197)  ;capital A, ring
  ("Atilde" 195)  ;capital A, tilde
  ("Auml"   196)  ;capital A, dieresis or umlaut mark
  ("Ccedil" 199)  ;capital C, cedilla
  ("ETH"    208)  ;capital Eth, Icelandic
  ("Eacute" 201)  ;capital E, acute accent
  ("Ecirc"  202)  ;capital E, circumflex accent
  ("Egrave" 200)  ;capital E, grave accent
  ("Euml"   203)  ;capital E, dieresis or umlaut mark
  ("Iacute" 205)  ;capital I, acute accent
  ("Icirc"  206)  ;capital I, circumflex accent
  ("Igrave" 204)  ;capital I, grave accent
  ("Iuml"   207)  ;capital I, dieresis or umlaut mark
  ("Ntilde" 209)  ;capital N, tilde
  ("Oacute" 211)  ;capital O, acute accent
  ("Ocirc"  212)  ;capital O, circumflex accent
  ("Ograve" 210)  ;capital O, grave accent
  ("Oslash" 216)  ;capital O, slash
  ("Otilde" 213)  ;capital O, tilde
  ("Ouml"   214)  ;capital O, dieresis or umlaut mark
  ("THORN"  222)  ;capital THORN, Icelandic
  ("Uacute" 218)  ;capital U, acute accent
  ("Ucirc"  219)  ;capital U, circumflex accent
  ("Ugrave" 217)  ;capital U, grave accent
  ("Uuml"   220)  ;capital U, dieresis or umlaut mark
  ("Yacute" 221)  ;capital Y, acute accent
  ("aacute" 225)  ;small a, acute accent
  ("acirc"  226)  ;small a, circumflex accent
  ("aelig"  230)  ;small ae diphthong (ligature)
  ("agrave" 224)  ;small a, grave accent
  ("aring"  229)  ;small a, ring
  ("atilde" 227)  ;small a, tilde
  ("auml"   228)  ;small a, dieresis or umlaut mark
  ("ccedil" 231)  ;small c, cedilla
  ("eacute" 233)  ;small e, acute accent
  ("ecirc"  234)  ;small e, circumflex accent
  ("egrave" 232)  ;small e, grave accent
  ("eth"    240)  ;small eth, Icelandic
  ("euml"   235)  ;small e, dieresis or umlaut mark
  ("iacute" 237)  ;small i, acute accent
  ("icirc"  238)  ;small i, circumflex accent
  ("igrave" 236)  ;small i, grave accent
  ("iuml"   239)  ;small i, dieresis or umlaut mark
  ("ntilde" 241)  ;small n, tilde
  ("oacute" 243)  ;small o, acute accent
  ("ocirc"  244)  ;small o, circumflex accent
  ("ograve" 242)  ;small o, grave accent
  ("oslash" 248)  ;small o, slash
  ("otilde" 245)  ;small o, tilde
  ("ouml"   246)  ;small o, dieresis or umlaut mark
  ("szlig"  223)  ;small sharp s, German (sz ligature)
  ("thorn"  254)  ;small thorn, Icelandic
  ("uacute" 250)  ;small u, acute accent
  ("ucirc"  251)  ;small u, circumflex accent
  ("ugrave" 249)  ;small u, grave accent
  ("uuml"   252)  ;small u, dieresis or umlaut mark
  ("yacute" 253)  ;small y, acute accent
  ("yuml"   255)  ;small y, dieresis or umlaut mark

  ;; adobe symbol font chars
  ("universal" #o442)
  ("existential" #o444)
  ("Omega" #o527)
  ("mu" #o555)
  ("lessequal" #o643)
  ("infinity" #o645)
  ("greaterequal" #o663)

  ("lambda" #o554)
  ("pi"     #o560)
  
  ("epsilon" #o545)
  ("sigma"   #o563)
  ("Sigma"   #o523)

  ("multiply" #o664)
  ("proportional" #o665)
  ("partitaldiff" #o666)
  ("bullet" #o667)
  ("divide" #o670)
  ("notequal" #o671)
  ("equivalence" #o672)
  ("approxequal" #o673)
  ("ellipsis" #o674)
  ("arrowvertex" #o675)
  ("arrowhorizex" #o676)
  ("carriagereturn" #o677)

  ("aleph" #o700)
  ("Ifraktur" #o701)
  ("Rfraktur" #o702)
  ("weierstrass" #o703)
  ("circlemultiply" #o704)
  ("circleX" #o704)
  ("circleplus" #o705)
  ("emptyset" #o706)
  ("intersection" #o707)

  ("union" #o710)
  ("propersuperset" #o711)
  ("reflexsuperset" #o712)
  ("notsubset" #o713)
  ("propersubset" #o714)
  ("reflexsubset" #o715)
  ("element" #o716)
  ("notelement" #o717)

  ("angle" #o720)
  ("gradient" #o721)
  ("registerserif" #o722)
  ("copyrightserif" #o723)
  ("trademarkserif" #o724)
  ("product" #o725)
  ("radical" #o726)
  ("dotmath" #o727)

  ("logicalnot" #o730)
  ("logicaland" #o731)
  ("logicalor" #o732)
  ("arrowdblboth" #o733)
  ("arrowdblleft" #o734)
  ("arrowdblup" #o735)
  ("arrowdblright" #o736)
  ("arrowdbldown" #o737)

  ("lozenge" #o740)
  ("angleleft" #o741)
  ("copyrightsans" #o742)
  ("trademarksans" #o743)
  ("summation" #o744)

  ;; The characters 346..376 are thought for type setting.

  )
;;; since we have an early bail out during parsing it should be forbidden to have
;;; two entities a and b, where a is the first substring of b.
;;;

#|
10.1 White space
   
   A line break occurring immediately following a start tag must be
   ignored, as must a line break occurring immediately before an end tag.
   This applies to all HTML elements without exceptions. In addition, for
   all elements except PRE, leading white space characters, such as
   spaces, horizontal tabs, form feeds and line breaks, following the
   start tag must be ignored, and any subsequent sequence of contiguous
   white space characters must be replaced by a single word space.
   
   Since the notion of what word space is varies from script (written
   language) to script, user agents should collapse white space in
   script-sensitive ways. For example, in Latin scripts, a single word
   space is just a space (ASCII decimal 32), while in Thai it is a
   zero-width word separator. In Japanese and Chinese, a word space is
   ignored entirely.
   
   The PRE element is used for preformatted text, where white space is
   significant. The PRE element is described below.
   
   Word space processing can and should be done even in the absence of
   language information specified by the lang attribute.
|#