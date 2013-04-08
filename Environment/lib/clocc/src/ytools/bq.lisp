;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: bq.lisp,v 2.3 2006/05/22 12:08:38 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on :at-run-time %ytools/ signal misc setter outin multilet)

(defmacro bq-comma (lev spl exp)
   (signal-problem bq-comma
      "bq-comma outside of backquote: "
      "," lev (:q (spl "@")) exp))

(defmacro bq-backquote (lev exp)
   (bq-expand lev exp))

(needed-by-macros

;;; Produce an expression that quotes all but bq-comma-marked
;;; parts of 'exp'
;;; Note that we check for 'bq-comma' in two different places,
;;; which is usually a blemish in a recursive function.  Here
;;; we could work a little harder to avoid it, but leave it in
;;; because (a) efficiency is not an issue in a readmacro; 
;;; (b) the first check is for the unusual case where 'bq-comma'
;;; occurs as the very first thing in a 'bq-backquote', so it
;;; really is different, not the normal base case.
(defun bq-expand (lev exp)
   (match-cond exp
     ?( (bq-comma ?lv ?spl ?e)
       (cond ((= lv lev)
	      (cond (spl
		     (signal-problem bq-backquote
			"Meaningless: !`" lev "," lev "@" e))
		    (t e)))
	     (t
	      `(bq-comma ,lv ,spl ,(bq-expand lev e)))))
;;; Does it really make sense to expand into Qvaroids?
      ((is-Qvaroid exp)
;;;;       (let ((sym-note-exp 
;;;;		(cond ((eq (Qvaroid-sym exp) 'bq-comma)
;;;;		       (bq-expand lev `(bq-comma ,@notexp)))
;;;;		      (t
;;;;		       (bq-res-cleanup
;;;;			  `(cons ,(bq-expand lev (Qvaroid-sym exp))
;;;;				 ,(bq-expand lev (Qvaroid-notes exp)))))))
;;;;	     (atexp (bq-expand lev (Qvaroid-atsign exp)))
;;;;	     (comexp (bq-expand lev (Qvaroid-comma exp))))
       (multi-let (((symexp notexp)
		    (cond ((eq (Qvaroid-sym exp) 'bq-comma)
			   (values (bq-expand lev
					      `(bq-comma ,@(Qvaroid-notes exp)))
				   '!()))
			  (t
			   (values  (bq-expand lev (Qvaroid-sym exp))
				    (bq-expand lev (Qvaroid-notes exp))))))
		   (atexp (bq-expand lev (Qvaroid-atsign exp)))
		   (comexp (bq-expand lev (Qvaroid-comma exp))))
	  (cond ((and (car-eq atexp 'quote)
		      (car-eq comexp 'quote)
		      (car-eq symexp 'quote)
		      (or (car-eq notexp 'quote)
			  (car-eq notexp 'empty-list)))
		 `',exp)
		(t `(make-Qvaroid ,atexp ,comexp ,symexp, notexp)))))
      ((atom exp) `(quote ,exp))
      (t
       (repeat :for ((x :in (reverse exp))
		     (res '!()))
	  (match-cond x
	     ?( (bq-comma ?lv ?spl ?e)
	       (cond ((= lv lev)
		      (!= res `(,(cond (spl 'append) (t 'cons))
				,e ,res)))
		     (t
		      (!= res `(cons (list 'bq-comma ',lv ',spl ,(bq-expand lev e))
				     ,res)))))
	     (t
	      (!= res `(cons ,(bq-expand lev x) ,res))))
	:result (bq-res-cleanup res)))))

(defun bq-res-cleanup (builder)
   (cond ((equal builder '!()) ''())
	 ;;;;((null builder) '!())
	 ((eq (car builder) 'cons)
	  (let ((b1 (cadr builder))
		(b2 (bq-res-cleanup (caddr builder)))
		fq qv)
	     (!= fq (matchq ((:quote quote) ?qv)
			    b1))
	     (cond ((equal b2 '!())
                    ;;;;(equal b2 '!())
		    (cond (fq `',qv)
			  (t
			   `(list b1 ))))
		   ((car-eq b2 'list)
		    `(list ,b1 ,@(cdr b2)))
		   ((car-eq b2 'quote)
		    (cond (fq
			   `'(,qv ,@(cadr b2)))
			  (t
			   `(cons ,b1 ,b2))))
		   (t
		    `(cons ,b1 ,b2)))))
	 ((eq (car builder) 'append)
	  ;; We know there's only two args at this point
	  (let ((b1 (cadr builder))
		(b2 (bq-res-cleanup (caddr builder))))
	     (cond ((equal b2 ''())
		    ;;;;(equal b2 '!())
		    b1)
		   ((car-eq b2 'append)
		    `(append ,b1 ,@(cdr b2)))
		   (t
		    `(append ,b1 ,b2)))))
	 (t
	  (signal-problem bq-res-cleanup
	     "Unexpected form: " builder))))
		    
;;; Either :one (always level 1), :inner (innermost), or :outer (outermost).
;;; This is a constant because once experimentation is over, we're going to
;;; freeze the design decision.
(defconstant +unnum-comma-interp+ ':one)

;;; List of explicit or implicit backquote level markers,
;;; ** innermost first **
(defvar bq-levs* !())

;;;;(defvar in-bq* false)

(defvar standard-comma-reader* (get-macro-character #\, lisp-readtable*))

(defun bq-comma-reader (srm chr)
   (cond ((null bq-levs*)
	  (funcall standard-comma-reader* srm chr))
	 (t
	  (let ((lev (or (bq-read-lev srm)
			 (ecase +unnum-comma-interp+
			    (:one 1)
			    (:outer (car (last bq-levs*)))
			    (:inner (car bq-levs*)))))
		(atsign (char= (peek-char nil srm) #\@)))
	     (cond (atsign
		    (read-char srm)))
	     (let ((bq-levs*
		      (let ((tl (member lev bq-levs*)))
			 (cond (tl
				(remove lev bq-levs*))
			       (t
				(let ((r (read srm)))
				   (signal-problem
				       "Undefined backquote level ,"
					  lev
					  (:q (atsign "@") (t ""))
					  r)))))))
		`(bq-comma ,lev ,atsign ,(read srm)))))))

(set-macro-character #\, #'bq-comma-reader nil ytools-readtable*)

(def-excl-dispatch #\` (srm _)
   (backquote-read srm #'build-bq-exp))

(defun backquote-read (srm builder)
  (let ((lev (or (bq-read-lev srm)
		 (cond ((null bq-levs*) 1)
		       (t false)))))
     (let ((recorded-lev
	      (or lev
		  (repeat :for ((n = 1 :by 1))
		   :until (not (member n bq-levs*))
		   :result n))))
	(let ((a (let ((bq-levs* (cons recorded-lev
				       bq-levs*)))
		    (read srm))))
	   (cond ((and (not lev)
		       (not (= recorded-lev 1)))
		  (!= lev recorded-lev)
		  (signal-problem bq-reader
				  "Unnumbered internal backquote !`" a
		     (:continue "I'll assign it a number "))))
	   (funcall builder lev a)))))

;;; Inside excl-backquote handlers, we usually want to
;;; let the backquoteness extend to some of the arguments
;;; of the entity we're building.  This is a slightly
;;; more optimized way of doing this than just writing
;;; `(bq-backquote ,lev ,e)
(defun build-bq-exp (lev e)
   (match-cond e
      (:? (yt::bq-comma ?,lev ?,false ?e1)
         ;; The backquote would just cancel the
         ;; comma out
         e1)
      ((atom e) `',e)
      (t
       `(bq-backquote ,lev ,e))))

;;;;(set-dispatch-macro-character #\! #\` #'bq-reader ytools-readtable*)

;;; 0 is not allowed as a level.
(defun bq-read-lev (srm)
   (let ((ch (peek-char nil srm)))
      (cond ((and (digit-char-p ch)
		  (not (char= ch #\0)))
	     (read-char srm)
	     (cond ((char= (peek-char nil srm) #\#)
		    (read-char srm)))
	     (- (char-code ch) (char-code #\0)))
	    (t nil))))

(defvar bq-pprint* true)

(set-pprint-dispatch
    '(cons (eql bq-backquote))
    (\\ (srm el)
       (cond ((and bq-pprint*
		   (= (length el) 3)
		   (is-Integer (cadr el)))
	      (out (:to srm) "!`"
		             (cadr el)
			     (:q ((atom (caddr el)) "#"))
			     (caddr el)))
	     (t
	      (pprint-fill srm el true))))
    2)

(set-pprint-dispatch
    '(cons (eql bq-comma))
    (\\ (srm el)
       (cond ((and bq-pprint*
		   (= (length el) 4)
		   (is-Integer (cadr el))
		   (member (caddr el) '(t nil)))
	      (out (:to srm)
		   "," (cadr el)
		   (:q ((caddr el) "@"))
		   (:q ((and (not (caddr el))
			     (not (is-Pair (cadddr el))))
			"#"))
		   (cadddr el)))
	     (t
	      (pprint-fill srm el true))))
    2)
)