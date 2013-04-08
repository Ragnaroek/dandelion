;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;;$Id: outin.lisp,v 2.2 2006/01/13 14:35:26 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:compile-toplevel :load-toplevel)
   (export '(out in out-indent stream-indent
	     read-string read-y-or-n lineread)))

(eval-when (:compile-toplevel :load-toplevel :slurp-toplevel)
   (datafun-table out-ops* out-operator)

   (defun get-out-operator (exp)
      (and (is-Pair exp)
	   (is-Symbol (car exp))
	   (table-entry out-ops* (car exp)))))

;;;;(defvar out-indent* 0)

(defvar out-vals* '())

;;;;(declaim (special cleanup-forms*))

(defstruct Out-stream 
   state ; one of indented, unindented
   indent ; number to indent
   stream)

;;; Prepare an out-stream for imminent output of something.
(defun out-prepare (stream)
   (let ((curstate (Out-stream-state stream))
	 (real-stream (Out-stream-stream stream)))
;;;;      (setq real-stream* real-stream)
      (cond ((eq curstate 'unindented)
#+allegro    (format real-stream "~V,0T" (Out-stream-indent stream))
#-allegro    (progn #-openmcl (fresh-line real-stream)
		    (print-spaces (Out-stream-indent stream)
				  real-stream))
	     (setf (Out-stream-state stream) 'indented)))
      real-stream))

;; An alist giving mapping of regular streams to their corresponding 
;; Out-streams
(defvar out-streams* '())

(defvar out-stream-cleanup* #+openmcl false #-openmcl true)

(defun stream-outify (srm)
   (cond ((Out-stream-p srm)
	  srm)
	 ((is-Stream srm)
	  (cond ((some #'(lambda (p)
			    (or (not (streamp (car p)))
				(not (stream-is-open (car p) false))))
		       out-streams*)
		 ;; clean up table
		 (setq out-streams*
		       (delete-if #'(lambda (p)
				       (or (not (streamp (car p)))
					   (not (stream-is-open (car p)
								false))))
				  out-streams*))))
	  (let ((p (assq srm out-streams*)))
	     (cond (p (cadr p))
		   (t
		    (let ((newsrm (make-Out-stream :state 'unindented
						   :indent 0
						   :stream srm)))
		       (stream-is-open srm false)
		       (setq out-streams*
			     (cons (list srm newsrm) out-streams*))
		       newsrm)))))
	 (t
	  (error "Attempt to outify nonstream ~s" srm))))

(defun stream-is-open (srm warn)
   (declare (ignorable warn))
   #+openmcl
   (handler-case (open-stream-p srm)
      (error ()
	 (cond (warn
		(format *error-output*
		    "Warning -- unintelligible stream ~s~%"
		    srm)))
	 true))
   #-openmcl
   (open-stream-p srm))
	  
(eval-when (:compile-toplevel :load-toplevel)
   (defvar default-out-stream-var* '*standard-output*))

(defmacro out-indent (srm^ ind^ &rest body)
   (let ((indent-var (gensym))
	 (srmvar (gensym)))
      `(let ((,srmvar (stream-outify 
			        ,(cond ((eq srm^ 't) default-out-stream-var*)
				       (t srm^)))))
	  (let ((,indent-var (Out-stream-indent ,srmvar)))
	     (unwind-protect
		(progn
		   ,@(include-if (not (eql ind^ '0))
			`(setf (Out-stream-indent ,srmvar)
			       (+ ,indent-var ,ind^)))
		   ,@body)
	       (setf (Out-stream-indent ,srmvar)
		     ,indent-var))))))

(defun stream-indent (srm amt)
   (setq srm (stream-outify (cond ((eq srm t)
				   (eval default-out-stream-var*))
				  (t srm))))
   (setf (Out-stream-indent srm)
	 (+ (Out-stream-indent srm) amt)))

(defmacro out (&rest exps)
  (let ((stream (gensym))
	;;;;(cleanup-forms* '())
	(stream-form default-out-stream-var*)
	(string-output false))
     (cond ((and (not (null exps))
		 (is-Pair (car exps))
		 (memq (car (car exps)) '(:to to)))
	    ;; (:to s) almost always occurs as first thing, so we
	    ;; catch this here
	    (cond ((not (eq (cadar exps) 't))
		   (setq stream-form (cadar exps))))
	    (setq exps (cdr exps))))
     (cond ((memq stream-form '(:string nil))
	    (setq string-output true)
	    (setq stream-form (gensym))))
     (let ((code `(let ((,stream (stream-outify ,stream-form)))
		     (out-indent ,stream 0
			(let ((out-vals* '()))
			   (cl:declare (special out-vals*))
			   ,@(expand-out-body exps stream)
			   ,@(include-if (not string-output)
					'(list->values out-vals*)))))))
        (cond (string-output
	       `(with-output-to-string (,stream-form)
		   ,code))
	      (t code)))))

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)

;; The stream variable is bound to a variable that will (at run time)
;; evaluate to an Out-stream, not a real stream.

   (defun expand-out-body (exps stream)
      (mapcar #'(lambda (e) (out-expand e stream))
	      exps))

   (defun out-expand (exp stream)
      (cond ((stringp exp)
	     `(princ ,exp (out-prepare ,stream)))
	    ((numberp exp)
	     (if (> exp 0)
		 `(print-spaces ,exp (out-prepare ,stream))
		 `(out-srmlines ,(- exp) ,stream)))
	    ((memq exp '(t :%))
	     `(out-srmlines 1 ,stream))
	    (t
	     (let ((out-op (get-out-operator exp)))
	        (cond (out-op
		       (funcall out-op exp stream))
		      (t
		       `(prin1 ,exp (out-prepare ,stream))))))))
)

(defun out-srmlines (num srm)
  (let ((real-stream (Out-stream-stream srm)))
     (if (< num 1)
	 (fresh-line real-stream)
	 (dotimes (n num) (terpri real-stream)))
     (setf (Out-stream-state srm) 'unindented)))

;;; (define-out-operator (<op> cmdvar stream-var) ---body---)
;;; Causes 'body' to be executed whenever a command beginning
;;; with <op> occurs.  'cmdvar' will be bound to command, and
;;; 'stream-var' will be bound to the variable where the current 'out' stream
;;; lives.
(defmacro define-out-operator (form &rest body)
   `(eval-when (:compile-toplevel :load-toplevel :slurp-toplevel)
       (datafun out-operator ,(car form)
	  (defun :^ ,(cdr form) ,@body))))
	     
;;;;   `(setf (get ',(car form) 'out-operator)
;;;;          (\\  ,(cdr form) ,@body)))

;;; (:e (:stream x) ...) gives access to current out-stream as value of
;;; 'x' in the body.
(define-out-operator (:e cmd stream)
   (multiple-value-bind (streamvar cmds)
			(cond ((and (not (null (cdr cmd)))
				    (is-Pair (cadr cmd))
				    (eq (car (cadr cmd)) ':stream))
			       (values (cadr (cadr cmd))
				       (cddr cmd)))
			      (t
			       (values false (cdr cmd))))
      (labels ((build-it (streamexp)
		  (out-e-o
		     (cond ((= (len cmds) 1)
			    (car cmds))
			   (t
			    `(progn ,@cmds)))
		     streamexp)))
	 (cond (streamvar
		`(let ((,streamvar ,stream))
		    ,(build-it streamvar)))
	       (t
		(build-it stream))))))

(defun out-e-o (exp stream)
   (out-e-o-map
      (\\ (o-stuff)
	 `(progn ,@(expand-out-body o-stuff stream)))
      exp))

;; Replace every occurrence of (:o -s-) with result of applying fcn to
;; s
(defun out-e-o-map (fcn exp)
   (cond ((atom exp) exp)
	 ((memq (car exp) '(:o o))
	  (cond ((eq (car exp) 'o)
		 (format *error-output*
		      "Warning -- obsolete :e-:o form ~s~%" exp)))
	  (funcall fcn (cdr exp)))
	 (t
	  (do ((el exp (cdr el))
	       (res '() `(,(out-e-o-map fcn (car el))
			  ,@res)))
	      ((atom el)
	       `(,@(nreverse res) ,@el))))))

(define-out-operator (:t cmd stream)
   `(format (out-prepare ,stream) "~VT" ,(cadr cmd)))

(define-out-operator (:q cmd stream)
   `(cond ,@(mapcar (\\ (clause)
		       `(,(car clause)
			 ,@(expand-out-body (cdr clause) stream)
			 ;; This might work, but stream is not a real
			 ;; stream!
			 ;(out (to ,stream) ,@(cdr clause))
			 ))
		    (cdr cmd))))

(define-out-operator (:to cmd stream)
   `(setf ,stream (stream-outify
		     ,(let ((srm (cadr cmd)))
			 (cond ((eq srm 't) *standard-output*)
			       (t srm))))))

; (:a -exps-) evaluate exps and princ's the results.
(define-out-operator (:a cmd stream)
   `(progn ,@(mapcar (\\ (e) `(princ ,e (out-prepare ,stream)))
		     (cdr cmd))))

;;; (:_ -exps-) evaluate exps, which should evaluate to numbers.
;;; These are then treated as if they occurred as constants in the original
;;; call to 'out'.  Anything other than a number is princ'ed.
(define-out-operator (:_ cmd stream)
   `(progn ,@(mapcar (\\ (e) `(out-space-or-display ,e ,stream))
		     (cdr cmd))))

(defun out-space-or-display (x stream)
   (cond ((is-Number x)
	  (cond ((> x 0) (print-spaces x (out-prepare stream)))
		(t
		 (print-lines (- x) (Out-stream-stream stream))
		 (setf (Out-stream-state stream) 'unindented))))
	 (t (princ x (out-prepare stream)))))

(defun print-lines (number stream)
  (if (< number 1)
      (fresh-line stream)
      (dotimes (n number) (terpri stream))))

(define-out-operator (:i> cmd srm)
   `(setf (Out-stream-indent ,srm)
          (+ (Out-stream-indent ,srm) ,(cadr cmd))))
;;;;   (ignore srm)
;;;;   `(setq out-indent* (+ out-indent* ,(cadr cmd))))

(define-out-operator (:i< cmd srm)
   `(setf (Out-stream-indent ,srm)
          (- (Out-stream-indent ,srm) ,(cadr cmd))))

(define-out-operator (:i= cmd srm)
   `(setf (Out-stream-indent ,srm)
          ,(cadr cmd)))

;;;;(defmacro out-indent (space &rest body)
;;;;   `(let ((out-indent* (+ out-indent* ,space)))
;;;;       (declare (special out-indent*))
;;;;       ,@body))


;;;;   (ignore srm)
;;;;   `(setq out-indent* (- out-indent* ,(cadr cmd))))

;;;;(define-out-operator (v cmd srm)
;;;;   (ignore srm)
;;;;   `(setq out-vals*
;;;;          (multiple-value-list (progn ,@(cdr cmd)))))

; Embed FORMAT in OUT.  E.g., (out "x,y = " (:f "~5,1f,~5,1f" x y) :%)
(define-out-operator (:f cmd stream)
   `(format (out-prepare ,stream) ,@(cdr cmd))   )

;;; Format (:pp-block [(:pre <prefix>)] -out-cmds- [(:suf <suffix>)])
(define-out-operator (:pp-block cmd stream)
   (setq cmd (cdr cmd))
   (multiple-value-bind (prefix cmd suffix)
                        (pp-block-analyze cmd)
	 (let (
	       #+openmcl (save-srm-var (gensym))
;;;;	       (pp-srmvar (gensym))
;;;;	       (outified-pp-srmvar (gensym))
;;;;	       (srmvar (gensym))
;;;;	       (realsrmvar default-out-stream-var*)  ;;;;(gensym)
              )

;;;;	        `(let (
;;;;		   (,srmvar ,stream))

	    `(let ((,stream ;;;; ,pp-srmvar
		    (out-prepare ,stream))
		    #+openmcl (,save-srm-var nil)
		    )
		(unwind-protect
		   (pprint-logical-block
				  (,stream nil 
				   ,@(cond (prefix `(:prefix ,prefix))
					   (t '()))
				   ,@(cond (suffix `(:suffix ,suffix))
				     (t '())))
		         #+openmcl (setq ,save-srm-var ,stream)
			 (let ((,stream  
				  (stream-outify ,stream)))
			    ,@(expand-out-body cmd stream)))
		  #+openmcl (cond (,save-srm-var
				   (openmcl-out-stream-cleanup ,save-srm-var)))
               )))))

(defun openmcl-out-stream-cleanup (pp-srm)
   (cond ((assoc pp-srm out-streams*)
	  (setq out-streams*
		(delete-if (\\ (e) (eq (first e) pp-srm))
			   out-streams*)))))

(defun pp-block-analyze (pp-block-body)
   (let ((pre (car pp-block-body)) (prefix false))
      (cond ((car-eq pre ':pre)
	     (setq prefix (cadr pre))
	     (setq pp-block-body (cdr pp-block-body))))
      (let ((post (and (not (null pp-block-body))
		       (lastelt pp-block-body)))
	    (suffix false))
	 (cond ((car-eq post ':suf)
		(setq suffix (cadr post))
		(setq pp-block-body (butlast pp-block-body))))
	 (values prefix pp-block-body suffix))))

(define-out-operator (:pp-nl cmd stream)
   `(pprint-newline ,(cadr cmd) (Out-stream-stream ,stream)))

(define-out-operator (:pp-ind cmd stream)
   `(pprint-indent ,@(cdr cmd) (Out-stream-stream ,stream)))

;;;;(define-out-operator (:pp cmd stream)
;;;;   `(let ((*print-pretty* true))
;;;;       (print ,(cadr cmd) (Out-stream-stream ,stream))))

(define-out-operator (:pp cmd stream)
   `(pprint ,(cadr cmd) (out-prepare ,stream)))

(define-out-operator (:v cmd _)
   `(setq out-vals* (values->list ,(cadr cmd))))

(defmacro in (&rest exps)
   (let ((stream-var (gensym)))
      (multiple-value-bind (stream-exp exps)
	                   (cond ((and (not (null exps))
				       (car-eq (car exps) ':from))
				  (values (cadr (car exps)) (cdr exps)))
				 (t
				  (values '*standard-input* exps)))
	 (let ((vals (mapcar (\\ (e) (cond ((is-Symbol e) (gensym))
					   (t nil)   ))
			     exps)))
	    (let ((vars (mapcan (\\ (v) (cond (v (list v)) (t nil)   ))
				vals)))
               `(let ((,stream-var ,stream-exp) ,@vars)
		   ,@(mapcar
			(\\ (exp v)
			   (cond ((is-Symbol exp)
				  `(setf ,v
					 ,(case exp
					     ((:obj :read :object t)
					      `(read ,stream-var false eof*))
					     (:char `(read-char ,stream-var false eof*))
					     (:peek
					      `(peek-char false ,stream-var false eof*))
					     (:linestring
					      `(read-line ,stream-var false eof*))
					     (:linelist `(lineread ,stream-var))
					     (:string
					      `(read-string ,stream-var))
					     (:keyword
					      `(intern (read-string ,stream-var)
						       keyword-package*))
					     (t
					      (cerror "I'll read as ':obj'"
						      "Meaningless 'in' construct ~s~%"
						      exp)
					      `(srmread ,stream-var)))))
				 (t
				  (cerror "I'll read as ':obj'"
					  "Meaningless 'in' construct ~s~%"
					  exp)
				  `(srmread ,stream-var))))
			exps vals)
		   (values ,@vars)))))))

;;; Return a list of all expressions on the current line.
;;; If an expression begins on one line and ends on another,
;;; 'line-read' will blithely assume it's still on the same line.
(defun line-read (srm)
   (let ((res !()) c)
      (loop
	 ;; Advance to first non-space and set c to it.
	 (loop
	    (setq c (peek-char false srm false eof*))
	    (cond ((and (is-Char c)
			(or (char= c #\Space)
			    (char= c #\Tab)))
		   (read-char srm))
		  (t (return))))
	 (cond ((or (eq c eof*) (char= c #\Newline))
		(cond ((char= c #\Newline)
		       (read-char srm)))
		(return (nreverse res)))
	       (t
		(setq res (cons (read-preserving-whitespace srm)
				res)))))))

(defun read-string (srm)
   (let ((ch (peek-char true srm false eof*)))
      ;; Peeking at first non-whitespace char
      (cond ((eq ch eof*) eof*)
	    (t
	     (with-output-to-string (ss)
	        (loop
		   (write-char ch ss)
		   (read-char srm)
		   (setq ch (peek-char false srm false eof*))
		   (cond ((or (eq ch eof*) (is-whitespace ch))
			  (return)))))))))

;;;;   (let ((obj (read srm)))
;;;;     (format nil "~a" obj)))

(defmacro read-y-or-n (&rest out-stuff)
   (multiple-value-bind (y-or-n-fn out-stuff)
                        (cond ((memq ':yes-no out-stuff)
                               (values 'yes-or-no-p
                                       (remove ':yes-no out-stuff)))
                              (t
                               (values 'y-or-n-p
                                       out-stuff)))

      `(,y-or-n-fn "~s" (make-Printable (\\ (srm) (out (:to srm) ,@out-stuff))))))

(defun lineread (&optional (s *standard-input*))
	  (prog ((res nil) c)
	   next
	     (prog ()
	      gobble-space
		(setq c (peek-char false s false eof*))
		(cond ((and (is-Char c)
			    (char= c #\Space))
		       (read-char s)
		       (go gobble-space)))
		(return))
	     (cond ((or (eq c eof*) (char= c #\Newline))
		    (read-char s)
		    (return (nreverse res)))
		   (t
		    (setq res (cons (read-preserving-whitespace s) res))
		    (go next)))))