;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: misc.lisp,v 2.1 2005/12/26 00:25:17 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on :at-run-time %ytools/ setter mapper signal)

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(out-to-string dbg-out dbg-out-indent
	     err-out cons-if-new plev plen
	     classify shorter list-splice is-list-of boole-eq eqn
	     mod-load val-or-initialize memoize-val)))

(defmacro out-to-string (&rest outargs)
   `(with-output-to-string (string-stream) (out (:to string-stream) ,@outargs)))

(defmacro dbg-out (gate^ &rest msgstuff)
   `(cond (,gate^
	   (err-out ,@msgstuff))))

(defmacro dbg-out-indent (gate^ space^ &body body)
   (let ((bod-fun (gensym)))
      `(let-fun ((,bod-fun () ,@body))
	  (cond (,gate^
		 (out-indent *error-output* ,space^
		    (,bod-fun)))
		(t
		 (,bod-fun))))))

(defmacro err-out (&rest msgstuff) 
	   `(progn (out (:to *error-output*)
			 ,@msgstuff
			 ,@(include-if (not (eq (lastelt msgstuff) ':%))
			   ':%))
		   (force-output *error-output*)))

(defun cons-if-new (x y l)
   (cond ((and (eq x (car l)) (eq y (cdr l)))
          l)
         (t (cons x y))))

(defun plev (n)
  #+allegro (progn (!= tpl:*print-level* n))
  (!=/ *print-level* n))

(defun plen (n)
  #+allegro (progn (!= tpl:*print-length* n))
  (!=/ *print-length* n))

(defun classify (l pred)
   (repeat :for ((x :in l)
		 :collectors yes no)
      (cond ((funcall pred x)
	     (one-collect yes x))
	    (t
	     (one-collect no x)))
    :result (values yes no)))


;;;;   (cond ((null l)
;;;;	  (values '() '()))
;;;;	 (t
;;;;	  (multiple-value-let (yes no)
;;;;			      (classify (cdr l) pred)
;;;;	     (cond ((funcall pred (car l))
;;;;		    (values (cons (car l) yes)
;;;;			    no))
;;;;		   (t
;;;;		    (values yes
;;;;			    (cons (car l) no))))))))

(defun shorter (l n)
   (declare (type fixnum n))
   (cond ((null l)
	  (cond ((> n 0) 0)
		(t false)))
	 ((= n 0) false)
	 (t
	  (let ((k (shorter (cdr l) (- n 1))))
	     (cond (k (+ k 1))
		   (t false))))))

;(LIST-SPLICE a b c) splices out cell following cell b
;in list a, replacing with c.  If b = NIL, splicing is at front.
(defmacro list-splice (a b c)
   `(cond (,b (setf (cdr ,b) ,c))
          (t (setf ,a ,c))   ))

(defun is-list-of (x predfn)
   (cond ((null x) t)
         ((is-Pair x)
          (and (funcall predfn (car x))
               (is-list-of (cdr x) predfn)))
         (t nil)   ))

(defun boole-eq (x y) (eq (not x) (not y)))

(defun eqn (sym1 sym2)
   (string= (symbol-name sym1)
	    (symbol-name sym2)))

;;; Convenience

(defun mod-load (module-name)
   (let ((str
	    (cond ((is-Symbol module-name)
		   (Symbol-name module-name))
		  (t
		   module-name))))
      (do-fload `(,(concatenate 'string
		       "../" str "/")
		  ,(concatenate 'string
		       str ".lsy")))
      (do-fload `(%module/ ,(intern str (find-package str))))))
	     
(defmacro val-or-initialize (e^ &key ((:missing-if missing^) 'false)
				     ((:init init^)
				      (signal-problem val-or-initialize
				         "'val-or-initialize' must have"
				         " :init argument")))
   (multiple-value-bind (itemp-vars ivals istore-vars iset iacc)
                        (get-setf-expansion e^)
      (let ((curval-var (car istore-vars)))   ;(gensym)
	 `(let* ,(<# tuple itemp-vars ivals)
	     (let ((,curval-var ,iacc))
		(cond ((eq ,curval-var ,missing^)
		       (!= ,curval-var ,init^)
		       ,iset))
		,curval-var)))))

;; Notational variant
(defmacro memoize-val (exp^ &key ((:missing-if uncached-val^) 'false)
				 ((:store-as store-place^)
				  (signal-problem memoize-val
				         "'val-or-initialize' must have"
				         " :store-as argument")))
   `(val-or-initialize ,store-place^ :init ,exp^ :missing-if ,uncached-val^))

