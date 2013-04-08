;;; Stuff for Type-optimizations and declarations
;;;
;;; Copyright (C) 1997-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: withtype.lisp,v 1.8 2005/01/27 23:02:46 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/withtype.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mk-arr' [:ext is already required by :base anyway]
  (require :port-ext (translate-logical-pathname "port:ext")))

(in-package :cllib)

(export '(index-t map-vec dfloat with-type +whitespace+ whitespace-char-p))

;;;
;;; White space
;;;

(defconst +whitespace+ (simple-array character (*))
  (mk-arr 'character '(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page))
  "*The whitespace characters.")

(defsubst whitespace-char-p (char)
   (find char +whitespace+ :test #'char=))

;;;
;;; Misc
;;;

(defmacro map-vec (type len &rest args)
  "MAP into a simple-array with elements of TYPE and length LEN."
  `(map-into (make-array ,len :element-type ,type) ,@args))

(defmacro dfloat (num)
  "Coerce to double float."
  `(float ,num 1d0))

(deftype index-t () '(unsigned-byte 28))

(defmacro with-type (type expr)
  "Evaluate the arithmetic expression in TYPE.
Adopted from P.Graham `ANSI CL', p 410; with some modifications."
  `(the ,type
    ,(if (and (consp expr)
              (member (car expr) '(+ - * / abs sin cos tan cot
                                   signum log exp expt)
                      :test #'eq))
         (let ((nexp
                (labels ((binarize (expr)
                           (if (and (nthcdr 3 expr)
                                    (member (car expr) '(+ - * /)))
                               (destructuring-bind (op a1 a2 . rest) expr
                                 (binarize `(,op (,op ,a1 ,a2) ,@rest)))
                               expr)))
                  (binarize expr))))
           `(,(car nexp) ,@(mapcar #'(lambda (ee) `(with-type ,type ,ee))
                                   (cdr nexp))))
         expr)))

(provide :cllib-withtype)
;;; file withtype.lisp ends here
