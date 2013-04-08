;;; Typed variable syntax for Common Lisp
;;; Bruno Haible 2004-08-05
;;; This file is put in the public domain by its authors.

;;; A typed variable is a variable whose value at any time is guaranteed
;;; by the programmer to belong to a given type. It is assumed that the
;;; type's semantics doesn't change during the dynamic extent of the variable.
;;;
;;; Common Lisp supports typed variables in various situations, but the
;;; syntax to declare a typed variable is different each time:
;;; - In LAMBDA, LET, LET*, MULTIPLE-VALUE-BIND:
;;;     (lambda (x ...) (declare (integer x)) ...)
;;;     (lambda (x ...) (declare (type integer x)) ...)
;;;     (let ((x ...)) (declare (integer x)) ...)
;;;     (let ((x ...)) (declare (type integer x)) ...)
;;; - In DEFMETHOD, DEFGENERIC:
;;;     (defmethod foo ((x integer) ...) ...)
;;; - In LOOP:
;;;     (loop for x integer across v ...)
;;;     (loop for x of-type integer across v ...)
;;; - Type declarations in function returns:
;;;     (declaim (ftype (function (t) integer) foo))
;;;     (defun foo (x) ...)
;;;
;;; This file supports typed variables and typed function returns through
;;; a simple common syntax: [variable type] and [type].
;;; Examples:
;;; - In LAMBDA, LET, LET*, MULTIPLE-VALUE-BIND:
;;;     (lambda ([x integer] ...) ...)
;;;     (let (([x integer] ...) ...)
;;; - In DEFMETHOD, DEFGENERIC:
;;;     (defmethod foo ([x integer] ...) ...)
;;; - In LOOP:
;;;     (loop for [x integer] across v ...)
;;; - Type declarations in function returns:
;;;     (defun foo (x) [integer] ...)
;;;
;;; The variable name always comes before the type, because (assuming decent
;;; coding style) it carries more information than the type.

;;; Example:
;;;    (defun scale-long-float (x exp)
;;;      (declare (long-float x) (fixnum exp))
;;;      ...)
;;; -> (defun scale-long-float ([x long-float] [exp fixnum])
;;;      ...)
;;;
;;; Note: Specialized lambda lists in DEFMETHOD, DEFGENERIC can contain an
;;; an evaluated form, whereas type specifiers cannot. Therefore
;;;   (defmethod foo ([x (eql a)]) ...)
;;; is equivalent to
;;;   (defmethod foo ((x (eql 'a))) ...),
;;; and there is no typed-variable syntax for
;;;   (defmethod foo ((x (eql a))) ...).
;;;
;;; Note: Another difference with specialized lambda lists in DEFMETHOD is
;;; that the typed variable syntax not only defines a specializer, but also
;;; a declaration for the entire scope of variable. I.e.
;;;   (defmethod foo ([x integer]) ...)
;;; is equivalent to
;;;   (defmethod foo ((x integer)) (declare (type integer x)) ...)
;;; It would be bad style anyway to assign a non-integer value to x inside
;;; the method.
;;;
;;; Note: When a return type declaration and documentation string are both
;;; specified together, the type declaration should come first:
;;;   (defun foo (x) [integer] "comment" ...)

;; ============================ Package Setup ============================

(defpackage "TYPEDVAR-COMMON-LISP"
  (:use "COMMON-LISP")
  (:export . #1=("DEFCONSTANT"
                 "DEFGENERIC"
                 "DEFMETHOD"
                 "DEFPARAMETER"
                 "DEFUN"
                 "DEFVAR"
                 ;"DESTRUCTURING-BIND"
                 "DO"
                 "DO*"
                 "DO-ALL-SYMBOLS"
                 "DO-EXTERNAL-SYMBOLS"
                 "DO-SYMBOLS"
                 #+CLISP "DOHASH"
                 "DOLIST"
                 #+CLISP "DOSEQ"
                 "DOTIMES"
                 "FLET"
                 "FUNCTION"
                 ;"HANDLER-CASE"
                 "LABELS"
                 "LAMBDA"
                 "LET"
                 "LET*"
                 ;"LOOP"
                 "MULTIPLE-VALUE-BIND"
                 "PROG"
                 "PROG*"
                 ;"RESTART-CASE"
                 "WITH-ACCESSORS"
                 "WITH-INPUT-FROM-STRING"
                 "WITH-OPEN-FILE"
                 "WITH-OPEN-STREAM"
                 "WITH-OUTPUT-TO-STRING"
                 "WITH-SLOTS"))
  (:shadow . #1#))

(in-package "TYPEDVAR-COMMON-LISP")

(cl:do-external-symbols (s "COMMON-LISP")
  (unless (member (nth-value 1 (find-symbol (symbol-name s))) '(:internal :external))
    (import (list s))
    (export (list s))))

;; ============================ Basic definitions ============================

;; In-memory representation of syntax [type]: `(type-decl ,type).
(defmacro type-decl (type)
  (error "misplaced type declaration: [~S]" type))
(cl:defun type-decl-p (form)
  (and (consp form) (eq (car form) 'type-decl)
       (consp (cdr form))
       (null (cddr form))))
(cl:defun type-decl-type (x) (second x))

;; In-memory representation of syntax [var type]: `(typed-var ,var ,type).
(defmacro typed-var (var type)
  (error "misplaced typed variable declaration: [~S ~S]" var type))
(cl:defun typed-var-p (form)
  (and (consp form) (eq (car form) 'typed-var)
       (consp (cdr form))
       (consp (cddr form))
       (null (cdddr form))))
(cl:defun typed-var-variable (x) (second x))
(cl:defun typed-var-type (x) (third x))

;; ============================ Readtable Setup ============================

(set-macro-character #\[
  #'(cl:lambda (stream char)
      (declare (ignore char))
      (cl:let ((contents (read-delimited-list #\] stream t)))
        (when (null contents)
          (error "invalid syntax: []"))
        (case (length contents)
          (1 `(type-decl ,(first contents)))
          (2 (unless (symbolp (first contents))
               (error "invalid syntax, variable should be a symbol: [~S ~S]"
                      (first contents) (second contents)))
             `(typed-var ,(first contents) ,(second contents)))))))
(set-syntax-from-char #\] #\))

;; =================== Subroutines for the extended macros ===================

(cl:defun variable-to-keyword (variable)
  (if (symbolp variable)
    (intern (symbol-name variable) (find-package "KEYWORD"))
    nil))

(cl:defun analyze-typed-lambdalist (lambdalist)
  (cl:let ((pure-lambdalist '())
           (typelist '())
           (declspecs '())
           (seen-&key nil)
           (seen-&aux nil))
    (cl:dolist (item lambdalist)
      (if (typed-var-p item)
        (progn
          (push (typed-var-variable item) pure-lambdalist)
          (push `(type ,(typed-var-type item) ,(typed-var-variable item)) declspecs)
          (unless seen-&aux
            (push (if seen-&key
                    (list (variable-to-keyword (typed-var-variable item)) (typed-var-type item))
                    (typed-var-type item))
                  typelist)))
        (if (atom item)
          (progn
            (push item pure-lambdalist)
            (if (member item lambda-list-keywords)
              (progn
                (when (eq item '&key)
                  (setq seen-&key t))
                (when (eq item '&aux)
                  (setq seen-&aux t))
                (when (member item '(&optional &rest &key))
                  (push item typelist)))
              (unless seen-&aux
                (push (if seen-&key
                        (list (variable-to-keyword item) 't)
                        't)
                      typelist))))
          (cl:multiple-value-bind (var1 type1 keyword)
              (if (typed-var-p (first item))
                (progn
                  (push `(type ,(typed-var-type (first item)) ,(typed-var-variable (first item))) declspecs)
                  (values (typed-var-variable (first item))
                          (typed-var-type (first item))
                          (if (and seen-&key (not seen-&aux))
                            (variable-to-keyword (typed-var-variable (first item)))
                            nil)))
                (if (atom (first item))
                  (values (first item)
                          't
                          (if (and seen-&key (not seen-&aux))
                            (variable-to-keyword (first item))
                            nil))
                  (if (and (consp (cdr (first item)))
                           (null (cddr (first item))))
                    (if (typed-var-p (second (first item)))
                      (progn
                        (push `(type ,(typed-var-type (second (first item))) ,(typed-var-variable (second (first item)))) declspecs)
                        (values (list (first (first item))
                                      (typed-var-variable (second (first item))))
                                (typed-var-type (second (first item)))
                                (first (first item))))
                      (values (first item)
                              't
                              (first (first item))))
                    (values (first item)
                            't
                            nil))))
            (unless seen-&aux
              (push (if seen-&key (list keyword type1) type1) typelist))
            (push (cons var1
                        (if (and (consp (cdr item)) (consp (cddr item)))
                           (cl:let ((var3
                                      (if (typed-var-p (third item))
                                        (progn
                                          (push `(type ,(typed-var-type (third item)) ,(typed-var-variable (third item))) declspecs)
                                          (typed-var-variable (third item)))
                                        (third item))))
                             (list* (second item) var3 (cdddr item)))
                           (cdr item)))
                  pure-lambdalist)))))
    (values (nreverse pure-lambdalist)
            (nreverse typelist)
            (nreverse declspecs))))

(cl:defun analyze-typed-lambdabody (lambdalist body)
  (cl:multiple-value-bind (pure-lambdalist typelist declspecs)
      (analyze-typed-lambdalist lambdalist)
    (if (or declspecs (and (consp body) (type-decl-p (car body))))
      (cl:multiple-value-bind (resulttype rbody)
          (if (and (consp body) (type-decl-p (car body)))
            (values (type-decl-type (car body)) (cdr body))
            (values 't body))
        (values `(,pure-lambdalist
                    ,@(if declspecs `((declare ,@declspecs)))
                    ,@rbody)
                `(cl:function ,typelist ,resulttype)))
      (values `(,lambdalist ,@body) nil))))

(cl:defun untyped-flet-bindings (bindings)
  (cl:let ((declspecs '())
           (pure-bindings '()))
    (cl:dolist (binding bindings)
      (cl:let ((funname (car binding)))
        (cl:multiple-value-bind (pure-lambdabody ftype)
            (analyze-typed-lambdabody (cadr binding) (cddr binding))
          (when ftype (push `(ftype ,ftype ,funname) declspecs))
          (push (cons funname pure-lambdabody) pure-bindings))))
    (values (nreverse pure-bindings) (nreverse declspecs))))

(cl:defun untyped-variable-bindings (bindings)
  (cl:let ((declspecs '())
           (pure-bindings '()))
    (cl:dolist (binding bindings)
      (if (atom binding)
        (if (typed-var-p binding)
          (progn
            (push (typed-var-variable binding) pure-bindings)
            (push `(type ,(typed-var-type binding) ,(typed-var-variable binding)) declspecs))
          (push binding pure-bindings))
        (cl:let ((var (first binding)))
          (if (typed-var-p var)
            (progn
              (push (cons (typed-var-variable var) (rest bindings)) pure-bindings)
              (push `(type ,(typed-var-type var) ,(typed-var-variable var)) declspecs))
            (push binding pure-bindings)))))
    (values (nreverse pure-bindings) (nreverse declspecs))))

(cl:defun untyped-method-body (method-body)
  (cl:let* ((lambdalist-position (position-if #'listp method-body))
            (qualifiers (subseq method-body 0 lambdalist-position))
            (lambdalist (nth lambdalist-position method-body))
            (body (nthcdr (+ lambdalist-position 1) method-body))
            (req-num (or (position-if #'(cl:lambda (x) (member x lambda-list-keywords)) lambdalist)
                         (length lambdalist))))
    (cl:multiple-value-bind (pure-lambdalist typelist declspecs)
        (analyze-typed-lambdalist lambdalist)
      (declare (ignore typelist))
      `(,@qualifiers
        (,@(mapcar #'(cl:lambda (specialized-argument)
                       (if (typed-var-p specialized-argument)
                         (list (typed-var-variable specialized-argument)
                               (cl:let ((type (typed-var-type specialized-argument)))
                                 (if (and (consp type) (eq (car type) 'eql)
                                          (consp (cdr type)) (null (cddr type))
                                          (cl:let ((singleton (second type)))
                                            ;; not self-evaluating?
                                            (or (symbolp singleton) (consp singleton))))
                                   `(eql ',(second type))
                                   type)))
                         specialized-argument))
                   (subseq lambdalist 0 req-num))
         ,@(nthcdr req-num pure-lambdalist))
        ,@(if declspecs `((declare ,@declspecs)))
        ,@body))))

;; ============ Extended Common Lisp special operators and macros ============

;; TODO: Review the declaration scope in macros that establish several
;;       bindings, like LET, LET*, LAMBDA, DO, DO*.

(defmacro defconstant (&whole whole
                       name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  (if (typed-var-p name)
    `(progn
       (declaim (type ,(typed-var-type name) ,(typed-var-variable name)))
       (cl:defconstant ,(typed-var-variable name) ,@(cddr whole)))
    `(cl:defconstant ,@(cdr whole))))

(defmacro defgeneric (funname gf-lambda-list &rest options)
  `(cl:defgeneric ,funname ,gf-lambda-list
     ,@(mapcar #'(cl:lambda (option)
                   (if (and (consp option) (eq (car option) ':method))
                     `(:method ,@(untyped-method-body (cdr option)))
                     option))
               options)))

(defmacro defmethod (funname &rest method-body)
  `(cl:defmethod ,funname ,@(untyped-method-body method-body)))

(defmacro defparameter (&whole whole
                        name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  (if (typed-var-p name)
    `(progn
       (declaim (type ,(typed-var-type name) ,(typed-var-variable name)))
       (cl:defparameter ,(typed-var-variable name) ,@(cddr whole)))
    `(cl:defparameter ,@(cdr whole))))

(defmacro defun (&whole whole
                 funname lambdalist &body body)
  (cl:multiple-value-bind (pure-lambdabody ftype)
      (analyze-typed-lambdabody lambdalist body)
    (if ftype
      `(progn
         (declaim (ftype ,ftype ,funname))
         (cl:defun ,funname ,@pure-lambdabody))
      `(cl:defun ,@(cdr whole)))))

(defmacro defvar (&whole whole
                  name &optional initial-value documentation)
  (declare (ignore initial-value documentation))
  (if (typed-var-p name)
    `(progn
       (declaim (type ,(typed-var-type name) ,(typed-var-variable name)))
       (cl:defvar ,(typed-var-variable name) ,@(cddr whole)))
    `(cl:defvar ,@(cdr whole))))

#| TODO: destructuring-bind |#

(defmacro do (&whole whole
              bindings endtest &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-variable-bindings bindings)
    (if declspecs
      `(cl:do ,pure-bindings ,endtest (declare ,@declspecs) ,@body)
      `(cl:do ,@(cdr whole)))))

(defmacro do* (&whole whole
               bindings endtest &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-variable-bindings bindings)
    (if declspecs
      `(cl:do* ,pure-bindings ,endtest (declare ,@declspecs) ,@body)
      `(cl:do* ,@(cdr whole)))))

(defmacro do-all-symbols (&whole whole
                          (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:do-all-symbols (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:do-all-symbols ,@(cdr whole))))

(defmacro do-external-symbols (&whole whole
                               (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:do-external-symbols (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:do-external-symbols ,@(cdr whole))))

(defmacro do-symbols (&whole whole
                      (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:do-symbols (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:do-symbols ,@(cdr whole))))

#+CLISP
(defmacro dohash (&whole whole
                  (var1 var2 &rest options) &body body)
  (if (or (typed-var-p var1) (typed-var-p var2))
    `(ext:dohash (,(if (typed-var-p var1) (typed-var-variable var1) var1)
                  ,(if (typed-var-p var2) (typed-var-variable var2) var2)
                  ,@options)
       (declare
         ,@(if (typed-var-p var1)
             `((type ,(typed-var-type var1) ,(typed-var-variable var1))))
         ,@(if (typed-var-p var2)
             `((type ,(typed-var-type var2) ,(typed-var-variable var2)))))
       ,@body)
    `(ext:dohash ,@(cdr whole))))

(defmacro dolist (&whole whole
                  (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:dolist (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:dolist ,@(cdr whole))))

#+CLISP
(defmacro doseq (&whole whole
                 (var &rest options) &body body)
  (if (typed-var-p var)
    `(ext:doseq (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(ext:doseq ,@(cdr whole))))

(defmacro dotimes (&whole whole
                   (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:dotimes (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:dotimes ,@(cdr whole))))

(defmacro flet (&whole whole
                bindings &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-flet-bindings bindings)
    (if declspecs
      `(cl:flet ,pure-bindings (declare ,@declspecs) ,@body)
      `(cl:flet ,@(cdr whole)))))

#-CLISP
(defmacro function (arg)
  (if (and (consp arg) (member (first arg) '(cl:lambda lambda)))
    (cl:multiple-value-bind (pure-lambdabody ftype)
        (analyze-typed-lambdabody (cadr arg) (cddr arg))
      (if ftype
        `(the ,ftype (cl:function (cl:lambda ,@pure-lambdabody)))
        `(cl:function (cl:lambda ,@(cdr arg)))))
    `(cl:function ,arg)))
#+CLISP
(defmacro function (&whole whole arg1 &optional (arg2 nil arg2-p))
  (cl:let ((named (if arg2-p (list arg1) '()))
           (arg (if arg2-p arg2 arg1)))
    (if (and (consp arg) (member (first arg) '(cl:lambda lambda)))
      (cl:multiple-value-bind (pure-lambdabody ftype)
          (analyze-typed-lambdabody (cadr arg) (cddr arg))
        (if ftype
          `(the ,ftype (cl:function ,@named (cl:lambda ,@pure-lambdabody)))
          `(cl:function ,@named (cl:lambda ,@(cdr arg)))))
    `(cl:function ,@(cdr whole)))))
(setf (find-class 'function) (find-class 'cl:function))
(deftype function (&rest args) `(cl:function ,@args))
(cl:defmethod documentation (x (doc-type (eql 'function)))
  (documentation x 'cl:function))
(cl:defmethod (setf documentation) (new-value x (doc-type (eql 'function)))
  (funcall #'(setf documentation) new-value x 'cl:function))
(set-dispatch-macro-character #\# #\'
  (cl:function
    (cl:lambda (stream sub-char n)
      (when (and n (not *read-suppress*))
        (error "no number allowed between # and '"))
      (list 'function (read stream t nil t)))))

#| TODO: (defmacro handler-case |#

(defmacro labels (&whole whole
                  bindings &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-flet-bindings bindings)
    (if declspecs
      `(cl:labels ,pure-bindings (declare ,@declspecs) ,@body)
      `(cl:labels ,@(cdr whole)))))

(defmacro lambda (&whole whole
                  lambdalist &body body)
  (cl:multiple-value-bind (pure-lambdabody ftype)
      (analyze-typed-lambdabody lambdalist body)
    (if ftype
      `(the ,ftype (cl:function (cl:lambda ,@pure-lambdabody)))
      `(cl:function (cl:lambda ,@(cdr whole))))))

(defmacro let (&whole whole
               bindings &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-variable-bindings bindings)
    (if declspecs
      `(cl:let ,pure-bindings (declare ,@declspecs) ,@body)
      `(cl:let ,@(cdr whole)))))

(defmacro let* (&whole whole
                bindings &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-variable-bindings bindings)
    (if declspecs
      `(cl:let* ,pure-bindings (declare ,@declspecs) ,@body)
      `(cl:let* ,@(cdr whole)))))

#| TODO: (defmacro loop ...) |#

(defmacro multiple-value-bind (&whole whole
                               variables values-form &body body)
  (cl:let ((declspecs '())
           (pure-variables '()))
    (cl:dolist (var variables)
      (if (typed-var-p var)
        (progn
          (push (typed-var-variable var) pure-variables)
          (push `(type ,(typed-var-type var) ,(typed-var-variable var)) declspecs))
        (push var pure-variables)))
    (setq declspecs (nreverse declspecs))
    (setq pure-variables (nreverse pure-variables))
    (if declspecs
      `(cl:multiple-value-bind ,pure-variables ,values-form
         (declare ,@declspecs)
         ,@body)
      `(cl:multiple-value-bind ,@(cdr whole)))))

(defmacro prog (&whole whole
                bindings &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-variable-bindings bindings)
    (if declspecs
      `(cl:prog ,pure-bindings (declare ,@declspecs) ,@body)
      `(cl:prog ,@(cdr whole)))))

(defmacro prog* (&whole whole
                 bindings &body body)
  (cl:multiple-value-bind (pure-bindings declspecs)
      (untyped-variable-bindings bindings)
    (if declspecs
      `(cl:prog* ,pure-bindings (declare ,@declspecs) ,@body)
      `(cl:prog* ,@(cdr whole)))))

#| TODO: (defmacro restart-case ... |#

(defmacro with-accessors (&whole whole
                          slot-entries instance-form &body body)
  (cl:let ((declspecs '())
           (pure-slot-entries '()))
    (cl:dolist (slot-entry slot-entries)
      (if (and (consp slot-entry) (typed-var-p (first slot-entry)))
        (cl:let ((var (first slot-entry)))
          (push (cons (typed-var-variable var) (rest slot-entry)) pure-slot-entries)
          (push `(type ,(typed-var-type var) ,(typed-var-variable var)) declspecs))
        (push slot-entry pure-slot-entries)))
    (setq declspecs (nreverse declspecs))
    (setq pure-slot-entries (nreverse pure-slot-entries))
    (if declspecs
      `(cl:with-accessors ,pure-slot-entries ,instance-form
         (declare ,@declspecs)
         ,@body)
      `(cl:with-accessors ,@(cdr whole)))))

(defmacro with-input-from-string (&whole whole
                                  (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:with-input-from-string (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:with-input-from-string ,@(cdr whole))))

(defmacro with-open-file (&whole whole
                          (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:with-open-file (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:with-open-file ,@(cdr whole))))

(defmacro with-open-stream (&whole whole
                            (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:with-open-stream (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:with-open-stream ,@(cdr whole))))

(defmacro with-output-to-string (&whole whole
                                 (var &rest options) &body body)
  (if (typed-var-p var)
    `(cl:with-output-to-string (,(typed-var-variable var) ,@options)
       (declare (type ,(typed-var-type var) ,(typed-var-variable var)))
       ,@body)
    `(cl:with-output-to-string ,@(cdr whole))))

(defmacro with-slots (&whole whole
                      slot-entries instance-form &body body)
  (cl:let ((declspecs '())
           (pure-slot-entries '()))
    (cl:dolist (slot-entry slot-entries)
      (if (typed-var-p slot-entry)
        (progn
          (push (typed-var-variable slot-entry) pure-slot-entries)
          (push `(type ,(typed-var-type slot-entry) ,(typed-var-variable slot-entry)) declspecs))
        (if (and (consp slot-entry) (typed-var-p (first slot-entry)))
          (cl:let ((var (first slot-entry)))
            (push (cons (typed-var-variable var) (rest slot-entry)) pure-slot-entries)
            (push `(type ,(typed-var-type var) ,(typed-var-variable var)) declspecs))
          (push slot-entry pure-slot-entries))))
    (setq declspecs (nreverse declspecs))
    (setq pure-slot-entries (nreverse pure-slot-entries))
    (if declspecs
      `(cl:with-slots ,pure-slot-entries ,instance-form
         (declare ,@declspecs)
         ,@body)
      `(cl:with-slots ,@(cdr whole)))))
