;;; Basic extensions: conditions, compositions &c
;;;
;;; Copyright (C) 1999-2006 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: ext.lisp,v 1.43 2006/04/07 21:59:23 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/ext.lisp,v $

(defpackage #:port
  (:use #:common-lisp)
  (:nicknames #:org.cons.clocc/sds/port)
  (:export #:code #:case-error #:not-implemented ; conditions
	   #:code-proc #:code-mesg #:code-args   ; slot accessors
           #:defsubst #:defcustom #:defconst
           #:mk-arr #:map-in #:with-gensyms
           #:gc #:quit
           #:+eof+ #:eof-p #:string-tokens #:remove-plist
           #-cmu #:required-argument
           #:unlock-package #:restore-package-lock
           #:compose #:compose-safe #:compose-f #:compose-all))

(in-package :port)

(setf (logical-pathname-translations "port")
      `(("**;*" ,(logical-pathname "clocc:src;port;**;*"))))

;;;
;;; Conditions
;;;

(define-condition code (error)
  ((proc :reader code-proc :initarg :proc :initform nil)
   (mesg :type (or null simple-string) :reader code-mesg
         :initarg :mesg :initform nil)
   (args :type list :reader code-args :initarg :args :initform nil))
  (:documentation "An error in the user code.")
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~s]~@[ ~?~]" (code-proc cc) (code-mesg cc)
                     (code-args cc)))))

(define-condition case-error (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "`~s' evaluated to `~s', not one of [~@{`~s'~^ ~}]"))
  (:documentation "An error in a case statement.
This carries the function name which makes the error message more useful."))

(define-condition not-implemented (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "not implemented for ~a [~a]")
   (args :type list :reader code-args :initform
         (list (lisp-implementation-type) (lisp-implementation-version))))
  (:documentation "Your implementation does not support this functionality."))

;;;
;;; Extensions
;;;

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name)) (defun ,name ,arglist ,@body)))

(defmacro defcustom (name type init doc)
  "Define a typed global variable."
  `(progn (declaim (type ,type ,name))
    (defvar ,name (the ,type ,init) ,doc)))

(defmacro defconst (name type init doc)
  "Define a typed constant."
  `(progn (declaim (type ,type ,name))
    ;; since constant redefinition must be the same under EQL, there
    ;; can be no constants other than symbols, numbers and characters
    ;; see ANSI CL spec 3.1.2.1.1.3 "Constant Variables"
    (,(if (subtypep type '(or symbol number character)) 'defconstant 'defvar)
     ,name (the ,type ,init) ,doc)))

(defmacro mk-arr (type init &optional len)
  "Make array with elements of TYPE, initializing."
  (if len `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
        :initial-contents ,init)))

(defmacro with-gensyms ((title &rest names) &body body)
  "Bind symbols in NAMES to gensyms.  TITLE is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy)
                     `(,sy (gensym ,(concatenate 'string title
                                                 (symbol-name sy) "-"))))
                   names))
     ,@body))

(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (with-gensyms ("MI-" mi)
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

(defun gc ()
  "Invoke the garbage collector."
  #+abcl (ext:gc)
  #+allegro (excl:gc)
  #+clisp (#+lisp=cl ext:gc #-lisp=cl lisp:gc)
  #+cmu (ext:gc)
  #+cormanlisp (cl::gc)
  #+gcl (si::gbc)
  #+lispworks (hcl:normal-gc)
  #+lucid (lcl:gc)
  #+sbcl (sb-ext:gc)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'gc)))

(defun quit (&optional code)
  #+abcl (ext:quit code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-status
                      (typecase code ((signed-byte 32) code) (null 0) (t 1)))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit code)))

(defconst +eof+ cons (list '+eof+)
  "*The end-of-file object.
To be passed as the third arg to `read' and checked against using `eq'.")

(defun eof-p (stream)
  "Return T if the stream has no more data in it."
  (null (peek-char nil stream nil nil)))

(defun string-tokens (string &key (start 0) end max)
  "Read from STRING repeatedly, starting with START, up to MAX tokens.
Return the list of objects read and the final index in STRING.
Binds `*package*' to the keyword package,
so that the bare symbols are read as keywords."
  (declare (type (or null fixnum) max) (type fixnum start))
  (let ((*package* (find-package :keyword)))
    (if max
        (do ((beg start) obj res (num 0 (1+ num)))
            ((or (= max num) (and end (>= beg end)))
             (values (nreverse res) beg))
          (declare (fixnum beg num))
          (setf (values obj beg)
                (read-from-string string nil +eof+ :start beg :end end))
          (if (eq obj +eof+)
              (return (values (nreverse res) beg))
              (push obj res)))
        (with-input-from-string (st string :start start :end end)
          (loop :for obj = (read st nil st)
            :until (eq obj st) :collect obj)))))

(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))

#+cmu (progn
        (import 'ext:required-argument :port)
        (export 'ext:required-argument :port))
#-cmu (progn
        ;; return type NIL means non-returning function
        (proclaim '(ftype (function () nil) required-argument))
        (defun required-argument ()
          "A useful default for required arguments and DEFSTRUCT slots."
          (error "A required argument was not supplied.")))

;;;
;;; package locking
;;;

(defvar *lock-package-saved-value*)

(defmacro unlock-package (pack)
  #+allegro
  `(eval-when (:compile-toplevel)
     (let ((pa (find-package ,pack)))
       (setf *lock-package-saved-value* (excl:package-definition-lock pa)
             (excl:package-definition-lock pa) nil)))
  #+clisp
  `(eval-when (:compile-toplevel)
     (setf *lock-package-saved-value* (ext:package-lock ,pack)
           (ext:package-lock ,pack) nil))
  #+ecl
  `(eval-when (:compile-toplevel)
     (si:package-lock (find-package ,pack) *lock-package-saved-value*)
     (makunbound '*lock-package-saved-value*))
  #+lispworks (declare (ignore pack))
  #+lispworks
  `(eval-when (:compile-toplevel :load-toplevel)
     (setf *lock-package-saved-value* lw:*handle-warn-on-redefinition*
           lw:*handle-warn-on-redefinition* nil))
  #-(or allegro clisp ecl lispworks)
  ;; nothing to be done
  (declare (ignore pack)))

(defmacro restore-package-lock (pack)
  #+allegro
  `(eval-when (:compile-toplevel)
     (setf (excl:package-definition-lock (find-package ,pack))
           *lock-package-saved-value*)
     (makunbound '*lock-package-saved-value*))
  #+clisp
  `(eval-when (:compile-toplevel)
     (setf (ext:package-lock ,pack) *lock-package-saved-value*)
     (makunbound '*lock-package-saved-value*))
  #+ecl
  `(eval-when (:compile-toplevel)
     (si:package-lock (find-package ,pack) *lock-package-saved-value*)
     (makunbound '*lock-package-saved-value*))
  #+lispworks (declare (ignore pack))
  #+lispworks
  `(eval-when (:compile-toplevel :load-toplevel)
     (setf lw:*handle-warn-on-redefinition* *lock-package-saved-value*)
     (makunbound '*lock-package-saved-value*))
  #-(or allegro clisp ecl lispworks)
  ;; nothing to be done
  (declare (ignore pack)))

;;;
;;; Function Compositions
;;;

(defmacro compose (&rest functions)
  "Macro: compose functions or macros of 1 argument into a lambda.
E.g., (compose abs (dl-val zz) 'key) ==>
  (lambda (yy) (abs (funcall (dl-val zz) (funcall key yy))))"
  (labels ((rec (xx yy)
             (let ((rr (list (car xx) (if (cdr xx) (rec (cdr xx) yy) yy))))
               (if (consp (car xx))
                   (cons 'funcall (if (eq (caar xx) 'quote)
                                      (cons (cadar xx) (cdr rr)) rr))
                   rr))))
    (with-gensyms ("COMPOSE-" arg)
      `(lambda (,arg) ,(rec functions arg)))))

(defmacro compose-safe (&rest functions)
  "Like COMPOSE, but return NIL as soon as an intermediate value is NIL."
  (labels ((rec (xx yy)
             (let* ((first (first xx)) (rest (rest xx))
                    (var (gensym (format nil "~S ~S " 'compose-safe first))))
               (if rest
                   `(let ((,var ,(rec rest yy))) (and ,var (,first ,var)))
                   `(and ,yy (,first ,yy))))))
    (with-gensyms ("COMPOSE-SAFE-" arg)
      `(lambda (,arg) ,(rec functions arg)))))

(defun compose-f (&rest functions)
  "Return the composition of all the arguments.
All FUNCTIONS should take one argument, except for
the last one, which can take several."
  (reduce (lambda (f0 f1)
            (declare (function f0 f1))
            (lambda (&rest args) (funcall f0 (apply f1 args))))
          functions :initial-value #'identity))

(defun compose-all (&rest functions)
  "Return the composition of all the arguments.
All the values from nth function are fed to the n-1th."
  (reduce (lambda (f0 f1)
            (declare (function f0 f1))
            (lambda (&rest args) (multiple-value-call f0 (apply f1 args))))
          functions :initial-value #'identity))

(provide :port-ext)
;;; file ext.lisp ends here
