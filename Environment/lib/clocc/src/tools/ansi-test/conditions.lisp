;;; based on v1.12 -*- mode: lisp -*-
;;;; Test suite for the Common Lisp condition system
;;;; Written by David Gadbois <gadbois@cs.utexas.edu> 30.11.1993
(in-package :cl-user)

;;;
;;; Helpers
;;;

#+(or clisp allegro cmu sbcl)
(check-for-bug :conditions-legacy-11
  #+CLISP
  (defun my-cpl (class)
    (clos::class-precedence-list (clos:find-class class))
    )
  #+ALLEGRO
  (defun my-cpl (class)
    (clos:finalize-inheritance (find-class class))
    (clos:class-precedence-list (find-class class))
    )
  #+cmu
  (defun my-cpl (class)
    (pcl:class-precedence-list (find-class class))
    )
  #+sbcl
  (defun my-cpl (class)
    (sb-pcl:class-precedence-list (find-class class))
    )
  MY-CPL)

(check-for-bug :conditions-legacy-31
  (defun check-superclasses (class expected)
    (let ((expected (list* class 't
                           #+(or CLISP ALLEGRO) 'standard-object
                           #+(or cmu sbcl) 'instance
                           'condition expected))
          (super (mapcar #' #+(or CLISP ALLEGRO) class-name
                            #+cmu pcl:class-name
                            #+sbcl sb-pcl:class-name
                            (my-cpl class))))
      (list (set-difference super expected)
            (set-difference expected super))))
  CHECK-SUPERCLASSES)

;;;
;;; IGNORE-ERRORS
;;;
;;; If this does not work, none of the tests that check for getting an error
;;; will.

;;; IGNORE-ERRORS should work.
(check-for-bug :conditions-legacy-54
  (multiple-value-bind (value condition)
      (ignore-errors (error "Foo"))
    (list value (type-of condition)))
  (nil simple-error))

;;; IGNORE-ERRORS should not interfere with values in non-error situations.
(check-for-bug :conditions-legacy-61
  (multiple-value-list
      (ignore-errors (values 23 42)))
  (23 42))

;;;
;;; Predefined condition types.
;;;

(check-for-bug :conditions-legacy-70
  (check-superclasses 'warning '())
  (nil nil))


(check-for-bug :conditions-legacy-74
  (check-superclasses 'style-warning '(warning))
  (nil nil))

(check-for-bug :conditions-legacy-78
  (check-superclasses 'serious-condition '())
  (nil nil))

(check-for-bug :conditions-legacy-82
  (check-superclasses 'error '(serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-86
  (check-superclasses 'cell-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-90
  (check-superclasses 'parse-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-94
  (check-superclasses 'storage-condition '(serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-98
  (check-superclasses 'simple-error '(simple-condition error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-102
  (check-superclasses 'simple-condition '())
  (nil nil))

(check-for-bug :conditions-legacy-106
  (check-superclasses 'simple-warning '(simple-condition warning))
  (nil nil))

(check-for-bug :conditions-legacy-110
  (check-superclasses 'file-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-114
  (check-superclasses 'control-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-118
  (check-superclasses 'program-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-122
  (check-superclasses 'undefined-function '(cell-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-126
  (check-superclasses 'arithmetic-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-130
  (check-superclasses 'division-by-zero '(arithmetic-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-134
  (check-superclasses 'floating-point-invalid-operation '(arithmetic-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-138
  (check-superclasses 'floating-point-inexact '(arithmetic-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-142
  (check-superclasses 'floating-point-overflow '(arithmetic-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-146
  (check-superclasses 'floating-point-underflow '(arithmetic-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-150
  (check-superclasses 'unbound-slot '(cell-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-154
  (check-superclasses 'package-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-158
  (check-superclasses 'print-not-readable '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-162
  (check-superclasses 'reader-error '(parse-error stream-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-166
  (check-superclasses 'stream-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-170
  (check-superclasses 'end-of-file '(stream-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-174
  (check-superclasses 'unbound-variable '(cell-error error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-178
  (check-superclasses 'type-error '(error serious-condition))
  (nil nil))

(check-for-bug :conditions-legacy-182
  (check-superclasses 'simple-type-error
                      '(simple-condition
                        type-error error serious-condition))
  (nil nil)
  "Condition Type SIMPLE-TYPE-ERROR

Class Precedence List:

simple-type-error, simple-condition, type-error, error, serious-condition, condition, t
")

;;;
;;; Defining conditions.
;;;
(check-for-bug :conditions-legacy-197
  (progn (define-condition test () ()) t)
  T)

(check-for-bug :conditions-legacy-201
  (check-superclasses  'test '())
  (nil nil))

(check-for-bug :conditions-legacy-205
  (progn (define-condition test2 (test) ()) t)
  T)

(check-for-bug :conditions-legacy-209
  (check-superclasses 'test2 '(test))
  (nil nil))

(check-for-bug :conditions-legacy-213
  (progn (define-condition test3 (test2 simple-condition) ()) t)
  T)

(check-for-bug :conditions-legacy-217
  (check-superclasses 'test3 '(test2 test simple-condition))
  (nil nil))

;;;
;;; Making conditions
;;;
(check-for-bug :conditions-legacy-224
  (progn (make-condition 'test) t)
  T)

(check-for-bug :conditions-legacy-228
  (ignore-errors (progn (make-condition 'integer) t))
  NIL)

;;;
;;; :REPORT option to DEFINE-CONDITION
;;;
(check-for-bug :conditions-legacy-235
  (progn (define-condition test4 (test3)
	  ()
	  (:report (lambda (condition stream)
		     (format stream "Yow! -- ~S" (type-of condition)))))
	t)
 T)

(check-for-bug :conditions-legacy-243
  (with-output-to-string (s) (princ (make-condition 'test4) s))
  "Yow! -- TEST4")

(check-for-bug :conditions-legacy-247
  (progn (define-condition test5 (test4) ()) t)
  T)

(check-for-bug :conditions-legacy-251
  (with-output-to-string (s) (princ (make-condition 'test5) s))
  "Yow! -- TEST5")

(check-for-bug :conditions-legacy-255
  (with-output-to-string (s)
    (princ (make-condition 'test3
                           :format-control "And How! -- ~S"
                           :format-arguments '(23)) s))
  "And How! -- 23"
  "From simple-condition:

The type simple-condition represents conditions that are signaled by
signal whenever a format-control is supplied as the function's first
argument. The format control and format arguments are initialized with
the initialization arguments named :format-control and
:format-arguments to make-condition, and are accessed by the functions
simple-condition-format-control and
simple-condition-format-arguments. If format arguments are not
supplied to make-condition, nil is used as a default. "
  )

;;;
;;; Condition slots.
;;;
(check-for-bug :conditions-legacy-276
  (progn (define-condition test6 (test4)
	  ((foo :initarg :foo :initform 23 :accessor test6-foo))
	  (:report (lambda (condition stream)
		     (format stream "~S -- ~S"
			     (type-of condition)
			     (test6-foo condition)))))
	t)
 T)

(check-for-bug :conditions-legacy-286
  (test6-foo (make-condition 'test6))
  23)

(check-for-bug :conditions-legacy-290
  (test6-foo (make-condition 'test6 :foo 42))
  42)

(check-for-bug :conditions-legacy-294
  (setf (test6-foo (make-condition 'test6 :foo 42)) 17)
  17)

(check-for-bug :conditions-legacy-298
  (with-output-to-string (s) (princ (make-condition 'test6 :foo 42) s))
  "TEST6 -- 42")

;;;
;;; HANDLER-BIND
;;;

;;; You do not have to bind handlers.
(check-for-bug :conditions-legacy-307
  (ignore-errors
    (handler-bind
        ()
      (error "Foo")))
  nil)

;;; Handlers should not interfere with values in non-error situations.
(check-for-bug :conditions-legacy-315
  (multiple-value-list
      (block foo
        (handler-bind
            ((error #'(lambda (c)
                        (declare (ignore c))
                        (return-from foo 23))))
          (values 42 17))))
  (42 17))

;;; Handlers should work.
(check-for-bug :conditions-legacy-326
  (multiple-value-list
      (block foo
        (handler-bind
            ((error #'(lambda (c)
                        (declare (ignore c))
                        (return-from foo (values 23 17)))))
          (error "Foo"))))
  (23 17))

;;; Only the appropriate handlers should be called.
(check-for-bug :conditions-legacy-337
  (ignore-errors
    (block foo
      (handler-bind
          ((type-error #'(lambda (c)
                           (declare (ignore c))
                           (return-from foo 23))))
        (error "Foo"))))
  nil)

;;; Handlers can be specified type expressions.
(check-for-bug :conditions-legacy-348
  (block foo
    (handler-bind
        (((or type-error error)
          #'(lambda (c)
              (declare (ignore c))
              (return-from foo 23))))
      (error "Foo")))
  23
  "typespecifier can be non-trivial.")

;;; Handlers should be undone.
(check-for-bug :conditions-legacy-360
  (ignore-errors
    (block foo
      (let ((first-time t))
        (handler-bind
            ((error
              #'(lambda (c)
                  (declare (ignore c))
                  (if first-time
                      (progn
                        (setq first-time nil)
                        (error "Bar"))
                      (return-from foo 23)))))
          (error "Foo")))))
  nil)

;;; Handlers should be undone.
(check-for-bug :conditions-legacy-377
  (block foo
    (let ((first-time t))
      (handler-bind
          ((error
            #'(lambda (c)
                (declare (ignore c))
                (return-from foo 23))))
        (handler-bind
            ((error
              #'(lambda (c)
                  (declare (ignore c))
                  (if first-time
                      (progn
                        (setq first-time nil)
                        (error "Bar"))
                      (return-from foo 42)))))
          (error "Foo")))))
  23)

;;; Handlers in the same cluster should be accessible.
(check-for-bug :conditions-legacy-398
  (ignore-errors
    (block foo
      (handler-bind
          ((error
            #'(lambda (c) (declare (ignore c)) nil))
           (error
            #'(lambda (c)
                (declare (ignore c))
                (return-from foo 23))))
        (error "Foo"))))
  23
  "If a handler declines (ie. just return) the next available is used, so
 the first one just returns nil, and the second, returning 23 is called")

;;; Multiple handlers should work.
(check-for-bug :conditions-legacy-414
  (block foo
    (handler-bind
        ((type-error
          #'(lambda (c)
              (declare (ignore c))
              (return-from foo 42)))
         (error
          #'(lambda (c)
              (declare (ignore c))
              (return-from foo 23))))
      (error "Foo")))
  23)

;;; Handlers should be undone.
(check-for-bug :conditions-legacy-429
  (block foo
    (handler-bind
        ((error #'(lambda (c)
                    (declare (ignore c))
                    (return-from foo 23))))
      (block bar
        (handler-bind
            ((error #'(lambda (c)
                        (declare (ignore c))
                        (return-from foo 42))))
          (return-from bar)))
      (error "Foo")))
  23)

;;;
;;; HANDLER-CASE
;;;

;;; HANDLER-CASE should handle errors.
(check-for-bug :conditions-legacy-449
  (multiple-value-list
      (handler-case
   (error "Foo")
   (error (c) (when (typep c 'error) (values 23 42)))))
 (23 42))

;;; Except those it doesn't handle.
(check-for-bug :conditions-legacy-457
 (ignore-errors
   (handler-case
    (error "Foo")
    (type-error () 23)))
 NIL)

;;; You don't have to specify handlers.
(check-for-bug :conditions-legacy-465
 (ignore-errors
   (handler-case
    (error "Foo")))
 NIL)

;;; HANDLER-CASE should not interfere with values in non-error situations.
(check-for-bug :conditions-legacy-472
 (multiple-value-list
  (handler-case
   (values 42 17)
   (error () 23)))
 (42 17))

;;; :NO-ERROR should return values.
(check-for-bug :conditions-legacy-480
 (multiple-value-list
  (handler-case
   (values 23 42)
   (:no-error (a b)
	      (values b a))))
 (42 23))

;;; Except when there is an error.
(check-for-bug :conditions-legacy-489
 (handler-case
  (error "Foo")
  (error () 23)
  (:no-error (&rest args) (declare (ignore args)) 42))
 23)

;;; It does not have to be the last clause.
(check-for-bug :conditions-legacy-497
  (handler-case
  23
  (:no-error (v) (1+ v))
  (error () 42))
 24
 "The spec is not 100% clear here...
Macro HANDLER-CASE

Syntax:

handler-case expression [[{error-clause}* | no-error-clause]] => result*

clause::= error-clause | no-error-clause

So in the cause thing the no-error-clause can be everwhere,
in the real thing it looks like it can only be last.

Need to ask comp.lang.lisp...

")

;;; Multiple handlers should be OK.
(check-for-bug :conditions-legacy-520
  (handler-case
  (error "Foo")
  (type-error () 23)
  (error () 42))
 42)

;;; Handlers should get undone.
(check-for-bug :conditions-legacy-528
 (ignore-errors
   (progn
     (block foo
       (handler-case
	(return-from foo 23)
	(error () 42)))
     (error "Foo")))
 NIL)

;;; Ditto.
(check-for-bug :conditions-legacy-539
 (ignore-errors
   (block foo
     (let ((first-time t))
       (handler-case
	(error "Foo")
	(error ()
	       (if first-time
		   (progn
		     (setf first-time nil)
		     (error "Bar"))
		   (return-from foo 23)))))))
 NIL)



