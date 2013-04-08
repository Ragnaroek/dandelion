;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base:10 -*-
;;;;
;;;;	Author:	Frank A. Adrian
;;;;
;;;; Release history:
;;;;		20010605 -	Release 1.1
;;;;		20010527 -	Release 1.0
;;;;
;;;; Modification history:
;;;;		20010605 -	Added licensing text, compare-fn keyword.
;;;;		20010604 -	Added :input-form and :output-form options,
;;;;					failed-tests function
;;;;		20010524 -	Code readied for public distribution.
;;;;		20010219 -	Added list-* functions.
;;;;		20000614 -	Added input-fn, output-fn.
;;;;		20000520 -	Added categories.
;;;;		20000502 -	Added deftest.
;;;;		20000428 -	Initial Revision.
;;;;
;;;; Copyright (c) 2000,2001.  Frank A. Adrian.  All rights reserved.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;;
;;;; The author also requests that any changes and/or improvents to the
;;;; code be shared with the author for use in subsequent releases.  Author's
;;;; E-mail: fadrian@qwest.net.
;;;;
;;;;

(defpackage :org.ancar.CLUnit
	(:use "COMMON-LISP")
;Kill the next form in Corman and Franz Lisps because their defpackage :documentation
;option is broken.
#-(or :cormanlisp excl)
	(:documentation
		"This package contains a unit testing environment for Common Lisp.
		All tests are held in the system image.  Each test has a name and
		a category.  All tests in the system can be run, as can all tests
		in a given category.
				
		The tests are specified by a test function that is normally written
		so as to take no input and to return T if the test passes.  Optionally,
		an input function and/or an output function can also be specified.
		If an input function is specified, the test function is applied to
		the return value(s) of the input function.  If the output function
		is specified, then the return value(s) of the test function is
		compared (via #'eql) to the return value(s) of the output function
		to check if the test succeeded.
				
		The package provides several functions and a deftest macro that makes
		specifying a test simple:
			clear-tests: 		Remove all tests from the system.
			remove-test: 		Remove a test from the system by name.
			run-category:		Run all tests from a given category.
			run-all-tests:		Run all the tests in the system.
			list-categories:	List the categories of tests in the system.
			list-tests:			List all of the tests in the system.
			run-named-test:		Run the test of the given name (mainly for
								debugging use after a given test has not
								passed).
			failed-tests:		Return a list of all tests that failed during the
								last run-all-tests or run-category call.
			deftest:			Define a test for the system."))
		 
(in-package :org.ancar.CLUnit)
(provide :org.ancar.CLUnit)

(defconstant *not-categorized* "*UNCATEGORIZED*")
(defun t-func () t)
(defun nil-func () nil)

(defclass test ()
	((descr
		:accessor descr
		:initform ""
		:initarg :descr)
	 (category
		:accessor category
		:initform *not-categorized*
		:initarg :category)
	 (test-fn
		:accessor test-fn
		:initform #'t-func
		:initarg :test-fn)
	 (compare-fn
		:accessor compare-fn
		:initform #'equal
		:initarg :compare-fn)
	 (input-fn
		:accessor input-fn
		:initform nil
		:initarg :input-fn)
	 (output-fn
		:accessor output-fn
		:initform nil
		:initarg :output-fn))
	(:documentation
		"Test holds information that enables test to be located and run.
		Slots:
			descr:		Test name.
			category:	Category test belongs to.
			test-fn:	Function run for test - by default, a zero-input,
						boolean output function. T means the test succeeded.
			compare-fn:	Function that compares test function output to the
						expected output.  Takes 2 lists of values.
			input-fn:	Function that provides input to the test.  When this
						item is used, test-fn is applied to the values returned
						by this function.
			output-fn:	Function that provides data that the output of test-fn
						is compared against.
		Things to do (when necessary):
			Add test-fn slot to hold equality test to compare output of
			test-fn to output of output-fn."))

(defmethod print-object ((tst test) (str t))
	(print-unreadable-object (tst str :type t :identity t)
		(format str "~A/~A" (descr tst) (category tst))))

(defvar *all-tests* nil
	"Currently, this is a simple list of tests.  If the number of tests
	starts becoming too large, this should probably turn into a hash-table
	of tests hashed on category name.")

(defun clear-tests ()
	"Remove all tests from the system."
	(setf *all-tests* nil))

(defun remove-test (test-name)
	"Remove the test with the given name."
	;(format t "In remove-test~%")
	(setf *all-tests*
		(delete-if #'(lambda (i) (string-equal (descr i) test-name)) *all-tests*)))

(defmethod initialize-instance :after ((tst test) &rest initargs)
	(declare (ignore initargs))
	"Add each test as it is created to the *all-tests* collection."
	;(format t "In initialize-instance for test.  Initargs: ~A~%" initargs)
	(remove-test (descr tst))
	(push tst *all-tests*))

(defmethod run-unprotected ((test test))
	"Run a test.  No protection against errors."
	(let* ((input-fn (input-fn test))
		  (output-fn (output-fn test))
		  (test-fn (test-fn test))
		  (has-specified-input-fn input-fn))
		
		(unless input-fn (setf input-fn #'nil-func))
		(unless output-fn (setf output-fn #'t-func))
		(let ((test-input (multiple-value-list (funcall input-fn))))
			;(format t "~&Input: ~A~%" test-input)
			(let ((vals (multiple-value-list 
							(if has-specified-input-fn
								(apply test-fn test-input)
								(funcall test-fn))))
				  (tvals (multiple-value-list (funcall output-fn))))
				;(format t "~&Test output: ~A~%Expected output: ~A~%"
				;	vals tvals)
				(funcall (compare-fn test) vals tvals)))))

(defmethod run-protected ((test test))
	"Protect the test while running with ignore-errors."
	(let ((vals (multiple-value-list (ignore-errors (run-unprotected test)))))
		;(format t "~&vals: ~A~%" vals)
		(unless (eq (car vals) t)
			(if (cadr vals)
				(format t "~&~A occurred in test ~S~%"
					(cadr vals) (descr test))
				(format t "~&Output did not match expected output in test ~S~%"
					(descr test))))
		vals))

(defun test-or-tests (count)
	"This is for Corman Lisp which does not handle ~[ quite correctly."
	(if (eq count 1) "test" "tests"))

(defvar *failed-tests* nil
	"Holds the set of failed tests from last test run.")

(defun failed-tests ()
	"Return the set of tests that failed during the last test run"
	*failed-tests*)
	
(defun run-tests (tests)
	"Run the set of tests passed in."
	(let ((passed-tests nil)
		  (failed-tests nil))
		(loop for test in tests do
			;(format t "~&Running test: ~A~%" test)
			(let ((test-result (run-protected test)))
				(if (eq (car test-result) t)
					(push test passed-tests)
					(push test failed-tests))))
		(setf *failed-tests* failed-tests)
;		(format t "~&Passed tests: ~A; failed tests: ~A.~%"
;			passed-tests failed-tests)
		(let ((passed-count (length passed-tests))
			  (failed-count (length failed-tests)))
;			(format t "~&Passed count: ~A; failed count: ~A~%"
;				passed-count failed-count)
;			(format t "~&~A ~[tests~;test~:;tests~] run; ~A ~[tests~;test~:;tests~] passed; ~A ~[tests~;test~:;tests~] failed.~%"
;				(+ passed-count failed-count) (+ passed-count failed-count)
;				passed-count passed-count failed-count failed-count)
			(format t "~&~A ~A run; ~A ~A passed; ~A ~A failed.~%"
				(+ passed-count failed-count) (test-or-tests (+ passed-count failed-count))
				passed-count (test-or-tests passed-count)
				failed-count (test-or-tests failed-count))
		(values (null failed-tests) failed-count passed-count))))

(defun filter-tests (category)
	"Filter tests by category."
	(remove-if #'(lambda (test) ;(format t "~&~A~A~%" category (category test))
		(not (string-equal category (category test))))
		*all-tests*))

(defun run-category (category)
	"Run all the tests in a given category."
	(run-tests (filter-tests category)))

(defun run-all-tests ()
	"Run all tests in the system."
	(run-tests *all-tests*))

(defmacro form-to-fn (form)
	"Return a function that will return the form when evaluated.
	Will be used when we add input-form and output-form parameters to
	deftest."
	`#'(lambda () ,form))

(defmacro deftest (description &key	category
									(test-fn #'t-func)
									(input-fn nil input-fn-present)
									(output-fn nil output-fn-present)
									(input-form nil input-form-present)
									(output-form nil output-form-present)
									(compare-fn #'equal))
	
	"Use of :input-fn and :output-fn keywords override use of :input-form and
	:output-form keywords respectively."
	
	(let ((mia-args-gen (gensym))
		  (cat-gen (gensym))
		  (ifmfn `#'(lambda () ,input-form))
		  (ofmfn `#'(lambda () ,output-form)))
		`(let (,mia-args-gen
			   (,cat-gen ,category))
			(push :descr ,mia-args-gen) (push ,description ,mia-args-gen)
			(when ,cat-gen
				(push :category ,mia-args-gen) (push ,cat-gen ,mia-args-gen))
			(push :compare-fn ,mia-args-gen) (push ,compare-fn ,mia-args-gen)
			(push :test-fn ,mia-args-gen) (push ,test-fn ,mia-args-gen)
			(when (and ,output-form-present (not ,output-fn-present))
				(push :output-fn ,mia-args-gen) (push ,ofmfn ,mia-args-gen))				
			(when ,output-fn-present
				(push :output-fn ,mia-args-gen) (push ,output-fn ,mia-args-gen))
			(when (and ,input-form-present (not ,input-fn-present))
				(push :input-fn ,mia-args-gen) (push ,ifmfn ,mia-args-gen))				
			(when ,input-fn-present
				(push :input-fn ,mia-args-gen) (push ,input-fn ,mia-args-gen))
			(apply #'make-instance 'test (nreverse ,mia-args-gen)))))
				
		
	
(defun list-categories ()
	"List all of the categories in the system."
	(let (cats)
		(loop for test in *all-tests* doing
			(setf cats (adjoin (category test) cats :test #'string-equal)))
		cats))

(defun list-tests (&optional category)
	"List the tets in the system / category."
	(let ((tests (if category (filter-tests category) *all-tests*)))
		(loop for test in tests collecting
			(concatenate 'string (descr test) "/" (category test)))))

(defun run-named-test (name &optional protected)
	"Run the given test in either protected or unprotected mode."
	(let ((test (find name *all-tests* :key #'descr :test #'string-equal)))
		(when test
			(if protected
				(run-protected test)
				(run-unprotected test)))))

(export '(
		run-category
		run-all-tests
		clear-tests
		remove-test
		deftest
		list-categories
		list-tests
		run-named-test
		failed-tests
		;with-supressed-summary
		))

(in-package "COMMON-LISP-USER")
(use-package :org.ancar.CLUnit)

;;;
;;; Self test...
;;;

;; tests basic test definition
(deftest "test1" :category "CLUnit-pass1"
	:test-fn #'(lambda () (eq (car '(a)) 'a)))

;; tests input-fn
(deftest "test-2" :category "CLUnit-pass1"
	:input-fn #'(lambda () '(a))
	:test-fn #'(lambda (x) (eq (car x) 'a)))

;; tests output-fn
(deftest "test-3" :category "CLUnit-pass1"
	:input-fn #'(lambda () '(a))
	:output-fn #'(lambda () 'a)
	:test-fn #'(lambda (x) (car x)))

;; tests remove-test, run-category, and multiple-values in test-fn and
;; output-fn
(deftest "meta" :category "CLUnit-meta"
	:input-fn #'(lambda () (remove-test "test1"))
	:test-fn #'(lambda (x) (declare (ignore x)) (run-category "CLUnit-pass1"))
	:output-fn #'(lambda () (values t 0 2)))

;; tests multiple values from input-fn to test-fn
(deftest "test1" :category "CLUnit-pass2"
	:input-fn #'(lambda () (values 'a '(b)))
	:test-fn #'cons
	:output-fn #'(lambda () '(a b)))

;;check error trapping
(deftest "meta2" :category "CLUnit-meta"
	:input-fn
		#'(lambda () (deftest "Error test" :category "CLUnit-pass3"
						:test-fn #'(lambda ()
							(remove-test "Error test") (error "Dummy error"))))
	:test-fn #'(lambda (x) (declare (ignore x)) (run-category "CLUnit-pass3"))
	:output-fn #'(lambda () (values nil 1 0)))

;;check input-form
(deftest "testx" :category "CLUnit"
	:input-form '(a b c)
	:test-fn #'car
	:output-fn #'(lambda () 'a))

;;check output form
(deftest "testx2" :category "CLUnit"
	:input-form '(a b c)
	:test-fn #'car
	:output-form 'a)

;;check multiple input-forms
(deftest "testx3" :category "CLUnit"
	:input-form (values '(1 2 3) '(10 20 30))
	:test-fn #'(lambda (&rest lists) (car lists))
	:output-fn #'(lambda () '(1 2 3)))

;;check multiple output-forms
(deftest "testx4" :category "CLUnit"
	:input-form (values '(1 2 3) '(10 20 30))
	:test-fn #'(lambda (&rest lists) (apply #'values lists))
	:output-fn #'(lambda () (values '(1 2 3) '(10 20 30))))

;;check failed-tests
(deftest "meta5" :category "CLUnit-meta"
	:input-fn
		#'(lambda () (deftest "Error test" :category "CLUnit-pass4"
						:test-fn #'(lambda ()
							(remove-test "Error test") (error "Dummy error"))))
	:test-fn #'(lambda (x) (declare (ignore x))
				(run-category "CLUnit-pass4")
				(values (length (failed-tests)) (org.ancar.CLUnit::descr (car (failed-tests)))))
	:output-fn #'(lambda () (values 1 "Error test")))

(deftest "Test compare-fn"
	:test-fn #'(lambda () "abc")
	:output-form "abc"
	:compare-fn #'(lambda (rlist1 rlist2)
					(reduce #'and (mapcar #'string-equal rlist1 rlist2))))

;;; run self test	
(when (run-all-tests)
	(format t "~&CLUnit self-test passed.~%")
	(clear-tests)
	(values))
