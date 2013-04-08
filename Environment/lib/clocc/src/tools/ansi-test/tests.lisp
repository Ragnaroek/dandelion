;; Test-Suiten ablaufen lassen: -*- mode: lisp -*-
;; based on version 1.9

(in-package :cl-user)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

;;; known bugs: When (GETHASH BUGID *BUGID->KNOWNP*), the bug
;;; associated with BUGID is a known bug, and I don't want to hear
;;; about it right now, probably because I'm currently trying to
;;; determine whether I've introduced new bugs.
(defvar *bugid->knownp* (make-hash-table))

;;; Much of the point of BUGIDs would be lost if they weren't unique
;;; across the test suite. Here we keep track of the ones we've already
;;; seen in order to look for duplicates; and it also helps us check
;;; for *BUGID->WINP* entries which don't correspond to any actual BUGID.
(defvar *bugid->seenp* (make-hash-table))

(defmacro with-ignored-errors (&rest forms)
  "This macro will evaluate the forms and return
the returnvalues or the type of the condition used."
  (let ((tag (gensym)))
    `(block ,tag
       (handler-bind
           ((serious-condition
             #'(lambda (condition)
                 (return-from ,tag
                   (values :ERROR
                           condition)))))
         ,@forms))))

(defvar *log* nil)
(defvar *output-generated* nil)
(defvar *lisp-type*
  #+lispworks "LWL"
  #+CLISP "CLISP"
  #+AKCL "AKCL"
  #+CMU "CMUCL"
  #+sbcl "SBCL")

(defun check-and-puke (bugid mode form result my-result condition why)
  (flet ((safe-format (stream string &rest args)
           (unless (ignore-errors
                     (progn
                       (apply #'format stream string args)
                       t))
             (format stream "~&~%format of ~S failed!"
                     string))))
    (cond
      ((eql result my-result)
       (safe-format t "~%EQL-OK: ~S" my-result))
      ((equal result my-result)
       (safe-format t "~%EQUAL-OK: ~S" my-result))
      ((equalp result my-result)
       (safe-format t "~%EQUALP-OK: ~S" my-result))
      ((eq my-result :ERROR)
       (cond
         ((ignore-errors
            (typep condition result))
          (safe-format t "~%TYPEP-OK, is of the expected error :~S"
                       result))
         (t
          (safe-format
           t
           "~&~%ERROR!! (BUGID=~S) Got an error ~S (~A) I expected a instance of ~S~%"
           bugid condition condition
           result)
          (safe-format
           t
           "~%Form: ~S~%Should be an error of type: ~S~%~A: ~S (~A)~%Why: ~S~%"
           form result *lisp-type*
           condition condition
           why)
          (setf *output-generated* t)
          (safe-format
           *log*
           "~&~%Bugid: ~S ~A Form: ~S~%Should be an error of type: ~S~%~A: ~S (~A) ~%Why: ~S~%"
           bugid mode form result *lisp-type*
           condition condition
           why))))
      (t
       (safe-format t
                    "~&~%ERROR!! (BUGID=~S) Got ~S solution ~S expected!"
                    bugid my-result result)
       (safe-format t
                    "~%~A Form: ~S~%Should be: ~S~%~A: ~S~%Why: ~S~%"
                    mode form result *lisp-type*
                    my-result why)
       (setf *output-generated* t)
       (safe-format *log*
                    "~&~%Bugid: ~S ~A Form: ~S~%Should be: ~S~%~A: ~S~%Why : ~S~%"
                    bugid mode form result *lisp-type*
                    my-result why)))))

;;; Test for bug of given BUGID by executing FORM, expecting RESULT.
;;;
;;; about BUGIDs:
;;;   * A BUGID must be something which can be compared with EQL,
;;;     because we use it in a plain vanilla hash table. This could
;;;     easily be changed to EQUALity if people want to use strings
;;;     for BUGIDs. However, arguably symbols are the right thing
;;;     anyway, since they're the native Lisp way of identifying
;;;     something uniquely.
;;;   * BUGIDs must be unique, since otherwise the system reasonably
;;;     assumes that someone made a clerical error.
;;;   * Ideally BUGIDs should be something at least slightly mnemonic,
;;;     e.g. :LOOP-BARFS-ON-DOUBLE-COMPLEX or at least :TYPED-DEFSTRUCT-1,
;;;     :TYPED-DEFSTRUCT-2, etc. But they don't need to be, and aren't
;;;     necessarily. (A script was used to create marginally-mnemonic
;;;     names of the form <filenamestem>-LEGACY-<oldlinenumber> were
;;;     created automatically for old tests when BUGIDs were introduced.)
(defmacro check-for-bug (bugid form result &optional (why ""))
  `(progn

     ;; testing the test suite itself: BUGIDs should be unique.
     (cond
       ((gethash ',bugid *bugid->seenp*)
        (format *log* "~&~%ERROR!! duplicate BUGID=~S~%" ',bugid)
        (format t "~&~%ERROR!! duplicate BUGID=~S~%" ',bugid))
       (t
        (setf (gethash ',bugid *bugid->seenp*) t)))

     ;; actually testing the Lisp implementation
     (cond ((gethash ',bugid *bugid->knownp*)
	    (format t "~&~%skipping known bug ~S~%" ',bugid))
	   (t
	    (format t "~&~%testing ~S: ~S~%" ',bugid ',form)

	    ;; First, we check whether it works in interpreted mode.
            (multiple-value-bind (my-result condition)
                (with-ignored-errors
                    (eval ',form))
	      (check-and-puke ',bugid
			      "interpreted"
                              ',form ',result
                              my-result condition
                              ,why))

            (force-output)

	    ;; Now we try to compile.
            #+nil                       ; HACK
            (multiple-value-bind (my-result condition)
                (with-ignored-errors
                    (multiple-value-bind (function warnings-p failure-p)
                        (compile nil
                                 #'(lambda ()
                                     ,form))
                      (format t "~&compiled  ~S ~S ~S"
                              function warnings-p failure-p)

                      (multiple-value-bind (my-result condition)
                          (with-ignored-errors
                              (funcall function))
                        (check-and-puke ',bugid
                                        "compiled"
                                        ',form ',result
                                        my-result condition
                                        ,why))))
              (when (eq my-result :error)
		(check-and-puke ',bugid
				"while compiling"
                                ',form ',result
                                my-result condition
				,why)))))))

(defun run-test (testname)
  (let ((*package* *package*)
	(*print-pretty* nil)
	(*print-circle* nil)
	;; to make the system quiet:
	#+(or cmu sbcl)
	(*gc-verbose* nil)
	#+(or cmu sbcl)
	(*compile-verbose* nil)
	#+(or cmu sbcl)
	(*compile-print* nil)
	#+(or cmu sbcl)
	(*compile-progress* nil)
	#+(or cmu sbcl)
	(*TOP-LEVEL-AUTO-DECLARE* nil))

    (with-open-file (*log* (format nil "~a.erg" testname)
			   :direction :output)
      (setf *output-generated* nil)
      (load (format nil "~a.lisp" testname))
      (force-output *log*))
    (unless *output-generated*
      (delete-file (format nil "~a.erg"
			   testname))))
  (values))

(defun run-all-tests ()

  (mapc #'run-test
	'(
	  "symboltest"
          "alltest"
	  "array"
	  "backquot"
          "characters"
	  #+(or ALLEGRO CLISP CMU GCL SBCL)"clos"
	  "cmucl-bugs"
	  #+(or ALLEGRO CLISP CMU GCL SBCL) "conditions"
	  "eval20"
	  #-gcl "excepsit"
	  "format"
	  #+xcl "hash"
	  "hashlong"
	  "iofkts"
	  "lambda"
	  "lists151"
	  "lists152"
	  "lists153"
	  "lists154"
	  "lists155"
	  "lists156"
	  #+(or CLISP ALLEGRO CMU SBCL) "loop"
	  "macro8"
	  "map"
	  #+(or CLISP ALLEGRO CMU SBCL) "mop"
	  "new-bugs"
	  #-(or cmu sbcl) "number"
	  #+clisp "number2"
	  #+(or XCL CLISP) "path"
	  #+xcl "readtable"
	  "section10"
	  "section11"
	  "section12"
	  "section13"
	  "section14"
	  "section15"
	  "section16"
	  "section17"
	  #-gcl "section18-errors"
	  "section18"
	  "section19"
	  "section2"
	  "section20"
	  "section21"
	  "section22"
	  "section3"
	  "section4"
	  "section5"
	  "section6"
	  "section7"
	  "section8"
	  "section9"
	  "setf"
	  "steele7"
	  #-allegro "streams"
	  #-gcl "streamslong"
	  "strings"
	  "symbol10"
	  "symbols"
          #-gcl "type"
	  #+(or sbcl cmu)
	  "unix-tests"
	  ))

  ;; testing the test suite itself again: Check for
  ;; mistyped/gone-stale/otherwise-hosed *BUGID->KNOWNP* data.
  (with-open-file (*log* "state-info.erg"
                         :direction :output)
    (loop for item in (list
                       'machine-version
                       'machine-type
                       'machine-instance
                       'lisp-implementation-version
                       'lisp-implementation-type
                       'software-type
                       'software-version)
          do
          (format *log* "~A: ~A~%" item (funcall item)
                  (lisp-implementation-version)))


    (maphash (lambda (bugid ignored-knownp)
               (declare (ignore ignored-knownp))
               (unless (gethash bugid *bugid->seenp*)
                 (format t
                         "~&~%ERROR!! KNOWNP BUGID=~S corresponds to no test."
                         bugid)
                 (format *log*
                         "~&~%ERROR!! KNOWNP BUGID=~S corresponds to no test."
                         bugid)))
             *bugid->knownp*))

  ;; Voila.
  t)

(run-all-tests)

(format t "~%~%tests complete~%")
#+cmu
(unix:unix-exit 0)

#+sbcl
(SB-UNIX:UNIX-EXIT 0)
