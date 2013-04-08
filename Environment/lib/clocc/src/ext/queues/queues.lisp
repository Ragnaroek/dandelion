;;; -*- Mode: CLtL -*-

;;; queues.lisp --
;;; Simple queues in the Abelson and Sussman SICP style.
;;; Package definition file (in CLtL2 style).
;;;
;;; Author: Marco Antoniotti <marcoxa@cons.org>
;;;
;;; Copyright (c) 1992 - 2000 Marco Antoniotti. All rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

;;;============================================================================
;;; Documentation (initial)
;;;
;;; *QUEUES-VERSION*                                                [PARAMETER]
;;;
;;; QUEUE (name front rear size)                                    [STRUCTURE]
;;;    The QUEUE data type.
;;;
;;; EMPTY-QUEUE "()"                                                [CONDITION]
;;;
;;; QPRINT (q stream depth)                                          [FUNCTION]
;;;    Print function for the 'queue' data type.
;;;    Arguments and return value are standard.
;;;
;;; PPRINT-QUEUE (q)                                                 [FUNCTION]
;;;
;;; EMPTY-P (q)                                                      [FUNCTION]
;;;    Test whether a queue Q is empty.
;;;
;;; SIZE (q)                                                         [FUNCTION]
;;;    Returns the current number of elements in the queue Q.
;;;
;;; ENQUEUE (item q)                                                 [FUNCTION]
;;;    Inserts an ITEM at the end of the queue Q.
;;;
;;; DEQUEUE (q)                                                      [FUNCTION]
;;;    Removes the item at the beginning of the queue Q.
;;;    It is an error to try to remove something from an empty queue.
;;;
;;; FIRST (q)                                                        [FUNCTION]
;;;    Returns the head of the queue.
;;;
;;; LAST (q)                                                         [FUNCTION]
;;;    Returns the last element in the queue
;;;
;;; ENQUEUED-P (item q &key (key #'identity) (test #'eql))           [FUNCTION]
;;;    Checks whether an item is in the queue or not.
;;;
;;; CLEAR-QUEUE (q)                                                  [FUNCTION]
;;;    Clears the queue Q.
;;;
;;; DEQUEUE-ITEMS-IF (predicate queue)                               [FUNCTION]
;;;    Deletes all items from QUEUE for which PREDICATE is true.
;;;
;;; DO-QUEUE ((qvar q &optional (queue-result nil)) &body forms)        [MACRO]
;;;    Iteration macro for queues.
;;;    FORMS are executed once per each item in the queue from the queue
;;;    front to the rear, in an enviroment where QVAR is bound to each item
;;;    in turn.  If supplied, QUEUE-RESULT is returned at the end of the
;;;    iteration.
;;;

;;;============================================================================
;;; History:

;;;============================================================================
;;; Prologue

(eval-when (load compile eval)
  (unless (find-package "CL.UTIL.QUEUES")
    (load "QUEUES:queues-package")))


(in-package "CL.UTIL.QUEUES")


(defparameter *queues-version* "CL.UTIL.QUEUES 1.0")


(defstruct (queue (:print-function qprint))
  "The QUEUE data type."
  (name nil :type symbol)
  (front () :type list)
  (rear  () :type list)
  (size 0 :type integer))

(define-condition empty-queue (cell-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "CL.UTIL.QUEUES: queue ~S is empty."
		     (cell-error-name condition)))))


(defun qprint (q stream depth)
  "Print function for the 'queue' data type.
Arguments and return value are standard."
  (declare (ignore depth))
  (print-unreadable-object
   (q stream :type nil :identity t)
   (format stream
	   "QUEUE~@[ ~A~] [size ~D]~:[~;:~]"
	   (queue-name q)
	   (queue-size q)
	   (empty-p q))
   (when (not (empty-p q))
     (format stream " with first ~S last ~S:" (first q) (last q)))
   ))

(defun pprint-queue (q)
  (format t "QUEUE :")
  (when (not (empty-p q))
    (format t "~{ ~S~}~%" (queue-front q))))


(defun empty-p (q)
  "Test whether a queue Q is empty."
  (null (queue-front q)))


(defun size (q)
  "Returns the current number of elements in the queue Q."
  (queue-size q))


(defun enqueue (item q)
  "Inserts an ITEM at the end of the queue Q."
  (let ((qitem (list item)))
    (cond ((empty-p q)
	   (setf (queue-front q) qitem)
	   (setf (queue-rear q) qitem))
	  (t
	   (setf (cdr (queue-rear q)) qitem)
	   (setf (queue-rear q) qitem)))
    (incf (queue-size q))
    qitem))


(defun dequeue (q)
  "Removes the item at the beginning of the queue Q.
It is an error to try to remove something from an empty queue."
  (if (empty-p q)
      (error 'empty-queue :name q)
    (prog1
	(car (queue-front q))
      (decf (queue-size q))
      (setf (queue-front q) (cdr (queue-front q))))))

(defun first (q)
  "Returns the head of the queue."
  (if (empty-p q)
      (error 'empty-queue :name q)
      (common-lisp:first (queue-front q))))

(defun last (q)
  "Returns the last element in the queue"
  (if (empty-p q)
      (error 'empty-queue :name q)
      (common-lisp:first (queue-rear q))))

(defun enqueued-p (item q &key (key #'identity) (test #'eql))
  "Checks whether an item is in the queue or not."
  (not (null (member item (queue-front q) :test test :key key))))


(defun clear-queue (q)
  "Clears the queue Q."
  (unless (empty-p q)
    (setf (queue-front q) ())
    (setf (queue-rear q) ())
    ))

(defun dequeue-items-if (predicate queue)
  "Deletes all items from QUEUE for which PREDICATE is true."
  (setf (queue-front queue) (delete-if predicate (queue-front queue))
	(queue-rear queue) (common-lisp:last (queue-front queue))
	(queue-size queue) (length (queue-front queue)))
  queue
  )


;;; DO-QUEUE

(defmacro do-queue ((qvar q &optional (queue-result nil)) &body forms)
  "Iteration macro for queues.
FORMS are executed once per each item in the queue from the queue
front to the rear, in an enviroment where QVAR is bound to each item
in turn.  If supplied, QUEUE-RESULT is returned at the end of the
iteration."
  `(dolist (,qvar (queue-front ,q) ,queue-result)
     ,@forms))


#| Sample test
(defvar qq (queues:make-queue))
QQ
* (dotimes (i 20) (queues:enqueue (random 100) qq))
NIL
* qq
#<QUEUE [size 20] with first 44 last 67: {705500D}>
* ;;;Evaluate defun pprint-queue
QUEUES::PPRINT-QUEUE
* (queues::pprint-queue qq)
QUEUE : 44 78 71 44 11 26 62 38 2 59 46 19 80 13 63 77 29 52 29 67
NIL
* (dotimes (i 10 (terpri)) (format t "~S " (queues:dequeue qq)))
44 78 71 44 11 26 62 38 2 59 
NIL
* (queues::pprint-queue qq)
QUEUE : 46 19 80 13 63 77 29 52 29 67
NIL
* (dotimes (i 10 (terpri)) (format t "~S " (queues:dequeue qq)))
46 19 80 13 63 77 29 52 29 67 
NIL
* (queues::pprint-queue qq)
QUEUE :
NIL
* (queues:dequeue qq)


Error in function QUEUES:DEQUEUE:  empty queue

|#
;;; end of file -- queues.lisp --
