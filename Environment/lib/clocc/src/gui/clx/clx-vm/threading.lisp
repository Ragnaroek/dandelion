;;; -*- Mode: Lisp; Package: CLX-VM;  -*-


;; This file contains the MP support for CLX VM
;; copyright 2003, Peter Van Eynde (BSD like license)
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.
;; 3. The name of the author may not be used to endorse or promote
;;    products derived from this software without specific prior
;;    written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
#+cmu
(ext:file-comment
 "$Header: /cvsroot/clocc/clocc/src/gui/clx/clx-vm/threading.lisp,v 1.2 2003/03/10 09:00:56 pvaneynd Exp $")

(in-package :clx-vm)

;; The basic idea is very simple:
;; we don't do without-scheduling anymore...
;; we should support the non-MP cmucl system serve-event in a
;; portable way
;;
;; so: what do we need:
;;
;; There are two ways to interact with the X messages:
;;
;; - You can be a clx-event-handler. You are not allowed to read the queue
;;   yourself. You should be a short and quick function, as you stop
;;   the reader process from running. You are running with ignore-errors
;;   and outside the original thread (so don't count on special variables
;;   to keep on working). All events are given to the queue of clx-event-handlers
;;   until one returns true (event handled). Then it is given to all read-input
;;   blocked threads.
;;
;; - You called read-input. Your thread blocks until an event is read. All events
;;   are presented to you, also those are are handled by a clx-event-handler. You
;;   continue to run after the block in your own thread without extra wrappers.
;;
;; There are two views of the world:
;; - lisps with MP:
;;    * we have a writer thread that takes messages
;;      from a queue and sends them. If errors can be
;;      detected we signal then the next time that thread
;;      tries to send a new message.
;;    * we have a reader thread that keeps reading for
;;      messages. When it has one it calls all the clx-event-handlers
;;      and then wakes up all read-input blocked threads and gives then
;;      the event.
;;    * read-input adds the semaphore to the read-input-semaphores and then
;;      blocks on it. When it detects that the semaphore is already in
;;      use (we are a new thread) a new semaphore is created.
;;  - lisps without MP:
;;    * we write blockingly
;;    * we read with the native add-clx-event-handler support, first
;;      handling the event to the event handlers we emulate, then
;;      returning the event to the blocked read-input.

;; So we need:
;;
;; [a] something that keeps a list of things to call. We can add an item,
;;     remove an item and call all of the items with a parameter.
;;     This we call a "task-list", used for the event-handlers.
;; [b] something to put messages in, and out. This we call a "queue".
;;     This is used in the writer thread and the read-input blocked threads.
;; and in general:
;; [c] semaphores to protect shared datastructures.


;;; MP low level handling

(defconstant +CLX-real-MP+
  (or
   #+(or (and :CMU :MP) Allegro LispWorks MCL) T
   NIL)
  "Do we have real MP or we just have an eventhandling system?")

(defvar *CLX-MP-initialized*
  NIL
  "T if the MP system is active")

;;
;; The base of almost all further work is the
;;; semaphore:

(defclass semaphore ()
  (#-(or (and :CMU :MP))
   (name :reader semaphore-name
	 :type 'string
	 :initarg :name)
   #-(or (and :CMU :MP) Allegro)
   (taken-p :reader semaphore-taken-p
	    :type boolean
	    :initform nil)
   (%real  :reader %semaphore-real
	   :initarg :real)))

#+(or (and :CMU :MP))
(defmethod semaphore-name ((semaphore semaphore))
  #+(and :CMU :MP)
  (mp::lock-name (%semaphore-real
		  semaphore)))

#+(or (and :CMU :MP) Allegro)
(defmethod semaphore-taken-p ((semaphore semaphore))
    #+Allegro
  (if (mp:process-lock-locker (%semaphore-real
			       semaphore))
      t
      nil)
  #+(and :CMU :MP)
  (if (typep (mp::lock-process (%semaphore-real
				semaphore))
	     'mp::process)
      t
      nil))
   
(defun %make-real-lock (name)
  (or
   #+Allegro
   (mp:make-process-lock :name name)
   #+(and :CMU :MP)
   (mp:make-lock name)
   #+LispWorks
   (mp:make-lock :name name)
   #+MCL
   (ccl:make-process-queue name)
   NIL))

(defun make-semaphore (name)
  "Makes a semaphore with the given NAME.
Returns the new semaphore"

  (let ((real (%make-real-lock name)))
    (make-instance 'semaphore
		   #-(or (and :CMU :MP)) 
		   :name  #-(or (and :CMU :MP))  name
		   :real real)))

(define-condition Semaphore-Timeout ()
  ((semaphore :reader semaphore-timeout-semaphore
	  :initarg :semaphore))
  (:report
   (lambda (condition stream)
     (format stream "Got a timeout while trying to take the semaphore ~A"
	     (semaphore-timeout-semaphore condition)))))

(defun take-semaphore (semaphore &optional
			 (timeout 0 timeout-p))
  "Tries to take the SEMAPHORE, waiting for the semaphore TIMEOUT seconds.
Returns the semaphore on success, trows an error on failure.."
  (or
   (and
    #+Allegro
    (if timeout-p
	(mp:process-lock (%semaphore-real semaphore)
			 :timeout timeout)
	(mp:process-lock (%semaphore-real semaphore)))
    #+(and :CMU :MP)
    (if timeout-p
	(mp::lock-wait-with-timeout (%semaphore-real semaphore)
				    "Lock Wait"
				    timeout)
	(mp::lock-wait (%semaphore-real semaphore)
		       "Lock Wait"))
    #+LispWorks
    (if timeout-p
	(lcl:process-lock (%semaphore-real semaphore "Lock Wait" timeout))
	(lcl:process-lock (%semaphore-real semaphore)))
    #+MCL
    (if timeout-p
	(lcl:process-enqueue (%semaphore-real semaphore))
	(lcl:process-enqueue (%semaphore-real semaphore)))
    #-(or Allegro (and :CMU :MP) LispWorks MCL)
    (if (%semaphore-real semaphore)
	(error "Semaphore already locked and no real MP!?!")
	(setf (slot-value semaphore 'real) t
	      (slot-value semaphore 'taken-p) t))
    semaphore)
   (error 'Sempahore-Timeout
	  :semaphore semaphore)))

(defun release-semaphore (semaphore)
  "Releases the SEMAPHORE. Returns nothing."

  #+Allegro
  (mp:process-unlock (%semaphore-real semaphore))
  #+(and :CMU :MP)
  (setf (mp::lock-process (%semaphore-real semaphore))
	nil)
  #+LispWorks
  (mp:release-lock lock)
  #+MCL
  (ccl:process-dequeue lock)
  #-(or Allegro (and :CMU :MP) LispWorks MCL)
  (if (%semaphore-real semaphore)
      (setf (slot-value semaphore 'real) nil	    
	    (slot-value semaphore 'taken-p) nil)
      (error "Semaphore NOT locked!"))
  (values))
 
(defmacro with-semaphore-taken ((semaphore &optional
				   (timeout 0 timeout-p))
			    &body body)
  "Evaluates BODY with SEMAPHORE held.
Will put PUT-VALUE into the value field during the execution, setting it to
NIL on exit. Will wait TIMEOUT time for the lock, throwing an
error of type SEMAPHORE-TIMEOUT on timeout."
  #+Allegro
  (if timeout-p
      `(mp:with-process-lock ((%semaphore-real ,semaphore) :timeout ,timeout) ,@body)
      `(mp:with-process-lock ((%semaphore-real ,semaphore)) ,@body))
  #+(and :CMU :MP)
  (if timeout-p
      `(mp:with-lock-held ((%semaphore-real ,semaphore) "lock wait" :wait t :timeout ,timeout) ,@body)
      `(mp:with-lock-held ((%semaphore-real ,semaphore) "lock wait" :wait t) ,@body))
  #+LispWorks
  (if timeout-p
      `(mp:with-lock ((%semaphore-real ,semaphore) "Lock Wait" timeout) ,@body)
      `(mp:with-lock ((%semaphore-real ,semaphore)) ,@body))
  #+MCL
  `(ccl:with-process-enqueued ((%semaphore-real ,semaphore)) ,@body)
  #-(or Allegro (and :CMU :MP) LispWorks MCL)
  `(progn
    (setf (slot-value semaphore 'value) t	    
     (slot-value semaphore 'taken-p) t)
    (unwind-protect
	 (progn ,@body)
      (setf (slot-value semaphore 'value) nil	    
	    (slot-value semaphore 'taken-p) nil))))

;;; Task-List

;; the task-list is a semaphore, which is locked on modification...
;; we make no guarantees that all tasks are called if the
;; list is modified during execution of a run

(defclass task-list (semaphore)
  ((tasks :initform nil)))

(defun make-task-list ()
  (let ((real (%make-real-lock "task list lock")))
    (make-instance 'task-list
		   #-(or (and :CMU :MP)) 
		   :name  #-(or (and :CMU :MP))  "task list lock"
		   :real real)))

(defun add-task (task-list task signature)
  "Adds the TASK to the TASK-LIST.
Locks the task-list for modifications"
  (with-semaphore-taken (task-list)
    (pushnew (list signature task)
	     (slot-value task-list 'tasks)
	     :key #'first))
  (values))

(defun remove-task (task-list signature)
  "Removed the TASK from the TASK-LIST"
  (with-semaphore-taken (task-list)
    (setf (slot-value task-list 'tasks)
	  (remove signature
		  (slot-value task-list 'tasks)
		  :key #'first)))
  (values))

(defun run-tasks (task-list &rest args)
  (mapc (lambda (task)
	  (apply (second task) args))
	(slot-value task-list 'tasks))
  (values))

;;; queue
;; This is a general queue that will block when there
;; is nothing in the queue

(defclass queue (semaphore)
  ((head :initform nil)
   (tail :initform nil)
   (wait-lock :initarg :wait-lock
	      :reader queue-wait-lock)))

(defun make-queue ()
  (let ((modification-lock (%make-real-lock "Queue lock"))
	(wait-lock (make-semaphore "Queue Wait lock")))
    (make-instance 'queue
		   #-(or (and :CMU :MP)) 
		   :name  #-(or (and :CMU :MP))  "Queue lock"
		   :real modification-lock
		   :wait-lock wait-lock)))

(defun enqueue (queue value)
  "Puts the VALUE in the FIFO queue"
  (with-semaphore-taken (queue)
    (with-slots (head tail)
	queue
      (cond
	((null tail)
	 ;; queue is empty
	 (unless (null head)
	   (error "The queue is in an inconsistant state!"))
	 (setf head
	       (cons value nil))
	 (setf tail head))
	(t
	 (let ((new-tail (cons value nil)))
	   (setf (cdr tail) new-tail)
	   (setf tail new-tail))))))
  ;; signal we put something in
  (release-semaphore (queue-wait-lock queue))
  (values))

(defun dequeue (queue &optional (wait t)) 
  "Pops the queue if there is something to pop, otherwise when WAIT is
true sleep until something appears. If WAIT is false, return NIL."
  (with-slots (head tail)
      queue
    ;; we have to be careful here...
    (take-semaphore queue)
    ;; so noone modifies the queue
    (cond
      ((and (null head)
	    wait)
       ;; there is nothing in it!
       ;; so take the wait lock twice and wait for the unlock
       (take-semaphore (queue-wait-lock queue))
       ;; now the others can modify:
       (release-semaphore queue)
       ;; wait for a enqueue:
       (take-semaphore (queue-wait-lock queue))
       ;; we got unlocked.. so release it again
       (release-semaphore (queue-wait-lock queue))
       ;; now retry:
       (dequeue queue))
      ((null head)
       ;; and no WAIT
       NIL)
      (t
       (let ((result (car head))
	     (new-head (cdr head)))
	 (if (null new-head)
	   ;; the end of the queue:
	   (setf head nil
		 tail nil)
	   ;; otherwise normal
	   (setf head new-head))
	 (release-semaphore queue)
	 result)))))

#|
;;; CLX Event handling
;;
;; We use an abstraction of MP to describe event handing in X:
;; the application can push an event handler on a queue, when an
;; X event arrives all the handlers on the queue are called in order
;; until one returns true. The handlers are called in a wrapper
;; that flushes all errors.
;;
;; We use a queue so the programmer doesn't have to worry if s/he
;; needs to restore a handler another piece of the program installed
;; before.

(defun push-clx-event-handler (display handler)
  "This function pushes HANDLER on the wake up queue for activity from DISPLAY.
The system will call the HANDLERs in the queue when there is input from the
X windows stream related to DISPLAY. If a handler returns true
the event is considered handled.

The handler is called with the object of the event ??? XXX 

All errors during the execution of HANDLER will be flushed.

This function returns nothing."
  (values))

(defun pop-clx-event-handling (display handler) 
  "This function remove HANDLER from the event queue  for DISPLAY.
Is this HANDLER was not installed nothing happens.

This function returns nothing."
  (values))

(defun with-clx-event-handling ((display handler) &body body)
  "This macro evaluates BODY in a block where the HANDLER gets woken up with activity on DISPLAY.
It is just an easy wrapper of enable-clx-event-handling."
  (let ((handler-symbol (make-symbol "clx-event-handler")))
    `(let ((,handler-symbol ,handler))
      (unwind-protect
	   (progn
	     (push-clx-event-handling ,display ,handler-symbol)
	     ,@body)
	(pop-clx-event-handling ,display ,handler-symbol)))))

|#




				