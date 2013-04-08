;;; -*- Mode: Lisp; Package: CLX-VM;  -*-


;; This file contains the MP support tests for CLX VM
;;
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
 "$Header: /cvsroot/clocc/clocc/src/gui/clx/clx-vm/threading-tests.lisp,v 1.1 2003/03/10 09:00:57 pvaneynd Exp $")

(in-package :clx-vm)


(require :clunit)


;;; semaphore

(org.ancar.CLUnit:deftest "Creating of semaphores"
    :test-fn
  (lambda ()
    (let ((semaphore (make-semaphore "test")))
      (typep semaphore
	     'semaphore))))

(org.ancar.CLUnit:deftest "Names of semaphores"
    :test-fn
  (lambda ()
    (let ((semaphore (make-semaphore "test")))
      (string= (semaphore-name semaphore)
	       "test"))))

(org.ancar.CLUnit:deftest "Default state of semaphores"
    :test-fn
  (lambda ()
    (let ((semaphore (make-semaphore "test")))
      (eq (semaphore-taken-p semaphore)
	  nil))))

(org.ancar.CLUnit:deftest "Locking and releasing of a free semaphore"
    :test-fn
  (lambda ()
    (let ((semaphore (make-semaphore "test")))
      (take-semaphore semaphore)
      (prog1
	  (semaphore-taken-p semaphore)
	(release-semaphore semaphore)))))

(org.ancar.CLUnit:deftest "With-semaphore-taken macro"
    :test-fn
  (lambda ()
    (let ((semaphore (make-semaphore "test")))
      (with-semaphore-taken (semaphore)
	(semaphore-taken-p semaphore)))))

;; task-list

(org.ancar.CLUnit:deftest "Creating of task-list"
    :test-fn
  (lambda ()
    (let ((task-list (make-task-list)))
      (typep task-list
	     'task-list))))

(org.ancar.CLUnit:deftest "A task list is also a semaphore"
    :test-fn
  (lambda ()
    (let ((task-list (make-task-list)))
      (typep task-list
	     'semaphore))))

(org.ancar.CLUnit:deftest "Add task"
    :test-fn
  (lambda ()
    (let* ((task-list (make-task-list))
	   (trigger nil)
	   (task (lambda  () (setf trigger t))))
      (add-task task-list task :first)
      t)))

(org.ancar.CLUnit:deftest "Add task,remove task"
    :test-fn
  (lambda ()
    (let* ((task-list (make-task-list))
	   (trigger nil)
	   (task (lambda  () (setf trigger t))))
      (add-task task-list task :first)
      (remove-task task-list :first)
      t)))

(org.ancar.CLUnit:deftest "Add task, fire task"
    :test-fn
  (lambda ()
    (let* ((task-list (make-task-list))
	   (trigger nil)
	   (task (lambda  () (setf trigger t))))
      (add-task task-list task :first)
      (run-tasks task-list)
      trigger)))

(org.ancar.CLUnit:deftest "Add task, remove task, fire task"
    :test-fn
  (lambda ()
    (let* ((task-list (make-task-list))
	   (trigger nil)
	   (task (lambda  () (setf trigger t))))
      (add-task task-list task :first)
      (remove-task task-list :first)
      (run-tasks task-list)
      (eq trigger nil))))

(org.ancar.CLUnit:deftest "fire task"
    :test-fn
  (lambda ()
    (let ((task-list (make-task-list)))
      (run-tasks task-list)
      t)))

(org.ancar.CLUnit:deftest "Add tasks, fire tasks"
    :test-fn
  (lambda ()
    (let* ((task-list (make-task-list))
	   (counter 0)
	   (task1 (lambda  () (incf counter)))
	   (task2 (lambda  () (incf counter 10))))
      (add-task task-list task1 :first)
      (add-task task-list task2 :second)
      (run-tasks task-list)
      (= counter 11))))

(org.ancar.CLUnit:deftest "Add tasks, remove first fire tasks"
    :test-fn
  (lambda ()
    (let* ((task-list (make-task-list))
	   (counter 0)
	   (task1 (lambda  () (incf counter)))
	   (task2 (lambda  () (incf counter 10))))
      (add-task task-list task1 :first)
      (add-task task-list task2 :second)
      (remove-task task-list :first)
      (run-tasks task-list)
      (= counter 10))))

(org.ancar.CLUnit:deftest "Add tasks, fire tasks"
    :test-fn
  (lambda ()
    (let* ((task-list (make-task-list))
	   (counter 0)
	   (task1 (lambda  () (incf counter)))
	   (task2 (lambda  () (incf counter 10))))
      (add-task task-list task1 :first)
      (add-task task-list task2 :second)
      (remove-task task-list :second)
      (run-tasks task-list)
      (= counter 1))))

;; queue

(org.ancar.CLUnit:deftest "Creating of queue"
    :test-fn
  (lambda ()
    (let ((queue (make-queue)))
      (typep queue
	     'queue))))

(org.ancar.CLUnit:deftest "adding to queue"
    :test-fn
  (lambda ()
    (let ((queue (make-queue)))
      (enqueue queue 1)
      t)))

(org.ancar.CLUnit:deftest "adding to queue, pop queue"
    :test-fn
  (lambda ()
    (let ((queue (make-queue)))
      (enqueue queue 1)
      (= 1
	 (dequeue queue nil)))))

(org.ancar.CLUnit:deftest "add, add to queue, pop, pop queue"
    :test-fn
  (lambda ()
    (let ((queue (make-queue)))
      (enqueue queue 1)
      (enqueue queue 2)
      (and (= 1
	      (dequeue queue nil))
	   (= 2
	      (dequeue queue nil))))))

(org.ancar.CLUnit:deftest "add, add to queue, pop, pop, pop (no wait) queue"
    :test-fn
  (lambda ()
    (let ((queue (make-queue)))
      (enqueue queue 1)
      (enqueue queue 2)
      (and (= 1
	      (dequeue queue nil))
	   (= 2
	      (dequeue queue nil))
	   (eq NIL
	       (dequeue queue nil))))))

(org.ancar.CLUnit:deftest "add, add to queue, pop, add , pop, pop queue"
    :test-fn
  (lambda ()
    (let ((queue (make-queue)))
      (enqueue queue 1)
      (enqueue queue 2)
      (and (= 1
	      (dequeue queue nil))
	   (progn
	     (enqueue queue 3)
	     (= 2
		(dequeue queue nil)))
	   (= 3
	      (dequeue queue nil))))))

(org.ancar.CLUnit:run-all-tests)