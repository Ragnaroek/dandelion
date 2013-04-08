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
 "$Header: /cvsroot/clocc/clocc/src/gui/clx/clx-vm/package.lisp,v 1.1 2003/03/10 09:00:56 pvaneynd Exp $")

(in-package :common-lisp-user)

(defpackage :clx-vm
    (:use :common-lisp)
  (:export #:semaphore
	   #:semaphore-name
	   #:sempahore-taken-p
	   #:make-semaphore
	   #:take-seamphore
	   #:release-semaphore
	   #:with-semaphore-taken
	   #:task-list
	   #:make-tast-list
	   #:add-task
	   #:remove-task
	   #:run-tasks
	   #:queue
	   #:make-queue
	   #:enqueue
	   #:dequeue	   
	   ))


