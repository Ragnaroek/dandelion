;;; -*- Mode: Lisp; Package: CLX-VM;  -*-

;; This file contains the high-level socket support for CLX VM
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
 "$Header: /cvsroot/clocc/clocc/src/gui/clx/clx-vm/sockets.lisp,v 1.1 2003/02/24 11:05:33 pvaneynd Exp $")

(in-package :clx-vm)

;; This defines little more then support for the socket/stream
;; duality.


(defclass socket ()
  ((stream :reader socket-stream
	   :initarg :stream)
   (type   :reader socket-type
	   :initarg :type
	  :type (:stream or :datagram))
   (family :reader socket-family
	   :initarg :family
	   :type (or :ipv4 :ipv6 :file))
   (format :reader socket-format
	   :initarg :format
	   :type (or :binary :text))))
  

(defun socket-close (socket)
  "Closes the socket"
  )


(defun socket-accept (socket &optional (timeout 0))
  "Accepts incomming connections on SOCKET, waits TIMEOUT seconds for a connection.
Throws an error on failure and on timeout."
  )


;; Socket errors:


(define-condition Socket-Error ()
  )

(define-condition Socket-address-in-use (Socket-Error))
(define-condition Socket-address-not-available (Socket-Error))
(define-condition Socket-network-down (Socket-Error))
(define-condition Socket-network-reset (Socket-Error))
(define-condition Socket-connection-aborted (Socket-Error))
(define-condition Socket-connection-reset (Socket-Error))
(define-condition Socket-no-buffer-space (Socket-Error))
(define-condition Socket-closed (Socket-Error))
(define-condition Socket-connection-timed-out (Socket-Error))
(define-condition Socket-connection-refused (Socket-Error))
(define-condition Socket-host-down (Socket-Error))
(define-condition Socket-host-unreachable (Socket-Error))






