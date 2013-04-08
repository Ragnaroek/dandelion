;;; -*- Mode: Lisp; Package: CLX-VM;  -*-


;; This file contains the bsd sockets support for CLX VM
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
 "$Header: /cvsroot/clocc/clocc/src/gui/clx/clx-vm/bsd-sockets.lisp,v 1.1 2003/02/24 11:05:32 pvaneynd Exp $")

(in-package :clx-vm)

(defclass bsd-socket (socket)
  ((socket :reader socket-socket
	   :initarg :socket)
   (connected-p :reader socket-connected-p)
   (local-ip :reader socket-local-ip)
   (local-port :reader socket-local-port)
   (remote-ip :reader socket-remote-ip)
   (remote-port :reader socket-remote-port)))

(defun socket-bind (socket ip port)
  "Binds a socket to a given IP and PORT number.
Throws an error on failures, returns nothing."
  )

(defun socket-connect (socket ip port)
  "Connects to a given IP on a PORT.
Throws an error on problems, returns nothing."
  )

(defun socket-listen (socket backlog)
  "Notifies the system you can to listen for new connections on SOCKET, allowing for BACKLOG pending connections.
Throws an error on problems, returns nothing."
  )


(defun socket-receive (socket buffer length &key oob peek (waitall t) (element-type 'character))
  "Recieves LENGTH octets of data from SOCKET, placing it in BUFFER.
if OOB is true, this is Out of band data (like the passwords in a telnet session)
if PEEK is true, the data is not actually removed from the stream
if WAITALL is true, we wait until LENGTH octets have been read.
This function returns the number of octets read."
  )


(defun socket-send (socket buffer length &key oob (waitall t))
  "Sends LENGTH  data in BUFFER on the SOCKET.
if OOB is true, we send it out-of-band
if WAITALL is true, we wait until it all got send
This function retuns the number of octets send")

(defun sockopt-reuse-address  (socket argument)
   Accessor
    Return the value of the SO-REUSEADDR socket option for SOCKET

(defun sockopt-keep-alive (socket socket) argument) Accessor

    Return the value of the SO-KEEPALIVE socket option for SOCKET

(defun sockopt-oob-inline (socket socket) argument) Accessor

    Return the value of the SO-OOBINLINE socket option for SOCKET

(defun sockopt-bsd-compatible (socket socket) argument) Accessor

    Return the value of the SO-BSDCOMPAT socket option for SOCKET

(defun sockopt-pass-credentials (socket socket) argument) Accessor

    Return the value of the SO-PASSCRED socket option for SOCKET

(defun sockopt-debug (socket socket) argument) Accessor

    Return the value of the SO-DEBUG socket option for SOCKET

(defun sockopt-dont-route (socket socket) argument) Accessor

    Return the value of the SO-DONTROUTE socket option for SOCKET

(defun sockopt-broadcast (socket socket) argument) Accessor

    Return the value of the SO-BROADCAST socket option for SOCKET
 
   