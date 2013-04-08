;;; Network Access
;;;
;;; Copyright (C) 1999-2006 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: net.lisp,v 1.61 2006/06/22 20:33:25 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/net.lisp,v $

(eval-when (compile load eval)
  (require :port-ext (translate-logical-pathname "clocc:src;port;ext"))
  ;; `getenv'
  (require :port-sys (translate-logical-pathname "port:sys"))
  #+(or cmu scl) (require :simple-streams) ; for `set-socket-stream-format'
  #+cormanlisp (require :winsock)
  #+lispworks (require "comm")
  #+(and sbcl (not (or db-sockets net.sbcl.sockets)))
  (progn (require :sb-bsd-sockets) (pushnew :sb-bsd-sockets *features*)))

(in-package :port)

(export
 '(resolve-host-ipaddr ipaddr-to-dotted dotted-to-ipaddr
   hostent hostent-name hostent-aliases hostent-addr-list hostent-addr-type
   socket open-socket socket-host/port socket-string socket-server
   set-socket-stream-format
   socket-accept open-socket-server socket-server-close socket-server-host/port
   socket-service-port servent-name servent-aliases servent-port servent-proto
   servent-p servent network timeout login net-path))

;;;
;;; {{{ name resolution
;;;

(declaim (ftype (function ((unsigned-byte 32)) (values simple-string))
                ipaddr-to-dotted))
(defun ipaddr-to-dotted (ipaddr)
  "Number --> string."
  (declare (type (unsigned-byte 32) ipaddr))
  #+allegro (socket:ipaddr-to-dotted ipaddr)
  #+openmcl (ccl:ipaddr-to-dotted ipaddr)
  #+(and sbcl net.sbcl.sockets) (net.sbcl.sockets:ipaddr-to-dot-string ipaddr)
  #-(or allegro openmcl (and sbcl net.sbcl.sockets))
  (format nil "~d.~d.~d.~d"
          (logand #xff (ash ipaddr -24)) (logand #xff (ash ipaddr -16))
          (logand #xff (ash ipaddr -8)) (logand #xff ipaddr)))

(declaim (ftype (function (string) (values (unsigned-byte 32)))
                dotted-to-ipaddr))
(defun dotted-to-ipaddr (dotted)
  "String --> number."
  (declare (string dotted))
  #+allegro (socket:dotted-to-ipaddr dotted)
  #+openmcl (ccl:dotted-to-ipaddr dotted)
  #+(and sbcl net.sbcl.sockets) (net.sbcl.sockets:dot-string-to-ipaddr dotted)
  #-(or allegro openmcl (and sbcl net.sbcl.sockets))
  (let ((ll (string-tokens (substitute #\Space #\. dotted))))
    (+ (ash (first ll) 24) (ash (second ll) 16)
       (ash (third ll) 8) (fourth ll))))

;#+(and sbcl (or db-sockets sb-bsd-sockets))
;(declaim (ftype (function (vector) (values (unsigned-byte 32)))
;                vector-to-ipaddr))
#+(and sbcl (or db-sockets sb-bsd-sockets))
(defun vector-to-ipaddr (vector)
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

;#+(and sbcl (or db-sockets sb-bsd-sockets))
;(declaim (ftype (function (vector) (values (unsigned-byte 32)))
;                ipaddr-to-vector))
#+(and sbcl (or db-sockets sb-bsd-sockets))
(defun ipaddr-to-vector (ipaddr)
  (vector (ldb (byte 8 24) ipaddr)
          (ldb (byte 8 16) ipaddr)
          (ldb (byte 8 8) ipaddr)
          (ldb (byte 8 0) ipaddr)))

(defstruct hostent
  "see gethostbyname(3) for details"
  (name "" :type simple-string) ; canonical name of host
  (aliases nil :type list)      ; alias list
  (addr-list nil :type list)    ; list of addresses
  (addr-type 2 :type fixnum))   ; host address type

(defun resolve-host-ipaddr (host)
  "Call gethostbyname(3) or gethostbyaddr(3)."
  #+allegro
  (let* ((ipaddr
          (etypecase host
            (string
             (if (every (lambda (ch) (or (char= ch #\.) (digit-char-p ch)))
                        host)
                 (socket:dotted-to-ipaddr host)
                 (socket:lookup-hostname host)))
            (integer host)))
         (name (socket:ipaddr-to-hostname ipaddr)))
    (make-hostent :name name :addr-list
                  (list (socket:ipaddr-to-dotted ipaddr))))
  #+(and clisp syscalls)
  (let ((he (posix:resolve-host-ipaddr host)))
    (make-hostent :name (posix::hostent-name he)
                  :aliases (posix::hostent-aliases he)
                  :addr-list (posix::hostent-addr-list he)
                  :addr-type (posix::hostent-addrtype he)))
  #+(or cmu scl)
  (let ((he (ext:lookup-host-entry host)))
    (make-hostent :name (ext:host-entry-name he)
                  :aliases (ext:host-entry-aliases he)
                  :addr-list (mapcar #'ipaddr-to-dotted
                                     (ext:host-entry-addr-list he))
                  :addr-type (ext::host-entry-addr-type he)))
  #+gcl (make-hostent :name (or (si:hostid-to-hostname host) host)
                      :addr-list (list (si:hostname-to-hostid host)))
  #+lispworks
  (multiple-value-bind (name addr aliases)
      (comm:get-host-entry host :fields '(:name :address :aliases))
    (make-hostent :name name :addr-list (list (ipaddr-to-dotted addr))
                  :aliases aliases))
  #+openmcl
  (let* ((ipaddr
          (etypecase host
            (string
             (if (every (lambda (ch) (or (char= ch #\.) (digit-char-p ch)))
                        host)
                 (dotted-to-ipaddr host)
                 (ccl:lookup-hostname host)))
            (integer host)))
         (name (ccl:ipaddr-to-hostname ipaddr)))
    (make-hostent :name name :addr-list (list (ccl:lookup-hostname ipaddr))))
  #+(and sbcl db-sockets)
  (let* ((ipaddr
          (etypecase host
            (string
             (if (every (lambda (ch) (or (char= ch #\.) (digit-char-p ch)))
                        host)
                 (dotted-to-ipaddr host)
                 (let ((hostent
                        (sockets:get-host-by-name host)))
                   (when hostent
                     (vector-to-ipaddr
                      (sockets::host-ent-address hostent))))))
            (integer host)))
         (name
          (when ipaddr
            (let ((hostent
                   (sockets:get-host-by-address
                    (ipaddr-to-vector ipaddr))))
              (when (and hostent
                         (sockets::host-ent-aliases hostent))
                (first (sockets::host-ent-aliases hostent)))))))
    (make-hostent :name name :addr-list (list ipaddr)))
  #+(and sbcl net.sbcl.sockets)
  (let ((he (net.sbcl.sockets:lookup-host-entry host)))
    (make-hostent :name (net.sbcl.sockets:host-entry-name he)
                  :aliases (net.sbcl.sockets:host-entry-aliases he)
                  :addr-list (mapcar #'ipaddr-to-dotted
                                     (net.sbcl.sockets:host-entry-addr-list he))
                  :addr-type (net.sbcl.sockets::host-entry-addr-type he)))
  #-(or allegro (and clisp syscalls) cmu gcl lispworks openmcl
        (and sbcl (or db-sockets net.sbcl.sockets)) scl)
  (error 'not-implemented :proc (list 'resolve-host-ipaddr host)))

;;;
;;; }}}{{{ sockets
;;;

(deftype socket ()
  #+abcl 'to-way-stream
  #+allegro 'excl::socket-stream
  #+clisp 'stream
  #+(or cmu scl) 'stream ; '(or stream:socket-simple-stream sys:fd-stream)
  #+gcl 'stream
  #+lispworks 'comm:socket-stream
  #+openmcl 'ccl::socket
  #+(and sbcl (or db-sockets sb-bsd-sockets)) 'sb-sys:fd-stream
  #+(and sbcl net.sbcl.sockets) 'net.sbcl.sockets:stream-socket
  #-(or abcl allegro clisp cmu gcl lispworks openmcl
        (and sbcl (or db-sockets net.sbcl.sockets sb-bsd-sockets)) scl) 'stream)

(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (type (or integer string) host) (fixnum port)
           #+(or cmu scl) (ignore bin))
  (let ((host (etypecase host
                (string host)
                (integer (hostent-name (resolve-host-ipaddr host))))))
    #+abcl (ext:get-socket-stream
            (sys:make-socket host port)
            :element-type (if bin '(unsigned-byte 8) 'character))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (#+lisp=cl ext:socket-connect #-lisp=cl lisp:socket-connect
                       port host :element-type
                       (if bin '(unsigned-byte 8) 'character))
    #+(or cmu scl)
    (make-instance 'stream:socket-simple-stream :direction :io
                   :remote-host host :remote-port port)
    #+gcl (si:socket port :host host)
    #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
                                      (if bin 'unsigned-byte 'base-char))
    #+mcl (ccl:make-socket :remote-host host :remote-port port
                           :format (if bin :binary :text))
    #+(and sbcl db-sockets)
    (let ((socket (make-instance 'sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (sockets:socket-connect socket
                              (sockets::host-ent-address
                               (sockets:get-host-by-name host))
                              port)
      (sockets:socket-make-stream
       socket :input t :output t :buffering (if bin :none :line)
       :element-type (if bin '(unsigned-byte 8) 'character)))
    #+(and sbcl net.sbcl.sockets)
    (net.sbcl.sockets:make-socket
     (if bin
         'net.sbcl.sockets:binary-stream-socket
         'net.sbcl.sockets:character-stream-socket)
     :port port :host host)
    #+(and sbcl sb-bsd-sockets)
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (sb-bsd-sockets:socket-connect socket
				     (sb-bsd-sockets::host-ent-address
				      (sb-bsd-sockets:get-host-by-name host))
				     port)
      (sb-bsd-sockets:socket-make-stream
       socket :input t :output t :buffering (if bin :none :line)
       :element-type (if bin '(unsigned-byte 8) 'character)))
    #-(or abcl allegro clisp cmu gcl lispworks mcl
          (and sbcl (or net.sbcl.sockets db-sockets sb-bsd-sockets)) scl)
    (error 'not-implemented :proc (list 'open-socket host port bin))))

(defun set-socket-stream-format (socket format)
  "switch between binary and text output"
  #+clisp (setf (stream-element-type socket) format)
  #+(or acl cmu lispworks scl)
  (declare (ignore socket format)) ; bivalent streams
  #-(or acl clisp cmu lispworks scl)
  (error 'not-implemented :proc (list 'set-socket-stream-format socket format)))

#+(and sbcl sb-bsd-sockets)
(defun funcall-on-sock (function sock)
  "Apply function (getsockname/getpeername) on socket, return host/port as two values"
  (let ((sockaddr (sockint::allocate-sockaddr-in)))
    (funcall function (sb-sys:fd-stream-fd sock) sockaddr sockint::size-of-sockaddr-in)
    (let ((host (coerce (loop :for i :from 0 :below 4
                          :collect (sb-alien:deref (sockint::sockaddr-in-addr sockaddr) i))
			'(vector (unsigned-byte 8) 4)))
	  (port (+ (* 256 (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 0))
		   (sb-alien:deref (sockint::sockaddr-in-port sockaddr) 1))))
      (sockint::free-sockaddr-in sockaddr)
      (values host port))))

(defun socket-host/port (sock)
  "Return the remote and local host&port, as 4 values."
  (declare (type socket sock))
  #+allegro (values (socket:ipaddr-to-dotted (socket:remote-host sock))
                    (socket:remote-port sock)
                    (socket:ipaddr-to-dotted (socket:local-host sock))
                    (socket:local-port sock))
  #+clisp (flet ((ip (ho) (subseq ho 0 (position #\Space ho :test #'char=))))
            (multiple-value-bind (ho1 po1)
                (#+lisp=cl  ext:socket-stream-peer
                 #-lisp=cl lisp:socket-stream-peer sock)
              (multiple-value-bind (ho2 po2)
                  (#+lisp=cl  ext:socket-stream-local
                   #-lisp=cl lisp:socket-stream-local sock)
                (values (ip ho1) po1
                        (ip ho2) po2))))
  #+(or cmu scl)
  (let ((fd (sys:fd-stream-fd sock)))
    (multiple-value-bind (ho1 po1) (ext:get-peer-host-and-port fd)
      (multiple-value-bind (ho2 po2) (ext:get-socket-host-and-port fd)
        (values (ipaddr-to-dotted ho1) po1
                (ipaddr-to-dotted ho2) po2))))
  #+gcl (let ((peer (si:getpeername sock))
              (loc (si:getsockname sock)))
          (values (car peer) (caddr peer)
                  (car loc) (caddr loc)))
  #+lispworks
  (multiple-value-bind (ho1 po1) (comm:socket-stream-peer-address sock)
    (multiple-value-bind (ho2 po2) (comm:socket-stream-address sock)
      (values (ipaddr-to-dotted ho1) po1
              (ipaddr-to-dotted ho2) po2)))
  #+mcl
  (values (ccl:ipaddr-to-dotted (ccl:remote-host sock))
          (ccl:remote-port sock)
          (ccl:ipaddr-to-dotted (ccl:local-host sock))
          (ccl:local-port sock))
  #+(and sbcl db-sockets)
  (let ((sock (sb-sys:fd-stream-fd sock)))
    (multiple-value-bind (remote remote-port) (sockets:socket-peername sock)
      (multiple-value-bind (local local-port) (sockets:socket-name sock)
        (values (ipaddr-to-dotted (vector-to-ipaddr remote))
                remote-port
                (ipaddr-to-dotted (vector-to-ipaddr local))
                local-port))))
  #+(and sbcl net.sbcl.sockets)
  (net.sbcl.sockets:socket-host-port sock)
  #+(and sbcl sb-bsd-sockets)
  (multiple-value-bind (remote remote-port)
      (funcall-on-sock #'sockint::getpeername sock)
    (multiple-value-bind (local local-port)
	(funcall-on-sock #'sockint::getsockname sock)
      (values (ipaddr-to-dotted (vector-to-ipaddr remote))
	      remote-port
	      (ipaddr-to-dotted (vector-to-ipaddr local))
	      local-port)))
  #-(or allegro clisp cmu gcl lispworks mcl
        (and sbcl (or net.sbcl.sockets db-sockets sb-bsd-sockets)) scl)
  (error 'not-implemented :proc (list 'socket-host/port sock)))

(defun socket-string (sock)
  "Print the socket local&peer host&port to a string."
  (declare (type socket sock))
  (with-output-to-string (stream)
    (print-unreadable-object (sock stream :type t :identity t)
      (multiple-value-bind (ho1 po1 ho2 po2) (socket-host/port sock)
        (format stream "[local: ~a:~d] [peer: ~s:~d]" ho2 po2 ho1 po1)))))

;;;
;;; }}}{{{ socket-servers
;;;

#+lispworks (defstruct socket-server proc mbox port)
#-lispworks
(deftype socket-server ()
  #+abcl 'ext:javaobject
  #+allegro 'acl-socket::socket-stream-internet-passive
  #+(and clisp      lisp=cl)   'ext:socket-server
  #+(and clisp (not lisp=cl)) 'lisp:socket-server
  #+(or cmu scl) 'integer
  #+gcl 'si:socket-stream
  #+mcl 'ccl::listener-socket
  #+(and sbcl db-sockets) 'sb-sys:fd-stream
  #+(and sbcl net.sbcl.sockets) 'net.sbcl.sockets:passive-socket
  #+(and sbcl sb-bsd-sockets) 'sb-bsd-sockets:inet-socket
  #-(or abcl allegro clisp cmu gcl mcl
        (and sbcl (or net.sbcl.sockets db-sockets)) scl) t)

(defun open-socket-server (&optional port)
  "Open a `generic' socket server."
  (declare (type (or null integer #-sbcl socket) port))
  #+abcl (ext:make-server-socket port)
  #+allegro (socket:make-socket :connect :passive :local-port
                                (when (integerp port) port))
  #+clisp (#+lisp=cl ext:socket-server #-lisp=cl lisp:socket-server port)
  #+(or cmu scl) (ext:create-inet-listener (or port 0))
  #+gcl (si:make-socket-pair port) ; FIXME
  #+lispworks (let ((mbox (mp:make-mailbox :size 1)))
                (make-socket-server
                 :mbox mbox :port port
                 :proc (comm:start-up-server
                        :function (lambda (sock) (mp:mailbox-send mbox sock))
                        :service port)))
  #+mcl
  (ccl:make-socket :connect :passive
                   :type :stream
                   :reuse-address t
                   :local-port (or port 0))
  #+(and sbcl db-sockets)
  (let ((socket (make-instance 'sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (sockets:socket-bind socket (vector 0 0 0 0) (or port 0)))
  #+(and sbcl net.sbcl.sockets)
  (net.sbcl.sockets:make-socket 'net.sbcl.sockets:passive-socket :port port)
  #+(and sbcl sb-bsd-sockets)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                             :type :stream
                             :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock (vector 0 0 0 0) (or port 0))
    (sb-bsd-sockets:socket-listen sock 15)
    sock)
  #-(or abcl allegro clisp cmu gcl lispworks mcl
        (and sbcl (or net.sbcl.sockets db-sockets sb-bsd-sockets)) scl)
  (error 'not-implemented :proc (list 'open-socket-server port)))

(defun socket-accept (serv &key bin wait)
  "Accept a connection on a socket server (passive socket).
Keyword arguments are:
 BIN - create a binary stream;
 WAIT - wait for the connection this many seconds
        (the default is NIL - wait forever).
Returns a socket stream or NIL."
  (declare (type socket-server serv)
           #+(or (and allegro (version>= 6)) openmcl)
           (ignore bin))
  #+abcl (ext:get-socket-stream
          (ext:socket-accept serv)
          :element-type (if bin '(unsigned-byte 8) 'character))
  #+allegro (let* ((fmt (if bin :binary :text))
                   #+allegro-v5.0
                   (excl:*default-external-format* fmt)
                   (sock (if wait
                             (if (plusp wait)
                                 (mp:with-timeout (wait)
                                   (socket:accept-connection serv :wait t))
                                 (socket:accept-connection serv :wait nil))
                             (socket:accept-connection serv :wait t))))
              (when sock
                ;; From: John Foderaro <jkf@franz.com>
                ;; Date: Sun, 12 Nov 2000 16:58:28 -0800
                ;; in ACL6 and later, all sockets are bivalent (both
                ;; text and binary) and thus there's no need to convert
                ;; between the element types.
                #+allegro-v5.0
                (unless (eq (socket:socket-format sock) fmt)
                  (warn "~s: ACL5 cannot modify socket format"
                        'socket-accept))
                #+allegro-v4.3
                (socket:set-socket-format sock fmt)
                sock))
  #+clisp (multiple-value-bind (sec usec) (floor (or wait 0))
            (when (#+lisp=cl ext:socket-wait #-lisp=cl lisp:socket-wait
                             serv (and wait sec) (round usec 1d-6))
              (#+lisp=cl ext:socket-accept #-lisp=cl lisp:socket-accept
                         serv :element-type
                         (if bin '(unsigned-byte 8) 'character))))
  #+(or cmu scl)
  (when (sys:wait-until-fd-usable serv :input wait)
    (sys:make-fd-stream (ext:accept-tcp-connection serv)
                        :buffering (if bin :full :line)
                        :input t :output t :element-type
                        (if bin '(unsigned-byte 8) 'character)))
  #+gcl (si:accept-socket-connection serv bin wait) ; FIXME
  #+lispworks (make-instance
               'comm:socket-stream :direction :io
               :socket (mp:mailbox-read (socket-server-mbox serv))
               :element-type (if bin 'unsigned-byte 'base-char))
  #+mcl (ccl:accept-connection serv :wait wait)
  #+(and sbcl db-sockets)
  (let ((new-connection (sockets:socket-accept serv)))
    ;; who needs WAIT and BIN anyway :-S
    new-connection)
  #+(and sbcl net.sbcl.sockets)
  (net.sbcl.sockets:accept-connection
   serv
   (if bin
       'net.sbcl.sockets:binary-stream-socket
       'net.sbcl.sockets:character-stream-socket)
   :wait wait)
  #+(and sbcl sb-bsd-sockets)
  (progn
    (setf (sb-bsd-sockets:non-blocking-mode serv) wait)
    (let ((s (sb-bsd-sockets:socket-accept serv)))
      (if s
	  (sb-bsd-sockets:socket-make-stream
           s :input t :output t
           :element-type (if bin '(unsigned-byte 8) 'character)
           :buffering (if bin :full :line))
	  (sleep wait))))
  #-(or abcl allegro clisp cmu gcl lispworks mcl
        (and sbcl (or net.sbcl.sockets db-sockets sb-bsd-sockets)) scl)
  (error 'not-implemented :proc (list 'socket-accept serv bin)))

(defun socket-server-close (server)
  "Close the server."
  (declare (type socket-server server))
  #+abcl (ext:server-socket-close server)
  #+allegro (close server)
  #+clisp (#+lisp=cl  ext:socket-server-close
           #-lisp=cl lisp:socket-server-close server)
  #+(or cmu scl) (unix:unix-close server)
  #+gcl (close server)
  #+lispworks (mp:process-kill (socket-server-proc server))
  #+openmcl (close server)
  #+(and sbcl db-sockets) (sockets:socket-close server)
  #+(and sbcl net.sbcl.sockets) (close server)
  #+(and sbcl sb-bsd-sockets) (sb-bsd-sockets:socket-close server)
  #-(or abcl allegro clisp cmu gcl lispworks openmcl
        (and sbcl (or net.sbcl.sockets db-sockets sb-bsd-sockets)) scl)
  (error 'not-implemented :proc (list 'socket-server-close server)))

(defun socket-server-host/port (server)
  "Return the local host&port on which the server is running, as 2 values."
  (declare (type socket-server server))
  #+allegro (values (socket:ipaddr-to-dotted (socket:local-host server))
                    (socket:local-port server))
  #+(and clisp      lisp=cl)  (values  (ext:socket-server-host server)
                                       (ext:socket-server-port server))
  #+(and clisp (not lisp=cl)) (values (lisp:socket-server-host server)
                                      (lisp:socket-server-port server))
  #+(or cmu scl)
  (values (ipaddr-to-dotted (car (ext:host-entry-addr-list
                                  (ext:lookup-host-entry "localhost"))))
          (nth-value 1 (ext:get-socket-host-and-port server)))
  #+gcl (let ((sock (si:getsockname server)))
          (values (car sock) (caddr sock)))
  #+lispworks (values (ipaddr-to-dotted (comm:get-host-entry
                                         "localhost" :fields '(:address)))
                      (socket-server-port server))
  #+openmcl
  (values (ccl:ipaddr-to-dotted (ccl:local-host server))
          (ccl:local-port server))
  #+(and sbcl db-sockets)
  (multiple-value-bind (addr port) (sockets:socket-name server)
    (values (vector-to-ipaddr addr) port))
  #+(and sbcl net.sbcl.sockets)
  (net.sbcl.sockets:passive-socket-host-port server)
  #+(and sbcl sb-bsd-sockets)
  (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name server)
    (values (ipaddr-to-dotted (vector-to-ipaddr addr)) port))
  #-(or allegro clisp cmu gcl lispworks openmcl
        (and sbcl (or net.sbcl.sockets db-sockets sb-bsd-sockets)) scl)
  (error 'not-implemented :proc (list 'socket-server-host/port server)))

;;;
;;; }}}{{{ for CLX
;;;

(defun wait-for-stream (stream &optional timeout)
  "Sleep until there is input on the STREAM, or for TIMEOUT seconds,
whichever comes first. If there was a timeout, return NIL."
  #+clisp (multiple-value-bind (sec usec) (floor (or timeout 0))
            (#+lisp=cl ext:socket-status #-lisp=cl lisp:socket-status
                       stream (and timeout sec) (round usec 1d-6)))
  #+(or cmu scl)
  (#+mp mp:process-wait-until-fd-usable #-mp sys:wait-until-fd-usable
        (system:fd-stream-fd stream) :input timeout)
  #+openmcl
  (ccl:make-socket :type :stream
                   :address-family :file
                   :connect :active
                   :format (if bin :binary :text)
                   :remote-filename path)
  #+(and sbcl net.sbcl.sockets)
  (net.sbcl.sockets:wait-for-input-data stream timeout)
  #+(and sbcl db-sockets)
  (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream) :input timeout)
  #-(or clisp cmu (and sbcl (or net.sbcl.sockets db-sockets)) scl)
  (error 'not-implemented :proc (list 'wait-for-stream stream timeout)))

(defun open-unix-socket (path &key (kind :stream) bin)
  "Opens a unix socket. Path is the location.
Kind can be :stream or :datagram."
  (declare (simple-string path) #-(or cmu sbcl) (ignore kind))
  #+allegro (socket:make-socket :type :stream
                                :address-family :file
                                :connect :active
                                :remote-filename path)
  #+cmu (sys:make-fd-stream (ext:connect-to-unix-socket path kind)
                            :input t :output t :element-type
                            (if bin '(unsigned-byte 8) 'character))
  #+(and sbcl net.sbcl.sockets)
  (net.sbcl.sockets:make-socket 'net.sbcl.sockets:unix-stream-socket
                                :buffering :full :path path :type kind)
  #+(and sbcl db-sockets)
  (let ((socket (make-instance 'sockets:unix-socket :type :stream)))
    (sockets:socket-connect socket path)
    (sockets:socket-make-stream socket :input t :output t
                                :buffering :none
                                :element-type '(unsigned-byte 8)))
  #-(or allegro cmu (and sbcl (or net.sbcl.sockets db-sockets)))
  (open path :element-type (if bin '(unsigned-byte 8) 'character)
        :direction :io))

;;;
;;; }}}{{{ conditions
;;;

(defun report-network-condition (cc out)
  (declare (stream out))
  (format out "[~s] ~s:~d~@[ ~?~]" (net-proc cc) (net-host cc)
          (net-port cc) (net-mesg cc) (net-args cc)))

(define-condition network (error)
  ((proc :type symbol :reader net-proc :initarg :proc :initform nil)
   (host :type simple-string :reader net-host :initarg :host :initform "")
   (port :type (unsigned-byte 16) :reader net-port :initarg :port :initform 0)
   (mesg :type (or null simple-string) :reader net-mesg
         :initarg :mesg :initform nil)
   (args :type list :reader net-args :initarg :args :initform nil))
  (:report report-network-condition))

(define-condition timeout (network)
  ((time :type (real 0) :reader timeout-time :initarg :time :initform 0))
  (:report (lambda (cc out)
             (declare (stream out))
             (report-network-condition cc out)
             (when (plusp (timeout-time cc))
               (format out " [timeout ~a sec]" (timeout-time cc))))))

(define-condition login (network) ())
(define-condition net-path (network) ())

;;;
;;; }}}{{{ `socket-service-port'
;;;

(defstruct servent
  "see getservbyname(3) for details"
  (name "" :type simple-string) ; official name of service
  (aliases nil :type list)      ; alias list
  (port -1 :type fixnum)        ; port service resides at
  (proto :tcp :type symbol))    ; protocol to use

(defun socket-service-port (&optional service (protocol "tcp"))
  "Return the SERVENT structure corresponding to the SERVICE.
When SERVICE is NIL, return the list of all services."
  (with-open-file (fl #+unix "/etc/services" #+(or win32 mswindows)
                      (concatenate 'string (getenv "windir")
                                   "/system32/drivers/etc/services")
                      :direction :input)
    (loop :with name :and aliases :and port :and prot :and tokens
      :for st = (read-line fl nil nil)
      :until (null st)
      :unless (or (zerop (length st)) (char= #\# (schar st 0)))
        :do (setq tokens (string-tokens
                          (nsubstitute
                           #\Space #\/ (subseq st 0 (position #\# st))))
                  name (string-downcase (string (first tokens)))
                  aliases (mapcar (compose string-downcase string)
                                  (cdddr tokens))
                  port (second tokens)
                  prot (third tokens)) :and
        :if service
          :when (and (string-equal protocol prot)
                     (or (string-equal service name)
                         (member service aliases :test #'string-equal)))
            :return (make-servent :name name :aliases aliases :port port
                                  :proto prot)
          :end
          :else :collect (make-servent :name name :aliases aliases :port port
                                       :proto prot)
        :end
      :end
      :finally (when service
                 (error "~s: service ~s is not found for protocol ~s"
                        'socket-service-port service protocol)))))

;;; }}}

(provide :port-net)
;;; file net.lisp ends here
