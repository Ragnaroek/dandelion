#|
A multi-service tcp server in lisp

Mail to don@isis.cs3-inc.com (then read the bounce).

table of contents:
 license
 introductory documentation
 code
 Instructions for use, examples

 ==== license ====
Copyright (c) 2000 CS3
All rights reserved.

This program is distributed under the terms of
the GNU Lesser General Public License (LGPL) which can be found at
 http://www.gnu.org/copyleft/lesser.html 
as clarified by the Franz AllegroServe Prequel which can be found at
 http://AllegroServe.sourceforge.net/license-allegroserve.txt 
with the obvious substitutions:
- replace "Franz Inc." with "CS3"
- replace references to "AllegroServe" with "this program" or a name
  for this program.

 ==== introductory documentation ====

 note: this implementation requires either 
 clisp 2.31 + as of 2003/9/19 with (ext:write-byte-sequence :no-hang)
   packages changed to socket, ext
   uses SOCKET-STATUS, READ-BYTE-WILL-HANG-P, READ-BYTE-NO-HANG
    socket-stream-peer with second argument
 or
 Allegro CL 5.0.1 (modifications expected to be needed for later versions)

This code supports multiple tcp based servers in one shared lisp process.
Why is that a good thing?  One objective is to write/maintain/customize
servers in lisp rather than ... well, anything else.
The other objective is to defeat various types of denial of service attacks.
We need to limit resources in a global way.  If we simply fork independent 
processes to deal with different connections then an attacker can make more 
connections than we can handle.  If we have independent processes responding 
to different ports, an attacker might create too many connections by using 
a lot of different services.  Of course, you can also provide different
services from different processes by just running multiple copies of this
program.

Here we wish to limit such things as
 - the time a given connection has to complete its business
 - the number of characters a client can send the server
   (It's up to the server code to limit the amount of resources it 
   expends in responding to that input.)
 - the number of connections from a given host at a time
 - more generally the number of connections at a time from a set
   of IP addresses.

The default below limits the number of connections at one time from any set 
of IP addresses with the same initial n bits:
  leading bits  max # connections
	32		 4	;; number from one host
	24		 8	;; number from one class c network
	16		16
	 8		32
	 0		64	;; total number
This way we limit the total number of connections at one time, and yet
it's very difficult for one attacker or one small group of attackers
to deny service to more than their own sites.

For services such as telnet, which are generally open only to trusted
clients but should not be restricted in resource consumption, it 
probably makes more sense to use the normal server mechanism.

The current implementation does not rely on multiple processes or threads 
because Clisp does not support them.  Instead it uses a facility, 
recently (2000) added to Clisp at my request, the ability to test whether 
non-blocking output (as well as input) to a socket is possible.  Franz 
has told me how to do something similar in Allegro.  See output-possible
below.  I don't know of similar facilities in other lisp implementations 
but I suppose if they don't already exist they could be added with foreign 
functions.

The non-threaded implementation complicates some aspects of implementation 
for the servers that use this facility.  I'll call this the "shared server" 
and I'll call those others the "server instances".
Server instances have to read and write in special ways and have to avoid 
other operations that might block.  However, the fact that the server 
instances run in a non-threaded mode also simplifies other aspects of 
their implementation.

This implementation polls different ports for new connections and different 
existing connections for new input.  This is not overly expensive as long 
as we don't need extremely quick response.  The default below is that if 
we go through a complete polling cycle without finding any new connections 
or new input from existing connections, we wait one second before checking 
again.  This means that a client may have to wait up to a second for the 
server to notice new input.  For most services that's no problem.  Polling 
a relatively small number of ports and connections once a second does not 
use a significant portion of the processing power of a pc.  

I don't actually intend this server to deal with thousands of 
simultaneously open connections.  Still, this might be possible with 
some more support from lisp.  In particular, if we had slightly better 
access to the underlying "select" system call and could use it also to 
react to the appearance of connection requests, this server could be 
done without polling.  

Note that this server is not meant to prevent denial of service attacks
at lower levels, such as flooding your site with connection attempts or
just garbage packets.  These have to be handled by other mechanisms.

I mark with [***] things that I expect you might want to change in this 
code.

|#

;; ==== code ====

(in-package :user)

(defpackage "SSS" (:use "COMMON-LISP")
	    (:nicknames "SharedSocketServer")
	    (:export
	     "*PORTS*"  ;; push port numbers onto this
	     "START-SERVERS" ;; open servers for ports in *ports*
	     "SERVE" ;; call this to start the shared server
	     "START-SERVER"  ;; (start-server port-number) for a single port
	     "CLOSE-SERVERS" "CLOSE-SERVER"  ;; analogous to above
	     "CONNECTION"  ;; class to specialize
	     "TEMP-FILE-NAME"
	     "*SERVER-HOOKS*"
	     "*CYCLE-IO-LIMIT*"

	     ;; possibly useful accessors for connections
	     "SSTREAM"  ;;  the underlying socket stream
	     "INPUT"    ;;  the input waiting to be processed
	     "CREATION-TIME"  ;; universal time when the connection was created
	     "BINDINGS" ;; variables and values for a given connection
	     "NCHARS"   ;; number of characters read so far
	     "POS"      ;; where the next input starts

	     ;; you may add methods to these
	     "CONNECTION-CLASS"  ;; make connections of your own classes
	     "CONNECTION-CLOSE-P"  ;; decide whether to close a connection
	     "ACCEPT-P"  ;; decide whether to accept a connection
	     "REASON-NOT-TO-ACCEPT" ;; for use in above method
	     "MAX-INPUT-SIZE"  ;; how much input to accept
	     "MAX-INPUT-REACHED"  ;; what to do when that max is reached
	     "SHUT-DOWN-CONNECTION"  ;; how to shut down a connection
	     "READER" ;; get input to process
	     "EVALER" ;; process the above input
	     "PRINTER"  ;; report the results
	     "PROCESS-CONNECTION"  ;; do all the processing for a connection

	     ;; variables you might want to change
	     "*WAIT-TIME*"  ;; max time without running hooks
	     "*DEBUG*"  ;; set to T to get debug output
	     "*IGNORE-ERRS-P*"  ;; set to nil to stop catching errors
	     "LOG-ERR" ;; function for reporting ignored errors
	     
	     ;; functions to use in the evaler
	     "BIND"
	     "UNBIND"
	     "DONE"  ;; meaning disconnect after finishing output
	     "DISCONNECT"  ;; for client to use 
	     "CONNECTION-IPADDR" ;; useful wherever you have a connection
	     "DBG"
	     "SEND-STRING"      ;; output functions
	     "SEND-BYTE-VECTOR"
	     "SEND-FILE"
	     "PROCESS-OUTPUT" ;; sort of like force-output from evaler
	     ))

(in-package :sss)
(provide :sss)

;; debugging support
(defvar *ignore-errs-p* t) ;; [***] set to nil for serious debugging
(defmacro ignore-errs (&rest forms)
  `(flet ((f () .,forms))
     (if *ignore-errs-p*
	 ;; it's dangerous to ignore errors without even logging
	 (multiple-value-bind
	   (ans err)
	   (ignore-errors (multiple-value-list (f)))
	   (when err (log-err (format nil "ignore error ~a" err)))
	   (values-list ans))
       (f))))

(defun log-err (string) ;; [***] meant to be replaced by application
  (format t "~%~a" string))

(defvar *debug* nil) ;; set to T for debugging output [***]
(defvar *debug2* nil) ;; more selective debugging

(defvar *last-dbg-real-time* 0)
(defvar *last-dbg-run-time* 0)
(defun dbg (&rest rest)
  (when *debug*
	(format *trace-output* "~& debug ~A ~A "
		(- (get-internal-real-time) *last-dbg-real-time*)
		(- (get-internal-run-time) *last-dbg-run-time*))
	(setf *last-dbg-real-time* (get-internal-real-time)
	       *last-dbg-run-time* (get-internal-run-time))
	(apply 'format *trace-output* (abbrev rest))))
;; just cause we sometimes get huge strings
;; clearly we could also get huge lists, arrays, etc.
;; so you might replace this for a given application
(defun abbrev (x)
  (cond ((and (stringp x) (> (length x) 300))
	 (format nil "~A~% ... [total length = ~A] ... ~%~A"
		 (subseq x 0 100) (length x) (subseq x (- (length x) 100))))
	((consp x) (cons (abbrev (car x)) (abbrev (cdr x))))
	(t x)))

(defvar *ports* nil) ;; push your ports onto here [***]
(defvar *server-hooks* nil) ;; and push your hooks here [***]
;; A hook is a function of no arguments that is executed on each
;; server cycle.  It should return non-nil if it actually does
;; something and nil if it finds nothing to do.
(defvar *servers* nil)

;; Now that we have socket-status (in clisp, at least)
;; we should be able to avoid sleep
(defvar *socket-status-arg* nil)
;; we try to maintain above and avoid consing
;; each element is (stream direction . result)
;; where stream is either a socket-server or a socket-stream,
;; direction is :input, :output or :io, depending on whether we're
;; waiting for the stream to be readable, writable or either,
;; and result is t or nil, written by socket-status (we'll ignore it).

(defun start-servers ()
  (loop for p in *ports* do (start-server p)))

(defun start-server (p) ;; (port:open-socket-server p)
  (push #+clisp (socket:SOCKET-SERVER p)
	#+allegro(socket:make-socket :connect :passive :local-port p
				     :format :bivalent)
	#-(or clisp allegro) (error "no implementation for start-server")
	*servers*)
  (push (list (car *servers*) :input) *socket-status-arg*))

;; while we're at it
(defun close-server (s) ;; (port:socket-server-close s)
  #+clisp (socket:socket-server-close s)
  #+allegro (close s)
  #-(or clisp allegro) (error "no implementation for close-server")
  (setf *socket-status-arg* (remove s *socket-status-arg* :key 'car))
  (setf *servers* (delete s *servers*)))

(defun close-servers ()
  (loop for s in *servers* do
	(close-server s)))

(defgeneric connection-class (port)) ;; return class name
(defmethod connection-class ((port t)) 'connection)

(defun accept-connection (server) ;; not implemented in port
  (ignore-errs
   #+clisp
   (when (socket:SOCKET-WAIT server 0)
     (make-instance (connection-class (socket:socket-server-port server))
       :stream (socket:SOCKET-ACCEPT server)))
   #+allegro
   (let ((c (socket:accept-connection server :wait nil)))
     (when c (make-instance (connection-class (socket:local-port server))
	       :stream c)))
   #-(or clisp allegro) (error "no implementation for accept-connection")))

(defvar *last-connection* 0)

(defclass connection ()
  (;; all connections have id's for logging
   (id :accessor id :initform (incf *last-connection*))
   (creation-time :initform (get-universal-time))
   (bindings :accessor bindings :initform nil)
   (stream :accessor sstream :initarg :stream)
   ;; accessor stream -> sstream cause stream is external in cl pkg
   ;; since socket streams tend to have fixed finite buffers for every
   ;; socket stream there must be a buffer where we copy the the input
   ;; to be processed later
   (nchars :initform 0) ;; # chars read so far
   (done :initform nil) ;; t when we're done, just waiting to finish output
   (new-input :initform nil) ;; t if there are chars that reader hasn't seen
   ;; if reader sees them but returns nil then it's set to nil
   (input :accessor input :initform
	  (make-array 0 
		      :element-type 'character
		      :fill-pointer t :adjustable t))
   (pos :accessor pos :initform 0)
   (pending-output :initform nil :accessor output)))

(defun connection-ipaddr (c) ;; port:socket-host/port ?
  #+clisp ;; why can't clisp just give me this directly?
  (let ((p (socket:socket-stream-peer (sstream c) t)) (ans 0)(pos 0) next)
    (loop for i below 4 do
	  (setf next (position #\. p :start pos))
	  (setf ans (+ (ash ans 8)
		       (read-from-string p nil nil :start pos :end next))
          pos (and next (1+ next))))
    ans)
  #+allegro (socket:remote-host (sstream c))
  #-(or clisp allegro) (error "no implementation for connection-ipaddr"))

(defvar *connections* nil) ;; a list of active connections

(defvar *temp-file* ;; [***]
    ;; I need the name of a file that I can open for input
    ;; This is just to reserve an fd - see *temp-stream* below
    #+unix "/tmp/foo"
    #+mswindows "c:/tmp/foo"
    #-(or mswindows unix) (error "need a temp file"))

(defvar *temp-stream* nil)
;; In order to be pretty sure that I'll be able to open one file at a
;; time without running out of fd's I open a temp file to reserve an fd.
;; Then when I want to output the contents of a file (to a socket) I'll
;; close that, open the file, read it and write its contents, then close
;; it and reopen the temp file.
;; This is only used for send-file, so if your servers don't use that
;; then this (and *temp-file*) can be removed.

(defvar *wait-time* nil) ;; [***]
#| max time to wait for i/o without running server hooks
 nil = never wake up without i/o
 otherwise this should be a non-negative integer or float (see use below)
 If you have hooks that should run every so often even when there's
 nothing else going on, then make this the number of seconds you're
 willing to wait between executing the hooks.
|#

;; start the server
(defun serve (&aux done-anything)
  (unless (and *temp-stream* (open-stream-p *temp-stream*))
    ;; if this errs we want to break here ...
    (setf *temp-stream* (open *temp-file* :if-does-not-exist :create)))
  (loop ;; for age from 0 do ;; just to show time passing on log
	(setf done-anything nil)
	(loop for h in *server-hooks* when (funcall h) do
	      (setf done-anything t))
	(loop for c in *connections* do
	      (if (process-connection c)
		  ;; should return t if it stops with more to do
		  (setf done-anything t))
	      (when (connection-close-p c)
		(disconnect-connection c)))
	;; now check for new connections
	(loop for s in *servers* do
	      (ignore-errs
	       (let ((new (accept-connection s)))
		 (when new
		   (setf done-anything t)
		   (if (accept-p new)
		       (progn (push new *connections*)
			      (push (list (sstream new) :input)
				    *socket-status-arg*))
		       ;; would be nice if we could send an error message ...
		       (disconnect-connection new))))))
	(unless done-anything (wait *wait-time*))))

(defun wait (time)
  #-clisp (sleep time)
  ;; In the case above time is the max time you're willing to make someone
  ;; wait for service.
  ;; In the case below time is the time you're willing to wait between
  ;; executions of hooks in *server-hooks*, generally a lot longer,
  ;; or even infinite.
  #+clisp
  (progn (loop for c in *connections* do
	   (setf (cadr (assoc (sstream c) *socket-status-arg*))
		 (if (output c) (if (slot-value c 'done) :output :io) :input)))
	 ;; if we have pending output then wait for the stream to be
	 ;; either readable or writable, else just readable
	 (if time
	     (multiple-value-bind (a b) (floor time)
	       (ext:socket-status *socket-status-arg* a (round (* b 1e6))))
	   (ext:socket-status *socket-status-arg*))))
	       

;; Support for IO

(defun output-possible (socket-stream) 
  #+clisp
  (member (socket:SOCKET-STATUS socket-stream 0) '(:output :io :append))
  ;; append added in 2003, meaning input => eof
  ;; If you're not using a version that supports this operation it's
  ;; at least semi-reasonable for many applications to simply define 
  ;; output-possible as always T - meaning that you'll block when you 
  ;; try to write too much.
  #+ignore (progn (warn "socket-status not available - using T instead") t) 
  #+allegro ;; [***] not expected to work after acl 5.0.1 !! 
  (excl::filesys-fn-will-not-block-p  
   (- -1 (excl:stream-output-fn socket-stream))) 
  #-(or allegro clisp) (error "no implementation for output-possible"))

(defun input-possible (socket-stream) 
  ;; should return t if input functions will not block, i.e. 
  ;; if they will return either eof or a character/byte 
  #+clisp ;; note the stream is in binary mode here
  (not (ext:READ-BYTE-WILL-HANG-P socket-stream))
  #+ignore ;; old version
  (let ((s-e-t (stream-element-type socket-stream))) 
    (unwind-protect 
	(progn (setf (stream-element-type socket-stream) 'character) 
	       ;; for binary stream gives wrong answer!@#$% 
	       (not (ext:read-char-will-hang-p socket-stream))) 
      (setf (stream-element-type socket-stream) s-e-t)))
  ;; above = (lisp:read-byte-look-ahead socket-stream) 
  #+allegro ;; [***] see above 
  (excl::filesys-fn-will-not-block-p  
   (- -1 (excl:stream-input-fn socket-stream)))
  #-(or allegro clisp) (error "no implementation for input-possible")) 

;; These are mostly for use by evalers and printers.

(defun send-string (connection string &optional start end)
  ;; enqueue a string for output on the current connection
  (setf (output connection)
    (nconc (output connection)
	   (list (list :string string start end)))))

(defun send-byte-vector (connection vec &optional start end)
  ;; analogous to string but binary output
  (setf (output connection)
    (nconc (output connection)
	   (list (list :byte-vector vec start end)))))

(defun send-file (connection file &optional start end)
  ;; enqueue a file for output on the current connection
  (setf (output connection)
    (nconc (output connection)
	   (list (list :file file start end)))))

(defvar *cycle-io-limit* 10000) ;; [***]
;; don't do more than this many characters of input or output per cycle
;; related to single thread (so other connections get a chance)
(defparameter *byte-io-vector*
  (make-array *cycle-io-limit* :element-type '(unsigned-byte 8)))
;; Note this must be reinitialized if *cycle-io-limit* ever changes.

;; This can be used to get the effect of force-output to the connection.

;; return t if there's reason not to sleep (more output and it won't hang)
(defun process-output (connection)
  (multiple-value-bind (val err)
    (ignore-errs
       (when (output connection)
	(let* ((stream (sstream connection))
	       (out (car (output connection)))
	       (type (car out))
	       (output (cadr out))
	       (start (max 0 (or (third out) 0)))
	       end) ;; end computed differently for different cases
	  (let ((*debug* *debug2*))(dbg "process-output ~A" out))
	  (ecase type ;; beginning to look like we should move to methods
	    (:string
	     (setf end (min (length output) (or (fourth out) (length output))))
	     #+clisp (setf (stream-element-type stream) 'character)
	     ;; assume that strings are small, hence not worth using
	     ;; write-byte-sequence
	     (loop for i from start below end
		 for char-number below *cycle-io-limit*
		 while (output-possible stream) do
		   (write-char (char output i) stream)
		   finally
		   (force-output stream)
		   (setf (third out) i)
		   (when (>= i end) (pop (output connection)))))
	    (:byte-vector
	     (setf end (min (length output) (or (fourth out) (length output))))
	     #+clisp (setf (stream-element-type stream) '(unsigned-byte 8))
	     ;; assume that strings are small, hence not worth using
	     ;; write-byte-sequence
	     (loop for i from start below end
		 for char-number below *cycle-io-limit*
		 while (output-possible stream) do
		   (write-byte (aref output i) stream)
		   finally
		   (force-output stream)
		   (setf (third out) i)
		   (when (>= i end) (pop (output connection)))))
	    (:file
	     (when (and *temp-stream* (open-stream-p *temp-stream*))
	       (close *temp-stream*))
	     (setf output (open output :element-type '(unsigned-byte 8)))
	     (file-position output start)
	     (setf end (min (file-length output)
			    (or (fourth out) (file-length output))))
	     #+clisp (setf (stream-element-type stream) '(unsigned-byte 8))
	     #+clisp ;; relies on presence of write-byte-sequence :no-hang
	     (let* ((nread (ext:read-byte-sequence
			    *byte-io-vector* output
			    :end (min end *cycle-io-limit*) :no-hang t))
		    (nwrite (cadr (multiple-value-list
				   (ext:write-byte-sequence
				    *byte-io-vector* stream
				    :end nread :no-hang t)))))
	       (force-output stream)
	       (setf (third out) (+ start nwrite))
	       (close output)
	       (setf *temp-stream*
		     ;; really need this :create if file is in /tmp !!
		     ;; (how do you *think* I know?)
		     (open *temp-file* :if-does-not-exist :create))
	       (when (>= (third out) end) (pop (output connection))))
	     #-clisp
	     (loop for i from start below end
		 for char-number below *cycle-io-limit*
		 while (output-possible stream) do
		   ;; I assume we're not changing the file now ...
		   (write-byte (read-byte output) stream)
		   finally
		   (force-output stream)
		   (setf (third out) i)
		   (close output)
		   (setf *temp-stream*
			 (open *temp-file* :if-does-not-exist :create))
		   (when (>= i end) (pop (output connection))))))
	  (return-from process-output
	    (and (output connection) (output-possible stream))))))
    (when err
      (dbg "process-output closing stream due to error: ~A" err)
      (close (sstream connection)))
    val))

;; not to be called by external code
;; return t if there's reason not to sleep (more input waiting)
(defun process-input (c)
  #+clisp ;; all input will be in binary
  (setf (stream-element-type (sstream c)) '(unsigned-byte 8))
  (ignore-errs;; in case stream closes while reading
   ;; Luckily read-char-no-hang on a closed stream => eof
   #+clisp
   (loop with stream = (sstream c)
       with already = (slot-value c 'nchars)
       with limit = (min *cycle-io-limit* (- (max-input-size c) already))
       for i below
       (ext:read-byte-sequence
	*byte-io-vector* (sstream c) :no-hang t
	:end limit)
       do (vector-push-extend (code-char (aref *byte-io-vector* i)) (input c))
       finally
       (let ((*debug* *debug2*))(dbg "read-byte-sequence read ~A bytes" i))
       (when (member (socket:SOCKET-STATUS stream 0) '(:eof :append))
	 (setf (slot-value c 'done) t)) ;; not (close stream)
       (incf (slot-value c 'nchars) i)
       (setf (slot-value c 'new-input) t)
       (when (= (slot-value c 'nchars) (max-input-size c))
	 (max-input-reached c))
       ;; maybe more to read if we read the limit
       (return-from process-input (= i limit)))
   #-clisp
   (loop with char = t with stream = (sstream c)
       with already = (slot-value c 'nchars)
       for i below (- (max-input-size c) already)
       for char-number below *cycle-io-limit*
       while (setf char (next-char stream)) do
	 (if (eq char :eof) (setf (slot-value c 'done) t) ;; not (close stream)
	     (vector-push-extend
	      char (input c)
	      #+allegro;; allegro has the wrong default here
	      ;; clisp is significantly faster without the argument
	      (max 20 (length (input c)))))
	 finally
	 ;; above code seems better - return t if you read the limit
	 (when (and (= 0 i) (not (slot-value c 'new-input)))
	   (return-from process-input nil))
	 (incf (slot-value c 'nchars) i)
	 (setf (slot-value c 'new-input) t)
	 (when (= (slot-value c 'nchars) (max-input-size c))
	   (max-input-reached c)))))

(defun next-char (stream)
  ;; stream is a socket stream
  ;; we want a character but we have to distinguish CR from LF
  ;; also we want :eof or nil if there is no character available,
  ;; depending on whether the stream is closed
  #+clisp ;; note the stream is in binary mode here
  (let ((ans (ext:READ-BYTE-NO-HANG stream nil :eof)))
    (if (symbolp ans) ans (code-char ans)))
  #+ignore ;; old version
  (if (input-possible stream)
	(let ((ans (read-byte stream nil :eof)))
	  (if (eq ans :eof) ans (code-char ans))))
  #+allegro ;; already does the right thing
  (read-char-no-hang stream nil :eof)
  #-(or allegro clisp) (error "not yet implemented"))

(defun remove-read-chars (s pos) ;; only used by process-connection on socket
  ;; we've read up to pos so get rid of those chars
  (loop for i from pos below (length s) do 
	(setf (aref s (- i pos)) (aref s i))) 
  (setf (fill-pointer s) (- (fill-pointer s) pos)))

;; Call this to indicate that you want to finish the output already
;; queued and then close the connection.
(defun done (connection)
  (setf (slot-value connection 'done) t)
  (setf (slot-value connection 'new-input) nil))

;; decide whether it's time to close this connection
;; return t if so, otherwise nil
(defgeneric connection-close-p (connection))
(defmethod connection-close-p (connection)
  ;; I figure for most connections 1 minute should be sufficient
  (> (get-universal-time) (+ 60 (slot-value connection 'creation-time))))

;; what to do when a new connection appears
;; you should close it and return nil if there are too many connections
;; otherwise do whatever you do to welcome the connection and return c.
;; Ok, I admit, by the time this is called the connection is already
;; accepted, but this is your change to immediately close it.
(defgeneric accept-p (connection))
(defmethod accept-p (connection)
  ;; here's my default method
  (let ((complaint (reason-not-to-accept connection)))
    (if complaint
	(progn (send-string connection (format nil "~A" complaint)) nil)
      t)))

(defun reason-not-to-accept (connection)
  (loop for (nbits limit) in '((32 4) (24 8) (16 16) (8 32) (0 64)) do
	(let ((count (loop for c in *connections* count
			   (= (ash (connection-ipaddr connection) (- nbits 32))
			      (ash (connection-ipaddr c) (- nbits 32))))))
	  (when (>= count limit)
	    (return-from reason-not-to-accept
	      (format nil
		      "we already have ~A connections from clients ~
                       with addresses that agree with the first ~A of yours"
		      count nbits))))))

;; how many input bytes are we willing to accept from a client
(defgeneric max-input-size (connection)) 
(defmethod max-input-size ((connection t)) 5000)
;; seems a reasonable default - make it more for mail

;; what should we do when that limit is reached
(defgeneric max-input-reached (connection))
;; probably any other method should first print what it wants to the 
;; connection and then call next method
(defmethod max-input-reached (c)
  (disconnect-connection c))

;; === disconnecting connections ===

;; the polite way for a client to leave is to call disconnect
(defun disconnect ()
  (declare (special connection))
  (disconnect-connection connection))

;; the real one
(defun disconnect-connection (connection)
  (ignore-errs (shut-down-connection connection))
  ;; ignore errors in case you try to print to a closed connection
  ;; Of course, if you fail to close it, you're going to run out of fd's !
  (setf *connections* (delete connection *connections*))
  (setf *socket-status-arg*
	(remove (sstream connection) *socket-status-arg* :key 'car))
  nil)

#| In order to make the disconnect function do "the right thing" for any
sort of connection, it calls the shut-down-connection method which is supposed
to close open streams etc. |#
;; This method allows you to not bother defining a method for connections that
;; don't need any further action.
(defgeneric shut-down-connection (connection))
;; probably other methods that specialize this should also call next method
(defmethod shut-down-connection ((connection connection))
  (let ((stream (sstream connection)))
    (when (open-stream-p stream)
      (close stream))))

;; === processing commands from connections ===
		 
#| The process-connection method is the one that does whatever has to be
done for a connection:
If there's output pending do whatever output you can.
If there's input available read it into the input buffer.
Call the reader to determine whether there is enough input to process.
If so call the evaler to process it.

There is no protection here against infinite computations.  In the normal
case where the evaler does finish, it either causes an error or returns
some number of values.  At this point the printer is called.  It is passed
the list of return values (the evaler is called inside multiple-value-list), 
the error object, and the client connection  If there was no error, the error
object is nil.  Otherwise the list of return values is nil.  The printer 
function is also error protected.  
[removed: Also, to be on the safe side, *print-circle* is t.
 The problem is that clients don't like #1=...
 I guess the server instances will have to be careful about infinite
 prints along with other infinite computations]
|#

(defgeneric reader (connection string start)) ;; you must supply a method
(defgeneric evaler (connection input)) ;; you must supply a method
(defgeneric printer (connection output error)) 
(defmethod printer (connection output error)
  (declare (ignore connection output))
  (when error (format t "~&printer: ~A" error)))

;; You could just define a method for your connection class,
;; but I hope that won't be the normal thing to do.
(defmethod process-connection ((c connection) &aux more2do)
  ;; read pending input
  (unless (open-stream-p (sstream c))
	  (disconnect-connection c)
	  (return-from process-connection nil))
  ;; Get rid of streams when the client just disconnects.
  ;; If the disconnect happens while reading below
  ;; we give the client one last chance to eval
  ;; (if his reader succeeds).
  (setf more2do (process-output c)) ;; write whatever pending output we can
  (when (and (slot-value c 'done) (null (output c)))
    ;; redhat 6.2 closes with rst instead of fin if there's unread input !
    ;; this process-input just reduces the chance that this will happen
    (process-input c)
    (disconnect-connection c))
  (unless (slot-value c 'done)
    (setf more2do (or (process-input c) more2do)))
    ;; read whatever characters are waiting
  (unless (slot-value c 'new-input) (return-from process-connection more2do))
  (progv ;; read, eval, print - all in context of bindings
   (loop for varval in (bindings c) collect (car varval))
   (loop for varval in (bindings c) collect (cdr varval))
   (let ((connection c) ;; give read/eval/print access to client
	 ;;(*print-circle* t) ;; to avoid infinite loops in print
	 input output err pos)
     (declare (special connection))
     ;; call reader
     (when (> (length (input c)) 0)
	   (dbg "reader ~s ~a" (input c) (pos c)))
     (setf err nil)
     (handler-case
      (multiple-value-setq (input pos) (reader c (input c) (pos c)))
      (error (e)
	     ;; consider read errors as normal - print only if *debug* below
	     (setf err e)
	     (setf input nil pos nil)
	     (setf (fill-pointer (input c)) 0 (pos c) 0)))
     (when (> (length (input c)) 0)
	   (if err
	       (dbg "error in reader: ~a" err)
	     (dbg "reader => ~s, ~s" input pos)))
     (unless (or err (numberp pos))
       (setf (slot-value c 'new-input) nil))
     (when (or err (numberp pos)) ;; reader succeeded or erred
	   ;; If you're interested in using multiple processes in acl
	   ;; to do the eval/print, just insert here
	   ;; (mp:process-run-function "eval-print"
	   ;;    #'(lambda () <next two forms>))
	   ;; however you should be aware that the forms you send are
	   ;; no longer ordered (!) which may necessitate locking
	   (unless err
	     (setf (pos c) pos)
	     (when (> pos (/ (length (input c)) 2))
	       ;; limit the wasted space to <= unwasted
	       ;; also limit the wasted time in moving characters
	       (remove-read-chars (input c) pos)
	       (dbg "removing ~a chars from input" pos)
	       (setf (pos c) 0))
	     ;; call evaler
	     (dbg "evaler ~s" input)
	     ;; These bindings seem reasonable defaults.
	     ;; They can be overridden by the evaler
	     (multiple-value-setq (output err)
	       ;; This one is *NOT* ignore-errs - we need the error result
	       (ignore-errors
		(multiple-value-list (evaler c input))))
	     (dbg "evaler => ~s, ~a" output err))
	   ;; call printer
	   (when (open-stream-p (sstream c))
		 ;; unless eval closed it
		 (dbg "printer")
	    (handler-case
	     (printer c output err)
	     (error (err)
		    (format *trace-output*
			    "~&error in printer: ~a" err))))
	   (setf more2do t) ;; to indicate that some input was processed
	   )))
  more2do)

(defun bind (symbol value)
  (declare (special connection))
  (let ((assoc (assoc symbol (bindings connection))))
    (if assoc (setf (cdr assoc) value)
      (push (cons symbol value) (bindings connection))))
  value)

(defun unbind (symbol)
  (declare (special connection))
  (let ((assoc (assoc symbol (bindings connection))))
    (when assoc
	  (setf (bindings connection)
		(delete assoc (bindings connection)))))
  nil)


;; ==== Instructions for use, examples ====

#|
How to use the shared server

A number of things above are marked with [***].
These are things that I anticipate you might want to change.
If you search for *** above you will find them, along with text that 
I hope will adequately explain them.

The anticipated usage is
- load this file into lisp 
- load other files defining your services
- do (sss:start-servers)
- do (sss:serve)

So, the only question is how to write your own services.

Our model is that, for any connection, a server repeatedly reads 
a request, computes an answer, and sends it back to the client.
It's important not to block at any stage.  The main problem is IO.
The shared server solves this problem as described below.  The server 
instances (your code) should not directly use the socket streams.

The shared server collects characters as they arrive, without blocking,
into a string.  Your code supplies a reader method that determines 
whether that string contains a complete input, and if so, what it is.
When the reader succeeds, its result is passed to your evaler method.
This can send output to the client by calling send-string, 
send-byte-vector or send-file, which enqueue output.  In addition, 
the evaler may return values to be sent back by your printer method 
which should call the send- functions above to do the actual output.
The default printer method does nothing.

We use as an example a server that accepts up to 100 characters for
up to one minute and reports the number of characters you've sent so far
at every new line and at the end.  Below is what you would put into a file 
to define this service.

 (in-package :user)
 ;; the port on which your server will accept clients
 (pushnew 2000 sss:*ports*) ;; start-servers opens servers for these ports
 ;; define a subclass for your connections
 (defclass example-connection (sss:connection)
   ()) ;; add any slots specific to your connections

I guess you're really not supposed to specialize on (eql 2000) but 
 (1) it does work in both acl and clisp
 (2) clos really should have allowed something like this

 (defmethod sss:connection-class ((port (eql 2000))) 'example-connection)
 ;; this means that whenever we accept a connection on port 2000 we'll
 ;; make an instance of example-connection

You must supply a reader method.  It accepts as input a string and 
starting position in that string.  
If the string contains, as an initial substring, a "complete" input
then the reader should return two values, the object read (to be passed
to the evaler) and the position in the string where the next read should
start, possibly the length of the string.

If the reader fails, due to not enough input, it should return something 
other than a number for the second value.  If the reader generates an 
error, all the characters from the input stream are discarded and the 
printer is used to report the error.  The next call to the reader will 
see whatever characters have arrived since the call that got the error.

Note the following to important special cases.  Suppose we are trying
to read lisp forms to be evaluated.
- an incomplete input must not cause an error, e.g., the string
  "(+ " should simply return without a numeric second value.
- a string that could be read but could also be extended to read as
  something else should not return a numeric second value, e.g.,
  the string "123" should not return a numeric second value since
  the input might actually be "12345 ".

 (defmethod sss:reader ((c example-connection) string start)
   ;; if there's a newline return a value (in this case ignored) and,
   ;; the number of characters to consume
   (let ((pos (position #\newline string :start start)))
     (when pos (values nil (1+ pos)))))

You must also supply an evaler.

 ;; When reader succeeds, what do we do ...
 (defmethod sss:evaler ((c example-connection) form)
   ;; in this case a noop
   nil)

And you may supply a printer.

 ;; Report the results.
 (defmethod sss:printer ((c example-connection) output err)
   ;; in general we get either a list of values returned by the evaler
   ;; if there was no error or a condition object if there was one
   (declare (ignore output err))
   (sss:send-string c (format nil "~& so far ~A characters received~%"
                              (slot-value c 'sss:nchars)))) 

Other methods:

connection-close-p (connection) - should we close this connection?
The default returns T when the connection is more than one minute old
which is what we want here.

accept-p (connection) - when a connection appears, should we accept it?
Actually it's already accepted, but if this returns nil then we'll
immediately close it.
The default is that if there are too many other connections open from
similar ip addresses then we close it.  The sss:reason-not-to-accept
function returns the error message in this case.

 (defmethod sss:accept-p ((connection example-connection))
  ;; I add to the default method a greeting
  (let ((complaint (sss:reason-not-to-accept connection)))
    (sss:send-string connection
       (or complaint "You have one minute to type up to 100 characters ..."))
    (if complaint nil t)))

shut-down-connection (connection) - what to do when you shut down
The default just closes the stream.

 (defmethod sss:shut-down-connection ((c example-connection))
   ;; we want to report the number of characters at shutdown
   (sss:send-string c
     (format nil "~& closing connection after ~A characters received~%"
             (slot-value c 'sss::nchars)))
   (sss:process-output c) ;; this is analogous to force-output
   (call-next-method))

max-input-size (connection) - how many characters do we accept from client
Default is 5000.
 ;; we only want to allow 100 characters
 (defmethod sss:max-input-size ((connection example-connection)) 100)

max-input-reached (connection) - what to do when the limit is reached
Default is disconnect.  That's fine in this example.


Second example: a lisp listener
Of course, I would not recommend actually offering this service without 
a lot of restrictions!

 (in-package :user)
 (pushnew 3000 sss:*ports*) ;; at startup we'll open servers for these ports
 (defclass eval-connection (sss:connection)
   ()) 
 (defmethod sss:connection-class ((port (eql 3000))) 'eval-connection)
 (defun read-from-adjustable-string (string start)
  (vector-push-extend #\space string)
  (unwind-protect
      (multiple-value-bind
       (obj pos)
       (handler-case (read-from-string string nil nil
                         :start start :preserve-whitespace t)
		     (end-of-file ()
			(return-from read-from-adjustable-string)))
       (unless (= pos (length string)) ;; read could continue
	       (values obj pos)))
    (vector-pop string)))
 (defmethod sss:reader ((c eval-connection) string start)
   ;; Read-from-string is almost right, but if we start to type 123 we
   ;; don't want it to read 1 or 12.  
   (read-from-adjustable-string string start))
 (defmethod sss:evaler ((c eval-connection) form)
   (eval form))
 (defmethod sss:printer ((c eval-connection) values err)
   ;; the return values come back as a multiple value list
   (if err (sss:send-string c (format nil "~%error: ~A~%" err))
       (loop for v in values do (sss:send-string c (format nil "~A~%" v)))))

 ;; probably in this case we don't want to limit the session to 1 minute
 (defmethod sss:connection-close-p ((c eval-connection)) nil)

Some other facilities:

Each connection has its own set of variable bindings that are
available to the reader, evaler and printer.
To bind a variable for a connection, evaluate a form (from the reader,
evaler or printer) that does
(bind <symbol> <value>), e.g.
(bind '*package* (find-package :ap5))

This replaces any previous value, like setf would normally do.
The new value takes effect on the next evaluation for the connection.
Setf would work for this evaluation, but that's not recommended since
it could have unpredictable effects on other connections.

To remove a variable binding, do something like
(unbind '*package*)

Notice that in this case the client can do things like
 (bind '*package* (find-package :foo))
and the next interaction will be read, evaluated and printed from that 
package.

The client can do (sss:disconnect) to disconnect.
>From your server code it's normally better to do
(done <connection>) which will try to finish the output to the 
connection before closing it.

Naturally, these things also work from your own evaler methods.

One more thing.  I used to have logging facilities.  I've removed them
cause I expect that every service will want to do its own.

|#

#| notes on performance
clisp, linux, 200Mhz pentium

usec     bytes   operation
1            0   code-char
6            0   read-byte
6.7          0   read-char
9            0   write-byte
10.5         0   write-char
12.8         0   listen
14.5         0   system::listen-byte
18.4         0   lisp::socket-status
14.8        28   set stream elt type twice
135 !        0   read-char-no-hang when there is data

e.g.,
(compile (defun f(n stream)
 (loop for i below n do
  (setf (stream-element-type stream) '(unsigned-byte 8))
  (setf (stream-element-type stream) 'character))))
(time (f 100000 stream))
Real time: 1.487881 sec.
Run time: 1.47 sec.
Space: 2800000 Bytes
GC: 5, GC time: 0.09 sec.

(with-open-file (in "/tmp/1MB" :buffered nil :element-type '(unsigned-byte 8))
 (with-open-file (out "/tmp/foobar" :direction :output :if-exists :append
                      :if-does-not-exist :create :buffered nil
		      :element-type '(unsigned-byte 8))
   (let ((n (time (ext:read-byte-sequence *byte-io-vector* in :end *nbytes*
					  :no-hang t))))
     (cdr (multiple-value-list
	   (time (ext:write-byte-sequence *byte-io-vector* out :end n
					  :no-hang t)))))))
=> for 1 byte ~150, 130 usec
read: Real time: 1.48E-4 sec.; Run time: 0.0 sec.; Space: 0 Bytes
write:Real time: 1.28E-4 sec.; Run time: 0.0 sec.; Space: 0 Bytes
=> for 100KB ~3000, 2000 usec
read: Real time: 0.002894 sec.;Run time: 0.0 sec.; Space: 0 Bytes
write:Real time: 0.001943 sec.;Run time: 0.0 sec.; Space: 0 Bytes

|#
