;;; CL server - not yet fully implemented
;;;
;;; Copyright (C) 1997-2002 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: server.lisp,v 1.8 2005/01/27 23:02:46 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/server.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `list-format'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `package-short-name'
  (require :cllib-prompt (translate-logical-pathname "cllib:prompt"))
  (require :port-net (translate-logical-pathname "port:net")))

(in-package :cllib)

(export '(cl-server))

;;;
;;; }}}{{{
;;;

(defcustom *cl-server-port* integer 453
  "*The default port for `cl-server'.
Defaults to (parse-integer \"cl\" :radix 36) ==> 453")

(defcustom *cl-server-password* (or null simple-string) "ansi-cl"
  "*The simple authentication.
Set to NIL to disable.")

(defcustom *cl-server-quit* list '("bye" "quit" "exit")
  "*The list of `quit' commands.")

(defun cl-server-mesg (sock fmt &rest args)
  (princ "[") (current-time) (princ "]")
  (apply #'format t fmt args)
  (format t "~60t~a~%" (socket-string sock)))

(defun cl-server (&key (port *cl-server-port*) (password *cl-server-password*))
  "Establish a connection and answer questions."
  (declare (integer port))
  (let ((serv (open-socket-server port)))
    (declare (type socket-server serv))
    (unwind-protect
       (loop :for sock :of-type socket = (socket-accept serv) :with pwd :do
             (cl-server-mesg sock "connected:")
             (let ((*standard-output* sock)) (sysinfo))
             :if (or (null password)
                     (progn
                       (format sock "Enter password: ") (force-output sock)
                       (let ((str (read-line sock nil "")))
                         (declare (simple-string str))
                         (setq pwd (subseq str 0 (1- (length str))))
                         (string= pwd password))))
             :do (format sock "password ok~%to quit, type one of~?.~%"
                         (list-format "~s") *cl-server-quit*)
             (loop :for ii :of-type index-t :upfrom 1 :do
                   (format sock "~a[~d]: > " (package-short-name *package*) ii)
                   (force-output sock)
                   (handler-case
                       (let ((form (read sock)))
                         (when (and (or (stringp form) (symbolp form))
                                    (member form *cl-server-quit*
                                            :test #'string-equal))
                           (return))
                         (format sock "~{~a~^ ;~%~}~%"
                                 (multiple-value-list (eval form))))
                     (error (co)
                       (format sock "error:~%~a~%...flushed...~%" co))))
             (format sock "goodbye~%")
             (cl-server-mesg sock "connection closed:")
             :else :do (format sock "wrong password~%")
             (cl-server-mesg sock "access denied [~a]:" pwd)
             :end :do (close sock))
      (socket-server-close serv))))

;;; cmucl/src/code/multi-proc.lisp
#+cmu
(defun start-lisp-connection-listener (&key (port 1025)
                                       (password (random (expt 2 24))))
  (declare (type (unsigned-byte 16) port))
  "Create a Lisp connection listener, listening on a TCP port for new
connections and starting a new top-level loop for each. If a password
is not given then one will be generated and reported."
  (labels (;; The session top level read eval loop.
	   (start-top-level (stream)
             (unwind-protect
                  (let* ((*terminal-io* stream)
                         (*standard-input*
                          (make-synonym-stream '*terminal-io*))
                         (*standard-output* *standard-input*)
                         (*error-output* *standard-input*)
                         (*debug-io* *standard-input*)
                         (*query-io* *standard-input*)
                         (*trace-output* *standard-input*))
                    (format t "Enter password: ")
                    (finish-output)
                    (let* ((*read-eval* nil)
                           (read-password
                            (handler-case
                                (read)
                              (error () (return-from start-top-level)))))
                      (unless (equal read-password password)
                        (return-from start-top-level)))
                    (sysinfo)
                    (mp::top-level))
               (handler-case
                   (close stream)
                 (error ()))))
	   ;; The body of the connection listener.
	   (listener ()
	     (declare (optimize (speed 3)))
	     (let ((serv (open-socket-server port)))
	       (unwind-protect
		    (progn
		      (format t "~&;;; Started lisp connection listener on ~
 				  port ~d with password ~d~%"
			      port password)
		      (loop
		       ;; Wait for new connections.
                       (let ((sock (socket-accept serv)))
                         (make-process (format nil "Lisp session from ~a"
                                               (socket-string sock))
                                       #'(lambda () (start-top-level sock))))))
		 ;; Close the listener stream.
		 (when serv (socket-server-close serv))))))
    ;; Make the listening thread.
    (make-process (format nil "Lisp connection listener on port ~d" port)
                  #'listener)))

#|
In Linux 2.2.X with the binmisc module, any .x86f file is a binary file.

pvaneynd:~$ cat /usr/doc/cmucl/examples/Demos/register-lisp-as-executables.sh
#!/bin/sh
echo ':lisp:E::x86f::/usr/bin/lisp-start:'  >
/proc/sys/fs/binfmt_misc/register # this should only work for root
under linux 2.1.XX or later
# now you can do "chmod a+x hello.x86f" and
# ./hello.x86f
# from your favorite shell.

pvaneynd:~$ cat /usr/doc/cmucl/examples/Demos/lisp-start
#!/bin/sh
/usr/bin/lisp -load $1

There is even a demo of how to use a lisp-server to wait for
commands, so you avoid the startup-delay of cmucl...

pvaneynd:~$ cat /usr/doc/cmucl/examples/Demos/Start-up-server.lisp
(in-package :user)

(format t "THIS A A HUDGE SECURITY RISC. CHANGE THE PASSWORD!!!~%~%")
(setf mp::*idle-process* mp::*initial-process*)
(mp::start-lisp-connection-listener :port 6789 :password "Clara")
|#


(provide :cllib-server)
;;; file server.lisp ends here
