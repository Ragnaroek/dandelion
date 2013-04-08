#| spam filtering esmtp (extended simple mail transport protocol) demon 

Mail to don@isis.cs3-inc.com (then read the bounce).

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
====

This server is meant to be customized for each site.  The places where
I anticipate you'll want to change things are marked with ***.
Some of these require action (default won't do), so at least look.

This program is the demon that accepts (or rejects) incoming mail.  
It is written with these objectives in mind:
- prevention of various sorts of attacks, which is inherited from the 
  shared server in server.lsp on which this code relies.
- a rather aggressive sort of spam prevention.
- relative simplicity so you can understand and alter this code to do
  what you really want

This server uses another program that you supply to actually deliver 
the mail, e.g., append it to your mailbox file.  Currently I use sendmail 
for this.  See the section on delivery.  Convenient use requires some
cooperation from the software used to send mail.  In my case that's emacs.
I supply emacs code separately.

Some limitiations:
- See server.lsp for its limitations (currently only runs in a few lisp
  implementations)
- In order to accept mail for delivery this service must run as root on 
  port 25

Spam prevention strategy:

The general idea is that only those with permission can send you mail
directly.  Others get a bounce with a temporary address they can use
to send you one message.  This is expected to be not too much extra
trouble for someone who really wants to send you mail, but too much
for a spammer.  

The current scheme allows you to create new addresses for yourself.
Permission to send you mail is identified with knowing at least one
such address.  In effect, you're protected by spammers not knowing
these addresses.  You try to restrict each address to a small set of
correspondents to make it take longer to leak out to spammers, and 
also to be easy to change when it does (that is, a small number of
correspondents have to be notified).  Of course, there's also one 
"standard" address which is used only for rejecting mail with a one 
time password.

Originally, in order to make it easier for the mail receiving agent
these addresses were to take the form <userid>+<password>.  However,
US Patent 5,930,479 (Hall) includes as its first claim:
  ... wherein the address comprises a common address portion that
  indicates the identity of the recipient in the network and a channel 
  identifier portion for verifying that the message is authorized for 
  delivery to the recipient

Therefore I change that to what I regard as a better scheme in which
any address can map to any user.  Of course, the mail receiving demon
must still be able to determine a user from an address.  It therefore
needs a database of sorts, and users must not be allowed to claim 
overlapping sets of addresses.

Within the set of addresses that map to a given user, the user has
control over which are valid and for how long.  The costs are
- creating and managing the different addresses
This is not difficult, but it is more to think about when you send mail.
- distributing them to those you want to use them
I hope this will not be done very often but you may end up sending mail 
to inform a large group of a change in your address.  Or you can just
let those people use the old address and get one time addresses, then
reply to their resends with the new address.

Like normal addresses, the new ones will tend to become more and more
widely distributed as people who get them send them to others.  That is,
X sends mail to Y using "from" address Ax.  This allows Y to reply to X.
However Y might cc Z in that reply.  Now Z has Ax and can use it to send
mail to X without X having given him explicit permission.

You (and your mail sending program) maintain a database of addresses
that allow those who know them to send mail to you.  These may have
expiration dates or may be permanent (until explicitly removed).
Typically you send mail from an address that is at least valid temporarily
so that the recipient can reply, or at least other mail transport agents 
can return error messages.  You use permanent addresses for people or
groups you correspond with regularly.  To subscribe to a mailing list you
create an address for the list and use that address to subscribe to
the list.  If that address becomes used for spam you simply change it 
(and change your subscription address for the mailing list).  Note that 
one person may have several addresses that he can use to send you mail.

The case that is not covered above is that someone you don't know wants
to send mail to you.  It might be spam, of course, but it might not.
Our strategy causes a small amount of inconvenience for this person.
He sends a message to your "normal" address.  The mail is not accepted.
However, along with the "bounce" comes a temporary one-time password that 
he can use to send you one message.  When you get that message, of course,
you can send a permanent address to this person if you choose.

The implicit assumption is that spammers won't bother to read the bounce
message and resend to the temporary address.  If they do then they're at
least taking more trouble to send you the message than you take to read
it.  A long term problem is that, should this software become widely used,
spammers might eventually try to automatically process the bounce message.
In that case I propose to alter the bounce message, providing different
instructions from different places, including different descriptions of
how to use the one time password.  So one place might send a number and
the instructions would say to add 7 to that to get your temporary password.

A previous version used a database of email addresses and ip addresses
to decide which messages to accept.  One problem was variability in this
data that was hard to predict.  Mail from a big place like aol or yahoo 
may come from any of a large number of machines, and there's no easy way 
to determine the set of addresses ahead of time.  Similarly, some mailing 
list mail ends up coming from large numbers of addresses.  In some cases
it appears to come from the mailing list member, and you certainly don't
want to maintain the mailing list in your .ac file.  An even more serious 
problem is the fact that error replies indicating delivery problems tend
to get lost with this solution.  These tend to appear to come from a few
standard addresses, such as MAILER-DAEMON or <>, but I've also received
spam from those addresses!  And limiting IP addresses from which you 
accept these messages is a bad solution because your mail may be forwarded
to places you never heard of before the error is discovered and reported.

The code below still supports this mechanism.  It does allow you to refuse 
mail (without offering a one time password) from certain addresses, and
this could conceivably be useful, though spammers seem to have no shortage
of addresses and IP addresses.  In case you really want to use it, I point 
out that a mode of :training accepts all mail and adds entries to the 
.smtp.ac file described below.

Each user has his own set of files which this server uses in order to 
decide what to do.  These files and their contents are described below.
The actual places where these files reside are adjustable, but the 
expectation is that they will be in the user's home directory.

file .smtp.cf - a configuration file containing some of the data below

;; The server does one lisp READ in order to read this file.
((:expire (7 0 0 0))
 ;;  how long one time passwords last - default is 7 days
 ;;  Time differences are represented as lists of the form 
 ;;  (days hours minutes seconds) where shorter lists are
 ;;  treated as if zeros were added to the end.
 ;;  I don't allow years or months cause of questions about the meaning
 ;;  across months of different length and leap years, e.g., what is 
 ;;  one month and 2 days past 2000/1/30  
 (:mode :off) ;; :off, :on, or :training
 ;; off means always deliver (default)
 ;; on means the behavior described above
 ;; training accepts all mail and adds entries to the .smtp.ac file
 (:password-limits (32 2)(24 4)(16 8)(8 16)(0 32))
 ;; these are the default values
 ;;  The server records the IP addresses of connections to which
 ;;  it sends one time passwords.
 ;;  This limits the number of entries in the one time password file.
 ;;  After we get, e.g., 2 passwords stored for one IP address or
 ;;  4 for multiple ip addresses that have the same first 24 bits,
 ;;  the next one will be shared among all such addresses.
 ;;  Currently only these 5 values (0, 8, 16, 24, 32) are used.
 ;;  Whenever a password is used it is removed, so if there really are
 ;;  4 people from one host trying to send you mail, then the last two
 ;;  will share a password that can only be used by one.  After the 
 ;;  first one uses it the second will find that it no longer works,
 ;;  and will get another password.  That means that you can get 
 ;;  repeated failures if you wait too long.  So it's best to check your
 ;;  mail shortly after sending and respond with the password right away. 
)

File .smtp.adr 

;; The server does one lisp READ in order to read this file.
;; This file is the address database maintained by the user (and his
;; mailer).  It contains a list of entries of form:
;;   (address expiration to-string) 
;; Addresses are strings that could be legal local addresses.
;; Expiration dates are either NIL, meaning no expriration, or lists of
;; numbers.  The general form of an absolute time is
;; (year month day hour minute second) where shorter lists are
;; treated as if zeros were added to the end.
;; Absolute times are always interpreted as GMT (so computers can move).
;; The address strings are to be used by mailer interface software.
(("don-fred-cc" nil "fred@all.net
cc: don") ;; a permanent entry
 ("Don13327756" (2000 11 12 13) "") ;; temporary 
 ("cmc-list-2001" nil "cmc-mail@onelist.com") ;; mailing list
)

File .smtp.pw

;; This is the one time password file.  It is written by the server and
;; generally should not be altered by the user.
;; It contains entries of the form
;; <ip-addr> <sender> <expire-time> <password>
;; The ip-addr is used to avoid filling the password file with entries
;; from the same places.  See :password-limits above.

File .smtp.ac

;; I leave this in case it turns out to be still useful.
;; It is written by the server only when in training mode.
;; It is read, as usual, with one lisp READ.
(;; again lisp syntax, one form containing a list
 ;;  of entries of form (ip-addr-list mail-addr-list), as in
 ((1 2 3 T) ("sender" "foo" "com")) ;; ip address, from address,
 ;; or (ip-addr-list mail-addr-list :accept), which means the same thing
 ((1 2 3 T) ("sender" "foo" "com") :accept)
 ;; or (ip-addr-list mail-addr-list :reject) meaning do NOT accept mail
 ((5 6 7 8) ("sender" "foo" "com") :reject) 
 ;; T matches anything 
 ;; mail addresses are compared to userid then the tail of the domain
 ;; ("sam" "foo" "com") and  ("sam" "foo" T) both match sam@bar.baz.foo.com
 ;; ip addresses are lists of up to 4 numbers.  Missing entries act like T.
 ;;  (1 2 T 3) and (1 2) both match, 1.2.35.3
 ;; user id and domains are case insensitive
 ;; You can edit this file in order to allow people to send to you.
)

and finally, 
File .smtp.deliver
If this file exists it is expected to be an executable program
(from the shell) for delivering your mail.

Design of the mailer interface

The program with which the user sends mail should read and write the 
.smtp.adr file.  I expect it to provide operations that add, delete
and edit the entries.  Further, I expect that the addresses will be
used to refer to the corresponding to-strings, much as one would
use the name of a mailing list.  In particular there will be a command
that causes the message being composed to be sent to the to-string
corresponding to a given address, with the mail being sent from that
address.  I propose a command that allows you to 
choose an existing address and inserts headers: 
    from: <address>
    to: <to-string>
e.g., if the .smtp.adr file contains 
    ("fred-swamy" nil "fc@all.net, swamy@cinenet.net")
and you ask to send to fred-swamy, the following is inserted.
    from: fred-swamy
    to: fc@all.net, swamy@cinenet.net
The to-string can contain returns, so it could even contain cc: etc.
The command above can be used more than once for the same message.

There will be a new command to check the addresses.  First, it will
check that there are no recipients before the first from line.  Then
it will check that all the from lines contain valid addresses.

The command to actually send the mail will be altered to first do the
check above.  If that's ok, it will send to each recipient from the 
address given on the preceding from line.  Only the one appropriate
from line will be in the message delivered to each user.

|#

(in-package :user)

;; why does this give me trouble?
(eval-when (compile eval load) (require :sss "server")) ;; shared socket server

(defpackage "ESMTP" (:use "COMMON-LISP")
	    (:export
	     "*LOG*"   ;; name of a log file
	     "*MYDOMAIN*"  ;; string that identifies this domain
	     "*PASSWORD-CHAR*" ;; character described below
	     "USER-FILE"  ;; where user config files are found
	     "*TRAINING-RECORD-IP*" ;; controls what to record in .ac
	     "REPORT-PW"  ;; controls bounce message
	     "*USER-TRANSLATIONS*" ;; alist described below
	     "TRANSLATE-USER"  ;; function that uses above
	     "TEMP-MAIL-LOCATION" ;; where to write temp files
	     "*DEFAULT-DELIVER*"  ;; how to deliver mail
	     "*TMP-ERR-FILE*"  ;; temp file for delivery error messages
	     ))

(in-package :esmtp)

;; ==== time utilities shared by this and emacs code ====

;; absolute times are lists of up to 6 numbers (y m d h m s) 
;; interpreted as GMT
;; shorter lists are interpreted by adding defaults to the end,
;; where the defaults are (0 1 1 0 0 0) - months and days start at 1
;; time differences are lists of up to 4 numbers (d h m s)
;; shorter lists are interpreted as if zeros were added at the end
;; We don't support y,m cause the meaning is unclear:
;;  What is one month and 2 days past 2000/1/30 ?

(defun extend-time-difference (td)
  ;; add up to 4 zero's to end
  (append td (nthcdr (length td) '(0 0 0 0))))

(defun extend-absolute-time (td)
  (append td (nthcdr (length td) '(0 1 1 0 0 0))))

#+ignore ;; emacs lisp only
(defvar two16 (expt 2 16)) 
#+ignore 
(defun emacs-time+ (abs diff)
  ;; encode accepts tz but decode does not
  ;; not supplying tz to encode uses the same one as decode
  ;; which gives me the right result even though the intermediate
  ;; result is wrong
  (let ((wrong-ut (apply 'encode-time (reverse (extend-absolute-time abs)))))
    (loop for x in (extend-time-difference diff)
	as y in '(86400 3600 60 1) do (incf (cadr wrong-ut) (* x y)))
    (incf (car wrong-ut) (floor (cadr wrong-ut) two16))
    (setf (cadr wrong-ut) (mod (cadr wrong-ut) two16))    
    (nthcdr 3 (reverse (decode-time wrong-ut)))))

(defun cl-time+ (abs diff)
  (let ((wrong-ut (apply 'encode-universal-time
			 (reverse (extend-absolute-time abs)))))
    (loop for x in (extend-time-difference diff)
	as y in '(86400 3600 60 1) do (incf wrong-ut (* x y)))
    (nthcdr 3 (reverse (multiple-value-list
			(decode-universal-time wrong-ut))))))

#+ignore 
(defun emacs-current-gmt ()
  (let ((wrong-ut (current-time)))
    (decf (cadr wrong-ut) (car (current-time-zone)))
    (incf (car wrong-ut) (floor (cadr wrong-ut) two16))
    (setf (cadr wrong-ut) (mod (cadr wrong-ut) two16))
    (nthcdr 3 (reverse (decode-time wrong-ut)))))

(defun cl-current-gmt ()
  (nthcdr 3 (reverse (multiple-value-list
		      (decode-universal-time (get-universal-time) 0)))))

(defun abs-time-< (at1 at2)
  "compare two absolute times"
  (number-list< (extend-absolute-time at1) (extend-absolute-time at2)))

(defun number-list< (l1 l2)
  (loop for x in l1 
    as y in l2 do
    (if (< x y) (return t)
      (if (> x y) (return nil)))))

;; ==== debugging ====
(defmacro ignore-errs (form)
  ;; use the one in sss now
  `(sss::ignore-errs ,form))

;; Much of this is explained in sss 

(pushnew 25 sss:*ports*) ;; smtp listens on port 25

(defclass smtp-connection (sss:connection)
  ())

(defmethod sss:connection-class ((port (eql 25))) 'smtp-connection)

(defun print-current-time (&optional (stream *standard-output*))
   (multiple-value-bind
    (second minute hour day month year)
    (get-decoded-time)
    (format stream "~@?" "~d/~d/~d ~2,'0d:~2,'0d:~2,'0d"
	    month day year hour minute second)))

(defvar *log* nil) ;; log file [***]
;; If you want to log the inputs, set *log* to the name of a log file
;; Naturally this file should belong to root and users shouldn't
;; be able to change it to arbitrary symlinks.
(defun logform (string &optional (show-connection t))
  (if (stringp *log*)
    (with-open-file (log *log* :direction :output
		     :if-does-not-exist :create :if-exists :append)
      (terpri log) (print-current-time log)
      (when show-connection (show-connection log))
      (format log "~% ~A" string))
    (progn (terpri) (print-current-time)
	   (when show-connection (show-connection))
	   (format t "~% ~A" string))))

(defun sss:log-err (string) ;; redefine from server
  (logform string nil))

(defun show-connection (&optional (stream t) &aux c)
  (declare (special sss::connection reverse-path forward-path))
  ;; sss::connection is special bound by process-connection
  (setf c sss::connection)
  (ignore-errors ;; in case no longer connected
    (format stream " ~A" (list-bytes (sss:connection-ipaddr c))))
  ;; look at bindings rather than just variables
  ;; in case they were bound by this invocation of process-connection
  (let ((b (assoc 'reverse-path (sss:bindings c))))
    (when b (format stream " from ~A" (cdr b))))
  #+ignore ;; actually, don't show forward path for every message
  (let ((b (assoc 'forward-path (sss:bindings c))))
    (when b (format stream " to ~A" (cdr b)))))

(defvar *mydomain*)
;; I see no general way to set this - have to leave it to user
;; *** after loading this file, something like
;; (setf esmtp:*mydomain*
;;       (read-line (run-shell-command "hostname" :output :stream)))

(defvar *tmp-err-file* "/root/sendmail-error")
;; *** the file where we put error messages from delivery
;; (Since we only deliver one at a time we can get away with one file.)
;; Since we write this as root, it should belong to root and
;; no user should be able to e.g., change it to a symlink.

(defvar crlf (format nil "~C~C" #.(code-char 13) #.(code-char 10)))

(defun crlf (con)
  (sss:send-byte-vector con #(13 10)))

(defun crlf+ (con)
  (crlf con)
  (sss:process-output con))

;; since all my sends seem to be followed by crlf's ...
(defun send-string-crlf (con string)
  (sss:send-string con string) (crlf con))
(defun send-string-crlf+ (con string)
  (sss:send-string con string) (crlf+ con))

(defmethod sss:accept-p ((connection smtp-connection))
  (let ((complaint (sss:reason-not-to-accept connection)))
    (if complaint
	(progn (send-string-crlf
		connection
		(format nil "421-~A Service not available" *mydomain*))
	       (send-string-crlf+ connection (format nil "421 ~A" complaint))
	       (logform (format nil "error 421 - connection not accepted: ~A"
				complaint))
	       nil)
	(progn (send-string-crlf+
		connection
		(format nil
			"220 ~A ESMTP Don's anti-spam mail demon - ~A"
			*mydomain* (date)))
	       t))))
;; no shut-down-connection method - just use the default that closes it
;; we have to be sure to send the last message from elsewhere

(defmethod sss:max-input-size ((connection smtp-connection)) 300000) ;; ***
;; I'll set max-input-size to .3M and advertise as a max length
;; something a little less.

;; I'll also use the default one minute limit but have to send a close 
;; message; timing suggests that for 10^6 chars we need ~ 2.5 minutes ;;***
(defmethod sss:connection-close-p ((connection smtp-connection))
  (when (> (get-universal-time)
	   (+ 60 (slot-value connection 'sss:creation-time)))
    (send-string-crlf+ connection "451 time limit exceeded")
    ;; this method called outside server binding of sss:connection
    (let ((sss:connection connection)) (declare (special sss:connection))
	 (logform "error 451 - time limit exceeded"))
    t))
(defmethod sss:max-input-reached ((connection smtp-connection))
  (send-string-crlf+ connection "552 length limit exceeded")
  (logform "error 552 - length limit exceeded")
  (call-next-method)) ;; disconnects

(defvar clrf-dot-crlf (concatenate 'string crlf "." crlf))
(defmethod sss:reader ((c smtp-connection) string start)
  ;; Everything of interest ends in crlf
  (if (boundp 'data)
      ;; too inefficient to do the whole loop for every line of data
      (if (and (>= (length string) (+ start 3))
	       (search clrf-dot-crlf string :start1 2
		       :start2 start :end2 (+ start 3)))
	  (values "" (+ start 3)) ;; leave off the final dot line
	(let ((pos (search clrf-dot-crlf string :start2 start)))
	  (when pos (values (subseq string start (+ pos 2)) (+ pos 5)))))
    (let ((pos (search crlf string :start2 start)))
      (when pos (values (subseq string start (+ pos 2)) (+ pos 2))))))

#| 
     MUST insert a
         "Received:" line at the beginning of a message.  In this line,
         called a "time stamp line" in RFC-821:

         *    The FROM field SHOULD contain both (1) the name of the
              source host as presented in the HELO command and (2) a
              domain literal containing the IP address of the source,
              determined from the TCP connection.

         *    The ID field MAY contain an "@" as suggested in RFC-822,
              but this is not required.

         *    The FOR field MAY contain a list of <path> entries when
              multiple RCPT commands have been given.

         When the receiver-SMTP makes "final delivery" of a message,
         then it MUST pass the MAIL FROM: address from the SMTP envelope
         with the message, for use if an error notification message must
         be sent later (see Section 5.3.3).

         the case of an empty path:  "MAIL FROM: <>" (see RFC-821 Page
         15).  An empty reverse path MUST be supported.

         The syntax for the date is hereby changed to:
            date = 1*2DIGIT month 2*4DIGIT
|#

(defmethod sss:evaler ((c smtp-connection) string)
  ;; string is a line of input ending with crlf
  ;; The most common case (the one we should optimize) is in the middle of data
  (if (boundp 'data) (moredata c string)
      (macrolet ((cmd (cmd)
		   `(and (>= (length string) 4)
			 (string-equal ,cmd string :end2 4))))
      (cond ((cmd "HELO") (helo c string))
	    ((cmd "EHLO") (ehlo c string))
	    ((cmd "MAIL") (mail c string))
	    ((cmd "RCPT") (rcpt c string))
	    ((cmd "DATA") (data c string))
	    ((cmd "SEND") (send c string))
	    ((cmd "SOML") (soml c string))
	    ((cmd "SAML") (saml c string))
	    ((cmd "RSET") (rset c string))
	    ((cmd "VRFY") (vrfy c string))
	    ((cmd "EXPN") (expn c string))
	    ((cmd "HELP") (help c string))
	    ((cmd "NOOP") (noop c string))
	    ((cmd "QUIT") (quit c string))
	    ((cmd "TURN") (turn c string))
	    (t (other c string))))))



(defun other (connection string)
  (logform (format nil "error 500 - unknown command: ~A" string))
  (send-string-crlf+ connection "500 Syntax error, command unrecognized"))

(defun send (connection string)
  (declare (ignore string))
  (logform "error 502 - send command not implemented")
  (send-string-crlf+ connection "502 Command not implemented"))

(defun saml (connection string)
  (declare (ignore string))
  (logform "error 502 - saml command not implemented")
  (send-string-crlf+ connection "502 Command not implemented"))

(defun soml (connection string)
  (declare (ignore string))
  (logform "error 502 - soml command not implemented")
  (send-string-crlf+ connection "502 Command not implemented"))

(defun turn (connection string)
  (declare (ignore string))
  (logform "error 502 - turn command not implemented")
  (send-string-crlf+ connection "502 Command not implemented"))

(defun help (connection string)
  (declare (ignore string))
  (send-string-crlf+ connection "214 see rfc 821, 1123, 1869, 1870"))

#| The state of a connection is held by the following variables:
data - if bound we are collecting data lines by pushing them onto data
domain - the domain name sent in the last helo/ehlo command
domain-ip - the ip address from which the connection came
reverse-path - the path supplied in the mail command
forward-path - list of paths accepted so far in rcpt commands
 2003 - change this to list of pairs (translated-addr . input-addr)
 so I can report the input-addr in the delivered header
These are all unbound if there has been no helo/ehlo or if there
has been an helo/ehlo since they were last set
|#

(defun ehlo (c string) (helo c string))

(defun helo (connection string)
  (when (boundp 'domain)
    ;; I want to allow only one helo/ehlo per session so that the
    ;; limit on time/chars will translate properly into length limit
    (send-string-crlf connection "421-Service not available")
    (send-string-crlf+ connection
		       "421 only one message per session supported here")
    (logform "error 421 - helo not accepted - only one message/connection")
    (return-from helo nil))
  (unless (eql #\space (char string 4))
    (send-string-crlf+ connection "501 Syntax error in parameters or arguments")
    (logform (format nil "error 501 - helo got ~A" string))
    (return-from helo nil))
  (let ((arg (string-trim " " (subseq string 4 (- (length string) 2)))))
    (when (= 0 (length arg))
      (send-string-crlf+ connection
			 "501 Syntax error in parameters or arguments")
      (logform (format nil "error 501 - helo got ~A" string))
      (return-from helo nil))
    (sss:bind 'domain-ip (sss:connection-ipaddr connection))
    ;; would have done that earlier, but have to be in an evaler
    (sss:bind 'domain arg)
    (sss:bind 'forward-path nil)
    (send-string-crlf
     connection
     (format nil "250-~A ~A Requested mail action okay, completed"
	     *mydomain* (date)))
    (send-string-crlf+
     connection
     (format nil "250 SIZE ~A" (- (sss:max-input-size connection) 1000)))
    ;; figure 1000 chars should be enough for all commands
    ))

(defun mail (connection string)
  ;; MAIL From:<don@baja.nichimen.com> SIZE=47
  ;; accept send MAIL<sp>From:<sp>...  (in violation of spec !@#$%)
  (unless (boundp 'domain)
    (send-string-crlf+ connection "503 you need to start with helo/ehlo")
    (logform "error 503 - mail: need to start with helo/ehlo")
    (return-from mail nil))
  (unless (eql #\space (char string 4))
    (send-string-crlf+ connection "501 Syntax error in parameters or arguments")
    (logform (format nil "error 501 - mail got ~A" string))
    (return-from mail nil))
  (unless (and (>= (length string) 10)
	       (string-equal "MAIL FROM:" string :end2 10))
    (send-string-crlf+ connection
		       "501 Syntax error in parameters or arguments")
    (logform (format nil "error 501 - mail got ~A" string))
    (return-from mail nil))
  (let* ((args (string-trim " " (subseq string 10 (- (length string) 2))))
	 (space (position #\space args))
	 (arg1 (subseq args 0 space))
	 (arg2 (when space (string-trim " " (subseq args space)))))
    (when (= 0 (length arg1))
      (send-string-crlf+ connection
			 "501 Syntax error in parameters or arguments")
      (logform (format nil "error 501 - mail got ~A" string))
      (return-from mail nil))
    (when arg2 ;; rfc 1870
      (unless (and (>= (length arg2) 5) (string-equal "SIZE=" arg2 :end2 5))
	(send-string-crlf+ connection "504 Command parameter not implemented")
	(logform (format nil "error 501 - mail expected size, got ~A" string))
	(return-from mail nil))
      (unless (integerp (setf arg2
			  (ignore-errs
			   (let (*read-eval*)
			     (read-from-string arg2 nil nil :start 5)))))
	(send-string-crlf+ connection
			   "501 Syntax error in parameters or arguments")
	(logform (format nil "error 501 - mail expected size, got ~A" string))
	(return-from mail nil))
      (when (> arg2 (- (sss:max-input-size connection) 1000))
	(send-string-crlf+ connection "552 message too long")
	(logform (format nil "error 552 message too long: ~A" string))
	(return-from mail nil)))
    (let ((rp arg1))
      ;; illegal return path can mess us up at delivery time
      (unless (> (length rp) 2) (setf rp "<nobody>")) ;; <>
      (unless (and (eql (char rp 0) #\<) (eql (char rp (1- (length rp))) #\>))
	(logform (format nil "replaced bad return path: ~A" rp))
	(setf rp "<bad-return-path>"))
      ;; rfc822 has a long spec of legal addresses containin words which can
      ;; contain quoted strings with almost any character following a \
      ;; For now I'll accept only the most obvious characters and replace
      ;; any weird ones
      ;; things that would mess up shell include tab, ?, * ...
      (loop for i from 1 below (1- (length rp))
	  unless (alphanumericp (char rp i))
	  unless (position (char rp i) "@.-_") ;; chars that are allowed
	  do (setf (char rp i) #\_))
      ;; multiple mail commands can be used in one session
      ;; on each one we have to start over with a new forward path
      (sss:bind 'forward-path nil)
      (sss:bind 'reverse-path rp))
    (send-string-crlf+ connection "250 ok")))

(defvar *PASSWORD-CHAR* #\+) ;; ***
;; this char is not to appear in legitimate addresses on your system
;; its presence indicates a one time password

(defun rcpt (connection string)
  ;; RCPT To:<root@we-24-130-53-144.we.mediaone.net>
  ;; also accept RCPT<sp>To:<sp> ... 
  (declare (special domain-ip reverse-path forward-path))
  (unless (and (boundp 'domain) (boundp 'reverse-path))
    (send-string-crlf+ connection
		       "503 you need to start with helo/ehlo then mail")

    (logform "error 503 - rcpt: need to start with helo/ehlo then mail")
    (return-from rcpt nil))
  (unless (eql #\space (char string 4))
    (send-string-crlf+ connection "501 Syntax error in parameters or arguments")
    (logform (format nil "error 501 - rcpt got ~A" string))
    (return-from rcpt nil))
  (let ((arg (string-trim " " (subseq string 4 (- (length string) 2))))
	to pos user password cf-data ac-data adr-data)
    (when (= 0 (length arg))
      (send-string-crlf+ connection
			 "501 Syntax error in parameters or arguments")
      (logform (format nil "error 501 - rcpt got ~A" string))
      (return-from rcpt nil))
    (cond
     ((and (>= (length arg) 4) (string-equal "TO:<" arg :end2 4))
      (setf to (subseq arg 4 (position #\@ arg))))
     ((and (>= (length arg) 5) (string-equal "TO: <" arg :end2 5))
      (setf to (subseq arg 5 (position #\@ arg))))
     (t
      (send-string-crlf+ connection
			 "501 Syntax error in parameters or arguments")
      (logform (format nil "error 501 - rcpt got ~A" string))
      (return-from rcpt nil)))
    ;; and now begins the fun ...
    (setf user (subseq to 0 (setf pos (position *PASSWORD-CHAR* to)))
	  password (and pos (subseq to (1+ pos)))
	  user (translate-user user)
	  cf-data (user-data user :cf)
	  adr-data (user-data user :adr)
	  ac-data (user-data user :ac))
    (let ((mode (ignore-errs (cadr (assoc :mode cf-data)))) accept t-acc err)
      (unless (or (eql mode :on) (eql mode :training)) (setf mode :off))
      (cond ((eql mode :off) (setf accept t))
	    ((multiple-value-setq (t-acc err)
	       (ignore-errs
		(loop for adr in adr-data thereis
		      (and (string-equal to (car adr))
			   (or (null (cadr adr)) ;; permanent
			       (abs-time-< (cl-current-gmt) (cadr adr)))))))
	     (setf accept t))
	    (err ;; log errors and accept
	     (logform (format nil "error in .adr test for ~A: ~A"
			      user err))
	     (setf accept t))
	    ((eql (multiple-value-setq (t-acc err)
		    (ignore-errs (test-accept ac-data)))
		  :accept)
	     (setf accept t))
	    ((eql t-acc :reject)
	     (send-string-crlf
	      connection
	      (format nil "554-It seems that ~A does not want your mail" user))
	     (logform (format nil "reject mail to ~A" to))
	     (setf accept nil))
	    (err ;; log errors
	     (logform (format nil "error in .ac test for ~A: ~A"
			      user err))
	     (setf accept t))
	    ((eql mode :training) ;; don't overwrite .ac on err
	     (unless err (add-accept user ac-data domain-ip reverse-path))
	     (setf accept t))
	    ((multiple-value-setq (t-acc err)
	       (test-password user password cf-data connection))
	     (setf accept t))
	    (err
	     (logform (format nil "error in .pw test for ~A: ~A"
			      user err))
	     (setf accept t))
	    (t (send-string-crlf+ connection "554 Sorry.")
	       (logform (format nil "password sent for ~A" to))
	       (return-from rcpt nil)))
      ;; &&& It would be worth while to check whether this is a legal user.
      ;; current solution: end file *user-translations* with ("" "unknown")
      (when (equal user "unknown")
	(logform (format nil "failed to translate to user: ~A" to))
	(setf accept nil))
      (when accept
	(logform (format nil "accept mail to ~A" to))
	(unless (member user forward-path :key 'car :test 'equal)
	  (sss:bind 'forward-path
		    (cons (cons (format nil "<~A~A" user
					(subseq arg (position #\@ arg)))
				to)
			  forward-path))))
      (if accept
	  (send-string-crlf+ connection "250 ok")
	(send-string-crlf+ connection "554 Sorry")))))

;; *** this may be redefined for your system
;; we use these values for file: "cf" "adr" "pw" "ac" "deliver"
;; This version allows for virtual users but requires a little
;; additional setup - search below for virtual user
(defun user-file (user file)
  (if (ext:probe-directory (format nil "/root/smtp/~a/" user))
       (format nil "/root/smtp/~a/.smtp.~a" user file)
    (format nil "/root/smtp/~a/.smtp.~a" "default" file)))
#+ignore ;; old version which stores user files in their home dir's  
(defun user-file (user file)
  (if (equal user "root")
      (format nil "/root/.smtp.~A" file)
      (format nil "/home/~A/.smtp.~A" user file)))

;; caches for data read from files 
;; (gethash user table) => (cons write-date data)
(defvar *cf-data* (make-hash-table :test 'equal))
(defvar *adr-data* (make-hash-table :test 'equal))
(defvar *pw-data* (make-hash-table :test 'equal))
(defvar *ac-data* (make-hash-table :test 'equal))

(defun user-data (user table)
  ;; table in (:cf :adr :pw :ac)
  (let (file userfile data)
    (ecase table
      (:cf (setf file "cf" table *cf-data*))
      (:adr (setf file "adr" table *adr-data*))
      (:pw (setf file "pw" table *pw-data*))
      (:ac (setf file "ac" table *ac-data*)))
    (setf userfile (user-file user file))
    (or (setf data (gethash user table))
	(setf (gethash user table) (setf data (list 0))))
    (if (probe-file userfile)
	(when (> (file-write-date userfile) (car data))
	  (multiple-value-bind (ans err)
	      (ignore-errs
	       (with-open-file (x userfile)
		 (setf data (cons (file-write-date userfile)
				  (let (*read-eval*) (read x)))
		       (gethash user table) data)))
	    ans ;; otherwise not used
	    (when err
	      (logform (format nil "error: (user-data ~A ~A) => ~A"
			       user table err))
	      (return-from user-data :error))))
	(setf (gethash user table) (setf data (list 0))))
    (cdr data)))

(defun list-bytes (unsigned32)
  (loop for i from 3 downto 0 collect
	(ldb (byte 8 (* 8 i)) unsigned32)))

(defun parse-addr (string)
  ;; expect <user@foo.bar.baz>
  (let ((at (position #\@ string)) dot ans (end (1- (length string)))) ;; >
    (when at
      (loop while
	    (setf dot (position #\. string :start at :end end :from-end t)) do
	    (push (subseq string (1+ dot) end) ans)
	    (setf end dot))
      (push (subseq string (1+ at) end) ans))
    (push (subseq string 1 (or at end)) ans) ;; <, > when no @
    ans))

(defvar *training-record-ip* nil) ;; ***
;; In training mode should we record the ip address from which we get mail?
;; The reason not to is that often the ip address varies a lot.
;; That's one reason to abandon the old approach.

;; writing user files - dangerous to just open and write cause
;; the user could, e.g., ln -s /etc/passwd .smtp.adr
(defun write-user-file (user filetype data
			     &aux (file (user-file user filetype)))
  (ignore-errors (delete-file file)) ;; possible error if not there
  (with-open-file (f file :direction :output
		     :if-does-not-exist :create :if-exists :error)
    (print data f))
  ;; make it readable by user
  (shell-command
   (with-standard-io-syntax ;; no #1= ...
     (format nil "chown ~A ~A; chmod 400 ~A" user file file))))

;; used only when in training mode
(defun add-accept (user data domain-ip reverse-path)
  (let ((newac
	 (append data
		 (list (list (and *training-record-ip* (list-bytes domain-ip))
			     (parse-addr reverse-path))))))
    (write-user-file user "ac" newac)))

(defun test-accept (data)
  ;; return :accept, :reject or nil to go on to the password check
  (declare (special domain domain-ip reverse-path))
  (loop for entry in data do
	(if (and (match-ip (car entry) (list-bytes domain-ip))
		 (match-from (cadr entry) (parse-addr reverse-path)))
	    (if (eql (caddr entry) :reject)
		(return-from test-accept :reject)
		(return-from test-accept :accept)))))

(defun match-ip (pattern-ip real-ip)
  (loop for p in pattern-ip as i in real-ip always
	(or (eql p t) (= p i))))
  
(defun match-from (pattern-addr addr)
  (and (or (eq (car pattern-addr) t)
	   (string= (car pattern-addr) (car addr)))
       (>= (length addr) (length pattern-addr))
       (loop for x in (reverse (cdr pattern-addr))
	   as y in (reverse (cdr addr))
	   always (or (eq x t) (string-equal x y)))))

(defun test-password (user password cf-data con)
  ;; In this case we return t/nil possibly after writing a new password 
  ;; file and sending stuff to the connection
  (declare (special domain domain-ip reverse-path))
  (let* (changed
	 (ip-bytes (list-bytes domain-ip))
	 (from (parse-addr reverse-path))
	 (pw-data (user-data user :pw))
	 (matchn (make-array 5 :initial-element 0))
	 ;; number of entries with same first n bytes
	 (now (get-universal-time))
	 (expire 
	  (let ((assoc (ignore-errs (cadr (assoc :expire cf-data)))))
	    (if (numberp assoc) assoc (* 7 24 3600)))) ;; default 1 week
	 limits cflimits)
    ;; validate the data even though nobody else should write it
    (let ((len (length pw-data)))
      (setf pw-data
	(loop for d in pw-data
	    when (and (listp d) (= (length d) 4)
		      (listp (car d)) (= (length (car d)) 4)
		      (loop for x in (car d) always (or (numberp x) (eq x t)))
		      (listp (cadr d))
		      (loop for x in (cadr d) always (or (stringp x) (eq x t)))
		      (numberp (third d)) (> (third d) now)
		      (stringp (fourth d)))
	    collect d))
      (unless (= (length pw-data) len) (setf changed t)))
    ;; first see if the password is listed
    ;; if so we don't care whether it matches ip-bytes or from
    (loop for d in pw-data when (equal password (fourth d)) do
	  (write-pw (remove d pw-data) user)
	  (send-string-crlf con "250-one time password accepted")
	  (return-from test-password t))
    ;; [optional] see whether we already have a password for this address
    (loop for d in pw-data when (equal (cadr d) from) do
	  (send-string-crlf con "554-You've already been told what to do")
	  ;; log-err ?
	  (report-pw user d con)
	  (when changed (write-pw pw-data user))
	  (return-from test-password nil))
    ;; inexact matches:
    ;; (member t (cadr d)) iff (eq t (cadr d)) iff (member t (car d))
    (loop for d in pw-data
	when (or (member t (car d)) (member t (cadr d)))
	when (and (match-ip (car d) ip-bytes)
		  (match-from (cadr d) from)) ;; match-from should be T
	do ;; we don't know whether this person already got the pw
	  (setf (third d) (+ now expire))
	  (write-pw pw-data user)
	  (report-pw user d con)
	  (return-from test-password nil))
    ;; no matches - count passwords for each nbytes
    (loop for d in pw-data 
	unless (or (member t (car d)) (member t (cadr d)))
	do (incf (aref matchn  (loop for x in (car d) as y in ip-bytes
				   while (= x y) count t))))
    (setf limits (make-array 5 :initial-element 0)
	  cflimits (cdr (assoc :password-limits cf-data)))
    (loop for i below 5 as nb in '(0 8 16 24 32)
	as default in '(32 16 8 4 2) do
	  (setf (aref limits i)
	    (let ((l (ignore-errs (cadr (assoc nb cflimits :test '=)))))
	      (if (numberp l) l default))))
    (loop for i below 5 when (>= (aref matchn i) (aref limits i)) do
	  (push (list
		 (loop for b in ip-bytes as j below 4 collect
		       (if (< j i) b t))
		 (list t) ;; user will always be wild
		 (+ now expire)
		 (new-password))
		pw-data)
	  (write-pw pw-data user)
	  (report-pw user (car pw-data) con)
	  (return-from test-password nil))
    ;; we can afford to give this guy his own password
    (push (list ip-bytes from (+ now expire) (new-password)) pw-data)
    (write-pw pw-data user)
    (report-pw user (car pw-data) con)
    (return-from test-password nil)))

;; *** You might want to change the output
;; so as to make it harder for a program to construct the one time address
(defun report-pw (user data con)
  ;; data is the password entry - ip list, return-path list, expire, password
  (send-string-crlf con "554- ")
  (send-string-crlf
   con (format nil "554-  This is the spam filter at ~A" *mydomain*))
  (send-string-crlf con "554-  For a more complete explanation see")
  (send-string-crlf con "554-   http://www.ap5.com/~donc/spamfilter.html")
  (send-string-crlf con "554-  Your message was sent (or forwarded) to")
  (send-string-crlf
   con (format nil "554-   ~A@~A" user *mydomain*))
  (send-string-crlf con "554-  This user does not accept unsolicited mail.")
  (send-string-crlf con "554-  However, ONE message will be accepted for ")
  (send-string-crlf con
   (format nil "554-  the address ~A~C~A@~A"
	   user *PASSWORD-CHAR* (fourth data) *mydomain*))
  (when (or (member t (car data)) (member t (cadr data)))
    (loop for m in
	  '("554- "
	    "554-  This address may be sent to others as well as you."
	    "554-  The first to use it will succeed and the rest"
	    "554-  will get another annoying message like this."
	    "554-  So better reply asap."
	    "554- ")
	  do (send-string-crlf con m)))
  (send-string-crlf
   con (format nil "554-  This one time address will expire at ~A"
	       (date (third data))))
  (send-string-crlf con "554- "))

(defun write-pw (data user)
  (write-user-file user "pw" data))

;; rfc821 except that rfc1123 says no 2 digit years
(defvar months '("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
		 "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))

(defun date (&optional (ut (get-universal-time)))
  (multiple-value-bind
      (second minute hour day month year)
      (decode-universal-time ut 0)
    (format nil "~a ~a ~a ~2,'0d:~2,'0d:~2,'0d UT"
	    day (nth (1- month) months) year hour minute second)))

(defun new-password ()
  ;; don't generate the same sequence of passwords every session
  ;; yeah, we could do this only once per session ...
  (setf *random-state* (make-random-state t))
  (format nil "~A" (random 999999)))

(defvar *user-translations* "/root/.smtp.translations") ;; ***
;; a file read by one lisp READ, a list of pairs of strings 
;; (<prefix> <userid>)
;; ("don" "donc") means any address starting with "don" is for user donc
;; note that order is critical - first match wins
;; end this list with ("" "unknown") - see comment above containing &&&
;; This is analogous to /etc/aliases.
;; Make it readable by all so anyone can check whether a password is his.
;; Also make sure that each userid (allowed to receive mail) maps to itself
;; so that unsolicited mail will be correctly translated.
;; rfc 1123 says we MUST support mailbox "Postmaster"
;; Note that I don't promise to deliver to Postmaster - then you could fill
;; my disk with mail to Postmaster.  I treat it just like any other mail.

;; *** you might want to change this whole function
(defvar translation-file-last-read 0)
(defvar translation-cache nil)
(defun translate-user (adr)
  (when (null (probe-file *user-translations*))
    (return-from translate-user "root")) ;; roots fault that it's not there
  (when (>= (file-write-date *user-translations*) translation-file-last-read)
    (setf translation-cache
      (ignore-errors (with-open-file (f *user-translations*) (read f)))
      translation-file-last-read (get-universal-time)))
  (or (loop for (prefix user) in translation-cache thereis
	    (and (<= (length prefix) (length adr))
		 (string-equal prefix adr :end2 (length prefix))
		 user))
      "root"))

(defun data (c string)
  (declare (ignore string))
  (unless (and (boundp 'domain) (boundp 'reverse-path))
    (send-string-crlf+ c "503 you need to start with helo/ehlo then mail")
    (logform "error 503 - data: need to start with helo/ehlo then mail")
    (return-from data nil))
  (send-string-crlf+ c "354 Start mail input; end with <CRLF>.<CRLF>")
  (sss:bind 'data nil))

(defun moredata (c string)
  (declare (special data))
  (deliver-msg c string)
  (sss:unbind 'data))

;; *** temp mail files are written here
;; the value returned should be writable by user (if it's a real user)
;; This version supports virtual users.  Search for virtual users below
(defun temp-mail-location (user)
  ;; wherever this returns, the user should be
  ;; able to delete his own files from there
  (if (ext:probe-directory (format nil "/root/smtp/~a/" user))
       (format nil "/root/smtp/~a/" user)
    (format nil "/root/smtp/~a/" "default")))
#+ignore ;; version that does not support virtual users
(defun temp-mail-location (user)
  ;; wherever this returns, the user should be
  ;; able to delete his own files from there
  (declare (ignorable user))
  ;; you might want this in the user's directory or ...
  (if (equal user "root")
      (format nil "/tmp/mail") ;; need a file the user can delete
      (format nil "/home/~A/" user)))

(defvar *default-deliver* "/root/.smtp.default-deliver")
;; *** program that delivers mail - see below

#|     Delivery

Each user can control what is done with arriving messages by putting
a shell script named .smtp.deliver in the same place as the other .smtp- 
files.  If there is such a file it is called as follows:
  su <user> -c "<deliver> <filename> <sender> <user>" &

Otherwise we expect *default-deliver* to name a file belonging to root
and that file is used as follows
  <deliver> <filename> <sender> <user>

This should return 0 to indicate success, and otherwise generate an error 
message on standard output.

In the case of user controlled delivery, the filename will be a file that
belongs to the user and this program does not delete that file.  Since the
program is called with & we do not even attempt to decide whether it 
succeeds or fails.  If it does nothing at all the user will simply collect
a set of mail files in the directory returned by temp-mail-location.

In the case of non-user-controlled delivery one shared file is used for all
recipients, so the default delivery program must not delete the file.  This
file is deleted after all the deliveries are done.

Here's the default delivery program I currently use:

[linux version]
 #! /bin/sh
 cat $1 | sendmail -i -N never -f $2 $3
[sun version]
 #! /bin/sh 
 cat $1 | /usr/lib/sendmail -i -o eq -f $2 $3 

The -N and -o mean not to send error notifications - these are already being
handled in the code below.

Another one I've tried:
 #! /bin/sh
 cat $1 | procmail -f $2 -Y -d $3

Procmail seems to exist in linux but not on suns (I don't know about others),
so that's one reason for choosing the second option.
An advantage of sendmail is that you then get to use your .forward, 
and, for that matter, you can still use procmail.
A disadvantage of sendmail is that you get an extra received: line which 
is a little confusing.  A private delivery program can do the same as
above, (well, you want to "rm $1"  afterwards), but the -f seems not to work 
right unless the user is listed as a "trusted" user in the sendmail.cf file.

Notice that sendmail to a local address does not go through our demon.

|#

(defun shell-command (string)
  (#+allegro excl::run-shell-command
   #+clisp ext:run-shell-command
   #-(or clisp allegro) (error "no implementation yet")
   string))

(defun write-mail (stream body)
  (declare (special data))
  ;; send each input line (minus any leading . as required by rfc821)
  ;; without the final crlf and then add the lf
  (loop for d in data do (write-line d stream))
  (let ((start 0) pos) ;; process body
    (loop while (setf pos (search crlf body :start2 start)) do
	  (when (eql (char body start) #\.) (incf start))
	  (loop for i from start below pos do
		(write-char (char body i) stream))
	  (terpri stream)
	  (setf start (+ pos 2)))))

(defun write-temp-mail (user body &aux file)
  ;; can fail if out of space
  ;; returns name of file
  (loop for i from 0 with ut = (get-universal-time) do 
	(when (> i 10) (error "can't find a temporary mail file name!?"))
	(setf file (format nil "~A/~A-~A" (temp-mail-location user) ut i))
	(ensure-directories-exist file)
	(ignore-errors
	 (with-open-file (f file :direction :output
			  :if-does-not-exist :create :if-exists :error)
	   (return nil) ;; leave loop with the file created
	   )))
  (with-open-file (f file :direction :output
		   :if-does-not-exist :error :if-exists :append)
    (write-mail f body))
  ;; Sunday 2005/12/04 - write it before allowing user to change it
  ;; otherwise he might change it to a symlink
  (shell-command
   (with-standard-io-syntax ;; no #1= ...
     (format nil "chown ~A ~A; chmod 400 ~A" user file file)))
  file)

(defun filecontents (file)
  (with-open-file (f file)
    (loop with line while (setf line (read-line f nil nil)) collect line)))

;; *** used for virtual user support
;; is this a "real" user - to whom we can actually deliver mail here
(defun real-user (user)
  (or (equal user "root")
      (ext::probe-directory (format nil "/home/~a/" user))))

#| virtual user support - added 2004/2
Instead of a user's customization files being in his home directory
we create a separate directory for each user who wants to customize
and one other "default" directory for other users.
All virtual users need their own customization directories.
Real users who customize should be able to write to their customization
directories.
What I do:
 mkdir /root/smtp # the user customization directories
 mkdir /root/smtp/default # the default customization directory
 mkdir /root/smtp/don # a non-default user directory
 chown don /root/smtp/don # which should be writable by the user
To add a virtual user, first add a line to the translation file such as
 ("feedback" "feedback") ;; the first is the address, the second the user
then create a directory as above
 mkdir /root/smtp/feedback
and at very least, give it a delivery file to say what to do with
the mail for that user, e.g. put this in /root/smtp/feedback/.smtp.deliver
 #! /bin/bash
 cat $1 | /usr/lib/sendmail -i -N never -f $2 don
 rm $1
|#

(defun deliver-msg (connection body)
  (declare (special domain domain-ip reverse-path forward-path data))
  (setf data (reverse data));; in transmission order
  ;; a few more things required by rfc821 
  (push (format nil "Received: FROM ~A ~A by ~A with ESMTP ; ~A"
		domain (list-bytes domain-ip) *mydomain* (date))
	data)
  (push (format nil "Return-Path: ~A" reverse-path) data)
  (let (shared-file deliver-file user file result err)
    (unwind-protect
	(loop for (fp . to) in forward-path do
	  (let ((data (cons (format nil "X-Sent-To: ~A" to) data)))
	    (declare (special data))
	    (setf user (subseq fp 1 (position #\@ fp)))
	    ;; interesting - if fp is user@some.other.place
	    ;; we assume incorrectly it's for user@here
	    ;; which has the benefit (I think) of NOT relaying
	    (setf deliver-file (user-file user "deliver"))
	    (if (and (probe-file deliver-file)
		     ;; 2004/02/26 virtual user support
		     (real-user user))
		(progn;; every such user gets his own copy
		  (setf file (write-temp-mail user body))
		  (shell-command
		   ;; su <user> -c "<deliver> <filename> <sender> <user>" &
		   (format nil "su ~A -c \"~A ~A ~A ~A\" &"
			   user deliver-file file
			   (string-trim "<>" reverse-path) user))
		  ;; user doesn't have permission to (delete-file file)
		  )
	      (progn
		(unless shared-file
		  (setf shared-file (write-temp-mail "root" body)))
		(setf result
		      (shell-command
		       ;; <default-deliver> <filename> <sender> <user> 
		       (format nil "~A ~A ~A ~A > ~A"
			       ;; 2004/02/26 virtual user support
			       (if (probe-file deliver-file) deliver-file
				 *default-deliver*)
			       shared-file (string-trim "<>" reverse-path)
			       user *tmp-err-file*)))
		(unless (= 0 result)
		  (setf err
			(cons (format nil "error delivering to ~A" fp)
			      ;; just in case the error file is empty
			      (append (filecontents *tmp-err-file*) err)))
		  (logform (format nil
				   "error in delivery: ~A ~A~
                                      ~% returned from~% ~A ~A ~A ~A"
				   result (car err)
				   *default-deliver*
				   shared-file
				   (string-trim "<>" reverse-path)
				   user)))))))
      (when (and shared-file (not err)) (delete-file shared-file)))
    (if err
	(loop for e in err do
	  (send-string-crlf connection (format nil "554- ~A" e))
	  finally (send-string-crlf+ connection "554 sorry"))
      ;; 451 => nothing, 550, 553 => remote protocol error, 
      ;; 554 => service unavailable
      (send-string-crlf+ connection "250 ok!"))))

(defun rset (c string)
  (declare (ignore string))
  (send-string-crlf+ c "250 ok, start again")
  ;; (sss:unbind 'domain) ;; nope, this stays!!
  (sss:bind 'forward-path nil) ;; not unbind
  (sss:unbind 'reverse-path)
  (sss:unbind 'data))

(defun vrfy (c string)
  (declare (ignore string))
  ;; changed Friday 2005/09/23 cause sourceforge insists on verifying
  ;; that sender accepts mail to postmaster
  (logform "(error) 252 - vrfy command not implemented")
  (send-string-crlf+ c "252 I pretend to accept everything"))

(defun expn (c string)
  (declare (ignore string))
  (logform "error 502 - expn command not implemented")
  (send-string-crlf+ c "502 I don't even support mailing lists "))

(defun noop (c string)
  (declare (ignore string))
  (send-string-crlf+ c "250 OK, fine.  Be that way."))

(defun quit (c string)
  (declare (ignore string))
  (send-string-crlf+ c (format nil "221 ~A - good bye" *mydomain*))
  (sss:done c))
