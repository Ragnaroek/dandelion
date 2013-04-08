#| http server using server.lsp

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

General instructions:
- understand and modify as appropriate both server.lsp and this file
- load (modified versions of) server.lsp and this file (or compiled versions)
  into lisp
- install customizations
  The places I expect people will want to customize are marked by ***.
- do (sss:start-servers)
- do (sss:serve)

As an example, try (in package :http)
- (defmethod get-method (c (p (eql :|/bin/query|)) data)
     (status c "HTTP/1.0 200 OK") 
     (sss:send-string c (format nil "/bin/query: ~S" data))
     (sss:done c))
- then try url http://<host>:8000/bin/query?p=foo+fum&q=0&r=17
- (defmethod post-method (c (p (eql :/example)) data)
     (status c "HTTP/1.0 200 OK") 
     (sss:send-string c (format nil "example: ~S" data))
     (sss:done c))
- (setf http:*server-root* "/tmp")
- install the following text in the file /tmp/sample.html
  <FORM METHOD="POST" action="EXAMPLE">
  <br>username:<INPUT TYPE="text" NAME="user">
  <br>password:<INPUT TYPE="password" NAME="pass">
  <br><input type="submit" name="submit" value="try it"></form>
- in your browser type in the url
  http://<your host>:<your port>/sample.html 
  This demonstrates the default get method.  When you fill in the form and
  press the "try it" button it demonstrates the post method.

Now replace the methods above by code that does what you want.

As the call to the status function suggests, the output of these methods
is more than the html to be displayed.  It includes the entire http
protocol.  You can also send back other headers.  See the do-get function 
for examples.  In that case you send something like
HTTP/1.0 200 OK<CRLF>Content-Type: text/html<CRLF><CRLF>
and then the html.  Or change the content type to text/plain or whatever.
If your function computes .gif images you can send image/gif.
And of course lots of other headers, e.g., Expires, Content-Length, 
Content-Encoding.  See rfc1945 for http/1.0, rfc2616 for http/1.1.

This is a (distant) descendant of a file with the following header:
 ;; simple web server
 ;; Copyright Franz Inc., 1997
 ;; Permission is granted to copy, modify, or use the code
 ;; in this file freely, provided the copyright notice is preserved.

I don't think that any code from that file actually appears here, but
even if it does not, I should at least point out that this is where I 
first saw code to act as a web server. 

|#

(in-package :user)

(eval-when (compile eval load) (require :sss "server")) ;; shared socket server

(defpackage "HTTP" (:use "COMMON-LISP")
	    (:export
	     "*SERVER-ROOT*"  ;; set these
	     "*LOG*"
	     "*MIME-TYPE-FILE*"

	     "STATUS"  ;; useful in replies
	     "CRLF"

	     "GET-METHOD"  ;; define methods of these
	     "POST-METHOD"
	     ))

(in-package :http)

;; for multiple servers you can make this an alist of (port . root) pairs
(defparameter *server-root* ;; *** location where files are found
    #+mswindows "c:/tmp/"
    #+unix "/tmp/"
    #-(or unix mswindows) (error "need to fill in server-root"))

(defun print-current-time (&optional (stream *standard-output*))
   (multiple-value-bind
    (second minute hour day month year)
    (get-decoded-time)
    (format stream "~@?" "~d/~d/~d ~2,'0d:~2,'0d:~2,'0d"
	    month day year hour minute second)))

(defvar *log* nil) ;; log file [***]
;; If you want to log the inputs, set *log* to the name of a log file
(defun logform (form)
  (when (stringp *log*)
    (with-open-file (log *log* :direction :output
		     :if-does-not-exist :create :if-exists :append)
      (with-standard-io-syntax (print form log)))))

;; used to limit the amount of junk in the log
(defun limit-print (x)
  (if (consp x) (cons (limit-print (car x)) (limit-print (cdr x)))
    (if (stringp x) (if (> (length x) 100)
			(concatenate 'string (subseq x 0 100) "...")
		      x)
      x)))

(defclass http-connection (sss:connection)
  ((state :accessor state :initform :initial)
   (meth :accessor meth :initform nil) ;; can't use method - in cl package
   (uri :accessor uri :initform nil)
   (protocol :accessor protocol :initform nil)
   (content-length :accessor content-length :initform nil)
   (boundary :accessor boundary :initform nil) ;; for multipart
   (headers :accessor headers :initform nil)
   (body :accessor body :initform nil)))

(pushnew 8000 sss:*ports*) ;; *** what port should we serve?
(defmethod sss:connection-class ((port (eql 8000))) 'http-connection) ;; ***
;; *** you can repeat the above two lines for multiple ports

#|
An http connection speaks HTTP/0.9, 1.0 or 1.1.
It can be in one of three states:

initial: it has no protocol, method or uri and is waiting for a CRLF.
The evaler then parses the start-line to get a method, uri, protocol.
If the protocol is HTTP/0.9 the evaluer also does the GET on the uri and
then we're done.  Otherwise we proceed to the next state.

header: we're collecting headers, in which case we again wait for CRLF.
The evaler parses the input to get either an empty line or a header.
If it gets a header, this is added to the headers and we remain in header 
state.  In the special case of a content-length header we also store the
value in a slot of the connection.  Another special case is a header of
form 
Content-type: multipart/form-data; boundary=...
This is is added to handle forms containing the FILE input type, which
have to be inside <FORM ENCTYPE="multipart/form-data" ...>
I claim to have made this work in one (my own) version of Netscape,
but I don't know whether it will work anywhere else.
Note: *** If you plan to use the file input type then you should
remember to change the max-input-size for http connections, e.g.,
(defmethod sss:max-input-size ((connection http-connection)) 500000)
or whatever size of files you wish to allow.

If we get an empty line then, depending on the method, the evaler
either executes the method (if GET) and then we're done, or (if POST),
moves to the next state.

body: we're collecting data for the body.  We're done when either of two
situations arise. (1) we get eof, (2) there's a content-length header that 
tells us how many bytes to read, and that many have been read.

At this point the method is executed and then, finally, we're done.
|#

(defvar crlf (format nil "~C~C" #.(code-char 13) #.(code-char 10)))

(defmethod sss:reader ((c http-connection) string start)
  (sss:dbg "http: reader state=~A, start=~a, string=~S"
	   (state c) start string)
  (multiple-value-bind (ans err);; since we have trouble with this ...
      (sss::ignore-errs 
       (multiple-value-list

	(cond ((and (eq (state c) :body) (content-length c)
		    (<= (+ start (content-length c));; extra crlf at end?
			(length string)))
	       ;; have to copy cause it's about to to be smashed
	       (values (subseq string start (+ start (content-length c)))
		       (+ start (content-length c))))
	      ((not (eq (state c) :body))
	       (let ((pos (search crlf string :start2 start)))
		 (when pos (values (subseq string start pos) (+ pos 2)))))
	      ((not (open-stream-p (sss:sstream c)));; got eof
	       ;; probably won't do much good though ...
	       (logform (list :http :reader (limit-print string) start))
	       (values (subseq string start) (length string))))))
    (when err (logform (list :http :reader (format nil "~a" err) start)))
    (apply 'values ans)))


(defmethod sss:evaler ((c http-connection) string)
  (multiple-value-bind
      (ans err)
      (sss::ignore-errs 

       (sss:dbg "http: evaler state=~A, string=~S" (state c) string)
       (cond ((eq (state c) :initial)
	      (multiple-value-bind (found method uri protocol)
		  (parse-http-command string)
		(if found
		    (sss:dbg "http: parse-http-command => ~A, ~A, ~A"
			     method uri protocol)
		  (sss:dbg "http: parse-http-command => not found"))
		(when found
		  (setf (meth c) method
			(uri c) uri
			(protocol c) protocol)
		  (if (equal protocol "");; only get is allowed ...
		      (do-get c)
		    (setf (state c) :header))))
	      "";; in case printer tries to do something
	      )
	     ((and (eq (state c) :header) (equal string ""))
	      (when (content-length c);; so we have to read a body
		(setf (state c) :body)
		(return-from sss:evaler ""))
	      (call-meth c)
	      "")
	     ((eq (state c) :header)
	      (parse-header c string)
	      "")
	     ((eq (state c) :body)
	      (setf (body c) string)
	      (call-meth c))
	     (t
	      (logform (list :http (print-current-time nil)
			     :unknown-state (state c)))
	      (sss:dbg "http: unknown state")
	      (status c "HTTP/1.0 500 I'm in an unknown state")
	      (sss:send-string c "the server is totally confused, sorry"))))
    (when err (logform (list :http :evaler-err
			     (format nil "~a" err) (limit-print string))))
    ans))

(defun parse-http-command (command)
  (let (cend ustart uend pstart pend)
    (setf cend
      (position #\space command)
      ustart
      (and cend (> cend 0)
	   (position #\space command :start cend :test-not #'eql))
      uend
      (and ustart (position #\space command :start ustart))
      pstart
      (and uend (position #\space command :start uend :test-not #'eql))
      pend
      (and pstart (position-if-not
		   (lambda (c) (or (alphanumericp c) (eql c #\/) (eql c #\.)))
		   command :start pstart)))
    (when ustart
      (values t
	      (subseq command 0 cend)
	      (subseq command ustart uend)
	      (if pstart (subseq command pstart pend) "")))))

(defun parse-header (c string)
  (let ((pos (search ": " string)))
    (if pos
	(let ((field (subseq string 0 pos))
	      (value (subseq string (+ pos 2))))
	  (push (cons field value) (headers c))
	  (sss:dbg "http: header ~A: ~A" field value)
	  (when (string-equal field "Content-Length") ;; C-l instead of C-L
	    (setf (content-length c)
	      ;; yeah, I know, not correct for Content-Length: (hello)123
	      (let (*read-eval* val)
		(setf val (sss::ignore-errs (read-from-string value)))
		(when (integerp val) val))))
	  (when (and (string-equal field "Content-type")
		     (search "multipart/form-data" value :test 'char-equal)
		     (setf pos (search "boundary=" value :test 'char-equal)))
	    (setf (boundary c) (subseq value (+ pos 9)))))
      ;; else do nothing?
      (sss:dbg "http: header? ~A" string)
      )))

(defun call-meth (c)
  (sss:dbg "http: method ~S" (meth c))
  (cond ((string-equal (meth c) "GET") ;; could be picky and use string=
	 (do-get c))
	#+ignore ;; anyone who wants this is welcome to implement it
	((string-equal (meth c) "HEAD") 
	 (do-head c))
	((string-equal (meth c) "POST")
	 (do-post c))
	(t (status c "HTTP/1.0 400 I don't know that method")
	   (sss:send-string c
			    (format nil "method ~A not supported" (meth c)))
	   (logform (list :http (print-current-time nil)
			  :unknown-method (meth c)))))
  (sss:done c))

(defun connection-local-port (c)
  #+clisp (socket:socket-stream-local (sss:sstream c))
  #+allegro (socket:remote-host (sss:sstream c))
  #-(or allegro clisp) (error "connection-local-port not yet implemented"))

#+ignore 
(defun dotted (ip)
  (format nil "~A.~A.~A.~A"
	  (ldb (byte 8 24) ip)
	  (ldb (byte 8 16) ip)
	  (ldb (byte 8 8) ip)
	  (ldb (byte 8 0) ip)))

(defun peer-ip-port (c)
  #+clisp
  (multiple-value-bind
      (ip port)
      (socket:socket-stream-peer (sss:sstream c) t)
    (format nil "~a:~a" ip port))
  #-clisp (error "no implementation for peer-ip-port")
  )

(defun do-get (c &aux (uri (uri c)) (pos (position #\? uri)))
  (logform (list :http (print-current-time nil)
		 (peer-ip-port c) :get (uri c)))
  (sss:dbg "http: get uri=~A" uri)
  (get-method c
	      ;; this is supposed to let you specialize on (eql :|/foo|)
	      (intern (subseq uri 0 pos) :keyword)
	      (when pos (parse-form-contents (subseq uri (1+ pos))))))

(defvar *mime-types* (make-hash-table :test 'equal))
;; in case there's no mime types file we'll put in a few common cases
(setf (gethash "html" *mime-types*) "text/html"
      (gethash "htm" *mime-types*) "text/html"
      (gethash "jpeg" *mime-types*) "image/jpeg"
      (gethash "jpg" *mime-types*) "image/jpeg"
      (gethash "gif" *mime-types*) "image/gif")

(defvar *mime-type-file* "/etc/mime.types") ;; ***
;; The "correct" way to adjust types is to change this file.
;; The file is interpreted as a sequence of lines.
;; Each line not starting with # contains strings separated by tabs or spaces.
;; The first is a mime type, the rest are file extensions.
;; I downcase the types (should be a noop?), compare extensions case insensitive.

(defun whitespace-p (x) (member x '(#\tab #\space)))
(defun read-mime-types ()
  (sss::ignore-errs ;; in case file is not there or unreadable or ...
   (with-open-file (f *mime-type-file*)
     (let (line type pos1 pos2)
       (loop while (setf line (read-line f nil nil)) do
	     (setf pos1 (position-if-not 'whitespace-p line))
	     (when (and pos1 (not (eql #\# (char line pos1))))
	       (setf pos2 (position-if 'whitespace-p line :start pos1))
	       (setf type (subseq line pos1 pos2)) ;; nil ok
	       (loop while (and pos2
				(setf pos1 (position-if-not
					    'whitespace-p line :start pos2)))
		   do (setf pos2 (position-if
				  'whitespace-p line :start pos1))
		      (setf (gethash (string-downcase
				      (subseq line pos1 pos2))
				     *mime-types*)
			(string-downcase type)))))))))
(read-mime-types)

;; *** you may want to define methods for get-method

(defvar wkdays '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar months '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;; as defined in rfc 1945 (http 1.0)
(defun rfc1123-date (&optional (ut (get-universal-time)))
  (multiple-value-bind
      (second minute hour day month year wkday)
      (decode-universal-time ut 0)
    (format nil "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT"
	    (nth wkday wkdays) day (nth (1- month) months)
	    year hour minute second)))

;; default method tries to find the file
(defmethod get-method (c path query &aux root file) 
  (declare (ignore path query))
  (setf root
    (if (listp *server-root*)
	(cdr (assoc (connection-local-port c) *server-root* :test '=))
      *server-root*))
  (setf file (merge-pathnames (pathname (subseq (uri c) 1)) root))
  (when
      (let ((probe (probe-file file)))
	(or (not probe)
	    (< (length (pathname-directory probe))
	       (length (pathname-directory root)))
	    (loop for x in (pathname-directory probe)
		as y in (pathname-directory root)
		thereis (not (equal x y)))
	    ;; also reject directories, since otherwise open errs
	    (null (pathname-name probe))))
    (logform (list :http :not-found file))
    (status c "HTTP/1.0 404 Not Found")
    (sss:send-string
     c (format nil "~A Not Found" (uri c))) ;; this to appear as body
    (sss:done c)
    (return-from get-method))
  (unless (equal "" (protocol c))
    ;; It looks like we have to guess at the content type ...
    (let ((ptype (pathname-type file)) ctype)
      (setf ctype (gethash (string-downcase ptype) *mime-types* "text/plain"))
      (sss:send-string c "HTTP/1.0 200 OK") (crlf c)
      (sss:send-string c (format nil "Content-Type: ~A" ctype))
      (crlf c)
      (sss:send-string c (format nil "Date: ~A" (rfc1123-date)))
      (crlf c)
      ;; without this clients tend to get everything twice
      (sss:send-string c (format nil "Expires: ~A"
				 (rfc1123-date ;; 5 min from now?
				  (+ (get-universal-time) 300))))
      (crlf c)
      (crlf c)))
  ;; my theory is that we can treat all data as binary here
  (sss:send-file c file)
  (sss:done c))

(defun do-post (c &aux (uri (uri c))
		       (args (parse-form-contents
			      (body c)
			      ;; see rfc 1521
			      (when (boundary c)
				(concatenate 'string "--" (boundary c))))))
  (logform (list :http (print-current-time nil)
		 (peer-ip-port c) :post uri
		 (limit-print args)))
  (sss:dbg "http: post uri=~A args=~A" uri (limit-print args))
  (post-method c
	       ;; this is supposed to let you specialize on (eql :|/foo|)
	       (intern uri :keyword) args))

;; *** you may want to define methods for post-method

;; default method simply fails
(defmethod post-method (connection uri data)
  (declare (ignore data))
  (status connection "HTTP/1.0 404 Not Found")
  (sss:send-string
   connection (format nil "the function ~A is not supported" uri))
  (logform (list :http :not-found uri)))

(defun status (con string)
  (sss:send-string con string) (crlf con) (crlf con))

(defun crlf (con)
  (sss:send-byte-vector con #(13 10)))

(defun parse-form-contents (contents &optional boundary)
  (multiple-value-bind
      (ans err)
      (sss::ignore-errs 
	(when boundary
	  (return-from parse-form-contents
	    (parse-form-contents-boundary contents boundary)))
	;; input values come in the pairs name=value, delimited by & name is a
	;; "name" specified in the HTML form value is the string input or
	;; selection by the user on this form special cases like ? and & are
	;; ignored in this parser.
	;; return a list of dotted pairs (("name" . "value") ....)
	(loop
	  with len = (length contents)
	  with start = 0
	  for sep = (position #\& contents :start start)
	  for end = (or sep len)
	  for varend = (position #\= contents :start start)
	  for sym = (subseq contents start varend)
	  for val = (subseq contents (if varend (1+ varend) end) end)
	  collect (cons sym (html-to-ascii val))
	  until (null sep)
	  do (setq start (1+ sep))))
    (when err (logform (list :http :parse-form-contents-err
			     (format nil "~a" err) contents boundary)))
    ans))

(defun html-to-ascii (string)
  ;; recover the real chars the user sent
  ;; translate + to space and %xx where xx is two hex digits
  ;;  to that character in hex
  (let* ((chars
	  (loop with pos = 0 while (< pos (length string)) collect
		(if (eql (char string pos) #\+)
		    (progn (incf pos) #\space)
		  (if (eql (char string pos) #\%)
		      (prog1 (code-char (parse-integer
					 string :start (+ pos 1)
					 :end (+ pos 3) :radix 16))
			(incf pos 3))
		    (prog1 (char string pos) (incf pos))))))
	 (ans (make-string (length chars))))
    (loop for i from 0 as c in chars do (setf (char ans i) c))
    ans))

(defun parse-form-contents-boundary (contents boundary)
  ;; it appears that the contents in this case are not encoded as above
  (let ((start 0) end hstart hend name nstart nend result)
    ;; I expect contents of form
    ;; <boundary> <crlf>
    ;;  (<headers> <data> <crlf> <boundary> <crlf>)*
    ;; where <headers> => (<header> <crlf>)* <crlf>
    ;; I gather from the two crlfs separating header from data that
    ;; there might be multiple headers, but I really only care about
    ;; one of the form
    ;; Content-Disposition: form-data; name="foo" ... 

    ;; start with leading boundary
    (setf start (search crlf contents :start2 start))
    (when (null start)
      (return-from parse-form-contents-boundary '(("error" . "no crlf"))))
    ;; we get some cases where contents is just crlf 2004/1
    (when (<= (length contents) 4)
      (return-from parse-form-contents-boundary nil))
    (unless (string= boundary (subseq contents 0 start))
      (push (cons "error" (format nil "wrong boundary: got ~A, expected ~A"
				  (subseq contents 0 start) boundary))
	    result)
      ;; expect the others to be the same as the first
      (setf boundary (subseq contents 0 start)))
    (incf start 2) ;; past crlf
    ;; (setf boundary (concatenate 'string boundary crlf))
  (loop while (setf end (search boundary contents :start2 start)) do
	(setf name "") ;; in case it's not in a header
	(setf hstart start)
	(loop while (> (setf hend
			 (or (search crlf contents :start2 hstart :end2 end)
			     0))
		       hstart)
	    do ;; handle header
	      (when (and (equal hstart ;; = no good cause search might be nil
				(search "Content-Disposition: form-data"
					contents :start2 hstart))
			 (setf nstart
			   (search "name=\"" contents :start2 hstart))
			 (setf nend (position #\" contents :start 
					      (+ nstart 6))))
		(setf name (subseq contents (+ nstart 6) nend)))
	      (setf hstart (+ hend 2)))
	;; leave out the crlf at both ends
	(push (cons name (subseq contents (+ hstart 2) (- end 2))) result)
	(setf start (+ end 2 (length boundary))))
  result))