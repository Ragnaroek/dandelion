;; sendmail-nospam.el --
;; Emacs interface for sending mail in cooperation with smtp.lisp
;; See configuration section for things to be customized.

;; ==== spec from smtp.lisp ====
;; Design of the mailer interface
;; 
;; The program with which the user sends mail should read and write the 
;; .smtp.adr file.  I expect it to provide operations that add, delete
;; and edit the entries.  Further, I expect that the addresses will be
;; used to refer to the corresponding to-strings, much as one would
;; use the name of a mailing list.  In particular there will be a command
;; that causes the message being composed to be sent to the to-string
;; corresponding to a given address, with the mail being sent from that
;; address.  I propose a command that allows you to 
;; choose an existing address and inserts headers: 
;;     from: <address>
;;     to: <to-string>
;; e.g., if the .smtp.adr file contains 
;;     ("fred-swamy" nil "fc@all.net, swamy@cinenet.net")
;; and you ask to send to fred-swamy, the following is inserted.
;;     from: fred-swamy
;;     to: fc@all.net, swamy@cinenet.net
;; The to-string can contain returns, so it could even contain cc: etc.
;; The command above can be used more than once for the same message.
;; 
;; There will be a new command to check the addresses.  First, it will
;; check that there are no recipients before the first from line.  Then
;; it will check that all the from lines contain valid addresses.
;; 
;; The command to actually send the mail will be altered to first do the
;; check above.  If that's ok, it will send to each recipient from the 
;; address given on the preceding from line.  Only the one appropriate
;; from line will be in the message delivered to each user.


;; ==== code copied from xemacs (21.1.4) source ====

(require 'sendmail) 
;; so stuff below redefines what's in sendmail.el, not vice versa! 

;; copied and modified from sendmail -- changes marked by ##
(defun sendmail-send-it ()
  (require 'mail-utils)
  (check-addresses)  ;; ## inserted check
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " sendmail errors")
		  0))
	(tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	resend-to-addresses
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
;Removed.  See `mail-abbrevs.el'.
;	  (sendmail-synch-aliases)
;	  (if mail-aliases
;	      (expand-mail-aliases (point-min) delimline))
;	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (while (re-search-forward "^Resent-to:" delimline t)
	      (setq resend-to-addresses
		    (save-restriction
		      (narrow-to-region (point)
					(save-excursion
					  (forward-line 1)
					  (while (looking-at "^[ \t]")
					    (forward-line 1))
					  (point)))
		      (append (mail-parse-comma-list)
			      resend-to-addresses))))
;; Apparently this causes a duplicate Sender.
;;	    ;; If the From is different than current user, insert Sender.
;;	    (goto-char (point-min))
;;	    (and (re-search-forward "^From:"  delimline t)
;;		 (progn
;;		   (require 'mail-utils)
;;		   (not (string-equal
;;			 (mail-strip-quoted-names
;;			  (save-restriction
;;			    (narrow-to-region (point-min) delimline)
;;			    (mail-fetch-field "From")))
;;			 (user-login-name))))
;;		 (progn
;;		   (forward-line 1)
;;		   (insert "Sender: " (user-login-name) "\n")))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
		(replace-match ""))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    ;; ## this section is not needed since check above requires from:
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login (user-mail-address))
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 says \ and nonmatching parentheses
			     ;; must be escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward 
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  (let ((default-directory "/")
		exit-value)
	    (setq exit-value  ;; ## see below
		  (replace-call-to-sendmail errbuf resend-to-addresses))
	    (if (not (or (eq errbuf 0)
			 ;; ## replace-call returns a list 
			 (loop for e in exit-value always (zerop e))))
		(error "Sending...failed with exit value %s" exit-value)))
	  (if mail-interactive
	      (save-excursion
		(set-buffer errbuf)
		(goto-char (point-min))
		(while (re-search-forward "\n\n* *" nil t)
		  (replace-match "; "))
		(if (not (zerop (buffer-size)))
		    (error "Sending...failed to %s"
			   (buffer-substring (point-min) (point-max)))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

;; original code replaced above by replace-call-to-sendmail
(defun original-call-to-sendmail (errbuf resend-to-addresses)
 (apply 'call-process-region
	(append (list (point-min) (point-max)
		      (if (boundp
			   'sendmail-program)
			  sendmail-program
			"/usr/lib/sendmail")
		      nil errbuf nil "-oi")
		;; Always specify who from,
		;; since some systems have
		;; broken sendmails.
		(list "-f" (user-login-name))
		(and mail-alias-file
		     (list
		      (concat "-oA"
			      mail-alias-file)))
		;; These mean "report errors by
		;; mail" and "deliver in
		;; background".
		(if (null mail-interactive)
		    '("-oem" "-odb"))
		;; Get the addresses from the
		;; message unless this is a
		;; resend.  We must not do that
		;; for a resend because we
		;; would find the original
		;; addresses.  For a resend,
		;; include the specific
		;; addresses.
		(or resend-to-addresses
		    '("-t")))))

;; Just the thing I need!  How nice.

(or (ignore-errors (require 'smtpmail))
 ;; copied defn from smtpmail - prefer require in case a new version arrives
 (defun smtpmail-deduce-address-list
   (smtpmail-text-buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO: <address>."
  (require 'mail-utils)  ;; pick up mail-strip-quoted-names
  (unwind-protect
      (save-excursion
	(set-buffer smtpmail-address-buffer) (erase-buffer)
	(let
	    ((case-fold-search t)
	     (simple-address-list "")
	     this-line
	     this-line-end
	     addr-regexp)
	  (insert-buffer-substring smtpmail-text-buffer header-start header-end)
	  (goto-char (point-min))
	  ;; RESENT-* fields should stop processing of regular fields.
	  (save-excursion
	    (if (re-search-forward "^Resent-\\(to\\|cc\\|bcc\\):" header-end t)
		(setq addr-regexp "^Resent-\\(to\\|cc\\|bcc\\):")
	      (setq addr-regexp  "^\\(To:\\|Cc:\\|Bcc:\\)")))

	  (while (re-search-forward addr-regexp header-end t)
	    (replace-match "")
	    (setq this-line (match-beginning 0))
	    (forward-line 1)
	    ;; get any continuation lines
	    (while (and (looking-at "^[ \t]+") (< (point) header-end))
	      (forward-line 1))
	    (setq this-line-end (point-marker))
	    (setq simple-address-list
		  (concat simple-address-list " "
			  (mail-strip-quoted-names
			   (buffer-substring this-line this-line-end)))))
	  (erase-buffer)
	  (insert-string " ")
	  (insert-string simple-address-list)
	  (insert-string "\n")
	  (subst-char-in-region (point-min) (point-max) 10 ?  t);; newline --> blank
	  (subst-char-in-region (point-min) (point-max) ?, ?  t);; comma   --> blank
	  (subst-char-in-region (point-min) (point-max)  9 ?  t);; tab     --> blank
	  (goto-char (point-min))
	  ;; tidyness in case hook is not robust when it looks at this
	  (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))
	  (goto-char (point-min))
	  (let (recipient-address-list)
	    (while (re-search-forward " \\([^ ]+\\) " (point-max) t)
	      (backward-char 1)
	      (setq recipient-address-list
		    (cons (buffer-substring (match-beginning 1) (match-end 1))
			  recipient-address-list)))
	    (setq smtpmail-recipient-address-list recipient-address-list))
	  )))))


;; ==== time utilities shared by this and smtp.lisp ====

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

;; emacs lisp only
(defvar two16 (expt 2 16)) 
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

' ;; #+ignore ;; common lisp version
(defun cl-time+ (abs diff)
  (let ((wrong-ut (apply 'encode-universal-time
			 (reverse (extend-absolute-time abs)))))
    (loop for x in (extend-time-difference diff)
	as y in '(86400 3600 60 1) do (incf wrong-ut (* x y)))
    (nthcdr 3 (reverse (multiple-value-list
			(decode-universal-time wrong-ut))))))

(defun emacs-current-gmt ()
  (let ((wrong-ut (current-time)))
    (decf (cadr wrong-ut) (car (current-time-zone)))
    (incf (car wrong-ut) (floor (cadr wrong-ut) two16))
    (setf (cadr wrong-ut) (mod (cadr wrong-ut) two16))
    (nthcdr 3 (reverse (decode-time wrong-ut)))))

' ;; #+ignore 
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

;; well, these are sort of related to time

(defun file-write-time (f)
  ;; list of two integers, (0 0) if not readable
  (if (file-readable-p f) (sixth (file-attributes f)) (list 0 0)))

;; ==== configuration - things you might want to change ====

;; this code originally written for xemacs 21.1
;; these corrections are needed for xemacs 19.13 !@#$%
; (defun user-home-directory () "/export/home/donc")
; (defvar mail-host-address nil)
; (defvar mail-mailer-swallows-blank-line nil)

;; regexp for recognizing your addresses
;; you can avoid this error by putting the correct defvar in .emacs
;; BEFORE you load this file.
(defvar address-string (error "fill this in appropriately"))
;; something like "\\<don[-A-Z0-9a-z]*@isis.cs3-inc.com"
(defvar local-address (concat (user-login-name) " ***"))

;; linux
(defvar *user-translations* "/root/.smtp.translations")
;; sun: (defvar *user-translations* "/.smtp.translations")
;; the name of the file with translations from addresses to user id's
;; should be the same as the corresponding variable in smtp.lisp

;; you can set this before you start to the name of your adr file 
(defvar *adr-file* ".smtp.adr")
;; or if you want to use one not in your homedir then set this one 
(defvar *user-adr-file* "/root/smtp/don/.smtp.adr")

(defvar from-line-suffix "")
;; or something like " (Don Cohen)"

(defvar *sendmail-config* "-C/etc/sendmail.cf")

;; let the program set these
(defvar *adr-cache* nil)
(defvar *adr-cache-time* (list 0 0)) ;; can be reset to reread the .adr file

(defun update-adr-cache ()
  ;; update the cache for the .adr file
  (or *user-adr-file*
      (setf *user-adr-file* (concat (user-home-directory) "/" *adr-file*)))
  (when (number-list< *adr-cache-time* (file-write-time *user-adr-file*))
    (let ((temp-buffer (generate-new-buffer "temp-adr")))
      (save-excursion
	(set-buffer temp-buffer)
	(insert-file-contents *user-adr-file*)
	(goto-char (point-min))
	(setf *adr-cache* (ignore-errors (read temp-buffer)))
	(setf *adr-cache-time* (file-write-time *user-adr-file*)))
      (kill-buffer temp-buffer))))

(defvar *user-cache* nil)
(defvar *user-cache-time* (list 0 0)) ;; can be reset to reread the .adr file

(defun update-user-cache ()
  ;; update the cache for the system user translations file
  (when (number-list< *user-cache-time* (file-write-time *user-translations*))
    (let ((temp-buffer (generate-new-buffer "temp-user")))
      (save-excursion
	(set-buffer temp-buffer)
	(insert-file-contents *user-translations*)
	(goto-char (point-min))
	(setf *user-cache* (ignore-errors (read temp-buffer)))
	(setf *user-cache-time* (file-write-time *user-translations*)))
      (kill-buffer temp-buffer))))

(defun from (arg) ;; a command for composing mail
  ;; insert from line - prompt for address
  ;; with arg insert addresses
  (interactive "P")
  (update-adr-cache)
  (block fromblock
    (let ((adr (completing-read "address: " *adr-cache* nil t)))
      (when (equal adr "") (return-from fromblock))
      (beginning-of-line)
      (insert "\n")
      (backward-char)
      (insert "From: @" (or mail-host-address (system-name))
	      from-line-suffix)
      (search-backward "@")
      (insert adr)
      (end-of-line)
      (forward-char)
      (insert "To: \n")
      (backward-char)
      (when arg
	(insert (third (assoc adr *adr-cache*)) "\n")))))

;; add fromtemp (arg) arg defaults to *temp-duration* initially 7 days
;; which adds a temp address, removes any expired and writes .adr file


(defun check-addresses () ;; another possibly useful command
  ;; generate an error if there is any recipient not preceded by a from line 
  ;; or any from line without a valid address.
  (let* ((start (point-min))
	 (end (progn (goto-char (point-min))
		     (re-search-forward
		      (concat "^" (regexp-quote mail-header-separator) "\n"))))
	 (case-fold-search t)
	 (last-from 0)
	 (from (progn (goto-char (point-min))
		      (re-search-forward "^from:" end))))
    ;; any recipients before first from: line?
    (when (find-recipients (point-min) from)
      (error "reciepients appear before first from header"))
    (update-adr-cache)
    (update-user-cache)
    (check-next-address)
    (loop while (setf next-from (re-search-forward "^from:" end t)) do
	  (check-next-address))))

;; emacs lisp version of similar function in .smtp.lisp
;; see comment there about possible need to change it
(defun translate-user (adr)
  ;; update-user-cache already called
  (or (loop for (prefix user) in *user-cache* thereis
	    (and (<= (length prefix) (length adr))
		 (string-equal prefix (substring adr 0 (length prefix)))
		 user))
      "root"))

(defun check-next-address ()
  ;; currently at end of from:
  (let ((adr (assoc 
	      (let ((at (search-forward "@")))
		(backward-sexp 1)
		(buffer-substring (point) (1- at)))
		   *adr-cache*)))
    (unless (equal (user-login-name) (translate-user (car adr)))
      (error "not your address"))
    (unless adr (error "not a known address"))
    (when (and (cadr adr) (abs-time-< (cadr adr) (emacs-current-gmt)))
      (error "address expired"))))

(defun find-recipients (min max)
  (setq smtpmail-address-buffer (generate-new-buffer "*smtp-mail*"))
  (prog1 (ignore-errors
	  (xxsmtpmail-deduce-address-list (current-buffer) min max))
    (kill-buffer smtpmail-address-buffer)))

;; taken from smtpmail.el, seems to have disappeared in more recent version
(defun xxsmtpmail-deduce-address-list (smtpmail-text-buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO: <address>."
  (require 'mail-utils)  ;; pick up mail-strip-quoted-names
    
  (unwind-protect
      (save-excursion
	(set-buffer smtpmail-address-buffer) (erase-buffer)
	(let
	    ((case-fold-search t)
	     (simple-address-list "")
	     this-line
	     this-line-end
	     addr-regexp)
	  (insert-buffer-substring smtpmail-text-buffer header-start header-end)
	  (goto-char (point-min))
	  ;; RESENT-* fields should stop processing of regular fields.
	  (save-excursion
	    (if (re-search-forward "^Resent-\\(to\\|cc\\|bcc\\):" header-end t)
		(setq addr-regexp "^Resent-\\(to\\|cc\\|bcc\\):")
	      (setq addr-regexp  "^\\(To:\\|Cc:\\|Bcc:\\)")))

	  (while (re-search-forward addr-regexp header-end t)
	    (replace-match "")
	    (setq this-line (match-beginning 0))
	    (forward-line 1)
	    ;; get any continuation lines
	    (while (and (looking-at "^[ \t]+") (< (point) header-end))
	      (forward-line 1))
	    (setq this-line-end (point-marker))
	    (setq simple-address-list
		  (concat simple-address-list " "
			  (mail-strip-quoted-names (buffer-substring this-line this-line-end))))
	    )
	  (erase-buffer)
	  (insert-string " ")
	  (insert-string simple-address-list)
	  (insert-string "\n")
	  (subst-char-in-region (point-min) (point-max) 10 ?  t);; newline --> blank
	  (subst-char-in-region (point-min) (point-max) ?, ?  t);; comma   --> blank
	  (subst-char-in-region (point-min) (point-max)  9 ?  t);; tab     --> blank

	  (goto-char (point-min))
	  ;; tidyness in case hook is not robust when it looks at this
	  (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

	  (goto-char (point-min))
	  (let (recipient-address-list)
	    (while (re-search-forward " \\([^ ]+\\) " (point-max) t)
	      (backward-char 1)
	      (setq recipient-address-list (cons (buffer-substring (match-beginning 1) (match-end 1))
						 recipient-address-list))
	      )
	    (setq smtpmail-recipient-address-list recipient-address-list))

	  )
	)
    )
  )

;; already called from inside save-excursion
(defun replace-call-to-sendmail (errbuf resend-to-addresses)
  (let* ((start (point-min))
	 (end (progn (goto-char (point-min))
		     (search-forward "\n\n")))
	 (case-fold-search t)
	 (from (progn (goto-char (point-min))
		      (re-search-forward "^from:" end t)))
	 nextfrom fromline from-to-list)
    (loop while from do
	  (goto-char from) ;; to be on the safe side
	  (let (b e)
	    (beginning-of-line) (setf b (point))
	    (next-line 1) (setf e (point))
	    (setf fromline (buffer-substring b e)))
	  (setf nextfrom (re-search-forward "^from:" end t))
	  (push (cons fromline (find-recipients from (or nextfrom end)))
		from-to-list)
	  (setf from nextfrom))
    (goto-char end)
    (let (prev) (loop while (setq prev (re-search-backward "^from:" nil t))
		      do (beginning-of-line) (kill-line 1)))
    (loop for (from . to) in from-to-list collect
	  (save-excursion
	    (goto-char (point-min))
	    (insert from)
	    ;; hmm, is this a reliable way to find the address?  What is?
	    (goto-char (point-min))
	    (search-forward "@")
	    (let ((at (point)) sender)
	      (backward-sexp 1)
	      (setf sender (buffer-substring (point) (1- at)))
	      (apply 'call-process-region
		     (point-min) (point-max)
		     (if (boundp
			  'sendmail-program)
			 sendmail-program
		       "/usr/lib/sendmail")
		     nil errbuf nil "-oi" ;; oi related to dots
		     *sendmail-config*
		     ;; postfix doesn't like this arg
		     ;; "-O" (concat "DoubleBounceAddress=" (user-login-name))
		     "-f" sender
		     (append
		      (if (null mail-interactive)
			  '("-oem" "-odb"))
		      (or resend-to-addresses
			  to))))))))
;; man sendmail:
;       -ox value
;              Set  option  x  to  the specified value.  This form
;              uses single character names only.  The short  names
;              are  not  described  in  this  manual page; see the
;              Sendmail  Installation  and  Operation  Guide   for
;              details.
; at home: gv /root/sendmail-8.11.1/doc/op/op.ps &
; p.50 (sec 5.6)
; -oi - ignore dots
; -oem - mail back errors
; -odb - deliver in background (asynchronously)

;; At this point it appears that .forward is ignored as long as I use
;; either -C or DoubleBounceAddress.
;; -- Ahh - not quite true.  The .forward has to be readable by the
;; user running sendmail with such arguments since it gives up its
;; root priv's when those args are supplied.  For some reason it can
;; still deliver mail to the user's mail file though.

(setq mail-setup-hook '(do-more-mail-setup))
(defvar *reply-cc-list* nil)
(defun do-more-mail-setup ()
  (let* ((end (progn (goto-char (point-min))
		     (re-search-forward
		      (concat "^" (regexp-quote mail-header-separator) "\n"))))
	 (case-fold-search t)
	 ans)
    (goto-char (point-min))
    (loop while (re-search-forward address-string end t) do
      (replace-match local-address)
      (setq end (re-search-forward
		 (concat "^" (regexp-quote mail-header-separator) "\n")))
      (goto-char (point-min)))
    (goto-char (point-min))
    (loop for x in *reply-cc-list* do ;; set above
      (when (setq ans (string-match address-string x))
	(insert "From: " (substring x ans (match-end 0))
		from-line-suffix "\n")))
    (setq end (re-search-forward
	       (concat "^" (regexp-quote mail-header-separator) "\n")))
    ;; now leave me at the first *** inserted due to local address above
    (goto-char (point-min))
    (search-forward "***" end t)))

;; modified from
;; /usr/local/lib/xemacs-21.1.14/xemacs-packages/lisp/vm/vm-reply.el
;; changes marked with "Don"
;; Friday 2005/09/09 RH8.0 errors loading vm-reply from this symbol
(or (boundp 'vm-fsfemacs-mule-p) (setf vm-fsfemacs-mule-p nil))
(require 'vm-reply) ;; otherwise this is overwritten when we first reply.
(defun vm-do-reply (to-all include-text count)
    (let ((mlist (vm-select-marked-or-prefixed-messages count))
	  (dir default-directory)
	  (message-pointer vm-message-pointer)
	  (case-fold-search t)
	  to cc subject mp in-reply-to references tmp tmp2 newsgroups)
      (setq mp mlist)
      (while mp 
	(cond
	 ((eq mlist mp)
	  (cond ((setq to
		       (let ((reply-to
			      (vm-get-header-contents (car mp) "Reply-To:"
						      ", ")))
			 (if (vm-ignored-reply-to reply-to)
			     nil
			   reply-to ))))
		((setq to (vm-get-header-contents (car mp) "From:" ", ")))
		;; bad, but better than nothing for some
		((setq to (vm-grok-From_-author (car mp))))
		(t (error "No From: or Reply-To: header in message")))
	  (setq subject (vm-get-header-contents (car mp) "Subject:")
		in-reply-to
		(and vm-in-reply-to-format
		     (let ((vm-summary-uninteresting-senders nil))
		       (vm-summary-sprintf vm-in-reply-to-format (car mp))))
		in-reply-to (and (not (equal "" in-reply-to)) in-reply-to))
	  (and subject (stringp vm-reply-subject-prefix)
	       (let ((case-fold-search t))
		  (not
		   (equal
		    (string-match (regexp-quote vm-reply-subject-prefix)
				  subject)
		    0)))
	       (setq subject (concat vm-reply-subject-prefix subject))))
	 (t (cond ((setq tmp (vm-get-header-contents (car mp) "Reply-To:"
						     ", "))
		   (setq to (concat to "," tmp)))
		  ((setq tmp (vm-get-header-contents (car mp) "From:"
						     ", "))
		   (setq to (concat to "," tmp)))
		  ;; bad, but better than nothing for some
		  ((setq tmp (vm-grok-From_-author (car mp)))
		   (setq to (concat to "," tmp)))
		  (t (error "No From: or Reply-To: header in message")))))
	(if t ;; to-all -- Don
	    (progn
	      (setq tmp (vm-get-header-contents (car mp) "To:"
						", "))
	      (setq tmp2 (vm-get-header-contents (car mp) "Cc:"
						 ", "))
	      (if tmp
		  (if cc
		      (setq cc (concat cc "," tmp))
		    (setq cc tmp)))
	      (if tmp2
		  (if cc
		      (setq cc (concat cc "," tmp2))
		    (setq cc tmp2)))))
	(setq references
	      (cons (or (vm-get-header-contents (car mp) "References:" " ")
			(vm-get-header-contents (car mp) "In-reply-to:" " "))
		    (cons (vm-get-header-contents (car mp) "Message-ID:" " ")
			  references)))
	(setq newsgroups
	      (cons (or (and to-all (vm-get-header-contents (car mp) "Followup-To:" ","))
			(vm-get-header-contents (car mp) "Newsgroups:" ","))
		    newsgroups))
	(setq mp (cdr mp)))
      (if vm-strip-reply-headers
	  (let ((mail-use-rfc822 t))
	    (and to (setq to (mail-strip-quoted-names to)))
	    (and cc (setq cc (mail-strip-quoted-names cc)))))
      (setq to (vm-parse-addresses to)
	    cc (vm-parse-addresses cc))
      ;; added by Don
      (setq *reply-cc-list* cc 
	    cc (and to-all cc))
      (if vm-reply-ignored-addresses
	  (setq to (vm-strip-ignored-addresses to)
		cc (vm-strip-ignored-addresses cc)))
      (setq to (vm-delete-duplicates to nil t))
      (setq cc (vm-delete-duplicates
		(append (vm-delete-duplicates cc nil t)
			to (copy-sequence to))
		t t))
      (and to (setq to (mapconcat 'identity to ",\n    ")))
      (and cc (setq cc (mapconcat 'identity cc ",\n    ")))
      (and (null to) (setq to cc cc nil))
      (setq references (delq nil references)
	    references (mapconcat 'identity references " ")
	    references (vm-parse references "[^<]*\\(<[^>]+>\\)")
	    references (vm-delete-duplicates references)
	    references (if references (mapconcat 'identity references "\n\t")))
      (setq newsgroups (delq nil newsgroups)
	    newsgroups (mapconcat 'identity newsgroups ",")
	    newsgroups (vm-parse newsgroups "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)")
	    newsgroups (vm-delete-duplicates newsgroups)
	    newsgroups (if newsgroups (mapconcat 'identity newsgroups ",")))
      (vm-mail-internal
       (format "reply to %s%s" (vm-su-full-name (car mlist))
	       (if (cdr mlist) ", ..." ""))
       to subject in-reply-to cc references newsgroups)
      (make-local-variable 'vm-reply-list)
      (setq vm-system-state 'replying
	    vm-reply-list mlist
	    default-directory dir)
      (if include-text
	  (save-excursion
	    (goto-char (point-min))
	    (let ((case-fold-search nil))
	      (re-search-forward
	       (concat "^" (regexp-quote mail-header-separator) "$") nil 0))
	    (forward-char 1)
	    (while mlist
	      (save-restriction
		(narrow-to-region (point) (point))
		(vm-yank-message (car mlist))
		(goto-char (point-max)))
	      (setq mlist (cdr mlist)))))
      (run-hooks 'vm-reply-hook)
      (run-hooks 'vm-mail-mode-hook)))

;; so now I insert a from (or several) but still should remove myself
;; from the cc list, possibly insert a bcc instead
;; (mapcar 'mail-strip-quoted-names (vm-parse-addresses ???))


;;  #| for testing (but #| |# syntax not supported by emacs)
; (setf calls nil)
; (defun call-process-region1
;   (&rest r) (push r calls) (apply 'call-process-region r))
; |#
