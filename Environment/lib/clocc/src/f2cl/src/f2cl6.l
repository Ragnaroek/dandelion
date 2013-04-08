; f2cl6.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suite of functions for replacing ".EQ.", ".AND." etc with "=", "AND" etc
;; as a preprocessor for fortran-to-lisp.
;; Numbers such as 6. are replaced with 6.0
;;                 .6                   0.6
;;                 -.6                  -0.6
;;                 6.D0                 6.0D0
;;  etc
;; Also replaces x.xE-x with x.xE~x and x.xE+x with x.xEx so that the lisp reader
;; when set to read FORTRAN can cope.
;; Replaces single quotes (') with double quotes (").

;; Comments are removed

; functions:
;     preprocess
;     adjust_nrs_and_dots
;     parse-logl-op
;     replace-str
;     parse-number
;	write-comment-line
;--------------------------------------------------------------------------
(in-package :f2cl)
(eval-when (compile load eval) (proclaim '(special *verbose* *comments*)))
;--------------------------------------------------------------------------  
; global stream input for parse-number:
(defvar *inp* nil)
;---------------------------------------------------------------------------

;; Fortran comment line characters are "c", "C", or "*".  "!" is an
;; extension.
(defvar *comment-line-characters* '(#\c #\C #\* #\!)
  "A list of Fortran comment line characters")

;; Find the next quote
(defun find-quote (k line out)
  ;; LINE is the line we're looking at.  K is where the initial
  ;; single-quote character is.  OUT is an output string stream where
  ;; we copy the characters inside the quote.
  ;;
  ;; Return the position of the character after the matching end quote.
  ;;
  ;; Remember that Fortran says 'It''s time' is the string "It's
  ;; time".  That is, a quote followed by a quote is a single quote,
  ;; not the termination of the string.
  (let ((len (length line)))
    (incf k)
    (loop
       (when (char-equal (aref line k) #\')
	 ;; End of string or quoted quote character?
	 (if (and (< k len) (char-equal (aref line (1+ k)) #\'))
	     (incf k)
	     (return)))
       (when (>= k len)
	 (return))
       (when (char-equal (aref line k) #\")
	 ;; Quote any double-quote characters because we're writing
	 ;; out a Lisp string.
	 (write-char #\\ out))
       (write-char (aref line k) out)
       (incf k))
    (write-char #\" out)
    (1+ k)))

(defun process-format-line (line)
  ;; We have a Fortran format line.  Go through and convert all
  ;; Hollerith strings to regular strings.  Convert Fortran strings
  ;; (single quotes) to Lisp strings (double quotes). Hollerith
  ;; strings look like a number followed by H (or h).  Skip over any
  ;; strings that might be there.
  (let ((k 0)
	(line-len (length line))
	(out (make-string-output-stream)))
    (loop
	;; Done if there's no data left!
	(when (>= k line-len)
	  (return (get-output-stream-string out)))
	(let ((ch (aref line k)))
	  (cond ((char-equal ch #\')
		 ;; Find the next quote
		 (write-char #\" out)
		 (setf k (find-quote k line out)))
		((digit-char-p ch)
		 ;; Got a digit.  Look for the next non-digit.
		 (let ((posn (1+ k)))
		   (loop while (digit-char-p (aref line posn)) do
			 (incf posn))
		   (if (char-equal (aref line posn) #\H)

		       ;; A Hollerith!  Compute the length of the
		       ;; Hollerith string and where the number ends.
		       
		       (let ((len
			      (parse-integer line :start k :junk-allowed t)))
			 ;; Copy this many characters to the output
			 (incf posn)
			 (write-char #\" out)
			 (dotimes (n len)
			   ;; Need to quote any double-quote
			   ;; characters because we print out Lisp
			   ;; strings!
			   (when (char-equal (aref line posn) #\")
			     (write-char #\\ out))
			   (write-char (aref line posn) out)
			   (incf posn))
			 (write-char #\" out)
			 (setf k posn))
		       ;; Not a hollerith.  Copy the digits out
		       (loop while (< k posn) do
			     (write-char (aref line k) out)
			     (incf k)))))
		(t
		 (write-char ch out)
		 (incf k)))))))
  
(defun preprocess (file &key (outfile-name nil))
  (when *verbose*
    (format t "~&preprocessing begins ...~%"))
  (unless outfile-name
    ;; Pick some suitable default based on FILE.
    (let ((compiled-path (compile-file-pathname file)))
      (setf outfile-name
	    (merge-pathnames (make-pathname :host (pathname-host compiled-path)
					       :type "p")
			     compiled-path))))
  (with-open-file (inport file :direction :input)
    (with-open-file (outport outfile-name
			     :direction :output
			     :if-exists :rename-and-delete)
      (let ((*package* (find-package :f2cl))
	    (collected-comments nil))
	(do ((prev-line nil)
	     (line (read-line inport nil 'eof)
		   (read-line inport nil 'eof)))
	    ((eq line 'eof)
	     (when prev-line
	       (write-line prev-line outport))
	     outfile-name)
	  ;; Hmm, technically it might not be right to trim spaces from
	  ;; the end of the line.  Consider what happens we are trying
	  ;; to continue a string with trailing spaces.  The resulting
	  ;; string is wrong.
	  (let ((line (string-right-trim '(#\Space) (subseq line 0 (min 72 (length line))))))
	    (cond 
	      ((string= line "") nil)	; we leave out blank lines
	      ((member (char line 0) *comment-line-characters*)
	       (when *comments*
		 ;; If we don't have anything buffered, we can print
		 ;; out the comment now.  Otherwise, we need to save
		 ;; the comment and print it out later, after we've
		 ;; accumulated the continuation line, if any.
		 (if prev-line
		     (push line collected-comments)
		     (write-comment-line line outport))))
	      (t
	       (when *verbose* (princ line) (terpri))
	       ;; Handle tabs.  If the line begins with a tab, replace it with 6
	       ;; spaces.
	       (when (char-equal (aref line 0) #\Tab)
		 (setf line (concatenate 'string "      " (subseq line 1))))
	       ;; Do we really want to do this?  What if the tab is in a
	       ;; string?  I think tabs are illegal in Fortran, though.
	       (setf line (substitute #\Space #\Tab line))
	       ;; If line is a continuation line, append it.  If not,
	       ;; print out our buffered line.
	       (cond ((or (< (length line) 6)
			  (char-equal (aref line 5) #\Space))
		      ;; Not continuation.  Print out the buffered line, if
		      ;; any, and save this line for later.
		      (when prev-line
			;;(format t "~S~%" (adjust_nrs (replace_logl_ops prev-line)))
			;;(format t "prev-line = ~S~%" prev-line)

			;; Handle FORMAT statements carefully.  If we
			;; have a format statement, we want to replace
			;; any Hollerith strings in it with real
			;; strings.
			(let ((maybe-format (string-left-trim " " (subseq prev-line 6))))
			  (if (string-equal maybe-format "format"
					    :end1 (min 6 (length maybe-format)))
			      (write-line (process-format-line prev-line) outport)
			      (write-line (adjust_nrs (replace_logl_ops prev-line)) outport))))
		      (setf prev-line (copy-seq line))
		      ;; Print out the collected comments, if any
		      (when collected-comments
			(dolist (c (nreverse collected-comments))
			  (write-comment-line c outport))
			(setf collected-comments nil)))
		     (t
		      ;; A continuation line!  Append the line to our
		      ;; buffer, after removing any line number or
		      ;; continuation character.
		      (setf prev-line (concatenate 'string prev-line
						   (subseq line 6)))))))))))))


;--------------------------------------------------------------------------
(defun write-comment-line (line outport)
 (if *verbose* 
   (format t "      fortran_comment ~S~%" (string-downcase (subsequence line 1))))
 (format outport "      fortran_comment ~S~%" (string-downcase (subsequence line 1))))

(defun replace_logl_ops  (line)
   (prog (inport outport char)
     (setq inport (make-string-input-stream line)
           outport (make-string-output-stream))
     loop
     (setq char (read-char inport nil 'eof t))
     (cond ((eql char 'eof) 
            (return (get-output-stream-string outport)))
           ((char= char '#\.) 
            (parse-logl-op inport outport)
            (go loop))
           (t (write-char char outport)
              (go loop)))))
;--------------------------------------------------------------------------

#+nil
(defun adjust_nrs (line)
   (prog (outport char prev-char)
     (setq prev-char '#\space
           *inp* (make-string-input-stream line)
           outport (make-string-output-stream))
     loop
     (setq char (read-char *inp* nil 'eof t))
     (cond ((eql char 'eof) )

           ; check for single quote and pass over all char until next one
           ((char= char '#\')
            (setq prev-char '#\space)
            (write-char '#\" outport)
            (do ((char (read-char *inp* nil 'eof t) 
                       (read-char *inp* nil 'eof t)))
                ((char= char '#\') (write-char '#\" outport))
                (write-char char outport))
            (go loop))

           ; check for non-complex number 
           ((parse-number prev-char char outport)
            (setq prev-char '#\space)
            (go loop))

           (t (write-char char outport)
              (if (digit-char-p char)
                  (setq prev-char '#\?)
                  (setq prev-char char))
              (go loop)))
     (return (get-output-stream-string outport))))

(defun adjust_nrs (line)
  (let ((prev-char #\Space)
	(*inp* (make-string-input-stream line))
	(outport (make-string-output-stream)))
    ;; Read the first 6 characters and write them out.
    ;; (Don't want PARSE-NUMBER to mangle the spacing.)
    (let ((maybe-line-number (read-six-chars *inp*)))
      (dolist (char maybe-line-number)
	(when (not (eq char 'eof))
	  (write-char char outport)))
      (when (member 'eof maybe-line-number)
	(return-from adjust_nrs (get-output-stream-string outport))))
    (loop
	(let ((char (read-char *inp* nil 'eof t)))
	  (cond ((eql char 'eof)
		 (return-from adjust_nrs (get-output-stream-string outport)))

		;; check for single quote and pass over all characters
		;; until next one.  But a single quote followed by a
		;; single quote quotes the quote and doesn't terminate
		;; the quoted string.
		((char= char #\')
		 (setq prev-char #\space)
		 (write-char #\" outport)
		 (loop
		     (setf char (read-char *inp* nil 'eof t))
		     (let ((nxt (peek-char nil *inp* nil nil)))
		       (cond ((and (char= char #\')
				   (and nxt (char= nxt #\')))
			      (write-char #\' outport)
			      ;; Skip over the quote
			      (read-char *inp* nil 'eof t))
			     ((char= char #\')
			      (write-char #\" outport)
			      (return))
			     (t
			      (when (char= char #\")
				;; Need to quote any double-quote
				;; characters!
				(write-char #\\ outport))
			      (write-char char outport))))))

		;; check for non-complex number 
		((parse-number prev-char char outport)
		 (setq prev-char '#\space))

		(t
		 (write-char char outport)
		 (if (digit-char-p char)
		     (setq prev-char '#\?)
		     (setq prev-char char))))))))
;------------------------------------------------------------------------------

(defun parse-logl-op (inport outport)
   (prog (str nxt-char logl-op)
      (setq nxt-char (peek-char nil inport nil 'eof #+broken-peek-char t))
      (if (or (member nxt-char `(eof #\space #\' #\.)) 
              (digit-char-p nxt-char))
          (and (write-char '#\. outport)
               (return)))
      (setq str (make-string-output-stream))
      loop
        (write-char (read-char inport nil 'eof t) str)
        (setq nxt-char (peek-char nil inport nil 'eof #+broken-peek-char t))
        (if (and (not (eql nxt-char 'eof)) 
                      (char/= nxt-char `#\.)) (go loop))
      (cond ((setq str (get-output-stream-string str)
                   logl-op (replace-str str))
             (read-char inport nil 'eof t) ; read final dot
             (write-char '#\space outport)
             (write logl-op :stream outport) ; write the converted string with spaces
             (write-char '#\space outport))
            (t
	     (write-char '#\. outport) ; put the dot back
               (write  str :stream outport :escape nil))))) ; put everything else back

;------------------------------------------------------------------------------
(defun replace-str (str)
   (cond ((equalp str "gt")    '>)
         ((equalp str "ge")    '>=)
         ((equalp str "eq")    'equal)
         ((equalp str "le")    '<=)
         ((equalp str "lt")    '<)
         ((equalp str "ne")    '><)
         ((equalp str "not")   'not)
         ((equalp str "and")   'and)
         ((equalp str "or")    'or)
         ((equalp str "true")  '%true%)
         ((equalp str "t")     '%true%)
         ((equalp str "false") '%false%)
         ((equalp str "f")     '%false%)
         ((equalp str "eqv")   'logeqv)
         ((equalp str "neqv")  'logxor)
         (t nil)))
;------------------------------------------------------------------------------
;inport
#+nil
(defun parse-number (prev-char char outport)
  (flet ((my-digit-char-p (char)
	   (and (characterp char)
		(digit-char-p char)))
	 (my-char= (char1 char2)
	   (and (characterp char1)
		(characterp char2)
		(char= char1 char2))))
    #-clisp(declare (inline my-digit-char-p my-char=))
    (prog (str nxt-char)
       ;; first check prev-char suitable to precede a number
       (if (not (member prev-char 
			'(#\newline #\space #\. #\= #\+ #\- #\* #\/ #\( #\, )))
	   (return nil))
       (setq str (make-string-output-stream))
       ;; examine first char and peek at second to ascertain this is a number
       (setq nxt-char (peek-char nil *inp* nil 'eof #+broken-peek-char t))
       ;; (format t "prev, char, nxt = ~S ~S ~S~%" prev-char char nxt-char)
       (cond ((and (my-digit-char-p char) 
		   (or (and (not (eql nxt-char 'eof)) 
			    (my-digit-char-p nxt-char))
		       (member nxt-char '(#\. #\D #\d #\E #\e))))
	      (write-char char str) )
	     ((and (or (my-char= char '#\+) (my-char= char '#\-))
		   (my-char= nxt-char '#\.))
	      (write-char char str) (write-char '#\0 str) )
	     ((and (or (my-char= char '#\+) (my-char= char '#\-))
		   (my-digit-char-p nxt-char))
	      (write-char char str))
	     ((and (my-char= char '#\.) (my-digit-char-p nxt-char))
	      (write-char '#\0 str) (write-char '#\. str) )
	     (t (return nil)))
       ;; parse rest of number;eof
       loop
       (setq char (read-char *inp* nil 'eof t)
	     nxt-char (peek-char nil *inp* nil 'eof #+broken-peek-char t))
       (when (eql nxt-char 'eof)
	 (setq nxt-char `#\space))
       ;; (format t "char, nxt-char = ~S ~S~%" char nxt-char)
       (cond ((eql char 'eof)
	      ;; Assume number is complete.
	      (write (get-output-stream-string str) :stream outport :escape nil)
	      (write-char '#\space outport)
	      (return t))
	     ((my-digit-char-p char)
	      (write-char char str) 
	      (go loop))
	     ((and (my-char= char '#\.)
		   (not (or (my-digit-char-p nxt-char)
			    (my-char= nxt-char '#\D)
			    (my-char= nxt-char '#\e)
			    (my-char= nxt-char '#\E)
			    (my-char= nxt-char '#\e))))
	      ;; Convert "0." to "0.0"
	      (write-char '#\. str) (write-char '#\0 str)
	      (go loop))
	     ((and (my-char= char '#\.)
		   (my-digit-char-p nxt-char))
	      ;; We have "n.n"
	      (write-char '#\. str) 
	      (go loop))
	     ((and (my-char= char '#\.)
		   (not (my-digit-char-p nxt-char)))
	      ;; We have "." followed by some non digit.  Append a 0
	      ;; to the number.
	      (write-char '#\. str) (write-char '#\0 str)
	      (go loop))
	     ((or (char-equal char '#\E) (char-equal char '#\e))
	      (write-char '#\e str)
	      (cond ((my-char= nxt-char '#\+) (read-char *inp* nil 'eof t))
		    ((my-char= nxt-char '#\-) (read-char *inp* nil 'eof t)
		     (write-char '#\% str)))
	      (go loop))
	     ((or (char-equal char '#\D) (char-equal char '#\d))
	      (write-char '#\d str)
	      (cond ((my-char= nxt-char '#\+) (read-char *inp* nil 'eof t))
		    ((my-char= nxt-char '#\-) (read-char *inp* nil 'eof t)
		     (write-char '#\% str)))
	      (go loop))
	     ((or (my-char= char '#\-) (my-char= char '#\+))
	      (write-char char str) 
	      (go loop))
	     ((my-char= char #\Space)
	      ;; Spaces embedded within a number don't terminate the
	      ;; number so skip over the space.  (Spaces in Fortran
	      ;; are "invisible".)
	      (go loop))
	     (t				;number complete
	      #+clisp 
	      (if (equal (peek-char nil *inp* nil 'eof t) 'eof)
		  (setq *inp* (make-string-input-stream (string char)))
		  (setq *inp* 
			(make-string-input-stream
			 (concatenate 'string (string char) (read-line *inp*)))))
	      #-clisp (unread-char char *inp*)
	      (write (get-output-stream-string str) :stream outport :escape nil)
	      (write-char '#\space outport)
	      (return t))))))

(defun parse-number (prev-char char outport)
  (labels ((my-digit-char-p (char)
	     (and (characterp char)
		  (digit-char-p char)))
	   (my-char= (char1 char2)
	     (and (characterp char1)
		  (characterp char2)
		  (char= char1 char2)))
	   (skip-spaces ()
	     (do ((nxt (peek-char nil *inp* nil 'eof)
		       (peek-char nil *inp* nil 'eof)))
		 ((not (my-char= nxt #\Space))
		  nxt)
	       (read-char *inp* nil 'eof t))))
    #-clisp(declare (inline my-digit-char-p my-char=))
    (let ((in-number-p nil))
      ;; first check prev-char suitable to precede a number
      (unless (member prev-char 
		      '(#\newline #\space #\. #\= #\+ #\- #\* #\/ #\( #\, ))
	(return-from parse-number nil))
      (let ((str (make-string-output-stream))
	    ;; examine first char and peek at second to ascertain this
	    ;; is a number.  Don't try to skip over spaces here because
	    ;; it confuses later stages of the parser.  (In particular,
	    ;; the Fortran "6.or." gets mangled into the symbol "6OR".)
	    (nxt-char (peek-char nil *inp* nil 'eof #+broken-peek-char t)))
	;;(format t "prev, char, nxt = ~S ~S ~S~%" prev-char char nxt-char)
	(cond ((and (my-digit-char-p char) 
		    (or (and (not (eql nxt-char 'eof))
			     (my-digit-char-p nxt-char))
			(member nxt-char '(#\. #\D #\E #\d #\e))))
	       (write-char char str))
	      ((and (or (my-char= char '#\+)
			(my-char= char '#\-))
		    (my-char= nxt-char '#\.))
	       (write-char char str)
	       (write-char '#\0 str))
	      ((and (or (my-char= char '#\+)
			(my-char= char '#\-))
		    (my-digit-char-p nxt-char))
	       (write-char char str))
	      ((and (my-char= char '#\.)
		    (my-digit-char-p nxt-char))
	       (write-char '#\0 str)
	       (write-char '#\. str) )
	      (t
	       ;;(format t "parse-number done!?~%")
	       (return-from parse-number nil)))
	;; parse rest of number;eof
	;;(format t "parse rest~%")
	(loop
	    (skip-spaces)
	  (setq char (read-char *inp* nil 'eof t)
		nxt-char (peek-char nil *inp* nil 'eof #+broken-peek-char t))
	  ;;(format t "    char, nxt-char = ~S ~S~%" char nxt-char)
	  ;; Skip over spaces if the current character is a part of a number.
	  (when (and (eql nxt-char #\Space)
		     (or (my-digit-char-p char)
			 (member char (if in-number-p
					  '(#\E #\D #\e #\d)
					  '(#\E #\D #\- #\+ #\e #\d #\.)))))
	    (skip-spaces)
	    (setq nxt-char (peek-char nil *inp* nil 'eof))
	    ;;(format t "nxt-char = ~S~%" nxt-char)
	    )
	  (when (eql nxt-char 'eof)
	    (setq nxt-char `#\space))
	  ;;(format t "new char, nxt-char = ~S ~S~%" char nxt-char)
	  (cond ((eql char 'eof)
		 ;; Assume number is complete.
		 (write (get-output-stream-string str) :stream outport :escape nil)
		 (write-char '#\space outport)
		 (return-from parse-number t))
		((my-digit-char-p char)
		 (setf in-number-p t)
		 (write-char char str))
		((and (my-char= char '#\.)
		      (not (or (my-digit-char-p nxt-char)
			       (my-char= nxt-char '#\D)
			       (my-char= nxt-char '#\d)
			       (my-char= nxt-char '#\E)
			       (my-char= nxt-char '#\e))))
		 ;; Convert "n." to "n.0" if there's no following number.

		 ;;(format t "inserting 0: char, nxt = ~S ~S~%" char nxt-char)
		 (setf in-number-p t)
		 (write-char '#\. str)
		 (write-char '#\0 str))
		((and (my-char= char '#\.)
		      (my-digit-char-p nxt-char))
		 ;; We have "n.n"
		 (setf in-number-p t)
		 (write-char '#\. str))
		((and (my-char= char '#\.)
		      (not (my-digit-char-p nxt-char)))
		 ;; We have "." followed by some non-digit or non-space.
		 ;; Append a 0 to the number.

		 ;;(format t "inserting 0: char, nxt = ~S ~S~%" char nxt-char)

		 (setf in-number-p t)
		 (write-char '#\. str)
		 (write-char '#\0 str))
		((or (char-equal char #\E) (char-equal char #\e)
		     (char-equal char #\D) (char-equal char #\d))
		 ;;(format t "got exponent ~S~%" char)
		 (setf in-number-p t)
		 ;; Process exponent, skip over white space.
		 (when (my-char= nxt-char #\Space)
		   (skip-spaces)
		   (setf nxt-char (peek-char nil *inp* nil 'eof)))
		 ;;(format t " nxt = ~S~%" nxt-char)
		 (cond ((my-char= nxt-char '#\+)
			;; Write the exponent char out, read and
			;; discard the "+" char
			(write-char char str)
			(read-char *inp* nil 'eof t))
		       ((my-char= nxt-char '#\-)
			;; Write the exponent char out, read and
			;; discard "-" char, and send a "%" out
			;; instead.
			(write-char char str)
			(read-char *inp* nil 'eof t)
			(write-char '#\% str))
		       ((my-digit-char-p nxt-char)
			;; Exponent followed by a digit.  Write the
			;; exponent out.
			(write-char char str))
		       (t
			(let ((s (get-output-stream-string str)))
			  ;;(format t "number is ~a~%" s)
			  (write s :stream outport :escape nil))
			;; The exponent char we got isn't really part
			;; of the number.  Push back the char.
			(setq *inp*
			      (make-string-input-stream
			       (concatenate 'string " "
					    (string char)
					    (read-line *inp*))))
			(return-from parse-number t))))
		(t			;number complete
		 ;;(format t "complete char, nxt = ~S ~S~%" char nxt-char)
		 ;; We want to unread CHAR, but, according to the CLHS,
		 ;; unread-char must push back the last character that
		 ;; was read.  Because of skip-spaces above, this CHAR
		 ;; is not necessarily the last character read anymore.
		 ;; We have to modify the stream directly now.
		 ;;
		 ;; Do we want recursive-p to be T here or not?
		 (let ((p (peek-char nil *inp* nil 'eof #+nil t)))
		   (if (equal p 'eof)
		       (setq *inp* (make-string-input-stream (string char)))
		       (setq *inp* 
			     (make-string-input-stream
			      (concatenate 'string (string char) (read-line *inp*))))))
		 (let ((s (get-output-stream-string str)))
		   ;;(format t "number is ~a~%" s)
		   (write s :stream outport :escape nil))
		 (write-char '#\space outport)
		 (return-from parse-number t))))))))
;;;-----------------------------------------------------------------------------
;;; end of f2cl6.l
;;;
;;; $Id: f2cl6.l,v 1.42 2006/01/30 21:23:34 rtoy Exp $
;;; $Log: f2cl6.l,v $
;;; Revision 1.42  2006/01/30 21:23:34  rtoy
;;; Revert change made in 1.39 for logical operations.  This was causing
;;; f2cl to delete all spaces after the logical operation and that really
;;; confuses later parts because we need whitespace to separate symbols
;;; and such.
;;;
;;; Revision 1.41  2006/01/09 19:26:19  rtoy
;;; o We need to quote any double-quote characters that we might find in a
;;;   string, so that we can parse it correctly during translation.  (For
;;;   this to work, we MUST NOT change the syntax of \ in the Fortran
;;;   readtable.)
;;; o Don't trim any spaces and such when combining continuation lines.
;;;   Doing so definitely breaks Hollerith (ick!) strings in format
;;;   statements.
;;;
;;; Revision 1.40  2006/01/08 16:51:22  rtoy
;;; We were (stupidly) not recognizing FORMAT statements that didn't start
;;; in column 7, so we weren't converting any Hollerith strings to strings
;;; in the format.
;;;
;;; Revision 1.39  2006/01/04 17:39:56  rtoy
;;; src/f2cl6.l:
;;; o Skip over whitespace when parsing logical operations.  This is meant
;;;   to handle things like "ZERO. AND . ONE", which Fortran says is the
;;;   same as "ZERO .AND. ONE".
;;;
;;; val/tst-parse.f:
;;; o Test file for parsing logical operations with spaces.
;;;
;;; Revision 1.38  2004/08/11 15:53:31  rtoy
;;; This bit of Fortran code
;;;
;;;       COMMON /DLS001/ ROWNS(209),
;;;      1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
;;;      2   INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS(6),
;;;      3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
;;;      4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
;;;      5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
;;; C
;;;       DATA  MORD(1),MORD(2)/12,5/, MXSTP0/500/, MXHNL0/10/
;;;       IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601
;;;       end
;;;
;;; causes an end-of-file error because RECURSIVE-P is T (on CMUCL).  I'm
;;; going to make RECURSIVE-P NIL here.  Don't know if that's right or
;;; not.
;;;
;;; Revision 1.37  2003/09/20 13:56:55  rtoy
;;; [ 807859 ] translate-pathname strangeness in f2cl6.l
;;;
;;; Use merge-pathnames instead of translate-pathnames to compute the name
;;; of the output file.
;;;
;;; Revision 1.36  2003/07/12 18:44:04  rtoy
;;; Include directory and name when computing the name of the output file.
;;;
;;; Revision 1.35  2002/09/13 17:50:19  rtoy
;;; From Douglas Crosher:
;;;
;;; o Make this work with lower-case Lisps
;;; o Fix a few typos
;;; o Make a safer fortran reader.
;;;
;;; Revision 1.34  2002/03/11 17:57:32  rtoy
;;; Compute the output temp file name better.  (Was failing for logical
;;; pathnames.)
;;;
;;; Revision 1.33  2002/02/08 03:32:55  rtoy
;;; Oops.  Can't use #'char-equal because nxt-char might not be a char.
;;;
;;; Revision 1.32  2002/02/07 23:27:05  rtoy
;;; Some more number parsing bugs in PARSE-NUMBER.
;;; o d and e are valid exponent indicators and we missed them sometimes.
;;; o Add a few more comments
;;; o Make it easier to debug what parse-number would return as the
;;;   number.
;;;
;;; Revision 1.31  2002/02/07 22:12:56  rtoy
;;; Oops.  Remove that debugging statement.
;;;
;;; Revision 1.30  2002/02/07 20:35:22  rtoy
;;; Choose a better name than "prep.tmp" in whatever directory we are in
;;; for the temporary preprocessed file.  Make it go into the same
;;; directory as the Fortran file, but with the extension "p".
;;;
;;; Revision 1.29  2002/02/07 20:14:12  rtoy
;;; f2cl was incorrectly parsing
;;;
;;; 	character*10 dig
;;;
;;; as
;;;
;;; 	character * 10d ig
;;;
;;; because we weren't handling the case where the exponent character
;;; wasn't really an exponent character.
;;;
;;; Revision 1.28  2002/01/05 18:28:34  rtoy
;;; Was still erroneously doing an unread-char.  (Caught by Clisp.)
;;; Works with Clisp and CMUCL now, and probably others.
;;;
;;; Revision 1.27  2001/09/10 16:37:14  rtoy
;;; Oops.  Forgot to strip out the extra quote when we got quoted quotes
;;; in format strings.
;;;
;;; Revision 1.26  2001/09/10 16:21:39  rtoy
;;; Handle Fortran-style strings better by handling quoted quotes.  (In
;;; Fortran 'It''s time' is the string "It's time".)
;;;
;;; Revision 1.25  2001/09/09 21:05:50  rtoy
;;; Undo rev 1.23.  This one should work better.
;;;
;;; Revision 1.24  2001/09/09 04:34:07  rtoy
;;; An attempt at handling Fortran Hollerith strings in FORMAT
;;; statements.  During preprocessing, look for a FORMAT statement and
;;; replace any Hollerith strings with real quoted strings.  (Can't do
;;; this for Hollerith in code because we don't have a real Fortran
;;; parser.)
;;;
;;; (FIND-QUOTE needs work to get Fortran string quoting correct.)
;;;
;;; Revision 1.23  2001/09/08 21:41:37  rtoy
;;; parse-number was mangling
;;;
;;; 	y = 1.5d0 - exp(-x)*top
;;;
;;; into the invalid
;;;
;;; 	y = 1.5d0 - e xp(-x)*top
;;;
;;; Fix this.
;;;
;;; Revision 1.22  2001/06/04 14:06:03  rtoy
;;; Add *comment-line-characters* to hold a list of valid comment line
;;; characters.
;;;
;;; Revision 1.21  2000/09/01 14:07:37  rtoy
;;; In PARSE-NUMBER, we want to unread CHAR, but, according to the CLHS
;;; (and Clisp checks for this), unread-char must push back the last
;;; character that was read.  Because of SKIP-SPACES above, this CHAR is
;;; not necessarily the last character read anymore.  We have to modify
;;; the stream directly now.
;;;
;;; Revision 1.20  2000/08/18 15:22:11  rtoy
;;; Can't swallow blanks (for now) at the first part of parse-number
;;; because it causes things like "6.or." to become the symbol "6OR",
;;; which is wrong.
;;;
;;; Revision 1.19  2000/08/13 22:31:10  rtoy
;;; With the change in PARSE-NUMBER, ADJUST_NRS was sometimes messing up
;;; the columns for the statement number and the statement itself.  Fix
;;; this by having ADJUST_NRS alwasy read the first 6 characters of the
;;; line itself and interpreting the statement number instead of having
;;; PARSE-NUMBER do that.  Statement numbers are working again!
;;;
;;; Revision 1.18  2000/08/13 04:18:44  rtoy
;;; Make PARSE-NUMBER handle sapces within numbers, which is perfectly
;;; acceptable Fortran.  (May be a bit buggy still; probably should
;;; rewrite PARSE-NUMBER.)
;;;
;;; Revision 1.17  2000/08/01 23:02:36  rtoy
;;; We were messing up the printing of comments after we changed
;;; PREPROCESS to handle continuation lines.  We need to accumulate
;;; comment lines until we know we have a complete line (with all
;;; continuation lines).  Then we can print out the line followed by the
;;; accumulated comment lines.  This makes the comments possible out of
;;; sync with the generated Lisp code, but they were never really in sync
;;; anyway.
;;;
;;; Revision 1.16  2000/07/28 17:07:11  rtoy
;;; o We are in the f2cl package now.
;;; o When we preprocess the file, we want to READ using the F2CL package
;;;   so the symbols are in our package.  (I think that's what I want.)
;;;
;;; Revision 1.15  2000/07/27 16:39:17  rtoy
;;; We want to be in the CL-USER package, not the USER package.
;;;
;;; Revision 1.14  2000/07/21 21:15:17  rtoy
;;; Fix a typo in the punctuation list and add "/" to the list.
;;;
;;; Revision 1.13  2000/07/21 17:43:17  rtoy
;;; Need to be more careful about continuation lines.  We now leave a
;;; space between lines when concatenating them, except when the previous
;;; line ends with punctuation, or the current line begins with
;;; punctuation.  This is intended to handle cases like:
;;;
;;; 	double precision
;;;        & x
;;;
;;; from being converted to
;;;
;;; 	double precisionx
;;;
;;; which is wrong.  We want a space before the x.
;;;
;;; We want to handle these cases to, though:
;;;
;;; 	x = y .le
;;;        &. 42
;;;
;;; which shouldn't get a space between lines because we want "x = y
;;; .le. 42", not the invalid "x = y .le . 42".
;;;
;;; Revision 1.12  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
