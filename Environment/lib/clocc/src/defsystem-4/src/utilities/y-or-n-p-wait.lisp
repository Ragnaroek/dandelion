;;; -*- Mode: CLtL -*-

;;; y-or-n-p-wait --

(in-package "MK4")			; Maybe this functionality
					; should be in its own
					; package.

;;; ********************************
;;; Y-OR-N-P-WAIT ******************
;;; ********************************
;;; Y-OR-N-P-WAIT is like Y-OR-N-P, but will timeout after a specified
;;; number of seconds. I should really replace this with a call to
;;; the Y-OR-N-P-WAIT defined in the query.cl package and include that
;;; instead.

(defparameter *use-timeouts* t
 "If T, timeouts in Y-OR-N-P-WAIT are enabled.
Otherwise it behaves like Y-OR-N-P. This is provided for users whose
lisps don't handle READ-CHAR-NO-HANG properly.")

(defparameter *clear-input-before-query* t
 "If T, Y-OR-N-P-WAIT will clear the input before printing the prompt
and asking the user for input.")

;;; The higher *sleep-amount* is, the less consing, but the lower the
;;; responsiveness.
(defparameter *sleep-amount* #-CMU 0.1 #+CMU 1.0
 "Amount of time to sleep between checking *QUERY-IO*.
In multiprocessing Lisps, this allows other processes to continue
while we busy-wait. If 0, skips call to SLEEP.")

(defun internal-real-time-in-seconds ()
  (get-universal-time))

(defun read-char-wait (&optional (timeout 20)
				 input-stream
                                 (eof-error-p t)
				 eof-value
                                 &aux
				 peek)
  (do ((start (internal-real-time-in-seconds)))
      ((or (setq peek (listen input-stream))
           (< (+ start timeout) (internal-real-time-in-seconds)))
       (when peek
         ;; was read-char-no-hang
         (read-char input-stream eof-error-p eof-value)))
    (unless (zerop *sleep-amount*)
      (sleep *sleep-amount*))))

;;; Lots of lisps, especially those that run on top of UNIX, do not get
;;; their input one character at a time, but a whole line at a time because
;;; of the buffering done by the UNIX system. This causes y-or-n-p-wait
;;; to not always work as expected. 
;;;
;;; I wish lisp did all its own buffering (turning off UNIX input line
;;; buffering by putting the UNIX into CBREAK mode). Of course, this means
;;; that we lose input editing, but why can't the lisp implement this? 

(defun y-or-n-p-wait (&optional (default #\y)
				(timeout 20) 
				format-string
				&rest args)
  "Y-OR-N-P-WAIT prints the message, if any, and reads characters from
   *QUERY-IO* until the user enters y, Y or space as an affirmative, or either
   n or N as a negative answer, or the timeout occurs. It asks again if
   you enter any other characters."
  (when *clear-input-before-query* (clear-input *query-io*))
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    ;; FINISH-OUTPUT needed for CMU and other places which don't handle
    ;; output streams nicely. This prevents it from continuing and
    ;; reading the query until the prompt has been printed.
    (finish-output *query-io*))
  (loop
   (let* ((read-char (if *use-timeouts*
			 (read-char-wait timeout *query-io* nil nil)
			 (read-char *query-io*)))
	  (char (or read-char default)))
     ;; We need to ignore #\newline because otherwise the bugs in 
     ;; clear-input will cause y-or-n-p-wait to print the "Type ..."
     ;; message every time... *sigh*
     ;; Anyway, we might want to use this to ignore whitespace once
     ;; clear-input is fixed.
     (unless (find char '(#\tab #\newline #\return))
       (when (null read-char) 
	 (format *query-io* "~@[~A~]" default)
	 (finish-output *query-io*))
       (cond ((null char) (return t))
	     ((find char '(#\y #\Y #\space) :test #'char=) (return t))
	     ((find char '(#\n #\N) :test #'char=) (return nil))
	     (t 
	      (when *clear-input-before-query* (clear-input *query-io*))
	      (format *query-io* "~&Type \"y\" for yes or \"n\" for no. ")
	      (when format-string
		(fresh-line *query-io*)
		(apply #'format *query-io* format-string args))
	      (finish-output *query-io*)))))))

#|
(y-or-n-p-wait #\y 20 "What? ")
(progn (format t "~&hi") (finish-output)
       (y-or-n-p-wait #\y 10 "1? ")
       (y-or-n-p-wait #\n 10 "2? "))
|#

;;; end of file -- y-or-n-p-wait --
