;; -*- Mode: LISP; Package: USER; Syntax: Common-lisp -*-

;; I, Donald Cohen (donc@{isi.edu,compsvcs.com,ap5.com} hereby
;; (1) assert that I am the author of this file and
;; (2) place it in the public domain.  Feb. 1, 2000.

(lisp::in-package "USER")

#|  Record the calls to given functions 
Unlike most function monitoring packages that tell you the number of
calls and total time spent in a function, this records the inputs,
outputs and times of individual calls.  It's meant for helping you to
see WHICH PARTICULAR calls are expensive.  It's also useful for debugging 
in that you can see a history of calls, when they enter and exit, etc.

Notes: 
- This works only on functions.
- It's best to avoid trace, advice, etc. on the functions you want to
  record, since these facilities redefine the function (as does this).
  Just untrace functions before recording and turn off recording before
  you trace them.

 How to use it

  In order to monitor a function one first prepares it for monitoring, then
  one can turn monitoring on and off at high frequency.  One can also reset
  or read the monitoring data for a function.  Finally one can forget about
  monitoring a function.

  *monitored-fns* is a list of functions currently prepared for monitoring.

  (prepare-record-calls '(f1 f2 f3)) prepares the functions named.
    additional keyword arguments: entryforms, exitforms, test
    The entryforms are evaluated at function entry, the exitforms at function
    exit.  This is where you can add things like the current process, 
    a cons counter, or your own application data.
    The results are recorded along with inputs, outputs, entry time,
    exit time and entry sequence number and exit sequence number.
    Test is a form (default is T) that determines whether this particular
    call will be recorded.  It runs in an environment where USER::ARGS
    is bound to the argument list of the function.

    Note that if you redefine a function after it is prepared for recording 
    the preparation is, in effect, undone but this package does not know
    about it.  However you can redo the prepare for it and then all will be
    well.
    
  (record-on '(f1 f2 f3)) turns on recording for these functions.

  (record-off '(f1 f2 f3)) turns it off.

  (initialize-records '(f1 f2 f3)) discards all monitoring data for the
    functions (but does not turn recording off or on and does not forget
    preparation).
    
  (recorded-calls 'f1) returns a list of the call records for f1.
    This is a list of call records (see call-rec below) containing
     inputs outputs start-time end-time entry-sequence# exit-sequence#
	<values of entry forms> <values of exit forms>
    Times are represented as integers - microseconds of run time.
    The sequence numbers are integers that increase with each call to be
    recorded.  The entry (exit) sequence number of one record is less than
    that of another iff the first call was entered (exited) earlier than
    the second.

  (forget-record-calls '(f1 f2 f3))
    discards all monitoring data and preparation

  The expected mode of usage is that you write your own programs to
  do the sorts of analysis you need.  However I do include here a
  few functions that provide a start.  
    
  (summarize-calls '(f1 f2 f3)) prints a summary of the calls.
    This is the traditional number of calls, total time, average.
    The argument defaults to *monitored-fns*.
    Additional optional argument: name-alist
    Name-alist is something like ((f1 . "updating database") (f2 . "waiting"))
    and is used to translate function names into something more meaningful.

  (longest-n-calls 'f2 3) returns the 3 longest recorded calls of f2
   additional keyword arguments: start end filterfn
   filterfn - a function of 1 arg (a call record)
    should return T if the call is "interesting"
   start/end are special cases - filter out anything that starts before start
    or ends after end

  (time-line '(f1 f2 f3) produces an ascii time line of activity
   additional keyword arguments: (width 80) filterfn start end name-alist
   This is handy for getting a pictorial view of when functions start and
   finish.

   Since calls are recorded by pushing onto a list at exit, they are ordered
   by decreasing exit time.  This is handy for finding the outermost calls
   in the case where the calls all come from the same process (and must thus
   be properly nested).
  (outermost (recorded-calls 'foo))
   returns the subset of the calls to foo that are outermost.

  (outline calls ...) prints an outline of a set of calls (presumed to
   be from the same process - who knows what will happen if they don't nest)
   See definition for more arguments.
  
  More sample code is in the comment at the end.

  ** this section de-implemented **
    Both symbolics and TI have a fast short clock and a slow long one.
    We use the fast one on symbolics, slow one on TI.
		      time before wrap around / #usec to read clock
		      --------------------------------------------
		      symbolics 3600           TI explorer II
	       fast   >.5 hour / 67   *        16 sec. / 260
	       slow   >100 yrs / 218           >1 hour / 260   *

    Actually we notice wrap around and record it - whenever a clock access
    returns a smaller value than the previous one we increment a counter.
    Therefore all events are ordered correctly, but if you fail to read the
    clock for an hour or so, it's as if that time never passed.  This is bad
    if you time things on such a coarse scale, but good if you time one thing
    for a minute today and something else for a minute tomorrow - the time
    line between such events never separates them by much more than an hour.
    In practice I don't think this will matter much.
  **

  2/17/89 - try to avoid advice, not so much because it's not commonlisp
  as because it's not compiled!  In fact, I want to be able to turn on and
  off recording at high frequency and encapsulations seem to get in the way
  of this.  For now I'll assume that one does not encapsulate and record the
  same functions.

  1/27/2000 - remove use of pp and update doc in preparation for export
|#

(defvar *monitored-fns* nil)
(defvar *enter-seq-num* 0)
(defvar *exit-seq-num* 0)
;(defvar *clock-cycle* 0)
;(defvar *last-time* 0)

(defstruct call-rec inputs outputs start end enter# exit# entryvals exitvals)

(defun prepare-record-calls (fns &key entryforms exitforms (test t))
  (loop for fn in fns do (prepare-record-call fn entryforms exitforms test)))

; record-calls-fn prop is cons substitute and original fns
(defun prepare-record-call (fn entryforms exitforms test &aux prop)
  (cond ((not (fboundp fn)) (error "no such function as ~A" fn))
	#+zetalisp
	((and (si:function-encapsulated-p fn)
	      (warn "~A is an encapsulation") nil))
	((and (setf prop (get fn 'record-calls-fn))
	      (or (eq (cdr prop) (symbol-function fn))
		  (eq (car prop) (symbol-function fn))))
	 (warn "~A already recorded - doing nothing" fn))
	;; note - if you redefine a function after preparing it
	;; the preparation goes away without us knowing about it
	;; but you can redo the prepare and then all is ok
	(t ; not cached ...
	 (setf (get fn 'record-calls-fn)
	       (cons (make-record-fn fn entryforms exitforms test)
		     (symbol-function fn)))
	 (pushnew fn *monitored-fns*))))

(defun make-record-fn (fn entryforms exitforms test)
  (compile nil
      `(lambda (&rest args &aux start values finish entryvals sn)
	 (if ,test
	     (unwind-protect
		 (progn (setq entryvals (list ,@entryforms)
			      sn (incf *enter-seq-num*)
			      start (microsec-time)
			      ; start1 *clock-cycle*
			      values (multiple-value-list
				       (apply ',(symbol-function fn) args))
			      finish (microsec-time) ; finish1 *clock-cycle*
			      )
			(values-list values))
	       (record-1-call ',fn (copy-list args)
			      (if finish values :abnormal-exit)
			      start ; start1
			      (or finish (microsec-time))
			      ; (or finish1 *clock-cycle*)
			      sn
			      entryvals
			      (list ,@exitforms)))
	     (apply ',(symbol-function fn) args)))))
; perhaps we should try to correct for the time spent in the new function?

(defun forget-record-calls (fns)
  (record-off fns)
  (loop for fn in fns do
    (setq *monitored-fns* (delete fn *monitored-fns*))
    (setf (get fn 'record-calls-fn) nil)
    (setf (get fn 'recorded-calls) nil)))

(defun record-on (fns)
  (loop for fn in fns do
        (let ((prop (get fn 'record-calls-fn)))
	  (cond ((not prop) (cerror "skip turning on recording"
				    "~A not prepared for recording" fn))
		((eq (cdr prop) (symbol-function fn))
		 (setf (symbol-function fn) (car prop)))
		;; if already on then it's a noop
		((eq (car prop) (symbol-function fn)))
		(t (cerror "skip turning on recording"
			   "~A has changed since last prepared for recording"
			   fn))))))

(defun record-off (fns)
  (loop for fn in fns do
        (let ((prop (get fn 'record-calls-fn)))
	  (cond ((not prop)
		 (cerror "continue" "~A not prepared for recording" fn))
		((eq (car prop) (symbol-function fn))
		 (setf (symbol-function fn) (cdr prop)))
		;; if already off then it's a noop
		((eq (cdr prop) (symbol-function fn)))
		(t (cerror "continue"
			   "~A has changed since recording last turned on"
			   fn))))))

(defun microsec-time ()
  ;; hmm - might end up using rational arithmetic if internal...second
  ;; does not divide a million ...
  (* (get-internal-run-time) (/ 1000000 internal-time-units-per-second)))

#+ignore ;; old version for lisp machine microsecond clocks
(defun microsec-time (&aux time)  
  (setq time
        #-(or symbolics ti)
	(* (get-internal-run-time) (/ 1000000 internal-time-units-per-second))
	#+symbolics (time:fixnum-microsecond-time)
	#+TI (time:microsecond-time))
  (when (< time *last-time*) (incf *clock-cycle*))
  (setf *last-time* time))

(defun record-1-call (fn inputs results t1 t2 seqno entryvals exitvals)
  (push (make-call-rec :inputs inputs :outputs results :start t1 :end t2
		       :enter# seqno :exit# (incf *exit-seq-num*)
		       :entryvals entryvals :exitvals exitvals)
	(get fn 'recorded-calls)))

(defun initialize-records (fns)
  (loop for fn in fns do (setf (get fn 'recorded-calls) nil)))

(defun recorded-calls (fn) (get fn 'recorded-calls))

(defun summarize-calls (&optional (fns *monitored-fns*) name-alist)
  (loop for fn in fns do
    (summarize-record fn (get fn 'recorded-calls) name-alist)))

(defun summarize-record (fn calls name-alist)
  (when calls (loop for x in calls sum 1 into ncalls
		    sum (elapsed (call-rec-start x) (call-rec-end x))
		    into time finally
		    (print-summarize-record fn ncalls time name-alist))))

(defun print-summarize-record (fn ncalls time name-alist)
  (multiple-value-bind (total tunits)
      (standardized-time-units time)
    (multiple-value-bind (avg aunits)
      (standardized-time-units (float (/ time ncalls)))
      (format *standard-output* "~%~A: ~A calls, ~A ~A (avg. ~A~:[ ~a~; ~])"
	  (or (cdr (assoc fn name-alist)) fn)
	  ncalls total tunits avg (eq aunits tunits) aunits))))

(defun standardized-time-units (usec)
  (cond ((> usec 999999) (values (float (/ usec 1000000)) "sec."))
	((> usec 999) (values (float (/ usec 1000))  "msec."))
	(t (values usec "usec."))))

(defun elapsed (t1 t2) (- t2 t1))

#+ignore ;; again, lisp machine version 
(defun elapsed (t1 t11 t2 t21)
  (+ (- t2 t1) (* (- t21 t11) (* 1024 1024 2048 #+TI 2))))

(defun longest-n-calls (fn n &key start end filterfn
			&aux next time current
			(candidates (recorded-calls fn)) (i 0))
  ; filterfn decides whether a record is "interesting"
  ; special cases: start/end filters out anything that starts before start
  ; or ends after end
  (flet ((filter (e) (and (or (null start)
			      (plusp (elapsed start (call-rec-start e))))
			  (or (null end)
			      (plusp (elapsed (call-rec-end e) end)))
			  (or (null filterfn) (funcall filterfn e)))))
    (loop while (and (< i n) (setq next (pop candidates)))
	  when (filter next)
	  do (incf i)
	     (push (cons (elapsed (call-rec-start next) (call-rec-end next))
			 next) current))
    (setq current (sort current #'<= :key #'car))
    (loop while (setq next (pop candidates))
	  when (filter next)
	  when (< (caar current)
		  (setq time (elapsed (call-rec-start next)
				      (call-rec-end next))))
	  do (setq current (merge 'list (cdr current)
				  (list (cons time next))
				  #'<= :key #'car)))
    (nreverse current)))

(defmacro totalt (x) x)
#+ignore 
; get the time represented by the two numbers x (low order) and y (high order)
(defun totalt (x y) (elapsed 0 0 x y))

(defvar *time-line-key*
  "Start time = ~A, End time = ~A, Width = ~A, ~
  ~& each column represents ~A ~A~
  ~& Key: ( = 1 entry, ) = 1 exit, * = more than one entry/exit~
  ~&      if no entry/exit, a digit indicates number of active calls,~
  ~&         blank indicates no change, + indicates >9 ~% ")

(defun time-line (fns &key (width 80) filterfn start end len name-alist
		    &aux events)
  (flet ((filter (e) (and (or (null start)
			      (plusp (elapsed start (call-rec-start e))))
			  (or (null end)
			      (plusp (elapsed (call-rec-end e) end)))
			  (or (null filterfn) (funcall filterfn e)))))
    (setq events (loop for f in fns collect
	 	       (cons f (loop for e in (recorded-calls f)
				     when (filter e) collect e))))
    (unless (and start end)
      (loop for e in events do
	    (loop for r in (cdr e) do
	          (when (or (null start)
			    (minusp (elapsed start (call-rec-start r))))
		    (setq start (totalt (call-rec-start r))))
		  (when (or (null end)
			    (minusp (elapsed (call-rec-end r) end)))
		    (setq end (totalt (call-rec-end r)))))))
    (when (and start end) (setq len (- end start)))
    (unless (and len (> len 0)) (return-from time-line "empty interval"))
    (multiple-value-bind (number unit)
	(when (and start end width)
	  (standardized-time-units (/ (- end start 0.0) width)))
      (apply #'concatenate 'string
	     (format nil *time-line-key* start end width number unit)
	     (loop for f in events collect
		   (concatenate 'string
		       (let ((string (make-string width
						  :initial-element #\space))
			     index
			     (countstart
			       (make-array (list width)
					   :initial-element 0
					   :element-type 'integer))
			     (countend
			       (make-array (list width) :initial-element 0
					   :element-type 'integer)))
			 (loop for e in (cdr f) do
			   (setq index
				 (min (1- width)
				      (floor (* width (/ (- (totalt
							     (call-rec-start e))
							    start)
							 len)))))
			   (incf (aref countstart index))
			   (setf (aref string index)
				 (if (char= #\space (aref string index))
				     #\( #\*))
			   (setq index
				 (min (1- width)
				      (floor (* width (/ (- (totalt
							     (call-rec-end e))
							    start)
							 len)))))
			   (decf (aref countend index))
			   (setf (aref string index)
				 (if (char= #\space (aref string index))
				     #\) #\*)))
			 (loop for i below width with sum = 0 do
			   (setf sum (+ sum (aref countstart i)
					(aref countend i)))
			   (when (and (/= i 0)
				      (/= (aref countstart (1- i)) 0)
				      (/= (aref countend (1- i)) 0)
				      (char= #\space (aref string i))
				      (> sum 0))
			     (setf (aref string i)
				   (if (> sum 9) #\+ (aref "0123456789" sum)))))
			 string)
		       (format nil "  ~A~& "
			       (symbol-name (or (cdr (assoc (car f) name-alist))
						(car f))))))))))

; relies on being sorted by decreasing exit time
(defun outermost (calls &aux outer)
  ; change to use sequence numbers instead of time
  (loop for c in calls
	unless (and outer
		    (<= (call-rec-enter# outer) (call-rec-enter# c)))
	collect (setf outer c)))


#| While I'm at it, perhaps a call tree would be nice:
((call1 (call2 (call3 call4) call5) call6) call7 call8)
or even printing an outline:
call1
  call2
    call3
      call4
    call5
  call6
call7
call8

In such a call outline, we want to print records in entry order, with
indentation proportional to the number of other records that both
entered earlier and exited later.
|#

; this seems to come up often
(defun call-time (x)
  (/ (- (call-rec-end x) (call-rec-start x)) 1000000.0))

(defun default-show-record (x depth)
  ; list structure that shows what we think we'll want to see
 (list (call-time x)
       :depth depth ; useful for searching the output
       ; useful for limiting the scope of later outlines
       :enter# (call-rec-enter# x) :exit# (call-rec-exit# x)
       (call-rec-inputs x)))

;; (require :pp) ;; just use the printer of your choice
(defun outline (calls &key
		      (stream *standard-output*) (rmargin 80)
		      (printer 'princ)
		      (indentation 3) (showfn #'default-show-record)
		      (max-depth 15) ; don't go deeper than this
		      &aux (sorted (sort (copy-list calls) #'<=
					 :key #'call-rec-enter#))
		      open depth nspaces)
  (loop for c in sorted do
	(setf open
	      (loop for o in open when
		    (and (< (call-rec-enter# o) (call-rec-enter# c))
			 (< (call-rec-exit# c) (call-rec-exit# o)))
		    collect o))
	(setf depth (length open))
	(if (> depth max-depth)
	  (princ "."stream) ; just to indicate that something's there
	  (progn
	   (terpri stream)
	   (setf nspaces (mod (* indentation depth) (/ rmargin 2)))
	   ; don't space out too far
	   (loop for i below nspaces do (princ " " stream))
	   ;;(moveto stream 1 nspaces) ;; from pp
	   (funcall printer (funcall showfn c depth) stream)))
	(push c open)))

(defun call-inside (c1 c2) ; c1 is inside c2
  (and (< (call-rec-enter# c2) (call-rec-enter# c1))
       (< (call-rec-exit# c1)  (call-rec-exit# c2))))

; useful for filtering sets of records
(defun calls-between (calls &key (enter 0) (exit 999999))
  (loop for c in calls
	when (and (<= enter (call-rec-enter# c)) (<= (call-rec-exit# c) exit))
	collect c))

#|
 sample code for finding the "internal" time for each call, i.e., the
 time not included in any call made by that one:

 ; all calls
 (length (setf all (loop for x in *monitored-fns* append
			 (recorded-calls x))))
 ; sorted so outermost works on it
 (length (setf all (sort (copy-list all) #'>= :key #'call-rec-exit#)))
 ; compute internal time (stored in exitvals)
 (loop for a in all do
       (setf (call-rec-exitvals a)
	     (- (call-time a)
		(loop for c in
		      (outermost
		       (cdr (calls-between all
				 :enter (call-rec-enter# a)
				 :exit (call-rec-exit# a))))
		      sum (call-time c)))))
 ; sorted by internal time
 (length (setf z (sort (copy-list all) #'>= :key #'call-rec-exitvals)))

 ; while I'm at it, store the function name in the entryvals
 (loop for f in *monitored-fns* do
       (loop for c in (recorded-calls f) do
	     (setf (call-rec-entryvals c) f)))
 ; show function to highlight internal time
 (defun show-call (x &optional depth)
  (list (call-rec-exitvals x) ; internal time
	(call-rec-entryvals x) ; fn
	; useful for limiting the scope of later outlines
	:enter# (call-rec-enter# x) :exit# (call-rec-exit# x)
	:start (/ (call-rec-start x) 1000000.0)
	:end (/ (call-rec-end x) 1000000.0)
	:total-time (call-time x)
	(call-rec-inputs x)))
 ; show top 10
 (loop for i below 10 as a in z do (terpri)(princ (show-call a)))
|#