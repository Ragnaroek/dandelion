;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: fload.lisp,v 2.3 2006/05/25 20:01:19 airfoyle Exp $

;;; Copyright (C) 1976-2005
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(fload filespecs-load fcompl filespecs-compile
	     fcompl-load* fload-compile* bind-fload-compile*
	     fload-versions fload-version-suffix*
             postponed-files-update 
	     warn-about-postponed-file-chunks
	     warn-about-postponed-file-chunks* ask-about-fload-version-mgt*)))

(defvar file-op-count* 0)

(defparameter fload-flags* '(#\f #\c #\a #\s #\o #\n #\x #\z))
(defparameter filespecs-load-flags* '(#\c #\s #\o #\z))
     ;;;; ;;; -  -> "Clear sticky flags"
;;; -f -> "Force load even if apparently up to date"
;;; -c -> "Compile and load each file even if apparently up to date"
;;; -a -> "Ask whether to compile each file"
;;; -s -> "Load source, ignoring possibility of loading object"
;;; -o -> "Load object, without recompiling"
;;; -n -> "Do whatever fload-compile* says" ("normal" case)
;;; -x -> "Stop managing loaded file (i.e., stop loading)"
;;; -z -> "Postpone update of chunks for files supported by this one"

(defmacro fload (&rest specs)
  `(do-fload ',specs))

;;; files, flags, readtable
(defvar default-fload-args* (vector !() !() nil))

(defvar postponed-file-chunks* !())

(defun do-fload (specs)
   (labels ((do-it ()
	       (with-compilation-unit ()
		  (file-op-defaults-update 'fload specs fload-flags*
					 (lambda () default-fload-args*)
					 (lambda (a)
					    (setq default-fload-args* a)))
		  (apply #'filespecs-do-load
			 (coerce default-fload-args* 'list)))))
      (cond (file-op-in-progress*
	     (do-it))
	    (t
	     (setq file-op-count* (+ file-op-count* 1))
	     (let ((before-num-postponed
		       (num-out-of-date-postponed-file-chunks)))
		(cond ((not (= before-num-postponed 0))
		       (warn-about-postponed-file-chunks
			   before-num-postponed)))
		(let ((file-op-in-progress* true))
		   (catch 'fload-abort
		      (do-it)))
		(let ((after-num-postponed
		         (num-out-of-date-postponed-file-chunks)))
		   (cond ((not (= after-num-postponed
				  before-num-postponed))
			  (warn-about-postponed-file-chunks
			     after-num-postponed)))
		   after-num-postponed))))))

(defun filespecs-load (specs &optional (flags !()) (rt *readtable*))
   (filespecs-do-load
       specs
       (flags-check flags filespecs-load-flags*)
       rt))

(defun filespecs-do-load (specs flags *readtable*)
   (let ((*load-verbose* false))
      (let ((force-flag false)
	    (file-manip false)
	    (postpone-derivees false))
	 (labels ((set-manip (v force-too flag)
		     (cond (file-manip
			    (format *error-output*
			       "Ignoring contradictory flag '~a' to fload"
			       flag))
			   (t
			    (setq file-manip v)
			    (cond (force-too
				   (setq force-flag v))))))
		  (set-force (v flag)
		     (cond (force-flag
			    (format *error-output*
			       "Ignoring contradictory flag '~a' to fload"
			       flag))
			   (t
			    (setq force-flag v))))
		  (ask-user-to-resolve-load-flags (stated-manip stated-force-flag)
		     (let (choice)
			(loop 
			   (format *query-io*
			      "Which do you really mean: ~s or ~s? "
			      stated-manip stated-force-flag)
			   (setq choice (read *query-io*))
			   (cond ((eq choice stated-manip)
				  (return (lambda () 
					     (setq force-flag false))))
				 ((eq choice stated-force-flag)
				  (return (lambda ()
					     (setq file-manip false)))))))))
	    (dolist (flag flags)
	       (cond ((eql flag '#\z)
		      (setq postpone-derivees true))
		     (t
		      (case flag
			 (#\s
			  (set-manip ':source false '#\s))
			 (#\o
			  (set-manip ':object false '#\o))
			 (#\n
			  (set-manip ':defer false '#\n))
			 (#\a
			  (set-manip ':ask-ask false '#\a))
			 (#\f
			  (set-force ':load '#\f))
			 (#\c
			  (set-force ':compile '#\c))
			 (#\x
			  (set-force ':noload '#\x))
			 (t
			  (cerror "I will ignore it"
				  "Illegal flag to 'fload': '~a'" flag))))))
	    ;; Complain about various contradictory pairings --
	   (cond ((and (or file-manip postpone-derivees)
		       (eq force-flag ':noload))
		  (cerror "I will ignore it"
			  "Flag '-x' contradicts presence of other flags")
		  (setq force-flag false))
		 ((and force-flag
		       (memq file-manip '(:defer :ask)))
		  (cerror "I will ignore it"
			  "Forcing flag '~a' contradicts file-manip instructions"
			  (cond ((eq force-flag ':load) '#\f)
				(t '#\c)))
                  (setq force-flag false))
		 ((member (list file-manip force-flag)
			  '((:source :compile))
			  :test #'equal)
		  (restart-case
		      (error "Contradictory instructions ~s/~s derived from flags"
			     file-manip force-flag)
		    (error (resolver)
		     :report "I will prompt for which to do"
		     :interactive (lambda ()
				     (ask-user-to-resolve-load-flags
					  file-manip force-flag))
		       (funcall resolver))))))
	(dolist (pn (filespecs->ytools-pathnames specs))
  	   (filoid-fload pn :force-load force-flag
			    :manip file-manip
			    :postpone-derivees postpone-derivees)
	 ))))

;;; A class with just one instance, representing the fload-compile-flag*'s
;;; being set to the user's desired value --
(defclass Fload-compile-flag-set (Chunk)
   ((value :accessor Fload-compile-flag-value
	   :initform fload-compile-flag*)))

(defmethod derive ((fcf Fload-compile-flag-set))
   (cond ((eq (Fload-compile-flag-value fcf) fload-compile-flag*)
	  false)
	 (t
	  (setf (Fload-compile-flag-value fcf) fload-compile-flag*)
	  true)))

(defun default-fload-manip () fload-compile-flag*)

(defun (setf default-fload-manip) (v)
   (cond ((memq v '(:ask :source :object :compile
		    :ask-ask :ask-once :ask-every))
	  (cond ((not (eq v fload-compile-flag*))
		 (format *error-output*
		    !"Change of fload-compile* from ~s to ~s ~% triggers ~
                      file-chunk rescan~%"
		    fload-compile-flag* v)
		 (setq fload-compile-flag* v)
		 (loadeds-check-bases)
		 (format *error-output*
		    "File-chunk rescan complete~%"))))
	 (t
	  (cerror "I will leave the value unchanged"
		  !"Illegal value ~s for fload-compile*"
		  v)))
  v)

(define-symbol-macro fload-compile* (default-fload-manip))

;; Must use this to "bind" fload-compile* --
(defmacro bind-fload-compile* (newval &body b)
   `(let ((fload-compile-flag* fload-compile-flag*))
       (setf fload-compile* ,newval)
       ,@b))

(defvar fload-compile-flag-chunk* (make-instance 'Fload-compile-flag-set))

(defvar postpone-recursive-rescans* true)

(defun loadeds-check-bases ()
   (let ((loadeds-needing-checking !()))
      (dolist (lc all-loaded-file-chunks*)
	 (cond ((memq (Loaded-file-chunk-manip lc)
		      '(:defer :follow))
		(format *error-output*
		   "  File-chunk rescan nets ~s~%"
		   lc)
		(on-list lc loadeds-needing-checking))))
      (cond ((or (not postpone-recursive-rescans*)
		 (= chunk-update-depth* 0))
	     (chunks-update (cons fload-compile-flag-chunk*
				  loadeds-needing-checking)
			    false false))
	    (t
	     ;; If update is already in progress, then we are quite
	     ;; likely to get a meta-cycle if we call 'chunks-update'
	     ;; immediately.
	     (file-ops-maybe-postpone
	        (cons fload-compile-flag-chunk*
		      loadeds-needing-checking))))))

;;; 'filoid-fload' is the entry point to declare that a filoid
;;; should be loaded, figure out its basis, and tell the chunk
;;; network to load it.  
;;; It should never be called _by_ a filoid (Loaded-) chunk deriver, 
;;; because it alters the bases of such chunks. 
(defgeneric filoid-fload (pn &key force-load manip postpone-derivees))

(defmethod filoid-fload ((ytpn YTools-pathname)
			 &key force-load manip postpone-derivees)
   (filoid-fload (pathname-resolve ytpn false)
		 :force-load force-load
		 :manip manip
		 :postpone-derivees postpone-derivees))

(defmethod filoid-fload ((pn pathname)
			 &key force-load manip postpone-derivees)
   (let* ((pchunk (pathname-denotation-chunk pn true))
	  (lpchunk (place-Loaded-chunk pchunk manip)))
      (cond ((not (eq manip ':noload))
	     (loaded-chunk-fload lpchunk force-load postpone-derivees)))))

(defun loaded-chunk-fload (loaded-chunk force-load postpone-derivees)
   (cond ((eq force-load ':compile)
	  ;; Treat (fload -c <file>) as = (fcompl -f -l <file>)
	  (let ((src-file-chunk
		   (Loaded-file-chunk-source loaded-chunk)))
	     (cond (src-file-chunk
		    (filoid-fcompl
		       (Code-file-chunk-pathname src-file-chunk)
		       :force-compile true
		       :load true
		       :postpone-derivees true))
		   (t
		    (format *error-output*
		       !"Ignoring request to compile ~s;~
                         ~% there is no source file"
		       loaded-chunk)))))
	 ((eq force-load ':noload)
	  (chunk-terminate-mgt loaded-chunk ':ask))
	 (t
	  (monitor-filoid-basis loaded-chunk)
	  (cond ((loaded-chunk-set-basis loaded-chunk)
		 (chunk-request-mgt loaded-chunk)
		 (let ((d (Chunk-date loaded-chunk)))
		    (file-ops-maybe-postpone
		       (chunk-update loaded-chunk false postpone-derivees))
		    (cond ((and force-load
				(= (Chunk-date loaded-chunk)
				   d))
			   ;; It was apparently already up to date,
			   ;; so forcing makes sense
			   (loaded-chunk-force
			      loaded-chunk force-load)))))))))

(defgeneric loaded-chunk-force (loaded-chunk force))

(defvar fcompl-load* ':ask)

(defmethod loaded-chunk-force ((loaded-chunk Loaded-file-chunk)
			       force)
   (let ((obj-file-chunk (Loaded-file-chunk-object loaded-chunk))
	 (src-file-chunk (Loaded-file-chunk-source loaded-chunk)))
      ;; If force = :load, load object if it exists --
      (cond ((eq force ':load)
	     (setq force
		   (cond ((and obj-file-chunk
			       (probe-file
				  (Code-file-chunk-pathname obj-file-chunk)))
			  ':object)
			 (t
			  ':source)))))
      (let ((operative-chunk
	       (ecase force
		  ((:source :object)
		   (place-Loaded-source-chunk
		      (cond ((eq force ':object)
			     obj-file-chunk)
			    (t
			     src-file-chunk))))
		  ((:compile)
		   (Loaded-file-chunk-compiled loaded-chunk)))))
	(let ((fcompl-load* true))
	   (file-ops-maybe-postpone
	      (chunk-update operative-chunk true true))))))

;;;;   (pathname-is-source (Code-file-chunk-pathname file-ch))

;;;;(defvar final-load* false)
;;;; When building systems, bind to true to minimize soul-searching later.

;;; 'loaded-ch' is a Loaded-file-chunk
(defun ask-user-for-manip (loaded-ch obj-exists)
   (let ()
      (loop 
	 (format *query-io*
	    !"Do you want to load the object or source version of ~
              ~% of ~s~% (o[bject]/s[ource]/+/-, ~
              \\! to cancel \\\\ to abort)? "
	      (Code-file-chunk-pn (Loaded-chunk-loadee loaded-ch)))
;;;;	 (cond ((is-lsy-loaded-ch loaded-ch)
;;;;		(dbg-save loaded-ch obj-exists)
;;;;		(signal-problem ask-user-for-manip
;;;;		   "Asking about " loaded-ch)))
	 (multiple-value-bind
	          (manip general)
		  (case (keyword-if-sym (read *query-io*))
		     ((:s :source)
		      (values ':source false))
		     ((:o :object)
		      (values (cond (obj-exists
				     ':object)
				    (t ':compile))
			      false))
		     (:+
		      (setq fload-compile* ':compile)
		      (values ':compile true))
		     (:-
		      (setq fload-compile* ':object)
		      (values ':object true))
		     ((:c :compile)
		      (values ':compile false))
		     ((:\\)
		      (throw 'fload-abort 'fload-aborted))
		     ((:\!)
		      (values ':cancel false))
		     (t
		      (values false nil)))
;;;;	    (format *query-io* "manip = ~s general = ~s~%"
;;;;		    manip general)
	    (cond (manip
		   (cond ((eq manip ':cancel)
			  (return ':cancel))
			 (general 
			  (setf (Loaded-file-chunk-manip loaded-ch)
				manip)
			  (return manip))
			 (t
			  (return (manip-refine-remember manip loaded-ch)))))
		  (t
		   (format *query-io*
			 !"Type 's' or 'source' to load source file;~%~
                           type 'o' or 'object' to load object file;~%~
                           type '+' to recompile this and every subsequent ~
                           file from now on;~%~
                           type '-' to load object files without recompiling ~
                           whenever possible, from now on.~%~
                           type \! to stop trying to load this file~%")))))))

(setq user-manip-asker* !'ask-user-for-manip)

(defun is-lsy-loaded-ch (loaded-ch)
   (let* ((file-ch (Loaded-chunk-loadee loaded-ch))
	  (pn (Code-file-chunk-pathname file-ch)))
      (cond ((not pn)
	     (cerror "I'll go on"
		     "File chunk has no pathname: " file-ch)
	     false)
	    (t
	     (string= (Pathname-type pn) "lsy")))))			     

(defun manip-refine-remember (manip loaded-ch)
   (cond ((eq manip ':object)
	  (cond ((y-or-n-p 
		    !"Do you want to check file write times and recompile ~
		      if necessary? ")
		 (setq manip ':compile)))))
   (block dialogue
      (let ((old-manip (Loaded-file-chunk-manip loaded-ch)))
	 (cond ((eq old-manip ':ask-once)
		(setf (Loaded-file-chunk-manip loaded-ch)
		       manip))
	       ((eq old-manip ':ask-ask)
		(loop
		   (format *query-io*
			   !"Record this choice for future encounters ~
                             with this file (y/n/d, \\\\ to abort)? ")
		   (case (keyword-if-sym (read *query-io*))
		      ((:y :yes :t)
		       (setf (Loaded-file-chunk-manip loaded-ch)
			     manip)
		       (ask-if-generalize manip)
		       (return-from dialogue))
		      ((:n :no)
		       (loop
			  (format *query-io*
				  !"Ask again next time whether to record ~
				    (y/n, \\\\ to abort)? ")
			  (let ((q (keyword-if-sym (read *query-io*))))
			     (case q
			        ((:n :no)
				 (return-from dialogue))
				((:y :yes)
				 (setf (Loaded-file-chunk-manip loaded-ch)
				       ':ask-every)
				 (return-from dialogue))
				((:\\)
				 (throw 'fload-abort 'fload-aborted))
				(t
				 (format *query-io*
				    !"Type 'y' to ensure that every question ~
                                      about what to load will be followed by ~
                                      a~%  ~
				      question about whether to record;~%~
				      type 'n' to suppress that second ~
				      annoying question.~%"))))))
		      ((:d :defer)
		       (setf (Loaded-file-chunk-manip loaded-ch)
			     ':defer)
		       (return-from dialogue))
		      ((:\\)
		       (throw 'fload-abort 'fload-aborted))
		      (t
		       (format *query-io*
			  "Type 'y' or 'yes' to record ~s as form of ~s;~%~
                           type 'n' or 'no' to use that value once, ~
                           ask again next;~%~
                           type 'd' or 'defer' to use value of ~
                           'fload-compile* to decide each time.~%"
			  manip old-manip)))))
	       (t
		(format *query-io*
		     "[~s -becomes-> ~s]~%"
		     old-manip manip)))))
  manip)

(defun ask-if-generalize (manip)
   (cond ((and (not (eq fload-compile* manip))
	       (y-or-n-p
		  "Should the same decision apply to all other files? "))
	  (setq fload-compile* manip))))

(defparameter fcompl-flags* '(#\f #\x #\l #\z))
(defparameter filespecs-compile-flags* '(#\l #\z))
;;; -x -> "Stop managing compiled file (i.e., stop compiling)"
;;; -l -> "Load after compile" (just this time)
;;; -f -> "Force compile even if apparently up to date"
;;; -z -> "Postpone update of chunks for files supported by this one"

(defmacro fcompl (&rest specs)
  `(do-fcompl ',specs))

;;; files, flags, readtable
(defvar default-fcompl-args* (vector !() !() false))

(defun do-fcompl (specs)
   (labels ((do-it ()
	       (with-compilation-unit ()
		  (file-op-defaults-update 'fcompl specs fcompl-flags*
					   (lambda () default-fcompl-args*)
					   (lambda (a)
					     (setq default-fcompl-args* a)))
		  (apply #'filespecs-do-compile
			 (coerce default-fcompl-args* 'list)))))
      (cond (file-op-in-progress*
	     (do-it))
	    (t
	     (setq file-op-count* (+ file-op-count* 1))
	     (let ((before-num-postponed
		      (num-out-of-date-postponed-file-chunks)))
		(cond ((not (= before-num-postponed 0))
		       (warn-about-postponed-file-chunks
			   before-num-postponed)))
		(let ((file-op-in-progress* true))
		   (catch 'fload-abort
		      (do-it)))
		(let ((after-num-postponed
		          (num-out-of-date-postponed-file-chunks)))
		   (cond ((not (= after-num-postponed
				  before-num-postponed))
			  (warn-about-postponed-file-chunks
			     after-num-postponed)))
		   after-num-postponed))))))

(defun filespecs-compile (specs flags rt)
   (filespecs-do-compile
      specs
      (flags-check flags filespecs-compile-flags*)
      rt))

(defun filespecs-do-compile (specs flags *readtable*)
;;;;   (dbg-save specs flags)
;;;;   (breakpoint filespecs-do-compile
;;;;      "specs = " specs)
   (let ((*load-verbose* false))
      (let ((force-flag false)
	    (load-flag false)
	    (cease-mgt false)
	    (postpone-derivees false))
	(dolist (flag flags)
	    (case flag
	       (#\f (setq force-flag true))
	       (#\l (setq load-flag true))
	       (#\x (setq cease-mgt true))
	       (#\z (setq postpone-derivees true))
	       (t (cerror "I will ignore it"
			  "Illegal flag to 'fcompl': ~s" flag))))
	(dolist (pn (filespecs->ytools-pathnames specs))
	   (filoid-fcompl pn
			    :force-compile force-flag
			    :load load-flag
			    :cease-mgt cease-mgt
			    :postpone-derivees postpone-derivees)))))

(defgeneric filoid-fcompl (pn &key force-compile
				     load
				     cease-mgt
				     postpone-derivees))

(defmethod filoid-fcompl ((ytpn YTools-pathname)
			  &key force-compile load cease-mgt postpone-derivees)
   (filoid-fcompl (pathname-resolve ytpn false)
		  :force-compile force-compile
		  :load load
		  :cease-mgt cease-mgt
		  :postpone-derivees postpone-derivees))

(defmethod filoid-fcompl ((pn pathname)
			    &key force-compile
				 load
				 cease-mgt
				 postpone-derivees)
   (let* ((file-chunk
	     (pathname-denotation-chunk pn true))
	  (lpchunk (place-Loaded-chunk
		      file-chunk
		      false))
	  (compiled-chunk
	     (place-compiled-chunk file-chunk))
	  (object-file-chunk
	     (Compiled-file-chunk-object-file compiled-chunk))
	  (object-pathname
	     (Code-file-chunk-pathname object-file-chunk))
	  (object-file-date
	     (and (probe-file object-pathname)
		  (file-write-date object-pathname)))
;;;;	  (comp-date
;;;;	     (pathname-write-time
;;;;	        (Code-file-chunk-pathname compiled-chunk)))
	  )
      (labels ((force-compile ()
		  (file-ops-maybe-postpone
		     (chunks-update (list compiled-chunk)
				    true true)))
	       (consider-loading ()
		  (cond ((and (not (chunk-up-to-date lpchunk))
			      (probe-file object-pathname)
			      (or (not object-file-date)
				  (> (file-write-date object-pathname)
				     object-file-date))
			      (or load (load-after-compile)))
			 (file-ops-maybe-postpone
			    (chunks-update (list lpchunk)
					   true postpone-derivees))))))
	 (monitor-filoid-basis lpchunk)
	 (cond (cease-mgt
		(chunk-terminate-mgt compiled-chunk ':ask)
		(cond (force-compile
		       ;; One last fling --
		       (force-compile)
		       (consider-loading))))
	       (t
		(let (
;;;;		      (comp-date
;;;;			 (Chunk-date compiled-chunk))
		      )
		   (setf (Loaded-file-chunk-manip lpchunk)
			 ':compile)
		   (chunk-request-mgt compiled-chunk)
		   (cond (force-compile
			  (force-compile)))
		   (file-ops-maybe-postpone
		      (chunk-update compiled-chunk false postpone-derivees))
;;;;		   (cond ((and force-compile
;;;;			       (= (Chunk-date compiled-chunk)
;;;;				  comp-date))
;;;;			  ;; Hasn't been compiled yet
;;;;			  (force-compile)
;;;;			  (chunk-derive-and-record compiled-chunk)
;;;;			  ))
		   (consider-loading)))))))

(defun num-out-of-date-postponed-file-chunks ()
   (reduce (\\ (tot ch)
	      (cond ((chunk-up-to-date ch) tot)
		    (t (+ tot 1))))
	   postponed-file-chunks*
	   :initial-value 0))

(defvar warn-about-postponed-file-chunks* true)

(defun warn-about-postponed-file-chunks
          (&optional (num-postponed (len postponed-file-chunks*)))
   (cond (warn-about-postponed-file-chunks*
	  (format *error-output*
	      !"There are ~s file chunks waiting to be updated ~
                ~%   [when you call (postponed-files-update)]~%"
	     num-postponed))))

(defun fcompl-log (src-pn obj-pn-if-succeeded)
  (let ((log-pn (pathname-resolve
		   (make-Pathname :host (Pathname-host src-pn)
				  :device (Pathname-device src-pn)
				  :directory (Pathname-directory src-pn)
				  :name "Niscom_Log")
		   true)))
    (let ((oldlog
	   (cond ((probe-file log-pn)
		  (with-open-file (ss log-pn :direction ':input)
                     (read ss)))
		 (t !())))
	  (fname (build-symbol (Pathname-name src-pn))))
       (let ((newlog
		(cond (obj-pn-if-succeeded
                       (cons (list fname
                                   (current-time-string)
                                   ;;;;(compile-opt-lev)
				   )
                             (mapcan (lambda (x)
					(cond ((eq (car x) fname)
					       '())
					      (t (list x))))
				     oldlog)))
                      (t
		       (cons (list fname
                                   (current-time-string)
                                   "Compilation failed")
                             oldlog)))))
          (with-open-file (ss log-pn :direction ':output
			             :if-exists ':supersede)
             (let  ((*print-level* nil)
                    (*print-length* nil)
		    (*print-pretty* t))
		(prin1 newlog ss)
		))))))

(setq fcompl-logger* !'fcompl-log)

;;;;(defun compile-opt-lev () `((compile-opt ,compile-opt*)))

(defun current-time-string ()
   (multiple-value-bind (sec mnt hr day mo yr wkday dst tz)
                       (get-decoded-time)
                       (declare (ignore sec wkday dst tz))
      (with-output-to-string (ss)
         (format ss "~s:~s ~a ~s ~s"
              hr mnt
	      (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
		    (- mo 1))
	      day yr))))

(defun load-after-compile ()
   (and fcompl-load*
	(or (not (eq fcompl-load* ':ask))
	    (progn
	       (format *query-io*
		    "Load newly compiled file? ")
               (let ((r (keyword-if-sym
			   (read *query-io* false false))))
                  (cond ((memq r '(:-))
                         (setq fcompl-load* false))
                        ((memq r '(:+))
                         (setq fcompl-load* true))
                        (t
                         (memq r '(:y :yes :t)))))))))

(defvar fload-version-suffix* ':-new)

(defmacro fload-versions (&rest specs)
   (let ((olds (mapcar (lambda (x) 
			  (cond ((consp x) (car x))
				(t x)))
		       specs))
	 (news (mapcar (lambda (x)
			  (cond ((consp x)
                                 (cond ((null (cdr x))
                                        (build-symbol
					   (:< (car x))
					   (:< fload-version-suffix*)))
				       (t
					(let ((delta (cadr x)))
					   (cond ((or (is-String delta)
						      (is-Keyword delta)
						      (and (is-Symbol delta)
							   (not (symbol-package
								    delta))))
						  (build-symbol
						     (:< (car x)) (:< delta)))
						 (t delta))))))
				(t x)))
		       specs)))
      `(fload-versions-setup ',olds ',news)))

(defvar ask-about-fload-version-mgt* true)

(defun fload-versions-setup (olds news)
   (multiple-value-bind (set-olds set-news reset-olds)
                        (labels ((segregate (olds news)
                                    (cond ((null news)
                                           (values !() !() !()))
                                          (t
                                           (multiple-value-bind
                                                   (so sn rso)
                                                   (segregate (cdr olds)
                                                              (cdr news))
                                              (cond ((eq (car news) '-)
                                                     (values so sn
                                                             (cons (car olds)
                                                                   rso)))
                                                    ((eq (car news)
							 (car olds))
                                                     (values
                                                        (cons (car olds) so)
                                                        (cons (car news) sn)
                                                        (cons (car olds) rso)))
                                                    (t
                                                     (values
                                                        (cons (car olds) so)
                                                        (cons (car news) sn)
                                                        rso))))))))
                            (segregate olds news))
      (let ((changing-chunks !()))
	 (do ((oldl (filespecs->ytools-pathnames set-olds)
		    (tail oldl))
	      (newl (filespecs->ytools-pathnames set-news)
		    (tail newl)))
	     ((null oldl))
	    (let* ((old-cfc (pathname-denotation-chunk (head oldl) false))
		   (old-av (Code-chunk-alt-version old-cfc))
		   (new-cfc (pathname-denotation-chunk (head newl) false)))
	       (cond ((not (eq old-cfc new-cfc))
		      (setf (Code-chunk-alt-version old-cfc)
			    new-cfc)
		      (cond ((and old-av
				  (Chunk-manage-request old-av)
				  (or (not ask-about-fload-version-mgt*)
				      (y-or-n-p
					    !"Cancel request to manage ~s ~
					      [probably yes]?"
					    old-av)))
			     (on-list old-av changing-chunks)))
		      (cond ((and (Chunk-managed old-cfc)
				  (not (Chunk-manage-request new-cfc))
				  (or (not ask-about-fload-version-mgt*)
				      (y-or-n-p !"Begin management of ~s ~
						  ~%     [probably yes]? "
						new-cfc)))
			     (on-list new-cfc changing-chunks)))))))
	 (do ((oldl (filespecs->ytools-pathnames reset-olds)
		    (tail oldl)))
	     ((null oldl))
	    (let* ((rfc (pathname-denotation-chunk (head oldl) false))
		   (old-av (Code-chunk-alt-version rfc)))
	       (cond (old-av
		      (setf (Code-chunk-alt-version rfc)
			    false)
		      (cond ((and (Chunk-manage-request old-av)
				  (or (not ask-about-fload-version-mgt*)
				      (y-or-n-p !"Cancel request to manage ~s ~
						  [probably yes]?"
						old-av)))
			     (on-list old-av changing-chunks)))))))
	 (do ((ccl changing-chunks (tail ccl))
	      cc)
	     ((null ccl))
	    (setq cc (head ccl))
	    (cond ((Chunk-manage-request cc)
		   (chunk-terminate-mgt cc false))
		  (t
		   (chunk-request-mgt cc))))
	 (chunks-update changing-chunks false false)
	 (nconc reset-olds (mapcar #'list set-olds set-news)))))

;;; The "maybe" is because 'chunks' can be the empty list if the
;;; appropriate "postpone" argument was false in the call that 
;;; produced it.--
(defun file-ops-maybe-postpone (chunks)
   (setq postponed-file-chunks*
	 (nodup (append chunks postponed-file-chunks*))))

(defun postponed-files-update ()
   (chunks-update postponed-file-chunks* false false)
   (setq postponed-file-chunks* !()))

(defun keyword-if-sym (x)
   (cond ((is-Symbol x)
	  (intern (symbol-name x) keyword-package*))
	 (t x)))

