;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;; $Id: chunk.lisp,v 2.7 2006/12/01 17:46:16 airfoyle Exp $

;;; This file depends on nothing but the facilities introduced
;;; in base.lisp

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(Chunk Or-chunk Form-chunk Chunk-basis derive print-innards
	     find-chunk chunk-with-name chunk-destroy
	     chunk-request-mgt chunk-terminate-mgt
	     chunk-up-to-date chunk-declare-updated
	     chunk-update chunks-update dutl)))

;;;;; Some of the code in this file is referred to in the following
;;;;; paper:
#| <<<< :eval
    (referring-paper "McDermott05"
                      "~/UNIfied/word/pub/Lisp2005/lisp05.txl"
                      :default true)
   >>>> :eval
|#

;;; The date for latest supporter if no supporters --
(defconstant +no-supporters-date+ -1000)

;;; The date that means "no information"
(defconstant +no-info-date+ -1)

;;; A Chunk represents a piece of information, or a form of a piece of
;;; information, or perhaps merely a piece of information copied to a
;;; particular place.  We can't, and don't need to, describe the
;;; information in the Chunk.  We just have to ensure that anyone who
;;; needs it has a pointer to the Chunk so it can tell whether the
;;; information is up to date and if not recompute it.
;;;;; <<<< Chunk-defn 
(defclass Chunk ()
  ((name :accessor Chunk-name  ; -- An S-expression
	  :initarg :name
	  :initform "")
                    ;;;;; <<<< Mgt-stuff
   (manage-request :accessor Chunk-manage-request
		   :initform false
		   :type boolean)
   ;; -- true if user has declared that this chunk should be
   ;; managed.
   (managed :accessor Chunk-managed
	    :initform false)
    ;; -- A chunk is being kept up to date if and only if its 'managed'
    ;; field is non-false.
    ;; Global invariant: c is managed if and only if either its
    ;; manage-request is t or some derivee is managed.
                    ;;;;; >>>> Mgt-stuff
   (height :accessor Chunk-height
	   :type integer)
   ;; -- 0 if not derived from anything, else 1 + max height
   ;; of chunks in 'basis' (but different for Or-chunks; see below).
                    ;;;;; <<<< Update-stuff
   (depth :accessor Chunk-depth
	  :type integer
	  :initform 0)
   ;; -- used for debugging; depth changes every time we change
   ;; what counts as 0 depth.

   (date :accessor Chunk-date
	 :initform +no-info-date+
	 :type number)
   ;; -- date when last updated or -1 if never updated
   (latest-supporter-date
         :accessor Chunk-latest-supporter-date
	 :initform +no-supporters-date+
	 :type number)
   ;; -- date of latest element of basis, or basis of some element of basis,
   ;;  or .....  Value = -1000 if unknown
   ;; More precisely, this is the latest value any supporter has ever had.
   ;; If the basis of a chunk changes, that will not ever cause its
   ;; 'latest-supporter-date' to decrease.  In particular, a chunk can
   ;; go from being out of date to being up to date only by a call
   ;; to 'chunks-update'.
                    ;;;;; <<<< Bases-derivees
   (basis :accessor Chunk-basis
	  :initarg :basis
	  :initform !()
	  :type list)
   ;; -- list of Chunks this one is derived from
   (derivees :accessor Chunk-derivees
	     :initform !()
	     :type list)
   ;; -- back pointers to chunks this one is a member of the basis of
                    ;;;;; >>>> Bases-derivees
		    ;;;;; <<<< update-basis
   (update-basis :accessor Chunk-update-basis
		 :initarg :update-basis
		 :initform !()
		 :type list)
   ;; -- A list of Chunks governing the derivation of this one.
   ;; Think of it as keeping track of the "derivability" of this one.
   ;; Example: The chunk for a compiled file M might have in its
   ;; basis (:macros B) for a file containing macros it uses.  But if it
   ;; is necessary to recompile M, then the chunk (:loaded (:macros B))
   ;; must be up to date; i.e., the macros must actually be loaded.
   ;; So this chunk is in its update-basis.
   (update-derivees :accessor Chunk-update-derivees
		    :initform !()
		    :type list)
		    ;;;;; >>>> update-basis
   ;; -- back pointers to chunks this one is a member of the
   ;; update basis of
   (update-marks :accessor Chunk-update-marks
		 :initform  !())
   ;; -- Marked with various numbers 
   ;; to keep track of progress of chunk-update process.
                    ;;;;; >>>> Update-stuff
   ;; True during 'derive' 
   (derive-in-progress :accessor Chunk-derive-in-progress
		       :initform false
		       :type boolean)
   ))
;;;;; >>>> Chunk-defn
;;; We sort of assume that update-basis is "derivative" in the sense
;;; that if the chunk is up to date wrt the basis, there are no "surprises" 
;;; in the update-basis.  Every chunk in the update basis is a function
;;; of a subset of the basis, so if we updated it it wouldn't change the
;;; the output of 'derive'.

#| Used to pursue anomalous behavior --
(defmethod (setf Chunk-date) :before (new-date (ch Chunk))
   (cond ((and (> new-date 10000)
               (< new-date (Chunk-latest-supporter-date ch)))
          (break "Chunk ~s~% date ~s --> ~s~% latest supp date = ~s"
                 ch (Chunk-date ch) new-date
                 (Chunk-latest-supporter-date ch)))))

(defmethod (setf Chunk-latest-supporter-date) :before (new-date (ch Chunk))
   (cond ((and (> (Chunk-date ch) 0)
               (> new-date 10000)
               (> new-date (Chunk-date ch)))
          (break "Chunk ~s~% supp date ~s --> ~s~% date = ~s"
                 ch
                 (Chunk-latest-supporter-date ch) new-date
                 (Chunk-date ch)))))
|#

;;; These are used purely temporarily, during chunk construction.
;;; Finding one is an error.
(defclass Transient-chunk (Chunk) ())

;;; Key fact about an Or-chunk is that it supplies no reason for any of its
;;; disjuncts to be managed, except its default.
;;; This class is handled specially by 'chunk-manage', 'chunk-terminate-mgt',
;;; and 'chunks-update'. --
;;;;; <<<< Or-chunk
(defclass Or-chunk (Chunk)
   ((disjuncts :accessor Or-chunk-disjuncts
	       :initarg :disjuncts)
    (default :accessor Or-chunk-default
	     :initarg :default
	     :initform false)))
;;;;; >>>> Or-chunk

(defgeneric print-innards (x srm)
  (:method ((x t) srm)
     (declare (ignore srm))
     (values)))

(defmethod print-object ((c Chunk) srm)
   (print-unreadable-object (c srm)
      (format srm "Chunk~a~s"
	      (cond ((eq (Chunk-managed c) ':in-transition)
		     "~")
		    ((Chunk-managed c) "=")
		    (t "_"))
	      (chunk-name-abbrev-pathnames (Chunk-name c)))
      (print-innards c srm)
      (format srm "~a~a~a"
	      (cond ((and (slot-boundp c 'basis)
			  (chunk-is-leaf c))
		     "*")
		    (t ""))
	      (cond ((Chunk-derive-in-progress c)
		     "/")
		    (t ""))
	      (cond ((and (slot-boundp c 'date)
			  (slot-boundp c 'latest-supporter-date)
			  (chunk-date-up-to-date c))
		     "!")
		    (t "?")))))

(defun chunk-name-abbrev-pathnames (name)
   (cond ((typep name 'pathname)
	  (printed-snip name -20))
	 ((atom name) name)
	 (t
	  (mapcar #'chunk-name-abbrev-pathnames name))))

(defmethod print-innards :before ((orch Or-chunk) srm)
   (cond ((slot-boundp orch 'disjuncts)
	  (format srm "|~s|" (len (Or-chunk-disjuncts orch))))
	 (t (format srm "<no disjuncts>"))))

(defgeneric chunk-calculate-height (chunk)
   (:method ((ch Chunk))
      (cond ((chunk-is-leaf ch)
	     0)
	    (t
	     (+ (reduce #'max (Chunk-basis ch) :key #'Chunk-height)
		1)))))

(defmethod chunk-calculate-height :around ((or-ch Or-chunk))
   (let ((default (Or-chunk-default or-ch)))
      (cond (default
	     (max (Chunk-height default)
		  (call-next-method or-ch)))
	    (t
	     (call-next-method or-ch)))))

(defvar height-cycle*)

(defun chunk-propagate-height (ch)
   (labels ((propagate (ch sofar)
	       (cond ((memq ch sofar)
		      (setq height-cycle* sofar)
		      (error 
			 "Derivee cycle at ~s~% during height computation"
			 ch))
		     (t
		      (let* ((current-height (Chunk-height ch))
			     (new-height (chunk-calculate-height ch)))
			 (cond ((not (= new-height current-height))
				(setf (Chunk-height ch) new-height)
				(dolist (d (Chunk-derivees ch))
				   (propagate
				      d (cons ch sofar))))))))))
      (propagate ch !())))

;;; For non-leaf chunks, report the last time the chunk became derived or
;;; false if it is out of date.  (In the latter case 'chunk-date-record'
;;; will not be called.)
;;; This is not just the default behavior; it is the correct behavior
;;; for almost all non-leaf chunks, because whatever date was last recorded
;;; is the only date there is.
;;;;; <<<< derive-date
(defgeneric derive-date (chunk)
   ;; Default dater marks "no info" for a leaf;
   ;; for a non-leaf, it leaves the date alone --
   (:method ((c Chunk))
       (cond ((chunk-is-leaf c)
	      +no-info-date+)
	     ((chunk-up-to-date c)
	      (Chunk-date c))
	     (t
	      false))))
;;;;; >>>> derive-date

(defvar bad-ch*)

;;;;; <<<< derive
(defgeneric derive (chunk)
   (:method ((c Chunk))
      (format *error-output*
	 "No derive method found for ~s; assuming up to date~%"
	 c)
;;;;; <<<< _
      (setq bad-ch* c)
      (break "Underivable chunk")
;;;;; >>>> _
      false))
;;; Recomputes chunk
;;;   and returns time when it changed (usually current universal time)
;;;   or false if it's up to date.
;;;;; >>>> derive
;;;   If it returns t, that's equivalent to returning (get-universal-time).

(defmethod initialize-instance :after ((ch Chunk) &rest initargs)
   (declare (ignore initargs))
   (setf (Chunk-height ch)
         (cond ((null (Chunk-basis ch)) 0)
	       (t
		(+ (chunks-max-height (Chunk-basis ch))
		   1))))
   (dolist (b (Chunk-basis ch))
      (pushnew ch (Chunk-derivees b) :test #'eq))
   (dolist (b (Chunk-update-basis ch))
      (pushnew ch (Chunk-update-derivees b) :test #'eq))
   (chunk-propagate-height ch)
   (cond ((and (null (Chunk-basis ch))
	       (Chunk-managed ch))
	  (leaf-chunk-update ch)))
;;;;; <<<< _
;;; There is no point in doing this, because 'ch' has no derivees
;;; immediately after being created.   
;;;;   (set-latest-support-date ch)
;;;;; >>>> _
   )

(defmethod initialize-instance :after ((orch Or-chunk) &rest _)
   (cond ((slot-truly-filled orch 'disjuncts)
	  (or-chunk-set-default-and-update orch)
	  (dolist (b (Or-chunk-disjuncts orch))
	     (pushnew orch (Chunk-derivees b)))))
   ;; Note that 'chunk-propagate-height' is called twice for
   ;; Or-chunks, first by the initialize-instance[:after] method
   ;; for chunks in general, then by this method.  
   (chunk-propagate-height orch)
;;;;   (cond ((slot-truly-filled orch 'default)
;;;;	  (or-chunk-modify-height orch (Or-chunk-default orch))))
   orch)

(defmethod (setf Or-chunk-disjuncts) :before (disjuncts (orch Or-chunk))
   (cond ((and (slot-boundp orch 'disjuncts)
	       (not (eq disjuncts (Or-chunk-disjuncts orch))))
	  (derivee-cycle-check orch disjuncts)
	  (cond ((slot-boundp orch 'disjuncts)
		 (dolist (d (Or-chunk-disjuncts orch))
		    (setf (Chunk-derivees d)
			   (remove orch (Chunk-derivees d))))))
	  (setf (Chunk-update-basis orch) !())))
   orch)

;;; All of this is mainly to make the internals of Or-chunks
;;; look reasonably consistent.  The final decision about
;;; the default and update-basis are actually made in 'chunk-manage'.
(defmethod (setf Or-chunk-disjuncts) :after (disjuncts (orch Or-chunk))
   (dolist (d disjuncts)
      (setf (Chunk-derivees d)
	    (adjoin orch (Chunk-derivees d))))
   (or-chunk-set-default-and-update orch)
   (chunk-propagate-height orch)
   orch)

(defun or-chunk-set-default-and-update (orch)
	  (cond ((not (slot-truly-filled orch 'default))
		 (setf (Or-chunk-default orch)
		       (lastelt (Or-chunk-disjuncts orch)))))
	  (or-chunk-set-update-basis orch))

(defmethod (setf Or-chunk-default) :after (_ (orch Or-chunk))
   (chunk-propagate-height orch))

;;;;   (or-chunk-modify-height orch d)
;;;;
;;;;(defun or-chunk-modify-height (orch default)
;;;;   (let ((h-from-d (+ (Chunk-height default) 1)))
;;;;      (cond ((> h-from-d (Chunk-height orch))
;;;;	     (setf (Chunk-height orch) h-from-d)))))

(defun chunk-is-leaf (ch)
   (null (Chunk-basis ch)))

(defvar basis-inverters-dbg* false)

;;; Don't ever, ever modify the basis destructively.  Always setf the 
;;; whole basis ** to a fresh list ** (i.e., *not* the result of applying
;;; 'delete' to the list!), so that these demons run --

(defmethod (setf Chunk-basis) :before (new-basis ch)
                                     ;;;;(declare (ignore new-basis))
   (cond ((not (eq new-basis (Chunk-basis ch)))
	  (derivee-cycle-check ch new-basis)
	  (cond (basis-inverters-dbg*
		 (format t "Before setting basis of ~s to ~s~%"
			 ch new-basis)))
	  (dolist (b (Chunk-basis ch))
	     (setf (Chunk-derivees b)
		   (remove ch (Chunk-derivees b)))))))

(defmethod (setf Chunk-basis) :after (new-basis ch)
                                     ;;;;(declare (ignore new-basis))
   (cond (basis-inverters-dbg*
	  (format t "After setting basis of ~s to ~s~%"
		  ch new-basis)))
   (dolist (b (Chunk-basis ch))
      (setf (Chunk-derivees b)
	    (adjoin ch (Chunk-derivees b))))
   (cond ((Chunk-managed ch)
	  (dolist (b (Chunk-basis ch))
	     (chunk-manage b))))
   (chunk-propagate-height ch)
   (set-latest-support-date ch))

;;; Update the 'latest-supporter-date' of derivees* of ch.
;;; Return all the chunks are found to be out of date (which is
;;; not the same as the chunks whose 'latest-supporter-date' change).
;;; This returns a fresh list you can nconc things onto...
(defun set-latest-support-date (ch)
   (let ((checked !())
	 (out-of-date !()))
      (labels ((walk-thru-derivees (ch)
		  (on-list ch checked)
		  (let ((latest-supporter-date +no-supporters-date+))
		     (dolist (b (Chunk-basis ch))
			(let ((late (max (Chunk-date b)
					 (Chunk-latest-supporter-date b))))
			   (cond ((> late latest-supporter-date)
				  (setf latest-supporter-date late)))))
		     (cond ((> latest-supporter-date
			       (Chunk-latest-supporter-date ch))
			    (setf (Chunk-latest-supporter-date ch)
				  latest-supporter-date)))
		     (let ((ch-date (Chunk-date ch)))
			(dolist (d (Chunk-derivees ch))
			   (cond ((not (memq d checked))
				  (let ((d-latest (Chunk-latest-supporter-date
                                                     d))
					(d-date (Chunk-date d)))
				     (cond ((or (> ch-date d-latest)
						(> latest-supporter-date
                                                   d-latest)
						(= d-date +no-info-date+)
						(> ch-date d-date)
						(> latest-supporter-date
                                                   d-date))
					    (walk-thru-derivees d)))))))
			(cond ((> latest-supporter-date
				  ch-date)
			       (on-list ch out-of-date)))))))
	 (walk-thru-derivees ch)
	 out-of-date)))


(defmethod (setf Chunk-update-basis) :before (new-update-basis ch)
   (cond ((not (eq new-update-basis (Chunk-update-basis ch)))
	  (dolist (b (Chunk-update-basis ch))
	     (setf (Chunk-update-derivees b)
		   (remove ch (Chunk-update-derivees b)))))))

(defmethod (setf Chunk-update-basis) :after (new-update-basis ch)
                                     (declare (ignore new-update-basis))
   (dolist (b (Chunk-update-basis ch))
      (setf (Chunk-update-derivees b)
	    (adjoin ch (Chunk-update-derivees b)))))

;;; The number of the next chunk event (management request or update) .
(defvar chunk-event-num* 1)

(defvar chunk-update-dbg* false)

;;; A list recording events that are finished.  Format: First element
;;; is smallest number such that all numbers less are for finished events.
;;; Remaining elements, if any, are finished events with numbers greater
;;; than that, in ascending order.
;;; Corollary: (rest active-chunk-events*) is either empty or starts with a
;;; number > (+ (first active-chunk-events*) 1).  
(defvar active-chunk-events* (list 1))

(defun chunk-event-discard (evnum)
;;;;   (format *error-output*
;;;;      "Before discarding ~s, active-chunk-events* = ~s~%"
;;;;     evnum active-chunk-events*)
;;;;   (cond ((= evnum 5)
;;;;	  (break "Discarding event 5")))
   (cond ((= evnum chunk-event-num*)
	  ;; This can happen only if (e.g.) 'chunk-request-mgt' is
	  ;; interrupted just before 'chunk-event-num*' is
	  ;; incremented; so let's increment it.
	  (setq chunk-event-num* (+ chunk-event-num* 1))
	  (cond (chunk-update-dbg*
		 (format *error-output*
		    "Incremented chunk-event-num* to ~s in chunk-event-discard~%"
		    chunk-event-num*)))))
   (cond ((< evnum (first active-chunk-events*))
	  (error "Discarding ~s when chunk-event-num* = ~s"
		 evnum chunk-event-num*))
	 ((= evnum (first active-chunk-events*))
	  ;; Opportunity to condense the list of done events
	  (let ((evnum evnum))
	     (loop
		;; In this loop, 'evnum' = event number for smallest
		;; event known to be >= all discarded events except
		;; those on 'active-chunk-events*'.
		(setq active-chunk-events* (rest active-chunk-events*))
		;; At this point, 'active-chunk-events*' is the list of numbers
		;; of all discarded events > 'evnum'.
	        ;; 'evnum' is the number that occurred just before
	        ;; those numbers.
	        (setq evnum (+ evnum 1))
	        ;; Now 'evnum' is > all discarded events except those
	        ;; on 'active-chunk-events*'.
		(cond ((and (not (null active-chunk-events*))
			    (= (first active-chunk-events*)
			       evnum))
		       (setq evnum (first active-chunk-events*)))
		      (t
		       (on-list evnum active-chunk-events*)
		       ;; At this point the global invariant (above the 'defvar'
		       ;; for active-chunk-events*) is restored.
		       (return))))))
	 (t
	  ;; If not discarding the first, just put it in the proper
	  ;; order on (rest active-chunk-events*)
	  (let ((done active-chunk-events*))
	     (loop
		(cond ((null (tail done))
		       (on-list evnum (tail done)))
		      ((= (first (tail done))
			  evnum)
		       (return))
		      ((> (first (tail done))
			  evnum)
		       (on-list evnum (tail done))
		       (return))
		      (t
		       (setq done (tail done))))))))
;;;;  (format *error-output*
;;;;     "After discarding ~s, active-chunk-events* = ~s~%"
;;;;     evnum active-chunk-events*)
  )

(defun chunk-mark (ch evnum)
   (on-list evnum (Chunk-update-marks ch)))

;;; This is *not* the right way to undo 'chunk-mark', except
;;; in the kludgy case in 'derivee-cycle-check'.
(defun chunk-unmark (ch evnum)
   (setf (Chunk-update-marks ch)
	 (remove evnum (Chunk-update-marks ch) :test #'= :count 1)))

(defun chunk-is-marked (ch evnum)
   (do ((marks (Chunk-update-marks ch) (rest marks))
	(splice false)
	(prev false (cond (splice prev) (t marks))))
       ((or (null marks)
	    (= (first marks) evnum))
	(not (null marks)))
;;;;      (out "marks = " marks
;;;;	   :% "  chunk marks = " (Chunk-update-marks ch)
;;;;	   :% "  prev = " prev :%)
      (setq splice (< (first marks) (first active-chunk-events*)))
      (cond (splice
	     (cond (prev
		    (setf (rest prev) (rest marks)))
		   (t
		    (setf (Chunk-update-marks ch)
			  (rest marks))))))))

(defun derivee-cycle-check (ch potential-supporters)
   (let ((evnum chunk-event-num*))
      (unwind-protect
	 (progn
	    ;; Kludge alert -- This is one place where allocating
	    ;; a new chunk event _cannot_ cause any change to the
	    ;; chunk network, and hence should not cause chunks-udpate
	    ;; to restart.  (Such a restart is harmless, but wasteful.)
	    ;; Hence we "deallocate" it by not allocating it in the
	    ;; first place, that is, by not incrementing chunk-event-num*.
	    ;; The "orthodox" way of doing things is commented out,
	    ;; but has a double-asterisk (**) flagging the code.
;;;;**	    (setq chunk-event-num* (+ evnum 1))
;;;;**	    (cond (chunk-update-dbg*
;;;;**		   (format *error-output*
;;;;**		      !"Incrementing chunk-event-num* to ~s in ~
;;;;**                        derivee-cycle-check~%"
;;;;**		      chunk-event-num*)
;;;;		   (setq bad-ch* ch)
;;;;		   (break "Cycle check for ~s" ch)
;;;;**		   ))
	    (labels ((pursue (trail)
		        (let ((derivee (first trail)))
			   (cond ((memq derivee potential-supporters)
				  (error
				     !"Cycle in derivee links ~%  ~s"
				     (reverse trail)))
				 ((not (chunk-is-marked derivee evnum))
				  (chunk-mark derivee evnum)
				  (dolist (d (Chunk-derivees derivee))
				     (pursue (cons d trail))))))))
	       (pursue (list ch))))
	 (labels ((pursue-erasing (derivee)
		     (cond ((chunk-is-marked derivee evnum)
			    (chunk-unmark derivee evnum)
			    (dolist (d (Chunk-derivees derivee))
			       (pursue-erasing d))))))
	    (pursue-erasing ch))
;;;;**	 (chunk-event-discard evnum)
      )))

;;;;; <<<< chunk-requesters
;;; Returns management state of 'c' (normally true after this runs).
(defun chunk-request-mgt (c)
   (cond ((or (not (Chunk-manage-request c))
	      (not (Chunk-managed c)))
	  (let ((evnum chunk-event-num*))
	     (unwind-protect
		(progn
		   (setq chunk-event-num* (+ evnum 1))
		   (cond (chunk-update-dbg*
			  (format *error-output*
			     !"Incrementing chunk-event-num* to ~s ~
			       in chunk-request-mgt~%"
			     chunk-event-num*)))
		   (chunk-internal-req-mgt c))
	        (chunk-event-discard evnum))))
	 (t true)))

(defun chunk-internal-req-mgt (c)
;;;;   (cond ((eq c (g c1))
;;;;	  (dbg-save c)
;;;;	  (breakpoint chunk-request-mgt
;;;;	     "About to re-manage " c)))
	  (setf (Chunk-manage-request c) true)
	  (chunk-manage c))

;;; This terminates the explicit request for management.
;;; If 'propagate' is false, then the chunk will remain managed unless
;;; none of its derivees are managed.
;;; If 'propagate' is :ask, then the user will be asked if its
;;; derivees should become unmanaged.
;;; Any other value will cause any derivees to become unmanaged.
;;; Returns management state of 'c'.
(defun chunk-terminate-mgt (c propagate)
   (let ((evnum chunk-event-num*))
      (unwind-protect
	 (progn
	    (setq chunk-event-num* (+ evnum 1))
	    (cond (chunk-update-dbg*
		   (format *error-output*
		      !"Incrementing chunk-event-num* to ~s ~
			in chunk-request-mgt~%"
		      chunk-event-num*)))
	    (chunk-internal-term-mgt c propagate))
	 (chunk-event-discard evnum))))

(defun chunk-internal-term-mgt (c propagate)
   (setf (Chunk-manage-request c) false)
   (cond ((Chunk-managed c)  ;;;;(Chunk-manage-request c)
	  (labels (;; Returns true if should unmanage derivees --
		   (propagate-to-derivees (all-derivees)
			 (or (null all-derivees)
			     (and propagate
				  (or (not (eq propagate ':ask))
				      (yes-or-no-p
					 !"Should I really stop managing ~
					   (keeping up to date) all of ~
					   the following chunks?~
					   --~%  ~s (yes/no)? "
					 all-derivees)))))
		   (chunk-gather-derivees (ch dvl)
		      (dolist (d (Chunk-derivees ch))
			 (cond ((and (Chunk-managed d)
				     (not (member d dvl))
				     (or (member ch (Chunk-basis d))
					 (cond ((typep d 'Or-chunk)
						(or-dflt-reason-to-manage
						   d ch))
					       (t
						(error "Missing back ptr")))))
				(setq dvl
				      (chunk-gather-derivees
					  d (cons d dvl))))))
		      dvl))
	     (let ((all-derivees
		      (chunk-gather-derivees c !())))
;;;;		(format t "All-derivees = ~s~%" all-derivees)
		(cond ((propagate-to-derivees all-derivees)
		       (cond ((null all-derivees)
			      (chunk-unmanage c))
			     (t
			      (dolist (d all-derivees)
				 (setf (Chunk-manage-request d)
				       false))
			      (dolist (d all-derivees)
				 (cond ((chunk-has-no-managed-derivees d)
					(chunk-unmanage d))))
                              (chunk-unmanage c)
			      (cond ((Chunk-managed c)
				     (cerror !"I'll proceed with chunk in ~
                                               unexpected state"
					     !"Chunk ~s still managed after ~
                                               request to terminate ~
                                               management"
					     c)
				     true)
				    (t false)))))
		      (t true)))))
	 (t false)))

(defun chunk-has-no-managed-derivees (ch)
   (let ((found-one false))
      (dolist (d (Chunk-derivees ch))
	 (cond ((Chunk-managed d)
		(cond ((typep d 'Or-chunk)
		       (cond ((or (member ch (Chunk-basis d))
				  (or-dflt-reason-to-manage d ch))
			      (setq found-one true)
			      (return))))
		      (t
		       (setq found-one true)
		       (return))))))
      (not found-one)))
;;;;; >>>> chunk-requesters

;;; This doesn't call chunk-update, but presumably everyone who calls
;;; this will call chunk-update immediately thereafter.
;;;;; <<<< chunk-managers
;;; Returns management state of 'chunk' --
(defun chunk-manage (chunk)
   (cond ((Chunk-managed chunk)
	  ;;;;; <<<< _ 
	  (cond ((eq (Chunk-managed chunk)
		     ':in-transition)
		 (error
		    !"Chunk basis cycle detected at ~S" chunk 
                     "~%     [-- chunk-manage]")))
	  ;;;;; >>>> _
	  true)
	 (t
	  (let ((its-an-or (typep chunk 'Or-chunk)))
	     (unwind-protect
		(progn
		   ;;;;; <<<< temporarily-in-transition
		   (setf (Chunk-managed chunk) ':in-transition)
		   ;;;;; >>>> temporarily-in-transition
		   (dolist (b (Chunk-basis chunk))
		      (chunk-manage b))
		   (cond (its-an-or
			  (or-chunk-set-update-basis chunk)
			  (chunk-manage (first (Chunk-update-basis chunk)))))
		   ;; If 'chunk' is a non-default disjunct
		   ;; of an Or-chunk, then the default disjunct no
		   ;; longer gets a reason to be managed from the Or.
		   (dolist (d (Chunk-derivees chunk))
		      (cond ((and (Chunk-managed d)
				  (typep d 'Or-chunk))
			     (let ((d-disjuncts (Or-chunk-disjuncts d))
				   (d-default (Or-chunk-default d)))
				(cond ((and (not (eq chunk d-default))
					    (memq chunk d-disjuncts)
					    (not (reason-to-manage d-default)))
				       (setf (Chunk-update-basis d)
					     (list chunk))
				       (chunk-unmanage d-default))))))))
	        ;; Normally this just sets (chunk-managed chunk)
	        ;; to true, but not if an interrupt occurred --
		(progn
		    (setf (Chunk-managed chunk) (reason-to-manage chunk))
		 ))
	     (Chunk-managed chunk)))))

;;; Returns management state of 'c' -- 
(defun chunk-unmanage (c)
   ;; This is called only by 'chunk-terminate-mgt' and 'chunk-manage',
   ;; and only after verifying that 'c' has no managed derivees --
   (cond ((Chunk-managed c)
	  ;;;;; <<<< cycle-check
	  (cond ((eq (Chunk-managed c)
		     ':in-transition)
		 (error
		    "Chunk basis cycle detected at ~S" c
		    "~%     [-- chunk-unmanage]")))
	  ;;;;; >>>> cycle-check
	  (unwind-protect
	     (progn
		(setf (Chunk-managed c)
		      (reason-to-manage c))
		(cond ((Chunk-managed c)
		       (cerror "I'll proceed with chunk in unexpected state"
			       !"Chunk ~s has unexpected reason to be ~
                                 managed"
			       c)
		       true)
		      (t
		       ;; If c is the last disjunct that supplied
		       ;; an independent way of deriving a managed
		       ;; Or-chunk d, then the default disjunct of
		       ;; d must be managed.
		       (dolist (d (Chunk-derivees c))
			  (cond ((and (Chunk-managed d)
				      (typep d 'Or-chunk))
				 (let ((d-default (Or-chunk-default d)))
				    (cond ((every (\\ (j)
						     (or (eq j d-default)
							 (not (Chunk-managed
							         j))))
						  (Or-chunk-disjuncts d))
					   (setf (Chunk-update-basis d)
						 (list d-default))
					   (chunk-manage d-default)))))))
		       (cond ((typep c 'Or-chunk)
			      ;; If 'c' is an Or-chunk,
			      ;; check if now there is no reason to
			      ;; keep managing its default
			      (setf (Chunk-update-basis c) !())
			      (let ((c-default (Or-chunk-default c)))
				 (cond ((not (reason-to-manage c-default))
					(chunk-unmanage c-default))))))
		       (dolist (b (Chunk-basis c))
			  (cond ((and (Chunk-managed b)
				      (not (reason-to-manage b)))
				 ;; No further reason to manage b
				 (chunk-unmanage b)))))))
	       ;; This is normally redundant, but we need it in the
	       ;; call to 'unwind-protect' to ensure the invariant
	       ;; that if a chunk is managed then its basis is --
	       (setf (Chunk-managed c)
		     (reason-to-manage c))))
	 (t false)))
;;;; >>>> chunk-managers

;;; Returns "the" chunk that gives a reason for 'ch' to be managed, either
;;; 'ch' itself if there's a request to manage it, or an appropriate
;;; derivee.  Returns false if there's no reason for it to be managed. --
;;;;; <<<< reason-to-manage
(defun reason-to-manage (ch)
   (cond ((Chunk-manage-request ch) ch)
	 (t
	  (dolist (d (Chunk-derivees ch) false)
	     (cond ((Chunk-managed d)
		    (cond ((typep d 'Or-chunk)
			   (cond ((memq ch (Chunk-basis d))
				  (return-from reason-to-manage d))
				 ((or-dflt-reason-to-manage d ch)
				  (return-from reason-to-manage d))))
			  (t
			   (return-from reason-to-manage d)))))))))

;;; 'orch' is an Or-chunk, and is a derivee of 'ch'.  Does 'orch'
;;; supply a reason to manage 'ch' by virtue of 'ch' being the default
;;; for 'orch'?
(defun or-dflt-reason-to-manage (orch ch)
   (and (eq (Or-chunk-default orch)
	    ch)
	(every (\\ (j)
		  (or (eq j ch)
		      (not (Chunk-managed j))))
	       (Or-chunk-disjuncts orch))))
;;;;; >>>> reason-to-manage

;;;;(defvar update-no* 0)

(defvar temp-mgt-dbg* false)
(defvar bad-bases*)

;;; Debugging instrumentation in 'chunks-update-now' (see below);
;;; normally commented out.
(defmacro report-reason-to-skip-chunk ()
  '(format t " ...Skipping because ~s~%"
	   (cond ((not (Chunk-managed ch))
		  "not managed")
	         ((Chunk-derive-in-progress ch)
		  "derivation already in progress")
	         ((not (chunk-is-marked ch down-mark))
		  "never marked with down-mark")
		 ((chunk-is-marked ch up-mark)
		  "already marked with up-mark")
		 ((and (chunk-date-up-to-date ch)
		       (or (not force)
			   (not (memq ch must-derive))))
;;;;		  (cond ((eq ch (elt input-chunks* 3))
;;;;			 (setq ch* ch)
;;;;			 (break "Got input 3")))
		  "up to date and not forced")
		 ((not
		      (every #'chunk-up-to-date
			     (Chunk-basis ch)))
		  (setq bad-bases*
		     (remove-if
			 #'chunk-up-to-date
			 (Chunk-basis ch)))
		  (setq bad-ch* ch)
		  (format nil
		     "Out-of-date bases ~%~s"
		     bad-bases*)
;;;;		  (break "bases not up to date: ~s for chunk ~s"
;;;;			 bad-bases* bad-ch*)
		  )
		 ((not
		     (every #'chunk-up-to-date
			    (Chunk-update-basis ch)))
		  (setq bad-bases*
		     (remove-if
			 #'chunk-up-to-date
			 (Chunk-update-basis ch)))
		  (format nil "Out-of-date update bases~%~s"
		              bad-bases*))
		 (t "of a mystery"))))

;;;;(defvar postpone-updates* ':do-now)

(defun chunk-declare-updated (chunk time)
   (cond ((and (is-Number time)
	       (not (> time 0)))
	  (error "Illegal time '~s' in update declaration~%  for chunk ~s"
		 time chunk)))
   (setf (Chunk-date chunk)
	 (or time (get-universal-time)))
   (cond ((chunk-up-to-date chunk)
	  true)
	 (t
	  (cerror "I'll pretend it is"
		  !"Chunk date set to time ~s that is before latest supporter date ~s~
                    ~% for chunk ~s"
		  (Chunk-date chunk)
		  (Chunk-latest-supporter-date chunk)
		  chunk))))

(defun chunk-update (ch force postpone-derivees)
    (chunks-update (list ch) force postpone-derivees))
	 
(defvar chunk-update-depth* 0)
(defvar chunk-depth-error-thresh* 10)

;;; The number of calls to chunks-update so far, sorted by
;;; depth --
(defvar chunk-update-count* !())
(defvar chunk-restart-count* !())

;;; If you are several levels down in chunk-update's, restarting
;;; an inner one will eventually require you to restart the outermost
;;; one, which will then restart the inner ones, etc., costing an
;;; exponential amount of time (k^d, where d is the depth).
;;; If this flag is true, we always just restart at the top.
(defvar long-distance-chunk-restart* false)
;;; -- which turns out not to work, so leave this false!

(defvar restart-if-detect-derive-in-progress* false)

;;; Don't even try checking derivees in recursive calls
;;; to 'chunks-update', because experience shows they will
;;; produce meta-cycles and cause a lot of trouble --
(defvar postpone-derivees-in-inner-updates* true)

(defvar update-fail-dbg* 1)
;;; 0 means Ignore; 1 means Print warning; 2 means break

(defvar last-meta-cycle* nil)

;;; 'force' is true if the 'chunks' must be derived, even if
;;; up to date.
;;; 'postpone-derivees' is true if the only chunks to be updated are the
;;; supportive descendents of 'chunks'.
;;; Returns list of postponed derivees, 
;;; which will be () if postpone-derivees=false.
(defun chunks-update (orig-chunks force postpone-derivees)
   (incf (alref chunk-update-count* chunk-update-depth* 0 :test #'=))
;;;;   (dbg-save (cl1 chunks))
;;;;   (breakpoint "boing")
   (cond ((and postpone-derivees-in-inner-updates*
	       (> chunk-update-depth* 0))
	  (setq postpone-derivees true)))
   (let* (derive-mark down-mark up-mark
	  max-here temporarily-managed
	  (chunks orig-chunks)
	  (must-derive (cond (force (list-copy chunks))
			     (t !())))
	  (postponed !())
	  (chunk-update-depth* (+ chunk-update-depth* 1))
	  ;; This is a list of chunks found along the way that
	  ;; need to be considered for update.  They are unimportant
	  ;; unless we need to restart, in which case they get added
	  ;; to the 'chunks' list; i.e., they become seeds from which
	  ;; the update propagates --
	  (found-chunks !())
	  (found-meta-cycle false)
	  update-interrupted iter)
      (labels ((down-then-up (chunks)
		  (let ((chunks-needing-update
			    (chunks-leaves-up-to-date chunks !())))
		     (cond ((memq ':meta-cycle chunks-needing-update)
			    (do ((cl chunks (cdr cl))
				 (ul chunks-needing-update (cdr ul))
				 (non-cyclical !())
				 (bad !()))
				((null ul)
				 (cond ((not (null bad))
					(error !"Found chunk meta-cycle(s) ~
                                                 for ~%  ~s"
					       bad)))
				 (setq chunks-needing-update
				       non-cyclical))
			       (cond ((eq (car ul) ':meta-cycle)
				      (cond ((memq (car cl) orig-chunks)
					     (on-list-if-new (car cl)
							     bad))))
				     (t
				      (on-list (car ul) non-cyclical))))))
		     ;; No fatal :meta-cycles, so we can combine all the
		     ;; lists.--
		     (setq chunks-needing-update
			   (apply #'nconc chunks-needing-update))
		     (cond (chunk-update-dbg*
			    (format *error-output*
			       !"Updating derivees of chunks ~s~
                                 ~%  found-chunks = ~s~%"
			       chunks-needing-update found-chunks)))
		     (dolist (ch chunks-needing-update)
			(derivees-update ch !()))))

	       ;; DOWN Phase
	       ;; Returns (Lst (Alt (Lst Chunk) (Const :meta-cycle))) 
	       ;; i.e., a list of lists of chunks, with the occasional
	       ;; :meta-cycle thrown in, which is deadly if encountered
	       ;; below the original chunks, but harmless elsewhere.
	       (chunks-leaves-up-to-date (chunkl in-progress)
                  (cond ((and chunk-update-dbg*
                              (not (null in-progress)))
                         (format *error-output*
                            !"Looking at supporters of ~s~%"
                            (car in-progress))
;;;;                         (cond ((eq (car in-progress) c-s)
;;;;                                (break "Got it")))
                         ))
                  (prog1
		     (mapcar (\\ (ch)
                                (check-leaves-up-to-date ch in-progress))
                             chunkl)
                     (cond ((and chunk-update-dbg*
                                 (not (null in-progress)))
                            (format *error-output*
                               !"Done with supporters of ~s~%"
                               (car in-progress))))))

	       ;; Return all derivees that need to be updated (plus 
	       ;; supporters* of those derivees).
	       ;; If some supporter of 'ch' has a derivation already
	       ;; in progress, then return :meta-cycle.
	       (check-leaves-up-to-date (ch in-progress)
		  ;; Also marks all derivees* of those leaves with proper
		  ;; latest-supporter-date.
		  (cond ((or (chunk-is-marked ch down-mark)
			     ;; This can happen if deriver of some
			     ;; chunk terminated the management of
			     ;; this one (due to an error being
			     ;; detected) --
			     (not (Chunk-managed ch)))
			 !())
			((member ch in-progress)
;;;;			 (setq ch* ch)
;;;;			 (setq inp* in-progress)
			 (error
			     !"Path to leaf chunks from ~s apparently goes ~
                               through itself"
			     ch))
			(t
			 (setf (Chunk-depth ch)
			       (cond ((null in-progress) 0)
				     (t
				      (+ (Chunk-depth (first in-progress))
					 1))))
			 (chunk-mark ch down-mark)
			 (cond ((found-meta-cycle-at ch in-progress)
				(cond (chunk-update-dbg*
				       (format *error-output*
					   !"Encountered chunk with derive ~
                                             in progress: ~s~%"
					   ch)))
				;; Oops -- "meta-circularity"
				(cond (restart-if-detect-derive-in-progress*
				       ;; Plan A: Start again in postpone mode
				       (start-again-in-postpone-mode ch))
				      (t
				       ;; Plan B: Ignore this whole subtree
				       ':meta-cycle)))
			       (t
				(check-date-and-descend ch in-progress))))))

	       (check-date-and-descend (ch in-progress)
		  (cond ((not (chunk-is-marked ch derive-mark))
			 (chunk-derive-date-and-record ch)
			 (cond (chunk-update-dbg*
				(chunk-date-note ch "Dated chunk ")))
			 (check-interrupt)
			 (cond ((and (chunk-is-leaf ch)
				     (= (Chunk-date ch)
					+no-info-date+))
				;; See note on 'do-derive', below
				(cond (chunk-update-dbg*
				       (chunk-date-note
					  ch " Leaf chunk; deriving!")))
				(do-derive ch)))))
		  (let* ((in-progress (cons ch in-progress))
			 (to-be-derived
			      (check-from-derivees ch in-progress)))
		     (cond ((chunk-is-leaf ch)
			    to-be-derived)
			   (t
			    (reachables-combine
			       to-be-derived
			       (chunks-leaves-up-to-date
					     (Chunk-basis ch)
					     in-progress)
			       ;; The call to 'chunk-up-to-date'
			       ;; works because the leaves supporting
			       ;; ch have passed their dates up
			       ;; via 'set-latest-support-date' --
			       (cond ((still-must-derive ch)
				      (cond ((and temp-mgt-dbg*
						  (some (lambda (ch)
						           (not (Chunk-managed
                                                                   ch)))
							(Chunk-update-basis
                                                           ch)))
					     (format *error-output*
						!"To handle update ~
                                                  basis of ~s ... ~%"
						ch)))
				      (temporarily-manage
					 (Chunk-update-basis ch))
				      (chunks-leaves-up-to-date
					 (Chunk-update-basis ch)
					 in-progress))
				     (t
				      !())))))))

               (check-from-derivees (ch in-progress)
		  (let ((updatees
			   (retain-if-x
			      (\\ (c)
				 (and (not (chunk-up-to-date c))
				      (not (memq c in-progress))
				      (not (found-meta-cycle-at c in-progress))
				      (or (eq c ch)
					  (and (Chunk-managed c)
					       (not (chunk-is-marked
						       c down-mark))))))
			      (set-latest-support-date ch))))
		     (cons ch
			   (cond ((null updatees)
				  !())
				  ;; Sweep up from leaves may have
				  ;; found new chunks that need to be
				  ;; checked.
				 (postpone-derivees
				  ;; But if postponing, these are precisely the
				  ;; chunks we want to save for later.
				  (cond (chunk-update-dbg*
					 (format *error-output*
					    "Postponing chunks ~s~%"
					    updatees)))
				  (setq postponed
					(nconc updatees postponed))
				  !())
				 (t
				  (cond (chunk-update-dbg*
					 (format *error-output*
						 "New starting points = ~s~%"
						 updatees)))
				  (setq found-chunks
					(append updatees found-chunks))
				  ;; Otherwise, start from those
				  ;; chunks as if new-- 
				  (mapcan
				     (\\ (upd-supporters)
					(cond ((eq upd-supporters ':meta-cycle)
					       ;; A cycle detected "off to the
					       ;; side" just means we shouldn't
					       ;; have tried that updatee
					       !())
					      (t
					       upd-supporters)))
				     (chunks-leaves-up-to-date
					updatees !())))))))

	       ;; Three groups of chunks reachable from a single chunk
	       ;; (which happens to be the first element of 'from-derivees').
	       ;; The second and third are as returned by
	       ;; 'chunks-leaves-up-to-date'.
	       (reachables-combine (from-derivees from-basis from-upd-basis)
		  (cond ((or (memq ':meta-cycle from-basis)
			     (memq ':meta-cycle from-upd-basis))
			 ':meta-cycle)
			(t
			 (nconc from-derivees
				(apply #'nconc from-basis)
				(apply #'nconc from-upd-basis)))))

	       (temporarily-manage (update-chunks)
;;;;		  (trace-around temporarily-manage
;;;;		     (:> "(temporarily-manage: " update-chunks ")")
		  (dolist (ud-ch update-chunks)
		     (cond ((not (Chunk-manage-request ud-ch))
                            ;; -- This used to be (not (Chunk-managed ud-ch)).
                            ;; But, because of nonmonotonicity, a chunk
                            ;; can become unmanaged when another chunk
                            ;; gets temporarily managed.  So we have to
                            ;; put in the request even if it seems redundant.
			    (cond (temp-mgt-dbg*
				   (format t "Temporarily managing ~s~%"
					   ud-ch)
;;;;                                   (cond ((and c-s-break*
;;;;                                               (eq update-chunks
;;;;                                                   (Chunk-update-basis
;;;;                                                      c-s)))
;;;;                                          (setq ud-ch* ud-ch
;;;;                                                ud-ch-l* update-chunks)
;;;;                                          (break "got update-chunks ~s"
;;;;                                                 ud-ch-l*)))
                                   ))
			    (on-list ud-ch temporarily-managed)
			    (chunk-internal-req-mgt ud-ch))))
;;;;		     (:< (&rest _) "temporarily-manage: "))
		  )

	       ;; The term "meta-cycle" refers to the fact that in the
	       ;; process of deriving 'ch' we triggered an update that
	       ;; is now considering deriving 'ch'.
	       (found-meta-cycle-at (ch in-progress)
		  (let ((found-it (Chunk-derive-in-progress ch)))
		     (cond (found-it
			    (setq found-meta-cycle true)
			    (setq last-meta-cycle* (cons ch in-progress))
			    (cond (chunk-update-dbg*
				   (format *error-output*
				      !"Encountered chunk with derive in ~
					progress: ~s~%"
				      ch)))))
		     found-it))

	       (start-again-in-postpone-mode (stumble-ch)
		  ;; In updating 'stumble-ch', 'chunks-update' got
		  ;; called and is now trying to update 'stumble-ch'
		  ;; again.  We can still survive if 'stumble-ch' is
		  ;; postponable
		  (cond (postpone-derivees
			 ;; Guess not
			 (cerror "I'll just skip it, but ..."
				 !"Apparent need to update ~s~
				   ~% during its own derivation"
				 stumble-ch)
			 !())
			(t
			 (setq postpone-derivees true)
			 (setq chunks orig-chunks)
			 (setq found-chunks !())
			 (cond (chunk-update-dbg*
				(format *error-output*
				   !"Apparent need to update ~s~
				     ~% during its own ~
				     derivation; switching ~
				     to postpone mode~%"
				   stumble-ch)))
			 (throw 'update-interrupted true))))

	       ;; UP Phase
	       (derivees-update (ch in-progress)
;;;;		  (trace-around derivees-update
;;;;		     (:> "(derivees-update: " ch ")")
		  (cond (chunk-update-dbg*
			 (format *error-output*
			    "Considering ~s~%  [after ~s] [depth ~s]~%"
				 ch in-progress (Chunk-depth ch))))
		  (cond ((member ch in-progress)
			 (error
			    !"Cycle in derivation links from ~s"
			    ch))
			((and (Chunk-managed ch)
			      (not (Chunk-derive-in-progress ch))
			      (chunk-is-marked ch down-mark)
			      (not (chunk-is-marked ch up-mark))
			      (still-must-derive ch)
			      ;; Run the deriver when and only when
			      ;; its basis is up to date --
			      (every #'chunk-up-to-date
				     (Chunk-basis ch))
			      ;; Ditto for update-basis --
			      (every (\\ (ub)
					(or (not (Chunk-managed ub))
					    (chunk-up-to-date ub)))
				     (Chunk-update-basis ch)))
			 (dolist (ub (Chunk-update-basis ch))
			    (cond ((not (Chunk-managed ub))
				   (error !"Managed chunk ~s~%  has ~
                                            unmanaged update-basis element ~s"
					  ch ub))))
			 (cond ((and postpone-derivees
				     (not (chunk-is-marked ch down-mark)))
				;; We're about to go too far; put it on the
				;; postponed list.
				(cond (chunk-update-dbg*
				       (format *error-output*
					  "Postponing ~s~%"
					  ch)))
				(on-list ch postponed))
			       (t
                                (cond (chunk-update-dbg*
                                       (format *error-output*
                                               " ...Deriving!")
                                       (cond ((chunk-is-marked ch derive-mark)
                                              (format *error-output*
                                                 !" -- even though already ~
                                                   marked with ~
                                                   derive-mark~%"))
                                             (t
                                              (format *error-output* "~%")))))
                                ;; See note on 'do-derive', below
                                (do-derive ch)
				(let ((to-explore
				         (append (Chunk-update-derivees ch)
						 (Chunk-derivees ch)))
				      ;; -- [don't nconc! shares structure
				      ;; with derivees list on ch]
				      (in-progress
					  (cons ch in-progress)))
				   (chunk-mark ch up-mark)
				   (let ((updatees 
					    (nodup
					       (mapcan
						  #'set-latest-support-date
						  to-explore))))
				      (cond (postpone-derivees
					     (setq postponed
						   (append updatees
							   postponed)))
					    (t
					     (setq found-chunks
						   (append updatees
							   found-chunks))))
				      (setq to-explore
					    (nconc updatees to-explore)))
				   (dolist (upd to-explore)
				      (derivees-update upd in-progress))))))
			(chunk-update-dbg*
			 (report-reason-to-skip-chunk)))
;;;;		     (:< (&rest _) "derivees-update: "))
		  )

	       (still-must-derive (ch)
		  (and (not (Chunk-derive-in-progress ch))
		       (or (not (chunk-date-up-to-date ch))
			   (and force (memq ch must-derive)))))

	       ;; This is called in two places above.  In each case,
	       ;; we will pursue the derivees of 'ch' after deriving it.
	       ;; The deriver for 'ch' is allowed to change its derivees!
	       ;; So in both places where 'do-derive' is called, the
	       ;; derivees are obtained only after it returns.
	       (do-derive (ch)
		 (chunk-mark ch derive-mark)
		 (chunk-derive-and-record ch)
		 (cond ((not (chunk-up-to-date ch))
			(error "Chunk not up to date after deriving:~%  ~s"
			       ch))
		       ((< (Chunk-date ch) 0)
			(error "Chunk has date ~s after deriving:~%   ~s"
			       (Chunk-date ch) ch)))
		 (cond (force
			(setq must-derive
			      (delete ch must-derive))))
		 (check-interrupt))

	       (chunk-failed-to-update (ch)
		  (and (Chunk-managed ch)
		       (not (chunk-up-to-date ch))))

	       (check-interrupt ()
		  (cond ((> chunk-event-num* max-here)
			 (throw 'update-interrupted true)))))

	 ;; The following comment describes the inner recursions 
	 ;; of 'chunks-update' --

	 ;; We have two mechanisms for keeping track of updates in
	 ;; progress.  The 'in-progress' stack is used to detect a
	 ;; situation where a chunk feeds its own input, which would
	 ;; cause an infinite recursion if undetected (and may
	 ;; indicate an impossible update goal).  The mark mechanism
	 ;; is used to avoid processing a chunk which has already been
	 ;; processed. during this call to 'chunk-update'.  This is
	 ;; "merely" for efficiency, but it's not a case of premature
	 ;; optimization, because it's very easy for the derivation
	 ;; graph to have exponentially many occurrences of a chunk if
	 ;; the graph is expanded to a tree (which is what eliminating
	 ;; this optimization would amount to).  The algorithm is
	 ;; hairy because of the need to handle "update bases," the
	 ;; chunks needed to run a chunk's deriver, but not to test
	 ;; whether it is up to date.  We first propagate down to
	 ;; leaves ('check-leaves-up-to-date'), setting dates if possible
	 ;; using 'derive-date', and passing the new dates up to set
	 ;; 'latest-supporter-date' of its derivees.
	 ;; If that allows us to detect that a chunk is out
	 ;; of date, we must go down and up again through its
	 ;; update-basis.  We temporarily make the update-basis chunks
	 ;; managed (using 'unwind-protect' to avoid leaving them
	 ;; managed afterward).  The process stops when no further
	 ;; updates can be found.  Now we call 'derivees-update' to
	 ;; call 'derive' on all out-of-date chunks that can be
	 ;; reached from the marked leaves.

	 ;; Around the inner recursions is wrapped a restart mechanism
	 ;; to handle the (not uncommon) situation where deriving a chunk
	 ;; has an effect on the chunk network.  We detect this by
	 ;; noting when chunk-event-num* is incremented when we're
	 ;; not looking.  When 'chunks-update' is restarted, we do everything
	 ;; all over again, but 'derive' is _not_ rerun on any chunk it
	 ;; was previously run on.

	 (cond ((or (and force (not (null chunks)))
		    (some #'Chunk-managed chunks))
		(cond ((> chunk-update-depth* chunk-depth-error-thresh*)
		       (cerror "I will continue, with threshold depth doubled"
			       "Depth of recursive calls to 'chunks-update' exceeds ~s"
			       chunk-depth-error-thresh*)
		       (setq chunk-depth-error-thresh*
			     (* 2 chunk-depth-error-thresh*))))
		;; Every chunk derived is marked with this mark,
		;; to avoid deriving it twice (even if the update
		;; process is restarted) --
		(setq derive-mark chunk-event-num*)
		(setq max-here (+ chunk-event-num* 1))
		(setq chunk-event-num* max-here)
                (setq iter 0)
		(unwind-protect
		   (loop 
		      (setq down-mark chunk-event-num*)
		      (setq up-mark (+ chunk-event-num* 1))
		      (cond (chunk-update-dbg*
			     (format *error-output*
				     !"chunks-update [~s][~s] ~s~
                                       ~%  force: ~s postpone: ~s~
                                       ~%  Derive mark: ~s down mark: ~
                                       ~s up mark: ~s~%"
				     chunk-update-depth* iter chunks
				     force postpone-derivees
				     derive-mark down-mark up-mark)))
                      (incf iter)
		      ;; If a chunk event occurs while we're updating,
		      ;; we must restart.  We detect that if
		      ;; chunk-event-num* ever exceeds 'max-here' --
		      (setq max-here (+ up-mark 1))
		      (setq chunk-event-num* max-here)
		      (setq temporarily-managed !())
		      (unwind-protect
			 (progn
			    (cond (force
				   (cond ((and temp-mgt-dbg*
					       (not (every #'Chunk-managed
							   chunks)))
					  (format *error-output*
					     "Because it's being forced ...~%")
;;;;					  (break "Forced!")
					  ))
				   (temporarily-manage chunks)))
			    (setq found-chunks !())
			    (setq update-interrupted 
			       (catch 'update-interrupted
			          (down-then-up chunks)
				  false))
			    (cond (update-interrupted
				   (cond (chunk-update-dbg*
					  (format *error-output*
					      !"Chunk update ~
                                                interrupted~%")))
				   (cond ((and long-distance-chunk-restart*
					       (> chunk-update-depth* 1))
					  (cond (chunk-update-dbg*
						 (format *error-output*
						    !"Aborting from ~
                                                      chunk-update level ~s~%"
						    chunk-update-depth*)))
					  (throw 'update-interrupted
					         true))))))
			 (dolist (ud-ch temporarily-managed)
			    (cond (temp-mgt-dbg*
				   (format t "No longer managing ~s~%" ud-ch)))
			    (chunk-internal-term-mgt ud-ch false))
			 (chunk-event-discard down-mark)
			 (chunk-event-discard up-mark))
		      (cond ((not update-interrupted)
			     (return)))
		      ;; Make sure 'found-chunks' has no duplicates or
		      ;; chunks that are already being explored --
		      (let ((l !()))
			 (dolist (fc found-chunks)
			    (cond ((and (not (memq fc l))
					(not (memq fc chunks)))
				   (on-list fc l))))
			 (setq found-chunks l))
;;;;		      (cond ((and postpone-derivees
;;;;				  (not (null found-chunks)))
;;;;			     (format *error-output* "Postpone bogosity!~%")
;;;;			     (setq chunk-update-dbg* true)))
		      (cond (chunk-update-dbg*
			     (format *error-output*
				"Restarting update [~s] of ~s~%"
				chunk-update-depth* chunks)
			     (cond ((not (null found-chunks))
				    (format *error-output*
				       "  Plus found chunks ~s~%"
				       found-chunks)))))
		      (incf (alref chunk-restart-count*
				   chunk-update-depth*
				   0 :test #'=))
		      (setq chunks (nconc found-chunks chunks)))
		   (chunk-event-discard derive-mark)
		   )))
	 (cond (force
		(cond ((not (null must-derive))
		       (cerror "Will proceed with derivations unforced"
			       "Forced derivations of ~s did not occur"
			       must-derive)))))
	 (cond ((some #'chunk-failed-to-update
		      (cond ((or postpone-derivees
				 found-meta-cycle)
			     orig-chunks)
			    (t chunks)))
		(let ((unsuccessful
			 (retain-if #'chunk-failed-to-update
				    (cond ((or postpone-derivees
					       found-meta-cycle)
					   orig-chunks)
					  (t chunks)))))
		   (cond ((or (= chunk-update-depth* 1)
			      (> update-fail-dbg* 1))
			  (cerror "I will pretend everything is fine"
				  !"After chunks-update, the following chunks ~
				    failed to update:~
				    ~%   ~s"
				  unsuccessful))
			 ((> update-fail-dbg* 0)
			  (format *error-output*
				  !"Warning!!! After chunks-update, the ~
                                    following chunks ~
				    failed to update:~
				    ~%   ~s~%"
				  unsuccessful))))))
	 (cond (chunk-update-dbg*
		(format *error-output*
		    "Exit chunks-update [~s]~%" chunk-update-depth*)))
	 (nodup postponed))))

(defun chunk-date-note (ch context)
   (format *error-output*
      "~a ~s ~s [~a] [depth ~s]~%"
      context ch
      (Chunk-date ch)
      (cond ((chunk-up-to-date ch)
	     "up to date")
	    ((= (Chunk-date ch) +no-info-date+)
	     "no info")
	    (t
	     "out of date"))
      (Chunk-depth ch)))

(defun or-chunk-set-update-basis (orch)
   (cond ((null (Or-chunk-disjuncts orch))
	  (error !"Attempting to manage Or-chunk with ~
                   no disjuncts: ~s"
		 orch)))
   (cond ((or (null (Chunk-update-basis orch))
	      (not (Chunk-managed (first (Chunk-update-basis orch)))))
	  (dolist (b (Or-chunk-disjuncts orch))
	     (cond ((Chunk-managed b)
		    (setf (Chunk-update-basis orch)
			  (list b))
		    (return-from or-chunk-set-update-basis))))
	  (setf (Chunk-update-basis orch)
	        (list (Or-chunk-default orch))))))

;;; There is no independent content of an Or-chunk to check the date of --
(defmethod derive-date ((orch Or-chunk))
   (cond ((slot-boundp orch 'disjuncts)
          (let ((earliest-disjunct-date
                   (earliest-up-to-date-disjunct-date orch false))
                (already (Chunk-date orch)))
             ;; Let's get the logic right here (finally!): We want to
             ;; return the date when this chunk became correct.
             ;; If the currently active disjunct switches to a chunk
             ;; with an older date, that's _irrelevant_ to when this
             ;; one stabilized.  (It just means the _reason_ why this one
             ;; is stable is now a chunk that stabilized earlier.) --
             (cond ((and already earliest-disjunct-date
                         (>= already earliest-disjunct-date))
                    already)
                   (t earliest-disjunct-date))))
	 (t
	  +no-info-date+)))

;;; Run 'derive', update ch's date, and return t if date has moved 
;;; forward -- 
;;;;; <<<< Or-chunk/derive
(defmethod derive ((orch Or-chunk))
   (cond ((not (slot-truly-filled orch 'disjuncts))
	  (error "Attempt to derive Or-chunk with no disjuncts: ~s"
		 orch))
	 ((not (slot-truly-filled orch 'default))
	  (error "Deriving Or-chunk with no default disjunct: ~s"
		  orch)))
   (earliest-up-to-date-disjunct-date orch true))

(defun earliest-up-to-date-disjunct-date (orch must-exist)
   (let ((date nil))
      (dolist (d (Or-chunk-disjuncts orch)
		(or date
		    (cond (must-exist
			   (error
			       "No disjunct of or-chunk ~s is managed and up to date"
			       orch))
			  (t +no-info-date+))))
	 (cond ((and (Chunk-managed d)
		     (chunk-up-to-date d))
		(cond ((or (not date)
			   (and (< (Chunk-date d) date)
				(>= (Chunk-date d) 0)))
		       (setq date (Chunk-date d)))))))))
;;;;; >>>> Or-chunk/derive

;;;;(defun chunks-force-update (chunks postpone-derivees)
;;;;   (dolist (ch chunks)
;;;;     (chunk-derive-and-record ch))
;;;;   (let ((combined-derivees
;;;;	    (mapcan (\\ (ch) (list-copy (Chunk-derivees ch)))
;;;;		    chunks)))
;;;;      (cond (postpone-derivees
;;;;	     combined-derivees)
;;;;	    (t
;;;;	     (chunks-update combined-derivees false)))))

(defun chunk-up-to-date (ch)
;;;;   (let ()
;;;;      (cond ((null (Chunk-basis ch))
;;;;	     (leaf-chunk-update ch))
;;;;	    (t ... )))
   (chunk-date-up-to-date ch))

(defun chunk-date-up-to-date (ch)
   (and (>= (Chunk-date ch)
	    (Chunk-latest-supporter-date ch))
	(>= (Chunk-date ch) 0)))

(defun leaf-chunk-update (leaf-ch)
   (chunk-derive-date-and-record leaf-ch)
   true)

(defun chunk-derive-date-and-record (ch)
   (let ((old-date (Chunk-date ch)))
      (let ((new-date (derive-date ch)))
	 (cond (new-date
		(chunk-date-record
		    ch
		    new-date
		    old-date)))
	 new-date)))

;;; The key fact is that *** after a deriver runs, the chunk is up to
;;; date ***, or as up to date as it's ever gonna be (until its basis
;;; changes).  The basis is assumed to contain _all_ the factors that
;;; could change its value.  If the deriver returns false, then the
;;; current content (or state or location or whatever) of the chunk is
;;; correct, and, as far as the deriver can tell, has been correct
;;; since the last time the basis changed.

;;; Call 'derive', update date, and return true if date advanced (else
;;; false if already up to date).
;;; The value is currently not used by any caller.
(defun chunk-derive-and-record (ch)
  (cond ((Chunk-derive-in-progress ch)
	 (error !"Attempt to derive chunk that has a derivation ~
                  in progress:~%~S~%"
		ch)))
  (let ((successful false)
	(old-date (Chunk-date ch)))
     (unwind-protect
	(progn
	   (setf (Chunk-derive-in-progress ch) true)
	   (let ((new-date (derive ch)))
	      (chunk-date-record
		 ch
		 (or new-date
		     ;; If the deriver returned false, that means that
		     ;; the chunk is now up to date with respect to
		     ;; its supporters.
		     (max 0 old-date (Chunk-latest-supporter-date ch)))
		 old-date)
;;;;	      (cond (chunk-update-dbg*
;;;;		     (format *error-output*
;;;;			"Date set to ~s for chunk ~s~%"
;;;;			(Chunk-date ch) ch)))
	      (setq successful true)))
       (setf (Chunk-derive-in-progress ch) false))
     successful))

(defparameter max-time-discrep* 2)

;;; Returns true if the new date is later than the old one.  (No one uses
;;; the return value at this point.)
(defun chunk-date-record (ch new-date old-date)
	 (cond ((not new-date)
		(error "Attempt to record false date for ~s"
		       ch))
	       ((not (is-Number new-date))
;;;;		(setq ch* ch)
;;;;		(break 
;;;;		    "Date = ~s [old ~s] for chunk = ~s~%"
;;;;		    new-date old-date ch)
		(setq new-date (get-universal-time))))
	 (cond ((or (> new-date old-date)
		    ;; Loss of information --
		    (= new-date +no-info-date+))
		(setf (Chunk-date ch) new-date)
		(cond (chunk-update-dbg*
		       (format *error-output*
			  "Recording date ~s for ~s~%"
			  new-date ch)))
;;;;		(cond ((> new-date 10000)
;;;;		       (setq bad-ch* ch old-date* old-date)
;;;;		       (break
;;;;			  "Setting date to  (was ~s)~%  for ~s"
;;;;			  new-date old-date ch)))
		true)
	       ((< new-date old-date)
		;; For some reason universal-time dates sometimes
		;; slip by a second here or there ...
		(cond ((and (> old-date 100000000)
			    (=< (- old-date new-date) max-time-discrep*))
		       (format *error-output*
			  !"Universal-time slips by ~s sec; ~
                            alert Prof. Hawking!~%"
                           (- old-date new-date)))
		      (t
		       (cerror "I will set the date to the new date"
			       !"Chunk deriver or dater returned date ~s, ~
				 which is before current date ~s"
				 new-date old-date)))
;;; Just ignoring the new date usually causes the bug to recur --
		(setf (Chunk-date ch) new-date)
                false)
	       (t
		false)))

(defvar chunk-table* (make-hash-table :test #'equal :size 300))

(defun chunk-system-clear ()
   (chunk-table-clear)
   (setq chunk-event-num* 1)
   (setq active-chunk-events* (list 1)))

(defun chunk-table-clear ()
   (clrhash chunk-table*))

(defgeneric chunk-name->list-structure (name)

  (:method ((x t))
     x)

  (:method ((l cons))
     (mapcar #'chunk-name->list-structure l)))

(defun find-chunk (exp must-exist)
   (or (chunk-with-name exp false)
       (cond (must-exist
	      (error "Can't find chunk with name ~s" exp))
	     (t false))))

;;; IMPORTANT: All chunks should be created using the following function --

;;; Index chunks by first pathname in their names, if any
;;; If 'creator' is non-false, it's a function that creates
;;; a new chunk, which is placed in the table.
(defun chunk-with-name (exp creator &key initializer)
       ;; The kernel is the first atom in a non-car position,
       ;; which is often a pathname, but need not be.
       ;; (Which is good, because the file pathname.lisp now
       ;; depends on this one.)
   (labels ((chunk-name-kernel (e piecefn)
	       (dolist (x (funcall piecefn e) false)
		  (cond ((atom x) (return x))
			(t
			 (let ((k (chunk-name-kernel x piecefn)))
			    (cond (k (return k))))))))

	    (create (exp)
	       (let ((new-chunk (funcall creator exp)))
		  (cond ((or (not (slot-truly-filled new-chunk 'name))
;;;;			     (not (Chunk-name new-chunk))
			     (cond ((equal (Chunk-name new-chunk)
					   exp)
				    false)
				   (t
				    (cerror "I will change the name"
					    !"Name of new chunk is ~s; ~
					      it should be ~s~%"
					    (Chunk-name new-chunk)
					    exp)
				    true)))
			 (setf (Chunk-name new-chunk)
			       exp)))
		  new-chunk)))
      (let ((name-kernel
	       (let ((list-version
		        (chunk-name->list-structure exp)))
		  (cond ((atom list-version) list-version)
			(t (or (chunk-name-kernel list-version
						  #'cdr)
			       (chunk-name-kernel list-version
						  #'identity)))))))
	 (let ((bucket (href chunk-table* name-kernel)))
;;;;	    (format t "Looking for ~s in~%  bucket ~s~%"
;;;;		    exp bucket)
;;;;	    (cond ((is-Pathname exp)
;;;;		   (break "Chunk with pathname exp")))
	    (do ((bl bucket (cdr bl))
		 (c false))
		((or (null bl)
		     (equal (Chunk-name (setq c (car bl)))
			    exp))
		 (let ((found-chunk (and (not (null bl))
					 c)))
		    (cond ((and found-chunk
				(not (typep found-chunk 'Transient-chunk)))
			   found-chunk)
			  ((not creator)
			   false)
			  (t
			   ;; 'found-chunk' is either false or transient
			   ;; 'creator' has been supplied
			   (cond (found-chunk
				  (cerror !"I will delete evidence of ~
					    circularity and proceed"
					  !"Circularity during chunk ~
					    construction detected at ~
					    ~s"
					  found-chunk)))
			   (let ((temp-chunk
				    (or found-chunk
					(make-instance 'Transient-chunk
					   :name exp))))
			      (cond ((not found-chunk)
				     (on-list temp-chunk
					      (href chunk-table*
						    name-kernel))))
			      (let ((new-chunk (create exp)))
				 (setf (href chunk-table* name-kernel)
				       (cons new-chunk
					     (delete temp-chunk
						     (href chunk-table*
							   name-kernel))))
				 (cond (initializer
					(funcall initializer new-chunk)))
				 (cond ((Chunk-managed new-chunk)
					(chunk-update new-chunk false false)))
				 new-chunk))))))
	      )))))

(defun chunk-destroy (ch)
   (block seek-and-destroy
      (walk-table
	 (\\ (k chunks-with-kernel-k)
	    (cond ((memq ch chunks-with-kernel-k)
		   (setf (gethash k chunk-table*)
			 (delete ch chunks-with-kernel-k))
		   (return-from seek-and-destroy))))
	 chunk-table*)
      (cerror "I'll assume this is not a problem, since I'm destroying it anyway"
	      "Chunk not in chunk table: ~s" ch))
  (setf (Chunk-basis ch) !())
  (setf (Chunk-update-basis ch) !())
  (dolist (d (Chunk-derivees ch))
     (setf (Chunk-basis d)
           (remove ch (Chunk-basis d))))
  (dolist (d (Chunk-update-derivees ch))
     (setf (Chunk-update-basis d)
           (remove ch (Chunk-update-basis d)))))

;;; See the note attached to the methods for (setf Chunk-basis) :before/:after
;;; The following are either unnecessary or too risky --
;;;;  (dolist (b (Chunk-basis ch))
;;;;     (setf (Chunk-derivees b) (delete ch (Chunk-derivees b))))
;;;;  (dolist (d (Chunk-derivees ch))
;;;;     (setf (Chunk-basis d) (delete ch (Chunk-basis d))))
;;;;  (dolist (b (Chunk-update-basis ch))
;;;;     (setf (Chunk-update-derivees b) (delete ch (Chunk-update-derivees b))))
;;;;  (dolist (d (Chunk-update-derivees ch))
;;;;     (setf (Chunk-update-basis d) (delete ch (Chunk-update-basis d))))


(defun chunks-max-height (chunks)
   (reduce #'max chunks :key #'Chunk-height :initial-value 0))

;;; A chunk the derivation of which consists of evaluating 'form' --
(defclass Form-chunk (Chunk)
   ((form :accessor Form-chunk-form
	  :initarg :form)))

;;; It's dangerous to use the form as the name-kernel, because then there's
;;; no way to change the form without destroying the old chunk.
(defun place-Form-chunk (form &optional (name-kernel form))
   (chunk-with-name `(:form ,name-kernel)
      (\\ (name)
	 (make-instance 'Form-chunk
	    :name name))
    :initializer
      (\\ (new-form-chunk)
         (setf (Form-chunk-form new-form-chunk)
	       form))))

(defmethod derive-date ((fc Form-chunk))
   false)
;;; Why this _isn't_ +no-info-date+:
;;; If we make the seemingly conservative assumption that
;;; evaluating the form doesn't "take" and must  be done again,
;;; then the form of a Form-chunk can never include anything that
;;; affects the chunk system.  If it does, then chunks-update
;;; will realize it's been interrupted repeatedly and restart
;;; ad infinitum.  The default is to evaluate just once, which
;;; probably isn't right, and should be overridden in more
;;; specific classes.

(defmethod derive ((fc Form-chunk))
   (eval (Form-chunk-form fc))
   true)

(defun chunk-zap-dates (c)
;; Zap all chunks supporting c, except inputs.
;; Doesn't check whether already zapped, so it's sure of getting
;; them all.
   (cond ((not (null (Chunk-basis c)))
	  (setf (Chunk-date c) 0)
	  (dolist (b (Chunk-basis c))
	     (chunk-zap-dates b))
	  (dolist (u (Chunk-update-basis c))
	     (chunk-zap-dates u)))))

;;; Find a chunk path from chunk 'ch' to chunk 'd' through
;;; derivees links
(defun find-in-derivees (d ch)
   (cond ((eq d ch) (list d))
         (t
          (dolist (x (Chunk-derivees ch) false)
             (let ((l (find-in-derivees d x)))
                (cond (l
                       (return (cons ch l)))))))))
