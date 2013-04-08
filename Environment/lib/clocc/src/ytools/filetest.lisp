;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;; $Id: filetest.lisp,v 2.1 2005/12/26 00:25:17 airfoyle Exp $

(defclass Test-file-chunk (Code-file-chunk)
   ((callee :accessor Test-file-chunk-callee
	    :initarg :callee)
    (slurpee :accessor Test-file-chunk-slurpee
	     :initarg :slurpee)))

(defclass Test-code-dep-chunk (Code-dep-chunk)
   ())

(defvar file-op-count* 0)

(defmethod Code-dep-chunk-meta-clock-val ((tcd Test-code-dep-chunk))
   file-op-count*)

(defmethod create-loaded-controller ((tfc Test-file-chunk)
				     (lc Loaded-file-chunk))
   (chunk-with-name `(:test-dep ,tfc)
      (\\ (name)
	 (make-instance 'Test-code-dep-chunk
	    :name name
	    :controllee lc))))

(defmethod derive ((tlc Test-code-dep-chunk))
   (let* ((loaded-ch (Code-dep-chunk-controllee tlc))
	  (file-ch (Loaded-chunk-loadee loaded-ch))
	  (compiled-ch (place-compiled-chunk file-ch))
	  (c (Test-file-chunk-callee file-ch))
	  (s (Test-file-chunk-slurpee file-ch)))
      (cond (c
	     (setf (Code-chunk-callees file-ch) (list c))
	     (loaded-chunk-change-basis
	        loaded-ch (list (place-Loaded-file-chunk c false)))
	     (compiled-ch-sub-file-link compiled-ch c macros-sub-file-type*
					true)))
      (cond (s
;;;;	     (setq cch* compiled-ch s* s)
;;;;	     (break "About to link ~s from ~s" compiled-ch s)
	     ;; This kind of link corresponds to nothing that
	     ;; occurs "in nature," namely a slurpee that isnt'
	     ;; a callee.--
	     (compiled-ch-sub-file-link
	         compiled-ch
		 s macros-sub-file-type* true)))
      file-op-count*))

(defvar file-chunk-l*)
(defvar file-chunk-s*)
(defvar file-chunk-c*)
(defvar file-chunk-d*)

(defvar loaded-file-chunk-l* false)
(defvar loaded-file-chunk-s* false)
(defvar loaded-file-chunk-c* false)
(defvar loaded-file-chunk-d* false)

(defvar compiled-file-chunk-c* false)

(defvar file-l-status*)
(defvar file-s-status*)
(defvar file-c-status*)
(defvar file-d-status*)

;;; This is of type (Lst (Lst (Lrcd String String)))
;;; The i'th element of 'file-contents*', call it Ci, describes
;;; the contents of the test files on iteration 'i' of the 'mapcar'
;;; iteration in file-test.  Every element of Ci is a pair
;;; (filename file-contents).
(defparameter file-contents*
   (list 
;;; Iteration 0 --
      (list

(list '"tezt-l.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-l-status* ':slurped))

(setq file-l-status* ':loaded)

(eval-when (:load-toplevel)
   (setq file-l-status* ':compiled))

(defun stat-c (x) (> x 0))
"
)

(list '"tezt-s.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defmacro set-file-c-status (x)
   `(setq file-c-status* ',x))

(defmacro set-file-d-status (x)
   `(setq file-d-status* ',x))

(needed-by-macros (setq file-s-status* ':slurped))

(setq file-s-status* ':loaded)

(eval-when (:load-toplevel)
   (setq file-s-status* ':compiled))

(defun stat-d (x) (> x 0))
"
)

(list '"tezt-c.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-c-status* ':slurped))

(set-file-c-status :loaded)

(cond ((stat-c -1)
       (set-file-c-status :loaded-alt)))

(eval-when (:load-toplevel)
   (set-file-c-status :compiled)
   (cond ((stat-c -1)
	  (set-file-c-status :compiled-alt))))
"
)

(list "tezt-d.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-d-status* ':slurped))

(set-file-d-status :loaded)

(cond ((stat-d -1)
       (set-file-d-status :loaded-alt)))

(eval-when (:load-toplevel)
   (set-file-d-status :compiled)
   (cond ((stat-d -1)
	  (set-file-d-status :compiled-alt))))

"
 )
       )
     
;;; Iteration 1 --
      (list

(list '"tezt-l.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-l-status* ':slurped-1))

(setq file-l-status* ':loaded-1)

(eval-when (:load-toplevel)
   (setq file-l-status* ':compiled-1))

(defun stat-c (x) (< x 0))
"
)

;;; tezt-s, tezt-c, tezt-d stay the same
       )

;;; Iteration 2 --
      (list

;;; tezt-l.lisp stays the same

(list '"tezt-s.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defmacro set-file-c-status (x)
   `(setq file-c-status* (build-symbol (:package :keyword) z- (:< ',x))))

(defmacro set-file-d-status (x)
   `(setq file-d-status* (build-symbol (:package :keyword) y- (:< ',x))))

(needed-by-macros (setq file-s-status* ':slurped-2))

(setq file-s-status* ':loaded-2)

(eval-when (:load-toplevel)
   (setq file-s-status* ':compiled-2))
"
)

;;; tezt-c and tezt-d stay the same     
    )

;;; Iteration 3 -- 
   (list 

;;; tezt-l.lisp na~o se muda ...

(list '"tezt-s.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defmacro set-file-c-status (x)
   `(setq file-c-status* (build-symbol (:package :keyword) zz- (:< ',x))))

(defmacro set-file-d-status (x)
   `(setq file-d-status* (build-symbol (:package :keyword) yy- (:< ',x))))

(needed-by-macros (setq file-s-status* ':slurped-3))

(setq file-s-status* ':loaded-3)

(eval-when (:load-toplevel)
  (setq file-s-status* ':compiled-3))

(defun stat-d (x) (< x 0))
"
)
;;; ... nem tezt-c, tezt-d
    )

;;; Iteration 4
   (list

(list '"tezt-l.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-l-status* ':slurped-4))

(setq file-l-status* ':loaded-4)

(eval-when (:load-toplevel)
   (setq file-l-status* ':compiled-4))

(defun stat-c (x) (< x 0))
"
)

(list '"tezt-s.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defmacro set-file-c-status (x)
   `(setq file-c-status* (build-symbol (:package :keyword) z4- (:< ',x))))

(defmacro set-file-d-status (x)
   `(setq file-d-status* (build-symbol (:package :keyword) y4- (:< ',x))))

(needed-by-macros (setq file-s-status* ':slurped-4))

(setq file-s-status* ':loaded-4)

(eval-when (:load-toplevel)
  (setq file-s-status* ':compiled-4))

(defun stat-d (x) (> x 0))
"
)
;;; tezt-c and tezt-d are unchanged
    )
))
  
;;; The following test must be able to run prior to the definition
;;; of 'depends-on', and hence does not use 'fload', 'fcompl', or 
;;; slurping.  So some of the stuff normally done by these facilities,
;;; such as incrementing the file-op-count*, is done by the tester --

(defparameter correct-file-test-result*
   '((:loaded :slurped :compiled :initial)
     (:loaded-1 :slurped :compiled-alt :initial)
     (:loaded-1 :slurped-2 :z-compiled-alt :initial)
     (:loaded-1 :compiled-3 :zz-compiled-alt :yy-compiled-alt)
     (:loaded-1 :compiled-4 :zz-compiled-alt :y4-compiled)))

(defvar sml-ch*)

(defun sml-check ()
   (setq sml-ch*
     (chunk-with-name
	 `(:slurped (:macros ,(Code-file-chunk-pathname file-chunk-l*)))
	 false))
   (cond ((and sml-ch*
	       (Chunk-managed sml-ch*)
	       (not (reason-to-manage sml-ch*)))
	  (break "chunk managed badly"))))

;;;;(defun loaded-c-check ()
;;;;      (cond ((and (Chunk-managed loaded-file-chunk-c*)
;;;;		  (not (every (\\ (b) (Chunk-managed b))
;;;;			      (Chunk-basis loaded-file-chunk-c*))))
;;;;	     (break "Damn"))))


(defvar sms-ch*)

;;; Note that the following function clears the chunk table, so it
;;; should not be used except in situations where it's okay to 
;;; start everything from scratch. --

(defun file-test (&optional (num-iters (length correct-file-test-result*)))
   (chunk-system-clear)
   (setq file-op-count* (+ file-op-count* 1))
   ;; Set up the chunk network --
   (setq file-l-status* ':initial)
   (setq file-s-status* ':initial)
   (setq file-c-status* ':initial)
   (setq file-d-status* ':initial)
   (fmakunbound 'set-file-c-status)
   (fmakunbound 'set-file-d-status)
   (fmakunbound 'stat-c)
   (fmakunbound 'stat-d)
   (setq file-chunk-l* false)
   (setq file-chunk-s* false)
   (setq file-chunk-c* false)
   (labels ((set-em-up (name-chars callee slurpee manip)
	       (let ((file-ch
		        (place-Code-file-chunk
			   (merge-pathnames
			       (concatenate 'string
				  "tezt-" name-chars ".lisp")
			       *load-truename*))))
		  (change-class file-ch 'Test-file-chunk
				:callee callee :slurpee slurpee)
		  (let ((loaded-ch
			   (place-Loaded-chunk file-ch manip)))
		     (values file-ch loaded-ch)))))
      (multiple-value-setq
	  (file-chunk-l* loaded-file-chunk-l*)
	  (set-em-up "l" false false ':source))
      (sml-check)
      (multiple-value-setq
	  (file-chunk-s* loaded-file-chunk-s*)
	  (set-em-up "s" false false ':compile))
      (sml-check)
      (multiple-value-setq
	  (file-chunk-c* loaded-file-chunk-c*)
	  (set-em-up "c" file-chunk-l* file-chunk-s* ':compile))
      (setq compiled-file-chunk-c*
	    (place-compiled-chunk file-chunk-c*))
      (sml-check)
      (setq file-chunk-d* false)
      (setq loaded-file-chunk-d* false)
      (chunk-request-mgt loaded-file-chunk-c*)
;;;;      (break "loaded-file-chunk-c* basis = ~s"
;;;;	     (mapcar (\\ (b) (tuple (Chunk-manage-request b)
;;;;				    (Chunk-managed b)
;;;;				    b))
;;;;		     (Chunk-basis loaded-file-chunk-c*)))
;;;;      (loaded-c-check)
      (let ((res (mapcar
		    (\\ (i files-cont)
		       (cond ((> i 0)
			      ;; Avoid things happening so fast
			      ;; that universal-time is inadequate
			      (sleep 2)))
		       (format t "~%Begin iteration ~s ~s~%"
			       i (get-universal-time))
		       (dolist (p files-cont)
			  (with-open-file (srmf (first p)
					    :direction ':output
					    :if-exists ':supersede)
			     (princ (second p) srmf)))
		       (dolist (ch (list loaded-file-chunk-l*
					 loaded-file-chunk-s*
					 loaded-file-chunk-c*))
			  (monitor-filoid-basis ch)
			  (loaded-chunk-set-basis ch))
;;;;		       (format t "Date of compiled tezt-c: ~s~%"
;;;;			       (Chunk-date compiled-file-chunk-c*))
;;;;		       (break "Filoid bases monitored")
		       (case i
			  (0 (chunk-update loaded-file-chunk-c* false false))
			  (1 (chunk-update file-chunk-l* false false))
			  (2 (chunk-update file-chunk-s* false false)
			     (setq sms-ch*
			           (chunk-with-name
				      `(:slurped (:macros ,(Code-file-chunk-pathname
							      file-chunk-s*)))
				      false))
			     (sms-check "at end of iter 2"))
			  (3 (sms-check "before creating d")
			     (multiple-value-setq
				(file-chunk-d* loaded-file-chunk-d*)
				(set-em-up "d" file-chunk-s* false ':compile))
			     (sms-check "before monitoring d")
			     (monitor-filoid-basis loaded-file-chunk-d*)
			     (loaded-chunk-set-basis loaded-file-chunk-d*)
			     (sms-check "after monitoring d")
			     (chunk-request-mgt loaded-file-chunk-d*)
			     (sms-check "after d mgt request")
;;;;			     (chunk-terminate-mgt loaded-file-chunk-c* false)
			     (chunks-update
			        (list ;;;; loaded-file-chunk-c*
				      loaded-file-chunk-d*)
				false false))
			  (4 (chunk-terminate-mgt loaded-file-chunk-c* false)
			     (chunk-update loaded-file-chunk-c* false false)
			     (chunk-update ;;;;loaded-file-chunk-d*
			                   file-chunk-s* false false)))
		       (format t !"After iteration ~s:~
				   ~% File 'l' status = ~s ~
                                   File 's' status = ~s ~
				   ~% File 'c' status = ~s ~
                                   File 'd' status = ~s~%~%"
			       i file-l-status* file-s-status*
				 file-c-status* file-d-status*)
;;;;		       (format t "compiled-file-chunk-c* date = ~s~%"
;;;;			       (Chunk-date compiled-file-chunk-c*))
		       (list file-l-status* file-s-status*
			     file-c-status* file-d-status*))
		    (series 0 (- num-iters 1))
		    file-contents*)))
	 (values (every
		    (\\ (r w)
		       (every #'eq r w))
		    res
		    correct-file-test-result*)
		 res
		 (mapcar
		    (\\ (r w)
		       (mapcar #'eq r w))
		    res
		    correct-file-test-result*)))))

(defun sms-check (label)
   (cond ((and (Chunk-managed sms-ch*)
	       (not (reason-to-manage sms-ch*)))
	  (break "Unreasonable management ~a" label))))

