;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: files.lisp,v 2.2 2006/05/25 14:17:12 airfoyle Exp $
	     
;;; Copyright (C) 2004-2005
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(postponed-files-update
	     always-slurp
	     debuggable debuggability* def-sub-file-type)))

(declaim (special fload-indent* now-compiling* now-slurping* now-loading*))

;;; A piece (maybe a multi-file piece) of code that can be executed.
;;; The word "filoid" is synonymous with "Code-chunk."
(defclass Code-chunk (Chunk)
  (;; Code-chunks that must be loaded when this one is --
   (callees :accessor Code-chunk-callees
	    :initform !())
   ;; Inverse of 'callees' --
   (callers :accessor Code-chunk-callers
	    :initform !())
   (alt-version :accessor Code-chunk-alt-version
		:initarg :alt-version
		:initform false)
   ;; - If not nil, the Code-file-chunk for the alternative version.
   ;; Inverse --
   (alt-version-of :accessor Code-chunk-alt-version-of
		   :initform !())
   ;; All the chunks this one "depends on" (e.g., may need to be
   ;; loaded before this one is compiled or loaded)
   (depends-on :accessor Code-chunk-depends-on
	       :initform !())))

;; These two methods ensure that 'callers' is the inverse of 'callees' --
(defmethod (setf Code-chunk-callees) :before (_ code-ch)
   (dolist (clee (Code-chunk-callees code-ch))
      (setf (Code-chunk-callers clee)
	    (remove code-ch (Code-chunk-callers clee)))))

(defmethod (setf Code-chunk-callees) :after (_ code-ch)
   (dolist (clee (Code-chunk-callees code-ch))
      (setf (Code-chunk-callers clee)
	    (adjoin code-ch (Code-chunk-callers clee)))))

;; These two ensure that 'alt-version' is the inverse of 'alt-version-of'.
;; The first also makes sure no cycle of 'alt-version' links is created.
(defmethod (setf Code-chunk-alt-version) :before (new-v code-ch)
   (cond (new-v
	  (labels ((check-for-cycle (ch)
		      (cond ((eq ch code-ch)
			     (error !"About to create a cycle of alt-version ~
				      pointers from ~s to ~s and around"
				    new-v code-ch))
			    ((Code-chunk-alt-version ch)
			     (check-for-cycle (Code-chunk-alt-version ch)))
			    (t nil))))
	    (check-for-cycle new-v))
	  (let ((current-version (Code-chunk-alt-version code-ch)))
	     (cond (current-version
		    (setf (Code-chunk-alt-version-of current-version)
			  (remove code-ch (Code-chunk-alt-version-of
					     current-version)))))))))

(defgeneric Code-chunk-pathname (cp)
   (:method ((x Code-chunk))
       false))

(defmethod (setf Code-chunk-alt-version) :after (new-v code-ch)
   (cond (new-v
	  (setf (Code-chunk-alt-version-of new-v)
		(adjoin code-ch (Code-chunk-alt-version-of new-v))))))

;;; The name of a Code-file-chunk is always its yt-pathname if non-nil,
;;; else its pathname.
;;; This represents a primitive file, not one whose contents are
;;; managed by the file/chunk system itself (e.g., an object file
;;; whose compilation is handled by a Compiled-file-chunk).
(defclass Code-file-chunk (Code-chunk)
  ((pathname :reader Code-file-chunk-pathname
	     :initarg :pathname
	     :type pathname)
   (yt-pathname :reader Code-file-chunk-yt-pathname
		:initarg :yt-pathname
		:initform false)
   ;;  -- Either false or a YTools pathname, in which case
   ;; 'pathname' is its resolution
   (kind :accessor Code-file-chunk-kind
	 :initarg :kind)
   ;; -- :source, :object, :data (":data" can include files of
   ;; of source code that won't be compiled).
   (mate :initarg :mate
	 :initform false
	 :accessor Code-file-chunk-mate)
   ;; -- If this is :source, then the chunk for the compiled :object file;
   ;; if it's :object, then the chunk for its source file;
   ;; else false.
   (readtable :accessor Code-file-chunk-readtable
	      :initarg :readtable
	      :initform false)
;;;;   ;; Files that must be loaded when this one is read
;;;;   ;;  (not currently used) --
;;;;   (read-basis :accessor Code-file-chunk-read-basis
;;;;	       :initform false)
;;;;   ;; Obsolete, but not yet elided --
;;;;   (obs-alt-version :accessor Code-file-chunk-obs-alt-version
;;;;		:initarg :obs-version
;;;;		:initform false))
  ))
;; --  For now,
;; don't allow a chunk to be associated with more than one file.

(defvar nd*)
(defvar fc*)
(defvar fd*)

;;; Tracking down strange bug--
(defmethod (setf Chunk-date) :before (new-date (fc Code-file-chunk))
   (let ((file-date (pathname-write-time (Code-file-chunk-pathname fc))))
      ;; The file-date can be nil if the file got renamed or deleted
      (cond ((and file-date
		  (> new-date
		     (+ file-date 1)))
	     (setq nd* new-date fc* fc fd* file-date)
	     (cerror "Just try to proceed"
		  !"Changing code file chunk date to ~s, which is ~
                    after its write date ~s"
		  new-date file-date)))))

(defmethod derive-date ((fc Code-file-chunk))
;;;;   (let ((v (Code-file-chunk-obs-alt-version fc)))
;;;;      (cond (v (derive-date v))
;;;;	    (t ...)))
   (let ((pn (Code-file-chunk-pathname fc)))
      (cond ((probe-file pn)
	     (file-write-date pn))
	    (t
	     (restart-case
		(error !"Code-file-chunk corresponds to ~
			 nonexistent file: ~s"
		       fc)
	       (unmanage-chunk ()
		   :report !"I'll stop managing the chunk"
		  (chunk-terminate-mgt fc ':ask)
		  (get-universal-time)))))))

(defmethod derive ((fc Code-file-chunk))
   false)

(defun Code-file-chunk-pn (file-ch)
   (or (Code-file-chunk-yt-pathname file-ch)
       (Code-file-chunk-pathname file-ch)))

(defmethod Code-chunk-pathname ((file-ch Code-file-chunk))
   (Code-file-chunk-pathname file-ch))

(defun file-chunk-is-source (file-ch)
   (eq (Code-file-chunk-kind file-ch) ':source))

(defun file-chunk-is-object (file-ch)
   (eq (Code-file-chunk-kind file-ch) ':object))

;;; 'pn' could be a Pseudo-pathname (see module.lisp)
;;; If 'pursue-alt-versions', then if the pn's Code-chunk has an
;;; 'alt-version', property, return that one instead of this one ---
;;; or rather: keep looking until the chain of alt versions runs dry.
(defgeneric pathname-denotation-chunk (pn pursue-alt-versions))

(defmethod pathname-denotation-chunk :around (pn pursue-alt-versions)
   (let ((prima-facie
	     (call-next-method pn pursue-alt-versions)))
      (cond (pursue-alt-versions
	     (labels ((pursue (code-ch)
			 (cond ((Code-chunk-alt-version code-ch)
				(pursue (Code-chunk-alt-version code-ch)))
			       (t
				code-ch))))
	        (pursue prima-facie)))
	    (t
	     prima-facie))))

(defmethod pathname-denotation-chunk ((ytpn YTools-pathname)
				      pursue-alt-versions)
                                     (declare (ignore pursue-alt-versions))
				     ;; -- ignorable because if true,
				     ;; we're already in the :around
				     ;; method that will do the pursuit
   (pathname-denotation-chunk
       (pathname-resolve ytpn false)
       false)
       ;; -- so it saves a bit of duplication to use 'false' as
       ;; the "pursue" argument in the recursion
 )

(defmethod pathname-denotation-chunk ((pn pathname) _)
   (cond ((null (Pathname-type pn))
	  (let ((source-pn (pathname-source-version pn)))
	     (cond ((not source-pn)
		    (error "No source file corresponding to ~s"
			   pn)))
	     (setq pn source-pn))))
   (place-Code-file-chunk pn))

(defun place-Code-file-chunk (pn &key kind mate)
   (multiple-value-bind (yt-pathname l-pathname)
			(cond ((is-YTools-pathname pn)
			       (values pn (pathname-resolve pn true)))
			      (t
			       (values false (pathname pn))))
      (let ((cf-chunk
	       (chunk-with-name
		  `(:code-file ,(or yt-pathname l-pathname))
		  (\\ (e)
		     (make-instance 'Code-file-chunk
			:name e
			:kind kind
			:mate mate
			:yt-pathname yt-pathname
			:pathname l-pathname
	 ;;;;	       :alt-version false
			))
		  :initializer
		      (\\ (new-chunk)
			 (cond ((not kind)
				(setf (Code-file-chunk-kind new-chunk)
				  (cond ((pathname-is-source l-pathname)
					 ':source)
					((pathname-is-object l-pathname)
					 ':object)
					(t
					 ':data)))))
			 (cond ((not mate)
				(cond ((eq kind ':source)
				       (let ((obj-pn (pathname-object-version
						        l-pathname false)))
					  (cond (obj-pn
						 (setf (Code-file-chunk-mate new-chunk)
						       (place-Code-file-chunk
							  obj-pn
							  :kind ':object
							  :mate new-chunk)))))))))))))
	 (cond ((and kind (not (eq (Code-file-chunk-kind cf-chunk)
				   kind)))
		(cerror "I will change it"
			!"New kind ~s specified for existing Code-file-chunk with kind ~s~
                          ~%  chunk: ~s"
			kind (Code-file-chunk-kind cf-chunk) cf-chunk)))
	 (cond ((and mate (not (eq (Code-file-chunk-mate cf-chunk)
				   mate)))
		(cerror "I will change it"
			!"Code-file chunk ~s~
                          ~% has mate ~s~
                          ~% Newly specified mate differs:" 
			cf-chunk (Code-file-chunk-mate cf-chunk) mate)))
	 cf-chunk)))

;;; There are two sorts of strings attached to loadable files: what files
;;; they depend on in order to be compiled and loaded; and what version of
;;; is to be loaded (source, object, or some other file entirely).  The 
;;; latter is controlled directly by user requests, using 'fload' and 'fcompl',
;;; or some other facility (simple 'load', for instance).  The former is
;;; knottier.  YTFM wants to figure out dependencies by slurping, but 
;;; 'fload'/'fcompl' should be able to decipher dependencies declared by
;;; some version of 'defsystem.'  That's where Code-dep-chunks come in (qv).

;;; An entity as (having the appropriate variant) loaded into primary
;;; memory --
(defclass Loaded-chunk (Chunk)
   (;; The entity this one governs --
    (loadee :accessor Loaded-chunk-loadee
	    :initarg :loadee)
    ;; The final basis is these plus those obtained by scanning
    ;; dependencies from whatever places are appropriate --
    (principal-bases :accessor Loaded-chunk-principal-bases
		     :initform !())
    (status :initform ':unknown
	    :accessor Loaded-chunk-status)
   ;; -- values :load-succeeded or :load-failed
   ;; once load has been attempted
    ;; The Code-dep-chunk that computes the basis of this one --
    (controller :accessor Loaded-chunk-controller
		:initarg :controller)))

(defmethod initialize-instance :after ((lc Loaded-chunk )
				       &key &allow-other-keys)
   (setf (Chunk-basis lc)
         (list (Loaded-chunk-loadee lc))))

(defgeneric place-Loaded-chunk (ch variant)
   (:method ((ch t) variant)
	    (declare (ignore variant))
      (error "No way to make a Loaded-chunk for ~s" ch)))

;;; Manages which version of a Loaded-chunk to load (source, object,
;;; etc.)
(defclass Version-to-load-chunk (Chunk)
   ((loaded :reader Version-to-load-chunk-loaded
            :initarg :loaded
            :type Chunk)  ; -- A Loaded-chunk
    (version :accessor Version-to-load-chunk-version
             :initform false)))
    ;; -- Meanings:
    ;;   :source -> don't compile, load source;
    ;;   :object -> compile only if no object; load object
    ;;   :fresh-object -> if object up-to-date, load, else = :ask-ask
    ;;   :compile -> compile and then load object;
    ;;   :ask-once -> ask user which to do and record result;
    ;;   :ask-every -> ask user every time basis is recomputed;
    ;;   :ask-ask -> ask once and ask if it should keep asking;
    ;;   :defer -> refer to value of fload-compile-flag*;
    ;;   :follow -> replace 'manip' value with value of fload-compile-flag*

(defmethod print-innards ((vc Version-to-load-chunk) srm)
   (format srm "~S" (Version-to-load-chunk-version vc)))

(defmethod derive ((fc Version-to-load-chunk))
   false)

(defclass Loaded-file-chunk (Loaded-chunk)
   (;; The variant currently selected, either a Code-file-chunk, or
    ;; a Loaded-file-chunk if we're indirecting through an obs-alt-version --
    (selection :accessor Loaded-file-chunk-selection
	  :initform false)
    ;; The criterion (supplied by user) for how to make the selection --
    (which :accessor Loaded-file-chunk-which
	   :type Version-to-load-chunk)
    (source :reader Loaded-file-chunk-source
	    :initarg :source)
    (compiled :accessor Loaded-file-chunk-compiled
	      :initform false)
    (object :accessor Loaded-file-chunk-object
	    :initform false)))

(defun Loaded-file-chunk-manip (lfc)
   (Version-to-load-chunk-version (Loaded-file-chunk-which lfc)))

(defun (setf Loaded-file-chunk-manip) (new-manip lfc)
   (let ((vc (Loaded-file-chunk-which lfc)))
      (cond ((not (eq new-manip (Version-to-load-chunk-version vc)))
             (setf (Version-to-load-chunk-version vc)
                   new-manip)
             (setf (Chunk-date vc)
                   (get-universal-time))))
      new-manip))

(defun loaded-chunk-augment-basis (loaded-ch added-bases)
  (let ((non-prin-bases (loaded-chunk-verify-basis loaded-ch)))
     ;; Avoid calling the Chunk-basis setter if possible
     ;; so as not to trigger chunks-update that might interrupt
     ;; another already in progress.--
     (cond ((some (\\ (ab)
		     (not (member ab non-prin-bases)))
		  added-bases)
	    (setf (Chunk-basis loaded-ch)
		  (append (Loaded-chunk-principal-bases loaded-ch)
			  (union non-prin-bases
				 added-bases)))))))

#|
;;; The first elements of the basis are always the principal-bases.
;;; Use this to change the rest --
(defun loaded-chunk-change-basis (loaded-ch revised-basis)
  (let ((non-prin-bases (loaded-chunk-verify-basis loaded-ch)))
     (cond ((some (\\ (rb)
		     (not (member rb non-prin-bases)))
		  revised-basis)
	    (setf (Chunk-basis loaded-ch)
		  (append (Loaded-chunk-principal-bases loaded-ch)
			  revised-basis))))))
|#

;;; Returns excess basis elements beyond the principal ones --
(defun loaded-chunk-verify-basis (loaded-ch)
   (let ((loaded-basis (Chunk-basis loaded-ch)))
      (do ((pl (Loaded-chunk-principal-bases loaded-ch)
	       (cdr pl))
	   (bl loaded-basis (cdr bl)))
	  ((or (null pl)
	       (null bl)
	       (not (eq (car bl) (car pl))))
	   (cond ((null pl) bl)
		 (t
		  (error !"Chunk for loaded entity ~s~%~
                           has bogus basis ~s~%~
                           which differs from principal basis ~s"
			 loaded-ch loaded-basis
			 (Loaded-chunk-principal-bases loaded-ch))))))))

(defmethod derive-date ((lc Loaded-file-chunk))
   false)

(defvar compilation-failure-paranoia* 1)
 
(defmethod derive ((lc Loaded-file-chunk))
   (multiple-value-bind (file-ch loaded-ch)
                        (loaded-file-chunk-and-selection lc)
      (cond ((not file-ch)
	     ;; User canceled
	     false)
	    ((not (eq loaded-ch lc))
             (error !"Loaded-file-chunk current version screwed up: ~
                      ~%  chunk: ~s~%  version: ~s"
                    lc loaded-ch))
            ((and (eq (Loaded-chunk-status lc) ':load-succeeded)
;;;;                  (progn
;;;;                     (dbg-save lc)
;;;;                     (breakpoint Loaded-file-chunk/derive
;;;;                        "Does it really need deriving? " lc)
;;;;                    t)
                  (every (\\ (pb) (=< (Chunk-date pb)
                                      (Chunk-date lc)))
                         (Loaded-chunk-principal-bases lc)))
             ;; Out-of-date-ness is only apparent; the file is loaded
             ;; and some file it depends on has been reloaded.
             false)
	    ((typep file-ch 'Compiled-file-chunk)
	     (loop
		(let ((obj-file-ch
			 (freshly-compiled-object file-ch)))
		   (cond (obj-file-ch
			  (return (file-chunk-load obj-file-ch lc)))
			 (t
			  (let* ((source-file-chunk
				    (Loaded-file-chunk-source lc))
				 (source-file-pn
				    (Code-file-chunk-pathname
				       source-file-chunk)))
			     (cond ((> compilation-failure-paranoia* 1)
				    (restart-case
				       (error 
					  "Compilation of ~s apparently failed"
					  source-file-pn)
				      (skip ()
				       :report !"Skip compilation (it will ~
						 probably come back to haunt ~
                                                 you)"
					 (return (get-universal-time)))
				      (load-source ()
				       :report "Load source version"
					 (return
					     (file-chunk-load
					         source-file-chunk
						 lc)))
				      (compile-anyway ()
				       :report !"Compile again in spite of ~
						 apparent failure"
					 (chunks-update (list file-ch)
							true false))))
				   (t
				    (cond ((> compilation-failure-paranoia*
					      0)
					   (format *error-output*
					      !"Compilation of ~s~% ~
                                                apparently ~
                                                failed, but do you care?~%"
					      source-file-pn)))
				    (chunks-update (list file-ch)
						   true false)))))))))
	    ((typep file-ch 'Code-file-chunk)
	     (file-chunk-load file-ch lc))
	    (t
	     (error "Can't extract file to load from ~s"
		    lc)))))

;;; If 'compiled-ch' is up to date, and the object file it is supposed
;;; to produce exists, return its chunk; else return false.
(defun freshly-compiled-object (compiled-ch)
   ;;; This should be redundant (and is verboten anyway) --
   ;;;;(chunk-derive-date-and-record compiled-ch)
   (cond ((not (chunk-up-to-date compiled-ch))
	  (cerror "I'll rederive the date"
		  "Compiled-file chunk unexpectedly out of date: ~s"
		  compiled-ch)
	  (chunk-derive-date-and-record compiled-ch)))
   (cond ((eq (Compiled-file-chunk-status compiled-ch)
	      ':compile-failed)
	  false)
	 (t
	  (let* ((obj-file-ch (Compiled-file-chunk-object-file
				 compiled-ch))
		 (object-file
		   (Code-file-chunk-pathname obj-file-ch)))
	     (cond ((probe-file object-file)
		    obj-file-ch)
		   (t false))))))

;;; Follow indirection links to Loaded-file-chunk 
;;;   and the contents of its 'selection' field
;;; Returns both of them (selection first).
;;; If attempt to find basis results in user canceling the management
;;; of the loaded chunk, then return false.
(defun loaded-file-chunk-and-selection (lf-ch)
   (let ((file-ch
            (Loaded-file-chunk-selection lf-ch)))
      (cond ((not file-ch) 
             (values file-ch lf-ch))
            ((loaded-chunk-set-basis lf-ch)
             (values (Loaded-file-chunk-selection lf-ch)
                     lf-ch))
            (t
             (values false nil)))))

;;; This is a "meta-chunk," which keeps the network of Loaded-chunks 
;;; up to date.
;;; It manages: "The tree of 'depends-on's above 'controllee' is
;;; up to date."  
;;; More precisely: Let C be the controller for 'controllee', a
;;; Loaded-chunk managing "Code-chunk F is loaded."  Let L be the set
;;; of Code-chunks that F requires.  Then C manages "For each
;;; Code-chunk H in L, and each Code-chunk G that requires* F , there
;;; is a chain of 'depends-on' links from G to H."
;;; Here "A requires B" means that B must be loaded before 
;;; A.  This is not a metaphysical statement, but is determined by
;;; the 'derive' method for the kind of Code-chunks involved, 
;;; presumably by inspecting various data structures, file headers, and
;;; the like.
;;; Then "requires*" is the transitive closure of "requires."  (Which 
;;; means that there is some notion of "immediacy" implied by the
;;; definition of "requires," but it's not necessary that this be
;;; formalized; "requires" and "requires*" might be the same.)

(defclass Code-dep-chunk (Chunk)
   ((controllee :reader Code-dep-chunk-controllee
		:initarg :controllee
		:type Loaded-chunk))
   (:default-initargs :basis !()))
;;; -- basis doesn't stay empty.  A Code-dep-chunk has as derivees 
;;; all the Code-dep-chunks for files that it depends on.  (Note
;;; reverse flow: If file A depends on file B, then (:loaded A) is
;;; a base of (:loaded B), but (:code-dep A) is a derivee of (:code-dep B).

(defun place-Code-dep-chunk (code-ch)
   (chunk-with-name `(:dep-tree-known ,(Chunk-name code-ch))
      (\\ (exp)
	 (make-instance 'Code-dep-chunk
	    :name exp
	    :controllee code-ch))))

(defmethod derive ((cd Code-dep-chunk))
   (error "No method supplied for figuring out what entities ~s depends on"
	  cd))
;;; -- We must subclass Code-dep-chunk to supply methods for figuring
;;; out file dependencies.  See depend.lisp for the YTFM approach.

;;; A useful check for those methods is that they must return the
;;; value of the "meta-clock"; for files being managed by 'fload', 
;;; this is the file-op-count*.   
;;; We avoid using that variable here, because so far we're talking
;;; about some kind Code-chunk, and we haven't committed to using
;;; 'fload' to manage those chunks.  (There might well be an alternative
;;; meta-clock for packages managed using 'defsystem' or asdf.)

(defgeneric Code-dep-chunk-meta-clock-val (cd)
  (:method ((cd Code-dep-chunk))
     (error "No meta-clock defined for ~s" cd)))

(defmethod derive :around ((cd Code-dep-chunk))
   (let ((d (call-next-method))
	 (date (Code-dep-chunk-meta-clock-val cd)))
      (cond ((not (and (is-Integer d)
		       (= d date)))
	     (cerror
	        "I will force the correct value"
		!"Deriving Code-dep-chunk failed to get proper clock val~
                  :~s~%" cd)
	     date)
	    (t d))))

;;; Creates a Code-dep-chunk to act as a controller for this filoid
;;; and the chunk corresponding to its being loaded.
;;; In depend.lisp we'll supply the standard controller for ordinary files
;;; following YTools rules.  
(defgeneric create-loaded-controller (filoid loaded)
  (:method ((x t) y)
     (declare (ignore y))
     (error "No way supplied to create controllers for objects of type ~s~%"
	    (type-of x))))

(defvar all-loaded-file-chunks* !())


(defmethod place-Loaded-chunk ((file-ch Code-file-chunk) manip)
   (place-Loaded-file-chunk file-ch manip))

(defun place-Loaded-file-chunk (file-chunk file-manip)
   (cond ((and file-manip
              (not (memq file-manip
                         '(:source :object :fresh-object :compile
                           :ask-once :ask-every :ask-ask :defer :follow))))
          (error "Illegal loaded-file-chunk manip ~s" file-manip)))
   (let ((is-source (file-chunk-is-source file-chunk))
	 (is-object (file-chunk-is-object file-chunk)))
      (let ((lc (chunk-with-name
                   `(:loaded ,(Code-file-chunk-pathname file-chunk))
                   (\\ (name)
                        (let ((new-lc
                                 (make-instance 'Loaded-file-chunk 
                                    :name name
                                    :loadee file-chunk
                                    :source (cond (is-object false)
                                                  (t file-chunk)))))
                           (push new-lc all-loaded-file-chunks*)
                           new-lc))
                   :initializer
                   (\\ (new-lc)
                      (let ((compiled-chunk
                               (and is-source
                                    (place-compiled-chunk file-chunk))))
                        (place-Version-to-load-chunk new-lc)
                        (cond ((not file-manip)
                               (setq file-manip ':follow)))
                        (setf (Loaded-file-chunk-compiled new-lc)
                              (cond (is-source compiled-chunk)
                                    (is-object file-chunk)
                                    (t false)))
                        (setf (Loaded-file-chunk-object new-lc)
                              (cond (is-source
                                     (place-Code-file-chunk
                                        (Code-file-chunk-pathname
                                           compiled-chunk)))
                                    (is-object file-chunk)
                                    (t false)))
                        (let ((src-file-ch (Loaded-file-chunk-source new-lc))
                              (obj-file-ch (Loaded-file-chunk-object new-lc)))
                           (cond ((and src-file-ch obj-file-ch)
                                  (setf (Code-file-chunk-mate src-file-ch)
                                        obj-file-ch)
                                  (setf (Code-file-chunk-mate obj-file-ch)
                                        src-file-ch))))
;;;;                        (format t "Initializing ~s~%"
;;;;                                new-lc)
                        (cond ((not (slot-truly-filled new-lc 'controller))
;;;;                               (format t "  Creating controller~%")
                               (setf (Loaded-chunk-controller new-lc)
                                     (create-loaded-controller
                                        file-chunk new-lc)))))))))
            (cond (file-manip
                   (let ((vc (Loaded-file-chunk-which lc)))
                      (setf (Version-to-load-chunk-version vc)
                            file-manip)
                      (setf (Chunk-date vc)
                            (get-universal-time)))))
            lc)))

(defun place-Version-to-load-chunk (loaded-chunk)
   (let ((vc (chunk-with-name
                `(:version ,(Code-file-chunk-pathname
                               (Loaded-chunk-loadee
                                  loaded-chunk)))
                (\\ (vexp)
                   (make-instance
                       'Version-to-load-chunk
                      :name vexp
                      :loaded loaded-chunk)))))
      (setf (Loaded-file-chunk-which loaded-chunk)
            vc)
      vc))

;;; Given the names, one might think this should be a subclass of 
;;; Loaded-file-chunk.  However, Loaded-file-chunks are complex entities
;;; that can map into different versions depending on user decisions.
;;; This class is just a simple loaded-or-not-loaded affair.
;;; The file doesn't have to be a source file (!?)
(defclass Loaded-source-chunk (Loaded-chunk)
   ((file-ch :accessor Loaded-source-chunk-file
	     :initarg :file)))
	  
(defun place-Loaded-source-chunk (file-ch)
   (chunk-with-name `(:loaded ,(Chunk-name file-ch))
      (\\ (name)
	 (make-instance 'Loaded-source-chunk
	    :name name
	    :file file-ch
	    :loadee file-ch
	    ))))

(defmethod derive-date ((l-source Loaded-source-chunk))
   false)

(defmethod derive ((l-source Loaded-source-chunk))
   (file-chunk-load (Loaded-source-chunk-file l-source)
		    l-source))

(defvar loading-stack* !())

(defun file-chunk-load (file-ch loaded-ch)
      (with-post-file-transduction-hooks
	 (cleanup-after-file-transduction
	    (let* ((sourcetab (code-file-chunk-readtable-if-known file-ch))
		   (*package* *package*)
		   (*readtable* (or sourcetab *readtable*))
		   (now-loading* (Code-file-chunk-pathname file-ch))
		   (loading-stack* (cons (Code-file-chunk-pathname file-ch)
					 loading-stack*))
		   (now-slurping* false)
		   (now-compiling* false)
		   (success false)
		   (errors-during-load !())
		   (fload-indent* ;;;;(+ 3 fload-indent*)
		       (* 3 (Chunk-depth file-ch))))
	       (file-op-message "Loading"
				 (Code-file-chunk-pn
				     file-ch)
				 (Code-file-chunk-pathname file-ch)
				 "...")
;;;;	       (dbg-save file-ch)
;;;;	       (breakpoint file-chunk-load
;;;;		  "Ready to load " file-ch)
	       (unwind-protect
		  (handler-bind ((error
				    (\\ (e)
				       (on-list e errors-during-load))))
		     (load (Code-file-chunk-pathname file-ch))
		     (setq success true))
		  (let ((time-now (get-universal-time)))
		     (cond (success
			    (cond (loaded-ch
				   (setf (Loaded-chunk-status loaded-ch)
					 ':load-succeeded)))
			    (file-op-message "...loaded"
					      (Code-file-chunk-pn
						  file-ch)
					      false ""))
			   (t
			    (cond (loaded-ch
				   (setf (Loaded-chunk-status loaded-ch)
					 ':load-failed)))
			    (file-op-message "...load attempt failed!"
					     false false "")))
		     time-now))))))

(defun code-file-chunk-readtable-if-known (file-ch)
   (let ((source-readtab
	    (Code-file-chunk-readtable file-ch)))
      (cond (source-readtab
	     (cond ((not (eq source-readtab *readtable*))
		    (format *error-output*
		       !"Source file readtable (from mode line) = ~s~
			 ~%   *readtable* = ~s~
			 ~%   The former will be used to load or compile ~
			     ~s~%"
		       source-readtab
		       *readtable*
		       (Code-file-chunk-pathname file-ch))))
	     source-readtab)
	    (t false))))

;;; Returns true unless user tells us to cancel the management of this
;;; chunk.
(defgeneric loaded-chunk-set-basis (loaded-ch)
   (:method ((lch t)) true))

(defvar basis-monitor-dbg* false)

(defun monitor-filoid-basis (loaded-filoid-ch)
   (cond (basis-monitor-dbg*
          (format *error-output* "Monitoring basis of ~s~%" loaded-filoid-ch)))
   (cond ((not (typep loaded-filoid-ch 'Loaded-chunk))
	  (error "Attempt to monitor basis of non-Loaded-chunk: ~s"
		 loaded-filoid-ch)))
   (let ((controllers
	    (list (verify-loaded-chunk-controller loaded-filoid-ch false)))
	 under-new-management prev-derivees)
      (loop
         (cond (basis-monitor-dbg*
                (format *error-output* "Checking controllers ~s~%" controllers)))
	 (dolist (c controllers)
	   (chunk-request-mgt c))
	 (setq prev-derivees (mapcar #'Chunk-derivees controllers))
	 ;; We expect this call to find new filoids this one depends on.
	 ;; Their controllers become derivees of the 'controllers'.
	 (chunks-update controllers false false)
	 ;; We find those derivees, turn them on, and repeat the
	 ;; cycle --
         (cond (basis-monitor-dbg*
                (format *error-output* "Considering derivees beyond ~s~%"
                        prev-derivees)))
	 (setq under-new-management
	      (mapcan (\\ (c prev-dl)
                         (cond (basis-monitor-dbg*
                                (format *error-output*
                                    "Controller ~s~%" c)))
			 (mapcan (\\ (new-d)
				    (cond ((or (Chunk-managed new-d)
					       (memq new-d prev-dl))
                                           (cond (basis-monitor-dbg*
                                                  (format *error-output*
                                                    "   Ignoring ~s because ~a~%"
                                                    new-d
                                                    (cond ((Chunk-managed new-d)
                                                           " already managed")
                                                          (t
                                                           " already a derivee")))))
					   !())
					  (t (list new-d))))
				 (Chunk-derivees c)))
		      controllers
		      prev-derivees))
	(cond ((null under-new-management)
	       (return)))
	(setq controllers under-new-management))
      (cond (basis-monitor-dbg*
             (format *error-output* "Monitor-filoid-basis done~%")))))

(defun verify-loaded-chunk-controller (lc allow-null)
   (let ((controller (Loaded-chunk-controller lc)))
      (cond (controller controller)
	    (allow-null
	     (cerror "I'll do without it, but something's wrong"
		     "Loaded-chunk has no controller: ~s" lc)
	     false)
	    (t
	     (error "Loaded-chunk has no controller: ~s" lc)))))

(defvar user-manip-asker* false)
;;; -- Function to call to ask how file should be handled.
(defvar loaded-manip-dbg* false)

;;; Possible values: 
;;; :compile -- always compile when object file is missing or out of date
;;; :source -- always load source without compiling (if missing, load object)
;;; :object -- compile if object file is missing,
;;;         else load existing object even if out of date
;;; :fresh-object -- load if up-to-date object file exists, else = :ask-ask
;;; :ask-every -- ask user what to do every time the file is looked at
;;;         (this gets tedious!)
;;; :ask-once -- ask user once and file the answer
;;; :ask-ask -- ask, then ask the user whether to keep asking
(defvar fload-compile-flag* ':fresh-object)
;;; -- IMPORTANT: This flag is not accessed directly by the user,
;;; but instead through the 'fload-compile*' symbol macro, defined
;;; in fload.lisp.

;;; This must be called whenever the 'manip' field changes.
(defmethod loaded-chunk-set-basis ((loaded-ch Loaded-file-chunk))
   (let* (;;;;(file-ch (Loaded-chunk-loadee loaded-ch))
	  (manip (Loaded-file-chunk-manip loaded-ch))
;;;;	  (alt-chunk (Code-file-chunk-obs-alt-version file-ch))
	  (source-exists (Loaded-file-chunk-source loaded-ch))
	  (obj-file-chunk (Loaded-file-chunk-object loaded-ch))
	  (object-exists
	     (and obj-file-chunk
		  (probe-file (Code-file-chunk-pathname obj-file-chunk)))))
      (cond (loaded-manip-dbg*
	     (format t !"Setting loaded chunk basis, ~
                         manip initially = ~s~%" manip)))
      (cond ((not (or source-exists object-exists)) ;;;; ... alt-chunk
	     (error "No source or object file can be found for ~s"
		    loaded-ch)))
;;;;      (cond (alt-chunk
;;;;	     (let ((alt-loaded-chunk
;;;;		       (place-Loaded-file-chunk
;;;;				  (Code-file-chunk-obs-alt-version file-ch)
;;;;				  false)))
;;;;		(setf (Loaded-chunk-principal-bases loaded-ch)
;;;;		      (list alt-loaded-chunk))
;;;;		(setf (Chunk-basis loaded-ch)
;;;;		      (list alt-loaded-chunk))
;;;;		(loaded-chunk-set-selection loaded-ch)))
;;;;	    (t ...))
      (cond ((not source-exists)
	     (setq manip ':object))
	    ((memq manip '(:defer :follow))
	     (let ((prev-manip manip))
		(setq manip fload-compile-flag*)
		(cond ((eq manip ':ask)
		       ;; Old style doesn't make enough distinctions
		       (setq manip ':ask-ask)))
		(cond ((eq prev-manip ':follow)
		       (setf (Loaded-file-chunk-manip loaded-ch)
			     manip)))))) 
     (cond ((eq manip ':fresh-object)
	     ;; We might not really mean :compile, but there's no
	     ;; way to tell if the object file is "fresh" without
	     ;; getting to the brink of compiling it, inside
	     ;; Compiled-file-chunk/derive --
	     (setq manip ':compile)))
     (cond ((memq manip '(:ask-once :ask-every :ask-ask))
	    (loop 
               (setq manip
                     (try-ask-user-for-manip loaded-ch object-exists))
               (cond ((eq manip ':cancel)
                      (cond ((not (chunk-terminate-mgt loaded-ch ':ask))
                             (return)))
                      ;; If chunk still managed, user must have had
                      ;; second thoughts about canceling, so go around
                      ;; loop again
                      )
                     (t (return)))))
	   ((and (eq manip ':object)
                 (not object-exists))
;;;;		    (cerror "I will compile the source file"
;;;;			    !"Request to load nonexistent ~
;;;;			      object file for ~s"
;;;;			    (Code-file-chunk-pathname file-ch))
            (setq manip ':compile)))
      (cond ((eq manip ':cancel)
	     (cond (loaded-manip-dbg*
		    (format *error-output*
		       "Management terminated for loaded chunk~%  ~s~%"
		       loaded-ch)))
	     false)
	    (t
	     ;; At this point manip is either :object, :source,
	     ;; or :compile
	     (cond (loaded-manip-dbg*
		    (format *error-output*
			    !"Now manip = ~s ~
				[should be :object, :source, or :compile]~
				~%  loaded-ch = ~s~%"
			    manip loaded-ch)))
	     (loaded-chunk-set-basis-after-query loaded-ch manip)
	     true))))

;;; 'manip' is either :object,:source, or :compile.
(defun loaded-chunk-set-basis-after-query (loaded-ch manip)
   (let ((other-chunks
            (cons (Loaded-file-chunk-which loaded-ch)
	          (mapcar (\\ (callee)
                             (place-Loaded-chunk callee false))
                          (Code-chunk-callees
                             (Loaded-chunk-loadee loaded-ch))))))
      (setf (Loaded-chunk-principal-bases loaded-ch)
	    (list (ecase manip
		     (:source
		      (Loaded-file-chunk-source loaded-ch))
		     (:compile
		      (place-compiled-chunk
			 (Loaded-file-chunk-source loaded-ch)))
		     (:object
		      (Loaded-file-chunk-object loaded-ch)))))
      (setf (Chunk-basis loaded-ch)
            (append (Loaded-chunk-principal-bases loaded-ch)
                    other-chunks))
      (cond (loaded-manip-dbg*
             (format *error-output*
		"Basis = ~%  ~s~%"
                (Chunk-basis loaded-ch))))
      (loaded-chunk-set-selection loaded-ch)))

(defun loaded-chunk-set-selection (loaded-ch)
      (let ((selection 
	       (head (Chunk-basis loaded-ch))))
	 (cond (loaded-manip-dbg*
		(format *error-output*
                        "Setting selection of ~s~% to ~s~%"
			loaded-ch selection)))
	 (setf (Loaded-file-chunk-selection loaded-ch)
	       selection)))

;;; A loaded-chunk's basis should start with the universal-bases,
;;; and the universal-bases should be non-empty.
;;; And, of course, the non-principal bases should be well-founded
(defun loaded-chunk-bases-in-harmony (loaded-ch non-princips)
   (let ((principals (Loaded-chunk-principal-bases loaded-ch)))
      (and (not (null principals))
	   (do ((pbl principals
		     (tail pbl))
		(bl (Chunk-basis loaded-ch)
		    (tail bl)))
	       ((null pbl)
		(every (\\ (b) (memq b non-princips))
		       bl))
	     (cond ((or (null bl)
			(not (eq (head pbl) (head bl))))
		    (return false)))))))

;;; Represents a source file having an up-to-date compiled version.
;;; Actually, an attempt to compile is good enough, because we wouldn't
;;; want to try to recompile before a basis chunk changes.
(defclass Compiled-file-chunk (Code-file-chunk)
  ((source-file :initarg :source-file
		:reader Compiled-file-chunk-source-file
		:type Code-file-chunk)
   (loaded-file  :accessor Compiled-file-chunk-loaded-file
		 :initform false
		 :type (or null Loaded-file-chunk))
   (object-file :initarg :object-file
		 :accessor Compiled-file-chunk-object-file
		 :type (or null Code-file-chunk))
   (status :initform ':unknown
	   :accessor Compiled-file-chunk-status)
   ;; -- values :compile-succeeded or :compile-failed
   ;; once compilation has been attempted
   (last-compile-time :accessor Compiled-file-chunk-last-compile-time
		      :initform -1)
   (result :accessor Compiled-file-chunk-result
	   :initform '!())))

(defgeneric place-compiled-chunk (ch))

(defmethod place-compiled-chunk ((source-file-chunk Code-file-chunk))
   (let* ((filename (Code-file-chunk-pathname source-file-chunk)))
      (let ((compiled-chunk
	       (chunk-with-name
		   `(:compiled ,filename)
		   (\\ (exp)
		      (let ((new-chunk
			       (make-instance 'Compiled-file-chunk
				  :source-file source-file-chunk
				  :pathname (pathname-object-version
					       filename
					       false)
				  :kind ':object
				  :name exp
				  ;; This will be changed when we
				  ;; derive the Loaded-basis-chunk
				  ;; for the source file, but
				  ;; it must always include the
				  ;; source-file chunk --
				  :basis (list source-file-chunk))))
			 (compiled-chunk-set-obj-version new-chunk)
			 (setf (Chunk-basis new-chunk)
			       (list source-file-chunk))
			 new-chunk))
		   :initializer
		   (\\ (new-chunk)
		      (compiled-chunk-include-dir-basis
		         new-chunk)
;;;;		      (format *error-output*
;;;;			 "After adjoining, compiled chunk basis = ~s~%"
;;;;			 (Chunk-basis new-chunk))
		      (setf (Compiled-file-chunk-loaded-file new-chunk)
			    (place-Loaded-chunk source-file-chunk false))))))
	 compiled-chunk)))
	    
(defun compiled-chunk-include-dir-basis (compiled-ch)
		      (setf (Chunk-basis compiled-ch)
			    (adjoin
			       (place-Dir-associate-chunk
				  (dir-pn
				     (Code-file-chunk-pathname
					(Compiled-file-chunk-source-file
					   compiled-ch)))
				  'obj-version
				  (let ((ofch (Compiled-file-chunk-object-file
					       compiled-ch)))
				     (and ofch
					  (dir-pn
					     (Code-file-chunk-pathname
					        ofch)))))
			       (Chunk-basis compiled-ch)
			       :test #'eq)))

(defun compiled-chunk-set-obj-version (compiled-ch)
   (let* ((source-ch (Compiled-file-chunk-source-file compiled-ch))
	  (filename (Code-file-chunk-pathname source-ch))
	  (ov (pathname-object-version filename false))
	  (real-ov (and (not (eq ov ':none))
			(pathname-resolve ov false))))
      (setf (Compiled-file-chunk-object-file compiled-ch)
	    (and real-ov (place-Code-file-chunk real-ov :kind ':object)))))

(defvar debuggability* 1)

(defmethod derive-date ((cf-ch Compiled-file-chunk))
   (let ((pn (Code-file-chunk-pn
		(Compiled-file-chunk-source-file cf-ch))))
      (let ((ov (pathname-object-version pn false)))
	 (let ((real-ov (and (not (eq ov ':none))
			     (pathname-resolve ov false)))
	       (prev-time (Compiled-file-chunk-last-compile-time cf-ch)))
	    (let  ((obj-write-time
		      (and real-ov
			   (probe-file real-ov)
			   (pathname-write-time real-ov))))
	       (cond ((and obj-write-time
			   (>= prev-time 0)
			   (< prev-time obj-write-time))
		      (format *error-output*
			 !"Warning -- file ~s apparently compiled outside ~
                           control of ~s~%"
			 real-ov cf-ch)))
	       (or obj-write-time
		   (cond (real-ov
			  (cond ((eq (Compiled-file-chunk-status cf-ch)
				     ':compile-succeeded)
				 (format *error-output*
				    !"Warning -- object file ~s apparently ~
			              deleted outside control of ~s~%"
				    real-ov cf-ch)
				 +no-info-date+)
				(t
				 prev-time)))
			 (t
			  (format *error-output*
			     !"Warning -- no object file associated with ~s~%"
			     cf-ch)
			  prev-time))))))))

;;; We can assume that derive-date has already set the date to the old write
;;; time, and that some supporter has a more recent time.
(defmethod derive ((cf-ch Compiled-file-chunk))
   (compiled-chunk-set-obj-version cf-ch)
   (let* ((old-obj-pn (Code-file-chunk-pathname
			 (Compiled-file-chunk-object-file cf-ch)))
	  (old-obj (and old-obj-pn
			(probe-file old-obj-pn)))
	  (loaded-ch (Compiled-file-chunk-loaded-file cf-ch))
	  (source-ch (Compiled-file-chunk-source-file cf-ch))
	  (loaded-ch-manip (Loaded-file-chunk-manip loaded-ch)))
      (labels ((compile-by-any-other-name ()
		  (let* ((pn (Code-file-chunk-pn source-ch))
			 (real-pn (pathname-resolve pn true))
			 (source-readtab
			     (code-file-chunk-readtable-if-known source-ch))
			 (old-obj-write-time
				  (and old-obj
				       (pathname-write-time old-obj-pn))))
		     (let ((now-compiling* pn)
			   (now-loading* false)
			   (now-slurping* false)
			   (debuggability* debuggability*)
			   (*readtable* (or source-readtab *readtable*))
			   (fload-indent*
			      (* 3 (Chunk-depth cf-ch))))
			(compile-and-record pn real-pn old-obj-pn
					    cf-ch old-obj-write-time)))))
;;;;	 (format *error-output*
;;;;	    "In Compiled-file-chunk/derive for ~s,~
;;;;             ~% manip of loaded-ch = ~s, old-date* = ~s~%"
;;;;	    cf-ch loaded-ch-manip old-date*)
	 (cond ((and (eq loaded-ch-manip ':fresh-object)
		     (not (eq fload-compile-flag* ':compile)))
		;; If we're about to compile, then the object file
		;; must be non-fresh (or nonexistent); the 'manip' sez
		;; we've got to ask the user
		(let ((manip (try-ask-user-for-manip loaded-ch old-obj)))
		   (cond ((eq manip ':compile)
			  ;; Full speed ahead!
			  (compile-by-any-other-name))
			 (t
			  (loaded-chunk-set-basis-after-query
			     loaded-ch manip)
			  (chunk-event-discard chunk-event-num*)
			  ;; This should cause an interrupt to chunks-update
			  ;; Act nonchalant --
			  false))))
	       (t
		(compile-by-any-other-name))))))

(defun try-ask-user-for-manip (loaded-ch object-exists)
   (cond (user-manip-asker*
	  (funcall user-manip-asker*
		   loaded-ch
		   object-exists))
	 (t
	  (error !"No function supplied to ask user for ~
		   desired manipulation of ~% ~s"
		 loaded-ch))))

;;; If non-false, this is a function that logs the results of
;;; compilations --
(defvar fcompl-logger* false)

(defun compile-and-record (pn real-pn object-file cf-ch old-obj-write-time)
   (file-op-message "Beginning compilation of" pn real-pn "...")
;;;;   (dbg-save cf-ch)
;;;;   (breakpoint compile-and-record
;;;;      "cf-ch = " cf-ch)
   (with-post-file-transduction-hooks
      (cleanup-after-file-transduction
	 (let ((*compile-verbose* false)
	       (apparent-success false)
	       (errors-during-compilation !()))
	    (unwind-protect
	       (handler-bind ((error
			         (\\ (e)
				    (on-list e errors-during-compilation))))
		  (cond (object-file
			 (compile-file real-pn :output-file object-file))
			(t
			 (compile-file real-pn)
			 (setq object-file (pathname-object-version
					      real-pn true))))
		  (setq apparent-success true))
	      (let* ((success
			(and apparent-success
			     object-file
			     (probe-file object-file)))
		     (time-now (get-universal-time))
		     (new-compile-time
			(cond (success
			       (pathname-write-time object-file))
			      (t
			       time-now))))
		 (cond ((not success)
			(format *error-output*
			    "Error(s) during compilation: ~s~%"
			    errors-during-compilation)))
		 (cond ((and success
			     old-obj-write-time
			     (< new-compile-time old-obj-write-time))
			(format *error-output*
			   "Object-file write-time anomaly ~s~%"
			   object-file)
			(setq success false)))
		 (cond (success
			(setf (Compiled-file-chunk-last-compile-time
				      cf-ch)
			      new-compile-time)
			(cond (fcompl-logger*
			       (funcall fcompl-logger*
					real-pn object-file)))
			(setf (Compiled-file-chunk-status cf-ch)
			      ':compile-succeeded)
			(file-op-message "...compiled to"
					 object-file false "")
			new-compile-time)
		       (t
			(setf (Compiled-file-chunk-last-compile-time
				      cf-ch)
			      time-now)
			(cond (fcompl-logger*
			       (funcall fcompl-logger*
					real-pn false)))
			(setf (Compiled-file-chunk-status cf-ch)
			      ':compile-failed)
			(file-op-message "...compilation failed!"
					 false false "")
			time-now))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar sub-file-types* !())

;;; A "sub-file" is a chunk of data that is a subset (usually proper)
;;; of a file, with the property that when the file is loaded the subset
;;; is sure to be loaded, so that (:loaded f) obviates (:slurped (S f)),
;;; where S is the kind of subfile.  (S is :macros, :nisp-type-decls, or
;;; the like.)
;;; We need 3 Chunk subclasses for sub-file-type N:
;;; Class name: N-chunk         Chunk name: (N f)            - the contents 
;;;                                                            of the sub-file
;;;             Slurped-N-chunk             (slurped (N f)) - as slurped
;;;             Loaded-N-chunk              (loaded (N f))  - Or-chunk
;;;							       (see below)
;;; Plus one or more existing classes L, typically (loaded f), that trump
;;; (slurped (N f))
;;; The Or-chunk has disjuncts (union L {(slurped (N f))}), default
;;; (slurped (N f)).

(defstruct (Sub-file-type
	    (:print-object
	       (lambda (sfty srm)
		  (format srm "#<Sub-file-type ~s>"
			  (Sub-file-type-name sfty)))))
   name
   chunker
   ;; -- Function to produce chunk (N F) for file F (N= name of Sub-file-type)
   slurp-chunker
   ;; -- Function to produce chunk (:slurped (N F))
   load-chunker
   ;; -- Function to produce chunk (:loaded (N F)) 
)

;;; Creates a slurp-task whose name is :slurp-<sub-file-type-name>,
;;; which is the value of slurp-<sub-file-type-name>*
(defmacro def-sub-file-type (sym
			     &key
			     ((:default default-handler^)
			      'nil)
			     ((:file->state-fcn file->state-fcn^)
			      '(\\ (_) nil)))
   (let  ((sym-name (Symbol-name sym)))
      (let ((subfile-class-name (build-symbol (:< (string-capitalize sym-name))
					      -chunk))
	    (slurped-sym-class-name
	       (build-symbol Slurped- (:< sym-name) -chunk))
	    (loaded-sym-class-name
	       (build-symbol Loaded- (:< sym-name) -chunk)))
	 (let* ((sub-pathname-read-fcn (build-symbol (:< subfile-class-name)
						     -pathname))
		(slurped-subfile-read-fcn
		   (build-symbol (:< slurped-sym-class-name)
				 -subfile))
		(subfile-placer-fcn (build-symbol place- (:< sym-name) -chunk))
		(slurped-subfile-placer-fcn
		   (build-symbol place-slurped- (:< sym-name) -chunk))
		(ld-pathname-read-fcn (build-symbol (:< loaded-sym-class-name)
						    -pathname))
		(loaded-class-loaded-file-acc
		   (build-symbol (:< loaded-sym-class-name)
				 -loaded-file))
		(loaded-subfile-placer-fcn
		   (build-symbol place-loaded- (:< sym-name) -chunk))
		(slurp-task-name (build-symbol (:package :keyword)
				    slurp- (:< sym-name)))
		(sub-file-type-name (build-symbol (:< sym-name)
					     -sub-file-type*)))
	    `(progn
		(defclass ,subfile-class-name (Chunk)
		   ((pathname :reader ,sub-pathname-read-fcn
			      :initarg :pathname
			      :type pathname)))

		(defmethod derive-date ((ch ,subfile-class-name))
		   (pathname-write-time (,sub-pathname-read-fcn ch)))

		;; There's nothing to derive for the subfile; as long as the
		;; underlying file is up to date, so is the subfile.	     
		(defmethod derive ((ch ,subfile-class-name))
		   false)

		(defmethod initialize-instance :after
				     ((ch ,subfile-class-name)
				      &rest _)
		   (setf (Chunk-basis ch)
			 (list (place-Code-file-chunk
				  (,sub-pathname-read-fcn ch)))))

		(defclass ,slurped-sym-class-name (Chunk)
		  ((subfile :reader ,slurped-subfile-read-fcn
			    :initarg :subfile
			    :type Chunk)))

		(defmethod initialize-instance :after
					       ((ch ,slurped-sym-class-name)
						&rest _)
		   (setf (Chunk-basis ch)
			 (list (,slurped-subfile-read-fcn ch))))

		(defun ,subfile-placer-fcn (pn)
		   (chunk-with-name
		      `(,',sym ,pn)
		      (\\ (sub-name-exp)
			 (make-instance
			    ',subfile-class-name
			   :name sub-name-exp
			   :pathname pn))))

		(defun ,slurped-subfile-placer-fcn (pn)
		   (chunk-with-name
		      `(:slurped (,',sym ,pn))
		      (\\ (name-exp)
			 (make-instance ',slurped-sym-class-name
			    :name name-exp
			    :subfile (,subfile-placer-fcn pn)))))

		(defclass ,loaded-sym-class-name (Or-chunk)
		   ((pathname :reader ,ld-pathname-read-fcn
			      :initarg :pathname
			      :type pathname)
		    (loaded-file :accessor ,loaded-class-loaded-file-acc
				 :type Loaded-chunk)))

		(defun ,loaded-subfile-placer-fcn (pn)
      ;;;;		(format t "Placing new loaded chunk for ~s subfile
      ;;;;		chunk of ~s~%" ',sym pn)
		   (chunk-with-name
		       `(:loaded (,',sym ,pn))
		       (\\ (name-exp)
   ;;;;		       (format t !"Creating new loaded chunk for ~
   ;;;;                                   ~s subfile chunk of ~s~%"
   ;;;;			       ',sym pn)
			  (let ()
			     (make-instance ',loaded-sym-class-name
				 :name name-exp
				 :pathname pn)))
		       :initializer
			  (\\ (new-ch)
			     (let* ((file-ch (place-Code-file-chunk pn))
				    (loaded-file-chunk
				       (place-Loaded-chunk file-ch false)))
				(setf (,loaded-class-loaded-file-acc new-ch)
				      loaded-file-chunk)
				(setf (Chunk-basis new-ch)
				      (list file-ch))
				(setf (Or-chunk-disjuncts new-ch)
				      (list loaded-file-chunk
					    (,slurped-subfile-placer-fcn
					        pn)))))))

		(def-slurp-task ,slurp-task-name
		   :default ,default-handler^
		   :file->state-fcn ,file->state-fcn^)

 		(defmethod derive ((x ,slurped-sym-class-name))
		   (let ((file (,sub-pathname-read-fcn
				  (,slurped-subfile-read-fcn
				     x))))
   ;;;;		   (setq slurp-ch* x)
   ;;;;		   (break "About to slurp ~s~%" file)
		      (file-slurp file
				  (list ,(build-symbol (:< slurp-task-name)
						       *))
				  false)
		      (get-universal-time)))

		(defparameter ,sub-file-type-name
		   (make-Sub-file-type
		      :name ',sym
		      :chunker #',subfile-placer-fcn
		      :slurp-chunker #',slurped-subfile-placer-fcn
		      :load-chunker #',loaded-subfile-placer-fcn))

		(setq sub-file-types*
		      (cons ,sub-file-type-name
			    (remove-if
			       (\\ (sfty)
				  (eq (Sub-file-type-name sfty)
				      ',sym))
			       sub-file-types*))))))))
)

(defun lookup-sub-file-type (name)
   (dolist (sfty
	    sub-file-types*
	    (error "Undefined sub-file-type ~s" name))
      (cond ((eq (Sub-file-type-name sfty) name)
	     (return sfty)))))

(defun compiled-ch-sub-file-link (compiled-ch callee-ch sub-file-type load)
      (pushnew (funcall
		  (Sub-file-type-chunker sub-file-type)
		  (Code-file-chunk-pathname callee-ch))
	       (Chunk-basis compiled-ch))
      (pushnew (funcall (cond (load
			       (Sub-file-type-load-chunker
				  sub-file-type))
			      (t
			       (Sub-file-type-slurp-chunker
				  sub-file-type)))
			 (Code-file-chunk-pathname callee-ch))
	       (Chunk-update-basis compiled-ch)))

(eval-when (:compile-toplevel :load-toplevel :execute)
   (def-sub-file-type :macros)

;;;;   (defparameter standard-sub-file-types* (list macros-sub-file-type*))

   (defun macros-slurp-eval (e _) (eval e) false))

(datafun :slurp-macros defmacro #'macros-slurp-eval)

(datafun :slurp-macros defsetf  #'macros-slurp-eval)

(datafun :slurp-macros defstruct #'macros-slurp-eval)

(datafun :slurp-macros defvar
   (defun :^ (form _)
      (eval `(defvar ,(cadr form)))
      false))

(datafun :slurp-macros defconstant
   (defun :^ (form _)
      (cond ((not (boundp (cadr form)))
	     (eval form)))
      false))

(datafun :slurp-macros subr-synonym #'macros-slurp-eval)

(datafun :slurp-macros datafun
  (defun :^ (e _)
    (cond ((eq (cadr e) 'attach-datafun)
	   (eval e)))
    false))

(datafun :slurp-macros eval-when
   (defun :^ (form _)
      (cond ((memq ':slurp-toplevel (cadr form))
	     (dolist (e (cddr form))
	        (eval e))))
      false))

(datafun :slurp-macros eval-when-slurping
   (defun :^ (form _)
      (dolist (e (cdr form))
	 (eval e))
      false))

(defmacro needed-by-macros (&body l)
   `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@l))

(datafun :slurp-macros needed-by-macros eval-when-slurping)

;;;;(defun compilable (pathname)
;;;;   (member (Pathname-type pathname) source-suffixes* :test #'equal))

;;; For debugging --
(defun flchunk (pn)
   (let ((pn (->pathname pn)))
      (place-Loaded-file-chunk (place-Code-file-chunk pn)
			       false)))

;;; (file loaded compiled)-chunk list
(defun f-l-c-chunk (pn)
   (let ((l-ch (flchunk pn)))
      (let ((f-ch (Loaded-chunk-loadee l-ch)))
	 (let ((c-ch (place-compiled-chunk f-ch)))
	    (values f-ch l-ch c-ch)))))