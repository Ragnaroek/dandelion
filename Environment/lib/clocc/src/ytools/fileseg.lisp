;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(def-file-segment)))


;;; Let's keep this really low-level, part of YTFM
;;;; (depends-on %module/ ytools) 
;;;;(depends-on (:at :read-time) %ytools/ bq)


(defclass File-segment-chunk (Chunk)
  ((name :initarg :name)
   ;; Chunk for the file containing this segment --
   (containing-file :initarg :containing-file
		    :accessor File-segment-chunk-containing-file)
   ;; Function of no arguments such that calling it derives
   ;; the chunk --
   (content :initarg :content
	    :accessor File-segment-chunk-content)
   (content-call-date :initform +no-info-date+
		      :accessor File-segment-chunk-content-call-date)))
		    

(defmacro def-file-segment (name basis-names &body forms)
   (labels ((strings-to-pathnames-converter (name)
	       (cond ((is-String name)
		      `(->pathname ',name))
		     ((atom name)
		      `(find-chunk ',name true))
		     (t
		      `(list (<# strings-to-pathnames-converter
				 name))))))
      (let* ((containing-file (assoc ':containing-file forms))
	     (file-key-arg
		(cond (containing-file
		       `(:containing-file ,(cadr containing-file)))
		      (t '()))))
	 (cond (containing-file
		(setq forms (remove containing-file forms))))
	 `(define-file-segment
	      ',name
	      (list ,@(<# strings-to-pathnames-converter
			  basis-names))
	      (\\ () ,@forms)
	      ,@file-key-arg))))

(defvar file-seg-chunk*)

(defun define-file-segment (name basis fcn &key (containing-file nil))
;;;   (!= fcn* fcn)
   (let* ((this-file-pathname
	    (or containing-file
		now-loading*
		(error !"'define-file-segment' ~s occurs ~
                          outside recognizable file"
		       name)))
	  (this-file-chunk
	     (cond ((eq containing-file ':none)
		    false)
		   (t
		    (place-Code-file-chunk
		       (->pathname this-file-pathname)))))
	  (source-file-chunk
	      (cond (this-file-chunk
		     (let* ((kind (Code-file-chunk-kind this-file-chunk))
			    (mate (Code-file-chunk-mate this-file-chunk)))
			(cond ((and mate (eq kind ':object))
			       mate)
			      (t this-file-chunk))))
		    (t false))))
;;;;      (err-out "Calling chunk-with-name " name :%)
      (let ((file-seg-chunk
	       (chunk-with-name name
		   (\\ (_)
		      (make-instance 'File-segment-chunk
			 :name name)))))
	 (setf (File-segment-chunk-containing-file file-seg-chunk)
	       source-file-chunk)
	 (setf (File-segment-chunk-content file-seg-chunk)
	       fcn)
	 (setf (Chunk-basis file-seg-chunk)
	       (cond (source-file-chunk
		      (cons source-file-chunk basis))
		     (t basis)))
	 ;; Phase 1 -- no update basis --
	 (setf (Chunk-update-basis file-seg-chunk)
	       !()) 
	 (setf (File-segment-chunk-content-call-date file-seg-chunk)
	       +no-info-date+)
	 (chunk-request-mgt file-seg-chunk)
	 (chunk-update file-seg-chunk false false)
	 (setq file-seg-chunk* file-seg-chunk)
	 ;; Phase 2 -- update basis includes "this very file is loaded"
	 ;; We can't incorporate it before now, because during the
	 ;; initial update we don't need (and definitely don't want) to
	 ;; force the file we are now loading to be loaded.
	 (cond (source-file-chunk
		(setf (Chunk-update-basis file-seg-chunk)
		      (list (place-Loaded-chunk
			       source-file-chunk false)))))
	 file-seg-chunk)))
			    
;;; A File-segment-chunk is up to date if either of the following is true:
;;; - Its file was loaded after all of its other bases' dates.
;;; - Its content function was called after all of its other bases' dates.

(defvar file-seg-dbg* false)

(defmethod derive ((file-seg-ch File-segment-chunk))
   (let ((loaded-date (loaded-date-if-loaded file-seg-ch))
	 (content-call-date
	    (File-segment-chunk-content-call-date file-seg-ch))
	 (latest-supp-date (Chunk-latest-supporter-date file-seg-ch)))
      (cond (file-seg-dbg*
	     (format *error-output* "Deriving ~s~%" file-seg-ch)))
      (cond ((and (> loaded-date 0)
		  (> content-call-date 0)
		  (or (>= loaded-date latest-supp-date)
		      (>= content-call-date latest-supp-date)))
	     (cond (file-seg-dbg*
		    (format *error-output*
		       !"File-segment chunk up to date:~% latest supporter=~s ~
                         loaded-date=~s content-call-date=~s~%"
                         latest-supp-date loaded-date content-call-date)))
	     ;; up to date
	     false)
	    (t
	     (cond (file-seg-dbg*
		    (format *error-output*
		       "Calling fileseg chunk deriver~%")))
	     (funcall (File-segment-chunk-content file-seg-ch))
	     (let ((now (get-universal-time)))
	        (setf (File-segment-chunk-content-call-date file-seg-ch)
		      now)
		now)))))

(defmethod derive-date ((file-seg-ch File-segment-chunk))
   (let* ((loaded-date (loaded-date-if-loaded file-seg-ch))
	  (content-call-date
	     (File-segment-chunk-content-call-date file-seg-ch))
	  (update-date (max loaded-date content-call-date)))
      (let ((cand-date
	       (reduce #'max (Chunk-basis file-seg-ch)
		       :initial-value update-date
		       :key #'Chunk-date)))
	 (cond ((= cand-date update-date)
		update-date)
	       (t
		false)))))

(defun loaded-date-if-loaded (file-seg-ch)
   (let ((updb (Chunk-update-basis file-seg-ch)))
      (cond ((null updb)
	     +no-info-date+)
	    (t
	     (Chunk-date (first updb))))))

