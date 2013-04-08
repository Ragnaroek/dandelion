;-*- Mode: Common-lisp; Package: ytools; -*-
(in-package :ytools)
;;;$Id: raw-ytfm-load.lisp,v 2.1 2005/12/26 00:25:17 airfoyle Exp $

;;; This file is for recompiling a subset of ytools-core-files* 
;;; (in the proper order) when debugging YTFM.
;;; It assumes that ytload/ytload.lisp has already been loaded.
;;; The working directory should be set to the ytools directory.

(load-yt-config-file)

(setq *default-pathname-defaults*
      (pathname ytools-home-dir*))

(load (concatenate 'string ytools-home-dir* "ytload/ytfm.lmd"))

(cond ((boundp 'bin-idio*)
       (load (concatenate 'string ytools-home-dir* "ytools.lsy"))))

(setq *readtable* ytools-readtable*)

(defvar last-yt-comp* nil)

(defun yt-comp (&optional fname)
   (setq *readtable* ytools-readtable*)
   (cond (fname
	  (setq last-yt-comp* fname))
	 (last-yt-comp*
	  (setq fname last-yt-comp*))
	 (t
	  (error "No default yt-comp argument set yet")))
   (compile-file fname
		 :output-file (merge-pathnames
			         (make-pathname :name fname :type lisp-object-extn*)
				 ytools-bin-dir-pathname*)))

(defun yt-bload (&optional fname)
   (cond ((not fname)
	  (cond (last-yt-comp*
		 (setq fname last-yt-comp*))
		(t
		 (error "No default yt-comp argument set yet")))))
   (load (merge-pathnames
	    (make-pathname :name fname :type lisp-object-extn*)
	    ytools-bin-dir-pathname*)))

;;; Load ytfm (ytools-core-files*).  Substitute some of the
;;; files as indicated in the 'new-files'
;;; alist (each entry is (old-file-name temp-new-file-name compile)).
;;; If 'cautious', then compile such replacements,
;;; then recompile all files after the first replacement.
;;; Otherwise, the third slot of the alist entry determines
;;; whether to compile.
;;; Special case: If 'new-files' is (), recompile everything.
(defun yt-recompile (new-files cautious
		     &key (start-with nil) (stop-after nil))
   (let ((compiling (null new-files))
	 (files (cond (start-with
		       (member start-with ytools-core-files*
			       :test #'string=))
		      (t ytools-core-files*))))
      (dolist (fname files (push :ytfm all-loaded*))
	 (let ((e (assoc fname new-files :test #'string=)))
	    (cond (e
		   (cond (cautious
			  (setq compiling t)))
		   (cond ((or compiling (third e))
			  (yt-comp (second e))
			  (yt-bload))
			 (t
			  (yt-bload (second e)))))
		  ((and compiling cautious)
		   (yt-comp fname)
		   (yt-bload))
		  (t
		   (yt-bload fname)))
	    (cond ((and stop-after
			(string= fname stop-after))
		   (return)))))))

;;; More variations on the same theme.
;;; This is all ad-hoc, and contains various directory names hard-wired. --

(defun co (s &optional no-lo)
   (compile-file
      (concatenate 'string "~/CVSified/dev/clocc/src/ytools/" s ".lisp")
      :output-file
      (concatenate 'string "~/CVSified/dev/clocc/bin/acl62/ytools/" s ".fasl"))
  (cond ((not no-lo) (lo s))))

(defun lo (s)
   (load (concatenate 'string "~/CVSified/dev/clocc/bin/acl62/ytools/"
		      s ".fasl")))
