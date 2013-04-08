;-*- Mode: Common-lisp; Package: ytools; -*-
;;;$Id: ytload.lisp,v 2.5 2006/08/19 14:54:14 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(defpackage :ytools
   (:nicknames #:yt)    ;;;;#:mcdermott
   (:use :common-lisp)
   ;; Reason for these exports kind of lost in the mists of time....
   (:export #:series #:forall #:exists #:either #:yt-load #:yt-install
	   "^^" "?" #:object)
   (:import-from :cl-user cl-user::yt-install cl-user::yt-load)
   (:import-from :cl-user cl-user::mcd-install cl-user::mcd-load))
		   
(in-package :ytools)

;;;;(defvar tools-version* "1.3")

;; Directory ending in system-specific directory delimiter.
;; nil if unknown.
(defvar config-directory* nil)
(defvar ytload-directory* "")
(defvar config-file* "ytconfig.lisp")

;;;;(defvar bin-bit* "")

(defmacro intern-call (pkg fcn &rest args)
   `(funcall (intern ',(string fcn) (find-package ',(string pkg)))
	     ,@args))

;; We need the orig-pkg because different implementations expand macros
;; at different times, so we can't control the package in effect when the
;; first format is called.
(defmacro intern-eval (pkg expression orig-pkg)
   `(let ((_ss_ (let ((*package* (find-package ',orig-pkg)))
		    (format nil "~a" ',expression))))
       (let ((*package* (find-package ',pkg)))
	  ;;(declare (special *package*))
	  (let ((xx (with-input-from-string (_srm_ _ss_)
		       (read _srm_))))
	     (format t "Intern-eval ~s ~%   [~s]: ~s ~%   [~s]: ~s~%"
		       ',expression ',orig-pkg _ss_ ',pkg xx)
	     (eval xx)))))
   
;;   `(eval (intern ',(string var) (find-package ',(string pkg)))))

(defvar config-variables* '(config-variables*))

;; This list holds variables that will be added to config-variables*
;; just before the next dump of the config file.  (Otherwise, loading
;; the config file would reset the config-variables.)
(defvar new-config-variables* '())
    
(defmacro declare-config-var (vname)
   `(progn (declaim (special ,vname))
	   (setq new-config-variables*
		 (adjoin ',vname new-config-variables* :test #'eq))))

(defmacro is-set (vname)
   `(and (boundp ',vname) ,vname))

(declare-config-var directory-delimiter*)
(declare-config-var filename-case*)

(declare-config-var all-installed*)
(defvar all-installed* '())

;; This is *not* a config-var; it doesn't persist from run to run.
(defvar all-loaded* '())

;;; 'start-over' = true means ignore existing settings for config vars
;;; Implies reinstall.
;;; 'start-over' keyword arg has been made obsolete by :start-over value of
;;; :'if-installed' arg
;;; Return value = t if install was successful (including if it was
;;; unnecessary).  This is a change (2006-08-02); previously it meant
;;; something was actually installed.
(defun cl-user::yt-install (module &key (if-installed ':warn)
					(start-over nil)
					(config-dir nil))
   (setq module (intern (string module) :keyword))
   (cond (config-dir
	  (setq config-directory* config-dir)))
   (load-yt-config-file)
   (cond ((or start-over
	      (not (check-installed module))
	      (member if-installed '(:reinstall :start-over))
	      (and (eq if-installed ':warn)
		   (y-or-n-p "Module ~s already installed; reinstall?"
			     module)))
	  (cond ((really-install module
                                 (or start-over
                                     (eq if-installed ':start-over)))
		 (format t "~a is installed~%" module)
		 t)
		(t nil)))
	 (t t)))

(defun really-install (module start-over)
   (load-module-file module)   
   (cond ((call-installer-or-loader ':install module start-over)
	  (note-installed module)
	  (dump-yt-config-file)
	  t)
	 (t nil)))
	  
(defun check-installed (module)
   (member module all-installed* :test #'equal))

(defun note-installed (module)
	  (setq all-installed*
		(adjoin module all-installed* :test #'equal)))

;;; Backward compatibility
;;;;(defun cl-user::mcd-load (module &optional (if-loaded ':warn) (config-dir nil))
;;;;   (yt-load module :if-loaded if-loaded :config-dir config-dir))

(defun cl-user::yt-load (&optional (module ':ytfm)
			 &key (if-loaded ':warn)
                              ;; -- or :reload or nil
			      (if-not-installed ':ask)
                              ;; -- or :install or :noload
			      (config-dir nil))
;;;;   (format t "Ab initio, config-directory* = ~s (config-dir = ~s)~%"
;;;;	   config-directory* config-dir)
   (setq module (intern (string module) :keyword))
   (cond (config-dir
	  (setq config-directory* config-dir)))
   (load-yt-config-file)
;;;;   (format t "Before attempting load, config-directory* = ~s~%"
;;;;	   config-directory*)
   (cond ((or (member if-loaded '(:force :reload))
	      (not (check-loaded module)))
	  (load-module-file module)
	  (let ((loadable (check-installed module)))
	     (cond ((and (not loadable)
			 (or (eq if-not-installed ':install)
			     (and (not (eq if-not-installed ':noload))
				  (y-or-n-p "System ~a not installed; shall I install it? "
				     module))))
		    (cond ((really-install module nil)
			   (setq loadable
                                 (or (eq if-not-installed ':install)
			             (y-or-n-p "Do you want to proceed to load ~a now? "
                                               module)))))))
	     (cond (loadable
		    (let ((lr (call-installer-or-loader ':load module nil)))
		       (cond (lr
			      (set-read-env lr)
			      (note-loaded module)
			      t)
			     (t nil))))
		   (t nil))))
	 (t
	  (cond ((eq if-loaded ':warn)
		 (format *query-io*
			 "System ~a already loaded; to force re-load, ~% give yt-load keyword argument \":if-loaded ':force\"~%"
			 module)))
	  '(nil nil))))

(defun check-loaded (module)
   (member module all-loaded* :test #'equal))

(defun note-loaded (module)
	  (setq all-loaded*
		(adjoin module all-loaded* :test #'equal)))

(defun call-installer-or-loader (which module &optional start-over)
   (let ((str (format nil "~a-~a" which module)))
      (let ((funname (intern str :ytools)))
	 (let ((fun (symbol-function funname)))
	    ;;(format t "Calling ~s/~s/~s~%" str funname fun)
	    (cond ((eq which ':install)
		   (funcall fun start-over))
		  (t
		   (funcall fun)))))))

;;; Alist whose entries tell where module files live.  Default is
;;; same directory as ytload.lisp itself.
(defvar module-filenames* '())

;; The yt-config file must already be loaded.
(defun load-module-file (module)
   ;(load-yt-config-file)
   (setq module (string module))
   (setq module 
      (cond ((eq filename-case* ':upper)
	     (string-upcase module))
	    (t
	     (string-downcase module))))
   (let ((e (assoc module module-filenames* :test #'equal)))
      (let ((mod-file
               ;; Kludge --
	       (cond (e
		      (->pathname (cadr e)))
                      ;; -- '->pathname' is not defined yet; by the time
                      ;; it is needed, YTFM will be loaded and ->pathname
                      ;; will be defined.
		     (t
		      (merge-pathnames 
			 (make-pathname
			     :name module
			     :type (if (eq filename-case* ':upper)
                                       "LMD" "lmd"))
			 (pathname ytload-directory*))))))
	 (cond ((probe-file mod-file)
		(load mod-file))
	       ((and (not (check-loaded ':ytools))
		     (not (string= module 
				   (cond ((eq filename-case* ':upper)
					  "YTOOLS")
					 (t
					  "ytools"))))
		     (y-or-n-p "Can't find file for module ~a; load ytools and repeat search? "
			       module))
		
		(yt-load :ytools)
		(load-module-file module))
	       (t
;;;;		(format nil "Can't load ~s" module)
		(error "Can't find file for module ~s" module)
		))))
   module)

   ;;;;		   :directory `(:relative
   ;;;;				   ,(cond ((eq filename-case* ':upper)
   ;;;;					   "LOADER")
   ;;;;					  (t "loader")))

(defun set-read-env (l)
   (cond ((and l (consp l))
	  (cond ((car l) (setq *package* (car l))))
	  (cond ((cadr l) (setq *readtable* (cadr l))))
	  (labels ((bring-sym (s)
		      (let ((prev (find-symbol (symbol-name s)
					       *package*)))
			 (cond ((not prev)
				(import s *package*))))))
	     (bring-sym 'yt-install)
	     (bring-sym 'yt-load))))
   t)

; Each yt-load-XXX function returns either nil, meaning it failed to
; load (sometimes because it installed and recommends that Lisp be
; restarted), or a list (pk rt), where pk is the new package and 
; rt is the new read table.

(defvar num-config-vars-loaded* 0)

(defun load-yt-config-file ()
      (ensure-config-directory)
      (let ((filename (yt-config-file-name)))
	 (cond ((probe-file filename)
		(load filename))
	       ((y-or-n-p "No ~s file; ~% config-variables* so far = ~s ~% shall I create one in ~a ? "
			  config-file* config-variables* config-directory*)
		(dump-yt-config-file))
	       (t
		(error "No ~s file" config-file*)))
	 (cond ((ensure-filename-case)
		;; changed something
		(dump-yt-config-file)))
	 (config-check)))

;;;;(defun load-mcd-config-file () (load-yt-config-file))

(defun ensure-config-directory ()
   (loop 
      (cond ((and config-directory* (is-set directory-delimiter*))
	     (return)))
      (cond ((not config-directory*)
	     (query "Directory containing ~s (end with slash or other directory delimiter): "
		       config-file*)
	     (setq config-directory* (clear-read-line)))
	    (t
	     (format *query-io* "Assuming ~s is in ~a~%"
		       config-file* config-directory*)))
      (setq directory-delimiter*
	   (nth-value 1 (peel-last-non-whitespace
				     config-directory*)))
      (cond ((stringp directory-delimiter*)
	     (cond ((member (elt directory-delimiter* 0)
			    '(#\/ #\\ #\:))
		    (return))
		   (t
		    (query
			    "Is '~a' really the directory delimiter ~% (type blank line if so, else correct delimiter): "
			    directory-delimiter*)
		    (let ((delim (nth-value 1 (peel-last-non-whitespace
						 (read-line *query-io*)))))
		       (cond (delim
			      (setq directory-delimiter* delim)
			      (setq config-directory*
				    (concatenate 'string config-directory*
						 directory-delimiter*))
			      (return)))))))
	    (t (setq directory-delimiter* nil)))))
		    
(defun ensure-filename-case ()
	 (cond ((is-set filename-case*) nil)
	       (t
		(query "What is the usual case for file names on your system (upper or lower)? ")
		(loop
		   (let ((case (clear-read-line)))
		      (cond ((= (length case) 0)
			     (format *query-io* "Assuming lower case~%")
			     (setq filename-case* ':lower)
			     (return))
			    (t
			     (setq case
			       (let ((*package* (find-package ':keyword)))
				  (read-from-string case)))
			     (cond ((member case '(:upper :lower))
				    (setq filename-case* case)
				    (return)))))
		      (query "Type 'upper' or 'lower': ")))
		t)))

(defparameter space-bag* " 	")  ; <-- space & tab

(defun peel-last-non-whitespace (str)
   (setq str (string-right-trim space-bag* str))
   (let ((dlen (length str)))
      (cond ((> dlen 0)
	     (values (subseq str 0 (- dlen 1))
		     (subseq str (- dlen 1) dlen)))
	    (t
	     (values nil nil)))))

(defun better-restart-message ()
  (format *query-io* "It's probably better to quit Lisp before loading any module~%")
  t)

(defun prompt-for-dir-name (message)
   (loop
      (query message)
      (let ((dir (clear-read-line)))
         (cond ((< (length dir) (length directory-delimiter*))
		(format *query-io* "You must type a directory name~%"))
	       (t
		(return (dirname-with-delimiter dir)))))))

(defun dirname-with-delimiter (dir)
   (cond ((not (string-ends-with dir directory-delimiter*))
	  (strings-concat dir directory-delimiter*))
	 (t dir)))

(defun string-ends-with (s end)
   (let ((m (mismatch s end :from-end t)))
      (or (not m)
	  (= m (- (length s) (length end))))))

(defun dir-has-subdir (dir subdir)
   (let ((dir-pn (pathname dir))
	 (sub-pn (make-pathname :directory `(:relative ,subdir))))
      (probe-file (merge-pathnames sub-pn dir-pn))))

(defun dir-string-concat-sub (dir sub)
   (concatenate 'string
      dir
      (cond ((eq filename-case* ':upper)
	     (string-upcase sub))
	    (t sub))))

(defun dump-yt-config-file ()
   (cond ((not (null new-config-variables*))
	  (format t "Before dump, new-config-vars = ~s~%   config-vars = ~s~%"
		  new-config-variables* config-variables*)
	  (setq config-variables*
	        (union new-config-variables*
		       config-variables*
		       :test #'eq))
	  (setq new-config-variables* '())))
   (let ((*package* (find-package :ytools))
	 (*print-length* nil))
      (with-open-file (srm (yt-config-file-name)
		       :direction ':output
		       :if-exists ':supersede)
	 (format srm "(in-package :ytools)~%~%")
	 (dolist (v config-variables*)
	    (dump-var v srm)))
      (config-check)
      ))

;;; For backwards compatibility
;;;;(defun dump-mcd-config-file () (dump-yt-config-file))

(defun dump-var (vname srm)
   (cond ((boundp vname)
	  (format srm "(setq ~s '~s)~%"
		  vname (eval vname)))))

(defun read-line-trim ()
   (string-trim space-bag*
		(clear-read-line)))

(defun clear-read-line ()
   #+lispworks
   (clear-input)
   #+sbcl
   (finish-output *query-io*)
   (read-line *query-io*))

(defun yt-config-file-name ()
   (strings-concat config-directory* config-file*))

;;;;		   "ytconfig"
;;;;		   (cond ((boundp ytconfig-name*)
;;;;			  ytconfig-name*)
;;;;			 (t ""))
;;;;		   ".lisp"))

(defun strings-concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun source-subdirectory (dir)
  (strings-concat dir "src"))

(defvar lisp-standard-readtable* *readtable*)

;;;;(defun bin-subdirectory (dir)
;;;;  (strings-concat dir bin-bit* "bin"))

(defun cl-user::yt ()
   (in-package :ytools)
   (setq *readtable* lisp-standard-readtable*))

(defun ytools::see-yt ()
   (export '(cl-user::yt cl-user::yt-install cl-user::yt-load)
	   :cl-user))

(defvar config-paranoia* nil)

(defun config-check ()
   (cond (config-paranoia*
	  (let ((cpn (parse-namestring (yt-config-file-name))))
	     (let ((okcpn (merge-pathnames "ytconfig-ok" cpn)))
		(cond ((probe-file okcpn)
		       (let ((p (compare-files cpn okcpn)))
			  (cond ((numberp p)
				 (error "Current ~s differs from ytconfig-ok.lisp at position ~s"
					config-file* p)))))))))))

(defun compare-files (pn1 pn2)
   (with-open-file (srm1 pn1 :direction ':input)
      (with-open-file (srm2 pn2 :direction ':input)
	 (let (c1 c2 (i 0))
	    (loop
	       (setq c1 (read-char srm1 nil))
	       (setq c2 (read-char srm2 nil))
	       (cond (c1
		      (cond (c2
			     ;(format t "~a" c1)
			     (cond ((not (equal c1 c2))
				    ;(format t "~a" c2)
				    (return i))))
			    (t
			     (return i))))
		     (c2
		      (return i))
		     (t
		      (return t)))
	       (setq i (+ i 1)))))))

(defun query (&rest format-args)
   (apply #'format *query-io* format-args)
#+sbcl
   (finish-output *query-io*)
 )

(defmacro set-config-var (var even-if-set &rest body)
   `(cond ((or ,even-if-set (not (boundp ',var)))
	   (setq ,var (progn ,@body))
	   (dump-yt-config-file))))

;;; Returns t if fload wasn't aborted
(defmacro with-fload-abort (&rest body)
   `(let ((fl-finished nil))
       (catch 'fload-abort
	  ,@body
	  (setq fl-finished t))
       fl-finished))

(defun revnum-from-changelog (base-version direc)
   (let ((revnum (or (find-revision-num (strings-concat direc "CHANGELOG"))
                     "??")))
     (strings-concat base-version "." revnum)))

;;; Find string "$Id" in file, following by num.num.....num, and
;;; return the last num, or nil if none can be found
(defun find-revision-num (pn)
   (cond ((fboundp '->pathname)
          ;; Kludge
          (setq pn (->pathname pn))))
   (with-open-file (insrm pn :direction ':input)
      (labels ((advance-to-$Id ()
                  (loop
                     (let ((r (read-char insrm nil nil)))
                        (cond (r
                               (cond ((char= r #\$)
                                      (return t))))
                              (t
                               (return nil))))))

               (advance-to-revision-nums ()
                  (loop
                     (let ((r (read-char insrm nil nil)))
                        (cond (r
                               (cond ((char= r #\.)
                                      (return t))))
                              (t
                               (return nil))))))

               (get-last-num ()
                  (let ((numchars '()))
                     (loop
                        (let ((r (read-char insrm nil nil)))
                           (cond (r
                                  (cond ((char= r #\.)
                                         (setq numchars '()))
                                        ((digit-char-p r)
                                         (setq numchars (cons r numchars)))
                                        (t
                                         (return
                                            (cond ((null numchars)
                                                   nil)
                                                  (t
                                                   (coerce (reverse numchars)
                                                           'string)))))))
                                 (t
                                  (return nil))))))))
         (cond ((advance-to-$Id)
                (cond ((advance-to-revision-nums insrm)
                       (get-last-num insrm))
                      (t nil)))
               (t nil)))))

(defun version-num-from-changelog (direc &optional (prefix ""))
   (or (find-version-num (strings-concat direc prefix "CHANGELOG"))
       "??"))

(defvar version-header* "[- Version ")

(defun find-version-num (pn)
   (cond ((fboundp '->pathname)
          ;; Kludge
          (setq pn (->pathname pn))))
   (with-open-file (insrm pn :direction ':input)
      (labels ((find-version-in-line (line startpos)
;;;;                  (trace-around find-version-in-line
;;;;                     (:> "(find-version-in-line: " startpos 1 line ")")
                  (let ((q (position
                              #\Space line
                              :start startpos)))
                     (cond (q
                            (subseq line startpos q))
                           (t "??.??")))
;;;;                     (:< (val &rest _) "find-version-in-line: " val))
                  ))
         (loop
            (let ((r (read-line insrm nil nil)))
               (cond (r
                      (let ((p (search version-header*
                                       r)))
                        (cond (p
                               (return
                                  (find-version-in-line
                                     r (+ p (length version-header*))))))))
                     (t (return "??.??"))))))))



