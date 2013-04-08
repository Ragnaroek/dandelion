;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;$Id: filedeps.lisp,v 2.3 2006/08/19 14:54:14 airfoyle Exp $

;;; This stuff is for use with CVSified/prog/misc/filedag.lisp.
;;; See 'module-deps-display' in that file.

(depends-on %module/ ytools)

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(files-scan-for-deps module-deps)))

(defun module-deps (module support-kinds directories special-test)
   (cond ((is-Symbol module)
	  (!= module (lookup-YT-module *-*))))
   (cond ((not (is-YT-module module))
	  (signal-problem module-deps
	     "Illegal module " module)))

   (files-scan-for-deps
      (module-pathnames module support-kinds)
      (code-file-chunk-in-directories directories)
      special-test))

;;;;      (repeat :for ((c :in (YT-module-contents module)))
;;;;       :when (or (null support-kinds)
;;;;		 (not (null (intersection (first c)
;;;;					  support-kinds))))
;;;;       :nconc (repeat :for ((e :in (rest c)))
;;;;	       :when (car-eq e 'module-elements)
;;;;	       :nconc (multi-let (((files _ _)
;;;;				   (flags-separate
;;;;				         (cdr e) filespecs-load-flags*)))
;;;;			 (filespecs->pathnames files))))

(defun module-pathnames (module support-kinds)
      (repeat :for ((c :in (YT-module-contents module)))
       :when (or (null support-kinds)
		 (not (null (intersection (first c)
					  support-kinds))))
       :nconc (repeat :for ((e :in (rest c)))
	       :when (car-eq e 'module-elements)
	       :nconc (multi-let (((files _ _)
				   (flags-separate
				         (cdr e) filespecs-load-flags*)))
			 (filespecs->pathnames files)))))

(defun code-file-chunk-in-directories (directories)
   (!= directories
       (<# Pathname-directory (->pathname-list *-*)))
   (\\ (code-file-ch)
;;;;      (trace-around chunk-in-dir
;;;;	 (:> "(chunk-in-dir: " code-file-ch :% 1 directories ")")
      (member (Pathname-directory
		 (Code-file-chunk-pathname code-file-ch))
	      directories
	      :test #'equal)
;;;;	 (:< (val &rest _) "chunk-in-dir: " val))
      ))

(defun files-scan-for-deps (filenames filter-pred special-test)
   (!= filenames (filespecs->pathnames *-*))
   (let ((file-chunks (<# (\\ (fn) (pathname-denotation-chunk fn true))
			  filenames)))
      (repeat :for ((f-ch :in file-chunks))
	 (monitor-filoid-basis (place-Loaded-chunk f-ch false)))
      ;; Now all the dependency information should be up to date.
      (let ((checked !()))
	 (let-fun ((:def list-walk-and-collect (file-chunks have-special-anc)
;;;;		      (trace-around list-walk
;;;;			 (:> "(list-walk: " file-chunks ")")
		      (<! (\\ (ch)
			     (walk-and-collect ch have-special-anc))
			  file-chunks)
;;;;			 (:< (val &rest _) "list-walk: " val))
		      )
		   (:def walk-and-collect (code-file-ch has-special-anc)
		    ;; Returns a list in the format expected by
		    ;; 'file-deps-display' (see dagprint.lisp).
;;;;		       (trace-around walk-deps
;;;;			  (:> "(walk-deps: " code-file-ch ")")
		       (cond ((or (memq code-file-ch checked)
				  (not (funcall filter-pred code-file-ch)))
			      !())
			     (t
			      (on-list code-file-ch checked)
;;;;			      (cond ((string=
;;;;				        (Pathname-name
;;;;					   (Code-file-chunk-pathname
;;;;					      code-file-ch))
;;;;					"signal")
;;;;				     (!= ch1* code-file-ch)))
			      (let* ((run-time-deps
					(Code-chunk-callees code-file-ch))
				     (is-special
				        (or has-special-anc
					    (and special-test
						 (funcall special-test
							  code-file-ch))))
				     (dep-names
				        (<# file-ch-name
					    run-time-deps)))
				 (cons `(,(file-ch-name code-file-ch)
					 ,@(include-if is-special '*)
					 :<- ,@dep-names)
				       (list-walk-and-collect
					  run-time-deps is-special)))))
;;;;			  (:< (val &rest _) "walk-deps: " val))
		       ))
	    (strings->syms (list-walk-and-collect file-chunks false))
	  :where
	    (:def file-ch-name (file-ch)
	       (Pathname-name (Code-file-chunk-pathname file-ch)))))))

(defun pathnames-directories (pnl)
   (repeat :for ((pn :in pnl)
                 (directories !()))
    :result directories
      (let ((dir-pn (make-Pathname
                       :host (Pathname-host pn)
                       :device (Pathname-device pn)
                       :directory (Pathname-directory pn)
                       :name false
                       :type false)))
         (on-list-if-new dir-pn directories :test #'equal))))

(defun ->pathname-list (x)
   (cond ((atom x)
	  (!= x (list x))))
   (<# (\\ (fnm)
	  (->pathname
	     (cond ((or (is-Pathname fnm) (is-String fnm))
		    fnm)
		   ((is-Symbol fnm)
		    (symbol-name-as-file-name (Symbol-name fnm)))
		   (t (signal-problem ->pathname-list
			 "Undecipherable as pathname: " fnm)))))
       x))

;;;;   (cond ((exists (fn :in x) (is-Pathname fn))
;;;;	  (<# ->pathname x))
;;;;	 ((forall (fn :in x)
;;;;	     (or (is-Pathname fn)
;;;;		 (string-is-ytools-logical-pathname fn)))
;;;;	  (<# ->pathname x))
;;;;	 (t
;;;;	  (filespecs->pathnames x)))


;;; More general than we need --
(defun strings->syms (e)
   (cond ((is-String e) (intern e))
	 ((atom e) e)
	 (t (<# strings->syms e))))