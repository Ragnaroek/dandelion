;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: ytcopy.lisp,v 2.3 2006/07/24 11:43:00 airfoyle Exp $

;;; THIS IS NOT PORTABLE (yet?)  It depends on Allegro,
;;; because it uses excl:run-shell-command.

(eval-when (:compile-toplevel :load-toplevel :execute :slurp-toplevel)
   (export '(dirs-file-copy dir-pns-file-copy pathname-append-dir
             cvs-export create-tar-file tar-file-dir*
             dirs-file-copy dir-pns-file-copy directory-empty)))

(depends-on %module/ ytools)

(eval-when (:compile-toplevel :slurp-toplevel :execute :load-toplevel)
   (def-ytools-logical-pathname clocc-port
       "%clocc/src/port/"
       "../../bin/--/")

   (def-ytools-logical-pathname cvs "~/CVSified/")
   (def-ytools-logical-pathname wkg "~/workingdir/")
   (def-ytools-logical-pathname doc "~/BIG/OFFICIAL/doc/"

   (fload %clocc-port/ ext)))

(depends-on (:at :run-time) %clocc-port/ shell path)

(defvar really-copy-files* true)

;;; This file is not intended to be compiled, but it won't
;;; hurt to do so.

(eval-when (:execute load-toplevel :compile-toplevel)
)

(defvar extra-lmds* 
    '("ynisp" "lexiparse" "lisplang" "opt" "optop"
      "litlisp"))

(defvar extra-lmd-dir* "%cvs/prog/ytload/")

(defvar tar-file-dir* "%wkg/yale/")

(defparameter clocc-host*
              ":ext:airfoyle@cvs.sourceforge.net:/cvsroot/clocc/")

(defparameter clocc-levels* '("clocc" "src" "ytools"))

(defun ytools-build-for-export
                   (&key
                    (source-dir "%ytools/")
                    (doc-dir "%doc/ytools/")
                    (dest-dir
                       "%wkg/CVStemp/clocc/src/ytools/")
                    ;; main-source is
                    ;; either :cvs or :directory
                    (main-source ':cvs)
                    (extra-lmds extra-lmds*))
   (let ((expanded-dest-dir (->pathname dest-dir)))
      (ecase main-source
        (:cvs
         (err-out "Exporting from CVS to " expanded-dest-dir)
         (cvs-export expanded-dest-dir clocc-levels*
                     :host clocc-host*))
        (:directory
         (err-out "Copying from " source-dir " to " dest-dir)
         (ytools-copy source-dir dest-dir)))
      (err-out "Adding extra .lmd and documentation "
               "files to ytload subdirectory")
      (let ((dest-ytload-dir (pathname-append-dir dest-dir "ytload")))
         (extra-lmds-copy extra-lmds extra-lmd-dir* dest-ytload-dir))
      (let ((doc-pn (->pathname doc-dir))
            (dest-pn (->pathname dest-dir)))
         (dir-pns-file-copy doc-pn dest-pn "ytdoc.pdf")
         (err-out "Creating compressed YTools tar file")
         (let ((tar-dir-pn (create-tar-file dest-pn :compress true)))
            (!= (port:default-directory) tar-dir-pn)
            (err-out "Copying tar file to cyndra (takes a while)")
            (try-run-shell-command
               !"scp ytools.tar.gz ~
                    dvm@cyndra.cs.yale.edu:ftparea/software/ytools.tar.gz")
            (!= (port:default-directory) doc-pn)
            (err-out "Copying YTools manual to cyndra")
            (try-run-shell-command
               "scp ytdoc.pdf dvm@cyndra.cs.yale.edu:webpage/papers/ytdoc.pdf")))
      (err-out 
           "Don't forget to edit webpage and Releases list" :%
           "to reflect latest YTools version " (:a +ytools-version+) :%)
      '***))

(defvar cvs-export-delete-ask* true)

;;; 'dest-dir' is a pathname for the directory where the files are
;;; to wind up.  To check them out, we have to ascend "levels".  
(defun cvs-export (dest-dir expected-lower-dirs &key host)
   (!= dest-dir (->pathname *-*))
   (ensure-directories-exist dest-dir)
   (let* ((dest-dir-len (len (Pathname-directory
                                             dest-dir)))
          (expected-dir-len (len expected-lower-dirs))
          (levels
            (cond ((is-Number expected-lower-dirs)
                   expected-lower-dirs)
                  ((equal (nthcdr (- dest-dir-len
                                     expected-dir-len)
                                  (Pathname-directory dest-dir))
                          expected-lower-dirs)
                   expected-dir-len)
                  (t
                   (signal-problem cvs-export
                      "CVS export discrepancy:"
                      :% "   Expected directory matching " expected-lower-dirs
                      :% "   Got: " dest-dir
                      (:prompt-for "Number of directory levels to"
                                   " pop up to to reach level to"
                                   " check out at"))))))
      ;; CVS doesn't like it if there's old crap in the way
      (directory-empty dest-dir :ask cvs-export-delete-ask*)
      (let ((direc (Pathname-directory dest-dir)))
         (let ((check-out (take (- levels) direc))
               (where
                  (make-Pathname
                     :host (Pathname-host dest-dir)
                     :device (Pathname-device dest-dir)
                     :directory (drop (- levels) direc))))
            (!= (port:default-directory)
                where)
            (multi-let (((_ _ _ date month year _ _ _)
                         (get-decoded-time)))
               ;; Use the usual trick of exporting everything
               ;; up to tomorrow's date; i.e., everything --
               (err-out "Just before export, directory")
;;;;               (excl.osi:with-command-io
;;;;                   ("pwd")
;;;;                 (:output (wd) (err-out " [" (:a wd) "]")))
               (err-out " looks like:" :%)
               (excl.osi:with-command-io
                        ("ls" :whole true :directory dest-dir
                              :show-window ':hide)
                  (:output (listing)
                     (err-out (:a listing))))
               (err-out :% "**********" :%)
               (try-run-shell-command
                  (out (:to :string)
                     "cvs "
                     (:q (host
                          "-d " (:a host) 1))
                     "export -D " year "-" month "-" (+ date 1)
                                 1 (:a (<< dir-strings-concat
                                           check-out)))))))))

;;; 'dir' is a pathname at this point
;;; 'skip' is a list of things not to try to
;;; delete.  (I say "things" because they're not
;;; usually "real" files; more like subdirectories,
;;; links, "..", and the like.)
(defun directory-empty (dir &key (ask true) (skip !()))
   (!= (port:default-directory)
       dir)
   (!= skip (union *-* '("." "..") :test #'equal))
   (let (;;;;(ls-output false)
         (files !()))
      (excl.osi:with-command-io
               ("ls -a" :show-window ':hide ; Windows lossage
                        :directory dir
;;;;                        :whole true
                )
        (:output (listing)
           (cond ((not (member listing skip
                               :test #'string=))
;;;;           (!= ls-output listing)
                  (on-list listing files)))))
      (cond ((not (null files))
             (cond ((cond (ask
                           (out (:to *query-io*)
                              "About to delete all the files in directory "
                              dir ", to wit: " :%
                              files
;;;;                              (:a ls-output)
                              :%)
                           (read-y-or-n :yes-no "Shall I proceed? "))
                          (t true))
                    (repeat :for ((file :in files))
                       (delete-file (merge-pathnames file dir))
;;;;                       (excl:run-shell-command
;;;;                          (out (:to :string) "rm " (:a file))
;;;;                          :show-window ':hide)
                     )))))))

(defun directory-list (dir)
   (!= (port:default-directory)
       dir)
   (let (;;;;(ls-output false)
         (files !()))
      (excl.osi:with-command-io
               ("ls -a" :show-window ':hide ; Windows lossage
                        :directory dir)
        (:output (listing)
           (cond ((not (member listing '("." "..")
                               :test #'string=))
                  (on-list listing files)))))
      files))

(defun create-tar-file (dest-pn &key (compress true))
   (let ((tar-dir-pn (->pathname tar-file-dir*))
         (dest-dir (Pathname-directory dest-pn)))
      (cond ((=< (len dest-dir) 2)
             (signal-problem create-tar-file
                "Can't find good place to run 'tar' command: "
                dest-dir
                (:proceed "I'll put it in a bad place"))))
      (let ((dir-name (quote-if-contains-spaces (lastelt dest-dir)))
            (tar-exec-dir
               (make-Pathname
                  :host (Pathname-host dest-pn)
                  :device (Pathname-device dest-pn)
                  :directory (drop -1 dest-dir))))
         (!= (port:default-directory)
             tar-exec-dir)
         (ensure-directories-exist tar-dir-pn)
         ;; We can't just create the tar file where we want it
         ;; because in Windows we're calling a cygwin product,
         ;; and it gets dizzy when given a Windows destination
         ;; anywhere besides ... here.
         (out "Working directory = " (port:default-directory) :%)
         (let* ((tar-file-name (out (:to :string) (:a dir-name) ".tar"))
                (full-tar-file-pn (merge-pathnames tar-file-name tar-exec-dir)))
            ;; Clear the decks --
            (file-delete-if-exists full-tar-file-pn)
            (try-run-shell-command
               (out (:to :string)
                  "tar -cf " (:a tar-file-name)
                       1 (:a dir-name)))
            ;; Apparently 'tar' runs asynchronously
            ;; even though we said ':wait true'.
            (repeat :for ((i = 1 :to 10))
             :result (signal-problem create-tar-file
                        "Tar file " tar-file-name " never got created")
             :until (probe-file full-tar-file-pn)
                (sleep 1.0))
            (try-run-shell-command
               (out (:to :string)
                  "mv " (:a tar-file-name)
                  1 (:a (namestring (pathname-quote-spaces tar-dir-pn)))))
            (cond ((probe-file full-tar-file-pn)
                   (signal-problem create-tar-file
                      "tar file did not get moved")))
            (!= (port:default-directory)
                tar-dir-pn)
            (cond (compress
                   (file-delete-if-exists
                       (merge-pathnames
                          (make-Pathname
                             :name tar-file-name
                             :type "gz")
                          tar-dir-pn))
;;; This works, but 'rm' insists on returning 1 if the file doesn't exist
;;;;                   (try-run-shell-command
;;;;                      (out (:to :string)
;;;;                         "rm " (:a tar-file-name) ".gz"))
                   (try-run-shell-command
                      (out (:to :string)
                         "gzip " (:a tar-file-name))))))
         tar-dir-pn)))

;;;;      (let ((script-pn
;;;;               (make-Pathname
;;;;                  :host (Pathname-host script-pn)
;;;;                  :device (Pathname-device script-pn)
;;;;                  :directory (drop -1 dest-dir))))
;;;;         (with-open-file (sh-srm (merge-pathnames
;;;;                                    "make-yt-tar.sh"
;;;;                                    script-pn)
;;;;                              :direction ':output
;;;;                              :if-exists ':supersede)
;;;;            (out (:to sh-srm)
;;;;               "tar -cf " (:a (namestring tar-file-pn)) "ytools.tar" :%
;;;;               "cd " (:a (namestring tar-file-pn))
;;;;               "rm ytools.tar.gz" :%
;;;;               "gzip ytools.tar" :%))
;;;;         (!= (port:default-directory) tar-file-pn)
;;;;         (port:run-prog "chmod" :args '("u+x" "make-yt-tar.sh"))
;;;;         (excl:run-shell-command "./make-yt-tar.sh")))

(defun ytload-copy (source-dir target-dir)
   (let ((source-pn (->pathname source-dir))
	 (target-pn (->pathname target-dir)))
      (ensure-directories-exist target-pn)
      (repeat :for ((fname :in '("ytfm.lmd" "ytools.lmd"
                                 "ytload.lisp" "raw-ytfm-load.lisp")))
	 (dirs-file-copy source-pn target-pn fname "lmd"))))

(defun extra-lmds-copy (extra-lmds source-dir target-dir)
   (let ((source-pn (->pathname source-dir))
	 (target-pn (->pathname target-dir)))
      (ensure-directories-exist target-pn)
      (repeat :for ((fname :in extra-lmds))
	 (dirs-file-copy source-pn target-pn fname "lmd"))))

(defun ytools-copy (source-dir target-dir)
   (let* ((source-pn (->pathname source-dir))
	  (target-pn (->pathname target-dir))
          (target-ytload-pn (pathname-append-dir target-pn "ytload")))
      (ensure-directories-exist target-ytload-pn)
      (directory-empty target-ytload-pn :ask cvs-export-delete-ask*)
      (directory-empty target-pn
                       :ask cvs-export-delete-ask*
                       :skip '("ytload"))
      ;; Deletion may have deleted ytload directory, so redo this --
      (ensure-directories-exist target-ytload-pn)
      (repeat :for ((fname :in `(,@ytools-core-files*
				 "binders" "bq"
                                 "debug" "filedeps" "fileseg"
                                 "mapper" "misc" "multilet" "nilscompat"
				 "object" "outin" "repeat" "repl"
                                 "setter" "signal" "tracearound"
                                 "ytcopy.lisp"
				 "ytools.lsy" "ytools.system")))
	 (dirs-file-copy source-pn target-pn fname "lisp"))
      (dir-pns-file-copy source-pn target-pn "CHANGELOG")
      (ytload-copy (pathname-append-dir source-dir "ytload")
                   target-ytload-pn)))

(defun dirs-file-copy (source-dir target-dir fname type)
   (let ((source-dpn (merge-pathnames
			(make-pathname :type type)
			source-dir))
	 (target-dpn (merge-pathnames
			(make-pathname :type type)
			target-dir)))
      (dir-pns-file-copy source-dpn target-dpn fname)))
	   
(defun dir-pns-file-copy (source-dpn target-dpn fname
                          &key (really really-copy-files*))
   (let ((file-pn (parse-namestring fname)))
      (let ((from-pn (merge-pathnames file-pn source-dpn))
	    (to-pn (merge-pathnames file-pn target-dpn)))
	 (cond (really
		(sys::copy-file from-pn to-pn :overwrite true))
	       (t
		(out "Copy: " from-pn
		     :% " => " to-pn :%))))))

(defun run-shell-script (dir script)
   (port:run-prog (out (:to :string)
                     "exec " (:a (namestring dir)) (:a script))))
                      
                      
;;; The default values for 'wait' and 'show-window'
;;; are                    true   and :hide        respectively.
(defun try-run-shell-command
      (command &key input output error-output separate-streams
                    (wait nil wait-supplied)
                    if-input-does-not-exist
                    if-output-exists if-error-output-exists show-window
                    environment directory
                    uid gid effective initgroups-user)
   (let ((result 
            (excl:run-shell-command command
               :input input
               :output output
               :error-output error-output
               :separate-streams separate-streams
               :wait (cond (wait-supplied wait)
                           (t true))
               :if-input-does-not-exist if-input-does-not-exist
               :if-output-exists if-output-exists
               :if-error-output-exists if-error-output-exists
               :show-window (or show-window ':hide) ; -- please...
               :environment environment
               :directory directory
               :uid uid
               :gid gid
               :effective effective
               :initgroups-user initgroups-user)))
      (cond ((and (is-Number result)
                  (not (eql result 0)))
             (signal-problem try-run-shell-command
                "Command resulted in return value " result ":"
                :% " [" (:a command) "]"
                (:proceed "Come on ... what does the shell know?"))))
      result))

(defun file-delete-if-exists (pn)
   (cond ((probe-file pn)
          (delete-file pn))))

;;; 'pn' is a directory-only pathname (or might as well be).
;;; Returns a new directory-only pathname that is a copy of 'pn'
;;; with 'dir' appended to the directory field.  If 'dir' is not a
;;; list of strings, it should be a single string, which is treated as
;;; if it were a singleton list.
(defun pathname-append-dir (pn dir)
   (!= pn (->pathname *-*))
   (cond ((is-String dir)
          (!= dir (list *-*))))
   (make-Pathname
      :host (Pathname-host pn)
      :device (Pathname-device pn)
      :directory (append (Pathname-directory pn) dir)))

(defun pathname-quote-spaces (pn)
   (let-fun ()
      (make-Pathname
         :host (Pathname-host pn)
         :device (Pathname-device pn)
         :directory (cons (first (Pathname-directory pn))
                          (<# quote-if-contains-spaces
                              (rest (Pathname-directory pn))))
         :name (quote-if-contains-spaces (Pathname-name pn))
         :type (Pathname-type pn))))

(defun quote-if-contains-spaces (string)
   (cond ((find #\Space string)
          (out (:to :string)
             "\"" (:a string) "\""))
         (t string)))

;;;;(defun os-is-evil ()
;;;;   (or (search "microsoft" (string-downcase (software-type)))
;;;;       (memq ':mswindows *features*)))
       