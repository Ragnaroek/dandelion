;;; CVS (Concurrent Versioning System) interface
;;; http://www.cyclic.com
;;; http://www.sourcegear.com/CVS
;;;
;;; Copyright (C) 1996 by Bruno Haible
;;; Copyright (C) 1998-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: cvs.lisp,v 2.27 2005/12/12 16:33:17 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/cvs.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `string-beg-with', `substitute-subseq'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; `skip-to-line', `read-list-from-stream', `file-size-t'
  (require :cllib-fileio (translate-logical-pathname "cllib:fileio"))
  ;; `string->dttm', `dttm->string'
  (require :cllib-date (translate-logical-pathname "cllib:date"))
  ;; `hash-table->alist'
  (require :cllib-miscprint (translate-logical-pathname "cllib:miscprint"))
  ;; `top-bottom-ui'
  (require :cllib-sorted (translate-logical-pathname "cllib:sorted"))
  ;; `default-directory', `pathname-ensure-name', `probe-directory'
  (require :port-sys (translate-logical-pathname "port:sys"))
  ;; `with-open-pipe'
  (require :port-shell (translate-logical-pathname "port:shell")))

(in-package :cllib)

(export '(cvs-diff2patch cvs-stat-log cvs-change-root read-all-changelogs))

;;;
;;; CVS diff ---> patch
;;;
;; Bruno Haible originally wrote this in 1996 as a filter.
;; Sam Steingold fixed some bugs in 1998 and
;; converted to the generic function in 2000.

(eval-when (compile load eval) (fmakunbound 'cvs-diff2patch))
;;;###autoload
(defgeneric cvs-diff2patch (in out)
  (:documentation "Convert a CVS diff to a patchable diff.")
  (:method ((in stream) (out stream))
    (do* ((line (read-line in nil nil) (read-line in nil nil))
          (len (length line) (length line)) path base)
         ((null line))
      (declare (simple-string line) (type index-t len))
      (cond ((or (string-beg-with "? " line len) ; skip
                 (string-beg-with "==" line len)
                 (string-beg-with "RCS" line len)
                 (string-beg-with "diff" line len)
                 (string-beg-with "retrieving" line len)))
            ((string-beg-with "Index: " line len)
             (setq path (subseq line 7)
                   base (subseq path (1+ (or (position #\/ path :from-end t)
                                             -1)))))
            ((and (>= len (+ 4 (length base))) (string= line "*** " :end1 4)
                  (or (string= line base :start1 4 :end1 (+ 4 (length base)))
                      (string= line "/tmp/" :start1 4 :end1 9)))
             (format out "*** ~a~a~%" path
                     (subseq line (position #\Tab line))))
            ((and (>= len (+ 4 (length base)))
                  (string= line "--- " :end1 4)
                  (or (string= line base :start1 4 :end1 (+ 4 (length base)))
                      (string= line "/tmp/" :start1 4 :end1 9)))
             (format out "--- ~a~a~%" path
                     (subseq line (position #\Tab line))))
            ((write-line line out)))))
  (:method ((in string) (out t)) (cvs-diff2patch (pathname in) out))
  (:method ((in t) (out string)) (cvs-diff2patch in (pathname out)))
  (:method ((in pathname) (out t))
    (with-open-file (istream in :direction :input)
      (format t "~&~s: reading: ~s [~:d byte~:p]~%"
              'cvs-diff2patch in (file-length istream))
      (cvs-diff2patch istream out)))
  (:method ((in t) (out pathname))
    (with-open-file (ostream out :direction :output)
      (format t "~&~s: writing: ~s~%" 'cvs-diff2patch out)
      (cvs-diff2patch in ostream))))

;;;
;;; CVS log stat
;;;

(defstruct (revision)
  (rev "" :type string)
  (time 0 :type integer)
  (author "" :type string)
  (state "" :type string)
  (lines+ 0 :type index-t)
  (lines- 0 :type index-t)
  (log nil :type list))         ; of strings

(defstruct (cvs-file (:conc-name cvsf-))
  (rcs "" :type string)
  (work "" :type string)
  (head "" :type string)
  (size 0 :type file-size-t)
  (tot-rev 0 :type index-t)
  (revs nil :type list))        ; of revisions

(defparameter *cvs-log-sep-1* (make-string 28 :initial-element #\-))
(defparameter *cvs-log-sep-2* (make-string 77 :initial-element #\=))

(defun cvs-read-change (in ra)
  "Read a CHANGE from a stream.
Suitable for `read-list-from-stream'."
  (declare (stream in) (symbol ra))
  (unless (eq ra :revision)
    (error "~s: read-ahead is `~s' (`~s' expected)"
           'cvs-read-change ra :revision))
  (flet ((extract (line label)
           (let ((pos (search label line :test #'char=)))
             (when pos
               (subseq line (+ (length label) pos)
                       (position #\; line :start pos :test #'char=))))))
    (let* ((rev (read-line in)) (id (read-line in)) fin
           (lines (extract id "lines: "))
           (lines+- (and lines (string-tokens lines))))
      (values
       (make-revision :log (loop :for line = (read-line in)
                                 :do (setq fin (string= line *cvs-log-sep-2*))
                                 :until (or fin (string= line *cvs-log-sep-1*))
                                 :collect line)
                      :rev rev
                      :time (string->dttm (extract id "date: "))
                      :author (extract id "author: ")
                      :state (extract id "state: ")
                      :lines+ (or (car lines+-) 0)
                      :lines-  (abs (or (cadr lines+-) 0)))
       (if fin +eof+ (read in))))))

(defun cvs-read-file (in ra)
  "Read a CVS-FILE from a stream.
Suitable for `read-list-from-stream'."
  (declare (stream in) (symbol ra))
  (loop :while (eq ra :cvs) :do (read-line in) (setq ra (read in)))
  (unless (eq ra :rcs)
    (error "~s: read-ahead is `~s' (`~s' or `~s' expected)"
           'cvs-read-file ra :rcs :cvs))
  (flet ((from-colon (line)
           (subseq line (+ 2 (position #\: line :test #'char=)))))
    (let* ((rcs (from-colon (read-line in)))
           (work (from-colon (read-line in)))
           (head (from-colon (read-line in)))
           (tot-rev-l (skip-to-line in "total revisions:" nil))
           (p0 (position #\; tot-rev-l :test #'char=))
           (p1 (position #\: tot-rev-l :test #'char= :from-end t))
           (tot-rev (parse-integer tot-rev-l :end p0))
           (sel-rev (parse-integer tot-rev-l :start (1+ p1)))
           (revs (progn (skip-to-line in *cvs-log-sep-1* nil)
                        (read-list-from-stream in #'cvs-read-change)))
           (path (merge-pathnames (pathname-ensure-name work)
                                  (directory-namestring in))))
      (unless (= tot-rev sel-rev)
        (warn "total revision (~d) != selected revisions (~d)"
              tot-rev sel-rev))
      (unless (= tot-rev (length revs))
        (warn "total revision (~d) != number of revisions (~d)"
              tot-rev (length revs)))
      (values (make-cvs-file
               :revs revs :work work :head head :tot-rev tot-rev :rcs rcs
               :size (if (probe-file path) (file-size path) 0))
              (read in nil +eof+)))))

(defun cvs-read-log (path)
  "Read CVS log, return a list of CVS-FILE structures."
  (with-timing (:done t)
    (with-open-file (in path :direction :input)
      (format t "~&~s: ~a [~:d bytes]..." 'cvs-read-log path (file-length in))
      (force-output)
      (loop :while (char= #\? (peek-char nil in)) :do (read-line in))
      (read-list-from-stream in #'cvs-read-file))))

(defsubst rev-lines (rr)
  (declare (type revision rr))
  (+ (revision-lines+ rr) (revision-lines- rr)))

(defsubst cvsf-lines (ff)
  (declare (type cvs-file ff))
  (reduce #'+ (cvsf-revs ff) :key #'rev-lines))

(defsubst cvsf-list-size (fl)
  (declare (type list fl))
  (reduce #'+ fl :key #'cvsf-size))

(defsubst cvsf-dead-p (ff)
  (declare (type cvs-file ff))
  (string= "dead" (revision-state (car (cvsf-revs ff)))))

;;;
;;; stat by the author
;;;

(defstruct (author)
  (name "" :type string)
  (owns nil :type list)         ; list of files owned
  (mods nil :type list)         ; list of files modified
  (revs nil :type list))        ; list of revisions

(defun author-lines (au)
  (reduce #'+ (author-revs au) :key #'rev-lines))

(defmethod print-object ((au author) (out stream))
  (if *print-readably* (call-next-method)
      (format out "[~a: owns: ~:d mods: ~:d revs: ~:d lines: ~:d]"
              (author-name au) (length (author-owns au))
              (length (author-mods au)) (length (author-revs au))
              (author-lines au))))

(defun cvs-stat-files (fl)
  "Generate and print some statistics about a list of CVS-FILE structs."
  (let ((aht (make-hash-table :test 'equal)) aul)
    (format
     t "~:d file~:p (~:d byte~:p), ~:d revision~:p, ~:d line~:p changed~%"
     (length fl) (cvsf-list-size fl)
     (reduce #'+ fl :key #'cvsf-tot-rev) (reduce #'+ fl :key #'cvsf-lines))
    (dolist (ff fl)
      (do ((rr (cvsf-revs ff) (cdr rr)) au re na)
          ((null rr))
        (setq re (car rr) na (revision-author re)
              au (or (gethash na aht)
                     (setf (gethash na aht) (make-author :name na))))
        (push re (author-revs au))
        (pushnew ff (author-mods au) :key #'cvsf-rcs)
        (when (null (cdr rr)) (push ff (author-owns au)))))
    (setq aul (sort (mapcar #'cdr (cdr (hash-table->alist aht))) #'<
                    :key (compose length author-revs)))
    (format t "name~10t ~21@a ~10@a ~10@a ~15@a~%"
            "owns: files     bytes" "modified" "revisions" "lines changed")
    (dolist (au aul)
      (format t "~a~10t ~5:d ~15:d ~10:d ~10:d ~15:d~%"
              (author-name au) (length (author-owns au))
              (cvsf-list-size (author-owns au)) (length (author-mods au))
              (length (author-revs au)) (author-lines au)))
    (format t "total~10t ~5:d ~15:d ~10:d ~10:d ~15:d~%"
            (reduce #'+ aul :key (compose length author-owns))
            (reduce #'+ aul :key (compose cvsf-list-size author-owns))
            (reduce #'+ aul :key (compose length author-mods))
            (reduce #'+ aul :key (compose length author-revs))
            (reduce #'+ aul :key #'author-lines))
    (values aul aht)))

;;;###autoload
(defun cvs-stat-log (path)
  "Generate and print some statistics of the CVS repository.
Careful - this will return a huge list!"
  (when (probe-directory path)
    (with-timing ()
      (format t "~&~s: ~a is a directory, running `cvs log`..."
              'cvs-stat-log path)
      (force-output)
      (let ((old-path (default-directory)))
        (setf (default-directory) path)
        (setq path (merge-pathnames "cvs.log" path))
        (unwind-protect
            (with-open-pipe (pipe (pipe-input "cvs" "-q" "log"))
              (with-open-file (log path :direction :output
                                   :if-exists :supersede)
                (loop :for line = (read-line pipe nil nil)
                      :while line :do (write-line line log))
                (format t "done [~:d byte~:p]" (file-length log))))
          (setf (default-directory) old-path)))))
  (format t "~a: " path)
  (let ((logl (cvs-read-log path)))
    (cvs-stat-files logl)
    (format t "~2& -- by the number of revisions --~%")
    (top-bottom-ui logl 10 nil nil :key #'cvsf-tot-rev :label #'cvsf-work)
    (format t "~2& -- the most recently modified --~%")
    (top-bottom-ui logl 10 nil nil :key (compose revision-time car cvsf-revs)
                   :label #'cvsf-work :klabel #'dttm->string)
    logl))

;;;
;;; change the repository of the current sandbox
;;;

;;;###autoload
(defun cvs-change-root (root substitutions &key (dry-run nil) (log t))
  "Change Root and Repository files in the CVS directories under ROOT.
When `DRY-RUN' is non-NIL, no actual changes are done.
This is a Lisp version of
  find -path '.*/CVS/Root' -print0 | xargs -0 perl -i -p -e 's/from/to/'
with some trivial bells and whistles."
  (flet ((change-one-line (file substitutions)
           (let ((line (with-open-file (in file) (read-line in))))
             (mesg :log log "~s:~%" file)
             (dolist (subst substitutions)
               (mesg :log log " - ~s:~%" line)
               (setq line (substitute-subseq line (car subst) (cdr subst))))
             (mesg :log log " * ~s~%" line)
             (unless dry-run
               (with-open-file (out file :direction :output
                                    #+clisp :external-format #+clisp :unix
                                    :if-exists :supersede)
                 (write-line line out))))))
    (let ((root-cvs (mk-path root :directory '(:relative "CVS")))
          (root-subdirs (mk-path root :directory '(:relative "*"))))
      (when (probe-directory root-cvs)
        (change-one-line (merge-pathnames "Repository" root-cvs) substitutions)
        (change-one-line (merge-pathnames "Root" root-cvs) substitutions))
      ;; CMUCL's `directory' is buggy - won't work!
      (dolist (dir (directory root-subdirs))
        (unless (string-equal "CVS" (car (last (pathname-directory dir))))
          (cvs-change-root dir substitutions :dry-run dry-run :log log))))))

;;;
;;; stat ChangeLog files
;;;
;; e.g:
;; (read-all-changelogs "/usr/local/src/clisp/current/")
;; (sort *known-developers* #'> :key #'developer-hit)

(defstruct (developer)
  (nick "" :type string)
  (hit 1 :type integer)
  (names nil :type list)
  (emails nil :type list))

(defconst +bad-developer+ developer (make-developer)
  "*The convenient constant for init.")

(defcustom *known-developers* list nil
  "The list of all known developers so far.")

(defun find-dev (id)
  "Find the ID in `*known-developers*'."
  (find id *known-developers* :test
        (lambda (id dev)
          (or (string-equal id (developer-nick dev))
              (find id (developer-names dev) :test
                    (lambda (x y) (search x y :test #'char-equal)))
              (find id (developer-emails dev) :test
                    (lambda (x y) (search x y :test #'char-equal
                                          :end2 (position #\@ y))))))))

(defstruct (changelog-entry (:conc-name chlen-))
  (date +bad-date+ :type date)
  (id "" :type string)
  (name "" :type string)
  (email "" :type string)
  (developer +bad-developer+ :type developer))

(defun string->chlen (str)
  "Parse a string like \"2002-05-29  John D. Doe  <jdd@foo.org>\"
from a ChangeLog file into
  #S(changelog-entry :date 2002-05-29 :id \"jdd\" :name \"John D. Doe\"
     :email \"<jdd@foo.org>\")"
  (let* ((e-start (1+ (position #\space str :from-end t)))
         (n-end (position #\space str :from-end t :test #'char/= :end e-start))
         (sep1 (search "  " str :from-end t :end2 n-end))
         (n-start (if sep1 (+ 2 sep1) (1+ (position #\space str))))
         (d-end (position #\space str :from-end t :test #'char/= :end n-start))
         (date (time2date (string->dttm (subseq str 0 (1+ d-end)))))
         (email (string-trim "(<>)" (subseq str e-start)))
         (id (subseq email (if (find (char email 0) "<(") 1 0)
                     (position #\@ email)))
         (name (subseq str n-start (1+ n-end)))
         (lname-pos (position #\space name :from-end t))
         (lname (if lname-pos (subseq name (1+ lname-pos)) name))
         (developer (or (find-dev id) (find-dev lname))))
    (cond (developer
           (incf (developer-hit developer))
           (pushnew name (developer-names developer) :test #'string=)
           (pushnew email (developer-emails developer) :test #'string=)
           (unless (string-equal id (developer-nick developer))
             (pushnew id (developer-names developer) :test #'string=)))
          ((setq developer (make-developer :nick id :names (list name)
                                           :emails (list email)))
           (push developer *known-developers*)))
    (make-changelog-entry :date date :id id :name name :email email
                          :developer developer)))

(defun read-chlen (in ra)
  "Read a single `changelog-entry'.
Suitable for `read-list-from-stream'."
  (declare (stream in) (string ra))
  (loop :for str = (read-line in nil +eof+)
    :while (and (stringp str)
                (or (zerop (length str)) (whitespace-char-p (char str 0))))
    :finally (return (values (string->chlen ra)
                             (if (and (stringp str)
                                      (or (search
                                           ;; avoid Emacs recognizing this
                                           #.(concatenate 'string "Local"
                                                          " Variables:")
                                           str)
                                          (search "See ChangeLog" str)))
                                 +eof+ str)))))

(defun read-changelog (fn)
  "Read the ChangeLog file, returning the list of records."
  (read-list-from-file fn #'read-chlen :read-ahead-function #'read-line))

(defun read-all-changelogs (dir)
  "Read all ChangeLog files under the directory DIR."
  (setq *known-developers* nil)
  (labels ((one-dir (dd)
             (nconc
              (mapcan #'read-changelog (directory (merge-pathnames
                                                   dd "ChangeLog")))
              (mapcan #'one-dir (directory (merge-pathnames "*/" dd))))))
    (one-dir dir)))

(provide :cllib-cvs)
;;; file cvs.lisp ends here
