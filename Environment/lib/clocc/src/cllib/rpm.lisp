;;; RPM updates
;;;
;;; Copyright (C) 1998-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: rpm.lisp,v 2.21 2005/01/27 23:02:46 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/rpm.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `url' - generic function
  (require :cllib-url (translate-logical-pathname "cllib:url"))
  ;; `with-open-pipe', `pipe-input'
  (require :port-shell (translate-logical-pathname "port:shell")))

(in-package :cllib)

(export '(*rpm-locations* rpm show-rpms rpm-to-be-installed rpm-get-list
          rpm-get-new-rpms rpm-list-rpm rpm-clean-up))

;;;
;;; download data
;;;

(defstruct (download-data (:conc-name dld-))
  (url +bad-url+ :type url)     ; location
  (err nil :type (or null error)) ; was there an error?
  (all nil :type list)          ; all files at the location
  (fls nil :type list))         ; files found at the location

(defmethod url ((dld download-data)) (dld-url dld))

(defun rpm-list-size (rpms)
  "Sum the sizes of the RPMs in the list."
  (reduce #'+ rpms :key (safe-fun1 rpm-size rpm-p 0)))

(defmethod print-object ((dld download-data) (out stream))
  (when *print-readably* (return-from print-object (call-next-method)))
  (write (dld-url dld) :stream out)
  (cond ((dld-err dld) (format out "~% - ~a" (dld-err dld)))
        ((dld-all dld)
         (format out " [~:d RPMs, ~:d bytes]" (length (dld-all dld))
                 (rpm-list-size (dld-all dld)))))
  (when (dld-fls dld)
    (format out " [~:d RPMs, ~:d bytes]: ~{~<~%~10t ~1,74:; ~a~>~^,~}."
            (length (dld-fls dld)) (rpm-list-size (dld-fls dld))
            (dld-fls dld))))

(defun dld-reset (dld)
  "Reset ERR and FLS to NIL."
  (declare (type download-data dld))
  (setf (dld-err dld) nil (dld-fls dld) nil (dld-all dld) nil))

(defcustom *rpm-present* simple-vector (make-array 0)
  "The list of the present packages.")
(defcustom *rpm-locations* list
  (macrolet ((mk (&rest args) `(make-download-data :url (make-url ,@args))))
    (list (mk :prot :ftp :host "rawhide.redhat.com"
              :path "/pub/rawhide/i386/RedHat/RPMS/")
          (mk :prot :ftp :host "ftp.rge.com"
              :path "/pub/systems/linux/redhat/rawhide/i386/RedHat/RPMS/")
          (mk :prot :ftp :host "ftp.redhat.com"
              :path "/pub/redhat/current/i386/RedHat/RPMS/")
          (mk :prot :ftp :host "updates.redhat.com"
              :path "/current/i386/")
          (mk :prot :ftp :host "updates.redhat.com"
              :path "/current/noarch/")
          (mk :prot :ftp :host "contrib.redhat.com"
              :path "/contrib/libc6/i386/")
          (mk :prot :ftp :host "contrib.redhat.com"
              :path "/contrib/noarch/noarch/")))
          ;;(mk :prot :ftp :host "developer.redhat.com"
          ;;    :path "/pub/rhcn/RPMS/i386/")
          ;;(mk :prot :ftp :host "developer.redhat.com"
          ;;    :path "/pub/rhcn/RPMS/noarch/")
          ;;(mk :prot :ftp :host "ftp.inconnect.com" ; gnome
          ;;    :path "/pub/unix/linux/redhat-6.0/contrib-updates/")
  ;; (mk :prot :ftp :host "ftp.suse.com" :path ; Xserver
  ;;     "/pub/suse_update/XFree86-3.3.3.1-SuSE/glibc2/")
  ;; (mk :prot :ftp :host "ftp.cc.gatech.edu" :path
  ;; "/pub/linux/distributions/suse/suse_update/XFree86-3.3.3.1-SuSE/glibc2/")
  ;; (mk :prot :ftp :host "ftp.dosemu.org" :path "/dosemu/")
  "*The list of `download-data' structs where to look for RPMs.")
(defcustom *rpm-locations-timeout* integer (* 3600 6)
  "*The number of seconds after which `*rpm-locations*' is updated.")
(defcustom *rpm-local-paths* list '(#p"/var/tmp/RPMS/")
  "The paths to the local RPMs.")
(defcustom *rpm-local-target* pathname #p"/var/tmp/"
  "The local path where to download stuff.")
(defcustom *rpm-command-line* simple-string
  "rpm --queryformat \"%{NAME}-%{VERSION}-%{RELEASE}.%{ARCH} %{BUILDTIME} %{INSTALLTIME} %{SIZE}\\n\""
  "The rpm(1) command line to print all the installed packages.
See `rpm --querytags' for more tags")
(defcustom *rpm-timeout* index-t 600
  "*The default timeout (in seconds) for RPM network operations.")
(defcustom *rpm-max-retry* (or null index-t) 10
  "*The default value of max-retry for RPM.
If nil, retry ad infinitum, otherwise a positive fixnum.")

;;;
;;; RPM
;;;

(defstruct (rpm)
  (name "name?" :type simple-string)
  (vers "vers?" :type simple-string)
  (rels "rels?" :type simple-string)
  (arch "arch?" :type simple-string)
  (btime nil :type (or null integer)) ; build time
  (itime nil :type (or null integer)) ; install time
  (ftime nil :type (or null integer)) ; file time
  (size nil :type (or null integer))
  (note nil))

(defconst +bad-rpm+ rpm (make-rpm) "*The convenient constant for init.")

(defmethod print-object ((rpm rpm) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~:[~;\"~]~a-~a-~a.~a~:[~;\"~]" *print-escape*
              (rpm-name rpm) (rpm-vers rpm) (rpm-rels rpm) (rpm-arch rpm)
              *print-escape*)))

(define-condition rpm-error (file-error code) ()
  (:report (lambda (cc out)
             (format out "[~s/~s]~@[ ~?~]" (port:code-proc cc)
                     (file-error-pathname cc)
                     (port:code-mesg cc) (port:code-args cc)))))

(defun del-file (file &optional (out *standard-output*))
  "Delete the file (no error!) and print a message"
  (multiple-value-bind (tt co) (ignore-errors (delete-file file))
    (format out "~:[failed: ~a~;done~*~]~%" tt co)))

(defun rpm-file (rpm &optional dir)
  "Return the file name on disk that corresponds to this RPM object."
  (declare (type rpm rpm) (type (or pathname null) dir))
  (let ((note (rpm-note rpm)))
    (if (and (consp note) (pathnamep (car note)))
        (if dir (merge-pathnames (car note) dir) (car note))
        (make-pathname :name (format nil "~a" rpm)
                       :type "rpm" :defaults dir))))

(defsubst rpm-pos (name)
  "Find the position of the RPM in `*rpm-present*'."
  (declare (simple-string name))
  (binary-pos name *rpm-present* :test #'string< :key #'rpm-name))

(defun rpm-skip-p (rpm)
  "Do not download such an RPM."
  (declare (type rpm rpm))
  (or (string-beg-with "pre" (rpm-vers rpm))
      (and (search "kernel" (rpm-name rpm) :test #'char=)
           (or (notevery #'digit-char-p (rpm-rels rpm))
               (string= (rpm-vers rpm)
                        (rpm-vers (svref *rpm-present*
                                         (rpm-pos (rpm-name rpm)))))))))
(defcustom *rpm-skip* (or (function (rpm) boolean) string null) #'rpm-skip-p
  "Skip these RPMs.")

(defun short-string-to-rpm (name)
  "Convert the short string like `pack-1.2-3.i386.rpm' to RPM.
Do not use it!!!  Use the generic function `rpm' instead!!!"
  (declare (simple-string name))
  (let* ((len (length name)) (p0 (position #\- name :from-end t))
         (p1 (position #\- name :from-end t :end (1- p0)))
         (p2 (cond ((string= ".rpm" name :start2 (- len 4)) (- len 4))
                   ((position #\Space name)) ; for `rpm-path-valid-p' output
                   (len)))
         (p3 (or (position #\. name :from-end t :start p0 :end p2) p2)))
    (make-rpm :name (subseq name 0 p1) :vers (subseq name (1+ p1) p0)
              :rels (subseq name (1+ p0) p3) :arch (subseq name (1+ p3) p2))))

(eval-when (compile load eval) (fmakunbound 'rpm))
(defgeneric rpm (obj)
  (:documentation "Convert to RPM.")
  (:method ((obj rpm)) obj)
  (:method ((obj symbol))
    (unintern obj)
    (short-string-to-rpm (symbol-name obj)))
  (:method ((path pathname))
    (let ((rr (or (unless (member "apt" (pathname-directory path)
                                  :test #'equal)
                    ;; apt-get saves RPMs under some garbled names;
                    ;; in fact, it would probably be a good idea to
                    ;; always rely on RPM-PATH-VALID-P, but this
                    ;; would be rather expensive
                    (ignore-errors (short-string-to-rpm (pathname-name path))))
                  (short-string-to-rpm
                   (or (rpm-path-valid-p path)
                       (cerror "delete the file" 'rpm-error :proc 'rpm
                               :mesg "invalid file" :pathname path)
                       (progn (format t "~&removing ~s..." path)
                              (del-file path)
                              (return-from rpm nil)))))))
      (setf (rpm-note rr) (list path)
            (rpm-size rr) (file-size path)
            (rpm-ftime rr) (file-write-date path))
      rr)))
(declaim (ftype (function (t) rpm) rpm))
(defmethod rpm ((obj string))
  ;; should work with strings like:
  ;; "pack-1.2-3.i386.rpm"
  ;; "-rw-r--r-- 1 sds sds 220876 Mar 20 19:29 mgetty-voice-1.1.20-1.i386.rpm"
  ;; "rpm-2.92-14.i386 922075412 922585615 1667226"
  (let* ((toks (string-tokens
                (nsubstitute #\- #\: (string-trim +whitespace+ obj))))
         (rpm (ignore-errors (rpm (car toks)))))
    (assert (symbolp (car toks)) ()
            "The first token in ~s is not a symbol: cannot convert to RPM" obj)
    (cond (rpm
           (setf (rpm-btime rpm)
                 (if (cadr toks) (+ +unix-epoch+ (cadr toks)))
                 (rpm-itime rpm)
                 (if (caddr toks) (+ +unix-epoch+ (caddr toks)))
                 (rpm-size rpm) (cadddr toks))
           (return-from rpm rpm))
          ((char/= #\- (schar (symbol-name (car toks)) 0))
           (error "special file cannot be an RPM: ~s" obj))
          ((numberp (fourth toks)) (setq toks (cdddr toks)))
          ((numberp (fifth toks)) (setq toks (cddddr toks))))
    (assert (symbolp (fifth toks)) ()
            "~s: ~s is not a symbol: cannot convert to RPM" obj (fifth toks))
    (setf rpm (rpm (fifth toks)) (rpm-size rpm) (first toks)
          (rpm-ftime rpm)
          (let ((day (third toks)) (mon (infer-month (second toks))))
            (if (numberp (fourth toks))
                (encode-universal-time 0 0 0 day mon (fourth toks))
                (let* ((str (symbol-name (fourth toks)))
                       (mi (parse-integer str :start 3))
                       (ho (parse-integer str :end 2))
                       (now (get-universal-time))
                       (ye (nth-value 5 (decode-universal-time now)))
                       (ft (encode-universal-time 0 mi ho day mon ye)))
                  (if (< ft now) ft
                      (encode-universal-time  0 mi ho day mon (1- ye)))))))
    rpm))

(defun rpm= (r0 r1)
  "Are these RPMs the same?"
  (declare (type rpm r0 r1))
  (and (string= (rpm-name r0) (rpm-name r1))
       (string= (rpm-vers r0) (rpm-vers r1))
       (string= (rpm-rels r0) (rpm-rels r1))
       (string= (rpm-arch r0) (rpm-arch r1))
       (or (and (null (rpm-note r0)) (null (rpm-note r1)))
           (file-equal-p (car (rpm-note r0)) (car (rpm-note r1))))))

(defun rpm< (r0 r1)
  "Sorting order on RPMs."
  (declare (type rpm r0 r1))
  (or (string< (rpm-name r0) (rpm-name r1))
      (and (string= (rpm-name r0) (rpm-name r1))
           (or (version< (rpm-vers r0) (rpm-vers r1))
               (and (string= (rpm-vers r0) (rpm-vers r1))
                    (or (release< (rpm-rels r0) (rpm-rels r1))
                        (and (string= (rpm-rels r0) (rpm-rels r1))
                             (string< (rpm-arch r0) (rpm-arch r1)))))))))

(defun rpm=< (r0 r1)
  "Sorting order on RPMs."
  (declare (type rpm r0 r1))
  (or (string< (rpm-name r0) (rpm-name r1))
      (and (string= (rpm-name r0) (rpm-name r1))
           (or (version< (rpm-vers r0) (rpm-vers r1))
               (and (string= (rpm-vers r0) (rpm-vers r1))
                    (or (release< (rpm-rels r0) (rpm-rels r1))
                        (and (string= (rpm-rels r0) (rpm-rels r1))
                             (string<= (rpm-arch r0) (rpm-arch r1)))))))))

(defun version< (v0 v1)
  "Sorting order for versions."
  (declare (simple-string v0 v1))
  (do* ((b0 0 (1+ e0)) (b1 0 (1+ e1))
        (e0 (position #\. v0) (position #\. v0 :start b0))
        (e1 (position #\. v1) (position #\. v1 :start b1)))
       ((or (null e0) (null e1)
            (string/= v0 v1 :start1 b0 :start2 b1 :end1 e0 :end2 e1))
        (or (release< (subseq v0 b0 e0) (subseq v1 b1 e1))
            (and (string= v0 v1 :start1 b0 :start2 b1 :end1 e0 :end2 e1)
                 (null e0) e1)))))

(defun release< (v0 v1)
  "Sorting order for releases."
  (declare (simple-string v0 v1))
  (let ((l0 (or (position-if-not #'digit-char-p v0) (length v0)))
        (l1 (or (position-if-not #'digit-char-p v1) (length v1))))
    (declare (type index-t l0 l1))
    (cond ((and (= l0 (length v0)) (< l1 (length v1)) ; v1 - beta
                (char-equal #\b (schar v1 l1)))
           nil)
          ((and (= l1 (length v1)) (< l0 (length v0)) ; v0 - beta
                (char-equal #\b (schar v0 l0)))
           t)
          ((> l0 l1) (string< v0 (format nil "~v,,,'0@a" l0 v1)))
          ((< l0 l1) (string< (format nil "~v,,,'0@a" l1 v0) v1))
          ((string< v0 v1)))))

(defun rpm-merge-notes (r0 r1)
  "Put the notes from r1 into r0."
  (declare (type rpm r0 r1))
  (setf (rpm-note r0) (nconc (rpm-note r0) (rpm-note r1)
                             (when (rpm-itime r1)
                               (list (cons (time2date (rpm-itime r1))
                                           (time2date (rpm-btime r1)))))))
  r0)

(defun rpm-read (in)
  "Read RPMs from the stream till EOF.  Close the stream."
  (declare (stream in))
  (sort (loop :for line :of-type (or null simple-string) =
              (read-line in nil nil)
              :for rr :of-type (or null rpm) = (ignore-errors (rpm line))
              :while line :when rr :collect rr :finally (close in))
        #'rpm<))

(defun rpm-installed ()
  "Return the list of RPMs installed."
  (with-open-pipe (st (pipe-input *rpm-command-line* "-qa")) (rpm-read st)))

(defun rpm-path-valid-p (path)
  "Check for validity of the RPM."
  (declare (type pathname path))
  (with-open-pipe (st (pipe-input *rpm-command-line* "-qp" path))
    (read-line st nil nil)))

(defun rpm-downloaded ()
  "Return the list of RPMs already downloaded."
  (sort (mapcar
         #'rpm (delete-if-not
                #'rpm-path-valid-p
                (mapcan (lambda (pp) (directory (merge-pathnames "*.rpm" pp)))
                        (adjoin *rpm-local-target* *rpm-local-paths*
                                :test #'equalp))))
        #'rpm<))

(defun rpm-present ()
  "Return the simple vector of RPMs installed and already downloaded."
  (delete-duplicates
   (merge 'simple-vector (rpm-downloaded) (rpm-installed)
          (lambda (r0 r1)
            (let ((res (rpm< r0 r1)))
              (when (string= (rpm-name r0) (rpm-name r1))
                (if res (rpm-merge-notes r1 r0) (rpm-merge-notes r0 r1)))
              res)))
   :test #'string= :key #'rpm-name))

(defun rpm-available (url &key (out *standard-output*) (err *error-output*)
                      ((:max-retry *url-max-retry*) *rpm-max-retry*)
                      ((:timeout *url-timeout*) *rpm-timeout*)
                      (retry 2))
  "Return the list of all available RPMs."
  (declare (type url url) (type (or null stream) err out) (type index-t retry))
  (handler-case
      (with-open-url (sock url :err err)
        (let ((data (ftp-get-passive-socket sock nil)))
          (when (ignore-errors (url-ask sock :list "list *.rpm")) ; 150
            (prog1 (map-in (lambda (rr) (setf (rpm-note rr) (list url)) rr)
                           (rpm-read data))
              (url-ask sock :list))))) ; 226
    ((or net-path login) (co)
      (mesg :log err " * rpm-available [~a]: Cannot login:~% - ~a~%" url co)
      (error co))
    (network (co)
      (mesg :log err " * rpm-available [~a]:~% - ~a~%" url co)
      (cond ((plusp retry)
             (mesg :err err " * rpm-available: ~r more attempt~:p~%" retry)
             (rpm-available url :out out :err err :retry (1- retry)))
            (t (error co))))))

(defun rpm-prune-list (rpms)
  "Remove the elements in the list which are better in `*rpm-present*'.
The elements matching `*rpm-skip*' are removed, too."
  (declare (list rpms))
  (remove-if-not
   (lambda (rpm)
     (let ((pos (rpm-pos (rpm-name rpm))))
       (and pos (rpm< (svref *rpm-present* pos) rpm)
            (not (typecase *rpm-skip*
                   (string (search *rpm-skip* (rpm-name rpm)
                                   :test #'char=))
                   (function (funcall *rpm-skip* rpm))
                   (null nil)
                   (t (error 'case-error :proc 'rpm-prune-list
                             :args (list '*rpm-skip* *rpm-skip*
                                         'function 'string nil))))))))
   rpms))

(defun rpm-print (rpm &optional idx (out *standard-output*))
  "Print RPM in a full form."
  (declare (type rpm rpm) (stream out))
  (format out "~@[~3d. ~]~a [~@[~:d bytes, ~]~a/~a/~a]:~{~%~10t~a~}~%"
          idx rpm (rpm-size rpm)
          (if (rpm-btime rpm) (time2date (rpm-btime rpm)))
          (if (rpm-itime rpm) (time2date (rpm-itime rpm)))
          (if (rpm-ftime rpm) (time2date (rpm-ftime rpm)))
          (rpm-note rpm)))

(defun rpm-get-available (&key force (out *standard-output*)
                          ((:max-retry *url-max-retry*) *rpm-max-retry*)
                          ((:timeout *url-timeout*) *rpm-timeout*)
                          (err *error-output*))
  "Get the list of all RPMs in `*rpm-locations*' and put it there.
Then generate the list to download."
  (declare (type (or null stream) out err))
  (when (or force (notany #'dld-all *rpm-locations*)
            (null (get '*rpm-locations* 'updated))
            (> (- (get-universal-time) (get '*rpm-locations* 'updated))
               *rpm-locations-timeout*))
    (format out " *** Getting the list of new packages...~%")
    (with-timing (:out out)
      (let ((na 0) (le 0)
            #+clisp (#+lisp=cl  ext:*pprint-first-newline*
                     #-lisp=cl lisp:*pprint-first-newline* nil))
        (declare (type index-t na le))
        (dolist (dld *rpm-locations*)
          (declare (type download-data dld))
          (dld-reset dld)
          (format out " *** processing `~a'...~%" dld)
          (handler-case
              (with-timing (:out out)
                (setf (dld-all dld) (rpm-available (dld-url dld)
                                                   :out out :err err)
                      (dld-fls dld) (rpm-prune-list (dld-all dld))
                      le (length (dld-fls dld)))
                (incf na le)
                (format out " *** ~d new RPM~:p (out of ~d):~% --> ~a~% * "
                        le (length (dld-all dld)) dld))
            (error (co)
              (setf (dld-err dld) co)
              (format out " *** failed:~% - ~a~%" co))))
        (format out " *** ~d new RPM~:p in ~r URL~:p " na
                (length *rpm-locations*))))
    (setf (get '*rpm-locations* 'updated) (get-universal-time))
    (dolist (dld *rpm-locations*)
        (when (dld-fls dld)
          (format out " * ~a~%" dld)))))

;;;###autoload
(defun show-rpms (&optional (what "") (local t))
  "Print some RPMs."
  (declare (simple-string what))
  (if local (rpm-get-present) (rpm-get-available))
  (let ((nrpm (if local (length *rpm-present*)
                  (reduce #'+ *rpm-locations* :key (compose length dld-all))))
        #+clisp (#+lisp=cl  ext:*pprint-first-newline*
                 #-lisp=cl lisp:*pprint-first-newline* nil)
        (nf 0))
    (declare (type index-t nf))
    (if (zerop (length what))
        (if local (format t "~& *** ~d RPM~:p~%" nrpm)
            (format t "~& *** ~d RPM~:p in ~r URL~:p~%" nrpm
                    (length *rpm-locations*)))
        (if local (format t "~&Searching ~d RPM~:p for `~a'~%" nrpm what)
            (format t "~&Searching ~d RPM~:p (~r URL~:p) for `~a'~%" nrpm
                    (length *rpm-locations*) what)))
    (dolist (elt (if local (list *rpm-present*) *rpm-locations*))
      (unless local (format t " *** ~a~%" elt))
      (map nil (lambda (rpm)
                 (declare (type rpm rpm))
                 (when (search what (rpm-name rpm) :test #'char-equal)
                   (rpm-print rpm (incf nf))))
           (if local elt (dld-all elt))))
    (format t "~[None~:;~:*~d~] found.~%" nf)
    nf))

(defun delete-rpms (what)
  "Delete some RPMs from the vector `*rpm-present*'."
  (declare (simple-string what))
  (flet ((killrpm (what where)
           (delete what where :key #'rpm-name
                   :test (lambda (wh nm) (search wh nm :test #'char-equal)))))
    (when (and (/= 0 (show-rpms what)) (yes-or-no-p "Delete all these? "))
      (setq *rpm-present* (killrpm what *rpm-present*))
      (dolist (dld *rpm-locations*)
        (setf (dld-fls dld) (killrpm what (dld-fls dld)))
        (format t " -> ~a~%" dld))
      (show-rpms what))))

(defun rpm-current (name)
  "Return the rpm corresponding to this name."
  (declare (simple-string name))
  (with-open-pipe (st (pipe-input *rpm-command-line* "-q" name))
    (let ((ln (read-line st nil nil))) (when ln (rpm ln)))))

(defun rpm-to-be-installed ()
  "Print RPMs which are downloaded but not yet installed."
  (loop :for rpm :of-type rpm :across *rpm-present*
        :with ii :of-type index-t = 0
        :unless (rpm-itime rpm)
        :do (rpm-print rpm (incf ii))))

(defun rpm-get-present (&optional force (out *standard-output*))
  "Make sure `*rpm-present*' is initialized."
  (declare (type (or null stream) out))
  (when (or force (zerop (length *rpm-present*)))
    (with-timing (:out out)
      (mesg t out " * Finding the RPMs...")
      (setq *rpm-present* (rpm-present))
      (mesg t out "done [~d package~:p]" (length *rpm-present*)))))

;;;###autoload
(defun rpm-get-list (url rpms &key (out *standard-output*) (err *error-output*)
                      ((:max-retry *url-max-retry*) *rpm-max-retry*)
                      ((:timeout *url-timeout*) *rpm-timeout*)
                     (len (length rpms)) (retry 2))
  "Get the list of RPMS from URL."
  (declare (type url url) (list rpms) (type (or null stream) out err)
           (type index-t len retry))
  (handler-case
      (with-open-url (sock url :err err)
        (url-ask sock :type "type i") ; 200
        (loop :for rpm :in rpms
              :and ii :of-type index-t :upfrom 1
              :for pos = (rpm-pos (rpm-name rpm))
              :and all :of-type file-size-t = (reduce #'+ rpms :key #'rpm-size)
              :then (- all (rpm-size rpm))
              :with got :of-type file-size-t = 0
              :do (format out " *** [~d/~d] ~a~@[ [~:d bytes~@[, ~/pr-secs/~]~
~@[/~/pr-secs/~]]~]~%"
                          ii len rpm (rpm-size rpm) (url-eta (rpm-size rpm))
                          (url-eta all))
              :if (rpm< (svref *rpm-present* pos) rpm) :do
              (multiple-value-bind (tot el st path)
                  (ftp-get-file sock (rpm-file rpm)
                                *rpm-local-target* :err err :out out :bin t)
                (declare (type file-size-t tot) (double-float el)
                         (simple-string st) (type pathname path))
                (cond ((rpm-path-valid-p path)
                       (incf got tot)
                       (format out " *** done [~:d bytes, ~a, ~:d bytes/sec]
 *** replacing ~a with ~a in `*rpm-present*'~%"
                               tot st (round tot el)
                               (svref *rpm-present* pos) rpm)
                       (setf (svref *rpm-present* pos)
                             (rpm-merge-notes rpm (svref *rpm-present* pos))))
                      (t (format out " *** file `~a' is corrupted:~%" path)
                         (run-prog "rpm" :args (list "-K" path))
                         (del-file path))))
              :else :do (format out " *** already have~25t`~a'.~%" rpm)
              :end :finally (return got)))
    (network (co)
      (mesg :log err "rpm-get-list [~a]:~% - ~a~%" url co)
      (cond ((plusp retry)
             (mesg :err err "rpm-get-list: ~r more attempt~:p~%" retry)
             (rpm-get-list url rpms :out out :err err :retry (1- retry)
                           :len len))
            (0)))))

;;;###autoload
(defun rpm-get-new-rpms (&key force (out *standard-output*)
                         ((:max-retry *url-max-retry*) *rpm-max-retry*)
                         ((:timeout *url-timeout*) *rpm-timeout*)
                         (err *error-output*))
  ;; (rpm-get-new-rpms)
  "Download the RPMs from `*rpm-locations*'."
  (declare (type (or null stream) err out))
  (format out "~& *** [~a] `rpm-get-new-rpms' started.~%" (current-time nil))
  (rpm-get-present force out)
  (format out " *** ~d package~:p present~%" (length *rpm-present*))
  (rpm-get-available :force force :out out :err err)
  (let ((bt (get-int-time nil)) (glob 0)
        #+clisp (#+lisp=cl  ext:*pprint-first-newline*
                 #-lisp=cl lisp:*pprint-first-newline* nil))
    (declare (type (integer 0) bt) (type file-size-t glob))
    (dolist (dld *rpm-locations*)
      (declare (type download-data dld))
      (cond ((dld-fls dld)
             (let ((len (length (dld-fls dld))))
               (format out " *** getting ~d file~:p: ~a~%" len dld)
               (incf glob (rpm-get-list (dld-url dld) (dld-fls dld) :out out
                                        :err err :len len))
               (format out " *** Pruning [~d]..." len) (force-output)
               (setf (dld-fls dld) (rpm-prune-list (dld-fls dld)))
               (format out "done [~d left]~%" (length (dld-fls dld)))))
            ((dld-err dld) (format out " *** ~a~%" dld))
            (t (format out " *** ~a:~60tno new packages~%" dld))))
    (format out " *** [~a] done" (current-time nil))
    (if (zerop glob) (format out ".~%")
        (multiple-value-bind (el st) (elapsed bt nil t)
          (format out " [~:d bytes, ~a, ~:d bytes/sec]~%"
                  glob st (round glob el))))
    glob))

;;;###autoload
(defun rpm-list-rpm (name &key (out *standard-output*) (err *error-output*)
                     ((:max-retry *url-max-retry*) *rpm-max-retry*)
                     ((:timeout *url-timeout*) *rpm-timeout*)
                     local)
  "Look for the RPM on all sites.
If `local' keyword argument is non-nil, use only the data already
available in `*rpm-locations*'."
  (dolist (dld *rpm-locations*)
    (if local
        (loop :initially (format out " *** ~a:~%" dld)
              :for rpm :in (dld-all dld) :with ii :of-type index-t = 0
              :when (search name (rpm-name rpm))
              :do (format out "~3d: ~a~%" (incf ii) rpm))
        (ignore-errors
          (with-open-url (sock (dld-url dld) :err err)
            (format out " *** ~a:~%" dld)
            (ftp-list sock :name name :err err :out out))))))

;; (rpm-clean-up :dirs "/var/tmp/")
;; (rpm-clean-up)
;; (rpm-clean-up :dirs "/var/tmp/o/")

(defun rpm-clean-hdr (dir &optional (out *standard-output*))
  (format out "~& ***** HDR Cleaning up `~a'~%" dir)
  (do ((fl (directory (merge-pathnames "**/*.hdr" dir)) (cdr fl))
       (nn 0) (tot 0 (1+ tot)) (fs 0) (tfs 0) (sz 0))
      ((null fl)
       (format out " ***** ~:d/~:d HDR file~:p (~:d/~:d byte~:p) deleted~%"
               nn tot fs tfs)
       (values nn fs tot tfs))
    (setq sz (file-size (car fl)) tfs (+ sz tfs))
    (unless (probe-file (make-pathname :type "rpm" :defaults (car fl)))
      (format out " ~3d * removing ~a (~:d bytes) (no RPM)..."
              (incf nn) (car fl) sz)
      (del-file (car fl))
      (incf fs sz))))

;;;###autoload
(defun rpm-clean-up (&key (out *standard-output*) total-list
                     (dirs (remove *rpm-local-target* *rpm-local-paths*
                                   :test #'equalp)))
  "Remove old RPM files.
This will remove the RPM files for which there is a newer version,
as well as corrupt RPM files.
If you are using both up2date and apt-get, you should pass the up2date
cache directory before the apt-get one:
  (rpm-clean-up :dirs '(\"/var/spool/up2date/\" \"/var/cache/apt/archives/\"))
Then the up2date files will be kept even when there is a newer apt-get
package, because we can assume that apt-get collects packages from less
trustworthy sites than up2date (which uses only redhat.com)."
  (declare (stream out))
  (etypecase dirs
    ((or string pathname)
     (format out "~& ***** RPM Cleaning up `~a'~%" dirs)
     (do* ((all (sort (delete nil (map-in #'rpm (directory (merge-pathnames
                                                            "**/*.rpm" dirs))))
                     #'rpm<))
           (nn 0) (tot 0 (1+ tot)) (fs 0) (tfs 0) (sz 0) nfile (rm nil nil)
           (fl all (cdr fl)) (file (rpm-file (car fl) dirs) nfile)
           (good (rpm-path-valid-p file)))
          ((null fl)
           (format out " ***** ~:d/~:d RPM file~:p (~:d/~:d byte~:p) deleted~%"
                   nn tot fs tfs)
           (multiple-value-bind (nn1 fs1 tot1 tfs1) (rpm-clean-hdr dirs out)
             (incf nn nn1) (incf fs fs1) (incf tot tot1) (incf tfs tfs1))
           (format out " ===== ~:d/~:d file~:p (~:d/~:d byte~:p) deleted~%"
                   nn tot fs tfs)
           (values nn fs tot tfs (merge 'list (delete nil all)
                                        total-list #'rpm<)))
       (declare (type index-t nn tot) (type file-size-t sz fs tfs))
       (setq sz (file-size file) tfs (+ sz tfs))
       (unless good (setq rm "corruption"))
       (when (cdr fl)
         (setq nfile (rpm-file (cadr fl) dirs) good (rpm-path-valid-p nfile))
         (unless good (run-prog "rpm" :args (list "-K" nfile)))
         (when (and good (string= (rpm-name (car fl)) (rpm-name (cadr fl))))
           (setq rm (cadr fl))))
       (unless rm
         (setq rm (binary-member (car fl) total-list :test #'rpm=<))
         (if (and rm
                  (or (rpm= (car rm) (car fl))
                      (and (string= (rpm-name (car rm)) (rpm-name (car fl)))
                           (rpm< (car fl) (car rm)))))
             (setq rm (rpm-file (car rm)))
             (setq rm nil)))
       (when rm
         (format out " ~3d * removing ~a[~s] (~:d bytes) because of ~a..."
                 (incf nn) (car fl) file sz rm)
         (del-file file) (incf fs sz)
         (setf (car fl) nil)))) ; remove (car fl)
    (sequence
     (let ((nnt 0) (fst 0) (tott 0) (tfst 0))
       (declare (type index-t nnt tott) (type file-size-t fst tfst))
       (map nil (lambda (dd)
                  (multiple-value-bind (nn fs tot tfs all-rpms)
                      (rpm-clean-up :dirs dd :out out :total-list total-list)
                    (declare (type index-t nn tot) (type file-size-t fs tfs))
                    (setq total-list all-rpms)
                    (incf nnt nn) (incf fst fs)
                    (incf tott tot) (incf tfst tfs)))
            dirs)
       (format out " Total ~:d/~:d/~:d file~:p (~:d/~:d byte~:p) deleted~%"
               nnt tott (length total-list) fst tfst)
       (values nnt fst tott tfst total-list)))))

;;; active mode ftp - doesn't work - why?!

#+nil ;; (or clisp allegro)
(progn

(defun local-host (sock)
  #+clisp (let ((ho (ext:socket-stream-local sock)))
            (subseq ho 0 (position #\Space ho :test #'char=)))
  #+allegro (socket:ipaddr-to-dotted (socket:local-host sock)))

(defun local-port (serv)
  (#+clisp ext:socket-server-port #+allegro socket:local-port serv))

(defun ftp-port-command (sock serv)
  (let ((port (local-port serv)))
    (declare (type index-t port))
    (url-ask sock :port "port ~a,~d,~d" ; 200
             (substitute #\, #\. (local-host sock))
             (logand #xff (ash port -8)) (logand #xff port))))
)

#+nil
(progn

(setq sock (open-url (url "ftp://ftp.gnu.org/pub/gnu/")))
(setq sock (open-url (url "ftp://ftp.nyu.edu/pub/")))
(setq serv (open-socket-server sock))
(ftp-port-command sock serv)
(read-line sock)
(progn
  ;; (format sock "retr clisp.README~%")
  (format sock "list~%")
  (setq sk (socket-accept serv)))
(read-line sk)
(close sock)
(close serv)
(close sk)
(setq serv (ext:socket-server 3288))
(ext:socket-server-close serv)
(ext:socket-stream-handle sock)
(ext:socket-stream-host sock)
(ext:socket-stream-port sock)
(ext:socket-stream-peer sock)
(ext:socket-stream-local sock)

(ftp-get-file sock "wn.README" "/var/tmp/" )
(ftp-list sock)
(url-ask sock :pwd "pwd") ; 257
(url-ask sock :help "help ~a" "type") ; 214
(url-ask sock :help "help") ; 214
(url-ask sock :cwd "cwd gnu") ; 250
(ftp-list sock)
(show-rpms "print")

(setq serv (ext:socket-server 0))
(ext:socket-server-close serv)
(setq sock (ext:socket-connect 21 "ftp.gnu.org"))
(setq serv (ext:socket-server sock))
(setq sock (ext:socket-connect 21 "mute"))

;socket.d:fill_hostname
;    printf ("#(%lu \"%s\" \"%s\" %u)\n", hd->host, hd->hostname,
;            hd->truename, hd->port);

)

(provide :cllib-rpm)
;;; file rpm.lisp ends here
