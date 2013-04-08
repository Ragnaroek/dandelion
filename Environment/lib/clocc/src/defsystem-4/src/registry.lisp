;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; registry.lisp --
;;; Registry and PROVIDE/REQUIRE utilities.

(in-package "MK4")

(defvar *system-file-extension* "system")

(defparameter *central-registry*
  (list (make-pathname :directory '(:absolute
				    "usr"
				    "local"
				    "common-lisp"
				    "systems")))
  "A list of locations (pathnames) where to look for system definition
files.  The list is searched sequentially.  It should not be
manipulated directly. Use ADD-REGISTRY-LOCATION,
REMOVE-REGISTRY-LOCATION, and REMOVE-REGISTRY-ENTRY
and SYSTEM-REGISTRY-PATHS instead.")


(defun add-registry-location (pathname &optional at-end-p)
  "Adds a pathname to the central registry.
If AT-END-P is non NIL the PATHNAME is attached at the end of the
registry list, otherwise it is prepended (the default behavior)."
  (if at-end-p
      (setf *central-registry*
	    (nconc *central-registry* (list (pathname pathname))))
      (push (pathname pathname) *central-registry*)))


(defun delete-registry-location (location)
  "Deletes a pathname (LOCATION) from the central registry.
Note that equality for pathnames is not relied upon in this case.  It
is assumed that the LOCATION being removed is EQUALP to one of those
listed in the central registry."
  (setf *central-registry* (delete location *central-registry*
				   :test #'equalp)))

(defun delete-registry-entry (entry-index
			      &aux (registry-length
				    (list-length *central-registry*)))
  "Removes the entry at ENTRY-INDEX from the central registry."
  (cond ((or (minusp entry-index)
	     (>= entry-index registry-length))
	 (warn "ENTRY-INDEX is out of bounds 0 and ~D.~@
                The registry won't be modified."
	       registry-length))
	((zerop entry-index)
	 (setf *central-registry* (rest *central-registry*)))
	((= entry-index (1- registry-length))
	 (setf *central-registry* (butlast *central-registry*)))
	(t
	 (setf *central-registry*
	       (nconc (subseq *central-registry* 0 entry-index)
		      (subseq *central-registry* (1+ entry-index)))))
	))


;;; system-registry-paths --
;;; Note that the HOST spec is left out because it is  too
;;; implementation dependent to be relied on.

(defun system-registry-paths (&optional paths-list
					(add-current-directory-pathname-p t)
					(add-user-homedir-pathname-p t)
					;; host
					)
  "Returns a list of paths where to look for system definitions.
The value of *CENTRAL-REGISTRY* is returned prepended by the contents
of the list PATHS-LIST, by the `current directory', if
ADD-CURRENT-DIRECTORY-PATHNAME-P is non NIL, the default, and by the
value of USER-HOMEDIR-PATHNAME if ADD-USER-HOMEDIR-PATHNAME-P is non
NIL, the default. USER-HOMEDIR-PATHNAME is called without supplying a
`host'. Cfr. the ANSI specification for an explanation."

  (declare (type list paths-list))
  (append (mapcar #'pathname paths-list)
	  (when add-current-directory-pathname-p (list (cl.env:cwd)))
	  (when add-user-homedir-pathname-p (list (user-homedir-pathname ; host
						   )))
	  *central-registry*))


(defun add-registry-location (pathname)
  "Adds a path to the central registry."
  (push pathname *central-registry*))

(define-condition system-definition-file-not-found (file-error)
  ((name :reader file-not-found-system-name
	 :initarg :system-name)
   (search-paths :reader file-not-found-system-search-paths
		 :initarg :system-search-paths)
   )
  (:report (lambda (cnd stream)
	     (format stream
		     "MK4: system ~S file definition ~S not found in~{~&~S~}."
		     (file-not-found-system-name cnd)
		     (file-error-pathname cnd)
		     (file-not-found-system-search-paths cnd)))))


(defun compute-system-definition-file (system-name
				       &key
				       definition-pathname
				       alternative-locations
				       (current-directory-p t)
				       (user-homedir-pathname-p t)
				       (if-does-not-exist :error)
				       )
  ;; If IF-DOES-NOT-EXIST is not EQ :ERROR then the error is not signalled.
  (declare (type (or symbol string) system-name)
	   (type (or null string pathname) definition-pathname))

  (when (and definition-pathname (probe-file definition-pathname))
    (return-from compute-system-definition-file
      (pathname definition-pathname)))

  (let* ((system-file-name
	  (etypecase system-name
	    (symbol (string-downcase (symbol-name system-name)))
	    (string system-name)))
	 (system-file-name-pathname
	  (make-pathname :name system-file-name
			 :type *system-file-extension*))
	 )

    (dolist (registry-path (system-registry-paths alternative-locations
						  current-directory-p
						  user-homedir-pathname-p))
      (let ((system-file-pathname
	     (adjoin-directories registry-path system-file-name-pathname))
	    )
	(when (probe-file system-file-pathname)
	  (return-from compute-system-definition-file system-file-pathname))))

    (when (eq if-does-not-exist :error)
      (error 'system-definition-file-not-found
	     :system-name system-name
	     :system-search-paths (system-registry-paths alternative-locations)
	     :pathname (or definition-pathname system-file-name-pathname)))))


;;; end of file -- registry.lisp --
