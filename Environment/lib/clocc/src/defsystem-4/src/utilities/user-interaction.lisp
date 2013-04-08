;;; -*- Mode: CLtL -*-


;;; user-interaction.lisp --
;;; Output to User.
;;;
;;; Notes.
;;;
;;; 20020725 Marco Antoniotti
;;; The MK3 code is rather complex and mayby too much.
;;; It will be replaced with simpler functionality.

(in-package "MK4")

;;;===========================================================================
;;; New functionality.


(defvar *defsystem-standard-output* *standard-output*)

(defvar *defsystem-error-output* *error-output*)

(defvar *defsystem-trace-output* *trace-output*)

(defvar *user-message-prefix* ";;; MK4: ")

(defun user-message (where format-string &rest format-arguments)
  (format where "~&~A~?~%"
	  *user-message-prefix*
	  format-string
	  format-arguments))


;;;===========================================================================
;;; Old code

;;; All output to the user is via the tell-user functions.

;; probably should remove the ",1" entirely. But AKCL 1.243 dies on it 
;; because of an AKCL bug.
;; KGK suggests using an 8 instead, but 1 does nicely.
(defun prompt-string (component)
  (format nil "; ~:[~;TEST:~]~V,1@T "
	  *oos-test*
	  (component-indent component)))

#|
(defun format-justified-string (prompt contents)
  (format t (concatenate 'string
			 "~%"
			 prompt
			 "-~{~<~%" prompt " ~1,80:; ~A~>~^~}")
	  (split-string contents))
  (finish-output *standard-output*))
|#

(defun format-justified-string (prompt contents &optional (width 80)
				       (stream *standard-output*))
  (let ((prompt-length (+ 2 (length prompt))))
    (cond ((< (+ prompt-length (length contents)) width)
	   (format stream "~%~A- ~A" prompt contents))
	  (t
	   (format stream "~%~A-" prompt)
	   (do* ((cursor prompt-length)
		 (contents (split-sequence:split-sequence #\Space contents)
			   (cdr contents))
		 (content (car contents) (car contents))
		 (content-length (1+ (length content)) (1+ (length content))))
	       ((null contents))
	     (cond ((< (+ cursor content-length) width)
		    (incf cursor content-length)
		    (format stream " ~A" content))
		   (t
		    (setf cursor (+ prompt-length content-length))
		    (format stream "~%~A  ~A" prompt content)))))))
  (finish-output stream))


(defun tell-user (what component &optional type no-dots force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
     (format nil "~A ~(~A~) ~@[\"~A\"~] ~:[~;...~]"
	     ;; To have better messages, wrap the following around the
	     ;; case statement:
	     ;;(if (find (component-type component) 
	     ;;    '(:defsystem :system :subsystem :module))
	     ;;  "Checking"
	     ;;  (case ...))
	     ;; This gets around the problem of DEFSYSTEM reporting
	     ;; that it's loading a module, when it eventually never
	     ;; loads any of the files of the module.
	     (case what
	       ((compile :compile) 
		(if (component-load-only component)
		    ;; If it is :load-only t, we're loading.
		    "Loading"
		    ;; Otherwise we're compiling.
		    "Compiling"))
	       ((load :load) "Loading")
	       (otherwise what))
	     (component-type component)
	     (or (when type
		   (get-component-source-pathname component type))
		 (component-name component))
	     (and *tell-user-when-done*
		  (not no-dots))))))


(defun tell-user-done (component &optional force no-dots)
  ;; test is no longer really used, but we're leaving it in.
  (when (and *tell-user-when-done*
	     (or *oos-verbose* force))
    (format t "~&~A~:[~;...~] Done."
	    (prompt-string component) (not no-dots))
    (finish-output *standard-output*)))


(defmacro with-tell-user ((what component &optional type no-dots force)
			  &body body)
  `(progn
     (tell-user ,what ,component ,type ,no-dots ,force)
     ,@body
     (tell-user-done ,component ,force ,no-dots)))

#|
(defun tell-user-no-files (component &optional force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
      (format nil "Source file ~A ~
             ~:[and binary file ~A ~;~]not found, not loading."
	      (component-full-pathname component :source)
	      (or (session-load-source-if-no-binary component)
		  (sessions-load-source-instead-of-binary component))
	      (component-full-pathname component :binary)))))
|#


(defun tell-user-require-system (name parent)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - System ~A requires ~S"
	    *oos-test* (component-name parent) name)
    (finish-output *standard-output*)))


(defun tell-user-generic (string)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - ~A"
	    *oos-test* string)
    (finish-output *standard-output*)))

;;; end of file -- user-interaction.lisp --
