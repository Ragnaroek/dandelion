;;; -*- Mode: Lisp -*-

;;; DEFSYSTEM 4.0

;;; actions.lisp --

(in-package "MK4")


;;; Class Action -- A good idea from ASDF and implied by KMP.

(defclass action ()
  ((action-tag :reader action-tag :initarg :tag))
  (:default-initargs :tag :generic-action)
  (:documentation "The Action Class."))


(defgeneric actionp (a)
  (:method ((a action)) t)
  (:method ((a t)) nil))


(defmethod print-object ((a action) (s stream))
  (print-unreadable-object (a s :identity t)
     (format s "MK4 ACTION ~A"
	     (class-name (class-of a)))))


(defclass standard-load-action (action)
  ()
  (:default-initargs :tag :load))

(defclass standard-compile-action (action)
  ()
  (:default-initargs :tag :compile))

(defclass standard-clean-action (action)
  ()
  (:default-initargs :tag :clean))

(defclass standard-describe-action (action)
  ()
  (:default-initargs :tag :describe))


;;; Actions Data Base.

(defparameter *actions-registry*
  (list :load
	:compile
	:clean
	:describe
	))


(defun list-actions-registry ()
  (copy-list *actions-registry*)) ; To avoid programmers messing
				  ; around with it.

(defun action-registered-p (action)
  (etypecase action
    (symbol (not (null (member action *actions-registry*
			       :test #'eq))))
    (string (not (null (member action *actions-registry*
			       :test #'string-equal))))
    (action (action-registered-p (action-tag action)))))


(defun register-action (action)
  (etypecase action
    (symbol (pushnew action *actions-registry* :test #'eq))
    (action (pushnew (action-tag action) *actions-registry* :test #'eq))))

  

;;; We need to simplify a `specialized lambda list' into a `generic
;;; function lambda list'.

(defun simplify-lambda-list (spec-lambda-list)
  (mapcar #'(lambda (arg)
	      (if (consp arg)
		  (first arg)		; This takes care of the &key
					; (:kwd-var var) case.
		  arg))
	  spec-lambda-list))


(defmacro define-action (name specialized-lambda-list
			      (&key (conc-name t)
				    (documentation nil)
				    (package (symbol-package name))
				    (system t))
			      &body forms)
  (declare (type symbol name)
	   (ignore system))
  (let* ((name-as-string (symbol-name name))
	 (action-name (if conc-name
			 (intern (format nil "~A-ACTION"
					 name-as-string
					 package))
			 name))
	 (lambda-list (mapcar #'simplify-lambda-list specialized-lambda-list))
	 )

    (register-action (intern name-as-string (find-package "KEYWORD")))
    #||
    (intern name-as-string (find-package "KEYWORD"))
    (register-action system
		     action-name
		     (find-symbol name-as-atring (find-package "KEYWORD")))
    ||#
    `(defgeneric ,action-name ,lambda-list
       ,@(when documentation `(:documentation ,documentation))
       (:method ,specialized-lambda-list ,@forms))))


	 

;;; end of file -- actions.lisp --
