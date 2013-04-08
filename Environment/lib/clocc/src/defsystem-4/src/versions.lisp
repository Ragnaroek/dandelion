;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; versions.lisp --

(in-package "MK4")

(defstruct (version (:print-function print-version)
		    (:constructor %make-version))
  (symbolic-tag 'nil :type (or symbol string) :read-only t)
  (numbers-tags '() :type list :read-only t)
  (documentation "" :type string))


(defun make-version (&key symbolic-tag (numbers-tags ()) (documentation ""))
  (declare (type symbol symbolic-tag)
	   (type string documentation))
  (assert (every (lambda (x)
		   (or (numberp x) (stringp x) (symbolp x)))
		 numbers-tags)
	  (numbers-tags)
	  (make-condition 'type-error
			  :datum numbers-tags
			  :expected-type '(list (or number string symbol))))
  (%make-version :symbolic-tag symbolic-tag
		 :numbers-tags numbers-tags
		 :documentation documentation))

(defun make-version* (&rest numbers-and-tags)
  (make-version :numbers-tags numbers-and-tags))
	  

(defun version-major-number-tag (v)
  (first (version-numbers-tags v)))


(defun version-minor-number-tag (v)
  (second (version-numbers-tags v)))


(defparameter *parse-version-readtable* (copy-readtable))

(defun parse-version-string (version-string
			     &key
			     (case (readtable-case *readtable*)))
  "Parses a string containing a `version'.
It returns a list suitable as an argument to

    (APPLY #'MK4:MAKE-VERSION <parsed-version-string>)

The syntax accepted for a version is is

    version-string ::= <tag>?
                       ':'?
                       <version-n-or-tag>
                       ('.'  <version-n-or-tag>)*
    version-n-or-tag ::= <positive-integer>
                     |   <string>
                     |   <symbol>
"
  (flet ((parse-version-number-tag (vnt)
	   (cond ((string-equal vnt '*) :wild)
		 ((string-equal vnt '**) :wild-inferiors)
		 (t (read-from-string vnt))))
	 )
    (assert (<= (count #\: version-string) 1)
	    (version-string)
	    "Parsing version string ~S: too many colons #\\: (~D).~@
             At most a single colon is permitted separating a symbolic tag~@
             from a sequence of numbers and other identifiers.~%~@
             Please refer to the documentation of MK4:PARSE-VERSION-STRING~@
             for more information."
	    version-string
	    (count #\: version-string))
    ;; Hacking the readtable. May be an overkill.
    (let ((*readtable* *parse-version-readtable*))
      (setf (readtable-case *readtable*) case)
      (let* ((s version-string)
	     (colon-pos (position #\: s))
	     (start-ns-tags (if colon-pos (1+ colon-pos) 0))
	     (version-tag (unless (zerop start-ns-tags)
			    (read-from-string (subseq s 0 colon-pos))))
	     (number-tags-strings
	      (split-sequence:split-sequence #\. s
					     :start start-ns-tags
					     :remove-empty-subseqs t))
	     (number-tags (mapcar #'parse-version-number-tag
				  number-tags-strings))
	     )
	(make-version :symbolic-tag version-tag
		      :numbers-tags number-tags)))))



(defun print-version (version-structure stream level)
  (if (or (zerop level) (null *print-level*) (< level *print-level*))
      (let ((vmn (version-major-number-tag version-structure)))
	(print-unreadable-object (version-structure stream)
             (format stream "VERSION ~@[~A:~]~:[none~;~:*~A~]~{.~A~}~:[~; ~S~]"
		     (version-symbolic-tag version-structure)
		     (cond ((eql vmn :wild) "*")
			   ((eql vmn :wild-inferiors) "**")
			   (t vmn))
		     (substitute "*" :wild
				 (substitute "**" :wild-inferiors
					     (rest (version-numbers-tags
						    version-structure))))
		     (string/= "" (version-documentation version-structure))
		     (version-documentation version-structure))))
      (prin1 version-structure stream)))		 


;;; version= -- Note. Support for :WILD and :WILD-INFERIORS still not in.
;;; Equality test on symbolic tag is done using STRING-EQUAL, i.e.
;;; symbols are not checked for packages.

(defun version= (v1 v2)
  (declare (type version v1 v2))
  (or (eq v1 v2)
      (and (string-equal (version-symbolic-tag v1) (version-symbolic-tag v2))
	   (= (length (version-numbers-tags v1))
	      (length (version-numbers-tags v2)))
	   (every (lambda (nt1 nt2)
		    (typecase nt1
		      (number (and (numberp nt2) (= nt1 nt2)))
		      ((or string symbol) (and (or (symbolp nt2) (stringp nt2))
					       (string-equal nt1 nt2)))))
		  (version-numbers-tags v1)
		  (version-numbers-tags v2)))))


;;; list< -- Helper function.

(defun list< (l1 l2 &optional (previous nil))
  "Returns T if L1 is lexicographically smaller than L2.
L1 and L2 are assumed to be lists of numbers, strings or symbols."
  ;; Of course this could be generalized to SEQUENCEs etc. etc.
  ;; The following type would apply if we had it.
  ;; (declare (type (list (or number string symbol)) l1 l2))
  (flet ((test< (le1 le2)
	   (typecase le1
	     (number (and (numberp le2) (< le1 le2)))
	     ((or string symbol) (and  (or (stringp le2) (symbolp le2))
				       (string-lessp le1 le2)))
	     ))
	 (test> (le1 le2)
	   (typecase le1
	     (number (and (numberp le2) (> le1 le2)))
	     ((or string symbol) (and (or (stringp le2) (symbolp le2))
				      (string-greaterp le1 le2)))
	     ))
	 )
    (cond ((null l1)
	   (if (null l2)
	       previous
	       t))
	  ((null l2) previous)
	  (t
	   (if (test> (first l1) (first l2))
	       nil
	       (list< (rest l1) (rest l2)
		      (or previous (test< (first l1) (first l2))))
	       )))
    ))


(defun version< (v1 v2)
  (declare (type version v1 v2))
  (and (eql (version-symbolic-tag v1) (version-symbolic-tag v2))
       (list< (version-numbers-tags v1)
	      (version-numbers-tags v2))))
		     
       

(defun version<= (v1 v2)
  (declare (type version v1 v2))
  (or (version< v1 v2)
      (version= v1 v2)))

(defun version> (v1 v2)
  (declare (type version v1 v2))
  (not (version<= v1 v2)))

(defun version>= (v1 v2)
  (not (version< v1 v2)))
       

;;; end of file -- versions.lisp --
