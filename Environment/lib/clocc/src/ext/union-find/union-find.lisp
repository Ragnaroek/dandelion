;;; -*- Mode: CLtL -*-

;;; union-find.lisp --
;;; Simple implementation of the well known UNION-FIND data structure
;;; (with weighted path compression).
;;; See file README for more info.
;;;
;;; Author: Marco Antoniotti
;;;
;;; Copyright (c) 2000 Marco Antoniotti. All rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(unless (find-package "CL.UTIL.UNION-FIND")
  (load "union-find-pkg"))

(in-package "CL.UTIL.UNION-FIND")

(defstruct (partition (:constructor %make-partition))
  "The Union-Find Partion Structure.
The Partition structure serves as a collector of various auxiliary
data involved in the costruction and maintainance of disjoint sets
(represented as a forest of trees)."
  (element-sets-map () :type (or null hash-table)))



(defvar *accepted-equality-tests* (list #'eq #'eql #'equal #'equalp))

(defun make-partition (&key (test #'equal))
  "The constructor for a Partition.
The constructor takes a sole keyword parameter :TEST (default to
#'EQUAL), which is used to map elements to sets (i.e. forests) in the
partition.  The accepted values are #'EQ, #'EQL, #'EQUAL, and #'EQUALP.
The result it a newly created partition."
  ;; Some error prevention and checking.
  (let ((equality-test (typecase test
			 (function test)
			 (symbol (if (fboundp test)
				     (symbol-function test)
				     test)) ; Let the following code complain. 
			 (t test)))	; As above.
	)
    (restart-case
     (unless (member equality-test *accepted-equality-tests*)
       (error "UNION FIND: equality test ~S not supported. ~
               Must be one of EQ, EQL, EQUAL, EQUALP."
	      equality-test))
     (use-value ()
        :report (lambda (stream)
		  (format stream "Use EQ as equality test."))
        (setf equality-test #'eq))
     (use-value ()
        :report (lambda (stream)
		  (format stream "Use EQL as equality test."))
        (setf equality-test #'eql))
     (use-value ()
        :report (lambda (stream)
		  (format stream "Use EQUAL as equality test."))
        (setf equality-test #'equal))
     (use-value ()
        :report (lambda (stream)
		  (format stream "Use EQUALP as equality test."))
        (setf equality-test #'equalp)))
    (%make-partition :element-sets-map (make-hash-table :test equality-test))))



;;; Set Representatives.

(defstruct (set-representative (:conc-name set-rep-)
			       (:print-function %print-set-representative))
  "The Set Representative Structure.
This structure type is the main building block for the UNION-FIND internal
representation of dijoint sets."
  (item nil)
  (parent nil)
  (rank 0 :type fixnum)
  (partition nil :type (or null partition)))


(defun %print-set-representative (set-rep stream depth)
  (declare (ignore depth))
  (print-unreadable-object (set-rep stream :identity t)
    (format stream
	    "SET-REP {~S} rank: ~D is-root: ~S"
	    (set-rep-item set-rep)
	    (set-rep-rank set-rep)
	    (eq (set-rep-parent set-rep) set-rep))))


(defun make-set (partition x)
  "The public constructon for a Set Representative."
  (declare (type partition partition))
  (let ((x-set-rep (find-set partition x)))	; This is *not* CLR's FIND-SET.
    (if x-set-rep
	x-set-rep
	(let ((new-set-rep (make-set-representative :item x
						    :partition partition)))
	  (setf (set-rep-parent new-set-rep) new-set-rep)
	  (setf (gethash x
			 (partition-element-sets-map partition))
		new-set-rep)
	  (setf (set-rep-partition new-set-rep) partition)
	  new-set-rep))))


(defun union (partition x y)
  "The main UNION-FIND operator.
Note that this operator 'shadows' CL:UNION."
  (declare (type set-representative x y)
	   (type partition partition))
  (link (find-set-rep partition x) (find-set-rep partition y)))


(defun link (x y)
  (declare (type set-representative x y))
  (cond ((> (set-rep-rank x) (set-rep-rank y))
	 (setf (set-rep-parent y) x))
	(t
	 (setf (set-rep-parent x) y)
	 (when (= (set-rep-rank x) (set-rep-rank y))
	   (incf (set-rep-rank y))))))


(defun find-set-rep (partition x)	; This is CLR FIND-SET.
  "The traditional FIND operator of the UNION-FIND data structure."
  (declare (ignorable partition))	; This is done for
					; symmetry. All exported
					; function take a 'partition'
					; as parameter.
  (declare (type set-representative x))
  (unless (eq x (set-rep-parent x))
    (setf (set-rep-parent x) (find-set-rep partition (set-rep-parent x))))
  (set-rep-parent x))


(defun find-set (partition x)		; This instead keeps track of sets.
  "A more 'high-level' FIND operator which uses the actual set element X."
  (declare (type partition partition))
  (let ((set-cell (gethash x (partition-element-sets-map partition))))
    (when set-cell
      (find-set-rep partition set-cell))))


(defgeneric collect-set (partition x)
  (:documentation
   "This function collects all the elements of the set which contains X.
The result is a list.  Use this function with care."))

(defmethod collect-set ((p partition) x)
  (collect-set p (find-set p x)))
  
(defmethod collect-set ((p partition) (x set-representative))
  ;; Very stupid for the time being. Just looping through the hash table.
  (loop with x-set-rep = (find-set-rep p x)
	for set-rep being the hash-value
	    of (partition-element-sets-map p)
	    using (hash-key item)
	when (eq (find-set-rep p set-rep) x-set-rep)
	  collect item))


(defun print-set (partition x &optional (stream *standard-output*))
  "This is a utility function that PRINTS the result of COLLECT-SET on X."
  (declare (type set-representative x)
	   (type partition partition))
  (print (collect-set partition x) stream))
  

;;; end of file -- union-find.lisp --
