;;; -*- package: pcl -*-
(in-package :pcl)



;;; pw-- enhanced (11/96) to derive slot types from slot definition
;;; if class name is available via (the class instance). Note this
;;; may cause problems if the class layout changes and the forms that
;;; use this trick are not re-compiled. Maybe this isn't a good idea.

(defmacro with-slots (slots instance &body body)
"The macro WITH-SLOTS establishes a lexical environment for referring
to the SLOTS in the INSTANCE named by the given slot-names as though they
were variables. Within such a context the value of the slot can be
specified by using its slot name, as if it were a lexically bound variable.
Both setf and setq can be used to set the value of the slot.

The macro with-slots translates an appearance of the slot name as a
variable into a call to slot-value.
"
  (flet ((find-slot-type (class slot-name)
	   (let ((slotd (find-slot-definition class slot-name)))
	     (if slotd
		 (slot-definition-type slotd)
		 (warn "Slot ~a not defined in class ~a."
		       slot-name (class-name class))))))
    (let ((in (gensym))
	  ;; Use any available hints to derive the slot types.
	  ;; Slot-value on defstruct objects are already optimized
	  ;; so don't mess with them. I key on (the foo x).
	  (class
	   (and (consp instance)
		(eq (car instance) 'the)
		(symbolp (second instance))
		(let ((class (find-class (second instance))))
		  (and (not (structure-class-p class)) class)))))
      `(let ((,in ,instance))
	 #+cmu (declare (ignorable ,in))
	 ,@(let ((instance (if (and (consp instance) (eq (car instance) 'the))
			       (third instance)
			       instance)))
	     (and (symbolp instance)
		  `((declare (variable-rebinding ,in ,instance)))))
	 ,in
	 (symbol-macrolet
	  ,(mapcar #'(lambda (slot-entry)
		       (let* ((variable-name 
			       (if (symbolp slot-entry)
				   slot-entry
				   (car slot-entry)))
			      (slot-name
			       (if (symbolp slot-entry)
				   slot-entry
				   (cadr slot-entry)))
			      (slot-type
			       (and class
				    (find-slot-type class slot-name))))
			 `(,variable-name
			   ,(if slot-type
				`(the ,slot-type 
				      (slot-value ,in ',slot-name))
				`(slot-value ,in ',slot-name)))))
		   slots)
	  ,@body)))))
