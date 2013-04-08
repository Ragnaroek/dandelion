;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package "CLUEI")

(export '(*database*
	   convert
	   define-resources
	   undefine-resources
	   ))

(export '(default-resources ;; Debug aids
	   class-resources
	   describe-resource))

(defvar *database* (make-resource-database))

(defun describe-resource (name &optional (database *database*) (max-levels 32))
  "Describe the entries for NAME in DATA-BASE"
  (let ((name-vector (make-array max-levels))
	(tight-vector (make-array max-levels :element-type 'bit)))
    (labels ((dr-internal (name database level title-p)
	       (when (equal name (string (xlib::resource-database-name database)))
		 (unless title-p
		   (format t "~%Value      Name")
		   (setf title-p t))      
		 (format t "~%~10s (" (xlib::resource-database-value database))
		 (dotimes (i level)
		   (when (plusp i) (princ " "))
		   (when (zerop (aref tight-vector i)) (princ "* "))
		   (princ (aref name-vector i)))
		 (princ ")"))
	       (when (xlib::resource-database-tight database)
		 (dolist (tight (xlib::resource-database-tight database))
		   (setf (aref name-vector level) (xlib::resource-database-name tight)
			 (aref tight-vector level) 1)
		   (setf title-p (or (dr-internal name tight (1+ level) title-p) title-p))))
	       (when (xlib::resource-database-loose database)
		 (dolist (loose (xlib::resource-database-loose database))
		   (setf (aref name-vector level) (xlib::resource-database-name loose)
			 (aref tight-vector level) 0)
		   (setf title-p (or (dr-internal name loose (1+ level) title-p) title-p))))
	       title-p))
      (dr-internal
	(if (symbolp name) (symbol-name name) (string-upcase name))
	database
	0
	nil)
      (values))))

(defun default-resources (contact resource-class &optional resource-name)
  "Return the plist of default resources for RESOURCE-CLASS on CONTACT.
   If CONTACT is a string, its taken as a host name."
  (if (stringp contact)
      (let ((display (open-contact-display 'default-resources :host contact)))
	(unwind-protect
	    (default-resources (display-root display) resource-class resource-name)
	  (close-display display)))
      
      (let* ((contact-class (class-name (class-of contact)))
	     (class-key     (intern (symbol-name resource-class) 'keyword))
	     (name-key      (when resource-name (intern (symbol-name resource-name) 'keyword)))

	     ;; Find resource(s) that match given RESOURCE-CLASS and RESOURCE-NAME
	     (resources     (delete
			      nil
			      (append (class-resources   contact-class t)
				      (class-constraints contact-class t))
			      :key #'(lambda(r)
				       (let ((rclass (getf (rest r) :class))
					     (rname  (first r)))
					 (and
					   (eq (or rclass rname) class-key)
					   (or (null resource-name)
					       (eq rname name-key))))))))
	
	(when resources
	  (get-resources
	    nil
	    resources
	    contact
	    (append (contact-complete-name contact)  (list (or resource-name resource-class)))
	    (append (contact-complete-class contact) (list resource-class)))))))

(defun class-resources (class &optional full-p)
  "Return the resource list for CLASS.
  When full-p return the full alist."
  (let ((resources (clue-resources class)))
    (unless resources
      (error "~s isn't a CLUE class." class))
    (if full-p
	resources
      (mapcar #'car resources))))

(defun get-resources (arglist resources parent full-name full-class)	
  ;; Useful for making init-plists for contacts
  ;; arglist   Specifies  the  ArgList  to   override   resources
  ;;	   obtained from the resource database.
  ;; parent Specifies the  parent contact
  ;; full-name Specifies the name of this contact (may be overrid-
  ;;	   den by the arglist).
  ;; full-class Specifies the class of this contact.
  
  (declare (type list arglist)
	   (type contact parent)
	   (type list full-name full-class))
  
  (do* ((table (get-search-table *database* full-name full-class))
	(resources resources (cdr resources))
	(resource (caar resources) (caar resources))
	(value-type nil)
	(arg nil)
	(result nil))

       ((endp resources) result)
    
    (setq value-type (getf (cdar resources) :type))
    (if (setq arg (getf arglist resource))

	(when value-type
	  (let ((carg (convert parent arg value-type)))
	    (if (or carg (null arg))
		(setq arg carg)
		(error "The ~s initialization is ~s which isn't type ~s"
		       resource arg value-type))))
	
	(let ((value (get-search-resource table resource (getf (cdar resources) :class resource)))
	      (db nil))
	  
	  (if value

	      ;; Resource in the database
	      (when (and (setq arg value) value-type)
		(let ((carg (convert parent value value-type)))
		  (if (or carg (null arg))
		      (setq arg carg)
		      (error "The resource value for ~s is ~s which isn't type ~s"
			     (reverse (cons resource db)) value value-type))))
	      
	      ;; Resource NOT in the database
	      (let ((initform (getf (cdar resources) :initform)))
		(when initform			; Resource has an initform
		  (setq arg (eval initform))	;************ EVAL ALERT *********
		  (when value-type
		    (let ((carg (convert parent arg value-type)))
		      (if (or carg (null arg))
			  (setq arg carg)
			  (error "The ~s initialization has :initform ~s which evaluates to ~s which isn't type ~s"
				 resource initform arg value-type)))))))))
    (when arg
      (setq result (list* resource arg result)))))



(defun resource (contact name)
  "Lookup resource NAME for CONTACT"
  (check-type name symbol)
  (getf (slot-value (the contact contact) 'initialization)
	(intern (symbol-name name) 'keyword)))

(defun get-clue-resource-internal (contact name class)
  (let ((initialization (slot-value (the contact contact) 'initialization)))
    (or (getf initialization name)
	(get-search-resource (second initialization) name (or class name)))))

#+explorer
(defgeneric convert (contact value type)
  ;; This :argument-precedence-order makes things more efficient.
  (:argument-precedence-order type contact value))

;; The default method
(defmethod convert (contact value (type t))
  "Convert VALUE to TYPE"
  (cond
    ((and (consp type) (eq (car type) 'or))		 ; OR type -- use the first conversion that works
     (dolist (typ (cdr type))				 
       (if (eq typ 'null)
	   (when (null value) (return nil))
	   (let ((result (convert contact value typ)))
	     (when result
	       (return result))))))
    
    ((and (consp type) (eq (car type) 'member))		 ; MEMBER type
     (unless (keywordp value)
       (setq value (convert contact value 'keyword)))
     (and (member value (cdr type) :test #'eq) value))
    
    ((typep value type) value)				 ; If type works, use it!
    
    ((or (stringp value) (symbolp value))		 ; Last resort, try read-from-string
     (let ((value (string value))
	   (*read-base* 10.))
       (let ((result (ignore-errors (read-from-string value))))
	 (and result (typep result type) result))))
    (t nil)))

(defmethod convert (contact value (type (eql 'keyword)))
  (declare (ignore contact))
  (typecase value
    (keyword value)
    (symbol (intern (symbol-name value) 'keyword))
    (string
     (unless (position #\space (the string value))
       (intern (string-upcase value) 'keyword)))
    (otherwise nil)))

(defmethod convert (contact value (type (eql 'pixel)))
  (typecase value

    (stringable	
     (when (symbolp value) (setq value (symbol-name value)))
     (let ((screen (contact-screen contact)))
       (cond
	 ((equalp value "WHITE")
	  (screen-white-pixel screen))
	 
	 ((equalp value "BLACK")
	  (screen-black-pixel screen))
	 
	 (t
	  (let ((cache (getf (screen-plist screen) :color-cache)))
	    ;; Pixel already found for this color name?
	    (or
	      ;; Yes, return cached pixel.
	      (rest (assoc value cache :test #'equalp))
	      
	      ;; No, allocate pixel for color name.
	      (let*
		((color (convert contact value 'color))
		 (pixel (when color (convert contact color 'pixel))))

		(when pixel
		  ;; Add pixel to color name cache.
		  (setf (getf (screen-plist screen) :color-cache)
			(cons (cons value pixel) cache))
		  pixel))))))))
    (color
     (ignore-errors
       (alloc-color (screen-default-colormap (contact-screen contact)) value)))
    
    (pixel value)
    (otherwise nil)))

(defmethod convert (contact value (type (eql 'color)))
  (typecase value
    (stringable
     (ignore-errors
       (lookup-color (screen-default-colormap (contact-screen contact)) value)))
    (color value)
    (otherwise nil)))

(defmethod convert (contact value (type (eql 'font)))
  (typecase value
    (stringable
     (ignore-errors (open-font (contact-display contact) value)))
    (font value)
    (otherwise nil)))
     
(defmethod convert (contact value (type (eql 'pixmap)))
  (flet
    ((find-pixmap (contact image) 
       (let ((drawable (if (realized-p contact) contact (contact-root contact)))
             (screen (contact-screen contact)))
	 (cond
	   ((= (image-depth image) (contact-depth contact))
	    (contact-image-pixmap drawable image))
	   ((= (image-depth image) 1)
	    (contact-image-mask drawable image 
	                        :foreground (screen-white-pixel screen)
				:background (screen-black-pixel screen)))))))
    (typecase value    
      (stringable
       (let ((image (stringable-value value 'image)))
	 (when image (find-pixmap contact image))))
      
      ((or (rational 0 1) (float 0.0 1.0))
       (let ((gray (svref '#(0%gray  6%gray  12%gray 25%gray 37%gray 50%gray
			     62%gray 75%gray 88%gray 93%gray 100%gray)
			  (round (* value 10)))))
	 (and gray (boundp gray) (find-pixmap contact (symbol-value gray)))))
      
      (image     (find-pixmap contact value))
      (pixmap    value)
      (otherwise nil))))

(defmethod convert (contact value (type (eql 'image)))
  (declare (ignore contact))  
  (typecase value
    (stringable (stringable-value value 'image))
    (image      value)
    (otherwise  nil)))

(defun stringable-value (stringable type)
  (let ((symbol (if (symbolp stringable)
		    stringable
		    (let ((*package* (find-package "CLUE")))
		      (ignore-errors (read-from-string stringable))))))
    (when (and (symbolp symbol) (boundp symbol))
      (values
	(when (typep (symbol-value symbol) type) (symbol-value symbol))
	symbol))))

(defmethod convert (contact value (type (eql 'cursor)))  
  (typecase value
    (card8      (contact-glyph-cursor contact value))
    (stringable (let ((value (stringable-value value '(or image card8))))
		  (when value (convert contact value type))))
    (image      (let* ((image-name (image-name value))
		       (mask-name  (intern (format nil "~a-MASK" image-name)
					   (symbol-package image-name))))
		  (contact-image-cursor
		    contact value
		    :mask (when (and (boundp mask-name)
				     (typep (symbol-value mask-name) '(or pixmap image)))
			    (symbol-value mask-name)))))
    (cursor     value)
    (otherwise  nil)))

(defmethod convert (contact value (type (eql 'boolean)))
  (declare (ignore contact))
  value)

(defmethod convert (contact value (type (eql 'stringable)))
  (declare (ignore contact))
  (typecase value
    (stringable value)
    (otherwise
     (princ-to-string value))))

(defmethod convert (contact value (type (eql 'string)))
  (declare (ignore contact))
  (typecase value
    (string value)
    (symbol (symbol-name value))
    (otherwise
     (princ-to-string value))))

(defmethod convert (contact value (type (eql 'mask32)))
  (declare (ignore contact))
  (typecase value
    (mask32     value)
    (list       (ignore-errors (apply #'make-event-mask value)))
    (otherwise  nil)))

(defmacro define-resources (&body name-value-pairs)
  "Sugar coating for xlib:add-resource"
  `(progn ,@(do* ((name-values name-value-pairs (cddr name-values))
		  (result nil))
		 ((endp name-values) (nreverse result))
	      (push `(add-resource *database* ',(first name-values) ,(second name-values)) result))))

(defmacro undefine-resources (&body name-value-pairs)
  "Sugar coating for xlib:delete-resource"
  `(progn ,@(do* ((name-values name-value-pairs (cddr name-values))
		  (result nil))
		 ((endp name-values) (nreverse result))
	      (push `(delete-resource *database* ',(first name-values)) result))))


