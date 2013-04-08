;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-
;;;
;;;
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
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "PICTURES")


(export '(
	  family-name
	  make-font-family
	  find-font-family
	  find-font
	  find-family-members
	  default-family-name 
	  )
	'pictures)

;font families utilites 
(DEFPARAMETER default-family-name "-adobe-helvetica-medium-r-normal-")

(DEFCONSTANT minimum-font-attributes-in-a-name 1)

(DEFMACRO font-size (font)
  `(+ (font-ascent ,font)(font-descent ,font)))

(DEFMACRO x11-font-string-p (font-name)
  `(AND  (STRINGP ,font-name)
	(> (COUNT #\- ,font-name) minimum-font-attributes-in-a-name))
      )
 
(defclass font-family ()
  (
   (family-name       :type    string 
		      :accessor family-name
		      :initarg :family-name
		      :documentation  "The name for a family of fonts ")

   (family-members      :type  list
		      :initarg :family-members
		      :documentation  "An assoc list of the member fonts and their sizes for a font-family")
   )
  )

(DEFMETHOD print-object ((font font) stream )
  (FORMAT stream "~a" (font-name font)))

(DEFMACRO x11-font-sizes (font-string display)
     `(MAPCAR #'(lambda (font-name)
		  (READ-FROM-STRING (font-name-attribute font-name :size :size)))
	      (list-font-names ,display (CONCATENATE 'string  ,font-string "*" )))
    )


(DEFUN add-size (name size)
  (IF (x11-font-string-p name)
      (CONCATENATE 'string "-" (font-name-attribute name :company :blank) 
		   (FORMAT nil "-~A" size) "-" 
		   (OR (font-name-attribute :resolution :rest) "*"))
      (CONCATENATE 'string name size)))


(defmacro font-objects (members display)
  "determine if members is nil, a string or a list of (font-name size) and return nil or a list of (font-name size)"
  `(COND
     
     ((AND ,members (LISTP (FIRST ,members)))
      (MAPCAR #'(lambda (font-list)
		  (LIST (open-font ,display 
				   (CONCATENATE 'string (FIRST font-list) "*"))
			(SECOND font-list))) ,members))
     ((AND (STRINGP (FIRST ,members))
	   (x11-font-string-p (FIRST ,members))
	   (EQL (LENGTH ,members) 1))
      (mapcar #'(lambda (size)
		  (LIST
		   (open-font display (add-size (FIRST ,members) size)) size))
	      (SORT (x11-font-sizes (first ,members) ,display) '<)))
     ((STRINGP (FIRST ,members))
      
      (get-size-font-pairs (mapcar #'(lambda (string)
				       (open-font display string))
				   (list-ft-names ,members ,display)))))
     )

(DEFUN list-ft-names (names display &optional font-names)
  (DOLIST (name names)
  (IF (x11-font-string-p name)
      (SETF font-names
	    (APPEND font-names
		    (list-font-names display (CONCATENATE 'string name  "*"))))
      (SETF font-names (APPEND font-names (LIST name)))))
  font-names)



(DEFUN get-size-font-pairs (font-list)
  "   given a font-list sort it according to font size and return a
   list of '(fonts size)"

  (LET* ((font-size-pairs
	  (MAPCAR #'(lambda (font)
		      (LIST
		       (IF (x11-font-string-p (font-name font))
			   (READ-FROM-STRING (font-name-attribute
					      (font-name font) :size :size))
			   (font-size font))
		       font)) font-list))
	 (sorted-sizes (REMOVE-DUPLICATES
			 (SORT (MAPCAR #'(lambda (pairs)
					   (FIRST pairs)) font-size-pairs) #'<))))
    (MAPCAR #'(lambda (size)
		(NREVERSE (ASSOC size font-size-pairs))) sorted-sizes)))

	
(DEFUN make-font-family (family-name display &rest members)
  (DECLARE (LIST members))
  (funcall #'make-instance 'font-family
	   :display display
	   :family-name family-name
	   :family-members
	   (OR
	     
	     (font-objects members display)
	     (IF (x11-font-string-p family-name)
		 (mapcar
		  #'(lambda (x)
		      (LIST (open-font display (add-size family-name x)) x))
		  (SORT (x11-font-sizes family-name display) '<))
		 (mapcar
		  #'(lambda (x)
		      (LIST
		       (open-font display (add-size default-family-name x)) x))
		  (SORT (x11-font-sizes default-family-name  display) '<))))))
#+original
(DEFMETHOD initialize-instance
	   :around ((font-family font-family) &key family-name display)
  (LET ((font-families (GETF (display-plist display) 'font-families)))
    (SETF font-families
	  (REMOVE (ASSOC family-name font-families :test #'STRING-EQUAL)
		  font-families :test #'EQUAL))
    (SETF (GETF (display-plist display) 'font-families)
	  (APPEND font-families (LIST (LIST family-name (call-next-method)))))))

(DEFMETHOD initialize-instance
	   :around ((font-family font-family) &key family-name display)
  (LET ((font-families (GETF (display-plist display) 'font-families)))
    (SETF font-families
	  (REMOVE (ASSOC family-name font-families :test #'STRING-EQUAL)
		  font-families :test #'EQUAL))
    (call-next-method)
    (SETF (GETF (display-plist display) 'font-families)
	  (APPEND font-families (LIST (LIST family-name font-family))))))

(DEFUN find-font-family (family-name display)
  "Returns the font-family of the FAMILY-NAME for the DISPLAY"
  (SECOND (ASSOC family-name 
		 (GETF (display-plist display) 'font-families)
		 :test #'STRING-EQUAL ))
  )

(DEFSETF  find-font-family (family-name display) (font-family)
  `(LET ((font-families (GETF (display-plist ,display) 'font-families)))
	 (SETF font-families
	       (REMOVE (ASSOC ,family-name font-families :test #'STRING-EQUAL)
		     font-families :test #'EQUAL))
       (SETF (GETF (display-plist ,display) 'font-families)
	     (APPEND font-families (LIST (LIST ,family-name ,font-family))))
       ,font-family))

(DEFMACRO the-font-size (MEMBER)
  `(SECOND ,member))

(DEFMACRO the-font (MEMBER)
  `(FIRST ,member))

(DEFMETHOD find-font ((family font-family)  size)
  (with-slots (family-members) family
    (WHEN family-members
    (IF  (< size (the-font-size (FIRST family-members)))
	 (the-font (FIRST family-members))
	 
	 (DO* ((font-pair (FIRST family-members) next-pair)
	       (font-size (the-font-size font-pair) next-font-size)
	       (next-font-pairs (REST family-members) (REST next-font-pairs))
	       (next-pair (FIRST  next-font-pairs) (FIRST next-font-pairs))
	       (next-font-size (the-font-size  next-pair)
			       (the-font-size next-pair))
	       )
	      ((not next-pair ) (the-font font-pair))
	   (WHEN (= font-size size) (RETURN (the-font font-pair)))
	   (WHEN (< font-size size next-font-size)
	     (RETURN (the-font font-pair)
	       )))))))


(DEFMETHOD (SETF find-font ) ( font (family font-family) size )
  (with-slots (family-members) family
    (WHEN (< size (SECOND (FIRST family-members))) (FIRST (FIRST family-members)))
    (DO* ((pairs 1 (1+ pairs))
	  (font-pair (FIRST family-members) next-pair)
	  (font-size (SECOND font-pair) next-font-size)
	  (next-font-pairs (CDR family-members) (CDR next-font-pairs))
	  (next-pair (FIRST  next-font-pairs) (FIRST next-font-pairs))
	  (next-font-size (SECOND next-pair) (SECOND next-pair))
	  )
	 ((NOT next-pair )
	  (IF  (= font-size size)
	       (SETF (FIRST font-pair) font) 
	       (SETF family-members 
		     (APPEND family-members (LIST (LIST font size)))))
	  (RETURN font))
      (WHEN (= font-size size)
	(SETF (FIRST font-pair) font) (RETURN font))
      (WHEN (< font-size size next-font-size)
	(SETF family-members
	      (APPEND (BUTLAST family-members (- (LENGTH family-members) pairs))
					(LIST (LIST font size))
					(NTHCDR pairs family-members)))
	(RETURN font)))))




(DEFMETHOD font-family-members ((family font-family) display)
  (DECLARE (IGNORE display))
  (slot-value family 'family-members)
  )

 
(DEFMETHOD (SETF font-family-members) ( members (family font-family) display)
  (IF members
      (SETF (slot-value family 'family-members) (font-objects members display))
      (ERROR  "~%the value for members is  ~a ~%" members)))

;get the attributes from the X11 font name

(DEFCONSTANT large-number 1000)

(DEFUN dash-positions (STRING &optional position-list)
  (IF (> (OR (FIRST (LAST position-list)) 0) (LENGTH string))
      (BUTLAST position-list)
      (dash-positions
       string
       (APPEND position-list
	       (LIST (OR
		      (POSITION #\- string
				:start (1+ (OR (FIRST (LAST position-list)) -1)))
		      large-number))))))


(DEFUN font-name-attribute (name attribute &optional (name-range nil))
  "Parse the X11 font NAME string and for a ATTRIBUTE and return
   the string representing the ATTRIBUTE.
   ATTRIBUTE is (member :size :company :weight :blank :name :resolution :end)"

  (WHEN (x11-font-string-p name)
    (flet ((elt-safe
	     (sequence index)
	     (when (< index (length sequence))
	       (elt sequence index))))		     
    (LET* ((dash-positions (dash-positions name))
	   (attribute-position
	    (CASE attribute
	      (:size 	   (ELT-SAFE dash-positions 6))
	      (:company    (ELT-SAFE dash-positions 0))
	      (:weight 	   (ELT-SAFE dash-positions 2))
	      (:blank 	   (ELT-SAFE dash-positions 5))
	      (:name 	   (ELT-SAFE dash-positions 1))
	      (:resolution (ELT-SAFE dash-positions 7))
	      ))
	   (name-range-position
	     (CASE name-range
	       (:size       (ELT-SAFE dash-positions 7))
	       (:company    (ELT-SAFE dash-positions 1))
	       (:weight     (ELT-SAFE dash-positions 3))
	       (:blank      (ELT-SAFE dash-positions 6))
	       (:name       (ELT-SAFE dash-positions 2))
	       (:resolution (ELT-SAFE dash-positions 8))
	       (:end        (ELT-SAFE dash-positions 14))
	       (:rest       (LENGTH name))
	       )))
      (SUBSEQ name (1+ (OR attribute-position 0))
	       name-range-position )
      ))))
