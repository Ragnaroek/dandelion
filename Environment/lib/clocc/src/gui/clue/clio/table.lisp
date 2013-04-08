;;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                                AUSTIN, TEXAS 78714                               |
;;;                                                                                  |
;;;             Copyright (C) 1989, 1990 Texas Instruments Incorporated.             |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

(in-package "CLIO-OPEN")


(export '(
	  table
	  make-table
	  table-column-alignment
	  table-column-width
	  table-columns
	  table-delete-policy
	  table-layout-size-policy
	  table-member
	  table-row-alignment
	  table-row-height
	  table-same-height-in-row
	  table-same-width-in-column
	  table-separator
	  table-row
	  table-column
	  )
	'clio-open)

;;;
;;;  Call-Tree...
;;;


;;;   Preferred-Size (Table)
;;;   .  check-for-existing-wis
;;;   .  place-children-physically
;;;   .  .  put-kids-into-maximum-unaligned-columns
;;;   .  .  .  find-first-parents-width 
;;;   .  .  .  assign-kids-to-rows-and-columns
;;;   .  .  .  preferred-size (child)
;;;   .  .  .  move (child)
;;;   .  .  .  resize (child)
;;;   .  .  put-kids-into-maximum-aligned-columns
;;;   .  .  .  assign-kids-to-rows-and-columns
;;;   .  .  .  .  assign-a-kid-to-a-row-and-column
;;;   .  .  .  .  build-sorted-list-of-children
;;;   .  .  .  get-maximum-possible-ncolumns
;;;   .  .  .  .  preferred-size (child)
;;;   .  .  .  preferred-size (child)
;;;   .  .  .  adjust-column-widths-so-child-fits
;;;   .  .  put-kids-into-specified-number-of-columns   
;;;   .  .  .  assign-kids-to-rows-and-columns
;;;   .  .  .  preferred-size (child)
;;;   .  .  scan-for-largest-children
;;;   .  .  .  preferred-size (child)
;;;   .  .  determine-a-rows-height
;;;   .  .  preferred-size (child)
;;;   .  .  move (child)
;;;   .  .  resize (child)
;;;   .  .  calculate-preferred-height
;;;   .  .  determine-a-rows-height
;;;   .  .  .  preferred-size (child)
;;;   .  .  calculate-preferred-width
;;;   .
;;;   Change-Layout(Table)
;;;   .  check-for-existing-wis
;;;   .  place-children-physically
;;;   .  change-geometry (Table)
;;;   .
;;;   Resize :after (Table)
;;;   .  change-layout (Table)
;;;   .
;;;   Manage-Geometry (Table)
;;;   .  Change-Geometry (Table)

;;;  Basic Organization and Flow:
;;;	The Table contact lays out its children per the values of its policy resources and the
;;;	row/column constraints of its children, with the resource values always taking precedence
;;;	over the children's constraint values.  
;;;
;;;	The function place-children-physically does the real work of Table.  
;;;
;;;	The differences in Table's logical flow for the possible values for the :columns resource
;;;	are embodied primarily in the three routines
;;;
;;;		put-kids-into-maximum-unaligned-columns
;;;		put-kids-into-maximum-aligned-columns
;;;		put-kids-into-specified-number-of-columns
;;;
;;;	There are 5 ways into the Table contact's logic:
;;;
;;;		Preferred-Size (Table)
;;;		Change-Layout (Table)
;;;		Resize :after (Table)
;;;		Manage-Geometry (Table)
;;;		(SETF layout-policy-resource)
;;;


;;;  ===========================================================================
;;;		T h e   T A B L E   L a y o u t   C o n t a c t 
;;;  ===========================================================================

(DEFCONTACT table (gravity-mixin spacing-mixin core composite)
  ((column-alignment	:type		(MEMBER :left :center :right)
			:reader  	table-column-alignment	; SETF method defined below.
			:initarg	:column-alignment
			:initform	:left)
   
   (column-width	:type		(OR (MEMBER :maximum) cons (integer 1 *))
			:reader  	table-column-width	; SETF method defined below.
			:initarg	:column-width
			:initform	:maximum)
   
   (columns		:type		(OR (integer 1 *) (MEMBER :maximum :none))
			:reader  	table-columns	        ; SETF method defined below.
			:initarg	:columns
			:initform	:maximum)

   (delete-policy	:type	      (MEMBER :shrink-list :shrink-column :shrink-none :shrink-row)
			:reader  	table-delete-policy	; SETF method defined below.
			:initarg	:delete-policy
			:initform	:shrink-none)

   (layout-size-policy	:type		(MEMBER :maximum :minimum :none)
			:reader  	table-layout-size-policy ; SETF method defined below.
			:initarg	layout-size-policy
			:initform	:maximum)

   (row-height		:type		(OR (MEMBER :maximum) cons (integer 1 *))
			:reader  	table-row-height	; SETF method defined below.
			:initarg	:row-height
			:initform	:maximum)

   (row-alignment	:type		(MEMBER :top :center :bottom)
			:reader  	table-row-alignment	; SETF method defined below.
			:initarg	:row-alignment
			:initform	:bottom)

   (same-height-in-row
     			:type		(MEMBER :on :off)
			:reader  	table-same-height-in-row ; SETF method defined below.
			:initarg	:same-height-in-row
			:initform       :off)

   (same-width-in-column
     			:type		(MEMBER :on :off)
			:reader  	table-same-width-in-column ; SETF method defined below.
			:initarg	:same-width-in-column
			:initform	:off)

   (separators		:type		list
			:initarg	:separators
			:initform	nil))

  (:resources
    (border-width :initform 0)
    column-alignment
    column-width
    columns
    delete-policy
    layout-size-policy
    row-alignment    
    row-height
    same-height-in-row
    same-width-in-column
    separators)


  (:constraints
    (row 		:type 		(integer 0 *))
    (column 		:type 		(integer 0 *)))


  (:documentation
    "Arranges its children in an array of rows and columns."
    ))



(DEFUN make-table (&rest initargs &key &allow-other-keys)
  (APPLY #'make-contact 'table initargs))

;;;  ===========================================================================  ;;; 
;;;	      ORG-ENTRY: the entries on the what-if-organization list		  ;;;
;;;  ===========================================================================  ;;;

(DEFSTRUCT (org-entry :named (:type vector) (:conc-name "ORG-ENTRY-"))
  kid
  row
  column
  width
  height
  border-width)

(DEFUN establish-org-entry (kid row column)
  (MULTIPLE-VALUE-BIND (p-w p-h p-b-w)
      (preferred-size kid)
    (make-org-entry :kid kid :row row :column column
		    :width p-w :height p-h :border-width p-b-w)))


;;;  ===========================================================================  ;;; 
;;;		       What-if Structures and Their management			  ;;;
;;;  ===========================================================================  ;;;

;;;
;;;   Structures of this kind are placed on the Table's plist under the :what-if-structures
;;;   property to record already-performed preferred-size calculations for the current set of
;;;   policy resource values but different widths/heights.  Any change to a policy resource
;;;   destroys this cache of what-if structures, as does a call to change-layout.
;;;

;;;   Hmmmm...  We must keep the children's sizes here, have all the layout logic look here
;;;   rather than at the kids' preferred-size methods.  Where to keep this info?  In organization
;;;   (which is already a list of the kids) or in another list of kids, widths, heights, and
;;;   border-widths.  Or in an array...

(DEFSTRUCT (what-if-structure :named (:type vector) (:conc-name "WHAT-IF-"))
  width
  height
  border-width
  organization					; org-entrys for :mapped children only!
  column-widths
  nrows
  ncolumns
  (preferred-width 0)
  (preferred-height 0)
  in-use
  )

(DEFUN check-for-existing-wis (table width height border-width &optional dont-create-p)
  ;;  Returns the first (newest) wis found with width/height.
  ;;  If no wis satisfying width/height exists, create a new one unless DONT-CREATE-P
  ;;  is true, in which case return NIL.
  (LET ((old-wis-list (GETF (window-plist table) :what-if-structures)) wis)
    (SETF wis (FIND-IF #'(lambda (wis)
			   (AND (EQL (what-if-width wis) width)
				(EQL (what-if-height wis) height)
				(EQL (what-if-border-width wis) border-width)))
		       old-wis-list))
    (UNLESS (OR wis dont-create-p)
      (SETF (GETF (window-plist table) :what-if-structures)
	    (PUSH (SETF wis (make-what-if-structure :width width
						    :height height
						    :border-width border-width
						    :preferred-width 0
						    :preferred-height 0))
		  old-wis-list))
      )
    wis))


;;;  ===========================================================================  ;;; 
;;;		         A Table's Constraint's Accessors			  ;;;
;;;  ===========================================================================  ;;;


(defun table-row (member)
  (contact-constraint member :row))

(defsetf table-row setf-table-row)
(defun setf-table-row (member row)
  (check-type row (or null (integer 0 *)))
  (setf (contact-constraint member :row) row))

(defun table-column (member)
  (contact-constraint member :column))

(defsetf table-column setf-table-column)
(defun setf-table-column (member column)
  (check-type column (or null (integer 0 *)))
  (setf (contact-constraint member :column) column))




;;;  ===========================================================================  ;;; 
;;;		       SETF functions for a Table's Resources			  ;;;
;;;  ===========================================================================  ;;; 

(defmethod (setf display-left-margin) :after (new-value (table table))
  (declare (ignore new-value))
  (change-layout table))

(defmethod (setf display-right-margin) :after (new-value (table table))
  (declare (ignore new-value))
  (change-layout table))

(defmethod (setf display-top-margin) :after (new-value (table table))
  (declare (ignore new-value))
  (change-layout table))

(defmethod (setf display-bottom-margin) :after (new-value (table table))
  (declare (ignore new-value))
  (change-layout table))



(FLET ((force-relayout (table)
	 (SETF (GETF (window-plist table) :what-if-structures) nil)
	 (change-layout table))
       )

  (DEFMETHOD (SETF display-horizontal-space) :after (new-value (table table))
    (DECLARE (IGNORE new-value))
    (force-relayout table))

  (DEFMETHOD (SETF table-column-alignment) (new-value (table table))
    (with-slots (column-alignment) table
      (SETF column-alignment new-value)
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-column-width) (new-value (table table))
    (with-slots (column-width) table
      (SETF column-width new-value)
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-columns) (new-value (table table))
    (with-slots (columns) table
      (SETF columns new-value)
      (DOLIST (kid (composite-children table))
	(SETF (table-column kid) nil
	      (table-row kid) nil))
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-delete-policy) (new-value (table table))
    (with-slots (delete-policy) table
      (SETF delete-policy new-value)
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-layout-size-policy) (new-value (table table))
    (with-slots (layout-size-policy) table
      (SETF layout-size-policy new-value)
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-row-height) (new-value (table table))
    (with-slots (row-height) table
      (SETF row-height new-value)
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-row-alignment) (new-value (table table))
    (with-slots (row-alignment) table
      (SETF row-alignment new-value)
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-same-width-in-column) (new-value (table table))
    (CHECK-TYPE new-value (MEMBER :on :off))
    (with-slots (same-width-in-column) table
      (SETF same-width-in-column new-value)
      (force-relayout table)
      new-value))
  
  (DEFMETHOD (SETF table-same-height-in-row) (new-value (table table))
    (CHECK-TYPE new-value (MEMBER :on :off))
    (with-slots (same-height-in-row) table
      (SETF same-height-in-row new-value)
      (force-relayout table)
      new-value))
  
  
;;;  ===========================================================================  ;;; 
;;;		         A Table's Separator Methods 			  	  ;;;
;;;  ===========================================================================  ;;;
  
;;;  Note: The physical size of an OL UI separator (white-space) will be defined 
;;;	   to be half the height of the row it follows.
  
  (DEFMETHOD table-separator ((table table) row-number)
    (DECLARE (type integer row-number))
    (check-type row-number (integer 0 *))
    (with-slots (separators) table
      (IF (MEMBER row-number separators) :on :off)))
  
  
  (DEFMETHOD (SETF table-separator) (on-or-off (table table) row-number)
    (DECLARE (type integer row-number))
    (check-type row-number (integer 0 *))
    (with-slots (separators) table
      (LET ((already-there-p (MEMBER row-number separators)))
	(ECASE on-or-off
	  (:on (UNLESS already-there-p
		 (PUSH row-number separators)
		 (force-relayout table)))
	  (:off (WHEN already-there-p
		  (SETF separators (DELETE row-number separators))
		  (force-relayout table))))))	
    on-or-off)
  
  
  
  
;;;  ===========================================================================  ;;; 
;;;		         A Table's Table-Member Method 			  	  ;;;
;;;  ===========================================================================  ;;;
  
  (DEFMETHOD table-member ((table table) row column)
    ;;  Return NIL if there is no child at position row/column.
    (LET ((wis (check-for-existing-wis table (contact-width table) (contact-height table)
				       (contact-border-width table))))
      (WHEN wis
	(org-entry-kid (FIND-IF #'(lambda (x)
				    (AND (= (org-entry-row x) row)
					 (= (org-entry-column x) column)))
				(REST (what-if-organization wis)))))))
  
  (DEFMETHOD (SETF table-member) (new-value (table table) row column)
    ;;  What should we do with the child currently at position row/column?
    ;;  Set its constraints to NIL?  Set just one of its constraints to NIL?
    ;;  Error if there's one there?  I've chosen to blast its constraints.
    (LET ((existing-child-at-that-position (table-member table row column)))
      (WHEN existing-child-at-that-position
	(SETF (table-row existing-child-at-that-position) nil
	      (table-column existing-child-at-that-position) nil))
      (SETF (table-row new-value) row)
      (SETF (table-column new-value) column)
      (force-relayout table)
      new-value))
  )



;;;  ===========================================================================  ;;; 
;;;		         A Table's Preferred-Size Method                          ;;;
;;;  ===========================================================================  ;;; 

(DEFMETHOD preferred-size ((table table) &key width height border-width)

  ;;
  ;;  Handle the case where we have no children...
  ;;
  (with-slots (children) table
    (UNLESS children
      (RETURN-FROM preferred-size
	(VALUES (+ (display-left-margin table) (display-right-margin table))
		(+ (display-top-margin table) (display-bottom-margin table))
		(contact-border-width table)))))
    
  
  (with-slots ((old-width width) (old-height height) (old-border-width border-width)) table    

    ;;
    ;;  When the caller specifies no what-if values and we have a good width & height, always
    ;;  return our current values...
    ;;
    (WHEN (AND (NULL width) (NULL height) (/= 0 old-width) (/= 0 old-height))

      (RETURN-FROM preferred-size (VALUES old-width old-height old-border-width)))

    ;;
    ;;  We need to what-if.  Figure out the width, height, and border-width to use...
    ;;
    (SETF width (OR width old-width)
	  height (OR height old-height)
	  border-width (OR border-width old-border-width))

    
    (LET ((wis (check-for-existing-wis table width height border-width)))

      (UNLESS (AND (what-if-organization wis)
		   (= (what-if-preferred-width wis) width)
		   (= (what-if-preferred-height wis) height))
	(place-children-physically table wis nil))

      (VALUES (what-if-preferred-width wis)
	      (what-if-preferred-height wis)
	      border-width))))


;;;  ===========================================================================  ;;; 
;;;		         A Table's Change-Layout Method                           ;;;
;;;  ===========================================================================  ;;; 

(DEFMETHOD change-layout ((table table) &optional newly-managed)
  (declare (type (or null contact) newly-managed))
  (DECLARE (SPECIAL *called-from-resize-method*))
  
  (with-slots (width height border-width) table
    
    ;;  Just update the current wis if a single child is being withdrawn...
    (when (AND newly-managed (EQ (contact-state newly-managed) :withdrawn))
      (LET ((wis (check-for-existing-wis table width height border-width)))
	(WHEN wis
	  (SETF (REST (what-if-organization wis))
		(DELETE newly-managed (REST (what-if-organization wis))
			:key #'org-entry-kid)))))
    
    (LET (p-width p-height
	  (wis (check-for-existing-wis table width height border-width)))
      ;;  With a change in layout we must really re-layout our children...
      (unless (what-if-in-use wis)
	(SETF (what-if-in-use wis) t)
	(place-children-physically table wis t)
	
	;;
	;;  Update the children's row/column constraints...
	;;
	(DOLIST (o-e (REST (what-if-organization wis)))
	  (SETF (table-row (org-entry-kid o-e)) (org-entry-row o-e)
		(table-column (org-entry-kid o-e)) (org-entry-column o-e)))
	
	(UNLESS (AND (BOUNDP '*called-from-resize-method*) *called-from-resize-method*)
	  (SETF p-width (what-if-preferred-width wis)
		p-height (what-if-preferred-height wis))     
	  
	  (UNLESS (AND (= height p-height) (= width p-width))
	    (SETF (what-if-width wis) p-width
		  (what-if-height wis) p-height)	      
	    (change-geometry table :width p-width :height p-height :accept-p t)))
	(SETF (what-if-in-use wis) nil)))))


;;;  ===========================================================================  ;;; 
;;;		         A Table's Resize :after Method                           ;;;
;;;  ===========================================================================  ;;; 

(DEFMETHOD resize :after ((table table) width height b-width)
  (DECLARE (IGNORE  width height b-width))
  (LET ((*called-from-resize-method* t))
    (DECLARE (SPECIAL *called-from-resize-method*))
    (change-layout table)))


						
;;;  ===========================================================================  ;;; 
;;;		         A Table's Manage-Geometry Method                         ;;;
;;;  ===========================================================================  ;;; 

;;;  This is not right yet.  It should run a what-if to get a Table size for the child's 
;;;  size change, but this is not possible yet -- the wis doesn't keep all children's
;;;  sizes.  Then it must call change-geometry to see if its parent will let it be that
;;;  size.  If so, it should return a thunk that invokes resize, not change-geometry.

(defmethod manage-geometry ((table table) child x y width height border-width &key)
  (values
      (if
	(or (and x (/= x (contact-x child)))
	    (and y (/= y (contact-y child)))
	    (and width (/= width (contact-width child)))
	    (and height (/= height (contact-height child)))
	    (and border-width (/= border-width (contact-border-width child))))
	#'(lambda (self)
	    (multiple-value-bind (p-w p-h p-b-w)
		(preferred-size self)
	      (change-geometry self
			       :width p-w
			       :height p-h
			       :border-width p-b-w
			       :accept-p t)
	      (change-layout self)
	      (display-force-output (contact-display self))))
	t)
      (or x (contact-x child))
      (or y (contact-y child))
      (or width (contact-width child))
      (or height (contact-height child))
      (or border-width (contact-border-width child))))



;;;
;;;   Internal routines that calculate the width/height of a table, given a What-if-Structure...
;;;		Calculate-Preferred-Width 
;;;		Calculate-Preferred-Height 

(DEFUN calculate-preferred-width (table wis)
  (LET* ((ncolumns (what-if-ncolumns wis))
	 (column-widths (what-if-column-widths wis))
	 (table-width (+ (display-left-margin table)
			 (display-right-margin table)
			 (* (1- ncolumns) (display-horizontal-space table)))))
    (DOTIMES (column ncolumns)
      (INCF table-width (AREF column-widths column 0)))
    table-width))


(DEFUN calculate-preferred-height (table wis)
  (with-slots (row-height separators) (THE table table)

    (LET* ((nrows (what-if-nrows wis))
	   (organization (what-if-organization wis))
	   (table-height (+ (display-top-margin table)
			    (display-bottom-margin table)
			    (* (1- nrows) (display-vertical-space table))))
	   (org-list (REST organization))
	   (fixed-row-heights row-height) height-for-this-row)

      (DO ((row 0 (1+ row)))
	  ((= row nrows))

	(MULTIPLE-VALUE-SETQ (height-for-this-row fixed-row-heights org-list)
	  (determine-a-rows-height row fixed-row-heights org-list))
	
	(INCF table-height height-for-this-row)

	;;  Note:  The physical size of an OL UI separator (white-space) will be defined 
	;;	   to be half the height of the row it follows.  A separator placed after
	;;	   the last row will result in extra white-space at the bottom of the table.
	(WHEN (MEMBER row separators)
	  (INCF table-height (FLOOR (+ height-for-this-row (display-vertical-space table)) 2))))
      
      table-height)))


(DEFUN determine-a-rows-height (row fixed-row-heights org-list1)
  (LET (fixed-height-for-this-row (height-for-this-row 0) found-a-kid-in-this-row-p)
    
    (TYPECASE fixed-row-heights
      (integer
       (SETF fixed-height-for-this-row fixed-row-heights))
      (cons
       (SETF fixed-height-for-this-row (FIRST fixed-row-heights))
       (SETF fixed-row-heights (REST fixed-row-heights))))
    
    (IF fixed-height-for-this-row
	(SETF height-for-this-row fixed-height-for-this-row)
	
	;;else find the tallest element and the largest border width in this row...
	(progn
	  (DO ((org-list1 org-list1 (REST org-list1))
	       kid1 org-entry1 (kid1s-row row))
	      ((OR (NULL org-list1) (AND found-a-kid-in-this-row-p (/= row kid1s-row))))
	    (SETF org-entry1 (FIRST org-list1))
	    (SETF kid1 (org-entry-kid org-entry1)
		  kid1s-row (org-entry-row org-entry1))
	    (WHEN (= row kid1s-row)
	      (SETF found-a-kid-in-this-row-p t)
	      (SETF height-for-this-row
		    (MAX height-for-this-row
			 (+ (org-entry-height org-entry1)
			    (org-entry-border-width org-entry1)
			    (org-entry-border-width org-entry1))))))))
    ;;
    ;;  Because all the members of a row may be withdrawn (and therefore not on the
    ;;  what-if-organization list) it is quite possible to find no children in a row.  For now
    ;;  such a row collapses to zero-height...
    (VALUES height-for-this-row fixed-row-heights org-list1)))






;;;  ===========================================================================  ;;; 
;;;	          The Guts of Table: Place-Children-Physically			  ;;;
;;;  ===========================================================================  ;;; 

(DEFUN place-children-physically (table wis really-p)  
  
  (with-slots (children same-width-in-column same-height-in-row columns
			column-alignment row-alignment
			column-width row-height
			separators) (THE table table)
    
    (LET (kid last-kid-processed height-for-this-row x1 y1
	  (fixed-row-heights (UNLESS (EQ row-height :maximum) row-height))
	  fixed-column-widths
	  width-for-this-column
	  childs-horizontal-size       ; Including border-widths.
	  childs-vertical-size	       ; Including border-widths.
	  max-child-heights-by-row
	  max-child-widths-by-columns
	  org-entry kids-row kids-column
	  y)

      (UNLESS children
	(RETURN-FROM place-children-physically))

      (CASE columns
	(:none
	 (put-kids-into-maximum-unaligned-columns table wis really-p)
	 (RETURN-FROM place-children-physically))

	(:maximum
	 ;; XtNmaximumColumns.
	 ;; Must scan the kids to figure out what width each column should be.
	 (put-kids-into-maximum-aligned-columns table wis))
	
	(otherwise
	 (UNLESS (INTEGERP columns)
	   (ERROR "~s is not a legal value for :columns" columns))
	 ;; XtNrequestedColumns.
	 (put-kids-into-specified-number-of-columns table wis)))
      
      ;;
      ;;  Position the children on the test sheet per the columnarization...
      ;;
      (WHEN really-p
	(MULTIPLE-VALUE-SETQ (max-child-heights-by-row max-child-widths-by-columns)
	  (scan-for-largest-children wis))

	(LET ((org-list (REST (what-if-organization wis)))
	      (column-widths (what-if-column-widths wis)))
	  (SETF y (display-top-margin table))
	  (CATCH 'out-of-kids
	    (DOTIMES (row (what-if-nrows wis))
	      (SETF fixed-column-widths (UNLESS (EQ column-width :maximum) column-width))
	      
	      (MULTIPLE-VALUE-SETQ (height-for-this-row fixed-row-heights)
		(determine-a-rows-height row fixed-row-heights org-list))
	      
	      (LET ((fixed-width-for-this-column
		      (AND (INTEGERP fixed-column-widths) fixed-column-widths))
		    (x (display-left-margin table)))
		
		;;  Now set the row's elements' geometries...
		(DOTIMES (column (what-if-ncolumns wis))
		  (WHEN (EQ kid last-kid-processed)
		    (SETF org-entry (FIRST org-list))
		    (WHEN (NULL org-entry)
		      (THROW 'out-of-kids t))
		    (SETF kid (org-entry-kid org-entry)
			  kids-row (org-entry-row org-entry)
			  kids-column (org-entry-column org-entry)))
		  
		  ;;  Figure out what width WE want this column to be...
		  (WHEN (CONSP fixed-column-widths)
		    (SETF fixed-width-for-this-column (FIRST fixed-column-widths)))
		  (SETF width-for-this-column
			(OR fixed-width-for-this-column (AREF column-widths column 0)))
		  (WHEN (AND (= row kids-row) (= column kids-column))		      
		    (SETF childs-horizontal-size (+ (org-entry-width org-entry)
						    (org-entry-border-width org-entry)
						    (org-entry-border-width org-entry))
			  childs-vertical-size (+ (org-entry-height org-entry)
						  (org-entry-border-width org-entry)
						  (org-entry-border-width org-entry)))
		    
		    (IF (EQ same-width-in-column :on)
			(SETF childs-horizontal-size width-for-this-column
			      x1 x)
		      ;; else...
		      (SETF childs-horizontal-size (MIN childs-horizontal-size
							width-for-this-column)
			    x1 (CASE column-alignment
				 (:left x)
				 (:right (+ x (- width-for-this-column
						 childs-horizontal-size)))
				 (:center (+ x (FLOOR (- width-for-this-column
							 childs-horizontal-size) 2))))))
		    
		    (IF (EQ same-height-in-row :on)
			(SETF childs-vertical-size height-for-this-row
			      y1 y)
		      ;; else...
		      (SETF childs-vertical-size (MIN childs-vertical-size
						      height-for-this-row)
			    y1 (CASE row-alignment
				 (:top y)
				 (:bottom (+ y (- height-for-this-row
						  childs-vertical-size)))
				 (:center (+ y (FLOOR (- height-for-this-row
							 childs-vertical-size) 2))))))
		    
		    ;;
		    ;;   Reposition and/or resize the child iff needed...
		    ;;
		    (LET ((desired-width (- childs-horizontal-size
					    (org-entry-border-width org-entry)
					    (org-entry-border-width org-entry)))
			  (desired-height (- childs-vertical-size
					     (org-entry-border-width org-entry)
					     (org-entry-border-width org-entry))))
		      (with-state (kid)
			(UNLESS (AND (= x1 (contact-x kid))
				     (= y1 (contact-y kid)))
			  (move kid x1 y1))
			(UNLESS (AND (= desired-width (contact-width kid))
				     (= desired-height (contact-height kid))
				     (= (org-entry-border-width org-entry)
					 (contact-border-width kid)))
			  (resize kid desired-width desired-height
				  (org-entry-border-width org-entry))))

		      ;;
		      ;;   Done with this child, move on to the next...
		      ;;
		      (SETF org-list (REST org-list))
		      (SETF last-kid-processed kid)))	  

		    ;;
		    ;;   Whether or not a kid was placed at this row/column, move on to the
		    ;;   next column...
		    (INCF x (+ width-for-this-column
			       (display-horizontal-space table)))
		    (WHEN (CONSP fixed-column-widths)
		      (SETF fixed-column-widths (REST fixed-column-widths))))
		    
		;;
		;;   Get vertical position of top of borders of next row's elements...
		;;
		(INCF y (+ height-for-this-row
			   (display-vertical-space table)))
		(WHEN (MEMBER row separators)
		  (INCF y (FLOOR (+ height-for-this-row
				    (display-vertical-space table)) 2))))))
	  ))
	
	;;
	;;   Having finished placing the kids we can put our preferred size into our wis...
	;;
      (SETF (what-if-preferred-height wis) (calculate-preferred-height table wis)
	    (what-if-preferred-width wis) (calculate-preferred-width table wis))
	)))

    
  
(DEFUN scan-for-largest-children (wis)
  
  (LET* ((max-child-heights-by-row (MAKE-ARRAY (what-if-nrows wis) :initial-element 0))
	 (max-child-widths-by-column (MAKE-ARRAY (what-if-ncolumns wis) :initial-element 0)))
    
    (DOLIST (org-entry (REST (what-if-organization wis)))
      (LET ((row (org-entry-row org-entry))
	    (column (org-entry-column org-entry))
	    (total-child-width (+ (org-entry-width org-entry)
				  (org-entry-border-width org-entry)
				  (org-entry-border-width org-entry)))
	    (total-child-height (+ (org-entry-height org-entry)
				   (org-entry-border-width org-entry)
				   (org-entry-border-width org-entry))))
	(SETF (SVREF max-child-heights-by-row row)
	      (MAX (SVREF max-child-heights-by-row row) total-child-height))
	(SETF (SVREF max-child-widths-by-column column)
	      (MAX (SVREF max-child-widths-by-column column) total-child-width))))
    
    (VALUES max-child-heights-by-row max-child-widths-by-column)))



(DEFUN put-kids-into-specified-number-of-columns (table wis)

  
  (with-slots (column-width columns children) (THE table table)
    (LET* (fixed-width-for-this-column total-kid-width 
	   (fixed-widths-for-columns column-width))

      (SETF (what-if-ncolumns wis) 	 columns
	    (what-if-nrows wis)	  	(CEILING (LENGTH children) columns)
	    (what-if-column-widths wis) (MAKE-ARRAY `(,columns 2) :initial-element 0))

      ;;  Construct the organization list by assigning the children to specific row/column
      ;;  positions in the Table...
      (assign-kids-to-rows-and-columns table wis)

      ;;  Ncolumns was specified by the user.  Nrows was determined from this and by
      ;;  assign-kids-to-rows-and-columns.  This routine scans the organization and builds the array
      ;;  of (list column-width width-of-widest-entry-column) entries.  This array is left in the
      ;;  column-widths slot.

      ;;
      ;;  Find the widest child in each row, set the 2nd element of each width-of-columns
      ;;  entry to the width of the widest child in that column...
      ;;
      (DO ((org-list1 (REST (what-if-organization wis)) (REST org-list1))
	   kid1 org-entry1 kid1s-column kid1s-row)
	  ((NULL org-list1))
	(SETF org-entry1 (FIRST org-list1))
	(SETF kid1 (org-entry-kid org-entry1)
	      kid1s-row (org-entry-row org-entry1)
	      kid1s-column (org-entry-column org-entry1))
	(SETF total-kid-width (+ (org-entry-width org-entry1)
				 (org-entry-border-width org-entry1)
				 (org-entry-border-width org-entry1)))
	(Setf (AREF (what-if-column-widths wis) kid1s-column 1)
	      (MAX (AREF (what-if-column-widths wis) kid1s-column 1) total-kid-width)))
      

      ;;
      ;;  Now go through the columns looking for those with pre-set widths.  Use any pre-set
      ;;  width as the column's width, otherwise use the width of the column's widest child.
      ;;
      (SETF fixed-widths-for-columns column-width)
      (DOTIMES (current-column (what-if-ncolumns wis))
	;;  Get current-column's fixed width, if any...
	(SETF fixed-width-for-this-column 
	      (TYPECASE fixed-widths-for-columns
		(integer fixed-widths-for-columns)
		(CONS (PROG1 (FIRST fixed-widths-for-columns)
			     (SETF fixed-widths-for-columns (REST fixed-widths-for-columns))))))
	(SETF (AREF (what-if-column-widths wis) current-column 0)
	      (OR fixed-width-for-this-column (AREF (what-if-column-widths wis) current-column 1)))))))

(DEFUN find-first-parents-width (table)
  (DO ((parent (contact-parent table) (contact-parent parent)))
      ((NULL parent))
    (UNLESS (ZEROP (contact-width parent))
      (RETURN (contact-width parent)))))

(DEFUN put-kids-into-maximum-unaligned-columns (table wis really-p)

  (with-slots (children same-width-in-column) (THE table table)
      
    (LET* ((org-list (LIST nil))
	   (working-width (what-if-width wis))
	   (border-width (what-if-border-width wis)))
      
      (WHEN (ZEROP working-width)
	  (SETF working-width (- (find-first-parents-width table) border-width border-width)))
      
      ;;  Start by sorting the list of children by their row/column constraints.  Once this is
      ;;  done we ignore the constraints from here on for :none layout policy...
      (LET ((nkids (LENGTH children)))
	(SETF (what-if-nrows wis) nkids
	      (what-if-ncolumns wis) nkids)
	(assign-kids-to-rows-and-columns table wis))
      
      (LET ((next-x-pos (display-left-margin table))
	    (next-y-pos (display-top-margin table))
	    (largest-height-this-row 0)
	    (columns-this-row 0)
	    (ncolumns-in-table 0)
	    (nrows-in-table 0)
	    (preferred-width-of-table 0))
	
	(FLET
	  ((handle-the-end-of-a-row ()
	     (SETF ncolumns-in-table (MAX ncolumns-in-table columns-this-row))
	     (SETF preferred-width-of-table
		   (MAX preferred-width-of-table
			(+ next-x-pos
			   (- (display-right-margin table)
			      (display-horizontal-space table)))))
	     (SETF next-x-pos (display-left-margin table))
	     (INCF nrows-in-table)
	     (INCF next-y-pos (+ largest-height-this-row
				 (display-vertical-space table)))
	     (SETF columns-this-row 0
		   largest-height-this-row 0))
	   )
	  
	  (DOLIST (child children)
	    (UNLESS (EQ (contact-state child) :withdrawn)
	      (MULTIPLE-VALUE-BIND (childs-p-width childs-p-height childs-p-border-width)
		  (preferred-size child)
		(LET ((childs-total-width (+ childs-p-width (* 2  childs-p-border-width)))
		      (childs-total-height (+ childs-p-height (* 2  childs-p-border-width))))
		  
		  ;;
		  ;;  If cannot place this child at the end of this row, finish off this row and move
		  ;;  on to the next row...
		  ;;
		  (WHEN (< (- working-width next-x-pos (display-right-margin table))
			   childs-total-width)
		    (handle-the-end-of-a-row))
		  ;;
		  ;;  Position this child where we've decided it should go...
		  ;;
		  (WHEN really-p
		    (with-state (child)
		      (UNLESS (AND (= next-x-pos (contact-x child))
				   (= next-y-pos (contact-y child)))
			(move child next-x-pos next-y-pos))
		      (UNLESS (AND (= childs-p-width (contact-width child))
				   (= childs-p-height (contact-height child))
				   (= childs-p-border-width (contact-border-width child)))
			(resize child childs-p-width childs-p-height childs-p-border-width))))
		  
		  ;;
		  ;;  Done with this child, move on to the next child and the next position in this
		  ;;  row...
		  ;;
		  (PUSH (make-org-entry :kid child
					:row  nrows-in-table
					:column columns-this-row
					:width childs-p-width
					:height childs-p-height
					:border-width childs-p-border-width) org-list)
		  (INCF next-x-pos (+ childs-total-width
				      (display-horizontal-space table)))
		  (SETF largest-height-this-row (MAX largest-height-this-row childs-total-height))
		  (INCF columns-this-row)))))

	  ;;
	  ;;  Set into the what-if structure the height, width, and organization just calculated...
	  ;;
	  (handle-the-end-of-a-row)
	  (SETF (what-if-nrows wis) nrows-in-table)
	  (SETF (what-if-ncolumns wis) ncolumns-in-table)
	  (SETF (what-if-preferred-height wis)
		(+ next-y-pos (- (display-vertical-space table))
		   (display-bottom-margin table)))
	  (SETF (what-if-preferred-width wis) preferred-width-of-table)
	  (SETF (what-if-organization wis) (NREVERSE org-list))
	  ;;
	  ;;  Set up a fake column-widths array for others...
	  ;;
	  (SETF (what-if-column-widths wis)
		(MAKE-ARRAY `(,ncolumns-in-table 2) :initial-element 0))
	  
	  (SETF (AREF (what-if-column-widths wis) 0 0) (what-if-preferred-width wis)))))))


(DEFUN put-kids-into-maximum-aligned-columns (table wis)
  ;; This is a guessing procedure that implements the XtNmaximumColumns policy for row and column
  ;; layout.  Keep an array of items (column-width max-width-of-columns-items).  Create and
  ;; initialize it from the 1st child: identical column widths = 1st child's preferred width,
  ;; max-width-of-columns-items = 0.  Set NROWS to 0.  Then start trying to place the children
  ;; into these columns.  The 1st child will fit for sure, updating the 1st column's max-width.
  ;; The 2nd-Nth children may or may not fit.  If it does, update max-width.  If not, see if
  ;; other columns' can be made narrower to allow this column to be made wide enough for him to
  ;; fit.  If so, do it.  If not, we must reduce the number of columns by one, assigning them
  ;; equal widths, then start the layout process from the top.  Each time we try to place a child
  ;; in the first column, increment NROWS.
  
  ;; Note that while this routine tends to give about the same amount of space to each column,
  ;; the slack space for the columns may differ considerably.  After we find a child the cannot
  ;; fit in a column and reduce the number of columns to get more space, we give each column the
  ;; same, new, enlarged space.  If one column is actually fairly narrow and doesn't need more
  ;; space it'll end up with extra slack space around it.  A slack-space-smoothing routine should
  ;; be written to improve this.
  
  
  (with-slots (children column-width) (THE table table)
    
      (LET ((nkids (LENGTH children))
	    (working-width (what-if-width wis))
	    (working-border-width (what-if-border-width wis)))
	
	(WHEN (<= working-width 0)
	  (SETF working-width (- (find-first-parents-width table)
				 working-border-width working-border-width)))

	;;
	;;  Start by sorting the list of children by their row/column constraints.  Once this is
	;;  done we ignore the constraints from here on for :maximum layout policy...
	;;
	(SETF (what-if-nrows wis) nkids
	      (what-if-ncolumns wis) nkids)
	(assign-kids-to-rows-and-columns table wis)
	
	
	;;  Start with an upper bound on the number of columns...
	(LET* ((ncolumns (MIN nkids (get-maximum-possible-ncolumns table working-width)))
	       (column-widths (MAKE-ARRAY `(,ncolumns 2)))
	       (column-widths-vector (MAKE-ARRAY (* 2 ncolumns) :displaced-to column-widths)))
					    

	  ;;
	  ;;  Each execution of this outer loop represents an attempt at fitting the children
	  ;;  into a given number of columns.  The inner loop below does the actual laying out of
	  ;;  the children; if it succeeds, it sets FINISHED to T as it exits.  If it fails, it
	  ;;  decrements NCOLUMNS and leaves FINISHED NIL.
	  ;;
	  (DO* (finished
		(org-list (LIST nil))
		(org-tail org-list)
		next-row next-column)
	       (finished
		 ;;
		 ;;  Make each column's real width equal to the widest child we've placed in it,
		 ;;  adjust ncolumns by the number of unused columns...
		 ;;
		 (DOTIMES (column ncolumns)
		   (IF (ZEROP (AREF column-widths column 1))
		       (DECF ncolumns)
		       (SETF (AREF column-widths column 0) (AREF column-widths column 1))))
		 
		 (SETF (what-if-column-widths wis) column-widths)
		 (SETF (what-if-ncolumns wis) ncolumns)
		 (SETF (what-if-organization wis) org-list)
		 (SETF (what-if-nrows wis) (1+ next-row)))
	       
	    ;;  Initialize the first ncolumns elements of the column-widths array...
	    ;;  Total horizontal space available for the columns:
	    ;;  	width - right-margin - left-margin - (n - 1)*horizontal-space.
	    ;;  This total is divided into ncolumns equal chunks, with any extra white space
	    ;;  being given a pixel at a time to the left-most columns.
	    
	    ;;  But not quite.  We need to handle fixed-width columns specially.  At this point
	    ;;  we know how many columns we're (tentatively) giving the table, call it N.  We
	    ;;  need to see how much of our space is occupied by fixed-width columns in the
	    ;;  first N columns and how many there are, call it M.  The remaining N-M columns
	    ;;  each gets 1/(N-M) of the remaining space.  Be careful abaout N=M!  And each
	    ;;  fixed-width column gets *both* of its column-width entries initialized here to
	    ;;  its fixed width so it'll look like there's no slack in that column (which there
	    ;;  isn't).  Unlike a variable-width column, a fixed-width column never gets its
	    ;;  2nd column-widths entry changed as we place kids in it.

	    (LET ((total-fixed-width 0) (n-fixed-width-columns 0)
		  (fixed-column-widths (UNLESS (EQ column-width :maximum) column-width)))

	      ;;  Forget the column widths calculated last time through the loop...
	      (FILL (THE vector column-widths-vector) nil)
	      
	      ;;  Calculate how much of the total table width is allocated to fixed-width
	      ;;  columns...
	      (COND
		((NULL fixed-column-widths))
		((INTEGERP fixed-column-widths)
		 (SETF total-fixed-width (* ncolumns fixed-column-widths)
		       n-fixed-width-columns ncolumns)
		 (DOTIMES (column-number ncolumns)
		   (SETF (AREF column-widths column-number 0)
			 (SETF (AREF column-widths column-number 1) fixed-column-widths))))
		((CONSP fixed-column-widths)
		 (DO ((fixed-column-widths fixed-column-widths (REST fixed-column-widths))
		      (column-number 0 (1+ column-number))
		      fixed-width)
		     ((OR (= column-number ncolumns)
			  (ENDP fixed-column-widths)))
		   (SETF fixed-width (FIRST fixed-column-widths))
		   (WHEN fixed-width
		     (INCF n-fixed-width-columns)
		     (INCF total-fixed-width fixed-width)
		     (SETF (AREF column-widths column-number 0)
			   (SETF (AREF column-widths column-number 1) fixed-width)))))
		(t (ERROR "column-width is ~a." fixed-column-widths)))
	      
	      ;;  Now n-fixed-width-columns = # of fixed width columns in first ncolumns
	      ;;      total-fixed-width     = # of pixels occupied by those columns
	      ;;  and for each fixed-width column both column-widths entries = the fixed width.

	      ;;  Take the remaining space and give it to the non-fixed-width columns...	      
	      (UNLESS  (ZEROP (- ncolumns n-fixed-width-columns))
		(MULTIPLE-VALUE-BIND (horizontal-space-for-each-var-column extra-white-space)
		    (FLOOR (- working-width
			      (display-left-margin table)
			      (display-right-margin table)
			      (* (1- ncolumns) (display-horizontal-space table))
			      total-fixed-width)
			   (- ncolumns n-fixed-width-columns))
		  
		  ;;  Assign the non-fixed-width space to the non-fixed-width columns.  Because
		  ;;  we FILL column-widths with NIL each time through the main loop, only
		  ;;  fixed-width columns will have none-NIL values in them.  Give the extra
		  ;;  white-space to the left-most variable-width columns a pixel at a time.
		  (DOTIMES (i ncolumns)
		    (WHEN (NULL (AREF column-widths i 0))
		      (SETF (AREF column-widths i 0)
			    (+ horizontal-space-for-each-var-column
			       (IF (ZEROP extra-white-space)
				   0
				   (PROGN (DECF extra-white-space) 1))))
		      (SETF (AREF column-widths i 1) 0)))))

	       
	       (SETF org-list (LIST nil)
		     org-tail org-list
		     next-row -1
		     next-column (1- ncolumns))
	       
	       ;;
	       ;;  Try to lay the children into the columns sized as they are now...
	       ;;
	       (DOLIST (child children (SETF finished t))
		 
		 (UNLESS (EQ (contact-state child) :withdrawn)
		   ;;
		   ;;  If the column this child's to go in is beyond ncolumns, wrap to the first
		   ;;  column of the next row...
		   ;;
		   (INCF next-column)
		   (WHEN (= next-column ncolumns)
		     (SETF next-column 0)
		     (INCF next-row)
		     (SETF fixed-column-widths (UNLESS (EQ column-width :maximum) column-width)))
		   
		   (LET* ((columns-width-right-now (AREF column-widths next-column 0))
			  (fixed-width-for-this-column
			    (IF (LISTP fixed-column-widths)      ;; ERCM
				(FIRST fixed-column-widths)
				fixed-column-widths)))

		     (UNLESS fixed-width-for-this-column			
			;;  Find out what width the child thinks he should be...
			(MULTIPLE-VALUE-BIND (childs-width childs-height childs-border-width)
			    (preferred-size child :width columns-width-right-now)
			  (DECLARE (IGNORE childs-height))
			  
			  ;;  Calculate how much horizontal space this child needs...
			  (LET ((horizontal-space-for-this-child
				  (+ childs-width childs-border-width childs-border-width)))
			    
			    (COND
			      ((OR (<= horizontal-space-for-this-child columns-width-right-now)
				   (adjust-column-widths-so-child-fits
				     column-widths horizontal-space-for-this-child
				     next-column ncolumns))
			       (SETF (AREF column-widths next-column 1)
				     (MAX (AREF column-widths next-column 1)
					  horizontal-space-for-this-child)))
			      (t			   
			       ;; else child can't fit in this column.  Reduce the number of
			       ;; columns and try again.
			       (DECF ncolumns)
			       (RETURN nil)))))))

		   ;;  To get here we must have decided we can successfully place this kid at
		   ;;  this position, so add an entry for it onto the org-list...
		   (SETF (REST org-tail)
			 (LIST (establish-org-entry child next-row next-column)))
		   (SETF org-tail (REST org-tail))
		   
		   ;;  Advance to the next column's entry in the fixed-width list if there is
		   ;;  one...
		   (WHEN (CONSP fixed-column-widths)
		     (SETF fixed-column-widths (REST fixed-column-widths)))))))))))


(DEFUN adjust-column-widths-so-child-fits (column-widths childs-width next-column ncolumns)
  
  (DO ((npixels-needed (- childs-width (AREF column-widths next-column 0))))
      ((ZEROP npixels-needed)
       (SETF (AREF column-widths next-column 0) childs-width)
       t)
    
    ;; Find column with greatest slack, if any...
    (LET ((max-slack 0) (max-slack-col nil))
      (DOTIMES (col ncolumns)
	(UNLESS (= next-column col)		; Don't look at column child goes in
	  (LET ((slack (- (AREF column-widths col 0) (AREF column-widths col 1))))
	    (WHEN (> slack max-slack)
	      (SETF max-slack slack
		    max-slack-col col)))))
      
      ;;  If no column had any slack, return NIL...
      (UNLESS max-slack-col (RETURN nil))

      ;;  Otherwise take a pixel from the max-slack-col's width, reduce our goal by one, try
      ;;  again...
      (DECF (AREF column-widths max-slack-col 0))
      (DECF npixels-needed))))



(DEFUN get-maximum-possible-ncolumns (table width)
  "Returns the maximum number of columns possible given the specified constraints."
  (with-slots (children column-width) (THE table table)
    
    (LET* ((fixed-column-widths (UNLESS (EQ column-width :maximum) column-width))
	   (minimum-column-width
	     (- width (display-left-margin table) (display-right-margin table))))

      ;;
      ;;  If the caller specified a single fixed width for all columns, then that's it...
      ;;
      (IF (INTEGERP fixed-column-widths)
	  (SETF minimum-column-width (MIN minimum-column-width fixed-column-widths))

	;; else...
	(PROGN
	  ;;
	  ;;  If the caller specified a list of fixed widths (and nil's) for (some of) the
	  ;;  columns, first find the minimum of these fixed column widths...
	  ;;
	  (WHEN (CONSP fixed-column-widths)
	    (DOLIST (this-fixed-column-width fixed-column-widths)
	      (WHEN this-fixed-column-width
		(SETF minimum-column-width
		      (MIN minimum-column-width this-fixed-column-width)))))
	  
	  ;;
	  ;;  Then as a crude approximation, find the narrowest child, not knowing what column
	  ;;  the child will go in...
	  ;;
	  (DOLIST (kid children)
	    (UNLESS (EQ (contact-state kid) :withdrawn)
	      (MULTIPLE-VALUE-BIND (preferred-width preferred-height preferred-border-width)
		  (preferred-size kid)
		(DECLARE (IGNORE preferred-height))
		(SETF minimum-column-width
		      (MIN minimum-column-width
			   (+ preferred-width preferred-border-width preferred-border-width))))))))
      
      ;;  Now that we have the smallest column width we could ever get, calculate and return the
      ;;  maximum number of columns we could ever have...
      (MIN (LENGTH children)
	   (FLOOR (+ (- width
			(display-left-margin table)
			(display-right-margin table))
		     (display-horizontal-space table))
		  (+ minimum-column-width (display-horizontal-space table)))))))



;;;
;;;   These routines construct the ORGANIZATION list by placing each child at a specific
;;;   row/column position
;;;
;;;.  Lexical variables:
;;;	hole-pointer	where in the existing organization list to rplacd-in an entry for an
;;;			unconstrained child -- the current "hole".  All entries in the
;;;			organization list preceding this one are contiguous starting from row 0,
;;;			column 0, so all attempts at child placement, regardless of the
;;;			constraints, start from here.  Hole-row & hole-column are one row/col
;;;			position beyond the row/col of (FIRST hole-pointer), unless (first
;;;			hole-pointer) is NIL, in which case they are (0,0).
;;;	hole-row	the row-number of the current hole.
;;;	hole-column	the column-number of the current hole.
;;;	ncolumns	the number of columns in the table.  Fixed.
;;;	nrows		the number of rows in the table.  Can change if a child specifies a big
;;;			row-constraint.
;;;

(DEFUN assign-kids-to-rows-and-columns (table wis)
  (LET (hole-pointer hole-row hole-column ncolumns nrows)
    
    
    #-cmu ;; Python will make local function. Not sure inline works here.
    (DECLARE (inline insert-into-organization-list))
    (LABELS
      (
       ;;
       ;;   Makes sure the hole-pointer/row/column actually point at a hole.  If they currently
       ;;   point at an allocated table row/column, moves them over until they point at an
       ;;   unallocated one. 
       ;;
       (find-next-hole
	 ()
	 (DO* (org-entry org-row org-column
	       (org-list hole-pointer))
	      (nil)
	   ;;
	   ;;  Look at the next org-entry, the one just beyond the hole pointer.  The second -
	   ;;  Nth times through the loop this also advances the hole-pointer...
	   ;;
	   (SETF hole-pointer org-list
		 org-list (REST org-list))
	   (WHEN org-list
	     (SETF org-entry (FIRST org-list)
		   org-row (org-entry-row org-entry)
		   org-column (org-entry-column org-entry)))
	   (WHEN (OR (NULL org-list)		; Exhausted org-list.  Leave hole pointing at
						;    row/col one beyond the last org-entry.
		     (/= org-row hole-row)	; There's space between the previous org-entry
		     (/= org-column hole-column))	;    and this one.  Leave hole pointing
						;    at row/col one beyond the previous
						;    org-entry. 
	     (RETURN))
	   ;;
	   ;;   The row/column position of the hole is occupied.  Move the row/column of the hole
	   ;;   over one position, try again...
	   ;;
	   (WHEN (= (INCF hole-column) ncolumns)
	     (INCF hole-row)
	     (SETF hole-column 0))))

       ;;
       ;;   Insert KID into the organization list at INSERTION-POINT at ROW/COLUMN...
       ;;
       (insert-into-organization-list
	 (kid insertion-point row column)
	 (RPLACD insertion-point
		 (CONS (establish-org-entry kid row column)
		       (REST insertion-point)))
	 (find-next-hole)	 
	 (WHEN (>= row nrows)			; Update nrows if necessary.
	   (SETF nrows (1+ row))))		;   *
       
       ;;
       ;;   Inserts a kid with no constraints in the next hole, moves the hole pointers.  Always
       ;;   successful, so always returns T.
       ;;
       (place-a-kid-at-any-row-and-column
	 (kid)
	 (insert-into-organization-list kid hole-pointer hole-row hole-column)
	 t)
       
       ;;
       ;;   Tries to insert a kid into a specific row/column, returning T if successful, NIL if
       ;;   not.  Fails if that row/column is already occupied or specified column is outside
       ;;   ncolumns.
       ;;
       (place-a-kid-at-a-specific-row-and-column
	 (kid kid-row kid-column)
	 
	 (LET ((kid-position (+ (* ncolumns kid-row) kid-column))
	       (last-occupied-position
		 (IF (FIRST hole-pointer)
		     (+ (* ncolumns (org-entry-row (FIRST hole-pointer)))
			(org-entry-column (FIRST hole-pointer)))
		     -1)))
	   (WHEN (OR (>= kid-column ncolumns)
		     (>= last-occupied-position kid-position))
	     (RETURN-FROM place-a-kid-at-a-specific-row-and-column nil))
	   
	   
	   (DO ((org-list hole-pointer) insertion-point org-position)
	       (nil)
	     
	     (SETF insertion-point org-list
		   org-list (REST org-list))
	     
	     (SETF org-position
		   (IF org-list
		       (+ (* ncolumns (org-entry-row (FIRST org-list)))
			  (org-entry-column (FIRST org-list)))
		       (1+ kid-position)))
	     
	     (COND
	       ((= org-position kid-position)	; Kid's row/column occupied: failure.
		(RETURN-FROM place-a-kid-at-a-specific-row-and-column nil))
	       ((> org-position kid-position)	; Kid's row/column free: success.
		(insert-into-organization-list kid insertion-point kid-row kid-column)
		(RETURN-FROM place-a-kid-at-a-specific-row-and-column t))
	       (t nil)))))
       
       ;;
       ;;   Tries to insert a kid into a specific row.
       ;;   Fails if row is full, returns NIL, otherwise is successful, returns T.
       ;;
       (place-a-kid-in-a-specific-row
	 (kid kid-row)

	 (WHEN (< kid-row hole-row)
	   (RETURN-FROM place-a-kid-in-a-specific-row nil))
	 
	 (DO ((org-list hole-pointer) insertion-point
	      (last-occupied-column
		(IF (FIRST hole-pointer) (org-entry-column (FIRST hole-pointer)) -1) org-column)
	      org-entry (org-row kid-row) org-column)
	     ((OR (NULL org-list)
		  (> org-row kid-row))
	      ;; Failure -- exit here iff couldn't insert child
	      nil)
	   (SETF insertion-point org-list
		 org-list (REST org-list))
	   (IF org-list
	       (SETF org-entry (FIRST org-list)
		     org-row (org-entry-row org-entry)
		     org-column (org-entry-column org-entry))
	     ;; else no more org-entries so fake one way out there...
	     (SETF org-row (1+ kid-row)))
	   
	   (WHEN (OR (AND (= org-row kid-row)	        ; In kid's row and there's a hole.
			  (< (1+ last-occupied-column)	;   *
			     org-column))		;   *
		     (AND (> org-row kid-row)		; First org-entry beyond kid's row
			  (< last-occupied-column	;   and there's a hole at the end
			     (1- ncolumns))))		;   of the kid's row.
	     (insert-into-organization-list
	       kid insertion-point kid-row (1+ last-occupied-column))
	     (RETURN-FROM place-a-kid-in-a-specific-row t))))
       
       ;;
       ;;   Inserts a kid into a specific column.
       ;;   Fails if column is not within ncolumns, returns NIL, otherwise always successful,
       ;;   returns T.
       ;;
       (place-a-kid-in-a-specific-column
	 (kid kids-column)
	 	   
	 (WHEN (>= kids-column ncolumns)
	   (RETURN-FROM place-a-kid-in-a-specific-column nil))
	 
	 (DO* ((org-list hole-pointer) insertion-point
	       (last-org-position -1 org-position) org-position
	       (insertion-row (IF (< kids-column hole-column) (1+ hole-row) hole-row))
	       (position-of-next-occurrence-of-kids-column
		 (+ (* ncolumns insertion-row) kids-column)))
	      (nil)
	   
	   (SETF insertion-point org-list
		 org-list (REST org-list))
	   
	   (SETF org-position
		 (IF org-list
		     (+ (* ncolumns (org-entry-row (FIRST org-list)))
			(org-entry-column (FIRST org-list)))
		     (1+ position-of-next-occurrence-of-kids-column)))
	   
	   (WHEN (< last-org-position
		    position-of-next-occurrence-of-kids-column
		    org-position)
	     (insert-into-organization-list kid insertion-point insertion-row kids-column)
	     (RETURN-FROM place-a-kid-in-a-specific-column t))
	   ;; Calculate a new position-of-next-occurrence-of-kids-column if this org-entry is at
	   ;; or beyond the current value...
	   (WHEN (>= org-position position-of-next-occurrence-of-kids-column)
	     (INCF position-of-next-occurrence-of-kids-column ncolumns)
	     (INCF insertion-row))))
       
       ;;
       ;;  This is called by assign-kids-to-rows-and-columns when it realizes it is dealing with
       ;;  a :maximum or :none table.  The Table's children list is rebuilt to be
       ;;  the (already sorted) kids in the org-list followed by the kids in the free-list.
       ;;  Where unconstrained kids would normally be used to fill in holes in a
       ;;  fixed-number-of-columns table, there really are no holes for a :maximum or
       ;;  :none table so such children are just placed at the end of the Table's
       ;;  children list.
       ;;
       (build-sorted-list-of-children
	 (table org-list free-list withdrawn-children)
	 (with-slots (children) (THE table table)
	   (LET* ((sorted-children-list (MAKE-LIST (LENGTH org-list)))	; includes leading NIL.
		  (next-sorted-children-list sorted-children-list)
		  (last-sorted-children-list sorted-children-list))
	     
	     (DOLIST (org-entry (REST org-list))
	       (SETF last-sorted-children-list next-sorted-children-list
		     next-sorted-children-list (REST next-sorted-children-list))
	       (RPLACA next-sorted-children-list (org-entry-kid org-entry)))
	     
	     (WHEN free-list
	       (RPLACD last-sorted-children-list (NCONC free-list withdrawn-children)))
	     (SETF children (REST sorted-children-list)))))

       )					; ...end of labels...
      
      ;; ====================================================================================
      ;;   The code for assign-kids-to-rows-and-columns (table wis):
      ;;   Constructs the what-if-organization list by assigning each kid to a specific
      ;;   row/column position in the table.
      ;;
      (with-slots (children) (THE table table)
	(LET (free-row free-col free (old-org-list (REST (what-if-organization wis)))
	      withdrawn-children)
	  (SETF (what-if-organization wis) (LIST nil)
		hole-pointer (what-if-organization wis)
		hole-row 0
		hole-column 0
		ncolumns (what-if-ncolumns wis)
		nrows (what-if-nrows wis))
	  ;;  First try to place all the kids with definite row/column constraints.
	  ;;  Any child specifying only a row goes on the free-col list.
	  ;;  Any child specifying only a column goes on the free-row list.
	  ;;  Any child specifying neither row nor column, or any child unable to be placed where
	  ;;      its definite row/column constraints placed it, goes on the free list.
	  (DOLIST (kid children)
	    (COND
	      ((NOT (EQ (contact-state kid) :withdrawn))
	       (UNLESS (OR (NULL old-org-list)
			   (EQ kid (org-entry-kid (FIRST old-org-list))))
		 (CERROR "continue" "children and org-list don't match"))
	       (LET ((row (OR (table-row kid)
			      (AND old-org-list (org-entry-row (FIRST old-org-list)))))
		     (column (OR (table-column kid)
				 (AND old-org-list (org-entry-column (FIRST old-org-list))))))
		 (SETF old-org-list (REST old-org-list))
		 (COND
		   ((AND row column)
		    (UNLESS (place-a-kid-at-a-specific-row-and-column kid row column)
		      (PUSH kid free)))
		   (row
		    (PUSH `(,kid ,row) free-col))
		   (column
		    (PUSH `(,kid ,column) free-row))
		   (t
		    (PUSH kid free)))))
	      (t
	       (PUSH kid withdrawn-children))))
	  
	  ;;  Now try to place all the kids specifying only a column.  Since it is always OK to
	  ;;  create a new row, such kids can always be placed...
	  (DOLIST (kid-and-column (NREVERSE free-row))
	    (place-a-kid-in-a-specific-column (FIRST kid-and-column) (SECOND kid-and-column)))
	  
	  ;;  Now try to place all the kids specifying only a row.  If that row is full, place
	  ;;  the child on the free list...
	  (DOLIST (kid-and-row (NREVERSE free-col))
	    (UNLESS (place-a-kid-in-a-specific-row (FIRST kid-and-row) (SECOND kid-and-row))
	      (PUSH (FIRST kid-and-row) free)))
	  
	  ;;  Finally, place the kids that are on the free list.  These kids have no constraints,
	  ;;  so they'll all be placed in holes scanning from top-left to bottom-right or new
	  ;;  rows will be created to hold them...
	  (IF (SYMBOLP (table-columns table))
	      (build-sorted-list-of-children
		table (what-if-organization wis) (NREVERSE free) withdrawn-children)
	      
	      ;; else...
	      (PROGN 
		(DOLIST (kid (NREVERSE free))
		  (place-a-kid-at-any-row-and-column kid))
		;;
		;;  Rebuild the children list in the order of the what-if-organization
		;;  followed by any :withdrawn children not on the what-if-organization list.
		;;
		(DO ((children children (REST children))
		     (organization (REST (what-if-organization wis)) (REST organization)))
		    ((NULL organization)
		     (DOLIST (withdrawn-child withdrawn-children)
		       (RPLACA children withdrawn-child)
		       (SETF children (REST children))))
		  (RPLACA children (org-entry-kid (FIRST organization))))))
	  
	  (SETF (what-if-nrows wis) nrows))))))

;;  This is called by assign-kids-to-rows-and-columns when it realizes it is dealing with a
;;  :maximum or :none table.  The Table's children list is rebuilt to be the
;;  (already sorted) kids in the org-list followed by the kids in the free-list.  Where
;;  unconstrained kids would normally be used to fill in holes in a fixed-number-of-columns
;;  table, there really are no holes for a :maximum or :none table so such children
;;  are just placed at the end of the Table's children list.

(DEFUN build-sorted-list-of-children (table org-list free-list withdrawn-children)
  (with-slots (children) (THE table table)
    (LET* ((sorted-children-list (MAKE-LIST (LENGTH org-list)))	; includes leading NIL.
	   (next-sorted-children-list sorted-children-list)
	   (last-sorted-children-list sorted-children-list))
      
      (DOLIST (org-entry (REST org-list))
	(SETF last-sorted-children-list next-sorted-children-list
	      next-sorted-children-list (REST next-sorted-children-list))
	(RPLACA next-sorted-children-list (org-entry-kid org-entry)))
      
      (WHEN free-list
	(RPLACD last-sorted-children-list (NCONC free-list withdrawn-children)))
      (SETF children (REST sorted-children-list)))))
