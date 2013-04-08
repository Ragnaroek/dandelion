;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Fonts:(CPTFONT); Syntax:Common-Lisp -*-


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

(EXPORT '(
	  choice-default
	  choice-font
	  choice-selection
	  make-multiple-choices
	  multiple-choices
	  ))


;;;  ============================================================================
;;;	     T h e   M U L T I P L E   -   C H O I C E S   C o n t a c t
;;;  ============================================================================

(DEFCONTACT multiple-choices (table)
  
  ((font 	:type 		fontable
	 	:reader 	choice-font
		:initarg	:font
	 	:initform 	nil)
    
   (selection   :type           list
                :accessor       choice-selection
                :initform       nil)

   (default	:type		list
                :accessor       choice-default
		:initarg	:default-selection
		:initform	nil)
   )
  
  (:resources
    font default
    (horizontal-space :initform 3)
    (vertical-space :initform 3))

  (:documentation
    "Provides a mechanism for displaying N choices to a user of which the user may select M,
where N >= M >= 0."))



(DEFUN make-multiple-choices (&rest initargs &key &allow-other-keys)
  (APPLY #'make-contact 'multiple-choices initargs))


(DEFMETHOD add-child :after ((choices multiple-choices) this-child &key)
  (flet
    (
     ;;; ===============================================================================
     ;;;
     ;;;       Our :changing and :canceling-change callback functions...
     ;;;
     (choices-changing (to-selected-p choices self)
       (DECLARE (IGNORE self))
       (LET((selection (choice-selection choices))
	    (default (choice-default choices)))

	 (WHEN default
	   ;; If there is a current choice default then we *may* have
	   ;; to temporarily inhibit display of the default ring.
	   (UNLESS (and selection to-selected-p)
	     ;; If there is a current selection already and we are
	     ;; transitioning *to* selected state then the default ring(s)
	     ;; are already inhibited.  Otherwise, there are two possibilites:
	     ;; (1) No selection, transitioning *to* selected.
	     ;;     We must inhibit ring display on all defaults.
	     ;; (2) Have selection, transitioning *from* selected.
	     ;;     If there is only one selection and it is transitioning to
	     ;;     unselected, then we must restore default ring(s) display.
	     (let
	       ((highlighted-p (not to-selected-p)))
	       (when (or to-selected-p (null (cdr selection)))
		 (DOLIST (item default)
		   (SETF (choice-item-highlight-default-p item) highlighted-p))))))))

     (choices-canceling-change (to-selected-p choices self)
       (DECLARE (IGNORE self))
       (LET((selection (choice-selection choices))
	    (default (choice-default choices)))

	 (WHEN default
	   ;; If we are canceling a transition to "selected" then we
	   ;; must restore the inhibited default ring display.
	   ;; If, on the other hand, we are canceling a transition
	   ;; back to "unselected" then we must once again inhibit
	   ;; default ring display.
	   (UNLESS (and selection to-selected-p)
	     ;; As in choices-changing, if there is a current selection
	     ;; already inhibiting default ring display then we need not
	     ;; restore display here.
	     (when (or to-selected-p (null (cdr selection)))
		 (DOLIST (item default)
		   (SETF (choice-item-highlight-default-p item) to-selected-p)))))))


     ;;; ================================================================================
     ;;;	This :off callback (destructively) removes the item from the current
     ;;;        selection set for this multiple-choices contact.
     ;;;
     (choices-off (choices self)
       (WITH-SLOTS (selection) choices
	 (WHEN selection (SETF selection (DELETE self selection)))))

     ;;; ================================================================================
     ;;;	This :on callback adds the item to the current selection set
     ;;;        for this multiple-choices contact.
     ;;;
     (choices-on (choices self)
       (WITH-SLOTS (selection) choices
	 ;; It is important to *not* use the SETF method here since doing so would
	 ;; potentially cause a loop. [SETF method invokes this callback!]
	 (SETF selection (cons self selection))))
     )						; ... end of flet ...

    (let((font (choice-font choices)))
      (WHEN font (SETF (choice-item-font this-child) font)))

    ;;  =====================================================================================
    ;;  If this child's name is on the default-selection list, replace it with this child.
    ;;
    (with-slots (default) choices
      (DO ((defaults default (REST defaults)))
	  ((NULL defaults))
	(WHEN (EQ (FIRST defaults) (contact-name this-child))
	  (RPLACA defaults this-child)
	  (SETF (choice-item-highlight-default-p this-child) T)
	  (RETURN))))

    (add-callback this-child :changing #'choices-changing choices this-child)
    (add-callback this-child :canceling-change #'choices-canceling-change choices this-child)
    (add-callback this-child :on #'choices-on choices this-child)
    (add-callback this-child :off #'choices-off choices this-child)))


;;; ===============================================================================
;;;
;;;              Method to set the default choice item set
;;;

(DEFMETHOD (SETF choice-default) (new-default-choice-items (choices multiple-choices))
  (with-slots (default children) choices
    (let
      ((new-defaults (set-difference new-default-choice-items default))
       (no-longer-defaults (set-difference default new-default-choice-items)))
      (WHEN new-defaults
	(ASSERT (subsetp new-defaults children)
		NIL
		"New default choice-items ~a are not children of ~a."
		(set-difference new-defaults children) choices)
	
	(DOLIST (item new-defaults)
	  (SETF (choice-item-highlight-default-p item) T))
	(DOLIST (item no-longer-defaults)
	  (SETF (choice-item-highlight-default-p item) NIL))
	(SETF default new-default-choice-items))))
  new-default-choice-items)


;;; ===============================================================================
;;;
;;;            Methods to set the selected choice-items set
;;;
(DEFMETHOD (SETF choice-selection) (children-to-be-selected (choices multiple-choices))

  (DECLARE (TYPE list children-to-be-selected))
  
  (with-slots (children selection) choices
    (let
      ((new-selections (set-difference children-to-be-selected selection))
       (no-longer-selected (set-difference selection children-to-be-selected)))
    
    ;;  Make sure the caller's selection are indeed a children of ours...
    (ASSERT (subsetp new-selections children)
	    NIL
      "Selections ~a are not children of ~a." (set-difference new-selections selection) choices)

    ;; Clear selected status of items no longer selected
    (DOLIST (item no-longer-selected)
      (SETF (choice-item-selected-p item) NIL))
    ;; Set selected status of items newly selected
    (DOLIST (item new-selections)
      (SETF (choice-item-selected-p item) T))))
  children-to-be-selected)

;;; ===============================================================================
;;;
;;;                   Method to force the font of all children...
;;;

(DEFMETHOD (SETF choice-font) (new-value (multiple-choices multiple-choices))
  
  (with-slots (children font) multiple-choices
    (if new-value
	(progn
	  (SETF font (find-font multiple-choices new-value))
	  (DOLIST (child children)
	    (SETF (choice-item-font child) new-value)))
	(SETF font NIL))
    new-value))
