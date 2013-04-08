;; -*- MODE:LISP; Package:CLIO-OPEN; Base:10; Lowercase:T; Fonts:(CPTFONT); Syntax:Common-Lisp -*-


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

;;; CHOICES:


(in-package "CLIO-OPEN")

(EXPORT '(
	  choices
	  choice-default
	  choice-font
	  choice-policy
	  choice-selection
	  make-choices
	  ))


;;;  ============================================================================
;;;	              T h e   C H O I C E S   C o n t a c t
;;;  ============================================================================

(defcontact choices (table)
  
  ((font 	:type 		fontable
	 	:accessor	choice-font
		:initarg	:font
	 	:initform 	nil)

   (selection   :type           (or null contact)
                :accessor       choice-selection
                :initform       nil)

   (default	:type		(or null contact symbol)
                :accessor       choice-default
		:initform	nil
		:initarg	:default-selection)

   (policy	:type		(member :always-one :one-or-none)
		:accessor	choice-policy
		:initarg	:choice-policy
		:initform	:one-or-none))
  
  (:resources
    font
    (default-selection :type symbol :initform nil)
    (choice-policy     :type (member :always-one :one-or-none)
		       :initform :one-or-none) 
    (horizontal-space  :initform -1)
    (vertical-space    :initform -1))

  (:documentation
    "Allows a user to choose at most one choice item."))


(DEFUN make-choices (&rest initargs &key &allow-other-keys)
  (APPLY #'make-contact 'choices initargs))

;;  Add our callbacks to each child as it is added, before anybody else gets in ahead of us...
(DEFMETHOD add-child :after ((choices choices) this-child &key)
  (flet
    (
     ;;; ===============================================================================
     ;;;
     ;;;                   Our :change-allowed-p callback function...
     ;;;	Applied by a choice-item child when the user tries to change its state
     ;;;		Tells the child whether or not the change may occur
     ;;;
     ;;;
     (choices-change-allowed-p (to-selected-p choices)
       (ECASE (choice-policy choices)
	 (:always-one (or to-selected-p
			  ;; Following allows deselection of old selection
			  ;; when transitioning to new selection. [See
			  ;; (SETF choice-selection) for details.]
			  (boundp '*within-setf-choice-selection*))) 
	 (:one-or-none t)))

     ;;; ===============================================================================
     ;;;
     ;;;       Our :changing and :canceling-change callback functions...
     ;;;
     (choices-changing (to-selected-p choices self)
       (LET((selection (choice-selection choices))
	    (default (choice-default choices)))

	 (WHEN (and to-selected-p selection (not (eq selection self)))
	   ;; When transitioning to selected state we must turn off
	   ;; the highlighting of the current choice selection, if any.
	   (SETF (choice-item-highlight-selected-p selection) nil))

	 (WHEN (AND default (NOT (EQ self default)))
	   ;; If there is a current choice default then we *may* have
	   ;; to temporarily inhibit display of the default ring.
	   (UNLESS (and selection to-selected-p)
	     ;; If there is a current selection already and we are
	     ;; transitioning *to* selected state then the current
	     ;; selection must be a toggle button (or some other
	     ;; button whose state is sticky.) In that case the
	     ;; default ring will already have been inhibited by
	     ;; that button's selection. Otherwise we inhibit the
	     ;; the default ring now.
	     (SETF (choice-item-highlight-default-p default) (not to-selected-p))))))

     (choices-canceling-change (to-selected-p choices self)
       (LET((selection (choice-selection choices))
	    (default (choice-default choices)))

	 (WHEN (and to-selected-p selection)
	   ;; If canceling change to selected state we must restore
	   ;; highlight of old selection (if any)
	   (SETF (choice-item-highlight-selected-p selection) t))

	 (WHEN (AND default (NOT (EQ default self)))
	   ;; If we are canceling a transition to "selected" then we
	   ;; must restore the inhibited default ring display.
	   ;; If, on the other hand, we are canceling a transition
	   ;; back to "unselected" then we must once again inhibit
	   ;; default ring display.
	   (UNLESS (and selection to-selected-p)
	     ;; As in choices-changing, if there is a current selection
	     ;; already inhibiting default ring display then we need not
	     ;; restore display here.
	     (SETF (choice-item-highlight-default-p default) to-selected-p)))))


     ;;; ================================================================================
     ;;;	This :off callback deselects currently selected child and
     ;;;        resets the choice selection to NIL. We assume choice policy
     ;;;        enforcement is done elsewhere and so allow deselection even
     ;;         if :always-one.
     ;;;
     (choices-off (choices self)
       (DECLARE (IGNORE self))
       ;;  +++ Gross hack until I find out what's really wrong.
       (unless (boundp '*within-setf-choice-selection*)
	 (SETF (choice-selection choices) nil)))

     ;; ================================================================================
     ;;	This :on callback deselects currently selected child (if any) and
     ;;        resets the choice selection to point to the specified child. 
     ;;
     (choices-on (choices self)
	(change-choices-selection choices self)))

    (let((font (choice-font choices)))
      (WHEN font (SETF (choice-item-font this-child) font)))

    ;;  Make this child the default if the child's name is currently the default...
    (with-slots (default) choices
      (when (and (symbolp default) (eq default (contact-name this-child)))
	(setf default nil)
	(setf (choice-default choices) this-child)))

    (add-callback this-child :change-allowed-p #'choices-change-allowed-p choices)
    (add-callback this-child :changing #'choices-changing choices this-child)
    (add-callback this-child :canceling-change #'choices-canceling-change choices this-child)
    (add-callback this-child :on #'choices-on choices this-child)
    (add-callback this-child :off #'choices-off choices this-child)

    ;;  This callback provides a hook used by the menu code.
    (apply-callback choices :new-choice-item this-child)))


(defmethod change-layout :after ((choices choices) &optional newly-managed)
  (declare (ignore newly-managed))
  (with-slots (policy selection children) choices
    (unless (realized-p choices)
      (when (and (eq policy :always-one) (not selection) children)
	(setf (choice-selection choices) (first children))))))


;;; ===============================================================================
;;;
;;;              Method to set the default choice item...
;;;

(DEFMETHOD (SETF choice-default) (new-default-choice-item (choices choices))
  (with-slots (default children) choices
    (UNLESS (EQ new-default-choice-item default)
      (ASSERT (MEMBER new-default-choice-item children)
	      NIL
	"New default choice-item ~a is not a child of ~a."
	new-default-choice-item choices)
      (when default (setf (choice-item-highlight-default-p default) NIL))
      (setf default new-default-choice-item)
      (setf (choice-item-highlight-default-p default) T)))
  new-default-choice-item)




;;; ===============================================================================
;;;
;;;            Method to set the selected choice-items...
;;;

;;  Assume ALL programmatic changes to the set of selected children come through here so all
;;  enforcement of the choice-policy is done here...

(defun change-choices-selection (choices child-to-be-selected)
  (declare (type (or null contact) child-to-be-selected))
  
  (with-slots (children selection) (the choices choices)
    (unless (eq selection child-to-be-selected)   
      
      ;;  Don't check for compliance while we're in the middle of a change...
      (unless (boundp '*within-setf-choice-selection*)
	(assert
	  (or child-to-be-selected (eq (choice-policy choices) :one-or-none)) nil
	  "Violating :always-one choice policy of choices contact ~a." choices))
      
      (let ((*within-setf-choice-selection* t))
	(declare (special *within-setf-choice-selection*))
	
	;;  Make sure the caller's selection is indeed a child of ours...
	(assert
	  (or (null child-to-be-selected) (member child-to-be-selected children)) nil
	  "Selection ~a is not a child of ~a." child-to-be-selected choices)            
	
	;; We must do things in just the right order here in case choice
	;; policy is :always-one, in which case turning off the old
	;; choice-item-selected-p is a bit tricky. In particular, the
	;; :change-allowed callback will fail unless we first update the
	;; current choices selection with the new value so it knows the
	;; choice policy will not be violated. 
	
	(let ((old-selection selection)) 
	  (when old-selection (setf (choice-item-selected-p old-selection) nil))
	  (setf selection child-to-be-selected)))))
      
  child-to-be-selected)

(defmethod (setf choice-selection) (child-to-be-selected (choices choices))
  (change-choices-selection choices child-to-be-selected)
  (when child-to-be-selected
    (setf (choice-item-selected-p child-to-be-selected) t))
  child-to-be-selected)



;;; ===============================================================================
;;;
;;;                   Method to force the font of all children...
;;;

(DEFMETHOD (SETF choice-font) (new-value (choices choices))
  
  (with-slots (children font) choices
    (IF new-value
	(PROGN
	  (SETF font (find-font choices new-value))
	  (DOLIST (child children)
	    (SETF (choice-item-font child) new-value)))
	;; Setting font to NIL allows the children to have distinct fonts
	;; so we skip resetting choice-item font slots on children.
	(SETF font NIL))
  new-value))  ;; grh 7/27


;;; ===============================================================================
;;;
;;;                   Method to set the choice-policy...
;;;

(DEFMETHOD (SETF choice-policy) (new-policy (choices choices))
  (with-slots (policy children selection default) choices
    (ECASE new-policy
      (:always-one				; Make sure one child is selected...
       (when (null selection)
	 (if default 
	     (setf (choice-selection choices)  default)
	     (if children
		 (setf (choice-selection choices) (first children))
		 (assert nil nil "~s choices does not have a default or children")))))
      (:one-or-none				; Nothing more to do...
       nil))
    (SETF policy new-policy)
    new-policy))
