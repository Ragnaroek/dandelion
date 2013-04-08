;;; -*- Mode:Lisp; Package:CLIO-OPEN; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


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


;;;
;;;  Form, a layout composite with fancy constraints.
;;;
;;;  Work remaining:
;;;
;;;  1.  To-link traversals (all the traversals to date use the from-links).
;;;      [This one's done, except for some refinement and testing.  - pf]
;;;  2.  When taking a child's size, check its state and use 0 if it isn't
;;;      :mapped.
;;;  3.  Compromise child-resize in manage-geometry-hard-case when the Form
;;;      is only granted part of its own resize request (currently it punts).
;;;
;;;  Open issues or places to work on are marked with +++.

(in-package "CLIO-OPEN")

(export '(
	  form
	  make-form
	  make-horizontal-link
	  make-vertical-link
	  form-max-height
	  form-max-width
	  form-min-height
	  form-min-width
	  link-from
	  link-to
	  link-orientation
	  link-attach-from
	  link-attach-to
	  link-length
	  link-maximum
	  link-minimum
	  link-update
	  find-link
	  )
	'clio-open)

;;;
;;;  The Form contact itself.  Horizontal-links and vertical-links normally
;;;  store lists of links that connect directly to the Form, but they can
;;;  also be passed as initargs to make-form in a form (no pun) that specifies
;;;  how to link the Form and its children (see resolve-initial-links for
;;;  details).

(defcontact form (core composite)
  ((horizontal-links :type list
		     :initform nil
		     :accessor form-horizontal-links
		     :initarg :horizontal-links)
   (vertical-links   :type list
		     :initform nil
		     :accessor form-vertical-links
		     :initarg :vertical-links))
  (:resources	     (border-width :initform 0))
  (:constraints	     (:max-height :type (or card16 (member :infinite)))
		     (:max-width  :type (or card16 (member :infinite)))
		     (:min-height :type card16)
		     (:min-width  :type card16))
  (:documentation "A layout using form constraints."))


(defun make-form (&rest initargs &key &allow-other-keys)
   (apply #'make-contact 'form initargs))


;;;
;;;  Link structure and functions.  Placement constraints among the children
;;;  of a Form are handled by attaching links between pairs of children and
;;;  between the Form and a child.  These links define a graph that is traversed
;;;  to determine the Form's preferred-size, or the amount of shrink or stretch
;;;  during a resizing (traversal functions are further below).  Links are not
;;;  too different from TeX's "glue" boxes, with the additional generality that
;;;  they can be attached to either edge or the center of a contact and that they
;;;  work in two dimensions rather than a one-dimensional line.

;;  Each link between two contacts is one of these (the same object is
;;  stored with both contacts).  Length is measured between FROM and
;;  TO.  The attach-points are on the contact named, and may be
;;  any of :left, :right, :top, :bottom, or :center.
;;
;;  This guy is a class chiefly for portability:  the type-checking and layout
;;  changing could just as easily be defined in a defsetf on a defstruct accessor,
;;  but there isn't a portable way to set a structure slot other than SETF on the
;;  accessor, so it couldn't be done portably.  If this turns out to be inefficient,
;;  we may want to change back, bearing in mind that some places in this code want
;;  to change slot values without doing the change-layout (manage-geometry, for one).
(defclass link ()
   ((orientation :initform nil
		 :reader   link-orientation
		 :initarg  :orientation)
    (from	 :initform nil
		 :initarg  :from)
    (attach-from :initform nil
		 :reader   link-attach-from
		 :initarg  :attach-from)
    (to		 :initform nil
		 :initarg  :to)
    (attach-to   :initform nil
		 :reader   link-attach-to
		 :initarg  :attach-to)
    (length      :initform 0
		 :reader   link-length
		 :initarg  :length)
    (minimum     :initform nil
		 :reader   link-minimum
		 :initarg  :minimum)
    (maximum     :initform nil
		 :reader   link-maximum
		 :initarg  :maximum)
    (tentative-length   :initform nil)
    (implicit-p  :initform nil
		 :accessor link-implicit-p)))

;;  These two readers are defined specially so they can error-check.
(defmethod link-from ((link link))
   (with-slots (from) link
     (or from
	 (error "Link ~S is not valid." link))))

(defmethod link-to ((link link))
   (with-slots (to) link
     (or to
	 (error "Link ~S is not valid." link))))

(defmethod (setf link-attach-from) (attach-from (link link))
   (ecase (link-orientation link)
     (:horizontal
      (check-type attach-from (member :left :right :center)))
     (:vertical
      (check-type attach-from (member :center :top :bottom))))
   (setf (slot-value link 'attach-from) attach-from)
   (link-update-change-layout link)
   attach-from)

(defmethod (setf link-attach-to) (attach-to (link link))
   (ecase (link-orientation link)
     (:horizontal
      (check-type attach-to (member :left :right :center)))
     (:vertical
      (check-type attach-to (member :center :top :bottom))))
   (setf (slot-value link 'attach-to) attach-to)
   (link-update-change-layout link)
   attach-to)

(defmethod (setf link-length) (length (link link))
   (check-type length int16)
   (setf (slot-value link 'length) length)
   (link-update-change-layout link)
   length)

(defmethod (setf link-maximum) (maximum (link link))
   (check-type maximum (or int16 (member :infinite)))
   (setf (slot-value link 'maximum) maximum)
   (link-update-change-layout link)
   maximum)

(defmethod (setf link-minimum) (minimum (link link))
   (check-type minimum int16)
   (setf (slot-value link 'minimum) minimum)
   (link-update-change-layout link)
   minimum)

;;  Let the tentative length default to the true length.
(defmacro link-tentative-length (link)
   `(or (slot-value (the link ,link) 'tentative-length)
	(link-length ,link)))

(defsetf link-tentative-length (link) (tentative-length)
   `(setf (slot-value ,link 'tentative-length) ,tentative-length))


(defun link-update (link &key length minimum maximum attach-from attach-to)
   "Make multiple changes to a link, as if by setf'ing all the fields given."
   (declare (type link link))
   (check-type link        link)
   (check-type length	   (or null int16))
   (check-type minimum	   (or null int16))
   (check-type maximum	   (or null int16 (member :infinite)))
   (check-type attach-from (or null (member :left :right :center :top :bottom)))
   (check-type attach-to   (or null (member :left :right :center :top :bottom)))
   (with-slots ((link-length      length)
		(link-minimum	  minimum)
		(link-maximum	  maximum)
		(link-attach-from attach-from)
		(link-attach-to	  attach-to))
	       link
     (when length
       (setq link-length length))
     (when minimum
       (setq link-minimum minimum))
     (when maximum
       (setq link-maximum maximum))
     (when attach-from
       (setq link-attach-from attach-from))
     (when attach-to
       (setq link-attach-to attach-to)))

   (link-update-change-layout link))

;;  Once the changes are complete, call change-layout to make it happen.
;;  This doesn't consistency-check the parentage, since that is done elsewhere
;;  (make-horizontal-link, make-vertical-link) and not changed here.
(defun link-update-change-layout (link)
   (let ((form (if (or (eq (contact-parent (link-from link))
			   (contact-parent (link-to link)))
		       (eq (contact-parent (link-from link))
			   (link-to link)))
		   (contact-parent (link-from link))
		   (contact-parent (link-to link)))))
     (change-layout form)))

(defun find-link (from to orientation &optional form-attach-point)
   "Find the link between FROM and TO with the orientation ORIENTATION.
Will find the link regardless of the ordering of FROM and TO.  Returns
NIL if no link found.  FORM-ATTACH-POINT is the attach-point on the Form
itself, if one of the contacts is the Form and one is a child."
   (check-type from contact)
   (check-type to   contact)
   (check-type orientation (member :horizontal :vertical))
   (check-type form-attach-point (or null (member :left :right :center :top :bottom)))
   (assert (not (eq from to)) () "A contact may not be linked to itself.")
   (assert (or (eq (contact-parent from) (contact-parent to))
	       (eq from (contact-parent to))
	       (eq to (contact-parent from)))
	   ()
	   "Two linked contacts must either be children of the same Form, or the Form and one of its children.")
   (let ((link-list (if (or (eq (contact-parent from) (contact-parent to))
			    (eq (contact-parent from) to))
			(if (eq orientation :horizontal)
			    (contact-constraint from :horizontal-links)
			    (contact-constraint from :vertical-links))
			(if (eq orientation :horizontal)
			    (contact-constraint to :horizontal-links)
			    (contact-constraint to :vertical-links)))))
     (dolist (link link-list nil)
       (when (and (eq (link-orientation link) orientation)
		  (or (and (eq (link-from link) from)
			   (eq (link-to link) to)
			   (or (null form-attach-point)
			       (cond ((eq from (contact-parent to))
				      (eq form-attach-point (link-attach-from link)))
				     ((eq to (contact-parent from))
				      (eq form-attach-point (link-attach-to link)))
				     (:else
				      t))))
		      (and (eq (link-to link) from)
			   (eq (link-from link) to)
			   (or (null form-attach-point)
			       (cond ((eq from (contact-parent to))
				      (eq form-attach-point (link-attach-to link)))
				     ((eq to (contact-parent from))
				      (eq form-attach-point (link-attach-from link)))
				     (:else
				      t))))))
	 (return link)))))

;;  Destroying a link means removing its connections.  We NIL out its contacts
;;  to flag later improper use.
(defmethod destroy ((link link))
   (with-slots (from to) link
     (if (eq from (contact-parent to))
	 (setf (form-horizontal-links from)
	       (delete link (form-horizontal-links from)))
	 (setf (contact-constraint from :horizontal-links)
	       (delete link (contact-constraint from :horizontal-links))))
     (if (eq (contact-parent from) to)
	 (setf (form-horizontal-links to)
	       (delete link (form-horizontal-links to)))
	 (setf (contact-constraint to :horizontal-links)
	       (delete link (contact-constraint to :horizontal-links))))
     (setq from nil
	   to	nil)))

;;  Only allow one link of a given orientation between two contacts.
;;
;;  This function is a little hairy to treat the different attach-points of the top-level
;;  Form as if they were separate contacts for the sake of this test.  It is allowed to
;;  have multiple links between the Form and a single child if they all attach at different
;;  places, eg, from the :left of the Form to the :left of the child and from the :right
;;  of the child to the :right of the Form.  So, the test is that if the FROM and TO fields
;;  of the links are the same, the links are equal if either both contacts are children,
;;  or one is the Form and the Form attach-points are the same.  The test is similar when
;;  the FROM and TO fields are crossed (from-1 is to-2 and vice versa).
(defun link-equal (link-1 link-2)
   (let ((from-1 (link-from link-1))
	 (from-2 (link-from link-2))
	 (to-1   (link-to   link-1))
	 (to-2   (link-to   link-2)))
     (and (eq (link-orientation link-1)
	      (link-orientation link-2))
	  (or (and (eq from-1 from-2)
		   (eq to-1   to-2)
		   (or (eq (contact-parent from-1) (contact-parent to-1))
		       (if (eq from-1 (contact-parent to-1))
			   (eq (link-attach-from link-1) (link-attach-from link-2))
			   (eq (link-attach-to link-1)   (link-attach-to link-2)))))
	      (and (eq from-1 to-2)
		   (eq from-2 to-1)
		   (or (eq (contact-parent from-1) (contact-parent to-1))
		       (if (eq from-1 (contact-parent to-1))
			   (eq (link-attach-from link-1) (link-attach-to link-2))
			   (eq (link-attach-to link-1)   (link-attach-from link-2)))))))))

(defun make-horizontal-link (&key from to
				  (minimum 0) (length minimum) (maximum :infinite)
				  (attach-from :right) (attach-to :left))
   "   Add a horizontal link between two contacts.  The contacts must either
be children of the same Form, or the Form and one of its children.  FROM
is the \"left\" contact -- lengths are positive when FROM is to the left
of TO, negative otherwise.
   ATTACH-FROM and ATTACH-TO indicate where the link is attached to the
FROM and TO contacts, respectively, and must be one of :LEFT, :RIGHT, or
:CENTER, referring to the left or right edge or the center of the contact.
   LENGTH, MINIMUM, and MAXIMUM define the length of the link and its
range of values.  All may be any INT16;  MAXIMUM may also be :INFINITE."

   (check-type from contact)
   (check-type to contact)
   (check-type attach-from (member :left :center :right))
   (check-type attach-to   (member :left :center :right))
   (check-type length int16)
   (check-type minimum int16)
   (check-type maximum (or int16 (member :infinite)))
   (assert (not (eq from to)) () "A contact may not be linked to itself.")
   (assert (or (eq (contact-parent from) (contact-parent to))
	       (eq from (contact-parent to))
	       (eq to (contact-parent from)))
	   ()
	   "Two linked contacts must either be children of the same Form, or the Form and one of its children.")

   ;;  The flags left-form-p and right-form-p are needed because a child contact's
   ;;  links are kept on its contact-constraints, while the parent Form's links
   ;;  are kept in its slot variables.  This distinction will crop up frequently.
   ;;  When true, the contact indicated is the parent Form.
   (let ((left-form-p  (eq from (contact-parent to)))
	 (right-form-p (eq to (contact-parent from)))
	 (link (make-instance 'link
			      :orientation :horizontal
			      :from	   from
			      :to	   to
			      :attach-from attach-from
			      :attach-to   attach-to
			      :length      length
			      :minimum     minimum
			      :maximum     maximum)))

     ;;  If there already exists a link between these two contacts, remove it
     ;;  superseding it with this one.  Note that link-equal special-cases links
     ;;  to the Form to allow links from the left of the Form to the left of the
     ;;  child, thence from the right of the child to the right of the Form.
     (cond (left-form-p
	    (setf (form-horizontal-links from)
		  (delete link (form-horizontal-links from) :test #'link-equal))
	    (setf (contact-constraint to :horizontal-links)
		  (delete link (contact-constraint to :horizontal-links) :test #'link-equal)))
	   (right-form-p
	    (setf (contact-constraint from :horizontal-links)
		  (delete link (contact-constraint from :horizontal-links) :test #'link-equal))
	    (setf (form-horizontal-links to)
		  (delete link (form-horizontal-links to) :test #'link-equal)))
	   (:else
	    (setf (contact-constraint from :horizontal-links)
		  (delete link (contact-constraint from :horizontal-links) :test #'link-equal))
	    (setf (contact-constraint to :horizontal-links)
		  (delete link (contact-constraint to :horizontal-links) :test #'link-equal))))

     ;;  Save the link on the appropriate list.
     (if left-form-p
	 (push link (form-horizontal-links from))
	 (push link (contact-constraint from :horizontal-links)))
     (if right-form-p
	 (push link (form-horizontal-links to))
	 (push link (contact-constraint to :horizontal-links)))
     link))

(defun make-vertical-link (&key from to
				(minimum 0) (length minimum) (maximum :infinite)
				(attach-from :bottom) (attach-to :top))
   "   Add a vertical link between two contacts.  The contacts must either
be children of the same Form, or the Form and one of its children.  FROM
is the \"top\" contact -- lengths are positive when FROM is above
TO, negative otherwise.
   ATTACH-FROM and ATTACH-TO indicate where the link is attached to the
FROM and TO, respectively, and must be one of :TOP, :BOTTOM, or
:CENTER, referring to the top or bottom edge or the center of the contact.
   LENGTH, MINIMUM, and MAXIMUM define the length of the link
and its range of values.  All may be any INT16;  MAXIMUM may also be
:INFINITE."
   (check-type from contact)
   (check-type to contact)
   (check-type attach-from (member :top :center :bottom))
   (check-type attach-to (member :top :center :bottom))
   (check-type length int16)
   (check-type minimum int16)
   (check-type maximum (or int16 (member :infinite)))
   (assert (not (eq from to)) () "A contact may not be linked to itself.")
   (assert (or (eq (contact-parent from) (contact-parent to))
	       (eq from (contact-parent to))
	       (eq to (contact-parent from)))
	   ()
	   "Two linked contacts must either be children of the same Form, or the Form and one of its children.")

   ;;  The flags top-form-p and bottom-form-p are needed because a child contact's
   ;;  links are kept on its contact-constraints, while the parent Form's links
   ;;  are kept in its slot variables.  This distinction will crop up frequently.
   ;;  When true, the contact indicated is the parent Form.
   (let ((top-form-p    (eq from (contact-parent to)))
	 (bottom-form-p (eq to (contact-parent from)))
	 (link (make-instance 'link
			      :orientation :vertical
			      :from	   from
			      :to	   to
			      :attach-from attach-from
			      :attach-to   attach-to
			      :length      length
			      :minimum     minimum
			      :maximum     maximum)))

     ;;  If there already exists a link between these two contacts, remove it
     ;;  superseding it with this one.  Note that link-equal special-cases links
     ;;  to the Form to allow links from the top of the Form to the top of the
     ;;  child, thence from the bottom of the child to the bottom of the Form.
     (cond (top-form-p
	    (setf (form-vertical-links from)
		  (delete link (form-vertical-links from) :test #'link-equal))
	    (setf (contact-constraint to :vertical-links)
		  (delete link (contact-constraint to :vertical-links) :test #'link-equal)))
	   (bottom-form-p
	    (setf (contact-constraint from :vertical-links)
		  (delete link (contact-constraint from :vertical-links) :test #'link-equal))
	    (setf (form-vertical-links to)
		  (delete link (form-vertical-links to) :test #'link-equal)))
	   (:else
	    (setf (contact-constraint from :vertical-links)
		  (delete link (contact-constraint from :vertical-links) :test #'link-equal))
	    (setf (contact-constraint to :vertical-links)
		  (delete link (contact-constraint to :vertical-links) :test #'link-equal))))

     ;;  Save the link on the appropriate list.
     (if top-form-p
	 (push link (form-vertical-links from))
	 (push link (contact-constraint from :vertical-links)))
     (if bottom-form-p
	 (push link (form-vertical-links to))
	 (push link (contact-constraint to :vertical-links)))
     link))


;;;
;;;  Constraints.  These functions are the advertised interface for accessing
;;;  and modifying constraints on the size of the children contacts, in addition
;;;  to the ability to specify them as initargs to make-contact when making the
;;;  children.  The maximum and minimum height and width, if unspecified, will
;;;  be the current height and width.

(defun form-max-height (contact)
   (or (contact-constraint contact :max-height)
       (contact-height contact)))
(defsetf form-max-height setf-form-max-height)
(defun setf-form-max-height (contact new-value)
   (check-type new-value (or null card16 (member :infinite)))
   (setf (contact-constraint contact :max-height) new-value))

(defun form-max-width (contact)
   (or (contact-constraint contact :max-width)
       (contact-width contact)))
(defsetf form-max-width setf-form-max-width)
(defun setf-form-max-width (contact new-value)
   (check-type new-value (or null card16 (member :infinite)))
   (setf (contact-constraint contact :max-width) new-value))

(defun form-min-height (contact)
   (or (contact-constraint contact :min-height)
       (contact-height contact)))
(defsetf form-min-height setf-form-min-height)
(defun setf-form-min-height (contact new-value)
   (check-type new-value (or null card16))
   (setf (contact-constraint contact :min-height) new-value))

(defun form-min-width (contact)
   (or (contact-constraint contact :min-width)
       (contact-width contact)))
(defsetf form-min-width setf-form-min-width)
(defun setf-form-min-width (contact new-value)
   (check-type new-value (or null card16))
   (setf (contact-constraint contact :min-width) new-value))


;;  Abstractions for various things placed on the window-plist of each child.
;;
;;  Form-tick is just a flag that is set on each child as it is visited in the
;;  resize process.  If an attempt is made to move a child that has already
;;  been moved, it's an indication that the constraints are inconsistent, and
;;  an error is signalled.
;;
;;  The "tentative" quantities are here for two reasons:  (1) They allow trying out
;;  sizes and placements without really changing anything, which we use in
;;  manage-geometry to shuffle things around when necessary.  (2) For resizing,
;;  it's part of an efficiency hack:  The algorithm for a Form resize is to resize
;;  the children, determining the maximum stretch or shrink across the link graph
;;  and apportioning the size change to the children according to their
;;  constraints, then to move the children so they satisfy the placement
;;  constraints of the links.  The hack is that the resizes and moves are faked --
;;  the new values are placed on the window-plists of the children, using the
;;  "tentative" accessors below, and are used by subsequent steps of the algorithm.
;;  Once all the new values are computed, one pass through the children combines
;;  the new width, height, x, and y values into a single move and resize within a
;;  with-state, thereby limiting server requests to a maximum of one per child.

(defmacro form-tick (contact)
   `(getf (window-plist ,contact) 'form-tick))

(defmacro contact-tentative-width (contact)
   `(or (getf (window-plist ,contact) 'tentative-width)
	(contact-width ,contact)))
(defsetf contact-tentative-width (contact) (new-val)
  `(setf (getf (window-plist ,contact) 'tentative-width) ,new-val))

(defmacro contact-tentative-height (contact)
   `(or (getf (window-plist ,contact) 'tentative-height)
	(contact-height ,contact)))
(defsetf contact-tentative-height (contact) (new-val)
  `(setf (getf (window-plist ,contact) 'tentative-height) ,new-val))

(defmacro contact-tentative-x (contact)
   `(or (getf (window-plist ,contact) 'tentative-x)
	(contact-x ,contact)))
(defsetf contact-tentative-x (contact) (new-val)
  `(setf (getf (window-plist ,contact) 'tentative-x) ,new-val))

(defmacro contact-tentative-y (contact)
   `(or (getf (window-plist ,contact) 'tentative-y)
	(contact-y ,contact)))
(defsetf contact-tentative-y (contact) (new-val)
  `(setf (getf (window-plist ,contact) 'tentative-y) ,new-val))

;;  These two are similar in concept, but are used during change-layout
;;  so manage-geometry can experiment with various Form sizes without
;;  really doing the change.
(defmacro form-projected-width (contact)
   `(or (getf (window-plist ,contact) 'form-projected-width)
	(contact-width ,contact)))
(defsetf form-projected-width (contact) (new-val)
  `(setf (getf (window-plist ,contact) 'form-projected-width) ,new-val))

(defmacro form-projected-height (contact)
   `(or (getf (window-plist ,contact) 'form-projected-height)
	(contact-height ,contact)))
(defsetf form-projected-height (contact) (new-val)
  `(setf (getf (window-plist ,contact) 'form-projected-height) ,new-val))


;;;
;;;  Traversal functions.  Lots of the important work in placing and sizing
;;;  the children of a Form happens right here.  The next 24 functions
;;;  traverse the graph defined by the links between the Form and the children
;;;  to determine the Form's preferred width and height and the allowable stretch
;;;  and shrink both horizontally and vertically.  Their use is explained below,
;;;  around place-and-size-children.
;;;
;;;  The pattern is pretty much the same for each of the six pairs of functions:
;;;  The "top-level" function looks at all the links attached to the Form that
;;;  have the Form in the FROM position, and maximises the desired quantity
;;;  over all paths through the graph that start with those links.  The "path"
;;;  function recurses through the children contacts and their links until it
;;;  reaches the Form again, also maximising as it goes.  The two "stretch"
;;;  functions vary a bit in that they pass multiple values around -- the maximum
;;;  values of child sizes and link lengths are allowed to be :infinite, so the
;;;  "stretch" functions maximise primarily over the number of :infinites, and
;;;  secondarily over the numerical values.

;;  Find the desired Form width, given the existing sizes of children and links.
;;  A path from the center of the Form through children to the Form's left or right
;;  defines half the width, and thus implies the whole width.  So, if the given
;;  path went from :left to :center or :right to :center, double it for the Form width.
(defun find-form-ideal-width (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-width 0))
     (dolist (link (form-horizontal-links form))
       (when (eq form (link-from link))
	 ;;  Path-value is the length of the link, corrected according to where
	 ;;  it attaches to the next contact (ie, the distance to the left edge
	 ;;  of the next contact), plus the value of the maximum path starting
	 ;;  at that contact.
	 (let ((path-value (+ (link-length link)
			      (link-horizontal-attach-to-correction link)
			      (find-path-ideal-width (link-to link) form))))
	   (when (eq (link-attach-from link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-width (max max-width path-value)))
	 (setf (form-tick (link-to link)) t)))
     ;;  Now do graphs rooted on the "to" side of the Form, not reachable from
     ;;  the "from" side.
     (dolist (link (form-horizontal-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 (let ((path-value (+ (link-length link)
			      (link-horizontal-attach-from-correction link)
			      (find-path-ideal-width (link-from link) form t))))
	   (when (eq (link-attach-to link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-width (max max-width path-value)))))
     max-width))

;;  The width down a given path is the width of the contact and its borders, plus
;;  the maximum path value for all paths using its links.  Link lengths are corrected
;;  for attachments other than right-edge to left-edge.
(defun find-path-ideal-width (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       0					; Back at the parent Form, end of path.
       (+ (contact-width contact)
	  (contact-border-width contact)
	  (contact-border-width contact)
	  (let ((max-width 0))
	    (dolist (link (contact-constraint contact :horizontal-links)
			  max-width)
	      (when (eq contact (if to-links-p
				    (link-to link)
				    (link-from link)))
		(let* ((next-contact (if to-links-p
					 (link-from link)
					 (link-to link)))
		       (path-value (+ (if to-links-p
					  (link-horizontal-attach-to-correction link)
					  (link-horizontal-attach-from-correction link))
				      (link-length link)
				      ;;  Don't compensate when attaching to form.
				      (if (eq next-contact top-level-form)
					  0
					  (if to-links-p
					      (link-horizontal-attach-from-correction link)
					      (link-horizontal-attach-to-correction link)))
				      (find-path-ideal-width next-contact top-level-form to-links-p))))
		  (setq max-width (max max-width path-value)))))))))

(defun link-horizontal-attach-to-correction (link)
   (let ((next-contact (link-to link)))
     (ecase (link-attach-to link)
       (:left 0)
       (:center (- (+ (round (contact-width next-contact) 2)
		      (contact-border-width next-contact))))
       (:right (- (+ (contact-width next-contact)
		     (contact-border-width next-contact)
		     (contact-border-width next-contact)))))))

(defun link-horizontal-attach-from-correction (link)
   (let ((contact (link-from link)))
     (ecase (link-attach-from link)
       (:left (- (+ (contact-width contact)
		    (contact-border-width contact)
		    (contact-border-width contact))))
       (:center (- (+ (round (contact-width contact) 2)
		      (contact-border-width contact))))
       (:right 0))))


;;  Find the desired Form height, given the existing sizes of children and links.
;;  A path from the center of the Form through children to the Form's top or bottom
;;  defines half the height, and thus implies the whole height.  So, if the given
;;  path went from :top to :center or :bottom to :center, double it for the Form height.
(defun find-form-ideal-height (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-height 0))
     (dolist (link (form-vertical-links form))
       (when (eq form (link-from link))
	 ;;  Path-value is the length of the link, corrected according to where
	 ;;  it attaches to the next contact, plus the value of the maximum path
	 ;;  starting at that contact.
	 (let ((path-value (+ (link-length link)
			      (link-vertical-attach-to-correction link)
			      (find-path-ideal-height (link-to link) form))))
	   (when (eq (link-attach-from link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-height (max max-height path-value))
	   (setf (form-tick (link-to link)) t))))
     (dolist (link (form-vertical-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 ;;  Path-value is the length of the link, corrected according to where
	 ;;  it attaches to the next contact, plus the value of the maximum path
	 ;;  starting at that contact.
	 (let ((path-value (+ (link-length link)
			      (link-vertical-attach-from-correction link)
			      (find-path-ideal-height (link-from link) form t))))
	   (when (eq (link-attach-to link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-height (max max-height path-value)))))
     max-height))

;;  The height down a given path is the height of the contact and its borders, plus
;;  the maximum path value for all paths using its links.  Link lengths are corrected
;;  for attachments other than bottom-edge to top-edge.
(defun find-path-ideal-height (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       0
       (+ (contact-height contact)
	  (contact-border-width contact)
	  (contact-border-width contact)
	  (let ((max-height 0))
	    (dolist (link (contact-constraint contact :vertical-links)
			  max-height)
	      (when (eq contact (if to-links-p (link-to link) (link-from link)))
		(let* ((next-contact (if to-links-p (link-from link) (link-to link)))
		       (path-value (+ (if to-links-p
					  (link-vertical-attach-to-correction link)
					  (link-vertical-attach-from-correction link))
				      (link-length link)
				      ;;  Don't compensate when attaching to form.
				      (if (eq next-contact top-level-form)
					  0
					  (if to-links-p
					      (link-vertical-attach-from-correction link)
					      (link-vertical-attach-to-correction link)))
				      (find-path-ideal-height next-contact top-level-form to-links-p))))
		  (setq max-height (max max-height path-value)))))))))

(defun link-vertical-attach-to-correction (link)
   (let ((next-contact (link-to link)))
     (ecase (link-attach-to link)
       (:top 0)
       (:center (- (+ (round (contact-height next-contact) 2)
		      (contact-border-width next-contact))))
       (:bottom (- (+ (contact-height next-contact)
		      (contact-border-width next-contact)
		      (contact-border-width next-contact)))))))

(defun link-vertical-attach-from-correction (link)
   (let ((contact (link-from link)))
     (ecase (link-attach-from link)
       (:top (- (+ (contact-height contact)
		   (contact-border-width contact)
		   (contact-border-width contact))))
       (:center (- (+ (round (contact-height contact) 2)
		      (contact-border-width contact))))
       (:bottom 0))))


;;  Stretch is a little hairy.  The ultimate result will be two values, the maximum
;;  number of :infinites and the largest numerical value associated with that many
;;  :infinites.  When scaling sizes and link-lengths, the size increase is apportioned
;;  among the contacts and links according to their "maximum" constraints.  If there
;;  are any :infinites in the stretch value, only those contacts and links with :infinite
;;  as their maximum will stretch, because :infinite is by definition much stretchier
;;  than any numerical maximum.
(defun find-form-horizontal-stretch (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-stretch-value 0)
	 (max-stretch-inf   0))
     (dolist (link (form-horizontal-links form))
       (when (eq form (link-from link))
	 ;;  Find the values for a given path, then add in the values for the link.
	 (multiple-value-bind (path-value path-inf)
	     (find-path-horizontal-stretch (link-to link) form)
	   (if (eq (link-maximum link) :infinite)
	       (incf path-inf)
	       (incf path-value (- (link-maximum link)
				   (link-length link))))
	   ;;  Maximise the number of :infinites, or the numerical value if :infinites
	   ;;  are equal.
	   (cond ((> path-inf max-stretch-inf)
		  (setq max-stretch-value path-value
			max-stretch-inf   path-inf))
		 ((= path-inf max-stretch-inf)
		  (setq max-stretch-value (max path-value max-stretch-value))))
	   (setf (form-tick (link-to link)) t))))
     ;;  Now do the isolated to-links.
     (dolist (link (form-horizontal-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 ;;  Find the values for a given path, then add in the values for the link.
	 (multiple-value-bind (path-value path-inf)
	     (find-path-horizontal-stretch (link-from link) form t)
	   (if (eq (link-maximum link) :infinite)
	       (incf path-inf)
	       (incf path-value (- (link-maximum link)
				   (link-length link))))
	   ;;  Maximise the number of :infinites, or the numerical value if :infinites
	   ;;  are equal.
	   (cond ((> path-inf max-stretch-inf)
		  (setq max-stretch-value path-value
			max-stretch-inf   path-inf))
		 ((= path-inf max-stretch-inf)
		  (setq max-stretch-value (max path-value max-stretch-value)))))))
     (values max-stretch-value
	     max-stretch-inf)))

(defun find-path-horizontal-stretch (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       (values 0 0)
       (let ((max-stretch-value 0)
	     (max-stretch-inf   0))
	 (dolist (link (contact-constraint contact :horizontal-links))
	   ;;  Find the values for a given path, then add in the values for the link.
	   (when (eq contact (if to-links-p (link-to link) (link-from link)))
	     (multiple-value-bind (path-value path-inf)
		 (find-path-horizontal-stretch (if to-links-p (link-from link) (link-to link))
					       top-level-form
					       to-links-p)
	       (if (eq (link-maximum link) :infinite)
		   (incf path-inf)
		   (incf path-value (- (link-maximum link)
				       (link-length link))))
	       ;;  Maximise the number of :infinites, or the numerical value if :infinites
	       ;;  are equal.
	       (cond ((> path-inf max-stretch-inf)
		      (setq max-stretch-value path-value
			    max-stretch-inf   path-inf))
		     ((= path-inf max-stretch-inf)
		      (setq max-stretch-value (max path-value max-stretch-value)))))))
	 ;;  Add in the values for the contact.
	 (if (eq (form-max-width contact) :infinite)
	     (setq max-stretch-inf (1+ max-stretch-inf))
	     (setq max-stretch-value (+ (- (form-max-width contact)
					   (contact-width contact))
					max-stretch-value)))
	 (values max-stretch-value
		 max-stretch-inf))))

;;  See comments in front of find-form-horizontal-stretch.
(defun find-form-vertical-stretch (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-stretch-value 0)
	 (max-stretch-inf   0))
     (dolist (link (form-vertical-links form))
       (when (eq form (link-from link))
	 ;;  Find the values for a given path, then add in the values for the link.
	 (multiple-value-bind (path-value path-inf)
	     (find-path-vertical-stretch (link-to link) form)
	   (if (eq (link-maximum link) :infinite)
	       (incf path-inf)
	       (incf path-value (- (link-maximum link)
				   (link-length link))))
	   ;;  Maximise the number of :infinites, or the numerical value if :infinites
	   ;;  are equal.
	   (cond ((> path-inf max-stretch-inf)
		  (setq max-stretch-value path-value
			max-stretch-inf   path-inf))
		 ((= path-inf max-stretch-inf)
		  (setq max-stretch-value (max path-value max-stretch-value))))
	   (setf (form-tick (link-to link)) t))))
     (dolist (link (form-vertical-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 ;;  Find the values for a given path, then add in the values for the link.
	 (multiple-value-bind (path-value path-inf)
	     (find-path-vertical-stretch (link-from link) form t)
	   (if (eq (link-maximum link) :infinite)
	       (incf path-inf)
	       (incf path-value (- (link-maximum link)
				   (link-length link))))
	   ;;  Maximise the number of :infinites, or the numerical value if :infinites
	   ;;  are equal.
	   (cond ((> path-inf max-stretch-inf)
		  (setq max-stretch-value path-value
			max-stretch-inf   path-inf))
		 ((= path-inf max-stretch-inf)
		  (setq max-stretch-value (max path-value max-stretch-value)))))))
     (values max-stretch-value
	     max-stretch-inf)))

(defun find-path-vertical-stretch (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       (values 0 0)
       (let ((max-stretch-value 0)
	     (max-stretch-inf   0))
	 (dolist (link (contact-constraint contact :vertical-links))
	   ;;  Find the values for a given path, then add in the values for the link.
	   (when (eq contact (if to-links-p (link-to link) (link-from link)))
	     (multiple-value-bind (path-value path-inf)
		 (find-path-vertical-stretch (if to-links-p (link-from link) (link-to link))
					     top-level-form
					     to-links-p)
	       (if (eq (link-maximum link) :infinite)
		   (incf path-inf)
		   (incf path-value (- (link-maximum link)
				       (link-length link))))
	       ;;  Maximise the number of :infinites, or the numerical value if :infinites
	       ;;  are equal.
	       (cond ((> path-inf max-stretch-inf)
		      (setq max-stretch-value path-value
			    max-stretch-inf   path-inf))
		     ((= path-inf max-stretch-inf)
		      (setq max-stretch-value (max path-value max-stretch-value)))))))
	 ;;  Add in the values for the contact.
	 (if (eq (form-max-height contact) :infinite)
	     (setq max-stretch-inf (1+ max-stretch-inf))
	     (setq max-stretch-value (+ (- (form-max-height contact)
					   (contact-height contact))
					max-stretch-value)))
	 (values max-stretch-value
		 max-stretch-inf))))

;;  Shrink is defined as the difference between the current size and the minimum
;;  size.  These functions find the minimum shrink across the link graph.
(defun find-form-horizontal-shrink (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-shrink 0))
     (dolist (link (form-horizontal-links form))
       (when (eq form (link-from link))
	 (let* ((next-contact (link-to link))
		(path-value (+ (- (link-length link)
				  (link-minimum link))
			       (find-path-horizontal-shrink next-contact form))))
	   (setq max-shrink (max max-shrink path-value))
	   (setf (form-tick next-contact) t))))
     (dolist (link (form-horizontal-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 (let* ((next-contact (link-from link))
		(path-value (+ (- (link-length link)
				  (link-minimum link))
			       (find-path-horizontal-shrink next-contact form t))))
	   (setq max-shrink (max max-shrink path-value)))))
     max-shrink))

(defun find-path-horizontal-shrink (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       0
       (+ (- (contact-width contact)
	     (form-min-width contact))
	  (let ((max-shrink 0))
	    (dolist (link (contact-constraint contact :horizontal-links)
			  max-shrink)
	      (when (eq contact (if to-links-p (link-to link) (link-from link)))
		(let* ((next-contact (if to-links-p (link-from link) (link-to link)))
		       (path-value (+ (- (link-length link)
					 (link-minimum link))
				      (find-path-horizontal-shrink next-contact top-level-form to-links-p))))
		  (setq max-shrink (max max-shrink path-value)))))))))

(defun find-form-vertical-shrink (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-shrink 0))
     (dolist (link (form-vertical-links form))
       (when (eq form (link-from link))
	 (let* ((next-contact (link-to link))
		(path-value (+ (- (link-length link)
				  (link-minimum link))
			       (find-path-vertical-shrink next-contact form))))
	   (setq max-shrink (max max-shrink path-value))
	   (setf (form-tick next-contact) t))))
     (dolist (link (form-vertical-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 (let* ((next-contact (link-from link))
		(path-value (+ (- (link-length link)
				  (link-minimum link))
			       (find-path-vertical-shrink next-contact form t))))
	   (setq max-shrink (max max-shrink path-value)))))
     max-shrink))

(defun find-path-vertical-shrink (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       0
       (+ (- (contact-height contact)
	     (form-min-height contact))
	  (let ((max-shrink 0))
	    (dolist (link (contact-constraint contact :vertical-links)
			  max-shrink)
	      (when (eq contact (if to-links-p (link-to link) (link-from link)))
		(let* ((next-contact (if to-links-p (link-from link) (link-to link)))
		       (path-value (+ (- (link-length link)
					 (link-minimum link))
				      (find-path-vertical-shrink next-contact top-level-form to-links-p))))
		  (setq max-shrink (max max-shrink path-value)))))))))

;;  This function-pair is like find-form-ideal-width except that it uses
;;  contact-tentative-width instead of contact-width and link-tentative-length
;;  instead of link-length.  It's used in manage-geometry to determine if
;;  the desired changes will cause a change in the Form's size.
(defun find-form-tentative-width (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-width 0))
     (dolist (link (form-horizontal-links form))
       (when (eq form (link-from link))
	 ;;  Path-value is the length of the link, corrected according to where
	 ;;  it attaches to the next contact (ie, the distance to the left edge
	 ;;  of the next contact), plus the value of the maximum path starting
	 ;;  at that contact.
	 (let ((path-value (+ (link-tentative-length link)
			      (link-tentative-horizontal-attach-to-correction link)
			      (find-path-tentative-width (link-to link) form))))
	   (when (eq (link-attach-from link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-width (max max-width path-value))
	   (setf (form-tick (link-to link)) t))))
     (dolist (link (form-horizontal-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 ;;  Path-value is the length of the link, corrected according to where
	 ;;  it attaches to the next contact (ie, the distance to the left edge
	 ;;  of the next contact), plus the value of the maximum path starting
	 ;;  at that contact.
	 (let ((path-value (+ (link-tentative-length link)
			      (link-tentative-horizontal-attach-from-correction link)
			      (find-path-tentative-width (link-from link) form t))))
	   (when (eq (link-attach-to link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-width (max max-width path-value)))))
     max-width))

(defun find-path-tentative-width (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       0					; Back at the parent Form, end of path.
       (+ (contact-tentative-width contact)
	  (contact-border-width contact)
	  (contact-border-width contact)
	  (let ((max-width 0))
	    (dolist (link (contact-constraint contact :horizontal-links)
			  max-width)
	      (when (eq contact (if to-links-p (link-to link) (link-from link)))
		(let* ((next-contact (if to-links-p (link-from link) (link-to link)))
		       (path-value (+ (if to-links-p
					  (link-tentative-horizontal-attach-to-correction link)
					  (link-tentative-horizontal-attach-from-correction link))
				      (link-tentative-length link)
				      ;;  Don't compensate when attaching to form.
				      (if (eq next-contact top-level-form)
					  0
					  (if to-links-p
					      (link-tentative-horizontal-attach-from-correction link)
					      (link-tentative-horizontal-attach-to-correction link)))
				      (find-path-tentative-width next-contact top-level-form to-links-p))))
		  (setq max-width (max max-width path-value)))))))))

(defun link-tentative-horizontal-attach-to-correction (link)
   (let ((next-contact (link-to link)))
     (ecase (link-attach-to link)
       (:left 0)
       (:center (- (+ (round (contact-tentative-width next-contact) 2)
		      (contact-border-width next-contact))))
       (:right (- (+ (contact-tentative-width next-contact)
		     (contact-border-width next-contact)
		     (contact-border-width next-contact)))))))

(defun link-tentative-horizontal-attach-from-correction (link)
   (let ((contact (link-from link)))
     (ecase (link-attach-from link)
       (:left (- (+ (contact-tentative-width contact)
		    (contact-border-width contact)
		    (contact-border-width contact))))
       (:center (- (+ (round (contact-tentative-width contact) 2)
		      (contact-border-width contact))))
       (:right 0))))

;;  This function-pair is like find-form-ideal-height except that it uses
;;  tentative heights, etc, like find-form-tentative-width.
(defun find-form-tentative-height (form)
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))
   (let ((max-height 0))
     (dolist (link (form-vertical-links form))
       (when (eq form (link-from link))
	 ;;  Path-value is the length of the link, corrected according to where
	 ;;  it attaches to the next contact, plus the value of the maximum path
	 ;;  starting at that contact.
	 (let ((path-value (+ (link-tentative-length link)
			      (link-tentative-vertical-attach-to-correction link)
			      (find-path-tentative-height (link-to link) form))))
	   (when (eq (link-attach-from link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-height (max max-height path-value))
	   (setf (form-tick (link-to link)) t))))
     (dolist (link (form-vertical-links form))
       (when (and (eq form (link-to link))
		  (null (form-tick (link-from link))))
	 ;;  Path-value is the length of the link, corrected according to where
	 ;;  it attaches to the next contact, plus the value of the maximum path
	 ;;  starting at that contact.
	 (let ((path-value (+ (link-tentative-length link)
			      (link-tentative-vertical-attach-from-correction link)
			      (find-path-tentative-height (link-from link) form t))))
	   (when (eq (link-attach-to link) :center)
	     (setq path-value (* 2 path-value)))
	   (setq max-height (max max-height path-value)))))
     max-height))

(defun find-path-tentative-height (contact top-level-form &optional to-links-p)
   (if (eq contact top-level-form)
       0
       (+ (contact-tentative-height contact)
	  (contact-border-width contact)
	  (contact-border-width contact)
	  (let ((max-height 0))
	    (dolist (link (contact-constraint contact :vertical-links)
			  max-height)
	      (when (eq contact (if to-links-p (link-to link) (link-from link)))
		(let* ((next-contact (if to-links-p (link-from link) (link-to link)))
		       (path-value (+ (if to-links-p
					  (link-tentative-vertical-attach-to-correction link)
					  (link-tentative-vertical-attach-from-correction link))
				      (link-tentative-length link)
				      ;;  Don't compensate when attaching to form.
				      (if (eq next-contact top-level-form)
					  0
					  (if to-links-p
					      (link-tentative-vertical-attach-from-correction link)
					      (link-tentative-vertical-attach-to-correction link)))
				      (find-path-tentative-height next-contact top-level-form to-links-p))))
		  (setq max-height (max max-height path-value)))))))))

(defun link-tentative-vertical-attach-to-correction (link)
   (let ((next-contact (link-to link)))
     (ecase (link-attach-to link)
       (:top 0)
       (:center (- (+ (round (contact-tentative-height next-contact) 2)
		      (contact-border-width next-contact))))
       (:bottom (- (+ (contact-tentative-height next-contact)
		      (contact-border-width next-contact)
		      (contact-border-width next-contact)))))))

(defun link-tentative-vertical-attach-from-correction (link)
   (let ((contact (link-from link)))
     (ecase (link-attach-from link)
       (:top (- (+ (contact-tentative-height contact)
		   (contact-border-width contact)
		   (contact-border-width contact))))
       (:center (- (+ (round (contact-tentative-height contact) 2)
		      (contact-border-width contact))))
       (:bottom 0))))


;;;
;;;  Abstractions of length comparisons that honor the :infinite length.

(defun length<= (length &rest lengths)
  (do ((a length c)
       (b lengths (cdr b))
       (c))
      ((null b) t)
    (setq c (car b))
    (if (and (not (eq c :infinite))
	     (or (eq a :infinite)
		 (> a c)))
	(return nil))))

(defun length> (length-1 length-2)
   (cond ((eq length-2 :infinite)
	  nil)
	 ((eq length-1 :infinite)
	  t)
	 ((> length-1 length-2))))

(defun length-min (length-1 length-2)
   (cond ((eq length-1 :infinite)
	  length-2)
	 ((eq length-2 :infinite)
	  length-1)
	 ((< length-1 length-2)
	  length-1)
	 (:else
	  length-2)))


;;;
;;;  Form characteristic methods.  Below are preferred-size, change-layout,
;;;  resize, and manage-geometry.  These guys provide the interface that
;;;  uses the functions above.

;;  If a what-if size is supplied, try it and return the tentative width and height.
;;  If not, just return the current "ideal" width and height.
;;  +++ Not thoroughly tested.
(defmethod preferred-size ((form form) &key width height border-width)
   (with-slots ((form-width  width)
		(form-height height))
	       form
     (let ((pref-width  (find-form-ideal-width form))
	   (pref-height (find-form-ideal-height form)))
       (if (or (and width (/= width form-width))
	       (and height (/= height form-height)))
	   ;;  Wants to try a new width and/or height.
	   (let ((new-form-width  (or width form-width))
		 (new-form-height (or height form-height)))
	     ;;  Try the new placement, changing only tentative values.
	     (clear-tentative-values form)
	     (place-and-size-children-internal
	       form
	       (- new-form-width  (max pref-width form-width))
	       (- new-form-height (max pref-height form-height)))
	     (values (find-form-tentative-width form)
		     (find-form-tentative-height form)
		     (or border-width (contact-border-width form))))
	   (values pref-width
		   pref-height
		   (or border-width (contact-border-width form)))))))


;;  Several consistency checks happen in change-layout:  (1) Look for circular
;;  links, ie, cases where links among contacts form a loop.  (2) Look for cases
;;  where children haven't been given a size and ensure that they are sized
;;  initially such that their constraints are satisfied -- for example, the Form
;;  has a specified size and the children should be sized to fit it and the links.
;;  (3) Look for links where the length does not equal the distance between the
;;  endpoints -- this is an inconsistency that should either signal an error or
;;  cause some link-stretching.  (2) and (3) happen somewhat together in
;;  adjust-sizes-to-fit, called from place-and-size-children.

(defmethod change-layout ((form form) &optional newly-managed)
   (declare (type (or null contact) newly-managed))

   ;;  Convert any initarg link-specs into links.
   (resolve-initial-links form)

   ;;  Check for and handle a single child being unmapped...
   (when (and newly-managed
	      (eq (contact-state newly-managed) :withdrawn))
     )

   ;;  If there are any circular link paths, error here.
   (check-for-circular-links form)

   ;;  Ensure that children have sizes that fit within their size constraints.
   (set-initial-child-sizes form)

   ;;  Set the Form's initial size, when necessary, then place and adjust the children.
   (multiple-value-bind (pref-width pref-height)
       (preferred-size form)
     (cond ((and (not (realized-p form))
		 (or (zerop (contact-height form))
		     (zerop (contact-width form))))
	    ;;  Form's dimensions are uninitialised:  Take the preferred size of the whole,
	    ;;  then place the children where they want to be.  Supply the difference
	    ;;  between the preferred size and the actual size in case the Form didn't
	    ;;  get the size it requested.
	    (change-geometry form :width pref-width :height pref-height :accept-p t)
	    (place-and-size-children form
				     (- (contact-width form) pref-width)
				     (- (contact-height form) pref-height)
				     t))
	   ((and (not (zerop (contact-height form)))
		 (not (zerop (contact-width form)))
		 (or (/= (contact-width form) pref-width)
		     (/= (contact-height form) pref-height)))
	    ;;  Form has a size, and it's different than the preferred size of the whole.
	    ;;  Resize the children to match.
	    (place-and-size-children form
				     (- (contact-width form) pref-width)
				     (- (contact-height form) pref-height)
				     t))
	   (:else
	    ;;  Either the Form has the same size as the children want, or some other case.
	    ;;  Just place the children.
	    (place-and-size-children form nil nil t)))))

;;  Check for links set up as initargs and not yet resolved.  Instead of a link
;;  object, the link will be a list, an argument list to make-horizontal-link or
;;  make-vertical-link, with contact-names instead of contacts.  Find the contacts
;;  and make the links.
(defun resolve-initial-links (form)
   (declare (type form form))
   (with-slots (horizontal-links vertical-links children name) form
     (check-type horizontal-links list)
     (check-type vertical-links	  list)

     ;;  Note that links can't be defstructs of :type :list or this test won't work.
     (let ((link-specs (remove-if-not #'listp horizontal-links)))
       (setq horizontal-links (nset-difference horizontal-links link-specs))
       (dolist (spec link-specs)
	 ;;  A spec instead of a link, delete it and do the make-link.  We make one pass
	 ;;  through all the links this way because it's possible to specify link-specs
	 ;;  in the initargs and do make-link later and have both coexist until realisation.
	 (let* ((from-name (getf spec :from))
		(to-name   (getf spec :to))
		(from (if (eq name from-name)
			  form
			  (find from-name children :key #'contact-name)))
		(to (if (eq name to-name)
			form
			(find to-name children :key #'contact-name))))
	   (if (or (null from) (null to))
	       (error "Link spec referred to nonexistent contact:  ~S" spec)
	       (apply #'make-horizontal-link :from from :to to spec)))))

     ;;  Note that links can't be defstructs of :type :list or this test won't work.
     (let ((link-specs (remove-if-not #'listp vertical-links)))
       (setq vertical-links (nset-difference vertical-links link-specs))
       (dolist (spec link-specs)
	 ;;  A spec instead of a link, delete it and do the make-link.  We make one pass
	 ;;  through all the links this way because it's possible to specify link-specs
	 ;;  in the initargs and do make-link later and have both coexist until realisation.
	 (let* ((from-name (getf spec :from))
		(to-name   (getf spec :to))
		(from (if (eq name from-name)
			  form
			  (find from-name children :key #'contact-name)))
		(to (if (eq name to-name)
			form
			(find to-name children :key #'contact-name))))
	   (if (or (null from) (null to))
	       (error "Link spec referred to nonexistent contact:  ~S" spec)
	       (apply #'make-vertical-link :from from :to to spec))))))

   ;;  If there are subgraphs that have no ultimate link connection to the Form,
   ;;  we add implicit 0-to-infinite links, so they'll play a part in the sizing
   ;;  algorithm.
   (add-implicit-links-if-needed form))

(defun add-implicit-links-if-needed (form)
   (declare (type form form))
   (labels ((mark-link-path (contact top-level-form link-type)
	      (unless (or (eq contact top-level-form)
			  (form-tick contact))
		(setf (form-tick contact) t)
		(dolist (link (contact-constraint contact link-type))
		  (mark-link-path (link-from link) top-level-form link-type)
		  (mark-link-path (link-to link) top-level-form link-type)))))

     (with-slots (horizontal-links vertical-links children) form
       ;;  The first thing we do is flush any existing implicit links,
       ;;  in case change-layout was called because of a new child with
       ;;  links we don't want to interfere with.
       (dolist (child children)
	 (dolist (link (contact-constraint child :horizontal-links))
	   (when (link-implicit-p link)
	     (destroy link)))
	 (dolist (link (contact-constraint child :vertical-links))
	   (when (link-implicit-p link)
	     (destroy link))))

       ;;  Then we walk the link graph, marking children as we go.
       ;;  We add implicit links to the unmarked children that don't have links
       ;;  in a given direction.
       (dolist (child children)
	 (setf (form-tick child) nil))
       (dolist (link horizontal-links)
	 (mark-link-path (link-from link) form :horizontal-links)
	 (mark-link-path (link-to link)	  form :horizontal-links))
       (dolist (child children)
	 (unless (form-tick child)
	   (let ((to-p nil)
		 (from-p nil))
	     (dolist (link (contact-constraint child :horizontal-links))
	       (cond ((eq child (link-from link))
		      (setq from-p t))
		     ((eq child (link-to link))
		      (setq to-p t))))
	     (when (null from-p)		; Add a to-link.
	       (let ((new-link (make-horizontal-link :from child :to form :attach-to :right)))
		 (setf (link-implicit-p new-link) t)))
	     (when (null to-p)			; Add a from-link.
	       (let ((new-link (make-horizontal-link :from form :to child :attach-from :left)))
		 (setf (link-implicit-p new-link) t))))))

       ;;  Again for the verticals.
       (dolist (child children)
	 (setf (form-tick child) nil))
       (dolist (link vertical-links)
	 (mark-link-path (link-from link) form :vertical-links)
	 (mark-link-path (link-to link)	  form :vertical-links))
       (dolist (child children)
	 (unless (form-tick child)
	   (let ((to-p nil)
		 (from-p nil))
	     (dolist (link (contact-constraint child :vertical-links))
	       (cond ((eq child (link-from link))
		      (setq from-p t))
		     ((eq child (link-to link))
		      (setq to-p t))))
	     (when (null from-p)		; Add a to-link.
	       (let ((new-link (make-vertical-link :from child :to form :attach-to :bottom)))
		 (setf (link-implicit-p new-link) t)))
	     (when (null to-p)			; Add a from-link.
	       (let ((new-link (make-vertical-link :from form :to child :attach-from :top)))
		 (setf (link-implicit-p new-link) t)))))))))


;;  The circularity check is a simple traversal of first the horizontal
;;  links and then the vertical links.  For each, we travel to all the contacts
;;  in depth-first order, marking contacts as we see them.  If we see a marked
;;  contact, we've found a circle and error.  We undo the marks as we backtrack,
;;  to allow the possibility of multiple non-circular paths to the same contact.
(defun check-for-circular-links (form)
   (labels ((check-for-circular-horizontal-links (form)
               ;;  Form-tick is used to mark contacts as they are visited.
	       (dolist (contact (composite-children form))
		 (setf (form-tick contact) nil))
	       (dolist (link (form-horizontal-links form))
		 (when (eq form (link-from link))
		   (check-for-circular-horizontal-links-path (link-to link) form))))

	    (check-for-circular-horizontal-links-path (contact top-level-form)
	       (unless (eq contact top-level-form)		; Back at the parent Form, end of path.
		 (when (form-tick contact)
		   (error "Circular horizontal-link path found at ~S." contact))
		 (setf (form-tick contact) t)
		 (dolist (link (contact-constraint contact :horizontal-links))
		   (when (eq contact (link-from link))
		     (check-for-circular-horizontal-links-path (link-to link) top-level-form)))
		 (setf (form-tick contact) nil)))

	    (check-for-circular-vertical-links (form)
               ;;  Form-tick is used to mark contacts as they are visited.
	       (dolist (contact (composite-children form))
		 (setf (form-tick contact) nil))
	       (dolist (link (form-vertical-links form))
		 (when (eq form (link-from link))
		   (check-for-circular-vertical-links-path (link-to link) form))))

	    (check-for-circular-vertical-links-path (contact top-level-form)
	       (unless (eq contact top-level-form)		; Back at the parent Form, end of path.
		 (when (form-tick contact)
		   (error "Circular vertical-link path found at ~S." contact))
		 (setf (form-tick contact) t)
		 (dolist (link (contact-constraint contact :vertical-links))
		   (when (eq contact (link-from link))
		     (check-for-circular-vertical-links-path (link-to link) top-level-form)))
		 (setf (form-tick contact) nil))))

     (check-for-circular-horizontal-links form)
     (check-for-circular-vertical-links form)))

;;  Set initial sizes.  Ensure that the children (a) have widths and heights and
;;  (b) those widths and heights are within their constraints.
;;
;;  Case A:  child has size -- if within constraints, fine, else set it
;;  to the minimum or maximum (whichever is nearer).  Case B:  child doesn't
;;  have a size (ie, dimensions of zero) -- set size to min if present, or
;;  preferred-size if not, because form-min-width and form-min-height will
;;  default to the current size if not specified.
(defun set-initial-child-sizes (form)
   (with-slots (children) form
     (dolist (child children)
       (if (or (zerop (contact-width child))
	       (zerop (contact-height child)))
	   ;;  No size given, take the minimum if there is one and the preferred
	   ;;  if there isn't.
	   (multiple-value-bind (pref-width pref-height)
	       (preferred-size child)
	     (resize child
		     (if (zerop (contact-width child))
			 (if (zerop (form-min-width child))
			     pref-width
			     (form-min-width child))
			 (contact-width child))
		     (if (zerop (contact-height child))
			 (if (zerop (form-min-height child))
			     pref-height
			     (form-min-height child))
			 (contact-height child))
		     (contact-border-width child)))
	   ;;  Size given, resize the child if it exceeds its constraints.
	   (when (or (not (length<= (form-min-width child)
				    (contact-width child)
				    (form-max-width child)))
		     (not (length<= (form-min-height child)
				    (contact-height child)
				    (form-max-height child))))
	     (resize child
		     (max (length-min (contact-width child)	; +++ Should this be preferred or current?
				      (form-max-width child))
			  (form-min-width child))
		     (max (length-min (contact-height child)
				      (form-max-height child))
			  (form-min-height child))
		     (contact-border-width child))))

       ;;  If there isn't a minimum size specified, make it be the initial size
       ;;  so later resizes won't forget it.
       (when (null (contact-constraint child :min-width))
	 (setf (form-min-width child) (contact-width child)))
       (when (null (contact-constraint child :min-height))
	 (setf (form-min-height child) (contact-height child))))))

;;  Idea borrowed from property-sheet.  Catch the preferred-size and the current size,
;;  go do the resize on the Form, then adjust the children according to the difference
;;  between the new size and the old.  Taking the larger of preferred-size and initial
;;  size ensures that the children don't try to grow or move until the Form is larger
;;  than the minimum as set by the children's constraints.
(defmethod resize :around ((form form) width height border-width)
  (let ((initial-width (contact-width form))
	(initial-height (contact-height form)))
    (multiple-value-bind (pw ph)
	(preferred-size form)
      (let ((resized-p (call-next-method)))
	(unless (or (zerop initial-width)	       ; To avoid startup glitches.
		    (zerop initial-height)
		    (getf (window-plist form) 'in-manage-geometry))
	  (place-and-size-children form
				   (- width  (max pw initial-width))
				   (- height (max ph initial-height))))
	resized-p))))


;;  The algorithm:  If the requested size change can happen entirely
;;  without disturbing the other children or the Form (ie, a shrink within
;;  the limits of the link stretchability, or a grow that doesn't push the
;;  neighbors aside), then do it and adjust the links accordingly.  If not,
;;  see if a position change, still within the limits of the links, will
;;  allow the size change, and return that as a compromise geometry if it
;;  works.  If neither idea works, treat the situation as an initial change-layout
;;  with new initial conditions (two subcases here, depending on whether or
;;  not the Form has to change size to accommodate the change).
;;
;;  More of the algorithm:  When leading up to a change-layout, use a variant
;;  of find-ideal-form-width, etc, that looks at the tentative size, flushing
;;  all the tentative sizes except the changing child's, and use that size to
;;  determine whether or not the Form needs to change size.  Use that answer
;;  to either call change-geometry upwards or not, then do change-layout just
;;  before returning if we're going to approve it.  If we're not going to
;;  approve, return NIL and maybe whatever size we could handle (or the original
;;  size, or nothing at all).
(defmethod manage-geometry ((form form) child x y width height border-width &key)
   (with-slots ((child-width width)
		(child-height height)
		(child-border-width border-width)
		(child-x x)
		(child-y y))
	       child
     (let* ((approved-p             t)
	    (total-width            (+ child-width child-border-width child-border-width))
	    (total-height           (+ child-height child-border-width child-border-width))
	    (requested-width        (or width child-width))
	    (requested-height       (or height child-height))
	    (requested-border-width (or border-width child-border-width))
	    (requested-x	    (or x child-x))
	    (requested-y	    (or y child-y))
	    (new-total-width        (+ requested-width requested-border-width requested-border-width))
	    (new-total-height       (+ requested-height requested-border-width requested-border-width)))

       ;;  Check if requested size change fits within size constraints.  If not,
       ;;  disapprove and limit it to within them.
       (when (or (not (length<= (form-min-width child) requested-width (form-max-width child)))
		 (not (length<= (form-min-height child) requested-height (form-max-height child))))
	 (setq approved-p nil)			; Tried to exceed size constraints.
	 (setq requested-width  (max (length-min requested-width
						 (form-max-width child))
				     (form-min-width child))
	       requested-height (max (length-min requested-height
						 (form-max-height child))
				     (form-min-height child))))

       ;;  Check if the change can be done without affecting any other children.
       ;;  If so, allow the change and modify the links accordingly;  if not,
       ;;  go back to square one and do change-layout.
       (let ((delta-left     (- requested-x child-x))	; Calculate changes in attach-points.
	     (delta-top      (- requested-y child-y))
	     (delta-right    (- (+ requested-x new-total-width)
				(+ child-x total-width)))
	     (delta-bottom   (- (+ requested-y new-total-height)
				(+ child-y total-height)))
	     (delta-h-center (- (round (+ requested-x new-total-width) 2)
				(round (+ child-x total-width) 2)))
	     (delta-v-center (- (round (+ requested-y new-total-height) 2)
				(round (+ child-y total-height) 2)))
	     (left-excess    0)
	     (right-excess   0)
	     (top-excess     0)
	     (bottom-excess  0))

	 (labels ((punt ()
		    ;;  If we give up completely, disapprove and return the original
		    ;;  geometry as the compromise.
		    (setq approved-p	         nil
			  requested-height	 child-height
			  requested-width	 child-width
			  requested-x		 child-x
			  requested-y		 child-y
			  requested-border-width child-border-width))

		  ;;  These next two functions either add or subtract the deltas from the link-lengths
		  ;;  based on the direction of the link.  There's not a lot of theory behind it,
		  ;;  but it has to do with the meaning of a positive length.
		  (tentative-link-length-horizontal (link)
		    (if (eq child (link-to link))
			(+ (link-length link)
			   (ecase (link-attach-to link)
			     (:left   delta-left)
			     (:center delta-h-center)
			     (:right  delta-right)))
			(- (link-length link)
			   (ecase (link-attach-from link)
			     (:left   delta-left)
			     (:center delta-h-center)
			     (:right  delta-right)))))

		  (tentative-link-length-vertical (link)
		    (if (eq child (link-to link))
			(+ (link-length link)
			   (ecase (link-attach-to link)
			     (:top    delta-top)
			     (:center delta-v-center)
			     (:bottom delta-bottom)))
			(- (link-length link)
			   (ecase (link-attach-from link)
			     (:top    delta-top)
			     (:center delta-v-center)
			     (:bottom delta-bottom)))))

		  (manage-geometry-hard-case ()
                    ;;  If all else fails, come here and do most of what change-layout does,
		    ;;  using the child's requested geometry, and see if it works out.
		    (clear-tentative-values form)
		    (setf (contact-tentative-width child)  requested-width)
		    (setf (contact-tentative-height child) requested-height)
		    (setf (contact-tentative-x child) requested-x)
		    (setf (contact-tentative-y child) requested-y)
		    (let ((new-form-width  (find-form-tentative-width form))
			  (new-form-height (find-form-tentative-height form)))
		      (unless (and (= new-form-width  (contact-width form))
				   (= new-form-height (contact-height form)))
			;;  Form has to change size.  The first thing to do is to pretend to
			;;  change it, using form-projected-width and form-projected-height,
			;;  and see if any other child's links will be violated (they may be
			;;  changed, and/or the children's sizes may, but they're not allowed
			;;  to exceed their constraints).
			;;
			;;  If none are, go ahead and try the resize.  If there's a violation,
			;;  punt, because we've tried to change the whole layout to accomodate
			;;  the change and still can't satisfy the constraints.
			(setf (form-projected-height form) new-form-height)
			(setf (form-projected-width form)  new-form-width)
			(place-and-size-children-internal form nil nil)	; Do the tentative placements.
			(let ((link-change-okay? (null (adjust-sizes-to-fit form))))
			  (cond ((or (/= (contact-tentative-width child) requested-width)
				     (/= (contact-tentative-height child) requested-height))
				 ;;  The attempted layout would change the size of the child, so
				 ;;  try again with the changed size and return that result (with
				 ;;  NIL for approved-p because the changed size means disapproval).
				 (multiple-value-setq (approved-p requested-x requested-y
						       requested-width requested-height requested-border-width)
				   (manage-geometry form child
						    (contact-tentative-x child)
						    (contact-tentative-y child)
						    (contact-tentative-width child)
						    (contact-tentative-height child)
						    requested-border-width))
				 (setq approved-p nil))
				(link-change-okay?
				 ;;  No links violated, go try to change size.  The "in-manage-geometry" flag
				 ;;  will prevent the resize from calling place-and-size-children, which
				 ;;  would scramble our efforts and flush the tentative values.  It's a
				 ;;  flag instead of a special variable because it's specific to one window.
				 ;;
				 ;;  +++ Note that we only care about the first value of change-geometry, because
				 ;;      at this point we aren't trying to handle partial resizes.  When/if we do,
				 ;;      this'll change to a multiple-value-bind.
				 (let ((form-approved-p
					 (unwind-protect
					     (progn
					       (setf (getf (window-plist form) 'in-manage-geometry) t)
;					       (change-geometry form :width new-form-width :height new-form-height)
					       (manage-geometry (contact-parent form) form nil nil
								new-form-width new-form-height nil)
					       )
					   (setf (getf (window-plist form) 'in-manage-geometry) nil))))
				   (when (not form-approved-p)
				     ;;  New Form size not approved.
				     ;;  +++ A smoother solution would let the child have part of its request,
				     ;;      but for the moment we'll just refuse it completely.
				     (punt))))
				(:else
				 ;;  Can't handle the link change, so, for now, punt.  Disapprove the change
				 ;;  and return the original size as the compromise.
				 (punt)))))
		      ;;  If, after all that, we approve, do the changes now.
		      (when approved-p
			(cond ((form-projected-height form)
			       ;;  Form has to change size, so include it in the approval function.
			       (setq approved-p #'(lambda (form)
						    (unwind-protect
							(progn
							  (setf (getf (window-plist form) 'in-manage-geometry) t)
							  (change-geometry form
									   :width new-form-width
									   :height new-form-height))
						      (setf (getf (window-plist form) 'in-manage-geometry) nil))
						    (really-change-the-children form))))
			      (:else
			       ;;  Non-NIL only when Form changed size, thus when place-and-size
			       ;;  has already been done.
			       (setf (form-projected-height form) nil)
			       (setf (form-projected-width form)  nil)
			       (place-and-size-children-internal form nil nil)
			       (setq approved-p #'really-change-the-children))))
		      (values approved-p
			      requested-x
			      requested-y
			      requested-width
			      requested-height
			      requested-border-width))))

	   ;;  Calculate the amount that the new geometry causes the link constraints
	   ;;  to be exceeded.  If it's all zero, no link's constraints are exceeded
	   ;;  and we can proceed without disturbing anyone.
	   (dolist (link (contact-constraint child :horizontal-links))
	     (let ((tentative-link-length (tentative-link-length-horizontal link)))
	       (if (or (and (eq child (link-to link))
			    (> tentative-link-length 0))
		       (and (eq child (link-from link))
			    (< tentative-link-length 0)))
		   ;;  Note the implicit assumption that we won't simultaneously exceed both
		   ;;  the minimum and the maximum in a given direction.
		   (cond ((< tentative-link-length (link-minimum link))
			  (setq left-excess (min left-excess
						 (- tentative-link-length (link-minimum link)))))
			 ((length> tentative-link-length (link-maximum link))
			  (setq left-excess (max left-excess
						 (- tentative-link-length (link-maximum link))))))
		   (cond ((< tentative-link-length (link-minimum link))
			  (setq right-excess (min right-excess
						  (- tentative-link-length (link-minimum link)))))
			 ((length> tentative-link-length (link-maximum link))
			  (setq right-excess (max right-excess
						  (- tentative-link-length (link-maximum link)))))))))
	   (dolist (link (contact-constraint child :vertical-links))
	     (let ((tentative-link-length (tentative-link-length-vertical link)))
	       (if (or (and (eq child (link-to link))
			    (> tentative-link-length 0))
		       (and (eq child (link-from link))
			    (< tentative-link-length 0)))
		   ;;  Note the implicit assumption that we won't simultaneously exceed both
		   ;;  the minimum and the maximum in a given direction.
		   (cond ((< tentative-link-length (link-minimum link))
			  (setq top-excess (min top-excess
						(- tentative-link-length (link-minimum link)))))
			 ((length> tentative-link-length (link-maximum link))
			  (setq top-excess (max top-excess
						(- tentative-link-length (link-maximum link))))))
		   (cond ((< tentative-link-length (link-minimum link))
			  (setq bottom-excess (min bottom-excess
						   (- tentative-link-length (link-minimum link)))))
			 ((length> tentative-link-length (link-maximum link))
			  (setq bottom-excess (max bottom-excess
						   (- tentative-link-length (link-maximum link)))))))))
	     
	   (cond ((and (zerop left-excess)
		       (zerop right-excess)
		       (zerop top-excess)
		       (zerop bottom-excess))
		  ;;  Okay, the proposed size and placement won't strain any links.
		  ;;  Approve the change without affecting anyone else (bearing in mind
		  ;;  that the size may have been constrained above).
		  (when approved-p
		    ;;  Since we're approving, set up the child for the geometry change.
		    (clear-tentative-values form)
		    (setf (contact-tentative-width child)  requested-width)
		    (setf (contact-tentative-height child) requested-height)
		    (setf (contact-tentative-x child) requested-x)
		    (setf (contact-tentative-y child) requested-y)

		    ;;  We're about to approve fully.  Modify the links to fit the new
		    ;;  size and placement of the child, basically the same loops as above
		    ;;  but for effect rather than verification.
		    (dolist (link (contact-constraint child :horizontal-links))
		      (setf (link-tentative-length link) (tentative-link-length-horizontal link)))
		    (dolist (link (contact-constraint child :vertical-links))
		      (setf (link-tentative-length link) (tentative-link-length-vertical link)))
		    
		    ;;  We're approving fully, so actually do the change.
		    (really-change-the-children form))
		  
		  ;;  All done, return the indicated values.
		  (values approved-p
			  requested-x
			  requested-y
			  requested-width
			  requested-height
			  requested-border-width))
		 
		 ((and (or (zerop left-excess)
			   (zerop right-excess)
			   (and (minusp left-excess)
				(plusp right-excess))
			   (and (plusp left-excess)
				(minusp right-excess)))
		       (or (zerop top-excess)
			   (zerop bottom-excess)
			   (and (minusp top-excess)
				(plusp bottom-excess))
			   (and (plusp top-excess)
				(minusp bottom-excess))))
		  ;;  Okay, we exceed one side in one direction and the other side either not
		  ;;  at all or in the other direction.  Try moving enough to handle the excess,
		  ;;  and if we don't violate any of the child's links with the new position,
		  ;;  disapprove but return the new position as a compromise.
		  (let ((x-change (if (> (abs left-excess)
					 (abs right-excess))
				      left-excess
				      right-excess))
			(y-change (if (> (abs top-excess)
					 (abs bottom-excess))
				      top-excess
				      bottom-excess)))
		    (setq delta-left     (if (> left-excess 0)
					     (- delta-left x-change)
					     (+ delta-left x-change))
			  delta-right    (if (> left-excess 0)
					     (- delta-right x-change)
					     (+ delta-right x-change))
			  delta-h-center (if (> left-excess 0)
					     (- delta-h-center x-change)
					     (+ delta-h-center x-change))
			  delta-top      (if (> top-excess 0)
					     (- delta-top y-change)
					     (+ delta-top y-change))
			  delta-bottom   (if (> top-excess 0)
					     (- delta-bottom y-change)
					     (+ delta-bottom y-change))
			  delta-v-center (if (> top-excess 0)
					     (- delta-v-center y-change)
					     (+ delta-v-center y-change)))

		    ;;  Check if the new deltas cause any link violations.  If not, return
		    ;;  the new position and the requested size as the compromise geometry.
		    ;;  If so, go to the hard case.
		    (if (and (dolist (link (contact-constraint child :horizontal-links)
					   t)
			       (let ((tentative-link-length (tentative-link-length-horizontal link)))
				 ;;  Projected link length exceeds limits, return NIL now.
				 ;;  If all link projections work, the DOLIST will return T.
				 (unless (length<= (link-minimum link)
						   tentative-link-length
						   (link-maximum link))
				   (return nil))))
			     (dolist (link (contact-constraint child :vertical-links)
					   t)
			       (let ((tentative-link-length (tentative-link-length-vertical link)))
				 ;;  Projected link length exceeds limits, return NIL now.
				 ;;  If all link projections work, the DOLIST will return T.
				 (unless (length<= (link-minimum link)
						   tentative-link-length
						   (link-maximum link))
				   (return nil)))))
			(values nil		; Can't approve, because we moved it.
				(if (> left-excess 0)
				    (- requested-x x-change)
				    (+ requested-x x-change))
				(if (> top-excess 0)
				    (- requested-y y-change)
				    (+ requested-y y-change))
				requested-width
				requested-height
				requested-border-width)
			(manage-geometry-hard-case))))
		 
		 (:else
		  ;;  The "hard" case -- the proposed change would violate one or more of
		  ;;  the links, so essentially do change-layout again with the requested
		  ;;  size and position as the new initial conditions of the child.
		  (manage-geometry-hard-case))))))))

;;  Checks for links whose endpoints aren't where they should be.
;;  Go through all the links, checking the actual length against the distance
;;  between the attach-points.  When there's a discrepancy, collect the link
;;  and the desired length for later use.
(defun find-disturbed-links (form)
   (nconc (find-horizontal-disturbed-links form)
	  (find-vertical-disturbed-links form)))

(defun find-horizontal-disturbed-links (form)
   (let ((changes nil))
     (dolist (link (form-horizontal-links form))
       (when (eq form (link-from link))
	 (let ((desired-length (- (contact-tentative-x (link-to link))
				  (link-horizontal-attach-to-correction link)
				  (ecase (link-attach-from link)
				    (:left 0)
				    (:center (round (form-projected-width form) 2))
				    (:right (form-projected-width form))))))
	   (unless (= (link-tentative-length link) desired-length)
	     (push (cons link desired-length) changes)))))
     (dolist (child (composite-children form))
       (dolist (link (contact-constraint child :horizontal-links))
	 (when (eq child (link-from link))
	   (let ((desired-length (if (eq (link-to link) form)
				     (abs (+ (contact-tentative-x (link-from link))
					     (- (ecase (link-attach-to link)
						  (:left 0)
						  (:center (round (form-projected-width form) 2))
						  (:right (form-projected-width form))))
					     (ecase (link-attach-from link)
					       (:left 0)
					       (:center (+ (round (contact-tentative-width child) 2)
							   (contact-border-width child)))
					       (:right (+ (contact-tentative-width child)
							  (contact-border-width child)
							  (contact-border-width child))))))
				     (- (contact-tentative-x (link-to link))
					(contact-tentative-x (link-from link))
					(link-horizontal-attach-to-correction link)
					(ecase (link-attach-from link)
					  (:left 0)
					  (:center (+ (round (contact-tentative-width child) 2)
						      (contact-border-width child)))
					  (:right (+ (contact-tentative-width child)
						     (contact-border-width child)
						     (contact-border-width child))))))))
	     (unless (= (link-tentative-length link) desired-length)
	       (push (cons link desired-length) changes))))))
     changes))

(defun find-vertical-disturbed-links (form)
   (let ((changes nil))
     (dolist (link (form-vertical-links form))
       (when (eq form (link-from link))
	 (let ((desired-length (- (contact-tentative-y (link-to link))
				  (link-vertical-attach-to-correction link)
				  (ecase (link-attach-from link)
				    (:top 0)
				    (:center (round (form-projected-height form) 2))
				    (:bottom (form-projected-height form))))))
	   (unless (= (link-tentative-length link) desired-length)
	     (push (cons link desired-length) changes)))))
     (dolist (child (composite-children form))
       (dolist (link (contact-constraint child :vertical-links))
	 (when (eq child (link-from link))
	   (let ((desired-length (if (eq (link-to link) form)
				     (abs (+ (contact-tentative-y (link-from link))
					     (- (ecase (link-attach-to link)
						  (:top 0)
						  (:center (round (form-projected-height form) 2))
						  (:bottom (form-projected-height form))))
					     (ecase (link-attach-from link)
					       (:top 0)
					       (:center (+ (round (contact-tentative-height child) 2)
							   (contact-border-width child)))
					       (:bottom (+ (contact-tentative-height child)
							   (contact-border-width child)
							   (contact-border-width child))))))
				     (- (contact-tentative-y (link-to link))
					(contact-tentative-y (link-from link))
					(link-vertical-attach-to-correction link)
					(ecase (link-attach-from link)
					  (:top 0)
					  (:center (+ (round (contact-tentative-height child) 2)
						      (contact-border-width child)))
					  (:bottom (+ (contact-tentative-height child)
						      (contact-border-width child)
						      (contact-border-width child))))))))
	     (unless (= (link-tentative-length link) desired-length)
	       (push (cons link desired-length) changes))))))
     changes))


;;;
;;;  Resize-algorithm core functions.  The basics of the resize algorithm
;;;  are contained in the functions below.  The algorithm is:  When the width
;;;  of the Form changes, calculate the maximum stretch or shrink across the
;;;  link graph and divide the width difference accordingly among the children.
;;;  Do the same with the height.  Then, go through the horizontal link graph
;;;  and move the children to account for changes in the sizes of contacts and
;;;  links.  Do the same with the vertical link graph.  That's all.  (When a
;;;  child resizes itself, manage-geometry handles it.)

;;  Function called from change-layout.  Follows the algorithm described above, with
;;  the efficiency hack described with the "tentative" abstraction macros.  It's
;;  broken into "clear," "internal," "adjust," and "really" so manage-geometry can
;;  use them separately.
(defun place-and-size-children (form &optional width-difference height-difference adjust-p)
   (clear-tentative-values form)
   (place-and-size-children-internal form width-difference height-difference (not adjust-p))
   (when adjust-p
     (let ((adjustments-needed (adjust-sizes-to-fit form)))
       (when adjustments-needed
	 (error "Inconsistent or incomplete layout constraints:  ~{~&   ~A~}"
		(let ((l nil))
		  (dolist (adj adjustments-needed l)
		    (let ((link (car adj))
			  (length (cdr adj)))
		      (push (format nil "~A link from ~A to ~A wants to be ~D long, ~
					      but its limits are ~D and ~D."
				    (link-orientation link)
				    (contact-name (link-from link))
				    (contact-name (link-to link))
				    length
				    (link-minimum link)
				    (link-maximum link))
			    l))))))))
   (really-change-the-children form))

(defun clear-tentative-values (form)
   ;;  Flush the old cached values from the last place-and-resize.
   ;;  +++ Would it be appropriate to remf them at the end instead?  This
   ;;      way at least the consing is limited, but it never goes away.
   (with-slots (children) form
     (dolist (contact children)
       (setf (contact-tentative-width  contact) nil)
       (setf (contact-tentative-height contact) nil)
       (setf (contact-tentative-x      contact) nil)
       (setf (contact-tentative-y      contact) nil)
       (dolist (link (contact-constraint contact :horizontal-links))
	 (setf (link-tentative-length link) nil))
       (dolist (link (contact-constraint contact :vertical-links))
	 (setf (link-tentative-length link) nil))))
   (setf (form-projected-width form) nil)
   (setf (form-projected-height form) nil))

(defun place-and-size-children-internal (form width-difference height-difference &optional (error-p t))
   ;;  Figure out the new sizes and placements, given the changes in Form size.
   (when (and width-difference
	      (not (zerop width-difference)))
     (resize-children-horizontal form width-difference))
   (when (and height-difference
	      (not (zerop height-difference)))
     (resize-children-vertical form height-difference))

   (place-children-from-form-horizontal form error-p)
   (place-children-from-form-vertical   form error-p))

(defun really-change-the-children (form)
   ;;  The above just set up cached values for x, y, width, and height.
   ;;  Now go through the children and adjust where appropriate.
   (with-slots (children) form
     (dolist (contact children)
       (with-state (contact)
	 (when (or (/= (contact-tentative-x contact) (contact-x contact))
		   (/= (contact-tentative-y contact) (contact-y contact)))
	   (move contact
		 (contact-tentative-x contact)
		 (contact-tentative-y contact)))
	 (when (or (/= (contact-tentative-width contact)  (contact-width contact))
		   (/= (contact-tentative-height contact) (contact-height contact)))
	   (resize contact
		   (contact-tentative-width contact)
		   (contact-tentative-height contact)
		   (contact-border-width contact))))
       (dolist (link (contact-constraint contact :horizontal-links))
	 (when (/= (link-tentative-length link) (link-length link))
	   (setf (slot-value link 'length) (link-tentative-length link))))
       (dolist (link (contact-constraint contact :vertical-links))
	 (when (/= (link-tentative-length link) (link-length link))
	   (setf (slot-value link 'length) (link-tentative-length link)))))))

;;  Algorithm:  Find all the links whose endpoints don't match reality.
;;  If the desired length is within their constraints, set their length to the
;;  the desired length.  If not, use the traversal functions to find the available
;;  stretch or shrink along the paths from both linked contacts;  if the stretch
;;  or shrink is enough to accomodate the desired length, fake the partial resize
;;  to adjust the affected children and links and set the link length to what's
;;  left.  If that still doesn't do it, return the list of unfixed links and
;;  lengths, else return NIL.
(defun adjust-sizes-to-fit (form)
   (let ((misfits (find-disturbed-links form))
	 (unfixables nil))
     (when misfits
       (dolist (misfit misfits)
	 (let ((link (car misfit))
	       (desired-length (cdr misfit)))
	   (if (length<= (link-minimum link)
			 desired-length
			 (link-maximum link))
	       (setf (link-tentative-length link) desired-length)
	       (if (< (link-length link) desired-length)
		   (ecase (link-orientation link)
		     (:horizontal
		      (multiple-value-bind (from-stretch from-stretch-inf)
			  (find-path-horizontal-stretch (link-from link) form)
			(multiple-value-bind (to-stretch to-stretch-inf)
			    (find-path-horizontal-stretch (link-to link) form)
			  ;;  If there's enough stretch to do it, stretch them, else
			  ;;  stick the "misfit" entry on unfixables.
			  (let* ((total-stretch (+ from-stretch to-stretch))
				 (total-stretch-inf (+ from-stretch-inf to-stretch-inf))
				 (total-diff (- desired-length (link-maximum link))))
			    (if (and (zerop total-stretch-inf)
				     (> total-diff total-stretch))
				(push misfit unfixables)
				(let ((from-diff (if (> total-stretch-inf 0)
						     (if (zerop from-stretch-inf)
							 0
							 (round (* total-diff
								   (/ from-stretch-inf total-stretch-inf))))
						     (round (* total-diff
							       (/ from-stretch total-stretch)))))
				      (to-diff   (if (> total-stretch-inf 0)
						     (if (zerop to-stretch-inf)
							 0
							 (round (* total-diff
								   (/ to-stretch-inf total-stretch-inf))))
						     (round (* total-diff
							       (/ to-stretch total-stretch))))))
				  (resize-by-path-horizontal
				    (link-from link) from-diff from-stretch from-stretch-inf form)
				  (resize-by-path-horizontal
				    (link-to link) to-diff to-stretch to-stretch-inf form t)
				  (setf (link-tentative-length link) (link-maximum link)))))
			  )))
		     (:vertical
		      (multiple-value-bind (from-stretch from-stretch-inf)
			  (find-path-vertical-stretch (link-from link) form)
			(multiple-value-bind (to-stretch to-stretch-inf)
			    (find-path-vertical-stretch (link-to link) form)
			  ;;  If there's enough stretch to do it, stretch them, else
			  ;;  stick the "misfit" entry on unfixables.
			  (let* ((total-stretch (+ from-stretch to-stretch))
				 (total-stretch-inf (+ from-stretch-inf to-stretch-inf))
				 (total-diff (- desired-length (link-maximum link))))
			    (if (and (zerop total-stretch-inf)
				     (> total-diff total-stretch))
				(push misfit unfixables)
				(let ((from-diff (if (> total-stretch-inf 0)
						     (if (zerop from-stretch-inf)
							 0
							 (round (* total-diff
								   (/ from-stretch-inf total-stretch-inf))))
						     (round (* total-diff
							       (/ from-stretch total-stretch)))))
				      (to-diff   (if (> total-stretch-inf 0)
						     (if (zerop to-stretch-inf)
							 0
							 (round (* total-diff
								   (/ to-stretch-inf total-stretch-inf))))
						     (round (* total-diff
							       (/ to-stretch total-stretch))))))
				  (resize-by-path-vertical
				    (link-from link) from-diff from-stretch from-stretch-inf form)
				  (resize-by-path-vertical
				    (link-to link) to-diff to-stretch to-stretch-inf form t)
				  (setf (link-tentative-length link) (link-maximum link)))))
			  ))))
		   (ecase (link-orientation link)
		     (:horizontal
		      (let* ((from-shrink (find-path-horizontal-shrink (link-from link) form))
			     (to-shrink   (find-path-horizontal-shrink (link-to link) form))
			     (total-shrink (+ from-shrink to-shrink))
			     (total-diff (- desired-length (link-minimum link))))
			;;  If there's enough shrink to do it, shrink them, else
			;;  stick the "misfit" entry on unfixables.
			(if (> (- total-diff) total-shrink)
			    (push misfit unfixables)
			    (let ((from-diff  (round (* total-diff
							(/ from-shrink total-shrink))))
				  (to-diff 	(round (* total-diff
							  (/ to-shrink total-shrink)))))
			      (resize-by-path-horizontal (link-from link) from-diff from-shrink 0 form)
			      (resize-by-path-horizontal (link-to link) to-diff to-shrink 0 form t)
			      (setf (link-tentative-length link) (link-minimum link)))))
		      )
		     (:vertical
		      (let* ((from-shrink (find-path-vertical-shrink (link-from link) form))
			     (to-shrink   (find-path-vertical-shrink (link-to link) form))
			     (total-shrink (+ from-shrink to-shrink))
			     (total-diff (- desired-length (link-minimum link))))
			;;  If there's enough shrink to do it, shrink them, else
			;;  stick the "misfit" entry on unfixables.
			(if (> (- total-diff) total-shrink)
			    (push misfit unfixables)
			    (let ((from-diff (round (* total-diff
						       (/ from-shrink total-shrink))))
				  (to-diff   (round (* total-diff
						       (/ to-shrink total-shrink)))))
			      (resize-by-path-vertical (link-from link) from-diff from-shrink 0 form)
			      (resize-by-path-vertical (link-to link) to-diff to-shrink 0 form t)
			      (setf (link-tentative-length link) (link-minimum link)))))
			)))))))
     unfixables))

;;  The width part of the resize algorithm.  Given the width difference, figure
;;  how much to scale (using the traversal functions from above), then adjust
;;  the children and the links in proportion to their maximum or minimum sizes.
;;  If stretching, and there are :infinites in the maximum stretch, only contacts
;;  and links with :infinite maximum sizes will be affected.
(defun resize-children-horizontal (form width-difference)
   (with-slots (children) form
     (let* ((h-shrink-p (< width-difference 0))
	    (h-scale nil)
	    (h-scale-inf 0))
       (labels ((compute-delta (length max-length min-length)
		  (cond ((and (zerop h-scale-inf)	; No change allowed.
			      (zerop h-scale))
			 0)
			(h-shrink-p		; A shrink.
			 (round (* width-difference
				   (/ (- length min-length)
				      h-scale))))
			((zerop h-scale-inf)	; A stretch without :infinites.
			 (round (* width-difference
				   (/ (- max-length length)
				      h-scale))))
			(:else			; A stretch with :infinites.
			 (if (eq max-length :infinite)
			     (round (/ width-difference
				       h-scale-inf))
			     0))))
		(scale-horizontal-link (link)
	          (let ((offset-delta (compute-delta (link-length link)
						     (link-maximum link)
						     (link-minimum link))))
		    (unless (zerop offset-delta)
		      (setf (link-tentative-length link)	; Constrain the length between its min and max.
			    (max (length-min (+ (link-length link) offset-delta)
					     (link-maximum link))
				 (link-minimum link)))))))

	 (if h-shrink-p
	     (setq h-scale (find-form-horizontal-shrink form))
	     (multiple-value-setq (h-scale h-scale-inf)
	       (find-form-horizontal-stretch form)))
	 (dolist (contact children)
	   (let ((delta-w (compute-delta (contact-width contact)
					 (form-max-width contact)
					 (form-min-width contact))))
	     (unless (zerop delta-w)
	       (setf (contact-tentative-width contact)	; Constrain the width between min and max.
		     (max (length-min (+ (contact-width contact) delta-w)
				      (form-max-width contact))
			  (form-min-width contact)))))
	   (dolist (link (contact-constraint contact :horizontal-links))
	     (when (eq contact (link-from link))
	       (scale-horizontal-link link))))
	 (dolist (link (form-horizontal-links form))
	   (when (eq form (link-from link))
	     (scale-horizontal-link link)))))))

;;  The height part of the resize algorithm.  Given the height difference, figure
;;  how much to scale (using the traversal functions from above), then adjust
;;  the children and the links in proportion to their maximum or minimum sizes.
;;  If stretching, and there are :infinites in the maximum stretch, only contacts
;;  and links with :infinite maximum sizes will be affected.
(defun resize-children-vertical (form height-difference)
   (with-slots (children) form
     (let* ((v-shrink-p (< height-difference 0))
	    (v-scale nil)
	    (v-scale-inf 0))
       (labels ((compute-delta (length max-length min-length)
		  (cond ((and (zerop v-scale-inf)	; No change allowed.
			      (zerop v-scale))
			 0)
			(v-shrink-p		; A shrink.
			 (round (* height-difference
				   (/ (- length min-length)
				      v-scale))))
			((zerop v-scale-inf)	; A stretch without :infinites.
			 (round (* height-difference
				   (/ (- max-length length)
				      v-scale))))
			(:else			; A stretch with :infinites.
			 (if (eq max-length :infinite)
			     (round (/ height-difference
				       v-scale-inf))
			     0))))
		(scale-vertical-link (link)
		  (let ((offset-delta (compute-delta (link-length link)
						     (link-maximum link)
						     (link-minimum link))))
		    (unless (zerop offset-delta)
		      (setf (link-tentative-length link)	; Keep length between link min and max.
			    (max (length-min (+ (link-length link) offset-delta)
					     (link-maximum link))
				 (link-minimum link)))))))

	 (if v-shrink-p
	     (setq v-scale (find-form-vertical-shrink form))
	     (multiple-value-setq (v-scale v-scale-inf)
	       (find-form-vertical-stretch form)))
	 (dolist (contact children)
	   (let ((delta-h (compute-delta (contact-height contact)
					 (form-max-height contact)
					 (form-min-height contact))))
	     (unless (zerop delta-h)
	       (setf (contact-tentative-height contact)	; Keep height between min and max.
		     (max (length-min (+ (contact-height contact) delta-h)
				      (form-max-height contact))
			  (form-min-height contact)))))
	   (dolist (link (contact-constraint contact :vertical-links))
	     (when (eq contact (link-from link))
	       (scale-vertical-link link))))
	 (dolist (link (form-vertical-links form))
	   (when (eq form (link-from link))
	     (scale-vertical-link link)))))))


;;  Move children around, following the link graph, based on the current
;;  (tentative) sizes and positions of contacts and links earlier in the
;;  graph.  This function and the next are a pair much like the traversal
;;  functions:  the first one operates on links attached to the Form, the
;;  second on the paths from children contacts recursively through the link
;;  graph to the Form again.
(defun place-children-from-form-horizontal (form error-p)
   ;;  Clear the ticks from last time.
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))

   ;;  For each link, the position of the contact on the other end is a
   ;;  function of the current contact's position and size and the attach
   ;;  points and length of the link.
   (dolist (link (form-horizontal-links form))
     (when (eq form (link-from link))
       (let* ((r-contact (link-to link))
	      (new-x (+ (ecase (link-attach-from link)
			  (:left 0)
			  (:right (+ (form-projected-width form)
				     (contact-border-width form)
				     (contact-border-width form)))
			  (:center (+ (round (form-projected-width form) 2)
				      (contact-border-width form))))
			(- (ecase (link-attach-to link)
			     (:left 0)
			     (:right (+ (contact-tentative-width r-contact)
					(contact-border-width r-contact)
					(contact-border-width r-contact)))
			     (:center (+ (round (contact-tentative-width r-contact) 2)
					 (contact-border-width r-contact)))))
			(link-tentative-length link))))
	 (when (/= new-x (contact-tentative-x r-contact))
	   (cond ((and (form-tick r-contact) error-p)
		  ;;  Already moved once, with a different X.
		  (error "Inconsistent horizontal links on contact ~S" r-contact))
		 (:else
		  (setf (contact-tentative-x r-contact) new-x))))
	 (setf (form-tick r-contact) t)
	 (place-children-from-links-horizontal r-contact form error-p))))

   ;;  Now do the to-link graph.
   (dolist (link (form-horizontal-links form))
     (when (and (eq form (link-to link))
		(null (form-tick (link-from link))))
       (let* ((l-contact (link-from link))
	      (new-x (+ (ecase (link-attach-to link)
			  (:left 0)
			  (:right (+ (form-projected-width form)
				     (contact-border-width form)
				     (contact-border-width form)))
			  (:center (+ (round (form-projected-width form) 2)
				      (contact-border-width form))))
			(- (ecase (link-attach-from link)
			     (:left 0)
			     (:right (+ (contact-tentative-width l-contact)
					(contact-border-width l-contact)
					(contact-border-width l-contact)))
			     (:center (+ (round (contact-tentative-width l-contact) 2)
					 (contact-border-width l-contact)))))
			(- (link-tentative-length link)))))
	 (when (/= new-x (contact-tentative-x l-contact))
	   (cond ((and (form-tick l-contact) error-p)
		  ;;  Already moved once, with a different X.
		  (error "Inconsistent horizontal links on contact ~S" l-contact))
		 (:else
		  (setf (contact-tentative-x l-contact) new-x))))
	 (setf (form-tick l-contact) t)
	 (place-children-from-links-horizontal l-contact form error-p t)))))

(defun place-children-from-links-horizontal (contact top-level-form error-p &optional to-links-p)
   (unless (eq contact top-level-form)		; Stop when hit the Form again.
     ;;  For each link, the position of the contact on the other end is a
     ;;  function of the current contact's position and size and the attach
     ;;  points and length of the link.
     (dolist (link (contact-constraint contact :horizontal-links))
       (when (and (eq contact (if to-links-p
				  (link-to link)
				  (link-from link)))
		  (not (eq (if to-links-p
			       (link-from link)
			       (link-to link))
			   top-level-form)))
	 (let* ((r-contact (if to-links-p (link-from link) (link-to link)))
		(new-x (+ (contact-tentative-x contact)
			  (ecase (if to-links-p (link-attach-to link) (link-attach-from link))
			    (:left 0)
			    (:right (+ (contact-tentative-width contact)
				       (contact-border-width contact)
				       (contact-border-width contact)))
			    (:center (+ (round (contact-tentative-width contact) 2)
					(contact-border-width contact))))
			  (- (ecase (if to-links-p (link-attach-from link) (link-attach-to link))
			       (:left 0)
			       (:right (+ (contact-tentative-width r-contact)
					  (contact-border-width r-contact)
					  (contact-border-width r-contact)))
			       (:center (+ (round (contact-tentative-width r-contact) 2)
					   (contact-border-width r-contact)))))
			  (if to-links-p
			      (- (link-tentative-length link))
			      (link-tentative-length link)))))
	   (when (/= new-x (contact-tentative-x r-contact))
	     (cond ((and (form-tick r-contact) error-p)
		    ;;  Already moved once, with a different X.
		    (error "Inconsistent horizontal links on contact ~S" r-contact))
		   (:else
		    (setf (contact-tentative-x r-contact) new-x))))
	   (setf (form-tick r-contact) t)
	   (place-children-from-links-horizontal r-contact top-level-form error-p to-links-p))))))


;;  Move the children vertically.  This function and the next are also a pair like
;;  the traversal functions (see comments at place-children-from-form-horizontal).
(defun place-children-from-form-vertical (form error-p)
   ;;  Clear the ticks from last time.
   (dolist (contact (composite-children form))
     (setf (form-tick contact) nil))

   ;;  For each link, the position of the contact on the other end is a
   ;;  function of the current contact's position and size and the attach
   ;;  points and length of the link.
   (dolist (link (form-vertical-links form))
     (when (eq form (link-from link))
       (let* ((b-contact (link-to link))
	      (new-y (+ (ecase (link-attach-from link)
			  (:top 0)
			  (:bottom (+ (form-projected-height form)
				      (contact-border-width form)
				      (contact-border-width form)))
			  (:center (+ (round (form-projected-height form) 2)
				      (contact-border-width form))))
			(- (ecase (link-attach-to link)
			     (:top 0)
			     (:bottom (+ (contact-tentative-height b-contact)
					 (contact-border-width b-contact)
					 (contact-border-width b-contact)))
			     (:center (+ (round (contact-tentative-height b-contact) 2)
					 (contact-border-width b-contact)))))
			(link-tentative-length link))))
	 (when (/= new-y (contact-tentative-y b-contact))
	   (cond ((and (form-tick b-contact) error-p)
		  ;;  Already moved once, with a different Y.
		  (error "Inconsistent vertical links on contact ~S" b-contact))
		 (:else
		  (setf (contact-tentative-y b-contact) new-y))))
	 (setf (form-tick b-contact) t)
	 (place-children-from-links-vertical b-contact form error-p))))

   ;;  Now do the to-link graph.
   (dolist (link (form-vertical-links form))
     (when (and (eq form (link-to link))
		(null (form-tick (link-from link))))
       (let* ((t-contact (link-from link))
	      (new-y (+ (ecase (link-attach-to link)
			  (:top 0)
			  (:bottom (+ (form-projected-height form)
				      (contact-border-width form)
				      (contact-border-width form)))
			  (:center (+ (round (form-projected-height form) 2)
				      (contact-border-width form))))
			(- (ecase (link-attach-from link)
			     (:top 0)
			     (:bottom (+ (contact-tentative-height t-contact)
					 (contact-border-width t-contact)
					 (contact-border-width t-contact)))
			     (:center (+ (round (contact-tentative-height t-contact) 2)
					 (contact-border-width t-contact)))))
			(- (link-tentative-length link)))))
	 (when (/= new-y (contact-tentative-y t-contact))
	   (cond ((and (form-tick t-contact) error-p)
		  ;;  Already moved once, with a different Y.
		  (error "Inconsistent vertical links on contact ~S" t-contact))
		 (:else
		  (setf (contact-tentative-y t-contact) new-y))))
	 (setf (form-tick t-contact) t)
	 (place-children-from-links-vertical t-contact form error-p t)))))

(defun place-children-from-links-vertical (contact top-level-form error-p &optional to-links-p)
   (unless (eq contact top-level-form)		; Stop when hit the Form again.
     ;;  For each link, the position of the contact on the other end is a
     ;;  function of the current contact's position and size and the attach
     ;;  points and length of the link.
     (dolist (link (contact-constraint contact :vertical-links))
       (when (and (eq contact (if to-links-p (link-to link) (link-from link)))
		  (not (eq (if to-links-p
			       (link-from link)
			       (link-to link))
			   top-level-form)))
	 (let* ((b-contact (if to-links-p (link-from link) (link-to link)))
		(new-y (+ (contact-tentative-y contact)
			  (ecase (if to-links-p (link-attach-to link) (link-attach-from link))
			    (:top 0)
			    (:bottom (+ (contact-tentative-height contact)
					(contact-border-width contact)
					(contact-border-width contact)))
			    (:center (+ (round (contact-tentative-height contact) 2)
					(contact-border-width contact))))
			  (- (ecase (if to-links-p (link-attach-from link) (link-attach-to link))
			       (:top 0)
			       (:bottom (+ (contact-tentative-height b-contact)
					   (contact-border-width b-contact)
					   (contact-border-width b-contact)))
			       (:center (+ (round (contact-tentative-height b-contact) 2)
					   (contact-border-width b-contact)))))
			  (if to-links-p
			      (- (link-tentative-length link))
			      (link-tentative-length link)))))
	   (when (/= new-y (contact-tentative-y b-contact))
	     (cond ((and (form-tick b-contact) error-p)
		    ;;  Already moved once, with a different Y.
		    (error "Inconsistent vertical links on contact ~S" b-contact))
		   (:else
		    (setf (contact-tentative-y b-contact) new-y))))
	   (setf (form-tick b-contact) t)
	   (place-children-from-links-vertical b-contact top-level-form error-p to-links-p))))))

;;;
;;;  Two specialised traversal-based resize functions for adjust-sizes-to-fit.

(defun resize-by-path-horizontal (contact width-difference h-scale h-scale-inf top-level-form &optional to-p)
   (unless (eq contact top-level-form)
     (let ((h-shrink-p (< width-difference 0)))
       (labels ((compute-delta (length max-length min-length)
		  (cond ((and (zerop h-scale-inf)	; No change allowed.
			      (zerop h-scale))
			 0)
			(h-shrink-p		; A shrink.
			 (round (* width-difference
				   (/ (- length min-length)
				      h-scale))))
			((zerop h-scale-inf)	; A stretch without :infinites.
			 (round (* width-difference
				   (/ (- max-length length)
				      h-scale))))
			(:else			; A stretch with :infinites.
			 (if (eq max-length :infinite)
			     (round (/ width-difference
				       h-scale-inf))
			     0))))
		(scale-horizontal-link (link)
		  (let ((offset-delta (compute-delta (link-length link)
						     (link-maximum link)
						     (link-minimum link))))
		    (unless (zerop offset-delta)
		      (setf (link-tentative-length link)	; Constrain the length between its min and max.
			    (max (length-min (+ (link-length link) offset-delta)
					     (link-maximum link))
				 (link-minimum link)))))))
	 (let ((delta-w (compute-delta (contact-width contact)
				       (form-max-width contact)
				       (form-min-width contact))))
	   (unless (zerop delta-w)
	     (setf (contact-tentative-width contact)	; Constrain the width between min and max.
		   (max (length-min (+ (contact-tentative-width contact) delta-w)
				    (form-max-width contact))
			(form-min-width contact)))))
	 (dolist (link (contact-constraint contact :horizontal-links))
	   (let ((next-contact (if to-p (link-from link) (link-to link))))
	     (unless (eq contact next-contact)
	       (scale-horizontal-link link)
	       (resize-by-path-horizontal next-contact width-difference h-scale h-scale-inf top-level-form to-p))))))))

(defun resize-by-path-vertical (contact height-difference v-scale v-scale-inf top-level-form &optional to-p)
   (unless (eq contact top-level-form)
     (let ((v-shrink-p (< height-difference 0)))
       (labels ((compute-delta (length max-length min-length)
		  (cond ((and (zerop v-scale-inf)	; No change allowed.
			      (zerop v-scale))
			 0)
			(v-shrink-p		; A shrink.
			 (round (* height-difference
				   (/ (- length min-length)
				      v-scale))))
			((zerop v-scale-inf)	; A stretch without :infinites.
			 (round (* height-difference
				   (/ (- max-length length)
				      v-scale))))
			(:else			; A stretch with :infinites.
			 (if (eq max-length :infinite)
			     (round (/ height-difference
				       v-scale-inf))
			     0))))
		(scale-vertical-link (link)
		  (let ((offset-delta (compute-delta (link-length link)
						     (link-maximum link)
						     (link-minimum link))))
		    (unless (zerop offset-delta)
		      (setf (link-tentative-length link)	; Constrain the length between its min and max.
			    (max (length-min (+ (link-length link) offset-delta)
					     (link-maximum link))
				 (link-minimum link)))))))
	 (let ((delta-h (compute-delta (contact-height contact)
				       (form-max-height contact)
				       (form-min-height contact))))
	   (unless (zerop delta-h)
	     (setf (contact-tentative-height contact)	; Constrain the height between min and max.
		   (max (length-min (+ (contact-tentative-height contact) delta-h)
				    (form-max-height contact))
			(form-min-height contact)))))
	 (dolist (link (contact-constraint contact :vertical-links))
	   (let ((next-contact (if to-p (link-from link) (link-to link))))
	     (unless (eq contact next-contact)
	       (scale-vertical-link link)
	       (resize-by-path-vertical next-contact height-difference v-scale v-scale-inf top-level-form to-p))))))))
