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
	  add-event delete-event 
	  )
	'pictures)
#-clx-mit-r4
(defmethod add-event ((graphic graphic) event-spec &rest actions)
  "Add EVENT-SPEC and ACTIONS to the plist for GRAPHIC."
  (let ((event-binding  (graphic-parse-event-spec
			 (list* event-spec (copy-list actions))))
	)
    (with-slots (plist) graphic
      (LET* ((event-translations (GETF plist :event-translations))
	    (previous (assoc (car event-binding) event-translations
			     :test #'equal))
	    )
	(if previous
	    ;; Yes, modify it with the new actions.
	    (setf (rest previous) (rest event-translations))
	    
	    ;; No, add new translation.
	    (progn
	      (push event-binding event-translations)
	      (SETF (GETF plist :event-translations) event-translations))
	    )
	)
      (DOLIST (view (graphic-views graphic) )
	
	(when (realized-p view)
	  (LET* ((view-event-mask (slot-value view 'event-mask)) ;update the view mask
		 (view-new-mask (cluei::update-event-mask event-binding view-event-mask)))
	    (unless (= view-new-mask view-event-mask) ;; When modified
	      (setf (window-event-mask view) view-new-mask)
	      ))))))
  (values))

#+clx-mit-r4
(defmethod add-event ((graphic graphic) event-spec &rest actions)
  "Add EVENT-SPEC and ACTIONS to the plist for GRAPHIC."
  (let ((event-binding  (graphic-parse-event-spec
			 (list* event-spec (copy-list actions)))))
    (with-slots ( plist) graphic
      (LET* ((event-translations (GETF plist :event-translations))
	     (previous (assoc (car event-binding) event-translations
			      :test #'equal)))
	(if previous
	    ;; Yes, modify it with the new actions.
	    (setf (rest previous) (rest event-translations))
	    
	    ;; No, add new translation.
	    (progn
	      (push event-binding event-translations)
	      (SETF (GETF plist :event-translations) event-translations))
	    )
	
      (DOLIST (view (graphic-views graphic) )
	
	(when (realized-p view)
	  (when (realized-p view)
	    (LET* ((view-event-mask (slot-value view 'event-mask))
		  (view-new-mask (cluei::event-translation-mask
				  view-event-mask event-translations) ))
	      (unless (= view-new-mask view-event-mask) 
		(setf (window-event-mask view) view-new-mask))))
	  
	  )))))
  (values))

(defmethod delete-event ((graphic graphic) event-spec)
  "Remove EVENT-SPEC from the event-translations for GRAPHIC."
  (let ((event-binding (graphic-parse-event-spec (list event-spec))))
    (with-slots (plist) graphic 
      ;; Compute event mask without current event translations	
      (LET* ((event-translations (GETF plist :event-translations)))	
	(SETF (GETF plist :event-translations)
	      (delete (car event-binding) event-translations 
		      :key #'car :count 1
		      :test #'equal)))
	;; Update the graphic event mask
	))
  (values))


(defun class-name-of (instance)
  (class-name (class-of instance)))

(defun translate-graphic-event (graphic event)
  "Returns the actions for the first event-translation matching EVENT"
  (declare (type graphic graphic)
	   (type event         event))
  (labels ((find-translation (event event-key translations)
	     (dolist (event-binding translations)
	       (let ((event-spec (car event-binding)))
		 (when (if (atom event-spec)
			   ;; Simple EQ test when event spec is an atom
			   (eq event-key event-spec)
			   ;; When event spec is a list, and
			   ;; the car of the list is EQ to the event, and
			   ;; the matcher function returns T
			   (and (eq event-key (car event-spec))
				(apply (cadr event-spec)
				       event (cddr event-spec))))
		   (return event-binding))))))

    (let ((key (slot-value (the event event) 'key)))
      (cdr
	(or
	  ;; Instance translations
	  (find-translation event key
			    (GETF (slot-value (the graphic graphic) 'plist)
				  :event-translations))

	  ;; Class translations 
	    (find-translation event key
			      (GET (class-name-of graphic) 'event-translations))))
      )))


(defun graphic-parse-event-spec (event-translation)
  "Return a canonical form of the EVENT-TRANSLATION."
  
  (cons
    (let ((event-spec (first event-translation)))
      (typecase event-spec
	(list
	 (let* ((key     (first event-spec))
		(checker (check-function key)))
	   (assert checker nil "No check function defined for ~s." key)
	   (multiple-value-bind (args real-key) (apply checker event-spec)
	     (cons (or real-key key) args))))
	
	(character
	 (cons :key-press (key-check :key-press event-spec)))
	
	(otherwise
	 (unless (assoc event-spec cluei::*event-mask-alist* :test #'eq)
	   (error "~s is not a known event keyword." event-spec))
	 event-spec)))
    
    (cdr event-translation)))


(defun key-check (event-key &optional char state select)
  ;; Returns the canonical form of the key (i.e. one that may speed up
  ;; the matching operation)
  (declare (ignore event-key))
  (unless (typep char '(or card16 character (member :any)))
    (error "~a is not a CHARACTER, CARD16 or :ANY." char))
  
  (let*
    ((modifiers
       (cond ((or (null state) 
		  (eq state :none))   0)
	     ((numberp state)         state)
	     ((eq state :any)         (setf select 0))
	     (t                       (encode-clue-modifier-mask state))))
     (mask 
       (cond ((null select)           (if (characterp char) 0 modifiers))
	     ((numberp select)        select)
	     ((eq select :same)       modifiers)
	     ((eq select :all)        #xffff)	     	     
	     (t                       (encode-clue-modifier-mask select)))))
    
    (list 'key-match char modifiers mask)))



(defun encode-clue-modifier-mask (modifiers)
  ;; Make a state-mask from modifiers
  (declare (type (or mask16 state-mask-key (member :meta :super :hyper) list)
		 modifiers))
  (typecase modifiers
    (fixnum (ldb (byte cluei::meta-shift 0) modifiers))
    (cons (let ((mask 0))
	    (dolist (modifier modifiers)
	      (setf mask (logior mask (encode-clue-modifier-mask modifier))))
	    mask))
    (otherwise
     (let ((temp (position modifiers 
			   (the list cluei::*meta-modifier-alist*)
			   :key #'car :test #'eq)))
       (if temp
	   (ash 1 (+ temp cluei::meta-shift))
	 (make-state-mask modifiers))))))

;;;  added for Lispworks and any derivites there of

(DEFMETHOD clue::class-event-translations  ((class t))
  (get (class-name  class) 'event-translations))

(DEFMETHOD (SETF clue::class-event-translations) (translations (class t))
  (setf (GET (class-name class)  'event-translations) translations))
