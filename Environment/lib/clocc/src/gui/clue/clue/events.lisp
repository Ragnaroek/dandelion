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

#| To Do:

1. Hooks are in place to handle button mult-click and hold using 3 extra modifier state bits
   (:hold :click :double), but the interface isn't there yet.

2. Add translation from symbolic mouse button names to actual mouse button/modifier specs.
   Use the keysym translation facilities (define 5 keysyms for each of the mouse keys,
   and use define-keysym)

|#

(in-package "CLUEI")

(export '( event
	  ;; Event slots
	  key display contact character keysym above-sibling
	  atom border-width child code colormap configure-p count data
	  drawable event-window focus-p format height hint-p installed-p
	  keymap kind major minor mode name new-p override-redirect-p
	  parent place property requestor root root-x root-y same-screen-p
	  selection send-event-p state target time type width window x y

	  mode-type
	  *remap-events*
	  *restrict-events*
	  add-mode
	  delete-mode
	  with-mode
	  with-event-mode
	  contact-mode
	  contact-super-mode

	  *contact*
	  apply-action
	  call-action
	  defaction
	  describe-action
	  eval-action
	  ignore-action
	  perform-callback
	  throw-action
	  trace-action
	  with-event
	  processing-event-p

	  add-before-action
	  delete-before-action
	  process-all-events
	  process-next-event
	  handle-event
	  translate-event
	  defevent
	  undefevent
	  event-actions
	  add-event
	  delete-event
	  describe-event-translations
	  add-timer
	  delete-timer
	  ))

;; INPUT PROCESSING

;; Rather than pass around event parameters in long plists,
;; parameters are stuffed into this structure.  For any one event,
;; most slots are undefined, and these are initialized to NIL.
;; Event structures are kept on a resource and re-used.
;; The only reason event is a class and not a structure is because
;; we want to use with-slots.
;;
;; Update: These days with-slots DOES work on structure-class, so
;; lets use defstruct!

(eval-when (compile load)
(defstruct (event (:print-function %print-event)(:copier nil)(:predicate nil))
  "CLUE event structure, one slot for every event value.  No methods."
  (key)					; Event key
  (display)				; Display event was reported to
  (contact)				; Contact the event is directed to
  (character
   nil :type (or null base-char keyword)) ; character from code and state
  (keysym 0 :type keysym)		; keysym from code and state
  (plist nil :type list)		; Place for extension data
  ;; The following are from the CLX event
  (above-sibling)			; Used by :configure-notify
                                        ;         :configure-request
  (atom nil :type (or null keyword))	; Used by :property-notify
  (border-width 0 :type card16)	; Used by :create-notify
                                        ;         :configure-notify
                                        ;         :configure-request
  (child nil :type (or null window))	; Used by :key-press :key-release
                                        ;         :button-press :button-release
                                        ;         :motion-notify :enter-notify
                                        ;         :leave-notify
  (code 0 :type card8)			; Used by :key-press :key-release
                                        ;         :button-press :button-release
  (colormap nil :type (or null colormap)) ; Used by :colormap-notify
  (sequence 0 :type card16)		; Used by all except :keymap-notify
  (configure-p)				; Used by :unmap-notify
  (count 0 :type card16)		; Used by :exposure :graphics-exposure
                                        ;         :mapping-notify
  (data)				; Used by :client-message :timer
  (drawable
   nil :type (or null drawable))	; Used by :graphics-exposure
                                        ;         :no-exposure
  (event-window
   nil :type (or null window))		; Used by :destroy-notify :unmap-notify
                                        ;         :map-notify :reparent-notify
                                        ;         :configure-notify
                                        ;         :gravity-notify
                                        ;         :circulate-notify
  (focus-p)				; Used by :enter-notify :leave-notify
  (format)				; Used by :client-message
  (height 0 :type card16)		; Used by :exposure :graphics-exposure
                                        ;         :create-notify
                                        ;         :configure-notify
                                        ;         :configure-request
                                        ;         :resize-request
  (hint-p nil :type  boolean)		; Used by :motion-notify
  (installed-p)			; Used by :colormap-notify
  (keymap)				; Used by :keymap-notify
  (kind)				; Used by :enter-notify :leave-notify
                                        ;         :focus-in :focus-out
  (major 0 :type card8)			; Used by :graphics-exposure
					;         :no-exposure
  (minor 0 :type card16)		; Used by :graphics-exposure
					;         :no-exposure
  (mode )				; Used by :enter-notify :leave-notify
					;         :focus-in :focus-out
  (name)				; Used by :timer
  (new-p)				; Used by :colormap-notify
  (override-redirect-p)			; Used by :create-notify :map-notify
					;         :reparent-notify
					;         :configure-notify
  (parent)				; Used by :create-notify :map-request
					;         :reparent-notify
                                        ;         :configure-request
					;         :circulate-notify
                                        ;         :circulate-request
  (place)				; Used by :circulate-notify
                                        ;         :circulate-request
  (property nil :type (or null keyword)) ; Used by :selection-request
                                        ;         :selection-notify
  (requestor)				; Used by :selection-request
  (root nil :type (or null window))	; Used by :key-press :key-release
                                        ;         :button-press :button-release
					;         :motion-notify :enter-notify
                                        ;         :leave-notify
  (root-x 0 :type int16)		; Used by :key-press :key-release
                                        ;         :button-press :button-release
					;         :motion-notify :enter-notify
                                        ;         :leave-notify
  (root-y 0 :type card16)		; Used by :key-press :key-release
                                        ;         :button-press :button-release
					;         :motion-notify :enter-notify
                                        ;         :leave-notify
  (same-screen-p nil :type boolean)	; Used by :key-press :key-release
                                        ;         :button-press :button-release
					;         :motion-notify :enter-notify
                                        ;         :leave-notify
  (selection nil :type (or null keyword))	; Used by :selection-clear
                                        ;         :selection-request
                                        ;         :selection-notify
  (send-event-p)			; Used by -all events-
  (state)				; Used by :key-press :key-release
                                        ;         :button-press :button-release
					;         :motion-notify :enter-notify
                                        ;         :leave-notify
					;         :visibility-notify
                                        ;         :property-notify
  (target nil :type (or null keyword))	; Used by :selection-request
                                        ;         :selection-notify
  (time nil :type (or null card32))	; Used by :key-press :key-release
					;         :button-press :button-release
					;         :motion-notify :enter-notify
					;         :leave-notify
                                        ;         :property-notify
					;         :selection-clear
                                        ;         :selection-request
					;         :selection-notify
  (type nil :type (or null keyword))	; Used by :client-message
  (width 0 :type card16)		; Used by :exposure :graphics-exposure
                                        ;         :create-notify :configure-notify
					;         :configure-request
                                        ;         :resize-request
  (window nil :type (or null window))	; Used by all events except
                                        ;         :graphics-exposure :no-exposure
                                        ;         :mapping-notify
  (x 0 :type int16)			; Used by :key-press :key-release
                                        ;         :button-press :button-release
					;         :motion-notify :enter-notify
                                        ;         :leave-notify :exposure
					;         :graphics-exposure
                                        ;         :create-notify :reparent-notify
					;         :configure-notify
                                        ;         :configure-request
                                        ;         :gravity-notify
  (y 0 :type int16)			; Used by :key-press :key-release
                                        ;         :button-press :button-release
					;         :motion-notify :enter-notify
                                        ;         :leave-notify :exposure
					;         :graphics-exposure
                                        ;         :create-notify :reparent-notify
					;         :configure-notify
                                        ;         :configure-request
                                        ;         :gravity-notify
  )
)


(defun %print-event (event stream depth)
  (declare (ignore depth))
  (print-object event stream)
  event)

(defmethod print-object ((instance event) stream)
  #+lispm
  (si:printing-random-object (instance stream)
    (with-slots (key contact) instance
      (format stream "Event ~a for ~a"
	      key (and (typep contact 'contact) (contact-name contact)))))
  #-lispm
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (key contact) instance
      (if key
	  (princ key stream)
	  (princ "unused" stream))
      (when (typep contact 'contact)
	(write-string " for " stream)
	(princ (contact-name contact) stream)))))

;;; PROCESS-NEXT-EVENT copies event data into an event structure.  After the
;;; event is processed, its put back into *event-cache* to be re-used on the
;;; next event.  This is done to reduce consing.  Care is taken to shield the
;;; application progammer from the actual event structure to prevent saving the
;;; event structure in application data structures.  If an application did this,
;;; the event would be destructively modified on subsequent events.
(defvar *event-cache* nil)

(defun allocate-event ()
  (or (pop *event-cache*)
      (make-event)))

;; This is a method so efficient slot access is obtained.
(defmethod deallocate-event ((event event))
  (declare (type event event))
  ;; Return an event to the cache, where it can be re-used. Reset slot
  ;; values to limit garbage lifetime.
  (with-slots (key display contact character keysym plist code state time
	       event-window root drawable window child parent root-x root-y x y
	       width height border-width override-redirect-p same-screen-p
	       configure-p hint-p kind mode keymap focus-p count major minor
	       above-sibling place atom selection requestor target property
	       colormap new-p installed-p format type data name send-event-p
	       ) (the event event)
    (setf key	nil
	  display nil
	  contact nil
	  character nil
	  keysym 0
	  code	0
	  state	nil
	  time	0
	  event-window nil
	  root	nil
	  drawable	nil
	  window	nil
	  child	nil
	  parent	nil
	  root-x	0
	  root-y	0
	  x		0
	  y		0
	  width	0
	  height	0
	  border-width 0
	  override-redirect-p nil
	  same-screen-p nil
	  configure-p nil
	  hint-p	nil
	  kind	nil
	  mode	nil
	  keymap	nil
	  focus-p	nil
	  count	0
	  major	0
	  minor	0
	  above-sibling nil
	  place	nil
	  atom	nil
	  selection	nil
	  requestor	nil
	  target	nil
	  property	nil
	  colormap	nil
	  new-p	nil
	  installed-p nil
	  format	nil
	  type	nil
	  data	nil
	  send-event-p nil
	  name	nil
	  plist	nil
	  ))
  (push event *event-cache*))


;;-----------------------------------------------------------------------------
;; MODES

;;; Applications may find it necessary to establish a special input
;;; "mode" in which the user is temporarily required to direct input
;;; to one or more specific contacts. In such a mode, user input
;;; events directed to other contacts are not handled normally, but
;;; instead are either ignored or acknowledged with some kind of
;;; warning.

(deftype mode-type () '(member :non-exclusive :exclusive :spring-loaded))

(defparameter *remap-events* '(:key-press :key-release :button-press :button-release)
  "These events are sent to the most recent :spring-loaded contact on the mode-stack.")

(defparameter *restrict-events*
	      '(:motion-notify :enter-notify :leave-notify)
  "These 'user' events are sent to the restrict-action of the first
   :exclusive contact on the mode-stack")

(defparameter *sensitive-events*
	      '(:key-press :key-release :button-press :button-release
		:motion-notify :enter-notify :leave-notify
		:focus-in)
  "These 'user' events are ignored by an insensitive contact.")

;; Other events (NOT in *remap-events* or *restrict-events*) are handled normally

;;; When dispatching *restrict-events*, if the mode-stack is non-nil,
;;; the event is restricted as follows.  For each entry of the
;;; mode-stack, if the event is for the contact on the stack, or one of
;;; its descendents, it is dispatched.  When a stack-entry with
;;; :Exclusive or :Spring-Loaded MODE-TYPE is encountered, the search
;;; stops, and the event is sent to the RESTRICT-ACTION action of the
;;; mode contact with ARGS.  If there are no :Exclusive or
;;; :Spring-Loaded contacts on the stack, the event is dispatched
;;; normally.
;;;
;;; When dispatching *remap-events*, if the mode-stack is non-nil, the
;;; event is sent (re-mapped) to the first :spring-loaded contact on the
;;; mode-stack.  If there is no :spring-loaded contact, *remap-events*
;;; are handled like *restrict-events*

(defun add-mode (contact &optional (mode-type :non-exclusive) (action 'restrict) &rest args)
  "Push CONTACT with (mode-type action . args) onto the mode-stack"
  (declare (type contact contact)
	   (type mode-type mode-type)
	   (type symbol action)
	   (type list args))
  (when (and (not (eq mode-type :non-exclusive))
	     (not (sensitive-p contact)))
    (error "ADD-MODE on insensitive contact ~s" contact))
  (push (list* contact mode-type action (copy-list args))
	(display-mode-stack (contact-display contact))))

(defun delete-mode (contact)
  "Pop CONTACT (and everything above CONTACT) off the mode-stack
   Returns T when found and removed, else NIL"
  (declare (type contact contact))
  (let* ((display (contact-display contact))
	 (mode-stack (display-mode-stack display)))
    (when mode-stack
      (do ((stack mode-stack (cdr stack)))
	  ((endp stack)
	   ;; If contact not found, check its children
	   ;; This feature utilized when un-mapping the parent of a modal contact
	   (do ((stack mode-stack (cdr stack))
		(found-p nil)
		(result nil))
	       ((endp stack)
		(when found-p
		  (setf (display-mode-stack display) result)
		  t))
	     (when (ancestor-p contact (caar stack))
	       (setq found-p t
		     result stack))))
	(when (eq contact (caar stack))
	  (setf (display-mode-stack display) (cdr stack))
	  (return t))))))

(defmacro with-mode ((contact &key (mode-type :exclusive)
			      (action 'ignore-action) args)
		     body-form &body cleanup-forms)
  "While executing BODY-FORM, user events will only be delivered to CONTACT
and its children.  Non-user events (e.g.  exposure,property-notify, etc)
will be delivered normally.  User events to other contacts will cause
the ACTION action for CONTACT's class to be invoked with ARGS.  The
primary contact method for the default ACTION, ignore-action, beeps on
*remap-events*, and ignores all others.

WITH-MODE executes BODY-FORM within an Unwind-Protect.  With-Mode
returns the value of its BODY-FORM, and executes CLEANUP-FORMS before
exiting. "
  (let ((local-contact (gensym)))
    `(let ((,local-contact ,contact))
       (unwind-protect
	   (progn
	     (add-mode ,local-contact ,mode-type (function ,action) ,@args)
	     ,body-form)
	 (delete-mode ,local-contact)
	 ,@cleanup-forms))))

(defun contact-mode (contact)
  "If contact is the descendent of a modal contact, return the modal contact, else NIL."
  (let ((modes (display-mode-stack (contact-display contact))))
    (if modes ;; No mode stack means EVERYTHING is in "on the stack"
	(do ((p contact (contact-parent p)))
	    ((null p) nil)
	  (dolist (mode modes)
	    (cond ((eq p (car mode))
		   (return-from contact-mode p))
		  ((eq (cadr mode) :exclusive)
		   (return nil))))))))

(defun contact-super-mode (contact)
  "If contact is the descendent of a modal contact, return the superior modal contact, else NIL."
  (let ((modes (cluei::display-mode-stack (contact-display contact))))
    (if modes ;; No mode stack means EVERYTHING is in "on the stack"
	(do ((p contact (contact-parent p)))
	    ((null p) nil)
	  (do ((mode modes (cdr mode))
	       (supermode nil))
	      ((endp mode))
	    (when (eq p (caar mode))
	      (return-from contact-super-mode supermode))
	    (unless (eq (cadar mode) :non-exclusive)
	      (setq supermode (caar mode))))))))


(defmacro with-event-mode ((contact &rest translations) &body body)
  "The given event TRANSLATIONS are defined for the CONTACT only within
the dynamic extent of the BODY. The TRANSLATIONS are processed before any
other previously-defined instance or class translations for CONTACT."
  (let ((previous-translations (gensym))
	(new-translations (gensym))
	(translation      (gensym))
	(previous         (gensym))
	(slot             (gensym)))

  `(let* ((,new-translations (list ,@translations))
	  (,slot             (slot-value (the contact ,contact) 'event-translations))
	  (,previous-translations
	   ;; Save any actions from previous instance translations for these event specs
	   (let (,previous-translations)
	     (dolist (,translation ,new-translations (nreverse ,previous-translations))
	       (when (assoc (first (parse-event-translation (first ,translation) (rest ,translation)))
			    ,slot
			    :test #'equal)
		 (push ,translation ,previous-translations))))))

     (unwind-protect
	 (progn
	   ;; Add modal translations
	   (dolist (,translation ,new-translations)
	     (apply #'add-event ,contact ,translation))

	   ,@body)

       ;; Delete modal translations and restore any previous ones
       (dolist (,translation ,new-translations)
	 (let ((,previous (pop ,previous-translations)))
	   (if ,previous
	       (apply #'add-event ,contact ,previous)
	       (delete-event ,contact (first ,translation)))))))))




;;;-----------------------------------------------------------------------------
;;; Actions


;; Retained temporarily for compatibility purposes
(defmacro defaction (name lambda-list &body body)
  "Define an action method. THIS MACRO IS NOW OBSOLETE. Just use defmethod."
  (let (qualifier self)

    ;; Handle method qualifiers (:before or :after)
    (when (atom lambda-list)
      (setq qualifier   (list lambda-list)
	    lambda-list (pop body)))

    ;; Get the first specialized parameter in the lambda-list
    (dolist (arg lambda-list)
      (when (member arg lambda-list-keywords) (return nil))
      (when (consp arg)
	(setf self (first arg))))

    `(progn
       (compiler-let (($contact$ ',self))	; Hook for call-action
	 (defmethod ,name ,@qualifier ,lambda-list
	   ,@body)))))


(defmacro using-event (&body body)
  `(locally
     (declare (special $event$))
     ,@body))

(defmacro processing-event-p ()
  `(using-event (boundp '$event$)))

(defmacro with-event (slots &body body)
  "Used within an action method to access event slots."

  `(using-event
     (assert
       (boundp '$event$) nil
       "WITH-EVENT used outside the dynamic extent of PROCESS-NEXT-EVENT.")
     (with-slots ,slots (the event $event$) ,@body)))

(defmacro call-action (action &rest args)
  "Used within DEFACTION to call another action. THIS MACRO IS NOW OBSOLETE. Replace
with a direct reference to the ACTION function."
  (declare (special $contact$))
  (unless (boundp '$contact$)
    (error "CALL-ACTION used outside DEFACTION."))
  `(,action ,$contact$ ,@args))


(proclaim '(inline call-action-internal))
(defun call-action-internal (contact action)
  (if (consp action)
      (apply (car action) contact (cdr action))
      (funcall action contact)))


(defun add-before-action (display class action-name &rest args)
    "Call the action named ACTION-NAME with ARGUMENTS before every event
   on DISPLAY directed to a contact whose class is the same as
   or superclass of the action class."
    (setf (before-actions display)
	  (cons
	    (list* class action-name (copy-list args))
	    (delete-if #'(lambda (entry)
			   (and (eq class (first entry))
				(eq action-name (second entry))))
		       (before-actions display)
		       :count 1)))
    action-name)

(defun delete-before-action (display class action-name)
  "Remove a before event-handler from display"
  (setf (before-actions display)
	(delete-if #'(lambda (entry)
		       (and (eq class (first entry))
			    (eq action-name (second entry))))
		   (before-actions display)
		   :count 1))
  action-name)



;;;-----------------------------------------------------------------------------
;;; BUILT-IN ACTIONS


(defmethod perform-callback ((contact basic-contact) name &rest args)
  ;; WARNING: duplicates apply-callback code, instead of (eval (apply-callback...))
  (let ((functions (callback-p contact name)))
    (when functions
      (let ((args (copy-list args)))		;Cons Alert!!
	(catch :abort-callback
	  (do* ((functions functions         (rest functions))
		(function  (first functions) (first functions)))

	       ((null (rest functions))
		;; Return value(s) of last callback function
		(apply (first function) (nconc args (rest function))))

	    (setf args (nconc args (rest function)))
	    (apply (first function) args)
	    (setf args (nbutlast args (length (rest function))))))))))

(defmethod apply-action ((contact basic-contact) function &rest args)
  (let ((*contact* contact))
    (declare (special *contact*))
    (apply function args)))

(defmethod eval-action ((contact basic-contact)  &rest forms)
  (let ((*contact* contact))
    (declare (special *contact*))
    (dolist (form forms)
      (eval form))))

(defmethod trace-action ((event-contact basic-contact)  &rest exceptions)
  (let (value result
	(name (contact-name event-contact)))
    (with-event ((event-key key))
      (unless (member event-key exceptions :test #'eq)
	(format *trace-output* "~%~s on ~a:"
		event-key name)
	(dolist (slot-name '(above-sibling atom border-width character child code colormap configure-p
			     count drawable event-window focus-p format height hint-p installed-p keymap
			     keysym kind major minor mode name new-p override-redirect-p parent place
			     plist property requestor selection send-event-p state target type width
			     window x y))
	  (when (and (setf value (slot-value (the event $event$) slot-name))
		     (not (eq value event-contact)))
	    (when (typep value 'contact) (setf value (contact-name value)))
	    (setf result (nconc result (list slot-name value)))))
	(format *trace-output* "~{~<~%~20@t~1:; ~s ~s~>~^ ~}." result)))))

(defmethod describe-action ((event-contact basic-contact) &rest exceptions)
  (with-event ((event-key key))
    (unless (member event-key exceptions :test #'eq)
      (format *trace-output* "~%~s on ~a:"
	      event-key (contact-name event-contact))
      ;; Loop over slots in alphabetical order
      (dolist (slot-name '(above-sibling atom border-width character child code colormap configure-p
			   count drawable event-window focus-p format height hint-p installed-p keymap keysym
			   kind major minor mode name new-p override-redirect-p parent place plist
			   property requestor selection send-event-p state target type width window x y))
	(let ((value (slot-value (the event $event$) slot-name)))
	  (when value
	    (when (typep value 'contact) (setf value (contact-name value)))
	    (format *trace-output* "~%~5t~20s~20s" slot-name value))))
      (terpri *trace-output*))))

(defmethod ignore-action ((contact basic-contact))
  ;; Beep on *remap-events* else ignore
  (with-event (key display)
    (when (member key *remap-events* :test #'eq)
      (bell display))))

(defmethod throw-action ((contact basic-contact) tag &optional value)
  (throw tag value))


;;-----------------------------------------------------------------------------
;; EVENT-TRANSLATIONS

(defmacro defevent (class event-spec &rest actions)
  "Add an event binding to the EVENT-TRANSLATIONS property of CLASS,
   where it can be shared by all instances of CLASS."
  (let ((event-parse          (parse-event-translation event-spec actions))
	(canonical-event-spec (gensym)))
    `(progn
       ;; Generate compiler warnings for missing actions
       #+ti ,@(mapcar #'(lambda (action)
			  (when (consp action) (setq action (first action)))
			  `(eval-when (compile)
			     (compiler:function-referenced
			       ',action ',(intern (format nil "DEFEVENT ~s ~s" class event-spec)))))
		      (rest event-parse))
       (let ((,canonical-event-spec ',(first event-parse)))

	 (setf
	   ;; Update class event translations
	   (class-name-event-translations ',class)
	   (cons
	     (cons ,canonical-event-spec ',(rest event-parse))
	     (delete ,canonical-event-spec
		     (class-name-event-translations ',class)
		     :key #'first :test #'equal :count 1))

	   ;; Flush cached class event mask, event precedence list
	   (class-name-event-mask ',class)
	   nil

	   (class-name-event-precedence-list ',class)
	   nil)))))


(defmacro undefevent (class event-spec &rest actions)
  "Remove an event binding from the EVENT-TRANSLATIONS property of CLASS."
  (declare (ignore actions))
  `(setf
       ;; Update class event translations
       (class-name-event-translations ',class)
       (delete ',(first (parse-event-translation event-spec nil))
	       (class-name-event-translations ',class)
	       :key #'first :count 1 :test #'equal)

       ;; Flush cached class event mask, event precedence list
       (class-name-event-mask ',class)
       nil

       (class-name-event-precedence-list ',class)
       nil))



(defmethod event-actions ((contact basic-contact) event-spec)
  "Return the list of actions for EVENT-SPEC."

  ;; Check instance translations
  (let ((event-binding (car (parse-event-translation event-spec nil))))

    (cdr
      (or
	;; Instance translation?
	(assoc event-binding
	       (slot-value (the basic-contact contact) 'event-translations)
	       :test #'equal)

	;; Class translation?
	(dolist (class (class-name-event-precedence-list (class-name-of contact)))
	  (let ((actions (assoc event-binding
				(class-name-event-translations class)
				:test #'equal)))
	    (when actions
	      (return actions))))))))


(defmethod add-event ((contact basic-contact) event-spec &rest actions)
  "Add EVENT-SPEC and ACTIONS to the event translations for CONTACT."

  ;; Compute canonical event translation.
  (let ((translation  (parse-event-translation event-spec (copy-list actions))))
    (with-slots (event-mask event-translations) (the contact contact)

      ;; Translation for this event spec already exists?
      (let ((previous (assoc (first translation) event-translations :test #'equal)))
	(if previous
	    ;; Yes, modify it with the new actions.
	    (setf (rest previous) (rest translation))

	    ;; No, add new translation.
	    (push translation event-translations)))

      ;; Update window event mask, if necessary
      (when (realized-p contact)
	(let ((new-mask (event-translation-mask event-mask translation)))
	  (unless (= new-mask event-mask)
	    (setf (window-event-mask contact) (setf event-mask new-mask)))))))
  (values))

(defmethod delete-event ((contact basic-contact) event-spec)
  "Remove any translation for EVENT-SPEC from the event translations for CONTACT."

    ;; Compute a canonical event translation for the event spec
  (let ((translation (parse-event-translation event-spec nil)))
    (with-slots (event-mask event-translations) (the contact contact)

      ;; Remove any matching translation.
      (setf event-translations
	    (delete (first translation) event-translations
		    :key #'first :count 1 :test #'equal))

      ;; Update window event mask, if necessary
      (when (realized-p contact)
	(let ((old-bit  (event-translation-mask 0 translation)))

	  ;; Don't change event mask if some other translation sets this bit
	  (when (zerop (logand (contact-event-translations-mask contact) old-bit))

	    ;; Only modify the event-mask bit for the event being deleted
	    (setf (window-event-mask contact)
		  (setf event-mask (logandc2 event-mask old-bit))))))))
  (values))





;;;-----------------------------------------------------------------------------
;;; CHECK/MATCH functions


(defun encode-button-number (button)
  (or (position button
		#(:any :button-1 :button-2 :button-3 :button-4 :button-5))
      (error 'type-error :datum button
             :expected-type '(member :Any :button-1 :button-2 :button-3
                              :button-4 :button-5))))

;; Alist associating modifier keys with modifier keysyms
(defvar *meta-modifier-alist*
	'((:meta  #.(keysym :left-meta) #.(keysym :right-meta)
		  #.(keysym :left-alt ) #.(keysym :right-alt ))
	  (:super #.(keysym :left-super) #.(keysym :right-super))
	  (:hyper #.(keysym :left-hyper) #.(keysym :right-hyper))))

(defconstant meta-shift 16.) ;; Where to shift meta-modifier keystates
(defconstant mod-1-shift (position :mod-1 xlib::*state-mask-vector*))
(defconstant button-0-shift (1- (position :button-1 xlib::*state-mask-vector*)))

(defun get-display-modifier-translate (display &optional update-p)
  ;; Returns a table that translates meta-modifier bits
  ;; into mod1/mod2/mod3/mod4/mod5 modifier state bits.
  (declare (type display display)
	   (type boolean update-p))
  (or (and (not update-p) (display-modifier-translate display))
      (let* ((mapping (xlib::get-display-modifier-mapping display))
	     (mod-length (length *meta-modifier-alist*))
	     (translate-length (ash 1 mod-length))
	     (display-translate (display-modifier-translate display))
	     (translate (or (and (>= (length display-translate) translate-length)
				 display-translate)
			    (make-array translate-length)))
	     (mod-vector (make-array mod-length)))
	(declare (type simple-vector translate mod-vector))
	(do* ((modifiers *meta-modifier-alist* (cdr modifiers))
	      (i 0 (1+ i))
	      (temp))
	     ((endp modifiers))
	  (setf (aref mod-vector i)
		(dolist (modifier (cdar modifiers) 0)
		  (when (setq temp (assoc modifier mapping :test #'eq))
		    (return (cdr temp))))))
	(dotimes (i translate-length)
	  (let ((mask 0))
	    (dotimes (j mod-length)
	      (when (logbitp j i)
		(setq mask (logior mask (aref mod-vector j)))))
	    (setf (aref translate i) mask)))
	(setf (display-modifier-translate display) translate))))

(proclaim '(inline translate-meta-modifiers))
(defun translate-meta-modifiers (state translate)
  ;; Translate the meta/super/hyper modifiers in state to mod-1/mod-2/mod3/mod4/mod5 modifiers.
  ;; TRANSLATE is the result from GET-DISPLAY-MODIFIER-TRANSLATE
  (logior (ldb (byte meta-shift 0) state)
	  (aref translate (ash state (- meta-shift)))))

(defun encode-clue-modifier-mask (modifiers)
  ;; Make a state-mask from modifiers
  (declare (type (or mask16 state-mask-key (member :meta :super :hyper) list) modifiers))
  (typecase modifiers
    (fixnum (ldb (byte meta-shift 0) modifiers))
    (cons (let ((mask 0))
	    (dolist (modifier modifiers)
	      (setf mask (logior mask (encode-clue-modifier-mask modifier))))
	    mask))
    (otherwise
     (let ((temp (position modifiers (the list *meta-modifier-alist*) :key #'car :test #'eq)))
       (if temp
	   (ash 1 (+ temp meta-shift))
	 (make-state-mask modifiers))))))

(proclaim '(notinline event-spec-match))
(defun event-spec-match (display state select event-state)
  (let ((translate (get-display-modifier-translate display)))
    (setq state (translate-meta-modifiers state translate)
	  select (translate-meta-modifiers select translate)))
						; The modifiers common to select and state must be DOWN and
						; The modifiers in select but not state must be UP
  (and (= (logand state select) (logand event-state select))
						; When there are modifiers in state that aren't in select
       (or (zerop (logandc2 state select))
						; At least one of them must be DOWN
	   (plusp (logand event-state (logandc2 state select)))))
						; Modifiers that aren't in state or select are ignored
  )

#| ;; event-spec-match implements the following relationships:

  .-------------------------------.
  | event-state  4 4 4 4 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  |4 4 .-------------------. 4 4 4|
  |4 4 |  Select 1 1 1 1 1 | 4 4 4|
  |4 4 | 1 1 1 1 1 1 1 1 1 | 4 4 4|
  |4 4 | 1 .-----------. 1 | 4 4 4| This would look better in color
  |4 4 | 1 | 2 2 2 2 2 | 1 | 4 4 4|
  |4 4 | 1 | 2 2 2 2 2 | 1 | 4 4 4|
  |4 4 | 1 | 2 2 2 2 2 | 1 | 4 4 4|
  |4 4 `---+-----------+---' 4 4 4|
  |4 4 4 4 | 3 3 3 3 3 | 4 4 4 4 4|
  |4 4 4 4 | 3 3 3 3 3 | 4 4 4 4 4|
  |4 4 4 4 | State 3 3 | 4 4 4 4 4|
  |4 4 4 4 `-----------' 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  `-------------------------------'

 1. Modifiers in select but not state		; Must be UP in the event
 2. Modifiers both in state and select		; Must be DOWN in the event
 3. Modifiers in state but not select		; If any, at least one must be down
 4. Modifiers in neither select or state	; are ignored

|#

(defun key-check (event-key &optional char state select)
  ;; Returns the canonical form of the key (i.e. one that may speed up
  ;; the matching operation)
  (declare (ignore event-key))
  (unless (typep char '(or card16 character (member :any)))
    (error "~a is not a CHARACTER, CARD16 or :ANY." char))

  (let*
    ((modifiers
       (cond ((or (null state) (eq state :none))   0)
	     ((numberp state)                      state)
	     ((eq state :any)                      (setf select 0))
	     (t                                    (encode-clue-modifier-mask state))))
     (mask
       (cond ((null select)                        (if (characterp char) 0 modifiers))
	     ((numberp select)                     select)
	     ((eq select :same)                    modifiers)
	     ((eq select :all)                     #xffff)
	     (t                                    (encode-clue-modifier-mask select)))))

    (list 'key-match char modifiers mask)))

(defun key-match (event spec-code spec-state spec-select)
  ;; Returns T if EVENT-SPEC matches EVENT
  (with-slots (display character state keysym) (the event event)
    (and (event-spec-match display spec-state spec-select state)
	 (cond
	   ((characterp spec-code) (eql spec-code character))
	   ((eq spec-code :any)    t)
	   (t                      (eql spec-code keysym))))))

(defun button-check (event-key &optional code state select)
  (let*
    ((click-option
       (when (consp state)
	 ;; Process state list for click type keys
	 (let* ((single (find :single-click state :test #'eq))
		(double (find :double-click state :test #'eq))
		(click  (or single double)))
	   (when click
	     (assert (not (and single double)) ()
		     "Can't specify both :SINGLE-CLICK and :DOUBLE-CLICK.")
	     (setf state (remove click state)))
	   click)))

     (button
       (encode-button-number (or code :any)))

     (modifiers
       (cond ((or (null state) (eq state :none))   0)
	     ((numberp state)                      state)
	     ((eq state :any)                      (setf select 0))
	     ((eq state :single-click)             (setf click-option state) 0)
	     ((eq state :double-click)             (setf click-option state) 0)
	     (t                                    (encode-clue-modifier-mask state))))
     (mask
       (cond ((or (null select) (eq select :same)) modifiers)
	     ((numberp select)                     select)
	     ((eq select :all)                     #xffff)
	     (t                                    (encode-clue-modifier-mask select))))

     (predicate
       (if (eq event-key :button-press)
	   'button-press-match
	   'button-release-match)))

    (list* predicate button modifiers mask (when click-option (list click-option)))))

(defun button-press-mask (button state select &optional option)
  (declare (ignore button state select))
  (if option
      #.(make-event-mask :button-press :button-release)
      #.(make-event-mask :button-press)))

(defun button-release-mask (button state select &optional option)
  (declare (ignore button state select))
  (if option
      #.(make-event-mask :button-press :button-release)
      #.(make-event-mask :button-release)))

(defun button-press-match (event button state select &optional option)
  (declare (type event event)
	   (type card8 button)
	   (type card16 state))
  (with-slots ((event-code code)
	       (event-state state)
	       display key plist time x y)
	      (the event event)
    (let* ((code event-code)
	   (mask (ash 1 (+ code button-0-shift))))
      (declare (type card8 code))
      (and
	(or (zerop button)			; Zero button means "any" button
	    (=  button code))
	(event-spec-match display state select
			  (logandc2 event-state mask))	; Clear state bit for current button
	(case option
	  (:single-click
	   (= (click-lookahead display 1 2 time (logior event-state mask) x y) 2))
	  (:double-click
	   (= (click-lookahead display 1 4 time (logior event-state mask) x y) 4))
	  (otherwise t))))))

(defun button-release-match (event button state select &optional option)
  (with-slots ((event-code code)
	       (event-state state)
	       key display plist time x y)
	      (the event event)
    (let* ((code event-code)
	   (mask (ash 1 (+ code button-0-shift))))
      (and
	(or (zerop button)			; Zero button means "any" button
	    (=  button code))
	(event-spec-match display state select (logandc2 event-state mask))	; Clear state bit for current button
	(case option
	  (:single-click
	   (= (click-lookahead display 2 2 time event-state x y) 2))
	  (:double-click
	   (= (click-lookahead display 2 4 time event-state x y) 4))
	  (otherwise t))))))

(defconstant all-button-mask
	     (make-state-mask :button-1 :button-2 :button-3 :button-4 :button-5))

(defun motion-check (event-key &optional state select)
  (declare (ignore event-key))
  (let*
    ((modifiers
       (cond ((or (null state) (eq state :none))   0)
	     ((numberp state)                      state)
	     ((eq state :any)                      (setf select 0) all-button-mask)
	     (t                                    (encode-clue-modifier-mask state))))
     (mask
       (cond ((or (null select) (eq select :same)) modifiers)
	     ((numberp select)                     select)
	     ((eq select :all)                     #xffff)
	     (t                                    (encode-clue-modifier-mask select)))))

    (list 'motion-match modifiers mask)))

(defun motion-match (event state select)
  (with-slots (display) (the event event)
    (or (eq state :any)
	(event-spec-match display state select (slot-value (the event event) 'state)))))

(defun motion-event-mask (state select)
  (if (= all-button-mask (logand (logior state select) all-button-mask))
      #.(make-event-mask :button-motion)
      (let ((mask (logand state select all-button-mask)))
	(when (zerop mask)
	  (setq mask #.(make-event-mask :pointer-motion)))
	mask)))

(eval-when (compile) ;; motion-event-mask makes the following assumption:
  (assert (and (= (make-event-mask :button-1-motion) (make-state-mask :button-1))
	       (= (make-event-mask :button-2-motion) (make-state-mask :button-2))
	       (= (make-event-mask :button-3-motion) (make-state-mask :button-3))
	       (= (make-event-mask :button-4-motion) (make-state-mask :button-4))
	       (= (make-event-mask :button-5-motion) (make-state-mask :button-5)))
	  () "Button event-mask is shifted relative to button state-mask"))

(defun enter-leave-check (event-key &rest kinds)
  (dolist (kind kinds)
    (unless (member kind '(:ancestor :virtual :inferior :nonlinear :nonlinear-virtual))
      (error "~s isn't an enter/leave kind for ~s" kind (cons event-key kinds))))
  (list 'enter-leave-match kinds))

(defun enter-leave-match (event kinds)
  (member (slot-value (the event event) 'kind) kinds :test #'eq))

(setf (check-function :key-press) 'key-check)
(setf (check-function :key-release) 'key-check)
(setf (check-function :button-press) 'button-check)
(setf (check-function :button-release) 'button-check)
(setf (check-function :motion-notify) 'motion-check)
(setf (check-function :enter-notify) 'enter-leave-check)
(setf (check-function :leave-notify) 'enter-leave-check)

(defun key-up-check (event-key &rest parms)
  (declare (ignore event-key))
  ;; Convert (:up ...) to (:key-press ...)
  (values (apply #'key-check :key-release parms)
	   :key-release))

(setf (check-function :up) #'key-up-check)

(defun client-message-check (event-key type &rest accessors)
  (declare (ignore event-key))
  (assert (typep type 'xatom) () "~s must be an X atom." type)
  (do* ((accessors accessors          (cddr accessors))
	(function  (first accessors)  (first accessors))
	(rest      (rest accessors)   (rest accessors)))
       ((null accessors))
    (assert rest () "No value given for ~s accessor." function))
  (values (list* 'client-message-match
		 (intern (string type) 'keyword)
		 accessors)
	  :client-message))

(defun client-message-match (event type &rest accessors)
  (with-slots ((event-type type) (event-data data) (event-display display)) event
    ;; Bind display for use in accessor functions
    (let ((*event-display* event-display))
      (declare (special *event-display*))
      (and (eq type event-type)
	   (do* ((accessors accessors          (cddr accessors))
		 (function  (first accessors)  (first accessors))
		 (value     (second accessors) (second accessors)))
		((null accessors) t)
	     (unless (equal value (funcall function event-data))
	       (return nil)))))))

(setf (check-function :client-message) #'client-message-check)


(defun wm-protocol-check (event-key &rest accessors)
  (apply 'client-message-check
	 :client-message :wm_protocols
	 'wm-message-protocol-atom event-key
	 accessors))

(setf (check-function :wm_take_focus)    #'wm-protocol-check)
(setf (check-function :wm_save_yourself) #'wm-protocol-check)
(setf (check-function :wm_delete_window) #'wm-protocol-check)

(defun timer-check (event-key timer-name)
  (declare (ignore event-key))
  (assert (symbolp timer-name) ()
	  "~a is not a timer name symbol." timer-name)
  (values
    (list 'timer-match timer-name)
    :timer))

(defun timer-match (event timer-name)
  (with-slots (name) (the event event)
    (eq timer-name name)))

(setf (check-function :timer) #'timer-check)

(defun property-check (event &optional property state)
  (declare (ignore event))
  (check-type property (or null xatom) "an XATOM")
  (check-type state    (or null (member :new-value :deleted)) ":NEW-VALUE or :DELETED")
  (cons
    'property-match
    (when property
      (cons (intern (string property) :keyword)
	    (when state
	      (cons state nil))))))

(defun property-match (event &optional property state)
  (with-slots ((event-property atom) (event-state state)) (the event event)
    (or (not property)
	(and (eq property event-property)
	     (or (not state) (eq state event-state))))))

(setf (check-function :property-notify) 'property-check)

;;;-----------------------------------------------------------------------------
;;; DOUBLE-CLICK events
(defun click-lookahead (display count max first-time state first-x first-y)
  (declare (type card8 count)) ;; even when the button is UP, odd when DOWN
  #+cmu ;; Hard to optimize this -- but it is rarely used.
  (declare (optimize (ext:inhibit-warnings 3)))
  (let* ((multipress-verify-p (display-multipress-verify-p display))
	 (multipress-delay-limit (display-multipress-delay-limit display))
	 (timeout (/  multipress-delay-limit 1000.0))
	 (distance-limit 5))

    (flet ((get-result (count timeoutp)
		       ;; If the result from GET-RESULT is NIL, all lookahead events
		       ;; remain on the event queue, otherwise the events are removed,
		       ;; and the result from GET-RESULT is returned.
		       (if (or (evenp count)	; Hold events only occur on timeout
			       timeoutp)
			   count
			   0)))
      (loop
	(let*
	  ((timeout-p t)
	   (result
	     (block result
	       ;; When succeeding, we want to "eat" the events.
	       ;; When failing, we want to leave events on the event queue.
	       ;; We're careful to return non-nil from event-case only on success.
	       ;; On failure, we return-from result, which leaves events on the queue.
	       ;; the timeout-p hair is to detect the difference between failure and timeout.
	       (event-case (display :timeout timeout :force-output-p nil)
		 ((motion-notify) (x y)		; Fail when pointer moves more than a jiggle
		  (setq timeout-p nil)
		  (when (> (+ (abs (- x first-x)) (abs (- y first-y))) distance-limit)
		    (return-from result (get-result count nil))))

		 ((enter-notify leave-notify) ()	; Fail when pointer moves to a new window
		  (setq timeout-p nil)
		  (return-from result (get-result count nil)))

		 (button-press (time (state event-state) code)
			       (setq timeout-p nil)
			       (cond ((>= count max) (return-from result :count))
				     ((> time (+ first-time multipress-delay-limit))
				      (return-from result :timeout))
				     ((or (oddp count)
					  (not (= state (logior event-state (ash 1 (+ code button-0-shift))))))
				      (return-from result (get-result count nil)))
				     (t (let ((result (click-lookahead display (1+ count) max
								       time state first-x first-y)))
					  (if (plusp result)
					      result
					      (if (plusp (setq result (get-result count nil)))
						  (return-from result result)
						  nil ;; else fall-through returning NIL
						  ))))))

		 (button-release (time (state event-state))
				 (setq timeout-p nil)
				 (cond ((>= count max) (return-from result :count))
				       ((> time (+ first-time multipress-delay-limit))
					(return-from result :timeout))
				       ((or (evenp count)
					    (not (= state event-state)))
					(return-from result (get-result count nil)))
				       (t (let ((result (click-lookahead display (1+ count) max
									 time state first-x first-y)))
					    (if (plusp result)
						result
						(if (plusp (setq result (get-result count nil)))
						    (return-from result result)
						    nil ;; else fall-through returning NIL
						    ))))))))))

	  (if timeout-p
	      ;; event-case timed out
	      (if (or (zerop timeout)
		      (not multipress-verify-p))

		  (return (get-result count :local-timeout))

		  (progn
		    ;; Verify timeout with a server round-trip and event-queue recheck
		    (display-finish-output display)
		    (setq timeout 0)))

	      ;; Else exit loop with result
	      (return (case result
			(:timeout  (get-result (1- count) :timeout))
			(:count    0)
			((nil)     0)
			(otherwise result)))))))))



;;;-----------------------------------------------------------------------------
;;; EVENT-PROCESSING

(defun process-all-events (display &optional (update-state-p t))
  "Repeatedly flush output and process resulting events until event queue is empty."
  (loop
    ;; Flush output buffer and wait for resulting events
    (display-finish-output display)

    ;; Any events left to process?
    (if (event-listen display 0)

	;; Yes, process remaining event queue
	(loop
	  (unless (process-next-event display 0 update-state-p) (return)))

	;; No
	(return))))


(defun process-next-event (display &optional timeout (update-state-p t))
  "Process one event. Call UPDATE-STATE iff UPDATE-STATE-P is true. "
  (declare (type display display)
	   (type (or null number) timeout)
	   (type boolean update-state-p)
	   #+cmu (optimize (ext:inhibit-warnings 3)))

  ;; Ensure consistent contact states
  (when update-state-p (update-state display))

  (let*
    (;; Process any timers that have expired
     (interval-until-next-timer  (execute-timers display))

     ;; Compute true timeout
     (wait-for-timer-p (when (or (null timeout)
				 (and interval-until-next-timer
				      (< interval-until-next-timer timeout)))
			 interval-until-next-timer))

     (event                      (allocate-event))
     (result                     nil))

    (declare (type event event))

    (setf (slot-value (the event event) 'display) display)

    (macrolet
	((set-event (&rest parameters)
	   `(progn ,@(mapcar
		      #'(lambda (parm)
			  `(setf (slot-value (the event event) ',parm) ,parm))
		      parameters)))
	 (dispatch (contact)
	   `(progn
	      (dispatch-event event event-key send-event-p sequence ,contact)
	      t)))
      ;; Wait for an event, copy info into the EVENT structure then call DISPATCH-EVENT
      (setf
	result
	(or
	  (xlib:event-cond (display :timeout (or wait-for-timer-p timeout)
				    :force-output-p t
				    :discard-p t)
	    ((:key-press :key-release :button-press :button-release)
	     (code time root window child root-x root-y x y
		   state same-screen-p event-key sequence send-event-p) t
	     (set-event code time root window child root-x root-y x y
			state same-screen-p)
	     (dispatch window))

	    (:motion-notify
	      (hint-p time root window child root-x root-y x y
		      state same-screen-p event-key sequence send-event-p) t
	      (set-event hint-p time root window child root-x root-y x y
			 state same-screen-p)
	      (dispatch window))

	    ((:enter-notify :leave-notify)
	     (kind time root window child root-x root-y x y
		   state mode focus-p same-screen-p event-key sequence send-event-p) t
	     (set-event kind time root window child root-x root-y x y
			state mode focus-p same-screen-p)
	     (dispatch window))

	    ((:focus-in :focus-out)
	     (kind window mode event-key sequence send-event-p) t
	     (set-event kind window mode)
	     (dispatch window))

	    (:exposure
	      (window x y width height count event-key sequence send-event-p) t
	      (set-event window x y width height count)
	      (dispatch window))

	    (:graphics-exposure
	      (drawable x y width height count major minor event-key sequence send-event-p) t
	      (set-event drawable x y width height count major minor)
	      (dispatch drawable))

	    (:no-exposure
	      (drawable major minor event-key sequence send-event-p) t
	      (set-event drawable major minor)
	      (dispatch drawable))

	    (:visibility-notify
	      (window state event-key sequence send-event-p) t
	      (set-event window state)
	      (dispatch window))

	    (:create-notify
	      (parent window x y width height border-width
		      override-redirect-p event-key sequence send-event-p) t
	      (set-event parent window x y width height border-width
			 override-redirect-p)
	      (dispatch parent))

	    (:destroy-notify
	      (event-window window event-key sequence send-event-p) t
	      (set-event event-window window)
	      (dispatch event-window))

	    (:unmap-notify
	      (event-window window configure-p event-key sequence send-event-p) t
	      (set-event event-window window configure-p)
	      (dispatch event-window))

	    (:map-notify
	      (event-window window override-redirect-p event-key sequence
	       send-event-p) t
	      (set-event event-window window override-redirect-p)
	      (dispatch event-window))

	    (:map-request
	      (parent window event-key sequence send-event-p) t
	      (set-event parent window)
	      (dispatch parent))

	    (:reparent-notify
	      (event-window window parent x y override-redirect-p event-key
	       sequence send-event-p) t
	      (set-event event-window window parent x y override-redirect-p)
	      (dispatch event-window))

	    (:configure-notify
	      (event-window window above-sibling x y width height border-width
			    override-redirect-p event-key sequence send-event-p) t
	      (set-event event-window window above-sibling x y width height
			 border-width override-redirect-p)
	      (dispatch event-window))

	    (:configure-request
	      (parent window above-sibling x y width height border-width
	       event-key sequence send-event-p) t
	      (set-event parent window above-sibling x y width height border-width)
	      (dispatch parent))

	    (:gravity-notify
	      (event-window window x y event-key sequence send-event-p) t
	      (set-event event-window window x y)
	      (dispatch event-window))

	    (:resize-request
	      (window width height event-key sequence send-event-p) t
	      (set-event window width height)
	      (dispatch window))

	    (:circulate-notify
	      (event-window window parent place event-key sequence send-event-p) t
	      (set-event event-window window parent place)
	      (dispatch event-window))

	    (:circulate-request
	      (parent window place event-key sequence send-event-p) t
	      (set-event parent window place)
	      (dispatch parent))

	    (:property-notify
	      (window atom time state event-key sequence send-event-p) t
	      (set-event window atom time state)
	      (dispatch window))

	    (:selection-clear
	      (time window selection event-key sequence send-event-p) t
	      (set-event time window selection)
	      (dispatch window))

	    (:selection-request
	      (time window requestor selection target property event-key
	       sequence send-event-p) t
	      (set-event time window requestor selection target property)
	      (dispatch window))

	    (:selection-notify
	      (time window selection target property event-key sequence send-event-p) t
	      (set-event time window selection target property)
	      (dispatch window))

	    (:colormap-notify
	      (window colormap new-p installed-p event-key sequence send-event-p) t
	      (set-event window colormap new-p installed-p)
	      (dispatch window))

	    (:client-message
	      (format window type data event-key sequence send-event-p) t
	      (set-event format window type data)
	      (dispatch window))

	    (:keymap-notify      ; Special case
	      (keymap event-key send-event-p) t
	      (set-event keymap) ; keymap-notify doesn't have an associated window.
	      (let ((sequence 0))
		; Send keymap-notify events to the root.
		(dispatch (display-root display))) )
	    (:mapping-notify			; Special case
	      (request start count) t
	      (mapping-notify display request start count)
	      (when (eq request :modifier) ; Update the modifier mapping translate table
		(get-display-modifier-translate display :update))
	      t))

	  ;; No event read -- return true (i.e. no timeout)
	  ;;                  if we now have a timer ready
	  (when wait-for-timer-p t))))

    ;; We could add an unwind protect to ensure that the event is always
    ;; deallocated (process-next-event is sometimes thrown out of).
    ;; However, we judge that an unwind-protect all the time is more
    ;; expensive than garbage collecting an event structure some of the
    ;; time.
    (deallocate-event event)
    result))

(defun dispatch-event (event event-key send-event-p sequence contact)
  ;; Called from PROCESS-NEXT-EVENT to filter events and call event handlers.
  ;; Hack on by pw on advice of efficiency notes.
  (declare (type event   event)
	   (type keyword event-key)
	   (type boolean send-event-p)
	   (type card16  sequence)
	   (type contact contact))
  (declare (optimize speed (safety 0) (space 0)
		     #+cmu (ext:inhibit-warnings 3)))
  (declare (inline sensitive-p))

  (with-slots ((event_key key)
	       (event-sequence sequence)
	       (event-send-event-p send-event-p)
	       (event-contact contact)) (the event event)
    (setf event_key event-key
	  event-send-event-p send-event-p
	  event-sequence sequence
	  event-contact contact))

  (let ((class (class-name-of contact)))
    ;;
    ;; Check for non-contact event drawables.
    ;;
    (if (or (eq class 'window) (eq class 'pixmap))

	(handle-event (display-root (drawable-display contact)) event)

	(if (destroyed-p contact)

	    ;; Destroyed-contact!
	    (when (eq event-key :destroy-notify)
	      (destroy-finish contact))

	    ;; Bind event for reference within with-event forms
	    (let ((display (slot-value contact 'display))
		  ($event$ event))
	      (declare (special $event$))

	      ;;
	      ;; Do key translation
	      ;;
	      (when (or (eq event-key :key-press)
			(eq event-key :key-release))
		(with-slots (keysym character code state) (the event event)
		  (let ((keysym-index (default-keysym-index display code state)))
		    (setf keysym (keycode->keysym display code keysym-index)
			  character (keycode->character
				     display code state
				     :keysym-index keysym-index)))))
	      ;;
	      ;; Call the before event handlers
	      ;;
	      (dolist (before-action (before-actions display))
		(when (subtypep class (first before-action))
		  (call-action-internal contact (rest before-action))))
	      ;;
	      ;; Handle insensitive contacts
	      ;;
	      (when (and (member event-key *sensitive-events* :test #'EQ)
			 (not (sensitive-p contact)))
		(return-from dispatch-event nil))

	      ;;
	      ;; Handle modes
	      ;;
	      (let ((modes (display-mode-stack display)))
		(when (and modes (not (contact-mode contact)))
		  (when
		    (or (member event-key *restrict-events* :test #'eq)
			(and (member event-key *remap-events* :test #'eq)
			     ;; Search for first :spring-loaded mode
			     (dolist (mode modes t)
			       (when (eq (second mode) :spring-loaded)
				 ;; *** DEBUG ***
				 (format t "~%Remapping ~s from ~s to ~s"
					 event-key contact (first mode))
				 (setq contact (first mode)) ;; Remap contact
				 (return nil)))))
		    ;; Call mode action on for first :exclusive or :spring-loaded mode
		    (dolist (mode modes)
		      (unless (eq (second mode) :non-exclusive)
			(call-action-internal (first mode) (cddr mode))
			;; quit
			(return-from dispatch-event nil))))))

	      ;;
	      ;; Handle event compression
	      ;;
	      (with-slots ((contact-compress-motion compress-motion)
			   (contact-compress-exposures compress-exposures))
			  (the contact contact)

		(case event-key
		  (:exposure			; Check for exposure compression
		   (with-slots ((event-x x)(event-y y)
				(event-w width)(event-h height)
				(event-count count)) (the event event)
		     (when (and (eq contact-compress-exposures :on)
				(plusp event-count))
		       ;; Accumulate total exposed area into one event
		       (let* ((exposed-min-x event-x)
			      (exposed-min-y event-y)
			      (exposed-max-x (+ exposed-min-x event-w))
			      (exposed-max-y (+ exposed-min-y event-h))
			      (compressed    0))
			 (declare (type int16 exposed-min-x exposed-min-y)
				  (type card16 exposed-max-x exposed-max-y)
				  (fixnum compressed))

			 (event-case (display :force-output-p nil :discard-p t)
			   ;; Assert: We can discard all events up to
			   ;; 0-count :exposure because the protocol says
			   ;; that no non-exposure events can intervene.
			   (:exposure
			    (x y width height count)
			    ;; Locally doesn't seem to do the right thing?
			    (let ((x x)(y y)(width width)(height height)(count count))
			      (declare (type int16 x y)
				       (type card16 width height count))
			      (setf exposed-min-x (min x exposed-min-x)
				    exposed-min-y (min y exposed-min-y)
				    exposed-max-x (max (+ x width) exposed-max-x)
				    exposed-max-y (max (+ y height)exposed-max-y))
			      (incf compressed)
			      (zerop count))))

			 (setf event-x exposed-min-x
			       event-y exposed-min-y
			       event-w (- exposed-max-x exposed-min-x)
			       event-h (- exposed-max-y exposed-min-y)
			       event-count  0)

			 ;; Ensure all of exposed region reported has been cleared.
			 (when (> compressed 1)
			   (clear-area
			    contact
			    :x      event-x
			    :y      event-y
			    :width  event-w
			    :height event-h))))))

		  (:motion-notify		; Check for motion compression
		   (when (eq contact-compress-motion :on)
		     (let ((count 0))
		       (declare (fixnum count))
		       ;; Count consecutive :motion-notify's currently in queue
		       (event-case (display :force-output-p nil :peek-p t :timeout 0)
			 (:motion-notify
			  (window)
			  (not (and (eq window contact) (incf count))))
			 (otherwise ()   t))

		       (when (plusp count)
			 ;; Remove all but last and quit immediately
			 (do () ((zerop (decf count)))
			   (event-case (display :timeout 0)
			     (otherwise ()   t)))
			 (return-from dispatch-event nil)))))))
	      ;;
	      ;; Handle event translations
	      ;;
	      (handle-event contact event))))))

(defmethod handle-event ((contact basic-contact) (event event))
  "Do event/callback translation based on the event-translations slot."
  (declare (type contact contact)
	   (type event event))
  ;;
  ;; Handle universal events
  ;;
  (when (eq :exposure (slot-value (the event event) 'key))
    (with-slots (x y width height) (the event event)
      (display contact x y width height)))

;; The following "universal event" is obsolete -- use shells for top-level windows
;    (:configure-notify
;     ;; A contact's x/y/width/height/border-width get updated immediately when
;     ;; changing geometry.  Top-level windows however, have their geometry
;     ;; arbitrated by the window-manager.  It probably doesn't make sense for
;     ;; a non-top-level contact to select structure-notify.  Because CLUE allows
;     ;; any contact to be top-level, CLUE automatically selects structure-notify
;     ;; for top-level contracts, and we set the size/position here.
;     ;; If non-top-level contacts select structure-notify, we let them handle it
;     ;; themselves.
;     ;;
;     ;; This is inadequate.  The geometry manager for the
;     ;; root should be used instead, waiting for the configure-notify,
;     ;; and returning an appropriate successs-p parameter.
;     ;;
;     (with-slots (x y width height border-width window) (the event event)
;       (when (and (eq window contact) (top-level-p contact))
;	 (without-requests contact
;	   (move contact x y)
;	   (resize contact width height border-width)))))

  ;;
  ;; Translate event and perform contact actions
  ;;
  (dolist (action (translate-event contact event))
    (call-action-internal contact action))

  t)

(defun translate-event (contact event)
  "Returns the actions for the first event-translation matching EVENT"
  (declare (type basic-contact contact)
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
			      (apply (cadr event-spec) event (cddr event-spec))))
		   (return event-binding))))))

    (let ((key (slot-value (the event event) 'key)))
      (cdr
	(or
	  ;; Instance translations
	  (find-translation event key (slot-value (the basic-contact contact) 'event-translations))

	  ;; Class translations
	  (dolist (class (class-name-event-precedence-list (class-name-of contact)))
	    (let ((translation (find-translation event key (class-name-event-translations class))))
	      (when translation (return translation)))))))))

(defmethod translate-key ((contact contact) event)
  ;; Find a translation for :key-press event EVENT which
  ;; was originally sent to CONTACT.
  (let* ((parent (contact-parent contact))
	 (siblings (and parent (composite-children parent)))
	 actions)
    (or ;; Check for handled by a sibling
      (dolist (sibling siblings)
	(unless (eq sibling contact)
	  (when (setq actions (translate-event sibling event))
	    (setf (slot-value (the event event) 'contact) sibling)
	    (dolist (action actions t)
	      (call-action-internal sibling action))
	    (return t))))
      ;; If not handled by a sibling of contact, check the parent
      (when (and parent
		 (setq actions (translate-key parent event)))
	(setf (slot-value (the event event) 'contact) parent)
	(dolist (action actions t)
	  (call-action-internal parent action)))
      ;; Not handled by parent, recurse up to the parent
      (translate-key parent contact))))


;;-----------------------------------------------------------------------------
;; TIMERS

(defstruct timer
  name
  time
  interval
  contact
  data)

(defun add-timer (contact name interval &optional data)
  "Send a :timer event to CONTACT every INTERVAL seconds passing DATA
 The timer will be named NAME.  The event is passed DATA NAME CONTACT and DISPLAY"
  ;; Timers are automatically removed when CONTACT is destroyed
  (declare (type contact contact)
	   (type number interval) ;; in seconds
           )
  (delete-timer contact name)
  (insert-timer (make-timer
		  :name name
		  :interval (* interval internal-time-units-per-second)
		  :contact contact
		  :data data))
  name)

;; Internal function
(defun insert-timer (timer)
  ;; Insert timer into its timer-queue
  (let* ((display  (contact-display (timer-contact timer)))
	 (queue    (timer-queue display))
	 (interval (timer-interval timer))
	 (time     (+ interval (get-internal-real-time))))

    (setf (timer-time timer) time)

    ;; Insert in order of execution (youngest first)
    (if (or (null queue) (< time (timer-time (first queue))))

	(push timer (timer-queue display))

	(loop
	  (when (or (null (cdr queue))
		    (< time (timer-time (cadr queue))))
	    (return (setf (cdr queue) (cons timer (cdr queue)))))
	  (pop queue)))

    timer))

(defun delete-timer (contact &optional timer-name)
  "Remove timer named TIMER-NAME from CONTACT
 If timer-name is NIL, remove ALL timers from CONTACT.
 Returns NIL when timer not found, else T."
  (let* ((display (contact-display contact))
	 (timer-queue (timer-queue display))
	 (deletedp nil))
    (dolist (timer timer-queue)
      (when (and (eq (timer-contact timer) contact)
		 (or (null timer-name)
		     (equal (timer-name timer) timer-name)))
	(setq deletedp t)
	(setf (timer-queue display)
	      (delete timer timer-queue :test #'eq :count 1))
	(when timer-name
	  (return t))))
    ;; Return T when timers deleted
    deletedp))


(defun execute-timers (display)
  "Execute all timers whose time has come, returning the time (in seconds)
 before the next timer executes for DISPLAY"
  (loop
    (let ((next-timer (car (timer-queue display))))

      (unless next-timer
	;; No timers active
	(return nil))

      (let ((next-time  (timer-time next-timer)))
	(when (> next-time (get-internal-real-time))
	  ;; Return time interval before next timer fires
	  (return
	    (/ (- next-time (get-internal-real-time))
	       #.(float internal-time-units-per-second)))))

      ;; Reinsert timer for next firing
      (pop (timer-queue display)) ;; Warning: If an abort happens here, There's a short
      (insert-timer next-timer)   ;;          interval where a timer may be lost.

      ;; Dispatch a :timer event
      (let ((event (allocate-event)))
	(with-slots ((event-display display)
		     name data) (the event event)
	  (setf event-display display
		name (timer-name next-timer)
		data (timer-data next-timer)))
	(dispatch-event event :timer nil 0 (timer-contact next-timer))
	(deallocate-event event)))))


;;;-----------------------------------------------------------------------------
;;; Utility functions

(defun describe-event-translations (contact &optional (stream *standard-output*))
  "Print the event translations for CONTACT. If contact is a contact class name,
print the event translations for that contact class."
  (flet ((print-event (class event stream)
	   (format stream "~%From ~20a ~s" class (car event))
	   (dolist (action (cdr event))
	     (write-char #\space stream)
	     (prin1 action stream))))

    (let ((translations (when (typep contact 'basic-contact)
			  (slot-value (the basic-contact contact) 'event-translations))))

      ;; Print instance event translations for the contact
      (dolist (event translations)
	(print-event contact event stream))

      ;; Print event-translations for the contact's superclasses
      (dolist (class (class-name-event-precedence-list
		       (if (symbolp contact) contact (class-name-of contact))))
	(dolist (event (class-name-event-translations class))
	  (unless (assoc (car event) translations :test #'equal)
	    (print-event class event stream)
	    (push event translations)))))))
