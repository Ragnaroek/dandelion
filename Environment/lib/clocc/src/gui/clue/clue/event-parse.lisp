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

(export '(
	 check-function
	 )
	"CLUEI")



;;;-----------------------------------------------------------------------------
;;; EVENT-PARSING

;; In X, there are event-NAMEs, which are used to specify which events are desired,
;; and event-KEYs, which are the names of specific events.  This ALIST maps
;; event-KEYs to event-MASK's.  This is used for automagicly creating the
;; event-name mask for window creation from the list of event-keys a contact is 
;; interested in.  If a third alist element is present, its a function to call
;; to compute the event-mask from the match parameters.


;; Optimized by listing in order of decreasing frequency of access. Access during
;; runtime (i.e. via add-event) should be fastest.

(defparameter
  *event-mask-alist*
  '((:configure-notify   #.(make-event-mask :structure-notify))
    (:unmap-notify       #.(make-event-mask :structure-notify))
    (:map-notify         #.(make-event-mask :structure-notify))
    (:button-press       #.(make-event-mask :button-press)       button-press-mask)
    (:button-release     #.(make-event-mask :button-release)     button-release-mask)
    (:motion-notify      #.(make-event-mask :pointer-motion)     motion-event-mask)
    (:timer)
    (:enter-notify       #.(make-event-mask :enter-window))
    (:leave-notify       #.(make-event-mask :leave-window))
    (:key-press          #.(make-event-mask :key-press))
    (:key-release        #.(make-event-mask :key-release))
    (:focus-in           #.(make-event-mask :focus-change)) 
    (:focus-out          #.(make-event-mask :focus-change))
    (:timer)
    (:property-notify    #.(make-event-mask :property-change))
    (:exposure           #.(make-event-mask :exposure))
    (:keymap-notify      #.(make-event-mask :keymap-state))
    (:graphics-exposure)
    (:no-exposure)
    (:visibility-notify  #.(make-event-mask :visibility-change))
    (:create-notify)	 ; substructure-notify on parent
    (:destroy-notify     #.(make-event-mask :structure-notify))  ; or substructure-notify on parent
    (:unmap-notify       #.(make-event-mask :structure-notify))  ; or substructure-notify on parent
    (:map-notify         #.(make-event-mask :structure-notify))  ; or substructure-notify on parent
    (:map-request)	 ; substructure-notify on parent
    (:reparent-notify    #.(make-event-mask :structure-notify))  ; or substructure-notify on parent
    (:configure-notify   #.(make-event-mask :structure-notify))  ; or substructure-notify on parent
    (:gravity-notify     #.(make-event-mask :structure-notify))  ; or substructure-notify on parent
    (:resize-request     #.(make-event-mask :resize-redirect))
    (:configure-request) ; substructure-notify on parent
    (:circulate-notify   #.(make-event-mask :structure-notify))  ; or substructure-notify on parent
    (:circulate-request) ; substructure-notify on parent    
    (:selection-clear)
    (:selection-request)
    (:selection-notify)
    (:colormap-notify    #.(make-event-mask :colormap-change))
    (:client-message)
    (:mapping-notify)))



(defmacro check-function (event-key)
  "Return the check function for parsing the EVENT-KEY."
  `(get ,event-key 'check))


(defun parse-event-translation (event-spec actions)
  "Return a canonical form of an event translation."
  
  (cons
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
       (unless (assoc event-spec *event-mask-alist* :test #'eq)
	 (error "~s is not a known event keyword." event-spec))
       event-spec))
    
    actions))


(proclaim '(inline event-translations-mask))
(defun event-translations-mask (translations &optional (initial-mask 0))
  "Return the bitwise-or of the INITIAL-MASK and the event mask 
specified by the given event TRANSLATIONS."
  (reduce #'event-translation-mask translations :initial-value initial-mask))

(defun event-translation-mask (initial-mask translation)
  "Return the bitwise-or of the INITIAL-MASK and the event mask 
specified by the given event TRANSLATION."
  (logior
    initial-mask
    
    ;; Compute mask needed for this translation.
    (multiple-value-bind (event-key event-spec-args)
	(let ((event-spec  (first translation)))
	  (if (consp event-spec)
	      (values (first event-spec) (cddr event-spec))
	      event-spec))
      
      ;; Get event mask and filter function for the event keyword.
      (let* ((mask-spec   (rest (assoc event-key *event-mask-alist* :test #'eq)))
	     (mask        (first mask-spec))
	     (mask-filter (second mask-spec)))
	(cond	   
	  ((null mask) 0)				 ; Return empty mask
	  
	  ((not (and mask-filter event-spec-args))	 ; Return mask for event-key
	   mask)
	  
	  (t (apply mask-filter event-spec-args))	 ; Return mask for event-spec
	  )))))


(defmacro clue-resources (class-name)
  `(get ,class-name 'resources))


(proclaim '(inline class-name-event-translations))
(defun class-name-event-translations (class-name)
  (get class-name 'event-translations))

(defsetf class-name-event-translations (class-name) (translations)
  `(setf (get ,class-name 'event-translations) ,translations))


(defsetf class-name-event-mask (class-name) (mask)
  `(setf (get ,class-name 'event-mask) ,mask))

(defun class-name-event-mask (class-name)
  "Return the event mask shared by all instances of CLASS-NAME."
  (or
    (get class-name 'event-mask)
    
    (setf
      (class-name-event-mask class-name)

      (labels
	((class-event-translations-mask
	   (classes mask)
	   ;; Return the bitwise-or of the initial MASK and
	   ;; the event mask specified by the class event translations of
	   ;; all the CLASSES.
	   (if classes
	       
	       (class-event-translations-mask
		 (rest classes)
		 (event-translations-mask
		   (class-name-event-translations (first classes))
		   mask))
	       
	       mask)))

	(class-event-translations-mask
	  (class-name-event-precedence-list class-name)

	  ;; Initial event mask is most specific class resource initform.	  
	  (dolist (class (class-name-precedence-list class-name) 0)
	    (let ((init-mask (getf (rest (assoc :event-mask (clue-resources class))) :initform)))
	      (when init-mask
		(return init-mask)))))))))

