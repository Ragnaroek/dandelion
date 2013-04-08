;;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
;;;                                                                                  |
;;;              Copyright (C) 1989, 1990 Texas Instruments Incorporated.            |
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
	  command-frame
	  command-frame-content
	  command-frame-controls
	  make-command-frame
	  )
	'clio-open)


(defcontact command-frame (core core-shell top-level-session)
  ()
  (:documentation "A  top-level-session containing a content and a set of controls.")
  (:resources
    (content  :type (or function list) :initform nil)
    (controls :type (or function list) :initform nil)))


(defmethod initialize-instance :after ((command-frame command-frame)
				       &rest initargs &key content controls)
  (with-slots (width height) command-frame
    
    ;; Initialize command-frame-form
    (assert content () "No content defined for ~a." command-frame)
    (multiple-value-bind (content-constructor content-initargs)
	(etypecase content
	  (function content)
	  (list (values (first content) (rest content))))
	
      (let*
	((content-name     (or (getf content-initargs :name) :content))
	 (hlinks           `((
			      :from        :command-frame-form
			      :to          ,content-name
			      :attach-from :left
			      :attach-to   :left
			      :maximum     0)
			     (
			      :from        ,content-name
			      :to          :command-frame-form
			      :attach-from :right
			      :attach-to   :right
			      :maximum     0)
			     (
			      :from        :command-frame-form
			      :to          :controls
			      :attach-from :left
			      :attach-to   :left
			      :maximum     0)
			     (
			      :from        :controls
			      :to          :command-frame-form
			      :attach-from :right
			      :attach-to   :right
			      :maximum     0)))
	 (vlinks           `((
			      :from        :command-frame-form
			      :to          :controls
			      :attach-from :top
			      :attach-to   :top
			      :maximum     0)
			     (
			      :from        :controls
			      :to          ,content-name
			      :maximum     0)
			     (
			      :from        ,content-name
			      :to          :command-frame-form
			      :attach-from :bottom
			      :attach-to   :bottom
			      :maximum     0)
			     ))
	 (form             (make-form
			     :name             :command-frame-form
			     :parent           command-frame
			     :width            width
			     :height           height
			     :horizontal-links hlinks
			     :vertical-links   vlinks)))
	
	;; Initialize content 
	(apply content-constructor
	       :name       content-name
	       :parent     form
	       :max-height :infinite
	       :min-height 0
	       :max-width  :infinite
	       :min-width  0	     		 
	       content-initargs)
	
	;; Initialize controls area
	(multiple-value-bind (controls-constructor controls-initargs)
	    (etypecase controls
	      (null
	       (let ((space (point-pixels
			      (contact-screen command-frame)
			      (getf *dialog-point-spacing* (contact-scale command-frame)))))
		 (values 'make-table 
			 `(
			   :columns              :maximum
			   :column-alignment     :center
			   :same-height-in-row   :on
			   :horizontal-space     ,space
			   :left-margin          ,space
			   :right-margin         ,space
			   :top-margin           ,(pixel-round space 2)
			   :bottom-margin        ,(pixel-round space 2)))))
	      
	      (function controls)
	      
	      (list (values (first controls) (rest controls))))
	  
	  (apply controls-constructor 
		 :parent       form
		 :name         :controls
		 :border-width 0
		 :max-width    :infinite
		 :min-width    0
		 controls-initargs))))))


(defun command-frame-form (command-frame)
  (first (slot-value command-frame 'children)))

(defmethod command-frame-content ((command-frame command-frame))
  (first (slot-value (command-frame-form command-frame) 'children)))

(defmethod command-frame-controls ((command-frame command-frame))
  (second (slot-value (command-frame-form command-frame) 'children)))

(defun make-command-frame (&rest initargs)
  (apply #'make-contact 'command-frame initargs))

(defmethod rescale :before ((command-frame command-frame))
  (let ((controls (command-frame-controls command-frame)))
    (multiple-value-bind (pw ph) (preferred-size controls)
      (declare (ignore pw))
      (setf (form-max-height controls) (setf (form-min-height controls) ph)))))