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


(defmethod handle-event ((view view) (event event))
  "Do event translation based on the graphic event translations
 or the view event-translations slot."
  (declare (type event event))
  (IF  (eq :exposure (slot-value (the event event) 'key))
       (IF (slot-boundp  view 'x)
	   (with-slots (x y width height) (the event event)
	     (display view x y width height))
	   (display view))
       
       ;; handle events
       (IF (NOT  (GETF (slot-value view 'plist) :ignore-graphic-events))
	   (WHEN (slot-boundp  event 'x)
	     (with-slots (x y) event
	       (UNLESS
		   (AND
		    x y ; is x,y a valid coordinate from the event
		    (MULTIPLE-VALUE-BIND
			(world-x world-y)
			(view-untransform-point view x y)
		      (or  (graphic-action-p ; yes, is x,y in a grabber box?
			    (graphic-pick
			     (view-selection-scene view) ; yes, translate the event
			     world-x world-y
			     (view-pixel-size view))		      
			    event view)
			   (graphic-action-p ; no, is x,y on a graphic?
			    (graphic-pick
			     (view-graphic view)
			     world-x world-y
			     (view-pixel-size view)) ;yes, is there an event translation? 
			    event view)))) ; yes, perform the event translation
		 (call-next-method))))
	   (call-next-method)))
  t)
 
(defun call-action-internal (contact action)
  (if (consp action)
      (apply (car action) contact (cdr action))
      (funcall action contact)))


(DEFUN   graphic-action-p (graphic event view)
  "Return true if an action was executed for the graphic"
  (WHEN graphic
    ;; Have a all of the graphic-parents been checked for actions
    ;;   or no graphic choosen?
    (LET ((actions (translate-graphic-event graphic event))) ;  no, get the actions for the event from the graphic
      (WHEN actions ;     are there any actions?
	(dolist (action actions t) ;       yes,  execute the actions
	  (graphic-call-action graphic view action) 
	  (graphic-action-p (graphic-parent graphic) event view) ; no, look to the graphic-parent
	  )))
    ) ;  yes, end the recursion and return nil
  )

(defun graphic-call-action (graphic contact action)
  (if (consp action)
      (apply (car action) graphic contact (cdr action))
      (funcall action graphic contact)))

(defgeneric graphic-pick (graphic x y &optional arpeture)
  (:documentation
   "Return the graphic if the world coordinate x,y is on the graphic
or return nil"))

(DEFMETHOD graphic-pick ((graphic graphic) x y &optional (pixel 1))

  (AND
   (NOT (EQ (graphic-sensitivity graphic) :viewable)) ; is the graphic not viewable or hidden?  
    (NOT (EQ (graphic-sensitivity graphic) :hidden))
    (graphic-contains-p graphic x y pixel) ; and the x,y coordinate on the graphic
    graphic));   yes, return the graphic


(DEFMETHOD graphic-pick ((scene scene) x y &optional (pixel 1))
  (UNLESS (OR (EQ (graphic-sensitivity scene) :viewable) ;is the scene  viewable or hidden?  
	      (EQ (graphic-sensitivity scene) :hidden)
	      (NOT (graphic-contains-p scene x y pixel))         )
    (LET* ((elements (scene-elements scene))
	   (element-length (FILL-POINTER elements)))            
      (WHEN  (> element-length 0) ;  no, is the scene empty?
	(DO* ((position (1- element-length)(1- position)) )	
	     ((<  position 0) ) ;no graphic in the scene is selected so return nil
	  (LET ((graphic
		 (graphic-pick
		  (ELT  elements position)  x y
		  pixel))) ;    no,  pick a graphic
	    (WHEN  graphic ;      is there a picked graphic?
	      (WHEN
		  ;;        yes, is the scene  selectable or editable?  
		  (OR (EQ (graphic-sensitivity scene) :selectable)
		      (EQ (graphic-sensitivity scene) :editable))
		(RETURN scene))		      ;          yes, return the scene
	      (RETURN graphic))	      ;          no,  return the graphic
	    ))))))

(DEFMETHOD graphic-pick ((scene grabber-rect) x y &optional (pixel 1))
  ;is the scene  viewable or hidden?  
  (UNLESS (OR (EQ (graphic-sensitivity scene) :viewable)
	      (EQ (graphic-sensitivity scene) :hidden)
	               )
    (LET* ((elements (scene-elements scene))
	  (element-length (FILL-POINTER elements)))            
      (WHEN  (> element-length 0) ;  no, is the scene empty?
	(DO* ((position (1- element-length)(1- position)) )	
	     ((<  position 0) ) ;no graphic in the scene is selected so return nil
	  (LET ((graphic 
		 (graphic-pick
		  (ELT  elements position)  x y
		  pixel))) ;    no,  pick a graphic
	    (WHEN  graphic ;      is there a picked graphic?
	      (WHEN
		  ;        yes, is the scene  selectable or editable?  
		  (OR (EQ (graphic-sensitivity scene) :selectable)
		      (EQ (graphic-sensitivity scene) :editable))
		(RETURN scene)) ;          yes, return the scene
	      (RETURN graphic)) ;          no,  return the graphic
		))))))

(DEFMETHOD graphic-pick ((scene selection-scene) x y &optional (pixel 1))
  ;is the scene  viewable or hidden?  
  (UNLESS (OR (EQ (graphic-sensitivity scene) :viewable)
	      (EQ (graphic-sensitivity scene) :hidden)
	      )
    (LET* ((elements (scene-elements scene))
	  (element-length (FILL-POINTER elements)))            
      (WHEN  (> element-length 0) ;  no, is the scene empty?
	(DO* ((position (1- element-length)(1- position)) )	
	     ((<  position 0) ) ;no graphic in the scene is selected so return nil
	  (LET ((graphic (graphic-pick
			  (ELT  elements position)  x y
			  pixel))) ;    no,  pick a graphic
		(WHEN  graphic ;      is there a picked graphic?
		  (WHEN
		      ;        yes, is the scene  selectable or editable?  
		      (OR (EQ (graphic-sensitivity scene) :selectable)
			  (EQ (graphic-sensitivity scene) :editable))
		    (RETURN scene)) ;          yes, return the scene
		  (RETURN graphic)) ;          no,  return the graphic
		))))))

(defgeneric graphic-within (graphic x y width height )
  (:documentation "Return the graphic if it is within the rectangle defined by X,
 Y, Width and Height or return nil.
   If a scene is subselectable a list of selectable graphics in the scene
 is returned."))

(DEFMETHOD graphic-within ((graphic graphic) x y width height)

  (AND
   ; is the graphic not viewable or hidden?  
    (NOT (EQ (graphic-sensitivity graphic) :viewable))
    (NOT (EQ (graphic-sensitivity graphic) :hidden))
    ; and the graphic is within the rectangle
    (graphic-within-p graphic x y width height)
    graphic)) ; return the graphic


(DEFMETHOD graphic-within ((scene scene) x y width height)
  ; is the scene  viewable or hidden?  
  (UNLESS (OR (EQ (graphic-sensitivity scene) :viewable)
	      (EQ (graphic-sensitivity scene) :hidden))         
    (LET* ((elements (scene-elements scene))
	   (elements-length (FILL-POINTER elements))
	   selected)            
      (WHEN  (> elements-length 0) ;   no, is the scene empty?
	(DO* ((position (1- elements-length)
			(1- position))) ;no, check for a picked graphic is the scene	
	     ((<  position 0) ) ;when no graphic in the scene is selected return nil
	  (LET ((graphic
		  (graphic-within
		    (ELT elements position)
		    x y width height))) ; pick a graphic
	    (WHEN  graphic ;    is there a picked graphic?
		  (IF (ATOM graphic) ;      yes, is it a atom
		      ;     yes, add it to the selected list
		      (SETF selected (CONS graphic selected ))
		      ;     no, add each graphic in the list to the selected list
		      (SETF selected
			    (APPEND graphic selected))
		      ))))
	(VALUES
	 (IF  (OR (EQ (graphic-sensitivity scene) :selectable)
		  (EQ (graphic-sensitivity scene) :editable))
	      (WHEN selected scene) ;    yes, return the scene
		   selected)))))) ;    no, return the graphic

