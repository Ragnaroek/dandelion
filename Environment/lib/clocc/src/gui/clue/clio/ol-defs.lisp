;;; -*- Mode:Lisp; Package:CLIO-OPEN; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
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


(defstruct (scrollbar (:type vector))
  "Scrollbar geometry descriptor."
  anchor-height
  anchor-width
  arrow-height   
  arrow-width
  cable-margin
  cable-width
  margin)

(defstruct (text-caret (:type vector))
  "Text-caret geometry descriptor." 
  height
  width
  inactive-height				; Non-nil if not same as height.
  baseline-offset)


;; Add in 1 for a margin.
(defconstant *slider-default-margin* 1
  "The MINIMUM margin gap for all 4 sides of a SLIDER. See also SLIDER-MARGIN .")

(defstruct (slidebar (:type vector) (:conc-name :slidebar-))
  "Slider geometry descriptor. Referenced to origin of top,left corner, 
not including the label or typein field, of :horizontal slider."

  ;; Numbers in parentheses refer to dimensions tables in rel 1.0 OPEN LOOK spec

  drag-box-width   	;; (b) this is the BLT width for ANY orientation
  gap			;; (c),(n),(p) also 1/2 of tick-mark thickness
  bar-thickness		;; (f)
  bar-drag-offset	;; (i) distance between top of drag box and top of bar
  tick-mark-length	;; (a3) is also same as end-box-width
  tick-mark-offset	;; (d2) distance from bar-end to tick-mark centerline

  ;;  bar-text-offset : (x,y) First # is x-offset, second # is y-offset,
  ;;  for :horizontal slider = (0,a3), for :vertical slider = (a5,a4)
  ;;  (b5) is the same as GAP
  bar-text-offset
  )


;================================================================;
;		   OPEN LOOK MENU & PUSHPIN SPECIFICATIONS       ;
;================================================================;

(DEFSTRUCT OL-menu-spec
  scale
  pushpin        ;Pushpin Specification for this scale
  pushpin-dx     ;horizontal distance from border to left of image
  pushpin-dy     ;vertical distance from border to pushpin baseline
  title-bar-dx   ;horizontal distance from border to title bar (left & right)
  title-bar-dy   ;vertical distance from title baseline to title bar
  title-dx       ;horizontal (minimum) left & right title margins
  title-dy       ;vertical distance from menu border to title baseline
  drop-shadow-width
  drop-shadow-offset)
    
  
(defstruct pushpin-spec
  scale
  box-width              ;actual bitmap width
  box-height             ;actual bitmap height
  baseline               ;image baseline (relative image top)
  image-in               ;bitmap image of pin in "in" state
  image-out              ;bitmap image of pin in "out" state
  default-ring-image     ;bitmap image of pin in "out & highlighted" state

  ;; The following (ring-x,ring-y) was intended to specify image relative origin for
  ;; drawing (superimposing) a ring over the image-out bitmap.  If we implement 
  ;; default ring by storing a complete default-ring-image then this pair won't be
  ;; needed.
  ring-x                 
  ring-y                 

  left-margin            ;# pixel columns for left padding            
  top-margin             ;# pixel columns for top padding
  bottom-margin)         ;# pixel columns for bottom padding


(defstruct more-text-arrow
  image		       ; Image of arrow.
  name		       ; Symbol to use when calling contact-mask to
		       ;    create a pixmap from this image.
  offset-from-baseline ; Top edge of arrow goes here relative to the
		       ;    baseline of the truncated text.
  offset-from-text)    ; Left edge of arrow usually goes here relative
						;    to the end of the truncated text.



;;;  Dialog spacing specifications ;;;;


(defconstant *dialog-point-spacing* '(:small 8 :medium 9 :large 10 :extra-large 14))