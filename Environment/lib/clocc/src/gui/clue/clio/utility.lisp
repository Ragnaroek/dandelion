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
	  contact-current-background
	  contact-current-background-pixel

	  inch-pixels
	  millimeter-pixels
	  pixel-inches
	  pixel-millimeters
	  pixel-points
	  point-pixels
	  ))

(defun contact-current-background (contact)
  "Returns the current CONTACT background, searching upward through
   the contact hierarchy to resolve :parent-relative."
  (declare (type contact contact))
  
  (do ((contact contact (contact-ancestor contact))
       (bg (contact-background contact) (contact-background contact)))
      ((not (eq bg :parent-relative))
       bg)))

(defmethod contact-ancestor ((contact contact))
   (with-slots (parent) contact
     parent))

(defmethod contact-ancestor ((shell shell))
   (shell-owner shell))

(defun contact-current-background-pixel (contact &optional (default-pixel :white))
  "Returns the current CONTACT background pixel, searching upward through
the contact hierarchy to resolve :parent-relative.  If the search returns
a non-pixel value, then the (converted) value of DEFAULT-PIXEL is returned."
  (let ((bg (contact-current-background contact)))
    (if (integerp bg) bg (convert contact default-pixel 'pixel))))
    


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 				 Unit Conversion                               |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defconstant *points-per-mm* (/ 72.27 25.4)
  "The number of points per millimeter.")

(defconstant *inches-per-mm* (/ 1.0 25.4)
  "The number of inches per millimeter.")

(defun pixel-points (screen &optional (number 1) (dimension :vertical))
  "Return the number of points represented by NUMBER pixels, in either
   the :vertical or :horzontal DIMENSION of the SCREEN."
  (declare (type screen screen)
	   (type number number)
	   (type (member :horizontal :vertical) dimension))
  (* number (pixel-millimeters screen 1 dimension) *points-per-mm*))


(defun point-pixels (screen &optional (number 1) (dimension :vertical))
  "Return the number of pixels represented by NUMBER points, in either
   the :vertical or :horzontal DIMENSION of the SCREEN."
  (declare (type screen screen)
	   (type number number)
	   (type (member :horizontal :vertical) dimension))
  (round (/ number (pixel-millimeters screen 1 dimension) *points-per-mm*)))


(defun pixel-inches (screen &optional (number 1) (dimension :vertical))
  "Return the number of inches represented by NUMBER pixels, in either
   the :vertical or :horzontal DIMENSION of the SCREEN."
  (declare (type screen screen)
	   (type number number)
	   (type (member :horizontal :vertical) dimension))
  (* number (pixel-millimeters screen 1 dimension) *inches-per-mm*))


(defun inch-pixels (screen &optional (number 1) (dimension :vertical))
  "Return the number of pixels represented by NUMBER inches, in either
   the :vertical or :horzontal DIMENSION of the SCREEN."
  (declare (type screen screen)
	   (type number number)
	   (type (member :horizontal :vertical) dimension))
  (round (/ number (pixel-millimeters screen 1 dimension) *inches-per-mm*)))

(defun pixel-millimeters (screen &optional (number 1) (dimension :vertical))
  "Return the number of millimeters represented by NUMBER pixels, in either
   the :vertical or :horzontal DIMENSION of the SCREEN."
  (declare (type screen screen)
	   (type number number)
	   (type (member :horizontal :vertical) dimension))
  (* number
     (/ (ecase dimension
	  (:vertical   (screen-height-in-millimeters screen))
	  (:horizontal (screen-width-in-millimeters screen)))
	(ecase dimension
	  (:vertical   (screen-height screen))
	  (:horizontal (screen-width screen))))))


(defun millimeter-pixels (screen &optional (number 1) (dimension :vertical))
  "Return the number of pixels represented by NUMBER millimeters, in either
   the :vertical or :horzontal DIMENSION of the SCREEN."
  (declare (type screen screen)
	   (type number number)
	   (type (member :horizontal :vertical) dimension))
  (round (/ number (pixel-millimeters screen 1 dimension))))


;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;; 				 Font Utilities                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(defmethod find-font (contact fontname)
  "Return an open font for the CONTACT. The FONTNAME represents a R3 fontname string
   specifying the requested font properties. Nil is returned if no such font can be
   opened."
  (declare (type stringable fontname))

  ;; Default method does no font negotiation
  (open-font (contact-display contact) fontname))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               Miscellaneous                                |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun area-overlaps-p (x1 y1 width1 height1 x2 y2 width2 height2)
  "Returns nil if the given rectangular areas do not intersect. Otherwise,
   the return values are the x, y, width, and height of the intersection area."
  (let (right1 bottom1 right2 bottom2)
    (when
      (and (< x2 (setf right1 (+ x1 width1)))
	   (< y2 (setf bottom1 (+ y1 height1)))
	   (> (setf right2 (+ x2 width2)) x1)
	   (> (setf bottom2 (+ y2 height2)) y1))

      (let ((x (max x1 x2)) (y (max y1 y2)))
	(values x y (- (min right1 right2) x) (- (min bottom1 bottom2) y))))))


(defun stringable-keyword (stringable)
  "Converts a stringable to a keyword symbol"
  (intern (nsubstitute #\- #\space (string-upcase stringable)) "KEYWORD"))

(defun stringable-label (stringable)
  "Convert a stringable into a string suitable for a label."
  (nsubstitute
    #\space #\-
    (if (symbolp stringable)
	;; Capitalize upper-case symbol name
	(string-capitalize (symbol-name stringable))
	;; Else assume string capitalization is already handled.
	(copy-seq stringable))))


(defmacro pixel-round (length &optional divisor)
  `(floor (+ 1/2 ,(if divisor `(/ ,length ,divisor) length))))



