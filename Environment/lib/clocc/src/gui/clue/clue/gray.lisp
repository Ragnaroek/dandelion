;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; Graying stipple patterns

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

;;; Some common grays
;;; These are used to draw gray stipple patterns onto the screen.
;;; Grays with an "R" on the end have regular patterns (grid-like)
;;; Other grays (without the "R") have pixels offset to avoid the grid look.

(in-package "CLUEI")

(export '(defimage 0%gray 6%grayr 6%gray 12%grayr 12%gray 25%grayr 25%gray 37%grayr 37%gray
	  33%gray 50%grayr 50%gray 66%gray 62%grayr 62%gray 75%grayr 75%grayh 75%gray
	  88%grayr 88%gray 93%grayr 93%gray 100%gray))

(defvar *bitmap-images* nil "List of defined bitmap images")

(defmacro defimage (name &optional plist &body patterns)
  (when (or (typep plist 'bit-vector)
	    (and (consp plist) (eq (car plist) 'quote) (typep (cadr plist) 'bit-vector)))
    (push plist patterns)
    (setq plist nil))
  `(progn
     (pushnew ',name *bitmap-images*)
     (defparameter ,name (bitmap-image '(:name ,name ,@plist)
				       ,@(mapcar #'eval patterns)))))

(defimage 100%gray
  '#*1)

(defimage 93%gray
  '#*01111111
  '#*11111111
  '#*11110111
  '#*11111111)

(defimage 93%grayr
  '#*0111
  '#*1111
  '#*1111
  '#*1111)

(defimage 88%gray
  '#*0111
  '#*1111
  '#*1101
  '#*1111)

(defimage 88%grayr
  '#*0111
  '#*1111
  '#*0111
  '#*1111)

(defimage 75%gray
  '#*0111
  '#*1101
  '#*0111
  '#*1101)

(defimage 75%grayh
  '#*0111
  '#*1101
  '#*1011
  '#*1110)

(defimage 75%grayr
  '#*0101
  '#*1111
  '#*0101
  '#*1111)

(defimage 62%gray
  '#*0111
  '#*1010
  '#*1101
  '#*1010)

(defimage 62%grayr
  '#*0111
  '#*1010
  '#*0111
  '#*1010)

(defimage 66%gray
  '#*011
  '#*101
  '#*110) ;; Very SLOW

(defimage 50%gray
  '#*01
  '#*10)

(defimage 50%grayr
  '#*10
  '#*10)

;; The rest are just reversals of the above
(defimage 33%gray
  '#*100
  '#*010
  '#*001) ;; Very SLOW

(defimage 37%gray
  '#*1000
  '#*0101
  '#*0010
  '#*0101)

(defimage 37%grayr
  '#*1000
  '#*0101
  '#*1000
  '#*0101)

(defimage 25%gray
  '#*1000
  '#*0010
  '#*1000
  '#*0010)

(defimage 25%grayr
  '#*1010
  '#*0000
  '#*1010
  '#*0000)

(defimage 12%gray
  '#*1000
  '#*0000
  '#*0010
  '#*0000)

(defimage 12%grayr
  '#*1000
  '#*0000
  '#*1000
  '#*0000)

(defimage 6%gray
  '#*10000000
  '#*00000000
  '#*00001000
  '#*00000000)

(defimage 6%grayr
  '#*1000
  '#*0000
  '#*0000
  '#*0000)

(defimage 0%gray
  '#*0)

