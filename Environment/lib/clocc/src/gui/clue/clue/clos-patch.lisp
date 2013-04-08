;;; -*- Mode:Lisp; Package:XLIB; Base:10; Lowercase:YES; Patch-file:T; Syntax:Common-Lisp -*-

;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                  P.O. BOX 149149                                 |
;;;                             AUSTIN, TEXAS 78714-9149                             |
;;;                                                                                  |
;;;                Copyright (C)1989,1990 Texas Instruments Incorporated.            |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

(in-package "XLIB")


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;; Redefine drawable, pixmap, and window to CLOS classes.                           |
;;;                                                                                  |
;;; This file was made obsolete by R4 CLX, which will define drawable, pixmap, and   |
;;; window as CLOS classes via the def-clx-class macro (see CLX/depdefs.l).          |
;;;                                                                                  |
;;; If using R3 CLX, then this file should be loaded when compiling CLX.             |
;;; Compile this file after clx.l has been loaded and load this file                 |
;;; after dependent.l has been loaded and before macros.l has been loaded.           |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+


#+comment ;; This should work, but doesn't on Franz Lisp...
(eval-when (compile load eval)
  (cond ((find-package 'clos)
	 (in-package "XLIB" :use '("CLOS")))

	((find-package 'pcl)
	 (in-package "XLIB" :use '("PCL")))
	
	((find-package 'cluei) ;; must be using clos-kludge
	 (in-package "CLUEI"))
	
	(t
	 (error "Can't find a CLOS"))))

;; Set the package with this cruft instead
(eval-when (eval compile load)
  (when (find-package 'pcl)
    (pushnew :pcl  *features*)
    (pushnew :clos *features*)))

#+(and clos (not pcl))
(in-package "XLIB" :use '("CLOS"))

#+pcl
(in-package "XLIB" :use '("PCL"))

#-(or pcl clos)
(in-package "CLUEI")


;; Nuke defstruct info from drawable window and pixmap
(eval-when (compile load eval)
  (dolist (symbol '( drawable drawable-id drawable-display drawable-plist make-drawable drawable-p
		     window window-id window-display window-plist make-window window-p
		     pixmap pixmap-id pixmap-display pixmap-plist make-pixmap pixmap-p))
    (setf (symbol-plist symbol) nil)
    (fmakunbound symbol)))

;;
;; Drawables
;;

;; Allow change in metaclass (structure-class to standard-class)
(setf (find-class 'drawable nil) nil)

(defclass drawable ()
  ((id      :type     resource-id
	    :initform 0
	    :accessor drawable-id
	    :initarg  :id)
   
   (display :type     (or null display)
	    :initform nil
	    :accessor drawable-display
	    :initarg  :display)
   
   (plist   :type     list
	    :initform nil
	    :accessor drawable-plist
	    :initarg  :plist))		; Extension hook

  (:documentation "The class of CLX drawable objects."))


(defun make-drawable (&rest initargs)
  (apply #'make-instance 'drawable initargs))

(defun drawable-p (object)
  (typep object 'drawable))

;;
;; Windows
;;

;; Allow change in metaclass (structure-class to standard-class)
(setf (find-class 'window nil) nil)

(defclass window (drawable)
  ((id      :type     resource-id
	    :initform 0
	    :accessor window-id
	    :initarg  :id)
   
   (display :type     (or null display)
	    :initform nil
	    :accessor window-display
	    :initarg  :display)
   
   (plist   :type     list
	    :initform nil
	    :accessor window-plist
	    :initarg  :plist))		; Extension hook

  (:documentation "The class of CLX window objects."))


(defun make-window (&rest initargs)
  (apply #'make-instance 'window initargs))

(defun window-p (object)
  (typep object 'window))


;;
;; Pixmaps
;;

;; Allow change in metaclass (structure-class to standard-class)
(setf (find-class 'pixmap nil) nil)

(defclass pixmap (drawable)
  ((id      :type     resource-id
	    :initform 0
	    :accessor pixmap-id
	    :initarg  :id)
   
   (display :type     (or null display)
	    :initform nil
	    :accessor pixmap-display
	    :initarg  :display)
   
   (plist   :type     list
	    :initform nil
	    :accessor pixmap-plist
	    :initarg  :plist))		; Extension hook

  (:documentation "The class of CLX pixmap objects."))


(defun make-pixmap (&rest initargs)
  (apply #'make-instance 'pixmap initargs))

(defun pixmap-p (object)
  (typep object 'pixmap))
