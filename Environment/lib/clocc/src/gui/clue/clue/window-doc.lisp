;;; -*- Mode:Lisp; Syntax: Common-lisp; Package:XLIB; Base:10 -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;; Created 3/03/88 by LGO

(in-package "XLIB")

(export '(window-documentation
	   change-window-documentation))

(defun change-window-documentation (window string &key (mode :replace) format
				    state select font translate)
   ;; STRING will be displayed using FONT when the mouse is over WINDOW
   ;; and (zerop (logand SELECT (logxor STATE current-state)))
   ;; TRANSLATE defaults to #'xlib:translate-default
   ;; FORMAT defaults to STRING when no keywords are specified,
   ;; otherwise it defaults to 8.
   (declare (type window window)
	    (type string string)
	    (type (member :replace :prepend :append) mode)
	    (type (member 8 16 string) format)
	    (type modifier-mask state select)
	    (type (or null font) font)
	    (type (or null translation-function) translate))
  (unless format
    (setq format (if (and (eq mode :replace) (zerop state) (zerop select)
			  (null font) (null translate))
		     :string 8)))
  (ecase format
    (:string
     (change-property window :wm_documentation (string string) :string 8 :transform #'char->card8))
    (8 (change-window-documentation8 window string mode state select font translate))
    (16 (change-window-documentation16 window string mode state select font translate))))

(defun change-window-documentation8 (window string mode state select font translate)
   (declare (type window window)
	    (type string string)
	    (type (member :replace :prepend :append) mode)
	    (type modifier-mask state select)
	    (type (or null font) font)
	    (type (or null translation-function) translate))
  (let* ((display (window-display window))
	 (src-start 0)
	 (src-end (length string))
	 (length (- src-end src-start))
	 (property (intern-atom display :wm_documentation))
	 (type property)
	 (state (encode-state-mask state))
	 (select (encode-state-mask (or select state))))
    (with-buffer-request (display *x-changeproperty* :length length)
      ((data (member :replace :prepend :append)) mode)
      (window window)
      (resource-id property type)
      (card8 8)
      (card32 length)
      (card8 0) ;; Flag to indicate state/select pair
      (card8 (ldb (byte 8 8) state) (ldb (byte 8 0) state))
      (card8 (ldb (byte 8 8) select) (ldb (byte 8 0) select))
      (progn
	(do* ((boffset (index+ buffer-boffset 29))
	      (src-chunk 0)
	      (dst-chunk 0)
	      (offset 0)
	      (stop-p nil))
	     ((or stop-p (zerop length))
	      (card32-put 20 (index- boffset buffer-boffset 24))   ;; Set property length
	      (length-put 2 (index-ash (index- (lround boffset) buffer-boffset) -2)) ;; Set request length
	      (setf (buffer-boffset display) (lround boffset)))

	  (declare (type array-index src-chunk dst-chunk offset)
		   (type boolean stop-p))
	  (setq src-chunk (index-min length *max-string-size*))
	  (multiple-value-bind (new-start new-font)
	      (funcall (or translate #'translate-default)
		       string src-start (index+ src-start src-chunk)
		       font buffer-bbuf (index+ boffset 2))
	    (setq dst-chunk (index- new-start src-start)
		  length (index- length dst-chunk)
		  src-start new-start)
	    (when (index-plusp dst-chunk)
	      (setf (aref buffer-bbuf boffset) dst-chunk)
	      (setf (aref buffer-bbuf (index+ boffset 1)) offset)
	      (incf boffset (index+ dst-chunk 2)))
	    (setq offset 0)
	    (cond ((null new-font)
		   ;; Don't stop if translate copied whole chunk
		   (unless (index= src-chunk dst-chunk)
		     (setq stop-p t)))
		  ((integerp new-font) (setq offset new-font))
		  ((type? new-font 'font)
		   (setq font new-font)
		   (let ((font-id (font-id font))
			 (buffer-boffset boffset))
		     (declare (type resource-id font-id)
			      (type array-index buffer-boffset))
		     (card8-put 0 #xff)
		     (card8-put 1 (ldb (byte 8 24) font-id))
		     (card8-put 2 (ldb (byte 8 16) font-id))
		     (card8-put 3 (ldb (byte 8 8) font-id))
		     (card8-put 4 (ldb (byte 8 0) font-id)))
		   (index-incf boffset 5)))
	    ))))))

;;;-----------------------------------------------------------------------------

(defun window-documentation (window)
  (xlib:get-property window :wm_documentation :type :string
		     :result-type 'string :transform #'xlib::card8->char))

(defsetf window-documentation (window &optional (format 8)) (doc)
  ;; DOC is a string or list with the following elements:
  ;; :STATE xlib:modifier-mask  - Strings following will use this state with zero select
  ;; :SELECT  xlib:modifier-mask  - Strings following will use this select
  ;; :FONT  (or font stringable)- Use this font for all following strings
  ;; :translate xlib:translation-function  - Use this translation function for all following strings
  ;; string                     - String to use with current state and select
  ;; Example: (:state (:button-1) :select (:button-1) "Foo" :state (:button-2) "Bar")
  ;; This will cause "Foo" to be displayed when button-1 and any other modifier is down in window.
  ;; "Bar" will be displayed when button-2 and ONLY button-2 is down in window.
  `(xlib::set-window-documentation ,window ,doc ,format))

(defun set-window-documentation (window doc format)
  (declare (type (or string list) doc)
	   (type (member 8 16) format))
  (if (stringp doc)
      (change-property window :wm_documentation (string doc) :string 8 :transform #'char->card8)
    (let ((mode :replace))
    (dolist (args doc)
      (apply 'change-window-documentation window (car args)
	     :format format :mode mode (cdr args))
      (setq mode :append))))
  doc)

;; Implement this someday...
(defun change-window-documentation16 (window string mode state select font translate)
  (declare (ignore window string mode state select font translate))
  (error "change-window-documentation16 not implemented yet"))