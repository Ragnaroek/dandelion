;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Patch-file:T -*-

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



(in-package "XLIB")

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;    Add an optional :WINDOW keyword parameter to create-window              |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defun create-window (&key
		      window
		      (parent (required-arg parent))
		      (x (required-arg x))
		      (y (required-arg y))
		      (width (required-arg width))
		      (height (required-arg height))
		      (depth 0) (border-width 0)
		      (class :copy) (visual :copy)
		      background border
		      bit-gravity gravity
		      backing-store backing-planes backing-pixel save-under
		      event-mask do-not-propagate-mask override-redirect
		      colormap cursor)
  ;; Display is obtained from parent.  Only non-nil attributes are passed on in
  ;; the request: the function makes no assumptions about what the actual protocol
  ;; defaults are.  Width and height are the inside size, excluding border.
  (declare (type window parent) ; required
	   (type (or null window) window)
	   (type int16 x y) ;required
	   (type card16 width height) ;required
	   (type card16 depth border-width)
	   (type (member :copy :input-output :input-only) class)
	   (type (or (member :copy) visual-info resource-id) visual)
	   (type (or null (member :none :parent-relative) pixel pixmap) background)
	   (type (or null (member :copy) pixel pixmap) border)
	   (type (or null bit-gravity) bit-gravity)
	   (type (or null win-gravity) gravity)
	   (type (or null (member :not-useful :when-mapped :always)) backing-store)
	   (type (or null pixel) backing-planes backing-pixel)
	   (type (or null event-mask) event-mask)
	   (type (or null device-event-mask) do-not-propagate-mask)
	   (type (or null (member :on :off)) save-under override-redirect)
	   (type (or null (member :copy) colormap) colormap)
	   (type (or null (member :none) cursor) cursor))
  (let* ((display (window-display parent))
	 (window  (or window (make-window :display display)))
	 (wid (allocate-resource-id display window 'window))
	 back-pixmap back-pixel
	 border-pixmap border-pixel)
    (declare (type display display)
	     (type window window)
	     (type resource-id wid)
	     (type (or null resource-id) back-pixmap border-pixmap)
	     (type (or null pixel) back-pixel border-pixel))
    (setf (window-id window) wid)
    (case background
      ((nil) nil)
      (:none (setq back-pixmap 0))
      (:parent-relative (setq back-pixmap 1))
      (otherwise
       (if (type? background 'pixmap)
	   (setq back-pixmap (pixmap-id background))
	 (if (integerp background)
           (setq back-pixel background)
           (error 'type-error :datum background
                  :expected-type '(or null (member :none :parent-relative)
                                   integer pixmap))))))
    (case border
      ((nil) nil)
      (:copy (setq border-pixmap 0))
      (otherwise
       (if (type? border 'pixmap)
         (setq border-pixmap (pixmap-id border))
	 (if (integerp border)
	   (setq border-pixel border)
           (error 'type-error :datum border
                  :expected-type '(or null (member :copy)
                                   integer pixmap))))))
    (when event-mask
      (setq event-mask (encode-event-mask event-mask)))
    (when do-not-propagate-mask
      (setq do-not-propagate-mask (encode-device-event-mask do-not-propagate-mask)))

						;Make the request
    (with-buffer-request (display *x-createwindow*)
      (data depth)
      (resource-id wid)
      (window parent)
      (int16 x y)
      (card16 width height border-width)
      ((member16 :copy :input-output :input-only) class)
      (resource-id (cond ((eq visual :copy)
			  0)
			 ((typep visual 'resource-id)
			  visual)
			 (t
			  (visual-info-id visual))))
      (mask (card32 back-pixmap back-pixel border-pixmap border-pixel)
	    ((member-vector *bit-gravity-vector*) bit-gravity)
	    ((member-vector *win-gravity-vector*) gravity)
	    ((member :not-useful :when-mapped :always) backing-store)
	    (card32  backing-planes backing-pixel)
	    ((member :off :on) override-redirect save-under)
	    (card32 event-mask do-not-propagate-mask)
	    ((or (member :copy) colormap) colormap)
	    ((or (member :none) cursor) cursor)))
    window))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;  Define stubs for wm-hints/wm-size-hints accessors needed for full ICCCM   |
;;;  support. Unnecessary with R4 CLX but needed when using R3 CLX.            |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(unless (find-symbol "WM-SIZE-HINTS-BASE-HEIGHT" 'xlib)
  (defun wm-size-hints-base-height (hints)
    (declare (ignore hints))
    nil)
  (export 'wm-size-hints-base-height 'xlib))

(unless (find-symbol "WM-SIZE-HINTS-BASE-HEIGHT" 'xlib)
  (defsetf wm-size-hints-base-height (hints) (value)
    (declare (ignore hints))
    `,value)
  (export 'wm-size-hints-base-height 'xlib))

(unless (find-symbol "WM-SIZE-HINTS-BASE-WIDTH" 'xlib)
  (defun wm-size-hints-base-width (hints)
    (declare (ignore hints))
    nil)
  (export 'wm-size-hints-base-width 'xlib))

(unless (find-symbol "WM-SIZE-HINTS-BASE-WIDTH" 'xlib)
  (defsetf wm-size-hints-base-width (hints) (value)
    (declare (ignore hints))
    `,value)
  (export 'wm-size-hints-base-width 'xlib))

(unless (find-symbol "WM-SIZE-HINTS-GRAVITY" 'xlib)
  (defun wm-size-hints-gravity (hints)
    (declare (ignore hints))
    nil)
  (export 'wm-size-hints-gravity 'xlib))

(unless (find-symbol "WM-SIZE-HINTS-GRAVITY" 'xlib)
  (defsetf wm-size-hints-gravity (hints) (value)
    (declare (ignore hints))
    `,value)
  (export 'wm-size-hints-gravity 'xlib))
