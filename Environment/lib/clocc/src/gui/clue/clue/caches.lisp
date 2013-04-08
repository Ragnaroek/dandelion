;;; -*- Mode:Lisp; Package:CLUEI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-


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

(in-package "CLUEI")

(export '(
	  contact-mask
	  contact-image-mask
	  contact-image-pixmap
	  contact-pixmap
	  display-mask
	  display-pixmap
	  using-gcontext
	  )
	'cluei)

;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                             Gcontexts                                      |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(defmethod contact-depth ((drawable drawable))
  (or (getf (drawable-plist drawable) 'depth)
      (setf (getf (drawable-plist drawable) 'depth)
	    (drawable-depth drawable))))

(defmethod contact-root ((drawable drawable))
  (or (getf (drawable-plist drawable) 'root)
      (setf (getf (drawable-plist drawable) 'root)
	    (drawable-root drawable))))


(let ((gcontext (xlib::make-gcontext)))

  (defun default-gcontext (drawable default)
    "Return a gcontext with the same attributes as DEFAULT which can be used
to search the gcontext cache for the DRAWABLE. The second return value 
is the cache to be searched."
    (declare
      (type drawable           drawable)
      (type (or null gcontext) default))
    (let*
      ((display  (drawable-display drawable)) 

       ;; Look up root gcontext cache for this depth.
       (cache    (rest
		   (or
		     ;; Cache already linked to drawable?
		     (getf (drawable-plist drawable) 'gcontext-cache)
		     
		     ;; Get cache from root and link to drawable
		     (setf
		       (getf (drawable-plist drawable) 'gcontext-cache)
		       (let ((root  (contact-root drawable))
			     (depth (contact-depth drawable)))
			 (or
			   ;; Cache for this already exists on root?
			   (assoc depth (getf (drawable-plist root) 'gcontexts))
			   
			   ;; Initialize cache for this depth.
			   (let ((entry (list 
					  depth
					  
					  ;; Also save default gcontext state vector here.
					  ;; Actually create/delete an unnecessary default
					  ;; gcontext to avoid knowledge of state vector
					  ;; representation.
					  (let ((default (create-gcontext :drawable drawable)))
					    (prog1
					      (copy-seq (xlib::gcontext-local-state default))
					      (free-gcontext default))))))
			     (push entry (getf (drawable-plist root) 'gcontexts))
			     entry))))))))

      ;; Return a gcontext (with given default state) and the cache to search.
      (setf (gcontext-display gcontext) display)
      (replace
	(xlib::gcontext-local-state gcontext)
	(if default (xlib::gcontext-local-state default) (first cache)))
      
      (values gcontext cache))))


(defmacro using-gcontext ((gcontext &rest options &key drawable default clip-ordering &allow-other-keys)
			  &body body)
  "Perform BODY with GCONTEXT bound to a gcontext containing the
specified OPTIONS, where OPTIONS is a list of any initargs
accepted by CREATE-GCONTEXT. For those values not specified 
by OPTIONS, GCONTEXT will match the DEFAULT gcontext."

  (assert drawable nil "Required DRAWABLE argument is missing or nil.")
  
  (setf options (copy-list options))
  (remf options :default)
  (remf options :drawable) 

  (let (values setfs (cache (gensym)) (dgc (gensym)))
    
    (do
      ((option options (cddr option)))      
      ((endp option))
      
      (let*
	((name         (first option))
	 (option-value (second option))
	 (function     `,(intern (concatenate 'string "GCONTEXT-" (symbol-name name)) :xlib)))

	(cond
	  ((eq name :clip-ordering))	        ; Skip if no accessor function
	  
	  ((constantp option-value)		; Optimize for constant value
	   (when (eval option-value)
	     (push `(setf (,function ,dgc ,@(when (eq name :clip-mask) `(,clip-ordering)))
			  ,option-value)
		   setfs)))
	  

	  
	  ((atom option-value)			; Optimize for variable value
	   (push `(when ,option-value
		    (setf (,function ,dgc ,@(when (eq name :clip-mask) `(,clip-ordering)))
			  ,option-value))
		 setfs))
	  
	  (t					; Don't evaluate value expression twice
	   (let ((value (gensym)))
	     (push `(,value ,option-value) values)
	     (setf (getf options name) value)
	     (push
	       `(when ,value
		  (setf (,function ,dgc ,@(when (eq name :clip-mask) `(,clip-ordering)))
			,value))
	       setfs)))))) 

    (if 
      ;; Simple form?
      (and default (null setfs))
      
      ;; Yes, just use default gcontext.
      `(let ((,gcontext ,default)) ,@body)
      
      ;; No, expand lookup.
      (let*
	((initargs (unless default options)) 
	 (lookup   `(multiple-value-bind (,dgc ,cache) (default-gcontext ,drawable ,default)
		      ,@setfs
		      (let ((,gcontext (find-gcontext ,drawable ,dgc ,cache ,@initargs)))
			,@body)))) 
	(if values
	    `(let (,@values)
	       ,lookup)
	    `,lookup)))))


(defconstant *gcontext-test-sequence*
	     (let ((state-indexes (append xlib::*gcontext-components* '(:clip :dash))))
	       (mapcar
	       #'(lambda (key) (position key state-indexes))
	       '(
		 :foreground
		 :fill-style
		 :background
		 :stipple
		 :function
		 :font
		 :clip
		 :clip-mask
		 :exposures
		 :subwindow-mode
		 :line-width
		 :ts-y
		 :ts-x
		 :tile
		 :line-style
		 :arc-mode
		 :plane-mask 
		 :join-style
		 :cap-style
		 :dash
		 :dashes
		 :dash-offset
		 :clip-y
		 :clip-x				 
		 :fill-rule
		 )))
  "A permutation of gcontext state indexes, in order of `most-likely-to-differ-first'. 
Use this order to test if two gcontext states are equal, in order to detect inequality
quickly.")

(defun find-gcontext (drawable match cache &rest initargs)
  "Return a gcontext for the DRAWABLE that matches the MATCH gcontext.
Return an existing element of the gcontext CACHE or create and 
return a new CACHE element. The INITARGS, if given, specify the
attributes used to create a matching gcontext."
  (let ((desired (xlib::gcontext-local-state match)))
    (declare (type xlib::gcontext-state desired))
    (declare (optimize speed (safety 0)))
    
    (or
      ;; Look up matching gcontext in cache.
      (do*
	((prev     cache         (rest prev))
	 (gcontext (second prev) (second prev)))
	
	((or
	   ;; End of cache?
	   (not gcontext)
 
	   (and
	     ;; Next gcontext matches?...
	     (let ((test-state (xlib::gcontext-local-state gcontext)))
	       (declare (type xlib::gcontext-state test-state))
	       (dolist (i *gcontext-test-sequence* t)
		 (unless (equalp  (svref test-state i) (svref desired i))
		   (return nil))))

	     ;; ...and matching gcontext is at head of cache?
	     (or (eq prev cache)

		 ;; No, promote to head of cache.
		 (progn
		   (setf (cdr prev) (cddr prev))
		   (push gcontext (rest cache))))))

	 ;; Return matching gcontext (or nil).
	 gcontext))
    
      
      
      ;; Create new gcontext.
      (let ((gcontext (apply #'create-gcontext :drawable drawable initargs)))
	
	;; If initargs are given, then they specify the desired attributes
	;; for the new gcontext. Otherwise, update local state in anticipation
	;; of next force-gcontext-changes.
	(unless initargs
	  (let ((local-state (xlib::gcontext-local-state gcontext)))
	    (declare (type xlib::gcontext-state local-state))
	    (replace local-state desired)
	    (setf (xlib::gcontext-internal-timestamp local-state) -1)))
	
	;; Add new gcontext to head of cache.
	(push gcontext (rest cache))
	gcontext))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                              Pixmaps                                       |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(proclaim '(inline display-pixmap))
(defun display-pixmap (display depth name)
  "Return and (with setf) change the pixmap associated with the given
   DISPLAY, DEPTH, and NAME."
  (getf (rest (assoc depth (getf (display-plist display) :pixmaps))) name))

(defsetf display-pixmap setf-display-pixmap)
(defun setf-display-pixmap (display depth name pixmap)
  (let* ((depth-pixmaps (assoc depth (getf (display-plist display) :pixmaps)))
	 (depth-plist   (rest depth-pixmaps))
	 (new-pixmaps   (or depth-pixmaps (list depth nil))))
    (setf (getf depth-plist name) pixmap)
    (rplacd new-pixmaps depth-plist)
    (unless depth-pixmaps
      (push new-pixmaps (getf (display-plist display) :pixmaps)))
    pixmap))

(defmacro contact-pixmap (contact name)
  "Return and (with setf) change the pixmap associated with the given
   NAME for the display and depth given by CONTACT."
  `(display-pixmap (slot-value ,contact 'display)
		   (slot-value ,contact 'depth)
		   ,name))


(defun contact-image-pixmap (contact image)
  "Returns a pixmap for the given IMAGE. The image must have the same depth as
   the given CONTACT."
  (declare (type   image   image)
	   (type   contact contact))
  (or
    (contact-pixmap contact image)

    (if (= (image-depth image) (contact-depth contact))    
	(setf
	  (contact-pixmap contact image)	  
	  (image-pixmap contact image))

	(error "~a and ~a have different depths." contact image))))




;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;        Masks: two-color (foreground/background) pixmaps)                   |
;;;                                                                            |
;;;----------------------------------------------------------------------------+



(defun display-mask (display depth name foreground background)
  "Return and (with setf) change the mask associated with the given
   DEPTH, DISPLAY, NAME, and FOREGROUND/BACKGROUND pixels."
  
  ;; The display-pixmap cache element for a mask name is a list of entries
  ;; of the form (fg bg pixmap)
  (third (display-mask-entry display depth name foreground background)))

(defun display-mask-entry (display depth name foreground background)
  (let ((entries (getf (rest (assoc depth (getf (display-plist display) :masks))) name)))
    (values
      (find-if #'(lambda (entry)
		   (and (= (first entry)  foreground)
			(= (second entry) background)))
	       entries)
      entries)))

(defsetf display-mask setf-display-mask)

(defun setf-display-mask (display depth name foreground background pixmap)
  (multiple-value-bind (entry entries)
      (display-mask-entry display depth name foreground background)
    (if entry
	(setf (third entry) pixmap)

	(let* ((depth-masks (assoc depth (getf (display-plist display) :masks)))
	       (depth-plist (rest depth-masks))
	       (new-masks   (or depth-masks (list depth nil)))) 
	  (setf (getf depth-plist name)
		(push (list foreground background pixmap) entries))
	  (rplacd new-masks depth-plist)
	  (unless depth-masks
	    (push new-masks (getf (display-plist display) :masks)))))
    pixmap))


(defmacro contact-mask (contact name &key (foreground 1) (background 0) depth)
  "Return and (with setf) change the mask pixmap associated with the given
   NAME and FOREGROUND/BACKGROUND pixels for the display given by CONTACT.
   The pixmap returned has the given DEPTH (by default, the same depth as CONTACT)."
  (let ((depth (or depth `(slot-value ,contact 'depth))))
    `(display-mask
       (slot-value ,contact 'display)
       ,depth
       ,name
       ,foreground
       ,background)))


(defun contact-image-mask (contact image &key foreground background depth)
  "Returns a mask pixmap for the given bitmap IMAGE, in which each 0 and 1 bit is
   replaced by the BACKGROUND and FOREGROUND pixels, respectively. BACKGROUND and
   FOREGROUND pixels default to 0 and 1, respectively. The IMAGE must have depth 1."
  (declare (type   image             image)
	   (type   contact           contact)
	   (type   (or null pixel)   foreground background)
	   (type   (or null card32)  depth))
  
  (let ((depth (or depth (slot-value contact 'depth)))
	(fg    (or foreground 1))
	(bg    (or background 0)))
    
    (or
      (contact-mask
	contact image
	:foreground fg
	:background bg
	:depth      depth)
      
      (progn
	(assert (= 1 (image-depth image)) ()
		"Depth of image  ~a is not 1." image)
	(setf
	  (contact-mask
	    contact image
	    :foreground fg
	    :background bg
	    :depth      depth)
	  
	  (if (and (= depth 1) (not foreground) (not background))
	      
	      ;; Use 1-bit image data directly
	      (image-pixmap contact image)

	      ;; Use contact gcontext to specify colors --
	      ;; Mask depth must be supported by contact screen, else Match error will occur.
	      (using-gcontext
		(gcontext :drawable contact :foreground fg :background bg)
		(image-pixmap
		  contact image
		  :depth    depth
		  :gcontext gcontext))))))))



;;;----------------------------------------------------------------------------+
;;;                                                                            |
;;;                               Cursors                                      |
;;;                                                                            |
;;;----------------------------------------------------------------------------+

(proclaim '(inline display-cursor))
(defun display-cursor (display name)
  "Return and (with setf) change the cursor associated with the given NAME."
  (second (assoc name (getf (display-plist display) :cursors))))

(defsetf display-cursor setf-display-cursor)
(defun setf-display-cursor (display name cursor)
  (let ((name-cursor (assoc name (getf (display-plist display) :cursors))))
    (if name-cursor
	(setf (second name-cursor) cursor)
	(push (list name cursor) (getf (display-plist display) :cursors)))  
    cursor))


(defun contact-image-cursor (contact image &key mask (x 0) (y 0) foreground background)
  "Returns and caches a cursor for the IMAGE. Either the IMAGE or the MASK can 
   be a pixmap or an image, but both must be depth 1. By default, MASK is IMAGE. 
   FOREGROUND and BACKGROUND are xlib:color objects and default to black and white, respectively."
  (declare (type contact contact)
	   (type (or null pixmap image) image mask)
	   (type int16 x y)
	   (type (or null color) foreground background))

  (let ((display (contact-display contact)))
    (or (display-cursor display image)
	
	(let ((source (if (pixmap-p image)
			  image
			  (image-pixmap contact image :depth 1)))
	      (mask   (if (pixmap-p (setf mask (or mask image)))
			  mask
			  (image-pixmap contact mask :depth 1))))
	  (setf (display-cursor display image)
		(create-cursor
		  :source     source
		  :mask       mask
		  :x          x
		  :y          y
		  :foreground (or foreground
				  (make-color :red 0.0 :green 0.0 :blue 0.0))
		  :background (or background
				  (make-color :red 1.0 :green 1.0 :blue 1.0))))))))

(defun contact-glyph-cursor (contact index &key foreground background)
  "Returns and caches a cursor for the glyph given by the INDEX in the cursor font."
  (declare (type card8 index)
	   (type contact contact))

  (let ((display (contact-display contact)))
    (or (display-cursor display index )
	
	(let ((font (open-font display "cursor")))
	  (setf (display-cursor display index)
		(create-glyph-cursor
		  :source-font font
		  :source-char index
		  :mask-font   font
		  :mask-char   (1+ index)
		  :foreground  (or foreground
				   (make-color :red 0.0 :green 0.0 :blue 0.0))
		  :background  (or background
				   (make-color :red 1.0 :green 1.0 :blue 1.0))))))))
