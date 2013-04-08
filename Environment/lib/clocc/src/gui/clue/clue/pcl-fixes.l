;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                   P.O. BOX 149149                                |
;;;                                AUSTIN, TEXAS 78714-9149                          |
;;;                                                                                  |
;;;          Copyright (C) 1987, 1988,1989,1990 Texas Instruments Incorporated.      |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

(in-package "PCL")

;;;----------------------------------------------------------------------------------+
;;;  Date: Fri, 26 May 89 12:11 PDT                                                  |
;;;  From: Gregor.pa@xerox.com                                                       |
;;;  Subject: EQL bug in Victoria Day PCL                                            |
;;;  To: CommonLoops.PA@xerox.com                                                    |
;;;                                                                                  |
;;;  There is a bug in Victoria Day PCL with certain uses of EQL                     |
;;;  specializers.  I believe that this bug could also strike people doing           |
;;;  simple meta-level programming, but have not heard any reports of such.          |
;;;                                                                                  |
;;;  The following patch should deal with this problem.  You can either put          |
;;;  this in a separate file and compile and load that on top of PCL, or you         |
;;;  can make the changes to the PCL sources and recompile.                          |
;;;----------------------------------------------------------------------------------+

;from file defs.lisp (this is an addition)
(defvar *notice-methods-change-force-p* ())

;from file boot.lisp
(defun fix-early-generic-functions (&optional noisyp)
  (allocate-instance (find-class 'standard-generic-function));Be sure this
						              ;class has an
						              ;instance.
  (let* ((class (find-class 'standard-generic-function))
	 (wrapper (class-wrapper class))
	 (n-static-slots (class-no-of-instance-slots class))
	 #+Lucid
	 (lucid::*redefinition-action* nil)
	 (*notice-methods-change-force-p* t))
    (flet ((fix-structure (gf)
	     (let ((static-slots
		     (%allocate-static-slot-storage--class n-static-slots)))
	       (setf (funcallable-instance-wrapper gf) wrapper
		     (funcallable-instance-static-slots gf) static-slots))))

      (dolist (early-gf-spec *early-generic-functions*)
	(when noisyp (format t "~&~S..." early-gf-spec))
	(let* ((early-gf (gdefinition early-gf-spec))
	       (early-static-slots
		 (funcallable-instance-static-slots early-gf))
	       (early-discriminator-code nil)
	       (early-methods nil)
	       (aborted t))
	  (flet ((trampoline (&rest args)
		   (apply early-discriminator-code args)))
	    (if (not (listp early-static-slots))
		(when noisyp (format t "already fixed?"))
		(unwind-protect
		    (progn
		      (setq early-discriminator-code
			    (early-gf-discriminator-code early-gf))
		      (setq early-methods
			    (early-gf-methods early-gf))
		      (setf (gdefinition early-gf-spec) #'trampoline)
		      (when noisyp (format t "trampoline..."))
		      (fix-structure early-gf)
		      (when noisyp (format t "fixed..."))
		      (initialize-instance early-gf)
		      (dolist (early-method early-methods)
			(destructuring-bind
			     (class quals lambda-list specs fn doc slot-name)
			     (cadddr early-method)
			  (setq specs
				(early-method-specializers early-method t))
			  (real-add-method early-gf 
					   (real-make-a-method class
							       quals
							       lambda-list
							       specs
							       fn
							       doc
							       slot-name))
			  (when noisyp (format t "m"))))
		      (setf (slot-value early-gf 'name) early-gf-spec)
		      (setq aborted nil))
		  (setf (gdefinition early-gf-spec) early-gf)
		  (when noisyp (format t "."))
		  (when aborted
		    (setf (funcallable-instance-static-slots early-gf)
			  early-static-slots)))))))
	  
      (dolist (fns *early-functions*)
	(setf (symbol-function (car fns)) (symbol-function (caddr fns))))
      (dolist (fixup *generic-function-fixups*)
	(destructuring-bind (gf-spec lambda-list specializers method-fn-name)
			    fixup
	  (let* ((fn (if method-fn-name
			 (symbol-function method-fn-name)
			 (symbol-function gf-spec)))
		 (gf (make-instance 'standard-generic-function))
		 (method (make-a-method
			   'standard-method () lambda-list specializers fn nil)))
	    (set-function-name gf gf-spec)
	    (setf (generic-function-name gf) gf-spec)
	    (real-add-method gf method)
	    (setf (gdefinition gf-spec) gf)))))))

;from file methods.lisp
(defun notice-methods-change (generic-function)
  (let ((old-discriminator-code
	  (generic-function-discriminator-code generic-function)))
   (if *notice-methods-change-force-p*
       (notice-methods-change-1 generic-function)
       ;; Install a lazy evaluation discriminator code updator as the
       ;; funcallable-instance function of the generic function.  When
       ;; it is called, it will update the discriminator code of the
       ;; generic function, unless it is inside a recursive call to
       ;; the generic function in which case it will call the old
       ;; version of the discriminator code for the generic function.
       ;;
       ;; Note that because this closure will be the discriminator code
       ;; of a generic function it must be careful about how it changes
       ;; the discriminator code of that same generic function.  If it
       ;; isn't careful, it could change its closure variables out from
       ;; under itself.
       ;;
       ;; In order to prevent this we take a simple measure:  we just
       ;; make sure that it doesn't try to reference its own closure
       ;; variables after it makes the dcode change.  This is done by
       ;; having notice-methods-change-2 do the work of making the
       ;; change AND calling the actual generic function (a closure
       ;; variable) over.  This means that at the time the dcode change
       ;; is made, there is a pointer to the generic function on the
       ;; stack where it won't be affected by the change to the closure
       ;; variables.
       ;;
       (set-funcallable-instance-function
	 generic-function
	 #'(lambda (&rest args)
	     #+Genera
	     (declare (dbg:invisible-frame :clos-internal))
	     (if (memq generic-function *invalid-generic-functions-on-stack*)
		 (if old-discriminator-code
		     (apply old-discriminator-code args)
		     (error "Tried to call an invalid generic function ~
                              that never was valid at all."))
		 (notice-methods-change-2 generic-function args)))))))



;;;----------------------------------------------------------------------------------+
;;;  Date: Thu, 7 Sep 89 13:18:12 +0100                                              |
;;;  From: matsj@delfi.UDAC.UU.SE                                                    |
;;;  Subject: A stab at fixing a bug in Victoria Day PCL                             |
;;;  To:   CommonLoops.PA@Xerox.COM                                                  |
;;;  Redistributed: CommonLoops.PA                                                   |
;;;                                                                                  |
;;;                                                                                  |
;;;  I use "Victoria Day" PCL. It has a bug with certain uses of                     |
;;;  EQL-specializers which makes it impossible to compile                           |
;;;  clue/intrinsics.lisp. I got a patch for this written by                         |
;;;  gregor.pa@xerox.com (26 may 89), which seemed to fix the problem.               |
;;;  However, when I got clue going, I noticed that it took a very long              |
;;;  time (30 cpu seconds) to instantiate contacts (make-contact).                   |
;;;                                                                                  |
;;;  The time was spent in cluei:default-options and in                              |
;;;  initialize-instance. When a generic function first is created it has a          |
;;;  function definition that will "compile" itself and then call the                |
;;;  result (notice-methods-change et al). When something a generic                  |
;;;  function depends on is redefined the generic function is                        |
;;;  "invalidated", which means resetting its function-definition to the             |
;;;  initial one. However, in certain cases (like the above mentioned) this          |
;;;  process also meant doing initialization of other classes, which caused          |
;;;  the invalidation of the originating generic function. This had the              |
;;;  effect that it had to "compile" itself every time it was called.                |
;;;                                                                                  |
;;;  The fix I tried was for notice-methods-change to do nothing if the              |
;;;  generic function is being initialized (member of the list                       |
;;;  *invalid-generic-functions-on-stack*). This seemed to work, contacts            |
;;;  now take a very long time to instantiate only the first time, and I             |
;;;  haven't noticed any bad side-effects yet. What do the pcl-gurus say             |
;;;  about this?                                                                     |
;;;                                                                                  |
;;;  Here follows notice-methods-change from Gregors patch (this is *not*            |
;;;  the complete patch by Gregor) with my fix added (marked with *'s) and           |
;;;  a lot of comments deleted.                                                      |
;;;                                                                                  |
;;;  Mats Johnson, UDAC, Box 2103, S-750 02 Uppsala, SWEDEN. Phone +46 18-187858     |
;;;  E-mail: matsj@delfi.UDAC.UU.SE                                                  |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+

;from file methods.lisp
(defun notice-methods-change (generic-function)
  (let ((old-discriminator-code
	 (generic-function-discriminator-code generic-function)))
    (if *notice-methods-change-force-p*
	(notice-methods-change-1 generic-function)

	(if (not (memq generic-function *invalid-generic-functions-on-stack*)) ; **********

	    (set-funcallable-instance-function
	     generic-function
	     #'(lambda (&rest args)
		 #+Genera
		 (declare (dbg:invisible-frame :clos-internal))
		 (if (memq generic-function *invalid-generic-functions-on-stack*)
		     (if old-discriminator-code
			 (apply old-discriminator-code args)
			 (error "Tried to call an invalid generic function ~
                              that never was valid at all."))
		     (notice-methods-change-2 generic-function args))))))))


