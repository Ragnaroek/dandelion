; f2cl7.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; optimization post processing phase
; functions:
;	find-go-labels 
;	count-go-end
;	labelp
;	find-all-labels
;	remove-from-tree
;	prune-labels
;	cond-remove-end-label

(in-package :f2cl)

(defvar *prune_labels* t)
(eval-when (compile load eval) (proclaim '(special *verbose*)))
;-----------------------------------------------------------------------
#+nil
(defun find-go-labels (x)
  (cond
     ((atom x) nil)
     ((eq (car x) 'go) (cons (cadr x) (find-go-labels (cdr x))))
     (t (append (find-go-labels (car x))
                (find-go-labels (cdr x))))))

(defun find-go-labels (x)
  (cond ((atom x) nil)
	((eq (car x) 'go)
	 (cons (cadr x) (find-go-labels (cdr x))))
	((eq (first x) 'computed-goto)
	 (append (second x)
		 (find-go-labels (cdr x))))
	(t
	 (append (find-go-labels (car x))
		 (find-go-labels (cdr x))))))
;-----------------------------------------------------------------------
(defun count-go-end (x)
  (cond
     ((atom x) 0)
     ((equal x  '(go end_label)) 1)
     (t (+ (count-go-end (car x))
           (count-go-end (cdr x))))))
;-----------------------------------------------------------------------
(defun labelp (x)
  (and (symbolp x)
       (eq 5 (string-lessp (symbol-name :label) (string x)))))
;------------------------------------------------------------------------
(defun find-all-labels (x)
  (cond
     ((atom x) nil)
     ((labelp (car x)) (cons (car x) (find-all-labels (cdr x))))
     (t (append (find-all-labels (car x))
                (find-all-labels (cdr x))))))
;-----------------------------------------------------------------------
(defun remove-from-tree (s tree) ; s must be a symbol
  (setq tree (if (atom tree) tree (remove s tree :test #'eq))) 
  (mapcar #'(lambda (y) (if (atom y) y (remove-from-tree s y))) tree))
;-----------------------------------------------------------------------
(defun prune-labels (fun)
 (if *verbose* (format t "post-processing begins ...~%"))
 (if *prune_labels*
  (let* ((labels (find-all-labels fun))
         (golabels (find-go-labels fun))
         (spare-labels (set-difference labels golabels)))
        (setq fun (cond-remove-end-label fun))
        (dolist (s spare-labels fun)
           (setq fun (remove-from-tree s fun))))
  fun))
;-----------------------------------------------------------------------
; If "(go end_label)" appears just once in a subroutine the second and
; third to last exprs of the subroutine are removed if (go end_label)
; is third to last expr.
; Also if "(go end_label)" does not apprear in the subroutine "end_label"
; is removed.
(defun cond-remove-end-label (fun)
  (let* ((name (second fun))
        (args (third fun))
        (body (nthcdr 3 fun))
        (rbody (reverse body)))
    (setq body
          (cond ((zerop (count-go-end body))
                 (remove 'end_label body))
                ((and (equal (third rbody) '(go end_label))
                      (= (count-go-end body) 1))
                 (reverse (cons (car rbody) (cdddr rbody))))
                (t body)))
    `(defun ,name ,args ,@body)))
;-----------------------------------------------------------------------
(defun find-declarations (fun)
 (remove nil 
   (mapcar 
      #'(lambda (x) (if (and (listp x) (or (eq (car x) 'declare)
                                           (eq (car x) 'proclaim))) 
                        x nil)) 
      fun)))
;;;-----------------------------------------------------------------------
;;; end of f2cl7.l
;;;
;;; $Id: f2cl7.l,v 1.6 2002/09/13 17:50:19 rtoy Exp $
;;; $Log: f2cl7.l,v $
;;; Revision 1.6  2002/09/13 17:50:19  rtoy
;;; From Douglas Crosher:
;;;
;;; o Make this work with lower-case Lisps
;;; o Fix a few typos
;;; o Make a safer fortran reader.
;;;
;;; Revision 1.5  2000/07/28 17:07:34  rtoy
;;; o We are in the f2cl package now.
;;;
;;; Revision 1.4  2000/07/27 16:39:17  rtoy
;;; We want to be in the CL-USER package, not the USER package.
;;;
;;; Revision 1.3  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------

