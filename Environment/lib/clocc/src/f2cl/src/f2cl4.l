; f2cl4.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------------------
; functions:
;	id-logical
;	id-logical-and 
;	id-logical-not
;	id-relational
;	memberq
;-----------------------------------------------------------------------------
(in-package :f2cl)
(defun memberq (x l) (member x l :test #'eq))

(defun id-logical (l)
   (cond ((member 'or l) 
          `(or ,@(mapcar #'id-logical-and (list-split-multi 'or l))))
         (t (id-logical-and l))))

(defun id-logical-and (l)
   (cond ((member 'and l)
          `(and ,@(mapcar #'id-logical-not (list-split-multi 'and l))))
         (t (id-logical-not l))))

(defun id-logical-not (l)
     (cond ((equal (car l) 'not)
            `(not ,(id-relational (cdr l))))
           (t (id-relational l))))
;----------------------------------------------------------------------------

(defun id-relational (l)
  (prog (lhs rhs tmplist)
    (return
      (cond

; true and false
            ((equal l '(true)) t)
            ((equal l '(false)) nil)

; bracketed logical expression:
            ((and (listp (car l)) (null (cdr l)))
             (id-logical (car l)))

; logical equivalence
            ((memberq 'logeqv l)
             (setq tmplist (list-split 'logeqv l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(logeqv ,lhs ,rhs))

; logical non-equivalence
            ((memberq 'logxor l)
             (setq tmplist (list-split 'logxor l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(logxor ,lhs ,rhs))

; greater than or equal:
            ((memberq '|>=| l)
             (setq tmplist (list-split '|>=| l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(>= ,lhs ,rhs))

; less than or equal:
            ((memberq '|<=| l)
             (setq tmplist (list-split '|<=| l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(<= ,lhs ,rhs))


; equality:
            ((memberq 'equal l)
             (setq tmplist (list-split-bin 'equal l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(= ,lhs ,rhs))

; not equal:
            ((memberq '|><| l)
             (setq tmplist (list-split '|><| l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(/= ,lhs ,rhs))

; less than:
            ((memberq '< l)
             (setq tmplist (list-split-bin '< l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(< ,lhs ,rhs))

; greater than:
            ((memberq '> l)
             (setq tmplist (list-split-bin '> l))
             (setq lhs (id-expression (car tmplist)))
             (setq rhs (id-expression (cadr tmplist)))
             `(> ,lhs ,rhs))

; bracketed logical:
            ((and (listp (car l)) (null (cdr l)))
             (id-logical (car l)))
            (t 
             (parse-expression l))   
))))

;;;------------------------------------------------------------------------------
;;; end of f2cl4.l
;;;
;;; $Id: f2cl4.l,v 1.5 2002/09/13 17:50:19 rtoy Exp $
;;; $Log: f2cl4.l,v $
;;; Revision 1.5  2002/09/13 17:50:19  rtoy
;;; From Douglas Crosher:
;;;
;;; o Make this work with lower-case Lisps
;;; o Fix a few typos
;;; o Make a safer fortran reader.
;;;
;;; Revision 1.4  2000/07/28 17:03:40  rtoy
;;; o We are in the f2cl package now.
;;;
;;; Revision 1.3  2000/07/27 16:39:17  rtoy
;;; We want to be in the CL-USER package, not the USER package.
;;;
;;; Revision 1.2  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
