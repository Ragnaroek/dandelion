
; f2cl3.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Copyright (c) University of  Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Hamilton, New Zeland 1992-95 - all rights reserved;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pattern matching for the syntax checker

; functions:
;	restrict-test
;	exact-match
;	match-atoms
;	match-atoms-lists
;	match-store-atoms
;       match-store - for atoms and lists
;	match-restrict - restricts atoms and matches for lists
;             - to use insert (restrict >var predicate) instead of >var or >
;	atomcar, atomcdr
;	match-separated - matches (a1 s1 a2 s2 ...... an) where the si are 
;	      separators , it delivers a list of two lists, the first being
;	      a list of lists of the ai and the second a list of the separators
;	      or returns nil if an ai is missing - this could be used to
;	      produce a syntax error.
;-----------------------------------------------------------------------------
(in-package :f2cl)

;(declare (localf atomcar atomcdr))

(defun sub-string (x index &optional len) 
  (cond ((null len) (subsequence x (sub1 index) (length x)))
        (t (subsequence x (sub1 index) (min (sub1 (+ index len)) (length x)) ))))

(defun match-separated (separators lis)
  (prog (ret)
  (setq ret (gen-list-split separators lis))
    (cond ((member nil (car ret)) (return nil))
          (t (return ret)))))

; exact match
(defun exact-match (p d)
   (cond ((and (null p) (null d)) t)  ; have we reached the end ?
         ((equal (car p) (car d))    ; check the first elements
                                     ; if not the same return nil
          (exact-match (cdr p) (cdr d)))))    ; recurse if the same


; match ">" against any atom
; we need a preliminary check to return nil if one list is shorter
(defun match-atoms (p d)
      (cond ((and (null p) (null d)) t)
            ((or (null p) (null d)) nil)   ; is one list shorter ?
            ((or (equal (car p) '>)       
                 (equal (car p) (car d)))
             (match-atoms (cdr p) (cdr d)))))

; incorporate the feature of allowing "+" to match against any number of atoms
(defun match-atoms-lists (p d)
      (cond ((and (null p) (null d)) t)
            ((or (null p) (null d)) nil)   
            ((or (equal (car p) '>)       
                 (equal (car p) (car d)))
             (match-atoms-lists (cdr p) (cdr d)))
            ((equal (car p) '+)                 ; first element + ?
             (cond ((match-atoms-lists (cdr p) (cdr d)))   ; drop +
                   ((match-atoms-lists p (cdr d)))))))           ; keep +


; bind matched atoms to A etc if the pattern has >A
(defun atomcar (a) (nthchar a 1))
(defun atomcdr (a) (concaten (sub-string (symbol-name a) 2)))

(defun match-store-atoms (p d)
      (cond ((and (null p) (null d)) t)
            ((or (null p) (null d)) nil)   
            ((or (equal (car p) '>)       
                 (equal (car p) (car d)))
             (match-store-atoms (cdr p) (cdr d)))
            ((and (equal (atomcar (car p)) '>)  ; check for >a
                  (match-store-atoms (cdr p) (cdr d)))
             (set (atomcdr (car p)) (car d)) t) ; bind the pattern variables
                                                 ; value to the matched atom
            ((equal (car p) '+)                 
             (cond ((match-store-atoms (cdr p) (cdr d)))   
                   ((match-store-atoms p (cdr d)))))))

; bind matched groups of atoms to B if the pattern has +B
; the eval obtains the list of atoms seen so far and the cons adds the atom
; matched at the current level
(defun match-store (p d)
      (cond ((and (null p) (null d)) t)
            ((or (null p) (null d)) nil)   
            ((or (equal (car p) '>)       
                 (equal (car p) (car d)))
             (match-store (cdr p) (cdr d)))
            ((and (equal (atomcar (car p)) '>)  
                  (match-store (cdr p) (cdr d)))
             (set (atomcdr (car p)) (car d)) t) 
            ((equal (car p) '+)                 
             (cond ((match-store (cdr p) (cdr d)))   
                   ((match-store p (cdr d)))))           
            ((equal (atomcar (car p)) '+)
             (cond ((match-store (cdr p) (cdr d))
                    (set (atomcdr (car p)) (list (car d))) t) ; building begins
                   ((match-store p (cdr d))
                    (set (atomcdr (car p))
                         (cons (car d) (eval (atomcdr (car p))))) t)))))



(defun restrict-test (predicates argument)
 (prog ()
       loop
	(cond ((null predicates) (return t)))   ; all tests t?
        (cond ((funcall (car predicates) argument) ; this test t?
	       (setq predicates (cdr predicates))
               (go loop))                         ; then repeat
               (t (return nil)))))                ; else fail

(defun match-restrict (p d)
	(cond
		((and (null p) (null d)) t)
                ((or (null p) (null d)) nil)
; restricted >
                ((and (not (atom (car p)))         
                      (equal (caar p) 'restrict)
                      (equal (cadar p) '>)
                      (restrict-test (cddar p) (car d)))
                 (match-restrict (cdr p) (cdr d)))
; restricted >var
		((and (not (atom (car p)))
                      (equal (caar p) 'restrict)
                      (equal (atomcar (cadar p)) '>)
                      (restrict-test (cddar p) (car d))
                      (match-restrict (cdr p) (cdr d)))
                 (set (atomcdr (cadar p)) (car d))
                 t)
; equality or >
		((or (equal (car p) '>)
                    (equal (car p) (car d)))
                 (match-restrict (cdr p) (cdr d)))
; >var
		((and (atom (car p))
                      (equal (atomcar (car p)) '>)
                      (match-restrict (cdr p) (cdr d)))
                 (set (atomcdr (car p)) (car d))
                t)
; +
		((equal (car p) '+)
                 (cond ((match-restrict (cdr p) (cdr d)))
                       ((match-restrict p (cdr d)))))
; +var
		((and (atom (car p)) (equal (atomcar (car p)) '+))
                 (cond ((match-restrict (cdr p) (cdr d))
                        (set (atomcdr (car p)) (list (car d)))
                        t)
                       ((match-restrict p (cdr d))
                        (set (atomcdr (car p))
                             (cons (car d) (eval (atomcdr (car p)))))
                        t)))))
;;;------------------------------------------------------------------------------
;;; end of f2cl3.l
;;;
;;; $Id: f2cl3.l,v 1.4 2000/07/28 17:02:14 rtoy Exp $
;;; $Log: f2cl3.l,v $
;;; Revision 1.4  2000/07/28 17:02:14  rtoy
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
