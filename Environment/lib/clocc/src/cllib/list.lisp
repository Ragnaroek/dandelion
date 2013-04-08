;;; Additional List Operations
;;;
;;; Copyright (C) 1997-2002, 2006 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: list.lisp,v 2.8 2006/04/07 18:29:27 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/list.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-collect', `zero-len-p', `to-list', `filter'
  (require :cllib-simple (translate-logical-pathname "cllib:simple")))

(in-package :cllib)

(export '(jumps count-jumps freqs
          check-list-type check-list-values out-of-bounds-p
          nsplit-list with-sublist with-nsplit call-on-split))

;;;
;;; {{{ misc
;;;

(defun jumps (seq &key (pred #'eql) (key #'value) args (what :both))
  "Return the list of elements of the sequence SEQ whose KEY differs
from that of the previous element according to the predicate PRED.
ARGS (list) are passed to PRED after the previous and the current KEYs.
WHAT can be
 :BOTH (list of conses of the previous and the next records,
 :PREV (list of records before the jump) or
 :NEXT (list of records after the jump).
Default is :BOTH."
  (declare (sequence seq) (type (function (t t) t) pred)
           (type (function (t) t) key))
  (with-collect (collect)
    (let (pkey prec)
      (map nil (lambda (rec)
                 (let ((ckey (funcall key rec)))
                   (when (and pkey (apply pred pkey ckey args))
                     (collect (ecase what
                                (:both (cons prec rec))
                                (:prev prec)
                                (:next rec))))
                   (setq prec rec pkey ckey)))
           seq))))

(defun count-jumps (seq &key (pred #'eql) (key #'value) args)
  "Like `jumps', but only count the jumps.
Thus, (apply #'count-jumps args) == (length (apply #'jumps args))."
  (declare (sequence seq) (type (function (t t) t) pred)
           (type (function (t) t) key))
  (let (pkey (res 0))
    (declare (type index-t res))
    (map nil (lambda (rec)
               (let ((ckey (funcall key rec)))
                 (when (and pkey (apply pred pkey ckey args)) (incf res))
                 (setq pkey ckey)))
         seq)
    res))

(defun freqs (seq &key (test #'eql) (key #'identity))
  "Return an alist of (num . freq) of elements of the SEQ.
The alist is sorted by decreasing frequencies. TEST defaults to `eql'."
  (declare (sequence seq) (type (function (t t) t) test)
           (type (function (t) t) key))
  (unless (zero-len-p seq)
    (sort
     (reduce (lambda (res el)
               (let ((fi (assoc el res :test test)))
                 (cond (fi (incf (cdr fi)) res) ((acons el 1 res)))))
             seq :key key :initial-value nil)
     #'> :key #'cdr)))

;;;
;;; }}}{{{ splitting, sublists
;;;

(defun nsplit-list (lst &key (pred #'eql) (key #'identity) (obj nil objp))
  "Return the list of sublists of LST, separated using PRED. Destructive.
When (funcall pred a0 a1) is nil, a1 starts another sublist,
i.e., in all sublists KEY is the same according to PRED.
When OBJ is given, it serves as separator and is omitted from the list."
  (declare (list lst) (type (function (t t) t) pred)
           (type (or function fixnum symbol) key))
  (when (symbolp key) (setq key (fdefinition key)))
  (unless lst (return-from nsplit-list nil))
  (if objp
      (do ((ll lst) (bb lst) res)
          ((null ll) (nreverse (if bb (cons bb res) res)))
        (if (funcall pred (funcall key (cadr ll)) obj)
            (setf res (cons bb res) bb (cddr ll) (cdr ll) nil ll bb)
            (setq ll (cdr ll))))
      (typecase key
        (function
         (do ((ll lst) (k0 (funcall key (first lst)) k1) k1 (res (list lst)))
             ((endp (cdr ll)) (nreverse res))
           (setq k1 (funcall key (second ll)))
           (cond ((not (funcall pred k0 k1))
                  (push (cdr ll) res)
                  (setf (cdr ll) nil)
                  (setq ll (car res)))
                 (t (setq ll (cdr ll))))))
        (fixnum
         (decf key)
         (do* ((ll lst) ta res) ((endp ll) (nreverse res))
           (push ll res) (setq ta (nthcdr key ll) ll (cdr ta))
           (when ta (setf (cdr ta) nil))))
        (t (error 'case-error :proc 'nsplit-list :args
                  (list 'key key 'function 'fixnum))))))

(defmacro with-sublist ((newl oldl e0 e1 &key (key '#'identity) (test '#'eql))
                        &body body)
  "Evaluate BODY, binding the NEWL to the sublist of OLDL from E0 to E1
inclusively. KEY and TEST have the usual meaning and default.
BODY may not modify the list structure of NEWL, or else!
Also, do NOT try to return a cons from NEWL.  You'd be surprised!"
  (with-gensyms ("WSL-" tt kk)
    `(let* (,kk (,newl (member-if (lambda (el) (setq ,kk (funcall ,key el))
                                          (or (funcall ,test ,kk ,e0)
                                              (funcall ,test ,kk ,e1))) ,oldl))
            (,tt (member (if (funcall ,test ,kk ,e0) ,e1 ,e0) ,newl :key
                         ,key :test ,test)))
      (unwind-protect
           (progn (when ,tt (setq ,kk (cdr ,tt)) (setf (cdr ,tt) nil))
                  ,@body)
        (when ,tt (setf (cdr ,tt) ,kk))))))

(defmacro with-nsplit ((newl oldl &rest split-args) &body body)
  "Evaluate BODY, binding NEWL to the splitting of OLDL.
BODY may not modify the list structure of NEWL, or else!
Also, do NOT try to return a cons from NEWL.  You'd be surprised!"
  `(let (,newl)
    (unwind-protect
         (progn (setq ,newl (nsplit-list ,oldl ,@split-args)) ,@body)
      (setq ,oldl (apply #'nconc ,newl)))))

(defun call-on-split (lst func &rest args &key (split-key #'value)
                      (split-pred #'eql) min-len &allow-other-keys)
  "Call FUNC on all sublists of LST generated by `nsplit-list'."
  (declare (list lst) (function func) (type (or null fixnum) min-len))
  (setq args (remove-plist args :split-key :split-pred :min-len))
  (with-nsplit (nl lst :key split-key :pred split-pred)
    (let ((ii -1) (cnt? (typep split-key 'fixnum)))
      (declare (type (signed-byte 21) ii))
      (filter nl (lambda (ll) (or (null min-len) (> (length ll) min-len)))
              (lambda (ll)
                (cons (if cnt? (incf ii) (funcall split-key (car ll)))
                      (apply func ll args)))))))

(provide :cllib-list)
;;; list.lisp ends here
