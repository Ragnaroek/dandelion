;;; print misc special opjects
;;;
;;; Copyright (C) 1997-2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: miscprint.lisp,v 1.21 2006/09/14 02:03:29 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/miscprint.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-collect', `count-all'
  (require :cllib-simple (translate-logical-pathname "cllib:simple")))

(in-package :cllib)

(export
 '(hash-table-keys hash-table->alist alist->hash-table make-ht-readtable pophash
   print-counts
   print-all-ascii print-all-packages plist->alist alist->plist plist= alist=))

;;;
;;; characters
;;;

(defun ascii-char (ch &optional (str *standard-output*))
  (declare (character ch) (stream str))
  (let ((co (char-code ch)))
    (format str "\"~c\"~5t~@c~17t~:@c~27t~:c~35t ~9:o ~9:d ~9:x  ~a"
            ch ch ch ch co co co (get-macro-character ch))))

;;;###autoload
(defun print-all-ascii (&optional (str *standard-output*))
  "Print all ASCII characters with their names and codes."
  (declare (type (or null stream) str))
  (loop :with *print-pretty* = nil
        :with st :of-type stream = (or str (make-string-output-stream))
        :initially
        (format
         st "~&char (~~c, ~~@c, ~~:@c, ~~:c)~42toct~52tdec~62thex  macro~%")
        :for co :from 0 :below char-code-limit
        :for ch :of-type (or null character) = (code-char co)
        :when ch :do (ascii-char ch st) (terpri st)
        :finally (unless str (return (get-output-stream-string st)))))

;;;
;;; packages
;;;

(defun print-package (pp &optional (out *standard-output*) verbose)
  "Print the package information.
Return 3 values: the number of accessible symbols,
the number of external symbols, and
the number of symbols without any bindings."
  (declare (type (or symbol string package) pp) (stream out))
  (let ((pa (if (packagep pp) pp
                (or (find-package pp)
                    (find-package (string-upcase (string pp))))))
        (ns 0) (ne 0) (nw 0))
    (declare (type index-t ns ne nw))
    (assert (packagep pa) (pa) " *** Package not found: `~s'" pp)
    (do-symbols (sy pa)
      (when (and (eq :internal (nth-value 1 (find-symbol (symbol-name sy) pa)))
                 (not (or (boundp sy) (fboundp sy) (symbol-plist sy))))
        (when verbose (format out " ~a" sy))
        (incf nw))
      (incf ns))
    (do-external-symbols (sy pa) (declare (ignore sy)) (incf ne))
    (format out "~& *** ~a~@[ [~{~a~^ ~}]~]:~%~@[~5tuses:~{ ~a~}~%~]~
~@[~5tused by:~{ ~a~}~%~]~5tsymbols: ~:d [external: ~:d] [wasted: ~:d]~%"
            (package-name pa) (package-nicknames pa)
            (mapcar #'package-name (package-use-list pa))
            (mapcar #'package-name (package-used-by-list pa)) ns ne nw)
    (values ns ne nw)))

;;;###autoload
(defun print-all-packages (&optional (out *standard-output*))
  "Print all packages."
  (declare (stream out))
  (let ((ns 0) (ne 0) (tn 0))
    (declare (type index-t ns ne tn))
    (dolist (pp (list-all-packages))
      (multiple-value-bind (n0 n1) (print-package pp out)
        (incf ns n0) (incf ne n1)))
    (format out " === `*package*' ===~%")
    (print-package *package* out)
    (do-all-symbols (sy) (declare (ignore sy)) (incf tn))
    (format out " === Total number of symbols: ~:d [~:d ~:d]~%" tn ns ne)))

;;;
;;; hash tables
;;;

(defun hash-table-keys (ht)
  "Return the list of all the keys in the hashtable."
  (declare (hash-table ht))
  ;; (loop :for kk :being :each :hash-key :of ht :collect kk)
  (with-collect (co)
    (with-hash-table-iterator (iter ht)
      (loop (multiple-value-bind (re kk) (iter)
              (unless re (return))
              (co kk))))))

(defun hash-table->alist (ht)
  "Return the alist with the same data as the hash-table.
Actually, the first element is the test: '(eql (key0 . val0) (key1 . val1)).
The inverse is `alist->hash-table'."
  (declare (hash-table ht))
  (cons (hash-table-test ht)
        (with-collect (co)
          (with-hash-table-iterator (iter ht)
            (loop (multiple-value-bind (re kk vv) (iter)
                    (unless re (return))
                    (co (cons kk vv))))))))

(defun alist->hash-table (alist)
  "Return the new hash-table based on this alist.
The inverse is `hash-table->alist'."
  (declare (list alist))
  (let ((ht (make-hash-table :test (car alist))))
    (dolist (co (cdr alist) ht)
      (setf (gethash (car co) ht) (cdr co)))))

(defun pophash (object ht)
  "Remove the value and return it."
  (multiple-value-bind (value present-p) (gethash object ht)
    (when present-p (remhash object ht))
    (values value present-p)))

(defun print-counts (seq &key (out *standard-output*) (key #'value)
                     (key-numeric-p nil) (test 'equal)
                     (format (if key-numeric-p
                                 (formatter ";; ~5:D --> ~5:D~%")
                                 (formatter ";; ~5A --> ~5:D~%"))))
  "Print counts of elements in the sequence, sorted by frequency.
If KEY-NUMERIC-P is non-NIL, sort by KEY instead."
  (when out
    (loop :for (object . count)
      :in (sort (cdr (hash-table->alist (count-all seq :key key :test test)))
                #'< :key (if key-numeric-p #'car #'cdr))
      :do (format out format object count))))

;;;###autoload
(defun make-ht-readtable (&optional (rt (copy-readtable)))
  "Make a readtable which will be able to read hash-tables with #h()."
  (set-dispatch-macro-character
   #\# #\h
   (lambda (st char arg)
     (declare (ignore char arg))
     (alist->hash-table (read st t nil t)))
   rt)
  rt)

;;; beware that some lisps (e.g., CLISP and CMUCL) will not use this
;;; method for hash-tables.  it does work with Allegro though.
;;; since CLISP externalizes hash-tables on its own, only CMUCL is a problem.
(unlock-package :common-lisp)
(defmethod print-object ((ht hash-table) (out stream))
  (if *print-readably*
      (format out "#h~s" (hash-table->alist ht))
      (call-next-method)))
(restore-package-lock :common-lisp)

;;;
;;; property lists
;;;

;; since neither `rotatef' nor `psetf' guarantee the order of
;; assignment, neither can be used here

(defun alist->plist (al)
  "Destructively transform an ALIST to a PLIST.  Non-consing."
  (do ((ll al (cddr ll))) ((null ll) al)
    (let ((co (car ll)))
      (setf (car ll) (car co)
            (car co) (cdr co)
            (cdr co) (cdr ll)
            (cdr ll) co))))

(defun plist->alist (pl)
  "Destructively transform a PLIST to an ALIST.  Non-consing."
  (do ((ll pl (cdr ll))) ((null (cdr ll)) pl)
    (let ((co (cdr ll)))
      (setf (cdr ll) (cdr co)
            (cdr co) (car co)
            (car co) (car ll)
            (car ll) co))))

(defun plist= (p1 p2 &key (test #'eql))
  "Check that the two property lists have the same properties."
  (macrolet ((p= (a b)
               `(do ((tail ,a (cddr tail)))
                    ((endp tail) t)
                  (unless (funcall test (second tail)
                                   (getf ,b (first tail) ,b))
                    (return nil)))))
    (and (p= p1 p2) (p= p2 p1))))

(defun alist= (a1 a2 &key (test #'eql))
  "Check that the two association lists have the same values."
  (macrolet ((a= (a b)
               `(dolist (pair ,a t)
                  (let ((other (assoc (car pair) ,b :test test)))
                    (unless (and other (funcall test (cdr pair) (cdr other)))
                      (return nil))))))
    (and (a= a1 a2) (a= a2 a1))))

(provide :cllib-miscprint)
;;; file miscprint.lisp ends here
