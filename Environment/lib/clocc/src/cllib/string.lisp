;;; String Utilities
;;;
;;; Copyright (C) 1997-2007 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: string.lisp,v 1.14 2007/01/03 17:32:39 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/string.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype")))

(in-package :cllib)

(export
 '(string-beg-with string-end-with string-beg-with-cs string-end-with-cs
   edit-distance position-limit *string-junk* remove-subseq
   purge-string split-string split-seq substitute-subseq substitute-subseq-if))

;;;
;;;
;;;

(let ((cache-vec (make-array 10 :adjustable t)) diag)
(defun edit-distance (s1 s2 &key (test #'eql))
  "The edit (Levenshtein) distance between strings
 (actually, arbitrary vectors).
See <http://www.merriampark.com/ld.htm>
<http://www.cut-the-knot.org/do_you_know/Strings.shtml>."
  (let ((l1 (length s1)) (l2 (length s2)))
    (unless (>= (length cache-vec) l1)
      (adjust-array cache-vec l1))
    (setq diag 0)
    (loop :for i :from 0 :below l1 :do (setf (aref cache-vec i) (1+ i)))
    (loop :for j :from 0 :below l2 :and c2 :across s2 :do
      (loop :for i :from 0 :below l1 :and c1 :across s1
        :for old = (aref cache-vec i) :do
        (shiftf diag (aref cache-vec i)
                (min (if (funcall test c1 c2) diag (1+ diag))
                     (1+ (aref cache-vec i))
                     (1+ (if (zerop i) (1+ j) (aref cache-vec (1- i))))))))
    (aref cache-vec (1- l1)))))

(defun position-limit (string string-seq limit)
  "Find the position of STRING in STRING-SEQ looking at the first LIMIT chars."
  (position string string-seq
            :test (lambda (s1 s2)
                    (string-equal s1 s2
                                  :end1 (min (length s1) limit)
                                  :end2 (min (length s2) limit)))))

(defmacro string-beg-with (beg strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV starts with BEG."
  (if (stringp beg)
      (let ((len (length beg)))
        `(and (>= ,lenv ,len) (string-equal ,beg ,strv :end2 ,len)))
      (with-gensyms ("SBW-" len)
        `(let ((,len (length ,beg)))
          (and (>= ,lenv ,len) (string-equal ,beg ,strv :end2 ,len))))))

(defmacro string-end-with (end strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV ends with END."
  (if (stringp end)
      (let ((len (length end)) (ll (gensym "SEW-")))
        `(let ((,ll ,lenv))
          (and (>= ,ll ,len) (string-equal ,end ,strv :start2 (- ,ll ,len)))))
      (with-gensyms ("SEW-" len ll)
        `(let ((,len (length ,end)) (,ll ,lenv))
          (and (>= ,ll ,len)
           (string-equal ,end ,strv :start2 (- ,ll ,len)))))))

(defmacro string-beg-with-cs (beg strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV starts with BEG, case sensitive."
  (if (stringp beg)
      (let ((len (length beg)))
        `(and (>= ,lenv ,len) (string= ,beg ,strv :end2 ,len)))
      (with-gensyms ("SBWC-" len)
        `(let ((,len (length ,beg)))
          (and (>= ,lenv ,len) (string= ,beg ,strv :end2 ,len))))))

(defmacro string-end-with-cs (end strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV ends with END, case sensitive."
  (if (stringp end)
      (let ((len (length end)) (ll (gensym "SEWC-")))
        `(let ((,ll ,lenv))
          (and (>= ,ll ,len) (string= ,end ,strv :start2 (- ,ll ,len)))))
      (with-gensyms ("SEWC-" len ll)
        `(let ((,len (length ,end)) (,ll ,lenv))
          (and (>= ,ll ,len) (string= ,end ,strv :start2 (- ,ll ,len)))))))

(defcustom *string-junk* (simple-string 5) ":-,./"
  "The characters removed from a string by `purge-string'.")

(defsubst purge-string (str &optional (*string-junk* *string-junk*))
  "Non-destructively remove junk from string."
  (substitute-if #\Space (lambda (cc) (find cc *string-junk* :test #'char=))
                 str))

(defun split-seq (seq pred &key (start 0) end key strict)
  "Return a list of subseq's of SEQ, split on predicate PRED.
Start from START, end with END.  If STRICT is non-nil, collect
zero-length subsequences too.
  (split-seq SEQ PRED &key (start 0) end key strict)"
  (declare (sequence seq) (type (function (t t) t) pred) (fixnum start))
  (loop :for st0 = (if strict start
                       (position-if-not pred seq :start start
                                        :end end :key key))
        :then (if strict (if st1 (1+ st1))
                  (position-if-not pred seq :start (or st1 st0)
                                   :end end :key key))
        :with st1 = 0 :while (and st0 st1) :do
        (setq st1 (position-if pred seq :start st0 :end end :key key))
        :collect (subseq seq st0 (or st1 end))))

(defsubst split-string (str chars &rest opts)
  "Split the string on chars."
  (declare (string str) (sequence chars))
  (apply #'split-seq str (lambda (ch) (declare (character ch)) (find ch chars))
         opts))

(defun sequence-type (seq)
  "Return the symbol representing the type of the sequence SEQ.
Returns one of (string, vector or list)."
  (typecase seq
    (string 'string) (list 'list)
    (vector (let ((eltype (array-element-type seq)))
              (if (eq eltype t) 'vector (list 'vector eltype))))
    (t (error 'case-error :proc 'sequence-type :args
              (list 'seq seq 'string 'vector 'list)))))

(defun substitute-subseq-if (seq begf endf repf &key (start 0) end)
  "Substitute all subsequences in a sequence with a replacement sequence.
The result is of the same type as SEQ.
  (substitute-subseq-if SEQ BEG-F END-F REPL-F &key START END)
BEG-F and END-F will be passed 3 arguments: SEQ, start and end of search
REP-F will be passed 4 arguments: SEQ, start and end of replacement, and
 the type of the sequence to return.
Therefore, `substitute-subseq' could be implemented as

 (defun substitute-subseq (seq sub rep &key (start 0) end
                           (test #'eql) (key #'identity))
   (substitute-subseq-if
    seq
    (lambda (seq beg fin)
      (search sub seq :start2 beg :end2 fin :test test :key key))
    (lambda (seq beg fin) (declare (ignore seq fin)) (+ beg (length sub)))
    (lambda (seq beg fin type) (declare (ignore seq beg fin type)) rep)
    :start start :end end))

Is is implemented separately for (non-existent :-) performance reasons."
  (declare (sequence seq) (type index-t start)
           (type (function (sequence index-t t) (or null index-t)) begf)
           (type (function (sequence index-t t) index-t) endf)
           (type (function (sequence index-t t symbol) sequence) repf))
  (loop :with type = (sequence-type seq)
        :and last :of-type index-t = start
        :for next = (funcall begf seq last end)
        :unless (or next all) :return seq
        :collect (subseq seq last next) :into all
        :while next
        :do (setq last (funcall endf seq next end))
        :collect (funcall repf seq next last type) :into all
        :finally (return (reduce (lambda (s0 s1) (concatenate type s0 s1))
                                 all :initial-value (subseq seq 0 start)))))

(defun substitute-subseq (seq sub rep &key (start 0) end
                          (test #'eql) (key #'identity))
  "Substitute all subsequences in a sequence with a replacement sequence.
The result is of the same type as SEQ.
  (substitute-subseq SEQ SUB REP &key START END TEST KEY)"
  (declare (sequence seq sub rep) (type index-t start))
  (loop :with type = (sequence-type seq)
        :and olen :of-type index-t = (length sub)
        :and last :of-type index-t = start
        :for next = (search sub seq :start2 last :end2 end :test test :key key)
        :unless (or next all) :return seq
        :collect (subseq seq last next) :into all
        :while next :collect rep :into all :do (setq last (+ next olen))
        :finally (return (reduce (lambda (s0 s1) (concatenate type s0 s1))
                                 all :initial-value (subseq seq 0 start)))))

(defun remove-subseq (seq beg &optional end)
  "Return a fresh sequence identical to the original one with a part removed."
  (let ((len (length seq)))
    (cond ((or (null end) (= end len)) (subseq seq 0 beg))
          ((zerop beg) (subseq seq (or end len)))
          (t (concatenate (sequence-type seq)
                          (subseq seq 0 beg) (subseq seq end))))))

(provide :cllib-string)
;;; }}} string.lisp ends here
