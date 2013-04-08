;#!/usr/bin/clisp -M ~sds/bin/clisp.mem -C
;;;
;;; Convert *.c to CLISP's ffi
;;;
;;; Copyright (C) 1999-2001 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: h2lisp.lisp,v 2.8 2005/01/27 23:02:47 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/h2lisp.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `text-stream'
  ;; (require :cllib-html (translate-logical-pathname "cllib:html"))
  )

(in-package :cllib)

(export '(h2lisp))

;;;
;;; C parsing
;;;

(defstruct c-dim "C dimension." dim)
(defstruct c-cmt "C comment." data)

(defun read-standalone-char (stream char) (declare (ignore stream)) char)

(defun read-c-object (stream char)
  (declare (stream stream) (character char))
  (ecase char
    (#\{ (read-delimited-list #\} stream t))
    (#\[ (make-c-dim :dim (car (read-delimited-list #\] stream t))))
    (#\#
     (let ((com (read stream t nil t)))
       (ecase com
         (|include|
          (let ((ec (ecase (peek-char t stream t nil t) (#\" #\") (#\< #\>))))
          (concatenate
           'string "#include "
           (loop :for cc :of-type character = (read-char stream t nil t)
                 :collect cc :until (char= cc ec))))))))
    ;;     (loop :for line :of-type simple-string =
    ;;           (concatenate 'string "#" (read-line stream))
    ;;           :then (read-line stream)
    ;;           :until (char= #\\ (schar line (1- (length line))))
    ;;           :collect line)
    (#\/
     (case (peek-char nil stream nil nil t)
       (#\*
        (read-char stream)
        (make-c-cmt :data
                    (concatenate
                     'string "/*"
                     (loop :for c1 = (read-char stream t nil t)
                           :and c2 = #\Null :then c1
                           :until (and (char= c2 #\*) (char= c1 #\/))
                           :collect c1)
                     "/")))
       (#\/ (make-c-cmt :data (concatenate 'string "/" (read-line stream))))
       (t #\/)))))

(defun make-c-readtable (&optional (rt (copy-readtable)))
  "Make the readtable for parsing C."
  (set-macro-character #\/ #'read-c-object nil rt)
  (set-macro-character #\| #'read-standalone-char nil rt)
  (set-macro-character #\# #'read-c-object nil rt)
  (set-syntax-from-char #\; #\a rt)
  (set-macro-character #\; #'read-standalone-char nil rt)
  (set-syntax-from-char #\# #\a rt)
  (set-macro-character #\# #'read-c-object nil rt)
  (set-syntax-from-char #\: #\a rt)
  (set-macro-character #\: #'read-standalone-char nil rt)
  (set-syntax-from-char #\, #\a rt)
  (set-macro-character #\, #'read-standalone-char nil rt)
  (set-macro-character #\* #'read-standalone-char nil rt)
  (set-macro-character #\{ #'read-c-object nil rt)
  (set-macro-character #\} (get-macro-character #\)) nil rt)
  (set-macro-character #\[ #'read-c-object nil rt)
  (set-macro-character #\] (get-macro-character #\)) nil rt)
  (setf (readtable-case rt) :preserve)
  rt)

(defparameter *c-readtable* (make-c-readtable) "The readtable for C parsing.")

;(defun uncomment-split (lst obj)
;  "Read object from TS, remove comments, split on OBJ."
;  (declare (list lst))
;  (nsplit-list (delete-if #'c-cmt-p lst) :obj obj))

(defun read-statement (ts)
  (declare (type text-stream ts))
  (loop :for zz = (read-next ts :skip #'c-cmt-p) :until (eql zz #\;)
        :collect zz))

(defparameter *c-types* '(int uint char) "Known C types.")
(defparameter *c-un-types* nil "UnKnown C types.")

(defun c-see-type (sym)
  (assert (and sym (not (string-equal (string sym) "void"))) ()
          "An attempt to define VOID or NIL: ~s" sym)
  (unless (member sym *c-types* :test #'eq)
    (pushnew sym *c-un-types* :test #'eq)))
(defun c-def-type (sym)
  ;(assert (not (member sym *c-types* :test #'eq)) () "redefining type ~s" sym)
  (push sym *c-types*)
  (setq *c-un-types* (delete sym *c-un-types* :test #'eq)))

(defun c-convert-decl (lst)
  "Convert declaration (type obj [dims]||(args)) to CLISP."
  (labels ((voidp (sy) (string-equal (string sy) "void"))
           (objp (ll) (or (symbolp ll)
                          (and (consp ll)
                               (or (eql (car ll) #\*)
                                   (null (cdr ll))))))
           (cc (ll)
             (format t " --> ~s~%" ll)
             (etypecase (car ll)
               (c-dim `(c-array ,(cc (cdr ll)) ,(c-dim-dim (car ll))))
               (cons (mapcar #'cc ll))
               (null nil)
               (c-cmt (cc (cdr ll)))
               (symbol
                (cond ((voidp (car ll))
                       (if (eql #\* (cadr ll))
                           'c-pointer nil))
                      (t (c-see-type (car ll))
                         (let* ((ptr (eql #\* (cadr ll))))
                           (list (c-convert-decl (if ptr (cddr ll) (cdr ll)))
                                 (if ptr `(c-ptr ,(car ll)) (car ll))))))))))
    (let* ((fu (find-if #'objp lst :from-end t)) (ta (member fu lst))
           (ld (ldiff lst ta))
           (tail (map-in (lambda (el) (nsplit-list el :obj #\,)) (cdr ta))))
      (typecase fu
        (symbol (unless (voidp fu) (list fu (cc (nconc tail ld)))))
        (cons `(,(car (last fu)) (c-function (:arguments ,(cc tail))
                                  (:return-type ,(cc ld)))))))))

(defun c-number (obj)
  (typecase obj
    (symbol (let* ((st (symbol-name obj)) (len (length st)))
              (cond ((string-beg-with "0x" st len)
                     (unintern obj)
                     (parse-integer st :radix 16 :start 2))
                    ((let ((ch (char st (1- len))))
                       (and (char-equal #\l ch)
                            (prog2 (setf (char st (1- len)) #\0)
                                (every #'digit-char-p st)
                              (setf (char st (1- len)) ch))))
                     (unintern obj)
                     (parse-integer st :end (1- len)))
                    (obj))))
    (t obj)))

(defun c-eval (lst)
  (cond ((null (cdr lst)) (c-number (car lst)))
        ((eq (second lst) '<<)
         (ash (c-number (first lst)) (c-number (third lst))))))

;;;
;;; H --> LISP
;;;

(defgeneric h2lisp (in out)
  (:documentation "Convert C header to lisp.
Return the number of forms processed.")
  (:method ((in string) (out t)) (h2lisp (pathname in) out))
  (:method ((in cons) (out t))
    (reduce #'+ in :key (lambda (in0) (h2lisp in0 out))))
  (:method ((in t) (out string)) (h2lisp in (pathname out)))
  (:method ((in t) (out pathname))
    (with-open-file (fout out :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
      (h2lisp in fout)))
  (:method ((in pathname) (out t))
    (with-open-file (fin in :direction :input)
      (h2lisp fin out))))

(defmethod h2lisp ((in stream) (out stream))
  (format out "~%;;; reading from ~a~2%" in)
  (unless (eq out *standard-output*)
    (format t "~%;;; reading from ~a~2%" in))
  (loop :with *readtable* = *c-readtable* :and numf = 0
        ;; :initially (setq *c-un-types* nil)
        :for line :of-type simple-string =
        (string-trim
         +whitespace+
         (or (read-line in nil nil)
             (return (progn (format out "~%;;; ~:d form~:p in ~a~2%" numf in)
                            (unless (eq out *standard-output*)
                              (format t "~%;;; ~:d form~:p in ~a~2%" numf in))
                            (when *c-un-types*
                              (format t "~% *** Undefined types:~
~{~<~%~20t ~1,79:; ~a~>~^,~}~%" *c-un-types*))
                            numf))))
        :for len = (length line)
        :with depth = 0 ; #if nesting
        :with ts :of-type text-stream = (make-text-stream :sock in)
        :do                     ; process statement
        (cond ((zerop len) (terpri out))
              ((< len 3) (format out ";;; ~a~%" line))
              ((string-beg-with "/*" line len)
               ;; comment, assume start/end on own line
               (format out ";;; ~a~%" line)
               (loop :until (search "*/" line :test #'char=)
                     :do (setq line (read-line in))
                     (format out ";;; ~a~%" line)))
              ((string-beg-with-cs "#if" line len)
               (incf depth)
               (format out ";;; ~a~%" line))
              ((string-beg-with-cs "#endif" line len)
               (decf depth)
               (format out ";;; ~a~%" line))
              ((string-beg-with-cs "#define" line len) (incf numf)
               ;; only 1-line simple defines!
               (format out ";;; ~a~%" line)
               (multiple-value-bind (var pos)
                   (read-from-string line nil +eof+ :start 7)
                 (let ((val (c-number (read-from-string
                                       line nil +eof+ :start pos))))
                   (typecase val
                     (symbol
                      (format out "(define-symbol-macro ~s ~s)~%" var val))
                     ((or number string)
                      (format out "(defconstant ~s ~s)~%" var val))))))
              ((string-beg-with-cs "#include" line len) (incf numf)
               (format out "(c-lines \"~a~~%\")~%" line))
              ((string-beg-with-cs "typedef struct" line len) (incf numf)
               ;; (format out ";;; ~a~%" line)
               (multiple-value-bind (t0 t1)
                   (values-list (string-tokens line :start 15 :max 2))
                 (c-def-type t1)
                 (c-see-type t0)
                 (format out "(def-c-type ~s ~s)~%" t0 t1)))
              ((string-beg-with-cs "typedef union" line len) (incf numf)
               ;; (format out ";;; ~a~%" line)
               (multiple-value-bind (t0 t1)
                   (values-list (string-tokens line :start 14 :max 2))
                 (c-def-type t1)
                 (c-see-type t0)
                 (format out "(def-c-type ~s ~s)~%" t0 t1)))
              ((string-beg-with-cs "typedef enum" line len) (incf numf)
               (setf (ts-buff ts) line (ts-posn ts) 12)
               (let* ((rr (read-next ts :skip #'c-cmt-p))
                      (en (nsplit-list (delete-if #'c-cmt-p rr) :obj #\,))
                      (tt (read-next ts :skip #'c-cmt-p)))
                 (c-def-type tt)
                 (dolist (el en)
                   (when (stringp (car el))
                     (format out "(c-lines \"~a~%\")~%" (car el))
                     (setf (car el) (cadr el) (cdr el) (cddr el)))
                   (when (cdr el)
                     (assert (eq '= (cadr el)))
                     (setf (cadr el) (c-eval (cddr el)) (cddr el) nil)))
                 (map-in (lambda (el) (if (cdr el) el (car el))) en)
                 ;; (format out "~s~%" (cons 'def-c-enum (cons tt en)))
                 (format out "(def-c-enum ~s~{~%  ~s~})~%" tt en)))
              ((string-beg-with-cs "typedef" line len) (incf numf)
               ;; (format out ";;; ~a~%" line)
               (setf (ts-buff ts) line (ts-posn ts) 7)
               (let ((ll (read-statement ts)))
                 (format out "(def-c-type ~s)~%" ll)
                 (format out "~s~%" (cons 'def-c-type (c-convert-decl ll)))))
              ((string-beg-with-cs "struct" line len) (incf numf)
               (setf (ts-buff ts) line (ts-posn ts) 6)
               (let ((tt (read-next ts :skip #'c-cmt-p))
                     (ll (map-in #'c-convert-decl
                                 (nsplit-list (read-next ts :skip #'c-cmt-p)
                                              :obj #\;))))
                 (c-def-type tt)
                 (format out "(def-c-struct ~s~{~%  ~s~})~%" tt ll)))
              ((string-beg-with-cs "extern \"C\" {" line len)
               (format out ";;; ~a~%" line))
              ((incf numf)  ; function declaration
               (setf (ts-buff ts) line (ts-posn ts) 0)
               (let* ((ll (read-statement ts))
                      (args (mapcar #'c-convert-decl
                                    (nsplit-list (last ll) :obj #\,)))
                      (fun (c-convert-decl (nbutlast ll))))
                 (format out "(def-c-call-out ~s
  (:arguments~{~<~%~12t ~1,79:; ~s~>~})~%  (:return-type~{ ~s~}))~%"
                         (car fun) args (cdr fun)))))))

;(h2lisp *standard-input* *standard-output*)
;(LET ((*readtable* (copy-readtable)))
;  (eval (read-from-string "(read-next ts :skip #'c-cmt-p)")))

(provide :cllib-h2lisp)
;;; file h2lisp.lisp ends here
