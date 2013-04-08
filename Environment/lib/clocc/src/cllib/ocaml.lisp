;;; Read Ocaml Sexp
;;; http://www.ocaml.info/home/ocaml_sources.html#toc8
;;; http://www.janestcapital.com/ocaml/index.html
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: ocaml.lisp,v 2.5 2006/11/30 02:35:11 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/ocaml.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  (require :port-mop (translate-logical-pathname "port:mop")))

(in-package :cllib)

(export '(def-sexp-to fix-slot))

;;;
;;; sexp parsing
;;;

(defmacro def-sexp-to (class-name)
  "Define a function `sexp-to-...'."
  (let* ((class (find-class class-name))
         (slots (port:class-slots class)))
    (labels ((parser-name (name) (intern (format nil "SEXP-TO-~S" name)))
             (parser-form (name arg)
               (let ((parser (parser-name name)))
                 (if (fboundp parser) `(,parser ,arg) `(the ,name ,arg))))
             (parser-function (name)
               (let ((parser (parser-name name)))
                 (if (fboundp parser) (fdefinition parser) #'identity)))
             (parser-setter (type place)
               (if (symbolp type)
                   (parser-form type place)
                   (ecase (first type)
                     (array `(map 'vector ,(parser-function (second type))
                                  ,place))))))
      `(defun ,(parser-name class-name) (sexp)
         (let ((ret (make-instance ',class-name)))
           (dolist (pair sexp ret)
             (ecase (car pair)
               ,@(mapcar
                  (lambda (slot &aux (name (port:slot-definition-name slot)))
                    `(,name
                      (setf (slot-value ret ',name)
                            ,(parser-setter (port:slot-definition-type slot)
                                            '(second pair)))))
                  slots))))))))

(defun fix-slot (name line &key (start 0) (end (length line)))
  "Convert a `list' representation to a `string' one;
\(NAME(.....)) --> (NAME\".....\")"
  (loop :with len = (length name)
    :for pos = (search name line :start2 start)
    :while (and pos (< 0 pos (- end len 1))
                (char= #\( (char line (1- pos))) ; must be '(foo('
                (char= #\( (char line (+ pos len)))) :do
    (incf pos len)
    (setf (aref line pos) #\"
          pos (position #\) line :start pos)
          (aref line pos) #\"
          start pos))
  line)

(provide :ocaml)
;;; file ocaml.lisp ends here
