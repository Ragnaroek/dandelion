;;; parse command line options
;;; this is useful for lisp scripting
;;;
;;; Copyright (C) 2000-2001 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: getopt.lisp,v 2.4 2005/01/27 23:02:49 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/getopt.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `kwd'
  (require :cllib-symb (translate-logical-pathname "cllib:symb")))

(in-package :cllib)

(export '(getopt))

(defun parse-object (type string name)
  "Parse the STRING argument named NAME according to the TYPE."
  (case type
    (:char string)
    (:number
     (let ((res (ignore-errors (read-from-string string))))
       (unless (numberp res)
         (error "argument for ~s must be a number, not ~s~%"
                name string))
       res))
    (:boolean (ignore-errors (read-from-string string)))
    (t (error "invalid type (~s) for argument ~s~%" type name))))

;;;###autoload
(defun getopt (arg-alist opt-alist arg-list &key (allow-less t) (allow-more t))
  "Parse the command line arguments.
`arg-alist' and `opt-alist' are alists of (option-name . argument-type),
`argument-type' should be `:char', `:number', or `:boolean',
`arg-list' is a list of options, like (\"-opt1\" \"val1\" ...)
Returns two values -
   a list of non-option arguments,
   and a plist of keyword args;
 or T if there was an error.
When `allow-less' (`allow-more') is non-NIL (default),
allow missing (additional) non-option arguments."
  (do ((ll arg-list (cdr ll)) (aal arg-alist)
       non-opt opts)
      ((null ll)
       (when (and aal (not allow-less))
         (format t "too few non-option arguments (must be at least ~d)~%"
                 (length arg-alist))
         (return t))
       (values (nreverse non-opt) (nreverse opts)))
    (handler-case
        (if (char/= #\- (char (car ll) 0))
            (let ((ar (pop aal)))
              (when (and (null ar) (not allow-more))
                (format
                 t "too many non-option arguments (must be no more than ~d)~%"
                 (length arg-alist))
                (return t))
              (push (parse-object (cdr ar) (car ll) (car ar)) non-opt))
            (let* ((cur (pop ll)) (kw (kwd (nstring-upcase (subseq cur 1))))
                   (opt (assoc kw opt-alist :test #'eq)))
              (unless opt
                (format t "unknown option: ~s~%" cur)
                (return t))
              (push kw opts)
              (push (parse-object (cdr opt) (car ll) cur) opts)))
      (error (co)
        (format t "~a~%" co)
        (return t)))))

(provide :cllib-getopt)
;;; file getopt.lisp ends here
