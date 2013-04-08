;;; generate and use autoloads
;;;
;;; Copyright (C) 2000-2003, 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: autoload.lisp,v 1.14 2006/08/29 00:13:48 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/autoload.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `skip-to-line', `timestamp'
  (require :cllib-fileio (translate-logical-pathname "cllib:fileio")))

(in-package :cllib)

(export '(autoload autoload-generate *autoload-cookie*))

;;;
;;; autoload
;;;

;; this will be written into `auto.lisp'
(defparameter *autoload-defun* '|
(defun autoload (symb file &optional comment)
  "Declare symbol SYMB to call a function defined in FILE."
  (declare (symbol symb) (type (or simple-string null) comment))
  (export (list symb))
  (unless (fboundp symb)
    (let ((path (translate-logical-pathname
                 (concatenate 'string "cllib:" file))))
      (setf (fdefinition symb)
            (lambda (&rest args)
              (setf (documentation symb 'function) nil)
              (fmakunbound symb)
              (format t "; ~s is being autoloaded from `~a'~%" symb file)
              (require file path)
              (apply (fdefinition symb) args))
            #+clisp (documentation symb 'sys::file) #+clisp path
            (documentation symb 'function)
            (format nil "Autoloaded (from ~a)~@[:~%~a~]" file comment)))))

|)

(defcustom *autoload-cookie* simple-string ";;;###autoload"
  "*The cookie preceeding a function to be autoloaded.")

(defgeneric autoload-stream (in out log)
  (:documentation "Generate the autoloads writing into a stream.")
  (:method ((in string) (out stream) (log t))
    (autoload-stream (pathname in) out log))
  (:method ((in sequence) (out stream) (log t))
    (reduce #'+ in :key (lambda (i0) (autoload-stream i0 out log))))
  (:method ((in pathname) (out stream) (log t))
    (with-open-file (ins in :direction :input)
      (format out "~&;;; file ~s, ~:d bytes~%" in (file-length ins))
      (format log "~&~s [~:d bytes] --> " in (file-length ins)) (force-output)
      (loop :while (ignore-errors (skip-to-line ins *autoload-cookie*))
            :count t :into total :do
            (let* ((*package* (find-package :cllib))
                   (form (read ins)) (name (second form))
                   (doc (case (car form)
                          ((defun defsubst)
                           (when (stringp (fourth form)) (fourth form)))
                          (defgeneric
                           (cadr (assoc :documentation (cdddr form))))
                          (t (error 'case-error :proc 'autoload-stream :args
                                    (list 'form (car form) 'defun 'defsubst
                                          'defgeneric))))))
              (format out "(export '(~s))~%(autoload '~s ~s ~s)~%"
                      name name (pathname-name in) doc))
            :finally (progn (format log "~d autoload~:p~%" total) (terpri out)
                            (return total))))))

(defun autoload-generate (in out &optional (log t))
  "Generate the autoloads, indicated by `*autoload-cookie*'."
  (with-open-file (outs out :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
    (format outs ";;; autoloads generated on ~a~%;;; by ~a [~a]~2%~
                  (in-package :cllib)~%(export '(autoload))~2%~a~2%"
            (timestamp) (lisp-implementation-type)
            (lisp-implementation-version) *autoload-defun*)
    (let ((tot (autoload-stream in outs log)))
      (format log "wrote ~d autoload~:p to ~s (~:d bytes)~%"
              tot out (file-length outs))
      tot)))

(provide :cllib-autoload)
;;; file autoload.lisp ends here
