;;; Load Emacs-Lisp files into Common Lisp
;;;
;;; Copyright (C) 1999-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: elisp.lisp,v 2.13 2004/12/24 19:35:14 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/elisp.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `make-clos-readtable'
  (require :cllib-closio (translate-logical-pathname "cllib:closio"))
  ;; `from-list'
  (require :cllib-list (translate-logical-pathname "cllib:list")))

(in-package :cllib)

(defpackage emacs-lisp
  #-allegro
  (:documentation "The package for loading Emacs-Lisp code into Common Lisp")
  (:nicknames "ELISP" "EL") (:use "CL" "CLLIB" "PORT")
  #+cmu (:shadowing-import-from #:port port:defstruct)
  (:shadow let let* if member delete load require provide ignore format /
           defcustom defconst autoload))

(defconst +elisp-pack+ package (find-package :el)
  "The Emacs-Lisp package.")

;;;
;;; Emacs-Lisp-specific special forms
;;;

(defmacro el::if (ii tt &rest ee)
  "Emacs-Lisp version of `if' (multiple `else' clauses)."
  (if ee `(if ,ii ,tt (progn ,@ee)) `(if ,ii ,tt)))

(defmacro el::let ((&rest vars) &rest forms)
  "Emacs-Lisp version of `let' (everything special)."
  `(let ,vars (declare (special ,@(mapcar #'from-list vars))) ,@forms))

(defmacro el::let* ((&rest vars) &rest forms)
  "Emacs-Lisp version of `let*' (everything special)."
  `(let* ,vars (declare (special ,@(mapcar #'from-list vars))) ,@forms))

(defmacro el::while (cc &body forms)
  "Emacs-Lisp `while'."
  `(do () ((not ,cc)) ,@forms))

;;;
;;; Read Emacs-Lisp objects
;;;

(eval-when (compile load eval)  ; CMUCL for `+elisp-readtable+'
(defun el::read-elisp-special (stream char)
  (declare (stream stream) (character char))
  (ecase char
    (#\[ (coerce (read-delimited-list #\] stream t) 'vector))
    (#\" (coerce
          (loop :with char-to-add :and next-char
                :for this-char = (read-char stream t nil t)
                :until (char= this-char #\")
                :if (char= this-char #\\)
                :do (setq char-to-add
                          (case (setq next-char (read-char stream t nil t))
                            (#\n #\Newline) (#\r #\Return) (#\f #\Page)
                            (#\t #\Tab) (#\v #\Linefeed) (t next-char)))
                :else :do (setq char-to-add this-char)
                :collect char-to-add)
          'string))
    (#\? (loop :for cc :of-type character = (read-char stream t nil t)
               :collect (if (char/= cc #\\) cc
                            (ecase (read-char stream t nil t)
                              (#\C (read-char stream t nil t) :control)
                              (#\^ :control)
                              (#\S (read-char stream t nil t) :shift)
                              (#\M (read-char stream t nil t) :meta)
                              (#\s (read-char stream t nil t) :super)
                              (#\H (read-char stream t nil t) :hyper)
                              (#\n (setq cc #\Null) #\Newline)
                              (#\t (setq cc #\Null) #\Tab)
                              (#\r (setq cc #\Null) #\Return)))
               :into res
               :finally (return (if (characterp (car res)) (car res) res))
               :while (char= cc #\\)))))

(defun el::make-elisp-readtable ()
  "Make the readtable for Emacs-Lisp parsing."
  (let ((rt (make-clos-readtable)))
    (set-macro-character #\? #'el::read-elisp-special nil rt)
    (set-macro-character #\[ #'el::read-elisp-special nil rt)
    (set-macro-character #\" #'el::read-elisp-special nil rt)
    ;; (setf (readtable-case rt) :downcase)
    rt))
)

(defconst +elisp-readtable+ readtable (el::make-elisp-readtable)
  "The readtable for Emacs-Lisp parsing.")

;;; bug in ACL and CMUCL
#+nil (progn
(defun read-vector (stream char)
  (coerce (read-delimited-list #\] stream t) 'vector))
(set-syntax-from-char #\[ #\()
(set-syntax-from-char #\] #\))
(set-macro-character #\[ #'read-vector nil)
(set-macro-character #\] (get-macro-character #\)) nil)
(read-from-string "[1 2 3]")
(read-from-string "#\\x")
(read-from-string "[#\\x]")
)

;;;
;;; Emacs-Lisp-specific functions
;;;

(defun el::memq (elt list) (member elt list :test #'eq))
(defun el::delq (elt list) (delete elt list :test #'eq))
(defun el::member (elt list) (member elt list :test #'equal))
(defun el::delete (elt list) (delete elt list :test #'equal))
(defun el::concat (&rest args) (apply #'concatenate 'string args))
(defun el::message (&rest args) (apply #'format t args))
(defun el::format (&rest args) (apply #'format nil args))
(defun el::put (symbol propname value) (setf (get symbol propname) value))
(defun el::fset (symbol def)
  (if (null def) (fmakunbound symbol)
      (setf (fdefinition symbol) (if (functionp def) def (fdefinition def)))))
(defun el::setcar (cons obj) (setf (car cons) obj))
(defun el::setcdr (cons obj) (setf (cdr cons) obj))
(defun el::ignore (&rest ignore) (declare (ignore ignore)) nil)
(defun el::sit-for (sec &optional (ms 0) nodisp)
  (declare (real sec ms) (ignore nodisp))
  (sleep (+ sec (/ ms 1000))))
(defun el::string-to-number (string &optional (base 10))
  (if (= base 10)
      (read-from-string string)
      (parse-integer string :radix base)))
(defun el::/ (&rest args)
  (if (every #'integerp args)
      (reduce #'floor (cdr args) :initial-value (car args))
      (apply #'/ args)))

(defun el::decode-time (&optional (time (get-universal-time)))
  (unless (numberp time)
    (setq time (+ (ash (car time) 16)
                  (if (numberp (cdr time)) (cdr time) (cadr time)))))
  (multiple-value-bind (sec minute hour day month year dow dst zone)
      (decode-universal-time time)
    (list sec minute hour day month year (mod (1+ dow) 7) dst (* 3600 zone))))

(defun el::encode-time (second minute hour day month year &rest zone)
  (encode-universal-time second minute hour day month year (car (last zone))))

;; defalias is a function in Emacs, cannot use defmacro
;; a simple (setf fdefinition) is no good
;; since Emacs defalias works on macros too
(defun el::defalias (symbol def)
  (eval `(defmacro ,symbol (&body body) (list* ',def body))))
(defmacro el::defgroup (&rest args) (declare (ignore args)))
(defmacro el::defcustom (var val doc &key (type t) &allow-other-keys)
  (let ((type (if (and (consp type) (eq 'quote (car type))) (cadr type) type)))
    `(progn (declaim (type ,type ,var)) (defvar ,var ,val ,doc))))
(defmacro el::defface (name val doc &rest args)
  (declare (ignore args))
  `(defvar ,name ,val ,doc))
(defmacro el::defconst (name val doc) `(defconstant ,name ,val ,doc))
(defmacro el::eval-when-compile (&rest body)
  `(eval-when (compile load) ,@body))
(defmacro el::setq-default (&rest body) `(setq ,@body))
(defmacro el::save-window-excursion (&rest body) `(progn ,@body))
(defmacro el::save-excursion (&rest body) `(progn ,@body))
(defmacro el::with-output-to-temp-buffer (&rest body) `(progn ,@body))
(defsubst el::number-to-string (number) (write-to-string number :radix 10))
(defsubst el::int-to-string (number) (el::number-to-string number))
(defsubst el::char-to-string (char) (string char))
(defsubst el::string-to-int (&rest args) (apply #'el::string-to-number args))
(defsubst el::file-truename (file) (truename file))
(defsubst el::file-exists-p (file) (probe-file file))
(defsubst el::substring (seq from &optional to) (subseq seq from to))
(defsubst el::% (x y) (rem x y))
(defsubst el::file-directory-p (ff) (probe-directory ff))
(defsubst el::sref (ar ix) (aref ar ix))
(defsubst el::set-default (sy va) (setf (symbol-value sy) va))
(defsubst el::default-value (sy) (symbol-value sy))

(defun el::run-hooks (&rest hooks)
  (labels ((rh (hook)
             (etypecase hook
               (list (dolist (hk hook) (rh hk)))
               ((or symbol function) (funcall hook)))))
    (dolist (hk hooks) (when (boundp hk) (rh (symbol-value hk))))))

(defun el::add-hook (hook function &optional append local)
  (declare (ignore local))
  (let ((val (if (boundp hook) (symbol-value hook) nil)))
    (when (or (functionp val) (symbolp val)) (setq val (list val)))
    (unless (member function val :test #'equal)
      (setf (symbol-value hook)
            (if append (append val (list function))
                (cons function val))))))

(defun el::remove-hook (hook function &optional local)
  (declare (ignore local))
  (when (and (boundp hook) (symbol-value hook) function)
    (let ((val (symbol-value hook)))
      (if (or (functionp val) (symbolp val))
          (when (eq val function) (setf val nil))
          (setf (symbol-value hook) (remove function val :test #'equal))))))

(defun el::add-to-list (list el)
  (unless (boundp list) (setf (symbol-value list) nil))
  (pushnew el (symbol-value list) :test #'equal))

;;; tmp hacks
(defmacro defun-fake (func)
  `(defun ,func (&rest args)
    (warn "~s [~{~s~^ ~}]: not implemented yet" ',func args)
    (values-list args)))
(defun-fake el::define-key)
(defun-fake el::make-sparse-keymap)
(defun-fake el::substitute-key-definition)
(defun-fake el::interactive)
(defun-fake el::make-help-screen)
(defun-fake el::help-for-help)
(defun-fake el::start-kbd-macro)
(defun-fake el::substitute-command-keys)
(defun-fake el::display-color-p)
(defun-fake el::propertize)
(defun-fake el::make-mode-line-mouse-map)

(defvar el::global-map (el::make-sparse-keymap))
(defvar el::help-char (code-char 8))
(defvar el::help-form nil)

(defvar el::window-system nil)
(defvar el::mode-line-format nil)
(defvar el::buffer-read-only nil)
(defvar el::indent-tabs-mode nil)

(deftype el::sexp () 'list)
(deftype el::file () 'string)
(deftype el::hook () '(or list symbol))

(defun el::mapconcat (function sequence separator)
  ;; (el::mapconcat #'identity '("a" "b" "c") " ") ==> "a b c"
  (apply #'concatenate 'string
         (cdr (mapcan (lambda (el) (list separator el))
                      (map 'list function sequence)))))

(defun to-directory (path)
  (if (char= #\/ (schar path (1- (length path)))) path
      (concatenate 'string path "/")))

(defun el::directory-files (directory &optional full match nosort)
  ;; incomplete
  (declare (ignore full nosort))
  (let ((dir (to-directory directory)))
    (directory (if match (merge-pathnames match dir) dir))))

(defun el::expand-file-name (name &optional (default-dir (default-directory)))
  (namestring (merge-pathnames (to-directory default-dir) name)))

(defun el::autoload (function file &optional docstring interactive type)
  (declare (symbol function) (type (or simple-string null) docstring)
           (ignore interactive type))
  (unless (fboundp function)
    (setf (fdefinition function)
          (lambda (&rest args)
            (setf (documentation function 'function) nil)
            (fmakunbound function)
            (format t "; ~s is being autoloaded from `~a'~%" function file)
            (el::load file)
            (apply function args))
          (documentation function 'function)
          (format nil "Autoloaded (from ~a)~@[:~%~a~]" file docstring))))

;;;
;;; Load Emacs-Lisp files
;;;

(defvar el::emacs-home
  #+(or mswindows win32) "d:/gnu/emacs/"
  #-(or mswindows win32) "/usr/local/src/emacs/")
(defvar el::site-lisp-dir
  #+(or mswindows win32) "c:/gnu/sitelisp/"
  #-(or mswindows win32) "/usr/local/share/emacs/site-lisp/")

(defvar el::load-path
  (list (concatenate 'string el::emacs-home "lisp/")
        (concatenate 'string el::emacs-home "leim/")
        el::site-lisp-dir))

(defvar el::features nil)

(defun el::featurep (feature) (el::memq feature el::features))

#+allegro (pushnew "el" sys:*source-file-types* :test #'equal)
#+cmu (pushnew "el" ext::*load-source-types* :test #'equal)
#+clisp (pushnew "el" custom:*source-file-types* :test #'equalp)
#+lispworks (pushnew "el" system:*text-file-types* :test #'equal)
#+gcl (error 'not-implemented :proc 'file-types)

(defun locate-file (file &optional source-only)
  (flet ((file-here (dir)
           ;; search the local files
           (let ((ff (merge-pathnames file dir)))
             (when (and (ignore-errors (probe-file ff))
                        (not (probe-directory ff)))
               (return-from locate-file ff)))
           (unless source-only
             (let ((ff (merge-pathnames (compile-file-pathname file) dir)))
               (when (probe-file ff) (return-from locate-file ff))))
           (let ((ff (merge-pathnames (concatenate 'string file ".el") dir)))
             (when (probe-file ff) (return-from locate-file ff)))))
    (dolist (path el::load-path)
      (let ((dir (to-directory path)))
        (file-here dir)
        ;; search the subdirectories
        (dolist (sub-dir (directory (concatenate 'string dir "*/")))
          (file-here sub-dir))))))

(defun el::load (file &optional noerror nomessage nosuffix must-suffix)
  "Emacs-Lisp load.
The suffix stuff is ignored."
  (declare (ignore nosuffix must-suffix))
  (let ((*readtable* +elisp-readtable+) (*package* +elisp-pack+)
        (ff (locate-file file)))
    (if ff (load ff :verbose (not nomessage))
        (unless noerror (error "file `~a' not found" file)))))

(defun el::require (feature &optional file-name noerror)
  "Emacs-Lisp require"
  (unless (el::featurep feature)
    (let ((file (or file-name (string-downcase (string feature)))))
      (el::load file noerror)
      (assert (el::featurep feature) ()
              "loading `~a' failed to provide `~a'" file feature)))
  feature)

(defun compile-el-file (file)
  (let ((*readtable* +elisp-readtable+) (*package* +elisp-pack+)
        (ff (or (locate-file file t) file)))
    (compile-file ff)))

(defun el::provide (feature) (pushnew feature el::features))

;; (in-package :cl-user)
;; (in-package :emacs-lisp)

;; (el::load "backquote")
;; (el::load "calendar")
;; (el::load "cal-hebrew")
;; (el::load "subr")
;; (el::load "help")
;; (cllib::locate-file "calendar")
;; (cllib::compile-el-file "backquote")
;; (cllib::compile-el-file "calendar")
;; (cllib::compile-el-file "cal-hebrew")
;; (el::calendar-hebrew-date-string)

(provide :cllib-elisp)
;;; file elisp.lisp ends here
