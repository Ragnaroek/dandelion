;;; bugfixes
;;;
;;; Copyright (C) 1997-2004 by Sam Steingold.
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: clocc.lisp,v 1.23 2004/05/09 21:03:40 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/clocc.lisp,v $

(in-package :cl-user)

;;;
;;; fix some bugs
;;;

#+allegro
(setq
 ;; All code here is supposed to be ANSI CL compliant,
 ;; so there is no use in this annoying warning
 comp:*cltl1-compile-file-toplevel-compatibility-p* nil
 ;; this is necessary for (require "foo") to load "foo.fasl"
 ;; Kevin Layer (layer@franz.com) [2003-04-16 15:26:42 PST]
 ;; <http://groups.google.com/groups?hl=en&ie=UTF-8&th=be07905ac53b210d>
 sys:*require-search-list*
 (append sys:*load-search-list* sys:*require-search-list*)
 ;; Duane Rettig <duane@franz.com>:
 ;; TIME reports 32 other bytes too many CL unless tuned with
 excl::time-other-base 32)
;; From Erik Naggum <erik@naggum.no> [22 Feb 1999 10:27:45 +0000]
;; fixes the (read-from-string "[#\\x]") problem
#+allegro-off
(loop with readtables = (excl::get-objects 11)
      for i from 1 to (aref readtables 0)
      for readtable = (aref readtables i) do
      (when (excl::readtable-dispatch-tables readtable)
        ;; reader for character names immune cltl1
        (set-dispatch-macro-character
         #\# #\\
         (excl::named-function
          excl::sharp-backslash
          (lambda (stream backslash font)
            (declare (ignore font))
            (unread-char backslash stream)
            (let* ((charstring (excl::read-extended-token stream)))
              (unless *read-suppress*
                (or
                 ;; the original code by Erik
                 #+(and allegro (not (version>= 6)))
                 (or (character charstring)
                     (name-char charstring))
                 ;; From: cox@franz.com (Charles A. Cox)
                 ;; Subject: Re: [spr22970]
                 ;; Date: Thu, 16 Nov 2000 18:43:13 -0800
                 #+(and allegro (version>= 6))
                 (if (= 1 (length charstring))
                     (char charstring 0)
                     (name-char charstring))
                 (excl::internal-reader-error
                  stream "Meaningless character name ~A"
                  (string-upcase charstring)))))))
         readtable)))
#+allegro-v4.3                  ; From Erik Naggum <erik@naggum.no>
(unless (member :key (excl:arglist #'reduce) :test #'string=)
  (setq excl:*compile-advice* t)
  (excl:defadvice reduce (support-key :before)
    (let ((key (getf (cddr excl:arglist) :key)))
      (when key
        (remf (cddr excl:arglist) :key)
        (setf (second excl:arglist)
              (map 'vector key (second excl:arglist)))))))

#+clisp
(without-package-lock ("CLOS")
  (setq clos::*warn-if-gf-already-called* nil
        clos::*gf-warn-on-replacing-method* nil))

#+cmu
(progn
  (pushnew 'compile pcl::*defclass-times*)
  (pushnew 'compile pcl::*defgeneric-times*)
  (pushnew 'compile pcl::*defmethod-times*))

#+cmu (setq ext:*gc-verbose* nil)

(eval-when (compile eval)
  (let (x y)
    (handler-case
        (progn
          (setf (values x y) (values 1 2))
          (unless (and (= x 1) (= y 2))
            (pushnew :broken-setf-values *features*)))
      (error () (pushnew :broken-setf-values *features*)))))
#+broken-setf-values
(define-setf-expander values (&rest places &environment env)
  (loop :for pl :in places :with te :and va :and ne :and se :and ge :do
        (multiple-value-setq (te va ne se ge) (get-setf-expansion pl env))
        :append te :into te1 :append va :into va1 :append ne :into ne1
        :collect se :into se1 :collect ge :into ge1
        :finally (return (values te1 va1 ne1 (cons 'values se1)
                                 (cons 'values ge1)))))

#+mcl
(import '(ccl:provide) :common-lisp)

;;;
;;; Path
;;;

(defvar *clocc-root*
  #-(or win32 winnt mswindows cygwin) "/usr/local/src/clocc/"
  #+(or win32 winnt mswindows cygwin) "d:/gnu/clocc/"
  "*The root CLOCC directory.")

(setf (logical-pathname-translations "clocc")
      `(("src;defsystem;*"
         ,(concatenate 'string *clocc-root* "src/defsystem-3.x/*"))
        ("src;defsystem;*.*"
         ,(concatenate 'string *clocc-root* "src/defsystem-3.x/*.*"))
        ("src;defsystem-3-x;*"
         ,(concatenate 'string *clocc-root* "src/defsystem-3.x/*"))
        ("src;defsystem-3-x;*.*"
         ,(concatenate 'string *clocc-root* "src/defsystem-3.x/*.*"))
        ("**;*" ,(concatenate 'string *clocc-root* "**/*"))
        ("**;*.*" ,(concatenate 'string *clocc-root* "**/*.*"))))

(provide :clocc-top)

;;; clocc.lisp ends here
