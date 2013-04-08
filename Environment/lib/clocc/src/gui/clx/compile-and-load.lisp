;;; -*- Mode: Lisp; Package: USER; Base: 10; Syntax: Common-Lisp -*-

(in-package "COMMON-LISP-USER")

(pushnew :clx-debugging *features*)


;;; Aid function:

(defun comf (file)
  (let ((output-file
         (compile-file
          (merge-pathnames
           (pathname file)
           *load-pathname*))))
    (load output-file)))
   
(defvar *clocc-root*
  (pathname-directory *load-pathname*))

(setf (logical-pathname-translations "clocc")
      `(("src;port;sys;**;*"  "**/*")
        ("**;*.*" "**/*.*")))
;;; First compile and load port:

(comf (make-pathname :directory '(:relative "clocc-port") :name "ext"))
(comf (make-pathname :directory '(:relative "clocc-port") :name "gray"))
(comf (make-pathname :directory '(:relative "clocc-port") :name "path"))
(comf (make-pathname :directory '(:relative "clocc-port") :name "sys"))
(comf (make-pathname :directory '(:relative "clocc-port") :name "net"))
(comf (make-pathname :directory '(:relative "clocc-port") :name "proc"))


;;; Then split-sequence

(comf (make-pathname :directory '(:relative "cclan") :name "split-sequence"))

;;; Then compile and load the true system:

(dolist (file (list 
               "package"
               "depdefs"
               "clx"
               "dependent"
               "macros"				; these are just macros
               "bufmac"				; these are just macros
               "buffer"
               "display"
               "gcontext"
               "input"
               "requests"
               "fonts"
               "graphics"
               "text"
               "attributes"
               "translate"
               "keysyms"
               "manager"
               "image"
               "resource"))
  (comf file))

         

