;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


;;;----------------------------------------------------------------------------------+
;;;                                                                                  |
;;;                          TEXAS INSTRUMENTS INCORPORATED                          |
;;;                                   P.O. BOX 149149                                |
;;;                                AUSTIN, TEXAS 78714-9149                          |
;;;                                                                                  |
;;;          Copyright (C) 1987, 1988,1989,1990 Texas Instruments Incorporated.      |
;;;                                                                                  |
;;; Permission is granted to any individual or institution to use, copy, modify, and |
;;; distribute this software, provided that  this complete copyright and  permission |
;;; notice is maintained, intact, in all copies and supporting documentation.        |
;;;                                                                                  |
;;; Texas Instruments Incorporated provides this software "as is" without express or |
;;; implied warranty.                                                                |
;;;                                                                                  |
;;;----------------------------------------------------------------------------------+


(in-package "USER")

#-lispm
(progn						; SITE DEPENDENT
  ;; NOTE: All pathname strings must end in /*
  (defvar *clx-directory*  "/usr/X11/lib/CLX/*")
  (defvar *clos-kludge-directory*)
  (defvar *clue-directory* nil)
  (defvar *clue-examples-directory* nil)
  (defvar *clue-demo-directory* nil)
  )
#+lispm
(progn						; Lispm's have logical pathnames
  (defvar *clx-directory*  "clx:clx;")
  (defvar *clue-directory* "clue:clue;")
  (defvar *clos-kludge-directory*  "clue:clos-kludge;")
  (defvar *clue-examples-directory* "clue:examples;")
  (defvar *clue-demo-directory* "clue:examples.old.demo;")
  )

#+comment
;; Here's a recommended set of LISPM logical pathname translations:
(fs:set-logical-pathname-host "CLUE" :physical-host si:local-host
			      :translations 
			      '(("CLUE" "CLUE;")
				("EXAMPLES" "CLUE.EXAMPLES;")
				("DOC" "CLUE.DOC;")
				("CLOS-KLUDGE" "CLUE.CLOS-KLUDGE;")
				))

;; Ensure VALUES is a legal declaration
(proclaim '(declaration values))

;; Ensure *features* knows about CLOS and PCL
(when (find-package 'pcl)
  (pushnew :pcl  *features*)
  (pushnew :clos *features*))

(when (find-package 'clos)
  (pushnew :clos *features*))

;; Ensure *features* knows about the Common Lisp Error Handler
(when (find-package 'conditions)
  (pushnew :cleh *features*))

;;;-----------------------------------------------------------------------------
;;; DEFSYSTEM forms, to make things easy for lispm users

#+explorer
(defsystem clue
  (:pathname-default "clue:clue;")
  (:warnings-pathname-default "sys:cwarns;clue.lisp")
  (:patchable "sys:patch.clue;")
  (:initial-status :experimental)

  (:module clue        "clue")
  (:module clx-macros  ("clx:clx;macros" "clx:clx;bufmac"))
  (:module clx-patch   ("clx-patch" "window-doc"))
  (:module defcontact  "defcontact")
  (:module events      "events")
  (:module event-parse "event-parse")
  (:module intrinsics  "intrinsics")
  (:module package     "package")
  (:module resource    ("resource" "gray" "cursor"))
  (:module root-gmgmt  "root-gmgmt")
  (:module shells      "shells")
  (:module stream      "stream")
  (:module virtual     "virtual")
  (:module caches      "caches" )
  (:module obsolete    "obsolete")
  
  (:module extras      ("clos-patch" "defsys" "pcl-fixes"
		       "sys:site;clue.system" "sys:site;clue.translations"))  

  (:auxiliary extras)  

  (:compile-load clue)
  (:skip :fasload clx-macros)
  (:compile-load clx-patch
		 (:fasload clue clx-macros)) 
  (:compile-load event-parse
		 (:fasload clue))
  (:compile-load defcontact
		 (:fasload clue clx-patch event-parse))
  (:compile-load intrinsics
		 (:fasload clue clx-patch defcontact event-parse))
  (:compile-load caches
		 (:fasload clue intrinsics))
  (:compile-load resource
		 (:fasload clue defcontact intrinsics caches))
  (:compile-load events
		 (:fasload clue clx-patch defcontact intrinsics event-parse))
  (:compile-load virtual
		 (:fasload clue defcontact intrinsics resource events)) 
  (:compile-load shells
		 (:fasload clue defcontact intrinsics resource events))
  (:compile-load stream
		 (:fasload clue defcontact intrinsics resource events))
  (:compile-load root-gmgmt
		 (:fasload clue defcontact intrinsics events shells))
  (:compile-load package
		 (:fasload
		   clue defcontact events intrinsics resource root-gmgmt
		   shells stream virtual caches event-parse))
  (:compile-load obsolete
		 (:fasload package)))		   

#+symbolics
(defsystem clue
  (:default-pathname "clue:clue;"
   :pretty-name "CLUE"
   :distribute-binaries t
   :initial-status :experimental
   :bug-reports ("clue-bugs@dsg.csc.ti.com" "Report problems with CLUE.")
   )
  (:module clue ("clue"))
  (:module clx-macros ("clx:clx;macros" "clx:clx;bufmac")
	   (:root-module nil))
  (:module clx-patch ("clx-patch" "window-doc")
	   (:uses-definitions-from clue)
	   (:uses-definitions-from clx-macros))
  (:serial clue clx-patch "event-parse" "defcontact" "intrinsics" "caches" "resource" "gray" "cursor"
	   "events" "virtual" "shells" "stream" "root-gmgmt" "package"
	   ;; "button" "menu" ;; Moved to Examples
	   )
  )

;;;-----------------------------------------------------------------------------
;;; Simple lisp make facility

(defvar *source-binary-extension-alist*
	(or (car
	      '(#+symbolics                         ("lisp"  "bin")
		#+(and dec common vax (not ultrix)) ("LSP"   "FAS")
		#+(and dec common vax ultrix)       ("lsp"   "fas")
		#+kcl                               ("lsp"   "o")
		#+xerox                             ("lisp"  "dfasl")
		#+(and lucid mc68000)               ("lisp"  "lbin")
		#+(and lucid vax vms)               ("lisp"  "vbin")
		#+(and lucid prime)                 ("lisp"  "pbin")
		#+(and lucid sparc)                 ("lisp"  "sbin")
		#+(and lucid sunrise)               ("lisp"  "lbin")
		#+(and lucid ibm-rt-pc)             ("lisp"  "bbin")
		#+(and excl allegro)                ("lisp"  "fasl")
		#+(and excl (not allegro))          ("cl"    "fasl")
		#+:cmu                              ("slisp" "sfasl")
		#+hp                                ("l"     "b")
		#+explorer ("lisp" #.(string (si::local-binary-file-type)))
		#+:gclisp                           ("LSP"   "F2S")
		#+pyramid                           ("clisp" "o")
		#+:coral                            ("lisp"  "fasl")
		))
	    '("l" "lbin")))

(defun compile-load (file &optional option)
  "Compile file when needed, then load it.
 Recompile when OPTION is :RECOMPILE, load-only when OPTION is LOAD."
  (declare (type (or string pathname) file))
  (check-type option (or null (member :load :compile :recompile)))
  (labels ((make-path (file type)
	     (make-pathname
	       :type (ecase type
		       (:default "l")
		       (:source (car *source-binary-extension-alist*))
		       (:binary (cadr *source-binary-extension-alist*)))
	       :defaults file)))

    (let* ((path (parse-namestring file))
	   (source (make-path path :source))
	   (binary (make-path path :binary)))
      (declare (type pathname path source binary))
      (unless (probe-file source)
	(setq source (make-path file :default)))
      (when (and (not (eq option :load))
		 (or (eq option :recompile)
		     (not (probe-file binary))
		     (> (or (file-write-date source) 1)
			(or (file-write-date binary) 0))))
	(format t "~&; Compiling ~A" (namestring source))
	(compile-file source))
	#+explorer(si:load-if binary :verbose t)			; Load file only if needed.
	#-explorer(format t "~&; Loading   ~A" (namestring binary))	; Is there a way to do this
	#-explorer(load binary :verbose nil)				; with other systems?
	)))

(defun directory-append (pathname sub-directory)
  ;; Return PATHNAME with sub-directory appended to its directory list.
  (declare (type (or string pathname) pathname)
	   (type string sub-directory))
  ;; This assumes that #'pathname-directory returns a list of sub-directory strings
  (make-pathname
    :defaults pathname
    :directory (append (pathname-directory pathname) (list sub-directory))))

;;;-----------------------------------------------------------------------------
;;; Compile/Load CLUE

(defun compile-clue (&key (option :compile)
		     (clue (or *clue-directory* *default-pathname-defaults*))
		     (clx *clx-directory*)
		     clos-kludge)
  ;; Load CLUE, optionally compiling changed files.
  ;; If OPTION is :RECOMPILE, recompile all files
  ;; If OPTION is :LOAD, don't compile anything, just load.
  ;; WARNING: CLX (and CLOS) MUST BE LOADED FIRST!!!
  (declare (type (or string pathname) clue clx)
	   (type (or null string pathname) clos-kludge)
	   (type (or null (member :load :compile :recompile)) option))
  (setq *clue-directory* clue			; Set defaults for the next time
	*clx-directory*  clx)
  (when clos-kludge (setq *clos-kludge-directory* clos-kludge))
  (flet ((module (file &optional opt dir)
	   (compile-load (merge-pathnames file (or dir clue)) (or opt option))))

    ;; ensure CLX is loaded
    (unless (find-package 'xlib)
      (compile-clos-clx :option option))

    ;; ensure CLOS is loaded
    (unless (member :clos *features*)
      ;; No CLOS, load clos-kludge
      (unless (boundp '*clos-kludge-directory*)
	;; Build a pathname to the clos-kludge directory
	(setq *clos-kludge-directory* (directory-append clue "clos-kludge")))
      (module "defsystem" option *clos-kludge-directory*)
      (compile-clos-kludge option))

    ;; These CLX files must be loaded to compile CLUE
    (unless (eq option :load)
      (module "macros" :load clx)
      (module "bufmac" :load clx))

    (module "clue")		;; Define packages
    (module "clx-patch")	;; Modify xlib:create-window
    (module "window-doc")	;; pointer documentation window support
    (module "event-parse")	;; Utilities for event translation
    (module "defcontact")	;; CLOS extension for resources and type conversion
    (module "intrinsics")	;; The "guts"
    (module "caches")		;; Support for gcontext, pixmap, cursor cacheing
    (module "resource")		;; Resource and type conversion
    (module "gray")		;; Gray stipple patterns
    (module "cursor")		;; Standard cursor names
    (module "events")		;; Event handling
    (module "virtual")		;; Support for windowless contacts
    (module "shells")		;; Support for top-level window/session mgr interaction
    (module "root-gmgmt")	;; Geometry management methods for root contacts
;;  (module "stream")		;; interactive-stream (non-portable!!)
    (module "package")		;; External cluei symbols exported from clue
    ))

(defun load-clue (&rest options)
  ;; Load CLUE
  ;; WARNING: CLX MUST BE LOADED FIRST!!!
  (apply #'compile-clue :option :load options))

(defun compile-clue-all (&rest options &key option &allow-other-keys)
  ;; Compile CLUE, clue-examples and clue-demo
  (apply #'compile-clue options)
  (unless *clue-examples-directory*
    (setq *clue-examples-directory* (directory-append *clue-directory* "examples")))
  (load (merge-pathnames "defsystem" *clue-examples-directory*))
  (compile-clue-examples option)
  (unless *clue-demo-directory*
    (setq *clue-demo-directory* (directory-append *clue-directory* "demo")))
  (load (merge-pathnames "defsystem" *clue-demo-directory*))
  (compile-clue-demo option))

;;;-----------------------------------------------------------------------------
;;; Compile/Load CLX with CLOS patches

(defun compile-clos-clx (&key (option :compile)
			 (clue *clue-directory*)
			 (clx *clx-directory*))
  ;; Load CLX, optionally compiling changed files.
  ;; If OPTION is :RECOMPILE, recompile all files
  ;; If OPTION is :LOAD, don't compile anything, just load.
  (declare (type (or string pathname) clue clx)
	   (type (or null (member :load :compile :recompile)) option))
  (setq *clue-directory* clue			; Set defaults for the next time
	*clx-directory*  clx)
  (flet ((module (file &optional opt dir)
		 (compile-load (merge-pathnames file (or dir clx)) (or opt option))))
    
    #+lucid
    (progn
      (module "make-sequence-patch")
      (clx-foreign-files))
    #+kcl
    (module "tcp/tcpinit")
    #+excl
    (module "excldep")
    (module "depdefs")
    (module "clx")
    (module "dependent")
    (when (member :clos *features*)		; Patch CLX to use CLOS defclass
      (module "clos-patch" option clue))	; (unless using clos-kludge)
    (unless (eq option :load)
      (module "macros")
      (module "bufmac"))
    (module "buffer")
    (module "display")
    (module "gcontext")
    (module "requests")
    (module "input")
    (module "fonts")
    (module "graphics")
    (module "text")
    (module "attributes")
    (module "translate")
    (module "keysyms")
    (module "manager")
    (module "image")
    (module "resource")
    ))

(defun load-clos-clx (&rest options)
  ;; Load CLX
  (apply #'compile-clos-clx :option :load options))

#+lucid
(defvar *foreign-libraries* '("-lc")) ; '("-lresolv" "-lc") for some sites

#+lucid
(defun clx-foreign-files ()
  (define-c-function (xlib::connect-to-server "_connect_to_server")
		     (host display)
		     :result-type :integer)
  (unintern 'display)
  (load-foreign-files '("socket.o") *foreign-libraries*))

;; End of file

