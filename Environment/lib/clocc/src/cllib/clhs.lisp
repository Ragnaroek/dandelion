;;; HyperSpec handling
;;;
;;; Copyright (C) 1999-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: clhs.lisp,v 3.9 2005/12/06 20:55:19 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/clhs.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `string-beg-with', `substitute-subseq'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; `skip-search'
  (require :cllib-fileio (translate-logical-pathname "cllib:fileio"))
  ;; `with-timing'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `xmlize-string'
  (require :cllib-xml (translate-logical-pathname "cllib:xml"))
  ;; `html-translate-specials'
  (require :cllib-html (translate-logical-pathname "cllib:html")))

(in-package :cllib)

(export '(*clhs-root* *clhs-hashtable* clhs-doc clhs-write-entities))

#+nil
(setq gtki::*gtkd-executable* "/usr/src/clisp/cl-gtk/bin/gtkd")

;;;
;;;
;;;

(defcustom *clhs-root* url (url "http://www.lisp.org/HyperSpec/")
  "The root of the HyperSpec tree.")

(defcustom *clhs-root-local* pathname
  (parse-namestring "/usr/share/doc/HyperSpec/")
  "The root of the local HyperSpec tree.")

(defun clhs-snarf-examples (&key (root *clhs-root-local*)
                            (out *standard-output*))
  "Get the examples from the HyperSpec."
  (declare (pathname root) (stream out))
  (format t " *** processing `~a'~%" root)
  (dolist (fl (directory (merge-pathnames "*.html" root)))
    (with-open-file (ff fl)
      (unless (or (null (skip-search ff "<P><B>Examples:</B><P>"))
                  (null (skip-search ff "<PRE>")))
        (format out " +++ `~a'~%" fl)
        (do ((st (read-line ff nil nil) (read-line ff nil nil)))
            ((or (null st) (string= st "</PRE>")))
          (princ (html-translate-specials st) out) (terpri out)))))
  (dolist (dir (directory (merge-pathnames "*/" root)))
    (clhs-snarf-examples :root dir :out out)))

(defstruct clhs-version
  (name (required-argument))
  (sym-tab (required-argument) :type string) ; file in Data/ symbol->file
  (iss-tab (required-argument) :type string) ; file in Data/ issue->file
  (any (required-argument) :type string) ; "any" file prefix: choice
  (fun (required-argument) :type list) ; FUNCTION prefixes
  (mac (required-argument) :type list) ; MACRO prefixes
  (spe (required-argument) :type list) ; SPECIAL-OPERATOR & SYMBOL prefixes
  (typ (required-argument) :type list) ; TYPE prefixes
  (var (required-argument) :type list) ; VARIABLE prefixes
  (dec (required-argument) :type list) ; DECLARE prefixes
  (res (required-argument) :type list) ; RESTART prefixes
  (glo (required-argument) :type list) ; GLOSSARY prefixes
  (doc (required-argument) :type string)) ; file name for DOCUMENTATION

(defcustom *clhs-version-table* list
  (list (make-clhs-version
         :name :long :sym-tab "Symbol-Table.text" :any "any"
         :iss-tab "Issue-Cross-Refs.text"
         :doc "stagenfun_doc_umentationcp.html"
         :fun '("acc" "fun" "locfun" "stagenfun")
         :spe '("sym" "spefor" "speope") :mac '("locmac" "mac")
         :typ '("cla" "contyp" "syscla" "typ" "typspe")
         :var '( "convar" "var") :dec '("dec") :res '("res") :glo '("glo"))
        (make-clhs-version
         :name :short :sym-tab "Map_Sym.txt" :any "a" :iss-tab "Map_IssX.txt"
         :doc "f_docume.htm" :fun '("f") :mac '("m") :spe '( "s")
         :typ '("t" "e") :var '("v") :dec '("d") :res '("r") :glo '("26")))
  "*The list of known CLHS versions.")

(defparameter *clhs-alist* nil)
(defparameter *clhs-issues* nil)
(defparameter *clhs-version* nil)

(defun clhs-read-map (map root old-path ver err)
  (declare (stream map))
  (loop :with rec
     :for sym = (read-line map nil nil)
     :for file = (read-line map nil nil)
     :while (and sym file) :do
     (setq rec (list sym (subseq file #.(length "../Body/"))))
     :collect rec
     :when (string-beg-with (clhs-version-any ver) (second rec))
     :do ; get all the options for the symbol
     (setf (url-path root) (concatenate 'string old-path "Body/" (second rec)))
     (mesg :log err "expanding the choices for ~s" (first rec))
     (with-open-url (any root :err err)
       (case (url-prot root) ((:http :www) (flush-http any)))
       (loop :for line = (read-line any)
          :until (string-beg-with "Please select which reference to" line))
       (read-line any) ; skip <ul>
       (setf (cddr rec)
             (loop :with line = (read-line any) :with pos2 = 0
                :for pos1 = (search "<li><a HREF=\"" line
                                    :start2 pos2 :test #'char-equal)
                :while pos1 :do
                (incf pos1 #.(length "<li><a HREF=\""))
                (setq pos2 (position #\" line :test #'char= :start pos1))
                :collect (subseq line pos1 pos2))))
     (mesg :log err "~s~%" rec)))

(defun clhs-read-issues (root err)
  (declare (type url root))
  (with-open-url (map root :err err)
    (case (url-prot root) ((:http :www) (flush-http map)))
    (loop :for iss = (read-line map nil nil) :for file = (read-line map nil nil)
      :while (and iss file) :collect (list iss (subseq file #.(length ".."))))))

(defcustom *clhs-hashtable* (or null hash-table) nil
  "The hashtable for the CL symbols.")

(defun clhs-init (&key (root *clhs-root*) (err *error-output*))
  "Set `*clhs-alist*', `*clhs-hashtable*' and `*clhs-version*' from ROOT."
  ;; make sure root ends with "/"
  (unless (char= #\/ (aref (url-path root) (1- (length (url-path root)))))
    (setf (url-path root) (concatenate 'string (url-path root) "/")))
  (dolist (ver *clhs-version-table*)
    ;; check all versions one by one
    (let ((old-path (url-path root)))
      (setf (url-path root)
            (concatenate 'string old-path "Data/" (clhs-version-sym-tab ver)))
      (mesg :log err "~& *** ~s~%" root)
      (unwind-protect           ; restore the PATH of ROOT
           (handler-case        ; ignore file opening errors
               (with-open-url (map root :err err)
                 (case (url-prot root) ((:http :www) (flush-http map)))
                 (setf *clhs-version* ver
                       *clhs-alist* (clhs-read-map map root old-path ver err))
                 (mesg :log err "~&read ~:d symbol~:p" (length *clhs-alist*))
                 (setf (url-path root)
                       (concatenate 'string old-path "Data/"
                                    (clhs-version-iss-tab ver))
                       *clhs-issues* (clhs-read-issues root err))
                 (mesg :log err "~&read ~:d issue~:p" (length *clhs-issues*))
                 (return *clhs-version*))
             ((or code login net-path file-error) (co)
               ;; ignore the file opening errors
               (mesg :log err "~s: failed: ~a" root co)))
        (setf (url-path root) old-path))))
  (mesg :log err "~&filling up ~s~%" '*clhs-hashtable*)
  (setq *clhs-hashtable* (make-hash-table :test #'equal :size 1000))
  (dolist (el *clhs-alist*)
    (multiple-value-bind (re fp) (gethash (car el) *clhs-hashtable*)
      (assert (null fp) () "double record for ~s:~%  ~a~%  ~a~%"
              (car el) re (cdr el))
      (setf (gethash (car el) *clhs-hashtable*) (cdr el))))
  ;; return CLHS version
  *clhs-version*)

(defun clhs-write-entity (name html out)
  (let ((ent (or (cdr (assoc name
                             '(("=" . "areq") ("/=" . "arneq") ("<" . "lst")
                               (">" . "grt") (">=" . "geq") ("<=" . "leq")
                               ("*" . "star") ("**" . "st2") ("***" . "st3")
                               ("/" . "slash") ("//" . "sl2") ("///" . "sl3")
                               ("+" . "plus") ("++" . "pl2") ("+++" . "pl3")
                               ("-" . "subt") ("1+" . "pl1") ("1-" . "su1"))
                             :test #'string=))
                 (string-downcase name)))
        (xml-name (xmlize-string name))
        (type (subseq html 0 (position #\_ html :test #'char=)))
        (font "function")
        (ent-tab '((">=" . "-geq") ("<=" . "-leq") ("/=" . "-neq")
                   ("=" . "-eq") (">" . "-grt") ("<" . "-lst"))))
    (dolist (re ent-tab)
      (setq ent (substitute-subseq ent (car re) (cdr re) :test #'char=)))
    (flet ((type-is (what)
             (let ((li (slot-value *clhs-version* what)))
               (if (stringp li) (string= li type)
                   (member type li :test #'string=)))))
      (cond ((type-is 'var)
             (if (alpha-char-p (char name 0)) ; *var*, -, / &c
                 (setq font "constant")
                 (setq ent (concatenate 'string (string-trim "*" ent) "-var")
                       font "varname")))
            ((or (type-is 'fun) (type-is 'mac) (type-is 'spe))
             (when (char= #\* (char ent (1- (length ent))))
               (setq ent (concatenate 'string (subseq ent 0 (1- (length ent)))
                                      "-star")))
             (when (and (string= html (clhs-version-doc *clhs-version*))
                        (not (string= name "DOCUMENTATION")))
               (setq ent (concatenate 'string ent "-doc")))
             (when (and (string= name "LAMBDA") (type-is 'spe))
               (setq ent (concatenate 'string ent "-sym"))))
            ((type-is 'typ)
             (setq font "classname")
             (unless (or (string-end-with "class" ent)
                         (string-end-with "type" ent))
               (setq ent (concatenate 'string ent "-t"))))
            ((type-is 'res) ; restart
             (setq ent (concatenate 'string ent "-s")))
            ((type-is 'dec) ; declaratons
             (setq font "literal" ent (concatenate 'string ent "-dec")))
            ((type-is 'glo) ; glossary
             (setq font "replaceable" ent (concatenate 'string ent "-glo")))
            ((type-is 'any) ; selection
             (setq font "literal" ent (concatenate 'string ent "-any")))
            ((char= #\& (char ent 0))
             (setq font "literal"
                   ent (concatenate 'string (subseq ent 1) "-amp")))
            (t (warn "~s: strange object: ~s [~s]~% [~s ~s]~%"
                     'clhs-write-entity name html ent font))))
    ;; do not override the standard entities
    (when (member ent '("and" "lambda" "or") :test #'string=)
      (setq ent (concatenate 'string ent "-m"))) ; macros
    (when (member ent '("map" "not") :test #'string=)
      (setq ent (concatenate 'string ent "-f"))) ; functions
    (when (member ent '("pi") :test #'string=)
      (setq ent (concatenate 'string ent "-v"))) ; variable
    (format
     out "<!ENTITY ~a '<ulink url=\"&clhs;/Body/~a\"><~a>~a</~a></ulink>'>~%"
     ent html font xml-name font)))

(defun clhs-write-entities (file)
  "Write the CLHS entities into the file."
  (unless *clhs-alist* (clhs-init))
  (with-timing ()
    (with-open-file (str file :direction :output :if-exists :supersede)
      (format t "~s: writing ~s..." 'clhs-write-entities file)
      (force-output)
      (format str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%
<!-- generated by `~s' -->~2%" 'clhs-write-entities)
      (let ((count-e 0) (count-i 0))
        (dolist (el *clhs-alist*)
          (dolist (html (cdr el))
            (incf count-e)
            (clhs-write-entity (car el) html str)))
        (format str "~%<!-- issues -->~%")
        (dolist (il *clhs-issues*)
          (incf count-i)
          (let ((path (second il)))
            (format str "<!ENTITY ~A '<ulink url=\"&clhs;~A\">~A</ulink>'>~%"
                    (subseq path (1+ (position #\/ path :from-end t))
                            (position #\. path :from-end t))
                    path (xmlize-string (first il)))))
        (format t "done [~:d entit~:@p, ~:d issue~:p] [~:d byte~:p]"
                count-e count-i (file-length str))))))

(defun clhs-doc (symb &key (out *standard-output*) (root *clhs-root*))
  "Dump the CLHS doc for the symbol."
  (declare (type (or symbol string) symb) (stream out))
  (unless *clhs-hashtable* (clhs-init))
  (let* ((sy (etypecase symb
               (symbol (symbol-name symb))
               (string (string-upcase symb))))
         (pa (gethash sy *clhs-hashtable*))
         (url (copy-url root)))
    (assert pa () "No HyperSpec doc for `~s'" sy)
    (dolist (pp pa)
      (setf (url-path url) (concatenate 'string (url-path root) "Body/" pp))
      (dump-url url :fmt "~*~a~%" :out out :proc #'html-translate-specials))))

#||
 (defun hw ()
  (gtk:with-connection ()
    (let ((w (gtk:window-new :toplevel))
          (b (gtk:button-new-with-label "Drueck mich.")))
      (gtk:signal-connect-full b "clicked"
                               (lambda ()
                                 (return-from hw))
                               0 0)
      (gtk:container-add w b)
      (gtk:widget-show b)
      (gtk:widget-show w)
      (print w)
      (print (gtk:widget-window w))
      (print (gtk:widget-style w))
      (gtk:event-loop))))

 (eval-when (compile load eval) (export '(gtk::signal-connect) :gtk))

 (defun gtk:signal-connect (widget signal-name fun &optional (bla 0) (blu 0))
  (gtk:signal-connect-full
   widget (nstring-downcase (substitute #\_ #\- (string signal-name))) fun
   bla blu))

 (defun text-insert (text-widget string &key (font nil) (fore nil) (back nil))
  (gtk:text-insert text-widget font fore back string (length string)))


 (defun set-clist-contents (clist strings)
  (gtk:clist-clear clist)
  (gtk:clist-freeze clist)
  (dolist (s (reverse strings)) (gtk:clist-insert clist 0 (list s)))
  (gtk:clist-thaw clist))

 (defun make-menu (items)
  (let ((menu (gtk:menu-new)))
    (dolist (x items menu)
      (if (eq x '-)
          (let ((i (gtk:menu-item-new)))
            (let ((a (gtk:hseparator-new)))
              (gtk:widget-show a)
              (gtk:container-add i a))
            (gtk:widget-set-sensitive i nil)
            (gtk:widget-show i)
            (gtk:menu-append menu i))
          (destructuring-bind (name &optional action doc) x
            (declare (ignorable doc))
            (let ((a (gtk:menu-item-new-with-label name)))
              (when action
                (gtk:signal-connect-full a "activate" action 0 0))
              (when (string-equal name "Save As")
                (gtk:widget-set-sensitive a nil))
              ;;(when docu
              ;;  (gtk:tooltips-set-tip tooltips a docu ""))
              (gtk:menu-append menu a)
              (gtk:widget-show a)))))))

 (defun make-menu-bar (items)
  (let ((menu-bar (gtk:menu-bar-new)))
    (dolist (x items menu-bar)
      (destructuring-bind (name menu) x
        (let ((root-menu (gtk:menu-item-new))
              (label (gtk:label-new name)))
          (gtk:container-add root-menu label)
          (gtk:widget-show label)
          (gtk:menu-item-set-submenu root-menu menu)
          (gtk:menu-bar-append menu-bar root-menu)
          (gtk:widget-show root-menu)
          (gtk:widget-show menu))))))

 (defun doc-window ()
  (gtk:with-connection ()
    (let* ((window (gtk:window-new :toplevel))
           (vbox   (gtk:vbox-new nil 0))
           (hpane  (gtk:hpaned-new))
           (vpane  (gtk:vpaned-new))
           (sw     (gtk:scrolled-window-new nil nil))
           (lst    (gtk:clist-new 1))
           (entry  (gtk:entry-new))
           (vbox2  (gtk:vbox-new nil 0))
           (vbox3  (gtk:vbox-new nil 0))
           (vbox4  (gtk:vbox-new nil 0))
           (txt    (gtk:text-new nil nil))
           (label1 (gtk:label-new "Description"))
           (label2 (gtk:label-new "Options"))
           (wholine (gtk:hbox-new nil 0))
           (stat (gtk:label-new "Status Line"))
           (menu (make-menu-bar
                  (list (list "File"
                              (make-menu
                               (list (list "Open"
                                           (lambda ()
                                             (gtk:label-set stat "Open")))
                                     (list "Save"
                                           (lambda ()
                                             (gtk:label-set stat "Save")))
                                     '("Quit"))))))))

      (gtk:signal-connect window "expose-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect window "configure-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect window "button-press-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect window "button-release-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect window "key-press-event"
                          (lambda (&rest x) (print x)))
      (gtk:widget-set-events
       window '(:button-press-mask :button-release-mask :key-press-mask))
      (gtk:widget-set-usize menu 200 40)
      (gtk:fixed-put window menu 100 100)
      (gtk:widget-show menu)
      (gtk:widget-set-usize window 800 300)

      (gtk:window-set-title window "Lisp Documentation")

      (gtk:widget-show wholine)
      (gtk:box-pack-end vbox wholine nil nil 0)
      ;; das hier geht nicht ;-(
      ;;(gtk:label-set-justify stat :left)
      (gtk:widget-show stat)
      (gtk:box-pack-start wholine stat t t 0)

      (gtk:scrolled-window-add-with-viewport sw lst)
      (gtk:box-pack-start vbox2 entry nil t 0)
      (gtk:box-pack-start vbox2 sw t t 0)

      (gtk:box-pack-start vbox3 label1 nil t 0)
      (gtk:box-pack-start vbox3 txt t t 0)

      (gtk:box-pack-start vbox4 label2 nil t 0)

      (gtk:paned-add1 hpane vbox2)
      (gtk:paned-add2 hpane vpane)

      (gtk:paned-add1 vpane vbox4)
      (gtk:paned-add2 vpane vbox3)
      (gtk:box-pack-start vbox hpane t t 0)
      (gtk:container-add window vbox)

      (mapc #'gtk:widget-show
            (list window vbox hpane sw lst entry vbox2
                  txt vpane vbox3 label1 vbox4 label2))

      (let ((syms nil))
        (labels ((new-syms (new-syms)
                   (setq syms new-syms)
                   (let ((*print-case* :downcase))
                     (set-clist-contents lst (mapcar #'symbol-name syms))))
                 (describe-sym (sym)
                   (gtk:text-freeze txt)
                   (gtk:text-set-point txt 0)
                   (gtk:text-forward-delete txt (gtk:text-get-length txt))
                   (text-insert
                    txt (with-output-to-string (sink)
                          (format sink " *** Symbol: `~a':~2%" sym)
                          (gtk:label-set stat (symbol-name sym))
                          (describe sym sink)
                          (dolist (ty '(compiler-macro setf structure
                                        type variable function))
                            (when (documentation sym ty)
                              (format
                               sink "~& *** Documentation as a ~a:~%~a~%"
                               ty (documentation sym ty))))
                          (when (fboundp sym)
                            (format sink "~& *** Args: ~s~%"
                                    (arglist (fdefinition sym))))
                          (when (boundp sym)
                            (format sink "~& *** Value: ~s~%"
                                    (symbol-value sym)))
                          (when (symbol-plist sym)
                            (format sink "~& *** Plist: ~s~%"
                                    (symbol-plist sym)))
                          (ignore-errors (clhs-doc sym sink))))
                   (gtk:text-thaw txt)))

          (gtk:signal-connect lst :select-row
                              (lambda (row col event)
                                (declare (ignore col event))
                                (describe-sym (elt syms row))))

          (gtk:signal-connect entry :activate
                              (lambda ()
                                (new-syms (apropos-list
                                           (string-upcase (gtk:entry-get-text
                                                           entry))))))
          ))

      (gtk:event-loop))))


 (defun hello-world ()
  (gtk:with-connection ()
    (let ((window (gtk:window-new :toplevel))
          (button (gtk:button-new-with-label "Say Hello")))
      (gtk:window-set-title window "Guile-Gtk: Hello World")
      (gtk:container-border-width window 10)
      (gtk:container-add window button)
      (gtk:signal-connect button "clicked"
                          (lambda ()
                            (terpri)
                            (princ "Hello World!")
                            '(gtk:widget-destroy window)))
      (gtk:widget-show button)
      (gtk:widget-show window))
    (gtk:event-loop)))

# ||
#+CMU
 (defun foo2 ()
  (mp:make-process (lambda () (foo)) :name "gtk demo")
  (loop
   (mp:process-wait "Waiting for ping" (lambda () *ping*))
   (print 'hi)
   (setq *ping* nil)))

 (compile 'foo2)
|| #

 (defun bar ()
  (gtk:with-connection ()
    (let ((window (gtk:window-new :toplevel))
          (sw     (gtk:scrolled-window-new nil nil))
          (canvas (gtk:drawing-area-new)))

      (gtk:signal-connect canvas "expose-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect canvas "configure-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect canvas "button-press-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect canvas "button-release-event"
                          (lambda (&rest x) (print x)))
      (gtk:signal-connect canvas "key-press-event"
                          (lambda (&rest x) (print x)))
      (gtk:widget-set-events
       canvas '(:button-press-mask :button-release-mask :key-press-mask))

      '(let ((e (make-menu-bar
                 (list (list "File"
                             (make-menu
                              (list (list "Open" (lambda ()
                                                   (gtk:label-set ll "Open")))
                                    (list "Save" (lambda ()
                                                   (gtk:label-set ll "Save")))
                                    '("Quit"))))))))
        (gtk:widget-set-usize e 200 40)
        (gtk:fixed-put canvas e 100 100)
        (gtk:widget-show e))

      '(let ((e (gtk:combo-new)))
        '(gtk:widget-set-usize e 200 40)
        (gtk:fixed-put canvas e 100 100)
        (gtk:widget-show e))

      (gtk:widget-set-usize canvas 800 3000)

      (gtk:scrolled-window-add-with-viewport sw canvas)
      (gtk:container-add window sw)

      (mapc #'gtk:widget-show (list window sw canvas)))
    (gtk:event-loop)))
||#
(provide :cllib-clhs)
;;; file clhs.lisp ends here
