;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-
;;;; ------------------------------------------------------------------------------------------
;;;;     Title:	Ad hoc SGML->HTML conversion tool
;;;;   Created:	1997-12-17
;;;;    Author: Gilbert Baumann <unk6@rz.uni-karslruhe.de>
;;;; ------------------------------------------------------------------------------------------
;;;;  (c) copyright 1997,2002 by Gilbert Baumann

(defparameter *source-directory*
  (merge-pathnames
   (make-pathname :name :unspecific :type :unspecific
                  :directory '(:relative "source"))
   *load-truename*)
  "The directory, where the SGML sources and other stuff can be found.")

(defparameter *destination-directory*
  #-NIL
  (merge-pathnames
   (make-pathname :name :unspecific :type :unspecific
                  :directory '(:relative "clxman"))
   *load-truename*)
  #+NIL
  #p"/var/www/clxman/"
  "Where to put the resulting files.")

;;;; ------------------------------------------------------------------------------------------
;;;; Choose your options:

(defparameter *include-nav-bar-p* t
  "Whether to include a navigation bar at all.")

(defparameter *include-applet-p* t
  "Whether to include a nifty java applet to search the index.")

(defparameter *use-cgi-script-p* nil
  "Whether to generate a FORM element instead of an applet for index search.")

(defparameter *use-frames-p* nil
  "Whether to generate a multi-frame version.")

(defparameter *short-filenames-p* nil
  "Whether to use short file names; currently broken.")

(defparameter *short-anchors-p* nil
  "Whether to generate short anchor names; mainly for the framed
version to keep file name length reasonable.")

(defparameter *navbar-background*
  "#FFFFFF"
  "Background color of the navigation bar.")

;;;; ------------------------------------------------------------------------------------------
;;;; Global variables

(defvar *clxman-dtd* nil)
(defvar *clxman-dtd-read-time* 0)
(defvar *clxman-pt* nil)
(defvar *clxman-pt-read-time* 0)

(defvar *index-section*)
(defvar *top-section*)

;;;; ------------------------------------------------------------------------------------------
;;;;  Random utilities

(define-modify-macro nconcf (&rest args) nconc)

(defun split-string (bag string)
  (setq string (string-trim bag string))
  (cond ((= (length string) 0) nil)
	(t
	 (let ((p (position bag string :test #'(lambda (x y) (member y x)))))
	   (if p
	       (cons (subseq string 0 p) (split-string bag (subseq string p)))
	     (list string))) )))


(defun grind-iso-time (&optional (universal-time (get-universal-time))
                                 &key (timep t) (zonep t))
  (multiple-value-bind (second minute hour day month year week-day dst? zone) (decode-universal-time universal-time)
    (declare (ignore dst? week-day))
    (format nil "~4,'0D-~2,'0D-~2,'0D~@[~* ~2,'0D:~2,'0D:~2,'0D~@[~* ~A~2,'0D~2,'0D~]~]"
            year month day
            timep
            hour minute second
            zonep
            (if (> zone 0) "-" "+")
            (floor (abs zone))
            (floor (* 60 (nth-value 1 (floor (abs zone))))))))

;;;; ------------------------------------------------------------------------------------------
;;;;  Some PT utilities:

(defun pt-cdata (x)
  (assert (eq (pt-name x) :pcdata))
  (pt-attrs x))

(defun pt-cdata* (x)
  (cond ((eq (gi x) :pcdata)
         (pt-cdata x))
        ((eq (gi x) :br)
         " ") ;hack: treat <BR> as white space
        (t
         (reduce #'(lambda (&optional (x "") (y ""))
                     (concatenate 'string x y))
                 (mapcar #'pt-cdata* (pt-children x))))))

(defun map-pt (fun pt)
  (funcall fun pt)
  (mapc (lambda (x) (map-pt fun x)) (pt-children pt)))

;; gi is new API
(defun gi (pt)
  (pt-name pt))

;;;; ------------------------------------------------------------------------------------------
;;;;  Parse Tree Construction

;; The only difference between MPT and MPT* is that the later accepts
;; strings and lists (to be spliced in).

(defun mpt (name &rest args)
  (let ((attrs nil))
    (do ((q args (cddr q)))
	((not (keywordp (car q)))
	 (make-pt :name name 
		  :attrs attrs
		  :children q))
      (push (cadr q) attrs)
      (push (car q) attrs))))

(defun mpt* (name &rest args)
  (let ((attrs nil))
    (do ((q args (cddr q)))
	((not (keywordp (car q)))
	 (progn
	  (make-pt :name name 
		   :attrs (reverse attrs)
		   :children (mapcan #'(lambda (x)
					 (cond ((stringp x) 
						(list (make-cdata x)))
					       ((pt-p x) (list x))
					       (x))) 
				     q))))
      (push (car q) attrs)
      (push (cadr q) attrs))))

(defun make-cdata (str)
  (make-pt :name :pcdata :attrs str))

;;;; ------------------------------------------------------------------------------------------
;;;;  Anchors

;; We maintain hyperlinks through the means of these anchor objects. We
;; call a hyper links end-point the anchor, it has a name and knows
;; where it points to.

;; To generate a hyperlink lookup the anchor object by using
;; FIND-ANCHOR and apply it to MAKE-HYPER-LINK-FOR-ANCHOR. To generate
;; the HTML end point anchor element use MAKE-ANCHOR-ELEMENT.

;; Ideally all hyperlinks would be generated through this interface.

;; To retrieve a section's anchor use SECTION-ANCHOR.

(defvar *anchors*
  (make-hash-table :test #'equal))

(defstruct anchor
  name
  page-url
  title
  html-name)    ;name for the A element's NAME attribute

(defun define-anchor (name page-url &optional title)
  (setf (gethash (string-upcase name) *anchors*)
        (make-anchor :name name :page-url page-url :title title
                     :html-name (if *short-anchors-p*
                                    (format nil "A~5,'0D" (1+ (hash-table-count *anchors*)))
                                    name))))

(defun find-anchor (name)
  (or (gethash (string-upcase name) *anchors*)
      (error "Anchor ~S is not defined." name)))

(defun anchor-url (anchor)
  (cond (*use-frames-p*
         (if (anchor-html-name anchor)
             (format nil "~A_~A.html"
                     ;; hack, hack
                     (subseq (anchor-page-url anchor)
                             0 (search ".html" (anchor-page-url anchor)))
                     (anchor-html-name anchor))
             (format nil "~A" (anchor-page-url anchor))))
        (t
         (if (anchor-html-name anchor)
             (format nil "~A#~A" (anchor-page-url anchor) (anchor-html-name anchor))
             (format nil "~A" (anchor-page-url anchor))))))

(defun make-hyper-link-for-anchor (anchor &optional content)
  (cond (*use-frames-p*
         (mpt* :A
               :HREF (anchor-url anchor)
               :TARGET "_top"
               (or content (anchor-title anchor))))
        (t
         (mpt* :A
               :HREF (anchor-url anchor)
               :TARGET "_top"
               (or content (anchor-title anchor))))))

(defun make-anchor-element (anchor)
  (mpt* :A :NAME (anchor-html-name anchor)))

;;;; ------------------------------------------------------------------------------------------
;;;;  Sections

;; After we parsed the input we split it into these section objects,
;; which mainting the proper next/prev/up links.

(defstruct section
  ;; A single section
  level       ;The section's level, 0 is the root node, 1 is chapters and so on.
  title       ;The section's title, a string, I believe
  filename    ;The name of the file this section is supposed to be dumped to.
  pt          ;The parse tree node of the sections content
  ;; links:
  next        
  previous
  up
  children)   ;List of the section's children.

(defun section-anchor (section)
  "Given a section return its anchor suitable for
   MAKE-HYPER-LINK-FOR-ANCHOR."
  #+NIL
  (find-anchor
   (header-element-anchor-name
    (find-if (lambda (x) (member (pt-name x) '(:h1 :h2 :h3 :h4 :h5 :h6)))
             (section-pt section))))
  (make-anchor :name nil
               :page-url (section->url section)
               :title (section-title-to-anchor-title (section-level section) (section-title section))))

(defparameter *node-nr* 0)

(defun section-name->filename (str)
  (cond (*short-filenames-p*
         (format nil "N~5,'0D.html" (incf *node-nr*))
         )
        (t
         (cond ((string= str "Index") "the_index.html")
               ((and (= (length str) 4)
                     (string= (subseq str 1) "..."))
                (format nil "the_index_~(~A~).html" (char str 0)))
               (t
                (concatenate 'string
                             (map 'string #'(lambda (ch)
                                              (cond ((alpha-char-p ch) ch)
                                                    ((digit-char-p ch) ch)
                                                    ((find ch "-_") ch)
                                                    ((char= ch #\.) #\_)
                                                    ((char= ch #\space) #\_)
                                                    (t #\_)))
                                  str)
                             ".html"))))))

(defun hn->level (hn)
  (ecase hn
    (:h1 1) (:h2 2) (:h3 3) (:h4 4) (:h5 5) (:h6 6)))

(defun section-title-to-anchor-title (level title)
  (mpt* :SPAN
        (cond ((<= level 1)
               "section ")
              (t
               "paragrpah "))
        (let* ((s title)
               (p (position #\Space s)))
          (cond ((and p (>= p 1) (digit-char-p (char s (1- p))))
                 (mpt* :SPAN
                       (subseq s 0 p)
                       ", "
                       (mpt* :I
                             (subseq s (+ p 1)))))
                (t
                 s)))))

(defun collect-sections (pt)
  (let ((body (cadr (pt-children pt)))
	(sections nil)
	(section (make-section :level 0
			       :title "CLX Manual"
			       :filename "contents.html"
			       :pt nil)))
    (setf *top-section* section)
    (dolist (k (pt-children body))
      (cond ((member (pt-name k) '(:h1 :h2 :h3 :h4 :h5 :h6))
	     ;; start a new section
	     (and section
		  (nconcf sections (list section)))
	     (setq section
		   (make-section :title (pt-cdata* k)
				 :filename (section-name->filename (pt-cdata* k))
				 :pt nil
				 :level (hn->level (pt-name k))))
             (nconcf (section-pt section) (list k))
             )
	    (t
	     ;;add to current section
	     (nconcf (section-pt section) (list k))) ))
    (link-sections sections)
    sections))

(defun link-sections (lst)
  (do ((q lst (cdr q)))
      ((null q))
    ;; first the next and previous links (this is easy)
    (and (cdr q)
	 (setf (section-next (car q)) (cadr q)
	       (section-previous (cadr q)) (car q)))
    ;; now the up and children links
    (do ((k (cdr q) (cdr k)))
	((or (null k)
	     (<= (section-level (car k)) (section-level (car q)))))
      (when (= (section-level (car k)) (+ 1 (section-level (car q))))
	;; this is a direct subsection
	(nconcf (section-children (car q)) (list (car k)))
	(setf (section-up (car k)) (car q))) ) ))

(defun section->url (x)
  (section-filename x))

;;;; ------------------------------------------------------------------------------------------
;;;;  The Index
;;;;

(defstruct index-entry
  sec
  name
  kind
  ckind)

(defun index-entry-anchor (ie)
  (find-anchor (index-entry-name ie)))

(defun canon-kind (str)
  (cond ((string-equal str "Condition") :condition)
	((string-equal str "Macro") :macro)
	((string-equal str "Event Type") :event-type)
	((string-equal str "Function") :function)
	((string-equal str "Structure") :type)
	((string-equal str "Type") :type)
	((string-equal str "Slot" :end1 4) :slot)
        (t
         (warn "~S has no kind?" str)
         :unknown
         )))

(defun defun-name->anchor-list (name)
  (setq name (ignore-errors (sanify-string/2 (pt-cdata* name))))
  (cond ((and (not (string-equal name "setf"))
	      (not (find #\, name)))
	 (list name))
	((and (not (string-equal name "setf"))
	      (find #\, name))
	 (split-string '(#\, #\space) name))
	(t
	 (format t "~%;dfndnp: ~S" name))))

(defun header-element-anchor-name (pt)
  (or (and (getf (pt-attrs pt) :NAME)
           (format nil "R_~A" (getf (pt-attrs pt) :NAME)))
      (format nil "S_~A" (subseq (pt-cdata* pt) 0 (or (position #\space (pt-cdata* pt))
                                                      (length (pt-cdata* pt)))))))

(defun collect-defun-index (sections)
  ;; XXX also collects the ref-index
  ;; XXX also collect *index-section* and *top-section*
  (let ((res nil))
    (dolist (sec sections res)
      (labels ((add (name ckind kind)
		 (push (make-index-entry :sec sec
					 :name name
					 :ckind ckind
					 :kind kind)
		       res))
	       (walk (x)
		 (cond ((eq (pt-name x) :DEFUN)
			(let ((name (find-elm :name (pt-children x)))
			      (names nil)
			      (kind (find-elm :kind (pt-children x)))
			      (ckind nil))
			  (and name
			       (setq names (defun-name->anchor-list name)))
			  ;; (ignore-errors (sanify-string/2 (pt-cdata* name))))
			  (setq ckind (progn 'ignore-errors (canon-kind (sanify-string/2 (pt-cdata* kind)))))
			  (cond ((and names ckind)
				 (dolist (k names)
				   (when k
                                     (add k ckind kind)
                                     (define-anchor k (section->url sec) (make-cdata k))
                                     )))
				(t ))))
                       ;;
                       ((member (pt-name x) '(:H1 :H2 :H3 :H4 :H5 :H6))
                        ;; Now every section gets an anchor
                        (let ((name (header-element-anchor-name x)))
                          (define-anchor name (section->url sec)
                            (section-title-to-anchor-title (hn->level (gi x)) (pt-cdata* x))))
                        (when (string-equal (pt-cdata* x) "Index")
                          (setf *index-section* sec)) )
                       ;;
		       (t
			(mapc #'walk (pt-children x))) )))
	(dolist (k (section-pt sec))
	  (walk k))))))

(defvar *defun-index*)

(defun sort-index (index)
  (sort index #'(lambda (x y)
		  (setq x (remove-if #'(lambda (c) (find c ":->")) x))
		  (setq y (remove-if #'(lambda (c) (find c ":->")) y))
		  (string-lessp x y))
	:key #'index-entry-name))

(defun patch-index-section (sec ch index)
  (let ((res nil))
    (dolist (k index)
      (cond 
       ((let ((foo (string-left-trim ":->" (index-entry-name k))))
	  (char-equal (char foo 0) ch))
	(nconcf res 
		(list 
		 (mpt* :LI
                       (make-hyper-link-for-anchor (index-entry-anchor k)
                                                   (make-cdata (index-entry-name k)))
		       (make-cdata ", ")
		       (mpt* :I
			     (pt-children (index-entry-kind k)))))))))
    (cond (res
	   (nconcf (section-pt sec)
		   (list (mpt* :UL res))))
	(t
	 (warn "No entries in the `~A' section of the index." ch))) ))

(defun patch-index-sections (secs index)
  (dolist (k secs)
    ;; Arg! XXX This logic breaks, when using short file names 
    (cond ((and (>= (length (section-filename k)) 10)
		(string= (section-filename k) "the_index_" :end1 10))
	   (patch-index-section k (char (section-filename k) 10)
				index)))))

;;;; ------------------------------------------------------------------------------------------
;;;;  Main Processing

(defun process-elements (x)
  (mapcar #'process-element x))

(defun find-elm (name x)
  (find name x :key #'pt-name))

(defun process-element (x)
  (case (pt-name x)
    ((:H1 :H2 :H3 :H4 :H5 :H6)
     (make-pt :name :DIV
              :children
              (nconc
               (list
                (make-anchor-element (find-anchor (header-element-anchor-name x))))
               (list
                (mpt* (pt-name x)
                      (mpt* :FONT
                            :COLOR "#004000"
                            :FACE "helvetica"
                            (process-elements (pt-children x))))))))
    ;;
    ((:REF)
     (let ((entry (find-anchor (format nil "R_~A" (getf (pt-attrs x) :NAME)))))
       (make-hyper-link-for-anchor entry)))
    ;;                
    ((:XH1 :XH2 :XH3 :XH4 :XH5 :XH6)
     (make-pt :name (cdr (assoc (pt-name x) '((:XH1 . :H1) (:XH2 . :H2) (:XH3 . :H3)
					      (:XH4 . :H4) (:XH5 . :H5) (:XH6 . :H6))))
	      :attrs (pt-attrs x)
	      :children (process-elements (pt-children x))))
    ((:DEFUN)
     (process-defun x))
    ((:B)
     (let* ((contents (ignore-errors (sanify-string/2 (pt-cdata* x))))
	    (entry (and contents
			(find contents *defun-index* :key #'index-entry-name :test #'string-equal))))
       (cond (entry
              (make-hyper-link-for-anchor (find-anchor contents) (mpt* :B (pt-children x))) )
	     (t
	      (make-pt :name (pt-name x)
		       :attrs (pt-attrs x)
		       :children (process-elements (pt-children x)))))))
    ((:TERM)
     (make-pt :name :SPAN
              :attrs (list :CLASS "term")
              :children (process-elements (pt-children x))))
    
    ((:DESC :ARGS :RETURNS :KIND :NAME :LL)
     (process-elements (pt-children x)))
    ((:A)
     (let ((href (getf (pt-attrs x) :href)))
       (cond ((and href (char= (char href 0) #\#))
              (mpt* :A :HREF (anchor-url (find-anchor (format nil "R_~A" (subseq href 1))))
                    (process-elements (pt-children x))))
             (t
              (make-pt :name (pt-name x)
                       :attrs (pt-attrs x)
                       :children (process-elements (pt-children x)))))))         
    (t
     (make-pt :name (pt-name x)
	      :attrs (pt-attrs x)
	      :children (process-elements (pt-children x))) )))

(defun process-defun (x)
  (let ((name (find-elm :name (pt-children x)))
        (kind (find-elm :kind (pt-children x)))
        (ll   (find-elm :ll   (pt-children x)))
        (args (find-elm :args (pt-children x)))
        (desc (find-elm :desc (pt-children x)))
        (returns (find-elm :returns (pt-children x))))
    (when ll
      (setf ll (mungle-lambda-list ll)))
    (mpt* :DIV
          ;;(mpt* :BR)
          (mapcar #'(lambda (x)
                      (make-anchor-element (find-anchor x)))
                  (defun-name->anchor-list name))
          (mpt* :BR)
          (mpt* :HR :NOSHADE :NOSHADE :SIZE 1)
          (mpt* :TABLE
                :WIDTH "100%"
                (mpt* :COLGROUP :SPAN 3
                      (mpt* :COL :WIDTH "0*")
                      (mpt* :COL :WIDTH "1*")
                      (mpt* :COL :WIDTH "0*"))
                (mpt* :TR
                      (mpt* :TD 
                            :ALIGN :LEFT
                            :VALIGN :BASELINE
                            :NOWRAP :NOWRAP
                            (mpt* :B
                                  (mpt* :FONT
                                        :COLOR "#440000"
                                        (process-element name))))
                      (mpt* :TD 
                            :ALIGN :LEFT
                            :VALIGN :BASELINE
                            :WIDTH "100%"
                            (and ll (process-element ll)))
                      (mpt* :TD 
                            :ALIGN :RIGHT
                            :VALIGN :BASELINE
                            :NOWRAP :NOWRAP
                            (process-element kind))))
          (mpt* :TABLE
                (mpt* :TR
                      (mpt* :TD (make-array 8
                                            :element-type (array-element-type "") 
                                            :initial-element (code-char #o240)))
                      (mpt* :TD
                            (and args
                                 (mpt* :DL
                                       (process-element args)))
                            (and desc
                                 (process-element desc))
                            (and returns
                                 (mpt* :DL
                                       (process-element returns))))))
          #+NIL
          (mpt* :DIV :ALIGN :RIGHT
                (mpt* :A :HREF (format nil "mailto:unk6@rz.uni-karlsruhe.de?subject=~A"
                                       (substitute #\space #\space
                                                   (format nil "CLXMAN: ~A"
                                                           (sanify-string/2 (pt-cdata* name)))))
                      (mpt* :FONT :COLOR "#FF8888"
                            "Annotate")))
          )))

;;;
;;; This is an attempt to automatically parse and format lambda lists:
;;;

(defun mungle-lambda-list (LL-element)
  ;;  (sgml-unparse *clxman-dtd* LL-element *standard-output*)
  ;;(print (pt-cdata* LL-element))
  ;; as always, first thing we do is tokenizing:
  (let ((lambda-list (wu (pt-cdata* LL-element))))
    ;; look if this matches, what we expect ...
    (let ((res
           (block quux
             (labels ((sym-p (s) (alphanumericp (char s 0)))
                      (lst-p (s) (char= #\( (char s 0)))
                      (keyword-p (s) (char= #\: (char s 0)))
                      (&key-p (s) (string= s "&key"))
                      (&optional-p (s) (string= s "&optional"))
                      (&allow-other-keys-p (s) (string= s "&allow-other-keys"))
                      ;;
                      (grok-required (tokens)
                        (cond ((null tokens) nil)
                              ((sym-p (first tokens))
                               (list* (mpt* :I (first tokens))
                                      (make-cdata " ")
                                      (grok-required (rest tokens))))
                              ((&key-p (first tokens))
                               (list* (make-cdata "&key")
                                      (make-cdata " ")
                                      (grok-keys (rest tokens))))
                              ((&optional-p (first tokens))
                               (list* (make-cdata "&optional")
                                      (make-cdata " ")
                                      (grok-optionals (rest tokens))))
                              (t
                               (return-from quux nil))))
                      (grok-optionals (tokens)
                        (cond ((null tokens) nil)
                              ((sym-p (first tokens))
                               (list* (mpt* :I (first tokens))
                                      (make-cdata " ")
                                      (grok-optionals (rest tokens))))
                              ((&key-p (first tokens))
                               (list* (make-cdata "&key")
                                      (make-cdata " ")
                                      (grok-keys (rest tokens))))
                              ((lst-p (first tokens))
                               (let ((p (position #\space (first tokens))))
                                 (list* (mpt* :B "(")
                                        (make-cdata (subseq (first tokens) 1 p))
                                        (make-cdata (string (code-char #o240)))
                                        (mpt* :B
                                              (substitute (code-char #o240) #\space (subseq (first tokens) (+ p 1))))
                                        (grok-optionals (rest tokens)))))
                              (t
                               (return-from quux nil))))
                      (grok-keys (tokens)
                        (cond ((null tokens) nil)
                              ((or (lst-p (first tokens))
                                   (keyword-p (first tokens)))
                               (list* (mpt* :B
                                            (substitute (code-char #o240) #\space (first tokens)))
                                      (make-cdata " ")
                                      (grok-keys (rest tokens))))
                              ((and (&allow-other-keys-p (first tokens))
                                    (null (rest tokens)))
                               (list (make-cdata "&allow-other-keys")))
                              (t
                               (return-from quux nil)))))
               (mpt* :SPAN :CLASS "lambdalist" (grok-required lambda-list))))))
      (cond (res
             res)
            (t
             (warn "Cannot grok lambda-list: ~S." lambda-list)
             LL-element)))))

(defun wu (string &aux res)
  (let ((p 0) (e (length string)))
    (do ()
        (nil)
      (cond ((= p e) (return))
            ((char= (char string p) #\()
             (let ((level 1) (p2 (1+ p)))
               (do ()
                   ((= level 0))
                 (case (char string p2)
                   (#\( (incf level))
                   (#\) (decf level)))
                 (incf p2))
               (push (subseq string p p2) res)
               (setf p p2)))
            ((char/= (char string p) #\space)
             (let ((p2 p))
               (do ()
                   ((or (= p2 e)
                        (char= (char string p2) #\()
                        (char= (char string p2) #\space)))
                 (incf p2))
               (push (subseq string p p2) res)
               (setf p p2)))
            (t
             (incf p)))))
  (reverse res))

(defun make-hskip (n)
  (make-cdata (make-array n :element-type (array-element-type "") :initial-element (code-char #o240))))

(defun make-naviagtion-bar (sec top-hr? bottom-hr?)
  (funcall (if *use-cgi-script-p*
               (lambda (table)
                 (mpt :FORM
                      :METHOD :GET
                      :ACTION "doc-index.cgi"
                      ;;:ACTION "http://www.stud.uni-karlsruhe.de/~unk6/clxman/doc-index.cgi"
                      :TARGET "_top"
                      table))
               #'identity)
           (mpt* :DIV
                 :CLASS "navbar"
                 (append
                  (and top-hr? (list (mpt* :HR :SIZE 1 :NOSHADE :NOSHADE)))
                  (list
                   (mpt :TABLE
                        :WIDTH "100%"
                        :CELLSPACING 0
                        :CELLPADING 0
                        (mpt :COLGROUP
                             :SPAN 3
                             (mpt :COL :width "1*")
                             (mpt :COL :width "0*")
                             (mpt :COL :width "0*"))
                        (mpt :TR
                             (mpt* :TD
                                   :ALIGN :LEFT
                                   :VALIGN :BASELINE
                                   :WIDTH "100%"
                                   (and *include-nav-bar-p*
                                        (nconc
                                         (and (section-next sec)
                                              (list (make-hyper-link-for-anchor (section-anchor (section-next sec))
                                                                                "Next")
                                                    (make-hskip 5)))
                                         (and (section-previous sec)
                                              (list (make-hyper-link-for-anchor (section-anchor (section-previous sec))
                                                                                "Prev")
                                                    (make-hskip 5)))
                                         (and (section-up sec)
                                              (list
                                               (make-hyper-link-for-anchor (section-anchor (section-up sec))
                                                                           "Up")
                                               (make-hskip 5)))
                                         (and (not *use-frames-p*)
                                              (list (make-hyper-link-for-anchor (section-anchor *top-section*)
                                                                                "Top"))))))
                             (mpt* :TD
                                   :ALIGN :RIGHT
                                   :VALIGN :BASELINE
                                   (if (or *include-applet-p* *use-cgi-script-p*)
                                       "Seek: "
                                       ""))
                             (mpt* :TD
                                   :ALIGN :LEFT
                                   :VALIGN :BASELINE
                                   (cond (*use-cgi-script-p*
                                          (list
                                           (mpt :input
                                                :type :text
                                                :name "q"
                                                :value ""
                                                :size 35
                                                :maxlength 100)))
                                         (*include-applet-p*
                                          (list (mpt :APPLET
                                                     :CODEBASE "."
                                                     :CODE "docIndex/DocIndexApplet.class"
                                                     :ALIGN :CENTER
                                                     :WIDTH 300
                                                     :HEIGHT 40
                                                     (mpt :PARAM
                                                          :NAME "target" ;zzz
                                                          :VALUE "_top")
                                                     (mpt :PARAM
                                                          :NAME "index"
                                                          :VALUE "Xlib")
                                                     (mpt :PARAM :NAME "bgcolor"
                                                          :value *navbar-background*))))
                                         (t
                                          (list
                                           (make-hyper-link-for-anchor (section-anchor *index-section*)
                                                                       "Index")))) ))))
           (and bottom-hr? (list (mpt* :HR :SIZE 1 :NOSHADE :NOSHADE)))))))

(defun make-hier-children (sec)
  (cond ((not (null (section-children sec)))
	 (apply #'mpt* :UL
		(mapcar #'(lambda (x)
			    (mpt* :LI
                                  (make-hyper-link-for-anchor (section-anchor x)
                                                              (section-title x))))
			(section-children sec))))
	(t
	 (make-cdata "")) ))

(defun make-head (sec)
  (mpt* :HEAD
	(mpt* :TITLE (section-title sec))
	(mpt* :LINK 
	      :REV "made"
	      :HREF "mailto:unk6@rz.uni-karlsruhe.de")
	(if (section-next sec)
	    (mpt* :LINK :REL "Next" :HREF (section->url (section-next sec)))
	  nil)
	(if (section-previous sec)
	    (mpt* :LINK :REL "Previous" :HREF (section->url (section-previous sec)))
	  nil)
	(mpt* :LINK :REL "Start"     :HREF "index.html")
	(mpt* :LINK :REL "Contents"  :HREF "contents.html")
	(mpt* :LINK :REL "Index"     :HREF "the_index.html")
	(mpt* :LINK :REL "Glossary"  :HREF "Glossary.html")
	(mpt* :LINK :REL "Copyright" :HREF "Front_matter.html")
        (mpt* :LINK
              :REL "Stylesheet"
              :TYPE "text/css"
              :HREF "clxman.css") ))

(defvar *date-string* nil)

(defun make-footer (sec)
  (declare (ignore sec))
  (list
   (mpt* :DIV
         :CLASS "footer"
         (mpt* :a :href "Front_matter.html"
               (make-cdata "© 1988, 1989 Texas Instruments Incorporated"))
         (mpt* :br)
         (make-cdata "Conversion to HTML made by ")
         (mpt* :a 
               :lang "de"
               :href "mailto:unk6@rz.uni-karlsruhe.de"
               (make-cdata "Gilbert Baumann"))
         (make-cdata ".")
         (mpt* :br)
         (make-cdata (format nil "Last build: ~A" *date-string*)))))


(defun dump-section (sec)
  (if *use-frames-p*
      (dump-section-frames sec)
      (dump-section-no-frames sec)))

(defun dump-section-no-frames (sec)
  (with-open-file (sink (merge-pathnames (section-filename sec) *destination-directory*)
		   :direction :output
		   :if-exists :new-version)
    (write-string "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">" sink)
    (terpri sink)
    (write-string "<!-- This file is automatically generated, do not edit -->" sink)
    (terpri sink)
    (sgml-unparse *clxman-dtd*
		  (mpt* :HTML
			:LANG "en"
			(make-head sec)
			(process-element
			 (mpt* :BODY
			       :BGCOLOR "#FFFFFF"
                               (make-naviagtion-bar sec nil t)
			       (section-pt sec)
			       (and (section-children sec)
				    (mpt* :HR :NOSHADE :NOSHADE :SIZE 1))
			       (make-hier-children sec)
                               (make-naviagtion-bar sec t t)
			       (make-footer sec) )))
		  sink)))

(defun dump-section-frames (sec)
  (let ((body (process-element
               (mpt* :BODY
                     :BGCOLOR "#FFFFFF"
                     (section-pt sec)
                     (and (section-children sec)
                          (mpt* :HR :NOSHADE :NOSHADE :SIZE 1))
                     (make-hier-children sec)
                     ;;(mpt* :HR :NOSHADE :NOSHADE :SIZE 1)
                     (make-footer sec) ))))
    (with-open-file (sink (merge-pathnames (section-filename sec) *destination-directory*)
                          :direction :output
                          :if-exists :new-version)
      (write-string "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">" sink)
      (terpri sink)
      (write-string "<!-- This file is automatically generated, do not edit -->" sink)
      (terpri sink)
      (sgml-unparse *clxman-dtd*
                    (mpt* :HTML
                          :LANG "en"
                          (make-head sec)
                          (mpt* :frameset
                                :cols "300, *"
                                (mpt* :frame :src "full-contents.html")
                                (mpt* :frameset
                                      :rows "50, *"
                                      (mpt* :frame
                                            :src (format nil "h-~A" (section-filename sec))
                                            ;; :marginwidth 0
                                            :marginheight 0
                                            :noresize :noresize
                                            :scrolling :no
                                            :frameborder 0)
                                      (mpt* :frame
                                            :src (format nil "b-~A" (section-filename sec))
                                            :scrolling :yes
                                            :frameborder 0)))
                          body)
                    sink))
    ;;
    (with-open-file (sink (merge-pathnames (format nil "h-~A" (section-filename sec))
                                           *destination-directory*)
                          :direction :output
                          :if-exists :new-version)
      (write-string "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">" sink)
      (terpri sink)
      (write-string "<!-- This file is automatically generated, do not edit -->" sink)
      (terpri sink)
      (sgml-unparse *clxman-dtd*
                    (mpt* :HTML
                          :LANG "en"
                          (mpt* :BODY
                                :BGCOLOR *navbar-background*
                                (make-naviagtion-bar sec nil nil)
                                (mpt* :BR)
                                (mpt* :BR)))
                    sink))
    ;;
    (with-open-file (sink (merge-pathnames (format nil "b-~A" (section-filename sec))
                                           *destination-directory*)
                          :direction :output
                          :if-exists :new-version)
      (write-string "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">" sink)
      (terpri sink)
      (write-string "<!-- This file is automatically generated, do not edit -->" sink)
      (terpri sink)
      (sgml-unparse *clxman-dtd*
                    (mpt* :HTML
                          :LANG "en"
                          (make-head sec)
                          body)
                    sink))))

;;;; ------------------------------------------------------------------------------------------
;;;;  The Java Applet

(defun generate-java-applet ()
  (emit-java-index "Xlib")
  (ext:run-program "javac" (list "docIndex/DocIndexApplet.java"))
  (ext:run-program "javac" (list "docIndex/Index.java"))
  (ext:run-program "javac" (list "docIndex/XlibIndex.java"))
  (ext:run-program "cp" (list "docIndex/DocIndexApplet.class"
                              "docIndex/Index.class"
                              "docIndex/XlibIndex.class"
                              (namestring (truename
                                           (make-pathname :directory (append (pathname-directory *destination-directory*)
                                                                             (list "docIndex"))
                                                          :name nil
                                                          :type nil
                                                          :defaults *destination-directory*))))))
  

(defparameter *applet-index-preample*
  "// THIS FILE IS AUTOMATICALLY GENERATED -- DO NOT EDIT
//
// (c) Copyright 2002 by Deliana Foutekova <unjm@stud.uni-karlsruhe.de>
//                    by Gilbert Baumann <unk6@rz.uni-karslruhe.de>
package docIndex;

import java.util.*;
import java.net.*;

/** 
 * Provides a method that creates the index for Xlib.
 */
public class ~AIndex extends Index {

    /**
     * Get the index of the package Xlib */
    public String[][] getIndexTable() {
      return new String[][] 
")

(defparameter *applet-index-postample*
  ";
    }
}
")

(defun emit-java-index (name)
  (with-open-file (sink (format nil "docIndex/~AIndex.java" name)
                        :direction :output
                        :if-exists :new-version)
    (format sink *applet-index-preample* name)
    (format sink "{~{{~{~S~^, ~}}~^,~% ~}}"
            (sort (mapcar (lambda (k)
                            (list (index-entry-name k)
                                  (anchor-url (find-anchor (index-entry-name k)))) )
                          *defun-index*)
                  #'string<
                  :key #'first))
    (format sink *applet-index-postample*)))

;;;; ------------------------------------------------------------------------------------------
;;;;  Main Program

(defun run ()
  (when (or (null *clxman-dtd-read-time*)
            (and *clxman-dtd-read-time*
                 (> (file-write-date (merge-pathnames "clxman.dtd" *source-directory*))
                    *clxman-dtd-read-time*)))
    (setf *clxman-dtd-read-time* (get-universal-time))
    (progn
      (format T "~%;;Parsing DTD ")
      (setq *clxman-dtd* (parse-dtd (merge-pathnames "clxman.dtd" *source-directory*)))))
  (when (or (null *clxman-pt-read-time*)
            (and *clxman-pt-read-time*
                 (> (file-write-date (merge-pathnames "clxman.sgml" *source-directory*))
                    *clxman-pt-read-time*)))
    (setf *clxman-pt-read-time* (get-universal-time))
    (setq *clxman-pt*
	  (progn
	    (format T "~%;;Parsing input file ... ")
	    (car
	     (with-open-file (input (merge-pathnames "clxman.sgml" *source-directory*))
	       (parse4 input *clxman-dtd* :clxman))))))
  (format T "~&;; Generating manual ") (finish-output)
  (generate-clxman)
  (format T "~&;; Manual was successfully generated, point your browser to:~%")
  (format T "~&;;  file://~A~%"
          (namestring (truename (merge-pathnames "contents.html" *destination-directory*))))
  (values) )

(defun generate-clxman ()
  (clrhash *anchors*)
  (setf *node-nr* 0)
  (let* ((secs (collect-sections *clxman-pt*))
         (*defun-index*
          (collect-defun-index secs))
         (*date-string*
          (grind-iso-time (get-universal-time) :timep nil)))
    (when *use-frames-p*
      (with-open-file (sink (merge-pathnames "full-contents.html" *destination-directory*)
                            :direction :output
                            :if-exists :new-version)
        (sgml-unparse *clxman-dtd*
                      (mpt* :HTML
                            #+NIL
                            (mpt* :HEAD
                                  (mpt* :LINK
                                        :REL "Stylesheet"
                                        :TYPE "text/css"
                                        :HREF "clxman.css"))
                            (mpt* :BODY
                                  :BGCOLOR "white"
                                  :LINK "black"
                                  :VLINK "black"
                                  (apply #'mpt :DIV
                                         (mapcan (lambda (sec)
                                                   (cond ((= (section-level sec) 1)
                                                          (list
                                                           (mpt* :BR)
                                                           (make-hyper-link-for-anchor (section-anchor sec)
                                                                                       (mpt* :B (section-title sec)))
                                                           (mpt* :BR)))
                                                         (t
                                                          (list
                                                           (make-cdata (make-array 5 :element-type (array-element-type "")
                                                                                   :initial-element (code-char 160)))
                                                           (make-hyper-link-for-anchor (section-anchor sec)
                                                                                       (section-title sec))
                                                           (mpt* :BR)))))
                                                 secs))))
                      sink)))
    (setq *defun-index* (sort-index *defun-index*))
    (patch-index-sections secs *defun-index*)
    (mapc #'(lambda (x) 
	      (princ ".")(finish-output)
	      (dump-section x))
	  secs)

    ;;
    (when *use-frames-p*
      (maphash (lambda (key anchor)
                 (declare (ignore key))
                 (with-open-file (sink (merge-pathnames (anchor-url anchor) *destination-directory*)
                                       :direction :output
                                       :if-exists :new-version)
                   (write-string "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">" sink)
                   (terpri sink)
                   (write-string "<!-- This file is automatically generated, do not edit -->" sink)
                   (terpri sink)
                   (sgml-unparse *clxman-dtd*
                                 (mpt* :HTML
                                       :LANG "en"
                                       ;;(make-head sec) zzz
                                       (mpt* :frameset
                                             :cols "300, *"
                                             (mpt* :frame :src "full-contents.html")
                                             (mpt* :frameset
                                                   :rows "50, *"
                                                   (mpt* :frame
                                                         :src (format nil "h-~A" (anchor-page-url anchor))
                                                         ;; :marginwidth 0
                                                         :marginheight 0
                                                         :noresize :noresize
                                                         :scrolling :no
                                                         :frameborder 0)
                                                   (mpt* :frame
                                                         :src (format nil "b-~A#~A"
                                                                      (anchor-page-url anchor)
                                                                      (anchor-html-name anchor))
                                                         :scrolling :yes
                                                         :frameborder 0)))
                                       #+NIL
                                       body)
                                 sink)))
               *anchors*))
    ;;
    (with-open-file (o "mrindex" :direction :output :if-exists :new-version)
      (dolist (k *defun-index*)
        (print (list "XLIB"
                     (index-entry-name k)
                     (pt-cdata* (index-entry-kind k))
                     (format nil "~A#~A" 
                             (section->url (index-entry-sec k)) 
                             (index-entry-name k)))
               o)))
    (when *include-applet-p*
      (generate-java-applet))

    (when *use-cgi-script-p*
      (generate-cgi-script))

    ;; finally copy the style sheet
    (ext:run-program "cp" (list (namestring (truename (merge-pathnames "clxman.css" *source-directory*)))
                                (namestring (truename *destination-directory*))))
    ;;
    (values)))


;;; generation of the cgi script

(defparameter *cgi-template*
  "#! /usr/bin/perl -w

#
# doc-index.pl
# copyright (c) 2002 by Deliana Foutekova <unjm@stud.uni-karlsruhe.de>
#

use CGI qw(param url);

# list of lines.
# each line is a list of things separated by ';'.
# these things are: package symbol kind url
@lines = (
~{\"~{~A;~^~}\"~^,~%~}
# ################
#\"XLIB;bell;Dellys Klingel;hallo.html\",
);

$keyword = param(\"q\");

if (!defined ($keyword)) {
  $keyword = \"\";
}

$keyword = lc($keyword);   # lowercase
chomp($keyword);           # remove trailing newline if any
$i = index($keyword, \":\"); # check for a preceeding package name
if ($i > 0) {              # exclude i == 0 which indicates a constant
  $i = $i + 1;
  $len = length($keyword) - $i;
  $keyword = substr($keyword, $i, $len);
}
$keyword =~~ s/^\\s+//;     # delete space chars at the beginning
$keyword =~~ s/\\s+$//;     # delete space chars at the end

print (\"keyword: $keyword\\n\");

@url_lines =  grep (/\\;$keyword\\;/i, @lines);

if (defined (@url_lines) && scalar(@url_lines) == 1) {

  $right_line = $url_lines[0];
  %right_tokens = get_items_related_to_keyword($right_line);
  $right_token = $right_tokens{\"url\"};
  http_302_with_url($right_token);
} elsif (defined (@url_lines) && scalar (@url_lines) > 1) {

  create_choose_hit_page(@url_lines);
} else {

  create_not_found_page($keyword);
}


sub http_302_with_url {
  my($document_url) = @_;
  my ($what_i_mean_base_url) = create_my_base_url();

  print (\"Status: 302 Moved temporarily\\r\\n\");
  print (\"Content-type: text/plain\\r\\n\");
  print (\"Location: $what_i_mean_base_url$document_url\\r\\n\\r\\n\");
  return 1;
}


sub create_not_found_page{
  my ($key, @links) = @_;
  my ($what_i_mean_base_url) = create_my_base_url();
  my ($script_url) = url();
  my (@matching_keywords_substring_search) = substring_search($key);
  my ($line);

  print_content_type_text_html();
  print_header(\"Not Found\", \"Not found: $key\");
  print(\"<form action=\\\"$script_url\\\" method=\\\"get\\\">\\r\\n\");
  print(\"Seek:\\r\\n\");
  print(\"<input maxlength=\\\"100\\\" size=\\\"35\\\" name=\\\"q\\\" value=\\\"$key\\\" type=\\\"text\\\">\\r\\n\");
  print(\"</form>\\r\\n\");
  print(\"\\r\\n\");

  if (scalar(@matching_keywords_substring_search) > 0) {
    print(\"<p>\\r\\n\");
    print(\"Or choose a topic (substring search):\\r\\n\");
    print(\"</p>\\r\\n\");
    print(\"<ul>\\r\\n\");
    foreach $line (@matching_keywords_substring_search) {
      my (%tokens) = %{$line};
      my ($keyword) = $tokens{\"keyword\"};
      my ($kind) = $tokens{\"kind\"};
      my ($url) = $tokens{\"url\"};

      print (\"<li><a href=\\\"$what_i_mean_base_url$url\\\"><b>$keyword</b></a>, <i>$kind</i></li>\\r\\n\");
    }
    print(\"</ul>\\r\\n\");
  }

  print_footer();
}

sub create_choose_hit_page {
  my (@lines) = @_;
  my ($line);
  my ($what_i_mean_base_url) = create_my_base_url();

  @lines = convert_items_from_string_to_hashtable(@lines);

  print_content_type_text_html();
  print_header(\"Choose proper hit\", \"Please choose the right hit:\");

  print(\"<ul>\\r\\n\");
  foreach $line (@lines) {
    my (%tokens) = %{$line};
    my ($keyword) = $tokens{\"keyword\"};
    my ($kind) = $tokens{\"kind\"};
    my ($url) = $tokens{\"url\"};

    print(\"<li><a href=\\\"$what_i_mean_base_url$url\\\"><b>$keyword</b></a>, <i>$kind</i></li>\\r\\n\");
  }
  print(\"</ul>\\r\\n\");

  print_footer();
}

sub closest_n_keywords {
  my($n, $key) = @_;
  #does nothing at the moment. But may do something in the future.
}

sub substring_search {
  my($the_string) = @_;
  my ($line);
  my (@substrings);

  if ($the_string =~~ /.*\\-.*/ ||   # check if the_string contains a dash
      $the_string =~~ /^:/) {       # or the_str begins with \":\"
    foreach $line (@lines) {
      my (%line_items) = get_items_related_to_keyword($line);
      my ($kword) = $line_items{\"keyword\"};

      my ($i) = index($kword, $the_string);
      if ($i >= 0) {
	push(@substrings, \\%line_items);
      }
    }
  } else {
    foreach $line (@lines) {
      my (%line_items) = get_items_related_to_keyword($line);
      my ($kword) = $line_items{\"keyword\"};
      $kword =~~ s/://;  # ignore leading ':'

      my (@kword_components) = split(/-/,$kword);
      my (@lines_beginning_with_the_string) = grep (/\\b$the_string/, @kword_components);
      if (scalar (@lines_beginning_with_the_string) > 0) {
	push (@substrings, \\%line_items);
      }
    }
  }

  return @substrings;
}

sub get_items_related_to_keyword {
  my($line) = @_;
  my(@tokens);
  my (%tokenshash);
  my($right_token);
  my($i);

  @tokens = split(/\\;/, $line);

  %tokenshash = (\"package\", $tokens[0], \"keyword\", $tokens[1], \"kind\", $tokens[2], \"url\", $tokens[3]);
  return %tokenshash;
}

sub convert_items_from_string_to_hashtable {
  my (@stringlist) = @_;
  my ($line);
  my (@res);

  foreach $line (@stringlist) {
    my (%line_items) = get_items_related_to_keyword($line);
    push (@res, \\%line_items);
  }

  return @res;
}


sub create_my_base_url {
  my ($myurl);
  my($relative_url);
  my($what_i_mean_base_url);
  my ($i);

  # xxx
  # return \"http://www.uni-karlsruhe.de/%7eunk6/clxman/\";

  $myurl = url();
  $relative_url  = url(-relative=>1);
  $i = index($myurl, $relative_url);
  $what_i_mean_base_url = substr($myurl, 0, $i);

  return $what_i_mean_base_url;
}

sub print_content_type_text_html {
  print(\"Content-type: text/html\\r\\n\");
  print(\"\\r\\n\");
}

sub print_header {
  my ($title, $h1_tag) = @_;
  print(\"<!DOCTYPE HTML PUBLIC \\\"-//W3C//DTD HTML 4.01 Transitional//EN\\\">\\r\\n\");
  print(\"<html>\\r\\n\");
  print(\"<head>\\r\\n\");
  print(\"<title>$title</title>\\r\\n\");
  print(\"<meta http-equiv=\\\"Content-Type\\\" content=\\\"text/html; charset=iso-8859-1\\\">\\r\\n\");
  print (\"<LINK REL=\\\"Stylesheet\\\" TYPE=\\\"text/css\\\" HREF=\\\"clxman.css\\\">\\r\\n\");
  print(\"</head>\\r\\n\");
  print(\"\\r\\n\");
  print(\"<body>\\r\\n\");
  print(\"<h1>$h1_tag</h1>\\r\\n\");
}

sub print_footer {
  print(\"\\r\\n\");
}
")

(defun generate-cgi-script ()
  (let ((filename (merge-pathnames "doc-index.cgi" *destination-directory*)))
    (with-open-file (sink filename
                          :if-exists :new-version
                          :direction :output)
      (format sink *cgi-template*
              (mapcar (lambda (k)
                        (list "XLIB"
                              (index-entry-name k)
                              (pt-cdata* (index-entry-kind k))
                              (format nil "~A#~A" 
                                      (section->url (index-entry-sec k)) 
                                      (index-entry-name k))))
                      *defun-index*))
      (ext:run-program "chmod" (list "a+rx" (namestring (truename filename)))))))
