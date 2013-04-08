;;; XML parsing
;;;
;;; Copyright (C) 2000-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: xml.lisp,v 2.54 2005/09/02 16:01:26 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/xml.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `+whitespace+', `whitespace-char-p'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `substitute-subseq-if'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; print CLOS objects readably
  (require :cllib-closio (translate-logical-pathname "cllib:closio"))
  ;; `with-timing', `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `read-from-stream'
  (require :cllib-fileio (translate-logical-pathname "cllib:fileio"))
  ;; `open-url'
  (require :cllib-url (translate-logical-pathname "cllib:url"))
  ;; `required-argument'
  (require :port-ext (translate-logical-pathname "port:ext"))
  ;; `socket'
  (require :port-net (translate-logical-pathname "port:net"))
  (require :port-gray (translate-logical-pathname "port:gray")))

(in-package :cllib)

(export
 '(*xml-readtable* *xml-print-xml* *xml-read-balanced* *xml-read-entities*
   xml-xhtml-tidy xml-purge-data xmlize-string with-tag
   with-xml-input with-xml-file xml-read-from-file read-standalone-char
   xml-obj xml-obj-p xmlo-args xmlo-name xmlo-data do-xmlo-data
   xmlo-name-check xmlo-nm xmlo-tag
   xml-name xml-name-p xmln-ln xmln-ns xmln=
   xml-namespace xml-namespace-p xmlns-uri xmlns-pre xmlns-nht))

;;;
;;; Entities
;;;

(defpackage xml-tags (:use))
(defcustom *xml-pack* package (find-package :xml-tags)
  "The package with all the XML tags and entities.")
(defcustom *xml-amp* hash-table (make-hash-table :test 'equal)
  "The `&' entities (`general').")
(defcustom *xml-per* hash-table (make-hash-table :test 'equal)
  "The `%' entities (`parameter').")
(defcustom *xml-ent-file* pathname
  (translate-logical-pathname "cllib:entities.xml")
  "*The file with the default entities, like &gt; and &amp;.
See <http://www.w3.org/TR/WD-html40-970708/sgml/entities.html>.")
(defcustom *xml-keep-comments* boolean nil
  "*When non-nil, keep the comments inside the XML-OBJ structure.")

(defun xml-init-entities (&key (out *standard-output*))
  "Clear both `*xml-amp*' and `*xml-per*' and then read `*xml-ent-file*'."
  (clrhash *xml-amp*) (clrhash *xml-per*)
  (xml-read-from-file *xml-ent-file* :reset-ent nil :out out))

(defun xml-read-entity (stream)
  "read <!ENTITY ....>"
  (let ((ch (peek-char t stream)) (ht *xml-amp*) ent type data def)
    (when (char= ch #\%)
      (read-char stream) (peek-char t stream) (setq ht *xml-per*))
    (setq ent (xml-read-text stream #'whitespace-char-p :clean nil)
          type (read stream)
          data (if (symbolp type) (read stream) type))
    (check-type data string)
    (setf def
          (cond
            ((eq type 'xml-tags::system)
             (lambda (&optional junk)
               (declare (ignore junk))
               (let ((str (handler-case
                              (if (url-string-p data)
                                (open-url (url data))
                                (open (merge-pathnames data (xml-path stream))
                                      :direction :input))
                            (error (err)
                              (mesg :err t "~s: cannot open file [~s]/[~s]~%"
                                    'xml-read-entity data (xml-path stream t))
                              (error err)))))
                 (mesg :log t "~& * [~a ~:d bytes]..."
                       data (stream-length str))
                 str)))
            ((eq type 'xml-tags::cdata)
             (lambda (&optional string)
               (if string data (make-string-input-stream data))))
            ((symbolp type)
             (error 'code :proc 'xml-read-entity :mesg
                    "[~a]: not implemented: type: ~s data: ~s"
                    :args (list stream type data)))
            ((setq data (substitute #\" #\' data)) ; `nsubstitute'?
             (lambda (&optional string)
               (if string data (make-string-input-stream data))))))
    ;; finish reading - till #\>
    (xml-read-text stream #\> :clean nil) (read-char stream)
    (multiple-value-bind (val fp) (gethash ent ht)
      (when (and fp (not (equalp val def)))
        (warn "[~s]: redefining ~s [~c]" ; from ~s to ~s
              'xml-read-entity ent (if ch #\& #\%)))) ; val def
    (setf (gethash ent ht) def)
    ent))

(defun xml-entity (ent hash type &key (proc 'xml-entity) string)
  "Find the entity in the hash table."
  (multiple-value-bind (val fp) (gethash ent hash)
    (unless fp
      (warn "[~s]: ~c entity ~s undefined" proc type ent))
    (typecase val
      (function (funcall val string))
      (null "??")
      (t (error 'code :proc proc :args (list type ent val) :mesg
                "~c entity ~s was defined as ~s" )))))

(defun xml-expand-entities (string &key (start 0) end)
  "Substitute the expansions of all the entities in the string."
  (substitute-subseq-if
   string (lambda (seq beg fin)
            (position #\& seq :start beg :end fin :test #'char=))
   (lambda (seq beg fin) (1+ (position #\; seq :start beg :end fin)))
   (lambda (seq beg fin type)
     (declare (ignore type))
     (xml-entity (subseq seq (1+ beg) (1- fin)) *xml-amp* #\&
                 :proc 'xml-expand-entities :string t))
   :start start :end end))

(defun xmlize-string (string)
  "Replace XML-special characters, like &<>, with entities."
  (dolist (re '(("&" . "&amp;") ; & must come first!
                (">=" . "&gt;=") ("<=" . "&lt;=")
                (">" . "&gt;") ("<" . "&lt;")) string)
    (setq string (substitute-subseq string (car re) (cdr re)
                                    :test #'char=))))

;;;
;;; XML objects
;;;

(defstruct (xml-misc)
  ;; XML object from the preamble which we cannot handle just yet
  (type nil :type symbol)
  (data nil :type list))        ; contents

(defcustom *xml-print-xml* symbol nil
  "*Set to non-NIL to print XML-OBJ for future parsing.
Note that the Unicode characters will NOT be printed as &#nnnn;.
If this is `:sgml', use maximum SGML compatibility.
If this is `:readably', print for the Lisp reader
  (you must also set `*print-circle*' to non-nil).")

(defun xml-print-readably-p ()
  (or *print-readably*
      (eq :readably *xml-print-xml*)))
;; do not check that *PRINT-CIRCLE* is non-NIL: when there are no
;; circularities, CLISP will reset *PRINT-CIRCLE* to NIL internally for
;; speedup, thus breaking this code gratuitously
;;           (or *print-circle*
;;               (error "~s is set to ~s, but ~s is ~s"
;;                      '*xml-print-xml* :readably '*print-circle* nil))

(defmethod print-object ((xm xml-misc) (out stream))
  (cond ((xml-print-readably-p) (call-next-method))
        (*xml-print-xml*
         (format out "<!~a" (symbol-name (xml-misc-type xm)))
         (dolist (dat (xml-misc-data xm))
           (write-char #\Space out)
           (typecase dat
             (symbol (write-string (symbol-name dat) out))
             (t (write dat :stream out :escape t))))
         (write-char #\> out))
        ((print-unreadable-object (xm out :type t :identity t)
           (format out "~s [~:d object~:p]"
                   (xml-misc-type xm) (length (xml-misc-data xm)))))))

(eval-when (compile load eval)  ; ACL
(defcustom *xml-pre-namespaces* hash-table (make-hash-table :test 'equal)
  "The mapping from prefixes to namespaces.")

(defcustom *xml-uri-namespaces* hash-table (make-hash-table :test 'equal)
  "The mapping from URIs to namespaces.")
)

(defstruct (xml-comment)
  (data "" :type string))

(defstruct (xml-namespace (:conc-name xmlns-))
  (uri "" :type string)
  (pre (princ-to-string (gensym "NS")) :type string)
  (nht (make-hash-table :test 'equal) :type hash-table)) ; names

(defun xmlns-get (uri &rest opts &key pre-tmp (out *standard-output*)
                  &allow-other-keys)
  "Get the XML namespace or create a new one.
Add it to `*xml-pre-namespaces*' and `*xml-uri-namespaces*'."
  (multiple-value-bind (ns oldp)
      (typecase uri
        (xml-namespace (values uri t))
        (string (gethash uri *xml-uri-namespaces*)))
    (unless oldp
      (setq ns (apply #'make-xml-namespace :uri uri
                      (remove-plist opts :pre-tmp :out)))
      ;; only newly created namespaces have to be reported to the user
      (mesg :xml out "~& * added XML namespace: ~s~@[ [prefix ~s]~]~%"
            ns pre-tmp))
    (setf (gethash (xmlns-uri ns) *xml-uri-namespaces*) ns)
    (pushnew ns (gethash (xmlns-pre ns) *xml-pre-namespaces*))
    (when pre-tmp
      (pushnew ns (gethash pre-tmp *xml-pre-namespaces*)))
    ns))

(defconst +xml-namespace-xml+ xml-namespace
  (xmlns-get "http://www.w3.org/XML/1998/" :pre "xml" :out nil)
  "The XML namespace, as per 'Namespaces in XML' '4: Using Qualified Names'.
<URL:http://www.w3.org/TR/REC-xml-names/#ns-using>.")
(defconst +xml-namespace-none+ xml-namespace
  (xmlns-get "" :pre "" :out nil)
  "The namespace for unqualified names.")
(defcustom *xml-default-namespace* xml-namespace +xml-namespace-none+
  "The default namespace.")

(defstruct (xml-name (:conc-name xmln-))
  (ln "" :type string)          ; local name
  (ns +xml-namespace-none+ :type xml-namespace))

(defgeneric xmln= (xn1 xn2)
  (:documentation "Check whether the two names are the same.")
  (:method ((xn1 t) (xn2 t)) nil)
  (:method ((xn1 string) (xn2 string)) (string= xn1 xn2))
  (:method ((xn1 xml-name) (xn2 xml-name))
    (and (string= (xmln-ln xn1) (xmln-ln xn2))
         (eq (xmln-ns xn1) (xmln-ns xn2))))
  (:method ((xn1 string) (xn2 xml-name))
    (and (eq +xml-namespace-none+ (xmln-ns xn2))
         (string= xn1 (xmln-ln xn2))))
  (:method ((xn1 xml-name) (xn2 string)) (xmln= xn2 xn1))
  (:method ((xn1 cons) (xn2 cons))
    (and (string= (car xn1) (car xn2))
         (string= (cadr xn1) (cadr xn2)))))

(defun xmln-get (name namespace)
  "Create an XML name and add it to the appropriate hashtable.
If such a name already exists, re-use it."
  (declare (type string name))
  (let ((ns (typecase namespace
              (xml-namespace namespace)
              (t (or (car (gethash namespace *xml-pre-namespaces*))
                     (error 'code :proc 'xmln-get :args (list namespace)
                            :mesg "~s does not name a namespace"))))))
    (or (gethash name (xmlns-nht ns))
        (let ((nm (make-xml-name :ln name :ns ns)))
          (setf (gethash (xmln-ln nm) (xmlns-nht (xmln-ns nm))) nm)
          (mesg :log t "~& * new XML name: ~s~%" nm)
          nm))))

(defun xmlns-print-all (&key (out *standard-output*))
  (declare (stream out))
  (format out " * ~:d XML namespaces present:~%"
          (hash-table-count *xml-uri-namespaces*))
  (let ((ii 0))
    (with-hash-table-iterator (iter *xml-uri-namespaces*)
      (loop (multiple-value-bind (re key val) (iter)
              (unless re (return))
              (format out " [~d] ~s -> ~s~%" (incf ii) key val))))))

(defun xmlns-reset ()
  (xmlns-print-all)
  (clrhash *xml-uri-namespaces*)
  (clrhash *xml-pre-namespaces*)
  (clrhash (xmlns-nht +xml-namespace-none+))
  (clrhash (xmlns-nht +xml-namespace-xml+))
  (xmlns-get +xml-namespace-none+)
  (xmlns-get +xml-namespace-xml+)
  (xmlns-print-all))

(defsubst xmln-prefix (xmln)
  "Return the prefix for the name."
  (declare (type xml-name xmln))
  (let ((ns (xmln-ns xmln)))
    (unless (eq ns +xml-namespace-none+) (xmlns-pre ns))))

(defmethod print-object ((cmt xml-comment) (out stream))
  (cond ((xml-print-readably-p) (call-next-method))
        (*xml-print-xml*
         (format out "~&<!-- ~a -->~%" (xml-comment-data cmt)))
        ((print-unreadable-object (cmt out :type t :identity t)
           (format out "~:d char~:p" (length (xml-comment-data cmt)))))))

(defmethod print-object ((ns xml-namespace) (out stream))
  (if (xml-print-readably-p) (call-next-method)
      (print-unreadable-object (ns out :type t :identity t)
        (format out "~s ~s ~:d" (xmlns-uri ns) (xmlns-pre ns)
                (hash-table-count (xmlns-nht ns))))))

(defmethod print-object ((xmln xml-name) (out stream))
  (cond ((xml-print-readably-p) (call-next-method))
        (*xml-print-xml*
         (format out "~@[~a:~]~a" (xmln-prefix xmln) (xmln-ln xmln)))
        ((eq +xml-namespace-none+ (xmln-ns xmln))
         (format out ":~a:" (xmln-ln xmln)))
        ((format out "~a:~a" (xmln-ns xmln) (xmln-ln xmln)))))

(defstruct (xml-tag (:conc-name xmlt-))
  ;; `string' and `cons' are replaced with `xml-name' during
  ;; `xml-resolve-namespaces'
  (name (required-argument) :type (or string cons xml-name))
  (args nil :type list))        ; alist of arg/value

(defmethod print-object ((xmlt xml-tag) (out stream))
  (cond ((xml-print-readably-p) (call-next-method))
        (*xml-print-xml*
         (format out "~a~:{ ~a=~s~}" (xmlt-name xmlt) (xmlt-args xmlt)))
        ((format out "~a [~:{~a=~s~:^ ~}]"
                 (xmlt-name xmlt) (xmlt-args xmlt)))))

(defstruct (xml-decl (:include xml-tag)))

(defstruct (xml-obj (:include xml-tag) (:conc-name xmlo-))
  (data nil :type list))        ; list of objects in the tag

(defun xmlo-nm (tag)
  "Return the string name of the XML tag."
  (let ((nm (xmlo-name tag)))
    (etypecase nm
      (string nm)
      (cons (car nm))
      (xml-name (xmln-ln nm)))))

(defun xmlo-tag (tag name)
  "Get the value of the named arg in the XML tag."
  (cadr (assoc name (xmlo-args tag) :test #'xmln=)))

(defmethod print-object ((xml xml-decl) (out stream))
  (cond ((xml-print-readably-p) (call-next-method))
        (*xml-print-xml* (princ "<?" out) (call-next-method) (princ "?>" out))
        ((print-unreadable-object (xml out :type t :identity t)
           (call-next-method)))))

(defsubst xmlo-long-p (obj)
  (declare (type xml-obj obj))
  (or (xmlo-data obj) (eq *xml-print-xml* :sgml)))

(defun xmlo-name-check (obj name)
  "Make sure that OBJ is an XML-OBJ named NAME and return it."
  (assert (and (xml-obj-p obj) (string= name (xmlo-nm obj)))
          (obj) "~s is not an ~s named ~s" obj 'xml-obj name)
  obj)

(defun xml-size (obj)
  "Compute the approximate size of the object.
The first number returned is `text sise' (no tags)
the second is `file size' (including tags).
The third value is the number of sub-elements"
  (flet ((xmlt-size (oo)
           (reduce #'+ (xmlt-args oo)
                   :key (lambda (att)
                          (+ 4 (xml-size (car att)) ; #\Space=""
                             (length (cadr att))))
                   :initial-value (xml-size (xmlt-name oo)))))
    (typecase obj
      (string (let ((ll (length obj))) (values ll ll 1)))
      (sequence
       (let ((ts 0) (fs 0) (ne 0))
         (map nil (lambda (oo)
                    (multiple-value-bind (aa bb cc) (xml-size oo)
                      (incf ts aa) (incf fs bb) (incf ne cc)))
              obj)
         (values ts fs ne)))
      (symbol (xml-size (symbol-name obj)))
      (xml-name (values 0 (+ (length (xmln-ln obj))
                             (if (eq +xml-namespace-none+ (xmln-ns obj)) 0
                                 (+ 1 (length (xmln-prefix obj)))))
                        1))
      (xml-obj
       (multiple-value-bind (ts fs ne) (xml-size (xmlo-data obj))
         (values ts (+ fs (xmlt-size obj)
                       (if (xmlo-long-p obj) ; <tag></tag> vs <tag/>
                           (+ 5 (xml-size (xmlt-name obj))) 3))
                (1+ ne))))
      (xml-decl (values 0 (+ 4 (xmlt-size obj)) 1)) ; <??>
      (xml-tag (warn "~s: standalone tag: ~s~%" 'xml-size obj)
               (values 0 (xmlt-size obj) 1))
      (xml-misc (values 0 0 1))
      (xml-comment (let ((vv (+ 10 (length (xml-comment-data obj)))))
                     (values vv vv 1))) ; <!--  -->#\Newline
      (t (error 'case-error :proc 'xml-size :args
                (list 'obj obj 'string 'symbol 'sequence 'xml-name 'xml-obj
                      'xml-decl 'xml-tag 'xml-misc 'xml-comment))))))

(defun xml-ascii-p (char) (> 256 (char-code char)))

(defun xml-de-unicode (string)
  "Replace the unicode characters with &#NNNN;"
  (substitute-subseq-if
   string
   (lambda (str beg end)
     (position-if (complement #'xml-ascii-p) str :start beg :end end))
   (lambda (str beg end) (declare (ignore str end)) (1+ beg))
   (lambda (str beg end type)
     (declare (ignore end type))
     (format nil "&#~d;" (char-code (char str beg))))))

(defun xmlo-terpri-p (xo)
  "Check whether the XML object wants a newline."
  (declare (type xml-obj xo))
  (let ((name (xmln-ln (xmlo-name xo))))
    (or
     ;; (X)HTML
     (string= name "p") (string= name "br") (string= name "dt")
     (string= name "dd") (string= name "li") (string= name "td")
     (string= name "table") (string= name "tr") (string= name "div")
     (and (= 2 (length name)) (char= (aref name 0) #\h) ; header
          (digit-char-p (aref name 1)))
     ;; DocBook/XML
     (search "para" name) (search "list" name)
     (string= name "title") (string= name "section") (string= name "chapter")
     (string= name "row") (string= name "entry"))))

(defmethod print-object ((xml xml-obj) (out stream))
  (cond ((xml-print-readably-p) (call-next-method))
        (*xml-print-xml*
         (princ "<" out) (call-next-method)
         (cond ((xmlo-long-p xml)
                (princ ">" out)
                (dolist (dd (xmlo-data xml))
                  (princ (typecase dd (string (xml-de-unicode dd)) (t dd))
                         out))
                (format out "</~a>" (xmlt-name xml)))
               (t (princ "/>" out)))
         (when (xmlo-terpri-p xml)
           (terpri out)))
        ((print-unreadable-object (xml out :type t :identity t)
           (multiple-value-bind (ts fs ne) (xml-size xml)
             (call-next-method)
             (format out " ~:d/~:d object~:p ~:d/~:d chars"
                     ne (length (xmlo-data xml)) ts fs))))))

(declaim (ftype (function (t xml-obj) (values xml-obj)) xml-push))
(defun xml-push (new xml)
  "Add NEW to data in XML."
  (declare (type xml-obj xml))
  (typecase new
    (xml-obj (push new (xmlo-data xml)))
    (xml-decl (push new (xmlo-data xml)))
    (string (unless (zerop (length new))
              (if (stringp (car (xmlo-data xml)))
                  (let ((last (pop (xmlo-data xml))))
                    (push (concatenate 'string last new) (xmlo-data xml)))
                  (push new (xmlo-data xml)))))
    (xml-comment (when *xml-keep-comments* (push new (xmlo-data xml))))
    (cons (assert (equal (car new) (xmlo-name xml)) (new)
                  "~s: ~s was terminated by ~s" 'xml-push (xmlo-name xml) new)
          (setf (xmlo-data xml) (nreverse (xmlo-data xml))))
    (t (error 'case-error :proc 'xml-push :args
              (list 'new new 'xml-obj 'xml-decl 'string 'xml-comment 'cons))))
  xml)

(defmacro do-xmlo-data ((var obj &optional ret) &body forms)
  "Iterate over the elements in the DATA of OBJ:
 (do-xmlo-data (v zz)
    (\"foo\" bar)
    (\"zot\" baz))"
  (let (names got-t (data-name (gensym "DO-XMLO-DATA-")))
    `(dolist (,var (xmlo-data ,obj) ,ret)
       (let ((,data-name (xmlo-name ,var)))
         (cond
           ,@(mapcar (lambda (form)
                       (let ((name (car form)))
                         (cond ((stringp name)
                                (push name names)
                                (cons `(xmln= ,name ,data-name)
                                      (cdr form)))
                               (t (setq got-t t) form))))
                     forms)
           ,@(unless got-t
               `((t (cerror "ignore" "~S in ~S, expected one of ~S"
                            ,var ,obj ',names)))))))))

;;;
;;; XML streams
;;;

(defclass xml-stream-in (fundamental-character-input-stream)
  ((input :initarg :stream :initarg :input :type stream :accessor xmlis-st)
   (all-streams :type list :accessor xmlis-all)
   (tag-stack :type list :initform nil :accessor xmlis-stack)
   (size :type integer :initform 0 :accessor xmlis-size)
   (comment :accessor xmlis-comment :documentation
            "nil - outside comment, t - inside, 1 - inside, one `-' read"))
  (:documentation "The input stream for reading XML."))

(defun stream-length (st)
  "A wrap around for `file-length'."
  (etypecase st
    ((or file-stream #+allegro-v6.0 excl:file-simple-stream)
     (file-length st))
    (list (reduce #'+ st :key #'stream-length))
    (concatenated-stream (stream-length (concatenated-stream-streams st)))
    (string-stream 0)           ; can we do any better than this?
    (socket 0)))

(defmethod initialize-instance :after ((str xml-stream-in) &rest junk)
  (declare (ignore junk))
  (if (typep (xmlis-st str) 'concatenated-stream)
      (setf (xmlis-all str) (concatenated-stream-streams (xmlis-st str)))
      (setf (xmlis-all str) (list (xmlis-st str))
            (xmlis-st str) (make-concatenated-stream (xmlis-st str))))
  (setf (xmlis-size str) (stream-length (xmlis-st str))))

(defmethod stream-read-char ((in xml-stream-in))
  (read-char (xmlis-st in) nil :eof))
(defmethod stream-unread-char ((in xml-stream-in) (char character))
  (unread-char char (xmlis-st in)))
;; the default method is good enough,
;; but in Allegro CL it is not defined
(defmethod stream-read-char-no-hang ((in xml-stream-in))
  (read-char-no-hang (xmlis-st in) nil :eof))
(defmethod stream-peek-char ((in xml-stream-in))
  (peek-char nil (xmlis-st in) nil :eof))
(defmethod stream-listen ((in xml-stream-in))
  (listen (xmlis-st in)))
(defmethod stream-read-line ((in xml-stream-in))
  (read-line (xmlis-st in)))
(defmethod stream-clear-input ((in xml-stream-in))
  (clear-input (xmlis-st in)))
(defmethod close ((in xml-stream-in) &rest opts)
  (dolist (st (xmlis-all in)) (apply #'close st opts))
  (apply #'close (xmlis-st in) opts))
(defun xml-path (str &optional debug-p)
  (declare (type xml-stream-in str))
  (when debug-p
    (format t "~& * All streams:~{~%~s~}~% * Pending:~{~%~s~}~%"
            (xmlis-all str) (concatenated-stream-streams (xmlis-st str))))
  (dolist (st (concatenated-stream-streams (xmlis-st str)))
    (when (or (typep st 'file-stream)
              #+allegro-v6.0 (typep st 'excl:file-simple-stream))
      (when debug-p (format t " == ~s -> ~s~%" st (truename st)))
      (return (truename st)))))

(defun xmlis-push (newstr stream)
  "Push the stream NEWSTR into the beginning of STREAM
so that it is the first stream from which we will read."
  (flet ((dead-stream-p (str)
           (and (typep str 'string-stream)
                (or (not (open-stream-p str))
                    (and (eof-p str)
                         (close str))))))
    (setf (xmlis-st stream)
          (apply #'make-concatenated-stream newstr
                 (delete-if #'dead-stream-p
                            (concatenated-stream-streams (xmlis-st stream))))
          (xmlis-all stream)
          (cons newstr (delete-if #'dead-stream-p (xmlis-all stream))))
    (incf (xmlis-size stream) (stream-length newstr))
    stream))

;;;
;;; Reading
;;;

(defun compress-whitespace (list &optional ends)
  "Replace internal whitespace with #\\Space
and trim (when ENDS is non-NIL) the leading and trailing whitespace."
  (do ((ll list (cdr ll)) good)
      ((null ll)
       (when (and ends good) (setf (cdr good) nil))
       (if ends (member-if (complement #'whitespace-char-p) list) list))
    (if (whitespace-char-p (car ll))
        (loop :initially (setf (car ll) #\Space)
              :while (and (cdr ll) (whitespace-char-p (cadr ll)))
              :do (setf (cdr ll) (cddr ll)))
        (setq good ll))))

(defun xml-read-text (str term &key (clean t) base)
  "Read characters from stream STR until TERM.
Return a string (with whitespace compressed with `compress-whitespace'
if the keyword argument `clean' is non-NIL, which is the default, and
the unicode entities &#nnnn; replaced with the appropriate characters
base the keyword argument BASE, when it is non-NIL, default - NIL)
TERM can be a predicate, a character or a sequence of characters."
  (loop :with endp = (etypecase term
                       (function term)
                       (character (lambda (ch) (char= ch term)))
                       (sequence (lambda (ch) (find ch term :test #'char=))))
        :for ch = (read-char str nil nil t)
        :until (or (null ch) (funcall endp ch)) :collect ch :into list
        :finally
        (when ch (unread-char ch str))
        (when clean (setq list (compress-whitespace list)))
        (when base
          (do ((ll list (cdr ll))) ((null ll)) ; &#nnnn;
            (when (and (char= #\& (car ll)) (char= #\# (cadr ll)))
              (do* ((l1 (cddr ll) (cdr l1)) (nn 0))
                   ((char= #\; (car l1))
                    (setf (car ll) (code-char nn)
                          (cdr ll) (cdr l1)))
                (setq nn (+ (* nn base)
                            (digit-char-p (car l1) base)))))))
        (return (values (coerce list 'string) ch))))

(defun xml-read-comment (str)
  "We are inside a comment; read it and return as a string."
  (loop :with ch :for data = (xml-read-text str '#\- :clean nil)
        :collect data :into all :do (read-char str t nil t)
        :if (char= #\- (setq ch (read-char str t nil t))) :do
        (setq ch (read-char str t nil t))
        (assert (char= ch #\>) (ch) "~s[~a]: ~s instead of #\>"
                'xml-read-comment str ch)
        (return (reduce (lambda (s0 s1) (concatenate 'string s0 s1))
                        all :initial-value ""))
        :else :collect (concatenate 'string "-" (string ch)) :into all))

(defun xml-list-to-alist (list)
  "(x a #\: b #\= \"c\" d #\= \"e\" y) -->
   (x ((\"A\" \"B\") \"c\") (\"D\" \"e\") y)"
  ;; we do not resolve namespaces here (that is done during the
  ;; post-processing in `xml-resolve-namespaces') because of the
  ;; recursive nature of namespace resolution
  (flet ((tost (xx) (etypecase xx
                      (symbol (symbol-name xx)) (string xx)
                      (number (prin1-to-string xx)) ; for malformed border=1
                      (character (string xx)) ; for malformed href=http://
                      (cons xx))))
    (when (setq list (delete-if #'xml-comment-p list))
      (do ((ll list))
          ((null (cddr ll)) (setf (car list) (tost (car list))) list)
        (if (or (eql (cadr ll) #\:) (eql (cadr ll) #\=))
            (setf (cadr ll) (tost (car ll))
                  (car ll) (cdr ll) (cdr ll) (cdddr ll)
                  (cadar ll) (tost (cadar ll)) (cddar ll) nil)
            (setf (car ll) (tost (car ll)) ll (cdr ll)))))))

(defun xml-resolve-namespaces (obj &key (recursion-depth 0)
                               (out *standard-output*))
  "Resolve all names in the XML object.
Resolution is done according to `*xml-default-namespace*'
and `*xml-pre-namespaces*'."
  (etypecase obj
    (xml-comment obj) (xml-decl obj) (xml-misc obj) (string obj)
    (sequence (map-in (lambda (o) (xml-resolve-namespaces
                                   o :recursion-depth (1+ recursion-depth)
                                   :out out))
                      obj))
    (xml-obj
     (when (zerop recursion-depth)
       (mesg :xml-log out "~&~s [~s]: cleared ~s: ~s~%"
             'xml-resolve-namespaces (xmlt-name obj)
             '*xml-pre-namespaces* *xml-pre-namespaces*)
       (clrhash *xml-pre-namespaces*))
     (let ((pref (format nil "~s [~s/~:d]" 'xml-resolve-namespaces
                         (xmlt-name obj) recursion-depth))
           pref-list dns)
       (mesg :xml-log out "~a: defining the new namespaces~%" pref)
       (setf (xmlt-args obj)
             (delete-if
              (lambda (att)
                (if (consp (car att))
                    (when (string= "xmlns" (caar att))
                      (mesg :xml-log out "~a: ns: ~s~%" pref att)
                      (when (member (cdar att) pref-list :test #'eq)
                        (error 'code :proc 'xml-resolve-namespaces
                               :mesg "duplicate namespace ~s: ~s"
                               :args (list (cdar att) (xmlt-args obj))))
                      (let ((pref (cadar att)))
                        (push pref pref-list)
                        (xmlns-get (cadr att) :pre-tmp pref)))
                    (when (string= "xmlns" (car att))
                      (mesg :xml-log out "~a: dns: ~s~%" pref att)
                      (when dns
                        (error 'code :proc 'xml-resolve-namespaces
                               :mesg "duplicate default namespace: ~s"
                               :args (list (xmlt-args obj))))
                      (setq dns (xmlns-get (cadr att))))))
              (xmlt-args obj)))
       (let ((*xml-default-namespace* (or dns *xml-default-namespace*)))
         (mesg :xml-log out "~a: resolving the names~%" pref)
         (flet ((xmln-cons (obj default-ns)
                  (if (consp obj)
                      (xmln-get (cadr obj) (car obj))
                      (xmln-get obj default-ns))))
           (setf (xmlt-name obj) ; name
                 (xmln-cons (xmlt-name obj) *xml-default-namespace*))
           (do ((attr (xmlt-args obj) (cdr attr))) ; attributes
               ((null attr))
             ;; http://www.w3.org/TR/REC-xml-names/#defaulting
             ;; "Namespaces in XML": 5.2 "Namespace Defaulting"
             ;; "default namespaces do not apply directly to attributes"
             (mesg :xml-log out "~a: attr: ~s~%" pref (car attr))
             (setf (caar attr) (xmln-cons (caar attr) +xml-namespace-none+))))
         (mesg :xml-log out "~a: checking for identical attributes~%" pref)
         (do ((ll (xmlt-args obj) (cdr ll))) ((null ll))
           (do ((mm (cdr ll) (cdr mm))) ((null mm))
             (when (eq (caar mm) (caar ll))
               (error 'code :proc 'xml-resolve-namespaces
                      :mesg "duplicate attribute ~s: ~s"
                      :args (list (caar ll) (xmlt-args obj))))))
         (mesg :xml-log out "~a: processing the data~%" pref)
         (unwind-protect
              (dolist (oo (xmlo-data obj) obj)
                (typecase oo
                  (xml-obj (xml-resolve-namespaces
                            oo :recursion-depth (1+ recursion-depth)
                            :out out))))
           (dolist (pref pref-list)
             (pop (gethash pref *xml-pre-namespaces*)))))))))

(defcustom *xml-read-balanced* boolean t
  "*Require balanced tags. Bind to NIL when reading HTML.
When NIL, `xml-read-tag' is not recursive.")
(defcustom *xml-read-entities* boolean t
  "*Parse the entities.")

(defun xml-destructure-attributes (list)
  "Return the name, the attribute alist and the last element."
  (do* ((name (car list)) (last (car (last list)))
        (atts (if (eq 'xml-tags::/ last) (nbutlast (cdr list)) (cdr list)))
        (ll atts (cdr ll)))
       ((null ll) (values name atts last))
    (when (atom (car ll))
      (setf (car ll) (list (car ll) "true")))))

(defun xml-read-tag (str)
  "Read the tag, from <TAG-NAME> to </TAG-NAME>.
The first character to be read is #\T."
  (multiple-value-bind (name atts last)
      (xml-destructure-attributes
       (xml-list-to-alist (read-delimited-list #\> str t)))
    (let ((xml (make-xml-obj :name name :args atts)))
      (if (eq 'xml-tags::/ last) xml
          (if *xml-read-balanced*
              (loop :with next
                    :initially (push (xmlo-name xml) (xmlis-stack str))
                    :while (not (consp next)) :do
                    (xml-push (xml-read-text str "<&") xml)
                    (xml-push (setq next (read str t nil t)) xml)
                    :finally
                    (assert (equal (car next) (car (xmlis-stack str))) (next)
                            "~s[~a]: ~s terminated ~s" 'xml-read-tag str
                            next (xmlis-stack str))
                    (pop (xmlis-stack str))
                    (return xml))
              (xml-push (xml-read-text str "<&") xml))))))

(defun read-xml (stream char)
  (ecase char
    (#\/ 'xml-tags::/)
    (#\<                        ; read tag
     (let ((ch (read-char stream t nil t)) (*package* *xml-pack*))
       (case ch
         (#\/ (let ((tag (xml-list-to-alist
                          (read-delimited-list #\> stream t))))
                (assert (null (cdr tag)) (tag)
                        "~s[~a]: end tag ~s has attributes ~s" 'read-xml stream
                        (car tag) (cdr tag))
                (if *xml-read-balanced* tag
                    (progn (setf (cdr tag) (xml-read-text stream "<&"
                                                          :clean nil))
                           tag))))
         (#\? (multiple-value-bind (name atts last)
                  (xml-destructure-attributes
                   (xml-list-to-alist ; can a declaration have namespaces?
                    (read-delimited-list #\> stream t)))
                (assert (eq 'xml-tags::? last) ()
                        "~s[~a]: <? was terminated by ~s"
                        'read-xml stream last)
                (make-xml-decl :name name :args (nbutlast atts))))
         (#\!
          (case (peek-char nil stream)
            (#\-                ; comment <!-- ... -->
             (let ((ch (progn (read-char stream) (read-char stream t nil t))))
               (assert (char= #\- ch) (ch)
                       "~s: cannot handle: <!-~c" 'read-xml ch)
               (make-xml-comment :data (xml-read-comment stream))))
            (#\[                ; character data <![CDATA[ ... ]]>
             (let ((res (make-array 20 :adjustable t :element-type 'character
                                    :fill-pointer #.(length "[CDATA["))))
               (assert (and (= (read-sequence res stream) 7)
                            (string= res "[CDATA["))
                       (res) "~s: cannot handle: <!~a" 'read-xml res)
               (setf (fill-pointer res) 0)
               (loop :for len = (vector-push-extend
                                 (read-char stream t nil t) res)
                 :until (and (>= len 3) (string= "]]>" res :start2 (- len 2)))
                 :finally (setf (fill-pointer res) (- len 2)))
               res))
            (t (let ((obj (read stream t nil t)))
                 (case obj
                   (xml-tags::entity (make-xml-comment
                                      :data (xml-read-entity stream)))
                   ((xml-tags::doctype xml-tags::element xml-tags::attlist
                                       xml-tags::notation)
                    (make-xml-misc :type obj :data
                                   (read-delimited-list #\> stream t)))
                   (t (warn "~s: what is `~s'? proceed, with fingers crossed..."
                            'read-xml obj)
                      (cons obj (xml-list-to-alist
                                 (read-delimited-list #\> stream t)))))))))
         (t (unread-char ch stream)
            (xml-read-tag stream)))))
    ;; do not need `xml-list-to-alist' in <!DOCTYPE foo [...]>
    (#\[ (read-delimited-list #\] stream t)) ;
    ;;(#\> (funcall (get-macro-character #\)) stream char)
    ;;     (xml-read-text stream #\<))
    ((#\& #\%)
     (let* ((ent (xml-read-text stream #\;))
            (str (if (and (char= #\& char) (char= #\# (char ent 0)))
                     (let ((code (parse-integer ent :start 1)))
                       (if (< code char-code-limit)
                           (string (code-char code))
                           (concatenate 'string "&" ent ";")))
                     (xml-entity ent (case char
                                       (#\& *xml-amp*) (#\% *xml-per*))
                                 char :proc 'read-xml))))
       (read-char stream)       ; #\;
       (etypecase str
         (string str)           ; "??" for undefined entities and &#nnnn;
         (stream
          (xmlis-push str stream)
          (if (find (peek-char t stream t nil t) "&%<>" :test #'char=)
              (read stream t nil t)
              (values (xml-read-text stream "<&")))))))))

;;;
;;; UI
;;;

(defun read-standalone-char (stream char)
  (declare (ignore stream))
  char)

(defun make-xml-readtable (&optional (rt (copy-readtable)))
  "Return a readtable for reading XML."
  (set-macro-character #\< #'read-xml nil rt)
  (set-macro-character #\[ #'read-xml nil rt)
  (set-macro-character #\/ #'read-xml nil rt)
  (cond (*xml-read-entities*
         (set-macro-character #\& #'read-xml nil rt)
         (set-macro-character #\% #'read-xml nil rt))
        (t (set-syntax-from-char #\& #\Space rt)
           (set-syntax-from-char #\% #\Space rt)))
  ;; (set-macro-character #\> #'read-xml nil rt)
  (set-macro-character #\> (get-macro-character #\)) nil rt)
  (set-macro-character #\] (get-macro-character #\)) nil rt)
  ;; this is a hack, but it works under Allegro, CLISP and CMUCL
  (set-macro-character #\' (get-macro-character #\") nil rt)
  (flet ((single-char (char)
           (set-syntax-from-char char #\Space rt)
           (set-macro-character char #'read-standalone-char nil rt)))
    (single-char #\:)           ; handle namespaces
    (single-char #\|)           ; selector
    (single-char #\*)           ; repeater
    (single-char #\=))          ; attribute="value"
  (set-syntax-from-char #\; #\Space rt) ; for HTML documents
  (set-syntax-from-char #\, #\Space rt) ; for (a, b, c) in preamble
  (set-syntax-from-char #\# #\Space rt) ; for HTML documents
  (setf (readtable-case rt) :preserve)
  rt)

(defcustom *xml-readtable* readtable (make-xml-readtable)
  "The readtable for XML parsing.")

(defmacro with-xml-input ((var stream) &body body)
  "Open the XML stream, evaluate the forms, make sure the stream is closed."
  `(with-open-stream (,var (make-instance 'xml-stream-in :input ,stream))
    ;; do not reuse `*xml-readtable*' since the parsing should depend on
    ;; `*xml-read-entities*' and `*xml-read-balanced*'
    (let ((*readtable* (make-xml-readtable))) ,@body)))

(defmacro with-xml-file ((var file &key reset-ent (out '*standard-output*))
                         &body body)
  "Open the XML stream to file."
  (with-gensyms ("WXF-" ff)
    `(with-timing (:out ,out :type :xml)
      (when ,reset-ent (xml-init-entities :out ,out))
      (let ((,ff ,file))
        (with-xml-input (,var (open ,ff :direction :input))
          (mesg :xml ,out "~&[~s]~% * [~a ~:d bytes]..." 'with-xml-file
                ,ff (file-length (car (xmlis-all ,var))))
          (unwind-protect (progn ,@body)
            (mesg :xml ,out "done [entities(%/&): ~:d/~:d] [bytes: ~:d]"
                  (hash-table-count *xml-per*) (hash-table-count *xml-amp*)
                  (xmlis-size ,var))))))))

(defmacro with-tag ((tag &key options (out '*standard-output*) (terpri t)
                         (fresh-line nil))
                    &body body)
  "Print the tag with options and body."
  (with-gensyms ("WT-" str tg)
    `(let ((,str ,out) (,tg ,tag))
       (when ,fresh-line (fresh-line ,str))
       ,@(if body
             `((format ,str "<~A~{ ~A=\"~A\"~}>" ,tg ,options)
               ,@body
               (format ,str "</~A>" ,tg))
             `((format ,str "<~A~@{ ~A=\"~A\"~}/>" ,tg ,@options)))
       (when ,terpri (terpri ,str)))))

(defun xml-default-reset-entities ()
  "Check whether the entities need to be initialized."
  (and *xml-read-entities*
       (zerop (hash-table-count *xml-per*))
       (zerop (hash-table-count *xml-amp*))))

;;;###autoload
(defun xml-read-from-file (file &key (repeat t)
                           (reset-ent (xml-default-reset-entities))
                           (resolve-namespaces *xml-read-balanced*)
                           (out *standard-output*))
  "Read all XML objects from the file."
  (let ((obj (with-xml-file (str file :reset-ent reset-ent :out out)
               (read-from-stream str :repeat repeat))))
    (if resolve-namespaces
        (xml-resolve-namespaces obj :out out)
        obj)))

(defun xml-purge-data (obj)
  "Delete all non-XML objects from all data.
Useful when the data was in a pretty-printed format -
just kill whitespace strings."
  (typecase obj
    (xml-obj (setf (xmlo-data obj) (xml-purge-data (xmlo-data obj)))
             obj)
    (list (delete-if (lambda (oo)
                       (typecase oo
                         (xml-obj (xml-purge-data oo) nil) ; keep it
                         (string (every #'whitespace-char-p oo))))
                     obj))))

;;;
;;; XHTML
;;;

(defun xml-xhtml-remove-unused-ids (xo &key (out *standard-output*))
  "Remove unused IDs and NAMEs."
  (declare (type xml-obj xo))
  (let ((ids-def (make-hash-table :test 'equalp))
        (num-proc 0) (num-rem 0) (num-redef 0))
    ;; id --> (def . (uses list))
    (labels ((tag-name (tag) (xmln-ln (car tag)))
             (name-tag-p (tag) (or (string= tag "id") (string= tag "name")))
             (id-add (name ob defp)
               (let ((val (gethash name ids-def)))
                 (when (and defp (car val))
                   (incf num-redef)
                   (mesg :logv out "id ~s redefined in ~s, was defined in ~s"
                         name ob (car val)))
                 (if val
                     (if defp (push ob (car val)) (push ob (cdr val)))
                     (setf (gethash name ids-def)
                           (if defp
                               (cons (list ob) nil) (cons nil (list ob)))))))
             (handle-one (ob)
               (declare (type xml-obj ob))
               (incf num-proc)
               (dolist (tag (xmlo-args ob))
                 (let ((tag-name (tag-name tag)) (tag-val (cadr tag)))
                   (cond ((name-tag-p tag-name) ; definition
                          (id-add tag-val ob t))
                         ((and (string= (xmln-ln (xmlo-name ob)) "a") ; use
                               (string= tag-name "href")
                               (char= (aref tag-val 0) #\#))
                          (id-add (subseq tag-val 1) ob nil)))))
               (dolist (elt (xmlo-data ob))
                 (when (xml-obj-p elt)
                   (handle-one elt)))))
      (with-timing (:out out :type :xml)
        (mesg :xml out "scanning ~s..." xo)
        (handle-one xo)
        (mesg :xml out "done [~:d XML objects, ~:d IDs, ~:d redefined]"
              num-proc (hash-table-count ids-def) num-redef))
      (with-timing (:out out :type :xml)
        (mesg :xml out "removing unused IDs...")
        (with-hash-table-iterator (iter ids-def)
          (loop (multiple-value-bind (re kk vv) (iter)
                  (unless re (return))
                  (unless (cdr vv)  ; unused
                    (incf num-rem)
                    (mesg :logv out "~s: unused, removing~%" kk)
                    (dolist (def (car vv))
                      (setf (xmlo-args def)
                            (delete-if #'name-tag-p (xmlo-args def)
                                       :key #'tag-name)))))))
        (mesg :xml out "done [~:d removed]" num-rem))
    (values num-proc num-rem num-redef (hash-table-count ids-def)))))

;;;###autoload
(defun xml-xhtml-tidy (file &key (out *standard-output*) (output file)
                       (verbose nil))
  "Tidy up the XHTML file."
  (let ((all (xml-read-from-file file :out out))
        (*print-log* (if verbose *print-log* (remove :logv *print-log*))))
    (with-open-file (str output :direction :output)
      (dolist (obj all)
        (when (and (xml-obj-p obj)
                   (string= "html" (xmln-ln (xmlo-name obj))))
          (xml-xhtml-remove-unused-ids obj :out out))
        (with-timing (:out out :type :xml)
          (mesg :xml out "Writing ~s into ~s..." obj output)
          (let ((*xml-print-xml* t)) (write obj :stream str))
          (mesg :xml out "done [~:d bytes total]" (stream-length str)))
        (terpri str)))))

(provide :cllib-xml)
;;; file xml.lisp ends here
