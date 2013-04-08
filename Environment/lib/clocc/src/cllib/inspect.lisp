;;; Inspect
;;;
;;; Copyright (C) 2000-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: inspect.lisp,v 1.37 2005/12/05 15:16:08 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/inspect.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `list-length-dotted'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `class-slot-list'
  (require :cllib-closio (translate-logical-pathname "cllib:closio"))
  ;; `open-socket-server'
  (require :port-net (translate-logical-pathname "port:net"))
  ;; `browse-url', `flush-http'
  (require :cllib-url (translate-logical-pathname "cllib:url"))
  ;; `string-beg-with'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; `with-tag'
  (require :cllib-htmlgen (translate-logical-pathname "cllib:htmlgen")))

(in-package :cllib)

(export
 '(inspect-cllib *inspect-frontend* *inspect-browser* *inspect-print-lines*
   *inspect-print-level* *inspect-print-length* *inspect-length*))

;;;
;;; options
;;;

(defcustom *inspect-frontend* symbol :tty
  "*The default inspect frontend.
The possible values are
  :tty    use the text-only interface
  :http   use a web browser (see `*inspect-browser*')
To define a frontend, one has to define methods for
 `print-inspection' and `inspect-frontend'.")
(defcustom *inspect-browser* (or symbol list) nil
  "*The default browser to use with the `:http' `*inspect-frontend*'.
If nil, no browser is started - you will have to point your browser to
a URL supplied by `inspect-frontend'.
This way the browser and Lisp can run on different machines.
See `browse-url', `*browser*', and `*browsers*'.")
(defcustom *inspect-print-lines* fixnum 5
  "*The default value for `*print-lines*'.")
(defcustom *inspect-print-level* fixnum 5
  "*The default value for `*print-level*'.")
(defcustom *inspect-print-length* fixnum 10
  "*The default value for `*print-length*'.")
(defcustom *inspect-length* fixnum 5
  "*The number of sequence elements to print.")

(defparameter *inspect-all* nil) ; all `inspection' objects in this session
(defparameter *inspect-debug* 0) ; debug level
(defvar *inspect-unbound-value*) ; the value for the unbound slots

;;;
;;; backend
;;;

(defstruct (inspection (:conc-name insp-))
  self                          ; the object being inspected
  (id (fill-pointer *inspect-all*) :type fixnum) ; unique in a session
  (title "" :type string)       ; the short description of the object
  (blurb nil :type list)        ; list of strings with general information
  (up nil :type (or null inspection)) ; parent
  (num-slots 0 :type fixnum)    ; the number of slots
  (pos nil :type (or null fixnum)) ; pos in parent
  (nth-slot nil :type (or null (function (integer) (values t t)))) ; val & name
  (set-slot nil :type (or null (function (integer t) t)))) ; set Nth slot

(defun insp-check (insp)
  ;; this should always be okay, nevertheless
  ;; we use `warn' instead of `assert' or `error' because
  ;; the objects being inspected could possibly be modified
  ;; in another thread
  (let ((up (insp-up insp)) (pos (insp-pos insp)) (id (insp-id insp)))
    (unless (eq insp (aref *inspect-all* id))
      (warn "~s: ~s appears corrupted (~d->~d):~%~s~%~s~%"
            'get-insp '*inspect-all* id (insp-id (aref *inspect-all* id))
            insp (aref *inspect-all* id)))
    (when up
      (unless (< -1 pos (insp-num-slots up))
        (warn "~s: pos out of range: ~d ~d~%" 'insp-check pos
              (insp-num-slots up)))
      (unless (eq (funcall (insp-nth-slot up) pos) (insp-self insp))
        (warn "~s: slot ~d of the ~s has changed:~%~s~%"
              'insp-check pos (insp-self up) up)))))

(defun insp-last-slot (insp)
  (1- (insp-num-slots insp)))
(defun insp-num-slots-print (insp)
  (min (insp-last-slot insp) *inspect-length*))
(defun insp-left-p (insp)
  "Check for the presence of a left neighbor."
  (let ((pos (insp-pos insp)) (up (insp-up insp)))
    (and pos up (< 0 pos))))
(defun insp-right-p (insp)
  "Check for the presence of a right neighbor."
  (let ((pos (insp-pos insp)) (up (insp-up insp)))
    (and pos up (< pos (insp-last-slot up)))))

(defun set-slot-error (ii obj)
  (error "~s: Cannot set the slot number ~s for object ~s"
         'set-slot-error ii obj))

(defmacro with-nth-hash-slot (ht args1 args2 retform)
  (with-gensyms ("WNGS-" ii jj)
    `(lambda (,ii ,@args1)
      (block with-nth-hash-slot
       (let ((,jj -1))
         (maphash (lambda ,args2
                    (declare (ignorable ,@args2))
                    (when (= ,ii (incf ,jj))
                      (return-from with-nth-hash-slot ,retform)))
                  ,ht))))))

(defgeneric inspect-backend (object &rest opts)
  (:method ((obj array) &rest opts)
    (let* ((siz (array-total-size obj)) (type (array-element-type obj))
           (arr (make-array siz :displaced-to obj :element-type type)))
      (apply #'make-inspection :self obj :title
             (typecase obj (string "String") (vector "Vector") (t "Array"))
             :blurb (list (format nil "dimension~p:~{ ~:d~}"
                                  (array-rank obj) (array-dimensions obj))
                          (format nil "element-type: ~s" type)
                          (if (= 1 (array-rank obj))
                              (if (array-has-fill-pointer-p obj)
                                  (format nil "fill-pointer: ~:d"
                                              (fill-pointer obj))
                                  "no fill pointer")
                              (format nil "total size: ~:d" siz))
                          (multiple-value-bind (di off)
                              (array-displacement obj)
                            (if di (format nil "displaced to (~:d): ~s" off di)
                                "not displaced")))
             :num-slots siz :nth-slot (lambda (ii) (aref arr ii))
             :set-slot (lambda (ii val) (setf (aref arr ii) val))
             opts)))
  (:method ((obj hash-table) &rest opts)
    (let ((count (hash-table-count obj)))
      (apply #'make-inspection :self obj :title "Hash Table"
             :blurb (list (format nil "count: ~:d" count)
                          (format nil "size: ~:d" (hash-table-size obj))
                          (format nil "rehash-size: ~:d"
                                  (hash-table-rehash-size obj))
                          (format nil "rehash-threshold: ~:d"
                                  (hash-table-rehash-threshold obj))
                          (format nil "test: ~:d" (hash-table-test obj)))
             :num-slots count :set-slot
             (with-nth-hash-slot obj (val) (kk vv) (setf (gethash kk obj) val))
             :nth-slot
             (with-nth-hash-slot obj nil (kk vv) (values vv kk)) opts)))
  (:method ((obj cons) &rest opts)
    (multiple-value-bind (len dotted-p) (list-length-dotted obj)
      (apply
       #'make-inspection
       :num-slots (if len (if dotted-p (1+ len) len)
                      (1+ (position obj obj :test #'eq)))
       :nth-slot (lambda (ii) (if (and dotted-p (= ii len)) dotted-p
                                  (nth ii obj)))
       :set-slot (lambda (ii val)
                   (if (and dotted-p (= ii len))
                       (setf (cdr (nthcdr (1- ii) obj)) val)
                       (setf (nth ii obj) val)))
       :blurb (list (if len
                        (if dotted-p
                            (if (> len 1)
                                (format nil "a dotted list of length ~:d" len)
                                (format nil "a cons"))
                            (format nil "a list of length ~:d" len))
                        (format nil "a cyclic list")))
       :self obj :title (format nil "Cons") opts)))
  (:method ((obj symbol) &rest opts)
    (apply #'make-inspection :num-slots 2
           :nth-slot (lambda (ii)
                       (case ii
                         (0 (values (if (boundp obj)
                                        (symbol-value obj)
                                        *inspect-unbound-value*)
                                    :symbol-value))
                         (1 (values (symbol-plist obj) :symbol-plist))))
           :set-slot (lambda (ii val)
                       (case ii
                         (0 (setf (symbol-value obj) val))
                         (1 (setf (symbol-plist obj) val))))
           :blurb (list (format nil "package: ~s" (symbol-package obj)))
           :self obj :title "Symbol" opts))
  (:method ((obj structure-object) &rest opts)
    (let ((slots (class-slot-list obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (slot-value obj slot) slot)))
             :set-slot (lambda (ii val)
                         (setf (slot-value obj (nth ii slots)) val))
             :self obj :title "structure object"
             :blurb (list (format nil "type: ~s" (type-of obj))) opts)))
  (:method ((obj standard-object) &rest opts)
    (let ((slots (class-slot-list obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (if (slot-boundp obj slot)
                                       (slot-value obj slot)
                                       *inspect-unbound-value*)
                                   slot)))
             :set-slot (lambda (ii val)
                         (setf (slot-value obj (nth ii slots)) val))
             :self obj :title "standard object"
             :blurb (list (format nil "type: ~s" (type-of obj))) opts)))
  (:method ((obj ratio) &rest opts)
    (apply #'make-inspection :self obj :title "rational number"
           :num-slots 2 :nth-slot
           (lambda (ii)
             (if (zerop ii)
                 (values (numerator obj) 'numerator)
                 (values (denominator obj) 'denominator)))
           :set-slot #'set-slot-error opts))
  (:method ((obj complex) &rest opts)
    (apply #'make-inspection :self obj :title "complex number"
           :num-slots 2 :nth-slot
           (lambda (ii)
             (if (zerop ii)
                 (values (realpart obj) 'realpart)
                 (values (imagpart obj) 'imagpart)))
           :set-slot #'set-slot-error opts))
  (:method ((obj t) &rest opts)
    (apply #'make-inspection :self obj :title "atom"
           :blurb (list (format nil "type: ~s" (type-of obj))
                        (format nil "class: ~s" (class-of obj))) opts))
  (:method :around ((obj t) &key id &allow-other-keys)
    (or (and (not id) (find obj *inspect-all* :key #'insp-self))
        (let ((insp (call-next-method)))
          (when (> *inspect-debug* 0)
            (format t "~s [id: ~:d, forced: ~s]: ~s~%" 'inspect-backend
                    (insp-id insp) id (insp-self insp)))
          (if id (setf (aref *inspect-all* id) insp)
              (vector-push-extend insp *inspect-all*))
          insp))))

(defun get-insp (id-or-insp com)
  "Get the INSPECTION object from the ID (or inspection object) and COMmand."
  (let ((insp (etypecase id-or-insp
                (inspection id-or-insp)
                (fixnum (aref *inspect-all* id-or-insp)))))
    (when insp
      (insp-check insp)
      (case com
        (:q :q)
        (:s                     ; re-inspect Self
         (inspect-backend (insp-self insp) :up (insp-up insp)
                          :pos (insp-pos insp) :id (insp-id insp)))
        (:u (insp-up insp))
        (:l (when (insp-pos insp)
              (get-insp (insp-up insp) (1- (insp-pos insp)))))
        (:r (when (insp-pos insp)
              (get-insp (insp-up insp) (1+ (insp-pos insp)))))
        (t (when (and (integerp com) (< -1 com (insp-num-slots insp)))
             (inspect-backend (funcall (insp-nth-slot insp) com)
                              :up insp :pos com)))))))

;;;
;;; frontends - common
;;;

(defgeneric print-inspection (insp out frontend &rest opts)
  (:documentation "This function prints inspection objects for the user.
It should be appropriate for your frontend (use `eql' specifiers).
The default method signals an error, so you must define a method
for your frontend.")
  (:method ((insp inspection) (out stream) (frontend t) &rest opts)
    (error "~s: unknown inspect front end: ~s [~s ~s]"
           'print-inspection frontend out opts)))

(defgeneric inspect-frontend (insp frontend)
  (:documentation "This generic function implements the inspect loop.
The default method signals an error, so you must define a method
for your frontend.")
  (:method ((insp inspection) (frontend t))
    (error "~s: unknown inspect front end: ~s" 'inspect-frontend frontend)))

(defgeneric inspect-finalize (frontend)
  (:documentation "This should clean after an inspect session.
You do not have to define methods for this function.")
  (:method ((frontend t))))

(defun inspect-read-clean-eval (insp stream)
  "Read a form, massage it, then evaluate it.
 - `read' a form,
 - destructively replace `:self' with INSP and
                         `:slot' with the appropriate `funcall',
 - `eval'
This is useful for frontends which provide an eval/modify facility."
  (labels ((clean (form)
             (cond ((eq (car form) :self)
                    (setf (car form) (list 'quote (insp-self insp)))
                    (clean-up (cdr form)))
                   ((eq (car form) :slot)
                    (setf (car form) 'funcall
                          (cdr form) (cons (insp-nth-slot insp) (cdr form)))
                    (clean-up (cddr form)))
                   (t (clean-up (car form))
                      (clean-up (cdr form)))))
           (clean-up (form) (when (consp form) (clean form)) form))
    (eval (clean-up (read stream nil nil)))))

;;;
;;; TTY frontend
;;;

(defmethod print-inspection ((insp inspection) (out stream)
                             (backend (eql :tty)) &rest opts)
  (declare (ignore opts))
  (format out "~&~s:  ~a~%~{ ~a~%~}" (insp-self insp) (insp-title insp)
          (insp-blurb insp))
  (when (insp-nth-slot insp)
    (loop :for ii :from 0 :to (insp-num-slots-print insp)
          :do (multiple-value-bind (val name) (funcall (insp-nth-slot insp) ii)
                (format out "~d~@[ [~a]~]:  ~s~%" ii name val)))))

(defmethod inspect-frontend ((insp inspection) (frontend (eql :tty)))
  (print-inspection insp *terminal-io* frontend)
  (do (com (id (insp-id insp)))
      ((eq com :q))
    (fresh-line)
    (princ "INSPECT-- type :h for help; :q to return to the REPL ---> ")
    (force-output)
    (case (setq com (read *terminal-io* nil :q))
      (:q)
      ((:h :?) (format t " *** commands:~% :h, :?~15t this help
 :p, :a~15t Print the current item Again
 :s~15t re-inspect this item (Self)
 :d~15t Describe the current item~%")
       (when (insp-up insp)
         (format t " :u~15t return UP to the parent~%"))
       (when (insp-left-p insp)
         (format t " :l~15t inspect the left neighbor~%"))
       (when (insp-right-p insp)
        (format t " :r~15t inspect the right neighbor~%"))
       (when (insp-nth-slot insp)
         (format t " number~15t inspect this slot~%"))
       (format t " :e lisp-form~15t eval this form, with these substitutions:
 ~20t (:slot number) is replaced with the appropriate slot value
 ~20t :self is replaced with this object
 :m num lisp~15t Modify this slot
 :q~15t return to the main Read/Eval/Print loop~% ---> ")
       (force-output))
      (:d (describe (insp-self insp)))
      ((:p :a) (print-inspection insp *terminal-io* frontend))
      (:e (handler-case (let ((v (inspect-read-clean-eval insp *terminal-io*)))
                          (format t "~&~S~%" v))
            (error (err) (format t " *** error: ~A" err))))
      (:m (handler-case
              (let ((v (funcall (insp-set-slot insp)
                                (inspect-read-clean-eval insp *terminal-io*)
                                (inspect-read-clean-eval insp *terminal-io*))))
                (format t "~&~S~%" v))
            (error (err) (format t " *** error: ~A" err))))
      (t (cond ((setq insp (get-insp id com))
                (print-inspection insp *terminal-io* frontend)
                (setq id (insp-id insp)))
               (t (format t "command `~s' is not valid here~%" com)
                  (setq insp (get-insp id :s))))))))

;;;
;;; HTTP frontend
;;;

(defmethod print-inspection ((insp inspection) (raw stream)
                             (backend (eql :http)) &key keep-alive)
  (flet ((href (com) (format nil "/~d/~s" (insp-id insp) com)))
    (with-http-output (out raw :keep-alive keep-alive :debug *inspect-debug*
                           :title (insp-title insp) :footer nil)
      (with-tag (:h1) (princ (insp-title insp) out))
      (with-tag (:ul)
        (dolist (item (insp-blurb insp))
          (with-tag (:li) (princ item out))))
      (with-tag (:font :size "+4")
        (with-tag (:pre) (write (insp-self insp) :stream out)))
      (when (insp-nth-slot insp)
        (with-tag (:ol)
          (loop :for ii :from 0 :to (insp-num-slots-print insp)
                :do (multiple-value-bind (val name)
                        (funcall (insp-nth-slot insp) ii)
                      (with-tag (:li)
                        (with-tag (:a :href (href ii))
                          (princ (or name "inspect") out))
                        (with-tag (:pre) (write val :stream out)))))))
      (with-tag (:hr))
      (with-tag (:h2) (princ "describe:" out))
      (with-tag (:pre) (describe (insp-self insp) out))
      (with-tag (:hr))  ; footer
      (with-tag (:table :width "100%")
        (with-tag (:tr)
          (with-tag (:td :align "left")
            (with-tag (:a :href (href :q)) (princ "quit" out)))
          (when (insp-left-p insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :l)) (princ "left" out))))
          (when (insp-right-p insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :r)) (princ "right" out))))
          (when (insp-up insp)
            (with-tag (:td :align "center")
              (with-tag (:a :href (href :u)) (princ "parent" out))))
          (with-tag (:td :align "right")
            (with-tag (:a :href (href :s)) (princ "self" out))))))))

(defun http-command (server &key (debug *inspect-debug*) socket)
  "Accept a connection from the server, return the GET command and the socket."
  (when (> debug 1)
    (format t "~s: server: ~s; socket: ~s~%" 'http-command server socket))
  (let (response id com keep-alive)
    (loop (unless (and socket (open-stream-p socket))
            (setq socket (socket-accept server))
            (when (> debug 1)
              (format t "~s: new socket: ~s~%" 'http-command socket)))
          (setq response (flush-http socket))
          (when response (return))
          ;; connection timed out?
          (close socket))
    (when (> debug 1)
      (dolist (line response)
        (format t "-> ~a~%" line)))
    (dolist (line response)
      (when (string-beg-with "Connection: " line)
        (setq keep-alive (string-equal line "keep-alive" :start1 12))
        (when (> debug 0)
          (format t "~s: connection: ~s (keep-alive: ~s)~%"
                  'http-command (subseq line 12) keep-alive))
        (when keep-alive
          ;; we override `keep-alive' because it makes it impossible to
          ;; switch browsers in the middle of an inspection session
          (setq keep-alive nil)
          (when (> debug 0)
            (format t "~s: overriding keep-alive to NIL~%" 'http-command))))
      (when (string-beg-with "GET /" line)
        (let ((pos (position #\/ line :test #'char= :start 5)))
          (setq id (parse-integer line :start 5 :end pos :junk-allowed t))
          (cond (id
                 (setq com
                       (if (char= #\% (aref line (1+ pos))) ; %xx hex
                           (progn ; "%3a" --> ":"
                             (setf (aref line (+ 3 pos))
                                   (code-char (parse-integer
                                               line :radix 16 :start (+ 2 pos)
                                               :end (+ 4 pos))))
                             (read-from-string line nil nil :start (+ 3 pos)))
                           (read-from-string line nil nil :start (1+ pos))))
                 (when (> debug 0)
                   (format t "~s: command: id=~d com=~s~%"
                           'http-command id com)))
                (t
                 (http-error socket line :debug debug :keep-alive keep-alive)
                 (when (> debug 0)
                   (format t "~s: invalid request: ~s~%"
                           'http-command line)))))))
    (values socket id com keep-alive)))

(defmethod inspect-frontend ((insp inspection) (frontend (eql :http)))
  (let ((server
         (let* ((server (open-socket-server))
                (port (nth-value 1 (socket-server-host/port server))))
           (when (> *inspect-debug* 0)
             (format t "~&~s [~s]: server: ~s~%"
                     'inspect-frontend frontend server))
           (browse-url (format nil "http://~a:~d/0/:s"
                               (if *inspect-browser*
                                 "127.0.0.1" *mail-host-address*)
                               port)
                       :browser *inspect-browser*)
           server))
        sock id com keep-alive)
    (unwind-protect
         (loop (when (eq com :q) (return))
           (setf (values sock id com keep-alive)
                 (http-command server :socket sock))
           (when (> *inspect-debug* 0)
             (format t "~s [~s]: socket: ~s; id: ~s; com: ~s; keep-alive: ~s~%"
                     'inspect-frontend frontend sock id com keep-alive))
           (when id
             (if (eq com :q)
               (with-http-output (out sock :keep-alive keep-alive
                                      :debug *inspect-debug*
                                      :title "inspect" :footer nil)
                 (with-tag (:h1) (princ "thanks for using inspect" out))
                 (with-tag (:p) (princ "you may close this window now" out)))
               (if (setq insp (get-insp id com))
                 (print-inspection insp sock frontend :keep-alive keep-alive)
                 (with-http-output (out sock :keep-alive keep-alive
                                        :debug *inspect-debug*
                                        :title "inspect" :footer nil)
                   (with-tag (:h1)
                     (format out "error: wrong command: ~:d/~s" id com))
                   (with-tag (:p)
                     (princ "either this is an old inspect session, or a " out)
                     (with-tag (:a :href "https://sourceforge.net/bugs/?func=addbug&group_id=1802") (princ "bug" out))))))
             (when (> *inspect-debug* 0)
               (format t "~s [~s]: cmd:~d/~s id:~d~%" 'inspect-frontend
                       frontend id com (insp-id insp)))))
      (socket-server-close server)
      (when (open-stream-p sock)
        (do () ((null (read-char-no-hang sock))))
        (close sock)))))

;;;
;;; the juice
;;;

;;;###autoload
(defun inspect-cllib (object &key
                      ((:frontend *inspect-frontend*) *inspect-frontend*)
                      ((:browser *inspect-browser*) *inspect-browser*))
  "This function implements the ANSI Common Lisp INSPECT function."
  (let* ((*print-array* nil) (*print-pretty* t)
         (*print-circle* t) (*print-escape* t)
         (*print-lines* *inspect-print-lines*)
         (*print-level* *inspect-print-level*)
         (*print-length* *inspect-print-length*)
         (*inspect-all* (make-array 10 :fill-pointer 0 :adjustable t))
         (tmp-pack (make-package (gensym "INSPECT-TMP-PACKAGE-")))
         (*package* tmp-pack)
         (*inspect-unbound-value* (intern "#<unbound>" tmp-pack)))
    (unwind-protect
         (inspect-frontend (inspect-backend object) *inspect-frontend*)
      (inspect-finalize *inspect-frontend*)
      (delete-package tmp-pack))
    (values)))

(provide :cllib-inspect)
;;; inspect.lisp ends here
