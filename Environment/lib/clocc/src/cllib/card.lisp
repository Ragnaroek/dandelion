;;; Personal Database - Rolodex (BBDB, VCARD)
;;; Relevant URLs:
;;;  http://bbdb.sourceforge.net/
;;;  http://www.jwz.org/bbdb/
;;;  http://www.cis.ohio-state.edu/htbin/rfc/rfc2426.html
;;;  http://www.imc.org/pdi
;;;
;;; Copyright (C) 1999-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: card.lisp,v 2.15 2005/01/27 23:02:50 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/card.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `class-slot-list'
  (require :cllib-closio (translate-logical-pathname "cllib:closio"))
  ;; `pr-secs'
  (require :cllib-tilsla (translate-logical-pathname "cllib:tilsla"))
  ;; `substitute-subseq', `string-beg-with', `string-end-with', `split-string'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; `date2time', `string->dttm', `dttm->string'
  (require :cllib-date (translate-logical-pathname "cllib:date"))
  ;; `url'
  (require :cllib-url (translate-logical-pathname "cllib:url")))

(in-package :cllib)

(export '(*user-bbdb-file* *user-vcard-file* *user-native-file*))

;;;
;;; {{{ definitions
;;;

(eval-when (compile load eval)  ; CMUCL
(defclass name ()
  ((first :type simple-string :initarg first :accessor name-first
          :documentation "the first name")
   (ini :type simple-string :initarg ini :accessor name-ini
        :documentation "the middle initial name")
   (last :type simple-string :initarg last :accessor name-last
         :documentation "the last name")
   (prefix :type simple-string :initarg prefix :accessor name-prefix
           :documentation "the prefix (like `PhD')")
   (suffix :type simple-string :initarg suffix :accessor name-suffix
            :documentation "the suffix (like `VII')")
   (aka :type cons :initarg aka :accessor name-aka
        :documentation "the list of aliases"))
  (:documentation "The name - with bells and whistles."))

(defclass phone ()
  ((loc :type simple-string :initarg loc :accessor phone-loc
        :documentation "location")
   (nmb :type simple-string :initarg nmb :accessor phone-nmb
        :documentation "the phone number"))
  (:documentation "The phone - location and number."))

(defclass address ()
  ((loc :type simple-string :initarg loc :accessor address-loc
        :documentation "location")
   (street1 :type simple-string :initarg street1 :accessor address-street1
            :documentation "line one of the street address")
   (street2 :type simple-string :initarg street2 :accessor address-street2
            :documentation "line two of the street address")
   (street3 :type simple-string :initarg street3 :accessor address-street3
            :documentation "line three of the street address")
   (city :type simple-string :initarg city :accessor address-city
         :documentation "the city")
   (state :type simple-string :initarg state :accessor address-state
          :documentation "the state")
   (zip :type simple-string :initarg zip :accessor address-zip
        :documentation "the zip code")
   (country :initform "USA" :type simple-string :initarg country
            :accessor address-country :documentation "the country"))
  (:documentation "The full mailing address."))

(defclass card ()
  ((label :type simple-string :initarg label :accessor card-label
          :documentation "The unique record ID.")
   (name :type name :initarg name :accessor card-name
         :documentation "The name of the person.")
   (addrl :type cons :initarg addrl :accessor card-addrl
          :documentation "The list of addresses.")
   (phonel :type cons :initarg phonel :accessor card-phonel
           :documentation "The list of phones.")
   (emaill :type cons :initarg emaill :accessor card-emaill
           :documentation "The list of e-mail addresses.")
   (urll :type list :initarg urll :accessor card-urll
           :documentation "The list of URLs.")
   (security :type simple-string :initarg security :accessor card-security
             :documentation "PGP or X509 key.")
   (note :type simple-string :initarg note :accessor card-note
         :documentation "Random note not fitting anything else.")
   (org :type simple-string :initarg org :accessor card-org
        :documentation "The organization or company.")
   (title :type simple-string :initarg title :accessor card-title
          :documentation "The title of the person.")
   (tz :type (rational -24 24) :initarg tz :accessor card-tz
       :documentation "The time zone.")
   (geo :type complex :initarg geo :accessor card-geo
        :documentation "The geografic coordinates, in degrees.")
   (class :type symbol :initarg class :accessor card-class
          :documentation "Access classification: PRIVATE, PUBLIC &c")
   ;; (bday
   (created :type (integer 0) :initarg created :accessor card-created
            :documentation "The creation time, in seconds since the epoch.")
   (timestamp :type (integer 0) :initarg timestamp :accessor card-timestamp
              :documentation "The last modification time."))
  (:documentation "The personal information record."))
)

(defun slot-val (obj slot &optional default)
  (or (when (slot-boundp obj slot) (slot-value obj slot)) default))

;;;
;;; }}}{{{ search
;;;

(defcustom *user-cards* list nil "*The user database of records.")

(defun object-match-p (cc nm)
  "Check whether the object matches the string."
  (declare (type card cc) (simple-string nm))
  (dolist (slot (class-slot-list cc))
    (when (slot-boundp cc slot)
      (let ((val (slot-value cc slot)))
        (typecase val
          (standard-object (when (object-match-p val nm) (return t)))
          (string (when (search nm val) (return t))))))))

(defun find-card (nm &optional (cards *user-cards*))
  "Return the list of cards matching NM."
  (declare (simple-string nm))
  (loop :for cc :of-type card :in cards
        :when (object-match-p cc nm) :collect cc))

;;;
;;; }}}{{{ output
;;;

(defstruct card-output
  (card nil :type symbol)
  (name nil :type symbol)
  (phone nil :type symbol)
  (address nil :type symbol))

(defcustom *card-output-type* (or null card-output) nil
  "The type of output for CARD.
Valid value are instances of `card-output' or NIL (for #[card ...]).
See constants `+card-output-bbdb+', `+card-output-vcard+',
`+card-output-pretty+'.")

(defcustom *user-bbdb-file* pathname
  (mk-path (user-homedir-pathname) :name ".bbdb")
  "*The path to the user's BBDB file.")

(defcustom *user-vcard-file* pathname
  (mk-path (user-homedir-pathname) :directory '(:relative ".gnome")
           :name "GnomeCard.gcrd")
  "*The path to the user's VCARD file.")

(defcustom *user-native-file* pathname
  (mk-path (user-homedir-pathname) :name ".rolodex")
  "*The path to the user's native data file.")

(defconst +card-output-bbdb+ card-output
  (make-card-output :card 'card-print-as-bbdb
                    :name 'name-print-as-bbdb
                    :phone 'phone-print-as-bbdb
                    :address 'address-print-as-bbdb)
  "The constant, a valid value for `*card-output-type*'.")

(defconst +card-output-vcard+ card-output
  (make-card-output :card 'card-print-as-vcard
                    :name 'name-print-as-vcard
                    :phone 'phone-print-as-vcard
                    :address 'address-print-as-vcard)
  "The constant, a valid value for `*card-output-type*'.")

(defconst +card-output-pretty+ card-output
  (make-card-output :card 'card-print-as-pretty
                    :name 'name-print-as-pretty
                    :phone 'phone-print-as-pretty
                    :address 'address-print-as-pretty)
  "The constant, a valid value for `*card-output-type*'.")

(defun card-org/title (cc)
  "Merge the TITLE with ORG."
  (when (or (slot-boundp cc 'org) (slot-boundp cc 'title))
    (format nil "~@[~a~]~:[~;; ~]~@[~a~]" (slot-val cc 'org)
            (and (slot-boundp cc 'org) (slot-boundp cc 'title))
            (slot-val cc 'title))))

(defun card-print-as-bbdb (cc out)
  (declare (type card cc) (stream out))
  (let ((*print-right-margin* nil) (*print-pretty* nil))
    (if (slot-boundp cc 'name) (format out "[~a " (card-name cc))
        (write-string "[nil nil nil " out))
    (if (or (slot-boundp cc 'org) (slot-boundp cc 'title))
        (format out "\"~a~:[~;; ~a~]\" " (slot-val cc 'org)
                (and (slot-boundp cc 'org) (slot-boundp cc 'title))
                (slot-val cc 'title))
        (write-string "nil " out))
    (format out "~s ~s ~s ~s (" (card-org/title cc) (slot-val cc 'phonel)
            (slot-val cc 'addrl) (slot-val cc 'emaill))
    (when (slot-boundp cc 'note) (format out "(notes . ~s) " (card-note cc)))
    (format out "~@[(url . \"~{~a~^, ~}\") ~]~
\(creation-date . \"~a\") (timestamp . \"~a\")) nil]"
            (slot-val cc 'urll) (dttm->string (card-created cc) :format :date)
            (dttm->string (card-timestamp cc) :format :date))))

(defconst +card-vcard-begin+ string "BEGIN:VCARD" "VCARD start marker")
(defconst +card-vcard-end+ string "END:VCARD" "VCARD end marker")

(defun card-print-as-vcard (cc out)
  (declare (type card cc) (stream out))
  (format out "~&~a~%FN:~a~%~@[N:~a~%~]~{ADR:~a~%~}~{TEL:~a~%~}~
~{EMAIL:~a~%~}~{URL:~a~%~}~@[TITLE:~a~%~]~@[ORG:~a~%~]~@[NOTE:~a~%~]"
          +card-vcard-begin+
          (card-label cc) (slot-val cc 'name) (slot-val cc 'addrl)
          (slot-val cc 'phonel) (slot-val cc 'emaill) (slot-val cc 'urll)
          (slot-val cc 'title) (slot-val cc 'org) (slot-val cc 'note))
  (when (slot-boundp cc 'tz)
    (format out "TZ:~@/pr-secs/~%" (* 3600 (card-tz cc))))
  (when (slot-boundp cc 'geo)
    (format out "GEO:~f:~f~%"
            (realpart (card-geo cc)) (imagpart (card-geo cc))))
  (format out "REV:~a~%CRE:~a~%~@[CLASS:~a~%~]~a~%"
          (dttm->string (card-timestamp cc) :format :datetime)
          (dttm->string (card-created cc) :format :datetime)
          (slot-val cc 'security) +card-vcard-end+))

(defun card-print-as-pretty (cc out)
  (declare (type card cc) (stream out))
  (format out "~@[~a~%~]~@[[~a]~%~]" (slot-val cc 'name) (card-org/title cc))
  (when (slot-boundp cc 'emaill)
    (format out "[email:~10t~{~<~%~9t ~1,74:; <~a>~>~^,~}.]~%"
            (card-emaill cc)))
  (when (slot-boundp cc 'urll)
    (format out "[URL:~10t~{~<~%~9t ~1,74:; <~a>~>~^,~}.]~%" (card-urll cc)))
  (format out "~{~a~%~}~{~a~%~}~@[[~a]~%~]{~a  ~a}~2%"
          (slot-val cc 'phonel) (slot-val cc 'addrl) (slot-val cc 'note)
          (dttm->string (card-created cc) :format :datetime)
          (dttm->string (card-timestamp cc) :format :datetime)))

(defun name-print-as-bbdb (nm out)
  (declare (type name nm) (stream out))
  (format out "~:[~*~;~:*\"~a~@[ ~a~]\"~] ~s ~s" (slot-val nm 'first)
          (slot-val nm 'ini) (slot-val nm 'last) (slot-val nm 'aka)))

(defun name-print-as-vcard (nm out)
  (declare (type name nm) (stream out))
  (format out "~@[~a~];~@[~a~];~@[~a~];~@[~a~];~@[~a~]"
          (slot-val nm 'last) (slot-val nm 'first) (slot-val nm 'ini)
          (slot-val nm 'prefix) (slot-val nm 'suffix)))

(defun name-print-as-pretty (nm out)
  (declare (type name nm) (stream out))
  (format out "~@[~a ~]~@[~a ~]~@[~a ~]~@[~a~]~@[, ~a~]"
          (slot-val nm 'prefix) (slot-val nm 'first) (slot-val nm 'ini)
          (slot-val nm 'last) (slot-val nm 'suffix)))

(defun phone-print-as-bbdb (ph out)
  (declare (type phone ph) (stream out))
  (format out "[~s ~s]" (phone-loc ph) (phone-nmb ph)))

(defun phone-print-as-vcard (ph out)
  (declare (type phone ph) (stream out))
  (format out "~a:~a" (phone-loc ph) (phone-nmb ph)))

(defun phone-print-as-pretty (ph out)
  (declare (type phone ph) (stream out))
  (format out " ~20@a:  ~a" (phone-loc ph) (phone-nmb ph)))

(defun address-print-as-bbdb (adrs out)
  (declare (type address adrs) (stream out))
  (format out "[~s ~s ~s ~s ~s ~s ~s ~s]"
          (slot-val adrs 'loc "") (slot-val adrs 'street1 "")
          (slot-val adrs 'street2 "") (slot-val adrs 'street3 "")
          (slot-val adrs 'city "") (slot-val adrs 'state "")
          (slot-val adrs 'zip "") (slot-val adrs 'country "")))

(defun address-print-as-vcard (adrs out)
  (declare (type address adrs) (stream out))
  (format out "~a:~a;~a;~a;~a;~a;~a;~a"
          (slot-val adrs 'loc "") (slot-val adrs 'street1 "")
          (slot-val adrs 'street2 "") (slot-val adrs 'street3 "")
          (slot-val adrs 'city "") (slot-val adrs 'state "")
          (slot-val adrs 'zip "") (slot-val adrs 'country "")))

(defun address-print-as-pretty (adrs out)
  (declare (type address adrs) (stream out))
  (format
   out "~a:~%~@[ ~a~%~]~@[ ~a~%~]~@[ ~a~%~]~@[ ~a ~]~@[~a ~]~@[~a ~]~@[~a~]"
   (slot-val adrs 'loc) (slot-val adrs 'street1)
   (slot-val adrs 'street2) (slot-val adrs 'street3)
   (slot-val adrs 'city) (slot-val adrs 'state)
   (slot-val adrs 'zip) (slot-val adrs 'country)))

(defmacro define-print-method (cs)
  `(defmethod print-object ((cc ,cs) (out stream))
    (cond ((or *print-readably* (null *card-output-type*)) (call-next-method))
          ((card-output-p *card-output-type*)
           (funcall (,(symbol-concat 'card-output- cs) *card-output-type*)
                    cc out))
          (t (error 'code :proc 'print-object :args (list *card-output-type*)
                    :mesg "Illegal value of `*card-output-type*': `~s'")))))

(define-print-method card)
(define-print-method name)
(define-print-method phone)
(define-print-method address)

(defun init-sans-null-args (obj list)
  "(a 10 b nil c 1 d nil) --> (a 10 c 1)"
  (loop :for arg :in list :by #'cddr :and val :in (cdr list) :by #'cddr
        :when val :do (setf (slot-value obj arg) val)))

;; has to be here since it uses `+card-output-pretty+'
(defmethod initialize-instance ((cc card) &rest args)
  (init-sans-null-args cc args)
  ;; init `label' from `name' and `emaill'
  (when (and (not (slot-boundp cc 'label))
             (or (slot-boundp cc 'name) (slot-boundp cc 'emaill)))
    (let ((*card-output-type* +card-output-pretty+))
      (setf (card-label cc)
            (format nil "~@[~a ~]~@[<~a>~]" (slot-val cc 'name)
                    (car (slot-val cc 'emaill))))))
  ;; init `timestamp' and `created' from `get-universal-time'
  (unless (and (slot-boundp cc 'timestamp) (slot-boundp cc 'created))
    (let ((tm (get-universal-time)))
      (unless (slot-boundp cc 'timestamp) (setf (card-timestamp cc) tm))
      (unless (slot-boundp cc 'created) (setf (card-created cc) tm))))
  ;; init `tz' from `geo'
  (when (and (slot-boundp cc 'geo) (not (slot-boundp cc 'tz)))
    (setf (card-tz cc) (round (realpart (card-geo cc)) 15)))
  ;; replace #\Newline with \n in multiline strings.
  (when (slot-boundp cc 'note)
    (setf (card-note cc)
          (substitute-subseq (card-note cc) (string #\Newline) "\\n")))
  (when (slot-boundp cc 'security)
    (setf (card-note cc)
          (substitute-subseq (card-security cc) (string #\Newline) "\\n")))
  cc)

(defcustom *card-apellations* list
  '("Mr." "Mrs." "Ms." "Miss" "Dr" "Dr." "Prof.")
  "*The list of recognizable apellations.")

(defcustom *card-suffixes* list
  '("PhD" "Ph.D." "PhD." "MD" "CPA" "Jr" "3rd")
  "*The list of recognizable apellations.")

(defmethod initialize-instance ((nn name) &rest args)
  (init-sans-null-args nn args)
  (when (slot-boundp nn 'first)
    (let* ((fi (name-first nn)) (len (length fi)))
      (declare (simple-string fi))
      ;; init `prefix' from `first'
      (unless (slot-boundp nn 'prefix)
        (dolist (st *card-apellations*)
          (declare (simple-string st))
          (when (string-beg-with st fi len)
            (setf (name-prefix nn) st
                  fi (string-left-trim +whitespace+ (subseq fi (length st)))
                  (name-first nn) fi)
            (return))))
      ;; init `ini' from `first'
      (unless (slot-boundp nn 'ini)
        (let ((pos (position #\Space fi :from-end t)))
          (when pos
            (setf (name-ini nn) (subseq fi (1+ pos))
                  (name-first nn) (subseq fi 0 pos)))))))
  ;; init `suffix' from `last'
  (when (and (slot-boundp nn 'last) (not (slot-boundp nn 'suffix)))
    (let* ((la (name-last nn)) (len (length la)))
      (declare (simple-string la))
      (dolist (st *card-suffixes*)
        (declare (simple-string st))
        (when (string-end-with st la len)
          (setf (name-suffix nn) st
                (name-last nn)
                (string-right-trim
                 +whitespace+ (subseq la 0 (- len (length st))))
                len (length (name-last nn)))
          ;; remove the taling comma
          (when (char= #\, (schar (name-last nn) (1- len)))
            (setf (name-last nn) (subseq (name-last nn) 0 (1- len))))
          (return)))))
  nn)

;;;
;;; }}}{{{ input
;;;

(declaim (ftype (function (simple-vector) (values phone)) vector-to-phone))
(defun vector-to-phone (vec)
  "Convert the vector (from BBDB) to a PHONE instance."
  (declare (simple-vector vec))
  (make-instance 'phone 'loc (aref vec 0) 'nmb
                 (typecase (aref vec 1)
                   (string (aref vec 1))
                   (number (format nil "(~3,'0d) ~3,'0d-~4,'0d~:[ x~d~;~]"
                                   (aref vec 1) (aref vec 2) (aref vec 3)
                                   (zerop (aref vec 4)) (aref vec 4))))))

(declaim (ftype (function (simple-vector) (values address)) vector-to-address))
(defun vector-to-address (vec)
  "Convert the vector (from BBDB) to an ADDRESS  instance."
  (declare (simple-vector vec))
  (let ((ad (make-instance 'address 'loc (aref vec 0))))
    (macrolet ((putslot (nn slot)
                 `(unless (zerop (length (the simple-string (aref vec ,nn))))
                   (setf (slot-value ad ',slot) (aref vec ,nn)))))
      (putslot 1 street1)
      (putslot 2 street2)
      (putslot 3 street3)
      (putslot 4 city)
      (putslot 5 state))
    (setf (address-zip ad)
          (let ((zip (aref vec 6)))
            (typecase zip
              (number (format nil "~5,'0d" zip))
              (string zip)
              (list (format nil "~5,'0d-~4,'0d" (first zip) (second zip))))))
    ad))

(declaim (ftype (function (simple-vector) (values card)) vector-to-card))
(defun vector-to-card (vec)
  "Convert the vector (as in BBDB) to a CARD."
  (declare (simple-vector vec))
  (let ((nts (aref vec 7)) tmp)
    (make-instance
     'card
     'name (make-instance 'name 'first (aref vec 0) 'last (aref vec 1)
                          'aka (aref vec 2))
     'org (aref vec 3)
     'phonel (mapcar #'vector-to-phone (aref vec 4))
     'addrl (mapcar #'vector-to-address (aref vec 5))
     'emaill (aref vec 6)
     'note (cdr (assoc 'notes nts :test #'eq))
     'created (when (setq tmp (assoc 'creation-date nts :test #'eq))
                (date2time (date (cdr tmp))))
     'timestamp (when (setq tmp (assoc 'timestamp nts :test #'eq))
                  (date2time (date (cdr tmp))))
     'urll (mapcan
            (lambda (ff)
              (mapcar #'url (split-string (cdr (assoc ff nts :test #'eq))
                                          #(#\Space #\Newline #\,))))
            '(www ftp gopher telnet)))))

(defun card-read-bbdb (in ra)
  "Read CARD from a BBDB stream.  Suitable for `read-list-from-stream'."
  (declare (stream in) (simple-vector ra))
  (handler-case (values (vector-to-card ra) (read in nil +eof+))
    (error (co)
      (format t "~& *** [vector-to-card] Broken on~%~s~%" ra)
      (error co))))

(defun card-read-vcard (in ra)
  "Read CARD from a vCard stream.  Suitable for `read-list-from-stream'."
  (declare (stream in))
  (unless (string= ra +card-vcard-begin+)
    (warn "~s: a VCARD should start with ~s, not with ~s"
          'card-read-vcard +card-vcard-begin+ ra))
  (macrolet ((pushslot (val obj slot)
               `(if (slot-boundp ,obj ',slot)
                 (push ,val (slot-value ,obj ',slot))
                 (setf (slot-value ,obj ',slot) (list ,val)))))
    (loop :with cc :of-type card = (make-instance 'card)
          :for str :of-type (or null simple-string) = (read-line in nil nil)
          :for len :of-type index-t = (length str)
          :while (and str (not (string= str +card-vcard-end+)))
          :do
          (cond ((string-beg-with "FN:" str len)
                 (setf (card-label cc) (subseq str 3)))
                ((string-beg-with "N:" str len)
                 (let ((ll (split-string (subseq str 2) ";" :strict t)))
                   (setf (card-name cc)
                         (make-instance
                          'name 'last (first ll) 'first (second ll)
                          'ini (third ll) 'prefix (fourth ll)
                          'suffix (fifth ll)))))
                ((string-beg-with "ADR;" str len)
                 (let ((ll (split-string (subseq str 4) ":;" :strict t)))
                   (pushslot
                    (make-instance 'address 'loc (first ll)
                                   'street1 (second ll)
                                   'street2 (third ll) 'street3 (fourth ll)
                                   'city (fifth ll)
                                   'state (sixth ll) 'zip (seventh ll)
                                   'country (eighth ll))
                    cc addrl)))
                ((string-beg-with "TEL;" str len)
                 (let ((pos (position #\: str :start 4)))
                   (declare (type index-t pos))
                   (pushslot (make-instance 'phone 'loc (subseq str 4 pos)
                                            'nmb (subseq str (1+ pos)))
                             cc phonel)))
                ((string-beg-with "EMAIL;" str len)
                 (pushslot (subseq str (1+ (position #\: str :start 6)))
                           cc emaill))
                ((string-beg-with "TZ:" str len)
                 (let ((ll (split-string (subseq str 3) ":")))
                   (setf (card-tz cc)
                         (+ (parse-integer (first ll))
                            (/ (parse-integer (second ll)) 60)
                            (if (cddr ll)
                                (/ (parse-integer (third ll)) 3600) 0)))))

                ((string-beg-with "URL:" str len)
                 (pushslot (url (subseq str 4)) cc urll))
                ((string-beg-with "ORG:" str len)
                 (setf (card-org cc) (subseq str 4)))
                ((string-beg-with "TITLE:" str len)
                 (setf (card-title cc) (subseq str 6)))
                ((string-beg-with "NOTE:" str len)
                 (setf (card-note cc) (subseq str 5)))
                ((string-beg-with "REV:" str len)
                 (setf (card-timestamp cc) (string->dttm (subseq str 4))))
                ((string-beg-with "CRE:" str len)
                 (setf (card-created cc) (string->dttm (subseq str 4))))
                (t (warn "~s: unknown field: ~s~%" 'card-read-vcard str)))
          :finally (dolist (slot '(addrl urll emaill phonel))
                     (when (slot-boundp cc slot)
                       (setf (slot-value cc slot)
                             (nreverse (slot-value cc slot)))))
          :finally (loop :for str = (read-line in nil nil)
                         :until (or (null str)
                                    (string= str +card-vcard-begin+))
                         :finally (return-from card-read-vcard
                                    (values (initialize-instance cc)
                                            (or str +eof+)))))))

#+nil (progn

;;; BBDB i/o
(setq *user-cards* (read-list-from-file *user-bbdb-file* #'card-read-bbdb
                                        :readtable +elisp-readtable+))
(let ((*card-output-type* +card-output-bbdb+))
  (write-list-to-file *user-cards* *user-bbdb-file*))

;;; vCard i/o
(let ((*card-output-type* +card-output-vcard+))
  (print *user-cards*)
  (write-list-to-file *user-cards* *user-vcard-file*))
(setq *user-cards* (read-list-from-file *user-vcard-file* #'card-read-vcard
                                        :read-ahead-function #'read-line))
(setq *user-cards* (read-list-from-file
                    (mk-path (user-homedir-pathname) :name ".vcard")
                    #'card-read-vcard :read-ahead-function #'read-line))

;;; native i/o
(let ((*card-output-type* nil))
  (write-to-file *user-cards* *user-native-file*))
(setq *user-cards* (read-from-file *user-native-file*
                                   :readtable +clos-readtable+))

;;; Pretty printing
(let ((*card-output-type* +card-output-pretty+))
  (write-list-to-stream *user-cards* *standard-output*))

;; testing
(let ((*card-output-type* +card-output-vcard+))
  (print (car *user-cards*)))

)

;;; }}}
(provide :cllib-card)
;;; file card.lisp ends here
