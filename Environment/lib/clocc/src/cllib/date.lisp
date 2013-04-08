;;; Date-related structures
;;;
;;; Copyright (C) 1997-2006 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: date.lisp,v 2.35 2006/03/06 17:46:35 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/date.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `skip-to-new'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `purge-string'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; `binary-member'
  (require :cllib-sorted (translate-logical-pathname "cllib:sorted"))
  ;; `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `dfloat'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype")))

(in-package :cllib)

(export '(string->dttm dttm->string date-formatter +day-sec+ print-date-month
          date date2time date2num date2days time2date days2date mk-date
          date-da date-mo date-ye unix-date infer-timezone infer-month
          days-week-day date-week-day black-days working-day-p
          days-to-next-working-day next-working-day previous-working-day
          days-since days-since-f *y2k-cut* make-date-readtable
          date= date/= date< date> date<= date<=3 date<3 date>= date>=3 date>3
          date=*1 date=* date-max date-min date-earliest date-latest
          next-month-p date-month= next-quarter-p date-quarter days-between
          today tomorrow yesterday date-next-year date-next-all
          date-in-list sync-dates sync-dates-ui))

(defconst +day-sec+ fixnum (* 24 60 60) "The number of seconds per day.")

;;;
;;; Date
;;;

(deftype days-t () '(signed-byte 20))

;; Of course, a class would be more appropriate here, especially since
;; this would allow us to use an :after `initialize-instance' method to
;; properly init DD.  Unfortunately, this would mean that we will have
;; to use our own i/o procedures.  Both are in-place already (see
;; `read-object' and (print-object standard-object) in closio.lisp), but
;; they should be awfully slow (each printing of an object requires a
;; call to `class-slot-list').  Another option is to have a
;; class-allocated slot `printable-slots', thus avoiding calling
;; `class-slot-list' for each output.  Still this would be much slower
;; than the current implementation, especially in CLISP, which doesn't
;; have native compilation and thus executes user-supplied functions
;; like `read-object' and `print-object' much slower than the system
;; functions (like `structure-object' i/o).
;; E.g.:
#|
 (defclass date ()
  ((ye :type days-t :initform 0 :initarg year :initarg ye
       :accessor date-ye :documentation "year")
   (mo :type (integer 1 12) :initform 1 :initarg month :initarg mo
       :accessor date-mo :documentation "month")
   (da :type (integer 1 31) :initform 1 :initarg day :initarg da
       :accessor date-da :documentation "day")
   (dd :type days-t :initarg dd :accessor date-dd
       :documentation "the number of days since the epoch - 1900-1-1")
   (nn :type index-t :initform 0 :allocation :class
       :documentation "The number of instances already allocated.")
   (printable-slots :type list :initform '(ye mo da dd) :allocation :class
                    :documentation "The list of slots for readable printing."))
  (:documentation "The DATE class."))

 (defmethod initialize-instance :after ((dt date-c) &rest junk)
  (declare (ignore junk))
  (incf (slot-value dt 'nn))
  (unless (slot-boundp dt 'dd)
    (let ((tm (encode-universal-time
               0 0 0 (date-da dt) (date-mo dt) (date-ye dt) 0)))
      (declare (integer tm))
      (setf (dadd dt) (floor tm +day-sec+)
            (daye dt) (nth-value 5 (decode-universal-time tm 0))))))

 (defun make-date (&key (ye 1) (mo 1) (da 1) (dd nil dd-p))
  (if dd-p (make-instance 'date 'ye ye 'mo mo 'da da 'dd dd)
      (make-instance 'date 'ye ye 'mo mo 'da da)))
|#

(defstruct (date)
  "The date structure -- year, month, and day."
  (ye 1 :type days-t)
  (mo 1 :type (integer 1 12))
  (da 1 :type (integer 1 31))
  (dd nil :type (or null days-t))) ; days since the epoch (1900-1-1 == 0)
(deftype date-f-t () '(function (t) date))

(defmethod print-object ((dt date) (out stream))
  (cond (*print-readably* (call-next-method))
        (*print-escape*
         (write-string "#D" out)
         (prin1 (list (date-ye dt) (date-mo dt) (date-da dt)) out))
        (t (format out "~4,'0d-~2,'0d-~2,'0d"
                   (date-ye dt) (date-mo dt) (date-da dt)))))

(defun make-date-readtable (&optional (rt (copy-readtable)))
  "Return a readtable that will read dates as #d"
  (set-dispatch-macro-character
   #\# #\D
   (lambda (stream char-d infix-arg)
     (declare (ignore char-d infix-arg))
     (destructuring-bind (ye mo da) (read stream)
       (fix-date (make-date :ye ye :mo mo :da da))))
   rt)
  rt)

(defconst +bad-date+ date (make-date) "*The convenient constant for init.")

(declaim (ftype (function (date) (values simple-string)) date-mon-name))
(defun date-mon-name (dt)
  "Return the name of the month."
  (declare (type date dt))
  (aref +month-names+ (1- (date-mo dt))))

(declaim (ftype (function (date) (values (unsigned-byte 10))) date-mon-offset))
(defun date-mon-offset (dt)
  "Return the number of characters printed for the previous months."
  (declare (type date dt))
  (let ((pos (1- (date-mo dt))))
    (reduce #'+ +month-names+ :key #'length :end pos :initial-value pos)))

(defun print-date-month (dt &optional (str t))
  "Print the date to the STREAM, month and year only."
  (declare (type date dt))
  (format str "~a ~d" (date-mon-name dt) (date-ye dt)))

;;; converters

(declaim (ftype (function (date) (values integer)) date2num date2time))
(defsubst date2num (dt)
  "Convert the date to the numerical format YYYYMMDD."
  (declare (type date dt))
  (+ (* 10000 (date-ye dt)) (* 100 (date-mo dt)) (date-da dt)))

(defsubst date2time (dt)
  "Call `encode-universal-time' on the date.
Returns the number of seconds since the epoch (1900-01-01)."
  (declare (type date dt))
  (encode-universal-time 0 0 0 (date-da dt) (date-mo dt) (date-ye dt) 0))

(declaim (ftype (function (real) (values date)) time2date))
(defun time2date (num)
  "Convert the universal time (GMT) to date."
  (declare (real num))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time num 0)
    (declare (ignore se mi ho))
    (make-date :ye ye :mo mo :da da :dd (floor num +day-sec+))))

(declaim (ftype (function (date) (values date)) fix-date))
(defun fix-date (dt)
  "Make sure the date is correct."
  (declare (type date dt))
  (let ((tm (date2time dt)))
    (declare (integer tm))
    (setf (date-dd dt) (floor tm +day-sec+)
          (date-ye dt) (nth-value 5 (decode-universal-time tm 0))))
  dt)

(defmacro mk-date (&rest args)
  "Make date, fixing DD and then YE."
  `(fix-date (make-date ,@args)))

(declaim (ftype (function (date) (values days-t)) date2days))
(defsubst date2days (dt)
  "Convert the date to the number of days since the epoch (1900-01-01)."
  (declare (type date dt))
  (or (date-dd dt)
      (progn (format t "~& *** Fixed date ~a~%" dt)
             (date-dd (fix-date dt)))))

(declaim (ftype (function (days-t) (values date)) days2date))
(defsubst days2date (days)
  "Convert the number of days since the epoch (1900-01-01) to the date."
  (declare (type days-t days))
  (time2date (* days +day-sec+)))

(declaim (ftype (function (integer) (values date)) num2date))
(defun num2date (num)
  "Get the date from the number."
  (declare (integer num))
  (mk-date :ye (floor num 10000) :mo (mod (floor num 100) 100)
           :da (mod num 100)))

(defun to-string (obj)
  "Unintern if symbol and return string."
  (declare (type (or symbol string) obj))
  (etypecase obj
    (string obj)
    (symbol (unintern obj) (symbol-name obj))))

(defun infer-timezone (obj &optional minutes)
  "Guess the timezone."
  (typecase obj
    (symbol (unintern obj) (infer-timezone (symbol-name obj)))
    (string (or (car (string->tz obj)) 0))
    (number
     (multiple-value-bind (ho mi)
         (cond ((numberp minutes) (values obj minutes))
               ((< -24 obj 24) (values (- obj) 0))
               (t (floor obj 100)))
       ;; CL uses positive offsets to the West of Greenwich,
       ;; while the rest of the world counts positive to the East.
       (- (+ ho (/ mi 60)))))
    (t (error 'case-error :proc 'infer-timezone :args
              (list 'obj obj 'symbol 'string 'number)))))

(defcustom +unix-epoch+ integer (encode-universal-time 0 0 0 1 1 1970 0)
  "The start of the UNIX epoch - 1970-01-01.")

(declaim (ftype (function ((unsigned-byte 32)) (values date)) unix-date))
(defun unix-date (nn)
  "Convert UNIX time_t number to date."
  (declare (type (unsigned-byte 32) nn))
  (time2date (+ nn +unix-epoch+)))

(defun string-w3-datetime (str &key (start 0) end)
  "Check whether the string is in the W3 datatime format.
Returns the decoded date or NIL.
See <http://www.w3.org/TR/NOTE-datetime>."
  (declare (type string str))
  (unless end (setq end (length str)))
  (let ((len (- end start)))
    (macrolet ((num (s e)
                 `(parse-integer str :start (+ ,s start) :end (+ ,e start)))
               (ch= (c p) `(char= ,c (aref str (+ ,p start)))))
      (when (and (>= len 10) (ch= #\- 4) (ch= #\- 7))
        (if (= len 10) ; just the date
            (values 0 0 0 (num 8 10) (num 5 7) (num 0 4))
            (when (and (>= len 16) (ch= #\T 10) (ch= #\: 13))
              (let ((z (position #\Z str :start (+ 14 start) :end end)))
                (if z
                    (values
                     (if (>= (+ start 17) end) 0 ; sec
                         (ignore-errors
                           (round (read-from-string
                                   str nil 0 :start (+ start 17) :end z))))
                     (num 14 16) (num 11 13) ; mi ho
                     (num 8 10) (num 5 7) (num 0 4) ; da mo ye
                     (ignore-errors
                       (parse-integer str :start (1+ z) :end end))) ; tz
                    (values
                     (if (>= (+ start 17) end) 0 ; sec
                         (ignore-errors
                           (round (read-from-string
                                   str nil 0 :start (+ start 17) :end end))))
                     (num 14 16) (num 11 13) ; mi ho da mo ye
                     (num 8 10) (num 5 7) (num 0 4))))))))))

(defun string-w3-dttm (str &key (start 0) end)
  (multiple-value-bind (se mi ho da mo ye tz)
      (string-w3-datetime str :start start :end end)
    (and se (encode-universal-time se mi ho da mo ye
                                   (infer-timezone (or tz 0))))))

(defun dttm->string (dttm &key (format :long) (tz 0) (dst nil dst-p))
  "Print the date/time as returned by `encode-universal-time'.
DTTM is the universal time (GMT).
FORMAT is passed to DATE-FORMATTER.
TZ is the time zone in which the time is printed, NIL means local.
DST is Daylight Saving Time indicator."
  (declare (type (integer 0) dttm))
  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
      (decode-universal-time dttm tz)
    (date-formatter format se mi ho da mo ye dd (if dst-p dst dst1) tz1)))

(defgeneric date-formatter (format se mi ho da mo ye dd dst tz)
  (:documentation "Format the decoded time using the given format spec.
The supported specs are:
  function -- run it
  string   -- assume it's a format string for the other arguments
  :date    -- \"2001-04-22\", see <http://www.w3.org/TR/NOTE-datetime>
  :datetime -- \"2001-04-22T04:53:44\", ditto
  :short   -- \"2001-04-22 Sun 04:53:44\"
  :long    -- \"2001-04-22 Sun 04:53:44 +0000 (GMT)\"
  :mbox    -- \"Sun Apr 22 04:53:44 2001\"
  :usa     -- \"Sun, 22 Apr 2001 04:53:44 +0000 (GMT)\"")
  (:method ((format function) se mi ho da mo ye dd dst tz)
    (funcall format se mi ho da mo ye dd dst tz))
  (:method ((format string) se mi ho da mo ye dd dst tz)
    (format nil format se mi ho da mo ye dd dst tz))
  (:method ((format t) se mi ho da mo ye dd dst tz)
    (error "~s: unknown format: ~s [date: ~s]" 'date-formatter format
           (date-formatter :short se mi ho da mo ye dd dst tz)))
  (:method ((format (eql :date)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (format nil "~d-~2,'0d-~2,'0d" ye mo da))
  (:method ((format (eql :datetime)) se mi ho da mo ye dd dst tz)
    (declare (ignore dd))
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~@[Z~a~]"
            ye mo da ho mi se (and (/= 0 tz) (tz->string tz dst nil))))
  (:method ((format (eql :short)) se mi ho da mo ye dd dst tz)
    (format nil "~d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d~@[ ~a~]"
            ye mo da (aref +week-days+ dd) ho mi se
            (and (/= 0 tz) (tz->string tz dst))))
  (:method ((format (eql :long)) se mi ho da mo ye dd dst tz)
    (format nil "~d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a"
            ye mo da (aref +week-days+ dd) ho mi se (tz->string tz dst)))
  (:method ((format (eql :mbox)) se mi ho da mo ye dd dst tz)
    (format nil "~a ~a ~d ~2,'0d:~2,'0d:~2,'0d ~d~@[ ~a~]"
            (aref +week-days+ dd) (aref +month-names+ (1- mo))
            da ho mi se ye (and (/= 0 tz) (tz->string tz dst))))
  (:method ((format (eql :usa)) se mi ho da mo ye dd dst tz)
    (format nil "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d ~a"
            (aref +week-days+ dd) da (aref +month-names+ (1- mo))
            ye ho mi se (tz->string tz dst))))

(defcustom *y2k-cut* (mod 100) 50
  "*The first year which goes to the 21st century in `fix-y2k'.")
(defun fix-y2k (ye)
  "Fix the `Y2K'-buggy year; cuts at `*y2k-cut*'.
You can redefine this function to your taste.
You can disable Y2K fixing with (setf (fdefinition 'fix-y2k) #'identity)"
  (cond ((< ye *y2k-cut*) (+ ye 2000))
        ((<= *y2k-cut* ye 99) (+ ye 1900))
        (t ye)))

(defun string->dttm (xx &key (start 0) end)
  "Parse the string into a date/time integer."
  (declare (simple-string xx))
  (or (string-w3-dttm xx :start start :end end)
      (multiple-value-bind (v0 v1 v2 v3 v4 v5 v6 v7)
          (values-list
           (delete-if (lambda (st) ; remove week day names
                        (and (symbolp st)
                             (position-limit (to-string st) +week-days+ 3)))
                      (string-tokens
                       (purge-string (if (< (count #\- xx) 2) xx
                                         ;; kill #\- in yyyy-mm-dd
                                         (substitute #\Space #\- xx :count 2))
                                     ":,/|")
                       :max 9 :start start :end end)))
        (flet ((eut (se mi ho da mo ye tz1 tz2)
                 (multiple-value-bind (sec ms) (floor (or se 0))
                   (+ ms (encode-universal-time sec (or mi 0) (or ho 0) da
                                                (infer-month mo) (fix-y2k ye)
                                                (infer-timezone tz1 tz2))))))
          (if (numberp v0)
              (eut v5 v4 v3 (min v0 v2) v1 (max v0 v2) v6 v7)
              (eut v4 v3 v2 v1 v0 v5 v6 v7))))))

(defun infer-month (mon)
  "Get the month from the object, number or name."
  (if (numberp mon) mon
      (let ((pos (position-limit (to-string mon) +month-names+ 3)))
        (when pos (1+ pos)))))

(declaim (ftype (function (days-t) (values (integer 0 6))) days-week-day))
(defun days-week-day (days)
  "Return the week day of the date DAYS from the epoch."
  (declare (type days-t days))
  (nth-value 6 (decode-universal-time (* days +day-sec+) 0)))

(declaim (ftype (function (date) (values (integer 0 6))) date-week-day))
(defsubst date-week-day (date)
  "Return the week day of the date."
  (declare (type date date))
  (days-week-day (date-dd date)))

;;;
;;; generic
;;;

(eval-when (compile load eval) (fmakunbound 'date))
(declaim (ftype date-f-t date))
;;;###autoload
(defgeneric date (xx)
  (:documentation "Convert to or extract a date.
The argument can be:
   - a date - it is returned untouched;
   - a string - it is destructively parsed;
   - a symbol - it is uninterned and its name is destructively parsed;
   - an integer - interpreted as the number of days since the epoch,
   - a real number - interpreted as the number of seconds since the epoch;
   - a stream - read as Month Day Year;
   - a structure - the appropriate slot is used;
   - a cons - called recursively on CAR;
   - NIL - an error is signalled.")
  (:method ((xx date)) xx)
  (:method ((xx string))
    ;; The following formats are accepted:
    ;; `1969-12-7', `May 8, 1945', `1945, September 2'.
    (multiple-value-bind (ye mo da)
        (values-list (string-tokens (purge-string xx) :max 3))
      (if (numberp ye) (mk-date :ye ye :mo (infer-month mo) :da da)
          (mk-date :ye da :mo (infer-month ye) :da mo))))
  (:method ((xx null)) (error "Cannot convert NIL to date")) ; +bad-date+
  (:method ((xx symbol)) (unintern xx) (date (symbol-name xx)))
  (:method ((xx integer)) (days2date xx))
  (:method ((xx real)) (time2date xx))
  (:method ((xx stream))        ; Read date in format MONTH DAY YEAR
    (mk-date :mo (infer-month (read xx)) :da (read xx) :ye (read xx)))
  (:method ((xx cons)) (date (car xx))))

;;;
;;; utilities
;;;

(declaim (ftype (function (date-f-t t) (values (function (t) days-t)))
                days-since))
(defun days-since (key beg)
  "Return a function that will return the number of days between BEG
and (funcall KEY arg), as a fixnum. KEY should return a date."
  (declare (type date-f-t key))
  (let ((dd (date-dd (date beg))))
    (declare (type days-t dd))
    (lambda (rr) (- (date-dd (funcall key rr)) dd))))

(declaim (ftype (function (date-f-t t) (values (function (t) double-float)))
                days-since-f))
(defun days-since-f (key beg)
  "Return a function that will return the number of days between BEG
and (funcall KEY arg), as a double-float. KEY should return a date."
  (declare (type date-f-t key))
  (let ((dd (dfloat (date-dd (date beg)))))
    (declare (double-float dd))
    (lambda (rr) (dfloat (- (date-dd (funcall key rr)) dd)))))

(declaim (ftype (function () (values date)) today))
(defun today ()
  "Return today's date."
  (time2date (get-universal-time)))

(defun date= (d0 d1)
  "Check that the two dates are the same."
  (declare (type date d0 d1)) (= (date-dd d0) (date-dd d1)))

(defun date/= (d0 d1)
  "Check that the two dates are not the same."
  (declare (type date d0 d1)) (/= (date-dd d0) (date-dd d1)))

(defun date< (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (< (date-dd d0) (date-dd d1)))

(defun date> (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (> (date-dd d0) (date-dd d1)))

(defun date<= (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (<= (date-dd d0) (date-dd d1)))

(defun date<=3 (d0 d1 d2)
  "Check the precedence of the three dates."
  (declare (type date d0 d1 d2))
  (<= (date-dd d0) (date-dd d1) (date-dd d2)))

(defun date<3 (d0 d1 d2)
  "Check the precedence of the three dates."
  (declare (type date d0 d1 d2))
  (< (date-dd d0) (date-dd d1) (date-dd d2)))

(defun date>= (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (>= (date-dd d0) (date-dd d1)))

(defun date>=3 (d0 d1 d2)
  "Check the precedence of the three dates."
  (declare (type date d0 d1 d2))
  (>= (date-dd d0) (date-dd d1) (date-dd d2)))

(defun date>3 (d0 d1 d2)
  "Check the precedence of the three dates."
  (declare (type date d0 d1 d2))
  (> (date-dd d0) (date-dd d1) (date-dd d2)))

(defun date=*1 (&rest dates)
  "Check any number of dates for being the same."
  (declare (dynamic-extent dates))
  (apply #'= (map-into dates #'date-dd dates)))

(defun date=* (date1 &rest dates)
  "Check any number of dates for being the same."
  (let ((d1 (date-dd date1)))
    (declare (type days-t d1))
    (every (lambda (dd) (declare (type date dd)) (= d1 (date-dd dd)))
           dates)))

(declaim (ftype (function (date date) (values date)) date-max date-min))
(defsubst date-max (d0 d1)
  "Return the latest date."
  (declare (type date d0 d1))
  (if (date< d0 d1) d1 d0))

(defsubst date-min (d0 d1)
  "Return the earliest date."
  (declare (type date d0 d1))
  (if (date> d0 d1) d1 d0))

(defun date-latest (ll &key (key #'date))
  "Return the last date in the list thereof."
  (reduce #'date-max (rest ll) :key key
          :initial-value (funcall key (first ll))))

(defun date-earliest (ll &key (key #'date))
  "Return the last date in the list thereof."
  (reduce #'date-min (rest ll) :key key
          :initial-value (funcall key (first ll))))

(defun next-month-p (d0 d1)
  "True if D1 is the next month of D0."
  (declare (type date d0 d1))
  (let ((y0 (date-ye d0)) (y1 (date-ye d1))
        (m0 (date-mo d0)) (m1 (date-mo d1)))
    (or (and (= (1+ m0) m1) (= y0 y1))
        (and (= m0 12) (= m1 1) (= (1+ y0) y1)))))

(defun date-month= (d0 d1)
  "Return t if the dates are in the same month."
  (declare (type date d0 d1))
  (and (= (date-ye d0) (date-ye d1)) (= (date-mo d0) (date-mo d1))))

(defun next-quarter-p (d0 d1)
  "True if D1 is the next quarter of D0."
  (declare (type date d0 d1))
  (let ((y0 (date-ye d0)) (y1 (date-ye d1))
        (m0 (date-mo d0)) (m1 (date-mo d1)))
    (or (and (= m1 1) (= (1+ y0) y1) (> m0 9))
        (and (= y0 y1) (< m0 m1) (> (+ 4 m0) m1) (member m1 '(1 4 7 10))))))

(declaim (ftype (function (date) (values (integer 1 4))) date-quarter))
(defun date-quarter (dt)
  "Return the quarter of the date."
  (declare (type date dt))
  (1+ (floor (1- (date-mo dt)) 3)))

(defun date-quarter= (d0 d1)
  "Return t if the dates are in the same quarter."
  (declare (type date d0 d1))
  (and (= (date-ye d0) (date-ye d1)) (= (date-quarter d0) (date-quarter d1))))

(declaim (ftype (function (date &optional date) (values fixnum)) days-between))
(defsubst days-between (d0 &optional (d1 (today)))
  "Return the number of days between the two dates."
  (declare (type date d0 d1))
  (- (date-dd d1) (date-dd d0)))

(declaim (ftype (function (&optional date days-t) (values date))
                tomorrow yesterday))
(defun tomorrow (&optional (dd (today)) (skip 1))
  "Return the next day in a new date structure.
With the optional second argument (defaults to 1) skip as many days.
I.e., (tomorrow (today) -1) is yesterday."
  (declare (type date dd) (type days-t skip))
  (days2date (+ (date-dd dd) skip)))

(defsubst yesterday (&optional (dd (today)) (skip 1))
  "Return the previous day.  Calls tomorrow."
  (declare (type date dd) (type days-t skip))
  (tomorrow dd (- skip)))

(defun date-next-year (dd &optional (skip 1))
  "Increment (destructively) the year."
  (declare (type date dd) (type days-t skip))
  (incf (date-ye dd) skip)
  (fix-date dd))

(defun date-next-month (dd &optional (skip 1))
  "Increment (destructively) the month."
  (declare (type date dd) (type days-t skip))
  (multiple-value-bind (iy im) (floor (+ -1 skip (date-mo dd)) 12)
    (incf (date-ye dd) iy)
    (setf (date-mo dd) (1+ im))
    (fix-date dd)))

(defun date-next-all (dd &optional (skip 1))
  "Increment (non-destructively) year, month and day."
  (declare (type date dd) (type days-t skip))
  (date-next-year (date-next-month (tomorrow dd skip) skip) skip))

(defun working-day-p (&optional (dd (today)))
  "Is today a working day?"
  (declare (type date dd))
  (case (date-week-day dd)
    ((0 1 2 3 4) t)
    (t nil)))

(declaim (ftype (function (&optional date days-t) (values days-t))
                days-to-next-working-day))
(defun days-to-next-working-day (&optional (date (today)) (skip 1))
  "Return the number of days until the next Nth working day."
  (declare (type date date) (type days-t skip))
  (multiple-value-bind (ww rr) (floor skip 5)
    (+ (* 7 ww)
       (if (> (+ rr (min 4 (date-week-day date))) 4)
           (+ rr 2)
           rr))))

(declaim (ftype (function (&optional date days-t) (values date))
                next-working-day previous-working-day))
(defsubst next-working-day (&optional (date (today)) (skip 1))
  "Return the Nth next working day, ignoring holidays."
  (declare (type date date) (type days-t skip))
  (tomorrow date (days-to-next-working-day date skip)))

(defsubst previous-working-day (&optional (date (today)) (skip 1))
  "Return the previous working day, ignoring holidays."
  (declare (type date date) (type days-t skip))
  (tomorrow date (days-to-next-working-day date (- skip))))

(defcustom *black-day-week* (mod 7) 4 "*Friday.")
(defcustom *black-day-date* (mod 32) 13 "*13th.")
(defun black-days (&key (start (today)) (end 1)
                   (week-day *black-day-week*)
                   (month-day *black-day-date*))
  "Return the list of Fridays the 13th from START to END.
START is a date, END is either a date (later than START)
or an integer - the number of `black days' to return.
If it is negative, search backwards.
The second value returned is the number of black days found."
  (declare (type date start) (type (or days-t date) end)
           (type (mod 32) week-day month-day))
  (when (date-p end)
    (assert (date<= start end) (end)
            "~s: END (~s) should be after START (~s)" 'black-days end start))
  (do* ((dd (date-dd start)) res (numf 0)
        (step (* 7 (if (and (numberp end) (minusp end)) -1 1)))
        (fri (+ dd (mod (- week-day (days-week-day dd)) step)) (+ fri step))
        (dt (days2date fri) (days2date fri)))
       ((if (date-p end) (date> dt end) (= numf (abs end)))
        (values (nreverse res) numf))
    (when (= month-day (date-da dt))
      (push dt res)
      (incf numf))))

;;;
;;; dates and lists
;;;

(declaim (ftype (function (t list &optional date-f-t t) (values list))
                date-in-list))
(defun date-in-list (dt lst &optional (key #'date) last)
  "Return the tail of LST starting with DT.
If LAST is non-nil, make sure that the next date is different.
*Important*: assumes that the list is ordered according to `date>'."
  (declare (list lst) (type date-f-t key))
  (let ((ll (binary-member (date dt) lst :key key :test #'date<=)))
    (if (and last ll (cdr ll)) (skip-to-new ll :key key :test #'date=) ll)))

(defun sync-dates (lists &key labels key cpy set (out *standard-output*)
                   op skip)
  "Make all the lists have records for all the dates.
If LABELS is not given, no messages are printed.
\(funcall KEY record) ==> date
\(funcall CPY rec) ==> copy of rec
\(funcall SET rec dt) ==> set the date of the rec.
Stops when the lists end. Prints messages to OUT.
Calls OP on the section of the LISTS with the same dates, using the
previous record when SKIP is non-nil and nil otherwise.
  (sync-dates lists &key labels key cpy set (out *standard-output*) op skip)"
  (declare (list lists) (type (or null stream) out))
  (mesg t out "~&Synching dates in ~d lists.~%" (length lists))
  (do ((sec (copy-list lists)) (heads (make-list (length lists))) fnn (nerr 0)
       (bd +bad-date+) (err nil nil) (ckey +bad-date+) (len 0 (1+ len)))
      ((every #'null sec)
       (mesg t out "~:d records in ~d lists checked. ~d error~:p found.~%"
             len (length lists) nerr))
    (declare (type index-t nerr len) (type date bd ckey))
    ;; get the current date
    (setq fnn (member nil sec :test-not #'eq)
          bd (funcall key (caar fnn)))
    (dolist (ls (rest fnn))
      (when ls (setq ckey (funcall key (car ls)))
            (when (date> bd ckey) (setq bd ckey))
            (unless (date= bd ckey) (setq err t))))
    ;; handle date mismatches
    (when (and out labels err)
      (format out " --[~:d]--> date mismatch:~:{~%  <~a: ~a>~}~%"
              len (mapcar #'cons labels sec))
      (incf nerr))
    ;; shift, fix and operate
    (mapl (lambda (ls hd)
            (if (date= bd (funcall key (caar ls)))
                (setf (car hd) (caar ls))
                (let ((nr (funcall cpy (caar ls))))
                  (funcall set (caar ls) bd)
                  (setf (cdar ls) (cons nr (cdar ls))
                        (car hd) (if skip (caar ls)))))
            (pop (car ls)))
          sec heads)
    (when op (apply op heads))))

(defmacro sync-dates-ui (lists &key labels key cpy (out '*standard-output*)
                         op skip)
  "Use KEY for SET, which should be a slot. See `sync-dates'."
  `(sync-dates ,lists :labels ,labels :key (lambda (rr) (slot-value rr ,key))
    :cpy ,cpy :out ,out  :op ,op :skip ,skip
    :set (lambda (rr dd) (setf (slot-value rr ,key) dd))))

(provide :cllib-date)
;;; date.lisp ends here
