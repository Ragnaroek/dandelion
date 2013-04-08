;;; GetQuote
;;; get stock/mutual fund quotes from the Internet
;;; via the WWW using HTTP/1.0, save into a file, plot.
;;;
;;; Copyright (C) 1997-2004 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: gq.lisp,v 2.34 2005/01/27 23:02:48 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gq.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t', `whitespace-char-p'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `with-xml-input'
  (require :cllib-xml (translate-logical-pathname "cllib:xml"))
  ;; `make-url'
  (require :cllib-url (translate-logical-pathname "cllib:url"))
  ;; `next-token', `next-number', `text-stream'
  (require :cllib-html (translate-logical-pathname "cllib:html"))
  ;; `plot-dated-lists'
  (require :cllib-gnuplot (translate-logical-pathname "cllib:gnuplot")))

(in-package :cllib)

(export '(update-quotes *hist-data-file*))

;;;
;;;
;;;

(defcustom *gq-error-stream* (or null stream) *error-output*
  "The error stream for `with-open-url'.")
(defcustom *gq-timeout* (real 0) 600
  "*The default timeout, in seconds.")

(defun gq-complete-url (url &rest ticks)
  "Complete URL to get the quotes for TICKS."
  (setq url (if (url-p url) (copy-url url) (url url)))
  (setf (url-path url) (format nil "~a~{~:@(~a~)~^+~}" (url-path url) ticks))
  (mesg :log *gq-error-stream* " *** <URL:~s>~%" url)
  url)

;;;
;;; {{{ Daily Data
;;;

(defstruct (daily-data (:conc-name dd-))
  (nav 0d0 :type double-float)
  (chg 0d0 :type double-float)
  (prc 0d0 :type double-float)
  (vol 0 :type integer)
  (bid 0d0 :type double-float)
  (ask 0d0 :type double-float)
  (pre 0d0 :type double-float)
  (low 0d0 :type double-float)
  (hgh 0d0 :type double-float))

(defun mk-daily-data (&rest args)
  "Make the DAILY-DATA structure, inferring the missing information."
  (let* ((dd (apply #'make-daily-data args))
         (nav (dd-nav dd)) (chg (dd-chg dd)))
    (unless (zerop chg)
      (when (zerop (dd-prc dd)) (setf (dd-prc dd) (* 100d0 (/ chg nav))))
      (when (zerop (dd-pre dd)) (setf (dd-pre dd) (- nav chg))))
    dd))

(defmethod print-object ((dd daily-data) (out stream))
  (if *print-readably* (call-next-method)
      (format out "price:~15t~7,2f~35tbid:~45t~7,2f
previous:~15t~7,2f~35task:~45t~7,2f
change:~15t~7,2f~35thigh:~45t~7,2f
%:~15t~7,2f~35tlow:~45t~7,2f~%~[~:;~:*vol: ~17:d~%~]"
              (dd-nav dd) (dd-bid dd) (dd-pre dd) (dd-ask dd) (dd-chg dd)
              (dd-hgh dd) (dd-prc dd) (dd-low dd) (dd-vol dd))))

(declaim (ftype (function () (values date)) gq-guess-date))
(defun gq-guess-date ()
  "Guess the date: the last date when the quote could be available."
  (multiple-value-bind (se mi ho da mo ye wd) (get-decoded-time)
    (declare (ignore se mi) (fixnum ho da mo ye wd))
    (let ((td (mk-date :ye ye :mo mo :da da)))
      (declare (type date td))
      (if (< wd 5)              ; weekday
          (if (< ho 17) (yesterday td) td)
          (yesterday td (if (= wd 5) 1 2))))))

(defmacro with-url-xml ((var url &key (err '*error-output*)
                             (max-retry '*url-max-retry*)
                             (timeout '*gq-timeout*))
                        &body body)
  "Open the URL as an XML stream."
  (with-gensyms ("WUX-" str)
    `(with-open-url (,str ,url :err ,err :max-retry ,max-retry
                     :timeout ,timeout)
      (mesg :xml ,err " *** header:~%")
      (loop :for xx = (read-line ,str nil nil) :do (mesg :xml ,err "~s~%" xx)
       :while (and xx (not (string= "" xx))))
      (mesg :xml ,err " *** data:~%")
      (with-xml-input (,var ,str) ,@body))))

;;;###autoload
(defun get-url-xml-tokens (url &key (out *standard-output*)
                           (err *error-output*))
  "Dump the URL token by token.
This is just a debugging function, to be called interactively."
  (with-url-xml (sock (url url) :err err)
    (loop :for ii :upfrom 1 :and rr = (read sock nil +eof+)
          :until (eq +eof+ rr)
          :do (format out "~&~d: " ii) (pr rr out) (terpri out))))

(defun stream-next-num (stream)
  (loop (let ((cur (read stream))) (when (numberp cur) (return cur)))))

(defun gq-dat (obj)
  ;; (mesg :log *gq-error-stream* "-> ~s~%" obj)
  (typecase obj
    (xml-obj (car (xmlo-data obj)))
    (xml-comment (xml-comment-data obj))
    (cons (cdr obj))
    (string obj)
    (symbol (symbol-name obj))
    (t (prin1-to-string obj))))

(defun gq-next (stream &optional (nn 1))
  (loop :repeat nn :for cur = (gq-dat (read stream)) :finally (return cur)))

(defun gq-skip (stream string)
  (loop (when (string-equal (gq-next stream) string) (return))))

(defun gq-next-num (stream &optional (nn 1))
  (labels ((nump (ch)
             (or (whitespace-char-p ch)
                 (member ch '(#\- #\.))
                 (digit-char-p ch)))
           (clean (line)
             (nsubstitute #\Space #\% line)
             (nsubstitute #\Space #\+ line)
             (nsubstitute #\Space #\, line)
             line)
           (next (stream)
             (loop (let ((tok (clean (gq-next stream))))
                     (when (and (some #'digit-char-p tok) (every #'nump tok))
                       (return (with-standard-io-syntax
                                 (dfloat (read-from-string tok)))))))))
    (loop :repeat nn :for cur = (next stream) :finally (return cur))))

(defun date-mo/da/ye (xx)
  "Parse the date in the MM/DD/YYYY format"
  (multiple-value-bind (mo da ye)
      (values-list (string-tokens (purge-string xx "/") :max 3))
    (mk-date :ye ye :mo mo :da da)))

(defun get-quotes-apl (url &rest ticks)
  "Get the data from the APL WWW server."
  (with-url-xml (sock (apply #'gq-complete-url url ticks)
                       :timeout *gq-timeout* :err *gq-error-stream*)
    (do ((ti ticks (cdr ti)) res dt)
        ((null ti) (cons (or dt (gq-guess-date)) (nreverse res)))
      (gq-skip sock (symbol-name (car ti)))
      (push (mk-daily-data :nav (gq-next-num sock) :chg (gq-next-num sock)
                           :prc (/ (gq-next-num sock) 100d0) :bid 0d0 :ask 0d0
                           :pre (gq-next-num sock) :low (gq-next-num sock)
                           :hgh (gq-next-num sock))
            res)
      ;;(setq dt (string-tokens (gq-next sock 2))
      ;;      dt (infer-date (car dt) (cadr dt)))
      (mesg :log *gq-error-stream* " *** Found [~s] [~a]: ~a~%"
            (car ti) dt (car res)))))

(defun get-quotes-yahoo (url &rest ticks)
  "Get the data from the Yahoo WWW server."
  (with-open-url (sock (apply #'gq-complete-url url ticks)
                       :timeout *gq-timeout* :err *gq-error-stream*)
    (mesg t *gq-error-stream* " *** header:~%")
    (loop :for xx = (read-line sock nil nil)
          :do (mesg t *gq-error-stream* "~s~%" xx)
          :while (and xx (not (string= "" xx))))
    (mesg t *gq-error-stream* " *** data:~%")
    (let ((*read-default-float-format* 'double-float)
          date res line)
      (dolist (ti ticks (cons date (nreverse res)))
        (setq line (read-line sock))
        (mesg t *gq-error-stream* " * ~a~%" line)
        (multiple-value-bind (name nav dt time chg open hgh low vol)
            (values-list (string-tokens (purge-string line ",") :max 9))
          (declare (ignore time open))
          (assert (string= (symbol-name ti) name) (name ti)
                  "~s: name(~s)/tick(~s) mismatch" 'get-quotes-yahoo name ti)
          (setq dt (date-mo/da/ye dt))
          (when (and date (date/= date dt))
            (warn "~s: date mismatch: ~s and ~s"
                  'get-quotes-yahoo date dt))
          (setq date dt)
          (push (mk-daily-data :nav nav :chg chg
                               :vol (if (numberp vol) vol 0)
                               :low (if (numberp low) low 0d0)
                               :hgh (if (numberp hgh) hgh 0d0))
                res))))))

(defun get-quotes-sm (url &rest ticks)
  "Get the data from the StockMaster WWW server."
  (do ((ti ticks (cdr ti)) (ds nil nil) (vs nil nil) hh ar res ts dt
       (gd (gq-guess-date)))
      ((null ti)
       (setq dt (caar (last (car hh))))
       (unless (date= dt gd)
         (format t "Warning: implied date (~a) differs from given date (~a)~%"
                 gd dt))
       (values (cons dt (nreverse res))
               (apply #'mapcar
                      (lambda (&rest cns)
                        (assert
                         (every (lambda (cn) (date= (caar cns) (car cn)))
                                (cdr cns)) ()
                         "Date mismatch: ~a~%" cns)
                        (make-hist :date (caar cns) :navs (mapcar #'cdr cns)))
                      (nreverse hh))
               (nreverse ar)))
    (mesg :log *gq-error-stream* " *** Processing ~a~%" (car ti))
    (with-url-xml (sock (gq-complete-url url (car ti))
                        :timeout *gq-timeout* :err *gq-error-stream*)
      (do ((st (read-line sock) (read-line sock))
           (sy (concatenate 'string "(" (symbol-name (car ti)) ")")))
          ((string-beg-with sy st)
           (mesg :log *gq-error-stream* "found `~a'~%" st))
        (declare (simple-string st sy)))
      (setq ts (make-text-stream :sock sock))
      (push (mk-daily-data :nav (next-number ts) :chg (next-number ts)
                           :prc (next-number ts))
            res)
      (mesg :log *gq-error-stream* "~a~%" (car res))
      (push (list (next-number ts :num 5)
                  (next-token ts :type 'number :dflt 0d0)
                  (next-token ts :type 'number :dflt 0d0)
                  (next-token ts :type 'number :dflt 0d0)) ar)
      (push (mapcar
             #'cons
             (dotimes (ii 10 (nreverse ds))
               (push (infer-date (next-token ts) (next-token ts)) ds))
             (dotimes (ii 10 (nreverse vs))
               (push (next-number ts) vs)))
            hh))))

(defun get-quotes-pf (url &rest ticks)
  "Get the data from the PathFinder WWW server."
  (with-url-xml (sock (apply #'gq-complete-url url ticks)
                      :timeout *gq-timeout* :err *gq-error-stream*)
    ;; the morons that run PathFinder put commas in the
    ;; attribute lists!!!  BEWARE!!!
    (do ((ti ticks (cdr ti)) res)
        ((null ti) (cons (gq-guess-date) (nreverse res)))
      (gq-skip sock (symbol-name (car ti)))
      (push (mk-daily-data :nav (gq-next-num sock) :chg (gq-next-num sock 2))
            res)
      (mesg :log *gq-error-stream* " *** Found [~s]: ~a~%"
            (car ti) (car res)))))

(defun get-quotes-cnn (url &rest ticks)
  "Get the data from the CNN-FN WWW server."
  (with-url-xml (sock (apply #'gq-complete-url url ticks)
                      :timeout *gq-timeout* :err *gq-error-stream*)
    (do ((ti ticks (cdr ti)) res)
        ((null ti) (cons (gq-guess-date) (nreverse res)))
      (gq-skip sock (symbol-name (car ti)))
      (push (mk-daily-data :nav (stream-next-num sock)
                           :chg (stream-next-num sock))
            res)
      (mesg :log *gq-error-stream* " *** Found [~s]: ~a~%"
            (car ti) (car res)))))

(defun get-quotes-xmltoday (url &rest ticks)
  "Get the data from the XMLToday StockQuote server."
  (let ((xo (with-url-xml (sock (apply #'gq-complete-url url ticks)
                                :timeout *gq-timeout* :err *gq-error-stream*)
              (read-from-stream sock)))
        (*xml-print-xml* :readably)
        date res)
    (mesg :xml *gq-error-stream* "~s" xo)
    (assert (and xo (null (cdr xo))) (xo)
            "~s: wrong object read: ~s" 'get-quotes-xmltoday xo)
    (setq xo (xml-purge-data (xmlo-name-check (car xo) "stock_quotes")))
    (mapc (lambda (ti da)
            (let* ((data (xmlo-data (xmlo-name-check da "stock_quote"))))
              (assert (string= (symbol-name ti)
                               (car (xmlo-data (xmlo-name-check
                                                (first data) "symbol"))))
                      (ti da) "~s: tick(~s)/data mismatch:~s"
                      'get-quotes-xmltoday ti da)
              (let* ((wh (xmlo-data (xmlo-name-check (second data) "when")))
                     (dd (car (xmlo-data (xmlo-name-check (car wh) "date"))))
                     (dt (date-mo/da/ye dd)))
                (when (and date (date/= date dt))
                  (warn "~s: date mismatch: ~s and ~s"
                        'get-quotes-xmltoday date dt))
                (setq date dt))
              (flet ((get-price (type)
                       (let ((val (xmlo-tag (xmlo-name-check
                                             (find type data :test #'equal :key
                                                   (lambda (xo)
                                                     (xmlo-tag xo "type")))
                                             "price")
                                            "value")))
                         (if (string-equal val "n/a") 0d0
                             (read-from-string val))))
                     (get-data (type)
                       (let ((val (car (xmlo-data
                                        (find type data :test #'string=
                                              :key #'xmlo-nm)))))
                         (if (string-equal val "n/a") 0
                             (read-from-string val)))))
                (let ((*read-default-float-format* 'double-float))
                  (push (mk-daily-data :nav (get-price "ask")
                                       :hgh (get-price "dayhigh")
                                       :low (get-price "daylow")
                                       :chg (float (get-data "change") 1d0)
                                       :vol (get-data "volume"))
                        res)))))
          ticks (xmlo-data xo))
    (cons date (nreverse res))))

(defcustom *get-quote-url-list* list
  (list (list (make-url :prot :http :port 80 :host "finance.yahoo.com"
                        :path "/d/quotes.csv?f=sl1d1t1c1ohgv&e=.csv&s=")
              'get-quotes-yahoo "Yahoo")
        (list (make-url :prot :http :port 80 :host "www.xmltoday.com"
                        :path "/examples/stockquote/getxmlquote.vep?s=")
              'get-quotes-xmltoday "xmltoday")
        (list (make-url :prot :http :port 80 :host "www.stockmaster.com"
                        :path "/exe/sm/chart&Symbol=")
              'get-quotes-sm "StockMaster")
        (list (make-url :prot :http :port 80 :host "quote.pathfinder.com"
                        :path "/money/quote/qc?symbols=")
              'get-quotes-pf "PathFinder")
        (list (make-url :prot :http :port 80 :host "qs.cnnfn.com"
                        :path "/tq/stockquote?symbols=")
              'get-quotes-cnn "CNN")
        ;; SecAPL appears to be dead
        ;; (list (make-url :prot :http :port 80 :host "qs.secapl.com"
        ;;                :path "/cgi-bin/qs?ticks=")
        ;;      'get-quotes-apl "APL")
        )
  "*The list of known URL templates for getting quotes.")

(defun get-quotes (server &rest ticks)
  "Get the quotes from one of `*get-quote-url-list*'.
The first arg, SERVER, when non-nil, specifies which server to use,
 when it is T, all server are tried unconditionally."
  (flet ((gq (srv) (apply (second srv) (first srv) ticks)))
    (if (and server (not (eq server t)))
        (let ((qq (if (numberp server) (nth server *get-quote-url-list*)
                      (find server *get-quote-url-list* :key #'third
                            :test (lambda (se na)
                                    (search se na :test #'char-equal))))))
          (assert qq (server) "Unknown server `~a'.~%" server)
          (multiple-value-call #'values (third qq) (gq qq)))
        (do ((ql *get-quote-url-list* (cdr ql)) res name log)
            ((or (endp ql) (and (null server) (car res)))
             (mesg :log *gq-error-stream* "Results:~%~:{~a: ~a~%~}"
                   (nreverse log))
             (values-list (cons name res)))
          (format t "~&Trying ~a...~%" (setq name (caddar ql)))
          (let ((re (multiple-value-list (ignore-errors (gq (car ql))))))
            (if (car re) (setq res re)
                (format t "~&~a: ~a~%" name (cadr re)))
            (push (list name (or (car re) (cadr re))) log))))))

(defun infer-date (&optional mon day)
  "Guess what the date is.
Return the most recent date with these month and day.
If the first argument is a date, fix the year."
  (multiple-value-bind (se mm ho da mo ye) (get-decoded-time)
    (declare (ignore se mm) (fixnum mo ye mm))
    (if mon
        (if (date-p mon)
            (progn
              (when (< 1 (abs (- ye (date-ye mon))))
                (setf (date-ye mon) ye)
                (fix-date mon))
              mon)
            (let ((mm (infer-month mon)))
              (mk-date :da day :mo mm :ye (if (<= mm mo) ye (1- ye)))))
        (let ((dd (mk-date :da da :mo mo :ye ye)))
          (if (< ho 18) (previous-working-day dd) dd)))))

;;;
;;; }}}{{{ Holdings
;;;

(defcustom *hist-data-file* pathname
  (mk-path (user-homedir-pathname) :directory '(:relative "text")
           :name "invest" :type "txt")
  "*The file with the historical data.
See the value of `*hist-data-file-header*' for file format.")
(defcustom *hist-data-file-header* simple-string
  ";*GetQuote portfolio*
; file format is as follows:
; - lines starting with `;' are ignored
; - empty lines are ignored
; - all non-ignored lines until the separator token, `~',
; are expected to contain ticker info in the following format
;     <ticker> <number of shares> <buying price> \"<comment (fund name)>\"
; - all the lines after that are history of this portfolio, in the format
;     <date> <total value> [<price>]*
" "The header of the data file.")
(defcustom *hist-data-file-sep* symbol :~
  "*The separator between the portfolio and its history")
(defcustom *holdings* list nil
  "The holdings, to be read from `*hist-data-file*'.")
(defcustom *history* list nil
  "The history, to be read from `*hist-data-file*'.")

(defstruct (pfl)
  (tick nil :type symbol)       ; TICKer
  (nums 0d0 :type double-float) ; NUMber of Shares
  (bprc 0d0 :type double-float) ; Base PRiCe
  (name "" :type string))       ; full NAME

(defun read-pfl (stream ra)
  "Read a PFL from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (let ((*read-default-float-format* 'double-float))
    (values (make-pfl :tick ra :nums (read stream) :bprc (read stream)
                      :name (read stream))
            (read stream nil +eof+))))

(defmethod print-object ((pfl pfl) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~:@(~a~) ~8,3f ~7,2f ~a" (pfl-tick pfl) (pfl-nums pfl)
              (pfl-bprc pfl) (pfl-name pfl))))

(defsubst find-pfl (sy)
  "Find the holding corresponding to the symbol."
  (declare (symbol sy)) (find sy *holdings* :key #'pfl-tick :test #'eq))

(defstruct (hist)
  (date +bad-date+ :type date)
  (totl 0d0 :type double-float)
  (navs nil :type list))

(defmethod value ((hs hist)) (hist-totl hs))
(defmethod date ((hs hist)) (hist-date hs))
(defmethod code ((pfl pfl)) (pfl-tick pfl))

(defun read-hist (stream ra)
  "Read a HIST from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (do* ((*read-default-float-format* 'double-float)
        (hist (make-hist :date (date ra) :totl (read stream))) rr
        (vl (read stream) (read stream nil +eof+)))
       ((not (numberp vl)) (setf (hist-navs hist) (nreverse rr))
        (values hist vl))
    (push vl rr)))

(defmethod print-object ((hist hist) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~a ~15,5f~{ ~7,2f~}" (hist-date hist)
              (hist-totl hist) (hist-navs hist))))

(defun hist-totl-comp (hold navs)
  "Compute the correct total from the hist records and holdings list."
  (declare (list hold navs))
  (loop :for ho :of-type pfl :in hold :for vl :of-type double-float :in navs
        :sum (* vl (pfl-nums ho)) :of-type double-float))

(defun read-data-file (file)
  "Return 2 values: the list of holdings and the history list."
  (with-open-file (fl file :direction :input :if-does-not-exist nil)
    (unless fl
      (return-from read-data-file
        (format t " *** Cannot open file `~a' for reading~%" file)))
    (values (read-list-from-stream fl #'read-pfl :eof *hist-data-file-sep*)
            (read-list-from-stream fl #'read-hist))))

(defun save-data (file hold hist)
  "Save the history into the file.
If the file doesn't exist, it is created. If it exists,
only the data after `*hist-data-file-sep*' is changed."
  (declare (list hold hist))
  (with-open-file (outst file :direction :io :if-exists :overwrite)
    (cond ((< 0 (file-length outst)) ; file existed
           (format t "File `~a' exists.  Appending...~%" file)
           (do (zz (*package* +kwd+))
               ((or (eq zz +eof+) (eq zz *hist-data-file-sep*))
                (when (eq zz +eof+)
                  (error "File `~a' is corrupted: `~a' not found.~%"
                         file *hist-data-file-sep*)))
             (setq zz (read outst nil +eof+)))
           (terpri outst)
           (write-list-to-stream hist outst))
          (t                            ; new file
           (format t "File `~a' does not exists. Creating...~%" file)
           (princ *hist-data-file-header* outst)
           (terpri outst)
           (write-list-to-stream hold outst)
           (terpri outst) (princ *hist-data-file-sep* outst) (terpri outst)
           (write-list-to-stream hist outst))))
  (format t "~d record~:p about ~r holding~:p written.~%"
          (length hist) (length hold)))

(defun navs= (nav1 nav2)
  "Test that the two NAV lists are the same within $0.01."
  (let ((*num-tolerance* 5d-3))
    (every #'approx=-abs nav1 nav2)))

(defun gq-fix-hist (hold hist hhh)
  "Fix the values in the history. Return T if something was fixed."
  (declare (list hist hold hhh))
  (let ((fixed nil))
    (when (consp hhh)
      (setq hhh
            (map-sorted
             'list
             (lambda (hi hs)
               (when hs
                 (cond (hi
                        (unless (navs= (hist-navs hi) (hist-navs hs))
                          (format t "Incorrect NAVs on ~a:
~5tOriginal:~15t~{~7,2f~}~%~5tActual:~15t~{~7,2f~}~%"
                                  (hist-date hi) (hist-navs hi) (hist-navs hs))
                          (setf (hist-navs hi) (hist-navs hs)
                                (hist-totl hi)
                                (hist-totl-comp hold (hist-navs hs))
                                fixed t)))
                       (t
                        (setf (hist-totl hs)
                              (hist-totl-comp hold (hist-navs hs)))
                        (format t "Missing record added: [~a].~%" hs)
                        (setq hi hs fixed t))))
               hi)
             #'date< hist hhh :ckey #'hist-date))
      ;; modify hist by side effect
      (setf (car hist) (car hhh) (cdr hist) (cdr hhh)))
    (let (ntot)
      (dolist (hr hist)
        (setq ntot (hist-totl-comp hold (hist-navs hr)))
        (unless (approx= ntot (hist-totl hr) 1d-2 1d-5)
          (format t "Incorrect total (~a): ~15,5f; correct: ~15,5f~%"
                  (hist-date hr) (hist-totl hr) ntot)
          (setf (hist-totl hr) ntot fixed t))))
    fixed))

(defun pr-res (out pref v0 v1 per apy v2 &optional (tc (percent-change v2 v0)))
  (declare (stream out) (double-float v0 v1 per apy v2 tc))
  (let ((fmt (formatter
              "~15t~2,7/comma/ - ~2,7/comma/ = ~2,6/comma/    [~7,3f%")))
    (format out "~aP/L: total:" pref)
    (funcall fmt out v0 v1 (- v0 v1) per)
    (format out " APY:~8,3f%]~%        today:" apy)
    (funcall fmt out v0 v2 (- v0 v2) tc)
    (format out "]~2%")))

(defun show-results (tickers srv res &optional (out *standard-output*))
  "Print the results."
  (when res
    (format out "~2%~72:@<~a results for ~a:~>~%" srv (car res))
    (map nil (lambda (ti dd) (format out " *** ~s:~%~s~%" ti dd))
         tickers (cdr res))))

(defun process-results (hold hist srv res yea
                        &optional (out *standard-output*))
  "Process the results, update the history.
Return non-nil if the history was updated and needs to be saved.
RES is a list of SERVER, DATE and DAILY-RECS.
YEA is the list of 1, 3, 5, and 10-year returns."
  (declare (list hold hist res) (stream out))
  (unless res
    (return-from process-results (format t "no results - timed out?~%")))
  (let* ((lh (last hist 2)) (ctot 0d0) (otot 0d0) (ptot 0d0)
         (begd (hist-date (car hist))) pers apy db
         (nnavs (mapcar #'dd-nav (cdr res))) (*print-case* :upcase)
         (pnavs (mapcar #'dd-pre (cdr res))))
    (declare (double-float ctot otot ptot) (type date begd))
    (format out "~2%~72:@<~a results [~a]:~>
~72:@<<[~a -- ~a (~:d days)]>~>~2%"
            srv (current-time nil) begd (car res)
            (setq db (days-between begd (car res))))
    (apply #'mapc
           (lambda (hl dd &optional ye)
             (let* ((cv (* (pfl-nums hl) (dd-nav dd)))
                    (ov (* (pfl-nums hl) (pfl-bprc hl)))
                    (cn (cond ((not (zerop (dd-pre dd))) (dd-pre dd))
                              ((not (zerop (dd-chg dd)))
                               (- (dd-nav dd) (dd-chg dd)))
                              (t (nth (position (pfl-tick hl) hold :test #'eq
                                                :key #'pfl-tick)
                                      (hist-navs (cadr lh))))))
                    (pv (* (pfl-nums hl) cn)))
               (declare (double-float cv ov cn pv))
               (format out "Fund: ~a [~a]~48t~7,3f * ~6,2f = ~2,6:/comma/~%"
                       (pfl-tick hl) (pfl-name hl) (pfl-nums hl)
                       (dd-nav dd) cv)
               (when ye
                 (format out "~10t[1ye:~6,2f%; 3ye:~6,2f%; 5ye:~6,2f%; ~
10ye:~6,2f%]~%" (first ye) (second ye) (third ye) (fourth ye)))
               (prin1 dd out)
               (incf ctot cv) (incf otot ov) (incf ptot pv)
               (setf (values pers apy)
                     (percent-change (pfl-bprc hl) (dd-nav dd) db))
               (pr-res out "   " cv ov pers apy pv
                       (percent-change cn (dd-nav dd)))))
           hold (rest res) (if yea (list yea) nil))
    (setf (values pers apy) (percent-change otot ctot db))
    (pr-res out " * " ctot otot pers apy ptot)
    (cond ((date> (car res) (hist-date (cadr lh)))
           (unless (or (some #'zerop pnavs)
                       (navs= (hist-navs (cadr lh)) pnavs)
                       (and (navs= (hist-navs (car lh)) pnavs)
                            (navs= (hist-navs (cadr lh)) nnavs)))
             (format out "A discrepancy found:~% last record:~15t~{~7,2f~}~% ~
previous day:~15t~{~7,2f~}~%Added an extra record~%~5t~{~a~}~%"
                     (hist-navs (cadr lh)) pnavs
                     (setf (cddr lh)
                           (list (make-hist :date (previous-working-day
                                                   (car res))
                                            :navs pnavs :totl
                                            (hist-totl-comp hold pnavs)))))
             (setq lh (cdr lh)))
           (cond ((navs= nnavs (hist-navs (cadr lh)))
                  (format out "Same NAVs as on ~a, no record added.~%"
                          (hist-date (cadr lh))))
                 (t (format out "A record for date ~a added:~%~5t~{~a~}~%"
                            (car res)
                            (setf (cddr lh)
                                  (list (make-hist
                                         :date (car res) :navs nnavs
                                         :totl (hist-totl-comp hold
                                                               nnavs)))))
                    t)))
          (t (format out "Old news [~a <= ~a].~%"
                     (car res) (hist-date (cadr lh)))))))

(defun plot-portfolio (hold hist plot)
  "Plot the portfolio."
  (declare (list hold hist))
  (do ((ii 0 (1+ ii)) (hl hold (cdr hl)) res)
      ((null hl)
       (plot-dated-lists
        (hist-date (car hist)) (hist-date (car (last hist)))
        (cons (mk-dl (list (cons :all hist)) :date 'hist-date :name
                     "Total Value" :misc 'hist-totl)
              (nreverse res))
        :rel t :slot 'misc :grid t
        :title "Portfolio History" :data-style :linespoints
        :ylabel "Relative Value" :plot plot :legend '(:top :left :box)))
    (declare (fixnum ii))
    (push (mk-dl (list (cons :all hist)) :date 'hist-date
                 :name (pfl-name (car hl))
                 :code (pfl-tick (car hl)) :misc
                 (let ((ii ii)) (declare (fixnum ii))
                      (lambda (hs) (nth ii (hist-navs hs)))))
          res)))

;;;
;;; }}}{{{ Run the thing
;;;

(defcustom *gq-log* pathname
  (mk-path (user-homedir-pathname) :directory '(:relative "text")
           :name "getquote" :type "log")
  "The log file for the batch processing.")

;;;###autoload
(defun update-quotes (&key (plot nil plotp) server debug
                      (log #+(or win32 mswindows) *gq-log*
                           #-(or win32 mswindows) nil)
                      tickers (hist-file *hist-data-file*))
  "Read the history. Update quotes. Plot (optionally),
if PLOT is non-nil, or if it is not given but there was new data.
If PLOT is T, just plot, do not try to update quotes.
See `*get-quote-url-list*' for available SERVERs.
If DEBUG is non-nil, do not bind `*print-log*' and `*gq-error-stream*'."
  (let ((*print-log* (if debug *print-log* #()))
        (*gq-error-stream* (if debug *gq-error-stream* nil)))
    (setf (values *holdings* *history*)
          (if hist-file (read-data-file hist-file)
              (mapcar (lambda (ti) (make-pfl :tick ti)) tickers)))
    (if (eq plot t) (setq plot :plot)
      (multiple-value-bind (srv res hhh yea)
          (apply #'get-quotes server
                 (if hist-file (mapcar #'pfl-tick *holdings*) tickers))
        (let* ((out (if (null log) *standard-output*
                        (open log :direction :output :if-exists
                              :append :if-does-not-exist :create)))
               (fixed (and *history* (gq-fix-hist *holdings* *history* hhh)))
               (new (if *history*
                        (process-results *holdings* *history* srv res yea out)
                        (show-results tickers srv res out))))
          (when *history*
            (top-bottom-fl *history* :val #'hist-totl :label #'hist-date
                           :out out))
          (when (and *history* (or fixed new))
            (save-data hist-file *holdings* *history*)
            (unless plotp (setq plot :plot)))
          (when log (close out) (format t "Wrote log to ~s~%" log))))))
  (when plot (plot-portfolio *holdings* *history* plot)))

(provide :cllib-gq)
;;; }}} gq.lisp ends here
