;;; read/write comma-separated values
;;;
;;; Copyright (C) 2003-2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: csv.lisp,v 2.20 2006/08/16 01:59:17 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/csv.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-collect'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `with-timing', `log'
  (require :cllib-log (translate-logical-pathname "cllib:log")))

(in-package :cllib)

(export '(csv-print-vector csv-parse-string csv-read-file with-csv csv-names
          *csv-separator* *csv-whitespace* *csv-progress* *csv-progress-1*))

(defcustom *csv-separator* character #\,
  "The separator in the CSV file, normally the comma.")

(defun csv-print-vector (vec &optional (out *standard-output*))
  "Print a vector as a comma-separated line."
  (declare (type vector vec) (stream out))
  (loop :with len = (length vec) :for val :across vec :and ii :from 1
        :when val :do (write val :stream out :escape nil)
        :unless (= ii len) :do (write-char *csv-separator* out))
  (terpri out))

(defcustom *csv-whitespace* (or null string) +whitespace+
  "The string of characters to trim from the values.")
(defcustom *csv-progress* integer 1000
  "*How often the progress report should be made")
(defcustom *csv-progress-1* integer 10
  "*How often the secondary progress report should be made")

(defun csv-trim (whitespace string)
  "Trim the string argument from the whitespace."
  (let ((clean (string-trim whitespace string)))
    (if (zerop (length clean)) nil clean)))

(defun csv-parse-string (string &key
                         ((:separator *csv-separator*) *csv-separator*)
                         ((:whitespace *csv-whitespace*) *csv-whitespace*))
  "Parse a string, returning a vector of strings."
  (loop :with num = (count *csv-separator* string :test #'char=)
    :with res = (make-array (1+ num))
    :for ii :from 0 :to num
    :for beg = 0 :then (1+ end)
    :for end = (or (position *csv-separator* string :test #'char= :start beg)
                   (length string))
    :do (setf (aref res ii)
              (when (> end beg) ; otherwise NIL = missing
                (csv-trim *csv-whitespace* (subseq string beg end))))
    :finally (return res)))

(defconst +comments+ string "#;" "Characters that start comments.")
(defun uncomment-line (line)
  "Remove the comment prefix from the string."
  (if (find (char line 0) +comments+)
      (string-left-trim +whitespace+ (string-left-trim +comments+ line))
      line))

;;;###autoload
(defun csv-names (file)
  "Read and parse as names the first line in the file."
  (csv-parse-string (uncomment-line (with-open-file (s file) (read-line s)))))

(defun csv-check-vec-len (vec cols fn pos)
  (unless (= cols (length vec))
    (error "~S:~:D: Wrong column count: ~:D instead of ~:D: ~S"
           fn pos (length vec) cols vec)))

(defmacro with-csv ((vec file &key (progress '*csv-progress*)
                         (first-line-names :default) junk-allowed
                         (progress-1 '*csv-progress-1*) limit
                         (out '*standard-output*) columns)
                    &body body)
  "Open FILE and set VEC to successive vectors in it.
Return 3 values:
  number of records (lines) read,
  number of bytes in the file,
  fraction of bytes read
  vector of column names if FIRST-LINE-NAMES is non-NIL
    or if it is :DEFAULT and the first line starts with a +COMMENTS+ character."
  (with-gensyms ("WITH-CSV-" in fn fsize ln len cols lim l1 fln pro)
    `(with-timing (:out ,out :count ,len :units "records" :progress ,progress
                   :progress-1 ,progress-1)
       (let* ((,fn ,file) (,pro ,progress) ,fsize ,l1
              (,fln ,first-line-names) (,cols ,columns)
              ,@(when limit `((,lim ,limit))))
         (with-open-file (,in ,fn :direction :input)
           (format ,out "~&Reading `~a' [~:d bytes]..."
                   ,fn (setq ,fsize (file-length ,in)))
           (force-output ,out)
           (when (eq ,fln :default)
             (setq ,fln (find (peek-char nil ,in) +comments+)))
           (when ,fln
             (let ((line1 (read-line ,in)))
               (cond ((zerop (length line1))
                      (cerror "ignore, return NIL for names"
                              "empty first line, names expected"))
                     (t (setq ,l1 (csv-parse-string (uncomment-line line1)))
                        (if ,cols (csv-check-vec-len ,l1 ,cols ,fn 0)
                            (setq ,cols (length ,l1)))))))
           (loop :with ,vec :for ,ln = (read-line ,in nil nil) :while ,ln
             ,@(when limit
                 `(:when (and ,lim (= ,len ,lim))
                    :do (warn "reached the limit of ~:D record~:P ~
                               at ~:D byte~:P (~4F%), aborted~%"
                              ,len (file-position ,in)
                              (/ (file-position ,in) ,fsize 1d-2))
                        (loop-finish)))
             :unless (and ,junk-allowed
                          (zerop (length (setq ,ln (string-trim
                                                    *csv-whitespace* ,ln)))))
             :do (setq ,vec (csv-parse-string ,ln)) (incf ,len)
             (if ,cols
                 (csv-check-vec-len ,vec ,cols ,fn ,len)
                 (setq ,cols (length ,vec)))
             ,@body
             (progress (/ (file-position ,in) ,fsize)
                       ;; print <*...*> when we expect to reach limit
                       (if ,(when limit `(and ,lim (> ,len (* ,lim pos))))
                           "*" ""))
             :end
             :finally (format ,out "done [~:d record~:p~@[, ~:d column~:p~]]"
                              ,len ,cols)
             :finally (return
                        (values ,len (file-length ,in)
                                (if (zerop ,fsize) 1
                                    (/ (file-position ,in) ,fsize))
                                ,l1))))))))

;;;###autoload
(defun csv-read-file (inf &key (first-line-names :default))
  "Read comma-separated values into a list of vectors."
  (let (len file-size complete names)
    (values (with-collect (coll)
              (setf (values len file-size complete names)
                    (with-csv (vec inf :first-line-names first-line-names)
                      (coll vec))))
            len file-size names)))

(provide :cllib-csv)
;;; file csv.lisp ends here
