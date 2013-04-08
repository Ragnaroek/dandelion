;;; Data Analysis and Visualization
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: data.lisp,v 1.33 2006/12/07 01:24:57 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/data.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mdl'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `with-csv'
  (require :cllib-csv (translate-logical-pathname "cllib:csv"))
  ;; `fill-buckets', `bucketize'
  (require :cllib-lift (translate-logical-pathname "cllib:lift"))
  ;; `plot-histogram'
  (require :cllib-gnuplot (translate-logical-pathname "cllib:gnuplot")))

(in-package :cllib)

(export '(*buckets* *columns* *levels* *value-boundary* stat-column
          sc-table sc-pos sc-name sc-mdl sc-buckets sc-median sc-levels
          show-sc show-sc-list aref-i ensure-buckets ensure-levels
          table table-path table-lines table-stats table-names write-table
          table-accessor table-column-pos table-lines$ compress-tables *tables*
          table-stat-column table-p ensure-table-stat-column column-histogram
          analyse-csv table-stats-refresh add-column column-name-sc
          plot-columns table-to-hash summarize table-select purge-columns
          evaluate-predictor))

(defcustom *buckets* (or null (cons lift:bucket)) ()
  "The list of buckets to fill in `analyse-csv'.")
(defcustom *columns* (or (eql t) (cons (or fixnum string (eql NOT)))) t
  "The list of column specs to study in `analyse-csv'.")
(defcustom *levels* list
  '(0.1 0.2 0.25 0.3 0.4 0.5 0.6 0.7 0.75 0.8 0.9)
  "The percentile levels for order statistics.")

(defun column-spec-list (col-specs names ncol)
  "Return the list of positions corresponding to the column specifiers."
  (delete
   nil (mapcar (lambda (spec)
                 (etypecase spec
                   (string
                    (assert names (names)
                            "column spec ~S requires a names line" spec)
                    (or (position spec names :test #'string-equal)
                        (cerror "drop it" "no ~S in ~S" spec names)))
                   (fixnum
                    (if (< -1 spec ncol) spec
                        (cerror "drop it" "~S is out of range [0:~D]"
                                spec (1- ncol))))))
               col-specs)))

(defun columns-complement (ncol cols)
  "Return columns in 1..NCOL not in COLS."
  (sort (nset-difference (loop :for i :from 0 :below ncol :collect i) cols)
        #'<))

(defun unroll-column-specs (col-specs names ncol)
  "Turn `columns' into a list of `interesting' indexes."
  (etypecase col-specs
    (null (error "empty column selection"))
    ((eql t) (loop :for i :from 0 :below ncol :collect i))
    ((cons (eql not))           ; exclude specified columns
     (columns-complement ncol (column-spec-list (rest col-specs) names ncol)))
    (cons                       ; include specified columns
     (column-spec-list col-specs names ncol))))

(defcustom *value-boundary* (or null double-float) (/ double-float-epsilon)
  "The upper boundary for the data values.")

(defun numeric (v i &optional names
                &aux (*read-default-float-format* 'double-float))
  (let ((n (read-from-string (aref v i))))
    (if (numberp n)
        (if (< (abs n) *value-boundary*) n
            (cerror "drop the whole line"
                    "extreme value ~S in ~S at ~:D~@[ (~A)~]"
                    n v i (and names (aref names i))))
        (cerror "drop the whole line" "non-number ~S in ~S at ~:D~@[ (~A)~]"
                n v i (and names (aref names i))))))

(defvar *min-name-length* 5)
(defun max-name-length (names)
  (reduce #'max names :key #'length :initial-value *min-name-length*))
(defun column-name (names col)
  (if names (aref names col) (format nil "C~D" col)))

(defun strings-to-nums (lines col-specs &key names (len (length lines))
                        (max-name-length (max-name-length names))
                        (out *standard-output*)
                        ((:value-boundary *value-boundary*) *value-boundary*))
  "Convert some strings to numbers, in place."
  (let ((dnum (make-array (1+ (reduce #'max col-specs)) :initial-element 0))
        (drop 0) (total (length lines)))
    (with-timing (:out out :count line :units "records"
                  :progress *csv-progress* :progress-1 *csv-progress-1*)
      (mesg :log out "Converting strings to numbers...")
      (setq lines
            (delete-if-not
             (lambda (v)
               (incf line) (progress (/ line total))
               (dolist (i col-specs t)
                 (handler-bind ((error (lambda (c)
                                         (warn "~A -- line ~:D dropped" c line)
                                         (incf (aref dnum i))
                                         (incf drop) (return nil))))
                   (setf (aref v i) (numeric v i names)))))
             lines))
      (if (zerop drop) (mesg :log out "done")
          (loop :for i :in col-specs :for d = (aref dnum i) :unless (zerop d)
            :do (mesg :log out "~%~3D ~V@A:  ~:D lines dropped"
                      i max-name-length (column-name names i) d)
            :finally (mesg :log out "~%...dropped ~:D lines (out of ~:D, ~4F%)"
                           drop len (/ (* 1d2 drop) len))))
      (values lines (- len drop)))))

(defstruct (stat-column (:conc-name sc-))
  (table nil :type (or nil table))
  (pos 0 :type index-t)
  (name "" :type string)
  (mdl +bad-mdl+ :type mdl)
  (buckets nil :type list)      ; of buckets
  (median nil :type (or null real))
  (levels nil :type list))      ; of (level value)
(defmethod print-object ((sc stat-column) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (sc out :type t)
        (format out "~W ~:D~@[ ~W~]" (sc-name sc) (sc-pos sc)
                (let ((tab (sc-table sc))) (and tab (table-path tab)))))))
(defun show-sc (sc &key (width 0) (out *standard-output*))
  "Print a STAT-COLUMN nicely, on its own line or to string."
  (format out "~:[~;~&;; ~]~3D ~V@A ~A~@[  ~F~]~:{ [~F:~A]~}~:[~;~%~]"
          out (sc-pos sc) width (sc-name sc) (sc-mdl sc) (sc-median sc)
          (sc-levels sc) out))
(defun show-sc-list (sc-list &key (out *standard-output*))
  "Print a list of STAT-COLUMNs an as aligned table."
  (let ((width (reduce #'max sc-list :key (port:compose length sc-name)
                       :initial-value *min-name-length*)))
    (dolist (r sc-list) (show-sc r :width width :out out))))
(defun aref-i (i) (lambda (v) (aref v i)))
(defun stat-column (lines col names &key (out *standard-output*)
                    ((:buckets *buckets*) *buckets*)
                    (max-name-length (max-name-length names)) table)
  (let* ((name (column-name names col)) (key (aref-i col))
         (mdl (standard-deviation-mdl lines :key key)))
    (mesg :log out "~3D ~V@A ~A~%" col max-name-length name mdl)
    (make-stat-column :pos col :name name :mdl mdl :table table
                      :buckets (lift:bucketize lines *buckets*
                                               :key key :out out))))
(defun ensure-buckets (sc &key (out *standard-output*)
                       ((:buckets *buckets*) *buckets*))
  "When buckets are not present in a STAT-COLUMN, add it."
  (unless (sc-buckets sc) ; no buckets - create them!
    (show-sc sc :out out)
    (setf (sc-buckets sc) (lift:bucketize (table-lines (sc-table sc)) *buckets*
                                          :out out :key (aref-i (sc-pos sc))))))
(defun ensure-levels (sc &key (out *standard-output*)
                      ((:levels *levels*) *levels*)
                      &aux (pos (sc-pos sc))
                      (table (or (sc-table sc)
                                 (error "~S: ~S does not belong to a table"
                                        'ensure-levels sc)))
                      (max-name-length (max-name-length (table-names table))))
  (with-timing (:out out)
    (format t "~3D ~V@A" pos max-name-length (sc-name sc)) (force-output)
    (let* ((lines (setf (table-lines table)
                        (sort (table-lines table) #'< :key (aref-i pos))))
           (len (length lines)))
      (setf (sc-median sc) (aref (nth (ash len -1) lines) pos)
            (sc-levels sc)
            (mapcar (lambda (level)
                      (let ((l (etypecase level
                                 (cons (car level))
                                 ((real (0) (1)) level))))
                        (list l (aref (nth (round (* len l)) lines) pos))))
                    *levels*))
      (format out " ~F~:{ [~F:~A]~}" (sc-median sc) (sc-levels sc)))))

(defstruct table
  (path "" :type (or string pathname)) ; file containing the table
  (lines () :type (or list integer)) ; lines or line count
  (stats () :type list)              ; of stat-column
  (names #() :type vector))          ; of column names
(defun table-lines$ (table)
  "Return the number of lines in the TABLE."
  (let ((lines (table-lines table)))
    (etypecase lines
      (integer lines)
      (list (length lines)))))
(defun table-column-pos (name table)
  "Return the position of the column NAME in the TABLE."
  (or (position name (table-names table) :test #'string=)
      (error "~S: no ~S in ~S" 'table-column-pos name table)))
(defun table-accessor (name table)
  (aref-i (table-column-pos name table)))

(defun write-table (tab out)
  (format out "~W ~:Dx~:D" (table-path tab) (table-lines$ tab)
          (length (table-names tab))))
(defmethod print-object ((tab table) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (tab out :type t) (write-table tab out))))
(defvar *tables* nil "The list of currently loaded tables")
(defun compress-tables ()
  (dolist (tab *tables* (port:gc))
    (etypecase (table-lines tab)
      (integer (mesg :log t "~&no lines in <~S> - was ~:D~%"
                     (table-path tab) (table-lines tab)))
      (list (let ((len (length (table-lines tab))))
              (mesg :log t "~&removed ~:D lines from <~S>~%"
                    len (table-path tab))
              (setf (table-lines tab) len))))))

(defun table-stat-column (pos-or-name table &key (out *standard-output*)
                          (buckets
                           (let ((stats (table-stats table)))
                             (if stats (sc-buckets (car stats)) *buckets*)))
                          (max-name-length
                           (max-name-length (table-names table))))
  "Return a freshly computed STAT-COLUMN."
  (stat-column (table-lines table)
               (etypecase pos-or-name
                 (integer pos-or-name)
                 (string (table-column-pos pos-or-name table)))
               (table-names table) :buckets buckets
               :out out :max-name-length max-name-length :table table))
(defun ensure-table-stat-column (name table)
  "Make shure that there is a STAT-COLUMN for NAME in TABLE and return it."
  (or (find name (table-stats table) :test #'string= :key #'sc-name)
      (car (push (table-stat-column name table) (table-stats table)))))
(defun maybe-ensure-table-stat-column (pos name table)
  "Ensure the presence of STAT-COLUMN is the column is numeric."
  (and (numberp (aref (first (table-lines table)) pos))
       (ensure-table-stat-column name table)))

(defun column-histogram (sc nbins &rest plot-opts)
  (apply #'plot-histogram (table-lines (sc-table sc)) nbins
         :key (aref-i (sc-pos sc)) :mdl (sc-mdl sc)
         :xlabel (format nil "~A -- ~A(~D)" (table-path (sc-table sc))
                         (sc-name sc) (sc-pos sc))
         plot-opts))

;;;###autoload
(defun analyse-csv (file &key (first-line-names :default)
                    (out *standard-output*) medians
                    ((:value-boundary *value-boundary*) *value-boundary*)
                    ((:columns *columns*) *columns*)
                    ((:levels *levels*) *levels*)
                    ((:buckets *buckets*) *buckets*))
  "Analyse columns in the CSV file."
  (multiple-value-bind (lines len file-size names)
      (csv-read-file file :first-line-names first-line-names)
    (declare (ignore file-size))
    (let* ((columns (unroll-column-specs *columns* names
                                         (length (or names (car lines)))))
           (max-name-length
            (if names
                (reduce #'max columns :key (lambda (i) (length (aref names i)))
                        :initial-value *min-name-length*)
                *min-name-length*))
           (tab (make-table :path file :names names)))
      (assert columns (columns) "no interesting columns left")
      (setf (values (table-lines tab) len)
            (strings-to-nums lines columns :names names
                             :max-name-length max-name-length
                             :len len :out out))
      (push tab *tables*)
      (setf (table-stats tab)
            (mapcar (lambda (i)
                      (table-stat-column i tab :out out
                                         :max-name-length max-name-length))
                    columns))
      (when medians
        (with-timing (:out out)
          (format out "computing medians & levels...~%")
          (dolist (sc (table-stats tab)) (ensure-levels sc :out out))
          (format out "~:D sort~:P" (length (table-stats tab)))))
      tab)))

(defun table-stats-refresh (table)
  "Update TABLE-STATS."
  (with-timing ()
    (let ((stats (table-stats table)))
      (map-into stats (lambda (sc)
                        (let ((nsc (table-stat-column (sc-pos sc) table)))
                          (when (sc-median sc)
                            (ensure-levels nsc :levels (sc-levels sc)))
                          nsc))
                stats))))

(defun coerce-int-float (num)
  "Ensure that the argument is either an integer or a float."
  (typecase num
    (integer num)
    (float num)
    (t (float num 0d0))))

(defun add-column (table1 function name)
  "Add a new column named NAME to the table.
The value is computed by calling FUNCTION on the existing row vector.
Everything is allocated anew."
  (let* ((table2 (copy-table table1))
         (names (table-names table2)) (len (length names))
         (stats (table-stats table2)))
    (setf (table-names table2) (concatenate 'vector names (vector name))
          (table-lines table2)
          (mapcar (lambda (v)
                    (concatenate 'vector v (vector (coerce-int-float
                                                    (funcall function v)))))
                  (table-lines table2))
          (table-stats table2)
          (nconc (mapcar (lambda (sc)
                           (let ((cs (copy-stat-column sc)))
                             (setf (sc-table cs) table2)
                             cs))
                         stats)
                 (list (stat-column
                        (table-lines table2) len
                        (table-names table2) :table table2
                        :buckets (and stats (sc-buckets (car stats)))))))
    (push table2 *tables*)
    table2))

(defun column-name-sc (obj table)
  "Return two values: column position and column name in table."
  (etypecase obj
    (integer
     (let ((name (aref (table-names table) obj)))
       (values obj name (maybe-ensure-table-stat-column obj name table))))
    (string
     (let ((pos (position obj (table-names table) :test #'string=)))
       (values pos obj (maybe-ensure-table-stat-column pos obj table))))
    (stat-column
     (unless (eq table (sc-table obj))
       (cerror "ignore and proceed" "~S(~S): ~S /= ~S"
               'column-name-sc obj (sc-table obj) table)
       (let ((pos (column-name-sc (sc-name obj) table)))
         (unless (= (sc-pos obj) pos)
           (cerror "ignore and proceed" "~S(~S): ~D /= ~D"
                   'column-name-sc obj (sc-pos obj) pos)))
       (let ((name (aref (table-names table) (sc-pos obj))))
         (unless (string= (sc-name obj) name)
           (cerror "ignore and proceed" "~S(~S): ~S /= ~S"
                   'column-name-sc obj (sc-name obj) name))))
     (values (sc-pos obj) (sc-name obj) obj))))

(defun table-to-hash (table column &key (out *standard-output*))
  "TABLE --> HASH-TABLE column->(list record)"
  (let ((ht (make-hash-table :test 'equal))
        (pos (column-name-sc column table)))
    (with-timing (:out out)
      (mesg :log out "~S: ~A/~S..." 'table-to-hash table column)
      (dolist (v (table-lines table)) (push v (gethash (aref v pos) ht)))
      (mesg :log out "~:D entries" (hash-table-count ht)))
    ht))

(defun plot-columns (table col1 col2 &rest options
                     &key by (out *standard-output*) &allow-other-keys
                     &aux (*gnuplot-msg-stream* out))
  "Plot one column vs the other."
  (multiple-value-bind (c1 n1 s1) (column-name-sc col1 table)
    (multiple-value-bind (c2 n2 s2) (column-name-sc col2 table)
      (flet ((f (l) (mapcar (lambda (v) (cons (aref v c1) (aref v c2))) l)))
        (apply #'plot-lists-arg
               (if by
                   (let (l)
                     (maphash (lambda (val list)
                                (push (cons (princ-to-string val) (f list)) l))
                              (table-to-hash table by :out out))
                     (nreverse l))
                   (list (cons (format nil "~A vs ~A" n1 n2)
                               (f (table-lines table)))))
               (append (remove-plist options :by :out)
                       (let ((xmdl (sc-mdl s1)) (ymdl (sc-mdl s2)))
                         (list :xlabel n1 :ylabel n2
                               :xb (mdl-mi xmdl) :xe (mdl-ma xmdl)
                               :yb (mdl-mi ymdl) :ye (mdl-ma ymdl)
                               :data-style :points
                               :title (princ-to-string table)))))))))

(defun restat-table (old-table new-table &key (out *standard-output*)
                     (label 'restat-table))
  "Recompute all stats present in OLD-TABLE for NEW-TABLE."
  (with-timing (:out out :done t)
    (mesg :log out "~S: stats...~%" label)
    (dolist (sc (table-stats old-table))
      (let ((nsc (table-stat-column (sc-name sc) new-table :out out)))
        (push nsc (table-stats new-table))
        (ensure-levels nsc :out out)))
    (mesg :log out "~S: stats..." label)))

(defun summarize (table column &key (out *standard-output*))
  "Summarize TABLE by COLUMN returning a new table of means."
  (with-timing (:out out :done t)
    (mesg :log out "~S: summarizing ~S by ~S~%" 'summarize table column)
    (let* ((stats (table-stats table)) (ncol (1+ (length stats)))
           (ret (make-table :path (format nil "[~A]@~S"
                                          (write-table table nil) column)
                            :names
                            (apply #'vector
                                   (cons column (mapcar #'sc-name stats)))))
           (accessors (mapcar (port:compose aref-i sc-pos) stats))
           (ht (table-to-hash table column :out out)))
      (with-timing (:out out :done t)
        (mesg :log out "~S: filling return table lines..." 'summarize)
        (maphash (lambda (symbol list)
                   (let ((v (make-array ncol)))
                     (loop :for i :upfrom 1 :for acc :in accessors
                       :do (setf (aref v i) (coerce-int-float
                                             (mean list :key acc))))
                     (setf (aref v 0) symbol)
                     (push v (table-lines ret))))
                 ht))
      (restat-table table ret :out out :label 'summarize)
      (mesg :log out "~S: ~A..." 'summarize ret)
      (push ret *tables*)
      ret)))

(defun table-select (table &key column (value t) predicate
                     (title (and column (format nil "~S=~S" column value)))
                     (out *standard-output*))
  "Create a new table - a subset of the original."
  (with-timing (:out out :done t)
    (mesg :log out "~S(~A:~A)~%" 'table-select table title)
    (let ((ret (make-table
                :path (format nil "[~A]@~A" (write-table table nil) title)
                :names (if column
                           (remove column (table-names table) :test #'string=)
                           (copy-seq (table-names table)))
                :lines (if column
                           (loop :with pos = (column-name-sc column table)
                             :for v :in (table-lines table)
                             :when (string= (aref v pos) value)
                             :collect (remove-subseq v pos (1+ pos)))
                           (loop :for v :in (table-lines table)
                             :when (equal (funcall predicate v) value)
                             :collect v)))))
      (restat-table table ret :out out :label 'table-select)
      (mesg :log out "~S: ~A..." 'table-select ret)
      (push ret *tables*)
      ret)))

(defun list->intervals (list)
  ;; (10 9 8 5 4 3 1) --> ((10 . 8) (5 . 3) (1 . 1))
  (let ((beg (car list)) (end (car list)) ret)
    (dolist (curr (cdr list) (nreverse (cons (cons beg end) ret)))
      (if (= curr (1+ end))
          (setq end curr)
          (setq ret (cons (cons beg end) ret) end curr beg curr)))))

(defun extract-columns (vec columns)
  "Extract COLUMNS from VEC into a vector."
  (let ((ret (make-array (length columns))))
    (loop :for i :upfrom 0 :for c :in columns
      :do (setf (aref ret i) (aref vec c)))
    ret))

(defun purge-columns (table &key (test 'equal) (out *standard-output*))
  "Remove columns with identical values."
  (with-timing (:out out :done t)
    (mesg :log out "~S(~A): " 'purge-columns table)
    (let* ((names (table-names table)) (lines (table-lines table))
           (ncol (length names)) (delenda nil) preserva)
      (dotimes (i ncol)
        (let* ((tab (count-all lines :test test :key (aref-i i)))
               (count (hash-table-count tab)))
          (cond ((= 1 count)
                 (mesg :log out "<~A=~S>" (aref names i)
                       (aref (first lines) i))
                 (push i delenda))
                (t (mesg :log out "[~A:~:D]" (aref names i) count)))))
      (unless delenda
        (mesg :log out "~%~S: no delenda!" 'purge-columns)
        (return-from purge-columns nil))
      (setq delenda (nreverse delenda))
      (mesg :log out "~%~S: delenda: ~:D column~:P ~S ~S~%" 'purge-columns
            (length delenda) (list->intervals delenda)
            (mapcar (lambda (i) (aref names i)) delenda))
      (setq preserva (columns-complement ncol delenda))
      (mesg :log out "~S: preserva: ~:D column~:P ~S ~S~%" 'purge-columns
            (length preserva) (list->intervals preserva)
            (mapcar (lambda (i) (aref names i)) preserva))
      (setf (table-names table) (extract-columns names preserva)
            (table-lines table)
            (mapcar (lambda (v) (extract-columns v preserva)) lines))
      table)))

;;;###autoload
(defun evaluate-predictor (file &optional (out *standard-output*))
  "Evaluate the quality of the predictor the produced the file.
File: CSV, 1st column: actuals, 2nd column: predicted."
  (let ((data (with-collect (coll)
                (with-csv (vec file)
                  (coll (cons (numeric vec 0) (numeric vec 1)))))))
    (mesg :log out "actual: ~a~%" (standard-deviation-mdl data :key #'car))
    (mesg :log out "pred  : ~a~%" (standard-deviation-mdl data :key #'cdr))
    (multiple-value-bind (co m0 m1 d0 d1 n) (cov data)
      (let ((s0 (sqrt d0)) (s1 (sqrt d1)))
        (mesg :log out "actual: mean=~9f std=~9f n=~9:d~%pred  : mean=~9f std=~9f corr=~f~%"
              m0 s0 n m1 s1 (/ co (* s0 s1)))
        (let* ((deviation (mapcar (lambda (x) (- (car x) (cdr x))) data))
               (mdl (standard-deviation-mdl deviation)))
          (mesg :log out "diff  : ~a  r2=~f~%"
                mdl (/ (- s1 (mdl-sd mdl)) s1)))))))

(provide :cllib-data)
;;; file data.lisp ends here
