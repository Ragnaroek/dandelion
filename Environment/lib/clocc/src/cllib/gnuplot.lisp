;;; Gnuplot (http://www.gnuplot.info/) interface
;;;
;;; Copyright (C) 1997-2006 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: gnuplot.lisp,v 3.37 2006/12/20 01:39:39 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gnuplot.lisp,v $

;;; the main entry point is WITH-PLOT-STREAM
;;; (see also other exported functions)

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `date2time', `+day-sec+', `days-between', `date>'
  (require :cllib-date (translate-logical-pathname "cllib:date"))
  ;; `dl-nth-date', `dated-list-name', `dl-nth-slot', `dl-shift',
  ;; `copy-dated-list', `dl-endp', `dl-len', `dl-ll', `dl-date'
  (require :cllib-datedl (translate-logical-pathname "cllib:datedl"))
  ;; `regress', `make-line', `line-sl', `line-co'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `regress-poly', `histogram'
  (require :cllib-stat (translate-logical-pathname "cllib:stat"))
  ;; `pipe-output', `close-pipe', `run-prog'
  (require :port-shell (translate-logical-pathname "port:shell")))

(in-package :cllib)

(export '(*gnuplot-path* *gnuplot-printer* *gnuplot-default-directive*
          *gnuplot-file* *gnuplot-dribble*
          plot-output *plot-term-screen* *plot-term-printer* *plot-term-file*
          plot-term make-plot-term plot-histogram
          +plot-timestamp+ directive-term make-plot-stream
          with-plot-stream plot-dated-lists plot-dated-lists-depth
          plot-lists plot-lists-arg plot-error-bars plot-functions))

;;;
;;; variables
;;;

(defcustom *plot-default-backend* symbol :gnuplot
  "The default plot backend designator.")
(defcustom *gnuplot-path* simple-string
  #+(or win32 mswindows) "d:/gnu/gp400w32/bin/pgnuplot.exe"
  #+unix "gnuplot"
  "*The path to the graphics-capable gnuplot executable.
This must be either a full path or a name of an executable in your PATH.")
(defconst +gnuplot-epoch+ integer (encode-universal-time 0 0 0 1 1 2000 0)
  "*The gnuplot epoch - 2000-01-01.")
(eval-when (compile load eval)  ; CMUCL
(defcustom *gnuplot-printer* simple-string
  (format nil
          #+(or win32 mswindows) "//SEATTLE/4th Floor NE"
          #+unix "|lpr -h~@[ -P~a~]"
          (getenv "SDSPRT"))
  "*The printer to print the plots."))
(defcustom *gnuplot-stream* (or null stream) nil
  "The current gnuplot output stream.")
(eval-when (compile load eval)  ; CMUCL
(defcustom *gnuplot-file* pathname (merge-pathnames "plot.tmp" *datadir*)
  "*The tmp file for gnuplot."))
(defcustom *gnuplot-msg-stream* (or stream null) *standard-output*
  "*The message stream of gnuplot functions.")
(defcustom *gnuplot-default-directive* t :plot
  "*The default action for `with-plot-stream'.")
(defcustom *gnuplot-dribble* (or null stream) nil
  "*The output stream where the gnuplot commands are output
in addition to *GNUPLOT-STREAM* or NIL for no dribbling.")

(declaim (ftype (function (date) (values integer)) plot-sec-to-epoch))
(defsubst plot-sec-to-epoch (dt)
  "Return the number of seconds from date DT to `+gnuplot-epoch+'."
  (declare (type date dt))
  (- (date2time dt) +gnuplot-epoch+))

(defmacro with-plot-stream ((str &rest options) &body body)
  "Execute body, with STR bound to the gnuplot stream.
Usage: (with-plot-stream (stream :plot PLOT &rest OPTIONS) body).
OPTIONS are gnuplot(1) options, the following are accepted:
 XLABEL YLABEL TIMEFMT XDATA DATA-STYLE TITLE XB XE GRID TERM
 BORDER LEGEND MULTIPLOT (key in gnuplot)
PLOT means:
  :plot    => plot;
  :print   => print;
  :wait    => plot and wait for gnuplot to terminate;
  :file    => write `*gnuplot-file*' and print a message;
  :return  => return a PLOT-SPEC object for future use;
  pathname designator => write this file and print a message;
  stream   => write gnuplot commands to this stream;
  NIL      => do nothing, print nothing, return NIL.
When MULTIPLOT is non-NIL, it should be a cons (NROW . NCOL) and
 BODY should return a list of PLOT-SPEC objects, e.g.:
\(with-plot-stream (out :multiplot '(1 . 2))
  (list (plot-functions (list (cons \"cosine\" #'cos)) 0 pi 100 :plot :return)
        (plot-functions (list (cons \"sine\" #'sin)) 0 pi 100 :plot :return)))"
  (with-gensyms ("WPS-" body-function)
    `(flet ((,body-function (,str) ,@body))
      ;; this cannot be replaced with a simple inline funcall since
      ;; OPTIONS are not necessarily known at compile time
      ,(if (oddp (length options))
           `(apply #'internal-with-plot-stream #',body-function ,@options)
           `(internal-with-plot-stream #',body-function ,@options)))))

(defgeneric plot-output (plot out backend)
  (:documentation "Ourput a plot-related object to the plot stream
according to the given backend")
  (:method ((plot t) (out stream) (backend t))
    (declare (ignorable plot))
    (unless (and (output-stream-p out) (open-stream-p out))
      (error 'code :proc 'plot-output :args (list out)
             :mesg "2nd argument must be an open output stream, got: ~s"))
    (error 'code :proc 'plot-output :args (list plot backend)
           :mesg "unknown plot=~s backend=~s")))

;; this specifies the plot output terminal
(defstruct (plot-term (:conc-name pltm-))
  (terminal nil :type (or null string))
  (terminal-options nil :type list)
  (target nil :type (or null string pathname symbol)))

(defmethod plot-output ((pt plot-term) (out stream) (backend (eql :gnuplot)))
  (format out "set terminal ~a~{ ~a~}~%"
          (pltm-terminal pt) (pltm-terminal-options pt))
  (let ((target (pltm-target pt)))
    (etypecase target
      (null (format out "set output~%"))
      (symbol (format out "set output '~a'~%" (symbol-value target)))
      ((or string pathname)
       (format out "set output '~a'~%"
               (make-pathname :type (if (string= "postscript"
                                                 (pltm-terminal pt))
                                        "ps" (pltm-terminal pt))
                              :defaults target))))))

(defun ps-terminal (target)
  (make-plot-term :target target :terminal "postscript"
                  :terminal-options '("landscape" "'Helvetica'" 7)))

(defcustom *plot-term-screen* plot-term
  (make-plot-term :terminal #+unix "x11" #+(or win32 mswindows) "windows"
                  :terminal-options '("font 'Helvetica,6"))
  "The `plot-term' object sending the plot to the screen.")
(defcustom *plot-term-printer* plot-term (ps-terminal '*gnuplot-printer*)
  "The `plot-term' object sending the plot to the printer.")
(defcustom *plot-term-file* plot-term (ps-terminal *gnuplot-file*)
  "The `plot-term' object sending the plot to the printer.")

(defgeneric directive-term (directive)
  (:documentation "Return the PLOT-TERM object appropriate for this directive")
  (:method ((directive t))
    (error 'code :proc 'directive-term :args (list directive)
           :mesg "unknown directive: ~s"))
  (:method ((directive (eql :plot))) *plot-term-screen*)
  (:method ((directive (eql :wait))) *plot-term-screen*)
  (:method ((directive (eql :print))) *plot-term-printer*)
  (:method ((directive (eql :file))) *plot-term-file*)
  (:method ((directive (eql :return))) nil)
  (:method ((directive string)) (ps-terminal directive))
  (:method ((directive pathname)) (ps-terminal directive)))

(defstruct (plot-label (:conc-name pll-))
  (text "" :type string)
  (rot nil :type boolean)
  (front t :type boolean)
  (pos '(0 . 0) :type cons)
  (font "Helvetica" :type string))

(defmethod plot-output ((pl plot-label) (out stream)
                        (backend (eql :gnuplot)))
  (format out "set label ~s at screen ~f, screen ~f ~arotate ~a font '~a'~%"
          (pll-text pl) (car (pll-pos pl)) (cdr (pll-pos pl))
          (if (pll-rot pl) "" "no") (if (pll-front pl) "front" "back")
          (pll-font pl)))

(defstruct (plot-timestamp (:conc-name plts-))
  (fmt "%Y-%m-%d %a %H:%M:%S %Z" :type string)
  (loc :bottom :type (member :top :bottom))
  (rot nil :type boolean)
  (pos '(0 . 0) :type cons)
  (font "Helvetica" :type string))

(defmethod plot-output ((pt plot-timestamp) (out stream)
                        (backend (eql :gnuplot)))
  (format out "set timestamp ~s ~(~a~) ~arotate ~d,~d '~a'~%"
          (plts-fmt pt) (plts-loc pt) (if (plts-rot pt) "" "no")
          (car (plts-pos pt)) (cdr (plts-pos pt)) (plts-font pt)))

(defconst +plot-timestamp+ plot-timestamp (make-plot-timestamp)
  "The standard timestamp.")

(defstruct (plot-axis (:conc-name plax-))
  (name "" :type string)        ; gnuplot name: [xyz]{2}
  (label "" :type string)       ; label: "value", "time"...
  (tics t :type boolean)
  (fmt "%g" :type string)
  (time-p nil :type boolean)
  (logscale nil :type (or null (eql t) (real (1))))
  (range '(* . *) :type cons))

(defstruct coordinate
  (system :first :type symbol)
  (pos (port:required-argument) :type real))
(defstruct point
  (x (port:required-argument) :type coordinate)
  (y (port:required-argument) :type coordinate))
(defstruct arrow
  (beg (port:required-argument) :type point)
  (end (port:required-argument) :type point)
  (head 0 :type (integer 0 2))
  (width 1 :type (integer 1 10)))

(defstruct (plot-spec (:conc-name plsp-))
  (term *plot-term-screen* :type (or plot-term null)) ; nil for return directive
  (timestamp +plot-timestamp+ :type (or null plot-timestamp))
  (x-axis (make-plot-axis :name "x") :type plot-axis)
  (y-axis (make-plot-axis :name "y") :type plot-axis)
  (data-style :lines :type symbol)
  (border t :type boolean)
  (timefmt nil :type (or null string))
  (title "" :type string)
  (legend nil :type list)
  (grid nil :type boolean)
  (arrows nil :type list)
  (multiplot nil :type (or null (cons integer integer))) ; (nrow . ncol)
  (labels nil :type list)
  (data-fun #'error :type function))

(defmethod plot-output ((pt null) (out stream) (backend (eql :gnuplot)))
  (declare (ignorable pt out)))

(defmethod plot-output ((cr coordinate) (out stream) (backend (eql :gnuplot)))
  (write (coordinate-system cr)  :escape nil :case :downcase :stream out)
  (format out " ~F" (coordinate-pos cr)))
(defmethod plot-output ((pt point) (out stream) (backend (eql :gnuplot)))
  (plot-output (point-x pt) out backend)
  (write-char #\, out)
  (plot-output (point-y pt) out backend))
(defmethod plot-output ((arw arrow) (out stream) (backend (eql :gnuplot)))
  (write-string "set arrow from " out)
  (plot-output (arrow-beg arw) out backend)
  (write-string " to " out)
  (plot-output (arrow-end arw) out backend)
  (write-char #\Space out)
  (write-string (aref #("nohead" "head" "heads") (arrow-head arw)) out)
  (format out " lw ~D~%" (arrow-width arw)))

(defun %plotout (xx)
  (typecase xx
    (number (format nil "~g" xx))
    (symbol (string-downcase (symbol-name xx)))
    ((cons real real) (format nil "~f,~f" (car xx) (cdr xx)))
    (list (format nil "~{ ~(~a~)~}" xx))
    (t (concatenate 'string "\""
                    ;; quote #\":
                    (substitute-subseq (princ-to-string xx) "\"" "\\\"")
                    "\""))))

(defmethod plot-output ((pa plot-axis) (out stream) (backend (eql :gnuplot)))
  (let ((name (plax-name pa)))
    (format out "set format ~a ~s~%" name (plax-fmt pa))
    (format out "set ~alabel ~s~%" name (or (plax-label pa) ""))
    (format out "set ~:[no~;~]~atics~%" (plax-tics pa) name)
    (if (plax-time-p pa)
        (format out "set timefmt ~s~%set ~adata time~%" (plax-fmt pa) name)
        (format out "set ~adata~%" name))
    (let ((logscale (plax-logscale pa)))
      (if logscale
          (format out "set logscale ~a~@[ ~f~]~%" name
                  (unless (eq logscale t) logscale))
          (format out "unset logscale ~a~%" name)))
    (let ((range (plax-range pa)))
      (format out "set ~arange [~a:~a]~%" name
              (%plotout (car range)) (%plotout (cdr range))))))

(defun multiplot-row-col (mp data-fun)
  "Compute the number of rows and columns for a multiplot."
  (if mp
      (let ((ncol (car mp)) (nrow (cdr mp)) len
            (data (funcall data-fun t)))
        (unless (every #'plot-spec-p data)
          (error 'code :proc 'plot-output :args (list 'plot-spec data)
                 :mesg "multiplot requires a list of ~s as data, not ~s"))
        (setq len (length data))
        (etypecase ncol
          ((integer 1)
           (etypecase nrow
             ((integer 1)
              (when (< (* nrow ncol) len)
                (error 'code :proc 'plot-output :args (list nrow ncol len)
                       :mesg "not enough slots: ~dx~d<~d")))
             (null (setq nrow (ceiling len ncol)))))
          (null
           (etypecase nrow
             ((integer 1) (setq ncol (ceiling len nrow)))
             (null (setq ncol (isqrt len) nrow (ceiling len ncol))))))
        (values nrow ncol data))
      (values nil nil nil)))

(defmethod plot-output ((ps plot-spec) (out stream) (backend (eql :gnuplot)))
  (flet ((set-opt (nm par)
           (case par
             ((t) (format out "set ~a~%" nm))
             ((nil) (format out "unset ~a~%" nm))
             (t (format out "set ~a ~a~%" nm (%plotout par))))))
    (multiple-value-bind (nrow ncol data)
        (multiplot-row-col (plsp-multiplot ps) (plsp-data-fun ps))
      (plot-output (plsp-term ps) out backend)
      (let ((timestamp (plsp-timestamp ps)))
        (if timestamp
            (plot-output timestamp out backend)
            (set-opt "timestamp" nil)))
      (when (and nrow ncol) (set-opt "multiplot" t))
      (plot-output (plsp-x-axis ps) out backend)
      (plot-output (plsp-y-axis ps) out backend)
      (set-opt "border" (plsp-border ps))
      (set-opt "style data" (plsp-data-style ps))
      (set-opt "title" (plsp-title ps))
      (set-opt "key" (plsp-legend ps))
      (set-opt "grid" (plsp-grid ps))
      (if (and nrow ncol)
          (loop :initially (set-opt "size" (cons (/ nrow) (/ ncol)))
            :for plsp :in data :for pos :upfrom 0
            :for (row col) = (multiple-value-list (floor pos ncol))
            :do (set-opt "origin" (cons (/ row nrow) (/ col ncol)))
            (plot-output plsp out backend))
          (funcall (plsp-data-fun ps) out))
      (dolist (arrow (plsp-arrows ps))
        (plot-output arrow out backend))
      (dolist (label (plsp-labels ps))
        (plot-output label out backend))
      (when (and nrow ncol)
        (set-opt "multiplot" nil)
        (set-opt "size" '(1 . 1))
        (set-opt "origin" '(0 . 0)))
      (set-opt "arrow" nil)     ; reset arrows & labels
      (set-opt "label" nil))))

(defun make-plot (&key data-fun (plot *gnuplot-default-directive*)
                  (xlabel "x") (ylabel "y") arrows multiplot
                  (timestamp +plot-timestamp+) labels
                  (data-style :lines) (border t) timefmt
                  (xb '*) (xe '*) (yb '*) (ye '*) legend
                  (title (if multiplot "multiplot" "plot"))
                  (xtics t) (ytics t) grid xlogscale ylogscale
                  (xfmt (or timefmt "%g")) (yfmt "%g")
                  (ts (cond ((plot-timestamp-p timestamp) timestamp)
                            ((or (eq timestamp t) (eq timestamp :default))
                             +plot-timestamp+)
                            ((null timestamp) nil)
                            (t (error 'code :proc 'make-plot
                                      :args (list timestamp)
                                      :mesg "invalid timestamp: ~s")))))
  (make-plot-spec
   :data-fun data-fun :term (directive-term plot) :data-style data-style
   :x-axis (make-plot-axis :name "x" :label xlabel :tics xtics :fmt xfmt
                           :range (cons xb xe)
                           :logscale xlogscale :time-p (not (null timefmt)))
   :y-axis (make-plot-axis :name "y" :label ylabel :tics ytics :fmt yfmt
                           :range (cons yb ye)
                           :logscale ylogscale)
   :multiplot multiplot :timestamp (and (null multiplot) ts)
   :labels (if multiplot
               (delete nil
                       (list* (and ts (make-plot-label
                                       :text (port:current-time nil)
                                       :pos (plts-pos ts) :rot (plts-rot ts)
                                       :font (plts-font ts)))
                              (make-plot-label :text title :pos '(0 . 0.99))
                              labels))
               labels)
   :grid grid :legend legend :title title :border border :arrows arrows))

(defgeneric make-plot-stream (directive)
  (:documentation "Create the stream appropriate for the directive.")
  (:method ((directive t))
    (error 'code :proc 'make-plot-stream :args (list directive)
           :mesg "unknown plot directive: ~s"))
  (:method ((directive stream)) directive)
  (:method ((directive (eql :file))) (make-plot-stream *gnuplot-file*))
  (:method ((directive (eql :wait))) (make-plot-stream :plot))
  (:method ((directive string)) (open directive :direction :output))
  (:method ((directive pathname)) (open directive :direction :output))
  (:method ((directive (eql :print))) (make-plot-stream :plot))
  (:method ((directive (eql :return))) nil)
  (:method ((directive (eql :plot)))
    (unless (and *gnuplot-stream* (open-stream-p *gnuplot-stream*))
      (setq *gnuplot-stream* (pipe-output *gnuplot-path*)))
    *gnuplot-stream*))

(defun internal-with-plot-stream (body-function &rest opts
                                  &key (plot *gnuplot-default-directive*)
                                  (dribble *gnuplot-dribble*)
                                  (backend *plot-default-backend*)
                                  &allow-other-keys)
  "The gist of `with-plot-stream' is here.
Should not be called directly but only through `with-plot-stream'."
  (when (eq plot t)
    (mesg :plot *gnuplot-msg-stream*
          "~&~s: plot directive ~s is deprecated; use ~s~%"
          'internal-with-plot-stream t :plot)
    (setq plot :plot))
  (let* ((*print-pretty* nil)  ; ensure no unwanted newlines in commands
         (plot-out (make-plot-stream plot))
         (plot-stream (if (and plot-out dribble)
                          (make-broadcast-stream plot-out dribble)
                          plot-out))
         (plot-spec (apply #'make-plot :data-fun body-function :plot plot
                           (remove-plist opts :dribble :backend)))
         (plot-file (typecase plot-stream
                      (file-stream (namestring plot-stream)))))
    (declare (stream plot-stream))
    (when (or (stringp plot) (pathnamep plot)) (setq plot :file))
    (unwind-protect
         (case plot
           (:return plot-spec)
           (t (plot-output plot-spec plot-stream backend)))
      (when (streamp plot-stream) ; clean up
        (fresh-line plot-stream)
        (force-output plot-stream))
      (when (streamp plot)
        (mesg :plot *gnuplot-msg-stream*
              "~&wrote plot commands to ~s~%" plot))
      (ecase plot
        ((t :plot) (mesg :plot *gnuplot-msg-stream* "~&Done plotting.~%"))
        (:return nil)
        (:wait
         (fresh-line *terminal-io*)
         (princ "Press <enter> to continue..." *terminal-io*)
         (force-output *terminal-io*) (read-line *terminal-io* nil nil))
        (:print (mesg :plot *gnuplot-msg-stream*
                      "~&Sent the plot to `~a'.~%" *gnuplot-printer*)
                (format plot-stream "set output~%"))
        (:file
         (close plot-stream)
         (mesg :plot *gnuplot-msg-stream*
               "~&Wrote `~a'.~%Type \"load '~a'\" at the gnuplot prompt.~%"
               plot-file plot-file))))))

(defun plot-data-style (num-ls)
  "Decide upon the appropriate data style for the number of points."
  (when (listp num-ls)
    (setq num-ls (1- (reduce #'min num-ls :key #'length))))
  (assert (realp num-ls) (num-ls)
          "~s got neither number nor list: ~s" 'plot-data-style num-ls)
  (if (> num-ls 30) :lines :linespoints))

;;;###autoload
(defun plot-dated-lists (begd endd dls &rest opts &key (title "Dated Plot")
                         (xlabel "time") rel data-style
                         (ylabel (if rel "relative value" "value"))
                         (timefmt "%Y-%m-%d") ema (slot 'val)
                         &allow-other-keys)
  "Plot the dated lists from BEGD to ENDD.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
REL means plot everything relative to the first value.
EMA is the list of parameters for Exponential Moving Averages."
  (assert dls () "Nothing to plot for `~a'~%" title)
  (setq begd (if begd (date begd) (dl-nth-date (car dls)))
        endd (if endd (date endd) (dl-nth-date (car dls) -1)))
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel :title title
                     :data-style (or data-style (plot-data-style
                                                 (days-between begd endd)))
                     :timefmt timefmt :xb begd :xe endd
                     (remove-plist opts :ema :rel :slot))
    (format str "plot~{ '-' using 1:2 title ~s~^,~}"
            ;; Ugly.  But gnuplot requires a comma *between* plots,
            ;; and this is the easiest way to do that.
            (mapcan (lambda (dl)
                      (cons (dated-list-name dl)
                            (mapcar (lambda (ee)
                                      (format nil "~a - EMA [~a]"
                                              (dated-list-name dl) ee))
                                    ema)))
                    dls))
    (terpri str)                ; the command line is over!
    (let* ((emal (make-list (length ema))) bv
           (val (if rel (lambda (dl) (/ (dl-nth-slot dl slot) bv))
                    (lambda (dl) (dl-nth-slot dl slot)))))
      (dolist (dl dls)
        (setq bv (dl-nth-slot dl slot))
        (do* ((td (dl-shift (copy-dated-list dl) begd) (dl-shift td)))
             ((or (dl-endp td) (date> (dl-nth-date td) endd))
              (format str "e~%"))
          (mapl (lambda (ee cc)
                  (let ((vv (funcall val td)))
                    (push (cons (dl-nth-date td)
                                (+ (* (car cc) vv)
                                   (* (- 1 (car cc)) (or (cdaar ee) vv))))
                          (car ee))))
                emal ema)
          (format str "~a ~f~%" (dl-nth-date td) (funcall val td)))
        (dolist (em emal)
          (dolist (ee (nreverse em) (format str "e~%"))
            (format str "~a ~f~%" (car ee) (cdr ee))))
        ;; clean EMAL for the next pass
        (do ((ee emal (cdr ee))) ((null ee)) (setf (car ee) nil))))))

;;;###autoload
(defun plot-lists (lss &rest opts &key (key #'value) (title "List Plot") rel
                   (xlabel "nums") (ylabel (if rel "relative value" "value"))
                   (depth (1- (reduce #'min lss :key #'length)))
                   (data-style (plot-data-style depth)) &allow-other-keys)
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the numbers."
  (declare (list lss) (type fixnum depth))
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel :title title
                     :data-style data-style :xb 0 :xe (1- depth)
                     (remove-plist opts :depth :rel :key))
    (format str "plot~{ '-' using 1:2 title ~s~^,~}~%" (mapcar #'car lss))
    (let* (bv (val (if rel
                       (lambda (ll) (if ll (/ (funcall key (car ll)) bv) 1))
                       (lambda (ll) (if ll (funcall key (car ll)) bv)))))
      (dolist (ls lss)
        (setq bv (funcall key (cadr ls)))
        (do ((ll (cdr ls) (cdr ll)) (ix 0 (1+ ix)))
            ((= ix depth) (format str "e~%"))
          (declare (fixnum ix))
          (format str "~f~20t~f~%" ix (funcall val ll)))))))

;;;###autoload
(defun plot-lists-arg (lss &rest opts &key (key #'identity)
                       (title "Arg List Plot") (xlabel "nums") rel lines
                       (ylabel (if rel "relative value" "value"))
                       data-style quads xb xe &allow-other-keys)
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinates. KEY is used to extract the cons."
  (declare (list lss))
  (when (eq lines t)
    (setq lines (mapcar (lambda (ls)
                          (regress (cdr ls) :xkey (compose car 'key)
                                   :ykey (compose cdr 'key))) lss)))
  (when (eq quads t)
    (setq quads (mapcar (lambda (ls)
                          (regress-poly (cdr ls) 2 :xkey (compose car 'key)
                                        :ykey (compose cdr 'key))) lss)))
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel :title title
                     :data-style (or data-style (plot-data-style lss))
                     :xb (or xb (reduce #'min lss
                                        :key (compose car 'key cadr)))
                     :xe (or xe (reduce #'max lss
                                        :key (compose car 'key car last)))
                     (remove-plist opts :key :rel :lines :quads))
    (format str "plot~{ '-' using 1:2 title ~s~^,~}" (mapcar #'car lss))
    (dolist (ln lines) (plot-line-str ln xb xe str))
    (dolist (qu quads) (plot-quad-str qu xb xe str))
    (terpri str)
    (let* (bv (val (if rel (lambda (kk) (/ kk bv)) #'identity)))
      (dolist (ls lss)
        (setq bv (cdr (funcall key (cadr ls))))
        (do ((ll (cdr ls) (cdr ll)) kk)
            ((null ll) (format str "e~%"))
          (setq kk (funcall key (car ll)))
          (format str "~f~20t~f~%" (car kk) (funcall val (cdr kk))))))))

;;;###autoload
(defun plot-error-bars (ll &rest opts &key (title "Error Bar Plot")
                        (xlabel "nums") (ylabel "value")
                        (data-style (plot-data-style (list ll)))
                        (xkey #'first) (ykey #'second) (ydkey #'third)
                        &allow-other-keys)
  "Plot the list with errorbars.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
The first element is the title, all other are records from which we
get x, y and ydelta with xkey, ykey and ydkey."
  (declare (list ll))
  (with-plot-stream (str :xlabel xlabel :ylabel ylabel :title title
                     :data-style data-style :xb (funcall xkey (second ll))
                     :xe (funcall xkey (car (last ll)))
                     (remove-plist opts :xkey :ykey :ydkey))
    (format str "plot 0 title \"\", '-' title ~s with errorbars,~
 '-' title \"\", '-' title \"\", '-' title \"\"~%" (pop ll))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a ~a~%" (funcall xkey rr)
              (funcall ykey rr) (funcall ydkey rr)))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr) (funcall ykey rr)))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (- (funcall ykey rr) (funcall ydkey rr))))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (+ (funcall ykey rr) (funcall ydkey rr))))))

;;;###autoload
(defun plot-functions (fnl xmin xmax numpts &rest opts &key data-style
                       (title "Function Plot") &allow-other-keys)
  "Plot the functions from XMIN to XMAX with NUMPTS+1 points.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
FNL is a list of (name . function).
E.g.:
  (plot-functions (list (cons 'sine #'sin) (cons 'cosine #'cos)) 0 pi 100
                  :legend '(:bot :left :box) :grid t :plot :wait)"
  (declare (list fnl) (real xmin xmax) (type index-t numpts))
  (with-plot-stream (str :xb xmin :xe xmax :title title
                     :data-style (or data-style (plot-data-style numpts)) opts)
    (format str "plot~{ '-' using 1:2 title '~a'~^,~}~%" (mapcar #'car fnl))
    (dolist (fn fnl)
      (mesg :plot *gnuplot-msg-stream* "~&Plotting ~S..." (car fn))
      (dotimes (ii (1+ numpts) (format str "e~%"))
        (declare (type index-t ii))
        (let ((xx (dfloat (/ (+ (* ii xmax) (* (- numpts ii) xmin)) numpts))))
          (format str "~f~20t~f~%" xx (funcall (cdr fn) xx))))
      (mesg :plot *gnuplot-msg-stream* "done~%"))))

;;;###autoload
(defun plot-dated-lists-depth (depth dls slot &rest opts)
  "Plot the dated lists, DEPTH *days* from the beginning.
OPTS is passed to `plot-lists-arg'."
  (apply #'plot-lists-arg
         (mapcar
          (lambda (dl)
            (cons (prin1-to-string dl)
                  (dated-list-to-day-list dl :slot slot :depth depth)))
          dls)
         opts))

;;;###autoload
(defun plot-histogram (list nbins &rest opts &key (mean t) (key #'value)
                       (mdl (standard-deviation-mdl list :key key))
                       (min (mdl-mi mdl)) (max (mdl-ma mdl))
                       (title (princ-to-string mdl)) xlogscale
                       (xlabel "x") (ylabel "count") &allow-other-keys)
  "Plot the data in the list as a histogram.
When :MEAN is non-NIL (default), show mean and mean+-standard deviation
 with vertical lines."
  (multiple-value-bind (vec width)
      (histogram list nbins :key key :out *gnuplot-msg-stream* :mdl mdl
                 :logscale xlogscale :min min :max max)
    (let ((arrows
           (when mean
             (flet ((vertical (x thickness)
                      (make-arrow
                       :beg (make-point
                             :x (make-coordinate :pos x)
                             :y (make-coordinate :system :graph :pos 0))
                       :end (make-point
                             :x (make-coordinate :pos x)
                             :y (make-coordinate :system :graph :pos 1))
                       :width thickness)))
               (let ((arrows (list (vertical (mdl-mn mdl) 4))))
                 (let ((lo (- (mdl-mn mdl) (mdl-sd mdl))))
                   (when (>= lo min)
                     (push (vertical lo 2) arrows)))
                 (let ((hi (+ (mdl-mn mdl) (mdl-sd mdl))))
                   (when (<= hi max)
                     (push (vertical hi 2) arrows)))
                 arrows)))))
      (with-plot-stream (str :title title :data-style :histeps :arrows arrows
                             :xlabel xlabel :ylabel ylabel
                             (remove-plist opts :key :mean :mdl :min :max))
        (format str "plot '-' using 1:2~%")
        (loop :for height :across vec
          :for mid = (if xlogscale
                         (* min (sqrt width))
                         (+ min (/ width 2)))
          :then (if xlogscale (* mid width) (+ mid width))
          :do (format str "~F~20T~F~%" mid height))
        (format str "e~%")))))

(defun dated-list-to-day-list (dl &key (slot 'val) (depth (dl-len dl)))
  "Make a list of conses (days-from-beg . value) of length
DEPTH out of the dated list."
  (declare (type dated-list dl) (symbol slot) (fixnum depth))
  (do ((bd (dl-nth-date dl)) (ll (dl-ll dl) (cdr ll)) (ii 0 (1+ ii)) rr)
      ((or (null ll) (= ii depth)) (nreverse rr))
    (declare (fixnum ii) (type date bd) (list rr ll))
    (push (cons (days-between bd (funcall (dl-date dl) (car ll)))
                (slot-value (car ll) slot)) rr)))

(defun line-day2sec (ln begd)
  "Make a new line, converting from days to seconds."
  (declare (type line ln) (type integer begd))
  (make-line :sl (/ (line-sl ln) +day-sec+) :co
             (- (line-co ln) (* (line-sl ln) (/ begd +day-sec+)))))

(defun plot-line-str (ln beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the line from BEG to END.
This is not a complete plotting function (not a UI)!"
  (declare (type line ln) (real beg end) (stream str))
  (format str ", ((x>~a)?((x<~a)?(~a*x+~a):1/0):1/0) title ~s with lines~
~@[ ~d~]" beg end (line-sl ln) (line-co ln) title lt))

(defun plot-quad-str (qu beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the parabola
from BEG to END.  This is not a complete plotting function (not a UI)!"
  (declare (type (simple-array double-float (3)) qu) (real beg end))
  (format str ", ((x>~a)?((x<~a)?(~a*x*x+~a*x+~a):1/0):1/0) title ~s ~
with lines~@[ ~d~]" beg end (aref qu 0) (aref qu 1) (aref qu 2) title lt))

(provide :cllib-gnuplot)
;;; gnuplot.lisp ends here
