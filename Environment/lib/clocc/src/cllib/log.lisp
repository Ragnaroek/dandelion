;;; logging and progress reporting
;;;
;;; Copyright (C) 1997-2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: log.lisp,v 1.32 2006/09/20 01:04:22 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/log.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-type', `dfloat'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `linear'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `pr-secs'
  (require :cllib-tilsla (translate-logical-pathname "cllib:tilsla")))

(in-package :cllib)

(export '(get-int-time elapsed time-diff with-timing eta progress
          *print-log* mesg list-format fmt%))

;;;
;;;
;;;

(defun list-format (item-fmt)
  "Return the format string for list printing, printing the item as ITEM-FMT.
See CLtL2 p602 and <http://www.lisp.org/HyperSpec/Body/sec_22-3-7-2.html>"
  (format nil "~~#[ none~~; ~a~~; ~a and ~a~~:;~~@{~~#[~~; and~~] ~a~~^,~~}~~]"
          item-fmt item-fmt item-fmt item-fmt))

(defun fmt% (number)
  "Format the number as percent."
  (format nil "~5F%" (* 1d2 number)))

;;;
;;; {{{ progress reporting
;;;

(declaim (ftype (function (&optional t) (values (integer 0))) get-int-time))
(defun get-int-time (&optional (run t))
  "Return the run (or real) time counter, as a double float."
  (if run (get-internal-run-time) (get-internal-real-time)))

(defun time-diff (end beg)
  "Compute the time (in seconds) between the two given internal timestamps."
  (declare (type real end beg)) ; values of LINEAR in ETA are not integer
  (/ (- end beg)
     ;; CLISP compiled files are cross-platform,
     ;; so this value must be fixed at load time, not at read time
     #+clisp #,(dfloat internal-time-units-per-second)
     #-clisp #.(dfloat internal-time-units-per-second)))

(defun elapsed (bt run &optional fmt)
  "Return the time in seconds elapsed since BT,
previously set using `get-int-time'.
If FMT is non-NIL, return the corresponding string too."
  (declare (type (integer 0) bt))
  (let ((nn (time-diff (get-int-time run) bt)))
    (declare (double-float nn))
    (if fmt (values nn (format nil "~/pr-secs/" nn)) nn)))

;;;
;;; }}}{{{ logging
;;;

(defcustom *print-log* simple-vector
  '#(:log :logv :date :plot :head :res :opt :err :test :xml)
  "The list of message types which are being printed.")

(defun print-log-p (type)
  "Check whether this message TYPE is being logged."
  (or (eq type t) (find type *print-log* :test #'eq)))

(defmacro mesg (type str &rest args)
  "Call format -- conditionally.
This has to be a macro to avoid needless evaluating the args."
  (with-gensyms ("MESG-" out typ)
    `(let ((,out ,str) (,typ ,type) (*print-pretty* nil))
       (declare (type (or stream (member nil t)) ,out))
       (when (and ,out (print-log-p ,typ))
         (format ,out ,@args)
         (force-output (if (eq t ,out) *standard-output* ,out))))))

(defmacro with-timing ((&key (terpri t) (done nil) (run t) (real t)
                             (count nil) (units "bytes")
                             (progress nil) (progress-1 nil)
                             (type t) (out '*standard-output*))
                       &body body)
  "Evaluate the body, then print the timing.
:COUNT specifies a variable that will be bound inside WITH-TIMING,
 it should be incremented on every iteration and should count :UNITS.
Within the body you can call a local function ETA of one argument -
 the current relative position which returns 2 values:
 the expected remaining run and real times.
When :PROGRESS is not NIL, it should be a number indicating how often
 a dot is printed to indicate progress.
When :PROGRESS-1 is not NIL, it should be a number indicating after how
 many dots the ETA is printed.
:TYPE is the log type, see *PRINT-LOG*.
:DONE means print \"done\" when done.
:RUN and :REAL specify whether to print the eponymous time."
  (with-gensyms ("TIMING-" bt b1 %out el last-pos last-time last-tim1
                           pro pro1 pro1-count)
    `(let* ((,bt (get-int-time)) (,b1 (get-int-time nil)) ,el
            (,pro ,progress) (,pro1 ,progress-1) (,pro1-count 0)
            (,%out (and (print-log-p ,type) ,out))
            (,last-time ,bt) (,last-tim1 ,b1) (,last-pos 0)
            ,@(when count `((,count 0))))
       (unwind-protect
            (labels ((eta (pos) ; pos is the current relative position
                       (if (zerop pos) (values 0 0)
                         (let* ((now (get-int-time)) (no1 (get-int-time nil))
                                (lt ,last-time) (l1 ,last-tim1)
                                (eta (linear 0 ,bt pos now 1))
                                (et1 (linear 0 ,b1 pos no1 1)))
                           (setq ,last-time now ,last-tim1 no1)
                           (if (= ,last-pos pos)
                             (values (time-diff eta now) (time-diff et1 no1))
                             (let ((eta* (linear ,last-pos lt pos now 1))
                                   (et1* (linear ,last-pos l1 pos no1 1)))
                               (setq ,last-pos pos)
                               (values (time-diff (/ (+ eta eta*) 2) now)
                                       (time-diff (/ (+ et1 et1*) 2) no1)))))))
                     (show-eta (pos bad)
                       (format ,%out "<~A~@[~:D: ~]~4F% ETA: ~/pr-secs/~A>"
                               bad ,count (* pos 1d2) (eta pos) bad)
                       (force-output ,%out)))
              (declare (ignorable (function eta) (function show-eta)))
              (macrolet ((progress (pos &optional (bad ""))
                           ;; has to be a macro to avoid computing pos too often
                           ,(when count
                              ``(when (and ,',%out ,',pro
                                           (zerop (mod ,',count ,',pro)))
                                 (princ "." ,',%out) (force-output ,',%out)
                                 (when (and ,',pro1
                                            (= ,',pro1 (incf ,',pro1-count)))
                                   (show-eta ,pos ,bad)
                                   (setq ,',pro1-count 0))))))
                ,@body))
         (when ,%out
           (when ,done (princ "done" ,%out))
           (when (or ,run ',count) (setq ,el (elapsed ,bt t)))
           (when ,run (format ,%out " [run: ~/pr-secs/]" ,el))
           (when ,real (format ,%out " [real: ~/pr-secs/]" (elapsed ,b1 nil)))
           ,(when count
              `(unless (zerop ,el)
                 (format ,%out " [~5f ~a per second]" (/ ,count ,el) ,units)))
           (when ,terpri (terpri ,%out)))))))

;;; }}}

(provide :cllib-log)
;;; file log.lisp ends here
