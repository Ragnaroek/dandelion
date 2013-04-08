;;; Stuff for the format ~// instruction
;;;
;;; Copyright (C) 1997-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: tilsla.lisp,v 1.8 2005/01/27 23:02:46 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/tilsla.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `dfloat'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype")))

(in-package :cllib)

(export '(commas pr-secs pr-arr print-seqs *seconds-long-threshold*))
(import '(comma pr-secs pr-arr) :cl-user)

;;;
;;; {{{ Commas
;;;

(defun commas (num &optional (dd 0 ddp) (di 0))
  "Commify a number into a string.
The two optional arguments are the number of digits
after and before the decimal point."
  (declare (fixnum dd di) (number num))
  (setq num (dfloat num))
  (multiple-value-bind (inum dnum) (truncate num)
    (declare (integer inum))
    (let ((dnum (abs (dfloat dnum))))
      (declare (double-float dnum))
      (unless ddp (setq dd (1- (length (format nil "~f" dnum)))))
      (when (< (- 1 dnum) (expt 1d-1 dd)) ; rounding at formatting
        (if (minusp inum) (decf inum) (incf inum))
        (setq dnum 0d0))
      (format nil "~v:d~v,vf" di inum (1+ dd) dd dnum))))

(defun comma (stream num colon-p atsign-p &optional (dd 0 ddp) (di 0)
              (padchar #\Space) (commachar #\,) (dollarchar #\$))
  "Print the number, commified.
Can be used in `format' as ~cents,dollars,padchar,commachar,dollchar/comma/,
where `cents' and `dollars' are the widths of the corresponding fields,
padchar is the character used to pad to the width on the left end, and
commachar is the separator between each group of three digits, dollarchar
is the currency character.
The @ modifier controls printing of the sign of positive amounts.  If
omitted, a positive value prints without a sign.  Otherwise, a positive
amount has an explicit + sign.
The : modifier controls the presence of the dollar sign: if given,
the dollar sign is printed right before the first digit, if not, no
dollar sign is printed."
  (declare (stream stream) (fixnum dd di) (number num)
           (character padchar commachar dollarchar))
  (multiple-value-bind (inum dnum) (truncate num)
    (declare (integer inum))
    (let ((dnum (abs (dfloat dnum))))
      (declare (double-float dnum))
      (unless ddp (setq dd (1- (length (format nil "~f" dnum)))))
      (when (< (- 1 dnum) (expt 1/10 dd)) ; rounding at formatting
        (incf inum (signum inum))
        (setq dnum 0d0))
      (let ((str (format nil "~,,v:d~v,vf" commachar inum (1+ dd) dd dnum))
            (sig (if (and atsign-p (plusp num)) #\+)))
        (declare (simple-string str))
        (format stream "~v,,,va~@[~c~]~@[~c~]~a"
                (max 0 (- (+ 1 dd di) (length str)
                          (if sig 1 0) (if colon-p 1 0)))
                padchar "" (if colon-p dollarchar) sig str)))))

;;;
;;; }}}{{{ time
;;;

(defconst +internal-time-digits+ fixnum
  (min 3 (round (log internal-time-units-per-second) (log 10)))
  "The number of digits in `internal-time-units-per-second'.")
(defcustom *seconds-long-threshold* double-float 300d0
  "If time is larger than this, `pr-secs' will use `hh:mm:ss' format.")

(defun pr-secs (stream sec colon-p atsign-p)
  "Print the number of seconds, nicely.
Can be used in `format', as ~/pr-secs/.
With @, print the sign.
With :, print both `comma' and the longer format:
Without :, `*seconds-long-threshold*' determines the format."
  (declare (stream stream) (real sec))
  (let ((sig (minusp sec)) (sec (abs sec)))
    (when (or colon-p (< sec *seconds-long-threshold*))
      (comma stream sec nil atsign-p +internal-time-digits+)
      (princ " sec" stream))
    (when colon-p (princ " [" stream))
    (when (or colon-p (>= sec *seconds-long-threshold*))
      (multiple-value-bind (mi se) (floor (round sec) 60)
        (declare (integer mi) (fixnum se))
        (multiple-value-bind (ho mi) (floor mi 60)
          (declare (integer ho) (fixnum mi))
          (multiple-value-bind (da ho) (floor ho 24)
            (declare (integer da) (fixnum ho))
            (format
             stream "~@[~*+~]~@[~*-~]~[~:;~:*~:d day~:p ~]~2,'0d:~2,'0d:~2,'0d"
             (and atsign-p (not sig)) sig da ho mi (round se))))))
    (when colon-p (princ "]" stream))))

;;;
;;; }}}{{{ Sequences
;;;

(defun pr-arr (stream arr colon-p atsign-p &optional
               (fmt (if colon-p "~6d " "~6f ")))
  "Print the array, in `format': ~fmt/pr-arr/.
: - ~d instead of ~f for elements; @ - print the indexes."
  (declare (type (simple-array * (*)) arr))
  (loop :for el :across arr :and idx :of-type index-t :upfrom 1 :do
        (format stream "~:[~*~; ~d: ~]~@?" atsign-p idx fmt el)))

(defun print-seqs (stream fmt &rest seqs)
  "Print the sequences SEQs to STREAM using format string FMT.
The first command in FMT will print the item number,
the rest - the items themselves. As usual, STREAM value of nil means
return the string with the printout.
As FMT is passed directly to `format', it can be a function generated by
the `FORMATTER' macro."
  (declare (type (or simple-string function) fmt))
  (let ((str (case stream ((nil) (make-string-output-stream))
                   ((t) *standard-output*) (t stream))) (idx 0))
    (declare (type index-t idx) (stream str))
    (apply #'map nil
           (if (functionp fmt)
               (lambda (&rest rr) (apply fmt str (incf idx) rr))
               (lambda (&rest rr) (apply #'format str fmt (incf idx) rr)))
           seqs)
    (unless stream (get-output-stream-string str))))

;;; }}}

(provide :cllib-tilsla)
;;; file tilsla.lisp ends here
