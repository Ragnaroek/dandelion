;;; hardcopy printing
;;;
;;; Copyright (C) 1997-2001 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: laser.lisp,v 1.5 2005/01/27 23:02:47 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/laser.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; for `list-format'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; for `run-prog', `pipe-output', `with-open-pipe'
  (require :port-shell (translate-logical-pathname "port:shell")))

(in-package :cllib)

(export '(print-files with-printing))

;;;
;;;
;;;

(defun done-files (action &rest fl)
  "Print a message that the ACTION has been performed on FILES."
  (declare (simple-string action) (list fl))
  (format t "~a ~r file~:p~?.~%" action (length fl) (list-format "`~a'") fl))

;(defun print-files (printer &rest file-paths)
;  "Print the files to a printer: nil: print; t: nprint /q:ddslaser."
;  (let ((com (if printer "z:/public/nprint.exe" "print"))
;       (switch (if printer '("/q=ddslaser") nil)))
;    (mapc (lambda (fl) (run-program com :output nil
;                                     :arguments (cons fl switch)))
;         file-paths))
;  (apply #'done-files (if printer "Laser printing" "Printing locally")
;        file-paths))

;;;###autoload
(defun print-files (printer &rest file-paths)
  "Print the files to a printer: nil: print; t: nprint /q:ddslaser."
  (let ((com (if printer "lprint" "print")))
    (dolist (fl file-paths) (run-prog com :output nil :args (list fl))))
  (apply #'done-files (if printer "Laser printing" "Printing locally")
         file-paths))

(defcustom *nprint-path* pathname (pathname "z:/public/nprint.exe")
  "*The pathname of the nprint program.")
(defcustom *nprint-switch* simple-string "/q=ddslaser"
  "*The switch for laser printing.")
(defcustom *nprint-tmp-file* pathname
  (merge-pathnames "nprint.tmp" (or (getenv "TMP") "/tmp/"))
  "The tmp file for nprint.")
(defcustom *print-line-printer* simple-string
  "E&k2G(s0p16.67h8.5v0s0b0T&l80P&l8D&l2A"
  "*The string to put the laser printer in the line printer mode.")
(defcustom *print-portrait* simple-string "&l0O"
  "*The string to print in portrait.")
(defcustom *print-landscape* simple-string "&l1O"
  "*The string to print in landscape.")
(defcustom *print-eject-page* simple-string "&l0H"
  "*The string to eject page.")
(defcustom *print-weight-bold* simple-string "(s3B"
  "*The string to print bold.")
(defcustom *print-weight-medium* simple-string "(s0B"
  "*The string to print bold.")
(defcustom *print-weight-light* simple-string "(s-3B"
  "*The string to print light.")
(defcustom *print-weight-boldness* simple-string
  "(s0p16.67h8.5v0s~db0T" ;"(s~dB"
  "*The string to modify boldness (-7-+7).")
(defcustom *print-style-upright* simple-string "(s0S"
  "*The string to print normal/upright/solid.")
(defcustom *print-style-italic* simple-string "(s1S"
  "*The string to print italic.")
(defcustom *print-style-condensed* simple-string "(s4S"
  "*The string to print condensed.")
(defcustom *print-underline-on* simple-string "&d3D"
  "*The string to start underlining.")
(defcustom *print-underline-off* simple-string "&d@"
  "*The string to stop underlining.")

(defmacro with-output-to-nprint ((ostr (&rest pre) &optional (post "E"))
                                 &body body)
  "Execute BODY, binding OSTR to the stream going to a file that
will be fed to nprint. PRE* and POST are strings appended and
prepended to the STR.
  (with-output-to-nprint (OSTR (PRE0 PRE1 PRE2 ...) POST) BODY)"
  `(let ((,ostr (open *nprint-tmp-file* :direction :output)))
    (unwind-protect
         (progn (princ (concatenate 'string ,@pre) ,ostr) ,@body)
      (princ ,post ,ostr) (terpri ,ostr) (close ,ostr)
      (run-prog *nprint-path* :output nil :input nil
                :args (list *nprint-tmp-file* *nprint-switch*))
      (delete-file *nprint-tmp-file*))))

(defmacro with-printing ((out &key printer pre) &body body)
  "Macro: binds OUT to the printer stream.
PRINTER is passed as the `-P' options to lpr.  PRE is written
to the stream before BODY and can be a string or a sequence thereof."
  ;; (with-printing (out) (format out "[~a]~%testing~%" (current-time nil)))
  (declare (type (or null simple-string) printer))
  `(with-open-pipe
    (,out (pipe-output "lpr" "-h"
           ,@(when printer (list (concatenate 'string "-P" printer)))))
    ,(when pre `(if (stringp ,pre) (write-string ,pre ,out)
                 (map nil (lambda (ll) (write-string ll ,out)) ,pre)))
    ,@body))

(provide :cllib-laser)
;;; file laser.lisp ends here
