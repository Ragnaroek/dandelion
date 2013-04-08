;;; Octave interface
;;;
;;; Copyright (C) 1997-2001 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: octave.lisp,v 2.8 2005/01/27 23:02:47 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/octave.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(export '(*octave-program* solve-lin))

;;;
;;;
;;;

(defun dot0 (l0 l1 &key (key #'value) key0 key1)
  "Compute the dot-product of the two sequences,
presumed to be of the same size."
  (declare (sequence l0 l1))
  (setq key0 (or key0 key) key1 (or key1 key))
  (reduce #'+ (map 'list (lambda (r0 r1)
			   (* (funcall key0 r0) (funcall key1 r1))) l0 l1)
	  :initial-value 0))

(defcustom *octave-program* simple-string
  #+(or win32 mswindows) "c:/bin/octave.exe" #+unix "/usr/local/bin/octave"
  "*The octave executable.")

(defun solve-lin (mx vec &optional dump)
  "Given a matrix N x N and an N vector, return the solution of the system.
Send the data to Octave, get the answer."
  (declare (array double-float (* *) mx)
	   (array double-float (*) vec))
  (assert (= (array-dimension mx 0) (array-dimension mx 1)
	     (array-dimension vec 0)) ()
             "solve-lin: the matrix must be N x N, and vector - N")
  (multiple-value-bind (oc-io oc-in oc-ou dim ans endstr les)
      (#+lisp=cl ext:make-pipe-io-stream #-lisp=cl lisp:make-pipe-io-stream
                 *octave-program*)
    (setq dim (array-dimension mx 0)
	  ans (make-array dim :element-type 'double-float
			  :initial-element 0d0 :adjustable nil)
	  endstr "ans = 579" les (length endstr))
    (format oc-ou "format long~%page_screen_output = 0~%
output_precision = 20~%AA=[")
    (dotimes (ii dim)
      (dotimes (jj dim)
	(format oc-ou "~f, " (aref mx ii jj)))
      (terpri oc-ou))
    (format oc-ou "]~%BB=[")
    (dotimes (ii dim)
      (format oc-ou "~f, " (aref vec ii)))
    (format oc-ou "]~%XX=BB/AA~%123+456~%")
    (do ((nn 0 (1+ nn)) (str (read-line oc-in) (read-line oc-in)))
	((and (>= (length str) les) (string= endstr str :end2 les)))
      (when dump (format t "octave --~3d--> ~a~%" nn str)))
    (dotimes (ii dim)
      (format oc-ou "XX(~d)~%" (1+ ii))
      (setf (aref ans ii) (read-from-string (read-line oc-in)
					    nil nil :start 5)))
    (close oc-in)
    (close oc-ou)
    (close oc-io)
    ans))

(provide :cllib-octave)
;;; octave.lisp ends here
