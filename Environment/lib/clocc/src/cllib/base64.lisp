;;; Base64 encoding and decoding
;;; <http://rfc.net/rfc2045.html>
;;;
;;; Copyright (C) 2004-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: base64.lisp,v 2.3 2006/03/09 15:42:22 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base64.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(export '(base64-encode base64-decode))

(defparameter *base64-table*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;;;###autoload
(defun base64-encode (vec &optional buffer)
  "Encode the vector of bytes as a string in base64."
  (let ((vec-len (length vec)) quotient remainder str full-top str-len)
    (setf (values quotient remainder) (ceiling vec-len 3)
          str-len (* 4 quotient)
          str (or buffer (make-string str-len))
          full-top (* 4 (if (zerop remainder) quotient (1- quotient))))
    (loop :with triplet :for vec-pos :from 0 :by 3 :for str-pos :from 0 :by 4
      :while (< str-pos full-top) :do
      (setf triplet (+ (ash (aref vec vec-pos) 16)
                       (ash (aref vec (+ 1 vec-pos)) 8)
                       (aref vec (+ 2 vec-pos)))
            (char str str-pos)
            (char *base64-table* (ldb (byte 6 18) triplet))
            (char str (+ 1 str-pos))
            (char *base64-table* (ldb (byte 6 12) triplet))
            (char str (+ 2 str-pos))
            (char *base64-table* (ldb (byte 6 6) triplet))
            (char str (+ 3 str-pos))
            (char *base64-table* (ldb (byte 6 0) triplet))))
    (ecase remainder
      (-2 (let ((val (ash (aref vec (- vec-len 1)) 16)))
            (setf (char str (- str-len 4))
                  (char *base64-table* (ldb (byte 6 18) val))
                  (char str (- str-len 3))
                  (char *base64-table* (ldb (byte 6 12) val))
                  (char str (- str-len 2)) #\=
                  (char str (- str-len 1)) #\=)))
      (-1 (let ((val (+ (ash (aref vec (- vec-len 2)) 16)
                        (ash (aref vec (- vec-len 1)) 8))))
            (setf (char str (- str-len 4))
                  (char *base64-table* (ldb (byte 6 18) val))
                  (char str (- str-len 3))
                  (char *base64-table* (ldb (byte 6 12) val))
                  (char str (- str-len 2))
                  (char *base64-table* (ldb (byte 6 6) val))
                  (char str (- str-len 1)) #\=)))
      (0))                      ; golden!
    str))

(defparameter *table-base64*
  (let ((vec (make-array 128)))
    (loop :for pos :upfrom 0 :for ch :across *base64-table*
      :do (setf (aref vec (char-code ch)) pos))
    vec))

;;;###autoload
(defun base64-decode (str &optional buffer)
  "Decode the string into a vector of bytes."
  (let* ((str-len (length str)) quotient remainder vec vec-len full-top
         (=count
          (if (and (> str-len 0) (char= #\= (char str (- str-len 1))))
              (if (and (> str-len 1) (char= #\= (char str (- str-len 2))))
                  2 1)
              0)))
    (setf (values quotient remainder) (floor str-len 4))
    (unless (zerop remainder)
      (error "~S: invalid base64 data length ~:D: ~S"
             'base64-decode str-len str))
    (setq full-top (* 3 quotient)
          vec-len (- full-top =count)
          vec (or buffer (make-array vec-len :element-type '(unsigned-byte 8))))
    (unless (zerop =count) (decf full-top 3))
    (macrolet ((get-byte (pos)
                 `(or (svref *table-base64* (char-code (char str ,pos)))
                      (error "~S: invalid base64 character ~@C at ~:D in ~S"
                             'base64-decode (char str ,pos) ,pos str))))
      (loop :with quad :for vec-pos :from 0 :by 3 :for str-pos :from 0 :by 4
        :while (< vec-pos full-top) :do
        (setf quad (+ (ash (get-byte str-pos) 18)
                      (ash (get-byte (+ 1 str-pos)) 12)
                      (ash (get-byte (+ 2 str-pos)) 6)
                      (get-byte (+ 3 str-pos)))
              (aref vec vec-pos) (ldb (byte 8 16) quad)
              (aref vec (+ 1 vec-pos)) (ldb (byte 8 8) quad)
              (aref vec (+ 2 vec-pos)) (ldb (byte 8 0) quad)))
      (ecase =count
        (2 (let ((val (+ (ash (get-byte (- str-len 4)) 18)
                         (ash (get-byte (- str-len 3)) 12))))
             (setf (aref vec (- vec-len 1)) (ldb (byte 8 16) val))))
        (1 (let ((val (+ (ash (get-byte (- str-len 4)) 18)
                         (ash (get-byte (- str-len 3)) 12)
                         (ash (get-byte (- str-len 2)) 6))))
             (setf (aref vec (- vec-len 2)) (ldb (byte 8 16) val)
                   (aref vec (- vec-len 1)) (ldb (byte 8 8) val))))
        (0))                    ; golden!
      vec)))

(provide :cllib-base64)
;;; file base64.lisp ends here
