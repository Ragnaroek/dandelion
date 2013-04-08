;;; section 21: streams -*- mode: lisp -*-
(in-package :cl-user)


(check-for-bug :section21-legacy-5
  (subtypep 'stream 't)
  t)

(check-for-bug :section21-legacy-9
  (subtypep 'broadcast-stream 'stream)
  t)

(check-for-bug :section21-legacy-13
  (subtypep 'concatenated-stream 'stream)
  t)

(check-for-bug :section21-legacy-17
  (subtypep 'echo-stream 'stream)
  t)

(check-for-bug :section21-legacy-21
  (subtypep 'file-stream 'stream)
  t)

(check-for-bug :section21-legacy-25
  (subtypep 'string-stream 'stream)
  t)

(check-for-bug :section21-legacy-29
  (subtypep 'synonym-stream 'stream)
  t)

(check-for-bug :section21-legacy-33
  (subtypep 'two-way-stream 'stream)
  t)


;;; input-stream-p

(check-for-bug :section21-legacy-40
  (input-stream-p *standard-input*)
  t)

(check-for-bug :section21-legacy-44
  (input-stream-p *terminal-io*)
  t)

(check-for-bug :section21-legacy-48
  (input-stream-p (make-string-output-stream))
  nil)

(check-for-bug :section21-legacy-52
  (output-stream-p *standard-output*)
  t)

(check-for-bug :section21-legacy-56
  (output-stream-p *terminal-io*)
  t)

(check-for-bug :section21-legacy-60
  (output-stream-p (make-string-input-stream "jr"))
  nil)

;;; open-stream-p

(check-for-bug :section21-legacy-66
  (open-stream-p *standard-input*)
  t)

;;; read-byte

(check-for-bug :section21-legacy-72
  (with-open-file (s "/tmp/temp-bytes"
                     :direction :output
                     :element-type 'unsigned-byte)
    (write-byte 101 s))
  101)

(check-for-bug :section21-legacy-79
  (unwind-protect
       (with-open-file (s "/tmp/temp-bytes" :element-type 'unsigned-byte)
         (list (read-byte s) (read-byte s nil 'eof)))
    (delete-file "/tmp/temp-bytes"))
  (101 EOF))

;;; peek-char

(check-for-bug :section21-legacy-86
  (with-input-from-string (input-stream "    1 2 3 4 5")
    (list (peek-char t input-stream)
          (peek-char #\4 input-stream)
          (peek-char nil input-stream)))
  (#\1 #\4 #\4))

;;; read-char

(check-for-bug :section21-legacy-95
  (with-input-from-string (is "0123")
    (let ((a nil))
      (do ((c (read-char is) (read-char is nil 'the-end)))
          ((not (characterp c)))
        (setq a (cons c a)))
      a))
  (#\3 #\2 #\1 #\0))

;;; make-concatenated-stream

(check-for-bug :section21-legacy-106
  (read (make-concatenated-stream
         (make-string-input-stream "1")
         (make-string-input-stream "2")))
  12)






