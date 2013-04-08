(in-package :cl-user)
;;; From: Fred Gilham <gilham@snapdragon.csl.sri.com>

(let* ((original-read-table *readtable*)
       (snepslog-read-table (copy-readtable nil))
       (*readtable* snepslog-read-table))
  (set-macro-character
   #\,
   #'(lambda (s c) (declare (ignore s c)) (quote \,)))
  (set-macro-character
   #\~
   #'(lambda (s c) (declare (ignore s c)) (quote \~)))
  (set-macro-character
   #\.
   #'(lambda (s c) (declare (ignore s c)) (quote \.)))
  (set-macro-character
   #\:
   #'(lambda (s c) (declare (ignore s c)) (quote \:)))
  (set-macro-character
   #\{
   #'(lambda (s c) (declare (ignore s c)) (quote \{)))
  (set-macro-character
   #\}
   #'(lambda (s c) (declare (ignore s c)) (quote \})))

  (defun snepslogreadon ()
    "Sets the readtable to the snepslog read table"
    (setq *readtable* snepslog-read-table))

  (defun snepslogreadoff ()
    "Sets  the readtable to the original readtable
(a copy of the initial readtable)"
    (setq *readtable* original-read-table)))
