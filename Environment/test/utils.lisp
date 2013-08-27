(in-package #:cl-user)

(defpackage #:dandelion-test-utils
  (:use #:common-lisp 
        #:dandelion-protocol)
  (:export #:with-test-writer
           #:with-test-reader
           #:read-test-file
           #:*writer*
           #:*reader*
           #:*testfile*))

(in-package #:dandelion-test-utils)

(defparameter *reader* nil)
(defparameter *writer* nil)
(defparameter *stream* nil)
(defparameter *testfile* (merge-pathnames "tmp/dandelion-testfile.tmp" (user-homedir-pathname)))

(defmacro with-test-writer (&body body)
  (let ((s (gensym)))
    `(with-open-file (*stream* *testfile* :direction :io :if-exists :supersede :element-type 'character)
       (let ((*writer* (make-instance 'protocol-writer :output-stream *stream*)))
           ,@body))))

(defun read-test-file (&aux strings)
    (file-position *stream* 0)
    (loop for line = (read-line *stream* nil)
          while line do (setf strings (cons line strings)))
  (nreverse strings))

(defmacro with-test-reader (string &body body)
  (let ((s (gensym)))
    `(with-input-from-string (,s ,string)
       (let ((*reader* (make-instance 'protocol-reader :input-stream ,s)))
         ,@body))))
