(in-package #:cl-user)

(defpackage :de.fh-trier.test.utils
  (:use #:common-lisp 
        #:de.fh-trier.evalserver.protocol)
  (:export #:with-test-writer2
           #:read-file ;flet-Funktion in with-test-writer2 Makro
           #:*writer*
           #:*reader*))

(in-package :de.fh-trier.test.utils)

(defparameter *reader* nil)
(defparameter *writer* nil)

;(macroexpand-1 '(with-test-writer2))
(defmacro with-test-writer2 (&body body)
  (let ((s (gensym)))
    `(with-open-file (,s "/tmp/testfile.tmp" :direction :output :if-exists :supersede :element-type (get-element-type)) ;'(UNSIGNED-BYTE 8))
       (flet ((read-file ()
                #+clisp (close ,s) ;Fix fuer Clisp, erlaubt nicht zweimal oeffnen selbe Datei
                (let ((s2 (gensym)) (strings nil)) 
                  (with-open-file (s2 "/tmp/testfile.tmp" :direction :input)
                    (loop for line = (read-line s2 nil)
                          while line do (setf strings (cons line strings)))) (nreverse strings))))
       (let ((*writer* (make-instance 'protocol-writer :output-stream ,s)))
           ,@body)))))

;(get-element-type)
(defun get-element-type ()
  #+clisp 'character
  #-(or clisp) '(unsigned-byte 8))
