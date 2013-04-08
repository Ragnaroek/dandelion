;;; based on v1.4 -*- mode: lisp -*-
(in-package :cl-user)

(check-for-bug :streamslong-legacy-4
  (read-from-string "123")
  123)

(check-for-bug :streamslong-legacy-8
  (prin1-to-string 123)
  "123")

(check-for-bug :streamslong-legacy-12
  (let ((*a*
         (make-array 10. :element-type 'character
                     :fill-pointer 0)))
    (format *a* "XXX"))
  nil)

(check-for-bug :streamslong-legacy-19
  (let ((*a*
         (make-array 10. :element-type 'character
                     :fill-pointer 0)))
    (format *a* "XXX")
    *a*)
  "XXX")

#+xcl
(check-for-bug :streamslong-legacy-28
  (sys::check-stream-system)
  #+xcl t)

(check-for-bug :streamslong-added-1
  (defun bin-stream-test (&key (size (integer-length most-positive-fixnum))
                               (type 'unsigned-byte)
                               (file-name "/tmp/foocl")
                               (num-bytes 10)
                               (bytes (if (eq type 'signed-byte)
                                          (loop :repeat num-bytes
                                            :collect
                                            (- (random (ash 1 size))
                                               (ash 1 (1- size))))
                                          (loop :repeat num-bytes
                                            :collect
                                            (random (ash 1 size))))))
    (with-open-file (foo file-name
                         :direction :output
                         :element-type (list type size))
      (dolist (byte bytes)
        (write-byte byte foo)))
    (unwind-protect
         (with-open-file (foo file-name
                              :direction :input
                              :element-type (list type size))
           (list (stream-element-type foo)
                 (file-length foo)
                 bytes
                 (loop :for byte :in bytes
                       :for nb = (read-byte foo)
                       :collect nb
                       :unless (= nb byte)
                       :do
                       (flet ((by-out (sz by)
                                (format nil "~v,'0,' ,4:b"
                                        (+ sz (floor sz 4))
                                        by)))
                         (error "~& * [(~s ~s)] ~a != ~a~%"
                                type size
                                (by-out size byte)
                                (by-out size nb))))))
      (delete-file file-name)))
  bin-stream-test)

(check-for-bug :streamslong-unsigned-bytes-upto-40bits
 (loop for size from 2 to 40
       do
       (bin-stream-test :size size))
 nil)

(check-for-bug ::streamslong-signed-bytes-upto-40bits
  (loop for size from 2 to 40
        do
        (bin-stream-test :size size :type 'signed-byte))
  nil)

#+clisp
(check-for-bug :streamslong-added-4
  (defun clisp-test-bin-i/o (&key (num 10)
                                  (file-name "/tmp/foocl")
                                  (size 16)
                                  (endianness :little)
                                  (int-list
                                   (loop :repeat num
                                     :collect (random (ash 1 size))))
                                  (float-list
                                   (loop :repeat num
                                     :collect (random 1d0))))
    (let ((eltype (list 'unsigned-byte size)))
      (with-open-file (foo file-name
                           :direction :output
                           :element-type 'unsigned-byte)
        (dolist (num int-list)
          (write-integer num foo eltype endianness))
        (dolist (num float-list)
          (write-float num foo 'double-float endianness)))
      (unwind-protect
           (with-open-file (foo file-name
                                :direction :input
                                :element-type 'unsigned-byte)
             (list (file-length foo)
                   int-list
                   float-list
                   (loop :for num :in int-list
                         :for nn = (read-integer foo eltype endianness)
                         :collect nn
                         :unless (= nn num)
                         :do
                         (error "~& ~s: wrote: ~s  read: ~s"
                                endianness num nn))
                   (loop :for num :in float-list
                         :for nn = (read-float foo 'double-float
                                               endianness)
                         :collect nn
                         :unless (= nn num)
                         :do
                         (error "~& ~s: wrote: ~s  read: ~s"
                                endianness num nn))))
        (delete-file file-name))))
  clisp-test-bin-i/o)

#+clisp
(check-for-bug :streamslong-added-5
  (dolist (e '(:little :big))
    (clisp-test-bin-i/o :endianness e))
  nil)
