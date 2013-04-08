;;; This software is Copyright (c) Barry Wilkes 2001
;;; Barry Wilkes grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; open-unix-stream : An interface to unix domain streams
;;; host == /local
;;; service == path to unix domain socket
;;; direction == :input, :output or :io
;;; element-type == :base-char, :unsigned-byte or :signed-byte
;;; errorp == nil -> returns nil on failure to connect, non-nil -> signals an error
;;; timeout == Not sure, but needed to match signature of comm:open-tcp-stream
;;; buffered == Again, not sure. I can only create buffered streams anyway. 
;;; As I do not know the defaults which comm:open-socket-stream has, unless buffered and 
;;; timeout are explicitly passed into me, I DO NOT forward them AT ALL to comm:open-tcp-stream
;;; Signature matches that of comm:open-tcp-stream
(in-package :uk.org.bew.comm-ext)
(eval-when (:compile-toplevel :load-toplevel)
  (require "comm"))
(eval-when (:load-toplevel)
    (fli:register-module :uk.org.bew.comm-ext :real-name (translate-logical-pathname "COMM-EXT:liblwcomm-ext.so")))

(fli:define-foreign-function
    (unix-domain-socket "unix_domain_socket" :source)
    ((service :pointer))
  :result-type :int
  :module :uk.org.bew.comm-ext
  :documentation "Returns an open domain socket on pathname 'service' or -1 on error")

(fli:define-foreign-function
    (close-fd "close" :source)
    ((socket :int))
  :result-type :int
  :module :uk.org.bew.comm-ext
  :documentation "Closes the open socket 'socket'")

(defun open-unix-stream (hostname service &key
                                  (direction :io)
                                  (element-type 'base-char)
                                  buffered
                                  (errorp nil)
                                  timeout)
  (declare (ignore hostname buffered timeout))
  (fli:with-foreign-string (ptr-service element-count byte-count
                                        :external-format :ascii)
      service
    (declare (ignore element-count byte-count))
    (let ((socket (unix-domain-socket ptr-service)))
      (if (= socket -1)
          (if errorp
              (error "Failed to create unix domain socket ~S" service)
            nil)
        (make-instance 'comm:socket-stream
                       :socket socket
                       :element-type element-type
                       :direction direction)))))

(pushnew :uk.org.bew.comm-ext *features*)
