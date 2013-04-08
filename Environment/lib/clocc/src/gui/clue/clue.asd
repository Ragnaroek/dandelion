;;; -*- Mode: lisp -*-

(in-package :asdf)

#+cmucl
(defsystem :clue
    :depends-on ( #+cmu :cmucl-clx #-cmu :clx)
    :components
    ((:file "clue")                     ; Define packages
     (:file "defgeneric" :depends-on ("clue"))		; pw adds 
     (:file "event-parse" :depends-on ("defgeneric"))		; Utilities for event translation
     (:file "defcontact" :depends-on ("event-parse"))		; CLOS for resources and type conversion
     (:file "intrinsics" :depends-on ("defcontact"))		; The "guts"
     (:file "caches" :depends-on ("intrinsics"))                   ; Support for gcontext, pixmap, cursor cacheing
     (:file "resource" :depends-on ("caches"))                 ; Resource and type conversion
     (:file "gray" :depends-on ("resource"))                     ; Gray stipple patterns
     (:file "cursor" :depends-on ("gray"))                   ; Standard cursor names
     (:file "events" :depends-on ("cursor"))                   ; Event handling
     (:file "virtual" :depends-on ("events"))                  ; Support for windowless contacts
     (:file "shells" :depends-on ("virtual"))                   ; Support for top-level window/session mgr.
     (:file "root-gmgmt" :depends-on ("shells"))		; Geometry management methods for root contacts
     (:file "package" :depends-on ("root-gmgmt"))))                  ; External cluei symbols exported from clue

