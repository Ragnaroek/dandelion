
(defpackage :dandelion-server.system
   (:use #:cl #:asdf))
   
(in-package :dandelion-server.system)

(defsystem :dandelion-server
     :version "1.1.0"
     :depends-on (:cl-base64 :cl-ppcre :log4cl :usocket)
     :components
     ((module "src" :serial T
                    :components ((:file "utils")
                    			 (:file "meta")
                    			 (:file "protocol")
                    			 (:file "server")
                    			 (:file "main")))))
                    			 

                     