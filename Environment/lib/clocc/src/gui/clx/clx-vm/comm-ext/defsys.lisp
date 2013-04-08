;;; This software is Copyright (c) Barry Wilkes 2001
;;; Barry Wilkes grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(defsystem comm-ext ()
  :members ("package"
            "comm-ext")
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))