;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

#+clisp
(setf (symbol-function 'setf-gethash)
      (symbol-function 'sys::puthash))

#+(or akcl ecls)
(setf (symbol-function 'setf-gethash)
      (symbol-function 'sys:hash-set)) t
#+allegro
(setf (symbol-function 'setf-gethash)
      (symbol-function 'excl::%puthash)) t
#+cmu
(setf (symbol-function 'setf-gethash)
      (symbol-function 'cl::%puthash)) t
#+sbcl
(setf (symbol-function 'setf-gethash)
      (symbol-function 'sb-impl::%puthash)) t

(check-for-bug :hashlong-legacy-21
  (defun symbole ()
    (let ((b 0.)
          (hash-table (make-hash-table :size 20.
                                       :rehash-threshold
                                       #+xcl 15.
                                       #-xcl 0.75))
          (liste (make-list 50.))
          (liste2 (make-list 50.)))
      (rplacd (last liste) liste)
      (rplacd (last liste2) liste2)
      (do-symbols (x (find-package #+xcl 'lisptest
                                   #-xcl "LISP"))
        ;;      (print x) (finish-output)
        (cond ((car liste)
               (let ((hval (gethash (car liste) hash-table))
                     (lval (car liste2)))
                 (unless (eq hval lval)
                   (print "mist, hash-tabelle kaputt")
                   (print (car liste))
                   (print hash-table)
                   (print (hash-table-count hash-table))
                   (print "hval:") (print hval)
                   (print "lval:") (print lval)
                   (return-from symbole 'error))
                 (remhash (car liste) hash-table)
                 #+xcl (when (< (room) 30000.) (system::%garbage-collection))
                 (setf-gethash x hash-table (setq b (+ 1. b)))
                 (rplaca liste x)
                 (rplaca liste2 b)
                 (setq liste (cdr liste))
                 (setq liste2 (cdr liste2))))
              (t (setf-gethash x hash-table (setq b (+ 1. b)))
                 (rplaca liste x)
                 (rplaca liste2 b)
                 (setq liste (cdr liste))
                 (setq liste2 (cdr liste2)))))))
  symbole)


(check-for-bug :hashlong-legacy-61
  (symbole) nil)

