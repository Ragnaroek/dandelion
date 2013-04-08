;;; based on v1.7 -*- mode: lisp -*-
(in-package :user)

(defun check-xgcd (a b)
  (multiple-value-bind (g u v) (xgcd a b)
    (if (= g (+ (* a u) (* b v)))
        g
        (format t "~& ~d~% ~d~%  ==> ~d~% ~d~% ~d~%" a b g u v))))
    
(check-for-bug :number2-legacy-4
  (check-xgcd 2346026393680644703525505657 17293822570713318399)
  11)

(check-for-bug :number2-legacy-8
  (check-xgcd 77874422 32223899)
  1)

(check-for-bug :number2-legacy-12
  (check-xgcd 560014183 312839871)
  1)

(check-for-bug :number2-legacy-16
  (check-xgcd 3 2)
  1)

(check-for-bug :number2-legacy-20
  (check-xgcd 2 3)
  1)

(check-for-bug :number2-legacy-24
  (check-xgcd  974507656412513757857315037382926980395082974811562770185617915360
               -1539496810360685510909469177732386446833404488164283)
  1)

(check-for-bug :number2-legacy-32
  (isqrt #x3FFFFFFFC000000000007F)
  #x7FFFFFFFBFF)

