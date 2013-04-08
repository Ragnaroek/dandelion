;;; section 7: objects -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; function-keywords
(check-for-bug :section7-legacy-7
  (progn
    (defmethod gf1 ((a integer) &optional (b 2)
                    &key (c 3) ((:dee d) 4) e ((eff f)))
        (list a b c d e f))
    t)
  T)

(check-for-bug :section7-legacy-15
  (eq (find-method #'gf1 '() (list (find-class 'integer)))  'nil)
  nil)					; XXX

(check-for-bug :section7-legacy-19
  (multiple-value-list
      (function-keywords (find-method #'gf1 '()
                                      (list (find-class 'integer)))))
  ((:C :DEE :E EFF) nil))

(check-for-bug :section7-legacy-25
  (eq (defmethod gf2 ((a integer))
          (list a b c d e f)) 'nil)
  nil)					; XXX

(check-for-bug :section7-legacy-30
  (multiple-value-list
      (function-keywords (find-method #'gf2 '() (list (find-class 'integer)))))
  (() nil))

(check-for-bug :section7-legacy-35
  (progn
    (defmethod gf3 ((a integer) &key b c d &allow-other-keys)
        (list a b c d e f))
    t)
  t)

(check-for-bug :section7-legacy-42
  (multiple-value-list
      (function-keywords (find-method #'gf3 '() (list (find-class 'integer)))))
  ((:B :C :D) t))

;;; if only i knew more about clos

