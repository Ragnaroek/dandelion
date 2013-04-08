;;; based on v1.7 -*- mode: lisp -*-
(in-package :cl-user)

;; test the macro functions; chapter 8
;; -----------------------------------


;; 8.1
;; macro-function | defmacro


(check-for-bug :macro8-legacy-12
  (and (macro-function 'push) T)
  T)

(check-for-bug :macro8-legacy-16
  (and (macro-function 'member) T)
  NIL)

(check-for-bug :macro8-legacy-20
  (defmacro arithmetic-if (test neg-form zero-form pos-form)
    (let ((var (gensym)))
      `(let ((,var ,test))
         (cond ((< ,var 0) ,neg-form)
               ((= ,var 0) ,zero-form)
               (T ,pos-form)))))
  arithmetic-if)


(check-for-bug :macro8-legacy-30
  (and (macro-function 'arithmetic-if) T)
  T)

(check-for-bug :macro8-legacy-34
  (setf x 8)
  8)

(check-for-bug :macro8-legacy-38
  (arithmetic-if (- x 4)(- x)(LIST "ZERO") x)
  8)


(check-for-bug :macro8-legacy-43
  (setf x 4)
  4)

(check-for-bug :macro8-legacy-47
  (arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
  ("ZERO"))


(check-for-bug :macro8-legacy-52
  (setf x 3)
  3)

(check-for-bug :macro8-legacy-56
  (arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
  -3)



(check-for-bug :macro8-legacy-62
  (defmacro arithmetic-if (test neg-form &optional zero-form pos-form)
    (let ((var (gensym)))
      `(let ((,var ,test))
         (cond ((< ,var 0) ,neg-form)
               ((= ,var 0) ,zero-form)
               (T ,pos-form)))))
  arithmetic-if)


(check-for-bug :macro8-legacy-72
  (setf x 8)
  8)

(check-for-bug :macro8-legacy-76
  (arithmetic-if (- x 4)(- x))
  nil)


(check-for-bug :macro8-legacy-81
  (setf x 4)
  4)

(check-for-bug :macro8-legacy-85
  (arithmetic-if (- x 4)(- x))
  NIL)


(check-for-bug :macro8-legacy-90
  (setf x 3)
  3)

(check-for-bug :macro8-legacy-94
  (arithmetic-if (- x 4)(- x))
  -3)

(check-for-bug :macro8-legacy-98
  (defmacro halibut ((mouth eye1 eye2)
                     ((fin1 length1)(fin2 length2))
                     tail)
    `(list ,mouth ,eye1 ,eye2 ,fin1 ,length1 ,fin2 ,length2 ,tail))
  halibut)

(check-for-bug :macro8-legacy-105
  (setf m 'red-mouth
        eyes '(left-eye . right-eye)
        f1 '(1 2 3 4 5)
        f2 '(6 7 8 9 0)
        my-favorite-tail '(list of all parts of tail))
  (list of all parts of tail))



(check-for-bug :macro8-legacy-115
  (halibut (m (car eyes)(cdr eyes))
           ((f1 (length f1))(f2 (length f2)))
           my-favorite-tail)
  (RED-MOUTH LEFT-EYE RIGHT-EYE (1 2 3 4 5) 5 (6 7 8 9 0) 5
             (LIST OF ALL PARTS OF TAIL)))

;; 8.2
;;  macroexpand | macroexpand-1


(check-for-bug :macro8-legacy-126
  (ecase 'otherwise
    (otherwise 4))
  4
  "This is bad style, but perfectly legal!!")

;; Issue MACRO-FUNCTION-ENVIRONMENT:YES
(check-for-bug :macro8-legacy-133
  (macrolet ((foo (&environment env)
               (if (macro-function 'bar env)
                   ''yes
                   ''no)))
    (list (foo)
          (macrolet ((bar () :beep))
            (foo))))
  (no yes))
