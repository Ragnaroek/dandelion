;;; Extensible Sequence Type
;;;
;;; Copyright (C) 1988, 1992 Bruno Haible
;;; This is Free Software, published under the GNU General Public License v2.

;; dummy-package for sequences in CLISP
;; Bruno Haible 24.04.1992

;; Sam Steingold 2004-09-05 - updated for ANSI CL and modern CLISP

(provide 'sequences)

(defpackage "SEQUENCES" (:nicknames "SEQ")
  (:use "COMMON-LISP")
  (:export .
       (elt subseq copy-seq length list-reverse reverse list-nreverse
        nreverse make-sequence concatenate map some every notany notevery
        reduce fill replace remove remove-if remove-if-not delete delete-if
        delete-if-not remove-duplicates delete-duplicates substitute
        substitute-if substitute-if-not nsubstitute nsubstitute-if
        nsubstitute-if-not find find-if find-if-not position position-if
        position-if-not count count-if count-if-not mismatch search sort
        stable-sort merge coerce))
  (:export do-sequence define-sequence))
(in-package "SEQUENCES")

(setf (symbol-function 'list-reverse) (symbol-function 'reverse))
(setf (symbol-function 'list-nreverse) (symbol-function 'nreverse))

;; (do-sequence (var sequenceform [resultform]) {declaration}* {tag|statement}*)
(defmacro do-sequence ((var sequenceform &optional (resultform nil)) &body body)
  (multiple-value-bind (body-rest declarations)
      (let ((body-rest body) (declarations nil))
        (loop
          (if (and (consp body-rest)
                   (consp (car body-rest))
                   (eq (caar body-rest) 'declare))
            (setq declarations (revappend (cdr (pop body-rest)) declarations))
            (return)))
        (values body-rest (cl:nreverse declarations)))
    (setq declarations (if declarations `((DECLARE ,declarations)) '()))
    (let ((seqvar (gensym)))
      `(BLOCK NIL
         (LET ((,seqvar ,sequenceform))
           (LET ((,var NIL))
             ,@declarations
             ,var ; var is only seemingly evaluated
             (MAP NIL
                  #'(LAMBDA (,var) ,@declarations (TAGBODY ,@body-rest))
                  ,seqvar)
             ,resultform))))))

(defmacro define-sequence-1 (name &rest functions)
  `(SYSTEM::%DEFSEQ
     (VECTOR
       ',name
       ,@(mapcar #'(lambda (function)
                     (if (and (consp function)
                              (eq (first function) 'FUNCTION)
                              (consp (second function))
                              (eq (first (second function)) 'LAMBDA))
                       `(FUNCTION
                         (LAMBDA (SEQ ,@(second (second function)))
                          ,@(cddr (second function))))
                       `(FUNCTION
                         (LAMBDA (SEQ &REST SEQ-ARGS)
                          (APPLY ,function SEQ-ARGS)))))
                 functions))))

(defmacro define-sequence
    (name &key init upd endtest fe-init fe-upd fe-endtest access access-set
     copy length make elt set-elt init-start fe-init-end)
  (unless upd (error ":UPD must be specified."))
  (unless fe-upd (error ":FE-UPD must be specified."))
  (unless access (error ":ACCESS must be specified."))
  (unless access-set (error ":ACCESS-SET must be specified."))
  (unless endtest (error ":ENDTEST must be specified."))
  (unless fe-endtest (error ":FE-ENDTEST must be specified."))
  (unless make (error ":MAKE must be specified."))
  (unless (or init init-start)
    (error ":INIT or :INIT-START must be specified."))
  (cond ((and init (not init-start))
         (setq init-start
           `#'(lambda (index)
                (let ((pointer (funcall ,init)))
                  (dotimes (i index) (setq pointer (funcall ,upd pointer)))
                  pointer))))
        ((and init-start (not init))
         (setq init
           `#'(lambda () (funcall ,init-start 0)))))
  (unless elt
    (setq elt
      `#'(lambda (index) (funcall ,access (funcall ,init-start index)))))
  (unless set-elt
    (setq set-elt
      `#'(lambda (index value)
           (funcall ,access-set (funcall ,init-start index) value))))
  (unless length
    (setq length
      `#'(lambda ()
           (do ((pointer (funcall ,init) (funcall ,upd pointer))
                (i 0 (1+ i)))
               ((funcall ,endtest pointer) i)))))
  (unless (or fe-init fe-init-end)
    (error ":FE-INIT or :FE-INIT-END must be specified."))
  (cond ((and fe-init (not fe-init-end))
         (setq fe-init-end
           `#'(lambda (index)
                (let ((pointer (funcall ,fe-init)))
                  (dotimes (i (- (funcall ,length) index))
                    (setq pointer (funcall ,fe-upd pointer)))
                  pointer))))
        ((and fe-init-end (not fe-init))
         (setq fe-init
           `#'(lambda () (funcall ,fe-init-end (funcall ,length))))))
  (unless copy
    (warn ":COPY not specified, interprete as #'IDENTITY")
    (setq copy '#'identity))
  `(DEFINE-SEQUENCE-1 ,name
     ,init ,upd ,endtest ,fe-init ,fe-upd ,fe-endtest ,access ,access-set
     ,copy ,length ,make ,elt ,set-elt ,init-start ,fe-init-end))

