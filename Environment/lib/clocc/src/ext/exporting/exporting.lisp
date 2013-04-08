;;; Macros that export their definiendum
;;;
;;; Copyright (C) 2004 by Bruno Haible
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.

(defpackage "EXPORTING"
  (:use "COMMON-LISP")
  (:shadow . #1=(defconstant defparameter defvar define-symbol-macro
                 defun defgeneric defmethod define-compiler-macro defsetf
                 define-setf-expander defmacro define-modify-macro
                 deftype defstruct defclass define-condition
                 define-method-combination))
  (:export . #1#)
  #+(or CLISP CMU SBCL OpenMCL ALLEGRO LISPWORKS)
  (:import-from #+CLISP "CLOS"
                #+CMU "PCL" ; or "MOP"?
                #+SBCL "SB-PCL" ; or "SB-MOP"?
                #+OpenMCL "CLOS"
                #+ALLEGRO "CLOS"
                #+LISPWORKS "CLOS"
                slot-definition-readers slot-definition-writers
                class-direct-slots))

(in-package "EXPORTING")

;; Macros for the variable namespace.

(cl:defmacro defconstant (&whole whole
                          name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFCONSTANT ,name ,@(cddr whole))))

(cl:defmacro defparameter (&whole whole
                           name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFPARAMETER ,name ,@(cddr whole))))

(cl:defmacro defvar (&whole whole
                     name &optional initial-value documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFVAR ,name ,@(cddr whole))))

(cl:defmacro define-symbol-macro (symbol expansion)
  `(PROGN
     (EXPORT ',(or symbol '(NIL)))
     (CL:DEFINE-SYMBOL-MACRO ,symbol ,expansion)))

;; Macros for the function namespace.

(cl:defun function-block-name (name)
  (cond ((symbolp name) name)
        ((and (consp name) (eq (first name) 'SETF)
              (consp (cdr name)) (symbolp (second name)) (null (cddr name)))
         (second name))
        (t (error "Not a function name: ~S" name))))

(cl:defmacro defun (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or (function-block-name name) '(NIL)))
     (CL:DEFUN ,name ,lambda-list ,@body)))

(cl:defmacro defgeneric (name lambda-list &rest options)
  `(PROGN
     (EXPORT ',(or (function-block-name name) '(NIL)))
     (CL:DEFGENERIC ,name ,lambda-list ,@options)))

(cl:defmacro defmethod (name &rest definition)
  `(PROGN
     (EXPORT ',(or (function-block-name name) '(NIL)))
     (CL:DEFMETHOD ,name ,@definition)))

(cl:defmacro define-compiler-macro (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or (function-block-name name) '(NIL)))
     (CL:DEFINE-COMPILER-MACRO ,name ,lambda-list ,@body)))

(cl:defmacro defsetf (name &rest definition)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFSETF ,name ,@definition)))

(cl:defmacro define-setf-expander (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-SETF-EXPANDER ,name ,lambda-list ,@body)))

(cl:defmacro defmacro (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFMACRO ,name ,lambda-list ,@body)))

(cl:defmacro define-modify-macro (&whole whole
                                  name lambda-list function &optional documentation)
  (declare (ignore lambda-list function documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-MODIFY-MACRO ,name ,@(cddr whole))))

;; Macros for the type namespace.

(cl:defmacro deftype (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFTYPE ,name ,lambda-list ,@body)))

(cl:defun concat-pnames (obj1 obj2)
  (let ((str (concatenate 'string (string obj1) (string obj2))))
    (if (and (plusp (length str)) (eql (char str 0) #\:))
      (intern (subseq str 1) (find-package "KEYWORD"))
      (intern str))))

(cl:defun ds-accessor-name (slotname concname)
  (if concname 
    (concat-pnames concname slotname) 
    slotname))

(cl:defmacro defstruct (name+options &rest slots)
  (let ((name (if (consp name+options) (first name+options) name+options)))
    `(PROGN
       (EXPORT '(,name
                 ,@(let ((constructor-option-list nil)
                         (copier-option 0)
                         (predicate-option 0))
                     (when (consp name+options)
                       (dolist (option (rest name+options))
                         (if (or (eq option ':CONSTRUCTOR) (equal option '(:CONSTRUCTOR)))
                           (push (concat-pnames "MAKE-" name) constructor-option-list)
                           (when (and (consp option) (consp (cdr option)))
                             (case (first option)
                               (:CONSTRUCTOR (push (second option) constructor-option-list))
                               (:COPIER (setq copier-option (second option)))
                               (:PREDICATE (setq predicate-option (second option))))))))
                     (nconc (if constructor-option-list
                              (delete 'NIL constructor-option-list)
                              (list (concat-pnames "MAKE-" name)))
                            (when copier-option
                              (list (if (eql copier-option 0)
                                      (concat-pnames "COPY-" name)
                                      copier-option)))
                            (when predicate-option
                              (list (if (eql predicate-option 0)
                                      (concat-pnames name "-P")
                                      predicate-option)))))
                 ,@(let ((conc-name-option 0))
                     (when (consp name+options)
                       (dolist (option (rest name+options))
                         (when (and (consp option) (consp (cdr option))
                                    (eq (first option) ':CONC-NAME))
                           (setq conc-name-option (second option)))))
                     (when (eql conc-name-option 0)
                       (setq conc-name-option (concatenate 'string (string name) "-")))
                     (mapcar #'(lambda (slot-spec)
                                 (ds-accessor-name
                                   (if (consp slot-spec) (first slot-spec) slot-spec)
                                   conc-name-option))
                             slots))))
       (CL:DEFSTRUCT ,name+options ,@slots))))

(cl:defun slot-definition-accessor-symbols (slot)
  (mapcar #'function-block-name
          (append (slot-definition-readers slot)
                  (slot-definition-writers slot))))

(cl:defun all-accessor-symbols (direct-slot-list)
  (mapcan #'slot-definition-accessor-symbols direct-slot-list))

(cl:defun class-accessor-symbols (class)
  (all-accessor-symbols (class-direct-slots class)))

(cl:defmacro defclass (name superclasses slot-specs &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (LET ((C (CL:DEFCLASS ,name ,superclasses ,slot-specs ,@options)))
       (EXPORT (CLASS-ACCESSOR-SYMBOLS C))
       C)))

(cl:defmacro define-condition (name parent-types slot-specs &rest options)
  `(PROGN
     (EXPORT '(,name
               ,@(mapcan #'(lambda (slot-spec)
                             (when (consp slot-spec)
                               (let ((symbols '()))
                                 (do ((slot-options (cdr slot-spec) (cddr slot-options)))
                                     ((endp slot-options))
                                   (case (first slot-options)
                                     ((:READER :WRITER :ACCESSOR)
                                      (push (function-block-name (second slot-options))
                                            symbols))))
                                 (nreverse symbols))))
                         slot-specs)))
     (CL:DEFINE-CONDITION ,name ,parent-types ,slot-specs ,@options)))

;; Macros for the method-combination namespace.

(cl:defmacro define-method-combination (name &rest definition)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-METHOD-COMBINATION ,name ,@definition)))
