;;; Environment & System access
;;;
;;; Copyright (C) 1999-2006 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: sys.lisp,v 1.66 2006/11/28 05:02:23 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/sys.lisp,v $

(eval-when (compile load eval)
  (require :port-ext (translate-logical-pathname "clocc:src;port;ext"))
  ;; `default-directory'
  (require :port-path (translate-logical-pathname "port:path"))
  #+sbcl
  (require :sb-introspect)
  #+(and allegro mswindows)
  (require :ole))

(in-package :port)

#+sbcl (eval-when (compile load eval)
         (shadow '(getenv finalize)))

(export
 '(getenv finalize variable-special-p variable-not-special arglist
   compiled-file-p
   class-slot-list class-slot-initargs
   +month-names+ +week-days+ +time-zones+ tz->string string->tz
   current-time sysinfo))

;;;
;;; System
;;;

(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+mcl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu gcl lispworks lucid mcl sbcl scl)
  (error 'not-implemented :proc (list 'getenv var)))

(defun (setf getenv) (val var)
  "Set an environment variable."
  #+allegro (setf (sys::getenv (string var)) (string val))
  #+clisp (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl)
  (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp
                     :key #'string)))
    (if cell
        (setf (cdr cell) (string val))
        (push (cons (intern (string var) "KEYWORD") (string val))
              ext:*environment-list*)))
  #+gcl (si:setenv (string var) (string val))
  #+lispworks (setf (lw:environment-variable (string var)) (string val))
  #+lucid (setf (lcl:environment-variable (string var)) (string val))
  #-(or allegro clisp cmu gcl lispworks lucid scl)
  (error 'not-implemented :proc (list '(setf getenv) var)))

(defun finalize (obj func)
  "When OBJ is GCed, FUNC is called on it."
  #+allegro (excl:schedule-finalization obj func)
  #+clisp (#+lisp=cl ext:finalize #-lisp=cl lisp:finalize obj func)
  #+(or cmu scl) (ext:finalize obj func)
  #+cormanlisp (cl::register-finalization obj func)
  #+sbcl (sb-ext:finalize obj func)
  #-(or allegro clisp cmu cormanlisp sbcl scl)
  (error 'not-implemented :proc (list 'finalize obj func)))

(defun compiled-file-p (file-name)
  "Return T if the FILE-NAME is a filename designator for a valid compiled.
Signal an error when it is not a filename designator.
Return NIL when the file does not exist, or is not readable,
or does not contain valid compiled code."
  #+clisp
  (with-open-file (in file-name :direction :input :if-does-not-exist nil)
    (handler-bind ((error (lambda (c) (declare (ignore c))
                                  (return-from compiled-file-p nil))))
      (and in (char= #\( (peek-char nil in nil #\a))
           (let ((form (read in nil nil)))
             (and (consp form)
                  (eq (car form) 'SYSTEM::VERSION)
                  (null (eval form)))))))
  #-clisp t)

;;;
;;; Introspection
;;;

(defun variable-special-p (symbol)
  "Return T if the symbol names a global special variable."
  #+(and allegro (not (version>= 6))) (clos::variable-special-p symbol nil)
  #+(and allegro (version>= 6)) (excl::variable-special-p symbol nil)
  #+clisp (sys::special-variable-p symbol)
  #+cmu (walker:variable-globally-special-p symbol)
  #+gcl (si:specialp symbol)
  #+lispworks (eq :special (hcl:variable-information symbol))
  #+lucid (system:proclaimed-special-p symbol)
  #+sbcl (sb-walker:var-globally-special-p symbol)
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'variable-special-p symbol)))

(defun variable-not-special (symbol)
  "Undo the global special declaration.
This returns a _new_ symbol with the same name, package,
fdefinition, and plist as the argument.
This can be confused by imported symbols.
Also, (FUNCTION-LAMBDA-EXPRESSION (FDEFINITION NEW))
will return the OLD (uninterned!) symbol as its 3rd value.
BEWARE!"
  (let ((pack (symbol-package symbol)) var)
    (unintern symbol)
    (setq var (intern (symbol-name symbol) pack))
    (when (fboundp symbol) (setf (fdefinition var) (fdefinition symbol)))
    (setf (symbol-plist var) (symbol-plist symbol))
    var))

(defun arglist (fn)
  "Return the signature of the function."
  #+allegro (excl:arglist fn)
  #+clisp (sys::arglist fn)
  #+(or cmu scl)
  (let ((f (coerce fn 'function)))
    (typecase f
      (STANDARD-GENERIC-FUNCTION (pcl:generic-function-lambda-list f))
      (EVAL:INTERPRETED-FUNCTION (eval:interpreted-function-arglist f))
      (FUNCTION (values (read-from-string (kernel:%function-arglist f))))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase fn (symbol (fdefinition fn)) (t fn)))
  #+gcl (let ((fn (etypecase fn
                    (symbol fn)
                    (function (si:compiled-function-name fn)))))
          (get fn 'si:debug))
  #+lispworks (lw:function-lambda-list fn)
  #+lucid (lcl:arglist fn)
  #+sbcl (sb-introspect:function-arglist fn)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl scl)
  (error 'not-implemented :proc (list 'arglist fn)))

#+(and clisp (not mop))
(eval-when (compile load eval)
  (when (find-symbol "SLOT-DEFINITION-NAME" "CLOS")
    (pushnew :MOP *features*)))

;; implementations with MOP-ish CLOS
#+(or allegro clisp cmu cormanlisp lispworks lucid sbcl scl)
;; we use `macrolet' for speed - so please be careful about double evaluations
;; and mapping (you cannot map or funcall a macro, you know)
(macrolet ((class-slots* (class)
             #+allegro `(clos:class-slots ,class)
             #+clisp `(clos::class-slots ,class)
             #+cmu `(pcl::class-slots ,class)
             #+cormanlisp `(cl:class-slots ,class)
             #+lispworks `(hcl::class-slots ,class)
             #+lucid `(clos:class-slots ,class)
             #+sbcl `(sb-pcl::class-slots ,class)
             #+scl `(clos:class-slots ,class))
           (class-slots1 (obj)
             `(class-slots*
               (typecase ,obj
                 (class ,obj)
                 (symbol (find-class ,obj))
                 (t (class-of ,obj)))))
           (slot-name (slot)
             #+(and allegro (not (version>= 6))) `(clos::slotd-name ,slot)
             #+(and allegro (version>= 6)) `(clos:slot-definition-name ,slot)
             #+(and clisp (not mop)) `(clos::slotdef-name ,slot)
             #+(and clisp mop) `(clos::slot-definition-name ,slot)
             #+cmu `(slot-value ,slot 'pcl::name)
             #+cormanlisp `(getf ,slot :name)
             #+lispworks `(hcl::slot-definition-name ,slot)
             #+lucid `(clos:slot-definition-name ,slot)
             #+sbcl `(slot-value ,slot 'sb-pcl::name)
             #+scl `(clos:slot-definition-name ,slot))
           (slot-initargs (slot)
             #+(and allegro (not (version>= 6))) `(clos::slotd-initargs ,slot)
             #+(and allegro (version>= 6))
             `(clos:slot-definition-initargs ,slot)
             #+(and clisp (not mop)) `(clos::slotdef-initargs ,slot)
             #+(and clisp mop) `(clos::slot-definition-initargs ,slot)
             #+cmu `(slot-value ,slot 'pcl::initargs)
             #+cormanlisp `(getf ,slot :initargs)
             #+lispworks `(hcl::slot-definition-initargs ,slot)
             #+lucid `(clos:slot-definition-initargs ,slot)
             #+sbcl `(slot-value ,slot 'sb-pcl::initargs)
             #+scl `(clos:slot-definition-initargs ,slot))
           (slot-one-initarg (slot) `(car (slot-initargs ,slot)))
           (slot-alloc (slot)
             #+(and allegro (not (version>= 6)))
             `(clos::slotd-allocation ,slot)
             #+(and allegro (version>= 6))
             `(clos:slot-definition-allocation ,slot)
             #+(and clisp (not mop)) `(clos::slotdef-allocation ,slot)
             #+(and clisp mop) `(clos::slot-definition-allocation ,slot)
             #+cmu `(pcl::slot-definition-allocation ,slot)
             #+cormanlisp `(getf ,slot :allocation)
             #+lispworks `(hcl::slot-definition-allocation ,slot)
             #+lucid `(clos:slot-definition-allocation ,slot)
             #+sbcl `(sb-pcl::slot-definition-allocation ,slot)
             #+scl `(clos:slot-definition-allocation ,slot)))

(defun class-slot-list (class &optional (all t))
  "Return the list of slots of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
  (mapcan (if all (compose list slot-name)
              (lambda (slot)
                (when (eq (slot-alloc slot) :instance)
                  (list (slot-name slot)))))
          (class-slots1 class)))

(defun class-slot-initargs (class &optional (all t))
  "Return the list of initargs of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
initargs for all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
  (mapcan (if all (compose list slot-one-initarg)
              (lambda (slot)
                (when (eq (slot-alloc slot) :instance)
                  (list (slot-one-initarg slot)))))
          (class-slots1 class)))

(defun structure-slots (struct)
  "Return the list of structure slot names."
  #+clisp (mapcar #'clos:slot-definition-name (sys::structure-slots struct))
  #-clisp (class-slot-list (find-class struct)))

(defun structure-keyword-constructor (struct)
  "Return the structure keyword constructor name."
  #+clisp (sys::structure-kconstructor struct)
  #-clisp                       ; LAME!!!
  (intern (concatenate 'string "MAKE-" (symbol-string struct))
          (symbol-package struct)))

(defun structure-boa-constructors (struct)
  "Return the list of structure BOA constructor names."
  #+clisp (sys::structure-boa-constructors struct)
  #-clisp nil)                  ; what else?

(defun structure-copier (struct)
  "Return the structure copier name."
  #+clisp (sys::structure-copier struct)
  #-clisp                       ; LAME!!!
  (intern (concatenate 'string "COPY-" (symbol-string struct))
          (symbol-package struct)))

(defun structure-predicate (struct)
  "Return the structure predicate name."
  #+clisp (sys::structure-predicate struct)
  #-clisp                       ; LAME!!!
  (intern (concatenate 'string (symbol-string struct) "-P")
          (symbol-package struct)))

) ; macrolet

;;;
;;; CMUCL structure hack - make them externalizable
;;;

#+cmu
(eval-when (compile load eval)
  (shadow "DEFSTRUCT") (export (intern "DEFSTRUCT")))
#+cmu
(defmacro defstruct (name &rest slots)
  `(progn
     (eval-when (compile load eval) (cl:defstruct ,name ,@slots))
     ,(unless (and (consp name) (assoc :type (cdr name)))
       `(defmethod make-load-form ((self ,(if (consp name) (first name) name))
                                   &optional environment)
          (make-load-form-saving-slots self :environment environment)))))

;;;
;;; Environment
;;;

(defun sysinfo (&optional (out *standard-output*))
  "Print the current environment to a stream."
  (declare (stream out))
  (format out "~&~%~75~~%~75,,,'-:@<<{[ The current environment ]}>~>~%~
Implementation:~20t~a~%~7tversion:~20t~a~%Machine:  type:~20t~a
~7tversion:~20t~a~%~6tinstance:~20t~a~%Opeating System:~19t"
          (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) (machine-version) (machine-instance))
  #+(or (and clisp win32) (and allegro mswindows))
  (let* ((root-nt "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion")
         (root-9x "SOFTWARE\\Microsoft\\Windows\\CurrentVersion")
         (key-nt #+(and clisp win32) root-nt
                 #+(and allegro mswindows)
                 (ole:open-registry-key ole:rkey-local-machine root-nt))
         (key-9x #+(and clisp win32) root-9x
                 #+(and allegro mswindows)
                 (ole:open-registry-key ole:rkey-local-machine root-9x)))
    (labels ((9x-p ()
               #+(and clisp win32) (sys::registry key-9x "ProductName")
               #+(and allegro mswindows)
               (ole:registry-value key-9x "ProductName"))
             (env (str)
               #+(and clisp win32)
               (sys::registry (if (9x-p) key-9x key-nt) str)
               #+(and allegro mswindows)
               (ole:registry-value (if (9x-p) key-9x key-nt) str)))
      (if (9x-p)
          (format out " ~a (~a - ~a; boot: ~a)~%~5tRegistered to: ~a, ~a [~a]"
                  (env "ProductName") (env "Version") (env "VersionNumber")
                  (env "BootCount") (env "RegisteredOwner")
                  (env "RegisteredOrganization") (env "ProductId"))
          (format
           out " WinNT ~a (build ~a: ~a) ~a~%~5tRegistered to: ~a, ~a [~a]"
           (env "CurrentVersion") (env "CurrentBuildNUmber") (env "CSDVersion")
           (env "CurrentType") (env "RegisteredOwner")
           (env "RegisteredOrganization") (env "ProductId")))))
  #+os/2 (princ " OS/2")
  #+unix (princ " Unix")
  #+dos (princ " DOS")
  #+pc386 (princ " PC386")
  #+amiga (princ " Exec/AmigaDOS")
  (format out "~%Software: type:~20t~a~%~7tversion:~20t~a~%Site:~20t~a (~a)
User home:~20t~a~%Current directory:~20t~a~%Default pathname:~20t~a
Features:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.
Modules:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.~%Current package:~30t~a~%"
          (software-type) (software-version) (long-site-name)
          (short-site-name) (user-homedir-pathname) (default-directory)
          *default-pathname-defaults* *features* *modules* *package*)
  #+clisp (format out "[CLISP] Current language:~30t~a~%"
                  (sys::current-language))
  (flet ((exdi (fl) (integer-length (nth-value 1 (decode-float fl)))))
    (format out "Fixnum length:~25t~3d bits
Short Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Single Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Double Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Long Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~%"
            (integer-length most-positive-fixnum)
            (exdi most-positive-short-float)
            (float-digits most-positive-short-float)
            (exdi most-positive-single-float)
            (float-digits most-positive-single-float)
            (exdi most-positive-double-float)
            (float-digits most-positive-double-float)
            (exdi most-positive-long-float)
            (float-digits most-positive-long-float)))
  #+clisp (format out "[CLISP] long-float-digits:~44t~3d~%"
                  #+lisp=cl (ext:long-float-digits)
                  #-lisp=cl (lisp:long-float-digits))
  (dolist (sy '(array-total-size-limit array-rank-limit array-dimension-limit
                lambda-parameters-limit call-arguments-limit
                multiple-values-limit char-code-limit))
    (format out " ~a:~30t~15:d~%" sy (symbol-value sy)))
  (format out "lambda-list-keywords:~30t~{~<~%~30t~1,74:; ~a~>~}~%"
          lambda-list-keywords)
  (format out "Internal time unit:~25t~f sec~%*gensym-counter*:~25t~:d
Current time:~25t" (/ internal-time-units-per-second) *gensym-counter*)
  (current-time out) (format out "~%~75~~%") (room) (values))

;;;
;;; time & date
;;;

(defconst +month-names+ (simple-array simple-string (12))
  (mk-arr 'simple-string '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                           "Sep" "Oct" "Nov" "Dec"))
  "*The names of the months.")

(defconst +week-days+ (simple-array simple-string (7))
  (mk-arr 'simple-string '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  "*The names of the days of the week.")

(defconst +time-zones+ list
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" . "MST") (8 "PDT" . "PST")
    (0 "BST" . "GMT") (-2 "MET DST" . "MET"))
  "*The string representations of the time zones.")

(defun tz->string (tz dst &optional (long t))
  "Convert the CL timezone (rational [-24;24], multiple of 3600) to a string."
  (declare (type rational tz))
  (multiple-value-bind (hr mm) (floor (abs (- (if dst 1 0) tz)))
    (let ((mi (floor (* 60 mm)))
          (zo (assoc tz +time-zones+)))
      (format nil "~:[+~;-~]~2,'0d~:[:~;~]~2,'0d~@[ (~a)~]"
              (minusp tz) hr long mi
              (and long (if dst (cadr zo) (cddr zo)))))))

(defun string->tz (obj)
  "Find the OBJ (symbol or string) in +TIME-ZONES+."
  (find obj +time-zones+ :test
        (lambda (st el) (or (string-equal st (cadr el))
                            (string-equal st (cddr el))))))

(defun current-time (&optional (out t))
  "Print the current time to the stream (defaults to t)."
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (fixnum se mi ho da mo ye dw) (type rational tz))
    (format out "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a"
            ye mo da (aref +week-days+ dw) ho mi se
            (tz->string tz dst))))

(provide :port-sys)
;;; file sys.lisp ends here
