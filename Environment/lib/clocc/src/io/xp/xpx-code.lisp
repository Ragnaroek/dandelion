;-*-syntax:COMMON-LISP-*-

(in-package "USER")

;This is the January 13, 1992 version of Richard C. Waters' example
;showing the use of his XP pretty printer.

#|----------------------------------------------------------------------------|
 | Copyright 1992 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

;The functions in this file make use of XP, which is documented in
;Chapter 27 of Common Lisp: the Language Second Edition, Guy L. Steele
;Jr, Digital press, 1990, and in even greater detail in MIT/AIM-1102a,
;July 1989.

(defvar *PD* (copy-pprint-dispatch))
(proclaim '(special *B*))

(defun pascal-write (sexpr &rest args)
  (let ((*B* 0))
    (apply #'write sexpr :pretty t
           :pprint-dispatch *PD* args)))

(defun pr-string (s string)
  (setq string (string string))
  (write-char #\' s)
  (dotimes (i (length string))
    (let ((char (aref string i)))
      (write-char char s)
      (when (char= char #\')
        (write-char #\' s))))
  (write-char #\' s))

(set-pprint-dispatch 'string
  #'pr-string 0 *PD*)

(set-pprint-dispatch 'character
  #'pr-string 0 *PD*)

(set-pprint-dispatch 'symbol
  #'(lambda (s id)
      (write-string
        (remove #\-
          (string-capitalize
           (string id)))
        s))
  0 *PD*)

(set-pprint-dispatch '(and rational (not integer))
  #'(lambda (s n) 
      (write (float n) :stream s))
  0 *PD*) 

(defvar *unary*
  '((+ "+" 2) (- "-" 2) (not "not " 4)))

(defun unary-p (x)
  (and (consp x)
       (assoc (car x) *unary*)
       (= (length x) 2)))

(set-pprint-dispatch '(satisfies unary-p)
  #'(lambda (s list)
      (let* ((info (cdr (assoc (car list)
                               *unary*)))
             (nest (<= (cadr info) *B*))
             (*B* (cadr info)))
        (when nest (write-char #\( s))
        (write-string (car info) s)
        (write (cadr list) :stream s)
        (when nest (write-char #\) s))))
  0 *PD*)

(defvar *builtin*
  '((atan "ArcTan") (code-char "Chr")
    (log "Ln") (oddp "Odd")
    (char-code "Ord") (truncate "Trunc")
    (prin1 "Write") (terpri "Writeln")))

(defun builtin-p (x)
  (and (consp x)
       (assoc (car x) *builtin*)))

(defun pr-arglist (s args)
  (when args
    (let ((*B* 0))
      (format s #"~:<~@{~W~^, ~_~}~:>"
              args))))

(set-pprint-dispatch '(satisfies builtin-p)
  #'(lambda (s list)
      (write-string
        (cadr (assoc (car list)
                     *builtin*))
        s)
      (pr-arglist s (cdr list)))
  0 *PD*)

(defvar *bin*
  '((* "*" 3) (/ "/" 3)
    (mod " mod " 3)
    (round " div " 3)
    (and " and " 3)
    (+ "+" 2) (- "-" 2)
    (or " or " 2)
    (= " = " 1)
    (< " < " 1) (> " > " 1)
    (/= " <> " 1) (<= " <= " 1)
    (>= " >= " 1) (eq " = " 1)
    (eql " = " 1) (equal " = " 1)))

(defun bin-p (x)
  (and (consp x)
       (assoc (car x) *bin*)
       (= (length x) 3)))

(set-pprint-dispatch
  '(satisfies bin-p)
  #'(lambda (s list)
      (let* ((info (cdr (assoc (car list)
                               *bin*)))
             (nest (<= (cadr info) *B*)))
        (pprint-logical-block
            (s (cdr list)
               :prefix (if nest "(" "")
               :suffix (if nest ")" ""))
          (let ((*B* (1- (cadr info))))
            (write (pprint-pop)
                   :stream s))
          (pprint-newline :linear s)
          (write-string (car info) s)
          (let ((*B* (cadr info)))
            (write (pprint-pop)
                   :stream s)))))
      0 *PD*)

(set-pprint-dispatch 'cons
  #'(lambda (s list)
      (write (car list) :stream s)
      (pr-arglist s (cdr list)))
  -1 *PD*)

(set-pprint-dispatch
  '(cons (member setq))
  #"~<~*~1@{~W :=~_ ~W~}~:>"
  0 *PD*)

(set-pprint-dispatch
  '(cons (member progn))
  #"~<~*~1@{begin ~2i~_~@{~W~^; ~_~} ~
    ~I~_end~}~:>"
  0 *PD*)

(defun pr-if (s list)
  (let ((then (caddr list))
        (else (cadddr list)))
    (when
      (and else (consp then)
           (or (member (car then)
                       '(when unless))
               (and (eq (car then) 'if)
                    (null (cdddr then)))))
      (setq then `(progn ,then)))
    (format s #"~@<if ~W ~i~:_~3I~
                   then ~_~W~@[ ~I~_~3I~
                   else ~_~W~]~:>"
            (cadr list) then else)))

(set-pprint-dispatch 
  '(cons (member if))
  #'pr-if 0 *PD*)

(defun maybe-progn (list)
  (if (cdr list)
      `(progn ., list)
      (car list)))

(set-pprint-dispatch
  '(cons (member when)) 
  #'(lambda (s list)
      (pr-if s `(if ,(cadr list)
                    ,(maybe-progn
                       (cddr list)))))
  0 *PD*)

(set-pprint-dispatch
  '(cons (member unless))
  #'(lambda (s list)
      (pr-if s `(if (not ,(cadr list))
                    ,(maybe-progn
                       (cddr list)))))
  0 *PD*)

(defun while-loop-p (x)
  (and (consp x) (eq (car x) 'loop)
       (exit-p (cadr x))))

(defun exit-p (x)
  (and (consp x)
       (member (car x) '(if when))
       (equal (cddr x) '((return nil)))))

(set-pprint-dispatch
  '(satisfies while-loop-p)
  #'(lambda (s list)
      (format s "~@<while ~W ~
                 ~:_do ~2I~_~W~:>"
              `(not ,(cadadr list))
              (maybe-progn (cddr list))))
  0 *PD*)

(defun repeat-loop-p (x)
  (and (consp x) (eq (car x) 'loop)
       (exit-p (car (last x)))))

(set-pprint-dispatch
  '(satisfies repeat-loop-p)
  #'(lambda (s list)
      (format s "~@<~<repeat ~2I~
                 ~@{~^~_~W~^; ~}~:> ~I~_~
                 until ~W~:>"
              (butlast (cdr list))
              (cadar (last list))))
  0 *PD*)

(proclaim '(special *decls*))

(defun pr-decl (s var &rest ignore)
  (declare (ignore ignore))
  (format s #"~W: ~W"
          var (declared-type var)))

(defun declared-type (var)
  (cdr (assoc
         (dolist (d *decls* 'integer)
           (when (member var (cdr d))
             (return (car d))))
         `((float . real)
           (single-float . real)
           (integer . integer)
           (fixnum . integer)
           (character . char)
           (string-char . char)))))

(defun pr-defun (s list)
  (let* ((name (cadr list))
         (args (caddr list))
         (body (cdddr list))
         (*decls* nil)
         (fn? (and (member name args)
                   (eq name
                       (car (last body)))))
         (locals
           (delete name
             (cdr (member '&aux args))))
         (parameters
           (ldiff args
             (member '&aux args)))
         (*B* 0))
   (loop
     (unless (eq (caar body) 'declare)
       (return nil))
     (setq *decls*
           (append *decls* (cdar body)))
     (pop body))
   (pprint-logical-block (s (cdr list))
     (write-string
       (if fn? "function " "procedure ")
       s)
     (write name :stream s)
     (format s #" ~:<~@{~/pr-decl/~^~
                        ; ~:_~}~:>"
             parameters)
     (when fn?
       (format s #": ~W"
               (declared-type name)))
     (format s #";~:@_")
     (when locals
       (format s #"  var ~4I~
                  ~{~:@_~/pr-decl/;~}~
                  ~0I~:@_"
               locals))
     (format s #"begin ~2i~:@_~{~W~^; ~_~}~
                 ~I~_end"
             (if fn?
                 (butlast body)
                 body)))))

(set-pprint-dispatch
  '(cons (member defun))
  #'pr-defun 0 *PD*)

#|----------------------------------------------------------------------------|
 | Copyright 1992 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#
