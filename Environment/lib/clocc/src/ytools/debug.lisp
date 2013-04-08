;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;;$Id: debug.lisp,v 2.12 2006/11/20 21:03:47 airfoyle Exp $

(depends-on %module/  ytools
	    :at-run-time %ytools/ nilscompat)

(self-compile-dep :macros)

(end-header)

(eval-when (:slurp-toplevel :load-toplevel)
   (export '(s sv ps ss dbg-stack* dbg-save st g gty package seek ev ev-ugly
	     get-frame-args
	     symshow =g htab-show file-show test check break-on-test-failure*
	     condition-display-string sym-val)))

(needed-by-macros

;; Each entry is of the form (flag form type), where flag is a symbol,
;; often *.  The idea is to make it easy to munge the forms further,
;; while debugging macros or for some other purpose.
(def-class Dbg-entry
   (:options (:medium :list))
   label
   object
   type)
)

(defvar dbg-stack* '())

;; Macro expand arg or arg applied to something on dbg-stack*
(defmacro s (&optional (form nil form-supp) (num 0) (flag '*))
   `(sv ,(cond (form-supp
                (cond ((atom form)
                       `(g ,form ,num))
                      (t
                       `',form)))
	       (t '(g *)))
	,flag))
	
;; Same, but always evaluates arg.
(defmacro sv (&optional (form '(g *)) (flag '*))
   `(dbg-push ',flag
	      (macroexpand-1 ,form)
	      'Sexp false))

;;;;	          ,(cond ((atom f) `(,f (g ,@stuff)))
;;;;				       (t `',f)))))



;;;;(defun sv (v &optional )
;;;;   (dbg-push '* (macroexpand-1 v) 'Sexp false))

;; evaluate form and push on stack if not atomic.
(defmacro ev (form &rest stuff)
   (let ((vl-var (gensym)) (ll-var (gensym)) (tl-var (gensym)))
      `(multi-let (((,vl-var ,ll-var ,tl-var)
		    (ev-process ',form ',stuff)))
          (list->values
             (list-dbg-push ,ll-var ,vl-var ,tl-var)))))

;; Like ev, but print nonprettily and don't put on stack
(defmacro ev-ugly (form)
   `(ev-process '(bind ((*print-pretty* false) (out ,form :%) (values)))
		'()))

;;; If * (value of last thing printed) is an Allegro-style function-call 
;;; display (i.e., (function-name val-of-arg-1 ... val-of-arg-N)),
;;; extract the args, give them the relevant names, and push them
;;; onto the debug-stack.  If a name is '_', the corresponding arg is
;;; skipped.  If there are fewer names than args, the excess args are
;;; not taken.
(defmacro get-frame-args (&rest names)
   `(progn
       ,@(repeat :for ((name :in names)
		       (i = 1 :by 1))
	  :when (not (eq name '_))
	  :collect `(ev (nth ,i *) ,name))
       ',names))

(needed-by-macros

;;; alist of special handlers
(defvar ev-process-tab* !())

(datafun attach-datafun ev-process
   (defun :^ (_ sym fname)
      (!= (alref ev-process-tab* sym)
	  (symbol-function fname))))

;;; Returns the form, perhaps tidied up some way, plus a list of
;;; labels for the values, plus a list of type designators,
;;; one per value the form will produce. 
(defun ev-process (form stuff)
   (let ((h (and (consp form) (alist-entry (car form) ev-process-tab*))))
      (cond (h
;;;;	     (out "Calling handler for " form " & " td :%)
	     (funcall h form stuff))
	    (t
;;;;             (cond ((null stuff)
;;;;                    (!= stuff '(*))))
             (let ((vals (values->list (eval (subst-with-stack-vars form)))))
                (let ((extra-labels (extra-ev-labels vals stuff)))
                   (cond ((not (null extra-labels))
                          (!= stuff `(,@*-* ,@extra-labels)))))

;;;;                (cond ((shorter stuff (len vals))
;;;;                       (!= stuff `(,@*-* ,@(<# (\\ (_) '_)
;;;;                                               (series (- (len vals)
;;;;                                                          (len stuff))))))))
                (values
                   vals
                   stuff
                   (<# (\\ (_) 'nil)
                       stuff)))))))

(defparameter base-extra-label* "*")
(defparameter extra-label-mark* "/")
(defparameter max-extra-label-marks* 3)

(defun extra-ev-labels (vals labels)
   (let ((num-vals (len vals))
         (num-labels (len labels)))
      (cond ((< num-labels num-vals)
             (let ((extra-label-list
                      (repeat :for ((i = 0 :to (- num-vals 1))
                                    (unary-label
                                        = base-extra-label*
                                        :then (string-concat unary-label
                                                             extra-label-mark*)))
                       :collect (tuple i
                                       (out-to-string
                                          (:a base-extra-label*)
                                          (:a extra-label-mark*)
                                          i)
                                       unary-label))))
                (let ((use-labels (nthcdr num-labels extra-label-list)))
                   (repeat :for ((trip :in use-labels))
                    :collect
                       (build-symbol
                          (:< (cond ((=< (first trip) max-extra-label-marks*)
                                     (third trip))
                                    (t (second trip)))))))))
            (t !()))))
;;; -- This could obviously be optimized, by moving the tests back into the loop
;;; and avoiding constructing the unused labels in the first place.  Not worth it.

(defvar absent-dbg-entry* (make-Dbg-entry nil "?" nil))

(defmacro get-and-pull-nth-dbg-entry (place^ sym^ n^)
   (with-gen-vars (n entry new-stack)
      `(let ((,n$ ,n^))
          (multi-let (((,entry$ ,new-stack$)
                       (nth-dbg-entry ,place^ ,sym^ ,n$)))
             (cond ((not (eq ,entry$ absent-dbg-entry*))
                    (!= ,place^
                        (cond ((= ,n$ 0)
                               (cons ,entry$ ,new-stack$))
                              (t ,new-stack$)))))
             ,entry$))))

(defvar stack-subst-exempt-vars* '(*))

(defun subst-with-stack-vars (exp)
   (cond ((and (is-Symbol exp)
	       (not (memq exp stack-subst-exempt-vars*)))
	  ;; '*' still means "just typed," not (g *), by default
	  (let ((ent (nth-dbg-entry dbg-stack* exp 0)))
	     (cond ((eq ent absent-dbg-entry*)
		    exp)
		   (t `(g ,exp)))))
	 ((atom exp) exp)
	 ((memq (car exp) '(g quote)) exp)
	 (t
	  (<# subst-with-stack-vars exp))))

(defvar dbg-stack-max-len* 200)

(defvar dbg-stack-trap-labels* '())

(defun list-dbg-push (labels vals types)
   (repeat :for ((lab :in labels)
                 (val :in vals)
                 (ty :in types))
    :within
       (cond ((not (member lab '(_ (_))
                           :test #'equal))
              (dbg-push (cond ((is-Symbol lab) lab)
                              (t (car lab)))
                        val ty)))
    :when (is-Symbol lab)
    :collect val))
       
;;;;    :when (not (eq lab '_))
;;;;    :collect val
;;;;      (dbg-push lab val ty)))

(defun dbg-push (label x &optional (type false) (even-trivia true))
   (cond ((or even-trivia
	      (not (or (is-Number x)
		       (not x)
		       (is-Symbol x))))
	  (cond ((memq label dbg-stack-trap-labels*)
		 (signal-problem dbg-push
		    "Saving under label: " label
		    t " Object: " x
		    (:continue "I'll go ahead and save it"))))
	  (push (make-Dbg-entry label x type) dbg-stack*)
	  (cond ((> (len dbg-stack*) dbg-stack-max-len*)
		 (let ((newlen
			  (signal-problem dbg-push
			     "Dbg-stack exceeds " dbg-stack-max-len*
			     " elements: "
			     (<# Dbg-entry-label dbg-stack*)
			     (:prompt-for "New stack length"
                                          " (default: no change): "
					  dbg-stack-max-len*))))
		    (cond ((> newlen (len dbg-stack*))
			   (!= dbg-stack-max-len* newlen))
			  (t
			   (ps (- (len dbg-stack*) newlen)))))))))
   (values x type))
 
)

;;;; dbg-save prints a message on *query-io* when it is expanded, so
;;;; the user doesn't forget the call to dbg-save is there.
;;;; If dbg-save-msg-postpone* is non-false, we wait and print the message
;;;; when the expanded code runs.
;;;;(needed-by-macros
;;;;(defvar dbg-save-msg-postpone* false)
;;;;)

;; (dbg-save [(:package p)] <announce> (v1 e1 [t1]) (v2 e2 [t2]) ...) 
;;   puts entries flagged vI with value
;;   eI and type tI on stack.  
;; (v v false) may be abbreviated v.  Each vI is reinterned
;; in package p, if supplied, else in the package in effect when dbg-save 
;; is executed.  (Big convenience during debugging.)
;; Generally the programmer wants to be reminded about an occurrence of 
;; dbg-save, either when it is expanded (typically at compile time), when 
;; its expansion is executed, or both.
;; The <announce> fields determine when the reminder is produced.
;; Their format is
;;    [| :comp-quiet | :comp-loud]   [| :run-quiet | :run-loud]
;; :run-loud is useful for reminding you of what's been saved on the dbg-stack
;; just before a breakpoint.
;; :comp-loud is useful for reminding you that there are stray debugging
;; statements in the program, that might be wasting space by growing the
;; dbg-stack for no reason.
;; Defaults: If nothing is supplied, the default is :comp-loud :run-quiet.
;; If just one is supplied, the other defaults to the opposite.
;; (:silent is a synonym for :comp-quiet.)
(defmacro dbg-save (&rest vars-n-vals)
   (multi-let (((pkg-name comp-loud run-loud real-vars-n-vals)
		(dbg-save-analyze vars-n-vals)))
      (let ((pkg-exp
	       `(maybe-package
		   ,(cond (pkg-name `',pkg-name)
			  (t 'false)))))
	 (cond (comp-loud 
		(out (:to *error-output*) "Dbg-save: " real-vars-n-vals :%)))
	 `(progn
	     ,@(include-if run-loud
		  `(out (:to *query-io*)
		      "Dbg-saving: "
		      (list ,@(<# (\\ (vv)
				     `(intern
					 ',(symbol-name
					      (dbg-save-var-val-analyze
						 vv))
					 ,pkg-exp))
				  real-vars-n-vals))
		      t))
	     ,@(repeat :for ((l :in real-vars-n-vals))
		:collect (multi-let (((var val type)
				      (dbg-save-var-val-analyze l)))
			    (!= var `(intern ',(symbol-name var)
					     ,pkg-exp))
			    `(dbg-push ,var ,val ,(cond (type `',type)
							(t 'false)))))))))

(needed-by-macros

(defun dbg-save-analyze (vars-n-vals)
   (let ((pkg-name false)
	 (real-vars-n-vals '())
	 (comp-explicit false)
	 (comp-loud true)
	 (run-explicit false)
	 (run-loud false))
      (repeat :for ((vv :in vars-n-vals))
       :result (!= real-vars-n-vals (nreverse *-*))
         (match-cond vv
	    ?( ?(:\| :comp-loud :comp-quiet :silent)
	      (cond (comp-explicit
		     (out (to *error-output*)
			"More than one :comp-X instruction in "
			`(dbg-save ,@vars-n-vals)
			t "  Ignoring all but first" t))
		    (t
		     (!= comp-explicit true)
		     (!= comp-loud (eq vv ':comp-loud)))))
	    ?( ?(:\| :run-loud :run-quiet)
	      (cond (run-explicit
		     (out (to *error-output*)
			"More than one :run-X instruction in "
			`(dbg-save ,@vars-n-vals)
			t "  Ignoring all but first" t))
		    (t
		     (!= run-explicit true)
		     (!= run-loud (eq vv ':run-loud)))))
	    ?( (?(:\| :package package) ?pkg)
	      (!= pkg-name pkg))
	    (t 
	     (!= real-vars-n-vals (cons vv *-*)))))
      (cond (comp-explicit
	     (cond ((not run-explicit)
		    (!= run-loud (not comp-loud)))))
	    (run-explicit
	     (!= comp-loud (not run-loud))))
      (values pkg-name comp-loud run-loud real-vars-n-vals)))

(defun dbg-save-var-val-analyze (l)
   (cond ((atom l) (values l l false))
	 (t
	  (values (car l)
		  (cadr l)
		  (cond ((cddr l) (caddr l))
			(t false))))))

)

(defun maybe-package (pkg-name)
   (let ((pkg (cond (pkg-name (find-package pkg-name))
		    (t *package*))))
      (cond (pkg pkg)
	    (t
	     (out (to *error-output*) "dbg-save can't find package " pkg-name
		  ", using " *package* t)
	     *package*))))

;; Get the nth entry on the stack associated with flag sym.
(defmacro g (&optional (sym '*) (n 0))
   (cond ((and (numberp sym)
	       (= n 0))
	  (!= n sym)
	  (!= sym '*)))
   `(Dbg-entry-object (get-and-pull-nth-dbg-entry dbg-stack* ',sym ,n)))

;; Get the type of the nth entry
(defmacro gty (&optional (sym '*) (n 0))
   (cond ((and (numberp sym)
	       (= n 0))
	  (!= n sym)
	  (!= sym '*)))
   `(Dbg-entry-type (get-and-pull-nth-dbg-entry dbg-stack* ',sym ,n)))

;;; Arguments are screwy, because in reality everything is optional
;;; except val.
;;; If all three arguments are supplied, this _changes_ an existing
;;; dbg-entry (which must exist).
;;; Otherwise, it's just a synonym for 'ev', but useful when
;;; we want to supply a place.
(defmacro set-g (&optional (sym '* sym-supplied)
                           (n 0 n-supplied)
                           (val false val-supplied))
   (cond ((not sym-supplied)
          (signal-problem set-g
             "'set-g' given no arguments")))
   (cond (val-supplied
          ;; Change an existing place
          `(set-dbg-entry-object ',sym ',n ,val))
         (n-supplied
          ;; "'n'" is really val'
          (cond ((not (is-Symbol sym))
                 (signal-problem set-g
                    "First argument to set-g must be a symbol, not: " sym)))
          `(ev ,n ,sym))
         (t
          ;; "'sym'" is really 'val'
          `(ev ,sym))))

(defsetf g set-g)
;;; This doesn't work because 'sym' gets evaluated --
;;;;(defsetf g (&optional (sym '*) (n 0)) (val)
;;;;   `(set-dbg-entry-object ',sym ',n ,val))

(defun set-dbg-entry-object (sym n val)
   (let ((e (get-and-pull-nth-dbg-entry dbg-stack* sym n)))
      (cond ((eq e absent-dbg-entry*)
	     "?")
	    (t
	     (!= (Dbg-entry-object e) val)))))

;;; Returns two values:
;;;    the debug entry found (with the given 'sym' and index 'n')
;;;    + a new version of 'a' with that entry _removed_ if n=0,
;;;      otherwise pulled as far forward as
;;;      possible without changing its index (vs. other entries
;;       with same 'sym')
;;; If none found, returns absent-dbg-entry* + original 'a' 
(defun nth-dbg-entry (a sym n)
   (let-fun ()
      (cond ((null a)
             (values absent-dbg-entry* !()))
            ((eq (Dbg-entry-label (head a)) sym)
             (cond ((= n 0)
                    (values (head a) (tail a)))
                   (t
                    (multi-let (((e s)
                                 (nth-dbg-entry (cdr a) sym (- n 1))))
                       (values e
                               (cond ((eq e absent-dbg-entry*)
                                      a)
                                     (t
                                      (cons (head a)
                                            (cond ((= n 1)
                                                   (cons e s))
                                                  (t s))))))))))
            (t
             (multi-let (((e s)
                          (nth-dbg-entry (cdr a) sym n)))
                (values e
                        (cond ((eq e absent-dbg-entry*)
                               a)
                              (t
                               (cons (head a)
                                     s)))))))
;;;;    :where
;;;;      (:def result (e s)
;;;;         )
    ))

(defvar dbg-stack-show* 3)

(defmacro =g (newlabel &optional (oldlabel '*) (n 0))
   `(dbg-entry-label-change ',oldlabel ,n ',newlabel))

(defun dbg-entry-label-change (oldlabel n newlabel)
  (let ((e (nth-dbg-entry dbg-stack* oldlabel n)))
     (cond ((eq e absent-dbg-entry*)
	    "?")
	   (t (!= (Dbg-entry-label e) newlabel)))))

;; Pop n things off *end* of dbg-stack and display prefix of result.
;; If n negative, pop off beginning
(defun ps (&optional (n 0) (display dbg-stack-show*))
   (!= dbg-stack* (cond ((> (abs n) (length dbg-stack*))
			 '())
			((< n 0)
			 (nthcdr (- n) *-*))
			(t
			 (drop (- n) *-*))))
   (format t "~s~%" (<# (\\ (e) `(,(Dbg-entry-label e)
				  ,(condense (Dbg-entry-object e))
				  ,(Dbg-entry-type e)))
			(take (min display (len dbg-stack*))
			      dbg-stack*)))
;;;   (bind ((*print-level* dbg-stack-show*)
;;;	  (*print-length* dbg-stack-show*)
;;;	  (*print-pretty* true))
;;;      (format t "~s~%" (cdr dbg-stack*)))
   (length dbg-stack*))

;; Display n things on top of stack
(defun ss (n)
  (bind ((dbg-stack-show* n))
     (ps 0)))

;; Display nth element of dbg-stack.
(defun st (&optional (n 0))
   (elt dbg-stack* n))

;; !^pkg sym returns pkg::sym, but imports sym to current package.
;; !^^ means "shadowing import."
(def-excl-dispatch #\^ (srm _)
   (let ((shadow
	    (cond ((char= (peek-char false srm)
			  #\^)
		   (read-char srm)
		   true)
		  (t false))))
       (let* ((pkgname (read srm))
              (pkg (and (is-Symbol pkgname)
                        (find-package (Symbol-name pkgname)))))
          (cond (pkg
                 (let ((x (let ((*package* pkg))
                             (read srm))))
                    (cond ((is-Symbol x)
                           (cond ((eq (find-symbol (symbol-name x)
                                                   *package*)
                                      x)
                                  (out x " already in " :% 3 *package* :%))
                                 (t
                                  (out "Importing " x " into " :% 3 *package*
                                       :%)
                                  (cond (shadow
                                         (shadowing-import x))
                                        (t
                                         (import x))))))
                          (t
                           (out x " is not a symbol" :%)))
                    x))
                (t
                 (out pkgname " is not the name of a package" :%))))))

;;; Push first subform of exp beginning with sym onto dbg-stack*
(defun seek (sym &optional (num 0) (exp (g *)) (label '*))
   (let-fun ((sk (e)
	       (cond ((atom e) false)
		     ((eq (car e) sym)
		      (cond ((= num 0)
			     e)
			    (t
			     (!= num (- num 1))
			     (skl (cdr e)))))
		     (t
		      (skl e))))
	    (skl (e)
	       (repeat :for (x)
		  (!= x (sk (car e)))
		:until x
		:result x
		  (!= e (cdr e))
		:until (atom e)
		:result false)))
      (let ((sub (sk exp)))
	 (cond (sub (dbg-push label sub))))))
				 
(defun symshow (str &optional (pkg1 *package*) (pkg2 *package*))
   (cond ((is-Symbol str)
	  (!= str (symbol-name str))))
   (cond ((eq pkg1 pkg2)
	  (!= pkg2 false)))
   (multiple-value-let (sym status)
		       (find-symbol str pkg1)
      (cond (sym
	     (let ((sympkg (symbol-package sym)))
		(out sym " home " sympkg)
		(let-fun ((also-show (pkg)
			    (multiple-value-let (sym-in-pkg stat)
						(find-symbol str pkg)
			       (cond (sym-in-pkg
				      (cond ((eq sym-in-pkg sym)
					     (out "  Also visible in "
						  pkg " [" stat "]" t))
					    (t
					     (out "  Shadowed in "
						  pkg " [" stat "]" t))))
				     (t
				      (out "  Not visible in " pkg))))))
		   (cond ((eq sympkg (find-package pkg1))
			  (out "[" status "]" t))
			 (sympkg
			  (multiple-value-let (_ stat)
					      (find-symbol str sympkg)
			     (out "[" stat "]" t)
			     (also-show pkg1))))
		   (cond ((and pkg2 (not (eq sympkg (find-package pkg2))))
			  (also-show pkg2))))))
	    (t
	     (out "No such symbol in " pkg1 t)))))

(defun sym-val (str &key (pkg *package*) (lev *print-level*))
   (cond ((is-Symbol str)
          (!= str (Symbol-name str))))
   (let ((sym (find-symbol str pkg)))
      (cond (sym
             (cond ((boundp sym)
                    (bind ((*print-level* lev))
                       (out "Value: " (Symbol-value sym) :%)))
                   (t
                    (out "Symbol exists; no value" :%))))
            (t
             (out "No such symbol" :%)))
      (values)))

(defun htab-show (htab)
   (with-hash-table-iterator (ht-iter htab)
      (repeat :for ((i = 0 :by 1))
       :within
	 (multiple-value-let (found key value)
			     (ht-iter)
	    (:continue
	     :while found
	     (out key " -- " (condense value) :%)
	     :result (out i " entries" :%)))))
  htab)

(defun file-show (filespecs)
   (with-open-file (srm (car (filespecs->pathnames filespecs)) :direction ':input)
      (repeat :for ((r = (in (:from srm) :obj)
		       :then :again))
       :until (eq r eof*)
          (out (:to *query-io*) r :% :%))))

(define-condition Test-failure (error)
   ((description :reader Test-failure-description
		 :initarg :description))
   (:report (lambda (tf srm)
	       (out (:to srm) "TEST FAILURE "
                    :% (Test-failure-description tf)))))

(defvar break-on-test-failure* true)

(defvar check-count*)

;;; (test id-string  -forms-)
;;; The forms include calls to the 'check' macro, below.
;;; Returns true if all calls to 'check' threw no Test-failures (and no
;;; one else did either).
;;; Optional keyword arg :end-string supplies string to print when
;;; test is finished.
(defmacro test (string &rest body)
   (multi-let (((body kal)
                (keyword-args-extract body '(:end-string))))
      ;; Historically the string came before the test.
      ;; So if string can't come right after the word "test",
      ;; pad it out --
      (cond ((not (= (string-length string) 0))
             (let ((ch1 (elt string 0)))
                (cond ((and (not (is-whitespace ch1))
                            (not (member ch1 '(#\: #\/) :test #'char=)))
                       (!= string (string-concat " " *-*)))))))
      (let ( ;;;; (block-label (gensym))
            (end-string
               (alref kal ':end-string "")))
         `(catch 'test-abort
             (let ((check-count* 0))
                (handler-bind ((Test-failure
                                  (\\ (tf)
                                     (cond ((not break-on-test-failure*)
                                            ;; We have to do this because
                                            ;; the :report clause doesn't seem
                                            ;; to work right --
                                            (bind ((*print-escape* false))
                                               (out (:to *error-output*)
                                                  "TEST FAILURE" :%
                                                  (Test-failure-description tf)
                                                  :% " *** " ,string " test FAILED ***" :%))
   ;;;;					 (return-from ,block-label false)
                                            (throw 'test-abort false)
                                            )))))
                     (progn (out (:to *error-output*) "Beginning test" ,string :%)
                            ,@body
                            (out (:to *error-output*)
                               5 "Test succeeded"
                               ,(cond ((eq end-string ':same)
                                       string)
                                      (t end-string))
                                :%)
                            true)))))))

;;; (check form -out-stuff-) runs form.  If it returns false, 'out' the
;;; out-stuff and break.
;;; If 'break-on-test-failure*' is true, then a description of the error
;;; is printed and a break-loop is entered.  If the user returns from it then
;;; the test goes on as if the check did not fail.  If the user executes
;;; (throw 'test-abort [true|false]) then the innermost call to 'test'
;;; returns with the given value.
(defmacro check (form &rest msgstuff)
   (!= msgstuff (remove ':else *-*))
   (cond ((not (null msgstuff))
	  (!= msgstuff `(": " ,@*-*))))
   (let ((srmvar (gensym)))
      `(progn
	  (!= check-count* (+ *-* 1))
;;;;	  (out "check-count* = " check-count* :%)
	  (cond ((not ,form)
		 (let ((cc check-count*))
		    (signal-problem :noplace
		       :class Test-failure
		       :description
			   (make-Printable
			       (\\ (,srmvar)
				  (out (:to ,srmvar)
				      1 cc ,@msgstuff)))
		       :proceed)))))))

(define-out-operator (:val= cmd stream)
   (let ((format-control-string
            (out-to-string
                (:e (repeat :for ((sym :in (cdr cmd)))
                       (:o sym " = ~s "))))))
      `(format (out-prepare ,stream)
          ,format-control-string
          ,@(cdr cmd))))

#+allegro
(defun condition-display-string (c)
   (with-output-to-string (srm)
      (cond ((forall (s :in '(excl::format-control excl::format-arguments))
		(and (slot-exists-p c s)
		     (slot-boundp c s)))
	     (apply #'format srm (slot-value c 'excl::format-control)
			     (slot-value c 'excl::format-arguments)))
	    (t
	     (out (:to srm) "Undisplayable " (:a c) :%)))))
