;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: object.lisp,v 2.1 2005/12/26 00:25:17 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

;;;;(depends-on :always %ytools/ misc)

(depends-on %ytools/ mapper repeat)

(depends-on :at-run-time %ytools/ outin setter)

(end-header)   ;;;; :continue-slurping

;;; We can't use signal-problem in this file, because it requires
;;; object.

(eval-when (:compile-toplevel :load-toplevel)

   (export '(def-class def-op slot-defaults
	     make-inst def-meth
	     continue-combined-method
	     slot slot-is-filled with-slots-from-class
	     initialize +unbound-slot-val+
	     max-no-key-slots* max-no-key-classes*)))

;;;;(eval-when (:slurp-toplevel)
;;;;   (fslurp %ytools/ setter))

(defstruct (YTools-class-descriptor (:conc-name ytd-))
   medium
   components
   slots
   key-cons    ;; boolean
   key-conser  ;; function name
;;;;   handler-fn   ;;; obsolete
   initforms)

;;; It's really a constant, but cmucl issues an error after
;;; slurp  evaluates it and then compile evaluates it again.
(defvar +unbound-slot-val+
        (make-Printable (\\ (srm) (format srm "<Unbound slot>"))))

(needed-by-macros

(defun declare-ytools-class (name kind components slots initforms
				  key-cons key-conser)
   (!= (get name 'ytools-class-descriptor)
       (make-YTools-class-descriptor
	  :medium kind :components components :slots slots :initforms initforms
	  :key-conser key-conser :key-cons key-cons))
   (let ((unclear (<? (\\ (c) (or (not (atom c))
				  (not (get c 'ytools-class-descriptor))
				  (not (eq (ytd-medium
					      (get c 'ytools-class-descriptor))
					   kind))))
		      components)))
      (cond ((not (null unclear))
	     (cerror "I will ignore them"
	       "YTools class ~s has bogus :INCLUDEs: ~s"
	       name unclear)))))

(defun get-ytools-class-descriptor (classname)
   (get classname 'ytools-class-descriptor))
)

;;; The maximum number of slots you can use in no-key mode without getting
;;; a (continuable) error
(defvar max-no-key-slots* 10)
;;; The maximum number of classes that can be involved in no-key mode without
;;; getting an error
(defvar max-no-key-classes* 2)

;;; (def-class <name>
;;;    [(:options --opts--)]
;;;    --slots--
;;;   [(:handler --clauses--]) 
;;; defines a structure or object type with an optional
;;; handler.  
;;; Handler is flagged by (:HANDLER -clauses-).
;;; Each slot is in 'defstruct' form

(defmacro def-class (name &rest slots)
   (let ((clauses '())
	 (options '())
         (h (member-if (\\ (x) (car-eq x ':handler))
		       slots))
	 (opts (member-if (\\ (x) (car-eq x ':options))
			  slots)))
      (cond (h
	     (!= clauses (cdar h))
	     (!= slots (remove (car h) *-* :count 1)))
	    (t
	     (!= clauses '())))
      (cond (opts
	     (!= options (cdar opts))
	     (!= slots (remove (car opts) *-* :count 1)))
	    (t
	     (!= options '())))
      (repeat :for ((s :in slots))
         (cond ((cond ((atom s)
		       (or (not (is-Symbol s))
			   (not s)))
		      (t (not (is-Symbol (car s)))))
		(error "In def-class for ~s, illegal slot ~s"
		       name s))))
      (cond ((null options)
	     (struct-defclass :name name :storage-class ':structure
			      :is-testable true :already-defined false
			      :component false :key-cons false
			      :clauses clauses :slots slots))
	    (t
	     (complex-defclass name options clauses slots))   )))

;;; "complex" just means the parsing task is complex; the result could
;;; still be a :structure .
(defun complex-defclass (name options clauses slots)
   (multiple-value-let (key-cons options)
		       (cond ((memq ':key options)
			      (values true (remove ':key options)))
			     ((memq ':nokey options)
			      (values ':nokey (remove ':nokey options)))
			     (t
			      (values false options)))
      (repeat :for ((opt :in options))
	 (cond ((or (atom opt)
		    (not (memq (car opt)
			       '(kind medium slotmethods include
				 :kind :medium :slotmethods :include))))
		(cerror "I will ignore it"
			"Meaningless 'def-class' option: ~s" opt))))
      (let ((kind (member-if (\\ (a)
				(and (consp a)
				     (memq (car a) '(kind :kind medium :medium))))
			     options))
	    (components (member-if (\\ (a) (memq (car a) '(include :include))   )
				   options))
	    (slotmethods (member-if (\\ (a) (memq (car a)
						  '(slotmethods :slotmethods))   )
				    options))
	    (is-testable true)
            (already-defined false))
	 (cond (components
		(!= components (cdar *-*))))
	 (cond (slotmethods
		(!= slotmethods (cdar *-*))))
	 ;(out "kind = " kind :%)
	 (cond (kind
		(!= < kind is-testable already-defined >
		    (class-kind-parse (cdar kind) name)))
	       (t
		(!= is-testable true)
		(!= already-defined false)))
	 (!= components
	     (repeat :for ((comp :in components)
			   :collector good-ones)
	      :within
	         (cond ((and (is-Symbol comp)
			     (get-ytools-class-descriptor comp))
			(:continue :collect comp))
		       ((not (is-Symbol comp))
			(cerror "I will omit it"
				"Illegal YTools class component: ~s in class ~s"
				comp name))
		       (t
			(cerror "I will omit it"
				"I will try to proceed with incomplete information"
				"Component not declared as YTools class: ~s in class ~s"
				comp name)))))
	 (let ((component-descriptors
		  (<# get-ytools-class-descriptor components)))
	    (cond ((not (null component-descriptors))
		   (let ((cda (car component-descriptors))
			 (cdd (cdr component-descriptors)))
		      (cond ((not kind)
			     (!= kind (ytd-medium cda)))
			    ((not (eq kind (ytd-medium cda)))
			     (error "Stated medium ~s for class ~s differs from medium of components ~s"
				    kind name (<# ytd-medium component-descriptors))))
		      (cond ((not (or (null cdd)
				      (eq kind ':object)))
			     (error "Class ~s with medium ~s has multiple components ~s"
				    name kind components)))
		      (repeat :for ((cd :in cdd)
				    (compname :in (cdr components)))
			 (cond ((not (eq (ytd-medium cd)
					 kind))
				(error "Class ~s with medium ~s has component ~s with medium ~s"
				       name kind compname (ytd-medium cd))))))))
	    (cond ((not kind)
		   (!= kind ':structure)))
	    (cond ((eq kind ':object)
		   (clos-defclass
		      name already-defined
		      components slotmethods key-cons clauses slots))
		  (t
		   (struct-defclass
		       :name name :storage-class kind
		       :is-testable is-testable
		       :already-defined already-defined
		       :component (and (not (null components))
				       (car components))
		       :slotmethods slotmethods
		       :key-cons key-cons
		       :clauses clauses :slots slots)))))))

(defun class-kind-parse (l classname)
   (let ((storage-class false)
         (explicit-testability false)
         is-testable
         (already-defined false))
      (repeat :for ((x :in l))
         (cond ((memq x '(:already-defined :built-in built-in))
		(!= already-defined true))
	       ((memq x '(:ordinary :nondescript :as-shown :slots-only :named))
                (!= explicit-testability true)
		(!= is-testable (eq x ':named)))
	       ((memq x '(:structure :object :list :vector))
		(cond (storage-class
		       (error "I'll ignore all but the first"
			      !"'def-class' medium specified more than once ~
                                 in: ~s for class ~s"
                              l classname))
		      (t
		       (!= storage-class x))))
	       (t
		(cerror "I'll ignore it"
		   "Illegal :medium in def-class: ~s for class ~s"
		   x classname))))
      (cond (explicit-testability
             (cond ((memq storage-class '(:structure :object))
                    (cond ((not is-testable)
                           (cerror "I'll ignore it"
                                   "Attempt to declare ~s ~s :slots-only"
                                   storage-class classname)
                           (!= is-testable true))))))
            (t
             (!= is-testable true)
;;; This would be the built-in Lisp behavior, but why should we have to
;;; remember different defaults for different cases? --
;;;;                 (memq storage-class '(:structure :object))
                 ))
      (values storage-class is-testable already-defined)))

;;;;(datafun to-slurp declare-ytools-class #'slurp-eval)

(declaim (inline default-op-handle))

(defun default-op-handle (op) (ignore op) false)

(defmacro declare-class (name clauses &rest slots)
   `(def-class ,name (:handler
                      ,@(mapcar
                          (\\ (c) 
			      `(,(car c) ,(cadr c) 
				 (declare (ignore ,@(cadr c)))
				 nil)   )
			  clauses))
	      ,@slots)   )

(needed-by-macros

(defun struct-defclass (&key name storage-class is-testable
                             already-defined component
			     (slotmethods false) key-cons clauses slots)
   (cond (slotmethods
	  (error ":slotmethods not allowed in :structure ~s" name)))
   (cond (already-defined
          (!= key-cons true)
          (cond ((or component (not (null clauses)))
                 (error !"Already-defined class ~s may not acquire ~
                          handler or components"
			name)))))
   (let ()
      (!= clauses (<? neg #'is-Symbol clauses))
      (let ((printer
	       (assoc-if (\\ (c)
                            (memq c '(print :print-object :print-function)))
			 clauses))
	    (slotnames (repeat :for (s :in slots)
			:collect (cond ((atom s) s)
				       (t (car s)))))
	    (slot-initforms (repeat :for ((s :in slots))
			     :collect (cond ((atom s)
					     '+unbound-slot-val+)
					    (t (cadr s))))))
	 (cond (printer
		(let ((printer-name (car printer))
		      (printer-args (cadr printer)))
		   (cond ((eq printer-name 'print)
			  (!= printer-name ':print-object))
			 (t
			  (!= printer-name (intern (symbol-name *-*)
						   keyword-package*))))
		   (setf clauses (remove printer clauses :count 1))
		   (match-cond printer-args
		      ?( ((?var ?,name) ?@rem)
;;;;			(out (:to *error-output*)
;;;;			   "First printer arg for class " name " has specializer: "
;;;;			   printer-args
;;;;			   :% " I will remove it" :%)
			(!= printer-args
			    `(,var ,@rem))))
		   (multiple-value-let (pr-args pr-body)
				       (ignore-smooth
					  printer-args
					  (cddr printer))
		      (setf printer `((,printer-name
					 (lambda ,pr-args ,@pr-body)))))))
	       (t (!= printer !())))
	 (let ((slot-arg-names (append (all-slot-names component)
				       slotnames))
	       (slot-arg-initforms (append (all-slot-initforms component)
					   slot-initforms)))
	    (cond ((and (not (eq storage-class ':structure))
			(or (not (null clauses)) printer))
		   (error "Class ~s with medium ~s may not have a handler or printer"
			   name storage-class)))
	    (cond (component
		   (let ((d (get-ytools-class-descriptor component)))
		      (cond ((ytd-key-cons d)
			     (!= key-cons true))))))
	    (key-cons-check key-cons slot-arg-names
			    (class-all-components component)
			    name)
	    (let ((conser (build-symbol make- (< name)))
		  (extra-key-conser
		      (cond ((or (not key-cons)
                                 (eq key-cons ':nokey))
			     (build-symbol make- (< name) -key))
			    (t false))))
	       (let ((uninit-conser (build-symbol (< conser) -uninit))
		     (uninit-extra-key-conser
			(and extra-key-conser
			     (build-symbol (< extra-key-conser) -uninit))))
		  (let ((key-consargs
			   `(,@(<# tuple slot-arg-names slot-arg-initforms))))
		   `(progn
                       (eval-when (:compile-toplevel :load-toplevel
                                   :execute :slurp-toplevel)
                          (declare-ytools-class
                             ',name ',storage-class
                             ',(cond (component `(,component))
                                     (t false))
                             ',slotnames
                             ',slot-initforms
                             ',key-cons
                             ',(or extra-key-conser conser)))
		       ,@(cond ((not already-defined)
                                `((defstruct
                                   (,name
                                    ,@(include-if component
                                         `(:include ,component))
                                    ,@(include-if (not (eq storage-class
                                                            ':structure))
                                         `(:type
                                           ,(cond ((eq storage-class
                                                       ':list)
                                                   'list)
                                                  (t 'vector))))
                                    ,@(cond (is-testable
                                             `(:named
                                               (:predicate
                                                  ,(build-symbol
                                                      is- (< name)))))
                                            (t !()))
                                    ,@(cond ((and key-cons
                                                  (not (eq key-cons
                                                           ':nokey)))
                                             `((:constructor ,uninit-conser
                                                  (&key ,@key-consargs))))
                                            (t
                                             `((:constructor ,uninit-conser
                                                      (,@slot-arg-names))
                                               (:constructor
                                                  ,uninit-extra-key-conser
                                                  (&key ,@key-consargs)))))
                                    ,@printer)
                                   ,@slots)
                                  ,@(cond ((and key-cons
                                                (not (eq key-cons
                                                         ':nokey)))
                                           `((defun ,conser (&key ,@key-consargs)
                                               (initialize
                                                 (,uninit-conser
                                                  ,@(key-args->call-form
                                                       key-consargs))))))
                                          (t
                                           `((defun ,conser (,@slot-arg-names)
                                                (initialize
                                                   (,uninit-conser
                                                      ,@slot-arg-names)))
                                             (defun ,extra-key-conser
                                                    (&key ,@key-consargs)
                                                (initialize
                                                   (,uninit-extra-key-conser
                                                    ,@(key-args->call-form
                                                        key-consargs)))))))
                                  ,@(clauses-ensure-class clauses name)
                                  ',name)))))))))))

(defun all-slot-names (sc)
   (cond (sc
	  (let ((x (get-ytools-class-descriptor sc)))
	     (cond (x
		    (append (<$ all-slot-names (ytd-components x))
			    (ytd-slots x)))
		   (t '()))))
	 (t '())))

(defun all-slot-initforms (sc)
   (cond (sc
	  (let ((x (get-ytools-class-descriptor sc)))
	     (cond (x
		    (append (<$ all-slot-initforms (ytd-components x))
			    (ytd-initforms x)))
		   (t '()))))
	 (t '())))

(defun key-args->call-form (kal)
   (<! (\\ (ka)
	  (list (intern (symbol-name (car ka)) keyword-package*)
		(car ka)))
       kal))

(defun component-storage-class (c)
   (cond ((or (not c) (not (is-Symbol c)))
	  false)
	 (t
	  (let ((nd (get-ytools-class-descriptor c)))
	     (and nd
		  (ytd-medium nd))))))

)

(defvar empty-frame-slot* (list 'empty-slot))

(defun clos-defclass (name already-defined components
		      slotmethods key-cons clauses slots)
   (!= clauses (<? neg is-Symbol *-*))
   (let ((local-slotnames
             (for (s in slots)
                 (save (cond ((atom s) s)
                             (t (car s))))))
	 (local-initforms
	     (repeat :for ((s :in slots))
	      :collect (cond ((atom s) '+unbound-slot-val+)
			     (t (cadr s))))))
      (let ((local-slotspecs
	       (repeat :for ((s :in slots)
			     (n :in local-slotnames))
		:collect
                      `(,n
                        :initarg ,(intern (symbol-name n)
					  keyword-package*)
		        :accessor ,(build-symbol 
				      (< name) - (< n))
			,@(cond ((consp s)
                                 `(:initform ,(cadr s)
				   ;; What's left is :type ..., we hope; should check --
					     ,@(cddr s)))
                                (t '()))))))
	 (let ((all-slotnames
		  (append (<$ all-slot-names components)
			  local-slotnames))
	       (all-initforms
		  (append local-initforms
			  (<$ all-slot-initforms components)))
	       (all-components
		  (nodup (<$ class-all-components components)
			 :test #'eq))
	       (maker-name (build-symbol make- (< name)))
	       (extra-key-conser
                   (cond ((or (not key-cons)
                              (eq key-cons ':nokey))
                          (build-symbol make- (< name) -key))
                         (t false))))
	    (cond ((exists (c :in all-components)
		      (ytd-key-cons (get-ytools-class-descriptor c)))
		   (!= key-cons true)))
	    (key-cons-check key-cons all-slotnames all-components name)
	    (let ((initargs
		     (repeat :for (sn :in all-slotnames)
		      :nconc
			   (list (intern (symbol-name sn)
					 keyword-package*)
				 sn))))
	       (!= components
		   `(,@*-* YTools-object))
	       `(progn
		  (eval-when (:compile-toplevel :load-toplevel
                              :execute :slurp-toplevel)
		     (declare-ytools-class
                        ',name ':object ',components
                        ',local-slotnames ',local-initforms
                        ',key-cons
                        ,(cond ((or (not key-cons)
                                    (eq key-cons ':nokey))
                                `',extra-key-conser)
                               (t
                                `',maker-name))))
		  ,@(include-if (not already-defined)
		      `(common-lisp::defclass ,name ,components
			                      ,local-slotspecs))
		  ,@(inherited-slot-definitions slotmethods name
						all-components)
;;;;		  ,@(slot-method-definitions slotmethods name local-slotnames)
		  ,@(clauses-ensure-class clauses name)
		  ,@(cond ((and key-cons (not (eq key-cons ':nokey)))
;;;;                           (let ((params (<# (\\ (s i) `(,s ,i))
;;;;                                             all-slotnames
;;;;                                             all-initforms))) ...)
                           `(,(clos-key-conser-defn
                                 maker-name name
                                 all-slotnames all-initforms)))
                          (t
			   `((defun ,maker-name ,all-slotnames
			        (make-instance ',name ,@initargs))
                             ,(clos-key-conser-defn
                                 extra-key-conser name
                                 all-slotnames all-initforms))))
		  (defun ,(build-symbol is- (< name)) (x) (typep x ',name))
		  ',name))))))

(defun clos-key-conser-defn (maker-name classname all-slotnames all-initforms)
   (let ((initargs
            (repeat :for (sn :in all-slotnames)
             :nconc
                  (list (intern (symbol-name sn)
                                keyword-package*)
                        sn)))
         (params (<# (\\ (s i) `(,s ,i))
                     all-slotnames
                     all-initforms)))
      (cond ((exists (inf :in all-initforms)
               (eq inf '+unbound-slot-val+))
             ;;; We'd rather have a true CLOS unbound slot, even
             ;;; though there's a cost to having it
             `(defun ,maker-name (&rest pl &key ,@params)
                 (ignore ,@all-slotnames)
                 (apply #'make-instance
                        ',classname
                        (remove-absent-args pl))))
            (t
             `(defun ,maker-name (&key ,@params)
                  (make-instance ',classname ,@initargs))))))

(defun key-cons-check (key-cons all-slot-names all-components classname)
   (cond ((not key-cons)
	  (let ((numcomps (len all-components))
		(numslots (len all-slot-names)))
	     (let ((comps-compare
		      (cond ((> numcomps max-no-key-classes*)
			     '>)
			    (t '=<)))
		   (slots-compare
		      (cond ((> numslots max-no-key-slots*)
			     '>)
			    (t '=<))))
		(cond ((or (eq comps-compare '>)
			   (eq slots-compare '>))
		       (cerror "I will proceed; consider incrementing ~
				 max-no-key-classes* or max-no-key-slots*"
			       "Class definition for ~s too complicated for no-key constructor ~%Number of slots = ~s ~s ~s = max-no-key-slots* ~%Number of components = ~s ~s ~s = max-no-key-classes*"
			       classname
			       numslots slots-compare max-no-key-slots*
			       numcomps comps-compare max-no-key-classes*))))))))

;;; If first arg in a clause has no specializer, use the current class name
(defun clauses-ensure-class (clauses classname)
   (<# (\\ (clause)
	  (let ((op-name (cond ((memq (car clause) '(print :print-object))
				'print-object)
			       ((eq (car clause) ':print-function)
				(error "Can't have :print-function in CLOS class ~s"
				       classname))
			       (t (car clause)))))
	     (multiple-value-let (quals params body)
				 (meth-defn-analyze (cdr clause) op-name classname)
		(cond ((null params)
		       (error "Null parameters for operation ~s in class ~s"
			      op-name classname))
		      (t
		       (let ((first-param (car params)))
			  (cond ((and (is-Symbol first-param)
				      (not (memq first-param
						 '(&key &rest &optional
						   &allow-other-keys))))
				 (!= first-param
				     `(,*-* ,classname))))
			  `(def-meth ,op-name ,@quals (,first-param ,@(cdr params))
			      ,@body)))))))
       clauses))

(defun inherited-slot-definitions (slotmethods class all-comps)
   (cond ((not (or (memq 'noinherit slotmethods)
		   (memq ':noinherit slotmethods)))
	  (repeat :for ((comp :in all-comps))
	   :nconc
	      (let ((ytd (get-ytools-class-descriptor comp)))
		 (let ((inh-slots (ytd-slots ytd)))
		    (repeat :for ((s :in inh-slots))
		     :nconc (slot-accessor-definitions s class comp))))))
	 (t !())))

(defun slot-accessor-definitions (s class component)
   (let ((accessor (build-symbol (< class) - (< s)))
	 (component-version (build-symbol (< component) - (< s))))
      (list `(declaim (inline ,accessor (setf ,accessor)))
	    `(defun ,accessor (x) (,component-version x))
	    `(defun (setf ,accessor) (n x)
	        (setf (,component-version x) n)))))

(defun class-all-components (cls)
   (let ((comps nil))
      (labels ((collect (cls)  ; Actually a class name
		   (cond ((not (memq cls comps))
			  (!= comps (cons cls *-*))
			  (let ((nd (get-ytools-class-descriptor cls)))
			     (cond (nd
				    (repeat :for ((c :in (ytd-components nd)))
				       (collect c)   ))   )))   )))
	 (collect cls)
	 (nreverse comps)   )))

; Get all slots that can be detected at clos-build time.  Since clos may be
; mixed in later, new slots may appear.
(defun class-all-slots (cls)
   (repeat :for ((f :in (class-all-components cls))
	      (sl '()))
      (let ((nd (get-ytools-class-descriptor f)))
         (cond (nd
		(!= sl (union (ytd-slots nd) *-*)))))
    :result sl)   )

(defmacro make-inst (class &rest args)
   (let ((nd (and (is-Symbol class)
		  (get-ytools-class-descriptor class))))
      (cond ((and nd (eq (ytd-medium nd) ':structure))
	     (cond ((ytd-key-conser nd)
		    `(,(ytd-key-conser nd) ,@args))
		   (t
		    (error "Can't do 'make-inst' of class ~s, because it has no key-conser"
			   class))))
	    (t
	     `(make-instance ',class ,@args)))))

;;;;(defun ytools-obj-initialize (x)
;;;;   (initialize x)
;;;;   x)

(defmacro def-op (name argl &rest body)
   (!= < argl body > (ignore-smooth argl body))
   (multiple-value-let (d body) (declarations-separate body)
      `(defgeneric ,name ,argl
	  ,@(include-if (not (null body))
	       `(:method ((,(car argl) t) ,@(cdr argl))
		  ,@d
		  ,@body)))))

(needed-by-macros
(defun make-funcall (fname argnames)
   (let ((l (memq '&rest argnames)) fn)
      (cond (l `(apply ,fname ,@(ldiff argnames l) ,(cadr l)))
	    ((matchq (?(:\| function funktion) ?fn) fname)
	     `(,fn ,@argnames))
	    (t `(funcall ,fname ,@argnames))   )))
)

(defmacro def-meth (op &rest stuff)
   (multiple-value-let (qualifiers params body)
		       (meth-defn-analyze stuff op false)
      (!= < params body > (op-ignore-smooth params body))
      `(defmethod ,op ,@qualifiers ,params ,@body)))

(needed-by-macros

;;; 'defn' is a method definition, minus the operator
;;; Returns < qualifiers, args, body >
(defun meth-defn-analyze (defn op classname)
   (repeat :for ((stuff defn) :collector quals)
    :until (or (null stuff)
	       (listp (car stuff)))
    :collect (car stuff)
      (!= stuff (cdr stuff))
    :result (cond ((null stuff)
		   (error "Bogus method definition for class ~s:~
                           ~% ~s"
			  (or classname "obscure class")
			  `(def-meth ,op ,@defn)))
		  (t
		   (values quals (car stuff) (cdr stuff))))))

)

(defmacro continue-combined-method (self^ &rest args^)
   ;(IGNORE SELF^)
   `(call-next-method ,self^ ,@args^)   )

(def-op initialize (ob) ob)
(def-op obj-flag (ob) '|???|)

(defclass YTools-object () ())

(declare-ytools-class 'YTools-object ':object '() '() '() false 'make-YTools-object)

(defmethod initialize-instance :after ((ob YTools-object) &rest initargs)
   (declare (ignore initargs))
   (initialize ob))

; (SLOT-DEFAULTS ob sl val sl val ...) initializes unfilled slots.  Order is 
; important, and later slots may use values of earlier ones.
(defmacro slot-defaults (ob &rest slotdefs)
   (cond ((not (evenp (len slotdefs)))
	  (error "Probable missing 'ob' argument to 'slot-defaults': ~s"
		 (cons ob slotdefs))))
   (let ((obvar (gensym)))
      `(let ((,obvar ,ob))
	  ,@(repeat :for ((slotdefs = slotdefs :then (cddr slotdefs)))
	     :until (null slotdefs)
	     :collect `(cond ((not (slot-is-filled ,obvar ',(car slotdefs)))
			      (!= (slot-value ,obvar ',(car slotdefs))
				  ,(cadr slotdefs))))))))

(defun op-ignore-smooth (args body)
   (let-fun ()
      (multiple-value-bind (newargs to-be-ignored)
			   (op-args-underscores-elim args)
	 (let ((realbod (ignore-convert body)))
	    (cond ((null to-be-ignored)
		   (values args realbod))
		  (t
		   (values newargs `((declare (cl:ignore ,@to-be-ignored))
				     ,@realbod))))))
    :where

       (:def op-args-underscores-elim (args)
;;;;	  (trace-around op-args-underscores-elim
;;;;	     (:> "(op-args-underscores-elim: " args ")")
	  (repeat :for ((a :in args)
			:collectors realargs new-ignores)
	      (match-cond a
		 ?( _
		   (fix-arg false))
		 ?( (_ ?s)
		   (fix-arg s))
		  (t
		   (one-collect realargs a)))
	   :result (values realargs new-ignores)

	   :where
	      (fix-arg (cl)
		 (let ((realarg (gensym)))
		    (one-collect new-ignores realarg)
		    (cond (cl
			   (one-collect realargs `(,realarg ,cl)))
			  (t
			   (one-collect realargs realarg))))))
;;;;	     (:< (realargs new-ignores)
;;;;		 "op-args-underscores-elim: " realargs 1 new-ignores))
	  )))

(defun slot-is-filled (ob slotname)
   (and (slot-boundp ob slotname)
	(not (eq (slot-value ob slotname)
		 +unbound-slot-val+))))

(defmacro slot-is-bound (inst slot)
   (cond ((slot-boundp inst slot)
	  (let ((val (slot-value inst slot)))
	     (not (eq val +unbound-slot-val+))))
	 (t false)))

(defun remove-absent-args (pl)
   (repeat :for ((plt = pl :then (cddr plt)))
    :until (null plt)
    :when (not (eq (cadr plt) +unbound-slot-val+))
    :nconc (list (car plt) (cadr plt))))
					       
(defmacro with-slots-from-class (&whole form
				 slot-entries instance &rest body)
;;;;   (out "instance = " instance " body= " body :%)
   (let (class actual-body)
      (or (matchq (- ?class ?@actual-body)
		  body)
	  (matchq (?class ?@actual-body)
		  body)
	  (error "Ill-formed: ~s" form))
      (let ((slot-entries
	       (mapcar (\\ (e) (cond ((atom e) `(,e ,e))
				     (t e)))
		       slot-entries))
	    (instance-var (gensym)))
	 `(let ((,instance-var ,instance))
	     (symbol-macrolet ,(mapcar (\\ (e)
					  `(,(car e)
					    (,(build-symbol (< class) - (< (cadr e)))
					     ,instance-var)))
				       slot-entries)
		,@actual-body)))))

(defun is-objclass-name (x)
   (and (is-Symbol x)
        (let ((nd (get-ytools-class-descriptor x)))
           (cond (nd (eq (ytd-medium nd) ':object))
                 (t
	          (let ((c (find-class x nil)))
	            (and c (typep c 'standard-class))))))))

(defun is-struct-name (x)
   (and (is-Symbol x)
        (let ((nd (get-ytools-class-descriptor x)))
           (cond (nd (eq (ytd-medium nd) ':structure))
                 (t
		  (let ((c (find-class x nil)))
		     (and c (typep c 'structure-class))))))))
