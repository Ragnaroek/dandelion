;;; Guess an Animal - CL implementation.
;;;
;;; Copyright (C) 1997-2001 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: animals.lisp,v 2.11 2005/01/27 23:03:05 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/animals.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `string-beg-with'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; `alist->hash-table', `hash-table->alist'
  (require :cllib-miscprint (translate-logical-pathname "cllib:miscprint"))
  ;; `write-to-file', `save-restore'
  (require :cllib-fileio (translate-logical-pathname "cllib:fileio"))
  ;; `+clos-readtable+'
  (require :cllib-closio (translate-logical-pathname "cllib:closio"))
  ;; `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `symbol-concat'
  (require :cllib-symb (translate-logical-pathname "cllib:symb")))

(in-package :cllib)

(export '(play-animals play-game *animals-file-name*))

;;;
;;; Standard version
;;;

(defvar *animals-debug-use-built-in-data* nil "Do not read the file.")
(defvar *animals-default-data*
  '("Is it an insect" ("Can it sting" "a bee" .
    "a roach") "Can it fly" "a duck" . "a penguin")
  "The built-in to be used if `*animals-debug-use-built-in-data*' is non-nil.")
(defvar *animals-data* nil "The actual data tree.")
(defcustom *animals-file-name* string "animals"
  "*The data file.
Defaults to (merge-pathnames \"animals\" *datadir*).")

(defun anml-get-question (old new)
  "Get the question that will distinguish betwee OLD and NEW.
The question will contain `it'."
  (do ((quest nil (if (search "it" quest) quest nil)))
      ((null quest) quest)
    (setq quest
	  (get-string t "What Yes/No question distinguishes between ~a and ~a?"
		      old new))))

(defun get-string (fmt &rest args)
  "Ask for input, return it."
  (apply #'format t fmt args)
  (read-line t))

(defun anml-add-article (str)
  "Add an article in the beginning of the string."
  (declare (simple-string str))
  (setq str (string-trim +whitespace+ str))
  (if (let ((len (length str)))
        (or (string-beg-with "a " str len)
            (string-beg-with "an " str len)
            (string-beg-with "the " str len)))
      str
      (concatenate 'string (if (find (schar str 0) #(#\a #\e #\i #\o #\u)
                                     :test #'char=)
                               "an " "a ") str)))

(defun anml-chop-article (str)
  "Remove the article from the beginning of the string.
Returnes a fresh string."
  (declare (simple-string str))
  (setq str (string-trim +whitespace+ str))
  (let ((len (length str)))
    (cond ((string-beg-with "a " str len) (subseq str 2))
          ((string-beg-with "an " str len) (subseq str 3))
          ((string-beg-with "the " str len) (subseq str 4))
          (t str))))

(defun fix-question (quest)
  (declare (simple-string quest))
  (let ((qq (string-trim +whitespace+ quest)))
    (declare (simple-string qq))
    (if (char= #\? (schar qq (1- (length qq)))) qq
        (concatenate 'string qq "?"))))

(defun get-question (1st 2nd)
  (loop :for quest :of-type simple-string =
        (fix-question
         (get-string "What Yes/No question distinguishes between ~a and ~a?
 ==> " 1st 2nd))
        :for it-pos :of-type (or null index-t) = (search "it" quest)
        :unless it-pos :do (format t "The question must contain `it'.~%")
        :else :return
        (if (y-or-n-p "~a~a~a" (subseq quest 0 it-pos) 1st
                      (subseq quest (+ it-pos 2)))
            (values quest 1st 2nd) (values quest 2nd 1st))))

(defun anml-finish (tail)
  "Endgame."
  (cond ((y-or-n-p "Is it ~a? " tail)
	 (format t "I won!~%") tail)
	(t
         (let ((new (anml-add-article
                     (get-string "I lost...~%What was your animal?..."))))
           (multiple-value-bind (quest 1st 2nd) (get-question new tail)
             (cons quest (cons 1st 2nd)))))))

(defun save-restore-animals (&optional what)
  (save-restore what :var '*animals-data* :name *animals-file-name*))

;;;###autoload
(defun play-animals ()
  "Play the famous game!"
  ;; read the initial data
  (if *animals-debug-use-built-in-data*
      (setq *animals-data* *animals-default-data*)
      (save-restore-animals))
  (mesg :animals t "animals-data now:~%~s~%" *animals-data*)
  ;; play the game
  (let ((animals-data-modified nil))
    (do ((root *animals-data* *animals-data*) follow over)
        (over)
      (do () ((not (consp root)))
        (setq follow root
              root (if (y-or-n-p (car root)) (cadr root) (cddr root))))
      (setf (cdr (or follow *animals-data*))
            (if (eq root (cadr follow))
                (cons (anml-finish root) (cddr follow))
                (cons (cadr follow) (anml-finish root)))
            animals-data-modified t)
      (mesg :animals t "`*animals-data*' now: ~a~%" *animals-data*)
      (setq over (y-or-n-p "Quit?")))
    ;; save the new information
    (mesg :animals t "done - saving the information...~%")
    (if *animals-debug-use-built-in-data*
        (print "used built-in data -- no save!")
        (if animals-data-modified
            (save-restore-animals t)
            (format t "You taught me no new animals this time...~%"))))
  (when (y-or-n-p "Exit lisp?") (quit)))

;;;
;;; Network implementation
;;;

(eval-when (compile load eval)  ; ACL: for `*root-node*'
(defclass node ()
  ((name :type symbol :accessor node-name :initarg name)
   (info :type simple-string :accessor node-info :initarg info)
   (yes :type symbol :accessor node-yes :initarg yes)
   (no :type symbol :accessor node-no :initarg no))
  (:documentation "The information node."))
)

(defmethod print-object ((nd node) (out stream))
  (if *print-readably* (call-next-method)
      (format out "[~s: ~s~@[ y:~s~]~@[ n:~s~]]" (node-name nd) (node-info nd)
              (if (slot-boundp nd 'yes) (node-yes nd))
              (if (slot-boundp nd 'no) (node-no nd)))))

(defcustom *network* hash-table (make-hash-table :test 'eq)
  "*symbol --> node.")

;(defconst +bad-node+ node (make-instance 'node)
;  "*The convenient constant for init.")

(defun name->node (name)
  "Find the node with this name."
  (declare (symbol name))
  (multiple-value-bind (node found-p) (gethash name *network*)
    (assert found-p (name) "No such node: `~s'" name)
    (assert (eq name (node-name node)) (name) "Node (~s) name (~s) mismatch"
            node name)
    node))

(defun mknode (name info &optional yes no)
  "Make a node and add it to the network."
  (assert name (name) "Cannot create a NULL node (~s ~s ~s ~s)"
          name info yes no)
  (let ((new (if yes (make-instance 'node 'name name 'info info
                                    'yes yes 'no no)
                 (make-instance 'node 'name name 'info info))))
    (multiple-value-bind (old found-p) (gethash name *network*)
      (when found-p
        (warn " ** Redefining node `~s' from~% -- ~s~% ** to~% -- ~s~%"
              name old new)))
    (setf (gethash name *network*) new)))

(defcustom *root-node* node
  (mknode 'root "Is is a living being?" 'living 'object)
  "*The root node, from which the search starts by default.")

(defun save-restore-network (&optional what)
  (save-restore what :name "network.dat"
                :var '*network* :basedir *datadir*
                :voidp (lambda (ht) (>= 1 (hash-table-count ht)))
                :readtable +clos-readtable+
                :pre-save #'hash-table->alist
                :post-read #'alist->hash-table))

(defun resolve (&optional (root *root-node*))
  "Walk through the `*network*' starting with root."
  (declare (type (or node symbol) root))
  (when (symbolp root) (setq root (name->node root)))
  (if (slot-boundp root 'yes)
      (let* ((ans (if (y-or-n-p (node-info root))
                      (node-yes root) (node-no root)))
             (new (gethash ans *network*)))
        (if new (resolve new) (values ans root)))
      (values (node-info root) root)))

(defun to-symbol (string)
  "Chop article, upcase and intern."
  (declare (string string))
  (intern (nstring-upcase
           (nsubstitute #\- #\Space (anml-chop-article string)))))

(defun concat-down (&rest args)
  "Concatenate the strings, downcasing."
  (nstring-downcase (apply #'concatenate 'string args)))

(defun get-symbol (&rest args)
  "Intern whatever was returned by `get-string'."
  (to-symbol (apply #'get-string args)))

(defun get-new-symbol (&rest args)
  "Call `get-symbol' and ensure that the symbol is not in `*network*'."
  (loop :for name :of-type symbol = (apply #'get-symbol args)
        :for node :of-type (or null node) = (gethash name *network*)
        :while node :do (format t "The name exists: ~s~%" node)
        :finally (return name)))

(defun add-node (name node)
  "Add a node at the return values of `resolve'."
  (declare (type node node) #+nil(type (or symbol string) name))
  (typecase name
    (symbol                     ; symbol w/o node
     (format t " *** adding a node named `~s'~%" name)
     (if (y-or-n-p
          "  *  Is it a leaf (object) or a node (category)? [Y-leaf; N-node]")
         (mknode name
                 (get-string "What is the content of the `~s' node?" name))
         (mknode name
                 (fix-question
                  (get-string "What is the question for the node `~s'?" name))
                 (get-symbol "What is the `yes' answer?")
                 (get-symbol "What is the `no' answer?"))))
    (string                     ; node w/o y/n
     (let* ((new (get-string "I lost...~%...What was it?"))
            (nna (symbol-concat (node-name node) "*"))
            (nsy (to-symbol new)) (nno (gethash nsy *network*)))
       (when (and nno
                  (not (y-or-n-p
                        " * Hey, I seem to know of ~s: ~s~% * Did you mean it?"
                        nsy nno)))
         (setq nsy (symbol-concat nsy "*") nno (mknode nsy name)))
       (multiple-value-bind (quest yes no) (get-question new name)
         (mknode nna name)
         (setf (node-info node) quest)
         (cond ((string= name yes)
                (setf (node-yes node) nna (node-no node) nsy))
               ((string= name no)
                (setf (node-no node) nna (node-yes node) nsy))
               (t (error 'code :proc 'add-node
                         :args (list quest yes no name node) :mesg
                         "get-question (~s ~s ~s): name: `~s'; node: ~s"))))))
    (t (error 'case-error :proc 'add-node :args
              (list 'name name 'symbol 'string)))))

(defun animals->network (&optional (data *animals-data*) (base "ANIMAL"))
  "Merge the animals data into the network."
  (let* ((nm (get-new-symbol "Content: ~s~%Base: ~s~%* What is the node name? "
                             (car data) base))
         (yy (cadr data)) (nn (cddr data)) (st (symbol-name nm)))
    (mknode nm (car data)
            (if (stringp yy) (node-name (mknode (to-symbol yy) yy))
                (animals->network yy (concat-down base " " st)))
            (if (stringp nn) (node-name (mknode (to-symbol nn) nn))
                (animals->network nn (concat-down base " !" st))))
    nm))

(defun play-game (&optional (root *root-node*))
  "Interactive `resolve'/`add-node'."
  (declare (type (or node symbol) root))
  (save-restore-network)
  (loop :do (multiple-value-bind (name node) (resolve root)
              (declare #+nil(type (or symbol string) name) (type node node))
              (if (typecase name
                    (string (y-or-n-p "Is it ~s?" name))
                    (symbol
                     (not (y-or-n-p "Dangling pointer: `~s'.  Add a node?"
                                    name)))
                    (t (error 'case-error :proc 'play-game :args
                              (list 'name name 'symbol 'string))))
                  (format t "I won!~%")
                  (add-node name node)))
        :while (y-or-n-p "One more game?"))
  (save-restore-network t))

(provide :cllib-animals)
;;; animals.lisp ends here
