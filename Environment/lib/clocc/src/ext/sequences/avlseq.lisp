;;; Extensible Sequence Type
;;; AVL trees as Sequences
;;;
;;; Copyright (C) 1988, 1992 Bruno Haible
;;; This is Free Software, published under the GNU General Public License v2.

;; AVL-trees, implemented in COMMON LISP, as sequences
;; Bruno Haible, 22.09.1988
;; CLISP-Version 29.12.1988
;; Version with many declarations,
;;             bookkeeping over the length of each subtree,
;;             definition of a sequence-subtype AVL-TREE.
;; Stefan Kain, 01.09.2004: translation of comments
;; Sam Steingold 2004-09-05: updated to ANSI CL

;; An AVL-tree is a binary tree, where in each node a datum
;; (value) is located. The tree is always balanced, in a way, that the depths
;; of two left and right subtrees differ at most by 1.
;; The order relation on the values is set by a comparison function
;; comp, that determines, when x<y (a fixed chosen order relation).
;; If (not (or (comp x y) (comp y x))) is true, both x and y are
;; considered equal.
;; The tree can be traversed like a sequence from left to right or from
;; right to left. Thus, all sequence-functions operate also on
;; AVL-trees.

(eval-when (load compile eval)
  (require 'sequences "sequences"))

(defpackage #:avl
  (:shadow #:length #:member #:delete #:copy #:merge)
  (:export #:treep #:member #:insert #:delete #:do-avl #:avl-to-seq
           #:seq-to-avl #:copy #:merge)
  (:import-from #:sequences sequences::seq))
(in-package #:avl)

;; data structure of a tree: empty tree=nil, else node
(deftype tree ()
  '(or null node))

(defstruct node
  (level 0 :type fixnum)
  (left nil :type tree)
  (right nil :type tree)
  (value nil)
  (length 1 :type (integer 1 *)))
(declaim (inline node-level node-left node-right node-value node-length))

;; (level tree) returns the depth of a tree
(declaim (ftype (function (node) fixnum) level))
(declaim (inline level))
(defun level (tr)
  (if tr (locally (declare (type node tr)) (node-level tr)) 0))

;; (length tree) returns the number of nodes of a tree
(declaim (ftype (function (tree) integer) length))
(declaim (inline length))
(defun length (tr)
  (if tr (locally (declare (type node tr)) (node-length tr)) 0))

;; (recalc-length node) recalculates (node-length node)
(declaim (ftype (function (node) integer) recalc-length))
(declaim (inline recalc-length))
(defun recalc-length (tr)
  (declare (type node tr))
  (setf (node-length tr) (+ (length (node-left tr)) 1
                            (length (node-right tr)))))

;; (deftype avl-tree (comp) ...) does not work.

;; (treep tr comp) determines, if the tree tr is an AVL-tree.
(declaim (ftype (function (tree function &optional t) symbol) treep))
(defun treep (tr comp &optional (el-type t))
  (or (null tr)
      (and
        (typep tr 'node)
        (locally (declare (type node tr))
          (and
            (typep (node-value tr) el-type)
            (let ((trl (node-left tr))
                  (trr (node-right tr)))
              (declare (type tree trl trr) (type node tr))
              (and (= (level tr)
                      (1+ (the fixnum
                            (max (the fixnum (level trl))
                                 (the fixnum (level trr))))))
                   (<= (the fixnum
                         (abs (the fixnum
                                (- (the fixnum (level trl))
                                   (the fixnum (level trr))))))
                       1)
                   (or (null trl)
                       (locally (declare (type node tr trl))
                         (funcall comp (node-value trl) (node-value tr))))
                   (or (null trr)
                       (locally (declare (type node tr trr))
                         (funcall comp (node-value tr) (node-value trr))))
                   (treep trl comp el-type)
                   (treep trr comp el-type))))))))


;; (ganzrechts tr) returns the "right-most" element of a non-empty tree
(declaim (ftype (function (node) node) ganzrechts))
(defun ganzrechts (tr)
  (declare (type node tr))
  (if (node-right tr) (ganzrechts (node-right tr)) (node-value tr)))

(declaim (ftype (function (node) node) ganzlinks))
;; (ganzlinks tr) returns the "left-most" element of a non-empty tree
(defun ganzlinks (tr)
  (declare (type node tr))
  (if (node-left tr) (ganzlinks (node-left tr)) (node-value tr)))


;; (member item tree comp) tests, if item is an element of the Tree tree.
;; By supplying an equality test eq-test, one can check if
;; both values (item and the value in the tree) are equal in a closer sense.
;; Trick: If NIL is not stored as a value in the tree, one can use
;; eq-test = #'(lambda (it val) (and ("=" it val) val)) in order to
;; get the value val from the tree.
(declaim (ftype (function (t tree function &optional function) t) member))
(defun member (item tr comp &optional (eq-test #'equal))
  (if (null tr) nil
    (locally (declare (type node tr))
      (cond ((funcall eq-test item (node-value tr)))
            ((funcall comp item (node-value tr))
             (member item (node-left tr) comp eq-test))
            ((funcall comp (node-value tr) item)
             (member item (node-right tr) comp eq-test))
            )))) ; else NIL


;; (balance tree) balances an non-empty Tree tree. prerequisite
;; is, that at most one element has brought the tree out of balance.
;; tree itself will be modified!
(declaim (ftype (function (node) node) balance))
(defun balance (b)
  (let ((l (level (node-left b)))
        (r (level (node-right b))))
    (declare (fixnum l r) (type node b c d))
    (setf (node-level b) (the fixnum (1+ (the fixnum (max l r)))))
    (case (the fixnum (- r l))
      ((-2)(let ((c (node-left b))
                 (d nil))
             (cond ((< (the fixnum (level (node-left c)))
                       (the fixnum (level (node-right c))))
                    (setq d (node-right c))
                    (setf (node-right c) (node-left d))
                    (setf (node-left b) (node-right d))
                    (setf (node-left d) c)
                    (setf (node-right d) b)
                    (setf (node-level b) (node-level d))
                    (setf (node-level d) (node-level c))
                    (setf (node-level c) (node-level b))
                    (recalc-length b)
                    (recalc-length c)
                    (recalc-length d)
                    d)
                   (t
                    (setf (node-left b) (node-right c))
                    (setf (node-right c) b)
                    (setf (node-level b)
                          (the fixnum (1+ (the fixnum (level (node-left b))))))
                    (setf (node-level c)
                          (the fixnum (1+ (the fixnum (node-level b)))))
                    (recalc-length b)
                    (recalc-length c)
                    c))))
      ((2) (let ((c (node-right b))
                 (d nil))
             (cond ((< (the fixnum (level (node-right c)))
                       (the fixnum (level (node-left c))))
                    (setq d (node-left c))
                    (setf (node-left c) (node-right d))
                    (setf (node-right b) (node-left d))
                    (setf (node-right d) c)
                    (setf (node-left d) b)
                    (setf (node-level b) (node-level d))
                    (setf (node-level d) (node-level c))
                    (setf (node-level c) (node-level b))
                    (recalc-length b)
                    (recalc-length c)
                    (recalc-length d)
                    d)
                   (t
                    (setf (node-right b) (node-left c))
                    (setf (node-left c) b)
                    (setf (node-level b)
                          (the fixnum (1+ (the fixnum (level (node-right b))))))
                    (setf (node-level c)
                          (the fixnum (1+ (the fixnum (node-level b)))))
                    (recalc-length b)
                    (recalc-length c)
                    c))))
      ((-1 0 1) (recalc-length b) b))))


;; (insert item tree comp) inserts item into the tree.
;; The result is again an AVL-tree. If an element that compares
;; equal to item is already there, item will replace it.
;; By specifying an equality test eq-test, it can be determined which elements
;; are considered as equal.
;; (This has to comprise those element pairs, that are not comparable:
;; for any x and y, you must have: x<y or y<x or (eq-test x y).)
;; tree itself will be modified!
(declaim (ftype (function (t tree function &optional function) node) insert))
(defun insert (item tr comp &optional (eq-test #'equal))
  (if (null tr) (make-node :level 1 :value item)
    (locally (declare (type node tr))
      (cond
        ((funcall eq-test item (node-value tr))
         (setf (node-value tr) item)
         tr)
        (t
         (cond
           ((funcall comp item (node-value tr))
            (setf (node-left tr) (insert item (node-left tr) comp eq-test)))
           ((funcall comp (node-value tr) item)
            (setf (node-right tr) (insert item (node-right tr) comp eq-test)))
           (t (error "element does not fit into the AVL-tree-sorting!")))
         (balance tr))))))


;; (delete item tree comp) removes item from tree and returns the
;; reduced tree.
(declaim (ftype (function (t tree function &optional function) tree) delete))
(defun delete (item tr comp &optional (eq-test #'equal))
  (if (null tr) tr
    (locally (declare (type node tr))
      (cond
        ((funcall eq-test item (node-value tr))
         (let ((r (node-right tr)))
           (declare (type node tr))
           (if (null r)
               (node-left tr)
               (multiple-value-bind (rest del) (delete-ganzlinks r)
                 (declare (type node del))
                 (setf (node-left del) (node-left tr))
                 (setf (node-right del) rest)
                 (balance del)))))
        ((funcall comp item (node-value tr))
         (setf (node-left tr) (delete item (node-left tr) comp eq-test))
         (balance tr))
        ((funcall comp (node-value tr) item)
         (setf (node-right tr) (delete item (node-right tr) comp eq-test))
         (balance tr))
        (t (error "element does not fit into the AVL-tree-sorting!"))))))

;; (delete-ganzlinks tree) removes the "smallest" element from a non-empty
;; tree and returns the remaining tree. The removed element appears as
;; second value (as node, in order to avoid creation of garbage).
(declaim (ftype (function (node) tree) delete-ganzlinks))
(defun delete-ganzlinks (tr)
  (declare (type node tr))
  (if (null (node-left tr))
      (values (node-right tr) tr)
      (multiple-value-bind (tl el) (delete-ganzlinks (node-left tr))
        (setf (node-left tr) tl)
        (decf (node-length tr))
        (values tr el))))


;; (do-avl (var treeform [resultform]) {declaration}* {tag|statement}* )
;; is a macro like dolist: for all var from the AVL-tree, that is issued
;; by treeform, the rest will be executed.
(defmacro do-avl (varform &rest body)
  `(progn
     (traverse ,(second varform)
               #'(lambda (,(first varform)) ,@body))
     ,(if (third varform)
        `(let ((,(first varform) nil)) ,(first varform) ,(third varform))
        'nil)))

(defmacro do-avl-1 ((var treeform &optional resultform) &body body)
  (let ((abstieg (gensym)) ; labels
        (aufstieg (gensym))
        (ende (gensym))
        (stack (gensym)) ; (cons ,top ,stack) is a "stack"
        (top (gensym)))
    `(prog ((,stack nil) (,top ,treeform))
        ,abstieg
        (if (null ,top) (go ,aufstieg))
        (push ,top ,stack) (setq ,top (node-left (the node ,top)))
        (go ,abstieg)
        ,aufstieg
        (if (null ,stack) (go ,ende))
        (if (eq ,top (node-right (the node (setq ,top (pop ,stack)))))
            (go ,aufstieg))
        (let ((,var (node-value (the node ,top)))) ,@body)
        (push ,top ,stack) (setq ,top (node-right (the node ,top)))
        (go ,aufstieg)
        ,ende
        (let ((,var nil)) (return ,resultform)))))

(declaim (ftype (function (tree (function (t) t)) null) traverse))
(defun traverse (tr fun)
  (if (null tr) nil
      (locally (declare (type node tr))
        (traverse (node-left tr) fun)
        (funcall fun (node-value tr))
        (traverse (node-right tr) fun))))


;; (avl-to-seq tree) yields a sorted list of all values of the tree.
;; (avl-to-seq tree seq-type) yields a sorted sequence of specified
;; type of all values of the tree.
(declaim (ftype (function (tree &optional t) sequence) avl-to-seq))
(defun avl-to-seq (tr &optional (result-type 'list))
  (if (null tr)
      (make-sequence result-type 0)
      (locally (declare (type node tr))
        (concatenate result-type
          (avl-to-seq (node-left tr))
          (make-sequence result-type 1 :initial-element (node-value tr))
          (avl-to-seq (node-right tr))))))

;; (seq-to-avl l comp) creates an AVL-tree from a (unsorted) sequence l
;; of elements.
(declaim (ftype (function (sequence function &optional function) tree)
                seq-to-avl))
(defun seq-to-avl (l comp &optional (eq-test #'equal))
  (reduce #'(lambda (tr item) (insert item tr comp eq-test))
          l :initial-value nil))


;; (copy tree) creats a copy of the AVL-tree tree.
;; Only the tree structure is copied, the values are taken over.
;; insert and delete can now be performed on the original and the
;; copy independently.
(declaim (ftype (function (tree) tree) copy))
(defun copy (tr)
  (if (null tr) nil
      (locally (declare (type node tr))
        (make-node :level (node-level tr)
                   :left (copy (node-left tr))
                   :right (copy (node-right tr))
                   :value (node-value tr)))))


;; (merge tree1 tree2 comp) creates a new AVL-tree, that consists of
;; the elements of the trees tree1 and tree2.
;; By specifying an equality test, it can be determind which elements
;; do not have to be taken over twice into the new AVL-tree
;; (because they are equal)
;; (Any two elements x,y that are neither x<y nor x>y have to be
;; equal in this sense (according to comp).)
(declaim (ftype (function (tree tree function &optional function) tree) merge))
(defun merge (tr1 tr2 comp &optional (eq-test #'equal))
  (if (< (the fixnum (level tr1)) (the fixnum (level tr2))) (rotatef tr1 tr2))
  ;; now, tr1 is the bigger of the trees
  (let ((tr (copy tr1)))
    (do-avl (x tr2 tr) (setq tr (insert x tr comp eq-test)))))

;; AVL-trees as sequences:

;; AVL-tree recognizable as such:
(defstruct (avl-tree (:constructor box-tree (tree)))
  (tree nil))
(declaim (inline box-tree avl-tree-tree))

(defmethod print-object ((seq avl-tree) (out stream))
  (format out #+VAX "~@!#S(~;~S ~:_~S ~:_~S ~:_~S ~:_~S~;)~."
              #-VAX "#S(~S ~S ~S ~S ~S)"
              'avl-tree ':contents (sequences:coerce seq 'list)
              ':length (sequences:length seq)))

;; new constructor function (for the reader):
(defun new-avl-tree-constructor (&key contents &allow-other-keys)
  (sequences:coerce contents 'avl-tree))
#+CLISP
(setf (svref (get 'avl-tree 'SYSTEM::DEFSTRUCT-DESCRIPTION) 3)
      'new-avl-tree-constructor)

(defmacro unbox (seq) `(avl-tree-tree ,seq))

;; (make-tree size) creates an empty AVL-tree with size nodes.
(defun make-tree (size)
  (if (zerop size)
    nil
    (let ((left (make-tree (floor (1- size) 2)))
          (right (make-tree (ceiling (1- size) 2))))
      (make-node :level (1+ (level right)) :left left :right right
                 :length size))))

;; AVL-tree as sequence of type AVL-TREE :
;; pointer is a vector with fill-pointer.
;; fill-pointer=0 -> end reached.
;; fill-pointer=2*k+1 -> the vector contains the path to the next NODE,
;;   alternating a NODE and a direction indicator (LEFT, RIGHT).
(sequences:define-sequence AVL-TREE
  :init        #'(lambda ()
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if tr
                       (locally (declare (type node tr))
                         (loop
                           (vector-push tr pointer)
                           (if (null (node-left tr)) (return))
                           (vector-push 'LEFT pointer)
                           (setq tr (node-left tr)))))
                     pointer))
  :upd         #'(lambda (pointer)
                   (declare (ignore seq))
                   (if (zerop (fill-pointer pointer))
                     (error "Reached the right end of a ~S." 'avl-tree)
                     (let ((tr (aref pointer (1- (fill-pointer pointer)))))
                       (declare (type node tr))
                       (if (node-right tr)
                         (progn
                           (setq tr (node-right tr))
                           (vector-push 'RIGHT pointer)
                           (loop
                             (vector-push tr pointer)
                             (if (null (node-left tr)) (return))
                             (setq tr (node-left tr))
                             (vector-push 'LEFT pointer)))
                         (loop
                           (vector-pop pointer)
                           (if (zerop (fill-pointer pointer)) (return))
                           (if (eq (vector-pop pointer) 'LEFT) (return))))
                       pointer)))
  :endtest     #'(lambda (pointer)
                   (declare (ignore seq))
                   (zerop (fill-pointer pointer)))
  :fe-init     #'(lambda ()
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if tr
                       (locally (declare (type node tr))
                         (loop
                           (vector-push tr pointer)
                           (if (null (node-right tr)) (return))
                           (vector-push 'RIGHT pointer)
                           (setq tr (node-right tr)))))
                     pointer))
  :fe-upd      #'(lambda (pointer)
                   (declare (ignore seq))
                   (if (zerop (fill-pointer pointer))
                     (error "Reached the left end of a ~S." 'avl-tree)
                     (let ((tr (aref pointer (1- (fill-pointer pointer)))))
                       (declare (type node tr))
                       (if (node-left tr)
                         (progn
                           (setq tr (node-left tr))
                           (vector-push 'LEFT pointer)
                           (loop
                             (vector-push tr pointer)
                             (if (null (node-right tr)) (return))
                             (setq tr (node-right tr))
                             (vector-push 'RIGHT pointer)))
                         (loop
                           (vector-pop pointer)
                           (if (zerop (fill-pointer pointer)) (return))
                           (if (eq (vector-pop pointer) 'RIGHT) (return))))
                       pointer)))
  :fe-endtest  #'(lambda (pointer)
                   (declare (ignore seq))
                   (zerop (fill-pointer pointer)))
  :access      #'(lambda (pointer)
                   (declare (ignore seq))
                   (if (zerop (fill-pointer pointer))
                     (error "Reached the end of a ~S." 'avl-tree)
                     (node-value (aref pointer (1- (fill-pointer pointer))))))
  :access-set  #'(lambda (pointer value)
                   (declare (ignore seq))
                   (if (zerop (fill-pointer pointer))
                     (error "Reached the end of a ~S." 'avl-tree)
                     (setf (node-value (aref pointer
                                             (1- (fill-pointer pointer))))
                           value)))
  :copy        #'(lambda (pointer)
                   (declare (ignore seq))
                   (make-array (array-dimensions pointer)
                               :initial-contents pointer
                               :fill-pointer (fill-pointer pointer)))
  :length      #'(lambda ()
                   (let ((tr (unbox seq)))
                     (if tr (node-length tr) 0)))
  :make        #'(lambda (size)
                   (declare (ignore seq))
                   (box-tree (make-tree size)))
  :elt         #'(lambda (index)
                   (let ((tr (unbox seq)))
                     (if (and tr (< index (node-length tr)))
                       (locally (declare (type node tr))
                         (loop ; here is 0 <= index < (node-length tr)
                           (let ((lleft (length (node-left tr))))
                             (cond ((< index lleft)
                                    (setq tr (node-left tr)))
                                   ((= index lleft) (return (node-value tr)))
                                   (t ; (> index lleft)
                                    (setq index (- index (+ lleft 1)))
                                    (setq tr (node-right tr)))))))
                       (error "Invalid index: ~S" index))))
  :set-elt     #'(lambda (index value)
                   (let ((tr (unbox seq)))
                     (if (and tr (< index (node-length tr)))
                       (locally (declare (type node tr))
                         (loop ; here is 0 <= index < (node-length tr)
                           (let ((lleft (length (node-left tr))))
                             (cond ((< index lleft)
                                    (setq tr (node-left tr)))
                                   ((= index lleft)
                                    (return (setf (node-value tr) value)))
                                   (t ; (> index lleft)
                                    (setq index (- index (+ lleft 1)))
                                    (setq tr (node-right tr)))))))
                       (error "Invalid index: ~S" index))))
  :init-start  #'(lambda (index)
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if (<= 0 index (length tr))
                       (if (and tr (< index (node-length tr)))
                         (locally (declare (type node tr))
                           (loop ; here is 0 <= index < (node-length tr)
                             (vector-push tr pointer)
                             (let ((lleft (length (node-left tr))))
                               (cond ((< index lleft)
                                      (vector-push 'LEFT pointer)
                                      (setq tr (node-left tr)))
                                     ((= index lleft) (return))
                                     (t ; (> index lleft)
                                      (vector-push 'RIGHT pointer)
                                      (setq index (- index (+ lleft 1)))
                                      (setq tr (node-right tr))))))))
                       (error "Invalid index: ~S" index))
                     pointer))
  :fe-init-end #'(lambda (index)
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if (<= 0 index (length tr))
                       (if (and tr (plusp index))
                         (locally (declare (type node tr))
                           (loop ; here is 0 < index <= (node-length tr)
                             (vector-push tr pointer)
                             (let ((lleft (length (node-left tr))))
                               (cond ((<= index lleft)
                                      (vector-push 'LEFT pointer)
                                      (setq tr (node-left tr)))
                                     ((plusp (setq index (- index (1+ lleft))))
                                      (vector-push 'RIGHT pointer)
                                      (setq tr (node-right tr)))
                                     (t ; (= index (1+ lleft))
                                      (return)))))))
                       (error "Invalid index: ~S" index))
                     pointer)))

(provide 'avlseq)
