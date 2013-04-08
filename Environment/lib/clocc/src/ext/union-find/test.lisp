;;; Test case from CLR, Ch 22, pg.442.

;;; Consider the (undirected) graph
;;;  (make-graph :vertices '(a b c d e f g h i j)
;;; 	     :undirectedp t		; All edges will also be reversed.
;;; 	     :edgs '((a b)
;;; 		     (a c)
;;; 		     (c b)
;;; 		     (b d)

;;; 		     (e f)
;;; 		     (e g)

;;; 		     (h i))
;;; 		     )


(defvar *vertices* '(a b c d e f g h i j))
  
(defun test-union-find ()
  (labels ((print-disjoint-sets (partition)
	     (let ((printed-sets (make-hash-table)))
	       (declare (dynamic-extent printed-sets))
	       (loop for v in *vertices*
		     for s = (cl-uf:find-set partition v)
		     unless (gethash s printed-sets)
		       do (setf (gethash s printed-sets) t)
		       and collect (cl-uf:collect-set partition v)
		           into sets
		     finally (format t "~&Sets:~{ ~S~}~%" sets))))

	   (union-and-print (p v1 v2)
	     (format t "~%Joining set with ~A with set with ~S.~%" v1 v2)
	     (cl-uf:union p (cl-uf:find-set p v1) (cl-uf:find-set p v2))
	     (print-disjoint-sets p))
	   )
    (let ((p (cl-uf:make-partition :test #'eq)))
      (dolist (v *vertices*)
	(cl-uf:make-set p v))

      (print-disjoint-sets p)

      (union-and-print p 'b 'd)
      (union-and-print p 'e 'g)
      (union-and-print p 'a 'c)
      (union-and-print p 'h 'i)
      (union-and-print p 'a 'b)
      (union-and-print p 'e 'f)
      (union-and-print p 'b 'c)
      )))

;;; end of file -- test.lisp --
