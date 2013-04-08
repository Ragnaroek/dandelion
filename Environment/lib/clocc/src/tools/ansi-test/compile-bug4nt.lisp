(in-package "USER")

(defun equal-terms (termx termy)
  (labels
      ((alpha-equal-bound-term-lists (listx listy)
	 (or (and (null listx) (null listy))
	     (and listx listy
		  (let ((bindings-x (bindings-of-bound-term (car listx)))
			(bindings-y (bindings-of-bound-term (car listy))))
		    (if (and (null bindings-x) (null bindings-y))
                        (alpha-equal-terms (term-of-bound-term (car listx))
                                           (term-of-bound-term (car listy)))
                        (and (= (length bindings-x) (length bindings-y))
                             (prog2
                                 (enter-binding-pairs (bindings-of-bound-term (car listx))
                                                      (bindings-of-bound-term (car listy)))
                                 (alpha-equal-terms (term-of-bound-term (car listx))
                                                    (term-of-bound-term (car listy)))
                               (exit-binding-pairs (bindings-of-bound-term (car listx))
                                                   (bindings-of-bound-term (car listy)))))))
		  (alpha-equal-bound-term-lists (cdr listx) (cdr listy)))))

       (alpha-equal-terms (termx termy)
	 (if (and (variable-p termx)
		  (variable-p termy))
             (equal-bindings (id-of-variable-term termx)
                             (id-of-variable-term termy))
             (and (equal-operators-p (operator-of-term termx) (operator-of-term termy))
                  (alpha-equal-bound-term-lists (bound-terms-of-term termx)
                                                (bound-terms-of-term termy))))))

    (or (eq termx termy)
	(and termx termy
	     (with-variable-invocation (alpha-equal-terms termx termy))))))
