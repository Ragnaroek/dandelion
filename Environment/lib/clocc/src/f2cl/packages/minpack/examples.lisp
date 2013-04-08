;;;
;;; Here are some examples of using LMDIF1, taken from tst-lmdif.f.
;;;
;;;

;; A simplified interface to LMDIF1.  We dynamically allocate the work arrays.
(defun lisp-lmdif1 (fcn m-fcns n-vars x tol)
  (let* ((lwa (+ (* m-fcns n-vars) (* 5 n-vars) m-fcns))
	 (iwa (make-array n-vars :element-type 'integer4))
	 (wa (make-array lwa :element-type 'double-float))
	 (fvec (make-array m-fcns :element-type 'double-float :initial-element 0d0)))
    (flet ((minpack-fcn (m-f n-v x fvec iflag)
	     (let ((result (funcall fcn x fvec)))
	       (replace fvec result)
	       (values m-f n-v x fvec iflag))))
      (multiple-value-bind (var-fcn var-m-fcns var-n-vars var-x var-fvec var-tol var-info)
	  (lmdif1 #'minpack-fcn m-fcns n-vars x fvec tol 0 iwa wa lwa)
	(values var-x var-fvec var-info)))))

(defun sum-squares (x)
  (reduce #'+ x :key #'(lambda (z) (* z z))))

(defun minpack-test-4 (&key (tol (sqrt double-float-epsilon)))
  ;; Rosenbrock function
  (flet ((fcn (x fvec)
	   (let ((x1 (aref x 0))
		 (x2 (aref x 1)))
	     (setf (aref fvec 0) (* 10 (- x2 (expt x1 2))))
	     (setf (aref fvec 1) (- 1 x1))
	     fvec)))
    (let ((init-x (make-array 2 :element-type 'double-float
			      :initial-contents '(1.2d0 1d0))))
      (format t "Rosenbrock function~%")
      (format t "Initial X: ~A~%" init-x)
      (format t "L2[F(X)]:  ~A~%" (sum-squares (fcn init-x (make-array 2 :element-type 'double-float))))
      (multiple-value-bind (x fx info)
	  (lisp-lmdif1 #'fcn 2 2 init-x tol)
	(format t "Final X:   ~A~%" x)
	(format t "L2[F(X)]:  ~A~%" (sum-squares fx))
	(format t "Info:      ~A~%" info)
	(format t "True X:    ~A~%" #(1d0 1d0))))))

(defun minpack-test-7 (&key (tol (sqrt double-float-epsilon)))
  ;; Freudenstein and Roth function
  (flet ((fcn (x fvec)
	   (let ((x1 (aref x 0))
		 (x2 (aref x 1)))
	     (setf (aref fvec 0) (+ -13 x1 (* x2 (- (* (- 5 x2) x2) 2))))
	     (setf (aref fvec 1) (+ -29 x1 (* x2 (- (* (+ 1 x2) x2) 14))))
	     fvec)))
    (let ((init-x (make-array 2 :element-type 'double-float
			      :initial-contents '(0.5d0 -2d0))))
      (format t "Freudenstein function~%")
      (format t "Initial X: ~A~%" init-x)
      (format t "L2[F(X)]:  ~A~%" (sum-squares (fcn init-x (make-array 2 :element-type 'double-float))))
      (multiple-value-bind (x fx info)
	  (lisp-lmdif1 #'fcn 2 2 init-x tol)
	(format t "Final X:   ~A~%" x)
	(format t "L2[F(X)]:   ~A~%" (sum-squares fx))
	(format t "Info:      ~A~%" info)
	(format t "True X:    ~A~%" (vector (/ (- 53 (* 4 (sqrt 22d0))) 3)
					    (- (/ (- (sqrt 22d0) 2) 3))))))))

(defun minpack-test-12 (&key (m-fcns 10) (tol (sqrt double-float-epsilon)))
  (assert (>= m-fcns 3))
  ;; Box 3-dimensional function
  (flet ((fcn (x fvec)
	   (dotimes (i m-fcns)
	     (let* ((temp (float (1+ i) 1d0))
		    (tmp1 (/ temp 10d0)))
	       (setf (aref fvec i)
		     (+ (- (exp (- (* tmp1 (aref x 0))))
			   (exp (- (* tmp1 (aref x 1)))))
			(* (- (exp (- temp))
			      (exp (- tmp1)))
			   (aref x 2))))))
	     fvec))
    (let ((init-x (make-array 3 :element-type 'double-float
			      :initial-contents '(0d0 10d0 20d0))))
      (format t "Box 3-dimensional function~%")
      (format t "Initial X: ~A~%" init-x)
      (format t "L2[F(X)]:  ~A~%" (sum-squares (fcn init-x (make-array m-fcns :element-type 'double-float))))
      (multiple-value-bind (x fx info)
	  (lisp-lmdif1 #'fcn m-fcns 3 init-x tol)
	(format t "Final X:   ~A~%" x)
	(format t "L2[F(X)]:   ~A~%" (sum-squares fx))
	(format t "Info:      ~A~%" info)))))
  
(let ((y5 (make-array 65 :element-type 'double-float
		      :initial-contents
		      '(1.366D0 1.191D0 1.112D0 1.013D0 9.91D-1 8.85D-1 8.31D-1 
			8.47D-1 7.86D-1 7.25D-1 7.46D-1 6.79D-1 6.08D-1 6.55D-1 
			6.16D-1 6.06D-1 6.02D-1 6.26D-1 6.51D-1 7.24D-1 6.49D-1 
			6.49D-1 6.94D-1 6.44D-1 6.24D-1 6.61D-1 6.12D-1 5.58D-1 
			5.33D-1 4.95D-1 5.0D-1 4.23D-1 3.95D-1 3.75D-1 3.72D-1 
			3.91D-1 3.96D-1 4.05D-1 4.28D-1 4.29D-1 5.23D-1 5.62D-1 
			6.07D-1 6.53D-1 6.72D-1 7.08D-1 6.33D-1 6.68D-1 6.45D-1 
			6.32D-1 5.91D-1 5.59D-1 5.97D-1 6.25D-1 7.39D-1 7.1D-1 
			7.29D-1 7.2D-1 6.36D-1 5.81D-1 4.28D-1 2.92D-1 1.62D-1 
			9.8D-2 5.4D-2))))
  (defun minpack-test-18 (&key (tol (sqrt double-float-epsilon)))
    ;; Osborne 2 function
    (flet ((fcn (x fvec)
	     (dotimes (i 65)
	       (let* ((temp (/ i 10d0))
		      (tmp1 (exp (- (* (aref x 4) temp))))
		      (tmp2 (exp (- (* (aref x 5) (expt (- temp (aref x 8)) 2)))))
		      (tmp3 (exp (- (* (aref x 6) (expt (- temp (aref x 9)) 2)))))
		      (tmp4 (exp (- (* (aref x 7) (expt (- temp (aref x 10)) 2))))))
		 (setf (aref fvec i)
		       (- (aref y5 i)
			  (+ (* (aref x 0) tmp1)
			     (* (aref x 1) tmp2)
			     (* (aref x 2) tmp3)
			     (* (aref x 3) tmp4))))))
	     fvec))
      (let ((init-x (make-array 11 :element-type 'double-float
				:initial-contents
				'(1.3d0 6.5d-1 6.5d-1 7d-1 6d-1
				  3d0 5d0 7d0 2d0
				  4.5d0 5.5d0))))
	(format t "Osborne 2 function~%")
	(format t "Initial X: ~A~%" init-x)
	(format t "L2[F(X)]:  ~A~%" (sum-squares (fcn init-x (make-array 65 :element-type 'double-float))))
	(multiple-value-bind (x fx info)
	    (lisp-lmdif1 #'fcn 65 11 init-x tol)
	  (format t "Final X:   ~A~%" x)
	  (format t "L2[F(X)]:  ~A~%" (sum-squares fx))
	  (format t "Info:      ~A~%" info))))))


;;------------------------------------------------------------------------------
;;
;; $Log: examples.lisp,v $
;; Revision 1.1  2002/01/07 03:10:46  rtoy
;; Initial revision.
;;
;;------------------------------------------------------------------------------
