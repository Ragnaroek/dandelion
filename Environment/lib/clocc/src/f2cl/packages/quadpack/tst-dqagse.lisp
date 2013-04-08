;;;
;;; Some simple tests of the DQAGSE routine, taken from the Quadpack book.
;;;

(declaim (optimize (speed 3)))

(declaim (inline square))

(defun square (x)
  (declare (type double-float x))
  (* x x))

(defun rel-error (est true)
  (abs (/ (- est true) est)))

;; Compute integral e^(alpha*x)*log(1/x) from 0 to 1.  The analytical
;; solution is e^((1+alpha)^(-2)).
(defun tst1 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (expt (+ 1d0 alpha) -2))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let ((alist (make-array limit :element-type 'double-float))
		   (blist (make-array limit :element-type 'double-float))
		   (rlist (make-array limit :element-type 'double-float))
		   (elist (make-array limit :element-type 'double-float))
		   (iord (make-array limit :element-type 'integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel limit result abserr neval ier z-a z-b z-r z-e z-iord last)
		   (dqagse #'(lambda (x)
			       (declare (type (double-float 0d0) x))
			       (* (expt x alpha)
				  (log (/ x))))
			   0d0 1d0 0d0 relerr limit
			   0d0 0d0 0 0 alist blist rlist elist iord 0)
		 (format t "~5f  ~21,15g  ~21,15g  ~5d  ~5d  ~21,15g ~21,15g~%"
			 alpha result abserr neval ier
			 (soln alpha) (rel-error result (soln alpha)))
		 ))))
    (format t
"alpha  est result            est abserr             neval    ier  true answer           rel err~%")
    (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	((> alpha 0d0))
      (quad alpha))

    (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	((> alpha 2.6d0))
      (quad alpha))))

  

;; Compute integral 4^(-alpha)/((x - pi/4)^2 + 16^(-alpha)) from 0 to 1
(defun tst2 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((z (expt 4d0 (- alpha 1d0))))
	       (declare (type double-float z))
	       (+ (atan (* (- 4d0 pi)
			   z))
		  (atan (* pi z)))))
			 
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let ((alist (make-array limit :element-type 'double-float))
		   (blist (make-array limit :element-type 'double-float))
		   (rlist (make-array limit :element-type 'double-float))
		   (elist (make-array limit :element-type 'double-float))
		   (iord (make-array limit :element-type 'integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel limit result abserr neval ier z-a z-b z-r z-e z-iord last)
		   (dqagse #'(lambda (x)
			       (declare (type (double-float 0d0) x))
			       (flet ((square (x)
					(declare (double-float x))
					(* x x)))
				 (/ (expt 4d0 (- alpha))
				    (+ (square (- x (/ pi 4d0)))
				       (expt 16d0 (- alpha))))))
			   0d0 1d0 0d0 relerr limit
			   0d0 0d0 0 0 alist blist rlist elist iord 0)
		 (format t "~5f  ~21,15g  ~21,15g  ~5d  ~5d  ~21,15g  ~10e~%"
			 alpha result abserr neval ier
			 (soln alpha) (rel-error result (soln alpha)))))))
    (format t
"alpha  est result            est abserr             neval    ier  true answer           rel err~%")
    (do ((alpha 0d0 (+ alpha 1d0)))
	((> alpha 20d0))
      (quad alpha))))

;; Compute integral |x - 1/3|^alpha from 0 to 1
(defun tst7 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((a1 (+ 1d0 alpha)))
	       (/ (+ (expt (float 2/3 1d0) a1)
		     (expt (float 1/3 1d0) a1))
		  a1)))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let ((alist (make-array limit :element-type 'double-float))
		   (blist (make-array limit :element-type 'double-float))
		   (rlist (make-array limit :element-type 'double-float))
		   (elist (make-array limit :element-type 'double-float))
		   (iord (make-array limit :element-type 'integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel limit result abserr neval ier z-a z-b z-r z-e z-iord last)
		 (dqagse #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (expt (abs (- x (float 1/3 1d0))) alpha))
			 0d0 1d0 0d0 relerr limit
			 0d0 0d0 0 0 alist blist rlist elist iord 0)
	       (format t "~5f  ~21,15g  ~21,15g  ~5d  ~5d  ~21,15g  ~10e~%"
		       alpha result abserr neval ier
		       (soln alpha) (rel-error result (soln alpha)))))))
    (format t
"alpha  est result            est abserr             neval    ier  true answer           rel err~%")
    (do ((alpha -0.8d0 (+ alpha 0.1d0)))
	((> alpha 2.1d0))
      (quad alpha))))

;; integral |x - pi/4|^alpha from 0 to 1
(defun tst8 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((a1 (+ 1d0 alpha)))
	       (/ (+ (expt #.(- 1d0 (/ pi 4d0)) a1)
		     (expt #.(/ pi 4d0) a1))
		  a1)))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let ((alist (make-array limit :element-type 'double-float))
		   (blist (make-array limit :element-type 'double-float))
		   (rlist (make-array limit :element-type 'double-float))
		   (elist (make-array limit :element-type 'double-float))
		   (iord (make-array limit :element-type 'integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel limit result abserr neval ier z-a z-b z-r z-e z-iord last)
		 (dqagse #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (expt (abs (- x #.(/ pi 4d0))) alpha))
			 0d0 1d0 0d0 relerr limit
			 0d0 0d0 0 0 alist blist rlist elist iord 0)
	       (format t "~5f  ~21,15g  ~21,15g  ~5d  ~5d  ~21,15g  ~10e~%"
		       alpha result abserr neval ier
		       (soln alpha) (rel-error result (soln alpha)))))))
    (format t
"alpha  est result            est abserr             neval    ier  true answer           rel err~%")
    (do ((alpha -0.8d0 (+ alpha 0.1d0)))
	((> alpha 2.1d0))
      (quad alpha))))

;; integral (1 - x*x)^(-1/2)/(x + 1 + 2^(-alpha))
(defun tst9 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (/ pi
		(sqrt (- (square (+ 1d0 (expt 2d0 (- alpha))))
			 1d0))))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let ((alist (make-array limit :element-type 'double-float))
		   (blist (make-array limit :element-type 'double-float))
		   (rlist (make-array limit :element-type 'double-float))
		   (elist (make-array limit :element-type 'double-float))
		   (iord (make-array limit :element-type 'integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel limit result abserr neval ier z-a z-b z-r z-e z-iord last)
		   (dqagse #'(lambda (x)
			       (declare (type (double-float -1d0 1d0) x))
			       (/ (/ (sqrt (- 1d0 (* x x))))
				  (+ x 1d0 (expt 2d0 (- alpha)))))
			   -1d0 1d0 0d0 relerr limit
			   0d0 0d0 0 0 alist blist rlist elist iord 0)
		 (format t "~5f  ~21,15g  ~21,15g  ~5d  ~5d  ~21,15g ~10e~%"
			 alpha result abserr neval ier
			 (soln alpha) (rel-error result (soln alpha)))))))
    (do ((alpha 1d0 (+ alpha 1d0)))
	((> alpha 20d0))
      (quad alpha))))
