;;;
;;; Some simple tests of the Quadpack routines, taken from the Quadpack book.
;;;
;;; $Id: quadpack-tests.lisp,v 1.5 2006/04/28 01:35:12 rtoy Exp $
;;; $Log: quadpack-tests.lisp,v $
;;; Revision 1.5  2006/04/28 01:35:12  rtoy
;;; In TST17 for DQAWC, the absolute error criterion was too small.  It
;;; must be strictly positive.  This test runs as expected.
;;;
;;; Revision 1.4  2002/03/19 23:12:33  rtoy
;;; It's f2cl-lib:integer4, not just plain integer4.
;;;
;;; Revision 1.3  2000/09/01 16:31:46  rtoy
;;; Add DO-TESTS to run all tests.
;;;
;;; Revision 1.2  2000/07/21 21:18:32  rtoy
;;; If besj0 and dgamma functions exist, use them in the solutions so we
;;; can compute the absolute error.
;;;
;;; Revision 1.1  2000/07/20 15:28:25  rtoy
;;; Initial revision
;;;
;;;

;;(declaim (optimize (speed 3)))

(declaim (inline square))

(defun square (x)
  (declare (type double-float x))
  (* x x))

(defun rel-error (est true)
  (abs (/ (- est true) est)))

(defun abs-error (est true)
  (abs (- est true)))

;; Test 1
;; Compute integral e^(alpha*x)*log(1/x) from 0 to 1.  The analytical
;; solution is e^((1+alpha)^(-2)).
;;
;; alpha = -0.9(0.1)0(0.2)2.6
;; QAG with key 1, 3, 6
;;
;; For key = 1, 3, 6: fails for alpha = -0.9 (ier = 3)
(defun tst1 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (expt (+ 1d0 alpha) -2))
	   (quad (alpha key)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last)
		   (dqag #'(lambda (x)
			       (declare (type (double-float 0d0) x))
			       (* (expt x alpha)
				  (log (/ x))))
			   0d0 1d0 0d0 relerr key
			   0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))
		 ))))
    (format t "Test integral 1:~%")
    (format t "
           1
          /                                      -2
          [    alpha x                (alpha + 1) 
        - I  %E        LOG(x) dx =  %E            
          ]
          /
           0

for alpha = -0.9(0.1)0(0.2)2.6

Characteristics: end-point singularity of integrand or derivative

Best integrator: QAG with key = 1 more efficient for pronounced
singularity.

Expect non-zero error codes for

alpha = -0.9 (ier = 3)
~3%")

    (dolist (key '(1 3 6))
      (format t "QAG, Key = ~d~%" key)
      (format t
	      "alpha   est result              est abserr             neval    ier   true answer            true abs err~%")
      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad alpha key))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad alpha key)))))


  
;; Test 2
;; Compute integral from 0 to 1 4^(-alpha)/((x - pi/4)^2 + 16^(-alpha))
;; Solution is atan((4-pi)*4^(alpha-1)) + atan(pi*4^(alpha-1)
;;
;; alpha = 0(1)20
;; QAG with key = 1, 3, 6
;;
;; Fails for key = 1: alpha >= 18 (ier = 2)
;; Fails for key = 3, 6: alpha >= 19 (ier = 2)
(defun tst2 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((z (expt 4d0 (- alpha 1d0))))
	       (declare (type double-float z))
	       (float (+ (atan (* (- 4d0 pi)
				  z))
			 (atan (* pi z)))
		      1d0)))
			 
	   (quad (alpha key)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last)
		   (dqag #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (flet ((square (x)
				      (declare (double-float x))
				      (* x x)))
			       (/ (expt 4d0 (- alpha))
				  (+ (square (- x (float (/ pi 4d0) 1d0)))
				     (expt 16d0 (- alpha))))))
			 0d0 1d0 0d0 relerr key
			 0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 2:
        1
       /
       [           1
       I  -------------------- dx
       ]       %PI 2      1
       /  (x - ---)  + -------
        0       4        alpha                      alpha                      alpha 
                       16                      %PI 4                (%PI - 4) 4	     
       --------------------------     =   ATAN(----------) + ATAN(- ----------------)
                  alpha			           4                       4         
                 4

for alpha = 0(1)20

Characteristics: integrand peak of height 4^alpha at x = pi/4.

Best integrator: QAG with key = 1

Expect non-zero error codes for

 Key = 1:     alpha >= 18 (ier = 2)
 Key = 3, 6:  alpha >= 19 (ier = 2)
~%~%
")
    (dolist (key '(1 3 6))
      (format t "~2&QAG; KEY = ~A~%" key)
      (format t
	      "alpha   est result              est abserr             neval    ier   true answer             true abs err~%")
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad alpha key)))))

;; Test 3
;; integral 0 pi cos(2^alpha*sin(x)) = pi * J0(2^alpha)
;;
;; alpha = 0(1)10
;;
;; QAG with Key 1, 3, 6
;;
(defun tst3 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (if (fboundp 'besj0)
		 (float (* pi (besj0 (expt 2 alpha)))
			1d0)
		 0d0))
			 
	   (quad (alpha key)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last)
		   (dqag #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (cos (* (expt 2 alpha) (sin x))))
			 0d0 (float pi 1d0) 0d0 relerr key
			 0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 3:
         %PI
        /
        [         alpha                         alpha	
        I    COS(2      SIN(x)) dx  =   %PI J0(2     )
        ]
        /
         0

for alpha = 0(1)10

Characteristics: integrand oscillates more strongly with increasing alpha.

Best integrator: QAG with key = 6 more efficient when integrand oscillates

No non-zero error codes expected.
~2%")
    (dolist (key '(1 3 6))
      (format t "~2&QAG; KEY = ~A~%" key)
      (format t
	      "alpha   est result              est abserr             neval    ier  true answer           true abs err~%")
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 10d0))
	(quad alpha key)))))

;; Test 4 (same integral as 1)
;; Compute integral e^(alpha*x)*log(1/x) from 0 to 1.  The analytical
;; solution is e^((1+alpha)^(-2)).
;;
;; DQNG, DQAGS, DQAG (key = 1)
;;
;; Failures:
;; DQNG: alpha <= 1.0 (ier = 1)
;; DQAG: alpha = -0.9 (ier = 3)
(defun tst4 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (expt (+ 1d0 alpha) -2))
	   (quad-qng (alpha)
	     (declare (double-float alpha))
	     (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier)
		 (dqng #'(lambda (x)
			   (declare (type (double-float 0d0) x))
			   (* (expt x alpha)
			      (log (/ x))))
		       0d0 1d0 0d0 relerr 
		       0d0 0d0 0 0)
	       (declare (ignorable junk a b epsabs epsrel result abserr neval ier))
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))
	       ))
	   (quad-qags (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (* (expt x alpha)
				 (log (/ x))))
			  0d0 1d0 0d0 relerr 
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))
		 )))
	   (quad-qag (alpha key)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last)
		   (dqag #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (* (expt x alpha)
				(log (/ x))))
			 0d0 1d0 0d0 relerr key
			 0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))
		 ))))
    (format t "Test integral 4:~%")
    (format t "
           1
          /                                      -2
          [    alpha x                (alpha + 1) 
        - I  %E        LOG(x) dx =  %E            
          ]
          /
           0

for alpha = -0.9(0.1)0(0.2)2.6

Best integrator:  QAGS without failures, more efficient and accurate than QAG

Expect non-zero error codes for

QNG: alpha <= 1.0 (ier = 1)
QAG: alpha = -0.9 (ier = 3)
~3%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))

      (format t "~2&QNG~%")
      (format t "~A~%" header)

      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad-qng alpha))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad-qng alpha))

      (format t "~2&DQAGS~%")
      (format t "~A~%" header)
      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad-qags alpha))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad-qags alpha))

      (format t "~2&DQAG~%")
      (format t "~A~%" header)
      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad-qag alpha 1))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad-qag alpha 1)))))

;; Test 5
;; Same integral as 2
;;
;; DQNG, DQAGS, DQAG (key = 1)
;;
;; Failures:
;; DQNG:  alpha >= 2 (ier = 1)
;; DQAGS: alpha >= 10 (ier = 5)
;; DQAG:  alpha >= 18 (ier = 2)
(defun tst5 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((z (expt 4d0 (- alpha 1d0))))
	       (declare (type double-float z))
	       (float (+ (atan (* (- 4d0 pi)
				  z))
			 (atan (* pi z)))
		      1d0)))
	   (quad-qng (alpha)
	     (declare (double-float alpha))
	     (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier)
		 (dqng #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (flet ((square (x)
				      (declare (double-float x))
				      (* x x)))
			       (/ (expt 4d0 (- alpha))
				  (+ (square (- x (float (/ pi 4d0) 1d0)))
				     (expt 16d0 (- alpha))))))
		       0d0 1d0 0d0 relerr 
		       0d0 0d0 0 0)
	       (declare (ignorable junk a b epsabs epsrel result abserr neval ier))
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))
	       ))
	   (quad-qags (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (flet ((square (x)
				      (declare (double-float x))
				      (* x x)))
			       (/ (expt 4d0 (- alpha))
				  (+ (square (- x (float (/ pi 4d0) 1d0)))
				     (expt 16d0 (- alpha))))))
			  0d0 1d0 0d0 relerr 
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))
		 )))
	   (quad-qag (alpha key)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last)
		   (dqag #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (flet ((square (x)
				      (declare (double-float x))
				      (* x x)))
			       (/ (expt 4d0 (- alpha))
				  (+ (square (- x (float (/ pi 4d0) 1d0)))
				     (expt 16d0 (- alpha))))))
			 0d0 1d0 0d0 relerr key
			 0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))
		 ))))
    (format t "Test integral 5:
        1
       /
       [           1
       I  -------------------- dx
       ]       %PI 2      1
       /  (x - ---)  + -------
        0       4        alpha                      alpha                      alpha 
                       16                      %PI 4                (%PI - 4) 4	     
       --------------------------     =   ATAN(----------) + ATAN(- ----------------)
                  alpha			           4                       4         
                 4

for alpha = 0(1)20

Best integrator: QAG applicable in relatively large alpha-range.

Expect non-zero error codes for

 QNG:  alpha >= 2 (ier = 1)
 QAGS: alpha >= 10 (ier = 5)
 QAG:  alpha >= 18 (ier = 2)  (key = 1)
~%~%
")

    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QNG~%")
      (format t "~A~%" header)
      (do ((alpha 0.0d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad-qng alpha))

      (format t "~2&QAGS~%")
      (format t "~A~%" header)
      (do ((alpha 0.0d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad-qags alpha))

      (format t "~2&QAG key = 1~%")
      (format t "~A~%" header)
      (do ((alpha 0.0d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad-qag alpha 1))
      )))

;; Test 6
;; Same integral as test 3
;;
;; DQNG, DQAGS, DQAG (key = 6)
;;
;; Failures:
;; DQNG: alpha >= 7 (ier = 1)
(defun tst6 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (if (fboundp 'besj0)
		 (float (* pi (besj0 (expt 2 alpha)))
			1d0)
		 0d0))
			 
	   (quad-qng (alpha)
	     (declare (double-float alpha))
	     (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier)
		 (dqng #'(lambda (x)
			   (declare (type (double-float 0d0) x))
			   (cos (* (expt 2 alpha) (sin x))))
		       0d0 (float pi 1d0)
		       0d0 relerr
		       0d0 0d0 0 0)
	       (declare (ignorable junk a b epsabs epsrel result abserr neval ier))
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))))
	   (quad-qags (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-lim z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (cos (* (expt 2 alpha) (sin x))))
			  0d0 (float pi 1d0)
			  0d0 relerr 
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qag (alpha key)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last)
		   (dqag #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (cos (* (expt 2 alpha) (sin x))))
			 0d0 (float pi 1d0) 0d0 relerr key
			 0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   )
    (format t "Test integral 6:
         %PI
        /
        [         alpha                         alpha	
        I    COS(2      SIN(x)) dx  =   %PI J0(2     )
        ]
        /
         0

for alpha = 0(1)10

Best integrator: QAG more efficient than QAGS

Expect non-zero error codes for
 QNG:  alpha >= 7 (ier = 1)
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))

      (format t "~2&QNG~%")
      (format t "~A~%" header)
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 10d0))
	(quad-qng alpha))
      (format t "~2&QAGS~%")
      (format t "~A~%" header)
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 10d0))
	(quad-qags alpha))

      (format t "~2&QAG Key = 6~%")
      (format t "~A~%" header)
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 10d0))
	(quad-qag alpha 6))
      )))

;; Test 7
;; Compute integral |x - 1/3|^alpha from 0 to 1
;; Solution: ((2/3)^(alpha + 1) + (1/3)^(alpha + 1))/(alpha + 1)
;;
;; alpha = -0.8(0.1)2.1
;; DQAGS, DQAGP (point of singularity supplied)
;;
;; No failures.
(defun tst7 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((a1 (+ 1d0 alpha)))
	       (/ (+ (expt (float 2/3 1d0) a1)
		     (expt (float 1/3 1d0) a1))
		  a1)))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (expt (abs (- x (float 1/3 1d0))) alpha))
			  0d0 1d0
			  0d0 relerr 
			  0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qagp (alpha)
	     (declare (double-float alpha))
	     (let* ((points (make-array 3 :element-type 'double-float
					:initial-element (float 1/3 1d0)))
		    (alist (make-array limit :element-type 'double-float))
		    (blist (make-array limit :element-type 'double-float))
		    (rlist (make-array limit :element-type 'double-float))
		    (elist (make-array limit :element-type 'double-float))
		    (pts (make-array 3 :element-type 'double-float))
		    (level (make-array limit :element-type 'f2cl-lib:integer4))
		    (ndin (make-array 3 :element-type 'f2cl-lib:integer4))
		    (iord (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b z-npts2 z-points epsabs epsrel z-limit result abserr neval ier z-a z-b z-r z-e z-pts z-iord z-level z-ndin last)
		   (dqagpe #'(lambda (x)
			       (declare (type (double-float 0d0) x))
			       (expt (abs (- x (float 1/3 1d0))) alpha))
			   0d0 1d0
			   3 points
			   0d0 relerr limit 
			   0d0 0d0 0 0
			   alist blist rlist elist
			   pts iord level ndin 0)
		 (declare (ignorable junk a b z-npts2 z-points epsabs epsrel z-limit result abserr neval ier z-a z-b z-r z-e z-pts z-iord z-level z-ndin last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 7:

       1
      /         alpha                 alpha		
      [  !    1!	           2 2      + 1	
      I  !x - -!      dx =     --------------------	
      ]  !    3!	                      alpha	
      /			       3 (alpha + 1) 3      
       0

for alpha = -0.8(0.1)2.1

Characteristics: integrand singularity at internal point with smal
binary period.

Best integrator: QAGS more efficient.

~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&DQAGS~%")
      (format t "~A~%" header)
      (do ((alpha -0.8d0 (+ alpha 0.1d0)))
	  ((> alpha 2.1d0))
	(quad alpha))

      (format t "~2& DQAGP~%")
      (format t "~A~%" header)
      (do ((alpha -0.8d0 (+ alpha 0.1d0)))
	  ((> alpha 2.1d0))
	(quad-qagp alpha)))))

;; Test 8
;; integral |x - pi/4|^alpha from 0 to 1
;; Solution: ((1-pi/4)^(alpha+1) + (pi/4)^(alpha + 1))/(alpha + 1)
;;
;; alpha = -0.8(0.1)2.1
;;
;; DQAGS, DQAGP
;;
;; Failures:
;; DQAGS: alpha <= -0.5 (ier = 3)

(defun tst8 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((a1 (+ 1d0 alpha)))
	       (coerce (/ (+ (expt #.(- 1d0 (/ pi 4d0)) a1)
			     (expt #.(/ pi 4d0) a1))
			  a1)
		       'double-float)))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (coerce (expt (abs (- x #.(/ pi 4d0))) alpha) 'double-float))
			  0d0 1d0 0d0 relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qagp (alpha)
	     (declare (double-float alpha))
	     (let* ((points (make-array 3 :element-type 'double-float
					:initial-element (float (/ pi 4) 1d0)))
		    (alist (make-array limit :element-type 'double-float))
		    (blist (make-array limit :element-type 'double-float))
		    (rlist (make-array limit :element-type 'double-float))
		    (elist (make-array limit :element-type 'double-float))
		    (pts (make-array 3 :element-type 'double-float))
		    (level (make-array limit :element-type 'f2cl-lib:integer4))
		    (ndin (make-array 3 :element-type 'f2cl-lib:integer4))
		    (iord (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b z-npts2 z-points epsabs epsrel z-limit
					  result abserr neval ier z-a z-b z-r z-e z-pts z-iord z-level z-ndin last)
		   (dqagpe #'(lambda (x)
			       (declare (type (double-float 0d0) x))
			       (coerce (expt (abs (- x #.(/ pi 4d0))) alpha) 'double-float))
			   0d0 1d0
			   3 points
			   0d0 relerr limit 
			   0d0 0d0 0 0
			   alist blist rlist elist
			   pts iord level ndin 0)
		 (declare (ignorable junk a b z-npts2 z-points epsabs epsrel z-limit
				     result abserr neval ier z-a z-b z-r z-e z-pts z-iord z-level z-ndin last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e  ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 8:

        1                          - alpha - 1    alpha + 1        %PI alpha + 1       
       /           alpha          4            %PI          + (1 - ---)	      
       [  !    %PI!	                                            4		      
       I  !x - ---!      dx  =    ----------------------------------------------     
       ]  !     4 !	                            alpha + 1                        
       /
        0


for alpha = -0.8(0.1)2.1

Characteristics: integrand singularity at internal point

Best integrator: QAGP wider applicable, more efficient and accurate.

Expect non-zero error code for:

 QAGS: alpha <= -0.5 (ier = 3)
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&DQAGS~%")
      (format t "~A~%" header)
      (dotimes (k 30)
	(let ((alpha (+ -0.8d0 (* k 0.1d0))))
	  (quad alpha)))

      (format t "~2&QAGP~%")
      (format t "~A~%" header)
      (dotimes (k 30)
	(let ((alpha (+ -0.8d0 (* k 0.1d0))))
	  (quad-qagp alpha))))))

;; Test 9
;; integral (1 - x*x)^(-1/2)/(x + 1 + 2^(-alpha))
;; Solution: pi*((1+2^(-alpha))^2-1)^(-1/2)
;;
;; alpha = 1(1)20
;;
;; DQAGS, DQAWS
;;
;; Failures:
;; DQAGS: alpha >= 17 (ier = 4 or 5)
(defun tst9 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (let ((2alpha (expt 2 (- alpha))))
	       (float (/ pi
			 (sqrt (* 2alpha (+ 2 2alpha))))
		      1d0)))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float -1d0 1d0) x))
			      (/ (/ (sqrt (- 1d0 (* x x))))
				 (+ x 1d0 (expt 2d0 (- alpha)))))
			  -1d0 1d0 0d0 relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qaws (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqaws #'(lambda (x)
			      (declare (type (double-float -1d0 1d0) x))
			      (/ (float (+ x 1d0 (expt 2d0 (- alpha))) 1d0)))
			  -1d0 1d0
			  -0.5d0 -0.5d0 1
			  0d0 relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 9:

      1
     /                                                   alpha      
     [                  1		            %PI 2	      
     I    ----------------------------- dx =    ------------------  
     ]           1                   2	                alpha	      
     /    (x + ------ + 1) SQRT(1 - x )	        SQRT(2 2      + 1)  
      - 1       alpha
               2



for alpha = 1(1)20

Characteristics: singular integrand factor (and end-points) times
factor which increases the effect of the singularity as alpha
increases.

Best integrator: QAWS wider applicable, more efficient and accurate.

Expect non-zero error code for:

 QAGS: alpha >= 17 (ier = 4 or 5)
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAGS~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad alpha))
      (format t "~2&QAWS~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad-qaws alpha)))))

;; Test 10
;; integral 0 to pi/2 (sin(x))^(alpha - 1) =
;; integral 0 to pi/2 x^(alpha - 1)*(sin(x)/x)^(alpha-1)
;; Solution: 2^(alpha - 2)*(Gamma(alpha/2))^2/Gamma(alpha)
;;
;; alpha = 0.1(0.1)2
;;
;; DQAGS, DQAWS
;; Failures:
;; None.
(defun tst10 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (if (fboundp 'dgamma)
		 (/ (* (expt 2 (- alpha 2))
		       (expt (dgamma (/ alpha 2)) 2))
		    (dgamma alpha))
		 0d0))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (expt (sin x) (- alpha 1)))
			   0d0 (float (/ pi 2) 1d0) 0d0 relerr
			   0d0 0d0 0 0
			   limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qaws (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last)
		 (dqaws #'(lambda (x)
			    (declare (type (double-float 0d0) x))
			    (let ((sinc (if (zerop x)
					    1d0
					    (/ (sin x) x))))
			      (expt sinc (- alpha 1))))
			   0d0 (float (/ pi 2) 1d0)
			   (- alpha 1) 0d0 1
			   0d0 relerr
			   0d0 0d0 0 0
			   limit lenw 0 iwork work)
		 (declare (ignorable junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 10:

      %PI                           %PI					    
      ---			    ---					    
       2			     2					    
     /				   /					    
     [          alpha - 1	   [     alpha - 1  SIN(x) alpha - 1	    
     I    SIN(x)          dx = 	   I    x          (------)          dx	 
     ]				   ]                  x			    
     /				   /					    
      0				    0                                        

                                      alpha  1	  
			         BETA(-----, -)  
			                2    2	  
                             =   --------------  
			               2         

					       alpha
			       SQRT(%PI) GAMMA(-----)
						 2
			     = ----------------------
					 alpha   1
				 2 GAMMA(----- + -)
					   2     2


for alpha = 0.1(0.1)2

Characteristics: integrand factor with algebraic end-point singularity
at x = 0 times a well-behaved factor.

Best integrator: QAWS more efficient and accurate

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAGS~%")
      (format t "~A~%" header)
      (do ((alpha 0.1d0 (+ alpha 0.1d0)))
	  ((> alpha 2.0d0))
	(quad alpha))
      (format t "~2&QAWS~%")
      (format t "~A~%" header)
      (do ((alpha 0.1d0 (+ alpha 0.1d0)))
	  ((> alpha 2.0d0))
	(quad-qaws alpha)))))

;; Test 11
;; integral 0 to 1 (log(1/x))^(alpha-1) =
;; integral 0 to 1 (1-x)^(alpha - 1)*(log(1/x)/(1-x))^(alpha-1)
;; Solution: Gamma(alpha)
;;
;; alpha = 0.1(0.1)2
;;
;; DQAGS, DQAWS
(defun tst11 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (if (fboundp 'dgamma)
		 (dgamma alpha)
		 0d0))

	   (quad (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (expt (log (/ x)) (- alpha 1)))
			   0d0 1d0 0d0 relerr
			   0d0 0d0 0 0
			   limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qaws (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last)
		 (dqaws #'(lambda (x)
			    (declare (type (double-float 0d0) x))
			    (let ((limit (if (= x 1)
					     1d0
					     (/ (log (/ x))
						(- 1 x)))))
			      (expt limit (- alpha 1))))
			   0d0 1d0
			   0d0 (- alpha 1) 1
			   0d0 relerr
			   0d0 0d0 0 0
			   limit lenw 0 iwork work)
		 (declare (ignorable junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 11:

    1                               1						
   /			           /						
   [            alpha - 1          [         alpha - 1    LOG(x) alpha - 1	
   I  (- LOG(x))          dx   =   I  (1 - x)          (- ------)          dx	
   ]			           ]                      1 - x		
   /			           /						
    0			            0                                          


                               = GAMMA(alpha)

for alpha = 0.1(0.1)2

Characteristics: integrand factor with algebraic end-point singularity
at x = 1 times well-behaved factor.

Best integrator: equivalent

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAGS~%")
      (format t "~A~%" header)
      (do ((alpha 0.1d0 (+ alpha 0.1d0)))
	  ((> alpha 2.0d0))
	(quad alpha))
      (format t "~2&QAWS~%")
      (format t "~A~%" header)
      (do ((alpha 0.1d0 (+ alpha 0.1d0)))
	  ((> alpha 2.0d0))
	(quad-qaws alpha)))))

;; Test 12
;; integral exp(20*(x-1))*sin(2^alpha*x) from 0 to 1 =
;; (20*sin(2^alpha) - 2^alpha*cos(2^alpha) + 2^alpha*exp(-20))/(400 + 4^alpha)
;;
;; alpha = 0(1)9
;;
;; DQAG (key = 6), DQAWO
;;
;; Failures:
;; None
(defun tst12 (&key (limit 200) (relerr 1d-8))
  (labels (
	   (soln (alpha)
	     (let ((2alpha (expt 2d0 alpha)))
	       (/ (+ (- (* 20 (sin 2alpha))
			(* 2alpha (cos 2alpha)))
		     (* 2alpha (exp -20d0)))
		  (+ 400 (expt 2alpha 2)))))
	   (quad-qag (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last)
		   (dqag #'(lambda (x)
			     (declare (type (double-float 0d0 1d0) x))
			     (* (exp (* 20d0 (- x 1)))
				(sin (* (expt 2d0 alpha) x))))
			 0d0 1d0 0d0 relerr 6
			 0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qawo (alpha)
	     (declare (double-float alpha))
	     (let* ((leniw limit)
		    (maxp1 100)
		    (lenw (+ (* 2 leniw) (* 25 maxp1)))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array leniw :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b omega integr epsabs epsrel result abserr neval ier z-leniw z-maxp1 z-lenw last)
		   (dqawo #'(lambda (x)
			      (declare (type (double-float 0d0 1d0) x))
			      (exp (* 20d0 (- x 1)))
			      )
			  0d0 1d0 (expt 2d0 alpha) 2 0d0 relerr
			  0d0 0d0 0 0
			  leniw maxp1 lenw 0 iwork work)
		 (declare (ignorable junk a b omega integr epsabs epsrel result abserr neval ier z-leniw z-maxp1 z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 12:

    1
   /
   [    20 (x - 1)      alpha
   I  %E           SIN(2      x) dx
   ]
   /
    0

		   alpha     alpha      alpha      - 20  alpha
	   20 SIN(2     ) - 2      COS(2     ) + %E     2
	=  ---------------------------------------------------
			      2 alpha
			     2        + 400

for alpha = 0(1)9

Characteristics: integrand factor sin(omega*x) times well-behaved factor.

Best integrator: QAWO more efficient for large alpha.

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAGS Key = 6~%")
      (format t "~A~%" header)
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 9d0))
	(quad-qag alpha))

      (format t "~2&QAWO~%")
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 9d0))
	(quad-qawo alpha))
      )))

;; Test 13
;; integral 0 to 1 (x*(1-x))^(-1/2)*cos(2^alpha)
;; Solution: cos(2^(alpha-1))*J0(2^(alpha - 1))
;;
;; alpha = 0(1)8
;;
;; DQAGS, DQAWO, DQAWS
;;
;; Failures:
;; DQAGS: alpha = 4 (ier = 5)
(defun tst13 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (if (fboundp 'besj0)
		 (* (float pi 1d0)
		    (cos (expt 2 (- alpha 1)))
		    (besj0 (expt 2 (- alpha 1))))
		 0d0))
	   (quad-qags (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqags #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (* (expt (* x (- 1 x)) -1/2)
				 (cos (* (expt 2 alpha) x))))
			  0d0 1d0 0d0 relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qawo (alpha)
	     (declare (double-float alpha))
	     (let* ((leniw limit)
		    (maxp1 100)
		    (lenw (+ (* 2 leniw) (* 25 maxp1)))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array leniw :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b omega integr epsabs epsrel result abserr neval ier z-leniw z-maxp1 z-lenw last)
		   (dqawo #'(lambda (x)
			      (declare (type (double-float -1d0 1d0) x))
			      (expt (* x (- 1 x)) -1/2))
			  1d-10 (- 1d0 1d-10) (expt 2d0 alpha) 1 0d0 relerr
			  0d0 0d0 0 0
			  leniw maxp1 lenw 0 iwork work)
		 ;; NOTE: integrand is singular at the end points and
		 ;; DQAWO evaluates the function at the end points.
		 ;; Thus, we changed the limits slightly.
		 (declare (ignorable junk a b omega integr epsabs epsrel result abserr neval ier z-leniw z-maxp1 z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qaws (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqaws #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (cos (* x (expt 2 alpha))))
			  0d0 1d0
			  -0.5d0 -0.5d0 1
			  0d0 relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b z-alfa z-beta z-int epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 13:

     1
    /        alpha
    [   COS(2      x)               alpha - 1       alpha - 1 
    I  --------------- dx =     J0(2         ) COS(2         )
    ]  SQRT((1 - x) x)
    /
     0


for alpha = 0(1)8

Characteristics: integrand factor cos(omega*x) times factor with
algebraic end-point singularity.

Best integrator: QAWS more efficient and accurate

Expect non-zero error codes for

 QAGS:  alpha = 4 (ier = 5)
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAGS~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 8.0d0))
	(quad-qags alpha))
      (format t "~2&QAWO~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 8.0d0))
	(quad-qawo alpha))
      (format t "~2&QAWS~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 8.0d0))
	(quad-qaws alpha)))))

;; Test 14
;; integral from 0 to b x^(-1/2)*exp(-2^(-alpha)*x) * cos(x) =
;; (1+eps)*sqrt(pi)*(1-4^(-alpha))^(-1/4)*cos(atan(2^alpha)/2)
;;
;; where eps = 0 if b = infinity and |eps| < 10^(-16) if b =
;; 20*2^alpha.
;;
;; alpha = 0(1)6
;;
;; DQAWF, DQAWO
;;
;; Failures:
;; None
(defun tst14 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (declare (double-float alpha))
	     (* (coerce (sqrt pi) 'double-float)
		(expt (+ 1 (expt 4 (- alpha))) -1/4)
		(cos (/ (atan (expt 2 alpha)) 2)))
	     )
	   (quad-qawf (alpha)
	     (declare (double-float alpha))
	     (let* ((leniw limit)
		    (limlst 10)
		    (maxp1 100)
		    (lenw (+ (* 2 leniw)
			     (* maxp1 25)))
		    (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a omega integr epsabs result abserr neval ier z-limlst z-lst z-leniw z-maxp1 z-lenw)
		   (dqawf #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (* (expt x -1/2)
				(exp (- (* (expt 2 (- alpha))
					   x))))
			     )
			 1d-10 1d0 1
			 relerr
			 0d0 0d0 0 0
			 limlst 0 leniw maxp1 lenw iwork work)
		 (declare (ignorable junk a omega integr epsabs result abserr neval ier z-limlst z-lst z-leniw z-maxp1 z-lenw))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha))))))
	   (quad-qawo (alpha)
	     (declare (double-float alpha))
	     (let* ((leniw limit)
		    (maxp1 100)
		    (lenw (+ (* 2 leniw) (* 25 maxp1)))
		    (work (make-array lenw :element-type 'double-float))
		    (iwork (make-array leniw :element-type 'f2cl-lib:integer4)))
	       (multiple-value-bind (junk a b omega integr epsabs epsrel result abserr neval ier z-leniw z-maxp1 z-lenw last)
		   (dqawo #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (* (expt x -1/2)
				 (exp (- (* (expt 2 (- alpha))
					    x))))
			      )
			  1d-10 (* 20 (expt 2 alpha)) 1d0 1 0d0 relerr
			  0d0 0d0 0 0
			  leniw maxp1 lenw 0 iwork work)
		 ;; NOTE: DQAWO evaluates the function at the
		 ;; end-points, and the function is singular there, so
		 ;; we changed the limits slightly.
		 (declare (ignorable junk a b omega integr epsabs epsrel result abserr neval ier z-leniw z-maxp1 z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 14:

             x
         - ------                             alpha			         
     b      alpha		        ATAN(2     )			       
    /      2			    COS(------------) (eps + 1) SQRT(pi)       
    [  %E         COS(x)	             2				       
    I  ----------------- dx   =     ------------------------------------       
    ]       SQRT(x)		                     1    1/4		       
    /				              (1 - ------)		       
     0				                    alpha		       
				                   4                           

where eps = 0 if b = infinity and |eps| < 1d-16 if b = 20*2^alpha.

for alpha = 0(1)6

Characteristics: infinite interval, integrand factor cos(x) times
factor with end-point singularity times factor which tends to zero
rapidly.

Best integrator: QAWF more efficient for alpha >= 6 and more accurate.

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAWF~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 6d0))
	(quad-qawf alpha))
      (format t "~2&QAWO~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 6d0))
	(quad-qawo alpha)))))


;; Test 15
;; integral 0 to b x^2*exp(-2^(-alpha)*x) = (1+eps)*2^(3*alpha + 1)
;; where eps = 0 if b = infinity and |e| < 10^(-16) if b = 20*2^alpha.
;;
;; alpha = 0(1)5
;;
;; DQAG (key = 6), DQAGI
;;
;; Failures:
;; None
(defun tst15 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha &optional limit)
	     (declare (double-float alpha))
	     (- (expt 2 (1+ (* 3 alpha)))
		(if limit
		    (let ((2alpha (expt 2 alpha)))
		      (* (exp (- (/ limit 2alpha)))
			 (+ (* limit limit)
			    (* 2 2alpha limit)
			    (* 2 2alpha))))
		    0))
	     )
	   (quad-qag (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (b (* 40 (expt 2 alpha)))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel key result abserr neval ier z-limit z-lenw last z-iwork z-work)
		   (dqag #'(lambda (x)
			     (declare (type (double-float 0d0) x))
			     (* x x (exp (* (- x) (expt 2 (- alpha)))))
			     )
			 0d0 b 0d0 relerr 6
			 0d0 0d0 0 0 limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel key result abserr neval ier z-limit z-lenw last z-iwork z-work))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha b) (abs-error result (soln alpha b))))))
	   (quad-qagi (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last)
		   (dqagi #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (* x x (exp (* (- x) (expt 2 (- alpha)))))
			      )
			  0d0 1 0d0 relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 15:

               x
     b      - ------
    /          alpha
    [   2     2                  
    I  x  %E         dx
    ]
    /
     0
                                                                                 b	   
		                                                             - ------  
		                                                                alpha  
		         3 alpha     alpha  2      2 alpha        3 alpha      2	   
                =     2 2        - (2      b  + 2 2        b + 2 2       ) %E          


                       3 alpha + 1	       	
                =     2            (eps + 1)


where eps = 0 if b = infinity and |eps| < 1d-14 if b = 40*2^alpha.

for alpha = 0(1)5

Characteristics: infinite interval, well-behaved integrand which tends
to zero rapidly.

Best integrator: QAG more efficient

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAG~%")
      (format t "~A~%" header)
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 5d0))
	(quad-qag alpha))
    
      (format t "~2&QAGI~%")
      (format t "~A~%" header)
      (do ((alpha 0d0 (+ alpha 1d0)))
	  ((> alpha 5d0))
	(quad-qagi alpha)))))

;; Test 16
;; integral 0 to infinity x^(alpha - 1)/(1+10*x)^2 =
;; 10^(-alpha)*(1-alpha)*pi/sin(pi*alpha)
;; if alpha /= 1.  Otherwise result = 1/10 when alpha = 1.
;;
;; alpha = 0.1(0.1)1.9
;;
;; DQAGI
;;
;; Failures: None
(defun tst16 (&key (limit 200) (relerr 1d-8))
  (labels ((soln (alpha)
	     (if (= alpha 1)
		 (float 1/10 1d0)
		 (float (/ (* pi (expt 10 (- alpha)) (- 1 alpha))
			   (sin (* pi alpha)))
			1d0)))
	   (quad-qagi (alpha)
	     (declare (double-float alpha))
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last z-iwork z-work)
		   (dqagi #'(lambda (x)
			      (declare (type (double-float 0d0) x))
			      (/ (expt x (- alpha 1))
				 (expt (+ 1 (* 10 x)) 2))
			      )
			  0d0 1 0d0 relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b epsabs epsrel result abserr neval ier z-limit z-lenw last z-iwork z-work))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 16:

    INF
   /     alpha - 1
   [    x                         %PI (1 - alpha)	
   I    ----------- dx   =     ----------------------
   ]              2	         alpha		
   /    (10 x + 1)	       10      SIN(%PI alpha)
    0

for alpha = 0.1(0.1)1.9

Characteristics: slowly convergent integral over infinite interval,
integrand with end-point singularity.

Best integrator:

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAGI~%")
      (format t "~A~%" header)
      (do ((alpha 0.1d0 (+ alpha 0.1d0)))
	  ((> alpha 1.9d0))
	(quad-qagi alpha)))))

;; Test 17
;; Cauchy integral 0 to 5 2^(-alpha)*(((x-1)^2 + 4^(-alpha))*(x-2))^(-1)
;; Solution: (2^(-alpha)*ln(3/2) - 2^(-alpha-1)*ln((16 + 4^(-alpha))/(1+4^(-alpha))) - atan(2^(alpha +  2)) - atan(2^alpha))/(1 + 4^(-alpha))
;;
;; alpha = 0(1)10
;;
;; DQAWC
;;
;; Failures: none
(defun tst17 (&key (limit 200) (relerr 1d-8) (abserr 1d-15))
  (labels ((soln (alpha)
	     (declare (type (double-float 0d0) alpha))
	     (/ (- (* (expt 2 (- alpha))
		      (log 1.5d0))
		   (* (expt 2 (- (- alpha) 1))
		      (log (/ (+ 16 (expt 4 (- alpha)))
			      (+ 1 (expt 4 (- alpha))))))
		   (atan (expt 2 (+ alpha 2)))
		   (atan (expt 2 alpha)))
		(+ 1 (expt 4 (- alpha)))))
	   (quad (alpha)
	     (let* ((lenw (* 4 limit))
		    (iwork (make-array limit :element-type 'f2cl-lib:integer4))
		    (work (make-array lenw :element-type 'double-float)))
	       (multiple-value-bind (junk a b c z-eps z-rel result abserr neval ier z-lim z-lenw last)
		   (dqawc #'(lambda (x)
			      (/ (expt 2 (- alpha))
				 (+ (expt (- x 1) 2)
				    (expt 4 (- alpha)))))
			  0d0 5d0 2d0
			  abserr relerr
			  0d0 0d0 0 0
			  limit lenw 0 iwork work)
		 (declare (ignorable junk a b c z-eps z-rel result abserr neval ier z-lim z-lenw last))
		 (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
			 alpha result abserr neval ier
			 (soln alpha) (abs-error result (soln alpha)))))))
    (format t "Test integral 17:

Cauchy principal value of

    5
   /
   [               1
   I  --------------------------- dx
   ]          2     1
   /  ((x - 1)  + ------) (x - 2)
    0              alpha
                  4
   ---------------------------------
                 alpha
                2


							       1
							     ------ + 16       3
							      alpha         ln(-)
		 alpha + 2          alpha     - alpha - 1    4                 2
	 - ATAN(2         ) - ATAN(2     ) - 2            ln(-----------) + ------
							       1             alpha
							     ------ + 1     2
							      alpha
							     4
    =    -------------------------------------------------------------------------
					  1
					------ + 1
					 alpha
					4

for alpha = 0(1)10

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAWC~%")
      (format t "~A~%" header)
      (do ((alpha 0.0d0 (+ alpha 1d0)))
	  ((> alpha 10d0))
	(quad alpha)))))

(defun do-tests ()
  (loop for k from 1 upto 17
	do
	(funcall (intern (format nil "TST~D" k)))))
