;;;; $Id: rng.lisp,v 1.15 2005/04/20 18:12:56 sds Exp $
;;;; $Source: /cvsroot/clocc/clocc/src/cllib/rng.lisp,v $
;;;;
;;;;  Class of Random number generators
;;;;
;;;;  $Log: rng.lisp,v $
;;;;  Revision 1.15  2005/04/20 18:12:56  sds
;;;;  removed (type random-state *random-state*) declarations because of
;;;;  http://www.lisp.org/HyperSpec/Body/sec_11-1-2-1-2.html item 11
;;;;  Reported by Ram Bhamidipaty <ram@verisity.com>
;;;;
;;;;  Revision 1.14  2005/03/08 22:50:54  sds
;;;;  export the interface functions
;;;;
;;;;  Revision 1.13  2005/03/08 21:33:26  sds
;;;;  use #+(or) instead of #+nil for portability
;;;;
;;;;  Revision 1.12  2005/03/08 21:32:21  sds
;;;;  STATE argument is always optional and binds *RANDOM-STATE*
;;;;
;;;;  Revision 1.11  2004/05/18 21:04:30  sds
;;;;  whitespace changes to help emacs
;;;;
;;;;  Revision 1.10  2001/11/04 07:02:08  sds
;;;;  (gen-exponential-variate-ziggurat): density has to accept 0d0 too
;;;;
;;;;  Revision 1.9  2001/11/02 22:31:15  sds
;;;;  prefix module names with `cllib-'
;;;;
;;;;  Revision 1.8  2001/08/27 17:21:33  rtoy
;;;;  o ZIGGURAT-INIT:  forgot to initialize x[0], fx[0], and fx[n].
;;;;    (Thanks to Clisp for catching this stupid error.)
;;;;  o Make TIME-EXPO, TIME-GAUSSIAN work for non-CMUCL systems too.
;;;;  o RNG-EXPO-HISTOGRAM should use PLOT-HIST-PDF.  Make it use m = 2, as
;;;;    a check that the generators work.
;;;;
;;;;  Revision 1.7  2001/08/27 13:53:24  rtoy
;;;;  o Change scaling in Ziggurat method for exponential variates because
;;;;    CMUCL on sparc doesn't convert (unsigned-byte 32) to floats very
;;;;    well.
;;;;  o Add timing info for CMUCL sparc.  Ziggurat method is the fastest
;;;;    still.
;;;;
;;;;  Revision 1.6  2001/08/26 13:50:33  rtoy
;;;;  Add Marsaglia's Ziggurat method for generating exponential and
;;;;  Gaussian variates.  Almost twice as fast as any of the others.
;;;;
;;;;  Revision 1.5  2001/03/29 21:21:56  sds
;;;;  (gen-exponential-variate-sa, gen-exponential-variate-algorithm-ma,
;;;;  gen-exponential-variate-ea, gen-exponential-variate-ea-2):
;;;;   added ignore declaration to avoid warnings
;;;;  (gen-std-laplacian-variate): use `gen-exponential-variate' (typo)
;;;;  (gen-gaussian-variate-box-trig): avoid CLISP floating contagion warning
;;;;
;;;;  Revision 1.4  2001/03/21 03:25:50  rtoy
;;;;  o Verify algorithms and add comments about which ones work and which
;;;;    ones don't
;;;;  o Add macros gen-<type>-variate to use the desired underlying
;;;;    generators.
;;;;  o Fix gamma generators that were calling non-existent Gaussian and
;;;;    exponential variates.
;;;;  o Some comment fixes.
;;;;
;;;;  Revision 1.3  2001/03/19 15:17:25  rtoy
;;;;  o Updated with several new generators for exponential.
;;;;  o Added and updated (but commented out) timing routines.
;;;;  o Added simple histogram routines used for testing whether the
;;;;    generators work or not.  (Commented out).
;;;;
;;;;  Revision 1.2  2001/03/15 00:04:01  sds
;;;;  added provide, in-package and use `dfloat'
;;;;
;;;;  Revision 1.1  2001/03/14 23:29:05  sds
;;;;  initial checkin
;;;;
;;;;  Revision 1.8  1999/11/17 16:48:11  toy
;;;;  o Correct some typos in the name of the exponential generator
;;;;    algorithms. (It's Knuth Algorith S, not F.)
;;;;  o Add another gamma generator (gen-gamma-variate-algo-a-2), based on
;;;;    Knuth's suggestion of using a polar method instead of computing
;;;;    tan(pi*u).
;;;;  o Add this new algorithm to the timing test.
;;;;
;;;;  Revision 1.7  1999/11/11 18:47:32  toy
;;;;  o Correct one bug in gen-std-exponential-algo-f (missing one shift).
;;;;    (The sample pdf is better, but it still seems to have some problems.
;;;;    The sample pdf for the log method is much, much better.)
;;;;  o Added generator for geometric distribution.
;;;;
;;;;  Revision 1.6  1999/11/08 17:49:58  toy
;;;;  Remove the output assertion in gen-std-exponential-variate-algo-f.
;;;;  (Speeds things up slightly.)
;;;;
;;;;  Revision 1.5  1999/11/08 16:01:53  toy
;;;;  o Added a deftype for non-negative floats.
;;;;  o Added Ahrens and Dieter's algorithm GO.
;;;;  o Added unstructured versions of some algorithm GN and GO.  These seem
;;;;    to run MUCH faster with MUCH less consing than the structured
;;;;    versions.  I don't know why that is.  They look the same to me.
;;;;
;;;;  Revision 1.4  1999/11/02 17:04:06  toy
;;;;  o Fix a bug in the exponential RV generator.
;;;;  o Move the classes out to another file.
;;;;  o Other minor random fixes.
;;;;
;;;;  Revision 1.3  1997/09/30 22:22:21  toy
;;;;  *** empty log message ***
;;;;
;;;;  Revision 1.2  1996/11/12 17:50:44  toy
;;;;  Lot's of changes that I don't remember, but lots of additions.
;;;;
;;;;  Revision 1.1  1996/10/24 22:12:10  toy
;;;;  Initial revision
;;;;

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `dfloat', `with-type'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype")))

(in-package :cllib)

(export '(gen-exponential-variate-log-method gen-exponential-variate-algo-s
          gen-exponential-variate-sa gen-exponential-variate-algorithm-ma
          gen-exponential-variate-ea gen-exponential-variate-ea-2
          gen-exponential-variate-ratio gen-exponential-variate-ziggurat
          gen-exponential-variate
          gen-std-laplacian-variate
          gen-cauchy-variate-tan gen-cauchy-variate
          gen-cauchy-variate-algorithm-ca
          gen-gaussian-variate-polar gen-gaussian-variate-algorithm-na
          gen-gaussian-variate-box-trig gen-gaussian-variate-ratio
          gen-gaussian-variate-ziggurat gen-gaussian-variate
          gen-gamma-variate-squeeze gen-gamma-variate-gn
          gen-gamma-variate-algo-a gen-gamma-variate-algo-a-2
          gen-gamma-variate-small-order gen-gamma-variate-direct
          gen-gamma-variate-algo-go gen-gamma-variate-ratio
          gen-gamma-variate
          gen-geometric-variate
          gen-beta-variate
          gen-binomial-variate
          gen-poisson-variate))

;; CLOCC should not do this, IMO:
;; (eval-when (compile)
;;   (declaim (optimize (speed 3))))

#+(and cmu negative-zero-is-not-zero)
(deftype non-negative-float (type &optional hi)
  `(or (,type ,(coerce 0 type) ,(or hi *))))

#-(and cmu negative-zero-is-not-zero)
(deftype non-negative-float (type &optional hi)
  `(or (member ,(coerce 0 type))
       (,type (,(coerce 0 type)) ,(or hi *))))

;; Initialize tables for Marsaglia's Ziggurat method of generating
;; random numbers.  See http://www.jstatsoft.org for a reference.
;;
;; Let 0 = x[0] < x[1] < x[2] <...< x[n].  Select a set of rectangles
;; with common area v such that
;;
;; x[k]*(f(x[k-1]) - f(x[k])) = v
;;
;; and
;;
;;              inf
;; v = r*f(r) + int f(x) dx
;;               r
;;
;; where r = x[n].
;;
(defun ziggurat-init (n r v scale f finv)
  ;; n = one less than the number of elements in the tables
  ;; r = x[n]
  ;; v = common area term
  ;; scale = 2^scale is the scaling to use to make integers
  ;; f = density function
  ;; finv = inverse density function
  (let ((x (make-array (1+ n) :element-type 'double-float))
	(fx (make-array (1+ n) :element-type 'double-float))
	(k-table (make-array (1+ n) :element-type '(unsigned-byte 32)))
	(w-table (make-array (1+ n) :element-type 'double-float)))
    (setf (aref x n) r)
    (loop for k from (1- n) downto 1 do
	  (let ((prev (aref x (1+ k))))
	    (setf (aref x k) (funcall finv (+ (/ v prev)
					      (funcall f prev))))
	    (setf (aref fx k) (funcall f (aref x k)))))

    (setf (aref x 0) 0d0)
    (setf (aref fx 0) (funcall f (aref x 0)))
    (setf (aref fx n) (funcall f (aref x n)))

    (loop for k from 1 to n do
	  (setf (aref k-table k)
		(floor (scale-float (/ (aref x (1- k)) (aref x k)) scale)))
	  (setf (aref w-table k)
		(* (aref x k) (expt .5d0 scale))))

    (setf (aref k-table 0) (floor (scale-float (/ (* r (funcall f r)) v) scale)))
    (setf (aref w-table 0) (* (/ v (funcall f r)) (expt 0.5d0 scale)))
    (values k-table w-table fx)))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Standard exponential random variate with mean m.
;;;;
;;;; f(x) = 1/m*e^{-x/m}, x >= 0
;;;; F(x) = 1 - e^{-x/m}, x >= 0
;;;;
;;;;-------------------------------------------------------------------------

;;; NOTE: Currently, some of methods for generating exponential
;;; variates seem to generate exponential variates that AREN'T
;;; actually exponential.  (Based on just looking at the histograms of
;;; some variates.)
;;;
;;; Here is a list of the ones that seem to work:
;;;
;;;    gen-exponential-variate-log-method
;;;    gen-exponential-variate-algo-s
;;;    gen-exponential-variate-ea
;;;    gen-exponential-variate-ea-2
;;;    gen-exponential-variate-ratio
;;;    gen-exponential-variate-ziggurat
;;;
;;; These don't seem to work:
;;;    gen-exponential-variate-algorithm-ma

;; GEN-EXPONENTIAL-VARIATE-LOG-METHOD
;;
;; Since the CDF is y = F(x) = 1 - e^(x/m), the inverse function is
;; -m*ln(1-y).  If y is a uniform random variate on [0,1), then
;; -m*ln(1-y) is exponential.  Note that if y is uniform, then 1-y is
;; also uniform, so we just use -m*ln(y).
;;
;; See Knuth, The Art of Computer Programming, Volume 2.

(defun gen-exponential-variate-log-method
    (mu &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from an exponential PDF with a
mean of mu:
                - X/MU
              %E
              --------
                 MU

 STATE is the random state to use.  The logarithmic method is used.
"
  (declare (type (double-float (0d0)) mu))
  (* mu (- (log (random 1d0)))))



;; This table is Knuth's Q[k] in Algorithm S.
;;
;; Q[k] = sum (ln 2)^k/k! for k = 1, 2,...,
;;
;; until Q[k] > 1 - 2^(-t) where the uniform deviates have t+1 bit
;; accuracy.
;;
;;
;; Here's how we generated this table.  I used CLISP to get the extra
;; digits in the table.
;;
;;(do* ((nbits (float-digits 1d0))
;;      (k 1 (1+ k))
;;      (term #.(log 2L0) (* term (/ #.(log 2L0) k)))
;;      (sum term (+ sum term)))
;;     ((or (> sum (- 1 (scale-float 1l0 (- nbits))))
;;          (> k 20)))
;;  (format t "~3D ~50,45F ~25G ~%" k sum term))
;;
;; Note that Q[k] -> 1 as k -> infinity.  Thus, we might get more
;; accurate results for large k if we compute
;;
;;            infty
;; Q[k] = 1 -  sum  (ln 2)^m/m!
;;            m=k+1
;;

(declaim (ftype (function ((double-float (0d0))
			   &optional random-state)
			  (non-negative-float double-float))
		gen-std-exponential-variate-algo-s))

(let ((std-exponential-variate-table
       (make-array 16
		   :element-type 'double-float
		   :initial-contents
		   '(
		     0d0
		     0.693147180559945309417232121458176568075d0
		     0.933373687519046021750783384621509053940d0
		     0.988877796183867601703925648390130811298d0
		     0.998495925291496078865904719963789676778d0
		     0.999829281106138923208245942162589294252d0
		     0.999983316410072739307790313135916717732d0
		     0.999998569143876799148070338574928727372d0
		     0.999999890692555813579019178950751556207d0
		     0.999999992473415905976016453850827533653d0
		     0.999999999528327526777139783726219715203d0
		     0.999999999972881353964220933485860571090d0
		     0.999999999998559789957709138627855373484d0
		     0.999999999999928938843099551515944568884d0
		     0.999999999999996726106647776972279059926d0
		     0.999999999999999858543354865400900694870d0
		     ))))
  (declare (type (simple-array double-float (16)) std-exponential-variate-table))

  ;; GEN-EXPONENTIAL-VARIATE-ALGO-S
  ;;
  ;; Knuth's Algorithm S for generating exponential random variates.
  ;;
  ;; 1.  Generate a (t+1)-bit uniform random binary fraction. U =
  ;; (0.b0 b1 b2...).  Locate the first zero bit b(j) and shift off
  ;; the leading j+1 bits, setting U = (0.b(j+1) b(j+2)...).
  ;;
  ;; 2.  If U < ln(2), set X = m*(j*ln(2) + U) and terminate
  ;;
  ;; 3.  Find the least k >= 2 such that U < Q[k].  Generate k new
  ;; uniform deviates U1, U2, ... U[k] and set V = min(U1, U2,...,
  ;; U[k]).
  ;;
  ;; 4.  Set X = m*(j + V)*ln(2)

  (defun gen-exponential-variate-algo-s
      (mu &optional (*random-state* *random-state*))
    "Generate a pseudo-random number drawn from an exponential PDF with a
mean of 1:

                - X
              %E

 for X >= 0.

 STATE is the random state to use.  Knuth's Algorithm S is used to
 generate the variate.
"
    (declare (type (double-float (0d0)) mu))

    (multiple-value-bind (u j)
	;; Step S1.  Find the first zero bit by comparing against 1/2.
	;; If more than 1/2, the leading bit is 1.  In this case,
	;; multiply by 2, and take the fractional part.  This drops the
	;; leading 1 bit.
	(let ((j 0)
	      (u (random 1d0)))
	  (declare (type (integer 0 100) j)
		   (type (double-float 0d0 1d0) u)
		   (optimize (speed 3) (safety 0)))
	  (loop while (> u 0.5d0) do
		(incf j)
		(setf u (- (+ u u) 1)))
	  (values (+ u u) j))

      (declare (type (integer 0 100) j))
      (let ((ln2 (aref std-exponential-variate-table 1)))
	(cond ((< u ln2)
	       (values (* mu (+ u (* j ln2)))))
	      (t
	       (do ((k 2 (+ k 1))
		    (v (min (random 1d0) (random 1d0))
		       (min v (random 1d0))))
		   ((<= u (aref std-exponential-variate-table k))
		    (values (* mu ln2 (+ j v))))
		 (declare (type (double-float 0d0 (1d0)) v)
			  (optimize (speed 3) (safety 0)))
		 ))))))

  ;; Ahrens Algorithm SA
  ;;
  ;; See Ahrens, CACM, vol 15, no. 10, 1972.
  ;;
  ;; 1.  Initialize a = 0 and generate a uniform variate u.  Let b be
  ;; the first bit of u after the binary point.
  ;;
  ;; 2.  If b = 1 (u >= 0.5) goto 4.
  ;;
  ;; 3.  If b = 0 (u < 0.5) set a = a + ln(2) and shift u to the left
  ;; by one bit (u = u + u).  This produces a new first bit b.  Go to
  ;; 2.
  ;;
  ;; 4.  Shift u to the left by one bit (u = u + u - 1).  If u >
  ;; ln(2), go to 6.
  ;;
  ;; 5.  If u <= ln(2), deliver x = a + u.
  ;;
  ;; 6.  Initialize i = 2 and generate u*.  umin = u*.
  ;;
  ;; 7.  Generate a new u*.  umin = min(umin, u*).
  ;;
  ;; 8.  If u > q[i], i = i + 1 and go to 7.
  ;;
  ;; 9.  If u <= q[k], deliver a + umin*ln(2).
  ;;
  ;; Notes:
  ;;
  ;; 1.  In the original paper, step 9 has a small typo where an
  ;; extraneous "x" is added.
  ;;
  ;; 2.  This appears to be exactly the same as Knuth's algorithm S,
  ;; but written in a floating point fashion.  Step S1 is the same as
  ;; steps 1-4.  Step S2 is step 5.  Step S3-S4 are steps 6-9.  This
  ;; also shows a typo.  In Knuth: Step S4 reads m*(j + V)*ln(2).
  ;; Step 9 here should read "deliever (a + umin)*ln(2)".

  (defun gen-exponential-variate-sa
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    (let ((a 0d0)
	  (u (random 1d0)))
      (declare (double-float a u))
      (loop
	  (tagbody
	   step-2
	     (when (>= u 0.5d0)
	       (go step-4))
	   step-3
	     (incf a (aref std-exponential-variate-table 1))
	     (setf u (+ u u))
	     (go step-2)
	   step-4
	     (setf u (+ u u -1))
	     (when (<= u (aref std-exponential-variate-table 1))
	       (return-from gen-exponential-variate-sa (+ a u)))
	   step-6
	     (let* ((i 2)
		    (u* (random 1d0))
		    (umin u*))
	       (declare (fixnum i)
			(type (double-float 0d0 1d0) u* umin))
	       (loop
		   (tagbody
		    step-7
		      (setf umin (min umin (random 1d0)))
		      (if (> u (aref std-exponential-variate-table i))
			  (progn
			    (incf i)
			    (go step-7))
			  (return-from gen-exponential-variate-sa (* (+ a umin (aref std-exponential-variate-table 1))))))))))))
  )

;; GEN-EXPONENTIAL-VARIATE-ALGORITHM-MA
;;
;; This is Algorithm MA given in Ahrens, but proposed by Marsaglia.
;;
;; See Ahrens, CACM, vol. 15, no. 10, 1972.
;;
;; p[k] = 1 - exp(-k)
;; q[k] = (1/1! + 1/2! + ... + 1/k!)/(e - 1)
;;
;; 1.  Initialize i = 0 and generate uniform variate u.
;;
;; 2.  If u <= p[i+1], go to 4.
;;
;; 3.  If u > p[i+1], i = i + 1, and go to 2.
;;
;; 4.  Initialize k = 1 and generate u and u*.  Set umin = u*.
;;
;; 5.  If u <= q[k], go to 8.
;;
;; 6.  If u > q[k], k = k + 1.
;;
;; 7.  Generate a new u*, and set umin=min(umin, u*).  Go to 5.
;;
;; 8.  Deliver x = i + umin.
;;
;; Here's how to compute the constants p[k] and q[k].  kernel:%expm1
;; is the function exp(x)-1, but done carefully to preserve accuracy.
;;
;; (do* ((k 0 (+ 1 k))
;; 	       (pk 0d0 (- (float (kernel:%expm1 (float (- k) 1L0)) 1d0)))
;; 	       (init (list pk) (cons pk init)))
;; 	     ((>= pk 1d0)
;; 	      (make-array (length init)
;; 			  :element-type 'double-float
;; 			  :initial-contents (nreverse init))))
;; (do* ((k 1 (+ 1 k))
;; 	       (term 1 (/ term k))
;; 	       (sum 1 (+ sum term))
;; 	       (init (list 0) (cons sum init)))
;; 	      ((>= (/ sum #.(- (exp 1d0) 1))
;; 		   1)
;; 	       (make-array (length init)
;; 			   :element-type 'double-float
;; 			   :initial-contents (mapcar #'(lambda (x)
;; 							 (/ x #.(- (exp 1d0) 1)))
;; 						     (nreverse init)))))

(let ((p
       (make-array 39
		   :element-type 'double-float
		   :initial-contents
		   '(0.0d0 0.6321205588285577d0 0.8646647167633873d0 0.950212931632136d0
		     0.9816843611112658d0 0.9932620530009145d0 0.9975212478233336d0
		     0.9990881180344455d0 0.9996645373720975d0 0.9998765901959134d0
		     0.9999546000702375d0 0.9999832982992097d0 0.9999938557876467d0
		     0.999997739670593d0 0.9999991684712809d0 0.9999996940976795d0
		     0.9999998874648253d0 0.9999999586006229d0 0.9999999847700203d0
		     0.9999999943972036d0 0.9999999979388464d0 0.999999999241744d0
		     0.9999999997210532d0 0.9999999998973812d0 0.9999999999622486d0
		     0.9999999999861121d0 0.9999999999948909d0 0.9999999999981205d0
		     0.9999999999993086d0 0.9999999999997456d0 0.9999999999999064d0
		     0.9999999999999656d0 0.9999999999999873d0 0.9999999999999953d0
		     0.9999999999999983d0 0.9999999999999993d0 0.9999999999999998d0
		     0.9999999999999999d0 1d0)))
      (q
       (make-array 17
		   :element-type 'double-float
		   :initial-contents
		   '(0.0d0 0.8729650603039897d0 0.9699611781155442d0 0.9942102075684327d0
		     0.9990600134590104d0 0.9998683144407733d0 0.9999837860095966d0
		     0.9999982199556996d0 0.9999998237274889d0 0.9999999841046677d0
		     0.9999999986844113d0 0.9999999998993898d0 0.9999999999928497d0
		     0.9999999999995255d0 0.9999999999999706d0 0.9999999999999983d0 1d0))))
  (declare (type (simple-array double-float (39)) p)
	   (type (simple-array double-float (17)) q))
  (defun gen-exponential-variate-algorithm-ma
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    ;; Find i such that u <= p[i+1].
    (let* ((u (random 1d0))
	   (i (do ((k 1 (+ 1 k)))
		  ((<= u (aref p k))
		   (- k 1)))))
      ;; Find k such that min(u1, u2,...,uk) <= q[k].  Then return i +
      ;; min(u1,...,uk)
      (do* ((new-u (random 1d0))
	    (k 1 (+ 1 k))
	    (umin (random 1d0) (min umin (random 1d0))))
	  ((<= new-u (aref q k))
	   (+ i umin))
	(declare (type (double-float 0d0 1d0) umin))))))

;; GEN-EXPONENTIAL-VARIATE-EA
;;
;; Ahrens Algorithm EA for generating exponential random variates. The
;; description given by Ahrens contains several typos which were
;; corrected by Kenneth G. Hamilton.  We describe the algorithm here,
;; with corrections made by Hamilton.
;;
;; See Ahrens, CACM, vol. 31, no. 11, 1988.
;; See Hamilton, ACM Trans on Math. Software, vol. 24, no. 1, 1998.
;;
;; 0.  Constants:
;;       a (* (log 2d0) (+ 4 (* 3 (sqrt 2d0))))
;;       b (+ 2 (sqrt 2d0)))
;;       c (- (* (log 2d0) (+ 1 (sqrt 2d0))))
;;       p (* (sqrt 2d0) (log 2d0))
;;       A (* a p))
;;       B (* b p))
;;       H 0.0026106723602095d0
;;       D (/ (* b b))))
;; 1.  Generate uniform variate U and set G = c.
;;
;; 2.  Set U = U + U.  If U >= 1 go to 4.
;;
;; 3.  Set G = G + ln(2) and go to 2.
;;
;; 4.  Set U = U - 1.  If U > p go to 6.
;;
;; 5.  Return X = G + A/(B - U).
;;
;; 6.  Generate U and set Y = a / (b - U).
;;
;; 7.  Generate U'.  If (U'*H + D)*(b - U)^2 > exp(-(Y + c), go to 6.
;;
;; 8.  Return X = G + Y.
;;
(let* ((ln-2 #.(log 2d0))
       (a #.(* (log 2d0) (+ 4 (* 3 (sqrt 2d0)))))
       (b #.(+ 2 (sqrt 2d0)))
       (c #.(- (* (log 2d0) (+ 1 (sqrt 2d0)))))
       (p #.(* (sqrt 2d0) (log 2d0)))
       (big-a (* a p))
       (big-b (* b p))
       (big-h 0.0026106723602095d0)
       (big-d (/ (* b b))))
  (declare (type double-float ln-2 a b c p big-a big-b big-h big-d))

  (defun gen-exponential-variate-ea
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    (declare (type (double-float (0d0)) mu)
	     (optimize (speed 3)))
    (let ((u (random 1d0))
	  (u1 0d0)
	  (y 0d0)
	  (g c))
      (declare (type (double-float 0d0) u u1)
	       (type double-float g)
	       (type double-float y))
      (loop
	  (tagbody
	   step-2
	     (setf u (+ u u))
	     (when (>= u 1)
	       (go step-4))
	     (setf g (+ g ln-2))
	     (go step-2)
	   step-4
	     (setf u (- u 1))
	     (when (> u p)
	       (go step-6))
	     (return-from gen-exponential-variate-ea (+ g (/ big-a (- big-b u))))
	   step-6
	     (setf u (random 1d0))
	     (setf y (/ a (- b u)))
	     (setf u1 (random 1d0))
	     (when (> (* (+ (* u1 big-h) big-d)
			 (expt (- b u) 2))
		      (exp (- (+ y c))))
	       (go step-6))
	     (return-from gen-exponential-variate-ea (+ g y))))))

  (defun gen-exponential-variate-ea-2
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    (let ((u (random 1d0))
	  (g c))
      (declare (double-float u g))
      (setf u (+ u u))
      (do ()
	  ((>= u 1))
	(incf g ln-2)
	(incf u u))

      (decf u 1)
      (when (<= u p)
	(return-from gen-exponential-variate-ea-2 (+ g (/ big-a (- big-b u)))))
      (loop
	  (let* ((u (random 1d0))
		 (y (/ a (- b u)))
		 (up (random 1d0)))
	    (declare (double-float u y up))
	    (when (<= (* (+ (* up big-h) big-d)
			 (expt (- b u) 2))
		      (exp (- (+ y c))))
	      (return-from gen-exponential-variate-ea-2 (+ g y)))))))
    )

;; Use the ratio-of-uniforms method to generate exponential variates.
;; This could probably be optimized further.
(defun gen-exponential-variate-ratio
    (mu &optional (*random-state* *random-state*))
  (declare (type (double-float (0d0)) mu))
  (let ((max-v (* mu #.(* 2 (exp -1d0)))))
  (do ((u (random 1d0) (random 1d0))
       (v (random max-v) (random max-v)))
      ((<= (* u u) (exp (- (/ v u mu))))
       (/ v u)))))

;; Marsaglia's Ziggurat method for generating exponential
;; variates. Note: this is slightly different from the version given
;; in his paper.  We changed the scaling from 2^32 to 2^31 because
;; CMUCL 18c on sparc doesn't do a good job of converting
;; (unsigned-byte 32) to floating-point because it doesn't have such
;; an instruction (only signed-bytes).  (Apparently x86 does.)  Tests
;; show that good exponential numbers are still generated.
(let ((r 7.69711747013104972d0))
  (flet ((density (x)
	   (declare (type (double-float 0d0) x))
	   (exp (- x))))
    (declare (inline density))
    (multiple-value-bind (k-table w-table f-table)
	(ziggurat-init 255 r 0.0039496598225815571993d0 31
		       #'density
		       #'(lambda (x)
			   (- (log x))))
      (defun gen-exponential-variate-ziggurat
          (mu &optional (*random-state* *random-state*))
	(declare (type (double-float 0d0) mu)
                 (optimize (speed 3)))
	(loop
	    (let* ((j (random (ash 1 31)))
		   (i (logand j 255))
		   (x (* j (aref w-table i))))
	      (when (< j (aref k-table i))
		(return (* mu x)))
	      (when (zerop i)
		(return (* mu (- r (log (random 1d0))))))
	      (when (< (* (random 1d0) (- (aref f-table (1- i))
						(aref f-table i)))
		       (- (density x) (aref f-table i)))
		(return (* mu x)))))))))

;;; Some timing results from running CMUCL 18c+ on a 866 MHz Pentium
;;; III:
;;;
;;; (cllib::time-expo 5000000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Log		1.97	1.66	0.27	79998968
;;; Algo S	2.15	1.81	0.3	79998968
;;; SA		1.99	1.59	0.35	79998968
;;; EA		2.7	2.38	0.29	79998968
;;; EA-2	2.34	1.92	0.38	79998976
;;; Ratio	4.18	3.75	0.3	79998976
;;; Zigg	1.28	0.94	0.32	79998976
;;;
;;; On this platform, Margaglia's Ziggurat method is far and away the
;;; fastest.
;;;
;;; For CMUCL 18c+ (with sparc-v9 changes) running on a 300 MHz Ultra 30:
;;; (cllib::time-expo 5000000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Log		1.63	1.5	0.02	16000480
;;; Algo S	1.44	1.24	0.12	16000480
;;; SA		1.45	1.25	0.1	16000480
;;; EA		1.17	1.06	0.08	16000480
;;; EA-2	1.33	1.07	0.11	16000480
;;; Ratio	3.31	3.02	0.11	16000480
;;; Zigg	0.81	0.62	0.15	16000480
;;;
;;; So the Ziggurat method is quite a bit faster on this platform too.
;;;
;;; Pick the one that works best for you.

(defmacro gen-exponential-variate (mu &optional (state '*random-state*))
  `(gen-exponential-variate-ziggurat ,mu ,state))


;;;;-------------------------------------------------------------------------
;;;;
;;;; Laplacian PDF
;;;;
;;;; f(x) = 1/2*exp(-|x|)
;;;;
;;;;-------------------------------------------------------------------------

(defun gen-std-laplacian-variate (&optional (*random-state* *random-state*))
  "Generate a pseudo-random number for a Laplacian random variable, defined by

         1    -|X|
 f(X) = --- %E
         2

 for real X.
"
  ;; Instead of using the inverse CDF to generate the random number,
  ;; we generate an exponential and flip its sign with probability 1/2.
  (if (zerop (random 2))
      (gen-exponential-variate 1d0)
      (- (gen-exponential-variate 1d0))))

;;;;-------------------------------------------------------------------------
;;;; Cauchy random variate.
;;;;
;;;; f(x) = 1/pi/(1 + x^2)
;;;; F(x) = 1/2 - 1/pi*atan(x)
;;;;
;;;;-------------------------------------------------------------------------

;; GEN-CAUCHY-VARIATE-TAN
;;
;; Use the inverse of the CDF to generate the desired Cauchy variate.

(defun gen-cauchy-variate-tan (&optional (*random-state* *random-state*))
  (tan (* #.(dfloat pi)
	  (- (random 1d0) 0.5d0))))

(declaim (inline gen-cauchy-variate-algorithm-ca-aux))

;; GEN-CAUCHY-VARIATE-ALGORITHM-CA
;;
;; Ahrens (1988), Algorithm CA for generating Cauchy variates.
;;
;; 0.  Constants
;;      a 0.6380631366077803d0
;;      b 0.5959486060529070d0
;;      q 0.9339962957603656d0
;;      W 0.2488702280083841d0
;;      A 0.6366197723675813d0
;;      B 0.5972997593539963d0
;;      H 0.0214949004570452d0
;;      P 4.9125013953033204d0
;;
;; 1.  Generate U.  Set T = U - 1/2 and S = W - T^2.  If S <= 0, go to
;; 3.
;;
;; 2.  Return X = T*(A/S+B).
;;
;; 3.  Generate U.  Set T = U - 1/2 and s = 1/4 - T^2 and X = T*(a/s +
;; b).
;;
;; 4.  Generate U'.  If s^2*((1 + X^2)*(H*U' + P) - q) + s > 1/2, go
;; to 3.
;;
;; 5.  Return X.
;;

(let ((a 0.6380631366077803d0)
      (b 0.5959486060529070d0)
      (q 0.9339962957603656d0)
      (ww 0.2488702280083841d0)
      (aa 0.6366197723675813d0)
      (bb 0.5972997593539963d0)
      (hh 0.0214949004570452d0)
      (pp 4.9125013953033204d0))
  (declare (double-float a b q ww aa bb hh pp))
  (defun gen-cauchy-variate-algorithm-ca-aux
      (u &optional (*random-state* *random-state*))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (optimize (speed 3) (safety 0) (space 0)))
    (let* ((tt (- u 0.5d0))
	   (s (- ww (* tt tt))))
      (when (> s 0)
	(return-from gen-cauchy-variate-algorithm-ca-aux (* tt (+ (/ aa s) bb))))

      (do ((u (random 1d0) (random 1d0))
	   (u1 (random 1d0) (random 1d0)))
	  (nil)
	(let* ((tt (- u 0.5d0))
	       (s (- 1/4 (* tt tt)))
	       (x (* tt (+ (/ a s) b))))
	  (when (<= (+ s (* s s
			    (- (* (+ 1 (* x x))
				  (+ (* hh u1) pp))
			       q)))
		    1/2)
	    (return-from gen-cauchy-variate-algorithm-ca-aux x))))))
  (defun gen-cauchy-variate-algorithm-ca
      (&optional (*random-state* *random-state*))
    (gen-cauchy-variate-algorithm-ca-aux (random 1d0)))
  )

;;; Select the one that works best for you.

(defmacro gen-cauchy-variate (&optional (state '*random-state*))
  `(gen-cauchy-variate-tan ,state))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Gaussian random variate
;;;;
;;;; f(x) = 1/sqrt(2*pi) e^{-1/2 * x^2}
;;;;
;;;;-------------------------------------------------------------------------

;; A simple structure to hold the cached value for the Gaussian
;; generators, which usually generate two variates at a time.
(defstruct gaussian-generator-cache
  (cached-value 0d0 :type double-float)
  (cache-valid nil :type (member t nil) ))

(let ((cache (make-gaussian-generator-cache)))
  (defun gen-gaussian-variate-polar (&optional (*random-state* *random-state*))
    (cond ((gaussian-generator-cache-cache-valid cache)
	   (setf (gaussian-generator-cache-cache-valid cache) nil)
	   (gaussian-generator-cache-cached-value cache))
	  (t
	   (do* ((u1 (- (* 2 (random 1d0)) 1)
		     (- (* 2 (random 1d0)) 1))
		 (u2 (- (* 2 (random 1d0)) 1)
		     (- (* 2 (random 1d0)) 1))
		 (w (+ (* u1 u1) (* u2 u2))
		    (+ (* u1 u1) (* u2 u2))))
		((<= w 1)
		 (locally
		   (declare (type (non-negative-float double-float 1d0) w))
		   (let ((s (sqrt (/ (* -2 (log w)) w))))
		     (setf (gaussian-generator-cache-cached-value cache) (* u2 s))
		     (setf (gaussian-generator-cache-cache-valid cache) t)
		     (* u1 s)))))))))

;; GEN-GAUSSIAN-VARIATE-ALGORITHM-NA
;;
;; Ahrens Algorithm NA for normal variates.
;;
;; 2.  Generate U.  Save the first bit B of U.  (B = 0 if U < 1/2.
;; Otherwise B = 1).
;;
;; 3.  Generate E, a standard exponential deviate and set S = E + E.
;;
;; 4.  Generate C, a standard Cauchy deviate.  Use algorithm CA but
;; instead of "Generate U" in Step 1 of CA, reuse the remaining bits
;; of U from Step 2.
;;
;; 5.  Set X = sqrt(S/(1+C^C)) and save Y = C*X.
;;
;; 6.  If B = 0, return X.  Otherwise return -X.
;;
;; On the next call, return Y

#+(or)
(let ((save 0d0)
      (gen -1))
  (declare (double-float save)
	   (fixnum gen))
  (defun gen-gaussian-variate-algorithm-na
      (&optional (*random-state* *random-state*))
    (declare (optimize (speed 3) (safety 0)))
    (setf gen (- gen))
    (cond ((= gen 1)
	   (let* ((u (random 1d0))
		  (e (gen-exponential-variate-log-method 1d0))
		  (s (+ e e))
		  (b 0))
	     (declare (type (non-negative-float double-float) u))
	     (cond ((< u 1/2)
		    (setf u (+ u u)))
		   (t
		    (setf b 1)
		    (setf u (+ u u -1))))
	     (let* ((c (gen-cauchy-variate-algorithm-ca-aux u))
		    (x (sqrt (/ s (+ 1 (* c c)))))
		    (y (* c x)))
	       (setf save y)
	       (if (zerop b)
		   x
		   (- x)))))
	  (t
	   save))))

(let ((cache (make-gaussian-generator-cache)))
  (defun gen-gaussian-variate-algorithm-na
      (&optional (*random-state* *random-state*))
    (declare (optimize (speed 3) (safety 0)))
    (cond ((gaussian-generator-cache-cache-valid cache)
	   (setf (gaussian-generator-cache-cache-valid cache) nil)
	   (gaussian-generator-cache-cached-value cache))
	  (t
	   (let* ((u (random 1d0))
		  (e (gen-exponential-variate-log-method 1d0))
		  (s (+ e e))
		  (b 0))
	     (declare (type (non-negative-float double-float) u))
	     (cond ((< u 1/2)
		    (setf u (+ u u)))
		   (t
		    (setf b 1)
		    (setf u (+ u u -1))))
	     (let* ((c (gen-cauchy-variate-algorithm-ca-aux u))
		    (x (sqrt (/ s (+ 1 (* c c)))))
		    (y (* c x)))
	       (setf (gaussian-generator-cache-cached-value cache) y)
	       (setf (gaussian-generator-cache-cache-valid cache) t)
	       (if (zerop b)
		   x
		   (- x))))))))

(let ((cache (make-gaussian-generator-cache)))
  (defun gen-gaussian-variate-box-trig
      (&optional (*random-state* *random-state*))
    "Generate a pseudo-random number drawn from a Gaussian PDF with a mean
of zero and a variance of 1.  The PDF is

                    2
                   X
                 - --
                   2
               %E
          ----------------
          SQRT(2) SQRT(PI)


 STATE is the random state to use.
 The Box-Mueller method is used but the acceptance/rejection method is
 replaced by direct calculation via trigonometric functions..  See
 Knuth, Seminumerical Algorithms.
"
    (cond ((gaussian-generator-cache-cache-valid cache)
	   (setf (gaussian-generator-cache-cache-valid cache) nil)
	   (gaussian-generator-cache-cached-value cache))
	  (t
	   (let ((r1 (sqrt (* -2d0 (log (random 1d0)))))
		 (r2 (random #.(* (dfloat pi) 2d0))))
	     ;;(declare (double-float r1 r2))
	     (setf (gaussian-generator-cache-cached-value cache)
		   (* r1 (sin r2)))
	     (setf (gaussian-generator-cache-cache-valid cache) t)
	     (* r1 (cos r2)))))))

(let ((+sqrt-8/e+ #.(sqrt (/ 8.0d0 (exp 1d0))))
      (+4-exp-1/4+ #.(* 4.0d0 (exp 0.25d0)))
      (+4-exp-minus-1.35+ #.(* 4.0d0 (exp (- 1.35d0)))))
  (declare (double-float +sqrt-8/e+ +4-exp-1/4+ +4-exp-minus-1.35+))
  (defun gen-gaussian-variate-ratio (&optional (*random-state* *random-state*))
    "Generate a pseudo-random number drawn from a Gaussian PDF with a mean
of zero and a variance of 1.

                    2
                   X
                 - --
                   2
               %E
          ----------------
          SQRT(2) SQRT(PI)

 STATE is the random state to use.

 The ratio of uniform variates method is used.  See Knuth,
 Seminumerical Algorithms, Algorithm R.
"
    (do ((u (random 1d0) (random 1d0)))
	(nil)
      (declare (double-float u))
      (let* ((x (/ (* +sqrt-8/e+
		      (- (random 1d0) 0.5d0))
		   u))
	     (xs (* x x)))
	(declare (double-float x xs))
	(if (or (<= xs (- 5.0d0 (* u +4-exp-1/4+)))
		(and (<= xs (+ 1.4d0 (/ +4-exp-minus-1.35+ u)))
		     (<= xs (- (* 4.0d0 (log u))))))
	    (return-from gen-gaussian-variate-ratio x))))))

;; Marsaglia's Ziggurat method for Gaussians
(let ((r 3.442619855899d0))
  (flet ((density (x)
	   (declare (double-float x)
		    (optimize (speed 3) (safety 0)))
	   (exp (* -0.5d0 x x))))
    (declaim (inline density))
    (multiple-value-bind (k-table w-table f-table)
	(ziggurat-init 127 r 9.91256303526217d-3 31
		       #'density
		       #'(lambda (x)
			   (sqrt (* -2 (log x)))))
      (defun gen-gaussian-variate-ziggurat
          (&optional (*random-state* *random-state*))
	(declare (optimize (speed 3)))
	(loop
	    ;; We really want a signed 32-bit random number. So make a
	    ;; 32-bit unsigned number, take the low 31 bits as the
	    ;; number, and use the most significant bit as the sign.
	    ;; Doing this in other ways can cause consing.
	    (let* ((ran (random (ash 1 32)))
		   (sign (ldb (byte 1 31) ran))
		   (j (if (plusp sign)
			  (- (ldb (byte 31 0) ran))
			  (ldb (byte 31 0) ran)))
		   (i (logand j 127))
		   (x (* j (aref w-table i))))
	      (when (< (abs j) (aref k-table i))
		(return x))
	      (when (zerop i)
		(loop
		    (let ((x (/ (- (log (random 1d0))) r))
			  (y (- (log (random 1d0)))))
		      (when (> (+ y y) (* x x))
			(return-from gen-gaussian-variate-ziggurat
			  (if (plusp j)
			      (- (+ r x))
			      (+ r x)))))))
	      (when (< (* (random 1d0) (- (aref f-table (1- i))
						(aref f-table i)))
		       (- (density x) (aref f-table i)))
		(return x))))))))


;;; Some timing results for CMUCL 18c+ on a 866 MHz Pentium III:
;;;
;;; (cllib::time-gaussian 1000000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Polar	0.45	0.39	0.05	15994880
;;; NA		0.88	0.71	0.16	39997432
;;; Box/Trig	0.5	0.4	0.08	16003064
;;; Ratio	0.58	0.5	0.08	16003064
;;; Zigg	0.28	0.22	0.05	16437240
;;;
;;; Based on these results, Marsalia's Ziggurat method is far and away
;;; the fastest.
;;;
;;; Some timing results for CMUCL 18c+ (sparc-v9) on a 300 MHz Ultra
;;; 30:
;;;
;;; (cllib::time-gaussian 500000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Polar	0.83	0.78	0.02	 8000480
;;; NA		1.03	0.83	0.12	20000480
;;; Box/Trig	1.39	1.15	0.06	 8000480
;;; Ratio	1.27	0.93	0.08	 8000480
;;; Zigg	0.42	0.33	0.06	 8220912
;;;
;;; Based on these results, Marsalia's Ziggurat method is far and away
;;; the fastest on this platform too.

;;; Select one that works for you.
(defmacro gen-gaussian-variate (&optional (state '*random-state*))
  `(gen-gaussian-variate-ziggurat ,state))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Gamma random variate, of order a
;;;;
;;;; f(x) = 1/Gamma(a) x^{a-1} e^{-x}
;;;;
;;;;-------------------------------------------------------------------------

;;;
;;; Here are some timing results for the gamma generators
;;;
;;; 10000 numbers generated.  Time is elapsed real time in seconds,
;;; Second number is bytes consed.  (For the Ultra-30, 50000 numbers
;;; were generated.)
;;;
;;; Order = 1.1
;;; CPU	          Squeeze  GN         Algo. A           Algo GO
;;; 486-66        0.75     39.2       0.94
;;;               525k     11738k     391k
;;; Sparc20       0.19      9.0       0.25
;;;               335k     11493k     160k
;;; U-30 (300)    0.05      2.9       0.13
;;;               334k     12401k     160k
;;;
;;; Order = 10
;;; CPU	          Squeeze  GN         Algo. A  Direct
;;; 486-66        .71      14.9       0.93
;;;               498k     5026k      420k
;;; Sparc20       0.17      3.6       0.22
;;;               321k     4890k      160k
;;; U-30 (300)    0.28      5.00      0.30     0.31       0.26
;;;               2411k    21700k     800k     1600k      1874k
;;;
;;; Order = 100
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        .70      13.1       0.93
;;;               496k     4254k      442k
;;; Sparc20       0.17      3.5       0.22
;;;               321k     4102k      160k
;;; U-30 (300)    0.28      0.28      0.31     2.54       0.22
;;;               2400k    1674k      800k     1600k      1663k
;;;
;;; Order = 1000
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        .70      12.1       0.96
;;;               496k     4017k      450k
;;; Sparc20       0.17      2.8       0.22
;;;               321k     3878k      160k
;;; U-30 (300)    0.23      0.33      0.32                0.22
;;;               1600k    1627k      800k                1626k
;;;
;;; Order = 10000
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        0.67     10.9       0.95
;;;               496K     3950K      458k
;;; Sparc20       0.17      2.6       0.25
;;;               321k     3800k      160k
;;; U-30 (300)    0.24      0.26      0.33                0.25
;;;               1600k    1612k      800k                1619k
;;;

#+(or)
(defun gen-gamma-variate-squeeze
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above),
            ORDER >= 1.
  STATE   = random state to use.

  This uses Marsaglia's squeeze method.
"
  (declare (type (double-float 1d0) order))
  ;; Marsaglia's squeeze method for gamma variates.  This method is
  ;; valid for all order >= 1/3.  However, its efficiency gets better
  ;; with larger orders.  Thus, we want order to be at least 1.

  (let* ((s (/ #.(dfloat 1/3) (sqrt order)))
	 (z0 (- 1d0 (* s #.(sqrt 3d0))))
	 (cs (- 1d0 (* s s)))
	 (x0 (- s #.(sqrt 3d0)))
	 (cc (- (* order z0 z0 z0)
		(* 0.5d0 x0 x0)))
	 (cl (- 1d0 (* 3d0 order))))
    (do* ((x (gen-gaussian-variate)
	     (gen-gaussian-variate))
	  (z (+ (* s x) cs)
	     (+ (* s x) cs)))
	 (nil)
      (declare (double-float x z))
      (when (plusp z)
	(let* ((z z)
	       (rgama (* order z z z))
	       (e (gen-exponential-variate 1d0))
	       (cd (- (+ e (* 0.5d0 x x) cc)
		      rgama))
	       (tt (- 1d0 (/ z0 z))))
	  (declare (double-float e)
		   (type (non-negative-float double-float) z))
	  (when (or (plusp (- cd
			      (* cl tt
				 (+ 1d0 (* tt
					   (+ 0.5d0
					      (* #.(dfloat 1/3) tt)))))))
		    (>= (- cd (* cl (log (/ z z0))))
			0d0))
	    (return-from gen-gamma-variate-squeeze rgama)))))))

(defun gen-gamma-variate-squeeze
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above),
            ORDER >= 1.
  STATE   = random state to use.

  This uses Marsaglia's squeeze method.
"
  (declare (type (double-float 1d0) order))
  ;; Marsaglia's squeeze method for gamma variates.  This method is
  ;; valid for all order >= 1/3.  However, its efficiency gets better
  ;; with larger orders.  Thus, we want order to be at least 1.

  (let* ((s (/ #.(dfloat 1/3) (sqrt order)))
	 (z0 (- 1d0 (* s #.(sqrt 3d0))))
	 (cs (- 1d0 (* s s)))
	 (x0 (- s #.(sqrt 3d0)))
	 (cc (- (* order z0 z0 z0)
		(* 0.5d0 x0 x0)))
	 (cl (- 1d0 (* 3d0 order)))
	 (x 0d0)
	 (z 0d0))
    (declare (type double-float x z))
    (loop
	(tagbody
	  step-1
	   (setf x (gen-gaussian-variate))
	   (setf z (+ (* x s) cs))
	   (when (<= z 0)
	     (go step-1))
	   (let* ((z z)
		  (rgama (* order z z z))
		  (e (gen-exponential-variate 1d0))
		  (cd (- (+ e (* 0.5d0 x x) cc)
			 rgama))
		  (tt (- 1d0 (/ z0 z))))
	     (declare (double-float e)
		      (type (non-negative-float double-float) z))
	     (when (or (plusp (- cd
				 (* cl tt
				    (+ 1d0 (* tt
					      (+ 0.5d0
						 (* #.(dfloat 1/3) tt)))))))
		       (>= (- cd (* cl (log (/ z z0))))
			   0d0))
	       (return-from gen-gamma-variate-squeeze rgama)))))))


#+(or)
(defun gen-gamma-variate-gn (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1.
  STATE   = random state to use.

  This uses Ahrens and Dieter's Algorithm GN
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; Ahrens and Dieter Algorithm GN for gamma variates
  (let* ((mu (- order 1d0))
	 (sigma (sqrt (+ order (* (sqrt order) #.(sqrt (dfloat 8/3))))))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d)))
    ;;(declare (double-float mu sigma d b))
    (do ((u (random 1d0)
	    (random 1d0)))
	(nil)
      (declare (type (non-negative-float double-float (1d0)) u))
      (cond ((> u 0.009572265238289d0)
	     (let* ((s (gen-gaussian-variate))
		    (x (+ mu (* sigma s))))
	       (when (and (<= 0 x b)
			  (<= (log (random 1d0))
			      (- (+ (* mu
				       (+ 1d0
					  (log (/ (the (non-negative-float double-float) x)
						  mu))))
				    (* 0.5d0 s s))
				 x)))
		 (return-from gen-gamma-variate-gn x))))
	    (t
	     (let* ((s (gen-exponential-variate 1d0))
		    (x (* b (+ 1d0 (/ s d)))))
	       (when (<= (log (random 1d0))
			 (- (+ (* mu
				  (- (+ 2d0
					(log (/ x mu)))
				     (/ x b)))
			       3.7203284924588704d0
			       (log (the (non-negative-float double-float) (/ (* sigma d) b))))
			    b))
		 (return-from gen-gamma-variate-gn x))))))))

;; This unstructured version (which exactly follows the algorithm
;; description) is significantly faster than the structured version
;; above.  For example, for order = 100d0 and 50000 trials, the above
;; takes 4.4 sec, but this version takes 0.3 sec.  This version also
;; cons about 10 times less.  I don't know why that is.
(defun gen-gamma-variate-gn (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1.
  STATE   = random state to use.

  This uses Ahrens and Dieter's Algorithm GN
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; Ahrens and Dieter Algorithm GN for gamma variates
  (let* ((mu (- order 1d0))
	 (sigma (sqrt (+ order (* (sqrt order) #.(sqrt (dfloat 8/3))))))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d))
	 (x 0d0)
	 (s 0d0)
	 (u 0d0))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (type double-float x s))
    (loop
	(tagbody
	  step-2
	   (setf u (random 1d0))
	   (when (<= u 0.009572265238289d0)
	     (go step-5))
	   (setf s (gen-gaussian-variate))
	   (setf x (+ mu (* sigma s)))
	   (when (or (< x 0) (> x b))
	     (go step-2))

	   (setf u (random 1d0))
	   (if (> (log u)
		  (- (+ (* mu
			   (+ 1d0
			      (log (the (non-negative-float double-float)
				     (/ x mu)))))
			(* 0.5d0 s s))
		     x))
	       (go step-2)
	       (return-from gen-gamma-variate-gn x))
	  step-5
	   (setf s (gen-exponential-variate 1d0))
	   (setf x (* b (+ 1 (/ s d))))
	   (setf u (random 1d0))
	   (if (> (log u)
		  (- (+ (* mu
			   (- (+ 2d0
				 (log (the (non-negative-float double-float) (/ x mu))))
			      (/ x b)))
			3.7203284924588704d0
			(log (the (non-negative-float double-float) (/ (* sigma d) b))))
		     b))
	       (go step-2)
	       (return-from gen-gamma-variate-gn x))))))

(defun gen-gamma-variate-algo-a
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1
  STATE   = random state to use.

  This uses Algorithm A, in Knuth, Seminumerical Algorithms.
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; The large order case. This is Algorithm A, sec 3.4.1 E.
  (let* ((a order)
	 (sqrt2a-1 (sqrt (- (* 2d0 a) 1d0)))
	 (a-1 (- a 1d0)))
    (declare (type (double-float (1d0)) a))
    (do* ((y (tan (* #.(dfloat pi) (random 1d0)))
	     (tan (* #.(dfloat pi) (random 1d0))))
	  (x (+ (* sqrt2a-1 y) a-1)
	     (+ (* sqrt2a-1 y) a-1)))
	 ((and (> x 0d0)
	       (<= (random 1d0)
		   (* (+ 1d0 (* y y))
		      (exp (- (* a-1 (log (/ (the (double-float (0d0)) x)
					     a-1)))
			      (* sqrt2a-1 y))))))
	  x))))

;; Knuth mentions that instead of computing tan(pi*u), we can use a
;; polar method.  This implements that idea.
;;
;; Some simple timing tests show that this isn't any faster than the
;; above on an Ultra-30 300 MHz.  I guess the tan function is very
;; fast (or the CMUCL's RNG is slow).
(defun gen-gamma-variate-algo-a-2
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1
  STATE   = random state to use.

  This uses Algorithm A, in Knuth, Seminumerical Algorithms.
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; The large order case. This is Algorithm A, sec 3.4.1 E.
  (let* ((a order)
	 (sqrt2a-1 (sqrt (- (* 2d0 a) 1d0)))
	 (a-1 (- a 1d0)))
    (declare (type (double-float (1d0)) a))
    (flet ((tan-pi-u ()
	     (do* ((u (- (random 2d0) 1d0)
		      (- (random 2d0) 1d0))
		   (v (- (random 2d0) 1d0)
		      (- (random 2d0) 1d0))
		   (s (+ (* u u) (* v v))
		      (+ (* u u) (* v v)))
		   )
		((< s 1d0)
		 (/ v u)))))
      (do* ((y (tan-pi-u) (tan-pi-u))
	    (x (+ (* sqrt2a-1 y) a-1)
	       (+ (* sqrt2a-1 y) a-1)))
	   ((and (> x 0d0)
		 (<= (random 1d0)
		     (* (+ 1d0 (* y y))
			(exp (- (* a-1 (log (/ (the (double-float (0d0)) x)
					       a-1)))
				(* sqrt2a-1 y))))))
	    x)))))

(defun gen-gamma-variate-small-order
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            0 < ORDER < 1.
  STATE   = random state to use.

 This uses the method given in problem 16, in Knuth, Seminumerical
 Algorithms.
"
  (declare (type (double-float (0d0) (1d0)) order))
  ;; order < 1.  This is the algorithm in problem 16 in Sec. 3.4.1, in
  ;; Knuth.
  (let ((p (/ #.(exp 1d0) (+ order #.(exp 1d0))))
	(recip-order (/ order))
	(a-1 (- order 1d0)))
    (do* ((u (random 1d0)
	     (random 1d0))
	  (v (random 1d0)
	     (random 1d0))
	  (x (if (< u p) (expt v recip-order) (- 1d0 (log v)))
	     (if (< u p) (expt v recip-order) (- 1d0 (log v))))
	  (q (if (< u p) (exp (- x)) (expt x a-1))
	     (if (< u p) (exp (- x)) (expt x a-1))))
	 ((< (random 1d0) q)
	  x)
      (declare (type (non-negative-float double-float (1d0))
		     u v)
	       (type (double-float (0d0)) x)))))

(defun gen-gamma-variate-direct
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER > 0
  STATE   = random state to use.

 This uses a direct method to generate the random variates.  It uses
 the fact that the sum of two gamma's of order A and B is also a gamma
 of order A + Note that gamma of order 1 is exponential and we know
 how to generate that easily.  This is only good for moderate orders.
 For non-integral orders, the small-order algorithm is called.
"
  (declare (type (double-float (0d0) (20d0)) order))
  ;; Direct generation of Gamma variates using the fact that the sum
  ;; of two gamma's of order A and B is also a gamma of order A +
  ;; B. Note that gamma of order 1 is exponential and we know how to
  ;; generate that easily.  This is only good for moderate orders.
  ;; Use one of the obove generators for higher orders.
  (multiple-value-bind (n r)
      (floor order)
    (declare (fixnum n)
	     #+(or)(type (double-float 0d0) r))
    (let ((x 1d0))
      (declare (type (double-float (0d0)) x))
      ;; Sum up the exponential variates here.  This is done my
      ;; multiplying the uniform variates and taking the log at
      ;; the end, instead of summing the log of uniform variates.
      (dotimes (k n)
	(declare (fixnum k))
	(setf x (* x (random 1d0))))
      ;; If we still have some left, add in a gamma variate of
      ;; the remaining order.
      (if (zerop r)
	  (- (log x))
	  (- (gen-gamma-variate-small-order r)
	     (log x))))))

(eval-when (compile eval)
  (defconstant +beta-algo-go+ 0.009572265238289d0)
  (declaim (type (double-float 0.009572265238289d0 0.009572265238289d0)
		 +beta-algo-go+))
  )

;; Ahrens and Dieter's Algorithm GO.
#+(or)
(defun gen-gamma-variate-algo-go (a &optional (*random-state* *random-state*))
  (declare (type (double-float (2.5327805161251d0)) a)
	   (optimize (speed 3)))
  (let* ((mu (- a 1))
	 (v (sqrt a))
	 (sigma-2 (+ a #.(sqrt (/ 8d0 3d0))))
	 (sigma (sqrt sigma-2))
	 (w (/ sigma-2 mu))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d)))
    ;;(declare (type (double-float 0d0) w))

    (do ((u (random 1d0)
	    (random 1d0)))
	(nil)
      (cond ((<= u +beta-algo-go+)
	     ;; Step 8 and 9
	     (let ((x (* b (+ 1 (/ (gen-exponential-variate 1d0) d))))
		   (u (random 1d0)))
	       (when (<= (log u) (- (+ (* mu (+ 2 (log (/ x mu)) (- (/ x b))))
				       #.(- (log (/ (* (sqrt (* 2 pi)) +beta-algo-go+)
						  (- 1 +beta-algo-go+)))))
				    (+ b (log (/ (* sigma d) b)))))
		 (return-from gen-gamma-variate-algo-go x))))

	    (t
	     ;; Step 3
	     (let* ((s (gen-gaussian-variate))
		    (x (+ mu (* sigma s))))
	       (if (<= 0 x b)
		   (let ((x x)
			 (u (random 1d0))
			 (big-s (* 0.5d0 s s)))
		     (declare (type (non-negative-float double-float) x))
		     (if (< s 0)
			 (when (<= u (- 1 (* big-s (- (- 1 (* 2 (/ s v))) 1))))
			   (return-from gen-gamma-variate-algo-go x))
			 (when (<= u (- 1 (* big-s (- w 1))))
			   (return-from gen-gamma-variate-algo-go x)))
		     (when (<= (log u)
			       (+ (* mu (+ 1 (log (/ x mu))))
				  (- x)
				  big-s))
		       (return-from gen-gamma-variate-algo-go x))))))))))

;; This unstructured version (which exactly follows the algorithm
;; description) is significantly faster than the structured version
;; above.  For example, for order = 100d0 and 50000 trials, the above
;; takes 4.4 sec, but this version takes 0.3 sec.  This version also
;; cons about 10 times less.  I don't know why that is.
(defun gen-gamma-variate-algo-go (a &optional (*random-state* *random-state*))
  (declare (type (double-float (2.5327805161251d0)) a)
	   (optimize (speed 3)))
  (let* ((mu (- a 1))
	 (v (sqrt a))
	 (sigma-2 (+ a #.(sqrt (/ 8d0 3d0))))
	 (sigma (sqrt sigma-2))
	 (w (/ sigma-2 mu))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d))
	 (u 0d0)
	 (s 0d0)
	 (big-s 0d0)
	 (x 0d0))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (type double-float s)
	     (type (non-negative-float double-float) big-s x))
    (loop
	(tagbody
	  step-2
	   (setf u (random 1d0))
	   (when (<= u +beta-algo-go+)
	     (go step-8))
	   (setf s (gen-gaussian-variate))
	   (setf x (+ mu (* sigma s)))
	   (when (or (< x 0) (> x b))
	     (go step-2))
	   (setf u (random 1d0))
	   (setf big-s (* 0.5d0 s s))
	   (when (>= s 0)
	     (go step-6))
	   (if (<= u (- 1 (* big-s (- (* w (- 1 (/ (+ s s) v))) 1))))
	       (return-from gen-gamma-variate-algo-go x)
	       (go step-7))
	  step-6
	   (when (<= u (- 1 (* s (- w 1))))
	     (return-from gen-gamma-variate-algo-go x))
	  step-7
	   (if (> (log u) (+ (* mu (+ 1 (log (/ x mu))))
			     (- x)
			     big-s))
	       (go step-2)
	       (return-from gen-gamma-variate-algo-go x))
	  step-8
	   (setf s (gen-exponential-variate 1d0))
	   (setf x (* b (+ 1 (/ s d))))
	   (setf u (random 1d0))
	   (if (> (log u) (- (+ (* mu (+ 2 (log (/ x mu)) (- (/ x b))))
				#.(- (log (/ (* (sqrt (* 2 (dfloat pi)))
                                                +beta-algo-go+)
					     (- 1 +beta-algo-go+)))))
			     (+ b (log (/ (* sigma d) b)))))
	       (go step-2)
	       (return-from gen-gamma-variate-algo-go x))))))

;;; This is probably quite broken.  It's my own attempt at using the
;;; ratio method to generate Gamma variates.  The usual method gets
;;; progressively worse as the parameter gets larger.  This tries to
;;; take advantage of the fact that the desired region actually fits
;;; in a thin rotated rectangle.  Needs more work.
#+(or)
(defun gen-gamma-variate-ratio (a &optional (*random-state* *random-state*))
  (declare (type (double-float 1.5d0) a)
	   (optimize (speed 3)))
  (flet ((g (s)
	   (declare (type (double-float (0d0)) s))
	   (exp (- (* (- a 1) (log s))
		   s)))
	 (vr-limits (s0)
	   (let* ((a+1 (+ a 1))
		  (z (+ a+1 s0))
		  (z/2 (/ z 2))
		  (y (/ (sqrt (+ (expt (- a+1 s0) 2)
				 (* 8 s0)))
			2)))
	     (values (+ z/2 y) (- z/2 y)))))
    (let* ((alpha1 (* (- a 1) (+ (* a a a) (* 3 a a) (* 5 a) -11)))
	   (alpha2 (+ (* a a a) (* 3 a a) (* 12 a) -17))
	   (alpha (expt (+ (/ (sqrt alpha1) #.(* 3 (sqrt 3d0)))
			   (/ alpha2 27))
			#.(dfloat 1/3)))
	   (s0 (+ (/ (+ a 1) 3)
		  alpha
		  (/ (+ (* a a) a a -2)
		     9 alpha)))
	   (ur-hi (sqrt (* (g s0) (+ 1 (* s0 s0))))))
      (multiple-value-bind (s1 s2)
	  (vr-limits s0)
	(format t "s1, s2 = ~A ~A~%" s1 s2)
	(let ((vr-lo (* (sqrt (g s1))
			(+ 1 (* s1 s0))))
	      (vr-hi (* (sqrt (g s2))
			(+ 1 (* s2 s0)))))
	  (do* ((ur (random ur-hi)
		    (random ur-hi))
		(vr (- (random (- vr-hi vr-lo)) vr-lo)
		    (- (random (- vr-hi vr-lo)) vr-lo))
		(u (/ (- ur (* vr s0)) (+ 1 (* s0 s0)))
		   (/ (- ur (* vr s0)) (+ 1 (* s0 s0))))
		(v/u (/ (+ (* ur s0) vr)
			(- ur (* vr s0)))
		     (/ (+ (* ur s0) vr)
			(- ur (* vr s0)))))
	       ((<= (* u u) (g v/u))
		v/u)))))))

;;;
;;; The following Gamma variate routines appear to work (according to
;;; the sample histogram).
;;;
;;;    gen-gamma-variate-algo-a
;;;    gen-gamma-variate-algo-a-2
;;;    gen-gamma-variate-squeeze
;;;
;;; These do not appear to work:
;;;    gen-gamma-variate-algo-go  (some problem for small variates)
;;;    gen-gamma-variate-gn       (doesn't quite match expected values for large values)


(defun gen-gamma-variate (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER > 0
  STATE   = random state to use.

 This is the main routine for generating Gamma variates.
"
  (declare (type (double-float (0d0)) order))
  ;; We divide the set of possible orders into these ranges:
  ;; order > s, 1 < order <= s, order = 1, 0 < order < 1.
  ;; Select the appropriate value of s to minimize runtime.
  (cond ((> order 1d0)
	 ;; Pick the fastest of the three algorithms above.
	 (gen-gamma-variate-squeeze order))
	;; If the threshold s is 1, comment out this code.
	#+(or)
	((> order 1d0)
	 (gen-gamma-variate-direct order))
	((= order 1d0)
	 ;; Gamma variate of order 1 is an exponential variate
	 (gen-exponential-variate 1d0))
	(t
	 ;; order < 1.  This is the algorithm in problem 16 in Sec. 3.4.1
	 (gen-gamma-variate-small-order order))))


;;; Geometric random variable
(defun gen-geometric-variate (p &optional (*random-state* *random-state*))
  (declare (type (non-negative-float double-float (1d0)) p) (optimize (speed 3)))
  (let ((u (random 1d0)))
    (values (ceiling (/ (log u) (log (- 1 p)))))))

;;; Beta random variable

(defun gen-beta-variate (a b &optional (*random-state* *random-state*))
  "Generate a pseudo-random number from a beta distribution function
with parameters a and b:

                   B - 1  A - 1
            (1 - X)      X
     F(X) = -------------------
                BETA(A, B)


 where BETA(A, B) is


          GAMMA(A) GAMMA(B)
          -----------------
            GAMMA(B + A)

 The method uses the fact that

           X1
         -------
         X2 + X1
 is a beta variate if X1 is Gamma of order A, and X2 is Gamma of order
 B.

 A      = first parameter of beta density, A > 0
 B      = second parameter, B > 0
 STATE  = random state to use.
"
  (declare (type (double-float (0d0)) a b))
  (let ((x1 (gen-gamma-variate a))
	(x2 (gen-gamma-variate b)))
    (/ x1 (+ x1 x2))))

;;; Binomial random variate

#+(or)
(eval-when (compile eval)
(declaim (ftype (function ((and (integer 0) fixnum)
			   (non-negative-float double-float 1d0)
			   &optional random-state)
			  (and (integer 0) fixnum))
		gen-binomial-variate))
)

(defun gen-binomial-variate (ntrials p
                             &optional (*random-state* *random-state*))
  "Generate a pseudo-random number from a beta distribution function
with parameters N and p:

                                   N - K  K
      P(K) = BINOMIAL(N, K) (1 - P)      P

 where BINOMIAL(N, K) is

              N!
          -----------
          M! (N - M)!

 NTRIALS      = N, above, the number of trials
 P            = probability of success
 STATE        = random state to use

 The output is an integer.
"
  (declare (type (and (integer 0) fixnum) ntrials)
	   (type (non-negative-float double-float 1d0) p))
  ;; Select some suitable threshold between the direct generation and
  ;; the iterative technique. For a 486-66, the break-even point is
  ;; near 100.  Same is true for a Sparc-20
  (cond ((< ntrials 100)
	 ;; Direct generation
	 (let ((n 0))
	   (declare (fixnum n))
	   (dotimes (k ntrials n)
	     (declare (fixnum k))
	     (if (< (random 1d0) p)
		 (incf n)))))
	(t
	 (let* ((a (1+ (floor ntrials 2)))
		(b (1+ (- ntrials a)))
		(x (gen-beta-variate (dfloat a) (dfloat b))))
	   (declare (fixnum a b)
		    (double-float x))
	   (if (>= x p)
	       (gen-binomial-variate (1- a) (/ p x))
	       (+ a (gen-binomial-variate (1- b) (/ (- p x) (- 1d0 x)))))))))

;;; Poisson random variate

#+(or)
(eval-when (compile)
  (declaim (ftype (function ((double-float 0d0) &optional random-state)
			    (and (integer 0) fixnum))
		  gen-poisson-variate)))

(defun gen-poisson-variate (mean &optional (*random-state* *random-state*))
  "Generate a pseudo-random number from a Poisson distribution
with mean M:

               K   - M
              M  %E
       P(K) = --------
                 K!

 MEAN       = Mean (M) of the distribution, M >= 0
 STATE      = random state to use.

 The output is an integer.
"
  (declare (type (double-float 0d0) mean))
  (let ((threshold 30d0))
    (cond ((< mean threshold)
	   ;; Direct generation
	   (let ((limit (exp (- mean))))
	     (do ((prod (random 1d0))
		  (n 1 (+ n 1)))
		 ((<= prod limit)
		  (- n 1))
	       (declare (fixnum n)
			(type (double-float 0d0) prod))
	       (setf prod (* prod (random 1d0))))))
	  (t
	   ;; Indirect generation
	   (let* ((alpha #.(coerce 7/8 'double-float)) ; Suggested value
		  (order (floor (* alpha mean)))
		  (x (gen-gamma-variate (dfloat order))))
	     (declare (fixnum order))
	     (if (< x mean)
		 (+ order (gen-poisson-variate (- mean x)))
		 (gen-binomial-variate (1- order)
				       (/ mean x))))))))

#||
 (defun time-expo (n)
  (declare (fixnum n))
  (flet (#+cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~A~%" f)
	     (system:without-gcing
	      (time (dotimes (k n)
		      (declare (fixnum k))
		      (funcall func 1d0))))))
	 #-cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~&~A~%" f)
	     (time (dotimes (k n)
		     (declare (fixnum k))
		     (funcall func 1d0))))))
    (declaim (inline timer))
    (dolist (f (list #'gen-exponential-variate-log-method
		     #'gen-exponential-variate-algo-s
		     #'gen-exponential-variate-sa
		     #'gen-exponential-variate-ea
		     #'gen-exponential-variate-ea-2
		     #'gen-exponential-variate-ratio
		     #'gen-exponential-variate-ziggurat))
      (timer f))))

 (defun time-gaussian (n)
  (declare (fixnum n))
  (flet (#+cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~A~%" f)
	     (system:without-gcing
	      (time (dotimes (k n)
		      (declare (fixnum k))
		      (funcall func))))))
	 #-cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~&~A~%" f)
	     (time (dotimes (k n)
		     (declare (fixnum k))
		     (funcall func))))))
    (declare (inline timer))
    (dolist (f (list #'gen-gaussian-variate-polar
		     #'gen-gaussian-variate-algorithm-na
		     #'gen-gaussian-variate-box-trig
		     #'gen-gaussian-variate-ratio
		     #'gen-gaussian-variate-ziggurat))
      (timer f))))

 (defun time-cauchy (n)
  (declare (fixnum n))
  (gc)
  (format t "gen-cauchy-variate-tan~%")
  (system:without-gcing
   (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-cauchy-variate-tan))))

  (gc)
  (format t "gen-cauchy-variate-algorithm-ca~%")
  (system:without-gcing
   (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-cauchy-variate-algorithm-ca))))
  )

 (defun time-gamma (n a)
  (declare (fixnum n))
  (flet ((timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~A~%" f)
	     (system:without-gcing
	      (time (dotimes (k n)
		      (declare (fixnum k))
		      (funcall func a)))))))
    (declare (inline timer))
    (dolist (f (list #'gen-gamma-variate-squeeze
		     #'gen-gamma-variate-gn
		     #'gen-gamma-variate-algo-a
		     #'gen-gamma-variate-algo-a-2
		     #'gen-gamma-variate-algo-go
		     ))
      (timer f))))

;;;
;;; Some simple routines for plotting histograms.  This is meant to be
;;; used as a simple means of testing the generators above.

 (defun make-hist-centers (lo hi intervals)
  (let ((center (make-array intervals))
	(step (/ (- hi lo) intervals)))
    (dotimes (k intervals)
      (setf (aref center k) (+ lo (* k step) (/ step 2))))
    center))

 (defun make-hist (x &key lo hi (intervals 10))
  (let* ((lo-limit (or lo (reduce #'min x)))
	 (hi-limit (or hi (reduce #'max x)))
	 (hist (make-array intervals :initial-element 0))
	 (step (/ (- hi-limit lo-limit) intervals)))
    (dotimes (k (length x))
      (let ((posn (truncate (/ (- (aref x k) lo-limit) step))))
	(cond ((minusp posn)
	       (incf (aref hist 0)))
	      ((>= posn intervals)
	       (incf (aref hist (- intervals 1))))
	      (t
	       (incf (aref hist posn))))))
    (values step hist (make-hist-centers lo-limit hi-limit intervals))))


 (defun plot-hist (x &key (intervals 10) lo hi)
  (multiple-value-bind (step count center)
      (make-hist x :intervals intervals :lo lo :hi hi)
    (format t "step = ~A~%" step)
    (with-open-file (s "/tmp/out" :direction :output)
      (let ((n (reduce #'+ count)))
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (float (/ (aref count k) n step))))
	(format s "~%")
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (exp (- (float (aref center k) 1.0)))))))))


 (defun plot-hist-pdf (x pdf &key (intervals 10) lo hi)
  (multiple-value-bind (step count center)
      (make-hist x :intervals intervals :lo lo :hi hi)
    (format t "step = ~A~%" step)
    (with-open-file (s "/tmp/out" :direction :output)
      (let ((n (reduce #'+ count)))
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (float (/ (aref count k) n step))))
	(format s "~%")
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (float (funcall pdf (aref center k)))))))))

 (defun rng-expo-histogram (n gen)
  (let ((m 2d0)
	(r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen m)))
    (plot-hist-pdf r #'(lambda (x)
			 (declare (double-float x))
			 (/ (exp (- (/ x m))) m))
		   :intervals 50 :lo 0 :hi (* 10 m))))

 (defun rng-gaussian-histogram (n gen)
  (let ((r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen)))
    (plot-hist-pdf r #'(lambda (x)
			 (declare (double-float x))
			 (* (/ (sqrt (* 2 pi))) (exp (* -0.5d0 x x))))
		   :intervals 50 :lo -5 :hi 5)))

 (defun rng-cauchy-histogram (n gen &key (limit 100))
  (let ((r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen)))
    (plot-hist-pdf r #'(lambda (x)
			 (/ (* pi (+ 1 (* x x)))))
		   :intervals 500 :lo (- limit) :hi limit)))

 (defun rng-gamma-histogram (n a gamma gen)
  (let ((r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen a)))
    (plot-hist-pdf r #'(lambda (x)
			 (/ (* (expt x (- a 1)) (exp (- x)))
			    gamma))
		   :intervals 50 :lo 0 :hi (* 5 a))))

||#

(provide :cllib-rng)
;;; file rng.lisp ends here
