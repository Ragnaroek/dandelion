;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;; $Id: chunktest.lisp,v 2.1 2005/12/26 00:25:16 airfoyle Exp $

(defclass Num-reg (Chunk)
   ((cont :accessor Num-reg-contents
	  :initarg :contents
	  :type number)
    (fcn :reader Num-reg-fcn
	 :initarg :fcn)))

(defmethod print-innards ((n Num-reg) srm)
   (format srm "(= ~a)"
	   (cond ((slot-boundp n 'cont)
		  (Num-reg-contents n))
		 (t "??"))))

;; For ease of reading, we don't use universal time, but the number
;; of numerical ops, as our clock.

(defvar num-num-ops* 0)

;;; A Num-reg is a leaf iff it has no function.

(defmethod derive-date ((r Num-reg))
   (cond ((chunk-is-leaf r)
	  (cond ((slot-boundp r 'fcn)
		 ;; It might be changed by its deriver; there'no
		 ;; way to tell without running it.
		 +no-info-date+)
		((slot-boundp r 'cont)
		 ;; It has no deriver, so it must have been set
		 ;;  --- and dated --- by someone outside the
		 ;; chunk-update protocol
		 false)
		(t
		 ;; If no function and no contents, this Num-reg
		 ;; is of no use to anyone
		 (error "Num-reg with no contents or deriver fcn ~s"
			r))))
	 ((slot-boundp r 'cont)
	  ;; Whenever it was last computed, no one else
	  ;; has changed it since.
	  false)
	 (t
	  ;; It has no value, so it can't be up to date
	  +no-info-date+)))

(defmethod derive ((r Num-reg))
   (cond ((slot-boundp r 'fcn)
	  (let ((new-val 
		   (funcall (Num-reg-fcn r)
			 (mapcar #'Num-reg-contents (Chunk-basis r))
			 (mapcar #'Num-reg-contents (Chunk-update-basis r)))))
	     (setq num-num-ops* (+ num-num-ops* 1))
	     (cond ((or (not (slot-boundp r 'cont))
			(not (= new-val (Num-reg-contents r))))
		    (setf (Num-reg-contents r)
			  new-val)
		    num-num-ops*)
		   (t false))))
	 ((slot-boundp r 'cont)
	  ;; As mentioned above, it's being set outside
	  ;; the usual channels.  Assume it's up to date.
	  false)
	 (t
	  (error "Num-reg with no contents or deriver fcn ~s"
		 r))))

;;; We make this a subclass of Num-reg because it will have derivees
;;; that are Num-regs.
;;; Its 'fcn' slot is ignored.
(defclass Num-pair-reg (Num-reg)
   ((cont1 :accessor Num-pair-reg-one
	   :initarg :one
	   :type number)
    (cont2 :accessor Num-pair-reg-two
	   :initarg :two
	   :type number)))

;;; Must override Num-reg version!
(defmethod derive-date ((npr Num-pair-reg))
   (cond ((and (slot-boundp npr 'cont1)
	       (slot-boundp npr 'cont2))
	  false)
	 (t +no-info-date+)))

(defmethod derive ((npr Num-pair-reg))
   (let ((basis (Chunk-basis npr)))
      (cond ((and (= (length basis)
		     2)
		  (every (\\ (b) (typep b 'Num-reg))
			 basis))
	     (setf (Num-pair-reg-one npr)
		   (Num-reg-contents (head basis)))
	     (setf (Num-pair-reg-two npr)
	           (Num-reg-contents (head (tail basis))))
	     ;; We do this to avoid having the slot be empty --
	     (setf (Num-reg-contents npr) 0)
	     (incf num-num-ops*))
	    (t
	     (error "Buggy basis: ~s" npr)))))

(defun non-mon-cycle-detect ()
   (let* ((val -1)
	  (prin-chunk (make-instance 'Num-reg
			 :name 'prin
			 :basis !()
			 :contents 1
			 :fcn (\\ (_ _) (setq val 1))))
	  (def-chunk (make-instance 'Num-reg
			:name 'def
			:basis (list prin-chunk)
			:contents 0
			:fcn (\\ (_ _) (setq val 0))))
	  (or-chunk (make-instance 'Or-chunk
		       :disjuncts (list prin-chunk def-chunk)
		       :default def-chunk)))
      ;; This should cause an error; if it doesn't, an infinite
      ;; loop should ensue --
      (chunk-request-mgt or-chunk)
      ;; This point should never be reached --
      val))

;;; Disjuncts are a Num-pair-reg and a Num-reg that
;;; is equal to the second element of the pair
(defclass Denom-Or-chunk (Or-chunk Num-reg)
   ())
;;; The 'fcn' slot is irrelevant.

(defmethod derive-date ((npr Denom-Or-chunk))
   (cond ((slot-boundp npr 'cont)
	  false)
	 (t
	  +no-info-date+)))

(defmethod derive ((rc Denom-Or-chunk))
   (let ((disjuncts (Or-chunk-disjuncts rc)))
      (cond ((Chunk-managed (first disjuncts))
	     (setf (Num-reg-contents rc)
		   (Num-pair-reg-two (first disjuncts)))
	     (incf num-num-ops*))
	    ((Chunk-managed (second disjuncts))
	     (setf (Num-reg-contents rc)
		   (Num-reg-contents (second disjuncts)))
	     (incf num-num-ops*))
	    (t
	     (error "Denom-Or-chunk ~s has unmanaged disjuncts"
		    rc)))))

;; Numbers (these should all be nonnegative to avoid possibility
;; of dividing by zero below)--    
(defvar ivec*)

;; Chunks that just read those numbers
(defvar input-chunks*)

;; Add 1 to each input but the first.
;; (aref inter-chunks* i) contains the chunk for
;; (+ (ivec (+ i 1)) 1)
(defvar inter-chunks*)

(defvar numer*)

(defvar rough-denom*)

(defvar denom*)

(defvar quo*)

(defvar denom-or-chunk*)

;;;;(defvar numer-denom-reg* (list 0 0))

(defvar numer-denom-pair*)

(defun build-test-net (k)
   (setq ivec* (make-array k))
   (do ((i 0 (+ i 1)))
       ((= i k))
      (setf (aref ivec* i) i))
   (setq input-chunks* (make-array k))
   (do ((i 0 (+ i 1)))
       ((= i k))
      (setf (aref input-chunks* i)
	    (make-instance 'Num-reg
	       :name `(input ,i)
	       :basis !()
	       :update-basis !()
	       :fcn (let ((i i))
		      ;; Avoid classic bug (i clobbered by 'do')
		      (\\ (_ _)
;;;;			 (format t "Calculating (aref ivec* ~s)~%"
;;;;				 i)
			 (aref ivec* i))))))
   (setq inter-chunks* (make-array (- k 1)))
   (do ((i 0 (+ i 1)))
       ((= i (- k 1)))
      (setf (aref inter-chunks* i)
	    (make-instance 'Num-reg
	       :name `(inter ,(+ i 1))
	       :basis (list (aref input-chunks* (+ i 1)))
	       :update-basis !()
	       :fcn (\\ (b _) (+ (first b) 1)))))
   (setq numer*
	 (make-instance 'Num-reg
	    :name '"Numer"
	    :basis (concatenate 'list input-chunks*)
	    :update-basis (concatenate 'list inter-chunks*)
	    :fcn (\\ (b u)
		    (do ((bl b (cdr bl))
			 (ul u (cdr ul)) ; one shorter than bl
			 (accum 0)
			 (sign 1 (- sign)))
		        ((null ul)
			 (+ accum (first bl)))
		       (setq accum
			     (+ accum
				(* sign (first bl) (first ul))))))))
   (setq rough-denom*
	 (labels ((build-denom (j)
		     (cond ((= j (- k 2))
			    (aref inter-chunks* j))
			   (t
			    (let ((den (build-denom (+ j 1))))
			       (make-instance 'Num-reg
				  :name (format nil "(i[~a]+1)/~a"
						    (+ j 1) (Chunk-name den))
				  :basis (concatenate 'list (subseq inter-chunks* j))
				  :update-basis (list (aref inter-chunks* j)
						      den)
				  :fcn (\\ (_ ub)
					  (/ (first ub) (second ub)))))))))
	    (build-denom 0)))
   (setq numer-denom-pair*
         (make-instance 'Num-pair-reg
	    :name "<numer,denom>"
	    :basis (list numer* rough-denom*)
	    :update-basis !()))
   ;; This has the same value as rough-denom*, but a shorter name --
   (setq denom*
         (make-instance 'Num-reg
	    :name "denom"
	    :basis (list rough-denom*)
	    :fcn (\\ (d _) (first d))))
   (setq denom-or-chunk*
         (make-instance 'Denom-Or-chunk
	    :name 'denom
	    :basis (list numer* rough-denom*)
	    :disjuncts (list numer-denom-pair* denom*)
	    :default denom*))
;;;;   (cond ((not (null (Chunk-update-basis denom-or-chunk*)))
;;;;	  (break "denom-or-chunk* has suspicious update-basis: ~s"
;;;;		 (Chunk-update-basis denom-or-chunk*))))
   (setq quo*
	 (make-instance 'Num-reg
	     :name "numer/denom"
	     :basis (list numer-denom-pair*)
	     :update-basis !()
	     :fcn (\\ (_ _) (/ (Num-pair-reg-one numer-denom-pair*)
			       (Num-pair-reg-two numer-denom-pair*))))))

;;; Independent computer of value of quo* or denom-or-chunk*
(defun compute ()
   (let ((k (length ivec*)))
      (let ((n (do ((i 0 (+ i 1))
		     (accum 0)
		     (sign 1 (- sign)))
		    ((= i (- k 1))
		     (+ accum (aref ivec* i)))
		  (setq accum
			(+ accum (* sign
				    (aref ivec* i)
				    (+ (aref ivec* (+ i 1))
				       1))))
;;;;		  (format t "i = ~s   accum = ~s~%" i accum)
	       ))
	     (d (do ((i (- k 2) (- i 1))
		     (quo (+ (aref ivec* (- k 1))
			     1)))
		    ((= i 0)
		     quo)
		  (setq quo (/ (+ (aref ivec* i) 1)
			       quo))
;;;;		  (format t "i = ~s   quo = ~s~%" i quo)
		)))
	  (format t "n = ~s   d = ~s~%" n d)
	  (cond ((Chunk-managed quo*)
		 (/ n d))
		(t
		 d)))))
	   
;;; Date at start of current iteration of net-direct-compare--
(defvar current-test-date*)

;;; Build net before starting this --
(defun net-direct-compare (which-input start-val &optional (num-iters 1000))
   (chunk-request-mgt denom-or-chunk*)
   (do ((i start-val (+ i 1))
	(j which-input (+ j 1))
	(k (length ivec*)))
       ((>= (- i start-val) num-iters)
	true)
      (cond ((>= j k)
	     (setq j 0)))
      (setf (aref ivec* j) i)
      (incf num-num-ops*)
      (setq current-test-date* num-num-ops*)
      (setf (Chunk-date (aref input-chunks* j))
	    +no-info-date+)
;;;;      (chunk-up-to-date (aref input-chunks* j))
      (format t "chunk ~s <- ~s ~%" j i)
      (cond ((Chunk-managed quo*)
	     (chunk-terminate-mgt quo* false)
	     (chunk-update denom-or-chunk* false false))
	    (t
	     (chunk-request-mgt quo*)
	     (chunks-update (list quo* denom-or-chunk*)
			    false false)))
      (let ((direct (compute))
	    (via-chunks (Num-reg-contents
			   (cond ((Chunk-managed quo*)
				  quo*)
				 (t
				  denom-or-chunk*)))))
	 (format t "~s: ~s <?> ~s~%"
		   i direct via-chunks)
	 (cond ((not (= direct via-chunks))
		(format t "Oops! i = ~s j = ~s ~%" i j)
		(return false))
	       (t
		(let ((denom-mg (Chunk-managed denom*))
		      (quo-mg (Chunk-managed quo*)))
		   (cond ((cond (denom-mg quo-mg)
				(t (not quo-mg)))
			  (format t "Bummer: denom-mg = ~s quo-mg = ~s~%"
				  denom-mg quo-mg)
			  (return false)))))))))

(defun chunk-test ()
   (build-test-net 10)
   (net-direct-compare 2 4 100))

;;; Everything else is not currently needed --
#|
(defvar datum-a*)
(defvar datum-b*)
(defvar datum-c*)

(defparameter
    chunk-a* 
    (make-instance 'Num-reg
	  :name 'a
	  :basis '()))

(defparameter
    chunk-b*
    (make-instance 'Num-reg
	  :name 'b
	  :basis '()))

(defparameter
    chunk-c*
    (make-instance 'Num-reg
	  :name 'c
	  :basis '()))

(defclass K-A-aux (Num-reg)
   ((delta :accessor K-A-aux-s
	   :initarg :delta
	   :type number)))
  
;;; Create a new Num-reg whose value is (v+1)^2, where v is the 
;;; contents of reg.
(defun setup-fcn1 (reg delta)
   (let* ((aux-chunk (make-instance 'K-A-aux
		        :name (build-symbol aux- (:++ symno*))
			:delta delta
			:fcn (\\ (bn _) (+ (first bn) delta ))
			:basis (list reg)))
	  (res-chunk (make-instance 'Num-reg
		       :name (build-symbol res- (:++ symno*))
		       :basis (list reg)
		       :update-basis (list aux-chunk)
		       :fcn (\\ (_ xn) (* (first xn)
					  (first xn))))))
      res-chunk))

(defparameter nr1*
    (setup-fcn1 chunk-a* 1))


|#