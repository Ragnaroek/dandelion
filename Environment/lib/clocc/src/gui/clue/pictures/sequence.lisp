;;;-*- Mode: Lisp; Package:PICTURES; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "PICTURES")


(export '(length-point-seq
	  print-seq
	  vertex-i
	  vertex-x
	  vertex-y
	  length-vertices
	  point-seq-x-min
	  point-seq-y-min
	  point-seq-x-max
	  point-seq-y-max
	  find-point-seq-x
	  find-point-seq-y
	  insert-vertex
	  delete-vertex 
	  )
	'pictures)

(DEFMACRO  length-point-seq (sequence)
  `(VALUES (FLOOR (length ,sequence) 2)))

(DEFUN print-seq (seq &optional (STREAM t)  return)
  "print the value in the sequence to the stream"
  
  (when (> (LENGTH seq) 0)
    (FORMAT stream "~%")
    (DO ((place 0 (1+ place)))
	((= place (LENGTH seq)) (RETURN (VALUES) ))
      (FORMAT stream " ~a" 
	      (elt seq place ))
      (WHEN (AND return (> (MOD place 2) 0))
	(FORMAT stream "~%")))))


(DEFMETHOD vertex-x ((sequence list) position)
    (declare (type (integer 0 *) position))
  
  (IF (< position  (length-point-seq sequence))
      (ELT sequence (* position 2))
      (ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD vertex-x ((sequence array) position)
    (declare (type (integer 0 *) position))
  
  (IF (< position  (length-point-seq sequence))
      (ELT sequence (* position 2))
      (ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD (SETF vertex-x) (x (sequence array) i)
    (IF  (< i  (LENGTH-point-seq sequence))
         (SETF (elt sequence (* i 2)) x )
         (PROGN  
           (VECTOR-PUSH-EXTEND x  sequence 5 )
           (VECTOR-PUSH-EXTEND 0 sequence 5 )
           x)))

(DEFMETHOD (SETF vertex-x) (x (sequence list) i)
    (IF (< i  (LENGTH-point-seq sequence))
        (SETF (elt sequence (* i 2)) x )
        (progn
          (nconc sequence (LIST x  0))
          x)))


(DEFMETHOD vertex-y ((sequence list) position)
  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(DEFMETHOD vertex-y ((sequence array) position)
  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(Defmethod (SETF vertex-y) (y (sequence array) i)
 (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (PROGN  
	      (VECTOR-PUSH-EXTEND 0 sequence 5 )
	      (VECTOR-PUSH-EXTEND y  sequence 5 )
	      y)))

(Defmethod (SETF vertex-y) (y (sequence list) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (progn
	      (nconc sequence (LIST 0  y))
		y)))

(defmethod vertex-i ((sequence list) i)
  (declare (type (integer 0 *) i))
  (VALUES (vertex-x sequence i)(vertex-y sequence i)))
  

(Defmethod (SETF vertex-i) (point-seq (sequence list) i)
  (declare (type (integer 0 *) i))
  (WHEN (>= (LENGTH point-seq) 2)
    (DO*
      ((pos i (1+ pos))
       (x 0 (+ x 2) )
       (y  (+ x 1) (+ x 1)))
      ((>= y (LENGTH point-seq)))
      (IF (< i (length-point-seq sequence))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence pos) (ELT point-seq y)))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence (1- (length-point-seq sequence))) (ELT point-seq y)))))

    point-seq))

(defmethod vertex-i ((sequence array) i)
  (declare (type (integer 0 *) i))
  (VALUES (vertex-x sequence i)(vertex-y sequence i)))
  

(Defmethod (SETF vertex-i) (point-seq (sequence array) i)
  (declare (type (integer 0 *) i))
  (WHEN (>= (LENGTH point-seq) 2)
    (DO*
      ((pos i (1+ pos))
       (x 0 (+ x 2) )
       (y  (+ x 1) (+ x 1)))
      ((>= y (LENGTH point-seq)))
      (IF (< i (length-point-seq sequence))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence pos) (ELT point-seq y)))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence (1- (length-point-seq sequence)))
		  (ELT point-seq y)))))

    point-seq))




(DEFMETHOD vertex-x ((sequence list) position)
  (declare (type (integer 0 *) position))

    (IF (< position  (length-point-seq sequence))
	(ELT sequence (* position 2))
	(ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD vertex-x ((sequence array) position)
  (declare (type (integer 0 *) position))

    (IF (< position  (length-point-seq sequence))
	(ELT sequence (* position 2))
	(ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD (SETF vertex-x) (x (sequence array) i)
  (declare (type (integer 0 *) i))
      (IF  (< i  (LENGTH-point-seq sequence))
	   (SETF (elt sequence (* i 2)) x )
	   (PROGN  
	     (VECTOR-PUSH-EXTEND x  sequence 5 )
	     (VECTOR-PUSH-EXTEND 0 sequence 5 )
	     x)))

(DEFMETHOD (SETF vertex-x) (x (sequence list) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (* i 2)) x )
	    (progn
	      (nconc sequence (LIST x  0))
	      x)))

(DEFMETHOD vertex-y ((sequence list) position)

  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(DEFMETHOD vertex-y ((sequence array) position)

  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(Defmethod (SETF vertex-y) (y (sequence array) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (PROGN  
	      (VECTOR-PUSH-EXTEND 0 sequence 5 )
	      (VECTOR-PUSH-EXTEND y  sequence 5 )
	      y)))

(Defmethod (SETF vertex-y) (y (sequence list) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (progn
	      (nconc sequence (LIST 0  y))
		y)))



(DEFMETHOD length-vertices ((sequence list))
  (length-point-seq sequence))

(DEFMETHOD length-vertices ((sequence array))
  (length-point-seq sequence))

(DEFUN point-seq-x-min (point-seq )
  "find the minimum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 0 (+ 2 index))
	  (value (ELT point-seq 0) (MIN (ELT point-seq index) value)))
 	 ((>= index (- length 2)) (RETURN value))
      )
    ))

(DEFUN point-seq-y-min (point-seq )
  "find the minimum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 1 (+ 2 index))
	  (value (ELT point-seq 1) (MIN (ELT point-seq index) value)))  
	 ((>= index (- length 1)) value))))

(DEFUN point-seq-x-max (point-seq )
  "find the maximum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 0 (+ 2 index))
	  (value (ELT point-seq 0) (MAX (ELT point-seq index) value)))  
	 ((>= index (- length  2)) value))))

(DEFUN point-seq-y-max (point-seq )
  "find the maximum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 1 (+ 2 index))
	  (value (ELT point-seq 1) (MAX (ELT point-seq index) value)))  
	 ((>= index (- length 1)) value))))


(DEFUN find-point-seq-x (point-seq x &key (start 0)
				   (end (1- (length-point-seq point-seq)) ))
  "Find the first occurance of X in the POINT-SEQ and return the POSITION
   START is the point-seq where the search starts,
   END is where point-seq where the search ends."

  (LET ((start-seq (IF (>= start (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 2)
		    (* start 2))) 
	(end-seq (IF (>= end (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 2)
		    (* end 2))))
    (DO* ((index start-seq  (+ 2 index) )
	  (POSITION (IF (= x (ELT point-seq index))(FLOOR (/ index 2)) nil)
		    (IF (= x (ELT point-seq index))(FLOOR (/ index 2)) position)))  
	 ((OR position (>= index end-seq )) position))))



(DEFUN find-point-seq-y (point-seq y &key (start 0)
				   (end (1- (/ (length point-seq) 2))))
  "Find the first occurance of Y in the POINT-SEQ and return the POSITION"

  (LET ((start-seq (IF (>= start (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 2)
		    (* start 2)))
	(end-seq (IF (>= end (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 1)
		    (+ (* end 2) 1))))
    (DO* ((index  (1+ start-seq) (+ 2 index) )
	  (POSITION (IF (= y (ELT point-seq index))(FLOOR (/ index 2)) nil)
		    (IF (= y (ELT point-seq index))(FLOOR (/ index 2)) position)))  
	 ((OR position (>= index end-seq )) position))))


(DEFMETHOD insert-vertex ((array array) new-x new-y  i)
  (DECLARE (type wcoord new-x))
  (DECLARE (type integer i))
    (IF (< i (length-point-seq array))
	;;subtract one because vectors have already been extended
	(PROGN 
	  ;push a point on the vector to make sure it is long enough for x
	  (VECTOR-PUSH-EXTEND 0 array 5)
	  ;push a point on the vector to make sure it is long enough for y
	  (VECTOR-PUSH-EXTEND 0 array 5)
	  ;count down to the position of insertion
	  (DO ((index (1- (LENGTH array)) (1- index)))
	      ((= (ROUND  index 2) i)
	       (SETF (AREF array (* i 2)) new-x)
	       (SETF (AREF array (+ (* i 2) 1)) new-y))
	    ;store value in vector(x) into vector(x+1)	  
	    (SETF (AREF array index) (AREF array (- index 2)))))
	(PROGN 
	  (VECTOR-PUSH-EXTEND new-x array 5)
	  (VECTOR-PUSH-EXTEND new-y array 5) ))
  (values new-x new-y))

(DEFMETHOD insert-vertex ((list list ) new-x new-y  i)
  (DECLARE (type wcoord new-x new-y))
  (DECLARE (type integer i))
  ;;subtract one because vectors have already been extended
  (IF (< i (length-point-seq list))
      (PROGN
	(nconc list (LIST 0 0))
	;count down to the position of insertion
	(DO ((index (1- (LENGTH list)) (1- index)))
	    ((= (ROUND  index 2) i)
	     (SETF (ELT list (* i 2)) new-x)
	     (SETF (ELT  list (+ (* i 2) 1)) new-y))
	  ;store value in vector(x) into vector(x+1)	  
	  (SETF (ELT list index) (ELT list (- index 2)))))
      (nconc list (LIST new-x new-y)))
  (values new-x new-y))


(DEFMETHOD delete-vertex ( (array array) i)
  (DECLARE (type integer i))
  (LET ((point-seq-length (length-point-seq array)))
    (WHEN (AND (< i  point-seq-length ) (NOT (= point-seq-length  0)))  
      (MULTIPLE-VALUE-BIND (x y)  (vertex-i array i)
	(DO ((index  (* 2 i)  (1+ index)))
	    ((= index (- (LENGTH array) 2))
	     ;move the fill pointer back one to shorten array	      
	     (SETF (FILL-POINTER array) index))
	  (SETF (ELT  array index) (elt array (+ 2 index))))
	(VALUES x y)))))

(DEFMETHOD delete-vertex ( (list list) i)
  (DECLARE (type integer i))
  (LET ((point-seq-length (length-point-seq list)))
    (WHEN (AND (< i  point-seq-length ) (NOT (= point-seq-length  0)))
      (MULTIPLE-VALUE-BIND (x y)  (vertex-i list i)
	    (DO ((index  (* 2 i)  (1+ index)))
		((= index (- (LENGTH list) 2))
		 (NBUTLAST list) (NBUTLAST list))	      
	      (SETF (ELT  list index) (elt list (+ 2 index))))
      (VALUES x y)))))

(DEFUN min-value-vector (cvector &optional (initial-index 0))
  "find the minimum value in a vector"
  (DO ((index (+ 2 initial-index) (+ 2 index))
       (value (ELT  cvector initial-index) (MIN (ELT cvector index) value)))  
      ((>= index (LENGTH cvector)) value)
    )
  )

(DEFUN max-value-vector (cvector &optional (initial-index 0))
  "find the minimum value in a vector"
  (DO ((index (+ 2 initial-index) (+ 2 index))
       (value (ELT  cvector initial-index) (MAX (ELT cvector index) value)))  
      ((>= index (LENGTH cvector)) value)
    )
  )
