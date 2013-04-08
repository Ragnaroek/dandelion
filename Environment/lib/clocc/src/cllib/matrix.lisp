;;; Matrix Routines
;;;
;;; The main advantage of this file is that is can work with matrices
;;; with elements of arbitrary types (bignums, ratios).
;;; The main disadvantages are performance and limited functionality.
;;;
;;; If you need fast floating point matrix operations,
;;; you should use MatLisp (http://matlisp.sourceforge.net/),
;;; which relies on BLAS (http://www.netlib.org/blas) and
;;; LAPACK (http://www.netlib.org/lapack) for heavy-duty computations.
;;;
;;; Copyright (C) 2000-2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: matrix.lisp,v 2.26 2006/06/22 20:11:01 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/matrix.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-type', `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `do-iter-ls'
  (require :cllib-iter (translate-logical-pathname "cllib:iter"))
  ;; `divf', `norm-functions'
  (require :cllib-math (translate-logical-pathname "cllib:math")))

(in-package :cllib)

(export '(matrix-print matrix-to-file matrix-from-file random-matrix
          matrix-multiply array-copy array-lin-comb dimension
          matrix-id matrix-id-p matrix-transpose matrix-symmetric-p bilinear
          array-slice array-marginal array-dist list-to-array
          matrix-solve-lower matrix-solve-upper matrix-solve-lu
          matrix-solve-lup matrix-lup
          matrix-solve matrix-inverse))
(import '(matrix-print) :cl-user) ; format ~//

;;;
;;; i/o
;;;

(defun matrix-print (out aa colp atp &optional fmt-arg)
  "Print a matrix.  Suitable for `format' ~//.
By default prints the contents.
@ means print the rank and dimensions too.
: means print just the rank and the dimensions."
  (declare (stream out) (type array aa))
  (let ((rank (array-rank aa))
        (fmt (case fmt-arg
               ((#\,) (formatter " ~:d"))      ; 123,456,789
               ((#\$) (formatter " ~5,1,10$")) ; Matlab-style
               ((nil) (formatter " ~S"))
               (t fmt-arg))))
    (declare (type index-t rank))
    (fresh-line out)
    (when atp
      (format out ";; ~[Scalar~;Vector~;Matrix~:;Rank ~:*~:D array~]; dimension~:p: ~{~d~^x~}~%"
              rank (array-dimensions aa)))
    (unless colp
      (case rank
        (1 (loop :for elt :across aa :do (format out fmt elt) (terpri out)))
        (2 (dotimes (ii (array-dimension aa 0))
             (dotimes (jj (array-dimension aa 1))
               (format out fmt (aref aa ii jj)))
             (terpri out)))
        (t (error 'case-error :proc 'matrix-print :args
                  (list 'matrix aa 1 2)))))
    aa))

(defun matrix-to-file (file matrix &key (log *standard-output*))
  "write matrix into the file, suitable for matlab/octave input"
  (with-timing (:out log)
    (when log
      (format log "~&;; ~S: ~{~D~^x~}..." 'matrix-to-file
              (array-dimensions matrix))
      (force-output log))
    (with-open-file (out file :direction :output :if-exists :rename)
      (with-standard-io-syntax
        (loop :for ii :from 0 :below (array-dimension matrix 0) :do
          (loop :for jj :from 0 :below (array-dimension matrix 1) :do
            (unless (zerop jj) (write-char #\Space out))
            (write (aref matrix ii jj) :stream out))
          (terpri out)))
      (when log
        (format log "~:D byte~:P" (file-length out))))))

(defun matrix-from-file (file &key (log *standard-output*))
  "read matrix from a file written by matlab/octave"
  (with-timing (:out log)
    (with-open-file (in file :direction :input)
      (when log
        (format log "~&;; ~S(~A, ~:D byte~:P)..."
                'matrix-from-file file (file-length in))
        (force-output log))
      (let* ((list
              (loop :for line = (read-line in nil nil) :while line
                :collect (port:string-tokens line)))
             (mx (make-array (list (length list) (length (first list)))
                             :initial-contents list)))
        (when log (format log "~{~D~^x~}" (array-dimensions mx)))
        mx))))

;;;
;;; unity &c
;;;

(defun random-matrix (rows cols max)
  "return the random matrix of the given dimensions and element range"
  (let ((mx (make-array (list rows cols))))
    (dotimes (i rows mx)
      (dotimes (j cols)
        (setf (aref mx i j) (random max))))))

(define-condition dimension (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "dimension mismatch:~@{ ~{~d~^x~}~}")))

(defun matrix-id (nn)
  "Generate an NxN identity matrix"
  (declare (type index-t nn))
  (loop :with matrix = (make-array (list nn nn) :initial-element 0)
    :for ii :from 0 :below nn :do (setf (aref matrix ii ii) 1)
    :finally (return matrix)))

(defun array-copy (aa &optional bb)
  "Copy an arbitrary array."
  (declare (type array aa))
  (case (array-rank aa)
    (1 (if bb (replace bb aa) (copy-seq aa)))
    (t (let ((res (or bb (make-array (array-dimensions aa)
                                     :element-type (array-element-type aa)))))
         (dotimes (ii (array-total-size aa) res)
           (setf (row-major-aref res ii) (row-major-aref aa ii)))))))

(defun matrix-symmetric-p (mx)
  "Check for the matrix being symmetric."
  (declare (type array mx))
  (let ((dim (array-dimension mx 0)))
    (and (= dim (array-dimension mx 1))
         (loop :for ii :of-type index-t :from 1 :below dim
           :unless (loop :for jj :of-type index-t :from 0 :below ii
                     :unless (= (aref mx ii jj) (aref mx jj ii))
                     :return nil :finally (return t))
           :return nil :finally (return t)))))

(defun matrix-id-p (mx)
  "Check the matrix for being identity."
  (declare (type array mx))
  (let ((dim (array-dimension mx 0)))
    (and (= dim (array-dimension mx 1))
         (loop :for ii :of-type index-t :from 1 :below dim
           :unless (and (loop :for jj :of-type index-t :from 0 :below ii
                          :unless (and (zerop (aref mx ii jj))
                                       (zerop (aref mx jj ii)))
                          :return nil :finally (return t))
                        (= 1 (aref mx ii ii)))
           :return nil :finally (return t)))))

(defun matrix-transpose (mx)
  "Transpose the matrix."
  (declare (type array mx))
  (let ((mxt (make-array (reverse (array-dimensions mx)))))
    (dotimes (ii (array-dimension mx 0) mxt)
      (dotimes (jj (array-dimension mx 1))
        (setf (aref mxt jj ii) (aref mx ii jj))))))

(defun array-slice (arr index-list)
  "Extract the sub-array with the supplied indexes.
Similar to Matlab `:'."
  (let* ((dims (delete nil (mapcar (lambda (dim index) (if index nil dim))
                                   (array-dimensions arr) index-list)))
         (idx (copy-seq index-list)) ; running index into ARR
         (ret (make-array dims)))
    (do-iter-ls (ii (nreverse dims) ret)
      (let ((tail ii))          ; update running index
        (mapl (lambda (idx-r index-r)
                (unless (car index-r)
                  (setf (car idx-r) (pop tail))))
              idx index-list)
        (when tail (error "~S: ~S should be ~S" 'array-slice tail nil)))
      (setf (apply #'aref ret ii) (apply #'aref arr idx)))))

(defun array-marginal (arr index-list)
  "Return the marginal array keeping the INDEX-LIST."
  (let* ((dims (mapcar (lambda (i) (array-dimension arr i)) index-list))
         (idx-ret (make-list (length index-list))) ; running index into ret
         (idx-arr (make-list (array-rank arr)))    ; running index into arr
         (ret (make-array dims :initial-element 0)))
    (do-iter (ii (mk-arr 'fixnum (array-dimensions arr)) ret)
      (replace idx-arr ii)
      ;; this AREF in LAMBDA means that we must use DO-ITER and not DO-ITER-LS
      (map-into idx-ret (lambda (idx) (aref ii idx)) index-list)
      (incf (apply #'aref ret idx-ret) (apply #'aref arr idx-arr)))))

(defun array-dist (arr1 arr2 &key (key #'value) (order 1))
  "Compute the distance between arguments and their norms, pointwise."
  (unless (equal (array-dimensions arr1) (array-dimensions arr2))
    (error 'dimension :proc 'array-dist :args
           (list (array-dimensions arr1) (array-dimensions arr2))))
  (let ((dist 0) (norm1 0) (norm2 0))
    (multiple-value-bind (pre combine post) (norm-functions order)
      (dotimes (ii (array-total-size arr1))
        (let ((v1 (funcall key (row-major-aref arr1 ii)))
              (v2 (funcall key (row-major-aref arr2 ii))))
          (setq dist (funcall combine dist (funcall pre (- v1 v2)))
                norm1 (funcall combine norm1 (funcall pre v1))
                norm2 (funcall combine norm2 (funcall pre v2)))))
      (values (funcall post dist) (funcall post norm1) (funcall post norm2)))))

(defun list-to-array (list dims)
  "Convert the list to an array of given dimensions, assuming row-major order."
  (let* ((arr (make-array dims)) (sz (array-total-size arr)))
    (unless (= (length list) sz)
      (error "~S: list/dimension mismatch: ~:D /= ~:D ~S"
             'list-to-array (length list) sz dims))
    (loop :for el :in list :for i :upfrom 0
      :do (setf (row-major-aref arr i) el))
    arr))

;;;
;;; linear combinations
;;;

(defun array-check-return (arr dims)
  "Make the value to be returned."
  (if arr
      (if (equal (array-dimensions arr) dims)
          arr
          (error 'dimension :proc 'array-check-return :args
                 (list (array-dimensions arr) dims)))
      (make-array dims :initial-element 0)))

(defun array-lin-comb (l0 a0 l1 a1 &optional res)
  "Linear combination of arbitrary arrays."
  (declare (type array a0 a1) (number l0 l1))
  (let ((dims (array-dimensions a0)))
    (unless (equal dims (array-dimensions a1))
      (error 'dimension :proc 'array-check-return
             :args (list dims (array-dimensions a1))))
    (let ((ret (array-check-return res dims)))
      (dotimes (ii (array-total-size ret) ret)
        (setf (row-major-aref ret ii)
              (+ (* l0 (row-major-aref a0 ii))
                 (* l1 (row-major-aref a1 ii))))))))

;;;
;;; Bilinear form
;;;

(defun bilinear (mx v0 v1)
  "Compute the bilinear form (Ax,y)."
  (declare (type (array * (* *)) mx) (type (array * (*)) v0 v1))
  (unless (and (= (array-dimension v0 0) (array-dimension mx 0))
               (= (array-dimension v0 0) (array-dimension mx 0)))
    (error 'dimension :proc 'bilinear :args
           (list (array-dimensions mx) (array-dimensions v0)
                 (array-dimensions v1))))
  (loop :for ii :of-type index-t :from 0 :below (array-dimension mx 0)
    :sum (* (aref v0 ii)
            (loop :for jj :of-type index-t :from 0 :below (array-dimension mx 1)
              :sum (* (aref mx ii jj) (aref v1 jj))))))

;;;
;;; Matrix Multiplication
;;;

(defun matrix-multiply (aa bb &optional re)
  "Multiply two matrixes.
The optional third argument is the place where to put the return value."
  (flet ((num-arr (num input dim)   ; scalar X any array
           (loop :with res :of-type array = (array-check-return re dim)
             :for ii :of-type index-t :from 0 :below (array-total-size res)
             :do (setf (row-major-aref res ii)
                       (* num (row-major-aref input ii)))
             :finally (return res))))
    (cond ((numberp aa) (num-arr aa bb (array-dimensions bb)))
          ((numberp bb) (num-arr bb aa (array-dimensions aa)))
          ((and (= 2 (array-rank aa)) ; matrix X matrix
                (= 2 (array-rank bb)))
           (if (= (array-dimension aa 1)
                  (array-dimension bb 0))
               (loop :with res :of-type array =
                 (array-check-return re (list (array-dimension aa 0)
                                              (array-dimension bb 1)))
                 :for ii :from 0 :below (array-dimension aa 0) :do
                 (loop :for jj :from 0 :below (array-dimension bb 1) :do
                   (setf (aref res ii jj) 0)
                   (loop :for kk :from 0 :below (array-dimension aa 1) :do
                     (incf (aref res ii jj)
                           (* (aref aa ii kk) (aref bb kk jj)))))
                 :finally (return res))
               (error 'dimension :proc 'matrix-multiply :args
                      (list (array-dimensions aa) (array-dimensions bb)))))
          ((and (= 1 (array-rank aa)) ; row X matrix
                (= 2 (array-rank bb)))
           (if (= (array-dimension aa 0)
                  (array-dimension bb 0))
               (loop :with res :of-type array =
                 (array-check-return re (list (array-dimension bb 1)))
                 :for ii :from 0 :below (array-dimension bb 1) :do
                 (setf (aref res ii) 0)
                 (loop :for jj :from 0 :below (array-dimension aa 0) :do
                   (incf (aref res ii)
                         (* (aref aa jj) (aref bb jj ii))))
                 :finally (return res))
               (error 'dimension :proc 'matrix-multiply :args
                      (list (array-dimensions aa) (array-dimensions bb)))))
          ((and (= 2 (array-rank aa)) ; matrix X column
                (= 1 (array-rank bb)))
           (if (= (array-dimension aa 1)
                  (array-dimension bb 0))
               (loop :with res :of-type array =
                 (array-check-return re (list (array-dimension aa 0)))
                 :for ii :from 0 :below (array-dimension aa 0) :do
                 (setf (aref res ii) 0)
                 (loop :for jj :from 0 :below (array-dimension bb 0) :do
                   (incf (aref res ii) (* (aref aa ii jj) (aref bb jj))))
                 :finally (return res))
               (error 'dimension :proc 'matrix-multiply :args
                      (list (array-dimensions aa) (array-dimensions bb)))))
          ((and (= 1 (array-rank aa)) ; column X row (row X col is dot)
                (= 1 (array-rank bb)))
           (loop :with res :of-type array =
                 (array-check-return
                  re (list (array-dimension aa 0) (array-dimension bb 0)))
                 :for ii :from 0 :below (array-dimension aa 0) :do
                 (loop :for jj :from 0 :below (array-dimension bb 0) :do
                   (setf (aref res ii jj) (* (aref aa ii) (aref bb jj))))
                 :finally (return res)))
          (t (error 'code :proc 'matrix-multiply :args (list aa bb) :mesg
                    "cannot multiply matrix <~:/matrix-print/> ~
                     by matrix <~:/matrix-print/>")))))

;;;
;;; solving linear systems
;;;

(defun mx-solve-check (aa bb)
  (unless (and (= 2 (array-rank aa)) (= 1 (array-rank bb))
               (= (array-dimension aa 0) (array-dimension aa 1)
                  (array-dimension bb 0)))
    (error 'dimension :proc 'matrix-solve :args
           (list (array-dimensions aa) (array-dimensions bb)))))

(defun permuted-aref (vec ii permutation)
  (aref vec (if permutation (aref permutation ii) ii)))
(defun (setf permuted-aref) (value vec ii permutation)
  (setf (aref vec (if permutation (aref permutation ii) ii)) value))

(macrolet ((aref-bb (ii)
             `(permuted-aref bb ,ii permutation)))
(defun matrix-solve-lower (aa bb diag1 &optional permutation)
  "Solve Ax=b, put the result in b.
A is assumed to be a lower-triangular matrix."
  (declare (type (array * (* *)) aa) (type (array * (*)) bb))
  (mx-solve-check aa bb)
  (loop :with nn :of-type index-t = (1- (array-dimension aa 0))
    :for ii :of-type index-t :from 0 :to nn :do
    (unless diag1 (divf (aref-bb ii) (aref aa ii ii)))
    (loop :for jj :of-type index-t :from (1+ ii) :to nn :do
      (decf (aref-bb jj) (* (aref aa jj ii) (aref-bb ii))))
    :finally (return bb)))

(defun matrix-solve-upper (aa bb diag1 &optional permutation)
  "Solve Ax=b, put the result in b.
A is assumed to be an upper-triangular matrix."
  (declare (type (array * (* *)) aa) (type (array * (*)) bb))
  (mx-solve-check aa bb)
  (loop :with nn :of-type index-t = (1- (array-dimension aa 0))
    :for ii :of-type index-t :downfrom nn :to 1 :do
    (unless diag1 (divf (aref-bb ii) (aref aa ii ii)))
    (loop :for jj :of-type index-t :downfrom (1- ii) :to 1 :do
      (decf (aref-bb jj) (* (aref aa jj ii) (aref-bb ii))))
    :finally (return bb)))
)
(defsubst mx-swap-rows (mx ii jj)
  (loop :for kk :of-type index-t :from 0 :below (array-dimension mx 1) :do
    (rotatef (aref mx ii kk) (aref mx jj kk))))

(defsubst mx-swap-cols (mx ii jj)
  (loop :for kk :of-type index-t :from 0 :below (array-dimension mx 0) :do
    (rotatef (aref mx kk ii) (aref mx kk jj))))

(defun mx-row-pivots (mx)
  "return the vector of row pivots"
  (let* ((size (array-dimension mx 0))
         (pivots (make-array size)))
    (loop :for ii :from 0 :below size
      :for pivot = (loop :for jj :from 0 :below (array-dimension mx 1)
                     :maximize (abs (aref mx ii jj)))
      :do (when (zerop pivot)
            (error 'division-by-zero :operation 'mx-row-pivots
                   :operands (list mx)))
      (setf (aref pivots ii) (/ pivot)))
    pivots))

(defun mx-lu-sum (mx ii jj)
  "compute the intermediate sum for LU decomposition"
  (loop :for kk :of-type index-t :from 0 :below (min ii jj)
    :sum (* (aref mx ii kk) (aref mx kk jj))))

(defun matrix-lup (mx)
  "LUP decomposition for MX
Numerical Recipies 2.3
return the row permutation vector."
  (unless (and (= 2 (array-rank mx))
               (= (array-dimension mx 0) (array-dimension mx 1)))
    (error 'dimension :proc 'matrix-lup :args
           (list (array-rank mx) (array-dimensions mx))))
  (let* ((size (array-dimension mx 0))
         (pivots (mx-row-pivots mx)) (permutations (make-array size)))
    (loop :for ii :of-type index-t :from 0 :below size
      :do (setf (aref permutations ii) ii))
    (loop :for jj :of-type index-t :from 0 :below size
      :and max = 0 :and imax = 0 :do
      (loop :for ii :of-type index-t :from 0 :below jj :do
        (decf (aref mx ii jj) (mx-lu-sum mx ii jj)))
      (loop :for ii :of-type index-t :from jj :below size
        :for here = (* (aref pivots ii)
                       (abs (decf (aref mx ii jj) (mx-lu-sum mx ii jj))))
        :do (when (< max here) (setq max here imax ii)))
      (when (zerop max)
        (error 'division-by-zero :operation 'mx-row-pivots :operands (list mx)))
      (unless (= jj imax)       ; must swap rows
        (mx-swap-rows mx jj imax)
        (rotatef (aref pivots jj) (aref pivots imax)))
      (rotatef (aref permutations imax) (aref permutations jj))
      (when (zerop (aref mx jj jj))
        (error 'division-by-zero :operation 'matrix-lup :operands (list mx)))
      (unless (= (1+ jj) size)
        (loop :with scale = (/ (aref mx jj jj))
          :for ii :of-type index-t :from (1+ jj) :below size :do
          (mulf (aref mx ii jj) scale))))
    (values permutations mx)))

(defun matrix-solve-lup (mx bb)
  "solve Mx=b in-place"
  (let ((permutation (matrix-lup mx)))
    (matrix-solve-upper mx bb  t  permutation)
    (matrix-solve-lower mx bb nil permutation)))

(defun matrix-lu (mx lu)
  "Decompose the matrix MX into Lower*Upper.
The diagonal elements of the upper-triangular part are 1.
MX and LU can be the same matrix.
If one of the principal minors of MX is 0, `matrix-lu'
 will signal the `division-by-zero' error.
See Horn/Johnson 'Matrix Analysis' 3.5.2."
  (declare (type (array * (* *)) mx lu))
  (unless (and (= 2 (array-rank mx) (array-rank lu))
               (equal (array-dimensions mx) (array-dimensions lu))
               (= (array-dimension mx 0) (array-dimension mx 1)))
    (error 'dimension :proc 'matrix-lu :args
           (list (array-dimensions mx) (array-dimensions lu))))
  (let ((size (array-dimension mx 0)))
    (loop :for ii :of-type index-t :from 0 :below size :do
      (loop :for jj :of-type index-t :from ii :below size
          :do (setf (aref lu jj ii)
                  (- (aref mx jj ii) (mx-lu-sum lu jj ii))))
      (loop :for jj :of-type index-t :from (1+ ii) :below size
          :do (setf (aref lu ii jj)
                  (/ (- (aref mx ii jj) (mx-lu-sum lu ii jj))
                       (aref lu ii ii))))
      :finally (return lu))))

(defun matrix-solve-lu (mx bb &optional (lu mx) (xx bb))
  "Solve the linear system Ax=b.
The result is placed in the 4th (optional, default - b) arg.
The 3rd (optional, default - A) arg, is destructively modified,
 it now contains the Lower*Upper decomposition on A.
If one of the principal minors of A is 0, `matrix-solve-lu'
 will signal the `division-by-zero' error."
  (declare (type (array * (* *)) mx) (type (array * (*)) bb))
  (mx-solve-check mx bb)
  (setq lu (array-check-return lu (array-dimensions mx))
        xx (array-check-return xx (array-dimensions bb)))
  (unless (eq xx bb) (replace xx bb))
  (matrix-lu mx lu)
  (matrix-solve-lower lu xx nil)
  (matrix-solve-upper lu xx t))

;;;
;;; matrix inversion
;;;

;; <garnet:/src/gesture/matrix.lisp>
(defun matrix-inverse (mx)
  "Invert the matrix in-place.
Return the log determinant of the matrix, parity, and the matrix argument."
  (let* ((dim (array-dimension mx 0))
         (logdet 1) (parity 1)  ; log determinant & its sign
         (l (make-array dim))   ; row permutation vector
         (m (make-array dim)))  ; column permutation vector
    (unless (and (= 2 (array-rank mx)) (= dim (array-dimension mx 1)))
      (error 'dimension :proc 'matrix-inverse
             :args (list (array-rank mx) (array-dimensions mx))))
    (loop :with biga = 0 :for k :from 0 :below dim :do
      (setf (aref l k) k
            (aref m k) k
            biga (aref mx k k))
      ;; find the biggest element in the submatrix
      (loop :for i :from k :below dim :do
        (loop :for j :from k :below dim :do
          (when (> (abs (aref mx i j)) (abs biga))
            (setf biga (aref mx i j)
                  (aref l k) i
                  (aref m k) j))))
      ;; interchange rows
      (when (> (aref l k) k)
        (mx-swap-rows mx (aref l k) k))
      ;; interchange columns
      (when (> (aref m k) k)
        (mx-swap-cols mx (aref m k) k))
      ;; divide column by minus pivot (pivot is in biga)
      (when (zerop biga)
        (error 'division-by-zero :operation 'matrix-inverse
               :operands (list mx)))
      (let ((/biga (/ biga)))
        (loop :for i :from 0 :below dim :do
          (when (/= i k)
            (mulf (aref mx i k) (- /biga))))
        ;; reduce matrix
        (loop :for i :from 0 :below dim :do
          (when (/= i k)
            (let ((temp (aref mx i k)))
              (loop :for j :from 0 :below dim :do
                (when (/= j k)
                  (incf (aref mx i j) (* temp (aref mx k j))))))))
        ;; divide row by pivot
        (loop :for j :from 0 :below dim :do
          (when (/= j k)
            (mulf (aref mx k j) /biga)))
        (incf logdet (log (abs biga))) ; DET is the product of pivots
        (when (minusp biga) (setq parity (- parity)))
        (setf (aref mx k k) /biga)))
    ;; final row & column interchanges
    (loop :for k :downfrom (1- dim) :to 0 :do
      (when (> (aref l k) k)
        (mx-swap-cols mx (aref l k) k))
      (when (> (aref m k) k)
        (mx-swap-rows mx (aref m k) k)))
    (values logdet parity mx)))

;;;
;;; `matrix-solve' - all in one
;;;

(defun matrix-solve (mx bb &optional (tm mx) (xx bb))
  "Solve the linear system Ax=b.
The optional 3rd and 4th arguments have the same meaning
 as in `matrix-solve-lu'.
If `matrix-solve-lu' fails, the matrix is inverted.  In this case,
 the 3rd argument contains the inverse, and the determinant
 is returned as the second value."
  (declare (type (array * (* *)) mx) (type (array * (*)) bb))
  (mx-solve-check mx bb)
  (setq tm (array-check-return tm (array-dimensions mx))
        xx (array-check-return xx (array-dimensions bb)))
  (handler-case (matrix-solve-lu mx bb tm xx)
    (division-by-zero (co)
      (declare (ignore co))
      (let ((det (matrix-inverse (array-copy mx tm))))
        (when (zerop det)
          (error "the matrix is degenerate (det = 0):~%~/matrix-print/" mx))
        (values (matrix-multiply tm bb xx) det)))))

(provide :cllib-matrix)
;;; matrix.lisp ends here
