C A NAG subroutine - contains arithmetic IFs
C
      INTEGER FUNCTION H02BAZ(U,V)
C     NAG COPYRIGHT 1975
C     MARK 4.5 REVISED
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     .. Scalar Arguments ..
      INTEGER                 U, V
C     .. Local Scalars ..
      INTEGER                 W
C     .. Executable Statements ..
      W = U/V
   20 IF (W*V-U) 60, 60, 40
   40 W = W - 1
      GO TO 20
   60 IF ((W+1)*V-U) 80, 80, 100
   80 W = W + 1
      GO TO 60
  100 H02BAZ = W
      RETURN
      END
