      SUBROUTINE DQ7ADR(P, QTR, RMAT, W, Y)
C
C  ***  ADD ROW W TO QR FACTORIZATION WITH R MATRIX RMAT AND
C  ***  Q**T * RESIDUAL = QTR.  Y = NEW COMPONENT OF RESIDUAL
C  ***  CORRESPONDING TO W.
C
      INTEGER P
      DOUBLE PRECISION QTR(P), RMAT(1), W(P), Y
C     DIMENSION RMAT(P*(P+1)/2)
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, II, IJ, IP1, J
      DOUBLE PRECISION RI, RW, T, U1, U2, V, WI, WR
C
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)
C
C------------------------------ BODY -----------------------------------
C
      II = 0
      DO 60 I = 1, P
         II = II+I
         WI = W(I)
         IF (WI .EQ. ZERO) GOTO  60
         RI = RMAT(II)
         IF (RI .NE. ZERO) GOTO 20
            IJ = II
C           *** SWAP W AND ROW I OF RMAT ***
            DO 10 J = I, P
               T = RMAT(IJ)
               RMAT(IJ) = W(J)
               W(J) = T
               IJ = IJ+J
 10            CONTINUE
            T = QTR(I)
            QTR(I) = Y
            Y = T
            GO TO 60
 20      IP1 = I+1
         IJ = II+I
         IF ( ABS(WI) .LE.  ABS(RI)) GO TO 40
            RW = RI/WI
            T =  SQRT(ONE+RW**2)
            IF (RW .GT. ZERO) T = -T
            V = RW-T
            U1 = ONE/T
            U2 = ONE/(T*V)
            RMAT(II) = WI*T
            T = Y+V*QTR(I)
            QTR(I) = QTR(I)+T*U1
            Y = Y+T*U2
            IF (IP1 .GT. P) GO TO 60
            DO 30 J = IP1, P
               T = W(J)+V*RMAT(IJ)
               RMAT(IJ) = RMAT(IJ)+T*U1
               W(J) = W(J)+T*U2
               IJ = IJ+J
 30            CONTINUE
            GO TO 60
C
C        *** AT THIS POINT WE MUST HAVE ABS(WI) .LE. ABS(RI)...
C
 40      WR = WI/RI
         T = - SQRT(ONE+WR**2)
         V = WR/(ONE-T)
         U1 = ONE/T-ONE
         U2 = V*U1
         RMAT(II) = RI*T
         T = QTR(I)+V*Y
         QTR(I) = QTR(I)+T*U1
         Y = Y+T*U2
         IF (IP1 .GT. P) GO TO 60
         DO 50 J = IP1, P
            T = RMAT(IJ)+V*W(J)
            RMAT(IJ) = RMAT(IJ)+T*U1
            W(J) = W(J)+T*U2
            IJ = IJ+J
 50         CONTINUE
 60      CONTINUE
 999  RETURN
      END
