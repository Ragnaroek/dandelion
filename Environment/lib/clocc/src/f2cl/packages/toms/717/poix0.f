      SUBROUTINE POIX0(A, IV, LA, LIV, LV, MODEL, N, P, V, X, YN)
C
C *** COMPUTE INITIAL X OF E. L. FROME ***
C
      INTEGER LA, LIV, LV, MODEL, N, P
      INTEGER IV(LIV)
      DOUBLE PRECISION X(P), A(LA,N), V(LV), YN(2,N)
C
      EXTERNAL DIVSET, POISX0, DV7SCP
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER C1, PP1O2, QTR1, TEMP1
      DOUBLE PRECISION ONE, ZERO
C
C  ***  IV COMPONENTS  ***
C
      INTEGER LMAT
      PARAMETER (LMAT=42)
      DATA ONE/1.D+0/, ZERO/0.D+0/
C
C---------------------------------  BODY  ------------------------------
C
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)
C
      C1 = IV(LMAT)
      PP1O2 = P * (P + 1) / 2
      QTR1 = C1 + PP1O2
      TEMP1 = QTR1 + P
      IF (TEMP1 .GT. LV) GO TO 10
      CALL POISX0(A, V(C1), LA, P*(P+1)/2, MODEL, N, P, V(QTR1), X, YN)
      GO TO 999
C
 10   IF (MODEL .GT. 1) GO TO 20
      CALL DV7SCP(P, X, ONE)
      GO TO 999
 20   CALL DV7SCP(P, X, ZERO)
C
 999   RETURN
       END
