C***********************************************************************
C
C     MADRJ
C
C***********************************************************************
      SUBROUTINE MADRJ(N, P, X, NF, NEED, R, RP, UI, UR, UF)
      INTEGER N, P, NF, NEED(2), UI(1)
      DOUBLE PRECISION X(P), R(N), RP(P,N), UR(1)
      EXTERNAL UF
      DOUBLE PRECISION TWO, ZERO
      PARAMETER (TWO=2.D+0, ZERO=0.D+0)
C
C *** BODY ***
C
      IF (NEED(1) .EQ. 2) GO TO 10
      R(1) = X(1)**2 + X(2)**2 + X(1)*X(2)
      R(2) = SIN(X(1))
      R(3) = COS(X(2))
      GO TO 999
C
 10   RP(1,1) = TWO*X(1) + X(2)
      RP(2,1) = TWO*X(2) + X(1)
      RP(1,2) = COS(X(1))
      RP(2,2) = ZERO
      RP(1,3) = ZERO
      RP(2,3) = -SIN(X(2))
C
 999  RETURN
      END
