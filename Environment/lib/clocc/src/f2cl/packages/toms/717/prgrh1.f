      SUBROUTINE PRGRH1(N, PT, R, RHO, RHOI, YN)
C
C  ***  RHO FOR PREGIBON ERROR MODELS ***
C
      INTEGER N, RHOI(3)
      DOUBLE PRECISION PT(2), R(*), RHO(N), YN(2,N)
C *** LOCAL VARIABLES ***
C
      INTEGER I
      DOUBLE PRECISION HTHETA, PHI, RI, RT1, T, T1, T1INV, T2, T2INV,
     1                  THETA, YI, YT1
C
      DOUBLE PRECISION HALF, ONE, TWO
      DATA HALF/0.5D+0/, ONE/1.D+0/, TWO/2.D+0/
C
C *** BODY ***
C
      PHI = PT(1)
      THETA = PT(2)
      HTHETA = HALF * THETA
      DO 10 I = 1, N
 10      RHO(I) = HTHETA*DLOG(PHI*YN(1,I))
      IF (THETA .EQ. ONE) GO TO 30
      IF (THETA .EQ. TWO) GO TO 50
      T1 = ONE - THETA
      T1INV = ONE / T1 / PHI
      T2 = TWO - THETA
      T2INV = ONE / T2 / PHI
      DO 20 I = 1, N
         RI = R(I)
         YI = YN(1,I)
         RT1 = RI**T1
         YT1 = YI**T1
         RHO(I) = RHO(I) + T2INV*(RI*RT1 - YI*YT1) + YI*T1INV*(YT1- RT1)
 20      CONTINUE
      GO TO 999
 30   DO 40 I = 1, N
         RI = R(I)
         YI = YN(1,I)
         T = RI - YI - YI*DLOG(RI/YI)
         RHO(I) = RHO(I) + T / PHI
 40      CONTINUE
      GO TO 999
 50   DO 60 I = 1, N
         YI = YN(1,I) / R(I)
         T = YI - DLOG(YI) - ONE
         RHO(I) = RHO(I) + T / PHI
 60      CONTINUE
 999  RETURN
      END
