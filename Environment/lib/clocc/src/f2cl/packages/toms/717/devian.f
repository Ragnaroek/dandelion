      SUBROUTINE DEVIAN(F, MODEL0, N, NW, PT, YN)
      INTEGER MODEL0, N, NW
      DOUBLE PRECISION F, PT(2), YN(2,N)
      DOUBLE PRECISION DATAN, DLOG
      INTEGER I, MODEL
      DOUBLE PRECISION CI, D, S, T, T1, YI
      DOUBLE PRECISION EIGHT, HALF, ONE, TWO, ZERO
      DATA EIGHT/8.D+0/, HALF/0.5D+0/, ONE/1.D+0/, TWO/2.D+0/,
     1     ZERO/0.D+0/
C
C *** BODY ***
C
      D = F
      MODEL = IABS(MODEL0)
      IF (MODEL .LT. 5) GO TO 20
      IF (MODEL .GT. 9) GO TO (40, 60, 999, 80) MODEL - 9
C
C *** BINOMIAL DEVIANCE ***
C
      DO 10 I = 1, N
         YI = YN(1,I)
         CI = YN(2,I)
         T = YI / CI
         IF (T .GT. ZERO) D = D + YI*DLOG(T)
         IF (T .LT. ONE) D = D + (CI-YI)*DLOG(ONE-T)
 10      CONTINUE
      GO TO 100
C
C *** POISSON DEVIANCE ***
C
 20   DO 30 I = 1, N
         YI = YN(1,I)
         IF (YI .GT. ZERO) D = D + YI*(DLOG(YI/YN(2,I)) - ONE)
 30      CONTINUE
      GO TO 100
C
C *** GAMMA DEVIANCE ***
C
 40   DO 50 I = 1, N
         YI = YN(1,I)
         IF (YI .LE. ZERO) GO TO 999
         D = D - YN(2,I)*(ONE + DLOG(YI))
 50      CONTINUE
      GO TO 100
C
C  *** PREGIBON DEVIANCE, REPLICATE WEIGHTS ***
C
 60   T = PT(2)
      T1 = DLOG(EIGHT*DATAN(ONE)*PT(1))
      S = ZERO
      DO 70 I = 1, N
 70      S = S + YN(2,I) * (T*DLOG(DBLE(YN(1,I))) + T1)
      D = PT(1) * (D - HALF*S)
      GO TO 100
C
C  *** PREGIBON DEVIANCE, VARIANCE WEIGHTS ***
C
 80   S = ZERO
      T = ZERO
      DO 90 I = 1, N
         S = S + DLOG(DBLE(YN(1,I)))
         T = T + DLOG(DBLE(YN(2,I)))
 90      CONTINUE
      D = PT(1) * (D -
     1     HALF*(PT(2)*S - T + N*DLOG(EIGHT*DATAN(ONE)*PT(1))))
C
 100  WRITE(NW,*) 'DEVIANCE = ', TWO*D
 999  RETURN
      END
