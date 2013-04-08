      SUBROUTINE RPOIL0(MDL, N, PT, R, RHO, YN)
      INTEGER N, MDL(1)
      DOUBLE PRECISION PT(1), R(N), RHO(N), YN(2,N)
      EXTERNAL LPN, DR7MDC
      DOUBLE PRECISION LPN, DR7MDC
      INTEGER I, MODEL
      DOUBLE PRECISION E, RI, T, YI
      DOUBLE PRECISION DEXP, DLOG
      DOUBLE PRECISION EXPMAX, EXPMIN, HALF, ONE, TWO, ZERO
      DATA EXPMAX/0.D+0/, EXPMIN/0.D+0/,
     1     HALF/0.5D+0/, ONE/1.D+0/, TWO/2.D+0/, ZERO/0.D+0/
C
C *** BODY ***
C
      MODEL = MDL(1)
      I = MODEL + 2
      IF (I .LE. 0 .OR. I .GT. 11) THEN
        WRITE(6,*) 'HELP! RPOIL0 HAS MODEL =', MODEL
        STOP
        END IF
      IF (EXPMAX .GT. ZERO) GO TO 10
         EXPMAX = TWO * DLOG(DR7MDC(5))
         EXPMIN = TWO * DLOG(DR7MDC(2))
 10   GO TO (20, 20, 40, 60, 80, 80, 100, 120, 140, 160, 180), I
C
C *** POISSON RHO (AND CONVENTIONAL IRLS) ***
C
 20   DO 30 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) THEN
                RI = ONE
                R(I) = ONE
                END IF
         RHO(I) = YN(2,I)*RI - YN(1,I)*DLOG(RI)
 30      CONTINUE
      GO TO 999
C
C *** LOG LINEAR ***
C
 40   DO 50 I = 1, N
         E = ZERO
         RI = R(I)
         IF (RI .GT. EXPMAX) THEN
                RI = HALF * EXPMAX
                R(I) = RI
                END IF
         IF (RI .GT. EXPMIN) E = EXP(RI)
         RHO(I) = YN(2,I)*E - YN(1,I)*RI
 50      CONTINUE
      GO TO 999
C
C *** SQUARE-ROOT LINEAR POISSON ***
C
 60   DO 70 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) THEN
                RI = ONE
                R(I) = RI
                END IF
         RHO(I) = YN(2,I)*RI**2 - TWO*YN(1,1)*DLOG(RI)
 70      CONTINUE
      GO TO 999
C
C *** BINOMIAL RHO (AND CONVENTIONAL IRLS) ***
C
 80   DO 90 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO .OR. RI .GE. ONE) THEN
                RI = HALF
                R(I) = RI
                END IF
         RHO(I) = -YN(1,I)*DLOG(RI) - (YN(2,I) - YN(1,I))*DLOG(ONE-RI)
 90      CONTINUE
      GO TO 999
C
C *** BINOMIAL LOGISTIC RHO ***
C
 100  DO 110 I = 1, N
         RI = R(I)
         IF (RI .GT. EXPMAX) THEN
                RI = HALF * EXPMAX
                R(I) = RI
                END IF
         E = ZERO
         IF (RI .GT. EXPMIN) E = DEXP(RI)
         RHO(I) = YN(2,I)*DLOG(ONE + E) - YN(1,I)*RI
 110     CONTINUE
      GO TO 999
C
C *** PROBIT ***
C
 120  DO 130 I = 1, N
         RI = R(I)
         YI = YN(1,I)
         RHO(I) = -YI*LPN(RI) - (YN(2,I)-YI)*LPN(-RI)
 130     CONTINUE
      GO TO 999
C
C *** WEIBULL ***
C
 140  DO 150 I = 1, N
         RI = R(I)
         IF (RI .GT. EXPMAX) THEN
                RI = HALF * EXPMAX
                R(I) = RI
                END IF
         E = ZERO
         IF (RI .GT. EXPMIN) E = DEXP(RI)
         T = ZERO
         IF (-E .GT. EXPMIN) T = DEXP(-E)
         RHO(I) = (YN(2,I) - YN(1,I))*E - YN(1,I)*DLOG(ONE - T)
 150     CONTINUE
      GO TO 999
C
C  *** GAMMA ERRORS ***
C
 160  DO 170 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) THEN
                WRITE(6,*) 'HELP! CHKDER HAS R(',I,') =', RI,' < 0'
                STOP
                END IF
         RHO(I) = YN(2,I) * (YN(1,I)*RI - DLOG(RI))
 170     CONTINUE
      GO TO 999
C
C  ***  PREGIBON ERRORS ***
C
C      *** IN THIS CASE, YN(1,I) = Y(I), YN(2,I) = LOG(Y(I))
C      *** AND YN(I,J), J = N+1(1)2*N, I = 1 OR 2 = SCRATCH
C
 180  DO 190 I = 1, N
         IF (R(I) .LT. ZERO) R(I) = -R(I)
 190     CONTINUE
      CALL PRGRH1(N, PT, R, RHO, MDL, YN)
C
 999  RETURN
      END
