      SUBROUTINE BRJ1(M, N, P, X, NF, NEED, R, RP, UI, A, UR, UF)
      INTEGER M, N, P, NF, NEED(2), UI(5)
      DOUBLE PRECISION X(P), R(N), RP(P,N), A(M,N), UR(N,6)
      EXTERNAL UF
      EXTERNAL DD7TPR, DR7MDC
      DOUBLE PRECISION DD7TPR, DR7MDC
C
C *** LOCAL VARIABLES ***
C
      INTEGER I, J, J2, J4, MODEL
      DOUBLE PRECISION ALPHA, BETA1, BETA2, DI, E, EMX, PHI, T, T1,
     1                 THETA, TI, X1, X1INV, X2, X3, X3M1, X4
      DOUBLE PRECISION EXPMAX, EXPMIN, ONE, TWO, ZERO
      DATA EXPMAX/0.D+0/, EXPMIN/0.D+0/, ONE/1.D+0/, TWO/2.D+0/,
     1     ZERO/0.D+0/
C
C *** BODY ***
C
      MODEL = IABS(UI(2))
      IF (MODEL .LE. 0) GO TO 520
      IF (MODEL .GT. 11) GO TO 520
      IF (EXPMAX .GT. ZERO) GO TO 10
         EXPMAX = TWO * DLOG(DR7MDC(5))
         EXPMIN = TWO * DLOG(DR7MDC(2))
 10   IF (NEED(1) .EQ. 2) GO TO 260
      J = 3 - UI(3)
      IF (UI(3+J) .EQ. NEED(2)) J = UI(3)
      UI(3) = J
      UI(3+J) = NF
      J2 = J + 2
      J4 = J + 4
      GO TO (20, 40, 60, 60, 80, 100, 120, 170, 190, 210, 230), MODEL
C
C *** LINEAR MODEL ***
C
 20   DO 30 I = 1, N
 30      R(I) = DD7TPR(P, X, A(1,I))
      GO TO 999
C
C *** EXPONENTIAL OF LINEAR ***
C
 40   DO 50 I = 1, N
         T = DD7TPR(P, X, A(1,I))
         IF (T .GE. EXPMAX) GO TO 520
         E = ZERO
         IF (T .GT. EXPMIN) E = DEXP(T)
         R(I) = E
         UR(I,J) = E
 50      CONTINUE
      GO TO 999
C
C *** NONLINEAR POISSON EXAMPLE FROM FROME*S PREG MANUAL ***
C
 60   X1 = X(1)
      X2 = X(2)
      X3 = X(3)
      DO 70 I = 1, N
         E = DEXP(-X2*A(2,I))
         UR(I,J2) = E
         T = (ONE - E) ** X3
         UR(I,J4) = T
         T = X1*A(1,I) * (ONE - T)
         IF (T .LE. ZERO) GO TO 520
         UR(I,J) = T
         IF (MODEL .EQ. 3) T = DLOG(T)
         R(I) = T
 70      CONTINUE
      GO TO 999
C
C *** CAESIUM DOSE EFFECT MODEL ***
C
 80   X1 = X(1)
      X2 = X(2)
      X3 = X(3)
      DO 90 I = 1, N
         DI = A(1,I)
         TI = A(2,I)
         IF (X3 .EQ. ZERO) GO TO 520
         IF (TI .EQ. ZERO) GO TO 520
         T = -TI / X3
         IF (T .GE. EXPMAX) GO TO 520
         E = ZERO
         IF (T .GT. EXPMIN) E = DEXP(T)
         UR(I,J) = E
         T = X3 / TI
         T = DI * (X2 + TWO*T*DI*(ONE - T*(ONE - E)))
         UR(I,J2) = T
         R(I) = X1 * T
 90      CONTINUE
      GO TO 999
C
C *** LUNG CANCER MODEL ***
C
 100  X1 = X(1)
      X2 = X(2)
      X3 = X(3)
      X4 = X(4)
      EMX = EXPMAX - 10.D+0
      DO 110 I = 1, N
         T1 = X1 * A(1,I)
         T = X2 + X3*A(2,I) + T1
         IF (T .GE. EMX) GO TO 520
         E = ZERO
         IF (T .GT. EXPMIN) E = DEXP(T)
         T = X4 + T1
         IF (T .GE. EMX) GO TO 520
         T1 = ZERO
         IF (T .GT. EXPMIN) T1 = DEXP(T)
         T = E + T1
         R(I) = T
         UR(I,J) = E
         UR(I,J2) = T1
         UR(I,J4) = T
 110     CONTINUE
      GO TO 999
C
C *** LOGISTIC OF LINEAR ***
C
 120  DO 160 I = 1, N
         T = DD7TPR(P, A(1,I), X)
         IF (T .LE. EXPMIN) GO TO 130
         IF (T .GE. EXPMAX) GO TO 140
         E = DEXP(T)
         T1 = ONE / (ONE + E)
         T = E * T1
         T1 = T * T1
         GO TO 150
 130     T = ZERO
         T1 = ZERO
         GO TO 150
 140     T = ONE
         T1 = ZERO
 150     R(I) = T
         UR(I,J) = T1
 160     CONTINUE
      GO TO 999
C
C *** LOG OF LINEAR ***
C
 170  DO 180 I = 1, N
         T = DD7TPR(P, X, A(1,I))
         IF (T .LE. ZERO) GO TO 520
         R(I) = DLOG(T)
         UR(I,J) = T
 180     CONTINUE
      GO TO 999
C
C *** EXAMPLE ON P. 204 OF MCCULLAGH AND NELDER ***
C
 190  ALPHA = X(1)
      BETA1 = X(2)
      BETA2 = X(3)
      PHI = X(4)
      DO 200 I = 1, N
         X2 = A(2,I)
         R(I) = ALPHA + BETA1*DLOG(A(1,I)) + BETA2*X2/(PHI + X2)
 200     CONTINUE
      GO TO 999
C
C *** EXAMPLE ON P. 205 OF MCCULLAGH AND NELDER ***
C
 210  ALPHA = X(1)
      BETA1 = X(2)
      BETA2 = X(3)
      PHI = X(4)
      THETA = X(5)
      DO 220 I = 1, N
         X2 = A(2,I)
         T = A(1,I) - THETA
         IF (T .LE. ZERO) GO TO 520
         R(I) = ALPHA + BETA1*DLOG(T) + BETA2*X2/(PHI + X2)
 220     CONTINUE
      GO TO 999
C
C *** EXAMPLE P. 202 OF MCCULLAGH AND NELDER ***
C
 230  DO 250 I = 1, N
         T = X(1)
         DO 240 J = 1, 3
            T1 = A(J,I) + X(2*J+1)
            IF (T1 .LE. ZERO) GO TO 520
 240        T = T + X(2*J)/T1
         R(I) = T
 250     CONTINUE
      GO TO 999
C
C *** JACOBIAN EVALUATIONS...
C
 260  J = UI(3)
      IF (NF .EQ. UI(J+3)) GO TO 270
      J = 3 - J
      IF (NF .EQ. UI(J+3)) GO TO 270
      WRITE(6,*) 'HELP! UNAVAILABLE INTERMEDIATE INFO!'
      GO TO 520
 270  J2 = J + 2
      J4 = J + 4
      GO TO (280, 290, 310, 340, 370, 390, 410, 430, 450, 470, 490),
     1           MODEL
C
C *** LINEAR MODEL ***
C
C
 280  CALL DV7CPY(N*P, RP, A)
      GO TO 999
C
C *** EXPONENTIAL OF LINEAR MODEL ***
C
 290  DO 300 I = 1, N
 300     CALL DV7SCL(P, RP(1,I), UR(I,J), A(1,I))
      GO TO 999
C
C *** LOG OF NONLINEAR POISSON EXAMPLE FROM FROME*S PREG MANUAL ***
C
 310  X1 = X(1)
      X2 = X(2)
      X3 = X(3)
      X3M1 = X3 - ONE
      X1INV = ONE / X1
      DO 330 I = 1, N
         RP(1,I) = X1INV
         E = UR(I,J2)
         T1 = ONE - E
         T = -A(1,I) * X1 / UR(I,J)
         RP(2,I) = T * X3 * A(2,I) * E * T1**X3M1
         IF (T1 .LE. ZERO) GO TO 320
         RP(3,I) = T * UR(I,J4) * DLOG(T1)
         GO TO 330
 320     RP(3,I) = ZERO
 330     CONTINUE
      GO TO 999
C
C *** NONLINEAR POISSON EXAMPLE FROM FROME*S PREG MANUAL ***
C
 340  X1 = X(1)
      X2 = X(2)
      X3 = X(3)
      X3M1 = X3 - ONE
      X1INV = ONE / X1
      DO 360 I = 1, N
         RP(1,I) = A(1,I) * (ONE - UR(I,J4))
         E = UR(I,J2)
         T1 = ONE - E
         T = -A(1,I) * X1
         RP(2,I) = T * X3 * A(2,I) * E * T1**X3M1
         IF (T1 .LE. ZERO) GO TO 350
         RP(3,I) = T * UR(I,J4) * DLOG(T1)
         GO TO 360
 350     RP(3,I) = ZERO
 360     CONTINUE
      GO TO 999
C
C *** CAESIUM DOSE EFFECT MODEL ***
C
 370  X1 = X(1)
      X3 = X(3)
      DO 380 I = 1, N
         RP(1,I) = UR(I,J2)
         DI = A(1,I)
         TI = A(2,I)
         RP(2,I) = X1 * DI
         E = UR(I,J)
         T = TWO * X3 / TI
         RP(3,I) = TWO * X1 * (DI/TI) * DI * (ONE - T + E*(T + ONE))
 380     CONTINUE
      GO TO 999
C
C *** LUNG CANCER MODEL ***
C
 390  DO 400 I = 1, N
         RP(1,I) = UR(I,J4) * A(1,I)
         T = UR(I,J)
         RP(2,I) = T
         RP(3,I) = T * A(2,I)
         RP(4,I) = UR(I,J2)
 400     CONTINUE
      GO TO 999
C
C *** LOGISTIC OF LINEAR ***
C
 410  DO 420 I = 1, N
 420     CALL DV7SCL(P, RP(1,I), UR(I,J), A(1,I))
      GO TO 999
C
C *** LOG OF LINEAR ***
C
 430  DO 440 I = 1, N
 440     CALL DV7SCL(P, RP(1,I), ONE/UR(I,J), A(1,I))
      GO TO 999
C
C *** EXAMPLE ON P. 204 OF MCCULLAGH AND NELDER ***
C
 450  ALPHA = X(1)
      BETA1 = X(2)
      BETA2 = X(3)
      PHI = X(4)
      DO 460 I = 1, N
         X2 = A(2,I)
C        R(1,I) = ALPHA + BETA1*DLOG(A(1,I)) + BETA2*X2/(PHI + X2)
         RP(1,I) = ONE
         RP(2,I) = DLOG(A(1,I))
         RP(3,I) = X2/(PHI + X2)
         RP(4,I) = -BETA2*X2/(PHI + X2)**2
         RP(1,I) = ONE
 460     CONTINUE
      GO TO 999
C
C
C *** EXAMPLE ON P. 205 OF MCCULLAGH AND NELDER ***
C
 470  ALPHA = X(1)
      BETA1 = X(2)
      BETA2 = X(3)
      PHI = X(4)
      THETA = X(5)
      DO 480 I = 1, N
         X2 = A(2,I)
C        R(I) = ALPHA + BETA1*DLOG(A(1,I) - THETA) + BETA2*X2/(PHI + X2)
         RP(1,I) = ONE
         RP(2,I) = DLOG(A(1,I) - THETA)
         RP(3,I) = X2/(PHI + X2)
         RP(4,I) = -BETA2*X2/(PHI + X2)**2
         RP(5,I) = -BETA1/(A(1,I) - THETA)
 480     CONTINUE
      GO TO 999
C
C *** EXAMPLE P. 202 OF MCCULLAGH AND NELDER ***
C
 490  DO 510 I = 1, N
C        DO 453 J = 1, 3
C453        RI = RI + X(2*J)/(A(J,I) + X(2*J+1))
         RP(1,I) = ONE
         DO 500 J = 1, 3
            T = ONE / (A(J,I) + X(2*J+1))
            RP(2*J,I) = T
            RP(2*J+1,I) = -X(2*J)*T*T
 500        CONTINUE
 510     CONTINUE
      GO TO 999
 520  NF = 0
 999  RETURN
      END
