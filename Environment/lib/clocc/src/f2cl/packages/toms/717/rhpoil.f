      SUBROUTINE RHPOIL(NEED, F, N, NF, PT, R, RD, RHOI, YN, W)
      COMMON /FUDGE/ NFUDGE
      INTEGER NFUDGE
      INTEGER NEED(2), N, NF, RHOI(6)
      DOUBLE PRECISION F, PT(3), R(*), RD(*), W(N), YN(2,N)
C PT = PHI AND THETA (WHEN PS == P, I.E. RHOI(2) == RHOI(3))
C
      DOUBLE PRECISION INVCN, LPN, PNORMS, DR7MDC
      EXTERNAL INVCN, LPN, PNORMS, DR7MDC
      INTEGER ERRFLG, I, IM, WCOMP
      DOUBLE PRECISION CI, E, PHI, PHIRI, PHIMRI, PSI, PSI1, PSI2,
     1                 RI, T, T1, T2, THETA, YI
      DOUBLE PRECISION DATAN, DEXP, DLOG,  SQRT
      DOUBLE PRECISION CNN, EIGHT, EXPMAX, EXPMIN, FOUR, HALF, ONE, TWO,
     1                 TWOPI, ZERO
      DATA CNN/0.D+0/, EXPMAX/0.D+0/, EIGHT/8.D+0/, EXPMIN/0.D+0/,
     1     FOUR/4.0D+0/, HALF/0.5D+0/, ONE/1.D+0/, TWO/2.D+0/,
     2     TWOPI/0.D+0/, ZERO/0.D+0/
C
C *** BODY ***
C
      IM = RHOI(1)
      WCOMP = RHOI(6)
      IF (IM .LE. 0) GO TO 800
      IF (IM .GT. 13) GO TO 800
      IF (EXPMAX .GT. ZERO) GO TO 10
         EXPMAX = TWO * DLOG(DR7MDC(5))
         EXPMIN = TWO * DLOG(DR7MDC(2))
         TWOPI = EIGHT * DATAN(ONE)
 10   IF (NEED(1) .EQ. 2) GO TO 240
      F = ZERO
      GO TO (20,20,40,60,80,80,100,120,140,160,180,220,180), IM
C
C *** POISSON RHO (AND CONVENTIONAL IRLS) ***
C
 20   DO 30 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         F = F + YN(2,I)*RI - YN(1,I)*DLOG(RI)
 30      CONTINUE
      GO TO 999
C
C *** LOG LINEAR POISSON ***
C
 40   DO 50 I = 1, N
         E = ZERO
         RI = R(I)
         IF (RI .GT. EXPMAX) GO TO 800
         IF (RI .GT. EXPMIN) E = EXP(RI)
         F = F + YN(2,I)*E - YN(1,I)*RI
         R(I) = E
 50      CONTINUE
      GO TO 999
C
C *** SQUARE-ROOT LINEAR POISSON ***
C
 60   DO 70 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         F = F + YN(2,I)*RI**2 - TWO*YN(1,1)*DLOG(RI)
 70      CONTINUE
      GO TO 999
C
C *** BINOMIAL RHO (AND CONVENTIONAL IRLS) ***
C
 80   DO 90 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         IF (RI .GE. ONE) GO TO 800
         F = F - YN(1,I)*DLOG(RI) - (YN(2,I) - YN(1,I))*DLOG(ONE-RI)
 90      CONTINUE
      GO TO 999
C
C *** BINOMIAL LOGISTIC RHO ***
C
 100  DO 110 I = 1, N
         RI = R(I)
         IF (RI .GE. EXPMAX) GO TO 800
         E = ZERO
         IF (RI .GT. EXPMIN) E = DEXP(RI)
         F = F + YN(2,I)*DLOG(ONE + E) - YN(1,I)*RI
         R(I) = E
 110     CONTINUE
      GO TO 999
C
C *** PROBIT ***
C
 120  DO 130 I = 1, N
         RI = R(I)
         YI = YN(1,I)
         F = F - YI*LPN(RI) - (YN(2,I)-YI)*LPN(-RI)
 130     CONTINUE
        IF (NFUDGE .GT. 0) WRITE(*,*) 'NFUDGE =', NFUDGE
        NFUDGE = 0
      GO TO 999
C
C *** WEIBULL ***
C
 140  DO 150 I = 1, N
         RI = R(I)
         IF (RI .GE. EXPMAX) GO TO 800
         E = ZERO
         IF (RI .GT. EXPMIN) E = DEXP(RI)
         R(I) = E
         T = ZERO
         IF (-E .GT. EXPMIN) T = DEXP(-E)
         F = F + (YN(2,I) - YN(1,I))*E - YN(1,I)*DLOG(ONE - T)
 150     CONTINUE
      GO TO 999
C
C  *** GAMMA ERRORS ***
C
 160  DO 170 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         F = F + YN(1,I)*RI - YN(2,I)*DLOG(RI)
 170     CONTINUE
      GO TO 999
C
C  ***  PREGIBON ERRORS ***
C
C      *** IN THIS CASE, YN(1,I) = Y(I), YN(2,I) = LOG(Y(I))
C      *** AND YN(I,J), J = N+1(1)2*N, I = 1 OR 2 = SCRATCH
C
 180  IF (NF .GT. 1) GO TO 190
      RHOI(4) = 0
      RHOI(5) = 0
 190  I = N + N + 3
C     *** THE YLOG ARRAY PASSED TO PREGRV MUST BE AT LEAST N+2 LONG
      IF (NEED(2) .NE. RHOI(4)) GO TO 200
         I = I + 3*N
         RHOI(5) = NF
         GO TO 210
 200  RHOI(4) = NF
 210  IF (IM .EQ. 11) THEN
        CALL PREGRH(0, F, N, NF, PT, R, RD, RHOI, YN(1,N+1), YN,
     1            YN(1,I))
      ELSE
        CALL PREGRV(0, F, N, NF, PT, R, RD, RHOI, YN(1,N+1), YN,
     1            YN(1,I))
        END IF
      GO TO 999
C
C *** LEAST-SQUARES ***
C
 220  DO 230 I = 1, N
        E = R(I) - YN(1,I)
        F = F + E*E
 230    CONTINUE
      F = HALF * F
      GO TO 999
C
 240  GO TO (250,270,310,350,400,420,460,500,570,620,660,780,660), IM
C
C *** IRLS POISSON DERIVATIVES ***
C
 250  DO 260 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         R(I) = YN(2,I) - YN(1,I) / RI
         RD(I) = YN(2,I) / RI
 260     CONTINUE
      GO TO 820
C
C *** POISSON DERIVATIVES ***
C
 270  DO 300 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         YI = YN(1,I)
         CI = YN(2,I)
         E = YI / RI
         R(I) = CI - E
         RD(I) = E / RI
         GO TO (300, 280, 280, 290), WCOMP
 280     W(I) = CI / RI
         GO TO 300
 290     IF (YI .LE. ZERO) THEN
             W(I) = HALF * CI / RI
         ELSE
            T1 = CI*RI + YI*(DLOG(E/CI) - ONE)
            IF (T1 .NE. ZERO) THEN
               T = R(I)
               W(I) = T*T / (T1+T1)
            ELSE
               W(I) = RD(I)
               END IF
            END IF
 300     CONTINUE
      GO TO 810
C
C *** LOG LINEAR POISSON ***
C
 310  DO 340 I = 1, N
         YI = YN(1,I)
         CI = YN(2,I)
         RI = CI*R(I)
         R(I) = RI - YI
         RD(I) = RI
         GO TO (340,340,320,330), WCOMP
 320     T = RI/YI
         IF (T .EQ. ONE) THEN
            W(I) = YI
         ELSE
            W(I) = YI * ((T - ONE) / DLOG(T))
            ENDIF
         GO TO 340
 330     T1 = RI + YI*(DLOG(YI/RI) - ONE)
         IF (T1 .NE. ZERO) THEN
            T = RI - YI
            W(I) = T*T / (T1+T1)
         ELSE
            W(I) = RD(I)
            END IF
 340     CONTINUE
      IF (WCOMP .LE. 2) GO TO 820
      GO TO 999
C
C *** SQUARE-ROOT LINEAR POISSON ***
C
 350  DO 390 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         YI = YN(1,I)
         CI = YN(2,I)
         E = YI / RI
         R(I) = TWO * (CI*RI - E)
         RD(I) = TWO * (CI + E/RI)
         GO TO (390, 360, 370, 380), WCOMP
 360     W(I) = FOUR * CI
         GO TO 390
 370     T1 = RI -  SQRT(YI/CI)
         IF (T1 .NE. ZERO) THEN
            T = CI*RI - YI/RI
            W(I) = (T+T) / T1
         ELSE
            W(I) = RD(I)
            END IF
         GO TO 390
 380     T1 = CI*RI*RI - YI + YI*DLOG(YI/(CI*RI*RI))
         IF (T1 .NE. ZERO) THEN
            T = CI*RI - YI/RI
            T = T / T1
            W(I) = T + T
         ELSE
            W(I) = RD(I)
            END IF
 390     CONTINUE
      GO TO 810
C
C *** IRLS BINOMIAL ***
C
 400  DO 410 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         IF (RI .GE. ONE) GO TO 800
         YI = YN(1,I)
         CI = YN(2,I)
         T = ONE / (ONE - RI)
         R(I) = (CI - YI) * T  -  YI / RI
         RD(I) = T * CI / RI
 410     CONTINUE
      GO TO 820
C
C *** BINOMIAL ***
C
 420  DO 450 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         IF (RI .GE. ONE) GO TO 800
         YI = YN(1,I)
         T = ONE / (ONE - RI)
         CI = (YN(2,I) - YI) * T
         YI = YI / RI
         R(I) = CI - YI
         RD(I) = T*CI + YI/RI
         GO TO (450,430,430,440), WCOMP
 430     W(I) = T*YN(2,I) / RI
         GO TO 450
 440     YI = YN(1,I)
         CI = YN(2,I)
         T2 = YI / CI
         T1 = (YI - CI)*DLOG((ONE - RI)/(ONE - T2)) + YI*DLOG(T2/RI)
         IF (T1 .NE. ZERO) THEN
            T = (CI*RI - YI)/(RI * (ONE - RI))
            W(I) = T*T / (T1+T1)
         ELSE
            W(I) = RD(I)
            END IF
 450     CONTINUE
      GO TO 810
C
C *** BINOMIAL LOGISTIC ***
C
 460  DO 490 I = 1, N
         RI = R(I)
         YI = YN(1,I)
         CI = YN(2,I)
         T = ONE / (ONE + RI)
         T1 = T * RI * CI
         R(I) = T1 - YI
         RD(I) = T * T1
         GO TO (490,490,470,480), WCOMP
 470     T1 = (ONE + RI)*DLOG(RI*(CI-YI)/YI)
         IF (T1 .NE. ZERO) THEN
            W(I) = ((CI - YI)*RI - YI) / T1
         ELSE
            W(I) = RD(I)
            END IF
         GO TO 490
 480     T1 = CI*DLOG((ONE+RI)*(ONE - YI/CI)) + YI*DLOG(YI/(RI*(CI-YI)))
         IF (T1 .NE. ZERO) THEN
            T = ((CI - YI)*RI - YI) / (ONE + RI)
            W(I) = T*T / (T1+T1)
         ELSE
            W(I) = RD(I)
            END IF
 490     CONTINUE
      IF (WCOMP .LE. 2) GO TO 820
      GO TO 999
C
C *** PROBIT ***
C
 500  IF (CNN .LE. ZERO) CNN = ONE /  SQRT(TWOPI)
      DO 560 I = 1, N
         RI = R(I)
         YI = YN(1,I)
         CI = YN(2,I) - YI
         E = ZERO
         T = -HALF * RI**2
         IF (T .GT. EXPMIN) E = CNN * DEXP(T)
         PHIRI = PNORMS(RI)
         IF (WCOMP .EQ. 2)
     1          W(I) = YN(2,I) * (E / PHIRI) * (E / (ONE - PHIRI))
         IF (PHIRI .LE. ZERO) GO TO 510
            PHIRI = ONE / PHIRI
            T1 = E*PHIRI*YI
            T2 = T1*(RI + PHIRI*E)
            T1 = -T1
            GO TO 520
 510     T1 = YI * (RI + ONE/RI)
         T2 = YI * (ONE - ONE/RI**2)
 520     PHIMRI = PNORMS(-RI)
         IF (PHIMRI .LE. ZERO) GO TO 530
            PHIMRI = ONE / PHIMRI
            T = E*CI*PHIMRI
            R(I) = T + T1
            RD(I) = T*(PHIMRI*E - RI) + T2
            GO TO (560,560,540,550), WCOMP
 530     R(I) = CI*(RI + ONE/RI) + T1
         RD(I) = CI*(ONE - ONE/RI**2) + T2
         GO TO (560,560,540,550), WCOMP
 540     T = RI - INVCN(YI/YN(2,I), ERRFLG)
         IF (ERRFLG .NE. 0) THEN
            WRITE(*,*) 'ERROR FROM INVCN: I, YI, YN(1,I), YN(2,I) ='
     1                  , I, YI, YN(1,I), YN(2,I)
             GO TO 800
             END IF
         IF (T .NE. ZERO) THEN
             W(I) = R(I) / T
         ELSE
             W(I) = RD(I)
             END IF
         GO TO 560
 550     T2 = CI
         CI = YN(2,I)
         T1 = T2*(DLOG(T2/CI) - LPN(-RI))
         IF (YI .GT. ZERO) T1 = T1 + YI*(DLOG(YI/CI) - LPN(RI))
         IF (T1 .NE. ZERO) THEN
             T = R(I)
             W(I) = T*T / (T1+T1)
         ELSE
             W(I) = RD(I)
             END IF
 560     CONTINUE
      GO TO 810
C
C *** WEIBULL ***
C
 570  DO 610 I = 1, N
         RI = R(I)
         E = ZERO
         IF (-RI .GT. EXPMIN) E = DEXP(-RI)
         T = RI / (ONE - E)
         CI = YN(2,I)*RI
         YI = YN(1,I)*T
         R(I) = CI - YI
         RD(I) = CI - YI*(ONE - E*T)
         GO TO (570,580,590,600), WCOMP
 580     W(I) = E*CI*RI / (ONE - E)
         GO TO 610
 590     T1 = DLOG(-RI / DLOG(ONE - YN(1,I)/YN(2,I)))
         IF (T1 .NE. ZERO) THEN
            W(I) = (CI - YI) / T1
         ELSE
            W(I) = RD(I)
            END IF
         GO TO 610
 600     YI = YN(1,I)
         CI = YN(2,I)
         T2 = YI / CI
         CI = CI - YI
         T1 = CI*(RI + DLOG(ONE - T2)) + YI*(DLOG(T2/(ONE - E)))
         IF (T1 .NE. ZERO) THEN
            T = CI - YI
            W(I) = T*T / (T1+T1)
         ELSE
            W(I) = RD(I)
            END IF
 610     CONTINUE
      GO TO 810
C
C  *** GAMMA ERRORS ***
C
 620  DO 650 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
C        F = F + YN(1,I)*RI - YN(2,I)*DLOG(RI)
         T = YN(2,I)/RI
         T1 = ONE
         R(I) = YN(1,I) - T
         RD(I) = T/RI
         GO TO (650,650,630,640), WCOMP
 630     W(I) = YN(1,I) / RI
         GO TO 650
 640     T2 = YN(1,I) * RI / YN(2,I)
         T1 = T2 - ONE
         T = T1*RD(I)*T1
         IF (T .GT. ZERO) THEN
            T2 = T1 - DLOG(T2)
            T = T / (T2+T2)
            END IF
         W(I) = T
 650     CONTINUE
      IF (WCOMP .LE. 2) GO TO 820
      GO TO 999
C
C ***  PREGIBON ERRORS ***
C
 660  IF (WCOMP .GE. 2) CALL DV7CPY(N, W, R)
      I = N + N + 3
      IF (RHOI(4) .EQ. NF) GO TO 670
         I = I + 3*N
         IF (RHOI(5) .EQ. NF) GO TO 670
         WRITE(6,*) 'HELP! NF =', NF, ' BUT RHOI =', RHOI
         GO TO 800
 670  IF (IM .EQ. 11) THEN
         CALL PREGRH(1, F, N, NF, PT, R, RD, RHOI, YN(1,N+1), YN,
     1            YN(1,I))
      ELSE
         CALL PREGRV(1, F, N, NF, PT, R, RD, RHOI, YN(1,N+1), YN,
     1            YN(1,I))
         END IF
      IF (NF .EQ. 0) GO TO 999
      GO TO (820,680,700,720), WCOMP
 680  PSI = PT(3)
      T = (TWO - PT(2))*PSI - TWO
      T1 = PSI*PSI
      DO 690 I = 1, N
 690        W(I) = YN(2,I) * T1 * W(I)**T
      GO TO 999
 700  T = ONE / PT(3)
      DO 710 I = 1, N
         T1 = W(I) - ONE
         IF (T1 .NE. ZERO) THEN
            YI = YN(1,I)
            W(I) = R(I) / (W(I) - YI**T)
         ELSE
            W(I) = RD(I)
            END IF
 710     CONTINUE
      GO TO 999
 720  PHI = PT(1)
      THETA = PT(2)
      PSI = PT(3)
      IF (THETA .EQ. ONE) GO TO 740
      IF (THETA .EQ. TWO) GO TO 760
      T1 = ONE - THETA
      T2 = TWO - THETA
      PSI1 = PSI * T1
      PSI2 = PSI * T2
      DO 730 I = 1, N
         RI = W(I)
         YI = YN(1,I)
         T = YI**T2
         E = YN(2,I)/PHI * ((T - YI*RI**PSI1)/T1 - (T - RI**PSI2)/T2)
         IF (E .NE. ZERO) THEN
            T = R(I)
            W(I) = T*T / (E+E)
         ELSE
            W(I) = RD(I)
            END IF
 730     CONTINUE
      GO TO 999
 740  DO 750 I = 1, N
         RI = W(I)
         YI = YN(1,I)
         T1 = YN(2,I)/PHI * (RI**PSI - YI + YI*(DLOG(YI)-PSI*DLOG(RI)))
         IF (T1 .NE. ZERO) THEN
            T = R(I)
            W(I) = T*T / (T1+T1)
         ELSE
            W(I) = RD(I)
            END IF
 750     CONTINUE
      GO TO 999
 760  DO 770 I = 1, N
         RI = W(I)
         YI = YN(1,I)
         T1 = YI*RI**(-PSI) - ONE + PSI*DLOG(RI) - DLOG(YI)
         IF (T1 .NE. ZERO) THEN
            T1 = T1 * YN(2,I) / PHI
            T = R(I)
            W(I) = T*T / (T1+T1)
         ELSE
            W(I) = RD(I)
            END IF
 770     CONTINUE
      GO TO 999
C
C *** LEAST SQUARES ***
C
 780  DO 790 I = 1, N
         R(I) = R(I) - YN(1,I)
         RD(I) = ONE
 790     CONTINUE
      GO TO 820
C
 800  NF = 0
      GO TO 999
C
 810  IF (WCOMP .GT. 1) GO TO 999
 820  CALL DV7CPY(N, W, RD)
C
 999  RETURN
      END
