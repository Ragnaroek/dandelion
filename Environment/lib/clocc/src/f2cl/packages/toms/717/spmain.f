      PROGRAM PMAIN
C *** MAIN PROGRAM FOR RUNNING PREG EXAMPLES USING  GLG ***
      INTEGER LIV, LV, MMAX, NMAX, NW, NR0, PMAX
      PARAMETER (LIV=200, LV=8000, NW=6, MMAX = 18, NMAX=200, NR0=8,
     1           PMAX=20)
      CHARACTER*72 FNAME
      CHARACTER*6 ALGNAM(4)
      INTEGER ALG, I, IV(LIV), J, J0, J1, K, KDIAG, M, MDL(6), MODEL,
     1        N, NIN, NR, NRUN, P, P0, PS, RHOI(NMAX+6), UI(7)
      REAL A((MMAX+6)*NMAX), B(2,PMAX),
     1                 RHOR((17+PMAX)*NMAX+4), T, T1, V(LV), X(PMAX+3),
     1                 X0(PMAX+3), YN(2,7*NMAX+3)
      EQUIVALENCE (RHOI(1), MDL(1)), (RHOR(1), YN(1,1))
      CHARACTER*96 DESC, FMT
      CHARACTER*8 WNAME(4)
      REAL  R7MDC
      EXTERNAL BRJ, CHKDER, DEVIAN,  GLF,  GLFB,  GLG,  GLGB,  IVSET,
     1          R7MDC,  V7CPY,  V7SCP, LOUCHK, POIX0, RHPOIL, RPOIL0
      REAL ONE
      INTEGER BS, BSSTR, F, FLO, FLOSTR, LOO, NB, NFIX, RDREQ, XNOTI
      PARAMETER (BS=85, BSSTR=86, F=10, FLO=88, FLOSTR=89, LOO=84,
     1           NB=87, NFIX=83, RDREQ=57, XNOTI=90)
      DATA ALG/1/, KDIAG/0/, NIN/5/
      DATA ALGNAM(1)/' GLG'/,  ALGNAM(2)/' GLF'/
      DATA ALGNAM(3)/' GLGB'/, ALGNAM(4)/' GLFB'/
      DATA ONE/1.E+0/
      DATA WNAME(1)/'  RHO"  '/, WNAME(2)/'  IRLS  '/,
     1     WNAME(3)/' SCORE  '/, WNAME(4)/'DEVIANCE'/
C
C *** BODY ***
C
      CALL  IVSET(1, IV, LIV, LV, V)
      IV(FLO) = 16*NMAX + 5
      IV(XNOTI) = IV(FLO) + NMAX
      IV(BS) = 7
      IV(BSSTR) = 1
      IV(FLOSTR) = 1
      IV(LOO) = 1
      IV(NB) = 5
      IV(NFIX) = 0
      CALL  V7SCP(NMAX, RHOR(IV(FLO)), ONE)
      CALL  V7SCP(NMAX, RHOR(IV(XNOTI)), -2.E+0)
      DO 10 I = IV(BS), IV(BS) + NMAX - 1
 10      RHOI(I) = 1
      T =  R7MDC(6)
      DO 20 I = 1, PMAX
         B(1,I) = -T
         B(2,I) = T
 20      CONTINUE
      NRUN = 0
      MDL(6) = 1
 30   READ(NIN,*,END=210) K
      WRITE(NW,*) '*', K
      GO TO (40, 50, 60, 70, 80, 90, 100, 110, 170, 180, 220,
     1       230, 240, 250, 260, 270, 300, 310, 320, 340,
     2       350, 360, 370, 380, 390, 430, 440, 450),  K
      WRITE(NW,*) '/// Invalid command', K
 40   WRITE(NW,*) '1 = LIST MENU'
      WRITE(NW,*) '2 = READ IV'
      WRITE(NW,*) '3 = READ V'
      WRITE(NW,*)
     1 '4 = READ ALG: 1 =  GLG, 2 =  GLF, 3 =  GLGB, 4 =  GLFB'
      WRITE(NW,*) '5 = READ ALL OF X0'
      WRITE(NW,*) '6 = COPY X TO X0'
      WRITE(NW,*) '7 = START'
      WRITE(NW,*) '8 = CONTINUE'
      WRITE(NW,*) '9 = READ COMMANDS FROM SPECIFIED FILE'
      WRITE(NW,*) '10 = READ PROBLEM'
      WRITE(NW,*) '11 = READ RHO'
      WRITE(NW,*) '12 = READ MODEL'
      WRITE(NW,*) '13 = CHECK RHO DERIVATIVES'
      WRITE(NW,*) '14 = READ P'
      WRITE(NW,*) '15 = READ X0 COMPONENTWISE'
      WRITE(NW,*) '16 = read new Y'
      WRITE(NW,*)
     1 '17 = negate RHO (negative ==> use weights; see KW = 19)'
      WRITE(NW,*) '18 = read KDIAG: 1 = from X*, 2 = from X0, 3 = both'
      WRITE(NW,*)
     1 '19 = read KW: 1 = RHO", 2 = IRLS, 3 = score, 4 = deviance'
      WRITE(NW,*) '20 = READ B (format i, b(1,i), b(2,i))'
      WRITE(NW,*) '21,22 = Read,Show RHOI (componentwise)'
      WRITE(NW,*) '23,24 = Read,Show RHOR        "'
      WRITE(NW,*) '25 = Show range of RHOR components'
      WRITE(NW,*) '26,27 = Show IV, V components'
      WRITE(NW,*) '28 = Read and echo comment'
      GO TO 30
 50   READ(NIN,*,END=210) I, J
      IF (I .LE. 0) GO TO 30
      IV(I) = J
      GO TO 50
 60   READ(NIN,*,END=210) I, T
      IF (I .LE. 0) GO TO 30
      V(I) = T
      GO TO 60
 70   READ(NIN,*,END=210) ALG
      GO TO 30
 80   READ(NIN,*,END=210) (X0(I), I = 1, P0)
      GO TO 30
 90   CALL  V7CPY(P0+3, X0, X)
      GO TO 30
 100  CALL  V7CPY(P0+3, X, X0)
      IV(1) = 12
 110  UI(6) = M
      NRUN = NRUN + 1
      IF (IV(1) .EQ. 0 .OR. IV(1) .EQ. 12) THEN
         WRITE(NW,'(/'' Run'',I5,'':  calling '',A,'' with PS ='',I5)')
     1      NRUN, ALGNAM(ALG), PS
       ELSE
         WRITE(NW,'(/'' Run'',I5,'':  continuing '',A,'', PS ='',I5)')
     1      NRUN, ALGNAM(ALG), PS
         END IF
      IF (KDIAG .GT. 0) IV(RDREQ) = 2
      GO TO (120,130,140,150), ALG
 120  CALL  GLG(N, P, PS, X, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 160
 130  CALL  GLF(N, P, PS, X, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 160
 140  CALL  GLGB(N, P, PS, X, B, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 30
 150  CALL  GLFB(N, P, PS, X, B, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 30
 160  IF (IV(1) .LT. 8) THEN
         CALL DEVIAN(V(F), MDL(1), N, NW, X(PS+1), YN)
         IF (ALG .EQ. 1) CALL LOUCHK(KDIAG,    GLG, X0, N, P, PS, X,
     1       RHPOIL, MDL, YN, IV, LIV, LV, V, BRJ, UI, A, BRJ)
         IF (ALG .EQ. 2) CALL LOUCHK(KDIAG,    GLF, X0, N, P, PS, X,
     1       RHPOIL, MDL, YN, IV, LIV, LV, V, BRJ, UI, A, BRJ)
         END IF
      GO TO 30
 170  IF (NIN .LE. 1) THEN
         WRITE(NW,*) '*** TOO MANY FILES OPEN'
         GO TO 30
         END IF
      READ(NIN,'(A)',END=200) FNAME
      NIN = NIN - 1
      OPEN(NIN,FILE=FNAME,STATUS='OLD',ERR=410)
      REWIND NIN
      GO TO 30
 180  READ(NIN,'(A)',END=200) FNAME
      IF (FNAME .EQ. '-') THEN
         NR = NIN
      ELSE
         OPEN(NR0,FILE=FNAME,STATUS='OLD',ERR=410)
         REWIND NR0
         NR = NR0
         END IF
      READ(NR, '(A)', END=200) DESC
      WRITE(NW,*) DESC
      READ(NR, '(9I4)', END=200) N, P, MODEL, M, MDL(1), I, J, PS
      P0 = P
      IF (PS .EQ. 0) PS = P
      IF (MODEL .LE. 2) M = PS
      IF (MIN(MDL(1),M,N,PS,P-PS+1,MODEL+1) .LE. 0 .OR. P .GT. PMAX
     1          .OR. M .GT. MMAX) THEN
         WRITE(NW,*) 'INVALID PROBLEM DIMENSIONS: M, N, P, MODEL  =',
     1                  M, N, P, MODEL
         STOP
         END IF
      MDL(2) = P
      MDL(3) = PS
      UI(1) = M
      UI(2) = MODEL
      UI(3) = 2
      UI(4) = 0
      UI(5) = 0
      UI(7) = PS
      CALL  V7SCP(3, X0(P+1), ONE)
      IF (MODEL .GT. 2) THEN
        READ(NR, *, END=200) (X0(I), I = 1, P)
       ELSE IF (PS .LT. P) THEN
        READ(NR, *, END=200) (X0(I), I = PS+1, P)
        END IF
      READ(NR, '(A)', END=200) FMT
      J1 = 0
      DO 190 I = 1, N
         J0 = J1 + 1
         J1 = J1 + M
         READ(NR, FMT, END=200) YN(1,I), YN(2,I), (A(J), J = J0, J1)
C        FROME*S DOCUMENTATION CLAIMS Y(I) IS YBAR(I), BUT HIS PROGRAM
C        ASSUMES IT IS THE TOTAL COUNT AND TURNS Y(I) INTO YBAR(I)
C        BY THE EQUIVALENT OF THE FOLLOWING STATEMENT...
C        YN(1,I) = YN(1,I) / YN(2,I)
 190     CONTINUE
      IF (MODEL .LE. 2) THEN
          CALL POIX0(A, IV, PS, LIV, LV, MODEL, N, PS, V, X0, YN)
          END IF
      GO TO 30
 200  WRITE(NW,*) '*** PREMATURE END OF FILE'
      IF (NR .NE. NIN) GO TO 30
 210  IF (NIN .GE. 5) STOP
      NIN = NIN + 1
      GO TO 30
 220  READ(NIN,*,END=210) I
      IF (I .LE. 0) I = MDL(1)
      WRITE(NW,*) 'Changing RHO from ', MDL(1), ' to ', I
      MDL(1) = I
      GO TO 30
 230  READ(NIN,*,END=210) I
      IF (I .EQ. 0) I = MODEL
      WRITE(NW,*) 'Changing MODEL from ', MODEL, ' to ', I
      MODEL = I
      UI(2) = MODEL
      GO TO 30
 240  CALL CHKDER(MDL, N, P-PS, X0(PS+1), V(200), RHPOIL, RPOIL0, YN)
      GO TO 30
 250  READ(NIN,*,END=210) I
      IF (I .GT. P0 .OR. I .LT. P0-3) THEN
         WRITE(NW,*) 'INVALID P = ', I, ' -- P REMAINS ', P
       ELSE
         P = I
         MDL(2) = I
         END IF
      GO TO 30
 260  READ(NIN,*,END=210) I, T
      IF (I .LE. 0) GO TO 30
      X0(I) = T
      GO TO 260
 270  DO 280 I = 1, N
 280     READ(NIN, FMT, END=290) YN(1,I), YN(2,I)
      GO TO 30
 290  WRITE(NW,*) 'Premature end of file!'
      GO TO 210
 300  I = 1
      IF (MDL(6) .EQ. 1) I = 2
      GO TO 330
 310  READ(NIN,*,END=210) KDIAG
      GO TO 30
 320  READ(NIN,*,END=210) I
      I =  MIN(4, MAX0(I,1))
 330  WRITE(NW,*) 'KW changed from ', MDL(6), ' = ', WNAME(MDL(6)),
     1 ' to ', I, ' = ', WNAME(I)
      MDL(6) = I
      GO TO 30
 340  READ(NIN,*,END=210) I, T, T1
      IF (I .LE. 0) GO TO 30
      B(1,I) = T
      B(2,I) = T1
      GO TO 340
 350  READ(NIN,*,END=210) I, J
      IF (I .LE. 0) GO TO 30
      RHOI(I) = J
      GO TO 350
 360  READ(NIN,*,END=210) I
      IF (I .LE. 0) GO TO 30
      WRITE(*,*) 'RHOI(',I,') = ', RHOI(I)
      GO TO 360
 370  READ(NIN,*,END=210) I, T
      IF (I .LE. 0) GO TO 30
      RHOR(I) = T
      GO TO 370
 380  READ(NIN,*,END=210) I
      IF (I .LE. 0) GO TO 30
      WRITE(*,*) 'RHOR(',I,') = ', RHOR(I)
      GO TO 380
 390  READ(NIN,*,END=210) I, J
      IF (I .LE. 0) GO TO 30
      WRITE(*,*) (RHOR(K), K = I, J)
      GO TO 390
 410  WRITE(*,420) FNAME
 420  FORMAT(' Can''t open ',A)
      GO TO 30
 430  READ(NIN,*,END=210) I
      IF (I .LE. 0) GO TO 30
      WRITE(*,*) 'IV(',I,') = ', IV(I)
      GO TO 430
 440  READ(NIN,*,END=210) I
      IF (I .LE. 0) GO TO 30
      WRITE(*,*) 'V(',I,') = ', V(I)
      GO TO 440
 450  READ(NIN,'(A)',END=200) FNAME
      WRITE(NW,*) FNAME
      GO TO 30
      END
      SUBROUTINE BRJ(N, P, X, NF, NEED, R, RP, UI, A, UF)
      INTEGER N, P, NF, NEED(2), UI(5)
      REAL X(P), R(N), RP(P,N), A(*)
      EXTERNAL UF
      EXTERNAL BRJ1
      INTEGER M
C
C *** BODY ***
C
      M = UI(6)
      CALL BRJ1(M, N, UI(7), X, NF, NEED, R, RP, UI, A, A(M*N+1), UF)
 999  RETURN
      END
      SUBROUTINE BRJ1(M, N, P, X, NF, NEED, R, RP, UI, A, UR, UF)
      INTEGER M, N, P, NF, NEED(2), UI(5)
      REAL X(P), R(N), RP(P,N), A(M,N), UR(N,6)
      EXTERNAL UF
      EXTERNAL  D7TPR,  R7MDC
      REAL  D7TPR,  R7MDC
C
C *** LOCAL VARIABLES ***
C
      INTEGER I, J, J2, J4, MODEL
      REAL ALPHA, BETA1, BETA2, DI, E, EMX, PHI, T, T1,
     1                 THETA, TI, X1, X1INV, X2, X3, X3M1, X4
      REAL EXPMAX, EXPMIN, ONE, TWO, ZERO
      DATA EXPMAX/0.E+0/, EXPMIN/0.E+0/, ONE/1.E+0/, TWO/2.E+0/,
     1     ZERO/0.E+0/
C
C *** BODY ***
C
      MODEL = IABS(UI(2))
      IF (MODEL .LE. 0) GO TO 520
      IF (MODEL .GT. 11) GO TO 520
      IF (EXPMAX .GT. ZERO) GO TO 10
         EXPMAX = TWO * ALOG( R7MDC(5))
         EXPMIN = TWO * ALOG( R7MDC(2))
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
 30      R(I) =  D7TPR(P, X, A(1,I))
      GO TO 999
C
C *** EXPONENTIAL OF LINEAR ***
C
 40   DO 50 I = 1, N
         T =  D7TPR(P, X, A(1,I))
         IF (T .GE. EXPMAX) GO TO 520
         E = ZERO
         IF (T .GT. EXPMIN) E =  EXP(T)
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
         E =  EXP(-X2*A(2,I))
         UR(I,J2) = E
         T = (ONE - E) ** X3
         UR(I,J4) = T
         T = X1*A(1,I) * (ONE - T)
         IF (T .LE. ZERO) GO TO 520
         UR(I,J) = T
         IF (MODEL .EQ. 3) T = ALOG(T)
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
         IF (T .GT. EXPMIN) E =  EXP(T)
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
      EMX = EXPMAX - 10.E+0
      DO 110 I = 1, N
         T1 = X1 * A(1,I)
         T = X2 + X3*A(2,I) + T1
         IF (T .GE. EMX) GO TO 520
         E = ZERO
         IF (T .GT. EXPMIN) E =  EXP(T)
         T = X4 + T1
         IF (T .GE. EMX) GO TO 520
         T1 = ZERO
         IF (T .GT. EXPMIN) T1 =  EXP(T)
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
         T =  D7TPR(P, A(1,I), X)
         IF (T .LE. EXPMIN) GO TO 130
         IF (T .GE. EXPMAX) GO TO 140
         E =  EXP(T)
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
         T =  D7TPR(P, X, A(1,I))
         IF (T .LE. ZERO) GO TO 520
         R(I) = ALOG(T)
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
         R(I) = ALPHA + BETA1*ALOG(A(1,I)) + BETA2*X2/(PHI + X2)
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
         R(I) = ALPHA + BETA1*ALOG(T) + BETA2*X2/(PHI + X2)
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
 280  CALL  V7CPY(N*P, RP, A)
      GO TO 999
C
C *** EXPONENTIAL OF LINEAR MODEL ***
C
 290  DO 300 I = 1, N
 300     CALL  V7SCL(P, RP(1,I), UR(I,J), A(1,I))
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
         RP(3,I) = T * UR(I,J4) * ALOG(T1)
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
         RP(3,I) = T * UR(I,J4) * ALOG(T1)
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
 420     CALL  V7SCL(P, RP(1,I), UR(I,J), A(1,I))
      GO TO 999
C
C *** LOG OF LINEAR ***
C
 430  DO 440 I = 1, N
 440     CALL  V7SCL(P, RP(1,I), ONE/UR(I,J), A(1,I))
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
C        R(1,I) = ALPHA + BETA1*ALOG(A(1,I)) + BETA2*X2/(PHI + X2)
         RP(1,I) = ONE
         RP(2,I) = ALOG(A(1,I))
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
C        R(I) = ALPHA + BETA1*ALOG(A(1,I) - THETA) + BETA2*X2/(PHI + X2)
         RP(1,I) = ONE
         RP(2,I) = ALOG(A(1,I) - THETA)
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
      SUBROUTINE CHKDER(MDL, N, NPT, PT, R, RHO, RHO0, YN)
      INTEGER MDL(1), N, NPT
C     REAL PT(NPT) -- BUT NPT MAY BE 0
      REAL PT(1), R(N,20), YN(2,N)
      EXTERNAL RHO, RHO0
      EXTERNAL  V2NRM
      REAL  V2NRM
      INTEGER I, J
      REAL F, H, T
      REAL FOO(10), FAC
      DATA FOO/.1, -.1, .2, -.2, .4, -.4, .6, -.6, .8, -.9/, H/.001E0/
C
C *** BODY ***
C
      J = 1
      FAC = 1.0
      DO 10 I = 1, N
         T = FAC * FOO(J)
         R(I,1) = T
         R(I,10) = T + H
         J = J + 1
         IF (J .LE. 10) GO TO 10
                J = 1
                FAC = 10. * FAC
 10      CONTINUE
      CALL RHO0(MDL, N, PT, R, R(1,4), YN)
      CALL RHO0(MDL, N, PT, R(1,10), R(1,13), YN)
      DO 20 I = 1, N
         T = R(I,10) - R(I,1)
         IF (T .NE. 0.E0) T = 1.E0 / T
         R(I,20) = T
 20      CONTINUE
      CALL  V2AXY(N, R(1,13), -1.E0, R(1,4), R(1,13))
      CALL  V7VMP(N, R(1,13), R(1,13), R(1,20), 1)
      J = 1
      CALL RHO(0, F, N, J, PT, R, R(1,4), MDL, YN)
      CALL RHO(1, F, N, J, PT, R, R(1,4), MDL, YN)
      CALL  V2AXY(N, R(1,19), -1.E0, R(1,13), R)
      T =  V2NRM(N,R(1,19))/( V2NRM(N,R(1,13)) +  V2NRM(N,R))
      WRITE(6,*) '1ST DERIV RELATIVE DIFFERENCE =', T
      IF (T .GT. .01) THEN
        WRITE(6,*) 'I   FD(I)   AN(I)'
        WRITE(6,'(I5,2G13.4)') (I, R(I,13), R(I,1), I = 1, N)
        END IF
      CALL RHO(0, F, N, J, PT, R(1,10), R(1,13), MDL, YN)
      CALL RHO(1, F, N, J, PT, R(1,10), R(1,13), MDL, YN)
      CALL  V2AXY(N, R(1,19), -1.E0, R, R(1,10))
      CALL  V7VMP(N, R(1,19), R(1,19), R(1,20), 1)
      CALL  V2AXY(N, R(1,13), -1.E0, R(1,19), R(1,4))
      T =  V2NRM(N,R(1,13))/( V2NRM(N,R(1,4)) +  V2NRM(N,R(1,19)))
      WRITE(6,*) '2ND DERIV RELATIVE DIFFERENCE =', T
      IF (T .GT. .01) THEN
        WRITE(6,*) 'I   FD(I)   AN(I)'
        WRITE(6,'(I5,2G13.4)') (I, R(I,19), R(I,4), I = 1, N)
        END IF
 999  RETURN
      END
      SUBROUTINE RPOIL0(MDL, N, PT, R, RHO, YN)
      INTEGER N, MDL(1)
      REAL PT(1), R(N), RHO(N), YN(2,N)
      EXTERNAL LPN,  R7MDC
      REAL LPN,  R7MDC
      INTEGER I, MODEL
      REAL E, RI, T, YI
      REAL  EXP, ALOG
      REAL EXPMAX, EXPMIN, HALF, ONE, TWO, ZERO
      DATA EXPMAX/0.E+0/, EXPMIN/0.E+0/,
     1     HALF/0.5E+0/, ONE/1.E+0/, TWO/2.E+0/, ZERO/0.E+0/
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
         EXPMAX = TWO * ALOG( R7MDC(5))
         EXPMIN = TWO * ALOG( R7MDC(2))
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
         RHO(I) = YN(2,I)*RI - YN(1,I)*ALOG(RI)
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
         RHO(I) = YN(2,I)*RI**2 - TWO*YN(1,1)*ALOG(RI)
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
         RHO(I) = -YN(1,I)*ALOG(RI) - (YN(2,I) - YN(1,I))*ALOG(ONE-RI)
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
         IF (RI .GT. EXPMIN) E =  EXP(RI)
         RHO(I) = YN(2,I)*ALOG(ONE + E) - YN(1,I)*RI
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
         IF (RI .GT. EXPMIN) E =  EXP(RI)
         T = ZERO
         IF (-E .GT. EXPMIN) T =  EXP(-E)
         RHO(I) = (YN(2,I) - YN(1,I))*E - YN(1,I)*ALOG(ONE - T)
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
         RHO(I) = YN(2,I) * (YN(1,I)*RI - ALOG(RI))
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
      SUBROUTINE DEVIAN(F, MODEL0, N, NW, PT, YN)
      INTEGER MODEL0, N, NW
      REAL F, PT(2), YN(2,N)
      REAL  ATAN, ALOG
      INTEGER I, MODEL
      REAL CI, D, S, T, T1, YI
      REAL EIGHT, HALF, ONE, TWO, ZERO
      DATA EIGHT/8.E+0/, HALF/0.5E+0/, ONE/1.E+0/, TWO/2.E+0/,
     1     ZERO/0.E+0/
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
         IF (T .GT. ZERO) D = D + YI*ALOG(T)
         IF (T .LT. ONE) D = D + (CI-YI)*ALOG(ONE-T)
 10      CONTINUE
      GO TO 100
C
C *** POISSON DEVIANCE ***
C
 20   DO 30 I = 1, N
         YI = YN(1,I)
         IF (YI .GT. ZERO) D = D + YI*(ALOG(YI/YN(2,I)) - ONE)
 30      CONTINUE
      GO TO 100
C
C *** GAMMA DEVIANCE ***
C
 40   DO 50 I = 1, N
         YI = YN(1,I)
         IF (YI .LE. ZERO) GO TO 999
         D = D - YN(2,I)*(ONE + ALOG(YI))
 50      CONTINUE
      GO TO 100
C
C  *** PREGIBON DEVIANCE, REPLICATE WEIGHTS ***
C
 60   T = PT(2)
      T1 = ALOG(EIGHT* ATAN(ONE)*PT(1))
      S = ZERO
      DO 70 I = 1, N
 70      S = S + YN(2,I) * (T*ALOG(REAL(YN(1,I))) + T1)
      D = PT(1) * (D - HALF*S)
      GO TO 100
C
C  *** PREGIBON DEVIANCE, VARIANCE WEIGHTS ***
C
 80   S = ZERO
      T = ZERO
      DO 90 I = 1, N
         S = S + ALOG(REAL(YN(1,I)))
         T = T + ALOG(REAL(YN(2,I)))
 90      CONTINUE
      D = PT(1) * (D -
     1     HALF*(PT(2)*S - T + N*ALOG(EIGHT* ATAN(ONE)*PT(1))))
C
 100  WRITE(NW,*) 'DEVIANCE = ', TWO*D
 999  RETURN
      END
      REAL FUNCTION DZERO(F,A,B,T)
C *** THE PORT ROUTINE, MODIFIED TO STOP RATHER THAN CALLING SETERR ***
C *** AND TO CALL  R7MDC RATHER THAN D1MACH ***
C
C  FINDS THE REAL ROOT OF THE FUNCTION F LYING BETWEEN A AND B
C  TO WITHIN A TOLERANCE OF
C
C         6*D1MACH(3) *  ABS(DZERO) + 2 * T
C
C  F(A) AND F(B) MUST HAVE OPPOSITE SIGNS
C
C  THIS IS BRENTS ALGORITHM
C
C  A, STORED IN SA, IS THE PREVIOUS BEST APPROXIMATION (I.E. THE OLD B)
C  B, STORED IN SB, IS THE CURRENT BEST APPROXIMATION
C  C IS THE MOST RECENTLY COMPUTED POINT SATISFYING F(B)*F(C) .LT. 0
C  D CONTAINS THE CORRECTION TO THE APPROXIMATION
C  E CONTAINS THE PREVIOUS VALUE OF D
C  M CONTAINS THE BISECTION QUANTITY (C-B)/2
C
      REAL F,A,B,T,TT,SA,SB,C,D,E,FA,FB,FC,TOL,M,P,Q,R,S
      EXTERNAL F
      REAL  R7MDC
C
      TT = T
      IF (T .LE. 0.0E0) TT = 10.E0* R7MDC(1)
C
      SA = A
      SB = B
      FA = F(SA)
      FB = F(SB)
      IF (FA .NE. 0.0E0) GO TO 5
      DZERO = SA
      RETURN
  5   IF (FB .EQ. 0.0E0) GO TO 140
        IF ( SIGN(FA,FB) .EQ. FA) THEN
                WRITE(*,*) 'DZERO: F(A) = ', FA, '; F(B) = ', FB
                STOP
                END IF
C
 10   C  = SA
      FC = FA
      E  = SB-SA
      D  = E
C
C  INTERCHANGE B AND C IF  ABS F(C) .LT.  ABS F(B)
C
 20   IF ( ABS(FC).GE. ABS(FB)) GO TO 30
      SA = SB
      SB = C
      C  = SA
      FA = FB
      FB = FC
      FC = FA
C
 30   TOL = 2.0E0* R7MDC(3)* ABS(SB)+TT
      M = 0.5E0*(C-SB)
C
C  SUCCESS INDICATED BY M REDUCES TO UNDER TOLERANCE OR
C  BY F(B) = 0
C
      IF (( ABS(M).LE.TOL).OR.(FB.EQ.0.0E0)) GO TO 140
C
C  A BISECTION IS FORCED IF E, THE NEXT-TO-LAST CORRECTION
C  WAS LESS THAN THE TOLERANCE OR IF THE PREVIOUS B GAVE
C  A SMALLER F(B).  OTHERWISE GO TO 40.
C
      IF (( ABS(E).GE.TOL).AND.( ABS(FA).GE. ABS(FB))) GO TO 40
      E = M
      D = E
      GO TO 100
 40   S = FB/FA
C
C  QUADRATIC INTERPOLATION CAN ONLY BE DONE IF A (IN SA)
C  AND C ARE DIFFERENT POINTS.
C  OTHERWISE DO THE FOLLOWING LINEAR INTERPOLATION
C
      IF (SA.NE.C) GO TO 50
      P = 2.0E0*M*S
      Q = 1.0E0-S
      GO TO 60
C
C  INVERSE QUADRATIC INTERPOLATION
C
 50   Q = FA/FC
      R = FB/FC
      P = S*(2.0E0*M*Q*(Q-R)-(SB-SA)*(R-1.0E0))
      Q = (Q-1.0E0)*(R-1.0E0)*(S-1.0E0)
 60   IF (P.LE.0.0E0) GO TO 70
      Q = -Q
      GO TO 80
 70   P = -P
C
C  UPDATE THE QUANTITIES USING THE NEWLY COMPUTED
C  INTERPOLATE UNLESS IT WOULD EITHER FORCE THE
C  NEW POINT TOO FAR TO ONE SIDE OF THE INTERVAL
C  OR WOULD REPRESENT A CORRECTION GREATER THAN
C  HALF THE PREVIOUS CORRECTION.
C
C  IN THESE LAST TWO CASES - DO THE BISECTION
C  BELOW (FROM STATEMENT 90 TO 100)
C
 80   S = E
      E = D
      IF ((2.0E0*P.GE.3.0E0*M*Q- ABS(TOL*Q)).OR.
     1    (P.GE. ABS(0.5E0*S*Q))) GO TO 90
      D = P/Q
      GO TO 100
 90   E = M
      D = E
C
C  SET A TO THE PREVIOUS B
C
 100  SA = SB
      FA = FB
C
C  IF THE CORRECTION TO BE MADE IS SMALLER THAN
C  THE TOLERANCE, JUST TAKE A  DELTA STEP  (DELTA=TOLERANCE)
C         B = B + DELTA * SIGN(M)
C
      IF ( ABS(D).LE.TOL) GO TO 110
      SB = SB+D
      GO TO 130
C
 110  IF (M.LE.0.0E0) GO TO 120
      SB = SB+TOL
      GO TO 130
C
 120  SB = SB-TOL
 130  FB = F(SB)
C
C  IF F(B) AND F(C) HAVE THE SAME SIGN ONLY
C  LINEAR INTERPOLATION (NOT INVERSE QUADRATIC)
C  CAN BE DONE
C
      IF ((FB.GT.0.0E0).AND.(FC.GT.0.0E0)) GO TO 10
      IF ((FB.LE.0.0E0).AND.(FC.LE.0.0E0)) GO TO 10
      GO TO 20
C
C***SUCCESS***
 140  DZERO = SB
      RETURN
      END
        REAL FUNCTION INVCN(X, ERRFLG)
        REAL X
        INTEGER ERRFLG
        COMMON /INVCMN/ XC, TOL, NCALL
        REAL XC, TOL
        INTEGER NCALL

        REAL CNERR, DZERO, PNORMS,  R7MDC
        EXTERNAL CNERR, PNORMS,  R7MDC

        REAL A, B
        REAL HALF, ONE, ZERO
        LOGICAL FIRST
        REAL HUGE
        PARAMETER (HALF = 0.5E+0, ONE = 1.E+0, ZERO = 0.E+0)
        SAVE FIRST, HUGE
        DATA FIRST/.TRUE./, HUGE/0.E+0/

        IF (FIRST) THEN
                TOL = 10.E+0 *  R7MDC(1)
                HUGE = 0.1E+0 *  R7MDC(6)
                FIRST = .FALSE.
                END IF

        NCALL = 0
        ERRFLG = 0
        IF (X .LE. ZERO) THEN
C               IF (X .EQ. ZERO) THEN
C                       INVCN = -HUGE
C                       GO TO 999
C                       END IF
                ERRFLG = 1
                INVCN = ZERO
                GO TO 999
                END IF
        IF (X .GE. ONE) THEN
C               IF (X .EQ. ONE) THEN
C                       INVCN = HUGE
C                       GO TO 999
C                       END IF
                ERRFLG = 1
                INVCN = ZERO
                GO TO 999
                END IF
        IF (X .GE. HALF) THEN
                A = ZERO
                B = ONE
 10             IF (PNORMS(B) .LT. X) THEN
                        B = B + ONE
                        GO TO 10
                        END IF
        ELSE
                B = ZERO
                A = -ONE
 20             IF (PNORMS(A) .GT. X) THEN
                        A = A - ONE
                        GO TO 20
                        END IF
                END IF
        XC = X
        INVCN = DZERO(CNERR,A,B,TOL)
 999    RETURN
        END

        REAL FUNCTION CNERR(X)
        REAL X

        COMMON /INVCMN/ XC, TOL, NCALL
        REAL XC, TOL
        INTEGER NCALL

        REAL PNORMS
        EXTERNAL PNORMS
        NCALL = NCALL + 1
        CNERR = XC - PNORMS(X)
        END
      SUBROUTINE LOUCHK(KDIAG,  GLG, X0, N, P, PS, X, RHPOIL, MDL, YN,
     1                  IV, LIV, LV, V, BRJ, UI, A, BRJ1)
      EXTERNAL  GLG, RHPOIL, BRJ, BRJ1
      INTEGER KDIAG, N, P, PS, LIV, LV
      INTEGER IV(LIV), MDL(2), UI(*)
      REAL X0(P), X(P), V(LV), A(*), YN(N)
C
C *** DUMMY REPLACEMENT FOR C ROUTINE (USED FOR DEBUGGING) ***
C
      END
      REAL FUNCTION PNORMS(X)
      REAL X

      EXTERNAL MECDF
      REAL D(1), PROB, RHO(1)
      INTEGER IER

      D(1) = X
      CALL MECDF(1, D, RHO, PROB, IER)
      PNORMS = 1.E+0 - PROB
      END
      SUBROUTINE POISX0(A, C, LA, LC, MODEL, N, P, QTR, X, YN)
      INTEGER LA, LC, MODEL, N, P
      REAL A(LA,N), C(LC), QTR(P), X(P), YN(2,N)
      EXTERNAL  L7ITV,  L7SVX,  L7SVN, Q7ADR,  R7MDC,  V7SCL,  V7SCP
      REAL  L7SVX,  L7SVN,  R7MDC
      INTEGER I
      REAL SX, W, WRT, WY, YN1
      REAL HALF, ONE, ZERO
      DATA HALF/0.5E+0/, ONE/1.E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
      CALL  V7SCP(LC, C, ZERO)
      CALL  V7SCP(P, QTR, ZERO)
      DO 30 I = 1, N
         W = YN(2,I)
         IF (W .LE. ZERO) GO TO 40
         WRT =  SQRT(W)
         YN1 = YN(1,I) / YN(2,I)
         IF (MODEL .EQ. 2) GO TO 10
            WY = WRT * YN1
            GO TO 20
 10      WY = WRT * ALOG(  MAX(YN1, HALF/W))
 20      CALL  V7SCL(P, X, WRT, A(1,I))
         CALL  Q7ADR(P, QTR, C, X, WY)
 30      CONTINUE
      SX =  L7SVX(P, C, X, X)
      IF (SX .LE. ZERO) GO TO 40
      IF ( L7SVN(P, C, X, X)/SX .LE.  R7MDC(3)) GO TO 40
      CALL  L7ITV(P, X, C, QTR)
      GO TO 999
 40   W = ONE
      IF (MODEL .EQ. 2) W = ZERO
      CALL  V7SCP(P, X, W)
C
 999  RETURN
      END
      SUBROUTINE POIX0(A, IV, LA, LIV, LV, MODEL, N, P, V, X, YN)
C
C *** COMPUTE INITIAL X OF E. L. FROME ***
C
      INTEGER LA, LIV, LV, MODEL, N, P
      INTEGER IV(LIV)
      REAL X(P), A(LA,N), V(LV), YN(2,N)
C
      EXTERNAL  IVSET, POISX0,  V7SCP
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER C1, PP1O2, QTR1, TEMP1
      REAL ONE, ZERO
C
C  ***  IV COMPONENTS  ***
C
      INTEGER LMAT
      PARAMETER (LMAT=42)
      DATA ONE/1.E+0/, ZERO/0.E+0/
C
C---------------------------------  BODY  ------------------------------
C
      IF (IV(1) .EQ. 0) CALL  IVSET(1, IV, LIV, LV, V)
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
      CALL  V7SCP(P, X, ONE)
      GO TO 999
 20   CALL  V7SCP(P, X, ZERO)
C
 999   RETURN
       END
      SUBROUTINE PREGRH(DERIV, F, N, NF, PT, R, RD, RHOI, YLOG, YN, ZN)
C
C  ***  RHO FOR PREGIBON ERROR MODELS WITH REPLICATE WEIGHTS ***
C  ***  SEE PREGRV FOR THE RIGHT WEIGHTING FOR THE INSURANCE EXAMPLE ***
C
      INTEGER DERIV, N, NF, RHOI(*)
      REAL F, PT(3), R(*), RD(*), YLOG(*), YN(2,N), ZN(3,N)
      EXTERNAL  R7MDC
      REAL  R7MDC
C
C *** LOCAL VARIABLES ***
C
      INTEGER I, K, KMP, KMPS, KMT, KPP, KPPS, KPSPS, KPT, KTT, KTPS
      REAL F1, MU, PHI, PHII2, PHII3, PHIINV, PSI, PSPHII,
     1                 RI, RL, RP0, RPP0, RT1, RT1L, RT2, RT2L, RTOL, T,
     2                 T1, T1INV, T1INV2, T2, T2INV, T2INV2, THETA, TT,
     3                 WI, WOVPHI, YI, YL, YT1, YT1L, YT2, YT2L
C
      REAL BIG, BIGH, TWOPI
      REAL BTOL, EIGHT, HALF, ONE, THREE, TWO, ZERO
      DATA BIG/0.E+0/, BIGH/0.E+0/, TWOPI/0.E+0/
      DATA BTOL/1.01E+0/, EIGHT/8.E+0/, HALF/0.5E+0/, ONE/1.E+0/,
     1     THREE/3.E+0/, TWO/2.E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
      IF (NF .GT. 1) GO TO 20
      IF (DERIV .GT. 0) GO TO 20
      DO 10 I = 1, N
 10      YLOG(I) = ALOG(YN(1,I))
 20   PHI = PT(1)
      PSI = PT(3)
      IF (PHI .LE. ZERO) GO TO 240
      THETA = PT(2)
      IF (TWOPI .GT. ZERO) GO TO 30
         TWOPI = EIGHT *  ATAN(ONE)
         BIGH =  R7MDC(5)
         BIG =  R7MDC(6)
 30   T2 = TWO - THETA
      T1 = ONE - THETA
      IF (DERIV .GT. 0) GO TO 120
      RTOL = BIG
      IF (T2 .LT. BTOL) GO TO 40
         RTOL = BIGH**(ONE/T2)
         RTOL = RTOL*RTOL
 40   T = ALOG(TWOPI * PHI)
      F = ZERO
      DO 50 I = 1, N
 50      F = F + YN(2,I)*(T + THETA*YLOG(I))
      F1 = ZERO
      IF (THETA .EQ. ONE) GO TO 70
      IF (THETA .EQ. TWO) GO TO 90
      T1INV = ONE / T1
      T2INV = ONE / T2
      DO 60 I = 1, N
         RI = R(I)
         IF (RI .GE. RTOL) GO TO 240
         IF (RI .LE. ZERO) GO TO 240
         YI = YN(1,I)
         RT1 = RI**(T1*PSI)
         ZN(2,I) = RT1
         YT1 = YI**T1
         ZN(3,I) = YT1
         T = T2INV*(RI**(T2*PSI) - YI*YT1) + YI*T1INV*(YT1 - RT1)
         F1 = F1 + T*YN(2,I)
         ZN(1,I) = T
 60      CONTINUE
      GO TO 110
C
C *** THETA == 1 ***
C
 70   DO 80 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 240
         MU = RI**PSI
         YI = YN(1,I)
         T = MU - YI - YI*ALOG(MU/YI)
         F1 = F1 + T*YN(2,I)
         ZN(1,I) = T
         ZN(2,I) = ONE
 80      CONTINUE
      GO TO 110
C
C *** THETA == 2 ***
C
 90   DO 100 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 240
         T1 = RI**(-PSI)
         YI = YN(1,I) * T1
         T = YI - ALOG(YI) - ONE
         F1 = F1 + T*YN(2,I)
         ZN(1,I) = T
         ZN(2,I) = T1
 100     CONTINUE
 110  F = HALF*F + F1/PHI
      GO TO 999
C
C  ***  GRADIENT COMPUTATIONS  ***
C
 120  PHIINV = ONE / PHI
      PHII2 = PHIINV * PHIINV
      RP0 = HALF * PHIINV
      RPP0 = -PHIINV * RP0
      PHII3 = TWO * PHIINV * PHII2
      KMP = N
      KPP = N + N
      T1 = ONE - THETA
      T2 = TWO - THETA
      IF (RHOI(2) .LE. RHOI(3)+2) GO TO 140
C
C  *** PSI DERIVATIVES ***
C
      K = KPP + N
      KMPS = 6*N
      KPPS = KMPS + N
      KTPS = KPPS + N
      KPSPS = KTPS + N
      DO 130 I = 1, N
         WI = YN(2,I)
         RI = R(I)
         MU = RI**PSI
         RL = ALOG(RI)
         RT1 = WI * ZN(2,I)
         RT2 = RT1 * MU
         YI = YN(1,I)
         T = (RL/PHI) * (RT2 - YI*RT1)
         K = K + 1
         R(K) = T
         KMPS = KMPS + 1
         TT = RL * (T2*RT2 - YI*T1*RT1)
         RD(KMPS) = (RT2 - YI*RT1 + PSI*TT) / (RI*PHI)
         KPPS = KPPS + 1
         RD(KPPS) = -T / PHI
         KTPS = KTPS + 1
         RD(KTPS) = -PSI * RL * T
         KPSPS = KPSPS + 1
         RD(KPSPS) = TT * RL / PHI
 130     CONTINUE
C
 140  IF (RHOI(2) .LE. RHOI(3)) GO TO 220
      IF (RHOI(2) .EQ. RHOI(3)+1) GO TO 200
C
C  *** THETA DERIVATIVES ***
C
      K = KPP
      KMT = K + N
      KPT = KMT + N
      KTT = KPT + N
      IF (THETA .EQ. ONE) GO TO 160
      IF (THETA .EQ. TWO) GO TO 180
      T1INV = ONE / T1
      T1INV2 = T1INV + T1INV
      T2INV = ONE / T2
      T2INV2 = T2INV + T2INV
      DO 150 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PHIINV
         RI = R(I)
         MU = RI**PSI
         RT1 = ZN(2,I)
         RT2 = RT1 * MU
         RL = ALOG(MU)
         RT1L = RT1 * RL
         RT2L = RT2 * RL
         YI = YN(1,I)
         YT1 = ZN(3,I)
         YT2 = YT1 * YI
         YL = YLOG(I)
         YT1L = YT1 * YL
         YT2L = YT2 * YL
         T = PHIINV * (YI * T1INV * (RL*RT1 - YL*YT1 +
     1                          T1INV*(YT1 - RT1))
     2                  + T2INV * (YL*YT2 - RL*RT2 +
     3                          T2INV*(RT2 - YT2)))
         K = K + 1
         R(K) = WI * (HALF*YL + T)
         KMT = KMT + 1
         RD(KMT) = PSI * WOVPHI * RL * (YI*RT1 - RT2) / RI
         KPT = KPT + 1
         RD(KPT) = -WOVPHI * T
         KTT = KTT + 1
         RD(KTT) = WOVPHI*(T1INV*YI*(YT1L*YL - RT1L*RL +
     1                       T1INV2*(RT1L - YT1L +
     2                        T1INV*(YT1 - RT1))) +
     3                        T2INV*(RT2L*RL - YT2L*YL +
     4                       T2INV2*(YT2L - RT2L +
     5                        T2INV*(RT2 - YT2))))
 150     CONTINUE
      GO TO 200
C
C *** THETA DERIVATIVES AT THETA == 1 ***
C
 160  DO 170 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PHIINV
         YI = YN(1,I)
         YL = YLOG(I)
         RI = R(I)
         MU = RI**PSI
         RL = ALOG(MU)
         K = K + 1
         T = HALF*YI*(RL*RL - YL*YL) + YI*YL - MU*RL + MU - YI
         R(K) =  WI*(HALF*YL + T)
         KMT = KMT + 1
         RD(KMT) = PSI * WOVPHI * RL * (YI - MU) / RI
         KPT = KPT + 1
         RD(KPT) = -WOVPHI * T
         KTT = KTT + 1
         T = RL * RL
         RD(KTT) = WOVPHI * ( MU * (TWO - TWO*RL + T)
     1          -YI*(TWO - T*RL/THREE + YL*(YL - TWO + YL*YL/THREE)))
 170     CONTINUE
      GO TO 200
C
C *** THETA DERIVATIVES AT THETA == 2 ***
C
 180  DO 190 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PHIINV
         YI = YN(1,I)
         YL = YLOG(I)
         RI = R(I)
         MU = RI**PSI
         RL = ALOG(MU)
         K = K + 1
         T = HALF*(YL*YL - RL*RL) + YL + ONE - (YI + YI*RL)/MU
         R(K) =  WI*(HALF*YL + T)
         KMT = KMT + 1
         RD(KMT) = PSI * WOVPHI * RL * (YI/MU - ONE) / RI
         KPT = KPT + 1
         RD(KPT) = -WOVPHI * T
         KTT = KTT + 1
         T = RL * RL
         RD(KTT) = WOVPHI * ((YL/MU)*(T + TWO*RL + TWO) - TWO
     1                  - YL*(TWO + YL*(ONE + YL/THREE)) + T*RL/THREE)
 190     CONTINUE
C
C *** PHI AND MU DERIVATIVES ***
C
 200  K = N
      THETA = ONE - PSI*T1
      T1 = PSI*T2 - ONE
      PSPHII = PSI * PHIINV
      PHIINV = -PHIINV
      DO 210 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PSPHII
         RI = R(I)
         MU = RI**PSI
         YI = YN(1,I)
         RT1 = ZN(2,I)/RI
         T2 = WOVPHI * RT1 * (MU - YI)
         R(I) = T2
         RD(I) = WOVPHI * RT1 * (T1*MU + YI*THETA) / RI
         T = ZN(1,I)
         K = K + 1
         R(K) = WI * (RP0 - PHII2*T)
         KMP = KMP + 1
         RD(KMP) = PHIINV * T2
         KPP = KPP + 1
         RD(KPP) = WI * (RPP0 + PHII3*T)
 210     CONTINUE
      GO TO 999
C
C *** JUST MU DERIVATIVES ***
C
 220  PHIINV = PHIINV * PSI
      THETA = ONE - PSI*T1
      T1 = PSI*T2 - ONE
      DO 230 I = 1, N
         WOVPHI = YN(2,I) * PHIINV
         RI = R(I)
         MU = RI**PSI
         YI = YN(1,I)
         RT1 = ZN(2,I)/RI
         R(I) = WOVPHI * RT1 * (MU - YI)
         RD(I) = WOVPHI * RT1 * (T1*MU + YI*THETA) / RI
 230     CONTINUE
      GO TO 999
C
 240  NF = 0
C
 999  RETURN
      END
      SUBROUTINE PREGRV(DERIV, F, N, NF, PT, R, RD, RHOI, YLOG, YN, ZN)
C
C  ***  RHO FOR PREGIBON ERROR MODELS WITH VARIANCE WEIGHTS ***
C
      INTEGER DERIV, N, NF, RHOI(*)
      REAL F, PT(3), R(*), RD(*), YLOG(N+2),YN(2,N),ZN(3,N)
      EXTERNAL  R7MDC
      REAL  R7MDC
C
C *** LOCAL VARIABLES ***
C
      INTEGER I, K, KMP, KMPS, KMT, KPP, KPPS, KPSPS, KPT, KTT, KTPS
      REAL F1, MU, PHI, PHII2, PHII3, PHIINV, PSI, PSPHII,
     1                 RI, RL, RP0, RPP0, RT1, RT1L, RT2, RT2L, RTOL, T,
     2                 T1, T1INV, T1INV2, T2, T2INV, T2INV2, THETA, TT,
     3                 WI, WOVPHI, YI, YL, YT1, YT1L, YT2, YT2L
C
      REAL BIG, BIGH, TWOPI
      REAL BTOL, EIGHT, HALF, ONE, THREE, TWO, ZERO
      DATA BIG/0.E+0/, BIGH/0.E+0/, TWOPI/0.E+0/
      DATA BTOL/1.01E+0/, EIGHT/8.E+0/, HALF/0.5E+0/, ONE/1.E+0/,
     1     THREE/3.E+0/, TWO/2.E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
      PHI = PT(1)
      IF (PHI .LE. ZERO) GO TO 230
      IF (TWOPI .GT. ZERO) GO TO 10
         TWOPI = EIGHT *  ATAN(ONE)
         BIGH =  R7MDC(5)
         BIG =  R7MDC(6)
 10   IF (NF .GT. 1) GO TO 30
      IF (DERIV .GT. 0) GO TO 30
      T1 = ZERO
      T2 = ZERO
      DO 20 I = 1, N
         T = ALOG(YN(1,I))
         YLOG(I) = T
         T1 = T1 + T
         T2 = T2 + ALOG(YN(2,I))
 20      CONTINUE
      YLOG(N+1) = T1
      YLOG(N+2) = -T2
 30   PSI = PT(3)
      THETA = PT(2)
      T2 = TWO - THETA
      T1 = ONE - THETA
      IF (DERIV .GT. 0) GO TO 110
      RTOL = BIG
      IF (T2 .LT. BTOL) GO TO 40
         RTOL = BIGH**(ONE/T2)
         RTOL = RTOL*RTOL
 40   F = N*ALOG(TWOPI*PHI) + YLOG(N+2) + THETA*YLOG(N+1)
      F1 = ZERO
      IF (THETA .EQ. ONE) GO TO 60
      IF (THETA .EQ. TWO) GO TO 80
      T1INV = ONE / T1
      T2INV = ONE / T2
      DO 50 I = 1, N
         RI = R(I)
         IF (RI .GE. RTOL) GO TO 230
         IF (RI .LE. ZERO) GO TO 230
         YI = YN(1,I)
         RT1 = RI**(T1*PSI)
         ZN(2,I) = RT1
         YT1 = YI**T1
         ZN(3,I) = YT1
         T = T2INV*(RI**(T2*PSI) - YI*YT1) + YI*T1INV*(YT1 - RT1)
         F1 = F1 + T*YN(2,I)
         ZN(1,I) = T
 50      CONTINUE
      GO TO 100
C
C *** THETA == 1 ***
C
 60   DO 70 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 230
         MU = RI**PSI
         YI = YN(1,I)
         T = MU - YI - YI*ALOG(MU/YI)
         F1 = F1 + T*YN(2,I)
         ZN(1,I) = T
         ZN(2,I) = ONE
 70      CONTINUE
      GO TO 100
C
C *** THETA == 2 ***
C
 80   DO 90 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 230
         T1 = RI**(-PSI)
         YI = YN(1,I) * T1
         T = YI - ALOG(YI) - ONE
         F1 = F1 + T*YN(2,I)
         ZN(1,I) = T
         ZN(2,I) = T1
 90      CONTINUE
 100  F = HALF*F + F1/PHI
      GO TO 999
C
C  ***  GRADIENT COMPUTATIONS  ***
C
 110  PHIINV = ONE / PHI
      PHII2 = PHIINV * PHIINV
      RP0 = HALF * PHIINV
      RPP0 = -PHIINV * RP0
      PHII3 = TWO * PHIINV * PHII2
      KMP = N
      KPP = N + N
      T1 = ONE - THETA
      T2 = TWO - THETA
      IF (RHOI(2) .LE. RHOI(3)+2) GO TO 130
C
C  *** PSI DERIVATIVES ***
C
      K = KPP + N
      KMPS = 6*N
      KPPS = KMPS + N
      KTPS = KPPS + N
      KPSPS = KTPS + N
      DO 120 I = 1, N
         WI = YN(2,I)
         RI = R(I)
         MU = RI**PSI
         RL = ALOG(RI)
         RT1 = WI * ZN(2,I)
         RT2 = RT1 * MU
         YI = YN(1,I)
         T = (RL/PHI) * (RT2 - YI*RT1)
         K = K + 1
         R(K) = T
         KMPS = KMPS + 1
         TT = RL * (T2*RT2 - YI*T1*RT1)
         RD(KMPS) = (RT2 - YI*RT1 + PSI*TT) / (RI*PHI)
         KPPS = KPPS + 1
         RD(KPPS) = -T / PHI
         KTPS = KTPS + 1
         RD(KTPS) = -PSI * RL * T
         KPSPS = KPSPS + 1
         RD(KPSPS) = TT * RL / PHI
 120     CONTINUE
C
 130  IF (RHOI(2) .LE. RHOI(3)) GO TO 210
      IF (RHOI(2) .EQ. RHOI(3)+1) GO TO 190
C
C  *** THETA DERIVATIVES ***
C
      K = KPP
      KMT = K + N
      KPT = KMT + N
      KTT = KPT + N
      IF (THETA .EQ. ONE) GO TO 150
      IF (THETA .EQ. TWO) GO TO 170
      T1INV = ONE / T1
      T1INV2 = T1INV + T1INV
      T2INV = ONE / T2
      T2INV2 = T2INV + T2INV
      DO 140 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PHIINV
         RI = R(I)
         MU = RI**PSI
         RT1 = ZN(2,I)
         RT2 = RT1 * MU
         RL = ALOG(MU)
         RT1L = RT1 * RL
         RT2L = RT2 * RL
         YI = YN(1,I)
         YT1 = ZN(3,I)
         YT2 = YT1 * YI
         YL = YLOG(I)
         YT1L = YT1 * YL
         YT2L = YT2 * YL
         T = PHIINV * (YI * T1INV * (RL*RT1 - YL*YT1 +
     1                          T1INV*(YT1 - RT1))
     2                  + T2INV * (YL*YT2 - RL*RT2 +
     3                          T2INV*(RT2 - YT2)))
         K = K + 1
         R(K) = HALF*YL + WI*T
         KMT = KMT + 1
         RD(KMT) = PSI * WOVPHI * RL * (YI*RT1 - RT2) / RI
         KPT = KPT + 1
         RD(KPT) = -WOVPHI * T
         KTT = KTT + 1
         RD(KTT) = WOVPHI*(T1INV*YI*(YT1L*YL - RT1L*RL +
     1                       T1INV2*(RT1L - YT1L +
     2                        T1INV*(YT1 - RT1))) +
     3                        T2INV*(RT2L*RL - YT2L*YL +
     4                       T2INV2*(YT2L - RT2L +
     5                        T2INV*(RT2 - YT2))))
 140     CONTINUE
      GO TO 190
C
C *** THETA DERIVATIVES AT THETA == 1 ***
C
 150  DO 160 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PHIINV
         YI = YN(1,I)
         YL = YLOG(I)
         RI = R(I)
         MU = RI**PSI
         RL = ALOG(MU)
         K = K + 1
         T = HALF*YI*(RL*RL - YL*YL) + YI*YL - MU*RL + MU - YI
         R(K) = HALF*YL + WI*T
         KMT = KMT + 1
         RD(KMT) = PSI * WOVPHI * RL * (YI - MU) / RI
         KPT = KPT + 1
         RD(KPT) = -WOVPHI * T
         KTT = KTT + 1
         T = RL * RL
         RD(KTT) = WOVPHI * ( MU * (TWO - TWO*RL + T)
     1          -YI*(TWO - T*RL/THREE + YL*(YL - TWO + YL*YL/THREE)))
 160     CONTINUE
      GO TO 190
C
C *** THETA DERIVATIVES AT THETA == 2 ***
C
 170  DO 180 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PHIINV
         YI = YN(1,I)
         YL = YLOG(I)
         RI = R(I)
         MU = RI**PSI
         RL = ALOG(MU)
         K = K + 1
         T = HALF*(YL*YL - RL*RL) + YL + ONE - (YI + YI*RL)/MU
         R(K) = HALF*YL + WI*T
         KMT = KMT + 1
         RD(KMT) = PSI * WOVPHI * RL * (YI/MU - ONE) / RI
         KPT = KPT + 1
         RD(KPT) = -WOVPHI * T
         KTT = KTT + 1
         T = RL * RL
         RD(KTT) = WOVPHI * ((YL/MU)*(T + TWO*RL + TWO) - TWO
     1                  - YL*(TWO + YL*(ONE + YL/THREE)) + T*RL/THREE)
 180     CONTINUE
C
C *** PHI AND MU DERIVATIVES ***
C
 190  K = N
      THETA = ONE - PSI*T1
      T1 = PSI*T2 - ONE
      PSPHII = PSI * PHIINV
      PHIINV = -PHIINV
      DO 200 I = 1, N
         WI = YN(2,I)
         WOVPHI = WI * PSPHII
         RI = R(I)
         MU = RI**PSI
         YI = YN(1,I)
         RT1 = ZN(2,I)/RI
         T2 = WOVPHI * RT1 * (MU - YI)
         R(I) = T2
         RD(I) = WOVPHI * RT1 * (T1*MU + YI*THETA) / RI
         T = ZN(1,I)
         K = K + 1
         R(K) = RP0 - WI*PHII2*T
         KMP = KMP + 1
         RD(KMP) = PHIINV * T2
         KPP = KPP + 1
         RD(KPP) = RPP0 + WI*PHII3*T
 200     CONTINUE
      GO TO 999
C
C *** JUST MU DERIVATIVES ***
C
 210  PHIINV = PHIINV * PSI
      THETA = ONE - PSI*T1
      T1 = PSI*T2 - ONE
      DO 220 I = 1, N
         WOVPHI = YN(2,I) * PHIINV
         RI = R(I)
         MU = RI**PSI
         YI = YN(1,I)
         RT1 = ZN(2,I)/RI
         R(I) = WOVPHI * RT1 * (MU - YI)
         RD(I) = WOVPHI * RT1 * (T1*MU + YI*THETA) / RI
 220     CONTINUE
      GO TO 999
C
 230  NF = 0
C
 999  RETURN
      END
      SUBROUTINE PRGRH1(N, PT, R, RHO, RHOI, YN)
C
C  ***  RHO FOR PREGIBON ERROR MODELS ***
C
      INTEGER N, RHOI(3)
      REAL PT(2), R(*), RHO(N), YN(2,N)
C *** LOCAL VARIABLES ***
C
      INTEGER I
      REAL HTHETA, PHI, RI, RT1, T, T1, T1INV, T2, T2INV,
     1                  THETA, YI, YT1
C
      REAL HALF, ONE, TWO
      DATA HALF/0.5E+0/, ONE/1.E+0/, TWO/2.E+0/
C
C *** BODY ***
C
      PHI = PT(1)
      THETA = PT(2)
      HTHETA = HALF * THETA
      DO 10 I = 1, N
 10      RHO(I) = HTHETA*ALOG(PHI*YN(1,I))
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
         T = RI - YI - YI*ALOG(RI/YI)
         RHO(I) = RHO(I) + T / PHI
 40      CONTINUE
      GO TO 999
 50   DO 60 I = 1, N
         YI = YN(1,I) / R(I)
         T = YI - ALOG(YI) - ONE
         RHO(I) = RHO(I) + T / PHI
 60      CONTINUE
 999  RETURN
      END
      SUBROUTINE RHPOIL(NEED, F, N, NF, PT, R, RD, RHOI, YN, W)
      COMMON /FUDGE/ NFUDGE
      INTEGER NFUDGE
      INTEGER NEED(2), N, NF, RHOI(6)
      REAL F, PT(3), R(*), RD(*), W(N), YN(2,N)
C PT = PHI AND THETA (WHEN PS == P, I.E. RHOI(2) == RHOI(3))
C
      REAL INVCN, LPN, PNORMS,  R7MDC
      EXTERNAL INVCN, LPN, PNORMS,  R7MDC
      INTEGER ERRFLG, I, IM, WCOMP
      REAL CI, E, PHI, PHIRI, PHIMRI, PSI, PSI1, PSI2,
     1                 RI, T, T1, T2, THETA, YI
      REAL  ATAN,  EXP, ALOG,  SQRT
      REAL CNN, EIGHT, EXPMAX, EXPMIN, FOUR, HALF, ONE, TWO,
     1                 TWOPI, ZERO
      DATA CNN/0.E+0/, EXPMAX/0.E+0/, EIGHT/8.E+0/, EXPMIN/0.E+0/,
     1     FOUR/4.0E+0/, HALF/0.5E+0/, ONE/1.E+0/, TWO/2.E+0/,
     2     TWOPI/0.E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
      IM = RHOI(1)
      WCOMP = RHOI(6)
      IF (IM .LE. 0) GO TO 800
      IF (IM .GT. 13) GO TO 800
      IF (EXPMAX .GT. ZERO) GO TO 10
         EXPMAX = TWO * ALOG( R7MDC(5))
         EXPMIN = TWO * ALOG( R7MDC(2))
         TWOPI = EIGHT *  ATAN(ONE)
 10   IF (NEED(1) .EQ. 2) GO TO 240
      F = ZERO
      GO TO (20,20,40,60,80,80,100,120,140,160,180,220,180), IM
C
C *** POISSON RHO (AND CONVENTIONAL IRLS) ***
C
 20   DO 30 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         F = F + YN(2,I)*RI - YN(1,I)*ALOG(RI)
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
         F = F + YN(2,I)*RI**2 - TWO*YN(1,1)*ALOG(RI)
 70      CONTINUE
      GO TO 999
C
C *** BINOMIAL RHO (AND CONVENTIONAL IRLS) ***
C
 80   DO 90 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         IF (RI .GE. ONE) GO TO 800
         F = F - YN(1,I)*ALOG(RI) - (YN(2,I) - YN(1,I))*ALOG(ONE-RI)
 90      CONTINUE
      GO TO 999
C
C *** BINOMIAL LOGISTIC RHO ***
C
 100  DO 110 I = 1, N
         RI = R(I)
         IF (RI .GE. EXPMAX) GO TO 800
         E = ZERO
         IF (RI .GT. EXPMIN) E =  EXP(RI)
         F = F + YN(2,I)*ALOG(ONE + E) - YN(1,I)*RI
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
         IF (RI .GT. EXPMIN) E =  EXP(RI)
         R(I) = E
         T = ZERO
         IF (-E .GT. EXPMIN) T =  EXP(-E)
         F = F + (YN(2,I) - YN(1,I))*E - YN(1,I)*ALOG(ONE - T)
 150     CONTINUE
      GO TO 999
C
C  *** GAMMA ERRORS ***
C
 160  DO 170 I = 1, N
         RI = R(I)
         IF (RI .LE. ZERO) GO TO 800
         F = F + YN(1,I)*RI - YN(2,I)*ALOG(RI)
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
            T1 = CI*RI + YI*(ALOG(E/CI) - ONE)
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
            W(I) = YI * ((T - ONE) / ALOG(T))
            ENDIF
         GO TO 340
 330     T1 = RI + YI*(ALOG(YI/RI) - ONE)
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
 380     T1 = CI*RI*RI - YI + YI*ALOG(YI/(CI*RI*RI))
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
         T1 = (YI - CI)*ALOG((ONE - RI)/(ONE - T2)) + YI*ALOG(T2/RI)
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
 470     T1 = (ONE + RI)*ALOG(RI*(CI-YI)/YI)
         IF (T1 .NE. ZERO) THEN
            W(I) = ((CI - YI)*RI - YI) / T1
         ELSE
            W(I) = RD(I)
            END IF
         GO TO 490
 480     T1 = CI*ALOG((ONE+RI)*(ONE - YI/CI)) + YI*ALOG(YI/(RI*(CI-YI)))
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
         IF (T .GT. EXPMIN) E = CNN *  EXP(T)
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
         T1 = T2*(ALOG(T2/CI) - LPN(-RI))
         IF (YI .GT. ZERO) T1 = T1 + YI*(ALOG(YI/CI) - LPN(RI))
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
         IF (-RI .GT. EXPMIN) E =  EXP(-RI)
         T = RI / (ONE - E)
         CI = YN(2,I)*RI
         YI = YN(1,I)*T
         R(I) = CI - YI
         RD(I) = CI - YI*(ONE - E*T)
         GO TO (570,580,590,600), WCOMP
 580     W(I) = E*CI*RI / (ONE - E)
         GO TO 610
 590     T1 = ALOG(-RI / ALOG(ONE - YN(1,I)/YN(2,I)))
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
         T1 = CI*(RI + ALOG(ONE - T2)) + YI*(ALOG(T2/(ONE - E)))
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
C        F = F + YN(1,I)*RI - YN(2,I)*ALOG(RI)
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
            T2 = T1 - ALOG(T2)
            T = T / (T2+T2)
            END IF
         W(I) = T
 650     CONTINUE
      IF (WCOMP .LE. 2) GO TO 820
      GO TO 999
C
C ***  PREGIBON ERRORS ***
C
 660  IF (WCOMP .GE. 2) CALL  V7CPY(N, W, R)
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
         T1 = YN(2,I)/PHI * (RI**PSI - YI + YI*(ALOG(YI)-PSI*ALOG(RI)))
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
         T1 = YI*RI**(-PSI) - ONE + PSI*ALOG(RI) - ALOG(YI)
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
 820  CALL  V7CPY(N, W, RD)
C
 999  RETURN
      END
      REAL FUNCTION LPN(X)
      COMMON /FUDGE/ NFUDGE
      INTEGER NFUDGE
      REAL X
      EXTERNAL PNORMS
      REAL PNORMS
      REAL T
      REAL ALOG
      REAL HALF, ZERO
      DATA HALF/0.5E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
      T = PNORMS(X)
      IF (T .GT. ZERO) THEN
         LPN = ALOG(T)
      ELSE
         NFUDGE = NFUDGE + 1
         LPN = -HALF*X**2 - ALOG(-X)
         END IF
 999  RETURN
      END
