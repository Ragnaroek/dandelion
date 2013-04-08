      PROGRAM PMAIN
C *** MAIN PROGRAM FOR RUNNING PREG EXAMPLES USING DGLG ***
      INTEGER LIV, LV, MMAX, NMAX, NW, NR0, PMAX
      PARAMETER (LIV=200, LV=8000, NW=6, MMAX = 18, NMAX=200, NR0=8,
     1           PMAX=20)
      CHARACTER*72 FNAME
      CHARACTER*6 ALGNAM(4)
      INTEGER ALG, I, IV(LIV), J, J0, J1, K, KDIAG, M, MDL(6), MODEL,
     1        N, NIN, NR, NRUN, P, P0, PS, RHOI(NMAX+6), UI(7)
      DOUBLE PRECISION A((MMAX+6)*NMAX), B(2,PMAX),
     1                 RHOR((17+PMAX)*NMAX+4), T, T1, V(LV), X(PMAX+3),
     1                 X0(PMAX+3), YN(2,7*NMAX+3)
      EQUIVALENCE (RHOI(1), MDL(1)), (RHOR(1), YN(1,1))
      CHARACTER*96 DESC, FMT
      CHARACTER*8 WNAME(4)
      DOUBLE PRECISION DR7MDC
      EXTERNAL BRJ, CHKDER, DEVIAN, DGLF, DGLFB, DGLG, DGLGB, DIVSET,
     1         DR7MDC, DV7CPY, DV7SCP, LOUCHK, POIX0, RHPOIL, RPOIL0
      DOUBLE PRECISION ONE
      INTEGER BS, BSSTR, F, FLO, FLOSTR, LOO, NB, NFIX, RDREQ, XNOTI
      PARAMETER (BS=85, BSSTR=86, F=10, FLO=88, FLOSTR=89, LOO=84,
     1           NB=87, NFIX=83, RDREQ=57, XNOTI=90)
      DATA ALG/1/, KDIAG/0/, NIN/5/
      DATA ALGNAM(1)/'DGLG'/,  ALGNAM(2)/'DGLF'/
      DATA ALGNAM(3)/'DGLGB'/, ALGNAM(4)/'DGLFB'/
      DATA ONE/1.D+0/
      DATA WNAME(1)/'  RHO"  '/, WNAME(2)/'  IRLS  '/,
     1     WNAME(3)/' SCORE  '/, WNAME(4)/'DEVIANCE'/
C
C *** BODY ***
C
      CALL DIVSET(1, IV, LIV, LV, V)
      IV(FLO) = 16*NMAX + 5
      IV(XNOTI) = IV(FLO) + NMAX
      IV(BS) = 7
      IV(BSSTR) = 1
      IV(FLOSTR) = 1
      IV(LOO) = 1
      IV(NB) = 5
      IV(NFIX) = 0
      CALL DV7SCP(NMAX, RHOR(IV(FLO)), ONE)
      CALL DV7SCP(NMAX, RHOR(IV(XNOTI)), -2.D+0)
      DO 10 I = IV(BS), IV(BS) + NMAX - 1
 10      RHOI(I) = 1
      T = DR7MDC(6)
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
     1 '4 = READ ALG: 1 = DGLG, 2 = DGLF, 3 = DGLGB, 4 = DGLFB'
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
 90   CALL DV7CPY(P0+3, X0, X)
      GO TO 30
 100  CALL DV7CPY(P0+3, X, X0)
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
 120  CALL DGLG(N, P, PS, X, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 160
 130  CALL DGLF(N, P, PS, X, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 160
 140  CALL DGLGB(N, P, PS, X, B, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 30
 150  CALL DGLFB(N, P, PS, X, B, RHPOIL, RHOI, YN,
     1       IV, LIV, LV, V, BRJ, UI, A, BRJ)
      GO TO 30
 160  IF (IV(1) .LT. 8) THEN
         CALL DEVIAN(V(F), MDL(1), N, NW, X(PS+1), YN)
         IF (ALG .EQ. 1) CALL LOUCHK(KDIAG,   DGLG, X0, N, P, PS, X,
     1       RHPOIL, MDL, YN, IV, LIV, LV, V, BRJ, UI, A, BRJ)
         IF (ALG .EQ. 2) CALL LOUCHK(KDIAG,   DGLF, X0, N, P, PS, X,
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
      CALL DV7SCP(3, X0(P+1), ONE)
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
