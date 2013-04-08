      SUBROUTINE PREGRH(DERIV, F, N, NF, PT, R, RD, RHOI, YLOG, YN, ZN)
C
C  ***  RHO FOR PREGIBON ERROR MODELS WITH REPLICATE WEIGHTS ***
C  ***  SEE PREGRV FOR THE RIGHT WEIGHTING FOR THE INSURANCE EXAMPLE ***
C
      INTEGER DERIV, N, NF, RHOI(*)
      DOUBLE PRECISION F, PT(3), R(*), RD(*), YLOG(*), YN(2,N), ZN(3,N)
      EXTERNAL DR7MDC
      DOUBLE PRECISION DR7MDC
C
C *** LOCAL VARIABLES ***
C
      INTEGER I, K, KMP, KMPS, KMT, KPP, KPPS, KPSPS, KPT, KTT, KTPS
      DOUBLE PRECISION F1, MU, PHI, PHII2, PHII3, PHIINV, PSI, PSPHII,
     1                 RI, RL, RP0, RPP0, RT1, RT1L, RT2, RT2L, RTOL, T,
     2                 T1, T1INV, T1INV2, T2, T2INV, T2INV2, THETA, TT,
     3                 WI, WOVPHI, YI, YL, YT1, YT1L, YT2, YT2L
C
      DOUBLE PRECISION BIG, BIGH, TWOPI
      DOUBLE PRECISION BTOL, EIGHT, HALF, ONE, THREE, TWO, ZERO
      DATA BIG/0.D+0/, BIGH/0.D+0/, TWOPI/0.D+0/
      DATA BTOL/1.01D+0/, EIGHT/8.D+0/, HALF/0.5D+0/, ONE/1.D+0/,
     1     THREE/3.D+0/, TWO/2.D+0/, ZERO/0.D+0/
C
C *** BODY ***
C
      IF (NF .GT. 1) GO TO 20
      IF (DERIV .GT. 0) GO TO 20
      DO 10 I = 1, N
 10      YLOG(I) = DLOG(YN(1,I))
 20   PHI = PT(1)
      PSI = PT(3)
      IF (PHI .LE. ZERO) GO TO 240
      THETA = PT(2)
      IF (TWOPI .GT. ZERO) GO TO 30
         TWOPI = EIGHT * DATAN(ONE)
         BIGH = DR7MDC(5)
         BIG = DR7MDC(6)
 30   T2 = TWO - THETA
      T1 = ONE - THETA
      IF (DERIV .GT. 0) GO TO 120
      RTOL = BIG
      IF (T2 .LT. BTOL) GO TO 40
         RTOL = BIGH**(ONE/T2)
         RTOL = RTOL*RTOL
 40   T = DLOG(TWOPI * PHI)
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
         T = MU - YI - YI*DLOG(MU/YI)
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
         T = YI - DLOG(YI) - ONE
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
         RL = DLOG(RI)
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
         RL = DLOG(MU)
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
         RL = DLOG(MU)
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
         RL = DLOG(MU)
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
