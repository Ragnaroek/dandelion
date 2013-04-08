      SUBROUTINE DG2LRD(DR, IV, L, LH, LIV, LV, ND, N, P, PS, R, RD,
     1                  RHOI, RHOR, V, W, X, Z)
C
C  ***  COMPUTE REGRESSION DIAGNOSTIC FOR  DRGLG  ***
C
C  ***  PARAMETERS  ***
C
      INTEGER LH, LIV, LV, ND, N, P, PS
      INTEGER IV(LIV), RHOI(*)
      DOUBLE PRECISION DR(ND,P), L(LH), R(N), RD(N), RHOR(*), V(LV),
     1                 W(P), X(P), Z(P)
C
C  ***  CODED BY DAVID M. GAY (SPRING 1986, SUMMER 1991)  ***
C
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL DD7TPR, DL7ITV, DL7IVM,DL7SRT, DL7SQR, DS7LVM,
     1        DV2AXY,DV7CPY, DV7SCP
      DOUBLE PRECISION DD7TPR
C
C DD7TPR... COMPUTES INNER PRODUCT OF TWO VECTORS.
C DL7ITV... MULTIPLIES INVERSE TRANSPOSE OF LOWER TRIANGLE TIMES VECTOR.
C DL7IVM... APPLY INVERSE OF COMPACT LOWER TRIANG. MATRIX.
C DL7SRT.... COMPUTES CHOLESKY FACTOR OF (LOWER TRIANG. OF) SYM. MATRIX.
C DL7SQR... COMPUTES L*(L**T) FOR LOWER TRIANG. MATRIX L.
C DS7LVM... MULTIPLIES COMPACTLY STORED SYM. MATRIX TIMES VECTOR.
C DV2AXY.... ADDS A MULTIPLE OF ONE VECTOR TO ANOTHER.
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.
C
C  ***  LOCAL VARIABLES  ***
C
      LOGICAL USEFLO
      INTEGER BS1, BSINC, FLO1, FLOINC, H1, HPS1, I,
     1        J, J1, K, KI, KI1, KID, L1, LE, LL, LOO1, N1,
     2        PMPS, PP1O2, PS1, PX, RDR, XNI, ZAP1, ZAPLEN
      DOUBLE PRECISION FRAC, HI, RI, S, T, T1
C
C  ***  CONSTANTS  ***
C
      DOUBLE PRECISION HALF, NEGONE, ONE, ZERO
C
C
C  ***  IV SUBSCRIPTS  ***
C
      INTEGER BS, BSSTR, COVREQ, FDH, FLO, FLOSTR, LOO, NB, NFIX,
     1        RDREQ, REGD, XNOTI
      PARAMETER (BS=85, BSSTR=86, COVREQ=15, FDH=74, FLO=88, FLOSTR=89,
     1           LOO=84, NB=87, NFIX=83, RDREQ=57, REGD=67, XNOTI=90)
      PARAMETER (HALF=0.5D+0, NEGONE=-1.D+0, ONE=1.D+0, ZERO=0.D+0)
C
C++++++++++++++++++++++++++++++++  BODY  +++++++++++++++++++++++++++++++
C
      I = IV(RDREQ)
      RDR = MOD(I/2, 3)
      IF (RDR .EQ. 0) GO TO 999
      H1 = IV(FDH)
      USEFLO = .FALSE.
      PX = P
      N1 = N
      FRAC = ONE
      XNI = 0
      IF (RDR .EQ. 1) GO TO 120
      LOO1 = IV(LOO)
      IF (LOO1 .LE. 0 .OR. LOO1 .GT. 6) THEN
         IV(REGD) = -1
         GO TO 999
         ENDIF
      IF (LOO1 .GT. 3) THEN
         USEFLO = .TRUE.
         FLO1 = IV(FLO)
         FLOINC = IV(FLOSTR)
         LOO1 = LOO1 - 3
         ENDIF
      XNI = IV(XNOTI)
      PX = P - IV(NFIX)
      IF (PX .LT. PS .OR. PX .GT. P) THEN
         IV(REGD) = -2
         GO TO 999
         ENDIF
      IF (LOO1 .EQ. 1) GO TO 120
      N1 = IV(NB)
      IF (N1 .LE. 0 .OR. N1 .GT. N) THEN
         IV(REGD) = -3
         GO TO 999
         ENDIF
      BS1 = IV(BS)
      BSINC = IV(BSSTR)
      IF (H1 .LE. 0) GO TO 190
      IF (IABS(IV(COVREQ)) .GE. 3) CALL DL7SQR(P, V(H1), L)
      PP1O2 = PX*(PX+1)/2
      PS1 = PS + 1
      ZAP1 = PS*(PS1)/2 + 1
      LE = 0
      DO 100 I = 1, N1
         IF (USEFLO) THEN
            FRAC = RHOR(FLO1)
            FLO1 = FLO1 + FLOINC
            ENDIF
         L1 = LE + 1
         IF (L1 .GT. N) GO TO 110
         LE = LE + RHOI(BS1)
         IF (LE .GT. N) LE = N
         BS1 = BS1 + BSINC
         CALL DV7CPY(PP1O2, L, V(H1))
         IF (PS .GE. PX) GO TO 50
            K = ZAP1
            KI = L1
            DO 40 J = PS1, P
               KI = KI + N
               KI1 = KI
               DO 10 LL = L1, LE
                  CALL DV2AXY(PS, L(K), -FRAC*RD(KI1), DR(1,LL), L(K))
                  KI1 = KI1 + 1
 10               CONTINUE
               K = K + PS
               DO 30 J1 = PS1, J
                  KI = KI + N
                  KI1 = KI
                  T = ZERO
                  DO 20 LL = L1, LE
                     T = T + RD(KI1)
                     KI1 = KI1 + 1
 20                  CONTINUE
                  L(K) = L(K) - FRAC*T
                  K = K + 1
 30               CONTINUE
 40            CONTINUE
 50      DO 70 LL = L1, LE
            T = -FRAC*RD(LL)
            K = 1
            DO 60 J = 1, PS
               CALL DV2AXY(J, L(K), T*DR(J,LL), DR(1,LL), L(K))
               K = K + J
 60            CONTINUE
 70         CONTINUE
         CALL DL7SRT(1, PX, L, L, J)
         IF (J .EQ. 0) THEN
            CALL DV7SCP(PX, W, ZERO)
            DO 90 LL = L1, LE
               CALL DV2AXY(PS, W, R(LL), DR(1,LL), W)
               IF (PS1 .GT. PX) GO TO 90
               K = L1
               DO 80 J = PS1, P
                  K = K + N
                  W(J) = W(J) + R(K)
 80               CONTINUE
 90            CONTINUE
            CALL DL7IVM(PX, W, L, W)
            CALL DL7ITV(PX, W, L, W)
            CALL DS7LVM(PX, Z, V(H1), W)
            RD(I) = HALF * FRAC * DD7TPR(PX, W, Z)
            IF (XNI .GT. 0) THEN
               CALL DV2AXY(PX, RHOR(XNI), FRAC, W, X)
               XNI = XNI + PX
               ENDIF
         ELSE
            RD(I) = NEGONE
            IF (XNI .GT. 0) THEN
               CALL DV7CPY(PX, RHOR(XNI), X)
               XNI = XNI + PX
               ENDIF
            ENDIF
 100     CONTINUE
 110  IV(REGD) = 1
C     *** RESTORE L ***
      CALL DL7SRT(1, P, L, V(H1), J)
      GO TO 999
C
 120  IF (H1 .LE. 0) GO TO 190
      IF (IABS(IV(COVREQ)) .GE. 3) CALL DL7SQR(P, V(H1), L)
      IF (PS .GE. PX) GO TO 170
      PS1 = PS + 1
      PMPS = PX - PS
      ZAP1 = PS*(PS1)/2
      ZAPLEN = PX*(PX+1)/2 - ZAP1
      HPS1 = H1 + ZAP1
      ZAP1 = ZAP1 + 1
      DO 160 I = 1, N
         IF (USEFLO) THEN
            FRAC = RHOR(FLO1)
            FLO1 = FLO1 + FLOINC
            ENDIF
         CALL DV7CPY(ZAPLEN, L(ZAP1), V(HPS1))
         CALL DV7SCP(PS, W, ZERO)
         K = ZAP1
         KI = I
         KID = KI
         DO 140 J = PS1, PX
            KI = KI + N
            CALL DV2AXY(PS, L(K), -FRAC*RD(KI), DR(1,I), L(K))
            K = K + PS
            KID = KID + N
            W(J) = FRAC*R(KID)
            DO 130 J1 = PS1, J
               KI = KI + N
               L(K) =  L(K) - FRAC*RD(KI)
               K = K + 1
 130           CONTINUE
 140        CONTINUE
         CALL DL7SRT(PS1, PX, L, L, J)
         IF (J .NE. 0) GO TO 150
         CALL DV7CPY(PS, Z, DR(1,I))
         CALL DV7SCP(PMPS, Z(PS1), ZERO)
         CALL DL7IVM(PX, Z, L, Z)
         HI = DD7TPR(PX, Z, Z)
         CALL DL7IVM(PX, W, L, W)
         RI = FRAC*R(I)
C        *** FIRST PS ELEMENTS OF W VANISH ***
         T = DD7TPR(PMPS, W(PS1), Z(PS1))
         S = FRAC*RD(I)
         T1 = ONE - S*HI
         IF (T1 .LE. ZERO) GO TO 150
         CALL DV2AXY(PX, W, (RI + S*T)/T1, Z, W)
         CALL DL7ITV(PX, W, L, W)
         CALL DS7LVM(PX, Z, V(H1), W)
         RD(I) = HALF * DD7TPR(PX, W, Z)
         IF (XNI .GT. 0) THEN
            CALL DV2AXY(PX, RHOR(XNI), ONE, W, X)
            XNI = XNI + PX
            ENDIF
         GO TO 160
 150     RD(I) = NEGONE
         IF (XNI .GT. 0) THEN
            CALL DV7CPY(PX, RHOR(XNI), X)
            XNI = XNI + PX
            ENDIF
 160     CONTINUE
C
C     *** RESTORE L ***
C
      CALL DV7CPY(ZAPLEN, L(ZAP1), V(HPS1))
      CALL DL7SRT(PS1, PX, L, L, J)
      GO TO 200
C
 170  DO 180 I = 1, N
         IF (USEFLO) THEN
            FRAC = RHOR(FLO1)
            FLO1 = FLO1 + FLOINC
            ENDIF
         CALL DL7IVM(PX, Z, L, DR(1,I))
         S = DD7TPR(PX, Z, Z)
         T = ONE - FRAC*RD(I) * S
         IF (T .LE. ZERO) THEN
            RD(I) = NEGONE
            IF (XNI .GT. 0) THEN
               CALL DV7CPY(PX, RHOR(XNI), X)
               XNI = XNI + PX
               ENDIF
         ELSE
            RD(I) = HALF * FRAC * (R(I)/T)**2 * S
            IF (XNI .GT. 0) THEN
               CALL DL7ITV(PX, Z, L, Z)
               CALL DV2AXY(PX, RHOR(XNI), FRAC*R(I)/T, Z, X)
               XNI = XNI + PX
               ENDIF
            ENDIF
 180     CONTINUE
      GO TO 200
C
 190  CALL DV7SCP(N1, RD, NEGONE)
 200  IV(REGD) = 1
C
 999  RETURN
C  ***  LAST LINE OF DG2LRD FOLLOWS  ***
      END
