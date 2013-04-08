C Contains some character declarations
      SUBROUTINE H03ABF(KOST,MMM,MA,MB,M,K15,MAXIT,K7,K9,NUMIT,K6,K8,
     *                  K11,K12,Z,IFAIL)
C     NAG COPYRIGHT 1975
C     MARK 4.5 REVISED
C     MARK 6 REVISED  IER-91
C     MARK 7C REVISED IER-189 (MAY 1979)
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     H03ABF SOLVES THE CLASSICAL TRANSPORTATION PROBLEM IN THAT IT
C     MINIMIZES THE COST OF SENDING GOODS FROM SOURCES TO
C     DESTINATIONS
C     SUBJECT TO CONSTRAINTS ON THE AVAILABILITIES AND
C     REQUIREMENTS.
C     .. Parameters ..
      CHARACTER*6     SRNAME
      PARAMETER         (SRNAME='H03ABF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  Z
      INTEGER           IFAIL, M, MA, MAXIT, MB, MMM, NUMIT
C     .. Array Arguments ..
      INTEGER           K11(M), K12(M), K15(M), K6(M), K7(M), K8(M),
     *                  K9(M), KOST(MMM,MB)
C     .. Local Scalars ..
      DOUBLE PRECISION  X
      INTEGER           I, II, INF, J, J11, J21, J22, J23, J24, J25,
     *                  J26, J27, J28, JC, JV, KI, KJ, KK, KKK, L, LC,
     *                  LCK, LD, LDEL, LF, LR, LRH, LRK, LRMI, LSTA, LT,
     *                  LTK, LTMI, LV, LVK, LVMI, LX, M1, MIN, MU
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF, X02BBF
      EXTERNAL          P01ABF, X02BBF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE
C     .. Executable Statements ..
      NUMIT = 0
      IF (MA) 180, 180, 20
   20 IF (MB) 180, 180, 40
   40 IF (MA-MMM) 60, 60, 180
   60 IF (MAXIT) 160, 160, 80
   80 IF (M-MA-MB) 180, 100, 180
  100 KKK = 0
      DO 120 I = 1, MA
         KKK = KKK + K15(I)
  120 CONTINUE
      II = MA + 1
      DO 140 I = II, M
         KKK = KKK - K15(I)
  140 CONTINUE
      IF (KKK) 200, 220, 200
  160 KKK = 3
      GO TO 1660
  180 KKK = 4
      GO TO 1660
  200 KKK = 1
      GO TO 1660
  220 M1 = M - 1
      INF = X02BBF(X)
      K11(M) = 0
      DO 240 I = 1, MA
         K7(I) = I
  240 CONTINUE
      DO 260 I = 1, MB
         K9(I) = I
  260 CONTINUE
      LTK = 0
      KI = MA
      KJ = MB
      MIN = INF
      DO 320 I = 1, MA
         DO 300 J = 1, MB
            IF (KOST(I,J)-MIN) 280, 300, 300
  280       MIN = KOST(I,J)
            LRMI = I
            LTMI = J
  300    CONTINUE
  320 CONTINUE
C
C     KOST(LRMI,LTMI) CONTAINS THE SMALLEST COST COFFICIENT IN
C     ARRAY KOST
C
      LVMI = LTMI + MA
      IF (K15(LRMI)-K15(LVMI)) 340, 360, 360
  340 LDEL = K15(LVMI)
      K9(LTMI) = K9(KJ)
      KJ = KJ - 1
      LSTA = -1
      GO TO 380
  360 LDEL = K15(LRMI)
      K7(LRMI) = K7(KI)
      KI = KI - 1
      LSTA = 1
C
C     LDEL CONTAINS THE MAX OF THE ROW OR COL TOTALS,LSTA=1 IF THE
C     ROW
C     IS THE MAXIMUM OR =0 OTHERWISE. APPROPRIATE MARKERS ARE SET
C     IN K14.
C
  380 DO 640 I = 1, M1
  400    LSTA = -LSTA
         MIN = INF
         IF (LSTA) 500, 500, 420
  420    K8(I) = LVMI
         IF (KI) 400, 400, 440
  440    DO 480 J = 1, KI
            II = K7(J)
            IF (KOST(II,LTMI)-MIN) 460, 460, 480
  460       MIN = KOST(II,LTMI)
            LRMI = II
            KK = J
  480    CONTINUE
         LF = LRMI
         K7(KK) = K7(KI)
         KI = KI - 1
         K6(I) = LF
         GO TO 580
  500    K6(I) = LRMI
         IF (KJ) 400, 400, 520
  520    DO 560 J = 1, KJ
            II = K9(J)
            IF (KOST(LRMI,II)-MIN) 540, 540, 560
  540       MIN = KOST(LRMI,II)
            LTMI = II
            KK = J
  560    CONTINUE
         LF = LTMI + MA
         K9(KK) = K9(KJ)
         KJ = KJ - 1
         LVMI = LF
         K8(I) = LF
  580    K12(I) = MIN
         LD = LDEL - K15(LF)
         IF (LD) 600, 620, 620
  600    K11(I) = LDEL
         LDEL = -LD
         GO TO 640
  620    K11(I) = K15(LF)
         LDEL = LD
         LSTA = -LSTA
  640 CONTINUE
  660 IF (NUMIT-MAXIT) 700, 680, 680
  680 KKK = 2
      GO TO 1660
  700 NUMIT = NUMIT + 1
      DO 720 I = 1, M
         K7(I) = 0
         K15(I) = 0
  720 CONTINUE
      LR = K6(1)
      K7(LR) = 1
      LT = 1
  740 DO 880 I = 1, M1
         IF (K15(I)) 760, 760, 880
  760    LR = K6(I)
         LV = K8(I)
         IF (K7(LR)) 800, 800, 780
  780    K7(LV) = 1
         K9(LT) = I
         GO TO 840
  800    IF (K7(LV)) 880, 880, 820
  820    K7(LR) = 1
         K9(LT) = -I
  840    K15(I) = 1
         IF (LT-M1) 860, 900, 900
  860    LT = LT + 1
  880 CONTINUE
      GO TO 740
  900 DO 920 I = 1, M1
         MU = ABS(K9(I))
         K7(I) = K6(MU)
         K15(I) = K8(MU)
  920 CONTINUE
C
C
C
      DO 940 I = 1, M1
         K6(I) = K7(I)
         K8(I) = K15(I)
  940 CONTINUE
C
C
      DO 960 I = 1, M1
         MU = ABS(K9(I))
         K7(I) = K11(MU)
         K15(I) = K12(MU)
  960 CONTINUE
      DO 980 I = 1, M1
         K11(I) = K7(I)
         K12(I) = K15(I)
  980 CONTINUE
C
      LR = K6(1)
      K7(LR) = 0
      LV = K8(1)
      K7(LV) = 0
C
      DO 1040 I = 1, M1
         LR = K6(I)
         LV = K8(I)
         LC = K12(I)
         IF (K9(I)) 1000, 1020, 1020
 1000    K7(LR) = LC - K7(LV)
         GO TO 1040
 1020    K7(LV) = LC - K7(LR)
 1040 CONTINUE
C
      MIN = 0
      DO 1100 I = 1, MA
         DO 1080 J = 1, MB
            LV = J + MA
            L = KOST(I,J) - K7(I) - K7(LV)
            IF (L-MIN) 1060, 1080, 1080
 1060       MIN = L
            LRK = I
            LTK = J
 1080    CONTINUE
 1100 CONTINUE
C
      LVK = LTK + MA
      IF (MIN) 1120, 1620, 1620
 1120 LCK = KOST(LRK,LTK)
      K6(M) = LRK
      K8(M) = LVK
      DO 1140 I = 1, M
         K9(I) = -1
 1140 CONTINUE
C
      DO 1220 I = 1, M1
         J = K6(I)
         IF (K9(J)) 1160, 1180, 1180
 1160    K9(J) = I
 1180    J = K8(I)
         IF (K9(J)) 1200, 1220, 1220
 1200    K9(J) = I
 1220 CONTINUE
C
      MU = M
      J21 = -1
      J22 = INF
      J23 = M
      J24 = 1
      J25 = INF
      J26 = 0
      JV = M
      JC = 1
      K7(JC) = M
      GO TO 1340
 1240 IF (J21) 1260, 1260, 1280
 1260 JC = JC + 1
      K7(JC) = J28
      GO TO 1300
 1280 K7(JV) = J28
      JV = JV - 1
 1300 MU = J28
      J26 = 1 - J26
 1320 IF (J26) 1360, 1340, 1360
 1340 J27 = K6(MU)
      GO TO 1380
 1360 J27 = K8(MU)
 1380 IF (J27-J25) 1400, 1440, 1400
 1400 J28 = K9(J27)
      IF (J28-J22) 1420, 1240, 1240
 1420 J22 = J28
      J11 = MU
      MU = J23
      J23 = J11
      I = J26
      J26 = J24
      J24 = I
      J25 = J27
      J21 = -J21
      GO TO 1320
C
C
C
 1440 IF (JV-M) 1460, 1540, 1460
 1460 IF (JC-JV) 1480, 1520, 1480
 1480 JV = JV + 1
      DO 1500 I = JV, M
         JC = JC + 1
         K7(JC) = K7(I)
 1500 CONTINUE
      GO TO 1540
 1520 JC = M
 1540 MIN = INF
      DO 1580 I = 2, JC, 2
         MU = K7(I)
         LX = K11(MU)
         IF (LX-MIN) 1560, 1580, 1580
 1560    MIN = LX
         LRH = MU
 1580 CONTINUE
      K6(LRH) = LRK
      K8(LRH) = LVK
      DO 1600 I = 1, JC, 2
         MU = K7(I)
         K11(MU) = K11(MU) + MIN
         MU = K7(I+1)
         K11(MU) = K11(MU) - MIN
 1600 CONTINUE
      K11(LRH) = MIN
      K12(LRH) = LCK
      GO TO 660
C
 1620 Z = 0.D0
      DO 1640 I = 1, M1
         Z = Z + DBLE(K11(I)*K12(I))
         K8(I) = K8(I) - MA
 1640 CONTINUE
C
      IFAIL = 0
      RETURN
 1660 IFAIL = P01ABF(IFAIL,KKK,SRNAME,0,P01REC)
      RETURN
      END
