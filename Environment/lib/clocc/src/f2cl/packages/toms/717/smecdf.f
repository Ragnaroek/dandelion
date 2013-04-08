      SUBROUTINE MECDF(NDIM, D, RHO, PROB, IER)
      INTEGER NDIM, IER
      REAL D(*), PROB, RHO(*)
C-----------------------------------------------------------------
C       6/29/90
C       This subroutine is designed to calculate the MVN CDF
C       using the Mendell-Elston procedure as described in
C       Kamakura (1989).  The current version is set up to go
C       as high as 19 dimensions (=> 20 MNP alternatives)
C       NOTE:  Equation (15) in Kamakura has an error.
C
C       Specifically, assume that Z is a set of random variables
C       with a standard normal distribution with correlations
C       stored in RHO (in packed form).  Then this subroutine
C       calculates Prob[Z(1)>D(1);...; Z(NDIM) > D(NDIM)].
C-----------------------------------------------------------------

      REAL ALNORM, PHI
      EXTERNAL ALNORM, PHI

      INTEGER MAXALT, NMAX
      PARAMETER (MAXALT=20, NMAX=MAXALT-1)

      INTEGER I, IM1, IR, J, JM1, K, KM1
      REAL PROBI, TMP
      REAL R(NMAX,NMAX,0:NMAX-1), SIG(NMAX,0:NMAX-1),
     1                 U(NMAX), UUMZ(NMAX-1), Z(NMAX,0:NMAX-1)

      REAL ONE, ZERO
      PARAMETER (ONE=1.E0, ZERO=0.E0)

C-----------------------------------------------------------------
C       Test dimension
      IER = 0
      IF (NDIM.GT.NMAX) THEN
         IER = -1
         RETURN
      ENDIF
C       Set up arrays
      IR = 0
      DO 10 I = 1, NDIM
         Z(I,0) = D(I)
         DO 10 J = 1, I-1
            IR = IR + 1
            R(J,I,0) = RHO(IR)
 10     CONTINUE
      PROB = ALNORM(Z(1,0), .TRUE.)
      IF (PROB.LE.ZERO) THEN
         IER = 1
         RETURN
      ENDIF
      U(1) = PHI(Z(1,0), ZERO)/PROB
      UUMZ(1) = U(1)*(U(1)-Z(1,0))

C       Main loop
      DO 40 I = 2, NDIM
         IM1 = I-1
         DO 30 J = 1, IM1
            JM1 = J-1
            DO 20 K = 1, JM1
               KM1 = K-1
               TMP = R(J,I,KM1)-R(K,J,KM1)*R(K,I,KM1)*UUMZ(K)
               R(J,I,K) = TMP/SIG(J,K)/SIG(I,K)
 20           CONTINUE
            SIG(I,J) = SQRT(ONE - UUMZ(J)*R(J,I,JM1)**2)
            Z(I,J) = (Z(I,JM1)-U(J)*R(J,I,JM1))/SIG(I,J)
 30        CONTINUE
         PROBI = ALNORM(Z(I,IM1), .TRUE.)
         IF (PROBI.LE.ZERO) THEN
            IER = I
            RETURN
         ENDIF
         PROB = PROB * PROBI
         IF (I.LT.NDIM) THEN
            U(I) = PHI(Z(I,IM1), ZERO)/PROBI
            UUMZ(I) = U(I)*(U(I)-Z(I,IM1))
         ENDIF
 40     CONTINUE
      END
C---------------------------------------------------
      REAL FUNCTION PHI(X, Y)
      REAL X, Y
      REAL ARG
      REAL HALF, SQ2P, XLOW, ZERO
      PARAMETER (HALF = 0.5E0, SQ2P = 0.91893853320467274E0,
     1           XLOW = -87.E0, ZERO = 0.E0)
      PHI = ZERO
      ARG = -HALF * X * X - SQ2P - Y
      IF (ARG .GT. XLOW) PHI = EXP(ARG)
      END
C---------------------------------------------------
      REAL FUNCTION ALNORM(X,UPPER)
      REAL X
      LOGICAL UPPER
C
C   ALGORITHM AS 66 BY I.D. HILL
C
      LOGICAL UP
      REAL Y, Z

      REAL CON, HALF, LTONE, ONE, UTZERO, ZERO
      PARAMETER (CON=1.28E0, HALF=0.5E0, LTONE=5.E0, ONE=1.E0,
     1           UTZERO=12.5E0, ZERO=0.E0)

      UP=UPPER
      Z=X
      IF(Z.GE.ZERO) GO TO 10
      UP=.NOT.UP
      Z=-Z
 10   IF(Z .LE. LTONE .OR. UP .AND. Z .LE. UTZERO) GO TO 20
      ALNORM = ZERO
      GO TO 40
 20   Y=HALF*Z*Z
      IF(Z.GT.CON) GO TO 30
      ALNORM = HALF - Z * (0.398942280444E0 - 0.399903438504E0*Y/
     1             (Y + 5.75885480458E0 - 29.8213557808E0/
     2             (Y + 2.62433121679E0 + 48.6959930692E0/
     3             (Y + 5.92885724438E0))))
      GO TO 40
 30   ALNORM = 0.398942280385E0 * EXP(-Y)/
     1             (Z - 3.8052E-8 + 1.00000615302E0/
     2             (Z + 3.98064794E-4 + 1.98615381364E0/
     3             (Z - 0.151679116635E0 + 5.29330324926E0/
     4             (Z + 4.8385912808E0 - 15.1508972451E0/
     5             (Z + 0.742380924027E0 + 30.789933034E0/
     6             (Z + 3.99019417011E0))))))
 40   IF(.NOT.UP) ALNORM = ONE - ALNORM
      END
