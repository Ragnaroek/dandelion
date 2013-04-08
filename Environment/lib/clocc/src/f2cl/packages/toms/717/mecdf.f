      SUBROUTINE MECDF(NDIM, D, RHO, PROB, IER)
      INTEGER NDIM, IER
      DOUBLE PRECISION D(*), PROB, RHO(*)
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

      DOUBLE PRECISION ALNORM, PHI
      EXTERNAL ALNORM, PHI

      INTEGER MAXALT, NMAX
      PARAMETER (MAXALT=20, NMAX=MAXALT-1)

      INTEGER I, IM1, IR, J, JM1, K, KM1
      DOUBLE PRECISION PROBI, TMP
      DOUBLE PRECISION R(NMAX,NMAX,0:NMAX-1), SIG(NMAX,0:NMAX-1),
     1                 U(NMAX), UUMZ(NMAX-1), Z(NMAX,0:NMAX-1)

      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.D0, ZERO=0.D0)

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
