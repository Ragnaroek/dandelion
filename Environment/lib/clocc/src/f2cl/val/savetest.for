C     bnldev.for from numerical recipes
C     example of SAVE and nested IFs
C
C
      FUNCTION BNLDEV(PP,N,IDUM)
      SAVE NOLD,POLD,PC,EN,OLDG,PLOG,PCLOG
      PARAMETER (PI=3.141592654)
      DATA NOLD /-1/, POLD /-1./
      IF(PP.LE.0.5)THEN
        P=PP
      ELSE
        P=1.-PP
      ENDIF
      AM=N*P
      IF (N.LT.25)THEN
        BNLDEV=0.
        DO 11 J=1,N
          IF(RAN1(IDUM).LT.P)BNLDEV=BNLDEV+1.
11      CONTINUE
      ELSE IF (AM.LT.1.) THEN
        G=EXP(-AM)
        T=1.
        DO 12 J=0,N
          T=T*RAN1(IDUM)
          IF (T.LT.G) GO TO 1
12      CONTINUE
        J=N
1       BNLDEV=J
      ELSE
        IF (N.NE.NOLD) THEN
          EN=N
          OLDG=GAMMLN(EN+1.)
          NOLD=N
        ENDIF
        IF (P.NE.POLD) THEN
          PC=1.-P
          PLOG=LOG(P)
          PCLOG=LOG(PC)
          POLD=P
        ENDIF
        SQ=SQRT(2.*AM*PC)
2       Y=TAN(PI*RAN1(IDUM))
        EM=SQ*Y+AM
        IF (EM.LT.0..OR.EM.GE.EN+1.) GO TO 2
        EM=INT(EM)
        T=1.2*SQ*(1.+Y**2)*EXP(OLDG-GAMMLN(EM+1.)
     *    -GAMMLN(EN-EM+1.)+EM*PLOG+(EN-EM)*PCLOG)
        IF (RAN1(IDUM).GT.T) GO TO 2
        BNLDEV=EM
      ENDIF
      IF (P.NE.PP) BNLDEV=N-BNLDEV
      RETURN
      END
