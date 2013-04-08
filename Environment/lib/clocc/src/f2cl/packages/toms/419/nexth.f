      SUBROUTINE NEXTH(BOOL)                                            NEXT3110
C CALCULATES THE NEXT SHIFTED H POLYNOMIAL.
C BOOL   -  LOGICAL, IF .TRUE. H(S) IS ESSENTIALLY ZERO
C COMMON AREA
      COMMON/GLOBAL/PR,PI,HR,HI,QPR,QPI,QHR,QHI,SHR,SHI,
     *    SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,
     *    PR(50),PI(50),HR(50),HI(50),QPR(50),QPI(50),QHR(50),
     *    QHI(50),SHR(50),SHI(50)
      DOUBLE PRECISION T1,T2
      LOGICAL BOOL
      N = NN-1
      NM1 = N-1
      IF (BOOL) GO TO 20
          DO 10 J = 2,N
               T1 = QHR(J-1)
               T2 = QHI(J-1)
               HR(J) = TR*T1-TI*T2+QPR(J)
               HI(J) = TR*T2+TI*T1+QPI(J)
   10     CONTINUE
          HR(1) = QPR(1)
          HI(1) = QPI(1)
          RETURN
C IF H(S) IS ZERO REPLACE H WITH QH.
   20 DO 30 J = 2,N
          HR(J) = QHR(J-1)
          HI(J) = QHI(J-1)
   30 CONTINUE
      HR(1) = 0.0D0
      HI(1) = 0.0D0
      RETURN
      END
