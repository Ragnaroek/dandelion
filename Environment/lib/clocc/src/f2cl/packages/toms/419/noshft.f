      SUBROUTINE  NOSHFT(L1)                                            NOSH1130
C COMPUTES  THE DERIVATIVE  POLYNOMIAL AS THE INITIAL H
C POLYNOMIAL AND COMPUTES L1 NO-SHIFT H POLYNOMIALS.
C COMMON AREA
      COMMON/GLOBAL/PR,PI,HR,HI,QPR,QPI,QHR,QHI,SHR,SHI,
     *    SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,
     *    PR(50),PI(50),HR(50),HI(50),QPR(50),QPI(50),QHR(50),
     *    QHI(50),SHR(50),SHI(50)
      DOUBLE PRECISION XNI,T1,T2,CMOD
      N = NN-1
      NM1 = N-1
      DO 10 I = 1,N
          XNI = NN-I
          HR(I) = XNI*PR(I)/FLOAT(N)
          HI(I) = XNI*PI(I)/FLOAT(N)
   10 CONTINUE
      DO 50 JJ = 1,L1
          IF (CMOD(HR(N),HI(N)) .LE. ETA*10.0D0*CMOD(PR(N),PI(N)))
     *    GO TO 30
          CALL CDIVID(-PR(NN),-PI(NN),HR(N),HI(N),TR,TI)
          DO 20 I = 1,NM1
               J = NN-I
               T1 = HR(J-1)
               T2 = HI(J-1)
               HR(J) = TR*T1-TI*T2+PR(J)
               HI(J) = TR*T2+TI*T1+PI(J)
   20     CONTINUE
          HR(1) = PR(1)
          HI(1) = PI(1)
          GO TO 50
C IF THE CONSTANT TERM IS ESSENTIALLY ZERO, SHIFT H COEFFICIENTS.
   30     DO 40 I = 1,NM1
               J = NN-I
               HR(J) = HR(J-1)
               HI(J) = HI(J-1)
   40     CONTINUE
          HR(1) = 0.0D0
          HI(1) = 0.0D0
   50 CONTINUE
      RETURN
      END
