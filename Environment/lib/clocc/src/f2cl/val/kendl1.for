      SUBROUTINE KENDL1(DATA1,DATA2,N,TAU,Z,PROB)
      DIMENSION DATA1(N),DATA2(N)
      N1=0
      N2=0
      IS=0
      DO 12 J=1,N-1
        DO 11 K=J+1,N
          A1=DATA1(J)-DATA1(K)
          A2=DATA2(J)-DATA2(K)
          AA=A1*A2
          IF(AA.NE.0.)THEN
            N1=N1+1
            N2=N2+1
            IF(AA.GT.0.)THEN
              IS=IS+1
            ELSE
              IS=IS-1
            ENDIF
          ELSE
            IF(A1 .NE.0.)N1=N1+1
            IF(A2 .NE.0.)N2=N2+1
          ENDIF
11      CONTINUE
12    CONTINUE
      TAU=FLOAT(IS)/SQRT(FLOAT(N1)*FLOAT(N2))
      VAR=(4.*N+10.)/(9.*N*(N-1.))
      Z=TAU/SQRT(VAR)
      PROB=ERFCC(ABS(Z)/1.4142136)
      RETURN
      END
