      FUNCTION PPSPF (X,IZ,C,A,BH)
      DIMENSION       A(1)       ,C(1)       ,BH(1)
      SUM = 0.
      DO 101 J=1,IZ
         SUM = SUM+1./(X-BH(J))
  101 CONTINUE
      PPSPF = SUM
      RETURN
      END
