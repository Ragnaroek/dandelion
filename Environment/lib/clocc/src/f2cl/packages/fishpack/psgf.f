      FUNCTION PSGF (X,IZ,C,A,BH)
      DIMENSION       A(1)       ,C(1)       ,BH(1)
      FSG = 1.
      HSG = 1.
      DO 101 J=1,IZ
         DD = 1./(X-BH(J))
         FSG = FSG*A(J)*DD
         HSG = HSG*C(J)*DD
  101 CONTINUE
      IF (MOD(IZ,2)) 103,102,103
  102 PSGF = 1.-FSG-HSG
      RETURN
  103 PSGF = 1.+FSG+HSG
      RETURN
      END
