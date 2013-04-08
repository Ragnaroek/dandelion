      SUBROUTINE INDXA (I,IR,IDXA,NA)
      COMMON /CBLKT/  NPP        ,K          ,EPS        ,CNV        ,
     1                NM         ,NCMPLX     ,IK
      NA = 2**IR
      IDXA = I-NA+1
      IF (I-NM) 102,102,101
  101 NA = 0
  102 RETURN
      END
