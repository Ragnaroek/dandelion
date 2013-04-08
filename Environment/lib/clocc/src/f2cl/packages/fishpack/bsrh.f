      FUNCTION BSRH (XLL,XRR,IZ,C,A,BH,F,SGN)
      DIMENSION       A(1)       ,C(1)       ,BH(1)
      COMMON /CBLKT/  NPP        ,K          ,EPS        ,CNV        ,
     1                NM         ,NCMPLX     ,IK
      XL = XLL
      XR = XRR
      DX = .5*ABS(XR-XL)
  101 X = .5*(XL+XR)
      IF (SGN*F(X,IZ,C,A,BH)) 103,105,102
  102 XR = X
      GO TO 104
  103 XL = X
  104 DX = .5*DX
      IF (DX-CNV) 105,105,101
  105 BSRH = .5*(XL+XR)
      RETURN
      END
