      SUBROUTINE COMPB (N,IERROR,AN,BN,CN,B,AH,BH)
C
C     COMPB COMPUTES THE ROOTS OF THE B POLYNOMIALS USING SUBROUTINE
C     TEVLS WHICH IS A MODIFICATION THE EISPACK PROGRAM TQLRAT.
C     IERROR IS SET TO 4 IF EITHER TEVLS FAILS OR IF A(J+1)*C(J) IS
C     LESS THAN ZERO FOR SOME J.  AH,BH ARE TEMPORARY WORK ARRAYS.
C
      DIMENSION       AN(1)      ,BN(1)      ,CN(1)      ,B(1)       ,
     1                AH(1)      ,BH(1)
      COMMON /CBLKT/  NPP        ,K          ,EPS        ,CNV        ,
     1                NM         ,NCMPLX     ,IK
      EPS = EPMACH(DUM)
      BNORM = ABS(BN(1))
      DO 102 J=2,NM
         BNORM = AMAX1(BNORM,ABS(BN(J)))
         ARG = AN(J)*CN(J-1)
         IF (ARG) 119,101,101
  101    B(J) = SIGN(SQRT(ARG),AN(J))
  102 CONTINUE
      CNV = EPS*BNORM
      IF = 2**K
      KDO = K-1
      DO 108 L=1,KDO
         IR = L-1
         I2 = 2**IR
         I4 = I2+I2
         IPL = I4-1
         IFD = IF-I4
         DO 107 I=I4,IFD,I4
            CALL INDXB (I,L,IB,NB)
            IF (NB) 108,108,103
  103       JS = I-IPL
            JF = JS+NB-1
            LS = 0
            DO 104 J=JS,JF
               LS = LS+1
               BH(LS) = BN(J)
               AH(LS) = B(J)
  104       CONTINUE
            CALL TEVLS (NB,BH,AH,IERROR)
            IF (IERROR) 118,105,118
  105       LH = IB-1
            DO 106 J=1,NB
               LH = LH+1
               B(LH) = -BH(J)
  106       CONTINUE
  107    CONTINUE
  108 CONTINUE
      DO 109 J=1,NM
         B(J) = -BN(J)
  109 CONTINUE
      IF (NPP) 117,110,117
  110 NMP = NM+1
      NB = NM+NMP
      DO 112 J=1,NB
         L1 = MOD(J-1,NMP)+1
         L2 = MOD(J+NM-1,NMP)+1
         ARG = AN(L1)*CN(L2)
         IF (ARG) 119,111,111
  111    BH(J) = SIGN(SQRT(ARG),-AN(L1))
         AH(J) = -BN(L1)
  112 CONTINUE
      CALL TEVLS (NB,AH,BH,IERROR)
      IF (IERROR) 118,113,118
  113 CALL INDXB (IF,K-1,J2,LH)
      CALL INDXB (IF/2,K-1,J1,LH)
      J2 = J2+1
      LH = J2
      N2M2 = J2+NM+NM-2
  114 D1 = ABS(B(J1)-B(J2-1))
      D2 = ABS(B(J1)-B(J2))
      D3 = ABS(B(J1)-B(J2+1))
      IF ((D2 .LT. D1) .AND. (D2 .LT. D3)) GO TO 115
      B(LH) = B(J2)
      J2 = J2+1
      LH = LH+1
      IF (J2-N2M2) 114,114,116
  115 J2 = J2+1
      J1 = J1+1
      IF (J2-N2M2) 114,114,116
  116 B(LH) = B(N2M2+1)
      CALL INDXB (IF,K-1,J1,J2)
      J2 = J1+NMP+NMP
      CALL PPADD (NM+1,IERROR,AN,CN,B(J1),B(J1),B(J2))
  117 RETURN
  118 IERROR = 4
      RETURN
  119 IERROR = 5
      RETURN
      END
