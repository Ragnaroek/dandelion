      SUBROUTINE CPRODP (ND,BD,NM1,BM1,NM2,BM2,NA,AA,X,YY,M,A,B,C,D,U,Y)
C
C PRODP APPLIES A SEQUENCE OF MATRIX OPERATIONS TO THE VECTOR X AND
C STORES THE RESULT IN YY       PERIODIC BOUNDARY CONDITIONS
C AND  COMPLEX  CASE
C
C BD,BM1,BM2 ARE ARRAYS CONTAINING ROOTS OF CERTIAN B POLYNOMIALS
C ND,NM1,NM2 ARE THE LENGTHS OF THE ARRAYS BD,BM1,BM2 RESPECTIVELY
C AA   ARRAY CONTAINING SCALAR MULTIPLIERS OF THE VECTOR X
C NA IS THE LENGTH OF THE ARRAY AA
C X,YY THE MATRIX OPERATIONS ARE APPLIED TO X AND THE RESULT IS YY
C A,B,C  ARE ARRAYS WHICH CONTAIN THE TRIDIAGONAL MATRIX
C M  IS THE ORDER OF THE MATRIX
C D,U,Y ARE WORKING ARRAYS
C ISGN  DETERMINES WHETHER OR NOT A CHANGE IN SIGN IS MADE
C
      COMPLEX         Y          ,D          ,U          ,V          ,
     1                DEN        ,BH         ,YM         ,AM         ,
     2                Y1         ,Y2         ,YH         ,BD         ,
     3                CRT
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,X(1)       ,
     1                Y(1)       ,D(1)       ,U(1)       ,BD(1)      ,
     2                BM1(1)     ,BM2(1)     ,AA(1)      ,YY(1)
      DO 101 J=1,M
         Y(J) = CMPLX(X(J),0.)
  101 CONTINUE
      MM = M-1
      MM2 = M-2
      ID = ND
      M1 = NM1
      M2 = NM2
      IA = NA
  102 IFLG = 0
      IF (ID) 111,111,103
  103 CRT = BD(ID)
      ID = ID-1
      IFLG = 1
C
C BEGIN SOLUTION TO SYSTEM
C
      BH = B(M)-CRT
      YM = Y(M)
      DEN = B(1)-CRT
      D(1) = C(1)/DEN
      U(1) = A(1)/DEN
      Y(1) = Y(1)/DEN
      V = CMPLX(C(M),0.)
      IF (MM2-2) 106,104,104
  104 DO 105 J=2,MM2
         DEN = B(J)-CRT-A(J)*D(J-1)
         D(J) = C(J)/DEN
         U(J) = -A(J)*U(J-1)/DEN
         Y(J) = (Y(J)-A(J)*Y(J-1))/DEN
         BH = BH-V*U(J-1)
         YM = YM-V*Y(J-1)
         V = -V*D(J-1)
  105 CONTINUE
  106 DEN = B(M-1)-CRT-A(M-1)*D(M-2)
      D(M-1) = (C(M-1)-A(M-1)*U(M-2))/DEN
      Y(M-1) = (Y(M-1)-A(M-1)*Y(M-2))/DEN
      AM = A(M)-V*D(M-2)
      BH = BH-V*U(M-2)
      YM = YM-V*Y(M-2)
      DEN = BH-AM*D(M-1)
      IF (CABS(DEN)) 107,108,107
  107 Y(M) = (YM-AM*Y(M-1))/DEN
      GO TO 109
  108 Y(M) = (1.,0.)
  109 Y(M-1) = Y(M-1)-D(M-1)*Y(M)
      DO 110 J=2,MM
         K = M-J
         Y(K) = Y(K)-D(K)*Y(K+1)-U(K)*Y(M)
  110 CONTINUE
  111 IF (M1) 112,112,114
  112 IF (M2) 123,123,113
  113 RT = BM2(M2)
      M2 = M2-1
      GO TO 119
  114 IF (M2) 115,115,116
  115 RT = BM1(M1)
      M1 = M1-1
      GO TO 119
  116 IF (ABS(BM1(M1))-ABS(BM2(M2))) 118,118,117
  117 RT = BM1(M1)
      M1 = M1-1
      GO TO 119
  118 RT = BM2(M2)
      M2 = M2-1
C
C MATRIX MULTIPLICATION
C
  119 YH = Y(1)
      Y1 = (B(1)-RT)*Y(1)+C(1)*Y(2)+A(1)*Y(M)
      IF (MM-2) 122,120,120
  120 DO 121 J=2,MM
         Y2 = A(J)*Y(J-1)+(B(J)-RT)*Y(J)+C(J)*Y(J+1)
         Y(J-1) = Y1
         Y1 = Y2
  121 CONTINUE
  122 Y(M) = A(M)*Y(M-1)+(B(M)-RT)*Y(M)+C(M)*YH
      Y(M-1) = Y1
      IFLG = 1
      GO TO 102
  123 IF (IA) 126,126,124
  124 RT = AA(IA)
      IA = IA-1
      IFLG = 1
C
C SCALAR MULTIPLICATION
C
      DO 125 J=1,M
         Y(J) = RT*Y(J)
  125 CONTINUE
  126 IF (IFLG) 127,127,102
  127 DO 128 J=1,M
         YY(J) = REAL(Y(J))
  128 CONTINUE
      RETURN
      END
