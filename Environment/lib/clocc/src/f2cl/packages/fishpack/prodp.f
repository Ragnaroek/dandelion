      SUBROUTINE PRODP (ND,BD,NM1,BM1,NM2,BM2,NA,AA,X,Y,M,A,B,C,D,U,W)
C
C PRODP APPLIES A SEQUENCE OF MATRIX OPERATIONS TO THE VECTOR X AND
C STORES THE RESULT IN Y        PERIODIC BOUNDARY CONDITIONS
C
C BD,BM1,BM2 ARE ARRAYS CONTAINING ROOTS OF CERTIAN B POLYNOMIALS
C ND,NM1,NM2 ARE THE LENGTHS OF THE ARRAYS BD,BM1,BM2 RESPECTIVELY
C AA   ARRAY CONTAINING SCALAR MULTIPLIERS OF THE VECTOR X
C NA IS THE LENGTH OF THE ARRAY AA
C X,Y  THE MATRIX OPERATIONS ARE APPLIED TO X AND THE RESULT IS Y
C A,B,C  ARE ARRAYS WHICH CONTAIN THE TRIDIAGONAL MATRIX
C M  IS THE ORDER OF THE MATRIX
C D,U,W ARE WORKING ARRAYS
C IS  DETERMINES WHETHER OR NOT A CHANGE IN SIGN IS MADE
C
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,X(1)       ,
     1                Y(1)       ,D(1)       ,U(1)       ,BD(1)      ,
     2                BM1(1)     ,BM2(1)     ,AA(1)      ,W(1)
      DO 101 J=1,M
         Y(J) = X(J)
         W(J) = Y(J)
  101 CONTINUE
      MM = M-1
      MM2 = M-2
      ID = ND
      IBR = 0
      M1 = NM1
      M2 = NM2
      IA = NA
  102 IF (IA) 105,105,103
  103 RT = AA(IA)
      IF (ND .EQ. 0) RT = -RT
      IA = IA-1
      DO 104 J=1,M
         Y(J) = RT*W(J)
  104 CONTINUE
  105 IF (ID) 128,128,106
  106 RT = BD(ID)
      ID = ID-1
      IF (ID .EQ. 0) IBR = 1
C
C BEGIN SOLUTION TO SYSTEM
C
      BH = B(M)-RT
      YM = Y(M)
      DEN = B(1)-RT
      D(1) = C(1)/DEN
      U(1) = A(1)/DEN
      W(1) = Y(1)/DEN
      V = C(M)
      IF (MM2-2) 109,107,107
  107 DO 108 J=2,MM2
         DEN = B(J)-RT-A(J)*D(J-1)
         D(J) = C(J)/DEN
         U(J) = -A(J)*U(J-1)/DEN
         W(J) = (Y(J)-A(J)*W(J-1))/DEN
         BH = BH-V*U(J-1)
         YM = YM-V*W(J-1)
         V = -V*D(J-1)
  108 CONTINUE
  109 DEN = B(M-1)-RT-A(M-1)*D(M-2)
      D(M-1) = (C(M-1)-A(M-1)*U(M-2))/DEN
      W(M-1) = (Y(M-1)-A(M-1)*W(M-2))/DEN
      AM = A(M)-V*D(M-2)
      BH = BH-V*U(M-2)
      YM = YM-V*W(M-2)
      DEN = BH-AM*D(M-1)
      IF (DEN) 110,111,110
  110 W(M) = (YM-AM*W(M-1))/DEN
      GO TO 112
  111 W(M) = 1.
  112 W(M-1) = W(M-1)-D(M-1)*W(M)
      DO 113 J=2,MM
         K = M-J
         W(K) = W(K)-D(K)*W(K+1)-U(K)*W(M)
  113 CONTINUE
      IF (NA) 116,116,102
  114 DO 115 J=1,M
         Y(J) = W(J)
  115 CONTINUE
      IBR = 1
      GO TO 102
  116 IF (M1) 117,117,118
  117 IF (M2) 114,114,123
  118 IF (M2) 120,120,119
  119 IF (ABS(BM1(M1))-ABS(BM2(M2))) 123,123,120
  120 IF (IBR) 121,121,122
  121 IF (ABS(BM1(M1)-BD(ID))-ABS(BM1(M1)-RT)) 114,122,122
  122 RT = RT-BM1(M1)
      M1 = M1-1
      GO TO 126
  123 IF (IBR) 124,124,125
  124 IF (ABS(BM2(M2)-BD(ID))-ABS(BM2(M2)-RT)) 114,125,125
  125 RT = RT-BM2(M2)
      M2 = M2-1
  126 DO 127 J=1,M
         Y(J) = Y(J)+RT*W(J)
  127 CONTINUE
      GO TO 102
  128 RETURN
      END
