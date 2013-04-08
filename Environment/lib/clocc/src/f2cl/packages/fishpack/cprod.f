      SUBROUTINE CPROD (ND,BD,NM1,BM1,NM2,BM2,NA,AA,X,YY,M,A,B,C,D,W,Y)
C
C PROD APPLIES A SEQUENCE OF MATRIX OPERATIONS TO THE VECTOR X AND
C STORES THE RESULT IN YY           (COMPLEX CASE)
C AA   ARRAY CONTAINING SCALAR MULTIPLIERS OF THE VECTOR X
C ND,NM1,NM2 ARE THE LENGTHS OF THE ARRAYS BD,BM1,BM2 RESPECTIVELY
C BD,BM1,BM2 ARE ARRAYS CONTAINING ROOTS OF CERTIAN B POLYNOMIALS
C NA IS THE LENGTH OF THE ARRAY AA
C X,YY THE MATRIX OPERATIONS ARE APPLIED TO X AND THE RESULT IS YY
C A,B,C  ARE ARRAYS WHICH CONTAIN THE TRIDIAGONAL MATRIX
C M  IS THE ORDER OF THE MATRIX
C D,W,Y ARE WORKING ARRAYS
C ISGN  DETERMINES WHETHER OR NOT A CHANGE IN SIGN IS MADE
C
      COMPLEX         Y          ,D          ,W          ,BD         ,
     1                CRT        ,DEN        ,Y1         ,Y2
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,X(1)       ,
     1                Y(1)       ,D(1)       ,W(1)       ,BD(1)      ,
     2                BM1(1)     ,BM2(1)     ,AA(1)      ,YY(1)
      DO 101 J=1,M
         Y(J) = CMPLX(X(J),0.)
  101 CONTINUE
      MM = M-1
      ID = ND
      M1 = NM1
      M2 = NM2
      IA = NA
  102 IFLG = 0
      IF (ID) 109,109,103
  103 CRT = BD(ID)
      ID = ID-1
C
C BEGIN SOLUTION TO SYSTEM
C
      D(M) = A(M)/(B(M)-CRT)
      W(M) = Y(M)/(B(M)-CRT)
      DO 104 J=2,MM
         K = M-J
         DEN = B(K+1)-CRT-C(K+1)*D(K+2)
         D(K+1) = A(K+1)/DEN
         W(K+1) = (Y(K+1)-C(K+1)*W(K+2))/DEN
  104 CONTINUE
      DEN = B(1)-CRT-C(1)*D(2)
      IF (CABS(DEN)) 105,106,105
  105 Y(1) = (Y(1)-C(1)*W(2))/DEN
      GO TO 107
  106 Y(1) = (1.,0.)
  107 DO 108 J=2,M
         Y(J) = W(J)-D(J)*Y(J-1)
  108 CONTINUE
  109 IF (M1) 110,110,112
  110 IF (M2) 121,121,111
  111 RT = BM2(M2)
      M2 = M2-1
      GO TO 117
  112 IF (M2) 113,113,114
  113 RT = BM1(M1)
      M1 = M1-1
      GO TO 117
  114 IF (ABS(BM1(M1))-ABS(BM2(M2))) 116,116,115
  115 RT = BM1(M1)
      M1 = M1-1
      GO TO 117
  116 RT = BM2(M2)
      M2 = M2-1
  117 Y1 = (B(1)-RT)*Y(1)+C(1)*Y(2)
      IF (MM-2) 120,118,118
C
C MATRIX MULTIPLICATION
C
  118 DO 119 J=2,MM
         Y2 = A(J)*Y(J-1)+(B(J)-RT)*Y(J)+C(J)*Y(J+1)
         Y(J-1) = Y1
         Y1 = Y2
  119 CONTINUE
  120 Y(M) = A(M)*Y(M-1)+(B(M)-RT)*Y(M)
      Y(M-1) = Y1
      IFLG = 1
      GO TO 102
  121 IF (IA) 124,124,122
  122 RT = AA(IA)
      IA = IA-1
      IFLG = 1
C
C SCALAR MULTIPLICATION
C
      DO 123 J=1,M
         Y(J) = RT*Y(J)
  123 CONTINUE
  124 IF (IFLG) 125,125,102
  125 DO 126 J=1,M
         YY(J) = REAL(Y(J))
  126 CONTINUE
      RETURN
      END
