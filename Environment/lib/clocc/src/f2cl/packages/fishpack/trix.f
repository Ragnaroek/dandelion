      SUBROUTINE TRIX (IDEGBR,IDEGCR,M,A,B,C,Y,TCOS,D,W)
C
C     SUBROUTINE TO SOLVE A SYSTEM OF LINEAR EQUATIONS WHERE THE
C     COEFFICIENT MATRIX IS A RATIONAL FUNCTION IN THE MATRIX GIVEN BY
C     TRIDIAGONAL  ( . . . , A(I), B(I), C(I), . . . ).
C
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,Y(1)       ,
     1                TCOS(1)    ,D(1)       ,W(1)
      MM1 = M-1
      FB = IDEGBR+1
      FC = IDEGCR+1
      L = FB/FC
      LINT = 1
      DO 108 K=1,IDEGBR
         X = TCOS(K)
         IF (K .NE. L) GO TO 102
         I = IDEGBR+LINT
         XX = X-TCOS(I)
         DO 101 I=1,M
            W(I) = Y(I)
            Y(I) = XX*Y(I)
  101    CONTINUE
  102    CONTINUE
         Z = 1./(B(1)-X)
         D(1) = C(1)*Z
         Y(1) = Y(1)*Z
         DO 103 I=2,MM1
            Z = 1./(B(I)-X-A(I)*D(I-1))
            D(I) = C(I)*Z
            Y(I) = (Y(I)-A(I)*Y(I-1))*Z
  103    CONTINUE
         Z = B(M)-X-A(M)*D(MM1)
         IF (Z .NE. 0.) GO TO 104
         Y(M) = 0.
         GO TO 105
  104    Y(M) = (Y(M)-A(M)*Y(MM1))/Z
  105    CONTINUE
         DO 106 IP=1,MM1
            I = M-IP
            Y(I) = Y(I)-D(I)*Y(I+1)
  106    CONTINUE
         IF (K .NE. L) GO TO 108
         DO 107 I=1,M
            Y(I) = Y(I)+W(I)
  107    CONTINUE
         LINT = LINT+1
         L = (FLOAT(LINT)*FB)/FC
  108 CONTINUE
      RETURN
      END
