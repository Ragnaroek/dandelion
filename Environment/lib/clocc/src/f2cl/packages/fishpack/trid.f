      SUBROUTINE TRID (MR,A,B,C,Y,D)
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,Y(1)       ,
     1                D(1)
      M = MR
      MM1 = M-1
      Z = 1./B(1)
      D(1) = C(1)*Z
      Y(1) = Y(1)*Z
      DO 101 I=2,MM1
         Z = 1./(B(I)-A(I)*D(I-1))
         D(I) = C(I)*Z
         Y(I) = (Y(I)-A(I)*Y(I-1))*Z
  101 CONTINUE
      Z = B(M)-A(M)*D(MM1)
      IF (Z .NE. 0.) GO TO 102
      Y(M) = 0.
      GO TO 103
  102 Y(M) = (Y(M)-A(M)*Y(MM1))/Z
  103 CONTINUE
      DO 104 IP=1,MM1
         I = M-IP
         Y(I) = Y(I)-D(I)*Y(I+1)
  104 CONTINUE
      RETURN
      END
