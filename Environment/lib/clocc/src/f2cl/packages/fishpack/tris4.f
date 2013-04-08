      SUBROUTINE TRIS4 (N,A,B,C,D,U,Z)
C
C     THIS SUBROUTINE SOLVES FOR A NON-ZERO EIGENVECTOR CORRESPONDING
C     TO THE ZERO EIGENVALUE OF THE TRANSPOSE OF THE RANK
C     DEFICIENT ONE MATRIX WITH SUBDIAGONAL A, DIAGONAL B, AND
C     SUPERDIAGONAL C , WITH A(1) IN THE (1,N) POSITION, WITH
C     C(N) IN THE (N,1) POSITION, AND ALL OTHER ELEMENTS ZERO.
C
      DIMENSION       A(N)       ,B(N)       ,C(N)       ,D(N)       ,
     1                U(N)       ,Z(N)
      BN = B(N)
      D(1) = A(2)/B(1)
      V = A(1)
      U(1) = C(N)/B(1)
      NM2 = N-2
      DO  10 J=2,NM2
         DEN = B(J)-C(J-1)*D(J-1)
         D(J) = A(J+1)/DEN
         U(J) = -C(J-1)*U(J-1)/DEN
         BN = BN-V*U(J-1)
         V = -V*D(J-1)
   10 CONTINUE
      DEN = B(N-1)-C(N-2)*D(N-2)
      D(N-1) = (A(N)-C(N-2)*U(N-2))/DEN
      AN = C(N-1)-V*D(N-2)
      BN = BN-V*U(N-2)
      DEN = BN-AN*D(N-1)
C
C     SET LAST COMPONENT EQUAL TO ONE
C
      Z(N) = 1.0
      Z(N-1) = -D(N-1)
      NM1 = N-1
      DO  20 J=2,NM1
         K = N-J
         Z(K) = -D(K)*Z(K+1)-U(K)*Z(N)
   20 CONTINUE
      RETURN
      END
