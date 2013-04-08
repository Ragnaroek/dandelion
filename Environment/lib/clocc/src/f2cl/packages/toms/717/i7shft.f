      SUBROUTINE I7SHFT(N, K, X)
C
C  ***  SHIFT X(K),...,X(N) LEFT CIRCULARLY ONE POSITION  ***
C
      INTEGER N, K
      INTEGER X(N)
C
      INTEGER I, NM1, T
C
      IF (K .GE. N) GO TO 999
      NM1 = N - 1
      T = X(K)
      DO 10 I = K, NM1
 10      X(I) = X(I+1)
      X(N) = T
 999  RETURN
      END
