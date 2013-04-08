      SUBROUTINE BRJ(N, P, X, NF, NEED, R, RP, UI, A, UF)
      INTEGER N, P, NF, NEED(2), UI(5)
      DOUBLE PRECISION X(P), R(N), RP(P,N), A(*)
      EXTERNAL UF
      EXTERNAL BRJ1
      INTEGER M
C
C *** BODY ***
C
      M = UI(6)
      CALL BRJ1(M, N, UI(7), X, NF, NEED, R, RP, UI, A, A(M*N+1), UF)
 999  RETURN
      END
