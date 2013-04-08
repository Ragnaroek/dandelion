      SUBROUTINE DD7UP5(D, IV, LIV, LV, P, PS, V)
C
C  ***  UPDATE SCALE VECTOR D FOR DG7LIT  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER LIV, LV, P, PS
      INTEGER IV(LIV)
      DOUBLE PRECISION D(P), V(LV)
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER D0, HII, I, JTOLI, JTOL0, R1I, S1
      DOUBLE PRECISION T, VDFAC
C
C     ***  CONSTANTS  ***
      DOUBLE PRECISION ZERO
C
C     ***  EXTERNAL FUNCTIONS  ***
C
      EXTERNAL DD7TPR
      DOUBLE PRECISION DD7TPR
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER DFAC, DTYPE, HC, JTOL, NITER, RMAT, S
      PARAMETER (DFAC=41, DTYPE=16, HC=71, JTOL=59, NITER=31, RMAT=78,
     1           S=62)
C
      PARAMETER (ZERO=0.D+0)
C
C  ***  BODY  ***
C
      IF (IV(DTYPE) .NE. 1 .AND. IV(NITER) .GT. 0) GO TO 999
      R1I = IV(RMAT)
      HII = IV(HC) - 1
      VDFAC = V(DFAC)
      JTOL0 = IV(JTOL) - 1
      D0 = JTOL0 + P
      S1 = IV(S) - 1
      DO 30 I = 1, P
         IF (R1I .LE. 0) GO TO 10
             T = DD7TPR(I, V(R1I), V(R1I))
             R1I = R1I + I
             GO TO 20
 10      HII = HII + I
         T =  ABS(V(HII))
 20      S1 = S1 + I
         IF (I .LE. PS) T = T +   MAX(V(S1), ZERO)
         T =  SQRT(T)
         JTOLI = JTOL0 + I
         D0 = D0 + 1
         IF (T .LT. V(JTOLI)) T =   MAX(V(D0), V(JTOLI))
         D(I) =   MAX(VDFAC*D(I), T)
 30      CONTINUE
C
 999  RETURN
C  ***  LAST LINE OF DD7UP5 FOLLOWS  ***
      END
