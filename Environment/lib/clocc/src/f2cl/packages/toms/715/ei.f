CS    REAL FUNCTION EI(X)
      DOUBLE PRECISION FUNCTION EI(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   exponential integral  Ei(x), where  x  is real.
C
C  Author: W. J. Cody
C
C  Latest modification: March 9, 1992
C
C--------------------------------------------------------------------
      INTEGER INT
CS    REAL  X, RESULT
      DOUBLE PRECISION  X, RESULT
C--------------------------------------------------------------------
      INT = 1
      CALL CALCEI(X,RESULT,INT)
      EI = RESULT
      RETURN
C---------- Last line of EI ----------
      END
