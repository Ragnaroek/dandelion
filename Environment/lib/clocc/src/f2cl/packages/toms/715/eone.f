CS    REAL FUNCTION EONE(X)
      DOUBLE PRECISION FUNCTION EONE(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   exponential integral E1(x), where  x  is real.
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
      INT = 2
      CALL CALCEI(X,RESULT,INT)
      EONE = RESULT
      RETURN
C---------- Last line of EONE ----------
      END
