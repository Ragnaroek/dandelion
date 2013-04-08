CS    REAL FUNCTION EXPEI(X)
      DOUBLE PRECISION FUNCTION EXPEI(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   function  exp(-x) * Ei(x), where  Ei(x)  is the exponential
C   integral, and  x  is real.
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
      INT = 3
      CALL CALCEI(X,RESULT,INT)
      EXPEI = RESULT
      RETURN
C---------- Last line of EXPEI ----------
      END
