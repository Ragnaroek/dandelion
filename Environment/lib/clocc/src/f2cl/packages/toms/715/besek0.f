CS    REAL FUNCTION BESEK0(X)
      DOUBLE PRECISION FUNCTION BESEK0(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order zero
C   multiplied by the Exponential function, for arguments
C   0.0 .LT. ARG.
C
C  Authors: W. J. Cody and Laura Stoltz
C
C  Latest Modification: March 14, 1992
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1    X, RESULT
C--------------------------------------------------------------------
      JINT = 2
      CALL CALCK0(X,RESULT,JINT)
      BESEK0 = RESULT
      RETURN
C---------- Last line of BESEK0 ----------
      END
