CS    REAL FUNCTION BESK0(X)
      DOUBLE PRECISION FUNCTION BESK0(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order zero
C   for arguments 0.0 .LT. ARG .LE. XMAX (see comments heading
C   CALCK0).
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
      JINT = 1
      CALL CALCK0(X,RESULT,JINT)
      BESK0 = RESULT
      RETURN
C---------- Last line of BESK0 ----------
      END
