CS    REAL FUNCTION BESK1(X)
      DOUBLE PRECISION FUNCTION BESK1(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order one
C   for arguments  XLEAST .LE. ARG .LE. XMAX.
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1    X, RESULT
C--------------------------------------------------------------------
      JINT = 1
      CALL CALCK1(X,RESULT,JINT)
      BESK1 = RESULT
      RETURN
C---------- Last line of BESK1 ----------
      END
