CS    REAL FUNCTION BESEK1(X)
      DOUBLE PRECISION FUNCTION BESEK1(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order one
C   multiplied by the exponential function, for arguments
C   XLEAST .LE. ARG .LE. XMAX.
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1    X, RESULT
C--------------------------------------------------------------------
      JINT = 2
      CALL CALCK1(X,RESULT,JINT)
      BESEK1 = RESULT
      RETURN
C---------- Last line of BESEK1 ----------
      END
