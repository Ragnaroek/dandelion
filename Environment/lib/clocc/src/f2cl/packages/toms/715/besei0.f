CS    REAL FUNCTION BESEI0(X)
      DOUBLE PRECISION FUNCTION BESEI0(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the first kind of order zero
C   multiplied by EXP(-ABS(X)), where EXP is the
C   exponential function, ABS is the absolute value, and X
C   is any argument.
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1    X, RESULT
C--------------------------------------------------------------------
      JINT=2
      CALL CALCI0(X,RESULT,JINT)
      BESEI0=RESULT
      RETURN
C---------- Last line of BESEI0 ----------
      END
