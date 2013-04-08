CS    REAL FUNCTION BESI1(X)
      DOUBLE PRECISION FUNCTION BESI1(X)
C--------------------------------------------------------------------
C
C This long precision subprogram computes approximate values for
C   modified Bessel functions of the first kind of order one for
C   arguments ABS(ARG) .LE. XMAX  (see comments heading CALCI1).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1    X, RESULT
C--------------------------------------------------------------------
      JINT=1
      CALL CALCI1(X,RESULT,JINT)
      BESI1=RESULT
      RETURN
C---------- Last line of BESI1 ----------
      END
