CS    REAL FUNCTION BESI0(X)
      DOUBLE PRECISION FUNCTION BESI0(X)
C--------------------------------------------------------------------
C
C This long precision subprogram computes approximate values for
C   modified Bessel functions of the first kind of order zero for
C   arguments ABS(ARG) .LE. XMAX  (see comments heading CALCI0).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1    X, RESULT
C--------------------------------------------------------------------
      JINT=1
      CALL CALCI0(X,RESULT,JINT)
      BESI0=RESULT
      RETURN
C---------- Last line of BESI0 ----------
      END
