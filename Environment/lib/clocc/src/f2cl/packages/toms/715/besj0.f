CS    REAL FUNCTION BESJ0(X)
      DOUBLE PRECISION FUNCTION BESJ0(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the first kind of order zero for arguments  |X| <= XMAX
C   (see comments heading CALJY0).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL  X, RESULT
      DOUBLE PRECISION  X, RESULT
C--------------------------------------------------------------------
      JINT=0
      CALL CALJY0(X,RESULT,JINT)
      BESJ0 = RESULT
      RETURN
C---------- Last line of BESJ0 ----------
      END
