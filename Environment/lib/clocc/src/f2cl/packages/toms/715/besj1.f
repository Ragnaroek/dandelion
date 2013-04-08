CS    REAL FUNCTION BESJ1(X)
      DOUBLE PRECISION FUNCTION BESJ1(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the first kind of order zero for arguments  |X| <= XMAX
C   (see comments heading CALJY1).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1   RESULT,X
C--------------------------------------------------------------------
      JINT=0
      CALL CALJY1(X,RESULT,JINT)
      BESJ1 = RESULT
      RETURN
C---------- Last card of BESJ1 ----------
      END
