CS    REAL FUNCTION BESY1(X)
      DOUBLE PRECISION FUNCTION BESY1(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the second kind of order zero for arguments 0 < X <= XMAX
C   (see comments heading CALJY1).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1   RESULT,X
C--------------------------------------------------------------------
      JINT=1
      CALL CALJY1(X,RESULT,JINT)
      BESY1 = RESULT
      RETURN
C---------- Last card of BESY1 ----------
      END
