CS    REAL FUNCTION BESY0(X)
      DOUBLE PRECISION FUNCTION BESY0(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the second kind of order zero for arguments 0 < X <= XMAX
C   (see comments heading CALJY0).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL  X, RESULT
      DOUBLE PRECISION  X, RESULT
C--------------------------------------------------------------------
      JINT=1
      CALL CALJY0(X,RESULT,JINT)
      BESY0 = RESULT
      RETURN
C---------- Last line of BESY0 ----------
      END
