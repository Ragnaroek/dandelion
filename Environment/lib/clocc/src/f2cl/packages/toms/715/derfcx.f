CS    REAL FUNCTION ERFCX(X)
      DOUBLE PRECISION FUNCTION DERFCX(X)
C------------------------------------------------------------------
C
C This subprogram computes approximate values for exp(x*x) * erfc(x).
C   (see comments heading CALERF).
C
C   Author/date: W. J. Cody, March 30, 1987
C
C------------------------------------------------------------------
      INTEGER JINT
CS    REAL             X, RESULT
      DOUBLE PRECISION X, RESULT
C------------------------------------------------------------------
      JINT = 2
      CALL CALERF(X,RESULT,JINT)
CS    ERFCX = RESULT
      DERFCX = RESULT
      RETURN
C---------- Last card of DERFCX ----------
      END
