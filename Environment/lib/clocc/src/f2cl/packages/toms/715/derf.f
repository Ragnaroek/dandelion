CS    REAL FUNCTION ERF(X)
      DOUBLE PRECISION FUNCTION DERF(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for erf(x).
C   (see comments heading CALERF).
C
C   Author/date: W. J. Cody, January 8, 1985
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL             X, RESULT
      DOUBLE PRECISION X, RESULT
C------------------------------------------------------------------
      JINT = 0
      CALL CALERF(X,RESULT,JINT)
CS    ERF = RESULT
      DERF = RESULT
      RETURN
C---------- Last card of DERF ----------
      END
