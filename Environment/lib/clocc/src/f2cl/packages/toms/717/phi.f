C---------------------------------------------------
      DOUBLE PRECISION FUNCTION PHI(X, Y)
      DOUBLE PRECISION X, Y
      DOUBLE PRECISION ARG
      DOUBLE PRECISION HALF, SQ2P, XLOW, ZERO
      PARAMETER (HALF = 0.5D0, SQ2P = 0.91893853320467274D0,
     1           XLOW = -87.D0, ZERO = 0.D0)
      PHI = ZERO
      ARG = -HALF * X * X - SQ2P - Y
      IF (ARG .GT. XLOW) PHI = EXP(ARG)
      END
