      SUBROUTINE COFX4(X,AF,BF,CF)
C
C     SET COEFFICIENTS IN THE X-DIRECTION.
C
      AF = (X+1.)**2
      BF = 2.0*(X+1.)
      CF = -X
      RETURN
      END
