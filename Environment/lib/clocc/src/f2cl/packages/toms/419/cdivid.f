      SUBROUTINE CDIVID(AR,AI,BR,BI,CR,CI)                              CDIV4490
C COMPLEX DIVISION C = A/B, AVOIDING OVERFLOW.
      DOUBLE PRECISION AR,AI,BR,BI,CR,CI,R,D,T,INFIN,DABS
      IF (BR .NE. 0.0D0  .OR. BI .NE. 0.0D0) GO TO 10
C DIVISION BY ZERO, C = INFINITY.
          CALL MCON (T,INFIN,T,T)
          CR = INFIN
          CI = INFIN
          RETURN
   10 IF (DABS(BR) .GE. DABS(BI)) GO TO 20
          R = BR/BI
          D = BI+R*BR
          CR = (AR*R+AI)/D
          CI = (AI*R-AR)/D
          RETURN
   20 R = BI/BR
      D = BR+R*BI
      CR = (AR+AI*R)/D
      CI = (AI-AR*R)/D
      RETURN
      END
