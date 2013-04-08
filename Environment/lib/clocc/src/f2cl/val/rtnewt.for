C Contains a subroutine call with uninitiated arg
      FUNCTION RTNEWT(FUNCD,X1,X2,XACC)
      PARAMETER (JMAX=20)
      RTNEWT=.5*(X1+X2)
      DO 11 J=1,JMAX
        CALL FUNCD(RTNEWT,F,DF)
        DX=F/DF
        RTNEWT=RTNEWT-DX
        IF((X1-RTNEWT)*(RTNEWT-X2) < 0.)PAUSE 'jumped out of brackets'
        IF(ABS(DX) < XACC) RETURN
11    CONTINUE
      PAUSE 'RTNEWT exceeding maximum iterations'
      END
