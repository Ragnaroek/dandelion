C Contains a jump out of a do loop to end
      FUNCTION RTFLSP(FUNC,X1,X2,XACC)
      PARAMETER (MAXIT=30)
      FL=FUNC(X1)
      FH=FUNC(X2)
      IF(FL*FH > 0.) PAUSE 'Root must be bracketed for false position.'
      IF(FL < 0.)THEN
        XL=X1
        XH=X2
      ELSE
        XL=X2
        XH=X1
        SWAP=FL
        FL=FH
        FH=SWAP
      ENDIF
      DX=XH-XL
      DO 11 J=1,MAXIT
        RTFLSP=XL+DX*FL/(FL-FH)
        F=FUNC(RTFLSP)
        IF(F < 0.) THEN
          DEL=XL-RTFLSP
          XL=RTFLSP
          FL=F
        ELSE
          DEL=XH-RTFLSP
          XH=RTFLSP
          FH=F
        ENDIF
        DX=XH-XL
        IF(ABS(DEL) < XACC or F .EQ. 0.)RETURN
11    CONTINUE
      PAUSE 'RTFLSP exceed maximum iterations'
      END
