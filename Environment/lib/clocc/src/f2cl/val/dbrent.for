      FUNCTION DBRENT(AX,BX,CX,F,DF,TOL,XMIN)
      PARAMETER (ITMAX=100,ZEPS=1.0E-10)
      LOGICAL OK1,OK2
      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      FX=F(X)
      FV=FX
      FW=FX
      DX=DF(X)
      DV=DX
      DW=DX
      DO 11 ITER=1,ITMAX
        XM=0.5*(A+B)
        TOL1=TOL*ABS(X)+ZEPS
        TOL2=2.*TOL1
        IF(ABS(X-XM).LE.(TOL2-.5*(B-A))) GO TO 3
        IF(ABS(E).GT.TOL1) THEN
          D1=2.*(B-A)
          D2=D1
          IF(DW.NE.DX) D1=(W-X)*DX/(DX-DW)
          IF(DV.NE.DX) D2=(V-X)*DX/(DX-DV)
          U1=X+D1
          U2=X+D2
          OK1=((A-U1)*(U1-B).GT.0.).AND.(DX*D1.LE.0.)
          OK2=((A-U2)*(U2-B).GT.0.).AND.(DX*D2.LE.0.)
          OLDE=E
          E=D
          IF(.NOT.(OK1.OR.OK2))THEN
            GO TO 1
          ELSE IF (OK1.AND.OK2)THEN
            IF(ABS(D1).LT.ABS(D2))THEN
              D=D1
            ELSE
              D=D2
            ENDIF
          ELSE IF (OK1)THEN
            D=D1
          ELSE
            D=D2
          ENDIF
          IF(ABS(D).GT.ABS(0.5*OLDE))GO TO 1
          U=X+D
          IF(U-A.LT.TOL2 .OR. B-U.LT.TOL2) D=SIGN(TOL1,XM-X)
          GO TO 2
        ENDIF
1       IF(DX.GE.0.) THEN
          E=A-X
        ELSE
          E=B-X
        ENDIF
        D=0.5*E
2       IF(ABS(D).GE.TOL1) THEN
          U=X+D
          FU=F(U)
        ELSE
          U=X+SIGN(TOL1,D)
          FU=F(U)
          IF(FU.GT.FX)GO TO 3
        ENDIF
        DU=DF(U)
        IF(FU.LE.FX) THEN
          IF(U.GE.X) THEN
            A=X
          ELSE
            B=X
          ENDIF
          V=W
          FV=FW
          DV=DW
          W=X
          FW=FX
          DW=DX
          X=U
          FX=FU
          DX=DU
        ELSE
          IF(U.LT.X) THEN
            A=U
          ELSE
            B=U
          ENDIF
          IF(FU.LE.FW .OR. W.EQ.X) THEN
            V=W
            FV=FW
            DV=DW
            W=U
            FW=FU
            DW=DU
          ELSE IF(FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
            V=U
            FV=FU
            DV=DU
          ENDIF
        ENDIF
11    CONTINUE
      PAUSE 'DBRENT exceeded maximum iterations'
3     XMIN=X
      DBRENT=FX
      RETURN
      END
