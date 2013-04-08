      SUBROUTINE CHKDER(MDL, N, NPT, PT, R, RHO, RHO0, YN)
      INTEGER MDL(1), N, NPT
C     DOUBLE PRECISION PT(NPT) -- BUT NPT MAY BE 0
      DOUBLE PRECISION PT(1), R(N,20), YN(2,N)
      EXTERNAL RHO, RHO0
      EXTERNAL DV2NRM
      DOUBLE PRECISION DV2NRM
      INTEGER I, J
      DOUBLE PRECISION F, H, T
      REAL FOO(10), FAC
      DATA FOO/.1, -.1, .2, -.2, .4, -.4, .6, -.6, .8, -.9/, H/.001D0/
C
C *** BODY ***
C
      J = 1
      FAC = 1.0
      DO 10 I = 1, N
         T = FAC * FOO(J)
         R(I,1) = T
         R(I,10) = T + H
         J = J + 1
         IF (J .LE. 10) GO TO 10
                J = 1
                FAC = 10. * FAC
 10      CONTINUE
      CALL RHO0(MDL, N, PT, R, R(1,4), YN)
      CALL RHO0(MDL, N, PT, R(1,10), R(1,13), YN)
      DO 20 I = 1, N
         T = R(I,10) - R(I,1)
         IF (T .NE. 0.D0) T = 1.D0 / T
         R(I,20) = T
 20      CONTINUE
      CALL DV2AXY(N, R(1,13), -1.D0, R(1,4), R(1,13))
      CALL DV7VMP(N, R(1,13), R(1,13), R(1,20), 1)
      J = 1
      CALL RHO(0, F, N, J, PT, R, R(1,4), MDL, YN)
      CALL RHO(1, F, N, J, PT, R, R(1,4), MDL, YN)
      CALL DV2AXY(N, R(1,19), -1.D0, R(1,13), R)
      T = DV2NRM(N,R(1,19))/(DV2NRM(N,R(1,13)) + DV2NRM(N,R))
      WRITE(6,*) '1ST DERIV RELATIVE DIFFERENCE =', T
      IF (T .GT. .01) THEN
        WRITE(6,*) 'I   FD(I)   AN(I)'
        WRITE(6,'(I5,2G13.4)') (I, R(I,13), R(I,1), I = 1, N)
        END IF
      CALL RHO(0, F, N, J, PT, R(1,10), R(1,13), MDL, YN)
      CALL RHO(1, F, N, J, PT, R(1,10), R(1,13), MDL, YN)
      CALL DV2AXY(N, R(1,19), -1.D0, R, R(1,10))
      CALL DV7VMP(N, R(1,19), R(1,19), R(1,20), 1)
      CALL DV2AXY(N, R(1,13), -1.D0, R(1,19), R(1,4))
      T = DV2NRM(N,R(1,13))/(DV2NRM(N,R(1,4)) + DV2NRM(N,R(1,19)))
      WRITE(6,*) '2ND DERIV RELATIVE DIFFERENCE =', T
      IF (T .GT. .01) THEN
        WRITE(6,*) 'I   FD(I)   AN(I)'
        WRITE(6,'(I5,2G13.4)') (I, R(I,19), R(I,4), I = 1, N)
        END IF
 999  RETURN
      END
