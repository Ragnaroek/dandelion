C
C
      SUBROUTINE DOTEST(i,n)
      INTEGER J,K
      WRITE(*,8000)
C     Test simple incrementing loop
      DO 13 J=i,n
        K=J*J
      WRITE(*,8005),J,K
13    CONTINUE
      WRITE(*,9999),J
      WRITE(*,8050)
C     Test simple decrementing loop
      DO 14 J=N,I,-1
        K=J*J
        WRITE(*,9000),J,K
14    CONTINUE
      WRITE(*,9999),J
      WRITE(*,7000)
C     Test nested loops
      DO 16 J=I,N
        DO 15 K=I,N-2
        WRITE(*,8005),J,K
15    CONTINUE
16    CONTINUE
      WRITE(*,7050)J,K
      STOP
7000  FORMAT("NESTED LOOP"/)
7050  FORMAT("AT END OF NESTED LOOP J=",I3," K=",I3/)
8000  FORMAT("INCREASING LOOP"/)
8005  FORMAT("J=", I3, "K=",I3/)
8050  FORMAT("DECREASING LOOP"/)
9000  FORMAT("J=", I3, "K=",I3/)
9999  FORMAT("AT END OF LOOP J = ", I3,/)
      END


      PROGRAM PROG
      CALL DOTEST(1,4)
      END
