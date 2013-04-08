      SUBROUTINE DX4(U,IDMN,I,J,UXXX,UXXXX)
C
C     THIS PROGRAM COMPUTES SECOND ORDER FINITE DIFFERENCE
C     APPROXIMATIONS TO THE THIRD AND FOURTH X
C     PARTIAL DERIVATIVES OF U AT THE (I,J) MESH POINT
C
      COMMON /SPL4/   KSWX       ,KSWY       ,K          ,L          ,
     1                AIT        ,BIT        ,CIT        ,DIT        ,
     2                MIT        ,NIT        ,IS         ,MS         ,
     3                JS         ,NS         ,DLX        ,DLY        ,
     4                TDLX3      ,TDLY3      ,DLX4       ,DLY4
      DIMENSION       U(IDMN,1)
      IF (I.GT.2 .AND. I.LT.(K-1)) GO TO  50
      IF (I .EQ. 1) GO TO  10
      IF (I .EQ. 2) GO TO  30
      IF (I .EQ. K-1) GO TO  60
      IF (I .EQ. K) GO TO  80
C
C     COMPUTE PARTIAL DERIVATIVE APPROXIMATIONS AT X=A
C
   10 IF (KSWX .EQ. 1) GO TO  20
      UXXX = (-5.0*U(1,J)+18.0*U(2,J)-24.0*U(3,J)+14.0*U(4,J)-
     1                                               3.0*U(5,J))/(TDLX3)
      UXXXX = (3.0*U(1,J)-14.0*U(2,J)+26.0*U(3,J)-24.0*U(4,J)+
     1                                      11.0*U(5,J)-2.0*U(6,J))/DLX4
      RETURN
C
C     PERIODIC AT X=A
C
   20 UXXX = (-U(K-2,J)+2.0*U(K-1,J)-2.0*U(2,J)+U(3,J))/(TDLX3)
      UXXXX = (U(K-2,J)-4.0*U(K-1,J)+6.0*U(1,J)-4.0*U(2,J)+U(3,J))/DLX4
      RETURN
C
C     COMPUTE PARTIAL DERIVATIVE APPROXIMATIONS AT X=A+DLX
C
   30 IF (KSWX .EQ. 1) GO TO  40
      UXXX = (-3.0*U(1,J)+10.0*U(2,J)-12.0*U(3,J)+6.0*U(4,J)-U(5,J))/
     1       TDLX3
      UXXXX = (2.0*U(1,J)-9.0*U(2,J)+16.0*U(3,J)-14.0*U(4,J)+6.0*U(5,J)-
     1                                                      U(6,J))/DLX4
      RETURN
C
C     PERIODIC AT X=A+DLX
C
   40 UXXX = (-U(K-1,J)+2.0*U(1,J)-2.0*U(3,J)+U(4,J))/(TDLX3)
      UXXXX = (U(K-1,J)-4.0*U(1,J)+6.0*U(2,J)-4.0*U(3,J)+U(4,J))/DLX4
      RETURN
C
C     COMPUTE PARTIAL DERIVATIVE APPROXIMATIONS ON THE INTERIOR
C
   50 CONTINUE
      UXXX = (-U(I-2,J)+2.0*U(I-1,J)-2.0*U(I+1,J)+U(I+2,J))/TDLX3
      UXXXX = (U(I-2,J)-4.0*U(I-1,J)+6.0*U(I,J)-4.0*U(I+1,J)+U(I+2,J))/
     1        DLX4
      RETURN
C
C     COMPUTE PARTIAL DERIVATIVE APPROXIMATIONS AT X=B-DLX
C
   60 IF (KSWX .EQ. 1) GO TO  70
      UXXX = (U(K-4,J)-6.0*U(K-3,J)+12.0*U(K-2,J)-10.0*U(K-1,J)+
     1                                                 3.0*U(K,J))/TDLX3
      UXXXX = (-U(K-5,J)+6.0*U(K-4,J)-14.0*U(K-3,J)+16.0*U(K-2,J)-
     1                                     9.0*U(K-1,J)+2.0*U(K,J))/DLX4
      RETURN
C
C     PERIODIC AT X=B-DLX
C
   70 UXXX = (-U(K-3,J)+2.0*U(K-2,J)-2.0*U(1,J)+U(2,J))/TDLX3
      UXXXX = (U(K-3,J)-4.0*U(K-2,J)+6.0*U(K-1,J)-4.0*U(1,J)+U(2,J))/
     1        DLX4
      RETURN
C
C     COMPUTE PARTIAL DERIVATIVE APPROXIMATIONS AT X=B
C
   80 UXXX = -(3.0*U(K-4,J)-14.0*U(K-3,J)+24.0*U(K-2,J)-18.0*U(K-1,J)+
     1                                                 5.0*U(K,J))/TDLX3
      UXXXX = (-2.0*U(K-5,J)+11.0*U(K-4,J)-24.0*U(K-3,J)+26.0*U(K-2,J)-
     1                                    14.0*U(K-1,J)+3.0*U(K,J))/DLX4
      RETURN
      END
