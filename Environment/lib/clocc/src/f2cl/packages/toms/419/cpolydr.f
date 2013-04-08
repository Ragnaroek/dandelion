C     ALGORITHM 419 COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN COMM. ACM, VOL. 15, NO. 02,
C     P. 097.
      PROGRAM CPOLYDR
C
C              DRIVER TO TEST CPOLY
C
C
      DIMENSION MULT(50)
      LOGICAL FLAG(50)
      REAL*8 FZR(50),FZI(50),BND(50)
      LOGICAL FAIL
      DOUBLE PRECISION  P(50),PI(50),ZR(50),ZI(50)
      WRITE(6,100)
  100 FORMAT('1EXAMPLE 1.  POLYNOMIAL WITH ZEROS 1,2,...,10.')
      P(1)=1
      P(2)=-55
      P(3)=1320
      P(4)=-18150
      P(5)=157773
      P(6)=-902055
      P(7) = 3416930
      P(8)=-8409500
      P(9)=12753576
      P(10)=-10628640
      P(11)=3628800
      DO 10 I=1,11
   10 PI(I)=0
      CALL PRTC(11,P,PI)
      CALL CPOLY(P,PI,10,ZR,ZI,FAIL)
      IF(FAIL) GO TO 95
      CALL PRTZ (10,ZR,ZI)
    2 WRITE(6,101)
  101 FORMAT('1EXAMPLE 2. ZEROS ON IMAGINARY AXIS DEGREE 3.')
      P(1)=1
      P(2)=0
      P(3)=-10001.0001D0
      P(4)=0
      PI(1)=0
      PI(2)=-10001.0001D0
      PI(3)=0
      PI(4)=1
      CALL PRTC(4,P,PI)
      CALL CPOLY(P,PI,3,ZR,ZI,FAIL)
      IF (FAIL) GO TO 96
      CALL PRTZ (3,ZR,ZI)
    3 WRITE(6,102)
  102 FORMAT('1EXAMPLE 3. ZEROS AT 1+I,1/2*(1+I)....1/(2**-9)*(1+I)')
      P(1)=1.0
      P(2)=-1.998046875
      P(3)=0.0
      P(4)=.7567065954208374D0
      P(5)=-.2002119533717632D0
      P(6)=1.271507365163416D-2
      P(7)=0
      P(8)=-1.154642632172909D-5
      P(9)=1.584803612786345D-7
      P(10)=-4.652065399568528D-10
      P(11)=0
      PI(1)=0
      PI(2)=P(2)
      PI(3)=2.658859252929688D0
      PI(4)=-7.567065954208374D-1
      PI(5)=0
      PI(6)=P(6)
      PI(7)=-7.820779428584501D-4
      PI(8)=-P(8)
      PI(9)=0
      PI(10)=P(10)
      PI(11)=9.094947017729282D-13
      CALL PRTC(11,P,PI)
      CALL CPOLY(P,PI,10,ZR,ZI,FAIL)
      IF (FAIL) GO TO 97
      CALL PRTZ(10,ZR,ZI)
    4 WRITE(6,103)
  103 FORMAT('1EXAMPLE 4. MULTIPLE ZEROS')
      P(1)=1
      P(2)=-10
      P(3)=3
      P(4)=284
      P(5)=-1293
      P(6)=2374
      P(7)=-1587
      P(8)=-920
      P(9)=2204
      P(10)=-1344
      P(11)=288
      PI(1)=0
      PI(2)=-10
      PI(3)=100
      PI(4)=-334
      PI(5)=200
      PI(6)=1394
      PI(7) =-3836
      PI(8)=4334
      PI(9)=-2352
      PI(10)=504
      PI(11)=0
      CALL PRTC(11,P,PI)
      CALL CPOLY(P,PI,10,ZR,ZI,FAIL)
      IF (FAIL) GO TO 98
      CALL PRTZ(10,ZR,ZI)
    5 WRITE(6,104)
  104 FORMAT('1EXAMPLE 5. 12 ZEROS EVENLY DISTRIBUTE ON A CIRCLE OF RADI
     -US 1. CENTERED AT 0+2I.')
      P(1)=1
      P(2)=0
      P(3)=-264
      P(4)=0
      P(5)=7920
      P(6)=0
      P(7)=-59136
      P(8)=0
      P(9)=126720
      P(10)=0
      P(11)=-67584
      P(12)=0
      P(13)=4095
      PI(1)=0
      PI(2)=-24
      PI(3)=0
      PI(4)=1760
      PI(5)=0
      PI(6)=-25344
      PI(7)=0
      PI(8)=101376
      PI(9)=0
      PI(10)=-112640
      PI(11)=0
      PI(12)=24576
      PI(13)=0
      CALL PRTC(13,P,PI)
      CALL CPOLY(P,PI,12,ZR,ZI,FAIL)
      IF(FAIL) GO TO 99
      CALL PRTZ(12,ZR,ZI)
      RETURN
   95 WRITE(6,105)
      GO TO 2
   96 WRITE(6,105)
      GO TO 3
   97 WRITE(6,105)
      GO TO 4
   98 WRITE(6,105)
      GO TO 5
   99 WRITE(6,105)
      RETURN
  105 FORMAT(//' CPOLY HAS FAILED ON THIS EXAMPLE')
      END
