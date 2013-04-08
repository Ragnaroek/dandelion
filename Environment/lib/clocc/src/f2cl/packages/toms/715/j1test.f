      PROGRAM J1TEST
C--------------------------------------------------------------------
C  Fortran 77 program to test BESJ1
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following six
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest integer such that
C                          FLOAT(IBETA)**MAXEXP causes overflow
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest positive normalized
C                          floating-point power of the radix
C                 XMAX   - the largest finite floating-point
C                          number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, FLOAT, LOG, MAX, SIGN, SQRT
C
C  Reference: "The use of Taylor series to test accuracy of
C              function programs", W. J. Cody and L. Stoltz,
C              submitted for publication.
C
C  Latest modification: March 13, 1992
C
C  Authors: W. J. Cody
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C--------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,II,III,IOUT,IRND,IT,J,JJ,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BESJ0,BESJ1,BETA,BJ0,BJ0P,BJ1,BJ1P,
     2   CJ0,CJ1,CONV,D,DEL,DELTA,EIGHT,ELEV,EPS,EPSNEG,FOUR,ONE,
     3   REN,R6,R7,SIXTEN,SUM,T,TERM,THRTEN,TWENTY,TWO,TWO56,W,X,
     4   XI,XL,XMAX,XMIN,XM,XN,X1,Y,YINV,YSQ,YX,Z,ZERO,ZZ
      DIMENSION BJ0P(6,10),BJ1P(6,10),XI(2),YX(2)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
      DATA IOUT/6/
CS    DATA ZERO,ONE,FOUR,DELTA/0.0E0,1.0E0,4.0E0,0.0625E0/,
CS   1   EIGHT,TWENTY,ALL9,TWO56/8.0E0,20.0E0,-999.0E0,256.0E0/,
CS   2   TWO,THRTEN,SIXTEN,ELEV/2.0E0,13.0E0,16.0E0,11.0E0/
      DATA ZERO,ONE,FOUR,DELTA/0.0D0,1.0D0,4.0D0,0.0625D0/,
     1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
     2   TWO,THRTEN,SIXTEN,ELEV/2.0D0,13.0D0,16.0D0,11.0D0/
C--------------------------------------------------------------------
C  Coefficients for Taylor expansion
C--------------------------------------------------------------------
CS    DATA BJ0P/1.99584E7,-2.58552E6,1.16235E5,-2.775E3,4.5E1,-1.0E0,
CS   1           0.0E0,-1.8144E6,2.3688E5,-1.0845E4,2.7E2,-5.0E0,
CS   2           1.8144E5,-2.394E4,1.125E3,-30.0E0,1.0E0,0.0E0,
CS   3           0.0E0,-2.016E4,2.7E3,-1.32E2,4.0E0,0.0E0,
CS   4           2.52E3,-3.45E2,18.0E0,-1.0E0,0.0E0,0.0E0,
CS   5           0.0E0,-3.6E2,51.0E0,-3.0E0,0.0E0,0.0E0,
CS   6           60.0E0,-9.0E0,1.0E0,0.0E0,0.0E0,0.0E0,
CS   7           0.0E0,-12.0E0,2.0E0,0.0E0,0.0E0,0.0E0,
CS   8           3.0E0,-1.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   9           0.0E0,-1.0E0,0.0E0,0.0E0,0.0E0,0.0E0/
CS    DATA BJ1P/-3.99168E7,1.016064E7,-6.7095E5,2.067E4,-3.9E2,6.0E0,
CS   1           3.6288E6,-9.2736E5,6.201E4,-1.965E3,40.0E0,-1.0E0,
CS   2          -3.6288E5,9.324E4,-6.345E3,2.1E2,-5.0E0,0.0E0,
CS   3           4.032E4,-1.044E4,7.29E2,-26.0E0,1.0E0,0.0E0,
CS   4          -5.04E3,1.32E3,-96.0E0,4.0E0,0.0E0,0.0E0,
CS   5           7.2E2,-1.92E2,15.0E0,-1.0E0,0.0E0,0.0E0,
CS   6          -1.2E2,33.0E0,-3.0E0,0.0E0,0.0E0,0.0E0,
CS   7           24.0E0,-7.0E0,1.0E0,0.0E0,0.0E0,0.0E0,
CS   8          -6.0E0,2.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   9           2.0E0,-1.0E0,0.0E0,0.0E0,0.0E0,0.0E0/
      DATA BJ0P/1.99584D7,-2.58552D6,1.16235D5,-2.775D3,4.5D1,-1.0D0,
     1           0.0D0,-1.8144D6,2.3688D5,-1.0845D4,2.7D2,-5.0D0,
     2           1.8144D5,-2.394D4,1.125D3,-30.0D0,1.0D0,0.0D0,
     3           0.0D0,-2.016D4,2.7D3,-1.32D2,4.0D0,0.0D0,
     4           2.52D3,-3.45D2,18.0D0,-1.0D0,0.0D0,0.0D0,
     5           0.0D0,-3.6D2,51.0D0,-3.0D0,0.0D0,0.0D0,
     6           60.0D0,-9.0D0,1.0D0,0.0D0,0.0D0,0.0D0,
     7           0.0D0,-12.0D0,2.0D0,0.0D0,0.0D0,0.0D0,
     8           3.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     9           0.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      DATA BJ1P/-3.99168D7,1.016064D7,-6.7095D5,2.067D4,-3.9D2,6.0D0,
     1           3.6288D6,-9.2736D5,6.201D4,-1.965D3,40.0D0,-1.0D0,
     2          -3.6288D5,9.324D4,-6.345D3,2.1D2,-5.0D0,0.0D0,
     3           4.032D4,-1.044D4,7.29D2,-26.0D0,1.0D0,0.0D0,
     4          -5.04D3,1.32D3,-96.0D0,4.0D0,0.0D0,0.0D0,
     5           7.2D2,-1.92D2,15.0D0,-1.0D0,0.0D0,0.0D0,
     6          -1.2D2,33.0D0,-3.0D0,0.0D0,0.0D0,0.0D0,
     7           24.0D0,-7.0D0,1.0D0,0.0D0,0.0D0,0.0D0,
     8          -6.0D0,2.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     9           2.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
C--------------------------------------------------------------------
C  Zeroes of J1
C--------------------------------------------------------------------
CS    DATA XI/981.0E0,1796.0E0/
CS    DATA YX(1)/-1.3100 39300 13279 72376 E-4/,
CS   1     YX(2)/ 1.1503 46070 23016 98285 E-5/
      DATA XI/981.0D0,1796.0D0/
      DATA YX(1)/-1.3100393001327972376D-4/,
     1     YX(2)/ 1.1503460702301698285D-5/
C--------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C--------------------------------------------------------------------
CS    CONV(NDUM) = REAL(NDUM)
      CONV(NDUM) = DBLE(NDUM)
C--------------------------------------------------------------------
C  Determine machine parameters and set constants
C--------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
      B = ZERO
C--------------------------------------------------------------------
C  Random argument accuracy tests (based on a Taylor expansion)
C--------------------------------------------------------------------
      DO 300 J = 1, 4
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               B = FOUR
            ELSE IF (J .EQ. 3) THEN
               B = EIGHT
            ELSE
               B = TWENTY
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            IF (J .EQ. 1) THEN
C--------------------------------------------------------------------
C  Use traditional Maclaurin series for small arguments.
C--------------------------------------------------------------------
                  Y = X/TWO
                  SUM = Y
                  XM = THRTEN
                  DO 100 II = 1,12
                     SUM = SUM*Y/XM
                     XM = XM-ONE
                     SUM = (ONE - SUM/XM)*Y
  100             CONTINUE
                  ZZ = SUM
                  Z = BESJ1(X)
               ELSE
C--------------------------------------------------------------------
C  Use local Taylor series elsewhere.  First, purify arguments.
C--------------------------------------------------------------------
                  Y = X - DELTA
                  W = SIXTEN * Y
                  Y = (W + Y) - W
                  X = Y + DELTA
                  SUM = ZERO
                  TERM = ZERO
                  BJ1 = BESJ1(Y)
                  Z = BESJ1(X)
                  D = DELTA
                  IF (ABS(Z) .LT. ABS(BJ1)) THEN
                     CJ1 = X
                     X = Y
                     Y = CJ1
                     CJ1 = BJ1
                     BJ1 = Z
                     Z = CJ1
                     D = -D
                  END IF
                  BJ0 = BESJ0(Y)
                  YINV = ONE/Y
                  YSQ = ONE/(Y*Y)
                  XM = ELEV
C--------------------------------------------------------------------
C  Evaluate (12-II)th derivative at Y.
C--------------------------------------------------------------------
                  DO 170 II = 1, 10
                     CJ0 = BJ0P(1,II)
                     CJ1 = BJ1P(1,II)
                     JJ = (12-II)/2 + 1
                     DO 160  III = 2, JJ
                        CJ0 = CJ0 * YSQ + BJ0P(III,II)
                        CJ1 = CJ1 * YSQ + BJ1P(III,II)
  160                CONTINUE
                     IF ((II/2)*2 .EQ. II) THEN
                           CJ0 = CJ0 * YINV
                        ELSE
                           CJ1 = CJ1 * YINV
                     ENDIF
                     TERM = CJ0*BJ0 + CJ1*BJ1
                     SUM = (SUM + TERM) * D/XM
                     XM = XM - ONE
  170             CONTINUE
                  SUM = (SUM + BJ0 - BJ1*YINV)*D + BJ1
                  ZZ = SUM
            END IF
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
               R6 = W
               X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C--------------------------------------------------------------------
C  Process and output statistics
C--------------------------------------------------------------------
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
C--------------------------------------------------------------------
C  Special tests
C--------------------------------------------------------------------
      WRITE (IOUT,1030)
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 2
         X = XI(I)/TWO56
         Y = BESJ1(X)
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) THEN
               W = LOG(ABS(T))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
C--------------------------------------------------------------------
C  Test of error returns
C--------------------------------------------------------------------
      WRITE (IOUT,1033)
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESJ1(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      STOP
C--------------------------------------------------------------------
 1000 FORMAT('1Test of J1(X) VS Maclaurin expansion'  //)
 1001 FORMAT('1Test of J1(X) VS local Taylor expansion'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 '(',F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(J1(X)) was larger',I6,' times', /
     1     15X,' agreed',I6,' times, and'/
     1   11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1  F7.2/4X,'occurred for X =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near zeros'//10X,'X',15X,'BESJ1(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10E3,E25.15E3,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'///)
 1034 FORMAT(' J1 will be called with the argument ',E17.10E3/
     1     ' This may stop execution.'//)
 1036 FORMAT(' J1 returned the value',E25.17E3/)
 1100 FORMAT(' This concludes the tests.')
C---------- Last card of BESJ1 test program ----------
      END
