
      PROGRAM ALGTST
C------------------------------------------------------------------
C FORTRAN 77 program to test ALGAMA or DLGAMA
C
C   Method:
C
C      Accuracy tests compare function values against values
C      generated with the duplication formula.
C
C   Data required
C
C      None
C
C   Subprograms required from this package
C
C      MACHAR - An environmental inquiry program providing
C               information on the floating-point arithmetic
C               system.  Note that the call to MACHAR can
C               be deleted provided the following five
C               parameters are assigned the values indicated
C
C               IBETA  - The radix of the floating-point system
C               IT     - The number of base-ibeta digits in the
C                        significant of a floating-point number
C               EPS    - The smallest positive floating-point
C                        number such that 1.0+EPS .NE. 1.0
C               XMIN   - The smallest non-vanishing floating-point
C                        integral power of the radix
C               XMAX   - The largest finite floating-point number
C
C      REN(K) - A function subprogram returning random real
C               numbers uniformly distributed over (0,1)
C
C
C    Intrinsic functions required are:
C
C         ABS, ANINT, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs related
C              to the real gamma function", W. J. Cody,
C              submitted for publication.
C
C  Latest modification: March 9, 1992
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C--------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD
CS    REAL  ALGAMA,
      DOUBLE PRECISION DLGAMA,
     1    A,AIT,ALBETA,ALL9,B,BETA,CL,CONV,C1,C2,C3,DEL,EPS,EPSNEG,
     2    FUNC,HALF,ONE,P875,P3125,P625,P6875,REN,R6,R7,SIXTEN,TEN,
     3    TWO,U,V,W,X,XC,XL,XMAX,XMIN,XN,XP99,Y,Z,ZERO,ZZ
C------------------------------------------------------------------
C   C1 = 0.5 - LN(SQRT(PI))
C   C2 = LN(2)
C   C3 = LN(2) - 11/16
C------------------------------------------------------------------
CS    DATA C1,C2/-7.2364942924700087072E-2,6.9314718055994530942E-1/,
CS   1     C3,ZERO,HALF/5.6471805599453094172E-3,0.0E0,0.5E0/,
CS   2     ONE,TWO,TEN,SIXTEN/1.0E0,2.0E0,10.0E0,16.0E0/,
CS   3     P6875,P875,P3125,P625/0.6875E0,0.875E0,1.3125E0,1.625E0/,
CS   4     ALL9,XP99/-999.0E0,0.99E0/
      DATA C1,C2/-7.2364942924700087072D-2,6.9314718055994530942D-1/,
     1     C3,ZERO,HALF/5.6471805599453094172D-3,0.0D0,0.5D0/,
     2     ONE,TWO,TEN,SIXTEN/1.0D0,2.0D0,10.0D0,16.0D0/,
     3     P6875,P875,P3125,P625/0.6875D0,0.875D0,1.3125D0,1.625D0/,
     4     ALL9,XP99/-999.0D0,0.99D0/
      DATA IOUT/6/
C------------------------------------------------------------------
C  Statement functions for conversion
C------------------------------------------------------------------
CS    CONV(I) = REAL(I)
      CONV(I) = DBLE(I)
CS    FUNC(X) = ALGAMA(X)
      FUNC(X) = DLGAMA(X)
C------------------------------------------------------------------
C  Determine machine parameters and set constants
C------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      A = ZERO
      B = P875
      N = 2000
      XN = CONV(N)
      JT = 0
C-----------------------------------------------------------------
C  Determine largest argument for DLGAMA by iteration
C-----------------------------------------------------------------
      CL = XP99 * XMAX
      Z = -CL / ALL9
   80 ZZ = CL / (LOG(Z)-ONE)
      IF (ABS(ZZ/Z-ONE) .GT. (TWO*BETA*EPS)) THEN
         Z = ZZ
         GO TO 80
      END IF
      CL = ZZ
C-----------------------------------------------------------------
C  Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 3
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
C-----------------------------------------------------------------
C  Use duplication formula
C-----------------------------------------------------------------
            IF (J .NE. 3) THEN
                  IF (J .EQ. 1) THEN
                        Z = X + HALF
                        X = Z - HALF
                        Y = X + X
                     ELSE
                        X = X + X
                        X = X * HALF
                        Y = (X + X) - ONE
                        Z = X - HALF
                  END IF
                  U = FUNC(X)
                  W = (Y-HALF)-HALF
                  ZZ = ANINT(W*SIXTEN)/SIXTEN
                  W = W - ZZ
                  V = (((HALF-ZZ*P6875) - C1) - W*P6875)-C3*(W+ZZ)
                  V = ((V + FUNC(Y)) - FUNC(Z))
               ELSE
                  Z = X * HALF + HALF
                  Y = Z - HALF
                  X = Y + Y
                  U = FUNC(X)
                  V = (C1 + ((X-HALF)-HALF)*C2)+FUNC(Y)+FUNC(Z)-HALF
            END IF
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
            W = (U - V) / U
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C------------------------------------------------------------------
C  Gather and print statistics for test
C------------------------------------------------------------------
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE IF (J .EQ. 2) THEN
               WRITE (IOUT,1001)
            ELSE
               WRITE (IOUT,1002)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
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
C------------------------------------------------------------------
C  Initialize for next test
C------------------------------------------------------------------
         A = P3125
         B = P625
         IF (J .EQ. 2) THEN
               A = TWO + TWO
               B = TEN + TEN
         END IF
  300 CONTINUE
C-----------------------------------------------------------------
C  Special tests
C  First test with special arguments
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,1040)
      Z = EPS
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = HALF
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = ONE
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = TWO
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
C-----------------------------------------------------------------
C  Test of error returns
C-----------------------------------------------------------------
      WRITE (IOUT,1050)
      Z = XMIN
      WRITE (IOUT,1053) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = CL
      WRITE (IOUT,1053) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = -ONE
      WRITE (IOUT,1052) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = ZERO
      WRITE (IOUT,1052) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = XP99 * XMAX
      WRITE (IOUT,1052) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      WRITE (IOUT,1100)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of LGAMA(X) vs LN(2*SQRT(PI))-2X*LN(2)+',
     1    'LGAMA(2X)-LGAMA(X+1/2)'//)
 1001 FORMAT('1Test of LGAMA(X) vs LN(2*SQRT(PI))-(2X-1)*LN(2)+',
     1    'LGAMA(X-1/2)-LGAMA(2X-1)'//)
 1002 FORMAT('1Test of LGAMA(X) vs -LN(2*SQRT(PI))+X*LN(2)+',
     1    'LGAMA(X/2)+LGAMA(X/2+1/2)'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT('  LGAMA(X) was larger',I6,' times,'/
     1    14X,' agreed',I6,' times, and'/
     2    10X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1025 FORMAT('1Special Tests'//)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT(' LGAMA (',E13.6E3,') = ',E13.6E3//)
 1050 FORMAT('1Test of Error Returns'///)
 1052 FORMAT(' LGAMA will be called with the argument',E13.6E3,/
     1    ' This should trigger an error message'//)
 1053 FORMAT(' LGAMA will be called with the argument',E13.6E3,/
     1    ' This should not trigger an error message'//)
 1061 FORMAT(' LGAMA returned the value',E13.6E3///)
 1100 FORMAT(' This concludes the tests')
C---------- Last line of ALGAMA/DLGAMA test program ----------
      END
