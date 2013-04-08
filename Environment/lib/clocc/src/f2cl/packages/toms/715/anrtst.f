      PROGRAM ANRTST
C------------------------------------------------------------------
C FORTRAN 77 program to test ANORM, the normal probability integral.
C
C  Method:
C
C     Accuracy test compares function values against local Taylor's
C     series expansions.  Derivatives for anorm(x) are expressed as
C     repeated integrals of erfc(-x/sqrt(2)).  These are generated
C     from the recurrence relation using a technique due to Gautschi
C     (see references).
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C              IBETA  - The radix of the floating-point system
C              IT     - The number of base-ibeta digits in the
C                       significant of a floating-point number
C              EPS    - The smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - The smallest non-vanishing floating-point
C                       integral power of the radix
C              XMAX   - The largest finite floating-point number
C
C     REN(K) - A function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  References: "Performance evaluation of programs for the error
C               and complementary error functions", W. J. Cody,
C               TOMS 16, 1990, pp. 29-37.
C
C              "Evaluation of the repeated integrals of the coerror
C               function", W. Gautschi, TOMS 3, 1977, pp. 240-252.
C
C  Latest modification: March 15, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,N0,N1
CS    REAL  ANORM,
      DOUBLE PRECISION ANORM,
     1    A,AIT,ALBETA,B,BETA,C,CONV,C1,C2,DEL,EPS,EPSCON,EPSNEG,FF,
     2    F0,HALF,ONE,R,REN,ROOT32,R1,R6,R7,SC,SIXTEN,SQRTWO,THRESH,
     3    TEN,TWO,U,V,W,X,XBIG,XC,XL,XMAX,XMIN,XN,XN1,X99,Y,Z,ZERO
      DIMENSION R1(500)
C------------------------------------------------------------------
C   C1 = 1/sqrt(pi)
C------------------------------------------------------------------
CS    DATA HALF,ZERO,ONE,TWO,TEN/0.5E0,0.0E0,1.0E0,2.0E0,10.0E0/,
CS   1     SIXTEN,THRESH,X99/16.0E0,0.66291E0,-999.0E0/,
CS   2     C1/5.6418958354775628695E-1/,ROOT32/-5.65685E0/,
CS   3     SQRTWO/7.0710678118654752440E-1/
      DATA HALF,ZERO,ONE,TWO,TEN/0.5D0,0.0D0,1.0D0,2.0D0,10.0D0/,
     1     SIXTEN,THRESH,X99/16.0D0,0.66291D0,-999.0D0/,
     2     C1/5.6418958354775628695D-1/,ROOT32/-5.65685D0/,
     3     SQRTWO/7.0710678118654752440D-1/
      DATA IOUT/6/
C------------------------------------------------------------------
C  Statement functions for conversion
C------------------------------------------------------------------
CS    CONV(I) = REAL(I)
      CONV(I) = DBLE(I)
C------------------------------------------------------------------
C  Determine machine parameters and set constants
C------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      C = ABS(AIT*ALBETA) + TEN
      A = -THRESH
      B = THRESH
      N = 2000
      XN = CONV(N)
      JT = 0
      N0 = (IBETA/2)*(IT+5)/6+4
C-----------------------------------------------------------------
C  Determine largest argument for ANORM test by Newton iteration
C-----------------------------------------------------------------
      C2 = LOG(XMIN/C1/SQRTWO)
      XBIG = SQRT(-C2)
   50 X = XBIG
         F0 = X*X
         FF = F0 *HALF + ONE/F0 + LOG(X) + C2
         F0 = X + ONE/X - TWO/(X*F0-X)
         XBIG = X - FF/F0
         IF (ABS(X-XBIG)/X .GT. TEN*EPS) GO TO 50
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
            IF (J .EQ. 1) THEN
C-----------------------------------------------------------------
C  Test anorm against double series expansion
C-----------------------------------------------------------------
                  F0 = CONV(N0)
                  FF = F0+F0+ONE
                  W = X*X
                  Z = W*HALF
                  U = ZERO
                  V = ZERO
                  DO 60 K = 1, N0
                     U = -Z/F0*(ONE+U)
                     V = W/FF*(ONE+V)
                     F0 = F0 - ONE
                     FF = FF - TWO
   60             CONTINUE
                  V = HALF + SQRTWO*C1*X*(((U*V+(U+V))+HALF)+HALF)
                  U = ANORM(X)
               ELSE
C-----------------------------------------------------------------
C  Test anorm against expansion in repeated
C   integrals of the coerror function.
C-----------------------------------------------------------------
                  Y = X - HALF
                  X = Y + HALF
                  U = ANORM(X)
                  Z = -Y/SQRT(TWO)
                  R = ZERO
                  IF (Z .LE. ONE) THEN
                        N0 = 499
                     ELSE
                        N0 = MIN(499,INT(C/(ABS(LOG(Z)))))
                  END IF
                  N1 = N0
                  XN1 = CONV(N1+1)
                  DO 100 K=1,N0
                     R = HALF/(Z+XN1*R)
                     R1(N1) = R
                     N1 = N1 - 1
                     XN1 = XN1 - ONE
  100             CONTINUE
                  FF = C1/(Z+R1(1))
                  F0 = ANORM(Y)*EXP((-Y-HALF*HALF)*HALF)
                  SC = F0/FF
C-----------------------------------------------------------------
C  Scale things to avoid premature underflow
C-----------------------------------------------------------------
                  EPSCON = F0
                  FF = SIXTEN*FF/EPS
                  DO 110 N1=1,N0
                     FF = R1(N1)*FF
                     R1(N1) = FF * SC
                     IF (R1(N1) .LT. EPSCON ) THEN
                        K = N1
                        GO TO 111
                     END IF
  110             CONTINUE
  111             V = SQRTWO*R1(K)
                  DO 120 N1 = 1, K-1
                     V = (V + R1(K-N1))*SQRTWO
  120             CONTINUE
C-----------------------------------------------------------------
C  Remove scaling here
C-----------------------------------------------------------------
                  V = V*EPS/SIXTEN + F0
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
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1
         WRITE (IOUT,1015) K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
C------------------------------------------------------------------
C  Initialize for next test
C------------------------------------------------------------------
         IF (J .EQ. 1) THEN
               B = A
               A = ROOT32
            ELSE
               B = A
               A = -AINT(XBIG*SIXTEN)/SIXTEN+HALF
         END IF
  300 CONTINUE
C-----------------------------------------------------------------
C  Special tests
C  First check values for positive arguments.
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,1030)
      DEL = TEN
      DO 350 I = 1, 10
         X = REN(JT) * DEL
         U = ANORM(-X)
         A = U + ANORM(X)
         Y = (A - HALF) - HALF
         WRITE (IOUT,1032) X, U, Y
  350 CONTINUE
C-----------------------------------------------------------------
C  Test with special arguments
C-----------------------------------------------------------------
      WRITE (IOUT,1040)
      Z = XMAX
      Y = ANORM(Z)
      WRITE (IOUT,1041) Z,Y
      Z = ZERO
      Y = ANORM(Z)
      WRITE (IOUT,1041) Z,Y
      Z = -XMAX
      Y = ANORM(Z)
      WRITE (IOUT,1041) Z,Y
C-----------------------------------------------------------------
C  Test of error returns
C-----------------------------------------------------------------
      WRITE (IOUT,1050)
      W = XBIG
      Z = -W*(ONE-HALF*HALF)
      WRITE (IOUT,1052) Z
      Y = ANORM(Z)
      WRITE (IOUT,1062) Y
      Z = -W*(ONE+TEN*EPS)
      WRITE (IOUT,1053) Z
      Y = ANORM(Z)
      WRITE (IOUT,1062) Y
      WRITE (IOUT,1100)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of anorm(x) vs double series expansion'//)
 1001 FORMAT(///' Test of anorm(x) vs Taylor series about x-1/2'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT('  ANORM(X) was larger',I6,' times,')
 1015 FORMAT(14X,' agreed',I6,' times, and'/
     1    10X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1025 FORMAT('1Special Tests'//)
 1030 FORMAT(7X,'Check of identity anorm(X) + anorm(-X) = 1.0'//
     1       9X,'X',12X,'ANORM(-x)',3X,'ANORM(x)+ANORM(-x)-1'/)
c     1032 FORMAT(3(3X,E13.6E3)/)
 1032 FORMAT(3X,E13.6E3,3X,E13.6E3,3X,E13.6E3/)      
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT(' ANORM (',E13.6E3,') = ',E13.6E3//)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' ANORM will be called with the argument ',E13.6E3,/
     1    ' The result should not underflow'//)
 1053 FORMAT(' ANORM will be called with the argument ',E13.6E3,/
     1    ' The result may underflow'//)
 1062 FORMAT(' ANORM returned the value',E13.6E3///)
 1100 FORMAT(' This concludes the tests')
C---------- Last line of ANORM test program ----------
      END
