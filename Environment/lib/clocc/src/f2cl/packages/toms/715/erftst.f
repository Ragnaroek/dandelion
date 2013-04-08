      PROGRAM ERFTST
C------------------------------------------------------------------
C FORTRAN 77 program to test ERF and related functions.
C
C  Method:
C
C     Accuracy test compare function values against local Taylor's
C     series expansions.  Derivatives for erfc(x) are expressed as
C     repeated integrals of erfc(x).  These are generated from the
C     recurrence relation using a technique due to Gautschi (see
C     references).
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
C               submitted for publication.
C
C              "Evaluation of the repeated integrals of the coerror
C               function", W. Gautschi, TOMS 3, 1977, pp. 240-252.
C
C  Latest modification: March 12, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,N0,N1
CS    REAL  ERF,ERFC,ERFCX,
      DOUBLE PRECISION DERF,DERFC,DERFCX,
     1    A,AIT,ALBETA,B,BETA,C,CONV,C1,C2,DEL,EPS,EPSCON,EPSNEG,FF,
     2    FUNC1,FUNC2,FUNC3,F0,HALF,ONE,R,REN,R1,R6,R7,SC,SIXTEN,
     3    THRESH,TEN,TWO,U,V,W,X,XBIG,XC,XL,XMAX,XMIN,XN,XN1,X99,
     4    Z,ZERO,ZZ
      DIMENSION R1(500)
C------------------------------------------------------------------
C   C1 = 1/sqrt(pi)
C------------------------------------------------------------------
CS    DATA ZERO,HALF,ONE,TWO,TEN/0.0E0,0.5E0,1.0E0,2.0E0,10.0E0/,
CS   1     SIXTEN,THRESH,X99/16.0E0,0.46875E0,-999.0E0/,
CS   2     C1/5.6418958354775628695E-1/
      DATA ZERO,HALF,ONE,TWO,TEN/0.0D0,0.5D0,1.0D0,2.0D0,10.0D0/,
     1     SIXTEN,THRESH,X99/16.0D0,0.46875D0,-999.0D0/,
     2     C1/5.6418958354775628695D-1/
      DATA IOUT/6/
C------------------------------------------------------------------
C  Statement functions for conversion
C------------------------------------------------------------------
CS    CONV(I) = REAL(I)
CS    FUNC1(X) = ERF(X)
CS    FUNC2(X) = ERFC(X)
CS    FUNC3(X) = ERFCX(X)
      CONV(I) = DBLE(I)
      FUNC1(X) = DERF(X)
      FUNC2(X) = DERFC(X)
      FUNC3(X) = DERFCX(X)
C------------------------------------------------------------------
C  Determine machine parameters and set constants
C------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      C = ABS(AIT*ALBETA) + TEN
      A = ZERO
      B = THRESH
      N = 2000
      XN = CONV(N)
      JT = 0
      N0 = (IBETA/2)*(IT+5)/6+4
C-----------------------------------------------------------------
C  Determine largest argument for ERFC test by Newton iteration
C-----------------------------------------------------------------
      C2 = LOG(XMIN) + LOG(ONE/C1)
      XBIG = SQRT(-C2)
   50 X = XBIG
         F0 = X*X
         FF = F0 + HALF/F0 + LOG(X) + C2
         F0 = X+X + ONE/X - ONE/(X*F0)
         XBIG = X - FF/F0
         IF (ABS(X-XBIG)/X .GT. TEN*EPS) GO TO 50
C-----------------------------------------------------------------
C  Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 5
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
C  Test erf against double series expansion
C-----------------------------------------------------------------
                  F0 = CONV(N0)
                  FF = F0+F0+ONE
                  Z = X*X
                  W = Z+Z
                  U = ZERO
                  V = ZERO
                  DO 60 K = 1, N0
                     U = -Z/F0*(ONE+U)
                     V = W/FF*(ONE+V)
                     F0 = F0 - ONE
                     FF = FF - TWO
   60             CONTINUE
                  V = C1*(X+X)*(((U*V+(U+V))+HALF)+HALF)
                  U = FUNC1(X)
               ELSE
C-----------------------------------------------------------------
C  Test erfc or scaled erfc against expansion in repeated
C   integrals of the coerror function.
C-----------------------------------------------------------------
                  Z = X + HALF
                  X = Z - HALF
                  R = ZERO
                  IF (X .LE. ONE) THEN
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
                  IF ((J/2)*2 .EQ. J) THEN
                        F0 = FUNC2(Z)*EXP(X+HALF*HALF)
                        U = FUNC2(X)
                     ELSE
                        F0 = FUNC3(Z)
                        U = FUNC3(X)
                  END IF
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
  111             V = R1(K)
                  DO 120 N1 = 1, K-1
                     V = V + R1(K-N1)
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
               WRITE (IOUT,1010) N,A,B
               WRITE (IOUT,1011) K1
            ELSE IF ((J/2)*2 .EQ. J) THEN
               WRITE (IOUT,1001)
               WRITE (IOUT,1010) N,A,B
               WRITE (IOUT,1012) K1
            ELSE
               WRITE (IOUT,1002)
               WRITE (IOUT,1010) N,A,B
               WRITE (IOUT,1013) K1
         END IF
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
               A = B
               B = TWO
            ELSE IF (J .EQ. 3) THEN
               A = B
               B = AINT(XBIG*SIXTEN)/SIXTEN-HALF
            ELSE IF (J .EQ. 4) THEN
               B = TEN + TEN
         END IF
  300 CONTINUE
C-----------------------------------------------------------------
C  Special tests
C  First check values for negative arguments.
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,1030) IBETA
      X = ZERO
      DEL = -HALF
      DO 350 I = 1, 10
         U = FUNC1(X)
         A = U + FUNC1(-X)
         IF (A*U .NE. ZERO) A = AIT + LOG(ABS(A/U))/ALBETA
         V = FUNC2(X)
         B = U + V - ONE
         IF (B .NE. ZERO) B = AIT + LOG(ABS(B))/ALBETA
         W = FUNC3(X)
         C = AINT(X*SIXTEN)/SIXTEN
         R = (X-C)*(X+C)
         C = (EXP(C*C)*EXP(R)*V-W)/W
         IF (C .NE. ZERO) C = MAX(ZERO,AIT + LOG(ABS(C))/ALBETA)
         WRITE (IOUT,1031) X,A,B,C
         X = X + DEL
  350 CONTINUE
C-----------------------------------------------------------------
C  Next, test with special arguments
C-----------------------------------------------------------------
      WRITE (IOUT,1040)
      Z = XMAX
      ZZ = FUNC1(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = ZERO
      ZZ = FUNC1(Z)
      WRITE (IOUT,1041) Z,ZZ
      ZZ = FUNC2(Z)
      WRITE (IOUT,1042) Z,ZZ
      Z = -XMAX
      ZZ = FUNC2(Z)
      WRITE (IOUT,1042) Z,ZZ
C-----------------------------------------------------------------
C  Test of error returns
C-----------------------------------------------------------------
      WRITE (IOUT,1050)
      W = XBIG
      Z = W*(ONE-HALF*HALF)
      WRITE (IOUT,1052) Z
      ZZ = FUNC2(Z)
      WRITE (IOUT,1062) ZZ
      Z = W*(ONE+TEN*EPS)
      WRITE (IOUT,1053) Z
      ZZ = FUNC2(Z)
      WRITE (IOUT,1062) ZZ
      W = XMAX
      IF (C1 .LT. XMAX*XMIN) W = C1/XMIN
      Z = W*(ONE-ONE/SIXTEN)
      WRITE (IOUT,1054) Z
      ZZ = FUNC3(Z)
      WRITE (IOUT,1064) ZZ
      W = -SQRT(LOG(XMAX/TWO))
      Z = W*(ONE-ONE/TEN)
      WRITE (IOUT,1055) Z
      ZZ = FUNC3(Z)
      WRITE (IOUT,1064) ZZ
      Z = W*(ONE+TEN*EPS)
      WRITE (IOUT,1056) Z
      ZZ = FUNC3(Z)
      WRITE (IOUT,1064) ZZ
      WRITE (IOUT,1100)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of erf(x) vs double series expansion'//)
 1001 FORMAT(///' Test of erfc(x) vs exp(x+1/4) SUM i^n erfc(x+1/2)'//)
 1002 FORMAT('1Test of exp(x*x) erfc(x) vs SUM i^n erfc(x+1/2)'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT('    ERF(X) was larger',I6,' times,')
 1012 FORMAT('   ERFC(X) was larger',I6,' times,')
 1013 FORMAT('  ERFCX(X) was larger',I6,' times,')
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
 1030 FORMAT(7X,'Estimated loss of base',i3,'significant digits in'//
     1       3X,'X',5X,'Erf(x)+Erf(-x)',3X,'Erf(x)+Erfc(x)-1',
     1       3X,'Erfcx(x)-exp(x*x)*erfc(x)'/)
 1031 FORMAT(F7.3,3F16.2)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT('   ERF (',E13.6E3,') = ',E13.6E3//)
 1042 FORMAT('  ERFC (',E13.6E3,') = ',E13.6E3//)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' ERFC will be called with the argument',E13.6E3,/
     1    ' This should not underflow'//)
 1053 FORMAT(' ERFC will be called with the argument',E13.6E3,/
     1    ' This may underflow'//)
 1054 FORMAT(' ERFCX will be called with the argument',E13.6E3,/
     1    ' This should not underflow'//)
 1055 FORMAT(' ERFCX will be called with the argument',E13.6E3,/
     1    ' This should not overflow'//)
 1056 FORMAT(' ERFCX will be called with the argument',E13.6E3,/
     1    ' This may overflow'//)
 1062 FORMAT(' ERFC returned the value',E13.6E3///)
 1064 FORMAT(' ERFCX returned the value',E13.6E3///)
 1100 FORMAT(' This concludes the tests')
C---------- Last line of ERF test program ----------
      END
