      PROGRAM I0TEST
C------------------------------------------------------------------
C FORTRAN 77 program to test BESI0, BESEI0
C
C  Method:
C
C     Accuracy tests compare function values against values
C     generated with the multiplication formula for small
C     arguments and values generated from a Taylor's Series
C     Expansion using Amos' Ratio Scheme for initial values
C     for large arguments.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significant of a floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - the smallest non-vanishing normalized
C                       floating-point power of the radix
C              XMAX   - the largest finite floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  Reference: "Computation of Modified Bessel Functions and
C              Their Ratios," D. E. Amos, Math. of Comp.,
C              Volume 28, Number 24, January, 1974.
C
C             "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C             "Use of Taylor series to test accuracy of function
C              programs," W. J. Cody and L. Stoltz, submitted
C              for publication.
C
C  Latest modification: March 12, 1992
C
C  Authors:  W. J. Cody and L. Stoltz
C            Mathematics and Computer Science Division
C            Argonne National Laboratory
C            Argonne, IL 60439
C
C------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,J1,J2,K1,K2,K3,
     1    MACHEP,MAXEXP,MB,MBORG,MINEXP,MB2,N,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1    A,AIT,AK,AKK,ALBETA,ARR,ATETEN,B,BESEI0,BESI0,BETA,BOT,C,
     2    CONST,CONV,D,DEL,DELTA,E,EIGHT,EPS,EPSNEG,F,HALF,HUND,ONE,
     3    OVRCHK,ONE28,REN,R6,R7,SIXTEN,SUM,TEMP,TOP,TWO,T1,T2,U,U2,
     4    W,X,XA,XB,XBAD,XJ1,XL,XLAM,XLARGE,XMAX,XMB,XMIN,XN,XNINE,
     5    X1,X99,Y,Z,ZERO,ZZ
      DIMENSION ARR(8,6),U(560),U2(560)
CS    DATA ZERO,HALF,ONE,TWO,EIGHT/0.0E0,0.5E0,1.0E0,2.0E0,8.0E0/,
CS   1   XNINE,SIXTEN,ATETEN,HUND/9.0E0,1.6E1,1.8E1,1.0E2/,
CS   2   ONE28,X99,XLAM,XLARGE/1.28E2,-999.0E0,1.03125E0,1.0E4/,
CS   3   C/0.9189385332E0/
      DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/,
     1   XNINE,SIXTEN,ATETEN,HUND/9.0D0,1.6D1,1.8D1,1.0D2/,
     2   ONE28,X99,XLAM,XLARGE/1.28D2,-999.0D0,1.03125D0,1.0D4/,
     3   C/0.9189385332D0/
CS    DATA  ARR/0.0E0,1.0E0,-1.0E0,1.0E0,-2.0E0,1.0E0,-3.0E0,1.0E0,
CS   1          -999.0E0,-999.0E0,-999.0E0,3.0E0,-12.0E0,9.0E0,-51.0E0,
CS   2          18.0E0,-5040.0E0,720.0E0,0.0E0,-999.0E0,-999.0E0,
CS   3          60.0E0,-360.0E0,345.0E0,-1320.0E0,192.0E0,-120.0E0,
CS   4          24.0E0,0.0E0,-999.0E0,-999.0E0,2520.0E0,-96.0E0,15.0E0,
CS   5          -33.0E0,7.0E0,-6.0E0,2.0E0,0.0E0,-999.0E0,-4.0E0,1.0E0,
CS   6          -3.0E0,1.0E0,-2.0E0,1.0E0,-1.0E0,1.0E0/
      DATA  ARR/0.0D0,1.0D0,-1.0D0,1.0D0,-2.0D0,1.0D0,-3.0D0,1.0D0,
     1          -999.0D0,-999.0D0,-999.0D0,3.0D0,-12.0D0,9.0D0,-51.0D0,
     2          18.0D0,-5040.0D0,720.0D0,0.0D0,-999.0D0,-999.0D0,
     3          60.0D0,-360.0D0,345.0D0,-1320.0D0,192.0D0,-120.0D0,
     4          24.0D0,0.0D0,-999.0D0,-999.0D0,2520.0D0,-96.0D0,15.0D0,
     5          -33.0D0,7.0D0,-6.0D0,2.0D0,0.0D0,-999.0D0,-4.0D0,1.0D0,
     6          -3.0D0,1.0D0,-2.0D0,1.0D0,-1.0D0,1.0D0/
      DATA IOUT/6/
C------------------------------------------------------------------
C  Define statement functions for conversions
C------------------------------------------------------------------
CS    CONV(N) = REAL(N)
      CONV(N) = DBLE(N)
      TOP(X) = X - HALF*LOG(X) + LOG(ONE+(ONE/EIGHT-XNINE/
     1   ONE28/X)/X)
      BOT(X) = -(SIXTEN*X+ATETEN) / (((ONE28*X+SIXTEN)*X+
     1   XNINE)*X) + ONE - HALF/X
C------------------------------------------------------------------
C  Determine machine parameters and set constants
C------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = ZERO
      B = TWO
      JT = 0
      CONST = C + LOG(XMAX)
      DELTA = XLAM - ONE
      F = (XLAM-ONE) * (XLAM+ONE) * HALF
C-----------------------------------------------------------------
C  Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 4
C-------------------------------------------------------------------
C  Calculate the number of terms needed for convergence of the
C  series by using Newton's iteration on the asymptotic form of
C  the multiplication theorem
C-------------------------------------------------------------------
         XBAD = B
         D = AIT * ALBETA - C + ONE
         E = LOG(XBAD * F) + ONE
         AKK = ONE
  100    AK = AKK
            Z = D + E*AK - (AK+HALF) * LOG(AK+ONE)
            ZZ = E - (AK+HALF)/(AK+ONE) - LOG(AK+ONE)
            AKK = AK - Z/ZZ
         IF (ABS(AK-AKK) .GT. HUND*EPS*AK) GO TO 100
         MBORG = INT(AKK) + 1
         N = 2000
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
C------------------------------------------------------------------
C   Carefully purify arguments
C------------------------------------------------------------------
            IF (J .EQ. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            TEMP = SIXTEN * Y
            T1 = TEMP + Y
            T1 = TEMP + T1
            Y = T1 - TEMP
            Y = Y - TEMP
            IF (J .EQ. 1) THEN
                  X = Y * XLAM
               ELSE
                  X = Y + DELTA
            END IF
C------------------------------------------------------------------
C   Use Amos' Ratio Scheme
C------------------------------------------------------------------
            D = F*Y
            MB = MBORG + MBORG
            MB2 = MB - 1
            XMB = CONV(MB2)
            TEMP = (XMB + ONE + HALF) * (XMB + ONE + HALF)
            U2(MB) = Y / (XMB + HALF + SQRT(TEMP + Y*Y))
C------------------------------------------------------------------
C   Generate ratios using recurrence
C------------------------------------------------------------------
            DO 110 II = 2, MB
               OVRCHK = XMB/(Y*HALF)
               U2(MB2) = ONE / (OVRCHK + U2(MB2+1))
               XMB = XMB - ONE
               MB2 = MB2 - 1
  110       CONTINUE
            U(1) = BESI0(Y)
            IF (J .EQ. 1) THEN
C------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem
C------------------------------------------------------------------
                  MB = MB - MBORG
                  DO 120 II = 2, MB
                     U(II) = U(II-1) * U2(II-1)
  120             CONTINUE
C------------------------------------------------------------------
C   Accurate Summation
C------------------------------------------------------------------
                  MB = MB - 1
                  XMB = CONV(MB)
                  SUM = U(MB+1)
                  IND = MB
                  DO 155 II = 2, MB
                     SUM = SUM * D / XMB + U(IND)
                     IND = IND - 1
                     XMB = XMB - ONE
  155             CONTINUE
                  ZZ = SUM * D + U(IND)
               ELSE
C------------------------------------------------------------------
C   Accuracy test is based on Taylor's Series Expansion
C------------------------------------------------------------------
                  U(2) = U(1) * U2(1)
                  MB = 8
                  J1 = MB
                  XJ1 = CONV(J1+1)
                  IEXP = 0
C------------------------------------------------------------------
C   Accurate Summation
C------------------------------------------------------------------
                  DO 180 II = 1, MB
                     J2 = 1
  160                J2 = J2 + 1
                     IF (ARR(J1,J2) .NE. X99) GO TO 160
                     J2 = J2 - 1
                     T1 = ARR(J1,J2)
                     J2 = J2 - 1
C------------------------------------------------------------------
C   Group I0 terms in the derivative
C------------------------------------------------------------------
                     IF (J2 .EQ. 0) GO TO 168
  165                T1 = T1 / (Y*Y) + ARR(J1,J2)
                           J2 = J2 - 1
                     IF (J2 .GE. 1) GO TO 165
  168                IF (IEXP .EQ. 1) T1 = T1 / Y
                     J2 = 6
  170                J2 = J2 - 1
                     IF (ARR(II,J2) .NE. X99) GO TO 170
                     J2 = J2 + 1
                     T2 = ARR(II,J2)
                     J2 = J2 + 1
                     IF (IEXP .EQ. 0) THEN
                           IEXP = 1
                        ELSE
                           IEXP = 0
                     END IF
C------------------------------------------------------------------
C   Group I1 terms in the derivative
C------------------------------------------------------------------
                     IF (J2 .EQ. 7) GO TO 177
  175                T2 = T2 / (Y*Y) + ARR(II,J2)
                           J2 = J2 + 1
                     IF (J2 .LE. 6) GO TO 175
  177                IF (IEXP .EQ. 1) T2 = T2 / Y
                     IF (J1 .EQ. 8) THEN
                           SUM = U(1)*T1 + U(2)*T2
                        ELSE
                           SUM = SUM * (DELTA/XJ1) + (U(1)*T1 + U(2)*T2)
                     END IF
                     J1 = J1 - 1
                     XJ1 = CONV(J1+1)
  180             CONTINUE
                  ZZ = SUM * DELTA + U(1)
            END IF
            Z = BESI0(X)
C------------------------------------------------------------------
C   Accumulate Results
C------------------------------------------------------------------
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
C------------------------------------------------------------------
C   Gather and print statistics for test
C------------------------------------------------------------------
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
               W = LOG(R6)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(R7)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
C------------------------------------------------------------------
C   Initialize for next test
C------------------------------------------------------------------
         A = B
         B = B + B
         IF (J .EQ. 1) B = B + B - HALF
  300 CONTINUE
C-----------------------------------------------------------------
C   Test of error returns
C
C   Special tests
C-----------------------------------------------------------------
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESI0(XMIN)
      WRITE (IOUT,1032) Y
      Y = BESI0(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = -ONE * REN(JT)
      Y = BESI0(X)
      WRITE (IOUT,1034) X,Y
      X = -X
      Y = BESI0(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEI0(XMAX)
      WRITE (IOUT,1035) Y
C-----------------------------------------------------------------
C   Determine largest safe argument for unscaled functions
C-----------------------------------------------------------------
      WRITE (IOUT, 1036)
      XA = LOG(XMAX)
  330 XB = XA - (TOP(XA)-CONST) / BOT(XA)
      IF (ABS(XB-XA)/XB .LE. EPS) THEN
            GO TO 350
         ELSE
            XA = XB
            GO TO 330
      END IF
  350 XLARGE = XB / XLAM
      Y = BESI0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * XLAM
      Y = BESI0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      WRITE (IOUT, 1037)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of I0(X) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of I0(X) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' I0(X) was larger',I6,' times,'/
     1    10X,' agreed',I6,' times, and'/
     1    6X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Test with extreme arguments'/)
 1032 FORMAT(' I0(XMIN) = ',E24.17E3/)
 1033 FORMAT(' I0(',I1,') = ',E24.17E3/)
 1034 FORMAT(' I0(',E24.17E3,' ) = ',E24.17E3/)
 1035 FORMAT(' E**-X * I0(XMAX) = ',E24.17E3/)
 1036 FORMAT(' Tests near the largest argument for unscaled functions'/)
 1037 FORMAT(' This concludes the tests.')
C---------- Last line of BESI0 test program ----------
      END
