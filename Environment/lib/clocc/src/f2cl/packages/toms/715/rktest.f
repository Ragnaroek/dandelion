      PROGRAM RKTEST
C--------------------------------------------------------------------
C FORTRAN 77 program to test RKBESL
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
C                       significand of a floating-point number
C              MINEXP - the largest in magnitude negative
C                       integer such that FLOAT(IBETA)**MINEXP
C                       is a positive floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              EPSNEG - the smallest positive floating-point
C                       number such that 1.0-EPSNEG .NE. 1.0
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, EXP, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 14, 1992
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C--------------------------------------------------------------------
      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,IZE,IZ1,J,JT,K1,K2,
     1        K3,MACHEP,MAXEXP,MB,MINEXP,N,NCALC,NDUM,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1    A,AIT,ALBETA,ALPHA,AMAXEXP,A1,B,BETA,C,CONV,D,DEL,EIGHT,EPS,
     2    EPSNEG,FIVE,HALF,HUND,ONE,OVRCHK,REN,R6,R7,SIXTEN,SUM,T,TEN,
     3    TINYX,T1,U,U2,W,X,XL,XLAM,XMAX,XMB,XMIN,XN,XNINE,X1,X99,Y,Z,
     4    ZERO,ZZ
      DIMENSION U(560),U2(560)
CS    DATA ZERO,HALF,ONE,FIVE,EIGHT/0.0E0,0.5E0,1.0E0,5.0E0,8.0E0/
CS    DATA XNINE,TEN,HUND/9.0E0,10.0E0,1.0E2/
CS    DATA X99,SIXTEN,XLAM/-999.0E0,1.6E1,0.9375E0/
CS    DATA C/2.2579E-1/,TINYX/1.0E-10/
      DATA ZERO,HALF,ONE,FIVE,EIGHT/0.0D0,0.5D0,1.0D0,5.0D0,8.0D0/
      DATA XNINE,TEN,HUND/9.0D0,10.0D0,1.0D2/
      DATA X99,SIXTEN,XLAM/-999.0D0,1.6D1,0.9375D0/
      DATA C/2.2579D-1/,TINYX/1.0D-10/
      DATA IOUT/6/
C--------------------------------------------------------------------
C  Define statement function for integer to float conversion
C--------------------------------------------------------------------
CS    CONV(NDUM) = REAL(NDUM)
      CONV(NDUM) = DBLE(NDUM)
C--------------------------------------------------------------------
C  Determine machine parameters and set constants
C--------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      AMAXEXP = CONV(MAXEXP)
      A = EPS
      B = ONE
      JT = 0
C-------------------------------------------------------------------
C  Random argument accuracy tests
C-------------------------------------------------------------------
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         N = 2000
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         A1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            ALPHA = REN(JT)
C--------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem
C--------------------------------------------------------------------
            IZE = 1
            MB = 3
C--------------------------------------------------------------------
C   Carefully purify arguments
C--------------------------------------------------------------------
            Y = X/XLAM
            W = SIXTEN * Y
            T1 = W + Y
            Y = T1 - W
            X = Y * XLAM
            CALL RKBESL(Y,ALPHA,MB,IZE,U2,NCALC)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
                  U2(1) = U2(1) * EPS
                  U2(2) = U2(2) * EPS
            END IF
            MB = 1
            XMB = ZERO
            W = (ONE-XLAM) * (ONE+XLAM) * HALF
            D = W*Y
            T = U2(1) + D * U2(2)
            T1 = EPS / HUND
C--------------------------------------------------------------------
C   Generate terms using recurrence
C--------------------------------------------------------------------
            DO 110 II = 3, 35
               XMB = XMB + ONE
               T1 = XMB * T1 / D
               Z = U2(II-1)
               OVRCHK = (XMB+ALPHA)/(Y*HALF)
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (U2(II-1) .GT. ONE) THEN
                     IF (OVRCHK .GT. (XMAX/U2(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO 200
                     END IF
               END IF
               U2(II) = OVRCHK * U2(II-1) + U2(II-2)
               IF (T1 .GT. ONE/EPS) THEN
                     T = T * T1
                     T1 = ONE
               END IF
               MB = MB + 1
  110       CONTINUE
C--------------------------------------------------------------------
C   Accurate Summation
C--------------------------------------------------------------------
            XMB = XMB + ONE
  120       SUM = U2(MB+1)
            IND = MB
            DO 155 II = 1, MB
               SUM = SUM * D / XMB + U2(IND)
               IND = IND - 1
               XMB = XMB - ONE
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            ZZ = ZZ * XLAM ** ALPHA
            MB = 2
            CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
            Z = U(1)
            Y = Z
            IF (U2(1) .GT. Y) Y = U2(1)
C--------------------------------------------------------------------
C   Accumulate Results
C--------------------------------------------------------------------
            W = (Z - ZZ) / Y
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
                  A1 = ALPHA
                  IZ1 = IZE
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C--------------------------------------------------------------------
C   Gather and print statistics for test
C--------------------------------------------------------------------
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = X99
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1,A1,IZ1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1,A1,IZ1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = X99
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
C--------------------------------------------------------------------
C   Initialize for next test
C--------------------------------------------------------------------
         A = B
         B = B + B
         IF (J .EQ. 1) B = TEN
  300 CONTINUE
C-------------------------------------------------------------------
C   Test of error returns
C   First, test with bad parameters
C-------------------------------------------------------------------
      WRITE (IOUT, 2006)
      X = -ONE
      ALPHA = HALF
      MB = 5
      IZE = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = -X
      ALPHA = ONE + HALF
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = HALF
      MB = -MB
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      MB = -MB
      IZE = 5
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
C-------------------------------------------------------------------
C   Last tests are with extreme parameters
C-------------------------------------------------------------------
      X = XMIN
      ALPHA = ZERO
      MB = 2
      IZE = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = TINYX*(ONE-SQRT(EPS))
      MB = 20
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = TINYX*(ONE+SQRT(EPS))
      MB = 20
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
C-------------------------------------------------------------------
C   Determine largest safe argument for unscaled functions
C-------------------------------------------------------------------
      Z = LOG(XMIN)
      W = Z-C
      ZZ = -Z-TEN
  350 Z = ZZ
         ZZ = ONE/(EIGHT*Z)
         A = Z+LOG(Z)*HALF+ZZ*(ONE-XNINE*HALF*ZZ)+W
         B = ONE+(HALF-ZZ*(ONE-XNINE*ZZ))/Z
         ZZ = Z-A/B
      IF (ABS(Z-ZZ) .GT. HUND*EPS*Z) GO TO 350
      X = ZZ*XLAM
      IZE = 1
      MB = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = ZZ
      MB = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = TEN / EPS
      IZE = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = XMAX
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      STOP
C-------------------------------------------------------------------
 1000 FORMAT('1Test of K(X,ALPHA) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' K(X,ALPHA) was larger',I6,' times,'/
     1    15X,' agreed',I6,' times, and'/
     1    11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3,', NU =',E13.6E3,
     2    ' and IZE =',I2)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3,', NU =',E13.6E3,
     2    ' and IZE =',I2)
 1025 FORMAT(' The root mean square absolute error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 2006 FORMAT('1Check of Error Returns'///
     1    ' The following summarizes calls with indicated parameters'//
     2    ' NCALC different from MB indicates some form of error'//
     3    ' See documentation for RKBESL for details'//
     4    7X,'ARG',12X,'ALPHA',6X,'MB',3X,'IZ',7X,'RES',6X,'NCALC'//)
 2011 FORMAT(2E15.7E3,2I5,E15.7E3,I5//)
C---------- Last line of RKBESL test program ----------
      END
