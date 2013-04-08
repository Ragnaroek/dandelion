      PROGRAM DAWTST
C------------------------------------------------------------------
C FORTRAN 77 program to test DAW
C
C  Method:
C
C     Accuracy test compare function values against a local
C     Taylor's series expansion.  Derivatives are generated
C     from the recurrence relation.
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
C              XMIN   - the smallest positive floating-point number
C              XMAX   - the largest floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "The use of Taylor series to test accuracy of
C              function programs", W. J. Cody and L. Stoltz,
C              submitted for publication.
C
C  Latest modification: March 9, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,II,IOUT,IRND,IT,J,JT,K1,
     1    K2,K3,MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1    A,AIT,ALBETA,B,BETA,CONV,DAW,DEL,DELTA,EPS,EPSNEG,
     2    FORTEN,HALF,ONE,P,REN,R6,R7,SIXTEN,TWO,T1,W,X,
     3    XBIG,XKAY,XL,XMAX,XMIN,XN,X1,X99,Y,Z,ZERO,ZZ
      DIMENSION P(0:14)
C------------------------------------------------------------------
CS    DATA ZERO,HALF,ONE,TWO/0.0E0,0.5E0,1.0E0,2.0E0/,
CS   1   FORTEN,SIXTEN,X99,DELTA/14.0E0,1.6E1,-999.0E0,6.25E-2/
      DATA ZERO,HALF,ONE,TWO/0.0D0,0.5D0,1.0D0,2.0D0/,
     1   FORTEN,SIXTEN,X99,DELTA/14.0D0,1.6D1,-999.0D0,6.25D-2/
      DATA IOUT/6/
C------------------------------------------------------------------
C  Define statement functions for conversions
C------------------------------------------------------------------
CS    CONV(NDUM) = REAL(NDUM)
      CONV(NDUM) = DBLE(NDUM)
C------------------------------------------------------------------
C  Determine machine parameters and set constants
C------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = DELTA
      B = ONE
      JT = 0
C-----------------------------------------------------------------
C  Random argument accuracy tests based on local Taylor expansion.
C-----------------------------------------------------------------
      DO 300 J = 1, 4
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
C  Purify arguments
C------------------------------------------------------------------
            Y = X - DELTA
            W = SIXTEN * Y
            T1 = W + Y
            Y = T1 - W
            X = Y + DELTA
C------------------------------------------------------------------
C  Use Taylor's Series Expansion
C------------------------------------------------------------------
            P(0) = DAW(Y)
            Z = Y + Y
            P(1) = ONE - Z * P(0)
            XKAY = TWO
            DO 100 II = 2, 14
               P(II) = -(Z*P(II-1)+XKAY*P(II-2))
               XKAY = XKAY + TWO
  100       CONTINUE
            ZZ = P(14)
            XKAY = FORTEN
            DO 110 II = 1, 14
               ZZ = ZZ*DELTA/XKAY + P(14-II)
               XKAY = XKAY - ONE
  110       CONTINUE
            Z = DAW(X)
C------------------------------------------------------------------
C  Accumulate Results
C------------------------------------------------------------------
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
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
C  Gather and print statistics for test
C------------------------------------------------------------------
         K2 = N - K1 - K3
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = X99
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = X99
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
C------------------------------------------------------------------
C  Initialize for next test
C------------------------------------------------------------------
         A = B
         B = B + B
         IF (J .EQ. 1) B = B + HALF
  300 CONTINUE
C-----------------------------------------------------------------
C  Special tests.  First check values for negative arguments.
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,1030) IBETA
      DO 350 I = 1, 10
         X = REN(J)*(TWO+TWO)
         B = DAW(X)
         A = B + DAW(-X)
         IF (A*B .NE. ZERO) A = AIT + LOG(ABS(A/B))/ALBETA
         WRITE (IOUT,1031) X,A
         X = X + DEL
  350 CONTINUE
C-----------------------------------------------------------------
C  Next, test with special arguments
C-----------------------------------------------------------------
      WRITE (IOUT,1040)
      Z = XMIN
      ZZ = DAW(Z)
      WRITE (IOUT,1041) ZZ
C-----------------------------------------------------------------
C  Test of error return for arguments > xmax.  First, determine
C    xmax
C-----------------------------------------------------------------
      IF (HALF .LT. XMIN*XMAX ) THEN
            XBIG = HALF/XMIN
         ELSE
            XBIG = XMAX
      END IF
      WRITE (IOUT,1050)
      Z = XBIG*(ONE-DELTA*DELTA)
      WRITE (IOUT,1052) Z
      ZZ = DAW(Z)
      WRITE (IOUT,1062) ZZ
      Z = XBIG
      WRITE (IOUT,1053) Z
      ZZ = DAW(Z)
      WRITE (IOUT,1062) ZZ
      W = ONE + DELTA*DELTA
      IF (W .LT. XMAX/XBIG ) THEN
            Z = XBIG*W
            WRITE (IOUT,1053) Z
            ZZ = DAW(Z)
            WRITE (IOUT,1062) ZZ
      END IF
      WRITE (IOUT,1100)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of Dawson''s Integral vs Taylor expansion'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT('  F(X) was larger',I6,' times,'/
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
 1025 FORMAT('1Special Tests'//)
 1030 FORMAT(7X,'Estimated loss of base',i3,' significant digits in'//
     1       8X,'X',10X,'F(x)+F(-x)'/)
 1031 FORMAT(3X,F7.3,F16.2)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT('  F(XMIN) = ',E24.17E3/)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' DAW will be called with the argument',E13.6E3,/
     1    ' This should not underflow'//)
 1053 FORMAT(' DAW will be called with the argument',E13.6E3,/
     1    ' This may underflow'//)
 1062 FORMAT(' DAW returned the value',E13.6E3///)
 1100 FORMAT(' This concludes the tests')
C---------- Last line of DAW test program ----------
      END
