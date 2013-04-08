      PROGRAM GAMTST
C------------------------------------------------------------------
C FORTRAN 77 program to test GAMMA or DGAMMA
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
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs related
C              to the real gamma function", W. J. Cody,
C              submitted for publication.
C
C  Latest modification: March 12, 1992
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C--------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,NX
CS    REAL  GAMMA,
      DOUBLE PRECISION  DGAMMA,
     1    A,AIT,ALBETA,ALNX,B,BETA,C,CL,CONV,C1,C2,DEL,EPS,
     2    EPSNEG,FUNC,HALF,ONE,REN,R6,R7,TEN,TWO,W,X,XC,XL,
     3    XMAX,XMIN,XMINV,XN,XNUM,XXN,XP,XPH,X99,XP99,Y,Z,ZERO,ZZ
CS    DATA C1,C2/2.8209479177387814347E-1,9.1893853320467274178E-1/,
CS   1     ZERO,HALF,ONE,TWO,TEN/0.0E0,0.5E0,1.0E0,2.0E0,10.0E0/,
CS   2     X99,XP99/-999.0E0,0.99E0/
      DATA C1,C2/2.8209479177387814347D-1,9.1893853320467274178D-1/,
     1     ZERO,HALF,ONE,TWO,TEN/0.0D0,0.5D0,1.0D0,2.0D0,10.0D0/,
     2     X99,XP99/-999.0D0,0.99D0/
      DATA IOUT/6/
C------------------------------------------------------------------
C  Statement functions for conversion
C------------------------------------------------------------------
CS    CONV(I) = REAL(I)
      CONV(I) = DBLE(I)
CS    FUNC(X) = GAMMA(X)
      FUNC(X) = DGAMMA(X)
C------------------------------------------------------------------
C  Determine machine parameters and set constants
C------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      A = ZERO
      B = TWO
      N = 2000
      XN = CONV(N)
      JT = 0
C-----------------------------------------------------------------
C  Determine smallest argument for GAMMA
C-----------------------------------------------------------------
      IF (XMIN*XMAX .LT. ONE) THEN
            XMINV = ONE/XMAX
         ELSE
            XMINV = XMIN
      END IF
C-----------------------------------------------------------------
C  Determine largest argument for GAMMA by Newton iteration
C-----------------------------------------------------------------
      CL = LOG(XMAX)
      XP = HALF * CL
      CL = C2 - CL
   50 X = XP
         ALNX = LOG(X)
         XNUM = (X-HALF)*ALNX - X + CL
         XP = X - XNUM/(ALNX-HALF/X)
         IF (ABS(XP-X)/X .GE. TEN*EPS) GO TO 50
      CL = XP
C-----------------------------------------------------------------
C  Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 4
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
C  Use duplication formula for X not close to the zero
C-----------------------------------------------------------------
            XPH = X * HALF + HALF
            XP = XPH - HALF
            X = XP + XP
            NX = INT(X)
            XXN = CONV(NX)
            C = (TWO ** NX) * (TWO ** (X-XXN))
            Z = FUNC(X)
            ZZ = ((C*C1)*FUNC(XP)) * FUNC(XPH)
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
            W = (Z - ZZ) / Z
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
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
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
         A = B
         IF (J .EQ. 1) THEN
               B = TEN
            ELSE IF (J .EQ. 2) THEN
               B = CL - HALF
            ELSE
               A = -(TEN-HALF) * HALF
               B = A + HALF
         END IF
  300 CONTINUE
C-----------------------------------------------------------------
C  Special tests
C  First test with special arguments
C-----------------------------------------------------------------
      WRITE (IOUT,1025)
      WRITE (IOUT,1040)
      X = -HALF
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = XMINV / XP99
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = ONE
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = TWO
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = CL * XP99
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
C-----------------------------------------------------------------
C  Test of error returns
C-----------------------------------------------------------------
      WRITE (IOUT,1050)
      X = -ONE
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      X = ZERO
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      X = XMINV * (ONE - EPS)
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      X = CL * (ONE + EPS)
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      WRITE (IOUT,1100)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of GAMMA(X) vs Duplication Formula'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT(' GAMMA(X) was larger',I6,' times,'/
     1    13X,' agreed',I6,' times, and'/
     2    9X,'was smaller',I6,' times.'//)
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
 1041 FORMAT(' GAMMA (',E13.6E3,') = ',E13.6E3//)
 1050 FORMAT('1Test of Error Returns'///)
 1052 FORMAT(' GAMMA will be called with the argument',E13.6E3,/
     1    ' This should trigger an error message'//)
 1061 FORMAT(' GAMMA returned the value',E13.6E3///)
 1100 FORMAT(' This concludes the tests')
C---------- Last line of GAMMA test program ----------
      END
