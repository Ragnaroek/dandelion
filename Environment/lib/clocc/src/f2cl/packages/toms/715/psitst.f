      PROGRAM PSITST
C--------------------------------------------------------------------
C  Fortran 77 program to test PSI
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
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MINEXP - the largest in magnitude negative
C                          integer such that  FLOAT(IBETA)**MINEXP
C                          is a positive floating-point number
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 EPSNEG - the smallest positive floating-point
C                          number such that 1.0-EPSNEG .NE. 1.0
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic Fortran functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs related
C              to the real gamma function", W. J. Cody,
C              submitted for publication.
C
C  Latest modification: March 14, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C--------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BETA,CONV,DEL,EIGHT,EPS,
     2   EPSNEG,HALF,ONE,ONE7,ONE6,PSI,REN,R6,R7,THREE,
     3   TWENTY,Y,V0,W,X,XH,XL,XL2,XMAX,XMIN,XN,XX,X0,
     4   X01,X1,Z,ZERO,ZH,ZZ
CS    DATA ZERO,ONE,THREE/0.0E0,1.0E0,3.0E0/,
CS   1   HALF,EIGHT,TWENTY,ALL9/0.5E0,8.0E0,20.0E0,-999.0E0/,
CS   2   XL2/6.9314718055994530942E-1/,
CS   3   ONE7,ONE6/-17.625E0,-16.875E0/,
CS   4   X0,X01,V0/374.0E0,256.0E0,-6.7240239024288040437E-04/
      DATA ZERO,ONE,THREE/0.0D0,1.0D0,3.0D0/,
     1   HALF,EIGHT,TWENTY,ALL9/0.5D0,8.0D0,20.0D0,-999.0D0/,
     2   XL2/6.9314718055994530942D-1/,
     3   ONE7,ONE6/-17.625D0,-16.875D0/,
     4   X0,X01,V0/374.0D0,256.0D0,-6.7240239024288040437D-04/
      DATA IOUT/6/
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
C--------------------------------------------------------------------
C     Random argument accuracy tests
C--------------------------------------------------------------------
      DO 300 J = 1, 4
         K1 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         IF (J .EQ. 1) THEN
               A = ZERO
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               A = B + B
               B = EIGHT
            ELSE IF (J .EQ. 3) THEN
               A = B
               B = TWENTY
            ELSE
               A = ONE7
               B = ONE6
               N = 500
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
C--------------------------------------------------------------------
C  Carefully purify arguments and evaluate identity
C--------------------------------------------------------------------
            XX = X * HALF
            XH = XX + HALF
            XX = XH - HALF
            X = XX + XX
            Z = PSI(X)
            ZH = PSI(XH)
            ZZ = PSI(XX)
            ZZ = (ZZ+ZH)*HALF + XL2
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
            W = (ZZ - Z) / ZZ
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
C--------------------------------------------------------------------
C  Process and output statistics
C--------------------------------------------------------------------
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (2*(J/2) .NE. J) WRITE (IOUT,1000)
         WRITE (IOUT,1001)
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
         X = X0/X01
         Y = PSI(X)
         Z = (Y-V0)/V0
         IF (Z .NE. ZERO) THEN
               W = LOG(ABS(Z))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1031) X,Y,IBETA,W
      WRITE (IOUT,1033)
      IF (XMAX*XMIN .GE. ONE) THEN
            X = XMIN
         ELSE
            X = ONE / XMAX
      END IF
      WRITE (IOUT,1035) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1035) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
C--------------------------------------------------------------------
C  Test of error returns
C--------------------------------------------------------------------
      WRITE (IOUT,1037)
      X = ZERO
      WRITE (IOUT,1034) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      X = -THREE/EPS
      WRITE (IOUT,1034) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      STOP
C--------------------------------------------------------------------
 1000 FORMAT('1')
 1001 FORMAT(' Test of PSI(X) vs (PSI(X/2)+PSI(X/2+1/2))/2 + ln(2)'
     1 //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 '(',F5.1,',',F5.1,')',//)
 1011 FORMAT(' ABS(PSI(X)) was larger',I6,' times', /
     1     21X,' agreed',I6,' times, and'/
     1   17X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4E3,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near positive zero'//' PSI(',E14.7E3,') = ',
     1    E24.17E3/13X,'Loss of base',I3,' digits = ',F7.2/)
 1033 FORMAT(//' Test with extreme arguments'/)
 1034 FORMAT(' PSI will be called with the argument ',E17.10E3/
     1     ' This may stop execution.'/)
 1035 FORMAT(' PSI will be called with the argument ',E17.10E3/
     1     ' This should not stop execution.'/)
 1036 FORMAT(' PSI returned the value',E25.17E3//)
 1037 FORMAT(//' Test of error returns'//)
 1100 FORMAT(' This concludes the tests.')
C---------- Last card of PSI test program ----------
      END
