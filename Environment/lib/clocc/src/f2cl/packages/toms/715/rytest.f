
      PROGRAM RYTEST
C----------------------------------------------------------------------
C FORTRAN 77 program to test RYBESL
C
C  Method:
C
C     Two different accuracy tests are used.  In the first interval,
C     function values are compared against values generated with the
C     multiplication formula, where the Bessel values used in the
C     multiplication formula are obtained from the function program.
C     In the remaining intervals, function values are compared
C     against values generated with a local Taylor series expansion.
C     Derivatives in the expansion are expressed in terms of the
C     first two Bessel functions, which are in turn obtained from
C     the function program.
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
C              XMIN   - the smallest positive normalized
C                       floating-point number
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
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C             "Use of Taylor series to test accuracy of function
C              programs," W. J. Cody and L. Stoltz, submitted for
C              publication.
C
C  Latest modification: March 14, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C Acknowledgement: this program is a minor modification of the test
C          driver for RIBESL whose primary author was Laura Stoltz.
C
C----------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,II,III,IND,IOUT,IRND,IT,J,JT,J1,J2,K,KK,
     1    K1,K2,K3,LAST,M,MACHEP,MAXEXP,MB,MINEXP,MVR,N,NCALC,NDUM,
     2    NDX,NDX2,NEGEP,NGRD,NK,NO1,NUM
CS    REAL
      DOUBLE PRECISION
     1    A,AIT,ALBETA,ALPHA,ALPHSQ,A1,AR1,AR2,B,BETA,CONV,D,DEL,
     2    DELTA,DERIV,EPS,EPSNEG,F,FXMX,G,HALF,ONE,ONEP25,P875,REN,
     3    R6,R7,SIXTEN,SUM,TEN,TWO,TWOBPI,T1,T2,U,U2,W,X,XJ1,XL,XLAM,
     4    XMAX,XMB,XMIN,XN,X1,X99,Y,YSQ,Z,ZERO,ZZ
      DIMENSION AR1(11,6),AR2(13,9),G(5),NDX(24),NDX2(8),U(20),U2(20)
CS    DATA ZERO,HALF,ONE,ONEP25,TWO/0.0E0,0.5E0,1.0E0,1.25E0,2.0E0/,
CS   1    P875,TEN,SIXTEN,X99/0.875E0,10.0E0,1.6E1,-999.0E0/,
CS   2    XLAM,TWOBPI/1.03125E0,0.6366E0/
      DATA ZERO,HALF,ONE,ONEP25,TWO/0.0D0,0.5D0,1.0D0,1.25D0,2.0D0/,
     1    P875,TEN,SIXTEN,X99/0.875D0,10.0D0,1.6D1,-999.0D0/,
     2    XLAM,TWOBPI/1.03125D0,0.6366D0/
C----------------------------------------------------------------------
C  Arrays related to expansion of the derivatives in terms
C   of the first two Bessel functions.
C----------------------------------------------------------------------
      DATA  NDX/9,7,5,3,1,8,6,4,2,7,5,3,1,6,4,2,5,3,1,4,2,3,1,2/
      DATA  NDX2/5,9,13,16,19,21,23,24/
CS    DATA AR1/0.0E0,-1.0E0,0.0E0,1.0E0,0.0E0,1.0E0,-3.0E0,0.0E0,-2.0E0,
CS   1         1.2E1,0.0E0,1.0E0,0.0E0,-1.0E0,-1.0E0,2.0E0,0.0E0,
CS   2         2.0E0,-6.0E0,1.0E0,-7.0E0,2.4E1,0.0E0,0.0E0,1.0E0,0.0E0,
CS   3         -3.0E0,0.0E0,-2.0E0,1.1E1,0.0E0,1.2E1,-5.0E1,0.0E0,
CS   4         0.0E0,0.0E0,0.0E0,1.0E0,0.0E0,0.0E0,-6.0E0,0.0E0,-2.0E0,
CS   5         3.5E1,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,1.0E0,
CS   6         0.0E0,0.0E0,-1.0E1,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   7         0.0E0,0.0E0,0.0E0,0.0E0,1.0E0/
CS    DATA AR2/-1.0E0,9.0E0,-6.0E1,0.0E0,3.0E0,-5.1E1,3.6E2,0.0E0,
CS   1          1.0E0,-1.8E1,3.45E2,-2.52E3,0.0E0,0.0E0,-3.0E0,3.3E1,
CS   2          -1.2E2,-1.0E0,1.5E1,-1.92E2,7.2E2,0.0E0,4.0E0,-9.6E1,
CS   3          1.32E3,-5.04E3,0.0E0,3.0E0,-7.8E1,2.74E2,0.0E0,-2.7E1,
CS   4          5.7E2,-1.764E3,0.0E0,-4.0E0,2.46E2,-4.666E3,1.3068E4,
CS   5          0.0E0,0.0E0,1.8E1,-2.25E2,0.0E0,3.0E0,-1.5E2,1.624E3,
CS   6          0.0E0,0.0E0,-3.6E1,1.32E3,-1.3132E4,0.0E0,0.0E0,-3.0E0,
CS   7          8.5E1,0.0E0,0.0E0,4.5E1,-7.35E2,0.0E0,0.0E0,6.0E0,
CS   8          -5.5E2,6.769E3,0.0E0,0.0E0,0.0E0,-1.5E1,0.0E0,0.0E0,
CS   9          -3.0E0,1.75E2,0.0E0,0.0E0,0.0E0,6.0E1,-1.96E3,0.0E0,
CS   a          0.0E0,0.0E0,1.0E0,0.0E0,0.0E0,0.0E0,-2.1E1,0.0E0,0.0E0,
CS   b          0.0E0,-4.0E0,3.22E2,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   c          0.0E0,1.0E0,0.0E0,0.0E0,0.0E0,0.0E0,-2.8E1,0.0E0,0.0E0,
CS   d          0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   e          0.0E0,1.0E0/
      DATA AR1/0.0D0,-1.0D0,0.0D0,1.0D0,0.0D0,1.0D0,-3.0D0,0.0D0,-2.0D0,
     1         1.2D1,0.0D0,1.0D0,0.0D0,-1.0D0,-1.0D0,2.0D0,0.0D0,
     2         2.0D0,-6.0D0,1.0D0,-7.0D0,2.4D1,0.0D0,0.0D0,1.0D0,0.0D0,
     3         -3.0D0,0.0D0,-2.0D0,1.1D1,0.0D0,1.2D1,-5.0D1,0.0D0,
     4         0.0D0,0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,-6.0D0,0.0D0,-2.0D0,
     5         3.5D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,1.0D0,
     6         0.0D0,0.0D0,-1.0D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     7         0.0D0,0.0D0,0.0D0,0.0D0,1.0D0/
      DATA AR2/-1.0D0,9.0D0,-6.0D1,0.0D0,3.0D0,-5.1D1,3.6D2,0.0D0,
     1          1.0D0,-1.8D1,3.45D2,-2.52D3,0.0D0,0.0D0,-3.0D0,3.3D1,
     2          -1.2D2,-1.0D0,1.5D1,-1.92D2,7.2D2,0.0D0,4.0D0,-9.6D1,
     3          1.32D3,-5.04D3,0.0D0,3.0D0,-7.8D1,2.74D2,0.0D0,-2.7D1,
     4          5.7D2,-1.764D3,0.0D0,-4.0D0,2.46D2,-4.666D3,1.3068D4,
     5          0.0D0,0.0D0,1.8D1,-2.25D2,0.0D0,3.0D0,-1.5D2,1.624D3,
     6          0.0D0,0.0D0,-3.6D1,1.32D3,-1.3132D4,0.0D0,0.0D0,-3.0D0,
     7          8.5D1,0.0D0,0.0D0,4.5D1,-7.35D2,0.0D0,0.0D0,6.0D0,
     8          -5.5D2,6.769D3,0.0D0,0.0D0,0.0D0,-1.5D1,0.0D0,0.0D0,
     9          -3.0D0,1.75D2,0.0D0,0.0D0,0.0D0,6.0D1,-1.96D3,0.0D0,
     a          0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,-2.1D1,0.0D0,0.0D0,
     b          0.0D0,-4.0D0,3.22D2,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     c          0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,-2.8D1,0.0D0,0.0D0,
     d          0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     e          0.0D0,1.0D0/
      DATA IOUT/6/
C----------------------------------------------------------------------
C  Statement function for integer to float conversion
C----------------------------------------------------------------------
CS    CONV(NDUM) = REAL(NDUM)
      CONV(NDUM) = DBLE(NDUM)
C----------------------------------------------------------------------
C  Determine machine parameters and set constants
C----------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = ZERO
      B = TWO
      JT = 0
      DELTA = XLAM - ONE
      F = (DELTA) * (XLAM+ONE) * HALF
C----------------------------------------------------------------------
C  Random argument accuracy tests
C----------------------------------------------------------------------
      DO 300 J = 1, 4
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
  110       ALPHA = REN(JT)
C----------------------------------------------------------------------
C   Carefully purify arguments
C----------------------------------------------------------------------
            IF (J .LE. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            W = SIXTEN * Y
            T1 = W + Y
            T1 = W + T1
            Y = T1 - W
            Y = Y - W
            IF (J .LE. 1) THEN
                  X = Y * XLAM
                  MB = 15
               ELSE
                  X = Y + DELTA
                  MB = 2
            END IF
            CALL RYBESL(Y,ALPHA,MB,U2,NCALC)
            CALL RYBESL(X,ALPHA,MB,U,NCALC)
            IF (J .LE. 1) THEN
C----------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem.
C   First, filter out cases where function values are small.
C----------------------------------------------------------------------
CS            IF (SIGN(ONE,U(1))*SIGN(ONE,U2(1)) .LT. ZERO) GO TO 110
            IF (DSIGN(ONE,U(1))*DSIGN(ONE,U2(1)) .LT. ZERO) GO TO 110
                  D = -F*Y
                  MB = NCALC - 1
                  XMB = CONV(MB)
                  SUM = U2(MB+1)
                  Z = SUM
                  IND = MB
                  DO 125 II = 2, MB
                     SUM = SUM * D / XMB + U2(IND)
                     Z = Z * D / XMB
                     IND = IND - 1
                     XMB = XMB - ONE
  125             CONTINUE
                  Z = Z * D
C----------------------------------------------------------------------
C   Check for convergence.
C----------------------------------------------------------------------
                  IF (ABS(Z/U2(IND)) .GT. EPS) THEN
                     XL = XL + DEL
                     GO TO 200
                  END IF
                  ZZ = SUM * D + U2(IND)
C----------------------------------------------------------------------
C   Check for numerical stability.
C----------------------------------------------------------------------
                  D = ABS(ZZ/U2(IND))
                  IF ((D .GT. ONEP25) .OR. (D .LT. P875)) GO TO 110
                  ZZ = ZZ * XLAM ** ALPHA
               ELSE
C----------------------------------------------------------------------
C   Accuracy test is based on local Taylor's series expansion.
C   First, filter out cases where function values or derivatives
C   are small.
C----------------------------------------------------------------------
                  W = MIN(ABS(U(1)),ABS(U2(1)),ABS(U2(2)))
                  IF (W .LT. SQRT(TWOBPI/X)/SIXTEN) GO TO 110
                  IF (ABS(U(1)) .LT. ABS(U2(1))) THEN
                     Z = X
                     X = Y
                     Y = Z
                     DELTA = X - Y
                     DO 120 II = 1, 9
                        Z = U(II)
                        U(II) = U2(II)
                        U2(II) = Z
  120                CONTINUE
                  END IF
                  YSQ = Y * Y
                  ALPHSQ = ALPHA * ALPHA
                  MB = 8
                  J1 = MB
                  XJ1 = CONV(J1+1)
                  IEXP = 0
                  NK = 13
                  NUM = 2
                  DO 180 II = 1, MB
                     IF (NK .EQ. 0) THEN
                           NK = 11
                           NUM = 1
                     END IF
                     K = 9 - J1
                     IF (K .GT. 1) THEN
                           NO1 = NDX2(K-1) + 1
                        ELSE
                           NO1 = 1
                     END IF
                     MVR = NO1
                     LAST = NDX2(K)
                     K = LAST - NO1 + 1
C----------------------------------------------------------------------
C         Group I(ALPHA) terms in the derivative
C----------------------------------------------------------------------
                     DO 160 III = 1, K
                        J2 = NDX(MVR)
                        IF (NUM .EQ. 1) THEN
                              G(III) = AR1(NK,J2)
                           ELSE
                              G(III) = AR2(NK,J2)
                        END IF
                        IF (J2 .GT. 1) THEN
  157                         J2 = J2 - 1
                              IF (NUM .EQ. 1) THEN
                                    G(III) = G(III) * ALPHA + AR1(NK,J2)
                                 ELSE
                                    G(III) = G(III) * ALPHA + AR2(NK,J2)
                              END IF
                           IF (J2 .GT. 1) GO TO 157
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  160                CONTINUE
                     T1 = G(1)
                     DO 162 III = 2, K
                        T1 = T1 / YSQ + G(III)
  162                CONTINUE
                     IF (IEXP .EQ. 1) T1 = T1 / Y
C----------------------------------------------------------------------
C         Group I(ALPHA+1) terms in the derivative
C----------------------------------------------------------------------
                     IEXP = 1 - IEXP
                     NK = NK + K
                     MVR = NO1
                     KK = K
                     DO 165 III = 1, K
                        J2 = NDX(MVR)
                        M = MOD(J2,2)
                        IF (M .EQ. 1) J2 = J2 - 1
                        IF (J2 .GE. 2) THEN
                              IF (NUM .EQ. 1) THEN
                                    G(III) = AR1(NK,J2)
                                 ELSE
                                    G(III) = AR2(NK,J2)
                              END IF
  163                         J2 = J2 - 2
                              IF (J2 .GE. 2) THEN
                                    IF (NUM .EQ. 1) THEN
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR1(NK,J2)
                                       ELSE
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR2(NK,J2)
                                    END IF
                                    GO TO 163
                              END IF
                           ELSE
                              KK = III - 1
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  165                CONTINUE
                     T2 = G(1)
                     DO 167 III = 2, KK
                        T2 = T2 / YSQ + G(III)
  167                CONTINUE
                     IF (IEXP .EQ. 1) T2 = T2 / Y
                     DERIV = U2(1) * T1 - U2(2) * T2
                     IF (J1 .EQ. 8) THEN
                           SUM = DERIV
                        ELSE
                           SUM = SUM * DELTA / XJ1 + DERIV
                     END IF
                     J1 = J1 - 1
                     XJ1 = XJ1 - ONE
  180             CONTINUE
                  ZZ = SUM * DELTA + U2(1)
            END IF
            MB = 2
            Z = U(1)
C----------------------------------------------------------------------
C   Accumulate Results
C----------------------------------------------------------------------
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
                  A1 = ALPHA
                  FXMX = Z
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C----------------------------------------------------------------------
C   Gather and print statistics for test
C----------------------------------------------------------------------
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         IF (J .LE. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         IF (N .NE. 2000) WRITE (IOUT,1012) 2000-N
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(R6)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1,A1
         WRITE (IOUT,1024) FXMX
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
C----------------------------------------------------------------------
C   Initialize for next test
C----------------------------------------------------------------------
         A = B
         IF (J .EQ. 1) THEN
               B = TEN
            ELSE IF (J .EQ. 2) THEN
               B = B + B
            ELSE IF (J .EQ. 3) THEN
               A = B + TEN
               B = A + TEN
         END IF
  300 CONTINUE
C----------------------------------------------------------------------
C   Test of error returns
C
C   First, test with bad parameters
C----------------------------------------------------------------------
      WRITE (IOUT, 2006)
      X = ONE
      ALPHA = ONE + HALF
      MB = 5
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      ALPHA = HALF
      MB = -MB
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
C----------------------------------------------------------------------
C   Tests with small parameters
C----------------------------------------------------------------------
      IF (XMIN*XMAX .GT. ONE) THEN
            X = XMIN
         ELSE
            X = ONE / XMAX
      END IF
      ALPHA = ZERO
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      X = X + X + X
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      ALPHA = ONE - EPS
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
C----------------------------------------------------------------------
C   Last tests are with large parameters
C----------------------------------------------------------------------
      WRITE (IOUT, 2015)
      X = HALF / SQRT(EPS)
      ALPHA = HALF
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2012) X,ALPHA
      WRITE (IOUT, 2014) NCALC,U(1)
      X = X * SIXTEN
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2012) X,ALPHA
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
      WRITE (IOUT, 2020)
      STOP
C----------------------------------------------------------------------
 1000 FORMAT('1Test of Y(X,ALPHA) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of Y(X,ALPHA) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' Y(X,ALPHA) was larger',I6,' times,'/
     1    15X,' agreed',I6,' times, and'/
     1    11X,'was smaller',I6,' times.'//)
 1012 FORMAT(' NOTE: first ',I3,' arguments in test interval skipped'/
     1    7x,'because multiplication theorem did not converge'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3,' and NU =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(4x,'with Y(X,ALPHA) = ',E13.6E3)
 2006 FORMAT('1Check of Error Returns'///
     1    ' The following summarizes calls with indicated parameters'//
     2    ' NCALC different from MB indicates some form of error'//
     3    ' See documentation for RYBESL for details'//
     4    7X,'ARG',12X,'ALPHA',6X,'MB',6X,'B(1)',6X,'NCALC'//)
 2011 FORMAT(2E15.7E3,I5,E15.7E3,I5//)
 2012 FORMAT(' RYBESL will be called with the arguments',2E13.6E3)
 2013 FORMAT(' This should trigger an error message.')
 2014 FORMAT(' NCALC returned the value',I5/
     1    ' and RYBESL returned U(1) = ',E13.6E3/)
 2015 FORMAT(' Tests near the largest acceptable argument for RYBESL'/)
 2020 FORMAT(' This concludes the tests.')
C     ---------- Last line of RYBESL test program ----------
      END
