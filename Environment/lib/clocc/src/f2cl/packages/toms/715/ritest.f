
      PROGRAM RITEST
C----------------------------------------------------------------------
C FORTRAN 77 program to test RIBESL
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
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C----------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,II,III,IND,IOUT,IRND,IT,IZE,J,JT,J1,J2,K,
     1    KK,K1,K2,K3,LAST,M,MACHEP,MAXEXP,MB,MBORG,MINEXP,MVR,N,NCALC,
     2    NDUM,NDX,NDX2,NEGEP,NGRD,NK,NO1,NUM
CS    REAL
      DOUBLE PRECISION
     1    A,AIT,AK,AKK,ALBETA,ALPHA,ALPHSQ,A1,AR1,AR2,B,BETA,C,CONV,D,
     2    DEL,DELTA,DERIV,E,EPS,EPSNEG,F,G,HALF,HUND,ONE,REN,R6,R7,
     3    SIXTEN,SUM,TEN,TWO,T1,T2,U,U2,W,X,XBAD,XL,XLAM,XLARGE,XMAX,
     4    XMB,XMIN,XJ1,XN,X1,X99,Y,YSQ,Z,ZERO,ZZ
      DIMENSION AR1(11,6),AR2(13,9),G(5),NDX(24),NDX2(8),U(560),U2(560)
CS    DATA ZERO,HALF,ONE,TWO/0.0E0,0.5E0,1.0E0,2.0E0/,
CS   1    TEN,SIXTEN,HUND,X99/10.0E0,1.6E1,1.0E2,-999.0E0/,
CS   2    XLAM,XLARGE/1.03125E0,1.0E4/,
CS   3    C/0.9189385332E0/
      DATA ZERO,HALF,ONE,TWO/0.0D0,0.5D0,1.0D0,2.0D0/,
     1    TEN,SIXTEN,HUND,X99/10.0D0,1.6D1,1.0D2,-999.0D0/,
     2    XLAM,XLARGE/1.03125D0,1.0D4/,
     3    C/0.9189385332D0/
C----------------------------------------------------------------------
C  Arrays related to expansion of the derivatives in terms
C   of the first two Bessel functions.
C----------------------------------------------------------------------
      DATA  NDX/9,7,5,3,1,8,6,4,2,7,5,3,1,6,4,2,5,3,1,4,2,3,1,2/
      DATA  NDX2/5,9,13,16,19,21,23,24/
CS    DATA  AR1/0.0E0,1.0E0,0.0E0,-1.0E0,0.0E0,1.0E0,3.0E0,0.0E0,-2.0E0,
CS   1          -1.2E1,0.0E0,1.0E0,0.0E0,-1.0E0,1.0E0,2.0E0,0.0E0,
CS   2          -2.0E0,-6.0E0,1.0E0,7.0E0,2.4E1,0.0E0,0.0E0,1.0E0,0.0E0,
CS   3          -3.0E0,0.0E0,2.0E0,1.1E1,0.0E0,-1.2E1,-5.0E1,0.0E0,
CS   4          0.0E0,0.0E0,0.0E0,1.0E0,0.0E0,0.0E0,-6.0E0,0.0E0,2.0E0,
CS   5          3.5E1,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,1.0E0,
CS   6          0.0E0,0.0E0,-1.0E1,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   7          0.0E0,0.0E0,0.0E0,0.0E0,1.0E0/
CS    DATA  AR2/1.0E0,9.0E0,6.0E1,0.0E0,-3.0E0,-5.1E1,-3.6E2,0.0E0,
CS   1          1.0E0,1.8E1,3.45E2,2.52E3,0.0E0,0.0E0,-3.0E0,-3.3E1,
CS   2          -1.2E2,1.0E0,1.5E1,1.92E2,7.2E2,0.0E0,-4.0E0,-9.6E1,
CS   3          -1.32E3,-5.04E3,0.0E0,3.0E0,7.8E1,2.74E2,0.0E0,-2.7E1,
CS   4          -5.7E2,-1.764E3,0.0E0,4.0E0,2.46E2,4.666E3,1.3068E4,
CS   5          0.0E0,0.0E0,-1.8E1,-2.25E2,0.0E0,3.0E0,1.5E2,1.624E3,
CS   6          0.0E0,0.0E0,-3.6E1,-1.32E3,-1.3132E4,0.0E0,0.0E0,3.0E0,
CS   7          8.5E1,0.0E0,0.0E0,-4.5E1,-7.35E2,0.0E0,0.0E0,6.0E0,
CS   8          5.5E2,6.769E3,0.0E0,0.0E0,0.0E0,-1.5E1,0.0E0,0.0E0,
CS   9          3.0E0,1.75E2,0.0E0,0.0E0,0.0E0,-6.0E1,-1.96E3,0.0E0,
CS   a          0.0E0,0.0E0,1.0E0,0.0E0,0.0E0,0.0E0,-2.1E1,0.0E0,0.0E0,
CS   b          0.0E0,4.0E0,3.22E2,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   c          0.0E0,1.0E0,0.0E0,0.0E0,0.0E0,0.0E0,-2.8E1,0.0E0,0.0E0,
CS   d          0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
CS   e          0.0E0,1.0E0/
      DATA  AR1/0.0D0,1.0D0,0.0D0,-1.0D0,0.0D0,1.0D0,3.0D0,0.0D0,-2.0D0,
     1          -1.2D1,0.0D0,1.0D0,0.0D0,-1.0D0,1.0D0,2.0D0,0.0D0,
     2          -2.0D0,-6.0D0,1.0D0,7.0D0,2.4D1,0.0D0,0.0D0,1.0D0,0.0D0,
     3          -3.0D0,0.0D0,2.0D0,1.1D1,0.0D0,-1.2D1,-5.0D1,0.0D0,
     4          0.0D0,0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,-6.0D0,0.0D0,2.0D0,
     5          3.5D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,1.0D0,
     6          0.0D0,0.0D0,-1.0D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     7          0.0D0,0.0D0,0.0D0,0.0D0,1.0D0/
      DATA  AR2/1.0D0,9.0D0,6.0D1,0.0D0,-3.0D0,-5.1D1,-3.6D2,0.0D0,
     1          1.0D0,1.8D1,3.45D2,2.52D3,0.0D0,0.0D0,-3.0D0,-3.3D1,
     2          -1.2D2,1.0D0,1.5D1,1.92D2,7.2D2,0.0D0,-4.0D0,-9.6D1,
     3          -1.32D3,-5.04D3,0.0D0,3.0D0,7.8D1,2.74D2,0.0D0,-2.7D1,
     4          -5.7D2,-1.764D3,0.0D0,4.0D0,2.46D2,4.666D3,1.3068D4,
     5          0.0D0,0.0D0,-1.8D1,-2.25D2,0.0D0,3.0D0,1.5D2,1.624D3,
     6          0.0D0,0.0D0,-3.6D1,-1.32D3,-1.3132D4,0.0D0,0.0D0,3.0D0,
     7          8.5D1,0.0D0,0.0D0,-4.5D1,-7.35D2,0.0D0,0.0D0,6.0D0,
     8          5.5D2,6.769D3,0.0D0,0.0D0,0.0D0,-1.5D1,0.0D0,0.0D0,
     9          3.0D0,1.75D2,0.0D0,0.0D0,0.0D0,-6.0D1,-1.96D3,0.0D0,
     a          0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,-2.1D1,0.0D0,0.0D0,
     b          0.0D0,4.0D0,3.22D2,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
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
C----------------------------------------------------------------------
C  Determine the number of terms needed for convergence of the series
C  used in the multiplication theorem.  Use Newton iteration on the
C  asymptotic form of the convergence check for I0(X).
C----------------------------------------------------------------------
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
         A1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            MB = MBORG
            X = DEL * REN(JT) + XL
            ALPHA = REN(JT)
            IZE = 1
C----------------------------------------------------------------------
C   Carefully purify arguments
C----------------------------------------------------------------------
            IF (J .EQ. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            W = SIXTEN * Y
            T1 = W + Y
            T1 = W + T1
            Y = T1 - W
            Y = Y - W
            IF (J .EQ. 1) THEN
                  X = Y * XLAM
               ELSE
                  X = Y + DELTA
            END IF
            CALL RIBESL(Y,ALPHA,MB,IZE,U2,NCALC)
            IF (J .EQ. 1) THEN
C----------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem
C----------------------------------------------------------------------
                  D = F*Y
                  MB = NCALC - 2
                  XMB = CONV(MB)
                  SUM = U2(MB+1)
                  IND = MB
                  DO 155 II = 2, MB
                     SUM = SUM * D / XMB + U2(IND)
                     IND = IND - 1
                     XMB = XMB - ONE
  155             CONTINUE
                  ZZ = SUM * D + U2(IND)
                  ZZ = ZZ * XLAM ** ALPHA
               ELSE
C----------------------------------------------------------------------
C   Accuracy test is based on local Taylor's series expansion
C----------------------------------------------------------------------
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
                     DERIV = U2(1) * T1 + U2(2) * T2
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
            CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
            Z = U(1)
C----------------------------------------------------------------------
C   Accumulate Results
C----------------------------------------------------------------------
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
                  A1 = ALPHA
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C----------------------------------------------------------------------
C   Gather and print statistics for test
C----------------------------------------------------------------------
         K2 = N - K1 - K3
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
         WRITE (IOUT,1021) R6,IBETA,W,X1,A1
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
         B = B + B
         IF (J .EQ. 2) B = TEN
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
      IZE = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = HALF
      MB = -MB
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      MB = -MB
      IZE = 5
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
C----------------------------------------------------------------------
C   Last tests are with extreme parameters
C----------------------------------------------------------------------
      X = ZERO
      ALPHA = REN(JT)
      MB = 2
      IZE = 1
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = ZERO
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = ONE
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = -ONE
      ALPHA = HALF
      MB = 5
      IZE = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
C----------------------------------------------------------------------
C   Determine largest safe argument for scaled functions
C----------------------------------------------------------------------
      WRITE (IOUT, 2015)
      X = XLARGE * (ONE - SQRT(SQRT(EPS)))
      IZE = 2
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2014) NCALC,U(1)
      X = XLARGE * (ONE + SQRT(SQRT(EPS)))
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
C----------------------------------------------------------------------
C   Determine largest safe argument for unscaled functions
C----------------------------------------------------------------------
      WRITE (IOUT, 2016)
      N = INT(LOG(XMAX))
      Z = CONV(N)
      X = Z * (ONE - SQRT(SQRT(EPS)))
      IZE = 1
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2014) NCALC,U(1)
      X = Z * (ONE + SQRT(SQRT(EPS)))
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
      WRITE (IOUT, 2020)
      STOP
C----------------------------------------------------------------------
 1000 FORMAT('1Test of I(X,ALPHA) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of I(X,ALPHA) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' I(X,ALPHA) was larger',I6,' times,'/
     1    15X,' agreed',I6,' times, and'/
     1    11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3,' and NU =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 2006 FORMAT('1Check of Error Returns'///
     1    ' The following summarizes calls with indicated parameters'//
     2    ' NCALC different from MB indicates some form of error'//
     3    ' See documentation for RIBESL for details'//
     4    7X,'ARG',12X,'ALPHA',6X,'MB',3X,'IZ',7X,'RES',6X,'NCALC'//)
 2011 FORMAT(2E15.7E3,2I5,E15.7E3,I5//)
 2012 FORMAT(' RIBESL will be called with the argument',E13.6E3)
 2013 FORMAT(' This should trigger an error message.')
 2014 FORMAT(' NCALC returned the value',I5/
     1    ' and RIBESL returned the value',E13.6E3/)
 2015 FORMAT(' Tests near the largest argument for scaled functions'/)
 2016 FORMAT(' Tests near the largest argument for unscaled functions'/)
 2020 FORMAT(' This concludes the tests.')
C     ---------- Last line of RIBESL test program ----------
      END
