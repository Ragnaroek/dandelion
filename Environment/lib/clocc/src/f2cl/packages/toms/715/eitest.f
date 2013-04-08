      PROGRAM EITEST
C------------------------------------------------------------------
C FORTRAN 77 program to test EI, EONE, and EXPEI.
C
C  Method:
C
C     Accuracy test compare function values against local Taylor's
C     series expansions.  Derivatives for Ei(x) are generated from
C     the recurrence relation using a technique due to Gautschi
C     (see references).  Special argument tests are run with the
C     related functions E1(x) and exp(-x)Ei(x).
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
C              XMAX   - The largest finite floating-point number
C
C     REN(K) - A function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, AINT, DBLE, LOG, MAX, REAL, SQRT
C
C  References: "The use of Taylor series to test accuracy of
C               function programs", Cody, W. J., and Stoltz, L.,
C               submitted for publication.
C
C              "Recursive computation of certain derivatives -
C               A study of error propagation", Gautschi, W., and
C               Klein, B. J., Comm. ACM 13 (1970), 7-9.
C
C              "Remark on Algorithm 282", Gautschi, W., and Klein,
C               B. J., Comm. ACM 13 (1970), 53-54.
C
C  Latest modification: March 9, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,N1
CS    REAL
      DOUBLE PRECISION
     1    A,AIT,ALBETA,B,BETA,CONV,C1,D,DEL,DX,EN,EI,EONE,EPS,EPSNEG,
     2    EXPEI,FIV12,FOUR,FOURTH,ONE,P0625,REM,REN,R6,R7,SIX,SUM,
     3    TEN,TWO,U,V,W,X,XBIG,XC,XDEN,XL,XLGE,XMAX,XMIN,XN,XNP1,
     4    XNUM,X0,X99,Y,Z,ZERO
      DIMENSION D(0:25)
C------------------------------------------------------------------
CS    DATA ZERO,FOURTH,ONE,FOUR,SIX/0.0E0,0.25E0,1.0E0,4.0E0,6.0E0/,
CS   1     TEN,X0,X99,P0625/10.0E0,0.3725E0,-999.0E0,0.0625E0/,
CS   2     FIV12,REM/512.0E0,-7.424779065800051695596E-5/
      DATA ZERO,FOURTH,ONE,FOUR,SIX/0.0D0,0.25D0,1.0D0,4.0D0,6.0D0/,
     1     TEN,X0,X99,P0625/10.0D0,0.3725D0,-999.0D0,0.0625D0/,
     2     FIV12,REM/512.0D0,-7.424779065800051695596D-5/
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
      DX = -P0625
      A = FOURTH + DX
      B = X0 + DX
      N = 2000
      N1 = 25
      XN = CONV(N)
      JT = 0
C-----------------------------------------------------------------
C  Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 8
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            Y = DEL * REN(JT) + XL
            X = Y - DX
            Y = X + DX
C-----------------------------------------------------------------
C  Test Ei against series expansion
C-----------------------------------------------------------------
            V = EI(X)
            Z = EI(Y)
            SUM = ZERO
            U = X
            CALL DSUBN(U,N1,XMAX,D)
            EN = CONV(N1)+ONE
            SUM = D(N1)*DX/EN
            DO 100 K = N1,1,-1
               EN = EN-ONE
               SUM = (SUM + D(K-1))*DX/EN
  100       CONTINUE
            U = V + SUM
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
            W = Z - U
            W = W / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = Y
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
               DX = -DX
               A = X0 + DX
               B = SIX
            ELSE IF (J .LE. 4) THEN
               A = B
               B = B+B
            ELSE IF (J .EQ. 5) THEN
               A = -FOURTH
               B = -ONE
            ELSE IF (J .EQ. 6) THEN
               A = B
               B = -FOUR
            ELSE
               A = B
               B = -TEN
         END IF
  300 CONTINUE
C-----------------------------------------------------------------
C  Special tests.  First, check accuracy near the zero of Ei(x)
C-----------------------------------------------------------------
      WRITE (IOUT,1040)
      X = (FOUR - ONE) / (FOUR + FOUR)
      Y = EI(X)
      WRITE (IOUT,1041) X,Y
      Z = ((Y - (FOUR+ONE)/(FIV12)) - REM)/Y
      IF (Z .NE. ZERO) THEN
            W = LOG(ABS(Z))/ALBETA
         ELSE
            W = X99
      END IF
      WRITE (IOUT,1042) Z,IBETA,W
      W = MAX(AIT+W,ZERO)
      WRITE (IOUT,1022) IBETA,W
C-----------------------------------------------------------------
C  Check near XBIG, the largest argument acceptable to EONE, i.e.,
C    the negative of the smallest argument acceptable to EI.
C    Determine XBIG with Newton iteration on the equation
C                  EONE(x) = XMIN.
C---------------------------------------------------------------------
      WRITE (IOUT,1050)
      TWO = ONE+ONE
      V = SQRT(EPS)
      C1 = CONV(MINEXP) * LOG(BETA)
      XN = -C1
  320 XNUM = -XN - LOG(XN) + LOG(ONE+ONE/XN) - C1
      XDEN = -(XN*XN+XN+XN+TWO) / (XN*(XN+ONE))
      XNP1 = XN - XNUM/XDEN
      W = (XN-XNP1)/XNP1
      IF (ABS(W) .GT. V) THEN
         XN = XNP1
         GO TO 320
      END IF
      XBIG = XNP1
      X = AINT(TEN*XBIG) / TEN
      WRITE (IOUT,1052) X
      Y = EONE(X)
      WRITE (IOUT,1062) Y
      X = XBIG * (ONE+V)
      WRITE (IOUT,1053) X
      Y = EONE(X)
      WRITE (IOUT,1062) Y
C---------------------------------------------------------------------
C  Check near XMAX, the largest argument acceptable to EI.  Determine
C    XLGE with Newton iteration on the equation
C                  EI(x) = XMAX.
C---------------------------------------------------------------------
      C1 = CONV(MAXEXP) * LOG(BETA)
      XN = C1
  330 XNUM = XN - LOG(XN) + LOG(ONE+ONE/XN) - C1
      XDEN = (XN*XN-TWO) / (XN*(XN+ONE))
      XNP1 = XN - XNUM/XDEN
      W = (XN-XNP1)/XNP1
      IF (ABS(W) .GT. V) THEN
         XN = XNP1
         GO TO 330
      END IF
      XLGE = XNP1
      X = AINT(TEN*XLGE) / TEN
      WRITE (IOUT,1054) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
      X = XLGE * (ONE+V)
      WRITE (IOUT,1055) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
C---------------------------------------------------------------------
C  Check with XHUGE, the largest acceptable argument for EXPEI
C---------------------------------------------------------------------
      IF (XMIN*XMAX .LE. ONE) THEN
            X = XMAX
         ELSE
            X = ONE/XMIN
      END IF
      WRITE (IOUT,1056) X
      Y = EXPEI(X)
      WRITE (IOUT,1065) Y
      X = ZERO
      WRITE (IOUT,1055) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
      WRITE (IOUT,1100)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of Ei(x) vs series expansion'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT('     EI(X) was larger',I6,' times,')
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
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT('   EI (',E13.6E3,') = ',E13.6E3//)
 1042 FORMAT(' The relative error is',E15.4E3,' = ',I4,' **',F7.2/)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' EONE will be called with the argument',E13.6E3,/
     1    ' This should not underflow'//)
 1053 FORMAT(' EONE will be called with the argument',E13.6E3,/
     1    ' This should underflow'//)
 1054 FORMAT(' EI will be called with the argument',E13.6E3,/
     1    ' This should not overflow'//)
 1055 FORMAT(' EI will be called with the argument',E13.6E3,/
     1    ' This should overflow'//)
 1056 FORMAT(' EXPEI will be called with the argument',E13.6E3,/
     1    ' This should not underflow'//)
 1062 FORMAT(' EONE returned the value',E13.6E3///)
 1064 FORMAT(' EI returned the value',E13.6E3///)
 1065 FORMAT(' EXPEI returned the value',E13.6E3///)
 1100 FORMAT(' This concludes the tests')
C---------- Last line of EI test program ----------
      END
