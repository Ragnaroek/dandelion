      PROGRAM K1TEST
C--------------------------------------------------------------------
C  FORTRAN 77 program to test BESK1
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
C              be deleted provided the following three
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest positive power of BETA
C                          that overflows
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest non-vanishing normalized
C                          floating-point power of the radix, i.e.,
C                          XMIN = FLOAT(IBETA) ** MINEXP
C                 XMAX   - the largest finite floating-point number.
C                          In particular XMAX = (1.0-EPSNEG) *
C                          FLOAT(IBETA) ** MAXEXP
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  User defined functions
C
C         BOT, TOP
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 14, 1992
C
C  Author - Laura Stoltz
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C--------------------------------------------------------------------
      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,AMAXEXP,B,BESEK1,BESK0,BESK1,BETA,BOT,C,
     2   CONST,CONV,DEL,EIGHT,EPS,EPSNEG,FIFTEN,FIVE,FOUR8,HALF,HUND,
     3   ONE,ONENEG,ONE28,PI,REN,R6,R7,SUM,T,T1,THIRTY,THREE,TOP,
     4   TWENTY,TWO,U,W,X,XA,XB,XDEN,XL,XLAM,XLARGE,XLEAST,XMAX,XMB,
     5   XMIN,XN,XNINE,X1,Y,Z,ZERO,ZZ
      DIMENSION U(0:559)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
CS    DATA ZERO,HALF,ONE,TWO,EIGHT/0.0E0,0.5E0,1.0E0,2.0E0,8.0E0/,
CS   1   XNINE,TWENTY,ONE28/9.0E0,20.0E0,128.0E0/,
CS   2   FIVE,ONENEG,XDEN,ALL9/5.0E0,-1.0E0,16.0E0,-999.0E0/,
CS   3   THREE,FIFTEN,THIRTY,FOUR8/3.0E0,15.0E0,30.0E0,48.0E0/,
CS   4   PI/3.141592653589793E0/,HUND/100.0E0/
      DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/,
     1   XNINE,TWENTY,ONE28/9.0D0,20.0D0,128.0D0/,
     2   FIVE,ONENEG,XDEN,ALL9/5.0D0,-1.0D0,16.0D0,-999.0D0/,
     3   THREE,FIFTEN,THIRTY,FOUR8/3.0D0,15.0D0,30.0D0,48.0D0/,
     4   PI/3.141592653589793D0/,HUND/100.0D0/
C---------------------------------------------------------------------
C  Machine-dependent constant
C---------------------------------------------------------------------
CS    DATA XLEAST/1.18E-38/
      DATA XLEAST/2.23D-308/
      DATA IOUT/6/
      TOP(X) = -X - HALF*LOG(TWO*X) + LOG(ONE+(THREE/EIGHT-FIFTEN/
     1   ONE28/X)/X)
      BOT(X) = - ONE - HALF/X + ((- FOUR8*X + THIRTY) /
     1   (((ONE28*X+FOUR8)*X-FIFTEN)*X))
C---------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C---------------------------------------------------------------------
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
      AMAXEXP = CONV(MAXEXP)
      JT = 0
      B = EPS
      XLAM = (XDEN - ONE) / XDEN
      CONST = HALF * LOG(PI) - LOG(XMIN)
C--------------------------------------------------------------------
C     Random argument accuracy tests
C--------------------------------------------------------------------
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
C---------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem
C---------------------------------------------------------------------
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            Y = X / XLAM
            W = XDEN * Y
            Y = (W + Y) - W
            X = Y * XLAM
            U(0) = BESK0(Y)
            U(1) = BESK1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
                  U(0) = U(0) * EPS
                  U(1) = U(1) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            W = (ONE-XLAM) * (ONE+XLAM)
            C = W *Y
            T = U(0) + C * U(1)
            T1 = EPS / HUND
            DO 110 II = 2, 60
               Z = U(II-1)
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (U(II-1) .GT. ONE) THEN
                     IF ((XMB/Y) .GT. (XMAX/U(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO  200
                     END IF
               END IF
               U(II) = XMB/Y * U(II-1) + U(II-2)
               IF (T1 .GT. ONE/EPS) THEN
                     T = T * T1
                     T1 = ONE
               END IF
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
  120       SUM = U(MB)
            IND = MB
            MB = MB - 1
            DO 155 II = 1, MB
               XMB = XMB - ONE
               IND = IND - 1
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            ZZ = ZZ * XLAM
            Z = BESK1(X)
            Y = Z
            IF (U(0) .GT. Y) Y= U(0)
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
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
         N = K1 + K2 + K3
         XN = CONV(N)
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = ALL9
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = ALL9
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
C--------------------------------------------------------------------
C  Special tests
C--------------------------------------------------------------------
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESK1(XLEAST)
      WRITE (IOUT,1032) Y
      Y = BESK1(XMIN)
      WRITE (IOUT,1036) Y
      Y = BESK1(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = REN(JT) * ONENEG
      Y = BESK1(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEK1(XMAX)
      WRITE (IOUT,1035) Y
      XA = LOG(XMAX)
  330 XB = XA - (TOP(XA)+CONST) / BOT(XA)
      IF (ABS(XB-XA)/XB .LE. EPS) THEN
            GO TO 350
         ELSE
            XA = XB
            GO TO 330
      END IF
  350 XLARGE = XB * XLAM
      Y = BESK1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * (XNINE / EIGHT)
      Y = BESK1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
C--------------------------------------------------------------------
C  Test of error returns
C--------------------------------------------------------------------
      STOP
 1000 FORMAT('1Test of K1(X) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(K1(X)) was larger',I6,' times,'/
     1    20X,' agreed',I6,' times, and'/
     1    16X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.'//)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4E3,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6E3)
 1025 FORMAT(' The root mean square absolute error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(//' Test with extreme arguments'/)
 1032 FORMAT(' K1(XLEAST) = ',E24.17E3/)
 1033 FORMAT(' K1(',I1,') = ',E24.17E3/)
 1034 FORMAT(' K1(',E24.17E3,' ) = ',E24.17E3/)
 1035 FORMAT(' E**X * K1(XMAX) = ',E24.17E3/)
 1036 FORMAT(' K1(XMIN) = ',E24.17E3/)
C---------- Last line of BESK0 test program ----------
      END
