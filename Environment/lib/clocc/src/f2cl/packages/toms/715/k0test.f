      PROGRAM K0TEST
C--------------------------------------------------------------------
C  FORTRAN 77 program to test BESK0
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
C  Author: Laura Stoltz
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C-----------------------------------------------------------------
      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,AMAXEXP,ATETEN,B,BESEK0,BESK0,BESK1,BETA,
     2   BOT,C,CONST,CONV,DEL,EIGHT,EPS,EPSNEG,FIVE,HALF,ONE,ONENEG,
     3   ONE28,PI,REN,R6,R7,SUM,T,TOP,TWENTY,TWO,U,W,X,XA,XB,XDEN,
     4   XL,XLAM,XLARGE,XMAX,XMB,XMIN,XN,XNINE,X1,Y,Z,ZERO,ZZ
      DIMENSION U(0:559)
CS    DATA ZERO,HALF,ONE,TWO,EIGHT/0.0E0,0.5E0,1.0E0,2.0E0,8.0E0/,
CS   1   XNINE,ATETEN,TWENTY,ONE28/9.0E0,18.0E0,20.0E0,128.0E0/,
CS   2   FIVE,ONENEG,XDEN,ALL9/5.0E0,-1.0E0,16.0E0,-999.0E0/,
CS   3   PI/3.141592653589793E0/
      DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/,
     1   XNINE,ATETEN,TWENTY,ONE28/9.0D0,18.0D0,20.0D0,128.0D0/,
     2   FIVE,ONENEG,XDEN,ALL9/5.0D0,-1.0D0,16.0D0,-999.0D0/,
     3   PI/3.141592653589793D0/
      DATA IOUT/6/
      TOP(X) = -X - HALF*LOG(TWO*X) + LOG(ONE-(ONE/EIGHT-XNINE/
     1   ONE28/X)/X)
      BOT(X) = (XDEN*X-ATETEN) / (((ONE28*X-XDEN)*X+XNINE)*X)
     1   - ONE - HALF/X
C------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C------------------------------------------------------------------
CS    CONV(NDUM) = REAL(NDUM)
      CONV(NDUM) = DBLE(NDUM)
C-----------------------------------------------------------------
C  Determine machine parameters and set constants
C-----------------------------------------------------------------
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
C-----------------------------------------------------------------
C     Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         IF (SFLAG) B = SQRT(EPS)
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
C------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem
C------------------------------------------------------------------
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
            T = U(0) * EPS
            W = (ONE-XLAM) * (ONE+XLAM)
            C = W *Y
            DO 110 II = 2, 60
               T = XMB * T / C
               Z = U(II-1)
               IF (Z .LT. T) THEN
                     GO TO 120
                  ELSE IF (U(II-1) .GT. ONE) THEN
                     IF ((XMB/Y) .GT. (XMAX/U(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO  200
                     END IF
               END IF
               U(II) = XMB/Y * U(II-1) + U(II-2)
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
  120       SUM = U(MB)
            IND = MB
            DO 155 II = 1, MB
               IND = IND - 1
               SUM = SUM * W * Y / XMB + U(IND)
               XMB = XMB - ONE
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            Z = BESK0(X)
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
C-----------------------------------------------------------------
C  Special tests
C-----------------------------------------------------------------
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESK0(XMIN)
      WRITE (IOUT,1032) Y
      Y = BESK0(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = REN(JT) * ONENEG
      Y = BESK0(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEK0(XMAX)
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
      Y = BESK0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * (XNINE / EIGHT)
      Y = BESK0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of K0(X) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(K0(X)) was larger',I6,' times,'/
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
 1032 FORMAT(' K0(XMIN) = ',E24.17E3/)
 1033 FORMAT(' K0(',I1,') = ',E24.17E3/)
 1034 FORMAT(' K0(',E24.17E3,' ) = ',E24.17E3/)
 1035 FORMAT(' E**X * K0(XMAX) = ',E24.17E3/)
C     ---------- Last line of BESK0 test program ----------
      END
