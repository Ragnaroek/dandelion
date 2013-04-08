      PROGRAM Y1TEST
C--------------------------------------------------------------------
C  Fortran 77 program to test BESY1
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
C              be deleted provided the following six
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest integer such that
C                          FLOAT(IBETA)**MAXEXP causes overflow
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest positive normalized
C                          floating-point power of the radix
C                 XMAX   - the largest finite floating-point
C                          number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, FLOAT, LOG, MAX, SIGN, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 13, 1992
C
C  Authors: George Zazi, W. J. Cody
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C--------------------------------------------------------------------
      LOGICAL SFLAG, TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MBM1,MBP1,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BESY0,BESY1,BETA,C,CONV,DEL,EIGHT,
     2   EPS,EPSNEG,FOUR,HALF,HUND,ONE,REN,R6,R7,SIXTEN,SUM,T,
     3   TWENTY,TWO56,T1,U,W,X,XI,XL,XLAM,XMAX,XMB,XMIN,X1,Y,YX,
     4   Z,ZERO,ZZ
      DIMENSION U(560),XI(2),YX(2)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
      DATA IOUT/6/
CS    DATA ZERO,ONE,FOUR/0.0E0,1.0E0,4.0E0/,
CS   1   EIGHT,TWENTY,ALL9,TWO56/8.0E0,20.0E0,-999.0E0,256.0E0/,
CS   2   HALF,SIXTEN,XLAM,HUND/0.5E0,16.0E0,0.9375E0,100.0E0/
      DATA ZERO,ONE,FOUR/0.0D0,1.0D0,4.0D0/,
     1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
     2   HALF,SIXTEN,XLAM,HUND/0.5D0,16.0D0,0.9375D0,100.0D0/
C--------------------------------------------------------------------
C  Zeroes of Y1
C--------------------------------------------------------------------
CS    DATA XI/562.0E0,1390.0E0/
CS    DATA YX(1)/-9.5282 39309 77227 03132 E-4/
CS   1     YX(2)/-2.1981 83008 05980 02741 E-6/
      DATA XI/562.0D0,1390.0D0/
      DATA YX(1)/-9.5282393097722703132D-4/,
     1     YX(2)/-2.1981830080598002741D-6/
C--------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C--------------------------------------------------------------------
CS    CONV(NDUM) = FLOAT(NDUM)
      CONV(NDUM) = DBLE(FLOAT(NDUM))
C--------------------------------------------------------------------
C  Determine machine parameters and set constants
C--------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
      B = EPS
C--------------------------------------------------------------------
C  Random argument accuracy tests (based on the
C    multiplication theorem)
C--------------------------------------------------------------------
      DO 300 J = 1, 3
         SFLAG = (J .EQ. 1) .AND. (MAXEXP/IT .LE. 5)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = FOUR
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
               N = 500
         END IF
         DEL = (B - A) / CONV(N)
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
C--------------------------------------------------------------------
C  Carefully purify arguments and evaluate identities
C--------------------------------------------------------------------
            Y = X/XLAM
            W = SIXTEN * Y
            Y = (W + Y) - W
            X = Y * XLAM
            U(1) = BESY0(Y)
            U(2) = BESY1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
               U(1) = U(1) * EPS
               U(2) = U(2) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            W = (ONE-XLAM)*(ONE+XLAM)
            C = W * Y
            T = ABS(XLAM*U(2))
            T1 = EPS/HUND
            DO 110 II = 3, 60
               U(II) = XMB/Y * U(II-1) - U(II-2)
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
               MBP1=MB + 1
               Z = ABS(U(II))
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (Y .LT. XMB) THEN
                     IF (Z .GT. XMAX*(Y/XMB)) THEN
                        A= X
                        XL=XL+DEL
                        GO TO  200
                     END IF
               END IF
               IF (T1 .GT. ONE/EPS) THEN
                  T = T * T1
                  T1 = ONE
               END IF
  110       CONTINUE
  120       SUM = U(MBP1)
            IND = MBP1
            MBM1 =MB-1
            DO 155 II = 1, MBM1
               IND = IND - 1
               XMB = XMB - ONE
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM*XLAM
            IF (TFLAG) ZZ = ZZ / EPS
            Z = BESY1(X)
            Y = Z
            IF (ABS(U(2)) .GT. ABS(Y)) Y =  U(2)
C--------------------------------------------------------------------
C  Accumulate results
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
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C--------------------------------------------------------------------
C  Process and output statistics
C--------------------------------------------------------------------
         N = K1 + K2 + K3
         R7 = SQRT(R7/CONV(N))
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         IF (J .EQ. 4) THEN
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
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 2
         X = XI(I)/TWO56
         Y = BESY1(X)
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) THEN
               W = LOG(ABS(T))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
C--------------------------------------------------------------------
C  Test of error returns
C--------------------------------------------------------------------
      WRITE (IOUT,1033)
      X = XMIN
      IF (XMIN*XMAX .LT. ONE) THEN
            WRITE (IOUT,1034) X
         ELSE
            WRITE (IOUT,1035) X
      END IF
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      X = -ONE
      WRITE (IOUT,1034) X
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      STOP
C--------------------------------------------------------------------
 1000 FORMAT('1Test of Y1(X) VS Multiplication Theorem'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 '(',F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(Y1(X)) was larger',I6,' times', /
     1     15X,' agreed',I6,' times, and'/
     1   11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4E3,' = ',I4,' **',
     1  F7.2/4X,'occurred for X =',E13.6E3)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4E3,' = ',I4,' **',
     1  F7.2/4X,'occurred for X =',E13.6E3)
 1025 FORMAT(' The root mean square absolute error was',E15.4E3,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near zeros',//,10X,'X',15X,'BESY1(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10E3,E25.15E3,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'///)
 1034 FORMAT(' Y1 will be called with the argument ',E17.10E3/
     1     ' This may stop execution.'//)
 1035 FORMAT(' Y1 will be called with the argument ',E17.10E3/
     1     ' This should not stop execution.'//)
 1036 FORMAT(' Y1 returned the value',E25.17E3/)
 1100 FORMAT(' This concludes the tests.')
C---------- Last card of BESY1 test program ----------
      END
