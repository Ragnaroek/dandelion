
CS    REAL FUNCTION ANORM(ARG)
      DOUBLE PRECISION FUNCTION ANORM(ARG)
C------------------------------------------------------------------
C
C This function evaluates the normal distribution function:
C
C                              / x
C                     1       |       -t*t/2
C          P(x) = ----------- |      e       dt
C                 sqrt(2 pi)  |
C                             /-oo
C
C   The main computation evaluates near-minimax approximations
C   derived from those in "Rational Chebyshev approximations for
C   the error function" by W. J. Cody, Math. Comp., 1969, 631-637.
C   This transportable program uses rational functions that
C   theoretically approximate the normal distribution function to
C   at least 18 significant decimal digits.  The accuracy achieved
C   depends on the arithmetic system, the compiler, the intrinsic
C   functions, and proper selection of the machine-dependent
C   constants.
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants.  Let
C
C   XMIN  = the smallest positive floating-point number.
C
C Then the following machine-dependent constants must be declared
C   in DATA statements.  IEEE values are provided as a default.
C
C   EPS   = argument below which anorm(x) may be represented by
C           0.5  and above which  x*x  will not underflow.
C           A conservative value is the largest machine number X
C           such that   1.0 + X = 1.0   to machine precision.
C   XLOW  = the most negative argument for which ANORM does not
C           vanish.  This is the negative of the solution to
C                    W(x) * (1-1/x**2) = XMIN,
C           where W(x) = exp(-x*x/2)/[x*sqrt(2*pi)].
C   XUPPR = positive argument beyond which anorm = 1.0.  A
C           conservative value is the solution to the equation
C                    exp(-x*x/2) = EPS,
C           i.e., XUPPR = sqrt[-2 ln(eps)].
C
C   Approximate values for some important machines are:
C
C                          XMIN        EPS        XLOW    XUPPR
C
C  CDC 7600      (S.P.)  3.13E-294   7.11E-15   -36.641   8.072
C  CRAY-1        (S.P.)  4.58E-246   7.11E-157 -106.521  26.816
C  IEEE (IBM/XT,
C    SUN, etc.)  (S.P.)  1.18E-38    5.96E-8    -12.949   5.768
C  IEEE (IBM/XT,
C    SUN, etc.)  (D.P.)  2.23D-308   1.11D-16   -37.519   8.572
C  IBM 195       (D.P.)  5.40D-79    1.39D-17   -18.781   8.811
C  VAX D-Format  (D.P.)  2.94D-39    1.39D-17   -13.055   8.811
C  VAX G-Format  (D.P.)  5.56D-309   1.11D-16   -37.556   8.572
C
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns  ANORM = 0     for  ARG .LE. XLOW.
C
C
C Intrinsic functions required are:
C
C     ABS, AINT, EXP
C
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C  Latest modification: March 15, 1992
C
C------------------------------------------------------------------
      INTEGER I
CS    REAL
      DOUBLE PRECISION
     1     A,ARG,B,C,D,DEL,EPS,HALF,P,ONE,Q,RESULT,SIXTEN,
     2     SQRPI,THRSH,ROOT32,X,XLOW,XDEN,XNUM,Y,XSQ,XUPPR,ZERO
      DIMENSION A(5),B(4),C(9),D(8),P(6),Q(5)
C------------------------------------------------------------------
C  Mathematical constants
C
C  SQRPI = 1 / sqrt(2*pi), ROOT32 = sqrt(32), and
C  THRSH is the argument for which anorm = 0.75.
C------------------------------------------------------------------
CS    DATA ONE,HALF,ZERO,SIXTEN/1.0E0,0.5E0,0.0E0,1.60E1/,
CS   1     SQRPI/3.9894228040143267794E-1/,THRSH/0.66291E0/,
CS   2     ROOT32/5.656854248E0/
      DATA ONE,HALF,ZERO,SIXTEN/1.0D0,0.5D0,0.0D0,1.60D1/,
     1     SQRPI/3.9894228040143267794D-1/,THRSH/0.66291D0/,
     2     ROOT32/5.656854248D0/
C------------------------------------------------------------------
C  Machine-dependent constants
C------------------------------------------------------------------
CS    DATA EPS/5.96E-8/,XLOW/-12.949E0/,XUPPR/5.768E0/
      DATA EPS/1.11D-16/,XLOW/-37.519D0/,XUPPR/8.572D0/
C------------------------------------------------------------------
C  Coefficients for approximation in first interval
C------------------------------------------------------------------
CS    DATA A/2.2352520354606839287E00,1.6102823106855587881E02,
CS   1       1.0676894854603709582E03,1.8154981253343561249E04,
CS   2       6.5682337918207449113E-2/
CS    DATA B/4.7202581904688241870E01,9.7609855173777669322E02,
CS   1       1.0260932208618978205E04,4.5507789335026729956E04/
      DATA A/2.2352520354606839287D00,1.6102823106855587881D02,
     1       1.0676894854603709582D03,1.8154981253343561249D04,
     2       6.5682337918207449113D-2/
      DATA B/4.7202581904688241870D01,9.7609855173777669322D02,
     1       1.0260932208618978205D04,4.5507789335026729956D04/
C------------------------------------------------------------------
C  Coefficients for approximation in second interval
C------------------------------------------------------------------
CS    DATA C/3.9894151208813466764E-1,8.8831497943883759412E00,
CS   1       9.3506656132177855979E01,5.9727027639480026226E02,
CS   2       2.4945375852903726711E03,6.8481904505362823326E03,
CS   3       1.1602651437647350124E04,9.8427148383839780218E03,
CS   4       1.0765576773720192317E-8/
CS    DATA D/2.2266688044328115691E01,2.3538790178262499861E02,
CS   1       1.5193775994075548050E03,6.4855582982667607550E03,
CS   2       1.8615571640885098091E04,3.4900952721145977266E04,
CS   3       3.8912003286093271411E04,1.9685429676859990727E04/
      DATA C/3.9894151208813466764D-1,8.8831497943883759412D00,
     1       9.3506656132177855979D01,5.9727027639480026226D02,
     2       2.4945375852903726711D03,6.8481904505362823326D03,
     3       1.1602651437647350124D04,9.8427148383839780218D03,
     4       1.0765576773720192317D-8/
      DATA D/2.2266688044328115691D01,2.3538790178262499861D02,
     1       1.5193775994075548050D03,6.4855582982667607550D03,
     2       1.8615571640885098091D04,3.4900952721145977266D04,
     3       3.8912003286093271411D04,1.9685429676859990727D04/
C------------------------------------------------------------------
C  Coefficients for approximation in third interval
C------------------------------------------------------------------
CS    DATA P/2.1589853405795699E-1,1.274011611602473639E-1,
CS   1       2.2235277870649807E-2,1.421619193227893466E-3,
CS   2       2.9112874951168792E-5,2.307344176494017303E-2/
CS    DATA Q/1.28426009614491121E00,4.68238212480865118E-1,
CS   1       6.59881378689285515E-2,3.78239633202758244E-3,
CS   2       7.29751555083966205E-5/
      DATA P/2.1589853405795699D-1,1.274011611602473639D-1,
     1       2.2235277870649807D-2,1.421619193227893466D-3,
     2       2.9112874951168792D-5,2.307344176494017303D-2/
      DATA Q/1.28426009614491121D00,4.68238212480865118D-1,
     1       6.59881378689285515D-2,3.78239633202758244D-3,
     2       7.29751555083966205D-5/
C------------------------------------------------------------------
      X = ARG
      Y = ABS(X)
      IF (Y .LE. THRSH) THEN
C------------------------------------------------------------------
C  Evaluate  anorm  for  |X| <= 0.66291
C------------------------------------------------------------------
            XSQ = ZERO
            IF (Y .GT. EPS) XSQ = X * X
            XNUM = A(5)*XSQ
            XDEN = XSQ
            DO 20 I = 1, 3
               XNUM = (XNUM + A(I)) * XSQ
               XDEN = (XDEN + B(I)) * XSQ
   20       CONTINUE
            RESULT = X * (XNUM + A(4)) / (XDEN + B(4))
            RESULT = HALF + RESULT
C------------------------------------------------------------------
C  Evaluate  anorm  for 0.66291 <= |X| <= sqrt(32)
C------------------------------------------------------------------
         ELSE IF (Y .LE. ROOT32) THEN
            XNUM = C(9)*Y
            XDEN = Y
            DO 120 I = 1, 7
               XNUM = (XNUM + C(I)) * Y
               XDEN = (XDEN + D(I)) * Y
  120       CONTINUE
            RESULT = (XNUM + C(8)) / (XDEN + D(8))
            XSQ = AINT(Y*SIXTEN)/SIXTEN
            DEL = (Y-XSQ)*(Y+XSQ)
            RESULT = EXP(-XSQ*XSQ*HALF)*EXP(-DEL*HALF)*RESULT
            IF (X .GT. ZERO) RESULT = ONE - RESULT
C------------------------------------------------------------------
C  Evaluate  anorm  for |X| > sqrt(32)
C------------------------------------------------------------------
         ELSE
            RESULT = ZERO
            IF ((X .GE. XLOW) .AND. (X .LT. XUPPR)) THEN
               XSQ = ONE / (X * X)
               XNUM = P(6)*XSQ
               XDEN = XSQ
               DO 240 I = 1, 4
                  XNUM = (XNUM + P(I)) * XSQ
                  XDEN = (XDEN + Q(I)) * XSQ
  240          CONTINUE
               RESULT = XSQ *(XNUM + P(5)) / (XDEN + Q(5))
               RESULT = (SQRPI -  RESULT) / Y
               XSQ = AINT(X*SIXTEN)/SIXTEN
               DEL = (X-XSQ)*(X+XSQ)
               RESULT = EXP(-XSQ*XSQ*HALF)*EXP(-DEL*HALF)*RESULT
            END IF
            IF (X .GT. ZERO) RESULT = ONE - RESULT
      END IF
C------------------------------------------------------------------
C  Fix up for negative argument, erf, etc.
C------------------------------------------------------------------
      ANORM = RESULT
C---------- Last card of ANORM ----------
      END
