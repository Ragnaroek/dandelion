C  ***  SIMPLE TEST PROGRAM FOR  GLGB AND  GLFB  ***
C
      INTEGER IV(92), LIV, LV, NOUT, UI(1)
      REAL B(2,2), V(200), X(2), UR(1)
      EXTERNAL I7MDCN, MADRJ, RHOLS
      INTEGER I7MDCN
C
C I7MDCN... RETURNS OUTPUT UNIT NUMBER.
C
      INTEGER LASTIV, LASTV, LMAX0
      PARAMETER (LASTIV=44, LASTV=45, LMAX0=35)
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      NOUT = I7MDCN(1)
      LV = 200
      LIV = 92
C
C  ***  SPECIFY INITIAL X AND BOUNDS ON X  ***
C
      X(1) = 3.E+0
      X(2) = 1.E+0
C     *** BOUNDS ON X(1)...
      B(1,1) = -.1E+0
      B(2,1) = 10.E+0
C     *** BOUNDS ON X(2)...
      B(1,2) =  0.E+0
      B(2,2) =  2.E+0
C
C  ***  SET IV(1) TO 0 TO FORCE ALL DEFAULT INPUT COMPONENTS TO BE USED.
C
       IV(1) = 0
C
       WRITE(NOUT,10)
 10    FORMAT('  GLGB ON PROBLEM MADSEN...')
C
C  ***  CALL  GLG, PASSING UI FOR RHOI, UR FOR RHOR, AND MADRJ FOR
C  ***  UFPARM (ALL UNUSED IN THIS EXAMPLE).
C
      CALL  GLGB(3, 2, 2, X, B, RHOLS, UI, UR, IV, LIV, LV, V, MADRJ,
     1           UI,UR, MADRJ)
C
C  ***  SEE HOW MUCH STORAGE  GLGB USED...
C
      WRITE(NOUT,20) IV(LASTIV), IV(LASTV)
 20   FORMAT('  GLGB NEEDED LIV .GE. ,I3,12H AND LV .GE.',I4)
C
C  ***  SOLVE THE SAME PROBLEM USING  GLFB...
C
      WRITE(NOUT,30)
 30   FORMAT(/'  GLFB ON PROBLEM MADSEN...')
      X(1) = 3.E+0
      X(2) = 1.E+0
      IV(1) = 0
      CALL  GLFB(3, 2, 2, X, B, RHOLS, UI, UR, IV, LIV, LV, V, MADRJ,
     1           UI,UR, MADRJ)
C
C  ***  REPEAT THE LAST RUN, BUT WITH A DIFFERENT INITIAL STEP BOUND
C
C  ***  FIRST CALL  IVSET TO GET DEFAULT IV AND V INPUT VALUES...
C
      CALL  IVSET(1, IV, LIV, LV, V)
C
C  ***  NOW ASSIGN THE NONDEFAULT VALUES.
C
      V(LMAX0) = 0.1E+0
      X(1) = 3.E+0
      X(2) = 1.E+0
C
      WRITE(NOUT,40)
 40   FORMAT(/'  GLFB ON PROBLEM MADSEN AGAIN...')
C
      CALL  GLFB(3, 2, 2, X, B, RHOLS, UI, UR, IV, LIV, LV, V, MADRJ,
     1           UI,UR, MADRJ)
C
      STOP
      END
C***********************************************************************
C
C     MADRJ
C
C***********************************************************************
      SUBROUTINE MADRJ(N, P, X, NF, NEED, R, RP, UI, UR, UF)
      INTEGER N, P, NF, NEED, UI(1)
      REAL X(P), R(N), RP(P,N), UR(1)
      EXTERNAL UF
      REAL TWO, ZERO
      PARAMETER (TWO=2.E+0, ZERO=0.E+0)
C
C *** BODY ***
C
      IF (NEED .EQ. 2) GO TO 10
      R(1) = X(1)**2 + X(2)**2 + X(1)*X(2)
      R(2) = SIN(X(1))
      R(3) = COS(X(2))
      GO TO 999
C
 10   RP(1,1) = TWO*X(1) + X(2)
      RP(2,1) = TWO*X(2) + X(1)
      RP(1,2) = COS(X(1))
      RP(2,2) = ZERO
      RP(1,3) = ZERO
      RP(2,3) = -SIN(X(2))
C
 999  RETURN
      END
      SUBROUTINE RHOLS(NEED, F, N, NF, XN, R, RP, UI, UR, W)
C
C *** LEAST-SQUARES RHO ***
C
      INTEGER NEED(2), N, NF, UI(1)
      REAL F, XN(*), R(N), RP(N), UR(1), W(N)
C
C *** EXTERNAL FUNCTIONS ***
C
      EXTERNAL  R7MDC,  V2NRM
      REAL  R7MDC,  V2NRM
C
C *** LOCAL VARIABLES ***
C
      INTEGER I
      REAL HALF, ONE, RLIMIT, ZERO
      DATA HALF/0.5E+0/, ONE/1.E+0/, RLIMIT/0.E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
      IF (NEED(1) .EQ. 2) GO TO 20
      IF (RLIMIT .LE. ZERO) RLIMIT =  R7MDC(5)
C     ** SET F TO 2-NORM OF R **
      F =  V2NRM(N, R)
      IF (F .GE. RLIMIT) GO TO 10
      F = HALF * F**2
      GO TO 999
C
C     ** COME HERE IF F WOULD OVERFLOW...
 10   NF = 0
      GO TO 999
C
 20   DO 30 I = 1, N
         RP(I) = ONE
         W(I) = ONE
 30      CONTINUE
 999  RETURN
C *** LAST LINE OF RHOLS FOLLOWS ***
      END
