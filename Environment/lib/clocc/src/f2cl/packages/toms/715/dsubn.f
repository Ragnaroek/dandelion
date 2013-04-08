      SUBROUTINE DSUBN(X,NMAX,XMAX,D)
C-------------------------------------------------------------------
C Translation of Gautschi'f CACM Algorithm 282 for
C   derivatives of Ei(x).
C
C  Intrinsic functions required are:
C
C      ABS, EXP, INT, LOG, MIN
C
C-------------------------------------------------------------------
      LOGICAL BOOL1, BOOL2
      INTEGER J,NMAX,N0,MINI,N,N1,LIM
CS    REAL
      DOUBLE PRECISION
     1     B0,B1,B2,B3,B4,B5,B6,C0,C1,D,E,EN,ONE,P,Q,T,TEN,
     2     TWO,X,XMAX,X1,Z,ZERO
      DIMENSION D(0:NMAX)
CS    DATA ZERO/0.0E0/,ONE/1.0E0/,TWO/2.0E0/,TEN/10.0E0/
CS    DATA C0/2.7183E0/,C1/4.67452E1/
CS    DATA B0/5.7941E-5/,B1/-1.76148E-3/,B2/2.08645E-2/,
CS   1     B3/-1.29013E-1/,B4/8.5777E-1/,B5/1.0125E0/,B6/7.75E-1/
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,TEN/10.0D0/
      DATA C0/2.7183D0/,C1/4.67452D1/
      DATA B0/5.7941D-5/,B1/-1.76148D-3/,B2/2.08645D-2/,
     1     B3/-1.29013D-1/,B4/8.5777D-1/,B5/1.0125D0/,B6/7.75D-1/
C-------------------------------------------------------------------
      X1 = ABS(X)
      N0 = INT(X1)
      E = EXP(X)
      D(0) = E/X
      BOOL1 = (X .LT. ZERO) .OR. (X1 .LE. TWO)
      BOOL2 = N0 .LT. NMAX
      MINI = MIN(N0,NMAX)
      IF (BOOL1) THEN
            LIM = NMAX
         ELSE
            LIM = MINI
      END IF
      N = 1
      EN = ONE
   50 D(N) = (E - EN*D(N-1))/X
         N = N +1
         EN = EN + ONE
         IF (X1 .LT. ONE) THEN
               IF ((ABS(D(N-1)) .LT. ABS(XMAX*X/EN)) .AND.
     1            (N .LE. LIM)) GO TO 50
            ELSE
               IF ((ABS(D(N-1)/X) .LT. XMAX/EN) .AND. (N .LE. LIM))
     1            GO TO 50
         END IF
         DO 100 J = N, LIM
            D(N) = ZERO
  100    CONTINUE
      IF ((.NOT. BOOL1) .AND. BOOL2) THEN
         T = (X1+C1)/(C0*X1)
         IF (T .LT. TEN) THEN
               T = ((((B0*T + B1)*T + B2)*T + B3)*T + B4)*T + B5
            ELSE
               Z = LOG(T) - B6
               P = (B6-LOG(Z))/(ONE+Z)
               P = ONE/(ONE+P)
               T = T*P/Z
         END IF
         N1 = C0*X1*T - ONE
         IF (N1 .LT. NMAX) N1 = NMAX
         Q = ONE/X
         EN = ONE
         DO 120 N = 1,N1+1
            Q = -EN*Q/X
            EN = EN+ONE
  120    CONTINUE
         DO 140 N = N1,N0+1,-1
            EN = EN - ONE
            Q = (E-X*Q)/EN
            IF (N .LE. NMAX) D(N) = Q
  140    CONTINUE
      END IF
      RETURN
C---------- Last line of DSUBN ----------
      END
