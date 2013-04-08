        DOUBLE PRECISION FUNCTION INVCN(X, ERRFLG)
        DOUBLE PRECISION X
        INTEGER ERRFLG
        COMMON /INVCMN/ XC, TOL, NCALL
        DOUBLE PRECISION XC, TOL
        INTEGER NCALL

        DOUBLE PRECISION CNERR, DZERO, PNORMS, DR7MDC
        EXTERNAL CNERR, PNORMS, DR7MDC

        DOUBLE PRECISION A, B
        DOUBLE PRECISION HALF, ONE, ZERO
        LOGICAL FIRST
        DOUBLE PRECISION HUGE
        PARAMETER (HALF = 0.5D+0, ONE = 1.D+0, ZERO = 0.D+0)
        SAVE FIRST, HUGE
        DATA FIRST/.TRUE./, HUGE/0.D+0/

        IF (FIRST) THEN
                TOL = 10.D+0 * DR7MDC(1)
                HUGE = 0.1D+0 * DR7MDC(6)
                FIRST = .FALSE.
                END IF

        NCALL = 0
        ERRFLG = 0
        IF (X .LE. ZERO) THEN
C               IF (X .EQ. ZERO) THEN
C                       INVCN = -HUGE
C                       GO TO 999
C                       END IF
                ERRFLG = 1
                INVCN = ZERO
                GO TO 999
                END IF
        IF (X .GE. ONE) THEN
C               IF (X .EQ. ONE) THEN
C                       INVCN = HUGE
C                       GO TO 999
C                       END IF
                ERRFLG = 1
                INVCN = ZERO
                GO TO 999
                END IF
        IF (X .GE. HALF) THEN
                A = ZERO
                B = ONE
 10             IF (PNORMS(B) .LT. X) THEN
                        B = B + ONE
                        GO TO 10
                        END IF
        ELSE
                B = ZERO
                A = -ONE
 20             IF (PNORMS(A) .GT. X) THEN
                        A = A - ONE
                        GO TO 20
                        END IF
                END IF
        XC = X
        INVCN = DZERO(CNERR,A,B,TOL)
 999    RETURN
        END
