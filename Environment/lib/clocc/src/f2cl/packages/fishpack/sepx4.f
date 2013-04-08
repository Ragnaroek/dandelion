      SUBROUTINE SEPX4(IORDER,A,B,M,MBDCND,BDA,ALPHA,BDB,BETA,C,D,N,
     1NBDCND,BDC,BDD,COFX,GRHS,USOL,IDMN,W,PERTRB,IERROR)
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C     *                                                               *
C     *                        F I S H P A K                          *
C     *                                                               *
C     *                                                               *
C     *     A PACKAGE OF FORTRAN SUBPROGRAMS FOR THE SOLUTION OF      *
C     *                                                               *
C     *      SEPARABLE ELLIPTIC PARTIAL DIFFERENTIAL EQUATIONS        *
C     *                                                               *
C     *                  (VERSION 3.1 , OCTOBER 1980)                  *
C     *                                                               *
C     *                             BY                                *
C     *                                                               *
C     *        JOHN ADAMS, PAUL SWARZTRAUBER AND ROLAND SWEET         *
C     *                                                               *
C     *                             OF                                *
C     *                                                               *
C     *         THE NATIONAL CENTER FOR ATMOSPHERIC RESEARCH          *
C     *                                                               *
C     *                BOULDER, COLORADO  (80307)  U.S.A.             *
C     *                                                               *
C     *                   WHICH IS SPONSORED BY                       *
C     *                                                               *
C     *              THE NATIONAL SCIENCE FOUNDATION                  *
C     *                                                               *
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C
C
C DIMENSION OF           BDA(N+1), BDB(N+1), BDC(M+1), BDD(M+1),
C ARGUMENTS              USOL(IDMN,N+1), GRHS(IDMN,N+1),
C                        W (SEE ARGUMENT LIST)
C
C LATEST REVISION        SEPTEMBER 1996
C
C
C PURPOSE                SEPX4 SOLVES FOR EITHER THE SECOND-ORDER
C                        FINITE DIFFERENCE APPROXIMATION OR A
C                        A FOURTH-ORDER APPROXIMATION  TO THE
C                        SOLUTION OF A SEPARABLE ELLIPTIC EQUATION
C                             AF(X)*UXX+BF(X)*UX+CF(X)*U+UYY = G(X,Y)
C
C                        ON A RECTANGLE (X GREATER THAN OR EQUAL TO A
C                        AND LESS THAN OR EQUAL TO B; Y GREATER THAN
C                        OR EQUAL TO C AND LESS THAN OR EQUAL TO D).
C                        ANY COMBINATION OF PERIODIC OR MIXED BOUNDARY
C                        CONDITIONS IS ALLOWED.
C                        IF BOUNDARY CONDITIONS IN THE X DIRECTION
C                        ARE PERIODIC (SEE MBDCND=0 BELOW) THEN THE
C                        COEFFICIENTS MUST SATISFY
C                        AF(X)=C1,BF(X)=0,CF(X)=C2 FOR ALL X.
C                        HERE C1,C2 ARE CONSTANTS, C1.GT.0.
C
C                        THE POSSIBLE BOUNDARY CONDITIONS ARE
C                        IN THE X-DIRECTION:
C                         (0) PERIODIC, U(X+B-A,Y)=U(X,Y) FOR ALL Y,X
C                         (1) U(A,Y), U(B,Y) ARE SPECIFIED FOR ALL Y
C                         (2) U(A,Y), DU(B,Y)/DX+BETA*U(B,Y) ARE
C                             SPECIFIED FOR ALL Y
C                         (3) DU(A,Y)/DX+ALPHA*U(A,Y),DU(B,Y)/DX+
C                             BETA*U(B,Y) ARE SPECIFIED FOR ALL Y
C                         (4) DU(A,Y)/DX+ALPHA*U(A,Y),U(B,Y) ARE
C                             SPECIFIED FOR ALL Y
C
C                        IN THE Y-DIRECTION:
C                         (0) PERIODIC, U(X,Y+D-C)=U(X,Y) FOR ALL X,Y
C                         (1) U(X,C),U(X,D) ARE SPECIFIED FOR ALL X
C                         (2) U(X,C),DU(X,D)/DY ARE SPECIFIED FOR ALL X
C                         (3) DU(X,C)/DY,DU(X,D)/DY ARE SPECIFIED FOR
C                            ALL X
C                        (4) DU(X,C)/DY,U(X,D) ARE SPECIFIED FOR ALL X
C
C USAGE                  CALL SEPX4(IORDER,A,B,M,MBDCND,BDA,ALPHA,BDB,
C                                  BETA,C,D,N,NBDCND,BDC,BDD,COFX,
C                                  GRHS,USOL,IDMN,W,PERTRB,IERROR)
C
C ARGUMENTS
C
C
C                        IORDER
C                          = 2 IF A SECOND-ORDER APPROXIMATION IS SOUGHT
C                          = 4 IF A FOURTH-ORDER APPROXIMATION IS SOUGHT
C
C                        A,B
C                          THE RANGE OF THE X-INDEPENDENT VARIABLE;
C                          I.E., X IS GREATER THAN OR EQUAL TO A AND
C                          LESS THAN OR EQUAL TO B.  A MUST BE LESS THAN
C                          B.
C
C                        M
C                          THE NUMBER OF PANELS INTO WHICH THE INTERVAL
C                          [A,B] IS SUBDIVIDED.  HENCE, THERE WILL BE
C                          M+1 GRID POINTS IN THE X-DIRECTION GIVEN BY
C                          XI=A+(I-1)*DLX FOR I=1,2,...,M+1 WHERE
C                          DLX=(B-A)/M IS THE PANEL WIDTH.  M MUST BE
C                          LESS THAN IDMN AND GREATER THAN 5.
C
C                        MBDCND
C                          INDICATES THE TYPE OF BOUNDARY CONDITION AT
C                          X=A AND X=B
C                          = 0 IF THE SOLUTION IS PERIODIC IN X; I.E.,
C                              U(X+B-A,Y)=U(X,Y) FOR ALL Y,X
C                          = 1 IF THE SOLUTION IS SPECIFIED AT X=A AND
C                              X=B; I.E., U(A,Y) AND U(B,Y) ARE
C                              SPECIFIED FOR ALL Y
C                          = 2 IF THE SOLUTION IS SPECIFIED AT X=A AND
C                              THE BOUNDARY CONDITION IS MIXED AT X=B;
C                              I.E., U(A,Y) AND DU(B,Y)/DX+BETA*U(B,Y)
C                              ARE SPECIFIED FOR ALL Y
C                          = 3 IF THE BOUNDARY CONDITIONS AT X=A AND X=B
C                              ARE MIXED; I.E., DU(A,Y)/DX+ALPHA*U(A,Y)
C                              AND DU(B,Y)/DX+BETA*U(B,Y) ARE SPECIFIED
C                              FOR ALL Y
C                          = 4 IF THE BOUNDARY CONDITION AT X=A IS MIXED
C                              AND THE SOLUTION IS SPECIFIED AT X=B;
C                              I.E., DU(A,Y)/DX+ALPHA*U(A,Y) AND U(B,Y)
C                              ARE SPECIFIED FOR ALL Y
C
C                        BDA
C                          A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT
C                          SPECIFIES THE VALUES OF DU(A,Y)/DX+
C                          ALPHA*U(A,Y) AT X=A, WHEN MBDCND=3 OR 4.
C                               BDA(J) = DU(A,YJ)/DX+ALPHA*U(A,YJ);
C                               J=1,2,...,N+1
C                          WHEN MBDCND HAS ANY OTHER VALUE, BDA IS A
C                          DUMMY PARAMETER.
C
C ON INPUT               ALPHA
C                          THE SCALAR MULTIPLYING THE SOLUTION IN CASE
C                          OF A MIXED BOUNDARY CONDITION AT X=A (SEE
C                          ARGUMENT BDA).  IF MBDCND ' 3,4 THEN ALPHA IS
C                          A DUMMY PARAMETER.
C
C                        BDB
C                          A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT
C                          SPECIFIES THE VALUES OF DU(B,Y)/DX+
C                          BETA*U(B,Y) AT X=B.  WHEN MBDCND=2 OR 3
C                               BDB(J) = DU(B,YJ)/DX+BETA*U(B,YJ);
C                               J=1,2,...,N+1
C                          WHEN MBDCND HAS ANY OTHER VALUE, BDB IS A
C                          DUMMY PARAMETER.
C
C                        BETA
C                          THE SCALAR MULTIPLYING THE SOLUTION IN CASE
C                          OF A MIXED BOUNDARY CONDITION AT X=B (SEE
C                          ARGUMENT BDB).  IF MBDCND'2,3 THEN BETA IS A
C                          DUMMY PARAMETER.
C
C                        C,D
C                          THE RANGE OF THE Y-INDEPENDENT VARIABLE;
C                          I.E., Y IS GREATER THAN OR EQUAL TO C AND
C                          LESS THAN OR EQUAL TO D.  C MUST BE LESS THAN
C                          D.
C
C                        N
C                          THE NUMBER OF PANELS INTO WHICH THE INTERVAL
C                          [C,D] IS SUBDIVIDED.  HENCE, THERE WILL BE
C                          N+1 GRID POINTS IN THE Y-DIRECTION GIVEN BY
C                          YJ=C+(J-1)*DLY FOR J=1,2,...,N+1 WHERE
C                          DLY=(D-C)/N IS THE PANEL WIDTH.  IN ADDITION,
C                          N MUST BE GREATER THAN 4.
C
C                        NBDCND
C                          INDICATES THE TYPES OF BOUNDARY CONDITIONS AT
C                          Y=C AND Y=D
C                          = 0 IF THE SOLUTION IS PERIODIC IN Y,
C                              I.E., U(X,Y+D-C)=U(X,Y) FOR ALL X,Y
C                          = 1 IF THE SOLUTION IS SPECIFIED AT Y=C
C                              AND Y = D, I.E., U(X,C)  AND U(X,D)
C                              ARE SPECIFIED FOR ALL X
C                          = 2 IF THE SOLUTION IS SPECIFIED AT Y=C
C                              AND THE BOUNDARY CONDITION IS MIXED
C                              AT Y=D, I.E., U(X,C) AND DU(X,D)/DY
C                              ARE SPECIFIED FOR ALL X
C                          = 3 IF THE BOUNDARY CONDITIONS ARE MIXED
C                              AT Y=C AND Y=D I.E.,
C                              DU(X,C)/DY AND DU(X,D)/DY ARE
C                              SPECIFIED FOR ALL X
C                          = 4 IF THE BOUNDARY CONDITION IS MIXED
C                              AT Y=C AND THE SOLUTION IS SPECIFIED
C                              AT Y=D, I.E. DU(X,C)/DY AND U(X,D)
C                              ARE SPECIFIED FOR ALL X
C
C                        BDC
C                          A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT
C                          SPECIFIES THE VALUE DU(X,C)/DY
C                          AT Y=C.  WHEN NBDCND=3 OR 4
C                            BDC(I) = DU(XI,C)/DY
C                             I=1,2,...,M+1.
C                          WHEN NBDCND HAS ANY OTHER VALUE, BDC IS A
C                          DUMMY PARAMETER.
C
C
C                        BDD
C                          A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT
C                           SPECIFIED THE VALUE OF DU(X,D)/DY
C                           AT Y=D.  WHEN NBDCND=2 OR 3
C                             BDD(I)=DU(XI,D)/DY
C                            I=1,2,...,M+1.
C                          WHEN NBDCND HAS ANY OTHER VALUE, BDD IS A
C                          DUMMY PARAMETER.
C
C
C                        COFX
C                          A USER-SUPPLIED SUBPROGRAM WITH
C                          PARAMETERS X, AFUN, BFUN, CFUN WHICH
C                          RETURNS THE VALUES OF THE X-DEPENDENT
C                          COEFFICIENTS AF(X), BF(X), CF(X) IN
C                          THE ELLIPTIC EQUATION AT X.
C                        IF BOUNDARY CONDITIONS IN THE X DIRECTION
C                        ARE PERIODIC THEN THE COEFFICIENTS
C                        MUST SATISFY AF(X)=C1,BF(X)=0,CF(X)=C2 FOR
C                        ALL X.  HERE C1.GT.0 AND C2 ARE CONSTANTS.
C
C                        NOTE THAT COFX MUST BE DECLARED EXTERNAL
C                        IN THE CALLING ROUTINE.
C
C                        GRHS
C                          A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE
C                          VALUES OF THE RIGHT-HAND SIDE OF THE ELLIPTIC
C                          EQUATION; I.E., GRHS(I,J)=G(XI,YI), FOR
C                          I=2,...,M; J=2,...,N.  AT THE BOUNDARIES,
C                          GRHS IS DEFINED BY
C
C                          MBDCND   GRHS(1,J)   GRHS(M+1,J)
C                          ------   ---------   -----------
C                            0      G(A,YJ)     G(B,YJ)
C                            1         *           *
C                            2         *        G(B,YJ)  J=1,2,...,N+1
C                            3      G(A,YJ)     G(B,YJ)
C                            4      G(A,YJ)        *
C
C                          NBDCND   GRHS(I,1)   GRHS(I,N+1)
C                          ------   ---------   -----------
C                            0      G(XI,C)     G(XI,D)
C                            1         *           *
C                            2         *        G(XI,D)  I=1,2,...,M+1
C                            3      G(XI,C)     G(XI,D)
C                            4      G(XI,C)        *
C
C                          WHERE * MEANS THESE QUANTITES ARE NOT USED.
C                          GRHS SHOULD BE DIMENSIONED IDMN BY AT LEAST
C                          N+1 IN THE CALLING ROUTINE.
C
C                        USOL
C                          A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE
C                          VALUES OF THE SOLUTION ALONG THE BOUNDARIES.
C                          AT THE BOUNDARIES, USOL IS DEFINED BY
C
C                          MBDCND   USOL(1,J)   USOL(M+1,J)
C                          ------   ---------   -----------
C                            0         *           *
C                            1      U(A,YJ)     U(B,YJ)
C                            2      U(A,YJ)        *     J=1,2,...,N+1
C                            3         *           *
C                            4         *        U(B,YJ)
C
C                          NBDCND   USOL(I,1)   USOL(I,N+1)
C                          ------   ---------   -----------
C                            0         *           *
C                            1      U(XI,C)     U(XI,D)
C                            2      U(XI,C)        *     I=1,2,...,M+1
C                            3         *           *
C                            4         *        U(XI,D)
C
C                          WHERE * MEANS THE QUANTITES ARE NOT USED IN
C                          THE SOLUTION.
C
C                          IF IORDER=2, THE USER MAY EQUIVALENCE GRHS
C                          AND USOL TO SAVE SPACE.  NOTE THAT IN THIS
C                          CASE THE TABLES SPECIFYING THE BOUNDARIES OF
C                          THE GRHS AND USOL ARRAYS DETERMINE THE
C                          BOUNDARIES UNIQUELY EXCEPT AT THE CORNERS.
C                          IF THE TABLES CALL FOR BOTH G(X,Y) AND
C                          U(X,Y) AT A CORNER THEN THE SOLUTION MUST BE
C                          CHOSEN.  FOR EXAMPLE, IF MBDCND=2 AND
C                          NBDCND=4, THEN U(A,C), U(A,D), U(B,D) MUST BE
C                          CHOSEN AT THE CORNERS IN ADDITION TO G(B,C).
C
C                          IF IORDER=4, THEN THE TWO ARRAYS, USOL AND
C                          GRHS, MUST BE DISTINCT.
C
C                          USOL SHOULD BE DIMENSIONED IDMN BY AT LEAST
C                          N+1 IN THE CALLING ROUTINE.
C
C                        IDMN
C                          THE ROW (OR FIRST) DIMENSION OF THE ARRAYS
C                          GRHS AND USOL AS IT APPEARS IN THE PROGRAM
C                          CALLING SEPX4.  THIS PARAMETER IS USED TO
C                          SPECIFY THE VARIABLE DIMENSION OF GRHS AND
C                          USOL.  IDMN MUST BE AT LEAST 7 AND GREATER
C                          THAN OR EQUAL TO M+1.
C
C                        W
C                          A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED
C                        BY THE USER FOR WORK SPACE.
C                        10*N+(16+INT(LOG2(N)))*(M+1)+23 WILL SUFFICE
C                        AS A LENGTH FOR W.  THE ACTUAL LENGTH OF
C                        W IN THE CALLING ROUTINE MUST BE SET IN W(1)
C                        (SEE IERROR=11).
C ON OUTPUT              USOL
C                          CONTAINS THE APPROXIMATE SOLUTION TO THE
C                          ELLIPTIC EQUATION.  USOL(I,J) IS THE
C                          APPROXIMATION TO U(XI,YJ) FOR I=1,2...,M+1
C                          AND J=1,2,...,N+1.  THE APPROXIMATION HAS
C                          ERROR O(DLX**2+DLY**2) IF CALLED WITH
C                          IORDER=2 AND O(DLX**4+DLY**4) IF CALLED WITH
C                          IORDER=4.
C
C                        W
C                          CONTAINS INTERMEDIATE VALUES THAT MUST NOT BE
C                          DESTROYED IF SEPX4 IS CALLED AGAIN WITH
C                          INTL=1.  IN ADDITION W(1) CONTAINS THE EXACT
C                          MINIMAL LENGTH (IN FLOATING POINT) REQUIRED
C                          FOR THE WORK SPACE (SEE IERROR=11).
C
C                        PERTRB
C                        IF A COMBINATION OF PERIODIC OR DERIVATIVE
C                        BOUNDARY CONDITIONS (I.E., ALPHA=BETA=0 IF
C                        MBDCND=3) IS SPECIFIED AND IF CF(X)=0 FOR ALL X
C                        THEN A SOLUTION TO THE DISCRETIZED MATRIX
C                        EQUATION MAY NOT EXIST (REFLECTING THE NON-
C                        UNIQUENESS OF SOLUTIONS TO THE PDE).  PERTRB
C                        IS A CONSTANT CALCULATED AND SUBTRACTED FROM
C                        THE RIGHT HAND SIDE OF THE MATRIX EQUATION
C                        INSURING THE EXISTENCE OF A SOLUTION.
C                        SEPX4 COMPUTES THIS SOLUTION WHICH IS A
C                        WEIGHTED MINIMAL LEAST SQUARES SOLUTION TO
C                        THE ORIGINAL PROBLEM.  IF SINGULARITY IS
C                        NOT DETECTED PERTRB=0.0 IS RETURNED BY
C                        SEPX4.
C
C                        IERROR
C                          AN ERROR FLAG THAT INDICATES INVALID INPUT
C                          PARAMETERS OR FAILURE TO FIND A SOLUTION
C                          = 0 NO ERROR
C                          = 1 IF A GREATER THAN B OR C GREATER THAN D
C                          = 2 IF MBDCND LESS THAN 0 OR MBDCND GREATER
C                              THAN 4
C                          = 3 IF NBDCND LESS THAN 0 OR NBDCND GREATER
C                              THAN 4
C                          = 4 IF ATTEMPT TO FIND A SOLUTION FAILS.
C                              (THE LINEAR SYSTEM GENERATED IS NOT
C                              DIAGONALLY DOMINANT.)
C                          = 5 IF IDMN IS TOO SMALL (SEE DISCUSSION OF
C                              IDMN)
C                          = 6 IF M IS TOO SMALL OR TOO LARGE (SEE
C                              DISCUSSION OF M)
C                          = 7 IF N IS TOO SMALL (SEE DISCUSSION OF N)
C                          = 8 IF IORDER IS NOT 2 OR 4
C                          = 9 IF INTL IS NOT 0 OR 1
C                          = 10 IF AFUN IS LESS THAN OR EQUAL TO ZERO
C                          FOR SOME INTERIOR MESH POINT XI
C                               SOME INTERIOR MESH POINT (XI,YJ)
C                          = 11 IF THE WORK SPACE LENGTH INPUT IN W(1)
C                               IS LESS THAN THE EXACT MINIMAL WORK
C                               SPACE LENGTH REQUIRED OUTPUT IN W(1).
C                        = 12 IF MBDCND=0 AND AF(X)=CF(X)=CONSTANT
C                             OR BF(X)=0 FOR ALL X IS NOT TRUE.
C
C
C SPECIAL CONDITIONS     NONE
C
C COMMON BLOCKS          SPL4
C
C I/O                    NONE
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       NONE
C FILES
C
C SPECIALIST             JOHN C. ADAMS, NCAR, BOULDER, COLORADO  80307
C
C LANGUAGE               FORTRAN
C
C
C  ENTRY POINTS          SEPX4,SPELI4,CHKPR4,CHKSN4,ORTHO4,MINSO4,TRIS4,
C                        DEFE4,DX4,DY4
C
C HISTORY                SEPX4 WAS DEVELOPED BY MODIFYING THE ULIB
C                        ROUTINE SEPELI DURING OCTOBER 1978.
C                        IT SHOULD BE USED INSTEAD OF SEPELI WHENEVER
C                        POSSIBLE.  THE INCREASE IN SPEED IS AT LEAST
C                        A FACTOR OF THREE.
C
C ALGORITHM              SEPX4 AUTOMATICALLY DISCRETIZES THE SEPARABLE
C                        ELLIPTIC EQUATION WHICH IS THEN SOLVED BY A
C                        GENERALIZED CYCLIC REDUCTION ALGORITHM IN THE
C                        SUBROUTINE POIS.  THE FOURTH ORDER SOLUTION
C                        IS OBTAINED USING THE TECHNIQUE OF
C                        DEFFERRED CORRECTIONS REFERENCED BELOW.
C
C
C REFERENCES             KELLER, H.B., NUMERICAL METHODS FOR TWO-POINT
C                          BOUNDARY-VALUE PROBLEMS, BLAISDEL (1968),
C                          WALTHAM, MASS.
C
C                        SWARZTRAUBER, P., AND R. SWEET (1975):
C                          EFFICIENT FORTRAN SUBPROGRAMS FOR THE
C                          SOLUTION OF ELLIPTIC PARTIAL DIFFERENTIAL
C                          EQUATIONS.  NCAR TECHNICAL NOTE
C                          NCAR-TN/IA-109, PP. 135-137.
C
C
C
C
      DIMENSION       GRHS(IDMN,1)           ,USOL(IDMN,1)
      DIMENSION       BDA(1)     ,BDB(1)     ,BDC(1)     ,BDD(1)     ,
     1                W(1)
      EXTERNAL COFX
C
C     CHECK INPUT PARAMETERS
C
      CALL CHKPR4(IORDER,A,B,M,MBDCND,C,D,N,NBDCND,COFX,IDMN,IERROR)
      IF (IERROR .NE. 0) RETURN
C
C     COMPUTE MINIMUM WORK SPACE AND CHECK WORK SPACE LENGTH INPUT
C
      L = N+1
      IF (NBDCND .EQ. 0) L = N
      K = M+1
      L = N+1
C     ESTIMATE LOG BASE 2 OF N
      LOG2N=INT(ALOG(FLOAT(N+1))/ALOG(2.0)+0.5)
      LENGTH=4*(N+1)+(10+LOG2N)*(M+1)
      IERROR = 11
      LINPUT = INT(W(1)+0.5)
      LOUTPT = LENGTH+6*(K+L)+1
      W(1) = FLOAT(LOUTPT)
      IF (LOUTPT .GT. LINPUT) RETURN
      IERROR = 0
C
C     SET WORK SPACE INDICES
C
      I1 = LENGTH+2
      I2 = I1+L
      I3 = I2+L
      I4 = I3+L
      I5 = I4+L
      I6 = I5+L
      I7 = I6+L
      I8 = I7+K
      I9 = I8+K
      I10 = I9+K
      I11 = I10+K
      I12 = I11+K
      I13 = 2
      CALL SPELI4(IORDER,A,B,M,MBDCND,BDA,ALPHA,BDB,BETA,C,D,N,
     1NBDCND,BDC,BDD,COFX,W(I1),W(I2),W(I3),
     2             W(I4),W(I5),W(I6),W(I7),W(I8),W(I9),W(I10),W(I11),
     3             W(I12),GRHS,USOL,IDMN,W(I13),PERTRB,IERROR)
      RETURN
      END
