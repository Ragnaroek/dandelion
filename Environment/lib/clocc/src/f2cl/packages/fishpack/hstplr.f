C FISHPAK8 FROM PORTLIB                                  12/30/83
      SUBROUTINE HSTPLR (A,B,M,MBDCND,BDA,BDB,C,D,N,NBDCND,BDC,BDD,
     1                   ELMBDA,F,IDIMF,PERTRB,IERROR,W)
C
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
C     * * * * * * * * *  PURPOSE    * * * * * * * * * * * * * * * * * *
C
C      HSTPLR SOLVES THE STANDARD FIVE-POINT FINITE DIFFERENCE
C      APPROXIMATION ON A STAGGERED GRID TO THE HELMHOLTZ EQUATION IN
C      POLAR COORDINATES
C
C      (1/R)(D/DR)(R(DU/DR)) + (1/R**2)(D/DTHETA)(DU/DTHETA)
C
C                      + LAMBDA*U = F(R,THETA)
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     * * * * * * * *    PARAMETER DESCRIPTION     * * * * * * * * * *
C
C             * * * * * *   ON INPUT    * * * * * *
C
C    A,B
C      THE RANGE OF R, I.E. A .LE. R .LE. B.  A MUST BE LESS THAN B AND
C      A MUST BE NON-NEGATIVE.
C
C    M
C      THE NUMBER OF GRID POINTS IN THE INTERVAL (A,B).  THE GRID POINTS
C      IN THE R-DIRECTION ARE GIVEN BY R(I) = A + (I-0.5)DR FOR
C      I=1,2,...,M WHERE DR =(B-A)/M.  M MUST BE GREATER THAN 2.
C
C    MBDCND
C      INDICATES THE TYPE OF BOUNDARY CONDITIONS AT R = A AND R = B.
C
C      = 1  IF THE SOLUTION IS SPECIFIED AT R = A AND R = B.
C
C      = 2  IF THE SOLUTION IS SPECIFIED AT R = A AND THE DERIVATIVE
C           OF THE SOLUTION WITH RESPECT TO R IS SPECIFIED AT R = B.
C           (SEE NOTE 1 BELOW)
C
C      = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO R IS
C           SPECIFIED AT R = A (SEE NOTE 2 BELOW) AND R = B.
C
C      = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO R IS
C           SPECIFIED AT R = A (SEE NOTE 2 BELOW) AND THE SOLUTION IS
C           SPECIFIED AT R = B.
C
C      = 5  IF THE SOLUTION IS UNSPECIFIED AT R = A = 0 AND THE SOLUTION
C           IS SPECIFIED AT R = B.
C
C      = 6  IF THE SOLUTION IS UNSPECIFIED AT R = A = 0 AND THE
C           DERIVATIVE OF THE SOLUTION WITH RESPECT TO R IS SPECIFIED AT
C           R = B.
C
C      NOTE 1:  IF A = 0, MBDCND = 2, AND NBDCND = 0 OR 3, THE SYSTEM OF
C               EQUATIONS TO BE SOLVED IS SINGULAR.  THE UNIQUE SOLUTION
C               IS DETERMINED BY EXTRAPOLATION TO THE SPECIFICATION OF
C               U(0,THETA(1)).  BUT IN THIS CASE THE RIGHT SIDE OF THE
C               SYSTEM WILL BE PERTURBED BY THE CONSTANT PERTRB.
C
C      NOTE 2:  IF A = 0, DO NOT USE MBDCND = 3 OR 4, BUT INSTEAD USE
C               MBDCND = 1,2,5, OR 6.
C
C    BDA
C      A ONE-DIMENSIONAL ARRAY OF LENGTH N THAT SPECIFIES THE BOUNDARY
C      VALUES (IF ANY) OF THE SOLUTION AT R = A.  WHEN MBDCND = 1 OR 2,
C
C               BDA(J) = U(A,THETA(J)) ,          J=1,2,...,N.
C
C      WHEN MBDCND = 3 OR 4,
C
C               BDA(J) = (D/DR)U(A,THETA(J)) ,    J=1,2,...,N.
C
C      WHEN MBDCND = 5 OR 6, BDA IS A DUMMY VARIABLE.
C
C    BDB
C      A ONE-DIMENSIONAL ARRAY OF LENGTH N THAT SPECIFIES THE BOUNDARY
C      VALUES OF THE SOLUTION AT R = B.  WHEN MBDCND = 1,4, OR 5,
C
C               BDB(J) = U(B,THETA(J)) ,          J=1,2,...,N.
C
C      WHEN MBDCND = 2,3, OR 6,
C
C               BDB(J) = (D/DR)U(B,THETA(J)) ,    J=1,2,...,N.
C
C    C,D
C      THE RANGE OF THETA, I.E. C .LE. THETA .LE. D.  C MUST BE LESS
C      THAN D.
C
C    N
C      THE NUMBER OF UNKNOWNS IN THE INTERVAL (C,D).  THE UNKNOWNS IN
C      THE THETA-DIRECTION ARE GIVEN BY THETA(J) = C + (J-0.5)DT,
C      J=1,2,...,N, WHERE DT = (D-C)/N.  N MUST BE GREATER THAN 2.
C
C    NBDCND
C      INDICATES THE TYPE OF BOUNDARY CONDITIONS AT THETA = C
C      AND THETA = D.
C
C      = 0  IF THE SOLUTION IS PERIODIC IN THETA, I.E.
C           U(I,J) = U(I,N+J).
C
C      = 1  IF THE SOLUTION IS SPECIFIED AT THETA = C AND THETA = D
C           (SEE NOTE BELOW).
C
C      = 2  IF THE SOLUTION IS SPECIFIED AT THETA = C AND THE DERIVATIVE
C           OF THE SOLUTION WITH RESPECT TO THETA IS SPECIFIED AT
C           THETA = D (SEE NOTE BELOW).
C
C      = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C           SPECIFIED AT THETA = C AND THETA = D.
C
C      = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C           SPECIFIED AT THETA = C AND THE SOLUTION IS SPECIFIED AT
C           THETA = D (SEE NOTE BELOW).
C
C      NOTE:  WHEN NBDCND = 1, 2, OR 4, DO NOT USE MBDCND = 5 OR 6 (THE
C      FORMER INDICATES THAT THE SOLUTION IS SPECIFIED AT R =  0; THE
C      LATTER INDICATES THE SOLUTION IS UNSPECIFIED AT R = 0).  USE
C      INSTEAD MBDCND = 1 OR 2.
C
C    BDC
C      A ONE DIMENSIONAL ARRAY OF LENGTH M THAT SPECIFIES THE BOUNDARY
C      VALUES OF THE SOLUTION AT THETA = C.   WHEN NBDCND = 1 OR 2,
C
C               BDC(I) = U(R(I),C) ,              I=1,2,...,M.
C
C      WHEN NBDCND = 3 OR 4,
C
C               BDC(I) = (D/DTHETA)U(R(I),C),     I=1,2,...,M.
C
C      WHEN NBDCND = 0, BDC IS A DUMMY VARIABLE.
C
C    BDD
C      A ONE-DIMENSIONAL ARRAY OF LENGTH M THAT SPECIFIES THE BOUNDARY
C      VALUES OF THE SOLUTION AT THETA = D.  WHEN NBDCND = 1 OR 4,
C
C               BDD(I) = U(R(I),D) ,              I=1,2,...,M.
C
C      WHEN NBDCND = 2 OR 3,
C
C               BDD(I) = (D/DTHETA)U(R(I),D) ,    I=1,2,...,M.
C
C      WHEN NBDCND = 0, BDD IS A DUMMY VARIABLE.
C
C    ELMBDA
C      THE CONSTANT LAMBDA IN THE HELMHOLTZ EQUATION.  IF LAMBDA IS
C      GREATER THAN 0, A SOLUTION MAY NOT EXIST.  HOWEVER, HSTPLR WILL
C      ATTEMPT TO FIND A SOLUTION.
C
C    F
C      A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUES OF THE RIGHT
C      SIDE OF THE HELMHOLTZ EQUATION.  FOR I=1,2,...,M AND J=1,2,...,N
C
C               F(I,J) = F(R(I),THETA(J)) .
C
C      F MUST BE DIMENSIONED AT LEAST M X N.
C
C    IDIMF
C      THE ROW (OR FIRST) DIMENSION OF THE ARRAY F AS IT APPEARS IN THE
C      PROGRAM CALLING HSTPLR.  THIS PARAMETER IS USED TO SPECIFY THE
C      VARIABLE DIMENSION OF F.  IDIMF MUST BE AT LEAST M.
C
C    W
C      A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE USER FOR
C      WORK SPACE.  W MAY REQUIRE UP TO 13M + 4N + M*INT(LOG2(N))
C      LOCATIONS.  THE ACTUAL NUMBER OF LOCATIONS USED IS COMPUTED BY
C      HSTPLR AND IS RETURNED IN THE LOCATION W(1).
C
C
C             * * * * * *   ON OUTPUT   * * * * * *
C
C    F
C      CONTAINS THE SOLUTION U(I,J) OF THE FINITE DIFFERENCE
C      APPROXIMATION FOR THE GRID POINT (R(I),THETA(J)) FOR
C      I=1,2,...,M, J=1,2,...,N.
C
C    PERTRB
C      IF A COMBINATION OF PERIODIC, DERIVATIVE, OR UNSPECIFIED
C      BOUNDARY CONDITIONS IS SPECIFIED FOR A POISSON EQUATION
C      (LAMBDA = 0), A SOLUTION MAY NOT EXIST.  PERTRB IS A CON-
C      STANT, CALCULATED AND SUBTRACTED FROM F, WHICH ENSURES
C      THAT A SOLUTION EXISTS.  HSTPLR THEN COMPUTES THIS
C      SOLUTION, WHICH IS A LEAST SQUARES SOLUTION TO THE
C      ORIGINAL APPROXIMATION.  THIS SOLUTION PLUS ANY CONSTANT IS ALSO
C      A SOLUTION; HENCE, THE SOLUTION IS NOT UNIQUE.  THE VALUE OF
C      PERTRB SHOULD BE SMALL COMPARED TO THE RIGHT SIDE F.
C      OTHERWISE, A SOLUTION IS OBTAINED TO AN ESSENTIALLY DIFFERENT
C      PROBLEM.  THIS COMPARISON SHOULD ALWAYS BE MADE TO INSURE THAT
C      A MEANINGFUL SOLUTION HAS BEEN OBTAINED.
C
C    IERROR
C      AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.
C      EXCEPT TO NUMBERS 0 AND 11, A SOLUTION IS NOT ATTEMPTED.
C
C      =  0  NO ERROR
C
C      =  1  A .LT. 0
C
C      =  2  A .GE. B
C
C      =  3  MBDCND .LT. 1 OR MBDCND .GT. 6
C
C      =  4  C .GE. D
C
C      =  5  N .LE. 2
C
C      =  6  NBDCND .LT. 0 OR NBDCND .GT. 4
C
C      =  7  A = 0 AND MBDCND = 3 OR 4
C
C      =  8  A .GT. 0 AND MBDCND .GE. 5
C
C      =  9  MBDCND .GE. 5 AND NBDCND .NE. 0 OR 3
C
C      = 10  IDIMF .LT. M
C
C      = 11  LAMBDA .GT. 0
C
C      = 12  M .LE. 2
C
C      SINCE THIS IS THE ONLY MEANS OF INDICATING A POSSIBLY
C      INCORRECT CALL TO HSTPLR, THE USER SHOULD TEST IERROR AFTER
C      THE CALL.
C
C    W
C      W(1) CONTAINS THE REQUIRED LENGTH OF W.
C
C
C     * * * * * * *   PROGRAM SPECIFICATIONS    * * * * * * * * * * * *
C
C     DIMENSION OF   BDA(N),BDB(N),BDC(M),BDD(M),F(IDIMF,N),
C     ARGUMENTS      W(SEE ARGUMENT LIST)
C
C     LATEST         JUNE 1, 1977
C     REVISION
C
C     SUBPROGRAMS    HSTPLR,POISTG,POSTG2,GENBUN,POISD2,POISN2,POISP2,
C     REQUIRED       COSGEN,MERGE,TRIX,TRI3,PIMACH
C
C     SPECIAL        NONE
C     CONDITIONS
C
C     COMMON         NONE
C     BLOCKS
C
C     I/O            NONE
C
C     PRECISION      SINGLE
C
C
C     SPECIALIST     ROLAND SWEET
C
C     LANGUAGE       FORTRAN
C
C     HISTORY        WRITTEN BY ROLAND SWEET AT NCAR IN FEBRUARY, 1977
C
C     ALGORITHM      THIS SUBROUTINE DEFINES THE FINITE-DIFFERENCE
C                    EQUATIONS, INCORPORATES BOUNDARY DATA, ADJUSTS THE
C                    RIGHT SIDE WHEN THE SYSTEM IS SINGULAR AND CALLS
C                    EITHER POISTG OR GENBUN WHICH SOLVES THE LINEAR
C                    SYSTEM OF EQUATIONS.
C
C     SPACE          8265(DECIMAL) = 20111(OCTAL) LOCATIONS ON THE
C     REQUIRED       NCAR CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE HSTPLR IS ROUGHLY PROPORTIONAL
C                    TO M*N*LOG2(N).  SOME TYPICAL VALUES ARE LISTED IN
C                    THE TABLE BELOW.
C                       THE SOLUTION PROCESS EMPLOYED RESULTS IN A LOSS
C                    OF NO MORE THAN FOUR SIGNIFICANT DIGITS FOR N AND M
C                    AS LARGE AS 64.  MORE DETAILED INFORMATION ABOUT
C                    ACCURACY CAN BE FOUND IN THE DOCUMENTATION FOR
C                    SUBROUTINE POISTG WHICH IS THE ROUTINE THAT
C                    ACTUALLY SOLVES THE FINITE DIFFERENCE EQUATIONS.
C
C
C                       M(=N)    MBDCND    NBDCND    T(MSECS)
C                       -----    ------    ------    --------
C
C                        32       1-6       1-4         56
C                        64       1-6       1-4        230
C
C     PORTABILITY    AMERICAN NATIONAL STANDARDS INSTITUTE FORTRAN.
C                    ALL MACHINE DEPENDENT CONSTANTS ARE LOCATED IN THE
C                    FUNCTION PIMACH.
C
C     REQUIRED       COS
C     RESIDENT
C     ROUTINES
C
C     REFERENCE      SCHUMANN, U. AND R. SWEET,"A DIRECT METHOD FOR
C                    THE SOLUTION OF POISSON"S EQUATION WITH NEUMANN
C                    BOUNDARY CONDITIONS ON A STAGGERED GRID OF
C                    ARBITRARY SIZE," J. COMP. PHYS. 20(1976),
C                    PP. 171-182.
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      DIMENSION       F(IDIMF,1)
      DIMENSION       BDA(1)     ,BDB(1)     ,BDC(1)     ,BDD(1)     ,
     1                W(1)
      IERROR = 0
      IF (A .LT. 0.) IERROR = 1
      IF (A .GE. B) IERROR = 2
      IF (MBDCND.LE.0 .OR. MBDCND.GE.7) IERROR = 3
      IF (C .GE. D) IERROR = 4
      IF (N .LE. 2) IERROR = 5
      IF (NBDCND.LT.0 .OR. NBDCND.GE.5) IERROR = 6
      IF (A.EQ.0. .AND. (MBDCND.EQ.3 .OR. MBDCND.EQ.4)) IERROR = 7
      IF (A.GT.0. .AND. MBDCND.GE.5) IERROR = 8
      IF (MBDCND.GE.5 .AND. NBDCND.NE.0 .AND. NBDCND.NE.3) IERROR = 9
      IF (IDIMF .LT. M) IERROR = 10
      IF (M .LE. 2) IERROR = 12
      IF (IERROR .NE. 0) RETURN
      DELTAR = (B-A)/FLOAT(M)
      DLRSQ = DELTAR**2
      DELTHT = (D-C)/FLOAT(N)
      DLTHSQ = DELTHT**2
      NP = NBDCND+1
      ISW = 1
      MB = MBDCND
      IF (A.EQ.0. .AND. MBDCND.EQ.2) MB = 6
C
C     DEFINE A,B,C COEFFICIENTS IN W-ARRAY.
C
      IWB = M
      IWC = IWB+M
      IWR = IWC+M
      DO 101 I=1,M
         J = IWR+I
         W(J) = A+(FLOAT(I)-0.5)*DELTAR
         W(I) = (A+FLOAT(I-1)*DELTAR)/DLRSQ
         K = IWC+I
         W(K) = (A+FLOAT(I)*DELTAR)/DLRSQ
         K = IWB+I
         W(K) = (ELMBDA-2./DLRSQ)*W(J)
  101 CONTINUE
      DO 103 I=1,M
         J = IWR+I
         A1 = W(J)
         DO 102 J=1,N
            F(I,J) = A1*F(I,J)
  102    CONTINUE
  103 CONTINUE
C
C     ENTER BOUNDARY DATA FOR R-BOUNDARIES.
C
      GO TO (104,104,106,106,108,108),MB
  104 A1 = 2.*W(1)
      W(IWB+1) = W(IWB+1)-W(1)
      DO 105 J=1,N
         F(1,J) = F(1,J)-A1*BDA(J)
  105 CONTINUE
      GO TO 108
  106 A1 = DELTAR*W(1)
      W(IWB+1) = W(IWB+1)+W(1)
      DO 107 J=1,N
         F(1,J) = F(1,J)+A1*BDA(J)
  107 CONTINUE
  108 GO TO (109,111,111,109,109,111),MB
  109 A1 = 2.*W(IWR)
      W(IWC) = W(IWC)-W(IWR)
      DO 110 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  110 CONTINUE
      GO TO 113
  111 A1 = DELTAR*W(IWR)
      W(IWC) = W(IWC)+W(IWR)
      DO 112 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  112 CONTINUE
C
C     ENTER BOUNDARY DATA FOR THETA-BOUNDARIES.
C
  113 A1 = 2./DLTHSQ
      GO TO (123,114,114,116,116),NP
  114 DO 115 I=1,M
         J = IWR+I
         F(I,1) = F(I,1)-A1*BDC(I)/W(J)
  115 CONTINUE
      GO TO 118
  116 A1 = 1./DELTHT
      DO 117 I=1,M
         J = IWR+I
         F(I,1) = F(I,1)+A1*BDC(I)/W(J)
  117 CONTINUE
  118 A1 = 2./DLTHSQ
      GO TO (123,119,121,121,119),NP
  119 DO 120 I=1,M
         J = IWR+I
         F(I,N) = F(I,N)-A1*BDD(I)/W(J)
  120 CONTINUE
      GO TO 123
  121 A1 = 1./DELTHT
      DO 122 I=1,M
         J = IWR+I
         F(I,N) = F(I,N)-A1*BDD(I)/W(J)
  122 CONTINUE
  123 CONTINUE
C
C     ADJUST RIGHT SIDE OF SINGULAR PROBLEMS TO INSURE EXISTENCE OF A
C     SOLUTION.
C
      PERTRB = 0.
      IF (ELMBDA) 133,125,124
  124 IERROR = 11
      GO TO 133
  125 GO TO (133,133,126,133,133,126),MB
  126 GO TO (127,133,133,127,133),NP
  127 CONTINUE
      ISW = 2
      DO 129 J=1,N
         DO 128 I=1,M
            PERTRB = PERTRB+F(I,J)
  128    CONTINUE
  129 CONTINUE
      PERTRB = PERTRB/(FLOAT(M*N)*0.5*(A+B))
      DO 131 I=1,M
         J = IWR+I
         A1 = PERTRB*W(J)
         DO 130 J=1,N
            F(I,J) = F(I,J)-A1
  130    CONTINUE
  131 CONTINUE
      A2 = 0.
      DO 132 J=1,N
         A2 = A2+F(1,J)
  132 CONTINUE
      A2 = A2/W(IWR+1)
  133 CONTINUE
C
C     MULTIPLY I-TH EQUATION THROUGH BY  R(I)*DELTHT**2
C
      DO 135 I=1,M
         J = IWR+I
         A1 = DLTHSQ*W(J)
         W(I) = A1*W(I)
         J = IWC+I
         W(J) = A1*W(J)
         J = IWB+I
         W(J) = A1*W(J)
         DO 134 J=1,N
            F(I,J) = A1*F(I,J)
  134    CONTINUE
  135 CONTINUE
      LP = NBDCND
      W(1) = 0.
      W(IWR) = 0.
C
C     CALL POISTG OR GENBUN TO SOLVE THE SYSTEM OF EQUATIONS.
C
      IF (LP .EQ. 0) GO TO 136
      CALL POISTG (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
      GO TO 137
  136 CALL GENBUN (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
  137 CONTINUE
      W(1) = W(IWR+1)+3.*FLOAT(M)
      IF (A.NE.0. .OR. MBDCND.NE.2 .OR. ISW.NE.2) GO TO 141
      A1 = 0.
      DO 138 J=1,N
         A1 = A1+F(1,J)
  138 CONTINUE
      A1 = (A1-DLRSQ*A2/16.)/FLOAT(N)
      IF (NBDCND .EQ. 3) A1 = A1+(BDD(1)-BDC(1))/(D-C)
      A1 = BDA(1)-A1
      DO 140 I=1,M
         DO 139 J=1,N
            F(I,J) = F(I,J)+A1
  139    CONTINUE
  140 CONTINUE
  141 CONTINUE
      RETURN
      END
C     CALL POISTG OR GENBUN TO SOLVE THE SYSTEM OF EQUATIONS.
