C FISHPAK7 FROM PORTLIB                                  12/30/83
      SUBROUTINE HSTCRT (A,B,M,MBDCND,BDA,BDB,C,D,N,NBDCND,BDC,BDD,
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
C      HSTCRT SOLVES THE STANDARD FIVE-POINT FINITE DIFFERENCE
C      APPROXIMATION ON A STAGGERED GRID TO THE HELMHOLTZ EQUATION IN
C      CARTESIAN COORDINATES
C
C      (D/DX)(DU/DX) + (D/DY)(DU/DY) + LAMBDA*U = F(X,Y)
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     * * * * * * * *    PARAMETER DESCRIPTION     * * * * * * * * * *
C
C             * * * * * *   ON INPUT    * * * * * *
C
C    A,B
C      THE RANGE OF X, I.E. A .LE. X .LE. B.  A MUST BE LESS THAN B.
C
C    M
C      THE NUMBER OF GRID POINTS IN THE INTERVAL (A,B).  THE GRID POINTS
C      IN THE X-DIRECTION ARE GIVEN BY X(I) = A + (I-0.5)DX FOR
C      I=1,2,...,M WHERE DX =(B-A)/M.  M MUST BE GREATER THAN 2.
C
C    MBDCND
C      INDICATES THE TYPE OF BOUNDARY CONDITIONS AT X = A AND X = B.
C
C      = 0  IF THE SOLUTION IS PERIODIC IN X,
C           U(M+I,J) = U(I,J).
C
C      = 1  IF THE SOLUTION IS SPECIFIED AT X = A AND X = B.
C
C      = 2  IF THE SOLUTION IS SPECIFIED AT X = A AND THE DERIVATIVE
C           OF THE SOLUTION WITH RESPECT TO X IS SPECIFIED AT X = B.
C
C      = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO X IS
C           SPECIFIED AT X = A  AND X = B.
C
C      = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO X IS
C           SPECIFIED AT X = A  AND THE SOLUTION IS SPECIFIED AT X = B.
C
C    BDA
C      A ONE-DIMENSIONAL ARRAY OF LENGTH N THAT SPECIFIES THE BOUNDARY
C      VALUES (IF ANY) OF THE SOLUTION AT X = A.  WHEN MBDCND = 1 OR 2,
C
C               BDA(J) = U(A,Y(J)) ,          J=1,2,...,N.
C
C      WHEN MBDCND = 3 OR 4,
C
C               BDA(J) = (D/DX)U(A,Y(J)) ,    J=1,2,...,N.
C
C    BDB
C      A ONE-DIMENSIONAL ARRAY OF LENGTH N THAT SPECIFIES THE BOUNDARY
C      VALUES OF THE SOLUTION AT X = B.  WHEN MBDCND = 1 OR 4
C
C               BDB(J) = U(B,Y(J)) ,          J=1,2,...,N.
C
C      WHEN MBDCND = 2 OR 3
C
C               BDB(J) = (D/DX)U(B,Y(J)) ,    J=1,2,...,N.
C
C    C,D
C      THE RANGE OF Y, I.E. C .LE. Y .LE. D.  C MUST BE LESS
C      THAN D.
C
C    N
C      THE NUMBER OF UNKNOWNS IN THE INTERVAL (C,D).  THE UNKNOWNS IN
C      THE Y-DIRECTION ARE GIVEN BY Y(J) = C + (J-0.5)DY,
C      J=1,2,...,N, WHERE DY = (D-C)/N.  N MUST BE GREATER THAN 2.
C
C    NBDCND
C      INDICATES THE TYPE OF BOUNDARY CONDITIONS AT Y = C
C      AND Y = D.
C
C      = 0  IF THE SOLUTION IS PERIODIC IN Y, I.E.
C           U(I,J) = U(I,N+J).
C
C      = 1  IF THE SOLUTION IS SPECIFIED AT Y = C AND Y = D.
C
C      = 2  IF THE SOLUTION IS SPECIFIED AT Y = C AND THE DERIVATIVE
C           OF THE SOLUTION WITH RESPECT TO Y IS SPECIFIED AT Y = D.
C
C      = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Y IS
C           SPECIFIED AT Y = C AND Y = D.
C
C      = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Y IS
C           SPECIFIED AT Y = C AND THE SOLUTION IS SPECIFIED AT Y = D.
C
C    BDC
C      A ONE DIMENSIONAL ARRAY OF LENGTH M THAT SPECIFIES THE BOUNDARY
C      VALUES OF THE SOLUTION AT Y = C.   WHEN NBDCND = 1 OR 2,
C
C               BDC(I) = U(X(I),C) ,              I=1,2,...,M.
C
C      WHEN NBDCND = 3 OR 4,
C
C               BDC(I) = (D/DY)U(X(I),C),     I=1,2,...,M.
C
C      WHEN NBDCND = 0, BDC IS A DUMMY VARIABLE.
C
C    BDD
C      A ONE-DIMENSIONAL ARRAY OF LENGTH M THAT SPECIFIES THE BOUNDARY
C      VALUES OF THE SOLUTION AT Y = D.  WHEN NBDCND = 1 OR 4,
C
C               BDD(I) = U(X(I),D) ,              I=1,2,...,M.
C
C      WHEN NBDCND = 2 OR 3,
C
C               BDD(I) = (D/DY)U(X(I),D) ,    I=1,2,...,M.
C
C      WHEN NBDCND = 0, BDD IS A DUMMY VARIABLE.
C
C    ELMBDA
C      THE CONSTANT LAMBDA IN THE HELMHOLTZ EQUATION.  IF LAMBDA IS
C      GREATER THAN 0, A SOLUTION MAY NOT EXIST.  HOWEVER, HSTCRT WILL
C      ATTEMPT TO FIND A SOLUTION.
C
C    F
C      A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUES OF THE RIGHT
C      SIDE OF THE HELMHOLTZ EQUATION.  FOR I=1,2,...,M AND J=1,2,...,N
C
C               F(I,J) = F(X(I),Y(J)) .
C
C      F MUST BE DIMENSIONED AT LEAST M X N.
C
C    IDIMF
C      THE ROW (OR FIRST) DIMENSION OF THE ARRAY F AS IT APPEARS IN THE
C      PROGRAM CALLING HSTCRT.  THIS PARAMETER IS USED TO SPECIFY THE
C      VARIABLE DIMENSION OF F.  IDIMF MUST BE AT LEAST M.
C
C    W
C      A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE USER FOR
C      WORK SPACE.  W MAY REQUIRE UP TO 13M + 4N + M*INT(LOG2(N))
C      LOCATIONS.  THE ACTUAL NUMBER OF LOCATIONS USED IS COMPUTED BY
C      HSTCRT AND IS RETURNED IN THE LOCATION W(1).
C
C
C             * * * * * *   ON OUTPUT   * * * * * *
C
C    F
C      CONTAINS THE SOLUTION U(I,J) OF THE FINITE DIFFERENCE
C      APPROXIMATION FOR THE GRID POINT (X(I),Y(J)) FOR
C      I=1,2,...,M, J=1,2,...,N.
C
C    PERTRB
C      IF A COMBINATION OF PERIODIC OR DERIVATIVE BOUNDARY CONDITIONS IS
C      SPECIFIED FOR A POISSON EQUATION (LAMBDA = 0), A SOLUTION MAY NOT
C      EXIST.  PERTRB IS A CONSTANT, CALCULATED AND SUBTRACTED FROM F,
C      WHICH ENSURES THAT A SOLUTION EXISTS.  HSTCRT THEN COMPUTES THIS
C      SOLUTION, WHICH IS A LEAST SQUARES SOLUTION TO THE ORIGINAL
C      APPROXIMATION.  THIS SOLUTION PLUS ANY CONSTANT IS ALSO A
C      SOLUTION; HENCE, THE SOLUTION IS NOT UNIQUE.  THE VALUE OF PERTRB
C      SHOULD BE SMALL COMPARED TO THE RIGHT SIDE F.  OTHERWISE, A
C      SOLUTION IS OBTAINED TO AN ESSENTIALLY DIFFERENT PROBLEM.  THIS
C      COMPARISON SHOULD ALWAYS BE MADE TO INSURE THAT A MEANINGFUL
C      SOLUTION HAS BEEN OBTAINED.
C
C    IERROR
C      AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.
C      EXCEPT TO NUMBERS 0 AND  6, A SOLUTION IS NOT ATTEMPTED.
C
C      =  0  NO ERROR
C
C      =  1  A .GE. B
C
C      =  2  MBDCND .LT. 0 OR MBDCND .GT. 4
C
C      =  3  C .GE. D
C
C      =  4  N .LE. 2
C
C      =  5  NBDCND .LT. 0 OR NBDCND .GT. 4
C
C      =  6  LAMBDA .GT. 0
C
C      =  7  IDIMF .LT. M
C
C      =  8  M .LE. 2
C
C      SINCE THIS IS THE ONLY MEANS OF INDICATING A POSSIBLY
C      INCORRECT CALL TO HSTCRT, THE USER SHOULD TEST IERROR AFTER
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
C     SUBPROGRAMS    HSTCRT,POISTG,POSTG2,GENBUN,POISD2,POISN2,POISP2,
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
C     SPECIALIST     ROLAND SWEET
C
C     LANGUAGE       FORTRAN
C
C     HISTORY        WRITTEN BY ROLAND SWEET AT NCAR IN JANUARY , 1977
C
C     ALGORITHM      THIS SUBROUTINE DEFINES THE FINITE-DIFFERENCE
C                    EQUATIONS, INCORPORATES BOUNDARY DATA, ADJUSTS THE
C                    RIGHT SIDE WHEN THE SYSTEM IS SINGULAR AND CALLS
C                    EITHER POISTG OR GENBUN WHICH SOLVES THE LINEAR
C                    SYSTEM OF EQUATIONS.
C
C     SPACE          8131(DECIMAL) = 17703(OCTAL) LOCATIONS ON THE
C     REQUIRED       NCAR CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE HSTCRT IS ROUGHLY PROPORTIONAL
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
C                        32       1-4       1-4         56
C                        64       1-4       1-4        230
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
      DIMENSION       F(IDIMF,1) ,BDA(1)     ,BDB(1)     ,BDC(1)     ,
     1                BDD(1)     ,W(1)
C
C     CHECK FOR INVALID PARAMETERS.
C
      IERROR = 0
      IF (A .GE. B) IERROR = 1
      IF (MBDCND.LT.0 .OR. MBDCND.GT.4) IERROR = 2
      IF (C .GE. D) IERROR = 3
      IF (N .LE. 2) IERROR = 4
      IF (NBDCND.LT.0 .OR. NBDCND.GT.4) IERROR = 5
      IF (IDIMF .LT. M) IERROR = 7
      IF (M .LE. 2) IERROR = 8
      IF (IERROR .NE. 0) RETURN
      NPEROD = NBDCND
      MPEROD = 0
      IF (MBDCND .GT. 0) MPEROD = 1
      DELTAX = (B-A)/FLOAT(M)
      TWDELX = 1./DELTAX
      DELXSQ = 2./DELTAX**2
      DELTAY = (D-C)/FLOAT(N)
      TWDELY = 1./DELTAY
      DELYSQ = DELTAY**2
      TWDYSQ = 2./DELYSQ
      NP = NBDCND+1
      MP = MBDCND+1
C
C     DEFINE THE A,B,C COEFFICIENTS IN W-ARRAY.
C
      ID2 = M
      ID3 = ID2+M
      ID4 = ID3+M
      S = (DELTAY/DELTAX)**2
      ST2 = 2.*S
      DO 101 I=1,M
         W(I) = S
         J = ID2+I
         W(J) = -ST2+ELMBDA*DELYSQ
         J = ID3+I
         W(J) = S
  101 CONTINUE
C
C     ENTER BOUNDARY DATA FOR X-BOUNDARIES.
C
      GO TO (111,102,102,104,104),MP
  102 DO 103 J=1,N
         F(1,J) = F(1,J)-BDA(J)*DELXSQ
  103 CONTINUE
      W(ID2+1) = W(ID2+1)-W(1)
      GO TO 106
  104 DO 105 J=1,N
         F(1,J) = F(1,J)+BDA(J)*TWDELX
  105 CONTINUE
      W(ID2+1) = W(ID2+1)+W(1)
  106 GO TO (111,107,109,109,107),MP
  107 DO 108 J=1,N
         F(M,J) = F(M,J)-BDB(J)*DELXSQ
  108 CONTINUE
      W(ID3) = W(ID3)-W(1)
      GO TO 111
  109 DO 110 J=1,N
         F(M,J) = F(M,J)-BDB(J)*TWDELX
  110 CONTINUE
      W(ID3) = W(ID3)+W(1)
  111 CONTINUE
C
C     ENTER BOUNDARY DATA FOR Y-BOUNDARIES.
C
      GO TO (121,112,112,114,114),NP
  112 DO 113 I=1,M
         F(I,1) = F(I,1)-BDC(I)*TWDYSQ
  113 CONTINUE
      GO TO 116
  114 DO 115 I=1,M
         F(I,1) = F(I,1)+BDC(I)*TWDELY
  115 CONTINUE
  116 GO TO (121,117,119,119,117),NP
  117 DO 118 I=1,M
         F(I,N) = F(I,N)-BDD(I)*TWDYSQ
  118 CONTINUE
      GO TO 121
  119 DO 120 I=1,M
         F(I,N) = F(I,N)-BDD(I)*TWDELY
  120 CONTINUE
  121 CONTINUE
      DO 123 I=1,M
         DO 122 J=1,N
            F(I,J) = F(I,J)*DELYSQ
  122    CONTINUE
  123 CONTINUE
      IF (MPEROD .EQ. 0) GO TO 124
      W(1) = 0.
      W(ID4) = 0.
  124 CONTINUE
      PERTRB = 0.
      IF (ELMBDA) 133,126,125
  125 IERROR = 6
      GO TO 133
  126 GO TO (127,133,133,127,133),MP
  127 GO TO (128,133,133,128,133),NP
C
C     FOR SINGULAR PROBLEMS MUST ADJUST DATA TO INSURE THAT A SOLUTION
C     WILL EXIST.
C
  128 CONTINUE
      S = 0.
      DO 130 J=1,N
         DO 129 I=1,M
            S = S+F(I,J)
  129    CONTINUE
  130 CONTINUE
      PERTRB = S/FLOAT(M*N)
      DO 132 J=1,N
         DO 131 I=1,M
            F(I,J) = F(I,J)-PERTRB
  131    CONTINUE
  132 CONTINUE
      PERTRB = PERTRB/DELYSQ
C
C     SOLVE THE EQUATION.
C
  133 CONTINUE
      IF (NPEROD .EQ. 0) GO TO 134
      CALL POISTG (NPEROD,N,MPEROD,M,W(1),W(ID2+1),W(ID3+1),IDIMF,F,
     1             IERR1,W(ID4+1))
      GO TO 135
  134 CONTINUE
      CALL GENBUN (NPEROD,N,MPEROD,M,W(1),W(ID2+1),W(ID3+1),IDIMF,F,
     1             IERR1,W(ID4+1))
  135 CONTINUE
      W(1) = W(ID4+1)+3.*FLOAT(M)
      RETURN
      END
