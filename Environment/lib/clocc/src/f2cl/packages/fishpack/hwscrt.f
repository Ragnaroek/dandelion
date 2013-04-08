C FISHPAK2 FROM PORTLIB                                  12/30/83
      SUBROUTINE HWSCRT (A,B,M,MBDCND,BDA,BDB,C,D,N,NBDCND,BDC,BDD,
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
C          SUBROUTINE HWSCRT SOLVES THE STANDARD FIVE-POINT FINITE
C     DIFFERENCE APPROXIMATION TO THE HELMHOLTZ EQUATION IN CARTESIAN
C     COORDINATES:
C
C          (D/DX)(DU/DX) + (D/DY)(DU/DY) + LAMBDA*U = F(X,Y).
C
C
C
C     * * * * * * * *    PARAMETER DESCRIPTION     * * * * * * * * * *
C
C             * * * * * *   ON INPUT    * * * * * *
C
C     A,B
C       THE RANGE OF X, I.E., A .LE. X .LE. B.  A MUST BE LESS THAN B.
C
C     M
C       THE NUMBER OF PANELS INTO WHICH THE INTERVAL (A,B) IS
C       SUBDIVIDED.  HENCE, THERE WILL BE M+1 GRID POINTS IN THE
C       X-DIRECTION GIVEN BY X(I) = A+(I-1)DX FOR I = 1,2,...,M+1,
C       WHERE DX = (B-A)/M IS THE PANEL WIDTH. M MUST BE GREATER THAN 3.
C
C     MBDCND
C       INDICATES THE TYPE OF BOUNDARY CONDITIONS AT X = A AND X = B.
C
C       = 0  IF THE SOLUTION IS PERIODIC IN X, I.E., U(I,J) = U(M+I,J).
C       = 1  IF THE SOLUTION IS SPECIFIED AT X = A AND X = B.
C       = 2  IF THE SOLUTION IS SPECIFIED AT X = A AND THE DERIVATIVE OF
C            THE SOLUTION WITH RESPECT TO X IS SPECIFIED AT X = B.
C       = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO X IS
C            SPECIFIED AT X = A AND X = B.
C       = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO X IS
C            SPECIFIED AT X = A AND THE SOLUTION IS SPECIFIED AT X = B.
C
C     BDA
C       A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO X AT X = A.
C       WHEN MBDCND = 3 OR 4,
C
C            BDA(J) = (D/DX)U(A,Y(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE, BDA IS A DUMMY VARIABLE.
C
C     BDB
C       A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO X AT X = B.
C       WHEN MBDCND = 2 OR 3,
C
C            BDB(J) = (D/DX)U(B,Y(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE BDB IS A DUMMY VARIABLE.
C
C     C,D
C       THE RANGE OF Y, I.E., C .LE. Y .LE. D.  C MUST BE LESS THAN D.
C
C     N
C       THE NUMBER OF PANELS INTO WHICH THE INTERVAL (C,D) IS
C       SUBDIVIDED.  HENCE, THERE WILL BE N+1 GRID POINTS IN THE
C       Y-DIRECTION GIVEN BY Y(J) = C+(J-1)DY FOR J = 1,2,...,N+1, WHERE
C       DY = (D-C)/N IS THE PANEL WIDTH.  N MUST BE GREATER THAN 3.
C
C     NBDCND
C       INDICATES THE TYPE OF BOUNDARY CONDITIONS AT Y = C AND Y = D.
C
C       = 0  IF THE SOLUTION IS PERIODIC IN Y, I.E., U(I,J) = U(I,N+J).
C       = 1  IF THE SOLUTION IS SPECIFIED AT Y = C AND Y = D.
C       = 2  IF THE SOLUTION IS SPECIFIED AT Y = C AND THE DERIVATIVE OF
C            THE SOLUTION WITH RESPECT TO Y IS SPECIFIED AT Y = D.
C       = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Y IS
C            SPECIFIED AT Y = C AND Y = D.
C       = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Y IS
C            SPECIFIED AT Y = C AND THE SOLUTION IS SPECIFIED AT Y = D.
C
C     BDC
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Y AT Y = C.
C       WHEN NBDCND = 3 OR 4,
C
C            BDC(I) = (D/DY)U(X(I),C), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDC IS A DUMMY VARIABLE.
C
C     BDD
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Y AT Y = D.
C       WHEN NBDCND = 2 OR 3,
C
C            BDD(I) = (D/DY)U(X(I),D), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDD IS A DUMMY VARIABLE.
C
C     ELMBDA
C       THE CONSTANT LAMBDA IN THE HELMHOLTZ EQUATION.  IF
C       LAMBDA .GT. 0, A SOLUTION MAY NOT EXIST.  HOWEVER, HWSCRT WILL
C       ATTEMPT TO FIND A SOLUTION.
C
C     F
C       A TWO-DIMENSIONAL ARRAY WHICH SPECIFIES THE VALUES OF THE RIGHT
C       SIDE OF THE HELMHOLTZ EQUATION AND BOUNDARY VALUES (IF ANY).
C       FOR I = 2,3,...,M AND J = 2,3,...,N
C
C            F(I,J) = F(X(I),Y(J)).
C
C       ON THE BOUNDARIES F IS DEFINED BY
C
C            MBDCND     F(1,J)        F(M+1,J)
C            ------     ---------     --------
C
C              0        F(A,Y(J))     F(A,Y(J))
C              1        U(A,Y(J))     U(B,Y(J))
C              2        U(A,Y(J))     F(B,Y(J))     J = 1,2,...,N+1
C              3        F(A,Y(J))     F(B,Y(J))
C              4        F(A,Y(J))     U(B,Y(J))
C
C
C            NBDCND     F(I,1)        F(I,N+1)
C            ------     ---------     --------
C
C              0        F(X(I),C)     F(X(I),C)
C              1        U(X(I),C)     U(X(I),D)
C              2        U(X(I),C)     F(X(I),D)     I = 1,2,...,M+1
C              3        F(X(I),C)     F(X(I),D)
C              4        F(X(I),C)     U(X(I),D)
C
C       F MUST BE DIMENSIONED AT LEAST (M+1)*(N+1).
C
C       NOTE
C
C       IF THE TABLE CALLS FOR BOTH THE SOLUTION U AND THE RIGHT SIDE F
C       AT  A CORNER THEN THE SOLUTION MUST BE SPECIFIED.
C
C     IDIMF
C       THE ROW (OR FIRST) DIMENSION OF THE ARRAY F AS IT APPEARS IN THE
C       PROGRAM CALLING HWSCRT.  THIS PARAMETER IS USED TO SPECIFY THE
C       VARIABLE DIMENSION OF F.  IDIMF MUST BE AT LEAST M+1  .
C
C     W
C       A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE USER FOR
C       WORK SPACE.  W MAY REQUIRE UP TO 4*(N+1) +
C       (13 + INT(LOG2(N+1)))*(M+1) LOCATIONS.  THE ACTUAL NUMBER OF
C       LOCATIONS USED IS COMPUTED BY HWSCRT AND IS RETURNED IN LOCATION
C       W(1).
C
C
C             * * * * * *   ON OUTPUT     * * * * * *
C
C     F
C       CONTAINS THE SOLUTION U(I,J) OF THE FINITE DIFFERENCE
C       APPROXIMATION FOR THE GRID POINT (X(I),Y(J)), I = 1,2,...,M+1,
C       J = 1,2,...,N+1  .
C
C     PERTRB
C       IF A COMBINATION OF PERIODIC OR DERIVATIVE BOUNDARY CONDITIONS
C       IS SPECIFIED FOR A POISSON EQUATION (LAMBDA = 0), A SOLUTION MAY
C       NOT EXIST.  PERTRB IS A CONSTANT, CALCULATED AND SUBTRACTED FROM
C       F, WHICH ENSURES THAT A SOLUTION EXISTS.  HWSCRT THEN COMPUTES
C       THIS SOLUTION, WHICH IS A LEAST SQUARES SOLUTION TO THE ORIGINAL
C       APPROXIMATION.  THIS SOLUTION PLUS ANY CONSTANT IS ALSO A
C       SOLUTION.  HENCE, THE SOLUTION IS NOT UNIQUE.  THE VALUE OF
C       PERTRB SHOULD BE SMALL COMPARED TO THE RIGHT SIDE F.  OTHERWISE,
C       A SOLUTION IS OBTAINED TO AN ESSENTIALLY DIFFERENT PROBLEM.
C       THIS COMPARISON SHOULD ALWAYS BE MADE TO INSURE THAT A
C       MEANINGFUL SOLUTION HAS BEEN OBTAINED.
C
C     IERROR
C       AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.  EXCEPT
C       FOR NUMBERS 0 AND 6, A SOLUTION IS NOT ATTEMPTED.
C
C       = 0  NO ERROR.
C       = 1  A .GE. B.
C       = 2  MBDCND .LT. 0 OR MBDCND .GT. 4  .
C       = 3  C .GE. D.
C       = 4  N .LE. 3
C       = 5  NBDCND .LT. 0 OR NBDCND .GT. 4  .
C       = 6  LAMBDA .GT. 0  .
C       = 7  IDIMF .LT. M+1  .
C       = 8  M .LE. 3
C
C       SINCE THIS IS THE ONLY MEANS OF INDICATING A POSSIBLY INCORRECT
C       CALL TO HWSCRT, THE USER SHOULD TEST IERROR AFTER THE CALL.
C
C     W
C       W(1) CONTAINS THE REQUIRED LENGTH OF W.
C
C
C     * * * * * * *   PROGRAM SPECIFICATIONS    * * * * * * * * * * * *
C
C
C     DIMENSION OF   BDA(N+1),BDB(N+1),BDC(M+1),BDD(M+1),F(IDIMF,N+1),
C     ARGUMENTS      W(SEE ARGUMENT LIST)
C
C     LATEST         JUNE 1, 1976
C     REVISION
C
C     SUBPROGRAMS    HWSCRT,GENBUN,POISD2,POISN2,POISP2,COSGEN,MERGE,
C     REQUIRED       TRIX,TRI3,PIMACH
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
C     HISTORY        STANDARDIZED SEPTEMBER 1, 1973
C                    REVISED APRIL 1, 1976
C
C     ALGORITHM      THE ROUTINE DEFINES THE FINITE DIFFERENCE
C                    EQUATIONS, INCORPORATES BOUNDARY DATA, AND ADJUSTS
C                    THE RIGHT SIDE OF SINGULAR SYSTEMS AND THEN CALLS
C                    GENBUN TO SOLVE THE SYSTEM.
C
C     SPACE          13110(OCTAL) = 5704(DECIMAL) LOCATIONS ON THE NCAR
C     REQUIRED       CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE HWSCRT IS ROUGHLY PROPORTIONAL
C                    TO M*N*LOG2(N), BUT ALSO DEPENDS ON THE INPUT
C                    PARAMETERS NBDCND AND MBDCND.  SOME TYPICAL VALUES
C                    ARE LISTED IN THE TABLE BELOW.
C                       THE SOLUTION PROCESS EMPLOYED RESULTS IN A LOSS
C                    OF NO MORE THAN THREE SIGNIFICANT DIGITS FOR N AND
C                    M AS LARGE AS 64.  MORE DETAILED INFORMATION ABOUT
C                    ACCURACY CAN BE FOUND IN THE DOCUMENTATION FOR
C                    SUBROUTINE GENBUN WHICH IS THE ROUTINE THAT
C                    SOLVES THE FINITE DIFFERENCE EQUATIONS.
C
C
C                       M(=N)    MBDCND    NBDCND    T(MSECS)
C                       -----    ------    ------    --------
C
C                        32        0         0          31
C                        32        1         1          23
C                        32        3         3          36
C                        64        0         0         128
C                        64        1         1          96
C                        64        3         3         142
C
C     PORTABILITY    AMERICAN NATIONAL STANDARDS INSTITUTE FORTRAN.
C                    ALL MACHINE DEPENDENT CONSTANTS ARE LOCATED IN THE
C                    FUNCTION PIMACH.
C
C     REFERENCE      SWARZTRAUBER,P. AND R. SWEET, 'EFFICIENT FORTRAN
C                    SUBPROGRAMS FOR THE SOLUTION OF ELLIPTIC EQUATIONS'
C                    NCAR TN/IA-109, JULY, 1975, 138 PP.
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
      DIMENSION       F(IDIMF,1)
      DIMENSION       BDA(1)     ,BDB(1)     ,BDC(1)     ,BDD(1)     ,
     1                W(1)
C
C     CHECK FOR INVALID PARAMETERS.
C
      IERROR = 0
      IF (A .GE. B) IERROR = 1
      IF (MBDCND.LT.0 .OR. MBDCND.GT.4) IERROR = 2
      IF (C .GE. D) IERROR = 3
      IF (N .LE. 3) IERROR = 4
      IF (NBDCND.LT.0 .OR. NBDCND.GT.4) IERROR = 5
      IF (IDIMF .LT. M+1) IERROR = 7
      IF (M .LE. 3) IERROR = 8
      IF (IERROR .NE. 0) RETURN
      NPEROD = NBDCND
      MPEROD = 0
      IF (MBDCND .GT. 0) MPEROD = 1
      DELTAX = (B-A)/FLOAT(M)
      TWDELX = 2./DELTAX
      DELXSQ = 1./DELTAX**2
      DELTAY = (D-C)/FLOAT(N)
      TWDELY = 2./DELTAY
      DELYSQ = 1./DELTAY**2
      NP = NBDCND+1
      NP1 = N+1
      MP = MBDCND+1
      MP1 = M+1
      NSTART = 1
      NSTOP = N
      NSKIP = 1
      GO TO (104,101,102,103,104),NP
  101 NSTART = 2
      GO TO 104
  102 NSTART = 2
  103 NSTOP = NP1
      NSKIP = 2
  104 NUNK = NSTOP-NSTART+1
C
C     ENTER BOUNDARY DATA FOR X-BOUNDARIES.
C
      MSTART = 1
      MSTOP = M
      MSKIP = 1
      GO TO (117,105,106,109,110),MP
  105 MSTART = 2
      GO TO 107
  106 MSTART = 2
      MSTOP = MP1
      MSKIP = 2
  107 DO 108 J=NSTART,NSTOP
         F(2,J) = F(2,J)-F(1,J)*DELXSQ
  108 CONTINUE
      GO TO 112
  109 MSTOP = MP1
      MSKIP = 2
  110 DO 111 J=NSTART,NSTOP
         F(1,J) = F(1,J)+BDA(J)*TWDELX
  111 CONTINUE
  112 GO TO (113,115),MSKIP
  113 DO 114 J=NSTART,NSTOP
         F(M,J) = F(M,J)-F(MP1,J)*DELXSQ
  114 CONTINUE
      GO TO 117
  115 DO 116 J=NSTART,NSTOP
         F(MP1,J) = F(MP1,J)-BDB(J)*TWDELX
  116 CONTINUE
  117 MUNK = MSTOP-MSTART+1
C
C     ENTER BOUNDARY DATA FOR Y-BOUNDARIES.
C
      GO TO (127,118,118,120,120),NP
  118 DO 119 I=MSTART,MSTOP
         F(I,2) = F(I,2)-F(I,1)*DELYSQ
  119 CONTINUE
      GO TO 122
  120 DO 121 I=MSTART,MSTOP
         F(I,1) = F(I,1)+BDC(I)*TWDELY
  121 CONTINUE
  122 GO TO (123,125),NSKIP
  123 DO 124 I=MSTART,MSTOP
         F(I,N) = F(I,N)-F(I,NP1)*DELYSQ
  124 CONTINUE
      GO TO 127
  125 DO 126 I=MSTART,MSTOP
         F(I,NP1) = F(I,NP1)-BDD(I)*TWDELY
  126 CONTINUE
C
C    MULTIPLY RIGHT SIDE BY DELTAY**2.
C
  127 DELYSQ = DELTAY*DELTAY
      DO 129 I=MSTART,MSTOP
         DO 128 J=NSTART,NSTOP
            F(I,J) = F(I,J)*DELYSQ
  128    CONTINUE
  129 CONTINUE
C
C     DEFINE THE A,B,C COEFFICIENTS IN W-ARRAY.
C
      ID2 = MUNK
      ID3 = ID2+MUNK
      ID4 = ID3+MUNK
      S = DELYSQ*DELXSQ
      ST2 = 2.*S
      DO 130 I=1,MUNK
         W(I) = S
         J = ID2+I
         W(J) = -ST2+ELMBDA*DELYSQ
         J = ID3+I
         W(J) = S
  130 CONTINUE
      IF (MP .EQ. 1) GO TO 131
      W(1) = 0.
      W(ID4) = 0.
  131 CONTINUE
      GO TO (135,135,132,133,134),MP
  132 W(ID2) = ST2
      GO TO 135
  133 W(ID2) = ST2
  134 W(ID3+1) = ST2
  135 CONTINUE
      PERTRB = 0.
      IF (ELMBDA) 144,137,136
  136 IERROR = 6
      GO TO 144
  137 IF ((NBDCND.EQ.0 .OR. NBDCND.EQ.3) .AND.
     1    (MBDCND.EQ.0 .OR. MBDCND.EQ.3)) GO TO 138
      GO TO 144
C
C     FOR SINGULAR PROBLEMS MUST ADJUST DATA TO INSURE THAT A SOLUTION
C     WILL EXIST.
C
  138 A1 = 1.
      A2 = 1.
      IF (NBDCND .EQ. 3) A2 = 2.
      IF (MBDCND .EQ. 3) A1 = 2.
      S1 = 0.
      MSP1 = MSTART+1
      MSTM1 = MSTOP-1
      NSP1 = NSTART+1
      NSTM1 = NSTOP-1
      DO 140 J=NSP1,NSTM1
         S = 0.
         DO 139 I=MSP1,MSTM1
            S = S+F(I,J)
  139    CONTINUE
         S1 = S1+S*A1+F(MSTART,J)+F(MSTOP,J)
  140 CONTINUE
      S1 = A2*S1
      S = 0.
      DO 141 I=MSP1,MSTM1
         S = S+F(I,NSTART)+F(I,NSTOP)
  141 CONTINUE
      S1 = S1+S*A1+F(MSTART,NSTART)+F(MSTART,NSTOP)+F(MSTOP,NSTART)+
     1     F(MSTOP,NSTOP)
      S = (2.+FLOAT(NUNK-2)*A2)*(2.+FLOAT(MUNK-2)*A1)
      PERTRB = S1/S
      DO 143 J=NSTART,NSTOP
         DO 142 I=MSTART,MSTOP
            F(I,J) = F(I,J)-PERTRB
  142    CONTINUE
  143 CONTINUE
      PERTRB = PERTRB/DELYSQ
C
C     SOLVE THE EQUATION.
C
  144 CALL GENBUN (NPEROD,NUNK,MPEROD,MUNK,W(1),W(ID2+1),W(ID3+1),
     1             IDIMF,F(MSTART,NSTART),IERR1,W(ID4+1))
      W(1) = W(ID4+1)+3.*FLOAT(MUNK)
C
C     FILL IN IDENTICAL VALUES WHEN HAVE PERIODIC BOUNDARY CONDITIONS.
C
      IF (NBDCND .NE. 0) GO TO 146
      DO 145 I=MSTART,MSTOP
         F(I,NP1) = F(I,1)
  145 CONTINUE
  146 IF (MBDCND .NE. 0) GO TO 148
      DO 147 J=NSTART,NSTOP
         F(MP1,J) = F(1,J)
  147 CONTINUE
      IF (NBDCND .EQ. 0) F(MP1,NP1) = F(1,NP1)
  148 CONTINUE
      RETURN
      END
