C FISHPAK4 FROM PORTLIB                                  12/30/83
      SUBROUTINE HWSCYL (A,B,M,MBDCND,BDA,BDB,C,D,N,NBDCND,BDC,BDD,
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
C
C     SUBROUTINE HWSCYL SOLVES A FINITE DIFFERENCE APPROXIMATION TO THE
C     HELMHOLTZ EQUATION IN CYLINDRICAL COORDINATES:
C
C          (1/R)(D/DR)(R(DU/DR)) + (D/DZ)(DU/DZ)
C
C                                + (LAMBDA/R**2)U = F(R,Z)
C
C     THIS MODIFIED HELMHOLTZ EQUATION RESULTS FROM THE FOURIER
C     TRANSFORM OF THE THREE-DIMENSIONAL POISSON EQUATION.
C
C     * * * * * * * *    PARAMETER DESCRIPTION     * * * * * * * * * *
C
C             * * * * * *   ON INPUT    * * * * * *
C
C     A,B
C       THE RANGE OF R, I.E., A .LE. R .LE. B.  A MUST BE LESS THAN B
C       AND A MUST BE NON-NEGATIVE.
C
C     M
C       THE NUMBER OF PANELS INTO WHICH THE INTERVAL (A,B) IS
C       SUBDIVIDED.  HENCE, THERE WILL BE M+1 GRID POINTS IN THE
C       R-DIRECTION GIVEN BY R(I) = A+(I-1)DR, FOR I = 1,2,...,M+1,
C       WHERE DR = (B-A)/M IS THE PANEL WIDTH. M MUST BE GREATER THAN 3.
C
C     MBDCND
C       INDICATES THE TYPE OF BOUNDARY CONDITIONS AT R = A AND R = B.
C
C       = 1  IF THE SOLUTION IS SPECIFIED AT R = A AND R = B.
C       = 2  IF THE SOLUTION IS SPECIFIED AT R = A AND THE DERIVATIVE OF
C            THE SOLUTION WITH RESPECT TO R IS SPECIFIED AT R = B.
C       = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO R IS
C            SPECIFIED AT R = A (SEE NOTE BELOW) AND R = B.
C       = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO R IS
C            SPECIFIED AT R = A (SEE NOTE BELOW) AND THE SOLUTION IS
C            SPECIFIED AT R = B.
C       = 5  IF THE SOLUTION IS UNSPECIFIED AT R = A = 0 AND THE
C            SOLUTION IS SPECIFIED AT R = B.
C       = 6  IF THE SOLUTION IS UNSPECIFIED AT R = A = 0 AND THE
C            DERIVATIVE OF THE SOLUTION WITH RESPECT TO R IS SPECIFIED
C            AT R = B.
C
C       NOTE:  IF A = 0, DO NOT USE MBDCND = 3 OR 4, BUT INSTEAD USE
C              MBDCND = 1,2,5, OR 6  .
C
C     BDA
C       A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO R AT R = A.
C       WHEN MBDCND = 3 OR 4,
C
C            BDA(J) = (D/DR)U(A,Z(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE, BDA IS A DUMMY VARIABLE.
C
C     BDB
C       A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO R AT R = B.
C       WHEN MBDCND = 2,3, OR 6,
C
C            BDB(J) = (D/DR)U(B,Z(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE, BDB IS A DUMMY VARIABLE.
C
C     C,D
C       THE RANGE OF Z, I.E., C .LE. Z .LE. D.  C MUST BE LESS THAN D.
C
C     N
C       THE NUMBER OF PANELS INTO WHICH THE INTERVAL (C,D) IS
C       SUBDIVIDED.  HENCE, THERE WILL BE N+1 GRID POINTS IN THE
C       Z-DIRECTION GIVEN BY Z(J) = C+(J-1)DZ, FOR J = 1,2,...,N+1,
C       WHERE DZ = (D-C)/N IS THE PANEL WIDTH. N MUST BE GREATER THAN 3.
C
C     NBDCND
C       INDICATES THE TYPE OF BOUNDARY CONDITIONS AT Z = C AND Z = D.
C
C       = 0  IF THE SOLUTION IS PERIODIC IN Z, I.E., U(I,1) = U(I,N+1).
C       = 1  IF THE SOLUTION IS SPECIFIED AT Z = C AND Z = D.
C       = 2  IF THE SOLUTION IS SPECIFIED AT Z = C AND THE DERIVATIVE OF
C            THE SOLUTION WITH RESPECT TO Z IS SPECIFIED AT Z = D.
C       = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Z IS
C            SPECIFIED AT Z = C AND Z = D.
C       = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Z IS
C            SPECIFIED AT Z = C AND THE SOLUTION IS SPECIFIED AT Z = D.
C
C     BDC
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Z AT Z = C.
C       WHEN NBDCND = 3 OR 4,
C
C            BDC(I) = (D/DZ)U(R(I),C), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDC IS A DUMMY VARIABLE.
C
C     BDD
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO Z AT Z = D.
C       WHEN NBDCND = 2 OR 3,
C
C            BDD(I) = (D/DZ)U(R(I),D), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDD IS A DUMMY VARIABLE.
C
C     ELMBDA
C       THE CONSTANT LAMBDA IN THE HELMHOLTZ EQUATION.  IF
C       LAMBDA .GT. 0, A SOLUTION MAY NOT EXIST.  HOWEVER, HWSCYL WILL
C       ATTEMPT TO FIND A SOLUTION.  LAMBDA MUST BE ZERO WHEN
C       MBDCND = 5 OR 6  .
C
C     F
C       A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUES OF THE RIGHT
C       SIDE OF THE HELMHOLTZ EQUATION AND BOUNDARY DATA (IF ANY).  FOR
C       I = 2,3,...,M AND J = 2,3,...,N
C
C            F(I,J) = F(R(I),Z(J)).
C
C       ON THE BOUNDARIES F IS DEFINED BY
C
C            MBDCND   F(1,J)            F(M+1,J)
C            ------   ---------         ---------
C
C              1      U(A,Z(J))         U(B,Z(J))
C              2      U(A,Z(J))         F(B,Z(J))
C              3      F(A,Z(J))         F(B,Z(J))   J = 1,2,...,N+1
C              4      F(A,Z(J))         U(B,Z(J))
C              5      F(0,Z(J))         U(B,Z(J))
C              6      F(0,Z(J))         F(B,Z(J))
C
C            NBDCND   F(I,1)            F(I,N+1)
C            ------   ---------         ---------
C
C              0      F(R(I),C)         F(R(I),C)
C              1      U(R(I),C)         U(R(I),D)
C              2      U(R(I),C)         F(R(I),D)   I = 1,2,...,M+1
C              3      F(R(I),C)         F(R(I),D)
C              4      F(R(I),C)         U(R(I),D)
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
C       PROGRAM CALLING HWSCYL.  THIS PARAMETER IS USED TO SPECIFY THE
C       VARIABLE DIMENSION OF F.  IDIMF MUST BE AT LEAST M+1  .
C
C     W
C       A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE USER FOR
C       WORK SPACE.  W MAY REQUIRE UP TO 4*(N+1) +
C       (13 + INT(LOG2(N+1)))*(M+1) LOCATIONS.  THE ACTUAL NUMBER OF
C       LOCATIONS USED IS COMPUTED BY HWSCYL AND IS RETURNED IN LOCATION
C       W(1).
C
C
C             * * * * * *   ON OUTPUT     * * * * * *
C
C     F
C       CONTAINS THE SOLUTION U(I,J) OF THE FINITE DIFFERENCE
C       APPROXIMATION FOR THE GRID POINT (R(I),Z(J)), I = 1,2,...,M+1,
C       J = 1,2,...,N+1  .
C
C     PERTRB
C       IF ONE SPECIFIES A COMBINATION OF PERIODIC, DERIVATIVE, AND
C       UNSPECIFIED BOUNDARY CONDITIONS FOR A POISSON EQUATION
C       (LAMBDA = 0), A SOLUTION MAY NOT EXIST.  PERTRB IS A CONSTANT,
C       CALCULATED AND SUBTRACTED FROM F, WHICH ENSURES THAT A SOLUTION
C       EXISTS.  HWSCYL THEN COMPUTES THIS SOLUTION, WHICH IS A LEAST
C       SQUARES SOLUTION TO THE ORIGINAL APPROXIMATION.  THIS SOLUTION
C       PLUS ANY CONSTANT IS ALSO A SOLUTION.  HENCE, THE SOLUTION IS
C       NOT UNIQUE.  THE VALUE OF PERTRB SHOULD BE SMALL COMPARED TO THE
C       RIGHT SIDE F.  OTHERWISE, A SOLUTION IS OBTAINED TO AN
C       ESSENTIALLY DIFFERENT PROBLEM.  THIS COMPARISON SHOULD ALWAYS
C       BE MADE TO INSURE THAT A MEANINGFUL SOLUTION HAS BEEN OBTAINED.
C
C     IERROR
C       AN ERROR FLAG WHICH INDICATES INVALID INPUT PARAMETERS.  EXCEPT
C       FOR NUMBERS 0 AND 11, A SOLUTION IS NOT ATTEMPTED.
C
C       =  0  NO ERROR.
C       =  1  A .LT. 0  .
C       =  2  A .GE. B.
C       =  3  MBDCND .LT. 1 OR MBDCND .GT. 6  .
C       =  4  C .GE. D.
C       =  5  N .LE. 3
C       =  6  NBDCND .LT. 0 OR NBDCND .GT. 4  .
C       =  7  A = 0, MBDCND = 3 OR 4  .
C       =  8  A .GT. 0, MBDCND .GE. 5  .
C       =  9  A = 0, LAMBDA .NE. 0, MBDCND .GE. 5  .
C       = 10  IDIMF .LT. M+1  .
C       = 11  LAMBDA .GT. 0  .
C       = 12  M .LE. 3
C
C       SINCE THIS IS THE ONLY MEANS OF INDICATING A POSSIBLY INCORRECT
C       CALL TO HWSCYL, THE USER SHOULD TEST IERROR AFTER THE CALL.
C
C     W
C       W(1) CONTAINS THE REQUIRED LENGTH OF W.
C
C
C     * * * * * * *   PROGRAM SPECIFICATIONS    * * * * * * * * * * * *
C
C     DIMENSION OF   BDA(N+1),BDB(N+1),BDC(M+1),BDD(M+1),F(IDIMF,N+1),
C     ARGUMENTS      W(SEE ARGUMENT LIST)
C
C     LATEST         JUNE 1, 1976
C     REVISION
C
C     SUBPROGRAMS    HWSCYL,GENBUN,POISD2,POISN2,POISP2,COSGEN,MERGE,
C     REQUIRED      TRIX,TRI3,PIMACH
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
C     SPACE          5818(DECIMAL) = 13272(OCTAL) LOCATIONS ON THE NCAR
C     REQUIRED       CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE HWSCYL IS ROUGHLY PROPORTIONAL
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
C                        32        1         0          31
C                        32        1         1          23
C                        32        3         3          36
C                        64        1         0         128
C                        64        1         1          96
C                        64        3         3         142
C
C     PORTABILITY    AMERICAN NATIONAL STANDARDS INSTITUTE FORTRAN.
C                    ALL MACHINE DEPENDENT CONSTANTS ARE LOCATED IN THE
C                    FUNCTION PIMACH.
C
C     REQUIRED       COS
C     RESIDENT
C     ROUTINES
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
      IF (A .LT. 0.) IERROR = 1
      IF (A .GE. B) IERROR = 2
      IF (MBDCND.LE.0 .OR. MBDCND.GE.7) IERROR = 3
      IF (C .GE. D) IERROR = 4
      IF (N .LE. 3) IERROR = 5
      IF (NBDCND.LE.-1 .OR. NBDCND.GE.5) IERROR = 6
      IF (A.EQ.0. .AND. (MBDCND.EQ.3 .OR. MBDCND.EQ.4)) IERROR = 7
      IF (A.GT.0. .AND. MBDCND.GE.5) IERROR = 8
      IF (A.EQ.0. .AND. ELMBDA.NE.0. .AND. MBDCND.GE.5) IERROR = 9
      IF (IDIMF .LT. M+1) IERROR = 10
      IF (M .LE. 3) IERROR = 12
      IF (IERROR .NE. 0) RETURN
      MP1 = M+1
      DELTAR = (B-A)/FLOAT(M)
      DLRBY2 = DELTAR/2.
      DLRSQ = DELTAR**2
      NP1 = N+1
      DELTHT = (D-C)/FLOAT(N)
      DLTHSQ = DELTHT**2
      NP = NBDCND+1
C
C     DEFINE RANGE OF INDICES I AND J FOR UNKNOWNS U(I,J).
C
      MSTART = 2
      MSTOP = M
      GO TO (104,103,102,101,101,102),MBDCND
  101 MSTART = 1
      GO TO 104
  102 MSTART = 1
  103 MSTOP = MP1
  104 MUNK = MSTOP-MSTART+1
      NSTART = 1
      NSTOP = N
      GO TO (108,105,106,107,108),NP
  105 NSTART = 2
      GO TO 108
  106 NSTART = 2
  107 NSTOP = NP1
  108 NUNK = NSTOP-NSTART+1
C
C     DEFINE A,B,C COEFFICIENTS IN W-ARRAY.
C
      ID2 = MUNK
      ID3 = ID2+MUNK
      ID4 = ID3+MUNK
      ID5 = ID4+MUNK
      ID6 = ID5+MUNK
      ISTART = 1
      A1 = 2./DLRSQ
      IJ = 0
      IF (MBDCND.EQ.3 .OR. MBDCND.EQ.4) IJ = 1
      IF (MBDCND .LE. 4) GO TO 109
      W(1) = 0.
      W(ID2+1) = -2.*A1
      W(ID3+1) = 2.*A1
      ISTART = 2
      IJ = 1
  109 DO 110 I=ISTART,MUNK
         R = A+FLOAT(I-IJ)*DELTAR
         J = ID5+I
         W(J) = R
         J = ID6+I
         W(J) = 1./R**2
         W(I) = (R-DLRBY2)/(R*DLRSQ)
         J = ID3+I
         W(J) = (R+DLRBY2)/(R*DLRSQ)
         K = ID6+I
         J = ID2+I
         W(J) = -A1+ELMBDA*W(K)
  110 CONTINUE
      GO TO (114,111,112,113,114,112),MBDCND
  111 W(ID2) = A1
      GO TO 114
  112 W(ID2) = A1
  113 W(ID3+1) = A1*FLOAT(ISTART)
  114 CONTINUE
C
C     ENTER BOUNDARY DATA FOR R-BOUNDARIES.
C
      GO TO (115,115,117,117,119,119),MBDCND
  115 A1 = W(1)
      DO 116 J=NSTART,NSTOP
         F(2,J) = F(2,J)-A1*F(1,J)
  116 CONTINUE
      GO TO 119
  117 A1 = 2.*DELTAR*W(1)
      DO 118 J=NSTART,NSTOP
         F(1,J) = F(1,J)+A1*BDA(J)
  118 CONTINUE
  119 GO TO (120,122,122,120,120,122),MBDCND
  120 A1 = W(ID4)
      DO 121 J=NSTART,NSTOP
         F(M,J) = F(M,J)-A1*F(MP1,J)
  121 CONTINUE
      GO TO 124
  122 A1 = 2.*DELTAR*W(ID4)
      DO 123 J=NSTART,NSTOP
         F(MP1,J) = F(MP1,J)-A1*BDB(J)
  123 CONTINUE
C
C     ENTER BOUNDARY DATA FOR Z-BOUNDARIES.
C
  124 A1 = 1./DLTHSQ
      L = ID5-MSTART+1
      GO TO (134,125,125,127,127),NP
  125 DO 126 I=MSTART,MSTOP
         F(I,2) = F(I,2)-A1*F(I,1)
  126 CONTINUE
      GO TO 129
  127 A1 = 2./DELTHT
      DO 128 I=MSTART,MSTOP
         F(I,1) = F(I,1)+A1*BDC(I)
  128 CONTINUE
  129 A1 = 1./DLTHSQ
      GO TO (134,130,132,132,130),NP
  130 DO 131 I=MSTART,MSTOP
         F(I,N) = F(I,N)-A1*F(I,NP1)
  131 CONTINUE
      GO TO 134
  132 A1 = 2./DELTHT
      DO 133 I=MSTART,MSTOP
         F(I,NP1) = F(I,NP1)-A1*BDD(I)
  133 CONTINUE
  134 CONTINUE
C
C     ADJUST RIGHT SIDE OF SINGULAR PROBLEMS TO INSURE EXISTENCE OF A
C     SOLUTION.
C
      PERTRB = 0.
      IF (ELMBDA) 146,136,135
  135 IERROR = 11
      GO TO 146
  136 W(ID5+1) = .5*(W(ID5+2)-DLRBY2)
      GO TO (146,146,138,146,146,137),MBDCND
  137 W(ID5+1) = .5*W(ID5+1)
  138 GO TO (140,146,146,139,146),NP
  139 A2 = 2.
      GO TO 141
  140 A2 = 1.
  141 K = ID5+MUNK
      W(K) = .5*(W(K-1)+DLRBY2)
      S = 0.
      DO 143 I=MSTART,MSTOP
         S1 = 0.
         NSP1 = NSTART+1
         NSTM1 = NSTOP-1
         DO 142 J=NSP1,NSTM1
            S1 = S1+F(I,J)
  142    CONTINUE
         K = I+L
         S = S+(A2*S1+F(I,NSTART)+F(I,NSTOP))*W(K)
  143 CONTINUE
      S2 = FLOAT(M)*A+(.75+FLOAT((M-1)*(M+1)))*DLRBY2
      IF (MBDCND .EQ. 3) S2 = S2+.25*DLRBY2
      S1 = (2.+A2*FLOAT(NUNK-2))*S2
      PERTRB = S/S1
      DO 145 I=MSTART,MSTOP
         DO 144 J=NSTART,NSTOP
            F(I,J) = F(I,J)-PERTRB
  144    CONTINUE
  145 CONTINUE
  146 CONTINUE
C
C     MULTIPLY I-TH EQUATION THROUGH BY DELTHT**2 TO PUT EQUATION INTO
C     CORRECT FORM FOR SUBROUTINE GENBUN.
C
      DO 148 I=MSTART,MSTOP
         K = I-MSTART+1
         W(K) = W(K)*DLTHSQ
         J = ID2+K
         W(J) = W(J)*DLTHSQ
         J = ID3+K
         W(J) = W(J)*DLTHSQ
         DO 147 J=NSTART,NSTOP
            F(I,J) = F(I,J)*DLTHSQ
  147    CONTINUE
  148 CONTINUE
      W(1) = 0.
      W(ID4) = 0.
C
C     CALL GENBUN TO SOLVE THE SYSTEM OF EQUATIONS.
C
      CALL GENBUN (NBDCND,NUNK,1,MUNK,W(1),W(ID2+1),W(ID3+1),IDIMF,
     1             F(MSTART,NSTART),IERR1,W(ID4+1))
      W(1) = W(ID4+1)+3.*FLOAT(MUNK)
      IF (NBDCND .NE. 0) GO TO 150
      DO 149 I=MSTART,MSTOP
         F(I,NP1) = F(I,1)
  149 CONTINUE
  150 CONTINUE
      RETURN
      END
