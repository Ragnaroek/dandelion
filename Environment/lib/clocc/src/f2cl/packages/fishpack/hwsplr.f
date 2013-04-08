C FISHPAK3 FROM PORTLIB                                  12/30/83
      SUBROUTINE HWSPLR (A,B,M,MBDCND,BDA,BDB,C,D,N,NBDCND,BDC,BDD,
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
C     SUBROUTINE HWSPLR SOLVES A FINITE DIFFERENCE APPROXIMATION TO THE
C     HELMHOLTZ EQUATION IN POLAR COORDINATES:
C
C          (1/R)(D/DR)(R(DU/DR)) + (1/R**2)(D/DTHETA)(DU/DTHETA)
C
C                                + LAMBDA*U = F(R,THETA).
C
C
C
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
C       INDICATES THE TYPE OF BOUNDARY CONDITION AT R = A AND R = B.
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
C            BDA(J) = (D/DR)U(A,THETA(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE, BDA IS A DUMMY VARIABLE.
C
C     BDB
C       A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO R AT R = B.
C       WHEN MBDCND = 2,3, OR 6,
C
C            BDB(J) = (D/DR)U(B,THETA(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE, BDB IS A DUMMY VARIABLE.
C
C     C,D
C       THE RANGE OF THETA, I.E., C .LE. THETA .LE. D.  C MUST BE LESS
C       THAN D.
C
C     N
C       THE NUMBER OF PANELS INTO WHICH THE INTERVAL (C,D) IS
C       SUBDIVIDED.  HENCE, THERE WILL BE N+1 GRID POINTS IN THE
C       THETA-DIRECTION GIVEN BY THETA(J) = C+(J-1)DTHETA FOR
C       J = 1,2,...,N+1, WHERE DTHETA = (D-C)/N IS THE PANEL WIDTH.  N
C       MUST BE GREATER THAN 3.
C
C     NBDCND
C       INDICATES THE TYPE OF BOUNDARY CONDITIONS AT THETA = C AND
C       AND THETA = D.
C
C       = 0  IF THE SOLUTION IS PERIODIC IN THETA, I.E.,
C            U(I,J) = U(I,N+J).
C       = 1  IF THE SOLUTION IS SPECIFIED AT THETA = C AND THETA = D
C            (SEE NOTE BELOW).
C       = 2  IF THE SOLUTION IS SPECIFIED AT THETA = C AND THE
C            DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C            SPECIFIED AT THETA = D (SEE NOTE BELOW).
C       = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C            SPECIFIED AT THETA = C AND THE SOLUTION IS SPECIFIED AT
C            THETA = D (SEE NOTE BELOW).
C
C       NOTE:  WHEN NBDCND = 1,2, OR 4, DO NOT USE MBDCND = 5 OR 6
C              (THE FORMER INDICATES THAT THE SOLUTION IS SPECIFIED AT
C              R = 0, THE LATTER INDICATES THE SOLUTION IS UNSPECIFIED
C              AT R = 0).  USE INSTEAD MBDCND = 1 OR 2  .
C
C     BDC
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA AT
C       THETA = C.  WHEN NBDCND = 3 OR 4,
C
C            BDC(I) = (D/DTHETA)U(R(I),C), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDC IS A DUMMY VARIABLE.
C
C     BDD
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA AT
C       THETA = D.  WHEN NBDCND = 2 OR 3,
C
C            BDD(I) = (D/DTHETA)U(R(I),D), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDD IS A DUMMY VARIABLE.
C
C     ELMBDA
C       THE CONSTANT LAMBDA IN THE HELMHOLTZ EQUATION.  IF
C       LAMBDA .LT. 0, A SOLUTION MAY NOT EXIST.  HOWEVER, HWSPLR WILL
C       ATTEMPT TO FIND A SOLUTION.
C
C     F
C       A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUES OF THE RIGHT
C       SIDE OF THE HELMHOLTZ EQUATION AND BOUNDARY VALUES (IF ANY).
C       FOR I = 2,3,...,M AND J = 2,3,...,N
C
C            F(I,J) = F(R(I),THETA(J)).
C
C       ON THE BOUNDARIES F IS DEFINED BY
C
C            MBDCND   F(1,J)            F(M+1,J)
C            ------   -------------     -------------
C
C              1      U(A,THETA(J))     U(B,THETA(J))
C              2      U(A,THETA(J))     F(B,THETA(J))
C              3      F(A,THETA(J))     F(B,THETA(J))
C              4      F(A,THETA(J))     U(B,THETA(J))   J = 1,2,...,N+1
C              5      F(0,0)            U(B,THETA(J))
C              6      F(0,0)            F(B,THETA(J))
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
C
C     IDIMF
C       THE ROW (OR FIRST) DIMENSION OF THE ARRAY F AS IT APPEARS IN THE
C       PROGRAM CALLING HWSPLR.  THIS PARAMETER IS USED TO SPECIFY THE
C       VARIABLE DIMENSION OF F.  IDIMF MUST BE AT LEAST M+1  .
C
C     W
C       A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE USER FOR
C       WORK SPACE.  W MAY REQUIRE UP TO 4*(N+1) +
C       (13 + INT(LOG2(N+1)))*(M+1) LOCATIONS.  THE ACTUAL NUMBER OF
C       LOCATIONS USED IS COMPUTED BY HWSPLR AND IS RETURNED IN LOCATION
C       W(1).
C
C
C             * * * * * *   ON OUTPUT     * * * * * *
C
C     F
C       CONTAINS THE SOLUTION U(I,J) OF THE FINITE DIFFERENCE
C       APPROXIMATION FOR THE GRID POINT (R(I),THETA(J)),
C       I = 1,2,...,M+1, J = 1,2,...,N+1  .
C
C     PERTRB
C       IF A COMBINATION OF PERIODIC, DERIVATIVE, OR UNSPECIFIED
C       BOUNDARY CONDITIONS IS SPECIFIED FOR A POISSON EQUATION
C       (LAMBDA = 0), A SOLUTION MAY NOT EXIST.  PERTRB IS A CONSTANT,
C       CALCULATED AND SUBTRACTED FROM F, WHICH ENSURES THAT A SOLUTION
C       EXISTS.  HWSPLR THEN COMPUTES THIS SOLUTION, WHICH IS A LEAST
C       SQUARES SOLUTION TO THE ORIGINAL APPROXIMATION.  THIS SOLUTION
C       PLUS ANY CONSTANT IS ALSO A SOLUTION.  HENCE, THE SOLUTION IS
C       NOT UNIQUE.  PERTRB SHOULD BE SMALL COMPARED TO THE RIGHT SIDE.
C       OTHERWISE, A SOLUTION IS OBTAINED TO AN ESSENTIALLY DIFFERENT
C       PROBLEM.  THIS COMPARISON SHOULD ALWAYS BE MADE TO INSURE THAT A
C       MEANINGFUL SOLUTIN HAS BEEN OBTAINED.
C
C     IERROR
C       AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.  EXCEPT
C       FOR NUMBERS 0 AND 11, A SOLUTION IS NOT ATTEMPTED.
C
C       =  0  NO ERROR.
C       =  1  A .LT. 0  .
C       =  2  A .GE. B.
C       =  3  MBDCND .LT. 1 OR MBDCND .GT. 6  .
C       =  4  C .GE. D.
C       =  5  N .LE. 3
C       =  6  NBDCND .LT. 0 OR .GT. 4  .
C       =  7  A = 0, MBDCND = 3 OR 4  .
C       =  8  A .GT. 0, MBDCND .GE. 5  .
C       =  9  MBDCND .GE. 5, NBDCND .NE. 0 AND NBDCND .NE. 3  .
C       = 10  IDIMF .LT. M+1  .
C       = 11  LAMBDA .GT. 0  .
C       = 12  M .LE. 3
C
C       SINCE THIS IS THE ONLY MEANS OF INDICATING A POSSIBLY INCORRECT
C       CALL TO HWSPLR, THE USER SHOULD TEST IERROR AFTER THE CALL.
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
C     SUBPROGRAMS    HWSPLR,GENBUN,POISD2,POISN2,POISP2,COSGEN,MERGE,
C     REQUIRED       TRIX,TRI3,PIMACH
C
C     SPECIAL        NONE
C     CONDITIONS
C
C     COMMON         NONE
C     BLOCKS
C
C     I/O
C
C     PRECISION      SINGLE
C
C     SPECIALIST     ROLAND SWEET
C
C     LANGUAGE       FORTRAN
C
C     HISTORY        STANDARDIZED APRIL 1, 1973
C                    REVISED JANUARY 1, 1976
C
C     ALGORITHM      THE ROUTINE DEFINES THE FINITE DIFFERENCE
C                    EQUATIONS, INCORPORATES BOUNDARY DATA, AND ADJUSTS
C                    THE RIGHT SIDE OF SINGULAR SYSTEMS AND THEN CALLS
C                    GENBUN TO SOLVE THE SYSTEM.
C
C     SPACE          13430(OCTAL) = 5912(DECIMAL)  LOCATIONS ON THE NCAR
C     REQUIRED       CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE HWSPLR IS ROUGHLY PROPORTIONAL
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
C                    ALL MACHINE DEPENDENT CONSTANTS ARE LOCATED  IN THE
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
      IF (MBDCND.GE.5 .AND. NBDCND.NE.0 .AND. NBDCND.NE.3) IERROR = 9
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
      MSTOP = MP1
      GO TO (101,105,102,103,104,105),MBDCND
  101 MSTOP = M
      GO TO 105
  102 MSTART = 1
      GO TO 105
  103 MSTART = 1
  104 MSTOP = M
  105 MUNK = MSTOP-MSTART+1
      NSTART = 1
      NSTOP = N
      GO TO (109,106,107,108,109),NP
  106 NSTART = 2
      GO TO 109
  107 NSTART = 2
  108 NSTOP = NP1
  109 NUNK = NSTOP-NSTART+1
C
C     DEFINE A,B,C COEFFICIENTS IN W-ARRAY.
C
      ID2 = MUNK
      ID3 = ID2+MUNK
      ID4 = ID3+MUNK
      ID5 = ID4+MUNK
      ID6 = ID5+MUNK
      A1 = 2./DLRSQ
      IJ = 0
      IF (MBDCND.EQ.3 .OR. MBDCND.EQ.4) IJ = 1
      DO 110 I=1,MUNK
         R = A+FLOAT(I-IJ)*DELTAR
         J = ID5+I
         W(J) = R
         J = ID6+I
         W(J) = 1./R**2
         W(I) = (R-DLRBY2)/(R*DLRSQ)
         J = ID3+I
         W(J) = (R+DLRBY2)/(R*DLRSQ)
         J = ID2+I
         W(J) = -A1+ELMBDA
  110 CONTINUE
      GO TO (114,111,112,113,114,111),MBDCND
  111 W(ID2) = A1
      GO TO 114
  112 W(ID2) = A1
  113 W(ID3+1) = A1
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
C     ENTER BOUNDARY DATA FOR THETA-BOUNDARIES.
C
  124 A1 = 1./DLTHSQ
      L = ID5-MSTART+1
      LP = ID6-MSTART+1
      GO TO (134,125,125,127,127),NP
  125 DO 126 I=MSTART,MSTOP
         J = I+LP
         F(I,2) = F(I,2)-A1*W(J)*F(I,1)
  126 CONTINUE
      GO TO 129
  127 A1 = 2./DELTHT
      DO 128 I=MSTART,MSTOP
         J = I+LP
         F(I,1) = F(I,1)+A1*W(J)*BDC(I)
  128 CONTINUE
  129 A1 = 1./DLTHSQ
      GO TO (134,130,132,132,130),NP
  130 DO 131 I=MSTART,MSTOP
         J = I+LP
         F(I,N) = F(I,N)-A1*W(J)*F(I,NP1)
  131 CONTINUE
      GO TO 134
  132 A1 = 2./DELTHT
      DO 133 I=MSTART,MSTOP
         J = I+LP
         F(I,NP1) = F(I,NP1)-A1*W(J)*BDD(I)
  133 CONTINUE
  134 CONTINUE
C
C     ADJUST RIGHT SIDE OF EQUATION FOR UNKNOWN AT POLE WHEN HAVE
C     DERIVATIVE SPECIFIED BOUNDARY CONDITIONS.
C
      IF (MBDCND.GE.5 .AND. NBDCND.EQ.3)
     1    F(1,1) = F(1,1)-(BDD(2)-BDC(2))*4./(FLOAT(N)*DELTHT*DLRSQ)
C
C     ADJUST RIGHT SIDE OF SINGULAR PROBLEMS TO INSURE EXISTENCE OF A
C     SOLUTION.
C
      PERTRB = 0.
      IF (ELMBDA) 144,136,135
  135 IERROR = 11
      GO TO 144
  136 IF (NBDCND.NE.0 .AND. NBDCND.NE.3) GO TO 144
      S2 = 0.
      GO TO (144,144,137,144,144,138),MBDCND
  137 W(ID5+1) = .5*(W(ID5+2)-DLRBY2)
      S2 = .25*DELTAR
  138 A2 = 2.
      IF (NBDCND .EQ. 0) A2 = 1.
      J = ID5+MUNK
      W(J) = .5*(W(J-1)+DLRBY2)
      S = 0.
      DO 140 I=MSTART,MSTOP
         S1 = 0.
         IJ = NSTART+1
         K = NSTOP-1
         DO 139 J=IJ,K
            S1 = S1+F(I,J)
  139    CONTINUE
         J = I+L
         S = S+(A2*S1+F(I,NSTART)+F(I,NSTOP))*W(J)
  140 CONTINUE
      S2 = FLOAT(M)*A+DELTAR*(FLOAT((M-1)*(M+1))*.5+.25)+S2
      S1 = (2.+A2*FLOAT(NUNK-2))*S2
      IF (MBDCND .EQ. 3) GO TO 141
      S2 = FLOAT(N)*A2*DELTAR/8.
      S = S+F(1,1)*S2
      S1 = S1+S2
  141 CONTINUE
      PERTRB = S/S1
      DO 143 I=MSTART,MSTOP
         DO 142 J=NSTART,NSTOP
            F(I,J) = F(I,J)-PERTRB
  142    CONTINUE
  143 CONTINUE
  144 CONTINUE
C
C     MULTIPLY I-TH EQUATION THROUGH BY (R(I)*DELTHT)**2.
C
      DO 146 I=MSTART,MSTOP
         K = I-MSTART+1
         J = I+LP
         A1 = DLTHSQ/W(J)
         W(K) = A1*W(K)
         J = ID2+K
         W(J) = A1*W(J)
         J = ID3+K
         W(J) = A1*W(J)
         DO 145 J=NSTART,NSTOP
            F(I,J) = A1*F(I,J)
  145    CONTINUE
  146 CONTINUE
      W(1) = 0.
      W(ID4) = 0.
C
C     CALL GENBUN TO SOLVE THE SYSTEM OF EQUATIONS.
C
      CALL GENBUN (NBDCND,NUNK,1,MUNK,W(1),W(ID2+1),W(ID3+1),IDIMF,
     1             F(MSTART,NSTART),IERR1,W(ID4+1))
      IWSTOR = W(ID4+1)+3.*FLOAT(MUNK)
      GO TO (157,157,157,157,148,147),MBDCND
C
C     ADJUST THE SOLUTION AS NECESSARY FOR THE PROBLEMS WHERE A = 0.
C
  147 IF (ELMBDA .NE. 0.) GO TO 148
      YPOLE = 0.
      GO TO 155
  148 CONTINUE
      J = ID5+MUNK
      W(J) = W(ID2)/W(ID3)
      DO 149 IP=3,MUNK
         I = MUNK-IP+2
         J = ID5+I
         LP = ID2+I
         K = ID3+I
         W(J) = W(I)/(W(LP)-W(K)*W(J+1))
  149 CONTINUE
      W(ID5+1) = -.5*DLTHSQ/(W(ID2+1)-W(ID3+1)*W(ID5+2))
      DO 150 I=2,MUNK
         J = ID5+I
         W(J) = -W(J)*W(J-1)
  150 CONTINUE
      S = 0.
      DO 151 J=NSTART,NSTOP
         S = S+F(2,J)
  151 CONTINUE
      A2 = NUNK
      IF (NBDCND .EQ. 0) GO TO 152
      S = S-.5*(F(2,NSTART)+F(2,NSTOP))
      A2 = A2-1.
  152 YPOLE = (.25*DLRSQ*F(1,1)-S/A2)/(W(ID5+1)-1.+ELMBDA*DLRSQ*.25)
      DO 154 I=MSTART,MSTOP
         K = L+I
         DO 153 J=NSTART,NSTOP
            F(I,J) = F(I,J)+YPOLE*W(K)
  153    CONTINUE
  154 CONTINUE
  155 DO 156 J=1,NP1
         F(1,J) = YPOLE
  156 CONTINUE
  157 CONTINUE
      IF (NBDCND .NE. 0) GO TO 159
      DO 158 I=MSTART,MSTOP
         F(I,NP1) = F(I,1)
  158 CONTINUE
  159 CONTINUE
      W(1) = IWSTOR
      RETURN
      END
