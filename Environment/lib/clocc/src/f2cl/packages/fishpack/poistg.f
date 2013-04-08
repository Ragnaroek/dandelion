      SUBROUTINE POISTG (NPEROD,N,MPEROD,M,A,B,C,IDIMY,Y,IERROR,W)
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
C     SUBROUTINE POISTG SOLVES THE LINEAR SYSTEM OF EQUATIONS
C
C       A(I)*X(I-1,J) + B(I)*X(I,J) + C(I)*X(I+1,J)
C       + X(I,J-1) - 2.*X(I,J) + X(I,J+1) = Y(I,J)
C
C       FOR I=1,2,...,M AND J=1,2,...,N.
C
C     THE INDICES I+1 AND I-1 ARE EVALUATED MODULO M, I.E.
C     X(0,J) = X(M,J) AND X(M+1,J) = X(1,J), AND X(I,0) MAY BE EQUAL TO
C     X(I,1) OR -X(I,1) AND X(I,N+1) MAY BE EQUAL TO X(I,N) OR -X(I,N)
C     DEPENDING ON AN INPUT PARAMETER.
C
C
C     * * * * * * * *    PARAMETER DESCRIPTION     * * * * * * * * * *
C
C             * * * * * *   ON INPUT    * * * * * *
C
C   NPEROD
C     INDICATES THE VALUES WHICH X(I,0) AND X(I,N+1) ARE ASSUMED
C     TO HAVE.
C     = 1 IF X(I,0) = -X(I,1) AND X(I,N+1) = -X(I,N)
C     = 2 IF X(I,0) = -X(I,1) AND X(I,N+1) =  X(I,N)
C     = 3 IF X(I,0) =  X(I,1) AND X(I,N+1) =  X(I,N)
C     = 4 IF X(I,0) =  X(I,1) AND X(I,N+1) = -X(I,N)
C
C   N
C     THE NUMBER OF UNKNOWNS IN THE J-DIRECTION.  N MUST
C     BE GREATER THAN 2.
C
C   MPEROD
C     = 0 IF A(1) AND C(M) ARE NOT ZERO
C     = 1 IF A(1) = C(M) = 0
C
C   M
C     THE NUMBER OF UNKNOWNS IN THE I-DIRECTION.  M MUST
C     BE GREATER THAN 2.
C
C   A,B,C
C     ONE-DIMENSIONAL ARRAYS OF LENGTH M THAT SPECIFY THE COEFFICIENTS
C     IN THE LINEAR EQUATIONS GIVEN ABOVE.  IF MPEROD = 0 THE ARRAY
C     ELEMENTS MUST NOT DEPEND ON THE INDEX I, BUT MUST BE CONSTANT.
C     SPECIFICALLY, THE SUBROUTINE CHECKS THE FOLLOWING CONDITION
C
C           A(I) = C(1)
C           B(I) = B(1)
C           C(I) = C(1)
C
C     FOR I = 1, 2, ..., M.
C
C   IDIMY
C     THE ROW (OR FIRST) DIMENSION OF THE TWO-DIMENSIONAL ARRAY Y AS
C     IT APPEARS IN THE PROGRAM CALLING POISTG.  THIS PARAMETER IS
C     USED TO SPECIFY THE VARIABLE DIMENSION OF Y.  IDIMY MUST BE AT
C     LEAST M.
C
C   Y
C     A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUES OF THE
C     RIGHT SIDE OF THE LINEAR SYSTEM OF EQUATIONS GIVEN ABOVE.
C     Y MUST BE DIMENSIONED AT LEAST M X N.
C
C   W
C     A ONE-DIMENSIONAL WORK ARRAY THAT MUST BE PROVIDED BY THE USER
C     FOR WORK SPACE.  W MAY REQUIRE UP TO 9M + 4N + M(INT(LOG2(N)))
C     LOCATIONS.  THE ACTUAL NUMBER OF LOCATIONS USED IS COMPUTED BY
C     POISTG AND RETURNED IN LOCATION W(1).
C
C
C             * * * * * *   ON OUTPUT     * * * * * *
C
C   Y
C     CONTAINS THE SOLUTION X.
C
C   IERROR
C     AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.  EXCEPT
C     FOR NUMBER ZERO, A SOLUTION IS NOT ATTEMPTED.
C     = 0  NO ERROR
C     = 1  IF M .LE. 2
C     = 2  IF N .LE. 2
C     = 3  IDIMY .LT. M
C     = 4  IF NPEROD .LT. 1 OR NPEROD .GT. 4
C     = 5  IF MPEROD .LT. 0 OR MPEROD .GT. 1
C     = 6  IF MPEROD = 0 AND
C          A(I) .NE. C(1) OR B(I) .NE. B(1) OR C(I) .NE. C(1)
C          FOR SOME I = 1, 2, ..., M.
C       = 7 IF MPEROD .EQ. 1 .AND. (A(1).NE.0 .OR. C(M).NE.0)
C
C   W
C     W(1) CONTAINS THE REQUIRED LENGTH OF W.
C
C
C     * * * * * * *   PROGRAM SPECIFICATIONS    * * * * * * * * * * * *
C
C     DIMENSION OF   A(M),B(M),C(M),Y(IDIMY,N),
C     ARGUMENTS      W(SEE ARGUMENT LIST)
C
C     LATEST         JUNE 1, 1977
C     REVISION
C
C     SUBPROGRAMS    POISTG,POSTG2,COSGEN,MERGE,TRIX,TRI3,PIMACH
C     REQUIRED
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
C     HISTORY        WRITTEN BY ROLAND SWEET IN 1973
C                    REVISED BY ROLAND SWEET IN 1977
C
C     ALGORITHM      THIS SUBROUTINE IS AN IMPLEMENTATION OF THE
C                    ALGORITHM PRESENTED IN THE REFERENCE.
C
C     SPACE          3297(DECIMAL) = 6341(OCTAL) LOCATIONS ON THE
C     REQUIRED       NCAR CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE POISTG IS ROUGHLY PROPORTIONAL
C                    TO M*N*LOG2(N).  SOME TYPICAL VALUES ARE LISTED
C                    IN THE TABLE BELOW.  MORE COMPREHENSIVE TIMING
C                    CHARTS MAY BE FOUND IN THE REFERENCE.
C                       TO MEASURE THE ACCURACY OF THE ALGORITHM A
C                    UNIFORM RANDOM NUMBER GENERATOR WAS USED TO CREATE
C                    A SOLUTION ARRAY X FOR THE SYSTEM GIVEN IN THE
C                    'PURPOSE' WITH
C
C                       A(I) = C(I) = -0.5*B(I) = 1,       I=1,2,...,M
C
C                    AND, WHEN MPEROD = 1
C
C                       A(1) = C(M) = 0
C                       B(1) = B(M) =-1.
C
C                    THE SOLUTION X WAS SUBSTITUTED INTO THE GIVEN SYS-
C                    TEM AND, USING DOUBLE PRECISION, A RIGHT SIDE Y WAS
C                    COMPUTED.  USING THIS ARRAY Y SUBROUTINE POISTG WAS
C                    CALLED TO PRODUCE AN APPROXIMATE SOLUTION Z.  THEN
C                    THE RELATIVE ERROR, DEFINED AS
C
C                       E = MAX(ABS(Z(I,J)-X(I,J)))/MAX(ABS(X(I,J)))
C
C                    WHERE THE TWO MAXIMA ARE TAKEN OVER ALL I=1,2,...,M
C                    AND J=1,2,...,N, WAS COMPUTED.  THE VALUE OF E IS
C                    GIVEN IN THE TABLE BELOW FOR SOME TYPICAL VALUES OF
C                    M AND N.
C
C
C                       M (=N)    MPEROD    NPEROD    T(MSECS)    E
C                       ------    ------    ------    --------  ------
C
C                         31        0-1       1-4        45     9.E-13
C                         31        1         1          21     4.E-13
C                         31        1         3          41     3.E-13
C                         32        0-1       1-4        51     3.E-12
C                         32        1         1          32     3.E-13
C                         32        1         3          48     1.E-13
C                         33        0-1       1-4        42     1.E-12
C                         33        1         1          30     4.E-13
C                         33        1         3          34     1.E-13
C                         63        0-1       1-4       186     3.E-12
C                         63        1         1          91     1.E-12
C                         63        1         3         173     2.E-13
C                         64        0-1       1-4       209     4.E-12
C                         64        1         1         128     1.E-12
C                         64        1         3         199     6.E-13
C                         65        0-1       1-4       143     2.E-13
C                         65        1         1         160     1.E-11
C                         65        1         3         138     4.E-13
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
      DIMENSION       Y(IDIMY,1)
      DIMENSION       W(1)       ,B(1)       ,A(1)       ,C(1)
      IERROR = 0
      IF (M .LE. 2) IERROR = 1
      IF (N .LE. 2) IERROR = 2
      IF (IDIMY .LT. M) IERROR = 3
      IF (NPEROD.LT.1 .OR. NPEROD.GT.4) IERROR = 4
      IF (MPEROD.LT.0 .OR. MPEROD.GT.1) IERROR = 5
      IF (MPEROD .EQ. 1) GO TO 103
      DO 101 I=1,M
         IF (A(I) .NE. C(1)) GO TO 102
         IF (C(I) .NE. C(1)) GO TO 102
         IF (B(I) .NE. B(1)) GO TO 102
  101 CONTINUE
      GO TO 104
  102 IERROR = 6
      RETURN
  103 IF (A(1).NE.0. .OR. C(M).NE.0.) IERROR = 7
  104 IF (IERROR .NE. 0) RETURN
      IWBA = M+1
      IWBB = IWBA+M
      IWBC = IWBB+M
      IWB2 = IWBC+M
      IWB3 = IWB2+M
      IWW1 = IWB3+M
      IWW2 = IWW1+M
      IWW3 = IWW2+M
      IWD = IWW3+M
      IWTCOS = IWD+M
      IWP = IWTCOS+4*N
      DO 106 I=1,M
         K = IWBA+I-1
         W(K) = -A(I)
         K = IWBC+I-1
         W(K) = -C(I)
         K = IWBB+I-1
         W(K) = 2.-B(I)
         DO 105 J=1,N
            Y(I,J) = -Y(I,J)
  105    CONTINUE
  106 CONTINUE
      NP = NPEROD
      MP = MPEROD+1
      GO TO (110,107),MP
  107 CONTINUE
      GO TO (108,108,108,119),NPEROD
  108 CONTINUE
      CALL POSTG2 (NP,N,M,W(IWBA),W(IWBB),W(IWBC),IDIMY,Y,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
      IPSTOR = W(IWW1)
      IREV = 2
      IF (NPEROD .EQ. 4) GO TO 120
  109 CONTINUE
      GO TO (123,129),MP
  110 CONTINUE
C
C     REORDER UNKNOWNS WHEN MP =0
C
      MH = (M+1)/2
      MHM1 = MH-1
      MODD = 1
      IF (MH*2 .EQ. M) MODD = 2
      DO 115 J=1,N
         DO 111 I=1,MHM1
            MHPI = MH+I
            MHMI = MH-I
            W(I) = Y(MHMI,J)-Y(MHPI,J)
            W(MHPI) = Y(MHMI,J)+Y(MHPI,J)
  111    CONTINUE
         W(MH) = 2.*Y(MH,J)
         GO TO (113,112),MODD
  112    W(M) = 2.*Y(M,J)
  113    CONTINUE
         DO 114 I=1,M
            Y(I,J) = W(I)
  114    CONTINUE
  115 CONTINUE
      K = IWBC+MHM1-1
      I = IWBA+MHM1
      W(K) = 0.
      W(I) = 0.
      W(K+1) = 2.*W(K+1)
      GO TO (116,117),MODD
  116 CONTINUE
      K = IWBB+MHM1-1
      W(K) = W(K)-W(I-1)
      W(IWBC-1) = W(IWBC-1)+W(IWBB-1)
      GO TO 118
  117 W(IWBB-1) = W(K+1)
  118 CONTINUE
      GO TO 107
  119 CONTINUE
C
C     REVERSE COLUMNS WHEN NPEROD = 4.
C
      IREV = 1
      NBY2 = N/2
      NP = 2
  120 DO 122 J=1,NBY2
         MSKIP = N+1-J
         DO 121 I=1,M
            A1 = Y(I,J)
            Y(I,J) = Y(I,MSKIP)
            Y(I,MSKIP) = A1
  121    CONTINUE
  122 CONTINUE
      GO TO (108,109),IREV
  123 CONTINUE
      DO 128 J=1,N
         DO 124 I=1,MHM1
            MHMI = MH-I
            MHPI = MH+I
            W(MHMI) = .5*(Y(MHPI,J)+Y(I,J))
            W(MHPI) = .5*(Y(MHPI,J)-Y(I,J))
  124    CONTINUE
         W(MH) = .5*Y(MH,J)
         GO TO (126,125),MODD
  125    W(M) = .5*Y(M,J)
  126    CONTINUE
         DO 127 I=1,M
            Y(I,J) = W(I)
  127    CONTINUE
  128 CONTINUE
  129 CONTINUE
C
C     RETURN STORAGE REQUIREMENTS FOR W ARRAY.
C
      W(1) = IPSTOR+IWP-1
      RETURN
      END
