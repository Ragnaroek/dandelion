      SUBROUTINE GENBUN (NPEROD,N,MPEROD,M,A,B,C,IDIMY,Y,IERROR,W)
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
C     SUBROUTINE GENBUN SOLVES THE LINEAR SYSTEM OF EQUATIONS
C
C          A(I)*X(I-1,J) + B(I)*X(I,J) + C(I)*X(I+1,J)
C
C          + X(I,J-1) - 2.*X(I,J) + X(I,J+1) = Y(I,J)
C
C               FOR I = 1,2,...,M  AND  J = 1,2,...,N.
C
C     THE INDICES I+1 AND I-1 ARE EVALUATED MODULO M, I.E.,
C     X(0,J) = X(M,J) AND X(M+1,J) = X(1,J), AND X(I,0) MAY BE EQUAL TO
C     0, X(I,2), OR X(I,N) AND X(I,N+1) MAY BE EQUAL TO 0, X(I,N-1), OR
C     X(I,1) DEPENDING ON AN INPUT PARAMETER.
C
C
C     * * * * * * * *    PARAMETER DESCRIPTION     * * * * * * * * * *
C
C             * * * * * *   ON INPUT    * * * * * *
C
C     NPEROD
C       INDICATES THE VALUES THAT X(I,0) AND X(I,N+1) ARE ASSUMED TO
C       HAVE.
C
C       = 0  IF X(I,0) = X(I,N) AND X(I,N+1) = X(I,1).
C       = 1  IF X(I,0) = X(I,N+1) = 0  .
C       = 2  IF X(I,0) = 0 AND X(I,N+1) = X(I,N-1).
C       = 3  IF X(I,0) = X(I,2) AND X(I,N+1) = X(I,N-1).
C       = 4  IF X(I,0) = X(I,2) AND X(I,N+1) = 0.
C
C     N
C       THE NUMBER OF UNKNOWNS IN THE J-DIRECTION.  N MUST BE GREATER
C       THAN 2.
C
C     MPEROD
C       = 0 IF A(1) AND C(M) ARE NOT ZERO
C       = 1 IF A(1) = C(M) = 0
C
C     M
C       THE NUMBER OF UNKNOWNS IN THE I-DIRECTION.  M MUST BE GREATER
C       THAN 2.
C
C     A,B,C
C       ONE-DIMENSIONAL ARRAYS OF LENGTH M THAT SPECIFY THE
C       COEFFICIENTS IN THE LINEAR EQUATIONS GIVEN ABOVE.  IF MPEROD = 0
C       THE ARRAY ELEMENTS MUST NOT DEPEND UPON THE INDEX I, BUT MUST BE
C       CONSTANT.  SPECIFICALLY, THE SUBROUTINE CHECKS THE FOLLOWING
C       CONDITION
C
C             A(I) = C(1)
C             C(I) = C(1)
C             B(I) = B(1)
C
C       FOR I=1,2,...,M.
C
C     IDIMY
C       THE ROW (OR FIRST) DIMENSION OF THE TWO-DIMENSIONAL ARRAY Y AS
C       IT APPEARS IN THE PROGRAM CALLING GENBUN.  THIS PARAMETER IS
C       USED TO SPECIFY THE VARIABLE DIMENSION OF Y.  IDIMY MUST BE AT
C       LEAST M.
C
C     Y
C       A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUES OF THE RIGHT
C       SIDE OF THE LINEAR SYSTEM OF EQUATIONS GIVEN ABOVE.  Y MUST BE
C       DIMENSIONED AT LEAST M*N.
C
C     W
C       A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE USER FOR
C       WORK SPACE.  W MAY REQUIRE UP TO 4*N + (10 + INT(LOG2(N)))*M
C       LOCATIONS.  THE ACTUAL NUMBER OF LOCATIONS USED IS COMPUTED BY
C       GENBUN AND IS RETURNED IN LOCATION W(1).
C
C
C             * * * * * *   ON OUTPUT     * * * * * *
C
C     Y
C       CONTAINS THE SOLUTION X.
C
C     IERROR
C       AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS  EXCEPT
C       FOR NUMBER ZERO, A SOLUTION IS NOT ATTEMPTED.
C
C       = 0  NO ERROR.
C       = 1  M .LE. 2  .
C       = 2  N .LE. 2
C       = 3  IDIMY .LT. M
C       = 4  NPEROD .LT. 0 OR NPEROD .GT. 4
C       = 5  MPEROD .LT. 0 OR MPEROD .GT. 1
C       = 6  A(I) .NE. C(1) OR C(I) .NE. C(1) OR B(I) .NE. B(1) FOR
C            SOME I=1,2,...,M.
C       = 7  A(1) .NE. 0 OR C(M) .NE. 0 AND MPEROD = 1
C
C     W
C       W(1) CONTAINS THE REQUIRED LENGTH OF W.
C
C     * * * * * * *   PROGRAM SPECIFICATIONS    * * * * * * * * * * * *
C
C     DIMENSION OF   A(M),B(M),C(M),Y(IDIMY,N),W(SEE PARAMETER LIST)
C     ARGUMENTS
C
C     LATEST         JUNE 1, 1976
C     REVISION
C
C     SUBPROGRAMS    GENBUN,POISD2,POISN2,POISP2,COSGEN,MERGE,TRIX,TRI3,
C     REQUIRED       PIMACH
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
C     HISTORY        STANDARDIZED APRIL 1, 1973
C                    REVISED AUGUST 20,1973
C                    REVISED JANUARY 1, 1976
C
C     ALGORITHM      THE LINEAR SYSTEM IS SOLVED BY A CYCLIC REDUCTION
C                    ALGORITHM DESCRIBED IN THE REFERENCE.
C
C     SPACE          4944(DECIMAL) = 11520(OCTAL) LOCATIONS ON THE NCAR
C     REQUIRED       CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE GENBUN IS ROUGHLY PROPORTIONAL
C                    TO M*N*LOG2(N), BUT ALSO DEPENDS ON THE INPUT
C                    PARAMETER NPEROD.  SOME TYPICAL VALUES ARE LISTED
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
C                       A(M) = C(1) = 2.
C
C                    THE SOLUTION X WAS SUBSTITUTED INTO THE GIVEN SYS-
C                    TEM AND, USING DOUBLE PRECISION, A RIGHT SIDE Y WAS
C                    COMPUTED.  USING THIS ARRAY Y SUBROUTINE GENBUN WAS
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
C                         31        0         0          36     6.E-14
C                         31        1         1          21     4.E-13
C                         31        1         3          41     3.E-13
C                         32        0         0          29     9.E-14
C                         32        1         1          32     3.E-13
C                         32        1         3          48     1.E-13
C                         33        0         0          36     9.E-14
C                         33        1         1          30     4.E-13
C                         33        1         3          34     1.E-13
C                         63        0         0         150     1.E-13
C                         63        1         1          91     1.E-12
C                         63        1         3         173     2.E-13
C                         64        0         0         122     1.E-13
C                         64        1         1         128     1.E-12
C                         64        1         3         199     6.E-13
C                         65        0         0         143     2.E-13
C                         65        1         1         120     1.E-12
C                         65        1         3         138     4.E-13
C
C     PORTABILITY    AMERICAN NATIONAL STANDARDS INSTITUE FORTRAN.
C                    ALL MACHINE DEPENDENT CONSTANTS ARE LOCATED IN THE
C                    FUNCTION PIMACH.
C
C     REQUIRED       COS
C     RESIDENT
C     ROUTINES
C
C     REFERENCE      SWEET, R., 'A CYCLIC REDUCTION ALGORITHM FOR
C                    SOLVING BLOCK TRIDIAGONAL SYSTEMS OF ARBITRARY
C                    DIMENSIONS,' SIAM J. ON NUMER. ANAL.,
C                    14(SEPT., 1977), PP. 706-720.
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
      DIMENSION       Y(IDIMY,1)
      DIMENSION       W(1)       ,B(1)       ,A(1)       ,C(1)
      IERROR = 0
      IF (M .LE. 2) IERROR = 1
      IF (N .LE. 2) IERROR = 2
      IF (IDIMY .LT. M) IERROR = 3
      IF (NPEROD.LT.0 .OR. NPEROD.GT.4) IERROR = 4
      IF (MPEROD.LT.0 .OR. MPEROD.GT.1) IERROR = 5
      IF (MPEROD .EQ. 1) GO TO 102
      DO 101 I=2,M
         IF (A(I) .NE. C(1)) GO TO 103
         IF (C(I) .NE. C(1)) GO TO 103
         IF (B(I) .NE. B(1)) GO TO 103
  101 CONTINUE
      GO TO 104
  102 IF (A(1).NE.0. .OR. C(M).NE.0.) IERROR = 7
      GO TO 104
  103 IERROR = 6
  104 IF (IERROR .NE. 0) RETURN
      MP1 = M+1
      IWBA = MP1
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
      MP = MPEROD+1
      NP = NPEROD+1
      GO TO (114,107),MP
  107 GO TO (108,109,110,111,123),NP
  108 CALL POISP2 (M,N,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
      GO TO 112
  109 CALL POISD2 (M,N,1,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWW1),
     1             W(IWD),W(IWTCOS),W(IWP))
      GO TO 112
  110 CALL POISN2 (M,N,1,2,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
      GO TO 112
  111 CALL POISN2 (M,N,1,1,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
  112 IPSTOR = W(IWW1)
      IREV = 2
      IF (NPEROD .EQ. 4) GO TO 124
  113 GO TO (127,133),MP
  114 CONTINUE
C
C     REORDER UNKNOWNS WHEN MP =0
C
      MH = (M+1)/2
      MHM1 = MH-1
      MODD = 1
      IF (MH*2 .EQ. M) MODD = 2
      DO 119 J=1,N
         DO 115 I=1,MHM1
            MHPI = MH+I
            MHMI = MH-I
            W(I) = Y(MHMI,J)-Y(MHPI,J)
            W(MHPI) = Y(MHMI,J)+Y(MHPI,J)
  115    CONTINUE
         W(MH) = 2.*Y(MH,J)
         GO TO (117,116),MODD
  116    W(M) = 2.*Y(M,J)
  117    CONTINUE
         DO 118 I=1,M
            Y(I,J) = W(I)
  118    CONTINUE
  119 CONTINUE
      K = IWBC+MHM1-1
      I = IWBA+MHM1
      W(K) = 0.
      W(I) = 0.
      W(K+1) = 2.*W(K+1)
      GO TO (120,121),MODD
  120 CONTINUE
      K = IWBB+MHM1-1
      W(K) = W(K)-W(I-1)
      W(IWBC-1) = W(IWBC-1)+W(IWBB-1)
      GO TO 122
  121 W(IWBB-1) = W(K+1)
  122 CONTINUE
      GO TO 107
C
C     REVERSE COLUMNS WHEN NPEROD = 4.
C
  123 IREV = 1
      NBY2 = N/2
  124 DO 126 J=1,NBY2
         MSKIP = N+1-J
         DO 125 I=1,M
            A1 = Y(I,J)
            Y(I,J) = Y(I,MSKIP)
            Y(I,MSKIP) = A1
  125    CONTINUE
  126 CONTINUE
      GO TO (110,113),IREV
  127 CONTINUE
      DO 132 J=1,N
         DO 128 I=1,MHM1
            MHMI = MH-I
            MHPI = MH+I
            W(MHMI) = .5*(Y(MHPI,J)+Y(I,J))
            W(MHPI) = .5*(Y(MHPI,J)-Y(I,J))
  128    CONTINUE
         W(MH) = .5*Y(MH,J)
         GO TO (130,129),MODD
  129    W(M) = .5*Y(M,J)
  130    CONTINUE
         DO 131 I=1,M
            Y(I,J) = W(I)
  131    CONTINUE
  132 CONTINUE
  133 CONTINUE
C
C     RETURN STORAGE REQUIREMENTS FOR W ARRAY.
C
      W(1) = IPSTOR+IWP-1
      RETURN
      END
