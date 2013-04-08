      SUBROUTINE POIS3D (LPEROD,L,C1,MPEROD,M,C2,NPEROD,N,A,B,C,LDIMF,
     1                   MDIMF,F,IERROR,W)
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
C    * * * * * * * * *  PURPOSE    * * * * * * * * * * * * * * * * * *
C
C     SUBROUTINE POIS3D SOLVES THE LINEAR SYSTEM OF EQUATIONS
C
C       C1*(X(I-1,J,K)-2.*X(I,J,K)+X(I+1,J,K))
C     + C2*(X(I,J-1,K)-2.*X(I,J,K)+X(I,J+1,K))
C     + A(K)*X(I,J,K-1)+B(K)*X(I,J,K)+C(K)*X(I,J,K+1) = F(I,J,K)
C
C     FOR  I=1,2,...,L , J=1,2,...,M , AND K=1,2,...,N .
C
C     THE INDICES K-1 AND K+1 ARE EVALUATED MODULO N, I.E.
C     X(I,J,0) = X(I,J,N) AND X(I,J,N+1) = X(I,J,1). THE UNKNOWNS
C     X(0,J,K), X(L+1,J,K), X(I,0,K), AND X(I,M+1,K) ARE ASSUMED TO TAKE
C     ON CERTAIN PRESCRIBED VALUES DESCRIBED BELOW.
C
C    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C    * * * * * * * *    PARAMETER DESCRIPTION     * * * * * * * * * *
C
C
C            * * * * * *   ON INPUT    * * * * * *
C
C     LPEROD   INDICATES THE VALUES THAT X(0,J,K) AND X(L+1,J,K) ARE
C              ASSUMED TO HAVE.
C
C              = 0  IF X(0,J,K) = X(L,J,K) AND X(L+1,J,K) = X(1,J,K).
C              = 1  IF X(0,J,K) = X(L+1,J,K) = 0.
C              = 2  IF X(0,J,K) = 0  AND X(L+1,J,K) = X(L-1,J,K).
C              = 3  IF X(0,J,K) = X(2,J,K) AND X(L+1,J,K) = X(L-1,J,K).
C              = 4  IF X(0,J,K) = X(2,J,K) AND X(L+1,J,K) = 0.
C
C     L        THE NUMBER OF UNKNOWNS IN THE I-DIRECTION. L MUST BE AT
C              LEAST 3.
C
C     C1       THE REAL CONSTANT THAT APPEARS IN THE ABOVE EQUATION.
C
C     MPEROD   INDICATES THE VALUES THAT X(I,0,K) AND X(I,M+1,K) ARE
C              ASSUMED TO HAVE.
C
C              = 0  IF X(I,0,K) = X(I,M,K) AND X(I,M+1,K) = X(I,1,K).
C              = 1  IF X(I,0,K) = X(I,M+1,K) = 0.
C              = 2  IF X(I,0,K) = 0 AND X(I,M+1,K) = X(I,M-1,K).
C              = 3  IF X(I,0,K) = X(I,2,K) AND X(I,M+1,K) = X(I,M-1,K).
C              = 4  IF X(I,0,K) = X(I,2,K) AND X(I,M+1,K) = 0.
C
C     M        THE NUMBER OF UNKNOWNS IN THE J-DIRECTION. M MUST BE AT
C              LEAST 3.
C
C     C2       THE REAL CONSTANT WHICH APPEARS IN THE ABOVE EQUATION.
C
C     NPEROD   = 0  IF A(1) AND C(N) ARE NOT ZERO.
C              = 1  IF A(1) = C(N) = 0.
C
C     N        THE NUMBER OF UNKNOWNS IN THE K-DIRECTION. N MUST BE AT
C              LEAST 3.
C
C
C     A,B,C    ONE-DIMENSIONAL ARRAYS OF LENGTH N THAT SPECIFY THE
C              COEFFICIENTS IN THE LINEAR EQUATIONS GIVEN ABOVE.
C
C              IF NPEROD = 0 THE ARRAY ELEMENTS MUST NOT DEPEND UPON THE
C              INDEX K, BUT MUST BE CONSTANT.  SPECIFICALLY,THE
C              SUBROUTINE CHECKS THE FOLLOWING CONDITION
C
C                          A(K) = C(1)
C                          C(K) = C(1)
C                          B(K) = B(1)
C
C                  FOR K=1,2,...,N.
C
C     LDIMF    THE ROW (OR FIRST) DIMENSION OF THE THREE-DIMENSIONAL
C              ARRAY F AS IT APPEARS IN THE PROGRAM CALLING POIS3D.
C              THIS PARAMETER IS USED TO SPECIFY THE VARIABLE DIMENSION
C              OF F.  LDIMF MUST BE AT LEAST L.
C
C     MDIMF    THE COLUMN (OR SECOND) DIMENSION OF THE THREE-DIMENSIONAL
C              ARRAY F AS IT APPEARS IN THE PROGRAM CALLING POIS3D.
C              THIS PARAMETER IS USED TO SPECIFY THE VARIABLE DIMENSION
C              OF F.  MDIMF MUST BE AT LEAST M.
C
C     F        A THREE-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUES OF
C              THE RIGHT SIDE OF THE LINEAR SYSTEM OF EQUATIONS GIVEN
C              ABOVE.  F MUST BE DIMENSIONED AT LEAST L X M X N.
C
C     W        A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE
C              USER FOR WORK SPACE.  THE LENGTH OF W MUST BE AT LEAST
C              30 + L + M + 2*N + MAX(L,M,N) +
C              7*(INT((L+1)/2) + INT((M+1)/2)).
C
C
C            * * * * * *   ON OUTPUT   * * * * * *
C
C     F        CONTAINS THE SOLUTION X.
C
C     IERROR   AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.
C              EXCEPT FOR NUMBER ZERO, A SOLUTION IS NOT ATTEMPTED.
C              = 0  NO ERROR
C              = 1  IF LPEROD .LT. 0 OR .GT. 4
C              = 2  IF L .LT. 3
C              = 3  IF MPEROD .LT. 0 OR .GT. 4
C              = 4  IF M .LT. 3
C              = 5  IF NPEROD .LT. 0 OR .GT. 1
C              = 6  IF N .LT. 3
C              = 7  IF LDIMF .LT. L
C              = 8  IF MDIMF .LT. M
C              = 9  IF A(K) .NE. C(1) OR C(K) .NE. C(1) OR B(I) .NE.B(1)
C                      FOR SOME K=1,2,...,N.
C              = 10 IF NPEROD = 1 AND A(1) .NE. 0 OR C(N) .NE. 0
C
C              SINCE THIS IS THE ONLY MEANS OF INDICATING A POSSIBLY
C              INCORRECT CALL TO POIS3D, THE USER SHOULD TEST IERROR
C              AFTER THE CALL.
C
C
C    * * * * * * *   PROGRAM SPECIFICATIONS    * * * * * * * * * * * *
C
C     DIMENSION OF   A(N),B(N),C(N),F(LDIMF,MDIMF,N),
C     ARGUMENTS      W(SEE ARGUMENT LIST)
C
C     LATEST         DECEMBER 1, 1978
C     REVISION
C
C     SUBPROGRAMS    POIS3D,POS3D1,TRID,RFFTI,RFFTF,RFFTF1,RFFTB,
C     REQUIRED       RFFTB1,COSTI,COST,SINTI,SINT,COSQI,COSQF,COSQF1
C                    COSQB,COSQB1,SINQI,SINQF,SINQB,CFFTI,CFFTI1,
C                    CFFTB,CFFTB1,PASSB2,PASSB3,PASSB4,PASSB,CFFTF,
C                    CFFTF1,PASSF1,PASSF2,PASSF3,PASSF4,PASSF,PIMACH,
C
C     SPECIAL        NONE
C     CONDITIONS
C
C     COMMON         VALUE
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
C     HISTORY        WRITTEN BY ROLAND SWEET AT NCAR IN JULY,1977
C
C     ALGORITHM      THIS SUBROUTINE SOLVES THREE-DIMENSIONAL BLOCK
C                    TRIDIAGONAL LINEAR SYSTEMS ARISING FROM FINITE
C                    DIFFERENCE APPROXIMATIONS TO THREE-DIMENSIONAL
C                    POISSON EQUATIONS USING THE FOURIER TRANSFORM
C                    PACKAGE SCLRFFTPAK WRITTEN BY PAUL SWARZTRAUBER.
C
C     SPACE          6561(DECIMAL) = 14641(OCTAL) LOCATIONS ON THE
C     REQUIRED       NCAR CONTROL DATA 7600
C
C     TIMING AND        THE EXECUTION TIME T ON THE NCAR CONTROL DATA
C     ACCURACY       7600 FOR SUBROUTINE POIS3D IS ROUGHLY PROPORTIONAL
C                    TO L*M*N*(LOG2(L)+LOG2(M)+5), BUT ALSO DEPENDS ON
C                    INPUT PARAMETERS LPEROD AND MPEROD.  SOME TYPICAL
C                    VALUES ARE LISTED IN THE TABLE BELOW WHEN NPEROD=0.
C                       TO MEASURE THE ACCURACY OF THE ALGORITHM A
C                    UNIFORM RANDOM NUMBER GENERATOR WAS USED TO CREATE
C                    A SOLUTION ARRAY X FOR THE SYSTEM GIVEN IN THE
C                    'PURPOSE' WITH
C
C                       A(K) = C(K) = -0.5*B(K) = 1,       K=1,2,...,N
C
C                    AND, WHEN NPEROD = 1
C
C                       A(1) = C(N) = 0
C                       A(N) = C(1) = 2.
C
C                    THE SOLUTION X WAS SUBSTITUTED INTO THE GIVEN SYS-
C                    TEM AND, USING DOUBLE PRECISION, A RIGHT SIDE Y WAS
C                    COMPUTED.  USING THIS ARRAY Y SUBROUTINE POIS WAS
C                    CALLED TO PRODUCE AN APPROXIMATE SOLUTION Z.  THEN
C                    THE RELATIVE ERROR, DEFINED AS
C
C                    E = MAX(ABS(Z(I,J,K)-X(I,J,K)))/MAX(ABS(X(I,J,K)))
C
C                    WHERE THE TWO MAXIMA ARE TAKEN OVER I=1,2,...,L,
C                    J=1,2,...,M AND K=1,2,...,N, WAS COMPUTED.  THE
C                    VALUE OF E IS GIVEN IN THE TABLE BELOW FOR SOME
C                    TYPICAL VALUES OF L,M AND N.
C
C
C                       L(=M=N)   LPEROD    MPEROD    T(MSECS)    E
C                       ------    ------    ------    --------  ------
C
C                         16        0         0         272     1.E-13
C                         15        1         1         287     4.E-13
C                         17        3         3         338     2.E-13
C                         32        0         0        1755     2.E-13
C                         31        1         1        1894     2.E-12
C                         33        3         3        2042     7.E-13
C
C
C     PORTABILITY    AMERICAN NATIONAL STANDARDS INSTITUTE FORTRAN.
C                    THE MACHINE DEPENDENT CONSTANT PI IS DEFINED IN
C                    FUNCTION PIMACH.
C
C     REQUIRED       COS,SIN,ATAN
C     RESIDENT
C     ROUTINES
C
C     REFERENCE      NONE
C
C    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,
     1                F(LDIMF,MDIMF,1)       ,W(1)       ,SAVE(6)
      LP = LPEROD+1
      MP = MPEROD+1
      NP = NPEROD+1
C
C     CHECK FOR INVALID INPUT.
C
      IERROR = 0
      IF (LP.LT.1 .OR. LP.GT.5) IERROR = 1
      IF (L .LT. 3) IERROR = 2
      IF (MP.LT.1 .OR. MP.GT.5) IERROR = 3
      IF (M .LT. 3) IERROR = 4
      IF (NP.LT.1 .OR. NP.GT.2) IERROR = 5
      IF (N .LT. 3) IERROR = 6
      IF (LDIMF .LT. L) IERROR = 7
      IF (MDIMF .LT. M) IERROR = 8
      IF (NP .NE. 1) GO TO 103
      DO 101 K=1,N
         IF (A(K) .NE. C(1)) GO TO 102
         IF (C(K) .NE. C(1)) GO TO 102
         IF (B(K) .NE. B(1)) GO TO 102
  101 CONTINUE
      GO TO 104
  102 IERROR = 9
  103 IF (NPEROD.EQ.1 .AND. (A(1).NE.0. .OR. C(N).NE.0.)) IERROR = 10
  104 IF (IERROR .NE. 0) GO TO 122
      IWYRT = L+1
      IWT = IWYRT+M
      IWD = IWT+MAX0(L,M,N)+1
      IWBB = IWD+N
      IWX = IWBB+N
      IWY = IWX+7*((L+1)/2)+15
      GO TO (105,114),NP
C
C     REORDER UNKNOWNS WHEN NPEROD = 0.
C
  105 NH = (N+1)/2
      NHM1 = NH-1
      NODD = 1
      IF (2*NH .EQ. N) NODD = 2
      DO 111 I=1,L
         DO 110 J=1,M
            DO 106 K=1,NHM1
               NHPK = NH+K
               NHMK = NH-K
               W(K) = F(I,J,NHMK)-F(I,J,NHPK)
               W(NHPK) = F(I,J,NHMK)+F(I,J,NHPK)
  106       CONTINUE
            W(NH) = 2.*F(I,J,NH)
            GO TO (108,107),NODD
  107       W(N) = 2.*F(I,J,N)
  108       DO 109 K=1,N
               F(I,J,K) = W(K)
  109       CONTINUE
  110    CONTINUE
  111 CONTINUE
      SAVE(1) = C(NHM1)
      SAVE(2) = A(NH)
      SAVE(3) = C(NH)
      SAVE(4) = B(NHM1)
      SAVE(5) = B(N)
      SAVE(6) = A(N)
      C(NHM1) = 0.
      A(NH) = 0.
      C(NH) = 2.*C(NH)
      GO TO (112,113),NODD
  112 B(NHM1) = B(NHM1)-A(NH-1)
      B(N) = B(N)+A(N)
      GO TO 114
  113 A(N) = C(NH)
  114 CONTINUE
      CALL POS3D1 (LP,L,MP,M,N,A,B,C,LDIMF,MDIMF,F,W,W(IWYRT),W(IWT),
     1             W(IWD),W(IWX),W(IWY),C1,C2,W(IWBB))
      GO TO (115,122),NP
  115 DO 121 I=1,L
         DO 120 J=1,M
            DO 116 K=1,NHM1
               NHMK = NH-K
               NHPK = NH+K
               W(NHMK) = .5*(F(I,J,NHPK)+F(I,J,K))
               W(NHPK) = .5*(F(I,J,NHPK)-F(I,J,K))
  116       CONTINUE
            W(NH) = .5*F(I,J,NH)
            GO TO (118,117),NODD
  117       W(N) = .5*F(I,J,N)
  118       DO 119 K=1,N
               F(I,J,K) = W(K)
  119       CONTINUE
  120    CONTINUE
  121 CONTINUE
      C(NHM1) = SAVE(1)
      A(NH) = SAVE(2)
      C(NH) = SAVE(3)
      B(NHM1) = SAVE(4)
      B(N) = SAVE(5)
      A(N) = SAVE(6)
  122 CONTINUE
      RETURN
      END
