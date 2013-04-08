      SUBROUTINE BLKTRI (IFLG,NP,N,AN,BN,CN,MP,M,AM,BM,CM,IDIMY,Y,
     1                   IERROR,W)
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
C     SUBROUTINE BLKTRI SOLVES A SYSTEM OF LINEAR EQUATIONS OF THE FORM
C
C          AN(J)*X(I,J-1) + AM(I)*X(I-1,J) + (BN(J)+BM(I))*X(I,J)
C
C          + CN(J)*X(I,J+1) + CM(I)*X(I+1,J) = Y(I,J)
C
C               FOR I = 1,2,...,M  AND  J = 1,2,...,N.
C
C     I+1 AND I-1 ARE EVALUATED MODULO M AND J+1 AND J-1 MODULO N, I.E.,
C
C          X(I,0) = X(I,N),  X(I,N+1) = X(I,1),
C          X(0,J) = X(M,J),  X(M+1,J) = X(1,J).
C
C     THESE EQUATIONS USUALLY RESULT FROM THE DISCRETIZATION OF
C     SEPARABLE ELLIPTIC EQUATIONS.  BOUNDARY CONDITIONS MAY BE
C     DIRICHLET, NEUMANN, OR PERIODIC.
C
C
C     * * * * * * * * * *     ON INPUT     * * * * * * * * * *
C
C     IFLG
C       = 0  INITIALIZATION ONLY.  CERTAIN QUANTITIES THAT DEPEND ON NP,
C            N, AN, BN, AND CN ARE COMPUTED AND STORED IN THE WORK
C            ARRAY  W.
C       = 1  THE QUANTITIES THAT WERE COMPUTED IN THE INITIALIZATION ARE
C            USED TO OBTAIN THE SOLUTION X(I,J).
C
C       NOTE   A CALL WITH IFLG=0 TAKES APPROXIMATELY ONE HALF THE TIME
C              TIME AS A CALL WITH IFLG = 1  .  HOWEVER, THE
C              INITIALIZATION DOES NOT HAVE TO BE REPEATED UNLESS NP, N,
C              AN, BN, OR CN CHANGE.
C
C     NP
C       = 0  IF AN(1) AND CN(N) ARE NOT ZERO, WHICH CORRESPONDS TO
C            PERIODIC BOUNARY CONDITIONS.
C       = 1  IF AN(1) AND CN(N) ARE ZERO.
C
C     N
C       THE NUMBER OF UNKNOWNS IN THE J-DIRECTION. N MUST BE GREATER
C       THAN 4. THE OPERATION COUNT IS PROPORTIONAL TO MNLOG2(N), HENCE
C       N SHOULD BE SELECTED LESS THAN OR EQUAL TO M.
C
C     AN,BN,CN
C       ONE-DIMENSIONAL ARRAYS OF LENGTH N THAT SPECIFY THE COEFFICIENTS
C       IN THE LINEAR EQUATIONS GIVEN ABOVE.
C
C     MP
C       = 0  IF AM(1) AND CM(M) ARE NOT ZERO, WHICH CORRESPONDS TO
C            PERIODIC BOUNDARY CONDITIONS.
C       = 1  IF AM(1) = CM(M) = 0  .
C
C     M
C       THE NUMBER OF UNKNOWNS IN THE I-DIRECTION. M MUST BE GREATER
C       THAN 4.
C
C     AM,BM,CM
C       ONE-DIMENSIONAL ARRAYS OF LENGTH M THAT SPECIFY THE COEFFICIENTS
C       IN THE LINEAR EQUATIONS GIVEN ABOVE.
C
C     IDIMY
C       THE ROW (OR FIRST) DIMENSION OF THE TWO-DIMENSIONAL ARRAY Y AS
C       IT APPEARS IN THE PROGRAM CALLING BLKTRI.  THIS PARAMETER IS
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
C       WORK SPACE.
C             IF NP=1 DEFINE K=INT(LOG2(N))+1 AND SET L=2**(K+1) THEN
C                     W MUST HAVE DIMENSION (K-2)*L+K+5+MAX(2N,6M)
C
C             IF NP=0 DEFINE K=INT(LOG2(N-1))+1 AND SET L=2**(K+1) THEN
C                     W MUST HAVE DIMENSION (K-2)*L+K+5+2N+MAX(2N,6M)
C
C       **IMPORTANT** FOR PURPOSES OF CHECKING, THE REQUIRED DIMENSION
C                     OF W IS COMPUTED BY BLKTRI AND STORED IN W(1)
C                     IN FLOATING POINT FORMAT.
C
C     * * * * * * * * * *     ON OUTPUT     * * * * * * * * * *
C
C     Y
C       CONTAINS THE SOLUTION X.
C
C     IERROR
C       AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.  EXCEPT
C       FOR NUMBER ZERO, A SOLUTION IS NOT ATTEMPTED.
C
C       = 0  NO ERROR.
C       = 1  M IS LESS THAN 5
C       = 2  N IS LESS THAN 5
C       = 3  IDIMY IS LESS THAN M.
C       = 4  BLKTRI FAILED WHILE COMPUTING RESULTS THAT DEPEND ON THE
C            COEFFICIENT ARRAYS AN, BN, CN.  CHECK THESE ARRAYS.
C       = 5  AN(J)*CN(J-1) IS LESS THAN 0 FOR SOME J. POSSIBLE REASONS
C            FOR THIS CONDITION ARE
C            1. THE ARRAYS AN AND CN ARE NOT CORRECT
C            2. TOO LARGE A GRID SPACING WAS USED IN THE DISCRETIZATION
C               OF THE ELLIPTIC EQUATION
C            3. THE LINEAR EQUATIONS RESULTED FROM A PARTIAL
C               DIFFERENTIAL EQUATION WHICH WAS NOT ELLIPTIC
C
C     W
C       CONTAINS INTERMEDIATE VALUES THAT MUST NOT BE DESTROYED IF
C       BLKTRI WILL BE CALLED AGAIN WITH IFLG=1. W(1) CONTAINS THE
C       NUMBER OF LOCATIONS REQUIRED BY W IN FLOATING POINT FORMAT.
C
C     * * * * * * *   PROGRAM SPECIFICATIONS    * * * * * * * * * * * *
C
C     DIMENSION OF   AN(N),BN(N),CN(N),AM(M),BM(M),CM(M),Y(IDIMY,N)
C     ARGUMENTS      W(SEE ARGUMENT LIST)
C
C     LATEST         JUNE 1979
C     REVISION
C
C     REQUIRED       BLKTRI,BLKTRI,PROD,PRODP,CPROD,CPRODP,COMPB,INDXA,
C     SUBPROGRAMS    INDXB,INDXC,PPADD,PSGF,PPSGF,PPSPF,BSRH,TEVLS,
C                    EPMACH,STORE
C
C     SPECIAL        THE ALGORITHM MAY FAIL IF ABS(BM(I)+BN(J)) IS LESS
C     CONDITIONS     THAN ABS(AM(I))+ABS(AN(J))+ABS(CM(I))+ABS(CN(J))
C                    FOR SOME I AND J. THE ALGORITHM WILL ALSO FAIL IF
C                    AN(J)*CN(J-1) IS LESS THAN ZERO FOR SOME J
C                    SEE THE DISCRIPTION OF THE OUTPUT PARAMETER IERROR.
C
C     COMMON         CBLKT,VALUE
C     BLOCKS
C
C     I/O            NONE
C
C     PRECISION      SINGLE
C
C     SPECIALIST     PAUL SWARZTRAUBER
C
C     LANGUAGE       FORTRAN
C
C     HISTORY        VERSION 1 SEPTEMBER 1973
C                    VERSION 2 APRIL     1976
C                    VERSION 3 JUNE      1979
C
C     ALGORITHM      GENERALIZED CYCLIC REDUCTION (SEE REFERENCE BELOW)
C
C     SPACE
C     REQUIRED       CONTROL DATA 7600
C
C     PORTABILITY    AMERICAN NATIONAL STANDARDS INSTITUTE FORTRAN.
C                    THE APPROXIMATE MACHINE ACCURACY IS COMPUTED IN
C                    FUNCTION EPMACH
C
C     REQUIRED       NONE
C     RESIDENT
C     ROUTINES
C
C     REFERENCES     SWARZTRAUBER,P. AND R. SWEET, 'EFFICIENT FORTRAN
C                    SUBPROGRAMS FOR THE SOLUTION OF ELLIPTIC EQUATIONS'
C                    NCAR TN/IA-109, JULY, 1975, 138 PP.
C
C                    SWARZTRAUBER P. N.,A DIRECT METHOD FOR THE DISCRETE
C                    SOLUTION OF SEPARABLE ELLIPTIC EQUATIONS, S.I.A.M.
C                    J. NUMER. ANAL.,11(1974) PP. 1136-1150.
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C
      DIMENSION       AN(1)      ,BN(1)      ,CN(1)      ,AM(1)      ,
     1                BM(1)      ,CM(1)      ,Y(IDIMY,1) ,W(1)
      EXTERNAL        PROD       ,PRODP      ,CPROD      ,CPRODP
      COMMON /CBLKT/  NPP        ,K          ,EPS        ,CNV        ,
     1                NM         ,NCMPLX     ,IK
C
C TEST M AND N FOR THE PROPER FORM
C
      NM = N
      IERROR = 0
      IF (M-5) 101,102,102
  101 IERROR = 1
      GO TO 119
  102 IF (NM-3) 103,104,104
  103 IERROR = 2
      GO TO 119
  104 IF (IDIMY-M) 105,106,106
  105 IERROR = 3
      GO TO 119
  106 NH = N
      NPP = NP
      IF (NPP) 107,108,107
  107 NH = NH+1
  108 IK = 2
      K = 1
  109 IK = IK+IK
      K = K+1
      IF (NH-IK) 110,110,109
  110 NL = IK
      IK = IK+IK
      NL = NL-1
      IWAH = (K-2)*IK+K+6
      IF (NPP) 111,112,111
C
C     DIVIDE W INTO WORKING SUB ARRAYS
C
  111 IW1 = IWAH
      IWBH = IW1+NM
      W(1) = FLOAT(IW1-1+MAX0(2*NM,6*M))
      GO TO 113
  112 IWBH = IWAH+NM+NM
      IW1 = IWBH
      W(1) = FLOAT(IW1-1+MAX0(2*NM,6*M))
      NM = NM-1
C
C SUBROUTINE COMP B COMPUTES THE ROOTS OF THE B POLYNOMIALS
C
  113 IF (IERROR) 119,114,119
  114 IW2 = IW1+M
      IW3 = IW2+M
      IWD = IW3+M
      IWW = IWD+M
      IWU = IWW+M
      IF (IFLG) 116,115,116
  115 CALL COMPB (NL,IERROR,AN,BN,CN,W(2),W(IWAH),W(IWBH))
      GO TO 119
  116 IF (MP) 117,118,117
C
C SUBROUTINE BLKTR1 SOLVES THE LINEAR SYSTEM
C
  117 CALL BLKTR1 (NL,AN,BN,CN,M,AM,BM,CM,IDIMY,Y,W(2),W(IW1),W(IW2),
     1             W(IW3),W(IWD),W(IWW),W(IWU),PROD,CPROD)
      GO TO 119
  118 CALL BLKTR1 (NL,AN,BN,CN,M,AM,BM,CM,IDIMY,Y,W(2),W(IW1),W(IW2),
     1             W(IW3),W(IWD),W(IWW),W(IWU),PRODP,CPRODP)
  119 CONTINUE
      RETURN
      END
