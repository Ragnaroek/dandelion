      SUBROUTINE TEVLS (N,D,E2,IERR)
C
      INTEGER         I          ,J          ,L          ,M          ,
     1                N          ,II         ,L1         ,MML        ,
     2                IERR
      REAL            D(N)       ,E2(N)
      REAL            B          ,C          ,F          ,G          ,
     1                H          ,P          ,R          ,S          ,
     2                MACHEP
C
C     REAL SQRT,ABS,SIGN
C
      COMMON /CBLKT/  NPP        ,K          ,EPS     ,CNV        ,
     1                NM         ,NCMPLX     ,IK
C
C     THIS SUBROUTINE IS A MODIFICATION OF THE EISPACK SUBROUTINE TQLRAT
C     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
C     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.
C
C     ON INPUT-
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E2 CONTAINS THE                SUBDIAGONAL ELEMENTS OF THE
C          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
C          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
C          THE SMALLEST EIGENVALUES,
C
C        E2 HAS BEEN DESTROYED,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C
C     ********** EPS IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C                **********
C
      IERR = 0
      IF (N .EQ. 1) GO TO 115
C
      DO 101 I=2,N
         E2(I-1) = E2(I)*E2(I)
  101 CONTINUE
C
      F = 0.0
      B = 0.0
      E2(N) = 0.0
C
      DO 112 L=1,N
         J = 0
         H = EPS*(ABS(D(L))+SQRT(E2(L)))
         IF (B .GT. H) GO TO 102
         B = H
         C = B*B
C
C     ********** LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT **********
C
  102    DO 103 M=L,N
            IF (E2(M) .LE. C) GO TO 104
C
C     ********** E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
  103    CONTINUE
C
  104    IF (M .EQ. L) GO TO 108
  105    IF (J .EQ. 30) GO TO 114
         J = J+1
C
C     ********** FORM SHIFT **********
C
         L1 = L+1
         S = SQRT(E2(L))
         G = D(L)
         P = (D(L1)-G)/(2.0*S)
         R = SQRT(P*P+1.0)
         D(L) = S/(P+SIGN(R,P))
         H = G-D(L)
C
         DO 106 I=L1,N
            D(I) = D(I)-H
  106    CONTINUE
C
         F = F+H
C
C     ********** RATIONAL QL TRANSFORMATION **********
C
         G = D(M)
         IF (G .EQ. 0.0) G = B
         H = G
         S = 0.0
         MML = M-L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
         DO 107 II=1,MML
            I = M-II
            P = G*H
            R = P+E2(I)
            E2(I+1) = S*R
            S = E2(I)/R
            D(I+1) = H+S*(H+D(I))
            G = D(I)-E2(I)/G
            IF (G .EQ. 0.0) G = B
            H = G*P/R
  107    CONTINUE
C
         E2(L) = S*G
         D(L) = H
C
C     ********** GUARD AGAINST UNDERFLOWED H **********
C
         IF (H .EQ. 0.0) GO TO 108
         IF (ABS(E2(L)) .LE. ABS(C/H)) GO TO 108
         E2(L) = H*E2(L)
         IF (E2(L) .NE. 0.0) GO TO 105
  108    P = D(L)+F
C
C     ********** ORDER EIGENVALUES **********
C
         IF (L .EQ. 1) GO TO 110
C
C     ********** FOR I=L STEP -1 UNTIL 2 DO -- **********
C
         DO 109 II=2,L
            I = L+2-II
            IF (P .GE. D(I-1)) GO TO 111
            D(I) = D(I-1)
  109    CONTINUE
C
  110    I = 1
  111    D(I) = P
  112 CONTINUE
C
      IF (ABS(D(N)) .GE. ABS(D(1))) GO TO 115
      NHALF = N/2
      DO 113 I=1,NHALF
         NTOP = N-I
         DHOLD = D(I)
         D(I) = D(NTOP+1)
         D(NTOP+1) = DHOLD
  113 CONTINUE
      GO TO 115
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
  114 IERR = L
  115 RETURN
C
C     ********** LAST CARD OF TQLRAT **********
C
      END
