      SUBROUTINE COSGEN (N,IJUMP,FNUM,FDEN,A)
      DIMENSION       A(1)
C
C
C     THIS SUBROUTINE COMPUTES REQUIRED COSINE VALUES IN ASCENDING
C     ORDER.  WHEN IJUMP .GT. 1 THE ROUTINE COMPUTES VALUES
C
C        2*COS(J*PI/L) , J=1,2,...,L AND J .NE. 0(MOD N/IJUMP+1)
C
C     WHERE L = IJUMP*(N/IJUMP+1).
C
C
C     WHEN IJUMP = 1 IT COMPUTES
C
C            2*COS((J-FNUM)*PI/(N+FDEN)) ,  J=1, 2, ... ,N
C
C     WHERE
C        FNUM = 0.5, FDEN = 0.0,  FOR REGULAR REDUCTION VALUES
C        FNUM = 0.0, FDEN = 1.0, FOR B-R AND C-R WHEN ISTAG = 1
C        FNUM = 0.0, FDEN = 0.5, FOR B-R AND C-R WHEN ISTAG = 2
C        FNUM = 0.5, FDEN = 0.5, FOR B-R AND C-R WHEN ISTAG = 2
C                                IN POISN2 ONLY.
C
C
      PI = PIMACH(DUM)
      IF (N .EQ. 0) GO TO 105
      IF (IJUMP .EQ. 1) GO TO 103
      K3 = N/IJUMP+1
      K4 = K3-1
      PIBYN = PI/FLOAT(N+IJUMP)
      DO 102 K=1,IJUMP
         K1 = (K-1)*K3
         K5 = (K-1)*K4
         DO 101 I=1,K4
            X = K1+I
            K2 = K5+I
            A(K2) = -2.*COS(X*PIBYN)
  101    CONTINUE
  102 CONTINUE
      GO TO 105
  103 CONTINUE
      NP1 = N+1
      Y = PI/(FLOAT(N)+FDEN)
      DO 104 I=1,N
         X = FLOAT(NP1-I)-FNUM
         A(I) = 2.*COS(X*Y)
  104 CONTINUE
  105 CONTINUE
      RETURN
      END
