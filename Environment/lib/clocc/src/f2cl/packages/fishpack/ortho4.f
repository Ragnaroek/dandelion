      SUBROUTINE ORTHO4 (USOL,IDMN,ZN,ZM,PERTRB)
C
C     THIS SUBROUTINE ORTHO4ONALIZES THE ARRAY USOL WITH RESPECT TO
C     THE CONSTANT ARRAY IN A WEIGHTED LEAST SQUARES NORM
C
      COMMON /SPL4/   KSWX       ,KSWY       ,K          ,L          ,
     1                AIT        ,BIT        ,CIT        ,DIT        ,
     2                MIT        ,NIT        ,IS         ,MS         ,
     3                JS         ,NS         ,DLX        ,DLY        ,
     4                TDLX3      ,TDLY3      ,DLX4       ,DLY4
      DIMENSION       USOL(IDMN,1)           ,ZN(1)      ,ZM(1)
      ISTR = IS
      IFNL = MS
      JSTR = JS
      JFNL = NS
C
C     COMPUTE WEIGHTED INNER PRODUCTS
C
      UTE = 0.0
      ETE = 0.0
      DO  20 I=IS,MS
         II = I-IS+1
         DO  10 J=JS,NS
            JJ = J-JS+1
            ETE = ETE+ZM(II)*ZN(JJ)
            UTE = UTE+USOL(I,J)*ZM(II)*ZN(JJ)
   10    CONTINUE
   20 CONTINUE
C
C     SET PERTURBATION PARAMETER
C
      PERTRB = UTE/ETE
C
C     SUBTRACT OFF CONSTANT PERTRB
C
      DO  40 I=ISTR,IFNL
         DO  30 J=JSTR,JFNL
            USOL(I,J) = USOL(I,J)-PERTRB
   30    CONTINUE
   40 CONTINUE
      RETURN
      END
