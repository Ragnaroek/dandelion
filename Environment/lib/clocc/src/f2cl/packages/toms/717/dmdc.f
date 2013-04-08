      DOUBLE PRECISION FUNCTION DR7MDC(K)
C
C  ***  RETURN MACHINE DEPENDENT CONSTANTS USED BY NL2SOL  ***
C
C +++  COMMENTS BELOW CONTAIN DATA STATEMENTS FOR VARIOUS MACHINES.  +++
C +++  TO CONVERT TO ANOTHER MACHINE, PLACE A C IN COLUMN 1 OF THE   +++
C +++  DATA STATEMENT LINE(S) THAT CORRESPOND TO THE CURRENT MACHINE +++
C +++  AND REMOVE THE C FROM COLUMN 1 OF THE DATA STATEMENT LINE(S)  +++
C +++  THAT CORRESPOND TO THE NEW MACHINE.                           +++
C
      INTEGER K
C
C  ***  THE CONSTANT RETURNED DEPENDS ON K...
C
C  ***        K = 1... SMALLEST POS. ETA SUCH THAT -ETA EXISTS.
C  ***        K = 2... SQUARE ROOT OF ETA.
C  ***        K = 3... UNIT ROUNDOFF = SMALLEST POS. NO. MACHEP SUCH
C  ***                 THAT 1 + MACHEP .GT. 1 .AND. 1 - MACHEP .LT. 1.
C  ***        K = 4... SQUARE ROOT OF MACHEP.
C  ***        K = 5... SQUARE ROOT OF BIG (SEE K = 6).
C  ***        K = 6... LARGEST MACHINE NO. BIG SUCH THAT -BIG EXISTS.
C
c      DOUBLE PRECISION BIG, ETA, MACHEP, ZERO
c      INTEGER BIGI(2), ETAI(2), MACHEI(2)
c      EQUIVALENCE (BIG,BIGI(1)), (ETA,ETAI(1)), (MACHEP,MACHEI(1))
c      PARAMETER (ZERO=0.D+0)
C
C  +++ IEEE ARITHMETIC MACHINES IN WHICH THE MOST SIGNIFICANT BYTE
C  +++ IS STORED FIRST, SUCH AS THE AT&T 3B SERIES AND MACHINES
C  +++ BASED ON SPARC, MIPS, AND MOTOROLA 68XXX PROCESSORS.
C
C      DATA BIGI(1),BIGI(2)     / 2146435071,         -1 /
C      DATA ETAI(1),ETAI(2)     /    1048576,          0 /
C      DATA MACHEI(1),MACHEI(2) / 1017118720,          0 /
C
C  +++ IEEE ARITHMETIC MACHINES IN WHICH THE LEAST SIGNIFICANT BYTE
C  +++ IS STORED FIRST, SUCH AS MACHINES BASED ON INTEL PROCESSORS,
C  +++ E.G. PERSONAL COMPUTERS WITH AN INTEL 80X87.
C
c      DATA BIGI(1),BIGI(2)     / -1, 2146435071 /
c      DATA ETAI(1),ETAI(2)     /  0,    1048576 /
c      DATA MACHEI(1),MACHEI(2) /  0, 1017118720 /
C
C  +++  IBM, AMDAHL, OR XEROX MAINFRAME  +++
C
C      DATA BIGI(1),BIGI(2)/2147483647, -1/
C      DATA ETAI(1),ETAI(2)/1048576, 0/
C      DATA MACHEI(1),MACHEI(2)/873463808,0/
C
C  +++  VAX  +++
C
C      DATA BIGI(1),BIGI(2)     / -32769, -1 /
C      DATA ETAI(1),ETAI(2)     /    128,  0 /
C      DATA MACHEI(1),MACHEI(2) /   9344,  0 /
C
C  +++  CRAY  +++
C
C      DATA BIGI(1)/6917247552664371199/
C      DATA BIGI(2)/128891879815246481/
C      DATA ETAI(1)/2332160919536140288/
C      DATA ETAI(2)/0/
C      DATA MACHEI(1)/4585931058058362880/
C      DATA MACHEI(2)/0/
C
C  +++  PORT LIBRARY -- REQUIRES MORE THAN JUST A DATA STATEMENT, +++
C  +++                  BUT HAS CONSTANTS FOR MANY MORE MACHINES. +++
C
C  To get the current D1MACH, which has constants for many more
C  machines, ask netlib@research.att.com to
C                    send d1mach from cor
C  For machines with rounded arithmetic (e.g., IEEE or VAX arithmetic),
C  use MACHEP = 0.5D0 * D1MACH(4) below.
C
C      DOUBLE PRECISION D1MACH
C      EXTERNAL D1MACH
C      DATA BIG/0.D+0/, ETA/0.D+0/, MACHEP/0.D+0/, ZERO/0.D+0/
C      IF (BIG .GT. ZERO) GO TO 1
C         BIG = D1MACH(2)
C         ETA = D1MACH(1)
C         MACHEP = D1MACH(4)
C1     CONTINUE
C
C  +++ END OF PORT +++
C
C-------------------------------  BODY  --------------------------------
C
c      IF (MACHEP .LE. ZERO) THEN
c         WRITE(*,*) 'Edit DR7MDC to activate the appropriate statements'
c         STOP 987
c         ENDIF
c      GO TO (10, 20, 30, 40, 50, 60), K
cC
c 10   DR7MDC = ETA
c      GO TO 999
cC
c 20   DR7MDC = SQRT(256.D+0*ETA)/16.D+0
c      GO TO 999
cC
c 30   DR7MDC = MACHEP
c      GO TO 999
cC
c 40   DR7MDC = SQRT(MACHEP)
c      GO TO 999
cC
c 50   DR7MDC = SQRT(BIG/256.D+0)*16.D+0
c      GO TO 999
cC
c 60   DR7MDC = BIG
cC
c 999  RETURN
      if (k .eq. 1) then
         dr7mdc = d1mach(1)
      else if (k .eq. 2) then
         dr7mdc = sqrt(d1mach(1))
      else if (k .eq. 3) then
         dr7mdc = d1mach(3)
      else if (k .eq. 4) then
         dr7mdc = sqrt(d1mach(3))
      else if (k .eq. 5) then
         dr7mdc = sqrt(d1mach(2))
      else if (k .eq. 6) then
         dr7mdc = d1mach(2)
      endif
      
C  ***  LAST LINE OF DR7MDC FOLLOWS  ***
      END
      INTEGER FUNCTION I7MDCN(K)
C
      INTEGER K
C
C  ***  RETURN INTEGER MACHINE-DEPENDENT CONSTANTS  ***
C
C     ***  K = 1 MEANS RETURN STANDARD OUTPUT UNIT NUMBER.   ***
C     ***  K = 2 MEANS RETURN ALTERNATE OUTPUT UNIT NUMBER.  ***
C     ***  K = 3 MEANS RETURN  INPUT UNIT NUMBER.            ***
C          (NOTE -- K = 2, 3 ARE USED ONLY BY TEST PROGRAMS.)
C
C  +++  PORT VERSION FOLLOWS...
C      INTEGER I1MACH
C      EXTERNAL I1MACH
C      INTEGER MDPERM(3)
C      DATA MDPERM(1)/2/, MDPERM(2)/4/, MDPERM(3)/1/
C      I7MDCN = I1MACH(MDPERM(K))
C  +++  END OF PORT VERSION  +++
C
C  +++  NON-PORT VERSION FOLLOWS...
      INTEGER MDCON(3)
      DATA MDCON(1)/6/, MDCON(2)/8/, MDCON(3)/5/
      I7MDCN = MDCON(K)
C  +++  END OF NON-PORT VERSION  +++
C
 999  RETURN
C  ***  LAST LINE OF I7MDCN FOLLOWS  ***
      END
