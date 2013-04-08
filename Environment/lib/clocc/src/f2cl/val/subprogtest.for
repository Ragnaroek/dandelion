C test for translation of various sub programs
C (Fortran version doesn't seem to work properly)
C
C a function sub program
      FUNCTION ifun(arg1, arg2)
      INTEGER arg1, arg2
c      write(*, 2000),arg1, arg2
      ifun = arg1 + arg2
c      write(*,2010),ifun
2000  FORMAT("args to ifun are", I3 , " and", I3)
2010  FORMAT("ifun returning ", I3/)
      RETURN
      END
C
C a typed FUNCTION
      REAL FUNCTION jfun(arg1)
      REAL arg1
c      write(*, 3000), arg1
      jfun = arg1
c      write(*, 3010), jfun
3000  FORMAT("arg to jfun is ", F4.2)
3010  FORMAT("jfun returning ", F4.2/)
      RETURN
      END
C
C subroutine with a stmt function
      SUBROUTINE subr1(a,b)
      DIMENSION arrayA(3,4,3)
      stmtfn(x, y, z) = max(a, max(a, b))
      c = stmtfn(b,a,a)
      arrayA(1,1,1) = c
      write(*,9000),a
9000  format("smntfun value is ",g10.4/)
      RETURN
      END

C subroutine with no args and which calls the functions
      SUBROUTINE subr2
      IMPLICIT REAL (l-n)
      real i
      real jfun
      write(*, 9030)
      i=1.
      j=2
      k=3
      l=4.5
      write(*,9010), ifun(j, k)
      write(*,9020), jfun(i+l)
9010  FORMAT("IFUN RETURNS ",I3) 
9020  FORMAT("JFUN RETURNS ",F10.5)
9030  FORMAT("inside subroutine subr2"/)
      END
C
C main
      PROGRAM TEST
      CALL subr1(3.0, 4.0)
      CALL subr2
      STOP
      END
C
