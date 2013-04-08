C test for translation of various format stmts
C
      SUBROUTINE subr1
      IMPLICIT REAL (i,l-n)
      write(*, 9030)
      i=1.
      j=2
      k=3
      l=4.5
      write(*,9010), j
      write(*,9011) 100
      write(*,9012) 50
      write(*,9013) 10000
      write(*,9020) l
      write(*,9025) l
      write(*,9040), k, j
      write(*,9050) l,l,l,l
      write(*,9060), l
      write(*,9065) l
 9010 FORMAT("INTEGER VALUE",I3/)
 9011 format("Integer value",i3/)
 9012 format("Integer value",i4.2/)
 9013 format("Overflow value", i3)
9020  FORMAT("REAL VALUE*",F3.2/)
 9025 format("REAL VALUE ", f4.2/)
9030  FORMAT("STRING WITH NO ARGS"/)
9040  FORMAT("MORE THAN 1 VALUE", I3, I3 /)
9050  FORMAT("Lots of reals: ", 4F5.2 /)
9060  FORMAT("scale factor of two*: ", 2PF4.1 /)
 9065 FORMAT("scale factor of two: ", 2PF5.1 /)
      END
C
      SUBROUTINE subr2
      J=10
      A=10.
      B=10.1
      C=10.2
      D=10.3
      WRITE(*,111),J,A,B,C,D
      write(*,112),J,dble(a),dble(b),dble(c),dble(d)
  111 FORMAT(1X,I6,2X,4E16.8)
  112 FORMAT(1X,I6,2X,4D16.8)
  372 WRITE(*,908),J,J,D                                              
  908 FORMAT(19H0ZFEAS ...ITERATION,I5,5X,18HPIVOT REJECTED  Q=,
     X I5,5X,6HPIVOT=,D16.8)
      WRITE(*,911),J,B                                     
  911 FORMAT(' ITERATION= ',I6,5X,'ZFEAS OBJECTIVE= ',D16.8) 
      WRITE(*, 904)                   
  904 FORMAT(' ZFEAS...INTEGER FEASIBLE SOLUTION FOUND')                        
      WRITE(*,830),J,A                                                       
  830 FORMAT(' 0/1 IN BASIS= ',I6,5X,'INTEGER INFEAS= ',D15.8)                  
      END
C
      SUBROUTINE subr3
      WRITE(*,*), "LIST DIRECTED OUTPUT"
      WRITE(*,*), 2.3, 3E5, +.123, 1D2, -4.8
      WRITE(*, *), 1,2,3
      END 
C
      SUBROUTINE subr4
      parameter (nsize = 100)
      INTEGER TESTARR(nsize)
      DO 200 I=1,nsize
        TESTARR(I) = I
  200 CONTINUE
      WRITE(*,*), "ARRAY ELEMENTS:"
      DO 300 I=1,nsize
        WRITE(*, *) TESTARR(I)
  300 CONTINUE
      WRITE(*, *) "ARRAY:"
      WRITE (*, 400) TESTARR
      WRITE(*, *) "ARRAY2:"
      WRITE (*, 410) TESTARR
      WRITE(*,*) "SUBARRAY:"
      WRITE(*, 400) (TESTARR(i), I=2,4)
  400 FORMAT(I4)
c  410 format(i3)
  410 format(3(i4, i3), 4(i2, i8))
      END

      subroutine subr5
      integer iunit(5)
      do 10 k = 1, 5
         iunit(k) = 5
   10 continue
      
      do 20 k = 1, 5
         write(iunit(k), 9000) 'Unit ', k
   20 continue
 9000 format(1x, a10, i4)
      end

C
C
C main
      PROGRAM TEST
      CALL subr1
      CALL subr2
      CALL subr3
      CALL SUBR4
      call subr5
      STOP
      END
C
