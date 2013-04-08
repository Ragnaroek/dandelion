      subroutine sub()
      parameter (n = 3)
      parameter (m = 4)
      integer ka(n, m)
      data ka/11,21,31,12, 22, 32/

      do 10 k = 1,n
         do 20 j = 1,m
            print 9000, k, j, ka(k, j)
  020    continue
   10 continue
 9000 format(1x, 'KA(', i1, ', ', i1, ') = ', i5)
      

      return
      end
      
      program init
      integer iarray(3,2)
      data iarray/11,21,31,12,22,32/

      do 10 k = 1,3
         do 20 j = 1,2
            print 9000, k, j, iarray(k, j)
  020    continue
   10 continue
 9000 format(1x, 'X(', i1, ', ', i1, ') = ', i5)

      call sub
      end
