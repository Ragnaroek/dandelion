      program slice
c
c     Simple program to test if array slicing works.
c
      parameter (len = 20)
      real x(len)
      integer k

      do 10 k = 1, len
         x(k) = k * k
   10 continue

      call sub2(x, len)
      call sub2(x(5), 5)
      call sub2(x(10), 10)
      end

      subroutine sub2(x,n)
      real x(n)
      x(2) = 89
      call sub(x(2), n-1)
      return
      end
      subroutine sub(x, n)
      real x(n)
      integer k

      do 10 k = 1, n
         write(*, 9000) k, x(k)
   10 continue
 9000 format(1x, i5, g15.7)
      end

