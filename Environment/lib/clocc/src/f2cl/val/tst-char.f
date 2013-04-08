c Some random tests for character handling
c      program prog
      parameter (len = 45)
      real x(37)
      real y(3, 4)
      character s1*10, s2*20
      character*33 s3, s4
      character ca(3,4)*11, ca2(4,5)*12
      character*17 cb(1,2), cb2(2,3), cb3(0:3, len)
      character*5 a1(4), a2(6)
      character*(*) zz
      character c1
      character*1 p01rec(1)

      x(8) = 8.0
      y(1, 3) = 7.0
      c1 = 'a'
      s3 = 'abcd'
      ca(1,1) = '12345678901'
      ca2(1,1) = ca(1,1)
      write(*, 9000) c1
      write(*, 9000) s3
      write(*, 9000) ca(1,1)
      write(*, 9000) ca2(1,1)
 9000 format(1x, '|', a, '|')

      call xyzzy(p01rec)
      end

      subroutine zz (x, y)
      real x
      real y(10)
      x = 5
      y(10) = 7
      y(3) = 8
      return
      end
