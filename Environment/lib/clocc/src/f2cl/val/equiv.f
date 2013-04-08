      double precision function elkup (n, y, ewt, itol, rtol, atol)
c This routine looks up approximately correct values of y at t = 0.4,
c ytrue = y9 or y99 depending on whether n = 9 or 99.  These were
c obtained by running DLSODI with very tight tolerances.
c The returned value is
c elkup  =  norm of  ( y - ytrue ) / ( rtol*abs(ytrue) + atol ).
c
      integer n, itol, i
      double precision y, ewt, rtol, atol, y9, y99, y99a, y99b, y99c,
     1   y99d, y99e, y99f, y99g, dvnorm
      dimension y(n), ewt(n), y9(9), y99(99)
      dimension y99a(16), y99b(16), y99c(16), y99d(16), y99e(16),
     1          y99f(16), y99g(3)
      equivalence (y99a(1),y99(1)), (y99b(1),y99(17)),
     1      (y99c(1),y99(33)), (y99d(1),y99(49)), (y99e(1),y99(65)),
     1     (y99f(1),y99(81)), (y99g(1),y99(97))
      equivalence (y, y99g(2))
      data y9 /
     1 1.07001457d-01, 2.77432492d-01, 5.02444616d-01, 7.21037157d-01,
     1 9.01670441d-01, 8.88832048d-01, 4.96572850d-01, 9.46924362d-02,
     1-6.90855199d-03 /
      data y99a /
     1 2.05114384d-03, 4.19527452d-03, 6.52533872d-03, 9.13412751d-03,
     1 1.21140191d-02, 1.55565301d-02, 1.95516488d-02, 2.41869487d-02,
     1 2.95465081d-02, 3.57096839d-02, 4.27498067d-02, 5.07328729d-02,
     1 5.97163151d-02, 6.97479236d-02, 8.08649804d-02, 9.30936515d-02 /
      data y99b /
     1 1.06448659d-01, 1.20933239d-01, 1.36539367d-01, 1.53248227d-01,
     1 1.71030869d-01, 1.89849031d-01, 2.09656044d-01, 2.30397804d-01,
     1 2.52013749d-01, 2.74437805d-01, 2.97599285d-01, 3.21423708d-01,
     1 3.45833531d-01, 3.70748792d-01, 3.96087655d-01, 4.21766871d-01 /
      data y99c /
     1 4.47702161d-01, 4.73808532d-01, 5.00000546d-01, 5.26192549d-01,
     1 5.52298887d-01, 5.78234121d-01, 6.03913258d-01, 6.29252015d-01,
     1 6.54167141d-01, 6.78576790d-01, 7.02400987d-01, 7.25562165d-01,
     1 7.47985803d-01, 7.69601151d-01, 7.90342031d-01, 8.10147715d-01 /
      data y99d /
     1 8.28963844d-01, 8.46743353d-01, 8.63447369d-01, 8.79046021d-01,
     1 8.93519106d-01, 9.06856541d-01, 9.19058529d-01, 9.30135374d-01,
     1 9.40106872d-01, 9.49001208d-01, 9.56853318d-01, 9.63702661d-01,
     1 9.69590361d-01, 9.74555682d-01, 9.78631814d-01, 9.81840924d-01 /
      data y99e /
     1 9.84188430d-01, 9.85656465d-01, 9.86196496d-01, 9.85721098d-01,
     1 9.84094964d-01, 9.81125395d-01, 9.76552747d-01, 9.70041743d-01,
     1 9.61175143d-01, 9.49452051d-01, 9.34294085d-01, 9.15063568d-01,
     1 8.91098383d-01, 8.61767660d-01, 8.26550038d-01, 7.85131249d-01 /
      data y99f /
     1 7.37510044d-01, 6.84092540d-01, 6.25748369d-01, 5.63802368d-01,
     1 4.99946558d-01, 4.36077986d-01, 3.74091566d-01, 3.15672765d-01,
     1 2.62134958d-01, 2.14330497d-01, 1.72640946d-01, 1.37031155d-01,
     1 1.07140815d-01, 8.23867920d-02, 6.20562432d-02, 4.53794321d-02 /
      data y99g / 3.15789227d-02, 1.98968820d-02, 9.60472135d-03 /
c
      if (n .eq. 99) go to 99
c
c Compute local error tolerance using correct y (n = 9).
c
      call dewset( n, itol, rtol, atol, y9, ewt )
c
c Invert ewt and replace y by the error, y - ytrue.
c
      do 20  i = 1, 9
        ewt(i) = 1.0d0/ewt(i)
 20     y(i) = y(i) - y9(i)
      go to 200
c
c Compute local error tolerance using correct y (n = 99).
c
 99   call dewset( n, itol, rtol, atol, y99, ewt )
c
c Invert ewt and replace y by the error, y - ytrue.
c
      do 120  i = 1, 99
        ewt(i) = 1.0d0/ewt(i)
 120    y(i) = y(i) - y99(i)
c
c Find weighted norm of the error and return.
c
 200  elkup = dvnorm (n, y, ewt)
      return
c end of function elkup for the DLSODI demonstration program.
      end
