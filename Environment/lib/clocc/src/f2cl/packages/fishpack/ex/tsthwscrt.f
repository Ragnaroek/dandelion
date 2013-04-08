c fishpk25 from portlib                                  12/30/83
c
c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c     *                                                               *
c     *                        f i s h p a k                          *
c     *                                                               *
c     *                                                               *
c     *     a package of fortran subprograms for the solution of      *
c     *                                                               *
c     *      separable elliptic partial differential equations        *
c     *                                                               *
c     *                  (version 3.1 , october 1980)                  *
c     *                                                               *
c     *                             by                                *
c     *                                                               *
c     *        john adams, paul swarztrauber and roland sweet         *
c     *                                                               *
c     *                             of                                *
c     *                                                               *
c     *         the national center for atmospheric research          *
c     *                                                               *
c     *                boulder, colorado  (80307)  u.s.a.             *
c     *                                                               *
c     *                   which is sponsored by                       *
c     *                                                               *
c     *              the national science foundation                  *
c     *                                                               *
c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c
c          program to illustrate the use of subroutine hwscrt to solve
c     the equation
c
c     (d/dx)(du/dx) + (d/dy)(du/dy) - 4*u
c
c     = (2 - (4 + pi**2/4)*x**2)*cos((y+1)*pi/2)
c
c     with the boundary conditions
c     on the rectangle 0 .lt. x .lt. 2, -1 .lt. y .lt. 3 with the
c
c     u(0,y) = 0
c                                          -1 .le. y .le. 3
c     (du/dx)(2,y) = 4*cos((y+1)*pi/2)
c
c     and with u periodic in y.
c          the x-interval will be divided into 40 panels and the
c     y-interval will be divided into 80 panels.
c
      dimension       f(45,82)   ,bdb(81)    ,w(1103)    ,x(41)      ,
     1                y(81)
      dimension bda(1), bdc(1), bdd(1)
c
c     from dimension statement we get value of idimf.  also note that w
c     is dimensioned 4*(n+1) + (13 + int(log2(n+1)))*(m+1) .
c
      idimf = 45
      a = 0.
      b = 2.
      m = 40
      mbdcnd = 2
      c = -1.
      d = 3.
      n = 80
      nbdcnd = 0
      elmbda = -4.
c
c     auxiliary quantities.
c
      pi = pimach(dum)
      piby2 = pi/2.
      pisq = pi**2
      mp1 = m+1
      np1 = n+1
c
c     generate and store grid points for the purpose of computing
c     boundary data and the right side of the helmholtz equation.
c
      do 101 i=1,mp1
         x(i) = float(i-1)/20.
  101 continue
      do 102 j=1,np1
         y(j) = -1.+float(j-1)/20.
  102 continue
c
c     generate boundary data.
c
      do 103 j=1,np1
         bdb(j) = 4.*cos((y(j)+1.)*piby2)
  103 continue
c
c     bda, bdc, and bdd are dummy variables.
c
      do 104 j=1,np1
         f(1,j) = 0.
  104 continue
c
c     generate right side of equation.
c
      do 106 i=2,mp1
         do 105 j=1,np1
            f(i,j) = (2.-(4.+pisq/4.)*x(i)**2)*cos((y(j)+1.)*piby2)
  105    continue
  106 continue
      call hwscrt (a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,elmbda,f,
     1             idimf,pertrb,ierror,w)
c
c     compute discretization error.  the exact solution is
c                u(x,y) = x**2*cos((y+1)*piby2)
c
      err = 0.
      do 108 i=1,mp1
         do 107 j=1,np1
            z = abs(f(i,j)-x(i)**2*cos((y(j)+1.)*piby2))
            if (z .gt. err) err = z
  107    continue
  108 continue
      print 1001 , ierror,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hwscrt example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 5.36508e-04/
     4        12x,32hrequired length of w array = 880//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5/
     7        12x,28hrequired length of w array =,f4.0)
c
      end
c
