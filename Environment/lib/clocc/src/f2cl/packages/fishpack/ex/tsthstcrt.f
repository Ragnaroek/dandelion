c fishpk30 from portlib                                  12/30/83
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
c     program to illustrate the use of subroutine hstcrt to solve
c     the equation
c
c    (d/dx)(du/dx) + (d/dy)(du/dy) - 2*u = -2(pi**2+1)sin(pi*x)cos(pi*y)
c
c     where  1 .le. x .le. 3 and -1 .le. y .le. 1 and the boundary
c     conditions are
c
c     u = 0 on x = 1,  du/dx = -pi*cos(pi*y) on x = 3
c
c     and u is periodic in y .
c
c     we want to have 48 unknowns in the x-interval and 53 unknowns
c     in the y-interval.
c
      dimension       f(50,53)   ,bda(53)    ,bdb(53)    ,w(1076)    ,
     1                x(48)      ,y(53)
      dimension bdc(1), bdd(1), bdc(1)
c
c     from the dimension statement we get idimf = 50.  also note that
c     w is dimensioned (13 + int(log2(n))*m + 4*n.
c
      idimf = 50
      a = 1.
      b = 3.
      m = 48
      dx = (b-a)/float(m)
      mbdcnd = 2
      c = -1.
      d = 1.
      n = 53
      dy = (d-c)/float(n)
      nbdcnd = 0
      elmbda = -2.
c
c     auxiliary quantities
c
      pi = pimach(dum)
      pisq = pi*pi
c
c     generate and store grid points for computation of boundary data
c     and the right side of the helmholtz equation.
c
      do 101 i=1,m
         x(i) = a+(float(i)-0.5)*dx
  101 continue
      do 102 j=1,n
         y(j) = c+(float(j)-0.5)*dy
  102 continue
c
c     generate boundary data.
c
      do 103 j=1,n
         bda(j) = 0.
         bdb(j) = -pi*cos(pi*y(j))
  103 continue
c
c     bdc and bdd are dummy arguments in this example.
c
c     generate right side of equation.
c
      t = -2.*(pisq+1.)
      do 105 i=1,m
         do 104 j=1,n
            f(i,j) = t*sin(pi*x(i))*cos(pi*y(j))
  104    continue
  105 continue
      call hstcrt (a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,elmbda,f,
     1             idimf,pertrb,ierror,w)
c
c     compute discretization error.  the exact solution is
c
c               u(x,y) = sin(pi*x)*cos(pi*y) .
c
      err = 0.
      do 107 i=1,m
         do 106 j=1,n
            t = abs(f(i,j)-sin(pi*x(i))*cos(pi*y(j)))
            if (t .gt. err) err = t
  106    continue
  107 continue
      print 1001 , ierror,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hstcrt example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 1.26001e-03/
     4        12x,32hrequired length of w array = 884//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5/
     7        12x,28hrequired length of w array =,f4.0)
c
      end
c     compute discretization error.  the exact solution is
