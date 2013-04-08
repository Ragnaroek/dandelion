c fishpk26 from portlib                                  12/30/83
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
c          program to illustrate the use of subroutine hwsplr to solve
c     the equation
c
c     (1/r)(d/dr)(r*(du/dr)) + (1/r**2)(d/dtheta)(du/dtheta) = 16*r**2
c
c     on the quarter-disk 0 .lt. r .lt. 1, 0 .lt. theta .lt. pi/2 with
c     with the boundary conditions
c
c     u(1,theta) = 1 - cos(4*theta), 0 .le. theta .le. 1
c
c     and
c
c     (du/dtheta)(r,0) = (du/dtheta)(r,pi/2) = 0,  0 .le. r .le. 1.
c
c     (note that the solution u is unspecified at r = 0.)
c          the r-interval will be divided into 50 panels and the
c     theta-interval will be divided into 48 panels.
c
      dimension       f(100,50)  ,bdc(51)    ,bdd(51)    ,w(1114)    ,
     1                r(51)      ,theta(49)
      dimension bda(1), bdb(1)
c
c     from dimension statement we get value of idimf.  also note that w
c     is dimensioned 4*(n+1) + (13 + int(log2(n+1)))*(m+1) .
c
      idimf = 100
      a = 0.
      b = 1.
      m = 50
      mbdcnd = 5
      c = 0.
      pi = pimach(dum)
      d = pi/2.
      n = 48
      nbdcnd = 3
      elmbda = 0.
c
c     auxiliary quantities.
c
      mp1 = m+1
      np1 = n+1
c
c     generate and store grid points for the purpose of computing
c     boundary data and the right side of the poisson equation.
c
      do 101 i=1,mp1
         r(i) = float(i-1)/50.
  101 continue
      do 102 j=1,np1
         theta(j) = float(j-1)*pi/96.
  102 continue
c
c     generate boundary data.
c
      do 103 i=1,mp1
         bdc(i) = 0.
         bdd(i) = 0.
  103 continue
c
c     bda and bdb are dummy variables.
c
      do 104 j=1,np1
         f(mp1,j) = 1.-cos(4.*theta(j))
  104 continue
c
c     generate right side of equation.
c
      do 106 i=1,m
         do 105 j=1,np1
            f(i,j) = 16.*r(i)**2
  105    continue
  106 continue
      call hwsplr (a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,elmbda,f,
     1             idimf,pertrb,ierror,w)
c
c     compute discretization error.  the exact solution is
c                u(r,theta) = r**4*(1 - cos(4*theta))
c
      err = 0.
      do 108 i=1,mp1
         do 107 j=1,np1
            z = abs(f(i,j)-r(i)**4*(1.-cos(4.*theta(j))))
            if (z .gt. err) err = z
  107    continue
  108 continue
      print 1001 , ierror,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hwsplr example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 6.19134e-04/
     4        12x,32hrequired length of w array = 882//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5/
     7        12x,28hrequired length of w array =,f4.0)
c
      end
