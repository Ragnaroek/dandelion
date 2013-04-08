c fishpk31 from portlib                                  12/30/83
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
c     program to illustrate the use of hstplr to solve the equation
c
c     (1/r)(d/dr)(r*du/dr) + (1/r**2)(d/dtheta)(du/dtheta) = 16*r**2
c
c     on the quarter-disk 0 .lt. r .lt. 1 and 0 .lt. theta .lt. pi/2
c     with the boundary conditions
c
c     u(1,theta) = 1 - cos(4*theta)  for  0 .le. theta .le. pi/2
c
c     and
c
c     (du/dr)(r,0) = (du/dr)(r,pi/2) = 0  for  o .lt. r .lt. 1 .
c
c     note that u at the origin is unspecified.  the exact solution to
c     this problem is
c
c               u(r,theta) = (r**4)(1-cos(4*theta)) .
c
c     we will use 50 unknowns in the r-interval and 48 unknowns in
c     the theta-interval.
c
      dimension       f(51,50)   ,bdb(48)    ,bdc(50)    ,bdd(50)    ,
     1                w(1092)    ,r(50)      ,theta(48)
      dimension bda(1)
c
c     from dimension statement we get value of idimf.  also note that w
c     is dimensioned (13 + log2(n))*m + 4*n .
c
      idimf = 51
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
c     generate and store grid points for the purpose of computing
c     boundary data and the right side of the poisson equation.
c
      do 101 i=1,m
         r(i) = (float(i)-0.5)/50.
  101 continue
      do 102 j=1,n
         theta(j) = (float(j)-0.5)*pi/96.
  102 continue
c
c     generate boundary data.
c
      do 103 j=1,n
         bdb(j) = 1.-cos(4.*theta(j))
  103 continue
      do 104 i=1,m
         bdc(i) = 0.
         bdd(i) = 0.
  104 continue
c
c     bda is a dummy variable.
c
c
c     generate right side of equation.
c
      do 106 i=1,m
         do 105 j=1,n
            f(i,j) = 16.*r(i)**2
  105    continue
  106 continue
      call hstplr (a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,elmbda,f,
     1             idimf,pertrb,ierror,w)
c
c     compute discretization error.  the exact solution is
c
c                u(r,theta) = r**4*(1 - cos(4*theta))
c
      err = 0.
      do 108 i=1,m
         do 107 j=1,n
            z = abs(f(i,j)-r(i)**4*(1.-cos(4.*theta(j))))
            if (z .gt. err) err = z
  107    continue
  108 continue
      print 1001 , ierror,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hstplr example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 1.13038e-03/
     4        12x,33hrequired length of w array = 1042//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5/
     7        12x,28hrequired length of w array =,f5.0)
c
      end
