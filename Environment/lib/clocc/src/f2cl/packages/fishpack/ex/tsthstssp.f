c fishpk33 from portlib                                  12/30/83
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
c     program to illustrate the use of hstssp to solve poisson"s
c     equation
c
c             (1/sin(theta))(d/dtheta)(sin(theta)*du/dtheta) +
c
c    (1/sin(theta)**2)(d/dphi)(du/dphi) = 2 - 6*(sin(theta)*sin(phi))**2
c     on the northern hemisphere subject to equatorial symmetry, i.e.
c     the derivative of the solution at theta = pi/2 is zero.  a
c     5-degree grid is to be used.
c
c     the exact solution is not unique.  any function of the form
c
c           u(theta,phi) = (sin(theta)*sin(phi))**2 + constant
c
c     is a solution.
c
      dimension       f(18,72)   ,bdb(72)    ,sint(18)   ,sinp(72)   ,
     1                w(630)
      dimension bda(1), bdc(1), bdd(1)
c
c     the value of idimf is the first dimension of f. w is dimensioned
c     (13 + int(log2(n)))*m + 4*n
c
      pi = pimach(dum)
      a = 0.
      b = pi/2.
      m = 18
      mbdcnd = 6
      c = 0.
      d = 2.*pi
      n = 72
      nbdcnd = 0
      elmbda = 0.
      idimf = 18
c
c     generate sines for use in subsequent computations
c
      dtheta = b/float(m)
      do 101 i=1,m
         sint(i) = sin((float(i)-0.5)*dtheta)
  101 continue
      dphi = d/float(n)
      do 102 j=1,n
         sinp(j) = sin((float(j)-0.5)*dphi)
  102 continue
c
c     compute right side of equation and store in f
c
      do 104 j=1,n
         do 103 i=1,m
            f(i,j) = 2.-6.*(sint(i)*sinp(j))**2
  103    continue
  104 continue
c
c     store derivative data at the equator
c
      do 105 j=1,n
         bdb(j) = 0.
  105 continue
c
c     bda, bdc, and bdd are dummy variables.
c
      call hstssp (a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,elmbda,f,
     1             idimf,pertrb,ierror,w)
c
c     compute discretization error. since problem is singular, the
c     solution must be normalized.
c
      err = 0.
      do 107 j=1,n
         do 106 i=1,m
            z = abs(f(i,j)-(sint(i)*sinp(j))**2-f(1,1))
            if (z .gt. err) err = z
  106    continue
  107 continue
c
      print 1001 , ierror,pertrb,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hstssp example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/32x,20hpertrb = 6.35830e-04/
     3        18x,34hdiscretization error = 3.37523e-03/
     4        12x,32hrequired length of w array = 540//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/32x,8hpertrb =,e12.5/
     7        18x,22hdiscretization error =,e12.5/
     8        12x,28hrequired length of w array =,f4.0)
c
      end
c     solution must be normalized.
