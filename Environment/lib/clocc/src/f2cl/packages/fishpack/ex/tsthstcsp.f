c fishpk34 from portlib                                  12/30/83
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
c     this program illustrates the use of subroutine hstcsp to solve
c     the equation
c
c               (1/r**2)(d/dr)(r**2(du/dr)) +
c
c     (1/r**2*sin(theta))(d/dtheta)(sin(theta)(du/dtheta))
c
c                     = 12*(r*cos(theta))**2
c
c     on the rectangle 0 .lt. theta .lt. pi , 0 .lt. r .lt. 1
c     with the boundary conditions
c
c     u(theta,1) = cos(theta)**4 , o .le. theta .le. pi
c
c     and the solution unspecified on the remaining boundaries.
c     we will use 45 unknowns in the theta-interval and 15 unknowns
c     in the r-interval.
c
      dimension       f(47,16)   ,bdd(45)    ,w(615)     ,theta(45)  ,
     1                r(15)      ,cost(45)
      dimension bda(1), bdb(1), bdc(1)
c
c     note that from dimension statement we get that idimf = 47  and
c     that w is dimensioned according to the statement in the
c     description of w.
c
      idimf = 47
      a = 0.
      b = pimach(dum)
c
c     note that b is set to pi using the function pimach as required.
c
      m = 45
      mbdcnd = 9
      dt = (b-a)/float(m)
c
c     define grid points theta(i) and cos(theta(i))
c
      do 101 i=1,m
         theta(i) = a+(float(i)-0.5)*dt
         cost(i) = cos(theta(i))
  101 continue
      c = 0.
      d = 1.
      n = 15
      nbdcnd = 5
      dr = (d-c)/float(n)
c
c     define grid points r(j)
c
      do 102 j=1,n
         r(j) = c+(float(j)-0.5)*dr
  102 continue
c
c     define boundary array bdd.  bda, bdb, and bdc are dummy
c     variables in this example.
c
      do 103 i=1,m
         bdd(i) = cost(i)**4
  103 continue
      elmbda = 0.
c
c     define right side f
c
      do 105 i=1,m
         do 104 j=1,n
            f(i,j) = 12.*(r(j)*cost(i))**2
  104    continue
  105 continue
      intl = 0
      call hstcsp (intl,a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,
     1             elmbda,f,idimf,pertrb,ierror,w)
c
c     compute discretization error.  the exact solution is
c
c     u(theta,r) = (r*cos(theta))**4
c
      err = 0.
      do 107 i=1,m
         do 106 j=1,n
            z = abs(f(i,j)-(r(j)*cost(i))**4)
            if (z .gt. err) err = z
  106    continue
  107 continue
      print 1001 , ierror,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hstcsp example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 5.58432e-03/
     4        12x,32hrequired length of w array = 583//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5/
     7        12x,28hrequired length of w array =,f4.0)
c
      end
c     compute discretization error.  the exact solution is
