c fishpk29 from portlib                                  12/30/83
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
c     program to illustrate the use of hwscsp
c
      dimension       f(48,33)   ,bdtf(33)   ,w(775)     ,r(33)      ,
     1                theta(48)
      dimension bdts(1), bdrs(1), bdrf(1)
c
      pi = pimach(dum)
      intl = 0
      ts = 0.
      tf = pi/2.
      m = 36
      mbdcnd = 6
      rs = 0.
      rf = 1.
      n = 32
      nbdcnd = 5
      elmbda = 0.
      idimf = 48
c
c     generate and store grid points for the purpose of computing the
c     boundary data and the right side of the equation.
c
      mp1 = m+1
      dtheta = tf/float(m)
      do 101 i=1,mp1
         theta(i) = float(i-1)*dtheta
  101 continue
      np1 = n+1
      dr = 1./float(n)
      do 102 j=1,np1
         r(j) = float(j-1)*dr
  102 continue
c
c     generate normal derivative data at equator
c
      do 103 j=1,np1
         bdtf(j) = 0.
  103 continue
c
c     compute boundary data on the surface of the sphere
c
      do 104 i=1,mp1
         f(i,n+1) = cos(theta(i))**4
  104 continue
c
c     compute right side of equation
c
      do 106 i=1,mp1
         ci4 = 12.*cos(theta(i))**2
         do 105 j=1,n
            f(i,j) = ci4*r(j)**2
  105    continue
  106 continue
c
      call hwscsp (intl,ts,tf,m,mbdcnd,bdts,bdtf,rs,rf,n,nbdcnd,bdrs,
     1             bdrf,elmbda,f,idimf,pertrb,ierror,w)
c
c     compute discretization error
c
      err = 0.
      do 108 i=1,mp1
         ci4 = cos(theta(i))**4
         do 107 j=1,n
            z = abs(f(i,j)-ci4*r(j)**4)
            if (z .gt. err) err = z
  107    continue
  108 continue
      iw = int(w(1))
      print 1001 , ierror,err,iw
c
c     the following program illustrates the use of hwscsp to solve
c     a three dimensional problem which has longitudnal dependence
c
      mbdcnd = 2
      nbdcnd = 1
      dphi = pi/72.
      elmbda = -2.*(1.-cos(dphi))/dphi**2
c
c     compute boundary data on the surface of the sphere
c
      do 109 i=1,mp1
         f(i,n+1) = sin(theta(i))
  109 continue
c
c     compute right side of the equation
c
      do 111 j=1,n
         do 110 i=1,mp1
            f(i,j) = 0.
  110    continue
  111 continue
c
      call hwscsp (intl,ts,tf,m,mbdcnd,bdts,bdtf,rs,rf,n,nbdcnd,bdrs,
     1             bdrf,elmbda,f,idimf,pertrb,ierror,w)
c
c     compute discretization error   (fourier coefficients)
c
      err = 0
      do 113 i=1,mp1
         si = sin(theta(i))
         do 112 j=1,np1
            z = abs(f(i,j)-r(j)*si)
            if (z .gt. err) err = z
  112    continue
  113 continue
c
      iw = int(w(1))
      print 1002 , ierror,err,iw
      stop
c
 1001 format (1h1,20x,27hsubroutine hwscsp example 1///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 7.99842e-04/
     4        12x,32hrequired length of w array = 775//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5/
     7        12x,28hrequired length of w array =,i4)
 1002 format (1h1,20x,27hsubroutine hwscsp example 2///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 5.86824e-05/
     4        12x,32hrequired length of w array = 775//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5/
     7        12x,28hrequired length of w array =,i4)
c
      end
