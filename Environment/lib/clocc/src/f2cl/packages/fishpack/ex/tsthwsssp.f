c fishpk28 from portlib                                  12/30/83
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
c
c     program to illustrate the use of hwsssp
c
      dimension       f(19,73)   ,bdtf(73)   ,sint(19)   ,sinp(73)   ,
     1                w(600)
      dimension bdts(1), bdps(1), bdpf(1)
c
      pi = pimach(dum)
      ts = 0
      tf = pi/2.
      m = 18
      mbdcnd = 6
      ps = 0
      pf = pi+pi
      n = 72
      nbdcnd = 0
      elmbda = 0.
      idimf = 19
c
c     generate sines for use in subsequent computations
c
      dtheta = tf/float(m)
      mp1 = m+1
      do 101 i=1,mp1
         sint(i) = sin(float(i-1)*dtheta)
  101 continue
      dphi = (pi+pi)/float(n)
      np1 = n+1
      do 102 j=1,np1
         sinp(j) = sin(float(j-1)*dphi)
  102 continue
c
c     compute right side of equation and store in f
c
      do 104 j=1,np1
         do 103 i=1,mp1
            f(i,j) = 2.-6.*(sint(i)*sinp(j))**2
  103    continue
  104 continue
c
c     store derivative data at the equator
c
      do 105 j=1,np1
         bdtf(j) = 0.
  105 continue
c
      call hwsssp (ts,tf,m,mbdcnd,bdts,bdtf,ps,pf,n,nbdcnd,bdps,bdpf,
     1             elmbda,f,idimf,pertrb,ierror,w)
c
c     compute discretization error. since problem is singular, the
c     solution must be normalized.
c
      err = 0
      do 107 j=1,np1
         do 106 i=1,mp1
            z = abs(f(i,j)-(sint(i)*sinp(j))**2-f(1,1))
            if (z .gt. err) err = z
  106    continue
  107 continue
c
      iw = int(w(1))
      print 1001 , ierror,err,iw
      stop
c
c
 1001 format (1h1,20x,25hsubroutine hwsssp example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 3.38107e-03/
     4        12x,32hrequired length of w array = 600//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5 /
     7        12x,28hrequired length of w array =,i4)
c
      end
c
