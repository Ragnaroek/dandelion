c fishpk27 from portlib                                  12/30/83
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
c          program to illustrate the use of subroutine hwscyl to solve
c     the equation
c
c     (1/r)(d/dr)(r*(du/dr)) + (d/dz)(du/dz)
c
c     = (2*r*z)**2*(4*z**2 + 3*r**2)
c
c     on the rectangle 0 .lt. r .lt. 1, 0 .lt. z .lt. 1 with the
c     boundary conditions
c
c     u(0,z) unspecified
c                                            0 .le. z .le. 1
c     (du/dr)(1,z) = 4*z**4
c
c     and
c
c     (du/dz)(r,0) = 0
c                                            0 .le. r .le. 1
c     (du/dz)(r,1) = 4*r**4 .
c
c          the r-interval will be divided into 50 panels and the
c     z-interval will be divided into 100 panels.
c
      dimension       f(75,105)  ,bda(101)   ,bdb(101)   ,bdc(51)    ,
     1                bdd(51)    ,w(1373)    ,r(51)      ,z(101)
c
c     from dimension statement we get value of idimf.  also note that w
c     is dimensioned 4*(n+1) + (13 + int(log2(n+1)))*(m+1) .
c
      idimf = 75
      a = 0.
      b = 1.
      m = 50
      mbdcnd = 6
      c = 0.
      d = 1.
      n = 100
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
         z(j) = float(j-1)/100.
  102 continue
c
c     generate boundary data.
c
      do 103 j=1,np1
         bdb(j) = 4.*z(j)**4
  103 continue
      do 104 i=1,mp1
         bdc(i) = 0.
         bdd(i) = 4.*r(i)**4
  104 continue
c
c     bda is a dummy variable.
c
c
c     generate right side of equation.
c
      do 106 i=1,mp1
         do 105 j=1,np1
            f(i,j) = 4.*r(i)**2*z(j)**2*(4.*z(j)**2+3.*r(i)**2)
  105    continue
  106 continue
      call hwscyl (a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,elmbda,f,
     1             idimf,pertrb,ierror,w)
c
c     compute discretization error by minimizing over all a the function
c     norm(f(i,j) - a*1 - u(r(i),z(j))).  the exact solution is
c                u(r,z) = (r*z)**4 + arbitrary constant.
c
      x = 0.
      do 108 i=1,mp1
         do 107 j=1,np1
            x = x+f(i,j)-(r(i)*z(j))**4
  107    continue
  108 continue
      x = x/float(np1*mp1)
      do 110 i=1,mp1
         do 109 j=1,np1
            f(i,j) = f(i,j)-x
  109    continue
  110 continue
      err = 0.
      do 112 i=1,mp1
         do 111 j=1,np1
            x = abs(f(i,j)-(r(i)*z(j))**4)
            if (x .gt. err) err = x
  111    continue
  112 continue
      print 1001 , ierror,pertrb,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hwscyl example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/32x,20hpertrb = 2.26734e-04/
     3        18x,34hdiscretization error = 3.73672e-04/
     4        12x,33hrequired length of w array = 1118//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/32x,8hpertrb =,e12.5/
     7        18x,22hdiscretization error =,e12.5/
     8        12x,28hrequired length of w array =,f5.0)
c
      end
