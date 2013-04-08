c fishpk35 from portlib                                  12/30/83
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
c        program to illustrate the use of subroutine hw3crt to
c     solve the helmholtz equation
c
c     (d/dx)(du/dx) + (d/dy)(du/dy) + (d/dz)(du/dz) - 3u
c
c       =  4x**2*(3-x**2):sin(y)*cos(z)
c
c     on the parallelepiped 0 .lt. x .lt. 1, 0 .lt. y .lt. 2*pi,
c     0 .lt. z .lt. pi/2 with the boundary conditions
c
c     u(0,y,z) = 0
c                           0 .le. y .le. 2*pi , 0 .le. z .le. pi/2
c     u(1,y,z) = sin(y)*cos(z)
c
c     u periodic in y,
c
c     u(x,y,0) = x**4*sin(y)
c                           0 .le. x .le. 1 , 0 .le. y .le. 2*pi
c     (du/dx)(x,y,pi/2) = -x**4*sin(y)
c
c     using a finite difference grid with panel widths
c
c     deltax (=dx) = 1/10 , deltay (=dy) = 1/40 , deltaz (=dz) = 1/15.
c
c        the exact solution of this problem is
c
c     u(x,y,z) = x**4*sin(y)*cos(z) .
c
      dimension       f(11,41,16),bdzf(11,41),w(370)     ,x(11)      ,
     1                y(41)      ,z(16)
      real dum(1)
c
c        from the description of the problem given above, we define
c     the following quantities
c
      elmbda = -3.
      xs = 0.
      xf = 1.
      lbdcnd = 1
      ys = 0.
      pi = pimach(dum0)
      yf = 2.*pi
      mbdcnd = 0
      zs = 0.
      zf = pi/2.
      nbdcnd = 2
      l = 10
      m = 40
      n = 15
c
c     from the dimension statement above we define
c
      ldimf = 11
      mdimf = 41
c
c     also note that w has been dimensioned
c        30+l+m+5*n+max(l,m,n)+7*(int((l+1)/2)+int((m+1)/2))
c      = 30+10+40+75+40+7*(5+20) = 370
c     we define the grid points for later use.
c
      lp1 = l+1
      dx = (xf-xs)/float(l)
      do 101 i=1,lp1
         x(i) = xs+float(i-1)*dx
  101 continue
      mp1 = m+1
      dy = (yf-ys)/float(m)
      do 102 j=1,mp1
         y(j) = ys+float(j-1)*dy
  102 continue
      np1 = n+1
      dz = (zf-zs)/float(n)
      do 103 k=1,np1
         z(k) = zs+float(k-1)*dz
  103 continue
c
c     we define the array of derivative boundary values.
c
      do 105 i=1,lp1
         do 104 j=1,mp1
            bdzf(i,j) = -x(i)**4*sin(y(j))
  104    continue
  105 continue
c
c     note that for this example all other boundary arrays are
c     dummy variables.
c     we define the function boundary values in the f array.
c
      do 107 j=1,mp1
         do 106 k=1,np1
            f(1,j,k) = 0.
            f(lp1,j,k) = sin(y(j))*cos(z(k))
  106    continue
  107 continue
      do 109 i=1,lp1
         do 108 j=1,mp1
            f(i,j,1) = x(i)**4*sin(y(j))
  108    continue
  109 continue
c
c     we now define the values of the right side of the helmholtz
c     equation.
c
      do 112 i=2,l
         do 111 j=1,mp1
            do 110 k=2,np1
               f(i,j,k) = 4.*x(i)**2*(3.-x(i)**2)*sin(y(j))*cos(z(k))
  110       continue
  111    continue
  112 continue
c
c     call hw3crt to generate and solve the finite difference equation.
c
c      call hw3crt (xs,xf,l,lbdcnd,bdxs,bdxf,ys,yf,m,mbdcnd,bdys,bdyf,
c     1             zs,zf,n,nbdcnd,bdzs,bdzf,elmbda,ldimf,mdimf,f,
c     2             pertrb,ierror,w)
      call hw3crt (xs,xf,l,lbdcnd,dum,dum,ys,yf,m,mbdcnd,dum,dum,
     1             zs,zf,n,nbdcnd,dum,bdzf,elmbda,ldimf,mdimf,f,
     2             pertrb,ierror,w)
c
c     compute discretization error.  the exact solution to the
c     problem is
c
c        u(x,y,z) = x**4*sin(y)*cos(z)
c
      err = 0.
      do 115 i=1,lp1
         do 114 j=1,mp1
            do 113 k=1,np1
               t = abs(f(i,j,k)-x(i)**4*sin(y(j))*cos(z(k)))
               if (t .gt. err) err = t
  113       continue
  114    continue
  115 continue
      print 1001 , ierror,err
      stop
c
 1001 format (1h1,20x,25hsubroutine hw3crt example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/
     3        18x,34hdiscretization error = 9.64802e-03//
     4        10x,32hthe output from your computer is//
     5        32x,8hierror =,i2/18x,22hdiscretization error =,e12.5)
c
      end
c     problem is
