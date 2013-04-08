c fishpk32 from portlib                                  12/30/83
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
c     program to illustrate the use of hstcyl to solve the equation
c
c    (1/r)(d/dr)(r*du/dr) + (d/dz)(du/dz) = (2*r*z)**2*(4*z**2 + 3*r**2)
c
c     on the rectangle 0 .lt. r .lt. 1 , 0 .lt. z .lt. 1 with the
c     boundary conditions
c
c     (du/dr)(1,z) = 4*z**2  for  0 .le. z .le. 1
c
c     and
c
c     (du/dz)(r,0) = 0 and (du/dz)(r,1) = 4*r**2  for  0 .le. r .le. 1 .
c
c     the solution to this problem is not unique.  it is a
c     one-parameter family of solutions given by
c
c            u(r,z) = (r*z)**4 + arbitrary constant .
c
c     the r-interval will contain 50 unknowns and the z-interval will
c     contain 52 unknowns.
c
      dimension       f(51,52)   ,bdb(52)    ,bdc(50)    ,bdd(50)    ,
     1                w(1108)    ,r(50)      ,z(52)
      dimension bda(1)
c
c     from dimension statement we get value of idimf.  also note that w
c     is dimensioned (13 + int(log2(n)))*m + 4*n .
c
      idimf = 51
      a = 0.
      b = 1.
      m = 50
      mbdcnd = 6
      c = 0.
      d = 1.
      n = 52
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
         z(j) = (float(j)-0.5)/52.
  102 continue
c
c     generate boundary data.
c
      do 103 j=1,n
         bdb(j) = 4.*z(j)**4
  103 continue
      do 104 i=1,m
         bdc(i) = 0.
         bdd(i) = 4.*r(i)**4
  104 continue
c
c     bda is a dummy variable.
c
c     generate right side of equation.
c
      do 106 i=1,m
         do 105 j=1,n
            f(i,j) = 4.*r(i)**2*z(j)**2*(4.*z(j)**2+3.*r(i)**2)
  105    continue
  106 continue
      call hstcyl (a,b,m,mbdcnd,bda,bdb,c,d,n,nbdcnd,bdc,bdd,elmbda,f,
     1             idimf,pertrb,ierror,w)
c
c     compute discretization error by minimizing over all a the function
c     norm(f(i,j) - a*1 - u(r(i),z(j))).  the exact solution is
c                u(r,z) = (r*z)**4 + arbitrary constant.
c
      x = 0.
      do 108 i=1,m
         do 107 j=1,n
            x = x+f(i,j)-(r(i)*z(j))**4
  107    continue
  108 continue
      x = x/float(m*n)
      do 110 i=1,m
         do 109 j=1,n
            f(i,j) = f(i,j)-x
  109    continue
  110 continue
      err = 0.
      do 112 i=1,m
         do 111 j=1,n
            x = abs(f(i,j)-(r(i)*z(j))**4)
            if (x .gt. err) err = x
  111    continue
  112 continue
      print 1001 , ierror,pertrb,err,w(1)
      stop
c
 1001 format (1h1,20x,25hsubroutine hstcyl example///
     1        10x,46hthe output from the ncar control data 7600 was//
     2        32x,10hierror = 0/32x,20hpertrb =-4.43114e-04/
     3        18x,34hdiscretization error = 7.52796e-05/
     4        12x,32hrequired length of w array = 958//
     5        10x,32hthe output from your computer is//
     6        32x,8hierror =,i2/32x,8hpertrb =,e12.5/
     7        18x,22hdiscretization error =,e12.5/
     8        12x,28hrequired length of w array =,f4.0)
c
      end
