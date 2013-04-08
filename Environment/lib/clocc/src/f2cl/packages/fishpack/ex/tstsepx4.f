c fishpk36 from portlib                                  12/30/83      
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
c     *                  (version 3.1 , october 1980)                 *
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
c     example showing the use of sepx4 to solve the elliptic pde       
c     (x+1)**2*uxx+2*(x+1)*ux+uyy-x*u=g(x,y) on the region             
c     0.le.x.le.1, 0.le.y.le.1  with specified boundary conditions     
c     at y=0,1 and mixed boundary conditions of the form               
c     ux(0,y)+u(0,y), ux(1,y)+u(1,y) at x=0,1.                         
c     the approximation is generated on a uniform 33 by 33 grid.       
c     the exact solution u(x,y)=(x*y)**3+1 is used to set the          
c     right hand side, boundary conditions, and compute  second and    
c     fourth order discretization error                                
c     the exact work space length required is 1005 words.  this        
c     was determined by a previous call to sepx4 and print out of      
c     w(1).                                                            
c                                                                      
      dimension       usol(33,33),grhs(33,33),bda(33)    ,bdb(33)    , 
     1                w(1024)                                          
      real dum(1)
      external cofx4                                                   
c                                                                      
c     define arithmetic functions giving exact solution                
c                                                                      
      ue(s,t)=(s*t)**3+1.0                                             
      uxe(s,t)=3.0*s**2*t**3                                           
      uxxe(s,t)=6.0*s*t**3                                             
      uye(s,t)=3.0*s**3*t**2                                           
      uyye(s,t)=6.0*s**3*t                                             
c                                                                      
c     set limits on region                                             
c                                                                      
      a = 0.0                                                          
      b = 1.0                                                          
      c = 0.0                                                          
      d = 1.0                                                          
c                                                                      
c     set grid size                                                    
c                                                                      
      m = 32                                                           
      n = 32                                                           
      dlx = (b-a)/float(m)                                             
      dly = (d-c)/float(n)                                             
      nx = m+1                                                         
      ny = n+1                                                         
          do 102 i=1,nx                                                
          x = a+float(i-1)*dlx                                         
c                                                                      
c     set specified boundary conditions at y=c,d                       
c                                                                      
          usol(i,1) = ue(x,c)                                          
          usol(i,ny) = ue(x,d)                                         
          call cofx4 (x,af,bf,cf)                                      
              do 101 j=1,ny                                            
              y = c+float(j-1)*dly                                     
c                                                                      
c     set right hand side                                              
c                                                                      
              grhs(i,j) = af*uxxe(x,y)+bf*uxe(x,y)+cf*ue(x,y)+uyye(x,y)
  101         continue                                                 
  102     continue                                                     
c                                                                      
c     set mixed boundary conditions at x=a,b                           
c                                                                      
      alpha = 1.0                                                      
      beta = 1.0                                                       
          do 103 j=1,ny                                                
          y = c+float(j-1)*dly                                         
          bda(j) = uxe(a,y)+alpha*ue(a,y)                              
          bdb(j) = uxe(b,y)+beta*ue(b,y)                               
  103     continue                                                     
c                                                                      
c     set boundary swithces                                            
c                                                                      
      mbdcnd = 3                                                       
      nbdcnd = 1                                                       
c                                                                      
c     set first dimension of usol,grhs and work space length           
c                                                                      
      idmn = 33                                                        
      w(1) = 1024.                                                     
c                                                                      
c     obtain second order approximation                                
c                                                                      
      iorder = 2                                                       
      call sepx4 (iorder,a,b,m,mbdcnd,bda,alpha,bdb,beta,c,d,n,nbdcnd, 
     1            dum,dum,cofx4,grhs,usol,idmn,w,pertrb,ierror)        
c                                                                      
c     compute second order discretization error (relative)             
c     also reset specified boundaries and right hand side.             
c                                                                      
      err = 0.0                                                        
          do 105 i=1,nx                                                
          x = a+float(i-1)*dlx                                         
          usol(i,1) = ue(x,c)                                          
          usol(i,ny) = ue(x,d)                                         
          call cofx4 (x,af,bf,cf)                                      
              do 104 j=1,ny                                            
              y = c+float(j-1)*dly                                     
              err = amax1(err,abs((usol(i,j)-ue(x,y))/ue(x,y)))        
              grhs(i,j) = af*uxxe(x,y)+bf*uxe(x,y)+cf*ue(x,y)+uyye(x,y)
  104         continue                                                 
  105     continue                                                     
      err2=err                                                         
c                                                                      
c     obtain fourth order approximation                                
c                                                                      
      iorder = 4                                                       
      call sepx4 (iorder,a,b,m,mbdcnd,bda,alpha,bdb,beta,c,d,n,nbdcnd, 
     1            dum,dum,cofx4,grhs,usol,idmn,w,pertrb,ierror)        
c                                                                      
c     compute fourth order discretization error (relative)             
c                                                                      
      err = 0.0                                                        
          do 107 j=1,ny                                                
          y = c+float(j-1)*dly                                         
              do 106 i=1,nx                                            
              x = a+float(i-1)*dlx                                     
              err = amax1(err,abs((usol(i,j)-ue(x,y))/ue(x,y)))        
  106         continue                                                 
  107     continue                                                     
      err4=err                                                         
      iw = int(w(1)+0.5)                                               
      print 1001,ierror,err2,err4,iw                                   
 1001 format(1h1,20x,25hsubroutine sepx4  example ///                  
     120x,46hthe output from the ncar control data 7600 was //         
     220x,10hierror = 0 /                                              
     320x,48hsecond order discretization error =  1.5985e-04  /        
     420x,48hfourth order discretization error =  1.85749e-06  /       
     520x,33hrequired length of w array = 1024 //                      
     620x, 32hthe output from your computer is //                      
     720x, 8hierror = ,i2 /                                             
     820x,36hsecond order discretization error = ,e12.5 /               
     920x,36hfourth order discretization error = ,e12.5 /               
     920x,29hrequired length of w array = ,i5)                          
c                                                                      
      end                                                              
