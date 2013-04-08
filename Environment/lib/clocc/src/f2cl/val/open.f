      program tstopn
c     Some random tests for opening and reading from files
      integer i, istat
      integer a(4)
      
      open(unit = 1, file = 'fort.10', iostat = istat, status = 'old')
      if (istat .eq. 0) then
         print *, 'Unexpectedly opened fort.10!'
      else
         print *, 'As expected, fort.10 could not be opened'
      endif

      open(1, file = 'fort.10', err = 10, status = 'old')
      print *, 'Err option did not work!'
      goto 20
   10 continue
      print *, 'Err option worked because open failed.'
   20 continue

      open(unit = 2, file = 'output', iostat = istat)
      if (istat .ne. 0) then
         print *, 'Could not open output!'
      else
         open(unit = 1, file = 'fort.1', iostat = istat)
         if (istat .ne. 0) then
            print *, 'Could not open fort.1!'
         else
            rewind 1
            read(1, *) (a(i), i = 1,4)
            print *, (a(i), i = 1,4)
            write(2, *) (a(i), i = 1,4)
            close(1)
            close(unit = 2)
         endif
      endif
      end
      
      
