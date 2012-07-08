      subroutine sensin(dt,nintval,iseed)
!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      integer nintval, iseed

      common /iopar/ in,iprii

c
      write (9999,*) ' ENTER THE SENSITIVITY ANALYSIS SUBROUTINE --- '
c
c
c  INITIALIZE I/O VARIABLES



c
      write (iprii,*)'================================================='

      write(iprii,700)
  700 format(10x,'sensitivity analysis inputs and results')
      write (iprii,*)'================================================='

      read(in,*) nintval
      read(in,*) dt
      read(in,*) iseed
      
      if (iseed .eq. 0) iseed = 1969
      if (dt .le. 0.or.dt.gt.0.5) dt = 1./nintval/2.
        write (iprii,*)'==============================================='
      write (iprii,*)'Control parameter are :'             
      write (iprii,*) 'number of intervals in Latin Hypercube = ',
     *       nintval
      write (iprii,*) ' parameter change for OAT  = ', dt
      write (iprii,*) ' random seed number = ', iseed
        write (iprii,*)'==============================================='

c
C  END OF SUBROUTINE SENSIN
      return
      end
