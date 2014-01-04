      subroutine  sample(ip,m,iseed, bbound,dt)



c      Latin Hypercube sampling program
c      Written by Ann van Griensven at UC Riverside
c
c
c      ip = number of parameters
c     m  = number of intervals
c      X= matrix with ip parameter values for m samples
c      IX stores positions within hypercube intervals      
c
c
c
      real*8 x(m,ip), bbound(2,200),dt
      integer ix(m,ip), ip, m, iseed
      integer isample(m)
      write (9999,*) 'starting sampling       ....'

      do ii=1,ip
      call sample1(isample,m,iseed)      
      do jj=1,m
      ix(jj,ii)=isample(jj)
      x(jj,ii)=(isample(jj)-1.+ran(iseed))/m
      end do
      end do
      do jj=1,m
      write(18018,1) isample(jj),(x(jj,kk), kk=1,ip)
1      format(i4,100f12.5)
      end do

      call oat(x,m,ip, bbound,dt,iseed)
      return
      end
