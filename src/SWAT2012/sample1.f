      subroutine sample1(isample,k,iseed)
      

c      This subroutine samples a sequence of m integers without repetition. 
      integer iseed, isample, k
      
      dimension isample(k)
      dimension ihelp(k)

      ihelp=0
      do ii=1,k
      n=k-ii+1
      isamp=1.+int(n*ran(iseed))
      jj=1
      jk=1
      iisamp=isamp
      do while (jj<=iisamp)
      if (ihelp(jk).eq.0)then 
      jj=jj+1
      else
      isamp=isamp+1
      end if
      jk=jk+1
      end do
      ihelp(isamp)=1
      isample(ii)=isamp
      end do
      return
      end
