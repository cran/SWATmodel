      subroutine automet
      use parm
      
      integer iobj,isens            
c      open(19030, file='swatfilenames.prn')

      open (18010, file='changepar.dat')
      call telpar(nopt)
      rewind(18010)
      open(18016, file='objmet.dat')
      open(18017, file='responsmet.dat')
      call telobjresp(iobj,isens)        
      rewind(18016)
      rewind(18017)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c               BEGIN SENSITIVITY PART BY AVG
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if (iclb.eq.1) then
      write(9999,*) 'starting sensitivity analysis...'      
      write(9999,*) ' written by Ann van Griensven'
      write(9999,*) '         at University of California Riverside'

      call sensmain(nopt,iobj,isens)
    
      write(9999,*) 'sensitivity analysis completed!'      
      end if      

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c               BEGIN OPTIMISATION AND UNCERTAINTY
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        continue
        if (iclb.eq.2.or.iclb.eq.3) then
      write(9999,*) ' starting PARASOL method...'
      write(9999,*) ' written by Ann van Griensven'
      write(9999,*) '         at University of California Riverside'
        call parasol(nopt,iobj,isens)
c       end autocalibration section
      write(9999,*) 'PARASOL method completed!'

        end if



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c               BEGIN RERUN
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if (iclb.eq.4) then
        write(9999,*) 'RERUN best parameter file...'

      call rerunfile(nopt,iobj,isens)
c       end autocalibration section
        end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c               BEGIN BATCHRUN PART BY AVG
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
1003        if (iclb.eq.5) then
      write(9999,*) 'rerun good parameter sets...'      
      write(9999,*) ' written by Ann van Griensven'
      write(9999,*) '         at University of California Riverside'
      call batchmain(nopt)
      write(9999,*) 'Batchruns completed!'
      end if
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c              RERUN UNCERTAINTY LOOP BY AVG
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
1004        continue
        if (iclb.eq.6) then
        write(9999,*) 'rerun uncertainty analysis'
       call rerunPS(nopt,iobj,isens)
c       end autocalibration section
        end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if (iclb.eq.8) then
        write(9999,*) 'start sunglasses method'
!       call sunglasses(iclb,nopt)
        call sunglasses(nopt,iobj,isens)
        end if


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if (iclb.eq.9) then
        write(9999,*) 'rerun uncertainty analysis'
       call sunglasrerun(nopt,iobj,isens)
              end if
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      return 
      end
