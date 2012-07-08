      subroutine readatmodep
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the atmospheric deposition values
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      character (len=80) :: titldum
      integer :: eof 
  
      eof = 0

!!    Atmosperic deposition filename present in file.cio
      if (atmofile /= '             ') then
        open (127,file=atmofile)
        do iii = 1, 5
          read (127,5101) titldum
        end do
       do isub = 1, subtot
        read (127,1000,iostat=eof)  rammo_sub(isub), rcn_sub(isub)
        if (eof < 0) then
          rcn_sub(isub) = rcn
          exit
        end if 
!         if (rcn_sub(isub) < 1.e-6) rcn_sub(isub) = rcn
!!    default values for mike van liew
          if (rcn_sub(isub) < 1.e-6) rcn_sub(isub) = rcn_sub_bsn
!!    default values for mike van liew
       end do
      else
!!    no filename present in file.cio - set defaults
       rammo_sub = 0.
       rcn_sub = rcn


      end if


1000  format (8x,2f10.3)
5101  format (a80)
      return
      end
