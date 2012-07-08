      subroutine closefiles

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    close files left open so that R does not hang.
!!

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nrgage      |none          |number of raingage files
!!    ntgage      |none          |number of temperature gage files
!!    petfile     |NA            |potential ET file name (.pet)
!!    rfile(:)    |NA            |rainfall file name (.pcp)
!!    rhfile      |NA            |relative humidity file name (.hmd)
!!    slrfile     |NA            |solar radiation file name (.slr)
!!    tfile(:)    |NA            |temperature file name (.tmp)
!!    wndfile     |NA            |wind speed file name (.wnd)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none           |counter
!!    k           |none           |counter
!!    kk1         |none           |gage code for first dataset in weather file
!!    kk2         |none           |gage code for last dataset in weather file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, kk1, kk2, k
      character (len=80) :: titldum

      do j = 1, nrgage
        if (rfile(j) /= '             ') then
          close (100+j)
        end if
      end do

      do j = 1, ntgage
        if (tfile(j) /= '             ') then
          close (118+j)
        end if
      end do

        if (slrfile /= '             ') then
          close (137)
        end if

        if (rhfile /= '             ') then
          close (138)
        end if

        if (wndfile /= '             ') then
          close (139)
        end if

        if (petfile /= '             ') then
          close (140)
        end if
      close (141)
      close (18)
      close (84)
      close (16)
      close (17)
      close (11123)
      close (9996)
      close (9995)
      close (9999)
      close (5)
      close (6)
      close (0)

      return
      end
      
