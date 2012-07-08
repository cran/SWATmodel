      subroutine rchuse
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine removes water from reach for consumptive water use

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1       |none          |reach number
!!    i_mo        |none          |month of simulation
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    wurch(:,:)  |10^4 m^3/day  |average daily water removal from the reach
!!                               |for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jrch        |none          |HRU number
!!    wtrin       |m^3 H2O       |water outflow from reach prior to
!!                               |subtracting irrigation diversions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, ii
      real :: wtrin

      jrch = 0
      jrch = inum1

      wtrin = 0.
      wtrin = rtwtr

      rtwtr = rtwtr - wurch(i_mo,jrch) * 10000.
      if (rtwtr < 0.) rtwtr = 0.

      if (ievent > 2) then
        do ii = 1, 24
          hrtwtr(ii) = hrtwtr(ii) - wurch(i_mo,jrch) * 10000. / 24.
          if (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
        end do
      end if

      if (wtrin /= rtwtr .and. wtrin > 0.01) then
        sedrch = sedrch * rtwtr / wtrin
        if (sedrch < 0.) sedrch = 0.
        if (ievent > 2) then
          do ii = 1, 24
            hsedyld(ii) = hsedyld(ii) * rtwtr / wtrin
            if (hrtwtr(ii) == 0.) hsedyld(ii) = 0.
          end do
        end if
      end if

      return
      end
