      subroutine volq

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Call subroutines to calculate the current day's CN for the HRU and
!!    to calculate surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: surq_daycn, surq_breakcn, surq_greenampt, dir_rnff
!!    SWAT: surq_hourly

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      character*1 wbflag

!! Compute surface runoff for day
      read(snam(ihru),'(a1)') wbflag
      if(wbflag .eq. "1") then
         call surq_waterbalance
      else
      select case (ievent)
        case (0)
          call surq_daycn
        case default
          call surq_greenampt
        !  call dir_rnff
        !case (3)
        !  call surq_hourly
      end select
      endif

      return
      end
