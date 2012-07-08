      subroutine rtmusk
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes a daily flow through a reach using the
!!    Muskingum method

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_k(2,:)   |mm/hr         |effective hydraulic conductivity of
!!                               |main channel alluvium
!!    ch_l2(:)    |km            |length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    chside(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    curyr       |none          |current year of simulation (consecutive)
!!    evrch       |none          |Reach evaporation adjustment factor.
!!                               |Evaporation from the reach is multiplied by
!!                               |EVRCH. This variable was created to limit the
!!                               |evaporation predicted in arid regions.
!!    flwin(:)    |m^3 H2O       |flow into reach on previous day
!!    flwout(:)   |m^3 H2O       |flow out of reach on previous day
!!    i           |none          |current day of simulation
!!    id1         |none          |first day of simulation in year
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    msk_co1     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at bankfull depth (phi(10,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_co2     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at 0.1 bankfull depth (phi(13,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_x       |none          |weighting factor controlling relative
!!                               |importance of inflow rate and outflow rate
!!                               |in determining storage on reach
!!    pet_day     |mm H2O        |potential evapotranspiration
!!    phi(1,:)    |m^2           |cross-sectional area of flow in channel at
!!                               |bankfull depth
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    phi(6,:)    |m             |bottom width of main channel
!!    phi(10,:)   |hr            |storage time constant for reach at
!!                               |bankfull depth (ratio of storage to
!!                               |discharge)
!!    phi(13,:)   |hr            |storage time constant for reach at
!!                               |0.1 bankfull depth (low flow) (ratio
!!                               |of storage to discharge)
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rnum1       |none          |fraction of overland flow
!!    varoute(2,:)|m^3 H2O       |water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flwin(:)    |m^3 H2O       |flow into reach on current day
!!    flwout(:)   |m^3 H2O       |flow out of reach on current day
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    c1          |
!!    c2          |
!!    c3          |
!!    c4          |m^3 H2O       |
!!    det         |hr            |time step (24 hours)
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of
!!                               |day
!!    wtrin       |m^3 H2O       |water entering reach on day
!!    xkm         |hr            |storage time constant for the reach on
!!                               |current day
!!    yy          |none          |variable to hold intermediate calculation
!!                               |value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    code provided by Dr. Valentina Krysanova, Pottsdam Institute for
!!    Climate Impact Research, Germany
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University
  
      use parm

      integer :: jrch
      real :: xkm, det, yy, c1, c2, c3, c4, wtrin, p, vol, c, rh
      real :: topw
      real :: volrt, maxrt, adddep, addp, addarea
      real :: tmpstor, rttlc1, rttlc2, rtevp1, rtevp2, rttime1

      jrch = 0
      jrch = inum1

!! Water entering reach on day
      wtrin = 0.
      wtrin = varoute(2,inum2) * (1. - rnum1)

!! calculate volume of water in reach
      vol = 0.
      vol = wtrin + rchstor(jrch)

!! Find average flowrate in a day
      volrt = vol / 86400

!! Find maximum flow capacity of the channel at bank full
      c = 0.
      c = chside(jrch)
      p = phi(6,jrch) + 2. * ch_d(jrch) * Sqrt(1. + c * c)
      rh = phi(1,jrch) / p
      maxrt = Qman(phi(1,jrch), rh, ch_n(2,jrch), ch_s(2,jrch))

      sdti = 0.
      rchdep = 0.
      p = 0.
      rh = 0.

      if (volrt >= maxrt) then
        rcharea = phi(1,jrch)
        rchdep = ch_d(jrch)
        p = phi(6,jrch) + 2. * ch_d(jrch) * Sqrt(1. + c * c)
        rh = phi(1,jrch) / p
        sdti = maxrt
        !!  This is the time to empty the volume of water
        !!  at the bankfull discharge rate
        rttime1 = vol / (3600. * sdti)
        if (rttime1 > 24) then
          !! perform flood plain simulation
          !! Increase the discharge in flood plain until all the volume can be emptied
          !! within a day.
            adddep = 0
          Do While (rttime1 > 24)
            !!By iteration at 1cm interval find out how much depth of water in flood plain 
              !!for the discharge volume within a day
            adddep = adddep + 0.01
            addarea=rcharea + ((ch_w(2,jrch) * 5) + c * adddep) * adddep
            addp = p + (ch_w(2,jrch) * 4) + 2. * adddep * Sqrt(17.)
            rh = addarea / addp
            sdti = Qman(addarea, rh, ch_n(2,jrch), ch_s(2,jrch))
            rttime1 = vol / (3600. * sdti)
          End Do
          rcharea = addarea
          rchdep = rchdep + adddep
          p = addp
        End if
      else
      !! find the crossectional area and depth for volrt
      !! by iteration method at 1cm interval depth
      !! find the depth until the discharge rate is equal to volrt
        Do While (sdti < volrt)
          rchdep = rchdep + 0.01
          rcharea = (phi(6,jrch) + c * rchdep) * rchdep
          p = phi(6,jrch) + 2. * rchdep * Sqrt(1. + c * c)
          rh = rcharea / p
          sdti = Qman(rcharea, rh, ch_n(2,jrch), ch_s(2,jrch))
        end do
      end if

      sdti = volrt

!! Compute storage time constant for reach
      xkm = 0.
      xkm = phi(10,jrch) * msk_co1 + phi(13,jrch) * msk_co2

      det = 24.

!! Compute coefficients
      yy = 0.
      c1 = 0.
      c2 = 0.
      c3 = 0.
      c4 = 0.
      yy = 2. * xkm * (1. - msk_x) + det
      c1 = (det - 2. * xkm * msk_x) / yy
      c2 = (det + 2. * xkm * msk_x) / yy
      c3 = (2. * xkm * (1. - msk_x) - det) / yy
      c4 = phi(5,jrch) * ch_l2(jrch) * det / yy

!! Compute water leaving reach on day
      if (curyr == 1 .and. i == id1) then
        rtwtr = c1 * wtrin + c2 * rchstor(jrch) + c3 * rchstor(jrch)    &
     &                                                              + c4
      else
        rtwtr = c1 * vol + c2 * flwin(jrch) + c3 * flwout(jrch)
      end if
      if (rtwtr < 0.) rtwtr = 0.
!! Add statement to make sure the flow out is not more than flow in + storage
      rtwtr = min(rtwtr, rchstor(jrch)+wtrin)

!! define flow parameters for current day
      flwin(jrch) = 0.
      flwout(jrch) = 0.
      flwin(jrch) = wtrin
      flwout(jrch) = rtwtr

!! calculate amount of water in channel at end of day
      rchstor(jrch) = rchstor(jrch) + wtrin - rtwtr
!! Add if statement to keep rchstor from becoming negative
      if (rchstor(jrch) < 0.0) rchstor(jrch) = 0.0

      if (sdti > 0) then
        !! calculate travel time
        vel_chan(jrch) = sdti / rcharea
        rttime = ch_l2(jrch) * 1000. / (3600. * vel_chan(jrch))
 
!! transmission and evaporation losses are proportionally taken from the 
!! channel storage and from volume flowing out

       !! calculate transmission losses
        rttlc = 0.

        if (rtwtr > 0.) then

          rttlc = 24 * ch_k(2,jrch) * ch_l2(jrch) * p

          rttlc2 = rttlc * rchstor(jrch) / (rtwtr + rchstor(jrch))

          if (rchstor(jrch) <= rttlc2) then
            rttlc2 = min(rttlc2, rchstor(jrch))
            rchstor(jrch) = rchstor(jrch) - rttlc2
            rttlc1 = rttlc - rttlc2
            if (rtwtr <= rttlc1) then
              rttlc1 = min(rttlc1, rtwtr)
              rtwtr = rtwtr - rttlc1
            else
              rtwtr = rtwtr - rttlc1
            end if
          else
            rchstor(jrch) = rchstor(jrch) - rttlc2
            rttlc1 = rttlc - rttlc2
            if (rtwtr <= rttlc1) then
              rttlc1 = min(rttlc1, rtwtr)
              rtwtr = rtwtr - rttlc1
            else
              rtwtr = rtwtr - rttlc1
            end if
          end if
        rttlc = rttlc1 + rttlc2
        end if


        !! calculate evaporation
        rtevp = 0.
       if (rtwtr > 0.) then
          !! calculate width of channel at water level
          topw = 0.
          if (rchdep <= ch_d(jrch)) then
            topw = phi(6,jrch) + 2. * rchdep * chside(jrch)
          else
            topw = 5 * ch_w(2,jrch) + 2. * (rchdep - ch_d(jrch)) * 4.
          end if

          rtevp = evrch * pet_day * ch_l2(jrch) * topw

          rtevp2 = rtevp * rchstor(jrch) / (rtwtr + rchstor(jrch))

          if (rchstor(jrch) <= rtevp2) then
            rtevp2 = min(rtevp2, rchstor(jrch))
            rchstor(jrch) = rchstor(jrch) - rtevp2
            rtevp1 = rtevp - rtevp2
            if (rtwtr <= rtevp1) then
              rtevp1 = min(rtevp1, rtwtr)
              rtwtr = rtwtr - rtevp1
            else
              rtwtr = rtwtr - rtevp1
            end if
          else
            rchstor(jrch) = rchstor(jrch) - rtevp2
            rtevp1 = rtevp - rtevp2
            if (rtwtr <= rtevp1) then
              rtevp1 = min(rtevp1, rtwtr)
              rtwtr = rtwtr - rtevp1
            else
              rtwtr = rtwtr - rtevp1
            end if
          end if
        rtevp = rtevp1 + rtevp2
        end if

      else
        rtwtr = 0.
        sdti = 0.
      end if

!! precipitation on reach is not calculated because area of HRUs 
!! in subbasin sums up to entire subbasin area (including channel
!! area) so precipitation is accounted for in subbasin loop

      if (rchstor(jrch) < 10.) then
        rtwtr = rtwtr + rchstor(jrch)
        rchstor(jrch) = 0.
      end if

      if (rtwtr < 0.) rtwtr = 0.
      if (rchstor(jrch) < 0.) rchstor(jrch) = 0.

      return
      end
