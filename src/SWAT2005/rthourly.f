      subroutine rthourly
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes hourly flow through the reach using a
!!    constant storage coefficient.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)         |m             |average depth of main channel
!!    ch_k(2,:)       |mm/hr         |effective hydraulic conductivity of
!!                                   |main channel alluvium
!!    ch_l2(:)        |km            |length of main channel
!!    ch_n(2,:)       |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)       |m/m           |average slope of main channel
!!    ch_w(2,:)       |m             |average width of main channel
!!    chside(:)       |none          |change in horizontal distance per unit
!!                                   |change in vertical distance on channel
!!                                   |side slopes; always set to 2 (slope=1/2)
!!    evrch           |none          |Reach evaporation adjustment factor.
!!                                   |Evaporation from the reach is multiplied
!!                                   |by EVRCH. This variable was created to 
!!                                   |limit the evaporation predicted in arid 
!!                                   |regions.
!!    hhvaroute(2,:,:)|m^3 H2O       |water
!!    inum1           |none          |reach number
!!    inum2           |none          |inflow hydrograph storage location number
!!    pet_day         |mm H2O        |potential evapotranspiration
!!    phi(1,:)        |m^2           |cross-sectional area of flow in channel at
!!                                   |bankfull depth
!!    phi(6,:)        |m             |bottom width of main channel
!!    rchstor(:)      |m^3 H2O       |water stored in reach
!!    rnum1           |none          |fraction of overland flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hdepth(:)   |m             |depth of flow during hour
!!    hharea(:)   |m^2           |cross-sectional area of flow
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of hour
!!    hhtime(:)   |hr            |flow travel time for hour
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of hour
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during hour
!!    hsdti(:)    |m^3/s         |average flow rate during hour
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    hrtevp      |m^3 H2O       |evaporation losses for hour
!!    hrttlc      |m^3 H2O       |transmission losses for hour
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    scoef       |none          |storage coefficient
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium

      use parm

      integer :: jrch, ii, inhyd
      real :: wtrin, hrttlc, hrtevp, c, p, rh, scoef
      real :: vol, topw


      jrch = 0
      jrch = inum1

      inhyd = 0
      inhyd = inum2

!! hour loop
      do ii = 1, 24
        !! water entering reach during hour
        wtrin = 0.
        wtrin = hhvaroute(2,inhyd,ii) * (1. - rnum1)

        !! calculate volume of water in reach
        vol = 0.
        if (ii == 1) then
          hrchwtr(ii) = rchstor(jrch)
          vol = wtrin + rchstor(jrch)
        else
          hrchwtr(ii) = hhstor(ii-1)
          vol = wtrin + hhstor(ii-1)
        end if
        vol = Max(vol,1.e-4)

        !! calculate cross-sectional area of flow
        hharea(ii) = vol / (ch_l2(jrch) * 1000.)

        !! calculate depth of flow
        c = 0.
        c = chside(jrch)
        if (hharea(ii) <= phi(1,jrch)) then
          hdepth(ii) = Sqrt(hharea(ii) / c + phi(6,jrch) * phi(6,jrch)  &
     &                          / (4. * c * c)) - phi(6,jrch) / (2. * c)
          if (hdepth(ii) < 0.) hdepth(ii) = 0.
        else
          hdepth(ii) = Sqrt((hharea(ii) - phi(1,jrch)) / 4. + 25. *     &
     &       ch_w(2,jrch) * ch_w(2,jrch) / 64.) - 5. * ch_w(2,jrch) / 8.
          if (hdepth(ii) < 0.) hdepth(ii) = 0.
          hdepth(ii) = hdepth(ii) + ch_d(jrch)
        end if

        !! calculate wetted perimeter
        p = 0.
        if (hdepth(ii) <= ch_d(jrch)) then
          p = phi(6,jrch) + 2. * hdepth(ii) * Sqrt(1. + c * c)
        else
          p = phi(6,jrch) + 2. * ch_d(jrch) * Sqrt(1. + c * c) +        &
     &    4. * ch_w(2,jrch) + 2. * (hdepth(ii) - ch_d(jrch)) * Sqrt(17.)
        end if

        !! calculate hydraulic radius
        rh = 0.
        if (p > 0.01) then
          rh = hharea(ii) / p
        else
          rh = 0.
        end if

        !! calculate rate of flow in reach
        hsdti(ii) = Qman(hharea(ii), rh, ch_n(2,jrch), ch_s(2,jrch))
       
        if (hsdti(ii) > 0.) then
          !! calculate travel time
          hhtime(ii) = ch_l2(jrch) * hharea(ii) / (3.6 * hsdti(ii))
          if (hhtime(ii) < 1.) then
            rttime = rttime + hhtime(ii)
          else
            rttime = rttime + 1.
          end if

          !! calculate volume of water leaving reach on day
          scoef = 0.
          scoef = 2. / (2. * hhtime(ii) + 1.)
          if (scoef > 1.) scoef = 1.
          hrtwtr(ii) = scoef * vol

          !! calculate transmission losses
          hrttlc = 0.
          if (hhtime(ii) < 1.) then
            hrttlc = ch_k(2,jrch) * ch_l2(jrch) * p * hhtime(ii)
          else
            hrttlc = ch_k(2,jrch) * ch_l2(jrch) * p
          end if
          hrttlc = Min(hrtwtr(ii),hrttlc)
          hrtwtr(ii) = hrtwtr(ii) - hrttlc
          rttlc = rttlc + hrttlc

          hrtevp = 0.
          if (hrtwtr(ii) > 0.) then
            !! calculate width of channel at water level
            topw = 0.
            if (hdepth(ii) <= ch_d(jrch)) then
              topw = phi(6,jrch) + 2. * hdepth(ii) * chside(jrch)
            else
              topw = 5. * ch_w(2,jrch) + 8. * (hdepth(ii) - ch_d(jrch))
            end if

            !! calculate evaporation
            if (hhtime(ii) < 1.) then
              hrtevp = evrch * pet_day/24 * ch_l2(jrch) * topw *        &
     &                                                        hhtime(ii)
            else
              hrtevp = evrch * pet_day/24 * ch_l2(jrch) * topw
            end if
            if (hrtevp < 0.) hrtevp = 0.
            hrtevp = Min(hrtwtr(ii),hrtevp)
            hrtwtr(ii) = hrtwtr(ii) - hrtevp
            rtevp = rtevp + hrtevp
          end if

          !! set volume of water in channel at end of hour
          if (ii == 1) then
            hhstor(ii) = rchstor(jrch) + wtrin - hrtwtr(ii) - hrtevp -  &
     &                                                            hrttlc
          else
            hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii) - hrtevp -   &
     &                                                            hrttlc
          end if
          if (hhstor(ii) < 0.) then
            hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
            hhstor(ii) = 0.
            if (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
          end if

        end if

      end do                     !! end hour loop

!! calculate amount of water in channel at end of day
      if (hhstor(24) < 10.) then
        hrtwtr(24) = hrtwtr(24) + hhstor(24)
        hhstor(24) = 0.
      end if
      if (hrtwtr(24) < 0.) hrtwtr(24) = 0.
      
!! daily average values
      !! set volume of water in reach at end of day
      rchstor(jrch) = hhstor(24)
      !! calculate total amount of water leaving reach
      rtwtr = Sum(hrtwtr)
      !! calculate average flow area
      rcharea = Sum (hharea) / 24.
      !! calculate average flow depth
      rchdep = Sum(hdepth) / 24.
      !! calculate average flow rate
      sdti = Sum(hsdti) / 24.

      return
      end
