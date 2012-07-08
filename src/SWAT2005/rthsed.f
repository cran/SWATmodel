      subroutine rthsed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    on an hourly timestep

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_cov(:)   |none          |channel cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_di(:)    |m             |initial depth of main channel
!!    ch_erod(:)  |none          |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_li(:)    |km            |initial length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_si(:)    |m/m           |initial slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    ch_wdr(:)   |m/m           |channel width to depth ratio
!!    hdepth(:)   |m             |depth of flow on day
!!    ideg        |none          |channel degredation code
!!                               |0: do not compute channel degradation
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    prf         |none          |Peak rate adjustment factor for sediment
!!                               |routing in the channel. Allows impact of
!!                               |peak flow rate on sediment routing and
!!                               |channel reshaping to be taken into account
!!    rnum1       |none          |fraction of overland flow
!!    sdti        |m^3/s         |average flow on day in reach
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    spcon       |none          |linear parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    spexp       |none          |exponent parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    varoute(3,:)|metric tons   |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    peakr       |m^3/s         |peak runoff rate in channel
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    sedrch      |metric tons   |sediment transported out of channel
!!                               |during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dat2        |m             |change in channel depth during time step
!!    deg         |metric tons   |sediment reentrained in water by channel
!!                               |degradation
!!    dep         |metric tons   |sediment deposited on river bottom
!!    depdeg      |m             |depth of degradation/deposition from original
!!    depnet      |metric tons   |
!!    dot         |
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    qdin        |m^3 H2O       |water in reach during time step
!!    vc          |m/s           |flow velocity in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, ii
      real :: qdin, sedin, vc, cyin, cych, depnet, deg, dep
      real :: depdeg, dot

      jrch = 0
      jrch = inum1

      do ii = 1, 24
        if (hrtwtr(ii) > 0. .and. hdepth(ii) > 0.) then

         !! initialize water in reach during time step
         qdin = 0.
         qdin = hrtwtr(ii) + hhstor(ii)

         !! do not perform sediment routing if no water in reach
         if (qdin > 0.01) then

           !! initialize sediment in reach during time step
           sedin = 0.
           if (ii == 1) then
             sedin = hhvaroute(3,inum2,ii) * (1. - rnum1) + sedst(jrch)
           else
             sedin = hhvaroute(3,inum2,ii) * (1. - rnum1) + hsedst(ii-1)
           end if

           !! initialize reach peak runoff rate
           peakr = prf * hsdti(ii)

           !! calculate flow velocity
           vc = 0.
           if (hharea(ii) < .010) then
             vc = 0.01
           else
             vc = peakr / hharea(ii)
           end if
           if (vc > 5.) vc = 5.

           !! JIMMY'S NEW IMPROVED METHOD for sediment transport
           cyin = 0.
           cych = 0.
           depnet = 0.
           deg = 0.
           dep = 0.
           cyin = sedin / qdin
           cych = spcon * vc ** spexp
           depnet = qdin * (cych - cyin)
           if (depnet > 0.) then
             deg = depnet * ch_erod(jrch) * ch_cov(jrch)
             dep = 0.
           else
             dep = -depnet
             deg = 0.
           endif

           hsedst(ii) = sedin + deg - dep
           hsedyld(ii) = hsedst(ii) * hrtwtr(ii) / qdin
           if (hsedyld(ii) < 0.) hsedyld(ii) = 0.
           sedrch = sedrch + hsedyld(ii)
           hsedst(ii) = hsedst(ii) - hsedyld(ii)
           if (hsedst(ii) < 0.) hsedst(ii) = 0.

         end if
        end if
      end do

      !! compute changes in channel dimensions
      if (ideg == 1) then
        qdin = 0.
        qdin = rtwtr + rchstor(jrch)
        depdeg = 0.
        depdeg = ch_d(jrch) - ch_di(jrch)
        if (depdeg < ch_si(jrch) * ch_li(jrch) * 1000.) then
          if (qdin > 1400000.) then
            dot = 0.
            dot = 358.6 * rchdep * ch_s(2,jrch) * ch_erod(jrch)
            dat2 = 1.
            dat2 =  dat2 * dot
            ch_d(jrch) = ch_d(jrch) + dat2
            ch_w(2,jrch) = ch_wdr(jrch) * ch_d(jrch)
            ch_s(2,jrch) = ch_s(2,jrch) - dat2 / (ch_l2(jrch) * 1000.)
            ch_s(2,jrch) = Max(.0001, ch_s(2,jrch))
            call ttcoef(jrch)
          endif
        endif
      endif

      return
      end

