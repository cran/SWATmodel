       subroutine route
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates channel routing     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bnke(:)|none         |Exp(-alpha_bnk(:))
!!    bankst(:)   |m^3 H2O       |bank storage
!!    ch_l2(:)    |km            |length of main channel
!!    ch_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from bank storage to the root
!!                               |zone as a result of soil moisture depletion
!!    ch_w(2,:)   |m             |average width of main channel
!!    da_ha       |ha            |area of watershed in hectares
!!    hru_sub(:)  |none          |subbasin number for HRU
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    irte        |none          |water routing method:
!!                               |0 variable storage method
!!                               |1 Muskingum method
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality (QUAL2E)
!!    nhru        |none          |number of HRUs in watershed
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    rchdep      |m             |depth of flow on day
!!    rnum1       |none          |fraction of overland flow 
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    varoute(3,:)|metric tons   |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    revapday    |m^3 H2O       |amount of water moving from bank storage
!!                               |into the soil profile or being taken
!!                               |up by plant roots in the bank storage zone
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: rchinit, rtover, rtday, rtmusk, rthourly, rtsed, rthsed, watqual
!!    SWAT: noqual, hhwatqual, hhnoqual, rtpest, rthpest, rtbact, irr_rch
!!    SWAT: rchuse, reachout

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, ii
      real :: subwtr

      jrch = 0
      jrch = inum1

!! initialize variables for route command loop
      call rchinit

!! route overland flow
      call rtover

      vel_chan(jrch) = 0.
      dep_chan(jrch) = 0.

!! route water through reach
      if (ievent < 3) then
        if (irte == 0) call rtday
        if (irte == 1) call rtmusk
      else
        if (irte == 0) call rthourly
        if (irte == 1) call rthmusk
      endif

!! average daily water depth for sandi doty 09/26/07
      dep_chan(jrch) = rchdep

!! if reach is an irrigation canal, restrict outflow
      if (icanal(jrch) == 1) then
        rchstor(jrch) = rchstor(jrch) + rtwtr
        rtwtr = 0.
      end if

!! add transmission losses to bank storage/deep aquifer in subbasin
      if (rttlc > 0.) then
        bankst(jrch) = bankst(jrch) + rttlc * (1. - trnsrch)
        subwtr = 0.
        subwtr = rttlc * trnsrch / (da_ha * sub_fr(jrch) * 10.)
        do j = hru1(jrch), hru1(jrch) + hrutot(jrch) - 1
          deepst(j) = deepst(j) + subwtr
        end do
      end if
 
!! compute revap from bank storage
      revapday = ch_revap(jrch) * pet_day * ch_l2(jrch) * ch_w(2,jrch)
      revapday = Min(revapday,bankst(jrch))
      bankst(jrch) = bankst(jrch) - revapday

!! compute contribution of water in bank storage to streamflow
      qdbank = bankst(jrch) * (1. - alpha_bnke(jrch))
      bankst(jrch) = bankst(jrch) - qdbank
      rtwtr = rtwtr + qdbank
      if (ievent > 2) then
        do ii = 1, 24
          hrtwtr(ii) = hrtwtr(ii) + qdbank / 24.
        end do
      end if

!! perform in-stream sediment calculations
        if (inum1 /= inum2) then
          !! do not perform sediment routing for headwater subbasins
            if (ievent < 3) then
              call rtsed
            else
              call rthsed
            end if
        else
          if (ievent < 3) then
            if (rtwtr > 0. .and. rchdep > 0.) then
              sedrch = varoute(3,inum2) * (1. - rnum1)
            end if
          else
            do ii = 1, 24
              if (hrtwtr(ii) > 0. .and. hdepth(ii) > 0.) then
                hsedyld(ii) = hhvaroute(3,inum2,ii) * (1. - rnum1)
                sedrch = sedrch + hsedyld(ii)
              end if
            end do
          end if
        end if

!! perform in-stream nutrient calculations
      if (ievent < 3) then
        if (iwq == 2) call watqual2
        if (iwq == 1) call watqual
        if (iwq == 0) call noqual
      else
        if (iwq == 1) call hhwatqual
        if (iwq == 0) call hhnoqual
      end if

!! perform in-stream pesticide calculations
      if (ievent < 3) then
        call rtpest
      else
        call rthpest
      end if

!! perform in-stream bacteria calculations
      call rtbact

!! remove water from reach for irrigation
      call irr_rch

!! remove water from reach for consumptive water use
      call rchuse

!! summarize output/determine loadings to next routing unit
      call rtout

      return
      end
