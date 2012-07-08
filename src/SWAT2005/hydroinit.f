      subroutine hydroinit 

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine computes variables related to the watershed hydrology:
!!    the time of concentration for the subbasins, lagged surface runoff,
!!    the coefficient for the peak runoff rate equation, and lateral flow travel
!!    time.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ch_l1(:)    |km            |longest tributary channel length in subbasin
!!    ch_l2(:)    |km            |main channel length in subbasin
!!    ch_n(1,:)   |none          |Manning's "n" value for the tributary channels
!!    ch_s(1,:)   |m/m           |average slope of tributary channels
!!    da_km       |km2           |area of the watershed in square kilometers
!!    gdrain(:)   |hrs           |drain tile lag time: the amount of time
!!                               |between the transfer of water from the soil
!!                               |to the drain tile and the release of the
!!                               |water from the drain tile to the reach.
!!    hru_dafr(:) |km2/km2       |fraction of total watershed area contained
!!                               |in HRU
!!    hru_km(:)   |km2           |area of HRU in square kilometers
!!    hru_slp(:)  |m/m           |average slope steepness
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    lat_ttime(:)|days          |lateral flow travel time
!!   tile_ttime(:)|none          |Exponential of the tile flow travel time
!!    ldrain(:)   |none          |soil layer where drainage tile is located
!!    nhru        |none          |number of HRUs in watershed
!!    ov_n(:)     |none          |Manning's "n" value for overland flow
!!    slsoil(:)   |m             |slope length for lateral subsurface flow
!!    slsubbsn(:) |m             |average slope length for subbasin
!!    sol_k(:,:)  |mm/hr         |saturated hydraulic conductivity of soil
!!                               |layer
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sub_fr(:)   |none          |fraction of total watershed area contained in
!!                               |subbasin
!!    surlag      |days          |Surface runoff lag time.
!!                               |This parameter is needed in subbasins where
!!                               |the time of concentration is greater than 1 
!!                               |day. SURLAG is used to create a "storage" for
!!                               |surface runoff to allow the runoff to take 
!!                               |longer than 1 day to reach the subbasin outlet
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    brt(:)      |none          |fraction of surface runoff within the subbasin
!!                               |which takes 1 day or less to reach the 
!!                               |subbasin outlet
!!    lat_ttime(:)|none          |Exponential of the lateral flow travel time
!!   tile_ttime(:)|none          |Exponential of the tile flow travel time
!!    sub_tc(:)   |hr            |time of concentration for subbasin
!!    t_ov(:)     |hr            |time for flow from farthest point in subbasin
!!                               |to enter a channel
!!    tconc(:)    |hr            |time of concentration for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |counter
!!    l           |none          |counter
!!    scmx        |mm/hr         |maximum soil hydraulic conductivity
!!    t_ch        |hr            |time for flow entering the farthest upstream 
!!                               |channel to reach the subbasin outlet
!!    xx          |none          |variable to hold calculation result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~  
!!    SWAT: Ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer :: j, l
      real :: t_ch, scmx, xx

      do j = 1, nhru

!! subbasin !!
!!    compute time of concentration (sum of overland and channel times)
        t_ch = 0
        t_ov(j) = .0556 * (slsubbsn(j)*ov_n(j)) ** .6 / hru_slp(j) ** .3
        t_ch = .62 * ch_l1(j) * ch_n(1,hru_sub(j)) ** .75 /             &
     &      ((da_km * sub_fr(hru_sub(j)))**.125 *                       &
     &                                         ch_s(1,hru_sub(j))**.375)
        sub_tc(hru_sub(j)) = t_ov(j) + t_ch
!! end subbasin !!


!! HRU !!
!!    compute time of concentration (sum of overland and channel times)
        t_ch = 0
        ch_l1(j) = ch_l1(j) * hru_dafr(j) / sub_fr(hru_sub(j))
        t_ov(j) = .0556 * (slsubbsn(j)*ov_n(j)) ** .6 / hru_slp(j) ** .3
        t_ch = .62 * ch_l1(j) * ch_n(1,hru_sub(j)) ** .75 /             &
     &              ((da_km*hru_dafr(j))**.125*ch_s(1,hru_sub(j))**.375)
        tconc(j) = t_ov(j) + t_ch

!!    compute delivery ratio
      rto = tconc(j) / sub_tc(hru_sub(j))
      dr_sub(j) = amin1(.95,rto ** .5)


!!    compute fraction of surface runoff that is lagged
        brt(j) = 1. - Exp(-surlag / tconc(j))
        if (isproj == 2) brt(j) = 1.

!!    compute lateral flow travel time
        if (lat_ttime(j) <= 0.) then
            scmx = 0.
            do l = 1, sol_nly(j)
              if (sol_k(l,j) > scmx) then
                scmx = sol_k(l,j)
              endif
            end do
            !! unit conversion:
            !! xx = m/(mm/h) * 1000.(mm/m)/24.(h/d) / 4.
            xx = 0.
            xx = 10.4 * slsoil(j) / scmx
            lat_ttime(j) = 1. - Exp(-1./xx)
        else
          lat_ttime(j) = 1. - Exp(-1./lat_ttime(j))
        end if

        if (ldrain(j) > 0 .and. gdrain(j) > 0.01) then
            tile_ttime(j) = 1. - Exp(-24. / gdrain(j))
        else
            tile_ttime(j) = 0.
        end if

!!    compute routing coefficients for main channel
        if (ch_l2(hru_sub(j)) > 0.) call ttcoef(hru_sub(j))
        if (j == hru1(hru_sub(j))) then
          if (alpha_bnk(hru_sub(j)) <= 0.) then
            alpha_bnk(hru_sub(j)) = alpha_bf(j)
          end if
          alpha_bnke(hru_sub(j)) = Exp(-alpha_bnk(hru_sub(j)))
        end if

      end do

      if (ievent > 1) then
!!    compute unit hydrograph for computing subbasin hydrograph from direct runoff
      ql = 0.
      sumq = 0.

      do isb = 1, msub - 1
        tb = .5 + .6 * sub_tc(isb) + tb_adj
        if (tb > 48.) tb = 48.
        tp = .375 * tb
        itb(isb) = int(tb) + 1
        do i = 1, itb(isb)
          xi = float(i)
          if (xi < tp) then           !! rising limb of hydrograph
            q = xi / tp
          else                        !! falling limb of hydrograph
            q = (tb - xi) / (tb - tp)
          end if
          q = Max(1.,q)
          uh(isb,i) = (q + ql) / 2.
          ql = q
          sumq = sumq + uh(isb,i)
        end do

        do i = 1, itb(isb)
          uh(isb,i) = uh(isb,i) / sumq
        end do
      end do
      end if

      return
      end
