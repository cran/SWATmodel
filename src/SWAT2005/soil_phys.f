      subroutine soil_phys

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil physical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cn2(:)        |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    ddrain(:)     |mm            |depth to the sub-surface drain
!!    ffc(:)        |none          |initial HRU soil water content
!!                                 |expressed as fraction of field capacity
!!    hru_dafr(:)   |km2/km2       |fraction of total watershed area contained
!!                                 |in HRU
!!    i             |none          |HRU number
!!    ievent        |none          |rainfall/runoff code
!!                                 |0 daily rainfall/curve number technique
!!                                 |1 daily rainfall/Green&Ampt technique/daily
!!                                 |  routing
!!                                 |2 sub-daily rainfall/Green&Ampt technique/
!!                                 |  daily routing
!!                                 |3 sub-daily rainfall/Green&Ampt/hourly 
!!                                 |routing
!!    rock(:)       |%             |percent of rock fragments in soil layer
!!    silt(:)       |%             |percent silt content in soil material
!!    sol_awc(:,:)  |mm H20/mm soil|available water capacity of soil layer
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_clay(:,:) |%             |percent clay content in soil material
!!    sol_crk(:)    |none          |crack volume potential of soil
!!    sno_hru(:)    |mm H2O        |amount of water stored as snow
!!    sol_k(:,:)    |mm/hr         |saturated hydraulic conductivity of soil 
!!                                 |layer
!!    sol_nly(:)    |none          |number of soil layers 
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    usle_k(:)     |none          |USLE equation soil erodibility (K) factor
!!    usle_ls(:)    |none          |USLE equation length slope (LS) factor
!!    usle_p(:)     |none          |USLE equation support practice (P) factor
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    crdep(:,:)    |mm            |maximum or potential crack volume
!!    ldrain(:)     |none          |soil layer where drainage tile is located
!!    rock(:)       |none          |exponential value that is a function of
!!                                 |percent rock
!!    sol_avbd(:)   |Mg/m^3        |average bulk density for soil profile
!!    sol_avpor(:)  |none          |average porosity for entire soil profile
!!    sol_fc(:,:)   |mm H2O        |amount of water available to plants in soil 
!!                                 |layer at field capacity (fc - wp)
!!    sol_hk(:,:)   |none          |beta coefficent to calculate hydraulic
!!                                 |conductivity
!!    sol_por(:,:)  |none          |total porosity of soil layer expressed as a 
!!                                 |fraction of the total volume
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer
!!                                 |on any given day (less wp water)
!!    sol_sumfc(:)  |mm H2O        |amount of water held in soil profile at
!!                                 |field capacity
!!    sol_sumul(:)  |mm H2O        |amount of water held in soil profile at
!!                                 |saturation
!!    sol_sw(:)     |mm H2O        |amount of water stored in soil profile on
!!                                 |any given day
!!    sol_ul(:,:)   |mm H2O        |amount of water held in the soil layer at
!!                                 |saturation (sat - wp water)
!!    sol_up(:,:)   |mm H2O/mm soil|water content of soil at -0.033 MPa (field
!!                                 |capacity)
!!    sol_wp(:,:)   |mm H20/mm soil|water content of soil at -1.5 MPa (wilting
!!                                 |point)
!!    sol_wpmm(:,:) |mm H20        |water content of soil at -1.5 MPa (wilting
!!                                 |point)
!!    usle_mult(:)  |none          |product of USLE K,P,LS,exp(rock)
!!    volcr(:,:)    |mm            |crack volume for soil layer
!!    wfsh(:)       |mm            |wetting front matric potential
!!    wshd_snob     |mm H20        |average amount of water stored in snow
!!                                 |at the beginning of the simulation for the
!!                                 |entire watershed
!!    wshd_sw       |mm H2O        |average amount of water stored in soil
!!                                 |for the entire watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dg          |mm            |depth of layer
!!    j           |none          |counter
!!    nly         |none          |number of soil layers
!!    pormm       |mm            |porosity in mm depth
!!    sand        |%             |percent sand content in soil material
!!    sumpor      |mm            |porosity of profile
!!    xx          |none          |variable to hold value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Sqrt
!!    SWAT: Curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: nly, j
      real :: xx, sumpor, dg, pormm

      nly = 0
    
      nly = sol_nly(i)


!!    calculate composite usle value
      rock(i) = Exp(-.053 * rock(i))
      usle_mult(i) = rock(i) * usle_k(i) * usle_p(i) * usle_ls(i) * 11.8


!!    calculate water content of soil at -1.5 MPa and -0.033 MPa
      do j = 1, nly
        sol_wp(j,i) = 0.4 * sol_clay(j,i) * sol_bd(j,i) / 100.
        if (sol_wp(j,i) <= 0.) sol_wp(j,i) = .005
        sol_up(j,i) = sol_wp(j,i) + sol_awc(j,i)
        sol_por(j,i) = 1. - sol_bd(j,i) / 2.65
        if (sol_up(j,i) >= sol_por(j,i)) then
          sol_up(j,i) = sol_por(j,i) - .05
          sol_wp(j,i) = sol_up(j,i) - sol_awc(j,i)
          if (sol_wp(j,i) <= 0.) then
            sol_up(j,i) = sol_por(j,i) * .75
            sol_wp(j,i) = sol_por(j,i) * .25
          end if
        end if
      end do


!!    initialize water/drainage coefs for each soil layer
      xx = 0.
      sumpor = 0.
      do j = 1, nly
        dg = 0.
        pormm = 0.
        dg = sol_z(j,i) - xx
        pormm = sol_por(j,i) * dg
        sumpor = sumpor + pormm
        sol_ul(j,i) = (sol_por(j,i) - sol_wp(j,i)) * dg
        sol_sumul(i) = sol_sumul(i) + sol_ul(j,i)
        sol_fc(j,i) = dg * (sol_up(j,i) - sol_wp(j,i))
        sol_sumfc(i) = sol_sumfc(i) + sol_fc(j,i)
        sol_st(j,i) = sol_fc(j,i) * ffc(i)
        sol_hk(j,i) = (sol_ul(j,i) - sol_fc(j,i)) / sol_k(j,i)
        if (sol_hk(j,i) < 1.) sol_hk(j,i) = 1.
        sol_sw(i) = sol_sw(i) + sol_st(j,i)
        sol_wpmm(j,i) = sol_wp(j,i) * dg
        sol_sumwp(i) = sol_sumwp(i) + sol_wpmm(j,i)
        crdep(j,i) = sol_crk(i) * 0.916 * Exp(-.0012 * sol_z(j,i)) * dg
        volcr(j,i) = crdep(j,i) * (sol_fc(j,i) - sol_st(j,i)) /   
     &       (sol_fc(j,i))
        xx = sol_z(j,i)
      end do
      sol_avpor(i) = sumpor / sol_z(nly,i)
      sol_avbd(i) = 2.65 * (1. - sol_avpor(i))


!!    define soil layer that the drainage tile is in
      if (ddrain(i) > 0) then
        do j = 1, nly
          if (ddrain(i) < sol_z(j,i)) ldrain(i) = j
          if (ddrain(i) < sol_z(j,i)) exit
        end do
      else
       ldrain(i) = 0
      endif

!!    calculate infiltration parameters for subdaily time step
      if (ievent > 0) then
        sand = 0.
        sand = 100. - sol_clay(1,i) - silt(i)
        wfsh(i) = 10. * Exp(6.5309 - 7.32561 * sol_por(1,i) +           &
     &    3.809479 * sol_por(1,i) ** 2 + 0.001583 * sol_clay(1,i) ** 2 +&
     &    0.000344 * sand * sol_clay(1,i) - 0.049837 * sol_por(1,i) *   &
     &    sand + 0.001608 * sol_por(1,i) ** 2 * sand ** 2 +             &
     &    0.001602 * sol_por(1,i) ** 2 * sol_clay(1,i) ** 2 -           &
     &    0.0000136 * sand ** 2 * sol_clay(1,i) -                       &
     &    0.003479 * sol_clay(1,i) ** 2 * sol_por(1,i) -                &
     &    0.000799 * sand ** 2 * sol_por(1,i))
      end if


!!    initialize watershed water parameters
      wshd_sw = wshd_sw + sol_sw(i) * hru_dafr(i)
      wshd_snob = wshd_snob + sno_hru(i) * hru_dafr(i)


      call curno(cn2(i),i)

      return
      end
