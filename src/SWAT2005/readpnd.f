      subroutine readpnd

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the HRU/subbasin pond input file (.pnd).
!!    This file contains data related to ponds and wetlands in the 
!!    HRUs/subbasins.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hrutot(:)   |none          |number of HRUs in subbasin
!!    i           |none          |subbasin number
!!    ihru        |none          |HRU number
!!    nhru        |none          |number of last HRU in previous subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    chlap(:)    |none          |chlorophyll-a production coefficient for pond
!!    chlaw(:)    |none          |chlorophyll-a production coefficient for 
!!                               |wetland
!!    iflod1(:)   |none          |beginning month of non-flood season
!!    iflod2(:)   |none          |ending month of non-flood season
!!    ipnd1(:)    |none          |beginning month of nutrient settling season
!!    ipnd2(:)    |none          |ending month of nutrient settling season
!!    ndtarg(:)   |none          |number of days required to reach target 
!!                               |storage from current pond storage
!!    nsetlp(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlp(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    nsetlw(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlw(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    pnd_esa(:)  |ha            |surface area of ponds when filled to
!!                               |emergency spillway
!!    pnd_evol(:) |10**4 m**3 H2O|runoff volume from catchment area needed
!!                               |to fill the ponds to the emergency spillway
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into ponds
!!    pnd_k(:)    |mm/hr         |hydraulic conductivity through bottom of 
!!                               |ponds
!!    pnd_no3(:)  |kg N          |amount of nitrate in pond 
!!    pnd_nsed(:) |mg/L          |normal sediment concentration in pond water
!!    pnd_orgn(:) |kg N          |amount of organic N in pond
!!    pnd_orgp(:) |kg P          |amount of organic P in pond
!!    pnd_psa(:)  |ha            |surface area of ponds when filled to 
!!                               |principal spillway
!!    pnd_pvol(:) |10**4 m**3 H2O|runoff volume from catchment area needed to 
!!                               |fill the ponds to the principal spillway
!!    pnd_sed(:)  |mg/L          |sediment concentration in pond water
!!    pnd_solp(:) |kg P          |amount of soluble P in pond
!!    pnd_vol(:)  |10**4 m**3 H2O|volume of water in ponds
!!    psetlp(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlp(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    psetlw(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlw(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    seccip(:)   |none          |water clarity coefficient for pond
!!    secciw(:)   |none          |water clarity coefficient for wetland
!!    wet_fr(:)   |none          |fraction of HRU/subbasin area that drains 
!!                               |into wetlands
!!    wet_k(:)    |mm/hr         |hydraulic conductivity of bottom of wetlands
!!    wet_mxsa(:) |ha            |surface area of wetlands at maximum water 
!!                               |level
!!    wet_mxvol(:)|10**4 m**3 H2O|runoff volume from catchment area needed to
!!                               |fill wetlands to maximum water level
!!    wet_no3(:)  |kg N          |amount of nitrate in wetland
!!    wet_nsa(:)  |ha            |surface area of wetlands in subbasin at
!!                               |normal water level
!!    wet_nsed(:) |mg/L          |normal sediment concentration in wetland 
!!                               |water
!!    wet_nvol(:) |10**4 m**3 H2O|runoff volume from catchment area needed to 
!!                               |fill wetlands to normal water level
!!    wet_orgn(:) |kg N          |amount of organic N in wetland
!!    wet_orgp(:) |kg P          |amount of organic P in wetland
!!    wet_sed(:)  |mg/L          |sediment concentration in wetland water
!!    wet_solp(:) |kg P          |amount of soluble P in wetland
!!    wet_vol(:)  |10**4 m**3 H2O|volume of water in wetlands
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    eof         |none          |end of file flag
!!    evpnd       |none          |pond evaporation coefficient
!!    j           |none          |counter
!!    schla       |none          |value for CHLA used in subbasin
!!    schlaw      |none          |value for CHLAW used in subbasin
!!    sifld1      |none          |value for IFLOD1 used in subbasin
!!    sifld2      |none          |value for IFLOD2 used in subbasin
!!    sn1         |m/year        |value for NSETL1 used in subbasin
!!    sn2         |m/year        |value for NSETL2 used in subbasin
!!    sndt        |none          |value for NDTARG used in subbasin
!!    snw1        |m/year        |value for NSETLW1 used in subbasin
!!    snw2        |m/year        |value for NSETLW2 used in subbasin
!!    sp1         |m/year        |value for PSETL1 used in subbasin
!!    sp2         |m/year        |value for PSETL2 used in subbasin
!!    spnd1       |none          |value for IPND1 used in subbasin
!!    spnd2       |none          |value for IPND2 used in subbasin
!!    spndesa     |ha            |value for PND_ESA used in subbasin
!!    spndev      |10^4 m^3 H2O  |value for PND_EVOL used in subbasin
!!    spndfr      |none          |value for PND_FR used in subbasin
!!    spndk       |mm/hr         |value for PND_K used in subbasin
!!    spndns      |mg/L          |value for PND_NSED used in subbasin
!!    spndpsa     |ha            |value for PND_PSA used in subbasin
!!    spndpv      |10^4 m^3 H2O  |value for PND_PVOL used in subbasin
!!    spnds       |mg/L          |value for PND_SED used in subbasin
!!    spndv       |10^4 m^3 H2O  |value for PND_VOL used in subbasin
!!    spno3       |mg N/L        |concentration of NO3 in pond
!!    sporgn      |mg N/L        |concentration of organic N in pond
!!    sporgp      |mg P/L        |concentration of organic P in pond
!!    spsolp      |mg P/L        |concentration of soluble P in pond
!!    sseci       |none          |value for SECCI used in subbasin
!!    sseciw      |none          |value for SECCIW used in subbasin
!!    sw1         |m/year        |value for PSETLW1 used in subbasin
!!    sw2         |m/year        |value for PSETLW2 used in subbasin
!!    swetfr      |none          |value for WET_FR used in subbasin
!!    swetk       |mm/hr         |value for WET_K used in subbasin
!!    swetmsa     |ha            |value for WET_MSA used in subbasin
!!    swetmv      |10^4 m^3 H2O  |value for WET_MVOL used in subbasin
!!    swetns      |mg/L          |value for WET_NSED used in subbasin
!!    swetnsa     |ha            |value for WET_NSA used in subbasin
!!    swetnv      |10^4 m^3 H2O  |value for WET_NVOL used in subbasin
!!    swets       |mg/L          |value fro WET_SED used in subbasin
!!    swetv       |10^4 m^3 H2O  |value for WET_VOL used in subbasin
!!    swno3       |mg N/L        |concentration of NO3 in water
!!    sworgn      |mg N/L        |concentration of organic N in water
!!    sworgp      |mg P/L        |concentration of organic P in water
!!    swsolp      |mg P/L        |concentration of soluble P in water
!!    titldum     |NA            |title line of .pnd file 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      integer :: eof, j, sifld1, sifld2, sndt, spnd1, spnd2
      real :: spndfr, spndpsa, spndpv, spndesa, spndev, spndv, spnds
      real :: spndns, spndk, swetfr, swetnsa, swetnv, swetmsa, sp1
      real :: swetmv, swetv, swets, swetns, swetk, sp2, sw1, sw2
      real :: sn1, sn2, snw1, snw2, schla, schlaw, sseci, sseciw
      real :: spno3, spsolp, sporgn, sporgp, swno3, swsolp, sworgn
      real :: sworgp

      eof = 0
      spndfr = 0.
      spndpsa = 0.
      spndpv = 0.
      spndesa = 0.
      spndev = 0.
      spndv = 0.
      spnds = 0.
      spndns = 0.
      spndk = 0.
      sifld1 = 0
      sifld2 = 0
      sndt = 0
      sp1 = 0.
      sp2 = 0.
      sn1 = 0.
      sn2 = 0.
      schla = 0.
      sseci = 0.
      spno3 = 0.
      spsolp = 0.
      sporgn = 0.
      sporgp = 0.
      spnd1 = 0
      spnd2 = 0
      swetfr = 0.
      swetnsa = 0.
      swetnv = 0.
      swetmsa = 0.
      swetmv = 0.
      swetv = 0.
      swets = 0.
      swetns = 0.
      swetk = 0.
      sw1 = 0.
      sw2 = 0.
      snw1 = 0.
      snw2 = 0.
      schlaw = 0.
      sseciw = 0.
      swno3 = 0.
      swsolp = 0.
      sworgn = 0.
      sworgp = 0.
      pndevcoeff = 0. 
      wetevcoeff = 0.

      do
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,*,iostat=eof) spndfr
      if (eof < 0) exit
      read (104,*,iostat=eof) spndpsa
      if (eof < 0) exit
      read (104,*,iostat=eof) spndpv
      if (eof < 0) exit
      read (104,*,iostat=eof) spndesa
      if (eof < 0) exit
      read (104,*,iostat=eof) spndev
      if (eof < 0) exit
      read (104,*,iostat=eof) spndv
      if (eof < 0) exit
      read (104,*,iostat=eof) spnds
      if (eof < 0) exit
      read (104,*,iostat=eof) spndns
      if (eof < 0) exit
      read (104,*,iostat=eof) spndk
      if (eof < 0) exit
      read (104,*,iostat=eof) sifld1
      if (eof < 0) exit
      read (104,*,iostat=eof) sifld2
      if (eof < 0) exit
      read (104,*,iostat=eof) sndt
      if (eof < 0) exit
      read (104,*,iostat=eof) sp1
      if (eof < 0) exit
      read (104,*,iostat=eof) sp2
      if (eof < 0) exit
      read (104,*,iostat=eof) sn1
      if (eof < 0) exit
      read (104,*,iostat=eof) sn2
      if (eof < 0) exit
      read (104,*,iostat=eof) schla
      if (eof < 0) exit
      read (104,*,iostat=eof) sseci
      if (eof < 0) exit
      read (104,*,iostat=eof) spno3
      if (eof < 0) exit
      read (104,*,iostat=eof) spsolp
      if (eof < 0) exit
      read (104,*,iostat=eof) sporgn
      if (eof < 0) exit
      read (104,*,iostat=eof) sporgp
      if (eof < 0) exit
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,*,iostat=eof) spnd1
      if (eof < 0) exit
      read (104,*,iostat=eof) spnd2
      if (eof < 0) exit
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,*,iostat=eof) swetfr
      if (eof < 0) exit
      read (104,*,iostat=eof) swetnsa
      if (eof < 0) exit
      read (104,*,iostat=eof) swetnv
      if (eof < 0) exit
      read (104,*,iostat=eof) swetmsa
      if (eof < 0) exit
      read (104,*,iostat=eof) swetmv
      if (eof < 0) exit
      read (104,*,iostat=eof) swetv
      if (eof < 0) exit
      read (104,*,iostat=eof) swets
      if (eof < 0) exit
      read (104,*,iostat=eof) swetns
      if (eof < 0) exit
      read (104,*,iostat=eof) swetk
      if (eof < 0) exit
      read (104,*,iostat=eof) sw1
      if (eof < 0) exit
      read (104,*,iostat=eof) sw2
      if (eof < 0) exit
      read (104,*,iostat=eof) snw1
      if (eof < 0) exit
      read (104,*,iostat=eof) snw2
      if (eof < 0) exit
      read (104,*,iostat=eof) schlaw
      if (eof < 0) exit
      read (104,*,iostat=eof) sseciw
      if (eof < 0) exit
      read (104,*,iostat=eof) swno3
      if (eof < 0) exit
      read (104,*,iostat=eof) swsolp
      if (eof < 0) exit
      read (104,*,iostat=eof) sworgn
      if (eof < 0) exit
      read (104,*,iostat=eof) sworgp
      if (eof < 0) exit
      read (104,*,iostat=eof) pndevcoeff
      if (eof < 0) exit
      read (104,*,iostat=eof) wetevcoeff
      if (eof < 0) exit
      exit
      end do

      if (isproj == 2) swetfr = 0.0

!     set default values
      if (sndt <= 0) sndt = 15
      if (spndv > spndpv) spndv = spndpv
      if (swetv > swetnv) swetv = .95 * swetnv
      if (schla <= 0.) schla = 1.
      if (schlaw <= 0.) schlaw = 1.
      if (sseci <= 0.) sseci = 1.
      if (sseciw <= 0.) sseciw = 1.
      if (pndevcoeff <= 0.) pndevcoeff = 0.6
      if (wetevcoeff <= 0.) wetevcoeff = 0.6

!!    perform conversions
      sp1 = sp1 / 365.                     !! m/yr to m/day
      sp2 = sp2 / 365.
      sw1 = sw1 / 365.
      sw2 = sw2 / 365.
      sn1 = sn1 / 365.
      sn2 = sn2 / 365.
      snw1 = snw1 / 365.
      snw2 = snw2 / 365.
      spno3 = spno3 * spndv * 10.          !! mg/L to kg
      spsolp = spsolp * spndv * 10.
      sporgn = sporgn * spndv * 10.
      sporgp = sporgp * spndv * 10.
      swno3 = swno3 * spndv * 10.
      swsolp = swsolp * spndv * 10.
      sworgn = sworgn * spndv * 10.
      sworgp = sworgp * spndv * 10.

!! assign values to HRUs
      do j = 1, hrutot(i)
        ihru = 0
        ihru = nhru + j
        pnd_fr(ihru) = spndfr
        pnd_psa(ihru) = spndpsa
        pnd_pvol(ihru) = spndpv
        pnd_esa(ihru) = spndesa
        pnd_evol(ihru) = spndev
        pnd_vol(ihru) = spndv
        pnd_sed(ihru) = spnds
        pnd_nsed(ihru) = spndns
        pnd_k(ihru) = spndk
        iflod1(ihru) = sifld1
        iflod2(ihru) = sifld2
        ndtarg(ihru) = sndt
        psetlp(1,ihru) = sp1
        psetlp(2,ihru) = sp2
        nsetlp(1,ihru) = sn1
        nsetlp(2,ihru) = sn2
        chlap(ihru) = schla
        seccip(ihru) = sseci
        pnd_no3(ihru) = spno3
        pnd_solp(ihru) = spsolp
        pnd_orgn(ihru) = sporgn
        pnd_orgp(ihru) = sporgp
        ipnd1(ihru) = spnd1
        ipnd2(ihru) = spnd2
        wet_fr(ihru) = swetfr
        wet_nsa(ihru) = swetnsa
        wet_nvol(ihru) = swetnv
        wet_mxsa(ihru) = swetmsa
        wet_mxvol(ihru) = swetmv
        wet_vol(ihru) = swetv
        wet_sed(ihru) = swets
        wet_nsed(ihru) = swetns
        wet_k(ihru) = swetk
        psetlw(1,ihru) = sw1
        psetlw(2,ihru) = sw2
        nsetlw(1,ihru) = snw1
        nsetlw(2,ihru) = snw2
        chlaw(ihru) = schlaw
        secciw(ihru) = sseciw
        wet_no3(ihru) = swno3
        wet_solp(ihru) = swsolp
        wet_orgn(ihru) = sworgn
        wet_orgp(ihru) = sworgp
        evpnd(ihru) = pndevcoeff
        evwet(ihru) = wetevcoeff
      end do

      close (104)
      return
 5100 format (a)
      end
