      subroutine readhru
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU general input file (.hru).
!!    This file contains data related to general processes modeled
!!    at the HRU level.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ifld(:)     |none          |number of HRU (in subbasin) that is a
!!                               |floodplain
!!    ihru        |none          |HRU number
!!    ipot(:)     |none          |number of HRU (in subbasin) that is ponding
!!                               |water--the HRU that the surface runoff from
!!                               |current HRU drains into. This variable is
!!                               |used only for rice paddys or closed
!!                               |depressional areas
!!    irip(:)     |none          |number of HRU (in subbasin) that is a
!!                               |riparian zone
!!    da_km       |km2           |area of the watershed in square kilometers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canmx(:)    |mm H2O        |maximum canopy storage
!!    cf          |              |this parameter controls the response of 
!!                               |decomposition to the combined effect of soil
!!                               |temperature and moisture.  
!!    cfh         |              |Maximum humification rate``
!!    cfdec       |              |the undisturbed soil turnover rate under
!!                               |optimum soil water and temperature. Increasing
!!                               |it will increase carbon and organic N decomp.
!!    dis_stream(:) | m          |average distance to stream
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    erorgn(:)   |none          |organic N enrichment ratio, if left blank
!!                               |the model will calculate for every event
!!    erorgp(:)   |none          |organic P enrichment ratio, if left blank
!!                               |the model will calculate for every event
!!    esco(:)     |none          |soil evaporation compensation factor (0-1)
!!    evpot(:)    |none          |pothole evaporation coefficient
!!    fld_fr(:)   |km2/km2       |fraction of HRU area that drains into floodplain
!!    hru_fr(:)   |km2/km2       |fraction of subbasin area contained in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    hru_slp(:)  |m/m           |average slope steepness
!!    lat_sed(:)  |g/L           |sediment concentration in lateral flow
!!    lat_ttime(:)|days          |lateral flow travel time
!!    ov_n(:)     |none          |Manning's "n" value for overland flow
!!    pot_fr(:)   |km2/km2       |fraction of HRU area that drains into pothole
!!    pot_no3l(:) |1/day         |nitrate decay rate in impounded area
!!    pot_nsed(:) |mg/L          |normal sediment concentration in impounded
!!                               |water (needed only if current HRU is IPOT)
!!    pot_tile(:) |m3/s          |average daily outflow to main channel from
!!                               |tile flow if drainage tiles are installed in
!!                               |pothole (needed only if current HRU is IPOT)
!!    pot_vol(:)  |mm            |initial volume of water stored in the
!!                               |depression/impounded area (read in as mm
!!                               |and converted to m^3) (needed only if current
!!                               |HRU is IPOT)
!!    pot_volx(:) |mm            |maximum volume of water stored in the
!!                               |depression/impounded area (read in as mm
!!                               |and converted to m^3) (needed only if current
!!                               |HRU is IPOT)
!!    rip_fr(:)   |km2/km2       |fraction of HRU area that drains into riparian 
!!                               |zone
!!    rsdin(:)    |kg/ha         |initial residue cover
!!    slsoil(:)   |m             |slope length for lateral subsurface flow
!!    slsubbsn(:) |m             |average slope length for subbasin
!!    usle_ls(:)  |none          |USLE equation length slope (LS) factor
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag (=-1 if eof, else =0)
!!    epcohru     |none          |plant water uptake compensation factor (0-1)
!!    escohru     |none          |soil evaporation compensation factor (0-1)
!!    sin_sl      |none          |Sin(slope angle)
!!    titldum     |NA            |title line of .sub file (not used)
!!    xm          |none          |exponential in equation to calculate
!!                               |USLE LS
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Sin, Atan

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      integer :: eof
      real :: xm, sin_sl, epcohru, escohru

      eof = 0
      escohru = 0.
      epcohru = 0.
      
      do
      read (108,5100) titldum
      read (108,*) hru_fr(ihru)
      read (108,*) slsubbsn(ihru)
      read (108,*) hru_slp(ihru) 
      read (108,*) ov_n(ihru)
      read (108,*) lat_ttime(ihru) 
      read (108,*) lat_sed(ihru)   !read in in mg/L
      read (108,*) slsoil(ihru)
      read (108,*,iostat=eof) canmx(ihru) 
      if (eof < 0) exit
      read (108,*,iostat=eof) escohru
      if (eof < 0) exit
      read (108,*,iostat=eof) epcohru 
      if (eof < 0) exit
      read (108,*,iostat=eof) rsdin(ihru) 
      if (eof < 0) exit
      read (108,*,iostat=eof) erorgn(ihru) 
      if (eof < 0) exit
      read (108,*,iostat=eof) erorgp(ihru) 
      if (eof < 0) exit
      read (108,*,iostat=eof) pot_fr(ihru) 
      if (eof < 0) exit
      read (108,*,iostat=eof) fld_fr(ihru) 
      if (eof < 0) exit
      read (108,*,iostat=eof) rip_fr(ihru) 
      if (eof < 0) exit
      read (108,5100,iostat=eof) titldum
      if (eof < 0) exit
!      if (ipot(ihru) == ihru) then   Srini pothole
        read (108,*,iostat=eof) pot_tilemm(ihru)    !!NUBZ
        if (eof < 0) exit
        read (108,*,iostat=eof) pot_volxmm(ihru) 
        if (eof < 0) exit
        read (108,*,iostat=eof) pot_volmm(ihru) 
        if (eof < 0) exit
        read (108,*,iostat=eof) pot_nsed(ihru) 
        if (eof < 0) exit
        read (108,*,iostat=eof) pot_no3l(ihru)
        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!      end if
      read (108,*,iostat=eof) dep_imp(ihru)
      if (eof < 0) exit
      read (108,5100,iostat=eof) titldum       
      if (eof < 0) exit
      read (108,5100,iostat=eof) titldum      
      if (eof < 0) exit
      read (108,5100,iostat=eof) titldum           
      if (eof < 0) exit
      read (108,*,iostat=eof) evpot(ihru)
      if (eof < 0) exit
      read (108,*,iostat=eof) dis_stream(ihru)
      if (eof < 0) exit
!! armen & stefan changes for SWAT-C
	read (108,*,iostat=eof) cf(ihru)
	if (eof < 0) exit
	read (108,*,iostat=eof) cfh(ihru)
	if (eof < 0) exit
	read (108,*,iostat=eof) cfdec(ihru)
	if (eof < 0) exit
        read (108,*,iostat=eof) sed_con(ihru)
        if (eof < 0) exit
        read (108,*,iostat=eof) orgn_con(ihru)
        if (eof < 0) exit
        read (108,*,iostat=eof) orgp_con(ihru)
        if (eof < 0) exit
        read (108,*,iostat=eof) soln_con(ihru)
        if (eof < 0) exit
        read (108,*,iostat=eof) solp_con(ihru)
	exit
      end do


!!    compare .hru input values to .bsn input values
      if (escohru > 1.e-4) esco(ihru) = escohru
      if (epcohru > 1.e-4) epco(ihru) = epcohru

!!    set default values
      if (dep_imp(ihru) <=0.) dep_imp(ihru) = depimp_bsn
!     if (ddrain(ihru) <= 0.) ddrain(ihru) = 1000.
!     if (tdrain(ihru) <= 0.) tdrain(ihru) = 24.
!     if (gdrain(ihru) <= 0.) gdrain(ihru) = 96.
!! comment the following line for the hru_fraction data !!
      if (hru_fr(ihru) <= 0.) hru_fr(ihru) = .0000001
      if (slsubbsn(ihru) <= 0.) slsubbsn(ihru) = 50.0
      if (hru_slp(ihru) <= 0.0001) hru_slp(ihru) = .0001
      if (hru_slp(ihru) >= 1.0) hru_slp(ihru) = 1.0
      if (slsoil(ihru) <= 0.)  slsoil(ihru) = slsubbsn(ihru)
      if (esco(ihru) <= 0.) esco(ihru) = .95
!     if (dep_imp(ihru) <= 0.) dep_imp(ihru) = 6000.
!     esco(ihru) = 1. - esco(ihru)
      if (epco(ihru) <= 0. .or. epco(ihru) > 1.) epco(ihru) = 1.0
      if (evpot(ihru) <= 0.) evpot(ihru) = 0.5
      if (dis_stream(ihru) <= 0.) dis_stream(ihru) = 35.0

!! armen & stefan changes for SWAT-C
     	if (cf(ihru) <= 0.) cf(ihru)= 1.0
	if (cfh(ihru) <= 0.) cfh(ihru)= 1.0
	if (cfdec(ihru) <= 0.) cfdec(ihru)= 0.055
!! armen & stefan end
     

!!    calculate USLE slope length factor
      xm = 0.
      sin_sl = 0.
      xm = .6 * (1. - Exp(-35.835 * hru_slp(ihru)))
      sin_sl = Sin(Atan(hru_slp(ihru)))
      usle_ls(ihru) = (slsubbsn(ihru)/22.128)**xm * (65.41 * sin_sl *   &
     &                sin_sl + 4.56 * sin_sl + .065)

!!    other calculations
      hru_km(ihru) = sub_km(i) * hru_fr(ihru)
      hru_ha(ihru) = hru_km(ihru) * 100.
      lat_sed(ihru) = lat_sed(ihru) * 1.e-3     !!mg/L => g/L
      pot_vol(ihru) = 10. * pot_volmm(ihru) * hru_ha(ihru)    !! mm => m^3  Srini pothole   NUBZ
      pot_volx(ihru) = 10. * pot_volxmm(ihru) * hru_ha(ihru)  !! mm => m^3
      pot_tile(ihru) = 10. * pot_tilemm(ihru) * hru_ha(ihru)  !! mm => m^3

      xx = pot_vol(ihru) / 1000000.  !! mg/L * m3 * 1000L/m3 * t/1,000,000,000   Srini pothole
	pot_sed(ihru) = pot_nsed(ihru) * xx
	pot_san(ihru) = 0. 
	pot_sil(ihru) = 0. 
	pot_cla(ihru) = pot_nsed(ihru) * xx 
	pot_sag(ihru) = 0. 
	pot_lag(ihru) = 0. 

      close (108)
      return
 5100 format (a)
      end
