      subroutine conapply
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies continuous pesticide

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)     |none             |application efficiency (0-1)
!!    curyr        |none             |current year of simulation
!!    drift(:)     |kg               |amount of pesticide drifting onto main 
!!                                   |channel in subbasin
!!    driftco(:)   |none             |coefficient for pesticide drift directly
!!                                   |onto stream
!!    hru_dafr(:)  |km**2/km**2      |fraction of watershed area in HRU
!!    hru_km(:)    |km**2            |area of HRU in square kilometers
!!    ihru         |none             |HRU number
!!    ipest(:,:,:) |none             |pesticide identification number from
!!                                   |pest.dat
!!    irtpest      |none             |the sequence number of the pesticide type
!!                                   |in NPNO(:) which is to be routed through
!!                                   |the watershed
!!    laiday(:)    |none             |leaf area index
!!    nope(:)      |none             |sequence number of pesticide in NPNO(:)
!!    npest(:)     |none             |sequence number of pesticide application
!!                                   |within the year
!!    nro(:)       |none             |sequence number of year in rotation
!!    nyskip       |none             |number of years to skip output
!!                                   |summarization/printing
!!    plt_pst(:,:) |kg/ha            |pesticide on plant foliage
!!    pst_kg(:,:,:)|kg/ha            |amount of pesticide applied to HRU
!!    sol_pst(:,:,1)|kg/ha           |pesticide in first layer of soil
!!    wshd_pstap(:)|kg/ha            |total amount of pesticide type applied in 
!!                                   |watershed during simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drift(:)    |kg            |amount of pesticide drifting onto main 
!!                               |channel in subbasin
!!    npest(:)    |none          |sequence number of pesticide application
!!                               |within the year
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    wshd_pstap(:)|kg/ha         |total amount of pesticide type applied in 
!!                               |watershed during simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gc          |none          |fraction of ground covered by plant foliage
!!    j           |none          |HRU number
!!    jj          |none          |subbasin number
!!    k           |none          |sequence number of pesticide in NPNO(:)
!!    kk          |none          |pesticide identification number from
!!                               |pest.dat
!!    xx          |kg/ha         |amount of pesticide applied to HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, kk, k, jj
      real :: xx, gc

      j = 0
      j = ihru

!! if continuous pesticide not currently on, check to see if it is time
!! to initialize continuous pesticide
      if (icpst(j) == 0) then
        if (icpest(nro(j),ncpest(j),j) > 0 .and.                        &
     &                          iida >= icpest(nro(j),ncpest(j),j)) then
          icpst(j) = 1
          ndcpst(j) = 1
          iday_pest(j) = ipst_freq(nro(j),ncpest(j),j)
        else if (phuacc(j) > phucp(nro(j),ncpest(j),j)) then
          icpst(j) = 1
          ndcpst(j) = 1
          iday_pest(j) = ipst_freq(nro(j),ncpest(j),j)
        else
          return
        end if
      else
        !! if not first day of continuous fert increment total days of
        !! continuous fert by one
        ndcpst(j) = ndcpst(j) + 1
      end if

      if (iday_pest(j) == ipst_freq(nro(j),ncpest(j),j)) then
        !! apply pesticide
        !! reset frequency counter
        iday_pest(j) = 1

        !! initialize local variables
        kk = 0
        k = 0
        jj = 0
        xx = 0.
  
        kk = cpst_id(nro(j),ncpest(j),j)
        k = nope(kk)
        xx = cpst_kg(nro(j),ncpest(j),j)
        jj = inum1

  !! calculate amount of pesticide drifting onto main channel in subbasin
!       if (k == irtpest) then
!         drift(jj) = drift(jj) + xx * hru_km(j) * 100. * driftco(j) *   &
!     *                                                            1.e6
!       end if
!       xx = xx * ap_ef(kk) * (1. - driftco(j))
        xx = xx * ap_ef(kk) 

!! calculate ground cover
        gc = 0.
        gc = (1.99532 - Erfc(1.333 * laiday(j) - 2.)) / 2.1
        if (gc < 0.) gc = 0.

!! update pesticide levels on ground and foliage
        plt_pst(k,j) = plt_pst(k,j) + gc * xx
        sol_pst(k,j,1) = sol_pst(k,j,1) + (1. - gc) * xx
      else
        iday_pest(j) = iday_pest(j) + 1
      end if

!! summary calculations
      if (curyr > nyskip) then
        wshd_pstap(k) = wshd_pstap(k) + pst_kg(nro(j),ncpest(j),j) *     &
     &                                         ap_ef(kk) * hru_dafr(j)
      end if

!! check to set if continuous pesticide period is over
      if (ndcpst(j) == pest_days(nro(j),ncpest(j),j)) then
        icpst(j) = 0
        ndcpst(j) = 0
        iday_pest(j) = 0
        ncpest(j) = ncpest(j) + 1
      end if

      return
      end
