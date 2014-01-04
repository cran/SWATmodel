      subroutine surq_waterbalance
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine is the Water Balance Infiltration Routine
!!    per (Easton et al 2011)
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    sol_sumfc(:)  |mm H2O        |amount of water held in soil profile at
!!                                 |field capacity
!!    sol_sw(:)     |mm H2O        |amount of water stored in soil profile on
!!                                 |any given day
!!    fcimp(:)      |fraction      |fraction of HRU area that is classified
!!                                 |as directly connected impervious
!!    iurban(:)     |none          |urban simulation code:
!!                                 |0  no urban sections in HRU
!!                                 |1  urban sections in HRU, simulate using USGS
!!                                 |   regression equations
!!                                 |2  urban sections in HRU, simulate using build
!!                                 |   up/wash off algorithm
!!    precip        |mm H2O        |precipitation for the day in HRU
!!    urblu(:)      |none          |urban land type identification number from
!!                                 |urban.dat
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer on
!!                                 |the current day (less wp water)
!!    sol_ul(:,:)   |mm H2O        |amount of water held in the soil layer at
!!                                 |saturation
!!    sol_fc(:,:)   |mm H2O        |amount of water available to plants in soil 
!!                                 |layer at field capacity (fc - wp)
!!    sol_up(:,:)   |mm H2O/mm soil|water content of soil at -0.033 MPa (field
!!                                 |capacity)
!!    sol_sumul(:)  |mm H2O        |amount of water held in soil profile at
!!                                 |saturation
!!   ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    surfq(:)      |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j             |none          |HRU number
!!    j1            |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, j1

      j = 0
      j = ihru

      read(snam(j),'(4x,f4.2)') edc(j)
      sol_totpor(j)=sol_por(1,j)*sol_z(1,j)+sol_por(2,j)*
     &     (sol_z(2,j)-sol_z(1,j))
      sol_availst(j)=max(0.0,edc(j)*sol_totpor(j)-sol_sw(j)) 


      if (precipday <= sol_availst(j)) then
        surfq(j) = 0.0
      else
        surfq(j) = precipday - sol_availst(j)
      endif
      inflpcp = 0
      inflpcp = precipday - surfq(j)

      if (iurban(j) > 0) then
         surfqimp = 0.        
         surfqimp=precipday
         surfq = surfq * (1. - fcimp(urblu(j))) +                        &
     &                                        surfqimp * fcimp(urblu(j))
      endif


      return
      end
