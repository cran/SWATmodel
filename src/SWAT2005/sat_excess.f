      subroutine sat_excess(j1)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icrk        |none          |crack flow code
!!                               |1 simulate crack flow in watershed
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    nn          |none          |number of soil layers
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp)
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flat(:,:)   |mm H2O        |lateral flow storage array
!!    latlyr      |mm H2O        |lateral flow in soil layer for the day
!!    latq        |mm H2O        |total lateral flow in soil profile for the 
!!                               |day in HRU
!!    lyrtile     |mm H2O        |drainage tile flow in soil layer for day
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sep         |mm H2O        |micropore percolation from soil layer
!!    sepbtm      |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on current day
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, j1
      real:: ul_excess

      j = 0
      j = ihru

      if (j1 < sol_nly(j)) then
        if (sol_st(j1,j) - sol_ul(j1,j) > 1.e-4) then
          sepday = sepday + (sol_st(j1,j) - sol_ul(j1,j))
          sol_st(j1,j) = sol_ul(j1,j)
        end if
      else

        if (sol_st(j1,j) - sol_ul(j1,j) > 1.e-4) then
          ul_excess = sol_st(j1,j) - sol_ul(j1,j)
          sol_st(j1,j) = sol_ul(j1,j)
          nn = sol_nly(j)
          do ly = nn - 1, 1, -1
            sol_st(ly,j) = sol_st(ly,j) + ul_excess
            if (sol_st(ly,j) > sol_ul(ly,j)) then
              ul_excess = sol_st(ly,j) - sol_ul(ly,j)
              sol_st(ly,j) = sol_ul(ly,j)
            else
              ul_excess = 0.
              exit
            end if
            if (j1 == 1 .and. ul_excess > 0.) then
              !! add ul_excess to depressional storage and then to surfq
            end if
          end do
          !compute tile flow again after saturation redistribution
!         if (ldrain(j) > 0.) then
!           ul_excess = sol_st(ldrain(j),j) - sol_fc(ldrain(j),j) 
!           if (ul_excess > 0.) then
!             lyrtilex = ul_excess * (1. - Exp(-24. / tdrain(j)))
!           end if
!         end if
        end if
      end if

      return
      end
