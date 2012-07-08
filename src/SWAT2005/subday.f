      subroutine subday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha            |area of watershed in hectares
!!    iida          |julian date   |current day of simulation
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_etday(:)  |mm H2O        |actual evapotranspiration on day in subbasin
!!    sub_fr(:)     |none          |fraction of watershed area in subbasin
!!    sub_gwq(:)    |mm H2O        |groundwater flow on day in subbasin
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_no3(:)    |kg N/ha       |NO3-N in surface runoff on day in subbasin
!!    sub_pet(:)    |mm H2O        |potential evapotranspiration for day in
!!                                 |subbasin
!!    sub_qd(:)     |mm H2O        |surface runoff loading to main channel on
!!                                 |day in subbasin
!!    sub_sedpa(:)  |kg P/ha       |amount of active mineral P attached to 
!!                                 |sediment removed in surface runoff on day 
!!                                 |in subbasin
!!    sub_sedps(:)  |kg P/ha       |amount of stable mineral P attached to 
!!                                 |sediment removed in surface runoff on day 
!!                                 |in subbasin
!!    sub_sedy(:)   |metric tons   |sediment yield for the day in subbasin
!!    sub_sep(:)    |mm H2O        |seepage from bottom of soil profile on day
!!                                 |in subbasin
!!    sub_snom(:)   |mm H2O        |snow melt on day in subbasin
!!    sub_solp(:)   |kg P/ha       |soluble P in surface runoff on day in 
!!                                 |subbasin
!!    sub_subp(:)   |mm H2O        |precipitation for day in subbasin
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    sub_wyld(:)   |mm H2O        |water yield on day in subbasin
!!    sub_yorgn(:)  |kg N/ha       |organic N in surface runoff on day in 
!!                                 |subbasin
!!    sub_yorgp(:)  |kg P/ha       |organic P in surface runoff on day in 
!!                                 |subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    sub_ha      |ha            |area of subbasin in hectares
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: sb, ii
      real :: sub_ha
      real, dimension (msubo) :: pdvab, pdvb

      sb = 0
      sb = hru_sub(ihru)

      sub_ha = 0.
      sub_ha = da_ha * sub_fr(sb)

      pdvab = 0.
      pdvb = 0.

      pdvab(1) = sub_subp(sb)
      pdvab(2) = sub_snom(sb)
      pdvab(3) = sub_pet(sb)
      pdvab(4) = sub_etday(sb)
      pdvab(5) = sub_sw(sb)
      pdvab(6) = sub_sep(sb)
      pdvab(7) = sub_qd(sb)
      pdvab(8) = sub_gwq(sb)
      pdvab(9) = sub_wyld(sb)
      pdvab(10) = sub_sedy(sb)/ sub_ha
      pdvab(11) = sub_yorgn(sb)
      pdvab(12) = sub_yorgp(sb)
      pdvab(13) = sub_no3(sb)
      pdvab(14) = sub_solp(sb)
      pdvab(15) = sub_sedpa(sb) + sub_sedps(sb)
      pdvab(16) = sub_latq(sb)
      pdvab(17) = sub_latno3(sb)
      pdvab(18) = sub_gwno3(sb)
!!    added for jennifer b.
!     pdvab(19) = chl_a(sb)
!     pdvab(20) = cbodu(sb)
!     pdvab(21) = doxq(sb)
      pdvab(19) = sub_chl(sb)
      pdvab(20) = sub_cbod(sb)
      pdvab(21) = sub_dox(sb)

      if (ipdvab(1) > 0) then
        do ii = 1, itotb
          pdvb(ii) = pdvab(ipdvab(ii))
        end do
        write (9996,1000) sb, subgis(sb), iida, sub_km(sb),             &
     &                                        (pdvb(ii), ii = 1, itotb)
      else
        write (9996,1000) sb, subgis(sb), iida, sub_km(sb),             &
     &                                        (pdvab(ii), ii = 1, msubo)
      end if

      return
!     changed for jennifer b.
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,18f10.3)
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,21f10.3)
 1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,18f10.3,1x,e10.5,2f10.3)
      end 
