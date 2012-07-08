      subroutine grass_wway
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the grass waterways                      
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm
      
      j = ihru
      k = ngrwat(j) + mhru

          chflow = surfq(j)                        

!!        compute channel peak rate using SCS triangular unit hydrograph
          chflow_m3 = 1000. * chflow * grwat_a(ihru)
          peakr = 2. * chflow_m3 / (1.5 * tc_gwat(j))
          if (peakr > phi(5,k)) then
            rcharea = phi(1,k)
            rchdep = grwat_d(j)
          else
!!          find the crossectional area and depth for volrt
!!          by iteration method at 1cm interval depth
!!          find the depth until the discharge rate is equal to volrt
            sdti = 0.
            rchdep = 0.
            Do While (sdti < peakr)
              rchdep = rchdep + 0.01
              rcharea = (phi(6,k) + chside(j) * rchdep) * rchdep
              p = phi(6,k) + 2. * rchdep * Sqrt(1. + chside(j) * 
     *           chside(j))
              rh = rcharea / p
              sdti = Qman(rcharea, rh, grwat_n(j), grwat_s(j))
            end do
          end if

!!        compute sediment yield with MUSLE
          sedin = (surfq(j) * peakr * 1000. * grwat_a(j) ** .56)  

!!        calculate flow velocity
          vc = 0.001
          if (rcharea > 1.e-4) then
            vc = peakr / rcharea
            if (vc > phi(9,k)) vc = phi(9,k)
          end if

!!        compute deposition and degradation in the channel
          cyin = 0.
          cych = 0.
          depnet = 0.
          deg = 0.
          dep = 0.
          if (chflow_m3 > 1.e-4) then
            cyin = sedin / chflow_m3
            cych = grwat_spcon(j) * vc ** 1.5
            depnet = chflow_m3 * (cych - cyin)
            if (depnet > 0.) then
              deg = depnet * ch_erod(j) * ch_cov(j)
              dep = 0.
            else
              dep = -depnet
              deg = 0.
            endif
          endif


      return
      end
