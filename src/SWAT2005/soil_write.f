      subroutine soil_write

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes output to the output.sol file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, l
      real :: solp_t, solno3_t, solorgn_t, solorgp_t

      do j = 1,nhru
        solp_t = 0.
        solno3_t = 0.
        solorgn_t = 0.
        solorgp_t = 0.
         do l = 1,sol_nly(j)
           solp_t = solp_t + sol_solp(l,j)
           solno3_t = solno3_t + sol_no3(l,j)
           solorgn_t = solorgn_t + sol_orgn(l,j)
           solorgp_t = solorgp_t + sol_orgp(l,j)
         end do
      end do
      do j = 1,nhru
         write (121,1000) i, j, sol_rsd(1,nhru), solp_t, 
     *       solno3_t, solorgn_t, solorgp_t
      end do

      return
 1000 format ('SOL   ',2i4,1x,5f10.2)
      end
