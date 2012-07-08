      subroutine schedule_ops
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida           |julian date   |day being simulated (current julian date)
!!    inum1          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, plant_no

      j = 0
      j = ihru


        iops = ioper(ihru)

        do while (iida == iopday(iops,ihru).and.iyr == iopyr(iops,ihru)) 

        select case(mgt_ops(iops,ihru))

        case (0)
     
        case (1)
         xm = 0.6 * (1. - Exp(-35.835 * hru_slp(ihru)))    
         sin_sl = Sin(Atan(hru_slp(ihru)))
         usle_ls(ihru) = (terr_sl(iops,ihru)/22.128) ** xm * (65.41 *   & 
     &      sin_sl * sin_sl + 4.56 * sin_sl + .065)
         usle_mult(ihru) = rock(ihru) * usle_k(ihru) * terr_p(iops,ihru)&
     &      * usle_ls(ihru) * 11.8
        if (terr_cn(iops,ihru) > 1.e-6) then
           call curno(terr_cn(iops,ihru),ihru)
        end if

         case (2)
         ddrain(ihru) = drain_d(iops,ihru)
         gdrain(ihru) = drain_g(iops,ihru)
         tdrain(ihru) = drain_t(iops,ihru)
         dep_imp(ihru) = drain_idep(iops,ihru)

         case (3)
         usle_mult(ihru) = usle_mult(ihru) * cont_p(iops,ihru) /        &
     &      usle_p(ihru)
         call curno(cont_cn(iops,ihru),ihru)

         case (4)
         filterw(ihru) = filt_w(iops,ihru)

         case (5)
         call curno(strip_cn(iops,ihru),ihru)
         usle_mult(ihru) = usle_mult(ihru) * strip_p(iops,ihru) /       &
     &      usle_p(ihru)
         tover = .0556 * (slsubbsn(j) * strip_n(iops,ihru)) ** .6 /     &
     &      hru_slp(j) ** .3  

         tconc(ihru) = tconc(ihru) + tover - t_ov(ihru)

!       ioper(ihru) = ioper(ihru) + 1
!       iops = ioper(ihru)

        case (6)
         call curno (fire_cn(iops,ihru),ihru)

        case (7)
           ngrwat(ihru) = ngrwat(ihru) + 1
           grwat_n(ihru) = gwatn(iops,ihru)
           grwat_i(ihru) = gwati(iops,ihru)
           grwat_l(ihru) = gwatl(iops,ihru)
           grwat_w(ihru) = gwatw(iops,ihru)
           grwat_d(ihru) = gwatd(iops,ihru)
           grwat_veg(ihru) = gwatveg(iops,ihru)
           grwat_s(ihru) = gwats(iops,ihru)
           grwat_a(ihru) = gwata(iops,ihru)
           grwat_spcon(ihru) = gwatspcon(iops,ihru)
           tch = .62 * grwat_l(ihru) * grwat_n(ihru) ** .6 / 
     *      (grwat_a(ihru) ** .125 * grwat_s(ihru) ** .375)
           tc_gwat(ihru) = tch + t_ov(ihru)
           k = mhru + ngrwat(ihru)
           call ttcoef(j)

        case (8)
           plant_no = cropno_upd(iops,ihru)
           blai(plant_no) = laimx_upd(iops,ihru)
           hvsti(plant_no) = hi_upd(iops,ihru)


         end select

           ioper(ihru) = ioper(ihru) + 1
           iops = ioper(ihru)

      end do

      return
      end
