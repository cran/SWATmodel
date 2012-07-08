      subroutine readops
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin management input file
!!    (.mgt). This file contains data related to management practices used in
!!    the HRU/subbasin.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                 |urban.dat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                    |daily
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    day         |none          |day operation occurs
!!    mgt_op      |none          |operation code number
!!                               |0 end of rotation year
!!                               |1 plant/beginning of growing season
!!    mgt1i       |none          |first management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt2i       |none          |second management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt3i       |none          |third management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt4        |none          |fourth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt5        |none          |fifth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt6        |none          |sixth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt7        |none          |seventh management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt8        |none          |eighth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt9        |none          |ninth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    titldum     |NA            |title line from input dataset
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Jdt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      integer :: eof
      integer :: mon, day, mgt_op, mgt2i, mgt1i
      real :: mgt6, mgt9, mgt4, mgt5, mgt7, mgt8

      read (111,5000) titldum

!!      read scheduled operations

          iops = 0

          do
          mon = 0
          day = 0
          year = 0.
          mgt_op = 0
          mgt1i = 0
          mgt2i = 0
          mgt3i = 0.
          mgt4 = 0.
          mgt5 = 0.
          mgt6 = 0
          mgt7 = 0.
          mgt8 = 0.

          read (111,5200,iostat=eof) mon, day, iyear, mgt_op, mgt1i,    &
     & mgt2i, mgt3, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10
          if (eof < 0) exit
          iops = iops + 1
          iopday(iops,ihru) = Jdt (ndays,day,mon)
          iopyr(iops,ihru) = iyear                  
          mgt_ops(iops,ihru) = mgt_op                

          select case (mgt_op)

          case (0)  !! end of operations file

          case (1)  !! terracing        
             terr_p(iops,ihru) = mgt4
             terr_cn(iops,ihru) = mgt5
             terr_sl(iops,ihru) = mgt6

          case (2)  !! tile drainage            
             drain_d(iops,ihru) = mgt4
             drain_t(iops,ihru) = mgt5
             drain_g(iops,ihru) = mgt6
             if (mgt7 < 1.e-6) mgt7 = 6000.0
             drain_idep(iops,ihru) = mgt7
        
          case (3)  !! contouring           
             cont_cn(iops,ihru) = mgt4
             cont_p(iops,ihru) =  mgt5

          case (4)  !! filter                   
             filt_w(iops,ihru) = mgt4

          case (5)  !! strip cropping
             strip_n(iops,ihru) = mgt4
             strip_cn(iops,ihru) = mgt5
             strip_c(iops,ihru) = mgt6
             strip_p(iops,ihru) = mgt7

          case (6)  !! fire
             fire_cn(iops,ihru) = mgt4

          case (7)  !! grass waterways
             gwati(iops,ihru) = mgt1i
             gwata(iops,ihru) = mgt3
             gwatn(iops,ihru) = mgt4
             gwatl(iops,ihru) = mgt5
             gwatw(iops,ihru) = mgt6
             gwatd(iops,ihru) = mgt7
             gwatveg(iops,ihru) = mgt8
             gwats(iops,ihru) = mgt9
             gwatspcon(iops,ihru) = mgt10

          case (8) !! plant parameter update
             cropno_upd(iops,ihru) = mgt1i
             hi_upd(iops,ihru) = mgt4
             laimx_upd(iops,ihru) = mgt5

          end select
          end do


      close (111)
     
      return
 5000 format (a)
 5200 format (1x,i2,1x,i2,5x,i4,1x,i2,1x,i4,1x,i3,1x,f6.2,1x,f12.5,1x,    &
     &        f6.2,1x,f11.5,1x,f8.2,1x,f6.2,1x,2f5.2)
      end
