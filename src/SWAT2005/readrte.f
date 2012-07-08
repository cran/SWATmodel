      subroutine readrte

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the reach (main channel) input file 
!!    (.rte). This file contains data related to channel attributes. Only
!!    one reach file should be made for each subbasin. If multiple HRUs are
!!    modeled within a subbasin, the same .rte file should be listed for all
!!    HRUs in file.cio

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bnk(:)  |days        |alpha factor for bank storage recession curve
!!    alpha_bnke(:) |none        |Exp(-alpha_bnk(:))
!!    ch_cov(:)     |none        |channel cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_d(:)       |m           |average depth of main channel
!!    ch_di(:)      |m           |initial depth of main channel
!!    ch_erod(:)    |none        |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_k(2,:)     |mm/hr       |effective hydraulic conductivity of 
!!                               |main channel alluvium
!!    ch_l2(:)      |km          |length of main channel
!!    ch_li(:)      |km          |initial length of main channel
!!    ch_n(2,:)     |none        |Manning's "n" value for the main channel
!!    ch_onco(:)    |ppm         |channel organic n concentration
!!    ch_opco(:)    |ppm         |channel organic p concentration
!!    ch_s(2,:)     |m/m         |average slope of main channel
!!    ch_si(:)      |m/m         |initial slope of main channel
!!    ch_w(2,:)     |m           |average width of main channel
!!    ch_wdr(:)     |m/m         |channel width to depth ratio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line of .rte file (not used elsewhere)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ SUBROUTINES/FUNCTIONS ~ ~ ~ ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
 

      use parm

      character (len=80) :: titldum
      integer :: eof
  
      eof = 0
      do
      read (103,5000) titldum
      read (103,*) ch_w(2,i)
      read (103,*) ch_d(i) 
      read (103,*) ch_s(2,i) 
      read (103,*) ch_l2(i)
      read (103,*) ch_n(2,i) 
      read (103,*) ch_k(2,i) 
      read (103,*) ch_erod(i) 
      read (103,*,iostat=eof) ch_cov(i) 
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_wdr(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) alpha_bnk(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) icanal(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_onco(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_opco(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_san(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_sil(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_cla(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_veg(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_rcur(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_ss(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_fpr(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_eqn(i)
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_crht(i)
      if (eof < 0) exit
      read (103,5100,iostat=eof) (ch_erodmo(i,mo), mo = 1,12)
      exit
      end do

!!    set default values for parameters
      if (ch_erod(i) <= 0.0) ch_erod(i) = 0.0
      if (ch_erod(i) >= 1.0) ch_erod(i) = 1.0
      if (ch_s(2,i) <= 0.) ch_s(2,i) = .0001
      if (ch_n(2,i) <= 0.01) ch_n(2,i) = .01
      if (ch_n(2,i) >= 0.70) ch_n(2,i) = 0.70
      if (ch_l2(i) <= 0.) ch_l2(i) = .0010
      if (ch_wdr(i) <= 0.) ch_wdr(i) = 3.5

     
      sumerod = 0.
      do mo = 1, 12
        sumerod = sumerod + ch_erodmo(i,mo)
      end do

      if (sumerod < 1.e-6) then
        do mo = 1, 12
          ch_erodmo(i,mo) = ch_erod(i)
        end do
      end if

!!    set default values for mike van liew
      if (ch_onco(i) <= 0.) ch_onco(i) = ch_onco_bsn
      if (ch_opco(i) <= 0.) ch_opco(i) = ch_opco_bsn
!!    set default values for mike van liew


!!    initialize variables for channel degradation
      ch_di(i) = ch_d(i)
      ch_li(i) = ch_l2(i)
      ch_si(i) = ch_s(2,i)
      ch_wi(i) = ch_w(2,i)

      close (103)
      return
 5000 format (a)
5100  format (12f6.2)
      end
