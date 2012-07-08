      subroutine bmpinit

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine sets default values for urban bmp parameters

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dtp_onoff(:)   |none          |sub-basin detention pond is associated with
!!    dtp_iy(:)      |none          |year of the simulation that the reservoir 
!!                                  |becomes operational
!!    dtp_imo(:)     |none          |month the reservoir becomes operational
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_parm(:)    |none          |BMP outflow hydrograph shape parameter
!!    dtp_totwrwid(:)|m             |Total constructed width of the detention wall across
!!                                  |the creek
!!    dtp_stagdis(:) |none          |0=use weir/orifice discharge equation to calculate 
!!                                  |outflow, 1=use stage-dicharge relationship
!!    dtp_reltype(:) |none          |Equations for Stage-Discharge relationship,1=exponential 
!!                                  |function, 2=linear, 3=logarithmic, 4=cubic, 5=power   
!!    dtp_intcept(:) |none          |Intercept used in regression equations
!!    dtp_expont(:)  |none          |Exponent used in the exponential equation
!!    dtp_coef1(:)   |none          |Coefficient of 3rd degree in the polynomial equation
!!    dtp_coef2(:)   |none          |Coefficient of 2nd degree in the polynomial equation
!!    dtp_coef3(:)   |none          |Coefficient of 1st degree in the polynomial equation
!!    dtp_weirtype(:,:)|none        |Type of weir: 1=rectangular and 2=circular
!!    dtp_weirdim(:,:)|none         |Weir dimensions, 1=read user input, 0=use model calculation
!!    dtp_wdratio(:,:)|none         |Width depth ratio of rectangular weirs
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of orifice hole at different stages
!!    dtp_addon(:,:)  |m            |The distance between spillway levels
!!    dtp_flowrate(:,:)|m3/sec      |Maximum discharge from each stage of the weir/hole
!!    dtp_cdis(:,:)  |none          |Discharge coeffieicne for weir/orifice flow
!!    dtp_retperd(:,:)|years        |Return period at different stages
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    res_out(:,:,:) |m**3/day      |measured average daily outflow from the 
!!                                  |reservoir for the month (needed if IRESCO=1)
!!                                  |(read in as m**3/s and converted to m**3/day)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    k           |none          |counter
!!    lnvol       |none          |variable to hold denominator value
!!    mon         |none          |counter
!!    titldum     |NA            |title line in .det file (not used in program)
!!    resmto      |NA            |name of reservoir outflow file
!!                               |(needed if IDETCO = 2)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      integer :: k, eof
      real :: hwq,wqv,sub_ha

      eof = 0
      sub_ha = sub_km(i) * 100. 

      !! Detention pond
      !!----------------
      if (dtp_onoff(i)==1) then
         if (dtp_imo(i)<=0)      dtp_imo(i) = 1
         if (dtp_iyr(i)<=1000)   dtp_iyr(i) = iyr
   	   if (dtp_evrsv(i)<=0)    dtp_evrsv(i) = 0.1
         if (dtp_numweir(i)<=0)  dtp_numweir(i) = 1
         if (dtp_numstage(i)<=0) dtp_numstage(i) = 2

         !!	Estimating emergency spillway volumes if not entered by user
         do k=1,dtp_numstage(i)
            if (dtp_flowrate(i,k)<=0.0) then 
                dtp_flowrate(i,k) = 0.5 * 1000.0 * dtp_pcpret(i,k) 
     &            * subdr_km(i) / (idt*60.)
            end if

            if (dtp_flowrate(i,k)<0) then
          print *,"Error.. Could not estimate emergency spillway volume"
               print *,"Please enter necessary data in *.pnd input file"
               print *,"for subbasin : ",i
   !!            stop
            end if
         end do

         !!	Separate cumulative flow information to individual weir stages
         do k=2,dtp_numstage(i)
            dtp_flowrate(i,k) = (dtp_flowrate(i,k) 
     &         - sum(dtp_flowrate(i,1:k-1))) / dtp_numweir(i)
         end do
      	
         !!Estimate weir dimensions based on existing data 
         do k=1,dtp_numstage(i)
            if (dtp_weirdim(i,k)==0) then  !! Estimating weir dimensions
               if (dtp_weirtype(i,k)==2) then	!! choosing weir type
                  call est_orfdim(dtp_flowrate(i,k),dtp_diaweir(i,k)
     &                  ,dtp_cdis(i,k))
               else
                  call est_weirdim(dtp_wdratio(i,k),dtp_flowrate(i,k)
     &                  ,dtp_wrwid(i,k),dtp_depweir(i,k),dtp_cdis(i,k))
               end if 
            else
               if (dtp_weirtype(i,k)==1) then  !! reading user-entered data
                  dtp_wrwid(i,k) = dtp_wdratio(i,k) * dtp_depweir(i,k)
               end if  
            end if
         end do
      end if
 
      !! Wet pond
      !!----------
      if (wtp_onoff(i)==1) then
         if (wtp_imo(i)<=0)      wtp_imo(i) = 1
         if (wtp_iyr(i)<=1000)   wtp_iyr(i) = iyr
         if (wtp_k(i) <=0)       wtp_k(i) = 0.01
         if (wtp_evrsv(i)<=0)    wtp_evrsv(i) = 0.01
         if (wtp_evrsv(i)>1)     wtp_evrsv(i) = 0.99
         if (wtp_sdslope(i)<3)   wtp_sdslope(i) = 3. !COA Manual 1.6.6.C
         if (wtp_lenwdth(i)<2)   wtp_lenwdth(i) = 2. !COA Manual 1.6.6.C
         if (wtp_pdia(i)<=0)     wtp_pdia(i) = 0.1524 !meter(=6inches), COA Manual 1.6.6 
         if (wtp_plen(i)<=2)     wtp_plen(i) = 2 !meter
         if (wtp_pmann(i)<=0)    wtp_pmann(i) = 0.012 ! concrete surface
         if (wtp_ploss(i)<=0)    wtp_ploss(i) = 0
      end if

      !!Retention-Irrigation
      !!---------------------
      do k=1,num_ri(i)
         ! skip the pond that has zero inflow
         if (ri_fr(i,k)==0) cycle

         ! determine water quality volume for defult pond sizes
         !City of Austin Design Guideline 1.6.2

         hwq = (0.5 + sub_ha_urb(i) / sub_ha - 0.2) !inches
         wqv = hwq / 12. * sub_ha_urb(i) * ri_fr(i,k) * 107639.104167 !ft3
                  
         if (ri_dim(i,k)==0) then
           !Determine pond size automatically based on City of Austin's Design Guideline 1.6
            ri_vol(i,k) = wqv * 0.028317 !m3
      	      ri_dep(i,k)=1.5 !assume 1.5m as default retention pond depth
            ri_sa(i,k) = ri_vol(i,k) / ri_dep(i,k) 
            ri_dd (i,k)=60.0 !drawdown time, hr
            ri_k(i,k)=2.5
            ri_evrsv(i,k)=0.6
         else
            !Evaluate unit variables provided by user
            if (ri_sa(i,k)<1.or.ri_vol(i,k)<1) then
               !City of Austin Design Guideline 1.6.5
               ri_vol(i,k) = wqv * 0.028317 !m3
               ri_sa(i,k) = ri_vol(i,k) / ri_dep(i,k) 
            end if

      	   ri_dep(i,k) = ri_vol(i,k) / ri_sa(i,k)
      	   if (ri_dd(i,k)<=0) ri_dd (i,k)=72.0
      	   if (ri_k(i,k) <=0 ) ri_k(i,k)=2.5
      	   if (ri_evrsv(i,k) <=0 ) ri_evrsv(i,k)=0.1
      	   if (ri_dep(i,k)<=1) ri_dep(i,k)=1. 
         end if
         
         ! draw down time [number of time step]
         ri_ndt(i,k) = (ri_dd(i,k) - 12.) * 60 / idt  !minus the first dry 12 hours no-pumping 
            
         ! pumping rate that empties the basin in 72 hours with initial 12 hour of no operatrion
         ri_pumpv(i,k) = ri_vol(i,k) / ri_ndt(i,k) !m3/dt
            
         if (ri_im(i,k)<0.or.ri_im(i,k)>12) ri_im(i,k) = 0
         if (ri_iy(i,k)<1000.and.ri_iy(i,k)>0) ri_iy(i,k) = 0
         if (ri_iy(i,k)==0) ri_iy(i,k) = iyr
         if (ri_im(i,k)==0) ri_im(i,k) = 1   

      end do
  
      !!Sedimentation-Filtration
      !!---------------------
      do k=1,num_sf(i)
         !determine water quality volume for defult pond sizes
         !City of Austin Design Guideline 1.6.2
         hwq = (0.5 + sub_ha_urb(i) / sub_ha - 0.2) !inches
         wqv = hwq / 12. * sub_ha_urb(i) * sf_fr(i,k) * 107639.104167 !ft3
                  
         if (sf_dim(i,k)==0) then
           !Determine pond size automatically based on City of Austin's Design Guideline 1.6
            if (sf_typ(i,k)==1) then
               ! full scale 
               sp_pvol(i,k) = wqv * 0.028317 !m3
               sp_sa(i,k) = sp_pvol(i,k) / 1.5 !assume 1.5m depth for sed pond
               sp_pd(i,k) = sqrt(4. * 2. * sp_sa(i,k) * 1.5**0.5        
     &                 / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
               ft_sa(i,k) = wqv/(7.+2.33*4.) * 0.093 !m2
               ft_fsa(i,k) = 1. 
              
            else
               ! partial scale
               ft_sa(i,k) = wqv * 0.028317 !m3
!               wqv/(4.+1.33*4.) * 0.093
               
            end if
            ft_pd(i,k) = 1524. !mm
            ft_dep(i,k) = 420. !mm
            ft_h(i,k) = 1200. !mm
        
         else
         !Evaluate unit variables given by users
            if (sf_typ(i,k)>3) sf_typ(i,k) = 1
            if (sp_sa(i,k)<1) then
               !City of Austin Design Guideline 1.6.5
               sp_pvol(i,k) = wqv * 0.028317 !m3
               sp_sa(i,k) = sp_pvol(i,k) / 1.5 !assume 1.5m depth for sed pond
            end if
            if (sp_pd(i,k)<0.1) sp_pd(i,k) = 152.4 !mm diameter, equivalent to 6in
            if (ft_h(i,k)<5) ft_h(i,k) = 1200. !mm 
            if (ft_sa(i,k)<1) then
               if (sf_typ(i,k)==1) then
                  ft_sa(i,k) = wqv/(7.+2.33*ft_h(i,k)/304.8) * 0.093 !m2
                  ft_fsa(i,k) = 1.
               else
                  ft_sa(i,k) = wqv * 0.028317 !m3
                  ft_fsa(i,k) = 1. /(4.+1.33*ft_h(i,k)/304.8) 
               end if
            end if
         end if
         
         !Outflow control
         if (sp_qfg(i,k)==0) then
            sp_pd(i,k) = sqrt(4. * 2. * sp_sa(i,k) * 1.5**0.5           
     &           / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
         end if
         if (ft_qfg(i,k)==0) then
            ft_pd(i,k) = sqrt(4. * 2. * ft_sa(i,k) * 1.5**0.5           
     &          / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
         end if
         
         if (ft_dep(i,k)<100) ft_dep(i,k) = 100.
         if (sf_ptp(i,k)>1) sf_ptp(i,k) = 1
         if (sf_typ(i,k)==1) sf_ptp(i,k) = 0 !no filter outflow control for partial systems
         if (sp_pd(i,k)>254) sp_pd(i,k) = 254. ! max 10inches dia
         if (sp_pd(i,k)<10) sp_pd(i,k) = 10. ! min 10mm dia
         if (ft_pd(i,k)>254) ft_pd(i,k) = 254. ! max 10inches
         if (ft_k(i,k)<1) ft_k(i,k) = 1.
         if (ft_dp(i,k)<0.0001) ft_dp(i,k) = 0.02
         if (ft_dc(i,k)<0.01) ft_dc(i,k) = 0.762
         if (ft_por(i,k)<0.1) ft_por(i,k) = 0.45
         if (tss_den(i,k)<0.5) tss_den(i,k) = 0.5
         if (ft_alp(i,k)<0.1) ft_alp(i,k) = 0.1
         if (sf_im(i,k)<0.or.sf_im(i,k)>12) sf_im(i,k) = 0
         if (sf_iy(i,k)<1000.and.sf_iy(i,k)>0) sf_iy(i,k) = 0
         if (sf_iy(i,k)==0) sf_iy(i,k) = iyr
         if (sf_im(i,k)==0) sf_im(i,k) = 1   
      end do 
      
      return
      end
   !-------------------------------------------------------------------

         subroutine est_orfdim(desdis,dia,cd)

   !!	This program estimates orifice dimensions based on 
   !!	design discharge at different stages

         real, intent(in) :: cd,desdis
         real, intent(out) :: dia
         real :: tempvar,pi

         pi = 3.14159
         tempvar = 0.6 * cd * pi * sqrt(9.81)
         dia = (4.0 * desdis / tempvar) ** 0.4

         return
         end subroutine est_orfdim
   !-------------------------------------------------------------------

         subroutine est_weirdim(depwid,desdis,wwidth,wdepth,cd)

   !!	This program estimates rectangular weir dimensions based on 
   !!	width-depth ratio of wier at different stages

         real, intent(in) :: depwid,cd,desdis
         real, intent(out) :: wdepth,wwidth
      real :: tempvar
      
         tempvar=depwid**1.5
         wwidth=(desdis*tempvar/(1.84*cd))**0.4
         wdepth=wwidth/depwid

         return
         end subroutine est_weirdim
