      subroutine canopyint

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes canopy interception of rainfall
!!    used for methods other than curve number

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blai(:)     |none          |maximum (potential) leaf area index
!!    canmx(:)    |mm H2O        |maximum canopy storage
!!    canstor(:)  |mm H2O        |amount of water held in canopy storage
!!    hhprecip(:) |mm H2O        |precipitation for hour in HRU
!!    icr(:)      |none          |sequence number of crop grown within a year
!!    idplt(:,:,:)|none          |land cover code from crop.dat
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihru        |none          |HRU number
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    nro(:)      |none          |sequence number of year in rotation
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    precipdt(:) |mm H2O        |precipitation in time step for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canstor(:)  |mm H2O        |amount of water held in canopy storage
!!    hhprecip(:) |mm H2O        |precipitation reaching soil surface in hour
!!    precipday   |mm H2O        |precipitation reaching soil surface
!!    precipdt(:) |mm H2O        |precipitation reaching soil surface in
!!                               |time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canmxl      |mm H2O        |maximum canopy storage at current day's leaf
!!                               |area
!!    canstori    |mm H2O        |initial canopy storage water content
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    xx          |mm H2O        |precipitation prior to canopy interception 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer :: j, ii
      real :: xx, canmxl, canstori

      j = 0
      j = ihru

      if (blai(idplt(nro(j),icr(j),j)) < 0.001) return

      select case (ievent)
        case (2,3)

          canstori = 0.
          canmxl = 0.
          canstori = canstor(j)
          canmxl = canmx(j) * laiday(j) / blai(idplt(nro(j),icr(j),j))
          do ii = 2, nstep+1
            xx = 0.
            xx = precipdt(ii)
            precipdt(ii) = precipdt(ii) - (canmxl - canstor(j))

            if (precipdt(ii) < 0.) then
              canstor(j) = canstor(j) + xx
              precipdt(ii) = 0.
            else
              canstor(j) = canmxl
            endif
          end do
          if (canstor(j) > canstori) then
            do ii = 1, 24
              xx = 0.
              xx = hhprecip(ii)
              hhprecip(ii) = hhprecip(ii) - (canstor(j) - canstori)

              if (hhprecip(ii) < 0.) then
                canstori = canstori + xx
                hhprecip(ii) = 0.
              else
                canstori = canstor(j)
              endif
            end do
          end if

        case default
          xx = 0.
          canmxl = 0.
          xx = precipday
          canmxl = canmx(j) * laiday(j) / blai(idplt(nro(j),icr(j),j))
          precipday = precipday - (canmxl - canstor(j))
          if (precipday < 0.) then
            canstor(j) = canstor(j) + xx
            precipday = 0.
          else
            canstor(j) = canmxl
          endif
       end select

      return
      end
