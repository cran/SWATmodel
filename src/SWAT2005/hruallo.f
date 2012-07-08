      subroutine hruallo(hru)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of management operation types, etc.
!!   used in the simulation. These values are used to allocate array sizes for
!!   processes occurring in the HRU.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none        |max number of applications
!!    mcr         |none        |max number of crops grown per year
!!    mcut        |none        |max number of cuttings per year
!!    mgr         |none        |max number of grazings per year
!!    mlyr        |none        |max number of soil layers
!!    mnr         |none        |max number of years of rotation
!!    pstflg(:)   |none        |flag for types of pesticide used in watershed
!!                             |array location is pesticide ID number
!!                             |0: pesticide not used
!!                             |1: pesticide used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_af       |none        |number of autofertilizer operations in mgt file
!!    ap_ai       |none        |number of autoirrigation operations in mgt file
!!    ap_cc       |none        |number of continuous cuuting operations in mgt
!!    ap_cf       |none        |number of continuous fertilization operations in mgt
!!    ap_ci       |none        |number of continuous irrigation operations in mgt
!!    ap_f        |none        |number of fertilizer operations in mgt file
!!    ap_i        |none        |number of irrigation operations in mgt file
!!    ap_p        |none        |number of pesticide operations in mgt file
!!    ap_r        |none        |number of release/impound operations in mgt file
!!    ap_s        |none        |number of sweep operations in mgt file
!!    ap_t        |none        |number of tillage operations in mgt file
!!    chmfile     |NA          |HRU soil chemical data file name (.chm)
!!    cut         |none        |number of harvest only operations in mgt file
!!    depth(:)    |mm          |depth to bottom of soil layer
!!    eof         |none        |end of file flag (=-1 if eof, else =0)
!!    grz         |none        |number of grazing operations in mgt file
!!    hkll        |none        |number of harvest/kill operations in mgt file
!!    hru         |none        |number of HRUs in subbasin
!!    hrufile     |NA          |name of HRU general data file name (.hru)
!!    ii          |none        |counter
!!    j           |none        |counter
!!    k           |none        |counter
!!    kll         |none        |number of kill operations in mgt file
!!    lyrtot      |none        |total number of layers in profile
!!    mgt_op      |none        |manangement operation code
!!    mgt1i       |none        |sixth parameter in mgt file operation line
!!    mgtfile     |NA          |HRU management data file name (.mgt)
!!    plt         |none        |number of plant operations in mgt file
!!    pstnum      |none        |pesticide ID number from database file
!!    rot         |none        |number of years in rotation used in HRU
!!    solfile     |NA          |HRU soil data file name (.sol)
!!    titldum     |NA          |input lines in .sub that are not processed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer, intent (in) :: hru
      character (len=13) :: hrufile, mgtfile, solfile, chmfile
      character (len=80) ::  titldum
      integer :: eof, j, k, lyrtot, rot, plt, ap_f, ap_p, ap_t, ap_i
      integer :: grz, cut, mgt1i, pstnum, ii, ap_r, ap_s, kll, hkll
      integer :: ap_ai, ap_af, mgt_op, ap_cf, ap_cc, ap_ci, jj
      real :: depth(25)

!! skip subbasin input data
      jj = 1
      read (2,6000) titldum
      do j = 1, 3
      read (2,6000) titldum
      mgtfile = ""
      solfile = ""
      chmfile = ""
      read (2,5300) hrufile, mgtfile, solfile, chmfile
        if (hrufile /= '             ') then
        call caps(mgtfile)
        call caps(solfile)
        call caps(chmfile)
        open (9,file=solfile,recl=350)
        !! calculate # of soil layers in profile
          depth = 0.
          lyrtot = 0
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6100) (depth(k), k = 1, 25)
          do k = 1, 25
            if (depth(k) <= 0.001) lyrtot = k - 1
            if (depth(k) <= 0.001) exit
          end do
          mlyr = Max(mlyr,lyrtot)
        open (10,file=mgtfile)
        !! calculate maximum number of years in a rotation
          rot = 0
          do k = 1, 28
          read (10,6000) titldum
          end do
          read (10,*) rot
          mnr = Max(mnr,rot)
          read (10,6000) titldum
        !! calculate maximum number of crops grown in a year
          ap_f = 0
          ap_t = 0
          ap_p = 0
          ap_r = 0
          ap_s = 0
          ap_i = 0
          ap_ai = 0
          ap_af = 0
          ap_cf = 0
          ap_cc = 0
          ap_ci = 0
          cut = 0
          grz = 0
          plt = 1
          kll = 0
          hkll = 0
          do k = 1, rot
            do
            mgt_op = 0
            mgt1i = 0
            read (10,6300) mgt_op, mgt1i
            select case (mgt_op)
             case (0) !! end of year flag
              mcr = Max(mcr,plt,kll,hkll)
              mapp = Max(mapp,ap_i,ap_f,ap_p,ap_t,ap_r,ap_s,ap_ai,ap_af)
              mapp = Max(mapp,plt,ap_cf,ap_ci)
              mgr = Max(mgr,grz)
              mcut = Max(mcut,cut,ap_cc)
                plt = 1 
                ap_i = 0
                ap_f = 0
                ap_p = 0
                ap_r = 0
                ap_s = 0
                ap_t = 0
                ap_ai = 0
                ap_af = 0
                ap_cf = 0
                ap_cc = 0
                ap_ci = 0
                cut = 0
                grz = 0
                kll = 0
                hkll = 0
                exit
              case (1) !!plant operation
                plt = plt + 1
              case (2) !! irrigation operation
                ap_i = ap_i + 1
              case (3) !! fertilizer operation
                ap_f = ap_f + 1
              case (4) !! pesticide operation
                ap_p = ap_p + 1
                if (mgt1i > 0) pstflg(mgt1i) = 1
              case (5) !! harvest/kill operation
                hkll = hkll + 1
              case (6) !! tillage operation
                ap_t = ap_t + 1
              case (7) !! harvest only operation
                cut = cut + 1
              case (8) !! kill operation
                kll = kll + 1
              case (9) !! grazing operation
                grz = grz + 1
              case (10) !! autoirr operation
                ap_ai = ap_ai + 1
              case (11) !! autofert operation
                ap_af = ap_af + 1
              case (12) !! sweep operation
                ap_s = ap_s + 1
              case (13) !! impound/release operation
                ap_r = ap_r + 1
              case (14) !! continuous fertilization
                ap_cf = ap_cf+ 1
              case (15) !! continuous cutting
                ap_cc = ap_cc + 1
              case (16) !! continuous irrigation
                ap_ci = ap_ci + 1
            end select
            end do
          end do
        open (11,file=chmfile)
          eof = 0
          do 
            do k = 1, 11
              read (11,6000,iostat=eof) titldum
              if (eof < 0) exit
            end do
            if (eof < 0) exit
            do
              pstnum = 0
              read (11,*,iostat=eof) pstnum
              if (eof < 0) exit
              if (pstnum > 0) pstflg(pstnum) = 1
            end do
            if (eof < 0) exit
          end do
        close (11)
        close (10)
        close (9)
        jj = jj + 1
        end if
      end do

      read (2,6000) titldum
      do j = jj, hru
        mgtfile = ""
        solfile = ""
        chmfile = ""
        read (2,5300) hrufile, mgtfile, solfile, chmfile
        call caps(mgtfile)
        call caps(solfile)
        call caps(chmfile)
        open (9,file=solfile,recl=350)
        !! calculate # of soil layers in profile
          depth = 0.
          lyrtot = 0
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6100) (depth(k), k = 1, 25)
          do k = 1, 25
            if (depth(k) <= 0.001) lyrtot = k - 1
            if (depth(k) <= 0.001) exit
          end do
          mlyr = Max(mlyr,lyrtot)
        open (10,file=mgtfile)
        !! calculate maximum number of years in a rotation
          rot = 0
          do k = 1, 28
            read (10,6000) titldum
          end do
          read (10,*) rot
          mnr = Max(mnr,rot)
        !! calculate maximum number of crops grown in a year
          read (10,6000) titldum
          ap_f = 0
          ap_t = 0
          ap_p = 0
          ap_r = 0
          ap_s = 0
          ap_i = 0
          ap_ai = 0
          ap_af = 0
          ap_cf = 0
          ap_cc = 0
          ap_ci = 0
          cut = 0
          grz = 0
          plt = 1
          kll = 0
          hkll = 0
          do k = 1, rot
            do
            mgt_op = 0
            mgt1i = 0
            read (10,6300) mgt_op, mgt1i
            select case (mgt_op)
             case (0) !! end of year flag
              mcr = Max(mcr,plt,kll,hkll)
              mapp = Max(mapp,ap_i,ap_f,ap_p,ap_t,ap_r,ap_s,ap_ai,ap_af)
              mapp = Max(mapp,plt,ap_cf,ap_ci)
              mgr = Max(mgr,grz)
              mcut = Max(mcut,cut,ap_cc)
                plt = 1 
                ap_i = 0
                ap_f = 0
                ap_p = 0
                ap_r = 0
                ap_s = 0
                ap_t = 0
                ap_ai = 0
                ap_af = 0
                ap_cf = 0
                ap_cc = 0
                ap_ci = 0
                cut = 0
                grz = 0
                kll = 0
                hkll = 0
                exit
              case (1) !!plant operation
                plt = plt + 1
              case (2) !! irrigation operation
                ap_i = ap_i + 1
              case (3) !! fertilizer operation
                ap_f = ap_f + 1
              case (4) !! pesticide operation
                ap_p = ap_p + 1
                if (mgt1i > 0) pstflg(mgt1i) = 1
              case (5) !! harvest/kill operation
                hkll = hkll + 1
              case (6) !! tillage operation
                ap_t = ap_t + 1
              case (7) !! harvest only operation
                cut = cut + 1
              case (8) !! kill operation
                kll = kll + 1
              case (9) !! grazing operation
                grz = grz + 1
              case (10) !! autoirr operation
                ap_ai = ap_ai + 1
              case (11) !! autofert operation
                ap_af = ap_af + 1
              case (12) !! sweep operation
                ap_s = ap_s + 1
              case (13) !! impound/release operation
                ap_r = ap_r + 1
              case (14) !! continuous fertilization
                ap_cf = ap_cf+ 1
              case (15) !! continuous cutting
                ap_cc = ap_cc + 1
              case (16) !! continuous irrigation
                ap_ci = ap_ci + 1
            end select
            end do
          end do
        open (11,file=chmfile)
          eof = 0
          do 
            do k = 1, 11
              read (11,6000,iostat=eof) titldum
              if (eof < 0) exit
            end do
            if (eof < 0) exit
            do
              pstnum = 0
              read (11,*,iostat=eof) pstnum
              if (eof < 0) exit
              if (pstnum > 0) pstflg(pstnum) = 1
            end do
            if (eof < 0) exit
          end do
        close (11)
        close (10)
        close (9)
      end do

      return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format(a)
 5100 format (20a4)
 5200 format (10i4)
 5300 format (6a13)
 6000 format (a80)
 6100 format (27x,25f12.2)
 6200 format (1x,i3)
 6300 format (16x,i2,1x,i4)
      end

