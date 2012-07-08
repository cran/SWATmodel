      subroutine headout

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes the headings to the major output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hedb(:)     |NA            |column titles in subbasin output files
!!    hedr(:)     |NA            |column titles in reach output files
!!    hedrsv(:)   |NA            |column titles in reservoir output files
!!    heds(:)     |NA            |column titles in HRU output files
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in 
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ipdvab(:)   |none          |output variable codes for output.sub file
!!    ipdvar(:)   |none          |output variable codes for .rch file
!!    ipdvas(:)   |none          |output variable codes for output.hru file
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotb       |none          |number of output variables printed (output.sub)
!!    itotr       |none          |number of output variables printed (.rch)
!!    itots       |none          |number of output variables printed (output.hru)
!!    msubo       |none          |maximum number of variables written to
!!                               |subbasin output file (output.sub)
!!    mhruo       |none          |maximum number of variables written to 
!!                               |HRU output file (output.hru)
!!    mrcho       |none          |maximum number of variables written to
!!                               |reach output file (.rch)
!!    prog        |NA            |program name and version
!!    title       |NA            |title lines from file.cio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ilen        |none          |width of data columns in output file
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    header

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, ilen
      
      call header

!! write headings to HRU output file (output.hru)
      write (3,1000) prog, values(2), values(3), values(1), values(5),  &
     &               values(6), values(7)
      write (3,1010) title
      if (ipdvas(1) > 0) then
        write (3,1020) (heds(ipdvas(j)), j = 1, itots) !!custom printout
      else
        write (3,1020) (heds(j), j = 1, mhruo)         !!default printout
      endif
!! write headings to HRU output file (output2.hru)
      if (isproj == 1) then
      write (21,1000) prog, values(2), values(3), values(1), values(5), &
     &               values(6), values(7)
      write (21,1010) title
      if (ipdvas(1) > 0) then
        write (21,1020) (heds(ipdvas(j)), j = 1, itots) !!custom printout
      else
        write (21,1020) (heds(j), j = 1, mhruo)         !!default printout
      endif
      endif

!! write headings to VB interface HRU output file (hru.dat)
      ilen = 10
      if (ipdvas(1) > 0) then
        write (12,2000) (heds(ipdvas(j)), icols(ipdvas(j)), ilen,       &
     &                  j = 1, itots)
      else
        write (12,2000) (heds(j), icols(j), ilen, j = 1, mhruo)
      endif
      close (12)


!! write headings to subbasin output file (output.sub)
      write (9996,1000) prog,values(2),values(3),values(1), values(5),  &
     &                values(6), values(7)
      write (9996,1010) title
      if (ipdvab(1) > 0) then
        write (9996,1030) (hedb(ipdvab(j)), j = 1, itotb) !!custom printout
      else
        write (9996,1030) (hedb(j), j = 1, msubo)         !!default printout
      endif

!! write headings to VB interface subbasin output file (sub.dat)
      ilen = 10
      if (ipdvab(1) > 0) then
        write (13,2000) (hedb(ipdvab(j)), icolb(ipdvab(j)), ilen,       &
     &                  j = 1, itotb)
      else
        write (13,2000) (hedb(j), icolb(j), ilen, j = 1, msubo)
      endif
      close (13)


!! write headings to reach output file (output.rch)
      write (7,1000) prog, values(2), values(3), values(1), values(5),  &
     &               values(6), values(7)
      write (7,1010) title
      if (ipdvar(1) > 0) then
        write (7,1040) (hedr(ipdvar(j)), j = 1, itotr)  !! custom printout
      else
        write (7,1040) (hedr(j), j = 1, mrcho)          !! default printout
      endif 
!! write headings to reach output file (output2.rch)
      if (isproj == 1) then
      write (20,1000) prog, values(2), values(3), values(1), values(5), &
     &               values(6), values(7)
      write (20,1010) title
      if (ipdvar(1) > 0) then
        write (20,1040) (hedr(ipdvar(j)), j = 1, itotr)  !! custom printout
      else
        write (20,1040) (hedr(j), j = 1, mrcho)          !! default printout
      endif 
      endif 

!! write headings to VB interface reach output file (rch.dat)
      ilen = 12
      if (ipdvar(1) > 0) then
        write (11,2000) (hedr(ipdvar(j)), icolr(ipdvar(j)), ilen,       &
     &                  j = 1, itotr)
      else
        write (11,2000) (hedr(j), icolr(j), ilen, j = 1, mrcho)
      endif
      close (11)


!! write headings to reservoir output file (output.rsv)
      write (8,1000) prog, values(2), values(3), values(1), values(5),  &
     &               values(6), values(7)
      write (8,1010) title
      write (8,1050) (hedrsv(j), j = 1, 41)
!! write headings to reservoir output file (output2.rsv)
      if (isproj == 1) then
      write (22,1000) prog, values(2), values(3), values(1), values(5), &
     &               values(6), values(7)
      write (22,1010) title
      write (22,1050) (hedrsv(j), j = 1, 41)
      end if

!!    write headings to VB interface reservoir output file (rsv.dat)
      ilen = 12
      write (14,2000) (hedrsv(j), icolrsv(j-1), ilen, j = 2, 19)
      close (14)

 
!! write headings to HRU impoundment output file (output.wtr)
      write (4,1000) prog, values(2), values(3), values(1), values(5),  &
     &                values(6), values(7)
      write (4,1010) title
      write (4,1020) (hedwtr(j), j = 1, 40)

!! write headings to pesticide output file (output.pst)
      if (iprp /= 0) then
        write (9995,1000) prog,values(2),values(3),values(1),values(5), &
     &                values(6), values(7)
        write (9995,1010) title
        write (9995,3000)
        write (9995,3001) (npno(j),npno(j), j = 1, npmx)
        write (9995,3002) (pname(npno(j)),pname(npno(j)), j = 1, npmx)
        write (9995,3003) (("SOLUBLE mg       SORBED mg"), j = 1, npmx)
      end if

      return
 1000 format ('1',/t5,a80,t105,2(i2,'/'),i4,5x,2(i2,':'),i2)
 1010 format (/(t5,20a4))
 1020 format (//'LULC HRU      GIS  SUB  MGT  MON','   AREAkm2',70(a10))
 1030 format (//6x,' SUB      GIS  MON   AREAkm2',21(a10))
 1040 format (//7x,'RCH      GIS   MON     AREAkm2',54a12)
 1050 format (//6x,'     RES  MON',41a12)
 1060 format (//6x,'RCH GIS  MON',26a12)
 2000 format (a12,12x,i4,4x,i4)
 3000 format ("Pesticide loadings to main channel by HRU",/)
 3001 format ("Pesticide #",250(13x,i3,1x))
 3002 format ("Pesticide name:      ",250(a16,1x))
 3003 format (2x,'HRU YEAR MON',7x,125(a26,8x))
      end
