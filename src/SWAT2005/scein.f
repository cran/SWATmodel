c::::::::::::::
c scein.for
c::::::::::::::

!     !  !         !         !         !         !         !         ! !
      subroutine scein(bbound,nopt,maxn,kstop,pcento,iseed,
     &iiname,
     &  ngs,npg,nps,nspl,mings,iniflg,iprint, iinr, inrhru,iobj,
     & calw, icalpar)
c
c   THIS SUBROUTINE READS AND PRINTS THE INPUT VARIABLES FOR
c   SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
c     -- Version 2.2
c
c   WRITTEN BY QINGYUN DUAN - UNIVERSITY OF ARIZONA, APRIL 1992
c
c

!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      integer icalpar(4,40), iobj
      real bl1, bu2
      integer  iiname(100), iinr(100), inrhru(100,2000)
      real*8 a(100), bbound(2,100), calw(100)
      common /iopar/ in,iprii
      character*10 pcntrl,deflt,usrsp
      character*4 reduc,initl,ysflg,noflg,xname(100)
      data deflt/' DEFAULT  '/
      data usrsp/'USER SPEC.'/
      data ysflg/'YES '/
      data noflg/'NO  '/
      data xname /'  X1','  X2','  X3','  X4','  X5','  X6','  X7',
     &'  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X16',
     &' X17',' X18',' X19',' X20',' X21',' X22',' X23',
     &' X24',' X25',' X26',' X27',' X28',' X29',' X30',' X31',' X32',
     &' X33',' X34',' X35',' X36',' X37',' X38',' X39',' X40',' X41',
     &' X42',' X43',' X44',' X45',' X46',' X47',' X48',' X49',' X50',
     &' X51',' X52',' X53',' X54',' X55',' X56',' X57',' X58',
     &' X59',' X60',' X61',' X62',' X63',' X64',' X65',' X66',' X67',
     &' X68',' X69',' X70',' X71',' X72',' X73',' X74',' X75',' X76',
     &' X77',' X78',' X79',' X80',' X81',' X82',' X83',' X84',' X85',
     &' X86',' X87',' X88',' X89',' X90',' X91',' X92',' X93',' X94',
     &' X95',' X96',' X97',' X98',' X99','X100'/
c
      write (9999,*) ' ENTER THE SCEIN SUBROUTINE --- '
c
c
c  INITIALIZE I/O VARIABLES



      in = 9998
      iprii = 9999
      open(unit=in,file='scein.dat',status='old')
      open(unit=iprii,file='sceout.dat',status='unknown')
      open(18012, file='calobjf.out')
      open(18013, file='calpar.out')
      open(18014, file='calgoc.out')
      open(18015, file='calstat.out')
      open(9997, file='calmet.dat')
c
      ierror = 0
      iwarn = 0
      bbound=0.

      write(iprii,700)
  700 format(10x,'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION',
     &       /,10x,46(1h=))
c
c
c  READ THE SCE CONTROL PARAMETERS

        do mm=1,40
!     !  !         !         !         !         !         !         ! !
      read(9997,997, end=996) icalpar(1,mm), icalpar(2,mm),icalpar(3,mm)
     *   ,icalpar(4,mm),calw(mm) 
        if (calw(mm).le.0.) calw(mm)=1.
        if (icalpar(1,mm).le.0) go to 996
        end do
996     continue
        iobj=mm-1
        write(8010,*) " objective codes are : "
        write(iprii,*) " objective codes are : "
        do mm = 1,iobj
        write (8010,997)icalpar(1,mm), icalpar(2,mm),icalpar(3,mm), 
     * icalpar(4,mm),calw(mm)
       write (iprii,997)icalpar(1,mm), icalpar(2,mm),icalpar(3,mm),
     * icalpar(4,mm),calw(mm)
       end do
997   format(4i3, f8.3)      
       ideflt = 0
      read(in,800) maxn,kstop,pcento,ngs,iseed,ideflt
  800 format(2i5,f5.2,3i5)
      if (iseed .eq. 0) iseed = 1969
c
c
c  IF ideflt IS EQUAL TO 1, READ THE SCE CONTROL PARAMETERS
      if (ideflt .eq. 1) Then
        read(in,810) npg,nps,nspl,mings,iniflg,iprint
  810   format(6i5)
        pcntrl = usrsp
      else
        read(in,*)
        pcntrl = deflt
      end if
        open (8014, file="help")
c
c
c  READ THE INITIAL PARAMETER VALUES AND THE PARAMETER BOUNDS
      iopt = 0
  820 iopt = iopt + 1

      read(in,830,end=840) bl1, bu2,in1, in2
       write(8010,832) iopt,bl1, bu2,in1, in2
       write(8014,1918) bl1, bu2
       bbound(1,iopt)=bl1
       bbound(2,iopt)=bu2
       iiname(iopt)=in1
       iinr(iopt)=in2
       if (in2.eq.0) go to 5566
       if (in2.gt.2000) go to 5566
       read(in,831,end=840) (inrhru(iopt,ii), ii=1,in2)
       write (8010,831) (iopt,inrhru(iopt,ii), ii=1,in2)
5566   continue      
111    format(2i5,a13)
  830 format(2f10.5,2i5)
  832 format(i4,2f10.5,2i5)
  831 format (50i5)

      go to 820
840   continue
      nopt=iopt-1
      noptmax=min(nopt,16)

1918   format(2f10.5)
       close(8014)
c
c  IF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO
c  THE DEFAULT VALUES
      if (ideflt .eq. 0) then
        npg = 2*noptmax + 1
        nps = noptmax + 1
        nspl = npg
        mings = ngs
        iniflg = 0
        iprint = 0
      end if
c
c
c  CHECK IF THE SCE CONTROL PARAMETERS ARE VALID
      if (ngs .lt. 1 .or. ngs .ge. 1320) then
        write(iprii,900) ngs
  900   format(//,1x,'**ERROR** NUMBER OF COMPLEXES IN INITIAL ',
     *         ' POPULATION ',i5,' IS NOT A VALID CHOICE')
        ierror = ierror + 1
      end if
c
      if (kstop .lt. 0 .or. kstop .ge. 20) then
        write(iprii,901) kstop
  901   format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN',
     *  ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE',
     *  ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2,
     *  ' WAS SPECIFIED.'/,13x,'BUT kstop = 5 WILL BE USED INSTEAD.')
        iwarn = iwarn + 1
        kstop=5
      end if
c
      if (mings .lt. 1 .or. mings .gt. ngs) then
        write(iprii,902) mings
  902   format(//,1x,'**WARNING** THE MINIMUM NUMBER OF COMPLEXES ',
     *         i2,' IS NOT A VALID CHOICE. SET IT TO DEFAULT')
        iwarn = iwarn + 1
        mings = ngs
      end if
c
      if (npg .lt. 2 .or. npg .gt. 1320/max(ngs,1)) then
        write(iprii,903) npg
  903   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A COMPLEX ',
     *         I4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        npg = 2*noptmax+1
        end if
c
      if (nps.lt.2 .or. nps.gt.npg .or. nps.gt.50) then
        write(iprii,904) nps
  904   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A SUB-',
     *  'COMPLEX ',i4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nps = noptmax + 1
      end if
c
      if (nspl .lt. 1) then
        write(iprii,905) nspl
  905   format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS ',
     *         'TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x,
     *         'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nspl = npg
      end if
c
c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION
      npt = ngs * npg
c
      if (npt .gt. 1320) then
        write(iprii,906) npt
  906   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN INITIAL ',
     *         'POPULATION ',i5,' EXCEED THE POPULATION LIMIT,',/,13x,
     *         'SET NGS TO 2, AND NPG, NPS AND NSPL TO DEFAULTS')
        iwarn = iwarn + 1
        ngs = 2
        npg = 2*nopt + 1
        nps = nopt + 1
        nspl = npg
      end if
c
c  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES
      if (ierror .ge. 1) write(iprii,907) ierror
  907 format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)
c
      if (iwarn .ge. 1) write(iprii,908) iwarn
  908 format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)
c
      if (mings .lt. ngs) then
        reduc = ysflg
      else
        reduc = noflg
      end if
c
      if (iniflg .ne. 0) then
        initl = ysflg
      else
        initl = noflg
      end if

c
c
c  PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS
  104 write(iprii,910)
  910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x,
     &'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x,
     &'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'SEED',/,
     &2x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))
c
      pcenta=pcento*100.
      write(iprii,912) pcntrl,maxn,pcenta,kstop,iseed
  912 format(3x,a10,7x,i5,10x,f3.1,9x,i2,9x,i5)
      write(iprii,914) ngs,npg,npt,nps,nspl
  914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=),
     &//,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER',
     &4x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.',
     &5x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x,
     &11(1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
      write(iprii,915) reduc,mings,initl
  915 format(//,15x,'COMPLX NO.',5x,'MIN COMPLEX',5x,'INI. POINT',/,
     &15x,'REDUCTION',6x,'NO. ALLOWED',6x,'INCLUDED',/,
     &15x,10(1h-),5x,11(1h-),5x,10(1h-),/,18x,a4,6x,i8,13x,a4)
      write(iprii,916)
  916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/,
     &       8x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x,
     &       'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x,
     &       11(1h-),5x,11(1h-))
      do 920 i = 1, nopt
        write(iprii,918) xname(i),i,bbound(1,i),bbound(2,i)
  918   format(4x,a4,16x,i4,2(6x,f10.3))
  920 continue
      if (ierror .ge. 1) then
      write(iprii,922)
  922 format(//,'*** THE OPTIMIZATION SEARCH IS NOT CONDUCTED BECAUSE',
     &       ' OF INPUT DATA ERROR ***')
      end if
c
C  END OF SUBROUTINE SCEIN

      return
      end
