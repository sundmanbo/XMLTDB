Module xtdblib
! program to read an xml file, in particular XTDB
  implicit none
#include "gtp3_xml.F90"

!  integer bmperr
! current number of nested tags, line number, used positions in attpos
!  integer fline,attpos
  integer attpos
! maximum number of nested tags
  integer, parameter :: maxlevels=10,commenttag=999
! tagnest(level) is negative, -tagno, if more attributes for tagno to read 
  integer, dimension(maxlevels) :: tagnest
! tagnames have max length 18, the endoftag is set to '</tagname>'
  character(len=21), dimension(maxlevels) :: endoftag
!   
! TPfuns are used in parameters. All TPfun for entered parameters must be found
    integer, parameter :: maxtpfun=500
    integer ntpfun,ntp
! alltpfun are names of all tpfuns missing or entered
    character*16, dimension(maxtpfun) :: alltpfun
! statustpfun -1 if missing, 1 if entered in OC
    integer, dimension(maxtpfun) :: statustpfun
! expression may be concatinated from TPfun/Parameter and Trange tags
!    character*1024 wholexpr
    character (len=:), allocatable :: wholexpr

contains
    
! subroutines and functions
!
! xtdbread(filename) 
! read a whole database
!
!*  xtdbgetel(filename,elnames) 
! extract all element names
!
!*  xtdbgetsys(filename,elnames,phases) 
! extract data for all or some phase that can exist for specified elements
!
!*  xtdbgetdata(unit,tags,attributes)
! extract data for phases that can exist for the specified elements
! and store them in the software for use in calculations.
! This routine may have to read/rewind the file several times.
!
!*  xtdbgettp(unit,TPlist,attribut) 
! extract all TPfun matching any in TPlist. Called from xtdbgetdata
!
! xtdbtag(unit,fline,tagname,matt,pretag,attributes)
! extract all attributes of a tag from the file in sequential order.
! fline is the line number.  The tag can depend om other nested tags.
!
! logical function xeolch(line,ip)
! Find first character in line by skipping spaces and TAB.
!
! logical function get1att(line,ip,attname,value)
! extract the values of the attribute attname sequatially from line.
! The initial value of ip is irrelevant and it is incremented for one.
!
! logical function check_mpid(mpid,phase)
! Check that phase has a model corresponding to the MPID of a parameter
!
! If the XML is ordered ELEMENTS, SPECIES, PHASES, PARAMETER, TPFUN 
! A fist sequential read will pick up all phases parameters 
! but many TPfuns call other TP funs and it may be necessary to read
! and rewind the file several times to get all.
! The xtdbextracttp can be called with an array of TPfun and extract
! several at each sequential read.
!
!--------------------------------------------------------
  
  subroutine xtdbread(filename)
! testing read xml files extraction tags and attributes including nesting
    character*(*) filename
! line is line read from file, bigline is concatinated input line
    character line*256,tagname*64
! tag attributes
    character attributes*1024
! for nested tags level is current level,  matt=0 if tagend missing (nesting)
    character pretag*24,values*256,attname*18
    integer tagno,matt,unit,ll,fline
! for storing data during nested tags, each at a higher level
    character*256, dimension(5) ::  saveatt
! tags with nested tagas are TPfun 1: Trange
!    character phaseid*24
! Phase sublattices, 10xconstituents, crystalstructure, amendphase, disord ...)
    character phid*24,phconfig*24,phconst(10)*256,phmodels*128,phstate*1
    integer phsub,isub(10)
    double precision phmult(10),nsub(10)
! extending an allocated variable
!  extending an allocated (data(m))
!     data = [ data, ( 0, kk=1,n ) ]
!      
! for tempraty storage of phase data
    integer phnsub,phisub
 
    character (len=:), allocatable :: lowt, default_lowt,tpfun
    character (len=:), allocatable :: hight,default_hight,expr

    character (len=:), allocatable :: parid,bibref

    character*16 usedtpfun   ! is tpfun found in Expr of TPfun or parameter

! handling of AppendXTDB
! The Models must be read first and only once
! The Parameters after reading the phases and only once
! The TPfun after all paramaters and maybe several times
! The Bibiliography read last and and only once
    character*64 appfiles(4)
    integer appfiledone(5),natt
!
! for various purposes
    integer ip,iq,ir,is,it,level,prevlev,lk,kk,semicolon
!
! software defaults
    default_lowt='298.15'
    default_hight='6000'
!
! data for missing and found AppendXTDB
    appfiledone=0
    ntpfun=0
    alltpfun(1)=' '
    statustpfun(1)=-1
!    write(*,3)ntpfun,alltpfun(1),statustpfun(1)
3   format('Alltpfun: ',i2,' "',a,'" ',i2)
!
    write(*,*)'Opening XTDB file: ',trim(filename)
    unit=21
    open(unit,file=filename,&
         access='sequential',form='formatted',status='old')
! zero file line number
    fline=0
! zero position in characters containing attributes
    attpos=0
! initiate tag nesting
    level=0
    matt=0
    pretag=' '
    xtdberr=0
    readall: do while(.true.)
! only one tag per line, attributes can be on following lines
       if(line(1:2).eq.'<?') then
          write(*,*)'Ignoring tag <?'
          fline=fline+1
          cycle readall
       endif
!--------------------------
! the call below will extract lines until the end of arrributes of a tag
17     continue
       if(fline.lt.0) exit readall
       call xtdbtag(unit,fline,tagname,matt,pretag,attributes)
       if(xtdberr.ne.0) goto 990
! matt= +1 new tag nest, 0 tag finished, -1 tag nest finished
!       write(*,44)trim(tagname),trim(pretag),matt,fline
44     format('44 Back from xtdbtag: "',a,'"  pretag: "',a,'"',i3,i7)
!
       if(matt.eq.1) then
! if matt=1 means new tag with attributes but no endoftag, increase level
          level=level+1
          saveatt(level)=attributes
          endoftag(level)=tagname
          pretag='</'//trim(tagname)//'>'
!========================= handling end of nested tags
       elseif(matt.eq.-1) then
!========================= nested TPfun =========
! if matt=-1 the line is end of nested pretag, decrease level !!??
!          write(*,*)' >>>Nested tag end: "',&
!               trim(tagname),'" "',trim(pretag),'" ',level
          if(trim(pretag).eq.'</XTDB>') then
             write(*,*)'Found end of XTDB database'
             goto 990
          elseif(trim(pretag).eq.'</TPfun>') then
! The end of a nested TPfun tag, simple TPfun are entered below
! check if the TPfun is needed, tpfun is Id of <TPfun tag, 
             call xtdbentertpfun(tpfun)!
! this call will be removed after testing as we only enter missing TPfuns
             call xtdbOCfun('TPfun ',tpfun)
!             write(*,15)trim(tpfun),matt,level,fline,trim(wholexpr)
15           format('   Merged TPfun: "',a,'"',3i3,' is:'/a)
          elseif(trim(pretag).eq.'</Phase>') then
!=========================== end of Phase, always nested
! The end of Phase tag (always nested)             
             wholexpr=phrec%confent//' '//phrec%state//' '//phrec%clist(1)%list
             call xtdbOCphase(phrec)
!             write(*,201)fline,phrec%id,phrec%confent,phrec%state,&
!                  phnsub,phrec%mult
201          format(/'Collected all phase data: ',i7/&
                  'Phase name: ',a,5x,a,5x,a,5x,i2,5x,a)
!             do ip=1,phnsub
!                write(*,202)phrec%clist(ip)%subx,phrec%clist(ip)%list
202             format('Sublattice: ',a,'  Constituents: ',a)
!             enddo
!
!             if(allocated(phrec%amendph)) write(*,203)phrec%amendph
203          format('Models: ',a)
! finish with empty line
             write(*,*)
!>>>>>>> create record for phase and store data in OC <<<<<<<<<<<<<
! clear for next phase by deallocate the whole phrec
             deallocate(phrec)
          elseif(trim(pretag).eq.'</Sublattices>') then
!============================ end of Sublattices tag
! Update level here for next tag, done separately here as code is messy
! Note that the <Constituent tag is inside sublattices without nested tags
! Data will be taken care of at the end of the <Phase tag 
!             write(*,*)'  End of nested Sublattice tag: "',level,matt,phnsub
             continue
          elseif(trim(pretag).eq.'</Parameter>') then
!============================ end of nested Parameter (with Trange)
! The end of a nested Parameter tag, non-nested parameters below 
! A nested parameter may contain Trange records
!             write(*,*)'Found end of nested </Parameter> add ',bibref
             wholexpr=wholexpr//' '//bibref
             call xtdbOCfun('Parameter ',parid)
!             write(*,665)trim(parid),matt,level,fline,len_trim(wholexpr)
665          format('   Parameter "',a,'"',2i3,2i7)
!             write(*,*)'  Expr: ',trim(wholexpr)
          else
!  still with matt=-1, nested tags without special action ...
             write(*,667)trim(tagname),trim(pretag),matt,fline
667          format('  Unknown end of nested tag: "',a,'" "',a,'"',i2,i7)
             continue
          endif
!============================ prepare to read next tag, matt=-1
! We have to decrease the nesting level and prepare for reading a new tag
!          write(*,650)trim(tagname),trim(pretag),level,fline
650       format('>>>Nested tag end: "',a,'" "',a,'" ',i2,i7)
          level=level-1
          if(level.gt.0) then
! this is the tag on the previous leveö
!             tagname=endoftag(level)
! it must be wrong to set pretag same as tagname!!
             pretag='</'//trim(endoftag(level))//'>'
          else
! Level 0 means we have read the whole xtdb file, we should never be here
             write(*,*)'We have read past end of </XTDB>'
          endif
          cycle readall
! Done all for end of nested tag, matt=-1
!============================ end of actions for all nested tags, matt=-1
       endif
! Here matt=0 or 1, maybe we need to save attributes
!       write(*,660)trim(tagname),trim(pretag),matt,fline
660    format('660 Tag: "',a,'" pretag "',a,'" ',i3,i7)
!
       if(matt.ge.0) then
          if(tagname(1:1).eq.' ') then
! This should not appear, originally it was a line with </tagname>          
             write(*,*)'xtdbtag found no tag on line ',fline,matt
             cycle readall
          endif
       else
! we have found the end of a nested tag
          write(*,*)'Handle end of nested tag: ',trim(tagname),matt,level,fline
       endif
! check if we have reached endoffile, maybe rewind and read again?
       if(fline.lt.0) then
! we may have to rewind/reread the file to pick up missing tags (TPfun etc)
          goto 990
       endif
! decode a new tag 
       lk=len_trim(tagname)+1
       tagno=0
      findtag: do kk=1,nxtdbtags
! compare ignoring trailing spaces
          if(tagname(1:lk).eq.xtdbtags(kk)(1:lk)) then
             tagno=kk;
! Abbreviated tags not allowed
             if(xtdbtags(kk).eq.' ') exit findtag
          endif
       enddo findtag
!       write(*,99)tagname(1:lk),tagno,fline,level,matt
99     format('99 Tag: ',a,', number',i3', line: ',i7,', level: ',2i3)
!
!
! detect tag
!------------------------------------------
       select case(tagno)
!------------------------------------------
       case default
! process the XTD tag, sometimes nested tags depend on previous attributes
          write(*,*)' *** Tag not found: <',trim(tagname),'>'
!------------------------------------------
       case(1) ! XTDB
          write(*,*)' *** Not implemented attributes of: ',trim(tagname)
!------------------------------------------
       case(2) ! Defaults
          ip=1; values=' '
          write(*,*)'Defaults: '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
38           format(3x,'Att: ',a,' = ',a,i5)
          enddo
!------------------------------------------
       case(3) ! DatabaseInfo
          ip=1; values=' '
          write(*,*)'DatabaseInfo: '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(4) ! AppendXTDB
          ip=1; values=' '
          write(*,*)'AppendTDB files: '
          do while(getatt(attributes,ip,attname,values))
! this will extract the attributes in any order
             write(*,38)trim(attname),trim(values)
! save appfiles in appfiles
             if(attname(1:6).eq.'Models') then
                modelappx=trim(values)
                appfiledone(1)=1
             endif
             if(attname(1:10).eq.'Parameters') then
                parappx=trim(values)
                appfiledone(2)=1
             endif
             if(attname(1:6).eq.'TPfuns') then
                tpfunappx=trim(values)
                appfiledone(3)=1
             endif
             if(attname(1:12).eq.'Bibliography') then
                biblioappx=trim(values)
                appfiledone(4)=1
             endif
             if(attname(1:13).eq.'Miscellaneous') then
                miscappx=trim(values)
                appfiledone(5)=1
             endif
          enddo
! if there is a Models appendix read that now!
          if(appfiledone(1).gt.0) then
             call xtdbmodels(modelappx)
          endif
!------------------------------------------
       case(5) ! Element, 5 attributes
          ip=1
          values=' '
          do while(getatt(attributes,ip,attname,values))
!             write(*,38)trim(attname),trim(values)
             if(attname(1:2).eq.'Id') tpfun=trim(values)
          enddo
          call xtdbOCspel('Element ',trim(tpfun),trim(attributes))
!------------------------------------------
       case(6) ! Species
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
!             write(*,38)trim(attname),trim(values)
             if(attname(1:2).eq.'Id') tpfun=trim(values)
          enddo
          call xtdbOCspel('Species ',trim(tpfun),trim(attributes))
!------------------------------------------
       case(7) ! TPfun
          ip=1; values=' '
          if(matt.eq.0) then
! matt=0 means no nested tags, matt=-1 is taken care of above
! We will read all TPfuns several times until all used has been found
! matt=0  means there are no nested tranges
!             if(matt.eq.0) then
             if(matt.lt.0) then
                write(*,*)'We should never be here with matt=-1',matt
                stop
             endif
             do while(getatt(attributes,ip,attname,values))
!                   write(*,38)trim(attname),trim(values)
                lowt=default_lowt
                hight=default_hight
                if(attname(1:2).eq.'Id') tpfun=trim(values)
                if(attname(1:4).eq.'LowT') lowt=trim(values)
                if(attname(1:4).eq.'Expr') expr=trim(values)
                if(attname(1:5).eq.'HighT') hight=trim(values)
             enddo
             semicolon=len_trim(expr)
             if(expr(semicolon:semicolon).eq.';') then
                expr(semicolon:semicolon)=' '
                semicolon=semicolon-1
             endif
!                write(*,*)'allocating wholexpr',semicolon,len_trim(lowt),&
!                     len_trim(hight),len_trim(bibref)
             wholexpr=lowt//' '//expr(1:semicolon)//'; '//hight
! check if the TPfun is needed, tpfun is Id of <TPfun tag, 
!             write(*,*)'Problem allocating character wholexpr ... no bibref!'
             call xtdbentertpfun(tpfun)!
             call xtdbOCfun('TPfun ',tpfun)
!             endif
          else
! this TPfun has nested Trange tags, extract the current data and read more
             do while(getatt(attributes,ip,attname,values))
!                write(*,38)trim(attname),trim(values)
                lowt=default_lowt
                hight=default_hight
                if(attname(1:2).eq.'Id') tpfun=trim(values)
                if(attname(1:4).eq.'LowT') lowt=trim(values)
                if(attname(1:4).eq.'Expr') expr=trim(values)
                if(attname(1:5).eq.'HighT') hight=trim(values)
             enddo
!             write(*,*)'  <<<Nested TPfun: ',matt
             semicolon=len_trim(expr)
             if(expr(semicolon:semicolon).eq.';') then
                expr(semicolon:semicolon)=' '
                semicolon=semicolon-1
             endif
             if(attname(1:5).eq.'HighT') hight=trim(values)
! this wholexpr will be extended by Trange records
!             wholexpr=expr(1:semicolon)//'; '//trim(hight)//
! in the TDB format there is a Y after highT if there is expression after
             wholexpr=expr(1:semicolon)//'; '//trim(hight)
          endif
!------------------------------------------
       case(8) ! Trange  
          if(matt.gt.0) then
             write(*,*)'Trange tags cannot have nested tags',fline
             xtdberr=4900; goto 1000
          endif
          ip=len_trim(wholexpr)
!          write(*,88)fline,ip
88        format(/'In Trange',2i7)
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
!             write(*,38)trim(attname),trim(values)
             hight=default_hight
             if(attname(1:4).eq.'Expr') expr=trim(values)
             if(attname(1:5).eq.'HighT') hight=trim(values)
          enddo
          semicolon=len_trim(expr)
          if(expr(semicolon:semicolon).eq.';') then
             expr(semicolon:semicolon)=' '
             semicolon=semicolon-1
          endif
          if(attname(1:5).eq.'HighT') hight=trim(values)
! there is already an expression with a highT limit in wholexpr, join with Y
          wholexpr=trim(wholexpr)//' Y '//expr(1:semicolon)//'; '//trim(hight)
! len_trim of parid which is not assigned can be infinit!!
!          write(*,19)trim(wholexpr)
19        format('Trange wholexpr: ',a)
!------------------------------------------
       case(9) ! Phase
          ip=1; values=' '
          if(allocated(phrec)) then
! remove data for any previous phase, this should already have been done
             write(*,*)'Failed deallocate previous phase data!'
             deallocate(phrec)
          endif
          allocate(phrec)
          do while(getatt(attributes,ip,attname,values))
!             write(*,38)trim(attname),trim(values)
             phrec%state='S'
             if(attname(1:2).eq.'Id') phrec%id=trim(values)
             if(attname(1:13).eq.'Configuration') phrec%confent=trim(values)
             if(attname(1:5).eq.'State') phrec%state=trim(values)
! more data in phrec in nested tags <Sublattices <Constutents <AmendPhase ...
          enddo
!------------------------------------------
       case(10) ! Sublattices
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             if(attname(1:8).eq.'NumberOf') phrec%noof=trim(values)
! we need to know the number of sublattices to allocate phrec%clist
             if(attname(1:14).eq.'Multiplicities') phrec%mult=trim(values)
          enddo
! use unformatted read to allocate pointer array for constituents
          read(phrec%noof,*)phnsub
          allocate(phrec%clist(phnsub))
 ! initiate phisub, incremented for each <Constituent tag
          phisub=0
!------------------------------------------
       case(11) ! Constituents
! there is one of these tag for each sublattice
          ip=1; values=' '
! phisub=0 set by sublattice tag, incremented for each Constituent record
          phisub=phisub+1
          do while(getatt(attributes,ip,attname,values))
!             write(*,38)trim(attname),trim(values)
! this is just sublattice number, will be ignored
             if(attname(1:10).eq.'Sublattice') &
                  phrec%clist(phisub)%subx=trim(values)
! This is list of constituents in the sublattice
! The character will be allocated automatically
             if(attname(1:4).eq.'List') phrec%clist(phisub)%list=trim(values)
          enddo
          if(phisub.gt.phnsub) then
             write(*,*)'Dimensioning clist error ',phisub,phnsub
          else
!             write(*,991)phisub,len_trim(phrec%clist(phisub)%list),&
!                  phrec%clist(phisub)%list
991          format('Subl: ',i2,i5,', const: ',a)
          endif
!------------------------------------------
       case(12) ! CrystalStructure
          write(*,*)'Not implemented tag  12'
!------------------------------------------
       case(13) ! AmendPhase
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             if(attname(1:6).eq.'Models') phrec%amendph=trim(values)
          enddo
!------------------------------------------
       case(14) ! Permutations?? should be in the attribute of AmendPhase
          write(*,*)'Not implemented tag  14 Permutations'
!------------------------------------------
       case(15) ! DisorderedPart
          write(*,*)'Not implemented tag  15' 
!------------------------------------------
       case(16) ! 
          write(*,*)'Not implemented tag  16 unused '
!------------------------------------------
       case(17) ! Parameter
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
!             write(*,38)trim(attname),trim(values)
             lowt=default_lowt
             hight=default_hight
             if(attname(1:2).eq.'Id') parid=trim(values)
             if(attname(1:4).eq.'LowT') lowt=trim(values)
             if(attname(1:4).eq.'Expr') expr=trim(values)
             if(attname(1:5).eq.'HighT') hight=trim(values)
             if(attname(1:6).eq.'Bibref') bibref=trim(values)
          enddo
          semicolon=len_trim(expr)
          if(expr(semicolon:semicolon).eq.';') then
             expr(semicolon:semicolon)=' '
             semicolon=semicolon-1
          endif
          if(matt.eq.0) then
! save the parameter in OC
             wholexpr=lowt//' '//expr(1:semicolon)//'; '//hight//' '//bibref
!             write(*,1700)parid,trim(wholexpr)
1700         format('Parameter ',a/a)
! save the parameter in OC
!             write(*,*)'Saving parameter in OC'
             call xtdbOCfun('Parameter ',parid)
          else
! There is one or more Trange records
! IMPORTANT, bibref must be added when matt=-1 for this parameter!!!
             wholexpr=lowt//' '//expr(1:semicolon)//'; '//hight
!             write(*,*)'   <<<Nested parameter: ',bibref,matt,fline
          endif
!------------------------------------------
       case(18) ! Parameter2
          write(*,*)'Not implemented tag  18 Parameter2'
!------------------------------------------
       case(19) ! Bibliography
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(20) ! Bibitem
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(21) ! Model maybe ModelInfo 
          write(*,*)'Not implemented tag  21 ModelInfo'
!------------------------------------------
       case(22) ! Magnetism
          write(*,*)'Modelinfo magnetism  22'
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(23) ! Einstein 
          write(*,*)'Modelinfo Einstein  23'
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(24) ! Liquid2state
          write(*,*)'Modelinfo Liquid2State  24'
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(25) ! Volume
          write(*,*)'Modelinfo Volume  25'
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(26) ! EEC
          write(*,*)'Modelinfo EEC 26'
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(27) ! TernaryXpol
          write(*,*)'Modelinfo TernaryXpol 27'
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
!------------------------------------------
       case(28) ! UnarySystem
          write(*,*)'Not implemented UnarySystems 28'
!------------------------------------------
       case(29) ! BinarySystem
          write(*,*)'Not implemented BinarySystem  29'
!------------------------------------------
       case(30) ! TernarySystem
          write(*,*)'Not implemented TernarySystem 30'  
       end select
!------------------------------------------
! end of file but maybe rewind and read for some special tag? such as TPfun
900    continue
       cycle readall
!
    enddo readall
! errors 
990 write(*,*)'End of file'
    close(unit)
1000 continue
    return
1100 continue
    write(*,*)'Error reading xtdbfile ',xtdberr
    goto 990
!
  end subroutine xtdbread
! \/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!

  subroutine xtdballel(filename,elnames)
! read xtdb file and extract selected tags
    character filename*(*),elnames(*)*2
! line is line read from file, bigline is concatinated input line
    character line*256,tagname*64
! tag attributes
    character attributes*1024
! for nested tags level is current level,  matt=0 if tagend missing (nesting)
    character pretag*24,values*256,attname*18
    integer tagno,matt,unit,ll,fline
! for various purposes
    integer ip,iq,ir,is,it,level,prevlev,lk,kk
!
    write(*,*)'Opening XTDB file: ',trim(filename)
    unit=21
    open(unit,file=filename,&
         access='sequential',form='formatted',status='old')
! zero file line number
    fline=0
! zero position in characters containing attributes
    attpos=0
! initiate tag nesting
    level=0
    matt=0
    pretag=' '
    xtdberr=0
    readall: do while(.true.)
! only one tag per line, attributes can be on following lines
!--------------------------
! the call below will extract lines until the end of arrributes of a tag
! all nested tags are saved in tagnest
! level+1 for rach unfinished tag
! <0 tag unfinished, 0 tag finished, >0 skip until end of comment
       if(fline.lt.0) exit readall
       call xtdbtag(unit,fline,tagname,matt,pretag,attributes)
       if(xtdberr.ne.0) goto 990

       if(matt.eq.1) then
! if matt=1 means tag and all attributes but no endoftag, increase level
          level=level+1
          endoftag(level)=tagname
! HERE TO WRITE TAG AS EXTRATED FROM FILE
!+          write(*,106)fline,trim(tagname),trim(attributes)
106       format(i7,' <<tag: ',a,2x,a)
! this is the end of the tag
          pretag='</'//trim(tagname)//'>'
       elseif(matt.eq.-1) then
! if matt=-1 the line is end of pretag, decrease level
          level=level-1
!+          write(*,108)fline,trim(endoftag(level+1))
108       format(i7,' >>tag: ',a)
          if(level.gt.0) then
! this is the end of the tag
             pretag='</'//trim(endoftag(level))//'>'
          else
             pretag=' '
          endif
! there are no attributes on this line
          cycle readall
       else
! matt=0 means no change of levels
!+          write(*,110)fline,trim(tagname),trim(attributes)
110       format(i7,'110   Tag: "',a,'" ',a)
       endif

       if(tagname(1:1).eq.' ') then
! This should not appear, originally it was a line with </tagname>          
!          write(*,*)'xtdbtag found no tag on line ',fline,matt
          cycle readall
       endif

! in previous tags, for example phase name or links.
       if(fline.lt.0) then
! we may have to rewind/reread the file to pick up missing tags (TPfun etc)
          goto 990
       endif
! decode tag
       
      lk=len_trim(tagname)
      findtag: do kk=1,nxtdbtags
          if(tagname(1:lk).eq.xtdbtags(kk)(1:lk)) then
             tagno=kk; exit findtag
          endif
       enddo findtag
       write(*,99)tagname(1:lk),tagno,fline
99     format('99 Tag: "',a,'," number',i3', line: ',i7)
!
    enddo readall
!
990 continue
!
1000 continue
    return
  end subroutine xtdballel

! \/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!\/!\/!\/!\/!/!

  subroutine xtdbtag(unit,fline,tagname,matt,pretag,attributes)
! this subroutine extract a tag and its attributes from lines read from file.  
! Tag begins with "<tagname>" if no attributes, otherwise "<tagname "
! ONLY ONE TAG PER LINE but the attributes can be on following lines.
! End of tag attributes is ">" or "/>", the latter also means end of tag
! End of tag may be on a separate line as "/>" or if nested </tagname>
! USING XEOLCH
    
! Attributes are one or more identifier="values" decoded in calling routine
!-------------------
    character tagname*(*),pretag*(*),attributes*(*)
    integer unit,fline,matt
!-------------
    character line*256
    integer ep,ip,jp,kp,tp,taglen,tagend,lines,lentagname
    logical comment
!-------
! matt  tagend meaning
!  0    -2     looking for <tag
!  0    -1     end of attributes not found 
!  1     0     end of attribues found but not end of tag, nesting
! -1     0     end of nested tag found, decrease level
    tagend=-2
    comment=.false.
    attpos=-1            ! attpost incremeneted by 2 when used
    tagname=' '
    attributes=' '
    lines=0
!    if(pretag(1:1).ne.' ') write(*,*)'Call with pretag: ',trim(pretag)
!================================================
! we may have to read the file several times to pick up all tags ....
    readtag: do while(.true.)
! maybe read several lines until all attributes extracted
       read(unit,100,end=1100)line
100    format(a)
       fline=fline+1
       lines=lines+1
!       write(*,102)tagend,fline,trim(line)
102    format(/'Read line ',i3,i6,' "',a,'"')
! skip continuation of comment lines
       if(comment) then
          ip=index(line,'--')
          if(ip.gt.0) then
             if(line(ip+2:ip+2).ne.'>') then
                write(*,*)' *** Error, use of -- inside comment, line',fline
                xtdberr=4518
                exit readtag
             endif
! we found end of multiline comment -->
!             write(*,*)'End of multiline comment',fline
             attributes=' '
             attpos=-1
             comment=.false.
! skip rest of line
             if(len_trim(line).gt.ip+3) then
                write(*,103)fline
103             format('Skipping text trailing comment on line ',i7)
             endif
! reset line count for next tag
             tagend=-2
             lines=0
          endif
          cycle readtag !------- skip rest of line and cycle
       endif
!================================================
! use xeolch to find first character on line
       ip=1
       if(xeolch(line,ip)) then
! if line empty read next line
          cycle readtag
       endif
! first character of a tag must be <
       if(line(ip:ip).eq.'<') then
          if(tagend.ne.-2) then
             write(*,*)' *** ERROR XTDB has only one tag per line,',fline
             xtdberr=4515; exit readtag
          endif
! we found <, if </ it is the end of a nested tag
          if(line(ip:ip+3).eq.'<!--') then
! we have found start of a comment, if not finish on same line set comment
             if(.not.index(line(ip:),'-->').gt.0) comment=.TRUE.
             cycle readtag
          endif
          if(line(ip:ip+1).eq.'</') then
! we have </ it is the end of a nested tag and must be equal to pretag
             jp=len_trim(pretag)
             if(line(ip:ip+jp-1).eq.pretag(1:jp)) then
! end of nested tag, skipping any text after </tagname>, negative matt
!                write(*,666)line(ip:ip+jp-1),trim(pretag),fline
666             format('PRETAG: "',a,'" "',a,'" ',i7)
                matt=-1
             else
                write(*,*)' *** Error, illegal end of tag, line',fline
                xtdberr=4514
             endif
             exit readtag
          endif
! new tag found, check if it has attributes, find first space
          jp=index(line(ip:),' ')
!          write(*,*)'Found a space: ',line(ip+1:ip+jp-1),jp !-------------
          if(line(ip+jp-2:ip+jp-2).eq.'>') then
! Tag name end with > this is a tag without attributes but with nested tags
! skip any trailing text
             tagname=line(ip+1:ip+jp-3)
!             write(*,*)'TAGNAME1: "',trim(tagname),'"'
             matt=1
             exit readtag
          else
             tagname=line(ip+1:ip+jp-2)
!             write(*,*)'TAGNAME2: "',trim(tagname),'"'
          endif
! we have found the tagname, tagend=-1 indicate attributes can be on next linws
          lentagname=len_trim(tagname)
          tagend=-1
          ip=ip+jp
       endif
! we are looking for attributes in line after ip
       if(xeolch(line,ip)) then
! line empty and we have not found end of attributes
          cycle readtag
       else
          jp=index(line(ip:),'>')
! this indicate end of attributes
          if(jp.le.0) then
! no end of attributes, copy all to attributes and read next line
             if(ip.gt.0) then
                attributes(attpos+2:)=trim(line(ip:))
                attpos=len_trim(attributes)
             endif
          else
! Found > as end of attributes.  maybe end of tag, matt=0 means no nested tags
             if(line(ip+jp-2:ip+jp-1).eq.'/>') then
                attributes(attpos+2:)=line(ip:ip+jp-3)
                matt=0
             else
! If no end of tag nested tags may follow on next lines
                attributes(attpos+2:)=line(ip:ip+jp-2)
                tp=ip+jp+1
                matt=1
! check for rubbish and maybe full endoftag after rubbish
                if(.not.xeolch(line,tp)) then
                   jp=index(line(tp:),'<')
                   if(jp.gt.0) then
! The text has a <, if part of </tagname> means end of tag
             write(*,69)line(tp+jp+1:tp+jp+lentagname),trim(tagname),tp
69                    format('Are "',a,'" and "',a,'" equal?',i7)
                      if(line(tp+jp:tp+jp).eq.'/' .and. &
             line(tp+jp+1:tp+jp+lentagname).eq.trim(tagname)) then
! end of tag found after >, no nesting for this tag
                write(*,*)'Trash found between > and tagend on line ',fline
                         matt=0
                      else
                         write(*,*)' *** Error, new tag after > on line ',fline
                         xtdberr=4766
                         exit readtag
                      endif
                   else
! ignore rubbish after >
                      write(*,*)' Text after > ignored on line ',fline
                      xtdberr=4766
                      exit readtag
                   endif
                endif
             endif
! we have found > or /> or </tagname>
             exit readtag
          endif
       endif
! maybe add
       if(lines.gt.2) then
! check for tags with multiple lines
          if(tagname(1:1).eq.' ') write(*,104)fline-1
104       format(' *** Warning, line without tag, line ',i7)
          if(lines.gt.3 .and. matt.lt.0) write(*,106)fline-3
106       format(' *** Warning, very long list of attributes, line ',i7)
       endif
    enddo readtag
! puh.................
1000 continue
!    write(*,1002)matt,fline,trim(tagname)
1002 format('Exiting xtdbtag: ',i2,i7,' ',a)
    return
1100 write(*,*)'End of file or fatal error'
    fline=-1
    goto 1000
  end subroutine xtdbtag

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  logical function xeolch(line,ip)
! identical to eolch in METLIB
!...End Of Line CHeck, TO SKIP SPACES/TAB FROM IP. RETURNS .TRUE. IF no other
    character line*(*)
    integer ip
!
    integer, parameter :: itab=9
    xeolch=.true.
    if(ip.le.0) ip=1
    do while(ip.lt.len(line) .and. &
         (line(ip:ip).eq.' ' .or. ichar(line(ip:ip)).eq.itab))
       ip=ip+1
    enddo
    if(ip.lt.len(line)) xeolch=.false.
900 RETURN
  END function xeolch

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  logical function getatt(text,ip,attname,value)
! extract "values" of any XML "attname" in text from position ip
    character*(*) text,attname,value
    integer ip
!
    integer jp,kp,attlen
!
    getatt=.false.
!    write(*,*)'In getatt 1',len(text)
    find: if(.not.xeolch(text,ip)) then
       if(text(ip:ip).eq.'=') exit find
       jp=index(text(ip+1:),'=')
       if(jp.gt.0) then
! the attname is terminated by a = (possibly preceeded by spaces)
          attname=text(ip:ip+jp-1)
!          write(*,*)'In getatt 2',trim(attname)
          ip=ip+jp+1
          if(.not.xeolch(text,ip)) then
! the values are preceeded by a " (possibly prceeded by spaces)
             if(text(ip:ip).eq.'"') then
                jp=index(text(ip+1:),'"')
                if(jp.gt.0) then
                   value=text(ip+1:ip+jp-1)
                   ip=ip+jp+1
                   getatt=.true.
                else
                   xtdberr=4601
                endif
             else
                xtdberr=4602
             endif
          else
             xtdberr=4603
          endif
       else
          xtdberr=4607
       endif
! no error if line empty
    endif find
    return
  end function getatt

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  logical function check_mpid(mpid,phase)
! check that phase has a model corresponding to the mpid of a parameter
    character*(*) mpid,phase
!
    integer np,ip
! 1. loop to find the phase
! 2. loop models for the phase to find one with the MPID
! 3. return TRUE if found, FALSE if not    
    
1000 continue
    return
  end function check_mpid


!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine getxtdbatt(attname,text,ip,values)
! extract "values" of attribute "att" of "tag" from text at position ip
    character*(*) text,attname,values
    integer att,ip
!
    integer jp,kp,attlen
! seach for attribute att is the array with attributes
    attlen=len_trim(attname)
!    write(*,*)'In getxtdbatt 1 >',text(ip:ip+25),'<',ip,attlen
    jp=index(text(ip:),attname(1:attlen))
    if(jp.le.0) goto 1100
! set jp to position after attname, the attribute must finish with ="
    jp=ip+jp+attlen-1
!    write(*,*)'In getxtdbatt 2 >',text(jp:jp+5),'<',jp
    if(text(jp:jp+1).ne.'="') goto 1110
    kp=index(text(jp+2:),'"')
!    write(*,*)'In getxtdbatt 3 >',text(jp:kp+5),'<',jp,kp
    if(kp.le.0) goto 1120
    values=text(jp+2:jp+kp)
! update ip to position after "
    ip=jp+kp+1
!
    1000 continue
!    write(*,*)'Exit getxtdbatt ',text(ip:ip+5),ip,xtdberr
    return
! cannot find attribute
1100 xtdberr=4501
    goto 1000
! attribute has no trailing ="
1110 xtdberr=4502
    goto 1000
! attribute value has no final "
1120 xtdberr=4503
    goto 1000
  end subroutine getxtdbatt

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine xtdbmodels(appfile)
! reads an AppendXTDB file with models and MPID
! may change a default MPID
    character*(*) appfile
!
    character tagname*18,pretag*24,attributes*128,values*24,attname*24
    integer unit,mline,matt,lc,ip
    logical modeltag
!    
    unit=22
    write(*,5)trim(appfile)
5   format(/'In xtdbmodels extracting MPID from: ',a)
    open(unit,file=appfile,access='sequential',form='formatted',status='old')
!
    mline=0
    modeltag=.false.
    models: do while(.true.)
       if(mline.lt.0) exit models
       call xtdbtag(unit,mline,tagname,matt,pretag,attributes)
       if(xtdberr.ne.0) goto 1100
       lc=len_trim(tagname)+1
!       write(*,10)trim(tagname),trim(attributes),mline,matt,modeltag
10     format('Model tag: "',a,'" att "',a,'"',i7,i3,l2)
       if(.not.modeltag) then
          if(tagname(1:lc).eq.'Models ') then
             modeltag=.true.
          else
             write(*,*)'Expecting only nested modeltags'
             xtdberr=4700; goto 1100
          endif
          cycle models
       endif
       if(matt.lt.0) then
! just skip the end of a model tag
          cycle models
       endif
! make sure we prepare an endoftag
       pretag='</'//tagname(1:lc-1)//'>'
       write(*,*)'tagname "',tagname(1:lc),'"',lc
! tags expected are
       if(tagname(1:lc).eq.'Magnetic ') then
!------------------------------------------!       case(22) ! Magnetism
          ip=1; values=' '
          write(*,*)'Modelinfo magnetism  22'
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
38           format(3x,'Att: ',a,' = ',a)
          enddo
       elseif(tagname(1:lc).eq.'Einstein ') then
!------------------------------------------
!       case(23) ! Einstein 
          write(*,*)'Modelinfo Einstein  23'
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
       elseif(tagname(1:lc).eq.'Einstein ') then
!------------------------------------------
!       case(24) ! Liquid2state
          write(*,*)'Modelinfo Liquid2State  24'
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
       elseif(tagname(1:lc).eq.'Volume ') then
!------------------------------------------
!       case(25) ! Volume
          write(*,*)'Modelinfo Volume  25'
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
       elseif(tagname(1:lc).eq.'EEC ') then
!------------------------------------------
!       case(26) ! EEC
          write(*,*)'Modelinfo EEC 26'
          ip=1; values=' '
          do while(getatt(attributes,ip,attname,values))
             write(*,38)trim(attname),trim(values)
          enddo
       elseif(tagname(1:lc-1).eq.'Bibliography') then
!------------------------------------------
          write(*,*)'Skipping bibliography of models, closing file'
          goto 1000
       else
!------------------------------------------
          write(*,*)'No such model tag "',trim(tagname),'".  closing file'
          goto 1000
       endif
!
    enddo models
!-----------------
1000 continue
    write(*,1005)trim(appfile)
1005 format('Closing: ',a/)
    close(unit)
    return
1100 continue
    write(*,*)'Error ',xtdberr,' reading ',trim(appfile)
    goto 1000
  end subroutine xtdbmodels

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine addmissingtp
! extract TPfuns called inside wholexpr and add those missing to alltpfun
! with statustpfun -1
! all variables global
!
    integer ip,jp,kp,ntp
    character symbol*16
    ip=1
    big: do while(ip.lt.len_trim(wholexpr))
! istpfun extracts symbols inside wholexpr ip is updated inside istpfun
       call istpfun(wholexpr,ip,symbol)
       if(symbol(1:1).ne.' ') then
          do ntp=1,ntpfun
             if(symbol.eq.alltpfun(ntp)) then
                symbol=' ';cycle big
             endif
!             write(*,55)ntp,ip,symbol,alltpfun(ntp)
55           format('Alltpfun ',2i5,' "',a,'" and "',a,'"')
          enddo
! this symbol is missing
!          write(*,*)'Adding missing tpfun: ',trim(symbol)
          ntpfun=ntpfun+1
          alltpfun(ntpfun)=symbol
          statustpfun(ntpfun)=-1
       endif
    enddo big
1000 continue
!    write(*,*)'Listing of alltpfun',ntpfun
!    do ntp=1,ntpfun
!       write(*,999)ntp,alltpfun(ntp),statustpfun(ntp)
999    format(i4,2x,a,i3)
!    enddo
    return
  end subroutine addmissingtp

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine istpfun(line,ip,symbol)
! extract unknown symbols (TPfuns) from an expression after position ip
    character line*(*),symbol*(*)
    integer ip
!------
    integer jp,kp,mp
    character ch1*1
    call capson(line)
    symbol=' '
    kp=0
!    write(*,*)'In istpfun',ip,trim(line),len(line)
    test: do while(ip.lt.len_trim(line))
! symbols must start with letter A-Z and can contain letters, digits and "_"
       ch1=line(ip:ip)
       ip=ip+1
       if((ch1.ge.'A' .and. ch1.le.'Z')) then
! first character must be a letter
          kp=kp+1
          symbol(kp:kp)=ch1
       elseif(kp.gt.1 .and. &
! later character can be number or _
            ((ch1.ge.'0' .and. ch1.le.'9') .or. ch1.eq.'_')) then
          kp=kp+1
          symbol(kp:kp)=ch1
          write(*,*)'Symbol: ',trim(symbol),kp,ip
       else
! We found a character illegal in a symbols, check if in nottpfun
          if(kp.ge.2 .and. kp.le.8) then
             do mp=1,5
! Skip symbols: LN LOG EXP ERF GEIN
                if(symbol(1:8).eq.nottpfun(mp)) goto 300
             enddo
! symbol is not predefined function: LN LOG EXP ERF GEIN
!             write(*,*)'check tpfun 1: ',trim(symbol),ip
             exit test
          endif
300       continue
          symbol=' '; kp=0
       endif
    enddo test
!    write(*,*)'Leaving istpfun: "',trim(symbol),'" ',ip
1000 continue
    return
  end subroutine istpfun

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

!\addtotable subroutine capson & Convert character to UPPER case
!\begin{verbatim}
  SUBROUTINE capson(text)
! converts lower case ASCII a-z to upper case A-Z, no other changes
    implicit none
    character text*(*)
!\end{verbatim}
    integer, parameter :: lowa=ichar('a'),lowz=ichar('z'),&
         iup=ICHAR('A')-ICHAR('a')
    integer i,ich1
    DO i=1,len(text)
       ich1=ichar(text(i:i))
       IF(ich1.ge.lowa .and. ich1.le.lowz) THEN
          text(i:i)=char(ich1+iup)
       ENDIF
    ENDDO
  END SUBROUTINE capson

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine xtdbentertpfun(tpfun)
! check if TPfun is needed and of so enter it
    character*(*) tpfun
!
    integer lentp
    lentp=len_trim(tpfun)
    do ntp=1,ntpfun
       write(*,8000)ntp,tpfun,alltpfun(ntp),statustpfun(ntp)
8000   format('Found missing? ',i3,' "',a,'" "',a,'"',i4)
       if(tpfun.eq.alltpfun(ntp)(1:lentp)) then
          if(statustpfun(ntp).eq.-1) then
             statustpfun(ntp)=1
             write(*,14)trim(tpfun),statustpfun(ntp),trim(wholexpr)
14           format('Found missing TPfun "',a,'"  ',i3/a)
!>>>>>>>>>  call to enter TPfun in OC
! check if this TPfun need other TPfuns
!             write(*,*)'Calling addmissingtp',ntpfun
             call addmissingtp
! this is already called but this call will be used when implemented
!             call xtdbOCfun('TPfun',tpfun,wholexpr)
          endif
       endif
    enddo
1000 continue
    return
  end subroutine xtdbentertpfun

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine xtdbOCphase(phrec)
! listing phase data to be entered in OC
    type(phnest) :: phrec
    integer ns,ms
    write(*,10)phrec%Id,phrec%confent,phrec%state,phrec%noof,phrec%mult
10  format('OC phase: ',a,1x,a,1x,a,1x,a,1x,a)
    ms=size(phrec%clist)
    do ns=1,ms
       write(*,20)phrec%clist(ns)%subx,phrec%clist(ns)%list
20     format('Sublattice: ',a,'  Constituents: ',a)
    enddo
    if(allocated(phrec%amendph)) write(*,30)phrec%amendph
30  format('Models: ',a)
1000 continue
    return
  end subroutine xtdbOCphase

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine xtdbOCfun(type,tpfuname)
! listing TPfun/Parameter data to be entered in OC.  Function in wholexpr
    character*(*) type,tpfuname
!    write(*,*)' ********** in xtdbOCfun' this works for parameters
    write(*,10)type,tpfuname,trim(wholexpr)
10  format(a,2x,a,2x,a)
    return
  end subroutine xtdbOCfun

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

  subroutine xtdbOCspel(type,spel,data)
! listing Element/species in xtdb file
    character*(*) type,spel,data
!    write(*,*)' ********** in xtdbOCspel'
    write(*,10)type,spel,trim(data)
10  format(a,2x,a,2x,a)
    return
  end subroutine xtdbOCspel

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!

end module xtdblib

program xmltest
  use xtdblib
  character*60 filename
  write(*,*)'XML file name:'
  read(*,*)filename
!  filename='test0.XTDB'
  call xtdbread(filename)
  if(xtdberr.ne.0) then
     write(*,*)'read error: ',xtdberr,' line: ',fline
  endif
! list all tpfun found or missing
  write(*,*)'All TPfun entered or missing (-1)'
  do ip=1,ntpfun
     write(*,70)ip,alltpfun(ip),statustpfun(ip)
70   format(i5,2x,a,2x,i2)
  enddo

end program xmltest
