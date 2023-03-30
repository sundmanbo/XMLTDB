!
! First draft of an XML upload library
!  
MODULE XMLTDB_LIB
!
! maybe needed eventually to decode TPFUN
!  use metlib
  use metlib_xmltdb
!
! Fortran definition of the XMLelements for use in XMLTDB project
!
! first draft 2023.03.15 Bo Sundman
!
  implicit none
!
  character (len=64), dimension(5000:5010) :: XMLTDBerror
  data XMLTDBerror(5000:5010) &
!        123456789.123456789.123456789.123456789.123456789.123456789.1234
       /'No error mmessage defined                                       ',&
        'No XMLelement definition file                                   ',&
        '                                                                ',&
        '                                                                ',&
        '                                                                ',&
        '                                                                ',&
        '                                                                ',&
        '                                                                ',&
        '                                                                ',&
        '                                                                ',&
        '                                                                '/
! definition of predefined model names and identifying integers
  integer,  parameter :: INDEN_MAGNETIC=1
  integer,  parameter :: XIONG_MAGNETIC=2
  integer,  parameter :: EINSTEIN_LOWT=3
  integer,  parameter :: TWOSTATE_MODEL1=4
  integer,  parameter :: VOLIME_MODEL=5
  integer,  parameter :: EEC_MODEL=6
  integer,  parameter :: FCC_PERMUTATIONS=7
  integer,  parameter :: BCC_PERMUTATIONS=8
  integer,  parameter :: DISORDERED_PART=9
  integer,  parameter :: EBEF=10
  integer,  parameter :: MQMQMA=11
  integer,  parameter :: UNIQUAC=12
! error code
  integer xmlerr
!------------------------------------------  
  TYPE xmltdb_string
! declaration of a long string divided in parts
     integer :: len
     character (len=80), dimension(:), allocatable :: text
  end type xmltdb_string
!------------------------------------------    
  TYPE xmltdb_element
! specification of the simple XMLelement ELEMENT     
     character (len=2) :: id
     character (len=32) :: reference
     double precision :: mass,h298h0,s298
!     type(xmltdb_bibliography), pointer :: bibref
  end type xmltdb_element
!------------------------------------------    
  TYPE xmltdb_species
! specification of the simple XMLelement SPECIES
     character (len=24) :: id
     character (len=48) :: stoichiometry
     logical IS_mqmqa, IS_uniquac, IS_element
! The mqmqma data are bonds and FNN/SNN ratio, for UNIQUAC volume and area
     character (len=80) :: amend
! The uniquac data are volume and area 
     double precision :: volume, area
! these are indices in ellist for the elements in the species
     integer numberofel
     integer, dimension(:), allocatable :: elements
     double precision, dimension(:), allocatable :: stoik
  end type xmltdb_species
!------------------------------------------    
  TYPE xmltdb_trange
! specification of the simple XMLelement TRANGE
     double precision :: high_t
! assuming miximum expression for one T range is less than 300 characters
     character (len=300) :: expression
     type(xmltdb_trange), pointer :: next
  end type xmltdb_trange
!------------------------------------------  
  TYPE xmltdb_tpfun
! specification of the XMLelement TPFUN
     character (len=16) :: id
     double precision :: low_t
! cannot be set as target, copy local (target) record in subroutine!
     type(xmltdb_trange) :: trange
  end type xmltdb_tpfun
!------------------------------------------    
!  TYPE xmltdb_modelparameterid
  TYPE xmltdb_mpid
! specification of the XMLelement MODEL_PARAMETER_ID
     character (len=12) :: id
! this is index of model in modellist
     integer index_in_modellist
  end type xmltdb_mpid
!------------------------------------------    
  TYPE xmltdb_phase
! specification of the XMLelement PHASE
     character (len=24) :: name
! the constmodel defines sublattices and, when applicable, sites
     character (len=24) :: configmodel
! sublattices and sites read from TDB file
     integer sublat
     double precision, dimension(:), allocatable :: sites
! TYPE_DEF letters
     character*24 type_defs
     type(xmltdb_amendphase), pointer :: phamend
! index in modellist of additional models
     type(xmltdb_xmodel), dimension(:), allocatable :: extramodel
! constituents
     integer, dimension(:), allocatable :: nconst
     character*24, dimension(:,:), allocatable :: constituents
  end type xmltdb_phase
!------------------------------------------      
  TYPE xmltdb_amendphase
! specification of the XMLelement AMEND_PHASE
     character (len=24) :: modelabbrev
! A model may require specific parameters (TC or BMAG)
     type(xmltdb_amendphase), pointer :: next
     type(xmltdb_mpid), dimension(:), allocatable :: requiredmpid
  end type xmltdb_amendphase
!------------------------------------------      
  TYPE xmltdb_parameter
! specification of the XMLelement PARAMETER
! model parameter id and phase in parameter
     character*12 mpid
     character*24 phase
! MPIDX and PHIDX are indices in mmpidlist and phlist resepectivly
     integer :: mpidx, phidx
! this is number of constituent in each sublattice and degree
     integer constinsubl(9),degree
! this is all constituents in a row
     character (len=24), dimension(:), allocatable :: constarray
! now how to store the expression
     double precision :: low_t,high_t
! this is the expression as short string (including low T limit and reference)
     character*128 tpfun
     character*24 bibref
! this for is storing T-ranges of parameter
     type(xmltdb_trange) :: trange
! this is to indicate if the parameter has been listed (as unary, binary etc)
! inititated to the number of elements, set negative when listed
     integer noofel
  end type xmltdb_parameter
!------------------------------------------      
  TYPE xmltdb_xmodel
! specification of the XMLelement MODEL (excluding constitution)
     character (len=24) :: ID
     character*80 text
!     type(xmltdb_string), pointer :: string
! necessary model parameter identifiers
     integer, dimension(:), allocatable :: mpid
  end type xmltdb_xmodel
!------------------------------------------      
  TYPE xmltdb_bibliography
! specification of the XMLelement BIBLIOGRAPHY
     character (len=24) :: ID
     character (len=512) :: text
     type(xmltdb_bibliography), pointer :: next
  end type xmltdb_bibliography
!------------------------------------------      
  TYPE xmltdb_typedefs
! specification of the XMLelement for handling AMEND for phases
     character id*1
     character*128 :: action
     type(xmltdb_typedefs), pointer :: next
  end type xmltdb_typedefs
!------------------------------------------      
!
! Allocatable arrys of elements, species etc
  type(xmltdb_element), dimension(:), allocatable :: ellist
  type(xmltdb_species), dimension(:), allocatable :: splist
  type(xmltdb_phase), dimension(:), allocatable :: phlist
  type(xmltdb_parameter), dimension(:), allocatable :: palist
  type(xmltdb_tpfun), dimension(:), allocatable :: tplist
  type(xmltdb_xmodel), dimension(:), allocatable :: modellist
  type(xmltdb_mpid), dimension(:), allocatable :: mpidlist
  type(xmltdb_bibliography), dimension(:), allocatable :: bibliolist
! for alphabetical ordering of species and phases
  integer, dimension(:), allocatable :: sporder
  integer, dimension(:), allocatable :: phorder
  integer, dimension(8) :: limits
  integer noel,nosp,noph,nopa,notp,nomod,nompid,nobib,notype,nl
  type(xmltdb_typedefs), target :: type_def_list
  type(xmltdb_typedefs), pointer :: new_type_def
! TDB file software
  character (len=12), dimension(8) :: software
  data software&
       /'Thermo-Calc ','OpenCalphad ','Pandat      ','MatCalc    ',&
        'PyCalphad   ','            ','            ','            '/
!
! to check phases are OK (ambiguietty etc)
  character*24, dimension(:), allocatable :: phasenames
!  
!------- TDB KEYWORDS
!
CONTAINS
!
  subroutine init_xmltdb(maxall)
    integer, dimension(8) :: maxall
! save limits and allocate arrays
    limits=maxall
    write(*,'(a,8I5)')'Allocation limits: ',maxall
    allocate(ellist(1:maxall(1)))
    allocate(splist(1:maxall(2)))
    allocate(sporder(1:maxall(2)))
    allocate(phlist(1:maxall(3)))
    allocate(phorder(1:maxall(3)))
    allocate(phasenames(1:maxall(3)))
    allocate(palist(1:maxall(4)))
    allocate(tplist(1:maxall(5)))
    allocate(modellist(1:maxall(6)))
    allocate(mpidlist(1:maxall(7)))
    allocate(bibliolist(1:maxall(8)))
    noel=0; nosp=0; noph=0; nopa=0; notp=0; nomod=0; nompid=0; nobib=0
! initiate list of type_defs, quite clumsy
    sporder=0
    type_def_list%id='@'
    type_def_list%action='head of list'
    nullify(type_def_list%next)
    return
  end subroutine init_xmltdb

  subroutine read_tdbfile(infile,origin)
    integer infile,ip,key,nextc,nl,jp,kp,nl1
    character (len=10000) :: line
    character (len=512) :: line2
! this indicate which software generated the TDB file
    character*32 origin
    logical more,lastkeywasph
!
!    write(*,*)'Reading a TDB file generated by: ',trim(origin)
    nl=0
    lastkeywasph=.FALSE.
    find: do while(.TRUE.)
       nl=nl+1
       read(infile,100,end=2000,err=2100)line
100    format(a)
!       write(*,*)'Read line: ',nl
       ip=1
! skipping blank lines
       if(eolch(line,ip)) cycle find
! skipping lines starting with '$' ... maybe they shoulp be kept!
       if(line(ip:ip).eq.'$') cycle find
! istdbkeyword starts from position 1 and ip is space after key
! The keywords are defined inside istdbkeyword
       key=istdbkeyword(line,ip)
! These are for Thermo-Calc TDB files
!   character (len=kwl), dimension(nkw), parameter :: keyword=&
!        ['ELEMENT             ','SPECIES             ',&
!         'PHASE               ','CONSTITUENT         ',&
!         'FUNCTION            ','PARAMETER           ',&
!         'TYPE_DEFINITION     ','LIST_OF_REFERENCES  ',&
!         'ADD_REFERENCES      ','ASSESSED_SYSTEMS    ',&
!         'DATABASE_INFORMATION','VERSION             ',&
!         'DEFAULT_COMMAND     ','DEFINE              ']
!
!       write(*,*)'Keyword index: ',key,xmlerr,ip,nl
       if(lastkeywasph) then
          if(key.ne.4) then
             write(*,111)nl
111          format('Please change line ',i7,' to be the CONSTITUENT keyword'/&
                  'corresponing to preceeding PHASE keyword!')
             stop
          endif
       endif
       nl1=nl
       do while(index(line,'!').le.0)
! data has more lines
!          write(*,*)'end of line: ',line(max(1,jp-10):jp)
! BEWHERE list_of_references can be VERY long
77        continue
          nl=nl+1
          read(infile,100,end=2000,err=2100)line2
          kp=1
! skip empty lines
          if(eolch(line2,kp)) goto 77
! append new line after jp
          jp=len_trim(line)
          if(jp+len(line2).gt.len(line)) then
             write(*,*)'Too long line "',line(1:40),'"',jp
             xmlerr=5000; goto 1000
!          elseif(line(jp-10:jp).eq.'_REFERENCES') then
! insert a few spaces to find the command!
!             jp=jp+10
          endif
! do not insert any space at linebreak but remove most leading spaces in line2
          if(kp.gt.2) then
             kp=kp-2
          else
             kp=1
          endif
          line(jp+1:)=line2(kp:)
!          write(*,*)'Merging TDB file line ',nl,' with ',nl1,', length: ',jp+kp
!          write(*,*)'Text <',line(1:jp+kp),'>'
       enddo
       if(lastkeywasph) then
! if lastkeywasph is true then this keyword must be constituent
          if(key.ne.4) then
             write(*,*)'Keyword "PHASE" must be followed by "CONSTUENT"'
             xmlerr=5000; goto 2200
          endif
       endif
!
       if(xmlerr.ne.0) goto 2000
       select case(key)
! error
       case default
          write(*,110)nl,line(1:40)
110       format(' *** Ignoring line ',i7,' starting: "',a,'"')
! element symbol, refstate, mass, H298_H0, S298
       case(1)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          noel=noel+1
          call geteldata(line,ip)
          if(xmlerr.ne.0) goto 2200
! species
       case(2)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          nosp=nosp+1
          call getspdata(line,ip)
          if(xmlerr.ne.0) goto 2200
! phase
       case(3)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          noph=noph+1
          call getphase(line,ip)
          if(xmlerr.ne.0) goto 2200
          lastkeywasph=.TRUE.
! constituents of phase
       case(4)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
! this must be for the last phase read!!
          call getconst(line,ip)
          if(xmlerr.ne.0) goto 2200
          lastkeywasph=.FALSE.
! function
       case(5)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          notp=notp+1
          call getfun(line,ip)
          if(xmlerr.ne.0) goto 2200
! parameter
       case(6)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          nopa=nopa+1
          call getparam(line,ip)
! type_definition
       case(7)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          call gettype_def(line,ip)
          if(xmlerr.ne.0) goto 2200
! list_of_references
       case(8)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
!          write(*,120)nl,ip,line(1:40)
120       format('Biblio line ',2i7,': ',a)
          call getbiblio(line,ip,0)
! add_references
       case(9)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,*)'Found add_references'
          call getbiblio(line,ip,1)
! assessed_systems
       case(10)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,*)'Found assessed system'
! database_information
       case(11)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,110)nl,line(1:40)
! version
       case(12)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,*)'Found version'
! default_command
       case(13)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,110)nl,line(1:40)
! define ?? define_system_default
       case(14)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,*)'Found define'
       end select
    enddo find
1000 continue
    return
2000 if(xmlerr.eq.0) write(*,2010)nl
     if(xmlerr.ne.0) write(*,2010)nl,xmlerr
2010 format('End of file after',i7,' lines. ',i5,' error code')
    goto 1000
2100 write(*,*)'Ending to read file at line ',nl,', due to reading error'
    goto 1000
2200 write(*,2210)key,nl,xmlerr
2210 format('Error decoding keywaord with index ',i2,' at line ',i7,&
          ', error: ',i5)
    goto 1000
  end subroutine read_tdbfile

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/

  subroutine geteldata(line,ip)
! extract a text, integer or real value from line
    integer ip,jp,kp,type,intval
    character line*(*),spname*24
    double precision relval
! element id
    if(eolch(line,ip)) goto 2000
    ellist(noel)%id=line(ip:ip+1)
    ip=ip+2
!    write(*,'(a,i3,a,a)')'pos ',ip,', symbol ',line(ip-2:ip) 
! reference state, eolch find first nonspace, state terminated by space
    if(eolch(line,ip)) goto 2000
    jp=ip
    call skip_to_space(line,jp)
    ellist(noel)%reference=line(ip:jp)
!    write(*,'(a,i3,3a)')'pos ',jp,', refer "',line(ip:jp),'"'
! mass, ip updated to next space by getrel
    ip=jp
    call getrel(line,ip,relval)
    ellist(noel)%mass=relval
! h298h0, ip automaaticaly updated to position after real
    call getrel(line,ip,relval)
    ellist(noel)%h298h0=relval
! s298
! remove any trailing !
    if(line(jp-1:jp-1).eq.'!') line(jp-1:jp-1)=' '
    call getrel(line,ip,relval)
    ellist(noel)%s298=relval
1000 continue
! enter the element also as species NOT /-
    if(ellist(noel)%id.ne.'/-') then
       nosp=nosp+1
       splist(nosp)%id=' '
       splist(nosp)%id=ellist(noel)%id
       splist(nosp)%stoichiometry=ellist(noel)%id
       splist(nosp)%numberofel=1
       allocate(splist(nosp)%elements(1:1))
       allocate(splist(nosp)%stoik(1:1))
       splist(nosp)%elements(1)=noel
       splist(nosp)%stoik(1)=1.0d0
       splist(nosp)%is_element=.TRUE.
       splist(nosp)%is_mqmqa=.FALSE.
       splist(nosp)%is_uniquac=.TRUE.
! sporder is the species in alphabetical order
       if(nosp.eq.1) then
          sporder(1)=1
       else
          sporder(nosp)=nosp
          loop: do jp=1,nosp-1
             if(splist(sporder(jp))%id.gt.splist(nosp)%id) then
                do kp=nosp,jp+1,-1
                   sporder(kp)=sporder(kp-1)
                enddo
                sporder(jp)=nosp
                exit loop
             endif
          enddo loop
20        format(a,20(1x,a))
21        format(a,20i3)
       endif
    endif
!    write(*,*)'Leaving geteldata'
    return
2000 continue
    goto 1000
  end subroutine geteldata

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getspdata(line,ip)
! extract a species ID and stoichiometry as texts
    integer ip,jp,kp
    character line*(*)
!
! remove any trailing !
    jp=index(line,'!')
    if(jp.gt.0) line(jp:jp)=' '
! beginning of species name
    if(eolch(line,ip)) goto 2000
    jp=ip
    call skip_to_space(line(ip:),jp)
    splist(nosp)%id=line(ip:ip+jp-1)
!
    ip=ip+jp+1
! extract stoichiometry
    if(eolch(line,ip)) goto 2000
    jp=ip
    call skip_to_space(line,jp)
! we should extract elements and stoichiometry eventually ...
    splist(nosp)%stoichiometry=line(ip:jp-1)
1000 continue
!    write(*,*)'Species: "',trim(splist(nosp)%id),'"',nosp
! sporder is the species in alphabetical order, nosp>1 here as elements added
    if(nosp.eq.1) then
       sporder(1)=1
    else
       loop: do jp=1,nosp-1
          if(splist(sporder(jp))%id.gt.splist(nosp)%id) then
             do kp=nosp,jp+1,-1
                sporder(kp)=sporder(kp-1)
             enddo
             sporder(jp)=nosp
             exit loop
          else
             sporder(nosp)=nosp
             exit loop
          endif
       enddo loop
!       write(*,21)'Order: ',(sporder(kp),kp=1,nosp)
!       write(*,20)'Order: ',(trim(splist(sporder(kp))%id),kp=1,nosp)
20     format(a,20(1x,a))
21     format(a,20i3)
    endif
    return
2000 xmlerr=5000
    goto 1000
  end subroutine getspdata

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getphase(line,ip)
! extract line with phase data
    character line*(*),ch1*1
    integer ip,jp,kp,intval
    double precision relval
!    write(*,*)'getphase line: ',trim(line(ip:))
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    jp=ip
    call skip_to_space(line,ip)
    phlist(noph)%name=line(jp:ip-1)
! default configmodel
    phlist(noph)%configmodel='CEF'
    fixname: do kp=1,24
       ch1=phlist(noph)%name(kp:kp)
! replace any - by _       
       if(ch1.eq.'-') then
          phlist(noph)%name(kp:kp)='_'
       elseif(ch1.eq.':') then
! this is an awkward way to specify a few things at the end of a phase name
          ch1=phlist(noph)%name(kp+1:kp+1)
          phlist(noph)%name(kp:)=' '
          if(ch1.eq.'G') then
             allocate(phlist(noph)%extramodel(1))
             phlist(noph)%extramodel%id='GAS_PHASE'
          elseif(ch1.eq.'L') then
             allocate(phlist(noph)%extramodel(1))
             phlist(noph)%extramodel%id='LIQUID_PHASE'
          elseif(ch1.eq.'B') then
             allocate(phlist(noph)%extramodel(1))
             phlist(noph)%extramodel%id='BCC_PERMUTATIONS'
          elseif(ch1.eq.'F') then
             allocate(phlist(noph)%extramodel(1))
             phlist(noph)%extramodel%id='FCC_PERMUTATIONS'
          elseif(ch1.eq.'Y') then
             allocate(phlist(noph)%extramodel(1))
             phlist(noph)%extramodel%id='LIQUID_PHASE'
             phlist(noph)%configmodel='I2SL'
          endif
          exit fixname
       endif
    enddo fixname
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
! store phase name in separate array to handle abbreviations
    phasenames(noph)=phlist(noph)%name
    if(xmlerr.ne.0) goto 1000
!
    jp=ip
    call skip_to_space(line,ip)
    phlist(noph)%type_defs=line(jp:ip-1)
    call getint(line,ip,intval)
    phlist(noph)%sublat=intval
    allocate(phlist(noph)%sites(1:intval))
    do jp=1,intval
       call getrel(line,ip,relval)
       phlist(noph)%sites(jp)=relval
    enddo
!    write(*,*)'Saved phase ',noph
1000 continue
    return
  end subroutine getphase

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getconst(line,ip)
! extract line with phase constituent data
    character line*(*),charray*3,ch1*1
! OC max 9 sublattices, max 30 per sublattice, only gas can have more ...
    character*24 constarray(9,30) 
    integer ip,jp,ll,kp,nc,nsub,ncmax
    integer nconst(9)
! terminating characters
    charray=' ,:'
! NOTE noph is used here because it is the last read phase!!!
!    write(*,*)'getconstituents'
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
! skip phase name
    call skip_to_char(':',line,ip)
! if letter before : this is a suffix to phase, skip it
    ch1=line(ip-1:ip-1)
    if(ch1.ne.' ') then
       ip=ip+2
       if(eolch(line,ip)) then
          xmlerr=5000; goto 1000
       endif
    endif
! we should noe be at the : for the first sublattice
! constituents can be separated by "," or space
! sublattices are separated by ":"
! OC MQMQA special ....
    ncmax=0
    nsub=phlist(noph)%sublat
    ip=ip+1
!    write(*,*)'Phase ',noph,' line: "',trim(line(ip:)),'"'
    sub: do ll=1,nsub
       nc=0; kp=0;
       con: do while(kp.ne.3)
          jp=ip
          call skip_to_first(charray,kp,line,ip)
!          write(*,*)'Back from skip: ',kp,jp,ip,' const: ',line(jp:ip)
          if(kp.eq.3) then
! end of sublattice
             cycle sub
          else
! new constituent, note "*" is an accepted constituent meaning all
             nc=nc+1
             if(nc.lt.1 .or. nc.gt.30) goto 2000
             constarray(ll,nc)=line(jp:ip-1)
!          write(*,'(a,2i3,2x,a)')'Constituent: ',ll,nc,constarray(ll,nc)(1:3)
          endif
! bypass terminating character and spaces, line can be  " : A , B , C : "
200       continue
          ip=ip+1
          if(eolch(line,ip)) goto 2000
          if(line(ip:ip).eq.':') then
! if kp=2 or 3 there are two : or a , followed by : 
! OK if constituent terminated by a space
             if(kp.ne.1) goto 2000
! 
             if(ll.eq.nsub) exit con
             ip=ip+1
! skip spaces after : to find first constituent in next sublattice
             if(eolch(line,ip)) goto 2000
             exit con
          elseif(line(ip:ip).eq.',') then
! if kp=2 then there are two commas without constituent in between
             if(kp.eq.2) goto 2000
! the constituent was followed by space and then a comma, skip spaces
             goto 200
          elseif(line(ip:ip).eq.'!') then
             goto 2000
          endif
       enddo con
       if(nc.gt.ncmax) ncmax=nc
       nconst(ll)=nc
    enddo sub
! remove any trailing % (indicate major constituent in TC)
    do ll=1,nsub
       do jp=1,nconst(ll)
          kp=index(constarray(ll,jp),'%')
          if(kp.gt.0) constarray(ll,jp)(kp:)=' '
       enddo
    enddo
! store in constituent record
    allocate(phlist(noph)%nconst(1:nsub))
    allocate(phlist(noph)%constituents(1:nsub,1:ncmax))
    do ll=1,nsub
       phlist(noph)%nconst(ll)=nconst(ll)
       do jp=1,nconst(ll)
          phlist(noph)%constituents(ll,jp)=constarray(ll,jp)
       enddo
    enddo
1000 continue
    return
2000 write(*,'(a,i2,a,i2)')'Sublattice,',ll,' too few/many constituents',nc
    xmlerr=5000
    goto 1000
  end subroutine getconst

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine gettype_def(line,ip)
! extract line with type definition
    character line*(*)
    type(xmltdb_typedefs), pointer :: new_type_def,save_type_def
    integer ip,jp,no
!    write(*,*)'In gettype_def: ',ip,' <',trim(line),'>'
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    save_type_def=>type_def_list%next
    allocate(type_def_list%next)
    new_type_def=>type_def_list%next
    new_type_def%id=line(ip:ip)
    new_type_def%action=line(ip+2:)
    new_type_def%next=>save_type_def
    new_type_def=>type_def_list
    notype=notype+1
! debug loop
!    do while(associated(new_type_def))
!       write(*,10)'type_def:  ',new_type_def%id,trim(new_type_def%action)
!       new_type_def=>new_type_def%next
!    enddo
10  format(a,a,2x,a)
1000 continue
    return
  end subroutine gettype_def

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getparam(line,ip)
! extract line with function with possible interval
    character line*(*),charray*(5),haha*1000
    integer ip,jp,kp,zp,ix,ll,curph,nc,degree,before
    double precision relval
! temporary storage for constituent array, tnc gives in each sublattice
    character*24, dimension(15) :: constarray
    integer tnc(9),semicolon
! for storing parameters using Trange
    type(xmltdb_trange), pointer :: prange
    type(xmltdb_trange), target :: trange
! keep track of number of elements, max 7
    integer elements(7)
! save model parameter id, phase, constituents etc, ip is position after PARA..
!    write(*,*)
!    write(*,*)'In getparam: "',trim(line),'" ',ip
! terminating characters.  Do not consider a space as terminator!
    charray=',:;)'
    if(eolch(line,ip)) goto 1000
    jp=index(line,'(')
    palist(nopa)%mpid=line(ip:jp-1)
    ip=jp+1
    call skip_to_char(',',line,ip)
    palist(nopa)%phase=line(jp+1:ip-1)
! add MPID to mpidlist if not already there    
    call check_mpid(palist(nopa)%mpid,ix)
    if(xmlerr.ne.0) goto 1000
    palist(nopa)%mpidx=ix
! second argument is array with phase names, third is number of phases,
! ix is position of last character in phase (irrelevant)
    curph=ncomp(palist(nopa)%phase,phasenames,noph,ix)
    if(curph.le.0) then
! error if not positive 
       write(*,*)'Parameter with wrong or ambigious phase name'
       xmlerr=5000; goto 1000
    endif
    palist(nopa)%phidx=curph
! crash if removed
!    write(haha,10)trim(palist(nopa)%mpid),trim(palist(nopa)%phase),curph,&
!    write(*,10)trim(palist(nopa)%mpid),trim(palist(nopa)%phase),curph,&
!    write(30,10)trim(palist(nopa)%mpid),trim(palist(nopa)%phase),curph,&
!         phlist(curph)%sublat,palist(nopa)%mpidx
10  format('Parameter MPID: "',a,'" and phase: "',a,'" ',3i3)
! increment ip to bypass , after phase name.  jp is beginning of const.name
    ip=ip+1
    jp=ip
    tnc=0
    before=0
    constarray=' '
    degree=0
    nc=0
! crash if removed
!    write(haha,'(a,a,2i5)')'database: ',line(jp:jp+10),jp,ip
!    write(*,'(a,a,2i5)')'database: ',line(jp:jp+20),jp,ip
!    write(30,'(a,a,2i5)')'database: ',line(jp:jp+10),jp,ip
!
! extract constituent array after position ip+1 (ip is at , or : after previous
    subl: do ll=1,phlist(curph)%sublat
! all constituent sequentially in constarray, tnc(ll) how many in each subl
       eternal: do while(.TRUE.)
          call skip_to_first(charray,ix,line,ip)
          select case(ix)
          case default
             write(*,*)' **** Error "',line(ip:ip),'"',ip,ix,ll,nc
             goto 1000
! a space cannot termiate a constituent, skip spaces and continue
! how to handle things  surrounded by spaces such as " ( FE , NI : Va ; 3 )
!          case(1)
!             if(eolch(line,ip)) goto 1000
! a , indicate interaction, more constituents in same sublatt
          case(1) 
             nc=nc+1
             constarray(nc)=line(jp:ip-1)
! a : indicate new sublattice
          case(2) 
             nc=nc+1
             constarray(nc)=line(jp:ip-1)
             ip=ip+1; jp=ip
             tnc(ll)=nc
!             write(*,'(a,3i4,2x,a)')'New subl: ',ll,nc,tnc(ll),trim(line(ip:))
             exit eternal
!             cycle subl
! a ; indicate end of array, can be followed by a degree ;2) or ;0) or ;) ...
          case(3) 
             nc=nc+1
             constarray(nc)=line(jp:ip-1)
             if(line(ip+1:ip+1).ne.')') then
                degree=ichar(line(ip+1:ip+1))-ichar('0')
             else
                degree=0
             endif
! bypass degree and final )
             ip=ip+3
             tnc(ll)=nc-before
             exit subl
          case(4) 
! ) indicate end of array, no degree
             if(nc.gt.4) goto 1000
             nc=nc+1
             constarray(nc)=line(jp:ip-1)
             ip=ip+1; jp=ip
             tnc(ll)=nc
             exit subl
          end select
          ip=ip+1; jp=ip
       enddo eternal
       before=nc
    enddo subl
    if(ll.ne.phlist(curph)%sublat) then
       if(phlist(curph)%configmodel(1:5).eq.'I2SL ') then
! FOR the I2SL phase the TC format has just a single sublattice for neutrals
          write(*,76)(trim(constarray(ix)),ix=1,9)
76        format('*** A ionic liquid parameter with a single sublattice,'/&
                 '*** constituents assumed to be neutrals: ',9(1x,a))
! this is OK if the constituents are neutrals, complicated to check       
! adopt OC syntax inside XMLTDB using * in first sublattice
! shift all neutral to second sublattice and add * in first
          do ix=nc+1,2,-1
             constarray(ix)=constarray(ix-1)
          enddo
          tnc(2)=nc
          tnc(1)=1
          constarray(1)='*'
       else
          write(*,77)trim(phlist(curph)%name),ll
77        format(' *** A sublattice has no constituent for phase: ',a,i2)
       endif
    endif
! check
!    write(*,'(a,i3,2x,9i3)')'tnc: ',nc,tnc
!    write(*,'(a,9(2x,a))')'constarray: ',(trim(constarray(ix)),ix=1,nc)
! We should check that the constituents exists as species!
! NOTE a * can be used to indicate that constituent irrelevant !!!
! save
    palist(nopa)%constinsubl=tnc
    allocate(palist(nopa)%constarray(nc))
    palist(nopa)%constarray=constarray
    palist(nopa)%degree=degree
! ignore ordering
!    palist(nopa)%noofel=1
!    goto 500
!------------------------------------------------
! check how many different elements in constarray
!    write(*,*)'We are here 6A',nc
    if(nc.eq.1) then
       palist(nopa)%noofel=1
    else
       ix=0
! max 7 different species in the constituent array
       elements=0
       elim: do jp=1,nc
! if the species already found the constarray(kp) is spaces
!          write(*,*)'We are here 6B',jp
          if(constarray(jp)(1:3).eq.'VA ') cycle elim
          if(constarray(jp)(1:1).ne.' ') then
! find the species index for this species, ignore VA
             do kp=1,nosp
                if(trim(constarray(jp)).eq.trim(splist(kp)%id)) then
                   if(ix.gt.7) then
                      write(*,*)'A parameter with more than 7 species!'
                      xmlerr=5000
                   endif
!                   write(*,*)'We are here 6C',ix
                   ix=ix+1; elements(ix)=kp
                endif
             enddo
          endif
! eliminate the same species in the rest of constarray
!          write(*,*)'We are here 6D',ix
          do kp=jp+1,nc
             if(constarray(kp)(1:1).ne.' ') then
                if(constarray(kp).eq.constarray(jp)) then
                   constarray(kp)=' '
                endif
             endif
          enddo
       enddo elim
       palist(nopa)%noofel=ix
!       write(*,66)ix,(trim(constarray(jp)),jp=1,nc)
66     format('Constituents: ',i2,10(2x,'"',a,'"'))
    endif
!------------------------------------------------
!500 continue
!    write(*,*)'We are here 6Z',jp
! skip spaces before tpfun, extract low_T and remove hashes ...
    call getrel(line,ip,relval)
    if(line(ip:ip).eq.',') then
! this means default value 298.15
       relval=298.15D0
!       write(*,*)'Default low_T: ',line(ip-1:ip+1),ip
       if(line(ip+1:ip+1).eq.',') then
          ip=ip+2
       endif
    endif
    palist(nopa)%low_t=relval
! bibligraphic reference
! skip high limit and search for " N " or " N!"
    jp=index(line(ip:),' N ')
    kp=index(line(ip:),'!')
    if(kp.eq.0) then
       write(*,*)'PARAMETER on line ',nl,' has no "!", struggling on ...'
       kp=len_trim(line)
    endif
! there can be several ranges ...
    semicolon=index(line(ip:),';')
    zp=ip+jp+2
! ensure no inital spaces in %BIBREF
    if(eolch(line,zp)) continue
!    if(ip+kp-2.gt.ip+jp+2) then
!       palist(nopa)%bibref=line(ip+jp+2:ip+kp-2)
    if(ip+kp-2.gt.zp) then
       palist(nopa)%bibref=line(zp:ip+kp-2)
    else
       palist(nopa)%bibref='NONE'
    endif
!    write(*,'(3a,2i5)')'bibref:"',line(ip+jp+2:ip+kp-2),"' ",kp,jp
!    write(*,*)'line:  "',trim(line(ip:)),'"'
! remove bibref from function but not N
    line(ip+jp+3:)=' '
! to decode the actual expression use getranges
!    write(*,*)'Saving parameter expression using getranges'
    call getranges(line,ip,trange)
    if(xmlerr.ne.0) goto 1000
    palist(nopa)%trange=trange
1000 continue
!    write(*,*)'End parameter decoding',xmlerr
    return
  end subroutine getparam 
!phidx
!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getfun(line,ip)
! extract line with function with possible interval
    character line*(*)
    integer ip,jp,kp
! one cannot mix target and pointer ??
    type(xmltdb_trange), pointer :: prange
    type(xmltdb_trange), target :: trange
    double precision relval
!    write(*,'(a,2i4,a,a,a)')'In getfun: ',ip,notp,' "',trim(line(ip:)),'"'
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
! extract ID and low T limit
    jp=ip
    call skip_to_space(line,ip)
    tplist(notp)%id=line(jp:ip-1)
    call getrel(line,ip,relval)
    tplist(notp)%low_t=relval
! ip is position after low T limit, extract ranges from there in getranges
! note trange is a local variable as it can not be target in declaration!!
    call getranges(line,ip,trange)
    if(xmlerr.ne.0) goto 1000
! have all ranges been stored in trange?
!    write(*,100)'range 1:',1,trange%high_t,&
!         trim(trange%expression)
!    write(*,*)'range 1: ',associated(trange%next)
!    prange=>trange%next
!    write(*,*)'Is prange associated?',associated(prange)
!    jp=1
!    do while(associated(prange))
!       jp=jp+1
!       write(*,100)'range: ',jp,prange%high_t,trim(prange%expression)
!       prange=>prange%next
!    enddo
!    write(*,*)
!    write(*,*)'Copy trange to tplist(notp)'
! copy trange into tplist
    tplist(notp)%trange=trange
! debug listing
!    write(*,*)'Success copy trange to tplist(notp)!!'
!    write(*,100)'range 1:',1,tplist(notp)%trange%high_t,&
!         trim(tplist(notp)%trange%expression)
!    write(*,*)'Done range 1: ',associated(tplist(notp)%trange%next)
!
!    prange=>tplist(notp)%trange%next
!    write(*,*)'Is prange associated?',associated(prange)
!    jp=1
!    do while(associated(prange))
!       jp=jp+1
!       write(*,100)'range: ',jp,prange%high_t,trim(prange%expression)
100    format(/a,i2,F10.2,': ',a)
!       prange=>prange%next
!    enddo
!
1000 continue
!    write(*,*)'Exiting getfun'
    return
  end subroutine getfun

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getranges(line,ip,trange)
! extract line of function or parameter with possible interval
    character line*(*),hash*1
    integer ip,jp,phash,ih,kp
    double precision relval
    logical solidlink
    type(xmltdb_trange), target :: trange
    type(xmltdb_trange), pointer :: prange
    hash='#'
!    write(*,'(a,i4,a,a,a/)')'In getranges: ',ip,' "',trim(line(ip:)),'"'
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    jp=ip
    prange=>trange
    nullify(prange%next)
! extract function between pp and next ";"
500 continue
    call skip_to_char(';',line,ip)
    if(ip.le.0) then
       write(*,'(a,2i7)')'Missing ; after FUNCTION or PARAMETER expression',&
            ip,jp
       write(*,*)'Line starting with: "',line(1:50),'"'
       write(*,*)'and end with:       "',line(max(1,jp-50):jp),'"'
       xmlerr=5000; goto 1000
    endif
!    write(*,*)'Saving: ',trim(prange%expression),ip
! save expression filling it with trailing spaces in %expression
    prange%expression=line(jp:ip)
    ip=ip+1
!    write(*,*)'Trange high_T: ',line(ip:ip+20),ip
    if(line(ip-1:ip+1).eq.';,,' .or. line(ip-1:ip+1).eq.'; ,') then
! standard Bengt default upper limit
       relval=6000.0D0
    else
       call getrel(line,ip,relval)
    endif
    prange%high_t=relval
! remove all # in expression
    kp=len_trim(prange%expression)
!    write(*,'(a,a)')'With hash:    ',prange%expression(1:kp)
    if(xmlerr.ne.0) goto 1000
    hashloop: do while(.TRUE.)
       ih=index(prange%expression,hash)
       if(ih.gt.0) then
!          write(*,*)'Removed # at position:',ih
          prange%expression(ih:kp)=prange%expression(ih+1:kp+1)
          kp=kp-1
       else
          exit hashloop
       endif
    enddo hashloop
!    write(*,'(a,a)')'Without hash: ',prange%expression(1:kp+5)
! we have to check for ranges ...
! note the final ";" is still in the expression
! There should be a Y or N after the high_T check if more ranges ....
    if(eolch(line,ip)) goto 1000
    if(line(ip:ip).eq.'y' .or. line(ip:ip).eq.'Y') then
       jp=ip+1
       allocate(prange%next)
       prange=>prange%next
       nullify(prange%next)
       goto 500
    endif
! all done for this function
1000 continue
    return
  end subroutine getranges

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getbiblio(line,ip,add)
! extract line with function with possible interval
! we can arrive here several times.  if add>0 no "source"
    character line*(*), bibref*24
    integer ip,jp,kp,add
!    write(*,*)'Biblio not implemented yet'
!    write(*,*)'References: ',line(1:ip+10),ip
!    write(*,*)trim(line(ip:))
    if(add.eq.0) then
! LIST_OF_REFERENCES NUMBER SOURCEbibref 'reference' bibref2 'referece' ...
       ip=index(line,' SOURCE')
       if(ip.le.0) then
          xmlerr=5010; goto 1000
       endif
    else
       if(eolch(line,ip)) then
          xmlerr=5010; goto 1000
       endif
    endif
! All reference in one line with no spaces in between
    ip=ip+7
    do while(.TRUE.)
!       write(*,*)trim(line(ip:)),ip
       if(eolch(line,ip)) goto 1000
       if(line(ip:ip).eq.'!') goto 1000
       jp=ip
       kp=index(line(ip:),"'")
       if(kp.le.0) then
          xmlerr=5010; goto 1000
       endif
       bibref=line(jp:jp+kp-2)
       ip=jp+kp+1
       kp=index(line(ip:),"'")
       if(kp.le.0) then
          xmlerr=5010; goto 1000
       endif
! save biblio
       nobib=nobib+1
       bibliolist(nobib)%id=bibref
       bibliolist(nobib)%text=line(ip-1:ip+kp-2)
! update position on line for next reference
       ip=ip+kp
    enddo
!
1000 continue
    return
    return
  end subroutine getbiblio

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine check_xmltdb
! checking xmltdb file
    integer ni
!
    write(*,10)noel,nosp,noph,nopa,notp,notype,nomod,nompid,nobib
10  format(/'Minimal checking xmltdb file with:',&
         /i5,' elements (including /- and Va)',&
         /i5,' species (including elements)'/i5,' phases'/i5,' parameters',&
         /i5,' TP functions',/i5,' type_defs and ',i5,' models',&
         /i5,' model parameter identifiers (mpid)'&
         /i5,' bibliographic references'/)
! check alphabetical order of species list 
!    write(*,20)(trim(splist(ni)%id),ni=1,nosp)
!    write(*,'("order: ",20i3)')(sporder(ni),ni=1,nosp)
    write(*,20)(trim(splist(sporder(ni))%id),ni=1,nosp)
20  format('Comstituents: ',20(1x,a))
1000 continue
    return
  end subroutine check_xmltdb

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine write_xmltdb(out,tdbfile)
! writing the stored information on the xml file
    integer out,ip,jp,kp,nl,ni,tr,nnw,jsign,ll,nc,tq
    character (len=512) :: line
    character (len=24) :: dummy
    character tdbfile*(*)
    logical more
    type(xmltdb_trange), target :: trange
    type(xmltdb_trange), pointer :: prange
    type(xmltdb_typedefs), pointer :: typedef
    type(xmltdb_bibliography), pointer :: bibitem
    integer parsel,nosel,totsel
!
    nnw=7; jsign=0
!
    nl=0
! predefined TP function R=8,31451;
! predefined TP function RT=R*T;
! predefined TP function GEIN(lntheta) =
!            1.5*exp(lntheta)+3*R*T*ln(1-exp(exp(lntheta))*T**(-1))

    write(out,10)trim(tdbfile)
10  format('<?xml version="1.0"?>'/&
         '<?xml-model href="database.rng" schematypens=',&
         '"http://relaxng.org/ns/structure/1.0" type="application/xml"?>'/&
         '<Database version="0">'/&
         '  <metadata>'/&
         '    <writer>xmltdbproject test'/&
         '       TDBfile="',a,'"'/&
         '    </writer>'/&
         '  </metadata>')
    do ni=1,noel
! elements
       write(out,100)ellist(ni)%id,trim(ellist(ni)%reference),&
            ellist(ni)%mass,ellist(ni)%h298h0,&
            ellist(ni)%s298
       nl=nl+1
! copy on screen
!       write(*,100)ellist(ni)%id,trim(ellist(ni)%reference),&
!            ellist(ni)%mass,ellist(ni)%h298h0,&
!            ellist(ni)%s298
100    format('  <Element id="',a,'" refstate="',a,&
            '" mass="',1pe14.6,'" H298="',1pe14.6,&
            '" S298="',1pe14.6,'" />')
    enddo
! species
    do ni=1,nosp
! in the furure species may have amendments due to MQMQA or uniquac
       write(out,200)trim(splist(ni)%id),trim(splist(ni)%stoichiometry)
200    format('  <Species id="',a,'" stoichiometry="',a,'" />')
    enddo
! functions
    do ni=1,notp
       ip=1
!       write(*,*)'Writing tpfun ',ni
! to have the value nicely within the "..."
       call wrinum(dummy,ip,nnw,jsign,tplist(ni)%low_t)
       write(out,300)trim(tplist(ni)%id),dummy(1:ip-1)
300    format('  <TPfun id="',a,'" Low_T="',a,'" >')
! maybe several ranges
       trange=tplist(ni)%trange
       ip=1
!       write(*,*)'Writing tpfun 2',trange%high_t
       call wrinum(dummy,ip,nnw,jsign,trange%high_t)
       write(out,310)dummy(1:ip-1),trim(trange%expression)
       prange=>trange%next
!       write(*,*)'writing tranges 1: ',associated(prange)
       do while(associated(prange))
          ip=1
          call wrinum(dummy,ip,nnw,jsign,prange%high_t)
          write(out,310)dummy(1:ip-1),trim(prange%expression)
310       format('    <Trange High_T="',a,'" > ',a,' />')
          prange=>prange%next
!          write(*,*)'writing tranges 2: ',associated(prange)
       enddo
       write(out,390)
390    format('  </TPfun>')
    enddo
! phases and constituents and amend phases
 !   write(*,*)'Writing phases'
    do ni=1,noph
       write(out,400)trim(phlist(ni)%name),trim(phlist(ni)%configmodel)
400    format('  <Phase id="',a,'" Configurational_model="',a,'" >')
! sites
       ip=-1
       line=' '
       do tr=1,phlist(ni)%sublat
          ip=ip+2
          call wrinum(line,ip,nnw,jsign,phlist(ni)%sites(tr))
       enddo
       write(out,410)phlist(ni)%sublat,line(1:ip-1)
410    format('    <Sublattices number_of="',i1,'"  Ratios="',a,'" >')
! constituents, each on a separate line prefixed by sublattice
       do tr=1,phlist(ni)%sublat
          do jp=1,phlist(ni)%nconst(tr)
             write(out,420)tr,trim(phlist(ni)%constituents(tr,jp))
420          format('      <Constituent sublattice="',i1,'" id="',a,'" />')
          enddo
       enddo
       write(out,422)
422    format('    </Sublattices>')
! additional models: magnetism, disordered fraction set etc not implented
! look at type_definitions
       tq=1
       do while(phlist(ni)%type_defs(tq:tq).ne.' ')
! skip first dummy typedef
          typedef=>type_def_list
          ff: do while(associated(typedef%next))
             if(typedef%id.eq.phlist(ni)%type_defs(tq:tq)) then
                write(out,430)trim(typedef%action)
430             format('    <AMEND MODEL="',a,'" />')
                exit ff
             else
                typedef=>typedef%next
             endif
          enddo ff
          tq=tq+1
       enddo
! look if extramodels allocated
       if(allocated(phlist(ni)%extramodel)) then
!          write(*,*)'extra moddel for ',phlist(ni)%name
          do tq=1,size(phlist(ni)%extramodel)
             write(out,430)phlist(ni)%extramodel%id
          enddo
       endif
       write(out,490)
490    format('  </Phase>')
    enddo
!
! all parameters, they should optionally be separated as unaries etc
! use sporder to list them in alphabetical order
! problematic to separate species and elements ...
    parsel=1
!    parsel=-300
    if(parsel.eq.1) then
! we should also order alphabetically using sporder(1..nosp) (ignoring VA)
       write(out,501)
501 format('  <Unary-parameters >')
    else
! no separation of unary, binary etc
       write(out,501)
502 format('  <All-parameters >')
    endif
    totsel=0
500 continue
    nosel=0
    paloop: do ni=1,nopa
       line=' '
! if parsel>0 select parameter with parsel=palist(ni)%noofel
! reconstruct the constituentarray from items
       if(palist(ni)%noofel.lt.0) cycle paloop
       if(parsel.gt.0 .and. parsel.ne.palist(ni)%noofel) cycle paloop
! list a parameter only once
       palist(ni)%noofel=-palist(ni)%noofel
       nosel=nosel+1
!
       line='   <Parameter id="'; ip=len_trim(line)+1
       line(ip:)=palist(ni)%mpid; ip=len_trim(line)+1
       line(ip:)='('//palist(ni)%phase; ip=len_trim(line)+1
!       if(parsel.lt.0) then
!          write(*,'(a,a)')'phase: ',trim(line(1:ip))
!          write(*,'(a,9i3)')'constinsubl: ',palist(ni)%constinsubl
!          write(*,'(a,i3)')'sublattices: ',phlist(palist(ni)%phidx)%sublat
!       endif
       ll=1; nc=0
! NOT IMPLEMENTED: a neutral in I2SL has just one sublattice in TC format
       carr: do while(.TRUE.)
          nc=nc+1
          if(nc.le.palist(ni)%constinsubl(ll)) then
             line(ip:)=','//palist(ni)%constarray(nc); ip=len_trim(line)+1
          elseif(ll.lt.phlist(palist(ni)%phidx)%sublat) then
! new sublattice
             ll=ll+1
             line(ip:)=':'//palist(ni)%constarray(nc); ip=len_trim(line)+1
          else
             line(ip:)=';'//char(ichar('0')+palist(ni)%degree)//')" '
             ip=len_trim(line)+3
             exit carr
          endif
!          if(parsel.lt.0) write(*,'(a,a,2i4)')'carr: ',line(1:ip),ll,nc
       enddo carr
!       if(parsel.lt.0) write(*,'(a,a,2i4)')'exit: ',line(1:ip),ll,nc
       line(ip:)=' Low_T="'; ip=len_trim(line)+1
       call wrinum(line,ip,nnw,jsign,palist(ni)%low_t)
!       call wrinum(line,ip,nnw,jsign,298.15D0)
       line(ip:)='"  Bibref="'
       line(ip+11:)=trim(palist(ni)%bibref)//'" >'; ip=len_trim(line)+1
!       write(out,*)trim(line(ip:))
       write(out,*)line(1:ip)
! new parameter expression with ranges
       trange=palist(ni)%trange
       ip=1
       call wrinum(dummy,ip,nnw,jsign,trange%high_t)
       write(out,520)dummy(1:ip-1),trim(trange%expression)
!       if(parsel.lt.0) write(*,*)'writing Trange'
       prange=>trange%next
       do while(associated(prange))
          ip=1
          call wrinum(dummy,ip,nnw,jsign,prange%high_t)
          write(out,520)dummy(1:ip-1),trim(prange%expression)
520       format('      <Trange High_T="',a,'" > ',a,' />')
          prange=>prange%next
       enddo
       write(out,550)
550    format('    </Parameter>')
    enddo paloop
    parsel=parsel+1
    totsel=totsel+nosel
    repeat: if(parsel.eq.2) then
       write(*,*)'Listed unary parameters:  ',nosel,' out of ',nopa
       write(out,552)
552    format('  </Unary-parameters>'/'  <Binary-parameters>')
       goto 500
    elseif(parsel.eq.3) then
       write(*,*)'Listed binary parameters: ',nosel,' out of ',nopa
       if(totsel.eq.nopa) then
          write(out,553)
553       format('  </Binary-parameters>')
          exit repeat
       else
          write(out,554)
554       format('  </Binary-parameters>'/'  <Higher-parameters>')
          parsel=-100
          goto 500
       endif
    elseif(parsel.lt.-200.0) then
       write(*,*)'Listed higher order parameters: ',nosel
       write(out,556)
556    format('  </All-parameters>')
    else
       write(out,558)
558    format('  </Higher-parameters>')
    endif repeat
590 continue
!
! binary parameters ordered by system
! ternary parameters ordered by system
! higher order parameters
! model information
! Bibliography
    if(nobib.eq.0) then
       write(*,*)'No bibliographic references'
    else
!       write(*,*)'The bibliography'
       write(out,900)
900    format('  <Bibliography>')
       do ni=1,nobib
          write(out,910)trim(bibliolist(ni)%id),trim(bibliolist(ni)%text)
910       format('    <Bibitem ID="',a,'" > ',a,' />')
       enddo
       write(out,911)
911    format('  </Bibliography>')
    endif
    write(out,90)
90  format('</Database>')
!
1000 continue
    return
  end subroutine write_xmltdb

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/

  subroutine getitem(line,ip,type,text,intval,relval)
! extract a text, integer or real value from line
! depending on type = 1, 2 or 3
    integer ip,jp,type,intval
    character line*(*),text*(*)
    double precision relval
    if(eolch(line,ip)) goto 2000
    jp=ip
    call skip_to_space(line,jp)
    if(jp.le.ip) goto 2000
    select case(type)
    case default
       xmlerr=5002
    case(1)
       text=line(ip:jp)
       ip=jp
    case(2)
       call getint(line,ip,intval)
    case(3)
       call getrel(line,ip,relval)
    end select
1000 continue
    return
2000 xmlerr=5000
    goto 1000
end subroutine getitem

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!
  
!\addtotable integer function istdbkeyword
!\begin{verbatim} %-
 integer function istdbkeyword(text,nextc)
! compare a text with a given keyword. Abbreviations allowed (not within _)
! but the keyword and abbreviation must be surrounded by spaces
! nextc set to space character in text after the (abbreviated) keyword
   implicit none
   character text*(*)
   integer nextc
!\end{verbatim} %+
! only those currently implemented ... rest ignored
   integer, parameter :: kwl=20
   integer, parameter :: nkw=14
   character (len=kwl), dimension(nkw), parameter :: keyword=&
        ['ELEMENT             ','SPECIES             ',&
         'PHASE               ','CONSTITUENT         ',&
         'FUNCTION            ','PARAMETER           ',&
         'TYPE_DEFINITION     ','LIST_OF_REFERENCES  ',&
         'ADD_REFERENCES      ','ASSESSED_SYSTEMS    ',&
         'DATABASE_INFORMATION','VERSION             ',&
         'DEFAULT_COMMAND     ','DEFINE              ']
!   
   character word*64
   integer j,ks,kt
! extract the first word of text
   ks=1
   if(eolch(text,ks)) then
! if empty line, just exit
      j=0; goto 1000
   else
! find the space after the first word
      kt=ks+index(text(ks:),' ')-1
! the abbreviation of the keyword must be at least 3 character, max kwl
      if(kt-ks.lt.3 .or. kt-ks.ge.kwl) then
!         write(*,*)'3E too long keyword: "',trim(text),'"',kt-ks,kwl
         j=0; goto 1000
      endif
   endif
   word=text(ks:kt)
   kt=kt-ks
   call capson(word)
! replace - by _
90 continue
   j=index(word,'-')
   if(j.gt.0) then
      word(j:j)='_'
      goto 90
   endif
! check if word is an abbreviation of a keyword
!   write(*,*)'abbreviation: ',kt,'>',word(1:kt),'<'
!   do j=1,10
   do j=1,nkw
      if(word(1:kt).eq.keyword(j)(1:kt)) goto 100
   enddo
   j=0
!   write(*,99)j,nextc,text(1:nextc),trim(text)
99 format('3E Not a keyword: ',2i3,'>',a,'<'/1x,a)
   goto 1000
! found keyword at start of line, set nextc to be positioned at the final space
100 continue
   if(j.eq.11 .and. kt.lt.8) then
! we found 'DATA' at the start of several lines that is not DATABASE_INFO
!      write(*,*)'3E why? ',trim(text),kt
      j=0
      goto 1000
   endif
   nextc=ks+kt
!   write(*,101)j,nextc,text(1:nextc),trim(text)
101 format('3E Found keyword: ',2i3,'>',a,'<'/1x,a)
1000 continue
   istdbkeyword=j
   return
 end function istdbkeyword

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!
  
 subroutine check_mpid(mpid,mpidx)
! adds and returns index of model parameter identifier
   character mpid*(*)
   integer mpidx,ix
! no abbreviations are allowed in MPID !!!
   do ix=1,nompid
      if(mpid.eq.mpidlist(ix)%id) then
         mpidx=ix; goto 1000
      endif
   enddo
! new mpid
   nompid=nompid+1
   mpidlist(nompid)%id=mpid
   mpidx=nompid
!
!   write(*,*)'New mpid: ',trim(mpid),mpidx
! possibly check if associated with a specific model
   mpidlist(nompid)%index_in_modellist=0
1000 continue
   return
 end subroutine check_mpid

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

end MODULE XMLTDB_LIB
