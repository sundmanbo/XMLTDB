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
  TYPE xmltdb_tpfun
! specification of the XMLelement TPFUN
     character (len=16) :: id
     double precision :: low_t,high_t
     integer :: tranges
     character*500 :: expression
     type(xmltdb_string), pointer :: long_expression
     type(xmltdb_tpfun), dimension(:), allocatable :: ranges
  end type xmltdb_tpfun
!------------------------------------------    
  TYPE xmltdb_trange
! specification of the simple XMLelement TRANGE
     double precision :: high_t
     type(xmltdb_string), pointer :: expression
  end type xmltdb_trange
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
! this is the expression as a long text
     type(xmltdb_string), pointer :: longfun
! this is the expression as the index of a tpfun
     integer tpfunindex
  end type xmltdb_parameter
!------------------------------------------      
  TYPE xmltdb_xmodel
! specification of the XMLelement MODEL (excluding constitution)
     character (len=24) :: ID
     type(xmltdb_string), pointer :: string
! necessary model parameter identifiers
     integer, dimension(:), allocatable :: mpid
  end type xmltdb_xmodel
!------------------------------------------      
  TYPE xmltdb_bibliography
! specification of the XMLelement BIBLIOGRAPHY
     character (len=24) :: ID
     type(xmltdb_string), pointer :: string
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
  integer, dimension(8) :: limits
  integer noel,nosp,noph,nopa,notp,nomod,nompid,nobib,notype
  type(xmltdb_typedefs), target :: type_def_list
  type(xmltdb_typedefs), pointer :: new_type_def
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
    allocate(phlist(1:maxall(3)))
    allocate(phasenames(1:maxall(3)))
    allocate(palist(1:maxall(4)))
    allocate(tplist(1:maxall(5)))
    allocate(modellist(1:maxall(6)))
    allocate(mpidlist(1:maxall(7)))
    allocate(bibliolist(1:maxall(8)))
    noel=0; nosp=0; noph=0; nopa=0; notp=0; nomod=0; nompid=0; nobib=0
! initiate list of type_defs, quite clumsy
    type_def_list%id='@'
    type_def_list%action='head of list'
    nullify(type_def_list%next)
    return
  end subroutine init_xmltdb

  subroutine read_tdbfile(infile,origin)
    integer infile,ip,key,nextc,nl,jp,kp,nl1
    character (len=4000) :: line
    character (len=512) :: line2
! this indicate which software generated the TDB file
    character*32 origin
    logical more,lastkeywasph
!
    write(*,*)'Reading a TDB file generated by: ',trim(origin)
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
!       write(*,*)'Keyword index: ',key,xmlerr,ip
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
77        continue
          nl=nl+1
          read(infile,100,end=2000,err=2100)line2
          kp=1
! skip empty lines
          if(eolch(line2,kp)) goto 77
! append new line after jp
          jp=len_trim(line)
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
          write(*,110)nl,line(1:40)
! add_references
       case(9)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,*)'Found add_references'
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
2000 write(*,2010)nl
2010 format('End of file after',i7,' lines')
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
    integer ip,jp,type,intval
    character line*(*)
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
       splist(nosp)%numberofel=1
       allocate(splist(nosp)%elements(1:1))
       allocate(splist(nosp)%stoik(1:1))
       splist(nosp)%elements(1)=noel
       splist(nosp)%stoik(1)=1.0d0
       splist(nosp)%is_element=.TRUE.
       splist(nosp)%is_mqmqa=.FALSE.
       splist(nosp)%is_uniquac=.TRUE.
    endif
    return
2000 continue
    goto 1000
  end subroutine geteldata

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getspdata(line,ip)
! extract a species ID and stoichiometry as texts
    integer ip,jp
    character line*(*)
!
    xmlerr=5000
    if(eolch(line,ip)) goto 1000
    call skip_to_space(line,jp)
    splist(nosp)%id=line(ip:jp)
!
    ip=jp
    if(eolch(line,ip)) goto 1000
    call skip_to_space(line,jp)
! remove any trailing !
    if(line(jp-1:jp-1).eq.'!') line(jp-1:jp-1)=' '
! we should extract elements and stoichiometry eventually ...
    splist(nosp)%id=line(ip:jp)
    write(*,*)'Species not implemented yet'
    xmlerr=5000
1000 continue
    return
  end subroutine getspdata

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getphase(line,ip)
! extract line with phase data
    character line*(*)
    integer ip,jp,kp,intval
    double precision relval
!    write(*,*)'getphase line: ',trim(line(ip:))
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    jp=ip
    call skip_to_space(line,ip)
    phlist(noph)%name=line(jp:ip-1)
    do kp=1,24
! replace any - by _       
       if(phlist(noph)%name(kp:kp).eq.'-') then
          phlist(noph)%name(kp:kp)='_'
       endif
    enddo
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
! store phase name in separate array to handle appreviations
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
! default configmodel
    phlist(noph)%configmodel='CEF'
!    write(*,*)'Saved phase ',noph
1000 continue
    return
  end subroutine getphase

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getconst(line,ip)
! extract line with phase constituent data
    character line*(*),charray*3
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
! new constituent
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
    character line*(*),charray*(5),haha*256
    integer ip,jp,kp,ix,ll,curph,nc,degree,before
    double precision relval
! temporary storage for constituent array, tnc gives in each sublattice
    character*24, dimension(9) :: constarray
    integer tnc(9)
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
! crash if removed
!    write(haha,10)trim(palist(nopa)%mpid),trim(palist(nopa)%phase),curph,&
!    write(*,10)trim(palist(nopa)%mpid),trim(palist(nopa)%phase),curph,&
    write(30,10)trim(palist(nopa)%mpid),trim(palist(nopa)%phase),curph,&
         phlist(curph)%sublat,palist(nopa)%mpidx
10  format('Parameter MPID: "',a,'" and phase: "',a,'" ',3i3)
! increment ip to bypass , after phase name.  jp is beginning of const.name
    ip=ip+1
    jp=ip
    tnc=0
    before=0
    constarray=' '
    degree=0
! crash if removed
!    write(haha,'(a,a,2i5)')'database: ',line(jp:jp+10),jp,ip
!    write(*,'(a,a,2i5)')'database: ',line(jp:jp+10),jp,ip
    write(30,'(a,a,2i5)')'database: ',line(jp:jp+10),jp,ip
!
! extract constituent array after position ip+1 (ip is at , or : after previous
    subl: do ll=1,phlist(curph)%sublat
! do not zero nc, they are all sequentially in constarray
!       nc=0
       eternal: do while(.TRUE.)
          call skip_to_first(charray,ix,line,ip)
!+          write(*,'(a,3i3,a,a,a)')'Found :',ix,jp,ip,' "',line(jp:ip-1),'"'
          select case(ix)
          case default
             write(*,*)' **** Error "',line(ip:ip),'"',ip,ix
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
! a ; indicate end of array, can be followed by a degree
          case(3) 
             nc=nc+1
             constarray(nc)=line(jp:ip-1)
             degree=ichar(line(ip+1:ip+1))-ichar('0')
             ip=ip+3
             tnc(ll)=nc-before
             exit eternal
          case(4) 
! ) indicate end of array, no degree
             if(nc.gt.4) goto 1000
             nc=nc+1
             constarray(nc)=line(jp:ip-1)
             ip=ip+1; jp=ip
             tnc(ll)=nc
             exit eternal
          end select
          ip=ip+1; jp=ip
! crash if removed
!          write(haha,'(a,3i3,2x,a)')'Rest: ',ll,nc,tnc(ll),trim(line(ip:))
!          write(*,'(a,3i3,2x,a)')'Rest: ',ll,nc,tnc(ll),trim(line(ip:))
          write(30,'(a,3i3,2x,a)')'Rest: ',ll,nc,tnc(ll),trim(line(ip:))
       enddo eternal
       before=nc
    enddo subl
! check
!+    write(*,'(a,i3,2x,9i3)')'tnc: ',nc,tnc
!+    write(*,'(a,5(2x,a))')'constarray: ',(trim(constarray(ix)),ix=1,nc)
! We should check that the constituents exists as species!
! save!
    palist(nopa)%constinsubl=tnc
    allocate(palist(nopa)%constarray(nc))
    palist(nopa)%constarray=constarray
    palist(nopa)%degree=degree
! skip spaces before tpfun, extract low_T and remove hashes ...
    call getrel(line,ip,relval)
    palist(nopa)%low_t=relval
! bibligraphic reference
! skip high limit and search for "N"
    jp=index(line(ip:),' N ')
    kp=index(line(ip:),'!')
    palist(nopa)%bibref=line(ip+jp+2:ip+kp-2)
!    write(*,*)'bibref: ',line(ip+jp+2:ip+kp-2)
! remove bibref from function but not N
    line(ip+jp+3:)=' '
! decode the actual expression use getfun
! We must provide a name for the tpfun ...
!    write(*,*)'tpfun: ',trim(line(ip:))
    palist(nopa)%tpfun=trim(line(ip:))
    nullify(palist(nopa)%longfun)
    palist(nopa)%tpfunindex=0
    goto 1000
! maybe store tpfun as tpfun?
    notp=notp+1
    call getfun(line,ip)
    if(xmlerr.ne.0) goto 1000
    palist(nopa)%tpfunindex=notp
1000 continue
!    write(*,*)'End parameter decoding',xmlerr
    return
  end subroutine getparam

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getfun(line,ip)
! extract line with function with possible interval
    character line*(*),hash*1
    integer ip,jp,phash,ih,kp
    double precision relval
    hash='#'
!    write(*,'(a,2i4,a,a,a)')'In getfun: ',ip,notp,' "',trim(line(ip:)),'"'
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    jp=ip
    call skip_to_space(line,ip)
    tplist(notp)%id=line(jp:ip-1)
    call getrel(line,ip,relval)
    tplist(notp)%low_t=relval
    jp=ip
! we have to implement very long expression strings, more than 5000 characters
    call skip_to_char(';',line,ip)
! remve all # in line(jp+1:ip), note jp is changing!
!    write(*,'(a,i3,2x,a)')'With hash:    ',ip,trim(line(jp+1:ip+5))
    hashloop: do while(.TRUE.)
       ih=index(line(jp+1:ip),hash)
       if(ih.gt.0) then
!          write(*,*)'Removed # at position:',ih
          line(jp+ih:ip)=line(jp+ih+1:ip)
          ip=ip-1
       else
          exit hashloop
       endif
    enddo hashloop
!    write(*,'(a,i3,2x,a)')'Without hash: ',ip,trim(line(jp+1:ip+5))
! we have to check for ranges ...
! note the final ";" is in the expression
    tplist(notp)%tranges=1
    tplist(notp)%expression=line(jp+1:ip)
    nullify(tplist(notp)%long_expression)
    ip=ip+1
    call getrel(line,ip,relval)
    tplist(notp)%high_t=relval
!    write(*,*)'Saved tpfun: ',notp
1000 continue
    return
  end subroutine getfun

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine check_xmltdb
! checking xmltdb file
    write(*,10)noel,nosp,noph,nopa,notp,notype,nomod,nompid,nobib
10  format(/'Minimal checking xmltdb file with:',&
         /i5,' elements (including /- and Va)',&
         /i5,' species'/i5,' phases'/i5,' parameters',&
         /i5,' TP functions',/i5,' type_defs and ',i5,' models',&
         /i5,' model parameter identifiers (mpid)'&
         /i5,' bibliographic references'/)
    return
  end subroutine check_xmltdb

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine write_xmltdb(out)
! writing the stored information on the xml file
    integer out,ip,jp,kp,nl,ni,tr,nnw,jsign,ll,nc
    character (len=512) :: line
    character (len=24) :: dummy
    logical more
!
    nnw=7; jsign=0
!
    nl=0
! predefined TP function R=8,31451;
! predefined TP function RT=R*T;
! predefined TP function GEIN(lntheta) =
!            1.5*exp(lntheta)+3*R*T*ln(1-exp(exp(lntheta))*T**(-1))

    write(out,10)
10  format('<?xml version="1.0"?>'/&
         '<?xml-model href="database.rng" schematypens=',&
         '"http://relaxng.org/ns/structure/1.0" type="application/xml"?>'/&
         '<Database version="0">'/&
         '  <metadata>'/&
         '    <writer>xmltdbproject test</writer>'/&
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
! functions
    do ni=1,notp
       ip=1
       call wrinum(dummy,ip,nnw,jsign,tplist(ni)%low_t)
       write(out,300)trim(tplist(ni)%id),dummy(1:ip-1)
300    format('  <TPfun id="',a,'" Low_T="',a,'" >')
       if(associated(tplist(ni)%long_expression)) then
          write(*,*)'Cannot handle long expressions'
          xmlerr=5000; goto 1000
       endif
! maybe several ranges
       do tr=1,tplist(ni)%tranges
          ip=1
          call wrinum(dummy,ip,nnw,jsign,tplist(ni)%high_t)
          write(out,310)dummy(1:ip-1),trim(tplist(ni)%expression)
310       format('    <Trange High_T="',a,'" > ',a,' </Trange>')
       enddo
       write(out,390)
390    format('  </TPfun>')
    enddo
! phases and constituents and amend phases
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
420          format('      </Constituent sublattice="',i1,'" id="',a,'" />')
          enddo
       enddo
       write(out,422)
422    format('    </Sublattices>')
       write(out,490)
490    format('  </Phase>')
    enddo
! additional models: magnetism, disordered fraction set etc not implented
!
! all parameters, they should optionally be separated as unaries etc
    paloop: do ni=1,nopa
       line=' '
! reconstruct the constituentarray from items
       line=' <Parameter id="'; ip=len_trim(line)+1
       line(ip:)=palist(ni)%mpid; ip=len_trim(line)+1
       line(ip:)='('//palist(ni)%phase; ip=len_trim(line)+1
       ll=1; nc=0
!       write(*,'(a,9i3)')'constinsubl: ',palist(ni)%constinsubl
!       write(*,'(a,i3)')'sublattices: ',phlist(palist(ni)%phidx)%sublat
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
!          write(*,'(a,a,2i4)')'carr: ',line(1:ip),ll,nc
       enddo carr
!       write(*,'(a,a,2i4)')'exit: ',line(1:ip),ll,nc
       line(ip:)=' Low_T="'; ip=len_trim(line)+1
       call wrinum(line,ip,nnw,jsign,palist(ni)%low_t)
!       call wrinum(line,ip,nnw,jsign,298.15D0)
       line(ip:)='"  Bibref="'
       line(ip+11:)=trim(palist(ni)%bibref)//'" >'; ip=len_trim(line)+1
       line(ip:)=palist(ni)%tpfun; ip=len_trim(line)
       line(ip:)='   </Parameter>'
       write(out,*)trim(line)
!       write(*,*)trim(line)
    enddo paloop
!
! unary parameters
! binary parameters ordered by system
! ternary parameters ordered by system
! higher order parameters
! model information and anything else?    
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
!         write(*,*)'3E too long keyword: ',trim(text),kt-ks,kwl
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
