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
!---------------------------
! these are amend model indices
    integer, parameter :: IHJBCC=1,IHJREST=2,IHJQX=3,GLOWTEIN=4
    integer, parameter :: LIQ2STATE=5,VOLOWP=6,DISFS=7,FCCPERM=8
    integer, parameter :: BCCPERM=9
! Not yet unstalled
    integer, parameter :: EEC=10,MQMQA=11,UNIQUAC=1
! NOTE: DISFS require names of ordered and disordered phases and the number
!        of sublattices to sum in the ordered phase
!        (this value is either all or all except the last (intersttial))
!----------------------------
! error messages
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
!  integer,  parameter :: VOLUME_MODEL
!  integer,  parameter :: EEC_MODEL    ?? For whole database
!  integer,  parameter :: EBEF         ?? maybe not needed
!  integer,  parameter :: MQMQMA
!  integer,  parameter :: UNIQUAC
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
  end type xmltdb_element
!------------------------------------------    
  TYPE xmltdb_species
! specification of the simple XMLelement SPECIES
     character (len=24) :: id
     character (len=48) :: stoichiometry
     logical IS_mqmqa, IS_uniquac, IS_element
! The mqmqma data are bonds and FNN/SNN ratio, for UNIQUAC volume and area
     character (len=80) :: amendspecies
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
! special for Pandat property=1 and variable=2 (function=0)
     integer pandatype
  end type xmltdb_tpfun
!------------------------------------------    
!  TYPE xmltdb_modelparameterid
  TYPE xmltdb_mpid
! specification of the XMLelement MODEL_PARAMETER_ID
     character (len=12) :: id
! this is index of model in modellist, phase must have this model!!
     integer modix
  end type xmltdb_mpid
!------------------------------------------      
  TYPE xmltdb_addmodel
! one or more of this record is linked from a phase record
! It has links to a xmltdb_model record and possible additional data
     character (len=24) :: ID
     integer :: model_int
     double precision :: model_real
! other info needed stored as text here
     character*128 :: model_info
     type(xmltdb_model), pointer :: model
! linked list
     type(xmltdb_addmodel), pointer :: next
  end type xmltdb_addmodel
!------------------------------------------    
  TYPE xmltdb_phase
! specification of the XMLelement PHASE
     character (len=24) :: name
! the constmodel defines sublattices and, when applicable, sites
     character (len=24) :: configmodel
     character (len=6) :: aggregation
! sublattices and sites read from TDB file
     integer sublat
     double precision, dimension(:), allocatable :: sites
! TYPE_DEF letters
     character*24 type_defs
! list with links to additional models (magnetism, disoredered sets etc)
     type(xmltdb_addmodel), pointer :: amendphase
     integer disfs_sum
! constituents
     integer, dimension(:), allocatable :: nconst
     character*24, dimension(:,:), allocatable :: constituents
  end type xmltdb_phase
!------------------------------------------      
  TYPE xmltdb_parameter
! specification of the XMLelement PARAMETER
! model parameter id and phase in parameter
     character*12 mpid
     character*24 phase
     character*128 parcomment
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
!     integer, dimension(:), allocatable :: mpid
  end type xmltdb_xmodel
!------------------------------------------      
  TYPE xmltdb_model
! list specification of models such as magnetism, permutations etc
     character (len=24) :: ID
     character*24 :: bibref
     integer :: model_int
     double precision :: model_real
! necessary model parameter identifiers such as TC, BMAGN, LNTH
     integer :: numberof_mpid
     character*24, dimension(:), allocatable :: required_mpid
! other info needed, functions, phases etc
     character*128,dimension(:), allocatable :: model_info
     double precision, dimension(:), allocatable :: model_numbers
  end type xmltdb_model
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
     type(xmltdb_model), pointer :: modelink
! model_int is used for the DISFS model for sublattices to be summed
     integer :: modelindex,model_int
     type(xmltdb_typedefs), pointer :: next
  end type xmltdb_typedefs
!------------------------------------------      
  TYPE xmltdb_ignored
! specification of the XMLelement for things ignored
     character id*24
     character*512 :: action
     type(xmltdb_ignored), pointer :: next
  end type xmltdb_ignored
!------------------------------------------      
!
! Allocatable arrys of elements, species etc
  type(xmltdb_element), dimension(:), allocatable :: ellist
  type(xmltdb_species), dimension(:), allocatable :: splist
  type(xmltdb_phase), dimension(:), allocatable :: phlist
  type(xmltdb_parameter), dimension(:), allocatable :: palist
  type(xmltdb_tpfun), dimension(:), allocatable :: tplist
  type(xmltdb_model), dimension(:), allocatable, target :: mlist
  type(xmltdb_mpid), dimension(:), allocatable :: mpidlist
  type(xmltdb_bibliography), dimension(:), allocatable :: bibliolist
! for alphabetical ordering of species and phases
  integer, dimension(:), allocatable :: sporder
  integer, dimension(:), allocatable :: phorder
  integer, dimension(8) :: limits
  integer noel,nosp,noph,nopa,notp,nomod,nompid,nobib,notype,nl
  type(xmltdb_typedefs), target :: type_def_list
  type(xmltdb_typedefs), pointer :: new_type_def
  type(xmltdb_ignored), target :: ignored_list
  type(xmltdb_ignored), pointer :: new_ignored
! TDB file software
  character (len=12), dimension(8) :: software
  data software&
       /'Thermo-Calc ','OpenCalphad ','Pandat      ','MatCalc    ',&
        'PyCalphad   ','            ','            ','            '/
! this is set to indicate the softtware, tofs=4 means MatCalc, =3 Pandat
  integer tofs
!
!-----------------------------
! to check phases are OK (ambiguietty etc)
  character*24, dimension(:), allocatable :: phasenames
! this is the global variable to know which line was read
  integer nline
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
    allocate(mlist(1:maxall(6)))
    allocate(mpidlist(1:maxall(7)))
    allocate(bibliolist(1:maxall(8)))
    noel=0; nosp=0; noph=0; nopa=0; notp=0; nomod=0; nompid=0; nobib=0
! initiate list of type_defs, quite clumsy
    sporder=0
    type_def_list%id='@'
    type_def_list%action='head of list'
    nullify(type_def_list%next)
    ignored_list%id='Dummy'
    ignored_list%action='head of list'
    nullify(ignored_list%next)
! descriptions of models implemented
    call enter_models
    return
  end subroutine init_xmltdb

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/

  subroutine enter_models
! enter known models
    integer ia
! NOTE the models should be entered in the order of the constants
!    integer, parameter :: IHJBCC=1,IHJREST=2,IHJQX=3,GLOWTEIN=4
!    integer, parameter :: LIQ2STATE=5,DISFS=6,FCCPERM=7,BCCPERM=8
!
! 1. Inden-Hillert-Jarl magnetic model for BCC
    nomod=nomod+1
    mlist(nomod)%id='IHJBCC'
    mlist(nomod)%bibref='82Her'
    mlist(nomod)%numberof_mpid=2
    allocate(mlist(nomod)%required_mpid(2))
    mlist(nomod)%required_mpid(1)='TC'
    mlist(nomod)%required_mpid(2)='BMAGN'
! the anti-ferromagnetic factor
    mlist(nomod)%model_real=-1.0D0
! functions above and below TC
    allocate(mlist(nomod)%model_info(2))
    mlist(nomod)%model_info(1)=' +1-0.905299383*TAO**(-1)'//&
         '-0.153008346*TAO**3-.00680037095*TAO**9-.00153008346*TAO**15;'
    mlist(nomod)%model_info(2)=' -.0641731208*TAO**(-5)-'//&
         '.00203724193*TAO**(-15)-.000427820805*TAO**(-25);'
    mlist(nomod)%model_int=0
!------------------------------------------
! 2. Inden-Hillert-Jarl magnetic model for BCC
    nomod=nomod+1
    mlist(nomod)%id='IHJREST'
    mlist(nomod)%bibref='82Her'
    mlist(nomod)%numberof_mpid=2
    allocate(mlist(nomod)%required_mpid(2))
    mlist(nomod)%required_mpid(1)='TC'
    mlist(nomod)%required_mpid(2)='BMAGN'
! the anti-ferromagnetic factor
    mlist(nomod)%model_real=-3.0D0
    allocate(mlist(nomod)%model_info(2))
    mlist(nomod)%model_info(1)=' +1-0.860338755*TAO**(-1)'//&
         '-0.17449124*TAO**3-.00775516624*TAO**9-.0017449124*TAO**15;'
    mlist(nomod)%model_info(2)=' -.0426902268*TAO**(-5)'//&
         '-.0013552453*TAO**(-15)-.000284601512*TAO**(-25);'
    mlist(nomod)%model_int=0
!------------------------------------------
! 3. Inden-Hillert-Jarl magnetic model modified by Qing and Xiong
    nomod=nomod+1
    mlist(nomod)%id='IHJQX'
    mlist(nomod)%bibref='12Xiong'
    mlist(nomod)%numberof_mpid=3
    allocate(mlist(nomod)%required_mpid(3))
    mlist(nomod)%required_mpid(1)='CT'
! Separate Neel T
    mlist(nomod)%required_mpid(2)='NT'
    mlist(nomod)%required_mpid(3)='BMAGN'
! the anti-ferromagnetic factor
    mlist(nomod)%model_real=0.0D0
    allocate(mlist(nomod)%model_info(2))
    mlist(nomod)%model_info(1)=' +1-0.842849633*TAO**(-1)-0.174242226*TAO**3'//&
         '-.00774409892*TAO**9-.00174242226*TAO**15-.000646538871*TAO**21;'
    mlist(nomod)%model_info(2)=' -.0261039233*TAO**(-7)'//&
         '-.000870130777*TAO**(-21)-.000184262988*TAO**(-35)'//&
         '-6.65916411E-05*TAO**(-49);'
    mlist(nomod)%model_int=0
!----------------------------------------------
! 4. Einstein low T vibrational model
    nomod=nomod+1
    mlist(nomod)%id='GLOWTEIN'
    mlist(nomod)%bibref='01Qing'
    mlist(nomod)%model_int=0
    mlist(nomod)%model_real=0.0D0
    mlist(nomod)%numberof_mpid=1
    allocate(mlist(nomod)%required_mpid(1))
    mlist(nomod)%required_mpid(1)='LNTH'
    allocate(mlist(nomod)%model_info(1))
    mlist(nomod)%model_info(1)='Gibbs energy due to the Einstein low T '//&
         'vibrational entropy model, G=1.5*R*THETA+3*R*T*LN(1-EXP(-THETA/T)).'
!----------------------------------------------
! 5. Liquid 2state model
    nomod=nomod+1
    mlist(nomod)%id='LIQ2STATE'
    mlist(nomod)%bibref='14Becker'
    mlist(nomod)%model_int=0
    mlist(nomod)%model_real=0.0D0
    mlist(nomod)%numberof_mpid=2
    allocate(mlist(nomod)%required_mpid(2))
    mlist(nomod)%required_mpid(1)='G2'
    mlist(nomod)%required_mpid(2)='LNTH'
    allocate(mlist(nomod)%model_info(1))
    mlist(nomod)%model_info(1)='Unified model for the liquid and '//&
         'amorphous state inluding the Einstein low T model'
!----------------------------------------------
! 6. Volume model for low and moderate Pressures (Lu 2003)
    nomod=nomod+1
    mlist(nomod)%ID='VOLOWP'
! Reference paper by Lu, Selleby,Sundman 2005 in Acta Metal 
    mlist(nomod)%bibref='05Lu'
    mlist(nomod)%numberof_mpid=3
    allocate(mlist(nomod)%required_mpid(3))
    mlist(nomod)%required_mpid(1)='V0'
    mlist(nomod)%required_mpid(2)='VA'
    mlist(nomod)%required_mpid(3)='VB'
    allocate(mlist(nomod)%model_info(1))
    mlist(nomod)%model_info(1)='The volume of a phase is described '//&
         'as function of T, P and the constitution of the phase.'
!
!----------------------------------------------
! 7. Disordered model for different phases, need to know how many sublattices 
    nomod=nomod+1
    mlist(nomod)%ID='DISFS'
! Reference AL-Fe assessment or has disordered fraction sets been used before?
    mlist(nomod)%bibref='09Sun'
    mlist(nomod)%numberof_mpid=0
    allocate(mlist(nomod)%model_info(1))
    mlist(nomod)%model_info(1)='The disordered fractions are summed'//&
         ' over the ordered sublattices indicated at the phase.'
! in the phase typedefs record with a link to this record the phase names
!  are stored and also the number of sublattices to be summed in the
! ordered phase.  For FCC, BCC normally 4 (the last sublattice is interstitial)
! For sigma, mu all sublattices are normally summed.
! this is stored as model_int in the xmltdb_typdefs record for the ordered ph.
!----------------------------------------------
! 8. Permutation of ordered FCC parameters
    nomod=nomod+1
    mlist(nomod)%id='FCCPERM'
    mlist(nomod)%bibref='09Sun'
    mlist(nomod)%model_int=0
    mlist(nomod)%model_real=0.0D0
    mlist(nomod)%numberof_mpid=0
!    allocate(mlist(nomod)%required_mpid(1))
!    mlist(nomod)%required_mpid(1)=
    allocate(mlist(nomod)%model_info(1))
    mlist(nomod)%model_info(1)='Permutations of ordered FCC parameters with '//&
         'the same set of elements are listed only once.'
!----------------------------------------------
! 9. Permutations of ordered BCC parameters
    nomod=nomod+1
    mlist(nomod)%id='BCCPERM'
    mlist(nomod)%bibref='09Sun'
    mlist(nomod)%model_int=0
    mlist(nomod)%model_real=0.0D0
    mlist(nomod)%numberof_mpid=0
!    allocate(mlist(nomod)%required_mpid(1))
!    mlist(nomod)%required_mpid(1)=
    allocate(mlist(nomod)%model_info(1))
    mlist(nomod)%model_info(1)='Permutations of ordered BCC parameters with '//&
         'the same set of elements are listed only once.'
!----------------------------------------------
!    nomod=nomod+1
!    mlist(nomod)%id=
!    mlist(nomod)%bibref=
!    mlist(nomod)%model_int=
!    mlist(nomod)%model_real=
!    mlist(nomod)%numberof_mpid=0
!    allocate(mlist(nomod)%required_mpid(2))
!    mlist(nomod)%required_mpid(1)=
!    allocate(mlist(nomod)%model_info(2))
!    mlist(nomod)%model_info(1)=
!----------------------------------------------
1000 continue
    return
  end subroutine enter_models

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/

  subroutine read_tdbfile(infile,origin)
    integer infile,ip,key,nextc,nl,jp,kp,nl1,zp,qref,pantyp
    character (len=10000) :: line
    character (len=512) :: line2
! this indicate which software generated the TDB file
    character*32 origin
    logical more,lastkeywasph,mcref
!
!    write(*,*)'Reading a TDB file generated by: ',trim(origin)
    nl=0
    lastkeywasph=.FALSE.
    find: do while(.TRUE.)
       nl=nl+1
       read(infile,100,end=2000,err=2099)line
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
! The first 14 are for Thermo-Calc, OC, MatCalc etc TDB files
!   character (len=kwl), dimension(nkw), parameter :: keyword=&
!        ['ELEMENT             ','SPECIES             ',&
!         'PHASE               ','CONSTITUENT         ',&
!         'FUNCTION            ','PARAMETER           ',&
!         'TYPE_DEFINITION     ','LIST_OF_REFERENCES  ',&
!         'ADD_REFERENCES      ','ASSESSED_SYSTEMS    ',&
!         'DATABASE_INFORMATION','VERSION             ',&
!         'DEFAULT_COMMAND     ','DEFINE              ']
! Pandat extra
!         'PROPERTY            ','VARIABLE            ',&
!         '                    ','                    ']
!---------------------------------------------------------------
!       write(*,103)'Keyword index: ',key,xmlerr,ip,nl,tofs,' "',line(1:20),'"'
103    format(a,5i5,3a)
       if(lastkeywasph) then
          if(key.ne.4) then
             write(*,111)nl
111          format('Please change line ',i7,' to be the CONSTITUENT keyword'/&
                  'corresponing to preceeding PHASE keyword!')
             stop
          endif
       endif
       nl1=nl
! for matcalc references
       mcref=.FALSE.; qref=0
! loop here until an exclamation found
       exclamation: do while(index(line,'!').le.0)
! data has more lines
!          write(*,*)'end of line: ',line(max(1,jp-10):jp)
! Special handling of references in MatCalc
! A new reference start with character in position 1, all following lines
! with a space in position 1 are continuation lines.  Normal references hase
! the text surrounded by ' text '.  MatCalc do not use any '
! Thus insert the references after reading MatCalc line by ' !!!
          if(tofs.eq.4 .and. line(1:15).eq.'ADD_REFERENCES ') then
!             if(qref.eq.0) write(*,*)'MatCalc references:'
             mcref=.TRUE.
          endif
! BEWHEARE list_of_references can be VERY long
77        continue
          nl=nl+1
          read(infile,100,end=2000,err=2099)line2
! debug
!          write(*,*)'Read line: ',line2(1:40),nl
          kp=1
! skip empty lines
          if(eolch(line2,kp)) goto 77
! append new line after jp after several checks ... jp is end-of-line of line
          jp=len_trim(line)
          if(jp+len(line2).gt.len(line)) then
             write(*,*)'Too long line "',line(1:40),'" at ',nline,nl
             xmlerr=5000; goto 1000
          endif
! do not insert any space at linebreak but remove most leading spaces in line2
          if(kp.gt.3) then
             kp=kp-2
          else
             kp=1
          endif
          matcalc: if(tofs.eq.4) then
! Special for matcalc
             mcrefs: if(mcref) then
! special for matcalc references
                if(kp.eq.1) then
! end of references with a single exclamantion in first position
                   if(line2(kp:kp).eq.'!') then
                      line(jp+1:jp+2)='' !'
                      exit exclamation
                   endif
! kp is position in new line2, if kp=1 this is a new reference
                   zp=1
! if qref=0 this is the first reference
!                   write(*,*)'qref: ',qref,kp
                   if(qref.gt.0) then
! mark end of previous reference, last character in line is at jp
!                      write(*,17)jp,len_trim(line)
17                    format(/' **** Terminate previous reference',2i5/)
                      line(jp+1:jp+1)="'"; jp=jp+3
                   else
! add spaces after ADD_REFERENCES
                      jp=jp+3
                      qref=qref+1
                   endif
! for new reference: add a ' after reference id and skipped spaces
                   zp=index(line2,' ')
                   if(eolch(line2,zp)) then
                      write(*,*)'Failed to extract reference id',zp
                      xmlerr=5000; goto 1000
                   endif
! insert a ' to mark the beginning of the reference text
                   qref=qref+1
                   line2(zp-1:zp-1)="'"
                   kp=1
                endif
             elseif(kp.eq.1) then
! when not reading references MatCalc needs a space when a new line added
                jp=jp+1
             endif mcrefs
          endif matcalc
! Finally adding line2 at the end of line
          line(jp+1:)=line2(kp:)
!          write(*,*)'Merging TDB file line ',nl,' with ',nl1,', length: ',jp+kp
          if(mcref) then
!             write(*,18)qref,trim(line)
18           format(/'Refs: ',i4,' "',a,'"')
          endif
       enddo exclamation
!-----------------------------------------------
       if(lastkeywasph) then
! if lastkeywasph is true then this keyword must be constituent
          if(key.ne.4) then
             write(*,*)'Keyword "PHASE" must be followed by "CONSTUENT"'
             xmlerr=5000; goto 2200
          endif
       endif
!
       if(xmlerr.ne.0) goto 2000
! this makes it possible to specify line when errors in the getxyz routines
       nline=nl
! CAPSON all commands except LIST_REFERENCES and ADD_REFERENCES
!       if(.not.(key.eq.8 .or. key.eq.9)) then
!          call capson(trim(line))
! do not call old capson with trim(line) as that does not change line !!!
       if(tofs.eq.3) then
! for Pandat files capson whole line ...
          call capson2(line,len_trim(line))
       endif
!          write(*,*)'Ut: ',trim(line)
!       endif
! debug
!       write(*,*)'Found keyword ',key,' read from line ',nline
       select case(key)
! somting on the TDB file not understood, save anyway
       case default
          write(*,110)nl,line(1:40)
110       format(' *** Ignoring line ',i7,' starting: "',a,'"')
          call getignored(line,ip)
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
!          write(*,*)'"',trim(line),'"'
          noph=noph+1
          call getphase(line,ip)
          if(xmlerr.ne.0) goto 2200
          lastkeywasph=.TRUE.
! constituents of phase
       case(4)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
! this must be the constituents for the last phase read!!
          call getconst(line,ip)
          if(xmlerr.ne.0) goto 2200
          lastkeywasph=.FALSE.
! function
       case(5)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          notp=notp+1
! third argument nonzero only for pandat property(=1) or variable(=2)
          call getfun(line,ip,0)
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
!          write(*,*)'Found add_references'
          call getbiblio(line,ip,1)
! assessed_systems
       case(10)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
          write(*,*)' *** Ignoring assessed_system'
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
! Pandat property or variable
       case(15,16)
!          write(*,*)'Keyword index: ',key,xmlerr,ip
! pandatype=1 for property, =2 for variable
          pantyp=key-14
          call getfun(line,ip,pantyp)
! not used yet
       case(17)
          write(*,*)'Keyword index unused: ',key,xmlerr,ip
! not used
       case(18)
          write(*,*)'Keyword index unused: ',key,xmlerr,ip
       end select
    enddo find
1000 continue
    return
2000 if(xmlerr.eq.0) write(*,2010)nl
     if(xmlerr.ne.0) write(*,2100)nl,xmlerr
2010 format('End of file after',i7,' lines. ',i5,' no error indicated')
    goto 1000
2099 continue
2100 format('Ending to read file at line ',i7,', due to reading error',i5)
    goto 1000
2200 write(*,2210)key,nl,xmlerr
2210 format('Error decoding keyword with index ',i2,' at line ',i7,&
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
    character line*(*),ch1*1,typedefs2*10
    integer ip,jp,kp,intval,za,zb
    double precision relval
    type(xmltdb_typedefs), pointer :: typelink
    type(xmltdb_model), pointer :: modelink
!    write(*,*)'getphase line: ',trim(line(ip:))
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    jp=ip
    call skip_to_space(line,ip)
    phlist(noph)%name=line(jp:ip-1)
! links for amend a phase for magnetism etc.
    nullify(phlist(noph)%amendphase)
    phlist(noph)%disfs_sum=0
! default configmodel and aggregation
    phlist(noph)%configmodel='CEF'
! maybe aggregation 'COMPOUND' or "C" for phases with fixed composition?
    phlist(noph)%aggregation='SOLID'
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
             phlist(noph)%aggregation='GAS'
          elseif(ch1.eq.'L') then
             phlist(noph)%aggregation='LIQUID'
          elseif(ch1.eq.'Y') then
             phlist(noph)%aggregation='LIQUID'
             phlist(noph)%configmodel='I2SL'
          elseif(ch1.eq.'B') then
             call amendphase(noph,'BCCPERM',&
                  'The parameters have BCC permutations')
          elseif(ch1.eq.'F') then
             call amendphase(noph,'FCCPERM',&
                  'The parameters have FCC permutations')
          endif
          exit fixname
       endif
    enddo fixname
    if(eolch(line,ip)) then
       write(*,*)'No data after phase name, line',nline
       xmlerr=5000; goto 1000
    endif
! store phase name in separate array to handle abbreviations
    phasenames(noph)=phlist(noph)%name
    if(xmlerr.ne.0) goto 1000
!
    jp=ip
! extract the typedefs and associate them with models or features
    call skip_to_space(line,ip)
! eventually type_defs will be redundant
    phlist(noph)%type_defs=line(jp:ip-1)
    if(ip-1-jp.gt.10) then
       write(*,*)'Too many type defs on line ',nline
       xmlerr=5007; goto 1000
    endif
    typedefs2=line(jp:ip-1)
! we should decode the type_defs as a model or action
    typelink=>type_def_list%next
! debug: list all type_defs
    kp=0
!    write(*,*)' ******** Loop to list all typedefs:'
!    do while(associated(typelink))
!       kp=kp+1
!       write(*,98)kp,typelink%id,typelink%modelindex,&
!            associated(typelink%modelink),trim(typelink%action)
!98     format('Typedef: ',i3,1x,a,' index: ',i2,' link: ',l1,&
!            ' action: "',a,'"')
!       typelink=>typelink%next
!    enddo
!----------------------------------------------
    kp=ip-jp
!    write(*,*)'Typedef 1: ',trim(typedefs2),' on line ',nline
    do jp=1,kp
       typelink=>type_def_list
!       write(*,*)'Checking type_def: ',typedefs2(jp:jp),jp
       loop: do while(associated(typelink))
          if(typedefs2(jp:jp).eq.typelink%id) then
             if(typelink%id.ne.'%') then
! here we must add link to find extra model
                if(associated(typelink%modelink)) then
!                   write(*,*)'Added model id: ',trim(typelink%modelink%id)
                   modelink=>typelink%modelink
! if this is DISFS model save the number of sublattices
                   typelink%model_int=intval
                else
                   write(*,105)typedefs2(jp:jp),nline
105                format('No model for this typedef: "',a,'" line ',i7)
                endif
             endif
! just ignore %
             exit loop
          else
             typelink=>typelink%next
          endif
       enddo loop
       if(.not.associated(typelink)) then
          if(typedefs2(jp:jp).ne.'%') then
             write(*,*)'Undefined typedef "',typedefs2(jp:jp),'" on line ',nline
             xmlerr=5008; goto 1000
          endif
       endif
    enddo
! but first we read the number of sublattices
    call getint(line,ip,intval)
    phlist(noph)%sublat=intval
!    write(*,*)'Sublattices: ',intval
! now the sites
    allocate(phlist(noph)%sites(1:intval))
    do jp=1,intval
       call getrel(line,ip,relval)
       phlist(noph)%sites(jp)=relval
    enddo
!    write(*,*)'Saved phase "',trim(phlist(noph)%name),'" ',noph
1000 continue
    return
  end subroutine getphase

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine amendphase(phno,model,text)
! set amend record for phase phno
    integer phno,ia,ib,ic,lm,ln
    character*(*) model,text
    type(xmltdb_addmodel), pointer :: newadd,saveadd
    type(xmltdb_model), pointer :: mlink
    lm=len_trim(model)
!    write(*,10)phno,model,trim(text) 
10  format('Phase ',i3,' have model: "',a,'" with info: "',a,'"')
    find: do ia=1,nomod
       if(len(mlist(ia)%id).lt.lm) cycle find
!       write(*,*)'Comparing "',trim(mlist(ia)%id),'" and "',trim(model),'"'
       if(mlist(ia)%id(1:lm).eq.model(1:lm)) then
!          write(*,*)'Found model!',ia,associated(phlist(phno)%amendphase)
! if first amendment set it 
          if(.not.associated(phlist(phno)%amendphase)) then
             allocate(phlist(phno)%amendphase)
             phlist(phno)%amendphase%id=model
             phlist(phno)%amendphase%model_info=text
             nullify(phlist(phno)%amendphase%next)
!             write(*,*)'Allocated first amend'
          else
! link a new addmodel record to this phase
             saveadd=>phlist(phno)%amendphase%next
!             write(*,*)'Trying to allocate next'
             allocate(phlist(phno)%amendphase%next)
!             write(*,*)'Allocated next'
             newadd=>phlist(phno)%amendphase%next
             newadd%id=model
             newadd%model_info=text
             newadd%next=>saveadd
!             write(*,*)'Allocated second amend'
          endif
          exit find
       endif
    enddo find
!    write(*,*)'Leaving amendphase'
1000 continue
     return
   end subroutine amendphase

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getconst(line,ip)
! extract line with phase constituent data
    character line*(*),charray*3,ch1*1
! OC max 9 sublattices, max 30 per sublattice, only gas can have more ...
    integer, parameter :: maxnc=500
    character*24 constarray(9,maxnc) 
    integer ip,jp,ll,kp,nc,nsub,ncmax
    integer nconst(9)
! terminating characters
    charray=' ,:'
! fill with spaces
    constarray=' '
! NOTE noph is used here because it is the last read phase!!!
!    write(*,'(3a,3i5)')'getconst: "',trim(line),'" ',ip,noph,nline
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
!    write(*,10)ip,trim(line)
10  format('getconst at : ',i3,' "',a,'"'/19x,'123456789.123456789.123456789.')
! we should now be at the : for the first sublattice
! constituents can be separated by "," or space (or both ...)
! sublattices are separated by ":"
! OC MQMQA special ....
    ncmax=0
    nsub=phlist(noph)%sublat
    ip=ip+1
!    write(*,*)'Phase ',noph,' line: "',trim(line(ip:)),'"'
    sub: do ll=1,nsub
       nc=0; kp=0;
       con: do while(kp.ne.3)
! ip here should be first character of the constituent, not a space
          jp=ip
          call skip_to_first(charray,kp,line,ip)
!          write(*,*)'Back from skip: ',kp,jp,ip,' const: ',line(jp:ip)
          if(kp.eq.3) then
! end of sublattice, if jp<ip and line(jp:jp) not a space there is a constituent
             if(jp.lt.ip .and. line(jp:jp).ne.' ') then
                nc=nc+1
!                write(*,*)'Adding contituent 2 "',line(jp:ip),'" in subl ',ll
                if(line(ip-1:ip-1).eq.'%') line(ip-1:ip-1)=' '
                constarray(ll,nc)=line(jp:ip-1)
! handle one or more space after :
                ip=ip+1
                if(eolch(line,ip)) then
                   write(*,*)'End of line reading constituents at ',nline
                   xmlerr=5050; goto 1000
                endif
             endif
             exit con
          else
! new constituent, note "*" is an accepted constituent meaning all or any
             if(ll.eq.1 .and. nc.eq.0) then
!                write(*,'(a,a,a,4i5)')'Line "',line(jp:ip+5),'" with ',&
!                     jp,ip,kp,nline
! Problem with Bengt's hmns file with a space after first :
                if(ip.eq.jp .and. line(jp:jp).eq.' ') then
                   ip=ip+1; cycle con
                endif
             endif
             nc=nc+1
!             write(*,*)'Adding contituent 1 "',line(jp:ip),'" in subl ',ll
             if(nc.lt.1 .or. nc.gt.maxnc) goto 2000
! remove any trailing % (indicate major constituent in TC)
! INFINISHED: this should be indicated some other way ...
             if(line(ip-1:ip-1).eq.'%') line(ip-1:ip-1)=' '
             constarray(ll,nc)=line(jp:ip-1)
!            write(*,'(a,2i3,2x,a)')'Constituent: ',ll,nc,constarray(ll,nc)(1:3)
          endif
! bypass terminating character and spaces, line can be  " : A , B , C : "
200       continue
          ip=ip+1
          if(eolch(line,ip)) then
             write(*,*)'Write EOL reading constituents'
             xmlerr=5003; goto 1000
          endif
          if(line(ip:ip).eq.':') then
! if kp=2 or 3 there are two : or a , followed by : 
! it is OK if kp=1 i.e. constituent terminated by a space
             if(kp.ne.1) then
                write(*,*)'Error in list of constiuents line: ',nline
                xmlerr=5004; goto 1000
             endif
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
             write(*,*)'Found "!" before final ":" line: ',nline
             xmlerr=5005; goto 1000
          endif
       enddo con
       nconst(ll)=nc
       if(nc.gt.ncmax) ncmax=nc
    enddo sub
!    do ll=1,nsub
!       do jp=1,nconst(ll)
!       kp=1; jp=1
!       do while(kp.gt.0)
!          kp=index(constarray(ll,jp),'%')
! UNFNISHED: handle the major constituent (before the %)
!          if(kp.gt.0) constarray(ll,jp)(kp:)=' '
!       enddo
!    enddo
! store in constituent record
!    write(*,'(a,5i5)')'getconst allocate: ',noph,nsub,ncmax,nconst(1),nconst(2)
    allocate(phlist(noph)%nconst(1:nsub))
    allocate(phlist(noph)%constituents(1:nsub,1:ncmax))
    do ll=1,nsub
       phlist(noph)%nconst(ll)=nconst(ll)
       do jp=1,nconst(ll)
          phlist(noph)%constituents(ll,jp)=constarray(ll,jp)
       enddo
    enddo
1000 continue
!    write(*,1010)'Exit getconst: ',ip,jp-1,xmlerr,'"',trim(line),'"'
1010 format(a,3i6/a,a,a)
    return
2000 write(*,2001)'Sublattice,',ll,' too few/many constituents: ',nc,maxnc
    write(*,'(3a,4i5)')'Last constituent: "',line(jp:ip-1),'" ',ll,nc,jp,ip
2001 format(a,i2,a,i3)
    xmlerr=5000
    goto 1000
  end subroutine getconst

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine gettype_def(line,ip)
! extract line with type definition
! sometimes but not always with an association with a phase ....
    character line*(*),ch1*1,phases*(64)
    type(xmltdb_typedefs), pointer :: new_type_def,save_type_def,typelink
    type(xmltdb_model), pointer :: modelink
    integer ip,jp,no,za,zb,zc,zd
!---------------------------
! these are global amend model indices
!    integer, parameter :: IHJBCC=1,IHJREST=2,DISFS=3,GLOWTEIN=4
!    integer, parameter :: FCCPERM=5,BCCPERM=6,LIQ2STATE=7
!---------------------------
    double precision relval
!    write(*,*)'In gettype_def: ',ip,' "',trim(line),'"'
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    save_type_def=>type_def_list%next
    allocate(type_def_list%next)
    new_type_def=>type_def_list%next
    new_type_def%id=line(ip:ip)
    ch1=line(ip:ip)
    new_type_def%action=line(ip+2:)
    nullify(new_type_def%modelink)
    new_type_def%modelindex=0
    new_type_def%next=>save_type_def
    typelink=>new_type_def
    new_type_def=>type_def_list
    
    notype=notype+1
! whenever possible create a link between type_def and xmltdb_model
    zb=ip
    za=index(line(ip:),' MAGNETIC')
    test1: if(za.gt.0) then
! za is position of space before MAGNETIC, skip 9 positions topick up AFF
       zb=ip+za+9; call skip_to_space(line,zb)
       call getrel(line,zb,relval)
!       write(*,*)'AFF ',relval
       if(abs(relval+1.0D0).lt.1.0D-3) then
! this is BCC magnetic model
          typelink%modelink=>mlist(IHJBCC)
          typelink%modelindex=IHJBCC
          typelink%action=' Magnetic model'
!          write(*,10)trim(typelink%action),mlist(1)%id,typelink%modelindex
10        format('Connect typedef: "',a,'" to model "',a,'" index: ',i2)
       else
! this is FCC magnetic model
!          write(*,10)trim(typelink%action),mlist(2)%id,typelink%modelindex
          typelink%modelink=>mlist(IHJREST)
          typelink%modelindex=IHJREST
          typelink%action=' Magnetic model'
       endif
       goto 900
    else
! one should allow abbreviations, start again from ip
!       write(*,*)'Not magnetic',ip
       za=index(line(ip:),' DIS_PART')
       if(za.eq.0) then
          za=index(line(ip:),' DISORDERED_PART')
! some more models after test1
          if(za.eq.0) exit test1
! zc is set to end of DIS_PART
          zc=ip+za+15
       else
          zc=ip+za+8
       endif
! This is disordered part, need ordered and disordered phase name
! Position ip+za-1 is at space before DIS, go backward to space before DIS...
       zb=ip+za-1
       call skip_back_to_space(line,zb,phases)
! zb set to length of phase name
!       write(*,*)'Ordered phase: "',phases(1:zb),'"'
!       write(*,*)'gettype_def associated? ',associated(typelink)
       typelink%modelink=>mlist(DISFS)
       typelink%modelindex=DISFS
! this will be updated when all phases read
       typelink%model_int=0
! find both ordered and disordered phase names around DIS_PARTxxx
!       write(*,*)'After DIS... "',trim(line(zc:)),'"'
       call skip_to_space(line,zc)
       if(eolch(line,zc)) then
          write(*,*)'Missing phase name after DIS_PART, line',nline
          xmlerr=5555; goto 1000
       endif
! now should zc be the beginning of disordered phase
!       write(*,*)'Disordered phase name: "',trim(line(zc:)),zc
! eliminate trailing characters from disordered phase
       za=index(line(zc:),' ')
       zd=index(line(zc:),',')-2
       phases(zb+2:)=line(zc:zc+min(za,zd))
!       write(*,*)'Ordered and disordered phases: ',trim(phases)
       typelink%action=phases
! if ordered has N, and disordered 1 sum all, if if disordered 2 then sum N-1
!       write(*,*)'Should create link to model: ',trim(line)
! !!!!!! we must later include how many sublattices to add !!!!!!!!!!!!
! at this stage the phases may not be entered
       typelink%modelindex=DISFS
!       write(*,10)trim(typelink%action),trim(mlist(DISFS)%id),&
!            typelink%modelindex
       goto 900
    endif test1
!---------------------------------------
! if we come here if we still have not found the model
! there is the Einstein model, the liquid 2-state model and some more
! The FCC and BCC permutation models are indicated with :F and :B after
! the phase name.  All quite messy ...
    za=index(line,' EINSTEIN')
    if(za.gt.0) then
! Einsten heat low T capacity model
       typelink%modelink=>mlist(GLOWTEIN)
! a space as first character supresses this text
       typelink%action=' Low T vibraional energy'
       typelink%modelindex=GLOWTEIN
       goto 900
    endif
!---------------------------------------
    za=index(line,' LIQUID 2-STATE')
    if(za.gt.0) then
! liquid 2 state model including amorphous state.  It includes EINSTEIN
       typelink%modelink=>mlist(LIQ2STATE)
       typelink%action=' Modeling liquid and amorphous as single phase'
       typelink%modelindex=LIQ2STATE
       goto 900
    endif
!--------------------------- we give up: EEC, MQMQA, UNIQUAC, VOLUME
    if(ch1.ne.'%') then
       write(*,800)ch1,trim(line)
800    format('Failed to associate type definition ',a,' with any model'/a)
    endif
!---------------------------------
! jump here when we found a model
900 continue
!
100  format(a,a,2x,a)
!    write(*,*)' >>>>> Exit gettype_def ',typelink%id,typelink%modelindex
1000 continue
    return
  end subroutine gettype_def

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getignored(line,ip)
! extract line with ignored data
    character line*(*)
    type(xmltdb_ignored), pointer :: new_type_def,save_ignored
    integer ip,jp,no
!    write(*,*)'In ignored: ',ip,' "',trim(line),'"'
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
    save_ignored=>ignored_list%next
    allocate(ignored_list%next)
    new_ignored=>ignored_list%next
    jp=ip
    call skip_to_space(line,ip)
    new_ignored%id=line(jp:ip)
    new_ignored%action=trim(line(ip+1:))
    new_ignored%next=>save_ignored
    new_ignored=>ignored_list
    notype=notype+1
! debug loop
!    do while(associated(new_ignored))
!       write(*,10)'ignored:  ',new_ignored%id,trim(new_ignored%action)
!       new_ignored=>new_ignored%next
!    enddo
10  format(a,a,2x,a)
1000 continue
    return
  end subroutine getignored

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine getparam(line,ip)
! extract line with function with possible interval
    character line*(*),charray*(5),haha*1000
    integer ip,jp,kp,zp,ix,ll,curph,nc,degree,before
    double precision relval
! temporary storage for constituent array, tnc gives in each sublattice
    character*24, dimension(15) :: constarray
    integer tnc(9),paracom
! for storing parameters using Trange
    type(xmltdb_trange), pointer :: prange
    type(xmltdb_trange), target :: trange
! keep track of number of elements, max 7
    integer elements(7)
! save model parameter id, phase, constituents etc, ip is position after PARA..
!    write(*,*)
!    write(*,*)'In getparam: "',trim(line),'" ',ip,nline
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
!          write(*,76)nline,trim(line)
76        format('Parameter in I2SL with a single sublattice line,',i7/a)
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
          write(*,77)trim(phlist(curph)%name),ll,nline
77        format('Missing constituent in sublattice for phase: "',a,'" ',2i7)
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
       if(line(ip+1:ip+1).eq.',') then
          ip=ip+2
       endif
    endif
    palist(nopa)%low_t=relval
! bibligraphic reference and possible comment with ot without @
! skip to high limit by searching for " N " or " N!"
    jp=index(line(ip:),' N ')
    if(jp.eq.0) then
       jp=index(line(ip:),' N!')
       if(jp.eq.0) then
          write(*,510)'N',nline,' Please correct'
510       format(' *** Parameter without final "',a,'" on line ',i7,a)
          xmlerr=5077; goto 1000
       endif
    endif
    kp=index(line(ip:),'!')
    if(kp.eq.0) then
! this should never happen as the final ! was found before calling getparam
       write(*,510)'!',nline,' Struggling on'
       kp=len_trim(line)
    else
       kp=ip+kp-1
    endif
! there can be several ranges ... jp is at the final N
    zp=ip+jp+2
! ensure no inital spaces in %BIBREF
    if(eolch(line,zp)) continue
! there can be a reference and comment text after final N, zp at space after N
    palist(nopa)%parcomment=' '
    paracom=zp
    call skip_to_space(line,paracom)
    if(paracom.gt.zp+1) then
!       write(*,'(3a,3i7)')'Bibref:  "',line(zp:paracom-1),'"',zp,paracom,kp
       palist(nopa)%bibref=line(zp:paracom-1)
! check if comment
       if(eolch(line,paracom)) then
          write(*,*)'PARAMETER on line ',nl,' no final "!", struggling on ...'
          kp=len_trim(line)
       endif
! kp is position of ! or end-of-line
!       write(*,'(3a,3i5)')'Comment? "',line(zp:paracom),'" ',zp,paracom,kp
       if(line(paracom:paracom).ne.'!' .and. paracom.lt.kp) then
!          write(*,*)'Found comment: "',line(paracom:kp-1),'"'
          palist(nopa)%parcomment=line(paracom:kp-1)
       endif
    else
       palist(nopa)%bibref='NONE'
    endif
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

  subroutine getfun(line,ip,pantyp)
! extract line with function with possible interval
    character line*(*)
    integer ip,jp,kp,pantyp
! one cannot mix target and pointer ??
    type(xmltdb_trange), pointer :: prange
    type(xmltdb_trange), target :: trange
    double precision relval
!    write(*,'(3a,3i5)')'In getfun: "',trim(line(ip:)),'" ',ip,notp,nline
    if(eolch(line,ip)) then
       xmlerr=5000; goto 1000
    endif
! extract ID and low T limit
    jp=ip
    call skip_to_space(line,ip)
    tplist(notp)%id=line(jp:ip-1)
    call getrel(line,ip,relval)
    tplist(notp)%low_t=relval
! pantyp nonzero only for pandat property(=1) or variable(=2)
    tplist(notp)%pandatype=pantyp
! ip is position after low T limit, extract ranges from there in getranges
! note trange is a local variable as it can not be target in declaration!!
    call getranges(line,ip,trange)
    if(xmlerr.ne.0) goto 1000
    tplist(notp)%trange=trange
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
    if(jp.gt.len(line) .or. ip.gt.len(line)) then
       write(*,507)nline,ip,jp,trim(line)
507    format('Line positions out of bounds: ',3i7/a)
       xmlerr=5007; goto 1000
    endif
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
    integer ip,jp,kp,add,nref
!    write(*,*)'Biblio not implemented yet'
!    write(*,*)'References: ',line(1:ip+10),ip
!    write(*,17)ip,add,tofs,trim(line)
17  format(/'getbiblio: ',3i7/a/)
    if(add.eq.0) then
! LIST_OF_REFERENCES NUMBER SOURCEbibref 'reference' bibref2 'referece' ...
       ip=index(line,' SOURCE')
       if(ip.le.0) then
          xmlerr=5010; goto 1000
       endif
       ip=ip+7
    elseif(tofs.ne.4) then
! ADD_REFERENCES, ip set to good value
       if(eolch(line,ip)) then
          xmlerr=5010; goto 1000
       endif
    else
! this bypass ADD_REFERENCES
       ip=15
       jp=len_trim(line)+1
       line(jp:jp+3)="' !"
    endif
! All reference in one line with no spaces in between
    nref=0
    do while(.TRUE.)
       if(eolch(line,ip)) goto 1000
       if(line(ip:ip).eq.'!') goto 1000
       jp=ip
       kp=index(line(ip:),"'")
       if(kp.le.0) then
          write(*,20)
20        format('Error extracting reference')
          xmlerr=5011; goto 1000
       endif
       bibref=line(jp:jp+kp-2)
       ip=jp+kp+1
       kp=index(line(ip:),"'")
       if(kp.le.0) then
          write(*,20)
          xmlerr=5012; goto 1000
       endif
! save biblio
       nobib=nobib+1
       bibliolist(nobib)%id=bibref
       bibliolist(nobib)%text=line(ip-1:ip+kp-2)
       nref=nref+1
!       write(*,*)'Saved reference',nref
! update position on line for next reference
       ip=ip+kp
    enddo
!
1000 continue
    return
  end subroutine getbiblio

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine check_xmltdb
! checking xmltdb file
    integer ni,nj,za,zb,zc
    type(xmltdb_addmodel), pointer :: amend
    type(xmltdb_typedefs), pointer :: typedef
    character phase1*24,phase2*24
!
    write(*,10)noel,nosp,noph,nopa,notp,notype,nomod,nompid,nobib
10  format(/'Minimal checking xmltdb file with:',&
         /i5,' elements (including maybe /- and Va)',&
         /i5,' species (including elements and maybe Va)'&
         /i5,' phases'/i5,' parameters',&
         /i5,' TP functions',&
         /i5,' type_defs',&
         /i5,' models',&
         /i5,' model parameter identifiers (mpid)'&
         /i5,' bibliographic references'/)
! check alphabetical order of species list 
!    write(*,20)(trim(splist(ni)%id),ni=1,nosp)
!    write(*,'("order: ",20i3)')(sporder(ni),ni=1,nosp)
    write(*,20)(trim(splist(sporder(ni))%id),ni=1,nosp)
20  format('Constituents: '/20(a,1x))
    write(*,30)nompid,(trim(mpidlist(ni)%id),ni=1,nompid)
30  format(/'Model parameter identifiers (MPID):',i2/a,20(', ',a))
!
! We have to check if any phase has a DISFS model it its typedef
    do ni=1,noph
       za=1
!       write(*,*)'Check typedefs for phase ',ni
       ff: do while(phlist(ni)%type_defs(za:za).ne.' ')
          typedef=>type_def_list%next
          do while(associated(typedef))
             if(typedef%id.eq.phlist(ni)%type_defs(za:za)) then
                if(typedef%modelindex.eq.DISFS) then
! check first phase is this one ....
                   zb=1
                   call skip_to_space(typedef%action,zb)
                   phase1=typedef%action(1:zb)
                   phase2=typedef%action(zb+1:)
                   if(phlist(ni)%name.ne.phase1) then
                      write(*,*)'wrong ordered phase in disordered set'
                      xmlerr=5200; goto 1000
                   endif
! search for disordered phase
                   do nj=1,noph
                      if(phlist(nj)%name.eq.phase2) then
!                         write(*,*)'Disordered sublattices: ',phlist(nj)%sublat
                         if(phlist(nj)%sublat.eq.2) then
! second sublattice assumed to be interstitial for BCC or FCC
                            phlist(ni)%disfs_sum=phlist(ni)%sublat-1
                         else
! sum over all sublattices as for sigma, mu etc.
                            phlist(ni)%disfs_sum=phlist(ni)%sublat
                         endif
!                         write(*,*)'Found disordered phase ',&
!                              phlist(ni)%sublat,phlist(ni)%disfs_sum
                         exit ff
                      endif
                   enddo
                   write(*,*)'Failed to find disordered phase',nj
                   xmlerr=5201; goto 1000
                endif
             endif
             typedef=>typedef%next
          enddo
          za=za+1
       enddo ff
    enddo
!---------------------------------------
    write(*,50)
50  format(/'Data ignored or not understood:')
    new_ignored=>ignored_list%next
    do while(associated(new_ignored))
       write(*,55)'ignored:  ',new_ignored%id,trim(new_ignored%action)
       new_ignored=>new_ignored%next
    enddo
55  format(a,a,2x,a)
1000 continue
    return
  end subroutine check_xmltdb

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

  subroutine write_xmltdb(out,tdbfile)
! writing the stored information on the xml file
    integer out,ip,jp,kp,nl,ni,tr,nnw,jsign,ll,nc,tq
    character (len=512) :: line
    character (len=800) :: clean
    character (len=24) :: dummy
    character tdbfile*(*)
    logical more
    type(xmltdb_trange), target :: trange
    type(xmltdb_trange), pointer :: prange
    type(xmltdb_typedefs), pointer :: typedef
    type(xmltdb_addmodel), pointer :: addmodel
    type(xmltdb_bibliography), pointer :: bibitem
    integer parsel,nosel,totsel,xyz
!
    nnw=7; jsign=0
!
    nl=0
!
!------------------------------------------------------
!    line='first line'
!    write(*,*)' THIS IS CRAZY '
!    write(out,'(a)')trim(line)
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
!
    line='  <Statistics elements="'; ip=len_trim(line)+1
    call wriint(line,ip,noel)
    line(ip:)='"  species="'; ip=ip+12
    call wriint(line,ip,nosp)
    line(ip:)='"  phases="'; ip=ip+11
    call wriint(line,ip,noph)
    line(ip:)='"  parameters="'; ip=ip+15
    call wriint(line,ip,nopa)
    line(ip:)='"  bibrefs="'; ip=ip+12
    call wriint(line,ip,nobib)
    line(ip:)='"  />'
    write(out,'(a)')trim(line)
    line='  <Defaults low_T="298.15" high_T="6000" />'
    write(out,'(a)')trim(line)
!--------------------------------------------------------------
! model information 
    write(out,20)
20  format(2x,'<Models>')
    mout: do ni=1,nomod
! the output of models must be coordinated with subroutine enter_models
! 1, 2 and 3 magnetic models
       if(mlist(ni)%id(1:3).eq.'IHJ') then
          if(mlist(ni)%id(4:5).eq.'QX') goto 77
! same output (but different data) for the IHJ magnetic models!!
          write(out,22)trim(mlist(ni)%id),trim(mlist(ni)%required_mpid(1)),&
               trim(mlist(ni)%required_mpid(2)),mlist(ni)%model_real,&
               trim(mlist(ni)%model_info(1)),trim(mlist(ni)%model_info(2)),&
               trim(mlist(ni)%bibref)
22        format(4x,'<Magnetic-model id="'a,'"  MPID1="',a,'" MPID2="',a,'"',&
               ' anti-ferromagnetic_factor="',F6.2,'" '/&
               6x,' f_below_TC="',a,'"'/&
               6x,' f_above_TC="',a,'" bibref="',a,'" >',&
               ' in G=f(TAO)*LN(BETA+1) where TAO=T/TC'/4x,'</Magnetic-model>')
          cycle mout
! The IHJQX magnetic model has different mpid
77        continue
          write(out,23)trim(mlist(ni)%id),trim(mlist(ni)%required_mpid(1)),&
               trim(mlist(ni)%required_mpid(2)),&
               trim(mlist(ni)%required_mpid(3)),mlist(ni)%model_real,&
               trim(mlist(ni)%model_info(1)),trim(mlist(ni)%model_info(2)),&
               trim(mlist(ni)%bibref)
23        format(4x,'<Magnetic-model id="'a,'"  MPID1="',a,'" MPID2="',a,'"',&
               ' MPID3="',a,'" anti-ferromagnetic_factor="',F6.2,'" '/&
               6x,' f_below_TC="',a,'"'/&
               6x,' f_above_TC="',a,'" bibref="',a,'" >',&
               ' in G=f(TAO)*LN(BETA+1) where TAO=T/CT or T/NT'/&
               4x,'</Magnetic-model>')
       elseif(mlist(ni)%id(1:9).eq.'GLOWTEIN ') then
! 4 Einsten model
          write(out,24)trim(mlist(ni)%id),trim(mlist(ni)%required_mpid(1)),&
               trim(mlist(ni)%bibref),trim(mlist(ni)%model_info(1))
24        format(4x,'<Einstein-model id="',a,'" MPID1="',a,'" bibref="',a,&
               '" > '/7x,a/4x,'</Einstein-model>')
       elseif(mlist(ni)%id(1:10).eq.'LIQ2STATE ') then
! 5 liquid 2state
          write(out,26)trim(mlist(ni)%id),trim(mlist(ni)%required_mpid(1)),&
               trim(mlist(ni)%required_mpid(2)),trim(mlist(ni)%bibref),&
               trim(mlist(ni)%model_info(1))
26        format(4x,'<Liquid-2state-model id="',a,'" MPID1="',a,'" ',&
               ' MPID2="',a,'" bibref="',a,'" >'/6x,a&
               /4x,'</Liquid-2state-model>')
       elseif(mlist(ni)%id(1:6).eq.'VOLOWP ') then
! 6 Volume model
          write(out,28)trim(mlist(ni)%id),trim(mlist(ni)%required_mpid(1)),&
               trim(mlist(ni)%required_mpid(2)),&
               trim(mlist(ni)%required_mpid(3)),trim(mlist(ni)%bibref),&
               trim(mlist(ni)%model_info(1))
28        format(4x,'<Volume-model id="',a,'" MPID1="',a,'" ',&
               ' MPID2="',a,'" MPID3="',a,'" bibref="',a,'" >'/6x,a&
               /4x,'</Volume-model>')
       elseif(mlist(ni)%id(1:6).eq.'DISFS ') then
! 7 Disordered fraction sets
          write(out,36)trim(mlist(ni)%id),trim(mlist(ni)%bibref),&
               trim(mlist(ni)%model_info(1))
36        format(4x,'<Disordered-fraction-model id="',a,'" bibref="',a,'" >'/&
               7x,a/4x,'</Disordered-fraction-model>')
       elseif(mlist(ni)%id(1:8).eq.'FCCPERM ') then
! 8 FCC permutations
          write(out,38)trim(mlist(ni)%id),trim(mlist(ni)%bibref),&
               trim(mlist(ni)%model_info(1))
38        format(4x,'<FCC-permutations id="',a,'" bibref="',a,'" >'/&
               6x,a/4x,'</FCC-permutations>')
       elseif(mlist(ni)%id(1:8).eq.'BCCPERM ') then
! 9 BCC permutations
          write(out,40)trim(mlist(ni)%id),trim(mlist(ni)%bibref),&
               trim(mlist(ni)%model_info(1))
40        format(4x,'<BCC-permutations id="',a,'" bibref="',a,'" >'/&
               6x,a/4x,'</BCC-permutations>')
       else
! 10 Missing: 10 EEC, 11 MQMQA, 12 UNIQUAC, 13 EBEF ??
          write(*,*)'**** Unknown model "',trim(mlist(ni)%id),'"'
       endif
    enddo mout
    write(out,62)
62  format('  </Models>')
!    
!--------------------------------------------------------------
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
!--------------------------------------------------------------
! functions
! predefined TP function R=8,31451;
! predefined TP function RT=R*T;
! predefined TP function GEIN(lntheta) =
!            1.5*exp(lntheta)+3*R*T*ln(1-exp(exp(lntheta))*T**(-1))
!
    do ni=1,notp
       ip=1
!       write(*,*)'Writing tpfun ',ni
! to have the value nicely within the "..."
       call wrinum(dummy,ip,nnw,jsign,tplist(ni)%low_t)
       if(tofs.ne.3) then
          write(out,300)trim(tplist(ni)%id),dummy(1:ip-1)
300       format('  <TPfun id="',a,'" Low_T="',a,'" >')
       else
          if(tplist(ni)%pandatype.eq.0) then
             write(out,300)trim(tplist(ni)%id),dummy(1:ip-1)
          elseif(tplist(ni)%pandatype.eq.1) then
             write(out,305)trim(tplist(ni)%id),dummy(1:ip-1)
305          format('  <TPfun id="',a,'" Low_T="',a,'"  type="Property" >')
          elseif(tplist(ni)%pandatype.eq.2) then
             write(out,306)trim(tplist(ni)%id),dummy(1:ip-1)
306          format('  <TPfun id="',a,'" Low_T="',a,'"  type="Variable" >')
          else
             write(*,*)'Unknown Pandat function type: ',tplist(ni)%pandatype
             xmlerr=5020; goto 1000
          endif
       endif
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
310       format('    <Trange High_T="',a,'" > ',a,' </Trange>')
          prange=>prange%next
!          write(*,*)'writing tranges 2: ',associated(prange)
       enddo
       write(out,390)
390    format('  </TPfun>')
    enddo
!--------------------------------------------------------------
! phases and constituents and amend phases
!   write(*,*)'Writing phases'
    do ni=1,noph
       write(out,400)trim(phlist(ni)%name),trim(phlist(ni)%configmodel),&
            phlist(ni)%aggregation(1:1)
400    format('  <Phase id="',a,'" Configurational_model="',a,&
            '" state="',a,'" >')
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
! skip first dummy typedef %
          typedef=>type_def_list
          ff: do while(associated(typedef%next))
             if(typedef%id.eq.phlist(ni)%type_defs(tq:tq)) then
                xyz=typedef%modelindex
                if(typedef%action(1:1).eq.' ') then
                   write(out,438)trim(mlist(xyz)%id)
438                format(4x,'<Amend-phase model="',a,'" />')
                elseif(xyz.eq.DISFS) then
                   write(out,439)trim(mlist(xyz)%id),trim(typedef%action),&
                        phlist(ni)%disfs_sum
439                format(4x,'<Amend-phase model="',a,'" info="',a,&
                        '" sum_sublattices="',i1,'" />')
                elseif(xyz.gt.0 .and. xyz.le.BCCPERM) then
                   write(out,440)xyz,trim(typedef%action)
                else
                   write(*,*)'Unkown amend-phase ignored for ',&
                        trim(phlist(ni)%name),xyz
                endif
                exit ff
             else
                typedef=>typedef%next
             endif
          enddo ff
          tq=tq+1
       enddo
! look if any phase has an amendphase link
       addmodel=>phlist(ni)%amendphase
       amend: do while(associated(addmodel))
!          write(*,*)'Found addmodel: "',trim(addmodel%id),'" for phase ',&
!               phlist(ni)%name
! this output has to be elaborated for different models
          write(out,440)trim(addmodel%id),trim(addmodel%model_info)
440       format(4x,'<Amend-phase model="',a,'" info="',a,'" />')
          addmodel=>addmodel%next
       enddo amend
       write(out,490)
490    format('  </Phase>')
    enddo
!
! all parameters, they should optionally be separated as unaries etc
! use sporder to list them in alphabetical order
! problematic to separate species and elements ...
!--------------------------------------------------------------
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
520       format('      <Trange High_T="',a,'" > ',a,' </Trange>')
          prange=>prange%next
       enddo
! is there any comments for this parameter?
       if(palist(ni)%parcomment(1:1).ne.' ')then
          eliminate2: do while(.TRUE.)
             ip=index(palist(ni)%parcomment,'--')
             if(ip.eq.0) exit eliminate2
             palist(ni)%parcomment(ip:ip+1)='=='
          enddo eliminate2
!          write(*,*)'XML parameter comment: ',trim(palist(ni)%parcomment)
          write(out,540)trim(palist(ni)%parcomment)
540       format('      <!-- ',a,' -->')
       endif
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
!--------------------------------------------------------------
! Bibliography
    if(nobib.eq.0) then
       write(*,*)'No bibliographic references'
    else
!       write(*,*)'The bibliography'
       write(out,900)
900    format('  <Bibliography>')
!----------------------------------------------------------------------
! We must remove any < and > and some other forbidden characters in %text
! < to &lt;       must not appear
! > to &gt;       should not appear
! & to &amp;      must not appear
! ' to &apos;     may appear
! " to &quot;     may appear
!----------------------------------------------------------------------
       do ni=1,nobib
!          call remove_xmlspecialchar(bibliolist(ni)%text,clean)
          call remove_xmlspecialchar2(bibliolist(ni)%text,clean)
          write(out,910)trim(bibliolist(ni)%id),trim(clean)
910       format('    <Bibitem ID="',a,'" Text="',a,'" />')
       enddo
! Include the model references at the end
       write(out,910)'82Her','S. Hertzman and B. Sundman, '//&
            'A Thermodynamic analysis of the Fe-Cr system, '//&
            'Calphad Vol 6 (1982) 67-80.'
       write(out,910)'12Xiong','W. Xiong, Q. Chen, P. K. Korzhavyi and '//&
            'M. Selleby, '//&
            'An improved magnetic model for thermodynamic modeling, '//&
            'Calphad, Vol 39 (2012) 11-20.'
       write(out,910)'01Qing','Q. Chen and B. Sundman, '//&
            'Modeling of thermodynamic properties for Bcc, Fcc, liquid, '//&
            'and amorphous iron, '//&
            'J. Phase Equilibria. Vol 22 (2001) 631-644.'
       write(out,910)'14Becker','C. A. Becker, J. AAgren, M. Baricco, '//&
            'Q. Chen, S. A. Decterov, U. R. Kattner, J. H. Perepezko, '//&
            'G. R. Pottlacher and M. Selleby, '//&
            'Thermodynamic modelling of liquids: CALPHAD approaches '//&
            'and contributions from statistical physics. '//&
            'Phys status solidi. B, Vol 251(1) (2014) 33-52.'
       write(out,910)'05Lu','X-G. Lu, M. Selleby and B. Sundman, '//&
            'Implementation of a new model for pressure dependence '//&
            'of condensed phases in Thermo-Calc, '//&
            'Calphad Vol 29 (2005) 49-55.'
       write(out,910)'09Sun','B. Sundman, I. Ohnuma, N. Dupin, '//&
            'U. R. Kattner, S. G. Fries, '//&
            'An assessment of the entire Al–Fe system including '//&
            'D03 ordering, '//&
            'Acta Mater. Vol 57 (2009) 2896-2908'
!       write(out,910)'
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

  subroutine remove_xmlspecialchar(inline,utline)
    character inline*(*),utline*(*),cha*1
    integer ia,ja,la
!----------------------------------------------------------------------
! We must remove any < and > and some other forbidden characters in %text
! < to &lt;       must not appear
! > to &gt;       should not appear
! & to &amp;      must not appear
! ' to &apos;     may appear
! " to &quot;     may appear
!----------------------------------------------------------------------
!    write(*,*)'text in: "',trim(inline),'"'
    la=len_trim(inline)
    ja=1
    do ia=1,la
       cha=inline(ia:ia)
       if(cha.eq.'&') then
          utline(ja:ja+4)='&amp;'
          ja=ja+5
       elseif(cha.eq.'<') then
          utline(ja:ja+3)='&lt;'
          ja=ja+4
       elseif(cha.eq.'>') then
          utline(ja:ja+3)='&gt;'
          ja=ja+4
       elseif(cha.eq."'") then
          utline(ja:ja+5)='&apos;'
          ja=ja+6
       elseif(cha.eq.'"') then
          utline(ja:ja+6)='&quot;'
          ja=ja+7
       else
          utline(ja:ja)=cha
          ja=ja+1
       endif
    enddo
    utline(ja:)=' '
!    write(*,*)'text ut: "',trim(utline),'"'
1000 continue
    return
  end subroutine remove_xmlspecialchar

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/

  subroutine remove_xmlspecialchar2(inline,utline)
    character inline*(*),utline*(*),cha*1
    integer ia,ja,la
!----------------------------------------------------------------------
! Simplified version of removing special xml characters
! We must remove any < and > and some other forbidden characters in %text
! < to &lt;       must not appear
! > to &gt;       should not appear
! & to &amp;      must not appear
! ' to &apos;     may appear
! " to &quot;     may appear
!----------------------------------------------------------------------
!    write(*,*)'text in: "',trim(inline),'"'
    la=len_trim(inline)
    ja=1
    do ia=1,la
       cha=inline(ia:ia)
       if(cha.eq.'&') then
! I do not really understand why & is forbidden but not important in references
          utline(ja:ja)=' '
          ja=ja+1
       elseif(cha.eq.'<') then
! replace < and > with [ and ] as single characters, used for example in <G> 
          utline(ja:ja)='[;'
          ja=ja+1
       elseif(cha.eq.'>') then
          utline(ja:ja)=']'
          ja=ja+1
!       elseif(cha.eq."'") then
! no need to replace single quotes
!          utline(ja:ja)="'"
!          ja=ja+1
       elseif(cha.eq.'"') then
! double quotes not good inside attributes
          utline(ja:ja)="'"
          ja=ja+1
       else
          utline(ja:ja)=cha
          ja=ja+1
       endif
    enddo
    utline(ja:)=' '
!    write(*,*)'text ut: "',trim(utline),'"'
1000 continue
    return
  end subroutine remove_xmlspecialchar2

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
   integer, parameter :: nkw=14, mkw=18
! for TC, MatCalc ...
   character (len=kwl), dimension(nkw), parameter :: keyword=&
        ['ELEMENT             ','SPECIES             ',&
         'PHASE               ','CONSTITUENT         ',&
         'FUNCTION            ','PARAMETER           ',&
         'TYPE_DEFINITION     ','LIST_OF_REFERENCES  ',&
         'ADD_REFERENCES      ','ASSESSED_SYSTEMS    ',&
         'DATABASE_INFORMATION','VERSION             ',&
         'DEFAULT_COMMAND     ','DEFINE              ']
! for Pandat
   character (len=kwl), dimension(mkw), parameter :: keyword2=&
        ['ELEMENT             ','SPECIES             ',&
         'PHASE               ','CONSTITUENT         ',&
         'FUNCTION            ','PARAMETER           ',&
         'TYPE_DEFINITION     ','LIST_OF_REFERENCES  ',&
         'ADD_REFERENCES      ','ASSESSED_SYSTEMS    ',&
         'DATABASE_INFORMATION','VERSION             ',&
         'DEFAULT_COMMAND     ','DEFINE              ',&
         'PROPERTY            ','VARIABLE            ',&
         '                    ','                    ']
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
! Thermo-Calc, OC, MatCalc
   if(tofs.ne.3) then
      do j=1,nkw
         if(word(1:kt).eq.keyword(j)(1:kt)) goto 100
      enddo
      j=0
   else
! Pandat has tofs=3
      do j=1,mkw
         if(word(1:kt).eq.keyword2(j)(1:kt)) goto 100
      enddo
      j=0
   endif
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
   mpidlist(nompid)%modix=0
1000 continue
   return
 end subroutine check_mpid

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/

 subroutine skip_back_to_space(line,ip,phases)
! extract ordered phase name backward from ip
   character line*(*),phases*(*)
   integer ip,za,zb
! first nonblank character
   za=ip
!   write(*,*)'skip_back: "',line(1:za),'"'
   loop1: do while(.TRUE.)
      if(line(za:za).ne.' ') exit loop1
      za=za-1
      if(za.le.0) goto 2000
   enddo loop1
   zb=za
   loop2: do while(.TRUE.)
      if(line(zb:zb).eq.' ') exit loop2
      zb=zb-1
      if(zb.le.0) goto 2000
   enddo loop2
   phases=line(zb+1:za)
   ip=za-zb
!   write(*,*)'Found ordered phase: ',trim(phases),ip
1000 continue
   return
! error
2000 write(*,*)'Failed to extract ordered phase name',zb,za,ip
   xmlerr=5200; goto 1000
 end subroutine skip_back_to_space

!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/

!\addtotable subroutine capson & Convert character to UPPER case
!\begin{verbatim}
  SUBROUTINE capson2(text,j)
! converts lower case ASCII a-z to upper case A-Z, no other changes
! it does not work to call this routine with trim(line) !!!
    implicit none
    integer j
    character text*(*)
!\end{verbatim}
    integer, parameter :: lowa=ichar('a'),lowz=ichar('z'),&
         iup=ICHAR('A')-ICHAR('a')
    integer i,ich1
    DO i=1,j
       ich1=ichar(text(i:i))
       IF(ich1.ge.lowa .and. ich1.le.lowz) THEN
          text(i:i)=char(ich1+iup)
       ENDIF
    ENDDO
  END SUBROUTINE capson2
  
!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!\!/!

end MODULE XMLTDB_LIB
