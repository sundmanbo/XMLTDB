program xmlupload
!
! draft XML upload program
!
  use xmltdb_lib
!
  implicit none
!
  integer, dimension(8) :: maxdim
  integer lina,luta,last,ip,jp,kp
  character (len=256) :: file,tdbfile
  character (len=128) :: cline
  character text*12,origin*32,outfile*64
  type(xmltdb_typedefs), pointer :: type_def
!
! 8 max for elements, species, phases, parameters, tpfun, models,
!           model-parameter-id, biblio
  maxdim(1)=100; maxdim(2)=1000; maxdim(3)=500; maxdim(4)=5000
  maxdim(5)=1000;  maxdim(6)=50; maxdim(7)=50; maxdim(8)=2000
!
  write(*,5)
5 format(//10x,'Program to convert TDB files to XTDB format version 0.0.1'//)
!  
  xmlerr=0
  call init_xmltdb(maxdim)
  if(xmlerr.ne.0) goto 2000
! a blank line
  write(*,*)
  cline=' '
  last=1
! without ftinyfiles
!  call gparcdx('TDB file name: ',cline,last,1,tdbfile,text,'?Read TDB')
!  read(*,100)file
100 format(a)
!
! The "1" in second last argument indicate to ftinyopen it is a TDB file
! default extension (1=TDB, 2=OCU, 3=OCM, 4=OCD, 5=PLT, 6=PDB, 7=DAT, 8=LOG
! negative is for write, 0 read without filter, -100 write without filter
! The last argument is a possible hyperlink to a HTML file for help
  call gparfilex('TDB file name: ',cline,last,1,tdbfile,' ',1,'?Read TDB')
!
  write(*,*)'Specify if origin is MatCalc or Pandat'
  origin='Thermo-Calc '
  tofs=1
  text='dummy'
! the second to last argument is a proposed default answer
  call gparcdx('Software used to generate TDB: ',cline,last,1,text,origin,&
       '?Read TDB')
!
  if(text.ne.origin) then
     if(text.eq.software(4)) then
        write(*,*)'Using MatCalc specials'
        tofs=4
     elseif(text.eq.software(3)) then
        write(*,*)'Using Pandat specials'
        tofs=3
     else
        write(*,107)text,software
107     format('Unknown software "',a,'", allowed TDB formats are:',//4a/4a)
        stop
     endif
  else
     write(*,*)'Assuming Thermo-Calc compatible TDB file'
  endif
!
  lina=21
  write(*,*)'Reading TDB file: ',trim(tdbfile)
  open(lina,file=tdbfile,access='sequential',status='old',err=2000)
!  
  call read_tdbfile(lina,origin)
  if(xmlerr.ne.0) goto 2100
!  write(*,'(/a,3i4)')'Number of elements, functions, phases: ',noel,notp,noph
!  write(*,'(a,3i4)')'Number of parameters and mpid: ',nopa,nompid
!  write(*,'(a,3i4/)')'Number of species, bibliographic refs: ',nosp,nobib
!
! obsolete debug output
! list functions
!  do jp=1,notp
!     write(*,150)trim(tplist(jp)%id),tplist(jp)%low_t
!     write(*,152)trim(tplist(jp)%expression),tplist(jp)%high_t
150  format('TP function: ',a,1pe14.6)
152  format(5x,a,1pe14.6)
!  enddo
! list phases
!  do ip=1,noph
!     write(*,200)trim(phlist(ip)%name),trim(phlist(noph)%type_defs),&
!          phlist(noph)%sublat,&
!          (phlist(noph)%sites(jp),jp=1,phlist(noph)%sublat)
200  format('Phase: ',a,2x,a,2x,i3,2x,9(F10.6))
!     do jp=1,phlist(noph)%sublat
!        write(*,202)jp,(trim(phlist(noph)%constituents(jp,kp)),&
!             kp=1,phlist(noph)%nconst(jp))
202     format('Constituents in: ',i2,2x,10(a,2x))
!     enddo
!  enddo
!  
  close(lina)
!
! perform some checks
  call check_xmltdb
! write the xmltdb file
  luta=21
  outfile='XMLTDB-file.XTDB'
!  open(luta,file=outfile,access='sequential',status='new',err=2200)
  open(luta,file=outfile,access='sequential',status='unknown',err=2200)
!  
  write(*,30)trim(outfile)
30 format(/'Writing output on file: ',a)
  call write_xmltdb(luta,tdbfile,software(tofs))
  if(xmlerr.ne.0) goto 2300
  close(luta)
!
! list all type defs
!  write(*,800)
!800 format(/'In the TDB fie the following TYPE_DEFINITIONS has been found:')
!  type_def=>type_def_list
!  do while(associated(type_def))
!     write(*,802)type_def%id,trim(type_def%action)
!802  format('TYPE_DEFINITION letter: ',a/&
!          'Action: ',a)
!     type_def=>type_def%next
!  enddo
!
  write(*,999)
999 format(/'All done'/)
  goto 3000
! error open
2000 write(*,*)'Error opening TDB file for reading'
  goto 3000
! error open
2100 write(*,*)'Error reading TDB file'
  goto 3000
! error open
2200 write(*,*)'Error opening XMLTDB file for writing'
  goto 3000
! error open
2300 write(*,*)'Error writing XMLTDB file'
  goto 3000
!
3000 continue
!
end program xmlupload
