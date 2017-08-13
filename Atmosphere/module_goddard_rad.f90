
module mod_goddard_rad


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_goddard_rad'
 !          
 !          Purpose:
 !                         This is wrapper to WRF module_ra_goddard.
 !                         Goddard Radiative transfer which computes
 !                         radiative heating rate and surface radiation.
 !                     
 !          History:
 !                        Date: 01-08-2017
 !                        Time: 13:12 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:    Bernard Gingold
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    implicit none
    use module_kinds
    use module_ra_goddard, only : goddardrad
    
    use module_class_error_check, only : array2D_not_alloc, &
                                         array3D_not_alloc, &
                                         array4D_not_alloc
    use module_logger
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_GODDARD_RAD_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_GODDARD_RAD_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_GODDARD_RAD_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_GODDARD_RAD_FULLVER = 1000*MOD_GODDARD_RAD_MAJOR+100*MOD_GODDARD_RAD_MINOR+ &
                                                                  10*MOD_GODDARD_RAD_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_GODDARD_RAD_CREATE_DATE = "01-08-2017 13:54 AM GMT+2 (TUE 01 AUG 2017 13:54 -00200)"

    ! Module build date (should be set to latest successful build date/time)
    character(*),  parameter, public :: MOD_GODDARD_RAD_BUILD_DATE = " "
    
    ! Module author info.
    character(*),  parameter, public :: MOD_GODDARD_RAD_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module description
    character(*),  parameter, public :: MOD_GODDARD_RAD_DESCRIPT = "Wrapping module to WRF module_ra_goddard"
    
    !================================================================70
    !  Derived type:
    !                 GoddardRadWrapper_t
    !================================================================70
    
    type :: GoddardRadWrapper_t
        
        private
        
        ! Indexing member variables
        integer(I64P) :: m_ids,m_ide,m_jds,m_jde, &
                         m_kds,m_kde,             &
                         m_ims,m_ime,m_jms,m_jme, &
                         m_kms,m_kme,             &
                         m_its,m_ite,m_jts,m_jte
                        
        real(R64P), allocatable, dimension(:,:,:) :: m_rthraten, &  ! theta tendency due to radiative heating (K/sec)
                                                     m_taucldi,  &  ! ice cloud optical thickness for visible broadband
                                                     m_taucld       ! liquid cloud optical thickness for visible braodband  
!DIR$   ATTRIBUTES ALIGN : 32 :: m_rthraten
!DIR$   ATTRIBUTES ALIGN : 32 :: m_taucldi
!DIR$   ATTRIBUTES ALIGN : 32 :: m_taucld
                                                                   
        real(R64P), allocatable, dimension(:,:)   :: m_gsf 
!DIR$   ATTRIBUTES ALIGN : 32 :: m_gsf
        ! (for SW) : net short wave flux at ground surface (W/m^2) 
        ! (for LW) : downward long wave flux at ground surface (W/m^2)
        ! 
        ! Extra 3D variables (last dimension 1-TOA LW down, 2-TOA LW up, 3-surface LW down, 4-surface LW up)
        ! 5-TOA SW down, 6-TOA SW up, 7-surface SW down, 8-surface SW up)
        
        real(R64P), allocatable, dimension(:,:,:) :: m_ERBout
!DIR$   ATTRIBUTES ALIGN : 32 :: m_ERBout
        
        real(R64P), allocatable, dimension(:,:)   :: m_swddir, & ! ! All-sky broadband surface direct horizontal irradiance
                                                     m_swddni, &   ! All-sky broadband surface direct normal irradiance
                                                     m_swddif      ! All-sky braodband surface diffuse irradiance
!DIR$   ATTRIBUTES ALIGN : 32 :: m_swddir
!DIR$   ATTRIBUTES ALIGN : 32 :: m_swddni
!DIR$   ATTRIBUTES ALIGN : 32 :: m_swddif  
        
        logical(I32P) :: m_isbuilt
        
        contains
    
        !===================================================60
        !  Declaration of Type-bound procedures
        !===================================================60
        
        ! Constructor subroutine
        procedure, pass(this), public :: goddrad_wrap_init
        
        ! Destructor subroutine
        procedure, pass(this), public :: goddrad_wrap_destroy
        
        ! Subroutine invokes WRF (modified) goddardrad subroutine
        procedure, pass(this), public :: goddrad_invoke
        
        ! Get m_rthraten
        procedure, pass(this), public :: get_rthraten
        
        procedure, pass(this), public :: get_taucldi
        
        procedure, pass(this), public :: get_taucldc
        
        procedure, pass(this), public :: get_gsf
        
        procedure, pass(this), public :: get_ERBout
        
        procedure, pass(this), public :: get_swddir
        
        procedure, pass(this), public :: get_swddni
        
        procedure, pass(this), public :: get_swddif
        
        procedure, nopass,     public :: print_state
        
        procedure, nopass,     public :: write_tofile
        
    end type GoddardRadWrapper_t
    
    contains
    
    !======================================================60
    !  Implementation of type bound procedures
    !======================================================60
    
    subroutine goddrad_wrap_init(this,ids,ide,jds,jde,kds,kde, &
                                ims,ime,jms,jme,kms,kme,its,  &
                                ite,jts,jte,kts,kte,logging,  &
                                filename,append )
          use ifcore
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          class(GoddardRadWrapper_t), intent(inout) :: this
          integer(I64P),              intent(in)    :: ids,ide,jds,jde, &
                                                       kds,kde,ims,ime, &
                                                       jms,jme,kms,kme, &
                                                       its,ite,jts,jte, &
                                                       kts,kte
          logical(I32P),              intent(in)    :: logging
          character(len=*),           intent(in)    :: filename
          logical(I32P),              intent(in)    :: append
          ! Locals
          integer(I64P)      :: i,j,k,erralloc
          integer(I32P)      :: bands
          character(len=256) :: emsg
          character(len=40)  :: dstr,tstr
          ! Start of executable statements
          ! Check if object has been allocated already
          if(this%m_isbuilt .EQ. .true.) then
              if(logging .EQ. .true.) then
                   call log_startup(filename,append)
                   call log_UsrMsg("logger->godrad_wrap_init: Object is initialized already!!")
                   call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================NON-FATAL========================="
                  write(ERROR_UNIT,*) "   (godrad_wrap_init: Object is initialized already!)"
                  write(ERROR_UNIT,*) "   ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          this%m_ids=ids;this%m_ide=ide
          this%m_jds=jds;this%m_jde=jde
          this%m_kds=kds;this%m_kde=kde
          this%m_ims=ims;this%m_ime=ime
          this%m_jms=jms;this%m_jme=jme
          this%m_kms=kms;this%m_kme=kme
          this%m_its=its;this%m_ite=ite
          this%m_jts=jts;this%m_jte=jte
          this%m_kts=kte;this%m_kte=kte
          associate(m1s=>this%m_ims,m1e=>this%m_ime, &
                    m2s=>this%m_kms,m2e=>this%m_kme, &
                    m3s=>this%m_jms,m3e=>this%m_jme   )
              allocate(this%m_rthraten(m1s:m1e,m2s:m2e,m3s:m3e), &
                       this%m_taucldi(m1s:m1e,m2s:m2e,m3s:m3e),  &
                       this%m_gsf(m1s:m1e,m3s:m3e),              &
                       this%m_ERBout(m1s:m1e,m3s:m3e,1:8),       &
                       this%m_swddir(m1s:m1e,m3s:m3e),           &
                       this%m_swddni(m1s:m1e,m3s:m3e),           &
                       this%m_swddif(m1s:m1e,m3s:m3e),           &
                       STAT=erralloc,ERRMSG=emsg )
          end associate
          if(erralloc.NE.0) then
              if(logging .EQ. .true.) then
                  call  log_startup(filename,append)
                  call  log_UsrMsg("logger: In godrad_wrap_init: Array(s) allocation failure!!")
                  call  log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (godrad_wrap_init: Memory allocation failure!)"
                  write(ERROR_UNIT,*) "System message:", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "godrad_wrap_init: [FATAL]->Memory allocation failure"
          end if
          ! Begin initialzation stage
          do j = this%m_jts, this%m_jte
              do k = this%m_kts, this%m_kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                  
                  do i = this%m_its, this%m_ite
                      this%m_rthraten(i,k,j) = 0._R64P
                      this%m_taucldi(i,k,j)  = 0._R64P
                      this%m_taucld(i,k,j)   = 0._R64P
                  end do
              end do
          end do
          do j = this%m_jms, this%m_jme
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))              
              do i = this%m_ims, this%m_ime
                  this%m_gsf(i,j) = 0._R64P
              end do
          end do
          do bands = 1, 8
              do j = this%m_jms, this%m_jme
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))                     
                  do i = this%m_ims, this%m_ime
                      this%m_ERBout(i,j,bands) = 0._R64P
                  end do
              end do
          end do
          do j = this%m_jms, this%m_jme
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))               
              do i = this%m_ims, this%m_ime
                   this%m_swddir(i,j) = 0._R64P
                   this%m_swddni(i,j) = 0._R64P
                   this%m_swddif(i,j) = 0._R64P
              end do
          end do
          this%m_isbuilt = .true.
    end subroutine
                                
    subroutine goddrad_wrap_destroy(this,logging,filename,append)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          class(GoddardRadWrapper_t), intent(inout) :: this
          logical(I32P),              intent(in)    :: logging
          character(len=*),           intent(in)    :: filename
          logical(I32P),              intent(in)    :: append
          ! Locals
          integer(I32P)      :: erralloc
          character(len=256) :: emsg
          character(len=40)  :: dstr,tstr
          ! Start of executable statements
          ! Check if object has been destroyed
          if(this%m_isbuilt .EQ. .false.) then
              if(logging .EQ. .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger: godrad_wrap_destroy->Object is already destroyed!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================NON-FATAL========================="
                  write(ERROR_UNIT,*) "   (godrad_wrap_destroy: Object is already destroyed!)"
                  write(ERROR_UNIT,*) "   ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          ! Nullify indices
          this%m_ids=0_I64P;this%m_ide=0_I64P
          this%m_jds=0_I64P;this%m_jde=0_I64P
          this%m_kds=0_I64P;this%m_kde=0_I64P
          this%m_ims=0_I64P;this%m_ime=0_I64P
          this%m_jms=0_I64P;this%m_jme=0_I64P
          this%m_kms=0_I64P;this%m_kme=0_I64P
          this%m_its=0_I64P;this%m_ite=0_I64P
          this%m_jts=0_I64P;this%m_jte=0_I64P
          this%m_kts=0_I64P;this%m_kte=0_I64P
          if(allocated(this%m_rthraten)) then
              deallocate(this%m_rthraten, &
                         STAT=erralloc,   &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! " 
              end if
          end if
          if(allocated(this%m_taucldi)) then
              deallocate(this%m_taucldi, &
                         STAT=erralloc,  &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! " 
              end if
          end if
          if(allocated(this%m_taucldc)) then
              deallocate(this%m_taucldc, &
                         STAT=erralloc,  &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! " 
              end if
          end if
          if(allocated(this%m_gsf)) then
              deallocate(this%m_gsf,    &
                         STAT=erralloc, &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! " 
              end if
          end if
          if(allocated(this%m_ERBout)) then
              deallocate(this%m_ERBout, &
                         STAT=erralloc, &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! " 
              end if
          end if
          if(allocated(this%m_swddir)) then
              deallocate(this%m_swddir, &
                         STAT=erralloc, &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! " 
              end if
          end if
          if(allocated(this%m_swddni)) then
              deallocate(this%m_swddni, &
                         STAT=erralloc, &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! "
              end if
          end if
          if(allocated(this%m_swddif)) then
              deallocate(this%m_swddif, &
                         STAT=erralloc, &
                         ERRMSG=emsg)
              if(erralloc.NE.0) then
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (godrad_wrap_destroy: Memory dealocation failed) "
                  write(ERROR_UNIT,*) "   System: ", emsg
                  write(ERROR_UNIT,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "godrad_wrap_destroy: [FATAL]: Memory deallocation failed! "
              end if
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    ! Invokes Goddard drive subroutine
    subroutine goddrad_invoke(this,xlat,xlong,dz8w,t8w,rho_phy,    &
                              sw_or_lw,alb,emiss,t3d,qv3d,qc3d,    &
                              qr3d,qi3d,qs3d,qg3d,p3d,p8w3d,       &
                              pi3d,cldfra3d,gmt,cp,g,julday,xtime, &
                              declin,solcon,center_lat,radfrq,     &
                              degrad,warm_rain,f_qv,f_qc,f_qr,f_qi, &
                              f_qs,f_qg,tauaer3d_sw,ssaaer3d_sw,   &
                              asyaer3d_sw,coszen,julian,aer_opt)
          implicit none
          class(GoddardRadWrapper_t),  intent(inout) :: this
          real(R64P), dimension(this%m_ims:this%m_ime,this%m_jms:this%m_jme),  &
                                                                   intent(in) :: xlat,xlong
!DIR$     ASSUME_ALIGNED   xlat:32,xlong:32        
          real(R64P), dimension(this%m_ims:this%m_ime,this%m_kms:this%m_kme,this%m_jms:this%m_jme), &
                                                                    intent(in) :: dz8w,t8w,rho_phy
!DIR$     ASSUME_ALIGNED   dz8w:32,t8w:32,rho_phy:32          
          character(len=2),            intent(in) :: sw_or_lw
          real(R64P), dimension(this%m_ims:this%m_ime,this%m_jms:this%m_jme), &
                                                                    intent(in) :: alb,emiss
!DIR$     ASSUME_ALIGNED   alb:32,emiss:32          
          real(R64P), dimension(this%m_ims:this%m_ime,this%m_kms:this%m_kme,this%m_jms:this%m_jme), &
                                                                    intent(in), optional :: t3d,qv3d,qc3d,qr3d, &
                                                                                            qi3d,qs3d,qg3d,p3d, &
                                                                                            p8w3d,pi3d,cldfra3d
!DIR$    ASSUME_ALIGNED    t3d:32,qv3d:32,qc3d:32,qr3d:32,qi3d:32,qs3d:32,qg3d:32,p3d:32
!DIR$    ASSUME_ALIGNED    p8w3d:32,pi3d:3d,cldfra3d:32
          integer(I32P),               intent(in) :: julday
          real(R64P),                  intent(in) :: gmt,cp,g,xtime,declin,solcon,center_lat,radfrq,degrad
          logical,                     intent(in) :: warm_rain
          logical,                     intent(in), optional :: f_qv,f_qc,f_qr,f_qi,f_qs,f_qg
          real(R64P), dimension(this%m_ims:this%m_ime,this%m_kms:this%m_kme,this%m_jms:this%m_jme), &
                                                                     intent(in), optional :: tauaer3d_sw, &
                                                                                             ssaaer3d_sw, &
                                                                                             asyaer3d_sw
!DIR$    ASSUME_ALIGNED   tauaer3d_sw:32,ssaaer3d_sw:32,asyaer3d_sw:32          
          real(R64P), dimension(this%m_ims:this%m_ime,this%m_jms:this%m_jme), &
                                                                     intent(in), optional :: coszen
!DIR$    ASSUME_ALIGNED   coszen:32          
          real(R64P),                  intent(in), optional :: julian
          integer(I32P),               intent(in)           :: aer_opt
          ! Start executable statements
          
          ! Call subroutine goddardrad
          
          call goddardrad(this%m_rthraten,this%m_gsf,xlat,xlong,                  &
                          dz8w,t8w,rho_phy,sw_or_lw,                              &
                          alb,emiss,t3d,qv3d,qc3d,qr3d,                           &
                          qi3d,qs3d,qg3d,                                         &
                          p3d,p8w3d,pi3d,cldfra3d,                                &
                          gmt,cp,g,julday,xtime,declin,solcon,                    &
                          center_lat,radfrq,degrad,this%m_taucldi,this%m_taucldc, &
                          warm_rain,f_qv,f_qc,f_qr,f_qi,f_qs,f_qh,                &
                          this%m_ids,this%m_ide,this%m_jds,this%m_jde,            &
                          this%m_kds,this%m_kde,this%m_ims,this%m_ime,            &
                          this%m_jms,this%m_jme,this%m_kms,this%m_kme,            &
                          this%m_its,this%m_ite,this%m_jts,this%m_jte,            &
                          this%m_kts,this%m_kte,this%m_ERBout,                    &
                          tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw,                    &
                          this%m_swddir,this%m_swddni,this%m_swddif,              &
                          coszen,julian,aer_opt                                   )
          
    end subroutine
                              
    pure function get_rthraten(this) result(rthraten)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: rthraten
!DIR$     ATTRIBUTES ALIGN : 32 :: rthraten
          rthraten = this%m_rthraten
    end function
    
    pure function get_taucldi(this) result(taucldi)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: taucldi
!DIR$     ATTRIBUTES ALIGN : 32 :: taucldi
          taucldi = this%m_taucldi
    end function
    
    pure function get_taucld(this) result(taucld)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: taucld
!DIR$     ATTRIBUTES ALIGN : 32 :: taucld
          taucld = this%m_taucld
    end function
    
    pure function get_gsf(this) result(gsf)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: gsf
!DIR$     ATTRIBUTES ALIGN : 32 :: gsf
          gsf = this%m_gsf
    end function
    
    pure function get_ERBout(this) result(ERBout)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: ERBout
!DIR$     ATTRIBUTES ALIGN : 32 :: ERBout
          ERBout = this%m_ERBout
    end function
    
    pure function get_swddir(this) result(swddir)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: swddir
!DIR$     ATTRIBUTES ALIGN : 32 :: swddir
          swddir = this%m_swddir
    end function
    
    pure function get_swddni(this) result(swddni)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: swddni
!DIR$     ATTRIBUTES ALIGN : 32 :: swddni
          swddni = this%m_swddni
    end function
    
    pure function get_swddif(this) result(swddif)
          implicit none
          class(GoddardRadWrapper_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: swddif
!DIR$     ATTRIBUTES ALIGN : 32 :: swddif
          swddif = this%m_swddif
    end function
    
end module mod_goddard_rad