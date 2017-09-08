
module mod_atmos_pressure3d


    
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_atmos_pressure3d'
 !          
 !          Purpose:
 !                   Atmospheric scalar pressure field 3D
 !                   
 !                     
 !          History:
 !                        Date: 03-09-2017
 !                        Time: 10:15 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
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
    private ! everything is private beside procedures deemed public
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT, only : TRACEBACKQQ
    use module_class_error_check, only : array3D_not_alloc, &
                                         conform_dim3D_Real
    use module_kinds
    use mod_constants
    use module_logger
    use mod_code_timing
    
    public ::  assignment (=)
    public ::  operator   (+)
    public ::  operator   (-)
    public ::  operator   (*)
    public ::  operator   (/)
    public ::  operator   (**)
    public ::  operator   (/=)
    public ::  operator   (==)
    public ::  operator   (>)
    public ::  operator   (<)
    public ::  operator   (>=)
    public ::  operator   (<=)
    public ::  operator   (.grad.)
    public ::  operator   (.laplacian.)
                
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_ATMOS_PRESSURE3D_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_ATMOS_PRESSURE3D_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_ATMOS_PRESSURE3D_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_ATMOS_PRESSURE3D_FULLVER = 1000*MOD_ATMOS_PRESSURE3D_MAJOR+100*MOD_ATMOS_PRESSURE3D_MINOR+ &            
                                                                       10*MOD_ATMOS_PRESSURE3D_MINOR
    
    ! Module/file creation date/time
    character(*),  parameter, public :: MOD_ATMOS_PRESSURE3D_CREATE_DATE = "03-09-2017 10:26 +00200 (SUN 03 SEP 2017 GMT+2)"
    
    ! Module build date/time (should be set after successful build date)
    character(*),  parameter, public :: MOD_ATMOS_PRESSURE3D_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_ATMOS_PRESSURE3D_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_ATMOS_PRESSURE3D_DESCRIPT = "Atmospheric scalar pressure field 3D."
    
    !======================================================60
    !  Type: AtmPressure3D
    !  Warning: member access is public by default.
    !  Array indices are compatible with WRF model
    !======================================================60
    
    type, public :: AtmPressure3D
        
          ! Public by default
          integer(I64P) :: m_ims,m_ime,m_kms,m_kme, &
                           m_jms,m_jme,m_ids,m_ide, &
                           m_kds,m_kde,m_jds,m_jde, &
                           m_its,m_ite,m_kts,m_kte, &
                           m_jts,m_jte
          
          character(len=64) :: m_rand
          
!DIR$     ATTRIBUTES ALIGN : 32 :: m_p3d
          real(R64P), allocatable, dimension(:,:,:) :: m_p3d
          
!DIR$     ATTRIBUTES ALIGN : 32 :: m_p8w3d
          real(R64P), allocatable, dimension(:,:,:) :: m_p8w3d
          
          ! Build indicator
          logical(I32P) :: m_isbuilt
          
          contains
    
          !===========================================55
          ! Constructors and Destructor procedures.
          !===========================================55
    
          procedure, pass(this), public :: default_init
          
          procedure, pass(this), public :: init
          
          procedure, pass(this), public :: copy
          
          procedure, pass(this), public :: destroy
          
         !============================================55
         ! Read/Write procedures
         !============================================55
          procedure, nopass, public :: read_press3d
          
          procedure, nopass, public :: write_press3d
          
    end type AtmPressure3D
          
         !=================================================60
         !  Module operators
         !=================================================60
          
         
          
         interface assignment (=)
             module procedure assign_press3D
         end interface
         
         interface operator   (+)
             module procedure press3D_add_press3D
         end interface
         
         interface operator   (-)
             module procedure press3D_sub_press3D
         end interface
         
         interface operator   (*)
             module procedure press3D_mul_press3D
             module procedure press3D_mul_scalar
             module procedure scalar_mul_press3D
         end interface
         
         interface operator   (/)
             module procedure press3D_div_press3D
             module procedure press3D_div_scalar
         end interface
         
         interface operator   (**)
             module procedure press3D_exponent
         end interface
         
         interface operator   (/=)
             module procedure press3D_neq_press3D
         end interface
         
         interface operator   (==)
             module procedure press3D_eq_press3D
         end interface
         
         interface operator   (>)
             module procedure press3D_gt_press3D
         end interface
         
         interface operator   (<)
             module procedure press3D_lt_press3D
         end interface
         
         interface operator   (>=)
             module procedure press3D_ge_press3D
         end interface
         
         interface operator   (<=)
             module procedure press3D_le_press3D
         end interface
         ! grad(f) -> vector field
         interface operator   (.grad.)
             module procedure press3D_gradient
         end interface
         ! div(grad(f)) 
         interface operator   (.laplacian.)
             module procedure press3D_laplacian
         end interface
         
    contains
          
         !=================================================60
         !              Implementation
         !=================================================60
         
    !=================================================60
    !  subroutine: default_init
    !  Default initialization to +INF values.
    !=================================================60
    subroutine default_init(this,indices,logging, &
                                 filename,append,dbg )
          implicit none
          class(AtmPressure3D),         intent(inout) :: this
          integer(I64P), dimension(18), intent(in)    :: indices
          logical(I32P),                intent(in)    :: logging
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          integer(I64P)      :: j,k,i
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:216, In->mod_atmos_pressure3d/default_init: AtmPressure3D already initialized!)
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_atmos_pressure3d/default_init:216,AtmPressure3D already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          ! Begin construction
          this%m_ims=indices(1);this%m_ime=indices(2)
          this%m_kms=indices(3);this%m_kme=indices(4)
          this%m_jms=indices(5);this%m_jme=indices(6)
          this%m_ids=indices(7);this%m_ide=indices(8)
          this%m_kds=indices(9);this%m_kde=indices(10)
          this%m_jds=indices(11);this%m_jde=indices(12)
          this%m_its=indices(13);this%m_ite=indices(14)
          this%m_kts=indices(15);this%m_kte=indices(16)
          this%m_jts=indices(17);this%m_jte=indices(18)
          this%m_rand = "default-none"
          ! Arrays
          associate(d1s=>this%m_ims,d1e=>this%m_ime, &
                    d2s=>this%m_kms,d2e=>this%m_kme, &
                    d3s=>this%m_jms,d3e=>this%m_jme  )
               allocate(this%m_p3d(d1s:d1e,d2s:d2e,d3s:d3e),   &
                        this%m_p8w3d(d1s:d1e,d2s:d2e,d3s:d3e), &
                        STAT=aerr,ERRMSG=emsg)
         end associate
         if(aerr.NE.0) then
             if(logging) then
                 call log_startup(filename,append)
                 call log_UsrMsg("logger:253, In->mod_atmos_pressure3d/default_init: Memory allocation failure!!")
                 call log_shutdown()
             else
                 call DATE_AND_TIME(date=dstr,time=tstr)
                 write(ERROR_UNIT,*) "===========================FATAL========================="
                 write(ERROR_UNIT,*) "   (mod_atmos_pressure3d/default_init:253, Memory allocation failure!)"
                 write(ERROR_UNIT,*) "   (System message:)", emsg
                 write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                 write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_atmos_pressure3d/default_init:253 -> [FATAL-ERROR]: Terminating execution!!"
         end if
         ! Arrays iniriliazation
         do j = this%m_jts, this%m_jte
             do k = this%m_kts, this%m_kte
!DIR$            SIMD VECTORLENGTHFOR(REAL(KIND=8))                 
                 do i = this%m_its, this%m_ite
                     this%m_p3d(i,k,j)   = LAM_PINF
                     this%m_p8w3d(i,k,j) = LAM_PINF
                 end do
             end do
         end do
         this%m_isbuilt = .true.
         if(dbg .EQ. .true.) then
             print*, "Pressure scalar field 3d:", this%m_p3d
             print*, "Pressure at full levels: ", this%m_p8w3d
         end if
    end subroutine
                                 
    !======================================================60
    ! subroutine: init
    ! Initialziation by user(caller) passed arrays which
    ! usually contain specific random scalar distribution.
    !======================================================60  
    subroutine init(this,indices,dr,p3d,p8w3d,logging,  &
                    profiling,filename,append,dbg,qpctimer     )
          implicit none
          class(AtmPressure3D),         intent(inout) :: this
          integer(I64P), dimension(18), intent(in)    :: indices
          character(len=*),             intent(in)    :: dr
          real(R64P), dimension(:,:,:), intent(in)    :: p3d,p8w3d
!DIR$     ASSUME_ALIGNED p3d:32,p8w3d:32
          logical(I32P),                intent(in)    :: logging,profiling
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          type(QPCTimer_t),             intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: ermsg
          integer(I32P)      :: aerr
          integer(I64P)      :: j,k,i
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:317, In->mod_atmos_pressure3d/init: AtmPressure3D already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_atmos_pressure3d/init:317, AtmPressure3D already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(array3D_not_alloc(p3d)   .OR.  &
             array3D_not_alloc(p8w3d) ) then
              if(logging) then
                   call log_startup(filename,append)
                   call log_UsrMsg("logger:333, In->mod_atmos_pressure3d/init: Array arguments not allocated!!")
                   call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_atmos_pressure3d/init:333, Array arguments not allocated!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          ! Begin construction
          this%m_ims=indices(1); this%m_ime=indices(2)
          this%m_kms=indices(3); this%m_kme=indices(4)
          this%m_jms=indices(5); this%m_jme=indices(6)
          this%m_ids=indices(7); this%m_ide=indices(8)
          this%m_kds=indices(9); this%m_kde=indices(10)
          this%m_jds=indices(11);this%m_jde=indices(12)
          this%m_its=indices(13);this%m_ite=indices(14)
          this%m_kts=indices(15);this%m_kte=indices(16)
          this%m_jts=indices(17);this%m_jte=indices(18)
          this%m_drand = dr
          ! Allocate arrays
          associate(d1s=>this%m_ims,d1e=>this%m_ime, &
                    d2s=>this%m_kms,d2e=>this%m_kme, &
                    d3s=>this%m_jms,d3e=>this%m_jme)
              allocate(this%m_p3d(d1s:d1e,d2s:d2e,d3s:d3e),   &
                       this%m_p8w3d(d1s:d1e,d2s:d2e,d3s:d3e), &
                       STAT=aerr, ERRMSG=emsg)
         end associate
         if(aerr.NE.0) then
             if(logging) then
                 call log_startup(filename,append)
                 call log_UsrMsg("logger:369, In->mod_atmos_pressure3d/init: Memory allocation failure!!")
                 call log_shutdown()
             else
                 call DATE_AND_TIME(date=dstr,time=tstr)
                 write(ERROR_UNIT,*) "===========================FATAL========================="
                 write(ERROR_UNIT,*) "   (mod_atmos_pressure3d/init:369, Memory allocation failure!)"
                 write(ERROR_UNIT,*) "   (System message:)", emsg
                 write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                 write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_atmos_pressure3d/init:369 -> [FATAL-ERROR]: Terminating execution!!"
         end if 
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_atmos_pressure3d/init: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
         do j = this%m_jts, this%m_jte
             do k = this%m_kts, this%m_kte
               
!DIR$            SIMD VECTORLENGTHFOR(REAL(KIND=8))                 
                 do i = this%m_its, this%m_ite
                     this%m_p3d(i,k,j)   = p3d(i,k,j)
                     this%m_p8w3d(i,k,j) = p8w3d(i,k,j)
!DIR$            IF (USE_SOFT_PREFETCH .EQ. 1)                     
                     call MM_PREFETCH(p3d(i+2,k,j),1)
                     call MM_PREFETCH(p8w3d(i+2,k,j),1)
!DIR$            ENDIF
                 end do
             end do
         end do
         if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_atmos_pressure3d/init: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_atmos_pressure3d/init: Unable to read performance counter -- fatal!!"
                end if
         end if
         if(dbg) then
             print*, "Pressure scalar field 3d:", this%m_p3d
             print*, "Pressure at full levels: ", this%m_p8w3d
         end if
    end subroutine
                    
    !======================================================60 
    ! subroutine: copy                            
    !======================================================60  
    subroutine copy(this,other,logging,filename,append,ierr)
          implicit none
          class(AtmPressure3D), intent(inout) :: this
          class(AtmPressure3D), intent(in)    :: other
          logical(I32P),        intent(in)    :: logging
          character(len=*),     intent(in)    :: filename
          logical(I32P),        intent(in)    :: append
          integer(I32P),        intent(inout) :: ierr
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(LOC(this).EQ.LOC(other) .OR. &
             this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:436, In->mod_atmos_pressure3d/copy: Attempted self-assignment or AtmPressure3D already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_atmos_pressure3d/copy:436, Attempted self_assignment or AtmPressure3D already initiliazed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          ! Begin construction
          this%m_ims=other%m_ims
          this%m_ime=other%m_ime
          this%m_kms=other%m_kms
          this%m_kme=other%m_kme
          this%m_jms=other%m_jms
          this%m_jme=other%m_jme
          this%m_ids=other%m_ids
          this%m_ide=other%m_ide
          this%m_kds=other%m_kds
          this%m_kde=other%m_kde
          this%m_jds=other%m_jds
          this%m_jde=other%m_jde
          this%m_its=other%m_its
          this%m_ite=other%m_ite
          this%m_kts=other%m_kts
          this%m_kte=other%m_kte
          this%m_jts=other%m_jts
          this%m_jte=other%m_jte
          this%m_rand=other%m_rand
          this%m_p3d=other%m_p3d
          this%m_p8w3d=other%m_p8w3d
          this%m_isbuilt= .true.
    end subroutine
    
    !======================================================60
    !  subroutine: destroy
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'ierr' will be set to -1.
    !======================================================60
    subroutine destroy(this,logging,filename,append,ierr)
          implicit none
          class(AtmPressure3D), intent(inout) :: this
          logical(I32P),        intent(in)    :: logging
          character(len=*),     intent(in)    :: filename
          logical(I32P),        intent(in)    :: append
          integer(I32P),        intent(inout) :: ierr
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:491, In->mod_atmos_pressure3d/destroy: AtmPressure3D already destroyed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_atmos_pressure3d/copy:436, Attempted self_assignment or AtmPressure3D already initiliazed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          this%m_ims=0;this%m_ime=0
          this%m_kms=0;this%m_kme=0
          this%m_jms=0;this%m_jme=0
          this%m_ids=0;this%m_ide=0
          this%m_kds=0;this%m_kde=0
          this%m_jds=0;this%m_jde=0
          this%m_its=0;this%m_ite=0
          this%m_kts=0;this%m_kte=0
          this%m_jts=0;this%m_jte=0
          this%m_rand = " "
          if(ALLOCATED(this%m_p3d)) then
              deallocate(this%m_p3d,  &
                         STAT=derr,   &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:527, In->mod_atmos_pressure3d: Failed deallocate: [m_p3d]!!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(ERROR_UNIT,*) "===========================FATAL========================="
                      write(ERROR_UNIT,*) "   (mod_atmos_pressure3d/destroy:527, Failed deallocate: [m_p3d]!!)"
                      write(ERROR_UNIT,*) "   (System message:)", emsg
                      write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_atmos_pressure3d/destroy:527 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          if(ALLOCATED(this%m_p8w3d)) then
              deallocate(this%m_p8w3d, &
                         STAT=derr,    &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:552, In->mod_atmos_pressure3d: Failed deallocate: [m_p8w3d]!!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(ERROR_UNIT,*) "===========================FATAL========================="
                      write(ERROR_UNIT,*) "   (mod_atmos_pressure3d/destroy:552, Failed deallocate: [m_p8w3d]!!)"
                      write(ERROR_UNIT,*) "   (System message:)", emsg
                      write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_atmos_pressure3d/destroy:552 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          this%m_isbuilt = .true.
    end subroutine
    
    !======================================================60
    !  Read/write subroutines
    !======================================================60
    subroutine read_press3D(this,logging,filename,append, &
                            unit, ioerr,err)
          implicit none
          class(AtmPressure3D), intent(in)    :: this
          logical(I32P),        intent(in)    :: logging
          character(len=*),     intent(in)    :: filename
          logical(I32P),        intent(in)    :: append
          integer,              intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr,err
          ! Locals
          character(len=40) :: dstr,tstr
          
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:594, In->mod_atmos_pressure3D/read_press3D: AtmPressure3D in invalid state!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_atmos_pressure3d/read_press3D:594, AtmPressure3D in invalid state!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          READ(unit,iostat=ioerr) this
    end subroutine
    
    subroutine write_press3D(this,logging,filename,append,&
                             unit,ioerr,err )
          implicit none
          class(AtmPressure3D), intent(in)    :: this
          logical(I32P),        intent(in)    :: logging
          character(len=*),     intent(in)    :: filename
          logical(I32P),        intent(in)    :: append
          integer,              intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr,err
          ! Locals
          character(len=40) :: dstr,tstr
          
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:625, In->mod_atmos_pressure3D/write_press3D: AtmPressure3D in invalid state!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_atmos_pressure3d/write_press3D, AtmPressure3D in invalid state!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          WRITE(unit,iostat=ioerr) this
    end subroutine
    
    !======================================================60
    !    Module operators
    !======================================================60
    
    !=======================================================60
    !  subroutine: overloaded assignment (=)
    !=======================================================60
    subroutine assign_press3D(this,other)
          implicit none
          type(AtmPressure3D), intent(inout) :: this
          type(AtmPressure3D), intent(in)    :: other
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
              call DATE_AND_TIME(date=dstr,time=tstr)
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_atmos_pressure3d/assignment(=):594, Attempted self_assignment!)"
               write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
               write(stderr,*) "===========================NON-FATAL=========================="
          end if
          this%m_ims=other%m_ims
          this%m_ime=other%m_ime
          this%m_kms=other%m_kms
          this%m_kme=other%m_kme
          this%m_jms=other%m_jms
          this%m_jme=other%m_jme
          this%m_ids=other%m_ids
          this%m_ide=other%m_ide
          this%m_kds=other%m_kds
          this%m_kde=other%m_kde
          this%m_jds=other%m_jds
          this%m_jde=other%m_jde
          this%m_its=other%m_its
          this%m_ite=other%m_ite
          this%m_kts=other%m_kts
          this%m_kte=other%m_kte
          this%m_jts=other%m_jts
          this%m_jte=other%m_jte
          this%m_rand=other%m_rand
          this%m_p3d=other%m_p3d
          this%m_p8w3d=other%m_p8w3d
          this%m_isbuilt=other%m_isbuilt
    end subroutine
    
    !======================================================60
    ! function: press3D_add_press3D i.e. overloaded
    !           operator (+)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_add_press3D(this,other) result(obj)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in)    :: other
          ! Locals/return
          type(AtmPressure3D) :: obj
          ! Start of executable statments
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d   = this%m_p3d + other%m_p3d
          obj%m_p8w3d = this%m_p8w3d + other%m_p8w3d
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: press3D_sub_press3D i.e. overloaded
    !           operator (-)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_sub_press3D(this,other) result(obj)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals/return
          type(AtmPressure3D) :: obj
          ! Start of executable statements
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d   = this%m_p3d   - other%m_p3d
          obj%m_p8w3d = this%m_p8w3d - other%m_p8w3d
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: press3D_mul_press3D i.e. overloaded
    !           operator (*)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_mul_press3D(this,other) result(obj)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals/return
          type(AtmPressure3D) :: obj
          ! Start of executable sstatements
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d   = this%m_p3d   * other%m_p3d
          obj%m_p8w3d = this%m_p8w3d * other%m_p8w3d
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: press3D_mul_scalar i.e. overloaded
    !           operator (*)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_mul_scalar(this,scalar) result(obj)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          real(R64P),          intent(in) :: scalar
          ! Locals
          type(AtmPressure3D) :: obj
          ! Start of executable statements
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d   = this%m_p3d   * scalar
          obj%m_p8w3d = this%m_p8w3d * scalar
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: scalar_mul_press3D i.e. overloaded
    !           operator (*)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function scalar_mul_press3D(scalar,this) result(obj)
          implicit none
          real(R64P),          intent(in) :: scalar
          type(AtmPressure3D), intent(in) :: this
          ! Locals/return
          type(AtmPressure3D) :: obj
          ! Start of executable statements
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d   = scalar * this%m_p3d
          obj%m_p8w3d = scalar * this%m_p8w3d
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: press3D_div_press3D i.e. overloaded
    !           operator (/)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_div_press3D(this,other) result(obj)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals
          type(AtmPressure3D) :: obj
          ! Start of executable statements
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d = this%m_p3d   / other%m_p3d
          obj%m_p8w3d = this%m_p3d / other%m_p3d
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: press3D_div_scalar i.e. overloaded
    !           operator (/)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_div_scalar(this,scalar) result(obj)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          real(R64P),          intent(in) :: scalar
          ! Locals
          type(AtmPressure3D) :: obj
          ! Start of executable statements
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d   = this%m_p3d   / scalar
          obj%m_p8w3d = this%m_p8w3d / scalar
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: press3D_exponent i.e. overloaded
    !           operator (**)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_exponent(this,ex) result(obj)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          real(R64P),          intent(in) :: ex
          ! Locals
          type(AtmPressure3D) :: obj
          ! Start of executable statements
          obj%m_ims=this%m_ims
          obj%m_ime=this%m_ime
          obj%m_kms=this%m_kms
          obj%m_kme=this%m_kme
          obj%m_jms=this%m_jms
          obj%m_jme=this%m_jme
          obj%m_ids=this%m_ids
          obj%m_ide=this%m_ide
          obj%m_kds=this%m_kds
          obj%m_kde=this%m_kde
          obj%m_jds=this%m_jds
          obj%m_jde=this%m_jde
          obj%m_its=this%m_its
          obj%m_ite=this%m_ite
          obj%m_kts=this%m_kts
          obj%m_kte=this%m_kte
          obj%m_jts=this%m_jts
          obj%m_jte=this%m_jte
          obj%m_rand=this%m_rand
          obj%m_p3d   = this%m_p3d   ** ex
          obj%m_p8w3d = this%m_p8w3d ** ex
          obj%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: press3D_neq_press3D i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_neq_press3D(this,other) result(neq)
          use module_helper_fields_equality
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals/return
          type(BoolCompField3D) :: neq
          ! Start of executable statements
          neq%bp3d   = this%m_p3d   /= other%m_p3d
          neq%bp8w3d = this%m_p8w3d /= other%m_p8w3d
    end function
    
    !======================================================60
    ! function: press3D_eq_press3D i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_eq_press3D(this,other) result(eq)
          use module_helper_fields_equality
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals
          type(BoolCompField3D) :: eq
          ! Start of executable statements
          eq%bp3d   = this%m_p3d   == other%m_p3d
          eq%bp8w3d = this%m_p8w3d == other%m_p8w3d
    end function
    
    !======================================================60
    ! function: press3D_gt_press3D i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_gt_press3D(this,other) result(gt)
          use module_helper_fields_equality
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals
          type(BoolCompField3D) :: gt
          ! Start of executable statements
          gt%bp3d   = this%m_p3d     > other%m_p3d
          gt%bp8w3d = this%m_p8w3d   > other%m_p8w3d
    end function
    
    !======================================================60
    ! function: press3D_lt_press3D i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_lt_press3D(this,other) result(lt)
          use module_helper_fields_equality
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals
          type(BoolCompField3D) :: lt
          ! Start of executable statements
          lt%bp3d   = this%m_p3d   < other%m_p3d
          lt%bp8w3d = this%m_p8w3d < other%m_p8w3d
    end function
    
    !======================================================60
    ! function: press3D_ge_press3D i.e. overloaded
    !           operator (>=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_ge_press3D(this,other) result(ge)
          use module_helper_fields_equality
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals
          type(BoolCompField3D) :: ge
          ! Start of executable statemetns
          ge%bp3d   = this%m_p3d   >= other%m_p3d
          ge%bp8w3d = this%m_p8w3d >= other%m_p8w3d
    end function
    
    !======================================================60
    ! function: press3D_le_press3D i.e. overloaded
    !           operator (<=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function press3D_le_press3D(this,other) result(le)
          use module_helper_fields_equality
          implicit none
          type(AtmPressure3D), intent(in) :: this
          type(AtmPressure3D), intent(in) :: other
          ! Locals
          type(BoolCompField3D) :: le
          ! Start of executable statemetns
          le%bp3d   = this%m_p3d   <= other%m_p3d
          le%bp8w3d = this%m_p8w3d <= other%m_p8w3d
    end function
    
    !======================================================60
    ! function: press3D_gradient i.e. 
    !           operator (.grad.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function press3D_gradient(this) result(pgrad)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: pgrad
!DIR$     ATTRIBUTES ALIGN : 32 :: pgrad
          integer(I64P) :: j,k,i
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_kms, &
                    d2e=>this%m_kme, &
                    d3s=>this%m_jms, &
                    d3e=>this%m_jme )
              allocate(pgrad(d1s:d1e,d2s:d2e,d3s:d3e))
          end associate
          isf = 1._R64P/real(sf,R64P)
          eps = MACHEPSF64*0.333333333333333333333333_R64P
          do j = 2, this%m_jme-1
              do k = 2, this%m_kme-1
                  do i = 2, this%m_ime-1
!DIR$                 IF(USE_SOFT_PREFETCH .EQ. 1)
                      call MM_PREFETCH(this%m_p3d(i+2,k,j),1)
!DIR$                 ENDIF                      
                      tmp = this%m_p3d(i+1,k+1,j+1)- &
                             this%m_p3d(i-1,k-1,j-1)
                      pgrad(i,k,j) = this%m_p3d(i+1,k+1,j+1)- &
                                     this%m_p3d(i-1,k-1,j-1)/ &
                                     (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                  end do
              end do
          end do
    end function 
    
    !======================================================60
    ! function: press3D_laplacian i.e. 
    !           operator (.laplacian.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function press3D_laplacian(this)  result(plap)
          implicit none
          type(AtmPressure3D), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: plap   ! In-place
!DIR$     ATTRIBUTES ALIGN : 32 :: plap
          integer(I64P) :: j,k,i
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          plap = .grad.this
          isf = 1._R64P/real(sf,R64P)
          eps = MACHEPSF64*0.333333333333333333333333_R64P
          do j = 2, this%m_jme-1
              do k = 2, this%m_kme-1
                  do i = 2, this%m_ime-1
!DIR$                 IF (USE_SOFT_PREFETCH .EQ. 1)
                      call MM_PREFETCH(plap(i+2,k,j),1)
!DIR$                 ENDIF
                      tmp = plap(i+1,k+1,j+1)- &
                            plap(i-1,k-1,j-1)
                      plap(i,k,j) = plap(i+1,k+1,j+1)- &
                                    plap(i-1,k-1,j-1)/ &
                                    (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                  end do
              end do
          end do
    end function      
    
    
end module mod_atmos_pressure3d