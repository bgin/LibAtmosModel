
module mod_temp_field3D

    !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_temp_field3D'
 !          
 !          Purpose:
 !                    Atmospheric scalar temperature field 3D
 !                   
 !                     
 !          History:
 !                        Date: 08-09-2017
 !                        Time: 12:12 GMT+2
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
    private ! everything is private beside variables and procedures deemed public
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT, only : TRACEBACKQQ
    use module_class_error_check, only : array3D_not_alloc, &
                                         conform_dim3D_Real
    use module_kinds
    use mod_constants
    use module_logger
    use mod_code_timing
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_TEMP_FIELD3D_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_TEMP_FIELD3D_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_TEMP_FIELD3D_MICRO = 0
    
    ! File/module full version
    integer(I32P), parameter, public :: MOD_TEMP_FIELD3D_FULLVER = 1000*MOD_TEMP_FIELD3D_MAJOR+100*MOD_TEMP_FIELD3D_MINOR+ &
                                                                   10*MOD_TEMP_FIELD3D_MICRO
    
    ! Module file creation date/time
    character(*),  parameter, public :: MOD_TEMP_FIELD3D_CREATE_DATE = "08-09-2017 14:09 +00200 (FRI 08 SEP 2017 GMT+2)"
    
    ! Module build date/time (should be set after successful buld date)
    character(*),  parameter, public :: MOD_TEMP_FIELD3D_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_TEMP_FIELD3D_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_TEMP_FIELD3D_DESCRIPT = "Atmospheric temperature scalar field 3D."
    
    !======================================================60
    !  Type: AtmTemp3D
    !======================================================60

    type, public :: AtmTemp3D
        
         ! public by default
        
         ! indices
         integer(I64P) ::  m_ims,m_ime,m_kms,m_kme, &
                           m_jms,m_jme,m_ids,m_ide, &
                           m_kds,m_kde,m_jds,m_jde, &
                           m_its,m_ite,m_kts,m_kte, &
                           m_jts,m_jte
          
         ! name of random temp. distribution
         character(len=64) :: m_rand
         
!DIR$    ATTRIBUTES ALIGN : 32 :: m_t3d
         real(R64P), allocatable, dimension(:,:,:) :: m_t3d
         
         ! built indicator (logical)
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
         
         procedure, nopass, public :: read_temp3D
         
         procedure, nopass, public :: write_temp3D
    
    end type AtmTemp3D
         
         !=================================================60
         !  Module operators
         !=================================================60
         
         interface assignment (=)
               module procedure assign_temp3D
         end interface
         
         interface operator (+)
               module procedure temp3D_add_temp3D
         end interface
         
         interface operator (-)
               module procedure temp3D_sub_temp3D
         end interface
         
         interface operator (*)
               module procedure temp3D_mul_temp3D
               module procedure temp3D_mul_scalar
               module procedure scalar_mul_temp3D
         end interface
         
         interface operator (/)
               module procedure temp3D_div_temp3D
               module procedure temp3D_div_scalar
         end interface
         
         interface operator (**)
               module procedure temp3D_exponent
         end interface
         
         interface operator (/=)
               module procedure temp3D_neq_temp3D
         end interface
         
         interface operator (==)
               module procedure temp3D_eq_temp3D
         end interface
         
         interface operator (>)
               module procedure temp3D_gt_temp3D
         end interface
         
         interface operator (<)
               module procedure temp3D_lt_temp3D
         end interface
         
         interface operator (>=)
               module procedure temp3D_ge_temp3D
         end interface
         
         interface operator (<=)
               module procedure temp3D_le_temp3D
         end interface
         
         interface operator (.grad.)
               module procedure temp3D_gradient
         end interface
         
         interface operator (.laplacian.)
              module procedure temp3D_laplacian
         end interface
         
    contains
    
    !=================================================60
    !              Implementation
    !=================================================60
         
    !=================================================60
    !  subroutine: default_init
    !  Default initialization to +INF values.
    !  @Warning:
    !            Upon non-fatal early exit parameter
    !            'err' will be set to -1
    !=================================================60
    subroutine default_init(this,indices,logging,  &
                            filename,append,dbg,err   )
          implicit none
          class(AtmTemp3D),             intent(inout) :: this
          integer(I64P), dimension(18), intent(in)    :: indices
          logical(I32P),                intent(in)    :: logging
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          integer(I32P),                intent(inout) :: err
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          integer(I64P)      :: j,k,i
          ! Start of executable sattements
          if(err.LT0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:220, In->mod_temp_field3D/default_init: AtmTemp3D already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_temp_field3D/default_init:220, AtmTemp3D already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
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
          ! Array
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_kms, &
                    d2e=>this%m_kme, &
                    d3s=>this%m_jms, &
                    d3e=>this%m_jme )
              allocate(this%m_t3d(d1s:d1e,d2s:d2e,d3s:d3e), &
                       STAT=aerr,                           &
                       ERRMSG=emsg)
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:259, In->mod_temp_field3D/default_init: Allocation of [m_t3d]: failed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_temp_field3D/default_init:259, Allocation of [m_t3d]: failed!!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_temp_field3D/default_init:259 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          ! Array default initilaization
          do j = this%m_jms, this%m_jme
              do k = this%m_kms, this%m_kme
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                  
                  do i = this%m_ims, this%m_ime
                      this%m_t3d(i,k,j) = LAM_PINF
                  end do
              end do
          end do
          this%m_isbuilt = .true.
          if(dbg) then
              print*, "Scalar temperature field3D: ", this%m_t3d
          end if
    end subroutine
                            
    !======================================================60
    ! subroutine: init
    ! Initialziation by user(caller) passed array which
    ! usually contain specific random scalar distribution.
    !   @Warning:
    !            Upon non-fatal early exit parameter
    !            'err' will be set to -1                      
    !======================================================60  
    subroutine init(this,indices,dr,t3d,logging, &
                    profiling,filename,append,dbg,err,qpctimer )
          implicit none
          class(AtmTemp3D),             intent(inout) :: this
          integer(I64P), dimension(18), intent(in)    :: indices
          character(len=*),             intent(in)    :: dr    ! Name of random distribution
          real(R64P), dimension(:,:,:), intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                intent(in)    :: logging,profiling
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          integer(I32P),                intent(inout) :: err
          type(QPCTimer_t),             intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I64P)      :: j,k,i
          integer(I32P)      :: aerr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:318, In->mod_temp_field3D/init: AtmTemp3D already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_temp_field3D/init:318, AtmTemp3D already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
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
          this%m_rand = dr
          ! Allocate arrays
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_kms, &
                    d2e=>this%m_kme, &
                    d3s=>this%m_jms, &
                    d3e=>this%m_jme )
              allocate(this%m_t3d(d1s:d1e,d2s:d2e,d3s:d3e), &
                       STAT=aerr,                           &
                       ERRMSG=emsg)
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:361, In->mod_temp_field3D/init: Failed to allocate: [m_t3d] !!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_temp_field3D/default_init:361, Allocation of [m_t3d]: failed!!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_temp_field3D/default_init:361 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          ! Array initialization
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_temp_field3D/init: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
          do j = this%m_jts, this%m_jte
              do k = this%m_kts, this%m_kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                  
                  do i = this%m_its, this%m_ite
                      this%m_t3d(i,k,j) = t3d(i,k,j)
                      call MM_PREFETCH(t3d(i+2,k,j),1)
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
                         write(stderr,*) "mod_temp_field3D/init: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_temp_field3D/init: Unable to read performance counter -- fatal!!"
                end if
          end if
          this%m_isbuilt = .true.
          if(dbg) then
              print*, "Temperature scalar field 3D: ", this%m_t3d
          end if
    end subroutine
                    
    !======================================================60 
    ! subroutine: copy 
    ! Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.                
    !======================================================60 
    subroutine copy(this,other,logging,filename,append,err)
          implicit none
          class(AtmTemp3D), intent(inout) :: this
          class(AtmTemp3D), intent(in)    :: other
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: filename
          logical(I32P),    intent(in)    :: append
          integer(I32P),    intent(inout) :: err
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:431, In->mod_temp_field3D/copy: AtmTemp3D already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_temp_field3D/copy:431, AtmTemp3D already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
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
          this%m_t3d=other%m_t3d
          this%m_isbuilt = .true.
    end subroutine
    
    !======================================================60
    !  subroutine: destroy
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine destroy(this,logging,filename,append,err)
          implicit none
          class(AtmTemp3D), intent(inout) :: this
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: filename
          logical(I32P),    intent(in)    :: append
          integer(I32P),    intent(inout) :: err
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:494, In->mod_temp_field3D/destroy: AtmTemp3D already destroyed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_temp_field3D/destroy:494, AtmTemp3D already destroyed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
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
          if(ALLOCATED(this%m_t3d)) then
              deallocate(this%m_t3d, &
                         STAT=derr,  &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:515, In->mod_temp_field3D/destroy: Failed to deallocate: [m_t3d] !!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(ERROR_UNIT,*) "===========================FATAL========================="
                      write(ERROR_UNIT,*) "   (mod_temp_field3D/destroy:515, Failed deallocate: [m_t3d]!!)"
                      write(ERROR_UNIT,*) "   (System message:)", emsg
                      write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_temp_field3D/destroy:515 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    !  subroutine: read_temp3D
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine read_temp3D(this,logging,filename,append,   &
                           unit,ioerr,err   )
          implicit none
          class(AtmTemp3D), intent(in)    :: this
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: filename
          logical(I32P),    intent(in)    :: append
          integer,          intent(in)    :: unit
          integer(I32P),    intent(inout) :: ioerr,err
          ! Locals
           character(len=40) :: dstr,tstr
          
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:565, In->mod_temp_field3D/read_temp3D: AtmTemp3Din invalid state!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_temp_field3D/read_temp3D:565, AtmTemp3D in invalid state!!)"
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
                           
    !======================================================60
    !  subroutine: write_temp3D
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine write_temp3D(this,logging,filename,append,  &
                             unit,ioerr,err)
          implicit none
          class(AtmTemp3D), intent(inout) :: this
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: filename
          logical(I32P),    intent(in)    :: append
          integer,          intent(in)    :: unit
          integer(I32P),    intent(inout) :: ioerr,err
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:608, In->mod_temp_field3D/write_temp3D: AtmTemp3Din invalid state!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_temp_field3D/write_temp3D:608, AtmTemp3D in invalid state!!)"
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
    !                   Module operators
    !======================================================60
    
    !=======================================================60
    !  subroutine: overloaded assignment (=)
    !=======================================================60  
    subroutine assign_temp3D(this,other)
          implicit none
          type(AtmTemp3D), intent(inout) :: this
          type(AtmTemp3D), intent(in)    :: this
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
               call DATE_AND_TIME(date=dstr,time=tstr)
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_temp_field3D/assignment(=):642, Attempted self_assignment!)"
               write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
               write(stderr,*) "===========================NON-FATAL=========================="
               return
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
          this%m_t3d=other%m_t3d
          this%m_isbuilt=other%m_isbuilt
    end subroutine
    
    !======================================================60
    ! function: temp3D_add_temp3D i.e. overloaded
    !           operator (+)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_add_temp3D(t1,t2) result(t3)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          type(AtmTemp3D) :: t3
          ! Start of executable statements
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = t1%m_t3d + t2%m_t3d
          t3%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: temp3D_sub_temp3D i.e. overloaded
    !           operator (-)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_sub_temp3D(t1,t2) result(t3)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          type(AtmTemp3D) :: t3
          ! Start of executable statements
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = t1%m_t3d - t2%m_t3d
          t3%m_isbuilt = .true.
    end function 
    
    !======================================================60
    ! function: temp3D_mul_temp3D i.e. overloaded
    !           operator (*)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_mul_temp3D(t1,t2) result(t3)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          type(AtmTemp3D) :: t3
          ! Start of executable statements
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = t1%m_t3d * t2%m_t3d
          t3%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: temp3D_mul_scalar i.e. overloaded
    !           operator (*)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_mul_scalar(t1,s1)  result(t3)
          implicit none
          type(AtmTemp3D), intent(in) :: t1
          real(R64P),      intent(in) :: s1
          ! Locals
          type(AtmTemp) :: t3
          ! Start of executable statements
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = t1%m_t3d * s1
          t3%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: scalar_mul_temp3D i.e. overloaded
    !           operator (*)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function scalar_mul_temp3D(s1,t1) result(t3)
          implicit none
          real(R64P),      intent(in) :: s1
          type(AtmTemp3D), intent(in) :: t1
          ! Locals
          type(AtmTemp3D) :: t3
          ! Start of executable statements
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = s1 * t1%m_t3d
          t3%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: temp3D_div_temp3D i.e. overloaded
    !           operator (/)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_div_temp3D(t1,t2) result(t3)
          implicit none
           type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          type(AtmTemp3D) :: t3
          ! Start of executable statements
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = t1%m_t3d / t2%m_t3d
          t3%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: temp3D_div_scalar i.e. overloaded
    !           operator (/)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_div_scalar(t1,s1) result(t3)
          implicit none
          type(AtmTemp3D), intent(in) :: t1
          real(R64P),      intent(in) :: s1
          ! Locals
          type(AtmTemp3D) :: t3
          ! Start of executable statemetns
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = t1%m_t3d / s1
          t3%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: temp3D_exponent i.e. overloaded
    !           operator (**)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60 
    function temp3D_exponent(t1,ex) result(t3)
          implicit none
          type(AtmTemp3D), intent(in) :: t1
          real(R64P),      intent(in) :: ex
          ! Locals
          type(AtmTemp3D) :: t3
          ! Start of executable sttements
          t3%m_ims=t1%m_ims
          t3%m_ime=t1%m_ime
          t3%m_kms=t1%m_kms
          t3%m_kme=t1%m_kme
          t3%m_jms=t1%m_jms
          t3%m_jme=t1%m_jme
          t3%m_ids=t1%m_ids
          t3%m_ide=t1%m_ide
          t3%m_kds=t1%m_kds
          t3%m_kde=t1%m_kde
          t3%m_jds=t1%m_jds
          t3%m_jde=t1%m_jde
          t3%m_its=t1%m_its
          t3%m_ite=t1%m_ite
          t3%m_kts=t1%m_kts
          t3%m_kte=t1%m_kte
          t3%m_jts=t1%m_jts
          t3%m_jte=t1%m_jte
          t3%m_rand=t1%m_rand
          t3%m_t3d = t1%m_t3d ** ex
          t3%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: temp3D_neq_temp3D i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_neq_temp3D(t1,t2) result(neq)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          logical(I32P), allocatable, dimension(:,:,:) :: neq
!DIR$     ATTRIBUTES ALIGN : 32 :: neq
          ! Start of executable stataments
          neq = t1%m_t3d /= t2%m_t3d
    end function
    
    !======================================================60
    ! function: temp3D_eq_temp3D i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_eq_temp3D(t1,t2) result(eq)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          logical(I32P), allocatable, dimension(:,:,:) :: eq
!DIR$     ATTRIBUTES ALIGN : 32 :: eq
          ! Start of executable statements
          eq = t1%m_t3d == t2%m_t3d
    end function
    
    !======================================================60
    ! function: temp3D_gt_temp3D i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_gt_temp3D(t1,t2) result(gt)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          logical(I32P), allocatable, dimension(:,:,:) :: gt
!DIR$     ATTRIBUTES ALIGN : 32 :: gt
          ! Start of executable statements
          gt = t1%m_t3d > t2%m_t3d
    end function
    
    !======================================================60
    ! function: temp3D_lt_temp3D i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_lt_temp3D(t1,t2) result(lt)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          logical(I32P), allocatable, dimension(:,:,:) :: lt
!DIR$     ATTRIBUTES ALIGN : 32 :: lt
          ! Start of executable statements
          lt = t1%m_t3d < t2%m_t3d
    end function
    
    !======================================================60
    ! function: temp3D_ge_temp3D i.e. overloaded
    !           operator (>=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_ge_temp3D(t1,t2) result(ge)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          logical(I32P), allocatable, dimension(:,:,:) :: ge
!DIR$     ATTRIBUTES ALIGN : 32 :: ge
          ! Start of executable sattemetns
          ge = t1%m_t3d >= t2%m_t3d
    end function
    
    !======================================================60
    ! function: temp3D_le_temp3D i.e. overloaded
    !           operator (<=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function temp3D_le_temp3D(t1,t2) result(le)
          implicit none
          type(AtmTemp3D), intent(in) :: t1,t2
          ! Locals
          logical(I32P), allocatable, dimension(:,:,:) :: le
!DIR$     ATTRIBUTES ALIGN : 32 :: le
          ! Start of executable sattemetns
          le = t1%m_t3d >= t2%m_t3d
    end function
    
    !======================================================60
    ! function: temp3D_gradient i.e. 
    !           operator (.grad.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function temp3D_gradient(this) result(tgrad)
          implicit none
          type(AtmTemp3D), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: tgrad
!DIR$     ATTRIBUTES ALIGN : 32 :: tgrad
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
              allocate(tgrad(d1s:d1e,d2s:d2e,d3s:d3e))
         end associate
         tgrad(:,:,:) = LAM_PINF          
         isf = 1._R64P/real(sf,R64P)
         eps = MACHEPSF64*0.333333333333333333333333_R64P   
         do j = 2, this%m_jme-1
             do k = 2, this%m_kme-1
                 do i = 2, this%m_ime-1
!DIR$            IF (USE_SOFT_PREFETCH .EQ. 1)
                     call MM_PREFETCH(this%m_t3d(i+2,k,j),1)
!DIR$            ENDIF
                     tmp = this%m_t3d(i+1,k+1,j+1)- &
                           this%m_t3d(i-1,k-1,j-1)
                     tgrad(i,k,j) = this%m_t3d(i+1,k+1,j+1)-
                                    this%%m_t3d(i-1,k-1,j-1)/ &
                                    (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                 end do
             end do
         end do
    end function  
    
    !======================================================60
    ! function: temp3D_laplacian i.e. 
    !           operator (.laplacian.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function temp3D_laplacian(this) result(tlap)
          implicit none
          type(AtmTemp3D), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: tlap
!DIR$     ATTRIBUTES ALIGN : 32 :: tlap
          integer(I64P) :: j,k,i
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          tlap = .grad.this
          isf = 1._R64P/real(sf,R64P)        
          eps = MACHEPSF64*0.333333333333333333333333_R64P          
          do j = 2, this%m_jme-1
             do k = 2, this%m_kme-1
                 do i = 2, this%m_ime-1
!DIR$            IF (USE_SOFT_PREFETCH .EQ. 1)
                     call MM_PREFETCH(tlap(i+2,k,j),1)
!DIR$            ENDIF
                     tmp = tlap(i+1,k+1,j+1)- &
                           tlap(i-1,k-1,j-1)
                     tlap(i,k,j) = tlap(i+1,k+1,j+1)-
                                    tlap(i-1,k-1,j-1)/ &
                                    (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                 end do
             end do
         end do          
    end function
   
    
    
end module mod_temp_field3D