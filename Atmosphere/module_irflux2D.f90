
module mod_irflux2D

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_irflux2D'
 !          
 !          Purpose:
 !                    Infra-red radiation flux field 2D.
 !                    Represented in units of w/m^2.
 !                   
 !                     
 !          History:
 !                        Date: 12-09-2017
 !                        Time: 10:12 GMT+2
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
    private
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT, only : TRACEBACKQQ
    use module_class_error_check, only : array2D_not_alloc
                                         
    use module_kinds
    use mod_constants
    use module_logger
    use mod_code_timing
    
    public :: assignment (=)
    public :: operator   (/=)
    public :: operator   (==)
    public :: operator   (>)
    public :: operator   (<)
    public :: operator   (.grad.)
    public :: operator   (.laplacian.)
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_IRFLUX2D_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_IRFLUX2D_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_IRFLUX2D_MICRO = 0
    
    ! File/module full version
    integer(I32P), parameter, public :: MOD_IRFLUX2D_FULLVER = 1000*MOD_IRFLUX2D_MAJOR+100*MOD_IRFLUX2D_MINOR+ &
                                                               10*MOD_IRFLUX2D_MICRO
    
    ! Module file creation date/time
    character(*),  parameter, public :: MOD_IRFLUX2D_CREATE_DATE = "12-09-2017 10:22 +00200 (TUE 12 SEP 2017 GMT+2)"
    
    ! Module build date/time (should be set after successful build date/time)
    character(*),  parameter, public :: MOD_IRFLUX2D_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_IRFLUX2D_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_IRFLUX2D_DESCRIPT = " IR radiation flux 2D."
    
    !======================================================60
    !       Type: IRFlux2D
    !======================================================60
    
    type, public :: IRFlux2D
        
          ! public by default
          ! indices
          integer(I64P) :: m_ims,m_ime,m_kms,m_kme, &
                           m_ids,m_ide,m_kds,m_kde, &
                           m_its,m_ite,m_kts,m_kte
          
          ! Name of random flux distribution values 
          ! This is used for simulation.
          character(len=64) :: m_rand
          
          ! IR Flux 2D
          real(R64P), allocatable, dimension(:,:) :: m_fir2d
!DIR$     ATTRIBUTES ALIGN : 32 :: m_fir2d
          
          ! Built indicator (logical)
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
         
         procedure, nopass, public :: read_irflux2D
         
         procedure, nopass, public :: write_irflux2D
          
    end type IRFlux2D
    
         !=================================================60
         !  Module operators
         !=================================================60
          
         interface assignment (=)
              module procedure assign_irflux2D
         end interface
         
         interface operator   (/=)
              module procedure irflux2D_neq_irflux2D
         end interface
         
         interface operator   (==)
              module procedure irflux2D_eq_irflux2D
         end interface
         
         interface operator   (>)
              module procedure irflux2D_gt_irflux2D
         end interface
         
         interface operator   (<)
              module procedure irflux2D_lt_irflux2D
         end interface
         
         interface operator   (.grad.)
              module procedure irflux2D_gradient
         end interface
         
         interface operator   (.laplacian.)
              module procedure irflux2D_laplacian
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
    subroutine default_init(this,indices,logging, &
                            filename,append,dbg,err)
          implicit none
          class(IRFlux2D),              intent(inout) :: this
          integer(I64P), dimension(12), intent(in)    :: indices
          logical(I32P),                intent(in)    :: logging
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          integer(I32P),                intent(inout) :: err
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          integer(I64P)      :: k,i
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:191, In->mod_irflux2D/default_init: IRFlux2D already initilaized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_irflux2D/default_init:191, IRFlux2D already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          ! Begin construction
          this%m_ims=indices(1)
          this%m_ime=indices(2)
          this%m_kms=indices(3)
          this%m_kme=indices(4)
          this%m_ids=indices(5)
          this%m_ide=indices(6)
          this%m_kds=indices(7)
          this%m_kde=indices(8)
          this%m_its=indices(9)
          this%m_ite=indices(10)
          this%m_kts=indices(11)
          this%m_kte=indices(12)
          this%m_rand = "default-none"
          ! Array member
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_kms, &
                    d2e=>this%m_kme )
              allocate(this%m_fir2d(d1s:d1e,d2s:d2e),  &
                       STAT=aerr,                      &
                       ERRMSG=emsg )
         end associate
         if(aerr.NE.0) then
             if(logging) then
                 call log_startup(filename,append)
                 call log_UsrMsg("logger:231, In->mod_irflux2D/default_init: Allocation of [m_fir2d]: failed!!")
                 call log_shutdown()
             else
                 call DATE_AND_TIME(date=dstr,time=tstr)
                 write(ERROR_UNIT,*) "===========================FATAL========================="
                 write(ERROR_UNIT,*) "   (mod_irflux2D/default_init:231, Allocation of [m_fir2d]: failed!!)"
                 write(ERROR_UNIT,*) "   (System message:)", emsg
                 write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_irflux2D/default_init:231 -> [FATAL-ERROR]: Terminating execution!!"
         end if
         ! Array default initialization
         do k = this%m_kts, this%m_kte
!DIR$        SIMD VECTORLENGTHFOR(REAL(KIND=8))             
             do i = this%m_its, this%m_ite
                 this%m_fir2d(i,k) = LAM_PINF
             end do
         end do
         this%m_isbuilt = .true.
         if(dbg) then
             print*, "IR Flux 2D: ", this%m_fir2d
         end if
    end subroutine
                            
    !======================================================60
    ! subroutine: init
    ! Initialziation by user(caller) passed array which
    ! usually contain specific random scalar distribution.
    ! This array can be also initialized by calls to Goddard
    ! radiation scheme moudule.
    !   @Warning:
    !            Upon non-fatal early exit parameter
    !            'err' will be set to -1                      
    !======================================================60 
    subroutine init(this,indices,dr,fir2d,logging, &
                    profiling,filename,append,dbg,err,qpctimer)
          implicit none
          class(IRFlux2D),              intent(inout) :: this
          integer(I64P), dimension(12), intent(in)    :: indices
          character(len=*),             intent(in)    :: dr
          real(R64P), dimension(:,:),   intent(in)    :: fir2d
!DIR$     ASSUME_ALIGNED fir2d:32
          logical(I32P),                intent(in)    :: logging,profiling
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          integer(I32P),                intent(inout) :: err
          type(QPCTimer_t),             intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          integer(I64P)      :: k,i
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:294, In->mod_irflux2D/init: IRFlux2D already initilaized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_irflux2D/init:294, IRFlux2D already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          ! Begin construction
          this%m_ims=indices(1)
          this%m_ime=indices(2)
          this%m_kms=indices(3)
          this%m_kme=indices(4)
          this%m_ids=indices(5)
          this%m_ide=indices(6)
          this%m_kds=indices(7)
          this%m_kde=indices(8)
          this%m_its=indices(9)
          this%m_ite=indices(10)
          this%m_kts=indices(11)
          this%m_kte=indices(12)
          this%m_rand = dr
          ! Array member
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_kms, &
                    d2e=>this%m_kme )
              allocate(this%m_fir2d(d1s:d1e,d2s:d2e), &
                       STAT=aerr,                     &
                       ERRMSG=emsg)
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:334, In->mod_irflux2D/init: Allocation of [m_fir2d]: failed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_irflux2D/init:334, Allocation of [m_fir2d]: failed!!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_irflux2D/init:334 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          ! Array initilaization
          if(profiling) then
               call qpctimer_start(qpctimer,ifail)
               if(ifail.EQ.0) then
                  write(stderr,*) "mod_irflux2D/init: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
          do k = this%m_kts, this%m_kte
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))              
              do i = this%m_its, this%m_ite
!DIR$           IF (USE_SOFT_PREFETCH .EQ. 1)
                  call MM_PREFETCH(fir2d(i+2,k),1)
!DIR$           ENDIF                  
                  this%m_fir2d(i,k) = fir2d(i,k)
              end do
          end do
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_irflux2D/init: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_irflux2D/init: Unable to read performance counter -- fatal!!"
                end if
          end if
          this%m_isbuilt = .true.
          if(dbg) then
              print*, "IR Flux 2D: ", this%m_fir2d
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
          class(IRFlux2D),  intent(inout) :: this
          class(IRFlux2D),  intent(in)    :: other
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
                  call log_UsrMsg("logger:407, In->mod_irflux2D/copy: IRFlux2D already initilized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_irflux2D/copy:407, IRFlux2D already initialized!)"
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
          this%m_ids=other%m_ids
          this%m_ide=other%m_ide
          this%m_kds=other%m_kds
          this%m_kde=other%m_kde
          this%m_its=other%m_its
          this%m_ite=other%m_ite
          this%m_kts=other%m_kts
          this%m_kte=other%m_kte
          this%m_rand=other%m_rand
          this%m_fir2d=other%m_fir2d
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
          class(IRFlux2D),  intent(inout) :: this
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
                  call log_UsrMsg("logger:461, In->mod_irflux2d/destroy: IRFlux2D already destroyed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_irflux2D/destroy:461, IRFlux2D already destroyed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          this%m_ims=0
          this%m_ime=0
          this%m_kms=0
          this%m_kme=0
          this%m_ids=0
          this%m_ide=0
          this%m_kds=0
          this%m_kde=0
          this%m_its=0
          this%m_ite=0
          this%m_kts=0
          this%m_kte=0
          this%m_rand= " "
          if(ALLOCATED(this%m_fir2d)) then
              deallocate(this%m_fir2d, &
                         STAT=derr,    &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:495, In->mod_irflux2D/destroy: Failed to deallocate: [m_t3d] !!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(ERROR_UNIT,*) "===========================FATAL========================="
                      write(ERROR_UNIT,*) "   (mod_temp_field3D/destroy:495, Failed deallocate: [m_fir2d]!!)"
                      write(ERROR_UNIT,*) "   (System message:)", emsg
                      write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_irflux2D/destroy:495 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    !  subroutine: read_irflux2D
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine read_irflux2D(this,logging,filename,append, &
                              unit,ioerr,err   )
          implicit none
          class(IRFlux2D),  intent(in)    :: this
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
                  call log_UsrMsg("logger:538, In->mod_irflux2D/read_irflux2D: IRFlux2D in invalid state!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_irfluxD/read_irflux2D:538, IRFlux2D in invalid state!!)"
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
    !  subroutine: write_irflux2D
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine write_irflux2D(this,logging,filename,append, &
                              unit,ioerr,err   )
          implicit none
          class(IRFlux2D),  intent(in)    :: this
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
                  call log_UsrMsg("logger:577, In->mod_irflux2D/write_irflux2D: IRFlux2D in invalid state!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_irfluxD/write_irflux2D:577, IRFlux2D in invalid state!!)"
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
    subroutine assign_irflux2D(this,other)
          implicit none
          type(IRFlux2D), intent(inout) :: this
          type(IRFlux2D), intent(in)    :: other
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
               call DATE_AND_TIME(date=dstr,time=tstr)
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_irflux2D/assignment(=):611, Attempted self_assignment!)"
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
          this%m_ids=other%m_ids
          this%m_ide=other%m_ide
          this%m_kds=other%m_kds
          this%m_kde=other%m_kde
          this%m_its=other%m_its
          this%m_ite=other%m_ite
          this%m_kts=other%m_kts
          this%m_kte=other%m_kte
          this%m_rand=other%m_rand
          this%m_fir2d=other%m_fir2d
          this%m_isbuilt=other%m_isbuilt
    end subroutine
    
    !======================================================60
    ! function: irflux2D_neq_irflux2D i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function irflux2D_neq_irflux2D(ir1,ir2) result(neq)
          implicit none
          type(IRFlux2D), intent(in) :: ir1,ir2
          ! Locals
          logical(I32P), allocatable, dimension(:,:) :: neq
!DIR$     ATTRIBUTES ALIGN : 32 :: neq
          ! Start of executable statements
          neq = r1%m_fir2d /= r2%m_fir2d
    end function
    
    !======================================================60
    ! function: irflux2D_eq_irfluxD i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function irflux2D_eq_irflux2D(ir1,ir2) result(eq)
          implicit none
          type(IRFlux2D), intent(in) :: ir1,ir2
          ! Locals
          logical(I32P), allocatable, dimension(:,:) :: eq
!DIR$     ATTRIBUTES ALIGN : 32 :: eq
          ! Start of executable statements
          eq = r1%m_fir2d == r2%m_fir2d
    end function
    
    !======================================================60
    ! function: irflux2D_gt_irflux2D i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function irflux2D_gt_irflux2D(ir1,ir2) result (gt)
          implicit none
          type(IRFlux2D), intent(in) :: ir1,ir2
          ! Locals
          logical(I32P), allocatable, dimension(:,:) :: gt
!DIR$     ATTRIBUTES ALIGN : 32 :: gt
          ! Start of executable statements
          gt = r1%m_fir2d > r2%m_fir2d
    end function
    
    !======================================================60
    ! function: irflux2D_lt_irflux2D i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size and same stat distribution.
    !======================================================60
    function irflux2D_lt_irflux2D(ir1,ir2) result(lt)
          implicit none
          type(IRFlux2D), intent(in) :: ir1,ir2
          ! Locals
          logical(I32P), allocatable, dimension(:,:) :: lt
!DIR$     ATTRIBUTES ALIGN : 32 :: lt
          ! Start of executable statements
          lt = r1%m_fir2d < r2%m_fir2d
    end function
    
    !======================================================60
    ! function: irflux2D_gradient i.e. 
    !           operator (.grad.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function irflux2D_gradient(this) result(tgrad)
          implicit none
          type(IRFlux2D), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: tgrad
!DIR$     ATTRIBUTES ALIGN : 32 :: tgrad
          integer(I64P) :: k,i
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_kms, &
                    d2e=>this%m_kme)
              allocate(tgrad(d1s:d1e,d2s:d2e))
          end associate
          tgrad(:,:) = LAM_PINF
          isf = 1._R64P/real(sf,R64P)
          eps = MACHEPSF64*0.333333333333333333333333_R64P  
          do k = 2, this%m_kte-1
              do i = 2, this%m_ite-1
!DIR$          IF (USE_SOFT_PREFETCH .EQ. 1)
                  call MM_PREFETCH(this%m_fir2d(i+2,k),1)
!DIR$          ENDIF
                  tmp = this%m_fir2d(i+1,k+1)- &
                        this%m_fir2d(i-1,k-1)
                  tgrad(i,k) = this%m_fir2d(i+1,k+1)- &
                               this%m_fir2d(i-1,k-1)/ &
                             (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
              end do
          end do
    end function      
    
    !======================================================60
    ! function: irflux2D_laplacian i.e. 
    !           operator (.laplacian.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function irflux2D_laplacian(this) result(irlap)
          implicit none
          type(IRFlux2D), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: irlap
!DIR$     ATTRIBUTES ALIGN : 32 :: irlap
          integer(I64P) :: k,i
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          irlap = .grad.this
          isf = 1._R64P/real(sf,R64P)        
          eps = MACHEPSF64*0.333333333333333333333333_R64P  
          do k = 2, this%m_kte-1
              do i = 2, this%m_ite-1
!DIR$         IF (USE_SOFT_PREFETCH .EQ. 1)
                  call MM_PREFETCH(irlap(i+2,k),1)
!DIR$         ENDIF
                  tmp = irlap(i+1,k+1)- &
                        irlap(i-1,k-1)
                  irlap(i,k) = irlap(i+1,k+1)- &
                               irlap(i-1,k-1)/ &
                           (2._R64P*esp*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
              end do
          end do
    end function      
    
    
end module mod_irflux2d