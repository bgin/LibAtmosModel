
module mod_echwsamples

#include "Config.hpp"

     
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_echwsamples'
 !          
 !          Purpose:
 !                   Mathematical representation and computation
 !                   of power weather sample of exponential chirp
 !                    wave radar signal.
 !                   This module also contains subroutines which
 !                   perform computation of signal statistics.
 !          History:
 !                        Date: 01-11-2017
 !                        Time: 10:00 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                      Doppler Radar and Weather Observations
 !                      Richard L. Dvorak, Dusan S. Zrnic
 !                      pages 67-72 
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
    use module_kinds
    use module_logger, only : log_startup, &
                              log_UsrMsg,  &
                              log_shutdown
    
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT, &
                                stdout=>OUTPUT_UNIT
    use IFPORT,          only : TRACEBACKQQ
    use mod_timer
    use mod_constants,   only : LAM_PINF
    
    public :: assignment (=)
    public :: operator   (/=)
    public :: operator   (==)
    public :: operator   (>)
    public :: operator   (<)
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter :: MOD_ECHWSAMPLES_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter :: MOD_ECHWSAMPLES_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter :: MOD_ECHWSAMPLES_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter :: MOD_ECHWSAMPLES_FULLVER = 1000*MOD_ECHWSAMPLES_MAJOR+100*MOD_ECHWSAMPLES_MINOR+ &
                                                          10*MOD_ECHWSAMPLES_MICRO
    
    ! Module creation date
    character(*),  parameter :: MOD_ECHWSAMPLES_CREATE_DATE = "01-11-2017 10:08 +00200 (WED 01 NOV 2017 GMT+2)"
    
    ! Module build date  (should be set after successful compilation)
    character(*),  parameter :: MOD_ECHWSAMPLES_BUILD_DATE = " "
    
    ! Module author short info
    character(*),  parameter :: MOD_ECHWSAMPLES_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter :: MOD_ECHWSAMPLES_DESCRIPT = " Exponential chirp weather  samples."
    
    !====================================================
    !  Type: ECSWSamples_t
    !====================================================
    
    type, public :: ECSWSamples_t
        
          private
          
          ! Number of echoes
          integer(I32P) :: m_nechoes
          
          ! Number of composed samples
          integer(I32P) :: m_size
          
          ! Number of scatterers
          integer(I32P) :: m_scatnum
          
          ! Time duration of samples interval
          real(R64P) ::   m_Ts
          
          ! Time duration of single sample
          real(R64P) ::   m_ths
          
          ! Samples indexing integral array sampID
          integer(I32P), allocatable, dimension(:) :: m_sampID
!DIR$     ATTRIBUTES ALIGN : 64 :: m_sampID
          
          ! Cached sample signal components
          
          ! Complex amplitude
          complex(R64P), allocatable, dimension(:) :: m_Ai
!DIR$     ATTRIBUTES ALIGN : 64 :: m_Ai
          
          ! Samples (IQ) components, one per sample
          complex(R64P), allocatable, dimension(:) :: m_Wi
!DIR$     ATTRIBUTES ALIGN : 64 :: m_Wi
          
        ! Complex exponential term itself beign a single complex value
        ! of function related to scaterrer distance and to specific bandwidth
          complex(R64P), allocatable, dimension(:) :: m_cexpt
!DIR$    ATTRIBUTES ALIGN : 64 :: m_cexpt
          
          ! Output members
          ! Power signal composition of sample train (per single sample)
          complex(R64P), allocatable, dimension(:) :: m_Vsamp
!DIR$    ATTRIBUTES ALIGN : 64 :: m_Vsamp
          
          ! Averaged power specific frequencies of sample train
          complex(R64P), allocatable, dimension(:) :: m_Vsampavg
!DIR$    ATTRIBUTES ALIGN : 64 :: m_Vsampavg
          
          ! built indicator (logical)
          logical(I32P) :: m_isbuilt
          
          contains
    
          !=====================================================
          !  Construction, copying, destruction
          !=====================================================
          
          procedure, pass(this), public :: init
          
          procedure, pass(this), public :: copy
          
          procedure, pass(this), public :: destroy
          
         !==================================================
         !      Getters
         !==================================================
          procedure, pass(this), public :: get_nechoes
          
          procedure, pass(this), public :: get_size
          
          procedure, pass(this), public :: get_scatnum
          
          procedure, pass(this), public :: get_Ts
          
          procedure, pass(this), public :: get_ths
          
          procedure, pass(this), public :: get_sampID
          
          procedure, pass(this), public :: get_Ai
          
          procedure, pass(this), public :: get_Wi
          
          procedure, pass(this), public :: get_cexpt
          
          procedure, pass(this), public :: get_Vsamp
          
          procedure, pass(this), public :: get_Vsampavg
          
          !================================================ 
          !  Computational procedures
          !================================================
          
          procedure, pass(this), public :: compute_Vsamp
          
          procedure, pass(this), public :: compute_Vsampavg
          
          !=================================================
          !    Read/write procedures
          !=================================================
          
          procedure, nopass, public :: read_samples
          
          procedure, nopass, public :: write_samples
          
    end type ECSWSamples_t
          
          !==================================================
          !  Module operators
          !==================================================
          
          interface assignment (=)
             module procedure assign_samples
          end interface
          
          interface operator   (/=)
             module procedure samples_neq_samples
          end interface
          
          interface operator   (==)
             module procedure samples_eq_samples
          end interface
          
          interface operator   (>)
            module procedure  samples_gt_samples
          end interface
          
          interface operator   (<)
            module procedure  samples_lt_samples
          end interface
          
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @subroutine: init                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of complex arrays (output)
    !  Copying of cached samples component arrays.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   2) -2 -- Invalid argument (any of them)
    !   3) -3 -- Floating-point arithmentic error 
    !            (general error) (not in this scope)
    !   4) -4 -- Overflow
    !   5) -5 -- Underflow
    !   6) -6 -- Inexact
    !   7) -7 -- Denormal
    !   8) -8 -- Cancellation error (cadna)
    !=================================================!
    subroutine init(this,nechoes,size,scatnum,Ts,ths,Ai, &
                    Wi,cexpt,logging,filename,append,dbg,verbose,err )
          implicit none
          class(ECSWSamples_t),           intent(inout) :: this
          integer(I32P),                  intent(in)    :: nechoes,size,scatnum
          real(R64P),                     intent(in)    :: Ts,ths
          complex(R64P), dimension(size), intent(in)    :: Ai
!DIR$     ASSUME_ALIGNED Ai:64
          complex(R64P), dimension(size), intent(in)    :: Wi
!DIR$     ASSUME_ALIGNED Wi:64
          complex(R64P), dimension(size), intent(in)    :: cexpt
!DIR$     ASSUME_ALIGNED cexpt:64
          logical(I32P),                  intent(in)    :: logging
          character(len=*),               intent(in)    :: filename
          logical(I32P),                  intent(in)    :: append,dbg,verbose
          integer(I32P),                  intent(inout) :: err
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: i,aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          ! Start of executable sttements
          if(err < 0) err = 0
          if(this%m_isbuilt == .true.) then
              if(logging == .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:283, In->mod_echwsamples/init: ECSWSamples_t already initialized!!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  
                      call DATE_AND_TIME(date=sdate,time=stime)
                      write(stderr,*) "===========================NON-FATAL=========================="
                      write(sdterr,*) " ( mod_echwsamples/init:283, ECSWSample_t already initialized!!)"
                      write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          if(nechoes <  1       .OR. &
             size    <= 1       .OR. &
             Ts      <= 0._R64P .OR. &
             ths     <= 0._R64P ) then
              if(logging == .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:303, In->mod_echwsamples/init: Invalid argument(s) in subroutine: init.")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_echwsamples/init:303, Invalid argument(s) in subroutine: init.)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -2
              return
          end if
          ! Begin construction
          this%m_nechoes = nechoes
          this%m_size    = size
          this%m_scatnum = scatnum
          this%m_Ts      = Ts
          this%m_ths     = this%m_Ts/REAL(size,R64P)
          ! Array members allocation
          associate(dim1=>this%m_size)
              allocate(this%m_sampID(dim1),   &
                       this%m_Ai(dim1),       &
                       this%m_Wi(dim1),       &
                       this%m_cexpt(dim1),    &
                       this%m_Vsamp(dim1),    &
                       this%m_Vsampavg(dim1), &
                       STAT=aerr,             &
                       ERRMSG=emsg)
          end associate
          if(aerr /= 0) then
              if(logging == .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:338,In->mod_echwsamples/init: Memory allocation failure!!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================FATAL=========================="
                  write(stderr,*) " ( mod_echwsamples/init:338, Memory allocation failure)"
                  write(stderr,*) " System message: ", emsg
                  write(stderr,*) " ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_echwsamples/init:338 -> [FATAL-ERROR]: Terminating execution!!"
          end if
         ! Arrays initialization                  
!DIR$    SIMD
          do i = 1, this%m_size
              this%m_sampID(i) = i
              this%m_Ai(i)     = A(i)
              this%m_Wi(i)     = Wi(i)
              this%m_cexpt(i)  = cexpt(i)
              this%m_Vsamp(i)  = DCMPLX(LAM_PIMF,LAM_PINF)
              this%m_Vsampavg(i) = DCMPLX(LAM_PINF,LAM_PINF)
          end do
          this%m_isbuilt = .true.
          if(dbg == .true.) then
             print*, "samples ID:                ", this%m_sampID
             print*, "Complex amplitude:         ", this%m_Ai
             print*, "IQ components:             ", this%m_Wi
             print*, "Exponential term:          ", this%m_cexpt
             print*, "V samp (default init):     ", this%m_Vsamp
             print*, "V samp avg (default init): ", this%m_Vsampavg
          end if
    end subroutine
                    
    !=================================================!
    !  @subroutine: copy                                          
    !  @Purpose:
    !            Copying of object state.
    !            Deep copy semantics in use.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   2) -2 -- Invalid argument (any of them)
    !   3) -3 -- Floating-point arithmentic error 
    !            (general error) (not in this scope)
    !   4) -4 -- Overflow
    !   5) -5 -- Underflow
    !   6) -6 -- Inexact
    !   7) -7 -- Denormal
    !   8) -8 -- Cancellation error (cadna)
    !=================================================! 
    subroutine copy(this,other,logging,filename,append,err)
          implicit none
          class(ECSWSamples_t),  intent(inout) :: this
          class(ECSWSamples_t),  intent(in)    :: other
          logical(I32P),         intent(in)    :: logging
          character(len=*),      intent(in)    :: filename
          logical(I32P),         intent(in)    :: append
          integer(I32P),         intent(inout) :: err
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(err < 0) err = 0
          if(this%m_isbuilt == .true.) then
              if(logging == .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:413, In->mod_echwsamples/copy: ECSWSamples_t already initialized!!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  
                      call DATE_AND_TIME(date=sdate,time=stime)
                      write(stderr,*) "===========================NON-FATAL=========================="
                      write(sdterr,*) " ( mod_echwsamples/copy:413, ECSWSample_t already initialized!!)"
                      write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          ! Begin copy-construction
          this%m_nechoes  = other%m_nechoes
          this%m_size     = other%m_size
          this%m_scatnum  = other%m_scatnum
          this%m_Ts       = other%m_Ts
          this%m_ths      = other%m_ths
          this%m_sampID   = other%m_sampID
          this%m_Ai       = other%m_Ai
          this%m_Wi       = other%m_Wi
          this%m_cexpt    = other%m_cexpt
          this%m_Vsamp    = other%m_Vsamp
          this%m_Vsampavg = other%m_Vsampavg
          this%m_isbuilt  = .true.
    end subroutine
    
    !=================================================!
    !  @subroutine: destroy                                          
    !  @Purpose:
    !            Destroys object state by allocatble
    !            arrays deallocation and setting
    !            member scalar variables to default values.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   2) -2 -- Invalid argument (any of them)
    !   3) -3 -- Floating-point arithmentic error 
    !            (general error) (not in this scope)
    !   4) -4 -- Overflow
    !   5) -5 -- Underflow
    !   6) -6 -- Inexact
    !   7) -7 -- Denormal
    !   8) -8 -- Cancellation error (cadna)
    !=================================================! 
    subroutine destroy(this,logging,filename,append,err)
          implicit none
          class(ECSWSamples_t), intent(inout) :: this
          logical(I32P),        intent(in)    :: logging
          character(len=*),     intent(in)    :: filename
          logical(I32P),        intent(in)    :: append
          integer(I32P),        intent(inout) :: err
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          ! Sanity check on err parameter
          if(err < 0) err = 0
          if(this%m_isbuilt == .true.) then
              if(logging == .true.) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:483, In->mod_echwsamples/destroy: ECSWSamples_t already destroyed!!")
                      call log_shutdown()
              else if(verbose == .true.) then
                  
                      call DATE_AND_TIME(date=sdate,time=stime)
                      write(stderr,*) "===========================NON-FATAL=========================="
                      write(sdterr,*) " ( mod_echwsamples/destroy:483, ECSWSample_t already destroyed!!)"
                      write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          this%m_nechoes = 0
          this%m_size    = 0
          this%m_scatnum = 0
          this%m_Ts      = LAM_PINF
          this%m_ths     = LAM_PINF
          deallocate(this%m_sampID,   &
                     this%m_Ai,       &
                     this%m_Wi,       &
                     this%m_cexpt,    &
                     this%m_Vsamp,    &
                     this%m_Vsampavg, &
                     STAT=derr,       &
                     ERRMSG=emsg )
          if(derr /= 0) then
              if(logging == .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:514,In->mod_echwsamples/destroy: Memory deallocation failure!!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================FATAL=========================="
                  write(stderr,*) " ( mod_echwsamples/destroy:514, Memory deallocation failure)"
                  write(stderr,*) " System message: ", emsg
                  write(stderr,*) " ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_echwsamples/destroy:514 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !====================================================
    !  @function: pure get_nechoes
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nechoes
!DIR$ ENDIF
    pure function get_nechoes(this) result(nechoes)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nechoes
          ! Start of executable statemetns
          nechoes = this%m_nechoes
    end function
    
    !====================================================
    !  @function: pure get_size
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_size
!DIR$ ENDIF
    pure function get_size(this) result(nsize)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nsize
          ! Start of executable statemetns
          nsize = this%m_size
    end function
    
    !====================================================
    !  @function: pure get_scatnum
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_scatnum
!DIR$ ENDIF
    pure function get_scatnum(this) result(scatnum)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          integer(I32P) :: scatnum
          ! Start of executable statements
          scatnum = this%m_scatnum
    end function
    
    !====================================================
    !  @function: pure get_Ts
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Ts
!DIR$ ENDIF
    pure function get_Ts(this) result(Ts)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          real(R64P) :: Ts
          ! Start of executable statemetns
          Ts = this%m_Ts
    end function
    
    !====================================================
    !  @function: pure get_ths
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_ths
!DIR$ ENDIF
    pure function get_ths(this) result(ths)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          real(R64P) :: ths
          ! Start of executable statemetns
          ths = this%m_ths
    end function
    
    !====================================================
    !  @function: pure get_sampID
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_sampID
!DIR$ ENDIF
    pure function get_sampID(this) result(sampID)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          integer(I32P), allocatable, dimension(:) :: sampID
!DIR$     ATTRIBUTES ALIGN : 64 :: sampID
          ! Start of executable statements
          sampID = this%m_sampID
    end function
    
    !====================================================
    !  @function: pure get_Ai
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Ai
!DIR$ ENDIF
    pure function get_Ai(this) result(Ai)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: Ai
!DIR$     ATTRIBUTES ALIGN : 64 :: Ai
          ! Start of executable statemetns
          Ai = this%m_Ai
    end function
    
    !====================================================
    !  @function: pure get_Wi
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Wi
!DIR$ ENDIF
    pure function get_Wi(this) result(Wi)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: Wi
!DIR$     ATTRIBUTES ALIGN : 64 :: Wi
          ! Start of executable statemetns
          Wi = this%m_Wi
    end function
    
    !====================================================
    !  @function: pure get_cexpt
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_cexpt
!DIR$ ENDIF
    pure function get_cexpt(this) result(cexpt)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: cexpt
!DIR$     ATTRIBUTES ALIGN : 64 :: cexpt
          ! Start of executable statemetns
          cexpt = this%m_cexpt
    end function
    
    !====================================================
    !  @function: pure get_Vsamp
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Vsamp
!DIR$ ENDIF
    pure function get_Vsamp(this) result(Vsamp)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: Vsamp
!DIR$     ATTRIBUTES ALIGN : 64 :: Vsamp
          ! Start of executable statements
          Vsamp = this%m_Vsamp
    end function
    
    !====================================================
    !  @function: pure get_Vsampavg
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Vsampavg
!DIR$ ENDIF
    pure function get_Vsampavg(this) result(Vsampavg)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: Vsampavg
!DIR$     ATTRIBUTES ALIGN : 64 :: Vsampavg
          ! Start of executable statemetms
          Vsampavg = this%m_Vsampavg
    end function
    
    !====================================================
    !  @function: pure get_isbuilt
    !  @Puprose:
    !              getter
    !====================================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$ ENDIF
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(ECSWSamples_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable statemetns
          isbuilt = this%m_isbuilt
    end function
    
    !=================================================!
    !  @subroutine: compute_Vsamp                                         
    !  @Purpose:
    !            Performs computation of weather radar
    !            signal samples and stores them in
    !            member array m_Vsamp
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   2) -2 -- Invalid argument (any of them)
    !   3) -3 -- Floating-point arithmentic error 
    !            (general error) (in this case)
    !   4) -4 -- Overflow
    !   5) -5 -- Underflow
    !   6) -6 -- Inexact
    !   7) -7 -- Denormal
    !   8) -8 -- Cancellation error (cadna)
    !=================================================! 
    subroutine compute_Vsamp(this,err,fpflags,verbose)
          use mod_constants, only : LAM_ISQRT2,LAM_ZC
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exception
!DIR$     ENDIF
          implicit none
          class(ECSWSamples_t),                  intent(inout) :: this
          integer(I32P),               intent(inout)           :: err
          logical(I32P), dimension(5), intent(inout), optional :: fpflags
          logical(I32P),               intent(in)              :: verbose
          ! Locals
          complex(R64P) :: tmp
          integer(I32P) :: i,j
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$     ENDIF
          ! Start of executable statements
          ! Sanity check on input arguments
          if(err < 0) err = 0
          if(present(fpflags) == .true.) then
              if(ANY(fpflags) == .true.) then
                  fpflags = .false.
              end if
          end if
          tmp = LAM_ZC
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          if(present(fpflags) == .true.) then
              call ieee_get_status(status_value)
              call ieee_set_handling_mode(ieee_all,.false.)
              call ieee_set_flags(ieee_all,fpflags)
          else
              WRITE(stderr,*) "mod_echwsamples/compute_Vsamp:801 -- optional parameter: fpflags - not present!!"
          end if
!DIR$     ENDIF
          do i = 1, this%m_size
!DIR$     SIMD
              do j = 1, this%m_size
                  tmp = tmp+(this%m_Ai(j)*this%m_Wi(j)*this%m_cexpt(j))
                  tmp = tmp*LAM_ISQRT2
                  this%m_Vsamp(i) = tmp
              end do
          end do
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          if(present(fpflags) == .true.) then
              call ieee_get_flag(ieee_all,fpflags)
                 if(ANY(fpflags) == .true.) then
                     if(present(verbose) .AND. verbose == .true.) then
                          WRITE(stderr,*) "=============================================================================="
                          WRITE(stderr,*) "  mod_echwsamples/compute_Vsamp:812 -- Floating-point exception(s) occurred!!"
                          WRITE(stderr,*) "==============================================================================="
                     end if
                     err = -3
                 end if
          end if
!DIR$     ENDIF          
    end subroutine
    
    !=================================================!
    !  @subroutine: compute_Vsampavg                                         
    !  @Purpose:
    !            Performs computation of weather radar
    !            signal samples averaged over frequency cycle
    !            and stores them in member array mVsampavg.
    !            
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   2) -2 -- Invalid argument (any of them)
    !   3) -3 -- Floating-point arithmentic error 
    !            (general error) (in this case)
    !   4) -4 -- Overflow
    !   5) -5 -- Underflow
    !   6) -6 -- Inexact
    !   7) -7 -- Denormal
    !   8) -8 -- Cancellation error (cadna)
    !   9) -9 -- Optional argument not present
    !=================================================!
    subroutine compute_Vsampavg(this,err,fpflags,verbose)
          use mod_constants, only : LAM_HR64P,LAM_ZC
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exception
!DIR$     ENDIF
          implicit none
          class(ECSWSamples_t),       intent(inout)           :: this
          integer(I32P),              intent(inout)           :: err
          logical(I32P), dimension(5),intent(inout), optional :: fpflags
          logical(I32P),              intent(in),    optional :: verbose
          ! locals
          complex(R64P) :: tmp
          integer(I32P) :: i,j
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$     ENDIF
          ! Start of executable statements
          ! Sanity check on input arguments
          if(err < 0) err = 0
          if(present(fpflags) == .true.) then
              if(ANY(fpflags) == .true.) then
                  fpflags = .false.
              end if
          end if
          tmp = LAM_ZC
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          if(present(fpflags) == .true.) then
              call ieee_get_status(status_value)
              call ieee_set_handling_mode(ieee_all,.false.)
              call ieee_set_flags(ieee_all,fpflags)
          else
              WRITE(stderr,*) "mod_echwsamples/compute_Vsampavg:877 -- Optional argument: fplfags -- not present!!"
          end if
!DIR$     ENDIF              
          do i = 1, this%m_size
!DIR$     SIMD
              do j = 1, this%m_size
                  tmp = tmp+(this%m_Ai(j)*DCONJG(this%m_Ai(j))* &
                         this%m_Wi(j)*DCONJG(this%m_Wi(j))*this%m_cexpt(j))
                  tmp = tmp*LAM_HR64P
                  this%m_Vsampavg(i) = tmp
              end do
          end do
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          if(present(fpflags) == .true.) then
              call ieee_get_flag(ieee_all,fpflags)
                 if(ANY(fpflags) == .true.) then
                     if(present(verbose) .AND. verbose == .true.) then
                          WRITE(stderr,*) "=============================================================================="
                          WRITE(stderr,*) "  mod_echwsamples/compute_Vsampavg:897 -- Floating-point exception(s) occurred!!"
                          WRITE(stderr,*) "==============================================================================="
                     end if
                     err = -3
                 end if
          end if
!DIR$     ENDIF           
    end subroutine
    
    !=================================================!
    !  @subroutine: read_samples                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine read_samples(this,form,unit,ioerr)
          implicit none
          class(ECSWSamples_t), intent(in)    :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
     !=================================================!
    !  @subroutine: write_samples                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine write_samples(this,form,unit,ioerr)
          implicit none
          class(ECSWSamples_t), intent(in)    :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    !==============================================
    !               Module operators
    !==============================================
    
    !==============================================
    !   subroutine: assignment (=)
    !==============================================
    subroutine assign_samples(this,other)
          implicit none
          type(ECSWSamples_t), intent(inout) :: this
          type(ECSWSamples_t), intent(in)    :: other
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statemetns
          if(LOC(this) == LOC(other)) then
               call DATE_AND_TIME(date=sdate,time=stime)
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_echwsamples/assignment(=):964, Attempted self_assignment!)"
               write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
               write(stderr,*) "===========================NON-FATAL=========================="
               return
          end if
          this%m_nechoes    = other%m_nechoes
          this%m_size       = other%m_size
          this%m_scatnum    = other%m_scatnum
          this%m_Ts         = other%m_Ts
          this%m_ths        = other%m_ths
          this%m_sampID     = other%m_sampID
          this%m_Ai         = other%m_Ai
          this%m_Wi         = other%m_Wi
          this%m_cexpt      = other%m_cexpt
          this%m_Vsamp      = other%m_Vsamp
          this%m_Vsampavg   = other%m_Vsampavg
          this%m_isbuilt    = other%m_isbuilt
    end subroutine
    
    !=====================================================60
    ! function: samples_neq_samples i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function samples_neq_samples(s1,s2) result(neq)
          use module_helper_fields_equality, only : Pair1D_t
          implicit none
          type(ECSWSamples_t), intent(in) :: s1,s2
          ! Locals
          type(Pair1D_t) :: neq
          ! Start of executable statemetns
          neq%x = s1%m_Vsamp    /= s2%m_Vsamp
          neq%y = s1%m_Vsampavg /= s2%m_Vsampavg
    end function
    
    !======================================================60
    ! function: samples_eq_samples i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function samples_eq_samples(s1,s2) result(eq)
          use module_helper_fields_equality, only : Pair1D_t
          implicit none
          type(ECSWSamples_t), intent(in) :: s1,s2
          ! Locals
          type(Par1D_t) :: eq
          ! Start of executable statements
          eq%x = s1%m_Vsamp    == s2%m_Vsamp
          eq%y = s1%m_Vsampavg == s2%m_Vsampavg
    end function
    
    !======================================================60
    ! function: samples_gt_samples i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function samples_gt_samples(s1,s2) result(gt)
          use module_helper_fields_equality, only : Pair1D_t
          implicit none
          type(ECSWSamples_t), intent(in) :: s1,s2
          ! Locals
          type(Par1D_t) :: gt
          ! Start of executable statements
          gt%x = s1%m_Vsamp    == s2%m_Vsamp
          gt%y = s1%m_Vsampavg == s2%m_Vsampavg
    end function
    
    !======================================================60
    ! function: samples_lt_samples i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function samples_lt_samples(s1,s2) result(lt)
          use module_helper_fields_equality, only : Pair1D_t
          implicit none
          type(ECSWSamples_t), intent(in) :: s1,s2
          ! Locals
          type(Par1D_t) :: lt
          ! Start of executable statements
          lt%x = s1%m_Vsamp    == s2%m_Vsamp
          lt%y = s1%m_Vsampavg == s2%m_Vsampavg
    end function
    
    
end module mod_echwsamples